"""Dump out the frost model temperature forecasts!"""
import netCDF4
import sys
import os
import numpy as np
from pyiem.network import Table as NetworkTable
import datetime


def compute_times(nc):
    tm = nc.variables["time"]
    base = datetime.datetime.strptime(
        tm.units.replace("minutes since ", ""), "%Y-%m-%d %H:%M:%S"
    )
    ret = []
    for t in tm[:]:
        ret.append(base + datetime.timedelta(minutes=t))
    return ret


def main(argv):
    """Go"""
    sites = ["RMCI4", "RMQI4", "ROSI4", "RTNI4", "RGRI4"]
    i_cross = []
    j_cross = []
    nt = NetworkTable("IA_RWIS")
    sts = datetime.datetime(2014, 10, 1, 0)
    ets = datetime.datetime(2015, 4, 27, 0)
    interval = datetime.timedelta(hours=12)
    now = sts
    o = open("forecast.csv", "w")
    o.write(
        ",".join(["station", "model_run", "forecast_time", "model", "temp_k"])
    )
    o.write("\n")
    while now < ets:
        for model in ["bridget", "metro"]:
            fn = now.strftime(
                (
                    "/mesonet/share/frost/" + model + "/"
                    "%Y%m%d%H%M_iaoutput.nc"
                )
            )
            if not os.path.isfile(fn):
                print("Missing %s" % (fn,))
                continue
            nc = netCDF4.Dataset(fn, "r")
            times = compute_times(nc)
            if len(i_cross) == 0:
                # Compute locations
                lats = nc.variables["lat"][:]
                lons = nc.variables["lon"][:]
                for site in sites:
                    lat = nt.sts[site]["lat"]
                    lon = nt.sts[site]["lon"]
                    field = ((lats - lat) ** 2 + (lons - lon) ** 2) ** 0.5
                    pos = np.argmin(field)
                    i_cross.append(pos % 29)
                    j_cross.append(pos / 29)
            for site, i, j in zip(sites, i_cross, j_cross):
                tmpk = nc.variables["tmpk"][:, j, i]
                for k, tm in zip(tmpk, times):
                    o.write(
                        ",".join(
                            [
                                site,
                                now.strftime("%Y-%m-%d %H:%M"),
                                tm.strftime("%Y-%m-%d %H:%M"),
                                model,
                                "%.3f" % (k,),
                            ]
                        )
                    )
                    o.write("\n")
            nc.close()
        now += interval

    o.close()


if __name__ == "__main__":
    main(sys.argv)
