'''
 Something to proctor the realtime run!
'''
import sys
import datetime
import pytz
import os
import urllib2
import subprocess
import glob

BASEFOLDER = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
HOURS = 72

def dl_ncep( ts ):
    ''' Download stuff we want from NCEP '''
    print '1. Download NCEP GFS Data'
    baseuri = "http://ftpprd.ncep.noaa.gov/data/nccf/com/gfs/prod"
    tmpdir = "/tmp/gfs.%s" % (ts.strftime("%Y%m%d%H"),)
    if not os.path.isdir(tmpdir):
        os.makedirs(tmpdir)
        
    for i in range(0,73,3):
        g1file = "%s/gfs.t%02iz.pgrb2.1p00.f%03i.grib" % (tmpdir, ts.hour, i)
        g2file = "%s/gfs.t%02iz.pgrb2.1p00.f%03i" % (tmpdir, ts.hour, i)
        if not os.path.isfile(g1file):
            print '   Fetching: %s' % (g2file,),
            uri = "%s/gfs.%s/gfs.t%02iz.pgrb2.1p00.f%03i" % (baseuri, 
                                        ts.strftime("%Y%m%d%H"), ts.hour, i)
            dldata = urllib2.urlopen(uri).read()
            o = open(g2file, 'wb')
            o.write(dldata)
            o.close()
            print '%s' % (len(dldata),)

        if not os.path.isfile(g1file):
            #convert to grib2
            subprocess.call("/usr/local/bin/cnvgrib -g21 %s %s" % (g2file, 
                                                                g1file),
                            shell=True, stdout=subprocess.PIPE, 
                            stderr=subprocess.PIPE)

        # Remove the grib2 file as it is no longer needed...
        if os.path.isfile(g1file) and os.path.isfile(g2file):
            os.unlink(g2file)

def pregrid(sts , ets):
    ''' Do the pregrid activity '''
    print '2. Running pregrid'
    os.chdir("%s/REGRID/pregrid" % (BASEFOLDER,))
    tmpdir = "/tmp/gfs.%s" % (sts.strftime("%Y%m%d%H"),)
    cmd = "csh pregrid.csh %s %s %s" % (tmpdir, sts.strftime("%Y %m %d %H"),
                                  ets.strftime("%Y %m %d %H"))
    p = subprocess.Popen(cmd,shell=True, stdout=subprocess.PIPE, 
                        stderr=subprocess.PIPE)
    o = open('pregrid.stdout.txt', 'w')
    o.write( p.stdout.read() )
    o.close()

    o = open('pregrid.stderr.txt', 'w')
    o.write( p.stderr.read() )
    o.close()

    
def regridder(sts, ets):
    ''' Do the regridder step '''
    print '3. Running regridder'
    os.chdir("%s/REGRID/regridder" % (BASEFOLDER,))
    o = open('namelist.input', 'w')
    o.write("""
&record1   
start_year                      = %s
start_month                     = %s
start_day                       = %s
start_hour                      = %s
end_year                        = %s
end_month                       = %s
end_day                         = %s
end_hour                        = %s
interval                        = 10800/    

&record2
 ptop_in_Pa                      = 10000
 new_levels_in_Pa                = 95000 , 92500 , 90000 ,
                                   80000 ,
                                   75000 ,
                                   65000 , 60000 ,
                                   55000 ,
                                   45000 ,
                                   35000
 sst_to_ice_threshold            = -9999
 linear_interpolation            = .FALSE. /

&record3
 root                            = '../pregrid/SST_FILE' '../pregrid/FILE' '../pregrid/SNOW_FILE' '../pregrid/SOIL_FILE'
 terrain_file_name               = '../../TERRAIN/TERRAIN_DOMAIN1' /
 constants_full_name             = ''

&record4
 print_echo                      = .FALSE. ,
 print_debug                     = .FALSE. ,
 print_mask                      = .FALSE. ,
 print_interp                    = .FALSE. ,
 print_link_list_store           = .FALSE. ,
 print_array_store               = .FALSE. ,
 print_header                    = .FALSE. ,
 print_output                    = .FALSE. ,
 print_file                      = .FALSE. ,
 print_f77_info                  = .TRUE. /
                                           
""" % (sts.year, sts.month, sts.day, sts.hour, ets.year, ets.month, 
       ets.day, ets.hour))
    o.close()
    
    p = subprocess.Popen("./regridder", shell=True, stdout=subprocess.PIPE, 
                        stderr=subprocess.PIPE)    
    o = open('regrid.stdout.txt', 'w')
    o.write( p.stdout.read() )
    o.close()

    o = open('regrid.stderr.txt', 'w')
    o.write( p.stderr.read() )
    o.close()
    
    # Lets go cleanup after ourselves
    os.chdir("%s/REGRID/pregrid" % (BASEFOLDER,))
    for fn in glob.glob("*FILE:*"):
        os.unlink(fn)
    
def interpf(sts, ets):
    ''' Do Interpf step '''
    print '4. Running interpf'
    os.chdir("%s/INTERPF" % (BASEFOLDER,))
    o = open('namelist.input', 'w')
    o.write("""
&record0
input_file     = '../REGRID/regridder/REGRID_DOMAIN1' /

&record1
start_year                      = %s
start_month                     = %s
start_day                       = %s
start_hour                      = %s
end_year                        = %s
end_month                       = %s
end_day                         = %s
end_hour                        = %s
interval                        = 10800
less_than_24h                   = .FALSE. /

&record2
 sigma_f_bu     = 1.00,0.99,0.98,0.96,0.93,0.89,     ! full sigma, bottom-up,
                  0.85,0.80,0.75,0.70,0.65,0.60,     ! start with 1.0, end
                  0.55,0.50,0.45,0.40,0.35,0.30,     ! with 0.0
                  0.25,0.20,0.15,0.10,0.05,0.00
 ptop           = 10000                              ! top pressure if need to be redefined
 isfc           = 0 /                                ! # sigma levels to spread
                                                     ! surface information

&record3
 p0             = 1.e5                               ! base state sea-level pres (Pa)
 tlp            = 50.                                ! base state lapse rate d(T)/d(ln P)
 ts0            = 275.                               ! base state sea-level temp (K)
 tiso           = 0./                                ! base state isothermal stratospheric temp (K)

&record4
 removediv      = .TRUE.                             ! T/F remove integrated mean divergence
 usesfc         = .TRUE.                             ! T/F use surface data
 wrth2o         = .TRUE. /                           ! T/F specific humidity wrt H2O

&record5
 ifdatim        = -1 /                               ! # of IC time periods to output

""" % (sts.year, sts.month, sts.day, sts.hour, ets.year, ets.month, 
       ets.day, ets.hour))
    o.close()
    
    p = subprocess.Popen("./interpf", shell=True, stdout=subprocess.PIPE, 
                        stderr=subprocess.PIPE)    
    o = open('interpf.stdout.txt', 'w')
    o.write( p.stdout.read() )
    o.close()

    o = open('interpf.stderr.txt', 'w')
    o.write( p.stderr.read() )
    o.close()
    
    for fn in ("BDYOUT_DOMAIN1", "MMINPUT_DOMAIN1", "LOWBDY_DOMAIN1"):
        sz = os.path.getsize(fn)
        if sz < 1000:
            print("ABORT: %s is %s bytes, too small!" % (fn, sz))
            sys.exit()
        os.rename(fn, "../MM5/Run/%s" % (fn,))
    
    # Cleanup after ourself
    os.unlink('../REGRID/regridder/REGRID_DOMAIN1')
    
def mm5deck():
    ''' Run mm5deck '''
    print '5. Running MM5 Deck'
    os.chdir("%s/MM5" % (BASEFOLDER,))
    p = subprocess.Popen("./mm5deck", shell=True, stdout=subprocess.PIPE, 
                        stderr=subprocess.PIPE)    
    o = open('mm5deck.stdout.txt', 'w')
    o.write( p.stdout.read() )
    o.close()

    o = open('mm5deck.stderr.txt', 'w')
    o.write( p.stderr.read() )
    o.close()
    
def run_mm5():
    ''' Run mm5d '''
    print '6. Running MM5'
    os.chdir("%s/MM5/Run" % (BASEFOLDER,))
    p = subprocess.Popen("/usr/local/openmpi-intel/bin/mpirun -np 2 ./mm5.mpp", 
                         shell=True, stdout=subprocess.PIPE, 
                        stderr=subprocess.PIPE)    
    o = open('mm5.mpp.stdout.txt', 'w')
    o.write( p.stdout.read() )
    o.close()

    o = open('mm5.mpp.stderr.txt', 'w')
    o.write( p.stderr.read() )
    o.close()

def archiver( sts ):
    ''' Run archiver '''
    print '7. Running Archiver'
    cmd = "/usr/local/bin/archiver MMOUT_DOMAIN1 0 %s isumm5_%s.nc" % (HOURS+1,
                                            sts.strftime("%Y%m%d%H%M"))
    p = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, 
                        stderr=subprocess.PIPE)    
    o = open('archiver.stdout.txt', 'w')
    o.write( p.stdout.read() )
    o.close()

    o = open('archiver.stderr.txt', 'w')
    o.write( p.stderr.read() )
    o.close()


def cleanup(ts):
    """Cleanup after ourself"""
    tmpdir = "/tmp/gfs.%s" % (ts.strftime("%Y%m%d%H"),)
    if os.path.isdir(tmpdir):
        subprocess.call("rm -rf %s" % (tmpdir,), shell=True)

if __name__ == '__main__':
    yyyy = int(sys.argv[1])
    mm = int(sys.argv[2])
    dd = int(sys.argv[3])
    hh24 = int(sys.argv[4])

    sts = datetime.datetime(yyyy, mm, dd, hh24)
    sts = sts.replace(tzinfo=pytz.timezone("UTC"))

    ets = sts + datetime.timedelta(hours=72)

    dl_ncep(sts)
    pregrid(sts, ets)
    regridder(sts, ets)
    interpf(sts, ets)
    mm5deck()
    run_mm5()
    archiver(sts)
    cleanup(sts)
