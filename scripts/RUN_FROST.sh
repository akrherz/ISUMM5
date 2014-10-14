# Do what's necessary for the Frost Model!
export MM5I=/mesonet/www/apps/ISUMM5
cd $MM5I
python scripts/run_realtime.py $(date -u +'%Y %m %d') $1
cd /mesonet/www/apps/bridget
python scripts/run_bridget.py $MM5I/MM5/Run/isumm5_$(date -u +'%Y%m%d'){$1}00.nc 
python scripts/make_plots.py output/$(date -u +'%Y%m%d'){$1}00_iaoutput.nc
