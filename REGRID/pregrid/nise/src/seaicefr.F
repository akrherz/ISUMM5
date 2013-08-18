PROGRAM seaicefr

   USE output_data
   USE namelist_stuff
   USE nsidc
   USE support

   IMPLICIT NONE

   CHARACTER (LEN=28) :: filename
   CHARACTER (LEN=27) :: filename2
   CHARACTER (LEN=24) :: datestr
   REAL :: row,column

   CALL get_namelist_data

   WRITE ( filename , FMT = '("NISE_SSMIF13_",I4.4,I2.2,I2.2,".HDFEOS")' ) &
      start_year , start_month , start_day

   CALL read_seaice ( filename )

   CALL seaice_gridded

   WRITE ( filename2 , FMT = '("SEAICEFR_FILE:",I4.4,"-",I2.2,"-",I2.2,"_00")' ) &
      start_year , start_month , start_day
   WRITE ( datestr , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_00:00:00.0000")' ) &
      start_year , start_month , start_day

   CALL output_seaice ( filename2 , datestr )

END PROGRAM seaicefr

