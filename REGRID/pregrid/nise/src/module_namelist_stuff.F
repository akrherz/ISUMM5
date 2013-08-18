MODULE namelist_stuff

   INTEGER :: start_year , start_month , start_day
   NAMELIST /RECORD1/ start_year , start_month , start_day

CONTAINS

   SUBROUTINE get_namelist_data

      OPEN ( UNIT = 10 , &
             FILE = 'namelist.input' , &
             STATUS = 'OLD' , &
             FORM = 'FORMATTED', & 
             ACTION = 'READ' , &
             ACCESS = 'SEQUENTIAL' )

      READ ( 10 , NML = RECORD1 )
      WRITE ( 6 , NML = RECORD1 )

      CLOSE (10)

   END SUBROUTINE get_namelist_data

END MODULE namelist_stuff
