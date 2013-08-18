MODULE output_data

CONTAINS

   SUBROUTINE output_seaice ( filename , datestr ) 

      USE nsidc
      IMPLICIT NONE

      CHARACTER (LEN=27) :: filename
      CHARACTER (LEN=24) :: datestr

      OPEN ( UNIT = 20 , &
             FILE = filename , &
             STATUS = 'UNKNOWN' , &
             FORM = 'UNFORMATTED' , &
             ACCESS = 'SEQUENTIAL' )

      WRITE (20) 3

      WRITE (20) datestr , 0., 'SEAICEFR ' , 'Fraction (0 --> 1)       ' , &
      'Fractional Seaice (http://nsidc.org NISE)     ', 200100., &
       360*resolution+1 , 180*resolution+1 , 0

      WRITE (20) -90., 0., 1./REAL(resolution) , 1./REAL(resolution)

      WRITE (20) seaice_ll

      CLOSE (20)

   END SUBROUTINE output_seaice

END MODULE output_data
