MODULE constants
   REAL , PARAMETER :: cp              = 1004.0
   REAL , PARAMETER :: es0             =    6.1121
   REAL , PARAMETER :: eps             =    0.622
   REAL , PARAMETER :: g               =    9.81
   REAL , PARAMETER :: pi              =    3.14159265358
   REAL , PARAMETER :: r               =  287.04
   REAL , PARAMETER :: radius_of_earth = 6370.0
   REAL , PARAMETER :: svp1            =    0.6112
   REAL , PARAMETER :: svp2            =   17.67
   REAL , PARAMETER :: svp3            =   29.65
   REAL , PARAMETER :: svpt0           =  273.15

   REAL , PARAMETER :: small_positive  =  1.0E-5
   REAL , PARAMETER ::   big_negative  = -1.0E19

   REAL , PARAMETER :: degran          = pi/180.
   REAL , PARAMETER :: rovcp           =  r / cp
END MODULE constants
