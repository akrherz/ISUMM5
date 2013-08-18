      SUBROUTINE INTERP(IMAX,JMAX,XLON,XLAT,HTGRID,HTIN,                         INTERP.1
     1                  NI,GRDLTMN,GRDLNMN,III,JJJ,START_J)                      INTERP.2
C                                                                                INTERP.3
C   PURPOSE          : CREATES THE TERRAIN HEIGHT FIELDS ON MESOSCALE            INTERP.4
C                      GRIDS BY USING THE OVERLAPPING PARABOLIC                  INTERP.5
C                      INTERPOLATION METHOD WITH THE TERRAIN SOURCE              INTERP.6
C                      DATA ON THE REGULAR LATITUDE-LONGITUDE GRID               INTERP.7
C                      AS INPUT.                                                 INTERP.8
C   ON ENTRY         :                                                           INTERP.9
C   IMAX,JMAX        : THE DIMENSIONS OF THE MESOSCALE DOMAIN.                   INTERP.10
C   XLON(IX,JX)      : LONGITUDES OF THE MESOSCALE GRID POINTS.                  INTERP.11
C   XLAT(IX,JX)      : LATITUDES OF THE MESOSCALE GRID POINTS.                   INTERP.12
C   HTIN(III,JJJ)    : THE SOURCE TERRAIN HEIGHT FIELD WITH THE                  INTERP.13
C                      REGULAR LATITUDE AND LONGITUDE INTERVALS.                 INTERP.14
C   NI               : THE ID NUMBER OF THE SOURCE DATA FILE.                    INTERP.15
C   GRDLTMN          : THE MINIMUM LATITUDE OF THE SOURCE DATA.                  INTERP.16
C   GRDLNMN          : THE MINIMUM LONGITUDE OF THE SOURCE DATA.                 INTERP.17
C   III,JJJ          : THE DIMENSIONS OF THE ARRAY HTIN.                         INTERP.18
C   ON EXIT          :                                                           INTERP.19
C   HTGRID(IMAX,JMAX): THE INTERPOLATED TERRAIN HEIGHT FIELD ON                  INTERP.20
C                      THE MESOSCALE DOMAIN.                                     INTERP.21
C                                                                                INTERP.22
C   CALLS FUNCTION BINT.                                                         INTERP.23
C   CALLED BY SUBROUTINE CRTER.                                                  INTERP.24
C                                                                                INTERP.25
C   COMMENTS         :                                                           INTERP.26
C IN THIS SUBROUTINE, THE FLAG IS ALWAYS SET TO BE .FALSE.                       INTERP.27
C THIS MEANS THE 16-POINT OVERLAPPING PARABOLIC INTERPOLATION IS                 INTERP.28
C USED. IF ONE OF THE 16 POINTS IS MISSING, THE INTERPOLATION HAS                INTERP.29
C NO MEANING AND THE PROGRAM IS STOPPED. SINCE THE 30-SECOND TERRAIN             INTERP.30
C HEIGHT DATA (NI = max-type) IS A LOCAL DATASET, THIS SITUATION MAY OCCUR       06NOV00.76
C AND INTERP WILL NOT WORK.                                                      INTERP.32
# include <paramed.incl>                                                         06NOV00.77
# include <io.incl>                                                              06NOV00.78
# include <option.incl>                                                          INTERP.33
# include <ltdata.incl>                                                          INTERP.34
C                                                                                INTERP.35
      DIMENSION XLAT(IMAX,JMAX),XLON(IMAX,JMAX),HTIN(III,JJJ),                   INTERP.36
     1   HTGRID(IMAX,JMAX)                                                       INTERP.37
      LOGICAL FLAG                                                               INTERP.38
C                                                                                INTERP.39
      FLAG  = .FALSE.                                                            INTERP.40
      IMAX1 = IMAX-1                                                             INTERP.41
      JMAX1 = JMAX-1                                                             INTERP.42
C                                                                                INTERP.43
      DSGRID = CENTER(NI)                                                        INTERP.44
C                                                                                INTERP.45
      DO 10 I=1,IMAX1                                                            INTERP.46
      DO 10 J=1,JMAX1                                                            INTERP.47
C                                                                                INTERP.48
      YY = -(GRDLTMN-XLAT(I,J))/DSGRID + 1.0                                     INTERP.49
        IF ( XLON(I,J) .GT.XMAXLON .AND. GRDLNMN.LT.-180)                        INTERP.50
                                                                                 INTERP.51
     2       XLON(I,J)=XLON(I,J)-360.                                            INTERP.52
      XX = -(GRDLNMN-XLON(I,J))/DSGRID + START_J                                 INTERP.53
C                                                                                INTERP.54
C   IF THERE ARE MISSING DATA AMONG THE 16 POINTS SURROUNDING (XX,YY)            INTERP.55
C   FOR 1/120 DEG. TERRAIN DATA, USE OBJECTIVE ANALYSIS (IFANAL=.T.)             INTERP.56
C   TO CREATE THE TERRAIN FILE.                                                  INTERP.57
C                                                                                INTERP.58
      IF (NI.EQ.max_type) THEN                                                   06NOV00.79
        I0    = INT(YY+0.00001)                                                  INTERP.60
        J0    = INT(XX+0.00001)                                                  INTERP.61
        DO 20 KK = I0-1,I0+2                                                     INTERP.62
        DO 20 LL = J0-1,J0+2                                                     INTERP.63
          HHH = HTIN(KK,LL)                                                      INTERP.64
          IF (.NOT.G30SD .AND. HHH .EQ. -99.99) THEN                             INTERP.65
          PRINT 21,NI,KK,LL,HHH,XLAT(I,J),XLON(I,J)                              INTERP.66
21      FORMAT(1X,'DATASET:',I3,'  I,J:',2I6,'  HT=',F8.1,                       INTERP.67
     1         ' MISSING DATA (NEAR LAT,LON:',2F7.2,/                            INTERP.68
     2  ' ***  USE ANAL2 (IFANAL=.T.)  ***')                                     INTERP.69
          STOP                                                                   INTERP.70
          ENDIF                                                                  INTERP.71
20      CONTINUE                                                                 INTERP.72
      ENDIF                                                                      INTERP.73
C                                                                                INTERP.74
C        YY AND XX ARE THE EXACT INDEX VALUES OF A POINT I,J OF THE              INTERP.75
C        MESOSCALE MESH WHEN PROJECTED ONTO AN EARTH-GRID OF LAT'S               INTERP.76
C        AND LON'S FOR WHICH TERRAIN OBSERVATIONS ARE AVAILABLE.  IT             INTERP.77
C        IS ASSUMED THAT THE EARTH GRID HAS EQUAL SPACING IN BOTH                INTERP.78
C        LATITUDE AND LONGITUDE.                                                 INTERP.79
C                                                                                INTERP.80
      HTGRID(I,J) = BINT(YY,XX,HTIN,III,JJJ,FLAG)                                INTERP.81
   10 CONTINUE                                                                   INTERP.82
C                                                                                INTERP.83
      RETURN                                                                     INTERP.84
      END                                                                        INTERP.85
