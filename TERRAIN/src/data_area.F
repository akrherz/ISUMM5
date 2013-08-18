      PROGRAM DATA_30S                                                           DATA_AREA.1
C                                                                                DATA_AREA.2
# include <parame.incl>                                                          DATA_AREA.3
# include <paramed.incl>                                                         06NOV00.67
# include <io.incl>                                                              06NOV00.68
# include <maps.incl>                                                            DATA_AREA.4
# include <nestdmn.incl>                                                         DATA_AREA.5
# include <ltdata.incl>                                                          DATA_AREA.6
C                                                                                DATA_AREA.7
C   SET UP THE PARAMETERS AND CONSTANTS                                          DATA_AREA.8
      CALL SETUP                                                                 DATA_AREA.9
C                                                                                DATA_AREA.10
      N30S = 0                                                                   DATA_AREA.11
      YMINLAT =  200.                                                            DATA_AREA.12
      YMAXLAT = -200.                                                            DATA_AREA.13
      YMINLON =  400.                                                            DATA_AREA.14
      YMAXLON = -400.                                                            DATA_AREA.15
C                                                                                DATA_AREA.16
      DO 10 NM = 1,MAXNES                                                        DATA_AREA.17
C                                                                                DATA_AREA.18
      PRINT 11, NM                                                               DATA_AREA.19
11    FORMAT(/2X,'=== DETERMINING THE SEARCH AREA FOR DOMAIN',I2,' ==')          DATA_AREA.20
C                                                                                DATA_AREA.21
      IF (NM.EQ.1) THEN                                                          DATA_AREA.22
      IMAX = IXEX                                                                DATA_AREA.23
      JMAX = JXEX                                                                DATA_AREA.24
      ELSE                                                                       DATA_AREA.25
      IMAX = NESTIX(NM)                                                          DATA_AREA.26
      JMAX = NESTJX(NM)                                                          DATA_AREA.27
      ENDIF                                                                      DATA_AREA.28
C                                                                                DATA_AREA.29
      CALL SEARCH(IMAX, JMAX, NM, NDI)                                           DATA_AREA.30
C                                                                                DATA_AREA.31
      PRINT 12, NDI, NM                                                          DATA_AREA.32
12    FORMAT(/20X,'=== THE DATASET',I2,' FOR DOMAIN ',I2,'  ===')                DATA_AREA.33
C                                                                                DATA_AREA.34
      IF (NDI.EQ.max_type .AND. N30S.NE.NDI) THEN                                06NOV00.69
        N30S = NDI                                                               DATA_AREA.36
        YMINLAT = AMIN1(YMINLAT,XMINLAT)                                         DATA_AREA.37
        YMAXLAT = AMAX1(YMAXLAT,XMAXLAT)                                         DATA_AREA.38
        YMINLON = AMIN1(YMINLON,XMINLON)                                         DATA_AREA.39
        YMAXLON = AMAX1(YMAXLON,XMAXLON)                                         DATA_AREA.40
C                                                                                DATA_AREA.41
        PRINT 13,NDI,YMAXLAT,YMINLAT,YMAXLON,YMINLON                             DATA_AREA.42
 13     FORMAT('DATA ID=',I2,' YMAXLAT,YMINLAT,YMAXLON,YMINLON:',4F10.2)         DATA_AREA.43
      ENDIF                                                                      DATA_AREA.44
C                                                                                DATA_AREA.45
10    CONTINUE                                                                   DATA_AREA.46
C                                                                                DATA_AREA.47
      IDEM  = 50                                                                 DATA_AREA.48
      IFORT = 60                                                                 DATA_AREA.49
      RESOL = 1./120.                                                            DATA_AREA.50
      IF (N30S.EQ.max_type)                                                      06NOV00.70
     -    CALL DATA30S(YMAXLAT,YMINLAT,YMAXLON,YMINLON,RESOL,                    DATA_AREA.52
     -                                            IDEM,IFORT)                    DATA_AREA.53
C                                                                                DATA_AREA.54
      STOP                                                                       DATA_AREA.55
      END                                                                        DATA_AREA.56
