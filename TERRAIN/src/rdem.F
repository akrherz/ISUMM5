      PROGRAM RDEM                                                               RDEM.1
C                                                                                RDEM.2
C THIS IS A SIMPLE EXAMPLE OF HOW ONE MIGHT READ THE DEM                         RDEM.3
C DATA FROM THE USGS 30 SECOND ELEVATION DATA (DSS REF DS758.0).                 RDEM.4
C                                                                                RDEM.5
C                                                                                RDEM.7
C .. ORIGINAL DATA PRINTED AT THE LATITUDE = PRNLAT:                             RDEM.8
C                                                                                RDEM.9
      PARAMETER (PRNLAT = 200.)                                                  RDEM.10
C                                                                                RDEM.11
C 40X50 DEMS                                                                     RDEM.12
      PARAMETER (N1 = 4800, N2 = 6000)                                           RDEM.13
C 60X30 DEMS                                                                     RDEM.14
      PARAMETER (N3 = 7200, N4 = 3600)                                           RDEM.15
C                                                                                RDEM.16
      DIMENSION KD(33),DLAT0(33),DLAT1(33),DLON0(33),DLON1(33)                   RDEM.17
CC      INTEGER*2 INTER1(43200)                                                  RDEM.18
      CHARACTER*2 INTER1(43200)                                                  RDEM.19
C                                                                                RDEM.20
      CHARACTER*2 ME1(N1), ME2(N3)                                               RDEM.21
C                                                                                RDEM.22
      NOUT = 98                                                                  RDEM.23
C                                                                                RDEM.24
      OPEN(99,FILE='data30sID',FORM='FORMATTED',STATUS='OLD')                    RDEM.25
C
      READ(99,37) XMINLAT,XMAXLAT,XMINLON,XMAXLON,
     -                 NLN,NLT,IFORT
  37   FORMAT(10X,F8.3,10X,F8.3,10X,F8.3,10X,F8.3,
     -        10X,I2,7X,I2,8X,I2)           
C
      READ (99,1) NK                                                             RDEM.26
      PRINT 1,NK                                                                 RDEM.27
      READ (99,2) (KD(K),K=1,NK)                                                 RDEM.28
      PRINT 2,(KD(K),K=1,NK)                                                     RDEM.29
      READ(99,*)                                                                 RDEM.30
      DO K = 1,NK                                                                RDEM.31
        READ (99,3) KK,DLAT0(K),DLAT1(K),DLON0(K),DLON1(K)                       RDEM.32
 3      FORMAT(I3,4F15.8)                                                        RDEM.33
      END DO                                                                     RDEM.34
 1    FORMAT(12X,I2)                                                             RDEM.35
 2    FORMAT(33I4)                                                               RDEM.36
C                                                                                RDEM.37
      LATMIN = NINT(XMINLAT-.5)                                                  RDEM.38
      LATMAX = NINT(XMAXLAT+.5)                                                  RDEM.39
      LONMIN = NINT(XMINLON-.5)                                                  RDEM.40
      LONMAX = NINT(XMAXLON+.5)                                                  RDEM.41
      XMINLT = FLOAT(LATMIN)                                                     RDEM.42
      XMAXLT = FLOAT(LATMAX)                                                     RDEM.43
      XMINLN = FLOAT(LONMIN)                                                     RDEM.44
      XMAXLN = FLOAT(LONMAX)                                                     RDEM.45
      PRINT 5,LATMIN,LATMAX,LONMIN,LONMAX                                        RDEM.46
 5    FORMAT('LATMIN=',I4,' LATMAX=',I4,'  LONMIN=',I4,' LONMAX=',I4)            RDEM.47
C                                                                                RDEM.48
      ID2 = NINT((XMAXLN-XMINLN)*120.)+1                                         RDEM.49
      LENGTH = ID2/2                                                             RDEM.50
      IF (MOD(ID2,2).NE.0) LENGTH = LENGTH+1                                     RDEM.51
#ifdef RECLENBYTE                                                                RDEM.52
      LENGTH = LENGTH*4                                                          RDEM.53
#endif                                                                           RDEM.54
      OPEN(NOUT,FILE='new_30sdata',ACCESS='DIRECT',RECL=LENGTH,                  RDEM.55
     -                                            STATUS='NEW')                  RDEM.56
        PRINT 997,NOUT,LENGTH,ID2                                                RDEM.57
 997    FORMAT('OPEN(',I2,'  new_30sdata, RECL=',I6,                             RDEM.58
     -         '  ID2=',I6)                                                      RDEM.59
                                                                                 RDEM.60
      NOUT1 = NOUT-1                                                             RDEM.61
      OPEN(NOUT1,FILE='new_30sdata_info',FORM='UNFORMATTED',                     RDEM.62
     -                                           STATUS='NEW')                   RDEM.63
      CALL CR30SDATA(NK,NLT,NLN,KD,IFORT,DLAT0, DLAT1,                           RDEM.64
     -               DLON0,DLON1,XMINLT,XMAXLT,XMINLN,XMAXLN,                    RDEM.65
     -               N1,N2,N3,N4,ME1,ME2,ID2,INTER1,NREC,NOUT,                   RDEM.66
     -               PRNLAT)                                                     RDEM.67
C                                                                                RDEM.68
      PRINT 102,ID2,XMINLAT,XMAXLAT,XMINLON,XMAXLON                              RDEM.69
 102  FORMAT('ID2=',I5,' XMINLAT=',F9.4,' XMAXLAT=',F9.4,                        RDEM.70
     -                 ' XMINLON=',F9.4,' XMAXLON=',F9.4)                        RDEM.71
C                                                                                RDEM.72
CC      REWIND NOUT                                                              RDEM.73
      REWIND (NOUT-1)                                                            RDEM.74
C                                                                                RDEM.75
        READ(NOUT-1) XLAT00,XLAT11,XLON00,XLON11,NLATS,NPOINTS                   RDEM.76
 995    FORMAT(7X,F15.8,8X,F15.8,8X,F15.8,8X,F15.8,6X,I5,7X,I5)                  RDEM.77
        PRINT 996, XLAT00,XLAT11,XLON00,XLON11,NLATS,NPOINTS                     RDEM.78
 996    FORMAT(//'XLAT00=',F15.8,' XLAT11=',F15.8,' XLON00=',F15.8,              RDEM.79
     -        ' XLON11=',F15.8,' NREC=',I5,' NLONG=',I5)                         RDEM.80
C                                                                                RDEM.81
        CALL READ30S(NREC,NPOINTS,INTER1,XLAT00,XLON00,NOUT,PRNLAT)              RDEM.82
C                                                                                RDEM.83
      STOP                                                                       RDEM.84
      END                                                                        RDEM.85
