      SUBROUTINE LATLON(IYY,JXX,XLAT,XLON,DS,XICNS,XJCNS,N,IRATIO,IP)            LATLON.1
C                                                                                LATLON.2
C   PURPOSE      : CALCULATES THE LATITUDES AND LONGITUDES OF THE                LATLON.3
C                  MESOSCALE GRIDS FOR DOMAIN N                                  LATLON.4
C   ON ENTRY     :                                                               LATLON.5
C   IYY,JXX      : THE DIMENSIONS OF THE MESOSCALE DOMAIN N.                     LATLON.6
C   DS           : THE GRID SIZE OF THE DOMAIN N.                                LATLON.7
C   XICNS,XJCNS  : THE I AND J LOCATION OF THE DOMAIN N GRID POINT (1,1)         LATLON.8
C                  IN THE COARSEST DOMAIN (DOMAIN 1). THEY ARE REAL              LATLON.9
C                  NUMBERS.                                                      LATLON.10
C   N            : THE ID NUMBER OF THE MESOSCALE DOMAIN.                        LATLON.11
C   IRATIO       : THE RATIO OF THE GRID DISTANCES BETWEEN DOMAIN 1 AND          LATLON.12
C                  DOMAIN N.                                                     LATLON.13
C   IP           : THE SWITCH FOR PRINTING THE XLAT AND XLON FIELDS.             LATLON.14
C   ON EXIT      :                                                               LATLON.15
C   XLAT(IYY,JXX): LATITUDES OF THE GRID POINTS FOR DOMAIN N.                    LATLON.16
C   XLON(IYY,JXX): LONGITUDES OF THE GRID POINTS FOR DOMAIN N.                   LATLON.17
C                                                                                LATLON.18
C   CALLS THE SUBROUTINE OUTPT AND THE INTRINSIC FUNCTIONS EXP,COS,              LATLON.19
C         SQRT, SIN, TAN, ATAN, AND ATAN2.                                       LATLON.20
C   CALLED BY SUBROUTINE TERDRV.                                                 LATLON.21
C                                                                                LATLON.22
C   COMMENTS :                                                                   LATLON.23
C IN ADDITION TO THE LIST OF ARGUMENTS, SUBROUTINE LATLON GETS MORE              LATLON.24
C INFORMATION FROM THE COMMON BLOCKS /MAPS/ AND /OPTION/. BECAUSE                LATLON.25
C TERRAIN HEIGHT AND LAND-USE ARE DEFINED AT THE CROSS POINTS, XLAT              LATLON.26
C AND XLON ARE THE LATITUDES AND LONGITUDES AT THE CROSS POINTS.                 LATLON.27
C THEIR RANGES ARE FROM -90 TO 90 AND FROM -180 TO 180, RESPECTIVELY.            LATLON.28
C THE LOCATION OF THE LOWER LEFT CORNER AND THE GRID SIZE RATIO ARE              LATLON.29
C RELATIVE TO THE COARSE DOMAIN, NOT ITS MOTHER DOMAIN. THIS INFORMATION         LATLON.30
C IS CALCULATED IN THE SUBROUTINE SETUP. IF IP = .TRUE., THE LATITUDE            LATLON.31
C AND LONGITUDE FIELDS ARE PRINTED OUT FOR VERIFICATION.                         LATLON.32
# include <maps.incl>                                                            LATLON.33
# include <option.incl>                                                          LATLON.34
C                                                                                LATLON.35
      DIMENSION XLAT(IYY,JXX),XLON(IYY,JXX)                                      LATLON.36
      LOGICAL IP                                                                 LATLON.37
C                                                                                LATLON.38
        CNTRI = CNTRI0                                                           LATLON.39
        CNTRJ = CNTRJ0                                                           LATLON.40
      IF(IPRINT.EQ.1) THEN                                                       LATLON.41
      PRINT 19,CNTRI,CNTRJ,XICNS,XJCNS,DS,IRATIO                                 LATLON.42
19    FORMAT(2X,'CNTRI=',F6.1,2X,'CNTRJ=',F6.1,2X,'XICNS=',F6.1,                 LATLON.43
     1       2X,'XJCNS=',F6.1,2X,'DS=',F6.1,2X,'IRATIO=',I4)                     LATLON.44
      ENDIF                                                                      LATLON.45
C                                                                                LATLON.46
      CNTRJ = (CNTRJ-XJCNS)*IRATIO + 0.5                                         LATLON.47
      CNTRI = (CNTRI-XICNS)*IRATIO + 0.5                                         LATLON.48
      IF(IPRINT.EQ.1) THEN                                                       LATLON.49
      PRINT 20,CNTRJ,CNTRI                                                       LATLON.50
20    FORMAT(1X,'THE INDICES OF THE CENTER ARE J = ',F5.1,                       LATLON.51
     1                                     ' AND I = ',F5.1)                     LATLON.52
      IF (N.GT.1) PRINT 30                                                       LATLON.53
   30 FORMAT(20X,'WHERE I AND J ARE IN TERMS OF THE FINE MESH ',                 LATLON.54
     1   'INDICES.')                                                             LATLON.55
      ENDIF                                                                      LATLON.56
C-----CALCULATE X AND Y POSITIONS OF GRID                                        LATLON.57
      DO 40 I=1,IYY                                                              LATLON.58
      DO 40 J=1,JXX                                                              LATLON.59
      X = XCNTR+(J-CNTRJ)*DS                                                     LATLON.60
      Y = YCNTR+(I-CNTRI)*DS                                                     LATLON.61
C-----NOW CALCULATE LAT AND LON OF THIS POINT                                    LATLON.62
      IF (IPROJ.NE.NPROJ(3)) THEN                                                LATLON.63
        IF(Y.EQ.0.) THEN                                                         LATLON.64
          IF(X.GE.0.0) FLP = 90.0/CONV                                           LATLON.65
          IF(X.LT.0.0) FLP = -90.0/CONV                                          LATLON.66
        ELSE                                                                     LATLON.67
         IF (PHIC.LT.0.0)THEN                                                    LATLON.68
            FLP = ATAN2(X,Y)                                                     LATLON.69
         ELSE                                                                    LATLON.70
            FLP = ATAN2(X,-Y)                                                    LATLON.71
         ENDIF                                                                   LATLON.72
        ENDIF                                                                    LATLON.73
         FLPP = (FLP/XN)*CONV+XLONC                                              LATLON.74
         IF (FLPP.LT.-180.) FLPP = FLPP + 360                                    LATLON.75
         IF (FLPP.GT.180.)  FLPP = FLPP - 360.                                   LATLON.76
         XLON(I,J) = FLPP                                                        LATLON.77
C--------NOW SOLVE FOR LATITUDE                                                  LATLON.78
         R = SQRT(X*X+Y*Y)                                                       LATLON.79
         IF (PHIC.LT.0.0) R = -R                                                 LATLON.80
         IF (IPROJ.EQ.NPROJ(1)) THEN                                             LATLON.81
            CELL = (R*XN)/(A*SIN(PSI1))                                          LATLON.82
            RXN  = 1.0/XN                                                        LATLON.83
            CEL1 = TAN(PSI1/2.)*(CELL)**RXN                                      LATLON.84
         ENDIF                                                                   LATLON.85
         IF (IPROJ.EQ.NPROJ(2)) THEN                                             LATLON.86
            CELL = R/A                                                           LATLON.87
            CEL1 = CELL/(1.0+COS(PSI1))                                          LATLON.88
         ENDIF                                                                   LATLON.89
         CEL2 = ATAN(CEL1)                                                       LATLON.90
         PSX  = 2.*CEL2*CONV                                                     LATLON.91
         XLAT(I,J) = POLE-PSX                                                    LATLON.92
      ENDIF                                                                      LATLON.93
C-----CALCULATIONS FOR MERCATOR LAT,LON                                          LATLON.94
      IF (IPROJ.EQ.NPROJ(3)) THEN                                                LATLON.95
         XLON(I,J) = XLONC + ((X-XCNTR)/C2)*CONV                                 LATLON.96
         IF (XLON(I,J).LT.-180.) XLON(I,J) = XLON(I,J) + 360                     LATLON.97
         IF (XLON(I,J).GT.180.)  XLON(I,J) = XLON(I,J) - 360.                    LATLON.98
         CELL = EXP(Y/C2)                                                        LATLON.99
         XLAT(I,J) = 2.*(CONV*ATAN(CELL))-90.0                                   LATLON.100
      ENDIF                                                                      LATLON.101
   40 CONTINUE                                                                   LATLON.102
C                                                                                LATLON.103
      IF(IP) THEN                                                                LATLON.104
         PRINT 50                                                                LATLON.105
         CALL OUTPT(XLAT,IYY,1,IYY,1,JXX,1,JXX,1,5,'XLATC   ')                   LATLON.106
         PRINT 60                                                                LATLON.107
         CALL OUTPT(XLON,IYY,1,IYY,1,JXX,1,JXX,1,5,'XLONC   ')                   LATLON.108
      END IF                                                                     LATLON.109
      RETURN                                                                     LATLON.110
   50 FORMAT(1X,//,'LATITUDE (NORTH POSITIVE)',//)                               LATLON.111
   60 FORMAT(1X,//,'LONGITUDE (EAST POSITIVE)',//)                               LATLON.112
      END                                                                        LATLON.113
