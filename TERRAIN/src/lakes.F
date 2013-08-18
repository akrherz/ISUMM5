      SUBROUTINE LAKES(IX,JX,HT,FORCETER,XLAT,XLON,IP)                           06NOV00.80
C                                                                                LAKES.2
C   PURPOSE    : CORRECTS THE TERRAIN HEIGHTS OVER THE GREAT LAKES.              LAKES.3
C   ON ENTRY   :                                                                 LAKES.4
C   IX,JX      : THE DIMENSIONS OF THE MESOSCALE DOMAIN.                         LAKES.5
C   HT(IX,JX)  : TERRAIN HEIGHT FIELD.                                           LAKES.6
C   LAND(IX,JX): LAND-USE FIELD.                                                 LAKES.7
C   XLAT(IX,JX): LATITUDES OF THE MESOSCALE GRID.                                LAKES.8
C   XLON(IX,JX): LONGITUDES OF THE MESOSCALE GRID.                               LAKES.9
C   ON EXIT    :                                                                 LAKES.10
C   HT(IX,JX)  : THE TERRAIN HEIGHT FIELD AFTER CORRECTION OVER THE              LAKES.11
C                GREAT LAKES.                                                    LAKES.12
C                                                                                LAKES.13
C   CALLS NO OTHER SUBROUTINES.                                                  LAKES.14
C   CALLED BY SUBROUTINE CRTER.                                                  LAKES.15
C                                                                                LAKES.16
C   COMMENTS :                                                                   LAKES.17
C BASED ON THE LAND-USE CATEGORY 7 (WATER) AND THE LATITUDES AND                 LAKES.18
C LONGITUDES OF THE MESOSCALE GRID, TERRAIN LOCATES THE GREAT                    LAKES.19
C LAKES REGION AND RE-SETS THE TERRAIN HEIGHT FOR THE GREAT LAKES.               LAKES.20
C                                                                                LAKES.21
C     LAKE NAME          HEIGHT(M)                                               LAKES.22
C     ERIE               174.0                                                   LAKES.23
C     HURON              177.0                                                   LAKES.24
C     ONTARIO             75.0                                                   LAKES.25
C     SUPERIOR           183.0                                                   LAKES.26
C     MICHIGAN           177.0                                                   LAKES.27
C                                                                                LAKES.28
      DIMENSION HT(IX,JX), FORCETER(IX,JX), XLAT(IX,JX), XLON(IX,JX)             LAKES.29
C                                                                                LAKES.30
      IF(IP.EQ.1)PRINT 1                                                         06NOV00.81
1     FORMAT(2X,'==> CALL LAKES <==')                                            LAKES.32
C                                                                                LAKES.33
      DO 10 I=1,IX                                                               LAKES.34
      DO 10 J=1,JX                                                               LAKES.35
C                                                                                LAKES.36
C  LAKE ERIE                                                                     LAKES.37
C                                                                                LAKES.38
      IF(XLAT(I,J).LE.43.2.AND.XLAT(I,J).GE.41.0.AND.XLON(I,J).LE.-78.0.         LAKES.39
     1   AND.XLON(I,J).GE.-84.0)THEN                                             LAKES.40
      IF(FORCETER(I,J).NE.9E9.OR.HT(I,J).LT.174.0)THEN                           LAKES.41
      HT(I,J)=174.0                                                              LAKES.42
      ENDIF                                                                      LAKES.43
      ENDIF                                                                      LAKES.44
C                                                                                LAKES.45
C  LAKE HURON                                                                    LAKES.46
C                                                                                LAKES.47
      IF(XLAT(I,J).LE.46.4.AND.XLAT(I,J).GE.43.0.AND.XLON(I,J).LE.-79.9.         LAKES.48
     1   AND.XLON(I,J).GE.-85.0)THEN                                             LAKES.49
      IF(FORCETER(I,J).NE.9E9.OR.HT(I,J).LT.177.0)THEN                           LAKES.50
      HT(I,J)=177.0                                                              LAKES.51
      ENDIF                                                                      LAKES.52
      ENDIF                                                                      LAKES.53
C                                                                                LAKES.54
C  LAKE ONTARIO                                                                  LAKES.55
C                                                                                LAKES.56
      IF(XLAT(I,J).LE.44.5.AND.XLAT(I,J).GE.43.2.AND.XLON(I,J).LE.-75.0.         LAKES.57
     1   AND.XLON(I,J).GE.-79.9)THEN                                             LAKES.58
      IF(FORCETER(I,J).NE.9E9.OR.HT(I,J).LT.75.0)THEN                            LAKES.59
      HT(I,J)=75.0                                                               LAKES.60
      ENDIF                                                                      LAKES.61
      ENDIF                                                                      LAKES.62
C                                                                                LAKES.63
C  LAKE SUPERIOR                                                                 LAKES.64
C                                                                                LAKES.65
      IF(XLAT(I,J).LE.49.4.AND.XLAT(I,J).GE.46.2.AND.XLON(I,J).LE.-84.2.         LAKES.66
     1   AND.XLON(I,J).GE.-93.0)THEN                                             LAKES.67
      IF(FORCETER(I,J).NE.9E9.OR.HT(I,J).LT.183.0)THEN                           LAKES.68
      HT(I,J)=183.0                                                              LAKES.69
      ENDIF                                                                      LAKES.70
      ENDIF                                                                      LAKES.71
C                                                                                LAKES.72
C  LAKE MICHIGAN                                                                 LAKES.73
C                                                                                LAKES.74
      IF(XLAT(I,J).LE.46.2.AND.XLAT(I,J).GE.41.0.AND.XLON(I,J).LE.-84.8.         LAKES.75
     1   AND.XLON(I,J).GE.-89.0)THEN                                             LAKES.76
      IF(FORCETER(I,J).NE.9E9.OR.HT(I,J).LT.177.0)THEN                           LAKES.77
      HT(I,J)=177.0                                                              LAKES.78
      ENDIF                                                                      LAKES.79
      ENDIF                                                                      LAKES.80
10    CONTINUE                                                                   LAKES.81
C                                                                                LAKES.82
      RETURN                                                                     LAKES.83
      END                                                                        LAKES.84
