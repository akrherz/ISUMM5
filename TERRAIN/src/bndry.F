      SUBROUTINE BNDRY(IX,JX,N,HT,LAND,LNDOUT)                                   BNDRY.1
C                                                                                BNDRY.2
C   PURPOSE      : ZERO THE TERRAIN HEIGHT AND LAND-USE NORMAL                   BNDRY.3
C                  GRADIENT AT THE BOUNDARY OF THE DOMAIN N.                     BNDRY.4
C   IX, JX       : THE DIMENSION OF THE MESOSCALE DOMAIN N.                      BNDRY.5
C   N            : THE ID NUMBER OF THE MESOSCALE DOMAIN.                        BNDRY.6
C   HT(IX,JX)    : THE TERRAIN HEIGHT FIELD.                                     BNDRY.7
C   LAND(IX,JX)  : THE LAND-USE FIELD. THIS IS AN INTEGER ARRAY.                 BNDRY.8
C   LNDOUT(IX,JX): THE LAND-USE FIELD. THIS IS A REAL ARRAY FOR THE              BNDRY.9
C                  USE OF PRINT AND PLOT.                                        BNDRY.10
C   HT(IX,JX)    : THE TERRAIN HEIGHT FIELD WITH THE ZERO NORMAL                 BNDRY.11
C                  GRADIENT AT THE BOUNDARY.                                     BNDRY.12
C   LAND(IX,JX)  : THE LAND-USE FIELD WITH THE ZERO NORMAL GRADIENT              BNDRY.13
C                  AT THE BOUNDARY.                                              BNDRY.14
C   LNDOUT(IX,JX): THE LAND-USE FIELD WITH THE ZERO NORMAL GRADIENT              BNDRY.15
C                  AT THE BOUNDARY.                                              BNDRY.16
C                                                                                BNDRY.17
C   CALLS NO OTHER SUBROUTINES.                                                  BNDRY.18
C   CALLED BY SUBROUTINE TERDRV.                                                 BNDRY.19
C   COMMENTS :                                                                   BNDRY.20
C                                                                                BNDRY.21
C BY SETTING THE VALUES AT THE BOUNDARY EQUAL TO THE VALUES ONE                  BNDRY.22
C GRID POINT INSIDE THE BOUNDARY, THE NORMAL GRADIENT AT THE BOUNDARY            BNDRY.23
C BECOMES ZERO. MOST OF THE NUMERICAL MODELS TAKE THIS PROVISION TO              BNDRY.24
C AVOID THE COMPUTATIONAL TROUBLE NEAR THE BOUNDARY.                             BNDRY.25
C                                                                                BNDRY.26
C IN THE MM5 MODELING SYSTEM, THE TERRAIN HEIGHT AND LAND-USE ARE                BNDRY.27
C DEFINED AT THE CROSS POINTS WITH THE DIMENSIONS OF IX-1 AND JX-1.              BNDRY.28
C THE VALUE OF 10E36 IS ASSIGNED TO THE LAST ROWS AND COLUMNS OF                 BNDRY.29
C THE ARRAY HT (HT(I,JX) AND HT(IX,J)) AND  10E7  TO THE ARRAYS                  BNDRY.30
C LAND (LAND(I,JX) AND LAND(IX,J)) AND LNDOUT (LNDOUT(I,JX) AND                  BNDRY.31
C LNDOUT(IX,J)).                                                                 BNDRY.32
C                                                                                BNDRY.33
# include <maps.incl>                                                            BNDRY.34
# include <option.incl>                                                          BNDRY.35
C                                                                                BNDRY.36
      DIMENSION HT(IX,JX), LAND(IX,JX), LNDOUT(IX,JX)                            BNDRY.37
      REAL LNDOUT                                                                BNDRY.38
C                                                                                BNDRY.39
        IEX = 0                                                                  BNDRY.40
        JEX = 0                                                                  BNDRY.41
        IF (N.EQ.1) THEN                                                         BNDRY.42
         IEX  = IOFFST                                                           BNDRY.43
         JEX  = JOFFST                                                           BNDRY.44
        ENDIF                                                                    BNDRY.45
         IB1  = IEX + 1                                                          BNDRY.46
         IB2  = IB1 + 1                                                          BNDRY.47
         JB1  = JEX + 1                                                          BNDRY.48
         JB2  = JB1 + 1                                                          BNDRY.49
         IBMX = IX - IEX                                                         BNDRY.50
         JBMX = JX - JEX                                                         BNDRY.51
         IE   = IBMX - 1                                                         BNDRY.52
         JE   = JBMX - 1                                                         BNDRY.53
         IE1  = IE-1                                                             BNDRY.54
         JE1  = JE-1                                                             BNDRY.55
C                                                                                BNDRY.56
        IF(IPRINT.EQ.1) THEN                                                     BNDRY.57
        PRINT 5,N,IEX,JEX                                                        BNDRY.58
5       FORMAT(2X,'==> CALL BNDRY: NEST-',I2,'  IEX=',I2,' JEX=',I2)             BNDRY.59
        ENDIF                                                                    BNDRY.60
C                                                                                BNDRY.61
         DO 10 J = JB2,JE1                                                       BNDRY.62
         HT(IB1,J) = HT(IB2,J)                                                   BNDRY.63
         HT(IE,J)  = HT(IE1,J)                                                   BNDRY.64
         LAND(IB1,J) = LAND(IB2,J)                                               BNDRY.65
         LNDOUT(IB1,J) = LNDOUT(IB2,J)                                           BNDRY.66
         LAND(IE,J)  = LAND(IE1,J)                                               BNDRY.67
10       LNDOUT(IE,J)  = LNDOUT(IE1,J)                                           BNDRY.68
         DO 20 I = IB1,IE                                                        BNDRY.69
         HT(I,JB1) = HT(I,JB2)                                                   BNDRY.70
         HT(I,JE)  = HT(I,JE1)                                                   BNDRY.71
         LAND(I,JB1) = LAND(I,JB2)                                               BNDRY.72
         LNDOUT(I,JB1) = LNDOUT(I,JB2)                                           BNDRY.73
         LAND(I,JE)  = LAND(I,JE1)                                               BNDRY.74
20       LNDOUT(I,JE)  = LNDOUT(I,JE1)                                           BNDRY.75
C                                                                                BNDRY.76
      DO 40 I=1,IX                                                               BNDRY.77
      HT(I,JX) = 1.E20                                                           BNDRY.78
      LAND(I,JX) = 1E7                                                           BNDRY.79
40    LNDOUT(I,JX) = 1.E20                                                       BNDRY.80
      DO 50 J=1,JX                                                               BNDRY.81
      HT(IX,J) = 1.E20                                                           BNDRY.82
      LAND(IX,J) = 1E7                                                           BNDRY.83
50    LNDOUT(IX,J) = 1.E20                                                       BNDRY.84
C                                                                                BNDRY.85
      RETURN                                                                     BNDRY.86
      END                                                                        BNDRY.87
