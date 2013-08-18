      SUBROUTINE BLW(IL,JL,WTTOP,BLWXY,BLDUM2D,RINBLW,DX,TIMANLS,IN)             07NOV00.1109
#     include <parame.incl>                                                      BLW.3
C THIS SUBROUTINE COMPUTES THE BL WEIGHTING FUNCTION  BASED ON                   BLW.4
C TOPOGRAPHY (WXYTOP) AND SURFACE DATA DENSITY (BLWXY) FROM RAWINS.              BLW.5
C                                                                                BLW.6
C NOTE: THIS SUBROUTINE ASSUMES A WEIGHT OF 1 AT A GRID POINT                    BLW.7
C       WHEN AN OBSERVATION IS WITHIN RINBLW/2., AND DECREASES TO 0.2            BLW.8
C       WHEN THE CLOSEST OBSERVATION IS FROM RINBLW/2. TO RINBLW AWAY            BLW.9
C       FROM THE GRID POINT, AND IS 0.2 WHEN NO OBS ARE WITHIN RINBLW OF         BLW.10
C       THE GRID POINT.  THE LINEAR FUNCTION IS Y=M*X +B WHERE                   BLW.11
C       M (SLOPE) AND B (INTERCEPT) ARE ASSIGNED BELOW.                          BLW.12
C       THIS WEIGHT IS THEN MULTIPLIED BY A WEIGHT BASED ON                      BLW.13
C       TOPOGRAPHY (TERRAIN HEIGHT) COMPUTED IN INIT WHERE                       BLW.14
C       LAND POINTS ARE 1 AND WATER POINTS 0.2 WITH AN AREA                      BLW.15
C       OF 0.5 BETWEEN THEM.                                                     BLW.16
      DIMENSION WTTOP(MIXFG,MJXFG),BLWXY(MIXFG,MJXFG),                           BLW.17
     1     BLDUM2D(MIXFG,MJXFG)                                                  BLW.18
      REAL M                                                                     BLW.19
      RINDX=RINBLW*1000./DX                                                      BLW.21
      M=-0.8*2./RINDX                                                            BLW.22
      B=1.8                                                                      BLW.23
C5    FORMAT('IN BLW: RINBLW,RINDX,M,B,TIM,DTG',5(1X,F10.3),1X,A19)              BLW.25
      DO 10 I=1,IL                                                               BLW.26
        DO 10 J=1,JL                                                             BLW.27
C CHECK TO SEE IF ANY OBSERVATIONS ARE WITHIN RINDX OF THIS (I,J)                BLW.28
C FIRST CHECK AT (I,J) ITSELF...                                                 BLW.29
          IF(BLWXY(I,J).GT.0.5)THEN                                              BLW.30
            BLDUM2D(I,J)=1.0                                                     BLW.31
          ELSE                                                                   BLW.32
            MAXJ=J+IFIX(RINDX+0.99)                                              BLW.33
            MAXJ=MIN0(JL,MAXJ)                                                   BLW.34
            MINJ=J-IFIX(RINDX+0.99)                                              BLW.35
            MINJ=MAX0(1,MINJ)                                                    BLW.36
            MAXI=I+IFIX(RINDX+0.99)                                              BLW.37
            MAXI=MIN0(IL,MAXI)                                                   BLW.38
            MINI=I-IFIX(RINDX+0.99)                                              BLW.39
            MINI=MAX0(1,MINI)                                                    BLW.40
            DIJMIN=999.                                                          BLW.41
            DO 20 II=MINI,MAXI                                                   BLW.42
              DO 20 JJ=MINJ,MAXJ                                                 BLW.43
                IF(BLWXY(II,JJ).GT.0.5)THEN                                      BLW.44
                  DI=ABS(II-I)                                                   BLW.45
                  DJ=ABS(JJ-J)                                                   BLW.46
                  DIJ=SQRT(DI*DI+DJ*DJ)                                          BLW.47
                  DIJMIN=AMIN1(DIJMIN,DIJ)                                       BLW.48
                ENDIF                                                            BLW.49
   20         CONTINUE                                                           BLW.50
                                                                                 BLW.51
            DIJMIN=AMIN1(DIJMIN,RINDX)                                           BLW.52
            IF(DIJMIN.GT.RINDX/2.)THEN                                           BLW.53
              BLDUM2D(I,J)=M*DIJMIN+B                                            BLW.54
            ELSE                                                                 BLW.55
              BLDUM2D(I,J)=1.0                                                   BLW.56
            ENDIF                                                                BLW.57
          ENDIF                                                                  BLW.58
   10   CONTINUE                                                                 BLW.59
                                                                                 BLW.60
      XTH=TIMANLS/60.                                                            BLW.61
C NOW RUN V9 SMOOTHER/DESMOOTHER                                                 BLW.62
      CALL SMTHER(BLDUM2D,MIXFG,MJXFG,1,2,IL-1,2,JL-1)                           BLW.63
C NOW COMBINE SMOOTHED/DESMOOTHED WEIGHT BASED ON SURFACE DATA DENSITY           BLW.64
C WITH THAT BASED ON TOPOGRAPHY...                                               BLW.65
C REMOVE WTTOP (TOPOGRAPHICAL WEIGHTING: LAND=1, COAST=0.5,SEA=0.2) IF           BLW.66
C IWTTOP.NE.1                                                                    BLW.67
      IWTTOP=0                                                                   BLW.68
      IF(IWTTOP.EQ.1)THEN                                                        BLW.69
        DO 30 II=1,IL                                                            BLW.70
          DO 30 JJ=1,JL                                                          BLW.71
            BLWXY(II,JJ)=WTTOP(II,JJ)*BLDUM2D(II,JJ)                             BLW.72
   30     CONTINUE                                                               BLW.73
                                                                                 BLW.74
        CALL SMTHER(BLWXY,MIXFG,MJXFG,1,2,IL-1,2,JL-1)                           BLW.75
      ELSE                                                                       BLW.76
        DO 31 II=1,IL                                                            BLW.77
          DO 31 JJ=1,JL                                                          BLW.78
            BLWXY(II,JJ)=BLDUM2D(II,JJ)                                          BLW.79
   31     CONTINUE                                                               BLW.80
                                                                                 BLW.81
      ENDIF                                                                      BLW.82
C MAKE SURE FINAL WEIGHTING FUNCTION IS BOUNDED BY 0. AND 1.                     BLW.83
      DO 40 I=1,IL                                                               BLW.84
        DO 40 J=1,JL                                                             BLW.85
          BLWXY(I,J)=AMIN1(BLWXY(I,J),1.0)                                       BLW.86
          BLWXY(I,J)=AMAX1(BLWXY(I,J),0.0)                                       BLW.87
   40   CONTINUE                                                                 BLW.88
                                                                                 BLW.89
      RETURN                                                                     BLW.90
      END                                                                        BLW.91
                                                                                 BLW.92
