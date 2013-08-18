      SUBROUTINE QFLUX(ILX,J,CQ4,RAW,XLAND)                                      QFLUX.1
      IMPLICIT NONE                                                              QFLUX.2
C***********************************************************************         QFLUX.3
C                                                                                QFLUX.4
C  FUNCTION:                                                                     QFLUX.5
C    THIS SUBROUTINE COMPUTES EVAPORATION FROM BARE SOIL (EG) AND FROM           QFLUX.6
C    THE WET PART OF CANOPY (ER) AND TRANSPIRATION FROM THE DRY PART OF          QFLUX.7
C    CANOPY (ETR).                                                               QFLUX.8
C                                                                                QFLUX.9
C  REVISION HISTORY:                                                             QFLUX.10
C    J. Pleim     Dec  1999  - v3                                                QFLUX.11
C    J. Pleim     Sept 1996  - Vectorize!!!!                                     QFLUX.12
C    A. XIU       JULY 1993                                                      QFLUX.13
C    J. PLEIM     APRIL 1993                                                     QFLUX.14
C                                                                                QFLUX.15
C***********************************************************************         QFLUX.16
#     include <parame.incl>                                                      QFLUX.17
#     include <comicl.incl>                                                      QFLUX.18
#     include <soilp.incl>                                                       QFLUX.19
C                                                                                QFLUX.20
C.......Arguments                                                                QFLUX.21
      REAL CQ4(MIX), RADF                                                        QFLUX.22
      REAL RAW(MIX),XLAND(MIX,MJX)                                               QFLUX.23
      INTEGER ILX,J                                                              QFLUX.24
                                                                                 QFLUX.25
C.......Local Arrays                                                             QFLUX.26
      REAL FTOT(MIX), F1(MIX),F4(MIX),SIGG(MIX)                                  QFLUX.27
                                                                                 QFLUX.28
C.......Local variables                                                          QFLUX.29
      INTEGER ISTI                                                               QFLUX.30
      REAL WRMAX,DELTA,RADL                                                      QFLUX.31
      REAL W2AVAIL,W2MXAV                                                        QFLUX.32
      REAL F2,F3,FSHELT,FX                                                       QFLUX.33
      REAL GS,GA                                                                 QFLUX.34
      REAL RSMAX,FTMIN,F3MIN                                                     QFLUX.35
      DATA RSMAX/5000./ ! S/M                                                    QFLUX.36
      DATA FTMIN/0.0000001/  ! M/S                                               QFLUX.37
      DATA F3MIN/0.25/                                                           QFLUX.38
C.......Indexes                                                                  QFLUX.39
      INTEGER I                                                                  QFLUX.40
                                                                                 QFLUX.41
      DO 20 I = 1,ILX                                                            QFLUX.42
C-- for water surface, no canopy evaporation and transpiration                   QFLUX.43
          ER(I) = 0.0                                                            QFLUX.44
          ETR(I) = 0.0                                                           QFLUX.45
20    CONTINUE                                                                   QFLUX.46
                                                                                 QFLUX.47
C-- GROUND EVAPORATION (DEPOSITION)                                              QFLUX.48
      DO 10 I = 1,ILX                                                            QFLUX.49
        IF(QSS(I).LT.QVS(I,1)) BETAP(I)=1.                                       QFLUX.50
        EG(I) = DENS(I,1) * (1.0 - VEGFRC(I,J))*BETAP(I)*(QSS(I) -               QFLUX.51
     &          QVS(I,1)) / RAW(I)                                               QFLUX.52
10    CONTINUE                                                                   QFLUX.53
                                                                                 QFLUX.54
C-- CANOPY                                                                       QFLUX.55
      DO 30 I = 1,ILX                                                            QFLUX.56
        ISTI = ISLTYP(I,J)                                                       QFLUX.57
        IF (XLAND(I,J).LT.1.5 .AND. ISTI .GT. 0)  THEN                           QFLUX.58
          WRMAX = 0.2E-3 * VEGFRC(I,J) * LAI(I,J)    ! in unit M                 QFLUX.59
          IF (WR(I) .LE. 0.0) THEN                                               QFLUX.60
            DELTA = 0.0                                                          QFLUX.61
          ELSE                                                                   QFLUX.62
C            DELTA = (WR(I) / WRMAX) ** 0.66667                                  QFLUX.63
            DELTA = WR(I) / WRMAX           ! referred to SiB model              QFLUX.64
          ENDIF                                                                  QFLUX.65
          IF (QSS(I) .GE. QVS(I,1)) THEN                                         QFLUX.66
            SIGG(I) = DELTA                                                      QFLUX.67
          ELSE                                                                   QFLUX.68
            SIGG(I) = 1.0                                                        QFLUX.69
          ENDIF                                                                  QFLUX.70
                                                                                 QFLUX.71
C          sigg = 0.  ! test of no canopy flux                                   QFLUX.72
          ER(I) = DENS(I,1) * VEGFRC(I,J) * SIGG(I) * (QSS(I) -                  QFLUX.73
     &            QVS(I,1)) / RAW(I)                                             QFLUX.74
        ENDIF                                                                    QFLUX.75
30    CONTINUE                                                                   QFLUX.76
                                                                                 QFLUX.77
C-- TRANSPIRATION                                                                QFLUX.78
      DO 40 I = 1,ILX                                                            QFLUX.79
        ISTI = ISLTYP(I,J)                                                       QFLUX.80
        IF (XLAND(I,J).LT.1.5 .AND. ISTI .GT. 0)  THEN                           QFLUX.81
C                                                                                QFLUX.82
C-RADIATION                                                                      QFLUX.83
          IF(RSTMIN(I,J).GT.130.) THEN                                           QFLUX.84
             RADL = 30.  ! W/M2                                                  QFLUX.85
           ELSE                                                                  QFLUX.86
             RADL = 100. ! W/M2                                                  QFLUX.87
          ENDIF                                                                  QFLUX.88
C          RADF = 1.1 * RG(I) / RADL  ! NP89 - EQN34 but no LAI                  QFLUX.89
          RADF = 1.1 * RG(I) / (RADL*LAI(I,J))  ! NP89 - EQN34                   QFLUX.90
          F1(I) = (RSTMIN(I,J) / RSMAX + RADF) / (1.0 + RADF)                    QFLUX.91
C-SOIL MOISTURE                                                                  QFLUX.92
          W2AVAIL = W2(I) - WWLT(ISTI)                                           QFLUX.93
          W2MXAV = WFC(ISTI) - WWLT(ISTI)                                        QFLUX.94
          F2 = 1.0 / (1.0 + EXP(-5.0 * ( W2AVAIL / W2MXAV -                      QFLUX.95
     &         (W2MXAV / 3.0 + WWLT(ISTI)))))    ! according JP, 9/94            QFLUX.96
C                                                                                QFLUX.97
C-AIR TEMP                                                                       QFLUX.98
C... according to Avissar (1985) and AX 7/95                                     QFLUX.99
          IF (TT(I,1) .LE. 302.15) THEN                                          QFLUX.100
            F4(I) = 1.0 / (1.0 + EXP(-0.41 * (TT(I,1) - 282.05)))                QFLUX.101
          ELSE                                                                   QFLUX.102
            F4(I) = 1.0 / (1.0 + EXP(0.5 * (TT(I,1) - 314.0)))                   QFLUX.103
          ENDIF                                                                  QFLUX.104
C...                                                                             QFLUX.105
          FTOT(I) = LAI(I,J) * F1(I) * F2 * F4(I)                                QFLUX.106
          FTOT(I) = AMAX1(FTOT(I),FTMIN)                                         QFLUX.107
        ENDIF                                                                    QFLUX.108
40    CONTINUE                                                                   QFLUX.109
                                                                                 QFLUX.110
      DO 50 I = 1,ILX                                                            QFLUX.111
        ISTI = ISLTYP(I,J)                                                       QFLUX.112
        IF (XLAND(I,J).LT.1.5 .AND. ISTI .GT. 0)  THEN                           QFLUX.113
C... adding shelter effect since RS is not linear function of LAI,               QFLUX.114
C...   Mascart et al.1991 and JP,9/94; updated 6/95 Kelliher etal 95             QFLUX.115
C          FSHELT = AMAX1(0.3 * LAI(I,J) + 0.7,1.0)  ! updated 4/97              QFLUX.116
          FSHELT = 1.0   ! go back to NP89                                       QFLUX.117
          GS = FTOT(I) / (RSTMIN(I,J) * FSHELT)                                  QFLUX.118
          GA = 1.0 / RAW(I)                                                      QFLUX.119
C-- Compute humidity effect according to RH at leaf surf                         QFLUX.120
          F3 = 0.5 * (GS - GA + SQRT(GA * GA + GA * GS *                         QFLUX.121
     &         (4.0 * QVS(I,1) / QSS(I) - 2.0) + GS * GS)) / GS                  QFLUX.122
          F3 = AMIN1(AMAX1(F3,F3MIN),1.0)                                        QFLUX.123
          RS(I,J) = 1.0 / (GS * F3)                                              QFLUX.124
                                                                                 QFLUX.125
C--- Compute Assimilation factor for soil moisture nudging - jp 12/94            QFLUX.126
C--     Note that the 30 coef is to result in order 1 factor for max             QFLUX.127
          IF (RG(I) .LT. 0.00001) THEN   ! do not nudge during night             QFLUX.128
            FX = 0.0                                                             QFLUX.129
          ELSE                                                                   QFLUX.130
            FX = 30.*F1(I)*F4(I)*LAI(I,J)/(RSTMIN(I,J)*FSHELT)                   QFLUX.131
          ENDIF                                                                  QFLUX.132
          fass(i) = fx                                                           QFLUX.133
          ETR(I) = DENS(I,1) * VEGFRC(I,J) * (1.0 - SIGG(I)) * (QSS(I)           QFLUX.134
     &           - QVS(I,1)) / (RAW(I) + RS(I,J))                                QFLUX.135
C..... CQ4 is used for the implicit calculation of TG in SURFACE                 QFLUX.136
          CQ4(I) = DENS(I,1) * VEGFRC(I,J) * ((1.0 - SIGG(I)) /                  QFLUX.137
     &             (RAW(I) + RS(I,J)) + SIGG(I) / RAW(I))                        QFLUX.138
        ENDIF         ! endif for if XLAND                                       QFLUX.139
50    CONTINUE                                                                   QFLUX.140
      RETURN                                                                     QFLUX.141
      END                                                                        QFLUX.142
