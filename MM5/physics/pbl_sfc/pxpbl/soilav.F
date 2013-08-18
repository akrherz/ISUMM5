      SUBROUTINE SOILAV(SOLCAT,ILX,JLX,ISLTYP)                                   SOILAV.1
      IMPLICIT NONE                                                              SOILAV.2
                                                                                 SOILAV.3
C***************************************************************************     SOILAV.4
C   SOILGRID                                                                     SOILAV.5
C                                                                                SOILAV.6
C   THIS PROGRAM COMPUTES GRID AGGREGATE SOIL TYPES FOR USE IN THE PLEIM-XIU     SOILAV.7
C   SURFACE MODEL.  AGGREGATION IS A SIMPLE LINEAR AVERAGE OF SAND AND           SOILAV.8
C   CLAY CONTENT AS RECOMENDED BY NOILHAN AND LACARRERE (J. OF CLIMO 1995).      SOILAV.9
C                                                                                SOILAV.10
C-- ARGUMENT LIST:                                                               SOILAV.11
C   SOLCAT      : PERCENT AREA OF EACH SOIL CAT BY GRID CELL                     SOILAV.12
C                                                                                SOILAV.13
C*****************************************************************************   SOILAV.14
C                                                                                SOILAV.15
      INTEGER MXSTYPS, MXTEX                                                     SOILAV.16
      PARAMETER (MXSTYPS = 17, MXTEX = 12)                                       SOILAV.17
C  MXSTYPS   -  NUMBER OF SOIL TYPES ON TERRAIN INPUT FILE                       SOILAV.18
C  MXTEX     -  NUMBER OF SOIL TEXTURE CLASSES                                   SOILAV.19
C                                                                                SOILAV.20
C  NOTE THAT THE 12 CLASSES FROM STATSGO ARE COLLAPSED TO 11 FOR PX-LSM          SOILAV.21
C   -- Sill and Silt are combined                                                SOILAV.22
*******************************************************************************  SOILAV.23
C--------------------------------------                                          SOILAV.24
C-- SOIL PARAMETERS ARE SPECIFIED BY SOIL TYPE:                                  SOILAV.25
C   #  SOIL TYPE  CODE  SAND  CLAY                                               SOILAV.26
C   _  _________  ____  ____  ____                                               SOILAV.27
C   1  SAND       Sand  92.5   2.1                                               SOILAV.28
C   2  LOAMY SAND Lsan  80.5   4.1                                               SOILAV.29
C   3  SANDY LOAM Sloa  61.1  10.9                                               SOILAV.30
C   4  SILT LOAM  Sill  19.6  19.1                                               SOILAV.31
C   5  SILT       Silt   4.0   7.3                                               SOILAV.32
C   6  LOAM       Loam  40.0  18.8                                               SOILAV.33
C   7  SND CLY LM Sclo  57.1  23.3                                               SOILAV.34
C   8  SLT CLY LM Sicl  11.3  32.2                                               SOILAV.35
C   9  CLAY LOAM  Cllo  26.8  36.6                                               SOILAV.36
C  10  SANDY CLAY Sacl  52.0  43.0                                               SOILAV.37
C  11  SILTY CLAY Sicy   6.5  46.2                                               SOILAV.38
C  12  CLAY       Clay  10.2  58.8                                               SOILAV.39
C  13  ORG MAT    Ormt                                                           SOILAV.40
C  14  WATER      Wate                                                           SOILAV.41
C  15  BEDROCK    Bedr                                                           SOILAV.42
C  16  OTHER      Othe                                                           SOILAV.43
C  17  No Data    Nodt                                                           SOILAV.44
C                                                                                SOILAV.45
#     include <parame.incl>                                                      SOILAV.46
      CHARACTER*4 AVCLASS                                                        SOILAV.47
      CHARACTER*4 TEXID(MXSTYPS)                                                 SOILAV.48
      REAL SOLCAT(MIXL,MJXL,MXSTYPS)                                             SOILAV.49
      REAL SAND(MXTEX), CLAY(MXTEX)                                              SOILAV.50
      INTEGER ISLTYP(MIXL,MJXL)                                                  SOILAV.51
                                                                                 SOILAV.52
      INTEGER STYP                                                               SOILAV.53
      INTEGER IRT,JCT,K                                                          SOILAV.54
      INTEGER ILX,JLX                                                            SOILAV.55
      REAL SUMAR,SUMSND,SUMCLY,FMAX,FAREA                                        SOILAV.56
      REAL AVS,AVC,AVSLT                                                         SOILAV.57
                                                                                 SOILAV.58
      DATA SAND/92.5,80.5,61.1,19.6,4.0,40.0,57.1,11.3,26.8,                     SOILAV.59
     >          52.0,6.5,10.2/                                                   SOILAV.60
      DATA CLAY/2.1,4.1,10.9,19.1,7.3,18.8,23.3,32.2,36.6,                       SOILAV.61
     >          43.0,46.2,58.8/                                                  SOILAV.62
      DATA TEXID/'Sand','Lsan','Sloa','Sill','Silt','Loam','Sclo',               SOILAV.63
     >           'Sicl','Cllo','Sacl','Sicy','Clay','Ormt','Wate',               SOILAV.64
     >           'Bedr','Othe','Nodt'/                                           SOILAV.65
                                                                                 SOILAV.66
C...START OF GRID CELL BY GRID CELL PROCESSING                                   SOILAV.67
                                                                                 SOILAV.68
      DO 99 IRT=1,ILX                                                            SOILAV.69
      DO 99 JCT=1,JLX                                                            SOILAV.70
                                                                                 SOILAV.71
      IF(SOLCAT(IRT,JCT,MXSTYPS).GT.99.0) THEN                                   SOILAV.72
        PRINT *,' NO soil DATA AT I=',IRT,' J=',JCT                              SOILAV.73
        STOP                                                                     SOILAV.74
      ENDIF                                                                      SOILAV.75
                                                                                 SOILAV.76
      SUMAR  = 0.0                                                               SOILAV.77
      SUMSND = 0.0                                                               SOILAV.78
      SUMCLY = 0.0                                                               SOILAV.79
      FMAX   = 0.0                                                               SOILAV.80
                                                                                 SOILAV.81
      DO K = 1,MXTEX                                                             SOILAV.82
         FAREA = SOLCAT(IRT,JCT,K)                                               SOILAV.83
         SUMAR = SUMAR + FAREA                                                   SOILAV.84
         SUMSND = SUMSND + SAND(K) * FAREA                                       SOILAV.85
         SUMCLY = SUMCLY + CLAY(K) * FAREA                                       SOILAV.86
      ENDDO                                                                      SOILAV.87
                                                                                 SOILAV.88
      DO K = MXTEX+1,MXSTYPS                                                     SOILAV.89
        IF(SOLCAT(IRT,JCT,K).GT.FMAX) THEN                                       SOILAV.90
          FMAX=SOLCAT(IRT,JCT,K)                                                 SOILAV.91
          STYP=K                                                                 SOILAV.92
        ENDIF                                                                    SOILAV.93
      ENDDO                                                                      SOILAV.94
                                                                                 SOILAV.95
C--- CALCULATE AVERAGE SAND AND CLAY CONTENT                                     SOILAV.96
      IF(SUMAR.GT.0.001) THEN                                                    SOILAV.97
        AVS = SUMSND / SUMAR                                                     SOILAV.98
        AVC = SUMCLY / SUMAR                                                     SOILAV.99
        AVSLT = 100 - AVS - AVC                                                  SOILAV.100
                                                                                 SOILAV.101
C--- CLASSIFY GRID AVGS                                                          SOILAV.102
                                                                                 SOILAV.103
                                                                                 SOILAV.104
       IF(AVS.GT.85..AND.AVC.LT.100.-AVS) THEN                                   SOILAV.105
          AVCLASS= 'Sand'                                                        SOILAV.106
          STYP = 1                                                               SOILAV.107
        ELSE IF(AVS.GT.70..AND.AVC.LT.100.-AVS) THEN                             SOILAV.108
          AVCLASS= 'Lsan'                                                        SOILAV.109
          STYP = 2                                                               SOILAV.110
        ELSE IF((AVC.LT.20..AND.AVS.GT.52..AND.AVC.GT.7.5)                       SOILAV.111
     >          .OR.(AVC.LE.7.5.AND.AVSLT.LT.50.)) THEN                          SOILAV.112
          AVCLASS= 'Sloa'                                                        SOILAV.113
          STYP = 3                                                               SOILAV.114
        ELSE IF(AVC.LT.35..AND.AVS.GT.45..AND.AVSLT.LT.28.) THEN                 SOILAV.115
          AVCLASS= 'Sclo'                                                        SOILAV.116
          STYP = 6                                                               SOILAV.117
        ELSE IF(AVC.GE.35..AND.AVS.GT.45.) THEN                                  SOILAV.118
          AVCLASS = 'Sacl'                                                       SOILAV.119
          STYP = 9                                                               SOILAV.120
        ELSE IF(AVC.LT.27.0.AND.AVSLT.LT.50.) THEN                               SOILAV.121
          AVCLASS= 'Loam'                                                        SOILAV.122
          STYP = 5                                                               SOILAV.123
        ELSE IF(AVC.LT.12..AND.AVSLT.GT.80.) THEN                                SOILAV.124
          AVCLASS = 'Silt'                                                       SOILAV.125
          STYP = 4                                                               SOILAV.126
        ELSE IF(AVC.LT.27.) THEN                                                 SOILAV.127
          AVCLASS = 'Sill'                                                       SOILAV.128
          STYP = 4                                                               SOILAV.129
        ELSE IF(AVC.LT.40..AND.AVS.GT.20.) THEN                                  SOILAV.130
          AVCLASS = 'Cllo'                                                       SOILAV.131
          STYP = 8                                                               SOILAV.132
        ELSE IF(AVC.LT.40.) THEN                                                 SOILAV.133
          AVCLASS = 'Sicl'                                                       SOILAV.134
          STYP = 7                                                               SOILAV.135
        ELSE IF(AVSLT.GE.40.) THEN                                               SOILAV.136
          AVCLASS = 'Sicy'                                                       SOILAV.137
          STYP = 10                                                              SOILAV.138
        ELSE                                                                     SOILAV.139
          AVCLASS = 'Clay'                                                       SOILAV.140
          STYP = 11                                                              SOILAV.141
        ENDIF                                                                    SOILAV.142
      ELSE                                                                       SOILAV.143
C--If Dominant Class is Organic Matter, Change to Loam (5)                       SOILAV.144
        IF(STYP.EQ.13) STYP = 5                                                  SOILAV.145
        AVCLASS = TEXID(STYP)                                                    SOILAV.146
      ENDIF                                                                      SOILAV.147
      ISLTYP(IRT,JCT) = STYP                                                     SOILAV.148
                                                                                 SOILAV.149
C      WRITE(22,805) JCT,IRT,STYP,AVCLASS,                                       SOILAV.150
C     >        AVS,AVC, SUMAR                                                    SOILAV.151
C805   FORMAT (2I4, 2X, I2,2X,A4,3F10.2)                                         SOILAV.152
                                                                                 SOILAV.153
99    CONTINUE                                                                   SOILAV.154
                                                                                 SOILAV.155
      RETURN                                                                     SOILAV.156
      END                                                                        SOILAV.157
