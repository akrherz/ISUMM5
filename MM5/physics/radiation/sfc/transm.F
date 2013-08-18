#ifndef MPP1_COLUMNWISE                                                          TRANSM.1
      SUBROUTINE TRANSM(PATH,PRW,FTABS,FTSCAT,FBSCAT,PSB,J,IYY)                  TRANSM.2
#else                                                                            TRANSM.3
      SUBROUTINE TRANSM_COL(PATH,PRW,FTABS,FTSCAT,FBSCAT,PSB,I,J,IYY)            TRANSM.4
#endif                                                                           TRANSM.5
      IMPLICIT NONE                                                              TRANSM.6
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          TRANSM.7
C                                                                     C          TRANSM.8
C                                                                     C          TRANSM.9
C     THIS SUBROUTINE OBTAINS SW TRANSMISSIVITY AS A FUNCTION OF      C          TRANSM.10
C       1) PATH LENGTH (1-10) (PATH LENGTH = 1 WHEN ZENITH ANGLE = 0),C          TRANSM.11
C       2) PRECIPITABLE WATER (0-5 CM)                                C          TRANSM.12
C     USING BILINEAR INTERPOLATION FROM LOOK-UP TABLES DETERMINED     C          TRANSM.13
C     A SHORTWAVE MODEL OF CARLSON AND BOLAND (1978, JAM)             C          TRANSM.14
C                                                                     C          TRANSM.15
C                                                                     C          TRANSM.16
C     PATH   : PATH LENGTH.                                - INPUT    C          TRANSM.17
C                                                                     C          TRANSM.18
C     PRW    : PRECIPITABLE WATER IN CM.                   - INPUT    C          TRANSM.19
C                                                                     C          TRANSM.20
C     FTABS  : ABSORPTION TRANSMISSIVITY (0 - 1).          - OUTPUT   C          TRANSM.21
C                                                                     C          TRANSM.22
C     FTSCAT : SCATTERING TRANSMISSIVITY (0 - 1).          - OUTPUT   C          TRANSM.23
C                                                                     C          TRANSM.24
C     FBSCAT : BACKSCATTERING COEFFICIENT (0 - 1).         - OUTPUT   C          TRANSM.25
C              (PERCENT OF SCATTERED RADIATION WHICH                  C          TRANSM.26
C               IS BACKSCATTERED. THE REST IS FORWARD                 C          TRANSM.27
C               SCATTERED.)                                           C          TRANSM.28
C                                                                     C          TRANSM.29
C     SP     : PERCENT OF STANDARD SURFACE PRESSURE.       - INPUT    C          TRANSM.30
C              (SURFACE PRESSURE IN CB / 101.325 CB.)                 C          TRANSM.31
C                                                                     C          TRANSM.32
C                                                                     C          TRANSM.33
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          TRANSM.34
#     include <parame.incl>                                                      TRANSM.35
C                                                                                TRANSM.36
      REAL      PATH(MIX), FTABS(MIX), FTSCAT(MIX), FBSCAT(MIX)                  TRANSM.37
      REAL      PRW(MIX,MJX),PSB(MIX,MJX)                                        TRANSM.38
      INTEGER   IYY,ILX                                                          TRANSM.39
      INTEGER   I,J,K                                                            TRANSM.40
      INTEGER   IPATH,JPATH,IOM,JOM                                              TRANSM.41
      REAL      FRACT,FRACT1,FRACT2,GRACT,GRACT1,GRACT2,FTA,FTS,SP               TRANSM.42
#     include <param3.incl>                                                      TRANSM.43
#     include <nonhyd.incl>                                                      TRANSM.44
C                                                                                TRANSM.45
C----------------------------------------------------------------------          TRANSM.46
C                                                                                TRANSM.47
      ILX=IYY-1                                                                  TRANSM.48
C                                                                                TRANSM.49
      DO 10 I=1,ILX                                                              TRANSM.50
        PATH(I)=AMIN1(PATH(I),9.99)                                              TRANSM.51
        PATH(I)=AMAX1(PATH(I),1.01)                                              TRANSM.52
        PRW(I,J)=AMIN1(PRW(I,J),4.99)                                            TRANSM.53
        PRW(I,J)=AMAX1(PRW(I,J),0.01)                                            TRANSM.54
   10 CONTINUE                                                                   TRANSM.55
C                                                                                TRANSM.56
C-----46 PATH LENGTHS, 1-10, INTERVAL=0.2                                        TRANSM.57
C                                                                                TRANSM.58
CDIR$ IVDEP                                                                      TRANSM.59
      DO 20 I=1,ILX                                                              TRANSM.60
        FRACT=5.*(PATH(I)-1.)+1.                                                 TRANSM.61
        IPATH=IFIX(FRACT+.001)                                                   TRANSM.62
        JPATH=IPATH+1                                                            TRANSM.63
        FRACT1=FRACT-IPATH                                                       TRANSM.64
        FRACT2=1.-FRACT1                                                         TRANSM.65
C                                                                                TRANSM.66
C-----11 PRECIP WATER AMTS., 0-5 CM, INTERVAL=0.5                                TRANSM.67
C                                                                                TRANSM.68
        GRACT=2.*PRW(I,J)+1.                                                     TRANSM.69
        IOM=IFIX(GRACT+.001)                                                     TRANSM.70
        JOM=IOM+1                                                                TRANSM.71
        GRACT1=GRACT-IOM                                                         TRANSM.72
        GRACT2=1.-GRACT1                                                         TRANSM.73
C                                                                                TRANSM.74
C                                                                                TRANSM.75
C-----INTERPOLATE TO GET ABS, SCAT, BSCAT VALUES                                 TRANSM.76
C        - BILINEAR INTERPOLATION FOR ABS (F(PATH,OMEGA)                         TRANSM.77
C        - LINEAR INTERPOLATION FOR SCAT, BSCAT (F(PATH ONLY))                   TRANSM.78
C                                                                                TRANSM.79
        FTA=FRACT2*GRACT2*TRAN(IPATH,IOM)+FRACT1*GRACT1*TRAN(JPATH,JOM)+         TRANSM.80
     +      FRACT1*GRACT2*TRAN(JPATH,IOM)+FRACT2*GRACT1*TRAN(IPATH,JOM)          TRANSM.81
        FTS=FRACT2*TRAN(IPATH,12)+FRACT1*TRAN(JPATH,12)                          TRANSM.82
        FBSCAT(I)=FRACT2*TRAN(IPATH,13)+FRACT1*TRAN(JPATH,13)                    TRANSM.83
C                                                                                TRANSM.84
C-----TRANSMISSIONS FROM TABLE ARE VALID FOR SURFACE PRESSURE OF                 TRANSM.85
C     1013.25 MB.  WE CORRECT BELOW FOR THE ACTUAL SURFACE PRESSURE              TRANSM.86
C     ASSUMING THAT ATTENUATIVE EFFECTS OF WATER VAPOR ARE                       TRANSM.87
C     SECONDARY TO THOSE OF OTHER ACTIVE CONSTITUENTS                            TRANSM.88
C     (AIR MOLECULES, AEROSOLS, OZONE)                                           TRANSM.89
C                                                                                TRANSM.90
        SP=(PSB(I,J)+PTOP)/101.325+PPB(I,J,KX)/PSB(I,J)/101.325/1000.            TRANSM.91
        FTABS(I)=SP*(FTA-1.)+1.                                                  TRANSM.92
        FTSCAT(I)=SP*(FTS-1.)+1.                                                 TRANSM.93
   20 CONTINUE                                                                   TRANSM.94
C                                                                                TRANSM.95
      RETURN                                                                     TRANSM.96
      END                                                                        TRANSM.97
                                                                                 TRANSM.98
