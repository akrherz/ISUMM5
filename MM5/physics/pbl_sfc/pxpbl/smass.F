      SUBROUTINE SMASS (ALPH1, ALPH2, BET1,BET2,J,ILX)                           SMASS.1
      IMPLICIT NONE                                                              SMASS.2
C*******************************************************************             SMASS.3
C     SMASS COMPUTES SOIL MOISTURE NUDGING COEFFICIENTS                          SMASS.4
C*******************************************************************             SMASS.5
#     include <parame.incl>                                                      SMASS.6
#     include <comicl.incl>                                                      SMASS.7
#     include <soilp.incl>                                                       SMASS.8
C                                                                                SMASS.9
C.........Arguments                                                              SMASS.10
      REAL ALPH1(MIX), ALPH2(MIX), BET1(MIX), BET2(MIX)                          SMASS.11
      INTEGER J,ILX                                                              SMASS.12
C........Local variables                                                         SMASS.13
      REAL A1MAX,A2MAX,B1MAX,B2MAX                                               SMASS.14
      REAL TASSI,RAMIN                                                           SMASS.15
      REAL FBET,FALPH,FRA,FTEXT                                                  SMASS.16
      INTEGER ISTI                                                               SMASS.17
C........Indexes                                                                 SMASS.18
      INTEGER I                                                                  SMASS.19
C                                                                                SMASS.20
      DATA A1MAX/-10.E-5/,A2MAX/1.E-5/ ! m/K, m for 6hr period                   SMASS.21
      DATA B1MAX/-10.E-3/,B2MAX/1.E-3/ ! m/K, m (Bouttier et al 1993)            SMASS.22
      DATA TASSI/4.6296E-5/            ! 1/6hr in 1/sec                          SMASS.23
      DATA RAMIN/10./                  ! 0.1 s/cm                                SMASS.24
C                                                                                SMASS.25
      DO 100 I=1,ILX                                                             SMASS.26
        ISTI = ISLTYP(I,J)                                                       SMASS.27
        FBET = FASS(I)                                                           SMASS.28
        FALPH=RG(I)/1370.                                                        SMASS.29
C--TEXTURE FACTOR NORMALIZED BY LOAM (IST=5)                                     SMASS.30
        FRA = RAMIN/RA(I,J) ! scale by aerodynamic resistance                    SMASS.31
        FTEXT=TASSI*(WWLT(ISTI)+WFC(ISTI))/(WWLT(5)+WFC(5))*FRA                  SMASS.32
C        write(6,*) ' ftot, fbet=',ftot, fbet,' ftext=',ftext/tassi              SMASS.33
C                                                                                SMASS.34
        ALPH1(I) = A1MAX*FALPH*(1.0-VEGFRC(I,J))*FTEXT                           SMASS.35
        ALPH2(I) = A2MAX*FALPH*(1.0-VEGFRC(I,J))*FTEXT                           SMASS.36
        BET1 (I) = B1MAX*FBET *     VEGFRC(I,J) *FTEXT                           SMASS.37
        BET2 (I) = B2MAX*FBET *     VEGFRC(I,J) *FTEXT                           SMASS.38
100   CONTINUE                                                                   SMASS.39
      RETURN                                                                     SMASS.40
      END                                                                        SMASS.41
