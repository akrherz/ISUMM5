      SUBROUTINE SETVEGFR(ILX,JLX,XTIME,JULDAY,GMT)                              SETVEGFR.1
      IMPLICIT NONE                                                              SETVEGFR.2
#include <parame.incl>                                                           SETVEGFR.3
#include <soilp.incl>                                                            SETVEGFR.4
C     INPUT                                                                      SETVEGFR.5
      INTEGER ILX, JLX, JULDAY                                                   SETVEGFR.6
      REAL XTIME, GMT                                                            SETVEGFR.7
C     LOCAL                                                                      SETVEGFR.8
      REAL REALMON, REALJUL, FRCMON                                              SETVEGFR.9
      INTEGER I, J, M1, M2                                                       SETVEGFR.10
C                                                                                SETVEGFR.11
C     REALJUL=0. AT 00Z JANUARY 1ST                                              SETVEGFR.12
C     REALMON=0. AND 12. ON DECEMBER 15TH                                        SETVEGFR.13
C                                                                                SETVEGFR.14
      REALJUL=FLOAT(JULDAY-1)+GMT/24.+XTIME/1440.                                SETVEGFR.15
      REALMON=REALJUL*12./365.+0.5                                               SETVEGFR.16
      FRCMON=REALMON-IFIX(REALMON)                                               SETVEGFR.17
      M1=IFIX(REALMON)                                                           SETVEGFR.18
      M1=MOD(M1,12)                                                              SETVEGFR.19
      M2=M1+1                                                                    SETVEGFR.20
      IF(M1.EQ.0)M1=12                                                           SETVEGFR.21
C                                                                                SETVEGFR.22
      DO J=1,JLX                                                                 SETVEGFR.23
         DO I=1,ILX                                                              SETVEGFR.24
C       LINEAR INTERPOLATION IN TIME FOR VEGFRC                                  SETVEGFR.25
           VEGFRC(I,J)=(1.-FRCMON)*VEGFMO(I,J,M1)+FRCMON*VEGFMO(I,J,M2)          SETVEGFR.26
           IF(VEGFRC(I,J).LT.1.E-3)VEGFRC(I,J)=0.                                SETVEGFR.27
           ALBEDO(I,J)=(1.-FRCMON)*ALBMON(I,J,M1)+FRCMON*ALBMON(I,J,M2)          19DEC02.1877
           IF(ALBEDO(I,J).LT.1.E-3)ALBEDO(I,J)=0.                                19DEC02.1878
         ENDDO                                                                   SETVEGFR.28
      ENDDO                                                                      SETVEGFR.29
                                                                                 SETVEGFR.30
      RETURN                                                                     SETVEGFR.31
      END                                                                        SETVEGFR.32
