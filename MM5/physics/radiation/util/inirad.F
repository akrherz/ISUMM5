C                                                                                INIRAD.1
       SUBROUTINE INIRAD (J, O3PROF)                                             INIRAD.2
C                                                                                INIRAD.3
#     include <parame.incl>                                                      INIRAD.4
#     include <parccm2.incl>                                                     INIRAD.5
#     include <various.incl>                                                     INIRAD.6
C#     INCLUDE <PARAME>                                                          INIRAD.7
C#     INCLUDE <PARAM1.CB>                                                       INIRAD.8
C#     INCLUDE <PARAM3.CB>                                                       INIRAD.9
C#     INCLUDE <RAD.CB>                                                          INIRAD.10
      DIMENSION O3PROF(IPLOND,IPLEVP)                                            INIRAD.11
      JX=MJX                                                                     INIRAD.12
      IX=MIX                                                                     INIRAD.13
C                                                                                INIRAD.14
C  COMPUTE OZONE MIXING RATIO DISTRIBUTION                                       INIRAD.15
C                                                                                INIRAD.16
C     IF (KTAU .EQ.  0) THEN                                                     INIRAD.17
      DO 10 K=1,KXP1                                                             INIRAD.18
C      DO 10 J=1,JX-1                                                            INIRAD.19
#ifndef MPP1                                                                     INIRAD.20
        DO 10 I=1,IX-1                                                           INIRAD.21
#else                                                                            INIRAD.22
          DO 10 I=1,IL-1                                                         INIRAD.23
#endif                                                                           INIRAD.24
            O3PROF(I,K)=0.                                                       INIRAD.25
   10     CONTINUE                                                               INIRAD.26
                                                                                 INIRAD.27
                                                                                 INIRAD.28
C      DO 11 J=1,JX-1                                                            INIRAD.29
C      DO 11 I=1,IX-1                                                            INIRAD.30
C       O3PROF(I,KXP1)=0.                                                        INIRAD.31
C11     CONTINUE                                                                 INIRAD.32
      CALL O3DATA(J,O3PROF)                                                      INIRAD.33
C      PRINT *,'OZONE PROFILES'                                                  INIRAD.34
C      DO 34 K=1,KX                                                              INIRAD.35
C       WRITE(6,1212)O3PROF(10,10,K),O3PROF(20,20,K),O3PROF(30,30                INIRAD.36
C    1               ,K),O3PROF(40,40,K)                                         INIRAD.37
C34     CONTINUE                                                                 INIRAD.38
C1212   FORMAT (1X,7E12.4)                                                       INIRAD.39
C     ENDIF                                                                      INIRAD.40
C                                                                                INIRAD.41
C  KOUTR = INDEX FOR OUTPUT OF RADIATION VARIABLES                               INIRAD.42
C     KOUTR=0                                                                    INIRAD.43
C                                                                                INIRAD.44
      RETURN                                                                     INIRAD.45
      END                                                                        INIRAD.46
                                                                                 INIRAD.47
