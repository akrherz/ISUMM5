C-----------------------------------------------------------------------         ZENITM.1
      SUBROUTINE ZENITM (JSLC, COSZRS)                                           ZENITM.2
                                                                                 ZENITM.3
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::              ZENITM.4
C                                                                                ZENITM.5
C   THIS SUBROUTINE CALCULATES THE COSINE OF THE SOLAR ZENITH ANGLE              ZENITM.6
C   FOR ALL LONGITUDE POINTS OF THE MM42D DOMAIN. IT NEEDS AS INPUTS             ZENITM.7
C   THE LONGITUDE AND LATITUDE OF THE POINTS, THE INITIAL DATE OF THE            ZENITM.8
C   SIMULATION AND THE GMT. ALL THESE QUANTITIES ARE SPECIFIED                   ZENITM.9
C   IN THE INITIALIZATION PROCEDURE OF MM42D                                     ZENITM.10
C                                                                                ZENITM.11
C     INCLUDE 'PARAME'                                                           ZENITM.12
C     INCLUDE 'PARAM1.CB'                                                        ZENITM.13
C     INCLUDE 'PARAM3.CB'                                                        ZENITM.14
C     INCLUDE 'MAINCB.BATS'                                                      ZENITM.15
#     include <parame.incl>                                                      ZENITM.16
#     include <param3.incl>                                                      ZENITM.17
#     include <parccm2.incl>                                                     ZENITM.18
#     include <point2d.incl>                                                     ZENITM.19
#     include <various.incl>                                                     ZENITM.20
#     include <param2.incl>                                                      ZENITM.21
C                                                                                ZENITM.22
C IN MAINCB.BATS (COMMON BLOCK) VA IS CHANGED TO VVA AND F INTO FFFF TO          ZENITM.23
C AVOID CONFUSION WITH VARIABLES IN BATS                                         ZENITM.24
C                                                                                ZENITM.25
      DIMENSION COSZRS(IPLOND)                                                   ZENITM.26
C                                                                                ZENITM.27
#ifndef MPP1                                                                     ZENITM.28
                                                                                 ZENITM.29
      IVMX=IPLOND                                                                ZENITM.30
      DEGRAD=0.0174533                                                           ZENITM.31
C     XT24 = AMOD(XTIME,1440.0)                                                  ZENITM.32
      XT24=AMOD(XTIME+RADFRQ*0.5,1440.0)                                         ZENITM.33
      DO 10 ILL=1,IVMX                                                           ZENITM.34
        ILLLIM=MIN0(ILL,ILX)                                                     ZENITM.35
        TLOCAP=GMT+XT24/60.+XLONG(ILLLIM,JSLC)/15.                               ZENITM.36
        TLOCAP=AMOD(TLOCAP+24.,24.)                                              ZENITM.37
        OMEGA=15.*(TLOCAP-12.)*DEGRAD                                            ZENITM.38
        XXLAT=XLAT(ILLLIM,JSLC)*DEGRAD                                           ZENITM.39
C      COSZRS = COSINE OF SOLAR ZENITH ANGLE                                     ZENITM.40
        COSZRS(ILL)=SIN(DECLIN)*SIN(XXLAT)+COS(DECLIN)*COS(XXLAT)*               ZENITM.41
     +              COS(OMEGA)                                                   ZENITM.42
        COSZRS(ILL)=AMAX1(0.,COSZRS(ILL))                                        ZENITM.43
   10 CONTINUE                                                                   ZENITM.44
                                                                                 ZENITM.45
#else                                                                            ZENITM.46
                                                                                 ZENITM.47
      DEGRAD=0.0174533                                                           ZENITM.48
C     XT24 = AMOD(XTIME,1440.0)                                                  ZENITM.49
      XT24=AMOD(XTIME+RADFRQ*0.5,1440.0)                                         ZENITM.50
      DO 10 ILL=1,ILX                                                            ZENITM.51
        TLOCAP=GMT+XT24/60.+XLONG(ILL,JSLC)/15.                                  ZENITM.52
        TLOCAP=AMOD(TLOCAP+24.,24.)                                              ZENITM.53
        OMEGA=15.*(TLOCAP-12.)*DEGRAD                                            ZENITM.54
        XXLAT=XLAT(ILL,JSLC)*DEGRAD                                              ZENITM.55
C      COSZRS = COSINE OF SOLAR ZENITH ANGLE                                     ZENITM.56
        COSZRS(ILL)=SIN(DECLIN)*SIN(XXLAT)+COS(DECLIN)*COS(XXLAT)*               ZENITM.57
     +              COS(OMEGA)                                                   ZENITM.58
        COSZRS(ILL)=AMAX1(0.,COSZRS(ILL))                                        ZENITM.59
   10 CONTINUE                                                                   ZENITM.60
                                                                                 ZENITM.61
#endif                                                                           ZENITM.62
      RETURN                                                                     ZENITM.63
      END                                                                        ZENITM.64
                                                                                 ZENITM.65
