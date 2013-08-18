        SUBROUTINE SUBCH(NUMNE,NUMN,NESTII,NESTJJ,ISOUTHO,JWESTO)                SUBCH.1
#     include <parame.incl>                                                      SUBCH.2
#     include <nestl.incl>                                                       SUBCH.3
#     include <variousn.incl>                                                    SUBCH.4
C                                                                                SUBCH.5
      JWEST=JWEST-(3*(NESTJJ-JWESTO)+1)                                          SUBCH.6
      ISOUTH=ISOUTH-(3*(NESTII-ISOUTHO)+1)                                       SUBCH.7
      JEAST=JWEST+(JNL-1)/IRAX                                                   SUBCH.8
      INORTH=ISOUTH+(INL-1)/IRAX                                                 SUBCH.9
      JEP1=JEAST+1                                                               SUBCH.10
      JEP2=JEAST+2                                                               SUBCH.11
      JEM1=JEAST-1                                                               SUBCH.12
      JEM2=JEAST-2                                                               SUBCH.13
      JWP1=JWEST+1                                                               SUBCH.14
      JWP2=JWEST+2                                                               SUBCH.15
      JWM1=JWEST-1                                                               SUBCH.16
      JWM2=JWEST-2                                                               SUBCH.17
      ISP1=ISOUTH+1                                                              SUBCH.18
      ISP2=ISOUTH+2                                                              SUBCH.19
      ISM1=ISOUTH-1                                                              SUBCH.20
      ISM2=ISOUTH-2                                                              SUBCH.21
      INP1=INORTH+1                                                              SUBCH.22
      INP2=INORTH+2                                                              SUBCH.23
      INM1=INORTH-1                                                              SUBCH.24
      INM2=INORTH-2                                                              SUBCH.25
      PRINT *,'NEW NESTI,J FOR NEST ',NUMN                                       SUBCH.26
      PRINT *,'AFTER MOVE OF NEST ',NUMNE                                        SUBCH.27
      PRINT *,' IS ',ISOUTH,JWEST                                                SUBCH.28
      RETURN                                                                     SUBCH.29
      END                                                                        SUBCH.30
                                                                                 SUBCH.31
