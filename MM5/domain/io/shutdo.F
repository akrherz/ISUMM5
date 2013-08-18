       SUBROUTINE SHUTDO                                                         SHUTDO.1
#     include <parame.incl>                                                      SHUTDO.2
#     include <addr0.incl>                                                       SHUTDO.3
#     include <param2.incl>                                                      SHUTDO.4
#     include <various.incl>                                                     SHUTDO.5
#     include <varia.incl>                                                       SHUTDO.6
      CALL ADDRX1C(IAXALL(1,1))                                                  SHUTDO.7
      PRINT *,'OUTPUTTING SHUTDOWN FILES AT KTAU = ',KTAU                        SHUTDO.8
      IXTIMR=NINT(XTIME)                                                         07NOV00.803
      PRINT *,' IXTIMR = ',IXTIMR                                                SHUTDO.10
      DO 66 LLN=1,MAXNES                                                         SHUTDO.11
        IF(IACTIV(LLN).EQ.0)GOTO 66                                              SHUTDO.12
         PRINT *,'OUTPUT SAVDATA FOR DOMAIN ',LLN                                SHUTDO.13
        IUTSAV=61+(LLN-1)                                                        SHUTDO.14
        REWIND IUTSAV                                                            SHUTDO.15
        CALL OUTSAV(IUTSAV,ALLARR(1,LLN),IRHUGE,INTALL(1,LLN),IIHUGE,            SHUTDO.16
     +       ALLFG(1,LLN),IFGHUG,INTFG(1,LLN),IFGIHUG,ALLFO(1,LLN),              SHUTDO.17
     +       IFOHUG,MAXNES,MKX,MIX,MJX)                                          SHUTDO.18
         PRINT *,'OUTPUT UNIT FOR THIS DOMAIN WAS UNIT ',IUTSAV                  SHUTDO.19
   66 CONTINUE                                                                   SHUTDO.20
      RETURN                                                                     SHUTDO.21
      END                                                                        SHUTDO.22
                                                                                 SHUTDO.23
