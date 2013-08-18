      SUBROUTINE NSTLEV5(IEXEC,ICOARS)                                           NSTLEV5.1
# include <defines.incl>                                                         NSTLEV5.2
#     include <parame.incl>                                                      NSTLEV5.3
#     include <param2.incl>                                                      NSTLEV5.4
#     include <param3.incl>                                                      NSTLEV5.5
#     include <varia.incl>                                                       NSTLEV5.6
#     include <addr0.incl>                                                       NSTLEV5.7
#     include <various.incl>                                                     NSTLEV5.8
#     include <variousn.incl>                                                    NSTLEV5.9
         DIMENSION IEXEC(MAXNES)                                                 NSTLEV5.10
C                                                                                NSTLEV5.11
C--- CHECK WHETHER FOR THIS LEVEL AND THIS COARSE DOMAIN ANY NEST                NSTLEV5.12
C--- IS ACTIVE                                                                   NSTLEV5.13
C                                                                                NSTLEV5.14
      IDLEV=LEVIDN(ICOARS)+1                                                     NSTLEV5.15
      IF(NUMLV(IDLEV,ICOARS).EQ.0)RETURN                                         NSTLEV5.16
C                                                                                NSTLEV5.17
C--- IRAX=3, DO THIS LOOP THREE TIMES                                            NSTLEV5.18
C                                                                                NSTLEV5.19
      DO 200 NN=1,IRAX                                                           NSTLEV5.20
C                                                                                NSTLEV5.21
C--- FOR EVERY NEST                                                              NSTLEV5.22
C                                                                                NSTLEV5.23
        DO 100 L=LEVIDN(ICOARS)+2,MAXNES                                         NSTLEV5.24
          IF(IACTIV(L).EQ.0)GOTO 100                                             NSTLEV5.25
          IF(LEVIDN(L).NE.LEVIDN(ICOARS)+1)GOTO 100                              NSTLEV5.26
          IF(NUMNC(L).NE.ICOARS)GOTO 100                                         NSTLEV5.27
C                                                                                NSTLEV5.28
C                                                                                NSTLEV5.29
C                                                                                NSTLEV5.30
          IF(NN.EQ.1)THEN                                                        NSTLEV5.31
            CALL ADDRX1C(IAXALL(1,ICOARS))                                       NSTLEV5.32
            CALL ADDRX1N(IAXALL(1,L))                                            NSTLEV5.33
            CALL STOTNDT(L,INL,JNL,IL,JL)                                        NSTLEV5.34
          ENDIF                                                                  NSTLEV5.35
C                                                                                NSTLEV5.36
C--- CHECK FOR NEXT NESTLEVEL                                                    NSTLEV5.37
C                                                                                NSTLEV5.38
          CALL ADDRX1C(IAXALL(1,L))                                              NSTLEV5.39
          IF(NUMLV(IDLEV+1,L).GT.0)THEN                                          NSTLEV5.40
            DO 98 LL=LEVIDN(L)+2,MAXNES                                          NSTLEV5.41
              IF(IACTIV(LL).EQ.0)GOTO 98                                         NSTLEV5.42
              IF(LEVIDN(LL).NE.LEVIDN(L)+1)GOTO 98                               NSTLEV5.43
              IF(NUMNC(LL).NE.L)GOTO 98                                          NSTLEV5.44
              CALL ADDRX1N(IAXALL(1,LL))                                         NSTLEV5.45
              CALL STOTNDI(LL,INL,JNL,IL,JL)                                     NSTLEV5.46
   98       CONTINUE                                                             NSTLEV5.47
          ENDIF                                                                  NSTLEV5.48
C                                                                                NSTLEV5.49
#ifdef MPP1                                                                      NSTLEV5.50
          CALL MP_INITDOMAIN(L)                                                  NSTLEV5.51
#endif                                                                           NSTLEV5.52
          CALL SOLVE(IEXEC(L),L,NN)                                              NSTLEV5.53
C                                                                                NSTLEV5.54
C          IF(NUMLV(IDLEV+1,L).GT.0)CALL NSTLEV6(IEXEC,L)                        NSTLEV5.55
  100   CONTINUE                                                                 NSTLEV5.56
C                                                                                NSTLEV5.57
C--- CHECK FOR NEST OVERLAYS                                                     NSTLEV5.58
C                                                                                NSTLEV5.59
#ifndef MPP1                                                                     NSTLEV5.60
        IF(MAXNES.GT.2)THEN                                                      NSTLEV5.61
          DO 150 L=2,MAXNES                                                      NSTLEV5.62
            DO 150 LL=2,MAXNES                                                   NSTLEV5.63
              IF(L.EQ.LL)GOTO 150                                                NSTLEV5.64
              IF(LEVIDN(L).NE.LEVIDN(LL))GOTO 150                                NSTLEV5.65
              IF(NUMNC(L).NE.NUMNC(LL))GOTO 150                                  NSTLEV5.66
              CALL ADDRX1N(IAXALL(1,L))                                          NSTLEV5.67
              CALL ADDRX1C(IAXALL(1,LL))                                         NSTLEV5.68
              CALL BDYOVL1(L)                                                    NSTLEV5.69
  150       CONTINUE                                                             NSTLEV5.70
                                                                                 NSTLEV5.71
        ENDIF                                                                    NSTLEV5.72
#endif                                                                           NSTLEV5.73
C                                                                                NSTLEV5.74
C                                                                                NSTLEV5.75
C                                                                                NSTLEV5.76
  200 CONTINUE                                                                   NSTLEV5.77
C                                                                                NSTLEV5.78
C--- FEEDBACK FOR COARSER DOMAIN(S)                                              NSTLEV5.79
C                                                                                NSTLEV5.80
      CALL ADDRX1C(IAXALL(1,ICOARS))                                             NSTLEV5.81
      IF(IFEED.EQ.0)RETURN                                                       NSTLEV5.82
      DO 300 L=LEVIDN(ICOARS)+2,MAXNES                                           NSTLEV5.83
        IF(IACTIV(L).EQ.0)GOTO 300                                               NSTLEV5.84
        IF(LEVIDN(L).NE.LEVIDN(ICOARS)+1)GOTO 300                                NSTLEV5.85
        IF(NUMNC(L).NE.ICOARS)GOTO 300                                           NSTLEV5.86
        CALL ADDRX1N(IAXALL(1,L))                                                NSTLEV5.87
        CALL FEEDBK(L,IL,JL)                                                     NSTLEV5.88
  300 CONTINUE                                                                   NSTLEV5.89
      RETURN                                                                     NSTLEV5.90
      END                                                                        NSTLEV5.91
                                                                                 NSTLEV5.92
