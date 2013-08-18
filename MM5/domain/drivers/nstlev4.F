      SUBROUTINE NSTLEV4(IEXEC,ICOARS)                                           NSTLEV4.1
# include <defines.incl>                                                         NSTLEV4.2
#     include <parame.incl>                                                      NSTLEV4.3
#     include <param2.incl>                                                      NSTLEV4.4
#     include <param3.incl>                                                      NSTLEV4.5
#     include <varia.incl>                                                       NSTLEV4.6
#     include <addr0.incl>                                                       NSTLEV4.7
#     include <various.incl>                                                     NSTLEV4.8
#     include <variousn.incl>                                                    NSTLEV4.9
         DIMENSION IEXEC(MAXNES)                                                 NSTLEV4.10
C                                                                                NSTLEV4.11
C--- CHECK WHETHER FOR THIS LEVEL AND THIS COARSE DOMAIN ANY NEST                NSTLEV4.12
C--- IS ACTIVE                                                                   NSTLEV4.13
C                                                                                NSTLEV4.14
      IDLEV=LEVIDN(ICOARS)+1                                                     NSTLEV4.15
      IF(NUMLV(IDLEV,ICOARS).EQ.0)RETURN                                         NSTLEV4.16
C                                                                                NSTLEV4.17
C--- IRAX=3, DO THIS LOOP THREE TIMES                                            NSTLEV4.18
C                                                                                NSTLEV4.19
      DO 200 NN=1,IRAX                                                           NSTLEV4.20
C                                                                                NSTLEV4.21
C--- FOR EVERY NEST                                                              NSTLEV4.22
C                                                                                NSTLEV4.23
        DO 100 L=LEVIDN(ICOARS)+2,MAXNES                                         NSTLEV4.24
          IF(IACTIV(L).EQ.0)GOTO 100                                             NSTLEV4.25
          IF(LEVIDN(L).NE.LEVIDN(ICOARS)+1)GOTO 100                              NSTLEV4.26
          IF(NUMNC(L).NE.ICOARS)GOTO 100                                         NSTLEV4.27
C                                                                                NSTLEV4.28
C                                                                                NSTLEV4.29
C                                                                                NSTLEV4.30
          IF(NN.EQ.1)THEN                                                        NSTLEV4.31
            CALL ADDRX1C(IAXALL(1,ICOARS))                                       NSTLEV4.32
            CALL ADDRX1N(IAXALL(1,L))                                            NSTLEV4.33
            CALL STOTNDT(L,INL,JNL,IL,JL)                                        NSTLEV4.34
          ENDIF                                                                  NSTLEV4.35
C                                                                                NSTLEV4.36
C--- CHECK FOR NEXT NESTLEVEL                                                    NSTLEV4.37
C                                                                                NSTLEV4.38
          CALL ADDRX1C(IAXALL(1,L))                                              NSTLEV4.39
          IF(NUMLV(IDLEV+1,L).GT.0)THEN                                          NSTLEV4.40
            DO 98 LL=LEVIDN(L)+2,MAXNES                                          NSTLEV4.41
              IF(IACTIV(LL).EQ.0)GOTO 98                                         NSTLEV4.42
              IF(LEVIDN(LL).NE.LEVIDN(L)+1)GOTO 98                               NSTLEV4.43
              IF(NUMNC(LL).NE.L)GOTO 98                                          NSTLEV4.44
              CALL ADDRX1N(IAXALL(1,LL))                                         NSTLEV4.45
              CALL STOTNDI(LL,INL,JNL,IL,JL)                                     NSTLEV4.46
   98       CONTINUE                                                             NSTLEV4.47
          ENDIF                                                                  NSTLEV4.48
C                                                                                NSTLEV4.49
#ifdef MPP1                                                                      NSTLEV4.50
          CALL MP_INITDOMAIN(L)                                                  NSTLEV4.51
#endif                                                                           NSTLEV4.52
          CALL SOLVE(IEXEC(L),L,NN)                                              NSTLEV4.53
C                                                                                NSTLEV4.54
          IF(NUMLV(IDLEV+1,L).GT.0)CALL NSTLEV5(IEXEC,L)                         NSTLEV4.55
  100   CONTINUE                                                                 NSTLEV4.56
C                                                                                NSTLEV4.57
C--- CHECK FOR NEST OVERLAYS                                                     NSTLEV4.58
C                                                                                NSTLEV4.59
#ifndef MPP1                                                                     NSTLEV4.60
        IF(MAXNES.GT.2)THEN                                                      NSTLEV4.61
          DO 150 L=2,MAXNES                                                      NSTLEV4.62
            DO 150 LL=2,MAXNES                                                   NSTLEV4.63
              IF(L.EQ.LL)GOTO 150                                                NSTLEV4.64
              IF(LEVIDN(L).NE.LEVIDN(LL))GOTO 150                                NSTLEV4.65
              IF(NUMNC(L).NE.NUMNC(LL))GOTO 150                                  NSTLEV4.66
              CALL ADDRX1N(IAXALL(1,L))                                          NSTLEV4.67
              CALL ADDRX1C(IAXALL(1,LL))                                         NSTLEV4.68
              CALL BDYOVL1(L)                                                    NSTLEV4.69
  150       CONTINUE                                                             NSTLEV4.70
                                                                                 NSTLEV4.71
        ENDIF                                                                    NSTLEV4.72
#endif                                                                           NSTLEV4.73
C                                                                                NSTLEV4.74
C                                                                                NSTLEV4.75
C                                                                                NSTLEV4.76
  200 CONTINUE                                                                   NSTLEV4.77
C                                                                                NSTLEV4.78
C--- FEEDBACK FOR COARSER DOMAIN(S)                                              NSTLEV4.79
C                                                                                NSTLEV4.80
      CALL ADDRX1C(IAXALL(1,ICOARS))                                             NSTLEV4.81
      IF(IFEED.EQ.0)RETURN                                                       NSTLEV4.82
      DO 300 L=LEVIDN(ICOARS)+2,MAXNES                                           NSTLEV4.83
        IF(IACTIV(L).EQ.0)GOTO 300                                               NSTLEV4.84
        IF(LEVIDN(L).NE.LEVIDN(ICOARS)+1)GOTO 300                                NSTLEV4.85
        IF(NUMNC(L).NE.ICOARS)GOTO 300                                           NSTLEV4.86
        CALL ADDRX1N(IAXALL(1,L))                                                NSTLEV4.87
        CALL FEEDBK(L,IL,JL)                                                     NSTLEV4.88
  300 CONTINUE                                                                   NSTLEV4.89
      RETURN                                                                     NSTLEV4.90
      END                                                                        NSTLEV4.91
                                                                                 NSTLEV4.92
