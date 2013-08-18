      SUBROUTINE NSTLEV3(IEXEC,ICOARS)                                           NSTLEV3.1
# include <defines.incl>                                                         NSTLEV3.2
#     include <parame.incl>                                                      NSTLEV3.3
#     include <param2.incl>                                                      NSTLEV3.4
#     include <param3.incl>                                                      NSTLEV3.5
#     include <varia.incl>                                                       NSTLEV3.6
#     include <addr0.incl>                                                       NSTLEV3.7
#     include <various.incl>                                                     NSTLEV3.8
#     include <variousn.incl>                                                    NSTLEV3.9
         DIMENSION IEXEC(MAXNES)                                                 NSTLEV3.10
C                                                                                NSTLEV3.11
C--- CHECK WHETHER FOR THIS LEVEL AND THIS COARSE DOMAIN ANY NEST                NSTLEV3.12
C--- IS ACTIVE                                                                   NSTLEV3.13
C                                                                                NSTLEV3.14
      IDLEV=LEVIDN(ICOARS)+1                                                     NSTLEV3.15
      IF(NUMLV(IDLEV,ICOARS).EQ.0)RETURN                                         NSTLEV3.16
C                                                                                NSTLEV3.17
C--- IRAX=3, DO THIS LOOP THREE TIMES                                            NSTLEV3.18
C                                                                                NSTLEV3.19
      DO 200 NN=1,IRAX                                                           NSTLEV3.20
C                                                                                NSTLEV3.21
C--- FOR EVERY NEST                                                              NSTLEV3.22
C                                                                                NSTLEV3.23
        DO 100 L=LEVIDN(ICOARS)+2,MAXNES                                         NSTLEV3.24
          IF(IACTIV(L).EQ.0)GOTO 100                                             NSTLEV3.25
          IF(LEVIDN(L).NE.LEVIDN(ICOARS)+1)GOTO 100                              NSTLEV3.26
          IF(NUMNC(L).NE.ICOARS)GOTO 100                                         NSTLEV3.27
C                                                                                NSTLEV3.28
C                                                                                NSTLEV3.29
C                                                                                NSTLEV3.30
          IF(NN.EQ.1)THEN                                                        NSTLEV3.31
            CALL ADDRX1C(IAXALL(1,ICOARS))                                       NSTLEV3.32
            CALL ADDRX1N(IAXALL(1,L))                                            NSTLEV3.33
            CALL STOTNDT(L,INL,JNL,IL,JL)                                        NSTLEV3.34
          ENDIF                                                                  NSTLEV3.35
C                                                                                NSTLEV3.36
C--- CHECK FOR NEXT NESTLEVEL                                                    NSTLEV3.37
C                                                                                NSTLEV3.38
          CALL ADDRX1C(IAXALL(1,L))                                              NSTLEV3.39
          IF(NUMLV(IDLEV+1,L).GT.0)THEN                                          NSTLEV3.40
            DO 98 LL=LEVIDN(L)+2,MAXNES                                          NSTLEV3.41
              IF(IACTIV(LL).EQ.0)GOTO 98                                         NSTLEV3.42
              IF(LEVIDN(LL).NE.LEVIDN(L)+1)GOTO 98                               NSTLEV3.43
              IF(NUMNC(LL).NE.L)GOTO 98                                          NSTLEV3.44
              CALL ADDRX1N(IAXALL(1,LL))                                         NSTLEV3.45
              CALL STOTNDI(LL,INL,JNL,IL,JL)                                     NSTLEV3.46
   98       CONTINUE                                                             NSTLEV3.47
          ENDIF                                                                  NSTLEV3.48
C                                                                                NSTLEV3.49
#ifdef MPP1                                                                      NSTLEV3.50
          CALL MP_INITDOMAIN(L)                                                  NSTLEV3.51
#endif                                                                           NSTLEV3.52
          CALL SOLVE(IEXEC(L),L,NN)                                              NSTLEV3.53
C                                                                                NSTLEV3.54
          IF(NUMLV(IDLEV+1,L).GT.0)CALL NSTLEV4(IEXEC,L)                         23DEC04.58
  100   CONTINUE                                                                 NSTLEV3.56
C                                                                                NSTLEV3.57
C--- CHECK FOR NEST OVERLAYS                                                     NSTLEV3.58
C                                                                                NSTLEV3.59
#ifndef MPP1                                                                     NSTLEV3.60
        IF(MAXNES.GT.2)THEN                                                      NSTLEV3.61
          DO 150 L=2,MAXNES                                                      NSTLEV3.62
            DO 150 LL=2,MAXNES                                                   NSTLEV3.63
              IF(L.EQ.LL)GOTO 150                                                NSTLEV3.64
              IF(LEVIDN(L).NE.LEVIDN(LL))GOTO 150                                NSTLEV3.65
              IF(NUMNC(L).NE.NUMNC(LL))GOTO 150                                  NSTLEV3.66
              CALL ADDRX1N(IAXALL(1,L))                                          NSTLEV3.67
              CALL ADDRX1C(IAXALL(1,LL))                                         NSTLEV3.68
              CALL BDYOVL1(L)                                                    NSTLEV3.69
  150       CONTINUE                                                             NSTLEV3.70
                                                                                 NSTLEV3.71
        ENDIF                                                                    NSTLEV3.72
#endif                                                                           NSTLEV3.73
C                                                                                NSTLEV3.74
C                                                                                NSTLEV3.75
C                                                                                NSTLEV3.76
  200 CONTINUE                                                                   NSTLEV3.77
C                                                                                NSTLEV3.78
C--- FEEDBACK FOR COARSER DOMAIN(S)                                              NSTLEV3.79
C                                                                                NSTLEV3.80
      CALL ADDRX1C(IAXALL(1,ICOARS))                                             NSTLEV3.81
      IF(IFEED.EQ.0)RETURN                                                       NSTLEV3.82
      DO 300 L=LEVIDN(ICOARS)+2,MAXNES                                           NSTLEV3.83
        IF(IACTIV(L).EQ.0)GOTO 300                                               NSTLEV3.84
        IF(LEVIDN(L).NE.LEVIDN(ICOARS)+1)GOTO 300                                NSTLEV3.85
        IF(NUMNC(L).NE.ICOARS)GOTO 300                                           NSTLEV3.86
        CALL ADDRX1N(IAXALL(1,L))                                                NSTLEV3.87
        CALL FEEDBK(L,IL,JL)                                                     NSTLEV3.88
  300 CONTINUE                                                                   NSTLEV3.89
      RETURN                                                                     NSTLEV3.90
      END                                                                        NSTLEV3.91
                                                                                 NSTLEV3.92
