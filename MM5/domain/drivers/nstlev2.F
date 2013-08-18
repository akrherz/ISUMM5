      SUBROUTINE NSTLEV2(IEXEC,ICOARS)                                           NSTLEV2.1
# include <defines.incl>                                                         NSTLEV2.2
#     include <parame.incl>                                                      NSTLEV2.3
#     include <param2.incl>                                                      NSTLEV2.4
#     include <param3.incl>                                                      NSTLEV2.5
#     include <varia.incl>                                                       NSTLEV2.6
#     include <addr0.incl>                                                       NSTLEV2.7
#     include <various.incl>                                                     NSTLEV2.8
#     include <variousn.incl>                                                    NSTLEV2.9
         DIMENSION IEXEC(MAXNES)                                                 NSTLEV2.10
C                                                                                NSTLEV2.11
C--- CHECK WHETHER FOR THIS LEVEL AND THIS COARSE DOMAIN ANY NEST                NSTLEV2.12
C--- IS ACTIVE                                                                   NSTLEV2.13
C                                                                                NSTLEV2.14
      IDLEV=LEVIDN(ICOARS)+1                                                     NSTLEV2.15
      IF(NUMLV(IDLEV,ICOARS).EQ.0)RETURN                                         NSTLEV2.16
C                                                                                NSTLEV2.17
C--- IRAX=3, DO THIS LOOP THREE TIMES                                            NSTLEV2.18
C                                                                                NSTLEV2.19
      DO 200 NN=1,IRAX                                                           NSTLEV2.20
C                                                                                NSTLEV2.21
C--- FOR EVERY NEST                                                              NSTLEV2.22
C                                                                                NSTLEV2.23
        DO 100 L=LEVIDN(ICOARS)+2,MAXNES                                         NSTLEV2.24
          IF(IACTIV(L).EQ.0)GOTO 100                                             NSTLEV2.25
          IF(LEVIDN(L).NE.LEVIDN(ICOARS)+1)GOTO 100                              NSTLEV2.26
          IF(NUMNC(L).NE.ICOARS)GOTO 100                                         NSTLEV2.27
C                                                                                NSTLEV2.28
C                                                                                NSTLEV2.29
C                                                                                NSTLEV2.30
          IF(NN.EQ.1)THEN                                                        NSTLEV2.31
            CALL ADDRX1C(IAXALL(1,ICOARS))                                       NSTLEV2.32
            CALL ADDRX1N(IAXALL(1,L))                                            NSTLEV2.33
            CALL STOTNDT(L,INL,JNL,IL,JL)                                        NSTLEV2.34
          ENDIF                                                                  NSTLEV2.35
C                                                                                NSTLEV2.36
C--- CHECK FOR NEXT NESTLEVEL                                                    NSTLEV2.37
C                                                                                NSTLEV2.38
          CALL ADDRX1C(IAXALL(1,L))                                              NSTLEV2.39
          IF(NUMLV(IDLEV+1,L).GT.0)THEN                                          NSTLEV2.40
            DO 98 LL=LEVIDN(L)+2,MAXNES                                          NSTLEV2.41
              IF(IACTIV(LL).EQ.0)GOTO 98                                         NSTLEV2.42
              IF(LEVIDN(LL).NE.LEVIDN(L)+1)GOTO 98                               NSTLEV2.43
              IF(NUMNC(LL).NE.L)GOTO 98                                          NSTLEV2.44
              CALL ADDRX1N(IAXALL(1,LL))                                         NSTLEV2.45
              CALL STOTNDI(LL,INL,JNL,IL,JL)                                     NSTLEV2.46
   98       CONTINUE                                                             NSTLEV2.47
          ENDIF                                                                  NSTLEV2.48
C                                                                                NSTLEV2.49
#ifdef MPP1                                                                      NSTLEV2.50
          CALL MP_INITDOMAIN(L)                                                  NSTLEV2.51
#endif                                                                           NSTLEV2.52
          CALL SOLVE(IEXEC(L),L,NN)                                              NSTLEV2.53
C                                                                                NSTLEV2.54
          IF(NUMLV(IDLEV+1,L).GT.0)CALL NSTLEV3(IEXEC,L)                         25JAN00.21
  100   CONTINUE                                                                 NSTLEV2.56
C                                                                                NSTLEV2.57
C--- CHECK FOR NEST OVERLAYS                                                     NSTLEV2.58
C                                                                                NSTLEV2.59
#ifndef MPP1                                                                     NSTLEV2.60
        IF(MAXNES.GT.2)THEN                                                      NSTLEV2.61
          DO 150 L=2,MAXNES                                                      NSTLEV2.62
            DO 150 LL=2,MAXNES                                                   NSTLEV2.63
              IF(L.EQ.LL)GOTO 150                                                NSTLEV2.64
              IF(LEVIDN(L).NE.LEVIDN(LL))GOTO 150                                NSTLEV2.65
              IF(NUMNC(L).NE.NUMNC(LL))GOTO 150                                  NSTLEV2.66
              CALL ADDRX1N(IAXALL(1,L))                                          NSTLEV2.67
              CALL ADDRX1C(IAXALL(1,LL))                                         NSTLEV2.68
              CALL BDYOVL1(L)                                                    NSTLEV2.69
  150       CONTINUE                                                             NSTLEV2.70
                                                                                 NSTLEV2.71
        ENDIF                                                                    NSTLEV2.72
#endif                                                                           NSTLEV2.73
C                                                                                NSTLEV2.74
C                                                                                NSTLEV2.75
C                                                                                NSTLEV2.76
  200 CONTINUE                                                                   NSTLEV2.77
C                                                                                NSTLEV2.78
C--- FEEDBACK FOR COARSER DOMAIN(S)                                              NSTLEV2.79
C                                                                                NSTLEV2.80
      CALL ADDRX1C(IAXALL(1,ICOARS))                                             NSTLEV2.81
      IF(IFEED.EQ.0)RETURN                                                       NSTLEV2.82
      DO 300 L=LEVIDN(ICOARS)+2,MAXNES                                           NSTLEV2.83
        IF(IACTIV(L).EQ.0)GOTO 300                                               NSTLEV2.84
        IF(LEVIDN(L).NE.LEVIDN(ICOARS)+1)GOTO 300                                NSTLEV2.85
        IF(NUMNC(L).NE.ICOARS)GOTO 300                                           NSTLEV2.86
        CALL ADDRX1N(IAXALL(1,L))                                                NSTLEV2.87
        CALL FEEDBK(L,IL,JL)                                                     NSTLEV2.88
  300 CONTINUE                                                                   NSTLEV2.89
      RETURN                                                                     NSTLEV2.90
      END                                                                        NSTLEV2.91
                                                                                 NSTLEV2.92
