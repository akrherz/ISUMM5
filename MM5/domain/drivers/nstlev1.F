      SUBROUTINE NSTLEV1(IEXEC,ICOARS)                                           NSTLEV1.1
# include <defines.incl>                                                         NSTLEV1.2
#     include <parame.incl>                                                      NSTLEV1.3
#     include <param2.incl>                                                      NSTLEV1.4
#     include <param3.incl>                                                      NSTLEV1.5
#     include <addr0.incl>                                                       NSTLEV1.6
#     include <pointbc.incl>                                                     NSTLEV1.7
#     include <nonhydb.incl>                                                     NSTLEV1.8
#     include <various.incl>                                                     NSTLEV1.9
#     include <variousn.incl>                                                    NSTLEV1.10
         DIMENSION IEXEC(MAXNES)                                                 NSTLEV1.11
      IDLEV=LEVIDN(ICOARS)+1                                                     25JAN00.18
      IF(NSTTOT.EQ.1)RETURN                                                      NSTLEV1.12
C                                                                                NSTLEV1.13
C--- IRAX=3, DO THIS LOOP THREE TIMES                                            NSTLEV1.14
C                                                                                NSTLEV1.15
      DO 200 NN=1,IRAX                                                           NSTLEV1.16
C                                                                                NSTLEV1.17
C--- FOR EVERY NEST AT THIS LEVEL                                                NSTLEV1.18
C                                                                                NSTLEV1.19
        DO 100 L=2,MAXNES                                                        NSTLEV1.20
C                                                                                NSTLEV1.21
          IF(IACTIV(L).EQ.0)GOTO 100                                             NSTLEV1.22
          IF(LEVIDN(L).NE.1)GOTO 100                                             NSTLEV1.23
          IF(NUMNC(L).NE.ICOARS)GOTO 100                                         NSTLEV1.24
C      PRINT *,'NEST COUNT = ',L                                                 NSTLEV1.25
C                                                                                NSTLEV1.26
C                                                                                NSTLEV1.27
          IF(NN.EQ.1)THEN                                                        NSTLEV1.28
            CALL ADDRX1C(IAXALL(1,ICOARS))                                       NSTLEV1.29
            CALL ADDRX1N(IAXALL(1,L))                                            NSTLEV1.30
            CALL STOTNDT(L,INL,JNL,IL,JL)                                        NSTLEV1.31
          ENDIF                                                                  NSTLEV1.32
          CALL ADDRX1C(IAXALL(1,L))                                              NSTLEV1.33
C                                                                                NSTLEV1.34
C--- CALCULATE BOUNDARY FIELDS BEFORE GOING INTO SOLVE. DO THIS AT               NSTLEV1.35
C--- EVERY TIMESTEP. THIS WILL BE FOR ICOARS+1 LEVEL, THE MOTHER                 NSTLEV1.36
C--- DOMAIN WILL BE DOMAIN L, KEEP COUNT OF DOMAIN NUMBERS                       NSTLEV1.37
C                                                                                NSTLEV1.38
          IF(NUMLV(IDLEV+1,L).GT.0)THEN                                          25JAN00.19
            DO 98 LL=LEVIDN(L)+2,MAXNES                                          NSTLEV1.40
              IF(IACTIV(LL).EQ.0)GOTO 98                                         NSTLEV1.41
              IF(LEVIDN(LL).NE.LEVIDN(L)+1)GOTO 98                               NSTLEV1.42
              IF(NUMNC(LL).NE.L)GOTO 98                                          NSTLEV1.43
              CALL ADDRX1N(IAXALL(1,LL))                                         NSTLEV1.44
              CALL STOTNDI(LL,INL,JNL,IL,JL)                                     NSTLEV1.45
   98       CONTINUE                                                             NSTLEV1.46
          ENDIF                                                                  NSTLEV1.47
C                                                                                NSTLEV1.48
C--- CALL SOLVER OF CHOICE                                                       NSTLEV1.49
C                                                                                NSTLEV1.50
#ifdef MPP1                                                                      NSTLEV1.51
          CALL ADDRX1C(IAXALL(1,L))                                              NSTLEV1.52
          CALL MP_INITDOMAIN(L)                                                  NSTLEV1.53
#endif                                                                           NSTLEV1.54
          CALL SOLVE(IEXEC(L),L,NN)                                              NSTLEV1.55
c      SUBROUTINE OUTTAP(IUTL,NESCOU,IX,JX,KZZ,IFWRTHDR)                         NSTLEV1.56
C                                                                                NSTLEV1.57
C--- CHECK FURTHER NESTLEVELS BEFORE NEXT TIMESTEP                               NSTLEV1.58
C                                                                                NSTLEV1.59
          IF(NUMLV(IDLEV+1,L).GT.0)CALL NSTLEV2(IEXEC,L)                         25JAN00.20
  100   CONTINUE                                                                 NSTLEV1.61
C                                                                                NSTLEV1.62
C--- CHECK FOR NEST-OVERLAYS. IF THEY EXIST, BOUNDARIES                          NSTLEV1.63
C--- HAVE TO BE OVERWRITTEN! L IS OVERWRITTEN WITH LL!                           NSTLEV1.64
C                                                                                NSTLEV1.65
#ifndef MPP1                                                                     NSTLEV1.66
        IF(MAXNES.GT.2)THEN                                                      NSTLEV1.67
          DO 150 L=2,MAXNES                                                      NSTLEV1.68
            DO 150 LL=2,MAXNES                                                   NSTLEV1.69
              IF(L.EQ.LL)GOTO 150                                                NSTLEV1.70
              IF(LEVIDN(L).NE.LEVIDN(LL))GOTO 150                                NSTLEV1.71
              IF(NUMNC(L).NE.NUMNC(LL))GOTO 150                                  NSTLEV1.72
              CALL ADDRX1N(IAXALL(1,L))                                          NSTLEV1.73
              CALL ADDRX1C(IAXALL(1,LL))                                         NSTLEV1.74
              CALL BDYOVL1(L)                                                    NSTLEV1.75
  150       CONTINUE                                                             NSTLEV1.76
                                                                                 NSTLEV1.77
        ENDIF                                                                    NSTLEV1.78
#endif                                                                           NSTLEV1.79
C                                                                                NSTLEV1.80
C                                                                                NSTLEV1.81
C                                                                                NSTLEV1.82
C                                                                                NSTLEV1.83
C                                                                                NSTLEV1.84
  200 CONTINUE                                                                   NSTLEV1.85
C                                                                                NSTLEV1.86
C--- FEEDBACK FOR COARSER DOMAIN                                                 NSTLEV1.87
C                                                                                NSTLEV1.88
      CALL ADDRX1C(IAXALL(1,1))                                                  NSTLEV1.89
      IF(IFEED.EQ.0)RETURN                                                       NSTLEV1.90
      DO 300 L=2,MAXNES                                                          NSTLEV1.91
        IF(IACTIV(L).EQ.0)GOTO 300                                               NSTLEV1.92
        IF(LEVIDN(L).NE.LEVIDN(ICOARS)+1)GOTO 300                                NSTLEV1.93
        IF(NUMNC(L).NE.ICOARS)GOTO 300                                           NSTLEV1.94
        CALL ADDRX1N(IAXALL(1,L))                                                NSTLEV1.95
        CALL FEEDBK(L,IL,JL)                                                     NSTLEV1.96
  300 CONTINUE                                                                   NSTLEV1.97
      RETURN                                                                     NSTLEV1.98
      END                                                                        NSTLEV1.99
                                                                                 NSTLEV1.100
