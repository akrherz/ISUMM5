      SUBROUTINE OVLCHK(L)                                                       OVLCHK.1
#     include <parame.incl>                                                      OVLCHK.2
#     include <nestl.incl>                                                       OVLCHK.3
#     include <param2.incl>                                                      OVLCHK.4
C                                                                                OVLCHK.5
      ISOUTH1=NESTI(L)                                                           OVLCHK.6
      JWEST1=NESTJ(L)                                                            OVLCHK.7
      NABI=(NESTIX(L)-1)/3+1                                                     OVLCHK.8
      NABJ=(NESTJX(L)-1)/3+1                                                     OVLCHK.9
      ISOUTH0=ISOUTH1+NABI                                                       OVLCHK.10
      JWEST0=JWEST1+NABJ                                                         OVLCHK.11
      DO 100 LL=2,MAXNES                                                         OVLCHK.12
        IF(LL.EQ.L)GOTO 100                                                      OVLCHK.13
        IF(LEVIDN(LL).NE.LEVIDN(L))GOTO 100                                      OVLCHK.14
        IF(NUMNC(LL).NE.NUMNC(L))GOTO 100                                        OVLCHK.15
C     CALL ADDRX1N(IAXALL(1,LL))                                                 OVLCHK.16
C     ISOUTH=NESTI(LL)                                                           OVLCHK.17
C     JWEST=NESTJ(LL)                                                            OVLCHK.18
        IF(NESTI(LL).GE.ISOUTH0.OR.NESTJ(LL).GE.JWEST0)THEN                      OVLCHK.19
          ISTAR(L)=-999                                                          OVLCHK.20
          JSTAR(L)=-999                                                          OVLCHK.21
          GOTO 100                                                               OVLCHK.22
        ENDIF                                                                    OVLCHK.23
        NABI=(NESTIX(LL)-1)/3+1                                                  OVLCHK.24
        NABJ=(NESTJX(LL)-1)/3+1                                                  OVLCHK.25
        ISOUTH0=NESTI(LL)+NABI                                                   OVLCHK.26
        JWEST0=NESTJ(LL)+NABJ                                                    OVLCHK.27
        IF(ISOUTH0.LE.ISOUTH1.OR.JWEST0.LE.JWEST1)THEN                           OVLCHK.28
          ISTAR(L)=-999                                                          OVLCHK.29
          JSTAR(L)=-999                                                          OVLCHK.30
          GOTO 100                                                               OVLCHK.31
        ENDIF                                                                    OVLCHK.32
        INLL=NESTIX(LL)                                                          OVLCHK.33
        JNLL=NESTJX(LL)                                                          OVLCHK.34
        I1=IRAX*(ISOUTH1-NESTI(LL))                                              OVLCHK.35
        J1=IRAX*(JWEST1-NESTJ(LL))                                               OVLCHK.36
        PRINT *,'I1,J1 = ',I1,J1                                                 OVLCHK.37
        IF(I1.GT.0)THEN                                                          OVLCHK.38
          ISTAR(L)=1                                                             OVLCHK.39
          IEN(L)=INLL-I1                                                         OVLCHK.40
          ISTO(L)=I1+1                                                           OVLCHK.41
        ELSEIF(I1.LT.0)THEN                                                      OVLCHK.42
          ISTAR(L)=1-I1                                                          OVLCHK.43
          IEN(L)=INLL                                                            OVLCHK.44
          ISTO(L)=1                                                              OVLCHK.45
        ENDIF                                                                    OVLCHK.46
        IF(J1.GT.0)THEN                                                          OVLCHK.47
          JSTAR(L)=1                                                             OVLCHK.48
          JEN(L)=JNLL-J1                                                         OVLCHK.49
          JSTO(L)=J1+1                                                           OVLCHK.50
        ELSEIF(J1.LT.0)THEN                                                      OVLCHK.51
          JSTAR(L)=1-J1                                                          OVLCHK.52
          JEN(L)=JNLL                                                            OVLCHK.53
          JSTO(L)=1                                                              OVLCHK.54
        ENDIF                                                                    OVLCHK.55
        IF(I1.EQ.0.AND.J1.EQ.0)THEN                                              OVLCHK.56
          ISTAR(L)=-999                                                          OVLCHK.57
          JSTAR(L)=-999                                                          OVLCHK.58
          GOTO 100                                                               OVLCHK.59
        ENDIF                                                                    OVLCHK.60
        PRINT *,'OVERLAPPING DOMAINS ',L,' AND ',LL                              OVLCHK.61
        PRINT1006,ISTAR(L),IEN(L)                                                OVLCHK.62
        PRINT1007,JSTAR(L),JEN(L)                                                OVLCHK.63
  100 CONTINUE                                                                   OVLCHK.64
 1006 FORMAT(' THIS NEST GRIDPOINTS ',I3,' TO ',I3,' ARE SWAPPED IN',            OVLCHK.65
     1       ' NORTH DIRECTION')                                                 OVLCHK.66
 1007 FORMAT(' THIS NEST GRIDPOINTS ',I3,' TO ',I3,' ARE SWAPPED IN',            OVLCHK.67
     1       ' EAST DIRECTION')                                                  OVLCHK.68
C     CALL ADDRX1N(IAXALL(1,L))                                                  OVLCHK.69
      RETURN                                                                     OVLCHK.70
      END                                                                        OVLCHK.71
                                                                                 OVLCHK.72
