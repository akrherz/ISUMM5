       SUBROUTINE SHALLCU(T3D,QV3D,T3DTEN,QV3DTEN,PP3D,PSB,HT,SCR1,SCR2,         SHALLCU.1
     &                    J,ISTART,IEND)                                         SHALLCU.2
                                                                                 SHALLCU.3
#     include <parame.incl>                                                      SHALLCU.4
#     include <param2.incl>                                                      SHALLCU.5
#     include <param3.incl>                                                      SHALLCU.6
#     include <pmoist.incl>                                                      SHALLCU.7
#     include <various.incl>                                                     SHALLCU.8
C                                                                                SHALLCU.9
C  ---------------------------------------------------------------------         SHALLCU.10
      PARAMETER (KDIM=MKX,KNUMS=1)                                               SHALLCU.11
      DIMENSION    T3D(MIX,MJX,MKX),    QV3D(MIX,MJX,MKX),                       SHALLCU.12
     &          T3DTEN(MIX,MJX,MKX), QV3DTEN(MIX,MJX,MKX),                       SHALLCU.13
     &            PP3D(MIX,MJX,MKX)                                              SHALLCU.14
      DIMENSION   SCR1(MIX,MJX,MKX),  SCR2(MIX,MJX,MKX),                         07NOV00.1473
     &             PSB(MIX,MJX),          HT(MIX,MJX)                            SHALLCU.16
C                                                                                SHALLCU.17
C--- DIMENSIONS NEEDED FOR LITTLE DRIVER ROUTINE                                 SHALLCU.18
C                                                                                SHALLCU.19
      DIMENSION T(KDIM),Q(KDIM),P(KDIM),OUTTS(KDIM),                             SHALLCU.20
     *          TNS(KDIM),QNS(KDIM),OUTQS(KDIM)                                  SHALLCU.21
      DIMENSION XMB(KNUMS)                                                       SHALLCU.22
      REAL MBDT                                                                  SHALLCU.23
                                                                                 SHALLCU.24
      ISHAL=0                                                                    SHALLCU.25
      RADS=50.                                                                   SHALLCU.26
      KBMAX=((MKX*3)/4-3)                                                        SHALLCU.27
      PCUT=400.                                                                  SHALLCU.28
      DTIME=DT                                                                   SHALLCU.29
      C0=0.000                                                                   SHALLCU.30
      KLNH=MKXNH                                                                 SHALLCU.31
      DO 50 I=ISTART,IEND                                                        SHALLCU.32
C                                                                                SHALLCU.33
C--- PREPARE INPUT, ERASE OUTPUT                                                 SHALLCU.34
C                                                                                SHALLCU.35
        KDET=0                                                                   SHALLCU.36
        DO 100 K=1,KDIM                                                          SHALLCU.37
          KK=KDIM-K+1                                                            SHALLCU.38
          T(K)=T3D(I,J,KK)                                                       SHALLCU.39
          Q(K)=QV3D(I,J,KK)                                                      SHALLCU.40
          IF(Q(K).LT.1.E-08)Q(K)=1.E-08                                          SHALLCU.41
          TNS(K)=T(K)+(SCR1(I,J,KK))/PSB(I,J)*DT                                 07NOV00.1474
          QNS(K)=Q(K)+(SCR2(I,J,KK))/PSB(I,J)*DT                                 07NOV00.1475
          P(K)=10.*PSB(I,J)*A(KK)+10.*PTOP+PP3D(I,J,KK)/100.                     SHALLCU.44
          PSUR=10.*PSB(I,J)+10.*PTOP+PP3D(I,J,KLNH)/100.                         SHALLCU.45
          OUTTS(K)=0.                                                            SHALLCU.46
          OUTQS(K)=0.                                                            SHALLCU.47
  100   CONTINUE                                                                 SHALLCU.48
        PRET=0.                                                                  SHALLCU.49
        TER11=HT(I,J)/G                                                          SHALLCU.50
        IF(TER11.LE.0.)TER11=1.E-05                                              SHALLCU.51
C                                                                                SHALLCU.52
C--- CALL CUMULUS PARAMETERIZATION                                               SHALLCU.53
C                                                                                SHALLCU.54
        IERR=0                                                                   SHALLCU.55
        CALL SHALLOW(T,Q,TER11,TNS,QNS,P,KDIM,PRET,P,OUTTS,OUTQS,DT,             SHALLCU.56
     +       KBMAX,PCUT,C0,PSUR,IERR,RADS,XMB)                                   SHALLCU.57
        IF(XMB(1).GT.0.)THEN                                                     SHALLCU.58
          ISHAL=ISHAL+1                                                          SHALLCU.59
        ELSE                                                                     SHALLCU.60
          GOTO 50                                                                SHALLCU.61
        ENDIF                                                                    SHALLCU.62
 1001   CONTINUE                                                                 SHALLCU.63
        DO 300 K=1,KDIM                                                          SHALLCU.64
          KK=KDIM-K+1                                                            SHALLCU.65
          T3DTEN(I,J,KK)=PSB(I,J)*OUTTS(K)+T3DTEN(I,J,KK)                        SHALLCU.66
          QV3DTEN(I,J,KK)=PSB(I,J)*OUTQS(K)+QV3DTEN(I,J,KK)                      SHALLCU.67
  300   CONTINUE                                                                 SHALLCU.68
C                                                                                SHALLCU.69
   50 CONTINUE                                                                   SHALLCU.70
      ICONS(J)=ISHAL                                                             SHALLCU.71
      RETURN                                                                     SHALLCU.72
      END                                                                        SHALLCU.73
                                                                                 SHALLCU.74
