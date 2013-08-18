       SUBROUTINE KFDRIVE(PSB,SCR9,J,INEST,IBEG,IEND,                            KFDRIVE.1
     & TBASE,U3D,V3D,PP3D,T3D,QV3D)                                              KFDRIVE.2
#     include <parame.incl>                                                      KFDRIVE.3
#     include <various.incl>                                                     KFDRIVE.4
#     include <param2.incl>                                                      KFDRIVE.5
#     include <param3.incl>                                                      KFDRIVE.6
#     include <pmoist.incl>                                                      KFDRIVE.7
#     include <varia.incl>                                                       KFDRIVE.8
#     include <addrcu.incl>                                                      KFDRIVE.9
#     include <rpstar.incl>                                                      KFDRIVE.10
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc                KFDRIVE.11
c                                                               c                KFDRIVE.12
c       subroutine to select grid points which should be        c                KFDRIVE.13
c       checked for possible initiation of KF scheme...         c                KFDRIVE.14
c                                                               c                KFDRIVE.15
c                                       Jack Kain               c                KFDRIVE.16
c                                       June 1997               c                KFDRIVE.17
c                                                               c                KFDRIVE.18
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc                KFDRIVE.19
C                                                                                KFDRIVE.20
      DIMENSION ICUYES(MIX),SEM(KX),                                             KFDRIVE.21
     *          SEMS(KX),PRS(KX),TV0(KX),Z00(KX),                                KFDRIVE.22
     *          SED(KX)                                                          KFDRIVE.23
      DIMENSION DP(KX),LSB(MIX)                                                  KFDRIVE.24
      LOGICAL IPRNT                                                              KFDRIVE.25
c     PARAMETER(XLV=2.5E6)                                                       KFDRIVE.26
      COMMON/VAPPRS/ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE,XLS0,XLS1            KFDRIVE.27
C                                                                                KFDRIVE.28
C...INPUT VARIABLES                                                              KFDRIVE.29
C                                                                                KFDRIVE.30
      REAL      U3D(MIX,MJX,MKX),  V3D(MIX,MJX,MKX),                             KFDRIVE.31
     &          T3D(MIX,MJX,MKX), QV3D(MIX,MJX,MKX),                             KFDRIVE.32
     &         PP3D(MIX,MJX,MKX), TBASE(MIXNH,MJXNH,MKXNH)                       KFDRIVE.33
                                                                                 KFDRIVE.34
      DIMENSION PSB(MIX,MJX),SCR9(MIX,MKX)                                       KFDRIVE.35
      DIMENSION T(3,MIX,MKX),QV(3,MIX,MKX)                                       KFDRIVE.36
      DIMENSION PPBS(MIX,MKX+1)                                                  KFDRIVE.37
c                                                                                KFDRIVE.38
c...make a quick for check for these things that can eliminate                   KFDRIVE.39
c...grid points from the possibility of convective initiation:                   KFDRIVE.40
c                                                                                KFDRIVE.41
c        1.) convection already active at this point                             KFDRIVE.42
c        2.) point close to grid-domain boundary                                 KFDRIVE.43
c        3.) downward motion at all levels in lowest 300 mb                      KFDRIVE.44
c        4.) no CAPE                                                             KFDRIVE.45
c                                                                                KFDRIVE.46
       NSHALL = 0                                                                KFDRIVE.47
cc$doacross                                                                      KFDRIVE.48
cc$& share(dtdt,dqdt,w0avg,nca,raincv,hbm2,lmh,pd,                               KFDRIVE.49
cc$&       res,aeta,pt,omgalf,t,q,rovg),                                         KFDRIVE.50
cc$& local(i,j,l,lg,ncuyes,icuyes,psfck,p300,prs,ttmp,qtmp,                      KFDRIVE.51
cc$&       tv0,z0,es,qs,sem,sems,semmx,semmin,lmax,lmin,semdif)                  KFDRIVE.52
                                                                                 KFDRIVE.53
       NCLDCK = NINT(1200./DT2)                                                  KFDRIVE.54
c     DO 70 J = 3,JM-2                                                           KFDRIVE.55
        NCUYES=0                                                                 KFDRIVE.56
c       DO 10 I = 1,IM                                                           KFDRIVE.57
c      write(6, *)'inside KFDRIVE; J =',J                                        KFDRIVE.58
c      call flush(6)                                                             KFDRIVE.59
c      IF(J.EQ.35 .and. inest.eq.2)                                              KFDRIVE.60
c    *  WRITE(98,*)'INSIDE KFDRIVE; NCLDCK =',NCLDCK                             KFDRIVE.61
        DO 10 I = IBEG,IEND                                                      KFDRIVE.62
          ICUYES(I)=0                                                            KFDRIVE.63
          LSB(I) = 0                                                             KFDRIVE.64
 10     CONTINUE                                                                 KFDRIVE.65
      DO 60 I = IBEG,IEND                                                        KFDRIVE.66
c        IF (NCA(I,J).GT.0)GOTO 60                                               KFDRIVE.67
        IF (NCA(I,J).GT.NCLDCK)GOTO 60                                           KFDRIVE.68
c       IF (HBM2(I,J) .eq. 0.) GOTO 60                                           KFDRIVE.69
c       LG = LMH(I,J)                                                            KFDRIVE.70
        LG = KL                                                                  KFDRIVE.71
          P200=1000.*(PSB(I,J)*A(KL)+PTOP-20.)+PP3D(I,J,KL)                      KFDRIVE.72
C                                                                                KFDRIVE.73
C...PRESSURE PERTURBATION TERM IS ONLY DEFINED AT MID-POINT OF                   KFDRIVE.74
C...VERTICAL LAYERS...SINCE TOTAL PRESSURE IS NEEDED AT THE TOP AND              KFDRIVE.75
C...BOTTOM OF LAYERS BELOW, DO AN INTERPOLATION...                               KFDRIVE.76
C                                                                                KFDRIVE.77
          DO 5 K=2,KX                                                            KFDRIVE.78
            PPBS(I,K)=(PP3D(I,J,K-1)*DSIGMA(K)+PP3D(I,J,K)*DSIGMA(K-1))/         KFDRIVE.79
     +                (DSIGMA(K-1)+DSIGMA(K))                                    KFDRIVE.80
    5     CONTINUE                                                               KFDRIVE.81
          PPBS(I,1)=PP3D(I,J,1)+((PP3D(I,J,1)-PP3D(I,J,2))/(A(1)-A(2))*(         KFDRIVE.82
     +              SIGMA(1)-A(1)))                                              KFDRIVE.83
          PPBS(I,KX+1)=PP3D(I,J,KX)+((PP3D(I,J,KX-1)-PP3D(I,J,KX))/              KFDRIVE.84
     &                (A(KX-1)-A(KX))*(SIGMA(KX+1)-A(KX)))                       KFDRIVE.85
c       PSFCK = PD(I,J)*RES(I,J)*AETA(LG)+PT                                     KFDRIVE.86
c        P300 = PSFCK-3.E4                                                       KFDRIVE.87
c       P200 = PSFCK-2.E4                                                        KFDRIVE.88
c       P400 = PSFCK-4.E4                                                        KFDRIVE.89
        DO 15 K = LG,1,-1                                                        KFDRIVE.90
c         PRS(L) = PD(I,J)*RES(I,J)*AETA(L)+PT                                   KFDRIVE.91
          PRS(K)=1.E3*(A(K)*PSB(I,J)+PTOP)+PP3D(I,J,K)                           KFDRIVE.92
 15     continue                                                                 KFDRIVE.93
        DO 16 L = LG,1,-1                                                        KFDRIVE.94
          IF(PRS(L).GT.P200)L200 = L                                             KFDRIVE.95
 16     CONTINUE                                                                 KFDRIVE.96
c                                                                                KFDRIVE.97
c...vertical velocity must be upward at some level in the                        KFDRIVE.98
c...lowest 200 mb...                                                             KFDRIVE.99
        ISINK = 0                                                                KFDRIVE.100
        DO 20 L = LG,L200,-1                                                     KFDRIVE.101
c         IF(OMGALF(I,J,L).LT.0. .or. Q2(I,J,L).GT.1.)GOTO 25                    KFDRIVE.102
          IF(SCR9(I,L).LT.0.)GOTO 25                                             KFDRIVE.103
 20     CONTINUE                                                                 KFDRIVE.104
        ISINK = 1                                                                KFDRIVE.105
 25     CONTINUE                                                                 KFDRIVE.106
c                                                                                KFDRIVE.107
c...calculate moist static energy and saturation moist static energy             KFDRIVE.108
c...at each vertical level...                                                    KFDRIVE.109
c                                                                                KFDRIVE.110
        DO 30 L = LG,1,-1                                                        KFDRIVE.111
          TTMP = T3D(I,J,L)                                                      KFDRIVE.112
          QTMP = QV3D(I,J,L)                                                     KFDRIVE.113
          NK = L                                                                 KFDRIVE.114
          TV0(L) = TTMP*(1.+.608*QTMP)                                           KFDRIVE.115
          IF(L.EQ.LG)THEN                                                        KFDRIVE.116
            Z00(L) = 0.                                                          KFDRIVE.117
          ELSE                                                                   KFDRIVE.118
            Z00(L) = Z00(L+1)-ROVG*0.5*(TV0(L)+TV0(L+1))*                        KFDRIVE.119
     *            ALOG(PRS(L)/PRS(L+1))                                          KFDRIVE.120
          ENDIF                                                                  KFDRIVE.121
          ES=ALIQ*EXP((BLIQ*TTMP-CLIQ)/(TTMP-DLIQ))                              KFDRIVE.122
          QSS=0.622*ES/(PRS(L)-ES)                                               KFDRIVE.123
          SED(L)=CP*TTMP+G*Z00(L)                                                KFDRIVE.124
          SEMS(L)=SED(L)+XLV*QSS                                                 KFDRIVE.125
          SEM(L)=SED(L)+XLV*QTMP                                                 KFDRIVE.126
 30     CONTINUE                                                                 KFDRIVE.127
c                                                                                KFDRIVE.128
c...If air is sinking everywhere in lowest 400 mb, require superadiabatic        KFDRIVE.129
c...layer in lowest 200 mb before checking for convection in KF scheme...        KFDRIVE.130
c                                                                                KFDRIVE.131
       IF(ISINK.EQ.1)THEN                                                        KFDRIVE.132
         DO 36 L = LG,L200-1,-1                                                  KFDRIVE.133
           DO 35 NL = L-1,L200,-1                                                KFDRIVE.134
             IF(SED(L).GT.SED(NL))THEN                                           KFDRIVE.135
               NCUYES=NCUYES+1                                                   KFDRIVE.136
#ifndef MPP1                                                                     KFDRIVE.137
               ICUYES(NCUYES)=I                                                  KFDRIVE.138
#else                                                                            KFDRIVE.139
               ICUYES(NOFLIC(NCUYES))=I                                          KFDRIVE.140
#endif                                                                           KFDRIVE.141
               LSB(I) = LG-L+1                                                   KFDRIVE.142
               GOTO 60                                                           KFDRIVE.143
             ENDIF                                                               KFDRIVE.144
 35        CONTINUE                                                              KFDRIVE.145
 36      CONTINUE                                                                KFDRIVE.146
       ELSE                                                                      KFDRIVE.147
c                                                                                KFDRIVE.148
c...if there is upward motion, require conditional instability for a             KFDRIVE.149
c...parcel originating in the lowest 400 mb...                                   KFDRIVE.150
c                                                                                KFDRIVE.151
c       DO 41 L = LG,L400,-1                                                     KFDRIVE.152
        DO 41 L = LG,L200,-1                                                     KFDRIVE.153
          DO 40 NL = L-1,1,-1                                                    KFDRIVE.154
           IF(SEM(L).GT.SEMS(NL))THEN                                            KFDRIVE.155
             NCUYES=NCUYES+1                                                     KFDRIVE.156
#ifndef MPP1                                                                     KFDRIVE.157
             ICUYES(NCUYES)=I                                                    KFDRIVE.158
#else                                                                            KFDRIVE.159
             ICUYES(NOFLIC(NCUYES))=I                                            KFDRIVE.160
#endif                                                                           KFDRIVE.161
             LSB(I) = LG-L+1                                                     KFDRIVE.162
             GOTO 60                                                             KFDRIVE.163
          ENDIF                                                                  KFDRIVE.164
 40       CONTINUE                                                               KFDRIVE.165
 41     CONTINUE                                                                 KFDRIVE.166
       ENDIF                                                                     KFDRIVE.167
c...                                                                             KFDRIVE.168
 60     CONTINUE                                                                 KFDRIVE.169
        IF(NCUYES.GT.0)                                                          KFDRIVE.170
     -       CALL KFPARA2(NCUYES,ICUYES,J,LSB,NSHALL,INEST,TBASE,                KFDRIVE.171
     -                    U3D,V3D,PP3D,T3D,QV3D,PSB)                             KFDRIVE.172
c70    CONTINUE                                                                  KFDRIVE.173
c     IF(J.EQ.35 .and. INEST.EQ.2)                                               KFDRIVE.174
c    *WRITE(98,*)'at KTAU =',KTAU,' NSHALL =',NSHALL                             KFDRIVE.175
      RETURN                                                                     KFDRIVE.176
      END                                                                        KFDRIVE.177
