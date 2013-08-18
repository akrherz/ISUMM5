#include <defines.incl>                                                          05DEC01.1063
      SUBROUTINE MYEPBL(J,IYY,JXX,KL,DT,KTAU,IN,ITGFLG,ISOIL,                    MYEPBL.1
     -     U3D,V3D,T3D,QV3D,PP3D,U3DTEN,V3DTEN,T3DTEN,QV3DTEN)                   MYEPBL.2
C     IMPLICIT NONE                                                              05DEC01.1064
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         MYEPBL.3
C                                                                      C         MYEPBL.4
C     BASED ON MELLOR-YAMADA LEVEL 2.5 SCHEME AS IMPLEMENTED IN        C         MYEPBL.5
C     NCEP ETA MODEL                                                   C         MYEPBL.6
C                                                                      C         MYEPBL.7
C     TKE IS A 3D PROGNOSTIC VARIABLE (NO ADVECTION OR HOR. DIFFUSION) C         MYEPBL.8
C     COMPUTES VERTICAL DIFFUSION OF T, QV, U, V ON CROSS POINTS       C         MYEPBL.9
C       WITH IMPLICIT SCHEME IN EACH COLUMN (CODE NOT VECTORIZED)      C         MYEPBL.10
C     STANDARD STABILITY FUNCTIONS FOR SURFACE SIMILARITY THEORY       C         MYEPBL.11
C     CALLS MIXLEN, PRODQ2, DIFCOF, VDIFQ, SFCDIF, VDIFH, VDIFV        C         MYEPBL.12
C     CALLS MM5 SFCRAD ROUTINE FOR RADIATIVE FLUXES                    C         MYEPBL.13
C     CALLS MM5 SLAB ROUTINE FOR GROUND TEMPERATURE PREDICTION         C         MYEPBL.14
C                                                                      C         MYEPBL.15
C     ROUTINES MIXLEN, PRODQ2, DIFCOF, VDIFQ, SFCDIF, VDIFH, VDIFV     C         MYEPBL.16
C       CODED BY JANJIC (NCEP)                                         C         MYEPBL.17
C     IMPLEMENTED IN MM5 BY DUDHIA (SUMMER 1997)                       C         MYEPBL.18
C                                                                      C         MYEPBL.19
C     REFERENCES:                                                      C         MYEPBL.20
C       JANJIC (1994), MON. WEA. REV.                                  C         MYEPBL.21
C                                                                                05DEC01.1065
C     APRIL 2001:                                                                05DEC01.1066
C       COUPLE WITH OSU LSM. NOAH LSM SINCE V3.6                                 19DEC02.1723
C                                                                                05DEC01.1068
C     SEPTEMBER 2001:                                                            05DEC01.1069
C       ESTIMATE PBL HEIGHT, LEVEL, REGIME FOR FDDA APPL.,                       05DEC01.1070
C       FOLLOWING GS PBL                                                         05DEC01.1071
C                                                                      C         MYEPBL.22
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         MYEPBL.23
C                                                                                MYEPBL.24
#     include <parame.incl>                                                      MYEPBL.25
#     include <point2d.incl>                                                     MYEPBL.26
#     include <navypb.incl>                                                      MYEPBL.27
#     include <rpstar.incl>                                                      MYEPBL.28
#     include <soil.incl>                                                        MYEPBL.29
#     include <param3.incl>                                                      MYEPBL.30
#     include <pmoist.incl>                                                      MYEPBL.31
#     include <nhcnst.incl>                                                      MYEPBL.32
#     include <varia.incl>                                                       05DEC01.1072
#     include <landuse.incl>                                                     19DEC02.1724
                                                                                 05DEC01.1073
      INTEGER LM                                                                 05DEC01.1074
      PARAMETER (LM=MKX)                                                         MYEPBL.33
C-----------------------------------------------------------------------         MYEPBL.34
                             P A R A M E T E R                                   MYEPBL.35
     &(LP1=LM+1,LM1=LM-1)                                                        MYEPBL.36
      PARAMETER (EPSZ=1.E-4,KTMQ2=1)                                             MYEPBL.37
      DIMENSION U3D(MIX,MJX,MKX),V3D(MIX,MJX,MKX),T3D(MIX,MJX,MKX),              MYEPBL.38
     -         QV3D(MIX,MJX,MKX),PP3D(MIX,MJX,MKX)                               MYEPBL.39
                                                                                 05DEC01.1075
      DIMENSION U3DTEN(MIX,MJX,MKX),V3DTEN(MIX,MJX,MKX),                         MYEPBL.40
     -          T3DTEN(MIX,MJX,MKX),QV3DTEN(MIX,MJX,MKX)                         MYEPBL.41
      DIMENSION TGDSB(MIX),TSLDSB(MIX,MLX)                                       MYEPBL.42
      DIMENSION UCOL(LM),VCOL(LM),TCOL(LM),QCOL(LM),Q2COL(LM),                   MYEPBL.43
     -     ZCOL(LP1),APECOL(LM),GM(LM1),GH(LM1),EL(LM1),                         MYEPBL.44
     -     AKMCOL(LM1),AKHCOL(LM1)                                               MYEPBL.45
      DIMENSION UCOLD(LM),VCOLD(LM),TCOLD(LM),QCOLD(LM),Q2COLD(LM)               MYEPBL.46
      DIMENSION ZCOLI(MIX,LP1),APECOLI(MIX,LM),AKHCOLI(MIX,LM1),                 05DEC01.1076
     -     AKMCOLI(MIX,LM1),Q2COLI(MIX,LM),THX(MIX,MKX),QX(MIX,MKX),             05DEC01.1077
     -     APES(MIX),                                                            07NOV00.1982
     -     FLHC(MIX),FLQC(MIX),QGH(MIX),UZ0HI(MIX),VZ0HI(MIX),THGB(MIX)          07NOV00.1983
      REAL ESG,QSSI,QSWT,THSI,THWT,HSFLXSI,HLFLXSI                               19DEC02.1725
C FOR LSM                                                                        05DEC01.1078
      REAL PREC(MIX), CPM(MIX), CHS(MIX), CHS2(MIX), CHFF(MIX)                   05DEC01.1079
      REAL U10I(MIX), V10I(MIX), TH2(MIX), Q2(MIX), RHOX(MIX)                    05DEC01.1080
      REAL T2D(MIX,MKX), QCX(MIXM,MKXM), UX(MIX,MKX), VX(MIX,MKX)                05DEC01.1081
      REAL PS(MIX)                                                               23DEC04.3647
      REAL ZA(MIX,MKX), ZQ(MIX,LP1)                                              05DEC01.1082
      REAL CKLQ(MIX,MJX)                                                         05DEC01.1083
C                                                                                MYEPBL.50
      IBNES=2                                                                    MYEPBL.51
      IENES=IYY-1                                                                MYEPBL.52
      JBNES=2                                                                    MYEPBL.53
      JENES=JXX-1                                                                MYEPBL.54
      IF(IN.GT.1)THEN                                                            MYEPBL.55
        IBNES=3                                                                  MYEPBL.56
        IENES=IYY-2                                                              MYEPBL.57
        JBNES=3                                                                  MYEPBL.58
        JENES=JXX-2                                                              MYEPBL.59
      ENDIF                                                                      MYEPBL.60
C                                                                                MYEPBL.61
      LMHK=KL                                                                    MYEPBL.62
      ILX=IYY-1                                                                  MYEPBL.63
      DTQ2=DT                                                                    MYEPBL.64
      DELTSM=DT                                                                  MYEPBL.65
C FIRST TIME STEP ONLY                                                           MYEPBL.66
      IF(KTAU.EQ.0)THEN                                                          MYEPBL.67
        DO I=1,ILX                                                               MYEPBL.68
          MOL(I,J)=0.01                                                          MYEPBL.69
          HOL(I,J)=0.01                                                          MYEPBL.70
          UST(I,J)=0.01                                                          MYEPBL.71
          DO K=1,KL                                                              MYEPBL.72
            TURKE(I,J,K)=0.2                                                     MYEPBL.73
          ENDDO                                                                  MYEPBL.74
        ENDDO                                                                    MYEPBL.75
      ENDIF                                                                      MYEPBL.76
C                                                                                MYEPBL.77
C-----BEGIN BIG I LOOP                                                           MYEPBL.78
      DO I=1,ILX                                                                 MYEPBL.79
C SET SURFACE VALUES                                                             MYEPBL.80
C                                                                                19DEC02.1726
C Initialize new variables to zero at start of i loop                            19DEC02.1727
C                                                                                19DEC02.1728
        THSI=0.                                                                  19DEC02.1729
        THWT=0.                                                                  19DEC02.1730
        QSSI=0.                                                                  19DEC02.1731
        QSWT=0.                                                                  19DEC02.1732
        ESG=0.                                                                   19DEC02.1733
        HSFLXSI=0.                                                               19DEC02.1734
        HLFLXSI=0.                                                               19DEC02.1735
        USTAR=UST(I,J)                                                           MYEPBL.81
        ISM=NINT(XLAND(I,J)-1.)                                                  MYEPBL.82
        IF(SEAICE(I,J).GT.0.02)THEN                                              19DEC02.1736
C Sea-ice grid point                                                             19DEC02.1737
          ISM=0                                                                  19DEC02.1738
        ENDIF                                                                    19DEC02.1739
        AKHS=HOL(I,J)                                                            MYEPBL.83
        AKMS=MOL(I,J)                                                            MYEPBL.84
        Z0=ZNT(I,J)                                                              MYEPBL.85
        WETM=MAVAIL(I,J)                                                         MYEPBL.86
        CKLQ(I,J)=1.                                                             05DEC01.1084
C SET COLUMN ARRAYS                                                              MYEPBL.87
        DO L=1,LMHK                                                              MYEPBL.88
          UCOL(L)=0.25*(U3D(I,J,L)+U3D(I+1,J,L)+U3D(I,J+1,L)+                    MYEPBL.89
     +            U3D(I+1,J+1,L))                                                MYEPBL.90
          VCOL(L)=0.25*(V3D(I,J,L)+V3D(I+1,J,L)+V3D(I,J+1,L)+                    MYEPBL.91
     +            V3D(I+1,J+1,L))                                                MYEPBL.92
          TCOL(L)=T3D(I,J,L)                                                     MYEPBL.93
          QCOL(L)=QV3D(I,J,L)                                                    MYEPBL.94
          Q2COL(L)=TURKE(I,J,L)                                                  MYEPBL.95
        ENDDO                                                                    MYEPBL.96
C LOWEST LEVEL WIND                                                              MYEPBL.97
        ULM=UCOL(LMHK)                                                           MYEPBL.98
        VLM=VCOL(LMHK)                                                           MYEPBL.99
C MORE SURFACE PROPERTIES                                                        MYEPBL.100
        PSURF=PSB(I,J)+PTOP+PP3D(I,J,KL)*0.001                                   MYEPBL.101
        PS(I) = PSURF                                                            23DEC04.3648
        RHOSFC=PSURF*1000./(R*TCOL(LMHK)*(1.+XMOIST(IN)*EP1*QCOL(LMHK)))         MYEPBL.102
        APES(I)=(100./PSURF)**ROVCP                                              07NOV00.1984
        TS=TGB(I,J)                                                              MYEPBL.104
        E1=SVP1*EXP(SVP2*(TS-SVPT0)/(TS-SVP3))                                   MYEPBL.105
        QS=EP2*E1/(PSURF-E1)                                                     MYEPBL.106
C                                                                                19DEC02.1740
C Adjust value of QS to account for ice surface (permanent or sea ice)           19DEC02.1741
C Calculate THSI and THWT for sea ice surface type                               19DEC02.1742
C                                                                                19DEC02.1743
        IF(NINT(SATBRT(I,J)).EQ.ISICE)THEN                                       19DEC02.1744
C Permanent ice surface type                                                     19DEC02.1745
          E1=0.611*EXP(22.514-6.15E3/TS)                                         19DEC02.1746
          QS=0.622*E1/(PSURF-E1)                                                 19DEC02.1747
        ELSEIF(SEAICE(I,J).GT.0.02)THEN                                          19DEC02.1748
C Sea ice surface type                                                           19DEC02.1749
          E1=SVP1*EXP(SVP2*(271.4-SVPT0)/(271.4-SVP3))                           19DEC02.1750
c         write(6,*) 'i = ',i,' j = ',j,' tgsi = ',tgsi(i,j),' in = ',in         19DEC02.1751
          ESG=0.611*EXP(22.514-6.15E3/TGSI(I,J))                                 19DEC02.1752
          QS=EP2*E1/(PSURF-E1)*(1.-SEAICE(I,J))+                                 19DEC02.1753
     +       0.622*ESG/(PSURF-ESG)*SEAICE(I,J)                                   19DEC02.1754
          QSSI=0.622*ESG/(PSURF-ESG)                                             19DEC02.1755
          QSWT=EP2*E1/(PSURF-E1)                                                 19DEC02.1756
C                                                                                19DEC02.1757
C Calculate potential temperature of sea ice surface                             19DEC02.1758
C                                                                                19DEC02.1759
          THSI=TGSI(I,J)*APES(I)                                                 19DEC02.1760
          THWT=271.4*APES(I)                                                     19DEC02.1761
        ENDIF                                                                    19DEC02.1762
        THGB(I)=TGB(I,J)*APES(I)                                                 07NOV00.1985
        THS=THGB(I)                                                              07NOV00.1986
        RHOX(I)=RHOSFC                                                           05DEC01.1085
C HEIGHTS                                                                        MYEPBL.110
        ZCOL(LMHK+1)=EPSZ                                                        MYEPBL.111
        ZQ(I,LMHK+1)=0.0                                                         05DEC01.1086
        DO K=LMHK,1,-1                                                           MYEPBL.112
          CELL=PTOP*RPSB(I,J)                                                    MYEPBL.113
          ZCOL(K)=ROVG*T0(I,J,K)*ALOG((SIGMA(K+1)+CELL)/(SIGMA(K)+CELL))         MYEPBL.114
     +            +ZCOL(K+1)                                                     MYEPBL.115
          PL=A(K)*PSB(I,J)+PTOP+PP3D(I,J,K)*0.001                                MYEPBL.116
          APECOL(K)=(100./PL)**ROVCP                                             MYEPBL.117
          ZA(I,K)=0.5*(ZCOL(K)+ZCOL(K+1))                                        05DEC01.1087
          ZQ(I,K)=ZCOL(K)                                                        05DEC01.1088
        ENDDO                                                                    MYEPBL.118
C CALCULATE VERTICAL MIXING AND UPDATE TKE (Q2)                                  MYEPBL.119
        CALL MIXLEN(LMHK,LPBL,HPBL,UCOL,VCOL,TCOL,QCOL,Q2COL,APECOL,ZCOL         MYEPBL.120
     +       ,GM,GH,EL)                                                          MYEPBL.121
        CALL PRODQ2(LMHK,DTQ2,USTAR,GM,GH,EL,Q2COL)                              MYEPBL.122
        CALL DIFCOF(LMHK,GM,GH,EL,Q2COL,ZCOL,AKMCOL,AKHCOL)                      MYEPBL.123
        CALL VDIFQ(LMHK,KTMQ2,DTQ2,Q2COL,EL,ZCOL)                                MYEPBL.124
C CALCULATE SURFACE FLUXES                                                       MYEPBL.125
        CALL SFCDIF(LMHK,ISM,THS,QS,UZ0H,VZ0H,THZ0,QZ0,USTAR,WSTAR,Z0,           MYEPBL.126
     +       WETM,AKMS,AKHS,HPBL,CT,U10,V10,TSHLTR,TH10,QSHLTR,Q10,ULM,          MYEPBL.127
     +       VLM,TCOL,QCOL,APECOL,ZCOL,HSFLX,HLFLX,AKHS02,                       19DEC02.1763
     +       HSFLXSI,HLFLXSI,                                                    19DEC02.1764
     +       SATBRT(I,J),SEAICE(I,J),THSI,QSSI,THWT,QSWT)                        19DEC02.1765
        HFX(I,J)=-RHOSFC*CP*HSFLX                                                MYEPBL.129
        QFX(I,J)=-RHOSFC*HLFLX                                                   MYEPBL.130
        HFXSI(I,J)=-RHOSFC*CP*HSFLXSI                                            19DEC02.1766
        QFXSI(I,J)=-RHOSFC*HLFLXSI                                               19DEC02.1767
        UST(I,J)=USTAR                                                           MYEPBL.131
        HOL(I,J)=AKHS                                                            MYEPBL.132
        MOL(I,J)=AKMS                                                            MYEPBL.133
        CHS(I)  =AKHS                                                            05DEC01.1092
C EXCHANGE COEFF AT 2 M (FOR LSM)                                                05DEC01.1093
        CHS2(I) = AKHS02                                                         05DEC01.1094
        ZNT(I,J)=Z0                                                              MYEPBL.134
        DO L=1,LMHK                                                              MYEPBL.135
          Q2COLI(I,L)=Q2COL(L)                                                   MYEPBL.136
          ZCOLI(I,L)=ZCOL(L)                                                     MYEPBL.137
          APECOLI(I,L)=APECOL(L)                                                 MYEPBL.138
          THX(I,L)=TCOL(L)*APECOL(L)                                             MYEPBL.141
          QX(I,L)=QCOL(L)                                                        MYEPBL.142
        ENDDO                                                                    MYEPBL.143
        DO L=1,LMHK-1                                                            05DEC01.1095
          AKHCOLI(I,L)=AKHCOL(L)                                                 05DEC01.1096
          AKMCOLI(I,L)=AKMCOL(L)                                                 05DEC01.1097
        ENDDO                                                                    05DEC01.1098
        ZCOLI(I,LMHK+1)=ZCOL(LMHK+1)                                             MYEPBL.144
        FLHC(I)=RHOSFC*CP*AKHS                                                   MYEPBL.145
        FLQC(I)=RHOSFC*AKHS*WETM                                                 MYEPBL.146
        UZ0HI(I)=UZ0H                                                            MYEPBL.147
        VZ0HI(I)=VZ0H                                                            MYEPBL.148
        QGH(I)=QS                                                                MYEPBL.149
C                                                                                05DEC01.1099
C  FILL UA10,VA10,T2 AND QA2 (QA2 WILL BE UPDATED BY LSM OVER LAND)              05DEC01.1100
C                                                                                05DEC01.1101
        UA10(I,J)=U10                                                            05DEC01.1102
        VA10(I,J)=V10                                                            05DEC01.1103
        TA2 (I,J)=TSHLTR/APES(I)                                                 05DEC01.1104
        QA2 (I,J)=QSHLTR                                                         05DEC01.1105
        TH2 (I)  =TSHLTR                                                         05DEC01.1106
        Q2  (I)  =QSHLTR                                                         05DEC01.1107
C                                                                                MYEPBL.150
C     CALL SURFCE(APE,ZINT,CKLQ)                                                 MYEPBL.151
C                                                                                MYEPBL.152
      ENDDO                                                                      MYEPBL.153
C-----END BIG I LOOP                                                             MYEPBL.154
C                                                                                05DEC01.1108
C BEGIN GROUND TEMPERATURE CALCULATION                                           MYEPBL.155
C                                                                                05DEC01.1109
      DO I=1,ILX                                                                 MYEPBL.156
        TGDSB(I)=TGB(I,J)                                                        MYEPBL.157
        IF(ISOIL.EQ.1)THEN                                                       MYEPBL.158
          DO L=1,MLX                                                             MYEPBL.159
            TSLDSB(I,L)=TSLB(I,J,L)                                              MYEPBL.160
          ENDDO                                                                  MYEPBL.161
        ENDIF                                                                    MYEPBL.162
      ENDDO                                                                      MYEPBL.163
C GROUND HEAT BUDGET                                                             MYEPBL.164
      CALL SFCRAD(IYY,JXX,J,IN,T3D,QV3D,PP3D)                                    07NOV00.1987
C                                                                                05DEC01.1110
      IF (ISOIL.LE.1) THEN                                                       05DEC01.1111
      CALL SLAB(DELTSM,J,IN,THX,QX,FLHC,FLQC,PP3D,T3D,1,ILX)                     07NOV00.1988
                                                                                 05DEC01.1112
      ELSE IF (ISOIL.EQ.2) THEN                                                  05DEC01.1113
#ifdef SOIL2                                                                     05DEC01.1114
        DO I=1,ILX                                                               05DEC01.1115
          DO L = 1,LMHK                                                          05DEC01.1116
             T2D(I,L) = T3D(I,J,L)                                               05DEC01.1117
C QCX: dummy, not used in LSM                                                    05DEC01.1118
             QCX(I,L) = 0.0                                                      05DEC01.1119
             UX(I,L) = 0.25*(U3D(I,J,L)+U3D(I+1,J,L)+U3D(I,J+1,L)+               05DEC01.1120
     +                       U3D(I+1,J+1,L))                                     05DEC01.1121
             VX(I,L) = 0.25*(V3D(I,J,L)+V3D(I+1,J,L)+V3D(I,J+1,L)+               05DEC01.1122
     +                       V3D(I+1,J+1,L))                                     05DEC01.1123
          END DO                                                                 05DEC01.1124
                                                                                 05DEC01.1125
          U10I(I) = UA10(I,J)                                                    05DEC01.1126
          V10I(I) = VA10(I,J)                                                    05DEC01.1127
                                                                                 05DEC01.1128
          CPM(I)=CP*(1.+0.8*QX(I,KL)*XMOIST(IN))                                 05DEC01.1129
          CHS(I)=HOL(I,J)                                                        05DEC01.1130
          CHFF(I)=CPM(I)*RHOX(I)*CHS(I)                                          05DEC01.1131
C        PREC (KG/M^2/S) FROM RAINP (CM) ACCUMULATION IN ONE TIMESTEP            05DEC01.1132
          PREC(I)=RAINP(I,J)*10./(DT/2.)                                         05DEC01.1133
        END DO                                                                   05DEC01.1134
                                                                                 05DEC01.1135
        CALL SURFCE(J,ZA,QX,THX,TGB,THGB,TGDSB,CHS,CHFF,CPM,PREC,                05DEC01.1136
     +       HFX,QFX,QGH,                                                        05DEC01.1137
     +       GSW,GLW,T2D,UX,VX,ALB,PP3D,PSB,RHOX,TMN,XLAND,QCX,IN,U10I,          05DEC01.1138
     +       V10I,TH2,Q2,RAINC,RAINNC,SNOWC,CHS2,CKLQ,ZNT,EMISS)                 23DEC04.3649
                                                                                 05DEC01.1140
        DO I=1,ILX                                                               05DEC01.1141
           QA2(I,J) = Q2(I)                                                      05DEC01.1142
           TA2(I,J) = TH2(I)*(PS(I)/100.)**ROVCP                                 23DEC04.3650
        END DO                                                                   05DEC01.1143
#endif                                                                           05DEC01.1144
      END IF                                                                     05DEC01.1145
C                                                                                05DEC01.1146
C-----BEGIN 2ND BIG I LOOP                                                       MYEPBL.167
C                                                                                05DEC01.1147
      DO I=1,ILX                                                                 MYEPBL.168
C SET SURFACE VALUES                                                             MYEPBL.169
        ISM=NINT(XLAND(I,J)-1.)                                                  MYEPBL.170
        THGB(I)=TGB(I,J)*APES(I)                                                 07NOV00.1989
        THS=THGB(I)                                                              07NOV00.1990
        QS=QGH(I)                                                                MYEPBL.172
        AKHS=HOL(I,J)                                                            MYEPBL.173
        AKMS=MOL(I,J)                                                            MYEPBL.174
        WETM=MAVAIL(I,J)                                                         MYEPBL.175
        UZ0H=UZ0HI(I)                                                            MYEPBL.176
        VZ0H=VZ0HI(I)                                                            MYEPBL.177
        DO L=1,LMHK                                                              MYEPBL.178
          ZCOL(L)=ZCOLI(I,L)                                                     MYEPBL.179
          APECOL(L)=APECOLI(I,L)                                                 MYEPBL.180
        ENDDO                                                                    05DEC01.1148
        DO L=1,LMHK-1                                                            05DEC01.1149
          AKHCOL(L)=AKHCOLI(I,L)                                                 MYEPBL.181
          AKMCOL(L)=AKMCOLI(I,L)                                                 MYEPBL.182
        ENDDO                                                                    MYEPBL.183
        ZCOL(LMHK+1)=ZCOLI(I,LMHK+1)                                             MYEPBL.184
C SET COLUMN ARRAYS                                                              MYEPBL.185
        DO L=1,LMHK                                                              MYEPBL.186
          UCOL(L)=0.25*(U3D(I,J,L)+U3D(I+1,J,L)+U3D(I,J+1,L)+                    MYEPBL.187
     +            U3D(I+1,J+1,L))                                                MYEPBL.188
          VCOL(L)=0.25*(V3D(I,J,L)+V3D(I+1,J,L)+V3D(I,J+1,L)+                    MYEPBL.189
     +            V3D(I+1,J+1,L))                                                MYEPBL.190
          TCOL(L)=T3D(I,J,L)                                                     MYEPBL.191
          QCOL(L)=QV3D(I,J,L)                                                    MYEPBL.192
          Q2COL(L)=Q2COLI(I,L)                                                   MYEPBL.193
          UCOLD(L)=UCOL(L)                                                       MYEPBL.194
          VCOLD(L)=VCOL(L)                                                       MYEPBL.195
          TCOLD(L)=TCOL(L)                                                       MYEPBL.196
          QCOLD(L)=QCOL(L)                                                       MYEPBL.197
          Q2COLD(L)=TURKE(I,J,L)                                                 MYEPBL.198
        ENDDO                                                                    MYEPBL.199
C     FOR CONSISTENT SURFACE FLUXES WITH SLAB, THZ0 AND QZ0 ARE MODIFIED         MYEPBL.200
        IF (ISOIL.LE.1) THEN                                                     05DEC01.1150
        THZ0=THX(I,LMHK)+HFX(I,J)/FLHC(I)                                        MYEPBL.201
        QZ0=QX(I,LMHK)+QFX(I,J)/FLQC(I)                                          MYEPBL.202
        ELSE IF (ISOIL.EQ.2) THEN                                                05DEC01.1151
        THZ0=THX(I,LMHK)+HFX(I,J)/(RHOX(I)*CP*CHS(I))                            05DEC01.1152
        QZ0=QX(I,LMHK)+QFX(I,J)/(RHOX(I)*CHS(I))                                 05DEC01.1153
        WETM=CKLQ(I,J)                                                           05DEC01.1154
        END IF                                                                   05DEC01.1155
C UPDATE HEAT, MOISTURE AND MOMENTUM WITH VERTICAL MIXING                        MYEPBL.203
        CALL VDIFH(LMHK,KTMQ2,DTQ2,THZ0,QZ0,AKHS,CT,WETM,TCOL,QCOL,              MYEPBL.204
     +       AKHCOL,APECOL,ZCOL)                                                 MYEPBL.205
        CALL VDIFV(LMHK,KTMQ2,DTQ2,UZ0H,VZ0H,AKMS,UCOL,VCOL,AKMCOL,ZCOL)         MYEPBL.206
C SET TENDENCIES                                                                 MYEPBL.207
        DO K=1,KL                                                                MYEPBL.208
          IF(I.GE.IBNES.AND.I.LE.IENES-1.AND.J.GE.JBNES.AND.J.LE.JENES-1         MYEPBL.209
     +      )THEN                                                                MYEPBL.210
            T3DTEN(I,J,K)=T3DTEN(I,J,K)+(TCOL(K)-TCOLD(K))/DTQ2*PSB(I,J)         MYEPBL.211
            QV3DTEN(I,J,K)=QV3DTEN(I,J,K)+(QCOL(K)-QCOLD(K))/DTQ2*               MYEPBL.212
     +                     PSB(I,J)                                              MYEPBL.213
          ENDIF                                                                  MYEPBL.214
          IF(I.GE.IBNES.AND.I.LE.IENES.AND.J.GE.JBNES.AND.J.LE.JENES)            MYEPBL.215
     +      THEN                                                                 MYEPBL.216
C     CROSS-POINT TENDENCIES OF U AND V                                          MYEPBL.217
            U3DTEN(I,J,K)=U3DTEN(I,J,K)+(UCOL(K)-UCOLD(K))/DTQ2*PSB(I,J)         MYEPBL.218
            V3DTEN(I,J,K)=V3DTEN(I,J,K)+(VCOL(K)-VCOLD(K))/DTQ2*PSB(I,J)         MYEPBL.219
          ENDIF                                                                  MYEPBL.220
C     UPDATE TURKE USING 0.5*DT                                                  MYEPBL.221
          TURKE(I,J,K)=Q2COLD(K)+0.5*(Q2COL(K)-Q2COLD(K))                        MYEPBL.222
        ENDDO                                                                    MYEPBL.223
C STORE SURFACE VALUES                                                           MYEPBL.224
        HOL(I,J)=AKHS                                                            MYEPBL.225
        MOL(I,J)=AKMS                                                            MYEPBL.226
C                                                                                MYEPBL.227
C ESTIMATE PBL HEIGHT FROM TKE - FOR V3.5                                        05DEC01.1156
C                                                                                05DEC01.1157
C   This is how it is done:                                                      05DEC01.1158
C     Following what is done in GS PBL, PBL height is determined                 05DEC01.1159
C     based on the TKE profile. In a general case, the PBL height                05DEC01.1160
C     is the height where TKE drops below a threshold value.                     05DEC01.1161
C     This threshold is 0.5xmin(0.6,TKEMAX) if the maximum TKE                   05DEC01.1162
C     is greater than 1.2.                                                       05DEC01.1163
C     It is KL if max TKE is less than TKE=0.4.                                  05DEC01.1164
C                                                                                05DEC01.1165
C     Algorithm parameters:                                                      05DEC01.1166
C                                                                                05DEC01.1167
        PTKEMAX=1.20                                                             05DEC01.1168
        PTKEFRACT=0.50                                                           05DEC01.1169
        PTKEWEAK=0.4                                                             05DEC01.1170
        PTKEZMIN=150.0                                                           05DEC01.1171
C                                                                                05DEC01.1172
C     Initialization of KPBL2D(mix,mjx) array                                    05DEC01.1173
C                                                                                05DEC01.1174
        KPBL2D(I,J)=-99                                                          05DEC01.1175
        REGIME(I,J)=1.                                                           05DEC01.1176
C                                                                                05DEC01.1177
C     Find the layer in which to look for maximum TKE                            05DEC01.1178
C     The layer is either where TKE begins to decrease or 150 m AGL              05DEC01.1179
C       LM=MKX                                                                   05DEC01.1180
C                                                                                05DEC01.1181
        KTEMP1=KL                                                                05DEC01.1182
        DO K=KL,1,-1                                                             05DEC01.1183
          IF(TURKE(I,J,K-1).LT.TURKE(I,J,K))THEN                                 05DEC01.1184
            KTEMP1=K-1                                                           05DEC01.1185
            GOTO 305                                                             05DEC01.1186
          ENDIF                                                                  05DEC01.1187
        ENDDO                                                                    05DEC01.1188
 305    CONTINUE                                                                 05DEC01.1189
        KTEMP2=KL                                                                05DEC01.1190
        DO K=KL,1,-1                                                             05DEC01.1191
           IF(ZA(I,K).GE.PTKEZMIN)THEN                                           05DEC01.1192
              KTEMP2=K                                                           05DEC01.1193
              GOTO 306                                                           05DEC01.1194
           ENDIF                                                                 05DEC01.1195
        ENDDO                                                                    05DEC01.1196
 306    CONTINUE                                                                 05DEC01.1197
        KTEMP=MIN(KTEMP1,KTEMP2)                                                 05DEC01.1198
C                                                                                05DEC01.1199
C     Find the maximum TKE below KTEMP                                           05DEC01.1200
C                                                                                05DEC01.1201
        TKEMAX=TURKE(I,J,KL)                                                     05DEC01.1202
        DO K=LM1,KTEMP,-1                                                        05DEC01.1203
           IF(TKEMAX.LT.TURKE(I,J,K)) TKEMAX=TURKE(I,J,K)                        05DEC01.1204
        ENDDO                                                                    05DEC01.1205
C                                                                                05DEC01.1206
C     Estimate PBL height                                                        05DEC01.1207
C                                                                                05DEC01.1208
C     Cases of weak or no turbulence                                             05DEC01.1209
C                                                                                05DEC01.1210
        IF(TKEMAX.LT.PTKEWEAK)THEN                                               05DEC01.1211
           PBL(I,J)=0.0                                                          05DEC01.1212
           KPBL2D(I,J)=KL                                                        05DEC01.1213
           GOTO 301                                                              05DEC01.1214
        ELSE                                                                     05DEC01.1215
C                                                                                05DEC01.1216
C     Cases of stronger turbulence                                               05DEC01.1217
C                                                                                05DEC01.1218
           TKELIM=PTKEFRACT*MIN(TKEMAX,PTKEMAX)                                  05DEC01.1219
           TKELIM=MAX(TKELIM,0.4)                                                05DEC01.1220
           DO K=LM1,1,-1                                                         05DEC01.1221
              IF(TURKE(I,J,K).LT.TKELIM.AND.                                     05DEC01.1222
     -           TURKE(I,J,K).LT.TURKE(I,J,K+1)) THEN                            05DEC01.1223
                 FRACT=(TURKE(I,J,K+1)-TKELIM)/                                  05DEC01.1224
     -                 (TURKE(I,J,K+1)-TURKE(I,J,K))                             05DEC01.1225
                 IF(FRACT.GE.0.0.AND.FRACT.LE.1.0)THEN                           23DEC04.3651
                    PBL(I,J)=ZA(I,K+1)+FRACT*(ZA(I,K)-ZA(I,K+1))                 05DEC01.1227
                    IF(PBL(I,J).LE.ZQ(I,K+1)) THEN                               05DEC01.1228
                       KPBL2D(I,J)=K+1                                           05DEC01.1229
                    ELSE                                                         05DEC01.1230
                       KPBL2D(I,J)=K                                             05DEC01.1231
                    END IF                                                       05DEC01.1232
                    GOTO 301                                                     05DEC01.1233
                 ENDIF                                                           23DEC04.3652
              ELSEIF(ZA(I,K).GT.5000.)THEN                                       05DEC01.1235
C                PRINT *,'PBL HT LIMITED TO 5 KM AGL AT ',I,J                    05DEC01.1236
                 PBL(I,J)=5000.                                                  05DEC01.1237
                 KPBL2D(I,J)=K                                                   05DEC01.1238
                 GOTO 301                                                        05DEC01.1239
              ENDIF                                                              05DEC01.1240
           ENDDO                                                                 05DEC01.1241
           STOP 294                                                              05DEC01.1242
        ENDIF                                                                    05DEC01.1243
  301   CONTINUE                                                                 05DEC01.1244
C                                                                                05DEC01.1245
C    Set REGIME for FDDA use                                                     05DEC01.1246
C                                                                                05DEC01.1247
        IF(KPBL2D(I,J).LT.KL)THEN                                                05DEC01.1248
           REGIME(I,J) = 4.                                                      05DEC01.1249
        ENDIF                                                                    05DEC01.1250
C end of PBL height estimation                                                   05DEC01.1251
                                                                                 05DEC01.1252
      ENDDO                                                                      MYEPBL.228
C-----END 2ND BIG I LOOP                                                         MYEPBL.229
C                                                                                MYEPBL.230
C-----APPLY ASSELIN FILTER TO TGD FOR LARGE TIME STEP:                           MYEPBL.231
C                                                                                MYEPBL.232
      IF(ITGFLG.EQ.1)THEN                                                        MYEPBL.233
        DO I=1,ILX                                                               MYEPBL.234
          TGC=TGB(I,J)                                                           MYEPBL.235
          TGB(I,J)=OMUHF*TGA(I,J)+GNUHF*(TGC+TGDSB(I))                           MYEPBL.236
          TGA(I,J)=TGC                                                           MYEPBL.237
          IF(ISOIL.EQ.1)THEN                                                     MYEPBL.238
            DO L=1,5                                                             MYEPBL.239
              TSLC=TSLB(I,J,L)                                                   MYEPBL.240
              TSLB(I,J,L)=OMUHF*TSLA(I,J,L)+GNUHF*(TSLC+TSLDSB(I,L))             MYEPBL.241
              TSLA(I,J,L)=TSLC                                                   MYEPBL.242
            ENDDO                                                                MYEPBL.243
          ENDIF                                                                  MYEPBL.244
        ENDDO                                                                    MYEPBL.245
      ENDIF                                                                      MYEPBL.246
C                                                                                MYEPBL.252
      RETURN                                                                     MYEPBL.253
      END                                                                        MYEPBL.254
                                                                                 MYEPBL.255
