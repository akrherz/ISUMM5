      SUBROUTINE SINT(XF,N,M,N1STAR,N1END,N2STAR,N2END)                          SINT.1
#     include <parame.incl>                                                      SINT.2
                                                                                 SINT.3
      PARAMETER(ONE12=1./12.,ONE24=1./24.)                                       SINT.4
      PARAMETER(N1=JJMX,N2=IIMX,NF=9,IFCT=1,IOR=2)                               SINT.5
C                                                                                SINT.6
      REAL XF(N,M,NF)                                                            SINT.7
C                                                                                SINT.8
      REAL Y(N1,N2,-IOR:IOR),Z(N1,N2,-IOR:IOR),                                  SINT.9
     1               F(N1,N2,0:1)                                                SINT.10
C                                                                                SINT.11
      DATA  EP/ 1.E-10/                                                          SINT.12
C                                                                                SINT.13
      PARAMETER(NONOS=1)                                                         SINT.14
      PARAMETER(N1OS=N1*NONOS+1-NONOS,N2OS=N2*NONOS+1-NONOS)                     SINT.15
C                                                                                SINT.16
      REAL W(N1OS,N2OS),OV(N1OS,N2OS),UN(N1OS,N2OS)                              SINT.17
      REAL MXM(N1OS,N2OS),MN(N1OS,N2OS)                                          SINT.18
      REAL FL(N1OS,N2OS,0:1)                                                     SINT.19
      COMMON /DEPAR2/ XIG(NF),XJG(NF),IG0(N1,N2),JG0(N1,N2)                      SINT.20
C                                                                                SINT.21
                                                                                 SINT.22
      DONOR(Y1,Y2,A)=(Y1*AMAX1(0.,SIGN(1.,A))-Y2*AMIN1(0.,SIGN(1.,A)))*A         SINT.23
C     TR2(Y1,Y2,A)=A*.5*(Y1+Y2)-A**2*.5*(Y2-Y1)                                  SINT.24
C                                                                                SINT.25
C      TR4(YM1,Y0,YP1,YP2,A)=A/12.*(7.*(YP1+Y0)-(YP2+YM1))                       SINT.26
C     1 -A**2/24.*(15.*(YP1-Y0)-(YP2-YM1))-A**3/12.*((YP1+Y0)                    SINT.27
C     2 -(YP2+YM1))+A**4/24.*(3.*(YP1-Y0)-(YP2-YM1))                             SINT.28
      TR4(YM1,Y0,YP1,YP2,A)=A*ONE12*(7.*(YP1+Y0)-(YP2+YM1))                      SINT.29
     1 -A*A*ONE24*(15.*(YP1-Y0)-(YP2-YM1))-A*A*A*ONE12*((YP1+Y0)                 SINT.30
     2 -(YP2+YM1))+A*A*A*A*ONE24*(3.*(YP1-Y0)-(YP2-YM1))                         SINT.31
C                                                                                SINT.32
C     TR6(YM2,YM1,Y0,YP1,YP2,YP3,A)=-A/60.*(-YM2+8.*YM1-37.*Y0                   SINT.33
C    1                                     -37.*YP1+8.*YP2-YP3)                  SINT.34
C    2-A**2/360.*(-2.*YM2+25.*YM1-245.*Y0+245.*YP1-25.*YP2+2.*YP3)               SINT.35
C    3-A**3/48.*(YM2-7.*YM1+6.*Y0+6.*YP1-7.*YP2+YP3)                             SINT.36
C    4-A**4/144.*(YM2-11.*YM1+28.*Y0-28.*YP1+11.*YP2-YP3)                        SINT.37
C    5-A**5/240.*(-YM2+3.*YM1-2.*Y0-2.*YP1+3.*YP2-YP3)                           SINT.38
C    6-A**6/720.*(-YM2+5.*YM1-10.*Y0+10.*YP1-5.*YP2+YP3)                         SINT.39
      PP(X)=AMAX1(0.,X)                                                          SINT.40
      PN(X)=AMIN1(0.,X)                                                          SINT.41
C                                                                                SINT.42
C SGI PAR MODS                                                                   SINT.43
C$DOACROSS                                                                       SINT.44
C$& SHARE(XF,XIG,XJG,IG0,JG0,EP),                                                SINT.45
C$& LASTLOCAL(IFM),                                                              SINT.46
C$& LOCAL(J,I,JJ,II,Y,Z,V,F,W,FL,OV,UN,MXM,MN)                                   SINT.47
C                                                                                SINT.48
      DO 2000 IFM=1,NF                                                           SINT.49
C                                                                                SINT.50
C  HERE STARTS RESIDUAL ADVECTION                                                SINT.51
C                                                                                SINT.52
C*$* ASSERT DO PREFER(CONCURRENT)                                                SINT.53
        DO 9000 JJ=N2STAR,N2END                                                  SINT.54
C                                                                                SINT.55
          DO 50 J=-IOR,IOR                                                       SINT.56
C                                                                                SINT.57
            DO 51 I=-IOR,IOR                                                     SINT.58
CC      DO 511 JJ=N2STAR,N2END                                                   SINT.59
              DO 511 II=N1STAR,N1END                                             SINT.60
  511         Y(II,JJ,I)=XF(IG0(II,JJ)+I,JG0(II,JJ)+J,IFM)                       SINT.61
   51       CONTINUE                                                             SINT.62
C                                                                                SINT.63
CC      DO 811 JJ=N2STAR,N2END                                                   SINT.64
C                                                                                SINT.65
CC      DO 8000 JJ=N2STAR,N2END                                                  SINT.66
            DO 811 II=N1STAR,N1END                                               SINT.67
              FL(II,JJ,0)=DONOR(Y(II,JJ,-1),Y(II,JJ,0),XIG(IFM))                 SINT.68
  811       FL(II,JJ,1)=DONOR(Y(II,JJ,0),Y(II,JJ,1),XIG(IFM))                    SINT.69
C      DO 812 JJ=N2STAR,N2END                                                    SINT.70
            DO 812 II=N1STAR,N1END                                               SINT.71
  812       W(II,JJ)=Y(II,JJ,0)-(FL(II,JJ,1)-FL(II,JJ,0))                        SINT.72
C      DO 813 JJ=N2STAR,N2END                                                    SINT.73
            DO 813 II=N1STAR,N1END                                               SINT.74
              MXM(II,JJ)=                                                        SINT.75
     +                   AMAX1(Y(II,JJ,-1),Y(II,JJ,0),Y(II,JJ,1),                SINT.76
     +                   W(II,JJ))                                               SINT.77
  813       MN(II,JJ)=AMIN1(Y(II,JJ,-1),Y(II,JJ,0),Y(II,JJ,1),W(II,JJ))          SINT.78
C                                                                                SINT.79
C      DO 312 JJ=N2STAR,N2END                                                    SINT.80
            DO 312 II=N1STAR,N1END                                               SINT.81
              F(II,JJ,0)=                                                        SINT.82
     +                   TR4(Y(II,JJ,-2),Y(II,JJ,-1),Y(II,JJ,0),                 SINT.83
     +                   Y(II,JJ,1),XIG(IFM))                                    SINT.84
  312       F(II,JJ,1)=                                                          SINT.85
     +                 TR4(Y(II,JJ,-1),Y(II,JJ,0),Y(II,JJ,1),Y(II,JJ,2),         SINT.86
     +                 XIG(IFM))                                                 SINT.87
C                                                                                SINT.88
C      DO 822 JJ=N2STAR,N2END                                                    SINT.89
            DO 822 II=N1STAR,N1END                                               SINT.90
              F(II,JJ,0)=F(II,JJ,0)-FL(II,JJ,0)                                  SINT.91
  822       F(II,JJ,1)=F(II,JJ,1)-FL(II,JJ,1)                                    SINT.92
C      DO 823 JJ=N2STAR,N2END                                                    SINT.93
            DO 823 II=N1STAR,N1END                                               SINT.94
              OV(II,JJ)=(MXM(II,JJ)-W(II,JJ))/(-PN(F(II,JJ,1))+                  SINT.95
     +                  PP(F(II,JJ,0))+EP)                                       SINT.96
  823       UN(II,JJ)=(W(II,JJ)-MN(II,JJ))/(PP(F(II,JJ,1))-                      SINT.97
     +                PN(F(II,JJ,0))+EP)                                         SINT.98
C      DO 824 JJ=N2STAR,N2END                                                    SINT.99
            DO 824 II=N1STAR,N1END                                               SINT.100
              F(II,JJ,0)=PP(F(II,JJ,0))*AMIN1(1.,OV(II,JJ))+                     SINT.101
     +                   PN(F(II,JJ,0))*AMIN1(1.,UN(II,JJ))                      SINT.102
              F(II,JJ,1)=PP(F(II,JJ,1))*AMIN1(1.,UN(II,JJ))+                     SINT.103
     +                   PN(F(II,JJ,1))*AMIN1(1.,OV(II,JJ))                      SINT.104
  824       CONTINUE                                                             SINT.105
C      DO 825 JJ=N2STAR,N2END                                                    SINT.106
            DO 825 II=N1STAR,N1END                                               SINT.107
  825       Y(II,JJ,0)=W(II,JJ)-(F(II,JJ,1)-F(II,JJ,0))                          SINT.108
C                                                                                SINT.109
C      DO 361 JJ=N2STAR,N2END                                                    SINT.110
            DO 361 II=N1STAR,N1END                                               SINT.111
  361       Z(II,JJ,J)=Y(II,JJ,0)                                                SINT.112
C                                                                                SINT.113
C  END IF FIRST J LOOP                                                           SINT.114
C                                                                                SINT.115
 8000       CONTINUE                                                             SINT.116
   50     CONTINUE                                                               SINT.117
C                                                                                SINT.118
C      DO 911 JJ=N2STAR,N2END                                                    SINT.119
C      DO 9000 JJ=N2STAR,N2END                                                   SINT.120
          DO 911 II=N1STAR,N1END                                                 SINT.121
            FL(II,JJ,0)=DONOR(Z(II,JJ,-1),Z(II,JJ,0),XJG(IFM))                   SINT.122
  911     FL(II,JJ,1)=DONOR(Z(II,JJ,0),Z(II,JJ,1),XJG(IFM))                      SINT.123
C      DO 912 JJ=N2STAR,N2END                                                    SINT.124
          DO 912 II=N1STAR,N1END                                                 SINT.125
  912     W(II,JJ)=Z(II,JJ,0)-(FL(II,JJ,1)-FL(II,JJ,0))                          SINT.126
C      DO 913 JJ=N2STAR,N2END                                                    SINT.127
          DO 913 II=N1STAR,N1END                                                 SINT.128
            MXM(II,JJ)=AMAX1(Z(II,JJ,-1),Z(II,JJ,0),Z(II,JJ,1),W(II,JJ))         SINT.129
  913     MN(II,JJ)=AMIN1(Z(II,JJ,-1),Z(II,JJ,0),Z(II,JJ,1),W(II,JJ))            SINT.130
C                                                                                SINT.131
C      DO 412 JJ=N2STAR,N2END                                                    SINT.132
          DO 412 II=N1STAR,N1END                                                 SINT.133
            F(II,JJ,0)=                                                          SINT.134
     +                 TR4(Z(II,JJ,-2),Z(II,JJ,-1),Z(II,JJ,0),Z(II,JJ,1)         SINT.135
     +                 ,XJG(IFM))                                                SINT.136
  412     F(II,JJ,1)=                                                            SINT.137
     +               TR4(Z(II,JJ,-1),Z(II,JJ,0),Z(II,JJ,1),Z(II,JJ,2),           SINT.138
     +               XJG(IFM))                                                   SINT.139
C                                                                                SINT.140
C      DO 922 JJ=N2STAR,N2END                                                    SINT.141
          DO 922 II=N1STAR,N1END                                                 SINT.142
            F(II,JJ,0)=F(II,JJ,0)-FL(II,JJ,0)                                    SINT.143
  922     F(II,JJ,1)=F(II,JJ,1)-FL(II,JJ,1)                                      SINT.144
C      DO 923 JJ=N2STAR,N2END                                                    SINT.145
          DO 923 II=N1STAR,N1END                                                 SINT.146
            OV(II,JJ)=(MXM(II,JJ)-W(II,JJ))/(-PN(F(II,JJ,1))+                    SINT.147
     +                PP(F(II,JJ,0))+EP)                                         SINT.148
  923     UN(II,JJ)=(W(II,JJ)-MN(II,JJ))/(PP(F(II,JJ,1))-PN(F(II,JJ,0))+         SINT.149
     +              EP)                                                          SINT.150
C      DO 924 JJ=N2STAR,N2END                                                    SINT.151
          DO 924 II=N1STAR,N1END                                                 SINT.152
            F(II,JJ,0)=PP(F(II,JJ,0))*AMIN1(1.,OV(II,JJ))+PN(F(II,JJ,0))         SINT.153
     +                 *AMIN1(1.,UN(II,JJ))                                      SINT.154
            F(II,JJ,1)=PP(F(II,JJ,1))*AMIN1(1.,UN(II,JJ))+PN(F(II,JJ,1))         SINT.155
     +                 *AMIN1(1.,OV(II,JJ))                                      SINT.156
  924     CONTINUE                                                               SINT.157
 9000   CONTINUE                                                                 SINT.158
        DO 925 JJ=N2STAR,N2END                                                   SINT.159
          DO 925 II=N1STAR,N1END                                                 SINT.160
  925     XF(II,JJ,IFM)=W(II,JJ)-(F(II,JJ,1)-F(II,JJ,0))                         SINT.161
                                                                                 SINT.162
C                                                                                SINT.163
 2000 CONTINUE                                                                   SINT.164
      RETURN                                                                     SINT.165
      END                                                                        SINT.166
                                                                                 SINT.167
