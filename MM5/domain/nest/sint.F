      SUBROUTINE SINT(XF,N,M,N1STAR,N1END,N2STAR,N2END)                          SINT.1
#     include <parame.incl>                                                      SINT.2
                                                                                 SINT.3
      PARAMETER(ONE12=1./12.,ONE24=1./24.)                                       SINT.4
      PARAMETER(N1=MJX,N2=MIX,NF=9,IFCT=1,IOR=2)                                 SINT.5
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
c$doacross                                                                       SINT.44
c$& lastlocal(iim),                                                              SINT.45
c$& local(j,i,jj,ii,y,z,v,f,w,fl,ov,un,mxm,mn)                                   SINT.46
c$omp parallel do lastprivate(iim)                                               SINT.47
c$omp&                                                                           SINT.48
c$omp&shared(xf,xig,xjg,ig0,jg0,ep),                                             SINT.49
c$omp&private(j,i,jj,ii,y,z,v,f,w,fl,ov,un,mxm,mn)                               SINT.50
C                                                                                SINT.51
      DO 2000 IIM=1,NF                                                           SINT.52
C                                                                                SINT.53
C  HERE STARTS RESIDUAL ADVECTION                                                SINT.54
C                                                                                SINT.55
C*$* ASSERT DO PREFER(CONCURRENT)                                                SINT.56
        DO 9000 JJ=N2STAR,N2END                                                  SINT.57
C                                                                                SINT.58
          DO 50 J=-IOR,IOR                                                       SINT.59
C                                                                                SINT.60
            DO 51 I=-IOR,IOR                                                     SINT.61
CC      DO 511 JJ=N2STAR,N2END                                                   SINT.62
              DO 511 II=N1STAR,N1END                                             SINT.63
  511         Y(II,JJ,I)=XF(IG0(II,JJ)+I,JG0(II,JJ)+J,IIM)                       SINT.64
   51       CONTINUE                                                             SINT.65
C                                                                                SINT.66
CC      DO 811 JJ=N2STAR,N2END                                                   SINT.67
C                                                                                SINT.68
CC      DO 8000 JJ=N2STAR,N2END                                                  SINT.69
            DO 811 II=N1STAR,N1END                                               SINT.70
              FL(II,JJ,0)=DONOR(Y(II,JJ,-1),Y(II,JJ,0),XIG(IIM))                 SINT.71
  811       FL(II,JJ,1)=DONOR(Y(II,JJ,0),Y(II,JJ,1),XIG(IIM))                    SINT.72
C      DO 812 JJ=N2STAR,N2END                                                    SINT.73
            DO 812 II=N1STAR,N1END                                               SINT.74
  812       W(II,JJ)=Y(II,JJ,0)-(FL(II,JJ,1)-FL(II,JJ,0))                        SINT.75
C      DO 813 JJ=N2STAR,N2END                                                    SINT.76
            DO 813 II=N1STAR,N1END                                               SINT.77
              MXM(II,JJ)=                                                        SINT.78
     +                   AMAX1(Y(II,JJ,-1),Y(II,JJ,0),Y(II,JJ,1),                SINT.79
     +                   W(II,JJ))                                               SINT.80
  813       MN(II,JJ)=AMIN1(Y(II,JJ,-1),Y(II,JJ,0),Y(II,JJ,1),W(II,JJ))          SINT.81
C                                                                                SINT.82
C      DO 312 JJ=N2STAR,N2END                                                    SINT.83
            DO 312 II=N1STAR,N1END                                               SINT.84
              F(II,JJ,0)=                                                        SINT.85
     +                   TR4(Y(II,JJ,-2),Y(II,JJ,-1),Y(II,JJ,0),                 SINT.86
     +                   Y(II,JJ,1),XIG(IIM))                                    SINT.87
  312       F(II,JJ,1)=                                                          SINT.88
     +                 TR4(Y(II,JJ,-1),Y(II,JJ,0),Y(II,JJ,1),Y(II,JJ,2),         SINT.89
     +                 XIG(IIM))                                                 SINT.90
C                                                                                SINT.91
C      DO 822 JJ=N2STAR,N2END                                                    SINT.92
            DO 822 II=N1STAR,N1END                                               SINT.93
              F(II,JJ,0)=F(II,JJ,0)-FL(II,JJ,0)                                  SINT.94
  822       F(II,JJ,1)=F(II,JJ,1)-FL(II,JJ,1)                                    SINT.95
C      DO 823 JJ=N2STAR,N2END                                                    SINT.96
            DO 823 II=N1STAR,N1END                                               SINT.97
              OV(II,JJ)=(MXM(II,JJ)-W(II,JJ))/(-PN(F(II,JJ,1))+                  SINT.98
     +                  PP(F(II,JJ,0))+EP)                                       SINT.99
  823       UN(II,JJ)=(W(II,JJ)-MN(II,JJ))/(PP(F(II,JJ,1))-                      SINT.100
     +                PN(F(II,JJ,0))+EP)                                         SINT.101
C      DO 824 JJ=N2STAR,N2END                                                    SINT.102
            DO 824 II=N1STAR,N1END                                               SINT.103
              F(II,JJ,0)=PP(F(II,JJ,0))*AMIN1(1.,OV(II,JJ))+                     SINT.104
     +                   PN(F(II,JJ,0))*AMIN1(1.,UN(II,JJ))                      SINT.105
              F(II,JJ,1)=PP(F(II,JJ,1))*AMIN1(1.,UN(II,JJ))+                     SINT.106
     +                   PN(F(II,JJ,1))*AMIN1(1.,OV(II,JJ))                      SINT.107
  824       CONTINUE                                                             SINT.108
C      DO 825 JJ=N2STAR,N2END                                                    SINT.109
            DO 825 II=N1STAR,N1END                                               SINT.110
  825       Y(II,JJ,0)=W(II,JJ)-(F(II,JJ,1)-F(II,JJ,0))                          SINT.111
C                                                                                SINT.112
C      DO 361 JJ=N2STAR,N2END                                                    SINT.113
            DO 361 II=N1STAR,N1END                                               SINT.114
  361       Z(II,JJ,J)=Y(II,JJ,0)                                                SINT.115
C                                                                                SINT.116
C  END IF FIRST J LOOP                                                           SINT.117
C                                                                                SINT.118
 8000       CONTINUE                                                             SINT.119
   50     CONTINUE                                                               SINT.120
C                                                                                SINT.121
C      DO 911 JJ=N2STAR,N2END                                                    SINT.122
C      DO 9000 JJ=N2STAR,N2END                                                   SINT.123
          DO 911 II=N1STAR,N1END                                                 SINT.124
            FL(II,JJ,0)=DONOR(Z(II,JJ,-1),Z(II,JJ,0),XJG(IIM))                   SINT.125
  911     FL(II,JJ,1)=DONOR(Z(II,JJ,0),Z(II,JJ,1),XJG(IIM))                      SINT.126
C      DO 912 JJ=N2STAR,N2END                                                    SINT.127
          DO 912 II=N1STAR,N1END                                                 SINT.128
  912     W(II,JJ)=Z(II,JJ,0)-(FL(II,JJ,1)-FL(II,JJ,0))                          SINT.129
C      DO 913 JJ=N2STAR,N2END                                                    SINT.130
          DO 913 II=N1STAR,N1END                                                 SINT.131
            MXM(II,JJ)=AMAX1(Z(II,JJ,-1),Z(II,JJ,0),Z(II,JJ,1),W(II,JJ))         SINT.132
  913     MN(II,JJ)=AMIN1(Z(II,JJ,-1),Z(II,JJ,0),Z(II,JJ,1),W(II,JJ))            SINT.133
C                                                                                SINT.134
C      DO 412 JJ=N2STAR,N2END                                                    SINT.135
          DO 412 II=N1STAR,N1END                                                 SINT.136
            F(II,JJ,0)=                                                          SINT.137
     +                 TR4(Z(II,JJ,-2),Z(II,JJ,-1),Z(II,JJ,0),Z(II,JJ,1)         SINT.138
     +                 ,XJG(IIM))                                                SINT.139
  412     F(II,JJ,1)=                                                            SINT.140
     +               TR4(Z(II,JJ,-1),Z(II,JJ,0),Z(II,JJ,1),Z(II,JJ,2),           SINT.141
     +               XJG(IIM))                                                   SINT.142
C                                                                                SINT.143
C      DO 922 JJ=N2STAR,N2END                                                    SINT.144
          DO 922 II=N1STAR,N1END                                                 SINT.145
            F(II,JJ,0)=F(II,JJ,0)-FL(II,JJ,0)                                    SINT.146
  922     F(II,JJ,1)=F(II,JJ,1)-FL(II,JJ,1)                                      SINT.147
C      DO 923 JJ=N2STAR,N2END                                                    SINT.148
          DO 923 II=N1STAR,N1END                                                 SINT.149
            OV(II,JJ)=(MXM(II,JJ)-W(II,JJ))/(-PN(F(II,JJ,1))+                    SINT.150
     +                PP(F(II,JJ,0))+EP)                                         SINT.151
  923     UN(II,JJ)=(W(II,JJ)-MN(II,JJ))/(PP(F(II,JJ,1))-PN(F(II,JJ,0))+         SINT.152
     +              EP)                                                          SINT.153
C      DO 924 JJ=N2STAR,N2END                                                    SINT.154
          DO 924 II=N1STAR,N1END                                                 SINT.155
            F(II,JJ,0)=PP(F(II,JJ,0))*AMIN1(1.,OV(II,JJ))+PN(F(II,JJ,0))         SINT.156
     +                 *AMIN1(1.,UN(II,JJ))                                      SINT.157
            F(II,JJ,1)=PP(F(II,JJ,1))*AMIN1(1.,UN(II,JJ))+PN(F(II,JJ,1))         SINT.158
     +                 *AMIN1(1.,OV(II,JJ))                                      SINT.159
  924     CONTINUE                                                               SINT.160
 9000   CONTINUE                                                                 SINT.161
        DO 925 JJ=N2STAR,N2END                                                   SINT.162
          DO 925 II=N1STAR,N1END                                                 SINT.163
  925     XF(II,JJ,IIM)=W(II,JJ)-(F(II,JJ,1)-F(II,JJ,0))                         SINT.164
                                                                                 SINT.165
C                                                                                SINT.166
 2000 CONTINUE                                                                   SINT.167
      RETURN                                                                     SINT.168
      END                                                                        SINT.169
                                                                                 SINT.170
