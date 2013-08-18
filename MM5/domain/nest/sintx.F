      SUBROUTINE SINTX(XF,N,M,N1STAR,N1END,JXWANT,                               SINTX.1
     1          ICOMPS,ICRSDOT)                                                  SINTX.2
#     include <parame.incl>                                                      SINTX.3
      PARAMETER(N1=MJX,N2=MIX,NF=9,IFCT=1,IOR=2)                                 SINTX.4
      PARAMETER(ONE12=1./12.,ONE24=1./24.)                                       SINTX.5
C                                                                                SINTX.6
      DIMENSION XF(N,M,9)                                                        SINTX.7
      DIMENSION Y(N1,-IOR:IOR),Z(N1,-IOR:IOR),                                   SINTX.8
     1                 F(N1,0:1)                                                 SINTX.9
      DATA  EP/ 1.E-10/                                                          SINTX.10
      PARAMETER(NONOS=1)                                                         SINTX.11
      PARAMETER(N1OS=N1*NONOS+1-NONOS,N2OS=N2*NONOS+1-NONOS)                     SINTX.12
      DIMENSION  W(N1OS),FL(N1OS,0:1),                                           SINTX.13
     1          OV(N1OS),UN(N1OS)                                                SINTX.14
      REAL      MXM(N1OS),MN(N1OS)                                               SINTX.15
      COMMON /DEPAR2/ XIG(9),XJG(9),IG0(N1,N2),JG0(N1,N2)                        SINTX.16
      INTEGER ICOM(9)                                                            SINTX.17
C                                                                                SINTX.18
      DONOR(Y1,Y2,A)=(Y1*AMAX1(0.,SIGN(1.,A))-Y2*AMIN1(0.,SIGN(1.,A)))*A         SINTX.19
C     TR2(Y1,Y2,A)=A*.5*(Y1+Y2)-A**2*.5*(Y2-Y1)                                  SINTX.20
C      TR4(YM1,Y0,YP1,YP2,A)=A/12.*(7.*(YP1+Y0)-(YP2+YM1))                       SINTX.21
C     1 -A**2/24.*(15.*(YP1-Y0)-(YP2-YM1))-A**3/12.*((YP1+Y0)                    SINTX.22
C     2 -(YP2+YM1))+A**4/24.*(3.*(YP1-Y0)-(YP2-YM1))                             SINTX.23
      TR4(YM1,Y0,YP1,YP2,A)=A*ONE12*(7.*(YP1+Y0)-(YP2+YM1))                      SINTX.24
     1 -A*A*ONE24*(15.*(YP1-Y0)-(YP2-YM1))-A*A*A*ONE12*((YP1+Y0)                 SINTX.25
     2 -(YP2+YM1))+A*A*A*A*ONE24*(3.*(YP1-Y0)-(YP2-YM1))                         SINTX.26
C     TR6(YM2,YM1,Y0,YP1,YP2,YP3,A)=-A/60.*(-YM2+8.*YM1-37.*Y0                   SINTX.27
C    1                                     -37.*YP1+8.*YP2-YP3)                  SINTX.28
C    2-A**2/360.*(-2.*YM2+25.*YM1-245.*Y0+245.*YP1-25.*YP2+2.*YP3)               SINTX.29
C    3-A**3/48.*(YM2-7.*YM1+6.*Y0+6.*YP1-7.*YP2+YP3)                             SINTX.30
C    4-A**4/144.*(YM2-11.*YM1+28.*Y0-28.*YP1+11.*YP2-YP3)                        SINTX.31
C    5-A**5/240.*(-YM2+3.*YM1-2.*Y0-2.*YP1+3.*YP2-YP3)                           SINTX.32
C    6-A**6/720.*(-YM2+5.*YM1-10.*Y0+10.*YP1-5.*YP2+YP3)                         SINTX.33
      PP(X)=AMAX1(0.,X)                                                          SINTX.34
      PN(X)=AMIN1(0.,X)                                                          SINTX.35
C                                                                                SINTX.36
C      N2STAR=JXWANT                                                             SINTX.37
C      N2END=JXWANT                                                              SINTX.38
C                                                                                SINTX.39
      JJ=JXWANT                                                                  SINTX.40
      DO 5 I=1,9                                                                 SINTX.41
        ICOM(I)=1                                                                SINTX.42
    5 CONTINUE                                                                   SINTX.43
      IF((ICOMPS.EQ.1.AND.ICRSDOT.EQ.1).OR.(ICOMPS.EQ.2.AND.ICRSDOT.EQ.0         SINTX.44
     +  ))THEN                                                                   SINTX.45
        DO 6 I=7,9                                                               SINTX.46
          ICOM(I)=0                                                              SINTX.47
    6   CONTINUE                                                                 SINTX.48
      ELSEIF((ICOMPS.EQ.1.AND.ICRSDOT.EQ.0).OR.(ICOMPS.EQ.2.AND.ICRSDOT          SINTX.49
     +      .EQ.1))THEN                                                          SINTX.50
        DO 7 I=1,3                                                               SINTX.51
          ICOM(I)=0                                                              SINTX.52
    7   CONTINUE                                                                 SINTX.53
      ELSE                                                                       SINTX.54
        PRINT *,'STOP IN SINTX, ICOMPASS, ICRSDOT = ',                           SINTX.55
     1           ICOMPS,ICRSDOT                                                  SINTX.56
        STOP                                                                     SINTX.57
      ENDIF                                                                      SINTX.58
C                                                                                SINTX.59
      DO 2000 IFM=1,9                                                            SINTX.60
        IF(ICOM(IFM).EQ.0)GOTO 2000                                              SINTX.61
C                                                                                SINTX.62
C  HERE STARTS RESIDUAL ADVECTION                                                SINTX.63
C                                                                                SINTX.64
        DO 50 J=-IOR,IOR                                                         SINTX.65
          DO 51 I=-IOR,IOR                                                       SINTX.66
            DO 511 II=N1STAR,N1END                                               SINTX.67
  511       Y(II,I)=XF(IG0(II,JJ)+I,JG0(II,JJ)+J,IFM)                            SINTX.68
   51     CONTINUE                                                               SINTX.69
C                                                                                SINTX.70
          DO 811 II=N1STAR,N1END                                                 SINTX.71
            FL(II,0)=DONOR(Y(II,-1),Y(II,0),XIG(IFM))                            SINTX.72
  811     FL(II,1)=DONOR(Y(II,0),Y(II,1),XIG(IFM))                               SINTX.73
          DO 812 II=N1STAR,N1END                                                 SINTX.74
  812     W(II)=Y(II,0)-(FL(II,1)-FL(II,0))                                      SINTX.75
          DO 813 II=N1STAR,N1END                                                 SINTX.76
            MXM(II)=AMAX1(Y(II,-1),Y(II,0),Y(II,1),W(II))                        SINTX.77
  813     MN(II)=AMIN1(Y(II,-1),Y(II,0),Y(II,1),W(II))                           SINTX.78
C                                                                                SINTX.79
          DO 312 II=N1STAR,N1END                                                 SINTX.80
            F(II,0)=TR4(Y(II,-2),Y(II,-1),Y(II,0),Y(II,1),XIG(IFM))              SINTX.81
  312     F(II,1)=TR4(Y(II,-1),Y(II,0),Y(II,1),Y(II,2),XIG(IFM))                 SINTX.82
C                                                                                SINTX.83
          DO 822 II=N1STAR,N1END                                                 SINTX.84
            F(II,0)=F(II,0)-FL(II,0)                                             SINTX.85
  822     F(II,1)=F(II,1)-FL(II,1)                                               SINTX.86
          DO 823 II=N1STAR,N1END                                                 SINTX.87
            OV(II)=(MXM(II)-W(II))/(-PN(F(II,1))+PP(F(II,0))+EP)                 SINTX.88
  823     UN(II)=(W(II)-MN(II))/(PP(F(II,1))-PN(F(II,0))+EP)                     SINTX.89
          DO 824 II=N1STAR,N1END                                                 SINTX.90
            F(II,0)=PP(F(II,0))*AMIN1(1.,OV(II))+PN(F(II,0))*                    SINTX.91
     +              AMIN1(1.,UN(II))                                             SINTX.92
            F(II,1)=PP(F(II,1))*AMIN1(1.,UN(II))+PN(F(II,1))*                    SINTX.93
     +              AMIN1(1.,OV(II))                                             SINTX.94
  824     CONTINUE                                                               SINTX.95
          DO 825 II=N1STAR,N1END                                                 SINTX.96
  825     Y(II,0)=W(II)-(F(II,1)-F(II,0))                                        SINTX.97
C                                                                                SINTX.98
          DO 361 II=N1STAR,N1END                                                 SINTX.99
  361     Z(II,J)=Y(II,0)                                                        SINTX.100
C                                                                                SINTX.101
   50   CONTINUE                                                                 SINTX.102
C                                                                                SINTX.103
        DO 911 II=N1STAR,N1END                                                   SINTX.104
          FL(II,0)=DONOR(Z(II,-1),Z(II,0),XJG(IFM))                              SINTX.105
  911   FL(II,1)=DONOR(Z(II,0),Z(II,1),XJG(IFM))                                 SINTX.106
        DO 912 II=N1STAR,N1END                                                   SINTX.107
  912   W(II)=Z(II,0)-(FL(II,1)-FL(II,0))                                        SINTX.108
        DO 913 II=N1STAR,N1END                                                   SINTX.109
          MXM(II)=AMAX1(Z(II,-1),Z(II,0),Z(II,1),W(II))                          SINTX.110
  913   MN(II)=AMIN1(Z(II,-1),Z(II,0),Z(II,1),W(II))                             SINTX.111
C                                                                                SINTX.112
        DO 412 II=N1STAR,N1END                                                   SINTX.113
          F(II,0)=TR4(Z(II,-2),Z(II,-1),Z(II,0),Z(II,1),XJG(IFM))                SINTX.114
  412   F(II,1)=TR4(Z(II,-1),Z(II,0),Z(II,1),Z(II,2),XJG(IFM))                   SINTX.115
C                                                                                SINTX.116
        DO 922 II=N1STAR,N1END                                                   SINTX.117
          F(II,0)=F(II,0)-FL(II,0)                                               SINTX.118
  922   F(II,1)=F(II,1)-FL(II,1)                                                 SINTX.119
        DO 923 II=N1STAR,N1END                                                   SINTX.120
          OV(II)=(MXM(II)-W(II))/(-PN(F(II,1))+PP(F(II,0))+EP)                   SINTX.121
  923   UN(II)=(W(II)-MN(II))/(PP(F(II,1))-PN(F(II,0))+EP)                       SINTX.122
        DO 924 II=N1STAR,N1END                                                   SINTX.123
          F(II,0)=PP(F(II,0))*AMIN1(1.,OV(II))+PN(F(II,0))*                      SINTX.124
     +            AMIN1(1.,UN(II))                                               SINTX.125
          F(II,1)=PP(F(II,1))*AMIN1(1.,UN(II))+PN(F(II,1))*                      SINTX.126
     +            AMIN1(1.,OV(II))                                               SINTX.127
  924   CONTINUE                                                                 SINTX.128
        DO 925 II=N1STAR,N1END                                                   SINTX.129
  925   XF(II,JJ,IFM)=W(II)-(F(II,1)-F(II,0))                                    SINTX.130
C                                                                                SINTX.131
 2000 CONTINUE                                                                   SINTX.132
      RETURN                                                                     SINTX.133
      END                                                                        SINTX.134
                                                                                 SINTX.135
