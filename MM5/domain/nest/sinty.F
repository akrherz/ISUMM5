      SUBROUTINE SINTY(XF,N,M,IYWANT,N2STAR,N2END,                               SINTY.1
     1          ICOMPS,ICRSDOT)                                                  SINTY.2
#     include <parame.incl>                                                      SINTY.3
      PARAMETER(N1=MJX,N2=MIX,NF=9,IFCT=1,IOR=2)                                 SINTY.4
      PARAMETER(ONE12=1./12.,ONE24=1./24.)                                       SINTY.5
C                                                                                SINTY.6
      DIMENSION XF(N,M,9)                                                        SINTY.7
      DIMENSION Y(N2,-IOR:IOR),Z(N2,-IOR:IOR),F(N2,0:1)                          SINTY.8
      DATA  EP/ 1.E-10/                                                          SINTY.9
      PARAMETER(NONOS=1)                                                         SINTY.10
      PARAMETER(N1OS=N1*NONOS+1-NONOS,N2OS=N2*NONOS+1-NONOS)                     SINTY.11
      DIMENSION  W(N2OS),FL(N2OS,0:1),                                           SINTY.12
     1          OV(N2OS),UN(N2OS)                                                SINTY.13
      REAL      MXM(N2OS),MN(N2OS)                                               SINTY.14
      COMMON /DEPAR2/ XIG(9),XJG(9),IG0(N1,N2),JG0(N1,N2)                        SINTY.15
      INTEGER ICOM(9)                                                            SINTY.16
                                                                                 SINTY.17
      DONOR(Y1,Y2,A)=(Y1*AMAX1(0.,SIGN(1.,A))-Y2*AMIN1(0.,SIGN(1.,A)))*A         SINTY.18
C     TR2(Y1,Y2,A)=A*.5*(Y1+Y2)-A**2*.5*(Y2-Y1)                                  SINTY.19
C      TR4(YM1,Y0,YP1,YP2,A)=A/12.*(7.*(YP1+Y0)-(YP2+YM1))                       SINTY.20
C     1 -A**2/24.*(15.*(YP1-Y0)-(YP2-YM1))-A**3/12.*((YP1+Y0)                    SINTY.21
C     2 -(YP2+YM1))+A**4/24.*(3.*(YP1-Y0)-(YP2-YM1))                             SINTY.22
      TR4(YM1,Y0,YP1,YP2,A)=A*ONE12*(7.*(YP1+Y0)-(YP2+YM1))                      SINTY.23
     1 -A*A*ONE24*(15.*(YP1-Y0)-(YP2-YM1))-A*A*A*ONE12*((YP1+Y0)                 SINTY.24
     2 -(YP2+YM1))+A*A*A*A*ONE24*(3.*(YP1-Y0)-(YP2-YM1))                         SINTY.25
C     TR6(YM2,YM1,Y0,YP1,YP2,YP3,A)=-A/60.*(-YM2+8.*YM1-37.*Y0                   SINTY.26
C    1                                     -37.*YP1+8.*YP2-YP3)                  SINTY.27
C    2-A**2/360.*(-2.*YM2+25.*YM1-245.*Y0+245.*YP1-25.*YP2+2.*YP3)               SINTY.28
C    3-A**3/48.*(YM2-7.*YM1+6.*Y0+6.*YP1-7.*YP2+YP3)                             SINTY.29
C    4-A**4/144.*(YM2-11.*YM1+28.*Y0-28.*YP1+11.*YP2-YP3)                        SINTY.30
C    5-A**5/240.*(-YM2+3.*YM1-2.*Y0-2.*YP1+3.*YP2-YP3)                           SINTY.31
C    6-A**6/720.*(-YM2+5.*YM1-10.*Y0+10.*YP1-5.*YP2+YP3)                         SINTY.32
      PP(X)=AMAX1(0.,X)                                                          SINTY.33
      PN(X)=AMIN1(0.,X)                                                          SINTY.34
C                                                                                SINTY.35
C      N1STAR=IYWANT                                                             SINTY.36
C      N1END=IYWANT                                                              SINTY.37
C                                                                                SINTY.38
      II=IYWANT                                                                  SINTY.39
C                                                                                SINTY.40
      DO 5 I=1,9                                                                 SINTY.41
        ICOM(I)=9                                                                SINTY.42
    5 CONTINUE                                                                   SINTY.43
      IF((ICOMPS.EQ.1.AND.ICRSDOT.EQ.1).OR.(ICOMPS.EQ.2.AND.ICRSDOT.EQ.0         SINTY.44
     +  ))THEN                                                                   SINTY.45
        DO 6 I=1,3                                                               SINTY.46
          ICOM(3*I)=0                                                            SINTY.47
    6   CONTINUE                                                                 SINTY.48
      ELSEIF((ICOMPS.EQ.1.AND.ICRSDOT.EQ.0).OR.(ICOMPS.EQ.2.AND.ICRSDOT          SINTY.49
     +      .EQ.1))THEN                                                          SINTY.50
        DO 7 I=1,3                                                               SINTY.51
C                                                                                SINTY.52
          ICOM(3*I-2)=0                                                          SINTY.53
    7   CONTINUE                                                                 SINTY.54
      ELSE                                                                       SINTY.55
        PRINT *,'STOP IN SINTY, ICOMPASS, ICRSDOT = ',                           SINTY.56
     1           ICOMPS,ICRSDOT                                                  SINTY.57
        STOP                                                                     SINTY.58
      ENDIF                                                                      SINTY.59
C                                                                                SINTY.60
      DO 2000 IFM=1,9                                                            SINTY.61
        IF(ICOM(IFM).EQ.0)GOTO 2000                                              SINTY.62
C                                                                                SINTY.63
C  HERE STARTS RESIDUAL ADVECTION                                                SINTY.64
C                                                                                SINTY.65
        DO 50 J=-IOR,IOR                                                         SINTY.66
          DO 51 I=-IOR,IOR                                                       SINTY.67
            DO 511 JJ=N2STAR,N2END                                               SINTY.68
  511       Y(JJ,I)=XF(IG0(II,JJ)+I,JG0(II,JJ)+J,IFM)                            SINTY.69
   51     CONTINUE                                                               SINTY.70
C                                                                                SINTY.71
          DO 811 JJ=N2STAR,N2END                                                 SINTY.72
            FL(JJ,0)=DONOR(Y(JJ,-1),Y(JJ,0),XIG(IFM))                            SINTY.73
  811     FL(JJ,1)=DONOR(Y(JJ,0),Y(JJ,1),XIG(IFM))                               SINTY.74
          DO 812 JJ=N2STAR,N2END                                                 SINTY.75
  812     W(JJ)=Y(JJ,0)-(FL(JJ,1)-FL(JJ,0))                                      SINTY.76
          DO 813 JJ=N2STAR,N2END                                                 SINTY.77
            MXM(JJ)=AMAX1(Y(JJ,-1),Y(JJ,0),Y(JJ,1),W(JJ))                        SINTY.78
  813     MN(JJ)=AMIN1(Y(JJ,-1),Y(JJ,0),Y(JJ,1),W(JJ))                           SINTY.79
          DO 312 JJ=N2STAR,N2END                                                 SINTY.80
            F(JJ,0)=TR4(Y(JJ,-2),Y(JJ,-1),Y(JJ,0),Y(JJ,1),XIG(IFM))              SINTY.81
  312     F(JJ,1)=TR4(Y(JJ,-1),Y(JJ,0),Y(JJ,1),Y(JJ,2),XIG(IFM))                 SINTY.82
C                                                                                SINTY.83
          DO 822 JJ=N2STAR,N2END                                                 SINTY.84
            F(JJ,0)=F(JJ,0)-FL(JJ,0)                                             SINTY.85
  822     F(JJ,1)=F(JJ,1)-FL(JJ,1)                                               SINTY.86
          DO 823 JJ=N2STAR,N2END                                                 SINTY.87
            OV(JJ)=(MXM(JJ)-W(JJ))/(-PN(F(JJ,1))+PP(F(JJ,0))+EP)                 SINTY.88
  823     UN(JJ)=(W(JJ)-MN(JJ))/(PP(F(JJ,1))-PN(F(JJ,0))+EP)                     SINTY.89
          DO 824 JJ=N2STAR,N2END                                                 SINTY.90
            F(JJ,0)=PP(F(JJ,0))*AMIN1(1.,OV(JJ))+PN(F(JJ,0))*                    SINTY.91
     +              AMIN1(1.,UN(JJ))                                             SINTY.92
            F(JJ,1)=PP(F(JJ,1))*AMIN1(1.,UN(JJ))+PN(F(JJ,1))*                    SINTY.93
     +              AMIN1(1.,OV(JJ))                                             SINTY.94
  824     CONTINUE                                                               SINTY.95
          DO 825 JJ=N2STAR,N2END                                                 SINTY.96
  825     Y(JJ,0)=W(JJ)-(F(JJ,1)-F(JJ,0))                                        SINTY.97
C                                                                                SINTY.98
          DO 361 JJ=N2STAR,N2END                                                 SINTY.99
  361     Z(JJ,J)=Y(JJ,0)                                                        SINTY.100
C                                                                                SINTY.101
   50   CONTINUE                                                                 SINTY.102
C                                                                                SINTY.103
        DO 911 JJ=N2STAR,N2END                                                   SINTY.104
          FL(JJ,0)=DONOR(Z(JJ,-1),Z(JJ,0),XJG(IFM))                              SINTY.105
  911   FL(JJ,1)=DONOR(Z(JJ,0),Z(JJ,1),XJG(IFM))                                 SINTY.106
        DO 912 JJ=N2STAR,N2END                                                   SINTY.107
  912   W(JJ)=Z(JJ,0)-(FL(JJ,1)-FL(JJ,0))                                        SINTY.108
        DO 913 JJ=N2STAR,N2END                                                   SINTY.109
          MXM(JJ)=AMAX1(Z(JJ,-1),Z(JJ,0),Z(JJ,1),W(JJ))                          SINTY.110
  913   MN(JJ)=AMIN1(Z(JJ,-1),Z(JJ,0),Z(JJ,1),W(JJ))                             SINTY.111
        DO 412 JJ=N2STAR,N2END                                                   SINTY.112
          F(JJ,0)=TR4(Z(JJ,-2),Z(JJ,-1),Z(JJ,0),Z(JJ,1),XJG(IFM))                SINTY.113
  412   F(JJ,1)=TR4(Z(JJ,-1),Z(JJ,0),Z(JJ,1),Z(JJ,2),XJG(IFM))                   SINTY.114
C                                                                                SINTY.115
        DO 922 JJ=N2STAR,N2END                                                   SINTY.116
          F(JJ,0)=F(JJ,0)-FL(JJ,0)                                               SINTY.117
  922   F(JJ,1)=F(JJ,1)-FL(JJ,1)                                                 SINTY.118
        DO 923 JJ=N2STAR,N2END                                                   SINTY.119
          OV(JJ)=(MXM(JJ)-W(JJ))/(-PN(F(JJ,1))+PP(F(JJ,0))+EP)                   SINTY.120
  923   UN(JJ)=(W(JJ)-MN(JJ))/(PP(F(JJ,1))-PN(F(JJ,0))+EP)                       SINTY.121
        DO 924 JJ=N2STAR,N2END                                                   SINTY.122
          F(JJ,0)=PP(F(JJ,0))*AMIN1(1.,OV(JJ))+PN(F(JJ,0))*                      SINTY.123
     +            AMIN1(1.,UN(JJ))                                               SINTY.124
          F(JJ,1)=PP(F(JJ,1))*AMIN1(1.,UN(JJ))+PN(F(JJ,1))*                      SINTY.125
     +            AMIN1(1.,OV(JJ))                                               SINTY.126
  924   CONTINUE                                                                 SINTY.127
        DO 925 JJ=N2STAR,N2END                                                   SINTY.128
  925   XF(II,JJ,IFM)=W(JJ)-(F(JJ,1)-F(JJ,0))                                    SINTY.129
C                                                                                SINTY.130
 2000 CONTINUE                                                                   SINTY.131
      RETURN                                                                     SINTY.132
      END                                                                        SINTY.133
                                                                                 SINTY.134
