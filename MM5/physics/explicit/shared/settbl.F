      SUBROUTINE SETTBL(IICE,TO,BVT,BVTS,PPI,PPIS,BETAI,XN0,                     SETTBL.1
     1            SVP1,SVP2,SVP3,SVPT0)                                          SETTBL.2
                                                                                 SETTBL.3
#     include <hdtabl.incl>                                                      SETTBL.4
#     include <jrg.incl>                                                         SETTBL.5
                                                                                 SETTBL.6
      REAL NOS,NOS25                                                             SETTBL.7
                                                                                 SETTBL.8
      DATA A1,B1,CC1,D1/0.611,22.514,-6150.0,0.0/                                SETTBL.9
      DATA AT1/8.794E-5/,BT1/1.81/                                               SETTBL.10
      DATA AT2/1.496E-6/,BT2/1.50/                                               SETTBL.11
                                                                                 SETTBL.12
      A2=SVP1                                                                    SETTBL.13
      B2=SVP2                                                                    SETTBL.14
      C2=SVP3-SVPT0                                                              SETTBL.15
      D2=-SVP3                                                                   SETTBL.16
      E2=C2*B2                                                                   SETTBL.17
C                                                                                SETTBL.18
C-----LOOK-UP TABLES FOR SATURATED VAPOR PRESSURE OVER ICE AND WATER.            SETTBL.19
C                                                                                SETTBL.20
C-----HDWES IS FOR WATER ONLY WITH TEMPERATURE FROM 100.0K TO 400.0K             SETTBL.21
C-----HDIES IS FOR BOTH WATER AND ICE, FOR ICE FROM 100.0K TO 273.1K,            SETTBL.22
C--------------------------------------FOR WATER FROM 273.2K TO 400.0K           SETTBL.23
C                                                                                SETTBL.24
      DO 100 I=10000,40000                                                       SETTBL.25
        T00=0.01*FLOAT(I)                                                        SETTBL.26
        TDD=T00+D2                                                               SETTBL.27
        HDWES(I)=A2*EXP(B2+E2/TDD)                                               SETTBL.28
        HDIES(I)=HDWES(I)                                                        SETTBL.29
  100 CONTINUE                                                                   SETTBL.30
      DO 110 I=10000,27315                                                       SETTBL.31
        T00=0.01*FLOAT(I)                                                        SETTBL.32
        TDD=T00+D1                                                               SETTBL.33
        HDIES(I)=A1*EXP(B1+CC1/TDD)                                              SETTBL.34
  110 CONTINUE                                                                   SETTBL.35
      DO 120 I=10000,27320                                                       SETTBL.36
        T00=0.01*FLOAT(I)                                                        SETTBL.37
        TERM=(BETAI*(TO-T00))                                                    SETTBL.38
        TERM=AMIN1(TERM,85.)                                                     SETTBL.39
C     XNCTBL(I)=XN0*EXP(BETAI*(TO-T00))                                          SETTBL.40
        XNCTBL(I)=XN0*EXP(TERM)                                                  SETTBL.41
  120 CONTINUE                                                                   SETTBL.42
C                                                                                SETTBL.43
C-----LOOK-UP TABLE FOR T**1.81 AND T**1.50                                      SETTBL.44
C                                                                                SETTBL.45
C     AT1=8.794E-5,         BT1=1.81                                             SETTBL.46
C     AT2=1.496E-6,         BT2=1.50                                             SETTBL.47
C                                                                                SETTBL.48
      DO 200 I=1000,4000                                                         SETTBL.49
        T00=0.1*FLOAT(I)                                                         SETTBL.50
        HDT181(I)=AT1*T00**BT1                                                   SETTBL.51
        HDT150(I)=AT2*T00**BT2                                                   SETTBL.52
  200 CONTINUE                                                                   SETTBL.53
C-----LOOK-UP TABLES FOR LAMDA, LAMDA**BVT2, LAMDA**BVT3,                        SETTBL.54
C-----------------------------LAMDA**BVTS2, LAMDA**BVTS3.                        SETTBL.55
C                                                                                SETTBL.56
C ---- HERE, RLAMDA REFERS TO RHO*Q.                                             SETTBL.57
C ---- USAULY,  WE HAVE 0.0 < RHO*Q < 0.1                                        SETTBL.58
C                                                                                SETTBL.59
      RLAMDA(0)=(1.0E-25)**0.25                                                  SETTBL.60
C     RLAMDA(0)=1.0E-9                                                           SETTBL.61
      X=1./SCALE                                                                 SETTBL.62
      DO 10 I=1,200000                                                           SETTBL.63
        RLAMDA(I)=(X*FLOAT(I))**0.25                                             SETTBL.64
   10 CONTINUE                                                                   SETTBL.65
      PPICAL=(PPI/1000.0)**0.25                                                  SETTBL.66
      PPISCL=(PPIS/1000.0)**0.25                                                 SETTBL.67
C                                                                                SETTBL.68
      BVT2=2.5+0.5*BVT                                                           SETTBL.69
      BVT3=3.0+BVT                                                               SETTBL.70
      BVTS2=2.5+0.5*BVTS                                                         SETTBL.71
      BVTS3=3.0+BVTS                                                             SETTBL.72
C                                                                                SETTBL.73
C ---- BELOW, XL OR FL REFERS TO SCR7.                                           SETTBL.74
C                                                                                SETTBL.75
C ---- FL = ( RHO*Q*PPI /1000.0)**0.25, FOR T > 273.15                           SETTBL.76
C ---- FL = ( RHO*Q*PPIS/1000.0)**0.25, FOR T < 273.15                           SETTBL.77
C                                                                                SETTBL.78
C----- USUALY,   0  1.E-4< FL < 1.E-2                                            SETTBL.79
C                                                                                SETTBL.80
C     XL=1.0E-9                                                                  SETTBL.81
C     PBVT(0)=XL**BVT                                                            SETTBL.82
C     PBVTS(0)=XL**BVTS                                                          SETTBL.83
C     PBVT2(0)=XL**BVT2                                                          SETTBL.84
C     PBVT3(0)=XL**BVT3                                                          SETTBL.85
C     PBVTS2(0)=XL**BVTS2                                                        SETTBL.86
C     PBVTS3(0)=XL**BVTS3                                                        SETTBL.87
      PBVT(0)=0.                                                                 SETTBL.88
      PBVTS(0)=0.                                                                SETTBL.89
      PBVT2(0)=0.                                                                SETTBL.90
      PBVT3(0)=0.                                                                SETTBL.91
      PBVTS2(0)=0.                                                               SETTBL.92
      PBVTS3(0)=0.                                                               SETTBL.93
      DO 20 I=1,100000                                                           SETTBL.94
        FL=X*FLOAT(I)                                                            SETTBL.95
        PBVT(I)=FL**BVT                                                          SETTBL.96
        PBVTS(I)=FL**BVTS                                                        SETTBL.97
        PBVT2(I)=FL**BVT2                                                        SETTBL.98
        PBVT3(I)=FL**BVT3                                                        SETTBL.99
        PBVTS2(I)=FL**BVTS2                                                      SETTBL.100
        PBVTS3(I)=FL**BVTS3                                                      SETTBL.101
   20 CONTINUE                                                                   SETTBL.102
C**** LOOK-UP TABLE FOR PRE**.5 AND PRC**.3333                                   SETTBL.103
      RPRC(0)=(1.0E-25)**0.3333                                                  SETTBL.104
      RPRE(0)=(1.0E-25)**0.5                                                     SETTBL.105
      DO 30 I=1,20000                                                            SETTBL.106
        RPRE(I)=(1.E-6*FLOAT(I))**0.5                                            SETTBL.107
   30 CONTINUE                                                                   SETTBL.108
      DO 40 I=1,20000                                                            SETTBL.109
        RPRC(I)=(1.E-2*FLOAT(I))**0.3333                                         SETTBL.110
   40 CONTINUE                                                                   SETTBL.111
C                                                                                SETTBL.112
C**** LOOK-UP TABLE FOR XMI*0.5                                                  SETTBL.113
C                                                                                SETTBL.114
      RXMI(0)=(1.0E-25)**0.5                                                     SETTBL.115
      DO I=1,100000                                                              SETTBL.116
        RXMI(I)=(1.0E-12*FLOAT(I))**0.5                                          SETTBL.117
      ENDDO                                                                      SETTBL.118
      IF(IICE.EQ.1)THEN                                                          SETTBL.119
C**** LOOK-UP TABLE FOR FACTOR OF SLOP INTERCEPT FOR SNOW                        SETTBL.120
C                                                                                SETTBL.121
        NOS(0)=1.                                                                SETTBL.122
        DO I=1,200000                                                            SETTBL.123
          NOS(I)=(CONST1A*(DRAIN/(CONST1B*1.E-7*FLOAT(I))*(PI*DSNOW/(            SETTBL.124
     +           1.E-7*FLOAT(I)))**(BS/4))**0.94)**(4./(0.94*BS+4))/SON          SETTBL.125
          IF(NOS(I).GT.1.)NOS(I)=1.                                              SETTBL.126
        ENDDO                                                                    SETTBL.127
        NOS25(0)=1.E+3                                                           SETTBL.128
        DO I=1,100001                                                            SETTBL.129
          NOS25(I)=1/((1.E-5*FLOAT(I))**0.25)                                    SETTBL.130
        ENDDO                                                                    SETTBL.131
C                                                                                SETTBL.132
C**** LOOK-UP TABLE FOR HETEOROGENOUS FREEZING OF CLOUD DROPLETS                 SETTBL.133
C                                                                                SETTBL.134
        DO I=10000,27320                                                         SETTBL.135
          T00=0.01*FLOAT(I)                                                      SETTBL.136
          TERM=AP*(TO-T00)                                                       SETTBL.137
          TERM=AMIN1(TERM,80.)                                                   SETTBL.138
          HETF(I)=BP*(EXP(TERM)-1.)                                              SETTBL.139
        ENDDO                                                                    SETTBL.140
      ENDIF                                                                      SETTBL.141
      RETURN                                                                     SETTBL.142
      END                                                                        SETTBL.143
                                                                                 SETTBL.144
