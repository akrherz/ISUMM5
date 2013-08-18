CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         PROF5.1
C  THIS SUBROUTINE INTEGRATES THE AREA UNDER THE CURVE IN THE GAUSSIAN           PROF5.2
C  DISTRIBUTION...THE NUMERICAL APPROXIMATION TO THE INTEGRAL IS TAKEN F         PROF5.3
C   HANDBOOK OF MATHEMATICAL FUNCTIONS WITH FORMULAS, GRAPHS AND MATHEMA         PROF5.4
C  TABLES  ED. BY ABRAMOWITZ AND STEGUN, NAT L BUREAU OF STANDARDS APPLI         PROF5.5
C  MATHEMATICS SERIES.  JUNE, 1964., MAY, 1968.                                  PROF5.6
C                                     JACK KAIN                                  PROF5.7
C                                     7/6/89                                     PROF5.8
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         PROF5.9
C***********************************************************************         PROF5.10
C*****    GAUSSIAN TYPE MIXING PROFILE....******************************         PROF5.11
      SUBROUTINE PROF5(EQ,EE,UD)                                                 PROF5.12
      DATA SQRT2P,A1,A2,A3,P,SIGMA,FE/2.506628,0.4361836,-0.1201676,             PROF5.13
     *0.9372980,0.33267,0.166666667,0.202765151/                                 PROF5.14
      X=(EQ-0.5)/SIGMA                                                           PROF5.15
      Y=6.*EQ-3.                                                                 PROF5.16
      EY=EXP(Y*Y/(-2))                                                           PROF5.17
      E45=EXP(-4.5)                                                              PROF5.18
      T2=1./(1.+P*ABS(Y))                                                        PROF5.19
      T1=0.500498                                                                PROF5.20
      C1=A1*T1+A2*T1*T1+A3*T1*T1*T1                                              PROF5.21
      C2=A1*T2+A2*T2*T2+A3*T2*T2*T2                                              PROF5.22
      IF(Y.GE.0.)THEN                                                            PROF5.23
        EE=SIGMA*(0.5*(SQRT2P-E45*C1-EY*C2)+SIGMA*(E45-EY))-E45*EQ*EQ/2.         PROF5.24
        UD=SIGMA*(0.5*(EY*C2-E45*C1)+SIGMA*(E45-EY))-E45*(0.5+EQ*EQ/2.-          PROF5.25
     +     EQ)                                                                   PROF5.26
      ELSE                                                                       PROF5.27
        EE=SIGMA*(0.5*(EY*C2-E45*C1)+SIGMA*(E45-EY))-E45*EQ*EQ/2.                PROF5.28
        UD=SIGMA*(0.5*(SQRT2P-E45*C1-EY*C2)+SIGMA*(E45-EY))-E45*(0.5+EQ*         PROF5.29
     +     EQ/2.-EQ)                                                             PROF5.30
      ENDIF                                                                      PROF5.31
      EE=EE/FE                                                                   PROF5.32
      UD=UD/FE                                                                   PROF5.33
      RETURN                                                                     PROF5.34
      END                                                                        PROF5.35
                                                                                 PROF5.36
