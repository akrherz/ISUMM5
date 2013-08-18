C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&           SPLINE.1
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&           SPLINE.2
      SUBROUTINE SPLINE(NOLD,XOLD,YOLD,Y2,NNEW,XNEW,YNEW,P,Q)                    SPLINE.3
C     ******************************************************************         SPLINE.4
C     *                                                                *         SPLINE.5
C     *  THIS IS A ONE-DIMENSIONAL CUBIC SPLINE FITTING ROUTINE        *         SPLINE.6
C     *  PROGRAMED FOR A SMALL SCALAR MACHINE.                         *         SPLINE.7
C     *                                                                *         SPLINE.8
C     *  PROGRAMER\ Z. JANJIC, YUGOSLAV FED. HYDROMET. INST., BEOGRAD  *         SPLINE.9
C     *                                                                *         SPLINE.10
C     *                                                                *         SPLINE.11
C     *                                                                *         SPLINE.12
C     *  NOLD - NUMBER OF GIVEN VALUES OF THE FUNCTION.  MUST BE GE 3. *         SPLINE.13
C     *  XOLD - LOCATIONS OF THE POINTS AT WHICH THE VALUES OF THE     *         SPLINE.14
C     *         FUNCTION ARE GIVEN.  MUST BE IN ASCENDING ORDER.       *         SPLINE.15
C     *  YOLD - THE GIVEN VALUES OF THE FUNCTION AT THE POINTS XOLD.   *         SPLINE.16
C     *  Y2   - THE SECOND DERIVATIVES AT THE POINTS XOLD.  IF NATURAL *         SPLINE.17
C     *         SPLINE IS FITTED Y2(1)=0. AND Y2(NOLD)=0. MUST BE      *         SPLINE.18
C     *         SPECIFIED.                                             *         SPLINE.19
C     *  NNEW - NUMBER OF VALUES OF THE FUNCTION TO BE CALCULATED.     *         SPLINE.20
C     *  XNEW - LOCATIONS OF THE POINTS AT WHICH THE VALUES OF THE     *         SPLINE.21
C     *         FUNCTION ARE CALCULATED.  XNEW(K) MUST BE GE XOLD(1)   *         SPLINE.22
C     *         AND LE XOLD(NOLD).                                     *         SPLINE.23
C     *  YNEW - THE VALUES OF THE FUNCTION TO BE CALCULATED.           *         SPLINE.24
C     *  P, Q - AUXILIARY VECTORS OF THE LENGTH NOLD-2.                *         SPLINE.25
C     *                                                                *         SPLINE.26
C     ******************************************************************         SPLINE.27
                             P A R A M E T E R                                   SPLINE.28
     & (JTB=150)                                                                 SPLINE.29
                             D I M E N S I O N                                   SPLINE.30
     & XOLD(JTB),YOLD(JTB),Y2(JTB),P(JTB),Q(JTB)                                 SPLINE.31
     &,XNEW(JTB),YNEW(JTB)                                                       SPLINE.32
C-----------------------------------------------------------------------         SPLINE.33
      NOLDM1=NOLD-1                                                              SPLINE.34
C                                                                                SPLINE.35
      DXL=XOLD(2)-XOLD(1)                                                        SPLINE.36
      DXR=XOLD(3)-XOLD(2)                                                        SPLINE.37
      DYDXL=(YOLD(2)-YOLD(1))/DXL                                                SPLINE.38
      DYDXR=(YOLD(3)-YOLD(2))/DXR                                                SPLINE.39
      RTDXC=.5/(DXL+DXR)                                                         SPLINE.40
C                                                                                SPLINE.41
      P(1)=RTDXC*(6.*(DYDXR-DYDXL)-DXL*Y2(1))                                    SPLINE.42
      Q(1)=-RTDXC*DXR                                                            SPLINE.43
C                                                                                SPLINE.44
      IF(NOLD.EQ.3)GOTO 700                                                      SPLINE.45
C-----------------------------------------------------------------------         SPLINE.46
      K=3                                                                        SPLINE.47
C                                                                                SPLINE.48
  100 DXL=DXR                                                                    SPLINE.49
      DYDXL=DYDXR                                                                SPLINE.50
      DXR=XOLD(K+1)-XOLD(K)                                                      SPLINE.51
      DYDXR=(YOLD(K+1)-YOLD(K))/DXR                                              SPLINE.52
      DXC=DXL+DXR                                                                SPLINE.53
      DEN=1./(DXL*Q(K-2)+DXC+DXC)                                                SPLINE.54
C                                                                                SPLINE.55
      P(K-1)=DEN*(6.*(DYDXR-DYDXL)-DXL*P(K-2))                                   SPLINE.56
      Q(K-1)=-DEN*DXR                                                            SPLINE.57
C                                                                                SPLINE.58
      K=K+1                                                                      SPLINE.59
      IF(K.LT.NOLD)GOTO 100                                                      SPLINE.60
C-----------------------------------------------------------------------         SPLINE.61
  700 K=NOLDM1                                                                   SPLINE.62
C                                                                                SPLINE.63
  200 Y2(K)=P(K-1)+Q(K-1)*Y2(K+1)                                                SPLINE.64
C                                                                                SPLINE.65
      K=K-1                                                                      SPLINE.66
      IF(K.GT.1)GOTO 200                                                         SPLINE.67
C-----------------------------------------------------------------------         SPLINE.68
      K1=1                                                                       SPLINE.69
C                                                                                SPLINE.70
  300 XK=XNEW(K1)                                                                SPLINE.71
C                                                                                SPLINE.72
      DO 400 K2=2,NOLD                                                           SPLINE.73
        IF(XOLD(K2).LE.XK)GOTO 400                                               SPLINE.74
        KOLD=K2-1                                                                SPLINE.75
        GOTO 450                                                                 SPLINE.76
  400 CONTINUE                                                                   SPLINE.77
      YNEW(K1)=YOLD(NOLD)                                                        SPLINE.78
      GOTO 600                                                                   SPLINE.79
C                                                                                SPLINE.80
  450 IF(K1.EQ.1)GOTO 500                                                        SPLINE.81
      IF(K.EQ.KOLD)GOTO 550                                                      SPLINE.82
C                                                                                SPLINE.83
  500 K=KOLD                                                                     SPLINE.84
C                                                                                SPLINE.85
      Y2K=Y2(K)                                                                  SPLINE.86
      Y2KP1=Y2(K+1)                                                              SPLINE.87
      DX=XOLD(K+1)-XOLD(K)                                                       SPLINE.88
      RDX=1./DX                                                                  SPLINE.89
C                                                                                SPLINE.90
CVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV                   SPLINE.91
C     WRITE(6,5000) K,Y2K,Y2KP1,DX,RDX,YOLD(K),YOLD(K+1)                         SPLINE.92
C5000 FORMAT(' K=',I4,' Y2K=',E12.4,' Y2KP1=',E12.4,' DX=',E12.4,' RDX='         SPLINE.93
C    2,E12.4,' YOK=',E12.4,' YOP1=',E12.4)                                       SPLINE.94
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA                   SPLINE.95
      AK=.1666667*RDX*(Y2KP1-Y2K)                                                SPLINE.96
      BK=.5*Y2K                                                                  SPLINE.97
      CK=RDX*(YOLD(K+1)-YOLD(K))-.1666667*DX*(Y2KP1+Y2K+Y2K)                     SPLINE.98
C                                                                                SPLINE.99
  550 X=XK-XOLD(K)                                                               SPLINE.100
      XSQ=X*X                                                                    SPLINE.101
C                                                                                SPLINE.102
      YNEW(K1)=AK*XSQ*X+BK*XSQ+CK*X+YOLD(K)                                      SPLINE.103
C                                                                                SPLINE.104
  600 K1=K1+1                                                                    SPLINE.105
      IF(K1.LE.NNEW)GOTO 300                                                     SPLINE.106
C-----------------------------------------------------------------------         SPLINE.107
      RETURN                                                                     SPLINE.108
      END                                                                        SPLINE.109
                                                                                 SPLINE.110
