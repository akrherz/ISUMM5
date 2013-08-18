      SUBROUTINE CADJMX(KB,KE,PI,S,NADJ,DSIGMA,DTHDPC,P,AX,B,C,D,Y,KL,           CADJMX.1
     1                  MKX)                                                     CADJMX.2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          CADJMX.3
C                                                                     C          CADJMX.4
C                                                                     C          CADJMX.5
C     THIS SUBROUTINE IS A PART OF THE DRY CONVECTIVE ADJUSTMENT AND  C          CADJMX.6
C     IS CALLED BY SUBROUTINE "CONVAD".                               C          CADJMX.7
C                                                                     C          CADJMX.8
C                                                                     C          CADJMX.9
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          CADJMX.10
C                                                                                CADJMX.11
      DIMENSION AX(MKX),B(MKX),C(MKX),D(MKX),Y(MKX)                              CADJMX.12
      DIMENSION DSIGMA(KL),P(MKX),PI(MKX),S(MKX)                                 CADJMX.13
C                                                                                CADJMX.14
      NADJ=KE-KB+1                                                               CADJMX.15
      NADJM1=NADJ-1                                                              CADJMX.16
C                                                                                CADJMX.17
C----FILL COEFFICIENT VECTORS:                                                   CADJMX.18
C                                                                                CADJMX.19
      DO 10 N=1,NADJM1                                                           CADJMX.20
        K=KE-NADJ+N                                                              CADJMX.21
C.....K RANGES FROM KB TO KE-1.                                                  CADJMX.22
        AX(N)=-PI(K)                                                             CADJMX.23
        B(N)=PI(K+1)                                                             CADJMX.24
   10 CONTINUE                                                                   CADJMX.25
C                                                                                CADJMX.26
C-----LAST ROW OF MATRIX:                                                        CADJMX.27
C                                                                                CADJMX.28
      DO 20 N=1,NADJ                                                             CADJMX.29
        K=KE-NADJ+N                                                              CADJMX.30
        C(N)=DSIGMA(K)                                                           CADJMX.31
   20 CONTINUE                                                                   CADJMX.32
C                                                                                CADJMX.33
C-----COMPUTE ELEMENTS OF Y COLUMN VECTOR:                                       CADJMX.34
C                                                                                CADJMX.35
      DO 30 N=1,NADJM1                                                           CADJMX.36
        K=KE-NADJ+N                                                              CADJMX.37
        Y(N)=DTHDPC*(P(K+1)-P(K))-PI(K+1)*S(K+1)+PI(K)*S(K)                      CADJMX.38
   30 CONTINUE                                                                   CADJMX.39
      Y(NADJ)=0.                                                                 CADJMX.40
C                                                                                CADJMX.41
      CALL GAUSS(AX,B,C,D,Y,NADJ,MKX)                                            CADJMX.42
C                                                                                CADJMX.43
      RETURN                                                                     CADJMX.44
      END                                                                        CADJMX.45
                                                                                 CADJMX.46
