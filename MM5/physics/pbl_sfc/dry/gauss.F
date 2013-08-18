      SUBROUTINE GAUSS(AX,B,C,D,Y,LM,MKX)                                        GAUSS.1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          GAUSS.2
C                                                                     C          GAUSS.3
C                                                                     C          GAUSS.4
C     THIS SUBROUTINE IS A PART OF THE DRY CONVECTIVE ADJUSTMENT AND  C          GAUSS.5
C     BY SUBROUTINE "CADJMX".                                         C          GAUSS.6
C                                                                     C          GAUSS.7
C                                                                     C          GAUSS.8
C     AX : IS THE MAIN DIAGONAL ARRAY.                                C          GAUSS.9
C                                                                     C          GAUSS.10
C     B  : IS THE UPPER OFF-DIAGONAL COEFFICIENT ARRAY.               C          GAUSS.11
C                                                                     C          GAUSS.12
C     C  : IS THE BOTTOM ROW COEFFICIENT ARRAY.                       C          GAUSS.13
C                                                                     C          GAUSS.14
C     D  : IS A WORKING ARRAY.                                        C          GAUSS.15
C                                                                     C          GAUSS.16
C     Y  : IS THE RHS ON INPUT, AND SOLUTION ON OUTPUT.               C          GAUSS.17
C                                                                     C          GAUSS.18
C     LM : IS THE ORDER OF THE MATRIX.                                C          GAUSS.19
C                                                                     C          GAUSS.20
C     MKX: IS THE DIMENSION FOR ALL THE ARRAYS.                       C          GAUSS.21
C                                                                     C          GAUSS.22
C                                                                     C          GAUSS.23
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          GAUSS.24
C                                                                                GAUSS.25
      DIMENSION AX(MKX),B(MKX),C(MKX),D(MKX),Y(MKX)                              GAUSS.26
C                                                                                GAUSS.27
      LMM1=LM-1                                                                  GAUSS.28
      DO 10 K=1,LMM1                                                             GAUSS.29
        D(K)=B(K)/AX(K)                                                          GAUSS.30
        Y(K)=Y(K)/AX(K)                                                          GAUSS.31
   10 CONTINUE                                                                   GAUSS.32
      F=C(1)                                                                     GAUSS.33
      DO 20 K=1,LMM1                                                             GAUSS.34
        Y(LM)=Y(LM)-F*Y(K)                                                       GAUSS.35
        F=C(K+1)-F*D(K)                                                          GAUSS.36
   20 CONTINUE                                                                   GAUSS.37
      Y(LM)=Y(LM)/F                                                              GAUSS.38
      DO 30 KK=1,LMM1                                                            GAUSS.39
        K=LM-KK                                                                  GAUSS.40
        Y(K)=Y(K)-D(K)*Y(K+1)                                                    GAUSS.41
   30 CONTINUE                                                                   GAUSS.42
C                                                                                GAUSS.43
      RETURN                                                                     GAUSS.44
      END                                                                        GAUSS.45
                                                                                 GAUSS.46
