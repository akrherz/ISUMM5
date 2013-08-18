      SUBROUTINE MINIMI(ARRAY,MIX,MJX,MKX,KS,KEND,KT,IST,IEN)                    07NOV00.1371
      IMPLICIT NONE                                                              07NOV00.1372
C MJX is needed for vector version                                               07NOV00.1373
      INTEGER   MIX,MJX,MKX                                                      07NOV00.1374
      REAL      ARRAY(MIX,MKX)                                                   07NOV00.1375
      INTEGER KT(MIX),KS(MIX),KEND(MIX)                                          MINIMI.3
      INTEGER I,K,IST,IEN                                                        07NOV00.1376
      REAL    X                                                                  07NOV00.1377
      DO 200 I=IST,IEN                                                           07NOV00.1378
        KT(I)=KS(I)                                                              MINIMI.5
        X=ARRAY(I,KS(I))                                                         MINIMI.6
C                                                                                MINIMI.7
        DO 100 K=KS(I)+1,KEND(I)                                                 MINIMI.8
          IF(ARRAY(I,K).LT.X)THEN                                                MINIMI.9
            X=ARRAY(I,K)                                                         MINIMI.10
            KT(I)=K                                                              MINIMI.11
          ENDIF                                                                  MINIMI.12
  100   CONTINUE                                                                 MINIMI.13
        IF(KT(I).EQ.MKX)THEN                                                     MINIMI.14
          PRINT *,'MINIMI WARNING: DOWNDRAFT STARTS AT MODEL TOP'                MINIMI.15
          KT(I)=MKX-1                                                            MINIMI.16
        ENDIF                                                                    MINIMI.17
  200 CONTINUE                                                                   MINIMI.18
C                                                                                MINIMI.19
      RETURN                                                                     MINIMI.20
      END                                                                        MINIMI.21
                                                                                 MINIMI.22
