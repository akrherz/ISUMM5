      SUBROUTINE MAXIMI(ARRAY,MIX,MJX,MKX,KS,KE,KT,IST,IEN)                      07NOV00.1363
      IMPLICIT NONE                                                              07NOV00.1364
C MJX is needed for vector version                                               07NOV00.1365
      INTEGER   MIX,MJX,MKX                                                      07NOV00.1366
      REAL      ARRAY(MIX,MKX)                                                   07NOV00.1367
      REAL      X,XAR                                                            07NOV00.1368
      INTEGER KT(MIX)                                                            MAXIMI.3
      INTEGER I,K,IST,IEN,KE,KS                                                  07NOV00.1369
      DO 200 I=IST,IEN                                                           07NOV00.1370
        KT(I)=KS                                                                 MAXIMI.5
        X=ARRAY(I,KS)                                                            MAXIMI.6
C                                                                                MAXIMI.7
        DO 100 K=KS,KE                                                           MAXIMI.8
          XAR=ARRAY(I,K)                                                         MAXIMI.9
          IF(XAR.GE.X)THEN                                                       MAXIMI.10
            X=XAR                                                                MAXIMI.11
            KT(I)=K                                                              MAXIMI.12
          ENDIF                                                                  MAXIMI.13
  100   CONTINUE                                                                 MAXIMI.14
  200 CONTINUE                                                                   MAXIMI.15
C                                                                                MAXIMI.16
      RETURN                                                                     MAXIMI.17
      END                                                                        MAXIMI.18
                                                                                 MAXIMI.19
