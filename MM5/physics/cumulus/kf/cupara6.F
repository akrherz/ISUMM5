       SUBROUTINE CUPARA6(PSB,T3DTEN,QV3DTEN,QR3DTEN,QC3DTEN,                    CUPARA6.1
     &                    QS3DTEN,QI3DTEN,RAINC,SCR1,U3D,V3D,T3D,QV3D,           CUPARA6.2
     &                    PP3D,J,INEST,IMPHYS,IBEG,IEND)                         CUPARA6.3
                                                                                 07NOV00.1384
      IMPLICIT NONE                                                              07NOV00.1385
                                                                                 CUPARA6.4
#     include <parame.incl>                                                      CUPARA6.5
#     include <various.incl>                                                     CUPARA6.6
#     include <param3.incl>                                                      CUPARA6.7
#     include <addrcu.incl>                                                      CUPARA6.8
#     include <pmoist.incl>                                                      CUPARA6.9
#     include <nhcnst.incl>                                                      CUPARA6.10
                                                                                 CUPARA6.11
      INTEGER J,INEST,IMPHYS,IBEG,IEND                                           07NOV00.1386
      REAL RAINC(MIX,MJX), PSB(MIX,MJX), SCR1(MIX,MKX)                           07NOV00.1387
      REAL T3DTEN(MIX,MJX,MKX), QV3DTEN(MIX,MJX,MKX)                             07NOV00.1388
      REAL QR3DTEN(MIXM,MJXM,MKXM), QC3DTEN(MIXM,MJXM,MKXM)                      07NOV00.1389
      REAL QS3DTEN(MIXM,MJXM,MKXM), QI3DTEN(MIXM,MJXM,MKXM)                      07NOV00.1390
      REAL T3D(MIX,MJX,MKX), QV3D(MIX,MJX,MKX)                                   07NOV00.1391
      REAL U3D(MIX,MJX,MKX), V3D(MIX,MJX,MKX)                                    07NOV00.1392
      REAL PP3D(MIX,MJX,MKX)                                                     07NOV00.1393
C                                                                                07NOV00.1394
      INTEGER I,K,NK                                                             07NOV00.1395
      INTEGER ICLDCK,NTST                                                        07NOV00.1396
      REAL TST,TV,PRS,RHOE,W0                                                    07NOV00.1397
C                                                                                CUPARA6.19
C--- CALL CUMULUS PARAMETERIZATION                                               CUPARA6.20
C                                                                                CUPARA6.21
C...TST IS THE NUMBER OF TIME STEPS IN 10 MINUTES...W0AVG IS CLOSE TO A          CUPARA6.22
C...RUNNING MEAN VERTICAL VELOCITY...NOTE THAT IF YOU CHANGE TST, IT WIL         CUPARA6.23
C...CHANGE THE FREQUENCY OF THE CONVECTIVE INTITIATION CHECK (SEE BELOW)         CUPARA6.24
C...NOTE THAT THE ORDERING OF VERTICAL LAYERS MUST BE REVERSED FOR W0AVG         CUPARA6.25
C...BECAUSE THE ORDERING IS REVERSED IN KFPARA...                                CUPARA6.26
C                                                                                CUPARA6.27
      NTST=NINT(1200./DT2)                                                       CUPARA6.28
      TST=FLOAT(NTST)                                                            CUPARA6.29
      DO 431 I=IBEG,IEND                                                         CUPARA6.30
        DO 431 NK=1,KL                                                           CUPARA6.31
          K=KL-NK+1                                                              CUPARA6.32
          TV=T3D(I,J,K)*(1.+EP1*QV3D(I,J,K)*XMOIST(INEST))                       CUPARA6.33
          PRS=1.E3*(A(K)*PSB(I,J)+PTOP)+PP3D(I,J,K)                              CUPARA6.34
          RHOE=PRS/(R*TV)                                                        CUPARA6.35
          W0=-101.9368*SCR1(I,K)/RHOE                                            CUPARA6.36
C           W0AVG(I,J,NK)=W0                                                     CUPARA6.37
          W0AVG(I,J,NK)=(W0AVG(I,J,NK)*(TST-1.)+W0)/TST                          CUPARA6.38
  431   CONTINUE                                                                 CUPARA6.39
                                                                                 CUPARA6.40
C                                                                                CUPARA6.41
C...CHECK FOR CONVECTIVE INITIATION EVERY 5 MINUTES (OR NTST/2)...               CUPARA6.42
C                                                                                CUPARA6.43
      NTST=NINT(0.5*TST)                                                         CUPARA6.44
      NTST=MAX0(NTST,1)                                                          CUPARA6.45
      ICLDCK=MOD(KTAU,NTST)                                                      CUPARA6.46
      IF(ICLDCK.EQ.0)CALL KFPARA(J,INEST,IBEG,IEND,T0,U3D,V3D,PP3D,T3D,          CUPARA6.47
     +  QV3D,PSB)                                                                CUPARA6.48
      DO 465 I=IBEG,IEND                                                         CUPARA6.49
        IF(NCA(I,J).LE.0)GOTO 465                                                CUPARA6.50
        DO 460 K=1,KL                                                            CUPARA6.51
          IF(IMPHYS.GE.3)THEN                                                    CUPARA6.52
            QR3DTEN(I,J,K)=QR3DTEN(I,J,K)+DQRDT(I,J,K)*PSB(I,J)                  CUPARA6.53
            QC3DTEN(I,J,K)=QC3DTEN(I,J,K)+DQLDT(I,J,K)*PSB(I,J)                  CUPARA6.54
          ENDIF                                                                  25JAN00.475
          IF(IMPHYS.GE.5)THEN                                                    25JAN00.476
C......QSTEN STORES GRAUPEL TENDENCY IF IT EXISTS, OTHERISE SNOW (V2)            CUPARA6.56
            QS3DTEN(I,J,K)=QS3DTEN(I,J,K)+DQSDT(I,J,K)*PSB(I,J)                  CUPARA6.57
            QI3DTEN(I,J,K)=QI3DTEN(I,J,K)+DQIDT(I,J,K)*PSB(I,J)                  CUPARA6.58
          ENDIF                                                                  CUPARA6.59
          T3DTEN(I,J,K)=T3DTEN(I,J,K)+DTDT(I,J,K)*PSB(I,J)                       CUPARA6.60
          QV3DTEN(I,J,K)=QV3DTEN(I,J,K)+DQDT(I,J,K)*PSB(I,J)                     CUPARA6.61
  460   CONTINUE                                                                 CUPARA6.62
        RAINC(I,J)=RAINC(I,J)+RAINCV(I,J)                                        CUPARA6.63
  465 CONTINUE                                                                   CUPARA6.64
      RETURN                                                                     CUPARA6.65
      END                                                                        CUPARA6.66
                                                                                 CUPARA6.67
