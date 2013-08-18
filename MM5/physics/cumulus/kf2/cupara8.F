       SUBROUTINE CUPARA8(PSB,T3DTEN,QV3DTEN,QR3DTEN,QC3DTEN,                    CUPARA8.1
     &                    QS3DTEN,QI3DTEN,RAINC,SCR1,U3D,V3D,T3D,QV3D,           CUPARA8.2
     &                    PP3D,J,INEST,IMPHYS,IBEG,IEND)                         CUPARA8.3
                                                                                 CUPARA8.4
      IMPLICIT NONE                                                              CUPARA8.5
                                                                                 CUPARA8.6
#     include <parame.incl>                                                      CUPARA8.7
#     include <various.incl>                                                     CUPARA8.8
#     include <param3.incl>                                                      CUPARA8.9
#     include <addrcu.incl>                                                      CUPARA8.10
#     include <pmoist.incl>                                                      CUPARA8.11
#     include <nhcnst.incl>                                                      CUPARA8.12
                                                                                 CUPARA8.13
      INTEGER J,INEST,IMPHYS,IBEG,IEND                                           CUPARA8.14
      REAL RAINC(MIX,MJX), PSB(MIX,MJX), SCR1(MIX,MKX)                           CUPARA8.15
      REAL T3DTEN(MIX,MJX,MKX), QV3DTEN(MIX,MJX,MKX)                             CUPARA8.16
      REAL QR3DTEN(MIXM,MJXM,MKXM), QC3DTEN(MIXM,MJXM,MKXM)                      CUPARA8.17
      REAL QS3DTEN(MIXM,MJXM,MKXM), QI3DTEN(MIXM,MJXM,MKXM)                      CUPARA8.18
      REAL T3D(MIX,MJX,MKX), QV3D(MIX,MJX,MKX)                                   CUPARA8.19
      REAL U3D(MIX,MJX,MKX), V3D(MIX,MJX,MKX)                                    CUPARA8.20
      REAL PP3D(MIX,MJX,MKX)                                                     CUPARA8.21
C                                                                                CUPARA8.22
      INTEGER I,K,NK                                                             CUPARA8.23
      INTEGER ICLDCK,NTST                                                        CUPARA8.24
      integer jcldck, ncldck                                                     CUPARA8.25
      REAL TST,TV,PRS,RHOE,W0                                                    CUPARA8.26
C                                                                                CUPARA8.27
C--- CALL CUMULUS PARAMETERIZATION                                               CUPARA8.28
C                                                                                CUPARA8.29
C...TST IS THE NUMBER OF TIME STEPS IN 10 MINUTES...W0AVG IS CLOSE TO A          CUPARA8.30
C...RUNNING MEAN VERTICAL VELOCITY...NOTE THAT IF YOU CHANGE TST, IT WILL        CUPARA8.31
C...CHANGE THE FREQUENCY OF THE CONVECTIVE INTITIATION CHECK (SEE BELOW)         CUPARA8.32
C...NOTE THAT THE ORDERING OF VERTICAL LAYERS MUST BE REVERSED FOR W0AVG         CUPARA8.33
C...BECAUSE THE ORDERING IS REVERSED IN KFPARA...                                CUPARA8.34
C                                                                                CUPARA8.35
      NTST=NINT(1200./DT2)                                                       CUPARA8.36
      TST=FLOAT(NTST)                                                            CUPARA8.37
      DO 431 I=IBEG,IEND                                                         CUPARA8.38
        DO 431 NK=1,KL                                                           CUPARA8.39
          K=KL-NK+1                                                              CUPARA8.40
          TV=T3D(I,J,K)*(1.+EP1*QV3D(I,J,K)*XMOIST(INEST))                       CUPARA8.41
          PRS=1.E3*(A(K)*PSB(I,J)+PTOP)+PP3D(I,J,K)                              CUPARA8.42
          RHOE=PRS/(R*TV)                                                        CUPARA8.43
          W0=-101.9368*SCR1(I,K)/RHOE                                            CUPARA8.44
C           W0AVG(I,J,NK)=W0                                                     CUPARA8.45
          W0AVG(I,J,NK)=(W0AVG(I,J,NK)*(TST-1.)+W0)/TST                          CUPARA8.46
  431   CONTINUE                                                                 CUPARA8.47
                                                                                 CUPARA8.48
C                                                                                CUPARA8.49
C...CHECK FOR CONVECTIVE INITIATION EVERY 5 MINUTES (OR NTST/2)...               CUPARA8.50
C                                                                                CUPARA8.51
      NTST=NINT(0.5*TST)                                                         CUPARA8.52
      NTST=MAX0(NTST,1)                                                          CUPARA8.53
      ICLDCK=MOD(KTAU,NTST)                                                      CUPARA8.54
c     ncldck = ntst                                                              CUPARA8.55
c     jcldck = mod(ktau-ncldck/2,ncldck)                                         CUPARA8.56
c     if ( j .eq. 10 .and. inest .eq. 1) then                                    CUPARA8.57
c       write(6,*) 'icldck = ',icldck,' jcldck = ',jcldck                        CUPARA8.58
c       write(6,*) 'ntst = ',ntst,' tst = ',tst,' ktau = ',ktau,                 CUPARA8.59
c    & ' dt2 = ',dt2                                                             CUPARA8.60
c     endif                                                                      CUPARA8.61
      IF(ICLDCK.EQ.0) THEN                                                       CUPARA8.62
         CALL KFDRIVE (psb,scr1,j,inest,ibeg,iend,T0,U3D,V3D,PP3D,T3D,           19DEC02.1362
     &     QV3D)                                                                 CUPARA8.64
      ENDIF                                                                      CUPARA8.65
      DO 465 I=IBEG,IEND                                                         CUPARA8.66
        IF(NCA(I,J).LE.0)GOTO 465                                                CUPARA8.67
        DO 460 K=1,KL                                                            CUPARA8.68
          IF(IMPHYS.GE.3)THEN                                                    CUPARA8.69
            QR3DTEN(I,J,K)=QR3DTEN(I,J,K)+DQRDT(I,J,K)*PSB(I,J)                  CUPARA8.70
            QC3DTEN(I,J,K)=QC3DTEN(I,J,K)+DQLDT(I,J,K)*PSB(I,J)                  CUPARA8.71
          ENDIF                                                                  CUPARA8.72
          IF(IMPHYS.GE.5)THEN                                                    CUPARA8.73
C......QSTEN STORES GRAUPEL TENDENCY IF IT EXISTS, OTHERISE SNOW (V2)            CUPARA8.74
            QS3DTEN(I,J,K)=QS3DTEN(I,J,K)+DQSDT(I,J,K)*PSB(I,J)                  CUPARA8.75
            QI3DTEN(I,J,K)=QI3DTEN(I,J,K)+DQIDT(I,J,K)*PSB(I,J)                  CUPARA8.76
          ENDIF                                                                  CUPARA8.77
          T3DTEN(I,J,K)=T3DTEN(I,J,K)+DTDT(I,J,K)*PSB(I,J)                       CUPARA8.78
          QV3DTEN(I,J,K)=QV3DTEN(I,J,K)+DQDT(I,J,K)*PSB(I,J)                     CUPARA8.79
  460   CONTINUE                                                                 CUPARA8.80
        RAINC(I,J)=RAINC(I,J)+RAINCV(I,J)                                        CUPARA8.81
        IF(NCAD(I,J).GT.0)THEN                                                   CUPARA8.82
          TNCA(I,J) = TNCA(I,J) + 1.                                             CUPARA8.83
c         SPSRC(I,J) = SPSRC(I,J) + PSRC(I,J)                                    CUPARA8.84
          SPCLB(I,J) = SPCLB(I,J) + PCLB(I,J)                                    CUPARA8.85
c...use peak value of UMF and PSRC instead of 1 hour avg.  10/6/99 jsk           CUPARA8.86
c                                                                                CUPARA8.87
c         SUMFB(I,J) = SUMFB(I,J) + UMFB(I,J)                                    CUPARA8.88
          SUMFB(I,J) = AMAX1(UMFB(I,J),SUMFB(I,J))                               CUPARA8.89
          SPSRC(I,J) = AMAX1(PSRC(I,J),SPSRC(I,J))                               CUPARA8.90
          NCAD(I,J) = NCAD(I,J)-1                                                CUPARA8.91
        ENDIF                                                                    CUPARA8.92
  465 CONTINUE                                                                   CUPARA8.93
      RETURN                                                                     CUPARA8.94
      END                                                                        CUPARA8.95
