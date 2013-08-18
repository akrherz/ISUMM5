C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&         VDIFH.1
      SUBROUTINE VDIFH(LMHK,KTM,DTQ2,THZ0,QZ0,AKHS,CT,WETM                       VDIFH.2
     &,                T,Q,AKH,APE,Z)                                            VDIFH.3
C     ******************************************************************         VDIFH.4
C     *                                                                *         VDIFH.5
C     *            VERTICAL DIFFUSION OF MASS VARIABLES                *         VDIFH.6
C     *                                                                *         VDIFH.7
C     ******************************************************************         VDIFH.8
C-----------------------------------------------------------------------         VDIFH.9
#     include <parame.incl>                                                      VDIFH.10
      PARAMETER (LM=MKX)                                                         VDIFH.11
C-----------------------------------------------------------------------         VDIFH.12
                             P A R A M E T E R                                   VDIFH.13
     &(LP1=LM+1,LM1=LM-1)                                                        VDIFH.14
C-----------------------------------------------------------------------         VDIFH.15
                             P A R A M E T E R                                   VDIFH.16
     &(CKLQ=1.)                                                                  VDIFH.17
C-----------------------------------------------------------------------         VDIFH.18
                             D I M E N S I O N                                   VDIFH.19
     & T     (LM),Q     (LM)                                                     VDIFH.20
                             D I M E N S I O N                                   VDIFH.21
     & AKH   (LM1)                                                               VDIFH.22
     &,APE   (LM)                                                                VDIFH.23
     &,Z     (LP1)                                                               VDIFH.24
                             D I M E N S I O N                                   VDIFH.25
     & CM    (LM1),CR    (LM1),RST   (LM1),RSQ   (LM1)                           VDIFH.26
     &,DTOZ  (LM1),AKCT  (LM1)                                                   VDIFH.27
C-----------------------------------------------------------------------         VDIFH.28
C***********************************************************************         VDIFH.29
      DTDIF=DTQ2/FLOAT(KTM)                                                      VDIFH.30
      LMHM=LMHK-1                                                                VDIFH.31
      LMHP=LMHK+1                                                                VDIFH.32
C-----------------------------------------------------------------------         VDIFH.33
      DO 100 L=1,LMHM                                                            VDIFH.34
        DTOZ(L)=DTDIF/(Z(L)-Z(L+1))                                              VDIFH.35
        CR(L)=-DTOZ(L)*AKH(L)                                                    VDIFH.36
        AKCT(L)=AKH(L)*(Z(L)-Z(L+2))*0.5*CT                                      VDIFH.37
  100 CONTINUE                                                                   VDIFH.38
C                                                                                VDIFH.39
      CM(1)=DTOZ(1)*AKH(1)+1.                                                    VDIFH.40
C-----------------------------------------------------------------------         VDIFH.41
      DO 300 KT=1,KTM                                                            VDIFH.42
C-----------------------------------------------------------------------         VDIFH.43
        RST(1)=-AKCT(1)*DTOZ(1)+T(1)*APE(1)                                      VDIFH.44
        RSQ(1)=Q(1)                                                              VDIFH.45
C-----------------------------------------------------------------------         VDIFH.46
        DO 110 L=2,LMHM                                                          VDIFH.47
          DTOZL=DTOZ(L)                                                          VDIFH.48
          CF=-DTOZL*AKH(L-1)/CM(L-1)                                             VDIFH.49
          CM(L)=-CR(L-1)*CF+(AKH(L-1)+AKH(L))*DTOZL+1.                           VDIFH.50
          RST(L)=-RST(L-1)*CF+(AKCT(L-1)-AKCT(L))*DTOZL+T(L)*APE(L)              VDIFH.51
          RSQ(L)=-RSQ(L-1)*CF+Q(L)                                               VDIFH.52
  110   CONTINUE                                                                 VDIFH.53
C-----------------------------------------------------------------------         VDIFH.54
        DTOZS=DTDIF/(Z(LMHK)-Z(LMHP))                                            VDIFH.55
        AKHH=AKH(LMHM)                                                           VDIFH.56
C                                                                                VDIFH.57
        CF=-DTOZS*AKHH/CM(LMHM)                                                  VDIFH.58
        AKQS=AKHS*CKLQ*WETM                                                      VDIFH.59
C                                                                                VDIFH.60
        CMB=CR(LMHM)*CF                                                          VDIFH.61
        CMTB=-CMB+(AKHH+AKHS)*DTOZS+1.                                           VDIFH.62
        CMQB=-CMB+(AKHH+AKQS)*DTOZS+1.                                           VDIFH.63
C                                                                                VDIFH.64
        RSTB=-RST(LMHM)*CF+(AKCT(LMHM)-AKHS*CT)*DTOZS+T(LMHK)*APE(LMHK)          VDIFH.65
        RSQB=-RSQ(LMHM)*CF+Q(LMHK)                                               VDIFH.66
C-----------------------------------------------------------------------         VDIFH.67
        T(LMHK)=(DTOZS*AKHS*THZ0+RSTB)/(APE(LMHK)*CMTB)                          VDIFH.68
        Q(LMHK)=(DTOZS*AKQS*QZ0+RSQB)/CMQB                                       VDIFH.69
C-----------------------------------------------------------------------         VDIFH.70
        DO 120 IVI=1,LMHM                                                        VDIFH.71
          L=LMHK-IVI                                                             VDIFH.72
          RCML=1./CM(L)                                                          VDIFH.73
          T(L)=(-CR(L)*T(L+1)*APE(L+1)+RST(L))*RCML/APE(L)                       VDIFH.74
          Q(L)=(-CR(L)*Q(L+1)+RSQ(L))*RCML                                       VDIFH.75
  120   CONTINUE                                                                 VDIFH.76
C-----------------------------------------------------------------------         VDIFH.77
  300 CONTINUE                                                                   VDIFH.78
C-----------------------------------------------------------------------         VDIFH.79
      RETURN                                                                     VDIFH.80
      END                                                                        VDIFH.81
                                                                                 VDIFH.82
