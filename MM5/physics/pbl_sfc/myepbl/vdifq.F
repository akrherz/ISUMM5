C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&         VDIFQ.1
      SUBROUTINE VDIFQ(LMHK,KTM,DTQ2,Q2,EL,Z)                                    VDIFQ.2
C     ******************************************************************         VDIFQ.3
C     *                                                                *         VDIFQ.4
C     *  VERTICAL DIFFUSION                                            *         VDIFQ.5
C     *                                                                *         VDIFQ.6
C     ******************************************************************         VDIFQ.7
C-----------------------------------------------------------------------         VDIFQ.8
#     include <parame.incl>                                                      VDIFQ.9
      PARAMETER (LM=MKX)                                                         VDIFQ.10
C-----------------------------------------------------------------------         VDIFQ.11
                             P A R A M E T E R                                   VDIFQ.12
     &(LP1=LM+1,LM1=LM-1,LM2=LM-2)                                               VDIFQ.13
                             P A R A M E T E R                                   VDIFQ.14
     &(ESQ=0.20,ELZ0=0.)                                                         VDIFQ.15
C-----------------------------------------------------------------------         VDIFQ.16
                             D I M E N S I O N                                   VDIFQ.17
     & Q2    (LM)                                                                VDIFQ.18
                             D I M E N S I O N                                   VDIFQ.19
     & EL    (LM1)                                                               VDIFQ.20
     &,Z     (LP1)                                                               VDIFQ.21
                             D I M E N S I O N                                   VDIFQ.22
     & CM    (LM2),CR    (LM2),RSQ2  (LM2),AKQ   (LM2),DTOZ  (LM2)               VDIFQ.23
C-----------------------------------------------------------------------         VDIFQ.24
C***********************************************************************         VDIFQ.25
      DTDIF=DTQ2/FLOAT(KTM)                                                      VDIFQ.26
      LMHM=LMHK-1                                                                VDIFQ.27
      LMH2=LMHK-2                                                                VDIFQ.28
      LMHP=LMHK+1                                                                VDIFQ.29
C-----------------------------------------------------------------------         VDIFQ.30
      DO 300 KT=1,KTM                                                            VDIFQ.31
C-----------------------------------------------------------------------         VDIFQ.32
        DO 100 L=1,LMH2                                                          VDIFQ.33
          DTOZ(L)=(DTDIF+DTDIF)/(Z(L)-Z(L+2))                                    VDIFQ.34
          AKQ(L)=SQRT((Q2(L)+Q2(L+1))*0.5)*(EL(L)+EL(L+1))*0.5*ESQ/(             VDIFQ.35
     +           Z(L+1)-Z(L+2))                                                  VDIFQ.36
          CR(L)=-DTOZ(L)*AKQ(L)                                                  VDIFQ.37
  100   CONTINUE                                                                 VDIFQ.38
C                                                                                VDIFQ.39
        CM(1)=DTOZ(1)*AKQ(1)+1.                                                  VDIFQ.40
        RSQ2(1)=Q2(1)                                                            VDIFQ.41
C-----------------------------------------------------------------------         VDIFQ.42
        DO 110 L=2,LMH2                                                          VDIFQ.43
          CF=-DTOZ(L)*AKQ(L-1)/CM(L-1)                                           VDIFQ.44
          CM(L)=-CR(L-1)*CF+(AKQ(L-1)+AKQ(L))*DTOZ(L)+1.                         VDIFQ.45
          RSQ2(L)=-RSQ2(L-1)*CF+Q2(L)                                            VDIFQ.46
  110   CONTINUE                                                                 VDIFQ.47
C-----------------------------------------------------------------------         VDIFQ.48
        DTOZS=(DTDIF+DTDIF)/(Z(LMHM)-Z(LMHP))                                    VDIFQ.49
        AKQS=SQRT((Q2(LMHM)+Q2(LMHK))*0.5)*(EL(LMHM)+ELZ0)*0.5*ESQ/(             VDIFQ.50
     +       Z(LMHK)-Z(LMHP))                                                    VDIFQ.51
C                                                                                VDIFQ.52
        CF=-DTOZS*AKQ(LMH2)/CM(LMH2)                                             VDIFQ.53
C-----------------------------------------------------------------------         VDIFQ.54
        Q2(LMHM)=(DTOZS*AKQS*Q2(LMHK)-RSQ2(LMH2)*CF+Q2(LMHM))/((                 VDIFQ.55
     +           AKQ(LMH2)+AKQS)*DTOZS-CR(LMH2)*CF+1.)                           VDIFQ.56
C                                                                                VDIFQ.57
        DO 120 IVI=1,LMH2                                                        VDIFQ.58
          L=LMHM-IVI                                                             VDIFQ.59
          Q2(L)=(-CR(L)*Q2(L+1)+RSQ2(L))/CM(L)                                   VDIFQ.60
  120   CONTINUE                                                                 VDIFQ.61
C-----------------------------------------------------------------------         VDIFQ.62
  300 CONTINUE                                                                   VDIFQ.63
C-----------------------------------------------------------------------         VDIFQ.64
      RETURN                                                                     VDIFQ.65
      END                                                                        VDIFQ.66
                                                                                 VDIFQ.67
