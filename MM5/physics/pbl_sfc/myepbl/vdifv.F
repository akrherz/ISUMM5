C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&         VDIFV.1
      SUBROUTINE VDIFV(LMVK,KTM,DTQ2,UZ0,VZ0,AKMS,U,V,AKM,Z)                     VDIFV.2
C     ******************************************************************         VDIFV.3
C     *                                                                *         VDIFV.4
C     *  VERTICAL DIFFUSION                                            *         VDIFV.5
C     *                                                                *         VDIFV.6
C     ******************************************************************         VDIFV.7
C-----------------------------------------------------------------------         VDIFV.8
#     include <parame.incl>                                                      VDIFV.9
      PARAMETER (LM=MKX)                                                         VDIFV.10
C-----------------------------------------------------------------------         VDIFV.11
                             P A R A M E T E R                                   VDIFV.12
     &(LP1=LM+1,LM1=LM-1)                                                        VDIFV.13
C-----------------------------------------------------------------------         VDIFV.14
                             D I M E N S I O N                                   VDIFV.15
     & U     (LM),V     (LM)                                                     VDIFV.16
                             D I M E N S I O N                                   VDIFV.17
     & AKM   (LM1)                                                               VDIFV.18
     &,Z     (LP1)                                                               VDIFV.19
                             D I M E N S I O N                                   VDIFV.20
     & CM    (LM1),CR    (LM1),RSU   (LM1),RSV   (LM1)                           VDIFV.21
     &,DTOZ  (LM1)                                                               VDIFV.22
C-----------------------------------------------------------------------         VDIFV.23
C***********************************************************************         VDIFV.24
      DTDIF=DTQ2/FLOAT(KTM)                                                      VDIFV.25
      LMVM=LMVK-1                                                                VDIFV.26
      LMVP=LMVK+1                                                                VDIFV.27
C-----------------------------------------------------------------------         VDIFV.28
      DO 300 KT=1,KTM                                                            VDIFV.29
C-----------------------------------------------------------------------         VDIFV.30
        DO 100 L=1,LMVM                                                          VDIFV.31
          DTOZ(L)=DTDIF/(Z(L)-Z(L+1))                                            VDIFV.32
          CR(L)=-DTOZ(L)*AKM(L)                                                  VDIFV.33
  100   CONTINUE                                                                 VDIFV.34
C                                                                                VDIFV.35
        CM(1)=DTOZ(1)*AKM(1)+1.                                                  VDIFV.36
        RSU(1)=U(1)                                                              VDIFV.37
        RSV(1)=V(1)                                                              VDIFV.38
C-----------------------------------------------------------------------         VDIFV.39
        DO 110 L=2,LMVM                                                          VDIFV.40
          DTOZL=DTOZ(L)                                                          VDIFV.41
          CF=-DTOZL*AKM(L-1)/CM(L-1)                                             VDIFV.42
          CM(L)=-CR(L-1)*CF+(AKM(L-1)+AKM(L))*DTOZL+1.                           VDIFV.43
          RSU(L)=-RSU(L-1)*CF+U(L)                                               VDIFV.44
          RSV(L)=-RSV(L-1)*CF+V(L)                                               VDIFV.45
  110   CONTINUE                                                                 VDIFV.46
C-----------------------------------------------------------------------         VDIFV.47
        DTOZS=DTDIF/(Z(LMVK)-Z(LMVP))                                            VDIFV.48
        AKMH=AKM(LMVM)                                                           VDIFV.49
C                                                                                VDIFV.50
        CF=-DTOZS*AKMH/CM(LMVM)                                                  VDIFV.51
        RCMVB=1./((AKMH+AKMS)*DTOZS-CR(LMVM)*CF+1.)                              VDIFV.52
        DTOZAK=DTOZS*AKMS                                                        VDIFV.53
C-----------------------------------------------------------------------         VDIFV.54
        U(LMVK)=(DTOZAK*UZ0-RSU(LMVM)*CF+U(LMVK))*RCMVB                          VDIFV.55
        V(LMVK)=(DTOZAK*VZ0-RSV(LMVM)*CF+V(LMVK))*RCMVB                          VDIFV.56
C-----------------------------------------------------------------------         VDIFV.57
        DO 120 IVI=1,LMVM                                                        VDIFV.58
          L=LMVK-IVI                                                             VDIFV.59
          RCML=1./CM(L)                                                          VDIFV.60
          U(L)=(-CR(L)*U(L+1)+RSU(L))*RCML                                       VDIFV.61
          V(L)=(-CR(L)*V(L+1)+RSV(L))*RCML                                       VDIFV.62
  120   CONTINUE                                                                 VDIFV.63
C-----------------------------------------------------------------------         VDIFV.64
  300 CONTINUE                                                                   VDIFV.65
C-----------------------------------------------------------------------         VDIFV.66
      RETURN                                                                     VDIFV.67
      END                                                                        VDIFV.68
                                                                                 VDIFV.69
