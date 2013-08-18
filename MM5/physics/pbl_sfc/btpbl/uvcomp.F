C                                                                                UVCOMP.1
C ===================================================================            UVCOMP.2
C                                                                                UVCOMP.3
      SUBROUTINE UVCOMP(PHI,PHIN,EKM,ZS,ZN,WK,DT,USTAR,ANG,Z,II)                 UVCOMP.4
      DIMENSION PHI(II),PHIN(II),EKM(II),ZS(II),ZN(II),                          UVCOMP.5
     1 WK(II,11),Z(II)                                                           UVCOMP.6
C                                                                                UVCOMP.7
C   PHI,PHIN=CURRENT AND FORECAST WIND COMPS.                                    UVCOMP.8
      IM1=II-1                                                                   UVCOMP.9
      IM2=II-2                                                                   UVCOMP.10
      DO 10 I=2,IM1                                                              UVCOMP.11
        WK(I,1)=(EKM(I)+EKM(I-1))/(ZS(I)*(ZN(I)+ZS(I)))                          UVCOMP.12
        WK(I,2)=((EKM(I+1)+EKM(I))/ZN(I)+(EKM(I)+EKM(I-1))/ZS(I))/(ZN(I)         UVCOMP.13
     +          +ZS(I))+1./DT                                                    UVCOMP.14
        WK(I,3)=(EKM(I)+EKM(I+1))/(ZN(I)*(ZN(I)+ZS(I)))                          UVCOMP.15
        WK(I,4)=PHI(I)/DT                                                        UVCOMP.16
   10 CONTINUE                                                                   UVCOMP.17
C   INSERT FLUX B.C.S.                                                           UVCOMP.18
      WK(1,1)=0.0                                                                UVCOMP.19
      WK(1,2)=(EKM(2)+EKM(1))/(Z(2)*(Z(2)-Z(1)))+1./DT                           UVCOMP.20
      WK(1,3)=(EKM(2)+EKM(1))/(Z(2)*(Z(2)-Z(1)))                                 UVCOMP.21
      WK(1,4)=PHI(1)/DT-(2.*USTAR*USTAR*ANG)/Z(2)                                UVCOMP.22
C   INSERT UPPER B.C.                                                            UVCOMP.23
      WK(IM1,4)=WK(IM1,4)+WK(IM1,3)*PHI(II)                                      UVCOMP.24
      WK(IM1,3)=0.0                                                              UVCOMP.25
C  BEGIN TRIDIAG. SOLN.                                                          UVCOMP.26
      WK(1,5)=WK(1,3)/WK(1,2)                                                    UVCOMP.27
      WK(1,6)=WK(1,4)/WK(1,2)                                                    UVCOMP.28
      DO 20 I=2,IM1                                                              UVCOMP.29
        DENOM=WK(I,2)-WK(I,1)*WK(I-1,5)                                          UVCOMP.30
        WK(I,5)=WK(I,3)/DENOM                                                    UVCOMP.31
        WK(I,6)=(WK(I,4)+WK(I,1)*WK(I-1,6))/DENOM                                UVCOMP.32
   20 CONTINUE                                                                   UVCOMP.33
      PHIN(IM1)=WK(IM1,6)                                                        UVCOMP.34
      DO 30 L=IM2,1,-1                                                           UVCOMP.35
        PHIN(L)=WK(L,6)+WK(L,5)*PHIN(L+1)                                        UVCOMP.36
   30 CONTINUE                                                                   UVCOMP.37
      RETURN                                                                     UVCOMP.38
      END                                                                        UVCOMP.39
                                                                                 UVCOMP.40
