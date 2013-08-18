      SUBROUTINE COEF_DIFFU(XKC,XKD,U,V,                                         COEF_DIFFU.1
     &                      XKHZ,XKHMAX,C200,ILX,JLX,KL,IN)                      23DEC04.3386
C                                                                                COEF_DIFFU.3
C-----THIS SUBROUTINE CALCULATES THE DIFFUSION COEFFICEINT.                      COEF_DIFFU.4
C                                                                                COEF_DIFFU.5
#       include <parame.incl>                                                    COEF_DIFFU.6
C                                                                                COEF_DIFFU.7
      DIMENSION XKC(MIX,MJX,MKX), XKD(MIX,MJX,MKX)                               COEF_DIFFU.8
      DIMENSION   U(MIX,MJX,MKX),   V(MIX,MJX,MKX)                               COEF_DIFFU.9
C                                                                                COEF_DIFFU.10
C DIMENSIONS ADDED FOR VECTORIZATION & MULTITASKING.                             COEF_DIFFU.11
C                                                                                COEF_DIFFU.12
      DIMENSION DUDX(MIX,MKX),DUDY(MIX,MKX),                                     COEF_DIFFU.13
     &          DVDX(MIX,MKX),DVDY(MIX,MKX)                                      COEF_DIFFU.14
C                                                                                COEF_DIFFU.15
C_FLIC_RUNPAD(1)                                                                 COEF_DIFFU.16
cmic$ do all autoscope                                                           COEF_DIFFU.17
cmic$1 shared(XKCRS,U,V,C200,XKHZ,XKHMAX)                                        COEF_DIFFU.18
cmic$2 private(I,J,K,DUDX,DVDX,DUDY,DVDY,DIV,CELL)                               COEF_DIFFU.19
c$doacross                                                                       COEF_DIFFU.20
c$& local(i,j,k,dudx,dvdx,dudy,dvdy,div,cell)                                    COEF_DIFFU.21
c$omp parallel do default(shared)                                                COEF_DIFFU.22
c$omp&private(i,j,k,dudx,dvdx,dudy,dvdy,div,cell)                                COEF_DIFFU.23
      DO J=1,JLX                                                                 COEF_DIFFU.24
        DO K=1,KL                                                                COEF_DIFFU.25
          DO I=1,ILX                                                             COEF_DIFFU.26
            DUDX(I,K)=U(I,J+1,K)+U(I+1,J+1,K)-U(I,J,K)-U(I+1,J,K)                COEF_DIFFU.27
            DVDX(I,K)=V(I,J+1,K)+V(I+1,J+1,K)-V(I,J,K)-V(I+1,J,K)                COEF_DIFFU.28
            DUDY(I,K)=U(I+1,J+1,K)+U(I+1,J,K)-U(I,J+1,K)-U(I,J,K)                COEF_DIFFU.29
            DVDY(I,K)=V(I+1,J+1,K)+V(I+1,J,K)-V(I,J+1,K)-V(I,J,K)                COEF_DIFFU.30
C                                                                                COEF_DIFFU.31
            DIV=                                                                 COEF_DIFFU.32
     +          SQRT((DUDX(I,K)-DVDY(I,K))*(DUDX(I,K)-DVDY(I,K))+(               COEF_DIFFU.33
     +          DVDX(I,K)+DUDY(I,K))*(DVDX(I,K)+DUDY(I,K)))                      COEF_DIFFU.34
            CELL=XKHZ+C200*DIV                                                   COEF_DIFFU.35
C                                                                                COEF_DIFFU.36
            IF(CELL.GT.XKHMAX)THEN                                               COEF_DIFFU.37
              XKC(I,J,K)=XKHMAX                                                  COEF_DIFFU.38
            ELSE                                                                 COEF_DIFFU.39
              XKC(I,J,K)=CELL                                                    COEF_DIFFU.40
            ENDIF                                                                COEF_DIFFU.41
          ENDDO                                                                  COEF_DIFFU.42
        ENDDO                                                                    COEF_DIFFU.43
      ENDDO                                                                      COEF_DIFFU.44
C                                                                                COEF_DIFFU.45
cmic$ do all autoscope                                                           COEF_DIFFU.46
cmic$1 shared(XKC,XKD)                                                           COEF_DIFFU.47
cmic$2 private(I,J,K)                                                            COEF_DIFFU.48
c$doacross                                                                       COEF_DIFFU.49
c$& local(i,j,k)                                                                 COEF_DIFFU.50
c$omp parallel do default(shared)                                                COEF_DIFFU.51
c$omp&private(i,j,k)                                                             COEF_DIFFU.52
      DO J=2,JLX                                                                 COEF_DIFFU.53
        DO K=1,KL                                                                COEF_DIFFU.54
          DO I=2,ILX                                                             COEF_DIFFU.55
            XKD(I,J,K)=0.25*(XKC(I,J,K)+XKC(I-1,J-1,K)+XKC(I,J-1,K)+             COEF_DIFFU.56
     +                 XKC(I-1,J,K))                                             COEF_DIFFU.57
          ENDDO                                                                  COEF_DIFFU.58
        ENDDO                                                                    COEF_DIFFU.59
      ENDDO                                                                      COEF_DIFFU.60
C                                                                                COEF_DIFFU.61
                                                                                 23DEC04.3387
       IF (IN .GT. 1) THEN                                                       23DEC04.3388
c  enhance momentum diffsion coefficient if two-grid interval                    23DEC04.3389
c  noise develops along the lateral boundary of a nest                           23DEC04.3390
c$doacross                                                                       23DEC04.3391
c$& local(i,j,k)                                                                 23DEC04.3392
c$omp parallel do default(shared)                                                23DEC04.3393
c$omp&private(i,j,k)                                                             23DEC04.3394
       DO K=1,KL                                                                 23DEC04.3395
         DO I=3,ILX-1                                                            23DEC04.3396
           j = 3                                                                 23DEC04.3397
           XKD(I,J,K) = XKD(I,J,K)+3*C200*(abs(4*U(I,J,K)-U(I+1,J,K)-            23DEC04.3398
     &     U(I-1,J,K)-U(I,J+1,K)-U(I,J-1,K))+abs(4*V(I,J,K)-V(I+1,J,K)-          23DEC04.3399
     &     V(I-1,J,K)-V(I,J+1,K)-V(I,J-1,K)))                                    23DEC04.3400
           if (XKD(I,J,K).gt.XKHMAX) XKD(I,J,K)=XKHMAX                           23DEC04.3401
           j = jlx-1                                                             23DEC04.3402
           XKD(I,J,K) = XKD(I,J,K)+3*C200*(abs(4*U(I,J,K)-U(I+1,J,K)-            23DEC04.3403
     &     U(I-1,J,K)-U(I,J+1,K)-U(I,J-1,K))+abs(4*V(I,J,K)-V(I+1,J,K)-          23DEC04.3404
     &     V(I-1,J,K)-V(I,J+1,K)-V(I,J-1,K)))                                    23DEC04.3405
           if (XKD(I,J,K).gt.XKHMAX) XKD(I,J,K)=XKHMAX                           23DEC04.3406
         ENDDO                                                                   23DEC04.3407
         DO J=4,JLX-2                                                            23DEC04.3408
           i = 3                                                                 23DEC04.3409
           XKD(I,J,K) = XKD(I,J,K)+3*C200*(abs(4*U(I,J,K)-U(I+1,J,K)-            23DEC04.3410
     &     U(I-1,J,K)-U(I,J+1,K)-U(I,J-1,K))+abs(4*V(I,J,K)-V(I+1,J,K)-          23DEC04.3411
     &     V(I-1,J,K)-V(I,J+1,K)-V(I,J-1,K)))                                    23DEC04.3412
           if (XKD(I,J,K).gt.XKHMAX) XKD(I,J,K)=XKHMAX                           23DEC04.3413
           i = ilx-1                                                             23DEC04.3414
           XKD(I,J,K) = XKD(I,J,K)+3*C200*(abs(4*U(I,J,K)-U(I+1,J,K)-            23DEC04.3415
     &     U(I-1,J,K)-U(I,J+1,K)-U(I,J-1,K))+abs(4*V(I,J,K)-V(I+1,J,K)-          23DEC04.3416
     &     V(I-1,J,K)-V(I,J+1,K)-V(I,J-1,K)))                                    23DEC04.3417
           if (XKD(I,J,K).gt.XKHMAX) XKD(I,J,K)=XKHMAX                           23DEC04.3418
         ENDDO                                                                   23DEC04.3419
        ENDDO                                                                    23DEC04.3420
       ENDIF                                                                     23DEC04.3421
                                                                                 23DEC04.3422
      RETURN                                                                     COEF_DIFFU.62
      END                                                                        COEF_DIFFU.63
                                                                                 COEF_DIFFU.64
