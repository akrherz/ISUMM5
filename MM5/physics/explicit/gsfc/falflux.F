C FALLING FLUXES FOR EACH KIND OF CLOUD PARTICLES: C.G.S. UNIT                   FALFLUX.1
      SUBROUTINE FALFLUX(Q,VFALL,CS,DZ,KTOT,DT,ISTART,ISTOP)                     FALFLUX.2
#     include <parame.incl>                                                      FALFLUX.3
      DIMENSION Q(MIX,MKX),VFALL(MIX,MKX),CS(MIX,MKX),DZ(MIX,MKX)                FALFLUX.4
      REAL DWFLUX(MIX,MKX)                                                       FALFLUX.5
C TIME STEP FOR PARTICLE FALLOUT                                                 FALFLUX.6
      DO 1000 I=ISTART,ISTOP                                                     FALFLUX.7
        TFALL=1.E10                                                              FALFLUX.8
        DO 10 K=1,KTOT                                                           FALFLUX.9
          TFALL=AMIN1(TFALL,DZ(I,K)/(VFALL(I,K)+1.E-20))                         FALFLUX.10
   10   CONTINUE                                                                 FALFLUX.11
        IF(TFALL.GE.1.E10)GOTO 1000                                              FALFLUX.12
        NSUB=(INT(2.0*DT/TFALL)+1)                                               FALFLUX.13
        DTFALL=DT/NSUB                                                           FALFLUX.14
        DO 200 N=1,NSUB                                                          FALFLUX.15
C USE UPSTREAM METHOD (VFALL IS POSITIVE)                                        FALFLUX.16
          DO 20 K=1,KTOT-1                                                       FALFLUX.17
            DWFLUX(I,K)=-(CS(I,K)*VFALL(I,K)*Q(I,K)-CS(I,K+1)*                   FALFLUX.18
     +                  VFALL(I,K+1)*Q(I,K+1))/(CS(I,K)*DZ(I,K))                 FALFLUX.19
   20     CONTINUE                                                               FALFLUX.20
          DWFLUX(I,KTOT)=-(CS(I,KTOT)*VFALL(I,KTOT)*Q(I,KTOT))/(                 FALFLUX.21
     +                   CS(I,KTOT)*DZ(I,KTOT))                                  FALFLUX.22
          DO 30 K=1,KTOT                                                         FALFLUX.23
            Q(I,K)=Q(I,K)+DWFLUX(I,K)*DTFALL                                     FALFLUX.24
   30     CONTINUE                                                               FALFLUX.25
  200   CONTINUE                                                                 FALFLUX.26
 1000 CONTINUE                                                                   FALFLUX.27
      RETURN                                                                     FALFLUX.28
      END                                                                        FALFLUX.29
                                                                                 FALFLUX.30
