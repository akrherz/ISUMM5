      SUBROUTINE NCONVP(T3D,QV3D,PP3D,QVTEN,TTEN,RAINNC,PSA,                     NCONVP.1
     &                  IST,IEN,JST,JEN,DT)                                      NCONVP.2
#     include <parame.incl>                                                      NCONVP.3
#     include <param2.incl>                                                      NCONVP.4
#     include <param3.incl>                                                      NCONVP.5
#     include <pmoist.incl>                                                      NCONVP.6
#     include <rpstar.incl>                                                      NCONVP.7
      DIMENSION QVTEN(MIX,MJX,MKX),TTEN(MIX,MJX,MKX)                             NCONVP.8
      DIMENSION  QV3D(MIX,MJX,MKX), T3D(MIX,MJX,MKX),                            NCONVP.9
     1           PP3D(MIX,MJX,MKX), PSA(MIX,MJX),                                NCONVP.10
     2         RAINNC(MIX,MJX)                                                   NCONVP.11
C                                                                                NCONVP.12
      XXOVRCP=XLV*XLV/(RV*CP)                                                    NCONVP.13
C     FACTOR 0.5 REQUIRED TO ACCOUNT FOR LEAPFROG SCHEME                         NCONVP.14
      HLFHRDOVG=0.5*100./G                                                       NCONVP.15
      XLVOCPDTI=XLVOCP*(1-IFDRY)/DT                                              NCONVP.16
cmic$ do all autoscope                                                           NCONVP.17
c$doacross                                                                       NCONVP.18
c$& share(psb,a,ptop,tb,qvb,rpsa,qvten,tten,rainnc,psa,dsigma,                   NCONVP.19
c$&       t3d,qv3d,pp3d,                                                         NCONVP.20
c$&       xxovrcp,hlfhrdovg,xlvocpdti,ist,jst,ien,jen,                           NCONVP.21
c$&       svpt0,svp1,svp2,svp3,ep2,dt),                                          NCONVP.22
c$& local(i,j,k,ttemp,qtemp,ptemp,e1,es1,dqv,r1,cond)                            NCONVP.23
      DO J=JST,JEN                                                               NCONVP.24
        DO K=1,MKX                                                               NCONVP.25
          DO I=IST,IEN                                                           NCONVP.26
            TTEMP=T3D(I,J,K)+DT*TTEN(I,J,K)*RPSA(I,J)                            NCONVP.27
            QTEMP=QV3D(I,J,K)+DT*QVTEN(I,J,K)*RPSA(I,J)                          NCONVP.28
            PTEMP=A(K)*PSA(I,J)+PTOP+PP3D(I,J,K)*0.001                           NCONVP.29
            E1=SVP1*EXP(SVP2*(TTEMP-SVPT0)/(TTEMP-SVP3))                         NCONVP.30
            ES1=EP2*E1/(PTEMP-E1)                                                NCONVP.31
            DQV=QTEMP-ES1*CONF                                                   NCONVP.32
            IF(DQV.GT.0.0)THEN                                                   NCONVP.33
              R1=1.+XXOVRCP*ES1/(TTEMP*TTEMP)                                    NCONVP.34
              COND=PSA(I,J)*DQV/R1                                               NCONVP.35
              QVTEN(I,J,K)=QVTEN(I,J,K)-COND/DT                                  NCONVP.36
              TTEN(I,J,K)=TTEN(I,J,K)+XLVOCPDTI*COND                             NCONVP.37
              RAINNC(I,J)=RAINNC(I,J)+DSIGMA(K)*COND*HLFHRDOVG                   NCONVP.38
            ENDIF                                                                NCONVP.39
          ENDDO                                                                  NCONVP.40
        ENDDO                                                                    NCONVP.41
      ENDDO                                                                      NCONVP.42
      RETURN                                                                     NCONVP.43
      END                                                                        NCONVP.44
                                                                                 NCONVP.45
