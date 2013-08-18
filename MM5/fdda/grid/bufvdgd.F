      SUBROUTINE BUFVDGD(UOB,VOB,UOBTEN,VOBTEN,                                  BUFVDGD.1
     1UB,VB,PSTO,FDTIM,MSD,MSX,VORDIF,PSTF,IN)                                   BUFVDGD.2
C THIS SUBROUTINE COMPUTES THE OBS AND FCST VORTICITY OVER                       BUFVDGD.3
C THE GRID ON CROSS POINTS, AND COMPUTES THE DIFFERENCE                          BUFVDGD.4
C (VORDIF) FOR USE IN NUDGD WHEN ID=2 (ROTATIONAL WIND                           BUFVDGD.5
C NUDGING). THIS SUBROUTINE IS CALLED ONCE PER TIME STEP                         BUFVDGD.6
C FOR EACH MESH (INEST=1 AND/OR 2).                                              BUFVDGD.7
#     include <parame.incl>                                                      BUFVDGD.8
#     include <param2.incl>                                                      BUFVDGD.9
#     include <parfdda.incl>                                                     BUFVDGD.10
#     include <various.incl>                                                     BUFVDGD.11
      DIMENSION UOB(MIXFG,MJXFG,MKXFG),   VOB(MIXFG,MJXFG,MKXFG),                BUFVDGD.12
     1          UOBTEN(MIXFG,MJXFG,MKXFG),VOBTEN(MIXFG,MJXFG,MKXFG),             BUFVDGD.13
     2          VORDIF(MIXFG,MJXFG,MKXFG),                                       BUFVDGD.14
     3          UJ(MIXFG,MKXFG),VJ(MIXFG,MKXFG),UJP1(MIXFG,MKXFG),               BUFVDGD.15
     4          VJP1(MIXFG,MKXFG),VOROB(MIXFG,MKXFG),VORFC(MIXFG,MKXFG),         BUFVDGD.16
     5          PSTF(MIXFG,MJXFG),PSTO(MIXFG,MJXFG),UB(MIX,MJX,MKX),             BUFVDGD.17
     6          VB(MIX,MJX,MKX)                                                  BUFVDGD.18
      REAL MSD(MIXFG,MJXFG),MSX(MIXFG,MJXFG)                                     BUFVDGD.19
C_FLIC_RUNPAD(8)                                                                 07NOV00.1110
C DECOUPLE OBS P*U AND P*V AND IF INT4=1 (INTERPOLATION IN TIME),                BUFVDGD.21
C CALCULATE U AND V AT FDTIM (TAU-1)                                             BUFVDGD.22
      DO 10 J=1,JLX                                                              BUFVDGD.23
C                                                                                BUFVDGD.24
        DO 20 K=1,KL                                                             BUFVDGD.25
          DO 20 I=1,IL                                                           BUFVDGD.26
            UJ(I,K)=(UOB(I,J,K)+UOBTEN(I,J,K)*FDTIM*FLOAT(INT4(IN,1)))/          BUFVDGD.27
     +              PSTO(I,J)                                                    BUFVDGD.28
            VJ(I,K)=(VOB(I,J,K)+VOBTEN(I,J,K)*FDTIM*FLOAT(INT4(IN,1)))/          BUFVDGD.29
     +              PSTO(I,J)                                                    BUFVDGD.30
            UJP1(I,K)=(UOB(I,J+1,K)+UOBTEN(I,J+1,K)*FDTIM*                       BUFVDGD.31
     +                FLOAT(INT4(IN,1)))/PSTO(I,J+1)                             BUFVDGD.32
            VJP1(I,K)=(VOB(I,J+1,K)+VOBTEN(I,J+1,K)*FDTIM*                       BUFVDGD.33
     +                FLOAT(INT4(IN,1)))/PSTO(I,J+1)                             BUFVDGD.34
   20     CONTINUE                                                               BUFVDGD.35
                                                                                 BUFVDGD.36
        DO 21 K=1,KL                                                             BUFVDGD.37
          DO 21 I=1,ILX                                                          BUFVDGD.38
            VOROB(I,K)=MSX(I,J)*MSX(I,J)*((VJP1(I+1,K)/MSD(I+1,J+1)+             BUFVDGD.39
     +                 VJP1(I,K)/MSD(I,J+1)-VJ(I+1,K)/MSD(I+1,J)-VJ(I,K)         BUFVDGD.40
     +                 /MSD(I,J))-(UJP1(I+1,K)/MSD(I+1,J+1)+UJ(I+1,K)/           BUFVDGD.41
     +                 MSD(I+1,J)-UJP1(I,K)/MSD(I,J+1)-UJ(I,K)/MSD(I,J))         BUFVDGD.42
     +                 )*0.5/DX                                                  BUFVDGD.43
   21     CONTINUE                                                               BUFVDGD.44
                                                                                 BUFVDGD.45
C                                                                                BUFVDGD.46
C DECOUPLE FCST P*U AND P*V                                                      BUFVDGD.47
        DO 22 K=1,KL                                                             BUFVDGD.48
          DO 22 I=1,IL                                                           BUFVDGD.49
            UJ(I,K)=UB(I,J,K)/PSTF(I,J)                                          BUFVDGD.50
            VJ(I,K)=VB(I,J,K)/PSTF(I,J)                                          BUFVDGD.51
            UJP1(I,K)=UB(I,J+1,K)/PSTF(I,J+1)                                    BUFVDGD.52
            VJP1(I,K)=VB(I,J+1,K)/PSTF(I,J+1)                                    BUFVDGD.53
   22     CONTINUE                                                               BUFVDGD.54
                                                                                 BUFVDGD.55
        DO 23 K=1,KL                                                             BUFVDGD.56
          DO 23 I=1,ILX                                                          BUFVDGD.57
            VORFC(I,K)=MSX(I,J)*MSX(I,J)*((VJP1(I+1,K)/MSD(I+1,J+1)+             BUFVDGD.58
     +                 VJP1(I,K)/MSD(I,J+1)-VJ(I+1,K)/MSD(I+1,J)-VJ(I,K)         BUFVDGD.59
     +                 /MSD(I,J))-(UJP1(I+1,K)/MSD(I+1,J+1)+UJ(I+1,K)/           BUFVDGD.60
     +                 MSD(I+1,J)-UJP1(I,K)/MSD(I,J+1)-UJ(I,K)/MSD(I,J))         BUFVDGD.61
     +                 )*0.5/DX                                                  BUFVDGD.62
            VORDIF(I,J,K)=VOROB(I,K)-VORFC(I,K)                                  BUFVDGD.63
   23     CONTINUE                                                               BUFVDGD.64
                                                                                 BUFVDGD.65
C                                                                                BUFVDGD.66
        IF(J.EQ.2.AND.MOD(KTAU,NPF).EQ.0)PRINT108,IN,J,KTAU,VOROB(2,2),          BUFVDGD.67
     +    VORFC(2,2),VORDIF(2,J,2)                                               BUFVDGD.68
108   FORMAT(1X,'IN BUFVDGD: IN,J,TAU,VOROB,VORFC,VORDIF',3I5,3E15.5)            BUFVDGD.69
   10 CONTINUE                                                                   BUFVDGD.70
      RETURN                                                                     BUFVDGD.71
      END                                                                        BUFVDGD.72
                                                                                 BUFVDGD.73
