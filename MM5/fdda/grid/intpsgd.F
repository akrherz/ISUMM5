      SUBROUTINE INTPSGD (PSO,PSOTEN,PSOC,PSOD,                                  INTPSGD.1
     1                    FDTIM,IN)                                              INTPSGD.2
C                                                                                INTPSGD.3
C WHEN INT4(IN,1)=1,INTPSGD INTERPOLATES THE OBS PSTAR TO TIME                   INTPSGD.4
C TAU-1,THEN INTERPOLATES THIS CROSS POINT FIELD TO DOT POINTS,                  INTPSGD.5
C AND IS ONLY CALLED OUTSIDE THE "J-LOOP" SINCE IT WORKS ON THE                  INTPSGD.6
C ENTIRE SLAB (2-D FIELD) AT ONE TIME. IF INT4(IN,)=0 FOR TARGET                 INTPSGD.7
C NUDGING, INTPSGD WILL ONLY PERFORM SPATIAL INTERPOLATION.                      INTPSGD.8
#     include <parame.incl>                                                      INTPSGD.9
#     include <param2.incl>                                                      INTPSGD.10
#     include <parfdda.incl>                                                     INTPSGD.11
#     include <various.incl>                                                     INTPSGD.12
      DIMENSION  PSO(MIX,MJX),PSOTEN(MIX,MJX),                                   INTPSGD.13
     1           PSOC(MIX,MJX),PSOD(MIX,MJX)                                     INTPSGD.14
C JLX IS JL-1, ILX IS IL-1 (IE., THE CROSS POINT DIMS)                           INTPSGD.15
      IF(IN.EQ.1)THEN                                                            INTPSGD.16
        NPF=NPFG                                                                 INTPSGD.17
      ELSE                                                                       INTPSGD.18
        FNPF=IRATIO**LEVIDN(IN)                                                  INTPSGD.19
        NPF=FNPF*NPFG                                                            INTPSGD.20
      ENDIF                                                                      INTPSGD.21
      DO 1 J=1,JLX                                                               INTPSGD.22
        DO 1 I=1,ILX                                                             INTPSGD.23
          PSOC(I,J)=PSO(I,J)+FLOAT(INT4(IN,1))*PSOTEN(I,J)*FDTIM                 INTPSGD.24
    1   CONTINUE                                                                 INTPSGD.25
                                                                                 INTPSGD.26
C CALCULATE OBS PSTAR ON DOT POINTS, EXCEPT AROUND THE                           INTPSGD.27
C LATERAL BOUNDARIES WHERE TEND IS NOT CALLED                                    INTPSGD.28
C EXCEPT FOR THE PBL TENDENCIES...                                               INTPSGD.29
#ifdef MPP1                                                                      INTPSGD.30
#  include <mpp_intpsgd_10.incl>                                                 INTPSGD.31
#endif                                                                           INTPSGD.32
      CALL DOTS(PSOC,PSOD,IL,JL,MIX,MJX)                                         INTPSGD.33
      IF(MOD(KTAU,NPF).EQ.0)THEN                                                 INTPSGD.34
        PRINT554,IN                                                              INTPSGD.35
554   FORMAT(38X,'*** GRID FDDA DIAGNOSTICS FOR INEST=',I2,' AT ',               INTPSGD.36
     1'I=10, J=10 ***')                                                          INTPSGD.37
        PRINT555,IN,KTAU,INT4(IN,1),PSO(10,10),PSOTEN(10,10),PSOC(10,10)         INTPSGD.38
     +           ,PSOD(10,10),FDTIM                                              INTPSGD.39
555   FORMAT(1X,'IN INTPSGD: IN,KTAU,INT4,PSO,PSOTEN,PSOC,PSOD,FDTIM: ',         INTPSGD.40
     1I1,1X,I4,1X,I1,5G10.3)                                                     INTPSGD.41
      ENDIF                                                                      INTPSGD.42
      RETURN                                                                     INTPSGD.43
      END                                                                        INTPSGD.44
                                                                                 INTPSGD.45
