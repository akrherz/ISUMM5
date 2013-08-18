      SUBROUTINE BUFSLGD(J,XOB,XOBTEN,XOBJK,PSTO,FDTIM,NV,                       BUFSLGD.1
     1                   IN)                                                     BUFSLGD.2
C THIS SUBROUTINE INTERPOLATES THE OBSERVED P*U, P*V, P*T, P*QV                  BUFSLGD.3
C TO THE TAU-1 TIME NECESSARY FOR THE NUDGING TERM, DECOUPLES                    BUFSLGD.4
C IT FROM PSTAR FOR THAT J SLICE (J), AND BUFFERS THIS SLICE INTO                BUFSLGD.5
C XOBJK. PSTO IS THE OBS PSTAR, WHERE NV.LT.3 IS FOR DOT POINT DIMENSION         BUFSLGD.6
C (TO DECOUPLE P*U AND P*V), AND NV.GE.3 IS FOR CROSS POINT                      BUFSLGD.7
C DIMENSION (TO DECOUPLE P*T AND P*QV). XOBTEN IS THE  OBS                       BUFSLGD.8
C TENDENCY IN THIS CASE FOR XOB.  IN=1 FOR COARSE MESH, IN=2                     BUFSLGD.9
C FOR THE FINE MESH.                                                             BUFSLGD.10
C   IF INT4=1 AN INTERPOLATION-IN-TIME IS PERFORMED AT TAU-1;                    BUFSLGD.11
C IF INT4=0, THE TARGET VALUES ARE BUFFERED INTO THE N-S                         BUFSLGD.12
C VERTICAL SLICE XOBJK.  THEREFORE, THIS SUBROUTINE MUST BE                      BUFSLGD.13
C CALLED EVERY TIMESTEP FOR EVERY J-SLICE (EXCLUDING THE                         BUFSLGD.14
C LATERAL BOUNDARIES).                                                           BUFSLGD.15
C                                                                                BUFSLGD.16
C                                                                                BUFSLGD.17
#     include <parame.incl>                                                      BUFSLGD.18
#     include <parfdda.incl>                                                     BUFSLGD.19
#     include <param2.incl>                                                      BUFSLGD.20
#     include <various.incl>                                                     BUFSLGD.21
      DIMENSION XOB(MIXFG,MJXFG,MKXFG),XOBTEN(MIXFG,MJXFG,MKXFG),                BUFSLGD.22
#ifndef IBMopt                                                                   23DEC04.2664
     1   XOBJK(MIXFG,MKXFG),PSTO(MIXFG,MJXFG)                                    BUFSLGD.23
#else                                                                            23DEC04.2665
     1   XOBJK(MIXFG,MKXFG),PSTO(MIXFG,MJXFG),pstoi(MIXFG)                       23DEC04.2666
      real xx                                                                    23DEC04.2667
#endif                                                                           23DEC04.2668
      ICUT=0                                                                     BUFSLGD.24
      IF(IN.GT.1)ICUT=1                                                          BUFSLGD.25
      IMAX=ILX                                                                   BUFSLGD.26
#ifdef IBMopt                                                                    23DEC04.2669
#ifdef MPP1                                                                      23DEC04.2670
      nn = ie_x 0 ( IMAX-ICUT  ) - is_x 0 ( 2+ICUT )  +1                         23DEC04.2671
      is = is_x 0 ( 2+ICUT )                                                     23DEC04.2672
      call vsrec(PSTOi(noflic(is)),PSTO(noflic(is),noflic(j)),nn)                23DEC04.2673
#else                                                                            23DEC04.2674
      nn = IMAX-2* ICUT - 1                                                      23DEC04.2675
      is = 2+icut                                                                23DEC04.2676
      call vsrec(PSTOi(is),PSTO(is,j),nn)                                        23DEC04.2677
#endif                                                                           23DEC04.2678
#endif                                                                           23DEC04.2679
      IF(NV.GE.3)IMAX=ILXM                                                       BUFSLGD.27
C     IF(J.EQ.10)THEN                                                            BUFSLGD.28
C        PRINT *,'IN BUFSLGD : XOB,XOBTEN,FDTIM,PSTO,INT,NV '                    BUFSLGD.29
C        PRINT *,XOB(10,10,KL),XOBTEN(10,10,KL),FDTIM,PSTO(10,10),               BUFSLGD.30
C    1           INT4(IN,1),NV                                                   BUFSLGD.31
C     ENDIF                                                                      BUFSLGD.32
C NV IS THE VARIABLE (U=1,V=2,T=3,Q=4,P*=5)                                      BUFSLGD.33
#ifdef IBMopt                                                                    23DEC04.2680
      xx = IN * FDTIM                                                            23DEC04.2681
#endif                                                                           23DEC04.2682
      DO 1 K=1,KL                                                                BUFSLGD.34
        DO 1 I=2+ICUT,IMAX-ICUT                                                  BUFSLGD.35
#ifndef IBMopt                                                                   23DEC04.2683
          XOBJK(I,K)=(XOB(I,J,K)+FLOAT(INT4(IN,1))*XOBTEN(I,J,K)*FDTIM)/         BUFSLGD.36
     +               PSTO(I,J)                                                   BUFSLGD.37
#else                                                                            23DEC04.2684
          XOBJK(I,K)=(XOB(I,J,K)+ xx * XOBTEN(I,J,K)) * PSTOi(I)                 23DEC04.2685
#endif                                                                           23DEC04.2686
    1   CONTINUE                                                                 BUFSLGD.38
                                                                                 BUFSLGD.39
      IF(MOD(KTAU,NPF).EQ.0.AND.J.EQ.10)GOTO 4646                                BUFSLGD.40
      GOTO 4747                                                                  BUFSLGD.41
 4646 CONTINUE                                                                   BUFSLGD.42
      PRINT333,IN,NV,J,KTAU,XOB(10,J,KL),XOBTEN(10,J,KL),PSTO(10,J),             BUFSLGD.43
     +         XOBJK(10,KL),FDTIM                                                BUFSLGD.44
333   FORMAT(1X,'IN BUFSLGD: IN,NV,J,KTAU,XOB,XOBTEN,PSTO,XOBJK,',               BUFSLGD.45
     1'FDTIM: ',I1,1X,I1,1X,I2,1X,I4,1X,5G10.3)                                  BUFSLGD.46
 4747 CONTINUE                                                                   BUFSLGD.47
      RETURN                                                                     BUFSLGD.48
      END                                                                        BUFSLGD.49
                                                                                 BUFSLGD.50
