C                                                                                QSATGD.1
C  X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X         QSATGD.2
C                                                                                QSATGD.3
      SUBROUTINE QSATGD(J,QSATF,T3D,PSB,PP3D,A,PTOP)                             QSATGD.4
C THIS SUBROUTINE COMPUTES QSAT FOR A N-S SLICE FOR LATER USE                    QSATGD.5
C IN  NUDGD.  WHEN NUDGING MOISTURE, THE OBSERVED MIXING RATIO                   QSATGD.6
C MUST BE LESS THAN OR EQUAL TO QSAT IN THE MODEL TO PREVENT                     QSATGD.7
C THE NUDGING TERM FROM BEING A SOURCE TERM FOR PRECIPITATION.                   QSATGD.8
C                                                                                QSATGD.9
C----------------------------------------------------------------------          QSATGD.10
C                                                                                QSATGD.11
#     include <parame.incl>                                                      QSATGD.12
#     include <various.incl>                                                     QSATGD.13
#     include <pmoist.incl>                                                      QSATGD.14
#     include <rpstar.incl>                                                      QSATGD.15
                                                                                 QSATGD.16
      DIMENSION T3D(MIX,MJX,MKX),PSB(MIX,MJX),                                   QSATGD.17
     1A(MKX),QSATF(MIXFG,MKXFG)                                                  QSATGD.18
      DIMENSION PP3D(MIXNH,MJXNH,MKXNH)                                          QSATGD.19
#ifdef IBMopt                                                                    23DEC04.2707
      integer nn,is                                                              23DEC04.2708
      real temp1(MIXFG)                                                          23DEC04.2709
#ifdef MPP1                                                                      23DEC04.2710
#define nf(A) noflic(A) 
#else                                                                            23DEC04.2712
#define nf(A) A        
#endif                                                                           23DEC04.2714
                                                                                 23DEC04.2715
#ifdef MPP1                                                                      23DEC04.2716
      nn = ie_x0(ILX) - is_x0(2) + 1                                             23DEC04.2717
      is = is_x0(2)                                                              23DEC04.2718
#else                                                                            23DEC04.2719
      nn = ILX - 1                                                               23DEC04.2720
      is = 2                                                                     23DEC04.2721
#endif                                                                           23DEC04.2722
#endif                                                                           23DEC04.2723
C                                                                                23DEC04.2724
      DO 10 K=1,KL                                                               QSATGD.20
#ifndef IBMopt                                                                   23DEC04.2725
        DO 10 I=2,ILX                                                            QSATGD.21
          TT=T3D(I,J,K)                                                          QSATGD.22
          CORR=PP3D(I,J,K)*0.001                                                 QSATGD.23
          PP=PSB(I,J)*A(K)+PTOP+CORR                                             QSATGD.24
          ES=SVP1*EXP(SVP2*(TT-SVPT0)/(TT-SVP3))                                 QSATGD.25
          QSATF(I,K)=EP2*ES/(PP-ES)                                              QSATGD.26
#else                                                                            23DEC04.2726
        DO I=2,ILX                                                               23DEC04.2727
          TT=T3D(I,J,K)                                                          23DEC04.2728
          temp1(i) = SVP2*(TT-SVPT0)/(TT-SVP3)                                   23DEC04.2729
        ENDDO                                                                    23DEC04.2730
                                                                                 23DEC04.2731
        call vsexp(temp1(nf(is)),temp1(nf(is)),nn)                               23DEC04.2732
                                                                                 23DEC04.2733
        DO I=2,ILX                                                               23DEC04.2734
          CORR=PP3D(I,J,K)*0.001                                                 23DEC04.2735
          PP=PSB(I,J)*A(K)+PTOP+CORR                                             23DEC04.2736
          ES=SVP1*temp1(i)                                                       23DEC04.2737
          QSATF(I,K)=EP2*ES/(PP-ES)                                              23DEC04.2738
        ENDDO                                                                    23DEC04.2739
#endif                                                                           23DEC04.2740
                                                                                 23DEC04.2741
   10  CONTINUE                                                                  23DEC04.2742
                                                                                 QSATGD.28
      RETURN                                                                     QSATGD.29
      END                                                                        QSATGD.30
