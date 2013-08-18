      SUBROUTINE ADDRX1C(IARR)                                                   ADDRX1C.1
#include <parame.incl>                                                           ADDRX1C.2
#include <point3d.incl>                                                          ADDRX1C.3
#include <point2d.incl>                                                          ADDRX1C.4
#include <pointbc.incl>                                                          ADDRX1C.5
#include <various.incl>                                                          ADDRX1C.6
#if defined (DEC_ALPHA) || defined (crayx1) 
      INTEGER*8 INC1,INC2,INC3,INC4,INC5,INC6,INC7,INC8,INC9,                    ADDRX1C.8
     1    INC10,INC11,INC12,INC13,INC14,INC15,INC16,INC17,INC18,                 ADDRX1C.9
     2    INC19,INC20,INC21,INC22,INC23,INC24,INC25,INC26,INC27                  ADDRX1C.10
      INTEGER*8 IDUDU1, IDUDU2, IDUDU3,                                          ADDRX1C.11
     1          IDUDU4,IDUDU5,IDUDU7,                                            ADDRX1C.12
     2          IDUDU8,IDUDU9,IDUD10,IDUD11,IDUD12,                              ADDRX1C.13
     3          IDUD13,IDUD14,IDUD15,IDUD16,IDUDU6A,                             ADDRX1C.14
     4          IDUDU6B,IDUDU16,IDUDUTKE,IDUDUTKE2,IDUDUFOG                      ADDRX1C.15
      INTEGER*8 IARR                                                             ADDRX1C.16
#endif                                                                           ADDRX1C.17
      COMMON/ADDRNC/ INC1,INC2,INC3,INC4,INC5,INC6,INC7,INC8,INC9,               ADDRX1C.18
     1    INC10,INC11,INC12,INC13,INC14,INC15,INC16,INC17,INC18,                 ADDRX1C.19
     2    INC19,INC20,INC21,INC22,INC23,INC24,INC25,INC26,INC27                  ADDRX1C.20
#include <addrcu.incl>                                                           ADDRX1C.21
#include <addras.incl>                                                           ADDRX1C.22
#include <fddagd.incl>                                                           ADDRX1C.23
#include <fddaob.incl>                                                           ADDRX1C.24
#include <navypb.incl>                                                           ADDRX1C.25
#include <nonhyd.incl>                                                           ADDRX1C.26
#include <nonhydb.incl>                                                          ADDRX1C.27
#include <nhcnst.incl>                                                           ADDRX1C.28
#include <soil.incl>                                                             ADDRX1C.29
#include <soilp.incl>                                                            ADDRX1C.30
#include <radiat.incl>                                                           ADDRX1C.31
#include <rpstar.incl>                                                           ADDRX1C.32
#include <uprad.incl>                                                            ADDRX1C.33
#include <pbltke.incl>                                                           ADDRX1C.34
#include <addrfog.incl>                                                          ADDRX1C.35
      PARAMETER (NEWWAT=2)                                                       ADDRX1C.36
      DIMENSION IDUDU1(NVARX+NVARMX+4+2*NEWWAT),IDUDU2(NVARSX),                  ADDRX1C.37
     1  IDUDU3(88+8*NEWWAT),                                                     ADDRX1C.38
     1         IDUDU4(NVARI+NVARP),IDUDU5(27),IDUDU7(9),                         ADDRX1C.39
     2         IDUDU8(14),IDUDU9(16),IDUD10(7),IDUD11(4),IDUD12(16),             ADDRX1C.40
     3         IDUD13(4),IDUD14(9),IDUD15(2),IDUDU6A(20),                        05DEC01.686
     4         IDUDU6B(4),IDUDU16(14),IDUD16(43)                                 19DEC02.1356
       DIMENSION IDUDUTKE(2),IDUDUTKE2(1),IDUDUFOG(2)                            ADDRX1C.43
       DIMENSION IARR(NUMVAR)                                                    ADDRX1C.44
       EQUIVALENCE (IDUDU1(1),IAUA)                                              ADDRX1C.45
       EQUIVALENCE (IDUDU2(1),IAPA)                                              ADDRX1C.46
       EQUIVALENCE (IDUDU3(1),IAUE)                                              ADDRX1C.47
       EQUIVALENCE (IDUDU4(1),IAIL)                                              ADDRX1C.48
       EQUIVALENCE (IDUDU5(1),INC1)                                              ADDRX1C.49
       EQUIVALENCE (IDUDU7(1),IFUBO)                                             ADDRX1C.50
       EQUIVALENCE (IDUDU8(1),IFPSO)                                             ADDRX1C.51
       EQUIVALENCE (IDUDU9(1),IFSFCO)                                            ADDRX1C.52
       EQUIVALENCE (IDUD10(1),IFSVWT)                                            ADDRX1C.53
       EQUIVALENCE (IDUD11(1),INHWA)                                             ADDRX1C.54
       EQUIVALENCE (IDUD12(1),INHWE)                                             ADDRX1C.55
       EQUIVALENCE (IDUD13(1),INHT0)                                             ADDRX1C.56
       EQUIVALENCE (IDUD14(1),IATURK)                                            ADDRX1C.57
       EQUIVALENCE (IDUD15(1),ITSLA)                                             ADDRX1C.58
       EQUIVALENCE (IDUDU16(1),IAPDA)                                            ADDRX1C.59
       EQUIVALENCE (IDUD16(1),IASFC)                                             ADDRX1C.60
       EQUIVALENCE (IDUDU6A(1),IADTDT)                                           ADDRX1C.61
       EQUIVALENCE (IDUDU6B(1),IAOUTT)                                           ADDRX1C.62
       EQUIVALENCE (IDUDUTKE(1),ITKEA)                                           ADDRX1C.63
       EQUIVALENCE (IDUDUTKE2(1),IMSTEPSS)                                       ADDRX1C.64
       EQUIVALENCE (IDUDUFOG(1),IFGSW)                                           ADDRX1C.65
C                                                                                ADDRX1C.66
C      PRINT *,'CALLING ADDRXC, IAUA = ',IAUA                                    ADDRX1C.67
C                                                                                ADDRX1C.68
      DO 10 N=1,NVARX+NVARMX+4+2*NEWWAT                                          ADDRX1C.69
        NN=N                                                                     ADDRX1C.70
        IDUDU1(N)=IARR(NN)                                                       ADDRX1C.71
   10 CONTINUE                                                                   ADDRX1C.72
      NM=NN                                                                      ADDRX1C.73
      DO 20 N=1,NVARSX                                                           ADDRX1C.74
        NN=NM+N                                                                  ADDRX1C.75
        IDUDU2(N)=IARR(NN)                                                       ADDRX1C.76
   20 CONTINUE                                                                   ADDRX1C.77
      NM=NN                                                                      ADDRX1C.78
      DO 30 N=1,88+8*NEWWAT                                                      ADDRX1C.79
        NN=NM+N                                                                  ADDRX1C.80
        IDUDU3(N)=IARR(NN)                                                       ADDRX1C.81
   30 CONTINUE                                                                   ADDRX1C.82
      NM=NN                                                                      ADDRX1C.83
      DO 40 N=1,NVARI+NVARP                                                      ADDRX1C.84
        NN=NM+N                                                                  ADDRX1C.85
        IDUDU4(N)=IARR(NN)                                                       ADDRX1C.86
   40 CONTINUE                                                                   ADDRX1C.87
      NM=NN                                                                      ADDRX1C.88
      DO 50 N=1,27                                                               ADDRX1C.89
        NN=NM+N                                                                  ADDRX1C.90
        IDUDU5(N)=IARR(NN)                                                       ADDRX1C.91
   50 CONTINUE                                                                   ADDRX1C.92
      NM=NN                                                                      ADDRX1C.93
      DO 65 N=1,20                                                               05DEC01.688
        NN=NM+N                                                                  ADDRX1C.95
        IDUDU6A(N)=IARR(NN)                                                      ADDRX1C.96
   65 CONTINUE                                                                   ADDRX1C.97
      NM=NN                                                                      ADDRX1C.98
      DO 66 N=1,4                                                                ADDRX1C.99
        NN=NM+N                                                                  ADDRX1C.100
        IDUDU6B(N)=IARR(NN)                                                      ADDRX1C.101
   66 CONTINUE                                                                   ADDRX1C.102
      NM=NN                                                                      ADDRX1C.103
      DO 70 N=1,9                                                                ADDRX1C.104
        NN=NM+N                                                                  ADDRX1C.105
        IDUDU7(N)=IARR(NN)                                                       ADDRX1C.106
   70 CONTINUE                                                                   ADDRX1C.107
      NM=NN                                                                      ADDRX1C.108
      DO 80 N=1,14                                                               ADDRX1C.109
        NN=NM+N                                                                  ADDRX1C.110
        IDUDU8(N)=IARR(NN)                                                       ADDRX1C.111
   80 CONTINUE                                                                   ADDRX1C.112
      NM=NN                                                                      ADDRX1C.113
      DO 90 N=1,16                                                               ADDRX1C.114
        NN=NM+N                                                                  ADDRX1C.115
        IDUDU9(N)=IARR(NN)                                                       ADDRX1C.116
   90 CONTINUE                                                                   ADDRX1C.117
      NM=NN                                                                      ADDRX1C.118
      DO 100 N=1,7                                                               ADDRX1C.119
        NN=NM+N                                                                  ADDRX1C.120
        IDUD10(N)=IARR(NN)                                                       ADDRX1C.121
  100 CONTINUE                                                                   ADDRX1C.122
      NM=NN                                                                      ADDRX1C.123
      DO 110 N=1,4                                                               ADDRX1C.124
        NN=NM+N                                                                  ADDRX1C.125
        IDUD11(N)=IARR(NN)                                                       ADDRX1C.126
  110 CONTINUE                                                                   ADDRX1C.127
      NM=NN                                                                      ADDRX1C.128
      DO 120 N=1,16                                                              ADDRX1C.129
        NN=NM+N                                                                  ADDRX1C.130
        IDUD12(N)=IARR(NN)                                                       ADDRX1C.131
  120 CONTINUE                                                                   ADDRX1C.132
      NM=NN                                                                      ADDRX1C.133
      DO 130 N=1,4                                                               ADDRX1C.134
        NN=NM+N                                                                  ADDRX1C.135
        IDUD13(N)=IARR(NN)                                                       ADDRX1C.136
  130 CONTINUE                                                                   ADDRX1C.137
      NM=NN                                                                      ADDRX1C.138
      DO 140 N=1,9                                                               ADDRX1C.139
        NN=NM+N                                                                  ADDRX1C.140
        IDUD14(N)=IARR(NN)                                                       ADDRX1C.141
  140 CONTINUE                                                                   ADDRX1C.142
      NM=NN                                                                      ADDRX1C.143
      DO 150 N=1,2                                                               ADDRX1C.144
        NN=NM+N                                                                  ADDRX1C.145
        IDUD15(N)=IARR(NN)                                                       ADDRX1C.146
  150 CONTINUE                                                                   ADDRX1C.147
      NM=NN                                                                      ADDRX1C.148
      DO 160 N=1,14                                                              ADDRX1C.149
        NN=NM+N                                                                  ADDRX1C.150
        IDUDU16(N)=IARR(NN)                                                      ADDRX1C.151
  160 CONTINUE                                                                   ADDRX1C.152
      NM=NN                                                                      ADDRX1C.153
      DO 170 N=1,43                                                              19DEC02.1357
        NN=NM+N                                                                  ADDRX1C.155
        IDUD16(N)=IARR(NN)                                                       ADDRX1C.156
  170 CONTINUE                                                                   ADDRX1C.157
      NM=NN                                                                      ADDRX1C.158
      IRTT=IARR(NN+1)                                                            ADDRX1C.159
      IUPR=IARR(NN+2)                                                            ADDRX1C.160
      NM=NM+2                                                                    ADDRX1C.161
      DO 161 N=1,2                                                               ADDRX1C.162
        NN=NM+N                                                                  ADDRX1C.163
        IDUDUTKE(N)=IARR(NN)                                                     ADDRX1C.164
  161 CONTINUE                                                                   ADDRX1C.165
      NM=NN                                                                      ADDRX1C.166
      NN=NM+1                                                                    ADDRX1C.167
      IDUDUTKE2(1)=IARR(NN)                                                      ADDRX1C.168
      NM=NN                                                                      ADDRX1C.169
      DO 162 N=1,2                                                               ADDRX1C.170
        NN=NM+N                                                                  ADDRX1C.171
        IDUDUFOG(N)=IARR(NN)                                                     ADDRX1C.172
  162 CONTINUE                                                                   ADDRX1C.173
      NM=NN                                                                      ADDRX1C.174
      RETURN                                                                     ADDRX1C.175
      END                                                                        ADDRX1C.176
