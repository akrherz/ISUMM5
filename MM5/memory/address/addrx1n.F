      SUBROUTINE ADDRX1N(IARR)                                                   ADDRX1N.1
#include <parame.incl>                                                           ADDRX1N.2
#include <point3dn.incl>                                                         ADDRX1N.3
#include <point2dn.incl>                                                         ADDRX1N.4
#include <pointbcn.incl>                                                         ADDRX1N.5
#include <variousn.incl>                                                         ADDRX1N.6
#include <nestl.incl>                                                            ADDRX1N.7
#if defined (DEC_ALPHA) || defined (crayx1)  
      INTEGER*8 IGUBO,IGVBO,IGUBT,IGVBT,IGTBO,IGTBT,IGQBO,IGQBT,                 ADDRX1N.9
     1          IGVOR                                                            ADDRX1N.10
      INTEGER*8 IGPSO,IGPST,IGPSC,IGPSD,IGPSB,IGWDT,IGWCS,                       ADDRX1N.11
     1          IGWQ,IGDMI,IGDMS,IGIPR,IGIPS,IGMPD,IGMPX                         ADDRX1N.12
      INTEGER*8 IGSFCO,IGBLWN,IGBLWT,IGBLWS,IGBLPC,                              ADDRX1N.13
     1          IGBLPD,IGWXY,IGSFT,IGTIB,IGTIE,IGNTB,IGNTE,                      ADDRX1N.14
     2          IGIQC,IGIDD,IGIDC,IGIDH                                          ADDRX1N.15
      INTEGER*8 IGSVWT,IGVAR,IGRIO,IGRJO,IGRKO,IGTIM,IGERR                       ADDRX1N.16
      INTEGER*8 INRTT                                                            ADDRX1N.17
      INTEGER*8 INUPR                                                            ADDRX1N.18
      INTEGER*8 IDUDU1,IDUDU2, IDUDU3,                                           ADDRX1N.19
     1          IDUDU4,IDUDU5,IDUDU7,                                            ADDRX1N.20
     2          IDUDU8,IDUDU9,IDUD10,IDUD11,IDUD12,                              ADDRX1N.21
     3          IDUD13,IDUD14,IDUD15,IDUD16,IDUDU6A,                             ADDRX1N.22
     4          IDUDU6B,IDUDU16,IDUDUTKE,IDUDUTKE2,IDUDUFOG                      ADDRX1N.23
      INTEGER*8 IARR                                                             ADDRX1N.24
#endif                                                                           ADDRX1N.25
#include <addrcun.incl>                                                          ADDRX1N.26
#include <addrasn.incl>                                                          ADDRX1N.27
      COMMON/ADDRN5/IGUBO,IGVBO,IGUBT,IGVBT,IGTBO,IGTBT,IGQBO,IGQBT,             ADDRX1N.28
     1     IGVOR                                                                 ADDRX1N.29
      COMMON/ADDRN6/ IGPSO,IGPST,IGPSC,IGPSD,IGPSB,IGWDT,IGWCS,                  ADDRX1N.30
     1       IGWQ,IGDMI,IGDMS,IGIPR,IGIPS,IGMPD,IGMPX                            ADDRX1N.31
      COMMON/ADDRN7/ IGSFCO,IGBLWN,IGBLWT,IGBLWS,IGBLPC,                         ADDRX1N.32
     1      IGBLPD,IGWXY,IGSFT,IGTIB,IGTIE,IGNTB,IGNTE,                          ADDRX1N.33
     2      IGIQC,IGIDD,IGIDC,IGIDH                                              ADDRX1N.34
      COMMON /ADDRN8/IGSVWT,IGVAR,IGRIO,IGRJO,IGRKO,IGTIM,IGERR                  ADDRX1N.35
#include <pnavyn.incl>                                                           ADDRX1N.36
#include <nnnhyd.incl>                                                           ADDRX1N.37
#include <nnnhydb.incl>                                                          ADDRX1N.38
#include <nncnst.incl>                                                           ADDRX1N.39
#include <rpstarn.incl>                                                          ADDRX1N.40
#include <soiln.incl>                                                            ADDRX1N.41
#include <soilpn.incl>                                                           ADDRX1N.42
#include <pbltken.incl>                                                          ADDRX1N.43
#include <addrfogn.incl>                                                         ADDRX1N.44
      COMMON /RADIATN/INRTT                                                      ADDRX1N.45
      COMMON /UPRADN/INUPR                                                       ADDRX1N.46
      PARAMETER (NEWWAT=2)                                                       ADDRX1N.47
      DIMENSION IDUDU1(NVARX+NVARMX+4+2*NEWWAT),IDUDU2(NVARSX),                  ADDRX1N.48
     1     IDUDU3(88+8*NEWWAT),                                                  ADDRX1N.49
     1         IDUDU4(NVARI+NVARP),IDUDU5(27),IDUDU7(9),                         ADDRX1N.50
     2         IDUDU8(14),IDUDU9(16),IDUD10(7),IDUD11(4),IDUD12(16),             ADDRX1N.51
     3         IDUD13(4),IDUD14(9),IDUD15(2),IDUDU6A(20),                        05DEC01.690
     4         IDUDU6B(4),IDUDU16(14),IDUD16(43)                                 19DEC02.1358
       DIMENSION IDUDUTKE(2),IDUDUTKE2(1),IDUDUFOG(2)                            ADDRX1N.54
C                                                                                ADDRX1N.55
       DIMENSION IARR(NUMVAR)                                                    ADDRX1N.56
       EQUIVALENCE (IDUDU1(1),INAUA)                                             ADDRX1N.57
       EQUIVALENCE (IDUDU2(1),INAPA)                                             ADDRX1N.58
       EQUIVALENCE (IDUDU3(1),INAUE)                                             ADDRX1N.59
       EQUIVALENCE (IDUDU4(1),INAIL)                                             ADDRX1N.60
       EQUIVALENCE (IDUDU5(1),INN1)                                              ADDRX1N.61
       EQUIVALENCE (IDUDU7(1),IGUBO)                                             ADDRX1N.62
       EQUIVALENCE (IDUDU8(1),IGPSO)                                             ADDRX1N.63
       EQUIVALENCE (IDUDU9(1),IGSFCO)                                            ADDRX1N.64
       EQUIVALENCE (IDUD10(1),IGSVWT)                                            ADDRX1N.65
       EQUIVALENCE (IDUD11(1),INNWA)                                             ADDRX1N.66
       EQUIVALENCE (IDUD12(1),INNWE)                                             ADDRX1N.67
       EQUIVALENCE (IDUD13(1),INNT0)                                             ADDRX1N.68
       EQUIVALENCE (IDUD14(1),INTURK)                                            ADDRX1N.69
       EQUIVALENCE (IDUD15(1),ITSLAN)                                            ADDRX1N.70
       EQUIVALENCE (IDUD16(1),INSFC)                                             ADDRX1N.71
       EQUIVALENCE (IDUDU6A(1),INDTDT)                                           ADDRX1N.72
       EQUIVALENCE (IDUDU6B(1),INOUTT)                                           ADDRX1N.73
       EQUIVALENCE (IDUDU16(1),INAPDA)                                           ADDRX1N.74
       EQUIVALENCE (IDUDUTKE(1),INTKEA)                                          ADDRX1N.75
       EQUIVALENCE (IDUDUTKE2(1),INMSTEPSS)                                      ADDRX1N.76
       EQUIVALENCE (IDUDUFOG(1),INFGSW)                                          ADDRX1N.77
C                                                                                ADDRX1N.78
C      PRINT *,'CALLING ADDRXN, INAUA = ',INAUA                                  ADDRX1N.79
C                                                                                ADDRX1N.80
      DO 10 N=1,NVARX+NVARMX+4+2*NEWWAT                                          ADDRX1N.81
        NN=N                                                                     ADDRX1N.82
        IDUDU1(N)=IARR(NN)                                                       ADDRX1N.83
   10 CONTINUE                                                                   ADDRX1N.84
      NM=NN                                                                      ADDRX1N.85
      DO 20 N=1,NVARSX                                                           ADDRX1N.86
        NN=NM+N                                                                  ADDRX1N.87
        IDUDU2(N)=IARR(NN)                                                       ADDRX1N.88
   20 CONTINUE                                                                   ADDRX1N.89
      NM=NN                                                                      ADDRX1N.90
      DO 30 N=1,88+8*NEWWAT                                                      ADDRX1N.91
        NN=NM+N                                                                  ADDRX1N.92
        IDUDU3(N)=IARR(NN)                                                       ADDRX1N.93
   30 CONTINUE                                                                   ADDRX1N.94
      NM=NN                                                                      ADDRX1N.95
      DO 40 N=1,NVARI+NVARP                                                      ADDRX1N.96
        NN=NM+N                                                                  ADDRX1N.97
        IDUDU4(N)=IARR(NN)                                                       ADDRX1N.98
   40 CONTINUE                                                                   ADDRX1N.99
      NM=NN                                                                      ADDRX1N.100
      DO 50 N=1,27                                                               ADDRX1N.101
        NN=NM+N                                                                  ADDRX1N.102
        IDUDU5(N)=IARR(NN)                                                       ADDRX1N.103
   50 CONTINUE                                                                   ADDRX1N.104
      NM=NN                                                                      ADDRX1N.105
      DO 65 N=1,20                                                               05DEC01.692
        NN=NM+N                                                                  ADDRX1N.107
        IDUDU6A(N)=IARR(NN)                                                      ADDRX1N.108
   65 CONTINUE                                                                   ADDRX1N.109
      NM=NN                                                                      ADDRX1N.110
      DO 66 N=1,4                                                                ADDRX1N.111
        NN=NM+N                                                                  ADDRX1N.112
        IDUDU6B(N)=IARR(NN)                                                      ADDRX1N.113
   66 CONTINUE                                                                   ADDRX1N.114
      NM=NN                                                                      ADDRX1N.115
      DO 70 N=1,9                                                                ADDRX1N.116
        NN=NM+N                                                                  ADDRX1N.117
        IDUDU7(N)=IARR(NN)                                                       ADDRX1N.118
   70 CONTINUE                                                                   ADDRX1N.119
      NM=NN                                                                      ADDRX1N.120
      DO 80 N=1,14                                                               ADDRX1N.121
        NN=NM+N                                                                  ADDRX1N.122
        IDUDU8(N)=IARR(NN)                                                       ADDRX1N.123
   80 CONTINUE                                                                   ADDRX1N.124
      NM=NN                                                                      ADDRX1N.125
      DO 90 N=1,16                                                               ADDRX1N.126
        NN=NM+N                                                                  ADDRX1N.127
        IDUDU9(N)=IARR(NN)                                                       ADDRX1N.128
   90 CONTINUE                                                                   ADDRX1N.129
      NM=NN                                                                      ADDRX1N.130
      DO 100 N=1,7                                                               ADDRX1N.131
        NN=NM+N                                                                  ADDRX1N.132
        IDUD10(N)=IARR(NN)                                                       ADDRX1N.133
  100 CONTINUE                                                                   ADDRX1N.134
      NM=NN                                                                      ADDRX1N.135
      DO 110 N=1,4                                                               ADDRX1N.136
        NN=NM+N                                                                  ADDRX1N.137
        IDUD11(N)=IARR(NN)                                                       ADDRX1N.138
  110 CONTINUE                                                                   ADDRX1N.139
      NM=NN                                                                      ADDRX1N.140
      DO 120 N=1,16                                                              ADDRX1N.141
        NN=NM+N                                                                  ADDRX1N.142
        IDUD12(N)=IARR(NN)                                                       ADDRX1N.143
  120 CONTINUE                                                                   ADDRX1N.144
      NM=NN                                                                      ADDRX1N.145
      DO 130 N=1,4                                                               ADDRX1N.146
        NN=NM+N                                                                  ADDRX1N.147
        IDUD13(N)=IARR(NN)                                                       ADDRX1N.148
  130 CONTINUE                                                                   ADDRX1N.149
      NM=NN                                                                      ADDRX1N.150
      DO 140 N=1,9                                                               ADDRX1N.151
        NN=NM+N                                                                  ADDRX1N.152
        IDUD14(N)=IARR(NN)                                                       ADDRX1N.153
  140 CONTINUE                                                                   ADDRX1N.154
      NM=NN                                                                      ADDRX1N.155
      DO 150 N=1,2                                                               ADDRX1N.156
        NN=NM+N                                                                  ADDRX1N.157
        IDUD15(N)=IARR(NN)                                                       ADDRX1N.158
  150 CONTINUE                                                                   ADDRX1N.159
      NM=NN                                                                      ADDRX1N.160
      DO 160 N=1,14                                                              ADDRX1N.161
        NN=NM+N                                                                  ADDRX1N.162
        IDUDU16(N)=IARR(NN)                                                      ADDRX1N.163
  160 CONTINUE                                                                   ADDRX1N.164
      NM=NN                                                                      ADDRX1N.165
      DO 170 N=1,43                                                              19DEC02.1359
        NN=NM+N                                                                  ADDRX1N.167
        IDUD16(N)=IARR(NN)                                                       ADDRX1N.168
  170 CONTINUE                                                                   ADDRX1N.169
      NM=NN                                                                      ADDRX1N.170
      INRTT=IARR(NN+1)                                                           ADDRX1N.171
      INUPR=IARR(NN+2)                                                           ADDRX1N.172
      NM=NM+2                                                                    ADDRX1N.173
      DO 161 N=1,2                                                               ADDRX1N.174
        NN=NM+N                                                                  ADDRX1N.175
        IDUDUTKE(N)=IARR(NN)                                                     ADDRX1N.176
  161 CONTINUE                                                                   ADDRX1N.177
      NM=NN                                                                      ADDRX1N.178
      NN=NM+1                                                                    ADDRX1N.179
      IDUDUTKE2(1)=IARR(NN)                                                      ADDRX1N.180
      NM=NN                                                                      ADDRX1N.181
      DO 162 N=1,2                                                               ADDRX1N.182
        NN=NM+N                                                                  ADDRX1N.183
        IDUDUFOG(N)=IARR(NN)                                                     ADDRX1N.184
  162 CONTINUE                                                                   ADDRX1N.185
      NM=NN                                                                      ADDRX1N.186
      RETURN                                                                     ADDRX1N.187
      END                                                                        ADDRX1N.188
