      PROGRAM MM5                                                                MM5.1
      IMPLICIT NONE                                                              MM5.2
C======================================================================C         MM5.3
C                                NOTICE                                C         MM5.4
C======================================================================C         MM5.5
C                                                                      C         MM5.6
C MM5 was developed in cooperation with The Pennsylvania State         C         MM5.7
C University (Penn State) and the University Corporation for           C         MM5.8
C Atmospheric Research (UCAR).                                         C         MM5.9
C                                                                      C         MM5.10
C Penn State and UCAR make no proprietary claims, either statutory or  C         MM5.11
C otherwise, to this version and release of MM5 and consider MM5 to be C         MM5.12
C in the public domain for use by any person or entity for any purpose C         MM5.13
C without any fee or charge. Penn State and UCAR request that any MM5  C         MM5.14
C user include this notice on any partial or full copies of MM5. Penn  C         MM5.15
C State and UCAR further request that the user credit Penn State and   C         MM5.16
C UCAR in any publications that result from the use of MM5. The names  C         MM5.17
C Penn State and UCAR shall not be used or referenced in any           C         MM5.18
C advertising or publicity which endorses or promotes any products or  C         MM5.19
C commercial entity associated with or using MM5, or any derivative    C         MM5.20
C works thereof, without the written authorization of UCAR and Penn    C         MM5.21
C State.                                                               C         MM5.22
C                                                                      C         MM5.23
C MM5 is provided by Penn State and UCAR on an "AS IS" basis and any   C         MM5.24
C warranties, either express or implied, including but not limited to  C         MM5.25
C implied warranties of noninfringement, originality, merchantability  C         MM5.26
C and fitness for a particular purpose, are disclaimed. Neither UCAR   C         MM5.27
C nor Penn State will be obligated to provide the user with any        C         MM5.28
C support, consulting, training or assistance of any kind with regard  C         MM5.29
C to the use, operation and performance of MM5 nor to provide the      C         MM5.30
C user with any updates, revisions, new versions, error corrections    C         MM5.31
C or "bug" fixes. In no event will UCAR and Penn State be liable for   C         MM5.32
C any damages, whatsoever, whether direct, indirect, consequential or  C         MM5.33
C special, which may result from an action in contract, negligence or  C         MM5.34
C other claim that arises out of or in connection with the access, use C         MM5.35
C or performance of MM5, including infringement actions.               C         MM5.36
C                                                                      C         MM5.37
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         MM5.38
C                                                                      C         MM5.39
C                                                                      C         MM5.40
C             THIS IS THE VERSION 3 RELEASE OF MM5.                    C         MM5.41
C                                                                      C         MM5.42
C             THIS F77 PROGRAM IS PORTABLE TO MOST PLATFORMS           C         MM5.43
C             AND MULTI-TASKS WITH CRAY, SGI AND OPENMP PARALLEL       C         MM5.44
C             DIRECTIVES.                                              C         MM5.45
C             IT CONTAINS CPP PRE-COMPILATION DIRECTIVES.              C         MM5.46
C                                                                      C         MM5.47
C             SEND MAIL TO mesouser@ncar.ucar.edu IF YOU FIND BUGS     C         MM5.48
C             OR IF YOU HAVE IMPROVEMENT SUGGESTIONS.                  C         MM5.49
C                                                                      C         MM5.50
C             THE CODE IS FREELY AVAILABLE AND UNDER CONSTANT          C         MM5.51
C             DEVELOPMENT. SEE WEB PAGE FOR MORE INFORMATION           C         MM5.52
C             http://www.mmm.ucar.edu/mm5/mm5-home.html                C         MM5.53
C                                                                      C         MM5.54
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         MM5.55
C                                                                                MM5.56
# include <defines.incl>                                                         MM5.57
#     include <parame.incl>                                                      MM5.58
#     include <param2.incl>                                                      MM5.59
#     include <param3.incl>                                                      MM5.60
#     include <addr0.incl>                                                       MM5.61
#     include <various.incl>                                                     MM5.62
#     include <dusolve1.incl>                                                    MM5.63
#     include <variousn.incl>                                                    MM5.64
#     include <addrcu.incl>                                                      MM5.65
C                                                                                MM5.66
      INTEGER IEXEC(MAXNES)                                                      MM5.67
      INTEGER I,J,L,N,NN                                                         MM5.68
      REAL EXTIME,DTINC                                                          MM5.69
      REAL TLEFT, TLEFTOLD, TMARGIN                                              MM5.70
#ifdef CRAY_TIM                                                                  23DEC04.1
      integer*8 flop_lclk_rate                                                   23DEC04.2
      real*8    f0, f1, flop, f2, f3, ftot                                       23DEC04.3
      integer*8 tick0, tick1, i0, i1, irtc_rate                                  23DEC04.4
      integer*8 tick2, tick3, i2, i3, itot, ttot                                 23DEC04.5
#endif                                                                           23DEC04.6
#ifdef MPP1                                                                      MM5.71
#  include <mpp_mm5_00.incl>                                                     MM5.72
#endif                                                                           MM5.73
#ifdef CRAY_TIM                                                                  23DEC04.7
       ttot = 0                                                                  23DEC04.8
       itot = 0                                                                  23DEC04.9
       ftot = 0.0                                                                23DEC04.10
       f0 = flop( tick0 )                                                        23DEC04.11
       i0 = irtc()                                                               23DEC04.12
#endif                                                                           23DEC04.13
C                                                                                MM5.74
C--- INITIALIZE TO ZERO                                                          MM5.75
C                                                                                MM5.76
      DO N=1,MAXNES                                                              MM5.77
        DO 1 I=1,IRHUGE                                                          MM5.78
          ALLARR(I,N)=0.                                                         MM5.79
    1   CONTINUE                                                                 MM5.80
        DO 3 I=1,IIHUGE                                                          MM5.81
          INTALL(I,N)=0                                                          MM5.82
    3   CONTINUE                                                                 MM5.83
        DO I=1,IFGHUG                                                            MM5.84
          ALLFG(I,N)=0.                                                          MM5.85
        ENDDO                                                                    MM5.86
        DO I=1,IFGIHUG                                                           MM5.87
          INTFG(I,N)=0                                                           MM5.88
        ENDDO                                                                    MM5.89
        DO I=1,IFOHUG                                                            MM5.90
          ALLFO(I,N)=0.                                                          MM5.91
        ENDDO                                                                    MM5.92
      ENDDO                                                                      MM5.93
      DO 2 N=1,MAXNES                                                            MM5.94
        DO 2 I=1,MIX                                                             MM5.95
          DO 2 J=1,MJX                                                           MM5.96
            XNUU(I,J,N)=0.                                                       MM5.97
            XMUU(I,J,N)=0.                                                       MM5.98
            XNUT(I,J,N)=0.                                                       MM5.99
            XMUT(I,J,N)=0.                                                       MM5.100
    2     CONTINUE                                                               MM5.101
                                                                                 MM5.102
#ifdef MPP1                                                                      MM5.103
#  include "mpp_mm5_05.incl"                                                     MM5.104
#endif                                                                           MM5.105
#ifdef CRAY                                                                      MM5.106
C    INITIALIZE VALUES FOR CRAY TREMAIN CHECK                                    MM5.107
      TMARGIN=100.                                                               MM5.108
      TLEFTOLD=TMARGIN                                                           MM5.109
#endif                                                                           MM5.110
C                                                                                MM5.111
C--- FIRST THINGS FIRST: PRESET ADDRESS OF ALL VARIABLES                         MM5.112
C--- FOR ALL NESTS.                                                              MM5.113
C                                                                                MM5.114
      NSTTOT=1                                                                   MM5.115
      CALL ADDALL                                                                MM5.116
C                                                                                MM5.117
C--- INITIALIZE NUMBER OF ACTIVE NESTS AND TOTAL POSSIBLE                        MM5.118
C--- NESTS TO ZERO                                                               MM5.119
C                                                                                MM5.120
      DO 4 N=1,MAXNES                                                            MM5.121
        DO 4 NN=1,MAXNES                                                         MM5.122
          NUMLV(N,NN)=0                                                          MM5.123
    4   CONTINUE                                                                 MM5.124
C                                                                                MM5.125
C--- FILL COARSE DOMAIN VARIABLES WITH DOMAIN (1)                                MM5.126
C                                                                                MM5.127
      CALL ADDRX1C(IAXALL(1,1))                                                  MM5.128
C                                                                                MM5.129
C-----SET UP PARAMETERS:                                                         MM5.130
C                                                                                MM5.131
      EXTIME=0.                                                                  MM5.132
      DTINC=0.                                                                   MM5.133
C                                                                                MM5.134
      DO 5 NN=1,MAXNES                                                           MM5.135
        IEXEC(NN)=1                                                              MM5.136
        LEVIDN(NN)=0                                                             MM5.137
        NUMNC(NN)=0                                                              MM5.138
    5 CONTINUE                                                                   MM5.139
      IF(MAXNES.EQ.1)THEN                                                        MM5.140
        PRINT9000                                                                MM5.141
      ELSE                                                                       MM5.142
        PRINT9010                                                                MM5.143
        PRINT9020,MAXNES                                                         MM5.144
      ENDIF                                                                      MM5.145
 9000 FORMAT('***************  ONE DOMAIN ONLY!!!  ***************')             MM5.146
 9010 FORMAT('***************  MULTI LEVEL RUN!!!  ***************')             MM5.147
 9020 FORMAT('***************  ',I3,' DOMAIN TOTAL    ***************')          MM5.148
C                                                                                MM5.149
C--- READ IN PARAMETERS AND NAMELISTS                                            MM5.150
C                                                                                MM5.151
      CALL PARAM(IEXEC)                                                          MM5.152
#ifdef MPP1                                                                      MM5.153
#  include <mpp_mm5_10.incl>                                                     MM5.154
#endif                                                                           MM5.155
C                                                                                MM5.156
C-----READ IN INITIAL DATA, INITIALIZE STARTING NESTS                            MM5.157
C                                                                                MM5.158
      IF(IFREST)THEN                                                             MM5.159
C                                                                                MM5.160
C--- RESTART ?                                                                   MM5.161
C                                                                                MM5.162
        CALL INITSAV(IEXEC)                                                      MM5.163
#ifdef NESTED                                                                    MM5.164
        IF(NSTTOT.LT.MAXNES)THEN                                                 MM5.165
          DO 76 L=2,MAXNES                                                       MM5.166
            CALL CHKNST(IEXEC,L)                                                 MM5.167
   76     CONTINUE                                                               MM5.168
        ENDIF                                                                    MM5.169
#endif                                                                           MM5.170
      ELSE                                                                       MM5.171
C                                                                                MM5.172
C--- INITIALIZE FROM ZERO                                                        MM5.173
C                                                                                MM5.174
#ifdef MPP1                                                                      MM5.175
        CALL MP_INITDOMAIN(1)                                                    MM5.176
        CALL ADDRX1C(IAXALL(1,1))                                                MM5.177
#endif                                                                           MM5.178
        CALL INIT(IEXEC,IL,JL)                                                   MM5.179
      ENDIF                                                                      MM5.180
C                                                                                MM5.181
      PRINT *,'!!!!!!!!!! NON-HYDROSTATIC RUN !!!!!!!!!!'                        MM5.182
#ifdef CRAY_TIM                                                                  23DEC04.14
      print *,'init wall time = ',                                               23DEC04.15
     &         float(irtc()-i0)/float(irtc_rate()),' sec'                        23DEC04.16
#endif                                                                           23DEC04.17
C                                                                                MM5.183
C--- TIMEINTEGRATION FOLLOWS                                                     MM5.184
C                                                                                MM5.185
   10 CONTINUE                                                                   MM5.186
C                                                                                23DEC04.18
#ifdef CRAY_TIM                                                                  23DEC04.19
      f2 = flop( tick2, 0 )                                                      23DEC04.20
      i2 = irtc()                                                                23DEC04.21
#endif                                                                           23DEC04.22
C                                                                                MM5.187
C-----READ IN BOUNDARY CONDITIONS FOR COARSE DOMAIN (1):                         MM5.188
C                                                                                MM5.189
      CALL ADDRX1C(IAXALL(1,1))                                                  MM5.190
#ifdef MPP1                                                                      MM5.191
      CALL MP_INITDOMAIN(1)                                                      MM5.192
#endif                                                                           MM5.193
      IF(XTIME+DTMIN .LT. TIMAX)THEN                                             07NOV00.1
        IF(XTIME.GE.BDYLTIM .AND. ISSTVAR.EQ.1)THEN                              07NOV00.2
          CALL LBDYIN(1,TBDYLBE,BDYLTIM,BDYLFRQ,ISOIL,IACTIV,                    05DEC01.1
     -     IFSNOW,IEXSI,IMOIAV,IL,JL)                                            23DEC04.23
#ifdef NESTED                                                                    07NOV00.4
          IF(NSTTOT.GT.1)THEN                                                    07NOV00.5
            DO L=2,MAXNES                                                        07NOV00.6
            IF(IACTIV(L).EQ.1)THEN                                               07NOV00.7
              CALL ADDRX1C(IAXALL(1,L))                                          07NOV00.8
              CALL LBDYIN(L,TBDYLBE,BDYLTIM,BDYLFRQ,ISOIL,IACTIV,                05DEC01.3
     -          IFSNOW,IEXSI,IMOIAV,IL,JL)                                       23DEC04.24
            ENDIF                                                                07NOV00.10
            ENDDO                                                                07NOV00.11
            CALL ADDRX1C(IAXALL(1,1))                                            07NOV00.12
          ENDIF                                                                  07NOV00.13
#endif                                                                           07NOV00.14
        ENDIF                                                                    07NOV00.15
        IF(XTIME.GE.BDYTIM)CALL BDYIN(9,TBDYBE,BDYTIM,BDYFRQ,IL,JL,              19DEC02.3
     -     IBMOIST)                                                              19DEC02.4
      ENDIF                                                                      07NOV00.17
C                                                                                07NOV00.18
#ifdef NESTED                                                                    07NOV00.19
C                                                                                07NOV00.20
C--- INITIALIZE NEW NESTS?                                                       07NOV00.21
C                                                                                07NOV00.22
      IF(MAXNES.GT.1)THEN                                                        07NOV00.23
        DO 77 L=2,MAXNES                                                         07NOV00.24
          CALL CHKNST(IEXEC,L)                                                   07NOV00.25
   77   CONTINUE                                                                 07NOV00.26
        CALL ADDRX1C(IAXALL(1,1))                                                07NOV00.27
      ENDIF                                                                      07NOV00.28
C                                                                                MM5.195
C--- GET THE BOUNDARY FIELDS FOR DOMAINS ON NEXT NEST LEVEL,                     MM5.196
C--- AT THIS LEVEL THE MOTHER DOMAIN WILL BE 1, THE NEXT LEVEL WILL BE           MM5.197
C--- THE FIRST, SO THIS IS EASY!                                                 MM5.198
C                                                                                MM5.199
      IF(NSTTOT.GT.1)THEN                                                        MM5.201
        DO 17 L=2,MAXNES                                                         MM5.202
          IF(IACTIV(L).EQ.0)GOTO 17                                              MM5.203
          IF(LEVIDN(L).NE.1)GOTO 17                                              MM5.204
          CALL ADDRX1N(IAXALL(1,L))                                              MM5.205
#ifndef MPP1                                                                     MM5.206
          CALL STOTNDI(1,INL,JNL,IL,JL)                                          MM5.207
#else                                                                            MM5.208
          CALL STOTNDI(L,INL,JNL,IL,JL)                                          MM5.209
#endif                                                                           MM5.210
   17   CONTINUE                                                                 MM5.211
      ENDIF                                                                      MM5.212
#endif                                                                           MM5.213
C                                                                                MM5.214
C--- SOLVE EQUATIONS FOR THIS DOMAIN                                             MM5.215
C                                                                                MM5.216
#ifdef MPP1                                                                      MM5.217
      CALL MP_INITDOMAIN(1)                                                      MM5.218
#endif                                                                           MM5.219
      CALL SOLVE(IEXEC(1),1,0)                                                   MM5.221
C                                                                                MM5.225
#ifndef MPP1                                                                     MM5.226
      CALL CONADV(IDRY(1),IMOIST(1))                                             MM5.227
      CALL CONMAS(IDRY(1),IMOIST(1),MASCHK)                                      MM5.228
#endif                                                                           MM5.229
C                                                                                MM5.230
C-- CHECK FOR FURTHER NEST LEVELS                                                MM5.231
C                                                                                MM5.232
#ifdef NESTED                                                                    MM5.233
      IF(NSTTOT.GT.1)CALL NSTLEV1(IEXEC,1)                                       MM5.234
#endif                                                                           MM5.235
C                                                                                MM5.236
C--- OUTPUT RESULTS                                                              MM5.237
C                                                                                MM5.238
      CALL OUTPUT(IEXEC,1)                                                       MM5.239
C                                                                                MM5.240
#if (defined(CRAY) && !defined(T3E) && !defined(crayx1))                         
C--- CHECK ON REMAINING CPU SECONDS USING CRAY LIB ROUTINE                       MM5.242
C--- EXTRAPOLATES TO CHECK IF SUFFICIENT TIME FOR NEXT COARSE-MESH STEP          MM5.243
C                                                                                MM5.244
      CALL TREMAIN(TLEFT)                                                        MM5.245
      IF(2.*TLEFT-TLEFTOLD.LT.TMARGIN)THEN                                       MM5.246
            PRINT *,'SHUT DOWN WITH ',TLEFT,' CPU SECONDS LEFT.'                 MM5.247
        CALL SHUTDO                                                              MM5.248
        STOP                                                                     MM5.249
      ENDIF                                                                      MM5.250
      TLEFTOLD=TLEFT                                                             MM5.251
#endif                                                                           MM5.252
C                                                                                MM5.264
#ifdef CRAY_TIM                                                                  23DEC04.26
      i3 = irtc()                                                                23DEC04.27
      f3 = flop( tick3, 0 )                                                      23DEC04.28
      itot = itot + (i3 - i2)                                                    23DEC04.29
      ftot = ftot + (f3 - f2)                                                    23DEC04.30
      ttot = ttot + (tick3 - tick2)                                              23DEC04.31
cfck                                                                             23DEC04.32
      print *,'tstep time=',float(i3-i2)/float(irtc_rate()),' flops=',           23DEC04.33
     & (f3-f2),' mflops/sec=',(f3-f2)*1.e-6*irtc_rate()/(i3-i2)                  23DEC04.34
#endif                                                                           23DEC04.35
C                                                                                23DEC04.36
      EXTIME=EXTIME+DTINC                                                        MM5.265
      IF(XTIME.LT.TIMAX)GOTO 10                                                  MM5.266
C                                                                                MM5.267
#ifdef CRAY_TIM                                                                  23DEC04.37
      i1 = irtc()                                                                23DEC04.38
      f1 = flop( tick1 )                                                         23DEC04.39
      print *,'inner flops =',ftot                                               23DEC04.40
      print *,'inner wall mflops/sec = ',                                        23DEC04.41
     &         (ftot)*1.e-6*irtc_rate()/(itot)                                   23DEC04.42
      print *,'inner wall time = ',                                              23DEC04.43
     &         float(itot)/float(irtc_rate()),' sec'                             23DEC04.44
      print *,'flops =',f1-f0                                                    23DEC04.45
      print *,'wall mflops/sec = ',(f1-f0)*1.e-6*irtc_rate()/(i1-i0)             23DEC04.46
      print *,'wall time = ',float(i1-i0)/float(irtc_rate()),' sec'              23DEC04.47
#endif                                                                           23DEC04.48
C                                                                                23DEC04.49
#ifdef MPP1                                                                      MM5.268
#  include <mpp_mm5_20.incl>                                                     MM5.269
#else                                                                            MM5.270
      STOP 99999                                                                 MM5.271
#endif                                                                           MM5.272
      END                                                                        MM5.273
