C **************************************************************************     RRTM_INIT.1
      SUBROUTINE RRTMINIT                                                        RRTM_INIT.2
C **************************************************************************     RRTM_INIT.3
C  RRTM Longwave Radiative Transfer Model                                        RRTM_INIT.4
C  Atmospheric and Environmental Research, Inc., Cambridge, MA                   RRTM_INIT.5
C                                                                                RRTM_INIT.6
C  Original version:       Michael J. Iacono; July, 1998                         RRTM_INIT.7
C  Revision for NCAR CCM:  Michael J. Iacono; September, 1998                    RRTM_INIT.8
C                                                                                RRTM_INIT.9
C  This subroutine performs calculations necessary for the initialization        RRTM_INIT.10
C  of the LW model, RRTM.  Lookup tables are computed for use in the LW          RRTM_INIT.11
C  radiative transfer, and input absorption coefficient data for each            RRTM_INIT.12
C  spectral band are reduced from 256 g-points to 140 for use in RRTM.           RRTM_INIT.13
C **************************************************************************     RRTM_INIT.14
                                                                                 RRTM_INIT.15
C Definitions                                                                    RRTM_INIT.16
C     Arrays for 5000-point look-up tables:                                      RRTM_INIT.17
C     TAU     Clear-sky optical depth (used in cloudy radiative transfer)        RRTM_INIT.18
C     TF      Tau transition function; i.e. the transition of the Planck         RRTM_INIT.19
C             function from that for the mean layer temperature to that for      RRTM_INIT.20
C             the layer boundary temperature as a function of optical depth.     RRTM_INIT.21
C             The "linear in tau" method is used to make the table.              RRTM_INIT.22
C     TRANS   Transmittance                                                      RRTM_INIT.23
C     BPADE   Inverse of the Pade approximation constant (= 1./0.278)            RRTM_INIT.24
C                                                                                RRTM_INIT.25
C Parameters                                                                     RRTM_INIT.26
      PARAMETER (MG=16, NBANDS=16)                                               RRTM_INIT.27
      PARAMETER (NGPT=140)                                                       RRTM_INIT.28
C Input                                                                          RRTM_INIT.29
      COMMON /FEATURES/ NG(NBANDS),NSPA(NBANDS),NSPB(NBANDS)                     RRTM_INIT.30
      COMMON /FEATUREB/ NGM(MG*NBANDS), WT(MG)                                   RRTM_INIT.31
      COMMON /FEATUREC/ NGC(NBANDS), NGS(NBANDS), NGN(NGPT), NGB(NGPT)           RRTM_INIT.32
      COMMON /PFDAT/    FREFA(NGPT,13),FREFB(NGPT,6),                            RRTM_INIT.33
     &                  FREFADF(NGPT,13),FREFBDF(NGPT,6)                         RRTM_INIT.34
C Output                                                                         RRTM_INIT.35
      COMMON /TAUTBL/   TAU(0:5000)                                              RRTM_INIT.36
      COMMON /TFTBL/    TF(0:5000)                                               RRTM_INIT.37
      COMMON /TRNTBL/   TRANS(0:5000)                                            RRTM_INIT.38
      COMMON /RWT/      RWGT(MG*NBANDS)                                          RRTM_INIT.39
      COMMON /PADE/     BPADE                                                    RRTM_INIT.40
      COMMON /TBG2TBL/  CORR1(0:200),CORR2(0:200)                                RRTM_INIT.41
C Local                                                                          RRTM_INIT.42
      DIMENSION WTSM(MG)                                                         RRTM_INIT.43
C                                                                                07NOV00.2201
      EXTERNAL DM_IONODE                                                         07NOV00.2202
      LOGICAL DM_IONODE                                                          07NOV00.2203
      INTEGER INUNIT                                                             07NOV00.2204
C                                                                                07NOV00.2205
c  initialize data blocks                                                        RRTM_INIT.44
                                                                                 RRTM_INIT.45
C     print *,' rrtminit called'                                                 RRTM_INIT.46
C                                                                                07NOV00.2206
      INUNIT = 20                                                                07NOV00.2207
      IF (DM_IONODE()) THEN                                                      07NOV00.2208
        OPEN(INUNIT,FILE='RRTM_DATA',                                            07NOV00.2209
     &       FORM='UNFORMATTED',STATUS='OLD',ERR=9010)                           07NOV00.2210
        PRINT *, 'OPEN RRTM DATA FILE FOR INITIALIZATION'                        07NOV00.2211
      ENDIF                                                                      07NOV00.2212
                                                                                 RRTM_INIT.47
      CALL KGB1                                                                  RRTM_INIT.48
      CALL KGB2                                                                  RRTM_INIT.49
      CALL KGB3                                                                  RRTM_INIT.50
      CALL KGB4                                                                  RRTM_INIT.51
      CALL KGB5                                                                  RRTM_INIT.52
      CALL KGB6                                                                  RRTM_INIT.53
      CALL KGB7                                                                  RRTM_INIT.54
      CALL KGB8                                                                  RRTM_INIT.55
      CALL KGB9                                                                  RRTM_INIT.56
      CALL KGB10                                                                 RRTM_INIT.57
      CALL KGB11                                                                 RRTM_INIT.58
      CALL KGB12                                                                 RRTM_INIT.59
      CALL KGB13                                                                 RRTM_INIT.60
      CALL KGB14                                                                 RRTM_INIT.61
      CALL KGB15                                                                 RRTM_INIT.62
      CALL KGB16                                                                 RRTM_INIT.63
C  Compute lookup tables for transmittance, tau transition function,             RRTM_INIT.64
C  and clear sky tau (for the cloudy sky radiative transfer).  Tau is            RRTM_INIT.65
C  computed as a function of the tau transition function, transmittance          RRTM_INIT.66
C  is calculated as a function of tau, and the tau transition function           RRTM_INIT.67
C  is calculated using the linear in tau formulation at values of tau            RRTM_INIT.68
C  above 0.01.  TF is approximated as tau/6 for tau < 0.01.  All tables          RRTM_INIT.69
C  are computed at intervals of 0.001.  The inverse of the constant used         RRTM_INIT.70
C  in the Pade approximation to the tau transition function is set to b.         RRTM_INIT.71
                                                                                 RRTM_INIT.72
      TAU(0) = 0.0                                                               RRTM_INIT.73
      TAU(5000) = 1.E10                                                          RRTM_INIT.74
      TRANS(0) = 1.0                                                             RRTM_INIT.75
      TRANS(5000) = 0.0                                                          RRTM_INIT.76
      TF(0) = 0.0                                                                RRTM_INIT.77
      TF(5000) = 1.0                                                             RRTM_INIT.78
      BPADE=1./0.278                                                             RRTM_INIT.79
      DO 1000 ITR = 1,4999                                                       RRTM_INIT.80
         TFN = ITR/5.E3                                                          RRTM_INIT.81
         TAU(ITR) = BPADE*TFN/(1.-TFN)                                           RRTM_INIT.82
         TRANS(ITR) = EXP(-TAU(ITR))                                             RRTM_INIT.83
         IF (TAU(ITR).LT.0.1) THEN                                               RRTM_INIT.84
            TF(ITR) = TAU(ITR)/6.                                                RRTM_INIT.85
         ELSE                                                                    RRTM_INIT.86
            TF(ITR) = 1.-2.*((1./TAU(ITR))-(TRANS(ITR)/(1.-TRANS(ITR))))         RRTM_INIT.87
         ENDIF                                                                   RRTM_INIT.88
 1000 CONTINUE                                                                   RRTM_INIT.89
C  Calculate lookup tables for functions needed in routine TAUMOL (TAUGB2)       RRTM_INIT.90
      CORR1(0) = 1.                                                              RRTM_INIT.91
      CORR1(200) = 1.                                                            RRTM_INIT.92
      CORR2(0) = 1.                                                              RRTM_INIT.93
      CORR2(200) = 1.                                                            RRTM_INIT.94
      DO 1200 I = 1,199                                                          RRTM_INIT.95
         FP = 0.005*FLOAT(I)                                                     RRTM_INIT.96
         RTFP = SQRT(FP)                                                         RRTM_INIT.97
         CORR1(I) = RTFP/FP                                                      RRTM_INIT.98
         CORR2(I) = (1.-RTFP)/(1.-FP)                                            RRTM_INIT.99
 1200 CONTINUE                                                                   RRTM_INIT.100
                                                                                 RRTM_INIT.101
C  Perform g-point reduction from 16 per band (256 total points) to              RRTM_INIT.102
C  a band dependant number (140 total points) for all absorption                 RRTM_INIT.103
C  coefficient input data and Planck fraction input data.                        RRTM_INIT.104
C  Compute relative weighting for new g-point combinations.                      RRTM_INIT.105
                                                                                 RRTM_INIT.106
      IGCSM = 0                                                                  RRTM_INIT.107
      DO 500 IBND = 1,NBANDS                                                     RRTM_INIT.108
         IPRSM = 0                                                               RRTM_INIT.109
         IF (NGC(IBND).LT.16) THEN                                               RRTM_INIT.110
            DO 450 IGC = 1,NGC(IBND)                                             RRTM_INIT.111
               IGCSM = IGCSM + 1                                                 RRTM_INIT.112
               WTSUM = 0.                                                        RRTM_INIT.113
               DO 420 IPR = 1, NGN(IGCSM)                                        RRTM_INIT.114
                  IPRSM = IPRSM + 1                                              RRTM_INIT.115
                  WTSUM = WTSUM + WT(IPRSM)                                      RRTM_INIT.116
 420           CONTINUE                                                          RRTM_INIT.117
               WTSM(IGC) = WTSUM                                                 RRTM_INIT.118
 450        CONTINUE                                                             RRTM_INIT.119
            DO 400 IG = 1,NG(IBND)                                               RRTM_INIT.120
               IND = (IBND-1)*16 + IG                                            RRTM_INIT.121
               RWGT(IND) = WT(IG)/WTSM(NGM(IND))                                 RRTM_INIT.122
 400        CONTINUE                                                             RRTM_INIT.123
         ELSE                                                                    RRTM_INIT.124
            DO 300 IG = 1,NG(IBND)                                               RRTM_INIT.125
               IGCSM = IGCSM + 1                                                 RRTM_INIT.126
               IND = (IBND-1)*16 + IG                                            RRTM_INIT.127
               RWGT(IND) = 1.0                                                   RRTM_INIT.128
 300        CONTINUE                                                             RRTM_INIT.129
         ENDIF                                                                   RRTM_INIT.130
 500  CONTINUE                                                                   RRTM_INIT.131
                                                                                 RRTM_INIT.132
C  Initialize arrays for combined Planck fraction data.                          RRTM_INIT.133
                                                                                 RRTM_INIT.134
      DO 2000 IPT = 1,13                                                         RRTM_INIT.135
         DO 2000 IPR = 1, NGPT                                                   RRTM_INIT.136
            FREFA(IPR,IPT) = 0.                                                  RRTM_INIT.137
 2000       FREFADF(IPR,IPT) = 0.                                                RRTM_INIT.138
      DO 2200 IPT = 1,6                                                          RRTM_INIT.139
         DO 2200 IPR = 1, NGPT                                                   RRTM_INIT.140
            FREFB(IPR,IPT) = 0.                                                  RRTM_INIT.141
 2200       FREFBDF(IPR,IPT) = 0.                                                RRTM_INIT.142
                                                                                 RRTM_INIT.143
C  Reduce g-points for relevant data in each LW spectral band.                   RRTM_INIT.144
                                                                                 RRTM_INIT.145
      CALL CMBDATA                                                               RRTM_INIT.146
      CALL CMBGB1                                                                RRTM_INIT.147
      CALL CMBGB2                                                                RRTM_INIT.148
      CALL CMBGB3                                                                RRTM_INIT.149
      CALL CMBGB4                                                                RRTM_INIT.150
      CALL CMBGB5                                                                RRTM_INIT.151
      CALL CMBGB6                                                                RRTM_INIT.152
      CALL CMBGB7                                                                RRTM_INIT.153
      CALL CMBGB8                                                                RRTM_INIT.154
      CALL CMBGB9                                                                RRTM_INIT.155
      CALL CMBGB10                                                               RRTM_INIT.156
      CALL CMBGB11                                                               RRTM_INIT.157
      CALL CMBGB12                                                               RRTM_INIT.158
      CALL CMBGB13                                                               RRTM_INIT.159
      CALL CMBGB14                                                               RRTM_INIT.160
      CALL CMBGB15                                                               RRTM_INIT.161
      CALL CMBGB16                                                               RRTM_INIT.162
                                                                                 07NOV00.2213
      CLOSE (INUNIT)                                                             07NOV00.2214
                                                                                 RRTM_INIT.163
      RETURN                                                                     RRTM_INIT.164
9010  PRINT *,'ERROR OPENING RRTM DATA FILE FROM UNIT ', INUNIT                  07NOV00.2215
      STOP 'STOP 9010 IN RRTMINIT'                                               07NOV00.2216
      END                                                                        RRTM_INIT.165
                                                                                 RRTM_INIT.166
C***************************************************************************     RRTM_INIT.167
                                                                                 RRTM_INIT.168
      SUBROUTINE CMBDATA                                                         RRTM_INIT.169
                                                                                 RRTM_INIT.170
C     Arrays for the g-point reduction from 256 to 140 for the 16 LW bands:      RRTM_INIT.171
C     This mapping from 256 to 140 points has been carefully selected to         RRTM_INIT.172
C     minimize the effect on the resulting fluxes and cooling rates, and         RRTM_INIT.173
C     caution should be used if the mapping is modified.                         RRTM_INIT.174
C                                                                                RRTM_INIT.175
C     NGPT    The total number of new g-points                                   RRTM_INIT.176
C     NGC     The number of new g-points in each band                            RRTM_INIT.177
C     NGM     The index of each new g-point relative to the original             RRTM_INIT.178
C             16 g-points for each band.                                         RRTM_INIT.179
C     NGN     The number of original g-points that are combined to make          RRTM_INIT.180
C             each new g-point in each band.                                     RRTM_INIT.181
C     NGB     The band index for each new g-point.                               RRTM_INIT.182
C     WT      RRTM weights for 16 g-points.                                      RRTM_INIT.183
                                                                                 RRTM_INIT.184
C Parameters                                                                     RRTM_INIT.185
      PARAMETER (MG=16, NBANDS=16)                                               RRTM_INIT.186
      PARAMETER (NGPT=140)                                                       RRTM_INIT.187
C Input                                                                          RRTM_INIT.188
      COMMON /FEATUREB/ NGM(MG*NBANDS), WT(MG)                                   RRTM_INIT.189
      COMMON /FEATUREC/ NGC(NBANDS), NGS(NBANDS), NGN(NGPT), NGB(NGPT)           RRTM_INIT.190
C Data Statements                                                                RRTM_INIT.191
      DATA NGC  /8,14,16,14,16,8,12,8,12,6,8,8,4,2,2,2/                          RRTM_INIT.192
      DATA NGS  /8,22,38,52,68,76,88,96,108,114,122,130,134,136,138,140/         RRTM_INIT.193
      DATA NGM  /1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,            ! Band 1            RRTM_INIT.194
     &           1,2,3,4,5,6,7,8,9,10,11,12,13,13,14,14,     ! Band 2            RRTM_INIT.195
     &           1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,     ! Band 3            RRTM_INIT.196
     &           1,2,3,4,5,6,7,8,9,10,11,12,13,14,14,14,     ! Band 4            RRTM_INIT.197
     &           1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,     ! Band 5            RRTM_INIT.198
     &           1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,            ! Band 6            RRTM_INIT.199
     &           1,1,2,2,3,4,5,6,7,8,9,10,11,11,12,12,       ! Band 7            RRTM_INIT.200
     &           1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,            ! Band 8            RRTM_INIT.201
     &           1,2,3,4,5,6,7,8,9,9,10,10,11,11,12,12,      ! Band 9            RRTM_INIT.202
     &           1,1,2,2,3,3,4,4,5,5,5,5,6,6,6,6,            ! Band 10           RRTM_INIT.203
     &           1,2,3,3,4,4,5,5,6,6,7,7,7,8,8,8,            ! Band 11           RRTM_INIT.204
     &           1,2,3,4,5,5,6,6,7,7,7,7,8,8,8,8,            ! Band 12           RRTM_INIT.205
     &           1,1,1,2,2,2,3,3,3,3,4,4,4,4,4,4,            ! Band 13           RRTM_INIT.206
     &           1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,            ! Band 14           RRTM_INIT.207
     &           1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,            ! Band 15           RRTM_INIT.208
     &           1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2/            ! Band 16           RRTM_INIT.209
      DATA NGN  /2,2,2,2,2,2,2,2,                            ! Band 1            RRTM_INIT.210
     &           1,1,1,1,1,1,1,1,1,1,1,1,2,2,                ! Band 2            RRTM_INIT.211
     &           16*1,                                       ! Band 3            RRTM_INIT.212
     &           1,1,1,1,1,1,1,1,1,1,1,1,1,3,                ! Band 4            RRTM_INIT.213
     &           16*1,                                       ! Band 5            RRTM_INIT.214
     &           2,2,2,2,2,2,2,2,                            ! Band 6            RRTM_INIT.215
     &           2,2,1,1,1,1,1,1,1,1,2,2,                    ! Band 7            RRTM_INIT.216
     &           2,2,2,2,2,2,2,2,                            ! Band 8            RRTM_INIT.217
     &           1,1,1,1,1,1,1,1,2,2,2,2,                    ! Band 9            RRTM_INIT.218
     &           2,2,2,2,4,4,                                ! Band 10           RRTM_INIT.219
     &           1,1,2,2,2,2,3,3,                            ! Band 11           RRTM_INIT.220
     &           1,1,1,1,2,2,4,4,                            ! Band 12           RRTM_INIT.221
     &           3,3,4,6,                                    ! Band 13           RRTM_INIT.222
     &           8,8,                                        ! Band 14           RRTM_INIT.223
     &           8,8,                                        ! Band 15           RRTM_INIT.224
     &           8,8/                                        ! Band 16           RRTM_INIT.225
      DATA NGB  /8*1,                                        ! Band 1            RRTM_INIT.226
     &           14*2,                                       ! Band 2            RRTM_INIT.227
     &           16*3,                                       ! Band 3            RRTM_INIT.228
     &           14*4,                                       ! Band 4            RRTM_INIT.229
     &           16*5,                                       ! Band 5            RRTM_INIT.230
     &           8*6,                                        ! Band 6            RRTM_INIT.231
     &           12*7,                                       ! Band 7            RRTM_INIT.232
     &           8*8,                                        ! Band 8            RRTM_INIT.233
     &           12*9,                                       ! Band 9            RRTM_INIT.234
     &           6*10,                                       ! Band 10           RRTM_INIT.235
     &           8*11,                                       ! Band 11           RRTM_INIT.236
     &           8*12,                                       ! Band 12           RRTM_INIT.237
     &           4*13,                                       ! Band 13           RRTM_INIT.238
     &           2*14,                                       ! Band 14           RRTM_INIT.239
     &           2*15,                                       ! Band 15           RRTM_INIT.240
     &           2*16/                                       ! Band 16           RRTM_INIT.241
      DATA WT/                                                                   RRTM_INIT.242
     &     0.1527534276,0.1491729617,0.1420961469,0.1316886544,                  RRTM_INIT.243
     &     0.1181945205,0.1019300893,0.0832767040,0.0626720116,                  RRTM_INIT.244
     &     0.0424925,0.0046269894,0.0038279891,0.0030260086,                     RRTM_INIT.245
     &     0.0022199750,0.0014140010,0.000533,0.000075/                          RRTM_INIT.246
                                                                                 RRTM_INIT.247
      END                                                                        RRTM_INIT.248
                                                                                 RRTM_INIT.249
C***************************************************************************     RRTM_INIT.250
      SUBROUTINE CMBGB1                                                          RRTM_INIT.251
C***************************************************************************     RRTM_INIT.252
C                                                                                RRTM_INIT.253
C  Original version:       Michael J. Iacono; July, 1998                         RRTM_INIT.254
C  Revision for NCAR CCM:  Michael J. Iacono; September, 1998                    RRTM_INIT.255
C                                                                                RRTM_INIT.256
C  The subroutines CMBGB1->CMBGB16 input the absorption coefficient              RRTM_INIT.257
C  data for each band, which are defined for 16 g-points and 16 spectral         RRTM_INIT.258
C  bands. The data are combined with appropriate weighting following the         RRTM_INIT.259
C  g-point mapping arrays specified in RRTMINIT.  Plank fraction data            RRTM_INIT.260
C  in arrays FRACREFA and FRACREFB are combined without weighting.  All          RRTM_INIT.261
C  g-point reduced data are put into new arrays for use in RRTM.                 RRTM_INIT.262
C                                                                                RRTM_INIT.263
C  BAND 1:  10-250 cm-1 (low - H2O; high - H2O)                                  RRTM_INIT.264
C***************************************************************************     RRTM_INIT.265
                                                                                 RRTM_INIT.266
C Parameters                                                                     RRTM_INIT.267
      PARAMETER (MG=16, NBANDS=16)                                               RRTM_INIT.268
      PARAMETER (NGPT=140, NG1=8)                                                RRTM_INIT.269
C Input                                                                          RRTM_INIT.270
      COMMON /K1/       KA(5,13,MG), KB(5,13:59,MG), SELFREF(10,MG)              RRTM_INIT.271
      COMMON /RWT/      RWGT(MG*NBANDS)                                          RRTM_INIT.272
      COMMON /FEATUREC/ NGC(NBANDS), NGS(NBANDS), NGN(NGPT), NGB(NGPT)           RRTM_INIT.273
C Output                                                                         RRTM_INIT.274
      COMMON /K1C/   KAC(5,13,NG1), KBC(5,13:59,NG1),                            RRTM_INIT.275
     &               SELFREFC(10,NG1), FORREFC(NG1)                              RRTM_INIT.276
      COMMON /PF1C/  FRACREFAC(NG1), FRACREFBC(NG1)                              RRTM_INIT.277
      COMMON /PFDAT/ FREFA(NGPT,13),FREFB(NGPT,6),                               RRTM_INIT.278
     &               FREFADF(NGPT,13),FREFBDF(NGPT,6)                            RRTM_INIT.279
                                                                                 RRTM_INIT.280
C  Data                                                                          RRTM_INIT.281
      DIMENSION FRACREFA(MG), FRACREFB(MG), FORREF(MG)                           RRTM_INIT.282
                                                                                 RRTM_INIT.283
      DATA FRACREFA/                                                             RRTM_INIT.284
     &    0.08452097,0.17952873,0.16214369,0.13602182,                           RRTM_INIT.285
     &    0.12760490,0.10302561,0.08392423,0.06337652,                           RRTM_INIT.286
     &    0.04206551,0.00487497,0.00410743,0.00344421,                           RRTM_INIT.287
     &    0.00285731,0.00157327,0.00080648,0.00012406/                           RRTM_INIT.288
      DATA FRACREFB/                                                             RRTM_INIT.289
     &    0.15492001,0.17384727,0.15165100,0.12675308,                           RRTM_INIT.290
     &    0.10986247,0.09006091,0.07584465,0.05990077,                           RRTM_INIT.291
     &    0.04113461,0.00438638,0.00374754,0.00313924,                           RRTM_INIT.292
     &    0.00234381,0.00167167,0.00062744,0.00010889/                           RRTM_INIT.293
                                                                                 RRTM_INIT.294
      DATA FORREF/                                                               RRTM_INIT.295
     &   -4.50470E-02,-1.18908E-01,-7.21730E-02,-2.83862E-02,                    RRTM_INIT.296
     &   -3.01961E-02,-1.56877E-02,-1.53684E-02,-1.29135E-02,                    RRTM_INIT.297
     &   -1.27963E-02,-1.81742E-03, 4.40008E-05, 1.05260E-02,                    RRTM_INIT.298
     &    2.17290E-02, 1.65571E-02, 7.60751E-02, 1.47405E-01/                    RRTM_INIT.299
                                                                                 RRTM_INIT.300
      REAL KA,KB,KAC,KBC                                                         RRTM_INIT.301
                                                                                 RRTM_INIT.302
      DO 2000 JT = 1,5                                                           RRTM_INIT.303
         DO 2200 JP = 1,13                                                       RRTM_INIT.304
            IPRSM = 0                                                            RRTM_INIT.305
            DO 2400 IGC = 1,NGC(1)                                               RRTM_INIT.306
               SUMK = 0.                                                         RRTM_INIT.307
               DO 2600 IPR = 1, NGN(IGC)                                         RRTM_INIT.308
                  IPRSM = IPRSM + 1                                              RRTM_INIT.309
                  SUMK = SUMK + KA(JT,JP,IPRSM)*RWGT(IPRSM)                      RRTM_INIT.310
 2600          CONTINUE                                                          RRTM_INIT.311
               KAC(JT,JP,IGC) = SUMK                                             RRTM_INIT.312
 2400       CONTINUE                                                             RRTM_INIT.313
 2200    CONTINUE                                                                RRTM_INIT.314
         DO 3200 JP = 13,59                                                      RRTM_INIT.315
            IPRSM = 0                                                            RRTM_INIT.316
            DO 3400 IGC = 1,NGC(1)                                               RRTM_INIT.317
               SUMK = 0.                                                         RRTM_INIT.318
               DO 3600 IPR = 1, NGN(IGC)                                         RRTM_INIT.319
                  IPRSM = IPRSM + 1                                              RRTM_INIT.320
                  SUMK = SUMK + KB(JT,JP,IPRSM)*RWGT(IPRSM)                      RRTM_INIT.321
 3600          CONTINUE                                                          RRTM_INIT.322
               KBC(JT,JP,IGC) = SUMK                                             RRTM_INIT.323
 3400       CONTINUE                                                             RRTM_INIT.324
 3200    CONTINUE                                                                RRTM_INIT.325
 2000 CONTINUE                                                                   RRTM_INIT.326
                                                                                 RRTM_INIT.327
      DO 4000 JT = 1,10                                                          RRTM_INIT.328
         IPRSM = 0                                                               RRTM_INIT.329
         DO 4400 IGC = 1,NGC(1)                                                  RRTM_INIT.330
            SUMK = 0.                                                            RRTM_INIT.331
            DO 4600 IPR = 1, NGN(IGC)                                            RRTM_INIT.332
               IPRSM = IPRSM + 1                                                 RRTM_INIT.333
               SUMK = SUMK + SELFREF(JT,IPRSM)*RWGT(IPRSM)                       RRTM_INIT.334
 4600       CONTINUE                                                             RRTM_INIT.335
            SELFREFC(JT,IGC) = SUMK                                              RRTM_INIT.336
 4400    CONTINUE                                                                RRTM_INIT.337
 4000 CONTINUE                                                                   RRTM_INIT.338
                                                                                 RRTM_INIT.339
      IPRSM = 0                                                                  RRTM_INIT.340
      DO 5400 IGC = 1,NGC(1)                                                     RRTM_INIT.341
         SUMK = 0.                                                               RRTM_INIT.342
         SUMF1 = 0.                                                              RRTM_INIT.343
         SUMF2 = 0.                                                              RRTM_INIT.344
         DO 5600 IPR = 1, NGN(IGC)                                               RRTM_INIT.345
            IPRSM = IPRSM + 1                                                    RRTM_INIT.346
            SUMK = SUMK + FORREF(IPRSM)*RWGT(IPRSM)                              RRTM_INIT.347
            SUMF1= SUMF1+ FRACREFA(IPRSM)                                        RRTM_INIT.348
            SUMF2= SUMF2+ FRACREFB(IPRSM)                                        RRTM_INIT.349
 5600    CONTINUE                                                                RRTM_INIT.350
         FORREFC(IGC) = SUMK                                                     RRTM_INIT.351
         FRACREFAC(IGC) = SUMF1                                                  RRTM_INIT.352
         FRACREFBC(IGC) = SUMF2                                                  RRTM_INIT.353
 5400 CONTINUE                                                                   RRTM_INIT.354
                                                                                 RRTM_INIT.355
      DO 8000 IGC = 1,NGC(1)                                                     RRTM_INIT.356
         FREFA(IGC,1) = FRACREFAC(IGC)                                           RRTM_INIT.357
         FREFB(IGC,1) = FRACREFBC(IGC)                                           RRTM_INIT.358
 8000 CONTINUE                                                                   RRTM_INIT.359
                                                                                 RRTM_INIT.360
      RETURN                                                                     RRTM_INIT.361
      END                                                                        RRTM_INIT.362
                                                                                 RRTM_INIT.363
C***************************************************************************     RRTM_INIT.364
      SUBROUTINE CMBGB2                                                          RRTM_INIT.365
C***************************************************************************     RRTM_INIT.366
C                                                                                RRTM_INIT.367
C     BAND 2:  250-500 cm-1 (low - H2O; high - H2O)                              RRTM_INIT.368
C***************************************************************************     RRTM_INIT.369
                                                                                 RRTM_INIT.370
C Parameters                                                                     RRTM_INIT.371
      PARAMETER (MG=16, NBANDS=16)                                               RRTM_INIT.372
      PARAMETER (NGPT=140, NG2=14)                                               RRTM_INIT.373
C Input                                                                          RRTM_INIT.374
      COMMON /K2/       KA(5,13,MG), KB(5,13:59,MG), SELFREF(10,MG)              RRTM_INIT.375
      COMMON /RWT/      RWGT(MG*NBANDS)                                          RRTM_INIT.376
      COMMON /FEATUREC/ NGC(NBANDS), NGS(NBANDS), NGN(NGPT), NGB(NGPT)           RRTM_INIT.377
C Output                                                                         RRTM_INIT.378
      COMMON /K2C/   KAC(5,13,NG2), KBC(5,13:59,NG2),                            RRTM_INIT.379
     &               SELFREFC(10,NG2), FORREFC(NG2)                              RRTM_INIT.380
      COMMON /PF2C/  FRACREFAC(NG2,13), FRACREFBC(NG2)                           RRTM_INIT.381
      COMMON /PFDAT/ FREFA(NGPT,13),FREFB(NGPT,6),                               RRTM_INIT.382
     &               FREFADF(NGPT,13),FREFBDF(NGPT,6)                            RRTM_INIT.383
                                                                                 RRTM_INIT.384
C Data                                                                           RRTM_INIT.385
      DIMENSION FRACREFA(MG,13), FRACREFB(MG), FORREF(MG)                        RRTM_INIT.386
                                                                                 RRTM_INIT.387
C     The ith set of reference fractions are from the ith reference              RRTM_INIT.388
C     pressure level.                                                            RRTM_INIT.389
      DATA FRACREFA/                                                             RRTM_INIT.390
     &    0.18068060,0.16803175,0.15140158,0.12221480,                           RRTM_INIT.391
     &    0.10240850,0.09330297,0.07518960,0.05611294,                           RRTM_INIT.392
     &    0.03781487,0.00387192,0.00321285,0.00244440,                           RRTM_INIT.393
     &    0.00179546,0.00107704,0.00038798,0.00005060,                           RRTM_INIT.394
     &    0.17927621,0.16731168,0.15129538,0.12328085,                           RRTM_INIT.395
     &    0.10243484,0.09354796,0.07538418,0.05633071,                           RRTM_INIT.396
     &    0.03810832,0.00398347,0.00320262,0.00250029,                           RRTM_INIT.397
     &    0.00178666,0.00111127,0.00039438,0.00005169,                           RRTM_INIT.398
     &    0.17762886,0.16638555,0.15115446,0.12470623,                           RRTM_INIT.399
     &    0.10253213,0.09383459,0.07560240,0.05646568,                           RRTM_INIT.400
     &    0.03844077,0.00409142,0.00322521,0.00254918,                           RRTM_INIT.401
     &    0.00179296,0.00113652,0.00040169,0.00005259,                           RRTM_INIT.402
     &    0.17566043,0.16539773,0.15092199,0.12571971,                           RRTM_INIT.403
     &    0.10340609,0.09426189,0.07559051,0.05678188,                           RRTM_INIT.404
     &    0.03881499,0.00414102,0.00328551,0.00258795,                           RRTM_INIT.405
     &    0.00181648,0.00115145,0.00040969,0.00005357,                           RRTM_INIT.406
     &    0.17335825,0.16442548,0.15070701,0.12667464,                           RRTM_INIT.407
     &    0.10452303,0.09450833,0.07599410,0.05706393,                           RRTM_INIT.408
     &    0.03910370,0.00417880,0.00335256,0.00261708,                           RRTM_INIT.409
     &    0.00185491,0.00116627,0.00041759,0.00005464,                           RRTM_INIT.410
     &    0.17082544,0.16321516,0.15044247,0.12797612,                           RRTM_INIT.411
     &    0.10574646,0.09470057,0.07647423,0.05738756,                           RRTM_INIT.412
     &    0.03935621,0.00423789,0.00342651,0.00264549,                           RRTM_INIT.413
     &    0.00190188,0.00118281,0.00042592,0.00005583,                           RRTM_INIT.414
     &    0.16809277,0.16193336,0.15013184,0.12937409,                           RRTM_INIT.415
     &    0.10720784,0.09485368,0.07692636,0.05771774,                           RRTM_INIT.416
     &    0.03966988,0.00427754,0.00349696,0.00268946,                           RRTM_INIT.417
     &    0.00193536,0.00120222,0.00043462,0.00005712,                           RRTM_INIT.418
     &    0.16517997,0.16059248,0.14984852,0.13079269,                           RRTM_INIT.419
     &    0.10865030,0.09492947,0.07759736,0.05812201,                           RRTM_INIT.420
     &    0.03997169,0.00432356,0.00355308,0.00274031,                           RRTM_INIT.421
     &    0.00197243,0.00122401,0.00044359,0.00005849,                           RRTM_INIT.422
     &    0.16209179,0.15912023,0.14938223,0.13198245,                           RRTM_INIT.423
     &    0.11077233,0.09487948,0.07831636,0.05863440,                           RRTM_INIT.424
     &    0.04028239,0.00436804,0.00360407,0.00279885,                           RRTM_INIT.425
     &    0.00200364,0.00124861,0.00045521,0.00005996,                           RRTM_INIT.426
     &    0.15962425,0.15789343,0.14898103,0.13275230,                           RRTM_INIT.427
     &    0.11253940,0.09503502,0.07884382,0.05908009,                           RRTM_INIT.428
     &    0.04053524,0.00439971,0.00364269,0.00284965,                           RRTM_INIT.429
     &    0.00202758,0.00127076,0.00046408,0.00006114,                           RRTM_INIT.430
     &    0.15926200,0.15770932,0.14891729,0.13283882,                           RRTM_INIT.431
     &    0.11276010,0.09507311,0.07892222,0.05919230,                           RRTM_INIT.432
     &    0.04054824,0.00440833,0.00365575,0.00286459,                           RRTM_INIT.433
     &    0.00203786,0.00128405,0.00046504,0.00006146,                           RRTM_INIT.434
     &    0.15926351,0.15770483,0.14891177,0.13279966,                           RRTM_INIT.435
     &    0.11268171,0.09515216,0.07890341,0.05924807,                           RRTM_INIT.436
     &    0.04052851,0.00440870,0.00365425,0.00286878,                           RRTM_INIT.437
     &    0.00205747,0.00128916,0.00046589,0.00006221,                           RRTM_INIT.438
     &    0.15937765,0.15775780,0.14892603,0.13273248,                           RRTM_INIT.439
     &    0.11252731,0.09521657,0.07885858,0.05927679,                           RRTM_INIT.440
     &    0.04050184,0.00440285,0.00365748,0.00286791,                           RRTM_INIT.441
     &    0.00207507,0.00129193,0.00046679,0.00006308/                           RRTM_INIT.442
C     From P = 0.432 mb.                                                         RRTM_INIT.443
      DATA FRACREFB/                                                             RRTM_INIT.444
     &    0.17444289,0.16467269,0.15021490,0.12460902,                           RRTM_INIT.445
     &    0.10400643,0.09481928,0.07590704,0.05752856,                           RRTM_INIT.446
     &    0.03931715,0.00428572,0.00349352,0.00278938,                           RRTM_INIT.447
     &    0.00203448,0.00130037,0.00051560,0.00006255/                           RRTM_INIT.448
                                                                                 RRTM_INIT.449
      DATA FORREF/                                                               RRTM_INIT.450
     &   -2.34550E-03,-8.42698E-03,-2.01816E-02,-5.66701E-02,                    RRTM_INIT.451
     &   -8.93189E-02,-6.37487E-02,-4.56455E-02,-4.41417E-02,                    RRTM_INIT.452
     &   -4.48605E-02,-4.74696E-02,-5.16648E-02,-5.63099E-02,                    RRTM_INIT.453
     &   -4.74781E-02,-3.84704E-02,-2.49905E-02, 2.02114E-03/                    RRTM_INIT.454
                                                                                 RRTM_INIT.455
      REAL KA,KB,KAC,KBC                                                         RRTM_INIT.456
                                                                                 RRTM_INIT.457
      DO 2000 JT = 1,5                                                           RRTM_INIT.458
         DO 2200 JP = 1,13                                                       RRTM_INIT.459
            IPRSM = 0                                                            RRTM_INIT.460
            DO 2400 IGC = 1,NGC(2)                                               RRTM_INIT.461
               SUMK = 0.                                                         RRTM_INIT.462
               DO 2600 IPR = 1, NGN(NGS(1)+IGC)                                  RRTM_INIT.463
                  IPRSM = IPRSM + 1                                              RRTM_INIT.464
                  SUMK = SUMK + KA(JT,JP,IPRSM)*RWGT(IPRSM+16)                   RRTM_INIT.465
 2600          CONTINUE                                                          RRTM_INIT.466
               KAC(JT,JP,IGC) = SUMK                                             RRTM_INIT.467
 2400       CONTINUE                                                             RRTM_INIT.468
 2200    CONTINUE                                                                RRTM_INIT.469
         DO 3200 JP = 13,59                                                      RRTM_INIT.470
            IPRSM = 0                                                            RRTM_INIT.471
            DO 3400 IGC = 1,NGC(2)                                               RRTM_INIT.472
               SUMK = 0.                                                         RRTM_INIT.473
               DO 3600 IPR = 1, NGN(NGS(1)+IGC)                                  RRTM_INIT.474
                  IPRSM = IPRSM + 1                                              RRTM_INIT.475
                  SUMK = SUMK + KB(JT,JP,IPRSM)*RWGT(IPRSM+16)                   RRTM_INIT.476
 3600          CONTINUE                                                          RRTM_INIT.477
               KBC(JT,JP,IGC) = SUMK                                             RRTM_INIT.478
 3400       CONTINUE                                                             RRTM_INIT.479
 3200    CONTINUE                                                                RRTM_INIT.480
 2000 CONTINUE                                                                   RRTM_INIT.481
                                                                                 RRTM_INIT.482
      DO 4000 JT = 1,10                                                          RRTM_INIT.483
         IPRSM = 0                                                               RRTM_INIT.484
         DO 4400 IGC = 1,NGC(2)                                                  RRTM_INIT.485
            SUMK = 0.                                                            RRTM_INIT.486
            DO 4600 IPR = 1, NGN(NGS(1)+IGC)                                     RRTM_INIT.487
               IPRSM = IPRSM + 1                                                 RRTM_INIT.488
               SUMK = SUMK + SELFREF(JT,IPRSM)*RWGT(IPRSM+16)                    RRTM_INIT.489
 4600       CONTINUE                                                             RRTM_INIT.490
            SELFREFC(JT,IGC) = SUMK                                              RRTM_INIT.491
 4400    CONTINUE                                                                RRTM_INIT.492
 4000 CONTINUE                                                                   RRTM_INIT.493
                                                                                 RRTM_INIT.494
      DO 5000 JP = 1,13                                                          RRTM_INIT.495
         IPRSM = 0                                                               RRTM_INIT.496
         DO 5400 IGC = 1,NGC(2)                                                  RRTM_INIT.497
            SUMF = 0.                                                            RRTM_INIT.498
            DO 5600 IPR = 1, NGN(NGS(1)+IGC)                                     RRTM_INIT.499
               IPRSM = IPRSM + 1                                                 RRTM_INIT.500
               SUMF = SUMF + FRACREFA(IPRSM,JP)                                  RRTM_INIT.501
 5600       CONTINUE                                                             RRTM_INIT.502
            FRACREFAC(IGC,JP) = SUMF                                             RRTM_INIT.503
 5400    CONTINUE                                                                RRTM_INIT.504
 5000 CONTINUE                                                                   RRTM_INIT.505
                                                                                 RRTM_INIT.506
      IPRSM = 0                                                                  RRTM_INIT.507
      DO 6400 IGC = 1,NGC(2)                                                     RRTM_INIT.508
         SUMK = 0.                                                               RRTM_INIT.509
         SUMF = 0.                                                               RRTM_INIT.510
         DO 6600 IPR = 1, NGN(NGS(1)+IGC)                                        RRTM_INIT.511
            IPRSM = IPRSM + 1                                                    RRTM_INIT.512
            SUMK = SUMK + FORREF(IPRSM)*RWGT(IPRSM+16)                           RRTM_INIT.513
            SUMF = SUMF + FRACREFB(IPRSM)                                        RRTM_INIT.514
 6600    CONTINUE                                                                RRTM_INIT.515
         FORREFC(IGC) = SUMK                                                     RRTM_INIT.516
         FRACREFBC(IGC) = SUMF                                                   RRTM_INIT.517
 6400 CONTINUE                                                                   RRTM_INIT.518
                                                                                 RRTM_INIT.519
      DO 8000 JP = 1,13                                                          RRTM_INIT.520
         DO 8000 IGC = 1,NGC(2)                                                  RRTM_INIT.521
            FREFA(NGS(1)+IGC,JP) = FRACREFAC(IGC,JP)                             RRTM_INIT.522
 8000 CONTINUE                                                                   RRTM_INIT.523
      DO 8100 JP = 2,13                                                          RRTM_INIT.524
         DO 8100 IGC = 1,NGC(2)                                                  RRTM_INIT.525
            FREFADF(NGS(1)+IGC,JP) = FRACREFAC(IGC,JP-1) -                       RRTM_INIT.526
     &                               FRACREFAC(IGC,JP)                           RRTM_INIT.527
 8100 CONTINUE                                                                   RRTM_INIT.528
      DO 8200 IGC = 1,NGC(2)                                                     RRTM_INIT.529
         FREFB(NGS(1)+IGC,1) = FRACREFBC(IGC)                                    RRTM_INIT.530
 8200 CONTINUE                                                                   RRTM_INIT.531
                                                                                 RRTM_INIT.532
      RETURN                                                                     RRTM_INIT.533
      END                                                                        RRTM_INIT.534
                                                                                 RRTM_INIT.535
C***************************************************************************     RRTM_INIT.536
      SUBROUTINE CMBGB3                                                          RRTM_INIT.537
C***************************************************************************     RRTM_INIT.538
C                                                                                RRTM_INIT.539
C     BAND 3:  500-630 cm-1 (low - H2O,CO2; high - H2O,CO2)                      RRTM_INIT.540
C***************************************************************************     RRTM_INIT.541
                                                                                 RRTM_INIT.542
C Parameters                                                                     RRTM_INIT.543
      PARAMETER (MG=16, NBANDS=16)                                               RRTM_INIT.544
      PARAMETER (NGPT=140, NG3=16)                                               RRTM_INIT.545
C Input                                                                          RRTM_INIT.546
      COMMON /K3/       KA(10,5,13,MG), KB(5,5,13:59,MG), SELFREF(10,MG)         RRTM_INIT.547
      COMMON /RWT/      RWGT(MG*NBANDS)                                          RRTM_INIT.548
      COMMON /FEATUREC/ NGC(NBANDS), NGS(NBANDS), NGN(NGPT), NGB(NGPT)           RRTM_INIT.549
C Output                                                                         RRTM_INIT.550
      COMMON /K3C/   KAC(10,5,13,NG3), KBC(5,5,13:59,NG3),                       RRTM_INIT.551
     &               SELFREFC(10,NG3), FORREFC(NG3),                             RRTM_INIT.552
     &               ABSN2OAC(NG3), ABSN2OBC(NG3)                                RRTM_INIT.553
      COMMON /PF3C/  FRACREFAC(NG3,10), FRACREFBC(NG3,5)                         RRTM_INIT.554
      COMMON /PFDAT/ FREFA(NGPT,13),FREFB(NGPT,6),                               RRTM_INIT.555
     &               FREFADF(NGPT,13),FREFBDF(NGPT,6)                            RRTM_INIT.556
                                                                                 RRTM_INIT.557
C Data                                                                           RRTM_INIT.558
      DIMENSION FRACREFA(MG,10), FRACREFB(MG,5)                                  RRTM_INIT.559
      DIMENSION FORREF(MG), ABSN2OA(MG), ABSN2OB(MG)                             RRTM_INIT.560
                                                                                 RRTM_INIT.561
      DATA FRACREFA/                                                             RRTM_INIT.562
C     From P = 1053.6 mb.                                                        RRTM_INIT.563
     &    0.15116400,0.14875700,0.14232300,0.13234501,                           RRTM_INIT.564
     &    0.11881600,0.10224100,0.08345580,0.06267490,                           RRTM_INIT.565
     &    0.04250650,0.00462650,0.00382259,0.00302600,                           RRTM_INIT.566
     &    0.00222004,0.00141397,0.00053379,0.00007421,                           RRTM_INIT.567
     &    0.15266000,0.14888400,0.14195900,0.13179500,                           RRTM_INIT.568
     &    0.11842700,0.10209000,0.08336130,0.06264370,                           RRTM_INIT.569
     &    0.04247660,0.00461946,0.00381536,0.00302601,                           RRTM_INIT.570
     &    0.00222004,0.00141397,0.00053302,0.00007498,                           RRTM_INIT.571
     &    0.15282799,0.14903000,0.14192399,0.13174300,                           RRTM_INIT.572
     &    0.11835300,0.10202700,0.08329830,0.06264830,                           RRTM_INIT.573
     &    0.04246910,0.00460242,0.00381904,0.00301573,                           RRTM_INIT.574
     &    0.00222004,0.00141397,0.00053379,0.00007421,                           RRTM_INIT.575
     &    0.15298399,0.14902800,0.14193401,0.13173500,                           RRTM_INIT.576
     &    0.11833300,0.10195800,0.08324730,0.06264770,                           RRTM_INIT.577
     &    0.04246490,0.00460489,0.00381123,0.00301893,                           RRTM_INIT.578
     &    0.00221093,0.00141397,0.00053379,0.00007421,                           RRTM_INIT.579
     &    0.15307599,0.14907201,0.14198899,0.13169800,                           RRTM_INIT.580
     &    0.11827300,0.10192300,0.08321600,0.06263490,                           RRTM_INIT.581
     &    0.04245600,0.00460846,0.00380836,0.00301663,                           RRTM_INIT.582
     &    0.00221402,0.00141167,0.00052807,0.00007376,                           RRTM_INIT.583
     &    0.15311401,0.14915401,0.14207301,0.13167299,                           RRTM_INIT.584
     &    0.11819300,0.10188900,0.08318760,0.06261960,                           RRTM_INIT.585
     &    0.04243890,0.00461584,0.00380929,0.00300815,                           RRTM_INIT.586
     &    0.00221736,0.00140588,0.00052776,0.00007376,                           RRTM_INIT.587
     &    0.15316001,0.14925499,0.14213000,0.13170999,                           RRTM_INIT.588
     &    0.11807700,0.10181400,0.08317400,0.06260300,                           RRTM_INIT.589
     &    0.04242720,0.00461520,0.00381381,0.00301285,                           RRTM_INIT.590
     &    0.00220275,0.00140371,0.00052776,0.00007376,                           RRTM_INIT.591
     &    0.15321200,0.14940999,0.14222500,0.13164200,                           RRTM_INIT.592
     &    0.11798200,0.10174500,0.08317500,0.06253640,                           RRTM_INIT.593
     &    0.04243130,0.00461724,0.00381534,0.00300320,                           RRTM_INIT.594
     &    0.00220091,0.00140364,0.00052852,0.00007300,                           RRTM_INIT.595
     &    0.15312800,0.14973100,0.14234400,0.13168900,                           RRTM_INIT.596
     &    0.11795200,0.10156100,0.08302990,0.06252240,                           RRTM_INIT.597
     &    0.04240980,0.00461035,0.00381381,0.00300176,                           RRTM_INIT.598
     &    0.00220160,0.00140284,0.00052774,0.00007376,                           RRTM_INIT.599
     &    0.15292500,0.14978001,0.14242400,0.13172600,                           RRTM_INIT.600
     &    0.11798800,0.10156400,0.08303050,0.06251670,                           RRTM_INIT.601
     &    0.04240970,0.00461302,0.00381452,0.00300250,                           RRTM_INIT.602
     &    0.00220126,0.00140324,0.00052850,0.00007300/                           RRTM_INIT.603
      DATA FRACREFB/                                                             RRTM_INIT.604
C     From P = 64.1 mb.                                                          RRTM_INIT.605
     &    0.16340201,0.15607700,0.14601400,0.13182700,                           RRTM_INIT.606
     &    0.11524700,0.09666570,0.07825360,0.05849780,                           RRTM_INIT.607
     &    0.03949650,0.00427980,0.00353719,0.00279303,                           RRTM_INIT.608
     &    0.00204788,0.00130139,0.00049055,0.00006904,                           RRTM_INIT.609
     &    0.15762900,0.15494700,0.14659800,0.13267800,                           RRTM_INIT.610
     &    0.11562700,0.09838360,0.07930420,0.05962700,                           RRTM_INIT.611
     &    0.04036360,0.00438053,0.00361463,0.00285723,                           RRTM_INIT.612
     &    0.00208345,0.00132135,0.00050528,0.00008003,                           RRTM_INIT.613
     &    0.15641500,0.15394500,0.14633600,0.13180400,                           RRTM_INIT.614
     &    0.11617100,0.09924170,0.08000510,0.06021420,                           RRTM_INIT.615
     &    0.04082730,0.00441694,0.00365364,0.00287723,                           RRTM_INIT.616
     &    0.00210914,0.00135784,0.00054651,0.00008003,                           RRTM_INIT.617
     &    0.15482700,0.15286300,0.14392500,0.13244100,                           RRTM_INIT.618
     &    0.11712000,0.09994920,0.08119200,0.06104360,                           RRTM_INIT.619
     &    0.04135600,0.00446685,0.00368377,0.00290767,                           RRTM_INIT.620
     &    0.00215445,0.00142865,0.00056142,0.00008003,                           RRTM_INIT.621
     &    0.15975100,0.15653500,0.14214399,0.12892200,                           RRTM_INIT.622
     &    0.11508400,0.09906020,0.08087940,0.06078190,                           RRTM_INIT.623
     &    0.04140530,0.00452724,0.00374558,0.00295328,                           RRTM_INIT.624
     &    0.00218509,0.00138644,0.00056018,0.00008003/                           RRTM_INIT.625
                                                                                 RRTM_INIT.626
      DATA ABSN2OA/                                                              RRTM_INIT.627
     &    1.50387E-01,2.91407E-01,6.28803E-01,9.65619E-01,                       RRTM_INIT.628
     &    1.15054E-00,2.23424E-00,1.83392E-00,1.39033E-00,                       RRTM_INIT.629
     &    4.28457E-01,2.73502E-01,1.84307E-01,1.61325E-01,                       RRTM_INIT.630
     &    7.66314E-02,1.33862E-01,6.71196E-07,1.59293E-06/                       RRTM_INIT.631
      DATA ABSN2OB/                                                              RRTM_INIT.632
     &    9.37044E-05,1.23318E-03,7.91720E-03,5.33005E-02,                       RRTM_INIT.633
     &    1.72343E-01,4.29571E-01,1.01288E+00,3.83863E+00,                       RRTM_INIT.634
     &    1.15312E+01,1.08383E+00,2.24847E+00,1.51268E+00,                       RRTM_INIT.635
     &    3.33177E-01,7.82102E-01,3.44631E-01,1.61039E-03/                       RRTM_INIT.636
      DATA FORREF/                                                               RRTM_INIT.637
     &    1.76842E-04, 1.77913E-04, 1.25186E-04, 1.07912E-04,                    RRTM_INIT.638
     &    1.05217E-04, 7.48726E-05, 1.11701E-04, 7.68921E-05,                    RRTM_INIT.639
     &    9.87242E-05, 9.85711E-05, 6.16557E-05,-1.61291E-05,                    RRTM_INIT.640
     &   -1.26794E-04,-1.19011E-04,-2.67814E-04, 6.95005E-05/                    RRTM_INIT.641
                                                                                 RRTM_INIT.642
      REAL KA,KB,KAC,KBC                                                         RRTM_INIT.643
                                                                                 RRTM_INIT.644
      DO 2000 JN = 1,10                                                          RRTM_INIT.645
         DO 2000 JT = 1,5                                                        RRTM_INIT.646
            DO 2200 JP = 1,13                                                    RRTM_INIT.647
               IPRSM = 0                                                         RRTM_INIT.648
               DO 2400 IGC = 1,NGC(3)                                            RRTM_INIT.649
                 SUMK = 0.                                                       RRTM_INIT.650
                  DO 2600 IPR = 1, NGN(NGS(2)+IGC)                               RRTM_INIT.651
                     IPRSM = IPRSM + 1                                           RRTM_INIT.652
                     SUMK = SUMK + KA(JN,JT,JP,IPRSM)*RWGT(IPRSM+32)             RRTM_INIT.653
 2600             CONTINUE                                                       RRTM_INIT.654
                  KAC(JN,JT,JP,IGC) = SUMK                                       RRTM_INIT.655
 2400          CONTINUE                                                          RRTM_INIT.656
 2200       CONTINUE                                                             RRTM_INIT.657
 2000 CONTINUE                                                                   RRTM_INIT.658
      DO 3000 JN = 1,5                                                           RRTM_INIT.659
         DO 3000 JT = 1,5                                                        RRTM_INIT.660
            DO 3200 JP = 13,59                                                   RRTM_INIT.661
               IPRSM = 0                                                         RRTM_INIT.662
               DO 3400 IGC = 1,NGC(3)                                            RRTM_INIT.663
                  SUMK = 0.                                                      RRTM_INIT.664
                  DO 3600 IPR = 1, NGN(NGS(2)+IGC)                               RRTM_INIT.665
                     IPRSM = IPRSM + 1                                           RRTM_INIT.666
                     SUMK = SUMK + KB(JN,JT,JP,IPRSM)*RWGT(IPRSM+32)             RRTM_INIT.667
 3600             CONTINUE                                                       RRTM_INIT.668
                  KBC(JN,JT,JP,IGC) = SUMK                                       RRTM_INIT.669
 3400          CONTINUE                                                          RRTM_INIT.670
 3200       CONTINUE                                                             RRTM_INIT.671
 3000 CONTINUE                                                                   RRTM_INIT.672
                                                                                 RRTM_INIT.673
      DO 4000 JT = 1,10                                                          RRTM_INIT.674
         IPRSM = 0                                                               RRTM_INIT.675
         DO 4400 IGC = 1,NGC(3)                                                  RRTM_INIT.676
            SUMK = 0.                                                            RRTM_INIT.677
            SUMF = 0.                                                            RRTM_INIT.678
            DO 4600 IPR = 1, NGN(NGS(2)+IGC)                                     RRTM_INIT.679
               IPRSM = IPRSM + 1                                                 RRTM_INIT.680
               SUMK = SUMK + SELFREF(JT,IPRSM)*RWGT(IPRSM+32)                    RRTM_INIT.681
               SUMF = SUMF + FRACREFA(IPRSM,JT)                                  RRTM_INIT.682
 4600       CONTINUE                                                             RRTM_INIT.683
            SELFREFC(JT,IGC) = SUMK                                              RRTM_INIT.684
            FRACREFAC(IGC,JT) = SUMF                                             RRTM_INIT.685
 4400    CONTINUE                                                                RRTM_INIT.686
 4000 CONTINUE                                                                   RRTM_INIT.687
                                                                                 RRTM_INIT.688
      DO 5000 JP = 1,5                                                           RRTM_INIT.689
         IPRSM = 0                                                               RRTM_INIT.690
         DO 5400 IGC = 1,NGC(3)                                                  RRTM_INIT.691
            SUMF = 0.                                                            RRTM_INIT.692
            DO 5600 IPR = 1, NGN(NGS(2)+IGC)                                     RRTM_INIT.693
               IPRSM = IPRSM + 1                                                 RRTM_INIT.694
               SUMF = SUMF + FRACREFB(IPRSM,JP)                                  RRTM_INIT.695
 5600       CONTINUE                                                             RRTM_INIT.696
            FRACREFBC(IGC,JP) = SUMF                                             RRTM_INIT.697
 5400    CONTINUE                                                                RRTM_INIT.698
 5000 CONTINUE                                                                   RRTM_INIT.699
                                                                                 RRTM_INIT.700
      IPRSM = 0                                                                  RRTM_INIT.701
      DO 6400 IGC = 1,NGC(3)                                                     RRTM_INIT.702
         SUMK1= 0.                                                               RRTM_INIT.703
         SUMK2= 0.                                                               RRTM_INIT.704
         SUMK3= 0.                                                               RRTM_INIT.705
         DO 6600 IPR = 1, NGN(NGS(2)+IGC)                                        RRTM_INIT.706
            IPRSM = IPRSM + 1                                                    RRTM_INIT.707
            SUMK1= SUMK1+ FORREF(IPRSM)*RWGT(IPRSM+32)                           RRTM_INIT.708
            SUMK2= SUMK2+ ABSN2OA(IPRSM)*RWGT(IPRSM+32)                          RRTM_INIT.709
            SUMK3= SUMK3+ ABSN2OB(IPRSM)*RWGT(IPRSM+32)                          RRTM_INIT.710
 6600    CONTINUE                                                                RRTM_INIT.711
         FORREFC(IGC) = SUMK1                                                    RRTM_INIT.712
         ABSN2OAC(IGC) = SUMK2                                                   RRTM_INIT.713
         ABSN2OBC(IGC) = SUMK3                                                   RRTM_INIT.714
 6400 CONTINUE                                                                   RRTM_INIT.715
                                                                                 RRTM_INIT.716
      DO 8000 JP = 1,10                                                          RRTM_INIT.717
         DO 8000 IGC = 1,NGC(3)                                                  RRTM_INIT.718
            FREFA(NGS(2)+IGC,JP) = FRACREFAC(IGC,JP)                             RRTM_INIT.719
 8000 CONTINUE                                                                   RRTM_INIT.720
      DO 8100 JP = 1,9                                                           RRTM_INIT.721
         DO 8100 IGC = 1,NGC(3)                                                  RRTM_INIT.722
            FREFADF(NGS(2)+IGC,JP) = FRACREFAC(IGC,JP+1) -                       RRTM_INIT.723
     &                               FRACREFAC(IGC,JP)                           RRTM_INIT.724
 8100 CONTINUE                                                                   RRTM_INIT.725
      DO 8200 JP = 1,5                                                           RRTM_INIT.726
         DO 8200 IGC = 1,NGC(3)                                                  RRTM_INIT.727
            FREFB(NGS(2)+IGC,JP) = FRACREFBC(IGC,JP)                             RRTM_INIT.728
 8200 CONTINUE                                                                   RRTM_INIT.729
      DO 8300 JP = 1,4                                                           RRTM_INIT.730
         DO 8300 IGC = 1,NGC(3)                                                  RRTM_INIT.731
            FREFBDF(NGS(2)+IGC,JP) = FRACREFBC(IGC,JP+1) -                       RRTM_INIT.732
     &                               FRACREFBC(IGC,JP)                           RRTM_INIT.733
 8300 CONTINUE                                                                   RRTM_INIT.734
                                                                                 RRTM_INIT.735
      RETURN                                                                     RRTM_INIT.736
      END                                                                        RRTM_INIT.737
                                                                                 RRTM_INIT.738
C***************************************************************************     RRTM_INIT.739
      SUBROUTINE CMBGB4                                                          RRTM_INIT.740
C***************************************************************************     RRTM_INIT.741
C                                                                                RRTM_INIT.742
C     BAND 4:  630-700 cm-1 (low - H2O,CO2; high - O3,CO2)                       RRTM_INIT.743
C***************************************************************************     RRTM_INIT.744
                                                                                 RRTM_INIT.745
C Parameters                                                                     RRTM_INIT.746
      PARAMETER (MG=16, NBANDS=16)                                               RRTM_INIT.747
      PARAMETER (NGPT=140, NG4=14)                                               RRTM_INIT.748
C Input                                                                          RRTM_INIT.749
      COMMON /K4/       KA(9,5,13,MG), KB(6,5,13:59,MG) , SELFREF(10,MG)         RRTM_INIT.750
      COMMON /RWT/      RWGT(MG*NBANDS)                                          RRTM_INIT.751
      COMMON /FEATUREC/ NGC(NBANDS), NGS(NBANDS), NGN(NGPT), NGB(NGPT)           RRTM_INIT.752
C Output                                                                         RRTM_INIT.753
      COMMON /K4C/   KAC(9,5,13,NG4), KBC(6,5,13:59,NG4),                        RRTM_INIT.754
     &               SELFREFC(10,NG4)                                            RRTM_INIT.755
      COMMON /PF4C/  FRACREFAC(NG4,9), FRACREFBC(NG4,6)                          RRTM_INIT.756
      COMMON /PFDAT/ FREFA(NGPT,13),FREFB(NGPT,6),                               RRTM_INIT.757
     &               FREFADF(NGPT,13),FREFBDF(NGPT,6)                            RRTM_INIT.758
                                                                                 RRTM_INIT.759
C Data                                                                           RRTM_INIT.760
      DIMENSION FRACREFA(MG,9), FRACREFB(MG,6)                                   RRTM_INIT.761
                                                                                 RRTM_INIT.762
      DATA FRACREFA/                                                             RRTM_INIT.763
C     From P =                                                                   RRTM_INIT.764
     &    0.15579100,0.14918099,0.14113800,0.13127001,                           RRTM_INIT.765
     &    0.11796300,0.10174300,0.08282370,0.06238150,                           RRTM_INIT.766
     &    0.04213440,0.00458968,0.00377949,0.00298736,                           RRTM_INIT.767
     &    0.00220743,0.00140644,0.00053024,0.00007459,                           RRTM_INIT.768
     &    0.15292799,0.15004000,0.14211500,0.13176700,                           RRTM_INIT.769
     &    0.11821100,0.10186300,0.08288040,0.06241390,                           RRTM_INIT.770
     &    0.04220720,0.00459006,0.00377919,0.00298743,                           RRTM_INIT.771
     &    0.00220743,0.00140644,0.00053024,0.00007459,                           RRTM_INIT.772
     &    0.14386199,0.15125300,0.14650001,0.13377000,                           RRTM_INIT.773
     &    0.11895900,0.10229400,0.08312110,0.06239520,                           RRTM_INIT.774
     &    0.04225560,0.00459428,0.00378865,0.00298860,                           RRTM_INIT.775
     &    0.00220743,0.00140644,0.00053024,0.00007459,                           RRTM_INIT.776
     &    0.14359100,0.14561599,0.14479300,0.13740200,                           RRTM_INIT.777
     &    0.12150100,0.10315400,0.08355480,0.06247240,                           RRTM_INIT.778
     &    0.04230980,0.00459916,0.00378373,0.00300063,                           RRTM_INIT.779
     &    0.00221111,0.00140644,0.00053024,0.00007459,                           RRTM_INIT.780
     &    0.14337599,0.14451601,0.14238000,0.13520500,                           RRTM_INIT.781
     &    0.12354200,0.10581200,0.08451810,0.06262440,                           RRTM_INIT.782
     &    0.04239590,0.00460297,0.00378701,0.00300466,                           RRTM_INIT.783
     &    0.00221899,0.00141020,0.00053024,0.00007459,                           RRTM_INIT.784
     &    0.14322001,0.14397401,0.14117201,0.13401900,                           RRTM_INIT.785
     &    0.12255500,0.10774100,0.08617650,0.06296420,                           RRTM_INIT.786
     &    0.04249590,0.00463406,0.00378241,0.00302037,                           RRTM_INIT.787
     &    0.00221583,0.00141103,0.00053814,0.00007991,                           RRTM_INIT.788
     &    0.14309500,0.14364301,0.14043900,0.13348100,                           RRTM_INIT.789
     &    0.12211600,0.10684700,0.08820590,0.06374610,                           RRTM_INIT.790
     &    0.04264730,0.00464231,0.00384022,0.00303427,                           RRTM_INIT.791
     &    0.00221825,0.00140943,0.00055564,0.00007991,                           RRTM_INIT.792
     &    0.15579100,0.14918099,0.14113800,0.13127001,                           RRTM_INIT.793
     &    0.11796300,0.10174300,0.08282370,0.06238150,                           RRTM_INIT.794
     &    0.04213440,0.00458968,0.00377949,0.00298736,                           RRTM_INIT.795
     &    0.00220743,0.00140644,0.00053024,0.00007459,                           RRTM_INIT.796
     &    0.15937001,0.15159500,0.14242800,0.13078900,                           RRTM_INIT.797
     &    0.11671300,0.10035700,0.08143450,0.06093850,                           RRTM_INIT.798
     &    0.04105320,0.00446233,0.00369844,0.00293784,                           RRTM_INIT.799
     &    0.00216425,0.00143403,0.00054571,0.00007991/                           RRTM_INIT.800
      DATA FRACREFB/                                                             RRTM_INIT.801
C     From P = 1.17 mb.                                                          RRTM_INIT.802
     &    0.15558299,0.14930600,0.14104301,0.13124099,                           RRTM_INIT.803
     &    0.11792900,0.10159200,0.08314130,0.06240450,                           RRTM_INIT.804
     &    0.04217020,0.00459313,0.00379798,0.00299835,                           RRTM_INIT.805
     &    0.00218950,0.00140615,0.00053010,0.00007457,                           RRTM_INIT.806
     &    0.15592700,0.14918999,0.14095700,0.13115700,                           RRTM_INIT.807
     &    0.11788900,0.10158000,0.08313780,0.06240240,                           RRTM_INIT.808
     &    0.04217000,0.00459313,0.00379798,0.00299835,                           RRTM_INIT.809
     &    0.00218950,0.00140615,0.00053010,0.00007457,                           RRTM_INIT.810
     &    0.15949000,0.15014900,0.14162201,0.13080800,                           RRTM_INIT.811
     &    0.11713500,0.10057100,0.08170080,0.06128110,                           RRTM_INIT.812
     &    0.04165600,0.00459202,0.00379835,0.00299717,                           RRTM_INIT.813
     &    0.00218958,0.00140616,0.00053010,0.00007457,                           RRTM_INIT.814
     &    0.15967900,0.15038200,0.14196999,0.13074800,                           RRTM_INIT.815
     &    0.11701700,0.10053000,0.08160790,0.06122690,                           RRTM_INIT.816
     &    0.04128310,0.00456598,0.00379486,0.00299457,                           RRTM_INIT.817
     &    0.00219016,0.00140619,0.00053011,0.00007456,                           RRTM_INIT.818
     &    0.15989800,0.15057300,0.14207700,0.13068600,                           RRTM_INIT.819
     &    0.11682900,0.10053900,0.08163610,0.06121870,                           RRTM_INIT.820
     &    0.04121690,0.00449061,0.00371235,0.00294207,                           RRTM_INIT.821
     &    0.00217778,0.00139877,0.00053011,0.00007455,                           RRTM_INIT.822
     &    0.15950100,0.15112500,0.14199100,0.13071300,                           RRTM_INIT.823
     &    0.11680800,0.10054600,0.08179050,0.06120910,                           RRTM_INIT.824
     &    0.04126050,0.00444324,0.00366843,0.00289369,                           RRTM_INIT.825
     &    0.00211550,0.00134746,0.00050874,0.00007863/                           RRTM_INIT.826
                                                                                 RRTM_INIT.827
      REAL KA,KB,KAC,KBC                                                         RRTM_INIT.828
                                                                                 RRTM_INIT.829
      DO 2000 JN = 1,9                                                           RRTM_INIT.830
         DO 2000 JT = 1,5                                                        RRTM_INIT.831
            DO 2200 JP = 1,13                                                    RRTM_INIT.832
               IPRSM = 0                                                         RRTM_INIT.833
               DO 2400 IGC = 1,NGC(4)                                            RRTM_INIT.834
                 SUMK = 0.                                                       RRTM_INIT.835
                  DO 2600 IPR = 1, NGN(NGS(3)+IGC)                               RRTM_INIT.836
                     IPRSM = IPRSM + 1                                           RRTM_INIT.837
                     SUMK = SUMK + KA(JN,JT,JP,IPRSM)*RWGT(IPRSM+48)             RRTM_INIT.838
 2600             CONTINUE                                                       RRTM_INIT.839
                  KAC(JN,JT,JP,IGC) = SUMK                                       RRTM_INIT.840
 2400          CONTINUE                                                          RRTM_INIT.841
 2200       CONTINUE                                                             RRTM_INIT.842
 2000 CONTINUE                                                                   RRTM_INIT.843
      DO 3000 JN = 1,6                                                           RRTM_INIT.844
         DO 3000 JT = 1,5                                                        RRTM_INIT.845
            DO 3200 JP = 13,59                                                   RRTM_INIT.846
               IPRSM = 0                                                         RRTM_INIT.847
               DO 3400 IGC = 1,NGC(4)                                            RRTM_INIT.848
                  SUMK = 0.                                                      RRTM_INIT.849
                  DO 3600 IPR = 1, NGN(NGS(3)+IGC)                               RRTM_INIT.850
                     IPRSM = IPRSM + 1                                           RRTM_INIT.851
                     SUMK = SUMK + KB(JN,JT,JP,IPRSM)*RWGT(IPRSM+48)             RRTM_INIT.852
 3600             CONTINUE                                                       RRTM_INIT.853
                  KBC(JN,JT,JP,IGC) = SUMK                                       RRTM_INIT.854
 3400          CONTINUE                                                          RRTM_INIT.855
 3200       CONTINUE                                                             RRTM_INIT.856
 3000 CONTINUE                                                                   RRTM_INIT.857
                                                                                 RRTM_INIT.858
      DO 4000 JT = 1,10                                                          RRTM_INIT.859
         IPRSM = 0                                                               RRTM_INIT.860
         DO 4400 IGC = 1,NGC(4)                                                  RRTM_INIT.861
            SUMK = 0.                                                            RRTM_INIT.862
            DO 4600 IPR = 1, NGN(NGS(3)+IGC)                                     RRTM_INIT.863
               IPRSM = IPRSM + 1                                                 RRTM_INIT.864
               SUMK = SUMK + SELFREF(JT,IPRSM)*RWGT(IPRSM+48)                    RRTM_INIT.865
 4600       CONTINUE                                                             RRTM_INIT.866
            SELFREFC(JT,IGC) = SUMK                                              RRTM_INIT.867
 4400    CONTINUE                                                                RRTM_INIT.868
 4000 CONTINUE                                                                   RRTM_INIT.869
                                                                                 RRTM_INIT.870
      DO 5000 JP = 1,9                                                           RRTM_INIT.871
         IPRSM = 0                                                               RRTM_INIT.872
         DO 5400 IGC = 1,NGC(4)                                                  RRTM_INIT.873
            SUMF = 0.                                                            RRTM_INIT.874
            DO 5600 IPR = 1, NGN(NGS(3)+IGC)                                     RRTM_INIT.875
               IPRSM = IPRSM + 1                                                 RRTM_INIT.876
               SUMF = SUMF + FRACREFA(IPRSM,JP)                                  RRTM_INIT.877
 5600       CONTINUE                                                             RRTM_INIT.878
            FRACREFAC(IGC,JP) = SUMF                                             RRTM_INIT.879
 5400    CONTINUE                                                                RRTM_INIT.880
 5000 CONTINUE                                                                   RRTM_INIT.881
                                                                                 RRTM_INIT.882
      DO 6000 JP = 1,6                                                           RRTM_INIT.883
         IPRSM = 0                                                               RRTM_INIT.884
         DO 6400 IGC = 1,NGC(4)                                                  RRTM_INIT.885
            SUMF = 0.                                                            RRTM_INIT.886
            DO 6600 IPR = 1, NGN(NGS(3)+IGC)                                     RRTM_INIT.887
               IPRSM = IPRSM + 1                                                 RRTM_INIT.888
               SUMF = SUMF + FRACREFB(IPRSM,JP)                                  RRTM_INIT.889
 6600       CONTINUE                                                             RRTM_INIT.890
            FRACREFBC(IGC,JP) = SUMF                                             RRTM_INIT.891
 6400    CONTINUE                                                                RRTM_INIT.892
 6000 CONTINUE                                                                   RRTM_INIT.893
                                                                                 RRTM_INIT.894
      DO 8000 JP = 1,9                                                           RRTM_INIT.895
         DO 8000 IGC = 1,NGC(4)                                                  RRTM_INIT.896
            FREFA(NGS(3)+IGC,JP) = FRACREFAC(IGC,JP)                             RRTM_INIT.897
 8000 CONTINUE                                                                   RRTM_INIT.898
      DO 8100 JP = 1,8                                                           RRTM_INIT.899
         DO 8100 IGC = 1,NGC(4)                                                  RRTM_INIT.900
            FREFADF(NGS(3)+IGC,JP) = FRACREFAC(IGC,JP+1) -                       RRTM_INIT.901
     &                               FRACREFAC(IGC,JP)                           RRTM_INIT.902
 8100 CONTINUE                                                                   RRTM_INIT.903
      DO 8200 JP = 1,6                                                           RRTM_INIT.904
         DO 8200 IGC = 1,NGC(4)                                                  RRTM_INIT.905
            FREFB(NGS(3)+IGC,JP) = FRACREFBC(IGC,JP)                             RRTM_INIT.906
 8200 CONTINUE                                                                   RRTM_INIT.907
      DO 8300 JP = 1,5                                                           RRTM_INIT.908
         DO 8300 IGC = 1,NGC(4)                                                  RRTM_INIT.909
            FREFBDF(NGS(3)+IGC,JP) = FRACREFBC(IGC,JP+1) -                       RRTM_INIT.910
     &                               FRACREFBC(IGC,JP)                           RRTM_INIT.911
 8300 CONTINUE                                                                   RRTM_INIT.912
                                                                                 RRTM_INIT.913
      RETURN                                                                     RRTM_INIT.914
      END                                                                        RRTM_INIT.915
                                                                                 RRTM_INIT.916
C***************************************************************************     RRTM_INIT.917
      SUBROUTINE CMBGB5                                                          RRTM_INIT.918
C***************************************************************************     RRTM_INIT.919
C                                                                                RRTM_INIT.920
C     BAND 5:  700-820 cm-1 (low - H2O,CO2; high - O3,CO2)                       RRTM_INIT.921
C***************************************************************************     RRTM_INIT.922
                                                                                 RRTM_INIT.923
C Parameters                                                                     RRTM_INIT.924
      PARAMETER (MG=16, NBANDS=16)                                               RRTM_INIT.925
      PARAMETER (NGPT=140, NG5=16)                                               RRTM_INIT.926
C Input                                                                          RRTM_INIT.927
      COMMON /K5/       KA(9,5,13,MG), KB(5,5,13:59,MG), SELFREF(10,MG)          RRTM_INIT.928
      COMMON /RWT/      RWGT(MG*NBANDS)                                          RRTM_INIT.929
      COMMON /FEATUREC/ NGC(NBANDS), NGS(NBANDS), NGN(NGPT), NGB(NGPT)           RRTM_INIT.930
C Output                                                                         RRTM_INIT.931
      COMMON /K5C/   KAC(9,5,13,NG5), KBC(5,5,13:59,NG5),                        RRTM_INIT.932
     &               SELFREFC(10,NG5),                                           RRTM_INIT.933
     &               CCL4C(NG5)                                                  RRTM_INIT.934
      COMMON /PF5C/  FRACREFAC(NG5,9), FRACREFBC(NG5,5)                          RRTM_INIT.935
      COMMON /PFDAT/ FREFA(NGPT,13),FREFB(NGPT,6),                               RRTM_INIT.936
     &               FREFADF(NGPT,13),FREFBDF(NGPT,6)                            RRTM_INIT.937
                                                                                 RRTM_INIT.938
C Data                                                                           RRTM_INIT.939
      DIMENSION FRACREFA(MG,9), FRACREFB(MG,5), CCL4(MG)                         RRTM_INIT.940
                                                                                 RRTM_INIT.941
      DATA FRACREFA/                                                             RRTM_INIT.942
C     From P = 387.6 mb.                                                         RRTM_INIT.943
     &    0.13966499,0.14138900,0.13763399,0.13076700,                           RRTM_INIT.944
     &    0.12299100,0.10747700,0.08942000,0.06769200,                           RRTM_INIT.945
     &    0.04587610,0.00501173,0.00415809,0.00328398,                           RRTM_INIT.946
     &    0.00240015,0.00156222,0.00059104,0.00008323,                           RRTM_INIT.947
     &    0.13958199,0.14332899,0.13785399,0.13205400,                           RRTM_INIT.948
     &    0.12199700,0.10679600,0.08861080,0.06712320,                           RRTM_INIT.949
     &    0.04556030,0.00500863,0.00416315,0.00328629,                           RRTM_INIT.950
     &    0.00240023,0.00156220,0.00059104,0.00008323,                           RRTM_INIT.951
     &    0.13907100,0.14250501,0.13889600,0.13297300,                           RRTM_INIT.952
     &    0.12218700,0.10683800,0.08839260,0.06677310,                           RRTM_INIT.953
     &    0.04538570,0.00495402,0.00409863,0.00328219,                           RRTM_INIT.954
     &    0.00240805,0.00156266,0.00059104,0.00008323,                           RRTM_INIT.955
     &    0.13867700,0.14190100,0.13932300,0.13327099,                           RRTM_INIT.956
     &    0.12280800,0.10692500,0.08844510,0.06658510,                           RRTM_INIT.957
     &    0.04519340,0.00492276,0.00408832,0.00323856,                           RRTM_INIT.958
     &    0.00239289,0.00155698,0.00059104,0.00008323,                           RRTM_INIT.959
     &    0.13845000,0.14158800,0.13929300,0.13295600,                           RRTM_INIT.960
     &    0.12348300,0.10736700,0.08859480,0.06650610,                           RRTM_INIT.961
     &    0.04498230,0.00491335,0.00406968,0.00322901,                           RRTM_INIT.962
     &    0.00234666,0.00155235,0.00058813,0.00008323,                           RRTM_INIT.963
     &    0.13837101,0.14113200,0.13930500,0.13283101,                           RRTM_INIT.964
     &    0.12349200,0.10796400,0.08890490,0.06646480,                           RRTM_INIT.965
     &    0.04485990,0.00489554,0.00405264,0.00320313,                           RRTM_INIT.966
     &    0.00234742,0.00151159,0.00058438,0.00008253,                           RRTM_INIT.967
     &    0.13834500,0.14093500,0.13896500,0.13262001,                           RRTM_INIT.968
     &    0.12326900,0.10828900,0.08950050,0.06674610,                           RRTM_INIT.969
     &    0.04476560,0.00489624,0.00400962,0.00317423,                           RRTM_INIT.970
     &    0.00233479,0.00148249,0.00058590,0.00008253,                           RRTM_INIT.971
     &    0.13831300,0.14069000,0.13871400,0.13247600,                           RRTM_INIT.972
     &    0.12251400,0.10831300,0.08977090,0.06776920,                           RRTM_INIT.973
     &    0.04498390,0.00484111,0.00398948,0.00316069,                           RRTM_INIT.974
     &    0.00229741,0.00150104,0.00058608,0.00008253,                           RRTM_INIT.975
     &    0.14027201,0.14420401,0.14215700,0.13446601,                           RRTM_INIT.976
     &    0.12303700,0.10596100,0.08650370,0.06409570,                           RRTM_INIT.977
     &    0.04312310,0.00471110,0.00393954,0.00310850,                           RRTM_INIT.978
     &    0.00229588,0.00146366,0.00058194,0.00008253/                           RRTM_INIT.979
      DATA FRACREFB/                                                             RRTM_INIT.980
C     From P = 1.17 mb.                                                          RRTM_INIT.981
     &    0.14339100,0.14358699,0.13935301,0.13306700,                           RRTM_INIT.982
     &    0.12135700,0.10590600,0.08688240,0.06553220,                           RRTM_INIT.983
     &    0.04446740,0.00483580,0.00399413,0.00316225,                           RRTM_INIT.984
     &    0.00233007,0.00149135,0.00056246,0.00008059,                           RRTM_INIT.985
     &    0.14330500,0.14430299,0.14053699,0.13355300,                           RRTM_INIT.986
     &    0.12151200,0.10529100,0.08627630,0.06505230,                           RRTM_INIT.987
     &    0.04385850,0.00476555,0.00395010,0.00313878,                           RRTM_INIT.988
     &    0.00232273,0.00149354,0.00056246,0.00008059,                           RRTM_INIT.989
     &    0.14328399,0.14442700,0.14078601,0.13390100,                           RRTM_INIT.990
     &    0.12132600,0.10510600,0.08613660,0.06494630,                           RRTM_INIT.991
     &    0.04381310,0.00475378,0.00394166,0.00313076,                           RRTM_INIT.992
     &    0.00231235,0.00149159,0.00056301,0.00008059,                           RRTM_INIT.993
     &    0.14326900,0.14453100,0.14114200,0.13397101,                           RRTM_INIT.994
     &    0.12127200,0.10493400,0.08601380,0.06483360,                           RRTM_INIT.995
     &    0.04378900,0.00474655,0.00393549,0.00312583,                           RRTM_INIT.996
     &    0.00230686,0.00148433,0.00056502,0.00008059,                           RRTM_INIT.997
     &    0.14328900,0.14532700,0.14179000,0.13384600,                           RRTM_INIT.998
     &    0.12093700,0.10461500,0.08573010,0.06461340,                           RRTM_INIT.999
     &    0.04366570,0.00473087,0.00392539,0.00311238,                           RRTM_INIT.1000
     &    0.00229865,0.00147572,0.00056517,0.00007939/                           RRTM_INIT.1001
                                                                                 RRTM_INIT.1002
      DATA CCL4/                                                                 RRTM_INIT.1003
     &     26.1407,  53.9776,  63.8085,  36.1701,                                RRTM_INIT.1004
     &     15.4099, 10.23116,  4.82948,  5.03836,                                RRTM_INIT.1005
     &     1.75558,0.,0.,0.,                                                     RRTM_INIT.1006
     &     0.,0.,0.,0./                                                          RRTM_INIT.1007
                                                                                 RRTM_INIT.1008
      REAL KA,KB,KAC,KBC                                                         RRTM_INIT.1009
                                                                                 RRTM_INIT.1010
      DO 2000 JN = 1,9                                                           RRTM_INIT.1011
         DO 2000 JT = 1,5                                                        RRTM_INIT.1012
            DO 2200 JP = 1,13                                                    RRTM_INIT.1013
               IPRSM = 0                                                         RRTM_INIT.1014
               DO 2400 IGC = 1,NGC(5)                                            RRTM_INIT.1015
                 SUMK = 0.                                                       RRTM_INIT.1016
                  DO 2600 IPR = 1, NGN(NGS(4)+IGC)                               RRTM_INIT.1017
                     IPRSM = IPRSM + 1                                           RRTM_INIT.1018
                     SUMK = SUMK + KA(JN,JT,JP,IPRSM)*RWGT(IPRSM+64)             RRTM_INIT.1019
 2600             CONTINUE                                                       RRTM_INIT.1020
                  KAC(JN,JT,JP,IGC) = SUMK                                       RRTM_INIT.1021
 2400          CONTINUE                                                          RRTM_INIT.1022
 2200       CONTINUE                                                             RRTM_INIT.1023
 2000 CONTINUE                                                                   RRTM_INIT.1024
      DO 3000 JN = 1,5                                                           RRTM_INIT.1025
         DO 3000 JT = 1,5                                                        RRTM_INIT.1026
            DO 3200 JP = 13,59                                                   RRTM_INIT.1027
               IPRSM = 0                                                         RRTM_INIT.1028
               DO 3400 IGC = 1,NGC(5)                                            RRTM_INIT.1029
                  SUMK = 0.                                                      RRTM_INIT.1030
                  DO 3600 IPR = 1, NGN(NGS(4)+IGC)                               RRTM_INIT.1031
                     IPRSM = IPRSM + 1                                           RRTM_INIT.1032
                     SUMK = SUMK + KB(JN,JT,JP,IPRSM)*RWGT(IPRSM+64)             RRTM_INIT.1033
 3600             CONTINUE                                                       RRTM_INIT.1034
                  KBC(JN,JT,JP,IGC) = SUMK                                       RRTM_INIT.1035
 3400          CONTINUE                                                          RRTM_INIT.1036
 3200       CONTINUE                                                             RRTM_INIT.1037
 3000 CONTINUE                                                                   RRTM_INIT.1038
                                                                                 RRTM_INIT.1039
      DO 4000 JT = 1,10                                                          RRTM_INIT.1040
         IPRSM = 0                                                               RRTM_INIT.1041
         DO 4400 IGC = 1,NGC(5)                                                  RRTM_INIT.1042
            SUMK = 0.                                                            RRTM_INIT.1043
            DO 4600 IPR = 1, NGN(NGS(4)+IGC)                                     RRTM_INIT.1044
               IPRSM = IPRSM + 1                                                 RRTM_INIT.1045
               SUMK = SUMK + SELFREF(JT,IPRSM)*RWGT(IPRSM+64)                    RRTM_INIT.1046
 4600       CONTINUE                                                             RRTM_INIT.1047
            SELFREFC(JT,IGC) = SUMK                                              RRTM_INIT.1048
 4400    CONTINUE                                                                RRTM_INIT.1049
 4000 CONTINUE                                                                   RRTM_INIT.1050
                                                                                 RRTM_INIT.1051
      DO 5000 JP = 1,9                                                           RRTM_INIT.1052
         IPRSM = 0                                                               RRTM_INIT.1053
         DO 5400 IGC = 1,NGC(5)                                                  RRTM_INIT.1054
            SUMF = 0.                                                            RRTM_INIT.1055
            DO 5600 IPR = 1, NGN(NGS(4)+IGC)                                     RRTM_INIT.1056
               IPRSM = IPRSM + 1                                                 RRTM_INIT.1057
               SUMF = SUMF + FRACREFA(IPRSM,JP)                                  RRTM_INIT.1058
 5600       CONTINUE                                                             RRTM_INIT.1059
            FRACREFAC(IGC,JP) = SUMF                                             RRTM_INIT.1060
 5400    CONTINUE                                                                RRTM_INIT.1061
 5000 CONTINUE                                                                   RRTM_INIT.1062
                                                                                 RRTM_INIT.1063
      DO 6000 JP = 1,5                                                           RRTM_INIT.1064
         IPRSM = 0                                                               RRTM_INIT.1065
         DO 6400 IGC = 1,NGC(5)                                                  RRTM_INIT.1066
            SUMF = 0.                                                            RRTM_INIT.1067
            DO 6600 IPR = 1, NGN(NGS(4)+IGC)                                     RRTM_INIT.1068
               IPRSM = IPRSM + 1                                                 RRTM_INIT.1069
               SUMF = SUMF + FRACREFB(IPRSM,JP)                                  RRTM_INIT.1070
 6600       CONTINUE                                                             RRTM_INIT.1071
            FRACREFBC(IGC,JP) = SUMF                                             RRTM_INIT.1072
 6400    CONTINUE                                                                RRTM_INIT.1073
 6000 CONTINUE                                                                   RRTM_INIT.1074
                                                                                 RRTM_INIT.1075
      IPRSM = 0                                                                  RRTM_INIT.1076
      DO 7400 IGC = 1,NGC(5)                                                     RRTM_INIT.1077
         SUMK = 0.                                                               RRTM_INIT.1078
         DO 7600 IPR = 1, NGN(NGS(4)+IGC)                                        RRTM_INIT.1079
            IPRSM = IPRSM + 1                                                    RRTM_INIT.1080
            SUMK = SUMK + CCL4(IPRSM)*RWGT(IPRSM+64)                             RRTM_INIT.1081
 7600    CONTINUE                                                                RRTM_INIT.1082
         CCL4C(IGC) = SUMK                                                       RRTM_INIT.1083
 7400 CONTINUE                                                                   RRTM_INIT.1084
                                                                                 RRTM_INIT.1085
      DO 8000 JP = 1,9                                                           RRTM_INIT.1086
         DO 8000 IGC = 1,NGC(5)                                                  RRTM_INIT.1087
            FREFA(NGS(4)+IGC,JP) = FRACREFAC(IGC,JP)                             RRTM_INIT.1088
 8000 CONTINUE                                                                   RRTM_INIT.1089
      DO 8100 JP = 1,8                                                           RRTM_INIT.1090
         DO 8100 IGC = 1,NGC(5)                                                  RRTM_INIT.1091
            FREFADF(NGS(4)+IGC,JP) = FRACREFAC(IGC,JP+1) -                       RRTM_INIT.1092
     &                               FRACREFAC(IGC,JP)                           RRTM_INIT.1093
 8100 CONTINUE                                                                   RRTM_INIT.1094
      DO 8200 JP = 1,5                                                           RRTM_INIT.1095
         DO 8200 IGC = 1,NGC(5)                                                  RRTM_INIT.1096
            FREFB(NGS(4)+IGC,JP) = FRACREFBC(IGC,JP)                             RRTM_INIT.1097
 8200 CONTINUE                                                                   RRTM_INIT.1098
      DO 8300 JP = 1,4                                                           RRTM_INIT.1099
         DO 8300 IGC = 1,NGC(5)                                                  RRTM_INIT.1100
            FREFBDF(NGS(4)+IGC,JP) = FRACREFBC(IGC,JP+1) -                       RRTM_INIT.1101
     &                               FRACREFBC(IGC,JP)                           RRTM_INIT.1102
 8300 CONTINUE                                                                   RRTM_INIT.1103
                                                                                 RRTM_INIT.1104
      RETURN                                                                     RRTM_INIT.1105
      END                                                                        RRTM_INIT.1106
                                                                                 RRTM_INIT.1107
C***************************************************************************     RRTM_INIT.1108
      SUBROUTINE CMBGB6                                                          RRTM_INIT.1109
C***************************************************************************     RRTM_INIT.1110
C                                                                                RRTM_INIT.1111
C     BAND 6:  820-980 cm-1 (low - H2O; high - nothing)                          RRTM_INIT.1112
C***************************************************************************     RRTM_INIT.1113
                                                                                 RRTM_INIT.1114
C Parameters                                                                     RRTM_INIT.1115
      PARAMETER (MG=16, NBANDS=16)                                               RRTM_INIT.1116
      PARAMETER (NGPT=140, NG6=8)                                                RRTM_INIT.1117
C Input                                                                          RRTM_INIT.1118
      COMMON /K6/       KA(5,13,MG), SELFREF(10,MG)                              RRTM_INIT.1119
      COMMON /RWT/      RWGT(MG*NBANDS)                                          RRTM_INIT.1120
      COMMON /FEATUREC/ NGC(NBANDS), NGS(NBANDS), NGN(NGPT), NGB(NGPT)           RRTM_INIT.1121
C Output                                                                         RRTM_INIT.1122
      COMMON /K6C/   KAC(5,13,NG6), SELFREFC(10,NG6),                            RRTM_INIT.1123
     &               ABSCO2C(NG6), CFC11ADJC(NG6), CFC12C(NG6)                   RRTM_INIT.1124
      COMMON /PF6C/  FRACREFAC(NG6)                                              RRTM_INIT.1125
      COMMON /PFDAT/ FREFA(NGPT,13),FREFB(NGPT,6),                               RRTM_INIT.1126
     &               FREFADF(NGPT,13),FREFBDF(NGPT,6)                            RRTM_INIT.1127
                                                                                 RRTM_INIT.1128
C Data                                                                           RRTM_INIT.1129
      DIMENSION FRACREFA(MG), ABSCO2(MG), CFC11ADJ(MG), CFC12(MG)                RRTM_INIT.1130
                                                                                 RRTM_INIT.1131
      DATA FRACREFA/                                                             RRTM_INIT.1132
C     From P = 706 mb.                                                           RRTM_INIT.1133
     &    0.13739009,0.14259538,0.14033118,0.13547136,                           RRTM_INIT.1134
     &    0.12569460,0.11028396,0.08626066,0.06245148,                           RRTM_INIT.1135
     &    0.04309394,0.00473551,0.00403920,0.00321695,                           RRTM_INIT.1136
     &    0.00232470,0.00147662,0.00056095,0.00007373/                           RRTM_INIT.1137
                                                                                 RRTM_INIT.1138
C      DATA CFC11/                                                               RRTM_INIT.1139
C     &     0., 0., 26.5435, 108.850,                                            RRTM_INIT.1140
C     &     58.7804, 54.0875, 41.1065, 35.6120,                                  RRTM_INIT.1141
C     &     41.2328, 47.7402, 79.1026, 64.3005,                                  RRTM_INIT.1142
C     &     108.206, 141.617, 186.565, 58.4782/                                  RRTM_INIT.1143
C     CFC11 is multiplied by 1.385 to account for the 1060-1107 cm-1 band.       RRTM_INIT.1144
      DATA CFC11ADJ/                                                             RRTM_INIT.1145
     &     0.,  0., 36.7627,    150.757,                                         RRTM_INIT.1146
     &     81.4109, 74.9112, 56.9325, 49.3226,                                   RRTM_INIT.1147
     &     57.1074, 66.1202, 109.557, 89.0562,                                   RRTM_INIT.1148
     &     149.865, 196.140, 258.393, 80.9923/                                   RRTM_INIT.1149
      DATA CFC12/                                                                RRTM_INIT.1150
     &     62.8368, 43.2626, 26.7549, 22.2487,                                   RRTM_INIT.1151
     &     23.5029, 34.8323, 26.2335, 23.2306,                                   RRTM_INIT.1152
     &     18.4062, 13.9534, 22.6268, 24.2604,                                   RRTM_INIT.1153
     &     30.0088, 26.3634, 15.8237, 57.5050/                                   RRTM_INIT.1154
      DATA ABSCO2/                                                               RRTM_INIT.1155
     &     7.44852E-05, 6.29208E-05, 7.34031E-05, 6.65218E-05,                   RRTM_INIT.1156
     &     7.87511E-05, 1.22489E-04, 3.39785E-04, 9.33040E-04,                   RRTM_INIT.1157
     &     1.54323E-03, 4.07220E-04, 4.34332E-04, 8.76418E-05,                   RRTM_INIT.1158
     &     9.80381E-05, 3.51680E-05, 5.31766E-05, 1.01542E-05/                   RRTM_INIT.1159
                                                                                 RRTM_INIT.1160
      REAL KA,KAC                                                                RRTM_INIT.1161
                                                                                 RRTM_INIT.1162
      DO 2000 JT = 1,5                                                           RRTM_INIT.1163
         DO 2200 JP = 1,13                                                       RRTM_INIT.1164
            IPRSM = 0                                                            RRTM_INIT.1165
            DO 2400 IGC = 1,NGC(6)                                               RRTM_INIT.1166
               SUMK = 0.                                                         RRTM_INIT.1167
               DO 2600 IPR = 1, NGN(NGS(5)+IGC)                                  RRTM_INIT.1168
                  IPRSM = IPRSM + 1                                              RRTM_INIT.1169
                  SUMK = SUMK + KA(JT,JP,IPRSM)*RWGT(IPRSM+80)                   RRTM_INIT.1170
 2600          CONTINUE                                                          RRTM_INIT.1171
               KAC(JT,JP,IGC) = SUMK                                             RRTM_INIT.1172
 2400       CONTINUE                                                             RRTM_INIT.1173
 2200    CONTINUE                                                                RRTM_INIT.1174
 2000 CONTINUE                                                                   RRTM_INIT.1175
                                                                                 RRTM_INIT.1176
      DO 4000 JT = 1,10                                                          RRTM_INIT.1177
         IPRSM = 0                                                               RRTM_INIT.1178
         DO 4400 IGC = 1,NGC(6)                                                  RRTM_INIT.1179
            SUMK = 0.                                                            RRTM_INIT.1180
            DO 4600 IPR = 1, NGN(NGS(5)+IGC)                                     RRTM_INIT.1181
               IPRSM = IPRSM + 1                                                 RRTM_INIT.1182
               SUMK = SUMK + SELFREF(JT,IPRSM)*RWGT(IPRSM+80)                    RRTM_INIT.1183
 4600       CONTINUE                                                             RRTM_INIT.1184
            SELFREFC(JT,IGC) = SUMK                                              RRTM_INIT.1185
 4400    CONTINUE                                                                RRTM_INIT.1186
 4000 CONTINUE                                                                   RRTM_INIT.1187
                                                                                 RRTM_INIT.1188
      IPRSM = 0                                                                  RRTM_INIT.1189
      DO 7400 IGC = 1,NGC(6)                                                     RRTM_INIT.1190
         SUMF = 0.                                                               RRTM_INIT.1191
         SUMK1= 0.                                                               RRTM_INIT.1192
         SUMK2= 0.                                                               RRTM_INIT.1193
         SUMK3= 0.                                                               RRTM_INIT.1194
         DO 7600 IPR = 1, NGN(NGS(5)+IGC)                                        RRTM_INIT.1195
            IPRSM = IPRSM + 1                                                    RRTM_INIT.1196
            SUMF = SUMF + FRACREFA(IPRSM)                                        RRTM_INIT.1197
            SUMK1= SUMK1+ ABSCO2(IPRSM)*RWGT(IPRSM+80)                           RRTM_INIT.1198
            SUMK2= SUMK2+ CFC11ADJ(IPRSM)*RWGT(IPRSM+80)                         RRTM_INIT.1199
            SUMK3= SUMK3+ CFC12(IPRSM)*RWGT(IPRSM+80)                            RRTM_INIT.1200
 7600    CONTINUE                                                                RRTM_INIT.1201
         FRACREFAC(IGC) = SUMF                                                   RRTM_INIT.1202
         ABSCO2C(IGC) = SUMK1                                                    RRTM_INIT.1203
         CFC11ADJC(IGC) = SUMK2                                                  RRTM_INIT.1204
         CFC12C(IGC) = SUMK3                                                     RRTM_INIT.1205
 7400 CONTINUE                                                                   RRTM_INIT.1206
                                                                                 RRTM_INIT.1207
      DO 8000 IGC = 1,NGC(6)                                                     RRTM_INIT.1208
         FREFA(NGS(5)+IGC,1) = FRACREFAC(IGC)                                    RRTM_INIT.1209
         FREFB(NGS(5)+IGC,1) = FRACREFAC(IGC)                                    RRTM_INIT.1210
 8000 CONTINUE                                                                   RRTM_INIT.1211
                                                                                 RRTM_INIT.1212
      RETURN                                                                     RRTM_INIT.1213
      END                                                                        RRTM_INIT.1214
                                                                                 RRTM_INIT.1215
                                                                                 RRTM_INIT.1216
C***************************************************************************     RRTM_INIT.1217
      SUBROUTINE CMBGB7                                                          RRTM_INIT.1218
C***************************************************************************     RRTM_INIT.1219
C                                                                                RRTM_INIT.1220
C     BAND 7:  980-1080 cm-1 (low - H2O,O3; high - O3)                           RRTM_INIT.1221
C***************************************************************************     RRTM_INIT.1222
                                                                                 RRTM_INIT.1223
C Parameters                                                                     RRTM_INIT.1224
      PARAMETER (MG=16, NBANDS=16)                                               RRTM_INIT.1225
      PARAMETER (NGPT=140, NG7=12)                                               RRTM_INIT.1226
C Input                                                                          RRTM_INIT.1227
      COMMON /K7/       KA(9,5,13,MG), KB(5,13:59,MG), SELFREF(10,MG)            RRTM_INIT.1228
      COMMON /RWT/      RWGT(MG*NBANDS)                                          RRTM_INIT.1229
      COMMON /FEATUREC/ NGC(NBANDS), NGS(NBANDS), NGN(NGPT), NGB(NGPT)           RRTM_INIT.1230
C Output                                                                         RRTM_INIT.1231
      COMMON /K7C/   KAC(9,5,13,NG7), KBC(5,13:59,NG7),                          RRTM_INIT.1232
     &               SELFREFC(10,NG7),                                           RRTM_INIT.1233
     &               ABSCO2C(NG7)                                                RRTM_INIT.1234
      COMMON /PF7C/  FRACREFAC(NG7,9), FRACREFBC(NG7)                            RRTM_INIT.1235
      COMMON /PFDAT/ FREFA(NGPT,13),FREFB(NGPT,6),                               RRTM_INIT.1236
     &               FREFADF(NGPT,13),FREFBDF(NGPT,6)                            RRTM_INIT.1237
                                                                                 RRTM_INIT.1238
C Data                                                                           RRTM_INIT.1239
      DIMENSION FRACREFA(MG,9), FRACREFB(MG), ABSCO2(MG)                         RRTM_INIT.1240
                                                                                 RRTM_INIT.1241
      DATA FRACREFA/                                                             RRTM_INIT.1242
     &    0.16461779, 0.14889984, 0.14233345, 0.13156526,                        RRTM_INIT.1243
     &    0.11679733, 0.09988949, 0.08078653, 0.06006384,                        RRTM_INIT.1244
     &    0.04028391, 0.00435899, 0.00359173, 0.00281707,                        RRTM_INIT.1245
     &    0.00206767, 0.00135012, 0.00050720, 0.00007146,                        RRTM_INIT.1246
     &    0.16442357, 0.14944240, 0.14245804, 0.13111183,                        RRTM_INIT.1247
     &    0.11688625, 0.09983791, 0.08085148, 0.05993948,                        RRTM_INIT.1248
     &    0.04028057, 0.00435939, 0.00358708, 0.00284036,                        RRTM_INIT.1249
     &    0.00208869, 0.00133256, 0.00049260, 0.00006931,                        RRTM_INIT.1250
     &    0.16368519, 0.15018989, 0.14262174, 0.13084342,                        RRTM_INIT.1251
     &    0.11682195, 0.09996257, 0.08074036, 0.05985692,                        RRTM_INIT.1252
     &    0.04045362, 0.00436208, 0.00358257, 0.00287122,                        RRTM_INIT.1253
     &    0.00211004, 0.00133804, 0.00049260, 0.00006931,                        RRTM_INIT.1254
     &    0.16274056, 0.15133780, 0.14228874, 0.13081114,                        RRTM_INIT.1255
     &    0.11688486, 0.09979610, 0.08073687, 0.05996741,                        RRTM_INIT.1256
     &    0.04040616, 0.00439869, 0.00368910, 0.00293041,                        RRTM_INIT.1257
     &    0.00211604, 0.00133536, 0.00049260, 0.00006931,                        RRTM_INIT.1258
     &    0.16176532, 0.15207882, 0.14226955, 0.13079646,                        RRTM_INIT.1259
     &    0.11688191, 0.09966998, 0.08066384, 0.06020275,                        RRTM_INIT.1260
     &    0.04047901, 0.00446696, 0.00377456, 0.00294410,                        RRTM_INIT.1261
     &    0.00211082, 0.00133536, 0.00049260, 0.00006931,                        RRTM_INIT.1262
     &    0.15993737, 0.15305527, 0.14259829, 0.13078023,                        RRTM_INIT.1263
     &    0.11686983, 0.09980131, 0.08058286, 0.06031430,                        RRTM_INIT.1264
     &    0.04082833, 0.00450509, 0.00377574, 0.00294823,                        RRTM_INIT.1265
     &    0.00210977, 0.00133302, 0.00049260, 0.00006931,                        RRTM_INIT.1266
     &    0.15371189, 0.15592396, 0.14430280, 0.13076764,                        RRTM_INIT.1267
     &    0.11720382, 0.10023471, 0.08066396, 0.06073554,                        RRTM_INIT.1268
     &    0.04121581, 0.00451202, 0.00377832, 0.00294609,                        RRTM_INIT.1269
     &    0.00210943, 0.00133336, 0.00049260, 0.00006931,                        RRTM_INIT.1270
     &    0.14262275, 0.14572631, 0.14560597, 0.13736825,                        RRTM_INIT.1271
     &    0.12271351, 0.10419556, 0.08294533, 0.06199794,                        RRTM_INIT.1272
     &    0.04157615, 0.00452842, 0.00377704, 0.00293852,                        RRTM_INIT.1273
     &    0.00211034, 0.00133278, 0.00049259, 0.00006931,                        RRTM_INIT.1274
     &    0.14500433, 0.14590444, 0.14430299, 0.13770708,                        RRTM_INIT.1275
     &    0.12288283, 0.10350952, 0.08269450, 0.06130579,                        RRTM_INIT.1276
     &    0.04144571, 0.00452096, 0.00377382, 0.00294532,                        RRTM_INIT.1277
     &    0.00210943, 0.00133228, 0.00049260, 0.00006931/                        RRTM_INIT.1278
      DATA FRACREFB/                                                             RRTM_INIT.1279
     &    0.15355594,0.15310939,0.14274909,0.13129812,                           RRTM_INIT.1280
     &    0.11736792,0.10118213,0.08215259,0.06165591,                           RRTM_INIT.1281
     &    0.04164486,0.00451141,0.00372837,0.00294095,                           RRTM_INIT.1282
     &    0.00215259,0.00136792,0.00051233,0.00007075/                           RRTM_INIT.1283
                                                                                 RRTM_INIT.1284
      DATA ABSCO2/                                                               RRTM_INIT.1285
     &    9.30038E-05, 1.74061E-04, 2.09293E-04, 2.52360E-04,                    RRTM_INIT.1286
     &    3.13404E-04, 4.16619E-04, 6.27394E-04, 1.29386E-03,                    RRTM_INIT.1287
     &    4.05192E-03, 3.97050E-03, 7.00634E-04, 6.06617E-04,                    RRTM_INIT.1288
     &    7.66978E-04, 6.70661E-04, 7.89971E-04, 7.55709E-04/                    RRTM_INIT.1289
                                                                                 RRTM_INIT.1290
      REAL KA,KB,KAC,KBC                                                         RRTM_INIT.1291
                                                                                 RRTM_INIT.1292
      DO 2000 JN = 1,9                                                           RRTM_INIT.1293
         DO 2000 JT = 1,5                                                        RRTM_INIT.1294
            DO 2200 JP = 1,13                                                    RRTM_INIT.1295
               IPRSM = 0                                                         RRTM_INIT.1296
               DO 2400 IGC = 1,NGC(7)                                            RRTM_INIT.1297
                 SUMK = 0.                                                       RRTM_INIT.1298
                  DO 2600 IPR = 1, NGN(NGS(6)+IGC)                               RRTM_INIT.1299
                     IPRSM = IPRSM + 1                                           RRTM_INIT.1300
                     SUMK = SUMK + KA(JN,JT,JP,IPRSM)*RWGT(IPRSM+96)             RRTM_INIT.1301
 2600             CONTINUE                                                       RRTM_INIT.1302
                  KAC(JN,JT,JP,IGC) = SUMK                                       RRTM_INIT.1303
 2400          CONTINUE                                                          RRTM_INIT.1304
 2200       CONTINUE                                                             RRTM_INIT.1305
 2000 CONTINUE                                                                   RRTM_INIT.1306
      DO 3000 JT = 1,5                                                           RRTM_INIT.1307
         DO 3200 JP = 13,59                                                      RRTM_INIT.1308
            IPRSM = 0                                                            RRTM_INIT.1309
            DO 3400 IGC = 1,NGC(7)                                               RRTM_INIT.1310
               SUMK = 0.                                                         RRTM_INIT.1311
               DO 3600 IPR = 1, NGN(NGS(6)+IGC)                                  RRTM_INIT.1312
                  IPRSM = IPRSM + 1                                              RRTM_INIT.1313
                  SUMK = SUMK + KB(JT,JP,IPRSM)*RWGT(IPRSM+96)                   RRTM_INIT.1314
 3600          CONTINUE                                                          RRTM_INIT.1315
               KBC(JT,JP,IGC) = SUMK                                             RRTM_INIT.1316
 3400       CONTINUE                                                             RRTM_INIT.1317
 3200    CONTINUE                                                                RRTM_INIT.1318
 3000 CONTINUE                                                                   RRTM_INIT.1319
                                                                                 RRTM_INIT.1320
      DO 4000 JT = 1,10                                                          RRTM_INIT.1321
         IPRSM = 0                                                               RRTM_INIT.1322
         DO 4400 IGC = 1,NGC(7)                                                  RRTM_INIT.1323
            SUMK = 0.                                                            RRTM_INIT.1324
            DO 4600 IPR = 1, NGN(NGS(6)+IGC)                                     RRTM_INIT.1325
               IPRSM = IPRSM + 1                                                 RRTM_INIT.1326
               SUMK = SUMK + SELFREF(JT,IPRSM)*RWGT(IPRSM+96)                    RRTM_INIT.1327
 4600       CONTINUE                                                             RRTM_INIT.1328
            SELFREFC(JT,IGC) = SUMK                                              RRTM_INIT.1329
 4400    CONTINUE                                                                RRTM_INIT.1330
 4000 CONTINUE                                                                   RRTM_INIT.1331
                                                                                 RRTM_INIT.1332
      DO 5000 JP = 1,9                                                           RRTM_INIT.1333
         IPRSM = 0                                                               RRTM_INIT.1334
         DO 5400 IGC = 1,NGC(7)                                                  RRTM_INIT.1335
            SUMF = 0.                                                            RRTM_INIT.1336
            DO 5600 IPR = 1, NGN(NGS(6)+IGC)                                     RRTM_INIT.1337
               IPRSM = IPRSM + 1                                                 RRTM_INIT.1338
               SUMF = SUMF + FRACREFA(IPRSM,JP)                                  RRTM_INIT.1339
 5600       CONTINUE                                                             RRTM_INIT.1340
            FRACREFAC(IGC,JP) = SUMF                                             RRTM_INIT.1341
 5400    CONTINUE                                                                RRTM_INIT.1342
 5000 CONTINUE                                                                   RRTM_INIT.1343
                                                                                 RRTM_INIT.1344
      IPRSM = 0                                                                  RRTM_INIT.1345
      DO 7400 IGC = 1,NGC(7)                                                     RRTM_INIT.1346
         SUMF = 0.                                                               RRTM_INIT.1347
         SUMK = 0.                                                               RRTM_INIT.1348
         DO 7600 IPR = 1, NGN(NGS(6)+IGC)                                        RRTM_INIT.1349
            IPRSM = IPRSM + 1                                                    RRTM_INIT.1350
            SUMF = SUMF + FRACREFB(IPRSM)                                        RRTM_INIT.1351
            SUMK = SUMK + ABSCO2(IPRSM)*RWGT(IPRSM+96)                           RRTM_INIT.1352
 7600    CONTINUE                                                                RRTM_INIT.1353
         FRACREFBC(IGC) = SUMF                                                   RRTM_INIT.1354
         ABSCO2C(IGC) = SUMK                                                     RRTM_INIT.1355
 7400 CONTINUE                                                                   RRTM_INIT.1356
                                                                                 RRTM_INIT.1357
      DO 8000 JP = 1,9                                                           RRTM_INIT.1358
         DO 8000 IGC = 1,NGC(7)                                                  RRTM_INIT.1359
            FREFA(NGS(6)+IGC,JP) = FRACREFAC(IGC,JP)                             RRTM_INIT.1360
 8000 CONTINUE                                                                   RRTM_INIT.1361
      DO 8100 JP = 1,8                                                           RRTM_INIT.1362
         DO 8100 IGC = 1,NGC(7)                                                  RRTM_INIT.1363
            FREFADF(NGS(6)+IGC,JP) = FRACREFAC(IGC,JP+1) -                       RRTM_INIT.1364
     &                               FRACREFAC(IGC,JP)                           RRTM_INIT.1365
 8100 CONTINUE                                                                   RRTM_INIT.1366
      DO 8200 IGC = 1,NGC(7)                                                     RRTM_INIT.1367
         FREFB(NGS(6)+IGC,1) = FRACREFBC(IGC)                                    RRTM_INIT.1368
 8200 CONTINUE                                                                   RRTM_INIT.1369
                                                                                 RRTM_INIT.1370
      RETURN                                                                     RRTM_INIT.1371
      END                                                                        RRTM_INIT.1372
                                                                                 RRTM_INIT.1373
C***************************************************************************     RRTM_INIT.1374
      SUBROUTINE CMBGB8                                                          RRTM_INIT.1375
C***************************************************************************     RRTM_INIT.1376
C                                                                                RRTM_INIT.1377
C     BAND 8:  1080-1180 cm-1 (low (i.e.>~300mb) - H2O; high - O3)               RRTM_INIT.1378
C***************************************************************************     RRTM_INIT.1379
                                                                                 RRTM_INIT.1380
C Parameters                                                                     RRTM_INIT.1381
      PARAMETER (MG=16, NBANDS=16)                                               RRTM_INIT.1382
      PARAMETER (NGPT=140, NG8=8)                                                RRTM_INIT.1383
C Input                                                                          RRTM_INIT.1384
      COMMON /K8/       KA(5,7,MG), KB(5,7:59,MG), SELFREF(10,MG)                RRTM_INIT.1385
      COMMON /RWT/      RWGT(MG*NBANDS)                                          RRTM_INIT.1386
      COMMON /FEATUREC/ NGC(NBANDS), NGS(NBANDS), NGN(NGPT), NGB(NGPT)           RRTM_INIT.1387
C Output                                                                         RRTM_INIT.1388
      COMMON /K8C/   KAC(5,7,NG8), KBC(5,7:59,NG8),                              RRTM_INIT.1389
     &               SELFREFC(10,NG8),                                           RRTM_INIT.1390
     &               ABSCO2AC(NG8), ABSCO2BC(NG8),                               RRTM_INIT.1391
     &               ABSN2OAC(NG8), ABSN2OBC(NG8),                               RRTM_INIT.1392
     &               CFC12C(NG8), CFC22ADJC(NG8)                                 RRTM_INIT.1393
      COMMON /PF8C/  FRACREFAC(NG8), FRACREFBC(NG8)                              RRTM_INIT.1394
      COMMON /PFDAT/ FREFA(NGPT,13),FREFB(NGPT,6),                               RRTM_INIT.1395
     &               FREFADF(NGPT,13),FREFBDF(NGPT,6)                            RRTM_INIT.1396
                                                                                 RRTM_INIT.1397
C Data                                                                           RRTM_INIT.1398
      DIMENSION FRACREFA(MG), FRACREFB(MG), ABSCO2A(MG), ABSCO2B(MG)             RRTM_INIT.1399
      DIMENSION ABSN2OA(MG), ABSN2OB(MG), CFC12(MG), CFC22ADJ(MG)                RRTM_INIT.1400
                                                                                 RRTM_INIT.1401
      DATA FRACREFA/                                                             RRTM_INIT.1402
C     From P = 1053.6 mb.                                                        RRTM_INIT.1403
     &    0.15309700,0.15450300,0.14458799,0.13098200,                           RRTM_INIT.1404
     &    0.11817900,0.09953490,0.08132080,0.06139960,                           RRTM_INIT.1405
     &    0.04132010,0.00446788,0.00372533,0.00294053,                           RRTM_INIT.1406
     &    0.00211371,0.00128122,0.00048050,0.00006759/                           RRTM_INIT.1407
      DATA FRACREFB/                                                             RRTM_INIT.1408
C     From P = 28.9 mb.                                                          RRTM_INIT.1409
     &    0.14105400,0.14728899,0.14264800,0.13331699,                           RRTM_INIT.1410
     &    0.12034100,0.10467000,0.08574980,0.06469390,                           RRTM_INIT.1411
     &    0.04394640,0.00481284,0.00397375,0.00315006,                           RRTM_INIT.1412
     &    0.00228636,0.00144606,0.00054604,0.00007697/                           RRTM_INIT.1413
                                                                                 RRTM_INIT.1414
      DATA CFC12/                                                                RRTM_INIT.1415
     &     85.4027, 89.4696, 74.0959, 67.7480,                                   RRTM_INIT.1416
     &     61.2444, 59.9073, 60.8296, 63.0998,                                   RRTM_INIT.1417
     &     59.6110, 64.0735, 57.2622, 58.9721,                                   RRTM_INIT.1418
     &     43.5505, 26.1192, 32.7023, 32.8667/                                   RRTM_INIT.1419
      DATA CFC22ADJ/                                                             RRTM_INIT.1420
C     Original CFC22 is multiplied by 1.485 to account for the 780-850 cm-1      RRTM_INIT.1421
C     and 1290-1335 cm-1 bands.                                                  RRTM_INIT.1422
     &     135.335, 89.6642, 76.2375, 65.9748,                                   RRTM_INIT.1423
     &     63.1164, 60.2935, 64.0299, 75.4264,                                   RRTM_INIT.1424
     &     51.3018, 7.07911, 5.86928, 0.398693,                                  RRTM_INIT.1425
     &     2.82885, 9.12751, 6.28271, 0./                                        RRTM_INIT.1426
      DATA ABSCO2A/                                                              RRTM_INIT.1427
     &     1.11233E-05, 3.92400E-05, 6.62059E-05, 8.51687E-05,                   RRTM_INIT.1428
     &     7.79035E-05, 1.34058E-04, 2.82553E-04, 5.41741E-04,                   RRTM_INIT.1429
     &     1.47029E-05, 2.34982E-05, 6.91094E-08, 8.48917E-08,                   RRTM_INIT.1430
     &     6.58783E-08, 4.64849E-08, 3.62742E-08, 3.62742E-08/                   RRTM_INIT.1431
      DATA ABSCO2B/                                                              RRTM_INIT.1432
     &     4.10977E-09, 5.65200E-08, 1.70800E-07, 4.16840E-07,                   RRTM_INIT.1433
     &     9.53684E-07, 2.36468E-06, 7.29502E-06, 4.93883E-05,                   RRTM_INIT.1434
     &     5.10440E-04, 9.75248E-04, 1.36495E-03, 2.40451E-03,                   RRTM_INIT.1435
     &     4.50277E-03, 2.24486E-02, 4.06756E-02, 2.17447E-10/                   RRTM_INIT.1436
      DATA ABSN2OA/                                                              RRTM_INIT.1437
     &     1.28527E-02,5.28651E-02,1.01668E-01,1.57224E-01,                      RRTM_INIT.1438
     &     2.76947E-01,4.93048E-01,6.71387E-01,3.48809E-01,                      RRTM_INIT.1439
     &     4.19840E-01,3.13558E-01,2.44432E-01,2.05108E-01,                      RRTM_INIT.1440
     &     1.21423E-01,1.22158E-01,1.49702E-01,1.47799E-01/                      RRTM_INIT.1441
      DATA ABSN2OB/                                                              RRTM_INIT.1442
     &     3.15864E-03,4.87347E-03,8.63235E-03,2.16053E-02,                      RRTM_INIT.1443
     &     3.63699E-02,7.89149E-02,3.53807E-01,1.27140E-00,                      RRTM_INIT.1444
     &     2.31464E-00,7.75834E-02,5.15063E-02,4.07059E-02,                      RRTM_INIT.1445
     &     5.91947E-02,5.83546E-02,3.12716E-01,1.47456E-01/                      RRTM_INIT.1446
                                                                                 RRTM_INIT.1447
      REAL KA,KB,KAC,KBC                                                         RRTM_INIT.1448
                                                                                 RRTM_INIT.1449
      DO 2000 JT = 1,5                                                           RRTM_INIT.1450
         DO 2200 JP = 1,7                                                        RRTM_INIT.1451
            IPRSM = 0                                                            RRTM_INIT.1452
            DO 2400 IGC = 1,NGC(8)                                               RRTM_INIT.1453
              SUMK = 0.                                                          RRTM_INIT.1454
               DO 2600 IPR = 1, NGN(NGS(7)+IGC)                                  RRTM_INIT.1455
                  IPRSM = IPRSM + 1                                              RRTM_INIT.1456
                  SUMK = SUMK + KA(JT,JP,IPRSM)*RWGT(IPRSM+112)                  RRTM_INIT.1457
 2600          CONTINUE                                                          RRTM_INIT.1458
               KAC(JT,JP,IGC) = SUMK                                             RRTM_INIT.1459
 2400       CONTINUE                                                             RRTM_INIT.1460
 2200    CONTINUE                                                                RRTM_INIT.1461
 2000 CONTINUE                                                                   RRTM_INIT.1462
      DO 3000 JT = 1,5                                                           RRTM_INIT.1463
         DO 3200 JP = 7,59                                                       RRTM_INIT.1464
            IPRSM = 0                                                            RRTM_INIT.1465
            DO 3400 IGC = 1,NGC(8)                                               RRTM_INIT.1466
               SUMK = 0.                                                         RRTM_INIT.1467
               DO 3600 IPR = 1, NGN(NGS(7)+IGC)                                  RRTM_INIT.1468
                  IPRSM = IPRSM + 1                                              RRTM_INIT.1469
                  SUMK = SUMK + KB(JT,JP,IPRSM)*RWGT(IPRSM+112)                  RRTM_INIT.1470
 3600          CONTINUE                                                          RRTM_INIT.1471
               KBC(JT,JP,IGC) = SUMK                                             RRTM_INIT.1472
 3400       CONTINUE                                                             RRTM_INIT.1473
 3200    CONTINUE                                                                RRTM_INIT.1474
 3000 CONTINUE                                                                   RRTM_INIT.1475
                                                                                 RRTM_INIT.1476
      DO 4000 JT = 1,10                                                          RRTM_INIT.1477
         IPRSM = 0                                                               RRTM_INIT.1478
         DO 4400 IGC = 1,NGC(8)                                                  RRTM_INIT.1479
            SUMK = 0.                                                            RRTM_INIT.1480
            DO 4600 IPR = 1, NGN(NGS(7)+IGC)                                     RRTM_INIT.1481
               IPRSM = IPRSM + 1                                                 RRTM_INIT.1482
               SUMK = SUMK + SELFREF(JT,IPRSM)*RWGT(IPRSM+112)                   RRTM_INIT.1483
 4600       CONTINUE                                                             RRTM_INIT.1484
            SELFREFC(JT,IGC) = SUMK                                              RRTM_INIT.1485
 4400    CONTINUE                                                                RRTM_INIT.1486
 4000 CONTINUE                                                                   RRTM_INIT.1487
                                                                                 RRTM_INIT.1488
      IPRSM = 0                                                                  RRTM_INIT.1489
      DO 7400 IGC = 1,NGC(8)                                                     RRTM_INIT.1490
         SUMF1= 0.                                                               RRTM_INIT.1491
         SUMF2= 0.                                                               RRTM_INIT.1492
         SUMK1= 0.                                                               RRTM_INIT.1493
         SUMK2= 0.                                                               RRTM_INIT.1494
         SUMK3= 0.                                                               RRTM_INIT.1495
         SUMK4= 0.                                                               RRTM_INIT.1496
         SUMK5= 0.                                                               RRTM_INIT.1497
         SUMK6= 0.                                                               RRTM_INIT.1498
         DO 7600 IPR = 1, NGN(NGS(7)+IGC)                                        RRTM_INIT.1499
            IPRSM = IPRSM + 1                                                    RRTM_INIT.1500
            SUMF1= SUMF1+ FRACREFA(IPRSM)                                        RRTM_INIT.1501
            SUMF2= SUMF2+ FRACREFB(IPRSM)                                        RRTM_INIT.1502
            SUMK1= SUMK1+ ABSCO2A(IPRSM)*RWGT(IPRSM+112)                         RRTM_INIT.1503
            SUMK2= SUMK2+ ABSCO2B(IPRSM)*RWGT(IPRSM+112)                         RRTM_INIT.1504
            SUMK3= SUMK3+ ABSN2OA(IPRSM)*RWGT(IPRSM+112)                         RRTM_INIT.1505
            SUMK4= SUMK4+ ABSN2OB(IPRSM)*RWGT(IPRSM+112)                         RRTM_INIT.1506
            SUMK5= SUMK5+ CFC12(IPRSM)*RWGT(IPRSM+112)                           RRTM_INIT.1507
            SUMK6= SUMK6+ CFC22ADJ(IPRSM)*RWGT(IPRSM+112)                        RRTM_INIT.1508
 7600    CONTINUE                                                                RRTM_INIT.1509
         FRACREFAC(IGC) = SUMF1                                                  RRTM_INIT.1510
         FRACREFBC(IGC) = SUMF2                                                  RRTM_INIT.1511
         ABSCO2AC(IGC) = SUMK1                                                   RRTM_INIT.1512
         ABSCO2BC(IGC) = SUMK2                                                   RRTM_INIT.1513
         ABSN2OAC(IGC) = SUMK3                                                   RRTM_INIT.1514
         ABSN2OBC(IGC) = SUMK4                                                   RRTM_INIT.1515
         CFC12C(IGC) = SUMK5                                                     RRTM_INIT.1516
         CFC22ADJC(IGC) = SUMK6                                                  RRTM_INIT.1517
 7400 CONTINUE                                                                   RRTM_INIT.1518
                                                                                 RRTM_INIT.1519
      DO 8000 IGC = 1,NGC(8)                                                     RRTM_INIT.1520
         FREFA(NGS(7)+IGC,1) = FRACREFAC(IGC)                                    RRTM_INIT.1521
         FREFB(NGS(7)+IGC,1) = FRACREFBC(IGC)                                    RRTM_INIT.1522
 8000 CONTINUE                                                                   RRTM_INIT.1523
                                                                                 RRTM_INIT.1524
      RETURN                                                                     RRTM_INIT.1525
      END                                                                        RRTM_INIT.1526
                                                                                 RRTM_INIT.1527
C***************************************************************************     RRTM_INIT.1528
      SUBROUTINE CMBGB9                                                          RRTM_INIT.1529
C***************************************************************************     RRTM_INIT.1530
C                                                                                RRTM_INIT.1531
C     BAND 9:  1180-1390 cm-1 (low - H2O,CH4; high - CH4)                        RRTM_INIT.1532
C***************************************************************************     RRTM_INIT.1533
                                                                                 RRTM_INIT.1534
C Parameters                                                                     RRTM_INIT.1535
      PARAMETER (MG=16, NBANDS=16)                                               RRTM_INIT.1536
      PARAMETER (NGPT=140, NG9=12)                                               RRTM_INIT.1537
C Input                                                                          RRTM_INIT.1538
      COMMON /K9/       KA(11,5,13,MG),KB(5,13:59,MG),SELFREF(10,MG)             RRTM_INIT.1539
      COMMON /RWT/      RWGT(MG*NBANDS)                                          RRTM_INIT.1540
      COMMON /FEATUREC/ NGC(NBANDS), NGS(NBANDS), NGN(NGPT), NGB(NGPT)           RRTM_INIT.1541
C Output                                                                         RRTM_INIT.1542
      COMMON /K9C/   KAC(11,5,13,NG9), KBC(5,13:59,NG9),                         RRTM_INIT.1543
     &               SELFREFC(10,NG9),                                           RRTM_INIT.1544
     &               ABSN2OC(3*NG9)                                              RRTM_INIT.1545
      COMMON /PF9C/  FRACREFAC(NG9,9), FRACREFBC(NG9)                            RRTM_INIT.1546
      COMMON /PFDAT/ FREFA(NGPT,13),FREFB(NGPT,6),                               RRTM_INIT.1547
     &               FREFADF(NGPT,13),FREFBDF(NGPT,6)                            RRTM_INIT.1548
                                                                                 RRTM_INIT.1549
C  Data                                                                          RRTM_INIT.1550
      DIMENSION FRACREFA(MG,9), FRACREFB(MG), ABSN2O(3*MG)                       RRTM_INIT.1551
                                                                                 RRTM_INIT.1552
      DATA FRACREFA/                                                             RRTM_INIT.1553
C     From P = 1053.6 mb.                                                        RRTM_INIT.1554
     &    0.16898900,0.15898301,0.13575301,0.12600900,                           RRTM_INIT.1555
     &    0.11545800,0.09879170,0.08106830,0.06063440,                           RRTM_INIT.1556
     &    0.03988780,0.00421760,0.00346635,0.00278779,                           RRTM_INIT.1557
     &    0.00206225,0.00132324,0.00050033,0.00007038,                           RRTM_INIT.1558
     &    0.18209399,0.15315101,0.13571000,0.12504999,                           RRTM_INIT.1559
     &    0.11379100,0.09680810,0.08008570,0.05970280,                           RRTM_INIT.1560
     &    0.03942860,0.00413383,0.00343186,0.00275558,                           RRTM_INIT.1561
     &    0.00204657,0.00130219,0.00045454,0.00005664,                           RRTM_INIT.1562
     &    0.18459500,0.15512000,0.13395500,0.12576801,                           RRTM_INIT.1563
     &    0.11276800,0.09645190,0.07956650,0.05903340,                           RRTM_INIT.1564
     &    0.03887050,0.00412226,0.00339453,0.00273518,                           RRTM_INIT.1565
     &    0.00196922,0.00119411,0.00040263,0.00005664,                           RRTM_INIT.1566
     &    0.18458800,0.15859900,0.13278100,0.12589300,                           RRTM_INIT.1567
     &    0.11272700,0.09599660,0.07903030,0.05843600,                           RRTM_INIT.1568
     &    0.03843400,0.00405181,0.00337980,0.00263818,                           RRTM_INIT.1569
     &    0.00186869,0.00111807,0.00040263,0.00005664,                           RRTM_INIT.1570
     &    0.18459301,0.16176100,0.13235000,0.12528200,                           RRTM_INIT.1571
     &    0.11237100,0.09618840,0.07833760,0.05800770,                           RRTM_INIT.1572
     &    0.03787610,0.00408253,0.00330363,0.00250445,                           RRTM_INIT.1573
     &    0.00176725,0.00111753,0.00040263,0.00005664,                           RRTM_INIT.1574
     &    0.18454400,0.16505300,0.13221300,0.12476600,                           RRTM_INIT.1575
     &    0.11158300,0.09618120,0.07797340,0.05740380,                           RRTM_INIT.1576
     &    0.03742820,0.00392691,0.00312208,0.00246306,                           RRTM_INIT.1577
     &    0.00176735,0.00111721,0.00040263,0.00005664,                           RRTM_INIT.1578
     &    0.18452001,0.16697501,0.13445500,0.12391300,                           RRTM_INIT.1579
     &    0.11059100,0.09596890,0.07761050,0.05643200,                           RRTM_INIT.1580
     &    0.03686520,0.00377086,0.00309351,0.00246297,                           RRTM_INIT.1581
     &    0.00176765,0.00111700,0.00040263,0.00005664,                           RRTM_INIT.1582
     &    0.18460999,0.16854499,0.13922299,0.12266400,                           RRTM_INIT.1583
     &    0.10962200,0.09452030,0.07653800,0.05551340,                           RRTM_INIT.1584
     &    0.03609660,0.00377043,0.00309367,0.00246304,                           RRTM_INIT.1585
     &    0.00176749,0.00111689,0.00040263,0.00005664,                           RRTM_INIT.1586
     &    0.18312500,0.16787501,0.14720701,0.12766500,                           RRTM_INIT.1587
     &    0.10890900,0.08935530,0.07310870,0.05443140,                           RRTM_INIT.1588
     &    0.03566380,0.00376446,0.00309521,0.00246510,                           RRTM_INIT.1589
     &    0.00176139,0.00111543,0.00040263,0.00005664/                           RRTM_INIT.1590
      DATA FRACREFB/                                                             RRTM_INIT.1591
C     From P = 0.071 mb.                                                         RRTM_INIT.1592
     &    0.20148601,0.15252700,0.13376500,0.12184600,                           RRTM_INIT.1593
     &    0.10767800,0.09307410,0.07674570,0.05876940,                           RRTM_INIT.1594
     &    0.04001480,0.00424612,0.00346896,0.00269954,                           RRTM_INIT.1595
     &    0.00196864,0.00122562,0.00043628,0.00004892/                           RRTM_INIT.1596
                                                                                 RRTM_INIT.1597
      DATA ABSN2O/                                                               RRTM_INIT.1598
C     From P = 952 mb.                                                           RRTM_INIT.1599
     &     3.26267E-01,2.42869E-00,1.15455E+01,7.39478E-00,                      RRTM_INIT.1600
     &     5.16550E-00,2.54474E-00,3.53082E-00,3.82278E-00,                      RRTM_INIT.1601
     &     1.81297E-00,6.65313E-01,1.23652E-01,1.83895E-03,                      RRTM_INIT.1602
     &     1.70592E-03,2.68434E-09,0.,0.,                                        RRTM_INIT.1603
C     From P = 620 mb.                                                           RRTM_INIT.1604
     &     2.08632E-01,1.11865E+00,4.95975E+00,8.10907E+00,                      RRTM_INIT.1605
     &     1.10408E+01,5.45460E+00,4.18611E+00,3.53422E+00,                      RRTM_INIT.1606
     &     2.54164E+00,3.65093E-01,5.84480E-01,2.26918E-01,                      RRTM_INIT.1607
     &     1.36230E-03,5.54400E-10,6.83703E-10,0.,                               RRTM_INIT.1608
C     From P = 313 mb.                                                           RRTM_INIT.1609
     &     6.20022E-02,2.69521E-01,9.81928E-01,1.65004E-00,                      RRTM_INIT.1610
     &     3.08089E-00,5.38696E-00,1.14600E+01,2.41211E+01,                      RRTM_INIT.1611
     &     1.69655E+01,1.37556E-00,5.43254E-01,3.52079E-01,                      RRTM_INIT.1612
     &     4.31888E-01,4.82523E-06,5.74747E-11,0./                               RRTM_INIT.1613
                                                                                 RRTM_INIT.1614
      REAL KA,KB,KAC,KBC                                                         RRTM_INIT.1615
                                                                                 RRTM_INIT.1616
      DO 2000 JN = 1,11                                                          RRTM_INIT.1617
         DO 2000 JT = 1,5                                                        RRTM_INIT.1618
            DO 2200 JP = 1,13                                                    RRTM_INIT.1619
               IPRSM = 0                                                         RRTM_INIT.1620
               DO 2400 IGC = 1,NGC(9)                                            RRTM_INIT.1621
                  SUMK = 0.                                                      RRTM_INIT.1622
                  DO 2600 IPR = 1, NGN(NGS(8)+IGC)                               RRTM_INIT.1623
                     IPRSM = IPRSM + 1                                           RRTM_INIT.1624
                     SUMK = SUMK + KA(JN,JT,JP,IPRSM)*RWGT(IPRSM+128)            RRTM_INIT.1625
 2600             CONTINUE                                                       RRTM_INIT.1626
                  KAC(JN,JT,JP,IGC) = SUMK                                       RRTM_INIT.1627
 2400          CONTINUE                                                          RRTM_INIT.1628
 2200       CONTINUE                                                             RRTM_INIT.1629
 2000 CONTINUE                                                                   RRTM_INIT.1630
                                                                                 RRTM_INIT.1631
      DO 3000 JT = 1,5                                                           RRTM_INIT.1632
         DO 3200 JP = 13,59                                                      RRTM_INIT.1633
            IPRSM = 0                                                            RRTM_INIT.1634
            DO 3400 IGC = 1,NGC(9)                                               RRTM_INIT.1635
               SUMK = 0.                                                         RRTM_INIT.1636
               DO 3600 IPR = 1, NGN(NGS(8)+IGC)                                  RRTM_INIT.1637
                  IPRSM = IPRSM + 1                                              RRTM_INIT.1638
                  SUMK = SUMK + KB(JT,JP,IPRSM)*RWGT(IPRSM+128)                  RRTM_INIT.1639
 3600          CONTINUE                                                          RRTM_INIT.1640
               KBC(JT,JP,IGC) = SUMK                                             RRTM_INIT.1641
 3400       CONTINUE                                                             RRTM_INIT.1642
 3200    CONTINUE                                                                RRTM_INIT.1643
 3000 CONTINUE                                                                   RRTM_INIT.1644
                                                                                 RRTM_INIT.1645
      DO 4000 JT = 1,10                                                          RRTM_INIT.1646
         IPRSM = 0                                                               RRTM_INIT.1647
         DO 4400 IGC = 1,NGC(9)                                                  RRTM_INIT.1648
            SUMK = 0.                                                            RRTM_INIT.1649
            DO 4600 IPR = 1, NGN(NGS(8)+IGC)                                     RRTM_INIT.1650
               IPRSM = IPRSM + 1                                                 RRTM_INIT.1651
               SUMK = SUMK + SELFREF(JT,IPRSM)*RWGT(IPRSM+128)                   RRTM_INIT.1652
 4600       CONTINUE                                                             RRTM_INIT.1653
            SELFREFC(JT,IGC) = SUMK                                              RRTM_INIT.1654
 4400    CONTINUE                                                                RRTM_INIT.1655
 4000 CONTINUE                                                                   RRTM_INIT.1656
                                                                                 RRTM_INIT.1657
      DO 5000 JN = 1,3                                                           RRTM_INIT.1658
         IPRSM = 0                                                               RRTM_INIT.1659
         DO 5400 IGC = 1,NGC(9)                                                  RRTM_INIT.1660
            SUMK = 0.                                                            RRTM_INIT.1661
            DO 5600 IPR = 1, NGN(NGS(8)+IGC)                                     RRTM_INIT.1662
               IPRSM = IPRSM + 1                                                 RRTM_INIT.1663
               JND = (JN-1)*16                                                   RRTM_INIT.1664
               SUMK = SUMK + ABSN2O(JND+IPRSM)*RWGT(IPRSM+128)                   RRTM_INIT.1665
 5600       CONTINUE                                                             RRTM_INIT.1666
            JNDC = (JN-1)*NGC(9)                                                 RRTM_INIT.1667
            ABSN2OC(JNDC+IGC) = SUMK                                             RRTM_INIT.1668
 5400    CONTINUE                                                                RRTM_INIT.1669
 5000 CONTINUE                                                                   RRTM_INIT.1670
                                                                                 RRTM_INIT.1671
      DO 6000 JP = 1,9                                                           RRTM_INIT.1672
         IPRSM = 0                                                               RRTM_INIT.1673
         DO 6400 IGC = 1,NGC(9)                                                  RRTM_INIT.1674
            SUMF = 0.                                                            RRTM_INIT.1675
            DO 6600 IPR = 1, NGN(NGS(8)+IGC)                                     RRTM_INIT.1676
               IPRSM = IPRSM + 1                                                 RRTM_INIT.1677
               SUMF = SUMF + FRACREFA(IPRSM,JP)                                  RRTM_INIT.1678
 6600       CONTINUE                                                             RRTM_INIT.1679
            FRACREFAC(IGC,JP) = SUMF                                             RRTM_INIT.1680
 6400    CONTINUE                                                                RRTM_INIT.1681
 6000 CONTINUE                                                                   RRTM_INIT.1682
                                                                                 RRTM_INIT.1683
      IPRSM = 0                                                                  RRTM_INIT.1684
      DO 7400 IGC = 1,NGC(9)                                                     RRTM_INIT.1685
         SUMF = 0.                                                               RRTM_INIT.1686
         DO 7600 IPR = 1, NGN(NGS(8)+IGC)                                        RRTM_INIT.1687
            IPRSM = IPRSM + 1                                                    RRTM_INIT.1688
            SUMF = SUMF + FRACREFB(IPRSM)                                        RRTM_INIT.1689
 7600    CONTINUE                                                                RRTM_INIT.1690
         FRACREFBC(IGC) = SUMF                                                   RRTM_INIT.1691
 7400 CONTINUE                                                                   RRTM_INIT.1692
                                                                                 RRTM_INIT.1693
      DO 8000 JP = 1,9                                                           RRTM_INIT.1694
         DO 8000 IGC = 1,NGC(9)                                                  RRTM_INIT.1695
            FREFA(NGS(8)+IGC,JP) = FRACREFAC(IGC,JP)                             RRTM_INIT.1696
 8000 CONTINUE                                                                   RRTM_INIT.1697
      DO 8100 JP = 1,8                                                           RRTM_INIT.1698
         DO 8100 IGC = 1,NGC(9)                                                  RRTM_INIT.1699
            FREFADF(NGS(8)+IGC,JP) = FRACREFAC(IGC,JP+1) -                       RRTM_INIT.1700
     &                               FRACREFAC(IGC,JP)                           RRTM_INIT.1701
 8100 CONTINUE                                                                   RRTM_INIT.1702
      DO 8200 IGC = 1,NGC(9)                                                     RRTM_INIT.1703
         FREFB(NGS(8)+IGC,1) = FRACREFBC(IGC)                                    RRTM_INIT.1704
 8200 CONTINUE                                                                   RRTM_INIT.1705
                                                                                 RRTM_INIT.1706
      RETURN                                                                     RRTM_INIT.1707
      END                                                                        RRTM_INIT.1708
                                                                                 RRTM_INIT.1709
C***************************************************************************     RRTM_INIT.1710
      SUBROUTINE CMBGB10                                                         RRTM_INIT.1711
C***************************************************************************     RRTM_INIT.1712
C                                                                                RRTM_INIT.1713
C     BAND 10:  1390-1480 cm-1 (low - H2O; high - H2O)                           RRTM_INIT.1714
C***************************************************************************     RRTM_INIT.1715
                                                                                 RRTM_INIT.1716
C Parameters                                                                     RRTM_INIT.1717
      PARAMETER (MG=16, NBANDS=16)                                               RRTM_INIT.1718
      PARAMETER (NGPT=140, NG10=6)                                               RRTM_INIT.1719
C Input                                                                          RRTM_INIT.1720
      COMMON /K10/      KA(5,13,MG), KB(5,13:59,MG)                              RRTM_INIT.1721
      COMMON /RWT/      RWGT(MG*NBANDS)                                          RRTM_INIT.1722
      COMMON /FEATUREC/ NGC(NBANDS), NGS(NBANDS), NGN(NGPT), NGB(NGPT)           RRTM_INIT.1723
C Output                                                                         RRTM_INIT.1724
      COMMON /K10C/  KAC(5,13,NG10), KBC(5,13:59,NG10)                           RRTM_INIT.1725
      COMMON /PF10C/ FRACREFAC(NG10), FRACREFBC(NG10)                            RRTM_INIT.1726
      COMMON /PFDAT/ FREFA(NGPT,13),FREFB(NGPT,6),                               RRTM_INIT.1727
     &               FREFADF(NGPT,13),FREFBDF(NGPT,6)                            RRTM_INIT.1728
                                                                                 RRTM_INIT.1729
C Data                                                                           RRTM_INIT.1730
      DIMENSION FRACREFA(MG), FRACREFB(MG)                                       RRTM_INIT.1731
                                                                                 RRTM_INIT.1732
      DATA FRACREFA/                                                             RRTM_INIT.1733
C     From P = 473 mb.                                                           RRTM_INIT.1734
     &    0.16271301,0.15141940,0.14065412,0.12899506,                           RRTM_INIT.1735
     &    0.11607002,0.10142808,0.08116794,0.06104711,                           RRTM_INIT.1736
     &    0.04146209,0.00447386,0.00372902,0.00287258,                           RRTM_INIT.1737
     &    0.00206028,0.00134634,0.00049232,0.00006927/                           RRTM_INIT.1738
      DATA FRACREFB/                                                             RRTM_INIT.1739
C     From P = 1.17 mb.                                                          RRTM_INIT.1740
     &    0.16571465,0.15262246,0.14036226,0.12620729,                           RRTM_INIT.1741
     &    0.11477834,0.09967982,0.08155201,0.06159503,                           RRTM_INIT.1742
     &    0.04196607,0.00453940,0.00376881,0.00300437,                           RRTM_INIT.1743
     &    0.00223034,0.00139432,0.00051516,0.00007095/                           RRTM_INIT.1744
                                                                                 RRTM_INIT.1745
      REAL KA,KB,KAC,KBC                                                         RRTM_INIT.1746
                                                                                 RRTM_INIT.1747
      DO 2000 JT = 1,5                                                           RRTM_INIT.1748
         DO 2200 JP = 1,13                                                       RRTM_INIT.1749
            IPRSM = 0                                                            RRTM_INIT.1750
            DO 2400 IGC = 1,NGC(10)                                              RRTM_INIT.1751
               SUMK = 0.                                                         RRTM_INIT.1752
               DO 2600 IPR = 1, NGN(NGS(9)+IGC)                                  RRTM_INIT.1753
                  IPRSM = IPRSM + 1                                              RRTM_INIT.1754
                  SUMK = SUMK + KA(JT,JP,IPRSM)*RWGT(IPRSM+144)                  RRTM_INIT.1755
 2600          CONTINUE                                                          RRTM_INIT.1756
               KAC(JT,JP,IGC) = SUMK                                             RRTM_INIT.1757
 2400       CONTINUE                                                             RRTM_INIT.1758
 2200    CONTINUE                                                                RRTM_INIT.1759
 2000 CONTINUE                                                                   RRTM_INIT.1760
      DO 3000 JT = 1,5                                                           RRTM_INIT.1761
         DO 3200 JP = 13,59                                                      RRTM_INIT.1762
            IPRSM = 0                                                            RRTM_INIT.1763
            DO 3400 IGC = 1,NGC(10)                                              RRTM_INIT.1764
               SUMK = 0.                                                         RRTM_INIT.1765
               DO 3600 IPR = 1, NGN(NGS(9)+IGC)                                  RRTM_INIT.1766
                  IPRSM = IPRSM + 1                                              RRTM_INIT.1767
                  SUMK = SUMK + KB(JT,JP,IPRSM)*RWGT(IPRSM+144)                  RRTM_INIT.1768
 3600          CONTINUE                                                          RRTM_INIT.1769
               KBC(JT,JP,IGC) = SUMK                                             RRTM_INIT.1770
 3400       CONTINUE                                                             RRTM_INIT.1771
 3200    CONTINUE                                                                RRTM_INIT.1772
 3000 CONTINUE                                                                   RRTM_INIT.1773
                                                                                 RRTM_INIT.1774
      IPRSM = 0                                                                  RRTM_INIT.1775
      DO 7400 IGC = 1,NGC(10)                                                    RRTM_INIT.1776
         SUMF1= 0.                                                               RRTM_INIT.1777
         SUMF2= 0.                                                               RRTM_INIT.1778
         DO 7600 IPR = 1, NGN(NGS(9)+IGC)                                        RRTM_INIT.1779
            IPRSM = IPRSM + 1                                                    RRTM_INIT.1780
            SUMF1= SUMF1+ FRACREFA(IPRSM)                                        RRTM_INIT.1781
            SUMF2= SUMF2+ FRACREFB(IPRSM)                                        RRTM_INIT.1782
 7600    CONTINUE                                                                RRTM_INIT.1783
         FRACREFAC(IGC) = SUMF1                                                  RRTM_INIT.1784
         FRACREFBC(IGC) = SUMF2                                                  RRTM_INIT.1785
 7400 CONTINUE                                                                   RRTM_INIT.1786
                                                                                 RRTM_INIT.1787
      DO 8000 IGC = 1,NGC(10)                                                    RRTM_INIT.1788
         FREFA(NGS(9)+IGC,1) = FRACREFAC(IGC)                                    RRTM_INIT.1789
         FREFB(NGS(9)+IGC,1) = FRACREFBC(IGC)                                    RRTM_INIT.1790
 8000 CONTINUE                                                                   RRTM_INIT.1791
                                                                                 RRTM_INIT.1792
      RETURN                                                                     RRTM_INIT.1793
      END                                                                        RRTM_INIT.1794
                                                                                 RRTM_INIT.1795
C***************************************************************************     RRTM_INIT.1796
      SUBROUTINE CMBGB11                                                         RRTM_INIT.1797
C***************************************************************************     RRTM_INIT.1798
C                                                                                RRTM_INIT.1799
C     BAND 11:  1480-1800 cm-1 (low - H2O; high - H2O)                           RRTM_INIT.1800
C***************************************************************************     RRTM_INIT.1801
                                                                                 RRTM_INIT.1802
C Parameters                                                                     RRTM_INIT.1803
      PARAMETER (MG=16, NBANDS=16)                                               RRTM_INIT.1804
      PARAMETER (NGPT=140, NG11=8)                                               RRTM_INIT.1805
C Input                                                                          RRTM_INIT.1806
      COMMON /K11/      KA(5,13,MG), KB(5,13:59,MG), SELFREF(10,MG)              RRTM_INIT.1807
      COMMON /RWT/      RWGT(MG*NBANDS)                                          RRTM_INIT.1808
      COMMON /FEATUREC/ NGC(NBANDS), NGS(NBANDS), NGN(NGPT), NGB(NGPT)           RRTM_INIT.1809
C Output                                                                         RRTM_INIT.1810
      COMMON /K11C/  KAC(5,13,NG11), KBC(5,13:59,NG11),                          RRTM_INIT.1811
     &               SELFREFC(10,NG11)                                           RRTM_INIT.1812
      COMMON /PF11C/ FRACREFAC(NG11), FRACREFBC(NG11)                            RRTM_INIT.1813
      COMMON /PFDAT/ FREFA(NGPT,13),FREFB(NGPT,6),                               RRTM_INIT.1814
     &               FREFADF(NGPT,13),FREFBDF(NGPT,6)                            RRTM_INIT.1815
                                                                                 RRTM_INIT.1816
C Data                                                                           RRTM_INIT.1817
      DIMENSION FRACREFA(MG), FRACREFB(MG)                                       RRTM_INIT.1818
                                                                                 RRTM_INIT.1819
      DATA FRACREFA/                                                             RRTM_INIT.1820
C     From P = 473 mb.                                                           RRTM_INIT.1821
     &    0.14152819,0.13811260,0.14312185,0.13705885,                           RRTM_INIT.1822
     &    0.11944738,0.10570189,0.08866373,0.06565409,                           RRTM_INIT.1823
     &    0.04428961,0.00481540,0.00387058,0.00329187,                           RRTM_INIT.1824
     &    0.00238294,0.00150971,0.00049287,0.00005980/                           RRTM_INIT.1825
      DATA FRACREFB/                                                             RRTM_INIT.1826
C     From P = 1.17 mb.                                                          RRTM_INIT.1827
     &    0.10874039,0.15164889,0.15149839,0.14515044,                           RRTM_INIT.1828
     &    0.12486220,0.10725017,0.08715712,0.06463144,                           RRTM_INIT.1829
     &    0.04332319,0.00441193,0.00393819,0.00305960,                           RRTM_INIT.1830
     &    0.00224221,0.00145100,0.00055586,0.00007934/                           RRTM_INIT.1831
                                                                                 RRTM_INIT.1832
      REAL KA,KB,KAC,KBC                                                         RRTM_INIT.1833
                                                                                 RRTM_INIT.1834
      DO 2000 JT = 1,5                                                           RRTM_INIT.1835
         DO 2200 JP = 1,13                                                       RRTM_INIT.1836
            IPRSM = 0                                                            RRTM_INIT.1837
            DO 2400 IGC = 1,NGC(11)                                              RRTM_INIT.1838
               SUMK = 0.                                                         RRTM_INIT.1839
               DO 2600 IPR = 1, NGN(NGS(10)+IGC)                                 RRTM_INIT.1840
                  IPRSM = IPRSM + 1                                              RRTM_INIT.1841
                  SUMK = SUMK + KA(JT,JP,IPRSM)*RWGT(IPRSM+160)                  RRTM_INIT.1842
 2600          CONTINUE                                                          RRTM_INIT.1843
               KAC(JT,JP,IGC) = SUMK                                             RRTM_INIT.1844
 2400       CONTINUE                                                             RRTM_INIT.1845
 2200    CONTINUE                                                                RRTM_INIT.1846
 2000 CONTINUE                                                                   RRTM_INIT.1847
      DO 3000 JT = 1,5                                                           RRTM_INIT.1848
         DO 3200 JP = 13,59                                                      RRTM_INIT.1849
            IPRSM = 0                                                            RRTM_INIT.1850
            DO 3400 IGC = 1,NGC(11)                                              RRTM_INIT.1851
               SUMK = 0.                                                         RRTM_INIT.1852
               DO 3600 IPR = 1, NGN(NGS(10)+IGC)                                 RRTM_INIT.1853
                  IPRSM = IPRSM + 1                                              RRTM_INIT.1854
                  SUMK = SUMK + KB(JT,JP,IPRSM)*RWGT(IPRSM+160)                  RRTM_INIT.1855
 3600          CONTINUE                                                          RRTM_INIT.1856
               KBC(JT,JP,IGC) = SUMK                                             RRTM_INIT.1857
 3400       CONTINUE                                                             RRTM_INIT.1858
 3200    CONTINUE                                                                RRTM_INIT.1859
 3000 CONTINUE                                                                   RRTM_INIT.1860
                                                                                 RRTM_INIT.1861
      DO 4000 JT = 1,10                                                          RRTM_INIT.1862
         IPRSM = 0                                                               RRTM_INIT.1863
         DO 4400 IGC = 1,NGC(11)                                                 RRTM_INIT.1864
            SUMK = 0.                                                            RRTM_INIT.1865
            DO 4600 IPR = 1, NGN(NGS(10)+IGC)                                    RRTM_INIT.1866
               IPRSM = IPRSM + 1                                                 RRTM_INIT.1867
               SUMK = SUMK + SELFREF(JT,IPRSM)*RWGT(IPRSM+160)                   RRTM_INIT.1868
 4600       CONTINUE                                                             RRTM_INIT.1869
            SELFREFC(JT,IGC) = SUMK                                              RRTM_INIT.1870
 4400    CONTINUE                                                                RRTM_INIT.1871
 4000 CONTINUE                                                                   RRTM_INIT.1872
                                                                                 RRTM_INIT.1873
      IPRSM = 0                                                                  RRTM_INIT.1874
      DO 7400 IGC = 1,NGC(11)                                                    RRTM_INIT.1875
         SUMF1= 0.                                                               RRTM_INIT.1876
         SUMF2= 0.                                                               RRTM_INIT.1877
         DO 7600 IPR = 1, NGN(NGS(10)+IGC)                                       RRTM_INIT.1878
            IPRSM = IPRSM + 1                                                    RRTM_INIT.1879
            SUMF1= SUMF1+ FRACREFA(IPRSM)                                        RRTM_INIT.1880
            SUMF2= SUMF2+ FRACREFB(IPRSM)                                        RRTM_INIT.1881
 7600    CONTINUE                                                                RRTM_INIT.1882
         FRACREFAC(IGC) = SUMF1                                                  RRTM_INIT.1883
         FRACREFBC(IGC) = SUMF2                                                  RRTM_INIT.1884
 7400 CONTINUE                                                                   RRTM_INIT.1885
                                                                                 RRTM_INIT.1886
      DO 8000 IGC = 1,NGC(11)                                                    RRTM_INIT.1887
         FREFA(NGS(10)+IGC,1) = FRACREFAC(IGC)                                   RRTM_INIT.1888
         FREFB(NGS(10)+IGC,1) = FRACREFBC(IGC)                                   RRTM_INIT.1889
 8000 CONTINUE                                                                   RRTM_INIT.1890
                                                                                 RRTM_INIT.1891
      RETURN                                                                     RRTM_INIT.1892
      END                                                                        RRTM_INIT.1893
                                                                                 RRTM_INIT.1894
                                                                                 RRTM_INIT.1895
C***************************************************************************     RRTM_INIT.1896
      SUBROUTINE CMBGB12                                                         RRTM_INIT.1897
C***************************************************************************     RRTM_INIT.1898
C                                                                                RRTM_INIT.1899
C     BAND 12:  1800-2080 cm-1 (low - H2O,CO2; high - nothing)                   RRTM_INIT.1900
C***************************************************************************     RRTM_INIT.1901
                                                                                 RRTM_INIT.1902
C Parameters                                                                     RRTM_INIT.1903
      PARAMETER (MG=16, NBANDS=16)                                               RRTM_INIT.1904
      PARAMETER (NGPT=140, NG12=8)                                               RRTM_INIT.1905
C Input                                                                          RRTM_INIT.1906
      COMMON /K12/      KA(9,5,13,MG), SELFREF(10,MG)                            RRTM_INIT.1907
      COMMON /RWT/      RWGT(MG*NBANDS)                                          RRTM_INIT.1908
      COMMON /FEATUREC/ NGC(NBANDS), NGS(NBANDS), NGN(NGPT), NGB(NGPT)           RRTM_INIT.1909
C Output                                                                         RRTM_INIT.1910
      COMMON /K12C/  KAC(9,5,13,NG12),                                           RRTM_INIT.1911
     &               SELFREFC(10,NG12)                                           RRTM_INIT.1912
      COMMON /PF12C/ FRACREFAC(NG12,9)                                           RRTM_INIT.1913
      COMMON /PFDAT/ FREFA(NGPT,13),FREFB(NGPT,6),                               RRTM_INIT.1914
     &               FREFADF(NGPT,13),FREFBDF(NGPT,6)                            RRTM_INIT.1915
                                                                                 RRTM_INIT.1916
C Data                                                                           RRTM_INIT.1917
      DIMENSION FRACREFA(MG,9)                                                   RRTM_INIT.1918
                                                                                 RRTM_INIT.1919
      DATA FRACREFA/                                                             RRTM_INIT.1920
C     From P = 706.3 mb.                                                         RRTM_INIT.1921
     &    0.21245100,0.15164700,0.14486700,0.13075501,                           RRTM_INIT.1922
     &    0.11629600,0.09266050,0.06579930,0.04524000,                           RRTM_INIT.1923
     &    0.03072870,0.00284297,0.00234660,0.00185208,                           RRTM_INIT.1924
     &    0.00133978,0.00082214,0.00031016,0.00004363,                           RRTM_INIT.1925
     &    0.14703900,0.16937999,0.15605700,0.14159000,                           RRTM_INIT.1926
     &    0.12088500,0.10058500,0.06809110,0.05131470,                           RRTM_INIT.1927
     &    0.03487040,0.00327281,0.00250183,0.00190024,                           RRTM_INIT.1928
     &    0.00133978,0.00082214,0.00031016,0.00004363,                           RRTM_INIT.1929
     &    0.13689300,0.16610400,0.15723500,0.14299500,                           RRTM_INIT.1930
     &    0.12399400,0.09907820,0.07169690,0.05367370,                           RRTM_INIT.1931
     &    0.03671630,0.00378148,0.00290510,0.00221076,                           RRTM_INIT.1932
     &    0.00142810,0.00093527,0.00031016,0.00004363,                           RRTM_INIT.1933
     &    0.13054299,0.16273800,0.15874299,0.14279599,                           RRTM_INIT.1934
     &    0.12674300,0.09664900,0.07462200,0.05620080,                           RRTM_INIT.1935
     &    0.03789090,0.00411690,0.00322920,0.00245036,                           RRTM_INIT.1936
     &    0.00178303,0.00098595,0.00040802,0.00010150,                           RRTM_INIT.1937
     &    0.12828299,0.15824600,0.15688400,0.14449100,                           RRTM_INIT.1938
     &    0.12787800,0.09517830,0.07679350,0.05890820,                           RRTM_INIT.1939
     &    0.03883570,0.00442304,0.00346796,0.00255333,                           RRTM_INIT.1940
     &    0.00212519,0.00116168,0.00067065,0.00010150,                           RRTM_INIT.1941
     &    0.12649800,0.15195100,0.15646499,0.14569700,                           RRTM_INIT.1942
     &    0.12669300,0.09653520,0.07887920,0.06106920,                           RRTM_INIT.1943
     &    0.04043910,0.00430390,0.00364453,0.00314360,                           RRTM_INIT.1944
     &    0.00203206,0.00187787,0.00067075,0.00010150,                           RRTM_INIT.1945
     &    0.12500300,0.14460599,0.15672199,0.14724600,                           RRTM_INIT.1946
     &    0.11978900,0.10190200,0.08196710,0.06315770,                           RRTM_INIT.1947
     &    0.04240100,0.00433645,0.00404097,0.00329466,                           RRTM_INIT.1948
     &    0.00288491,0.00187803,0.00067093,0.00010150,                           RRTM_INIT.1949
     &    0.12317200,0.14118700,0.15242000,0.13794300,                           RRTM_INIT.1950
     &    0.12119200,0.10655400,0.08808350,0.06521370,                           RRTM_INIT.1951
     &    0.04505680,0.00485949,0.00477105,0.00401468,                           RRTM_INIT.1952
     &    0.00288491,0.00187786,0.00067110,0.00010150,                           RRTM_INIT.1953
     &    0.10193600,0.11693000,0.13236099,0.14053200,                           RRTM_INIT.1954
     &    0.13749801,0.12193100,0.10221000,0.07448910,                           RRTM_INIT.1955
     &    0.05205320,0.00572312,0.00476882,0.00403380,                           RRTM_INIT.1956
     &    0.00288871,0.00187396,0.00067218,0.00010150/                           RRTM_INIT.1957
                                                                                 RRTM_INIT.1958
      REAL KA,KAC                                                                RRTM_INIT.1959
                                                                                 RRTM_INIT.1960
      DO 2000 JN = 1,9                                                           RRTM_INIT.1961
         DO 2000 JT = 1,5                                                        RRTM_INIT.1962
            DO 2200 JP = 1,13                                                    RRTM_INIT.1963
               IPRSM = 0                                                         RRTM_INIT.1964
               DO 2400 IGC = 1,NGC(12)                                           RRTM_INIT.1965
                  SUMK = 0.                                                      RRTM_INIT.1966
                  DO 2600 IPR = 1, NGN(NGS(11)+IGC)                              RRTM_INIT.1967
                     IPRSM = IPRSM + 1                                           RRTM_INIT.1968
                     SUMK = SUMK + KA(JN,JT,JP,IPRSM)*RWGT(IPRSM+176)            RRTM_INIT.1969
 2600             CONTINUE                                                       RRTM_INIT.1970
                  KAC(JN,JT,JP,IGC) = SUMK                                       RRTM_INIT.1971
 2400          CONTINUE                                                          RRTM_INIT.1972
 2200       CONTINUE                                                             RRTM_INIT.1973
 2000 CONTINUE                                                                   RRTM_INIT.1974
                                                                                 RRTM_INIT.1975
      DO 4000 JT = 1,10                                                          RRTM_INIT.1976
         IPRSM = 0                                                               RRTM_INIT.1977
         DO 4400 IGC = 1,NGC(12)                                                 RRTM_INIT.1978
            SUMK = 0.                                                            RRTM_INIT.1979
            DO 4600 IPR = 1, NGN(NGS(11)+IGC)                                    RRTM_INIT.1980
               IPRSM = IPRSM + 1                                                 RRTM_INIT.1981
               SUMK = SUMK + SELFREF(JT,IPRSM)*RWGT(IPRSM+176)                   RRTM_INIT.1982
 4600       CONTINUE                                                             RRTM_INIT.1983
            SELFREFC(JT,IGC) = SUMK                                              RRTM_INIT.1984
 4400    CONTINUE                                                                RRTM_INIT.1985
 4000 CONTINUE                                                                   RRTM_INIT.1986
                                                                                 RRTM_INIT.1987
      DO 7000 JP = 1,9                                                           RRTM_INIT.1988
         IPRSM = 0                                                               RRTM_INIT.1989
         DO 7400 IGC = 1,NGC(12)                                                 RRTM_INIT.1990
            SUMF = 0.                                                            RRTM_INIT.1991
            DO 7600 IPR = 1, NGN(NGS(11)+IGC)                                    RRTM_INIT.1992
               IPRSM = IPRSM + 1                                                 RRTM_INIT.1993
               SUMF = SUMF + FRACREFA(IPRSM,JP)                                  RRTM_INIT.1994
 7600       CONTINUE                                                             RRTM_INIT.1995
            FRACREFAC(IGC,JP) = SUMF                                             RRTM_INIT.1996
 7400    CONTINUE                                                                RRTM_INIT.1997
 7000 CONTINUE                                                                   RRTM_INIT.1998
                                                                                 RRTM_INIT.1999
      DO 8000 JP = 1,9                                                           RRTM_INIT.2000
         DO 8000 IGC = 1,NGC(12)                                                 RRTM_INIT.2001
            FREFA(NGS(11)+IGC,JP) = FRACREFAC(IGC,JP)                            RRTM_INIT.2002
 8000 CONTINUE                                                                   RRTM_INIT.2003
      DO 8100 JP = 1,8                                                           RRTM_INIT.2004
         DO 8100 IGC = 1,NGC(12)                                                 RRTM_INIT.2005
            FREFADF(NGS(11)+IGC,JP) = FRACREFAC(IGC,JP+1) -                      RRTM_INIT.2006
     &                                FRACREFAC(IGC,JP)                          RRTM_INIT.2007
 8100 CONTINUE                                                                   RRTM_INIT.2008
                                                                                 RRTM_INIT.2009
      RETURN                                                                     RRTM_INIT.2010
      END                                                                        RRTM_INIT.2011
                                                                                 RRTM_INIT.2012
C***************************************************************************     RRTM_INIT.2013
      SUBROUTINE CMBGB13                                                         RRTM_INIT.2014
C***************************************************************************     RRTM_INIT.2015
C                                                                                RRTM_INIT.2016
C     BAND 13:  2080-2250 cm-1 (low - H2O,N2O; high - nothing)                   RRTM_INIT.2017
C***************************************************************************     RRTM_INIT.2018
                                                                                 RRTM_INIT.2019
C Parameters                                                                     RRTM_INIT.2020
      PARAMETER (MG=16, NBANDS=16)                                               RRTM_INIT.2021
      PARAMETER (NGPT=140, NG13=4)                                               RRTM_INIT.2022
C Input                                                                          RRTM_INIT.2023
      COMMON /K13/      KA(9,5,13,MG), SELFREF(10,MG)                            RRTM_INIT.2024
      COMMON /RWT/      RWGT(MG*NBANDS)                                          RRTM_INIT.2025
      COMMON /FEATUREC/ NGC(NBANDS), NGS(NBANDS), NGN(NGPT), NGB(NGPT)           RRTM_INIT.2026
C Output                                                                         RRTM_INIT.2027
      COMMON /K13C/  KAC(9,5,13,NG13),                                           RRTM_INIT.2028
     &               SELFREFC(10,NG13)                                           RRTM_INIT.2029
      COMMON /PF13C/ FRACREFAC(NG13,9)                                           RRTM_INIT.2030
      COMMON /PFDAT/ FREFA(NGPT,13),FREFB(NGPT,6),                               RRTM_INIT.2031
     &               FREFADF(NGPT,13),FREFBDF(NGPT,6)                            RRTM_INIT.2032
                                                                                 RRTM_INIT.2033
C Data                                                                           RRTM_INIT.2034
      DIMENSION FRACREFA(MG,9)                                                   RRTM_INIT.2035
                                                                                 RRTM_INIT.2036
      DATA FRACREFA/                                                             RRTM_INIT.2037
C     From P = 706.3 mb.                                                         RRTM_INIT.2038
     &    0.17683899,0.17319500,0.15712699,0.13604601,                           RRTM_INIT.2039
     &    0.10776200,0.08750010,0.06808820,0.04905150,                           RRTM_INIT.2040
     &    0.03280360,0.00350836,0.00281864,0.00219862,                           RRTM_INIT.2041
     &    0.00160943,0.00101885,0.00038147,0.00005348,                           RRTM_INIT.2042
     &    0.17535400,0.16999300,0.15610200,0.13589200,                           RRTM_INIT.2043
     &    0.10842100,0.08988550,0.06943920,0.04974900,                           RRTM_INIT.2044
     &    0.03323400,0.00352752,0.00289402,0.00231003,                           RRTM_INIT.2045
     &    0.00174659,0.00101884,0.00038147,0.00005348,                           RRTM_INIT.2046
     &    0.17409500,0.16846400,0.15641899,0.13503000,                           RRTM_INIT.2047
     &    0.10838600,0.08985800,0.07092720,0.05075710,                           RRTM_INIT.2048
     &    0.03364180,0.00354241,0.00303507,0.00243391,                           RRTM_INIT.2049
     &    0.00177502,0.00114638,0.00043585,0.00005348,                           RRTM_INIT.2050
     &    0.17248300,0.16778600,0.15543500,0.13496999,                           RRTM_INIT.2051
     &    0.10826300,0.09028740,0.07156720,0.05187120,                           RRTM_INIT.2052
     &    0.03424890,0.00363933,0.00324715,0.00255030,                           RRTM_INIT.2053
     &    0.00187380,0.00116978,0.00051229,0.00009768,                           RRTM_INIT.2054
     &    0.17061099,0.16715799,0.15405200,0.13471501,                           RRTM_INIT.2055
     &    0.10896400,0.09069460,0.07229760,0.05218280,                           RRTM_INIT.2056
     &    0.03555340,0.00379576,0.00330240,0.00274693,                           RRTM_INIT.2057
     &    0.00201587,0.00119598,0.00061885,0.00009768,                           RRTM_INIT.2058
     &    0.16789700,0.16629100,0.15270300,0.13360199,                           RRTM_INIT.2059
     &    0.11047200,0.09151080,0.07325000,0.05261450,                           RRTM_INIT.2060
     &    0.03657990,0.00450092,0.00349537,0.00283321,                           RRTM_INIT.2061
     &    0.00208396,0.00140354,0.00066587,0.00009768,                           RRTM_INIT.2062
     &    0.16412200,0.16387400,0.15211500,0.13062200,                           RRTM_INIT.2063
     &    0.11325100,0.09348130,0.07381380,0.05434740,                           RRTM_INIT.2064
     &    0.03803160,0.00481346,0.00393592,0.00296633,                           RRTM_INIT.2065
     &    0.00222532,0.00163762,0.00066648,0.00009768,                           RRTM_INIT.2066
     &    0.15513401,0.15768200,0.14850400,0.13330200,                           RRTM_INIT.2067
     &    0.11446500,0.09868230,0.07642050,0.05624170,                           RRTM_INIT.2068
     &    0.04197810,0.00502288,0.00429452,0.00315347,                           RRTM_INIT.2069
     &    0.00263559,0.00171772,0.00066860,0.00009768,                           RRTM_INIT.2070
     &    0.15732600,0.15223300,0.14271900,0.13563600,                           RRTM_INIT.2071
     &    0.11859600,0.10274200,0.07934560,0.05763410,                           RRTM_INIT.2072
     &    0.03921740,0.00437741,0.00337921,0.00280212,                           RRTM_INIT.2073
     &    0.00200156,0.00124812,0.00064664,0.00009768/                           RRTM_INIT.2074
                                                                                 RRTM_INIT.2075
      REAL KA,KAC                                                                RRTM_INIT.2076
                                                                                 RRTM_INIT.2077
      DO 2000 JN = 1,9                                                           RRTM_INIT.2078
         DO 2000 JT = 1,5                                                        RRTM_INIT.2079
            DO 2200 JP = 1,13                                                    RRTM_INIT.2080
               IPRSM = 0                                                         RRTM_INIT.2081
               DO 2400 IGC = 1,NGC(13)                                           RRTM_INIT.2082
                  SUMK = 0.                                                      RRTM_INIT.2083
                  DO 2600 IPR = 1, NGN(NGS(12)+IGC)                              RRTM_INIT.2084
                     IPRSM = IPRSM + 1                                           RRTM_INIT.2085
                     SUMK = SUMK + KA(JN,JT,JP,IPRSM)*RWGT(IPRSM+192)            RRTM_INIT.2086
 2600             CONTINUE                                                       RRTM_INIT.2087
                  KAC(JN,JT,JP,IGC) = SUMK                                       RRTM_INIT.2088
 2400          CONTINUE                                                          RRTM_INIT.2089
 2200       CONTINUE                                                             RRTM_INIT.2090
 2000 CONTINUE                                                                   RRTM_INIT.2091
                                                                                 RRTM_INIT.2092
      DO 4000 JT = 1,10                                                          RRTM_INIT.2093
         IPRSM = 0                                                               RRTM_INIT.2094
         DO 4400 IGC = 1,NGC(13)                                                 RRTM_INIT.2095
            SUMK = 0.                                                            RRTM_INIT.2096
            DO 4600 IPR = 1, NGN(NGS(12)+IGC)                                    RRTM_INIT.2097
               IPRSM = IPRSM + 1                                                 RRTM_INIT.2098
               SUMK = SUMK + SELFREF(JT,IPRSM)*RWGT(IPRSM+192)                   RRTM_INIT.2099
 4600       CONTINUE                                                             RRTM_INIT.2100
            SELFREFC(JT,IGC) = SUMK                                              RRTM_INIT.2101
 4400    CONTINUE                                                                RRTM_INIT.2102
 4000 CONTINUE                                                                   RRTM_INIT.2103
                                                                                 RRTM_INIT.2104
      DO 7000 JP = 1,9                                                           RRTM_INIT.2105
         IPRSM = 0                                                               RRTM_INIT.2106
         DO 7400 IGC = 1,NGC(13)                                                 RRTM_INIT.2107
            SUMF = 0.                                                            RRTM_INIT.2108
            DO 7600 IPR = 1, NGN(NGS(12)+IGC)                                    RRTM_INIT.2109
               IPRSM = IPRSM + 1                                                 RRTM_INIT.2110
               SUMF = SUMF + FRACREFA(IPRSM,JP)                                  RRTM_INIT.2111
 7600       CONTINUE                                                             RRTM_INIT.2112
            FRACREFAC(IGC,JP) = SUMF                                             RRTM_INIT.2113
 7400    CONTINUE                                                                RRTM_INIT.2114
 7000 CONTINUE                                                                   RRTM_INIT.2115
                                                                                 RRTM_INIT.2116
      DO 8000 JP = 1,9                                                           RRTM_INIT.2117
         DO 8000 IGC = 1,NGC(13)                                                 RRTM_INIT.2118
            FREFA(NGS(12)+IGC,JP) = FRACREFAC(IGC,JP)                            RRTM_INIT.2119
 8000 CONTINUE                                                                   RRTM_INIT.2120
      DO 8100 JP = 1,8                                                           RRTM_INIT.2121
         DO 8100 IGC = 1,NGC(13)                                                 RRTM_INIT.2122
            FREFADF(NGS(12)+IGC,JP) = FRACREFAC(IGC,JP+1) -                      RRTM_INIT.2123
     &                                FRACREFAC(IGC,JP)                          RRTM_INIT.2124
 8100 CONTINUE                                                                   RRTM_INIT.2125
                                                                                 RRTM_INIT.2126
      RETURN                                                                     RRTM_INIT.2127
      END                                                                        RRTM_INIT.2128
                                                                                 RRTM_INIT.2129
C***************************************************************************     RRTM_INIT.2130
      SUBROUTINE CMBGB14                                                         RRTM_INIT.2131
C***************************************************************************     RRTM_INIT.2132
C                                                                                RRTM_INIT.2133
C     BAND 14:  2250-2380 cm-1 (low - CO2; high - CO2)                           RRTM_INIT.2134
C***************************************************************************     RRTM_INIT.2135
                                                                                 RRTM_INIT.2136
C Parameters                                                                     RRTM_INIT.2137
      PARAMETER (MG=16, NBANDS=16)                                               RRTM_INIT.2138
      PARAMETER (NGPT=140, NG14=2)                                               RRTM_INIT.2139
C Input                                                                          RRTM_INIT.2140
      COMMON /K14/      KA(5,13,MG), KB(5,13:59,MG) , SELFREF(10,MG)             RRTM_INIT.2141
      COMMON /RWT/      RWGT(MG*NBANDS)                                          RRTM_INIT.2142
      COMMON /FEATUREC/ NGC(NBANDS), NGS(NBANDS), NGN(NGPT), NGB(NGPT)           RRTM_INIT.2143
C Output                                                                         RRTM_INIT.2144
      COMMON /K14C/  KAC(5,13,NG14), KBC(5,13:59,NG14),                          RRTM_INIT.2145
     &               SELFREFC(10,NG14)                                           RRTM_INIT.2146
      COMMON /PF14C/ FRACREFAC(NG14), FRACREFBC(NG14)                            RRTM_INIT.2147
      COMMON /PFDAT/ FREFA(NGPT,13),FREFB(NGPT,6),                               RRTM_INIT.2148
     &               FREFADF(NGPT,13),FREFBDF(NGPT,6)                            RRTM_INIT.2149
                                                                                 RRTM_INIT.2150
C Data                                                                           RRTM_INIT.2151
      DIMENSION FRACREFA(MG), FRACREFB(MG)                                       RRTM_INIT.2152
                                                                                 RRTM_INIT.2153
      DATA FRACREFA/                                                             RRTM_INIT.2154
C     From P = 1053.6 mb.                                                        RRTM_INIT.2155
     &    0.18446200,0.16795200,0.14949700,0.12036000,                           RRTM_INIT.2156
     &    0.10440100,0.09024280,0.07435880,0.05629380,                           RRTM_INIT.2157
     &    0.03825420,0.00417276,0.00345278,0.00272949,                           RRTM_INIT.2158
     &    0.00200378,0.00127404,0.00050721,0.00004141/                           RRTM_INIT.2159
      DATA FRACREFB/                                                             RRTM_INIT.2160
C     From P = 0.64 mb.                                                          RRTM_INIT.2161
     &    0.19128500,0.16495700,0.14146100,0.11904500,                           RRTM_INIT.2162
     &    0.10350200,0.09151190,0.07604270,0.05806020,                           RRTM_INIT.2163
     &    0.03979950,0.00423959,0.00357439,0.00287559,                           RRTM_INIT.2164
     &    0.00198860,0.00116529,0.00043616,0.00005987/                           RRTM_INIT.2165
                                                                                 RRTM_INIT.2166
      REAL KA,KB,KAC,KBC                                                         RRTM_INIT.2167
                                                                                 RRTM_INIT.2168
      DO 2000 JT = 1,5                                                           RRTM_INIT.2169
         DO 2200 JP = 1,13                                                       RRTM_INIT.2170
            IPRSM = 0                                                            RRTM_INIT.2171
            DO 2400 IGC = 1,NGC(14)                                              RRTM_INIT.2172
               SUMK = 0.                                                         RRTM_INIT.2173
               DO 2600 IPR = 1, NGN(NGS(13)+IGC)                                 RRTM_INIT.2174
                  IPRSM = IPRSM + 1                                              RRTM_INIT.2175
                  SUMK = SUMK + KA(JT,JP,IPRSM)*RWGT(IPRSM+208)                  RRTM_INIT.2176
 2600          CONTINUE                                                          RRTM_INIT.2177
               KAC(JT,JP,IGC) = SUMK                                             RRTM_INIT.2178
 2400       CONTINUE                                                             RRTM_INIT.2179
 2200    CONTINUE                                                                RRTM_INIT.2180
 2000 CONTINUE                                                                   RRTM_INIT.2181
                                                                                 RRTM_INIT.2182
      DO 3000 JT = 1,5                                                           RRTM_INIT.2183
         DO 3200 JP = 13,59                                                      RRTM_INIT.2184
            IPRSM = 0                                                            RRTM_INIT.2185
            DO 3400 IGC = 1,NGC(14)                                              RRTM_INIT.2186
               SUMK = 0.                                                         RRTM_INIT.2187
               DO 3600 IPR = 1, NGN(NGS(13)+IGC)                                 RRTM_INIT.2188
                  IPRSM = IPRSM + 1                                              RRTM_INIT.2189
                  SUMK = SUMK + KB(JT,JP,IPRSM)*RWGT(IPRSM+208)                  RRTM_INIT.2190
 3600          CONTINUE                                                          RRTM_INIT.2191
               KBC(JT,JP,IGC) = SUMK                                             RRTM_INIT.2192
 3400       CONTINUE                                                             RRTM_INIT.2193
 3200    CONTINUE                                                                RRTM_INIT.2194
 3000 CONTINUE                                                                   RRTM_INIT.2195
                                                                                 RRTM_INIT.2196
      DO 4000 JT = 1,10                                                          RRTM_INIT.2197
         IPRSM = 0                                                               RRTM_INIT.2198
         DO 4400 IGC = 1,NGC(14)                                                 RRTM_INIT.2199
            SUMK = 0.                                                            RRTM_INIT.2200
            DO 4600 IPR = 1, NGN(NGS(13)+IGC)                                    RRTM_INIT.2201
               IPRSM = IPRSM + 1                                                 RRTM_INIT.2202
               SUMK = SUMK + SELFREF(JT,IPRSM)*RWGT(IPRSM+208)                   RRTM_INIT.2203
 4600       CONTINUE                                                             RRTM_INIT.2204
            SELFREFC(JT,IGC) = SUMK                                              RRTM_INIT.2205
 4400    CONTINUE                                                                RRTM_INIT.2206
 4000 CONTINUE                                                                   RRTM_INIT.2207
                                                                                 RRTM_INIT.2208
      IPRSM = 0                                                                  RRTM_INIT.2209
      DO 7400 IGC = 1,NGC(14)                                                    RRTM_INIT.2210
         SUMF1= 0.                                                               RRTM_INIT.2211
         SUMF2= 0.                                                               RRTM_INIT.2212
         DO 7600 IPR = 1, NGN(NGS(13)+IGC)                                       RRTM_INIT.2213
            IPRSM = IPRSM + 1                                                    RRTM_INIT.2214
            SUMF1= SUMF1+ FRACREFA(IPRSM)                                        RRTM_INIT.2215
            SUMF2= SUMF2+ FRACREFB(IPRSM)                                        RRTM_INIT.2216
 7600    CONTINUE                                                                RRTM_INIT.2217
         FRACREFAC(IGC) = SUMF1                                                  RRTM_INIT.2218
         FRACREFBC(IGC) = SUMF2                                                  RRTM_INIT.2219
 7400 CONTINUE                                                                   RRTM_INIT.2220
                                                                                 RRTM_INIT.2221
      DO 8000 IGC = 1,NGC(14)                                                    RRTM_INIT.2222
         FREFA(NGS(13)+IGC,1) = FRACREFAC(IGC)                                   RRTM_INIT.2223
         FREFB(NGS(13)+IGC,1) = FRACREFBC(IGC)                                   RRTM_INIT.2224
 8000 CONTINUE                                                                   RRTM_INIT.2225
                                                                                 RRTM_INIT.2226
      RETURN                                                                     RRTM_INIT.2227
      END                                                                        RRTM_INIT.2228
                                                                                 RRTM_INIT.2229
C***************************************************************************     RRTM_INIT.2230
      SUBROUTINE CMBGB15                                                         RRTM_INIT.2231
C***************************************************************************     RRTM_INIT.2232
C                                                                                RRTM_INIT.2233
C     BAND 15:  2380-2600 cm-1 (low - N2O,CO2; high - nothing)                   RRTM_INIT.2234
C***************************************************************************     RRTM_INIT.2235
                                                                                 RRTM_INIT.2236
C Parameters                                                                     RRTM_INIT.2237
      PARAMETER (MG=16, NBANDS=16)                                               RRTM_INIT.2238
      PARAMETER (NGPT=140, NG15=2)                                               RRTM_INIT.2239
C Input                                                                          RRTM_INIT.2240
      COMMON /K15/      KA(9,5,13,MG), SELFREF(10,MG)                            RRTM_INIT.2241
      COMMON /RWT/      RWGT(MG*NBANDS)                                          RRTM_INIT.2242
      COMMON /FEATUREC/ NGC(NBANDS), NGS(NBANDS), NGN(NGPT), NGB(NGPT)           RRTM_INIT.2243
C Output                                                                         RRTM_INIT.2244
      COMMON /K15C/  KAC(9,5,13,NG15),                                           RRTM_INIT.2245
     &               SELFREFC(10,NG15)                                           RRTM_INIT.2246
      COMMON /PF15C/ FRACREFAC(NG15,9)                                           RRTM_INIT.2247
      COMMON /PFDAT/ FREFA(NGPT,13),FREFB(NGPT,6),                               RRTM_INIT.2248
     &               FREFADF(NGPT,13),FREFBDF(NGPT,6)                            RRTM_INIT.2249
                                                                                 RRTM_INIT.2250
C Data                                                                           RRTM_INIT.2251
      DIMENSION FRACREFA(MG,9)                                                   RRTM_INIT.2252
                                                                                 RRTM_INIT.2253
      DATA FRACREFA/                                                             RRTM_INIT.2254
C     From P = 1053.6 mb.                                                        RRTM_INIT.2255
     &    0.11287100,0.12070200,0.12729000,0.12858100,                           RRTM_INIT.2256
     &    0.12743001,0.11961800,0.10290400,0.07888980,                           RRTM_INIT.2257
     &    0.05900120,0.00667979,0.00552926,0.00436993,                           RRTM_INIT.2258
     &    0.00320611,0.00204765,0.00077371,0.00010894,                           RRTM_INIT.2259
     &    0.13918801,0.16353001,0.16155800,0.14090499,                           RRTM_INIT.2260
     &    0.11322300,0.08757720,0.07225720,0.05173390,                           RRTM_INIT.2261
     &    0.04731360,0.00667979,0.00552926,0.00436993,                           RRTM_INIT.2262
     &    0.00320611,0.00204765,0.00077371,0.00010894,                           RRTM_INIT.2263
     &    0.14687300,0.17853101,0.15664500,0.13351700,                           RRTM_INIT.2264
     &    0.10791200,0.08684320,0.07158090,0.05198410,                           RRTM_INIT.2265
     &    0.04340110,0.00667979,0.00552926,0.00436993,                           RRTM_INIT.2266
     &    0.00320611,0.00204765,0.00077371,0.00010894,                           RRTM_INIT.2267
     &    0.15760700,0.17759100,0.15158001,0.13193300,                           RRTM_INIT.2268
     &    0.10742800,0.08693760,0.07159490,0.05196250,                           RRTM_INIT.2269
     &    0.04065270,0.00667979,0.00552926,0.00436993,                           RRTM_INIT.2270
     &    0.00320611,0.00204765,0.00077371,0.00010894,                           RRTM_INIT.2271
     &    0.16646700,0.17299300,0.15018500,0.13138700,                           RRTM_INIT.2272
     &    0.10735900,0.08713110,0.07130330,0.05279420,                           RRTM_INIT.2273
     &    0.03766730,0.00667979,0.00552926,0.00436993,                           RRTM_INIT.2274
     &    0.00320611,0.00204765,0.00077371,0.00010894,                           RRTM_INIT.2275
     &    0.17546000,0.16666500,0.14969499,0.13105400,                           RRTM_INIT.2276
     &    0.10782500,0.08718610,0.07156770,0.05308320,                           RRTM_INIT.2277
     &    0.03753960,0.00432465,0.00509623,0.00436993,                           RRTM_INIT.2278
     &    0.00320611,0.00204765,0.00077371,0.00010894,                           RRTM_INIT.2279
     &    0.18378501,0.16064601,0.14940400,0.13146400,                           RRTM_INIT.2280
     &    0.10810300,0.08775740,0.07115360,0.05400040,                           RRTM_INIT.2281
     &    0.03689970,0.00388333,0.00323610,0.00353414,                           RRTM_INIT.2282
     &    0.00320611,0.00204765,0.00077371,0.00010894,                           RRTM_INIT.2283
     &    0.18966800,0.15744300,0.14993000,0.13152599,                           RRTM_INIT.2284
     &    0.10899200,0.08858690,0.07142920,0.05399600,                           RRTM_INIT.2285
     &    0.03433460,0.00374886,0.00302066,0.00240653,                           RRTM_INIT.2286
     &    0.00199205,0.00204765,0.00077371,0.00010894,                           RRTM_INIT.2287
     &    0.11887100,0.12479600,0.12569501,0.12839900,                           RRTM_INIT.2288
     &    0.12473500,0.12012800,0.11086700,0.08493590,                           RRTM_INIT.2289
     &    0.05063770,0.00328723,0.00266849,0.00210232,                           RRTM_INIT.2290
     &    0.00152114,0.00095635,0.00035374,0.00004980/                           RRTM_INIT.2291
                                                                                 RRTM_INIT.2292
      REAL KA,KAC                                                                RRTM_INIT.2293
                                                                                 RRTM_INIT.2294
      DO 2000 JN = 1,9                                                           RRTM_INIT.2295
         DO 2000 JT = 1,5                                                        RRTM_INIT.2296
            DO 2200 JP = 1,13                                                    RRTM_INIT.2297
               IPRSM = 0                                                         RRTM_INIT.2298
               DO 2400 IGC = 1,NGC(15)                                           RRTM_INIT.2299
                  SUMK = 0.                                                      RRTM_INIT.2300
                  DO 2600 IPR = 1, NGN(NGS(14)+IGC)                              RRTM_INIT.2301
                     IPRSM = IPRSM + 1                                           RRTM_INIT.2302
                     SUMK = SUMK + KA(JN,JT,JP,IPRSM)*RWGT(IPRSM+224)            RRTM_INIT.2303
 2600             CONTINUE                                                       RRTM_INIT.2304
                  KAC(JN,JT,JP,IGC) = SUMK                                       RRTM_INIT.2305
 2400          CONTINUE                                                          RRTM_INIT.2306
 2200       CONTINUE                                                             RRTM_INIT.2307
 2000 CONTINUE                                                                   RRTM_INIT.2308
                                                                                 RRTM_INIT.2309
      DO 4000 JT = 1,10                                                          RRTM_INIT.2310
         IPRSM = 0                                                               RRTM_INIT.2311
         DO 4400 IGC = 1,NGC(15)                                                 RRTM_INIT.2312
            SUMK = 0.                                                            RRTM_INIT.2313
            DO 4600 IPR = 1, NGN(NGS(14)+IGC)                                    RRTM_INIT.2314
               IPRSM = IPRSM + 1                                                 RRTM_INIT.2315
               SUMK = SUMK + SELFREF(JT,IPRSM)*RWGT(IPRSM+224)                   RRTM_INIT.2316
 4600       CONTINUE                                                             RRTM_INIT.2317
            SELFREFC(JT,IGC) = SUMK                                              RRTM_INIT.2318
 4400    CONTINUE                                                                RRTM_INIT.2319
 4000 CONTINUE                                                                   RRTM_INIT.2320
                                                                                 RRTM_INIT.2321
      DO 7000 JP = 1,9                                                           RRTM_INIT.2322
         IPRSM = 0                                                               RRTM_INIT.2323
         DO 7400 IGC = 1,NGC(15)                                                 RRTM_INIT.2324
            SUMF = 0.                                                            RRTM_INIT.2325
            DO 7600 IPR = 1, NGN(NGS(14)+IGC)                                    RRTM_INIT.2326
               IPRSM = IPRSM + 1                                                 RRTM_INIT.2327
               SUMF = SUMF + FRACREFA(IPRSM,JP)                                  RRTM_INIT.2328
 7600       CONTINUE                                                             RRTM_INIT.2329
            FRACREFAC(IGC,JP) = SUMF                                             RRTM_INIT.2330
 7400    CONTINUE                                                                RRTM_INIT.2331
 7000 CONTINUE                                                                   RRTM_INIT.2332
                                                                                 RRTM_INIT.2333
      DO 8000 JP = 1,9                                                           RRTM_INIT.2334
         DO 8000 IGC = 1,NGC(15)                                                 RRTM_INIT.2335
            FREFA(NGS(14)+IGC,JP) = FRACREFAC(IGC,JP)                            RRTM_INIT.2336
 8000 CONTINUE                                                                   RRTM_INIT.2337
      DO 8100 JP = 1,8                                                           RRTM_INIT.2338
         DO 8100 IGC = 1,NGC(15)                                                 RRTM_INIT.2339
            FREFADF(NGS(14)+IGC,JP) = FRACREFAC(IGC,JP+1) -                      RRTM_INIT.2340
     &                                FRACREFAC(IGC,JP)                          RRTM_INIT.2341
 8100 CONTINUE                                                                   RRTM_INIT.2342
                                                                                 RRTM_INIT.2343
      RETURN                                                                     RRTM_INIT.2344
      END                                                                        RRTM_INIT.2345
                                                                                 RRTM_INIT.2346
C***************************************************************************     RRTM_INIT.2347
      SUBROUTINE CMBGB16                                                         RRTM_INIT.2348
C***************************************************************************     RRTM_INIT.2349
C                                                                                RRTM_INIT.2350
C     BAND 16:  2600-3000 cm-1 (low - H2O,CH4; high - nothing)                   RRTM_INIT.2351
C***************************************************************************     RRTM_INIT.2352
                                                                                 RRTM_INIT.2353
C Parameters                                                                     RRTM_INIT.2354
      PARAMETER (MG=16, NBANDS=16)                                               RRTM_INIT.2355
      PARAMETER (NGPT=140, NG16=2)                                               RRTM_INIT.2356
C Input                                                                          RRTM_INIT.2357
      COMMON /K16/      KA(9,5,13,MG), SELFREF(10,MG)                            RRTM_INIT.2358
      COMMON /RWT/      RWGT(MG*NBANDS)                                          RRTM_INIT.2359
      COMMON /FEATUREC/ NGC(NBANDS), NGS(NBANDS), NGN(NGPT), NGB(NGPT)           RRTM_INIT.2360
C Output                                                                         RRTM_INIT.2361
      COMMON /K16C/  KAC(9,5,13,NG16),                                           RRTM_INIT.2362
     &               SELFREFC(10,NG16)                                           RRTM_INIT.2363
      COMMON /PF16C/ FRACREFAC(NG16,9)                                           RRTM_INIT.2364
      COMMON /PFDAT/ FREFA(NGPT,13),FREFB(NGPT,6),                               RRTM_INIT.2365
     &               FREFADF(NGPT,13),FREFBDF(NGPT,6)                            RRTM_INIT.2366
                                                                                 RRTM_INIT.2367
C Data                                                                           RRTM_INIT.2368
      DIMENSION FRACREFA(MG,9)                                                   RRTM_INIT.2369
                                                                                 RRTM_INIT.2370
      DATA FRACREFA/                                                             RRTM_INIT.2371
C     From P = 862.6 mb.                                                         RRTM_INIT.2372
     &    0.17356300,0.18880001,0.17704099,0.13661300,                           RRTM_INIT.2373
     &    0.10691600,0.08222480,0.05939860,0.04230810,                           RRTM_INIT.2374
     &    0.02526330,0.00244532,0.00193541,0.00150415,                           RRTM_INIT.2375
     &    0.00103528,0.00067068,0.00024951,0.00003348,                           RRTM_INIT.2376
     &    0.17779499,0.19837400,0.16557600,0.13470000,                           RRTM_INIT.2377
     &    0.11013600,0.08342720,0.05987030,0.03938700,                           RRTM_INIT.2378
     &    0.02293650,0.00238849,0.00192400,0.00149921,                           RRTM_INIT.2379
     &    0.00103539,0.00067150,0.00024822,0.00003348,                           RRTM_INIT.2380
     &    0.18535601,0.19407199,0.16053200,0.13300700,                           RRTM_INIT.2381
     &    0.10779000,0.08408500,0.06480450,0.04070160,                           RRTM_INIT.2382
     &    0.02203590,0.00227779,0.00189074,0.00146888,                           RRTM_INIT.2383
     &    0.00103147,0.00066770,0.00024751,0.00003348,                           RRTM_INIT.2384
     &    0.19139200,0.18917400,0.15748601,0.13240699,                           RRTM_INIT.2385
     &    0.10557300,0.08383260,0.06724060,0.04364450,                           RRTM_INIT.2386
     &    0.02175820,0.00225436,0.00184421,0.00143153,                           RRTM_INIT.2387
     &    0.00103027,0.00066066,0.00024222,0.00003148,                           RRTM_INIT.2388
     &    0.19547801,0.18539500,0.15442000,0.13114899,                           RRTM_INIT.2389
     &    0.10515600,0.08350350,0.06909780,0.04671630,                           RRTM_INIT.2390
     &    0.02168820,0.00224400,0.00182009,0.00139098,                           RRTM_INIT.2391
     &    0.00102582,0.00065367,0.00023202,0.00003148,                           RRTM_INIT.2392
     &    0.19757500,0.18266800,0.15208900,0.12897800,                           RRTM_INIT.2393
     &    0.10637200,0.08391220,0.06989830,0.04964120,                           RRTM_INIT.2394
     &    0.02155800,0.00224310,0.00177358,0.00138184,                           RRTM_INIT.2395
     &    0.00101538,0.00063370,0.00023227,0.00003148,                           RRTM_INIT.2396
     &    0.20145500,0.17692900,0.14940600,0.12690400,                           RRTM_INIT.2397
     &    0.10828800,0.08553720,0.07004940,0.05153430,                           RRTM_INIT.2398
     &    0.02268740,0.00216943,0.00178603,0.00137754,                           RRTM_INIT.2399
     &    0.00098344,0.00063165,0.00023218,0.00003148,                           RRTM_INIT.2400
     &    0.20383500,0.17047501,0.14570600,0.12679300,                           RRTM_INIT.2401
     &    0.11043100,0.08719150,0.07045440,0.05345420,                           RRTM_INIT.2402
     &    0.02448340,0.00215839,0.00175893,0.00138296,                           RRTM_INIT.2403
     &    0.00098318,0.00063188,0.00023199,0.00003148,                           RRTM_INIT.2404
     &    0.18680701,0.15961801,0.15092900,0.13049100,                           RRTM_INIT.2405
     &    0.11418400,0.09380540,0.07093450,0.05664280,                           RRTM_INIT.2406
     &    0.02938410,0.00217751,0.00176766,0.00138275,                           RRTM_INIT.2407
     &    0.00098377,0.00063181,0.00023193,0.00003148/                           RRTM_INIT.2408
                                                                                 RRTM_INIT.2409
      REAL KA,KAC                                                                RRTM_INIT.2410
                                                                                 RRTM_INIT.2411
      DO 2000 JN = 1,9                                                           RRTM_INIT.2412
         DO 2000 JT = 1,5                                                        RRTM_INIT.2413
            DO 2200 JP = 1,13                                                    RRTM_INIT.2414
               IPRSM = 0                                                         RRTM_INIT.2415
               DO 2400 IGC = 1,NGC(16)                                           RRTM_INIT.2416
                  SUMK = 0.                                                      RRTM_INIT.2417
                  DO 2600 IPR = 1, NGN(NGS(15)+IGC)                              RRTM_INIT.2418
                     IPRSM = IPRSM + 1                                           RRTM_INIT.2419
                     SUMK = SUMK + KA(JN,JT,JP,IPRSM)*RWGT(IPRSM+240)            RRTM_INIT.2420
 2600             CONTINUE                                                       RRTM_INIT.2421
                  KAC(JN,JT,JP,IGC) = SUMK                                       RRTM_INIT.2422
 2400          CONTINUE                                                          RRTM_INIT.2423
 2200       CONTINUE                                                             RRTM_INIT.2424
 2000 CONTINUE                                                                   RRTM_INIT.2425
                                                                                 RRTM_INIT.2426
      DO 4000 JT = 1,10                                                          RRTM_INIT.2427
         IPRSM = 0                                                               RRTM_INIT.2428
         DO 4400 IGC = 1,NGC(16)                                                 RRTM_INIT.2429
            SUMK = 0.                                                            RRTM_INIT.2430
            DO 4600 IPR = 1, NGN(NGS(15)+IGC)                                    RRTM_INIT.2431
               IPRSM = IPRSM + 1                                                 RRTM_INIT.2432
               SUMK = SUMK + SELFREF(JT,IPRSM)*RWGT(IPRSM+240)                   RRTM_INIT.2433
 4600       CONTINUE                                                             RRTM_INIT.2434
            SELFREFC(JT,IGC) = SUMK                                              RRTM_INIT.2435
 4400    CONTINUE                                                                RRTM_INIT.2436
 4000 CONTINUE                                                                   RRTM_INIT.2437
                                                                                 RRTM_INIT.2438
      DO 7000 JP = 1,9                                                           RRTM_INIT.2439
         IPRSM = 0                                                               RRTM_INIT.2440
         DO 7400 IGC = 1,NGC(16)                                                 RRTM_INIT.2441
            SUMF = 0.                                                            RRTM_INIT.2442
            DO 7600 IPR = 1, NGN(NGS(15)+IGC)                                    RRTM_INIT.2443
               IPRSM = IPRSM + 1                                                 RRTM_INIT.2444
               SUMF = SUMF + FRACREFA(IPRSM,JP)                                  RRTM_INIT.2445
 7600       CONTINUE                                                             RRTM_INIT.2446
            FRACREFAC(IGC,JP) = SUMF                                             RRTM_INIT.2447
 7400    CONTINUE                                                                RRTM_INIT.2448
 7000 CONTINUE                                                                   RRTM_INIT.2449
                                                                                 RRTM_INIT.2450
      DO 8000 JP = 1,9                                                           RRTM_INIT.2451
         DO 8000 IGC = 1,NGC(16)                                                 RRTM_INIT.2452
            FREFA(NGS(15)+IGC,JP) = FRACREFAC(IGC,JP)                            RRTM_INIT.2453
 8000 CONTINUE                                                                   RRTM_INIT.2454
                                                                                 RRTM_INIT.2455
      DO 8100 JP = 1,8                                                           RRTM_INIT.2456
         DO 8100 IGC = 1,NGC(16)                                                 RRTM_INIT.2457
            FREFADF(NGS(15)+IGC,JP) = FRACREFAC(IGC,JP+1) -                      RRTM_INIT.2458
     &                                FRACREFAC(IGC,JP)                          RRTM_INIT.2459
 8100 CONTINUE                                                                   RRTM_INIT.2460
                                                                                 RRTM_INIT.2461
      RETURN                                                                     RRTM_INIT.2462
      END                                                                        RRTM_INIT.2463
