C***************************************************************************     RRTM_RTRN.1
      SUBROUTINE RTRN                                                            RRTM_RTRN.2
C***************************************************************************     RRTM_RTRN.3
C  RRTM Longwave Radiative Transfer Model                                        RRTM_RTRN.4
C  Atmospheric and Environmental Research, Inc., Cambridge, MA                   RRTM_RTRN.5
C                                                                                RRTM_RTRN.6
C  Original version:       E. J. Mlawer, et al.                                  RRTM_RTRN.7
C  Revision for NCAR CCM:  Michael J. Iacono; September, 1998                    RRTM_RTRN.8
C                                                                                RRTM_RTRN.9
C  This program calculates the upward fluxes, downward fluxes, and               RRTM_RTRN.10
C  heating rates for an arbitrary clear or cloudy atmosphere.  The input         RRTM_RTRN.11
C  to this program is the atmospheric profile, all Planck function               RRTM_RTRN.12
C  information, and the cloud fraction by layer.  The diffusivity angle          RRTM_RTRN.13
C  (SECANG=1.66) is used for the angle integration for consistency with          RRTM_RTRN.14
C  the NCAR CCM; the Gaussian weight appropriate to this angle (WTNUM=0.5)       RRTM_RTRN.15
C  is applied here.  Note that use of the emissivity angle for the flux          RRTM_RTRN.16
C  integration can cause errors of 1 to 4 W/m2 within cloudy layers.             RRTM_RTRN.17
C***************************************************************************     RRTM_RTRN.18
                                                                                 RRTM_RTRN.19
C Parameters                                                                     RRTM_RTRN.20
      PARAMETER (MXLAY=203)                                                      RRTM_RTRN.21
      PARAMETER (NBANDS=16)                                                      RRTM_RTRN.22
      PARAMETER (NGPT=140)                                                       RRTM_RTRN.23
      PARAMETER (SECANG=1.66)                                                    RRTM_RTRN.24
      PARAMETER (WTNUM=0.5)                                                      RRTM_RTRN.25
                                                                                 RRTM_RTRN.26
C Input                                                                          RRTM_RTRN.27
      COMMON /CONSTANTS/ PI,FLUXFAC,HEATFAC                                      RRTM_RTRN.28
      COMMON /FEATUREC/  NGC(NBANDS), NGS(NBANDS), NGN(NGPT), NGB(NGPT)          RRTM_RTRN.29
      COMMON /BANDS/     WAVENUM1(NBANDS),WAVENUM2(NBANDS),                      RRTM_RTRN.30
     &                   DELWAVE(NBANDS)                                         RRTM_RTRN.31
      COMMON /PROFILE/   NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),                      RRTM_RTRN.32
     &                   PZ(0:MXLAY),TZ(0:MXLAY)                                 RRTM_RTRN.33
      COMMON /CLOUDDAT/  CLDFRAC(MXLAY),TAUCLOUD(MXLAY)                          RRTM_RTRN.34
      COMMON /CLDFLG/    ICLDLYR(MXLAY),ICLD                                     RRTM_RTRN.35
      COMMON /SURFACE/   TBOUND,IREFLECT,SEMISS(NBANDS)                          RRTM_RTRN.36
      COMMON /SP/        PF(NGPT,MXLAY)                                          RRTM_RTRN.37
      COMMON /TBLINDX/   ITR(NGPT,MXLAY)                                         RRTM_RTRN.38
      COMMON /PADE/      BPADE                                                   RRTM_RTRN.39
      COMMON /TAUTBL/    TAU(0:5000)                                             RRTM_RTRN.40
      COMMON /TFTBL/     TF(0:5000)                                              RRTM_RTRN.41
      COMMON /TRNTBL/    TRANS(0:5000)                                           RRTM_RTRN.42
                                                                                 RRTM_RTRN.43
C  Output                                                                        RRTM_RTRN.44
      COMMON /OUTRRTM/    TOTUFLUX(0:MXLAY), TOTDFLUX(0:MXLAY),                  RRTM_RTRN.45
     &                   FNET(0:MXLAY), HTR(0:MXLAY)                             RRTM_RTRN.46
      COMMON /OUTCLR/    TOTUCLFL(0:MXLAY), TOTDCLFL(0:MXLAY),                   RRTM_RTRN.47
     &                   FNETC(0:MXLAY), HTRC(0:MXLAY)                           RRTM_RTRN.48
      COMMON /CLRFLX/    CLRNTTOA,CLRNTSRF                                       RRTM_RTRN.49
                                                                                 RRTM_RTRN.50
C Local                                                                          RRTM_RTRN.51
      COMMON /AVGPLNK/   TOTPLNK(181,NBANDS), TOTPLK16(181)                      RRTM_RTRN.52
                                                                                 RRTM_RTRN.53
C RRTM Definitions                                                               RRTM_RTRN.54
C Input                                                                          RRTM_RTRN.55
C    MXLAY                        ! Maximum number of model layers               RRTM_RTRN.56
C    NGPT                         ! Total number of g-point subintervals         RRTM_RTRN.57
C    NBANDS                       ! Number of longwave spectral bands            RRTM_RTRN.58
C    SECANG                       ! Diffusivity angle                            RRTM_RTRN.59
C    WTNUM                        ! Weight for radiance to flux conversion       RRTM_RTRN.60
C    NLAYERS                      ! Number of model layers (plev+1)              RRTM_RTRN.61
C    PAVEL(MXLAY)                 ! Layer pressures (mb)                         RRTM_RTRN.62
C    PZ(0:MXLAY)                  ! Level (interface) pressures (mb)             RRTM_RTRN.63
C    TAVEL(MXLAY)                 ! Layer temperatures (K)                       RRTM_RTRN.64
C    TZ(0:MXLAY)                  ! Level (interface) temperatures(mb)           RRTM_RTRN.65
C    TBOUND                       ! Surface temperature (K)                      RRTM_RTRN.66
C    CLDFRAC(MXLAY)               ! Layer cloud fraction                         RRTM_RTRN.67
C    TAUCLOUD(MXLAY)              ! Layer cloud optical depth                    RRTM_RTRN.68
C    ITR(NGPT,MXLAY)              ! Integer look-up table index                  RRTM_RTRN.69
C    PF(NGPT,MXLAY)               ! Planck fractions                             RRTM_RTRN.70
C    ICLDLYR(MXLAY)               ! Flag for cloudy layers                       RRTM_RTRN.71
C    ICLD                         ! Flag for cloudy in column                    RRTM_RTRN.72
C    IREFLECT                     ! Inactive in this version                     RRTM_RTRN.73
C    SEMISS(NBANDS)               ! Surface emissivities for each band           RRTM_RTRN.74
C    BPADE                        ! Pade constant                                RRTM_RTRN.75
C    TAU                          ! Clear sky optical depth look-up table        RRTM_RTRN.76
C    TF                           ! Tau transition function look-up table        RRTM_RTRN.77
C    TRANS                        ! Clear sky transmittance look-up table        RRTM_RTRN.78
C Local                                                                          RRTM_RTRN.79
C    ABSS(NGPT*MXLAY)             ! Gaseous absorptivity                         RRTM_RTRN.80
C    ABSCLD(MXLAY)                ! Cloud absorptivity                           RRTM_RTRN.81
C    ATOT(NGPT*MXLAY)             ! Combined gaseous and cloud absorptivity      RRTM_RTRN.82
C    ODCLR(NGPT,MXLAY)            ! Clear sky (gaseous) optical depth            RRTM_RTRN.83
C    ODCLD(MXLAY)                 ! Cloud optical depth                          RRTM_RTRN.84
C    EFCLFRAC(MXLAY)              ! Effective cloud fraction                     RRTM_RTRN.85
C    RADLU(NGPT)                  ! Upward radiance                              RRTM_RTRN.86
C    URAD                         ! Spectrally summed upward radiance            RRTM_RTRN.87
C    RADCLRU(NGPT)                ! Clear sky upward radiance                    RRTM_RTRN.88
C    CLRURAD                      ! Spectrally summed clear sky upward radiance  RRTM_RTRN.89
C    RADLD(NGPT)                  ! Downward radiance                            RRTM_RTRN.90
C    DRAD                         ! Spectrally summed downward radiance          RRTM_RTRN.91
C    RADCLRD(NGPT)                ! Clear sky downward radiance                  RRTM_RTRN.92
C    CLRDRAD                      ! Spectrally summed clear sky downward radianc RRTM_RTRN.93
C Output                                                                         RRTM_RTRN.94
C    TOTUFLUX(0:MXLAY)            ! Upward longwave flux (W/m2)                  RRTM_RTRN.95
C    TOTDFLUX(0:MXLAY)            ! Downward longwave flux (W/m2)                RRTM_RTRN.96
C    FNET(0:MXLAY)                ! Net longwave flux (W/m2)                     RRTM_RTRN.97
C    HTR(0:MXLAY)                 ! Longwave heating rate (K/day)                RRTM_RTRN.98
C    CLRNTTOA                     ! Clear sky TOA outgoing flux (W/m2)           RRTM_RTRN.99
C    CLRNTSFC                     ! Clear sky net surface flux (W/m2)            RRTM_RTRN.100
C    TOTUCLFL(0:MXLAY)            ! Clear sky upward longwave flux (W/m2)        RRTM_RTRN.101
C    TOTDCLFL(0:MXLAY)            ! Clear sky downward longwave flux (W/m2)      RRTM_RTRN.102
C    FNETC(0:MXLAY)               ! Clear sky net longwave flux (W/m2)           RRTM_RTRN.103
C    HTRC(0:MXLAY)                ! Clear sky longwave heating rate (K/day)      RRTM_RTRN.104
C                                                                                RRTM_RTRN.105
                                                                                 RRTM_RTRN.106
C This compiler directive was added to insure private common block storage       RRTM_RTRN.107
C in multi-tasked mode on a CRAY or SGI for all commons except those that        RRTM_RTRN.108
C carry constants.                                                               RRTM_RTRN.109
C SGI                                                                            RRTM_RTRN.113
C$OMP THREADPRIVATE (/PROFILE/,/SURFACE/,/CLOUDDAT/,/CLDFLG/)                    RRTM_RTRN.114
C$OMP THREADPRIVATE (/SP/,/TBLINDX/,/OUTRRTM/)                                   RRTM_RTRN.115
C$OMP THREADPRIVATE (/OUTCLR/,/CLRFLX/)                                          RRTM_RTRN.116
C ***                                                                            RRTM_RTRN.117
                                                                                 RRTM_RTRN.118
      DIMENSION BBU(NGPT*MXLAY),BBUTOT(NGPT*MXLAY),BGLEV(NGPT)                   RRTM_RTRN.119
      DIMENSION PLANKBND(NBANDS),PLNKEMIT(NBANDS)                                RRTM_RTRN.120
      DIMENSION PLVL(NBANDS,0:MXLAY),PLAY(NBANDS,MXLAY)                          RRTM_RTRN.121
      DIMENSION INDLAY(MXLAY),INDLEV(0:MXLAY)                                    RRTM_RTRN.122
      DIMENSION TLAYFRAC(MXLAY),TLEVFRAC(0:MXLAY)                                RRTM_RTRN.123
      DIMENSION ABSS(NGPT*MXLAY),ABSCLD(MXLAY),ATOT(NGPT*MXLAY)                  RRTM_RTRN.124
      DIMENSION ODCLR(NGPT,MXLAY),ODCLD(MXLAY),EFCLFRAC(MXLAY)                   RRTM_RTRN.125
      DIMENSION RADLU(NGPT),RADLD(NGPT)                                          RRTM_RTRN.126
      DIMENSION RADCLRU(NGPT),RADCLRD(NGPT)                                      RRTM_RTRN.127
      DIMENSION SEMIS(NGPT),RADUEMIT(NGPT)                                       RRTM_RTRN.128
                                                                                 RRTM_RTRN.129
      INDBOUND = TBOUND - 159.                                                   RRTM_RTRN.130
      TBNDFRAC = TBOUND - INT(TBOUND)                                            RRTM_RTRN.131
                                                                                 RRTM_RTRN.132
      DO 200 LAY = 0, NLAYERS                                                    RRTM_RTRN.133
         TOTUFLUX(LAY) = 0.0                                                     RRTM_RTRN.134
         TOTDFLUX(LAY) = 0.0                                                     RRTM_RTRN.135
         TOTUCLFL(LAY) = 0.0                                                     RRTM_RTRN.136
         TOTDCLFL(LAY) = 0.0                                                     RRTM_RTRN.137
         INDLEV(LAY) = TZ(LAY) - 159.                                            RRTM_RTRN.138
         TLEVFRAC(LAY) = TZ(LAY) - INT(TZ(LAY))                                  RRTM_RTRN.139
 200  CONTINUE                                                                   RRTM_RTRN.140
                                                                                 RRTM_RTRN.141
      DO 220 LEV = 1, NLAYERS                                                    RRTM_RTRN.142
                                                                                 RRTM_RTRN.143
         IF (ICLDLYR(LEV).EQ.1) THEN                                             RRTM_RTRN.144
            INDLAY(LEV) = TAVEL(LEV) - 159.                                      RRTM_RTRN.145
            TLAYFRAC(LEV) = TAVEL(LEV) - INT(TAVEL(LEV))                         RRTM_RTRN.146
C  Cloudy sky optical depth and absorptivity.                                    RRTM_RTRN.147
            ODCLD(LEV) = SECANG * TAUCLOUD(LEV)                                  RRTM_RTRN.148
            TRANSCLD = EXP(-ODCLD(LEV))                                          RRTM_RTRN.149
            ABSCLD(LEV) = 1. - TRANSCLD                                          RRTM_RTRN.150
            EFCLFRAC(LEV) = ABSCLD(LEV) * CLDFRAC(LEV)                           RRTM_RTRN.151
C  Get clear sky optical depth from TAU lookup table                             RRTM_RTRN.152
            DO 250 IPR = 1, NGPT                                                 RRTM_RTRN.153
               IND = ITR(IPR,LEV)                                                RRTM_RTRN.154
               ODCLR(IPR,LEV) = TAU(IND)                                         RRTM_RTRN.155
 250        CONTINUE                                                             RRTM_RTRN.156
         ELSE                                                                    RRTM_RTRN.157
            INDLAY(LEV) = TAVEL(LEV) - 159.                                      RRTM_RTRN.158
            TLAYFRAC(LEV) = TAVEL(LEV) - INT(TAVEL(LEV))                         RRTM_RTRN.159
         ENDIF                                                                   RRTM_RTRN.160
                                                                                 RRTM_RTRN.161
 220  CONTINUE                                                                   RRTM_RTRN.162
                                                                                 RRTM_RTRN.163
C      SUMPL   = 0.0                                                             RRTM_RTRN.164
C      SUMPLEM = 0.0                                                             RRTM_RTRN.165
C *** Loop over frequency bands.                                                 RRTM_RTRN.166
                                                                                 RRTM_RTRN.167
      DO 600 IBAND = 1, NBANDS                                                   RRTM_RTRN.168
         DBDTLEV = TOTPLNK(INDBOUND+1,IBAND)-TOTPLNK(INDBOUND,IBAND)             RRTM_RTRN.169
         PLANKBND(IBAND) = DELWAVE(IBAND) * (TOTPLNK(INDBOUND,IBAND) +           RRTM_RTRN.170
     &        TBNDFRAC * DBDTLEV)                                                RRTM_RTRN.171
         DBDTLEV = TOTPLNK(INDLEV(0)+1,IBAND) -                                  RRTM_RTRN.172
     &        TOTPLNK(INDLEV(0),IBAND)                                           RRTM_RTRN.173
         PLVL(IBAND,0) = DELWAVE(IBAND) * (TOTPLNK(INDLEV(0),IBAND) +            RRTM_RTRN.174
     &        TLEVFRAC(0)*DBDTLEV)                                               RRTM_RTRN.175
                                                                                 RRTM_RTRN.176
         PLNKEMIT(IBAND) = SEMISS(IBAND) * PLANKBND(IBAND)                       RRTM_RTRN.177
C         SUMPLEM  = SUMPLEM + PLNKEMIT(IBAND)                                   RRTM_RTRN.178
C         SUMPL    = SUMPL   + PLANKBND(IBAND)                                   RRTM_RTRN.179
                                                                                 RRTM_RTRN.180
         DO 300 LEV = 1, NLAYERS                                                 RRTM_RTRN.181
C     Calculate the integrated Planck functions at the level and                 RRTM_RTRN.182
C     layer temperatures.                                                        RRTM_RTRN.183
            DBDTLEV = TOTPLNK(INDLEV(LEV)+1,IBAND) -                             RRTM_RTRN.184
     &           TOTPLNK(INDLEV(LEV),IBAND)                                      RRTM_RTRN.185
            DBDTLAY = TOTPLNK(INDLAY(LEV)+1,IBAND) -                             RRTM_RTRN.186
     &           TOTPLNK(INDLAY(LEV),IBAND)                                      RRTM_RTRN.187
            PLAY(IBAND,LEV) = DELWAVE(IBAND) *                                   RRTM_RTRN.188
     &           (TOTPLNK(INDLAY(LEV),IBAND) + TLAYFRAC(LEV) * DBDTLAY)          RRTM_RTRN.189
            PLVL(IBAND,LEV) = DELWAVE(IBAND) *                                   RRTM_RTRN.190
     &           (TOTPLNK(INDLEV(LEV),IBAND) + TLEVFRAC(LEV) * DBDTLEV)          RRTM_RTRN.191
 300     CONTINUE                                                                RRTM_RTRN.192
 600  CONTINUE                                                                   RRTM_RTRN.193
                                                                                 RRTM_RTRN.194
C      SEMISLW = SUMPLEM / SUMPL                                                 RRTM_RTRN.195
                                                                                 RRTM_RTRN.196
C *** Initialize for radiative transfer.                                         RRTM_RTRN.197
      DO 500 IPR = 1, NGPT                                                       RRTM_RTRN.198
         RADCLRD(IPR) = 0.                                                       RRTM_RTRN.199
         RADLD(IPR) = 0.                                                         RRTM_RTRN.200
         SEMIS(IPR) = SEMISS(NGB(IPR))                                           RRTM_RTRN.201
         RADUEMIT(IPR) = PF(IPR,1) * PLNKEMIT(NGB(IPR))                          RRTM_RTRN.202
         BGLEV(IPR) = PF(IPR,NLAYERS) * PLVL(NGB(IPR),NLAYERS)                   RRTM_RTRN.203
 500  CONTINUE                                                                   RRTM_RTRN.204
                                                                                 RRTM_RTRN.205
                                                                                 RRTM_RTRN.206
C *** DOWNWARD RADIATIVE TRANSFER                                                RRTM_RTRN.207
C *** DRAD holds summed radiance for total sky stream                            RRTM_RTRN.208
C *** CLRDRAD holds summed radiance for clear sky stream                         RRTM_RTRN.209
                                                                                 RRTM_RTRN.210
      ICLDDN = 0                                                                 RRTM_RTRN.211
      DO 3000 LEV = NLAYERS, 1, -1                                               RRTM_RTRN.212
         DRAD = 0.0                                                              RRTM_RTRN.213
         CLRDRAD = 0.0                                                           RRTM_RTRN.214
                                                                                 RRTM_RTRN.215
         IF (ICLDLYR(LEV).EQ.1) THEN                                             RRTM_RTRN.216
                                                                                 RRTM_RTRN.217
C *** Cloudy layer                                                               RRTM_RTRN.218
         ICLDDN = 1                                                              RRTM_RTRN.219
         IENT = NGPT * (LEV-1)                                                   RRTM_RTRN.220
         DO 2000 IPR = 1, NGPT                                                   RRTM_RTRN.221
            INDEX = IENT + IPR                                                   RRTM_RTRN.222
c     Get lookup table index                                                     RRTM_RTRN.223
            IND = ITR(IPR,LEV)                                                   RRTM_RTRN.224
c     Add clear sky and cloud optical depths                                     RRTM_RTRN.225
            ODSM = ODCLR(IPR,LEV) + ODCLD(LEV)                                   RRTM_RTRN.226
            FACTOT = ODSM / (BPADE + ODSM)                                       RRTM_RTRN.227
            BGLAY = PF(IPR,LEV) * PLAY(NGB(IPR),LEV)                             RRTM_RTRN.228
            DELBGUP = BGLEV(IPR) - BGLAY                                         RRTM_RTRN.229
c     Get TF from lookup table                                                   RRTM_RTRN.230
            TAUF = TF(IND)                                                       RRTM_RTRN.231
            BBU(INDEX) = BGLAY + TAUF * DELBGUP                                  RRTM_RTRN.232
            BBUTOT(INDEX) = BGLAY + FACTOT * DELBGUP                             RRTM_RTRN.233
            BGLEV(IPR) = PF(IPR,LEV) * PLVL(NGB(IPR),LEV-1)                      RRTM_RTRN.234
            DELBGDN = BGLEV(IPR) - BGLAY                                         RRTM_RTRN.235
            BBD = BGLAY + TAUF * DELBGDN                                         RRTM_RTRN.236
            BBDLEVD = BGLAY + FACTOT * DELBGDN                                   RRTM_RTRN.237
c     Get clear sky transmittance from lookup table                              RRTM_RTRN.238
            ABSS(INDEX) = 1. - TRANS(IND)                                        RRTM_RTRN.239
            ATOT(INDEX) = ABSS(INDEX) + ABSCLD(LEV) -                            RRTM_RTRN.240
     &          ABSS(INDEX) * ABSCLD(LEV)                                        RRTM_RTRN.241
            GASSRC = BBD * ABSS(INDEX)                                           RRTM_RTRN.242
c     Total sky radiance                                                         RRTM_RTRN.243
            RADLD(IPR) = RADLD(IPR) - RADLD(IPR) * (ABSS(INDEX) +                RRTM_RTRN.244
     &         EFCLFRAC(LEV) * (1.-ABSS(INDEX))) + GASSRC +                      RRTM_RTRN.245
     &         CLDFRAC(LEV) * (BBDLEVD * ATOT(INDEX) - GASSRC)                   RRTM_RTRN.246
            DRAD = DRAD + RADLD(IPR)                                             RRTM_RTRN.247
c     Clear sky radiance                                                         RRTM_RTRN.248
            RADCLRD(IPR) = RADCLRD(IPR) + (BBD - RADCLRD(IPR))                   RRTM_RTRN.249
     &                   * ABSS(INDEX)                                           RRTM_RTRN.250
            CLRDRAD = CLRDRAD + RADCLRD(IPR)                                     RRTM_RTRN.251
 2000    CONTINUE                                                                RRTM_RTRN.252
                                                                                 RRTM_RTRN.253
         ELSE                                                                    RRTM_RTRN.254
                                                                                 RRTM_RTRN.255
C *** Clear layer                                                                RRTM_RTRN.256
         IENT = NGPT * (LEV-1)                                                   RRTM_RTRN.257
         DO 2100 IPR = 1, NGPT                                                   RRTM_RTRN.258
            INDEX = IENT + IPR                                                   RRTM_RTRN.259
            IND = ITR(IPR,LEV)                                                   RRTM_RTRN.260
            BGLAY = PF(IPR,LEV) * PLAY(NGB(IPR),LEV)                             RRTM_RTRN.261
            DELBGUP = BGLEV(IPR) - BGLAY                                         RRTM_RTRN.262
C     Get TF from lookup table                                                   RRTM_RTRN.263
            TAUF = TF(IND)                                                       RRTM_RTRN.264
            BBU(INDEX) = BGLAY + TAUF * DELBGUP                                  RRTM_RTRN.265
            BGLEV(IPR) = PF(IPR,LEV) * PLVL(NGB(IPR),LEV-1)                      RRTM_RTRN.266
            DELBGDN = BGLEV(IPR) - BGLAY                                         RRTM_RTRN.267
            BBD = BGLAY + TAUF * DELBGDN                                         RRTM_RTRN.268
c     Get clear sky transmittance from lookup table                              RRTM_RTRN.269
            ABSS(INDEX) = 1. - TRANS(IND)                                        RRTM_RTRN.270
c     Total sky radiance                                                         RRTM_RTRN.271
            RADLD(IPR) = RADLD(IPR) + (BBD - RADLD(IPR)) *                       RRTM_RTRN.272
     &                   ABSS(INDEX)                                             RRTM_RTRN.273
            DRAD = DRAD + RADLD(IPR)                                             RRTM_RTRN.274
 2100    CONTINUE                                                                RRTM_RTRN.275
C     Set clear sky stream to total sky stream as long as layers                 RRTM_RTRN.276
C     remain clear.  Streams diverge when a cloud is reached.                    RRTM_RTRN.277
            IF (ICLDDN.EQ.1) THEN                                                RRTM_RTRN.278
         DO 2200 IPR = 1, NGPT                                                   RRTM_RTRN.279
               RADCLRD(IPR) = RADCLRD(IPR) + (BBD - RADCLRD(IPR)) *              RRTM_RTRN.280
     &                        ABSS(INDEX)                                        RRTM_RTRN.281
               CLRDRAD = CLRDRAD + RADCLRD(IPR)                                  RRTM_RTRN.282
 2200    CONTINUE                                                                RRTM_RTRN.283
            ELSE                                                                 RRTM_RTRN.284
         DO 2300 IPR = 1, NGPT                                                   RRTM_RTRN.285
               RADCLRD(IPR) = RADLD(IPR)                                         RRTM_RTRN.286
               CLRDRAD = DRAD                                                    RRTM_RTRN.287
 2300    CONTINUE                                                                RRTM_RTRN.288
            ENDIF                                                                RRTM_RTRN.289
                                                                                 RRTM_RTRN.290
C 2100    CONTINUE                                                               RRTM_RTRN.291
                                                                                 RRTM_RTRN.292
         ENDIF                                                                   RRTM_RTRN.293
                                                                                 RRTM_RTRN.294
         TOTDFLUX(LEV-1) = DRAD * WTNUM                                          RRTM_RTRN.295
         TOTDCLFL(LEV-1) = CLRDRAD * WTNUM                                       RRTM_RTRN.296
                                                                                 RRTM_RTRN.297
 3000 CONTINUE                                                                   RRTM_RTRN.298
                                                                                 RRTM_RTRN.299
                                                                                 RRTM_RTRN.300
C SPECTRAL EMISSIVITY & REFLECTANCE                                              RRTM_RTRN.301
C Include the contribution of spectrally varying longwave emissivity and         RRTM_RTRN.302
C reflection from the surface to the upward radiative transfer.                  RRTM_RTRN.303
C Note: Spectral and Lambertian reflection are identical for the one angle       RRTM_RTRN.304
C flux integration used here.                                                    RRTM_RTRN.305
                                                                                 RRTM_RTRN.306
      URAD = 0.0                                                                 RRTM_RTRN.307
      CLRURAD = 0.0                                                              RRTM_RTRN.308
      DO 3500 IPR = 1, NGPT                                                      RRTM_RTRN.309
C     Total sky radiance                                                         RRTM_RTRN.310
         RADLU(IPR) = RADUEMIT(IPR) + (1. - SEMIS(IPR)) * RADLD(IPR)             RRTM_RTRN.311
         URAD = URAD + RADLU(IPR)                                                RRTM_RTRN.312
C     Clear sky radiance                                                         RRTM_RTRN.313
         RADCLRU(IPR) = RADUEMIT(IPR) + (1. - SEMIS(IPR))                        RRTM_RTRN.314
     &                  * RADCLRD(IPR)                                           RRTM_RTRN.315
         CLRURAD = CLRURAD + RADCLRU(IPR)                                        RRTM_RTRN.316
 3500 CONTINUE                                                                   RRTM_RTRN.317
      TOTUFLUX(0) = URAD * WTNUM                                                 RRTM_RTRN.318
      TOTUCLFL(0) = CLRURAD * WTNUM                                              RRTM_RTRN.319
                                                                                 RRTM_RTRN.320
                                                                                 RRTM_RTRN.321
C *** UPWARD RADIATIVE TRANSFER                                                  RRTM_RTRN.322
C *** URAD holds the summed radiance for total sky stream                        RRTM_RTRN.323
C *** CLRURAD holds the summed radiance for clear sky stream                     RRTM_RTRN.324
                                                                                 RRTM_RTRN.325
      DO 5000 LEV = 1, NLAYERS                                                   RRTM_RTRN.326
         URAD = 0.0                                                              RRTM_RTRN.327
         CLRURAD = 0.0                                                           RRTM_RTRN.328
                                                                                 RRTM_RTRN.329
C Check flag for cloud in current layer                                          RRTM_RTRN.330
                                                                                 RRTM_RTRN.331
         IF (ICLDLYR(LEV).EQ.1) THEN                                             RRTM_RTRN.332
                                                                                 RRTM_RTRN.333
C *** Cloudy layers                                                              RRTM_RTRN.334
         IENT = NGPT * (LEV-1)                                                   RRTM_RTRN.335
         DO 4000 IPR = 1, NGPT                                                   RRTM_RTRN.336
            INDEX = IENT + IPR                                                   RRTM_RTRN.337
            GASSRC = BBU(INDEX) * ABSS(INDEX)                                    RRTM_RTRN.338
C     Total sky radiance                                                         RRTM_RTRN.339
            RADLU(IPR) = RADLU(IPR) - RADLU(IPR) * (ABSS(INDEX) +                RRTM_RTRN.340
     &         EFCLFRAC(LEV) * (1.-ABSS(INDEX))) + GASSRC +                      RRTM_RTRN.341
     &         CLDFRAC(LEV) * (BBUTOT(INDEX) * ATOT(INDEX) - GASSRC)             RRTM_RTRN.342
            URAD = URAD + RADLU(IPR)                                             RRTM_RTRN.343
C     Clear sky radiance                                                         RRTM_RTRN.344
            RADCLRU(IPR) = RADCLRU(IPR) + (BBU(INDEX) - RADCLRU(IPR)) *          RRTM_RTRN.345
     &                     ABSS(INDEX)                                           RRTM_RTRN.346
            CLRURAD = CLRURAD + RADCLRU(IPR)                                     RRTM_RTRN.347
 4000    CONTINUE                                                                RRTM_RTRN.348
                                                                                 RRTM_RTRN.349
         ELSE                                                                    RRTM_RTRN.350
                                                                                 RRTM_RTRN.351
C *** Clear layer                                                                RRTM_RTRN.352
         IENT = NGPT * (LEV-1)                                                   RRTM_RTRN.353
         DO 4100 IPR = 1, NGPT                                                   RRTM_RTRN.354
            INDEX = IENT + IPR                                                   RRTM_RTRN.355
C     Total sky radiance                                                         RRTM_RTRN.356
            RADLU(IPR) = RADLU(IPR) + (BBU(INDEX)-RADLU(IPR)) *                  RRTM_RTRN.357
     &                   ABSS(INDEX)                                             RRTM_RTRN.358
            URAD = URAD + RADLU(IPR)                                             RRTM_RTRN.359
C     Clear sky radiance                                                         RRTM_RTRN.360
C     Upward clear and total sky streams must remain separate because surface    RRTM_RTRN.361
C     reflectance is different for each.                                         RRTM_RTRN.362
            RADCLRU(IPR) = RADCLRU(IPR) + (BBU(INDEX) - RADCLRU(IPR))            RRTM_RTRN.363
     &                     * ABSS(INDEX)                                         RRTM_RTRN.364
            CLRURAD = CLRURAD + RADCLRU(IPR)                                     RRTM_RTRN.365
 4100    CONTINUE                                                                RRTM_RTRN.366
                                                                                 RRTM_RTRN.367
         ENDIF                                                                   RRTM_RTRN.368
                                                                                 RRTM_RTRN.369
         TOTUFLUX(LEV) = URAD * WTNUM                                            RRTM_RTRN.370
         TOTUCLFL(LEV) = CLRURAD * WTNUM                                         RRTM_RTRN.371
                                                                                 RRTM_RTRN.372
 5000 CONTINUE                                                                   RRTM_RTRN.373
                                                                                 RRTM_RTRN.374
                                                                                 RRTM_RTRN.375
C *** Convert radiances to fluxes and heating rates for total sky.  Calculates   RRTM_RTRN.376
C     clear sky surface and TOA values.  To compute clear sky profiles, uncommen RRTM_RTRN.377
C     relevant lines below.                                                      RRTM_RTRN.378
      TOTUFLUX(0) = TOTUFLUX(0) * FLUXFAC                                        RRTM_RTRN.379
      TOTDFLUX(0) = TOTDFLUX(0) * FLUXFAC                                        RRTM_RTRN.380
      FNET(0) = TOTUFLUX(0) - TOTDFLUX(0)                                        RRTM_RTRN.381
      TOTUCLFL(0) = TOTUCLFL(0) * FLUXFAC                                        RRTM_RTRN.382
      TOTDCLFL(0) = TOTDCLFL(0) * FLUXFAC                                        RRTM_RTRN.383
      FNETC(0) = TOTUCLFL(0) - TOTDCLFL(0)                                       RRTM_RTRN.384
      CLRNTTOA = TOTUCLFL(NLAYERS)                                               RRTM_RTRN.385
      CLRNTSRF = TOTUFLUX(0) - TOTDCLFL(0)                                       RRTM_RTRN.386
                                                                                 RRTM_RTRN.387
      DO 7000 LEV = 1, NLAYERS                                                   RRTM_RTRN.388
         TOTUFLUX(LEV) = TOTUFLUX(LEV) * FLUXFAC                                 RRTM_RTRN.389
         TOTDFLUX(LEV) = TOTDFLUX(LEV) * FLUXFAC                                 RRTM_RTRN.390
         FNET(LEV) = TOTUFLUX(LEV) - TOTDFLUX(LEV)                               RRTM_RTRN.391
         TOTUCLFL(LEV) = TOTUCLFL(LEV) * FLUXFAC                                 RRTM_RTRN.392
         TOTDCLFL(LEV) = TOTDCLFL(LEV) * FLUXFAC                                 RRTM_RTRN.393
         FNETC(LEV) = TOTUCLFL(LEV) - TOTDCLFL(LEV)                              RRTM_RTRN.394
         L = LEV - 1                                                             RRTM_RTRN.395
C     Calculate Heating Rates.                                                   RRTM_RTRN.396
         HTR(L) = HEATFAC * (FNET(L) - FNET(LEV)) / (PZ(L) - PZ(LEV))            RRTM_RTRN.397
         HTRC(L) = HEATFAC * (FNETC(L) - FNETC(LEV)) / (PZ(L) - PZ(LEV))         RRTM_RTRN.398
 7000 CONTINUE                                                                   RRTM_RTRN.399
      HTR(NLAYERS) = 0.0                                                         RRTM_RTRN.400
      HTRC(NLAYERS) = 0.0                                                        RRTM_RTRN.401
                                                                                 RRTM_RTRN.402
      RETURN                                                                     RRTM_RTRN.403
      END                                                                        RRTM_RTRN.404
