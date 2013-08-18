C****************************************************************************    RRTM.1
C*                                                                          *    RRTM.2
C*                               RRTM                                       *    RRTM.3
C*                                                                          *    RRTM.4
C*                                                                          *    RRTM.5
C*                                                                          *    RRTM.6
C*                   RAPID RADIATIVE TRANSFER MODEL                         *    RRTM.7
C*                                                                          *    RRTM.8
C*                                                                          *    RRTM.9
C*            ATMOSPHERIC AND ENVIRONMENTAL RESEARCH, INC.                  *    RRTM.10
C*                        840 MEMORIAL DRIVE                                *    RRTM.11
C*                        CAMBRIDGE, MA 02139                               *    RRTM.12
C*                                                                          *    RRTM.13
C*                                                                          *    RRTM.14
C*                           ELI J. MLAWER                                  *    RRTM.15
C*                         STEVEN J. TAUBMAN~                               *    RRTM.16
C*                         SHEPARD A. CLOUGH                                *    RRTM.17
C*                                                                          *    RRTM.18
C*                                                                          *    RRTM.19
C*                         ~currently at GFDL                               *    RRTM.20
C*                                                                          *    RRTM.21
C*                                                                          *    RRTM.22
C*                                                                          *    RRTM.23
C*                       email:  mlawer@aer.com                             *    RRTM.24
C*                                                                          *    RRTM.25
C*        The authors wish to acknowledge the contributions of the          *    RRTM.26
C*        following people:  Patrick D. Brown, Michael J. Iacono,           *    RRTM.27
C*        Ronald E. Farren, Luke Chen, Robert Bergstrom.                    *    RRTM.28
C*                                                                          *    RRTM.29
C****************************************************************************    RRTM.30
                                                                                 RRTM.31
C *** This version of RRTM has been altered to interface with the                RRTM.32
C *** NCAR MM5 mesoscale model for the calculation of longwave radiative         RRTM.33
C *** transfer (based on a code for interface with CCM model by M. J. Iacono)    RRTM.34
C *** J. Dudhia ; March, 1999                                                    RRTM.35
                                                                                 RRTM.36
      SUBROUTINE RRTM(J,ILX,CLDFRA,INEST,GLW,OLW,RTTEN,T3D,QV3D,QC3D             07NOV00.2194
     +           ,QR3D,QI3D,QNI3D,QG3D,PP3D,PSA,TGA,EMISS)                       07NOV00.2195
                                                                                 RRTM.39
C *** This program is the driver for RRTM, the AER LW radiation model.           RRTM.40
C     This routine:                                                              RRTM.41
C     Calls MM5ATM to provide atmosphere in column and boundary values           RRTM.42
C     a) calls GASABS to calculate gaseous optical depths                        RRTM.43
C     b) calls SETCOEF to calculate various quantities needed for                RRTM.44
C        the radiative transfer algorithm                                        RRTM.45
C     c) calls RTRN (for both clear and cloudy columns) to do the                RRTM.46
C        radiative transfer calculation                                          RRTM.47
C     d) passes the necessary flux and cooling rate back to MM5                  RRTM.48
                                                                                 RRTM.49
#include <parame.incl>                                                           RRTM.50
#ifndef MPP1                                                                     RRTM.51
      DIMENSION CLDFRA(MIX-1,MKX), GLW(MIX,MJX), OLW(MIX,MJX),                   07NOV00.2196
     &           RTTEN(MIX,MJX,MKX)                                              07NOV00.2197
      DIMENSION O3PROF(MIX-1,KXP1)                                               RRTM.53
     &           ,T3D(MIX,MJX,MKX),                                              RRTM.54
     &           QV3D(MIX,MJX,MKX), QC3D(MIXM,MJXM,MKXM),                        RRTM.55
     &           PP3D(MIX,MJX,MKX), QR3D(MIXM,MJXM,MKXM),                        RRTM.56
     &           QI3D(MIXIC,MJXIC,MKXIC),QNI3D(MIXIC,MJXIC,MKXIC),               RRTM.57
     &           QG3D(MIXIG,MJXIG,MKXIG),                                        RRTM.58
     &           PSA(MIX,MJX),TGA(MIX,MJX),EMISS(MIX,MJX)                        RRTM.59
#else                                                                            RRTM.60
      DIMENSION CLDFRA(MIX,MKX), GLW(MIX,MJX), OLW(MIX,MJX),                     07NOV00.2198
     &           RTTEN(MIX,MJX,MKX)                                              07NOV00.2199
      DIMENSION O3PROF(MIX,KXP1)                                                 RRTM.62
     &           ,T3D(MIX,MJX,MKX),                                              RRTM.63
     &           QV3D(MIX,MJX,MKX), QC3D(MIXM,MJXM,MKXM),                        RRTM.64
     &           PP3D(MIX,MJX,MKX), QR3D(MIXM,MJXM,MKXM),                        RRTM.65
     &           QI3D(MIXIC,MJXIC,MKXIC),QNI3D(MIXIC,MJXIC,MKXIC),               RRTM.66
     &           QG3D(MIXIG,MJXIG,MKXIG),                                        RRTM.67
     &           PSA(MIX,MJX),TGA(MIX,MJX),EMISS(MIX,MJX)                        RRTM.68
#endif                                                                           RRTM.69
C Parameters                                                                     RRTM.70
      PARAMETER (MXLAY=203)                                                      RRTM.71
      PARAMETER (NGPT=140)                                                       RRTM.72
      PARAMETER (NBANDS=16)                                                      RRTM.73
                                                                                 RRTM.74
C Input                                                                          RRTM.75
      COMMON /CONSTANTS/ PI,FLUXFAC,HEATFAC                                      RRTM.76
      COMMON /FEATURES/  NG(NBANDS),NSPA(NBANDS),NSPB(NBANDS)                    RRTM.77
      COMMON /PRECISE/   ONEMINUS                                                RRTM.78
      COMMON /BANDS/     WAVENUM1(NBANDS),WAVENUM2(NBANDS),                      RRTM.79
     &                   DELWAVE(NBANDS)                                         RRTM.80
      COMMON /PROFILE/   NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),                      RRTM.81
     &                   PZ(0:MXLAY),TZ(0:MXLAY)                                 RRTM.82
      COMMON /CLOUDDAT/  CLDFRAC(MXLAY),TAUCLOUD(MXLAY)                          RRTM.83
      COMMON /TBLINDX/   ITR(NGPT,MXLAY)                                         RRTM.84
      COMMON /SP/        PFRAC(NGPT,MXLAY)                                       RRTM.85
      COMMON /SURFACE/   TBOUND,IREFLECT,SEMISS(NBANDS)                          RRTM.86
C Output                                                                         RRTM.87
      COMMON /CLDFLG/    ICLDLYR(MXLAY),ICLD                                     RRTM.88
      COMMON /OUTRRTM/    TOTUFLUX(0:MXLAY), TOTDFLUX(0:MXLAY),                  RRTM.89
     &                   FNET(0:MXLAY), HTR(0:MXLAY)                             RRTM.90
      COMMON /OUTCLR/    TOTUCLFL(0:MXLAY), TOTDCLFL(0:MXLAY),                   RRTM.91
     &                   FNETC(0:MXLAY), HTRC(0:MXLAY)                           RRTM.92
      COMMON /CLRFLX/    CLRNTTOA,CLRNTSRF                                       RRTM.93
                                                                                 RRTM.94
C RRTM Definitions                                                               RRTM.95
C    NGPT                         ! Total number of g-point subintervals         RRTM.96
C    MXLAY                        ! Maximum number of model layers               RRTM.97
C    NBANDS                       ! Number of longwave spectral bands            RRTM.98
C    PI                           ! Geometric constant                           RRTM.99
C    FLUXFAC                      ! Radiance to flux conversion factor           RRTM.100
C    HEATFAC                      ! Heating rate conversion factor               RRTM.101
C    NG(NBANDS)                   ! Number of g-points per band for input        RRTM.102
C                                   absorption coefficient data                  RRTM.103
C    NSPA(NBANDS),NSPB(NBANDS)    ! Number of reference atmospheres per band     RRTM.104
C    WAVENUM1(NBANDS)             ! Longwave band lower limit (wavenumbers)      RRTM.105
C    WAVENUM2(NBANDS)             ! Longwave band upper limit (wavenumbers)      RRTM.106
C    DELWAVE                      ! Longwave band width (wavenumbers)            RRTM.107
C    NLAYERS                      ! Number of model layers (mkx+1)               RRTM.108
C    PAVEL(MXLAY)                 ! Layer pressures (mb)                         RRTM.109
C    PZ(0:MXLAY)                  ! Level (interface) pressures (mb)             RRTM.110
C    TAVEL(MXLAY)                 ! Layer temperatures (K)                       RRTM.111
C    TZ(0:MXLAY)                  ! Level (interface) temperatures(mb)           RRTM.112
C    TBOUND                       ! Surface temperature (K)                      RRTM.113
C    CLDFRAC(MXLAY)               ! Layer cloud fraction                         RRTM.114
C    TAUCLOUD(MXLAY)              ! Layer cloud optical depth                    RRTM.115
C    ITR(NGPT,MXLAY)              ! Integer look-up table index                  RRTM.116
C    PFRAC(NGPT,MXLAY)            ! Planck fractions                             RRTM.117
C    ICLDLYR(MXLAY)               ! Flag for cloudy layers                       RRTM.118
C    TOTUFLUX(0:MXLAY)            ! Upward longwave flux (W/m2)                  RRTM.119
C    TOTDFLUX(0:MXLAY)            ! Downward longwave flux (W/m2)                RRTM.120
C    FNET(0:MXLAY)                ! Net longwave flux (W/m2)                     RRTM.121
C    HTR(0:MXLAY)                 ! Longwave heating rate (K/day)                RRTM.122
C    CLRNTTOA                     ! Clear-sky TOA outgoing flux (W/m2)           RRTM.123
C    CLRNTSRF                     ! Clear-sky net surface flux (W/m2)            RRTM.124
C    TOTUCLFL(0:MXLAY)            ! Clear-sky upward longwave flux (W/m2)        RRTM.125
C    TOTDCLFL(0:MXLAY)            ! Clear-sky downward longwave flux (W/m2)      RRTM.126
C    FNETC(0:MXLAY)               ! Clear-sky net longwave flux (W/m2)           RRTM.127
C    HTRC(0:MXLAY)                ! Clear-sky longwave heating rate (K/day)      RRTM.128
C                                                                                RRTM.129
C This compiler directive was added to insure private common block storage       RRTM.130
C in multi-tasked mode on a CRAY or SGI for all commons except those that        RRTM.131
C carry constants.                                                               RRTM.132
C SGI                                                                            RRTM.136
C$OMP THREADPRIVATE (/PROFILE/,/OUTRRTM/,/CLOUDDAT/,/TBLINDX/)                   RRTM.137
C$OMP THREADPRIVATE (/SP/,/CLRFLX/,/CLDFLG/)                                     RRTM.138
C$OMP THREADPRIVATE (/OUTCLR/, /SURFACE/)                                        RRTM.139
C ***                                                                            RRTM.140
                                                                                 RRTM.141
C  Longwave spectral band data                                                   RRTM.142
      DATA WAVENUM1(1) /10./, WAVENUM2(1) /250./, DELWAVE(1) /240./              RRTM.143
      DATA WAVENUM1(2) /250./, WAVENUM2(2) /500./, DELWAVE(2) /250./             RRTM.144
      DATA WAVENUM1(3) /500./, WAVENUM2(3) /630./, DELWAVE(3) /130./             RRTM.145
      DATA WAVENUM1(4) /630./, WAVENUM2(4) /700./, DELWAVE(4) /70./              RRTM.146
      DATA WAVENUM1(5) /700./, WAVENUM2(5) /820./, DELWAVE(5) /120./             RRTM.147
      DATA WAVENUM1(6) /820./, WAVENUM2(6) /980./, DELWAVE(6) /160./             RRTM.148
      DATA WAVENUM1(7) /980./, WAVENUM2(7) /1080./, DELWAVE(7) /100./            RRTM.149
      DATA WAVENUM1(8) /1080./, WAVENUM2(8) /1180./, DELWAVE(8) /100./           RRTM.150
      DATA WAVENUM1(9) /1180./, WAVENUM2(9) /1390./, DELWAVE(9) /210./           RRTM.151
      DATA WAVENUM1(10) /1390./,WAVENUM2(10) /1480./,DELWAVE(10) /90./           RRTM.152
      DATA WAVENUM1(11) /1480./,WAVENUM2(11) /1800./,DELWAVE(11) /320./          RRTM.153
      DATA WAVENUM1(12) /1800./,WAVENUM2(12) /2080./,DELWAVE(12) /280./          RRTM.154
      DATA WAVENUM1(13) /2080./,WAVENUM2(13) /2250./,DELWAVE(13) /170./          RRTM.155
      DATA WAVENUM1(14) /2250./,WAVENUM2(14) /2380./,DELWAVE(14) /130./          RRTM.156
      DATA WAVENUM1(15) /2380./,WAVENUM2(15) /2600./,DELWAVE(15) /220./          RRTM.157
      DATA WAVENUM1(16) /2600./,WAVENUM2(16) /3000./,DELWAVE(16) /400./          RRTM.158
                                                                                 RRTM.159
      DATA NG  /16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16/                 RRTM.160
      DATA NSPA /1, 1,10, 9, 9, 1, 9, 1,11, 1, 1, 9, 9, 1, 9, 9/                 RRTM.161
      DATA NSPB /1, 1, 5, 6, 5, 0, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0/                 RRTM.162
                                                                                 RRTM.163
C     HEATFAC is the factor by which one must multiply delta-flux/               RRTM.164
C     delta-pressure, with flux in w/m-2 and pressure in mbar, to get            RRTM.165
C     the heating rate in units of degrees/day.  It is equal to                  RRTM.166
C           (g)x(#sec/day)x(1e-5)/(specific heat of air at const. p)             RRTM.167
C        =  (9.8066)(3600)(1e-5)/(1.004)                                         RRTM.168
      DATA HEATFAC /8.4391/                                                      RRTM.169
                                                                                 RRTM.170
      ONEMINUS = 1. - 1.E-6                                                      RRTM.171
      PI = 2.*ASIN(1.)                                                           RRTM.172
      FLUXFAC = PI * 2.D4                                                        RRTM.173
                                                                                 RRTM.174
      CALL INIRAD (J, O3PROF)                                                    RRTM.175
C  This is the main MM5 I loop within RRTM.                                      RRTM.176
      do 4000 i = 1, ILX                                                         RRTM.177
                                                                                 RRTM.178
C  Prepare atmospheric profile from CCM for use in RRTM, and define              RRTM.179
C  other RRTM input parameters.  Arrays are passed back through the              RRTM.180
C  existing RRTM commons and arrays.                                             RRTM.181
                                                                                 RRTM.182
C        call ccmatm(pmid,pint,tnm,tint,qnm,o3vmr,ch4mmr,                        RRTM.183
C    1               n2ommr,c11mmr,c12mmr,cldfrc,clwp,kabsav)                    RRTM.184
C                                                                                RRTM.185
         CALL MM5ATM(I,J,CLDFRA,O3PROF,INEST,T3D,QV3D,QC3D,QR3D                  RRTM.186
     +           ,QI3D,QNI3D,QG3D,PP3D,PSA,TGA,EMISS)                            RRTM.187
                                                                                 RRTM.188
C  Calculate information needed by the radiative transfer routine                RRTM.189
C  that is specific to this atmosphere, especially some of the                   RRTM.190
C  coefficients and indices needed to compute the optical depths                 RRTM.191
C  by interpolating data from stored reference atmospheres.                      RRTM.192
                                                                                 RRTM.193
         CALL SETCOEF                                                            RRTM.194
                                                                                 RRTM.195
         CALL GASABS                                                             RRTM.196
                                                                                 RRTM.197
C  Check for cloud in column.  Use original CCM LW threshold: if total           RRTM.198
C  clear sky fraction < 0.999, then column is cloudy, otherwise consider         RRTM.199
C  it clear.  Also, set up flag array, icldlyr, for use in radiative             RRTM.200
C  transfer.  Set icldlyr to one for each layer with cloud.  If tclrsf           RRTM.201
C  is not available, icldlyr can be set from cldfrac alone.                      RRTM.202
                                                                                 RRTM.203
c       if (tclrsf(kxp1).lt.0.999) then                                          RRTM.204
c          icld = 1                                                              RRTM.205
c       else                                                                     RRTM.206
c          icld = 0                                                              RRTM.207
c       endif                                                                    RRTM.208
        do 1500 k = 1, nlayers                                                   RRTM.209
c          if (icld.eq.1.and.cldfrac(k).gt.0.) then                              RRTM.210
           if (cldfrac(k).gt.0.) then                                            RRTM.211
              icldlyr(k) = 1                                                     RRTM.212
           else                                                                  RRTM.213
              icldlyr(k) = 0                                                     RRTM.214
           endif                                                                 RRTM.215
 1500   continue                                                                 RRTM.216
                                                                                 RRTM.217
C  Call the radiative transfer routine.                                          RRTM.218
                                                                                 RRTM.219
           CALL RTRN                                                             RRTM.220
                                                                                 RRTM.221
C  Pass total sky up and down flux profiles to CCM output arrays and             RRTM.222
C  convert from mks to cgs units for CCM.  Pass clear sky TOA and surface        RRTM.223
C  net fluxes to CCM fields for diagnostics.  Pass total sky heating rate        RRTM.224
C  profile to CCM output arrays and convert units to K/sec.  The vertical        RRTM.225
C  array index (bottom to top in RRTM) is reversed for CCM fields.               RRTM.226
                                                                                 RRTM.227
C          flntc(iiplon) = CLRNTTOA*1.e3                                         RRTM.228
C          flnsc(iiplon) = CLRNTSRF*1.e3                                         RRTM.229
C           do 2400 k = 0, NLAYERS-1                                             RRTM.230
C              fulc(k+1) = TOTUCLFL(NLAYERS-1-k)*1.e3                            RRTM.231
C              fdlc(k+1) = TOTDCLFL(NLAYERS-1-k)*1.e3                            RRTM.232
C              ful(k+1) = TOTUFLUX(NLAYERS-1-k)*1.e3                             RRTM.233
C              fdl(k+1) = TOTDFLUX(NLAYERS-1-k)*1.e3                             RRTM.234
C 2400      continue                                                             RRTM.235
           do 2450 k = 1, NLAYERS-1                                              RRTM.236
C              qrlc(k) = HTRC(NLAYERS-1-k)/86400.                                RRTM.237
C              qrl(k) = HTR(NLAYERS-1-k)/86400.                                  RRTM.238
c             RTTEN(I,J,K)=(CLDFRAC(NLAYERS-1-k)*HTRC(NLAYERS-1-k)+              RRTM.239
c    -            (1.-CLDFRAC(NLAYERS-1-k))*HTR(NLAYERS-1-k))/86400.             RRTM.240
              RTTEN(I,J,K)=HTR(NLAYERS-1-k)/86400.                               RRTM.241
 2450      continue                                                              RRTM.242
           GLW(I,J) = TOTDFLUX(0)                                                RRTM.243
           OLW(I,J) = TOTUFLUX(NLAYERS)                                          07NOV00.2200
 4000 CONTINUE                                                                   RRTM.244
                                                                                 RRTM.245
      RETURN                                                                     RRTM.246
      END                                                                        RRTM.247
                                                                                 RRTM.248
C***************************************************************************     RRTM.249
