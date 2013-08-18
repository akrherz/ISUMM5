      SUBROUTINE DIFFMOI(KZZ,FTEN,F,PSB,XK,C203,IBGN,IEND,                       DIFFMOI.1
     & JBGN,JEND,KL,INEST,fin,fjn,chwgt,kmv,                                     DIFFMOI.2
     & kmi,kmj,gezip,gezim,gezjp,gezjm,                                          DIFFMOI.3
     & mrkip1,mrkip2,mrkim1,mrkim2,mrkjp1,mrkjp2,mrkjm1,mrkjm2,                  DIFFMOI.4
     & irkip1,irkip2,irkim1,irkim2,irkjp1,irkjp2,irkjm1,irkjm2,                  DIFFMOI.5
     & fion,fiun,fjon,fjun,dz3d,dx,zdiffst)                                      DIFFMOI.6
      IMPLICIT NONE                                                              DIFFMOI.7
c                                                                                DIFFMOI.8
c  Subroutine for moisture diffusion (QV and CLW)                                DIFFMOI.9
c  See ZÄNGL (2002, MWR 130, 1423-32 for description)                            DIFFMOI.10
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          DIFFMOI.11
C                                                                     C          DIFFMOI.12
C                                                                     C          DIFFMOI.13
C     THIS SUBROUTINE COMPUTES THE DIFFUSION TERM FOR DECOUPLED       C          DIFFMOI.14
C     VARIABLE ON CONSTANT SIGMA SURFACE.                             C          DIFFMOI.15
C                                                                     C          DIFFMOI.16
C                                                                     C          DIFFMOI.17
C     IYY, JXX, KZZ : DIMENSIONS                                      C          DIFFMOI.18
C                                                                     C          DIFFMOI.19
C     FTEN    : TENDENCY FOR VARIABLE FB                              C          DIFFMOI.20
C                                                                     C          DIFFMOI.21
C     F       : DECOUPLED VARIABLE AT TIME T-1                        C          DIFFMOI.22
C     PSB     : P* AT THE POINTS WHERE FB IS DEFINED                  C          DIFFMOI.23
C                                                                     C          DIFFMOI.24
C     XK      : HORIZONTAL DIFFUSION COEFFICIENT                      C          DIFFMOI.25
C                                                                     C          DIFFMOI.26
C     C203    : 1./(DX*DX), DEFINED IN 'PARAM'                        C          DIFFMOI.27
C                                                                     C          DIFFMOI.28
C     IEND    : = ILXM FOR CROSS-POINT VARIABLES                      C          DIFFMOI.29
C               = ILX  FOR DOT-POINT   VARIABLES                      C          DIFFMOI.30
C                                                                     C          DIFFMOI.31
C     JEND    : = JLXM FOR CROSS-POINT VARIABLES                      C          DIFFMOI.32
C               = JLX  FOR DOT-POINT   VARIABLES                      C          DIFFMOI.33
C                                                                     C          DIFFMOI.34
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          DIFFMOI.35
#       include <parame.incl>                                                    DIFFMOI.36
C                                                                                DIFFMOI.37
      INTEGER   KZZ,IBGN,IEND,JBGN,JEND,KL,INEST                                 DIFFMOI.38
      INTEGER   I,J,K,IB,JB,IE,JE,in,kmv,chwgt,kk,zdiffst(maxnes)                DIFFMOI.39
      integer kmi(MIX,MJX),kmj(MIX,MJX)                                          DIFFMOI.40
      REAL  C203,fijmin,XK(MIX,MJX,MKX),ums,dx                                   DIFFMOI.41
      real xxip1(MIX),xxip2(MIX),xxjp1(MIX),xxjp2(MIX),                          DIFFMOI.42
     + xxim1(MIX),xxim2(MIX),xxjm1(MIX),xxjm2(MIX),wip1(MIX),                    DIFFMOI.43
     + wip2(MIX),wjp1(MIX),wjp2(MIX),wim1(MIX),wim2(MIX),                        DIFFMOI.44
     + wjm1(MIX),wjm2(MIX),fip1(MIX),fip2(MIX),fjp1(MIX),                        DIFFMOI.45
     + fjp2(MIX),fim1(MIX),fim2(MIX),fjm1(MIX),fjm2(MIX)                         DIFFMOI.46
      real psb(MIX,MJX),fin(MIX,MJX),fjn(MIX,MJX),                               DIFFMOI.47
     + fion(MIX,MJX),fiun(MIX,MJX),fjon(MIX,MJX),fjun(MIX,MJX)                   DIFFMOI.48
      real ftj(MIX,kzz),fti(MIX,kzz),h1(MIX,kzz),                                DIFFMOI.49
     + h2(MIX,kzz),fitp(MIX,kzz),fjtp(MIX,kzz),fjt(MIX,kzz),                     DIFFMOI.50
     + fit(MIX,kzz)                                                              DIFFMOI.51
      real gesi(MIX,kmv+1:kzz),gesj(MIX,kmv+1:kzz),                              DIFFMOI.52
     + ftip(MIX,kmv+1:kzz),ftim(MIX,kmv+1:kzz),ftjp(MIX,kmv+1:kzz),              DIFFMOI.53
     + ftjm(MIX,kmv+1:kzz),ftil(MIX,kmv+1:kzz),ftih(MIX,kmv+1:kzz),              DIFFMOI.54
     + ftjl(MIX,kmv+1:kzz),ftjh(MIX,kmv+1:kzz),fitm(MIX,kmv+1:kzz),              DIFFMOI.55
     + fjtm(MIX,kmv+1:kzz)                                                       DIFFMOI.56
      real ften(MIX,MJX,KZZ),f(MIX,MJX,KZZ),gezip(MIX,MJX,KZZ),                  DIFFMOI.57
     + gezim(MIX,MJX,KZZ),gezjp(MIX,MJX,KZZ),gezjm(MIX,MJX,KZZ),                 DIFFMOI.58
     + mrkip1(MIX,MJX,KZZ),mrkip2(MIX,MJX,KZZ),mrkim1(MIX,MJX,KZZ),              DIFFMOI.59
     + mrkim2(MIX,MJX,KZZ),mrkjp1(MIX,MJX,KZZ),mrkjp2(MIX,MJX,KZZ),              DIFFMOI.60
     + mrkjm1(MIX,MJX,KZZ),mrkjm2(MIX,MJX,KZZ),dz3d(MIX,MJX,KZZ)                 DIFFMOI.61
      real fl(MIX,MJX,0:KZZ+2),dz3dl(MIX,MJX,0:KZZ+2)                            DIFFMOI.62
      integer irkip1(MIX,MJX,KZZ),irkip2(MIX,MJX,KZZ),                           DIFFMOI.63
     + irkim1(MIX,MJX,KZZ),irkim2(MIX,MJX,KZZ),                                  DIFFMOI.64
     + irkjp1(MIX,MJX,KZZ),irkjp2(MIX,MJX,KZZ),                                  DIFFMOI.65
     + irkjm1(MIX,MJX,KZZ),irkjm2(MIX,MJX,KZZ)                                   DIFFMOI.66
      real mrk,y0,y1,y2,y3,xmx0,xmx1,xmx2,x1mx0,x0mx2,x3mx1,x1mx2,               DIFFMOI.67
     + x3mx0,x3mx2,aa,bb,cc,ymax,ymin                                            DIFFMOI.68
                                                                                 DIFFMOI.69
                                                                                 DIFFMOI.70
                                                                                 DIFFMOI.71
C                                                                                DIFFMOI.72
C----------------------------------------------------------------------          DIFFMOI.73
C                                                                                DIFFMOI.74
C                                                                                DIFFMOI.75
C_FLIC_RUNPAD(2)                                                                 DIFFMOI.76
      IF(IBGN.GT.2)THEN                                                          DIFFMOI.77
        IB=IBGN                                                                  DIFFMOI.78
        JB=JBGN                                                                  DIFFMOI.79
        IE=IEND                                                                  DIFFMOI.80
        JE=JEND                                                                  DIFFMOI.81
      ELSE                                                                       DIFFMOI.82
        IB=IBGN+1                                                                DIFFMOI.83
        JB=JBGN+1                                                                DIFFMOI.84
        IE=IEND-1                                                                DIFFMOI.85
        JE=JEND-1                                                                DIFFMOI.86
      ENDIF                                                                      DIFFMOI.87
C                                                                                DIFFMOI.88
      in = inest                                                                 DIFFMOI.89
                                                                                 DIFFMOI.90
      if (dx.ge.750) then                                                        DIFFMOI.91
        ums = -0.01                                                              DIFFMOI.92
      else                                                                       DIFFMOI.93
        ums = -0.01*dx/750.                                                      DIFFMOI.94
      endif                                                                      DIFFMOI.95
                                                                                 DIFFMOI.96
                                                                                 DIFFMOI.97
c Polynomial interpolation needs larger field dimensions                         DIFFMOI.98
c                                                                                DIFFMOI.99
c$omp parallel do default(shared)                                                DIFFMOI.100
c$omp&private(i,j,k)                                                             DIFFMOI.101
        DO J=1,JE+2                                                              DIFFMOI.102
          do i=1,ie+2                                                            DIFFMOI.103
            do k=1,kl-1                                                          DIFFMOI.104
              fl(i,j,k) = f(i,j,k)                                               DIFFMOI.105
              dz3dl(i,j,k) = dz3d(i,j,k)                                         DIFFMOI.106
            enddo                                                                DIFFMOI.107
              fl(i,j,kl) = f(i,j,kl)                                             DIFFMOI.108
              fl(i,j,kl+1) = f(i,j,kl)                                           DIFFMOI.109
              fl(i,j,kl+2) = f(i,j,kl)                                           DIFFMOI.110
              fl(i,j,0) = f(i,j,1)                                               DIFFMOI.111
              dz3dl(i,j,kl) = dz3d(i,j,kl-1)                                     DIFFMOI.112
              dz3dl(i,j,kl+1) = dz3d(i,j,kl-1)                                   DIFFMOI.113
              dz3dl(i,j,kl+2) = dz3d(i,j,kl-1)                                   DIFFMOI.114
              dz3dl(i,j,0) = dz3d(i,j,1)                                         DIFFMOI.115
          enddo                                                                  DIFFMOI.116
        enddo                                                                    DIFFMOI.117
                                                                                 DIFFMOI.118
c                                                                                DIFFMOI.119
c  start of big j-loop                                                           DIFFMOI.120
c$omp parallel do default(shared)                                                DIFFMOI.121
c$omp&private(i,j,k,gesi,gesj,kk,mrk,y0,y1,y2,y3,xmx0,xmx1,xmx2,                 DIFFMOI.122
c$omp&x1mx0,x0mx2,x3mx1,x1mx2,x3mx0,x3mx2,aa,bb,cc,ymax,ymin,                    DIFFMOI.123
c$omp&fip1,fip2,fjp1,fjp2,fim1,fim2,fjm1,fjm2,h1,h2,fitp,fjtp,                   DIFFMOI.124
c$omp&xxip1,xxip2,xxjp1,xxjp2,xxim1,xxim2,xxjm1,xxjm2,                           DIFFMOI.125
c$omp&wip1,wip2,wim1,wim2,wjp1,wjp2,wjm1,wjm2,fti,ftj,fitm,fjtm,                 DIFFMOI.126
c$omp&ftip,ftim,ftjp,ftjm,ftil,ftjl,ftih,ftjh,fit,fjt)                           DIFFMOI.127
      DO J=JB,JE                                                                 DIFFMOI.128
                                                                                 DIFFMOI.129
c  Weighting coefficients for sigma-diffusion                                    DIFFMOI.130
        do k=kmv+1,kl                                                            DIFFMOI.131
          do  i=ib,ie                                                            DIFFMOI.132
            gesi(i,k) = 1.-0.5*(gezip(i,j,k)+gezim(i,j,k))                       DIFFMOI.133
            gesj(i,k) = 1.-0.5*(gezjp(i,j,k)+gezjm(i,j,k))                       DIFFMOI.134
          enddo                                                                  DIFFMOI.135
        enddo                                                                    DIFFMOI.136
                                                                                 DIFFMOI.137
c  Compute diffusion along the coordinate surfaces where they are flat enough    DIFFMOI.138
      do k=1,zdiffst(inest)-1                                                    DIFFMOI.139
      do I=ib,ie                                                                 DIFFMOI.140
      FTEN(I,J,K)=FTEN(I,J,K)-XK(I,J,K)*C203*PSB(I,J)*(F(I,J+2,K)+               DIFFMOI.141
     +            F(I,J-2,K)+F(I+2,J,K)+F(I-2,J,K)-4.*(F(I,J+1,K)+               DIFFMOI.142
     +            F(I,J-1,K)+F(I+1,J,K)+F(I-1,J,K))+12.*F(I,J,K))                DIFFMOI.143
                                                                                 DIFFMOI.144
      enddo                                                                      DIFFMOI.145
      enddo                                                                      DIFFMOI.146
                                                                                 DIFFMOI.147
                                                                                 DIFFMOI.148
C.....Truly horizontal diffusion where possible without intersecting the topogra DIFFMOI.149
c     at any grid point                                                          DIFFMOI.150
                                                                                 DIFFMOI.151
c     chwgt <= 1:                                                                DIFFMOI.152
c     Linear vertical interpolation. Used for interpolating CLW                  DIFFMOI.153
c                                                                                DIFFMOI.154
c     chwgt >= 2:                                                                DIFFMOI.155
c     Third-order polynomial interpolation. Needed for interpolating QV because  DIFFMOI.156
c     QV decreases exponentially with height, implying that linear interpolation DIFFMOI.157
c     would yield a systematic upward moisture transport over mountains.         DIFFMOI.158
c     Polynomial interpolation replaces the exponential interpolation described  DIFFMOI.159
c     Zaengl (2002) for reasons of better accuracy.                              DIFFMOI.160
C                                                                                DIFFMOI.161
                                                                                 DIFFMOI.162
        DO K=zdiffst(inest),kmv  ! Levels where horizontal computation of diffus DIFFMOI.163
c                       possible without intersecting the topography             DIFFMOI.164
          IF (chwgt.ge.2) then ! Third-order polynomial interpolation            DIFFMOI.165
            DO i=ib,ie                                                           DIFFMOI.166
c  j+2                                                                           DIFFMOI.167
              kk = irkjp2(i,j,k)                                                 DIFFMOI.168
              mrk = mrkjp2(i,j,k)                                                DIFFMOI.169
c                                                                                DIFFMOI.170
              y0 = fl(I,J+2,kk+1)                                                DIFFMOI.171
              y1 = fl(I,J+2,kk)                                                  DIFFMOI.172
              y2 = fl(I,J+2,kk+2)                                                DIFFMOI.173
              y3 = fl(I,J+2,kk-1)                                                DIFFMOI.174
              xmx0 = dz3dl(i,j+2,kk)*(1-mrk)                                     DIFFMOI.175
              xmx1 = -dz3dl(i,j+2,kk)*mrk                                        DIFFMOI.176
              xmx2 = dz3dl(i,j+2,kk+1) + xmx0                                    DIFFMOI.177
              x1mx0 = dz3dl(i,j+2,kk)                                            DIFFMOI.178
              x0mx2 = dz3dl(i,j+2,kk+1)                                          DIFFMOI.179
              x3mx1 = dz3dl(i,j+2,kk-1)                                          DIFFMOI.180
              x1mx2 = x1mx0 + x0mx2                                              DIFFMOI.181
              x3mx0 = x3mx1 + x1mx0                                              DIFFMOI.182
              x3mx2 = x3mx0 + x0mx2                                              DIFFMOI.183
              aa = (y1-y0)/x1mx0                                                 DIFFMOI.184
              bb = (y2-y0+aa*x0mx2)/(x0mx2*x1mx2)                                DIFFMOI.185
              cc = (y3-y0-aa*x3mx0-bb*x3mx0*x3mx1)/(x3mx0*x3mx1*x3mx2)           DIFFMOI.186
              wjp2(i) = y0+aa*xmx0+bb*xmx0*xmx1+cc*xmx0*xmx1*xmx2                DIFFMOI.187
              ymax = max(y0,y1)                                                  DIFFMOI.188
              ymin = min(y0,y1)                                                  DIFFMOI.189
              wjp2(i) = min(wjp2(i),ymax)                                        DIFFMOI.190
              wjp2(i) = max(wjp2(i),ymin)                                        DIFFMOI.191
c  j+1                                                                           DIFFMOI.192
              kk = irkjp1(i,j,k)                                                 DIFFMOI.193
              mrk = mrkjp1(i,j,k)                                                DIFFMOI.194
c                                                                                DIFFMOI.195
              y0 = fl(I,J+1,kk+1)                                                DIFFMOI.196
              y1 = fl(I,J+1,kk)                                                  DIFFMOI.197
              y2 = fl(I,J+1,kk+2)                                                DIFFMOI.198
              y3 = fl(I,J+1,kk-1)                                                DIFFMOI.199
              xmx0 = dz3dl(i,j+1,kk)*(1-mrk)                                     DIFFMOI.200
              xmx1 = -dz3dl(i,j+1,kk)*mrk                                        DIFFMOI.201
              xmx2 = dz3dl(i,j+1,kk+1) + xmx0                                    DIFFMOI.202
              x1mx0 = dz3dl(i,j+1,kk)                                            DIFFMOI.203
              x0mx2 = dz3dl(i,j+1,kk+1)                                          DIFFMOI.204
              x3mx1 = dz3dl(i,j+1,kk-1)                                          DIFFMOI.205
              x1mx2 = x1mx0 + x0mx2                                              DIFFMOI.206
              x3mx0 = x3mx1 + x1mx0                                              DIFFMOI.207
              x3mx2 = x3mx0 + x0mx2                                              DIFFMOI.208
              aa = (y1-y0)/x1mx0                                                 DIFFMOI.209
              bb = (y2-y0+aa*x0mx2)/(x0mx2*x1mx2)                                DIFFMOI.210
              cc = (y3-y0-aa*x3mx0-bb*x3mx0*x3mx1)/(x3mx0*x3mx1*x3mx2)           DIFFMOI.211
              wjp1(i) = y0+aa*xmx0+bb*xmx0*xmx1+cc*xmx0*xmx1*xmx2                DIFFMOI.212
              ymax = max(y0,y1)                                                  DIFFMOI.213
              ymin = min(y0,y1)                                                  DIFFMOI.214
              wjp1(i) = min(wjp1(i),ymax)                                        DIFFMOI.215
              wjp1(i) = max(wjp1(i),ymin)                                        DIFFMOI.216
c  j-2                                                                           DIFFMOI.217
              kk = irkjm2(i,j,k)                                                 DIFFMOI.218
              mrk = mrkjm2(i,j,k)                                                DIFFMOI.219
c                                                                                DIFFMOI.220
              y0 = fl(I,J-2,kk+1)                                                DIFFMOI.221
              y1 = fl(I,J-2,kk)                                                  DIFFMOI.222
              y2 = fl(I,J-2,kk+2)                                                DIFFMOI.223
              y3 = fl(I,J-2,kk-1)                                                DIFFMOI.224
              xmx0 = dz3dl(i,j-2,kk)*(1-mrk)                                     DIFFMOI.225
              xmx1 = -dz3dl(i,j-2,kk)*mrk                                        DIFFMOI.226
              xmx2 = dz3dl(i,j-2,kk+1) + xmx0                                    DIFFMOI.227
              x1mx0 = dz3dl(i,j-2,kk)                                            DIFFMOI.228
              x0mx2 = dz3dl(i,j-2,kk+1)                                          DIFFMOI.229
              x3mx1 = dz3dl(i,j-2,kk-1)                                          DIFFMOI.230
              x1mx2 = x1mx0 + x0mx2                                              DIFFMOI.231
              x3mx0 = x3mx1 + x1mx0                                              DIFFMOI.232
              x3mx2 = x3mx0 + x0mx2                                              DIFFMOI.233
              aa = (y1-y0)/x1mx0                                                 DIFFMOI.234
              bb = (y2-y0+aa*x0mx2)/(x0mx2*x1mx2)                                DIFFMOI.235
              cc = (y3-y0-aa*x3mx0-bb*x3mx0*x3mx1)/(x3mx0*x3mx1*x3mx2)           DIFFMOI.236
              wjm2(i) = y0+aa*xmx0+bb*xmx0*xmx1+cc*xmx0*xmx1*xmx2                DIFFMOI.237
              ymax = max(y0,y1)                                                  DIFFMOI.238
              ymin = min(y0,y1)                                                  DIFFMOI.239
              wjm2(i) = min(wjm2(i),ymax)                                        DIFFMOI.240
              wjm2(i) = max(wjm2(i),ymin)                                        DIFFMOI.241
c  j-1                                                                           DIFFMOI.242
              kk = irkjm1(i,j,k)                                                 DIFFMOI.243
              mrk = mrkjm1(i,j,k)                                                DIFFMOI.244
c                                                                                DIFFMOI.245
              y0 = fl(I,J-1,kk+1)                                                DIFFMOI.246
              y1 = fl(I,J-1,kk)                                                  DIFFMOI.247
              y2 = fl(I,J-1,kk+2)                                                DIFFMOI.248
              y3 = fl(I,J-1,kk-1)                                                DIFFMOI.249
              xmx0 = dz3dl(i,j-1,kk)*(1-mrk)                                     DIFFMOI.250
              xmx1 = -dz3dl(i,j-1,kk)*mrk                                        DIFFMOI.251
              xmx2 = dz3dl(i,j-1,kk+1) + xmx0                                    DIFFMOI.252
              x1mx0 = dz3dl(i,j-1,kk)                                            DIFFMOI.253
              x0mx2 = dz3dl(i,j-1,kk+1)                                          DIFFMOI.254
              x3mx1 = dz3dl(i,j-1,kk-1)                                          DIFFMOI.255
              x1mx2 = x1mx0 + x0mx2                                              DIFFMOI.256
              x3mx0 = x3mx1 + x1mx0                                              DIFFMOI.257
              x3mx2 = x3mx0 + x0mx2                                              DIFFMOI.258
              aa = (y1-y0)/x1mx0                                                 DIFFMOI.259
              bb = (y2-y0+aa*x0mx2)/(x0mx2*x1mx2)                                DIFFMOI.260
              cc = (y3-y0-aa*x3mx0-bb*x3mx0*x3mx1)/(x3mx0*x3mx1*x3mx2)           DIFFMOI.261
              wjm1(i) = y0+aa*xmx0+bb*xmx0*xmx1+cc*xmx0*xmx1*xmx2                DIFFMOI.262
              ymax = max(y0,y1)                                                  DIFFMOI.263
              ymin = min(y0,y1)                                                  DIFFMOI.264
              wjm1(i) = min(wjm1(i),ymax)                                        DIFFMOI.265
              wjm1(i) = max(wjm1(i),ymin)                                        DIFFMOI.266
c  i+2                                                                           DIFFMOI.267
              kk = irkip2(i,j,k)                                                 DIFFMOI.268
              mrk = mrkip2(i,j,k)                                                DIFFMOI.269
c                                                                                DIFFMOI.270
              y0 = fl(I+2,J,kk+1)                                                DIFFMOI.271
              y1 = fl(I+2,J,kk)                                                  DIFFMOI.272
              y2 = fl(I+2,J,kk+2)                                                DIFFMOI.273
              y3 = fl(I+2,J,kk-1)                                                DIFFMOI.274
              xmx0 = dz3dl(i+2,j,kk)*(1-mrk)                                     DIFFMOI.275
              xmx1 = -dz3dl(i+2,j,kk)*mrk                                        DIFFMOI.276
              xmx2 = dz3dl(i+2,j,kk+1) + xmx0                                    DIFFMOI.277
              x1mx0 = dz3dl(i+2,j,kk)                                            DIFFMOI.278
              x0mx2 = dz3dl(i+2,j,kk+1)                                          DIFFMOI.279
              x3mx1 = dz3dl(i+2,j,kk-1)                                          DIFFMOI.280
              x1mx2 = x1mx0 + x0mx2                                              DIFFMOI.281
              x3mx0 = x3mx1 + x1mx0                                              DIFFMOI.282
              x3mx2 = x3mx0 + x0mx2                                              DIFFMOI.283
              aa = (y1-y0)/x1mx0                                                 DIFFMOI.284
              bb = (y2-y0+aa*x0mx2)/(x0mx2*x1mx2)                                DIFFMOI.285
              cc = (y3-y0-aa*x3mx0-bb*x3mx0*x3mx1)/(x3mx0*x3mx1*x3mx2)           DIFFMOI.286
              wip2(i) = y0+aa*xmx0+bb*xmx0*xmx1+cc*xmx0*xmx1*xmx2                DIFFMOI.287
              ymax = max(y0,y1)                                                  DIFFMOI.288
              ymin = min(y0,y1)                                                  DIFFMOI.289
              wip2(i) = min(wip2(i),ymax)                                        DIFFMOI.290
              wip2(i) = max(wip2(i),ymin)                                        DIFFMOI.291
c  i+1                                                                           DIFFMOI.292
              kk = irkip1(i,j,k)                                                 DIFFMOI.293
              mrk = mrkip1(i,j,k)                                                DIFFMOI.294
c                                                                                DIFFMOI.295
              y0 = fl(I+1,J,kk+1)                                                DIFFMOI.296
              y1 = fl(I+1,J,kk)                                                  DIFFMOI.297
              y2 = fl(I+1,J,kk+2)                                                DIFFMOI.298
              y3 = fl(I+1,J,kk-1)                                                DIFFMOI.299
              xmx0 = dz3dl(i+1,j,kk)*(1-mrk)                                     DIFFMOI.300
              xmx1 = -dz3dl(i+1,j,kk)*mrk                                        DIFFMOI.301
              xmx2 = dz3dl(i+1,j,kk+1) + xmx0                                    DIFFMOI.302
              x1mx0 = dz3dl(i+1,j,kk)                                            DIFFMOI.303
              x0mx2 = dz3dl(i+1,j,kk+1)                                          DIFFMOI.304
              x3mx1 = dz3dl(i+1,j,kk-1)                                          DIFFMOI.305
              x1mx2 = x1mx0 + x0mx2                                              DIFFMOI.306
              x3mx0 = x3mx1 + x1mx0                                              DIFFMOI.307
              x3mx2 = x3mx0 + x0mx2                                              DIFFMOI.308
              aa = (y1-y0)/x1mx0                                                 DIFFMOI.309
              bb = (y2-y0+aa*x0mx2)/(x0mx2*x1mx2)                                DIFFMOI.310
              cc = (y3-y0-aa*x3mx0-bb*x3mx0*x3mx1)/(x3mx0*x3mx1*x3mx2)           DIFFMOI.311
              wip1(i) = y0+aa*xmx0+bb*xmx0*xmx1+cc*xmx0*xmx1*xmx2                DIFFMOI.312
              ymax = max(y0,y1)                                                  DIFFMOI.313
              ymin = min(y0,y1)                                                  DIFFMOI.314
              wip1(i) = min(wip1(i),ymax)                                        DIFFMOI.315
              wip1(i) = max(wip1(i),ymin)                                        DIFFMOI.316
c  i-2                                                                           DIFFMOI.317
              kk = irkim2(i,j,k)                                                 DIFFMOI.318
              mrk = mrkim2(i,j,k)                                                DIFFMOI.319
c                                                                                DIFFMOI.320
              y0 = fl(I-2,J,kk+1)                                                DIFFMOI.321
              y1 = fl(I-2,J,kk)                                                  DIFFMOI.322
              y2 = fl(I-2,J,kk+2)                                                DIFFMOI.323
              y3 = fl(I-2,J,kk-1)                                                DIFFMOI.324
              xmx0 = dz3dl(i-2,j,kk)*(1-mrk)                                     DIFFMOI.325
              xmx1 = -dz3dl(i-2,j,kk)*mrk                                        DIFFMOI.326
              xmx2 = dz3dl(i-2,j,kk+1) + xmx0                                    DIFFMOI.327
              x1mx0 = dz3dl(i-2,j,kk)                                            DIFFMOI.328
              x0mx2 = dz3dl(i-2,j,kk+1)                                          DIFFMOI.329
              x3mx1 = dz3dl(i-2,j,kk-1)                                          DIFFMOI.330
              x1mx2 = x1mx0 + x0mx2                                              DIFFMOI.331
              x3mx0 = x3mx1 + x1mx0                                              DIFFMOI.332
              x3mx2 = x3mx0 + x0mx2                                              DIFFMOI.333
              aa = (y1-y0)/x1mx0                                                 DIFFMOI.334
              bb = (y2-y0+aa*x0mx2)/(x0mx2*x1mx2)                                DIFFMOI.335
              cc = (y3-y0-aa*x3mx0-bb*x3mx0*x3mx1)/(x3mx0*x3mx1*x3mx2)           DIFFMOI.336
              wim2(i) = y0+aa*xmx0+bb*xmx0*xmx1+cc*xmx0*xmx1*xmx2                DIFFMOI.337
              ymax = max(y0,y1)                                                  DIFFMOI.338
              ymin = min(y0,y1)                                                  DIFFMOI.339
              wim2(i) = min(wim2(i),ymax)                                        DIFFMOI.340
              wim2(i) = max(wim2(i),ymin)                                        DIFFMOI.341
c  i-1                                                                           DIFFMOI.342
              kk = irkim1(i,j,k)                                                 DIFFMOI.343
              mrk = mrkim1(i,j,k)                                                DIFFMOI.344
c                                                                                DIFFMOI.345
              y0 = fl(I-1,J,kk+1)                                                DIFFMOI.346
              y1 = fl(I-1,J,kk)                                                  DIFFMOI.347
              y2 = fl(I-1,J,kk+2)                                                DIFFMOI.348
              y3 = fl(I-1,J,kk-1)                                                DIFFMOI.349
              xmx0 = dz3dl(i-1,j,kk)*(1-mrk)                                     DIFFMOI.350
              xmx1 = -dz3dl(i-1,j,kk)*mrk                                        DIFFMOI.351
              xmx2 = dz3dl(i-1,j,kk+1) + xmx0                                    DIFFMOI.352
              x1mx0 = dz3dl(i-1,j,kk)                                            DIFFMOI.353
              x0mx2 = dz3dl(i-1,j,kk+1)                                          DIFFMOI.354
              x3mx1 = dz3dl(i-1,j,kk-1)                                          DIFFMOI.355
              x1mx2 = x1mx0 + x0mx2                                              DIFFMOI.356
              x3mx0 = x3mx1 + x1mx0                                              DIFFMOI.357
              x3mx2 = x3mx0 + x0mx2                                              DIFFMOI.358
              aa = (y1-y0)/x1mx0                                                 DIFFMOI.359
              bb = (y2-y0+aa*x0mx2)/(x0mx2*x1mx2)                                DIFFMOI.360
              cc = (y3-y0-aa*x3mx0-bb*x3mx0*x3mx1)/(x3mx0*x3mx1*x3mx2)           DIFFMOI.361
              wim1(i) = y0+aa*xmx0+bb*xmx0*xmx1+cc*xmx0*xmx1*xmx2                DIFFMOI.362
              ymax = max(y0,y1)                                                  DIFFMOI.363
              ymin = min(y0,y1)                                                  DIFFMOI.364
              wim1(i) = min(wim1(i),ymax)                                        DIFFMOI.365
              wim1(i) = max(wim1(i),ymin)                                        DIFFMOI.366
            ENDDO                                                                DIFFMOI.367
                                                                                 DIFFMOI.368
                                                                                 DIFFMOI.369
          ELSE   ! linear interpolation                                          DIFFMOI.370
            DO i=ib,ie                                                           DIFFMOI.371
              wjp2(i) = mrkjp2(i,j,k)                                            DIFFMOI.372
              wjp1(i) = mrkjp1(i,j,k)                                            DIFFMOI.373
              wjm2(i) = mrkjm2(i,j,k)                                            DIFFMOI.374
              wjm1(i) = mrkjm1(i,j,k)                                            DIFFMOI.375
              wip2(i) = mrkip2(i,j,k)                                            DIFFMOI.376
              wip1(i) = mrkip1(i,j,k)                                            DIFFMOI.377
              wim2(i) = mrkim2(i,j,k)                                            DIFFMOI.378
              wim1(i) = mrkim1(i,j,k)                                            DIFFMOI.379
            ENDDO                                                                DIFFMOI.380
          ENDIF                                                                  DIFFMOI.381
                                                                                 DIFFMOI.382
                                                                                 DIFFMOI.383
                                                                                 DIFFMOI.384
          IF (chwgt.ge.2) then                                                   DIFFMOI.385
            DO I=ib,ie                                                           DIFFMOI.386
                                                                                 DIFFMOI.387
              h2(i,k) = wjp2(i)+wjm2(i)                                          DIFFMOI.388
              h1(i,k) = wjp1(i)+wjm1(i)                                          DIFFMOI.389
                                                                                 DIFFMOI.390
              FTJ(I,K) = h2(i,k)-4*h1(i,k)+6*fl(I,J,K)                           DIFFMOI.391
              FJTP(I,K) = -h2(i,k)-4*h1(i,k)+10*fl(I,J,K)                        DIFFMOI.392
              if (FJTP(i,k)*FTJ(i,k).lt.ums*fl(i,j,k)*fl(i,j,k)) then            DIFFMOI.393
                FTJ(I,K) = 2*(2*fl(I,J,K)-h1(i,k))                               DIFFMOI.394
              endif                                                              DIFFMOI.395
                                                                                 DIFFMOI.396
              h2(i,k) = wip2(i)+wim2(i)                                          DIFFMOI.397
              h1(i,k) = wip1(i)+wim1(i)                                          DIFFMOI.398
                                                                                 DIFFMOI.399
              FTI(I,K) = h2(i,k)-4*h1(i,k)+6*fl(I,J,K)                           DIFFMOI.400
              FITP(I,K) = -h2(i,k)-4*h1(i,k)+10*fl(I,J,K)                        DIFFMOI.401
              if (FITP(i,k)*FTI(i,k).lt.ums*fl(i,j,k)*fl(i,j,k)) then            DIFFMOI.402
                FTI(I,K) = 2*(2*fl(I,J,K)-h1(i,k))                               DIFFMOI.403
              endif                                                              DIFFMOI.404
                                                                                 DIFFMOI.405
                                                                                 DIFFMOI.406
                                                                                 DIFFMOI.407
               FTEN(I,J,K)=FTEN(I,J,K)-XK(I,J,K)*C203*PSB(I,J)*                  DIFFMOI.408
     &                     (FTI(I,K)+FTJ(I,K))                                   DIFFMOI.409
                                                                                 DIFFMOI.410
            ENDDO                                                                DIFFMOI.411
          ELSE                                                                   DIFFMOI.412
            DO I=ib,ie                                                           DIFFMOI.413
                                                                                 DIFFMOI.414
              h2(i,k) = (1-wjp2(i))*                                             DIFFMOI.415
     &             fl(I,J+2,irkjp2(i,j,k))+                                      DIFFMOI.416
     &             wjp2(i)*fl(I,J+2,irkjp2(i,j,k)+1)+                            DIFFMOI.417
     &             (1-wjm2(i))*fl(I,J-2,irkjm2(i,j,k))+                          DIFFMOI.418
     &             wjm2(i)*fl(I,J-2,irkjm2(i,j,k)+1)                             DIFFMOI.419
              h1(i,k) = (1-wjp1(i))*fl(I,J+1,irkjp1(i,j,k))+                     DIFFMOI.420
     &             wjp1(i)*fl(I,J+1,irkjp1(i,j,k)+1)+                            DIFFMOI.421
     &             (1-wjm1(i))*fl(I,J-1,irkjm1(i,j,k))+                          DIFFMOI.422
     &             wjm1(i)*fl(I,J-1,irkjm1(i,j,k)+1)                             DIFFMOI.423
                                                                                 DIFFMOI.424
              FTJ(I,K) = h2(i,k)-4*h1(i,k)+6*fl(I,J,K)                           DIFFMOI.425
              FJTP(I,K) = -h2(i,k)-4*h1(i,k)+10*fl(I,J,K)                        DIFFMOI.426
              if (FJTP(i,k)*FTJ(i,k).lt.ums*fl(i,j,k)*fl(i,j,k)) then            DIFFMOI.427
                FTJ(I,K) = 2*(2*fl(I,J,K)-h1(i,k))                               DIFFMOI.428
              endif                                                              DIFFMOI.429
                                                                                 DIFFMOI.430
              h2(i,k) = (1-wip2(i))*                                             DIFFMOI.431
     &             fl(I+2,J,irkip2(i,j,k))+                                      DIFFMOI.432
     &             wip2(i)*fl(I+2,J,irkip2(i,j,k)+1)+                            DIFFMOI.433
     &             (1-wim2(i))*fl(I-2,J,irkim2(i,j,k))+                          DIFFMOI.434
     &             wim2(i)*fl(I-2,J,irkim2(i,j,k)+1)                             DIFFMOI.435
              h1(i,k) = (1-wip1(i))*fl(I+1,J,irkip1(i,j,k))+                     DIFFMOI.436
     &             wip1(i)*fl(I+1,J,irkip1(i,j,k)+1)+                            DIFFMOI.437
     &             (1-wim1(i))*fl(I-1,J,irkim1(i,j,k))+                          DIFFMOI.438
     &             wim1(i)*fl(I-1,J,irkim1(i,j,k)+1)                             DIFFMOI.439
                                                                                 DIFFMOI.440
              FTI(I,K) = h2(i,k)-4*h1(i,k)+6*fl(I,J,K)                           DIFFMOI.441
              FITP(I,K) = -h2(i,k)-4*h1(i,k)+10*fl(I,J,K)                        DIFFMOI.442
              if (FITP(i,k)*FTI(i,k).lt.ums*fl(i,j,k)*fl(i,j,k)) then            DIFFMOI.443
                FTI(I,K) = 2*(2*fl(I,J,K)-h1(i,k))                               DIFFMOI.444
              endif                                                              DIFFMOI.445
                                                                                 DIFFMOI.446
               FTEN(I,J,K)=FTEN(I,J,K)-XK(I,J,K)*C203*PSB(I,J)*                  DIFFMOI.447
     &             (FTI(I,K)+FTJ(I,K))                                           DIFFMOI.448
                                                                                 DIFFMOI.449
            ENDDO                                                                DIFFMOI.450
          ENDIF                                                                  DIFFMOI.451
        ENDDO                                                                    DIFFMOI.452
                                                                                 DIFFMOI.453
                                                                                 DIFFMOI.454
                                                                                 DIFFMOI.455
                                                                                 DIFFMOI.456
c                                                                                DIFFMOI.457
        DO K=kmv+1,kl     ! Levels where topography may be intersected           DIFFMOI.458
          IF (chwgt.ge.2) then ! Third-order polynomial interpolation            DIFFMOI.459
            DO i=ib,ie                                                           DIFFMOI.460
              if (gezjp(i,j,k).gt.0.01) then                                     DIFFMOI.461
c  j+2                                                                           DIFFMOI.462
                kk = irkjp2(i,j,k)                                               DIFFMOI.463
                mrk = mrkjp2(i,j,k)                                              DIFFMOI.464
c                                                                                DIFFMOI.465
                y0 = fl(I,J+2,kk+1)                                              DIFFMOI.466
                y1 = fl(I,J+2,kk)                                                DIFFMOI.467
                y2 = fl(I,J+2,kk+2)                                              DIFFMOI.468
                y3 = fl(I,J+2,kk-1)                                              DIFFMOI.469
                xmx0 = dz3dl(i,j+2,kk)*(1-mrk)                                   DIFFMOI.470
                xmx1 = -dz3dl(i,j+2,kk)*mrk                                      DIFFMOI.471
                xmx2 = dz3dl(i,j+2,kk+1) + xmx0                                  DIFFMOI.472
                x1mx0 = dz3dl(i,j+2,kk)                                          DIFFMOI.473
                x0mx2 = dz3dl(i,j+2,kk+1)                                        DIFFMOI.474
                x3mx1 = dz3dl(i,j+2,kk-1)                                        DIFFMOI.475
                x1mx2 = x1mx0 + x0mx2                                            DIFFMOI.476
                x3mx0 = x3mx1 + x1mx0                                            DIFFMOI.477
                x3mx2 = x3mx0 + x0mx2                                            DIFFMOI.478
                aa = (y1-y0)/x1mx0                                               DIFFMOI.479
                bb = (y2-y0+aa*x0mx2)/(x0mx2*x1mx2)                              DIFFMOI.480
                cc = (y3-y0-aa*x3mx0-bb*x3mx0*x3mx1)/(x3mx0*x3mx1*x3mx2)         DIFFMOI.481
                wjp2(i) = y0+aa*xmx0+bb*xmx0*xmx1+cc*xmx0*xmx1*xmx2              DIFFMOI.482
                ymax = max(y0,y1)                                                DIFFMOI.483
                ymin = min(y0,y1)                                                DIFFMOI.484
                wjp2(i) = min(wjp2(i),ymax)                                      DIFFMOI.485
                wjp2(i) = max(wjp2(i),ymin)                                      DIFFMOI.486
c  j+1                                                                           DIFFMOI.487
                kk = irkjp1(i,j,k)                                               DIFFMOI.488
                mrk = mrkjp1(i,j,k)                                              DIFFMOI.489
c                                                                                DIFFMOI.490
                y0 = fl(I,J+1,kk+1)                                              DIFFMOI.491
                y1 = fl(I,J+1,kk)                                                DIFFMOI.492
                y2 = fl(I,J+1,kk+2)                                              DIFFMOI.493
                y3 = fl(I,J+1,kk-1)                                              DIFFMOI.494
                xmx0 = dz3dl(i,j+1,kk)*(1-mrk)                                   DIFFMOI.495
                xmx1 = -dz3dl(i,j+1,kk)*mrk                                      DIFFMOI.496
                xmx2 = dz3dl(i,j+1,kk+1) + xmx0                                  DIFFMOI.497
                x1mx0 = dz3dl(i,j+1,kk)                                          DIFFMOI.498
                x0mx2 = dz3dl(i,j+1,kk+1)                                        DIFFMOI.499
                x3mx1 = dz3dl(i,j+1,kk-1)                                        DIFFMOI.500
                x1mx2 = x1mx0 + x0mx2                                            DIFFMOI.501
                x3mx0 = x3mx1 + x1mx0                                            DIFFMOI.502
                x3mx2 = x3mx0 + x0mx2                                            DIFFMOI.503
                aa = (y1-y0)/x1mx0                                               DIFFMOI.504
                bb = (y2-y0+aa*x0mx2)/(x0mx2*x1mx2)                              DIFFMOI.505
                cc = (y3-y0-aa*x3mx0-bb*x3mx0*x3mx1)/(x3mx0*x3mx1*x3mx2)         DIFFMOI.506
                wjp1(i) = y0+aa*xmx0+bb*xmx0*xmx1+cc*xmx0*xmx1*xmx2              DIFFMOI.507
                ymax = max(y0,y1)                                                DIFFMOI.508
                ymin = min(y0,y1)                                                DIFFMOI.509
                wjp1(i) = min(wjp1(i),ymax)                                      DIFFMOI.510
                wjp1(i) = max(wjp1(i),ymin)                                      DIFFMOI.511
              endif                                                              DIFFMOI.512
              if (gezjm(i,j,k).gt.0.01) then                                     DIFFMOI.513
c  j-2                                                                           DIFFMOI.514
                kk = irkjm2(i,j,k)                                               DIFFMOI.515
                mrk = mrkjm2(i,j,k)                                              DIFFMOI.516
c                                                                                DIFFMOI.517
                y0 = fl(I,J-2,kk+1)                                              DIFFMOI.518
                y1 = fl(I,J-2,kk)                                                DIFFMOI.519
                y2 = fl(I,J-2,kk+2)                                              DIFFMOI.520
                y3 = fl(I,J-2,kk-1)                                              DIFFMOI.521
                xmx0 = dz3dl(i,j-2,kk)*(1-mrk)                                   DIFFMOI.522
                xmx1 = -dz3dl(i,j-2,kk)*mrk                                      DIFFMOI.523
                xmx2 = dz3dl(i,j-2,kk+1) + xmx0                                  DIFFMOI.524
                x1mx0 = dz3dl(i,j-2,kk)                                          DIFFMOI.525
                x0mx2 = dz3dl(i,j-2,kk+1)                                        DIFFMOI.526
                x3mx1 = dz3dl(i,j-2,kk-1)                                        DIFFMOI.527
                x1mx2 = x1mx0 + x0mx2                                            DIFFMOI.528
                x3mx0 = x3mx1 + x1mx0                                            DIFFMOI.529
                x3mx2 = x3mx0 + x0mx2                                            DIFFMOI.530
                aa = (y1-y0)/x1mx0                                               DIFFMOI.531
                bb = (y2-y0+aa*x0mx2)/(x0mx2*x1mx2)                              DIFFMOI.532
                cc = (y3-y0-aa*x3mx0-bb*x3mx0*x3mx1)/(x3mx0*x3mx1*x3mx2)         DIFFMOI.533
                wjm2(i) = y0+aa*xmx0+bb*xmx0*xmx1+cc*xmx0*xmx1*xmx2              DIFFMOI.534
                ymax = max(y0,y1)                                                DIFFMOI.535
                ymin = min(y0,y1)                                                DIFFMOI.536
                wjm2(i) = min(wjm2(i),ymax)                                      DIFFMOI.537
                wjm2(i) = max(wjm2(i),ymin)                                      DIFFMOI.538
c  j-1                                                                           DIFFMOI.539
                kk = irkjm1(i,j,k)                                               DIFFMOI.540
                mrk = mrkjm1(i,j,k)                                              DIFFMOI.541
c                                                                                DIFFMOI.542
                y0 = fl(I,J-1,kk+1)                                              DIFFMOI.543
                y1 = fl(I,J-1,kk)                                                DIFFMOI.544
                y2 = fl(I,J-1,kk+2)                                              DIFFMOI.545
                y3 = fl(I,J-1,kk-1)                                              DIFFMOI.546
                xmx0 = dz3dl(i,j-1,kk)*(1-mrk)                                   DIFFMOI.547
                xmx1 = -dz3dl(i,j-1,kk)*mrk                                      DIFFMOI.548
                xmx2 = dz3dl(i,j-1,kk+1) + xmx0                                  DIFFMOI.549
                x1mx0 = dz3dl(i,j-1,kk)                                          DIFFMOI.550
                x0mx2 = dz3dl(i,j-1,kk+1)                                        DIFFMOI.551
                x3mx1 = dz3dl(i,j-1,kk-1)                                        DIFFMOI.552
                x1mx2 = x1mx0 + x0mx2                                            DIFFMOI.553
                x3mx0 = x3mx1 + x1mx0                                            DIFFMOI.554
                x3mx2 = x3mx0 + x0mx2                                            DIFFMOI.555
                aa = (y1-y0)/x1mx0                                               DIFFMOI.556
                bb = (y2-y0+aa*x0mx2)/(x0mx2*x1mx2)                              DIFFMOI.557
                cc = (y3-y0-aa*x3mx0-bb*x3mx0*x3mx1)/(x3mx0*x3mx1*x3mx2)         DIFFMOI.558
                wjm1(i) = y0+aa*xmx0+bb*xmx0*xmx1+cc*xmx0*xmx1*xmx2              DIFFMOI.559
                ymax = max(y0,y1)                                                DIFFMOI.560
                ymin = min(y0,y1)                                                DIFFMOI.561
                wjm1(i) = min(wjm1(i),ymax)                                      DIFFMOI.562
                wjm1(i) = max(wjm1(i),ymin)                                      DIFFMOI.563
              endif                                                              DIFFMOI.564
              if (gezip(i,j,k).gt.0.01) then                                     DIFFMOI.565
c  i+2                                                                           DIFFMOI.566
                kk = irkip2(i,j,k)                                               DIFFMOI.567
                mrk = mrkip2(i,j,k)                                              DIFFMOI.568
c                                                                                DIFFMOI.569
                y0 = fl(I+2,J,kk+1)                                              DIFFMOI.570
                y1 = fl(I+2,J,kk)                                                DIFFMOI.571
                y2 = fl(I+2,J,kk+2)                                              DIFFMOI.572
                y3 = fl(I+2,J,kk-1)                                              DIFFMOI.573
                xmx0 = dz3dl(i+2,j,kk)*(1-mrk)                                   DIFFMOI.574
                xmx1 = -dz3dl(i+2,j,kk)*mrk                                      DIFFMOI.575
                xmx2 = dz3dl(i+2,j,kk+1) + xmx0                                  DIFFMOI.576
                x1mx0 = dz3dl(i+2,j,kk)                                          DIFFMOI.577
                x0mx2 = dz3dl(i+2,j,kk+1)                                        DIFFMOI.578
                x3mx1 = dz3dl(i+2,j,kk-1)                                        DIFFMOI.579
                x1mx2 = x1mx0 + x0mx2                                            DIFFMOI.580
                x3mx0 = x3mx1 + x1mx0                                            DIFFMOI.581
                x3mx2 = x3mx0 + x0mx2                                            DIFFMOI.582
                aa = (y1-y0)/x1mx0                                               DIFFMOI.583
                bb = (y2-y0+aa*x0mx2)/(x0mx2*x1mx2)                              DIFFMOI.584
                cc = (y3-y0-aa*x3mx0-bb*x3mx0*x3mx1)/(x3mx0*x3mx1*x3mx2)         DIFFMOI.585
                wip2(i) = y0+aa*xmx0+bb*xmx0*xmx1+cc*xmx0*xmx1*xmx2              DIFFMOI.586
                ymax = max(y0,y1)                                                DIFFMOI.587
                ymin = min(y0,y1)                                                DIFFMOI.588
                wip2(i) = min(wip2(i),ymax)                                      DIFFMOI.589
                wip2(i) = max(wip2(i),ymin)                                      DIFFMOI.590
c  i+1                                                                           DIFFMOI.591
                kk = irkip1(i,j,k)                                               DIFFMOI.592
                mrk = mrkip1(i,j,k)                                              DIFFMOI.593
c                                                                                DIFFMOI.594
                y0 = fl(I+1,J,kk+1)                                              DIFFMOI.595
                y1 = fl(I+1,J,kk)                                                DIFFMOI.596
                y2 = fl(I+1,J,kk+2)                                              DIFFMOI.597
                y3 = fl(I+1,J,kk-1)                                              DIFFMOI.598
                xmx0 = dz3dl(i+1,j,kk)*(1-mrk)                                   DIFFMOI.599
                xmx1 = -dz3dl(i+1,j,kk)*mrk                                      DIFFMOI.600
                xmx2 = dz3dl(i+1,j,kk+1) + xmx0                                  DIFFMOI.601
                x1mx0 = dz3dl(i+1,j,kk)                                          DIFFMOI.602
                x0mx2 = dz3dl(i+1,j,kk+1)                                        DIFFMOI.603
                x3mx1 = dz3dl(i+1,j,kk-1)                                        DIFFMOI.604
                x1mx2 = x1mx0 + x0mx2                                            DIFFMOI.605
                x3mx0 = x3mx1 + x1mx0                                            DIFFMOI.606
                x3mx2 = x3mx0 + x0mx2                                            DIFFMOI.607
                aa = (y1-y0)/x1mx0                                               DIFFMOI.608
                bb = (y2-y0+aa*x0mx2)/(x0mx2*x1mx2)                              DIFFMOI.609
                cc = (y3-y0-aa*x3mx0-bb*x3mx0*x3mx1)/(x3mx0*x3mx1*x3mx2)         DIFFMOI.610
                wip1(i) = y0+aa*xmx0+bb*xmx0*xmx1+cc*xmx0*xmx1*xmx2              DIFFMOI.611
                ymax = max(y0,y1)                                                DIFFMOI.612
                ymin = min(y0,y1)                                                DIFFMOI.613
                wip1(i) = min(wip1(i),ymax)                                      DIFFMOI.614
                wip1(i) = max(wip1(i),ymin)                                      DIFFMOI.615
              endif                                                              DIFFMOI.616
              if (gezim(i,j,k).gt.0.01) then                                     DIFFMOI.617
c  i-2                                                                           DIFFMOI.618
                kk = irkim2(i,j,k)                                               DIFFMOI.619
                mrk = mrkim2(i,j,k)                                              DIFFMOI.620
c                                                                                DIFFMOI.621
                y0 = fl(I-2,J,kk+1)                                              DIFFMOI.622
                y1 = fl(I-2,J,kk)                                                DIFFMOI.623
                y2 = fl(I-2,J,kk+2)                                              DIFFMOI.624
                y3 = fl(I-2,J,kk-1)                                              DIFFMOI.625
                xmx0 = dz3dl(i-2,j,kk)*(1-mrk)                                   DIFFMOI.626
                xmx1 = -dz3dl(i-2,j,kk)*mrk                                      DIFFMOI.627
                xmx2 = dz3dl(i-2,j,kk+1) + xmx0                                  DIFFMOI.628
                x1mx0 = dz3dl(i-2,j,kk)                                          DIFFMOI.629
                x0mx2 = dz3dl(i-2,j,kk+1)                                        DIFFMOI.630
                x3mx1 = dz3dl(i-2,j,kk-1)                                        DIFFMOI.631
                x1mx2 = x1mx0 + x0mx2                                            DIFFMOI.632
                x3mx0 = x3mx1 + x1mx0                                            DIFFMOI.633
                x3mx2 = x3mx0 + x0mx2                                            DIFFMOI.634
                aa = (y1-y0)/x1mx0                                               DIFFMOI.635
                bb = (y2-y0+aa*x0mx2)/(x0mx2*x1mx2)                              DIFFMOI.636
                cc = (y3-y0-aa*x3mx0-bb*x3mx0*x3mx1)/(x3mx0*x3mx1*x3mx2)         DIFFMOI.637
                wim2(i) = y0+aa*xmx0+bb*xmx0*xmx1+cc*xmx0*xmx1*xmx2              DIFFMOI.638
                ymax = max(y0,y1)                                                DIFFMOI.639
                ymin = min(y0,y1)                                                DIFFMOI.640
                wim2(i) = min(wim2(i),ymax)                                      DIFFMOI.641
                wim2(i) = max(wim2(i),ymin)                                      DIFFMOI.642
c  i-1                                                                           DIFFMOI.643
                kk = irkim1(i,j,k)                                               DIFFMOI.644
                mrk = mrkim1(i,j,k)                                              DIFFMOI.645
c                                                                                DIFFMOI.646
                y0 = fl(I-1,J,kk+1)                                              DIFFMOI.647
                y1 = fl(I-1,J,kk)                                                DIFFMOI.648
                y2 = fl(I-1,J,kk+2)                                              DIFFMOI.649
                y3 = fl(I-1,J,kk-1)                                              DIFFMOI.650
                xmx0 = dz3dl(i-1,j,kk)*(1-mrk)                                   DIFFMOI.651
                xmx1 = -dz3dl(i-1,j,kk)*mrk                                      DIFFMOI.652
                xmx2 = dz3dl(i-1,j,kk+1) + xmx0                                  DIFFMOI.653
                x1mx0 = dz3dl(i-1,j,kk)                                          DIFFMOI.654
                x0mx2 = dz3dl(i-1,j,kk+1)                                        DIFFMOI.655
                x3mx1 = dz3dl(i-1,j,kk-1)                                        DIFFMOI.656
                x1mx2 = x1mx0 + x0mx2                                            DIFFMOI.657
                x3mx0 = x3mx1 + x1mx0                                            DIFFMOI.658
                x3mx2 = x3mx0 + x0mx2                                            DIFFMOI.659
                aa = (y1-y0)/x1mx0                                               DIFFMOI.660
                bb = (y2-y0+aa*x0mx2)/(x0mx2*x1mx2)                              DIFFMOI.661
                cc = (y3-y0-aa*x3mx0-bb*x3mx0*x3mx1)/(x3mx0*x3mx1*x3mx2)         DIFFMOI.662
                wim1(i) = y0+aa*xmx0+bb*xmx0*xmx1+cc*xmx0*xmx1*xmx2              DIFFMOI.663
                ymax = max(y0,y1)                                                DIFFMOI.664
                ymin = min(y0,y1)                                                DIFFMOI.665
                wim1(i) = min(wim1(i),ymax)                                      DIFFMOI.666
                wim1(i) = max(wim1(i),ymin)                                      DIFFMOI.667
              endif                                                              DIFFMOI.668
            ENDDO                                                                DIFFMOI.669
                                                                                 DIFFMOI.670
          ELSE                                                                   DIFFMOI.671
            do i=ib,ie   ! linear interpolation                                  DIFFMOI.672
              wjp2(i) = mrkjp2(i,j,k)                                            DIFFMOI.673
              wjp1(i) = mrkjp1(i,j,k)                                            DIFFMOI.674
              wjm2(i) = mrkjm2(i,j,k)                                            DIFFMOI.675
              wjm1(i) = mrkjm1(i,j,k)                                            DIFFMOI.676
              wip2(i) = mrkip2(i,j,k)                                            DIFFMOI.677
              wip1(i) = mrkip1(i,j,k)                                            DIFFMOI.678
              wim2(i) = mrkim2(i,j,k)                                            DIFFMOI.679
              wim1(i) = mrkim1(i,j,k)                                            DIFFMOI.680
            enddo                                                                DIFFMOI.681
          ENDIF                                                                  DIFFMOI.682
                                                                                 DIFFMOI.683
                                                                                 DIFFMOI.684
                                                                                 DIFFMOI.685
          IF (chwgt.ge.2) then                                                   DIFFMOI.686
            DO I=ib,ie                                                           DIFFMOI.687
                                                                                 DIFFMOI.688
c  one-sided diffusion                                                           DIFFMOI.689
                                                                                 DIFFMOI.690
              if (gezjp(i,j,k).gt.0.01) then                                     DIFFMOI.691
                h2(i,k) = wjp2(i)                                                DIFFMOI.692
                h1(i,k) = wjp1(i)                                                DIFFMOI.693
                FTJP(I,K) = h2(i,k)-4*h1(i,k)+3*fl(I,J,K)                        DIFFMOI.694
                FJTP(I,K) = -h2(i,k)-4*h1(i,k)+5*fl(I,J,K)                       DIFFMOI.695
                if (FJTP(i,k)*FTJP(i,k).lt.ums*fl(i,j,k)*fl(i,j,k)) then         DIFFMOI.696
                  FTJP(I,K) = 2*(fl(I,J,K)-h1(i,k))                              DIFFMOI.697
                endif                                                            DIFFMOI.698
              else                                                               DIFFMOI.699
                FTJP(I,K) = 0                                                    DIFFMOI.700
              endif                                                              DIFFMOI.701
                                                                                 DIFFMOI.702
              if (gezip(i,j,k).gt.0.01) then                                     DIFFMOI.703
                h2(i,k) = wip2(i)                                                DIFFMOI.704
                h1(i,k) = wip1(i)                                                DIFFMOI.705
                FTIP(I,K) = h2(i,k)-4*h1(i,k)+3*fl(I,J,K)                        DIFFMOI.706
                FITP(I,K) = -h2(i,k)-4*h1(i,k)+5*fl(I,J,K)                       DIFFMOI.707
                if (FITP(i,k)*FTIP(i,k).lt.ums*fl(i,j,k)*fl(i,j,k)) then         DIFFMOI.708
                  FTIP(I,K) = 2*(fl(I,J,K)-h1(i,k))                              DIFFMOI.709
                endif                                                            DIFFMOI.710
              else                                                               DIFFMOI.711
                FTIP(I,K) = 0                                                    DIFFMOI.712
              endif                                                              DIFFMOI.713
                                                                                 DIFFMOI.714
              if (gezjm(i,j,k).gt.0.01) then                                     DIFFMOI.715
                h2(i,k) = wjm2(i)                                                DIFFMOI.716
                h1(i,k) = wjm1(i)                                                DIFFMOI.717
                FTJM(I,K) = h2(i,k)-4*h1(i,k)+3*fl(I,J,K)                        DIFFMOI.718
                FJTM(I,K) = -h2(i,k)-4*h1(i,k)+5*fl(I,J,K)                       DIFFMOI.719
                if (FJTM(i,k)*FTJM(i,k).lt.ums*fl(i,j,k)*fl(i,j,k)) then         DIFFMOI.720
                  FTJM(I,K) = 2*(fl(I,J,K)-h1(i,k))                              DIFFMOI.721
                endif                                                            DIFFMOI.722
              else                                                               DIFFMOI.723
                FTJM(I,K) = 0                                                    DIFFMOI.724
              endif                                                              DIFFMOI.725
                                                                                 DIFFMOI.726
              if (gezim(i,j,k).gt.0.01) then                                     DIFFMOI.727
                h2(i,k) = wim2(i)                                                DIFFMOI.728
                h1(i,k) = wim1(i)                                                DIFFMOI.729
                FTIM(I,K) = h2(i,k)-4*h1(i,k)+3*fl(I,J,K)                        DIFFMOI.730
                FITM(I,K) = -h2(i,k)-4*h1(i,k)+5*fl(I,J,K)                       DIFFMOI.731
                if (FITM(i,k)*FTIM(i,k).lt.ums*fl(i,j,k)*fl(i,j,k)) then         DIFFMOI.732
                  FTIM(I,K) = 2*(fl(I,J,K)-h1(i,k))                              DIFFMOI.733
                endif                                                            DIFFMOI.734
              else                                                               DIFFMOI.735
                FTIM(I,K) = 0                                                    DIFFMOI.736
              endif                                                              DIFFMOI.737
                                                                                 DIFFMOI.738
c  sigma-diffusion                                                               DIFFMOI.739
                                                                                 DIFFMOI.740
              IF ((gesi(i,k).gt.0.01).or.(gesj(i,k).gt.0.01)) then               DIFFMOI.741
                FTJ(I,K) = fl(I,J+2,K)+fl(I,J-2,K)-4*(fl(I,J+1,K)+               DIFFMOI.742
     &                     fl(I,J-1,K))+6*fl(I,J,K)                              DIFFMOI.743
                                                                                 DIFFMOI.744
                FTI(I,K) = fl(I+2,J,K)+fl(I-2,J,K)-4*(fl(I+1,J,K)+               DIFFMOI.745
     &                     fl(I-1,J,K))+6*fl(I,J,K)                              DIFFMOI.746
                                                                                 DIFFMOI.747
c  Check whether unstable mode of fourth-order diffusion may cause problems      DIFFMOI.748
                                                                                 DIFFMOI.749
                FJT(i,k) = FTJ(I,K)+4*fl(I,J,K)-2*(fl(I,J+2,K)+                  DIFFMOI.750
     &                     fl(I,J-2,K))                                          DIFFMOI.751
                                                                                 DIFFMOI.752
                FIT(i,k) = FTI(I,K)+4*fl(I,J,K)-2*(fl(I+2,J,K)+                  DIFFMOI.753
     &                     fl(I-2,J,K))                                          DIFFMOI.754
                                                                                 DIFFMOI.755
                                                                                 DIFFMOI.756
                if (FJT(i,k)*FTJ(i,k).lt.ums*fl(i,j,k)*fl(i,j,k)) then           DIFFMOI.757
                  FTJ(i,k) = 4*fl(I,J,K)-2*(fl(I,J+1,K)+fl(I,J-1,K))             DIFFMOI.758
                endif                                                            DIFFMOI.759
                                                                                 DIFFMOI.760
                if (FIT(i,k)*FTI(i,k).lt.ums*fl(i,j,k)*fl(i,j,k)) then           DIFFMOI.761
                  FTI(I,K) = 4*fl(I,J,K)-2*(fl(I+1,J,K)+fl(I-1,J,K))             DIFFMOI.762
                endif                                                            DIFFMOI.763
                                                                                 DIFFMOI.764
c  Subsidiary second-order diffusion                                             DIFFMOI.765
                                                                                 DIFFMOI.766
                FTIL(I,K) = fl(I,J,K)-fl(I-1,J,K)                                DIFFMOI.767
                FTJL(I,K) = fl(I,J,K)-fl(I,J-1,K)                                DIFFMOI.768
                FTIH(I,K) = fl(I,J,K)-fl(I+1,J,K)                                DIFFMOI.769
                FTJH(I,K) = fl(I,J,K)-fl(I,J+1,K)                                DIFFMOI.770
                                                                                 DIFFMOI.771
c                                                                                DIFFMOI.772
c                                                                                DIFFMOI.773
                FTEN(I,J,K)=FTEN(I,J,K)-XK(I,J,K)*C203*PSB(I,J)*(                DIFFMOI.774
     &             gesi(i,k)*(fin(i,j)*FTI(I,K)+                                 DIFFMOI.775
     &             min(100.,1./fin(i,j))/100.*max(0.,gesi(i,k)-0.75)*            DIFFMOI.776
     &             (fion(i,j)*FTIH(I,K)+fiun(i,j)*FTIL(I,K)))+                   DIFFMOI.777
     &             gesj(i,k)*(fjn(i,j)*FTJ(I,K)+                                 DIFFMOI.778
     &             min(100.,1./fjn(i,j))/100.*max(0.,gesj(i,k)-0.75)*            DIFFMOI.779
     &             (fjon(i,j)*FTJH(I,K)+fjun(i,j)*FTJL(I,K)))+(                  DIFFMOI.780
     &             gezip(i,j,k)*FTIP(I,K)+gezim(i,j,k)*FTIM(I,K)+                DIFFMOI.781
     &             gezjp(i,j,k)                                                  DIFFMOI.782
     &             *FTJP(I,K)+gezjm(i,j,k)*FTJM(I,K)))                           DIFFMOI.783
                                                                                 DIFFMOI.784
              else                                                               DIFFMOI.785
                                                                                 DIFFMOI.786
                FTEN(I,J,K)=FTEN(I,J,K)-XK(I,J,K)*C203*PSB(I,J)*(                DIFFMOI.787
     &             gezip(i,j,k)*FTIP(I,K)+gezim(i,j,k)*FTIM(I,K)+                DIFFMOI.788
     &             gezjp(i,j,k)                                                  DIFFMOI.789
     &             *FTJP(I,K)+gezjm(i,j,k)*FTJM(I,K))                            DIFFMOI.790
                                                                                 DIFFMOI.791
              ENDIF                                                              DIFFMOI.792
                                                                                 DIFFMOI.793
            ENDDO                                                                DIFFMOI.794
                                                                                 DIFFMOI.795
          ELSE                                                                   DIFFMOI.796
                                                                                 DIFFMOI.797
       DO I=ib,ie                                                                DIFFMOI.798
                                                                                 DIFFMOI.799
c  one-sided diffusion                                                           DIFFMOI.800
                                                                                 DIFFMOI.801
              if (gezjp(i,j,k).gt.0.01) then                                     DIFFMOI.802
                h2(i,k) = (1-wjp2(i))*                                           DIFFMOI.803
     &               fl(I,J+2,irkjp2(i,j,k))+                                    DIFFMOI.804
     &               wjp2(i)*fl(I,J+2,irkjp2(i,j,k)+1)                           DIFFMOI.805
                h1(i,k) = (1-wjp1(i))*fl(I,J+1,irkjp1(i,j,k))+                   DIFFMOI.806
     &               wjp1(i)*fl(I,J+1,irkjp1(i,j,k)+1)                           DIFFMOI.807
                FTJP(I,K) = h2(i,k)-4*h1(i,k)+3*fl(I,J,K)                        DIFFMOI.808
                FJTP(I,K) = -h2(i,k)-4*h1(i,k)+5*fl(I,J,K)                       DIFFMOI.809
                if (FJTP(i,k)*FTJP(i,k).lt.ums*fl(i,j,k)*fl(i,j,k)) then         DIFFMOI.810
                  FTJP(I,K) = 2*(fl(I,J,K)-h1(i,k))                              DIFFMOI.811
                endif                                                            DIFFMOI.812
              else                                                               DIFFMOI.813
                  FTJP(I,K) = 0                                                  DIFFMOI.814
              endif                                                              DIFFMOI.815
                                                                                 DIFFMOI.816
              if (gezip(i,j,k).gt.0.01) then                                     DIFFMOI.817
                h2(i,k) = (1-wip2(i))*                                           DIFFMOI.818
     &               fl(I+2,J,irkip2(i,j,k))+                                    DIFFMOI.819
     &               wip2(i)*fl(I+2,J,irkip2(i,j,k)+1)                           DIFFMOI.820
                h1(i,k) = (1-wip1(i))*fl(I+1,J,irkip1(i,j,k))+                   DIFFMOI.821
     &               wip1(i)*fl(I+1,J,irkip1(i,j,k)+1)                           DIFFMOI.822
                FTIP(I,K) = h2(i,k)-4*h1(i,k)+3*fl(I,J,K)                        DIFFMOI.823
                FITP(I,K) = -h2(i,k)-4*h1(i,k)+5*fl(I,J,K)                       DIFFMOI.824
                if (FITP(i,k)*FTIP(i,k).lt.ums*fl(i,j,k)*fl(i,j,k)) then         DIFFMOI.825
                  FTIP(I,K) = 2*(fl(I,J,K)-h1(i,k))                              DIFFMOI.826
                endif                                                            DIFFMOI.827
              else                                                               DIFFMOI.828
                FTIP(I,K) = 0                                                    DIFFMOI.829
              endif                                                              DIFFMOI.830
                                                                                 DIFFMOI.831
              if (gezjm(i,j,k).gt.0.01) then                                     DIFFMOI.832
                h2(i,k) = (1-wjm2(i))*                                           DIFFMOI.833
     &               fl(I,J-2,irkjm2(i,j,k))+                                    DIFFMOI.834
     &               wjm2(i)*fl(I,J-2,irkjm2(i,j,k)+1)                           DIFFMOI.835
                h1(i,k) = (1-wjm1(i))*fl(I,J-1,irkjm1(i,j,k))+                   DIFFMOI.836
     &               wjm1(i)*fl(I,J-1,irkjm1(i,j,k)+1)                           DIFFMOI.837
                FTJM(I,K) = h2(i,k)-4*h1(i,k)+3*fl(I,J,K)                        DIFFMOI.838
                FJTM(I,K) = -h2(i,k)-4*h1(i,k)+5*fl(I,J,K)                       DIFFMOI.839
                if (FJTM(i,k)*FTJM(i,k).lt.ums*fl(i,j,k)*fl(i,j,k)) then         DIFFMOI.840
                  FTJM(I,K) = 2*(fl(I,J,K)-h1(i,k))                              DIFFMOI.841
                endif                                                            DIFFMOI.842
              else                                                               DIFFMOI.843
                FTJM(I,K) = 0                                                    DIFFMOI.844
              endif                                                              DIFFMOI.845
                                                                                 DIFFMOI.846
              if (gezim(i,j,k).gt.0.01) then                                     DIFFMOI.847
                h2(i,k) = (1-wim2(i))*                                           DIFFMOI.848
     &               fl(I-2,J,irkim2(i,j,k))+                                    DIFFMOI.849
     &               wim2(i)*fl(I-2,J,irkim2(i,j,k)+1)                           DIFFMOI.850
                h1(i,k) = (1-wim1(i))*fl(I-1,J,irkim1(i,j,k))+                   DIFFMOI.851
     &               wim1(i)*fl(I-1,J,irkim1(i,j,k)+1)                           DIFFMOI.852
                FTIM(I,K) = h2(i,k)-4*h1(i,k)+3*fl(I,J,K)                        DIFFMOI.853
                FITM(I,K) = -h2(i,k)-4*h1(i,k)+5*fl(I,J,K)                       DIFFMOI.854
                if (FITM(i,k)*FTIM(i,k).lt.ums*fl(i,j,k)*fl(i,j,k)) then         DIFFMOI.855
                  FTIM(I,K) = 2*(fl(I,J,K)-h1(i,k))                              DIFFMOI.856
                endif                                                            DIFFMOI.857
              else                                                               DIFFMOI.858
                FTIM(I,K) = 0                                                    DIFFMOI.859
              endif                                                              DIFFMOI.860
                                                                                 DIFFMOI.861
c  sigma-diffusion                                                               DIFFMOI.862
                                                                                 DIFFMOI.863
              IF ((gesi(i,k).gt.0.01).or.(gesj(i,k).gt.0.01)) then               DIFFMOI.864
                FTJ(I,K) = fl(I,J+2,K)+fl(I,J-2,K)-4*(fl(I,J+1,K)+               DIFFMOI.865
     &                     fl(I,J-1,K))+6*fl(I,J,K)                              DIFFMOI.866
                                                                                 DIFFMOI.867
                FTI(I,K) = fl(I+2,J,K)+fl(I-2,J,K)-4*(fl(I+1,J,K)+               DIFFMOI.868
     &                     fl(I-1,J,K))+6*fl(I,J,K)                              DIFFMOI.869
                                                                                 DIFFMOI.870
c  Check whether unstable mode of fourth-order diffusion may cause problems      DIFFMOI.871
                                                                                 DIFFMOI.872
                FJT(i,k) = FTJ(I,K)+4*fl(I,J,K)-2*(fl(I,J+2,K)+                  DIFFMOI.873
     &                     fl(I,J-2,K))                                          DIFFMOI.874
                                                                                 DIFFMOI.875
                FIT(i,k) = FTI(I,K)+4*fl(I,J,K)-2*(fl(I+2,J,K)+                  DIFFMOI.876
     &                     fl(I-2,J,K))                                          DIFFMOI.877
                                                                                 DIFFMOI.878
                                                                                 DIFFMOI.879
                if (FJT(i,k)*FTJ(i,k).lt.ums*fl(i,j,k)*fl(i,j,k)) then           DIFFMOI.880
                  FTJ(i,k) = 4*fl(I,J,K)-2*(fl(I,J+1,K)+fl(I,J-1,K))             DIFFMOI.881
                endif                                                            DIFFMOI.882
                                                                                 DIFFMOI.883
                if (FIT(i,k)*FTI(i,k).lt.ums*fl(i,j,k)*fl(i,j,k)) then           DIFFMOI.884
                  FTI(I,K) = 4*fl(I,J,K)-2*(fl(I+1,J,K)+fl(I-1,J,K))             DIFFMOI.885
                endif                                                            DIFFMOI.886
                                                                                 DIFFMOI.887
                                                                                 DIFFMOI.888
                FTIL(I,K) = fl(I,J,K)-fl(I-1,J,K)                                DIFFMOI.889
                FTJL(I,K) = fl(I,J,K)-fl(I,J-1,K)                                DIFFMOI.890
                FTIH(I,K) = fl(I,J,K)-fl(I+1,J,K)                                DIFFMOI.891
                FTJH(I,K) = fl(I,J,K)-fl(I,J+1,K)                                DIFFMOI.892
                                                                                 DIFFMOI.893
c                                                                                DIFFMOI.894
c                                                                                DIFFMOI.895
                FTEN(I,J,K)=FTEN(I,J,K)-XK(I,J,K)*C203*PSB(I,J)*(                DIFFMOI.896
     &             gesi(i,k)*(fin(i,j)*FTI(I,K)+                                 DIFFMOI.897
     &             min(100.,1./fin(i,j))/100.*max(0.,gesi(i,k)-0.75)*            DIFFMOI.898
     &             (fion(i,j)*FTIH(I,K)+fiun(i,j)*FTIL(I,K)))+                   DIFFMOI.899
     &             gesj(i,k)*(fjn(i,j)*FTJ(I,K)+                                 DIFFMOI.900
     &             min(100.,1./fjn(i,j))/100.*max(0.,gesj(i,k)-0.75)*            DIFFMOI.901
     &             (fjon(i,j)*FTJH(I,K)+fjun(i,j)*FTJL(I,K)))+(                  DIFFMOI.902
     &             gezip(i,j,k)*FTIP(I,K)+gezim(i,j,k)*FTIM(I,K)+                DIFFMOI.903
     &             gezjp(i,j,k)                                                  DIFFMOI.904
     &             *FTJP(I,K)+gezjm(i,j,k)*FTJM(I,K)))                           DIFFMOI.905
                                                                                 DIFFMOI.906
              ELSE                                                               DIFFMOI.907
                                                                                 DIFFMOI.908
                FTEN(I,J,K)=FTEN(I,J,K)-XK(I,J,K)*C203*PSB(I,J)*(                DIFFMOI.909
     &             gezip(i,j,k)*FTIP(I,K)+gezim(i,j,k)*FTIM(I,K)+                DIFFMOI.910
     &             gezjp(i,j,k)                                                  DIFFMOI.911
     &             *FTJP(I,K)+gezjm(i,j,k)*FTJM(I,K))                            DIFFMOI.912
                                                                                 DIFFMOI.913
              ENDIF                                                              DIFFMOI.914
                                                                                 DIFFMOI.915
            ENDDO                                                                DIFFMOI.916
          ENDIF                                                                  DIFFMOI.917
        ENDDO                                                                    DIFFMOI.918
                                                                                 DIFFMOI.919
      ENDDO  ! End of big j-loop                                                 DIFFMOI.920
C_FLIC_RUNPAD(0)                                                                 DIFFMOI.921
C                                                                                DIFFMOI.922
C.....SECOND-ORDER SCHEME FOR BOUNDARIES:                                        DIFFMOI.923
C                                                                                DIFFMOI.924
      IF(INEST.GT.1)RETURN                                                       DIFFMOI.925
cmic$ do all autoscope                                                           DIFFMOI.926
cmic$1 shared(IBGN,IEND,JBGN,JEND,KL,                                            DIFFMOI.927
cmic$1        F,FTEN,XK,PSB)                                                     DIFFMOI.928
cmic$2 private(I,J,K)                                                            DIFFMOI.929
c$doacross                                                                       DIFFMOI.930
c$& local(i,j,k)                                                                 DIFFMOI.931
c$omp parallel do default(shared)                                                DIFFMOI.932
#ifndef MPP1                                                                     DIFFMOI.933
c$omp&private(i,j,k)                                                             DIFFMOI.934
#else                                                                            DIFFMOI.935
c$omp&private(i,j,k,jflict_0,jflict_1,jflict_2,jflict_3,                         DIFFMOI.936
c$omp&        jflict_4,jflict_5,iflict_6,iflict_7,iflict_8,                      DIFFMOI.937
c$omp&        iflict_9,iflict_10,iflict_11)                                      DIFFMOI.938
#endif                                                                           DIFFMOI.939
C_FLIC_RUNPAD(2)                                                                 DIFFMOI.940
      DO K=1,KL                                                                  DIFFMOI.941
        J=JBGN                                                                   DIFFMOI.942
        DO I=IBGN,IEND                                                           DIFFMOI.943
          FTEN(I,J,K)=FTEN(I,J,K)+XK(I,J,K)*C203*PSB(I,J)*(F(I,J+1,K)+           DIFFMOI.944
     +                F(I,J-1,K)+F(I+1,J,K)+F(I-1,J,K)-4.*F(I,J,K))              DIFFMOI.945
        ENDDO                                                                    DIFFMOI.946
        J=JEND                                                                   DIFFMOI.947
        DO I=IBGN,IEND                                                           DIFFMOI.948
          FTEN(I,J,K)=FTEN(I,J,K)+XK(I,J,K)*C203*PSB(I,J)*(F(I,J+1,K)+           DIFFMOI.949
     +                F(I,J-1,K)+F(I+1,J,K)+F(I-1,J,K)-4.*F(I,J,K))              DIFFMOI.950
        ENDDO                                                                    DIFFMOI.951
        I=2                                                                      DIFFMOI.952
        DO J=JB,JE                                                               DIFFMOI.953
          FTEN(I,J,K)=FTEN(I,J,K)+XK(I,J,K)*C203*PSB(I,J)*(F(I,J+1,K)+           DIFFMOI.954
     +                F(I,J-1,K)+F(I+1,J,K)+F(I-1,J,K)-4.*F(I,J,K))              DIFFMOI.955
        ENDDO                                                                    DIFFMOI.956
        I=IEND                                                                   DIFFMOI.957
        DO J=JB,JE                                                               DIFFMOI.958
          FTEN(I,J,K)=FTEN(I,J,K)+XK(I,J,K)*C203*PSB(I,J)*(F(I,J+1,K)+           DIFFMOI.959
     +                F(I,J-1,K)+F(I+1,J,K)+F(I-1,J,K)-4.*F(I,J,K))              DIFFMOI.960
        ENDDO                                                                    DIFFMOI.961
      ENDDO                                                                      DIFFMOI.962
C                                                                                DIFFMOI.963
C_FLIC_RUNPAD(0)                                                                 DIFFMOI.964
      RETURN                                                                     DIFFMOI.965
      END                                                                        DIFFMOI.966
                                                                                 DIFFMOI.967
