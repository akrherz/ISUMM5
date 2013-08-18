      SUBROUTINE DIFFTHD(KZZ,FTEN,F,T,T0,PSB,XK,C203,IBGN,IEND,                  DIFFTHD.1
     & JBGN,JEND,KL,INEST,TGB,fin,fjn,deltaz,lam,kmv,rmn,                        DIFFTHD.2
     & kmi,kmj,kmip,kmim,kmjp,kmjm,                                              DIFFTHD.3
     & mrkip1,mrkip2,mrkim1,mrkim2,mrkjp1,mrkjp2,mrkjm1,mrkjm2,                  DIFFTHD.4
     & irkip1,irkip2,irkim1,irkim2,irkjp1,irkjp2,irkjm1,irkjm2,                  DIFFTHD.5
     & fjt04,fit04,fit0o,fit0u,fjt0o,fjt0u,fion,fiun,fjon,fjun,dz3d,             DIFFTHD.6
     & zdiffst)                                                                  DIFFTHD.7
      IMPLICIT NONE                                                              DIFFTHD.8
c                                                                                DIFFTHD.9
c  Truly horizontal temperature diffusion (see ZÄNGL, 2002, MWR 130, 1423-32)    DIFFTHD.10
c  Here, temperature diffusion uses theta as diffusion variable and              DIFFTHD.11
c  involves an empirical correction of vertical interpolation in the case        DIFFTHD.12
c  of height-dependent vertical temperature gradients 
c  (ZÄNGL, 2003, QJRMS 129, 117-137)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          DIFFTHD.14
C                                                                     C          DIFFTHD.15
C                                                                     C          DIFFTHD.16
C     THIS SUBROUTINE COMPUTES THE DIFFUSION TERM FOR DECOUPLED       C          DIFFTHD.17
C     VARIABLE ON CONSTANT SIGMA SURFACE.                             C          DIFFTHD.18
C                                                                     C          DIFFTHD.19
C                                                                     C          DIFFTHD.20
C     IYY, JXX, KZZ : DIMENSIONS                                      C          DIFFTHD.21
C                                                                     C          DIFFTHD.22
C     FTEN    : TENDENCY FOR VARIABLE FB                              C          DIFFTHD.23
C                                                                     C          DIFFTHD.24
C     F       : DECOUPLED VARIABLE AT TIME T-1  (potential temperature) C        DIFFTHD.25
C     PSB     : P* AT THE POINTS WHERE FB IS DEFINED                  C          DIFFTHD.26
C                                                                     C          DIFFTHD.27
C     XK      : HORIZONTAL DIFFUSION COEFFICIENT                      C          DIFFTHD.28
C                                                                     C          DIFFTHD.29
C     C203    : 1./(DX*DX), DEFINED IN 'PARAM'                        C          DIFFTHD.30
C                                                                     C          DIFFTHD.31
C     IEND    : = ILXM FOR CROSS-POINT VARIABLES                      C          DIFFTHD.32
C               = ILX  FOR DOT-POINT   VARIABLES                      C          DIFFTHD.33
C                                                                     C          DIFFTHD.34
C     JEND    : = JLXM FOR CROSS-POINT VARIABLES                      C          DIFFTHD.35
C               = JLX  FOR DOT-POINT   VARIABLES                      C          DIFFTHD.36
C                                                                     C          DIFFTHD.37
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          DIFFTHD.38
#       include <parame.incl>                                                    DIFFTHD.39
C                                                                                DIFFTHD.40
      INTEGER   KZZ,IBGN,IEND,JBGN,JEND,KL,INEST                                 DIFFTHD.41
      INTEGER   I,J,K,IB,JB,IE,JE,in,kmv,zdiffst(maxnes)                         DIFFTHD.42
      integer kmi(MIX,MJX),kmj(MIX,MJX)                                          DIFFTHD.43
      integer kmip(MIX,MJX,maxnes),kmjp(MIX,MJX,maxnes),                         DIFFTHD.44
     + kmim(MIX,MJX,maxnes),kmjm(MIX,MJX,maxnes)                                 DIFFTHD.45
      REAL  C203,rmn,deltaz(MIX,MJX,kzz-2:kzz),XK(MIX,MJX,MKX)                   DIFFTHD.46
      real psb(MIX,MJX),lam(MIX,MJX),fin(MIX,MJX),fjn(MIX,MJX),                  DIFFTHD.47
     + fjt04(MIX,MJX),fit04(MIX,MJX),fit0o(MIX,MJX),fit0u(MIX,MJX),              DIFFTHD.48
     + fjt0o(MIX,MJX),fjt0u(MIX,MJX),fion(MIX,MJX),fiun(MIX,MJX),                DIFFTHD.49
     + fjon(MIX,MJX),fjun(MIX,MJX),tgb(MIX,MJX)                                  DIFFTHD.50
      real ftj(MIX,kzz),fti(MIX,kzz),tfac(MIX,kzz)                               DIFFTHD.51
      real gezip(MIX,kmv+1:kzz),                                                 DIFFTHD.52
     + gezim(MIX,kmv+1:kzz),gezjp(MIX,kmv+1:kzz),gezjm(MIX,kmv+1:kzz),           DIFFTHD.53
     + gesi(MIX,kmv+1:kzz),gesj(MIX,kmv+1:kzz),ftip(MIX,kmv+1:kzz),              DIFFTHD.54
     + ftim(MIX,kmv+1:kzz),ftjp(MIX,kmv+1:kzz),ftjm(MIX,kmv+1:kzz),              DIFFTHD.55
     + ftil(MIX,kmv+1:kzz),ftih(MIX,kmv+1:kzz),ftjl(MIX,kmv+1:kzz),              DIFFTHD.56
     + ftjh(MIX,kmv+1:kzz)                                                       DIFFTHD.57
      real fit(MIX,kzz-1:kzz),fjt(MIX,kzz-1:kzz)                                 DIFFTHD.58
      real ften(MIX,MJX,KZZ),f(MIX,MJX,KZZ),t0(MIX,MJX,KZZ),                     DIFFTHD.59
     + dz3d(MIX,MJX,KZZ),t(MIX,MJX,KZZ),                                         DIFFTHD.60
     + mrkip1(MIX,MJX,KZZ),mrkip2(MIX,MJX,KZZ),mrkim1(MIX,MJX,KZZ),              DIFFTHD.61
     + mrkim2(MIX,MJX,KZZ),mrkjp1(MIX,MJX,KZZ),mrkjp2(MIX,MJX,KZZ),              DIFFTHD.62
     + mrkjm1(MIX,MJX,KZZ),mrkjm2(MIX,MJX,KZZ)                                   DIFFTHD.63
      real dtdz3d(mix,mjx,0:kzz+1),scal                                          DIFFTHD.64
      real wip1(MIX),wip2(MIX),wjp1(MIX),wjp2(MIX),wim1(MIX),                    DIFFTHD.65
     + wim2(MIX),wjm1(MIX),wjm2(MIX)                                             DIFFTHD.66
      real fl(MIX,MJX,KZZ+1)                                                     DIFFTHD.67
      integer irkip1(MIX,MJX,KZZ),irkip2(MIX,MJX,KZZ),                           DIFFTHD.68
     + irkim1(MIX,MJX,KZZ),irkim2(MIX,MJX,KZZ),irkjp1(MIX,MJX,KZZ),              DIFFTHD.69
     + irkjp2(MIX,MJX,KZZ),irkjm1(MIX,MJX,KZZ),irkjm2(MIX,MJX,KZZ)               DIFFTHD.70
                                                                                 DIFFTHD.71
                                                                                 DIFFTHD.72
                                                                                 DIFFTHD.73
C                                                                                DIFFTHD.74
C----------------------------------------------------------------------          DIFFTHD.75
C                                                                                DIFFTHD.76
C                                                                                DIFFTHD.77
C_FLIC_RUNPAD(2)                                                                 DIFFTHD.78
      IF(IBGN.GT.2)THEN                                                          DIFFTHD.79
        IB=IBGN                                                                  DIFFTHD.80
        JB=JBGN                                                                  DIFFTHD.81
        IE=IEND                                                                  DIFFTHD.82
        JE=JEND                                                                  DIFFTHD.83
      ELSE                                                                       DIFFTHD.84
        IB=IBGN+1                                                                DIFFTHD.85
        JB=JBGN+1                                                                DIFFTHD.86
        IE=IEND-1                                                                DIFFTHD.87
        JE=JEND-1                                                                DIFFTHD.88
      ENDIF                                                                      DIFFTHD.89
C                                                                                DIFFTHD.90
        in = inest                                                               DIFFTHD.91
        scal = 300                                                               DIFFTHD.92
                                                                                 DIFFTHD.93
c$omp parallel do default(shared)                                                DIFFTHD.94
c$omp&private(i,j,k)                                                             DIFFTHD.95
        DO J=1,JE+2                                                              DIFFTHD.96
        do i=1,ie+2                                                              DIFFTHD.97
        do k=1,kl                                                                DIFFTHD.98
        fl(i,j,k) = f(i,j,k)                                                     DIFFTHD.99
        enddo                                                                    DIFFTHD.100
        fl(i,j,kl+1) = f(i,j,kl)                                                 DIFFTHD.101
        enddo                                                                    DIFFTHD.102
        enddo                                                                    DIFFTHD.103
                                                                                 DIFFTHD.104
c$omp parallel do default(shared)                                                DIFFTHD.105
c$omp&private(i,j,k)                                                             DIFFTHD.106
        DO J=1,JE+2                                                              DIFFTHD.107
        do k=1,kl-1                                                              DIFFTHD.108
        do i=1,ie+2                                                              DIFFTHD.109
        dtdz3d(i,j,k)=min((fl(i,j,k)-fl(i,j,k+1))/dz3d(i,j,k),1.25e-2)           DIFFTHD.110
        enddo                                                                    DIFFTHD.111
        enddo                                                                    DIFFTHD.112
        enddo                                                                    DIFFTHD.113
                                                                                 DIFFTHD.114
c$omp parallel do default(shared)                                                DIFFTHD.115
c$omp&private(i,j)                                                               DIFFTHD.116
        DO J=1,JE+2                                                              DIFFTHD.117
        do i=1,ie+2                                                              DIFFTHD.118
         dtdz3d(i,j,0) = dtdz3d(i,j,1)                                           DIFFTHD.119
         dtdz3d(i,j,kl) = dtdz3d(i,j,kl-1)                                       DIFFTHD.120
         dtdz3d(i,j,kl+1) = dtdz3d(i,j,kl-1)                                     DIFFTHD.121
        enddo                                                                    DIFFTHD.122
        enddo                                                                    DIFFTHD.123
                                                                                 DIFFTHD.124
                                                                                 DIFFTHD.125
                                                                                 DIFFTHD.126
                                                                                 DIFFTHD.127
c                                                                                DIFFTHD.128
c  ** big j-loop                                                                 DIFFTHD.129
c                                                                                DIFFTHD.130
c$omp parallel do default(shared)                                                DIFFTHD.131
c$omp&private(i,j,k,gezip,gezim,gezjp,gezjm,gesi,gesj,                           DIFFTHD.132
c$omp&wip1,wip2,wim1,wim2,wjp1,wjp2,wjm1,wjm2,fti,ftj,tfac,                      DIFFTHD.133
c$omp&ftip,ftim,ftjp,ftjm,ftil,ftjl,ftih,ftjh,fit,fjt)                           DIFFTHD.134
      DO J=JB,JE                                                                 DIFFTHD.135
                                                                                 DIFFTHD.136
                                                                                 DIFFTHD.137
                                                                                 DIFFTHD.138
        do k=kmv+1,kl                                                            DIFFTHD.139
          do  i=ib,ie                                                            DIFFTHD.140
                                                                                 DIFFTHD.141
c  Weighting coefficients for z and sigma diffusion                              DIFFTHD.142
                                                                                 DIFFTHD.143
            gezip(i,k) = (1.-lam(i,j))*max(0,kmi(i,j)+1-k)/                      DIFFTHD.144
     &        float(kmi(i,j)-kmv+1)+lam(i,j)*(1-min(0.5,(k-kmv)/                 DIFFTHD.145
     &        (2.*(kmi(i,j)-kmv+1))))*                                           DIFFTHD.146
     &        min(1,max(0,kmip(i,j,in)+1-k))                                     DIFFTHD.147
            gezim(i,k) = (1.-lam(i,j))*max(0,kmi(i,j)+1-k)/                      DIFFTHD.148
     &        float(kmi(i,j)-kmv+1)+lam(i,j)*(1-min(0.5,(k-kmv)/                 DIFFTHD.149
     &        (2.*(kmi(i,j)-kmv+1))))*                                           DIFFTHD.150
     &        min(1,max(0,kmim(i,j,in)+1-k))                                     DIFFTHD.151
            gezjp(i,k) = (1.-lam(i,j))*max(0,kmj(i,j)+1-k)/                      DIFFTHD.152
     &        float(kmj(i,j)-kmv+1)+lam(i,j)*(1-min(0.5,(k-kmv)/                 DIFFTHD.153
     &        (2.*(kmj(i,j)-kmv+1))))*                                           DIFFTHD.154
     &        min(1,max(0,kmjp(i,j,in)+1-k))                                     DIFFTHD.155
            gezjm(i,k) = (1.-lam(i,j))*max(0,kmj(i,j)+1-k)/                      DIFFTHD.156
     &        float(kmj(i,j)-kmv+1)+lam(i,j)*(1-min(0.5,(k-kmv)/                 DIFFTHD.157
     &        (2.*(kmj(i,j)-kmv+1))))*                                           DIFFTHD.158
     &        min(1,max(0,kmjm(i,j,in)+1-k))                                     DIFFTHD.159
                                                                                 DIFFTHD.160
            gesi(i,k) = (1.-0.5*(gezip(i,k)+gezim(i,k)))*(                       DIFFTHD.161
     &        (1.-lam(i,j))*0.5+lam(i,j) )                                       DIFFTHD.162
            gesj(i,k) = (1.-0.5*(gezjp(i,k)+gezjm(i,k)))*(                       DIFFTHD.163
     &        (1.-lam(i,j))*0.5+lam(i,j) )                                       DIFFTHD.164
                                                                                 DIFFTHD.165
          enddo                                                                  DIFFTHD.166
        enddo                                                                    DIFFTHD.167
                                                                                 DIFFTHD.168
c  Compute diffusion along the sigma surfaces where they are flat enough         DIFFTHD.169
      do k=1,zdiffst(inest)-1                                                    DIFFTHD.170
      do I=ib,ie                                                                 DIFFTHD.171
      FTEN(I,J,K)=FTEN(I,J,K)-XK(I,J,K)*C203*PSB(I,J)*(F(I,J+2,K)+               DIFFTHD.172
     +  F(I,J-2,K)+F(I+2,J,K)+F(I-2,J,K)-4.*(F(I,J+1,K)+F(I,J-1,K)+              DIFFTHD.173
     +  F(I+1,J,K)+F(I-1,J,K))+12.*F(I,J,K))*t(i,j,k)/f(i,j,k)                   DIFFTHD.174
                                                                                 DIFFTHD.175
      enddo                                                                      DIFFTHD.176
      enddo                                                                      DIFFTHD.177
                                                                                 DIFFTHD.178
C.....Truly horizontal diffusion where possible without intersecting the topogra DIFFTHD.179
c     at any grid point                                                          DIFFTHD.180
C                                                                                DIFFTHD.181
                                                                                 DIFFTHD.182
        DO K=zdiffst(inest),kmv                                                  DIFFTHD.183
                                                                                 DIFFTHD.184
c  Correction factor for vertical interpolation in case of non-constant vertical DIFFTHD.185
c  Seems to improve the model's capability of keeping thin inversions above moun DIFFTHD.186
                                                                                 DIFFTHD.187
          do I=ib,ie                                                             DIFFTHD.188
            wjp2(i) = mrkjp2(i,j,k)-((1-mrkjp2(i,j,k))*                          DIFFTHD.189
     &                mrkjp2(i,j,k))**2*(dtdz3d(i,j,irkjp2(i,j,k)+1)-            DIFFTHD.190
     &                dtdz3d(i,j,irkjp2(i,j,k)-1))*scal                          DIFFTHD.191
            wjp2(i) = max(wjp2(i),0.)                                            DIFFTHD.192
            wjp2(i) = min(wjp2(i),1.)                                            DIFFTHD.193
            wjp1(i) = mrkjp1(i,j,k)-((1-mrkjp1(i,j,k))*                          DIFFTHD.194
     &                mrkjp1(i,j,k))**2*(dtdz3d(i,j,irkjp1(i,j,k)+1)-            DIFFTHD.195
     &                dtdz3d(i,j,irkjp1(i,j,k)-1))*scal                          DIFFTHD.196
            wjp1(i) = max(wjp1(i),0.)                                            DIFFTHD.197
            wjp1(i) = min(wjp1(i),1.)                                            DIFFTHD.198
            wjm2(i) = mrkjm2(i,j,k)-((1-mrkjm2(i,j,k))*                          DIFFTHD.199
     &                mrkjm2(i,j,k))**2*(dtdz3d(i,j,irkjm2(i,j,k)+1)-            DIFFTHD.200
     &                dtdz3d(i,j,irkjm2(i,j,k)-1))*scal                          DIFFTHD.201
            wjm2(i) = max(wjm2(i),0.)                                            DIFFTHD.202
            wjm2(i) = min(wjm2(i),1.)                                            DIFFTHD.203
            wjm1(i) = mrkjm1(i,j,k)-((1-mrkjm1(i,j,k))*                          DIFFTHD.204
     &                mrkjm1(i,j,k))**2*(dtdz3d(i,j,irkjm1(i,j,k)+1)-            DIFFTHD.205
     &                dtdz3d(i,j,irkjm1(i,j,k)-1))*scal                          DIFFTHD.206
            wjm1(i) = max(wjm1(i),0.)                                            DIFFTHD.207
            wjm1(i) = min(wjm1(i),1.)                                            DIFFTHD.208
            wip2(i) = mrkip2(i,j,k)-((1-mrkip2(i,j,k))*                          DIFFTHD.209
     &                mrkip2(i,j,k))**2*(dtdz3d(i,j,irkip2(i,j,k)+1)-            DIFFTHD.210
     &                dtdz3d(i,j,irkip2(i,j,k)-1))*scal                          DIFFTHD.211
            wip2(i) = max(wip2(i),0.)                                            DIFFTHD.212
            wip2(i) = min(wip2(i),1.)                                            DIFFTHD.213
            wip1(i) = mrkip1(i,j,k)-((1-mrkip1(i,j,k))*                          DIFFTHD.214
     &                mrkip1(i,j,k))**2*(dtdz3d(i,j,irkip1(i,j,k)+1)-            DIFFTHD.215
     &                dtdz3d(i,j,irkip1(i,j,k)-1))*scal                          DIFFTHD.216
            wip1(i) = max(wip1(i),0.)                                            DIFFTHD.217
            wip1(i) = min(wip1(i),1.)                                            DIFFTHD.218
            wim2(i) = mrkim2(i,j,k)-((1-mrkim2(i,j,k))*                          DIFFTHD.219
     &                mrkim2(i,j,k))**2*(dtdz3d(i,j,irkim2(i,j,k)+1)-            DIFFTHD.220
     &                dtdz3d(i,j,irkim2(i,j,k)-1))*scal                          DIFFTHD.221
            wim2(i) = max(wim2(i),0.)                                            DIFFTHD.222
            wim2(i) = min(wim2(i),1.)                                            DIFFTHD.223
            wim1(i) = mrkim1(i,j,k)-((1-mrkim1(i,j,k))*                          DIFFTHD.224
     &                mrkim1(i,j,k))**2*(dtdz3d(i,j,irkim1(i,j,k)+1)-            DIFFTHD.225
     &                dtdz3d(i,j,irkim1(i,j,k)-1))*scal                          DIFFTHD.226
            wim1(i) = max(wim1(i),0.)                                            DIFFTHD.227
            wim1(i) = min(wim1(i),1.)                                            DIFFTHD.228
          enddo                                                                  DIFFTHD.229
                                                                                 DIFFTHD.230
                                                                                 DIFFTHD.231
C                                                                                DIFFTHD.232
                                                                                 DIFFTHD.233
          DO I=ib,ie                                                             DIFFTHD.234
            FTJ(I,K) = (1-wjp2(i))*                                              DIFFTHD.235
     &          fl(I,J+2,irkjp2(i,j,k))+                                         DIFFTHD.236
     &          wjp2(i)*fl(I,J+2,irkjp2(i,j,k)+1)+                               DIFFTHD.237
     &          (1-wjm2(i))*fl(I,J-2,irkjm2(i,j,k))+                             DIFFTHD.238
     &          wjm2(i)*fl(I,J-2,irkjm2(i,j,k)+1)                                DIFFTHD.239
     &          -4*( (1-wjp1(i))*fl(I,J+1,irkjp1(i,j,k))+                        DIFFTHD.240
     &          wjp1(i)*fl(I,J+1,irkjp1(i,j,k)+1)+                               DIFFTHD.241
     &          (1-wjm1(i))*fl(I,J-1,irkjm1(i,j,k))+                             DIFFTHD.242
     &          wjm1(i)*fl(I,J-1,irkjm1(i,j,k)+1) )+                             DIFFTHD.243
     &          6*fl(I,J,K)                                                      DIFFTHD.244
                                                                                 DIFFTHD.245
            FTI(I,K) = (1-wip2(i))*                                              DIFFTHD.246
     &          fl(I+2,J,irkip2(i,j,k))+                                         DIFFTHD.247
     &          wip2(i)*fl(I+2,J,irkip2(i,j,k)+1)+                               DIFFTHD.248
     &          (1-wim2(i))*fl(I-2,J,irkim2(i,j,k))+                             DIFFTHD.249
     &          wim2(i)*fl(I-2,J,irkim2(i,j,k)+1)                                DIFFTHD.250
     &          -4*( (1-wip1(i))*fl(I+1,J,irkip1(i,j,k))+                        DIFFTHD.251
     &          wip1(i)*fl(I+1,J,irkip1(i,j,k)+1)+                               DIFFTHD.252
     &          (1-wim1(i))*fl(I-1,J,irkim1(i,j,k))+                             DIFFTHD.253
     &          wim1(i)*fl(I-1,J,irkim1(i,j,k)+1) )+                             DIFFTHD.254
     &          6*fl(I,J,K)                                                      DIFFTHD.255
                                                                                 DIFFTHD.256
                                                                                 DIFFTHD.257
             FTEN(I,J,K)=FTEN(I,J,K)-XK(I,J,K)*C203*PSB(I,J)*                    DIFFTHD.258
     &          (FTI(I,K)+FTJ(I,K))*t(i,j,k)/fl(i,j,k)                           DIFFTHD.259
                                                                                 DIFFTHD.260
          ENDDO                                                                  DIFFTHD.261
        ENDDO                                                                    DIFFTHD.262
                                                                                 DIFFTHD.263
c calculate the temperature gradient                                             DIFFTHD.264
                                                                                 DIFFTHD.267
        do I=ib,ie                                                               DIFFTHD.268
          tfac(i,kl) = (1-lam(i,j))*164*(fl(i,j,kl-1)-tgb(i,j))/                 DIFFTHD.269
     &      deltaz(i,j,kl)                                                       DIFFTHD.270
          tfac(i,kl-1) = (1-lam(i,j))*164*(fl(i,j,kl-2)-fl(i,j,kl))/             DIFFTHD.271
     &      deltaz(i,j,kl-1)                                                     DIFFTHD.272
          tfac(i,kl-2) = (1-lam(i,j))*164*(fl(i,j,kl-3)-fl(i,j,kl-1))/           DIFFTHD.273
     &      deltaz(i,j,kl-2)                                                     DIFFTHD.274
        enddo                                                                    DIFFTHD.275
                                                                                 DIFFTHD.276
        do k=kl-2,kl                                                             DIFFTHD.277
          do I=ib,ie                                                             DIFFTHD.278
            tfac(i,k) = max(tfac(i,k),0.)   ! no superadiabatic gradients        DIFFTHD.279
            tfac(i,k) = min(tfac(i,k),6.5)   ! no gradients above +3K/100m to av DIFFTHD.280
          enddo                                                                  DIFFTHD.281
        enddo                                                                    DIFFTHD.282
                                                                                 DIFFTHD.283
        do k=kmv+1,kl-3                                                          DIFFTHD.284
          do I=ib,ie                                                             DIFFTHD.285
            tfac(i,k) = 0.6 + (tfac(i,kl-2)-0.6)*(k-kmv)/(kl-2-kmv)              DIFFTHD.286
          enddo                                                                  DIFFTHD.287
        enddo                                                                    DIFFTHD.288
c                                                                                DIFFTHD.289
c                                                                                DIFFTHD.290
        DO K=kmv+1,kl-2     ! Model levels where the orography may be intersecte DIFFTHD.291
                                                                                 DIFFTHD.292
                                                                                 DIFFTHD.293
          do I=ib,ie                                                             DIFFTHD.294
            wjp2(i) = mrkjp2(i,j,k)-((1-mrkjp2(i,j,k))*                          DIFFTHD.295
     &                mrkjp2(i,j,k))**2*(dtdz3d(i,j,irkjp2(i,j,k)+1)-            DIFFTHD.296
     &                dtdz3d(i,j,irkjp2(i,j,k)-1))*scal                          DIFFTHD.297
            wjp2(i) = max(wjp2(i),0.)                                            DIFFTHD.298
            wjp2(i) = min(wjp2(i),1.)                                            DIFFTHD.299
            wjp1(i) = mrkjp1(i,j,k)-((1-mrkjp1(i,j,k))*                          DIFFTHD.300
     &                mrkjp1(i,j,k))**2*(dtdz3d(i,j,irkjp1(i,j,k)+1)-            DIFFTHD.301
     &                dtdz3d(i,j,irkjp1(i,j,k)-1))*scal                          DIFFTHD.302
            wjp1(i) = max(wjp1(i),0.)                                            DIFFTHD.303
            wjp1(i) = min(wjp1(i),1.)                                            DIFFTHD.304
            wjm2(i) = mrkjm2(i,j,k)-((1-mrkjm2(i,j,k))*                          DIFFTHD.305
     &                mrkjm2(i,j,k))**2*(dtdz3d(i,j,irkjm2(i,j,k)+1)-            DIFFTHD.306
     &                dtdz3d(i,j,irkjm2(i,j,k)-1))*scal                          DIFFTHD.307
            wjm2(i) = max(wjm2(i),0.)                                            DIFFTHD.308
            wjm2(i) = min(wjm2(i),1.)                                            DIFFTHD.309
            wjm1(i) = mrkjm1(i,j,k)-((1-mrkjm1(i,j,k))*                          DIFFTHD.310
     &                mrkjm1(i,j,k))**2*(dtdz3d(i,j,irkjm1(i,j,k)+1)-            DIFFTHD.311
     &                dtdz3d(i,j,irkjm1(i,j,k)-1))*scal                          DIFFTHD.312
            wjm1(i) = max(wjm1(i),0.)                                            DIFFTHD.313
            wjm1(i) = min(wjm1(i),1.)                                            DIFFTHD.314
            wip2(i) = mrkip2(i,j,k)-((1-mrkip2(i,j,k))*                          DIFFTHD.315
     &                mrkip2(i,j,k))**2*(dtdz3d(i,j,irkip2(i,j,k)+1)-            DIFFTHD.316
     &                dtdz3d(i,j,irkip2(i,j,k)-1))*scal                          DIFFTHD.317
            wip2(i) = max(wip2(i),0.)                                            DIFFTHD.318
            wip2(i) = min(wip2(i),1.)                                            DIFFTHD.319
            wip1(i) = mrkip1(i,j,k)-((1-mrkip1(i,j,k))*                          DIFFTHD.320
     &                mrkip1(i,j,k))**2*(dtdz3d(i,j,irkip1(i,j,k)+1)-            DIFFTHD.321
     &                dtdz3d(i,j,irkip1(i,j,k)-1))*scal                          DIFFTHD.322
            wip1(i) = max(wip1(i),0.)                                            DIFFTHD.323
            wip1(i) = min(wip1(i),1.)                                            DIFFTHD.324
            wim2(i) = mrkim2(i,j,k)-((1-mrkim2(i,j,k))*                          DIFFTHD.325
     &                mrkim2(i,j,k))**2*(dtdz3d(i,j,irkim2(i,j,k)+1)-            DIFFTHD.326
     &                dtdz3d(i,j,irkim2(i,j,k)-1))*scal                          DIFFTHD.327
            wim2(i) = max(wim2(i),0.)                                            DIFFTHD.328
            wim2(i) = min(wim2(i),1.)                                            DIFFTHD.329
            wim1(i) = mrkim1(i,j,k)-((1-mrkim1(i,j,k))*                          DIFFTHD.330
     &                mrkim1(i,j,k))**2*(dtdz3d(i,j,irkim1(i,j,k)+1)-            DIFFTHD.331
     &                dtdz3d(i,j,irkim1(i,j,k)-1))*scal                          DIFFTHD.332
            wim1(i) = max(wim1(i),0.)                                            DIFFTHD.333
            wim1(i) = min(wim1(i),1.)                                            DIFFTHD.334
          enddo                                                                  DIFFTHD.335
                                                                                 DIFFTHD.336
          DO I=ib,ie                                                             DIFFTHD.337
                                                                                 DIFFTHD.338
c  truly horizontal diffusion, the one-sided approximations are needed in the st DIFFTHD.339
                                                                                 DIFFTHD.340
          if (gezjp(i,k).gt.0.01) then                                           DIFFTHD.341
            FTJP(I,K) = (1-wjp2(i))*                                             DIFFTHD.342
     &           fl(I,J+2,irkjp2(i,j,k))+                                        DIFFTHD.343
     &           wjp2(i)*fl(I,J+2,irkjp2(i,j,k)+1)                               DIFFTHD.344
     &           -4*( (1-wjp1(i))*fl(I,J+1,irkjp1(i,j,k))+                       DIFFTHD.345
     &           wjp1(i)*fl(I,J+1,irkjp1(i,j,k)+1)                               DIFFTHD.346
     &           )+3*fl(I,J,K)                                                   DIFFTHD.347
          else                                                                   DIFFTHD.348
            FTJP(I,K) = 0                                                        DIFFTHD.349
          endif                                                                  DIFFTHD.350
                                                                                 DIFFTHD.351
          if (gezip(i,k).gt.0.01) then                                           DIFFTHD.352
            FTIP(I,K) = (1-wip2(i))*                                             DIFFTHD.353
     &           fl(I+2,J,irkip2(i,j,k))+                                        DIFFTHD.354
     &           wip2(i)*fl(I+2,J,irkip2(i,j,k)+1)                               DIFFTHD.355
     &           -4*( (1-wip1(i))*fl(I+1,J,irkip1(i,j,k))+                       DIFFTHD.356
     &           wip1(i)*fl(I+1,J,irkip1(i,j,k)+1)                               DIFFTHD.357
     &           )+3*fl(I,J,K)                                                   DIFFTHD.358
          else                                                                   DIFFTHD.359
            FTIP(I,K) = 0                                                        DIFFTHD.360
          endif                                                                  DIFFTHD.361
                                                                                 DIFFTHD.362
          if (gezjm(i,k).gt.0.01) then                                           DIFFTHD.363
            FTJM(I,K) = (1-wjm2(i))*                                             DIFFTHD.364
     &           fl(I,J-2,irkjm2(i,j,k))+                                        DIFFTHD.365
     &           wjm2(i)*fl(I,J-2,irkjm2(i,j,k)+1)                               DIFFTHD.366
     &           -4*( (1-wjm1(i))*fl(I,J-1,irkjm1(i,j,k))+                       DIFFTHD.367
     &           wjm1(i)*fl(I,J-1,irkjm1(i,j,k)+1)                               DIFFTHD.368
     &           )+3*fl(I,J,K)                                                   DIFFTHD.369
          else                                                                   DIFFTHD.370
            FTJM(I,K) = 0                                                        DIFFTHD.371
          endif                                                                  DIFFTHD.372
                                                                                 DIFFTHD.373
          if (gezim(i,k).gt.0.01) then                                           DIFFTHD.374
            FTIM(I,K) = (1-wim2(i))*                                             DIFFTHD.375
     &           fl(I-2,J,irkim2(i,j,k))+                                        DIFFTHD.376
     &           wim2(i)*fl(I-2,J,irkim2(i,j,k)+1)                               DIFFTHD.377
     &           -4*( (1-wim1(i))*fl(I-1,J,irkim1(i,j,k))+                       DIFFTHD.378
     &           wim1(i)*fl(I-1,J,irkim1(i,j,k)+1)                               DIFFTHD.379
     &           )+3*fl(I,J,K)                                                   DIFFTHD.380
          else                                                                   DIFFTHD.381
            FTIM(I,K) = 0                                                        DIFFTHD.382
          endif                                                                  DIFFTHD.383
                                                                                 DIFFTHD.384
c  Centered sigma-diffusion with temperature gradient correction                 DIFFTHD.385
                                                                                 DIFFTHD.386
          if ((gesi(i,k).gt.0.01).or.(gesj(i,k).gt.0.01)) then                   DIFFTHD.387
            FTJ(I,K) = fl(I,J+2,K)+fl(I,J-2,K)-4*(fl(I,J+1,K)+                   DIFFTHD.388
     &          fl(I,J-1,K))+6*fl(I,J,K) + tfac(i,k)*fjt04(i,j)                  DIFFTHD.389
                                                                                 DIFFTHD.390
            FTI(I,K) = fl(I+2,J,K)+fl(I-2,J,K)-4*(fl(I+1,J,K)+                   DIFFTHD.391
     &          fl(I-1,J,K))+6*fl(I,J,K) + tfac(i,k)*fit04(i,j)                  DIFFTHD.392
                                                                                 DIFFTHD.393
c                                                                                DIFFTHD.394
c  Subsidiary one-sided second-order diffusion where centered sigma-diffusion is DIFFTHD.395
c  (recently changed; in the paper, it is centered second-order diffusion)       DIFFTHD.396
                                                                                 DIFFTHD.397
            FTIL(I,K) = fl(I,J,K)-fl(I-1,J,K)+tfac(i,k)*fit0u(i,j)               DIFFTHD.398
            FTJL(I,K) = fl(I,J,K)-fl(I,J-1,K)+tfac(i,k)*fjt0u(i,j)               DIFFTHD.399
            FTIH(I,K) = fl(I,J,K)-fl(I+1,J,K)+tfac(i,k)*fit0o(i,j)               DIFFTHD.400
            FTJH(I,K) = fl(I,J,K)-fl(I,J+1,K)+tfac(i,k)*fjt0o(i,j)               DIFFTHD.401
c                                                                                DIFFTHD.402
c   Summing up all the diffusion terms                                           DIFFTHD.403
c                                                                                DIFFTHD.404
            FTEN(I,J,K)=FTEN(I,J,K)-XK(I,J,K)*t(i,j,k)/fl(i,j,k)*C203*           DIFFTHD.405
     &           PSB(I,J)*(gesi(i,k)*(fin(i,j)*FTI(I,K)+                         DIFFTHD.406
     &           min(100.,1./fin(i,j))/100.*(1-lam(i,j))*                        DIFFTHD.407
     &           (fion(i,j)*FTIH(I,K)+fiun(i,j)*FTIL(I,K)))+                     DIFFTHD.408
     &           gesj(i,k)*(fjn(i,j)*FTJ(I,K)+min(100.,1./                       DIFFTHD.409
     &           fjn(i,j))/100.*(1-lam(i,j))*(fjon(i,j)*FTJH(I,K)+               DIFFTHD.410
     &           fjun(i,j)*FTJL(I,K)))+gezip(i,k)*FTIP(I,K)+                     DIFFTHD.411
     &           gezim(i,k)*FTIM(I,K)+gezjp(i,k)*FTJP(I,K)+                      DIFFTHD.412
     &           gezjm(i,k)*FTJM(I,K))                                           DIFFTHD.413
                                                                                 DIFFTHD.414
          else                                                                   DIFFTHD.415
                                                                                 DIFFTHD.416
            FTEN(I,J,K)=FTEN(I,J,K)-XK(I,J,K)*t(i,j,k)/fl(i,j,k)*C203*           DIFFTHD.417
     &        PSB(I,J)*(gezip(i,k)*FTIP(I,K)+gezim(i,k)*FTIM(I,K)+               DIFFTHD.418
     &        gezjp(i,k)*FTJP(I,K)+gezjm(i,k)*FTJM(I,K))                         DIFFTHD.419
                                                                                 DIFFTHD.420
          endif                                                                  DIFFTHD.421
                                                                                 DIFFTHD.422
          ENDDO                                                                  DIFFTHD.423
        ENDDO                                                                    DIFFTHD.424
                                                                                 DIFFTHD.425
        DO K=max(kmv+1,kl-1),kl     ! Lowermost two levels need an additional co DIFFTHD.426
          DO I=ib,ie      ! because of unstable mode of fourth-order diffusion   DIFFTHD.427
                                                                                 DIFFTHD.428
                                                                                 DIFFTHD.429
            if (gezjp(i,k).gt.0.01) then                                         DIFFTHD.430
              FTJP(I,K) = (1-mrkjp2(i,j,k))*                                     DIFFTHD.431
     &           fl(I,J+2,irkjp2(i,j,k))+                                        DIFFTHD.432
     &           mrkjp2(i,j,k)*fl(I,J+2,irkjp2(i,j,k)+1)                         DIFFTHD.433
     &           -4*( (1-mrkjp1(i,j,k))*fl(I,J+1,irkjp1(i,j,k))+                 DIFFTHD.434
     &           mrkjp1(i,j,k)*fl(I,J+1,irkjp1(i,j,k)+1)                         DIFFTHD.435
     &           )+3*fl(I,J,K)                                                   DIFFTHD.436
            else                                                                 DIFFTHD.437
              FTJP(I,K) = 0                                                      DIFFTHD.438
            endif                                                                DIFFTHD.439
                                                                                 DIFFTHD.440
            if (gezip(i,k).gt.0.01) then                                         DIFFTHD.441
              FTIP(I,K) = (1-mrkip2(i,j,k))*                                     DIFFTHD.442
     &           fl(I+2,J,irkip2(i,j,k))+                                        DIFFTHD.443
     &           mrkip2(i,j,k)*fl(I+2,J,irkip2(i,j,k)+1)                         DIFFTHD.444
     &           -4*( (1-mrkip1(i,j,k))*fl(I+1,J,irkip1(i,j,k))+                 DIFFTHD.445
     &           mrkip1(i,j,k)*fl(I+1,J,irkip1(i,j,k)+1)                         DIFFTHD.446
     &           )+3*fl(I,J,K)                                                   DIFFTHD.447
            else                                                                 DIFFTHD.448
              FTIP(I,K) = 0                                                      DIFFTHD.449
            endif                                                                DIFFTHD.450
                                                                                 DIFFTHD.451
            if (gezjm(i,k).gt.0.01) then                                         DIFFTHD.452
              FTJM(I,K) = (1-mrkjm2(i,j,k))*                                     DIFFTHD.453
     &           fl(I,J-2,irkjm2(i,j,k))+                                        DIFFTHD.454
     &           mrkjm2(i,j,k)*fl(I,J-2,irkjm2(i,j,k)+1)                         DIFFTHD.455
     &           -4*( (1-mrkjm1(i,j,k))*fl(I,J-1,irkjm1(i,j,k))+                 DIFFTHD.456
     &           mrkjm1(i,j,k)*fl(I,J-1,irkjm1(i,j,k)+1)                         DIFFTHD.457
     &           )+3*fl(I,J,K)                                                   DIFFTHD.458
            else                                                                 DIFFTHD.459
              FTJM(I,K) = 0                                                      DIFFTHD.460
            endif                                                                DIFFTHD.461
                                                                                 DIFFTHD.462
            if (gezim(i,k).gt.0.01) then                                         DIFFTHD.463
              FTIM(I,K) = (1-mrkim2(i,j,k))*                                     DIFFTHD.464
     &           fl(I-2,J,irkim2(i,j,k))+                                        DIFFTHD.465
     &           mrkim2(i,j,k)*fl(I-2,J,irkim2(i,j,k)+1)                         DIFFTHD.466
     &           -4*( (1-mrkim1(i,j,k))*fl(I-1,J,irkim1(i,j,k))+                 DIFFTHD.467
     &           mrkim1(i,j,k)*fl(I-1,J,irkim1(i,j,k)+1)                         DIFFTHD.468
     &           )+3*fl(I,J,K)                                                   DIFFTHD.469
            else                                                                 DIFFTHD.470
              FTIM(I,K) = 0                                                      DIFFTHD.471
            endif                                                                DIFFTHD.472
                                                                                 DIFFTHD.473
                                                                                 DIFFTHD.474
            FTJ(I,K) = fl(I,J+2,K)+fl(I,J-2,K)-4*(fl(I,J+1,K)+                   DIFFTHD.475
     &                 fl(I,J-1,K))+6*fl(I,J,K)+tfac(i,k)*fjt04(i,j)             DIFFTHD.476
                                                                                 DIFFTHD.477
            FTI(I,K) = fl(I+2,J,K)+fl(I-2,J,K)-4*(fl(I+1,J,K)+                   DIFFTHD.478
     &                 fl(I-1,J,K))+6*fl(I,J,K)+tfac(i,k)*fit04(i,j)             DIFFTHD.479
                                                                                 DIFFTHD.480
c  Check whether fourth-order diffusion yields a wrong-signed tendency           DIFFTHD.481
                                                                                 DIFFTHD.482
            FJT(i,k) = FTJ(I,K)+4*fl(I,J,K)-2*(fl(I,J+2,K)+                      DIFFTHD.483
     &                 fl(I,J-2,K))+tfac(i,k)*(4*T0(I,J,K)-2*                    DIFFTHD.484
     &                 (T0(I,J+2,K)+T0(I,J-2,K)))                                DIFFTHD.485
                                                                                 DIFFTHD.486
            FIT(i,k) = FTI(I,K)+4*fl(I,J,K)-2*(fl(I+2,J,K)+                      DIFFTHD.487
     &                 fl(I-2,J,K))+tfac(i,k)*(4*T0(I,J,K)-2*                    DIFFTHD.488
     &                 (T0(I+2,J,K)+T0(I-2,J,K)))                                DIFFTHD.489
                                                                                 DIFFTHD.490
c  If this turns out to be the case, second-order diffusion is applied           DIFFTHD.491
                                                                                 DIFFTHD.492
            if (FJT(i,k)*FTJ(i,k).lt.-0.2) then                                  DIFFTHD.493
              FTJ(i,k) = 4*fl(I,J,K)-2*(fl(I,J+1,K)+fl(I,J-1,K))                 DIFFTHD.494
     &            +tfac(i,k)*(4*T0(I,J,K)-2*(T0(I,J+1,K)+T0(I,J-1,K)))           DIFFTHD.495
            endif                                                                DIFFTHD.496
                                                                                 DIFFTHD.497
            if (FIT(i,k)*FTI(i,k).lt.-0.2) then                                  DIFFTHD.498
              FTI(I,K) = 4*fl(I,J,K)-2*(fl(I+1,J,K)+fl(I-1,J,K))                 DIFFTHD.499
     &            +tfac(i,k)*(4*T0(I,J,K)-2*(T0(I+1,J,K)+T0(I-1,J,K)))           DIFFTHD.500
            endif                                                                DIFFTHD.501
                                                                                 DIFFTHD.502
c                                                                                DIFFTHD.503
            FTIL(I,K) = fl(I,J,K)-fl(I-1,J,K)+tfac(i,k)*fit0u(i,j)               DIFFTHD.504
            FTJL(I,K) = fl(I,J,K)-fl(I,J-1,K)+tfac(i,k)*fjt0u(i,j)               DIFFTHD.505
            FTIH(I,K) = fl(I,J,K)-fl(I+1,J,K)+tfac(i,k)*fit0o(i,j)               DIFFTHD.506
            FTJH(I,K) = fl(I,J,K)-fl(I,J+1,K)+tfac(i,k)*fjt0o(i,j)               DIFFTHD.507
c                                                                                DIFFTHD.508
c                                                                                DIFFTHD.509
            FTEN(I,J,K)=FTEN(I,J,K)-XK(I,J,K)*t(i,j,k)/fl(i,j,k)*C203*           DIFFTHD.510
     &        PSB(I,J)*(gesi(i,k)*(fin(i,j)*FTI(I,K)+                            DIFFTHD.511
     &        min(100.,1./fin(i,j))/100.*(1-lam(i,j))*(fion(i,j)*                DIFFTHD.512
     &        FTIH(I,K)+fiun(i,j)*FTIL(I,K)))+gesj(i,k)*(fjn(i,j)*               DIFFTHD.513
     &        FTJ(I,K)+min(100.,1./fjn(i,j))/100.*(1-lam(i,j))*                  DIFFTHD.514
     &        (fjon(i,j)*FTJH(I,K)+fjun(i,j)*FTJL(I,K)))+                        DIFFTHD.515
     &        gezip(i,k)*FTIP(I,K)+gezim(i,k)*FTIM(I,K)+gezjp(i,k)*              DIFFTHD.516
     &        FTJP(I,K)+gezjm(i,k)*FTJM(I,K))                                    DIFFTHD.517
                                                                                 DIFFTHD.518
          ENDDO                                                                  DIFFTHD.519
        ENDDO                                                                    DIFFTHD.520
                                                                                 DIFFTHD.521
                                                                                 DIFFTHD.522
      ENDDO   ! ** End of outer j-loop                                           DIFFTHD.523
C_FLIC_RUNPAD(0)                                                                 DIFFTHD.524
C                                                                                DIFFTHD.525
C.....SECOND-ORDER SCHEME FOR BOUNDARIES:                                        DIFFTHD.526
C                                                                                DIFFTHD.527
      IF(INEST.GT.1)RETURN                                                       DIFFTHD.528
cmic$ do all autoscope                                                           DIFFTHD.529
cmic$1 shared(IBGN,IEND,JBGN,JEND,KL,                                            DIFFTHD.530
cmic$1        F,FTEN,XK,PSB)                                                     DIFFTHD.531
cmic$2 private(I,J,K)                                                            DIFFTHD.532
c$doacross                                                                       DIFFTHD.533
c$& local(i,j,k)                                                                 DIFFTHD.534
c$omp parallel do default(shared)                                                DIFFTHD.535
#ifndef MPP1                                                                     DIFFTHD.536
c$omp&private(i,j,k)                                                             DIFFTHD.537
#else                                                                            DIFFTHD.538
c$omp&private(i,j,k,jflict_0,jflict_1,jflict_2,jflict_3,                         DIFFTHD.539
c$omp&        jflict_4,jflict_5,iflict_6,iflict_7,iflict_8,                      DIFFTHD.540
c$omp&        iflict_9,iflict_10,iflict_11)                                      DIFFTHD.541
#endif                                                                           DIFFTHD.542
C_FLIC_RUNPAD(2)                                                                 DIFFTHD.543
      DO K=1,KL                                                                  DIFFTHD.544
        J=JBGN                                                                   DIFFTHD.545
        DO I=IBGN,IEND                                                           DIFFTHD.546
          FTEN(I,J,K)=FTEN(I,J,K)+XK(I,J,K)*C203*PSB(I,J)*(F(I,J+1,K)+           DIFFTHD.547
     +                F(I,J-1,K)+F(I+1,J,K)+F(I-1,J,K)-4.*F(I,J,K))              DIFFTHD.548
        ENDDO                                                                    DIFFTHD.549
        J=JEND                                                                   DIFFTHD.550
        DO I=IBGN,IEND                                                           DIFFTHD.551
          FTEN(I,J,K)=FTEN(I,J,K)+XK(I,J,K)*C203*PSB(I,J)*(F(I,J+1,K)+           DIFFTHD.552
     +                F(I,J-1,K)+F(I+1,J,K)+F(I-1,J,K)-4.*F(I,J,K))              DIFFTHD.553
        ENDDO                                                                    DIFFTHD.554
        I=2                                                                      DIFFTHD.555
        DO J=JB,JE                                                               DIFFTHD.556
          FTEN(I,J,K)=FTEN(I,J,K)+XK(I,J,K)*C203*PSB(I,J)*(F(I,J+1,K)+           DIFFTHD.557
     +                F(I,J-1,K)+F(I+1,J,K)+F(I-1,J,K)-4.*F(I,J,K))              DIFFTHD.558
        ENDDO                                                                    DIFFTHD.559
        I=IEND                                                                   DIFFTHD.560
        DO J=JB,JE                                                               DIFFTHD.561
          FTEN(I,J,K)=FTEN(I,J,K)+XK(I,J,K)*C203*PSB(I,J)*(F(I,J+1,K)+           DIFFTHD.562
     +                F(I,J-1,K)+F(I+1,J,K)+F(I-1,J,K)-4.*F(I,J,K))              DIFFTHD.563
        ENDDO                                                                    DIFFTHD.564
      ENDDO                                                                      DIFFTHD.565
C                                                                                DIFFTHD.566
C_FLIC_RUNPAD(0)                                                                 DIFFTHD.567
      RETURN                                                                     DIFFTHD.568
      END                                                                        DIFFTHD.569
                                                                                 DIFFTHD.570
