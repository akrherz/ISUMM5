      SUBROUTINE DIFFTH(KZZ,FTEN,F,T,T0,PSB,XK,C203,IBGN,IEND,                   DIFFTH.1
     & JBGN,JEND,KL,INEST,CLW,TGB,fin,fjn,deltaz,lam,kmv,rmn,                    DIFFTH.2
     & kmi,kmj,kmip,kmim,kmjp,kmjm,                                              DIFFTH.3
     & mrkip1,mrkip2,mrkim1,mrkim2,mrkjp1,mrkjp2,mrkjm1,mrkjm2,                  DIFFTH.4
     & irkip1,irkip2,irkim1,irkim2,irkjp1,irkjp2,irkjm1,irkjm2,                  DIFFTH.5
     & fjt04,fit04,fit0o,fit0u,fjt0o,fjt0u,fion,fiun,fjon,fjun,dz3d,             DIFFTH.6
     & zdiffst)                                                                  DIFFTH.7
      IMPLICIT NONE                                                              DIFFTH.8
c                                                                                DIFFTH.9
c  Truly horizontal temperature diffusion (see ZÄNGL, 2002, MWR 130, 1423-32)    DIFFTH.10
c  Here, temperature diffusion uses theta as diffusion variable and              DIFFTH.11
c  involves an empirical correction of vertical interpolation in the case        DIFFTH.12
c  of height-dependent vertical temperature gradients 
c  (ZÄNGL, 2003, QJRMS 129, 117-137)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          DIFFTH.14
C                                                                     C          DIFFTH.15
C                                                                     C          DIFFTH.16
C     THIS SUBROUTINE COMPUTES THE DIFFUSION TERM FOR DECOUPLED       C          DIFFTH.17
C     VARIABLE ON CONSTANT SIGMA SURFACE.                             C          DIFFTH.18
C                                                                     C          DIFFTH.19
C                                                                     C          DIFFTH.20
C     IYY, JXX, KZZ : DIMENSIONS                                      C          DIFFTH.21
C                                                                     C          DIFFTH.22
C     FTEN    : TENDENCY FOR VARIABLE FB                              C          DIFFTH.23
C                                                                     C          DIFFTH.24
C     F       : DECOUPLED VARIABLE AT TIME T-1  (potential temperature) C        DIFFTH.25
C     PSB     : P* AT THE POINTS WHERE FB IS DEFINED                  C          DIFFTH.26
C                                                                     C          DIFFTH.27
C     XK      : HORIZONTAL DIFFUSION COEFFICIENT                      C          DIFFTH.28
C                                                                     C          DIFFTH.29
C     C203    : 1./(DX*DX), DEFINED IN 'PARAM'                        C          DIFFTH.30
C                                                                     C          DIFFTH.31
C     IEND    : = ILXM FOR CROSS-POINT VARIABLES                      C          DIFFTH.32
C               = ILX  FOR DOT-POINT   VARIABLES                      C          DIFFTH.33
C                                                                     C          DIFFTH.34
C     JEND    : = JLXM FOR CROSS-POINT VARIABLES                      C          DIFFTH.35
C               = JLX  FOR DOT-POINT   VARIABLES                      C          DIFFTH.36
C                                                                     C          DIFFTH.37
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          DIFFTH.38
#       include <parame.incl>                                                    DIFFTH.39
C                                                                                DIFFTH.40
      INTEGER   KZZ,IBGN,IEND,JBGN,JEND,KL,INEST                                 DIFFTH.41
      INTEGER   I,J,K,IB,JB,IE,JE,in,kmv,zdiffst(maxnes)                         DIFFTH.42
      integer kmi(MIX,MJX),kmj(MIX,MJX)                                          DIFFTH.43
      integer kmip(MIX,MJX,maxnes),kmjp(MIX,MJX,maxnes),                         DIFFTH.44
     + kmim(MIX,MJX,maxnes),kmjm(MIX,MJX,maxnes)                                 DIFFTH.45
      REAL  C203,rmn,deltaz(MIX,MJX,kzz-2:kzz),XK(MIX,MJX,MKX)                   DIFFTH.46
      real psb(MIX,MJX),lam(MIX,MJX),fin(MIX,MJX),fjn(MIX,MJX),                  DIFFTH.47
     + fjt04(MIX,MJX),fit04(MIX,MJX),fit0o(MIX,MJX),fit0u(MIX,MJX),              DIFFTH.48
     + fjt0o(MIX,MJX),fjt0u(MIX,MJX),fion(MIX,MJX),fiun(MIX,MJX),                DIFFTH.49
     + fjon(MIX,MJX),fjun(MIX,MJX),tgb(MIX,MJX)                                  DIFFTH.50
      real ftj(MIX,kzz),fti(MIX,kzz),tfac(MIX,kzz)                               DIFFTH.51
      real l2(MIX,kmv+1:kzz),gezip(MIX,kmv+1:kzz),                               DIFFTH.52
     + gezim(MIX,kmv+1:kzz),gezjp(MIX,kmv+1:kzz),gezjm(MIX,kmv+1:kzz),           DIFFTH.53
     + gesi(MIX,kmv+1:kzz),gesj(MIX,kmv+1:kzz),ftip(MIX,kmv+1:kzz),              DIFFTH.54
     + ftim(MIX,kmv+1:kzz),ftjp(MIX,kmv+1:kzz),ftjm(MIX,kmv+1:kzz),              DIFFTH.55
     + ftil(MIX,kmv+1:kzz),ftih(MIX,kmv+1:kzz),ftjl(MIX,kmv+1:kzz),              DIFFTH.56
     + ftjh(MIX,kmv+1:kzz)                                                       DIFFTH.57
      real fit(MIX,kzz-1:kzz),fjt(MIX,kzz-1:kzz)                                 DIFFTH.58
      real ften(MIX,MJX,KZZ),f(MIX,MJX,KZZ),t0(MIX,MJX,KZZ),                     DIFFTH.59
     + clw(MIX,MJX,KZZ),dz3d(MIX,MJX,KZZ),t(MIX,MJX,KZZ),                        DIFFTH.60
     + mrkip1(MIX,MJX,KZZ),mrkip2(MIX,MJX,KZZ),mrkim1(MIX,MJX,KZZ),              DIFFTH.61
     + mrkim2(MIX,MJX,KZZ),mrkjp1(MIX,MJX,KZZ),mrkjp2(MIX,MJX,KZZ),              DIFFTH.62
     + mrkjm1(MIX,MJX,KZZ),mrkjm2(MIX,MJX,KZZ)                                   DIFFTH.63
      real dtdz3d(mix,mjx,0:kzz+1),scal                                          DIFFTH.64
      real wip1(MIX),wip2(MIX),wjp1(MIX),wjp2(MIX),wim1(MIX),                    DIFFTH.65
     + wim2(MIX),wjm1(MIX),wjm2(MIX)                                             DIFFTH.66
      real fl(MIX,MJX,KZZ+1)                                                     DIFFTH.67
      integer irkip1(MIX,MJX,KZZ),irkip2(MIX,MJX,KZZ),                           DIFFTH.68
     + irkim1(MIX,MJX,KZZ),irkim2(MIX,MJX,KZZ),irkjp1(MIX,MJX,KZZ),              DIFFTH.69
     + irkjp2(MIX,MJX,KZZ),irkjm1(MIX,MJX,KZZ),irkjm2(MIX,MJX,KZZ)               DIFFTH.70
                                                                                 DIFFTH.71
                                                                                 DIFFTH.72
                                                                                 DIFFTH.73
C                                                                                DIFFTH.74
C----------------------------------------------------------------------          DIFFTH.75
C                                                                                DIFFTH.76
C                                                                                DIFFTH.77
      IF(IBGN.GT.2)THEN                                                          DIFFTH.78
        IB=IBGN                                                                  DIFFTH.79
        JB=JBGN                                                                  DIFFTH.80
        IE=IEND                                                                  DIFFTH.81
        JE=JEND                                                                  DIFFTH.82
      ELSE                                                                       DIFFTH.83
        IB=IBGN+1                                                                DIFFTH.84
        JB=JBGN+1                                                                DIFFTH.85
        IE=IEND-1                                                                DIFFTH.86
        JE=JEND-1                                                                DIFFTH.87
      ENDIF                                                                      DIFFTH.88
C                                                                                DIFFTH.89
      in = inest                                                                 DIFFTH.90
      scal = 300                                                                 DIFFTH.91
                                                                                 DIFFTH.92
C_FLIC_RUNPAD(2)                                                                 DIFFTH.93
c$omp parallel do default(shared)                                                DIFFTH.94
c$omp&private(i,j,k)                                                             DIFFTH.95
        DO J=1,JE+2                                                              DIFFTH.96
        do i=1,ie+2                                                              DIFFTH.97
        do k=1,kl                                                                DIFFTH.98
        fl(i,j,k) = f(i,j,k)                                                     DIFFTH.99
        enddo                                                                    DIFFTH.100
        fl(i,j,kl+1) = f(i,j,kl)                                                 DIFFTH.101
        enddo                                                                    DIFFTH.102
        enddo                                                                    DIFFTH.103
                                                                                 DIFFTH.104
c$omp parallel do default(shared)                                                DIFFTH.105
c$omp&private(i,j,k)                                                             DIFFTH.106
       DO J=1,JE+2                                                               DIFFTH.107
       do k=1,kl-1                                                               DIFFTH.108
       do i=1,ie+2                                                               DIFFTH.109
       dtdz3d(i,j,k)=min((fl(i,j,k)-fl(i,j,k+1))/dz3d(i,j,k),1.25e-2)            DIFFTH.110
       enddo                                                                     DIFFTH.111
       enddo                                                                     DIFFTH.112
       enddo                                                                     DIFFTH.113
                                                                                 DIFFTH.114
c$omp parallel do default(shared)                                                DIFFTH.115
c$omp&private(i,j)                                                               DIFFTH.116
       DO J=1,JE+2                                                               DIFFTH.117
       do i=1,ie+2                                                               DIFFTH.118
       dtdz3d(i,j,0) = dtdz3d(i,j,1)                                             DIFFTH.119
       dtdz3d(i,j,kl) = dtdz3d(i,j,kl-1)                                         DIFFTH.120
       dtdz3d(i,j,kl+1) = dtdz3d(i,j,kl-1)                                       DIFFTH.121
       enddo                                                                     DIFFTH.122
       enddo                                                                     DIFFTH.123
                                                                                 DIFFTH.124
                                                                                 DIFFTH.125
                                                                                 DIFFTH.126
                                                                                 DIFFTH.127
c                                                                                DIFFTH.128
c  ** big j-loop                                                                 DIFFTH.129
c                                                                                DIFFTH.130
c$omp parallel do default(shared)                                                DIFFTH.131
c$omp&private(i,j,k,l2,gezip,gezim,gezjp,gezjm,gesi,gesj,                        DIFFTH.132
c$omp&wip1,wip2,wim1,wim2,wjp1,wjp2,wjm1,wjm2,fti,ftj,tfac,                      DIFFTH.133
c$omp&ftip,ftim,ftjp,ftjm,ftil,ftjl,ftih,ftjh,fit,fjt)                           DIFFTH.134
      DO J=JB,JE                                                                 DIFFTH.135
                                                                                 DIFFTH.136
                                                                                 DIFFTH.137
                                                                                 DIFFTH.138
        do k=kmv+1,kl                                                            DIFFTH.139
          do  i=ib,ie                                                            DIFFTH.140
                                                                                 DIFFTH.141
            l2(i,k) = min(clw(i,j,k)*6.e4,0.65)                                  DIFFTH.142
                                                                                 DIFFTH.143
c  Weighting coefficients for z and sigma diffusion                              DIFFTH.144
                                                                                 DIFFTH.145
            gezip(i,k) = (1.-lam(i,j))*max(0,kmi(i,j)+1-k)/                      DIFFTH.146
     &        float(kmi(i,j)-kmv+1)+lam(i,j)*(1-min(0.5,(k-kmv)/                 DIFFTH.147
     &        (2.*(kmi(i,j)-kmv+1))))*                                           DIFFTH.148
     &        min(1,max(0,kmip(i,j,in)+1-k))                                     DIFFTH.149
            gezim(i,k) = (1.-lam(i,j))*max(0,kmi(i,j)+1-k)/                      DIFFTH.150
     &        float(kmi(i,j)-kmv+1)+lam(i,j)*(1-min(0.5,(k-kmv)/                 DIFFTH.151
     &        (2.*(kmi(i,j)-kmv+1))))*                                           DIFFTH.152
     &        min(1,max(0,kmim(i,j,in)+1-k))                                     DIFFTH.153
            gezjp(i,k) = (1.-lam(i,j))*max(0,kmj(i,j)+1-k)/                      DIFFTH.154
     &        float(kmj(i,j)-kmv+1)+lam(i,j)*(1-min(0.5,(k-kmv)/                 DIFFTH.155
     &        (2.*(kmj(i,j)-kmv+1))))*                                           DIFFTH.156
     &        min(1,max(0,kmjp(i,j,in)+1-k))                                     DIFFTH.157
            gezjm(i,k) = (1.-lam(i,j))*max(0,kmj(i,j)+1-k)/                      DIFFTH.158
     &        float(kmj(i,j)-kmv+1)+lam(i,j)*(1-min(0.5,(k-kmv)/                 DIFFTH.159
     &        (2.*(kmj(i,j)-kmv+1))))*                                           DIFFTH.160
     &        min(1,max(0,kmjm(i,j,in)+1-k))                                     DIFFTH.161
                                                                                 DIFFTH.162
            gesi(i,k) = (1.-0.5*(gezip(i,k)+gezim(i,k)))*(                       DIFFTH.163
     &        (1.-lam(i,j))*0.5+lam(i,j) )                                       DIFFTH.164
            gesj(i,k) = (1.-0.5*(gezjp(i,k)+gezjm(i,k)))*(                       DIFFTH.165
     &        (1.-lam(i,j))*0.5+lam(i,j) )                                       DIFFTH.166
                                                                                 DIFFTH.167
          enddo                                                                  DIFFTH.168
        enddo                                                                    DIFFTH.169
                                                                                 DIFFTH.170
c  Compute diffusion along the sigma surfaces where they are flat enough         DIFFTH.171
      do k=1,zdiffst(inest)-1                                                    DIFFTH.172
      do I=ib,ie                                                                 DIFFTH.173
      FTEN(I,J,K)=FTEN(I,J,K)-XK(I,J,K)*C203*PSB(I,J)*(F(I,J+2,K)+               DIFFTH.174
     +  F(I,J-2,K)+F(I+2,J,K)+F(I-2,J,K)-4.*(F(I,J+1,K)+F(I,J-1,K)+              DIFFTH.175
     +  F(I+1,J,K)+F(I-1,J,K))+12.*F(I,J,K))*t(i,j,k)/f(i,j,k)                   DIFFTH.176
                                                                                 DIFFTH.177
      enddo                                                                      DIFFTH.178
      enddo                                                                      DIFFTH.179
                                                                                 DIFFTH.180
C.....Truly horizontal diffusion where possible without intersecting the topogra DIFFTH.181
c     at any grid point                                                          DIFFTH.182
C                                                                                DIFFTH.183
                                                                                 DIFFTH.184
        DO K=zdiffst(inest),kmv                                                  DIFFTH.185
                                                                                 DIFFTH.186
c  Correction factor for vertical interpolation in case of non-constant vertical DIFFTH.187
c  Seems to improve the model's capability of keeping thin inversions above moun DIFFTH.188
                                                                                 DIFFTH.189
          do I=ib,ie                                                             DIFFTH.190
            wjp2(i) = mrkjp2(i,j,k)-((1-mrkjp2(i,j,k))*                          DIFFTH.191
     &                mrkjp2(i,j,k))**2*(dtdz3d(i,j,irkjp2(i,j,k)+1)-            DIFFTH.192
     &                dtdz3d(i,j,irkjp2(i,j,k)-1))*scal                          DIFFTH.193
            wjp2(i) = max(wjp2(i),0.)                                            DIFFTH.194
            wjp2(i) = min(wjp2(i),1.)                                            DIFFTH.195
            wjp1(i) = mrkjp1(i,j,k)-((1-mrkjp1(i,j,k))*                          DIFFTH.196
     &                mrkjp1(i,j,k))**2*(dtdz3d(i,j,irkjp1(i,j,k)+1)-            DIFFTH.197
     &                dtdz3d(i,j,irkjp1(i,j,k)-1))*scal                          DIFFTH.198
            wjp1(i) = max(wjp1(i),0.)                                            DIFFTH.199
            wjp1(i) = min(wjp1(i),1.)                                            DIFFTH.200
            wjm2(i) = mrkjm2(i,j,k)-((1-mrkjm2(i,j,k))*                          DIFFTH.201
     &                mrkjm2(i,j,k))**2*(dtdz3d(i,j,irkjm2(i,j,k)+1)-            DIFFTH.202
     &                dtdz3d(i,j,irkjm2(i,j,k)-1))*scal                          DIFFTH.203
            wjm2(i) = max(wjm2(i),0.)                                            DIFFTH.204
            wjm2(i) = min(wjm2(i),1.)                                            DIFFTH.205
            wjm1(i) = mrkjm1(i,j,k)-((1-mrkjm1(i,j,k))*                          DIFFTH.206
     &                mrkjm1(i,j,k))**2*(dtdz3d(i,j,irkjm1(i,j,k)+1)-            DIFFTH.207
     &                dtdz3d(i,j,irkjm1(i,j,k)-1))*scal                          DIFFTH.208
            wjm1(i) = max(wjm1(i),0.)                                            DIFFTH.209
            wjm1(i) = min(wjm1(i),1.)                                            DIFFTH.210
            wip2(i) = mrkip2(i,j,k)-((1-mrkip2(i,j,k))*                          DIFFTH.211
     &                mrkip2(i,j,k))**2*(dtdz3d(i,j,irkip2(i,j,k)+1)-            DIFFTH.212
     &                dtdz3d(i,j,irkip2(i,j,k)-1))*scal                          DIFFTH.213
            wip2(i) = max(wip2(i),0.)                                            DIFFTH.214
            wip2(i) = min(wip2(i),1.)                                            DIFFTH.215
            wip1(i) = mrkip1(i,j,k)-((1-mrkip1(i,j,k))*                          DIFFTH.216
     &                mrkip1(i,j,k))**2*(dtdz3d(i,j,irkip1(i,j,k)+1)-            DIFFTH.217
     &                dtdz3d(i,j,irkip1(i,j,k)-1))*scal                          DIFFTH.218
            wip1(i) = max(wip1(i),0.)                                            DIFFTH.219
            wip1(i) = min(wip1(i),1.)                                            DIFFTH.220
            wim2(i) = mrkim2(i,j,k)-((1-mrkim2(i,j,k))*                          DIFFTH.221
     &                mrkim2(i,j,k))**2*(dtdz3d(i,j,irkim2(i,j,k)+1)-            DIFFTH.222
     &                dtdz3d(i,j,irkim2(i,j,k)-1))*scal                          DIFFTH.223
            wim2(i) = max(wim2(i),0.)                                            DIFFTH.224
            wim2(i) = min(wim2(i),1.)                                            DIFFTH.225
            wim1(i) = mrkim1(i,j,k)-((1-mrkim1(i,j,k))*                          DIFFTH.226
     &                mrkim1(i,j,k))**2*(dtdz3d(i,j,irkim1(i,j,k)+1)-            DIFFTH.227
     &                dtdz3d(i,j,irkim1(i,j,k)-1))*scal                          DIFFTH.228
            wim1(i) = max(wim1(i),0.)                                            DIFFTH.229
            wim1(i) = min(wim1(i),1.)                                            DIFFTH.230
          enddo                                                                  DIFFTH.231
                                                                                 DIFFTH.232
                                                                                 DIFFTH.233
C                                                                                DIFFTH.234
                                                                                 DIFFTH.235
          DO I=ib,ie                                                             DIFFTH.236
            FTJ(I,K) = (1-wjp2(i))*                                              DIFFTH.237
     &          fl(I,J+2,irkjp2(i,j,k))+                                         DIFFTH.238
     &          wjp2(i)*fl(I,J+2,irkjp2(i,j,k)+1)+                               DIFFTH.239
     &          (1-wjm2(i))*fl(I,J-2,irkjm2(i,j,k))+                             DIFFTH.240
     &          wjm2(i)*fl(I,J-2,irkjm2(i,j,k)+1)                                DIFFTH.241
     &          -4*( (1-wjp1(i))*fl(I,J+1,irkjp1(i,j,k))+                        DIFFTH.242
     &          wjp1(i)*fl(I,J+1,irkjp1(i,j,k)+1)+                               DIFFTH.243
     &          (1-wjm1(i))*fl(I,J-1,irkjm1(i,j,k))+                             DIFFTH.244
     &          wjm1(i)*fl(I,J-1,irkjm1(i,j,k)+1) )+                             DIFFTH.245
     &          6*fl(I,J,K)                                                      DIFFTH.246
                                                                                 DIFFTH.247
            FTI(I,K) = (1-wip2(i))*                                              DIFFTH.248
     &          fl(I+2,J,irkip2(i,j,k))+                                         DIFFTH.249
     &          wip2(i)*fl(I+2,J,irkip2(i,j,k)+1)+                               DIFFTH.250
     &          (1-wim2(i))*fl(I-2,J,irkim2(i,j,k))+                             DIFFTH.251
     &          wim2(i)*fl(I-2,J,irkim2(i,j,k)+1)                                DIFFTH.252
     &          -4*( (1-wip1(i))*fl(I+1,J,irkip1(i,j,k))+                        DIFFTH.253
     &          wip1(i)*fl(I+1,J,irkip1(i,j,k)+1)+                               DIFFTH.254
     &          (1-wim1(i))*fl(I-1,J,irkim1(i,j,k))+                             DIFFTH.255
     &          wim1(i)*fl(I-1,J,irkim1(i,j,k)+1) )+                             DIFFTH.256
     &          6*fl(I,J,K)                                                      DIFFTH.257
                                                                                 DIFFTH.258
                                                                                 DIFFTH.259
             FTEN(I,J,K)=FTEN(I,J,K)-XK(I,J,K)*C203*PSB(I,J)*                    DIFFTH.260
     &          (FTI(I,K)+FTJ(I,K))*t(i,j,k)/fl(i,j,k)                           DIFFTH.261
                                                                                 DIFFTH.262
          ENDDO                                                                  DIFFTH.263
        ENDDO                                                                    DIFFTH.264
                                                                                 DIFFTH.265
c calculate the temperature gradient                                             DIFFTH.266
                                                                                 DIFFTH.269
        do I=ib,ie                                                               DIFFTH.270
          tfac(i,kl) = (1-lam(i,j))*164*(fl(i,j,kl-1)-tgb(i,j))/                 DIFFTH.271
     &      deltaz(i,j,kl) +lam(i,j)*l2(i,kl)                                    DIFFTH.272
          tfac(i,kl-1) = (1-lam(i,j))*164*(fl(i,j,kl-2)-fl(i,j,kl))/             DIFFTH.273
     &      deltaz(i,j,kl-1) +lam(i,j)*l2(i,kl-1)                                DIFFTH.274
          tfac(i,kl-2) = (1-lam(i,j))*164*(fl(i,j,kl-3)-fl(i,j,kl-1))/           DIFFTH.275
     &      deltaz(i,j,kl-2) +lam(i,j)*l2(i,kl-2)                                DIFFTH.276
        enddo                                                                    DIFFTH.277
                                                                                 DIFFTH.278
        do k=kl-2,kl                                                             DIFFTH.279
          do I=ib,ie                                                             DIFFTH.280
            tfac(i,k) = max(tfac(i,k),0.)   ! no superadiabatic gradients        DIFFTH.281
            tfac(i,k) = min(tfac(i,k),6.5)   ! no gradients above +3K/100m to av DIFFTH.282
          enddo                                                                  DIFFTH.283
        enddo                                                                    DIFFTH.284
                                                                                 DIFFTH.285
        do k=kmv+1,kl-3                                                          DIFFTH.286
          do I=ib,ie                                                             DIFFTH.287
            tfac(i,k) = 0.6 + (tfac(i,kl-2)-0.6)*(k-kmv)/(kl-2-kmv)              DIFFTH.288
          enddo                                                                  DIFFTH.289
        enddo                                                                    DIFFTH.290
c                                                                                DIFFTH.291
c                                                                                DIFFTH.292
        DO K=kmv+1,kl-2     ! Model levels where the orography may be intersecte DIFFTH.293
                                                                                 DIFFTH.294
                                                                                 DIFFTH.295
          do I=ib,ie                                                             DIFFTH.296
            wjp2(i) = mrkjp2(i,j,k)-((1-mrkjp2(i,j,k))*                          DIFFTH.297
     &                mrkjp2(i,j,k))**2*(dtdz3d(i,j,irkjp2(i,j,k)+1)-            DIFFTH.298
     &                dtdz3d(i,j,irkjp2(i,j,k)-1))*scal                          DIFFTH.299
            wjp2(i) = max(wjp2(i),0.)                                            DIFFTH.300
            wjp2(i) = min(wjp2(i),1.)                                            DIFFTH.301
            wjp1(i) = mrkjp1(i,j,k)-((1-mrkjp1(i,j,k))*                          DIFFTH.302
     &                mrkjp1(i,j,k))**2*(dtdz3d(i,j,irkjp1(i,j,k)+1)-            DIFFTH.303
     &                dtdz3d(i,j,irkjp1(i,j,k)-1))*scal                          DIFFTH.304
            wjp1(i) = max(wjp1(i),0.)                                            DIFFTH.305
            wjp1(i) = min(wjp1(i),1.)                                            DIFFTH.306
            wjm2(i) = mrkjm2(i,j,k)-((1-mrkjm2(i,j,k))*                          DIFFTH.307
     &                mrkjm2(i,j,k))**2*(dtdz3d(i,j,irkjm2(i,j,k)+1)-            DIFFTH.308
     &                dtdz3d(i,j,irkjm2(i,j,k)-1))*scal                          DIFFTH.309
            wjm2(i) = max(wjm2(i),0.)                                            DIFFTH.310
            wjm2(i) = min(wjm2(i),1.)                                            DIFFTH.311
            wjm1(i) = mrkjm1(i,j,k)-((1-mrkjm1(i,j,k))*                          DIFFTH.312
     &                mrkjm1(i,j,k))**2*(dtdz3d(i,j,irkjm1(i,j,k)+1)-            DIFFTH.313
     &                dtdz3d(i,j,irkjm1(i,j,k)-1))*scal                          DIFFTH.314
            wjm1(i) = max(wjm1(i),0.)                                            DIFFTH.315
            wjm1(i) = min(wjm1(i),1.)                                            DIFFTH.316
            wip2(i) = mrkip2(i,j,k)-((1-mrkip2(i,j,k))*                          DIFFTH.317
     &                mrkip2(i,j,k))**2*(dtdz3d(i,j,irkip2(i,j,k)+1)-            DIFFTH.318
     &                dtdz3d(i,j,irkip2(i,j,k)-1))*scal                          DIFFTH.319
            wip2(i) = max(wip2(i),0.)                                            DIFFTH.320
            wip2(i) = min(wip2(i),1.)                                            DIFFTH.321
            wip1(i) = mrkip1(i,j,k)-((1-mrkip1(i,j,k))*                          DIFFTH.322
     &                mrkip1(i,j,k))**2*(dtdz3d(i,j,irkip1(i,j,k)+1)-            DIFFTH.323
     &                dtdz3d(i,j,irkip1(i,j,k)-1))*scal                          DIFFTH.324
            wip1(i) = max(wip1(i),0.)                                            DIFFTH.325
            wip1(i) = min(wip1(i),1.)                                            DIFFTH.326
            wim2(i) = mrkim2(i,j,k)-((1-mrkim2(i,j,k))*                          DIFFTH.327
     &                mrkim2(i,j,k))**2*(dtdz3d(i,j,irkim2(i,j,k)+1)-            DIFFTH.328
     &                dtdz3d(i,j,irkim2(i,j,k)-1))*scal                          DIFFTH.329
            wim2(i) = max(wim2(i),0.)                                            DIFFTH.330
            wim2(i) = min(wim2(i),1.)                                            DIFFTH.331
            wim1(i) = mrkim1(i,j,k)-((1-mrkim1(i,j,k))*                          DIFFTH.332
     &                mrkim1(i,j,k))**2*(dtdz3d(i,j,irkim1(i,j,k)+1)-            DIFFTH.333
     &                dtdz3d(i,j,irkim1(i,j,k)-1))*scal                          DIFFTH.334
            wim1(i) = max(wim1(i),0.)                                            DIFFTH.335
            wim1(i) = min(wim1(i),1.)                                            DIFFTH.336
          enddo                                                                  DIFFTH.337
                                                                                 DIFFTH.338
          DO I=ib,ie                                                             DIFFTH.339
                                                                                 DIFFTH.340
c  truly horizontal diffusion, the one-sided approximations are needed in the st DIFFTH.341
                                                                                 DIFFTH.342
          if (gezjp(i,k).gt.0.01) then                                           DIFFTH.343
            FTJP(I,K) = (1-wjp2(i))*                                             DIFFTH.344
     &           fl(I,J+2,irkjp2(i,j,k))+                                        DIFFTH.345
     &           wjp2(i)*fl(I,J+2,irkjp2(i,j,k)+1)                               DIFFTH.346
     &           -4*( (1-wjp1(i))*fl(I,J+1,irkjp1(i,j,k))+                       DIFFTH.347
     &           wjp1(i)*fl(I,J+1,irkjp1(i,j,k)+1)                               DIFFTH.348
     &           )+3*fl(I,J,K)                                                   DIFFTH.349
          else                                                                   DIFFTH.350
            FTJP(I,K) = 0                                                        DIFFTH.351
          endif                                                                  DIFFTH.352
                                                                                 DIFFTH.353
          if (gezip(i,k).gt.0.01) then                                           DIFFTH.354
            FTIP(I,K) = (1-wip2(i))*                                             DIFFTH.355
     &           fl(I+2,J,irkip2(i,j,k))+                                        DIFFTH.356
     &           wip2(i)*fl(I+2,J,irkip2(i,j,k)+1)                               DIFFTH.357
     &           -4*( (1-wip1(i))*fl(I+1,J,irkip1(i,j,k))+                       DIFFTH.358
     &           wip1(i)*fl(I+1,J,irkip1(i,j,k)+1)                               DIFFTH.359
     &           )+3*fl(I,J,K)                                                   DIFFTH.360
          else                                                                   DIFFTH.361
            FTIP(I,K) = 0                                                        DIFFTH.362
          endif                                                                  DIFFTH.363
                                                                                 DIFFTH.364
          if (gezjm(i,k).gt.0.01) then                                           DIFFTH.365
            FTJM(I,K) = (1-wjm2(i))*                                             DIFFTH.366
     &           fl(I,J-2,irkjm2(i,j,k))+                                        DIFFTH.367
     &           wjm2(i)*fl(I,J-2,irkjm2(i,j,k)+1)                               DIFFTH.368
     &           -4*( (1-wjm1(i))*fl(I,J-1,irkjm1(i,j,k))+                       DIFFTH.369
     &           wjm1(i)*fl(I,J-1,irkjm1(i,j,k)+1)                               DIFFTH.370
     &           )+3*fl(I,J,K)                                                   DIFFTH.371
          else                                                                   DIFFTH.372
            FTJM(I,K) = 0                                                        DIFFTH.373
          endif                                                                  DIFFTH.374
                                                                                 DIFFTH.375
          if (gezim(i,k).gt.0.01) then                                           DIFFTH.376
            FTIM(I,K) = (1-wim2(i))*                                             DIFFTH.377
     &           fl(I-2,J,irkim2(i,j,k))+                                        DIFFTH.378
     &           wim2(i)*fl(I-2,J,irkim2(i,j,k)+1)                               DIFFTH.379
     &           -4*( (1-wim1(i))*fl(I-1,J,irkim1(i,j,k))+                       DIFFTH.380
     &           wim1(i)*fl(I-1,J,irkim1(i,j,k)+1)                               DIFFTH.381
     &           )+3*fl(I,J,K)                                                   DIFFTH.382
          else                                                                   DIFFTH.383
            FTIM(I,K) = 0                                                        DIFFTH.384
          endif                                                                  DIFFTH.385
                                                                                 DIFFTH.386
c  Centered sigma-diffusion with temperature gradient correction                 DIFFTH.387
                                                                                 DIFFTH.388
          if ((gesi(i,k).gt.0.01).or.(gesj(i,k).gt.0.01)) then                   DIFFTH.389
            FTJ(I,K) = fl(I,J+2,K)+fl(I,J-2,K)-4*(fl(I,J+1,K)+                   DIFFTH.390
     &          fl(I,J-1,K))+6*fl(I,J,K) + tfac(i,k)*fjt04(i,j)                  DIFFTH.391
                                                                                 DIFFTH.392
            FTI(I,K) = fl(I+2,J,K)+fl(I-2,J,K)-4*(fl(I+1,J,K)+                   DIFFTH.393
     &          fl(I-1,J,K))+6*fl(I,J,K) + tfac(i,k)*fit04(i,j)                  DIFFTH.394
                                                                                 DIFFTH.395
c                                                                                DIFFTH.396
c  Subsidiary one-sided second-order diffusion where centered sigma-diffusion is DIFFTH.397
c  (recently changed; in the paper, it is centered second-order diffusion)       DIFFTH.398
                                                                                 DIFFTH.399
            FTIL(I,K) = fl(I,J,K)-fl(I-1,J,K)+tfac(i,k)*fit0u(i,j)               DIFFTH.400
            FTJL(I,K) = fl(I,J,K)-fl(I,J-1,K)+tfac(i,k)*fjt0u(i,j)               DIFFTH.401
            FTIH(I,K) = fl(I,J,K)-fl(I+1,J,K)+tfac(i,k)*fit0o(i,j)               DIFFTH.402
            FTJH(I,K) = fl(I,J,K)-fl(I,J+1,K)+tfac(i,k)*fjt0o(i,j)               DIFFTH.403
c                                                                                DIFFTH.404
c   Summing up all the diffusion terms                                           DIFFTH.405
c                                                                                DIFFTH.406
            FTEN(I,J,K)=FTEN(I,J,K)-XK(I,J,K)*t(i,j,k)/fl(i,j,k)*C203*           DIFFTH.407
     &           PSB(I,J)*(gesi(i,k)*(fin(i,j)*FTI(I,K)+                         DIFFTH.408
     &           min(100.,1./fin(i,j))/100.*(1-lam(i,j))*                        DIFFTH.409
     &           (fion(i,j)*FTIH(I,K)+fiun(i,j)*FTIL(I,K)))+                     DIFFTH.410
     &           gesj(i,k)*(fjn(i,j)*FTJ(I,K)+min(100.,1./                       DIFFTH.411
     &           fjn(i,j))/100.*(1-lam(i,j))*(fjon(i,j)*FTJH(I,K)+               DIFFTH.412
     &           fjun(i,j)*FTJL(I,K)))+gezip(i,k)*FTIP(I,K)+                     DIFFTH.413
     &           gezim(i,k)*FTIM(I,K)+gezjp(i,k)*FTJP(I,K)+                      DIFFTH.414
     &           gezjm(i,k)*FTJM(I,K))                                           DIFFTH.415
                                                                                 DIFFTH.416
          else                                                                   DIFFTH.417
                                                                                 DIFFTH.418
            FTEN(I,J,K)=FTEN(I,J,K)-XK(I,J,K)*t(i,j,k)/fl(i,j,k)*C203*           DIFFTH.419
     &        PSB(I,J)*(gezip(i,k)*FTIP(I,K)+gezim(i,k)*FTIM(I,K)+               DIFFTH.420
     &        gezjp(i,k)*FTJP(I,K)+gezjm(i,k)*FTJM(I,K))                         DIFFTH.421
                                                                                 DIFFTH.422
          endif                                                                  DIFFTH.423
                                                                                 DIFFTH.424
          ENDDO                                                                  DIFFTH.425
        ENDDO                                                                    DIFFTH.426
                                                                                 DIFFTH.427
        DO K=max(kmv+1,kl-1),kl     ! Lowermost two levels need an additional co DIFFTH.428
          DO I=ib,ie      ! because of unstable mode of fourth-order diffusion   DIFFTH.429
                                                                                 DIFFTH.430
                                                                                 DIFFTH.431
            if (gezjp(i,k).gt.0.01) then                                         DIFFTH.432
              FTJP(I,K) = (1-mrkjp2(i,j,k))*                                     DIFFTH.433
     &           fl(I,J+2,irkjp2(i,j,k))+                                        DIFFTH.434
     &           mrkjp2(i,j,k)*fl(I,J+2,irkjp2(i,j,k)+1)                         DIFFTH.435
     &           -4*( (1-mrkjp1(i,j,k))*fl(I,J+1,irkjp1(i,j,k))+                 DIFFTH.436
     &           mrkjp1(i,j,k)*fl(I,J+1,irkjp1(i,j,k)+1)                         DIFFTH.437
     &           )+3*fl(I,J,K)                                                   DIFFTH.438
            else                                                                 DIFFTH.439
              FTJP(I,K) = 0                                                      DIFFTH.440
            endif                                                                DIFFTH.441
                                                                                 DIFFTH.442
            if (gezip(i,k).gt.0.01) then                                         DIFFTH.443
              FTIP(I,K) = (1-mrkip2(i,j,k))*                                     DIFFTH.444
     &           fl(I+2,J,irkip2(i,j,k))+                                        DIFFTH.445
     &           mrkip2(i,j,k)*fl(I+2,J,irkip2(i,j,k)+1)                         DIFFTH.446
     &           -4*( (1-mrkip1(i,j,k))*fl(I+1,J,irkip1(i,j,k))+                 DIFFTH.447
     &           mrkip1(i,j,k)*fl(I+1,J,irkip1(i,j,k)+1)                         DIFFTH.448
     &           )+3*fl(I,J,K)                                                   DIFFTH.449
            else                                                                 DIFFTH.450
              FTIP(I,K) = 0                                                      DIFFTH.451
            endif                                                                DIFFTH.452
                                                                                 DIFFTH.453
            if (gezjm(i,k).gt.0.01) then                                         DIFFTH.454
              FTJM(I,K) = (1-mrkjm2(i,j,k))*                                     DIFFTH.455
     &           fl(I,J-2,irkjm2(i,j,k))+                                        DIFFTH.456
     &           mrkjm2(i,j,k)*fl(I,J-2,irkjm2(i,j,k)+1)                         DIFFTH.457
     &           -4*( (1-mrkjm1(i,j,k))*fl(I,J-1,irkjm1(i,j,k))+                 DIFFTH.458
     &           mrkjm1(i,j,k)*fl(I,J-1,irkjm1(i,j,k)+1)                         DIFFTH.459
     &           )+3*fl(I,J,K)                                                   DIFFTH.460
            else                                                                 DIFFTH.461
              FTJM(I,K) = 0                                                      DIFFTH.462
            endif                                                                DIFFTH.463
                                                                                 DIFFTH.464
            if (gezim(i,k).gt.0.01) then                                         DIFFTH.465
              FTIM(I,K) = (1-mrkim2(i,j,k))*                                     DIFFTH.466
     &           fl(I-2,J,irkim2(i,j,k))+                                        DIFFTH.467
     &           mrkim2(i,j,k)*fl(I-2,J,irkim2(i,j,k)+1)                         DIFFTH.468
     &           -4*( (1-mrkim1(i,j,k))*fl(I-1,J,irkim1(i,j,k))+                 DIFFTH.469
     &           mrkim1(i,j,k)*fl(I-1,J,irkim1(i,j,k)+1)                         DIFFTH.470
     &           )+3*fl(I,J,K)                                                   DIFFTH.471
            else                                                                 DIFFTH.472
              FTIM(I,K) = 0                                                      DIFFTH.473
            endif                                                                DIFFTH.474
                                                                                 DIFFTH.475
                                                                                 DIFFTH.476
            FTJ(I,K) = fl(I,J+2,K)+fl(I,J-2,K)-4*(fl(I,J+1,K)+                   DIFFTH.477
     &                 fl(I,J-1,K))+6*fl(I,J,K)+tfac(i,k)*fjt04(i,j)             DIFFTH.478
                                                                                 DIFFTH.479
            FTI(I,K) = fl(I+2,J,K)+fl(I-2,J,K)-4*(fl(I+1,J,K)+                   DIFFTH.480
     &                 fl(I-1,J,K))+6*fl(I,J,K)+tfac(i,k)*fit04(i,j)             DIFFTH.481
                                                                                 DIFFTH.482
c  Check whether fourth-order diffusion yields a wrong-signed tendency           DIFFTH.483
                                                                                 DIFFTH.484
            FJT(i,k) = FTJ(I,K)+4*fl(I,J,K)-2*(fl(I,J+2,K)+                      DIFFTH.485
     &                 fl(I,J-2,K))+tfac(i,k)*(4*T0(I,J,K)-2*                    DIFFTH.486
     &                 (T0(I,J+2,K)+T0(I,J-2,K)))                                DIFFTH.487
                                                                                 DIFFTH.488
            FIT(i,k) = FTI(I,K)+4*fl(I,J,K)-2*(fl(I+2,J,K)+                      DIFFTH.489
     &                 fl(I-2,J,K))+tfac(i,k)*(4*T0(I,J,K)-2*                    DIFFTH.490
     &                 (T0(I+2,J,K)+T0(I-2,J,K)))                                DIFFTH.491
                                                                                 DIFFTH.492
c  If this turns out to be the case, second-order diffusion is applied           DIFFTH.493
                                                                                 DIFFTH.494
            if (FJT(i,k)*FTJ(i,k).lt.-0.2) then                                  DIFFTH.495
              FTJ(i,k) = 4*fl(I,J,K)-2*(fl(I,J+1,K)+fl(I,J-1,K))                 DIFFTH.496
     &            +tfac(i,k)*(4*T0(I,J,K)-2*(T0(I,J+1,K)+T0(I,J-1,K)))           DIFFTH.497
            endif                                                                DIFFTH.498
                                                                                 DIFFTH.499
            if (FIT(i,k)*FTI(i,k).lt.-0.2) then                                  DIFFTH.500
              FTI(I,K) = 4*fl(I,J,K)-2*(fl(I+1,J,K)+fl(I-1,J,K))                 DIFFTH.501
     &            +tfac(i,k)*(4*T0(I,J,K)-2*(T0(I+1,J,K)+T0(I-1,J,K)))           DIFFTH.502
            endif                                                                DIFFTH.503
                                                                                 DIFFTH.504
c                                                                                DIFFTH.505
            FTIL(I,K) = fl(I,J,K)-fl(I-1,J,K)+tfac(i,k)*fit0u(i,j)               DIFFTH.506
            FTJL(I,K) = fl(I,J,K)-fl(I,J-1,K)+tfac(i,k)*fjt0u(i,j)               DIFFTH.507
            FTIH(I,K) = fl(I,J,K)-fl(I+1,J,K)+tfac(i,k)*fit0o(i,j)               DIFFTH.508
            FTJH(I,K) = fl(I,J,K)-fl(I,J+1,K)+tfac(i,k)*fjt0o(i,j)               DIFFTH.509
c                                                                                DIFFTH.510
c                                                                                DIFFTH.511
            FTEN(I,J,K)=FTEN(I,J,K)-XK(I,J,K)*t(i,j,k)/fl(i,j,k)*C203*           DIFFTH.512
     &        PSB(I,J)*(gesi(i,k)*(fin(i,j)*FTI(I,K)+                            DIFFTH.513
     &        min(100.,1./fin(i,j))/100.*(1-lam(i,j))*(fion(i,j)*                DIFFTH.514
     &        FTIH(I,K)+fiun(i,j)*FTIL(I,K)))+gesj(i,k)*(fjn(i,j)*               DIFFTH.515
     &        FTJ(I,K)+min(100.,1./fjn(i,j))/100.*(1-lam(i,j))*                  DIFFTH.516
     &        (fjon(i,j)*FTJH(I,K)+fjun(i,j)*FTJL(I,K)))+                        DIFFTH.517
     &        gezip(i,k)*FTIP(I,K)+gezim(i,k)*FTIM(I,K)+gezjp(i,k)*              DIFFTH.518
     &        FTJP(I,K)+gezjm(i,k)*FTJM(I,K))                                    DIFFTH.519
                                                                                 DIFFTH.520
          ENDDO                                                                  DIFFTH.521
        ENDDO                                                                    DIFFTH.522
                                                                                 DIFFTH.523
                                                                                 DIFFTH.524
      ENDDO   ! ** End of outer j-loop                                           DIFFTH.525
C_FLIC_RUNPAD(0)                                                                 DIFFTH.526
C                                                                                DIFFTH.527
C.....SECOND-ORDER SCHEME FOR BOUNDARIES:                                        DIFFTH.528
C                                                                                DIFFTH.529
      IF(INEST.GT.1)RETURN                                                       DIFFTH.530
cmic$ do all autoscope                                                           DIFFTH.531
cmic$1 shared(IBGN,IEND,JBGN,JEND,KL,                                            DIFFTH.532
cmic$1        F,FTEN,XK,PSB)                                                     DIFFTH.533
cmic$2 private(I,J,K)                                                            DIFFTH.534
c$doacross                                                                       DIFFTH.535
c$& local(i,j,k)                                                                 DIFFTH.536
c$omp parallel do default(shared)                                                DIFFTH.537
#ifndef MPP1                                                                     DIFFTH.538
c$omp&private(i,j,k)                                                             DIFFTH.539
#else                                                                            DIFFTH.540
c$omp&private(i,j,k,jflict_0,jflict_1,jflict_2,jflict_3,                         DIFFTH.541
c$omp&        jflict_4,jflict_5,iflict_6,iflict_7,iflict_8,                      DIFFTH.542
c$omp&        iflict_9,iflict_10,iflict_11)                                      DIFFTH.543
#endif                                                                           DIFFTH.544
C_FLIC_RUNPAD(2)                                                                 DIFFTH.545
      DO K=1,KL                                                                  DIFFTH.546
        J=JBGN                                                                   DIFFTH.547
        DO I=IBGN,IEND                                                           DIFFTH.548
          FTEN(I,J,K)=FTEN(I,J,K)+XK(I,J,K)*C203*PSB(I,J)*(F(I,J+1,K)+           DIFFTH.549
     +                F(I,J-1,K)+F(I+1,J,K)+F(I-1,J,K)-4.*F(I,J,K))              DIFFTH.550
        ENDDO                                                                    DIFFTH.551
        J=JEND                                                                   DIFFTH.552
        DO I=IBGN,IEND                                                           DIFFTH.553
          FTEN(I,J,K)=FTEN(I,J,K)+XK(I,J,K)*C203*PSB(I,J)*(F(I,J+1,K)+           DIFFTH.554
     +                F(I,J-1,K)+F(I+1,J,K)+F(I-1,J,K)-4.*F(I,J,K))              DIFFTH.555
        ENDDO                                                                    DIFFTH.556
        I=2                                                                      DIFFTH.557
        DO J=JB,JE                                                               DIFFTH.558
          FTEN(I,J,K)=FTEN(I,J,K)+XK(I,J,K)*C203*PSB(I,J)*(F(I,J+1,K)+           DIFFTH.559
     +                F(I,J-1,K)+F(I+1,J,K)+F(I-1,J,K)-4.*F(I,J,K))              DIFFTH.560
        ENDDO                                                                    DIFFTH.561
        I=IEND                                                                   DIFFTH.562
        DO J=JB,JE                                                               DIFFTH.563
          FTEN(I,J,K)=FTEN(I,J,K)+XK(I,J,K)*C203*PSB(I,J)*(F(I,J+1,K)+           DIFFTH.564
     +                F(I,J-1,K)+F(I+1,J,K)+F(I-1,J,K)-4.*F(I,J,K))              DIFFTH.565
        ENDDO                                                                    DIFFTH.566
      ENDDO                                                                      DIFFTH.567
C                                                                                DIFFTH.568
C_FLIC_RUNPAD(0)                                                                 DIFFTH.569
      RETURN                                                                     DIFFTH.570
      END                                                                        DIFFTH.571
                                                                                 DIFFTH.572
