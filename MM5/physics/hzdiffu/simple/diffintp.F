      subroutine diffintp(A,PSB,ptop,IBGN,IEND,JBGN,JEND,KL,in,T0,dx,            DIFFINTP.1
     + rkip1,rkip2,rkim1,rkim2,rkjp1,rkjp2,rkjm1,rkjm2,fin,fjn,                  DIFFINTP.2
     + kmip,kmjp,kmim,kmjm,kmv,rmn,tij,tij1,ti1j,ti1j1,tijmn,zdiffst)            DIFFINTP.3
                                                                                 DIFFINTP.4
        implicit none                                                            DIFFINTP.5
#       include <parame.incl>                                                    DIFFINTP.6
C                                                                                DIFFINTP.7
#ifdef MPP1                                                                      DIFFINTP.8
C_FLIC_BEGIN_NOFLIC                                                              DIFFINTP.9
      INCLUDE 'mpif.h'                                                           DIFFINTP.10
      integer myrank,nprocs,irecv,isend,ierr                                     DIFFINTP.11
      real rrecv                                                                 DIFFINTP.12
C_FLIC_END_NOFLIC                                                                DIFFINTP.13
#endif                                                                           DIFFINTP.14
C                                                                                DIFFINTP.15
c  This routine computes the interpolation coefficients needed for truly horizon DIFFINTP.16
c  diffusion. Currently, the coefficients are computed for cross-points only (i. DIFFINTP.17
c  for temperature and the moisture variables only). They are computed only once DIFFINTP.18
c  the beginning of the integration and then stored in common blocks. The diffin DIFFINTP.19
c  routine is currently called from solve. This avoids storing the interpolation DIFFINTP.20
c  coefficients in the restart files.                                            DIFFINTP.21
c                                                                                DIFFINTP.22
c  In addition, the coefficients for the orographic reduction of sigma-diffusion DIFFINTP.23
c  are computed in this routine and then stored.                                 DIFFINTP.24
c                                                                                DIFFINTP.25
       integer IBGN,IEND,JBGN,JEND,KL,I,J,K,IB,JB,IE,JE,in                       DIFFINTP.26
       integer kip1(MIX,MJX),kip2(MIX,MJX),kim1(MIX,MJX),kim2(MIX,MJX),          DIFFINTP.27
     +         kjp1(MIX,MJX),kjp2(MIX,MJX),kjm1(MIX,MJX),kjm2(MIX,MJX)           DIFFINTP.28
       integer kmv(maxnes),kmax(MIX,MJX),zdiffst(maxnes)                         DIFFINTP.29
       integer kmip(MIX,MJX,maxnes),kmjp(MIX,MJX,maxnes),                        DIFFINTP.30
     +  kmim(MIX,MJX,maxnes),kmjm(MIX,MJX,maxnes)                                DIFFINTP.31
      real fin(MIX,MJX,maxnes),fjn(MIX,MJX,maxnes),tij(MIX,MJX,maxnes),          DIFFINTP.32
     + tij1(MIX,MJX,maxnes),ti1j(MIX,MJX,maxnes),ti1j1(MIX,MJX,maxnes),          DIFFINTP.33
     + tijmn(MIX,MJX,maxnes),A(KL)                                               DIFFINTP.34
       real rmn,dx,psb(MIX,MJX),T0(MIX,MJX,KL),ptop                              DIFFINTP.35
       REAL sigip1(MIX,MJX,KL),sigip2(MIX,MJX,KL),                               DIFFINTP.36
     +  sigjp1(MIX,MJX,KL),sigjp2(MIX,MJX,KL),                                   DIFFINTP.37
     +  sigim1(MIX,MJX,KL),sigim2(MIX,MJX,KL),                                   DIFFINTP.38
     +  sigjm1(MIX,MJX,KL),sigjm2(MIX,MJX,KL)                                    DIFFINTP.39
       REAL fit4(MIX,MJX),fjt4(MIX,MJX),fitm(MIX,MJX),fjtm(MIX,MJX)              DIFFINTP.40
       REAL rkip1(MIX,MJX,KL,maxnes),rkip2(MIX,MJX,KL,maxnes),                   DIFFINTP.41
     +  rkim1(MIX,MJX,KL,maxnes),rkim2(MIX,MJX,KL,maxnes),                       DIFFINTP.42
     +  rkjp1(MIX,MJX,KL,maxnes),rkjp2(MIX,MJX,KL,maxnes),                       DIFFINTP.43
     +  rkjm1(MIX,MJX,KL,maxnes),rkjm2(MIX,MJX,KL,maxnes)                        DIFFINTP.44
       real hdiff(MIX,MJX,8),hdimax(kl)                                          DIFFINTP.45
c                                                                                DIFFINTP.46
                                                                                 DIFFINTP.47
                                                                                 DIFFINTP.48
c  Factor for orography-adjusted sigma-diffusion (see below). Experience shows t DIFFINTP.49
c  sigma-diffusion is needed at coarse resolutions than at high resolutions, par DIFFINTP.50
c  for temperature in the presence of strong radiative cooling (i.e. close to th DIFFINTP.51
c  This factor has been determined empirically and may be changed or made a cons DIFFINTP.52
           rmn = max(0.05*(dx/1000.)**2,0.2)                                     DIFFINTP.53
c                                                                                DIFFINTP.54
c  Some initialization                                                           DIFFINTP.55
                                                                                 DIFFINTP.56
c$omp parallel do default(shared)                                                DIFFINTP.57
c$omp&private(i,j)                                                               DIFFINTP.58
        do j=1,mjx                                                               DIFFINTP.59
          do i=1,mix                                                             DIFFINTP.60
            fin(i,j,in) = 0                                                      DIFFINTP.61
            fjn(i,j,in) = 0                                                      DIFFINTP.62
            kmip(i,j,in) = kl                                                    DIFFINTP.63
            kmim(i,j,in) = kl                                                    DIFFINTP.64
            kmjp(i,j,in) = kl                                                    DIFFINTP.65
            kmjm(i,j,in) = kl                                                    DIFFINTP.66
          enddo                                                                  DIFFINTP.67
        enddo                                                                    DIFFINTP.68
                                                                                 DIFFINTP.69
c                                                                                DIFFINTP.70
      IF(IBGN.GT.2)THEN                                                          DIFFINTP.71
        IB=IBGN                                                                  DIFFINTP.72
        JB=JBGN                                                                  DIFFINTP.73
        IE=IEND                                                                  DIFFINTP.74
        JE=JEND                                                                  DIFFINTP.75
      ELSE                                                                       DIFFINTP.76
        IB=IBGN+1                                                                DIFFINTP.77
        JB=JBGN+1                                                                DIFFINTP.78
        IE=IEND-1                                                                DIFFINTP.79
        JE=JEND-1                                                                DIFFINTP.80
      ENDIF                                                                      DIFFINTP.81
                                                                                 DIFFINTP.82
c$omp parallel do default(shared)                                                DIFFINTP.83
c$omp&private(i,j,k)                                                             DIFFINTP.84
        do j=JB,JE                                                               DIFFINTP.85
                                                                                 DIFFINTP.86
c  Sigma values of the surrounding grid points having the same height as the loc DIFFINTP.87
          do k=1,kl                                                              DIFFINTP.88
            do i=IB,IE                                                           DIFFINTP.89
              sigip1(i,j,k) = a(k)*psb(i,j)/psb(i+1,j)                           DIFFINTP.90
              sigip2(i,j,k) = a(k)*psb(i,j)/psb(i+2,j)                           DIFFINTP.91
              sigim1(i,j,k) = a(k)*psb(i,j)/psb(i-1,j)                           DIFFINTP.92
              sigim2(i,j,k) = a(k)*psb(i,j)/psb(i-2,j)                           DIFFINTP.93
              sigjp1(i,j,k) = a(k)*psb(i,j)/psb(i,j+1)                           DIFFINTP.94
              sigjp2(i,j,k) = a(k)*psb(i,j)/psb(i,j+2)                           DIFFINTP.95
              sigjm1(i,j,k) = a(k)*psb(i,j)/psb(i,j-1)                           DIFFINTP.96
              sigjm2(i,j,k) = a(k)*psb(i,j)/psb(i,j-2)                           DIFFINTP.97
            enddo                                                                DIFFINTP.98
          enddo                                                                  DIFFINTP.99
                                                                                 DIFFINTP.100
        enddo                                                                    DIFFINTP.101
                                                                                 DIFFINTP.102
c  Computation of the corresponding level index. This index is a real number     DIFFINTP.103
c  and includes the weighting coefficient for linear vertical interpolation      DIFFINTP.104
c  between the model levels.                                                     DIFFINTP.105
                                                                                 DIFFINTP.106
        call indint(1,0,sigip1,rkip1,a,mix,mjx,kl,                               DIFFINTP.107
     +    in,IBGN,IEND,JBGN,JEND,kip1,maxnes,psb,ptop)                           DIFFINTP.108
        call indint(2,0,sigip2,rkip2,a,mix,mjx,kl,                               DIFFINTP.109
     +    in,IBGN,IEND,JBGN,JEND,kip2,maxnes,psb,ptop)                           DIFFINTP.110
        call indint(-1,0,sigim1,rkim1,a,mix,mjx,kl,                              DIFFINTP.111
     +    in,IBGN,IEND,JBGN,JEND,kim1,maxnes,psb,ptop)                           DIFFINTP.112
        call indint(-2,0,sigim2,rkim2,a,mix,mjx,kl,                              DIFFINTP.113
     +    in,IBGN,IEND,JBGN,JEND,kim2,maxnes,psb,ptop)                           DIFFINTP.114
        call indint(0,1,sigjp1,rkjp1,a,mix,mjx,kl,                               DIFFINTP.115
     +    in,IBGN,IEND,JBGN,JEND,kjp1,maxnes,psb,ptop)                           DIFFINTP.116
        call indint(0,2,sigjp2,rkjp2,a,mix,mjx,kl,                               DIFFINTP.117
     +    in,IBGN,IEND,JBGN,JEND,kjp2,maxnes,psb,ptop)                           DIFFINTP.118
        call indint(0,-1,sigjm1,rkjm1,a,mix,mjx,kl,                              DIFFINTP.119
     +    in,IBGN,IEND,JBGN,JEND,kjm1,maxnes,psb,ptop)                           DIFFINTP.120
        call indint(0,-2,sigjm2,rkjm2,a,mix,mjx,kl,                              DIFFINTP.121
     +    in,IBGN,IEND,JBGN,JEND,kjm2,maxnes,psb,ptop)                           DIFFINTP.122
                                                                                 DIFFINTP.123
c$omp parallel do default(shared)                                                DIFFINTP.124
c$omp&private(i,j)                                                               DIFFINTP.125
        do j=JB,JE                                                               DIFFINTP.126
          do i=IB,IE                                                             DIFFINTP.127
            kmip(i,j,in) = min(kip1(i,j),kip2(i,j))                              DIFFINTP.128
            kmim(i,j,in) = min(kim1(i,j),kim2(i,j))                              DIFFINTP.129
            kmjp(i,j,in) = min(kjp1(i,j),kjp2(i,j))                              DIFFINTP.130
            kmjm(i,j,in) = min(kjm1(i,j),kjm2(i,j))                              DIFFINTP.131
          enddo                                                                  DIFFINTP.132
        enddo                                                                    DIFFINTP.133
                                                                                 DIFFINTP.134
c Reduction factor for orography-adjusted sigma-diffusion. For simplicity,       DIFFINTP.135
c the factor is computed based on the reference temperature field. Since this    DIFFINTP.136
c field has an approximately constant vertical temperature gradient (w.r.t. z),  DIFFINTP.137
c T0 is a good proxy for z.                                                      DIFFINTP.138
c                                                                                DIFFINTP.139
                                                                                 DIFFINTP.140
c$omp parallel do default(shared)                                                DIFFINTP.141
c$omp&private(i,j)                                                               DIFFINTP.142
        do j=JB,JE                                                               DIFFINTP.143
          do i=IB,IE                                                             DIFFINTP.144
                                                                                 DIFFINTP.145
          fjt4(i,j) = t0(I,J+2,kl)+t0(I,J-2,kl)-4*(t0(I,J+1,kl)+                 DIFFINTP.146
     +     t0(I,J-1,kl))+6*t0(I,J,kl)                                            DIFFINTP.147
          fit4(i,j) = t0(I+2,J,kl)+t0(I-2,J,kl)-4*(t0(I+1,J,kl)+                 DIFFINTP.148
     +     t0(I-1,J,kl))+6*t0(I,J,kl)                                            DIFFINTP.149
          fjtm(i,j) = 0.25*(t0(i,j+2,kl)+t0(i,j+1,kl)+                           DIFFINTP.150
     +     t0(i,j-1,kl)+t0(i,j-2,kl)) -t0(i,j,kl)                                DIFFINTP.151
          fitm(i,j) = 0.25*(t0(i+2,j,kl)+t0(i+1,j,kl)+                           DIFFINTP.152
     +     t0(i-1,j,kl)+t0(i-2,j,kl)) -t0(i,j,kl)                                DIFFINTP.153
                                                                                 DIFFINTP.154
          fjn(i,j,in)=rmn/(rmn+fjt4(i,j)**6+fjtm(i,j)**6)                        DIFFINTP.155
          fin(i,j,in)=rmn/(rmn+fit4(i,j)**6+fitm(i,j)**6)                        DIFFINTP.156
                                                                                 DIFFINTP.157
c Fourth-order diffusion has a numerical mode that may cause wrong-signed diffus DIFFINTP.158
c when a valley bottom has a width of three grid points. This is to switch off s DIFFINTP.159
c in this case.                                                                  DIFFINTP.160
                                                                                 DIFFINTP.161
          if ((fjt4(i,j).lt.0).and.(fjtm(i,j).le.0)) then                        DIFFINTP.162
            fjn(i,j,in) = min(fjn(i,j,in),1.e-6)                                 DIFFINTP.163
          endif                                                                  DIFFINTP.164
          if ((fit4(i,j).lt.0).and.(fitm(i,j).le.0)) then                        DIFFINTP.165
            fin(i,j,in) = min(fin(i,j,in),1.e-6)                                 DIFFINTP.166
          endif                                                                  DIFFINTP.167
                                                                                 DIFFINTP.168
      tij(i,j,in)= 1./(abs(t0(i,j,kl)-(t0(i,j-1,kl)+t0(i-1,j,kl)+                DIFFINTP.169
     f t0(i-1,j-1,kl))/3.)+0.1)                                                  DIFFINTP.170
      tij1(i,j,in)=1./(abs(t0(i,j,kl)-(t0(i,j+1,kl)+t0(i-1,j,kl)+                DIFFINTP.171
     f t0(i-1,j+1,kl))/3.)+0.1)                                                  DIFFINTP.172
      ti1j(i,j,in)=1./(abs(t0(i,j,kl)-(t0(i,j-1,kl)+t0(i+1,j,kl)+                DIFFINTP.173
     f t0(i+1,j-1,kl))/3.)+0.1)                                                  DIFFINTP.174
      ti1j1(i,j,in)=1./(abs(t0(i,j,kl)-(t0(i,j+1,kl)+t0(i+1,j,kl)+               DIFFINTP.175
     f t0(i+1,j+1,kl))/3.)+0.1)                                                  DIFFINTP.176
      tijmn(i,j,in) = 1./(tij(i,j,in)+tij1(i,j,in)+ti1j(i,j,in)+                 DIFFINTP.177
     f ti1j1(i,j,in))                                                            DIFFINTP.178
        enddo                                                                    DIFFINTP.179
       enddo                                                                     DIFFINTP.180
                                                                                 DIFFINTP.181
c kmv is the lowest index at which truly horizontal computation of diffusion is  DIFFINTP.182
c possible at all grid points without intersecting the topography                DIFFINTP.183
                                                                                 DIFFINTP.184
        kmv(in) = 100                                                            DIFFINTP.185
CCc$omp parallel do default(shared)                                              DIFFINTP.186
CCc$omp&private(i,j)                                                             DIFFINTP.187
        do j=jb,je                                                               DIFFINTP.188
          do i=ib,ie                                                             DIFFINTP.189
            kmax(i,j) = min(kmip(i,j,in),kmim(i,j,in),                           DIFFINTP.190
     +                      kmjp(i,j,in),kmjm(i,j,in))                           DIFFINTP.191
            if(kmax(i,j).ne.0) kmv(in) = min(kmax(i,j),kmv(in))                  DIFFINTP.192
          enddo                                                                  DIFFINTP.193
        enddo                                                                    DIFFINTP.194
                                                                                 DIFFINTP.195
#ifdef MPP1                                                                      DIFFINTP.196
        call MPI_ALLREDUCE(kmv(in),irecv,1,MPI_INTEGER,                          DIFFINTP.197
     +                      MPI_MIN,MPI_COMM_WORLD,ierr)                         DIFFINTP.198
        kmv(in) = irecv                                                          DIFFINTP.199
#endif                                                                           DIFFINTP.200
                                                                                 DIFFINTP.201
                                                                                 DIFFINTP.202
c  Check for sigma layers for which z-diffusion is not needed because they are   DIFFINTP.203
c  flat enough                                                                   DIFFINTP.204
                                                                                 DIFFINTP.205
        hdiff = 0                                                                DIFFINTP.206
        do 520 k=kl,1,-1                                                         DIFFINTP.207
                                                                                 DIFFINTP.208
CCc$omp parallel do default(shared)                                              DIFFINTP.209
CCc$omp&private(i,j)                                                             DIFFINTP.210
        do 510 j=JB,JE                                                           DIFFINTP.211
        do 510 i=IB,IE                                                           DIFFINTP.212
      hdiff(i,j,1)=abs(a(k)*psb(i,j)-a(k)*psb(i+1,j))                            DIFFINTP.213
     f *29.26*t0(i,j,k)/(a(k)*psb(i,j)+ptop)                                     DIFFINTP.214
      hdiff(i,j,2)=abs(a(k)*psb(i,j)-a(k)*psb(i+2,j))                            DIFFINTP.215
     f *29.26*t0(i,j,k)/(a(k)*psb(i,j)+ptop)                                     DIFFINTP.216
      hdiff(i,j,3)=abs(a(k)*psb(i,j)-a(k)*psb(i-1,j))                            DIFFINTP.217
     f *29.26*t0(i,j,k)/(a(k)*psb(i,j)+ptop)                                     DIFFINTP.218
      hdiff(i,j,4)=abs(a(k)*psb(i,j)-a(k)*psb(i-2,j))                            DIFFINTP.219
     f *29.26*t0(i,j,k)/(a(k)*psb(i,j)+ptop)                                     DIFFINTP.220
      hdiff(i,j,5)=abs(a(k)*psb(i,j)-a(k)*psb(i,j+1))                            DIFFINTP.221
     f *29.26*t0(i,j,k)/(a(k)*psb(i,j)+ptop)                                     DIFFINTP.222
      hdiff(i,j,6)=abs(a(k)*psb(i,j)-a(k)*psb(i,j+2))                            DIFFINTP.223
     f *29.26*t0(i,j,k)/(a(k)*psb(i,j)+ptop)                                     DIFFINTP.224
      hdiff(i,j,7)=abs(a(k)*psb(i,j)-a(k)*psb(i,j-1))                            DIFFINTP.225
     f *29.26*t0(i,j,k)/(a(k)*psb(i,j)+ptop)                                     DIFFINTP.226
      hdiff(i,j,8)=abs(a(k)*psb(i,j)-a(k)*psb(i,j-2))                            DIFFINTP.227
     f *29.26*t0(i,j,k)/(a(k)*psb(i,j)+ptop)                                     DIFFINTP.228
                                                                                 DIFFINTP.229
 510   continue                                                                  DIFFINTP.230
                                                                                 DIFFINTP.231
        hdimax(k) = maxval(hdiff)                                                DIFFINTP.232
                                                                                 DIFFINTP.233
#ifdef MPP1                                                                      DIFFINTP.234
        call MPI_ALLREDUCE(hdimax(k),rrecv,1,MPI_REAL,                           DIFFINTP.235
     +                      MPI_MAX,MPI_COMM_WORLD,ierr)                         DIFFINTP.236
        hdimax(k) = rrecv                                                        DIFFINTP.237
#endif                                                                           DIFFINTP.238
                                                                                 DIFFINTP.239
        if (k.eq.kl) print*, 'max hgt diff over two grid distances'              DIFFINTP.240
        print*, k, hdimax(k)                                                     DIFFINTP.241
        if (hdimax(k).lt.75) then  ! max. hgt diff less than 75 m                DIFFINTP.242
        zdiffst(in) = k+1                                                        DIFFINTP.243
        goto 525                                                                 DIFFINTP.244
        endif                                                                    DIFFINTP.245
 520    continue                                                                 DIFFINTP.246
 525    continue                                                                 DIFFINTP.247
                                                                                 DIFFINTP.248
        zdiffst(in) = min(zdiffst(in),kmv(in)-1)                                 DIFFINTP.249
        zdiffst(in) = max(zdiffst(in),1)                                         DIFFINTP.250
                                                                                 DIFFINTP.251
       end                                                                       DIFFINTP.252
                                                                                 DIFFINTP.253
                                                                                 DIFFINTP.254
                                                                                 DIFFINTP.255
                                                                                 DIFFINTP.256
      subroutine indint(iii,jjj,sigij,rkij,a,mix,mjx,kl,in,IBGN,IEND,            DIFFINTP.257
     +  JBGN,JEND,kmax,mn,psb,ptop)                                              DIFFINTP.258
c                                                                                DIFFINTP.259
c  computation of the real index (rkij) corresponding to a given sigma value (si DIFFINTP.260
c                                                                                DIFFINTP.261
        implicit none                                                            DIFFINTP.262
        integer mix,mjx,kl,mn,in,iii,jjj                                         DIFFINTP.263
        REAL sigij(mix,mjx,kl),rkij(mix,mjx,kl,mn),a(kl),                        DIFFINTP.264
     +  psl,psr,x,f,psb(MIX,MJX),ptop                                            DIFFINTP.265
        integer IBGN,IEND,JBGN,JEND,k,kk,kmax(MIX,MJX),i,j                       DIFFINTP.266
        integer IB,IE,JB,JE,m,n                                                  DIFFINTP.267
                                                                                 DIFFINTP.268
      IF(IBGN.GT.2)THEN                                                          DIFFINTP.269
        IB=IBGN                                                                  DIFFINTP.270
        JB=JBGN                                                                  DIFFINTP.271
        IE=IEND                                                                  DIFFINTP.272
        JE=JEND                                                                  DIFFINTP.273
      ELSE                                                                       DIFFINTP.274
        IB=IBGN+1                                                                DIFFINTP.275
        JB=JBGN+1                                                                DIFFINTP.276
        IE=IEND-1                                                                DIFFINTP.277
        JE=JEND-1                                                                DIFFINTP.278
      ENDIF                                                                      DIFFINTP.279
                                                                                 DIFFINTP.280
        do j=JB,JE                                                               DIFFINTP.281
        do i=IB,IE                                                               DIFFINTP.282
        m = i+iii                                                                DIFFINTP.283
        n = j+jjj                                                                DIFFINTP.284
        psl = psb(i,j)                                                           DIFFINTP.285
        psr = psb(m,n)                                                           DIFFINTP.286
        if (sigij(i,j,kl).gt.a(kl)) then                                         DIFFINTP.287
C               the remote grid point is higher than the lo                      DIFFINTP.288
c                                                                                DIFFINTP.289
        kmax(i,j) = kl-1                                                         DIFFINTP.290
        do 120 kk = 1,kl-1                                                       DIFFINTP.291
          if ((kk.eq.1).and.(a(1).gt.sigij(i,j,1))) then                         DIFFINTP.292
            rkij(i,j,kk,in) = 1                                                  DIFFINTP.293
            goto 120                                                             DIFFINTP.294
          endif                                                                  DIFFINTP.295
        do k=max(1,kk-3),kl-1                                                    DIFFINTP.296
        if ((a(k).le.1.00001*sigij(i,j,kk)).and.                                 DIFFINTP.297
     f  (a(k).ge.0.99999*sigij(i,j,kk))) then                                    DIFFINTP.298
          rkij(i,j,kk,in) = k                                                    DIFFINTP.299
          goto 120                                                               DIFFINTP.300
        else                                                                     DIFFINTP.301
        if ((a(k).lt.sigij(i,j,kk)).and.                                         DIFFINTP.302
     f  (a(k+1).ge.sigij(i,j,kk))) then                                          DIFFINTP.303
          x = 1-(sigij(i,j,kk)-a(k))/(a(k+1)-a(k))                               DIFFINTP.304
          f = max((a(k+1)*psr+ptop)/(a(k)*psr+ptop),1.00001)                     DIFFINTP.305
          rkij(i,j,kk,in) = k+1./(1-f)*(exp(x*log(f))-f)                         DIFFINTP.306
          if (rkij(i,j,kk,in).ge.kl) rkij(i,j,kk,in) = kl-1.e-6                  DIFFINTP.307
          goto 120                                                               DIFFINTP.308
        endif                                                                    DIFFINTP.309
        endif                                                                    DIFFINTP.310
        enddo                                                                    DIFFINTP.311
        do k=kk,kl                                                               DIFFINTP.312
           rkij(i,j,k,in) = kl+0.01                                              DIFFINTP.313
        enddo                                                                    DIFFINTP.314
        kmax(i,j) = kk-1                                                         DIFFINTP.315
        goto 115                                                                 DIFFINTP.316
 120    continue                                                                 DIFFINTP.317
        rkij(i,j,kl,in) = kl+0.01                                                DIFFINTP.318
 115    continue                                                                 DIFFINTP.319
                                                                                 DIFFINTP.320
        else    ! the remote grid point is located lower                         DIFFINTP.321
                                                                                 DIFFINTP.322
        kmax(i,j) = kl                                                           DIFFINTP.323
        do 130 kk = 1,kl                                                         DIFFINTP.324
          if ((kk.eq.1).and.(a(1).ge.sigij(i,j,1))) then                         DIFFINTP.325
            rkij(i,j,kk,in) = 1                                                  DIFFINTP.326
            goto 130                                                             DIFFINTP.327
          endif                                                                  DIFFINTP.328
          do k=min(kl,kk+3),2,-1                                                 DIFFINTP.329
          if ((a(k).le.1.00001*sigij(i,j,kk)).and.                               DIFFINTP.330
     f  (a(k).ge.0.99999*sigij(i,j,kk))) then                                    DIFFINTP.331
            rkij(i,j,kk,in) = k                                                  DIFFINTP.332
            goto 130                                                             DIFFINTP.333
          else                                                                   DIFFINTP.334
          if ((a(k).ge.sigij(i,j,kk)).and.                                       DIFFINTP.335
     f  (a(k-1).lt.sigij(i,j,kk))) then                                          DIFFINTP.336
            x = -(sigij(i,j,kk)-a(k))/(a(k)-a(k-1))                              DIFFINTP.337
            f = max((a(k)*psr+ptop)/(a(k-1)*psr+ptop),1.0001)                    DIFFINTP.338
            rkij(i,j,kk,in) = k-1+1./(1-f)*(exp(x*log(f))-f)                     DIFFINTP.339
            goto 130                                                             DIFFINTP.340
          endif                                                                  DIFFINTP.341
          endif                                                                  DIFFINTP.342
        enddo                                                                    DIFFINTP.343
        rkij(i,j,kk,in) = 1                                                      DIFFINTP.344
 130    continue                                                                 DIFFINTP.345
        endif                                                                    DIFFINTP.346
                                                                                 DIFFINTP.347
        enddo                                                                    DIFFINTP.348
        enddo                                                                    DIFFINTP.349
        end                                                                      DIFFINTP.350
