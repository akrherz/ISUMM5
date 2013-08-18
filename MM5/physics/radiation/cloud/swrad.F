      SUBROUTINE SWRAD(J,RTTEN,PSA,GSW,OSW,XLAT,XLONG,ALB,                       07NOV00.2189
     &                 T,QV,QC,QR,QI,QNI,QG,PP,INEST,HT)                         23DEC04.4145
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         SWRAD.3
C                                                                                SWRAD.4
C     TO CALCULATE SHORT-WAVE ABSORPTION AND SCATTERING IN CLEAR                 SWRAD.5
C     AIR AND REFLECTION AND ABSORPTION IN CLOUD LAYERS (STEPHENS,               SWRAD.6
C     1984)                                                                      SWRAD.7
C     CHANGES:                                                                   SWRAD.8
C       REDUCE EFFECTS OF ICE CLOUDS AND PRECIP ON LIQUID WATER PATH             SWRAD.9
C       ADD EFFECT OF GRAUPEL                                                    SWRAD.10
C                                                                                SWRAD.11
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         SWRAD.12
#     include <parame.incl>                                                      SWRAD.13
#     include <various.incl>                                                     SWRAD.14
#     include <nhcnst.incl>                                                      SWRAD.15
#     include <param2.incl>                                                      SWRAD.16
#     include <param3.incl>                                                      SWRAD.17
#     include <rpstar.incl>                                                      SWRAD.18
C                                                                                SWRAD.19
      DIMENSION RTTEN(MIXR,MJXR,MKXR)                                            SWRAD.20
      DIMENSION PSA(MIX,MJX),GSW(MIX,MJX),OSW(MIX,MJX),                          07NOV00.2190
     &         XLAT(MIX,MJX),XLONG(MIX,MJX),ALB(MIX,MJX),                        SWRAD.22
     &            T(MIX,MJX,MKX),                                                SWRAD.23
     &           QV(MIX,MJX,MKX), QC(MIXM,MJXM,MKXM),                            SWRAD.24
     &           PP(MIX,MJX,MKX), QR(MIXM,MJXM,MKXM),                            SWRAD.25
     &           QI(MIXIC,MJXIC,MKXIC),QNI(MIXIC,MJXIC,MKXIC),                   SWRAD.26
     &           QG(MIXIG,MJXIG,MKXIG)                                           SWRAD.27
C                                                                                23DEC04.4146
      real HT(MIX,MJX),hx(mix),hy(mix),aa(mix),ss(mix)                           23DEC04.4147
      REAL KORR                                                                  23DEC04.4148
C                                                                                SWRAD.28
      DIMENSION SDOWN(KXP1),XLWP(KXP1),XATP(KXP1),XWVP(KXP1)                     SWRAD.29
      DIMENSION P(MKX),DZ(MKX),RO(MKX)                                           SWRAD.30
C                                                                                SWRAD.31
      DIMENSION ALBTAB(4,5),ABSTAB(4,5),XMUVAL(4)                                SWRAD.32
C     READ IN DATA FOR MU = .0 .2 .5 1.0 AND LOG10(W G/M/M)=0 1 2 3 4            SWRAD.33
      DATA ALBTAB/0.,0.,0.,0.,                                                   SWRAD.34
     -     69.,58.,40.,15.,                                                      SWRAD.35
     -     90.,80.,70.,60.,                                                      SWRAD.36
     -     94.,90.,82.,78.,                                                      SWRAD.37
     -     96.,92.,85.,80./                                                      SWRAD.38
      DATA ABSTAB/0.,0.,0.,0.,                                                   SWRAD.39
     -     0.,2.5,4.,5.,                                                         SWRAD.40
     -     0.,2.6,7.,10.,                                                        SWRAD.41
     -     0.,3.3,10.,14.,                                                       SWRAD.42
     -     0.,3.7,10.,15./                                                       SWRAD.43
      DATA XMUVAL/0.,0.2,0.5,1.0/                                                SWRAD.44
C                                                                                SWRAD.45
      maxneslev = 0                                                              23DEC04.4149
      do i=1,NSTTOT                                                              23DEC04.4150
        maxneslev = max(maxneslev,levidn(i))                                     23DEC04.4151
      enddo                                                                      23DEC04.4152
C                                                                                23DEC04.4153
c  Orographic effects on surface radiation - calculation of orography slope      23DEC04.4154
                                                                                 23DEC04.4155
      if (levidn(inest).ge.levslp) then                                          23DEC04.4156
        pi = 4.*atan(1.)                                                         23DEC04.4157
        absch = 0                                                                23DEC04.4158
        abschintp = 0                                                            23DEC04.4159
        if ((j.ge.2).and.(j.le.jlx-1)) then                                      23DEC04.4160
          do i=1,ilx                                                             23DEC04.4161
             hx(i) = (ht(i,j+1)-ht(i,j-1))/(2*g*dx)                              23DEC04.4162
          enddo                                                                  23DEC04.4163
        else if (j.eq.1) then                                                    23DEC04.4164
          do i=1,ilx                                                             23DEC04.4165
             hx(i) = (ht(i,j+1)-ht(i,j))/(g*dx)                                  23DEC04.4166
          enddo                                                                  23DEC04.4167
        else if (j.eq.jlx) then                                                  23DEC04.4168
          do i=1,ilx                                                             23DEC04.4169
             hx(i) = (ht(i,j)-ht(i,j-1))/(g*dx)                                  23DEC04.4170
          enddo                                                                  23DEC04.4171
        endif                                                                    23DEC04.4172
        do i=2,ilx-1                                                             23DEC04.4173
          hy(i) = (ht(i+1,j)-ht(i-1,j))/(2*g*dx)                                 23DEC04.4174
        enddo                                                                    23DEC04.4175
        hy(1) = (ht(2,j)-ht(1,j))/(g*dx)                                         23DEC04.4176
        hy(ilx) = (ht(ilx,j)-ht(ilx-1,j))/(g*dx)                                 23DEC04.4177
        do i=1,ilx                                                               23DEC04.4178
          if ((hx(i).eq.0).and.(hy(i).eq.0)) then                                23DEC04.4179
            aa(i) = 0                                                            23DEC04.4180
          else                                                                   23DEC04.4181
            aa(i) = atan2(hx(i),hy(i))+pi    ! azimuth angle                     23DEC04.4182
          endif                                                                  23DEC04.4183
            ss(i) = atan((hx(i)**2+hy(i)**2)**.5)  ! slope                       23DEC04.4184
        enddo                                                                    23DEC04.4185
      endif                                                                      23DEC04.4186
                                                                                 23DEC04.4187
      DO 300 I=1,ILX                                                             SWRAD.46
        GSW(I,J)=0.0                                                             SWRAD.47
        OSW(I,J)=0.0                                                             07NOV00.2191
C                                                                                SWRAD.48
        SOLTOP=SOLCON                                                            SWRAD.49
        XT24=AMOD(XTIME+RADFRQ*0.5,1440.)                                        SWRAD.50
        TLOCTM=GMT+XT24/60.+XLONG(I,J)/15.                                       SWRAD.51
        HRANG=15.*(TLOCTM-12.)*DEGRAD                                            SWRAD.52
        XXLAT=XLAT(I,J)*DEGRAD                                                   SWRAD.53
        CSZA=SIN(XXLAT)*SIN(DECLIN)+COS(XXLAT)*COS(DECLIN)*COS(HRANG)            SWRAD.54
C     RETURN IF NIGHT                                                            SWRAD.55
        IF(CSZA.LE.1.E-9)GOTO 7                                                  SWRAD.56
C maximum length of orographic shadow: 25 km                                     23DEC04.4188
      shadl = min(25000.,2500./csza)                                             23DEC04.4189
C                                                                                SWRAD.57
        DO 8 K=1,MKX                                                             SWRAD.58
          P(K)=0.001*(PR0(I,J,K)+PP(I,J,K))                                      SWRAD.59
          RO(K)=P(K)*1000./(R*T(I,J,K))                                          SWRAD.60
          DZ(K)=PSA(I,J)*DSIGMA(K)*1000./(RHO0(I,J,K)*G)                         SWRAD.61
          XWVP(K)=RO(K)*QV(I,J,K)*DZ(K)*1000.                                    SWRAD.62
C     KG/M**2                                                                    SWRAD.63
          XATP(K)=RO(K)*DZ(K)                                                    SWRAD.64
    8   CONTINUE                                                                 SWRAD.65
C                                                                                SWRAD.66
C     G/M**2                                                                     SWRAD.67
C     REDUCE WEIGHT OF LIQUID AND ICE IN SHORT-WAVE SCHEME                       SWRAD.68
C      ADD GRAUPEL EFFECT (ASSUMED SAME AS RAIN)                                 SWRAD.69
C                                                                                SWRAD.70
        IF(ICLOUD(INEST).EQ.0)THEN                                               SWRAD.71
          DO K=1,MKX                                                             SWRAD.72
            XLWP(K)=0.                                                           SWRAD.73
          ENDDO                                                                  SWRAD.74
        ELSE                                                                     SWRAD.75
          IF(IICE.EQ.0)THEN                                                      SWRAD.76
            DO K=1,MKX                                                           SWRAD.77
              IF(T(I,J,K).LT.273.16)THEN                                         SWRAD.78
                QCWI=0.1*QC(I,J,K)                                               SWRAD.79
                QRWS=0.02*QR(I,J,K)                                              SWRAD.80
              ELSE                                                               SWRAD.81
                QCWI=QC(I,J,K)                                                   SWRAD.82
                QRWS=0.05*QR(I,J,K)                                              SWRAD.83
              ENDIF                                                              SWRAD.84
              XLWP(K)=RO(K)*(QCWI+QRWS)*DZ(K)*1000.                              SWRAD.85
            ENDDO                                                                SWRAD.86
          ELSE                                                                   SWRAD.87
            IF(IICEG.EQ.0)THEN                                                   SWRAD.88
              DO K=1,MKX                                                         SWRAD.89
                XLWP(K)=RO(K)*1000.*DZ(K)*(QC(I,J,K)+0.1*QI(I,J,K)+0.05*         SWRAD.90
     +                  QR(I,J,K)+0.02*QNI(I,J,K))                               SWRAD.91
              ENDDO                                                              SWRAD.92
            ELSE                                                                 SWRAD.93
              DO K=1,MKX                                                         SWRAD.94
                XLWP(K)=RO(K)*1000.*DZ(K)*(QC(I,J,K)+0.1*QI(I,J,K)+0.05*         SWRAD.95
     +                  QR(I,J,K)+0.02*QNI(I,J,K)+0.05*QG(I,J,K))                SWRAD.96
              ENDDO                                                              SWRAD.97
            ENDIF                                                                SWRAD.98
          ENDIF                                                                  SWRAD.99
        ENDIF                                                                    SWRAD.100
C                                                                                SWRAD.101
        XMU=CSZA                                                                 SWRAD.102
        SDOWN(1)=SOLTOP*XMU                                                      SWRAD.103
C     SET WW (G/M**2) LIQUID WATER PATH INTEGRATED DOWN                          SWRAD.104
C     SET UV (G/M**2) WATER VAPOR PATH INTEGRATED DOWN                           SWRAD.105
        WW=0.                                                                    SWRAD.106
        UV=0.                                                                    SWRAD.107
        OLDALB=0.                                                                SWRAD.108
        OLDABC=0.                                                                SWRAD.109
        TOTABS=0.                                                                SWRAD.110
C     CONTRIBUTIONS DUE TO CLEAR AIR AND CLOUD                                   SWRAD.111
        DSCA=0.                                                                  SWRAD.112
        DABS=0.                                                                  SWRAD.113
        DSCLD=0.                                                                 SWRAD.114
C                                                                                SWRAD.115
        DO 200 K=1,MKX                                                           SWRAD.116
          WW=WW+XLWP(K)                                                          SWRAD.117
          UV=UV+XWVP(K)                                                          SWRAD.118
C     WGM IS WW/COS(THETA) (G/M**2)                                              SWRAD.119
C     UGCM IS UV/COS(THETA) (G/CM**2)                                            SWRAD.120
          WGM=WW/XMU                                                             SWRAD.121
          UGCM=UV*0.0001/XMU                                                     SWRAD.122
C                                                                                SWRAD.123
          OLDABS=TOTABS                                                          SWRAD.124
C     WATER VAPOR ABSORPTION AS IN LACIS AND HANSEN (1974)                       SWRAD.125
          TOTABS=2.9*UGCM/((1.+141.5*UGCM)**0.635+5.925*UGCM)                    SWRAD.126
C     APPROXIMATE RAYLEIGH + AEROSOL SCATTERING                                  SWRAD.127
          XSCA=1.E-5*XATP(K)/XMU                                                 SWRAD.128
C     LAYER VAPOR ABSORPTION DONE FIRST                                          SWRAD.129
          XABS=(TOTABS-OLDABS)*(SDOWN(1)-DSCLD-DSCA)/SDOWN(K)                    SWRAD.130
          IF(XABS.LT.0.)XABS=0.                                                  SWRAD.131
C                                                                                SWRAD.132
          ALW=ALOG10(WGM+1.)                                                     SWRAD.133
          IF(ALW.GT.3.999)ALW=3.999                                              SWRAD.134
C                                                                                SWRAD.135
          DO 101 II=1,3                                                          SWRAD.136
            IF(XMU.GT.XMUVAL(II))THEN                                            SWRAD.137
              IIL=II                                                             SWRAD.138
              IU=II+1                                                            SWRAD.139
              XI=(XMU-XMUVAL(II))/(XMUVAL(II+1)-XMUVAL(II))+FLOAT(IIL)           SWRAD.140
            ENDIF                                                                SWRAD.141
  101     CONTINUE                                                               SWRAD.142
C                                                                                SWRAD.143
          JJL=IFIX(ALW)+1                                                        SWRAD.144
          JU=JJL+1                                                               SWRAD.145
          YJ=ALW+1.                                                              SWRAD.146
C     CLOUD ALBEDO                                                               SWRAD.147
          ALBA=(ALBTAB(IU,JU)*(XI-IIL)*(YJ-JJL)                                  SWRAD.148
     &         +ALBTAB(IIL,JU)*(IU-XI)*(YJ-JJL)                                  SWRAD.149
     &         +ALBTAB(IU,JJL)*(XI-IIL)*(JU-YJ)                                  SWRAD.150
     &         +ALBTAB(IIL,JJL)*(IU-XI)*(JU-YJ))                                 SWRAD.151
     &        /((IU-IIL)*(JU-JJL))                                               SWRAD.152
C     CLOUD ABSORPTION                                                           SWRAD.153
          ABSC=(ABSTAB(IU,JU)*(XI-IIL)*(YJ-JJL)                                  SWRAD.154
     &         +ABSTAB(IIL,JU)*(IU-XI)*(YJ-JJL)                                  SWRAD.155
     &         +ABSTAB(IU,JJL)*(XI-IIL)*(JU-YJ)                                  SWRAD.156
     &         +ABSTAB(IIL,JJL)*(IU-XI)*(JU-YJ))                                 SWRAD.157
     &        /((IU-IIL)*(JU-JJL))                                               SWRAD.158
C     LAYER ALBEDO AND ABSORPTION                                                SWRAD.159
          XALB=(ALBA-OLDALB)*(SDOWN(1)-DSCA-DABS)/SDOWN(K)                       SWRAD.160
          XABSC=(ABSC-OLDABC)*(SDOWN(1)-DSCA-DABS)/SDOWN(K)                      SWRAD.161
          IF(XALB.LT.0.)XALB=0.                                                  SWRAD.162
          IF(XABSC.LT.0.)XABSC=0.                                                SWRAD.163
          DSCLD=DSCLD+(XALB+XABSC)*SDOWN(K)*0.01                                 SWRAD.164
          DSCA=DSCA+XSCA*SDOWN(K)                                                SWRAD.165
          DABS=DABS+XABS*SDOWN(K)                                                SWRAD.166
          OLDALB=ALBA                                                            SWRAD.167
          OLDABC=ABSC                                                            SWRAD.168
C     LAYER TRANSMISSIVITY                                                       SWRAD.169
          TRANS0=100.-XALB-XABSC-XABS*100.-XSCA*100.                             SWRAD.170
          IF(TRANS0.LT.1.)THEN                                                   SWRAD.171
            FF=99./(XALB+XABSC+XABS*100.+XSCA*100.)                              SWRAD.172
            XALB=XALB*FF                                                         SWRAD.173
            XABSC=XABSC*FF                                                       SWRAD.174
            XABS=XABS*FF                                                         SWRAD.175
            XSCA=XSCA*FF                                                         SWRAD.176
            TRANS0=1.                                                            SWRAD.177
          ENDIF                                                                  SWRAD.178
          SDOWN(K+1)=AMAX1(1.E-9,SDOWN(K)*TRANS0*0.01)                           SWRAD.179
          OSW(I,J)=OSW(I,J)+SDOWN(K)*(XALB+XSCA)*0.01                            07NOV00.2192
          RTTEN(I,J,K)=RTTEN(I,J,K)+SDOWN(K)*(XABSC+XABS*100.)*0.01/(            SWRAD.180
     +                 RO(K)*CP*DZ(K))                                           SWRAD.181
  200   CONTINUE                                                                 SWRAD.182
C                                                                                SWRAD.183
c Orography effects                                                              23DEC04.4190
        if (levidn(inest).ge.levslp) then                                        23DEC04.4191
        absch = 0                                                                23DEC04.4192
        abschintp = 0                                                            23DEC04.4193
                                                                                 23DEC04.4194
c  estimate diffuse part of solar radiation - fitted to unpublished radiation m  23DEC04.4195
c  taken at the University of Munich                                             23DEC04.4196
                                                                                 23DEC04.4197
        diffus = min(1.,1/(max(0.1,2.619-2.337*log                               23DEC04.4198
     f      (log(sdown(1)/max(sdown(kxp1),1.e-10))))))                           23DEC04.4199
        if (diffus.le.0) print*, 'Error in swradoro', diffus                     23DEC04.4200
        if (diffus.eq.1) then                                                    23DEC04.4201
            korr = 1                                                             23DEC04.4202
            goto 140                                                             23DEC04.4203
        endif                                                                    23DEC04.4204
                                                                                 23DEC04.4205
c cosine of zenith angle over sloping topography                                 23DEC04.4206
                                                                                 23DEC04.4207
        cszk = ((SIN(XXLAT)*COS(HRANG))*(-cos(aa(i))*sin(ss(i)))-                23DEC04.4208
     f    SIN(HRANG)*(sin(aa(i))*sin(ss(i)))+(COS(XXLAT)*COS(HRANG))*            23DEC04.4209
     f    cos(ss(i)))*COS(DECLIN)+(COS(XXLAT)*(cos(aa(i))*sin(ss(i)))+           23DEC04.4210
     f    SIN(XXLAT)*cos(ss(i)))*SIN(DECLIN)                                     23DEC04.4211
        IF(CSZK.LE.1.E-4) cszk = 0                                               23DEC04.4212
                                                                                 23DEC04.4213
                                                                                 23DEC04.4214
#ifndef MPP1                                                                     23DEC04.4215
c  Topographic shadowing                                                         23DEC04.4216
                                                                                 23DEC04.4217
          IF ( OROSHAW .EQ. 1 ) THEN                                             23DEC04.4218
        if (cszk.gt.1.e-8) then                                                  23DEC04.4219
        argu=(csza*sin(XXLAT)-sin(DECLIN))/(sin(acos(csza))*cos(XXLAT))          23DEC04.4220
        if (argu.gt.1) argu = 1                                                  23DEC04.4221
        if (argu.lt.-1) argu = -1                                                23DEC04.4222
        azi = sign(acos(argu),sin(HRANG))  ! azimuth angle of the sun            23DEC04.4223
                                                                                 23DEC04.4224
        if ((azi.gt.0.75*pi).or.(azi.lt.-0.75*pi)) then                          23DEC04.4225
        do ii = i+1,ilx                                                          23DEC04.4226
        rj = j + (ii-i)*tan(pi+azi)                                              23DEC04.4227
        j1 = int(rj)                                                             23DEC04.4228
        j2 = j1+1                                                                23DEC04.4229
        if ((j1.lt.1).or.(j2.gt.jlx)) goto 110                                   23DEC04.4230
        gew = rj-j1                                                              23DEC04.4231
        dxabs = dx*sqrt((ii-i)**2+(rj-j)**2)                                     23DEC04.4232
        selev=sin(atan((gew*ht(ii,j2)+(1-gew)*ht(ii,j1)-ht(i,j))/                23DEC04.4233
     f        (g*dxabs)))                                                        23DEC04.4234
        if (selev.ge.csza) then                                                  23DEC04.4235
          cszk = 0                                                               23DEC04.4236
          absch = 1                                                              23DEC04.4237
          abschintp = 1                                                          23DEC04.4238
          goto 130                                                               23DEC04.4239
        endif                                                                    23DEC04.4240
        if (dxabs.gt.shadl) goto 130                                             23DEC04.4241
        enddo                                                                    23DEC04.4242
        goto 110                                                                 23DEC04.4243
        else if (azi.lt.-0.25*pi) then                                           23DEC04.4244
        do jj = j+1,jlx                                                          23DEC04.4245
        ri = i + (jj-j)*tan(pi/2.-azi)                                           23DEC04.4246
        i1 = int(ri)                                                             23DEC04.4247
        i2 = i1+1                                                                23DEC04.4248
        if ((i1.lt.1).or.(i2.gt.ilx)) goto 110                                   23DEC04.4249
        gew = ri-i1                                                              23DEC04.4250
        dxabs = dx*sqrt((jj-j)**2+(ri-i)**2)                                     23DEC04.4251
        selev=sin(atan((gew*ht(i2,jj)+(1-gew)*ht(i1,jj)-ht(i,j))/                23DEC04.4252
     f        (g*dxabs)))                                                        23DEC04.4253
        if (selev.ge.csza) then                                                  23DEC04.4254
          cszk = 0                                                               23DEC04.4255
          absch = 1                                                              23DEC04.4256
          abschintp = 1                                                          23DEC04.4257
          goto 130                                                               23DEC04.4258
        endif                                                                    23DEC04.4259
        if (dxabs.gt.shadl) goto 130                                             23DEC04.4260
        enddo                                                                    23DEC04.4261
        goto 110                                                                 23DEC04.4262
        else if (azi.lt.0.25*pi) then                                            23DEC04.4263
        do ii = i-1,1,-1                                                         23DEC04.4264
        rj = j + (ii-i)*tan(azi)                                                 23DEC04.4265
        j1 = int(rj)                                                             23DEC04.4266
        j2 = j1+1                                                                23DEC04.4267
        if ((j1.lt.1).or.(j2.gt.jlx)) goto 110                                   23DEC04.4268
        gew = rj-j1                                                              23DEC04.4269
        dxabs = dx*sqrt((ii-i)**2+(rj-j)**2)                                     23DEC04.4270
        selev=sin(atan((gew*ht(ii,j2)+(1-gew)*ht(ii,j1)-ht(i,j))/                23DEC04.4271
     f        (g*dxabs)))                                                        23DEC04.4272
        if (selev.ge.csza) then                                                  23DEC04.4273
          cszk = 0                                                               23DEC04.4274
          absch = 1                                                              23DEC04.4275
          abschintp = 1                                                          23DEC04.4276
          goto 130                                                               23DEC04.4277
        endif                                                                    23DEC04.4278
        if (dxabs.gt.shadl) goto 130                                             23DEC04.4279
        enddo                                                                    23DEC04.4280
        goto 110                                                                 23DEC04.4281
        else                                                                     23DEC04.4282
        do jj = j-1,1,-1                                                         23DEC04.4283
        ri = i - (jj-j)*tan(pi/2.+azi)                                           23DEC04.4284
        i1 = int(ri)                                                             23DEC04.4285
        i2 = i1+1                                                                23DEC04.4286
        if ((i1.lt.1).or.(i2.gt.ilx)) goto 110                                   23DEC04.4287
        gew = ri-i1                                                              23DEC04.4288
        dxabs = dx*sqrt((jj-j)**2+(ri-i)**2)                                     23DEC04.4289
        selev=sin(atan((gew*ht(i2,jj)+(1-gew)*ht(i1,jj)-ht(i,j))/                23DEC04.4290
     f      (g*dxabs)))                                                          23DEC04.4291
        if (selev.ge.csza) then                                                  23DEC04.4292
          cszk = 0                                                               23DEC04.4293
          absch = 1                                                              23DEC04.4294
          abschintp = 1                                                          23DEC04.4295
          goto 130                                                               23DEC04.4296
        endif                                                                    23DEC04.4297
        if (dxabs.gt.shadl) goto 130                                             23DEC04.4298
        enddo                                                                    23DEC04.4299
        goto 110                                                                 23DEC04.4300
        endif                                                                    23DEC04.4301
        endif                                                                    23DEC04.4302
        goto 130                                                                 23DEC04.4303
c  interpolate shadowing mask from next coarser domain if lateral domain bounda  23DEC04.4304
c  is closer than search distance (shadl)                                        23DEC04.4305
                                                                                 23DEC04.4306
 110    continue                                                                 23DEC04.4307
                                                                                 23DEC04.4308
                                                                                 23DEC04.4309
        if (levidn(inest)-1.ge.levslp) then                                      23DEC04.4310
          if (shadint(i,j,inest).eq.1) then                                      23DEC04.4311
            cszk = 0                                                             23DEC04.4312
            absch = 1                                                            23DEC04.4313
          endif                                                                  23DEC04.4314
        endif                                                                    23DEC04.4315
                                                                                 23DEC04.4316
 130    continue                                                                 23DEC04.4317
                                                                                 23DEC04.4318
           ENDIF                                                                 23DEC04.4319
#endif                                                                           23DEC04.4320
                                                                                 23DEC04.4321
c  Korrekturfaktor                                                               23DEC04.4322
                                                                                 23DEC04.4323
        korr = diffus + (1-diffus)*cszk/max(1.e-2,csza)                          23DEC04.4324
                                                                                 23DEC04.4325
 140    continue                                                                 23DEC04.4326
                                                                                 23DEC04.4327
#ifndef MPP1                                                                     23DEC04.4328
c Interpolate shadowing mask into nested domains                                 23DEC04.4329
                                                                                 23DEC04.4330
          IF ( OROSHAW .EQ. 1 ) THEN                                             23DEC04.4331
                                                                                 23DEC04.4332
        if ((inest.lt.NSTTOT).and.(levidn(inest).lt.maxneslev)) then             23DEC04.4333
                                                                                 23DEC04.4334
        do 50 i1=inest+1,NSTTOT                                                  23DEC04.4335
        if (numnc(i1).eq.inest) then                                             23DEC04.4336
       if ((i.ge.nesti(i1)).and.(i.le.nesti(i1)+(nestix(i1)-1)/3-1).and.         23DEC04.4337
     f (j.ge.nestj(i1)).and.(j.le.nestj(i1)+(nestjx(i1)-1)/3-1)) then            23DEC04.4338
        do ii = 2+(I-nesti(i1))*3-1 , 2+(I-nesti(i1))*3+1                        23DEC04.4339
        do jj = 2+(J-nestj(i1))*3-1 , 2+(J-nestj(i1))*3+1                        23DEC04.4340
        shadint(ii,jj,i1) = abschintp  ! Shadowing mask is not interp recursive  23DEC04.4341
        enddo                                                                    23DEC04.4342
        enddo                                                                    23DEC04.4343
        endif                                                                    23DEC04.4344
        endif                                                                    23DEC04.4345
 50     continue                                                                 23DEC04.4346
        endif                                                                    23DEC04.4347
                                                                                 23DEC04.4348
           ENDIF                                                                 23DEC04.4349
#endif                                                                           23DEC04.4350
                                                                                 23DEC04.4351
        GSW(I,J)=(1.-ALB(I,J))*SDOWN(KXP1)*korr                                  23DEC04.4352
        OSW(I,J)=OSW(I,J)+ALB(I,J)*SDOWN(KXP1)*korr                              23DEC04.4353
                                                                                 23DEC04.4354
        else  ! orographic effects are not taken into account                    23DEC04.4355
                                                                                 23DEC04.4356
        GSW(I,J)=(1.-ALB(I,J))*SDOWN(KXP1)                                       SWRAD.184
        OSW(I,J)=OSW(I,J)+ALB(I,J)*SDOWN(KXP1)                                   07NOV00.2193
                                                                                 23DEC04.4357
        endif                                                                    23DEC04.4358
                                                                                 23DEC04.4359
    7   CONTINUE                                                                 SWRAD.185
C                                                                                SWRAD.186
  300 CONTINUE                                                                   SWRAD.187
C                                                                                SWRAD.188
      RETURN                                                                     SWRAD.189
      END                                                                        SWRAD.190
                                                                                 SWRAD.191
