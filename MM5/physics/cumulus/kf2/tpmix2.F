      subroutine tpmix2(p,thes,tu,qu,qliq,qice,qnewlq,qnewic,ratio2,             TPMIX2.1
     2                  XLV1,XLV0)                                               TPMIX2.2
c******** LOOKUP TABLE VARIABLES... ****************************                 TPMIX2.3
#     include <parame.incl>                                                      TPMIX2.4
#     include <parkfcu2.incl>                                                    TPMIX2.5
c***********************************************************************         TPMIX2.6
c     scaling pressure & tt table index                                          TPMIX2.7
c***********************************************************************         TPMIX2.8
c                                                                                TPMIX2.9
      tp=(p-pttop)*rdpr                                                          TPMIX2.10
      qq=tp-aint(tp)                                                             TPMIX2.11
      iptb=int(tp)+1                                                             TPMIX2.12
c                                                                                TPMIX2.13
c***********************************************************************         TPMIX2.14
c              base and scaling factor for the                                   TPMIX2.15
c***********************************************************************         TPMIX2.16
c                                                                                TPMIX2.17
c  scaling the & tt table index                                                  TPMIX2.18
      bth=(the0k(iptb+1)-the0k(iptb))*qq+the0k(iptb)                             TPMIX2.19
      tth=(thes-bth)*rdthk                                                       TPMIX2.20
      pp   =tth-aint(tth)                                                        TPMIX2.21
      ithtb=int(tth)+1                                                           TPMIX2.22
c                                                                                TPMIX2.23
      t00=ttab(ithtb  ,iptb  )                                                   TPMIX2.24
      t10=ttab(ithtb+1,iptb  )                                                   TPMIX2.25
      t01=ttab(ithtb  ,iptb+1)                                                   TPMIX2.26
      t11=ttab(ithtb+1,iptb+1)                                                   TPMIX2.27
c                                                                                TPMIX2.28
      q00=qstab(ithtb  ,iptb  )                                                  TPMIX2.29
      q10=qstab(ithtb+1,iptb  )                                                  TPMIX2.30
      q01=qstab(ithtb  ,iptb+1)                                                  TPMIX2.31
      q11=qstab(ithtb+1,iptb+1)                                                  TPMIX2.32
c                                                                                TPMIX2.33
c***********************************************************************         TPMIX2.34
c              parcel temperature                                                TPMIX2.35
c***********************************************************************         TPMIX2.36
c                                                                                TPMIX2.37
      temp=(t00+(t10-t00)*pp+(t01-t00)*qq                                        TPMIX2.38
     1         +(t00-t10-t01+t11)*pp*qq)                                         TPMIX2.39
c                                                                                TPMIX2.40
      qs=(q00+(q10-q00)*pp+(q01-q00)*qq                                          TPMIX2.41
     1         +(q00-q10-q01+q11)*pp*qq)                                         TPMIX2.42
c                                                                                TPMIX2.43
      DQ=QS-QU                                                                   23DEC04.3212
      IF(DQ.LE.0.)THEN                                                           23DEC04.3213
        QNEW=QU-QS                                                               23DEC04.3214
        QU=QS                                                                    23DEC04.3215
      ELSE                                                                       23DEC04.3216
c                                                                                23DEC04.3217
c   IF THE PARCEL IS SUBSATURATED, TEMPERATURE AND MIXING RATIO MUST BE          23DEC04.3218
c   ADJUSTED...IF LIQUID WATER IS PRESENT, IT IS ALLOWED TO EVAPORATE            23DEC04.3219
c                                                                                23DEC04.3220
        QNEW=0.                                                                  23DEC04.3221
        QTOT=QLIQ+QICE                                                           23DEC04.3222
c                                                                                23DEC04.3223
c   IF THERE IS ENOUGH LIQUID OR ICE TO SATURATE THE PARCEL, TEMP STAYS AT ITS   23DEC04.3224
c   WET BULB VALUE, VAPOR MIXING RATIO IS AT SATURATED LEVEL, AND THE MIXING     23DEC04.3225
c   RATIOS OF LIQUID AND ICE ARE ADJUSTED TO MAKE UP THE ORIGINAL SATURATION     23DEC04.3226
c   DEFICIT... OTHERWISE, ANY AVAILABLE LIQ OR ICE VAPORIZES AND APPROPRIATE     23DEC04.3227
c   ADJUSTMENTS TO PARCEL TEMP; VAPOR, LIQUID, AND ICE MIXING RATIOS ARE MADE.   23DEC04.3228
c                                                                                23DEC04.3229
c...subsaturated values only occur in calculations involving various mixtures o  23DEC04.3230
c...updraft and environmental air for estimation of entrainment and detrainment  23DEC04.3231
c...For these purposes, assume that reasonable estimates can be given using      23DEC04.3232
c...liquid water saturation calculations only - i.e., ignore the effect of the   23DEC04.3233
c...ice phase in this process only...will not affect conservative properties...  23DEC04.3234
c                                                                                23DEC04.3235
        IF(QTOT.GE.DQ)THEN                                                       23DEC04.3236
          qliq=qliq-dq*qliq/(qtot+1.e-10)                                        23DEC04.3237
          qice=qice-dq*qice/(qtot+1.e-10)                                        23DEC04.3238
          QU=QS                                                                  23DEC04.3239
        ELSE                                                                     23DEC04.3240
          RLL=XLV0-XLV1*TEMP                                                     23DEC04.3241
          CPP=1004.*(1.+0.89*QU)                                                 23DEC04.3242
          IF(QTOT.LT.1.E-10)THEN                                                 23DEC04.3243
c                                                                                23DEC04.3244
c...IF NO LIQUID WATER OR ICE IS AVAILABLE, TEMPERATURE IS GIVEN BY:             23DEC04.3245
            TEMP=TEMP+RLL*(DQ/(1.+DQ))/CPP                                       23DEC04.3246
          ELSE                                                                   23DEC04.3247
c                                                                                23DEC04.3248
c...IF SOME LIQ WATER/ICE IS AVAILABLE, BUT NOT ENOUGH TO ACHIEVE SATURATION,    23DEC04.3249
c   THE TEMPERATURE IS GIVEN BY:                                                 23DEC04.3250
c                                                                                23DEC04.3251
            TEMP=TEMP+RLL*((DQ-QTOT)/(1+DQ-QTOT))/CPP                            23DEC04.3252
            QU=QU+QTOT                                                           23DEC04.3253
            QTOT=0.                                                              23DEC04.3254
            QLIQ=0.                                                              23DEC04.3255
            QICE=0.                                                              23DEC04.3256
          ENDIF                                                                  23DEC04.3257
        ENDIF                                                                    23DEC04.3258
      ENDIF                                                                      23DEC04.3259
      TU=TEMP                                                                    23DEC04.3260
      qnewlq=qnew                                                                23DEC04.3261
      qnewic=0.                                                                  23DEC04.3262
c                                                                                TPMIX2.102
      return                                                                     TPMIX2.103
      end                                                                        TPMIX2.104
c******************************************************************************  TPMIX2.105
