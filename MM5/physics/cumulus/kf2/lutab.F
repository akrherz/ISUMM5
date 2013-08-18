c***********************************************************************         LUTAB.1
c***********************************************************************         LUTAB.2
      subroutine lutab                                                           LUTAB.3
c                                                                                LUTAB.4
c  This subroutine is a lookup table.                                            LUTAB.5
c  Given a series of series of saturation equivalent potential                   LUTAB.6
c  temperatures, the temperature is calculated.                                  LUTAB.7
c                                                                                LUTAB.8
#     include <parame.incl>                                                      LUTAB.9
#     include <parkfcu2.incl>                                                    LUTAB.10
c equivalent potential temperature increment                                     LUTAB.11
      data dth/1./                                                               LUTAB.12
c minimum starting temp                                                          LUTAB.13
      data tmin/150./                                                            LUTAB.14
c tolerance for accuracy of temperature                                          LUTAB.15
      data toler/0.001/                                                          LUTAB.16
c top pressure (pascals)                                                         LUTAB.17
      pttop=5000.0                                                               23DEC04.3211
c bottom pressure (pascals)                                                      LUTAB.19
      pbot=110000.0                                                              LUTAB.20
                                                                                 LUTAB.21
c                                                                                LUTAB.22
c...define constants for calculation of saturation vapor pressure                LUTAB.23
c...according to buck (j. appl. meteo., december, 1981)...                       LUTAB.24
c                                                                                LUTAB.25
       aliq = 613.3                                                              LUTAB.26
       bliq = 17.502                                                             LUTAB.27
       cliq = 4780.8                                                             LUTAB.28
       dliq = 32.19                                                              LUTAB.29
c                                                                                LUTAB.30
c compute parameters                                                             LUTAB.31
c                                                                                LUTAB.32
c 1./(sat. equiv. theta increment)                                               LUTAB.33
      rdthk=1./dth                                                               LUTAB.34
c pressure increment                                                             LUTAB.35
      dpr=(pbot-pttop)/float(kfnp-1)                                             LUTAB.36
c 1./(pressure increment)                                                        LUTAB.37
      rdpr=1./dpr                                                                LUTAB.38
c compute the spread of thes                                                     LUTAB.39
c     thespd=dth*(kfnt-1)                                                        LUTAB.40
c                                                                                LUTAB.41
c calculate the starting sat. equiv. theta                                       LUTAB.42
c                                                                                LUTAB.43
      temp=tmin                                                                  LUTAB.44
      p=pttop-dpr                                                                LUTAB.45
      do 100 kp=1,kfnp                                                           LUTAB.46
      p=p+dpr                                                                    LUTAB.47
      es=aliq*exp((bliq*temp-cliq)/(temp-dliq))                                  LUTAB.48
      qs=0.622*es/(p-es)                                                         LUTAB.49
      pi=(1.e5/p)**(0.2854*(1.-0.28*qs))                                         LUTAB.50
      the0k(kp)=temp*pi*exp((3374.6525/temp-2.5403)*qs*                          LUTAB.51
     1         (1.+0.81*qs))                                                     LUTAB.52
 100  continue                                                                   LUTAB.53
                                                                                 LUTAB.54
c                                                                                LUTAB.55
c compute temperatures for each sat. equiv. potential temp.                      LUTAB.56
c                                                                                LUTAB.57
      p=pttop-dpr                                                                LUTAB.58
      do 110 kp=1,kfnp                                                           LUTAB.59
      thes=the0k(kp)-dth                                                         LUTAB.60
      p=p+dpr                                                                    LUTAB.61
      do 120 it=1,kfnt                                                           LUTAB.62
c define sat. equiv. pot. temp.                                                  LUTAB.63
      thes=thes+dth                                                              LUTAB.64
c iterate to find temperature                                                    LUTAB.65
c find initial guess                                                             LUTAB.66
      if(it.eq.1) then                                                           LUTAB.67
        tgues=tmin                                                               LUTAB.68
      else                                                                       LUTAB.69
        tgues=ttab(it-1,kp)                                                      LUTAB.70
      endif                                                                      LUTAB.71
      es=aliq*exp((bliq*tgues-cliq)/(tgues-dliq))                                LUTAB.72
      qs=0.622*es/(p-es)                                                         LUTAB.73
      pi=(1.e5/p)**(0.2854*(1.-0.28*qs))                                         LUTAB.74
      thgues=tgues*pi*exp((3374.6525/tgues-2.5403)*qs*                           LUTAB.75
     1         (1.+0.81*qs))                                                     LUTAB.76
      f0=thgues-thes                                                             LUTAB.77
      t1=tgues-0.5*f0                                                            LUTAB.78
      t0=tgues                                                                   LUTAB.79
      itcnt=0                                                                    LUTAB.80
c iteration loop                                                                 LUTAB.81
 130  continue                                                                   LUTAB.82
        es=aliq*exp((bliq*t1-cliq)/(t1-dliq))                                    LUTAB.83
        qs=0.622*es/(p-es)                                                       LUTAB.84
        pi=(1.e5/p)**(0.2854*(1.-0.28*qs))                                       LUTAB.85
        thtgs=t1*pi*exp((3374.6525/t1-2.5403)*qs*                                LUTAB.86
     1  (1.+0.81*qs))                                                            LUTAB.87
        f1=thtgs-thes                                                            LUTAB.88
        if(abs(f1).lt.toler)goto 140                                             LUTAB.89
        itcnt=itcnt+1                                                            LUTAB.90
        if(itcnt.gt.10) then                                                     LUTAB.91
          print *,' itcnt > 10',' it=',it,' p=',p,' t1=',t1,                     LUTAB.92
     1            ' thes=',thes                                                  LUTAB.93
          goto 140                                                               LUTAB.94
        endif                                                                    LUTAB.95
        dt=f1*(t1-t0)/(f1-f0)                                                    LUTAB.96
        t0=t1                                                                    LUTAB.97
        f0=f1                                                                    LUTAB.98
        t1=t1-dt                                                                 LUTAB.99
        goto 130                                                                 LUTAB.100
 140  continue                                                                   LUTAB.101
      ttab(it,kp)=t1                                                             LUTAB.102
      qstab(it,kp)=qs                                                            LUTAB.103
 120  continue                                                                   LUTAB.104
 110  continue                                                                   LUTAB.105
c                                                                                LUTAB.106
c lookup table for tlog(emix/aliq)                                               LUTAB.107
c                                                                                LUTAB.108
c set up intial values for lookup tables                                         LUTAB.109
c                                                                                LUTAB.110
       astrt=1.e-3                                                               LUTAB.111
       ainc=0.075                                                                LUTAB.112
c                                                                                LUTAB.113
       a1=astrt-ainc                                                             LUTAB.114
       do 200 i=1,200                                                            LUTAB.115
       a1=a1+ainc                                                                LUTAB.116
       alu(i)=alog(a1)                                                           LUTAB.117
 200   continue                                                                  LUTAB.118
c                                                                                LUTAB.119
      return                                                                     LUTAB.120
      end                                                                        LUTAB.121
