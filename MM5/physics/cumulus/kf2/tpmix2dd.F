       subroutine tpmix2dd(p,thes,ts,qs)                                         TPMIX2DD.1
c                                                                                TPMIX2DD.2
c******** LOOKUP TABLE VARIABLES... ****************************                 TPMIX2DD.3
#     include <parame.incl>                                                      TPMIX2DD.4
#     include <parkfcu2.incl>                                                    TPMIX2DD.5
c***********************************************************************         TPMIX2DD.6
c     scaling pressure & tt table index                                          TPMIX2DD.7
c***********************************************************************         TPMIX2DD.8
c                                                                                TPMIX2DD.9
      tp=(p-pttop)*rdpr                                                          TPMIX2DD.10
      qq=tp-aint(tp)                                                             TPMIX2DD.11
      iptb=int(tp)+1                                                             TPMIX2DD.12
c                                                                                TPMIX2DD.13
c***********************************************************************         TPMIX2DD.14
c              base and scaling factor for the                                   TPMIX2DD.15
c***********************************************************************         TPMIX2DD.16
c                                                                                TPMIX2DD.17
c  scaling the & tt table index                                                  TPMIX2DD.18
      bth=(the0k(iptb+1)-the0k(iptb))*qq+the0k(iptb)                             TPMIX2DD.19
      tth=(thes-bth)*rdthk                                                       TPMIX2DD.20
      pp   =tth-aint(tth)                                                        TPMIX2DD.21
      ithtb=int(tth)+1                                                           TPMIX2DD.22
c                                                                                TPMIX2DD.23
      t00=ttab(ithtb  ,iptb  )                                                   TPMIX2DD.24
      t10=ttab(ithtb+1,iptb  )                                                   TPMIX2DD.25
      t01=ttab(ithtb  ,iptb+1)                                                   TPMIX2DD.26
      t11=ttab(ithtb+1,iptb+1)                                                   TPMIX2DD.27
c                                                                                TPMIX2DD.28
      q00=qstab(ithtb  ,iptb  )                                                  TPMIX2DD.29
      q10=qstab(ithtb+1,iptb  )                                                  TPMIX2DD.30
      q01=qstab(ithtb  ,iptb+1)                                                  TPMIX2DD.31
      q11=qstab(ithtb+1,iptb+1)                                                  TPMIX2DD.32
c                                                                                TPMIX2DD.33
c***********************************************************************         TPMIX2DD.34
c              parcel temperature                                                TPMIX2DD.35
c***********************************************************************         TPMIX2DD.36
c                                                                                TPMIX2DD.37
      ts=(t00+(t10-t00)*pp+(t01-t00)*qq                                          TPMIX2DD.38
     1         +(t00-t10-t01+t11)*pp*qq)                                         TPMIX2DD.39
c                                                                                TPMIX2DD.40
      qs=(q00+(q10-q00)*pp+(q01-q00)*qq                                          TPMIX2DD.41
     1         +(q00-q10-q01+q11)*pp*qq)                                         TPMIX2DD.42
c                                                                                TPMIX2DD.43
      return                                                                     TPMIX2DD.44
      end                                                                        TPMIX2DD.45
