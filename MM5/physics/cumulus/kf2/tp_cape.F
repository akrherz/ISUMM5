      subroutine tp_cape(p,thes,tu,qu)                                           TP_CAPE.1
c                                                                                TP_CAPE.2
      parameter(kfnt=250,kfnp=220)                                               TP_CAPE.3
c                                                                                TP_CAPE.4
      COMMON/KFLUT/ ttab(kfnt,kfnp),qstab(kfnt,kfnp),the0k(kfnp),                TP_CAPE.5
     *              alu(200),rdpr,rdthk,ptop                                     TP_CAPE.6
C***************************************************************                 TP_CAPE.7
c                                                                                TP_CAPE.8
c***********************************************************************         TP_CAPE.9
c     scaling pressure & tt table index                                          TP_CAPE.10
c***********************************************************************         TP_CAPE.11
c                                                                                TP_CAPE.12
      tp=(p-ptop)*rdpr                                                           TP_CAPE.13
      qq=tp-aint(tp)                                                             TP_CAPE.14
      iptb=int(tp)+1                                                             TP_CAPE.15
c                                                                                TP_CAPE.16
c***********************************************************************         TP_CAPE.17
c              base and scaling factor for the                                   TP_CAPE.18
c***********************************************************************         TP_CAPE.19
c                                                                                TP_CAPE.20
c  scaling the & tt table index                                                  TP_CAPE.21
      bth=(the0k(iptb+1)-the0k(iptb))*qq+the0k(iptb)                             TP_CAPE.22
      tth=(thes-bth)*rdthk                                                       TP_CAPE.23
      pp   =tth-aint(tth)                                                        TP_CAPE.24
      ithtb=int(tth)+1                                                           TP_CAPE.25
c                                                                                TP_CAPE.26
      t00=ttab(ithtb  ,iptb  )                                                   TP_CAPE.27
      t10=ttab(ithtb+1,iptb  )                                                   TP_CAPE.28
      t01=ttab(ithtb  ,iptb+1)                                                   TP_CAPE.29
      t11=ttab(ithtb+1,iptb+1)                                                   TP_CAPE.30
c                                                                                TP_CAPE.31
      q00=qstab(ithtb  ,iptb  )                                                  TP_CAPE.32
      q10=qstab(ithtb+1,iptb  )                                                  TP_CAPE.33
      q01=qstab(ithtb  ,iptb+1)                                                  TP_CAPE.34
      q11=qstab(ithtb+1,iptb+1)                                                  TP_CAPE.35
c                                                                                TP_CAPE.36
c***********************************************************************         TP_CAPE.37
c              parcel temperature                                                TP_CAPE.38
c***********************************************************************         TP_CAPE.39
c                                                                                TP_CAPE.40
      tu=(t00+(t10-t00)*pp+(t01-t00)*qq                                          TP_CAPE.41
     1         +(t00-t10-t01+t11)*pp*qq)                                         TP_CAPE.42
c                                                                                TP_CAPE.43
      qu=(q00+(q10-q00)*pp+(q01-q00)*qq                                          TP_CAPE.44
     1         +(q00-q10-q01+q11)*pp*qq)                                         TP_CAPE.45
c                                                                                TP_CAPE.46
      return                                                                     TP_CAPE.47
      end                                                                        TP_CAPE.48
