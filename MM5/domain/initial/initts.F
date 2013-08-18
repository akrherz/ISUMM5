#ifndef MPP1                                                                     INITTS.1
      SUBROUTINE INITTS(INEST,IX,JX)                                             INITTS.2
      IMPLICIT NONE                                                              INITTS.3
C                                                                                INITTS.4
C-----INITIALIZE LOCATIONS FOR TIME SERIES OUTPUT                                INITTS.5
C                                                                                INITTS.6
#     include <parame.incl>                                                      INITTS.7
#     include <param2.incl>                                                      INITTS.8
#     include <param3.incl>                                                      INITTS.9
#     include <point2d.incl>                                                     INITTS.10
      INTEGER INEST,NTS,I,J,IX,JX                                                INITTS.11
      REAL    COSLAT,DSQMIN,DSQ                                                  INITTS.12
                                                                                 INITTS.13
      print 9000, INEST                                                          INITTS.14
      print 9010                                                                 INITTS.15
      do nts=1,itsdim                                                            INITTS.16
          if(tslat(nts).lt.900..and.tslon(nts).lt.900.)then                      INITTS.17
C.....find point nearest (tslat,tslon)                                           INITTS.18
            coslat=cos(tslat(nts)*degrad)                                        INITTS.19
            dsqmin=100.                                                          INITTS.20
            do j=1,jx-1                                                          INITTS.21
            do i=1,ix-1                                                          INITTS.22
              dsq=(xlat(i,j)-tslat(nts))*(xlat(i,j)-tslat(nts))+                 INITTS.23
     -            (xlong(i,j)-tslon(nts))*(xlong(i,j)-tslon(nts))*               INITTS.24
     -            coslat*coslat                                                  INITTS.25
              if(dsq.lt.dsqmin)then                                              INITTS.26
                itsn(inest,nts)=i                                                INITTS.27
                jtsn(inest,nts)=j                                                INITTS.28
                dsqmin=dsq                                                       INITTS.29
              endif                                                              INITTS.30
            enddo                                                                INITTS.31
            enddo                                                                INITTS.32
            if (itsn(inest,nts).ne.1 .and. jtsn(inest,nts).ne.1 ) then           INITTS.33
               print 9020, NTS,TSLAT(NTS), TSLON(NTS),                           INITTS.34
     -                     itsn(inest,nts),jtsn(inest,nts)                       INITTS.35
            else                                                                 INITTS.36
               print 9030, NTS, TSLAT(NTS), TSLON(NTS)                           INITTS.37
            endif                                                                INITTS.38
          endif                                                                  INITTS.39
      enddo                                                                      INITTS.40
                                                                                 INITTS.41
9000  FORMAT(1x,'INITIALIZING T-S FOR DOMAIN ',I2)                               INITTS.42
9010  FORMAT(1x,'T-S No',8x,'(LAT/LONG)',8x,'(I,J)')                             INITTS.43
9020  FORMAT(5x,i2,2x,'(',F8.2,',',F8.2,')',2x,'(',I4,',',I4,')')                INITTS.44
9030  FORMAT(5x,'T-S No ',I2,2x,'(',F8.2,',',F8.2,')',                           INITTS.45
     -  ' IS OUTSIDE THE DOMAIN')                                                INITTS.46
                                                                                 INITTS.47
      return                                                                     INITTS.48
      end                                                                        INITTS.49
#else                                                                            INITTS.50
      SUBROUTINE INITTS(INEST,IX,JX)                                             INITTS.51
      IMPLICIT NONE                                                              INITTS.52
C                                                                                INITTS.53
C-----INITIALIZE LOCATIONS FOR TIME SERIES OUTPUT                                INITTS.54
C                                                                                INITTS.55
#     include <parame.incl>                                                      INITTS.56
#     include <param2.incl>                                                      INITTS.57
#     include <param3.incl>                                                      INITTS.58
#     include <point2d.incl>                                                     INITTS.59
      INTEGER INEST,NTS,I,J,IX,JX                                                INITTS.60
      LOGICAL dm_ionode                                                          INITTS.61
      EXTERNAL dm_ionode                                                         INITTS.62
      REAL    COSLAT,DSQMIN,DSQ                                                  INITTS.63
      INTEGER GLEN_X(2)                                                          INITTS.64
      INTEGER IL, JL                                                             INITTS.65
      REAL XLAT_G(MIX_G*MJX_G)                                                   INITTS.66
      REAL XLONG_G(MIX_G*MJX_G)                                                  INITTS.67
      IL = NESTIX(INEST)                                                         INITTS.68
      JL = NESTJX(INEST)                                                         INITTS.69
      GLEN_X(1) = IL                                                             INITTS.70
      GLEN_X(2) = JL                                                             INITTS.71
      CALL RSL_WRITE(XLAT_G,IO2D_IJ_INTERNAL,XLAT,DOMAINS(INEST),                INITTS.72
     +               RSL_REAL,GLEN_X,LLEN)                                       INITTS.73
      CALL RSL_WRITE(XLONG_G,IO2D_IJ_INTERNAL,XLONG,DOMAINS(INEST),              INITTS.74
     +               RSL_REAL,GLEN_X,LLEN)                                       INITTS.75
                                                                                 INITTS.76
      print 9000, INEST                                                          INITTS.77
      print 9010                                                                 INITTS.78
      IF ( dm_ionode() ) THEN                                                    INITTS.79
        do nts=1,itsdim                                                          INITTS.80
          if(tslat(nts).lt.900..and.tslon(nts).lt.900.)then                      INITTS.81
C.....find point nearest (tslat,tslon)                                           INITTS.82
            coslat=cos(tslat(nts)*degrad)                                        INITTS.83
            dsqmin=100.                                                          INITTS.84
            do j=1,jx-1                                                          INITTS.85
            do i=1,ix-1                                                          INITTS.86
              dsq=(xlat_g(i+(j-1)*IL)-tslat(nts))*(xlat_g(i+(j-1)*IL)-           INITTS.87
     &            tslat(nts))+                                                   INITTS.88
     &            (xlong_g(i+(j-1)*IL)-tslon(nts))*(xlong_g(i+(j-1)*IL)          INITTS.89
     &            -tslon(nts))*coslat*coslat                                     INITTS.90
              if(dsq.lt.dsqmin)then                                              INITTS.91
                itsn(inest,nts)=i                                                INITTS.92
                jtsn(inest,nts)=j                                                INITTS.93
                dsqmin=dsq                                                       INITTS.94
              endif                                                              INITTS.95
            enddo                                                                INITTS.96
            enddo                                                                INITTS.97
            if (itsn(inest,nts).ne.1 .and. jtsn(inest,nts).ne.1 ) then           INITTS.98
               print 9020, NTS,TSLAT(NTS), TSLON(NTS),                           INITTS.99
     -                     itsn(inest,nts),jtsn(inest,nts)                       INITTS.100
            else                                                                 INITTS.101
               print 9030, NTS, TSLAT(NTS), TSLON(NTS)                           INITTS.102
            endif                                                                INITTS.103
          endif                                                                  INITTS.104
        enddo                                                                    INITTS.105
      ENDIF                                                                      INITTS.106
      CALL DM_BCAST_INTEGERS ( itsn , MAXNES*ITSDIM )                            INITTS.107
      CALL DM_BCAST_INTEGERS ( jtsn , MAXNES*ITSDIM )                            INITTS.108
                                                                                 INITTS.109
9000  FORMAT(1x,'INITIALIZING T-S FOR DOMAIN ',I2)                               INITTS.110
9010  FORMAT(1x,'T-S No',8x,'(LAT/LONG)',8x,'(I,J)')                             INITTS.111
9020  FORMAT(5x,i2,2x,'(',F8.2,',',F8.2,')',2x,'(',I4,',',I4,')')                INITTS.112
9030  FORMAT(5x,'T-S No ',I2,2x,'(',F8.2,',',F8.2,')',                           INITTS.113
     -  ' IS OUTSIDE THE DOMAIN')                                                INITTS.114
                                                                                 INITTS.115
      return                                                                     INITTS.116
      end                                                                        INITTS.117
#endif                                                                           INITTS.118
