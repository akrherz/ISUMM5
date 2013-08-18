      SUBROUTINE RDTER(IUNIT,INEST,IX,JX,HT,SATBRT,TMN,ISLTYP,                   RDTER.1
     -         XLAT,XLONG,MSFD,MSFX,F,ISOIL,VEGFRC,VEGFMO,IVGTYP)                05DEC01.326
      IMPLICIT NONE                                                              RDTER.3
C     THIS ROUTINE READS THE TERRAIN OUTPUT FILE                                 RDTER.4
#     include <parame.incl>                                                      RDTER.5
#     include <hdrv3.incl>                                                       RDTER.6
#     include <various.incl>                                                     RDTER.7
#     include <nestl.incl>                                                       RDTER.8
#     include <landuse.incl>                                                     25JAN00.128
#     include <chardate.incl>                                                    07NOV00.747
C                                                                                RDTER.9
      INTEGER IUNIT,INEST,IX,JX,ISOIL                                            24SEP99.291
      REAL    HT(MIX,MJX),SATBRT(MIX,MJX),TMN(MIX,MJX),F(MIX,MJX)                RDTER.11
      REAL    XLAT(MIX,MJX),XLONG(MIX,MJX),MSFD(MIX,MJX),MSFX(MIX,MJX)           RDTER.12
      INTEGER ISLTYP(MIXL,MJXL),IVGTYP(MIXL,MJXL)                                05DEC01.327
C                                                                                RDTER.14
      INTEGER I,J                                                                RDTER.15
      INTEGER IWARN,NESTICH,NESTJCH,IXCHECK,JXCHECK,ndim,IER                     RDTER.16
      INTEGER lendim1,lendim2                                                    RDTER.17
      REAL    DXCHECK                                                            RDTER.18
      CHARACTER*4 MMINLUN                                                        25JAN00.129
C                                                                                RDTER.20
      CHARACTER*8 NAME8                                                          RDTER.24
      REAL        HSCR1L(MIX*MJX),SCR(MIX,MJX)                                   RDTER.25
      LOGICAL LANDUSEMATCH                                                       25JAN00.130
      LOGICAL DM_IONODE                                                          RDTER.26
      EXTERNAL DM_IONODE                                                         RDTER.27
C                                                                                RDTER.28
      real vegfrc(mixl,mjxl)                                                     07NOV00.748
      real vegfmo(mixl,mjxl,12)                                                  07NOV00.749
      integer imon                                                               07NOV00.750
      character*6 name6                                                          07NOV00.751
      character*19 cdatenew                                                      07NOV00.752
      integer NY, NM, ND, MHOUR, MMIN, MSEC, m1, m2                              07NOV00.753
      real realmon                                                               07NOV00.754
      real frcmon                                                                07NOV00.755
C                                                                                07NOV00.756
      IF ( DM_IONODE() ) THEN                                                    RDTER.29
        OPEN(IUNIT,FILE='TERRAIN_DOMAIN'//CHAR(INEST+ICHAR('0')),                RDTER.30
     &       FORM='UNFORMATTED',STATUS='OLD',ERR=997)                            25JAN00.131
                                                                                 RDTER.32
      PRINT *,'READING V3 TERRAIN INPUT FILE FROM UNIT ',IUNIT                   RDTER.33
      ENDIF                                                                      RDTER.34
                                                                                 RDTER.35
   10 continue                                                                   RDTER.36
      if (dm_ionode()) read (iunit,err=998,end=999) flag                         RDTER.37
#ifdef MPP1                                                                      RDTER.38
      call dm_bcast_integers( flag, 1 )                                          RDTER.39
#endif                                                                           RDTER.40
      if (flag .eq. 0) then                                                      RDTER.41
        if (dm_ionode()) then                                                    RDTER.42
           read(iunit,iostat=ier) jbhi, jbhr, jbhic, jbhrc                       RDTER.43
           if(ier.ne.0) then                                                     RDTER.44
              write(*,'("Error reading big header")')                            RDTER.45
              STOP                                                               23DEC04.244
           endif                                                                 RDTER.47
        endif                                                                    RDTER.48
#ifdef MPP1                                                                      RDTER.49
        call dm_bcast_integers( jbhi, NUMINT*NUMPROGS)                           RDTER.50
        call dm_bcast_reals   ( jbhr, NUMREAL*NUMPROGS)                          25JAN00.132
#endif                                                                           RDTER.52
        IWARN=0                                                                  RDTER.53
        IF(JBHI(1,1).NE.1)THEN                                                   RDTER.54
          PRINT *,'TERRAIN FILE EXPECTED FOR INPUT UNIT',IUNIT                   RDTER.55
          IWARN=IWARN+1                                                          RDTER.56
        ENDIF                                                                    RDTER.57
        NESTICH=JBHI(18,1)                                                       RDTER.58
        NESTJCH=JBHI(19,1)                                                       RDTER.59
        DXCHECK=JBHR(9,1)                                                        RDTER.60
        IF(NINT(DXCHECK).NE.NINT(DX))THEN                                        RDTER.61
          PRINT *,'THE INPUT DATA DOES NOT MATCH EXPECTED GRID SIZE'             RDTER.62
          PRINT *,'DX FROM MODEL =',DX                                           RDTER.63
          PRINT *,'DX FROM HEADER=',DXCHECK                                      RDTER.64
          IWARN=IWARN+1                                                          RDTER.65
        ENDIF                                                                    RDTER.66
          IF((NESTICH.NE.ISOUTH).OR.(NESTJCH.NE.JWEST))THEN                      RDTER.67
            PRINT *,'THE INPUT DATA DOES NOT MATCH EXPECTED LOCATION'            RDTER.68
            PRINT *,'NESTI NESTJ FROM NAMELIST=',ISOUTH,JWEST                    RDTER.69
            PRINT *,'NESTI NESTJ FROM HEADER  =',NESTICH,NESTJCH                 RDTER.70
            IWARN=IWARN+1                                                        RDTER.71
          ENDIF                                                                  RDTER.72
        IXCHECK=JBHI(16,1)                                                       RDTER.73
        JXCHECK=JBHI(17,1)                                                       RDTER.74
        IF((IXCHECK.NE.IX).OR.(JXCHECK.NE.JX))THEN                               RDTER.75
          PRINT *,'THE INPUT DATA DOES NOT MATCH THE EXPECTED SIZE'              RDTER.76
          PRINT *,'NESTIX NESTJX FROM NAMELIST=',IX,JX                           RDTER.77
          PRINT *,'NESTIX NESTJX FROM HEADER  =',IXCHECK,JXCHECK                 RDTER.78
          PRINT *,'ASSUMING THE DOMAINS ARE NOT ASSIGNED CORRECTLY'              RDTER.79
          IWARN=IWARN+1                                                          RDTER.80
        ENDIF                                                                    RDTER.81
C CHECK TO MATCH LANDUSE INPUT                                                   25JAN00.133
        MMINLUN = JBHIC(23,1)(1:4)                                               25JAN00.134
        LANDUSEMATCH = MMINLUN.NE.MMINLU                                         25JAN00.135
#ifdef MPP1                                                                      25JAN00.136
        CALL RSL_MON_BCAST( LANDUSEMATCH, LWORDSIZE )                            25JAN00.137
#endif                                                                           25JAN00.138
        IF(LANDUSEMATCH) THEN                                                    25JAN00.139
          PRINT *,'THE NEST LANDUSE TYPE DOES NOT MATCH THAT ',                  25JAN00.140
     -            'IN COARSE DOMAIN'                                             25JAN00.141
          PRINT *,'THE COARSE DOMAIN LANDUSE TYPE IS ', MMINLU                   25JAN00.142
          PRINT *,'THE NEST DOMAIN LANDUSE TYPE IS   ', MMINLUN                  25JAN00.143
          IWARN=IWARN+1                                                          25JAN00.144
        ENDIF                                                                    25JAN00.145
        IF(IWARN.GT.0)STOP 'IN RDTER'                                            RDTER.82
        go to 10                                                                 RDTER.83
      elseif (flag .eq. 1) then                                                  RDTER.84
        if ( dm_ionode() ) then                                                  RDTER.85
           READ (iunit,iostat=ier) ndim, start_index, end_index, rtime,          RDTER.86
     -                             staggering, ordering, current_date,           RDTER.87
     -                             name, units, description                      RDTER.88
           if(ier.ne.0) then                                                     RDTER.89
              stop 'Error reading subheader in rdter'                            RDTER.90
           endif                                                                 RDTER.91
        endif                                                                    RDTER.92
#ifdef MPP1                                                                      RDTER.93
        call dm_bcast_integers (ndim,          1               )                 RDTER.94
        call dm_bcast_integers (start_index,   LEN_START_INDEX )                 RDTER.95
        call dm_bcast_integers (end_index,     LEN_END_INDEX   )                 RDTER.96
        call dm_bcast_reals    (rtime,         1               )                 RDTER.97
        call dm_bcast_string   (staggering,    LEN_STAGGERING  )                 RDTER.98
        call dm_bcast_string   (ordering,      LEN_ORDERING    )                 RDTER.99
        call dm_bcast_string   (current_date,  LEN_CURRENT_DATE)                 RDTER.100
        call dm_bcast_string   (name,          LEN_NAME        )                 RDTER.101
        call dm_bcast_string   (units,         LEN_UNITS       )                 RDTER.102
        call dm_bcast_string   (description,   LEN_DESCRIPTION )                 RDTER.103
#endif                                                                           RDTER.104
        lendim1 = (end_index(1)-start_index(1)+1)                                RDTER.105
        lendim2 = (end_index(2)-start_index(2)+1)                                RDTER.106
        if(ndim.eq.2 .and. ordering(1:2) .eq. 'YX' )then                         RDTER.107
#ifndef MPP1                                                                     RDTER.108
          read(iunit)(HSCR1L(i),i=1,lendim1*lendim2)                             RDTER.109
#else                                                                            RDTER.110
          DO I = 1, MIX*MJX                                                      RDTER.111
            HSCR1L(I) = 0.                                                       RDTER.112
          ENDDO                                                                  RDTER.113
          call dm_dist_read(iunit,inest,HSCR1L,ndim,                             RDTER.114
     +                      lendim1,lendim2,1,mix,mjx,1)                         RDTER.115
#endif                                                                           RDTER.116
        else                                                                     RDTER.117
          if (dm_ionode()) read(iunit)                                           RDTER.118
        endif                                                                    RDTER.119
        NAME8=name(1:8)                                                          RDTER.120
        NAME6=name(1:6)                                                          07NOV00.757
C       print *,'**',name8,'**'                                                  RDTER.121
        IF(NAME8.EQ.'TERRAIN ')THEN                                              RDTER.122
          CALL EQUATE(HSCR1L,IX,JX,1,HT,MIX,MJX,1)                               RDTER.123
        ELSE IF(NAME8.EQ.'LAND USE')THEN                                         RDTER.124
          CALL EQUATE(HSCR1L,IX,JX,1,SATBRT,MIX,MJX,1)                           RDTER.125
          IF(MIXL.GT.1) THEN                                                     05DEC01.328
             DO I=1,IX                                                           05DEC01.329
             DO J=1,JX                                                           05DEC01.330
                IVGTYP(I,J)=NINT(SATBRT(I,J))                                    05DEC01.331
             ENDDO                                                               05DEC01.332
             ENDDO                                                               05DEC01.333
          END IF                                                                 05DEC01.334
        ELSE IF(NAME6.EQ.'VEGFRC'.AND.MIXL.GT.1)THEN                             07NOV00.758
           READ(NAME8(7:8),'(I2)')IMON                                           07NOV00.759
#ifndef MPP1                                                                     07NOV00.760
           CALL EQUATE(HSCR1L,IX,JX,1,VEGFMO(1,1,IMON),MIXL,MJXL,1)              07NOV00.761
#else                                                                            07NOV00.762
           CALL EQUATE(HSCR1L,IX,JX,1,                                           07NOV00.763
     +                VEGFMO(NOFLIC(1),NOFLIC(1),IMON),MIXL,MJXL,1)              07NOV00.764
#endif                                                                           07NOV00.765
        ELSE IF(NAME8.EQ.'TEMPGRD ')THEN                                         RDTER.130
          IF(ISOIL.EQ.2)CALL EQUATE(HSCR1L,IX,JX,1,TMN,MIX,MJX,1)                24SEP99.292
        ELSE IF(NAME8.EQ.'SOILINDX'.AND.MIXL.GT.1)THEN                           RDTER.132
          CALL EQUATE(HSCR1L,IX,JX,1,SCR,MIX,MJX,1)                              RDTER.133
          DO I=1,IX                                                              RDTER.134
          DO J=1,JX                                                              RDTER.135
             ISLTYP(I,J)=NINT(SCR(I,J))                                          RDTER.136
          ENDDO                                                                  RDTER.137
          ENDDO                                                                  RDTER.138
        ELSE IF(NAME8.EQ.'LATITCRS')THEN                                         RDTER.139
          CALL EQUATE(HSCR1L,IX,JX,1,XLAT,MIX,MJX,1)                             RDTER.140
        ELSE IF(NAME8.EQ.'LONGICRS')THEN                                         RDTER.141
          CALL EQUATE(HSCR1L,IX,JX,1,XLONG,MIX,MJX,1)                            RDTER.142
        ELSE IF(NAME8.EQ.'MAPFACCR')THEN                                         RDTER.143
          CALL EQUATE(HSCR1L,IX,JX,1,MSFX,MIX,MJX,1)                             RDTER.144
        ELSE IF(NAME8.EQ.'MAPFACDT')THEN                                         RDTER.145
          CALL EQUATE(HSCR1L,IX,JX,1,MSFD,MIX,MJX,1)                             RDTER.146
        ELSE IF(NAME8.EQ.'CORIOLIS')THEN                                         RDTER.147
          CALL EQUATE(HSCR1L,IX,JX,1,F,MIX,MJX,1)                                RDTER.148
        ENDIF                                                                    RDTER.149
        go to 10                                                                 RDTER.150
      endif                                                                      RDTER.151
                                                                                 07NOV00.766
      IF(ILDDIM.EQ.1)THEN                                                        05DEC01.335
C       INTERPOLATE VEGFRC IN TIME                                               05DEC01.336
         CALL SETVEGFR(ILX,JLX,XTIME,JULDAY,GMT)                                 05DEC01.337
      ENDIF                                                                      05DEC01.338
      RETURN                                                                     RDTER.152
 999  CONTINUE                                                                   RDTER.153
      PRINT *,'END OF TERRAIN FILE, UNIT ',IUNIT                                 RDTER.154
     *      ,' CHECK IF FILE EXISTS'                                             RDTER.155
      STOP 'STOP 999 IN RDTER'                                                   RDTER.156
  998 PRINT *,'ERROR IN READING HEADER. CHECK TERRAIN FILE, UNIT ',IUNIT         RDTER.157
      STOP 'STOP 998 IN RDTER'                                                   RDTER.158
  997 PRINT *,'ERROR OPENING TERRAIN FILE'                                       25JAN00.146
      STOP 'STOP 997 IN RDTER'                                                   25JAN00.147
      END                                                                        RDTER.159
