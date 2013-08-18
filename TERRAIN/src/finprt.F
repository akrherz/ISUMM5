      SUBROUTINE FINPRT(HT,LAND,XLAND,VEGEC,SOILC,VEGEF,TEMPG,                   FINPRT.1
     -                  WLMSK,LVEGE,LSOIL,IMAX,JMAX,NM,DUM2D)                    FINPRT.2
C                                                                                FINPRT.3
C  PURPOSE   : READS THE TERRAIN HEIGHT AND LAND-USE FOR THE DOMAIN NM,          FINPRT.4
C              AND PRINTS AND PLOTS THESE FIELDS.                                FINPRT.5
C  ON ENTRY  :                                                                   FINPRT.6
C  IMAX,JMAX : THE DIMENSIONS OF THE DOMAIN NM (NON-EXPANDED GRID                FINPRT.7
C              DIMENSION EVEN IF IEXP = .TRUE.).                                 FINPRT.8
C  NM        : THE ID NUMBER OF THE MESOSCALE DOMAIN.                            FINPRT.9
C                                                                                FINPRT.10
C  ON EXIT   :                                                                   FINPRT.11
C  THE TERRAIN HEIGHT AND LAND-USE FOR THE DOMAIN NM ARE PRINTED ON THE          FINPRT.12
C  MESOSCALE MESH.                                                               FINPRT.13
C  THE PLOTS FOR THE DOMAIN NM (SEE SUBROUTINE PLTTER).                          FINPRT.14
C                                                                                FINPRT.15
C  CALLS THE SUBROUTINE OUTPT AND PLTTER.                                        FINPRT.16
C  CALLED BY THE MAIN PROGRAM TERRAIN.                                           FINPRT.17
C                                                                                FINPRT.18
C  COMMENTS  :                                                                   FINPRT.19
C  THE COMMON BLOCK /OPTION/ IS USED IN THIS SUBROUTINE.                         FINPRT.20
C  FOR THE COARSE DOMAIN (NM=1), THE COMMON HEADER INFORMATION IS                FINPRT.21
C  PRINTED OUT. AS FOR OTHER DOMAINS, ONLY THE DOMAIN HEADER                     FINPRT.22
C  INFORMATION IS PRINTED OUT.                                                   FINPRT.23
C                                                                                FINPRT.24
# include <option.incl>                                                          FINPRT.25
# include <parame.incl>                                                          FINPRT.26
# include <io.incl>                                                              06NOV00.71
# include <paramesv.incl>                                                        FINPRT.27
# include <headerv3.incl>                                                        FINPRT.28
                                                                                 FINPRT.29
      DIMENSION HT(IMAX,JMAX),LAND(IMAX,JMAX),XLAND(IMAX,JMAX)                   FINPRT.30
      DIMENSION VEGEC(IMAX,JMAX,NVC), SOILC(IMAX,JMAX,NSC),                      FINPRT.31
     >          VEGEF(IMAX,JMAX,12), TEMPG(IMAX,JMAX)   ,                        FINPRT.32
     >          WLMSK(IMAX,JMAX),    LVEGE(IMAX,JMAX)   ,                        FINPRT.33
     >          LSOIL(IMAX,JMAX)                                                 FINPRT.34
      DIMENSION WORK(NSIZE), DUM2D(IMAX,JMAX)                                    FINPRT.35
      INTEGER   NDIM                                                             FINPRT.36
C                                                                                FINPRT.37
      IF (IFTER) THEN                                                            FINPRT.38
C                                                                                FINPRT.39
C  READ IN THE TERRESTIAL FIELDS                                                 FINPRT.40
C                                                                                FINPRT.41
      LUNIT=IVOL_mdl+NM                                                          06NOV00.72
      REWIND LUNIT                                                               FINPRT.43
                                                                                 FINPRT.44
10    CONTINUE                                                                   FINPRT.45
      READ(LUNIT) FLAG                                                           FINPRT.46
                                                                                 FINPRT.47
      IF (FLAG.EQ.0) THEN                                                        FINPRT.48
                                                                                 FINPRT.49
      READ(LUNIT) BHI,BHR,BHIC,BHRC                                              FINPRT.50
      N     = BHI(13,1)                                                          FINPRT.51
      IF(N.NE.NM) STOP13                                                         FINPRT.52
                                                                                 FINPRT.53
      IF(NM.EQ.1) THEN                                                           FINPRT.54
C                                                                                FINPRT.55
C  PRINT OUT RECORD HEADER INFORMATION                                           FINPRT.56
C                                                                                FINPRT.57
      PRINT *,' '                                                                FINPRT.58
      PRINT *,' COMMON HEADER INFORMATION'                                       FINPRT.59
      DO 40 I=1,12                                                               FINPRT.60
      IF(BHI(I,1).EQ.-999) GO TO 45                                              FINPRT.61
         WRITE(6,100) I,BHI(I,1),BHIC(I,1)                                       FINPRT.62
   40 CONTINUE                                                                   FINPRT.63
   45 CONTINUE                                                                   FINPRT.64
  100 FORMAT('BHI(',I3,',1) = ',I10,5X,A50)                                      FINPRT.65
      DO 50 I=1,8                                                                FINPRT.66
      IF(BHR(I,1).EQ.-999.) GO TO 55                                             FINPRT.67
         WRITE(6,110) I,BHR(I,1),BHRC(I,1)                                       FINPRT.68
   50 CONTINUE                                                                   FINPRT.69
   55 CONTINUE                                                                   FINPRT.70
  110 FORMAT('BHR(',I3,',1) = ',F11.3,5X,A50)                                    FINPRT.71
      ENDIF                                                                      FINPRT.72
                                                                                 FINPRT.73
      IF(BHI(13,1).NE.-999) THEN                                                 FINPRT.74
         PRINT *,' '                                                             FINPRT.75
         PRINT *,' TERRAIN HEADER INFORMATION'                                   FINPRT.76
      ENDIF                                                                      FINPRT.77
                                                                                 FINPRT.78
      DO 60 I=13,NUMINT                                                          FINPRT.79
         IF(BHI(I,1).EQ.-999) GO TO 65                                           FINPRT.80
         WRITE(6,100) I,BHI(I,1),BHIC(I,1)                                       FINPRT.81
   60 CONTINUE                                                                   FINPRT.82
   65 CONTINUE                                                                   FINPRT.83
      DO 70 I=9,NUMREAL                                                          FINPRT.84
         IF(BHR(I,1).EQ.-999.) GO TO 75                                          FINPRT.85
         WRITE(6,110) I,BHR(I,1),BHRC(I,1)                                       FINPRT.86
   70 CONTINUE                                                                   FINPRT.87
   75 CONTINUE                                                                   FINPRT.88
      GO TO 10                                                                   FINPRT.89
                                                                                 FINPRT.90
      ELSE IF (FLAG.EQ.1) THEN                                                   FINPRT.91
                                                                                 FINPRT.92
        READ(LUNIT) NDIM, START_INDEX, END_INDEX, TIME, STAGGERING,              FINPRT.93
     -              ORDERING, CURRENT_DATE, NAME, UNITS, DESCRIPTION             FINPRT.94
        READ(LUNIT) DUM2D                                                        FINPRT.95
                                                                                 FINPRT.96
        IF (NAME(1:8).EQ.'TERRAIN ')                                             FINPRT.97
     -                  CALL EQUATE(DUM2D,   HT,IMAX,JMAX, 1,NAME)               FINPRT.98
        IF (NAME(1:8).EQ.'LAND USE')                                             FINPRT.99
     -                  CALL EQUATE(DUM2D,XLAND,IMAX,JMAX, 1,NAME)               FINPRT.100
C                                                                                FINPRT.101
        IF(NAME(1:6).EQ.'VEGCAT' .OR. NAME(1:6).EQ.'SOICAT'                      FINPRT.102
     -     .OR. NAME(1:6).EQ.'VEGFRC') READ(NAME,'(6X,I2)') IC                   FINPRT.103
C                                                                                FINPRT.104
        IF (NAME(1:8).GE.'VEGCAT00'.AND. NAME(1:8).LE.'VEGCAT99')                FINPRT.105
     -        CALL EQUATE(DUM2D,VEGEC(1,1,IC),IMAX,JMAX, 1,NAME)                 FINPRT.106
        IF (NAME(1:8).GE.'SOICAT00'.AND. NAME(1:8).LE.'SOICAT99')                FINPRT.107
     -        CALL EQUATE(DUM2D,SOILC(1,1,IC),IMAX,JMAX, 1,NAME)                 FINPRT.108
        IF (NAME(1:8).GE.'VEGFRC01'.AND. NAME(1:8).LE.'VEGFRC12')                FINPRT.109
     -        CALL EQUATE(DUM2D,VEGEF(1,1,IC),IMAX,JMAX, 1,NAME)                 FINPRT.110
        IF (NAME(1:8).EQ.'TEMPGRD ')                                             FINPRT.111
     -                  CALL EQUATE(DUM2D,TEMPG,IMAX,JMAX, 1,NAME)               FINPRT.112
        IF (NAME(1:8).EQ.'LANDMASK')                                             FINPRT.113
     -                  CALL EQUATE(DUM2D,WLMSK,IMAX,JMAX, 1,NAME)               FINPRT.114
C       IF (NAME(1:8).EQ.'SOILINDX') PRINT *,'==> ',NAME,' READ IN.'             FINPRT.117
C                                                                                FINPRT.118
          DO 90 I=1,IMAX                                                         FINPRT.119
          DO 90 J=1,JMAX                                                         FINPRT.120
          IF (NAME(1:8).EQ.'LAND USE') THEN                                      FINPRT.121
             LAND(I,J)=NINT(DUM2D(I,J))                                          FINPRT.122
          ELSE IF (NAME(1:8).EQ.'VEGEINDX') THEN                                 FINPRT.123
             LVEGE(I,J)=NINT(DUM2D(I,J))                                         FINPRT.124
             PRINT *, 'I SHOULD NOT BE HERE GETTING VEGEINDX FIELD'              FINPRT.125
          ELSE IF (NAME(1:8).EQ.'SOILINDX') THEN                                 FINPRT.126
             LSOIL(I,J)=NINT(DUM2D(I,J))                                         FINPRT.127
          ENDIF                                                                  FINPRT.128
   90     CONTINUE                                                               FINPRT.129
C                                                                                FINPRT.130
        IF(IPRTHT)                                                               06NOV00.73
     -    CALL OUTPT(DUM2D,IMAX,1,IMAX-1,1,JMAX,1,JMAX-1,1,6,NAME)               FINPRT.132
C                                                                                FINPRT.133
        GO TO 10                                                                 FINPRT.134
                                                                                 FINPRT.135
      ELSE IF (FLAG.EQ.2) THEN                                                   FINPRT.136
        IF(IPRINT.EQ.1)PRINT *, 'REACHING THE END OF FILE'                       06NOV00.74
      END IF                                                                     FINPRT.138
C                                                                                FINPRT.139
      ENDIF                                                                      FINPRT.140
C END OF IFTER CHECK                                                             FINPRT.141
C                                                                                FINPRT.142
#ifdef NCARG                                                                     FINPRT.143
C                                                                                FINPRT.144
C  PLOT GMETA                                                                    FINPRT.145
C                                                                                FINPRT.146
C  IF COARSE MESH IS EXPANDED, THEN IMX AND JMX SHOULD BE NON-EXPANDED GRID      FINPRT.147
C  DIMENSION                                                                     FINPRT.148
C                                                                                FINPRT.149
      IF(NM.EQ.1 .AND. BHI(8,1).EQ.1) THEN                                       FINPRT.150
         IMX = BHI(5,1)                                                          FINPRT.151
         JMX = BHI(6,1)                                                          FINPRT.152
      ELSE                                                                       FINPRT.153
         IMX = IMAX                                                              FINPRT.154
         JMX = JMAX                                                              FINPRT.155
      ENDIF                                                                      FINPRT.156
C     PRINT *,'IMX,JMX,IMAX,JMAX,NM= ',IMX,JMX,IMAX,JMAX,NM                      06NOV00.75
                                                                                 FINPRT.158
      CALL PLTTER(HT,LAND,WORK,IMAX,JMAX,IMX,JMX,NM,                             FINPRT.159
     -         VEGEC,SOILC,VEGEF,TEMPG,WLMSK,LVEGE,LSOIL)                        FINPRT.160
#endif                                                                           FINPRT.161
      RETURN                                                                     FINPRT.162
      END                                                                        FINPRT.163
