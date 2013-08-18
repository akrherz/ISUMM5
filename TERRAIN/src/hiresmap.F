      SUBROUTINE HIRESMAP(IUNIT)                                                 HIRESMAP.1
      DIMENSION XLATMAP(10000),XLONMAP(10000)                                    HIRESMAP.2
C                                                                                HIRESMAP.3
C     ... THE OTHER END OF THIS THING IS IN MRDCLT, WE ONLY WANT                 HIRESMAP.4
C         THE CONTINENT OUTLINE COLOR                                            HIRESMAP.5
C                                                                                HIRESMAP.6
      COMMON /OUTCOL/  COCOLR,           ! COLOR OF CONTINENTS                   HIRESMAP.7
     *                 USCOLR,           ! COLOR OF STATES                       HIRESMAP.8
     *                 CNCOLR            ! COLOR OF COUNTRIES                    HIRESMAP.9
C                                                                                HIRESMAP.10
C     ... THIS COMES FROM MRDDET, WE WANT THE MAP LINE WIDTH                     HIRESMAP.11
C                                                                                HIRESMAP.12
      COMMON /MOTDET/  DTDMP,            ! T => DRAW THE MAP WITH DOTS ?         HIRESMAP.13
     *                 MAPLW,            ! LINE WIDTH FOR MAP OUTLINES           HIRESMAP.14
     *                 DTDSP             ! DASH SPACING FOR MAP OUTLINES         HIRESMAP.15
      INTEGER COCOLR                                                             HIRESMAP.16
C                                                                                HIRESMAP.17
C     ... THIS ROUTINE CALLS ROUTINE TO READ THE HIGH RESOLUTION                 HIRESMAP.18
C         DATA ONCE FOR EACH BLOCK OF VALUES                                     HIRESMAP.19
C     ... THE NCARG ROUTINES MAPIT AND MAPIQ PLOT OUT THE LINES                  HIRESMAP.20
C         AND FLUSH THE BUFFERS AFTER EACH LINE SEGMENT IS DRAWN                 HIRESMAP.21
C     ... THE ROUTINE RETURNS CONTROL WHEN ALL OF THE DATA HAS                   HIRESMAP.22
C         BEEN PROCESSED                                                         HIRESMAP.23
C     ... THIS DATASET WAS RECEIVED FROM PETER NEILLEY RAP/NCAR                  HIRESMAP.24
                                                                                 HIRESMAP.25
#ifdef NCARG                                                                     HIRESMAP.26
      CALL GSPLCI (COCOLR)                                                       HIRESMAP.27
      CALL SETUSV('LW',MAPLW)                                                    HIRESMAP.28
      OPEN(IUNIT,FILE='hipone.ascii',FORM='FORMATTED',STATUS='UNKNOWN')          HIRESMAP.29
      IBLOCK=1                                                                   HIRESMAP.30
10    CONTINUE                                                                   HIRESMAP.31
      CALL READMAP(NUMPOINTS,                                                    HIRESMAP.32
     *                   XLATMIN,XLATMAX,XLONMIN,XLONMAX,                        HIRESMAP.33
     *                   XLATMAP,XLONMAP,                                        HIRESMAP.34
     *                   IUNIT,IEND)                                             HIRESMAP.35
                                                                                 HIRESMAP.36
      IF(IEND.EQ.1) GOTO 1000                                                    HIRESMAP.37
C     PRINT *,'FINISHED BLOCK ',IBLOCK                                           HIRESMAP.38
      CALL MAPIT(XLATMAP(1),XLONMAP(1),0)                                        HIRESMAP.39
      DO 200 IPOINT=2,NUMPOINTS/2                                                HIRESMAP.40
         CALL MAPIT(XLATMAP(IPOINT),XLONMAP(IPOINT),2)                           HIRESMAP.41
200   CONTINUE                                                                   HIRESMAP.42
      CALL MAPIQ                                                                 HIRESMAP.43
      IBLOCK=IBLOCK+1                                                            HIRESMAP.44
      GOTO 10                                                                    HIRESMAP.45
1000  CONTINUE                                                                   HIRESMAP.46
      PRINT *,'MAPDRV - END OF DATA FOR HIPONE.ASCII'                            HIRESMAP.47
      CLOSE(IUNIT)                                                               HIRESMAP.48
      RETURN                                                                     HIRESMAP.49
      END                                                                        HIRESMAP.50
                                                                                 HIRESMAP.51
                                                                                 HIRESMAP.52
      SUBROUTINE READMAP(NUMPOINTS,                                              HIRESMAP.53
     *                   XLATMIN,XLATMAX,XLONMIN,XLONMAX,                        HIRESMAP.54
     *                   XLAT,XLON,                                              HIRESMAP.55
     *                   IUNIT,IEND)                                             HIRESMAP.56
C                                                                                HIRESMAP.57
C     ... THIS PROGRAM READS THE ASCII DATA AND STORES IT IN                     HIRESMAP.58
C         ARRAYS OF LAT AND LON, AND THE NUMBER OF POINTS READ                   HIRESMAP.59
C     ... THE BOX SURROUNDING THE AREA ARE READ IN                               HIRESMAP.60
C                                                                                HIRESMAP.61
      DIMENSION XLAT(NUMPOINTS/2),XLON(NUMPOINTS/2),VAL(8)                       HIRESMAP.62
C                                                                                HIRESMAP.63
      READ(IUNIT,100,END=1000) NUMPOINTS,                                        HIRESMAP.64
     *                         XLATMIN,XLATMAX,XLONMIN,XLONMAX,                  HIRESMAP.65
     *                         XLATSTART,XLONSTART                               HIRESMAP.66
      XLAT(1)=XLATSTART                                                          HIRESMAP.67
      XLON(1)=XLONSTART                                                          HIRESMAP.68
      NUMLINESFULL=(NUMPOINTS-2)/8                                               HIRESMAP.69
C     PRINT *,'NUMBER OF FULL LINES=',NUMLINESFULL                               HIRESMAP.70
      NUMLEFT=NUMPOINTS-2-8*NUMLINESFULL                                         HIRESMAP.71
C     PRINT *,'NUMBER OF LEFT OVERS=',NUMLEFT                                    HIRESMAP.72
      ISTART=1                                                                   HIRESMAP.73
      DO 50 LINE=1,NUMLINESFULL                                                  HIRESMAP.74
         READ(IUNIT,108) VAL                                                     HIRESMAP.75
         DO 40 LOC=1,4                                                           HIRESMAP.76
            XLAT((LINE-1)*4+LOC+ISTART)=VAL(LOC*2-1)                             HIRESMAP.77
            XLON((LINE-1)*4+LOC+ISTART)=VAL(LOC*2  )                             HIRESMAP.78
40       CONTINUE                                                                HIRESMAP.79
50    CONTINUE                                                                   HIRESMAP.80
      IF     (NUMLEFT.EQ.6) THEN                                                 HIRESMAP.81
         READ(IUNIT,106) (VAL(II),II=1,NUMLEFT)                                  HIRESMAP.82
      ELSE IF(NUMLEFT.EQ.4) THEN                                                 HIRESMAP.83
         READ(IUNIT,104) (VAL(II),II=1,NUMLEFT)                                  HIRESMAP.84
      ELSE IF(NUMLEFT.EQ.2) THEN                                                 HIRESMAP.85
         READ(IUNIT,102) (VAL(II),II=1,NUMLEFT)                                  HIRESMAP.86
      ENDIF                                                                      HIRESMAP.87
      DO 60 LOC=1,NUMLEFT/2                                                      HIRESMAP.88
         XLAT(NUMLINESFULL*4+LOC+ISTART)=VAL(LOC*2-1)                            HIRESMAP.89
         XLON(NUMLINESFULL*4+LOC+ISTART)=VAL(LOC*2  )                            HIRESMAP.90
60    CONTINUE                                                                   HIRESMAP.91
                                                                                 HIRESMAP.92
C     PRINT *,'READ ALL DATA FROM THIS BLOCK'                                    HIRESMAP.93
      IEND=0                                                                     HIRESMAP.94
      RETURN                                                                     HIRESMAP.95
                                                                                 HIRESMAP.96
1000  CONTINUE                                                                   HIRESMAP.97
C     PRINT *,'END OF DATA'                                                      HIRESMAP.98
      IEND=1                                                                     HIRESMAP.99
      RETURN                                                                     HIRESMAP.100
#endif                                                                           HIRESMAP.101
100   FORMAT(I4,14X,6F9.3)                                                       HIRESMAP.102
102   FORMAT(2F9.3)                                                              HIRESMAP.103
104   FORMAT(4F9.3)                                                              HIRESMAP.104
106   FORMAT(6F9.3)                                                              HIRESMAP.105
108   FORMAT(8F9.3)                                                              HIRESMAP.106
                                                                                 HIRESMAP.107
      END                                                                        HIRESMAP.108
