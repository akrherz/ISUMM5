      SUBROUTINE SOLAR1(XTIME,JULDAY, GMT, IEXEC)                                SOLAR1.1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          SOLAR1.2
C                                                                     C          SOLAR1.3
C                                                                     C          SOLAR1.4
C     THIS SUBROUTINE COMPUTES THE SOLAR DECLINATION ANGLE FROM THE   C          SOLAR1.5
C     JULIAN DATE.                                                    C          SOLAR1.6
C                                                                     C          SOLAR1.7
C                                                                     C          SOLAR1.8
C     XTIME  : FORECAST TIME IN MINUTES.                              C          SOLAR1.9
C                                                                     C          SOLAR1.10
C     IEXEC  : = 1 ; REPRESENTS THIS SUBROUTINE IS CALLED FOR THE     C          SOLAR1.11
C                    FIRST TIME IN THIS FORECAST RUN.                 C          SOLAR1.12
C              > 1 ; REPRESENTS SUBSEQUENT CALLS TO THIS SUBROUTINE.  C          SOLAR1.13
C                                                                     C          SOLAR1.14
C                                                                     C          SOLAR1.15
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          SOLAR1.16
C     CHANGES:                                                        C          SOLAR1.17
C        USE ECCENTRICITY FACTOR IN SOLAR CONSTANT                    C          SOLAR1.18
C                                                                     C          SOLAR1.19
C        ALLOW SPECIFICATION OF SOLSET IN NAMELIST                    C          SOLAR1.20
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          SOLAR1.21
#     include <parame.incl>                                                      SOLAR1.22
#     include <param3.incl>                                                      SOLAR1.23
      REAL JULIAN                                                                SOLAR1.24
C                                                                                SOLAR1.25
C----------------------------------------------------------------------          SOLAR1.26
C                                                                                SOLAR1.27
C-----OBECL : OBLIQUITY = 23.5 DEGREE.                                           SOLAR1.28
C                                                                                SOLAR1.29
      OBECL=23.5*DEGRAD                                                          SOLAR1.30
      SINOB=SIN(OBECL)                                                           SOLAR1.31
C                                                                                SOLAR1.32
C-----CALCULATE LONGITUDE OF THE SUN FROM VERNAL EQUINOX:                        SOLAR1.33
C                                                                                SOLAR1.34
      JULIAN=FLOAT(JULDAY-1)+(XTIME/60.+GMT)/24.                                 SOLAR1.35
      IF(JULIAN.GE.80.)XLONG=DPD*(JULIAN-80.)                                    SOLAR1.36
      IF(JULIAN.LT.80.)XLONG=DPD*(JULIAN+285.)                                   SOLAR1.37
      XLONG=XLONG*DEGRAD                                                         SOLAR1.38
      ARG=SINOB*SIN(XLONG)                                                       SOLAR1.39
      DECLIN=ASIN(ARG)                                                           SOLAR1.40
      DECDEG=DECLIN/DEGRAD                                                       SOLAR1.41
C     SOLAR CONSTANT ECCENTRICITY FACTOR (PALTRIDGE AND PLATT 1976)              SOLAR1.42
      DJUL=JULIAN*360./365.                                                      SOLAR1.43
      RJUL=DJUL*DEGRAD                                                           SOLAR1.44
      ECCFAC=1.000110+0.034221*COS(RJUL)+0.001280*SIN(RJUL)+0.000719*            SOLAR1.45
     +       COS(2*RJUL)+0.000077*SIN(2*RJUL)                                    SOLAR1.46
      SOLCON=1370.*ECCFAC                                                        SOLAR1.47
      IF(SOLSET.GE.0.)THEN                                                       SOLAR1.48
        PRINT *,'NAMELIST VALUE OF SOLAR CONSTANT USED'                          SOLAR1.49
        SOLCON=SOLSET                                                            SOLAR1.50
      ENDIF                                                                      SOLAR1.51
C                                                                                SOLAR1.52
      PRINT10,DECDEG,SOLCON                                                      SOLAR1.53
   10 FORMAT(1X,'*** SOLAR DECLINATION ANGLE = ',F6.2,' DEGREES.',               SOLAR1.54
     -     ' SOLAR CONSTANT = ',F8.2,' W/M**2 ***')                              SOLAR1.55
C                                                                                SOLAR1.56
      IF(IEXEC.EQ.1)RETURN                                                       SOLAR1.57
      DECTIM=DECTIM+1440.                                                        SOLAR1.58
C                                                                                SOLAR1.59
      RETURN                                                                     SOLAR1.60
      END                                                                        SOLAR1.61
                                                                                 SOLAR1.62
