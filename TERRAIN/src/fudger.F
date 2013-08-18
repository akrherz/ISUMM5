      SUBROUTINE FUDGER(N,NPFUG,IFUG,JFUG,LNDFUG,LAND,LNDOUT,IX,JX)              FUDGER.1
C                                                                                FUDGER.2
C   PURPOSE      : ASSIGNS THE FUDGE VALUES FOR THE DOMAIN N.                    FUDGER.3
C   ON ENTRY     :                                                               FUDGER.4
C   N            : THE ID NUMBER OF THE MESOSCALE DOMAIN.                        FUDGER.5
C   NPFUG        : THE NUMBER OF FUDGE POINTS.                                   FUDGER.6
C   IFUG(NPFUG)  : THE I INDICES OF THE FUDGE POINTS.                            FUDGER.7
C   JFUG(NPFUG)  : THE J INDICES OF THE FUDGE POINTS.                            FUDGER.8
C   LNDFUG(NPFUG): THE LAND-USE CATEGORY AT THE FUDGE POINTS.                    FUDGER.9
C   LAND(IX,JX)  : THE LAND-USE FIELD, INTEGER ARRAY.                            FUDGER.10
C   LNDOUT(IX,JX): THE LAND-USE FIELD, REAL ARRAY.                               FUDGER.11
C   IX,JX        : THE DIMENSIONS OF THE MESOSCALE DOMAIN N.                     FUDGER.12
C   ON EXIT      :                                                               FUDGER.13
C   LAND(IX,JX)  : THE LAND-USE FIELD AFTER FUDGE, INTEGER ARRAY.                FUDGER.14
C   LNDOUT(IX,JX): THE LAND-USE FIELD AFTER FUDGE, REAL ARRAY.                   FUDGER.15
C                                                                                FUDGER.16
C   CALLS NO OTHER SUBROUTINES.                                                  FUDGER.17
C   CALLED BY SUBROUTINE CRLND.                                                  FUDGER.18
C                                                                                FUDGER.19
C   COMMENTS :                                                                   FUDGER.20
C THE I AND J INDICES OF THE COARSE DOMAIN ARE IN REFERENCE TO THE               FUDGER.21
C EXPANDED GRID. THE TERRAIN PROGRAM TAKES CARE OF THIS IN SUBROUTINE            FUDGER.22
C SETUP. FROM THE NAMELIST INPUT, THE I AND J PROVIDED BY THE USER               FUDGER.23
C ARE REFERRED TO THE NON-EXPANDED COARSE DOMAIN INDICES.                        FUDGER.24
                                                                                 FUDGER.25
      DIMENSION IFUG(NPFUG), JFUG(NPFUG), LNDFUG(NPFUG), LAND(IX,JX)             FUDGER.26
      REAL LNDOUT(IX,JX)                                                         FUDGER.27
C                                                                                FUDGER.28
C PERFORM FUDGING ON LANDUSE, IF FLAGGED AND IF DATA WAS INITIALIZED             FUDGER.29
C                                                                                FUDGER.30
          DO 10 I=1,NPFUG                                                        FUDGER.31
             LAND(IFUG(I),JFUG(I)) = LNDFUG(I)                                   FUDGER.32
             LNDOUT(IFUG(I),JFUG(I)) = FLOAT(LNDFUG(I))                          FUDGER.33
             PRINT 11, IFUG(I),JFUG(I),LNDFUG(I)                                 FUDGER.34
 11          FORMAT (7X,' LANDUSE (',I2,',',I2,') = ',I3)                        FUDGER.35
 10       CONTINUE                                                               FUDGER.36
C                                                                                FUDGER.37
      RETURN                                                                     FUDGER.38
      END                                                                        FUDGER.39
