      SUBROUTINE SOIL_TG(IX,JX,XLAT,TEMPG,FORCETER,TSPVAL,IP)                    06NOV00.449
C                                                                                SOIL_TG.2
C .. PURPOSE: USING LATITUDE-DEPENDED SOIL TEMPG TO FILL IN                      SOIL_TG.3
C             THE MISSING VALUES OVER THE LAND.                                  SOIL_TG.4
C                                                                                SOIL_TG.5
C             THE COEFFICIENTS C0, C1, AND C2 WERE OBTAINED FROM                 SOIL_TG.6
C             READTG.DECK.                                                       SOIL_TG.7
C                                                                                SOIL_TG.8
C                                                                                SOIL_TG.9
      DIMENSION XLAT(IX,JX), TEMPG(IX,JX), FORCETER(IX,JX)                       SOIL_TG.10
C                                                                                SOIL_TG.11
      IF(IP.EQ.1) PRINT 10                                                       06NOV00.450
10    FORMAT(/2X,'&& CALL SOIL_TG: LATITUDE-DEPENDED SOIL TEMPERATURE')          06NOV00.451
         C0= 0.24206E+03                                                         SOIL_TG.14
         C1= 0.59736E+02                                                         SOIL_TG.15
         C2= 0.19445E+01                                                         SOIL_TG.16
C                                                                                SOIL_TG.17
         N_MISS = 0                                                              SOIL_TG.18
         DO I=1,IX-1                                                             SOIL_TG.19
         DO J=1,JX-1                                                             SOIL_TG.20
            IF (FORCETER(I,J).EQ.9E9 .AND. (TEMPG(I,J).EQ.TSPVAL                 05DEC01.40
     -          .OR. TEMPG(i,j).EQ.-999.9)) THEN                                 05DEC01.41
               N_MISS = N_MISS+1                                                 SOIL_TG.22
               ANG   = 0.5*3.1415926*((89.5-XLAT(I,J))/89.5)                     SOIL_TG.23
               FIT_TG = C0 + C1*SIN(ANG) + C2*COS(ANG)                           SOIL_TG.24
CC               PRINT 11,N_MISS,I,J,XLAT(I,J),TEMPG(I,J),FIT_TG                 SOIL_TG.25
11             FORMAT(3X,'&& N_MISS,I,J,XLAT,TEMPG,FIT_TG:',3I5,3F10.2)          SOIL_TG.26
               TEMPG(I,J) = FIT_TG                                               SOIL_TG.27
            ENDIF                                                                SOIL_TG.28
         END DO                                                                  SOIL_TG.29
         END DO                                                                  SOIL_TG.30
C                                                                                SOIL_TG.31
         RETURN                                                                  SOIL_TG.32
         END                                                                     SOIL_TG.33
