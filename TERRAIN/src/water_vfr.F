      SUBROUTINE WATER_VFR(IX,JX,NVF,FORCETER,  VEGEF, RAD, DS,IP)               06NOV00.797
C                                                                                WATER_VFR.2
C  FORCETER -- MASK ARRAY: 9E9 --> LAND, OTHERWISE WATER                         WATER_VFR.3
C  RAD -- RADIUS OF INFLUENCE IN SEARCHING THE NEIGHBOR VALUES                   WATER_VFR.4
C  DS  -- GRID DISTANCE                                                          WATER_VFR.5
C                                                                                WATER_VFR.6
      DIMENSION FORCETER(IX,JX),  VEGEF(IX,JX,NVF)                               WATER_VFR.7
C                                                                                WATER_VFR.8
      IF(IP.EQ.1) PRINT *,'CALL WATER_VFR:  RAD=',RAD,'  DS=',DS                 06NOV00.798
C                                                                                WATER_VFR.10
      DO N = 1,NVF                                                               WATER_VFR.11
C                                                                                WATER_VFR.12
         DO 2091 I=1,IX-1                                                        WATER_VFR.13
         DO 2091 J=1,JX-1                                                        WATER_VFR.14
           IF (FORCETER(I,J).EQ.9E9.AND.VEGEF(I,J,N).LE.0.0) THEN                WATER_VFR.15
C  .. V.F. = 0% BUT OVER THE LAND BASED FORCETER:                                WATER_VFR.16
CC             PRINT 20,I,J,N,FORCETER(I,J), VEGEF(I,J,N)                        WATER_VFR.17
20           FORMAT('ADJUST POINT (TO LAND) I,J,N:',3I4,                         WATER_VFR.18
     -             '  FORCETER=',E12.5,'  V.F.=',E12.5)                          WATER_VFR.19
C                                                                                WATER_VFR.20
             RR = RAD/DS                                                         WATER_VFR.21
             RR2= RR*RR                                                          WATER_VFR.22
             NIX1 = I - NINT(RR)                                                 WATER_VFR.23
             NIX1 = MAX0(NIX1,   1)                                              WATER_VFR.24
             NIX2 = I + NINT(RR)                                                 WATER_VFR.25
             NIX2 = MIN0(NIX2,IX-1)                                              WATER_VFR.26
             NJX1 = J - NINT(RR)                                                 WATER_VFR.27
             NJX1 = MAX0(NJX1,   1)                                              WATER_VFR.28
             NJX2 = J + NINT(RR)                                                 WATER_VFR.29
             NJX2 = MIN0(NJX2,JX-1)                                              WATER_VFR.30
             PCNT = 0.0                                                          WATER_VFR.31
             WCNT = 0.0                                                          WATER_VFR.32
             NPT  = 0                                                            WATER_VFR.33
             DO II = NIX1,NIX2                                                   WATER_VFR.34
             DO JJ = NJX1,NJX2                                                   WATER_VFR.35
             IF (FORCETER(II,JJ).EQ.9E9.AND.VEGEF(II,JJ,N).GE.0.) THEN           WATER_VFR.36
               RS2 = (II-I)*(II-I)+(JJ-J)*(JJ-J)                                 WATER_VFR.37
               IF (RS2.LT.RR2) THEN                                              WATER_VFR.38
                 WW = (RR2-RS2)/(RR2+RS2)                                        WATER_VFR.39
                 PCNT = PCNT + WW*VEGEF(II,JJ,N)                                 WATER_VFR.40
                 WCNT = WCNT + WW                                                WATER_VFR.41
                 NPT = NPT + 1                                                   WATER_VFR.42
               ENDIF                                                             WATER_VFR.43
             ENDIF                                                               WATER_VFR.44
             END DO                                                              WATER_VFR.45
             END DO                                                              WATER_VFR.46
             IF (NPT.GT.0) VEGEF(I,J,N) = PCNT/WCNT                              WATER_VFR.47
CC             PRINT 10,N,I,J,VEGEF(I,J,N),NPT                                   WATER_VFR.48
 10          FORMAT('==> N=',I2,' POINT(',I3,',',I3,') PATCHED VEGEF=',          WATER_VFR.49
     -               F6.2,'  NPTS=',I6)                                          WATER_VFR.50
           ELSE IF (FORCETER(I,J).NE.9E9.AND.VEGEF(I,J,N).GT.0.0) THEN           WATER_VFR.51
C  .. V.F. NOT 0% BUT OVER THE WATER BASED ON FORCETER(I,J):                     WATER_VFR.52
CC             PRINT 30,I,J,N,FORCETER(I,J), VEGEF(I,J,N)                        WATER_VFR.53
30           FORMAT('ADJUST POINT (TO WATER) I,J,N:',3I4,                        WATER_VFR.54
     -             '  FORCETER=',E12.5,'  V.F.=',E12.5)                          WATER_VFR.55
             VEGEF(I,J,N) = -1.E-10                                              WATER_VFR.56
           ENDIF                                                                 WATER_VFR.57
 2091    CONTINUE                                                                WATER_VFR.58
C                                                                                WATER_VFR.59
         END DO                                                                  WATER_VFR.60
C                                                                                WATER_VFR.61
         RETURN                                                                  WATER_VFR.62
         END                                                                     WATER_VFR.63
                                                                                 WATER_VFR.64
                                                                                 WATER_VFR.65
                                                                                 WATER_VFR.66
                                                                                 WATER_VFR.67
                                                                                 WATER_VFR.68
