      SUBROUTINE WATERCHECK(IX,JX,NVC,FORCETER, VEGEC, LVEGE, IW, PC,            WATERCHECK.1
     -                      RAD,DS,IP)                                           06NOV00.799
C                                                                                WATERCHECK.3
C  IW -- WATER CATEGORY ID                                                       WATERCHECK.4
C  PC -- THE MAXIMUM PERCENTAGE OF WATER OVER THE LAND.                          WATERCHECK.5
C  FORCETER -- MASK ARRAY: 9E9 --> LAND, OTHERWISE WATER                         WATERCHECK.6
C  RAD -- RADIUS OF INFLUENCE IN SEARCHING THE NEIGHBOR VALUES                   WATERCHECK.7
C  DS  -- GRID DISTANCE                                                          WATERCHECK.8
C                                                                                WATERCHECK.9
      DIMENSION FORCETER(IX,JX),  VEGEC(IX,JX,NVC), LVEGE(IX,JX)                 WATERCHECK.10
C                                                                                WATERCHECK.11
      IF(IP.EQ.1)PRINT *,'CALL WATERCHECK: IW=',IW,' PCNT=',PC,                  06NOV00.800
     -        '  RAD=',RAD,'  DS=',DS                                            WATERCHECK.13
C                                                                                WATERCHECK.14
      MAX_DIM = MAX(IX,JX)                                                       WATERCHECK.15
      RAD_MAX = FLOAT(MAX_DIM)*DS                                                WATERCHECK.16
C                                                                                WATERCHECK.17
      PC1 = 100.-PC                                                              WATERCHECK.18
C                                                                                WATERCHECK.19
         DO 2091 I=1,IX-1                                                        WATERCHECK.20
         DO 2091 J=1,JX-1                                                        WATERCHECK.21
           IF (FORCETER(I,J).EQ.9E9.AND.LVEGE(I,J).EQ.IW) THEN                   WATERCHECK.22
C  .. WATER CATEGORY DOMINATING BUT OVER THE LAND BASED FORCETER:                WATERCHECK.23
             IF(IP.EQ.1)PRINT *,'ADJUST POINT I,J:',I,J,                         06NOV00.801
     -               '  FORCETER=',FORCETER(I,J),'  LVS=',LVEGE(I,J)             06NOV00.802
             VEGEC(I,J,IW) = PC                                                  WATERCHECK.26
             PCNT = 0.0                                                          WATERCHECK.27
             DO IC = 1,NVC                                                       WATERCHECK.28
               IF(IC.NE.IW) THEN                                                 WATERCHECK.29
                 PCNT = PCNT + VEGEC(I,J,IC)                                     WATERCHECK.30
               ENDIF                                                             WATERCHECK.31
             END DO                                                              WATERCHECK.32
             IF (PCNT.GT.0.) THEN                                                WATERCHECK.33
               ADJP = PC1/PCNT                                                   WATERCHECK.34
               DO IC = 1,NVC                                                     WATERCHECK.35
               IF(IC.NE.IW) THEN                                                 WATERCHECK.36
                 VEGEC(I,J,IC) =  VEGEC(I,J,IC)*ADJP                             WATERCHECK.37
               ENDIF                                                             WATERCHECK.38
               END DO                                                            WATERCHECK.39
             ELSE                                                                WATERCHECK.40
               RADXX = RAD                                                       WATERCHECK.41
               NITR  = 1                                                         WATERCHECK.42
2081           CONTINUE                                                          WATERCHECK.43
               DO IC = 1,NVC                                                     WATERCHECK.44
C     --> PERCENTAGE FROM THE NEIGHBOR VALUES:                                   WATERCHECK.45
               IF(IC.NE.IW) THEN                                                 WATERCHECK.46
                 CALL PATCH(IX,JX,VEGEC(1,1,IC),I,J,RAD,DS,0.0)                  WATERCHECK.47
CC                 PRINT *,'  PACTH CAT. IC=',IC,' VEGEC=',VEGEC(I,J,IC)         WATERCHECK.48
               ENDIF                                                             WATERCHECK.49
               END DO                                                            WATERCHECK.50
               PCNT = 0.0                                                        WATERCHECK.51
               DO IC = 1,NVC                                                     WATERCHECK.52
                 IF(IC.NE.IW) THEN                                               WATERCHECK.53
                   PCNT = PCNT + VEGEC(I,J,IC)                                   WATERCHECK.54
                 ENDIF                                                           WATERCHECK.55
               END DO                                                            WATERCHECK.56
C                                                                                WATERCHECK.57
               IF (PCNT.GT.0.) THEN                                              WATERCHECK.58
                 ADJP = PC1/PCNT                                                 WATERCHECK.59
               ELSE                                                              WATERCHECK.60
                 NITR  = 1+NITR                                                  WATERCHECK.61
                 RADXX = RADXX*1.1                                               WATERCHECK.62
                 IF (RADXX.GT.RAD_MAX) THEN                                      WATERCHECK.63
                   PRINT 2082,I,J,NITR,RADXX,RAD_MAX                             WATERCHECK.64
2082               FORMAT('*** I,J:',2I5,'  NITR=',I3,                           WATERCHECK.65
     -                    '  RADXX=',F10.2,'  > RAD_MAX=',F10.2)                 WATERCHECK.66
                   STOP                                                          WATERCHECK.67
                 ENDIF                                                           WATERCHECK.68
                 IF(IP.EQ.1) PRINT 2083,I,J,NITR,RADXX                           06NOV00.803
2083             FORMAT('==> I,J:',2I5,'  NITR=',I3,'  RADXX=',F10.2)            WATERCHECK.70
               ENDIF                                                             WATERCHECK.71
C                                                                                WATERCHECK.72
               DO IC = 1,NVC                                                     WATERCHECK.73
               IF(IC.NE.IW) THEN                                                 WATERCHECK.74
                 VEGEC(I,J,IC) =  VEGEC(I,J,IC)*ADJP                             WATERCHECK.75
               ENDIF                                                             WATERCHECK.76
               END DO                                                            WATERCHECK.77
             ENDIF                                                               WATERCHECK.78
             ZZ = -100.                                                          WATERCHECK.79
             DO IC = 1,NVC                                                       WATERCHECK.80
               IF (IC.NE.IW .AND. VEGEC(I,J,IC).GT.ZZ) THEN                      WATERCHECK.81
                 ZZ = VEGEC(I,J,IC)                                              WATERCHECK.82
                 LVEGE(I,J) = IC                                                 WATERCHECK.83
               ENDIF                                                             WATERCHECK.84
             END DO                                                              WATERCHECK.85
CC             PRINT *,'             ADJUSTED LVS=',LVEGE(I,J)                   WATERCHECK.86
           ELSE IF (IW.EQ.14 .AND. FORCETER(I,J).NE.9E9) THEN                    WATERCHECK.87
C .. FOR SOIL OVER WATER, CATEGORY WATER (IW) = 100%:                            WATERCHECK.88
              DO IC = 1,NVC                                                      WATERCHECK.89
                VEGEC(I,J,IC) =  0.0                                             WATERCHECK.90
              END DO                                                             WATERCHECK.91
              VEGEC(I,J,IW) = 100.                                               WATERCHECK.92
              LVEGE(I,J) = IW                                                    WATERCHECK.93
           ELSE IF (FORCETER(I,J).NE.9E9.AND.LVEGE(I,J).NE.IW) THEN              WATERCHECK.94
C  .. NOT WATER CATEGORY BUT OVER THE WATER BASED ON FORCETER(I,J):              WATERCHECK.95
CC             PRINT *,'ADJUST POINT I,J:',I,J,                                  WATERCHECK.96
CC     -               '  FORCETER=',FORCETER(I,J),'  LVS=',LVEGE(I,J)           WATERCHECK.97
             PCNT = 0.0                                                          WATERCHECK.98
             DO IC = 1,NVC                                                       WATERCHECK.99
               IF (IC.NE.IW) THEN                                                WATERCHECK.100
                 PCNT = PCNT + VEGEC(I,J,IC)                                     WATERCHECK.101
               ENDIF                                                             WATERCHECK.102
             END DO                                                              WATERCHECK.103
             IF (PCNT.GT.0.) THEN                                                WATERCHECK.104
               ADJP = PC/PCNT                                                    WATERCHECK.105
               DO IC = 1,NVC                                                     WATERCHECK.106
                 VEGEC(I,J,IC) =  VEGEC(I,J,IC)*ADJP                             WATERCHECK.107
               END DO                                                            WATERCHECK.108
               VEGEC(I,J,IW) = PC1                                               WATERCHECK.109
             ELSE                                                                WATERCHECK.110
               VEGEC(I,J,IW) =100.0                                              WATERCHECK.111
             ENDIF                                                               WATERCHECK.112
             LVEGE(I,J) = IW                                                     WATERCHECK.113
CC             PRINT *,'             ADJUSTED LVS=',LVEGE(I,J)                   WATERCHECK.114
           ENDIF                                                                 WATERCHECK.115
 2091    CONTINUE                                                                WATERCHECK.116
C                                                                                WATERCHECK.117
         RETURN                                                                  WATERCHECK.118
         END                                                                     WATERCHECK.119
