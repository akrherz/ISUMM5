      SUBROUTINE CR30SDATA(NK,NLT,NLN,KD,IFORT,DLAT0, DLAT1,                     CR30SDATA.1
     -               DLON0,DLON1,XMINLT,XMAXLT,XMINLN,XMAXLN,                    CR30SDATA.2
     -               N1,N2,N3,N4,ME1,ME2,ID2,INTER1,NREC,NOUT,                   CR30SDATA.3
     -               PRNLAT)                                                     CR30SDATA.4
C                                                                                CR30SDATA.5
CC      INTEGER*2 INTER1(ID2)                                                    CR30SDATA.6
      CHARACTER*2 INTER1(ID2)                                                    CR30SDATA.7
      DIMENSION KD(33),DLAT0(33),DLAT1(33),DLON0(33),DLON1(33)                   CR30SDATA.8
C                                                                                CR30SDATA.9
      CHARACTER*2 ME1(N1), ME2(N3)                                               CR30SDATA.10
C                                                                                CR30SDATA.11
      PRINT *,'IN CR30SDATA: ID2=',ID2                                           CR30SDATA.12
C                                                                                CR30SDATA.13
      RESOL = 1./120.                                                            CR30SDATA.14
      NREC = 0                                                                   CR30SDATA.15
CC      DO K = 1,NK                                                              CR30SDATA.16
CC        REWIND (IFORT+K+18)                                                    CR30SDATA.17
CC      END DO                                                                   CR30SDATA.18
C                                                                                CR30SDATA.19
C**** 1 N HENRY 05/05/99 IF THE DOMAIN STRADDLES LONGITUDE 180                   CR30SDATA.20
C     CHANGE DLON0/DLON1 TO NEGATIVE VALUES FOR TILES WEST OF                    CR30SDATA.21
C     180. OTHERWISE XMINLN/XMAXLN WON'T OVERLAP TILE.                           CR30SDATA.22
C                                                                                CR30SDATA.23
      PRINT *,'NLT=',NLT,' NLN=',NLN                                             CR30SDATA.24
      PRINT*,'XMINLAT: XMAXLAT: ',XMINLT,XMAXLT                                  CR30SDATA.25
      PRINT*,'XMINLON: XMAXLON: ',XMINLN,XMAXLN                                  CR30SDATA.26
      PRINT*,'CHECKING XMINLN/XMAXLN OVERLAP WITH TILES:'                        CR30SDATA.27
      DO J=1,NLT                                                                 CR30SDATA.28
         DO I=1,NLN                                                              CR30SDATA.29
            NIJ = (J-1)*NLN+I                                                    CR30SDATA.30
            IF ((DLON0(NIJ).GT.0).AND.(XMINLN.LT.-180.)) THEN                    CR30SDATA.31
               PRINT*,'CHANGED DLON0/DLON1 FOR TILE #: ',NIJ                     CR30SDATA.32
               PRINT*,'OLD VALUES DLNO0: DLON1: ',                               CR30SDATA.33
     &                 DLON0(NIJ),DLON1(NIJ)                                     CR30SDATA.34
               DLON0(NIJ)=-1*(360-DLON0(NIJ))                                    CR30SDATA.35
               DLON1(NIJ)=-1*(360-DLON1(NIJ))                                    CR30SDATA.36
               PRINT*,'NEW VALUES DLNO0: DLON1: ',                               CR30SDATA.37
     &                 DLON0(NIJ),DLON1(NIJ)                                     CR30SDATA.38
            ENDIF                                                                CR30SDATA.39
         ENDDO                                                                   CR30SDATA.40
      ENDDO                                                                      CR30SDATA.41
C                                                                                CR30SDATA.42
C**** END 1                                                                      CR30SDATA.43
C                                                                                CR30SDATA.44
      NBEG   =  10000                                                            CR30SDATA.45
      NEND   = -10000                                                            CR30SDATA.46
      XLON11 = -100000.0                                                         CR30SDATA.47
C .. LOOP OVER THE TILES FROM NORTH TO SOUTH:                                    CR30SDATA.48
      DO J = 1,NLT                                                               CR30SDATA.49
C                                                                                CR30SDATA.50
      NIJ1 = (J-1)*NLN+1                                                         CR30SDATA.51
      ID = KD(NIJ1)                                                              CR30SDATA.52
      XLAT0 = DLAT1(NIJ1)                                                        CR30SDATA.53
      IF (ID.GT.27) THEN                                                         CR30SDATA.54
         KLON = N3                                                               CR30SDATA.55
         KLAT = N4                                                               CR30SDATA.56
      ELSE                                                                       CR30SDATA.57
         KLON = N1                                                               CR30SDATA.58
         KLAT = N2                                                               CR30SDATA.59
      ENDIF                                                                      CR30SDATA.60
      LENGTH = KLON/2                                                            CR30SDATA.61
#ifdef RECLENBYTE                                                                CR30SDATA.62
      LENGTH = LENGTH*4                                                          CR30SDATA.63
#endif                                                                           CR30SDATA.64
      DO I = 1,NLN                                                               CR30SDATA.65
        NIJ = (J-1)*NLN+I                                                        CR30SDATA.66
        IUNIT = IFORT+NIJ                                                        CR30SDATA.67
        OPEN(IUNIT, ACCESS='DIRECT',RECL=LENGTH,STATUS='UNKNOWN')                CR30SDATA.68
        PRINT 8, J, I, NIJ, IUNIT, LENGTH                                        CR30SDATA.69
 8      FORMAT('OPEN TILE: ROW=',I2,' COLUMN=',I2,' N=',I3,                      CR30SDATA.70
     >                      '  --> FORT.',I2.2,'  RECL=',I6)                     CR30SDATA.71
      END DO                                                                     CR30SDATA.72
C                                                                                CR30SDATA.73
      DO N = 1,KLAT                                                              CR30SDATA.74
        XLAT = XLAT0 - (N-1)*RESOL                                               CR30SDATA.75
        IF (XLAT.LT.XMINLT)  GO TO 100                                           CR30SDATA.76
C                                                                                CR30SDATA.77
C .. LOOP OVER TILES FORM THE 1 TO NLN:                                          CR30SDATA.78
C                                                                                CR30SDATA.79
        DO I = 1,NLN                                                             CR30SDATA.80
C                                                                                CR30SDATA.81
        NIJ = (J-1)*NLN+I                                                        CR30SDATA.82
        ID = KD(NIJ)                                                             CR30SDATA.83
        IUNIT = IFORT+NIJ                                                        CR30SDATA.84
        IF (N.EQ.1) PRINT 10, I,J,NIJ,ID,IUNIT,                                  CR30SDATA.85
     -          DLAT0(NIJ),DLAT1(NIJ),DLON0(NIJ),DLON1(NIJ)                      CR30SDATA.86
 10     FORMAT('I=',I2,' J=',I2,' NIJ=',I2,' ID=',I2,' UNIT=',I2/                CR30SDATA.87
     -       ' DLAT0=',F15.8,' DLAT1=',F15.8,                                    CR30SDATA.88
     -       ' DLON0=',F15.8,' DLON1=',F15.8)                                    CR30SDATA.89
C                                                                                CR30SDATA.90
        IF (ID.GT.27) THEN                                                       CR30SDATA.91
          READ (IUNIT,REC=N) ME2                                                 CR30SDATA.92
        ELSE                                                                     CR30SDATA.93
          READ (IUNIT,REC=N) ME1                                                 CR30SDATA.94
        ENDIF                                                                    CR30SDATA.95
C                                                                                CR30SDATA.96
        IF (XLAT.GT.XMAXLT) GO TO 20                                             CR30SDATA.97
C                                                                                CR30SDATA.98
          IF (I.EQ.1) NREC = NREC+1                                              CR30SDATA.99
          IF (DLON0(NIJ).LE.XMINLN) THEN                                         CR30SDATA.100
            NLL = 0                                                              CR30SDATA.101
          ELSE                                                                   CR30SDATA.102
            NLL = NINT((DLON0(NIJ)-XMINLN)/RESOL)                                CR30SDATA.103
          ENDIF                                                                  CR30SDATA.104
          IF (NREC.EQ.1) NBEG = MIN(NBEG,NLL)                                    CR30SDATA.105
C                                                                                CR30SDATA.106
          DO M = 1,KLON                                                          CR30SDATA.107
            XLON = DLON0(NIJ) + (M-1)*RESOL                                      CR30SDATA.108
            IF (XLON.GE.XMINLN .AND. XLON.LE.XMAXLN) THEN                        CR30SDATA.109
              NLL = NLL+1                                                        CR30SDATA.110
              IF (ID.GT.27) THEN                                                 CR30SDATA.111
                INTER1(NLL) = ME2(M)                                             CR30SDATA.112
CC                INTER1(NLL) = IA(ME2(M),2,-9999)                               CR30SDATA.113
              ELSE                                                               CR30SDATA.114
                INTER1(NLL) = ME1(M)                                             CR30SDATA.115
CC                INTER1(NLL) = IA(ME1(M),2,-9999)                               CR30SDATA.116
              ENDIF                                                              CR30SDATA.117
CC              IF (INTER1(NLL).EQ.-9999) INTER1(NLL) = 0                        CR30SDATA.118
              IF (NREC.EQ.1 .AND. NLL.EQ.1) THEN                                 CR30SDATA.119
                XLAT00 = XLAT                                                    CR30SDATA.120
                XLON00 = XLON                                                    CR30SDATA.121
              ENDIF                                                              CR30SDATA.122
C                                                                                CR30SDATA.123
CCC              IF (NREC.EQ.1) PRINT 12,I,M,NLL,XLAT,XLON,ME1(M)                CR30SDATA.124
 12           FORMAT('I=',I2,'  M=',I5,' NLL=',I5,' LAT=',F9.4,                  CR30SDATA.125
     -             ' LON=',F9.4,' HT=',I5)                                       CR30SDATA.126
            ELSE IF (XLON.GT.XMAXLN) THEN                                        CR30SDATA.127
              GO TO 15                                                           CR30SDATA.128
            ENDIF                                                                CR30SDATA.129
          END DO                                                                 CR30SDATA.130
C                                                                                CR30SDATA.131
 15     IF (NREC.EQ.1) THEN                                                      CR30SDATA.132
C                                                                                CR30SDATA.133
C    NOTE THAT IN CERTAIN SITUATION,A TILE WITH A BIG INDEX(I)                   CR30SDATA.134
C              MAY HAVE A SMALLER DLON0, DLON1 THAN THOSE FROM                   CR30SDATA.135
C              A TILE WITH A SMALL INDEX(I).                                     CR30SDATA.136
C                                                                                CR30SDATA.137
          NEND = MAX(NEND,NLL)                                                   CR30SDATA.138
          XXLON  = XLON - RESOL                                                  CR30SDATA.139
          XLON11 = MAX(XLON11,XXLON)                                             CR30SDATA.140
        ENDIF                                                                    CR30SDATA.141
C                                                                                CR30SDATA.142
 20     CONTINUE                                                                 CR30SDATA.143
C                                                                                CR30SDATA.144
        END DO                                                                   CR30SDATA.145
        IF (XLAT.LT.XMINLT .OR. XLAT.GT.XMAXLT) GO TO 30                         CR30SDATA.146
CCC        PRINT 14,N,NREC,XLAT,NBEG+1,NEND                                      CR30SDATA.147
 14     FORMAT('NLAT=',I5,' NREC=',I5,                                           CR30SDATA.148
     -         ' LAT=',F9.4,' BEG=',I5,' END=',I5)                               CR30SDATA.149
        WRITE(NOUT,REC=NREC) INTER1                                              CR30SDATA.150
C                                                                                CR30SDATA.151
          IF (ABS(XLAT-PRNLAT).LT.1.E-5) THEN                                    CR30SDATA.152
            DO K = 1,NEND                                                        CR30SDATA.153
              XLON = XLON00 + (K-1)/120.                                         CR30SDATA.154
              PRINT 16,NREC,XLAT,K,XLON,INTER1(K)                                CR30SDATA.155
 16           FORMAT('NREC=',I5,' LAT=',F15.8,'  K=',I5,                         CR30SDATA.156
     -               ' LON=',F15.8,' TER=',I5)                                   CR30SDATA.157
            END DO                                                               CR30SDATA.158
          ENDIF                                                                  CR30SDATA.159
C                                                                                CR30SDATA.160
 30   CONTINUE                                                                   CR30SDATA.161
C                                                                                CR30SDATA.162
      END DO                                                                     CR30SDATA.163
      END DO                                                                     CR30SDATA.164
C                                                                                CR30SDATA.165
 100  XLAT11 = XLAT + RESOL                                                      CR30SDATA.166
C                                                                                CR30SDATA.167
      PRINT 101,XLAT00,XLAT11,XLON00,XLON11,NREC,NEND                            CR30SDATA.168
      IF (NEND.NE.ID2) NEND = ID2                                                CR30SDATA.169
      WRITE(NOUT-1) XLAT00,XLAT11,XLON00,XLON11,NREC,NEND                        CR30SDATA.170
 101  FORMAT(//'XLAT00=',F15.8,' XLAT11=',F15.8,' XLON00=',F15.8,                CR30SDATA.171
     -        ' XLON11=',F15.8,' NREC=',I5,' NLONG=',I5)                         CR30SDATA.172
C                                                                                CR30SDATA.173
      RETURN                                                                     CR30SDATA.174
      END                                                                        CR30SDATA.175
                                                                                 CR30SDATA.176
