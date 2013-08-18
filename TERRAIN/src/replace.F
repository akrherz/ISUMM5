      SUBROUTINE REPLACE(CT,CL,VC,SC,VFC,TGC,WLC,XLVC,XLSC,                      REPLACE.1
     -                 IYY ,JXX ,IINC,                                           REPLACE.2
     -                   FT,FL,VF,SF,VFF,TGF,WLF,XLVF,XLSF,                      REPLACE.3
     -                 IYYF,JXXF,IINF,PFLAG)                                     REPLACE.4
C                                                                                REPLACE.5
C   PURPOSE  :  REPLACES THE MOTHER DOMAIN TERRAIN HEIGHTS AT THE                REPLACE.6
C               OVERLAPPING GRID POINTS WITH ITS NEST VALUES.                    REPLACE.7
C   ON ENTRY :  IYY,JXX: THE DIMENSIONS OF THE MOTHER DOMAIN.                    REPLACE.8
C                  IINC: THE MOTHER DOMAIN ID.                                   REPLACE.9
C             IYYF,JXXF: THE DIMENSIONS OF THE NEST DOMAIN.                      REPLACE.10
C                  IINF: THE NEST DOMAIN ID.                                     REPLACE.11
C                 PFLAG: THE PRINT SWITCH. IF .TRUE., PRINT THE                  REPLACE.12
C                        TERRAIN HEIGHT FIELDS AFTER THE REPLACEMENT.            REPLACE.13
C                        IF .FALSE., DO NOT PRINT.                               REPLACE.14
C   ON EXIT :                                                                    REPLACE.15
C               THE REPLACED TERRAIN HEIGHT AND LAND-USE FOR MOTHER              REPLACE.16
C               DOMAIN ARE WRITTEN OUT TO DISK WITH THE SAME UNIT                REPLACE.17
C               NUMBER.                                                          REPLACE.18
C   CALLS :     SUBROUTINE REPLACE CALLS THE SUBROUTINE OUTPT.                   REPLACE.19
C   CALLED BY : THE MAIN PROGRAM TERRAIN.                                        REPLACE.20
C   COMMENTS :                                                                   REPLACE.21
C  THE COMMON BLOCK /OPTION/ IS USED IN THIS SUBROUTINE. THE LOCATIONS           REPLACE.22
C  OF THE SOUTH, NORTH, WEST AND EAST NEST DOMAIN BOUNDARIES ARE                 REPLACE.23
C  DETERMINED BASED ON THE INFORMATION FROM THE HEADER RECORDS OF BOTH           REPLACE.24
C  MOTHER AND NEST DOMAINS AFTER READING IN THE DATA FILES FROM THE              REPLACE.25
C  DISK. THEN THE VALUES AT THE INTERIOR OVERLAPPING GRID POINTS OF THE          REPLACE.26
C  NEST DOMAIN ARE ASSIGNED TO THE CORRESPONDING GRID POINTS OF THE              REPLACE.27
C  MOTHER DOMAIN. THE LAND-USE VALUES OF THE MOTHER DOMAIN AT THOSE              REPLACE.28
C  POINTS WHOSE HEIGHTS ARE LESS THAN ZERO AND GREATER THAN -1 ARE SET           REPLACE.29
C  TO BE 7 (WATER). FINALLY THE TERRAIN HEIGHT AND LAND-USE FOR THE              REPLACE.30
C  MOTHER DOMAIN ARE WRITTEN BACK TO THE DISK.                                   REPLACE.31
C                                                                                REPLACE.32
C  THIS FEEDBACK PROCEDURE IS NEEDED ONLY FOR THE TWO-WAY NESTED MODEL           REPLACE.33
C  APPLICATIONS (SEE SECTION 3.5.2).                                             REPLACE.34
C                                                                                REPLACE.35
# include <parame.incl>                                                          REPLACE.36
# include <paramed.incl>                                                         06NOV00.260
# include <io.incl>                                                              06NOV00.261
# include <option.incl>                                                          REPLACE.37
# include <paramesv.incl>                                                        REPLACE.38
# include <vs_cmn1.incl>                                                         REPLACE.39
# include <maps.incl>                                                            REPLACE.40
# include <nestdmn.incl>                                                         REPLACE.41
                                                                                 REPLACE.42
      DIMENSION CT(IYY,JXX),CL(IYY,JXX),FT(IYYF,JXXF),FL(IYYF,JXXF)              REPLACE.43
      DIMENSION VC(IYY ,JXX ,NVC), SC(IYY ,JXX ,NSC), VFC(IYY ,JXX ,12),         REPLACE.44
     -         TGC(IYY ,JXX ), WLC(IYY ,JXX ), XLVC(IYY ,JXX ),                  REPLACE.45
     -        XLSC(IYY ,JXX ),                                                   REPLACE.46
     -          VF(IYYF,JXXF,NVC), SF(IYYF,JXXF,NSC), VFF(IYYF,JXXF,12),         REPLACE.47
     -         TGF(IYYF,JXXF), WLF(IYYF,JXXF), XLVF(IYYF,JXXF),                  REPLACE.48
     -        XLSF(IYYF,JXXF)                                                    REPLACE.49
      CHARACTER*10 NAME                                                          REPLACE.50
      LOGICAL   PFLAG                                                            REPLACE.51
C                                                                                REPLACE.52
      NESTID = IINC - IVOL_mdl                                                   06NOV00.262
      PRINT *,'==> CALL REPLACE'                                                 06NOV00.263
      PRINT 910, NESTID,IINF-IVOL_mdl                                            06NOV00.264
910   FORMAT(1X,'REPLACE THE OVERLAPPING GRID VALUES IN MOTHER DOMAIN ',         06NOV00.265
     -       I2,' WITH NEST DOMAIN ',I2,' VALUES')                               06NOV00.266
      NVW = IVW(NTYPE(NESTID))                                                   23DEC04.13
      IF(IPRINT.EQ.1) THEN                                                       REPLACE.56
        PRINT *,'IYY,JXX:',IYY,JXX,'   IYYF,JXXF:',IYYF,JXXF                     REPLACE.57
        PRINT *,'IINC=',IINC,'   IINF=',IINF                                     REPLACE.58
      ENDIF                                                                      REPLACE.59
      KSIGT = 6                                                                  REPLACE.60
                                                                                 REPLACE.61
      REWIND IINC                                                                REPLACE.62
      REWIND IINF                                                                REPLACE.63
C                                                                                REPLACE.64
C   READ IN COARSE AND FINE MESH TERRAIN AND LANDUSE:                            REPLACE.65
C                                                                                REPLACE.66
C   COARSE:                                                                      REPLACE.67
C     READ(IINC) MIF,MRF,MIFC,MRFC                                               REPLACE.68
      NC     = NESTID                                                            REPLACE.69
                                                                                 REPLACE.70
      READ(IINC) CT                                                              REPLACE.71
      READ(IINC) CL                                                              REPLACE.72
                                                                                 REPLACE.73
      IF (VEGESOIL) THEN                                                         REPLACE.74
        IF (.NOT.VEGONLY) THEN                                                   REPLACE.75
          DO IC = 1,NVC                                                          REPLACE.76
            READ(IINC) TGC                                                       REPLACE.77
            CALL EQUATE(TGC,VC (1,1,IC),IYY ,JXX , 1,'VEGECATC')                 REPLACE.78
          END DO                                                                 REPLACE.79
          DO IC = 1,NSC                                                          REPLACE.80
            READ(IINC) TGC                                                       REPLACE.81
            CALL EQUATE(TGC,SC (1,1,IC),IYY ,JXX , 1,'SOILCATC')                 REPLACE.82
          END DO                                                                 REPLACE.83
          DO IC = 1,12                                                           REPLACE.84
            READ(IINC) TGC                                                       REPLACE.85
            CALL EQUATE(TGC,VFC(1,1,IC),IYY ,JXX , 1,'VEGEFRCC')                 REPLACE.86
          END DO                                                                 REPLACE.87
          READ(IINC) TGC                                                         REPLACE.88
          READ(IINC) WLC                                                         REPLACE.89
C         READ(IINC) XLVC                                                        REPLACE.90
          READ(IINC) XLSC                                                        REPLACE.91
        ENDIF                                                                    REPLACE.92
      ENDIF                                                                      REPLACE.93
                                                                                 REPLACE.94
      REWIND IINC                                                                REPLACE.95
C     WRITE(IINC) MIF,MRF,MIFC,MRFC                                              REPLACE.96
C   FINE:                                                                        REPLACE.97
C     READ(IINF) MIF,MRF,MIFC,MRFC                                               REPLACE.98
      NF     = IINF - IVOL_mdl                                                   06NOV00.267
      ISOUTH = NESTI(NF)                                                         REPLACE.100
      JWEST  = NESTJ(NF)                                                         REPLACE.101
      IRATIOF= IRATIO(NF)                                                        REPLACE.102
      IXF    = NESTIX(NF)                                                        REPLACE.103
      JXF    = NESTJX(NF)                                                        REPLACE.104
                                                                                 REPLACE.105
      READ(IINF) FT                                                              REPLACE.106
      READ(IINF) FL                                                              REPLACE.107
                                                                                 REPLACE.108
      IF (VEGESOIL) THEN                                                         REPLACE.109
        IF (.NOT.VEGONLY) THEN                                                   REPLACE.110
          DO IC = 1,NVC                                                          REPLACE.111
            READ(IINF) TGF                                                       REPLACE.112
            CALL EQUATE(TGF,VF (1,1,IC),IYYF,JXXF, 1,'VEGECATF')                 REPLACE.113
          END DO                                                                 REPLACE.114
          DO IC = 1,NSC                                                          REPLACE.115
            READ(IINF) TGF                                                       REPLACE.116
            CALL EQUATE(TGF,SF (1,1,IC),IYYF,JXXF, 1,'SOILCATF')                 REPLACE.117
          END DO                                                                 REPLACE.118
          DO IC = 1,12                                                           REPLACE.119
            READ(IINF) TGF                                                       REPLACE.120
            CALL EQUATE(TGF,VFF(1,1,IC),IYYF,JXXF, 1,'VEGEFRCF')                 REPLACE.121
          END DO                                                                 REPLACE.122
          READ(IINF) TGF                                                         REPLACE.123
          READ(IINF) WLF                                                         REPLACE.124
C         READ(IINF) XLVF                                                        REPLACE.125
          READ(IINF) XLSF                                                        REPLACE.126
        ENDIF                                                                    REPLACE.127
      ENDIF                                                                      REPLACE.128
C                                                                                REPLACE.129
      IF (NC.EQ.1) THEN                                                          REPLACE.130
        ISOUTH = ISOUTH + IOFFST                                                 REPLACE.131
        JWEST  = JWEST  + JOFFST                                                 REPLACE.132
      ENDIF                                                                      REPLACE.133
C                                                                                REPLACE.134
      INORTH = ISOUTH + (IXF-1)/IRATIOF                                          REPLACE.135
      JEAST  = JWEST  + (JXF-1)/IRATIOF                                          REPLACE.136
C                                                                                REPLACE.137
C NEST INPUT AND OUTPUT INTERFACES CANNOT OVERLAP                                REPLACE.138
C REPLACE AREA SHOULD BE ISOUTH+1,INORTH-2 AND JWEST+1,JEAST-2                   REPLACE.139
C WHERE (ISOUTH,INORTH,JWEST,JEAST) DEFINE THE FOUR CORNERS OF                   REPLACE.140
C THE MOTHER MESH WITH RESPECT TO DOT POINTS                                     REPLACE.141
C                                                                                REPLACE.142
      DO 23 I=ISOUTH+1,INORTH-2                                                  REPLACE.143
      DO 24 J=JWEST+1,JEAST-2                                                    REPLACE.144
         CT(I,J)=FT(((I-ISOUTH)*3)+2,((J-JWEST)*3)+2)                            REPLACE.145
         CL(I,J)=FL(((I-ISOUTH)*3)+2,((J-JWEST)*3)+2)
         IF (VEGESOIL) THEN                                                      REPLACE.146
           IF (.NOT.VEGONLY) THEN                                                REPLACE.147
           DO IC = 1,NVC                                                         REPLACE.148
             VC (I,J,IC)=VF (((I-ISOUTH)*3)+2,((J-JWEST)*3)+2,IC)                REPLACE.149
           END DO                                                                REPLACE.150
           DO IC = 1,NSC                                                         REPLACE.151
             SC (I,J,IC)=SF (((I-ISOUTH)*3)+2,((J-JWEST)*3)+2,IC)                REPLACE.152
           END DO                                                                REPLACE.153
           DO IC = 1,12                                                          REPLACE.154
             VFC(I,J,IC)=VFF(((I-ISOUTH)*3)+2,((J-JWEST)*3)+2,IC)                REPLACE.155
           END DO                                                                REPLACE.156
           TGC(I,J)=TGF(((I-ISOUTH)*3)+2,((J-JWEST)*3)+2)                        REPLACE.157
C          XLVC(I,J)=XLVF(((I-ISOUTH)*3)+2,((J-JWEST)*3)+2)                      REPLACE.158
           XLSC(I,J)=XLSF(((I-ISOUTH)*3)+2,((J-JWEST)*3)+2)                      REPLACE.159
           WLC(I,J)=WLF(((I-ISOUTH)*3)+2,((J-JWEST)*3)+2)
           ENDIF                                                                 REPLACE.160
         ENDIF                                                                   REPLACE.161
 24   CONTINUE                                                                   REPLACE.162
 23   CONTINUE                                                                   REPLACE.163
C                                                                                REPLACE.164
      IF (PFLAG) THEN                                                            REPLACE.165
      NAME='TC_FINAL'                                                            REPLACE.166
      CALL OUTPT(CT,IYY,1,IYY-1, 1,JXX, 1, JXX-1,1, KSIGT,NAME)                  REPLACE.167
      NAME='TF_FINAL'                                                            REPLACE.168
      CALL OUTPT(FT,IYYF,1,IYYF-1,1,JXXF,1,JXXF-1,1,KSIGT,NAME)                  REPLACE.169
      ENDIF                                                                      REPLACE.170
C                                                                                REPLACE.171
C  OVER THE OCEAN, SET LANDUSE = WATER CATEGORY NO (7 or NWV):                   06NOV00.268
C                                                                                REPLACE.173
          DO 2020 I = 1,IYY                                                      REPLACE.174
          DO 2020 J = 1,JXX                                                      REPLACE.175
          IF(CT(I,J).GE.0.0 .OR. CT(I,J).LE.-1.) GO TO 2020                      REPLACE.176
          IF (VEGESOIL) THEN                                                     REPLACE.177
          CL(I,J) = NVW                                                          REPLACE.178
          ELSE                                                                   REPLACE.179
          CL(I,J) = 7.                                                           REPLACE.180
          ENDIF                                                                  REPLACE.181
2020      CONTINUE                                                               REPLACE.182
C                                                                                REPLACE.183
        REWIND IINF                                                              REPLACE.184
        WRITE(IINC)CT                                                            REPLACE.185
        WRITE(IINC)CL                                                            REPLACE.186
        IF (VEGESOIL) THEN                                                       REPLACE.187
          IF (.NOT.VEGONLY) THEN                                                 REPLACE.188
          DO IC = 1,NVC                                                          REPLACE.189
            WRITE(IINC) ((VC (I,J,IC),I=1,IYY),J=1,JXX)                          REPLACE.190
          END DO                                                                 REPLACE.191
          DO IC = 1,NSC                                                          REPLACE.192
            WRITE(IINC) ((SC (I,J,IC),I=1,IYY),J=1,JXX)                          REPLACE.193
          END DO                                                                 REPLACE.194
          DO IC = 1,12                                                           REPLACE.195
            WRITE(IINC) ((VFC(I,J,IC),I=1,IYY),J=1,JXX)                          REPLACE.196
          END DO                                                                 REPLACE.197
          WRITE(IINC) TGC                                                        REPLACE.198
          WRITE(IINC) WLC                                                        REPLACE.199
C         WRITE(IINC) XLVC                                                       REPLACE.200
          WRITE(IINC) XLSC                                                       REPLACE.201
          ENDIF                                                                  REPLACE.202
        ENDIF                                                                    REPLACE.203
C                                                                                REPLACE.204
        RETURN                                                                   REPLACE.205
        END                                                                      REPLACE.206
