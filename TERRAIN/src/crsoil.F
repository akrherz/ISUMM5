      SUBROUTINE CRSOIL(NCAT, NLAT, NLON, RESOL, XLAT_VS0, XLON_VS0,             CRSOIL.1
     -                   NI1,  NI2,  NI3,   NI4,   NUNIT1,   NUNIT2,             CRSOIL.2
     -                    IX,   JX, XLAT,  XLON,    VEGEC,    LVEGE,             CRSOIL.3
     -                  ZLON,  ZLAT,  VEGECAT, VEGECAT30S,    SHIFT,             CRSOIL.4
     -                     N,  LREC, GRDLNMN, GRDLTMN, IDW, NAME, IP)            06NOV00.31
C                                                                                CRSOIL.6
C     NI1 < 0, NI1 = -1 --> VEGETATION.                                          CRSOIL.7
C              NI1 = -2 --> SOIL,                                                CRSOIL.8
C              NI1 = -3 --> VEGETATION FRACTION,                                 CRSOIL.9
C              NI1 = -4 --> GROUND TEMPERATURE.                                  CRSOIL.10
C     NI2 = 1 --> 5 MIN. DATA, NI2 = 2 --> 30 SEC. DATA                          CRSOIL.11
C     NI3 = 1 --> READ LAND-USE DATA (VEGE., SOIL, VEGE. FRAC.)                  CRSOIL.12
C     NI4 = 1 --> READ TERRAIN HEIGHT (GROUD TEMP.)                              CRSOIL.13
C                                                                                CRSOIL.14
# include <parame.incl>                                                          CRSOIL.15
# include <paramesv.incl>                                                        CRSOIL.16
# include <paramed.incl>                                                         CRSOIL.17
# include <io.incl>                                                              06NOV00.32
# include <ltdata.incl>                                                          CRSOIL.18
C                                                                                06NOV00.33
      DIMENSION VEGEC(IX,JX,NCAT), LVEGE(IX,JX)                                  CRSOIL.19
      DIMENSION XLAT(IX,JX), XLON(IX,JX), ZLON(NOBT), ZLAT(NOBT),                CRSOIL.20
     -          VEGECAT(NCAT,NOBT), VEGECAT30S(NOBT)                             CRSOIL.21
      DIMENSION WORK(NOBT),IWORK1(13*2160/4+2),IWORK2(2160/2+2)                  CRSOIL.22
#if (defined(SUN) || defined(HP))
      INTEGER*2 IWORK3(NCMAX*43200)                                              CRSOIL.24
#else                                                                            CRSOIL.25
      INTEGER*1 IWORK3(NCMAX*43200)                                              CRSOIL.26
#endif                                                                           CRSOIL.27
      INTEGER*2 IWORK4(43200)                                                    CRSOIL.28
      CHARACTER*1 IWORK3_CHR(NCMAX*43200)                                        CRSOIL.29
      CHARACTER*2 IWORK4_CHR(43200)                                              CRSOIL.30
      CHARACTER*(*) NAME                                                         CRSOIL.31
C                                                                                CRSOIL.32
C .. FIVE RESOLUTIONS:                                                           CRSOIL.33
      IRESO_SEC = NINT(60.*(RESOL*60.))                                          CRSOIL.34
      DO II = 1,max_type                                                         06NOV00.34
        JRESO_SEC = NINT(60.*(CENTER(II)*60.))                                   06NOV00.35
        IF (IRESO_SEC.EQ.JRESO_SEC) THEN                                         06NOV00.36
          NR = II                                                                06NOV00.37
          GO TO 8                                                                06NOV00.38
        ENDIF                                                                    06NOV00.39
      END DO                                                                     06NOV00.40
C                                                                                CRSOIL.46
  8   IF (IP.EQ.1) PRINT 10,NAME,NCAT,RESOL,NR,N                                 06NOV00.41
 10   FORMAT(/'CALL CRSOIL: FOR ',A8,2X,I2,' CAT. RESOL=',F8.5,                  CRSOIL.48
     -        '(NR=',I2,'),  DOMAIN ',I2)                                        CRSOIL.49
      IF (NAME.EQ.'VEGE_CAT') THEN                                               CRSOIL.50
        NCAT_SV = NVC                                                            CRSOIL.51
      ELSE IF (NAME.EQ.'SOIL_CAT') THEN                                          CRSOIL.52
        NCAT_SV = NSC                                                            CRSOIL.53
      ENDIF                                                                      CRSOIL.54
C                                                                                CRSOIL.55
      IF ((NAME.EQ.'VEGE_CAT' .OR. NAME.EQ.'SOIL_CAT') .AND. IDW.EQ.15)          05DEC01.7
     - CALL CHECK_DATA(XMINLAT,XMAXLAT,XMINLON,XMAXLON,                          CRSOIL.57
     -                XLAT_VS0, XLON_VS0,NLAT,NLON,RESOL,NAME)                   CRSOIL.58
C                                                                                CRSOIL.59
C  .. READ IN THE DATA:                                                          CRSOIL.60
C                                                                                CRSOIL.61
         CALL RDLDTR(NCAT,NLON,NLAT,LREC,                                        CRSOIL.62
     1            XLAT_VS0,XLON_VS0,CENTER(NR),GRDLTMN,GRDLNMN,                  CRSOIL.63
     2            XMAXLAT,XMINLAT,XMAXLON,XMINLON,NI1,NI2,NI3,                   CRSOIL.64
     3             NUNIT1,LNDNAME(N),NI4,NUNIT2,TERNAME(N),                      CRSOIL.65
     4            IWORK1,IWORK2,IWORK3,IWORK4,                                   CRSOIL.66
     5                          IWORK3_CHR,IWORK4_CHR,IP)                        06NOV00.42
      IF (IP.EQ.1) PRINT 5001,NAME,N                                             06NOV00.43
 5001 FORMAT('==> ',A8,' DATA READ IN FOR DOMAIN ',I2)                           CRSOIL.69
C                                                                                CRSOIL.70
      IF (NR.EQ.max_type) THEN                                                   06NOV00.44
        CALL CRVST30S(NAME,NCAT_SV,IX,JX,XLAT,XLON,VEGEC,WORK,LVEGE,N,           CRSOIL.72
     1           LREC,GRDLNMN,GRDLTMN,ZLON,ZLAT,VEGECAT30S,RESOL,IDW,IP)         06NOV00.45
        RETURN                                                                   CRSOIL.74
      ELSE                                                                       CRSOIL.75
C                                                                                CRSOIL.76
        CALL CRVST(NAME, NCAT,IX,JX,XLAT,XLON,VEGEC,WORK,LVEGE,N,                CRSOIL.77
     1           LREC,GRDLNMN,GRDLTMN,ZLON,ZLAT,VEGECAT,RESOL,IDW,IP)            06NOV00.46
      ENDIF                                                                      CRSOIL.79
C                                                                                CRSOIL.80
      RETURN                                                                     CRSOIL.81
      END                                                                        CRSOIL.82
