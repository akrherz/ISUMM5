      SUBROUTINE LLXY(XLAT,XLON,X,Y,IYY,JXX,DSKM)                                LLXY.1
C                                                                                LLXY.2
C   PURPOSE   : CALCULATES THE X AND Y GIVEN THE LATITUDE AND LONGITUDE.         LLXY.3
C   ON ENTRY  :                                                                  LLXY.4
C   XLAT      : THE LATITUDE.                                                    LLXY.5
C   XLON      : THE LONGITUDE.                                                   LLXY.6
C   IEND,JEND : THE NUMBER OF DOMAIN GRID POINTS IN THE I AND J                  LLXY.7
C               DIRECTIONS IN THE COARSE DOMAIN WITH THE GRID                    LLXY.8
C               DISTANCE DSKM.                                                   LLXY.9
C   DSKM      : THE DOMAIN GRID DISTANCE.                                        LLXY.10
C   ON EXIT   :                                                                  LLXY.11
C   X         : THE COORDINATE IN X (J)-DIRECTION.                               LLXY.12
C   Y         : THE COORDINATE IN Y (I)-DIRECTION.                               LLXY.13
C                                                                                LLXY.14
C   CALLS THE INTRINSIC FUNCTIONS SIN, COS AND TAN.                              LLXY.15
C   CALLED BY SUBROUTINE PLTTER                                                  LLXY.16
C                                                                                LLXY.17
C   COMMENTS :                                                                   LLXY.18
C IN ADDITION TO THE LIST OF ARGUMENTS, SUBROUTINE LLXY GETS THE MAP             LLXY.19
C INFORMATION (THE CENTRAL LATITUDE (PHIC) AND LONGITUDE (XLONC),                LLXY.20
C THE CONE CONSTANT XN, ETC.) FROM THE COMMON BLOCK /MAPS/.  SINCE               LLXY.21
C PHIC AND XLONC ARE SET WITH RESPECT TO THE COARSE MESH, THE X AND              LLXY.22
C Y VALUES THAT COME OUT OF THE SUBROUTINE LLXY ARE THE COARSE DOMAIN            LLXY.23
C GRID INDICES THAT DEFINE THE STATION LOCATION.  THE TERRAIN PROGRAM            LLXY.24
C THEN COVERTS THE X AND Y VALUES BACK TO THE NESTED DOMAIN INDICES.             LLXY.25
C                                                                                LLXY.26
# include <maps.incl>                                                            LLXY.27
C                                                                                LLXY.28
      CENTRI = FLOAT(IYY + 1)/2.0                                                LLXY.29
      CENTRJ = FLOAT(JXX + 1)/2.0                                                LLXY.30
      DDDD   = CENTRJ*DSKM                                                       LLXY.31
C                                                                                LLXY.32
      IF (PROJECT.EQ.'ME') THEN                                                  LLXY.33
         XC = XCNTR                                                              LLXY.34
         YC = YCNTR                                                              LLXY.35
         IF (XLAT.NE.-90.) THEN                                                  LLXY.36
             CELL = COS(XLAT/CONV)/(1.0+SIN(XLAT/CONV))                          LLXY.37
             YY = -C2*ALOG(CELL)                                                 LLXY.38
             XX =  C2*(XLON-XLONC)/CONV                                          LLXY.39
             IF (XLONC.GT.0. .AND. XX.LT.-DDDD) THEN                             LLXY.40
               XX = 2*C2*(180.+XLONC)/CONV + XX                                  LLXY.41
             ELSE IF (XLONC.LT.0. .AND. XX.GT.DDDD) THEN                         LLXY.42
               XX = XX - C2*360./CONV                                            LLXY.43
             ENDIF                                                               LLXY.44
         ENDIF                                                                   LLXY.45
      ELSE                                                                       LLXY.46
      PSI0 = ( POLE - PHIC )/CONV                                                LLXY.47
      XC = 0.0                                                                   LLXY.48
      YC = -A/XN*SIN(PSI1)*(TAN(PSI0/2.0)/TAN(PSI1/2.0))**XN                     LLXY.49
C                                                                                LLXY.50
C     CALCULATE X,Y COORDS. RELATIVE TO POLE                                     LLXY.51
C                                                                                LLXY.52
      YLON = XLON - XLONC                                                        LLXY.53
      IF(YLON.GT.180) YLON = YLON - 360.                                         LLXY.54
      IF(YLON.LT.-180) YLON = YLON + 360.                                        LLXY.55
      FLP = XN*YLON/CONV                                                         LLXY.56
      PSX = ( POLE - XLAT )/CONV                                                 LLXY.57
      R = -A/XN*SIN(PSI1)*(TAN(PSX/2.0)/TAN(PSI1/2.0))**XN                       LLXY.58
      IF ( PHIC.LT.0.0 ) THEN                                                    LLXY.59
         XX = R*SIN(FLP)                                                         LLXY.60
         YY = R*COS(FLP)                                                         LLXY.61
      ELSE                                                                       LLXY.62
         XX = -R*SIN(FLP)                                                        LLXY.63
         YY = R*COS(FLP)                                                         LLXY.64
      END IF                                                                     LLXY.65
C                                                                                LLXY.66
      ENDIF                                                                      LLXY.67
C                                                                                LLXY.68
C  TRANSFORM (1,1) TO THE ORIGIN                                                 LLXY.69
C                                                                                LLXY.70
      X = ( XX - XC )/DSKM + CENTRJ                                              LLXY.71
      Y = ( YY - YC )/DSKM + CENTRI                                              LLXY.72
      RETURN                                                                     LLXY.73
      END                                                                        LLXY.74
                                                                                 LLXY.75
