      SUBROUTINE RFLP(YLAT,YLON,R,FLP)                                           RFLP.1
C                                                                                RFLP.2
C   PURPOSE :  COMPUTES THE DISTANCE FROM THE POLE TO THE LATITUDE               RFLP.3
C              AND LONGITUDE (YLAT,YLON) LOCATION AND THE ANGLE                  RFLP.4
C              BETWEEN THE CENTRAL LONGITUDE AND THE LONGITUDE YLON              RFLP.5
C              (SEE SECTION 3.1).                                                RFLP.6
C   ON ENTRY : YLAT: LATITUDE.                                                   RFLP.7
C              YLON: LONGITUDE.                                                  RFLP.8
C   ON EXIT :     R: THE DISTANCE FROM THE POLE TO THE LATITUDE                  RFLP.9
C                    AND LONGITUDE (YLAT,YLON) LOCATION.                         RFLP.10
C               FLP: THE ANGLE BETWEEN THE CENTRAL LONGITUDE AND THE             RFLP.11
C                    LONGITUDE YLON.                                             RFLP.12
C   CALLS :    THE INTRINSIC FUNCTIONS SIN, COS, TAN, ALOG.                      RFLP.13
C   CALLED BY :SUBROUTINE XYOBSLL.                                               RFLP.14
C   COMMENTS :                                                                   RFLP.15
C  THE COMMON BLOCK /MAPS/ PROVIDES THE INFORMATION ABOUT THE MAP                RFLP.16
C  BACKGROUND IN THIS SUBROUTINE. IN NORTHERN HEMISPHERE (PHIC < 0),             RFLP.17
C  FOR LAMBERT CONFORMAL AND POLAR STEREOGRAPHIC PROJECTIONS, R < 0.             RFLP.18
C  FOR MERCATOR PROJECTION, R > 0. IN SOUTHERN HEMISPHERE (PHIC < 0),            RFLP.19
C  IT IS VICE VERSA.                                                             RFLP.20
C                                                                                RFLP.21
# include <maps.incl>                                                            RFLP.22
C                                                                                RFLP.23
      YLONRT=YLON-XLONC                                                          RFLP.24
      IF(YLONRT.GT. 180.) YLONRT=YLONRT-360.                                     RFLP.25
      IF(YLONRT.LT.-180.) YLONRT=YLONRT+360.                                     RFLP.26
C                                                                                RFLP.27
      IF(IPROJ.EQ.NPROJ(3)) THEN                                                 RFLP.28
      PHI1 = 0.0                                                                 RFLP.29
      PHI1 = PHI1/CONV                                                           RFLP.30
      C2 = A*COS(PHI1)                                                           RFLP.31
      FLP = C2*YLONRT/CONV                                                       RFLP.32
      PHICTR = YLAT/CONV                                                         RFLP.33
      CELL = COS(PHICTR)/(1.0+SIN(PHICTR))                                       RFLP.34
      R = -C2*ALOG(CELL)                                                         RFLP.35
      RETURN                                                                     RFLP.36
      ENDIF                                                                      RFLP.37
                                                                                 RFLP.38
      FLPP=YLONRT/CONV                                                           RFLP.39
      FLP = XN*FLPP                                                              RFLP.40
      PSX = (POLE-YLAT)/CONV                                                     RFLP.41
      IF(IPROJ .NE. NPROJ(2))GO TO 75                                            RFLP.42
      CELL = A*SIN(PSX)/XN                                                       RFLP.43
      CELL2 = (1. + COS(PSI1))/(1. + COS(PSX))                                   RFLP.44
      GO TO 76                                                                   RFLP.45
  75  IF(IPROJ.NE.NPROJ(1))STOP75                                                RFLP.46
      CELL = A*SIN(PSI1)/XN                                                      RFLP.47
      CELL2 = (TAN(PSX/2.))/(TAN(PSI1/2.))                                       RFLP.48
  76  CONTINUE                                                                   RFLP.49
      R = -CELL*(CELL2)**XN                                                      RFLP.50
      RETURN                                                                     RFLP.51
      END                                                                        RFLP.52
