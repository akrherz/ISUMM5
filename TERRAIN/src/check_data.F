      SUBROUTINE CHECK_DATA(XMINLAT,XMAXLAT,XMINLON,XMAXLON,                     CHECK_DATA.1
     -                      XLAT0, XLON0,NLAT,NLON,RESOL,NAME)                   CHECK_DATA.2
C                                                                                CHECK_DATA.3
      CHARACTER*(*) NAME                                                         CHECK_DATA.4
C                                                                                CHECK_DATA.5
        XLAT1 = XLAT0 - FLOAT(NLAT)*RESOL                                        CHECK_DATA.6
        XLON1 = XLON0 + FLOAT(NLON)*RESOL                                        CHECK_DATA.7
C                                                                                CHECK_DATA.8
C .. TO TRANSFORM THE LONGITUDE WITH IN RAGNE OF -180 TO 180:                    CHECK_DATA.9
C                                                                                CHECK_DATA.10
        XMINLN = XMINLON                                                         CHECK_DATA.11
        IF (XMINLN.LT.-180.) XMINLN = 360.+XMINLN                                CHECK_DATA.12
        IF (XMINLN.GT. 180.) XMINLN = XMINLN-360.                                CHECK_DATA.13
                                                                                 CHECK_DATA.14
        XMAXLN = XMAXLON                                                         CHECK_DATA.15
        IF (XMAXLN.LT.-180.) XMAXLN = 360.+XMAXLN                                CHECK_DATA.16
        IF (XMAXLN.GT. 180.) XMAXLN = XMAXLN-360.                                CHECK_DATA.17
                                                                                 CHECK_DATA.18
C FOR POLAR PROJECTION (XMAXLN= 180.0), TO SET                                   CHECK_DATA.19
         XMAXLN = XMAXLN - RESOL - 1.E-8                                         CHECK_DATA.20
         IF (XMAXLN.LT.-180.) XMAXLN = -180.                                     06NOV00.5
         IF (XMAXLN.GT. 180.) XMAXLN =  180.                                     06NOV00.6
C                                                                                CHECK_DATA.21
        IF (XMINLAT.LT.XLAT1 .OR. XMAXLAT.GT.XLAT0  .OR.                         CHECK_DATA.22
     -    .NOT.(XMINLN.GE.XLON0.AND.XMINLN.LE.XLON1) .OR.                        CHECK_DATA.23
     -    .NOT.(XMAXLN.GE.XLON0.AND.XMAXLN.LE.XLON1) ) THEN                      CHECK_DATA.24
          PRINT 10, NAME,XLAT1, XLAT0, XLON0, XLON1                              CHECK_DATA.25
  10      FORMAT('$$ ',A8,' DATA AVAILABLE ==> LAT.(',                           CHECK_DATA.26
     -          F10.5,',',F10.5,'); LON.(',F10.5,',',F10.5,')')                  CHECK_DATA.27
          PRINT 11,XMINLAT,XMAXLAT,XMINLON,XMAXLON,XMINLN,XMAXLN                 CHECK_DATA.28
  11      FORMAT('THE LAND-WATER MASK DATA ARE NOT ',                            CHECK_DATA.29
     -          'ABLE TO SATISFY THE REQUESTED GEOGRAPHIC AREA.'/                CHECK_DATA.30
     -           '  XMINLAT,XMAXLAT:(',F10.5,',',F10.5,');',                     CHECK_DATA.31
     -           '  XMINLON,XMAXLON:(',F10.5,',',F10.5,');',                     CHECK_DATA.32
     -           '  XMINLN ,XMAXLN :(',F10.5,',',F10.5,')')                      CHECK_DATA.33
          STOP                                                                   CHECK_DATA.34
        ENDIF                                                                    CHECK_DATA.35
C                                                                                CHECK_DATA.36
        RETURN                                                                   CHECK_DATA.37
        END                                                                      CHECK_DATA.38
C                                                                                CHECK_DATA.39
