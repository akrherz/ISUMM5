      SUBROUTINE READ30S(NREC,NPOINTS,ITER30S,XLATI,XLONI,NOUT,                  READ30S.1
     -                   PRNLAT)                                                 READ30S.2
C                                                                                READ30S.3
CC      INTEGER*2 ITER30S(NPOINTS)                                               READ30S.4
      CHARACTER*2 ITER30S(NPOINTS)                                               READ30S.5
C                                                                                READ30S.6
      PRINT 5,NREC,NPOINTS,XLATI,XLONI,NOUT                                      READ30S.7
 5    FORMAT('IN READ30S: NREC=',I5,' NPOINTS=',I5,                              READ30S.8
     -       ' LAT0=',F15.8,' LON-',F15.8,'  UNIT=',I2)                          READ30S.9
C                                                                                READ30S.10
        DO I = 1,NREC                                                            READ30S.11
          XLAT = XLATI - (I-1)/120.                                              READ30S.12
          READ(NOUT,REC=I) ITER30S                                               READ30S.13
CC          PRINT 6,I,XLAT                                                       READ30S.14
 6        FORMAT('I=',I5,'  LAT=',F15.8)                                         READ30S.15
C                                                                                READ30S.16
          IF (ABS(XLAT-PRNLAT).LT.1.E-5) THEN                                    READ30S.17
            DO J = 1,NPOINTS                                                     READ30S.18
              XLON = XLONI + (J-1)/120.                                          READ30S.19
              ITERN = IA(ITER30S(J),2,-9999)                                     READ30S.20
              PRINT 10,I,XLAT,J,XLON,ITER30S(J)                                  READ30S.21
 10           FORMAT('I=',I5,' LAT=',F15.8,'  J=',I5,                            READ30S.22
     -               ' LON=',F15.8,' TER=',I5)                                   READ30S.23
            END DO                                                               READ30S.24
          ENDIF                                                                  READ30S.25
C                                                                                READ30S.26
        END DO                                                                   READ30S.27
C                                                                                READ30S.28
      RETURN                                                                     READ30S.29
      END                                                                        READ30S.30
