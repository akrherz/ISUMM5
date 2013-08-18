      SUBROUTINE GET_DEM(IUNIT,NE,ME,N1,N2)                                      GET_DEM.1
C                                                                                GET_DEM.2
      CHARACTER*2 NE(N1,N2), ME(N1)                                              GET_DEM.3
      DIMENSION YE(80,100)                                                       GET_DEM.4
C                                                                                GET_DEM.5
        LENGTH = N1*N2/2                                                         GET_DEM.6
        PRINT 5,IUNIT,N1,N2,LENGTH                                               GET_DEM.7
5       FORMAT(/'UNIT =',I2,'  DIMENSION: N1=',I5,' N2=',I5,                     GET_DEM.8
     >        '  RECORD LENGTH=',I10)                                            GET_DEM.9
C                                                                                GET_DEM.10
CC        OPEN(IUNIT,ACCESS='DIRECT',RECL=LENGTH)                                GET_DEM.11
CC        READ(IUNIT,REC=1)NE                                                    GET_DEM.12
        READ(IUNIT) NE                                                           GET_DEM.13
C INVERT AND CONVERT TO REAL                                                     GET_DEM.14
      DO J=1,N2                                                                  GET_DEM.15
        DO I=1,N1                                                                GET_DEM.16
          ME(I)  =NE(I,J)                                                        GET_DEM.17
        ENDDO                                                                    GET_DEM.18
        WRITE(IUNIT+18) ME                                                       GET_DEM.19
      ENDDO                                                                      GET_DEM.20
C                                                                                GET_DEM.21
      RETURN                                                                     GET_DEM.22
      END                                                                        GET_DEM.23
