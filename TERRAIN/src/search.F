      SUBROUTINE SEARCH(IX, JX, N, NI)                                           SEARCH.1
C                                                                                SEARCH.2
# include <parame.incl>                                                          SEARCH.3
# include <paramed.incl>                                                         06NOV00.269
# include <io.incl>                                                              06NOV00.270
# include <maps.incl>                                                            SEARCH.4
# include <nestdmn.incl>                                                         SEARCH.5
# include <ltdata.incl>                                                          SEARCH.6
# include <option.incl>                                                          SEARCH.7
C                                                                                SEARCH.8
      DIMENSION XLAT(IIMX*JJMX), XLON(IIMX*JJMX)                                 SEARCH.9
      DIMENSION LPOLE(20)                                                        SEARCH.10
C                                                                                SEARCH.11
C                                                                                SEARCH.12
C   GET THE CLOSEST TERRAIN AND LAND USE RESOLUTIONS BASED ON THE GRID S         SEARCH.13
C                                                                                SEARCH.14
      XEXTND = 2.5                                                               SEARCH.15
      DSS = 2.*DIS(N)                                                            SEARCH.16
      IF (DATASW) THEN                                                           SEARCH.17
        NI=NTYPE(N)                                                              SEARCH.18
      ELSE                                                                       SEARCH.19
        DO 10 I = 1,max_type                                                     06NOV00.271
          IF (DSS.GT.DISOBS(I)) THEN                                             SEARCH.21
            NI = I                                                               SEARCH.22
            GO TO 11                                                             SEARCH.23
          ENDIF                                                                  SEARCH.24
10      CONTINUE                                                                 SEARCH.25
      ENDIF                                                                      SEARCH.26
      IF (NI.EQ.max_type) THEN                                                   06NOV00.272
        PRINT 12,N,IX,JX                                                         SEARCH.28
 12     FORMAT(//5X,'DOMAIN',I3,'(',I3,',',I3,') USING 30-SEC DATA')             SEARCH.29
      ENDIF                                                                      SEARCH.30
C                                                                                SEARCH.31
11    IF (NI.GE.3) XEXTND = 1.0                                                  SEARCH.32
      DSS = DSS/2.0                                                              SEARCH.33
C                                                                                SEARCH.34
C   DEFINE THE SEARCH AREA FOR THE DOMAIN:                                       SEARCH.35
C                                                                                SEARCH.36
      IRAT = NRATIO(N)                                                           SEARCH.37
      CALL LATLON(IX,JX,XLAT,XLON,DSS,XSOUTH(N),XWEST(N),N,IRAT,                 SEARCH.38
     1                                                   IPRNTD)                 SEARCH.39
      CALL MXMNLL(IX,JX,XLAT,XLON,XSOUTH(N),XNORTH(N),XWEST(N),                  SEARCH.40
     1              XEAST(N),XEXTND,XMAXLAT,XMINLAT,XMAXLON,XMINLON,             SEARCH.41
     2              LPOLE,N-1)                                                   SEARCH.42
      PRINT 13,NI,XEXTND,XMAXLAT,XMINLAT,XMAXLON,XMINLON                         SEARCH.43
13    FORMAT(1X,'DATASET', I2/                                                   SEARCH.44
     1       1X,'XEXTND,XMAXLAT,XMINLAT,XMAXLON,XMINLON:',5F10.2)                SEARCH.45
C                                                                                SEARCH.46
4000  RETURN                                                                     SEARCH.47
      END                                                                        SEARCH.48
