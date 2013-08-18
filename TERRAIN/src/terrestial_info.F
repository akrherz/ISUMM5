      SUBROUTINE TERRESTRIAL_INFO(XSOUTH,XWEST,CNTRIM,CNTRJM,DS,ICROSS,          TERRESTIAL_INFO.1
     &           IRATIO,F,SMAP,XLON,XLAT,IX,JX)                                  TERRESTIAL_INFO.2
C                                                                                TERRESTIAL_INFO.3
C  THIS ROUTINE CALCULATES MESO MAP(LAT,LONG,CORIOLIS,MAP SCALE)                 TERRESTIAL_INFO.4
C     ADAPTED FROM PROGRAM DATAGRID                                              TERRESTIAL_INFO.5
C                                                                                TERRESTIAL_INFO.6
C     IX IS THE I DIMENSION FOR THIS DOMAIN.                                     TERRESTIAL_INFO.7
C     JX IS THE J DIMENSION FOR THIS DOMAIN.                                     TERRESTIAL_INFO.8
C     XSOUTH AND XWEST ARE THE I AND J COORDINATES (FLOATING POINT) OF           TERRESTIAL_INFO.9
C            THE POINT (1,1) OF THIS NEST WITH RESPECT TO THE MOTHER OF          TERRESTIAL_INFO.10
C            ALL DOMAINS (MOAD).                                                 TERRESTIAL_INFO.11
C     CNTRIM AND CNTRJM ARE THE COORDINATES OF THE CENTER OF THE                 TERRESTIAL_INFO.12
C            MOAD IN THE (DOT-POINT) COORDINATE SYSTEM OF THIS DOMAIN.           TERRESTIAL_INFO.13
C     ICROSS IS ICROSS ( = 1)  OR IDOT ( = 0).                                   TERRESTIAL_INFO.14
C     IRATIO IS THE IRATIO FOR THIS DOMAIN, WITH RESPECT TO THE MOAD.            TERRESTIAL_INFO.15
C                                                                                TERRESTIAL_INFO.16
#include <maps.incl>                                                             TERRESTIAL_INFO.17
C                                                                                TERRESTIAL_INFO.18
      DIMENSION F(IX,JX),SMAP(IX,JX)                                             TERRESTIAL_INFO.19
      DIMENSION XLAT(IX,JX),XLON(IX,JX)                                          TERRESTIAL_INFO.20
      INTEGER ICROSS                                                             TERRESTIAL_INFO.21
                                                                                 TERRESTIAL_INFO.22
      POLE=90.                                                                   TERRESTIAL_INFO.23
      DEGRAN=1./CONV                                                             TERRESTIAL_INFO.24
      GFLAG=1.E36                                                                TERRESTIAL_INFO.25
C  EOMEG USES THE SIDEREAL DAY (NOT SOLAR DAY) - SAME AS IN MM5                  06NOV00.558
      OMEGA2=2.*7.2921E-5                                                        06NOV00.559
      CNTRJ = (CNTRJM-XWEST )*FLOAT(IRATIO)+(-.5*FLOAT(ICROSS))+1.               TERRESTIAL_INFO.27
      CNTRI = (CNTRIM-XSOUTH)*FLOAT(IRATIO)+(-.5*FLOAT(ICROSS))+1.               TERRESTIAL_INFO.28
      IF (PHIC.LT.0.0) POLE = -90.0                                              TERRESTIAL_INFO.29
      IF(IPROJ.NE.NPROJ(3)) THEN                                                 TERRESTIAL_INFO.30
         PSX = (POLE-PHIC)*DEGRAN                                                TERRESTIAL_INFO.31
         IF(IPROJ.EQ.NPROJ(2)) THEN                                              TERRESTIAL_INFO.32
            CELL=A*SIN(PSX)/XN                                                   TERRESTIAL_INFO.33
            CELL2 = (1. + COS(PSI1))/(1. + COS(PSX))                             TERRESTIAL_INFO.34
         ELSE                                                                    TERRESTIAL_INFO.35
            CELL=A*SIN(PSI1)/XN                                                  TERRESTIAL_INFO.36
            CELL2=(TAN(PSX/2.))/(TAN(PSI1/2.))                                   TERRESTIAL_INFO.37
         ENDIF                                                                   TERRESTIAL_INFO.38
         R=CELL*(CELL2)**XN                                                      TERRESTIAL_INFO.39
         XCNTR=0.                                                                TERRESTIAL_INFO.40
         YCNTR=-R                                                                TERRESTIAL_INFO.41
      ELSE                                                                       TERRESTIAL_INFO.42
C                                                                                TERRESTIAL_INFO.43
C        FOR MERCATOR PROJECTION TRUE AT PHI1                                    TERRESTIAL_INFO.44
C                                                                                TERRESTIAL_INFO.45
         PHI1=0.                                                                 TERRESTIAL_INFO.46
         PHI1=PHI1*DEGRAN                                                        TERRESTIAL_INFO.47
         C2=A*COS(PHI1)                                                          TERRESTIAL_INFO.48
         XCNTR=0.                                                                TERRESTIAL_INFO.49
         PHICTR=PHIC*DEGRAN                                                      TERRESTIAL_INFO.50
         CELL=COS(PHICTR)/(1.+SIN(PHICTR))                                       TERRESTIAL_INFO.51
         YCNTR=-C2*ALOG(CELL)                                                    TERRESTIAL_INFO.52
      ENDIF                                                                      TERRESTIAL_INFO.53
      II1 = IX - ICROSS                                                          TERRESTIAL_INFO.54
      JJ1 = JX - ICROSS                                                          TERRESTIAL_INFO.55
      DO 70 I=1,II1                                                              TERRESTIAL_INFO.56
         Y=YCNTR+(I-CNTRI)*DS                                                    TERRESTIAL_INFO.57
         DO 70 J=1,JJ1                                                           TERRESTIAL_INFO.58
            X=XCNTR+(J-CNTRJ)*DS                                                 TERRESTIAL_INFO.59
            IF(IPROJ.NE.NPROJ(3)) THEN                                           TERRESTIAL_INFO.60
               R = SQRT(X*X + Y*Y)                                               TERRESTIAL_INFO.61
               IF(Y.EQ.0.) THEN                                                  TERRESTIAL_INFO.62
                  IF(X.GE.0.) THEN                                               TERRESTIAL_INFO.63
                     FLP=90.*DEGRAN                                              TERRESTIAL_INFO.64
                  ELSE                                                           TERRESTIAL_INFO.65
                     FLP=-90.*DEGRAN                                             TERRESTIAL_INFO.66
                  ENDIF                                                          TERRESTIAL_INFO.67
               ELSE                                                              TERRESTIAL_INFO.68
                  IF (PHIC.LT.0.0) THEN                                          TERRESTIAL_INFO.69
                     FLP = ATAN2(X,Y)                                            TERRESTIAL_INFO.70
                  ELSE                                                           TERRESTIAL_INFO.71
                     FLP = ATAN2(X,-Y)                                           TERRESTIAL_INFO.72
                  END IF                                                         TERRESTIAL_INFO.73
               END IF                                                            TERRESTIAL_INFO.74
               FLPP = FLP/XN/DEGRAN+XLONC                                        TERRESTIAL_INFO.75
               IF (FLPP.GT.180.0) FLPP = FLPP-360.0                              TERRESTIAL_INFO.76
               IF (FLPP.LT.-180.0) FLPP = FLPP+360.0                             TERRESTIAL_INFO.77
               XLON(I,J) = FLPP                                                  TERRESTIAL_INFO.78
               IF(IPROJ.EQ.NPROJ(1)) THEN                                        TERRESTIAL_INFO.79
                  IF (PHIC.LT.0.0) R = -R                                        TERRESTIAL_INFO.80
                  CELL=R*XN/(A*SIN(PSI1))                                        TERRESTIAL_INFO.81
                  CELL1=TAN(PSI1/2.)*CELL**(1./XN)                               TERRESTIAL_INFO.82
               ELSEIF(IPROJ.EQ.NPROJ(2)) THEN                                    TERRESTIAL_INFO.83
                  IF (PHIC.LT.0.0) R = -R                                        TERRESTIAL_INFO.84
                  CELL = R/A                                                     TERRESTIAL_INFO.85
                  CELL1 = CELL/(1.0 + COS(PSI1))                                 TERRESTIAL_INFO.86
               ENDIF                                                             TERRESTIAL_INFO.87
               CELL2=ATAN(CELL1)                                                 TERRESTIAL_INFO.88
               PSX=2.*CELL2/DEGRAN                                               TERRESTIAL_INFO.89
               XLAT(I,J) = POLE - PSX                                            TERRESTIAL_INFO.90
               XLAT1=XLAT(I,J)*DEGRAN                                            TERRESTIAL_INFO.91
               PSIX=PSX*DEGRAN                                                   TERRESTIAL_INFO.92
               IF(IPROJ .EQ. NPROJ(1)) THEN                                      TERRESTIAL_INFO.93
                  F(I,J)=OMEGA2*SIN(XLAT1)                                       TERRESTIAL_INFO.94
                  SMAP(I,J)=(SIN(PSI1)/SIN(PSIX))*                               TERRESTIAL_INFO.95
     .                 ((TAN(PSIX/2.)/TAN(PSI1/2.))**XN)                         TERRESTIAL_INFO.96
               ELSEIF(IPROJ.EQ.NPROJ(2)) THEN                                    TERRESTIAL_INFO.97
                  F(I,J) = OMEGA2*SIN(XLAT1)                                     TERRESTIAL_INFO.98
                  SMAP(I,J) = ((1.0 + COS(PSI1))/(1.0 + COS(PSIX)))**XN          TERRESTIAL_INFO.99
               ENDIF                                                             TERRESTIAL_INFO.100
            ELSEIF(IPROJ.EQ.NPROJ(3)) THEN                                       TERRESTIAL_INFO.101
C                                                                                TERRESTIAL_INFO.102
C              CALCULATIONS FOR MERCATOR                                         TERRESTIAL_INFO.103
C                                                                                TERRESTIAL_INFO.104
               XLON(I,J)=XLONC+((X-XCNTR)/C2)/DEGRAN                             TERRESTIAL_INFO.105
               IF(XLON(I,J).GT. 180) XLON(I,J)=XLON(I,J)-360                     05DEC01.49
               IF(XLON(I,J).LT.-180) XLON(I,J)=XLON(I,J)+360                     05DEC01.50
               CELL=EXP(Y/C2)                                                    TERRESTIAL_INFO.106
               XLAT(I,J)=2.*(ATAN(CELL)/DEGRAN)-90.                              TERRESTIAL_INFO.107
               DEGLAT=XLAT(I,J)*DEGRAN                                           TERRESTIAL_INFO.108
               SMAP(I,J)=COS(PHI1)/COS(DEGLAT)                                   TERRESTIAL_INFO.109
               F(I,J)=OMEGA2*SIN(DEGLAT)                                         TERRESTIAL_INFO.110
            ENDIF                                                                TERRESTIAL_INFO.111
 70   CONTINUE                                                                   TERRESTIAL_INFO.112
      IF (ICROSS .EQ. 1) THEN                                                    TERRESTIAL_INFO.113
      DO 80 J=1,JX                                                               TERRESTIAL_INFO.114
         F(IX,J)=GFLAG                                                           TERRESTIAL_INFO.115
         SMAP(IX,J)=GFLAG                                                        TERRESTIAL_INFO.116
         XLAT(IX,J)=GFLAG                                                        TERRESTIAL_INFO.117
         XLON(IX,J)=GFLAG                                                        TERRESTIAL_INFO.118
 80   CONTINUE                                                                   TERRESTIAL_INFO.119
      DO 90 I=1,IX-1                                                             TERRESTIAL_INFO.120
         F(I,JX)=GFLAG                                                           TERRESTIAL_INFO.121
         SMAP(I,JX)=GFLAG                                                        TERRESTIAL_INFO.122
         XLAT(I,JX)=GFLAG                                                        TERRESTIAL_INFO.123
         XLON(I,JX)=GFLAG                                                        TERRESTIAL_INFO.124
 90   CONTINUE                                                                   TERRESTIAL_INFO.125
      ENDIF                                                                      TERRESTIAL_INFO.126
      RETURN                                                                     TERRESTIAL_INFO.127
      END                                                                        TERRESTIAL_INFO.128
