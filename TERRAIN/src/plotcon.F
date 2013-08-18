      SUBROUTINE PLOTCON(MAPUNIT,IMX,JMX,IYY,JXX,IEX,JEX,VEGEC,NVC,              PLOTCON.1
     -                             GRDOUT,FINC,N,N1,NAME,IFILL,IP)               06NOV00.122
C                                                                                PLOTCON.3
      DIMENSION VEGEC(IMX,JMX,NVC), GRDOUT(JXX,IYY)                              PLOTCON.4
      REAL LEVELS(100)                                                           PLOTCON.5
      CHARACTER NAME*8, TITLE*80                                                 PLOTCON.6
      LOGICAL IFILL                                                              PLOTCON.7
C                                                                                PLOTCON.8
      IF(IP.EQ.1)PRINT 5,IMX,JMX,IYY,JXX,IEX,JEX,NVC,N,N1                        06NOV00.123
  5   FORMAT(/'PLOTCON: IMX,JMX:',2I4,'  IYY,JXX:',2I4,                          PLOTCON.10
     >        '  IEX,JEX:',2I4,'  NC=',I2,'  N=',I3,' N1=',I3)                   PLOTCON.11
      XINC=1./FLOAT(JXX) * 0.5                                                   PLOTCON.12
         LEVELS(1) =   10.0                                                      PLOTCON.13
         LEVELS(2) =  110.0                                                      PLOTCON.14
         LEVELS(3) = - 10.0                                                      PLOTCON.15
C                                                                                PLOTCON.16
C  DO THE PLOT                                                                   PLOTCON.17
C                                                                                PLOTCON.18
#ifdef NCARG                                                                     PLOTCON.19
C  .....................................................................         PLOTCON.20
      DO 20 II = 1,NVC                                                           PLOTCON.21
C                                                                                PLOTCON.22
        XMINM =  1.E15                                                           PLOTCON.23
        XMAXM = -1.E15                                                           PLOTCON.24
        DO I=1,IYY-1                                                             PLOTCON.25
        DO J=1,JXX-1                                                             PLOTCON.26
        GRDOUT(J,I) = VEGEC(IEX+I,JEX+J,II)                                      PLOTCON.27
        IF (GRDOUT(J,I).GT.XMAXM) XMAXM = GRDOUT(J,I)                            PLOTCON.28
        IF (GRDOUT(J,I).LT.XMINM) XMINM = GRDOUT(J,I)                            PLOTCON.29
        END DO                                                                   PLOTCON.30
        END DO                                                                   PLOTCON.31
C                                                                                PLOTCON.32
        WRITE(TITLE,10) NAME,II,N                                                PLOTCON.33
 10     FORMAT(1X,A8,I2,' PERCENTAGE  DOMAIN',I2)                                PLOTCON.34
        IF (ABS(XMINM-XMAXM).LT.1.E-2) THEN                                      PLOTCON.35
          IF (IP.EQ.1) PRINT 19,II                                               06NOV00.124
 19       FORMAT(/5X,'** NO PLOT FOR CATEGOTY ',I2,                              PLOTCON.37
     -               ', BECAUSE OF THE CONSTANT FIELD. **')                      PLOTCON.38
          GO TO 12                                                               PLOTCON.39
        ENDIF                                                                    PLOTCON.40
C                                                                                PLOTCON.41
        CALL SET (0.0,1.0,0.0,1.0,0.0,100.,0.0,100.,1)                           PLOTCON.42
        CALL PLCHHQ(50.,95.,TITLE(1:32),.02,0.,0.)                               06NOV00.125
C                                                                                PLOTCON.44
C  TURN OFF CLIPPING                                                             PLOTCON.45
                                                                                 PLOTCON.46
      CALL GSCLIP (0)                                                            PLOTCON.47
                                                                                 PLOTCON.48
      IF(IFILL) THEN                                                             PLOTCON.49
                                                                                 PLOTCON.50
C  OPEN UNIT MAPUNIT AS TABLE TO READ CONDRV INFORMATION FROM                    PLOTCON.51
                                                                                 PLOTCON.52
         OPEN (MAPUNIT,FILE='confiP.tbl',STATUS='OLD')                           PLOTCON.53
                                                                                 PLOTCON.54
C  SET UP A COLOR TABLE                                                          PLOTCON.55
                                                                                 PLOTCON.56
         CALL RDCOLT (MAPUNIT)                                                   PLOTCON.57
C                                                                                PLOTCON.58
         CALL SET (XINC,1.-XINC,XINC,1.-XINC,                                    PLOTCON.59
     *          0.0,1.0,0.0,1.0,1)                                               PLOTCON.60
         CALL CONDRV (GRDOUT,JXX,IYY,1,1,JXX-1,IYY-1,1,-1,                       PLOTCON.61
     *                LEVELS,.TRUE.,                                             PLOTCON.62
     *                'NO',1.0,TITLE,0,MAPUNIT,0,0)                              PLOTCON.63
                                                                                 PLOTCON.64
C  CLOSE THE TABLES                                                              PLOTCON.65
                                                                                 PLOTCON.66
         CLOSE (MAPUNIT)                                                         PLOTCON.67
                                                                                 PLOTCON.68
         CALL GFLAS3(N1)                                                         PLOTCON.69
         CALL FRAME                                                              PLOTCON.70
      ELSE                                                                       PLOTCON.71
                                                                                 PLOTCON.72
         OPEN (MAPUNIT,FILE='con.tbl',STATUS='OLD')                              PLOTCON.73
         CALL RDCOLT (MAPUNIT)                                                   PLOTCON.74
                                                                                 PLOTCON.75
         CALL SET (XINC,1.-XINC,XINC,1.-XINC,                                    PLOTCON.76
     *           0.0,1.0,0.0,1.0,1)                                              PLOTCON.77
                                                                                 PLOTCON.78
         CALL CONDRV (GRDOUT,JXX,IYY,1,1,JXX-1,IYY-1,1,-1,                       PLOTCON.79
     *                LEVELS,.TRUE.,                                             PLOTCON.80
     *                'NO',1.0,TITLE,0,-1,0,0)                                   PLOTCON.81
C                                                                                PLOTCON.82
         CALL GFLAS3(N1)                                                         PLOTCON.83
         CALL FRAME                                                              PLOTCON.84
C                                                                                PLOTCON.85
C  CLOSE THE TABLES                                                              PLOTCON.86
         CLOSE (MAPUNIT)                                                         PLOTCON.87
C                                                                                PLOTCON.88
      ENDIF                                                                      PLOTCON.89
      IF(IP.EQ.1)PRINT *,'      ===> ',NAME,' PLOTTED'                           06NOV00.126
 12   IF(IP.EQ.1)PRINT 11,TITLE(1:32),XMAXM,XMINM                                06NOV00.127
 11   FORMAT(A32,'  MAX=',E12.5,'  MIN=',E12.5)                                  PLOTCON.92
C                                                                                PLOTCON.93
 20   CONTINUE                                                                   PLOTCON.94
#endif                                                                           PLOTCON.95
C                                                                                PLOTCON.96
      RETURN                                                                     PLOTCON.97
      END                                                                        PLOTCON.98
