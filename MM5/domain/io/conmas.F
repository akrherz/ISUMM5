      SUBROUTINE CONMAS(ID,IM,MC)                                                CONMAS.1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          CONMAS.2
C                                                                     C          CONMAS.3
C                                                                     C          CONMAS.4
C     THIS SUBROUTINE COMPUTES THE TOTAL DRY AIR AND WATER SUBSTANCE  C          CONMAS.5
C     WITHIN THE DOMAIN AND COMPARES WITH THE INITIAL VALUES.         C          CONMAS.6
C                                                                     C          CONMAS.7
C     ---THE UNIT USED IN ALL THE CALCULATION IS "KG".                C          CONMAS.8
C                                                                     C          CONMAS.9
C                                                                     C          CONMAS.10
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          CONMAS.11
#     include <parame.incl>                                                      CONMAS.12
#     include <param3.incl>                                                      CONMAS.13
#     include <point3d.incl>                                                     CONMAS.14
#     include <point2d.incl>                                                     CONMAS.15
#     include <various.incl>                                                     CONMAS.16
C                                                                                CONMAS.17
C----------------------------------------------------------------------          CONMAS.18
C                                                                                CONMAS.19
      IYY=IL                                                                     CONMAS.20
      JXX=JL                                                                     CONMAS.21
      KZZ=KL                                                                     CONMAS.22
      IYYM=1                                                                     CONMAS.23
      JXXM=1                                                                     CONMAS.24
      KZZM=1                                                                     CONMAS.25
      IF(IM.EQ.2)THEN                                                            CONMAS.26
        IYYM=IL                                                                  CONMAS.27
        JXXM=JL                                                                  CONMAS.28
        KZZM=KL                                                                  CONMAS.29
      ENDIF                                                                      CONMAS.30
      ERROR1=0.                                                                  CONMAS.31
      ERROR2=0.                                                                  CONMAS.32
C                                                                                CONMAS.33
C-----COMPUTE THE TOTAL DRY AIR AND WATER SUBSTANCE IN THE MODEL AT              CONMAS.34
C     THIS TIME:                                                                 CONMAS.35
C                                                                                CONMAS.36
      CALL TMASS(TDRYM,TQMASS,TVMASS,TCMASS,TRMASS,IYY,JXX,KZZ,ID,IM,PSA         CONMAS.37
     +     ,QVA,QCA,QRA,DSIGMA,DX,G)                                             CONMAS.38
C                                                                                CONMAS.39
C-----CONSERVATION OF DRY AIR:                                                   CONMAS.40
C                                                                                CONMAS.41
      TDRYM=TDRYM-TDADV                                                          CONMAS.42
      ERROR1=(TDRYM-TDINI)/TDINI*100.                                            CONMAS.43
C                                                                                CONMAS.44
C-----CONSERVATION OF WATER SUBSTANCE:                                           CONMAS.45
C                                                                                CONMAS.46
      IF(ID.EQ.1)GOTO 20                                                         CONMAS.47
C                                                                                CONMAS.48
C-----TOTAL RAINFALL AT THIS TIME:                                               CONMAS.49
C                                                                                CONMAS.50
      TCRAI=0.                                                                   CONMAS.51
      TNCRAI=0.                                                                  CONMAS.52
      DO 10 I=1,ILX                                                              CONMAS.53
        DO 10 J=1,JLX                                                            CONMAS.54
          TCRAI=TCRAI+RAINC(I,J)*10.*DXSQ                                        CONMAS.55
          TNCRAI=TNCRAI+RAINNC(I,J)*10.*DXSQ                                     CONMAS.56
   10   CONTINUE                                                                 CONMAS.57
                                                                                 CONMAS.58
      TQRAI=TCRAI+TNCRAI                                                         CONMAS.59
C                                                                                CONMAS.60
      TQMASS=TQMASS+TQRAI-TQEVA-TQADV                                            CONMAS.61
      ERROR2=(TQMASS-TQINI)/TQINI*100.                                           CONMAS.62
C                                                                                CONMAS.63
C-----PRINT OUT THE INFORMATION:                                                 CONMAS.64
C                                                                                CONMAS.65
   20 IF(MOD(KTAU,MC).NE.0)GOTO 50                                               CONMAS.66
      XH=XTIME/60.                                                               CONMAS.67
      PRINT30,XH,TDRYM,ERROR1,TDADV                                              CONMAS.68
   30 FORMAT(11X,'AT TIME = ',F10.3,' H, TOTAL AIR  = ',E12.5,' KG, CHA'         07NOV00.319
     1,'NGE = ',E12.5,' %.'/38X,'HORIZONTAL ADVECTION = ',E12.5,' KG.')          07NOV00.320
C                                                                                CONMAS.71
      IF(ID.EQ.1)GOTO 50                                                         CONMAS.72
      PRINT40,TQMASS,ERROR2,TVMASS,TCMASS,TRMASS,TQADV,TCRAI,TNCRAI,             CONMAS.73
     +        TQEVA                                                              CONMAS.74
   40 FORMAT(33X,'TOTAL WATER = ',E12.5,' KG, CHANGE = ',E12.5,' %.'/            07NOV00.321
     1       35X,'QV = ',E12.5,' KG,  QC = ',E12.5,' KG,  QR = ',E12.5,          CONMAS.76
     2           ' KG.'/                                                         CONMAS.77
     3       38X,'HORIZONTAL ADVECTION    = ',E12.5,' KG.'/                      CONMAS.78
     4       38X,'CONVECTIVE RAINFALL     = ',E12.5,' KG.'/                      CONMAS.79
     5       38X,'NONCONVECTIVE RAINFALL  = ',E12.5,' KG.'/                      CONMAS.80
     6       38X,'EVAPORATION FROM GROUND = ',E12.5,' KG.')                      CONMAS.81
C                                                                                CONMAS.82
   50 RETURN                                                                     CONMAS.83
      END                                                                        CONMAS.84
                                                                                 CONMAS.85
