       SUBROUTINE OUTPUT(IEXEC,NESCOUO)                                          OUTPUT.1
#     include <defines.incl>                                                     OUTPUT.2
#     include <parame.incl>                                                      OUTPUT.3
#     include <addr0.incl>                                                       OUTPUT.4
#     include <param2.incl>                                                      OUTPUT.5
#     include <parfdda.incl>                                                     OUTPUT.6
#     include <various.incl>                                                     OUTPUT.7
#     include <varia.incl>                                                       OUTPUT.8
       DIMENSION IEXEC(MAXNES)                                                   OUTPUT.9
#     include <addrcu.incl>                                                      OUTPUT.10
#     include <parkfcu2.incl>                                                    05DEC01.208
#ifdef MPP1                                                                      OUTPUT.11
      CHARACTER*80 SAVENAME                                                      OUTPUT.12
      LOGICAL RESTRTS_EXISTS                                                     19DEC02.521
#endif                                                                           OUTPUT.13
C                                                                                OUTPUT.14
C----------------------------------------------------------------------          OUTPUT.15
C-----OUTPUT FOR POST-PROCESSOR ANALYSES:                                        OUTPUT.16
C                                                                                OUTPUT.17
      XTIMC=XTIME                                                                OUTPUT.18
      IF((KTAU.LT.NINT(TAPTIM/DTMIN)).OR.(IFTAPE.NE.1))GOTO 30                   07NOV00.364
      DO 13 LLN=1,MAXNES                                                         OUTPUT.20
        IF(IACTIV(LLN).EQ.1.AND.MOD(NINT(XTIME/TAPFRQ),INCTAP(LLN)).EQ.0         OUTPUT.21
     +    )THEN                                                                  OUTPUT.22
          CALL ADDRX1C(IAXALL(1,LLN))                                            OUTPUT.23
C     RESET FINE MESH XTIME TO COARSE (CORRECTS TRUNCATION DRIFT)                OUTPUT.24
          XTIME=XTIMC                                                            OUTPUT.25
          IUTOUT=(LLN-1)+41                                                      OUTPUT.26
          CALL OUTTAP(IUTOUT,LLN,IL,JL,MKX,0)                                    OUTPUT.27
          PRINT 10, XTIMC,LLN                                                    07NOV00.365
   10 FORMAT(6X,'--- MODEL OUTPUT IS WRITTEN AT TIME = ',F10.2,                  OUTPUT.29
     *    ' MINUTES FOR DOMAIN ',I2)                                             OUTPUT.30
        ENDIF                                                                    OUTPUT.31
   13 CONTINUE                                                                   OUTPUT.32
      IF(KTAU.NE.0)TAPTIM=TAPTIM+TAPFRQ                                          OUTPUT.33
      NTTAPE=NTTAPE+1                                                            OUTPUT.34
   30 CONTINUE                                                                   OUTPUT.35
      CALL ADDRX1C(IAXALL(1,NESCOUO))                                            OUTPUT.36
C                                                                                OUTPUT.37
C-----OUTPUT FOR RESTART:                                                        OUTPUT.38
C                                                                                OUTPUT.39
#ifndef MPP1                                                                     OUTPUT.40
      IF((.NOT.IFSAVE).OR.(KTAU.LT.NINT(SAVTIM/DTMIN)))GOTO 80                   07NOV00.366
      DO 66 LLN=1,MAXNES                                                         OUTPUT.42
        IF(IACTIV(LLN).EQ.1)THEN                                                 OUTPUT.43
C     RESET FINE MESH XTIME TO COARSE (CORRECTS TRUNCATION DRIFT)                OUTPUT.44
          XTIME=XTIMC                                                            OUTPUT.45
          IUTSAV=51+(LLN-1)                                                      OUTPUT.46
          CALL OUTSAV(IUTSAV,ALLARR(1,LLN),IRHUGE,INTALL(1,LLN),IIHUGE,          OUTPUT.47
     +         ALLFG(1,LLN),IFGHUG,INTFG(1,LLN),IFGIHUG,ALLFO(1,LLN),            OUTPUT.48
     +         IFOHUG,MAXNES,MKX,MIX,MJX)                                        OUTPUT.49
          IF(SVLAST)THEN                                                         OUTPUT.50
            CLOSE  (IUTSAV)                                                      23DEC04.229
            PRINT 50, LLN                                                        OUTPUT.52
   50       FORMAT(6X,'+++ CLOSING SAVE FILE FOR DOMAIN ',I2)                    23DEC04.230
          ENDIF                                                                  OUTPUT.54
          PRINT 70, XTIMC,LLN,NINT(XTIME)                                        07NOV00.367
   70     FORMAT(6X,'+++ RESTART FILE IS WRITTEN AT TIME = ',F10.2,              25JAN00.108
     1       ' MINUTES FOR DOMAIN ',I2,'. IXTIMR = ',I6)                         05DEC01.209
        ENDIF                                                                    25JAN00.109
   66 CONTINUE                                                                   OUTPUT.59
      SAVTIM=SAVTIM+SAVFRQ                                                       OUTPUT.60
      NTSAVE=NTSAVE+1                                                            OUTPUT.61
   80 CONTINUE                                                                   OUTPUT.62
#else                                                                            OUTPUT.63
# include <mpp_output_10.incl>                                                   OUTPUT.64
#endif                                                                           OUTPUT.65
  150 CONTINUE                                                                   OUTPUT.66
C                                                                                OUTPUT.67
      CALL ADDRX1C(IAXALL(1,1))                                                  OUTPUT.68
      IF(XTIME.LT.PRTTIM)GOTO 250                                                OUTPUT.69
      IF(IFPRT.EQ.0)GOTO 250                                                     OUTPUT.70
#ifndef MPP1                                                                     OUTPUT.71
      DO 240 LLN=1,MAXNES                                                        OUTPUT.72
        IF(IACTIV(LLN).EQ.1)THEN                                                 OUTPUT.73
          CALL ADDRX1C(IAXALL(1,LLN))                                            OUTPUT.74
          CALL OUTPRT(IEXEC(LLN),LLN)                                            OUTPUT.75
        ENDIF                                                                    OUTPUT.76
  240 CONTINUE                                                                   OUTPUT.77
      PRTTIM=PRTTIM+PRTFRQ                                                       OUTPUT.78
      CALL ADDRX1C(IAXALL(1,NESCOUO))                                            OUTPUT.79
#else                                                                            OUTPUT.80
C OUTPRT DEFFERRED IN MPP VERSION                                                OUTPUT.81
#endif                                                                           OUTPUT.82
  250 CONTINUE                                                                   OUTPUT.83
C                                                                                OUTPUT.84
C----- SHUT DOWN FDDA                                                            OUTPUT.85
C                                                                                OUTPUT.86
#if defined(FDDAGD1) || defined(FDDAOBS1)
      IF(IFDDAG.EQ.1.OR.IFDDAO.EQ.1)THEN                                         OUTPUT.88
        DO 275 LLN=1,MAXNES                                                      OUTPUT.89
          IF(XTIME.GE.FDAEND(LLN))THEN                                           OUTPUT.90
            IF(I4D(LLN,1)+I4D(LLN,2)+I4DI(LLN).GT.0)THEN                         OUTPUT.91
              PRINT *,'XTIME , FDDAEND = ',XTIME,FDAEND(LLN)                     OUTPUT.92
              CALL FDAOFF(LLN)                                                   OUTPUT.93
            ENDIF                                                                OUTPUT.94
          ENDIF                                                                  OUTPUT.95
  275   CONTINUE                                                                 OUTPUT.96
      ENDIF                                                                      OUTPUT.97
#endif                                                                           OUTPUT.98
C...KF2 mods...zero out KF diagnostics every FRQKFDIAG minutes...                05DEC01.210
      IF(KTAU.GT.NINT(TIMKFDIAG/DTMIN))THEN                                      05DEC01.211
c     IF(XTIME.GT.TIMKFDIAG)THEN                                                 05DEC01.212
        DO 774 J = 1,MJXC                                                        19DEC02.522
        DO 774 I = 1,MIXC                                                        19DEC02.523
          TNCA(I,J) = 0.                                                         05DEC01.215
          SPSRC(I,J) = 0.                                                        05DEC01.216
          SPCLB(I,J) = 0.                                                        05DEC01.217
          SUMFB(I,J) = 0.                                                        05DEC01.218
          if(nca(i,j).le.0)CIN(I,J) = 0.                                         05DEC01.219
 774    CONTINUE                                                                 05DEC01.220
        TIMKFDIAG=TIMKFDIAG+FRQKFDIAG                                            05DEC01.221
C       print *,'KTAU, XTIME, TIMKFDIAG =',KTAU, XTIME,TIMKFDIAG                 05DEC01.222
      ENDIF                                                                      05DEC01.223
C...end of KF2 mods....                                                          05DEC01.224
C                                                                                OUTPUT.99
      RETURN                                                                     OUTPUT.100
      END                                                                        OUTPUT.101
                                                                                 OUTPUT.102
