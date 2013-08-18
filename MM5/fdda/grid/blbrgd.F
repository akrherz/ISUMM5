      SUBROUTINE BLBRGD(NV,MVAR,MTIM,XTIME,SFCTIM,TIMB,TIME,                     BLBRGD.1
     1NTB,NTE,IQCHK,IN,KTAU,KTAUR,IFREST)                                        BLBRGD.2
C THIS SUBROUTINE DETERMINES THE VALUES OF TIMB, NTB AND TIME, NTE               BLBRGD.3
C WHICH "BRACKET" XTIME WITH TWO QUALITY (IQCHK=1) SURFACE ANALYSES.             BLBRGD.4
C THIS SUBROUTINE DETERMINES TIMB, TIME, NTB, NTE FOR THIS VARIABLE              BLBRGD.5
C AND AND IS CALLED ONLY ONCE (AT J=2) IN THE FIRST TIME STEP IN WHICH           BLBRGD.6
C XTIME EXCEEDS THE PREVIOUS TIME FOR THAT VARIABLE NV. THE SUBROUTINE           BLBRGD.7
C THEN REDEFINES TIME(NV), AND WHEN XTIME EXCEEDS THIS NEW TIME, THE             BLBRGD.8
C SUBROUTINE IS CALLED AGAIN, AND SO ON.                                         BLBRGD.9
C IF XTIME WAS BRACKETED BY TWO ANALYSES IN WHICH EITHER ONE OR BOTH             BLBRGD.10
C HAD IQCHK=0, THEN -999. IS RETURNED IN TIMB AND TIME.                          BLBRGD.11
      DIMENSION SFCTIM(MTIM),NTB(MVAR),TIMB(MVAR),NTE(MVAR),TIME(MVAR),          BLBRGD.12
     1          IQCHK(MTIM,MVAR)                                                 BLBRGD.13
      LOGICAL IFREST                                                             BLBRGD.14
      ICUT=0                                                                     BLBRGD.15
      IF(IN.GT.1)ICUT=1                                                          BLBRGD.16
      PRINT1,XTIME,NV,IN                                                         BLBRGD.17
1     FORMAT(1X,'IN BLBRGD: XTIME = ',F7.2,', NV = ',I2,', AND INEST = '         BLBRGD.18
     1I2)                                                                        BLBRGD.19
      NTMAX=0                                                                    BLBRGD.20
      DO 10 NT=1,MTIM                                                            BLBRGD.21
        IF(SFCTIM(NT).LT.-900.)GOTO 15                                           BLBRGD.22
        NTMAX=NT                                                                 BLBRGD.23
   10 CONTINUE                                                                   BLBRGD.24
   15 CONTINUE                                                                   BLBRGD.25
      IF(NTMAX.EQ.0)STOP 15                                                      BLBRGD.26
C SET THE LAST NTE(NV) TO THE CURRENT NTB(NV) AND THEN CHECK                     BLBRGD.27
C IF IT QUALIFIES TO BE THE NEW NTB(NV)...                                       BLBRGD.28
      NTBE=NTE(NV)                                                               BLBRGD.29
      TIMB(NV)=-999.                                                             BLBRGD.30
      TIME(NV)=-999.                                                             BLBRGD.31
      NTB(NV)=-999                                                               BLBRGD.32
      NTE(NV)=-999                                                               BLBRGD.33
      IBR=0                                                                      BLBRGD.34
      DO 100 NT=NTBE,NTMAX-1                                                     BLBRGD.35
        IF(XTIME.GE.SFCTIM(NT).AND.XTIME.LT.SFCTIM(NT+1))THEN                    BLBRGD.36
C XTIME IS BRACKETED BT TWO DATA TIMES                                           BLBRGD.37
          IBR=1                                                                  BLBRGD.38
C CHECK THE DATA QUALITY AT BOTH TIMES                                           BLBRGD.39
          IF(IQCHK(NT,NV).EQ.1)THEN                                              BLBRGD.40
            NTB(NV)=NT                                                           BLBRGD.41
C IF THE NEW TIMB IS NOT THE OLD TIME, NUDGING IS DISCONTINUOUS SO STOP          BLBRGD.42
C...UNLESS KTAU=KTAUR AND THIS IS A RESTART RUN                                  BLBRGD.43
            IF(IFREST.AND.KTAU.EQ.KTAUR)GOTO 99                                  BLBRGD.44
            IF(NTB(NV).NE.NTBE)STOP 100                                          BLBRGD.45
   99       CONTINUE                                                             BLBRGD.46
            TIMB(NV)=SFCTIM(NT)                                                  BLBRGD.47
C SINCE TIMB(NV) IS DEFINED, FIND TIME(NV)                                       BLBRGD.48
            IF(IQCHK(NT+1,NV).EQ.1)THEN                                          BLBRGD.49
              NTE(NV)=NT+1                                                       BLBRGD.50
              TIME(NV)=SFCTIM(NT+1)                                              BLBRGD.51
            ELSE                                                                 BLBRGD.52
C FIND THE NEXT DATA TIME TO BRACKET XTIME                                       BLBRGD.53
              IF((NT+2).GT.NTMAX)RETURN                                          BLBRGD.54
              DO 150 NTN=NT+2,NTMAX                                              BLBRGD.55
                IF(IQCHK(NTN,NV).EQ.1)THEN                                       BLBRGD.56
                  NTE(NV)=NTN                                                    BLBRGD.57
                  TIME(NV)=SFCTIM(NTN)                                           BLBRGD.58
                  RETURN                                                         BLBRGD.59
                ENDIF                                                            BLBRGD.60
  150         CONTINUE                                                           BLBRGD.61
            ENDIF                                                                BLBRGD.62
          ENDIF                                                                  BLBRGD.63
        ENDIF                                                                    BLBRGD.64
  100 CONTINUE                                                                   BLBRGD.65
C IF XTIME IS NOT BRACKETED BY TWO DATA TIMES, SOMETHING IS WRONG!               BLBRGD.66
      IF(IBR.EQ.0)THEN                                                           BLBRGD.67
        PRINT110,XTIME,NV                                                        BLBRGD.68
110   FORMAT(1X,'---STOPPING EXECUTION IN BLBRGD AT XTIME = ',F7.2,              BLBRGD.69
     1' AND NV = ',                                                              BLBRGD.70
     1I2,' : XTIME NOT BRACKETED BY THE DATA')                                   BLBRGD.71
C     CALL ABORT                                                                 BLBRGD.72
C     PRINT *,SFCTIM                                                             BLBRGD.73
C     PRINT *,IQCHK                                                              BLBRGD.74
C     PRINT *,NTB,NTE                                                            BLBRGD.75
C     PRINT *,NT,NTMAX,NTBE                                                      BLBRGD.76
C     PRINT *,TIME,TIMB                                                          BLBRGD.77
        STOP 100                                                                 BLBRGD.78
      ENDIF                                                                      BLBRGD.79
C IF EXECUTION REACHES THIS POINT, XTIME WAS NOT BRACKETED BY TWO QUALIT         BLBRGD.80
C ANALYSES, AND -999. IS RETURNED IN TIMB, TIME, AND -999 IN NTB AND NTE         BLBRGD.81
      RETURN                                                                     BLBRGD.82
      END                                                                        BLBRGD.83
                                                                                 BLBRGD.84
