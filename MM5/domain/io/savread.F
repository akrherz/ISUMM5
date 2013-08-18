      SUBROUTINE SAVREAD(ALLARR,IRHUGE,INTALL,IIHUGE,ALLFG,IFGHUG,               SAVREAD.1
     * INTFG,IFGIHUG,ALLFO,IFOHUG,IUTSAV)                                        07NOV00.802
                                                                                 SAVREAD.4
      DIMENSION ALLARR(IRHUGE),INTALL(IIHUGE),ALLFG(IFGHUG),                     SAVREAD.5
     +          INTFG(IFGIHUG),ALLFO(IFOHUG)                                     SAVREAD.6
                                                                                 SAVREAD.7
      LOGICAL OPENED                                                             SAVREAD.8
                                                                                 SAVREAD.9
      INEST = IUTSAV-90                                                          SAVREAD.10
      INQUIRE(IUTSAV, OPENED=OPENED)                                             SAVREAD.11
      IF (.NOT. OPENED) THEN                                                     SAVREAD.12
          OPEN(IUTSAV, FILE='RESTART_DOMAIN'//CHAR(INEST+ICHAR('0')),            SAVREAD.13
     &         FORM='UNFORMATTED',STATUS='OLD',ERR=997)                          25JAN00.148
      ENDIF                                                                      SAVREAD.15
                                                                                 SAVREAD.16
      READ (IUTSAV,ERR=998,END=999)ALLARR                                        SAVREAD.17
      READ (IUTSAV)INTALL                                                        SAVREAD.18
      IF(IFOHUG.GT.100)THEN                                                      SAVREAD.19
        READ (IUTSAV)ALLFO                                                       SAVREAD.20
      ELSE                                                                       SAVREAD.21
        READ (IUTSAV)                                                            SAVREAD.22
      ENDIF                                                                      SAVREAD.23
      IF(IFGHUG.GT.100)THEN                                                      SAVREAD.24
        READ (IUTSAV)ALLFG                                                       SAVREAD.25
      ELSE                                                                       SAVREAD.26
        READ (IUTSAV)                                                            SAVREAD.27
      ENDIF                                                                      SAVREAD.28
      IF(IFGIHUG.GT.100)THEN                                                     SAVREAD.29
        READ (IUTSAV)INTFG                                                       SAVREAD.30
      ELSE                                                                       SAVREAD.31
        READ (IUTSAV)                                                            SAVREAD.32
      ENDIF                                                                      SAVREAD.33
      RETURN                                                                     SAVREAD.34
  999 CONTINUE                                                                   SAVREAD.35
      PRINT *,'END OF RESTART FILE, UNIT ',IUTSAV,                               SAVREAD.36
     *      ' CHECK IF RESTART FILE EXISTS'                                      SAVREAD.37
      STOP 'STOP 999 IN SAVREAD'                                                 SAVREAD.38
  998 PRINT *,'ERROR IN READING RESTART FILE. CHECK RESTART FILE,',              SAVREAD.39
     *      ' UNIT ',IUTSAV                                                      SAVREAD.40
      STOP 'STOP 998 IN SAVREAD'                                                 SAVREAD.41
  997 PRINT *,'ERROR OPENING READING RESTART FILE. CHECK RESTART FILE,',         25JAN00.149
     *      ' UNIT ',IUTSAV                                                      25JAN00.150
      STOP 'STOP 997 IN SAVREAD'                                                 25JAN00.151
      END                                                                        SAVREAD.42
