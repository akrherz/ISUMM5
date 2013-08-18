      SUBROUTINE OUTSAV(IUTSAV,ALLARR,IRHUGE,INTALL,IIHUGE,                      OUTSAV.1
     * ALLFG,IFGHUG,INTFG,IFGIHUG,ALLFO,IFOHUG,MAXNES,                           OUTSAV.2
     * MKX,MIX,MJX)                                                              OUTSAV.3
                                                                                 OUTSAV.4
      DIMENSION ALLARR(IRHUGE),INTALL(IIHUGE),ALLFG(IFGHUG),                     OUTSAV.5
     +          INTFG(IFGIHUG),ALLFO(IFOHUG)                                     OUTSAV.6
      LOGICAL OPENED                                                             OUTSAV.7
      LOGICAL DM_IONODE                                                          OUTSAV.8
      EXTERNAL DM_IONODE                                                         OUTSAV.9
C                                                                                OUTSAV.10
C----- OPEN NEW UNIT NUMBER IF NECESSARY                                         OUTSAV.11
C                                                                                OUTSAV.12
      IN = IUTSAV-50                                                             OUTSAV.13
      IF (IUTSAV.GT.60) IN = IUTSAV-60                                           OUTSAV.14
      if (dm_ionode()) then                                                      OUTSAV.15
        INQUIRE(IUTSAV, OPENED=OPENED)                                           OUTSAV.16
        IF (.NOT. OPENED) THEN                                                   OUTSAV.17
          IF (IUTSAV.LT.60) THEN                                                 OUTSAV.18
            OPEN(IUTSAV, FILE='SAVE_DOMAIN'//CHAR(IN+ICHAR('0')),                OUTSAV.19
     &           FORM='UNFORMATTED',STATUS='UNKNOWN')                            OUTSAV.20
          ELSE                                                                   OUTSAV.21
            OPEN(IUTSAV, FILE='SHUTDO_DOMAIN'//CHAR(IN+ICHAR('0')),              OUTSAV.22
     &           FORM='UNFORMATTED',STATUS='UNKNOWN')                            OUTSAV.23
          END IF                                                                 OUTSAV.24
        END IF                                                                   OUTSAV.25
      END IF                                                                     OUTSAV.26
                                                                                 OUTSAV.27
      WRITE (IUTSAV)ALLARR                                                       OUTSAV.28
      WRITE (IUTSAV)INTALL                                                       OUTSAV.29
      WRITE (IUTSAV)ALLFO                                                        OUTSAV.30
      WRITE (IUTSAV)ALLFG                                                        OUTSAV.31
      WRITE (IUTSAV)INTFG                                                        OUTSAV.32
      RETURN                                                                     OUTSAV.33
      END                                                                        OUTSAV.34
                                                                                 OUTSAV.35
