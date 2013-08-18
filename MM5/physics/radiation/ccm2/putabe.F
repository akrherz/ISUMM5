C-----------------------------------------------------------------------         PUTABE.1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC             PUTABE.2
      SUBROUTINE PUTABE(JSLC,JMAX,NLOND,NLEV,NLEVP,ABSTOT,                       PUTABE.3
     1                  ABSNXT,EMSTOT)                                           PUTABE.4
C     COMMON /RADFRQ/ IRADAE                                                     PUTABE.5
      DIMENSION ABSTOT(NLOND,NLEVP,NLEVP),ABSNXT(NLOND,NLEV,4)                   PUTABE.6
     1         ,EMSTOT(NLOND,NLEVP)                                              PUTABE.7
                                                                                 PUTABE.8
      DATA NABEM1,NABEM2 /97,98/                                                 PUTABE.9
                                                                                 PUTABE.10
      LENGTH=NLOND*NLEVP*NLEVP                                                   PUTABE.11
      CALL STORED(NABEM2,ABSTOT,LENGTH)                                          PUTABE.12
      LENGTH=NLOND*NLEV*4                                                        PUTABE.13
      CALL STORED(NABEM2,ABSNXT,LENGTH)                                          PUTABE.14
      LENGTH=NLOND*NLEVP                                                         PUTABE.15
      CALL STORED(NABEM2,EMSTOT,LENGTH)                                          PUTABE.16
      IF(JSLC.EQ.JMAX)THEN                                                       PUTABE.17
        REWIND NABEM2                                                            PUTABE.18
      ENDIF                                                                      PUTABE.19
C      PRINT *,'ABS-EMS STORAGE COMPLETED, IRADAE =',IRADAE                      PUTABE.20
C     IF (JSLC.EQ.18 .OR. JSLC.EQ.35) THEN                                       PUTABE.21
C      PRINT *,'STORE ABSTOT,ABSNXT,EMSTOT = ', JSLC, ABSTOT(10,12,8),           PUTABE.22
C    1   ABSNXT(10,12,2),EMSTOT(10,12), ABSTOT(42,5,15),ABSNXT(42,5,4),          PUTABE.23
C    1   EMSTOT(42,5)                                                            PUTABE.24
C     ENDIF                                                                      PUTABE.25
      RETURN                                                                     PUTABE.26
      END                                                                        PUTABE.27
                                                                                 PUTABE.28
