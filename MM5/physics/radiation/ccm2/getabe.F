C                                                                                GETABE.1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC              GETABE.2
      SUBROUTINE GETABE(JSLC,JMAX,NLOND,NLEV,NLEVP,ABSTOT,                       GETABE.3
     1                  ABSNXT,EMSTOT)                                           GETABE.4
C     COMMON /RADFRQ/ IRADAE                                                     GETABE.5
      DIMENSION ABSTOT(NLOND,NLEVP,NLEVP),ABSNXT(NLOND,NLEV,4)                   GETABE.6
     1         ,EMSTOT(NLOND,NLEVP)                                              GETABE.7
                                                                                 GETABE.8
      DATA NABEM1,NABEM2 /97,98/                                                 GETABE.9
                                                                                 GETABE.10
      LENGTH=NLOND*NLEVP*NLEVP                                                   GETABE.11
      CALL FETCHD(NABEM2,ABSTOT,LENGTH)                                          GETABE.12
      LENGTH=NLOND*NLEV*4                                                        GETABE.13
      CALL FETCHD(NABEM2,ABSNXT,LENGTH)                                          GETABE.14
      LENGTH=NLOND*NLEVP                                                         GETABE.15
      CALL FETCHD(NABEM2,EMSTOT,LENGTH)                                          GETABE.16
      IF(JSLC.EQ.JMAX)THEN                                                       GETABE.17
        REWIND NABEM2                                                            GETABE.18
       PRINT *,'ABS-EMS RETRIEVED'                                               GETABE.19
      ENDIF                                                                      GETABE.20
      IF(JSLC.EQ.18.OR.JSLC.EQ.35)THEN                                           GETABE.21
       PRINT *,'RETR. ABSTOT,ABSNXT,EMSTOT = ', JSLC, ABSTOT(10,12,8),           GETABE.22
     1   ABSNXT(10,12,2),EMSTOT(10,12), ABSTOT(42,5,15),ABSNXT(42,5,4),          GETABE.23
     1   EMSTOT(42,5)                                                            GETABE.24
      ENDIF                                                                      GETABE.25
      RETURN                                                                     GETABE.26
      END                                                                        GETABE.27
                                                                                 GETABE.28
