      SUBROUTINE SKIPF(IUNIT,ISKIP,IERR)                                         SKIPF.1
      DIMENSION IERR(2)                                                          SKIPF.2
C     RETURN                                                                     SKIPF.3
      IF(ISKIP.LE.0)RETURN                                                       SKIPF.4
      ISOFAR=0                                                                   SKIPF.5
   10 CONTINUE                                                                   SKIPF.6
      READ (IUNIT,END=100)                                                       SKIPF.7
      GOTO 10                                                                    SKIPF.8
  100 CONTINUE                                                                   SKIPF.9
      ISOFAR=ISOFAR+1                                                            SKIPF.10
      IF(ISOFAR.LT.ISKIP)GOTO 10                                                 SKIPF.11
      RETURN                                                                     SKIPF.12
      END                                                                        SKIPF.13
                                                                                 SKIPF.14
