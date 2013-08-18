CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC           STORED.1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC           STORED.2
      SUBROUTINE STORED(NUNIT,A,LENGTH)                                          STORED.3
      DIMENSION A(1)                                                             STORED.4
      LOGICAL PRNT                                                               STORED.5
      DATA PRNT /.FALSE./                                                        STORED.6
                                                                                 STORED.7
CSB     BUFFER OUT (NUNIT,1) (A(1),A(LENGTH))                                    STORED.8
      WRITE (NUNIT)(A(I),I=1,LENGTH)                                             STORED.9
CSB      IF (UNIT(NUNIT)) 10,12,13                                               STORED.10
   10 IF(.NOT.PRNT)RETURN                                                        STORED.11
      PRINT100,LENGTH,NUNIT                                                      STORED.12
100   FORMAT(/' DATA RECORD LENGTH    = ',I5,                                    STORED.13
     1        ' BUFFERED OUT ON UNIT  = ',I5//)                                  STORED.14
      RETURN                                                                     STORED.15
   12 PRINT200,NUNIT                                                             STORED.16
200   FORMAT (//' ERROR ON DATA WRITE TO UNIT = ',I5/)                           STORED.17
      STOP                                                                       STORED.18
   13 PRINT300,NUNIT                                                             STORED.19
300   FORMAT(//' END OF FILE ON DATA WRITE TO UNIT = ',I5//)                     STORED.20
      STOP                                                                       STORED.21
      END                                                                        STORED.22
                                                                                 STORED.23
