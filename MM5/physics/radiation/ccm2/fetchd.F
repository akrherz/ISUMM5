CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC           FETCHD.1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC           FETCHD.2
      SUBROUTINE FETCHD(NUNIT,A,LENGTH)                                          FETCHD.3
      DIMENSION A(1)                                                             FETCHD.4
      LOGICAL PRNT                                                               FETCHD.5
      DATA PRNT /.FALSE./                                                        FETCHD.6
                                                                                 FETCHD.7
CSB     BUFFER IN (NUNIT,1) (A(1),A(LENGTH))                                     FETCHD.8
      READ (NUNIT)(A(I),I=1,LENGTH)                                              FETCHD.9
CSB      IF (UNIT(NUNIT)) 10,12,13                                               FETCHD.10
   10 IF(.NOT.PRNT)RETURN                                                        FETCHD.11
      PRINT100,LENGTH,NUNIT                                                      FETCHD.12
100   FORMAT(/' DATA RECORD LENGTH    = ',I5,                                    FETCHD.13
     1        ' BUFFERED IN TO UNIT  = ',I5//)                                   FETCHD.14
      RETURN                                                                     FETCHD.15
   12 PRINT200,NUNIT                                                             FETCHD.16
200   FORMAT (//' ERROR ON DATA READ TO UNIT = ',I5/)                            FETCHD.17
      STOP                                                                       FETCHD.18
   13 PRINT300,NUNIT                                                             FETCHD.19
300   FORMAT(//' END OF FILE ON DATA READ FROM UNIT = ',I5//)                    FETCHD.20
      STOP                                                                       FETCHD.21
      END                                                                        FETCHD.22
                                                                                 FETCHD.23
