      SUBROUTINE GETH_IDTS(NDATE,ODATE,IDT)                                      DATE.1
C  This program is modified based on KWM's f90 code                              DATE.2
C  1.  geth_idts (ndate, odate, idts)                                            DATE.3
C  Get the time period between two dates.                                        DATE.4
      IMPLICIT NONE                                                              DATE.5
C  From 2 input mdates ('YYYY-MM-DD HH:MM:SS.ffff'),                             DATE.6
C  compute the time difference.                                                  DATE.7
C  on entry     -  ndate  -  the new hdate.                                      DATE.8
C                  odate  -  the old hdate.                                      DATE.9
C  on exit      -  idt    -  the change in time.                                 DATE.10
C                            Units depend on length of date strings.             DATE.11
      CHARACTER *(*)NDATE,ODATE                                                  DATE.12
      INTEGER IDT                                                                DATE.13
C  Local Variables                                                               DATE.14
C  yrnew    -  indicates the year associated with "ndate"                        DATE.15
C  yrold    -  indicates the year associated with "odate"                        DATE.16
C  monew    -  indicates the month associated with "ndate"                       DATE.17
C  moold    -  indicates the month associated with "odate"                       DATE.18
C  dynew    -  indicates the day associated with "ndate"                         DATE.19
C  dyold    -  indicates the day associated with "odate"                         DATE.20
C  hrnew    -  indicates the hour associated with "ndate"                        DATE.21
C  hrold    -  indicates the hour associated with "odate"                        DATE.22
C  minew    -  indicates the minute associated with "ndate"                      DATE.23
C  miold    -  indicates the minute associated with "odate"                      DATE.24
C  scnew    -  indicates the second associated with "ndate"                      DATE.25
C  scold    -  indicates the second associated with "odate"                      DATE.26
C  i        -  loop counter                                                      DATE.27
C  mday     -  a list assigning the number of days in each month                 DATE.28
C  newhrs   -  the number of hours between "ndate" and 1901                      DATE.29
C              whole 24 hour days                                                DATE.30
C  oldhrs   -  the number of hours between "odate" and 1901                      DATE.31
      CHARACTER *24TDATE                                                         DATE.32
      INTEGER LEN,OLEN,NLEN                                                      DATE.33
      INTEGER YRNEW,MONEW,DYNEW,HRNEW,MINEW,SCNEW,FRNEW                          DATE.34
      INTEGER YROLD,MOOLD,DYOLD,HROLD,MIOLD,SCOLD,FROLD                          DATE.35
      INTEGER MDAY(12),I,NEWDYS,OLDDYS                                           DATE.36
      LOGICAL NPASS,OPASS                                                        DATE.37
      INTEGER ISIGN                                                              DATE.38
      INTEGER IFRC                                                               DATE.39
      INTEGER NFEB                                                               DATE.40
      OLEN=LEN(ODATE)                                                            DATE.41
      NLEN=LEN(NDATE)                                                            DATE.42
      IF(NLEN.NE.OLEN)THEN                                                       DATE.43
         write(*,'("GETH_IDTS: NLEN /= OLEN: ", A, 3x, A)')                      DATE.44
     &         ndate(1:nlen), odate(1:olen)                                      DATE.45
        STOP 'char_len1'                                                         DATE.46
      ENDIF                                                                      DATE.47
      IF(ODATE.GT.NDATE)THEN                                                     DATE.48
        ISIGN=-1                                                                 DATE.49
        TDATE=NDATE                                                              DATE.50
        NDATE=ODATE                                                              DATE.51
        ODATE=TDATE                                                              DATE.52
      ELSE                                                                       DATE.53
        ISIGN=1                                                                  DATE.54
      ENDIF                                                                      DATE.55
C  Assign the number of days in a months                                         DATE.56
      MDAY(1)=31                                                                 DATE.57
      MDAY(2)=28                                                                 DATE.58
      MDAY(3)=31                                                                 DATE.59
      MDAY(4)=30                                                                 DATE.60
      MDAY(5)=31                                                                 DATE.61
      MDAY(6)=30                                                                 DATE.62
      MDAY(7)=31                                                                 DATE.63
      MDAY(8)=31                                                                 DATE.64
      MDAY(9)=30                                                                 DATE.65
      MDAY(10)=31                                                                DATE.66
      MDAY(11)=30                                                                DATE.67
      MDAY(12)=31                                                                DATE.68
C  Break down old hdate into parts                                               DATE.69
      HROLD=0                                                                    DATE.70
      MIOLD=0                                                                    DATE.71
      SCOLD=0                                                                    DATE.72
      FROLD=0                                                                    DATE.73
      READ (ODATE(1:4),'(i4)')YROLD                                              DATE.74
      READ (ODATE(6:7),'(i2)')MOOLD                                              DATE.75
      READ (ODATE(9:10),'(i2)')DYOLD                                             DATE.76
      IF(OLEN.GE.13)THEN                                                         DATE.77
        READ (ODATE(12:13),'(i2)')HROLD                                          DATE.78
        IF(OLEN.GE.16)THEN                                                       DATE.79
          READ (ODATE(15:16),'(i2)')MIOLD                                        DATE.80
          IF(OLEN.GE.19)THEN                                                     DATE.81
            READ (ODATE(18:19),'(i2)')SCOLD                                      DATE.82
            IF(OLEN.GT.20)THEN                                                   DATE.83
              IF(OLEN.EQ.21)THEN                                                 DATE.84
                READ (ODATE(21:21),'(i1)')FROLD                                  DATE.85
              ELSEIF(OLEN.EQ.22)THEN                                             DATE.86
                READ (ODATE(21:22),'(i2)')FROLD                                  DATE.87
              ELSEIF(OLEN.EQ.23)THEN                                             DATE.88
                READ (ODATE(21:32),'(i3)')FROLD                                  DATE.89
              ELSEIF(OLEN.EQ.24)THEN                                             DATE.90
                READ (ODATE(21:24),'(i4)')FROLD                                  DATE.91
              ENDIF                                                              DATE.92
            ENDIF                                                                DATE.93
          ENDIF                                                                  DATE.94
        ENDIF                                                                    DATE.95
      ENDIF                                                                      DATE.96
C  Break down new hdate into parts                                               DATE.97
      HRNEW=0                                                                    DATE.98
      MINEW=0                                                                    DATE.99
      SCNEW=0                                                                    DATE.100
      FRNEW=0                                                                    DATE.101
      READ (NDATE(1:4),'(i4)')YRNEW                                              DATE.102
      READ (NDATE(6:7),'(i2)')MONEW                                              DATE.103
      READ (NDATE(9:10),'(i2)')DYNEW                                             DATE.104
      IF(NLEN.GE.13)THEN                                                         DATE.105
        READ (NDATE(12:13),'(i2)')HRNEW                                          DATE.106
        IF(NLEN.GE.16)THEN                                                       DATE.107
          READ (NDATE(15:16),'(i2)')MINEW                                        DATE.108
          IF(NLEN.GE.19)THEN                                                     DATE.109
            READ (NDATE(18:19),'(i2)')SCNEW                                      DATE.110
            IF(NLEN.GT.20)THEN                                                   DATE.111
              READ (NDATE(21:NLEN),*)FRNEW                                       DATE.112
            ENDIF                                                                DATE.113
          ENDIF                                                                  DATE.114
        ENDIF                                                                    DATE.115
      ENDIF                                                                      DATE.116
C  Check that the dates make sense.                                              DATE.117
      NPASS=.TRUE.                                                               DATE.118
      OPASS=.TRUE.                                                               DATE.119
C  Check that the month of NDATE makes sense.                                    DATE.120
      IF((MONEW.GT.12).OR.(MONEW.LT.1))THEN                                      DATE.121
         print*, 'GETH_IDTS:  Month of NDATE = ', monew                          DATE.122
        NPASS=.FALSE.                                                            DATE.123
      ENDIF                                                                      DATE.124
C  Check that the month of ODATE makes sense.                                    DATE.125
      IF((MOOLD.GT.12).OR.(MOOLD.LT.1))THEN                                      DATE.126
         print*, 'GETH_IDTS:  Month of ODATE = ', moold                          DATE.127
        OPASS=.FALSE.                                                            DATE.128
      ENDIF                                                                      DATE.129
C  Check that the day of NDATE makes sense.                                      DATE.130
      IF(MONEW.NE.2)THEN                                                         DATE.131
C ...... For all months but February                                             DATE.132
        IF((DYNEW.GT.MDAY(MONEW)).OR.(DYNEW.LT.1))THEN                           DATE.133
            print*, 'GETH_IDTS:  Day of NDATE = ', dynew                         DATE.134
          NPASS=.FALSE.                                                          DATE.135
        ENDIF                                                                    DATE.136
      ELSEIF(MONEW.EQ.2)THEN                                                     DATE.137
C ...... For February                                                            DATE.138
        IF(MOD(YRNEW,4).EQ.0)THEN                                                DATE.139
! Leap Year                                                                      DATE.140
          IF((DYNEW.GT.29).OR.(DYNEW.LT.1))THEN                                  DATE.141
               print*, 'GETH_IDTS:  Day of NDATE = ', dynew                      DATE.142
            NPASS=.FALSE.                                                        DATE.143
          ENDIF                                                                  DATE.144
        ELSE                                                                     DATE.145
          IF((DYNEW.GT.28).OR.(DYNEW.LT.1))THEN                                  DATE.146
! Not a leap year                                                                DATE.147
               print*, 'GETH_IDTS:  Day of NDATE = ', dynew                      DATE.148
            STOP 'ndate_1'                                                       DATE.149
          ENDIF                                                                  DATE.150
        ENDIF                                                                    DATE.151
      ENDIF                                                                      DATE.152
C  Check that the day of ODATE makes sense.                                      DATE.153
      IF(MOOLD.NE.2)THEN                                                         DATE.154
C ...... For all months but February                                             DATE.155
        IF((DYOLD.GT.MDAY(MOOLD)).OR.(DYOLD.LT.1))THEN                           DATE.156
            print*, 'GETH_IDTS:  Day of ODATE = ', dyold                         DATE.157
          OPASS=.FALSE.                                                          DATE.158
        ENDIF                                                                    DATE.159
      ELSEIF(MOOLD.EQ.2)THEN                                                     DATE.160
C ....... For February                                                           DATE.161
        IF(MOD(YROLD,4).EQ.0)THEN                                                DATE.162
! Leap Year                                                                      DATE.163
#ifdef NOLEAP
          IF((DYOLD.GT.28).OR.(DYOLD.LT.1))THEN                                  DATE.164
#else
          IF((DYOLD.GT.29).OR.(DYOLD.LT.1))THEN                                  DATE.164
#endif
               print*, 'GETH_IDTS:  Day of ODATE = ', dyold                      DATE.165
            OPASS=.FALSE.                                                        DATE.166
          ENDIF                                                                  DATE.167
        ELSE                                                                     DATE.168
          IF((DYOLD.GT.28).OR.(DYOLD.LT.1))THEN                                  DATE.169
! Not a leap year                                                                DATE.170
               print*, 'GETH_IDTS:  Day of ODATE = ', dyold                      DATE.171
            OPASS=.FALSE.                                                        DATE.172
          ENDIF                                                                  DATE.173
        ENDIF                                                                    DATE.174
      ENDIF                                                                      DATE.175
C  Check that the hour of NDATE makes sense.                                     DATE.176
      IF((HRNEW.GT.23).OR.(HRNEW.LT.0))THEN                                      DATE.177
         print*, 'GETH_IDTS:  Hour of NDATE = ', hrnew                           DATE.178
        NPASS=.FALSE.                                                            DATE.179
      ENDIF                                                                      DATE.180
C  Check that the hour of ODATE makes sense.                                     DATE.181
      IF((HROLD.GT.23).OR.(HROLD.LT.0))THEN                                      DATE.182
         print*, 'GETH_IDTS:  Hour of ODATE = ', hrold                           DATE.183
        OPASS=.FALSE.                                                            DATE.184
      ENDIF                                                                      DATE.185
C  Check that the minute of NDATE makes sense.                                   DATE.186
      IF((MINEW.GT.59).OR.(MINEW.LT.0))THEN                                      DATE.187
         print*, 'GETH_IDTS:  Minute of NDATE = ', minew                         DATE.188
        NPASS=.FALSE.                                                            DATE.189
      ENDIF                                                                      DATE.190
C  Check that the minute of ODATE makes sense.                                   DATE.191
      IF((MIOLD.GT.59).OR.(MIOLD.LT.0))THEN                                      DATE.192
         print*, 'GETH_IDTS:  Minute of ODATE = ', miold                         DATE.193
        OPASS=.FALSE.                                                            DATE.194
      ENDIF                                                                      DATE.195
C  Check that the second of NDATE makes sense.                                   DATE.196
      IF((SCNEW.GT.59).OR.(SCNEW.LT.0))THEN                                      DATE.197
         print*, 'GETH_IDTS:  SECOND of NDATE = ', scnew                         DATE.198
        NPASS=.FALSE.                                                            DATE.199
      ENDIF                                                                      DATE.200
C  Check that the second of ODATE makes sense.                                   DATE.201
      IF((SCOLD.GT.59).OR.(SCOLD.LT.0))THEN                                      DATE.202
         print*, 'GETH_IDTS:  Second of ODATE = ', scold                         DATE.203
        OPASS=.FALSE.                                                            DATE.204
      ENDIF                                                                      DATE.205
      IF(.NOT.NPASS)THEN                                                         DATE.206
         print*, 'Screwy NDATE: ', ndate(1:nlen)                                 DATE.207
        STOP 'ndate_2'                                                           DATE.208
      ENDIF                                                                      DATE.209
      IF(.NOT.OPASS)THEN                                                         DATE.210
         print*, 'Screwy ODATE: ', odate(1:olen)                                 DATE.211
        STOP 'odate_1'                                                           DATE.212
      ENDIF                                                                      DATE.213
C  Date Checks are completed.  Continue.                                         DATE.214
C  Compute number of days from 1 January ODATE, 00:00:00 until ndate             DATE.215
C  Compute number of hours from 1 January ODATE, 00:00:00 until ndat             DATE.216
C  Compute number of minutes from 1 January ODATE, 00:00:00 until nd             DATE.217
      NEWDYS=0                                                                   DATE.218
      DO I=YROLD,YRNEW-1                                                         DATE.219
        IF(NFEB(I).EQ.29)THEN                                                    DATE.220
          NEWDYS=NEWDYS+366                                                      DATE.221
        ELSE                                                                     DATE.222
          NEWDYS=NEWDYS+365                                                      DATE.223
        ENDIF                                                                    DATE.224
      ENDDO                                                                      DATE.225
      IF(MONEW.GT.1)THEN                                                         DATE.226
        IF(MOD(YRNEW,4).EQ.0)MDAY(2)=NFEB(YRNEW)                                 DATE.227
        DO I=1,MONEW-1                                                           DATE.228
          NEWDYS=NEWDYS+MDAY(I)                                                  DATE.229
        ENDDO                                                                    DATE.230
        MDAY(2)=28                                                               DATE.231
      ENDIF                                                                      DATE.232
      NEWDYS=NEWDYS+DYNEW-1                                                      DATE.233
C  Compute number of hours from 1 January ODATE, 00:00:00 until odat             DATE.234
C  Compute number of minutes from 1 January ODATE, 00:00:00 until od             DATE.235
      OLDDYS=0                                                                   DATE.236
      IF(MOOLD.GT.1)THEN                                                         DATE.237
        IF(MOD(YROLD,4).EQ.0)MDAY(2)=NFEB(YROLD)                                 DATE.238
        DO I=1,MOOLD-1                                                           DATE.239
          OLDDYS=OLDDYS+MDAY(I)                                                  DATE.240
        ENDDO                                                                    DATE.241
        MDAY(2)=28                                                               DATE.242
      ENDIF                                                                      DATE.243
      OLDDYS=OLDDYS+DYOLD-1                                                      DATE.244
C  Determine the time difference                                                 DATE.245
      IDT=(NEWDYS-OLDDYS)                                                        DATE.246
      IF(OLEN.GT.10)THEN                                                         DATE.247
        IDT=IDT*24+(HRNEW-HROLD)                                                 DATE.248
        IF(OLEN.GT.13)THEN                                                       DATE.249
          IDT=IDT*60+(MINEW-MIOLD)                                               DATE.250
          IF(OLEN.GT.16)THEN                                                     DATE.251
            IDT=IDT*60+(SCNEW-SCOLD)                                             DATE.252
            IF(OLEN.GT.20)THEN                                                   DATE.253
              IFRC=OLEN-20                                                       DATE.254
              IFRC=10**IFRC                                                      DATE.255
              IDT=IDT*IFRC+(FRNEW-FROLD)                                         DATE.256
            ENDIF                                                                DATE.257
          ENDIF                                                                  DATE.258
        ENDIF                                                                    DATE.259
      ENDIF                                                                      DATE.260
      IF(ISIGN.EQ.-1)THEN                                                        DATE.261
        TDATE=NDATE                                                              DATE.262
        NDATE=ODATE                                                              DATE.263
        ODATE=TDATE                                                              DATE.264
        IDT=IDT*ISIGN                                                            DATE.265
      ENDIF                                                                      DATE.266
      RETURN                                                                     DATE.267
      END                                                                        DATE.268
C end subroutine geth_idts                                                       DATE.269
      SUBROUTINE GETH_NEWDATE(NDATE,ODATE,IDT)                                   DATE.270
C  This program is modified based on KWM's f90 code                              DATE.271
C  2. geth_newdate ( ndate, odate, idts)                                         DATE.272
C  Get the new date based on the old date and a time difference.                 DATE.273
      IMPLICIT NONE                                                              DATE.274
C  From old date ('YYYY-MM-DD HH:MM:SS.ffff') and                                DATE.275
C  delta-time, compute the new date.                                             DATE.276
C  on entry     -  odate  -  the old hdate.                                      DATE.277
C                  idt    -  the change in time                                  DATE.278
C  on exit      -  ndate  -  the new hdate.                                      DATE.279
      INTEGER IDT                                                                DATE.280
      CHARACTER *(*)NDATE                                                        DATE.281
      CHARACTER *(*)ODATE                                                        DATE.282
C  Local Variables                                                               DATE.283
C  yrold    -  indicates the year associated with "odate"                        DATE.284
C  moold    -  indicates the month associated with "odate"                       DATE.285
C  dyold    -  indicates the day associated with "odate"                         DATE.286
C  hrold    -  indicates the hour associated with "odate"                        DATE.287
C  miold    -  indicates the minute associated with "odate"                      DATE.288
C  scold    -  indicates the second associated with "odate"                      DATE.289
C  yrnew    -  indicates the year associated with "ndate"                        DATE.290
C  monew    -  indicates the month associated with "ndate"                       DATE.291
C  dynew    -  indicates the day associated with "ndate"                         DATE.292
C  hrnew    -  indicates the hour associated with "ndate"                        DATE.293
C  minew    -  indicates the minute associated with "ndate"                      DATE.294
C  scnew    -  indicates the second associated with "ndate"                      DATE.295
C  mday     -  a list assigning the number of days in each month                 DATE.296
C  i        -  loop counter                                                      DATE.297
C  nday     -  the integer number of days represented by "idt"                   DATE.298
C  nhour    -  the integer number of hours in "idt" after taking out             DATE.299
C              all the whole days                                                DATE.300
C  nmin     -  the integer number of minutes in "idt" after taking o             DATE.301
C              all the whole days and whole hours.                               DATE.302
C  nsec     -  the integer number of minutes in "idt" after taking o             DATE.303
C              all the whole days, whole hours, and whole minutes.               DATE.304
CJM Capitalize LEN to avoid clash with M4 macro of same name                     DATE.305
      INTEGER LEN,NLEN,OLEN                                                      DATE.306
      INTEGER YRNEW,MONEW,DYNEW,HRNEW,MINEW,SCNEW,FRNEW                          DATE.307
      INTEGER YROLD,MOOLD,DYOLD,HROLD,MIOLD,SCOLD,FROLD                          DATE.308
      INTEGER MDAY(12),NDAY,NHOUR,NMIN,NSEC,NFRAC,I,IFRC                         DATE.309
      LOGICAL OPASS                                                              DATE.310
      CHARACTER *10HFRC                                                          DATE.311
      CHARACTER *1SP                                                             DATE.312
      INTEGER NFEB                                                               DATE.313
C  Assign the number of days in a months                                         DATE.314
      MDAY(1)=31                                                                 DATE.315
      MDAY(2)=28                                                                 DATE.316
      MDAY(3)=31                                                                 DATE.317
      MDAY(4)=30                                                                 DATE.318
      MDAY(5)=31                                                                 DATE.319
      MDAY(6)=30                                                                 DATE.320
      MDAY(7)=31                                                                 DATE.321
      MDAY(8)=31                                                                 DATE.322
      MDAY(9)=30                                                                 DATE.323
      MDAY(10)=31                                                                DATE.324
      MDAY(11)=30                                                                DATE.325
      MDAY(12)=31                                                                DATE.326
C  Break down old hdate into parts                                               DATE.327
      HROLD=0                                                                    DATE.328
      MIOLD=0                                                                    DATE.329
      SCOLD=0                                                                    DATE.330
      FROLD=0                                                                    DATE.331
      OLEN=LEN(ODATE)                                                            DATE.332
      IF(OLEN.GE.11)THEN                                                         DATE.333
        SP=ODATE(11:11)                                                          DATE.334
      ELSE                                                                       DATE.335
         sp = ' '                                                                DATE.336
      ENDIF                                                                      DATE.337
C  Use internal READ statements to convert the CHARACTER string                  DATE.338
C  date into INTEGER components.                                                 DATE.339
      READ (ODATE(1:4),'(i4)')YROLD                                              DATE.340
      READ (ODATE(6:7),'(i2)')MOOLD                                              DATE.341
      READ (ODATE(9:10),'(i2)')DYOLD                                             DATE.342
      IF(OLEN.GE.13)THEN                                                         DATE.343
        READ (ODATE(12:13),'(i2)')HROLD                                          DATE.344
        IF(OLEN.GE.16)THEN                                                       DATE.345
          READ (ODATE(15:16),'(i2)')MIOLD                                        DATE.346
          IF(OLEN.GE.19)THEN                                                     DATE.347
            READ (ODATE(18:19),'(i2)')SCOLD                                      DATE.348
            IF(OLEN.GT.20)THEN                                                   DATE.349
              READ (ODATE(21:OLEN),*)FROLD                                       DATE.350
            ENDIF                                                                DATE.351
          ENDIF                                                                  DATE.352
        ENDIF                                                                    DATE.353
      ENDIF                                                                      DATE.354
C  Set the number of days in February for that year.                             DATE.355
      MDAY(2)=NFEB(YROLD)                                                        DATE.356
C  Check that ODATE makes sense.                                                 DATE.357
      OPASS=.TRUE.                                                               DATE.358
C  Check that the month of ODATE makes sense.                                    DATE.359
      IF((MOOLD.GT.12).OR.(MOOLD.LT.1))THEN                                      DATE.360
         write(*,*) 'GETH_NEWDATE:  Month of ODATE = ', moold                    DATE.361
        OPASS=.FALSE.                                                            DATE.362
      ENDIF                                                                      DATE.363
C  Check that the day of ODATE makes sense.                                      DATE.364
      IF((DYOLD.GT.MDAY(MOOLD)).OR.(DYOLD.LT.1))THEN                             DATE.365
         write(*,*) 'GETH_NEWDATE:  Day of ODATE = ', dyold                      DATE.366
        OPASS=.FALSE.                                                            DATE.367
      ENDIF                                                                      DATE.368
C  Check that the hour of ODATE makes sense.                                     DATE.369
      IF((HROLD.GT.23).OR.(HROLD.LT.0))THEN                                      DATE.370
         write(*,*) 'GETH_NEWDATE:  Hour of ODATE = ', hrold                     DATE.371
        OPASS=.FALSE.                                                            DATE.372
      ENDIF                                                                      DATE.373
C  Check that the minute of ODATE makes sense.                                   DATE.374
      IF((MIOLD.GT.59).OR.(MIOLD.LT.0))THEN                                      DATE.375
         write(*,*) 'GETH_NEWDATE:  Minute of ODATE = ', miold                   DATE.376
        OPASS=.FALSE.                                                            DATE.377
      ENDIF                                                                      DATE.378
C  Check that the second of ODATE makes sense.                                   DATE.379
      IF((SCOLD.GT.59).OR.(SCOLD.LT.0))THEN                                      DATE.380
         write(*,*) 'GETH_NEWDATE:  Second of ODATE = ', scold                   DATE.381
        OPASS=.FALSE.                                                            DATE.382
      ENDIF                                                                      DATE.383
C  Check that the fractional part  of ODATE makes sense.                         DATE.384
C!KWM      IF ((scold.GT.59).or.(scold.LT.0)) THEN                               DATE.385
C!KWM         WRITE(*,*) 'GETH_NEWDATE:  Second of ODATE = ', scold              DATE.386
C!KWM         opass = .FALSE.                                                    DATE.387
C!KWM      END IF                                                                DATE.388
      IF(.NOT.OPASS)THEN                                                         DATE.389
         write(*,*) 'Crazy ODATE: ', odate(1:olen), olen                         DATE.390
        STOP 'odate_3'                                                           DATE.391
      ENDIF                                                                      DATE.392
C  Date Checks are completed.  Continue.                                         DATE.393
C  Compute the number of days, hours, minutes, and seconds in idt                DATE.394
      IF(OLEN.GT.20)THEN                                                         DATE.395
!idt should be in fractions of seconds                                           DATE.396
        IFRC=OLEN-20                                                             DATE.397
        IFRC=10**IFRC                                                            DATE.398
        NDAY=ABS(IDT)/(86400*IFRC)                                               DATE.399
        NHOUR=MOD(ABS(IDT),86400*IFRC)/(3600*IFRC)                               DATE.400
        NMIN=MOD(ABS(IDT),3600*IFRC)/(60*IFRC)                                   DATE.401
        NSEC=MOD(ABS(IDT),60*IFRC)/(IFRC)                                        DATE.402
        NFRAC=MOD(ABS(IDT),IFRC)                                                 DATE.403
      ELSEIF(OLEN.EQ.19)THEN                                                     DATE.404
!idt should be in seconds                                                        DATE.405
        IFRC=1                                                                   DATE.406
        NDAY=ABS(IDT)/86400                                                      DATE.407
! integer number of days in delta-t                                              DATE.408
        NHOUR=MOD(ABS(IDT),86400)/3600                                           DATE.409
        NMIN=MOD(ABS(IDT),3600)/60                                               DATE.410
        NSEC=MOD(ABS(IDT),60)                                                    DATE.411
        NFRAC=0                                                                  DATE.412
      ELSEIF(OLEN.EQ.16)THEN                                                     DATE.413
!idt should be in minutes                                                        DATE.414
        IFRC=1                                                                   DATE.415
        NDAY=ABS(IDT)/1440                                                       DATE.416
! integer number of days in delta-ti                                             DATE.417
        NHOUR=MOD(ABS(IDT),1440)/60                                              DATE.418
        NMIN=MOD(ABS(IDT),60)                                                    DATE.419
        NSEC=0                                                                   DATE.420
        NFRAC=0                                                                  DATE.421
      ELSEIF(OLEN.EQ.13)THEN                                                     DATE.422
!idt should be in hours                                                          DATE.423
        IFRC=1                                                                   DATE.424
        NDAY=ABS(IDT)/24                                                         DATE.425
! integer number of days in delta-time                                           DATE.426
        NHOUR=MOD(ABS(IDT),24)                                                   DATE.427
        NMIN=0                                                                   DATE.428
        NSEC=0                                                                   DATE.429
        NFRAC=0                                                                  DATE.430
      ELSEIF(OLEN.EQ.10)THEN                                                     DATE.431
!idt should be in days                                                           DATE.432
        IFRC=1                                                                   DATE.433
        NDAY=ABS(IDT)                                                            DATE.434
! integer number of days in delta-time                                           DATE.435
        NHOUR=0                                                                  DATE.436
        NMIN=0                                                                   DATE.437
        NSEC=0                                                                   DATE.438
        NFRAC=0                                                                  DATE.439
      ELSE                                                                       DATE.440
         write(*,'("GETH_NEWDATE: Strange length for ODATE: ", i3)')             DATE.441
     &         olen                                                              DATE.442
        STOP 'odate_4'                                                           DATE.443
      ENDIF                                                                      DATE.444
      IF(IDT.GE.0)THEN                                                           DATE.445
        FRNEW=FROLD+NFRAC                                                        DATE.446
        IF(FRNEW.GE.IFRC)THEN                                                    DATE.447
          FRNEW=FRNEW-IFRC                                                       DATE.448
          NSEC=NSEC+1                                                            DATE.449
        ENDIF                                                                    DATE.450
        SCNEW=SCOLD+NSEC                                                         DATE.451
        IF(SCNEW.GE.60)THEN                                                      DATE.452
          SCNEW=SCNEW-60                                                         DATE.453
          NMIN=NMIN+1                                                            DATE.454
        ENDIF                                                                    DATE.455
        MINEW=MIOLD+NMIN                                                         DATE.456
        IF(MINEW.GE.60)THEN                                                      DATE.457
          MINEW=MINEW-60                                                         DATE.458
          NHOUR=NHOUR+1                                                          DATE.459
        ENDIF                                                                    DATE.460
        HRNEW=HROLD+NHOUR                                                        DATE.461
        IF(HRNEW.GE.24)THEN                                                      DATE.462
          HRNEW=HRNEW-24                                                         DATE.463
          NDAY=NDAY+1                                                            DATE.464
        ENDIF                                                                    DATE.465
        DYNEW=DYOLD                                                              DATE.466
        MONEW=MOOLD                                                              DATE.467
        YRNEW=YROLD                                                              DATE.468
        DO I=1,NDAY                                                              DATE.469
          DYNEW=DYNEW+1                                                          DATE.470
          IF(DYNEW.GT.MDAY(MONEW))THEN                                           DATE.471
            DYNEW=DYNEW-MDAY(MONEW)                                              DATE.472
            MONEW=MONEW+1                                                        DATE.473
            IF(MONEW.GT.12)THEN                                                  DATE.474
              MONEW=1                                                            DATE.475
              YRNEW=YRNEW+1                                                      DATE.476
C        If the year changes, recompute the number of days in Februa             DATE.477
              MDAY(2)=NFEB(YRNEW)                                                DATE.478
            ENDIF                                                                DATE.479
          ENDIF                                                                  DATE.480
        ENDDO                                                                    DATE.481
      ELSEIF(IDT.LT.0)THEN                                                       DATE.482
        FRNEW=FROLD-NFRAC                                                        DATE.483
        IF(FRNEW.LT.0)THEN                                                       DATE.484
          FRNEW=FRNEW+IFRC                                                       DATE.485
          NSEC=NSEC+1                                                            DATE.486
        ENDIF                                                                    DATE.487
        SCNEW=SCOLD-NSEC                                                         DATE.488
        IF(SCNEW.LT.00)THEN                                                      DATE.489
          SCNEW=SCNEW+60                                                         DATE.490
          NMIN=NMIN+1                                                            DATE.491
        ENDIF                                                                    DATE.492
        MINEW=MIOLD-NMIN                                                         DATE.493
        IF(MINEW.LT.00)THEN                                                      DATE.494
          MINEW=MINEW+60                                                         DATE.495
          NHOUR=NHOUR+1                                                          DATE.496
        ENDIF                                                                    DATE.497
        HRNEW=HROLD-NHOUR                                                        DATE.498
        IF(HRNEW.LT.00)THEN                                                      DATE.499
          HRNEW=HRNEW+24                                                         DATE.500
          NDAY=NDAY+1                                                            DATE.501
        ENDIF                                                                    DATE.502
        DYNEW=DYOLD                                                              DATE.503
        MONEW=MOOLD                                                              DATE.504
        YRNEW=YROLD                                                              DATE.505
        DO I=1,NDAY                                                              DATE.506
          DYNEW=DYNEW-1                                                          DATE.507
          IF(DYNEW.EQ.0)THEN                                                     DATE.508
            MONEW=MONEW-1                                                        DATE.509
            IF(MONEW.EQ.0)THEN                                                   DATE.510
              MONEW=12                                                           DATE.511
              YRNEW=YRNEW-1                                                      DATE.512
C        If the year changes, recompute the number of days in Februa             DATE.513
              MDAY(2)=NFEB(YRNEW)                                                DATE.514
            ENDIF                                                                DATE.515
            DYNEW=MDAY(MONEW)                                                    DATE.516
          ENDIF                                                                  DATE.517
        ENDDO                                                                    DATE.518
      ENDIF                                                                      DATE.519
C  Now construct the new mdate                                                   DATE.520
      NLEN=LEN(NDATE)                                                            DATE.521
      IF(NLEN.GT.20)THEN                                                         DATE.522
        WRITE (NDATE(1:19),19)YRNEW,MONEW,DYNEW,HRNEW,MINEW,                     DATE.523
     +                        SCNEW                                              DATE.524
        write(hfrc,'(i10)') frnew+1000000000                                     DATE.525
        ndate = ndate(1:19)//'.'//hfrc(31-nlen:10)                               DATE.526
      ELSEIF(NLEN.EQ.19.OR.NLEN.EQ.20)THEN                                       DATE.527
        WRITE (NDATE(1:19),19)YRNEW,MONEW,DYNEW,HRNEW,MINEW,                     DATE.528
     +                        SCNEW                                              DATE.529
19    FORMAT(i4,'-',i2.2,'-',i2.2,'_',i2.2,':',i2.2,':',i2.2)                    DATE.530
        IF(NLEN.EQ.20)NDATE=NDATE(1:19)//'.'                                     DATE.531
      ELSEIF(NLEN.EQ.16)THEN                                                     DATE.532
        WRITE (NDATE,16)YRNEW,MONEW,DYNEW,HRNEW,MINEW                            DATE.533
16    FORMAT(i4,'-',i2.2,'-',i2.2,'_',i2.2,':',i2.2)                             DATE.534
      ELSEIF(NLEN.EQ.13)THEN                                                     DATE.535
        WRITE (NDATE,13)YRNEW,MONEW,DYNEW,HRNEW                                  DATE.536
13    FORMAT(i4,'-',i2.2,'-',i2.2,'_',i2.2)                                      DATE.537
      ELSEIF(NLEN.EQ.10)THEN                                                     DATE.538
        WRITE (NDATE,10)YRNEW,MONEW,DYNEW                                        DATE.539
10    FORMAT(i4,'-',i2.2,'-',i2.2)                                               DATE.540
      ENDIF                                                                      DATE.541
      IF(OLEN.GE.11)NDATE(11:11)=SP                                              DATE.542
      RETURN                                                                     DATE.543
      END                                                                        DATE.544
C end subroutine geth_newdate                                                    DATE.545
      INTEGER FUNCTION NFEB(YEAR)                                                DATE.546
C Compute the number of days in February for the given year                      DATE.547
      IMPLICIT NONE                                                              DATE.548
      INTEGER YEAR                                                               DATE.549
#ifdef NOLEAP
      NFEB=28                                                                    DATE.550
#else
      NFEB=28                                                                    DATE.550
! By default, February has 28 days ...                                           DATE.551
      IF(MOD(YEAR,4).EQ.0)THEN                                                   DATE.552
        NFEB=29                                                                  DATE.553
! But every four years, it has 29 days ...                                       DATE.554
        IF(MOD(YEAR,100).EQ.0)THEN                                               DATE.555
          NFEB=28                                                                DATE.556
! Except every 100 years, when it has 28 days                                    DATE.557
          IF(MOD(YEAR,400).EQ.0)THEN                                             DATE.558
            NFEB=29                                                              DATE.559
! Except every 400 years, when it has 29 d                                       DATE.560
            IF(MOD(YEAR,3600).EQ.0)THEN                                          DATE.561
              NFEB=28                                                            DATE.562
! Except every 3600 years, when it has                                           DATE.567
            ENDIF                                                                DATE.563
          ENDIF                                                                  DATE.564
        ENDIF                                                                    DATE.565
      ENDIF
#endif                                                                      DATE.566
      RETURN                                                                     DATE.568
      END                                                                        DATE.569
C end function nfeb                                                              DATE.570
      SUBROUTINE SPLIT_DATE_CHAR(DATE,CENTURY_YEAR,MONTH,DAY,                    DATE.571
     +                          HOUR,MINUTE,                                     DATE.572
     +                          SECOND)                                          DATE.573
C  This program is modified based on DOG's f90 code                              DATE.574
C  3. split_date_char ( date , century_year , month , day , hour , m             DATE.575
C  Given the date, return the integer components.                                DATE.576
      IMPLICIT NONE                                                              DATE.577
C        Input data.                                                             DATE.578
      CHARACTER *24DATE                                                          DATE.579
C        Output data.                                                            DATE.580
      INTEGER CENTURY_YEAR,MONTH,DAY,HOUR,MINUTE,SECOND                          DATE.581
      READ(date,FMT='(    I4.4)') century_year                                   DATE.582
      READ(date,FMT='( 5X,I2.2)') month                                          DATE.583
      READ(date,FMT='( 8X,I2.2)') day                                            DATE.584
      READ(date,FMT='(11X,I2.2)') hour                                           DATE.585
      READ(date,FMT='(14X,I2.2)') minute                                         DATE.586
      READ(date,FMT='(17X,I2.2)') second                                         DATE.587
      RETURN                                                                     DATE.588
      END                                                                        DATE.589
                                                                                 DATE.590
C  END SUBROUTINE split_date_char                                                DATE.591
