      SUBROUTINE WRITE_FIELDREC(                                                 WRITE_FIELDREC.1
     +                            IUNIT,                                         WRITE_FIELDREC.2
     +                            NDIM,                                          WRITE_FIELDREC.3
     +                            BUFFER,                                        WRITE_FIELDREC.4
     +                            XTIME,                                         WRITE_FIELDREC.5
     +                            ANAME,                                         WRITE_FIELDREC.6
     +                            ACURRENT_DATE,                                 WRITE_FIELDREC.7
     +                            ASTAGGERING,                                   WRITE_FIELDREC.8
     +                            AORDERING,                                     WRITE_FIELDREC.9
     +                            AUNITS,                                        WRITE_FIELDREC.10
     +                            ADESCRIPTION,                                  WRITE_FIELDREC.11
     +                            IR, JR, KR)                                    WRITE_FIELDREC.12
C                                                                                WRITE_FIELDREC.13
C CALLED BY VERSION 3 OUTTAP.                                                    WRITE_FIELDREC.14
C                                                                                WRITE_FIELDREC.15
      IMPLICIT NONE                                                              WRITE_FIELDREC.16
      INTEGER        NDIM                                                        WRITE_FIELDREC.17
      INTEGER        IUNIT                                                       WRITE_FIELDREC.18
      CHARACTER*(*)  ANAME                                                       WRITE_FIELDREC.19
      CHARACTER*(*)  ASTAGGERING                                                 WRITE_FIELDREC.20
      CHARACTER*(*)  AORDERING                                                   WRITE_FIELDREC.21
      CHARACTER*(*)  ACURRENT_DATE                                               WRITE_FIELDREC.22
      CHARACTER*(*)  AUNITS                                                      WRITE_FIELDREC.23
      CHARACTER*(*)  ADESCRIPTION                                                WRITE_FIELDREC.24
      CHARACTER*9    NAME                                                        WRITE_FIELDREC.25
      CHARACTER*4    STAGGERING                                                  WRITE_FIELDREC.26
      CHARACTER*4    ORDERING                                                    WRITE_FIELDREC.27
      CHARACTER*24   CURRENT_DATE                                                WRITE_FIELDREC.28
      CHARACTER*25   UNITS                                                       WRITE_FIELDREC.29
      CHARACTER*46   DESCRIPTION                                                 WRITE_FIELDREC.30
      REAL           BUFFER(*)                                                   WRITE_FIELDREC.31
      REAL           XTIME                                                       WRITE_FIELDREC.32
      INTEGER        IR, JR, KR                                                  WRITE_FIELDREC.33
C                                                                                WRITE_FIELDREC.34
      INTEGER        I, J                                                        WRITE_FIELDREC.35
      INTEGER        SH_FLAG                                                     WRITE_FIELDREC.36
C                                                                                WRITE_FIELDREC.37
      NAME         = '         '                                                 WRITE_FIELDREC.38
      STAGGERING   = '    '                                                      WRITE_FIELDREC.39
      ORDERING     = '    '                                                      WRITE_FIELDREC.40
      CURRENT_DATE = '                        '                                  WRITE_FIELDREC.41
      UNITS        = '                         '                                 WRITE_FIELDREC.42
      DESCRIPTION  = '                                              '            WRITE_FIELDREC.43
                                                                                 WRITE_FIELDREC.44
      NAME         (1:LEN(ANAME          )) = ANAME                              WRITE_FIELDREC.45
      STAGGERING   (1:LEN(ASTAGGERING    )) = ASTAGGERING                        WRITE_FIELDREC.46
      ORDERING     (1:LEN(AORDERING      )) = AORDERING                          WRITE_FIELDREC.47
      CURRENT_DATE (1:LEN(ACURRENT_DATE  )) = ACURRENT_DATE                      WRITE_FIELDREC.48
      UNITS        (1:LEN(AUNITS         )) = AUNITS                             WRITE_FIELDREC.49
      DESCRIPTION  (1:LEN(ADESCRIPTION   )) = ADESCRIPTION                       WRITE_FIELDREC.50
                                                                                 WRITE_FIELDREC.51
      SH_FLAG = 1                                                                WRITE_FIELDREC.52
      WRITE (IUNIT)SH_FLAG                                                       WRITE_FIELDREC.53
      WRITE (IUNIT)NDIM,1,1,1,1,IR,JR,KR,1,XTIME,STAGGERING,ORDERING,            WRITE_FIELDREC.54
     +             CURRENT_DATE,NAME,UNITS,DESCRIPTION                           WRITE_FIELDREC.55
C     WRITE (IUNIT)((BUFFER(I,J),I=1,IR),J=1,JR)                                 WRITE_FIELDREC.56
      WRITE (IUNIT)(BUFFER(I),I=1,IR*JR*KR)                                      WRITE_FIELDREC.57
      RETURN                                                                     WRITE_FIELDREC.58
      END                                                                        WRITE_FIELDREC.59
