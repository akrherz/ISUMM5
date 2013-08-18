      SUBROUTINE WRITE_FIELDREC(                                                 WRITE_FIELDREC.1
     +                            IUNIT,                                         WRITE_FIELDREC.2
     +                            NDIM,                                          WRITE_FIELDREC.3
     +                            INEST,                                         WRITE_FIELDREC.4
     +                            BUFFER,                                        WRITE_FIELDREC.5
     +                            XTIME,                                         WRITE_FIELDREC.6
     +                            ANAME,                                         WRITE_FIELDREC.7
     +                            ACURRENT_DATE,                                 WRITE_FIELDREC.8
     +                            ASTAGGERING,                                   WRITE_FIELDREC.9
     +                            AORDERING,                                     WRITE_FIELDREC.10
     +                            AUNITS,                                        WRITE_FIELDREC.11
     +                            ADESCRIPTION,                                  WRITE_FIELDREC.12
     +                            IR, JR, KR,                                    WRITE_FIELDREC.13
     +                            IM, JM, KM )                                   19DEC02.869
C                                                                                WRITE_FIELDREC.15
C Called by version 3 outtap.                                                    WRITE_FIELDREC.16
C                                                                                WRITE_FIELDREC.17
      IMPLICIT NONE                                                              WRITE_FIELDREC.18
                                                                                 19DEC02.870
#ifdef MPP1                                                                      19DEC02.871
#include <parame.incl>                                                           19DEC02.872
#endif                                                                           19DEC02.873
      INTEGER        NDIM                                                        WRITE_FIELDREC.19
      INTEGER        IUNIT                                                       WRITE_FIELDREC.20
                                                                                 WRITE_FIELDREC.21
      CHARACTER*(*)  ANAME                                                       WRITE_FIELDREC.22
      CHARACTER*(*)  ASTAGGERING                                                 WRITE_FIELDREC.23
      CHARACTER*(*)  AORDERING                                                   WRITE_FIELDREC.24
      CHARACTER*(*)  ACURRENT_DATE                                               WRITE_FIELDREC.25
      CHARACTER*(*)  AUNITS                                                      WRITE_FIELDREC.26
      CHARACTER*(*)  ADESCRIPTION                                                WRITE_FIELDREC.27
      CHARACTER*9    NAME                                                        WRITE_FIELDREC.28
      CHARACTER*4    STAGGERING                                                  WRITE_FIELDREC.29
      CHARACTER*4    ORDERING                                                    WRITE_FIELDREC.30
      CHARACTER*24   CURRENT_DATE                                                WRITE_FIELDREC.31
      CHARACTER*25   UNITS                                                       WRITE_FIELDREC.32
      CHARACTER*46   DESCRIPTION                                                 WRITE_FIELDREC.33
                                                                                 WRITE_FIELDREC.34
#ifdef MPP1                                                                      WRITE_FIELDREC.35
C CHARACTER ARRAYS SUITABLE FOR PASSING TO NON-FORTRAN ROUTINES                  WRITE_FIELDREC.36
      CHARACTER      SNAME         (9)                                           WRITE_FIELDREC.37
      CHARACTER      SSTAGGERING   (4)                                           WRITE_FIELDREC.38
      CHARACTER      SORDERING     (4)                                           WRITE_FIELDREC.39
      CHARACTER      SCURRENT_DATE (24)                                          WRITE_FIELDREC.40
      CHARACTER      SUNITS        (25)                                          WRITE_FIELDREC.41
      CHARACTER      SDESCRIPTION  (46)                                          WRITE_FIELDREC.42
      REAL           BUFFER2(MIX*MJX*MKX)                                        19DEC02.874
#endif                                                                           WRITE_FIELDREC.43
                                                                                 WRITE_FIELDREC.44
      REAL           BUFFER(*)                                                   WRITE_FIELDREC.45
      INTEGER        INEST                                                       WRITE_FIELDREC.46
      REAL           XTIME                                                       WRITE_FIELDREC.47
      INTEGER        IR, JR, KR                                                  WRITE_FIELDREC.48
      INTEGER        IM, JM, KM                                                  19DEC02.875
      LOGICAL        DM_IONODE                                                   WRITE_FIELDREC.50
      EXTERNAL       DM_IONODE                                                   WRITE_FIELDREC.51
C                                                                                WRITE_FIELDREC.52
      INTEGER        I,J,K,IC                                                    19DEC02.876
      INTEGER        SH_FLAG                                                     WRITE_FIELDREC.54
C                                                                                WRITE_FIELDREC.55
      NAME         = '         '                                                 WRITE_FIELDREC.56
      STAGGERING   = '    '                                                      WRITE_FIELDREC.57
      ORDERING     = '    '                                                      WRITE_FIELDREC.58
      CURRENT_DATE = '                        '                                  WRITE_FIELDREC.59
      UNITS        = '                         '                                 WRITE_FIELDREC.60
      DESCRIPTION  = '                                              '            WRITE_FIELDREC.61
      NAME(1:LEN(ANAME))=ANAME                                                   WRITE_FIELDREC.62
      STAGGERING(1:LEN(ASTAGGERING))=ASTAGGERING                                 WRITE_FIELDREC.63
      ORDERING(1:LEN(AORDERING))=AORDERING                                       WRITE_FIELDREC.64
      CURRENT_DATE(1:LEN(ACURRENT_DATE))=ACURRENT_DATE                           WRITE_FIELDREC.65
      UNITS(1:LEN(AUNITS))=AUNITS                                                WRITE_FIELDREC.66
      DESCRIPTION(1:LEN(ADESCRIPTION))=ADESCRIPTION                              WRITE_FIELDREC.67
      SH_FLAG=1                                                                  WRITE_FIELDREC.68
      CALL WRITE_FLAG(IUNIT,SH_FLAG)                                             WRITE_FIELDREC.69
#ifndef MPP1                                                                     WRITE_FIELDREC.70
      WRITE (IUNIT)NDIM,1,1,1,1,IR,JR,KR,1,XTIME,STAGGERING,ORDERING,            WRITE_FIELDREC.71
     +             CURRENT_DATE,NAME,UNITS,DESCRIPTION                           WRITE_FIELDREC.72
      WRITE (IUNIT)(BUFFER(I),I=1,IR*JR*KR)                                      WRITE_FIELDREC.73
#else                                                                            WRITE_FIELDREC.74
      DO I=1,9                                                                   WRITE_FIELDREC.75
        SNAME(I)=NAME(I:I)                                                       WRITE_FIELDREC.76
      ENDDO                                                                      WRITE_FIELDREC.77
      DO I=1,4                                                                   WRITE_FIELDREC.78
        SSTAGGERING(I)=STAGGERING(I:I)                                           WRITE_FIELDREC.79
      ENDDO                                                                      WRITE_FIELDREC.80
      DO I=1,4                                                                   WRITE_FIELDREC.81
        SORDERING(I)=ORDERING(I:I)                                               WRITE_FIELDREC.82
      ENDDO                                                                      WRITE_FIELDREC.83
      DO I=1,24                                                                  WRITE_FIELDREC.84
        SCURRENT_DATE(I)=CURRENT_DATE(I:I)                                       WRITE_FIELDREC.85
      ENDDO                                                                      WRITE_FIELDREC.86
      DO I=1,25                                                                  WRITE_FIELDREC.87
        SUNITS(I)=UNITS(I:I)                                                     WRITE_FIELDREC.88
      ENDDO                                                                      WRITE_FIELDREC.89
      DO I=1,46                                                                  WRITE_FIELDREC.90
        SDESCRIPTION(I)=DESCRIPTION(I:I)                                         WRITE_FIELDREC.91
      ENDDO                                                                      WRITE_FIELDREC.92
      CALL RSL_WRITE_MM5V3_SM_HEADER(IUNIT,NDIM,1,1,1,1,IR,JR,KR,1,              WRITE_FIELDREC.93
     +     IWORDSIZE,XTIME,RWORDSIZE,SSTAGGERING,4,SORDERING,4,                  WRITE_FIELDREC.94
     +     SCURRENT_DATE,24,SNAME,9,SUNITS,25,SDESCRIPTION,46)                   WRITE_FIELDREC.95
C                                                                                WRITE_FIELDREC.96
      IF(NDIM.GE.2.AND.NDIM.LE.3.AND.ORDERING(1:2).EQ.'YX')THEN                  WRITE_FIELDREC.97
        CALL DM_DIST_WRITE(IUNIT,INEST,BUFFER,NDIM,IR,JR,KR,IM,JM,KM)            19DEC02.877
      ELSE IF(NDIM.EQ.2.AND.ORDERING(1:2).EQ.'CA')THEN                           07NOV00.805
        IF ( DM_IONODE() ) THEN                                                  07NOV00.806
          IC = 1                                                                 19DEC02.878
          DO J=1,JR                                                              19DEC02.879
            DO I=1,IR                                                            19DEC02.880
              IF ( IC .GT. MIX*MJX*MKX ) THEN                                                  19DEC02.881
		WRITE(0,*)'FATAL INTERNAL ERROR: WRITE_FIELDREC'                               19DEC02.882
		WRITE(0,*)'WOULD OVERWRITE BUFFER2'                                            19DEC02.883
		WRITE(0,*)'Contact mesouser@ucar.edu'                                          19DEC02.884
		STOP                                                                           19DEC02.885
              ENDIF                                                                            19DEC02.886
              BUFFER2(IC) = BUFFER(I+(J-1)*IM)                                   19DEC02.887
              IC = IC+1                                                          19DEC02.888
            ENDDO                                                                19DEC02.889
          ENDDO                                                                  19DEC02.890
          CALL RSL_WRITE_1D_DATA(IUNIT,BUFFER2,IC-1,RSL_REAL)                    19DEC02.891
        ENDIF                                                                    19DEC02.892
      ELSE                                                                       WRITE_FIELDREC.99
        CALL RSL_WRITE_1D_DATA(IUNIT,BUFFER,IR*JR*KR,RSL_REAL)                   WRITE_FIELDREC.100
      ENDIF                                                                      WRITE_FIELDREC.101
#endif                                                                           WRITE_FIELDREC.102
      RETURN                                                                     WRITE_FIELDREC.103
      END                                                                        WRITE_FIELDREC.104
                                                                                 WRITE_FIELDREC.105
