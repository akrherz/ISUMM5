#ifdef MPP1                                                                      DM_IO.1
                                                                                 DM_IO.2
#  include <mpp_dmio_10.incl>                                                    DM_IO.3
                                                                                 DM_IO.4
#else                                                                            DM_IO.5
                                                                                 DM_IO.6
C ALL STUBS WHEN NON-MPP                                                         DM_IO.7
                                                                                 DM_IO.8
       LOGICAL FUNCTION DM_IONODE()                                              DM_IO.9
       DM_IONODE = .TRUE.                                                        DM_IO.10
       RETURN                                                                    DM_IO.11
       END                                                                       DM_IO.12
                                                                                 DM_IO.13
       SUBROUTINE DM_BCAST_INTEGERS( BUF, N )                                    DM_IO.14
       IMPLICIT NONE                                                             DM_IO.15
       INTEGER BUF(*)                                                            DM_IO.16
       INTEGER N                                                                 DM_IO.17
       RETURN                                                                    DM_IO.18
       END                                                                       DM_IO.19
                                                                                 DM_IO.20
       SUBROUTINE DM_BCAST_REALS( BUF, N )                                       DM_IO.21
       IMPLICIT NONE                                                             DM_IO.22
       REAL BUF(*)                                                               DM_IO.23
       INTEGER N                                                                 DM_IO.24
       RETURN                                                                    DM_IO.25
       END                                                                       DM_IO.26
                                                                                 DM_IO.27
       SUBROUTINE DM_BCAST_LOGICALS( BUF, N )                                    DM_IO.28
       IMPLICIT NONE                                                             DM_IO.29
       LOGICAL BUF(*)                                                            DM_IO.30
       INTEGER N                                                                 DM_IO.31
       RETURN                                                                    DM_IO.32
       END                                                                       DM_IO.33
                                                                                 DM_IO.34
       SUBROUTINE DM_BCAST_BYTES( BUF, N )                                       DM_IO.35
       IMPLICIT NONE                                                             DM_IO.36
       CHARACTER BUF(*)                                                          DM_IO.37
       INTEGER N                                                                 DM_IO.38
       RETURN                                                                    DM_IO.39
       END                                                                       DM_IO.40
                                                                                 DM_IO.41
       SUBROUTINE DM_BCAST_STRING( BUF, N )                                      DM_IO.42
       IMPLICIT NONE                                                             DM_IO.43
       INTEGER N                                                                 DM_IO.44
       CHARACTER*(*) BUF                                                         DM_IO.45
       RETURN                                                                    DM_IO.46
       END                                                                       DM_IO.47
                                                                                 DM_IO.48
       SUBROUTINE DM_DIST_BDY( ordering,name,                                    DM_IO.49
     &     IUNIT,INEST,BUF,NDIM,lendim1,lendim2,lendim3,                         DM_IO.50
     &     IX,JX,MKX )                                                           DM_IO.51
       IMPLICIT NONE                                                             DM_IO.52
       CHARACTER*(*) ordering                                                    DM_IO.53
       CHARACTER*(*) name                                                        DM_IO.54
       INTEGER IUNIT,INEST,NDIM,lendim1,lendim2,lendim3,                         DM_IO.55
     &         IX,JX,MKX                                                         DM_IO.56
       REAL BUF(*)                                                               DM_IO.57
       RETURN                                                                    DM_IO.58
       END                                                                       DM_IO.59
                                                                                 DM_IO.60
                                                                                 DM_IO.61
#endif                                                                           DM_IO.62
                                                                                 DM_IO.63
                                                                                 DM_IO.64
