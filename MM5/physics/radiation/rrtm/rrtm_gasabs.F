C **************************************************************************     RRTM_GASABS.1
      SUBROUTINE GASABS                                                          RRTM_GASABS.2
C **************************************************************************     RRTM_GASABS.3
C  RRTM Longwave Radiative Transfer Model                                        RRTM_GASABS.4
C  Atmospheric and Environmental Research, Inc., Cambridge, MA                   RRTM_GASABS.5
C                                                                                RRTM_GASABS.6
C  Original version:       E. J. Mlawer, et al.                                  RRTM_GASABS.7
C  Revision for NCAR CCM:  Michael J. Iacono; September, 1998                    RRTM_GASABS.8
C                                                                                RRTM_GASABS.9
C  This routine calculates the gaseous optical depths for all 16 longwave        RRTM_GASABS.10
C  spectral bands.  The optical depths are used to define the Pade               RRTM_GASABS.11
C  approximation to the function of tau transition from tranparancy to           RRTM_GASABS.12
C  opacity.  This function, which varies from 0 to 1, is converted to an         RRTM_GASABS.13
C  integer that will serve as an index for the lookup tables of tau              RRTM_GASABS.14
C  transition function and transmittance used in the radiative transfer.         RRTM_GASABS.15
C  These lookup tables are created on initialization in routine RRTMINIT.        RRTM_GASABS.16
C **************************************************************************     RRTM_GASABS.17
C                                                                                RRTM_GASABS.18
C Definitions                                                                    RRTM_GASABS.19
C    NGPT                         ! Total number of g-point subintervals         RRTM_GASABS.20
C    MXLAY                        ! Maximum number of model layers               RRTM_GASABS.21
C    SECANG                       ! Diffusivity angle for flux computation       RRTM_GASABS.22
C    TAU(NGPT,MXLAY)              ! Gaseous optical depths                       RRTM_GASABS.23
C    NLAYERS                      ! Number of model layers used in RRTM          RRTM_GASABS.24
C    PAVEL(MXLAY)                 ! Model layer pressures (mb)                   RRTM_GASABS.25
C    PZ(0:MXLAY)                  ! Model level (interface) pressures (mb)       RRTM_GASABS.26
C    TAVEL(MXLAY)                 ! Model layer temperatures (K)                 RRTM_GASABS.27
C    TZ(0:MXLAY)                  ! Model level (interface) temperatures (K)     RRTM_GASABS.28
C    TBOUND                       ! Surface temperature (K)                      RRTM_GASABS.29
C    BPADE                        ! Pade approximation constant (=1./0.278)      RRTM_GASABS.30
C    ITR(NGPT,MXLAY)              ! Integer lookup table index                   RRTM_GASABS.31
C                                                                                RRTM_GASABS.32
C Parameters                                                                     RRTM_GASABS.33
      PARAMETER (MXLAY=203)                                                      RRTM_GASABS.34
      PARAMETER (NGPT=140)                                                       RRTM_GASABS.35
      PARAMETER (SECANG=1.66)                                                    RRTM_GASABS.36
C Input                                                                          RRTM_GASABS.37
      COMMON /TAUS/     TAU(NGPT,MXLAY)                                          RRTM_GASABS.38
      COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),                       RRTM_GASABS.39
     &                  PZ(0:MXLAY),TZ(0:MXLAY)                                  RRTM_GASABS.40
      COMMON /PADE/     BPADE                                                    RRTM_GASABS.41
C Output                                                                         RRTM_GASABS.42
      COMMON /TBLINDX/  ITR(NGPT,MXLAY)                                          RRTM_GASABS.43
                                                                                 RRTM_GASABS.44
C This compiler directive was added to insure private common block storage       RRTM_GASABS.45
C in multi-tasked mode on a CRAY or SGI for all commons except those that        RRTM_GASABS.46
C carry constants.                                                               RRTM_GASABS.47
C SGI                                                                            RRTM_GASABS.50
C$OMP THREADPRIVATE (/TAUS/,/PROFILE/,/TBLINDX/)                                 RRTM_GASABS.51
                                                                                 RRTM_GASABS.52
C **************************************************************************     RRTM_GASABS.53
                                                                                 RRTM_GASABS.54
C  Calculate optical depth for each band                                         RRTM_GASABS.55
                                                                                 RRTM_GASABS.56
      CALL TAUGB1                                                                RRTM_GASABS.57
      CALL TAUGB2                                                                RRTM_GASABS.58
      CALL TAUGB3                                                                RRTM_GASABS.59
      CALL TAUGB4                                                                RRTM_GASABS.60
      CALL TAUGB5                                                                RRTM_GASABS.61
      CALL TAUGB6                                                                RRTM_GASABS.62
      CALL TAUGB7                                                                RRTM_GASABS.63
      CALL TAUGB8                                                                RRTM_GASABS.64
      CALL TAUGB9                                                                RRTM_GASABS.65
      CALL TAUGB10                                                               RRTM_GASABS.66
      CALL TAUGB11                                                               RRTM_GASABS.67
      CALL TAUGB12                                                               RRTM_GASABS.68
      CALL TAUGB13                                                               RRTM_GASABS.69
      CALL TAUGB14                                                               RRTM_GASABS.70
      CALL TAUGB15                                                               RRTM_GASABS.71
      CALL TAUGB16                                                               RRTM_GASABS.72
                                                                                 RRTM_GASABS.73
C  Compute the lookup table index from the Pade approximation of the             RRTM_GASABS.74
C  tau transition function, which is derived from the optical depth.             RRTM_GASABS.75
                                                                                 RRTM_GASABS.76
      DO 6000 LAY = 1, NLAYERS                                                   RRTM_GASABS.77
         DO 5000 IPR = 1, NGPT                                                   RRTM_GASABS.78
            ODEPTH = SECANG * TAU(IPR,LAY)                                       RRTM_GASABS.79
            TF = ODEPTH/(BPADE+ODEPTH)                                           RRTM_GASABS.80
            IF (ODEPTH.LE.0.) TF=0.                                              RRTM_GASABS.81
            ITR(IPR,LAY) = 5.E3*TF+0.5                                           RRTM_GASABS.82
 5000    CONTINUE                                                                RRTM_GASABS.83
 6000 CONTINUE                                                                   RRTM_GASABS.84
                                                                                 RRTM_GASABS.85
      RETURN                                                                     RRTM_GASABS.86
      END                                                                        RRTM_GASABS.87
