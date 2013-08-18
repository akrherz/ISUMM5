MODULE all_io

   USE header_data
   USE util
   USE date_pack

   TYPE input_fields_3d
      REAL , POINTER , DIMENSION(:,:,:) :: array
      TYPE(sh)                          :: small_header
   END TYPE input_fields_3d

   TYPE input_fields_2d
      REAL , POINTER , DIMENSION(:,:)   :: array
      TYPE(sh)                          :: small_header
   END TYPE input_fields_2d

   TYPE input_fields_1d
      REAL , POINTER , DIMENSION(:)     :: array
      TYPE(sh)                          :: small_header
   END TYPE input_fields_1d

   TYPE(input_fields_3d) , ALLOCATABLE , DIMENSION(:) :: allp_3d
   TYPE(input_fields_3d) , ALLOCATABLE , DIMENSION(:) :: alls_3d
   TYPE(input_fields_2d) , ALLOCATABLE , DIMENSION(:) :: all_2d
   TYPE(input_fields_1d) , ALLOCATABLE , DIMENSION(:) :: all_1d

   TYPE input_and_bdy_fields_3d
      REAL , POINTER , DIMENSION(:,:,:) :: array
      REAL , POINTER , DIMENSION(:,:,:) :: ebdy , wbdy , nbdy , sbdy
      TYPE(sh)                          :: small_header
   END TYPE input_and_bdy_fields_3d

   TYPE(input_and_bdy_fields_3d) , ALLOCATABLE , DIMENSION(:) :: allm_3d

   INTEGER :: nump_3d , nums_3d , numm_3d , num_2d , num_1d
   INTEGER :: iprog_no_old

   CHARACTER(LEN=19) :: sh_date

   REAL , DIMENSION(:,:) , ALLOCATABLE :: tseasfc_sum , sfct_sum

   INTEGER :: index_t , index_u , index_v , index_height , index_rh
   INTEGER :: index_qc , index_qr , index_qs , index_qi , index_qg , index_qn
   INTEGER :: index_sfc_qc , index_sfc_qr , index_sfc_qs , index_sfc_qi , index_sfc_qg , index_sfc_qn
   INTEGER :: index_sfc_t , index_sfc_u , index_sfc_v , index_sfc_rh , index_terrain , index_land_use , index_mapfaccr , &
              index_mapfacdt , index_coriolis , index_latitcrs , index_longicrs , index_latitdot , index_longidot , &
              index_snowcovr , index_psealvld , index_psealvlc , index_tseasfc
   INTEGER :: index_p

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE read_data ( unit_fg , file_fg , count_fg , kx , daily_mean_sst , &
                          cloud_and_rain , snow_and_ice , graupel , number_concentration )

      IMPLICIT NONE

      !  Input variables.

      INTEGER :: unit_fg , count_fg
      CHARACTER(LEN=132) , DIMENSION(100) :: file_fg

      !  Output variables.

      INTEGER :: kx

      LOGICAL , INTENT(OUT) :: daily_mean_sst

      LOGICAL , INTENT(INOUT) :: cloud_and_rain , snow_and_ice , graupel , number_concentration

      !  Local variables.

      INTEGER :: loop3 , loop2 , loop1
      INTEGER :: flag , ok , bhi_i1
      LOGICAL :: cloud = .FALSE. , &
                 rain  = .FALSE. , &
                 ice   = .FALSE. , &
                 snow  = .FALSE.

      !  Loop over all of the variables.

      loop3 = 0
      loop2 = 0
      loop1 = 0
      daily_mean_sst = .TRUE.

      var_loop : DO

         !  Start off with a flag, as ususal.  We are here after a big header
         !  flag, so we can just look for the small header without trying to 
         !  do too many error tests for which flag is expected.
   
         READ ( unit_fg , IOSTAT = ok ) flag
   
         IF      ( ( ok .LT. 0 ) .AND. ( count_fg .EQ. 100 ) ) THEN
            PRINT '(A,I8,A)','Unexpected EOF in first-guess data: ',ok,'.'
            PRINT '(A)','Exhausted list of input file names in the namelist.input file.'
            PRINT '(A)','You may have asked for a time period that is not available.'
            STOP 'EOF_FG_read_all_files'
         ELSE IF ( ( ok .LT. 0 ) .AND. ( LEN_TRIM ( file_fg(count_fg+1) ) .EQ. 0 ) ) THEN
            PRINT '(A,I8,A)','Unexpected EOF in first-guess data: ',ok,'.'
            PRINT '(A)','No subsequent file specified for input in the namelist.input file.'
            PRINT '(A)','You may have asked for a time period that is not available.'
            STOP 'EOF_in_FG_data'
         ELSE IF ( ( ok .LT. 0 ) .AND. ( LEN_TRIM ( file_fg(count_fg+1) ) .NE. 0 ) ) THEN
            PRINT '(A,I8,A)','Unexpected EOF in first-guess data: ',ok,'.'
            PRINT '(A,A,A)','Moving to next file in the namelist.input file: ',TRIM ( file_fg(count_fg+1) ) ,'.'
            CLOSE ( unit_fg )
            count_fg = count_fg + 1
            CALL get_fg_file ( file_fg(count_fg) ,  unit_fg )
            CYCLE var_loop
         END IF

         IF ( flag .EQ. bh_flag ) THEN
            PRINT '(A,I1,A)','Wrong flag, wanted small header flag: ',flag,'.'
            PRINT '(A)','Assuming that this is an instance of multiple input files or cat''ed first-guess input files.'
            READ ( unit_fg ) bhi_i1
            IF ( ( bhi_i1 .EQ. 2 ) .OR. ( bhi_i1 .EQ. 3 ) .OR. ( bhi_i1 .EQ. 8 ) ) THEN
               CYCLE var_loop
            ELSE
               PRINT '(A,I8,A)','This did not turn out to be first-guess data, program ID = ',bhi_i1,'.'
               STOP 'Messed_up_multiple_input_files'
            END IF
         ELSE IF ( flag .EQ. eot_flag ) THEN
            PRINT '(A)','Found the end of the time period.'
            EXIT var_loop
         END IF
   
         !  We have the right flag, so get the header.
   
         READ ( unit_fg ) small_header%num_dims , small_header%start_dims , small_header%end_dims , &
                        small_header%xtime , small_header%staggering , small_header%ordering , &
                        small_header%current_date , small_header%name , small_header%units , &
                        small_header%description
   
         !  Let's allocate space for this one array.  Then read it in.  The 3d array will be
         !  input as a surface field and a 3d array (the pressure levels).

         IF      ( small_header%num_dims .eq. 3 ) THEN
            
            !  Increment the count of the fields.
          
            loop3 = loop3 + 1
            loop2 = loop2 + 1
    
            !  Number of pressure levels, EXCLUDING the surface.

            kx = small_header%end_dims(3)-1

            !  Allocate space for the data in the input array area.

            ALLOCATE ( allp_3d(loop3)%array(small_header%end_dims(1),small_header%end_dims(2),small_header%end_dims(3)-1) )
            ALLOCATE ( all_2d(loop2)%array(small_header%end_dims(1),small_header%end_dims(2)) )

            !  Use this space to input the data.

            READ ( unit_fg ) all_2d(loop2)%array , allp_3d(loop3)%array
       
            !  Assign the small_header data to this variable's small_header storage.  We need to
            !  modify the 2d header to reflect the 2d nature of the data.            

            all_2d(loop2)%small_header = small_header
            all_2d(loop2)%small_header%num_dims = 2
            all_2d(loop2)%small_header%ordering(3:3) = ' '
            all_2d(loop2)%small_header%name = TRIM ( all_2d(loop2)%small_header%name ) // 'SFC'

            allp_3d(loop3)%small_header = small_header

            !  Check to see if this 3d data contains any of the non-traditional hydrometeor fields that may
            !  be used with lateral boundary conditions in the model.

            IF      ( small_header%name(1:8) .EQ. 'CLW     ' ) THEN
               cloud                = .TRUE.
            ELSE IF ( small_header%name(1:8) .EQ. 'RNW     ' ) THEN
               rain                 = .TRUE.
            ELSE IF ( small_header%name(1:8) .EQ. 'ICE     ' ) THEN
               ice                  = .TRUE.
            ELSE IF ( small_header%name(1:8) .EQ. 'SNOW    ' ) THEN
               snow                 = .TRUE.
            ELSE IF ( small_header%name(1:8) .EQ. 'GRAUPEL ' ) THEN
               graupel              = .TRUE.
            ELSE IF ( small_header%name(1:8) .EQ. 'NCI     ' ) THEN
               number_concentration = .TRUE.
            END IF
   
         ELSE IF ( small_header%num_dims .eq. 2 ) THEN
            
            !  Increment the count of the fields.
          
            loop2 = loop2 + 1
    
            !  Allocate space for the data in the input array area.

            ALLOCATE ( all_2d(loop2)%array(small_header%end_dims(1),small_header%end_dims(2)) )

            !  Use this space to input the data.

            READ ( unit_fg ) all_2d(loop2)%array

            !  Assign the small_header data to this variable's small_header storage.

            all_2d(loop2)%small_header = small_header
           
            !  Did we find a "real" SST yet?  If so, we need to set the SST mean flag
            !  to .FALSE. - which inplies that we can do physically reasonable variable
            !  SST computations.

            IF ( small_header%name(1:8) .EQ. 'TSEASFC ' ) THEN
               daily_mean_sst = .FALSE.
            END IF
   
         ELSE IF ( small_header%num_dims .eq. 1 ) THEN
            
            !  Increment the count of the fields.
          
            loop1 = loop1 + 1
    
            !  Allocate space for the data in the input array area.

            ALLOCATE ( all_1d(loop1)%array(small_header%end_dims(1)) )

            !  Use this space to input the data.

            READ ( unit_fg ) all_1d(loop1)%array

            !  Assign the small_header data to this variable's small_header storage.

            all_1d(loop1)%small_header = small_header
   
         END IF
   
      END DO var_loop

      !  A few values that we want out of here: the date, and the number of fields.

      sh_date = small_header%current_date(1:19)

      nump_3d = loop3
      num_2d = loop2
      num_1d = loop1

      !  We'd also be interested in knowing if there are enough additional moisture species to make
      !  some fields available from the lateral boundaries.  Note that there is a hierarchy for the
      !  hydrometeors, based on the various physical parameterization assumptions in the model.  

      IF ( cloud .AND. rain ) THEN
         cloud_and_rain       = .TRUE.
      ELSE
         cloud_and_rain       = .FALSE.
      END IF

      IF ( cloud_and_rain .AND. ( ice .AND. snow ) ) THEN
         snow_and_ice         = .TRUE.
      ELSE
         snow_and_ice         = .FALSE.
      ENDIF

      IF ( cloud_and_rain .AND. snow_and_ice .AND. graupel ) THEN
         graupel              = .TRUE.
      ELSE
         graupel              = .FALSE.
      END IF

      IF ( cloud_and_rain .AND. snow_and_ice .AND. graupel .AND. number_concentration ) THEN
         number_concentration = .TRUE.
      ELSE
         number_concentration = .FALSE.
      END IF
      
   END SUBROUTINE read_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE assign_data ( t, u, v, height, rh, &
                            terrain, land_use, mapfaccr, mapfacdt, coriolis, &
                            latitcrs, longicrs, latitdot, longidot, snowcovr, &
                            psealvld, psealvlc, tseasfc, &
                            sfc_rh, sfc_t, sfc_u, sfc_v , &
                            p , kx , daily_mean_sst )

      IMPLICIT NONE

      !  Input variables.

      REAL , DIMENSION(:,:,:) :: t, u, v, height, rh

      REAL , DIMENSION(:,:)   :: terrain, land_use, mapfaccr, mapfacdt, coriolis, &
                                 latitcrs, longicrs, latitdot, longidot, snowcovr, &
                                 psealvld, psealvlc, tseasfc, &
                                 sfc_rh, sfc_t, sfc_u, sfc_v

      REAL , DIMENSION(:)     :: p

      INTEGER :: kx

      LOGICAL , INTENT(IN)    :: daily_mean_sst

      !  Local variables.

      INTEGER :: loop
      INTEGER :: is , js , ie , je
 
      !  If the data are expanded, we need to chop down the size on the assignment for
      !  the horizontal dimensions.

      IF ( ( bhi( 1,1) .EQ. 2 ) .AND. &
           ( bhi( 8,1) .EQ. 1 ) .AND. &
           ( bhi(15,1) .EQ. 0 ) ) THEN
         is = 1         + bhi(11,1)
         js = 1         + bhi(12,1)
         ie = bhi(16,1) + bhi(11,1)
         je = bhi(17,1) + bhi(12,1)
      ELSE
         is = 1
         js = 1
         ie = bhi(16,1)
         je = bhi(17,1)
      END IF
      iprog_no_old = bhi(1,1)

      !  Assign the 3d data.

      DO loop = 1 , nump_3d
         IF      ( allp_3d(loop)%small_header%name(1:8) .EQ. 'T       ' ) THEN
            t      = allp_3d(loop)%array(is:ie,js:je,:)
            CALL invert2 ( t      , bhi(16,1) , bhi(17,1) , kx )
            index_t = loop
         ELSE IF ( allp_3d(loop)%small_header%name(1:8) .EQ. 'U       ' ) THEN
            u      = allp_3d(loop)%array(is:ie,js:je,:)
            CALL invert2 ( u      , bhi(16,1) , bhi(17,1) , kx )
            index_u = loop
         ELSE IF ( allp_3d(loop)%small_header%name(1:8) .EQ. 'V       ' ) THEN
            v      = allp_3d(loop)%array(is:ie,js:je,:) 
            CALL invert2 ( v      , bhi(16,1) , bhi(17,1) , kx )
            index_v = loop
         ELSE IF ( allp_3d(loop)%small_header%name(1:8) .EQ. 'H       ' ) THEN
            height = allp_3d(loop)%array(is:ie,js:je,:)
            CALL invert2 ( height , bhi(16,1) , bhi(17,1) , kx )
            index_height = loop
         ELSE IF ( allp_3d(loop)%small_header%name(1:8) .EQ. 'RH      ' ) THEN
            rh     = allp_3d(loop)%array(is:ie,js:je,:)
            CALL invert2 ( rh     , bhi(16,1) , bhi(17,1) , kx )
            index_rh = loop
         ELSE
!           PRINT '(A,A,A)','Unnecessary 3d array: ',allp_3d(loop)%small_header%name(1:8),'.'
         END IF
      END DO

      !  Assign the 2d data.

      DO loop = 1 , num_2d
         IF      ( all_2d(loop)%small_header%name(1:8) .EQ. 'TSFC    ' ) THEN
            sfc_t    = all_2d(loop)%array(is:ie,js:je)
            index_sfc_t = loop
         ELSE IF ( all_2d(loop)%small_header%name(1:8) .EQ. 'USFC    ' ) THEN
            sfc_u    = all_2d(loop)%array(is:ie,js:je)
            index_sfc_u = loop
         ELSE IF ( all_2d(loop)%small_header%name(1:8) .EQ. 'VSFC    ' ) THEN
            sfc_v    = all_2d(loop)%array(is:ie,js:je)
            index_sfc_v = loop
         ELSE IF ( all_2d(loop)%small_header%name(1:8) .EQ. 'HSFC    ' ) THEN
            PRINT '(A)','Neglecting surface height field.'
         ELSE IF ( all_2d(loop)%small_header%name(1:8) .EQ. 'RHSFC   ' ) THEN
            sfc_rh   = all_2d(loop)%array(is:ie,js:je)
            index_sfc_rh = loop
         ELSE IF ( all_2d(loop)%small_header%name(1:8) .EQ. 'TERRAIN ' ) THEN
            terrain  = all_2d(loop)%array(is:ie,js:je)
            index_terrain = loop
         ELSE IF ( all_2d(loop)%small_header%name(1:8) .EQ. 'LAND USE' ) THEN
            land_use = all_2d(loop)%array(is:ie,js:je)
            index_land_use = loop
         ELSE IF ( all_2d(loop)%small_header%name(1:8) .EQ. 'MAPFACCR' ) THEN
            mapfaccr = all_2d(loop)%array(is:ie,js:je)
            index_mapfaccr = loop
         ELSE IF ( all_2d(loop)%small_header%name(1:8) .EQ. 'MAPFACDT' ) THEN
            mapfacdt = all_2d(loop)%array(is:ie,js:je)
            index_mapfacdt = loop
         ELSE IF ( all_2d(loop)%small_header%name(1:8) .EQ. 'CORIOLIS' ) THEN
            coriolis = all_2d(loop)%array(is:ie,js:je)
            index_coriolis = loop
         ELSE IF ( all_2d(loop)%small_header%name(1:8) .EQ. 'LATITCRS' ) THEN
            latitcrs = all_2d(loop)%array(is:ie,js:je)
            index_latitcrs = loop
         ELSE IF ( all_2d(loop)%small_header%name(1:8) .EQ. 'LONGICRS' ) THEN
            longicrs = all_2d(loop)%array(is:ie,js:je)
            index_longicrs = loop
         ELSE IF ( all_2d(loop)%small_header%name(1:8) .EQ. 'LATITDOT' ) THEN
            latitdot = all_2d(loop)%array(is:ie,js:je)
            index_latitdot = loop
         ELSE IF ( all_2d(loop)%small_header%name(1:8) .EQ. 'LONGIDOT' ) THEN
            longidot = all_2d(loop)%array(is:ie,js:je)
            index_longidot = loop
         ELSE IF ( all_2d(loop)%small_header%name(1:8) .EQ. 'SNOWCOVR' ) THEN
            snowcovr = all_2d(loop)%array(is:ie,js:je)
            index_snowcovr = loop
         ELSE IF ( all_2d(loop)%small_header%name(1:8) .EQ. 'PSEALVLD' ) THEN
            psealvld = all_2d(loop)%array(is:ie,js:je)
            index_psealvld = loop
         ELSE IF ( all_2d(loop)%small_header%name(1:8) .EQ. 'PSEALVLC' ) THEN
            psealvlc = all_2d(loop)%array(is:ie,js:je)
            index_psealvlc = loop
         ELSE IF ( all_2d(loop)%small_header%name(1:8) .EQ. 'TSEASFC ' ) THEN
            tseasfc  = all_2d(loop)%array(is:ie,js:je)
            index_tseasfc = loop
         ELSE IF(( all_2d(loop)%small_header%name(1:8) .EQ. 'SKINTEMP' ) .AND. ( daily_mean_sst ) ) THEN
            all_2d(loop)%small_header%name(1:8) = 'TSEASFC '
            tseasfc  = all_2d(loop)%array(is:ie,js:je)
            index_tseasfc = loop
         ELSE
!           PRINT '(A,A,A)','Found extra 2d array, probably for LSM: ',TRIM( all_2d(loop)%small_header%name(1:8) ),'.'
         END IF
      END DO

      !  Assign the 1d data.

      DO loop = 1 , num_1d
         IF      ( all_1d(loop)%small_header%name(1:8) .EQ. 'PRESSURE' ) THEN
            p      = all_1d(loop)%array(2:)
            index_p = loop
         ELSE
            PRINT '(A,A,A)','Unnecessary 1d array: ',all_1d(loop)%small_header%name(1:8),'.'
         END IF
      END DO

   END SUBROUTINE assign_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE assign_cloud_and_rain ( qc , qr , sfc_qc , sfc_qr , kx ) 

      IMPLICIT NONE

      !  Input variables.

      REAL , DIMENSION(:,:,:) :: qc , qr
      REAL , DIMENSION(:,:) :: sfc_qc , sfc_qr

      INTEGER :: kx

      !  Local variables.

      INTEGER :: loop
      INTEGER :: is , js , ie , je
 
      !  If the data are expanded, we need to chop down the size on the assignment for
      !  the horizontal dimensions.

      IF ( ( bhi( 1,1) .EQ. 2 ) .AND. &
           ( bhi( 8,1) .EQ. 1 ) .AND. &
           ( bhi(15,1) .EQ. 0 ) ) THEN
         is = 1         + bhi(11,1)
         js = 1         + bhi(12,1)
         ie = bhi(16,1) + bhi(11,1)
         je = bhi(17,1) + bhi(12,1)
      ELSE
         is = 1
         js = 1
         ie = bhi(16,1)
         je = bhi(17,1)
      END IF

      !  Assign the 3d data.

      DO loop = 1 , nump_3d
         IF      ( allp_3d(loop)%small_header%name(1:8) .EQ. 'CLW     ' ) THEN
            qc     = allp_3d(loop)%array(is:ie,js:je,:)
            CALL invert2 ( qc     , bhi(16,1) , bhi(17,1) , kx )
            index_qc = loop
         ELSE IF ( allp_3d(loop)%small_header%name(1:8) .EQ. 'RNW     ' ) THEN
            qr     = allp_3d(loop)%array(is:ie,js:je,:)
            CALL invert2 ( qr     , bhi(16,1) , bhi(17,1) , kx )
            index_qr = loop
         END IF
      END DO

      DO loop = 1 , num_2d
         IF      ( all_2d(loop)%small_header%name(1:8) .EQ. 'CLWSFC  ' ) THEN
            sfc_qc    = all_2d(loop)%array(is:ie,js:je)
            index_sfc_qc = loop
         ELSE IF ( all_2d(loop)%small_header%name(1:8) .EQ. 'RNWSFC  ' ) THEN
            sfc_qr    = all_2d(loop)%array(is:ie,js:je)
            index_sfc_qr = loop
         END IF
      END DO

   END SUBROUTINE assign_cloud_and_rain

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE assign_snow_and_ice ( qs , qi , sfc_qs , sfc_qi ,  kx ) 

      IMPLICIT NONE

      !  Input variables.

      REAL , DIMENSION(:,:,:) :: qs , qi
      REAL , DIMENSION(:,:) :: sfc_qs , sfc_qi

      INTEGER :: kx

      !  Local variables.

      INTEGER :: loop
      INTEGER :: is , js , ie , je
 
      !  If the data are expanded, we need to chop down the size on the assignment for
      !  the horizontal dimensions.

      IF ( ( bhi( 1,1) .EQ. 2 ) .AND. &
           ( bhi( 8,1) .EQ. 1 ) .AND. &
           ( bhi(15,1) .EQ. 0 ) ) THEN
         is = 1         + bhi(11,1)
         js = 1         + bhi(12,1)
         ie = bhi(16,1) + bhi(11,1)
         je = bhi(17,1) + bhi(12,1)
      ELSE
         is = 1
         js = 1
         ie = bhi(16,1)
         je = bhi(17,1)
      END IF

      !  Assign the 3d data.

      DO loop = 1 , nump_3d
         IF      ( allp_3d(loop)%small_header%name(1:8) .EQ. 'SNOW    ' ) THEN
            qs   = allp_3d(loop)%array(is:ie,js:je,:)
            CALL invert2 ( qs   , bhi(16,1) , bhi(17,1) , kx )
            index_qs = loop
         ELSE IF ( allp_3d(loop)%small_header%name(1:8) .EQ. 'ICE     ' ) THEN
            qi    = allp_3d(loop)%array(is:ie,js:je,:)
            CALL invert2 ( qi    , bhi(16,1) , bhi(17,1) , kx )
            index_qi = loop
         END IF
      END DO

      DO loop = 1 , num_2d
         IF      ( all_2d(loop)%small_header%name(1:8) .EQ. 'SNOWSFC ' ) THEN
            sfc_qs    = all_2d(loop)%array(is:ie,js:je)
            index_sfc_qs = loop
         ELSE IF ( all_2d(loop)%small_header%name(1:8) .EQ. 'ICESFC  ' ) THEN
            sfc_qi    = all_2d(loop)%array(is:ie,js:je)
            index_sfc_qi = loop
         END IF
      END DO

   END SUBROUTINE assign_snow_and_ice

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE assign_graupel ( qg , sfc_qg , kx ) 

      IMPLICIT NONE

      !  Input variables.

      REAL , DIMENSION(:,:,:) :: qg
      REAL , DIMENSION(:,:) :: sfc_qg

      INTEGER :: kx

      !  Local variables.

      INTEGER :: loop
      INTEGER :: is , js , ie , je
 
      !  If the data are expanded, we need to chop down the size on the assignment for
      !  the horizontal dimensions.

      IF ( ( bhi( 1,1) .EQ. 2 ) .AND. &
           ( bhi( 8,1) .EQ. 1 ) .AND. &
           ( bhi(15,1) .EQ. 0 ) ) THEN
         is = 1         + bhi(11,1)
         js = 1         + bhi(12,1)
         ie = bhi(16,1) + bhi(11,1)
         je = bhi(17,1) + bhi(12,1)
      ELSE
         is = 1
         js = 1
         ie = bhi(16,1)
         je = bhi(17,1)
      END IF

      !  Assign the 3d data.

      DO loop = 1 , nump_3d
         IF      ( allp_3d(loop)%small_header%name(1:8) .EQ. 'GRAUPEL ' ) THEN
            qg     = allp_3d(loop)%array(is:ie,js:je,:)
            CALL invert2 ( qg     , bhi(16,1) , bhi(17,1) , kx )
            index_qg = loop
         END IF
      END DO

      DO loop = 1 , num_2d
         IF      ( all_2d(loop)%small_header%name(1:8) .EQ. 'GRAUPELS' ) THEN
            sfc_qg    = all_2d(loop)%array(is:ie,js:je)
            index_sfc_qg = loop
         END IF
      END DO

   END SUBROUTINE assign_graupel

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE assign_number_concentration ( qn , sfc_qn , kx ) 

      IMPLICIT NONE

      !  Input variables.

      REAL , DIMENSION(:,:,:) :: qn
      REAL , DIMENSION(:,:) :: sfc_qn

      INTEGER :: kx

      !  Local variables.

      INTEGER :: loop
      INTEGER :: is , js , ie , je
 
      !  If the data are expanded, we need to chop down the size on the assignment for
      !  the horizontal dimensions.

      IF ( ( bhi( 1,1) .EQ. 2 ) .AND. &
           ( bhi( 8,1) .EQ. 1 ) .AND. &
           ( bhi(15,1) .EQ. 0 ) ) THEN
         is = 1         + bhi(11,1)
         js = 1         + bhi(12,1)
         ie = bhi(16,1) + bhi(11,1)
         je = bhi(17,1) + bhi(12,1)
      ELSE
         is = 1
         js = 1
         ie = bhi(16,1)
         je = bhi(17,1)
      END IF

      !  Assign the 3d data.

      DO loop = 1 , nump_3d
         IF      ( allp_3d(loop)%small_header%name(1:8) .EQ. 'NCI     ' ) THEN
            qn     = allp_3d(loop)%array(is:ie,js:je,:)
            CALL invert2 ( qn     , bhi(16,1) , bhi(17,1) , kx )
            index_qn = loop
         END IF
      END DO

      DO loop = 1 , num_2d
         IF      ( all_2d(loop)%small_header%name(1:8) .EQ. 'NCISFC  ' ) THEN
            sfc_qn    = all_2d(loop)%array(is:ie,js:je)
            index_sfc_qn = loop
         END IF
      END DO

   END SUBROUTINE assign_number_concentration
   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE add_lbc ( current_date19 , tseasfc , sfc_t , tseasfc_sh , sfc_t_sh , ground_t_sh , unit_lowerbc , &
                        imx , jmx , itimes , interval , less_than_24h , num_fields , iprocess , daily_mean_sst )

      IMPLICIT NONE

      CHARACTER (LEN=19)    :: current_date19
      REAL , DIMENSION(:,:) :: tseasfc , sfc_t
      TYPE(sh)              :: tseasfc_sh , sfc_t_sh , ground_t_sh
      INTEGER               :: unit_lowerbc , imx , jmx , itimes , interval
      LOGICAL               :: less_than_24h , daily_mean_sst
      INTEGER               :: num_fields , iprocess

      !  Data for small header.

      INTEGER                :: num_dims
      INTEGER , DIMENSION(4) :: start_dims
      INTEGER , DIMENSION(4) :: end_dims
      REAL                   :: xtime
      CHARACTER (LEN= 4)     :: staggering
      CHARACTER (LEN= 4)     :: ordering
      CHARACTER (LEN=24)     :: current_date
      CHARACTER (LEN= 9)     :: name
      CHARACTER (LEN=25)     :: units
      CHARACTER (LEN=46)     :: description

      INTEGER :: loop , loopmax
      INTEGER :: is , js , ie , je
      REAL , DIMENSION(imx,jmx) :: dum2d

      !  If the data are expanded, we need to chop down the size on the assignment for
      !  the horizontal dimensions.

      IF ( ( iprog_no_old .EQ. 2 ) .AND. &
           ( bhi( 8,1) .EQ. 1 ) .AND. &
           ( bhi(15,1) .EQ. 0 ) ) THEN
         is = 1         + bhi(11,1)
         js = 1         + bhi(12,1)
         ie = bhi(16,1) + bhi(11,1)
         je = bhi(17,1) + bhi(12,1)
      ELSE
         is = 1
         js = 1
         ie = bhi(16,1)
         je = bhi(17,1)
      END IF

      !  Is this the first time inside this routine?  If so, ALLOCATE the space for the
      !  fields that will be used to accumulate the sums of the various times for the
      !  mean of the sea surface temperature (tseasfc_sum) and the mean of the surface
      !  air temperature (sfct_sum).

      IF ( itimes .EQ. 1 ) THEN
         ALLOCATE ( tseasfc_sum(imx,jmx) ) 
         ALLOCATE (    sfct_sum(imx,jmx) ) 
         tseasfc_sum = 0
            sfct_sum = 0
      END IF

      !  We are now safely assured of having space, so we stuff things into the mean
      !  field arrays.  We do have to be careful to only include "full" days of info so
      !  as to minimize our temperature bias.

      IF ( .NOT. daily_mean_sst ) THEN
         loopmax = iprocess
      ELSE IF ( ( less_than_24h ) .AND. &
           ( interval * iprocess / 86400 .LT. 1 ) ) THEN
         loopmax = iprocess
      ELSE IF ( interval * iprocess / 86400 .GE. 1 ) THEN
         loopmax = ( interval * iprocess / 86400 ) * ( 86400 / interval )
      ELSE
         PRINT '(A)','You should not be allowed to be here.'
         STOP 'wrong_number_of_times'
      END IF

      IF      ( itimes .LT. loopmax ) THEN
         tseasfc_sum = tseasfc_sum + tseasfc
         sfct_sum    = sfct_sum    + sfc_t   
      ELSE IF ( itimes .EQ. loopmax ) THEN
         tseasfc_sum = tseasfc_sum + tseasfc
         sfct_sum    = sfct_sum    + sfc_t   
         tseasfc_sum = tseasfc_sum / REAL(loopmax)
         sfct_sum    = sfct_sum    / REAL(loopmax)
      END IF

      !  Most of the stuff in the ground_t_sh is the same for what we want in the tseasfc_sh
      !  and the sfc_t_sh header.

      tseasfc_sh = ground_t_sh
      sfc_t_sh   = ground_t_sh

      !  Specific things for sst small header.

      tseasfc_sh%name            = 'SST AVG  '
      tseasfc_sh%units           = 'K                        '
      tseasfc_sh%description     = 'Temporal mean of skin temperature             '

      WRITE ( unit_lowerbc ) tseasfc_sh%num_dims , tseasfc_sh%start_dims , tseasfc_sh%end_dims , &
                             tseasfc_sh%xtime , &
                             tseasfc_sh%staggering , tseasfc_sh%ordering , tseasfc_sh%current_date , &
                             tseasfc_sh%name , tseasfc_sh%units , tseasfc_sh%description
      WRITE ( unit_lowerbc ) tseasfc

      !  Specific things for surface air temperature small header.

      sfc_t_sh%name              = 'RES TEMP '
      sfc_t_sh%units             = 'K                        '
      sfc_t_sh%description       = 'Temperature of the infinite reservoir         '

      WRITE ( unit_lowerbc ) sfc_t_sh%num_dims , sfc_t_sh%start_dims , sfc_t_sh%end_dims , &
                             sfc_t_sh%xtime , &
                             sfc_t_sh%staggering , sfc_t_sh%ordering , sfc_t_sh%current_date , &
                             sfc_t_sh%name , sfc_t_sh%units , sfc_t_sh%description
      WRITE ( unit_lowerbc ) sfc_t

      !  Since we do not want to do lots of name checking, the easiest thing to
      !  do is pass all of the 2d fields through.  We have to modify the size of
      !  the arrays (possibly) to account for the expanded / unexpanded input.

      num_fields = 2
      DO loop = 1 , num_2d
         IF ( ( all_2d(loop)%small_header%name(1:8) .EQ. 'TSEASFC ' ) .OR. &
              ( all_2d(loop)%small_header%name(1:8) .EQ. 'SNOWCOVR' ) .OR. &
              ( all_2d(loop)%small_header%name(1:8) .EQ. 'SEAICE  ' ) .OR. &
              ( all_2d(loop)%small_header%name(1:8) .EQ. 'SEAICEFR' ) ) THEN
            num_fields = num_fields + 1
            num_dims        = 2
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ imx, jmx, 1, 1 /)
            xtime           = all_2d(loop)%small_header%xtime
            staggering      = all_2d(loop)%small_header%staggering
            ordering        = all_2d(loop)%small_header%ordering
            current_date    = current_date19 // '.0000'
            name            = all_2d(loop)%small_header%name
            units           = all_2d(loop)%small_header%units
            description     = all_2d(loop)%small_header%description
            WRITE ( unit_lowerbc ) num_dims , start_dims , end_dims , xtime , &
                                   staggering , ordering , current_date , name , units , description
            dum2d = all_2d(loop)%array(is:ie,js:je)
            WRITE ( unit_lowerbc ) dum2d 
         END IF
      END DO

   END SUBROUTINE add_lbc
   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE sum_lbc ( unit_lowerbc , itimes , interval , less_than_24h , &
                        start_year , start_month , start_day , start_hour , &
                        start_minute , start_second , start_frac , num_fields , &
                        imx , jmx , daily_mean_sst )

      IMPLICIT NONE

      !  Input data.

      INTEGER               :: unit_lowerbc , itimes , interval , &
                               start_year , start_month , start_day , start_hour , &
                               start_minute , start_second , start_frac , num_fields
      INTEGER               :: imx , jmx
      LOGICAL               :: less_than_24h
      LOGICAL               :: daily_mean_sst

      !  Local data.

      REAL , DIMENSION(imx,jmx) :: dum2d
      TYPE(sh)              :: sfc_sh
      INTEGER :: loop , loopmax , loop_fields
      CHARACTER (LEN=24) :: current_date
      INTEGER :: counter

      !  Read back the data from the lower boundary file and write the mean field back out 
      !  as a standard  v3 formatted file.  Since we have been writing to this unit, there is the
      !  small matter of a rewind.

      REWIND ( unit_lowerbc )
 
      !  Loop of the number of full days.  For example, if interval = 21600 s, and itimes = 9 
      !  (so, 0, 6, 12, 18, 24, 30, 36, 42, and 48h), then loop from 1 to 8.  If the user
      !  has SPECIFICALLY said "less than 24 h, please", then we acquiesce.

      IF ( .NOT. daily_mean_sst ) THEN
         loopmax = itimes
      ELSE IF ( ( less_than_24h ) .AND. &
           ( interval * itimes / 86400 .LT. 1 ) ) THEN
         loopmax = itimes
      ELSE IF ( interval * itimes / 86400 .GE. 1 ) THEN
         loopmax = ( interval * itimes / 86400 ) * ( 86400 / interval )
      ELSE
         PRINT '(A)','You should not be allowed to be here.'
         STOP 'wrong_number_of_times'
      END IF

      !  Make the big header for this file.

      WRITE ( unit_lowerbc + 1 ) bh_flag

      !  Just in case no one else cleared the big header, we should do it.  All
      !  of the header values for the lower boundary condition index can be set to -999.

      bhi (:,6) = -999
      bhr (:,6) = -999
      bhic(:,6) = '                                                                                '
      bhrc(:,6) = '                                                                                '

      !  Identify the data as the lower boundary condition for MM5.

      bhi( 1,1) = 6 ; bhic( 1,1) = 'Model lower boundary condition for MM5                                          '
 
      !  A few lines about data format and version numbers.  If we make a change in the
      !  IC INTERP output version numbers, those will now be reflected here.

      bhi( 2,6) = bhi(2,5) ; bhic( 2,6) = bhic(2,5)
      bhi( 3,6) = bhi(3,5) ; bhic( 3,6) = bhic(3,5)
      bhi( 4,6) = bhi(4,5) ; bhic( 4,6) = bhic(4,5)
      
      !  Starting date of the INTERP data, this was specified in the namelist.

      bhi ( 5,6) = start_year   
      bhic( 5,6) = 'Four-digit year of start time                                                   '
      bhi ( 6,6) = start_month
      bhic( 6,6) = 'Month of the year of the start time (1-12)                                      '
      bhi ( 7,6) = start_day
      bhic( 7,6) = 'Day of the month of the start time (1-31)                                       '
      bhi ( 8,6) = start_hour
      bhic( 8,6) = 'Hour of the day of the start time (0-23)                                        '
      bhi ( 9,6) = start_minute
      bhic( 9,6) = 'Minute of the start time (0-59)                                                 '
      bhi (10,6) = start_second
      bhic(10,6) = 'Second of the start time (0-59)                                                 '
      bhi (11,6) = start_frac
      bhic(11,6) = 'Ten thousandths of a second of the start time (0-9999)                          '

      !  How many levels to expect.

      bhi (12,6) = 1  
      bhic(12,6) = 'Number of levels in the lower boundary condition file                           '

      !  Time interval between output files.

      bhr ( 1,6) = interval
      bhrc( 1,6) = 'Time difference (seconds) during which the lower boundary condition is valid    '

      !  Output the big header.

      WRITE ( unit_lowerbc + 1 ) bhi , bhr , bhic , bhrc

      !  Loop over all of the time periods to process.

      time_loop : DO loop = 1 , loopmax

         !  For each time period, we need to process each of the lower boundary fields.

         counter = 0 
         field_loop : DO loop_fields = 1 , num_fields
         
            READ ( unit_lowerbc ) sfc_sh%num_dims , sfc_sh%start_dims , sfc_sh%end_dims , &
                                  sfc_sh%xtime , &
                                  sfc_sh%staggering , sfc_sh%ordering , sfc_sh%current_date , &
                                  sfc_sh%name , sfc_sh%units , sfc_sh%description

            READ ( unit_lowerbc ) dum2d

            !  Do we only do daily averaged values for the surface air temp and SST?
            !  If we were forced to use a skin temperature for the SST (back in REGRID),
            !  then a diurnally averaged skin temperature will be an OK estimate for SST.

            IF      ( (      daily_mean_sst ) .AND. ( sfc_sh%name(1:8) .EQ. 'SST AVG ' ) ) THEN
               WRITE ( unit_lowerbc + 1 ) sh_flag
               WRITE ( unit_lowerbc + 1 ) sfc_sh%num_dims , sfc_sh%start_dims , sfc_sh%end_dims , &
                                          sfc_sh%xtime , &
                                          sfc_sh%staggering , sfc_sh%ordering , sfc_sh%current_date , &
                                          'TSEASFC  ' , sfc_sh%units , sfc_sh%description
               WRITE ( unit_lowerbc + 1 ) tseasfc_sum

               counter = counter + 1
               PRINT '(A,I4,A,A,A,A)','Loop counter = ',counter, ', ', sfc_sh%name,', date = ',sfc_sh%current_date

            ELSE IF ( ( .NOT. daily_mean_sst ) .AND. ( sfc_sh%name(1:8) .EQ. 'SST AVG ' ) ) THEN
            ELSE IF ( (       daily_mean_sst ) .AND. ( sfc_sh%name(1:8) .EQ. 'TSEASFC ' ) ) THEN

            ELSE

               !  We are permitted to have variable SST and such.

               IF      ( sfc_sh%name(1:8) .EQ. 'RES TEMP' ) THEN
                  WRITE ( unit_lowerbc + 1 ) sh_flag
                  WRITE ( unit_lowerbc + 1 ) sfc_sh%num_dims , sfc_sh%start_dims , sfc_sh%end_dims , &
                                             sfc_sh%xtime , &
                                             sfc_sh%staggering , sfc_sh%ordering , sfc_sh%current_date , &
                                             sfc_sh%name , sfc_sh%units , sfc_sh%description
                  WRITE ( unit_lowerbc + 1 ) sfct_sum
               ELSE
                  WRITE ( unit_lowerbc + 1 ) sh_flag
                  WRITE ( unit_lowerbc + 1 ) sfc_sh%num_dims , sfc_sh%start_dims , sfc_sh%end_dims , &
                                             sfc_sh%xtime , &
                                             sfc_sh%staggering , sfc_sh%ordering , sfc_sh%current_date , &
                                             sfc_sh%name , sfc_sh%units , sfc_sh%description
                  WRITE ( unit_lowerbc + 1 ) dum2d
               END IF

               counter = counter + 1
               PRINT '(A,I4,A,A,A,A)','Loop counter = ',counter, ', ', sfc_sh%name,', date = ',sfc_sh%current_date

            END IF

         END DO field_loop

         !  Finish this time period with an end of time flag.

         WRITE ( unit_lowerbc + 1 ) eot_flag

      END DO time_loop

   END SUBROUTINE sum_lbc
   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE sum_lbc2 ( unit_lowerbc , itimes , interval , less_than_24h , &
                        start_year , start_month , start_day , start_hour , &
                        start_minute , start_second , start_frac , num_fields , &
                        imx , jmx , daily_mean_sst )

      IMPLICIT NONE

      !  Input data.

      INTEGER               :: unit_lowerbc , itimes , interval , &
                               start_year , start_month , start_day , start_hour , &
                               start_minute , start_second , start_frac , num_fields
      INTEGER               :: imx , jmx
      LOGICAL               :: less_than_24h
      LOGICAL               :: daily_mean_sst

      !  Local data.

      REAL , DIMENSION(imx,jmx) :: dum2d
      TYPE(sh)              :: sfc_sh
      INTEGER :: loop , loopmax , loop_fields
      CHARACTER (LEN=24) :: current_date
      
      !  Read back the data from the lower boundary file and write the mean field back out 
      !  as a standard  v3 formatted file.  Since we have been writing to this unit, there is the
      !  small matter of a rewind.

      REWIND ( unit_lowerbc )
 
      !  Loop of the number of full days.  For example, if interval = 21600 s, and itimes = 9 
      !  (so, 0, 6, 12, 18, 24, 30, 36, 42, and 48h), then loop from 1 to 8.  If the user
      !  has SPECIFICALLY said "less than 24 h, please", then we acquiesce.

      IF ( .NOT. daily_mean_sst ) THEN
         loopmax = itimes
      ELSE IF ( ( less_than_24h ) .AND. &
           ( interval * itimes / 86400 .LT. 1 ) ) THEN
         loopmax = itimes
      ELSE IF ( interval * itimes / 86400 .GE. 1 ) THEN
         loopmax = ( interval * itimes / 86400 ) * ( 86400 / interval )
      ELSE
         PRINT '(A)','You should not be allowed to be here.'
         STOP 'wrong_number_of_times'
      END IF

      !  Make the big header for this file.

      WRITE ( unit_lowerbc + 1 ) bh_flag

      !  Just in case no one else cleared the big header, we should do it.  All
      !  of the header values for the lower boundary condition index can be set to -999.

      bhi (:,6) = -999
      bhr (:,6) = -999
      bhic(:,6) = '                                                                                '
      bhrc(:,6) = '                                                                                '

      !  Identify the data as the lower boundary condition for MM5.

      bhi( 1,1) = 6 ; bhic( 1,1) = 'Model lower boundary condition for MM5                                          '
 
      !  A few lines about data format and version numbers.  If we make a change in the
      !  IC INTERP output version numbers, those will now be reflected here.

      bhi( 2,6) = bhi(2,5) ; bhic( 2,6) = bhic(2,5)
      bhi( 3,6) = bhi(3,5) ; bhic( 3,6) = bhic(3,5)
      bhi( 4,6) = bhi(4,5) ; bhic( 4,6) = bhic(4,5)
      
      !  Starting date of the INTERP data, this was specified in the namelist.

      bhi ( 5,6) = start_year   
      bhic( 5,6) = 'Four-digit year of start time                                                   '
      bhi ( 6,6) = start_month
      bhic( 6,6) = 'Month of the year of the start time (1-12)                                      '
      bhi ( 7,6) = start_day
      bhic( 7,6) = 'Day of the month of the start time (1-31)                                       '
      bhi ( 8,6) = start_hour
      bhic( 8,6) = 'Hour of the day of the start time (0-23)                                        '
      bhi ( 9,6) = start_minute
      bhic( 9,6) = 'Minute of the start time (0-59)                                                 '
      bhi (10,6) = start_second
      bhic(10,6) = 'Second of the start time (0-59)                                                 '
      bhi (11,6) = start_frac
      bhic(11,6) = 'Ten thousandths of a second of the start time (0-9999)                          '

      !  How many levels to expect.

      bhi (12,6) = 1  
      bhic(12,6) = 'Number of levels in the lower boundary condition file                           '

      !  Time interval between output files.

      IF ( daily_mean_sst ) THEN
         bhr ( 1,6) = interval * loopmax
      ELSE
         bhr ( 1,6) = interval
      END IF
      bhrc( 1,6) = 'Time difference (seconds) during which the lower boundary condition is valid    '

      !  Output the big header.

      WRITE ( unit_lowerbc + 1 ) bhi , bhr , bhic , bhrc

      !  Loop over all of the time periods to process (full days, unless specifically requested
      !  to permit partial days in the mean fields).

      IF ( daily_mean_sst ) THEN
         loopmax = 1
      END IF

      time_loop : DO loop = 1 , loopmax

         !  For each time period, we need to process each of the lower boundary fields.

         field_loop : DO loop_fields = 1 , num_fields
         
            READ ( unit_lowerbc ) sfc_sh%num_dims , sfc_sh%start_dims , sfc_sh%end_dims , &
                                  sfc_sh%xtime , &
                                  sfc_sh%staggering , sfc_sh%ordering , sfc_sh%current_date , &
                                  sfc_sh%name , sfc_sh%units , sfc_sh%description

            READ ( unit_lowerbc ) dum2d
            PRINT '(A,I4,A,I2,A,A,A,A)','Loop counter = ',loop,', field num = ',loop_fields,', ', &
                                        sfc_sh%name,', date = ',sfc_sh%current_date

            !  Do we only do daily averaged values for the surface air temp and SST?
            !  If we were forced to use a skin temperature for the SST (back in REGRID),
            !  then a diurnally averaged skin temperature will be an OK estimate for SST.

            IF ( daily_mean_sst ) THEN

               !  We want only daily averaged values for SST.

               IF      ( sfc_sh%name(1:8) .EQ. 'SST AVG ' ) THEN
                  WRITE ( unit_lowerbc + 1 ) sh_flag
                  WRITE ( unit_lowerbc + 1 ) sfc_sh%num_dims , sfc_sh%start_dims , sfc_sh%end_dims , &
                                             sfc_sh%xtime , &
                                             sfc_sh%staggering , sfc_sh%ordering , sfc_sh%current_date , &
                                             'TSEASFC  ' , sfc_sh%units , sfc_sh%description
                  WRITE ( unit_lowerbc + 1 ) tseasfc_sum
               ELSE IF ( sfc_sh%name(1:8) .EQ. 'RES TEMP' ) THEN
                  WRITE ( unit_lowerbc + 1 ) sh_flag
                  WRITE ( unit_lowerbc + 1 ) sfc_sh%num_dims , sfc_sh%start_dims , sfc_sh%end_dims , &
                                             sfc_sh%xtime , &
                                             sfc_sh%staggering , sfc_sh%ordering , sfc_sh%current_date , &
                                             sfc_sh%name , sfc_sh%units , sfc_sh%description
                  WRITE ( unit_lowerbc + 1 ) sfct_sum
               END IF

            ELSE

               !  We are permitted to have variable SST and such.

               IF      ( sfc_sh%name(1:8) .EQ. 'SST AVG ' ) THEN
                  PRINT '(A)','Skipping daily averaged SST in LOWBDY file, since SST is available instead of skin temperature.'
               ELSE IF ( sfc_sh%name(1:8) .EQ. 'RES TEMP' ) THEN
                  WRITE ( unit_lowerbc + 1 ) sh_flag
                  WRITE ( unit_lowerbc + 1 ) sfc_sh%num_dims , sfc_sh%start_dims , sfc_sh%end_dims , &
                                             sfc_sh%xtime , &
                                             sfc_sh%staggering , sfc_sh%ordering , sfc_sh%current_date , &
                                             sfc_sh%name , sfc_sh%units , sfc_sh%description
                  WRITE ( unit_lowerbc + 1 ) sfct_sum
               ELSE
                  WRITE ( unit_lowerbc + 1 ) sh_flag
                  WRITE ( unit_lowerbc + 1 ) sfc_sh%num_dims , sfc_sh%start_dims , sfc_sh%end_dims , &
                                             sfc_sh%xtime , &
                                             sfc_sh%staggering , sfc_sh%ordering , sfc_sh%current_date , &
                                             sfc_sh%name , sfc_sh%units , sfc_sh%description
                  WRITE ( unit_lowerbc + 1 ) dum2d
               END IF
            END IF

         END DO field_loop

         !  Finish this time period with an end of time flag.

         WRITE ( unit_lowerbc + 1 ) eot_flag

      END DO time_loop

   END SUBROUTINE sum_lbc2
   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE do_namelist ( input_file ,  &
                            start_year , start_month , start_day , start_hour , &
                            start_minute , start_second , start_frac , &
                            end_year ,   end_month ,   end_day ,   end_hour , &
                            end_minute ,   end_second ,   end_frac , &
                            interval ,  less_than_24h , &
                            sigma_f_bu , ptop , isfc ,  &
                            p0, tlp , ts0 , tiso , &
                            removediv , usesfc , wrth2o ,  &
!KWM START
                            psfc_method ,  &
!KWM END
                            ifdatim )

      IMPLICIT NONE

      !  Input variables.
   
      !  RECORD0
      
      CHARACTER(LEN=132) , DIMENSION(100) :: input_file
      NAMELIST /RECORD0/ input_file
      
      !  RECORD1
      
      INTEGER :: start_year , start_month , start_day , start_hour , &
                 start_minute , start_second , start_frac
      INTEGER ::   end_year ,   end_month ,   end_day ,   end_hour , &
                 end_minute ,   end_second ,   end_frac
      INTEGER :: interval
      LOGICAL :: less_than_24h

      NAMELIST /RECORD1/ start_year , start_month , start_day ,   &
                         start_hour ,  start_minute , start_second , start_frac ,  &
                         end_year ,   end_month ,   end_day ,     &
                         end_hour ,  end_minute ,   end_second ,   end_frac ,  &
                         interval , less_than_24h
      
      !  RECORD2
      
      REAL , DIMENSION(1000) :: sigma_f_bu
      REAL                   :: ptop
      INTEGER                :: isfc

      NAMELIST /RECORD2/ sigma_f_bu , ptop , isfc
      
      !  RECORD3
      
      REAL :: p0 , tlp , ts0 , tiso

      NAMELIST /RECORD3/ p0, tlp , ts0 , tiso
            
      !  RECORD4
      
      LOGICAL :: removediv
      LOGICAL :: usesfc   
      LOGICAL :: wrth2o   
      INTEGER :: psfc_method

      NAMELIST /RECORD4/ removediv, usesfc, wrth2o, psfc_method
      
      !  RECORD5
      
      INTEGER :: ifdatim

      NAMELIST /RECORD5/ ifdatim 

      !  Local variables.

      LOGICAL :: is_it_there = .FALSE.
      INTEGER , PARAMETER :: unit_nml=10

      !  Initialize the variable "input_file" to blank

      input_file = '                                                                                ' // &
                   '                                                    '

      !  Default value for variable "psfc_method"
      
      psfc_method = 0
      
      !     Does the file exist?
      
      INQUIRE ( FILE = 'namelist.input' , EXIST = is_it_there )
      
      IF ( is_it_there ) THEN
      
         !  The file exists, get a unit number.
      
         OPEN ( FILE   = 'namelist.input' , &
                UNIT   =  unit_nml        , &
                STATUS = 'OLD'            , &
                FORM   = 'FORMATTED'      , &
                ACTION = 'READ'           , &
                ACCESS = 'SEQUENTIAL'     )
   
         !  File is opened, so read it.
   
         READ (unit_nml , NML = RECORD0 )
!        WRITE (6    , NML = RECORD0 )
         READ (unit_nml , NML = RECORD1 )
         WRITE (6    , NML = RECORD1 )
         READ (unit_nml , NML = RECORD2 )
#ifndef IBM
         WRITE (6    , NML = RECORD2 )
#endif
         READ (unit_nml , NML = RECORD3 )
         WRITE (6    , NML = RECORD3 )
         READ (unit_nml , NML = RECORD4 )
         WRITE (6    , NML = RECORD4 )
         READ (unit_nml , NML = RECORD5 )
         WRITE (6    , NML = RECORD5 )
   
      ELSE
         PRINT '(A)','Could not find the namelist: "namelist.input".'
         STOP 'No_namelist_found'
      END IF
   
      CLOSE ( unit_nml )
   
   END SUBROUTINE do_namelist

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
   SUBROUTINE get_fg_file ( input_file , unit_fg )
   
      IMPLICIT NONE
   
      !  Input variables.

      CHARACTER(LEN=132) , INTENT(IN) :: input_file

      INTEGER , INTENT(IN) :: unit_fg

      !  Local variables.

      LOGICAL :: is_it_there = .FALSE.
   
   
      !  Does the file exist?
   
      INQUIRE ( FILE = TRIM(input_file) , EXIST = is_it_there )
   
      IF ( is_it_there ) THEN
         OPEN ( FILE   = TRIM(input_file) , &
                UNIT   =  unit_fg         ,  &
                STATUS = 'OLD'            , &
                FORM   = 'UNFORMATTED'    , &
                ACCESS = 'SEQUENTIAL'     )
         PRINT '(A)','Opened pressure level data for input.'
      
      ELSE
         PRINT '(A,A,A)','Could not find file ',TRIM(input_file),'.'
         STOP 'No_fg_file_found'
      
      END IF
   
   END SUBROUTINE get_fg_file
   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
   SUBROUTINE open_out_file ( mm_unit , bdy_unit , lbc_unit , domain_id )
   
      IMPLICIT NONE
      
      !  Input variables.

      INTEGER , INTENT(IN) :: mm_unit , bdy_unit , lbc_unit , domain_id
      
      !  Local variables

      LOGICAL :: is_it_there = .FALSE.
      CHARACTER(LEN=132) :: mm_name , bdy_name , lbc_name

      WRITE (  mm_name , '("MMINPUT_DOMAIN",i1)' ) domain_id
      WRITE ( bdy_name ,  '("BDYOUT_DOMAIN",i1)' ) domain_id
      WRITE ( lbc_name , ' ("LOWBDY_DOMAIN",i1)' ) domain_id
      
      OPEN ( FILE   =  TRIM(mm_name)   , &
             UNIT   =  mm_unit         , &
             STATUS = 'UNKNOWN'        , &
             FORM   = 'UNFORMATTED'    , &
             ACCESS = 'SEQUENTIAL'     )
             PRINT '(A)','Opened MMINPUT file for output.'

      OPEN ( FILE   =  TRIM(bdy_name)  , &
             UNIT   =  bdy_unit        , &
             STATUS = 'UNKNOWN'        , &
             FORM   = 'UNFORMATTED'    , &
             ACCESS = 'SEQUENTIAL'     )
             PRINT '(A)','Opened BDYOUT file for output.'

      OPEN ( FILE   = '.foo'           , & 
             UNIT   =  lbc_unit        , &
             STATUS = 'UNKNOWN'        , &
             FORM   = 'UNFORMATTED'    , &
             ACCESS = 'SEQUENTIAL'     )
             PRINT '(A)','Opened scratch LOWBDY file for output.'

      OPEN ( FILE   =  TRIM(lbc_name)  , &
             UNIT   =  lbc_unit + 1    , &
             STATUS = 'UNKNOWN'        , &
             FORM   = 'UNFORMATTED'    , &
             ACCESS = 'SEQUENTIAL'     )
             PRINT '(A)','Opened LOWBDY file for output.'

   END SUBROUTINE open_out_file

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE outmodel ( current_date19 , &
                         imx , jmx , kxs , immout , itimes , interval , &
                         start_year , start_month , start_day , start_hour , &
                         start_minute , start_second , start_frac , &
                         p0 , tlp , ts0 , tiso, &
                         sighup , &
                         ps0 , ground_t , ground_t_sh , &
                         pp_sig , q_sig , t_sig , u_sig , v_sig , w , w_top , &
                         cloud_and_rain , snow_and_ice , graupel , number_concentration )

      IMPLICIT NONE

      !  Input data.

      CHARACTER (LEN=19)      :: current_date19

      INTEGER                 :: imx , jmx , kxs , immout , itimes , interval , &
                                 start_year , start_month , start_day , start_hour , &
                                 start_minute , start_second , start_frac

      LOGICAL , INTENT(IN) :: cloud_and_rain , snow_and_ice , graupel , number_concentration

      REAL                    :: p0 , tlp , ts0, tiso

      REAL , DIMENSION(:)     :: sighup

      REAL , DIMENSION(:,:)   :: ps0 , ground_t

      REAL , DIMENSION(:,:,:) :: pp_sig , q_sig , t_sig , u_sig , v_sig , w
      REAL , DIMENSION(:,:)   :: w_top

      !  Output data.

      TYPE(sh) :: ground_t_sh

      !  Local data.

      REAL , DIMENSION(imx,jmx,kxs+1) :: w_sig

      INTEGER :: loop

      !  Variables for expanded domain cut down for the pass-through
      !  2d arrays.

      INTEGER :: is , js , ie , je
      REAL , DIMENSION(imx,jmx) :: dum2d

      !  Data for small header.

      INTEGER                :: num_dims
      INTEGER , DIMENSION(4) :: start_dims
      INTEGER , DIMENSION(4) :: end_dims
      REAL                   :: xtime
      CHARACTER (LEN= 4)     :: staggering
      CHARACTER (LEN= 4)     :: ordering
      CHARACTER (LEN=24)     :: current_date
      CHARACTER (LEN= 9)     :: name
      CHARACTER (LEN=25)     :: units
      CHARACTER (LEN=46)     :: description

      !  If the data are expanded, we need to chop down the size on the assignment for
      !  the horizontal dimensions.

      IF ( ( bhi( 1,1) .EQ. 2 ) .AND. &
           ( bhi( 8,1) .EQ. 1 ) .AND. &
           ( bhi(15,1) .EQ. 0 ) ) THEN
         is = 1         + bhi(11,1)
         js = 1         + bhi(12,1)
         ie = bhi(16,1) + bhi(11,1)
         je = bhi(17,1) + bhi(12,1)
      ELSE
         is = 1
         js = 1
         ie = bhi(16,1)
         je = bhi(17,1)
      END IF

      !  Put w + w_top = w_sig

      w_sig(:,:,2:kxs+1) = w
      w_sig(:,:,1      ) = w_top

      !  If this is the first time in here, then we need to build and output 
      !  the big header.

      IF ( itimes .EQ. 1 ) THEN

         !  First, the flag, then the data.

         WRITE ( immout ) bh_flag

         !  Just in case no one else cleared the big header, we should do it.  All
         !  of the header values for the model input index can be set to -999.

         bhi (:,5) = -999
         bhr (:,5) = -999
         bhic(:,5) = '                                                                                '
         bhrc(:,5) = '                                                                                '

         !  Identify the data as model input.

         bhi( 1,1) = 5 ; bhic( 1,1) = 'Model meteorological initial condition for MM5                                  '
 
         !  A few lines about data format and version numbers.

         bhi( 2,5) = 1 ; bhic( 2,5) = 'INTERP Version 3 MM5 System Format Edition Number                               '
         bhi( 3,5) = 7 ; bhic( 3,5) = 'INTERP Program Version Number                                                   '
         bhi( 4,5) = 0 ; bhic( 4,5) = 'INTERP Program Minor Revision Number                                            '
         
         !  Starting date of the INTERP data, this was specified in the namelist.

         bhi ( 5,5) = start_year   
         bhic( 5,5) = 'Four-digit year of start time                                                   '
         bhi ( 6,5) = start_month
         bhic( 6,5) = 'Month of the year of the start time (1-12)                                      '
         bhi ( 7,5) = start_day
         bhic( 7,5) = 'Day of the month of the start time (1-31)                                       '
         bhi ( 8,5) = start_hour
         bhic( 8,5) = 'Hour of the day of the start time (0-23)                                        '
         bhi ( 9,5) = start_minute
         bhic( 9,5) = 'Minute of the start time (0-59)                                                 '
         bhi (10,5) = start_second
         bhic(10,5) = 'Second of the start time (0-59)                                                 '
         bhi (11,5) = start_frac
         bhic(11,5) = 'Ten thousandths of a second of the start time (0-9999)                          '

         !  How many half sigma levels to expect.

         bhi (12,5) = kxs
         bhic(12,5) = 'Number of half-sigma layers in the model input data (top down)                  '

         !  Time interval between output files.

         bhr ( 1,5) = interval
         bhrc( 1,5) = 'Time difference (seconds) between model IC input files                          '

         !  Base state information.

         bhr ( 2,5) = p0
         bhrc( 2,5) = 'Non-hydrostatic base state sea-level pressure (Pa)                              '
         bhr ( 3,5) = ts0
         bhrc( 3,5) = 'Non-hydrostatic base state sea-level temperature (K)                            '
         bhr ( 4,5) = tlp
         bhrc( 4,5) = 'Non-hydrostatic base state lapse rate d(T)/d(ln P)                              '
         bhr ( 5,5) = tiso
         bhrc( 5,5) = 'Non-hydrostatic base state isothermal stratospheric temperature (K)             '

         !  Alrighty, we can output it now.
  
         WRITE ( immout ) bhi , bhr , bhic , bhrc
         
      END IF

      !  Triplets of data follow: 1) small head flag, 2) small header, 3) data.
      !  Each triplet is for a single array (3d, 2d, or 1d).  We will only output
      !  the processed 3d fields, regardless of how many unusual and special
      !  3d fields were originally input.  The 2d and 1d data is a different matter.
      !  Since it doesn't have to be interpolated, we will just run everything
      !  through that we find.

      !  Temperature.

      WRITE ( immout ) sh_flag
      num_dims        = 3
      start_dims      = (/ 1, 1, 1, 1 /)
      end_dims        = (/ imx, jmx, kxs, 1 /)
      xtime           = 0
      staggering      = 'C   '
      ordering        = 'YXS '
      current_date    = current_date19 // '.0000'
      name            = 'T        '
      units           = 'K                        '
      description     = 'Temperature                                   '
      WRITE ( immout ) num_dims , start_dims , end_dims , xtime , &
                       staggering , ordering , current_date , name , units , description
      WRITE ( immout ) t_sig

      !  U-component of wind.

      WRITE ( immout ) sh_flag
      num_dims        = 3
      start_dims      = (/ 1, 1, 1, 1 /)
      end_dims        = (/ imx, jmx, kxs, 1 /)
      xtime           = 0
      staggering      = 'D   '
      ordering        = 'YXS '
      current_date    = current_date19 // '.0000'
      name            = 'U        '
      units           = 'm/s                      '
      description     = 'U-component of velocity                       '
      WRITE ( immout ) num_dims , start_dims , end_dims , xtime , &
                       staggering , ordering , current_date , name , units , description
      WRITE ( immout ) u_sig

      !  V-component of wind.

      WRITE ( immout ) sh_flag
      num_dims        = 3
      start_dims      = (/ 1, 1, 1, 1 /)
      end_dims        = (/ imx, jmx, kxs, 1 /)
      xtime           = 0
      staggering      = 'D   '
      ordering        = 'YXS '
      current_date    = current_date19 // '.0000'
      name            = 'V        '
      units           = 'm/s                      '
      description     = 'V-component of velocity                       '
      WRITE ( immout ) num_dims , start_dims , end_dims , xtime , &
                       staggering , ordering , current_date , name , units , description
      WRITE ( immout ) v_sig

      !  Water vapor mixing ratio

      WRITE ( immout ) sh_flag
      num_dims        = 3
      start_dims      = (/ 1, 1, 1, 1 /)
      end_dims        = (/ imx, jmx, kxs, 1 /)
      xtime           = 0
      staggering      = 'C   '
      ordering        = 'YXS '
      current_date    = current_date19 // '.0000'
      name            = 'Q        '
      units           = 'kg/kg                    '
      description     = 'Water vapor mixing ratio                      '
      WRITE ( immout ) num_dims , start_dims , end_dims , xtime , &
                       staggering , ordering , current_date , name , units , description
      WRITE ( immout ) q_sig

      !  Pressure perturbation.

      WRITE ( immout ) sh_flag
      num_dims        = 3
      start_dims      = (/ 1, 1, 1, 1 /)
      end_dims        = (/ imx, jmx, kxs, 1 /)
      xtime           = 0
      staggering      = 'C   '
      ordering        = 'YXS '
      current_date    = current_date19 // '.0000'
      name            = 'PP       '
      units           = 'Pa                       '
      description     = 'Pressure perturbation                         '
      WRITE ( immout ) num_dims , start_dims , end_dims , xtime , &
                       staggering , ordering , current_date , name , units , description
      WRITE ( immout ) pp_sig

      !  Vertical velocity.

      WRITE ( immout ) sh_flag
      num_dims        = 3
      start_dims      = (/ 1, 1, 1, 1 /)
      end_dims        = (/ imx, jmx, kxs+1, 1 /)
      xtime           = 0
      staggering      = 'C   '
      ordering        = 'YXW '
      current_date    = current_date19 // '.0000'
      name            = 'W        '
      units           = 'm/s                      '
      description     = 'Vertical velocity                             '
      WRITE ( immout ) num_dims , start_dims , end_dims , xtime , &
                       staggering , ordering , current_date , name , units , description
      WRITE ( immout ) w_sig
    
      !  Cloud and rain water.
  
      IF ( cloud_and_rain ) THEN

         WRITE ( immout ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, jmx, kxs, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'YXW '
         current_date    = current_date19 // '.0000'
         name            = 'CLW      '
         units           = 'kg/kg                    '
         description     = 'Cloud Water                                   '
         WRITE ( immout ) num_dims , start_dims , end_dims , xtime , &
                          staggering , ordering , current_date , name , units , description
         WRITE ( immout ) allm_3d(1)%array

         WRITE ( immout ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, jmx, kxs, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'YXW '
         current_date    = current_date19 // '.0000'
         name            = 'RNW      '
         units           = 'kg/kg                    '
         description     = 'Rain Water                                    '
         WRITE ( immout ) num_dims , start_dims , end_dims , xtime , &
                          staggering , ordering , current_date , name , units , description
         WRITE ( immout ) allm_3d(2)%array
 
      END IF
    
      !  Snow and ice.
  
      IF ( cloud_and_rain .AND. snow_and_ice ) THEN

         WRITE ( immout ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, jmx, kxs, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'YXW '
         current_date    = current_date19 // '.0000'
         name            = 'SNOW     '
         units           = 'kg/kg                    '
         description     = 'Cloud Snow                                    '
         WRITE ( immout ) num_dims , start_dims , end_dims , xtime , &
                          staggering , ordering , current_date , name , units , description
         WRITE ( immout ) allm_3d(3)%array

         WRITE ( immout ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, jmx, kxs, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'YXW '
         current_date    = current_date19 // '.0000'
         name            = 'ICE      '
         units           = 'kg/kg                    '
         description     = 'Cloud Ice                                     '
         WRITE ( immout ) num_dims , start_dims , end_dims , xtime , &
                          staggering , ordering , current_date , name , units , description
         WRITE ( immout ) allm_3d(4)%array
 
      END IF
    
      !  Graupel
  
      IF ( cloud_and_rain .AND. snow_and_ice .AND. graupel ) THEN

         WRITE ( immout ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, jmx, kxs, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'YXW '
         current_date    = current_date19 // '.0000'
         name            = 'GRAUPEL  '
         units           = 'kg/kg                    '
         description     = 'Graupel                                       '
         WRITE ( immout ) num_dims , start_dims , end_dims , xtime , &
                          staggering , ordering , current_date , name , units , description
         WRITE ( immout ) allm_3d(5)%array

      END IF
    
      !  Number concentration.
  
      IF ( cloud_and_rain .AND. snow_and_ice .AND. graupel .AND. number_concentration ) THEN

         WRITE ( immout ) sh_flag
         num_dims        = 3
         start_dims      = (/ 1, 1, 1, 1 /)
         end_dims        = (/ imx, jmx, kxs, 1 /)
         xtime           = 0
         staggering      = 'C   '
         ordering        = 'YXW '
         current_date    = current_date19 // '.0000'
         name            = 'NCI      '
         units           = '#/m^3                    '
         description     = 'Number Concentration of Ice                   '
         WRITE ( immout ) num_dims , start_dims , end_dims , xtime , &
                          staggering , ordering , current_date , name , units , description
         WRITE ( immout ) allm_3d(6)%array

      END IF

      !  Output the "new" 2d fields.  The only 2d fields created in the INTERP program
      !  are the ground temperature and the non-hydrostatic pstar (the non-hydrostatic
      !  surface pressure - ptop).

      !  Ground temperature.

      WRITE ( immout ) sh_flag
      num_dims        = 2
      start_dims      = (/ 1, 1, 1, 1 /)
      end_dims        = (/ imx, jmx, 1, 1 /)
      xtime           = 0
      staggering      = 'C   '
      ordering        = 'YX  '
      current_date    = current_date19 // '.0000'
      name            = 'GROUND T '
      units           = 'K                        '
      description     = 'Ground temperature from Tsfc, SST and landmask'
      WRITE ( immout ) num_dims , start_dims , end_dims , xtime , &
                       staggering , ordering , current_date , name , units , description
      WRITE ( immout ) ground_t
      ground_t_sh%num_dims        = 2
      ground_t_sh%start_dims      = (/ 1, 1, 1, 1 /)
      ground_t_sh%end_dims        = (/ imx, jmx, 1, 1 /)
      ground_t_sh%xtime           = 0
      ground_t_sh%staggering      = 'C   '
      ground_t_sh%ordering        = 'YX  '
      ground_t_sh%current_date    = current_date19 // '.0000'
      ground_t_sh%name            = 'GROUND T '
      ground_t_sh%units           = 'K                        '
      ground_t_sh%description     = 'Ground temperature from Tsfc, SST and landmask'


      !  Non-hydrostatic pstar.

      WRITE ( immout ) sh_flag
      num_dims        = 2
      start_dims      = (/ 1, 1, 1, 1 /)
      end_dims        = (/ imx, jmx, 1, 1 /)
      xtime           = 0
      staggering      = 'C   '
      ordering        = 'YX  '
      current_date    = current_date19 // '.0000'
      name            = 'PSTARCRS '
      units           = 'Pa                       '
      description     = 'Non-hydrostatic p*                            '
      WRITE ( immout ) num_dims , start_dims , end_dims , xtime , &
                       staggering , ordering , current_date , name , units , description
      WRITE ( immout ) ps0

      !  Since we do not want to do lots of name checking, the easiest thing to
      !  do is pass all of the 2d fields through.  We have to modify the size of
      !  the arrays (possibly) to account for the expanded / unexpanded input.

      DO loop = 1 , num_2d
         IF ( all_2d(loop)%small_header%ordering(1:2) .EQ. 'YX' ) THEN
            WRITE ( immout ) sh_flag
            num_dims        = 2
            start_dims      = (/ 1, 1, 1, 1 /)
            end_dims        = (/ imx, jmx, 1, 1 /)
            xtime           = all_2d(loop)%small_header%xtime
            staggering      = all_2d(loop)%small_header%staggering
            ordering        = all_2d(loop)%small_header%ordering
            current_date    = current_date19 // '.0000'
            name            = all_2d(loop)%small_header%name
            units           = all_2d(loop)%small_header%units
            description     = all_2d(loop)%small_header%description
            WRITE ( immout ) num_dims , start_dims , end_dims , xtime , &
                             staggering , ordering , current_date , name , units , description
            dum2d = all_2d(loop)%array(is:ie,js:je)
            WRITE ( immout ) dum2d 
         END IF
      END DO

      !  There are two important 1D arrays: sigma and pressure.  The array
      !  created inside INTERP is sigma (top-down, half layers).  The pressure
      !  array is bottom up, and includes the "surface" level.

      !  Half sigma.

      WRITE ( immout ) sh_flag
      num_dims        = 1
      start_dims      = (/ 1, 1, 1, 1 /)
      end_dims        = (/ kxs, 1, 1, 1 /)
      xtime           = 0
      staggering      = 'H   '
      ordering        = 'S   '
      current_date    = current_date19 // '.0000'
      name            = 'SIGMAH   '
      units           = 'sigma                    '
      description     = 'Top-down, half sigma layers                   '
      WRITE ( immout ) num_dims , start_dims , end_dims , xtime , &
                       staggering , ordering , current_date , name , units , description
      WRITE ( immout ) sighup

      !  Pressure

      WRITE ( immout ) sh_flag
      num_dims        = 1
      start_dims      = (/ 1, 1, 1, 1 /)
      end_dims        = all_1d(1)%small_header%end_dims
      xtime           = 0
      staggering      = 'P   '
      ordering        = 'P   '
      current_date    = current_date19 // '.0000'
      name            = 'PRESSURE '
      units           = 'Pa                       '
      description     = 'Bottom-up pressure levels, includes surface   '
      WRITE ( immout ) num_dims , start_dims , end_dims , xtime , &
                       staggering , ordering , current_date , name , units , description
      WRITE ( immout ) all_1d(1)%array
      
      !  The last thing you do before going to sleep at night is to put out the 
      !  end of time flag.

      WRITE ( immout ) eot_flag

   END SUBROUTINE outmodel

!KWM START
   SUBROUTINE average_surface_T_over_time ( input_file , imx , jmx , kxs, avgsfct )
!
! Time average of the surface air T from the pressure-level input file.  This subroutine
! should be called only if "less_than_24h" is .FALSE.  To keep from introducing a bias
! when the number of time periods do not fit cleanly into days (for example, a data set with
! times 00Z, 12Z and 00Z will have a strong bias for the 00Z conditions if all three times
! go into the average) the data are first read into a holding array, and when that holding 
! array has one day's worth of data, those data are summed and added to the average array.
!
     IMPLICIT NONE
     CHARACTER ( LEN = 132 ) , DIMENSION ( 100 ) , INTENT( in ) :: input_file
     INTEGER , INTENT( in )                                     :: imx
     INTEGER , INTENT( in )                                     :: jmx
     INTEGER , INTENT( in )                                     :: kxs
     REAL    , DIMENSION ( imx , jmx ) , INTENT ( out )         :: avgsfct
     INTEGER                                                    :: flag
     INTEGER                                                    :: idum
     INTEGER                                                    :: ierr
     INTEGER                                                    :: i
     INTEGER                                                    :: icount
     INTEGER                                                    :: dcount
     INTEGER                                                    :: file_count
     INTEGER                                                    :: idts
     INTEGER                                                    :: intv
     INTEGER                                                    :: last_intv
     REAL                                                       :: xdum
     INTEGER , PARAMETER                                        :: nhold = 48
     INTEGER , PARAMETER                                        :: iunit = 60
     REAL    , DIMENSION ( imx , jmx , nhold )                  :: holdt
     CHARACTER ( len = 24 )                                     :: startdate
     CHARACTER ( len = 24 )                                     :: lastdate
     LOGICAL                                                    :: is_it_there
     REAL    , ALLOCATABLE, DIMENSION ( : , : )                 :: tmp2d

     startdate = "0000-00-00_00:00:00.0000"
     avgsfct = 0.

     file_loop : DO file_count = 1, 100

        IF ( input_file ( file_count ) == "" ) THEN
           EXIT file_loop
        END IF

        INQUIRE ( FILE = TRIM ( input_file ( file_count ) ) , EXIST = is_it_there )

        IF ( .NOT. is_it_there ) THEN
           EXIT file_loop
        END IF

        OPEN ( FILE   = TRIM ( input_file ( file_count ) ) , &
             UNIT   =  iunit                            , &
             STATUS = 'OLD'                             , &
             FORM   = 'UNFORMATTED'                     , &
             ACCESS = 'SEQUENTIAL'                      , &
             ACTION = 'READ'                            )

print *, 'opening file = ', input_file ( file_count )
print *, 'imx, jmx, nhold = ',imx, jmx, nhold

        var_loop : DO

           READ ( iunit , iostat=ierr ) flag
           IF ( ierr /= 0 ) EXIT var_loop

           IF ( flag == bh_flag ) THEN

              ! Just skip the big_header info
              READ ( iunit ) idum

           ELSE IF ( flag == sh_flag ) THEN

              READ ( iunit ) small_header%num_dims , small_header%start_dims , small_header%end_dims , &
                   small_header%xtime , small_header%staggering , small_header%ordering , &
                   small_header%current_date , small_header%name , small_header%units , &
                   small_header%description

              IF ( small_header%name == "T" ) THEN
                 PRINT * , " Read time " , small_header%current_date ( 1 : 19 )

                 IF ( startdate ( 1 : 4 ) == "0000" ) THEN

                    startdate = small_header%current_date
                    lastdate = startdate
                    holdt = 0.0
                    idts = 0
                    intv = 0
                    icount = 0
                    dcount = 0

                 ELSE

                    last_intv = intv
                    CALL geth_idts ( small_header%current_date ( 1 : 19 ) , startdate ( 1 : 19 ) , idts )
                    CALL geth_idts ( small_header%current_date ( 1 : 19 ) ,  lastdate ( 1 : 19 ) , intv )

                    IF ( ( last_intv > 0 ) .AND. ( intv /= last_intv ) ) THEN

                       PRINT * , " Intervals in input file not consistent "
                       STOP " STOP in subroutine 'average_surface_T_over_time' "

                    END IF

                 END IF

                 icount = icount + 1

                 IF ( icount > nhold ) THEN

                    PRINT * , " ************************************************************************** "
                    PRINT * , " NHOLD exceeded in subroutine 'average_surface_T_over_time': NHOLD = ", NHOLD
                    PRINT * , " This could happen if the input time interval is less than " , &
                         ( 1440 / float ( NHOLD ) ) , " minutes "
                    PRINT * , " or if the input time interval does not fit evenly into days. "
                    PRINT * , " If either of these is the case, try increasing NHOLD "
                    STOP 

                 END IF

                 IF ( ( bhi( 1,1) .EQ. 2 ) .AND. &
                      ( bhi( 8,1) .EQ. 1 ) .AND. &
                      ( bhi(15,1) .EQ. 0 ) ) THEN
                    ALLOCATE(tmp2d(small_header%end_dims(1), small_header%end_dims(2)))
                    READ ( iunit ) tmp2d
                    holdt ( : , : , icount ) = tmp2d ( bhi(11,1) + 1 : bhi(11,1) + imx , bhi(12,1) + 1 : bhi(12,1) + jmx )
                    DEALLOCATE(tmp2d)
                 else
                    READ ( iunit ) holdt( : , : , icount )
                 endif
                 holdt ( : , jmx , icount ) = holdt ( : , jmx - 1 , icount )
                 holdt ( imx , : , icount ) = holdt ( imx - 1 , : , icount )

                 IF ( ( idts > 0 ) .AND. ( MOD ( ( idts + intv ) , 86400 ) == 0 ) ) THEN

                    PRINT * , " Summing data to avgsfct at time " , small_header%current_date ( 1 : 19 )
                    dcount = dcount + icount
print *, 'idts,intv,icount = ', idts,intv,icount

                    DO i = 1 , icount
                       avgsfct = avgsfct + holdt( : , : , i )
                    END DO

                    icount = 0
                    holdt = 0.0
print *, 'dcount = ', dcount

                 END IF

                 lastdate = small_header%current_date

              ELSE

                 READ ( iunit ) xdum

              END IF


           ELSE IF ( flag == 2 ) THEN

              ! We've hit the end of time.  Nothing in particular to do.

           ELSE

              PRINT * , " Unrecognized FLAG in subroutine average_surface_T_over_time "
              PRINT * , " FLAG = " , flag
              STOP

           END IF

        END DO var_loop

        CLOSE ( iunit )

     END DO file_loop

     WRITE ( * , ' ( " Using " , i4 , " input T fields in our T average. " ) ' ) dcount

     IF ( icount > 0 ) THEN
        WRITE ( * , ' ( " Skipping " , i4 , " input T fields off the end of our input data set for our T average. " ) ' ) icount
     END IF

     avgsfct = avgsfct / float ( dcount )

   END SUBROUTINE average_surface_T_over_time
!KWM END


END MODULE all_io
