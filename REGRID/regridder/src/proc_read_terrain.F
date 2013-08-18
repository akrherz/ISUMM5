SUBROUTINE proc_read_terrain

   USE header_data
   USE terrain_data
   USE file_data
   USE namelist_info
   USE constants

   IMPLICIT NONE

   INTEGER                             :: num_vars
   CHARACTER (LEN=8)                   :: variable_name

   INTEGER :: i , j
   REAL :: x , y
   REAL , ALLOCATABLE , DIMENSION(:,:) :: dum2d

   TYPE terdat
      TYPE(sh)                        :: small_header
      REAL , DIMENSION(:,:) , POINTER :: data
      TYPE(terdat) , POINTER          :: next_field
   END TYPE
   
   TYPE(terdat) , POINTER :: head , old , current

   INTEGER :: ok , flag
   LOGICAL :: first_time = .TRUE.

   terrain_file = -1
   terrain_unit : DO file_index = file_index_min , file_index_max
      INQUIRE (UNIT=file_index, OPENED=already_opened)
      IF ( .NOT. already_opened) THEN
         terrain_file = file_index
         EXIT terrain_unit
      END IF
   END DO terrain_unit   
   
   IF ( terrain_file .EQ. -1 ) THEN
      PRINT '(A)','Could not find a spare unit for the TERRAIN data.'
      STOP 'terrain_file_unit'
   END IF

   REWIND ( UNIT=terrain_file )

   PRINT '(A,A/)','Trying to OPEN file: ',TRIM(rec3%terrain_file_name)

   OPEN ( UNIT=terrain_file , &
          FILE=TRIM(rec3%terrain_file_name) , &
          STATUS = 'OLD' , &
          FORM = 'UNFORMATTED' , &
          IOSTAT = open_status )
   
   IF ( open_status .NE. 0 ) THEN
      PRINT '(A)' , 'Error OPENing terrain file.'
      STOP 'terrain_file_open'
   END IF
   
   !  Read initial flag from TERRAIN file.  This is the big header
   !  flag.

   READ ( UNIT=terrain_file , IOSTAT = ok ) flag
   
   IF ( ok .NE. 0 ) THEN

      PRINT '(A)','Error trying to read the initial flag for TERRAIN.'
      STOP 'terrain_flag_read'

   END IF

   IF ( flag .EQ. bh_flag ) THEN
      IF ( rec4%print_file ) THEN
         PRINT '(A)','Found big header flag.'
      END IF
   ELSE
      PRINT '(A,I8,A)','Found wrong flag at beginning of TERRAIN: ',flag,'.'
      STOP 'wrong_terrain_bhflag'
   END IF

   !  Read header data from TERRAIN program.

   READ ( UNIT=terrain_file ) bhi , bhr , bhic , bhrc

   !  Spread the header information around to local variables.

   center_latitude = bhr(2,1)
   center_longitude = bhr(3,1)
   IF ( bhi(15,1) .EQ. 0 ) THEN
      ixc = bhi(10,1)
      jxc = bhi(9,1)
      ixf = bhi(10,1)
      jxf = bhi(9,1)
   ELSE
      ixc = bhi(6,1)
      jxc = bhi(5,1)
      ixf = bhi(17,1)
      jxf = bhi(16,1)
   END IF
   grid_ratio = NINT ( bhr(1,1) / bhr(9,1) ) 
   grid_distance_km = bhr(9,1) / 1000.
   xstart = bhr(11,1)
   ystart = bhr(10,1)
   water_index = bhi(23,1)
   IF ( water_index .LT. 0 ) THEN
      PRINT '(A)','Assuming that the water is index #7.  Make sure this is OK.'
      water_index = 7 
   END IF
   cone_factor = bhr(4,1)
   IF      ( bhi(7,1) .EQ. 1 ) THEN
      projection = 'LAMBERT CONFORMAL'
   ELSE IF ( bhi(7,1) .EQ. 2 ) THEN   
      projection = 'POLAR STEREOGRAPHIC'
   ELSE IF ( bhi(7,1) .EQ. 3 ) THEN   
      projection = 'MERCATOR'
   END IF
   true_latitude_1 = bhr(5,1)
   true_latitude_2 = bhr(6,1)

   !  ALLOCATE space for the dummy 2d array used for input.

   ALLOCATE (dum2d(jxf,ixf))

   !  READ in the 2d arrays.  Store both the gridded data and the CHARACTER string
   !  of the description of the variable.

   i = 0
   terrain_data_store : DO

      !  Read in the data flag.

      READ ( UNIT=terrain_file , IOSTAT = ok ) flag

      !  Was this read ok?

      IF      ( ok .LT. 0 ) THEN
         IF ( rec4%print_file ) THEN
            PRINT '(A)','Found end of TERRAIN file.'
         END IF
         EXIT terrain_data_store
      ELSE IF ( ok .GT. 0 ) THEN 
         PRINT '(A)','Error in reading the fields for TERRAIN.'
         STOP 'terrain_file_read_error'
      END IF

      !  Interpret the flag value.

      IF      ( flag .EQ. bh_flag ) THEN
         PRINT '(A)','Found a big header flag for TERRAIN file, didn''t expect one.'
         STOP 'why_bh_flag'
      ELSE IF ( flag .EQ. eot_flag ) THEN
         IF ( rec4%print_file ) THEN
            PRINT '(A)','Found the eot flag for TERRAIN file.  OK.'
         END IF
         EXIT terrain_data_store
      ELSE IF ( ( flag .EQ. sh_flag ) .AND. ( rec4%print_file ) ) THEN
         PRINT '(A)','Found small header flag for TERRAIN file.'
      END IF

      !  Now read the small header.  We know this comes next because the last
      !  header value was for the small header OR we exited out of this loop
      !  ( for eot_flag), OR we stopped (bh_flag).
 
      READ ( UNIT=terrain_file ) small_header%num_dims ,  &
                                 small_header%start_dims ,  &
                                 small_header%end_dims ,  &
                                 small_header%xtime ,  &
                                 small_header%staggering ,  &
                                 small_header%ordering ,  &
                                 small_header%current_date ,  &
                                 small_header%name ,  &
                                 small_header%units ,  &
                                 small_header%description 
      small_header%name(9:9) = ' '

      !  Pull in the actual data.

      READ ( UNIT=terrain_file ) dum2d

      !  For the cross point variables, expand out the last row and column.  This
      !  lets us use the entire 2d domain for the array syntax.

      IF ( ( small_header%name .NE. 'LATITDOT ' ) .AND. &
           ( small_header%name .NE. 'LONGIDOT ' ) .AND. &
           ( small_header%name .NE. 'MAPFACDT ' ) .AND. &
           ( small_header%name .NE. 'CORIOLIS ' ) ) THEN
         dum2d(jxf,:) = dum2d(jxf-1,:)
         dum2d(:,ixf) = dum2d(:,ixf-1)
      END IF
  
      !  Increment the counter of how many 2d fields there are.

      i = i + 1

      !  We are going to store this header information and the data, so 
      !  space has to be allocated.  It's a linked list, so nullify the 
      !  pointer to the next field.

      ALLOCATE(current)
      ALLOCATE(current%data(jxf,ixf))
      NULLIFY(current%next_field)

      !  Assign data to the linked list.

      current%small_header = small_header
      current%data         = dum2d

      !  Connect up list.

      IF ( first_time ) THEN
         first_time = .FALSE.
         head => current
      ELSE
         old%next_field => current
      END IF
      old => current
    
   END DO terrain_data_store

   !  Just how many 2d fields did we find?

   num3d = 0
   num2d = i
   num1d = 0

   !  ALLOCATE space for the terrestial gridded arrays.  The extra kicker is for
   !  the possible LANDMASK field.  This might need to be generated from the 
   !  LAND USE array.

   ALLOCATE (ter_data(num2d+1))
   DO i = 1 , num2d+1
      ALLOCATE (ter_data(i)%array(jxf,ixf))
   END DO

   !  Now that we know how many 2d fields there are, and we have them
   !  all stored in a linked list for safe keeping, and we have 
   !  allocated enough space in a permanent location, we pull the 
   !  data out of the linked list and put it in this array of all
   !  2d TERRAIN information.  As we pull the data out, we deallocate
   !  the space, since it will no longer be used.

   terrain_index         = 0
   land_mask_index       = 0
   land_use_index        = 0
   latitude_dot_index    = 0
   longitude_dot_index   = 0
   latitude_cross_index  = 0
   longitude_cross_index = 0
   tempgrd_index         = 0
   current => head

   terrain_data_permanent : DO i = 1 , num2d

      ter_data(i)%array        = current%data
      ter_data(i)%name_info    = current%small_header%name // &
                                 current%small_header%units // &
                                 current%small_header%description
      ter_data(i)%small_header = current%small_header
      IF      ( current%small_header%name(1:8) .EQ. 'TERRAIN ' ) THEN
         terrain_index         = i
      ELSE IF ( current%small_header%name(1:8) .EQ. 'LAND USE' ) THEN
         land_use_index        = i
      ELSE IF ( current%small_header%name(1:8) .EQ. 'LANDMASK' ) THEN
         land_mask_index       = i
      ELSE IF ( current%small_header%name(1:8) .EQ. 'LATITDOT' ) THEN
         latitude_dot_index    = i
      ELSE IF ( current%small_header%name(1:8) .EQ. 'LONGIDOT' ) THEN
         longitude_dot_index   = i
      ELSE IF ( current%small_header%name(1:8) .EQ. 'LATITCRS' ) THEN
         latitude_cross_index  = i
      ELSE IF ( current%small_header%name(1:8) .EQ. 'LONGICRS' ) THEN
         longitude_cross_index = i
      ELSE IF ( current%small_header%name(1:8) .EQ. 'TEMPGRD ' ) THEN
         tempgrd_index = i
      END IF
     
      !  Get the next node in the list.

      old     => current 
      IF ( ASSOCIATED(current%next_field) ) THEN
         current => current%next_field
      END IF

      !  Zap the old space that is no longer needed.

      DEALLOCATE(old%data)
      DEALLOCATE(old)

   END DO terrain_data_permanent

   !  Check for the necessary things.  We NEED the elevation.

   IF ( terrain_index .EQ. 0 ) THEN
      PRINT '(A)','No terrain elevation found in the TERRAIN file.  Bad news.'
      STOP 'no_terrain_elevation_found'
   END IF

   !  Check for the necessary things.  We NEED the land use.

   IF ( land_use_index .EQ. 0 ) THEN
      PRINT '(A)','No land use categories found in the TERRAIN file.  Bad news.'
      STOP 'no_land_use_found'
   END IF

   !  Check for the necessary things, again.  We NEED a land mask.

   IF ( land_mask_index .EQ. 0 ) THEN
      find_land_mask_2 : DO i = 1 , num2d
         IF      ( ter_data(i)%name_info(1:8) .EQ. 'LAND USE' ) THEN
            land_mask_index = num2d + 1
            ter_data(land_mask_index)%array          = ter_data(i)%array
            ter_data(land_mask_index)%name_info      = ter_data(i)%name_info
            ter_data(land_mask_index)%name_info(1:8) = 'LANDMASK'
            ter_data(land_mask_index)%small_header   = ter_data(i)%small_header
            WHERE ( ABS ( ter_data(land_mask_index)%array - water_index ) .LT. 0.1 )
               ter_data(land_mask_index)%array = 0
            END WHERE
            WHERE ( ter_data(land_mask_index)%array .NE. 0 ) 
               ter_data(land_mask_index)%array = 1
            END WHERE
            WRITE (6,'(A)') 'Generating fake land mask from land use data.'
            num2d = num2d + 1
            EXIT find_land_mask_2
         END IF
      END DO find_land_mask_2
   END IF

   IF ( land_mask_index .EQ. 0 ) THEN
      PRINT '(A)','No land mask found in the TERRAIN file.  Bad news.'
      STOP 'no_land_mask_found'
   END IF

   !  Other important fields are the terrestial fields.  We check for the latitude
   !  and longitude.  If these are present, the map factors and coriolis are 
   !  probably also there.

   IF ( ( latitude_dot_index    .EQ. 0 ) .OR. &
        ( longitude_dot_index   .EQ. 0 ) .OR. &
        ( latitude_cross_index  .EQ. 0 ) .OR. &
        ( longitude_cross_index .EQ. 0 ) ) THEN
      PRINT '(A)','This program expected V3 TERRAIN data.'
      PRINT '(A)','That format assumes the availability of several terrestail fields:'
      PRINT '(A)','dot and cross latitude, longitude, map factors, and dot point coriolis.'
      PRINT '(A)','Those fields were not found.'
      STOP 'not_enough_terrestial_data'
   END IF

   !  The latitude and longitude fields for the cross point staggering are pushed into
   !  the outer row and column.  This allows us to be a bit sloppy when we horizontally
   !  interpolate the data to these locations.

   ter_data(latitude_cross_index)%array(jxf,:)  = ter_data(latitude_cross_index)%array(jxf-1,:)
   ter_data(latitude_cross_index)%array(:,ixf)  = ter_data(latitude_cross_index)%array(:,ixf-1)
   ter_data(longitude_cross_index)%array(jxf,:) = ter_data(longitude_cross_index)%array(jxf-1,:)
   ter_data(longitude_cross_index)%array(:,ixf) = ter_data(longitude_cross_index)%array(:,ixf-1)

   !  Just so that our interpolations work OK, we force the longitude fields to be
   !  between -180 and +180 degrees.
     
   WHERE ( ter_data(longitude_dot_index)%array .GT. 180. )
      ter_data(longitude_dot_index)%array = ter_data(longitude_dot_index)%array - 360.
   END WHERE

   WHERE ( ter_data(longitude_cross_index)%array .GT. 180. )
      ter_data(longitude_cross_index)%array = ter_data(longitude_cross_index)%array - 360.
   END WHERE

   !  We are finished with the TERRAIN file.

   CLOSE (UNIT=terrain_file)

   IF ( rec4%print_file ) THEN
      PRINT '(A,A/)' , 'CLOSE file: ',TRIM(rec3%terrain_file_name)
   END IF

   !  We know longer need the space for the dummy array.

   DEALLOCATE (dum2d)
   
   !  Let everyone know how many 2d fields we now have with the
   !  TERRAIN data.  This is the original number of 2d fields (num2d).

   num2d_ter_data = num2d

END SUBROUTINE proc_read_terrain
