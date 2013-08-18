!  This program reads in a data set prepared by one of the
!  first-guess pre-processor programs (collectively given the
!  working name of "pregrid").  This program assumes that the
!  input data is in a very simple, binary format.  Following is
!  a description of the input format requirements:

!  The following write statements are iterated for as many 2d arrays as there
!  are to be put in the file:
!  
!     WRITE (iunit) iversion
!     WRITE (iunit) hdate, xfcst, field, units, desc, level, idim, jdim, llflag
!     IF      (llflag.eq.0) THEN
!        WRITE (iunit) startlat, startlon, deltalat, deltalon
!     ELSE IF (llflag.eq.1) THEN
!        WRITE (iunit) lat1, lon1, deltax, deltay, truelat1
!     ELSE IF (llflag.eq.3) THEN
!        WRITE (iunit) lat1, lon1, deltax, deltay, xlonc, truelat1, truelat2
!     ELSE IF (llflag.eq.4) THEN
!        WRITE (iunit) startlat, startlon, deltalon , nlat
!     ELSE IF (llflag.eq.5) THEN
!        WRITE (iunit) lat1, lon1, deltax, deltay, xlonc, truelat1
!     END IF
!     WRITE (iunit) array

!  Where:
!     INTEGER           :: iversion
!     CHARACTER(LEN=24) :: hdate
!     REAL              :: xfcst
!     CHARACTER(LEN=9)  :: field
!     CHARACTER(LEN=25) :: units
!     CHARACTER(LEN=46) :: desc
!     REAL              :: level
!     INTEGER           :: idim
!     INTEGER           :: jdim
!     INTEGER           :: llflag
!     REAL              :: startlat
!     REAL              :: startlon
!     REAL              :: deltalat
!     REAL              :: deltalon
!     REAL              :: lat1
!     REAL              :: lon1
!     REAL              :: deltax
!     REAL              :: deltay
!     REAL              :: xlonc
!     REAL              :: truelat1
!     REAL              :: truelat2
!     INTEGER           :: nlat

!     IVERSION is a format version number.  Currently, the most recent version is = 3.  
!     HDATE is a 24-character string, of which only the first 19 characters
!           are used, of the form YYYY-MM-DD_hh:mm:ss. The hh:mm:ss is 
!           assumed to be 00:00:00 for this program.
!     XFCST is the foreast hour of the first guess field (this is set
!           to zero if the data is from an analysis).
!     FIELD is an 9-character label for the field where the last character
!           is a blank space.  Labels understood so far are:
!                'T       ', Temperature (K)
!                'U       ', U wind component (m/s)
!                'V       '  V wind component (m/s)
!                'HGT     ', Geopotential Height (not geopotential) (m)
!                'RH      ', Relative Humidity (%)
!                'SNOWCOVR', Snow-cover flag (0/1)
!                'SST     ', Sea-surface Temperature (K)
!                'PRESSURE', Pressure (Pa) (You can tell whether it is
!                            sea-level pressure or surface pressure by
!                            checking on the value given for the pressure
!                            level.  This is to let someone put in sea level
!                            pressure in a different way than PMSL.
!                'SOILTxxx', Soil temperature (K), where xxx is the bottom
!                            of the level in cm
!                'SOILMxxx', Soil moisture fraction, where xxx is the bottom 
!                            of the level in cm
!                'WEASD   ', Water equivalent of accumulated snow depth (mm)
!                'SEAICE  ', Sea ice flag (0/1)
!                'PMSL    ', Mean sea level pressure (Pa)
!                'LANDSEA ', Flag field: 1=land, 0=water
!                'SOILHGT ', Terrain height of the analysis data (m)

!     UNITS is a 25 character string of the field's units, where the last
!           character is a blank space.
!     DESC is a 46 character string of the description of the field.
!     IDIM is the number of points in the first dimension.
!     JDIM is the number of points in the second dimension.
!     LEVEL is a floating-point pressure-level value, in Pa.  Special
!           values are 201300 for sea-level and 200100 for surface.

!     LLFLAG is a flag saying that the next record defines the projection
!           of the grid.  Valid values so far are 0 (regular latitude
!           longitude), 1 (Mercator), 3 (Lambert conformal), and 5 (polar 
!           stereographic).

!     STARTLAT is the starting latitude, in degrees N
!     STARTLON is the starting longitude, in degrees E
!     DELTALAT is the latitude increment, in degrees N
!     DELTALON is the longitude increment, in degrees E

!     LAT1 is the starting latitude, in degrees N
!     LON1 is the starting longitude, in degrees E
!     DELTAX is the x increment in km
!     DELTAY is the y increment in km
!     XLONC is the center longitude of the projection, in degrees E
!     TRUELAT1 is the first true latitude
!     TRUELAT2 is the second true latitude (not required for polar)
!     NLAT is the number of latitude loops between the pole and the equator

!     ARRAY is the actual 2d slab, dimensioned (IDIM,JDIM)

!  Note that ANY 2d array is allowed to be passed into this program.  This 
!  will allow an easier usage of the data later in the modeling system data 
!  stream.  Assumptions are made about only a few fields regarding the
!  possibilities of masking.  All unknown fields are assumed to be cross 
!  point on output.


!  1) proc_namelist
!           Read in the NAMELIST
!           Assign local variables to date and print 
!           Compute total number of times to process (given NAMELIST info)
!           Choose the pressure lid (ptop)
!           Any new pressure levels to vertically interpolate to
!           Input the name of the root of the first guess filename
!           Perform simple consistency checks on data

!  2) proc_read_terrain 
!           Set up linked list for terrain data
!           Read in terrain data to linked list - at least 2 fields required 
!              (terrain and land use)
!           Once all fields are ingested (and more importantly - counted), the
!              data is restored to an allocated array of all the terrain data
!           Compute latitude, longitude, map scale factor, coriolis for both
!              dot and cross point staggerings
!           Works on COARSE or FINE domains
!           1 TERRAIN domain at a time
!           Save locations of some important 2d fields: terrain, land use, 
!              latitude and longitude

!  Loop over times requested in NAMELIST
!  Using the NAMELIST date/time info, compute the
!     starting and ending times, and loop through the available times      
!  Compute the current date/time, both CHARACTER and INTEGER

!           3) proc_ingest_first_guess
!                    Loops over DATA/CONSTANT files
!                    Read first guess data (in the intermediate format
!                       described briefly above)
!                    Accept differnt formats based on version number
!                    Use all 80 characters for non-traditional DATAGRID fields
!                    Interpolate each 2d field to the model grid (dot only)
!                    If the data is a masked field, use nearest neighbor
!                    Compute number of first guess 3d/2d fields
!                    Allow ptop to regulate vertical level cut-off
!                    DEALLOCATE of array handled in the couple cases where
!                       the READ loop is cycled
!                    Compute maximum number of 3d levels for use in the 
!                       ALLOCATE of the pressure array (and others)
!                    Masked fields (land and water) are treated to a host
!                       of IF tests
!                    Allow Lambert projection for input first guess data
!                    Check header date with NAMELIST date for verification
!                    Allow polar stereographic projection for input first 
!                       guess data

!                    3a) proc_grid_store
!                              Initialize the linked list
!                              Generate correct names of variables
!                              For each variable, create a linked list of levels
!                              Each level has a pointer to a 2d gridded data array
!                              Check for same level same variable name

!           4) proc_list_to_array
!                    Organize the linked list data into regular arrays
!                    Put the 3d data onto additional levels
!                    Save names of 2d and 3d fields
!                    Save values of vertical pressure levels
!                    Check for repeated pressures, one from the new levels and one from
!                    first guess field, that are the same

!           5) proc_zap_space_list
!                    Loop over each different variable in the linked list
!                    Loop over each level in the linked list
!                    DEALLOCATE the 2d field and the current level

!           6) proc_make_dot_point_data
!                    Change names of 2d data from first guess file to 
!                       standard modeling system convention
!                    Change non-standard fields (SOIL, SEA ICE, etc) to 
!                       cross point, take care of masked fields
!                    Make sure no snow is sitting on top of water
!                    Add the TERRAIN file data to the 2d arrays            
!                    Horizontally interpolate dot point TERRAIN file data
!                       to cross points
!                    Rename dot point 3d data so that it is not used
!                    Horizontally interpolate 3d data to cross points
!                    Correct surface level of geopotential height to be
!                       the terrain elevation
!                    Set minimum relative humidity to 10%
!                    Sufficiency test for minimum data requirements
!                    Locate and save soil location for mask purposes
!                    Manufacture traditional snow cover based on snow depth
!                    Store terrain elevation in the surface location of the
!                       height array
!                    Print out the names of the variables for this time period

!           7) proc_map_to_met_winds
!                    Rotate winds based on input first guess projection
!                    Test is either for polar stereographic or Lambert
!                       conformal projection

!           8) proc_met_to_map_winds
!                    Rotate winds based on output map projection

!           9) proc_make_big_header
!                    Only called at the initial time
!                    Using NAMELIST info (such as dates), build the modeling 
!                       system record header

!          10) proc_make_small_header
!                    For each of the 3d and 2d fields, build the small header
!                    The 3d fields have the character strings modified, but the
!                       2d fields are as is (due to renaming 3d fields)

!          11) proc_tc_bogus
!                    Tropical bogussing scheme. The code will remove the old storm
!                    and replace it with a balanced symmetric vortex. The code will
!                    modify as many storms as are given in the namelist file.

!          12) proc_output
!                    Clean up masked SOIL[TM]xxx fields using the land sea mask
!                       and the sea ice array
!                    Check to see if there is an available UNIT
!                    If first time, try to OPEN the REGRID_DOMAINx file and write big header
!                       flag
!                    WRITE the data in a modeling system format (flag, header, data)
!                    If last time, write end of time flag and CLOSE file

!          13) proc_zap_space_array
!                    Delete ALLOCATEd space from the TERRAIN file only
!                       if this is the last time period
!                    Delete the space for the 2d,3d array holders

PROGRAM regridder

   USE date_pack
   USE namelist_info
   USE first_guess_data

   IMPLICIT NONE

   !  This data is the local stuff for the time loop.  We
   !  compute the incremental times from the starting and
   !  ending times defined in the NAMELIST.

   INTEGER :: cy1,mm1,dd1,hh1,mi1,ss1,yy,dh
   INTEGER :: cy2,mm2,dd2,hh2,mi2,ss2
   INTEGER :: loop_index , loop_max=1000
   CHARACTER (LEN=19) :: next_date

   !  Ingest the NAMELIST data.  From that we get the info
   !  drive the program. 

   CALL proc_namelist

   !  Read in the TERRAIN data.  This includes possibly MANY extra 2d arrays
   !  as well as elevation.  All of the domain computable arrays (latitude,
   !  longitude, map scale factor and coriolis) are stored.

   CALL proc_read_terrain 

   !  Build the INTEGER YYMMDHH of the starting date (for the
   !  record header ouput).

   cy1 = rec1%yyyy_st
   mm1 = rec1%mm_st
   dd1 = rec1%dd_st
   hh1 = rec1%hh_st

   READ  ( rec1%starting_date( 3: 4) , FMT='(I2.2)'     ) yy
   rec1%starting_date_int = yy*1000000 + mm1*10000 + dd1*100 + hh1
 
   PRINT '(A,A)','Initial time :                ',rec1%starting_date
   PRINT '(A,A)','Ending time:                  ',rec1%ending_date

   !  This is the time loop.  Initialize the current time to be the 
   !  starting time requested in the namelist.

   rec1%current_date = rec1%starting_date

   rec1%time_period_count = 0
   time_loop : DO WHILE ( rec1%current_date .LE. rec1%ending_date )

      PRINT '(//A,A)','Starting current time:        ',rec1%current_date

      !  Increment the time period counter.

      rec1%time_period_count = rec1%time_period_count + 1

      !  Build the INTEGER current date information from the CHARACTER
      !  string current date.  We get the individual pieces.

      READ ( rec1%current_date( 1: 4) , FMT='(I4.4)' ) cy1
      READ ( rec1%current_date( 6: 7) , FMT='(I2.2)' ) mm1
      READ ( rec1%current_date( 9:10) , FMT='(I2.2)' ) dd1
      READ ( rec1%current_date(12:13) , FMT='(I2.2)' ) hh1
      READ ( rec1%current_date(15:16) , FMT='(I2.2)' ) mi1
      READ ( rec1%current_date(18:19) , FMT='(I2.2)' ) ss1
      READ ( rec1%current_date( 3: 4) , FMT='(I2.2)' ) yy

      !  Build the INTEGER YYMMDHH of the current date as well (for the
      !  record header ouput).

      rec1%current_date_int = yy*1000000 + mm1*10000 + dd1*100 + hh1

      !  Put the local variable pieces into data structures that will be
      !  moved around the code.

      rec1%yyyy_now = cy1
      rec1%mm_now   = mm1
      rec1%dd_now   = dd1
      rec1%hh_now   = hh1

      !  Read in the 2d meteorological fields.  Each field is horizontally
      !  interpolated to the model grid (defined by the TERRAIN file).  All
      !  of the data comes into this routine as slices, even the 3d data come
      !  in as separate levels to be ordered in the linked list.
   
      CALL proc_ingest_first_guess

      !  The ingested data goes into a linked list, which is a bit difficult
      !  to handle with any grace or facility.  Transfer the data from 
      !  the linked list into a more traditional array oriented setting.  This
      !  is also where we include the additional levels requested in the 
      !  NAMELIST.

      CALL proc_list_to_array

      !  We have ALLOCATED quite a bit of space for this time period.
      !  Since we are finished with it, we deALLOCATE it.  This is the 
      !  linked list data only, not the gridded ALLOCATEd files.

      CALL proc_zap_space_list

      !  The data in the 2d arrays need some fine tuning before we
      !  can use it to build the header and output: there are missing
      !  dot point fields (primarily), and a few variables need to 
      !  be renamed.  We undertake to rectify these situations.

      CALL proc_make_dot_point_data

      !  The input data may need to have the first guess map projection
      !  rotation removed from the wind components.  The new data are stored
      !  in the original wind locations.  This is required if we are bringing 
      !  in the first guess field in either Lambert or a polar projection.  This
      !  call is not a function of the projection type, but is set up so that 
      !  you have to remember to fix it with new projection flags.

      IF ( ( wind_llflag .EQ. 3 ) .OR. ( wind_llflag .EQ. 5 ) ) THEN

         CALL proc_map_to_met_winds ( wind_xlonc , wind_cone_fac , wind_xlatc ) 

      END IF

      !  Rotate the meteorological winds to the output map coordinate.  The
      !  wind components are stored on top of the previous u, v
      !  values as they are no longer needed.

      CALL proc_met_to_map_winds

      !  The record header for the modeling system has to be built.  Information
      !  is needed from the NAMELIST, the TERRAIN input file, as well as from 
      !  the first guess data file.  The order of the fields is possibly
      !  different than from a traditional DATAGRID program.  For the V3 format,
      !  the big header is written only at the initial time.

      IF ( rec1%time_period_count .EQ. 1 ) THEN

         CALL proc_make_big_header

      END IF

      !  A small header must be built for each of the output variables at this
      !  time period.  

      CALL proc_make_small_header

      !  Do tropical cyclone bogussing, out with the old and in with the new.  Of
      !  course, the minimum requirement is that we are asking to do this option with
      !  at least one storm.  Also, it is reasonable to ONLY do this option during the
      !  first time period, as we have provided no mechanism for prescribing storms at more
      !  than just a single time.
       
      IF ( ( rec5%num_storm .GT. 0 ) .AND. &
           ( rec5%insert_bogus_storm ) .AND. &
           ( rec1%time_period_count .EQ. 1 ) ) THEN

         CALL proc_tc_bogus

      END IF

      !  WRITE the REGRID data to an output file.

      CALL proc_output

      !  We should delete the arrays that we no longer need.  If this is
      !  the ending time we delete all of the TERRAIN file info as well.
      !  This deletes this time period's stored 2d, 3d and pressure arrays.

      CALL proc_zap_space_array
    
      !  The first shall be last and the last shall be first.  Increment the
      !  time loop.
      
      PRINT '(A,A)','Ending current time:          ',rec1%current_date

      CALL geth_newdate( next_date , rec1%current_date , rec1%time_increment_second ) 
       
      rec1%current_date = next_date

   END DO time_loop

END PROGRAM regridder
