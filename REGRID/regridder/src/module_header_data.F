MODULE header_data

   INTEGER            , DIMENSION(50,20) :: bhi
   CHARACTER (LEN=80) , DIMENSION(50,20) :: bhic
   REAL               , DIMENSION(20,20) :: bhr
   CHARACTER (LEN=80) , DIMENSION(20,20) :: bhrc

   TYPE sh
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
   END TYPE sh

   TYPE(sh) :: small_header

   INTEGER , PARAMETER :: bh_flag  = 0 , &
                          sh_flag  = 1 , &
                          eot_flag = 2

   INTEGER                      :: ixc , jxc , ixf , jxf , grid_ratio , &
                                   num3d, num2d , num1d
   REAL                         :: xstart , ystart , cone_factor , &
                                   true_latitude_1 , true_latitude_2 , &
                                   center_latitude , center_longitude , &
                                   grid_distance_km 
   CHARACTER (LEN=80)           :: projection
   INTEGER                      :: num3d_fg , num2d_fg , num1d_fg
   INTEGER                      :: number_of_original_levels

END MODULE header_data
