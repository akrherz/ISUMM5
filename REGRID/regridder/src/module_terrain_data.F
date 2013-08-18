MODULE terrain_data

   USE header_data
 
   TYPE ter_file_data
      REAL , DIMENSION(:,:) , POINTER :: array
      CHARACTER(LEN=80)               :: name_info
      TYPE(sh)                        :: small_header
   END TYPE ter_file_data

   TYPE(ter_file_data) , ALLOCATABLE , DIMENSION(:) :: ter_data

   INTEGER :: num2d_ter_data , land_mask_index , latitude_cross_index , &
              latitude_dot_index , longitude_cross_index , longitude_dot_index , &
              terrain_index, water_index , land_use_index , tempgrd_index

END MODULE terrain_data
