MODULE gridded_data

   USE header_data

   REAL , DIMENSION(:,:,:,:) , ALLOCATABLE :: all_3d
   REAL , DIMENSION(:,:,:)   , ALLOCATABLE :: all_2d

   INTEGER , PARAMETER :: max_fg_variables = 200
   INTEGER :: max_fg_variables_index , fg_variables_sfc_index , fg_variables_up_index
   CHARACTER (LEN=80) , DIMENSION(max_fg_variables) :: all_names_sfc , all_names_up
   TYPE(sh) , DIMENSION(max_fg_variables) :: sh_sfc , sh_up
   LOGICAL :: found_name
   INTEGER , DIMENSION(:) , ALLOCATABLE :: pressure

END MODULE gridded_data
