MODULE file_data

   INTEGER :: terrain_file , first_guess_file , namelist_file , output_file
   INTEGER :: file_index , file_index_min = 10 , file_index_max = 99
   LOGICAL :: already_opened = .FALSE. , output_initialized = .FALSE.
   INTEGER :: open_status , read_status , eof_status

   INTEGER :: diff_files_per_single_time , file_name_len

   CHARACTER (LEN=132) :: file_name

   CHARACTER (LEN=14) , PARAMETER :: file_name_namelist = 'namelist.input'

END MODULE file_data
