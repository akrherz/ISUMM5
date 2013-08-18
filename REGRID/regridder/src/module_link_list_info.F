MODULE link_list_info

   TYPE twod
      REAL :: level
      REAL , DIMENSION(:,:) , POINTER :: field
      TYPE(twod) , POINTER :: next_slab
   END TYPE twod

   TYPE vars
      INTEGER :: num_levels
      CHARACTER (LEN=80) :: name
      TYPE(vars) , POINTER :: next_variable
      TYPE(twod) , POINTER :: slab
   END TYPE vars

   TYPE(vars) , TARGET  :: head
   TYPE(vars) , POINTER :: current_var , new_var , old_var
   TYPE(twod) , POINTER :: current_slab , new_slab , old_slab

   LOGICAL :: initialized = .FALSE. , found = .FALSE.
   REAL , DIMENSION(:,:) , POINTER :: new_field

END MODULE link_list_info
