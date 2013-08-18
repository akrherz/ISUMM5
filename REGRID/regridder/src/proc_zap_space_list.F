SUBROUTINE proc_zap_space_list

   USE link_list_info
      
   IMPLICIT NONE

   !  Point to the head with an incrementing pointer, and
   !  one that will stay put.

   current_var => head
   new_var     => head

   !  WHILE the pointer is at something that is ASSOCIATED,
   !  drop down into it's levels.

   each_variable : DO WHILE ( ASSOCIATED ( new_var ) ) 

      !  Hold onto where we are pointing since this is not a 
      !  doubly linked list.

      old_var => new_var

      !  Initialize the various levels' pointer.

      new_slab => new_var%slab

      !  Check each one of the levels.

      each_level : DO WHILE ( ASSOCIATED ( new_slab ) )

         !  Again, hold onto where we are as this is not
         !  doubly linked.

         old_slab => new_slab
      
         !  Check for the next level.
  
         new_slab => new_slab%next_slab

         !  DEALLOCATE what we can.

         DEALLOCATE ( old_slab%field )
         DEALLOCATE ( old_slab )
 
      END DO each_level

      !  Increment to the next variable.

      new_var => new_var%next_variable

      !  If this is not the head of the list (which is a
      !  TARGET - not ALLOCATABLE), zap this variable.

      IF ( current_var%name .NE. old_var%name ) THEN
         IF ( ASSOCIATED(old_var) ) THEN
            DEALLOCATE ( old_var )
         ENDIF
      END IF


   END DO each_variable

   !  Clean up the head to an unused an pristine state.

   head%name = '        '
   head%num_levels = 0
   NULLIFY ( head%next_variable )
   NULLIFY ( head%slab )

   !  Finally, set the linked list initialized flag to FALSE.

   initialized = .FALSE.

END SUBROUTINE proc_zap_space_list
