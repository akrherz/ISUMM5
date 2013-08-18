SUBROUTINE proc_grid_store ( name , level , field , ix , jx )

   USE link_list_info
   USE namelist_info

   IMPLICIT NONE

   CHARACTER (LEN=80) , INTENT(IN) :: name
   REAL , INTENT(IN) :: level
   INTEGER , INTENT(IN) :: ix,jx
   REAL , DIMENSION(jx,ix) , INTENT(IN) :: field

   LOGICAL :: bad = .FALSE.

   initialized_TF : IF ( .NOT. initialized ) THEN

      IF ( rec4%print_link_list_store ) THEN
         PRINT '(A,A)', 'Uninitialized ',name
      END IF

      initialized = .TRUE.
      
      !  Initialize the head of the linked list.

      head%num_levels=0
      NULLIFY(head%next_variable)
      NULLIFY(head%slab)

      !  We are going to index around with the current pointer.

      current_var => head

      !  Store the name of this variable.  Since this is the first
      !  one, there is no search to see what order they go in.

      current_var%name = name

      !  ALLOCATE space for a new data level.  This does not contain
      !  the gridded 2d data itself.

      ALLOCATE (new_slab)
      NULLIFY (new_slab%next_slab)
      current_var%slab => new_slab

      !  Store which level this data is valid for.

      current_var%slab%level = level

      !  Allocate the space needed to store the 2d data.

      ALLOCATE (new_field(jx,ix))
      current_var%slab%field => new_field

      !  Store the input gridded 2d data in this space.  Again,
      !  since this is the first store, we do not need to search
      !  for the correct vertical level.

      current_var%slab%field = field

      !  Initialize the number of 2d fields stored.  The first one
      !  is always the easy one.

      current_var%num_levels = 1

   ELSE IF ( initialized ) THEN

      IF ( rec4%print_link_list_store ) THEN
         PRINT '(A,A)', 'Initialized ',name
      END IF

      !  Check to see where this name can fit in.  We do not
      !  need to order these alphabetically.  Typically, there
      !  are going to be less than 30 different names, so a 
      !  linear search is not a problem.  If this name does not
      !  already exist, then ALLOCATE space for this new variable.

      current_var => head
      found = .FALSE.

      find_right_name : DO WHILE ( ASSOCIATED(current_var) )
         IF ( current_var%name(1:8) .EQ. name(1:8) ) THEN
            found = .TRUE.
            EXIT find_right_name
         END IF
         old_var => current_var
         current_var => current_var%next_variable
      END DO find_right_name

      !  If we need to add this name into the linked list, we 
      !  now know where to add it in (this is always just at
      !  the end of the list, as explained above).

      new_name_to_make : IF ( .NOT. found ) THEN
         
         IF ( rec4%print_link_list_store ) THEN
            PRINT '(A,A)', 'Name not found: ',name
         END IF

         !  ALLOCATE space for this new variable.
   
         ALLOCATE(new_var)
         new_var%num_levels = 0
         NULLIFY (new_var%next_variable)
         NULLIFY (new_var%slab)
  
         !  Include the new variable in the linked list (the one
         !  before this has to know how to get to him).

         old_var%next_variable => new_var

         !  Put the current pointer on the new variable.

         current_var => new_var   
   
         !  Store the name of this variable.
   
         current_var%name = name

      ELSE IF ( found ) THEN
         
         IF ( rec4%print_link_list_store ) THEN
            PRINT '(A,A)', 'Name found: ',name
         END IF
      END IF new_name_to_make

      !  Increment the levels counter.

      current_var%num_levels = current_var%num_levels + 1

      !  Find where to stick the vertical level.  This is ordered such that the
      !  first level is closest to the ground (i.e., the largest pressure).
      !  If there is only one level, then no search.

      first_level_TF : IF ( current_var%num_levels .EQ. 1 ) THEN
 
         IF ( rec4%print_link_list_store ) THEN
            PRINT '(A,A,1X,I8)', 'First level for this variable: ',name,NINT(level)
         END IF

         !  Make the space for the new level type.  Have the pointer
         !  index to it.

         ALLOCATE (new_slab)
         NULLIFY (new_slab%next_slab)
         current_var%slab => new_slab

         !  Now, make space for the 2d data.

         ALLOCATE (new_field(jx,ix))
         current_var%slab%field => new_field

         !  Load up the 2d data into the holder.
        
         current_var%slab%level = level
         current_var%slab%field = field
      
      !  We need to sort the levels since we already have at least
      !  one of them.

      ELSE
 
         IF ( rec4%print_link_list_store ) THEN
            PRINT '(A,A,1X,I8)', 'Other levels exist for this variable: ',name,NINT(level)
         END IF

         found = .FALSE.
         current_slab => current_var%slab
         old_slab => current_var%slab
             
         !  Does the new level go before all levels?  This is only checked
         !  once (outside of the following DO loop).

         bigger_than_first_level : IF ( level .GT. current_slab%level ) THEN

            IF ( rec4%print_link_list_store ) THEN
               PRINT '(A,A,1X,2I8)', 'This level goes before the others: ',&
                                     name,NINT(level),NINT(current_slab%level)
            END IF

            !  ALLOCATE space for the slab type and the 2d data.
            
            ALLOCATE (new_slab)
            NULLIFY (new_slab%next_slab)

            !  Make space for the 2d data, load up the space.

            ALLOCATE (new_field(jx,ix))
            new_slab%field => new_field

            !  Have the new slab point to the rest of the list.

            new_slab%next_slab => current_slab

            !  Have the top of the variable list point to this slab.

            current_var%slab => new_slab

            !  Fill in all of the data that we have for this slab.

            new_slab%level = level
            new_field = field
! foo this does not work on DEC, so using the above kludge
!           new_slab%field = field

            !  The data has been placed in the linked list successfully, so we
            !  set a flag so that we do not go into the following loop.

            found = .TRUE.
         
         END IF bigger_than_first_level

         !  Either the new level goes in between two levels or at the end of
         !  the list.  Check these two choices.  A DO loop is required as we
         !  traverse all of the way to the bottom.  We do not go in this
         !  loop if we have successfully already placed the level above.

         find_right_level : DO WHILE ( ASSOCIATED(current_slab) .AND. ( .NOT. found) )

            !  The new level goes at the end of the list.

            vertical_search : IF ( ( .NOT. ASSOCIATED (current_slab%next_slab) ) .AND. &
                      ( level .LT. current_slab%level  ) ) THEN
   
               IF ( rec4%print_link_list_store ) THEN
                  PRINT '(A,A,1X,2I8)', 'This level goes after all the others: ',name,NINT(level),NINT(current_slab%level)
               END IF

               !  ALLOCATE space for the slab type and the 2d data.
               
               ALLOCATE (new_slab)
               NULLIFY (new_slab%next_slab)

               !  Make space for the 2d data, load up the space.

               ALLOCATE (new_field(jx,ix))
               new_slab%field => new_field

               !  Fill in all of the data that we have for this slab.

               new_slab%level = level
               new_field = field
!              new_slab%field = field

               !  Have the previous slab in the list point to this slab.

               current_slab%next_slab => new_slab

               !  EXIT the DO loop, the data has been placed in the linked
               !  list successfully.
                            
               found = .TRUE.
               EXIT find_right_level

            !  The new level goes between two levels.

            ELSE IF ( ( level .GT. current_slab%level ) .AND. &
                      ( level .LT.     old_slab%level ) ) THEN
   
               IF ( rec4%print_link_list_store ) THEN
                  PRINT '(A,A,1X,3I8)', 'This level goes between two other levels: ', &
                                         name,NINT(level),NINT(old_slab%level),NINT(current_slab%level)
               END IF

               !  ALLOCATE space for the slab type and the 2d data.
               
               ALLOCATE (new_slab)
               NULLIFY (new_slab%next_slab)

               !  Make space for the 2d data, load up the space.

               ALLOCATE (new_field(jx,ix))
               new_slab%field => new_field

               !  Have the new slab point to the rest of the list.

               new_slab%next_slab => current_slab

               !  Have the previous slab in the list point to this slab.

               old_slab%next_slab => new_slab

               !  Fill in all of the data that we have for this slab.

               new_slab%level = level
               new_field = field
! foo this does not work on DEC, so using the above kludge
!              new_slab%field = field

               !  EXIT the DO loop, the data has been placed in the linked
               !  list successfully.

               found = .TRUE.
               EXIT find_right_level

            !  Oops, we found a duplicate.  This is the same level with the
            !  same name, and since this is a single file, at the same time.
            !  This could happen, for example, if there was an SST data segment
            !  in the first guess file, as well as in a separate SST file.  In any
            !  case, disregard the second occurrence.

            ELSE IF ( ABS(level-old_slab%level) .LT. 1 ) THEN

               !  Inform folks of the situation.

               PRINT '(A,A,A,F10.1,A)','A duplicate variable/level was detected: ',name(1:8),' at level ',level,' Pa.'
               PRINT '(A)','The first field/level data values are retained, &
                           &and all subsequent occurrences are disregarded.'

               !  Decrement the levels counter to get rid of this "bad" value.
  
               current_var%num_levels = current_var%num_levels - 1

               !  EXIT the loop - effectively, leave this routine.  Neither
               !  a new slab (for the new level) nor a new 2d field (for the
               !  actual (JX,IX) field) is generated.  We are exiting this
               !  routine without any changes to the linked list.

               bad = .TRUE.
               EXIT find_right_level

            END IF vertical_search
   
            IF ( rec4%print_link_list_store ) THEN
               PRINT '(A)', 'Have not yet found where the level goes.'
            END IF

            !  We have not found where the new level goes, so 
            !  increment the pointers.

            old_slab => current_slab
            current_slab => current_slab%next_slab

            !  Just in case the duplicate is at the end of the linked list, we must check
            !  against this level now.  In any case, disregard the second occurrence.

            IF ( ABS(level-old_slab%level) .LT. 1 ) THEN

               !  Inform folks of the situation.

               PRINT '(A,A,A,F10.1,A)','A duplicate variable/level was detected: ',name(1:8),' at level ',level,' Pa.'
               PRINT '(A)','The first field/level data values are retained, &
                           &and all subsequent occurrences are disregarded.'

               !  Decrement the levels counter to get rid of this "bad" value.
  
               current_var%num_levels = current_var%num_levels - 1

               !  EXIT the loop - effectively, leave this routine.  Neither
               !  a new slab (for the new level) nor a new 2d field (for the
               !  actual (JX,IX) field) is generated.  We are exiting this
               !  routine without any changes to the linked list.

               bad = .TRUE.
               EXIT find_right_level

            END IF

         END DO find_right_level

         IF ( ( .NOT. found ) .AND. ( .NOT. bad ) ) THEN
            PRINT '(A)', 'Error in finding correct level in data store.'
            STOP 'data_level_store'
         END IF

      END IF first_level_TF

   END IF initialized_TF

   !  Re-initialize the "bad" value flag to .FALSE.  We want to
   !  assume that the incoming data is .NOT. bad.  The bad flag is 
   !  only set when we find a duplicate of the same variable
   !  at the same vertical level for the same valid time.

   bad = .FALSE.
   
END SUBROUTINE proc_grid_store
