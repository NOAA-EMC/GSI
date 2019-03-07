! nc_diag_write - NetCDF Layer Diag Writing Module
! Copyright 2015 Albert Huang - SSAI/NASA for NASA GSFC GMAO (610.1).
! 
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
! 
!   http://www.apache.org/licenses/LICENSE-2.0
! 
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
! implied. See the License for the specific language governing
! permissions and limitations under the License.
! 
! chaninfo variable data storage resizing module - ncdw_ciresize
!
module ncdw_ciresize
    ! Module that provides chaninfo variable data storage resizing.
    ! 
    ! This module has all of the subroutines needed to resize chaninfo
    ! variable data storage. It includes resizing subroutines for all
    ! variable data storage types, including:
    !   integer(i_byte) for byte integer storage
    !   integer(i_short) for short integer storage
    !   integer(i_long) for long integer storage
    !   real(r_single) for float storage
    !   real(r_double) for double storage
    !   character(len=*) for string storage
    ! 
    ! The subroutines here serve as "smart" wrappers for the real
    ! reallocation subroutines in ncdw_realloc. 
    ! 
    ! For each subroutine:
    ! 
    !  -> It first checks if the type-specific variable data storage
    !     field (ci_*) has been allocated or not.
    !     
    !  -> If it hasn't been allocated:
    !     -> If the storage count is to be updated, it is set to the
    !        specified number of entries.
    !     -> The field is then allocated with the specified number of
    !        entries, plus the default initial number of entries.
    !     
    !  -> If it has been allocated:
    !     -> If the storage count is to be updated, the number of fields
    !        to allocate for are added to the count.
    !     -> The (potentially updated) field storage count is checked
    !        against the cached allocated size.
    !     -> If the count is greater than or equal to the cached
    !        allocated size, the proper reallocation subroutine from
    !        nc_diag_realloc is called, the cached allocated size is
    !        updated to the new size, and the allocation multiplier is
    !        incremented.
    !     -> Otherwise, nothing happens.
    ! 
    
    ! Load our numerical types from kinds
    ! Note that i_llong is not a type we store - it's just for keeping
    ! track of numeric indexes. (Maybe this is too excessive...)
    use ncd_kinds, only: i_byte, i_short, i_long, i_llong, r_single, &
        r_double
    
    ! Load state variables! We just need to know:
    !   diag_chaninfo_store - ...chaninfo variable information.
    !                         We pretty much do everything related to
    !                         chaninfo here, so we're using everything
    !                         inside this derived type! (Especially the
    !                         variable data storage fields, ci_*!)
    use ncdw_state, only: diag_chaninfo_store
    
    ! Load types! We need:
    !   NLAYER_DEFAULT_ENT - default starting number of element entries.
    !                        This is for the initial allocation of
    !                        space for data storage arrays, e.g.
    !                        the ci_* data arrays within diag_chaninfo.
    !   NLAYER_MULTI_BASE  - the base number to use when exponentiating
    !                        to allocate or reallocate data storage
    !                        arrays.
    use ncdw_types, only: NLAYER_DEFAULT_ENT, NLAYER_MULTI_BASE
    
    ! Load our fun reallocation subroutine - we need this to reallocate
    ! within our "smart" chaninfo reallocation subroutines:
    use ncdw_realloc, only: nc_diag_realloc
    
#ifdef ENABLE_ACTION_MSGS
    use ncdw_climsg, only: nclayer_enable_action, nclayer_actionm
#endif
    
    implicit none
    
    contains
        ! Make enough space in the internal variable data storage field
        ! for byte integer storage.
        ! 
        ! This attempts to resize the internal variable data storage
        ! field to accompany additional entries. If the size is already
        ! big enough to fit the existing data plus the additional
        ! entries, no actual memory reallocation will occur.
        ! 
        ! The storage count for the type is also updated, unless
        ! otherwise optionally disabled via an optional argument.
        ! 
        ! Disabling the storage count update can be useful for
        ! preallocation, where the preallocation can occur without
        ! updating the count, since the count stores the amount of data
        ! stored in the storage field. Since preallocation does not
        ! store any data, the count updating should be disabled.
        ! 
        ! This is an internal subroutine, and is NOT meant to be called
        ! outside of nc_diag_write. Calling this subroutine in your
        ! program may result in unexpected behavior and/or data
        ! corruption!
        ! 
        ! Args:
        !     addl_num_entries (integer(i_llong)): the number of entries
        !         to make enough space for.
        !     update_acount_in (logical, optional): whether to update
        !         the internal variable data storage count or not. If
        !         not specified, the count will be updated.
        !     
        ! Raises:
        !     The following errors will trigger indirectly from other
        !     subroutines called here:
        !     
        !     If data reallocation fails, this will result in an error.
        !     
        !     Other errors may result from invalid data storage, NetCDF
        !     errors, or even a bug. See the called subroutines'
        !     documentation for details.
        ! 
        subroutine nc_diag_chaninfo_resize_byte(addl_num_entries, update_acount_in)
            integer(i_llong), intent(in)    :: addl_num_entries
            logical, intent(in), optional   :: update_acount_in
            
            ! This is the Size Count index (sc_index) - we'll just set
            ! this and then just change the variable we're altering
            ! every time.
            integer(i_long)                 :: sc_index
            
            logical                         :: update_acount
            
            ! Assume true by default
            if (.NOT. present(update_acount_in)) then
                update_acount = .TRUE.
            else
                update_acount = update_acount_in
            end if
            
            ! Here, we increment the count by the number of additional entries,
            ! and the size by that amount as well. 
            ! 
            ! If we didn't allocate yet, we simply set the count to the number of
            ! initial entries, and then allocate that number + our default
            ! initialization amount. Our initial size is that number + the initial
            ! amount.
            
            ! NLAYER_BYTE is located at the first index, 1.
            sc_index = 1
            
            ! Check if the variable data storage field is allocated
            if (allocated(diag_chaninfo_store%ci_byte)) then
                ! If necessary, update the variable data storage count
                if (update_acount) diag_chaninfo_store%acount(sc_index) = diag_chaninfo_store%acount(sc_index) + addl_num_entries
                
                ! Check to see if we have enough memory space
                if (diag_chaninfo_store%acount(sc_index) >= diag_chaninfo_store%asize(sc_index)) then
#ifdef ENABLE_ACTION_MSGS
                    if (nclayer_enable_action) then
                        call nclayer_actionm("nc_diag_chaninfo_resize_byte: doing reallocation!")
                    end if
#endif
                    ! Reallocate to grow the variable data storage array
                    call nc_diag_realloc(diag_chaninfo_store%ci_byte, int8(addl_num_entries + (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** diag_chaninfo_store%alloc_multi))))
                    
                    ! Update the variable storage size with the new
                    ! reallocated size
                    diag_chaninfo_store%asize(sc_index) = size(diag_chaninfo_store%ci_byte)
                    
                    ! Increment the allocation multiplier
                    diag_chaninfo_store%alloc_multi = diag_chaninfo_store%alloc_multi + 1
                end if
            else
                ! If necessary, update the variable data storage count
                if (update_acount) diag_chaninfo_store%acount(sc_index) = addl_num_entries

                ! Allocate the number of entries to add + default
                ! initial size
                allocate(diag_chaninfo_store%ci_byte(addl_num_entries + NLAYER_DEFAULT_ENT))
                
                ! Set variable storage size to same amount
                diag_chaninfo_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
        end subroutine nc_diag_chaninfo_resize_byte
        
        ! Make enough space in the internal variable data storage field
        ! for short integer storage.
        ! 
        ! This attempts to resize the internal variable data storage
        ! field to accompany additional entries. If the size is already
        ! big enough to fit the existing data plus the additional
        ! entries, no actual memory reallocation will occur.
        ! 
        ! The storage count for the type is also updated, unless
        ! otherwise optionally disabled via an optional argument.
        ! 
        ! Disabling the storage count update can be useful for
        ! preallocation, where the preallocation can occur without
        ! updating the count, since the count stores the amount of data
        ! stored in the storage field. Since preallocation does not
        ! store any data, the count updating should be disabled.
        ! 
        ! This is an internal subroutine, and is NOT meant to be called
        ! outside of nc_diag_write. Calling this subroutine in your
        ! program may result in unexpected behavior and/or data
        ! corruption!
        ! 
        ! Args:
        !     addl_num_entries (integer(i_llong)): the number of entries
        !         to make enough space for.
        !     update_acount_in (logical, optional): whether to update
        !         the internal variable data storage count or not. If
        !         not specified, the count will be updated.
        !     
        ! Raises:
        !     The following errors will trigger indirectly from other
        !     subroutines called here:
        !     
        !     If data reallocation fails, this will result in an error.
        !     
        !     Other errors may result from invalid data storage, NetCDF
        !     errors, or even a bug. See the called subroutines'
        !     documentation for details.
        ! 
        subroutine nc_diag_chaninfo_resize_short(addl_num_entries, update_acount_in)
            integer(i_llong), intent(in)    :: addl_num_entries
            logical, intent(in), optional   :: update_acount_in
            
            ! This is the Size Count index (sc_index) - we'll just set
            ! this and then just change the variable we're altering
            ! every time.
            integer(i_long)                 :: sc_index
            
            logical                         :: update_acount
            
            ! Assume true by default
            if (.NOT. present(update_acount_in)) then
                update_acount = .TRUE.
            else
                update_acount = update_acount_in
            end if
            
            ! Here, we increment the count by the number of additional entries,
            ! and the size by that amount as well. 
            ! 
            ! If we didn't allocate yet, we simply set the count to the number of
            ! initial entries, and then allocate that number + our default
            ! initialization amount. Our initial size is that number + the initial
            ! amount.
            
            ! NLAYER_SHORT is located at the second index, 2.
            sc_index = 2
            
            ! Check if the variable data storage field is allocated
            if (allocated(diag_chaninfo_store%ci_short)) then
                ! If necessary, update the variable data storage count
                if (update_acount) diag_chaninfo_store%acount(sc_index) = diag_chaninfo_store%acount(sc_index) + addl_num_entries
                
                ! Check to see if we have enough memory space
                if (diag_chaninfo_store%acount(sc_index) >= diag_chaninfo_store%asize(sc_index)) then
#ifdef ENABLE_ACTION_MSGS
                    if (nclayer_enable_action) then
                        call nclayer_actionm("nc_diag_chaninfo_resize_short: doing reallocation!")
                    end if
#endif
                    ! Reallocate to grow the variable data storage array
                    call nc_diag_realloc(diag_chaninfo_store%ci_short, int8(addl_num_entries + (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** diag_chaninfo_store%alloc_multi))))
                    
                    ! Update the variable storage size with the new
                    ! reallocated size
                    diag_chaninfo_store%asize(sc_index) = size(diag_chaninfo_store%ci_short)
                    
                    ! Increment the allocation multiplier
                    diag_chaninfo_store%alloc_multi = diag_chaninfo_store%alloc_multi + 1
                end if
            else
                ! If necessary, update the variable data storage count
                if (update_acount) diag_chaninfo_store%acount(sc_index) = addl_num_entries

                ! Allocate the number of entries to add + default
                ! initial size
                allocate(diag_chaninfo_store%ci_short(addl_num_entries + NLAYER_DEFAULT_ENT))
                
                ! Set variable storage size to same amount
                diag_chaninfo_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
        end subroutine nc_diag_chaninfo_resize_short
        
        ! Make enough space in the internal variable data storage field
        ! for long integer storage.
        ! 
        ! This attempts to resize the internal variable data storage
        ! field to accompany additional entries. If the size is already
        ! big enough to fit the existing data plus the additional
        ! entries, no actual memory reallocation will occur.
        ! 
        ! The storage count for the type is also updated, unless
        ! otherwise optionally disabled via an optional argument.
        ! 
        ! Disabling the storage count update can be useful for
        ! preallocation, where the preallocation can occur without
        ! updating the count, since the count stores the amount of data
        ! stored in the storage field. Since preallocation does not
        ! store any data, the count updating should be disabled.
        ! 
        ! This is an internal subroutine, and is NOT meant to be called
        ! outside of nc_diag_write. Calling this subroutine in your
        ! program may result in unexpected behavior and/or data
        ! corruption!
        ! 
        ! Args:
        !     addl_num_entries (integer(i_llong)): the number of entries
        !         to make enough space for.
        !     update_acount_in (logical, optional): whether to update
        !         the internal variable data storage count or not. If
        !         not specified, the count will be updated.
        !     
        ! Raises:
        !     The following errors will trigger indirectly from other
        !     subroutines called here:
        !     
        !     If data reallocation fails, this will result in an error.
        !     
        !     Other errors may result from invalid data storage, NetCDF
        !     errors, or even a bug. See the called subroutines'
        !     documentation for details.
        ! 
        subroutine nc_diag_chaninfo_resize_long(addl_num_entries, update_acount_in)
            integer(i_llong), intent(in)    :: addl_num_entries
            logical, intent(in), optional   :: update_acount_in
            
            ! Did we realloc at all?
            !logical :: chaninfo_realloc
            
            ! This is the Size Count index (sc_index) - we'll just set
            ! this and then just change the variable we're altering
            ! every time.
            integer(i_long)                 :: sc_index
            
            logical                         :: update_acount
            
            ! Assume true by default
            if (.NOT. present(update_acount_in)) then
                update_acount = .TRUE.
            else
                update_acount = update_acount_in
            end if
            
            ! Here, we increment the count by the number of additional entries,
            ! and the size by that amount as well. 
            ! 
            ! If we didn't allocate yet, we simply set the count to the number of
            ! initial entries, and then allocate that number + our default
            ! initialization amount. Our initial size is that number + the initial
            ! amount.
            
            ! NLAYER_LONG is located at the third index, 3.
            sc_index = 3
            
            ! Check if the variable data storage field is allocated
            if (allocated(diag_chaninfo_store%ci_long)) then
                ! If necessary, update the variable data storage count
                if (update_acount) diag_chaninfo_store%acount(sc_index) = diag_chaninfo_store%acount(sc_index) + addl_num_entries
                
                
                ! Check to see if we have enough memory space
                if (diag_chaninfo_store%acount(sc_index) >= diag_chaninfo_store%asize(sc_index)) then
#ifdef _DEBUG_MEM_
                    print *, "realloc needed for chaninfo long!"
                    write (*, "(A, I0, A, I0, A)") "(size needed / size available: ", diag_chaninfo_store%acount(sc_index), " / ", diag_chaninfo_store%asize(sc_index), ")"
#endif
#ifdef ENABLE_ACTION_MSGS
                    if (nclayer_enable_action) then
                        call nclayer_actionm("nc_diag_chaninfo_resize_long: doing reallocation!")
                    end if
#endif
                    ! Reallocate to grow the variable data storage array
                    call nc_diag_realloc(diag_chaninfo_store%ci_long, int8(addl_num_entries + (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** diag_chaninfo_store%alloc_multi))))
                    
                    ! Update the variable storage size with the new
                    ! reallocated size
                    diag_chaninfo_store%asize(sc_index) = size(diag_chaninfo_store%ci_long)
                    
                    ! Increment the allocation multiplier
                    diag_chaninfo_store%alloc_multi = diag_chaninfo_store%alloc_multi + 1
                end if
            else
                ! If necessary, update the variable data storage count
                if (update_acount) diag_chaninfo_store%acount(sc_index) = addl_num_entries

                ! Allocate the number of entries to add + default
                ! initial size
                allocate(diag_chaninfo_store%ci_long(addl_num_entries + NLAYER_DEFAULT_ENT))
                
                ! Set variable storage size to same amount
                diag_chaninfo_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
        end subroutine nc_diag_chaninfo_resize_long
        
        ! Make enough space in the internal variable data storage field
        ! for float storage.
        ! 
        ! This attempts to resize the internal variable data storage
        ! field to accompany additional entries. If the size is already
        ! big enough to fit the existing data plus the additional
        ! entries, no actual memory reallocation will occur.
        ! 
        ! The storage count for the type is also updated, unless
        ! otherwise optionally disabled via an optional argument.
        ! 
        ! Disabling the storage count update can be useful for
        ! preallocation, where the preallocation can occur without
        ! updating the count, since the count stores the amount of data
        ! stored in the storage field. Since preallocation does not
        ! store any data, the count updating should be disabled.
        ! 
        ! This is an internal subroutine, and is NOT meant to be called
        ! outside of nc_diag_write. Calling this subroutine in your
        ! program may result in unexpected behavior and/or data
        ! corruption!
        ! 
        ! Args:
        !     addl_num_entries (integer(i_llong)): the number of entries
        !         to make enough space for.
        !     update_acount_in (logical, optional): whether to update
        !         the internal variable data storage count or not. If
        !         not specified, the count will be updated.
        !     
        ! Raises:
        !     The following errors will trigger indirectly from other
        !     subroutines called here:
        !     
        !     If data reallocation fails, this will result in an error.
        !     
        !     Other errors may result from invalid data storage, NetCDF
        !     errors, or even a bug. See the called subroutines'
        !     documentation for details.
        ! 
        subroutine nc_diag_chaninfo_resize_rsingle(addl_num_entries, update_acount_in)
            integer(i_llong), intent(in)    :: addl_num_entries
            logical, intent(in), optional   :: update_acount_in
            
            ! This is the Size Count index (sc_index) - we'll just set
            ! this and then just change the variable we're altering
            ! every time.
            integer(i_long)                 :: sc_index
            
            logical                         :: update_acount
            
            ! Assume true by default
            if (.NOT. present(update_acount_in)) then
                update_acount = .TRUE.
            else
                update_acount = update_acount_in
            end if
            
            ! Here, we increment the count by the number of additional entries,
            ! and the size by that amount as well. 
            ! 
            ! If we didn't allocate yet, we simply set the count to the number of
            ! initial entries, and then allocate that number + our default
            ! initialization amount. Our initial size is that number + the initial
            ! amount.
            
            ! NLAYER_FLOAT is located at the fourth index, 4.
            sc_index = 4
            
            ! Check if the variable data storage field is allocated
            if (allocated(diag_chaninfo_store%ci_rsingle)) then
                ! If necessary, update the variable data storage count
                if (update_acount) diag_chaninfo_store%acount(sc_index) = diag_chaninfo_store%acount(sc_index) + addl_num_entries
                
                ! Check to see if we have enough memory space
                if (diag_chaninfo_store%acount(sc_index) >= diag_chaninfo_store%asize(sc_index)) then
#ifdef ENABLE_ACTION_MSGS
                    if (nclayer_enable_action) then
                        call nclayer_actionm("nc_diag_chaninfo_resize_rsingle: doing reallocation!")
                    end if
#endif
                    ! Reallocate to grow the variable data storage array
                    call nc_diag_realloc(diag_chaninfo_store%ci_rsingle, int8(addl_num_entries + (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** diag_chaninfo_store%alloc_multi))))
                    
                    ! Update the variable storage size with the new
                    ! reallocated size
                    diag_chaninfo_store%asize(sc_index) = size(diag_chaninfo_store%ci_rsingle)
                    
                    ! Increment the allocation multiplier
                    diag_chaninfo_store%alloc_multi = diag_chaninfo_store%alloc_multi + 1
                end if
            else
                ! If necessary, update the variable data storage count
                if (update_acount) diag_chaninfo_store%acount(sc_index) = addl_num_entries

                ! Allocate the number of entries to add + default
                ! initial size
                allocate(diag_chaninfo_store%ci_rsingle(addl_num_entries + NLAYER_DEFAULT_ENT))
                
                ! Set variable storage size to same amount
                diag_chaninfo_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
        end subroutine nc_diag_chaninfo_resize_rsingle
        
        ! Make enough space in the internal variable data storage field
        ! for double storage.
        ! 
        ! This attempts to resize the internal variable data storage
        ! field to accompany additional entries. If the size is already
        ! big enough to fit the existing data plus the additional
        ! entries, no actual memory reallocation will occur.
        ! 
        ! The storage count for the type is also updated, unless
        ! otherwise optionally disabled via an optional argument.
        ! 
        ! Disabling the storage count update can be useful for
        ! preallocation, where the preallocation can occur without
        ! updating the count, since the count stores the amount of data
        ! stored in the storage field. Since preallocation does not
        ! store any data, the count updating should be disabled.
        ! 
        ! This is an internal subroutine, and is NOT meant to be called
        ! outside of nc_diag_write. Calling this subroutine in your
        ! program may result in unexpected behavior and/or data
        ! corruption!
        ! 
        ! Args:
        !     addl_num_entries (integer(i_llong)): the number of entries
        !         to make enough space for.
        !     update_acount_in (logical, optional): whether to update
        !         the internal variable data storage count or not. If
        !         not specified, the count will be updated.
        !     
        ! Raises:
        !     The following errors will trigger indirectly from other
        !     subroutines called here:
        !     
        !     If data reallocation fails, this will result in an error.
        !     
        !     Other errors may result from invalid data storage, NetCDF
        !     errors, or even a bug. See the called subroutines'
        !     documentation for details.
        ! 
        subroutine nc_diag_chaninfo_resize_rdouble(addl_num_entries, update_acount_in)
            integer(i_llong), intent(in)    :: addl_num_entries
            logical, intent(in), optional   :: update_acount_in
            
            ! This is the Size Count index (sc_index) - we'll just set
            ! this and then just change the variable we're altering
            ! every time.
            integer(i_long)                 :: sc_index
            
            logical                         :: update_acount
            
            ! Assume true by default
            if (.NOT. present(update_acount_in)) then
                update_acount = .TRUE.
            else
                update_acount = update_acount_in
            end if
            
            ! Here, we increment the count by the number of additional entries,
            ! and the size by that amount as well. 
            ! 
            ! If we didn't allocate yet, we simply set the count to the number of
            ! initial entries, and then allocate that number + our default
            ! initialization amount. Our initial size is that number + the initial
            ! amount.
            
            ! NLAYER_DOUBLE is located at the fifth index, 5.
            sc_index = 5
            
            ! Check if the variable data storage field is allocated
            if (allocated(diag_chaninfo_store%ci_rdouble)) then
                ! If necessary, update the variable data storage count
                if (update_acount) diag_chaninfo_store%acount(sc_index) = diag_chaninfo_store%acount(sc_index) + addl_num_entries
                
                ! Check to see if we have enough memory space
                if (diag_chaninfo_store%acount(sc_index) >= diag_chaninfo_store%asize(sc_index)) then
#ifdef ENABLE_ACTION_MSGS
                    if (nclayer_enable_action) then
                        call nclayer_actionm("nc_diag_chaninfo_resize_rdouble: doing reallocation!")
                    end if
#endif
                    ! Reallocate to grow the variable data storage array
                    call nc_diag_realloc(diag_chaninfo_store%ci_rdouble, int8(addl_num_entries + (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** diag_chaninfo_store%alloc_multi))))
                    
                    ! Update the variable storage size with the new
                    ! reallocated size
                    diag_chaninfo_store%asize(sc_index) = size(diag_chaninfo_store%ci_rdouble)
                    
                    ! Increment the allocation multiplier
                    diag_chaninfo_store%alloc_multi = diag_chaninfo_store%alloc_multi + 1
                end if
            else
                ! If necessary, update the variable data storage count
                if (update_acount) diag_chaninfo_store%acount(sc_index) = addl_num_entries

                ! Allocate the number of entries to add + default
                ! initial size
                allocate(diag_chaninfo_store%ci_rdouble(addl_num_entries + NLAYER_DEFAULT_ENT))
                
                ! Set variable storage size to same amount
                diag_chaninfo_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
        end subroutine nc_diag_chaninfo_resize_rdouble

        ! Make enough space in the internal variable data storage field
        ! for string storage.
        ! 
        ! This attempts to resize the internal variable data storage
        ! field to accompany additional entries. If the size is already
        ! big enough to fit the existing data plus the additional
        ! entries, no actual memory reallocation will occur.
        ! 
        ! The storage count for the type is also updated, unless
        ! otherwise optionally disabled via an optional argument.
        ! 
        ! Disabling the storage count update can be useful for
        ! preallocation, where the preallocation can occur without
        ! updating the count, since the count stores the amount of data
        ! stored in the storage field. Since preallocation does not
        ! store any data, the count updating should be disabled.
        ! 
        ! This is an internal subroutine, and is NOT meant to be called
        ! outside of nc_diag_write. Calling this subroutine in your
        ! program may result in unexpected behavior and/or data
        ! corruption!
        ! 
        ! Args:
        !     addl_num_entries (integer(i_llong)): the number of entries
        !         to make enough space for.
        !     update_acount_in (logical, optional): whether to update
        !         the internal variable data storage count or not. If
        !         not specified, the count will be updated.
        !     
        ! Raises:
        !     The following errors will trigger indirectly from other
        !     subroutines called here:
        !     
        !     If data reallocation fails, this will result in an error.
        !     
        !     Other errors may result from invalid data storage, NetCDF
        !     errors, or even a bug. See the called subroutines'
        !     documentation for details.
        ! 
        subroutine nc_diag_chaninfo_resize_string(addl_num_entries, update_acount_in)
            integer(i_llong), intent(in)    :: addl_num_entries
            logical, intent(in), optional   :: update_acount_in
            
            ! This is the Size Count index (sc_index) - we'll just set
            ! this and then just change the variable we're altering
            ! every time.
            integer(i_long)                 :: sc_index
            
            logical                         :: update_acount
            
            ! Assume true by default
            if (.NOT. present(update_acount_in)) then
                update_acount = .TRUE.
            else
                update_acount = update_acount_in
            end if
            
            ! Here, we increment the count by the number of additional entries,
            ! and the size by that amount as well. 
            ! 
            ! If we didn't allocate yet, we simply set the count to the number of
            ! initial entries, and then allocate that number + our default
            ! initialization amount. Our initial size is that number + the initial
            ! amount.
            
            ! NLAYER_STRING is located at the sixth index, 6.
            sc_index = 6
            
            ! Check if the variable data storage field is allocated
            if (allocated(diag_chaninfo_store%ci_string)) then
                ! If necessary, update the variable data storage count
                if (update_acount) diag_chaninfo_store%acount(sc_index) = diag_chaninfo_store%acount(sc_index) + addl_num_entries
                
                ! Check to see if we have enough memory space
                if (diag_chaninfo_store%acount(sc_index) >= diag_chaninfo_store%asize(sc_index)) then
#ifdef ENABLE_ACTION_MSGS
                    if (nclayer_enable_action) then
                        call nclayer_actionm("nc_diag_chaninfo_resize_string: doing reallocation!")
                    end if
#endif
                    ! Reallocate to grow the variable data storage array
                    call nc_diag_realloc(diag_chaninfo_store%ci_string, int8(addl_num_entries + (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** diag_chaninfo_store%alloc_multi))))
                    
                    ! Update the variable storage size with the new
                    ! reallocated size
                    diag_chaninfo_store%asize(sc_index) = size(diag_chaninfo_store%ci_string)
                    
                    ! Increment the allocation multiplier
                    diag_chaninfo_store%alloc_multi = diag_chaninfo_store%alloc_multi + 1
                end if
            else
                ! If necessary, update the variable data storage count
                if (update_acount) diag_chaninfo_store%acount(sc_index) = addl_num_entries

                ! Allocate the number of entries to add + default
                ! initial size
                allocate(diag_chaninfo_store%ci_string(addl_num_entries + NLAYER_DEFAULT_ENT))
                
                ! Set variable storage size to same amount
                diag_chaninfo_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
        end subroutine nc_diag_chaninfo_resize_string
end module ncdw_ciresize
