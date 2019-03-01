module ncdw_dresize
    use ncd_kinds, only: i_byte, i_short, i_long, i_llong, r_single, &
        r_double
    use ncdw_state, only: diag_data2d_store
    use ncdw_types, only: diag_d2d_iarr, NLAYER_DEFAULT_ENT, &
        NLAYER_MULTI_BASE
    use ncdw_realloc, only: nc_diag_realloc
    use ncdw_climsg, only: &
#ifdef ENABLE_ACTION_MSGS
        nclayer_enable_action, nclayer_actionm, &
#endif
#ifdef _DEBUG_MEM_
        nclayer_debug, &
#endif
        nclayer_error
    
    implicit none
    
    contains
        ! For all subroutines: update_acount_in specifies wheter to
        ! update acount or not. By default, this is true. This is useful
        ! for preallocation, when you aren't actually adding entries,
        ! so you're just allocating ahead of time and NOT adding 
        ! elements, thus not adding to acount.
        
        ! nc_diag_data2d_resize - input integer(i_byte)
        ! Corresponding NetCDF4 type: byte
        subroutine nc_diag_data2d_resize_byte(addl_num_entries, update_acount_in)
            integer(i_llong), intent(in)    :: addl_num_entries
            logical, intent(in), optional   :: update_acount_in
            
            ! This is the Size Count index (sc_index) - we'll just set
            ! this and then just change the variable we're altering
            ! every time.
            integer(i_long)                 :: sc_index
            integer(i_long)                 :: sc_index_vi
            
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
            ! sc_index_vi is just sc_index + 6, 6 being the number of single types
            sc_index = 1
            sc_index_vi = sc_index + 6
            
            if (allocated(diag_data2d_store%m_byte)) then
                if (update_acount) diag_data2d_store%acount(sc_index) = diag_data2d_store%acount(sc_index) + addl_num_entries
                if (diag_data2d_store%acount(sc_index) >= diag_data2d_store%asize(sc_index)) then
#ifdef ENABLE_ACTION_MSGS
                    if (nclayer_enable_action) then
                        call nclayer_actionm("nc_diag_data2d_resize_byte: doing reallocation!")
                    end if
#endif
                    call nc_diag_realloc(diag_data2d_store%m_byte, int8(addl_num_entries + (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** diag_data2d_store%alloc_m_multi(sc_index)))))
                    diag_data2d_store%asize(sc_index) = size(diag_data2d_store%m_byte)
                    
                    diag_data2d_store%alloc_m_multi(sc_index) = diag_data2d_store%alloc_m_multi(sc_index) + 1
                end if
            else
                if (update_acount) diag_data2d_store%acount(sc_index) = addl_num_entries
                allocate(diag_data2d_store%m_byte(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_data2d_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
        end subroutine nc_diag_data2d_resize_byte
        
        ! nc_diag_data2d_resize - input integer(i_short)
        ! Corresponding NetCDF4 type: short
        subroutine nc_diag_data2d_resize_short(addl_num_entries, update_acount_in)
            integer(i_llong), intent(in)    :: addl_num_entries
            logical, intent(in), optional   :: update_acount_in
            
            ! This is the Size Count index (sc_index) - we'll just set
            ! this and then just change the variable we're altering
            ! every time.
            integer(i_long)                 :: sc_index
            integer(i_long)                 :: sc_index_vi
            
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
            ! sc_index_vi is just sc_index + 6, 6 being the number of single types
            sc_index = 2
            sc_index_vi = sc_index + 6
            
            if (allocated(diag_data2d_store%m_short)) then
                if (update_acount) diag_data2d_store%acount(sc_index) = diag_data2d_store%acount(sc_index) + addl_num_entries
                if (diag_data2d_store%acount(sc_index) >= diag_data2d_store%asize(sc_index)) then
#ifdef ENABLE_ACTION_MSGS
                    if (nclayer_enable_action) then
                        call nclayer_actionm("nc_diag_data2d_resize_short: doing reallocation!")
                    end if
#endif
                    call nc_diag_realloc(diag_data2d_store%m_short, int8(addl_num_entries + (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** diag_data2d_store%alloc_m_multi(sc_index)))))
                    diag_data2d_store%asize(sc_index) = size(diag_data2d_store%m_short)
                    
                    diag_data2d_store%alloc_m_multi(sc_index) = diag_data2d_store%alloc_m_multi(sc_index) + 1
                end if
            else
#ifdef _DEBUG_MEM_
                print *, "nc_diag_data2d_resize_short: allocate NEW m_short"
#endif
                if (update_acount) diag_data2d_store%acount(sc_index) = addl_num_entries
                allocate(diag_data2d_store%m_short(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_data2d_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
        end subroutine nc_diag_data2d_resize_short
        
        ! nc_diag_data2d_resize - input integer(i_long)
        ! Corresponding NetCDF4 type: int (old: long)
        subroutine nc_diag_data2d_resize_long(addl_num_entries, update_acount_in)
            integer(i_llong), intent(in)    :: addl_num_entries
            logical, intent(in), optional   :: update_acount_in
            
            ! Did we realloc at all?
            !logical :: data2d_realloc
            
            ! This is the Size Count index (sc_index) - we'll just set
            ! this and then just change the variable we're altering
            ! every time.
            integer(i_long)                 :: sc_index
            integer(i_long)                 :: sc_index_vi
            
#ifdef _DEBUG_MEM_
            character(len=200) :: debugstr
#endif
            
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
            ! sc_index_vi is just sc_index + 6, 6 being the number of single types
            sc_index = 3
            sc_index_vi = sc_index + 6
            
            if (allocated(diag_data2d_store%m_long)) then
                if (update_acount) diag_data2d_store%acount(sc_index) = diag_data2d_store%acount(sc_index) + addl_num_entries
                
#ifdef _DEBUG_MEM_
                write (debugstr, "(A, I1, A, I7, A, I7)") "In sc_index ", sc_index, ", the acount/asize is: ", diag_data2d_store%acount(sc_index), "/", diag_data2d_store%asize(sc_index)
                call nclayer_debug(debugstr)
#endif
                
                if (diag_data2d_store%acount(sc_index) >= diag_data2d_store%asize(sc_index)) then
#ifdef _DEBUG_MEM_
                    call nclayer_debug("acount < asize, reallocating.")
                    print *, "Start long realloc..."
#endif
#ifdef ENABLE_ACTION_MSGS
                    if (nclayer_enable_action) then
                        call nclayer_actionm("nc_diag_data2d_resize_long: doing reallocation!")
                    end if
#endif
                    call nc_diag_realloc(diag_data2d_store%m_long, int8(addl_num_entries + (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** diag_data2d_store%alloc_m_multi(sc_index)))))
                    diag_data2d_store%asize(sc_index) = size(diag_data2d_store%m_long)
                    
                    diag_data2d_store%alloc_m_multi(sc_index) = diag_data2d_store%alloc_m_multi(sc_index) + 1
                    
#ifdef _DEBUG_MEM_
                    print *, "alloc_m_multi increased to:"
                    print *, diag_data2d_store%alloc_m_multi(sc_index)
#endif
                end if
            else
#ifdef _DEBUG_MEM_
                print *, "nc_diag_data2d_resize_long: allocate NEW m_long"
#endif
                if (update_acount) diag_data2d_store%acount(sc_index) = addl_num_entries
                allocate(diag_data2d_store%m_long(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_data2d_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
        end subroutine nc_diag_data2d_resize_long
        
        ! nc_diag_data2d_resize - input real(r_single)
        ! Corresponding NetCDF4 type: float (or real)
        subroutine nc_diag_data2d_resize_rsingle(addl_num_entries, update_acount_in)
            integer(i_llong), intent(in)    :: addl_num_entries
            logical, intent(in), optional   :: update_acount_in
            
            ! This is the Size Count index (sc_index) - we'll just set
            ! this and then just change the variable we're altering
            ! every time.
            integer(i_long)                 :: sc_index
            integer(i_long)                 :: sc_index_vi
            
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
            ! sc_index_vi is just sc_index + 6, 6 being the number of single types
            sc_index = 4
            sc_index_vi = sc_index + 6
            
            if (allocated(diag_data2d_store%m_rsingle)) then
                if (update_acount) diag_data2d_store%acount(sc_index) = diag_data2d_store%acount(sc_index) + addl_num_entries
                if (diag_data2d_store%acount(sc_index) >= diag_data2d_store%asize(sc_index)) then
#ifdef _DEBUG_MEM_
                    print *, "realloc needed for data2d rsingle!"
                    write (*, "(A, I0, A, I0, A)") "(size needed / size available: ", diag_data2d_store%acount(sc_index), " / ", diag_data2d_store%asize(sc_index), ")"
#endif
#ifdef ENABLE_ACTION_MSGS
                    if (nclayer_enable_action) then
                        call nclayer_actionm("nc_diag_data2d_resize_rsingle: doing reallocation!")
                    end if
#endif
                    call nc_diag_realloc(diag_data2d_store%m_rsingle, int8(addl_num_entries + (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** diag_data2d_store%alloc_m_multi(sc_index)))))
                    diag_data2d_store%asize(sc_index) = size(diag_data2d_store%m_rsingle)
                    
                    diag_data2d_store%alloc_m_multi(sc_index) = diag_data2d_store%alloc_m_multi(sc_index) + 1
                end if
            else
                if (update_acount) diag_data2d_store%acount(sc_index) = addl_num_entries
                allocate(diag_data2d_store%m_rsingle(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_data2d_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
        end subroutine nc_diag_data2d_resize_rsingle
        
        ! nc_diag_data2d_resize - input real(r_double)
        ! Corresponding NetCDF4 type: double
        subroutine nc_diag_data2d_resize_rdouble(addl_num_entries, update_acount_in)
            integer(i_llong), intent(in)    :: addl_num_entries
            logical, intent(in), optional   :: update_acount_in
            
            ! This is the Size Count index (sc_index) - we'll just set
            ! this and then just change the variable we're altering
            ! every time.
            integer(i_long)                 :: sc_index
            integer(i_long)                 :: sc_index_vi
            
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
            ! sc_index_vi is just sc_index + 6, 6 being the number of single types
            sc_index = 5
            sc_index_vi = sc_index + 6
            
            if (allocated(diag_data2d_store%m_rdouble)) then
                if (update_acount) diag_data2d_store%acount(sc_index) = diag_data2d_store%acount(sc_index) + addl_num_entries
                if (diag_data2d_store%acount(sc_index) >= diag_data2d_store%asize(sc_index)) then
#ifdef ENABLE_ACTION_MSGS
                    if (nclayer_enable_action) then
                        call nclayer_actionm("nc_diag_data2d_resize_rdouble: doing reallocation!")
                    end if
#endif
                    call nc_diag_realloc(diag_data2d_store%m_rdouble, int8(addl_num_entries + (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** diag_data2d_store%alloc_m_multi(sc_index)))))
                    diag_data2d_store%asize(sc_index) = size(diag_data2d_store%m_rdouble)
                    
                    diag_data2d_store%alloc_m_multi(sc_index) = diag_data2d_store%alloc_m_multi(sc_index) + 1
                end if
            else
                if (update_acount) diag_data2d_store%acount(sc_index) = addl_num_entries
                allocate(diag_data2d_store%m_rdouble(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_data2d_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
        end subroutine nc_diag_data2d_resize_rdouble

        ! nc_diag_data2d_resize - input character(len=*)
        ! Corresponding NetCDF4 type: string? char?
        subroutine nc_diag_data2d_resize_string(addl_num_entries, update_acount_in)
            integer(i_llong), intent(in)    :: addl_num_entries
            logical, intent(in), optional   :: update_acount_in
            
            ! This is the Size Count index (sc_index) - we'll just set
            ! this and then just change the variable we're altering
            ! every time.
            integer(i_long)                 :: sc_index
            integer(i_long)                 :: sc_index_vi
            
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
            
            ! NLAYER_BYTE is located at the sixth index, 6.
            ! sc_index_vi is just sc_index + 6, 6 being the number of single types
            sc_index = 6
            sc_index_vi = sc_index + 6
            
            if (allocated(diag_data2d_store%m_string)) then
                if (update_acount) diag_data2d_store%acount(sc_index) = diag_data2d_store%acount(sc_index) + addl_num_entries
                if (diag_data2d_store%acount(sc_index) >= diag_data2d_store%asize(sc_index)) then
#ifdef ENABLE_ACTION_MSGS
                    if (nclayer_enable_action) then
                        call nclayer_actionm("nc_diag_data2d_resize_string: doing reallocation!")
                    end if
#endif
                    call nc_diag_realloc(diag_data2d_store%m_string, int8(addl_num_entries  + (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** diag_data2d_store%alloc_m_multi(sc_index)))))
                    diag_data2d_store%asize(sc_index) = size(diag_data2d_store%m_string)
                    
                    diag_data2d_store%alloc_m_multi(sc_index) = diag_data2d_store%alloc_m_multi(sc_index) + 1
                end if
            else
                if (update_acount) diag_data2d_store%acount(sc_index) = addl_num_entries
                allocate(diag_data2d_store%m_string(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_data2d_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
        end subroutine nc_diag_data2d_resize_string
        
        subroutine nc_diag_data2d_resize_iarr_type(addl_num_entries)
            integer(i_llong), intent(in)    :: addl_num_entries
            
            type(diag_d2d_iarr), dimension(:), allocatable   :: tmp_stor_i_arr
            
#ifdef ENABLE_ACTION_MSGS
            if (nclayer_enable_action) then
                call nclayer_actionm("nc_diag_data2d_resize_iarr_type: doing reallocation!")
            end if
#endif
            
            ! We need to realloc ourselves here...
            allocate(tmp_stor_i_arr(size(diag_data2d_store%stor_i_arr) + addl_num_entries))
            tmp_stor_i_arr(1:size(diag_data2d_store%stor_i_arr)) = diag_data2d_store%stor_i_arr
            deallocate(diag_data2d_store%stor_i_arr)
            allocate(diag_data2d_store%stor_i_arr(size(tmp_stor_i_arr)))
            diag_data2d_store%stor_i_arr = tmp_stor_i_arr
            deallocate(tmp_stor_i_arr)
        end subroutine nc_diag_data2d_resize_iarr_type
        
        subroutine nc_diag_data2d_resize_iarr(iarr_index, addl_num_entries, update_icount_in)
            integer(i_long), intent(in)     :: iarr_index
            integer(i_llong), intent(in)    :: addl_num_entries
            logical, intent(in), optional   :: update_icount_in
            
            logical                         :: update_icount
            
            integer(i_llong)                :: addl_num_entries_r
            
            ! Assume true by default
            if (.NOT. present(update_icount_in)) then
                update_icount = .TRUE.
            else
                update_icount = update_icount_in
            end if
            
            if (allocated(diag_data2d_store%stor_i_arr(iarr_index)%index_arr)) then
                if (update_icount) diag_data2d_store%stor_i_arr(iarr_index)%icount = &
                    diag_data2d_store%stor_i_arr(iarr_index)%icount + addl_num_entries
                if (diag_data2d_store%stor_i_arr(iarr_index)%icount >= diag_data2d_store%stor_i_arr(iarr_index)%isize) then
#ifdef _DEBUG_MEM_
                    print *, "realloc needed for data2d iarr!"
                    write (*, "(A, I0, A, I0, A)") "(size needed / size available: ", diag_data2d_store%stor_i_arr(iarr_index)%icount, " / ", diag_data2d_store%stor_i_arr(iarr_index)%isize, ")"
                    print *, diag_data2d_store%alloc_sia_multi(iarr_index)
                    print *, int8(NLAYER_MULTI_BASE ** int8(diag_data2d_store%alloc_sia_multi(iarr_index)))
#endif
#ifdef ENABLE_ACTION_MSGS
                    if (nclayer_enable_action) then
                        call nclayer_actionm("nc_diag_data2d_resize_iarr: doing reallocation!")
                    end if
#endif
                    
                    if (update_icount) then
                        addl_num_entries_r = addl_num_entries + (int8(NLAYER_DEFAULT_ENT) * (NLAYER_MULTI_BASE ** int8(diag_data2d_store%alloc_sia_multi(iarr_index))))
                    else
                        addl_num_entries_r = addl_num_entries + NLAYER_DEFAULT_ENT
                    end if
#ifdef _DEBUG_MEM_
                    print *, " ** addl_num_entries_r = "
                    print *, addl_num_entries_r
#endif
                    call nc_diag_realloc(diag_data2d_store%stor_i_arr(iarr_index)%index_arr, addl_num_entries_r)
                    call nc_diag_realloc(diag_data2d_store%stor_i_arr(iarr_index)%length_arr, addl_num_entries_r)
#ifdef _DEBUG_MEM_
                    print *, " ** realloc done"
#endif
                    diag_data2d_store%stor_i_arr(iarr_index)%isize = size(diag_data2d_store%stor_i_arr(iarr_index)%index_arr)
                    
                    if (update_icount) diag_data2d_store%alloc_sia_multi(iarr_index) = diag_data2d_store%alloc_sia_multi(iarr_index) + 1
                end if
            else
                if (update_icount) diag_data2d_store%stor_i_arr(iarr_index)%icount = addl_num_entries
                allocate(diag_data2d_store%stor_i_arr(iarr_index)%index_arr(addl_num_entries + NLAYER_DEFAULT_ENT))
                allocate(diag_data2d_store%stor_i_arr(iarr_index)%length_arr(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_data2d_store%stor_i_arr(iarr_index)%isize = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
        end subroutine nc_diag_data2d_resize_iarr
end module ncdw_dresize
