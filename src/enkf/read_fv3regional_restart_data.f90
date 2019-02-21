       character(len=*), intent(in) :: varname
       character(len=*), intent(in) :: filename
       integer(i_kind), intent(in) :: file_id
       integer(i_kind) :: var_id
       call nc_check( nf90_inq_varid(file_id,trim(adjustl(varname)),var_id),&
       myname_,'inq_varid '//trim(adjustl(varname))//' '//trim(filename) )
       call nc_check( nf90_get_var(file_id,var_id,data_arr),&
       myname_,'get_var '//trim(adjustl(varname))//' '//trim(filename) )
