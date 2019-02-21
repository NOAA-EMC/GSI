       call nc_check( nf90_inq_varid(file_id,trim(adjustl(varname)),var_id),&
       myname_,'inq_varid '//trim(adjustl(varname))//' '//trim(filename) )
       call nc_check( nf90_put_var(file_id,var_id,data_arr),&
       myname_,'get_var '//trim(adjustl(varname))//' '//trim(filename) )
