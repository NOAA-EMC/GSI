!###########################################################################
! Program history log:
! 2014-02-24  J.Jin      -- READ GMI 1B/1C proxy data in HDF5 format
! 2014-06-10  ejones     -- Read GMI 1CR data in HDF5 format
! 2014-06-23  J.Jin      -- Made this general HDF5 reading module for integer and 
!                           real type datasets in order to reorganize the GMI HDF
!                           reading subroutine.
!                          
!
!   Subprograms called:
!     Library:
!       HDF5      - h5open_f  h5close_f      h5fopen_f     h5fclose_f
!                   h5gopen_f h5gclose_f
!                   h5aopen_f h5aget_space_f h5aget_info_f h5aget_type_f
!                   h5aread_f h5aclose_f
!                   h5dopen_f h5dread_f      h5dclose_f
!
!###########################################################################

module hdf5rd_mod
 
  USE HDF5 
!---------------------------------------------------------------------------
  IMPLICIT NONE
  INTEGER(HID_T)  :: file_id, group_id, dset_id, attr_id, space_id, memtype_id,&
                     type_id, error, subgroup_id, subdset_id


contains


  subroutine hdf5rd_i(inputFileName,groupname,subgroupname,dsetname,dim1,dim2,dim3,dataset)
!
! read hdf5 integer dataset

! input:  inputFileName,groupname,dsetname,dim1,dim2,dim3
! output: dataset

  CHARACTER (LEN = 80),intent(in)      :: inputFileName, groupname, subgroupname, dsetname
  integer, intent(in)                  :: dim1,dim2,dim3
  integer, intent(out)                 :: dataset(dim1,dim2,dim3)
  INTEGER(HSIZE_T),DIMENSION(:), allocatable :: data_dims


  if (dim1 > 1 .and. dim2 > 1 .and. dim3 > 1) then
     allocate( data_dims(3) )
     data_dims(1)=dim1
     data_dims(2)=dim2
     data_dims(3)=dim3
  else if (dim1 > 1 .and. dim2 > 1 .and. dim3 ==1) then
     allocate( data_dims(2) )
     data_dims(1)=dim1
     data_dims(2)=dim2
  else if (dim1 > 1 .and. dim2 ==1 .and. dim3 ==1) then
     allocate( data_dims(1) )
     data_dims(1)=dim1
  else 
     write(6,*) 'dim1, dim2, dim3 must >=1. '
     write(6,*) 'stop at hdf5rd_i'
     return 
  endif

  ! Initialize FORTRAN interface.
  CALL h5open_f(error)
  ! Open file to read VD (table) information
  call h5fopen_f(trim(inputFileName), H5F_ACC_RDONLY_F, file_id, error)
  if(error /=0) call error_message(1, inputFileName)
  call h5gopen_f(file_id, trim(groupname), group_id, error)
  if(error /=0) call error_message(2, groupname)
  if( len(trim(subgroupname)) > 0 ) then
     call h5gopen_f(group_id, trim(subgroupname), subgroup_id, error)
     if(error /=0) call error_message(3, subgroupname)
       call h5dopen_f(subgroup_id, trim(dsetname), dset_id, error)
       if(error /=0) call error_message(4, dsetname) 
       call h5dget_type_f(dset_id, type_id, error)
       !call h5dread_f(dset_id, type_id, dataset, data_dims, error)
       call h5dread_f(dset_id, H5T_NATIVE_INTEGER, dataset, data_dims, error)
       print *, 'read: ', trim(groupname),'/',trim(subgroupname), '/', trim(dsetname)
       call h5dclose_f(dset_id, error)
     call h5gclose_f(subgroup_id, error)
  else
       call h5dopen_f(group_id, trim(dsetname), dset_id, error)
       if(error /=0) call error_message(4, dsetname) 
       call h5dget_type_f(dset_id, type_id, error)
       !call h5dread_f(dset_id, type_id, dataset, data_dims, error)
       call h5dread_f(dset_id, H5T_NATIVE_INTEGER, dataset, data_dims, error)
       print *, 'read: ', trim(groupname), '/', trim(dsetname)
       call h5dclose_f(dset_id, error)
  endif
  call h5gclose_f(group_id, error)
  call h5fclose_f(file_id, error)
  IF( error == -1) THEN
    PRINT *, 'FAILED TO Close: ', inputFileName
    STOP
  ELSE
  END IF
  call h5close_f(error)
  deallocate(data_dims)
  end subroutine hdf5rd_i


  subroutine hdf5rd_f(inputFileName,groupname,subgroupname,dsetname,dim1,dim2,dim3,dataset)
!
! read hdf5 real type dataset

! input:  inputFileName,groupname,dsetname,dim1,dim2,dim3
! output: dataset

  CHARACTER (LEN = 80),intent(in)      :: inputFileName, groupname, subgroupname, dsetname
  integer, intent(in)                  :: dim1,dim2,dim3
  real, intent(out)                    :: dataset(dim1,dim2,dim3)
  INTEGER(HSIZE_T),DIMENSION(:), allocatable :: data_dims


  if (dim1 > 1 .and. dim2 > 1 .and. dim3 > 1) then
     allocate( data_dims(3) )
     data_dims(1)=dim1
     data_dims(2)=dim2
     data_dims(3)=dim3
  else if (dim1 > 1 .and. dim2 > 1 .and. dim3 ==1) then
     allocate( data_dims(2) )
     data_dims(1)=dim1
     data_dims(2)=dim2
  else if (dim1 > 1 .and. dim2 ==1 .and. dim3 ==1) then
     allocate( data_dims(1) )
     data_dims(1)=dim1
  else 
     write(6,*) 'dim1, dim2, dim3 must >=1. '
     write(6,*) 'stop at hdf5rd_f'
     return 
  endif

  ! Initialize FORTRAN interface.
  CALL h5open_f(error)
  ! Open file to read VD (table) information
  call h5fopen_f(trim(inputFileName), H5F_ACC_RDONLY_F, file_id, error)
  if(error /=0) call error_message(1, inputFileName)
  call h5gopen_f(file_id, trim(groupname), group_id, error)
  if(error /=0) call error_message(2, groupname)
  if( len(trim(subgroupname)) > 0 ) then
     call h5gopen_f(group_id, trim(subgroupname), subgroup_id, error)
     if(error /=0) call error_message(3, subgroupname)
       call h5dopen_f(subgroup_id, trim(dsetname), dset_id, error)
       if(error /=0) call error_message(4, dsetname) 
       call h5dget_type_f(dset_id, type_id, error)
       !call h5dread_f(dset_id, type_id, dataset, data_dims, error)
       call h5dread_f(dset_id, H5T_NATIVE_REAL, dataset, data_dims, error)
       print *, 'read: ', trim(groupname),'/',trim(subgroupname), '/', trim(dsetname)
       call h5dclose_f(dset_id, error)
     call h5gclose_f(subgroup_id, error)
  else
       call h5dopen_f(group_id, trim(dsetname), dset_id, error)
       if(error /=0) call error_message(4, dsetname) 
       call h5dget_type_f(dset_id, type_id, error)
       !call h5dread_f(dset_id, type_id, dataset, data_dims, error)
       call h5dread_f(dset_id, H5T_NATIVE_REAL, dataset, data_dims, error)
       print *, 'read: ', trim(groupname),'/', trim(dsetname)
       call h5dclose_f(dset_id, error)
  endif
  call h5gclose_f(group_id, error)
  call h5fclose_f(file_id, error)
  IF( error == -1) THEN
    PRINT *, 'FAILED TO Close: ', inputFileName
    STOP
  END IF
  call h5close_f(error)
  deallocate(data_dims)
  end subroutine hdf5rd_f

  subroutine error_message(errnumber, varname)
  CHARACTER (LEN = 80),intent(in)     :: varname
  integer, intent(in)                 :: errnumber
  if (errnumber ==1) then
     print *, 'file is not found: ', trim(varname) 
  else if (errnumber ==2) then   
     print *, 'group is not found: ', trim(varname) 
  else if (errnumber ==3) then   
     print *, 'subgroup is not found: ', trim(varname) 
  else if (errnumber ==4) then   
     print *, 'dataset is not found: ', trim(varname) 
  endif
  stop
  end subroutine error_message
end module hdf5rd_mod
