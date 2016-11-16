

program gen_be_diags
!------------------------------------------------------------------------
!  Purpose: Gathers background error statistics generated at various 
!           stages of "gen_be"
!
!  Auothor: Gael Descombes (MMM/NESL/NCAR)   Date: 01/07/2012
!           write be.nc, be.dat (WRFDA)
!
!  Note: Please acknowledge author/institute in work that uses this code.
!------------------------------------------------------------------------

   use configure
   use variable_types
   use io_input_model
   use io_output
   use io_input
   use io_output_application

   use da_control, only : stderr, stdout, filename_len
   use da_tools_serial, only : da_get_unit
   use da_gen_be, only : da_readwrite_be_stage2, da_readwrite_be_stage3, &
      da_readwrite_be_stage4

   implicit none

   character*10        :: variable                   ! Variable name
   integer             :: nk,nk_3d                   ! Dimensions read in.

   type (io_output_object) :: output_obj
   !character (len=filename_len ):: filename, fieldName
   character (len=32 ):: filename, fieldName

   type (bins_type), pointer :: bins
   type (mesh_type), pointer :: mesh
   type (state_matrix_type), pointer :: regcoeff

   type eigen_pointer
      type (eigen_type), pointer :: eigen
   end type eigen_pointer

   type state_eigen_type
   type (eigen_pointer), dimension(:), pointer :: num 
      integer :: nvar
   end type state_eigen_type
   type (state_eigen_type), pointer :: eigen_state

   type (state_type), pointer :: lenscale, ver_lenscale, variance, vert_autocovarce, varce

   type (field2DInteger), pointer :: covarID

   integer :: nferr, ivarnd
   integer :: ounit, iunit, namelist_unit, itime
   
   integer :: mlat, lenscale_bin
   real, allocatable, dimension(:) :: clat, lenscale1d
   real :: lat_max0, lat_min0 
   logical :: file_here = .false.
   integer :: io_opt 
   real(kind=8) :: limit_inf = 10.0e-30
   real(kind=8) :: limit_inf_varce = 10.0e-30 !    1e-11

   include 'netcdf.inc'

   stderr = 0
   stdout = 6
   ivarnd = 0
   itime  = 0

   !==================================================================================
   ! Read Data from splitted files
   !==================================================================================
  
   !---------------------------------------------- 
   ! Read mesh grid
   !----------------------------------------------
   filename='mesh_grid.nc'
   call read_namelist(nVar2d,nVar3d)   
   call get_vardim(filename, Dim1, Dim2, Dim3)
   nk = Dim3
   nk_3d = nk
   call init_mesh(filename, mesh, model)
   
   !---------------------------------------------- 
   ! Read binning
   !---------------------------------------------- 
   filename='bins.nc'
   call init_bins(filename, bins)

   !---------------------------------------------- 
   ! Read Regcoeff (stage2)
   !---------------------------------------------- 
   filename='regcoeff'
   write(*,*)'covar_ID ',covar_ID
   call allocate_state_matrix(regcoeff, bins, Dim3, nvar, covar_ID,cv_list,filename) 
   filename='gen_be_stage2_regcoeff.nc'
   if ( ncovar_tot.gt.0 ) then
      call read_matrix_state(filename, regcoeff)
   end if
   call check_matrix(regcoeff,1,1,1)

   !----------------------------------------------
   ! Read variance (from stage3)
   !----------------------------------------------
   vardim_list2 = vardim_list - 1
   do vv=1, nvar
      tmp_list(vv)  = 'vert_varce_'//trim(cv_listu(vv))
      filename_list(vv) = 'gen_be_stage3_vert_varce.'//trim(cv_listu(vv))//'.nc'
   end do
   call allocate_state_bin(variance, bins%num_bins, bins%num_bins2d, Dim3, nvar, tmp_list, vardim_list2)
   variance%name = 'bin2d'
   call read_state_from_filist(variance, filename_list, itime)

   vardim_list2 = vardim_list
   do vv=1, nvar
      if ( vardim_list(vv).eq.2 ) vardim_list2(vv)=1  
      tmp_list(vv)  = 'vert_autocovarce_'//trim(cv_listu(vv))
      filename_list(vv) = 'gen_be_stage3_vert_autocovarce.'//trim(cv_listu(vv))//'.nc'
      write(*,*)'tmp_list(vv) ',tmp_list(vv)
   end do
   call allocate_state_bin(vert_autocovarce, bins%num_bins, bins%num_bins2d, Dim3, nvar, tmp_list, vardim_list2)
   vert_autocovarce%name = 'bin2d'
   call read_state_from_filist(vert_autocovarce, filename_list, itime)


   if ( data_on_levels ) then

      !---------------------------------------------------- 
      ! allocation vertical length scale and read (stage3)
      !---------------------------------------------------- 
      vardim_list3 = vardim_list-1
      where ( vardim_list3(:) .eq. 1 ) vardim_list3(:) = -1 ! no vertical length scale for 2d variables
      do vv=1, nvar
         tmp_list(vv)  = 'vert_lenscale_'//trim(cv_listu(vv))
         filename_list(vv) = 'gen_be_stage3_vert_lenscale.'//trim(cv_listu(vv))//'.nc'
      end do
      call allocate_state_bin(ver_lenscale, bins%num_bins, bins%num_bins2d, Dim3, nvar, tmp_list, vardim_list3)
      ver_lenscale%name = 'bin2d'
      call read_state_from_filist(ver_lenscale, filename_list, itime)

   else

      !---------------------------------------------- 
      ! Read eigen variables (stage 3)
      !---------------------------------------------- 
      allocate(eigen_state)
      allocate(eigen_state%num(1:nvar))
      do vv=1, nvar
         filename='gen_be_stage3_eigen.'//trim(cv_listu(vv))//'.nc'
         write(*,*)'open ',trim(filename)
         call init_eigenvar(filename, eigen_state%num(vv)%eigen, use_global_eofs, cv_listu(vv), vardim_list(vv))
      end do

   end if

   ! allocate variance 3d (stage3)
   do vv=1, nvar
     tmp_list(vv) = 'varce_'//trim(cv_listu(vv))
     filename_list(vv) = 'gen_be_stage3_varce.'//trim(cv_listu(vv))//'.nc'
   end do
   call allocate_state(varce, Dim1, Dim2, Dim3, nvar, tmp_list, vardim_list)                  
   call read_state_from_filist(varce, filename_list, itime)

   !------------------------------------------------------------------ 
   ! allocation length scale (stage4) 
   ! dimensions are 1d for 3d variables and 0d(scalar) for 2d variable
   !------------------------------------------------------------------ 
   vardim_list2 = vardim_list - 1 
   do vv=1, nvar
      if (trim(uh_method) == 'power') then
         tmp_list(vv)  = 'power_spectra_'//trim(cv_listu(vv))
         filename_list(vv) = trim(filename_list(vv))//'.nc'
      else
         tmp_list(vv) = 'lenscale_'//trim(cv_listu(vv))
         filename_list(vv) = trim(tmp_list(vv))//'.nc' ! not used yet
      end if
   end do
   if (trim(uh_method) == 'power') then
      write(*,*) 'not supported yet'
      stop
   else
      call allocate_state_bin(lenscale, bins%num_bins2d, bins%num_bins2d, Dim3, nvar, tmp_list, vardim_list2)
      lenscale%name = 'bin2d'
   end if

   !==================================================================================   
   ! write binary file (WRFDA) 
   !==================================================================================   
   ounit = 101
   filename = 'be.dat' 
   open (ounit, file = filename, form='unformatted')


   !==================================================================================   
   ! Create Netcdf output
   !==================================================================================   
   filename = 'be.nc'
   output_obj % filename = trim(filename)
   output_obj % ivarnd = 0
   write(*,*)'output_obj % ivarnd ',output_obj % ivarnd
   nferr = nf_create(trim(output_obj % filename), ior(NF_CLOBBER,NF_64BIT_OFFSET), output_obj%wr_ncid)

   write(*,*)'1 output_obj % ivarnd ',output_obj % ivarnd, output_obj%wr_ncid
   write(*,*)'nvar ',nvar
   !----------------------------------------------------------------------------------
   ! Definition of the dimensions 
   !----------------------------------------------------------------------------------
   nferr = nf_def_dim(output_obj%wr_ncid, 'lon', Dim1, output_obj%wrDimIDlon)
   nferr = nf_def_dim(output_obj%wr_ncid, 'lat', Dim2, output_obj%wrDimIDlat)
   nferr = nf_def_dim(output_obj%wr_ncid, 'lev', Dim3, output_obj%wrDimIDlev)
   nferr = nf_def_dim(output_obj%wr_ncid, 'num_bins', bins%num_bins, output_obj%wrDimIDbin)
   nferr = nf_def_dim(output_obj%wr_ncid, 'num_bins2d', bins%num_bins2d, output_obj%wrDimIDbin2d)
   nferr = nf_def_dim(output_obj%wr_ncid, 'nvar', nvar, output_obj%wrDimIDnvar)
   nferr = nf_def_dim(output_obj%wr_ncid, 'maxvar', nMaxvar, output_obj%wrDimIDmaxvar)
   nferr = nf_def_dim(output_obj%wr_ncid, 'ne', ne, output_obj%wrDimIDne)
   nferr = nf_def_dim(output_obj%wr_ncid, 'DateStrLen', DateStrLen, output_obj%wrDimIDate)
   nferr = nf_def_dim(output_obj%wr_ncid, 'StrLen', StrLen, output_obj%wrDimIDStr)
   nferr = nf_def_dim(output_obj%wr_ncid, 'nregcoeff', ncovar_tot, output_obj%wrDimIDncovar)
   if (trim(uh_method) == 'power') then
      nferr = nf_def_dim(output_obj%wr_ncid, 'wave', output_obj % wrDimIDwav)
   end if
   nferr = nf_def_dim(output_obj%wr_ncid, 'lon_u', mesh%Dim1u, output_obj%wrDimIDlonu)
   nferr = nf_def_dim(output_obj%wr_ncid, 'lat_u', mesh%Dim2u, output_obj%wrDimIDlatu)
   nferr = nf_def_dim(output_obj%wr_ncid, 'lon_v', mesh%Dim1v, output_obj%wrDimIDlonv)
   nferr = nf_def_dim(output_obj%wr_ncid, 'lat_v', mesh%Dim2v, output_obj%wrDimIDlatv)
   
   nferr = nf_def_dim(output_obj%wr_ncid, 'mlat', mlat, output_obj%wrDimIDmlat)

   write(*,*)''

   write(*,*)'2 output_obj % ivarnd ',output_obj % ivarnd

   !----------------------------------------------------------------------------------
   ! Definition of the variable to write
   !----------------------------------------------------------------------------------
   write(*,*)'Definition mesh '
   call io_output_mesh_def(mesh,output_obj)
   call io_output_bins_def(bins,output_obj)

   write(*,*)'Definition regression coefficient '
   ! define regression coefficient
   call io_output_matrix_def(regcoeff, output_obj)
   write(*,*)'After Definition regression coefficient '

   ! define variance
   call io_output_state_def(variance, itime, output_obj)
   
   if ( data_on_levels ) then
      ! define vertical lenscale
      call io_output_state_def(ver_lenscale, itime, output_obj)
   else
      ! define eigen vector and eigen value
      do vv=1, nvar
         call io_output_eigen_def(eigen_state%num(vv)%eigen, output_obj)
      end do
   end if
   ! define vertical autocovariance
   call io_output_state_def(vert_autocovarce, itime, output_obj)
   ! define variance 
   call io_output_state_def(varce, itime, output_obj)

   ! define lenscale
   call io_output_state_def(lenscale, itime, output_obj)

   ! define the namelist
   call io_output_namelist_def(output_obj)

   ! end of definition
   nferr = nf_enddef(output_obj % wr_ncid)
   nferr = nf_sync(output_obj % wr_ncid)

   !----------------------------------------------------------------------------------
   !  write the variables
   !----------------------------------------------------------------------------------

   write(*,*) 'write variable'
   call io_output_mesh_var(mesh,output_obj)
   call io_output_bins_var(bins,output_obj)

   !------------------------------------------------------------------
   ! write stage 2 / regression coeff / variance
   !------------------------------------------------------------------
   ! netcdf format
   call io_output_matrix_var(regcoeff,output_obj)
   ! binary format wrfda

   call da_readwrite_be_stage2_binary(ounit, regcoeff, bins, mesh)
   ! write variance
   call filter_state_min(variance, Dim3, bins%num_bins2d)
   !call filter_state(variance, limit_inf)
   call io_output_state_var(variance, itime, output_obj)

   !-----------------------------------------------------------------
   ! write stage 3 / eigen vector - values / vert length scale
   !-----------------------------------------------------------------
   write(*,*)'stage 3'

   if ( data_on_levels ) then
      ! write vertical lenscale
      call filter_state(ver_lenscale, limit_inf) 
      call io_output_state_var(ver_lenscale, itime, output_obj)
 
   else
      
      ! write eigen vector and value
      do vv=1, nvar
         ! netcdf format
         call io_output_eigen_var(eigen_state%num(vv)%eigen, output_obj)
         ! binary format
         if ( vardim_list(vv) == 3 ) then
            call da_readwrite_be_stage3(ounit, Dim3, cv_listu(vv))
         else
            if (uh_method /= 'power') then ! ps_u ? GD
               call da_readwrite_be_stage3(ounit,  1, cv_listu(vv))
            else
               write(*,*)'check 2d power variable'
              stop 
            end if
         end if
      end do

   end if

   call filter_state(vert_autocovarce, limit_inf)
   call filter_state(vert_autocovarce, limit_inf_varce)
   call io_output_state_var(vert_autocovarce, itime, output_obj)
   call filter_state(varce, limit_inf_varce)
   !call filter_hydro(varce, vert_autocovarce, Dim3, bins%num_bins2d)
   call io_output_state_var(varce, itime, output_obj)

   !------------------------------------------------------------------
   ! write stage 4 /  lenscale
   !------------------------------------------------------------------
   write(*,*)'stage 4'

   do vv=1, nvar
      do ii=1, bins%num_bins2d
         write(UNIT=ce,FMT='(i3.3)')ii
         ! it depends if global binning or not
         write(*,*)'length scale : ',trim(cv_listu(vv))  
         filename_list(vv) = trim(cv_listu(vv))//'/sl_print.b'//ce//'.'//trim(cv_listu(vv))
         file_here = .false.
         inquire(file=trim(filename_list(vv)),exist=file_here)
         if (.not.file_here) then
            filename_list(vv) = trim(cv_listu(vv))//'/sl_print.b001.'//trim(cv_listu(vv)) 
         end if
         ! binary format read write
         if ( vardim_list(vv) == 3 ) then
            allocate(lenscale1d(1:Dim3))
            write(*,*)'n_smth_sl  ',n_smth_sl
            call da_readwrite_be_stage4_field( filename_list(vv), ounit, Dim3, uh_method, n_smth_sl, cv_listu(vv), lenscale1d )
            write(*,*)'after read stage 4 ',lenscale1d
            lenscale%num(vv)%field%field2d%array(:,ii) = lenscale1d(:) * mesh%ds%scalar
            deallocate(lenscale1d)
         else if ( vardim_list(vv) == 2 ) then
            allocate(lenscale1d(1:bins%num_bins2d))
            call da_readwrite_be_stage4_field( filename_list(vv),ounit, 1, uh_method, n_smth_sl, cv_listu(vv), lenscale1d )
            lenscale%num(vv)%field%field1d%array(ii) = lenscale1d(1) * mesh%ds%scalar
            write(*,*)'GD read stage4 var2d ',ii,lenscale1d(1)
            deallocate(lenscale1d)
         end if
      end do
   end do
   call filter_state_min(lenscale, Dim3, bins%num_bins2d) 
   call io_output_state_var(lenscale, itime, output_obj)

   !-------------------------------------------
   ! write the namelist 
   !-------------------------------------------
   call io_output_namelist_var(uh_method, nvar, ncovar_tot, cv_list, cv_listu, vardim_list, regcoeff, covar_ID, output_obj)


   !==================================================================================   
   ! close netcdf file 
   !==================================================================================
   call io_output_finalize(output_obj)

   !==================================================================================   
   ! close binary file 
   !================================================================================== 
   close(ounit)
stop
   !==================================================================================
   ! deallocate section
   !==================================================================================
   !call deallocate_field(covar_ID)
   call deallocate_state(lenscale)
   call deallocate_state(variance)
   call deallocate_mesh(mesh)
   call deallocate_bins(bins)

   if ( .not. data_on_levels ) then
      do vv=1, nvar
         call deallocate_eigenvar(eigen_state%num(vv)%eigen)      
      end do
      deallocate(eigen_state%num)
      deallocate(eigen_state)
   else
      call deallocate_state(ver_lenscale)
   end if

   call deallocate_state(vert_autocovarce)
   call deallocate_state(varce)


end program gen_be_diags
