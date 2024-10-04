

program gen_be_stage1 

!---------------------------------------------------------------------------------------------
! Authors: Dale Barker,    NCAR/MMM/DAG 
!          Yann Michel,    NCAR/MMM/DAG & CNRM-GAME/GMAP
!          Syed RH Rizvi,  NCAR/ESSL/MMM/DAG  08/06/2009
!          Gael Descombes  NCAR/ESSL/MMM/DAG  07/01/2012, 
!                          new features of gen_be, new modules variable_types, configure,
!                          io_input_model, io_output 
!---------------------------------------------------------------------------------------------
! Purpose : To remove the binned mean from the perturbation fields.
!
! Input   : binary files: "pert.ccyymmddhh.e"ce for ENS or
!                         "pert.ccyymmddhh.e001" for NMC.
!
! Output : binary files for use of the gen_be_stage2
!
!---------------------------------------------------------------------------------------------

   use da_control, only : stderr, stdout, filename_len, vertical_ip
   use da_tools_serial, only : da_get_unit,da_advance_cymdh
   use da_gen_be, only : write_2d_field, &
   	read_3d_field, write_3d_field, update_rain_bin

   use configure
   use variable_types
   use io_input_model
   use io_output
   use io_output_application
   use io_input
 
   
   implicit none

   character*10         :: date, new_date             ! Current date (ccyymmddhh).
   character*10         :: variable                   ! Variable name.
   character (len=1024) :: filename

   integer             :: counter_bin                ! counter for bins
   integer             :: member                     ! Loop counterer
   integer             :: sdate, cdate, edate        ! Starting, current ending dates.
   integer             :: num_bins                   ! Number of bins (3D fields).
   integer             :: num_bins2d                 ! Number of bins (2D fields).  
!   integer             :: N_holm_bins                ! Number of bins for Holm RH   
   integer :: counter
   real                :: counter_inv                  ! 1 / counter.

   real :: coeffa, coeffb 
 
   integer             :: b, b3d

   type (domain_type), pointer :: domain
   type (bins_type), pointer :: bins
   type (state_type), pointer :: state_mean
   real :: var

   ! -------------------------------------------
   integer :: nz = 10
   type (field3dReal), pointer :: field3d 
   integer :: inv_dim2d, l, kb, m, k, j, i
   !real, allocatable    :: bigxkm(:,:),vng(:,:),p8(:)
   !real, allocatable    :: rlen(:),p8(:)
   !real, allocatable    :: tlflt(:,:)

   type (field2dReal), pointer :: bigxkm, vng, vorr, tlflt
   type (field1dReal), pointer :: p8,rlen
   !real, allocatable    :: vorr(:,:)
   character(len=32) :: fieldName

   logical :: file_here

   !----------------------------------------------

   stderr = 0
   stdout = 6
   
!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [1] Read Namelist'
!---------------------------------------------------------------------------------------------

   call read_namelist(nVar2d,nVar3d)
   
   filename='mesh_grid.nc'
   call get_vardim(filename, Dim1, Dim2, Dim3)

   call allocate_domain(domain, Dim1, Dim2, Dim3, nvar, cv_list, vardim_list)
   domain % state % date = start_date(1:10)

   call init_mesh(filename, domain%mesh)

   call create_bins(bins, bin_type, lat_min, lat_max, binwidth_lat, &
                     hgt_min, hgt_max, binwidth_hgt, domain%mesh)
   bins%bin2d_pts%array = 0 
   bins%bin_pts%array = 0 
   write(*,*) 'num_bins2d, num_bins',bins%num_bins2d, bins%num_bins
   write(*,*)'bins%bin2d_pts%array ',bins%bin2d_pts%array
   ! to initialize flag dyn_mask
   member = 1
   write(UNIT=ce,FMT='(i3.3)')member
   call update_dynamic_mask(bins, domain%mesh, start_date, ce, dyn_mask)

   if ( dyn_mask ) then
      vardim_list2 = vardim_list - 1 
      call allocate_state_bin(state_mean, bins%num_bins2d, bins%num_bins2d, Dim3, nvar, cv_list, vardim_list2)
      state_mean%name = 'bins2d'
   else
      call allocate_state(state_mean, Dim1, Dim2, Dim3, nvar, cv_list, vardim_list)
   end if
   call initial_state(state_mean, value0)


!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [2] Read fields from standard files, and calculate mean fields'
!---------------------------------------------------------------------------------------------
   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
   write(6,'(4a)')' Computing statistics for dates ', start_date, ' to ', end_date
   write(6,'(a,i8,a)')' Interval between dates = ', interval, 'hours.'
   write(6,'(a,i8)')' Number of ensemble members at each time = ', ne
   write(6,'(2a)') ' Data dir is ',trim(dat_dir)
   date = start_date
   cdate = sdate
   counter = 0
 
   do while ( cdate <= edate )

      do member = 1, ne 

         write(UNIT=ce,FMT='(i3.3)')member
         filename = trim(dat_dir)//'/pert.'//date(1:10)//'.e'//trim(ce)
         file_here = .false.
         inquire(file=trim(filename),exist=file_here)
         if (file_here) then
        
         counter = counter + 1

         filename_list = trim(dat_dir)//'/pert.'//date(1:10)//'.e'//trim(ce)
         !call read_state_from_filist(domain%state, filename_list, 0)
         call read_input_state(filename, domain%state)
         write(*,*)'check state after reading, member ',trim(ce)
         call check_state(domain%state,10,10,10)

         call update_dynamic_mask(bins, domain%mesh, date, ce, dyn_mask)
 
         write(*,*)'------- state_mean -------' 
         if ( dyn_mask ) then
            ! (binning)
            do jj=1, Dim2
               do ii=1, Dim1
                  b = bins%bin2d%array(ii,jj)
                  bins%bin2d_pts%array(b) = bins%bin2d_pts%array(b) + 1
                  counter_bin = bins%bin2d_pts%array(b)
                  coeffa = 1/real(counter_bin)
                  coeffb = real(counter_bin-1)*coeffa                
                  do vv=1, nvar
                     if ( domain%state%num(vv)%IDdim == 3 ) then
                        do kk=1, Dim3
                           b3d = bins%bin%array(ii,jj,kk)
                           bins%bin_pts%array(b3d) = bins%bin_pts%array(b3d) + 1
                           state_mean%num(vv)%field%field2d%array(kk,b) = &
                           coeffb * state_mean%num(vv)%field%field2d%array(kk,b) &
                          + domain%state%num(vv)%field%field3d%array(ii,jj,kk) * coeffa
                        end do
                     else if ( domain%state%num(vv)%IDdim == 2 ) then
                        state_mean%num(vv)%field%field1d%array(b) = &
                       coeffb * state_mean%num(vv)%field%field1d%array(b) &
                        + domain%state%num(vv)%field%field2d%array(ii,jj)  * coeffa
                     end if
                  end do
               end do
           end do
         else          
            ! (no binning)
            call  compute_mean_state(state_mean, domain%state, counter)
            call check_state(state_mean,10,10,10)
         end if

      end if ! if member perturbation is present
	    
      end do  ! End loop over ensemble members.

      ! Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate

   
   end do     ! End loop over times.

   !-------------------------------------------------------------
   ! write bin file and state mean 
   !-------------------------------------------------------------
   filename = 'bins.nc'
   call check_state(state_mean,1,10,10)
   call write_output_bin(filename, bins, domain%mesh)

   filename = 'bin.data'
   call write_bins_binary(filename, bins)
      
   filename = 'state_mean.nc'
   call write_output_state(filename, state_mean, domain%mesh)

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [3] Read fields again, and remove mean'
!---------------------------------------------------------------------------------------------

   date = start_date
   cdate = sdate
   counter = 0

   do while ( cdate <= edate )
      do member = 1, ne

         write(6,'(3a,i3)')'    Removing Time mean for date ', date,' member ',member
	 write(ce,'(i3.3)')member
         write(UNIT=ce,FMT='(i3.3)')member
         filename = trim(dat_dir)//'/pert.'//date(1:10)//'.e'//trim(ce)
         file_here = .false.
         inquire(file=trim(filename),exist=file_here)
         if (file_here) then
	 
         counter = counter + 1

         call read_input_state(filename, domain%state)	 
         write(*,*)'---- load state ------------'
         call check_state(domain%state,10,10,10)	

         write(*,*)'---- remove mean --------'
         if ((use_mean_ens).or.be_method == "NMC") then
  
           if ( dyn_mask ) then
           ! (binning)
            call update_dynamic_mask(bins, domain%mesh, date, ce, dyn_mask)
            do ii=1, Dim1
               do jj=1, Dim2
                  do vv=1, nvar
                  b = bins%bin2d%array(ii,jj)
                  if ( domain%state%num(vv)%IDdim == 3 ) then
                    do kk=1, Dim3
                       domain%state%num(vv)%field%field3d%array(ii,jj,kk) = &
                        domain%state%num(vv)%field%field3d%array(ii,jj,kk)  &
                         - state_mean%num(vv)%field%field2d%array(kk,b)
                    end do
                  else if ( domain%state%num(vv)%IDdim == 2 ) then
                   domain%state%num(vv)%field%field2d%array(ii,jj) = &
                     domain%state%num(vv)%field%field2d%array(ii,jj) &
                     - state_mean%num(vv)%field%field1d%array(b)
                  end if
                  end do
               end do
            end do
           else
           ! (no binning)
            call substract_state(domain%state,state_mean)
           end if
         end if     
         call check_state(domain%state,1,10,10)
         call write_state_into_variable_file(domain%state, domain%mesh, date, ce)

         end if ! if member perturbation is present
 
      end do  ! End loop over ensemble members.

      ! Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times.

   call deallocate_bins(bins)

   call deallocate_domain(domain) 

   call deallocate_state(state_mean)


!----------------------------------------------------------------------------------------------------!   
write(6,'(a)')'    End of program gen_be_stage1'
!----------------------------------------------------------------------------------------------------!   
end program gen_be_stage1
