

program gen_be_stage0

!----------------------------------------------------------------------
! Authors: Dale Barker,    NCAR/MMM/DAG 
!          Yann Michel,    NCAR/MMM/DAG & CNRM-GAME/GMAP
!          Syed RH Rizvi,  NCAR/ESSL/MMM/DAG  08/06/2009
!          Gael Descombes  NCAR/ESSL/MMM/DAG  07/01/2012, 
!                          new features of gen_be new modules variable_types,
!                          configure, io_input_model, io_output 
!----------------------------------------------------------------------
! Purpose: Converts WRF-ARW standard output files (wrfout) to  the
!          desired format required by Stage1 of BE statistics for WRF/GSI
!
!  Input  : WRF forecasts for a specified date (NETCDF format).
!
!  Output : Binary files for use in Stage1 for WRF/GSI
!
!  Please acknowledge author/institute in work that uses this code.
!
!----------------------------------------------------------------------

   use configure
   use variable_types
   use io_input_model
   use io_output 
   use io_input 
   
   implicit none

   integer :: ee
   character (len=1024) :: input_file, input_fil2, fieldName, output_file 

   type (domain_type), pointer :: domain, domain_bc
   type (state_type), pointer :: state_mean, state_mean_bc
   integer :: indice
   type (field3DReal), pointer :: cld_fra, cld_fra_mean, cld_fra_bc, cld_fra_mean_bc
   real :: ds
   type (fieldnDReal), pointer :: mask

   real , allocatable, dimension(:,:) :: tmp
   character*10         :: date 

   logical :: holm_opt

   type (io_output_object) :: output_obj

   integer :: i0, j0, k0

   type (bins_type), pointer :: bins
   logical :: file_here, fil2_here, do_it  
   integer :: counter = 0
   real(kind=8) :: factor

   file_here = .false.
   fil2_here = .false.
   do_it     = .false.
   holm_opt  = .false.

   i0 = 10
   j0 = 10
   k0 = 10

   !================================================================================
   ! set up the global framework
   ! - the domain perturbation
   ! - the domain mean
   ! - several options if activated in the namelist 
   !================================================================================
   write(6,'(/a)')' [1] Setup up ancillary fields using 1st member values.'

  !---------------------------------------------------
  ! allocate and initialize Domain
  !---------------------------------------------------
   call framework_init_stage0(domain)
   call initialize_state(state_mean, domain%state ,domain%mesh%Dim1, domain%mesh%Dim2, domain%mesh%Dim3)
   date = start_date(1:10)

  !----------------------------------------------------  
  ! allocate subset domain
  !----------------------------------------------------
  Dim1 = domain%mesh%Dim1 -cut(1) -cut(2) 
  Dim2 = domain%mesh%Dim2 -cut(3) -cut(4) 
  Dim3 = domain%mesh%Dim3 -cut(5) -cut(6) 
  call allocate_domain(domain_bc, Dim1, Dim2, Dim3, nvar, cv_list, vardim_list)
  call subset_mesh_bc(domain%mesh, domain_bc%mesh, cut, model)
  call allocate_state(state_mean_bc, Dim1, Dim2, Dim3, nvar, cv_list, vardim_list)
  call initial_state(state_mean_bc, value0)
    
  !----------------------------------------------------  
  ! initialize holm_pt 
  !----------------------------------------------------
  if ( holm_opt ) then
      write(*,*)'holm option used'
      fieldName = 'cld_fra'
      call allocate_field3d(cld_fra, fieldName, domain%mesh%Dim1, domain%mesh%Dim2, domain%mesh%Dim3)
      call allocate_field3d(cld_fra_bc, fieldName, Dim1, Dim2, Dim3)
      fieldName = 'cld_fra_mean'
      call allocate_field3d(cld_fra_mean, fieldName, domain%mesh%Dim1, domain%mesh%Dim2, domain%mesh%Dim3)
      call allocate_field3d(cld_fra_mean_bc, fieldName, Dim1, Dim2, Dim3)
   end if

  !-------------------------------------------------------
  ! create bins
  !-------------------------------------------------------
  call read_namelist(nVar2d,nVar3d)
  call create_bins(bins, bin_type, lat_min, lat_max, binwidth_lat, &
                     hgt_min, hgt_max, binwidth_hgt, domain%mesh)
  output_file= 'bins.nc'
  call write_output_bin(output_file, bins, domain%mesh)

  !================================================================================ 
  ! convert all the meteorlogical fields in state vector
  ! compute cld_fra if necessary
  ! compute mean state if necessary
  ! write each member state vector in an output file
  !================================================================================ 
   if ( be_method == "NMC" ) then
      ne = 2
   end if
   write(6,'(/a)')' [2] Convert forecast fields data to standard fields and output'
   write(*,*)'n members : ',ne
   counter = 0

   do ee = 1, ne

      write(UNIT=ce,FMT='(i3.3)')ee 
      input_file = date(1:10)//'.e'//ce
      inquire(file=trim(input_file),exist=file_here)
      if ( file_here ) then
 
      counter = counter + 1
      write(*,*)'member : ',trim(input_file)
      
      call convert_uv2state(domain,input_file,fft_method)
      call convert_qcond2state(domain,input_file,rsmall)
      call convert_qt2state(domain,input_file)
      !call convert_chem2state(domain,input_file)
      call convert_psfc2state(domain,input_file)

      if ( holm_opt ) then
         write(*,*)'compute_cldfra'
         call compute_cldfra(cld_fra, domain, input_file)
         call subset_field_bc(cld_fra, cld_fra_bc, domain%mesh, cut)
      end if

      write(*,*)'compute mean state'
      call compute_mean_state(state_mean, domain%state, counter)
      call subset_state_bc( state_mean, state_mean_bc, domain%mesh, cut)
      call subset_state_bc( domain%state, domain_bc%state, domain%mesh, cut) 
      call check_state(domain_bc%state,i0,j0,k0)

      output_file = 'temp.e'//ce
      write(*,*)'temporary file ',output_file
      call write_output_state(output_file, domain_bc%state, domain_bc%mesh)

      end if !! if file is present
   end do !! end of loop on member

   call io_output_mesh(domain%mesh,'mesh_grid_fulldomain.nc')
   call io_output_mesh(domain_bc%mesh)
   call deallocate_domain(domain)
   call deallocate_state(state_mean)   

  !================================================================================
  ! - Load each file made in the precedent step and substract the mean
  ! - 3 options available : NMC, ENS, ENS + mean
  !================================================================================
   write(6,'(a)')' [4] Compute perturbations and output'

   !input_file = 'temp.'//date(1:10)//'.e'//ce

   if ( be_method == "NMC" ) then

       write(6,'(/a)')'    Compute perturbation as a difference between two forecasts'
       ! Re-read input forecast standard fields (ne=2 hard-wired above for NMC-method):
       input_file = 'temp.e001'
       input_fil2 = 'temp.e002'
       inquire(file=trim(input_file),exist=file_here)
       inquire(file=trim(input_fil2),exist=fil2_here)

       if ( (file_here) .and. (fil2_here) ) then
          call read_input_state(input_file, domain_bc%state)
          call read_input_state(input_fil2, state_mean_bc)
          write(*,*)'---------------------------------'
          write(*,*)'check state : '
          call check_state(domain_bc%state,i0,j0,k0)
          write(*,*)'check state mean : '
          call check_state(state_mean_bc,i0,j0,k0)
       
          ! Write out standard perturbations for this member
          call substract_state(domain_bc%state, state_mean_bc)
!          factor = 1/sqrt(2.)
          factor = 1.
          call mult_state(domain_bc%state, factor)
          write(*,*)'check state diff : '
          call check_state(domain_bc%state,i0,j0,k0)
          output_file = 'pert.'//date(1:10)//'.e001'
          call write_output_state(output_file, domain_bc%state, domain_bc%mesh)
          !Write out ensemble Holm/Full reference fields:

          ! Dynamical mask computed for each members
          ee = 1
          write(UNIT=ce,FMT='(i3.3)')ee
          call compute_dynamic_mask(bins, domain_bc%mesh, domain_bc%state, state_mean_bc, ce, date)
       end if

   else
       write(6,'(/a)')'    Compute perturbation be_method = "ENS'
       
       do ee = 1, ne
           ! difference between 2 members
           if (.not. use_mean_ens) then !! we can add .or.( holm_opt )
              if ( ee .eq. 1 ) then
                 write(UNIT=ce,FMT='(i3.3)')ne
              end if
              input_file = 'temp.e'//ce
              inquire(file=trim(input_file),exist=file_here)
              if (file_here) then
                 call read_input_state(input_file, state_mean_bc)
                 write(*,*)'check state mean opt not use_mean_ens : '
                 call check_state(state_mean_bc,i0,j0,k0)
                 write(UNIT=ce,FMT='(i3.3)')ee
                 call compute_dynamic_mask(bins, domain_bc%mesh, domain_bc%state, state_mean_bc, ce, date)
              end if     
           end if   

       write(UNIT=ce,FMT='(i3.3)')ee
       write(*,*)' ce : ',ce
       ! Re-read ensemble member standard fields
       input_file = 'temp.e'//ce
       inquire(file=trim(input_file),exist=fil2_here)
       do_it=.false.
       if ( (file_here) .and. (fil2_here) ) then
          do_it = .true.
       else if ( fil2_here .and. use_mean_ens ) then
          do_it = .true.
       end if
       
       if ( do_it ) then

          write(*,*)'==============================================='
          write(*,*)'input_file ',trim(input_file)       
          call read_input_state(input_file, domain_bc%state)
          write(*,*)'---------------------------------'
          write(*,*)'check state : '
          call check_state(domain_bc%state,i0,j0,k0)
          write(*,*)'check state mean : '
          call check_state(state_mean_bc,i0,j0,k0)
          ! Write out standard perturbations for this member:
          output_file = 'pert.'//date(1:10)//'.e'//ce
          call substract_state(domain_bc%state, state_mean_bc)
          write(*,*)'check state pert : '
          call check_state(domain_bc%state,i0,j0,k0)
          write(*,*)'---------------------------------'
          call write_output_state(output_file, domain_bc%state, domain_bc%mesh)                  
          ! Write out ensemble Holm/Full reference fields
          write(*,*)'==============================================='

       end if
       
       end do !! loop member

       ! Write the mean file
       if ( use_mean_ens) then
          output_file = 'state_mean_'//date(1:10)//'.nc'
          write(*,*)'output_file : ',output_file
          call write_output_state(output_file, state_mean_bc, domain_bc%mesh)
          call check_state(state_mean_bc,i0,j0,k0)       
       end if

    end if !! condition ensemble        
   
  !================================================================================
  ! deallocate state vector
  ! deallocate cld_fra if necessary
  !================================================================================

   call deallocate_domain(domain_bc)
   call deallocate_state(state_mean_bc)
   call deallocate_bins(bins)

   if ( holm_opt ) then
       call deallocate_field(cld_fra)
       call deallocate_field(cld_fra_bc)
       call deallocate_field(cld_fra_mean)
       call deallocate_field(cld_fra_mean_bc)
   end if
   !call deallocate_fieldnd(mask)
  
   input_file='standard_variables.txt'
   call write_ascii_file(input_file, cv_list, nvar)
   input_file='control_variables.txt'
   call write_ascii_file(input_file, cv_listu, nvar)

   write(*,*)'End of gen_be_stage0'


end program gen_be_stage0
