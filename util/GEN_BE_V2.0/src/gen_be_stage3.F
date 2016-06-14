program gen_be_stage3

! ---------------------------------------------------------------------------------
! Purpose : Diagnose Vertical Background Error Covariance Matrix                  !
!           Project fields on EOF modes (eigenvectors of BV)                      !
! ---------------------------------------------------------------------------------
!
! History:
!
! Date     Author & Comment
! -------- ----------------
! dd/mm/yy Dale Barker
!          Initial version
! 01/07/12 Gael Descombes  NCAR/ESSL/MMM/DAG (new features of gen_be)  
!          add the methods to compute vertical lengthscale in the physicals space,
!          and the vertical variance
!----------------------------------------------------------------------
  
#ifdef crayx1
#define iargc ipxfargc
#endif

   use variable_types
   use io_output
   use io_input
   use io_input_model
   use io_output_application
    
   use da_control, only : stderr, stdout, filename_len, vertical_ip
   use da_gen_be, only : da_eof_decomposition_test, da_eof_decomposition, &
      da_transform_vptovv, da_create_bins, update_rain_bin, da_transform_vptovv_bin7,&
      read_2d_field, read_3d_field, write_2d_field, write_3d_field
   use da_tools_serial, only : da_get_unit, da_advance_cymdh
   use da_horizontal_covariances

   implicit none

   character*10        :: date, new_date             ! Current date (ccyymmddhh).
   character*32        :: variable, variabl2, variabl3                   ! Variable name
   character(len=filename_len)        :: filename                   ! Input filename.
   character*3         :: ck                         ! Level index -> character.
   
   integer             :: Dim3dum                    ! Dimensions read in.
   integer             :: sdate, cdate, edate        ! Starting, current ending dates.
   integer             :: i, j, k, k1, k2, b, member ! Loop counters.
     
   real                :: inv_nij                    ! 1 / (Dim1*Dim2).
   real                :: mean_field                 ! Mean field.
   real                :: coeffa, coeffb             ! Accumulating mean coefficients.
   integer             :: counter

   real(kind=8), allocatable   :: vertical_wgt(:,:,:)        ! Inner product for vertical transform.
   real, allocatable   :: bv(:,:,:),bv2(:,:),bv4d(:,:,:,:)         ! Vertical BE for this time.
   real, allocatable   :: var_bin_field(:,:,:)       ! Vertical BE for this time.
   real, allocatable   :: work(:,:)                  ! EOF work array.
   real(kind=8), allocatable   :: e_vec_loc(:,:,:)           ! Latitudinally varying eigenvectors.
   real(kind=8), allocatable   :: e_val_loc(:,:)             ! Latitudinally varying eigenvalues.
   real(kind=8), allocatable   :: e_vec(:,:)                 ! Domain-averaged eigenvectors.
   real(kind=8), allocatable   :: e_val(:)                   ! Domain-averaged eigenvalues.
   real(kind=8), allocatable   :: evec(:,:,:)                ! Gridpoint eigenvectors.
   real(kind=8), allocatable   :: eval(:,:)                  ! Gridpoint sqrt(eigenvalues).
   real(kind=8), allocatable   :: evecbin(:,:,:)             ! Binned eigenvectors.
   real(kind=8), allocatable   :: evalbin(:,:)               ! Binned sqrt(eigenvalues)

   real :: rnorm 
   real , parameter    :: rsmall0 = 1.0e-30 
   real , parameter    :: rsmall1  = 1.0e-12 ! filter of vertical autocovariance of hydrometeors (GD)                    

   integer :: ounit,iunit,namelist_unit
   type (bins_type), pointer :: bins
   type (field3DReal), pointer :: field, field_out, field3d ,vert_varce3d
   type (field2DReal), pointer :: field2d
   type (field1DReal), pointer ::  vert_varce1d 

   integer :: vardim, numarg
#ifdef IARGC
   integer, intrinsic    :: iargc
#else
   integer, external     :: iargc
#endif

   integer :: IvarDim
   type (eigen_type), pointer :: eigen

   !! variance
   integer              :: bin_pts_glob, bin_pts_filt
   real                 :: coeffa_glob, coeffb_glob, coeffa3d, coeffb3d
   real, allocatable    :: varce_pts(:,:,:), varce_pts_filt(:,:,:)

   !! vertical length scale
   integer :: l, kb, kp, km, m, num_bins2d, b2
   real, allocatable    :: dsig(:), vorr(:,:,:), tmp(:)
   type (field2DReal), pointer :: vl
   character(len=filename_len)       :: fieldName
   real :: var0
   integer :: ni, nj, nk, nsig, num_bins
   type (mesh_type), pointer :: mesh
   integer, allocatable   :: bin2d_pts(:), counter3d(:,:,:) 
   type (field3DReal), pointer :: varce3d 
   type (field2DReal), pointer :: varce2d
   type (field1DReal), pointer :: varce1d
   integer, allocatable :: bin_pts_glo(:)

   integer              :: ll, ll_tot,ll_ind
   real(kind=8)         :: limit0
   real, allocatable    :: oned_lensup0(:), oned_len(:),len_field(:,:,:),vorr3d(:,:,:),med_len_field(:)

   real :: sum0
   logical :: file_here = .false.

   stderr = 0
   stdout = 6

!----------------------------------------------------------------------------------------------------
! Read the variable in argument
!----------------------------------------------------------------------------------------------------
   numarg = iargc()
   if ( numarg /= 1 )then
      write(UNIT=6,FMT='(a)') &
        "Usage: gen_be_stage3 need one control variable as argument Stop"
      stop
   end if
   ! Initialise to stop false Cray compiler warnings
   variable=""
   call getarg( 1, variable )

!-----------------------------------------------------------------------------------------------------
   write(6,'(a)')' [1] Initialize namelist variables and other scalars.'
!-----------------------------------------------------------------------------------------------------
   call da_get_unit(ounit)
   call da_get_unit(iunit)

   call read_namelist(nVar2d,nVar3d)
   call get_vardim('mesh_grid.nc', Dim1, Dim2, Dim3)
   call init_mesh('mesh_grid.nc', mesh, model)
   call init_bins('bins.nc', bins)
   bins%bin2d_pts%array = 0
   bins%counter2d%array = 0
   allocate(bin_pts_glo(1:bins%num_bins2d))
   bin_pts_glo = 0
   
   call read_varname(variable, vardim)   
   call allocate_field(field2d, variable, Dim1, Dim2)
   call allocate_field(field, variable, Dim1, Dim2, Dim3)

   inv_nij = 1.0 / real(Dim1*Dim2)
   if ( vardim .eq. 2 ) Dim3 = 1 
   allocate( bv2(1:Dim3,1:bins%num_bins2d) )
   allocate( bv(1:Dim3,1:Dim3,1:bins%num_bins2d) )
   allocate( bv4d(1:2,1:Dim3,1:Dim1,1:Dim2) )
   allocate( var_bin_field(1:Dim3,1:Dim3,1:bins%num_bins2d) )
   allocate( vorr(1:Dim3,1:Dim3,1:bins%num_bins2d) )
   allocate( counter3d(1:Dim1,1:Dim2,1:Dim3) )
   counter3d = 0
   bv = 0.
   bv2 = 0.
   var_bin_field = 0.
   vorr = 0.
   allocate( varce_pts(1:Dim1,1:Dim2,1:Dim3)) 
   varce_pts = 0. 

   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
   write(6,'(4a)')' Computing vertical error statistics for dates ', start_date, ' to ', end_date
   write(6,'(a,i8,a)')' Interval between dates = ', interval, 'hours.'
   write(6,'(a,i8)')' Number of ensemble members at each time = ', ne

   date = start_date
   cdate = sdate
   counter = 0

   !-----------------------------------------------------------------------------------------------------
   write(6,'(2a)')' [2] Compute vertical bv for variable ', variable
   !-----------------------------------------------------------------------------------------------------

      do while ( cdate <= edate )
         do member = 1, ne
            write(6,'(5a,i4)')'    Processing data for date ', date, ', variable ', trim(variable), &
                              ' and member ', member
            write(ce,'(i3.3)')member
            filename=trim(variable)
            filename=trim(filename)//'/'//trim(filename)//'.'//date(1:10)//'.e'//trim(ce)
            inquire(file=trim(filename),exist=file_here)
            if (file_here) then

            call update_dynamic_mask(bins, mesh, date, ce, dyn_mask)	    
	
	    if ( use_global_bin ) then
               ! Compute global vertical background error matrix, store it in bin 1
               ! Remove mean field:
               do k = 1, Dim3
                  mean_field = sum(field%array(1:Dim1,1:Dim2,k)) * inv_nij
                  field%array(1:Dim1,1:Dim2,k) = field%array(1:Dim1,1:Dim2,k) - mean_field
               end do
               bins%bin_type         = 5
               bins%bin2d%array(:,:) = 1
               num_bins2d            = 1
            else
               num_bins2d            = bins%num_bins2d
            end if

            write(*,*)'-->read filename ',filename
            if ( vardim .eq. 2 ) then ! 2D field
               call read_variable(filename, field2d,1)
               field%array(:,:,1) = field2d%array(:,:)
            else
               call read_variable(filename, field, 1)
               write(*,*)' field ',field%array(10,10,10)
            end if


            ! Compute local (2D-binned) vertical background error matrix
            do j = 1, Dim2
                do i = 1, Dim1
                   b = bins%bin2d%array(i,j)
                   bin_pts_glo(b) = bin_pts_glo(b) + 1
                   coeffa_glob    = 1.0 / real(bin_pts_glo(b))
                   coeffb_glob    = real(bin_pts_glo(b)-1)*coeffa_glob
                   do k1 = 1, Dim3
                      ! raw variance
                      counter3d(i,j,k1) = counter3d(i,j,k1) + 1
                      coeffa3d = 1.0 / real(counter3d(i,j,k1))
                      coeffb3d = (real(counter3d(i,j,k1)-1)) * coeffa3d 
                      varce_pts(i,j,k1) = coeffb3d*varce_pts(i,j,k1) + field%array(i,j,k1)*field%array(i,j,k1)*coeffa3d
                      bv2(k1,1) = bv2(k1,1) + field%array(i,j,k1)*field%array(i,j,k1)
                      ! vertical autocovariance
                      do k2=1, k1
                         bv(k1,k2,b) =  coeffb_glob*bv(k1,k2,b) + coeffa_glob*field%array(i,j,k1)*field%array(i,j,k2)
                         !if ( .not. use_med_ls ) then
                         !    bv4d(1,k2,i,j) =  bv4d(k1,k2,i,j) + field%array(i,j,k1)*field%array(i,j,k2)
                         !end if
                      end do
                      
                      if ( .not. use_med_ls ) then
                             bv4d(1,k1,i,j) =  bv4d(k1,k1,i,j)   + field%array(i,j,k1)*field%array(i,j,k1)
                             if (k1 .lt. 1) bv4d(2,k1,i,j) =  bv4d(k1,k1-1,i,j) + field%array(i,j,k1)*field%array(i,j,k1-1)
                      end if
                      ! variance filtered
                      if ( pass_filter(variable,field%array(i,j,k1)) ) then
                            bins%counter2d%array(b,k1) = bins%counter2d%array(b,k1)+ 1
                            coeffa = 1.0 / real(bins%counter2d%array(b,k1))
                            coeffb = real(bins%counter2d%array(b,k1)-1) * coeffa
                            var_bin_field(k1,k1,b) = coeffb*var_bin_field(k1,k1,b) + coeffa*field%array(i,j,k1)**2
                      end if
                  end do
                end do
              end do
             counter = counter + 1
             !write(*,*)'vertical variance ',bv2          

            end if ! end of test file_here

         end do  ! End loop over ensemble members.

         ! Calculate next date:
         call da_advance_cymdh( date, interval, new_date )
         date = new_date
         read(date(1:10), fmt='(i10)')cdate
      end do     ! End loop over times.

      deallocate(bin_pts_glo)

      rnorm = real ((Dim1*Dim2)*counter) 


      !  Fill in upper-right part of BE matrix by symmetry:
      do b = 1, bins%num_bins2d
         do k1 = 1, Dim3
            do k2 = k1+1, Dim3 ! Symmetry.
               bv(k1,k2,b)   = bv(k2,k1,b)
               bins%counter3d%array(b,k1,k2) = bins%counter3d%array(b,k2,k1)
            end do
         end do
      end do
     
      if ( num_bins2d .eq.1 ) then
         do b = 1, bins%num_bins2d
            bv(:,:,b) = bv(:,:,1)
         end do
      end if 
      
  ! Store vertical BE matrix in netcdf
  filename  = 'gen_be_stage3_vert_autocovarce.'//trim(variable)//'.nc'
  fieldname = 'vert_autocovarce_'//trim(variable)
  if ( vardim .eq. 3 ) then
     call allocate_field(vert_varce3d, trim(fieldName), Dim3, Dim3, bins%num_bins2d)
     vert_varce3d%array(:,:,:) = bv(:,:,:)
     !vert_varce3d%array(:,:,:) = var_bin_field(:,:,:)
     call write_variable(filename, vert_varce3d, mesh, bins, 0)
     call deallocate_field(vert_varce3d) 
  else
     call allocate_field(vert_varce1d, trim(fieldName), bins%num_bins2d)
     vert_varce1d%array(:) = bv(1,1,:)
     !vert_varce1d%array(:) = var_bin_field(1,1,:)
     call write_variable(filename, vert_varce1d, mesh, bins, 0)
     call deallocate_field(vert_varce1d) 
  end if
  ! Store vertical variance by bins in netcdf
  filename = 'gen_be_stage3_vert_varce.'//trim(variable)//'.nc'
  fieldName = 'vert_varce_'//trim(variable)
  if ( vardim .eq. 3 ) then
     call allocate_field(varce2d, variable, Dim3, bins%num_bins2d)
     varce2d%ioinfo%fieldName = fieldName
     do b = 1, bins%num_bins2d
         do k = 1, Dim3
            varce2d%array(k,b) = bv(k,k,b)
         end do
     end do
     call write_variable(filename, varce2d, mesh, bins, 0)
     call deallocate_field(varce2d) 
  else
     call allocate_field(varce1d, trim(fieldName), bins%num_bins2d)
     varce1d%array(:) = bv(1,1,:) 
     call write_variable(filename, varce1d, mesh, bins, 0)
     call deallocate_field(varce1d)
  end if
  ! Store  raw variance 
  filename = 'gen_be_stage3_varce.'//trim(variable)//'.nc'
  fieldName = 'varce_'//trim(variable)
  if ( vardim .eq. 3 ) then
     call allocate_field(varce3d, trim(fieldName), Dim1, Dim2, Dim3)
     varce3d%array(:,:,:) = varce_pts(:,:,:)
     write(*,*)'variance 3d : ',varce_pts(10,10,:) 
     call write_variable(filename, varce3d, mesh, 0)
     call deallocate_field(varce3d)
  else
     call allocate_field(varce2d, trim(fieldName), Dim1, Dim2)    
     varce2d%array(:,:) = varce_pts(:,:,1)
     call write_variable(filename, varce2d, mesh, 0) 
     call deallocate_field(varce2d)
  end if


!============================================
if ( .not. data_on_levels ) then
!============================================

!-----------------------------------------------------------------------------------------------------
  write(6,'(2a)')' [3] Calculate eigenvectors and eigenvalues for variable ', variable
!-----------------------------------------------------------------------------------------------------
      allocate( work(1:Dim3,1:Dim3) )
      allocate( e_vec_loc(1:Dim3,1:Dim3,1:bins%num_bins2d) )
      allocate( e_val_loc(1:Dim3,1:bins%num_bins2d) )
      allocate( e_vec(1:Dim3,1:Dim3) )
      allocate( e_val(1:Dim3) )

      if ( use_global_bin ) then
        !  Use domain-averaged BV stored in bin 1, compute EOF
        write(unit=6,fmt='(a)') " Use Domain-averaged BE decomposition  "
        work(1:Dim3,1:Dim3) = bv(1:Dim3,1:Dim3,1)
        call da_eof_decomposition( Dim3, work, e_vec, e_val )
        if ( testing_eofs ) then
           call da_eof_decomposition_test( Dim3, work, e_vec, e_val )
        end if
        do b = 1, bins%num_bins2d
           e_vec_loc(1:Dim3,1:Dim3,b) = e_vec(1:Dim3,1:Dim3)
           e_val_loc(1:Dim3,b) = e_val(1:Dim3)
           write(6,*)'e_val = ',e_val
           write(6,*)'e_vec = ',e_vec(1,:)           
        end do
     else
        ! Use local (2D-binned) BV
        do b = 1, bins%num_bins2d
           write(6,'(2(a,i6))')' Calculate eigenvectors and eigenvalues for bin ', b, &
                ' of ', bins%num_bins2d
           work(1:Dim3,1:Dim3) = bv(1:Dim3,1:Dim3,b)
           call da_eof_decomposition(Dim3, work, e_vec, e_val ) 
           if ( testing_eofs ) then
              call da_eof_decomposition_test( Dim3, work, e_vec, e_val )
           end if
           e_vec_loc(1:Dim3,1:Dim3,b) = e_vec(1:Dim3,1:Dim3)
           e_val_loc(1:Dim3,b) = e_val(1:Dim3)
        end do
     end if

     !rizvi
     e_val=max(rsmall0,e_val)
     e_val_loc=max(rsmall0,e_val_loc)
     !rizvi

     !  Output eigenvectors, eigenvalues for use in WRF_Var:
     filename = 'gen_be_stage3.'//trim(variable)//'.dat'
     write(unit=6,fmt='(2a)') " Write eigenvec and eigenval into ", trim(filename)
     open (ounit, file = filename, form='unformatted')
     write(ounit)variable
     write(ounit)Dim3, bins%num_bins2d
     write(ounit)e_vec
     write(ounit)e_val
     write(ounit)e_vec_loc
     write(ounit)e_val_loc
     close(ounit)

     !------------------------------------------------------------------
     ! Write NetCdF Format
     !------------------------------------------------------------------
     filename='gen_be_stage3_eigen.'//trim(variable)//'.nc'
     write(*,*)'varDim ',varDim, trim(variable)
     call allocate_eigenvar(eigen, use_global_eofs ,trim(variable), vardim, Dim3, bins%num_bins2d)
     variabl2 = 'eigen_vector_'//trim(variable)
     variabl3 = 'eigen_value_'//trim(variable)
     if (  eigen%vec%IDdim .eq. 3 ) then           ! variable 3d + no global_eof
         eigen%vec%field%field3d%ioinfo%fieldname = trim(variabl2)
         eigen%val%field%field3d%ioinfo%fieldname = trim(variabl3)
         eigen%vec%field%field3d%array = e_vec_loc
         eigen%val%field%field2d%array = e_val_loc
     else if (  eigen%vec%IDdim .eq. 2 ) then      ! variable 3d + global_eof
         eigen%vec%field%field2d%ioinfo%fieldname = trim(variabl2)
         eigen%val%field%field1d%ioinfo%fieldname = trim(variabl3)
         eigen%vec%field%field2d%array = e_vec
         eigen%val%field%field1d%array = e_val
     else if (  eigen%vec%IDdim .eq. 1 ) then      ! variable 2d + no global_eof
         eigen%vec%field%field1d%ioinfo%fieldname = trim(variabl2)
         eigen%val%field%field1d%ioinfo%fieldname = trim(variabl3)
         eigen%vec%field%field1d%array = e_vec_loc(1,1,1:bins%num_bins2d)
         eigen%val%field%field1d%array = e_val_loc(1,1:bins%num_bins2d)
     else if (  eigen%vec%IDdim .eq. 0 ) then      ! variable 2d + global_eof
         eigen%vec%field%field0d%ioinfo%fieldname = trim(variabl2)
         eigen%val%field%field0d%ioinfo%fieldname = trim(variabl3)
         eigen%vec%field%field0d%scalar = e_vec(1,1)
         eigen%val%field%field0d%scalar = e_val(1)
     end if        
     call io_output_eigen(filename, eigen, Dim3, bins%num_bins2d)
     call deallocate_eigenvar(eigen)

     !----------------------------------------------------------------------
    
     !  Map binned eigenvectors to x, y grid, and take sqrt(this is used in WRF-Var):
     if ( use_global_bin ) then
        allocate( evecbin(1:bins%num_bins2d,1:Dim3,1:Dim3) )
        allocate( evalbin(1:bins%num_bins2d,1:Dim3) )
        evecbin = 0.0
        evalbin = 0.0
     else
        allocate( evec(1:Dim2,1:Dim3,1:Dim3) )
        allocate( eval(1:Dim2,1:Dim3) )
        ! use local decomposition (which is global if use_global_eof is .true.)
        do j = 1, Dim2
           do i = 1, Dim1
              b = bins%bin2d%array(i,j)
              evec(j,1:Dim3,1:Dim3) = e_vec_loc(1:Dim3,1:Dim3,b)
              eval(j,1:Dim3) = sqrt(e_val_loc(1:Dim3,b))
           end do
        end do
     end if

  !-----------------------------------------------------------------------------------------------------
   write(6,'(2a)')' [4] Transform perturbations (or not), and output.'
  !-----------------------------------------------------------------------------------------------------
   date = start_date
   cdate = sdate
   allocate( vertical_wgt(1:Dim1,1:Dim2,1:Dim3) )
   call allocate_field(field_out, variable, Dim1, Dim2, Dim3)
   vertical_wgt(1:Dim1,1:Dim2,1:Dim3) = 1.0 ! vertical_ip = 0 hardwired.
   counter = 0

    do while ( cdate <= edate )
       do member = 1, ne

            write(6,'(5a,i4)')'    Date = ', date, ', variable ', trim(variable), &
                              ' and member ', member
            write(ce,'(i3.3)')member
            filename=trim(variable)
            filename=trim(filename)//'/'//trim(filename)//'.'//date(1:10)//'.e'//trim(ce)
            file_here = .false.
            inquire(file=trim(filename),exist=file_here)
            if (file_here) then           
 
            counter = counter + 1
            call update_dynamic_mask(bins, mesh, date, ce, dyn_mask)
            write(*,*)'-->read filename ',filename

            if ( vardim .eq. 3) then
            
               call read_variable(filename, field,1)

               if ( data_on_levels ) then
                  ! Keep data on vertical levels:
                  field_out%array(:,:,:) = field%array(:,:,:)
               else
	          ! Redefine evec/eval in rainy cases
                  if ( use_global_bin ) then
                     do b=1,bins%num_bins2d
                        evecbin(b,1:Dim3,1:Dim3)=e_vec_loc(1:Dim3,1:Dim3,b)
                        evalbin(b,1:Dim3) = sqrt(e_val_loc(1:Dim3,b))
                     end do
		     ! use different routine because this is 3D
                     call da_transform_vptovv_bin7(evecbin,evalbin,vertical_wgt,field%array,field_out%array,&
                         bins%num_bins2d,Dim1,Dim2,Dim3,bins%bin2d%array)
                  else
                  ! Project fields onto vertical modes:

        	      call da_transform_vptovv( evec, eval, vertical_wgt, &
                      field%array, field_out%array, Dim3, &
                      1, Dim3, & ! WRF ids, ide etc.
                      1, Dim1, 1, Dim2, 1, Dim3, & ! WRF ims, ime etc.
                      1, Dim1, 1, Dim2, 1, Dim3 )  ! WRF its, ite etc.
                      !write(*,*)'field_out field',trim(filename),field_out%array(10,10,1),field%array(10,10,1)
                      !do kk = 1, Dim3
                      !   write(*,*)'kk ',kk,vertical_wgt(1,1,kk)
                      !   write(*,*)'--> field_out,field ',field_out%array(10,10,kk),field%array(10,10,kk)
                      !   write(*,*)'--> evec ',evec(1,1,kk)
                      !   write(*,*)'--> eval ',eval(1,kk)
                      !end do
   	          end if
               end if

            else ! vardim = 2
               call read_variable(filename, field2d,1)
               field_out%array = 0.0
               field_out%array(:,:,1) = field2d%array
               Dim3 = 1 
            end if ! end of test on vardim 

            ! Output fields (split into 2D files to allow parallel horizontal treatment):
            do k = 1, Dim3
               write(ck,'(i3.3)')k
               !if ( k < 10 ) ck = '0'//ck(2:2)
               ! Assumes variable directory has been created by script:
               filename = trim(variable)//'/'//date(1:10)//'.'//trim(variable)
               filename = trim(filename)//'.e'//trim(ce)//'.'//trim(ck)
               !filename = trim(filename)//'.e'//ce
               write(*,*)'filename ',filename
               open (ounit, file = filename, form='unformatted')
               write(ounit)Dim1, Dim2, k
               write(ounit)field_out%array(1:Dim1,1:Dim2,k)
               !write(*,*)'field_out field',trim(filename),field_out%array(10,10,k),field%array(10,10,k)
               close(ounit)
            end do

         end if ! End of condition if file_here

         end do ! End loop over members.

         ! Calculate next date:
         call da_advance_cymdh( date, interval, new_date )
         date = new_date
         read(date(1:10), fmt='(i10)')cdate
      end do

      call deallocate_field(field_out)
      deallocate(vertical_wgt)



!=========================================================================
 else ! decomposition in physical space
!=========================================================================

   write(*,*)''
  !============================================================================================
   write(6,'(a)')' [4] Compute vertical length scale in physics space '
  !============================================================================================
 
   ni = mesh%Dim1
   nj = mesh%Dim2
   nk = mesh%Dim3
   nsig = mesh%Dim3

   ! allocate section
   call allocate_field(field3d, trim(variable), ni, nj, nk)
   fieldName = 'vert_lenscale_'//trim(variable)
   call allocate_field(vl, trim(fieldName), nsig, bins%num_bins2d)
   allocate(tmp(1:nsig))

   date = start_date
   cdate = sdate
   counter = 0

   do while ( cdate <= edate )

      do member = 1, ne

         write(UNIT=6,FMT='(5a,i4)')'    Date = ', date, ', variable ', trim(variable), &
                           ' and member ', member
         write(UNIT=ce,FMT='(i3.3)')member
         filename=trim(variable)//'/'//trim(variable)//'.'//date(1:10)//'.e'//trim(ce)
         inquire(file=trim(filename),exist=file_here)
         if (file_here) then

         counter = counter + 1
    
         write(*,*)'-->read filename ',filename

         if ( vardim .eq. 3 ) then
            call read_variable(filename, field3d,1)
            write(*,*)'field3d%array(i,j,k) ',field3d%array(10,10,10)
         else ! vardim = 2
            call read_variable(filename, field2d,1)
            field3d%array=0.0
            field3d%array(:,:,1) = field2d%array
            Dim3 = 1
         end if

         ! Output fields (split into 2D files to allow parallel horizontal treatment):
         do k = 1, Dim3
               write(ck,'(i3.3)')k
               !if ( k < 10 ) ck = '0'//ck(2:2)
               ! Assumes variable directory has been created by script:
               filename = trim(variable)//'/'//date(1:10)//'.'//trim(variable)
               filename = trim(filename)//'.e'//ce//'.'//ck
               open (ounit, file = filename, form='unformatted')
               write(ounit)Dim1, Dim2, k
               write(ounit)field3d%array(1:Dim1,1:Dim2,k)
               write(*,*)'field3d field',trim(filename),field3d%array(10,10,k)
               close(ounit)
          end do

          end if ! end of test file_here

      end do ! loop member


  ! Calculate next date:
   call da_advance_cymdh( date, interval, new_date )
   date = new_date
   read(date(1:10), fmt='(i10)')cdate

   end do ! loop date


   !-----------------------------------------------------------
   ! Vertical length scale
   !-----------------------------------------------------------

   if ( vardim .eq. 3 ) then

     vorr = 0.0

     !------------------------------------------------
     do b=1, num_bins2d
     !------------------------------------------------

      if (vert_ls_method.eq.1) then

        do k=2, nk
            if (abs(bv(k,k-1,b)) > rsmall1) vorr(k,k,b)=bv(k,k-1,b)/sqrt(bv(k-1,k-1,b)*bv(k,k,b))
        end do
        if ((bv(1,2,b).gt.rsmall1)) vorr(1,1,b)=bv(1,2,b)/sqrt(bv(1,1,b)*bv(2,2,b))

         do k=1, nk
            ! if (vorr(k,k,b).gt. 1.0) vorr(k,k,b)= vorr(k-1,k-1,b)   
            if (k.lt.nk) then
                vl%array(k,b) = min(0.33*nk,1./sqrt(abs(2.-vorr(k+1,k+1,b)-vorr(k,k,b))))
            else
                !vl%array(k,b) = min(0.33*nk,1./sqrt(abs(2. -vorr(k,k,b) -vorr(k-1,k-1,b))))
                vl%array(k,b) = min(0.33*nk,1./sqrt(abs(2.-vorr(k,k,b)-vorr(k-1,k-1,b))))
            end if
            write(6,*)'gsi /lev/cor/ls/ ',k,vorr(k,k,b), vl%array(k,b)
            vl%array(k,b) =min(0.33*nk,1./sqrt(abs(2.-2*vorr(k,k,b))))
            write(6,*)'gsi /lev/cor/ls/ ',k,vorr(k,k,b), vl%array(k,b)
         end do

      !--------------------------------------------------
      ! approx gaussian of Daley formula, initially specific WRFDA
      !--------------------------------------------------
      else if (vert_ls_method.eq.2) then
         write(6,*)'vert_ls_method = 2 selected, gaussian approx'
         do k=1, nk
             if (k.eq.1) then
                vl%array(k,b) = min(0.33*nk,1.0/sqrt(-2*log(max(vorr(k,k,b),1e-6))))
             else
                vl%array(k,b) =min(0.33*nk,1.0/sqrt(-2*log(max(vorr(k,k,b),1e-6) )))
             end if
             write(6,*)'wrfda /lev/cor/ls/ ',k,vorr(k,k,b), vl%array(k,b)
          end do

      !--------------------------------------------------
      ! exit 
      !--------------------------------------------------
      else
          write(6,*)'only vert_lenscale_ls = 1,2 are defined'
          write(6,*)'check your namelist.input or add a new method in thegen_be_stage3.F code'
          stop
      end if

      ! filter lengthscale
      allocate(oned_len(1:nk))
      oned_len(:) = vl%array(:,b)
      write(*,*)("oned_len : ",oned_len )
      call filter_lenscale(oned_len, n_smth_sl, spike_tolerance, nk)
      vl%array(:,b) = oned_len(:)
      deallocate(oned_len)


      ! write verticale lenscale variable
      if ( b .eq. 1 ) then
          do b2 = 1, bins%num_bins2d
             vl%array(:,b2) = vl%array(:,1)
             vorr(:,:,b2)   = vorr(:,:,1)
          end do
      end if




     !------------------------------------------------------
     end do ! binning possible, but not actived
     !------------------------------------------------------

     write(*,*)'verticale length scale ',vl%array(:,1)
     filename = 'gen_be_stage3_vert_lenscale.'//trim(variable)//'.nc'
     call write_variable(filename, vl , mesh, bins, 0)

     do k=1, nsig
        vl%array(k,:) = vorr(k,k,:)
     end do
     vl%ioinfo%fieldName = 'vert_correl_'//trim(variable)
     filename = 'gen_be_stage3_vert_correl.'//trim(variable)//'.nc'
     write(*,*)'verticale correlation ',vl%array(:,1)
     call write_variable(filename, vl , mesh, bins, 0)


     ! Store vertical BE matrix in netcdf
     filename = 'gen_be_stage3_vert_autocovarce.filt.'//trim(variable)//'.nc'
     fieldName = 'vert_autocovarce_'//trim(variable)
     if ( vardim .eq. 3 ) then
        call allocate_field(vert_varce3d, trim(fieldName), Dim3, Dim3, bins%num_bins2d)
        vert_varce3d%array(:,:,:) = bv(:,:,:)
        call write_variable(filename, vert_varce3d, mesh, bins, 0)
        call deallocate_field(vert_varce3d)
     else
        call allocate_field(vert_varce1d, trim(fieldName), bins%num_bins2d)
        vert_varce1d%array(:) = bv(1,1,:)
        call write_variable(filename, vert_varce1d, mesh, bins, 0)
        call deallocate_field(vert_varce1d)
     end if

     !------------------------------------------------------
     end if ! end of vardim = 3
     !------------------------------------------------------

     ! deallocate section
     call deallocate_field(field3d)
     call deallocate_field(vl)
     deallocate(tmp)
     deallocate(vorr)

  !========================================================================
  end if ! end of vertical length scale physical space calculation
  !========================================================================


!---- Deallocation before leaving ! ---------------------------------------

   call deallocate_bins(bins)
   call deallocate_mesh(mesh)
   call deallocate_field(field)
   call deallocate_field(field2d)

   write(6,'(a)') 'End of program gen_be_stage3'


end program gen_be_stage3
