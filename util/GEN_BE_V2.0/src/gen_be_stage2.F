

program gen_be_stage2
!---------------------------------------------------------------------- 
! Purpose: Compute regression coefficients.
!
! History:
!
! Date     Author & Comment
! -------- ----------------
! dd/mm/yy Dale Barker
!          Initial version
! 01/04/11 J.-F. Caron (Met Office)
!          Adaptation for the UK MetO model and variational system
! 01/07/11 Y. Michel (Meteo France)
!          Phasing geographical masks, combining with former stage 2a
! 01/07/12 Gael Descombes  NCAR/ESSL/MMM (new features of gen_be)  
!          Algorithm independent of the CV, implementation the method
!          to compute the regression coeff, using cholesky 
!----------------------------------------------------------------------
   use variable_types
   use io_output
   use io_input
   use io_input_model
   use io_output_application


   use da_control, only : stdout, stderr, filename_len
   use da_tools_serial, only : da_get_unit, da_advance_cymdh
   use da_gen_be, only : da_eof_decomposition,da_eof_decomposition_test, da_invert_var, &
   	read_2d_field, read_3d_field, write_2d_field, write_3d_field, update_rain_bin

   implicit none

   real, parameter     :: variance_threshold = 1e-6  ! Percentage of variance discarded.

   character*10        :: date, new_date             ! Current date (ccyymmddhh).
   character*10        :: variable                   ! Variable name
   character*3         :: ce2                        ! Variable name
   character(len=filename_len)        :: filename, varname, filename2                   ! Input filename.
   character(len=filename_len), dimension(100) :: regcoeff_name   

   integer             :: ni, nj, nk, nkdum          ! Grid dimensions.
   integer             :: i, j, k, member, member2, k1, k2, k3, m, ll, i1 ,j1 ! Loop counters.
   integer             :: b, funit                          ! Bin marker.
   integer             :: sdate, cdate, edate        ! Starting, current ending dates.

   type (field3DReal) , pointer :: varce3d, varce3d_inv, varce3d_filt, varce3d_filt2
   type (field3DReal) , pointer :: fld3d_tmp, fld3d_tmp2, fld3d_tmpa, fld3d_tmpb
   type (field1DReal) , pointer :: varce1d, varce1d_bin2d,varce1d0

   integer :: cc, vv2
   type (mesh_type), pointer :: mesh
   type (state_type), pointer :: state2, variance
   type (bins_type), pointer :: bins
   type (state_matrix_type), pointer :: covarce, regcoeff
   real :: summ, summ1, summ2

   integer :: counter

   real, allocatable :: cpt_3dfilt(:), cpt_3dfilt2(:)
   real*8 :: mask0

   ! -------------------------------------------
   type (field3dReal), pointer :: field3d, varce3d_tild, var3d_tild
   integer :: inv_dim2d, l, kb 
   type (field3dReal), pointer ::  biga8_bin
   !type (field2dReal), pointer :: bigxkm, vng, vorr, tlflt
   type (field2dReal), pointer :: func, p88_bin, vorr
   character(len=32) :: fieldName
   real :: mean_field
   integer :: ierror
   !logical :: use_chol
   logical :: file_here = .false.
   logical :: file_here2 = .false.
   !----------------------------------------------



!========================================================================================
! Allocation section
!========================================================================================
!----------------------------------------------------------------------------------------
   write(6,'(a)')' [1] Initialize namelist variables and other scalars.'
!----------------------------------------------------------------------------------------

   ! alloc mesh and state
   use_chol_reg = .false.
   call read_namelist(nVar2d,nVar3d)
   filename = 'mesh_grid.nc'
   call get_vardim(filename, Dim1, Dim2, Dim3)
   call allocate_state(state2, Dim1, Dim2, Dim3, nvar, cv_list, vardim_list)
   call check_state(state2,10,10,10)
   call init_mesh(filename, mesh, model)

   ! alloc bins
   call init_bins('bins.nc', bins)  
   write(*,*)'Dim3, nvar ',Dim3, nvar

   ! alloc matrix
   write(*,*)'cv_list ',cv_list
   !write(*,*)'covar_ID  ',covar_ID
   filename='covarce'
   call allocate_state_matrix(covarce, bins, Dim3, nvar, covar_ID,cv_list,filename)
   filename='regcoeff'
   call allocate_state_matrix(regcoeff, bins, Dim3, nvar, covar_ID, cv_list,filename)

   ! alloc one field for variance 1d,2d,3d
   varname='varce_'//cv_list(1)
   call allocate_field(varce1d, trim(varname), bins%num_bins)
   call allocate_field(varce1d_bin2d, trim(varname), bins%num_bins2d)
   call allocate_field(varce3d,trim(varname), Dim3, Dim3, bins%num_bins2d)
   call allocate_field(varce3d_filt,trim(varname), Dim3, Dim3, bins%num_bins2d)
   call allocate_field(varce3d_filt2,trim(varname), Dim3, Dim3, bins%num_bins2d)
   varname='inv_'//trim(varname)
   call allocate_field(varce3d_inv,trim(varname), Dim3, Dim3, bins%num_bins2d)
   varce1d%array = 0.0
   varce1d_bin2d%array = 0.0
   varce3d%array = 0.0
   allocate(cpt_3dfilt(Dim3))
   allocate(cpt_3dfilt2(Dim3))
                          varce3d_filt%array(:,:,:) = 0.0
                          varce3d_filt2%array(:,:,:) = 0.0
                          cpt_3dfilt(:)  = 0
                          cpt_3dfilt2(:) = 0 

   call allocate_field(fld3d_tmp,trim(varname), Dim1, Dim2, Dim3) 
   call allocate_field(fld3d_tmp2,trim(varname), Dim1, Dim2, Dim3) 
   call allocate_field(fld3d_tmpa,trim(varname), Dim1, Dim2, Dim3) 
   call allocate_field(fld3d_tmpb,trim(varname), Dim1, Dim2, Dim3) 

   vardim_list2 = vardim_list
   !vardim_list2(1) = 1
   where ( vardim_list2(:) .eq. 2 ) vardim_list2(:) = 1
   do vv=1, nvar
      tmp_list(vv)  = 'vert_varce_'//trim(cv_listu(vv))
   end do
   !call allocate_state_bin(variance, bins%num_bins, bins%num_bins2d, Dim3, nvar, tmp_list, vardim_list2)

   
   if ( use_chol_reg ) then
      call allocate_field(field3d, fieldName, Dim1, Dim2, Dim3)
      field3d%array=0.0
      call allocate_field(varce3d_tild, fieldName, Dim3, Dim3, bins%num_bins2d)
      call allocate_field(var3d_tild, fieldName, Dim1, Dim2, Dim3)
      call allocate_field(vorr, fieldName, Dim3, Dim3)
      call allocate_field(func, fieldName, Dim3, Dim3)
      call allocate_field(biga8_bin, fieldName, Dim3, Dim3, bins%num_bins2d)
      call allocate_field(p88_bin, fieldName, Dim3, bins%num_bins2d)
   end if

!---------------------------------------------------------------------------------------------
   write(6,'(2a)')' [2] Read fields, and calculate correlations'
!--------------------------------------------------------------------------------------------- 

   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
   write(6,'(4a)')' Computing regression coefficients'
   write(6,'(4a)') ' Time period is ', start_date, ' to ', end_date
   write(6,'(a,i8,a)')' Interval between dates = ', interval, 'hours.'
   write(6,'(a,i8)')' Number of ensemble members at each time = ', ne

   !=====================================================================
   do ii = 2, nvar ! loop to compute covar
   !===================================================================== 

   jj = ii -1 ! indice column
   bins%bin2d_pts%array = 0
   bins%bin_pts%array = 0

   if (ncovar_row(jj).gt.0) then

   ! for variable 3d, if there is regression for this variable in the row(jj)
   if (( use_chol_reg ) .and. (vardim_list(jj).eq.3))  then

      write(*,*)'=============================================================='
      write(*,*)' preprocess for GSI regcoeff method '
      write(*,*)'=============================================================='

      date = start_date
      cdate = sdate
      counter = 0
      fieldName=trim(cv_listu(jj))
      vorr%array = 0.0

      do while ( cdate <= edate )
         write(6,'(a,a)')' Processing data for date ', date

         do member = 1, ne
            write(ce,'(i3.3)')member
            write(*,*)'member : ',ce

            if ( vardim_list(jj).eq.3 ) then
               ! read var 
               filename=trim(cv_listu(jj))
               field3d%ioinfo%fieldname = trim(filename)
               filename=trim(filename)//'/'//trim(filename)//'.'//date(1:10)//'.e'//trim(ce)
               file_here = .false.
               inquire(file=trim(filename),exist=file_here)
               if ( file_here ) then

               counter = counter + 1
               write(*,*)'-->read filename ',filename
               call read_variable(filename, field3d, 1)
               !write(*,*)'field read ',field3d%array(10,10,10)
               ! remove 2d spatial mean
               do kk = 1, Dim3
                  mean_field = sum(field3d%array(1:Dim1,1:Dim2,kk)) * inv_dim2d
                  write(*,*)' sf(1:nx,1:ny,k) mean_field ', field3d%array(10,10,kk), mean_field 
                  field3d%array(:,:,kk) = field3d%array(:,:,kk) ! - mean_field
               end do
            
               do l=1, Dim3
                  do k=1, Dim3
                     do j=1, Dim2
                           do i=1, Dim1
                              vorr%array(l,k) = vorr%array(l,k) + field3d%array(i,j,k)*field3d%array(i,j,l)
                           end do
                     end do   
                  end do
               end do

               end if ! if the variable is present 
             end if ! if variable is 3d


      end do  ! End loop over ensemble members.

      ! Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate

   end do ! End loop over time


   do l=2, Dim3
      write(*,*)'vorr%array(l,l) ',l,vorr%array(l,l),vorr%array(l-1,l)
   end do

   !write(*,*)' vorr%array ',vorr%array(2,:)
   !write(*,*)' rlen%array ',rlen%array
   if ( vardim_list(jj).eq.3 ) then
      call vert_corr(mesh%Dim1, mesh%Dim2, mesh%Dim3, mesh%znu%array, vorr%array, func%array, ierror)
   end if
   write(*,*)'func ',func%array


   end if ! GSI specific

   write(*,*)'=============================================================='
   write(*,*)' PROCESS VARIABLE ',trim(cv_list(ii))
   write(*,*)'=============================================================='
   ! compute the variance of the variable at jj
   ! compute the covariance between the variable jj  
   ! and all the variable from ii to nvar 

   date = start_date
   cdate = sdate
   counter = 0
   do while ( cdate <= edate )
      write(6,'(a,a)')' Processing data for date ', date

      do member = 1, ne

         write(ce,'(i3.3)')member
         write(*,*)'member : ',ce
         
         filename=trim(cv_listu(jj))
         filename=trim(filename)//'/'//trim(filename)//'.'//date(1:10)//'.e'//trim(ce)
         file_here = .false.
         inquire(file=trim(filename),exist=file_here)
         if (file_here) then

         counter = counter + 1
         call update_dynamic_mask(bins, mesh, date, ce, dyn_mask)
   
         !-----------------------------------------------------------------------
         ! firstly, read all the variables available from stage1
         !-----------------------------------------------------------------------
         write(*,*) '==========================================================='
         write(*,*) ' Load state member : ',member
         write(*,*) ' Date : ',date
         write(*,*) '==========================================================='       
         ! update the list of control variables to read
         tmp_list(1:jj) = cv_listu(1:jj)
         tmp_list(jj+1:nvar) = cv_list(jj+1:nvar)
         call rename_state(state2, tmp_list)
         call read_state_from_variable_file(state2, 1, date, ce)
         if ( ( use_chol_reg ).and.(vardim_list(jj).eq.3) ) then
            var3d_tild%array =  state2%num(jj)%field%field3d%array 
            call var_tilde(Dim1, Dim2, Dim3, var3d_tild%array, func%array)
         end if
         call check_state(state2,10,10,10)
     
      
         write(*,*) '==========================================================='
         write(*,*) ' compute variance ', trim(cv_list(jj))
         write(*,*) '==========================================================='
         if  ( ( jj == 1 ).and.( ii == 2 ) ) then
             write(*,*)'Compute variance var1d-bin3d and var3d-bin2d'
             call compute_covar1d_bin3d(varce1d%array, state2%num(jj)%field%field3d%array,state2%num(jj)%field%field3d%array, bins, Dim1, Dim2, Dim3)  
             call compute_varce3dbin2d_var3d(varce3d%array, state2%num(jj)%field%field3d%array, bins, Dim1, Dim2, Dim3)  
             if ( use_chol_reg ) then
                call compute_varce3dbin2d_var3d(varce3d_tild%array,var3d_tild%array ,bins, Dim1, Dim2, Dim3) 
             end if
         end if

         do vv = ii, nvar ! for the all column jj, from  var ii to nvar         

            write(*,*)'================================================================'
            write(*,*)' compute covariance for variable ',trim(cv_list(vv))
            write(*,*)'================================================================'
            write(*,*) ''
            write(*,*) '   compute covariance with ',trim(cv_list(jj))
            !write(*,*) '   covariance dimension covar_ID(vv,jj) vv jj ',covar_ID(vv,jj), vv, jj

            if ( covar_ID(vv,jj) == 1 ) then

                       write(*,*)'compute_covar1d_bin3d ',vv,jj
                       call compute_covar1d_bin3d(covarce%num2d(vv,jj)%field%field1d%array, &
                       state2%num(vv)%field%field3d%array, &
                       state2%num(jj)%field%field3d%array, & 
                               bins, Dim1, Dim2, Dim3)

            else if ( covar_ID(vv,jj) == 2 ) then
                    if (vardim_list(vv) == 2 ) then
                       if ( use_chol_reg ) then
                          call compute_covar2d_bin2d (covarce%num2d(vv,jj)%field%field2d%array, &
                          state2%num(vv)%field%field2d%array, &
                          var3d_tild%array, bins, Dim1, Dim2, Dim3)
                          write(*,*)'var3d_tild%array ',var3d_tild%array(10,10,:)
                          write(*,*)'covarce%num2d(vv,jj)%field%field2d%array ',covarce%num2d(vv,jj)%field%field2d%array

                       else 
                          call compute_covar2d_bin2d (covarce%num2d(vv,jj)%field%field2d%array, &
                          state2%num(vv)%field%field2d%array, &
                          state2%num(jj)%field%field3d%array, &
                               bins, Dim1, Dim2, Dim3)
                       end if
                    else
                       call compute_covar2d_bin2d (covarce%num2d(vv,jj)%field%field2d%array, &
                       state2%num(jj)%field%field2d%array, &
                       state2%num(vv)%field%field3d%array, &
                               bins, Dim1, Dim2, Dim3)
                    end if       
            else if ( covar_ID(vv,jj) == 3 ) then
                    if (  use_chol_reg ) then
                       call compute_covar3d_bin2d(covarce%num2d(vv,jj)%field%field3d%array, &
                       state2%num(vv)%field%field3d%array, &
                       var3d_tild%array, &
                              bins, Dim1, Dim2, Dim3)

                    else 
                       call compute_covar3d_bin2d(covarce%num2d(vv,jj)%field%field3d%array, &
                       state2%num(vv)%field%field3d%array, &
                       state2%num(jj)%field%field3d%array, &
                               bins, Dim1, Dim2, Dim3)
                    end if

            end if

         end do

         call check_matrix(covarce,1,1,1)

      ! Increment bins
      call increment_bin_pts(bins, Dim1, Dim2, Dim3)

      end if ! if file_here condition
	 
     !--------------------------------------------------------------
      end do  ! End loop over ensemble members.
     !--------------------------------------------------------------

      ! Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate


!------------------------------------------------------------
   end do     ! End loop over times.
!------------------------------------------------------------


  if ( state2%num(jj)%IDdim .ne. 2 ) then

     if ( use_chol_reg ) then
        write(*,*)'================================================================'
        write(*,*)'[3] Compute functional, cholesky',trim(cv_list(jj))
        write(*,*)'================================================================'

        write(*,*)'varce3d_tild%array ',varce3d_tild%array(1,:,1)

        call chol_factor(biga8_bin%array, p88_bin%array, varce3d_tild%array, Dim3, bins%num_bins2d, ierror)
        if ( ierror .ne. 0 ) then
           write(6,*)'Error in chol_factor subroutine'
        end if 

     else

        varce3d_inv%array = 0.0
        write(*,*)'varce3d%array ',varce3d%array(1,1,1),varce3d%array(10,1,1)
        write(*,*)'================================================================'
        write(*,*)'[3]  Calculate eigenvectors, eigenvalues and inverse for covariance ',trim(cv_list(jj))
        write(*,*)'================================================================'
        testing_eofs = .true.
        call da_invert_var(varce3d_inv%array, varce3d%array, bins%num_bins2d, Dim3, testing_eofs)
        write(*,*)'varce3d%array ',varce3d%array(1,1,1),varce3d%array(10,1,1)
        write(*,*)'varce3d_inv%array ',varce3d_inv%array(1,1,1),varce3d_inv%array(10,1,1)

     end if

  end if


   write(6,'(2a)')' [4] Calculate regression coefficients '
   write(*,*)'covar_ID  ',covar_ID
   write(*,*)'covar_ID2 ',covar_ID2
   ll = 0

  !---------------------------------------------------------------------------------------------
   do vv = ii, nvar ! for the all column jj, from  var ii to nvar
  !---------------------------------------------------------------------------------------------

      write(*,*)'covar_ID2(vv,jj) ',covar_ID2(vv,jj),vv,jj

      !---------------------------------------------------------------------------------------------
      if ( covar_ID(vv,jj) == 1 ) then
      !---------------------------------------------------------------------------------------------
         ll = ll + 1
         regcoeff_name(ll) = trim(regcoeff%num2d(vv,jj)%field%field1d%ioinfo%fieldName)
         write(*,*)'Compute Recoeff 1d: ',trim(regcoeff_name(ll))
         do b = 1, bins%num_bins
            if ( varce1d%array(b) .ne. 0  ) then
               regcoeff%num2d(vv,jj)%field%field1d%array(b) = &
                  covarce%num2d(vv,jj)%field%field1d%array(b) / varce1d%array(b)
            end if
         end do
         write(*,*)'regcoeff%num2d(vv,jj)%field%field1d%array ',regcoeff%num2d(vv,jj)%field%field1d%array
         write(*,*)'coucou'


      !---------------------------------------------------------------------------------------------
      else if ( covar_ID(vv,jj) == 2 ) then
      !---------------------------------------------------------------------------------------------

         ll = ll + 1
         regcoeff_name(ll) = trim(regcoeff%num2d(vv,jj)%field%field2d%ioinfo%fieldName)
         write(*,*)'Compute Recoeff 2d: ',trim(regcoeff_name(ll))

         if ( (state2%num(jj)%IDdim .eq. 3) .and. (state2%num(vv)%IDdim .eq. 2) ) then
             regcoeff_name(ll) = trim(regcoeff%num2d(vv,jj)%field%field2d%ioinfo%fieldName)

            if ( use_chol_reg ) then
               write(*,*)'func%array reg2d ',func%array
               call chol_reg_coeff2d(regcoeff%num2d(vv,jj)%field%field2d%array, covarce%num2d(vv,jj)%field%field2d%array, &
                               biga8_bin%array, p88_bin%array, func%array, Dim3, bins%num_bins2d)
            else
               do b = 1, bins%num_bins2d
                  do k1 = 1, Dim3
                     summ = 0.0
                     do k2 = 1, Dim3
                        if (varce3d_inv%array(k2,k1,b).ne.0) then
                           summ = summ + covarce%num2d(vv,jj)%field%field2d%array(k2,b) * &
                            varce3d_inv%array(k2,k1,b)
                        end if
                     end do
                     regcoeff%num2d(vv,jj)%field%field2d%array(k1,b) = summ
                  end do
               end do
            end if
            write(*,*)'regcoeff 2d ',regcoeff%num2d(vv,jj)%field%field2d%array

         else  ! dimension var(jj) = 2

            do b = 1, bins%num_bins2d
               do k1 = 1, Dim3
                   if ( varce1d_bin2d%array(b) .ne. 0  ) then
                      regcoeff%num2d(vv,jj)%field%field2d%array(k1,b) = &
                       covarce%num2d(vv,jj)%field%field2d%array(k1,b) / varce1d_bin2d%array(b)
                   end if
               end do
            end do

         end if

      !---------------------------------------------------------------------------------------------
      else if ( covar_ID(vv,jj) == 3 ) then
      !---------------------------------------------------------------------------------------------
         ll = ll + 1
         regcoeff_name(ll) = trim(regcoeff%num2d(vv,jj)%field%field3d%ioinfo%fieldName)
         write(*,*)'Compute Recoeff 3d: ',trim(regcoeff_name(ll))

         if ( use_chol_reg ) then
             call chol_reg_coeff3d(regcoeff%num2d(vv,jj)%field%field3d%array, covarce%num2d(vv,jj)%field%field3d%array, &
                                biga8_bin%array, p88_bin%array, func%array,Dim3, bins%num_bins2d)

         else if ( covar_ID2(vv,jj) == 2 ) then
            do b = 1, bins%num_bins2d
               do k1 = 1, Dim3
                  if ( varce3d%array(k1,k1,b) .ne. 0 ) then
                        regcoeff%num2d(vv,jj)%field%field3d%array(k1,k1,b)  =  covarce%num2d(vv,jj)%field%field3d%array(k1,k1,b) / varce3d%array(k1,k1,b)
                  end if
               end do
            end do

         else
            do b = 1, bins%num_bins2d
               do k1 = 1, Dim3
                  do k2 = 1, Dim3
                     summ = 0.0
                     do k3 = 1, Dim3
                        if ( varce3d_inv%array(k3,k2,b) .ne. 0 ) then
                           summ = summ + covarce%num2d(vv,jj)%field%field3d%array(k1,k3,b) * &
                            varce3d_inv%array(k3,k2,b)
                        end if
                     end do
                     regcoeff%num2d(vv,jj)%field%field3d%array(k1,k2,b) = summ
                  end do
               end do
            end do

         end if

      !---------------------------------------------------------------------------------------------
      end if ! conditions dimensions covarce
      !---------------------------------------------------------------------------------------------



  !---------------------------------------------------------------------------------------------
   end do ! loop on line of the jj row
  !---------------------------------------------------------------------------------------------



   write(*,*)'variance psi   = ',varce3d%array(1,1,1),varce3d%array(10,1,1)
   write(*,*)'varce3d_inv%array ',varce3d_inv%array(1,1,1)
   write(*,*)'reg covar 1,1,1'
   !call check_matrix(covarce,1,1,1)
   write(*,*)'reg covar 10,1,1'
   !call check_matrix(covarce,10,1,1)

   write(*,*)'reg coeff'
!   call check_matrix(regcoeff,1,1,1)
!   write(*,*)'reg coeff 10,1,1'
!   call check_matrix(regcoeff,10,1,1)

!   write(*,*)'varce1d%array ',sqrt(varce1d%array)

end if ! if there is at least one covariance [ (ncovar_row(jj).gt.0) ]

!-----------------------------------------------------------------------
! filter
!-----------------------------------------------------------------------
!   if ( num_passes > 0 ) then
!      write(6,'(a,i4,a)') '[3] Apply ', &
!      num_passes, ' pass recursive filter to regression coefficients:'
!      call da_filter_regcoeffs( ni, nj, nk, num_bins, num_bins2d, num_passes, rf_scale, bin, &
!      !rizvi                                regcoeff1, regcoeff2, regcoeff3, regcoeff3p )
!                                regcoeff1, regcoeff2, regcoeff3)
!   else
!      write(6,'(a)') &
!      ' [3] num_passes = 0. Bypassing recursive filtering.'
!   end if
!------------------------------------------------------------------------

   write(*,*) '==========================================================='
   write(*,*) ' compute variance : ',trim(cv_listu(ii))
   write(*,*) '==========================================================='

   write(*,*)'ii jj ',ii,jj
   bins%bin2d_pts%array = 0
   bins%bin_pts%array = 0
   varce3d%array=0
   varce1d%array=0
   varce1d_bin2d%array=0
   counter = 0  

   cdate = sdate
   date = start_date
   do while ( cdate <= edate )
      write(6,'(a,a)')' Processing data for date ',date

      do member = 1, ne
         
         ! write unbalanced part
         write(ce,'(i3.3)')member
         filename  = trim(cv_list(ii))
         filename  = trim(filename)//'/'//trim(filename)//'.'//date(1:10)//'.e'//ce
         file_here = .false.
         inquire(file=trim(filename),exist=file_here)
         if (file_here) then
         counter = counter + 1
         filename  = trim(cv_listu(ii))
         filename  = trim(filename)//'/'//trim(filename)//'.'//date(1:10)//'.e'//ce

         call read_state_from_variable_file(state2, 1, date, ce)

         write(*,*)'write ',filename

         call update_dynamic_mask(bins, mesh, date, ce, dyn_mask)

         if ( state2%num(ii)%IDdim == 3 ) then

            ! compute and write unbalanced part
            if ( ncovar(ii).gt. 0 ) then
               write(*,*)"========================================="
               write(*,*)'var 3d',trim(state2%num(ii)%field%field3d%ioinfo%fieldname), state2%num(ii)%field%field3d%array(10,10,10)
               call compute_var3d_unbalanced(state2, ii, regcoeff, bins, Dim1, Dim2, Dim3)
               ! varname to write
               state2%num(ii)%field%field3d%ioinfo%fieldname = cv_listu(ii)
               write(*,*)'unbalanced part var_u 3d',trim(state2%num(ii)%field%field3d%ioinfo%fieldname), state2%num(ii)%field%field3d%array(10,10,10),ne
               write(*,*)"=========================================" 
               call write_variable(filename, state2%num(ii)%field%field3d , mesh, 1)
               state2%num(ii)%field%field3d%ioinfo%fieldname = cv_list(ii) 
            end if

            ! compute variance of unbalanced or full variable 
            ! this part is needed at the begining of the program
            if (ncovar_row(jj+1).gt.0) then
               write(*,*)' compute variance of unbalanced part of ',trim(state2%num(ii)%field%field3d%ioinfo%fieldname) 
               call compute_varce3dbin2d_var3d(varce3d%array, &
                          state2%num(ii)%field%field3d%array ,bins, Dim1, Dim2, Dim3)

               if ( ( use_chol_reg ) ) then
                  var3d_tild%array =  state2%num(ii)%field%field3d%array
                  call var_tilde(Dim1, Dim2, Dim3, var3d_tild%array, func%array)
               end if
            end if


         else

            ! compute and write unbalanced part
            if ( ncovar(ii).gt. 0 ) then
               call compute_var2d_unbalanced(state2, ii, regcoeff, bins, Dim1, Dim2, Dim3)
                state2%num(ii)%field%field2d%ioinfo%fieldname = cv_listu(ii)
               write(*,*)"========================================="
               write(*,*)'var 2d',trim(state2%num(ii)%field%field2d%ioinfo%fieldname), state2%num(ii)%field%field2d%array(10,10)  
               ! varname to write
               write(*,*)'unbalanced part var_u 2d ',trim(state2%num(ii)%field%field2d%ioinfo%fieldname), state2%num(ii)%field%field2d%array(10,10),ne
               write(*,*)"========================================="
               call write_variable(filename, state2%num(ii)%field%field2d , mesh, 1)
               state2%num(ii)%field%field2d%ioinfo%fieldname = cv_list(ii) 
            end if

            ! compute variance of unbalanced or full variable 
            ! this part is needed at the top part of the program
            if (ncovar_row(jj+1).gt.0) then
               call compute_varce1dbin2d_var2d(varce1d_bin2d%array, &
                          state2%num(ii)%field%field2d%array ,bins, Dim1, Dim2, Dim3)
                write(*,*)'varce1d_bin2d%array ',varce1d_bin2d%array
            end if

         end if
        
         ! Increment bins
         call increment_bin_pts(bins, Dim1, Dim2, Dim3)

         end if ! end condition file_here

      end do ! end loop on members

      ! Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate

   end do  ! end loop on time

  
!====================================================================================
   end do ! loop over the variables (by line)
!====================================================================================


  write(*,*)' write output'

  !----------------------------------------------
  ! write vertical covariance
  !---------------------------------------------- 
  !filename = 'gen_be_stage2_vert_covarce.nc'
  !call write_matrix_state(filename, covarce, bins, mesh)

  !----------------------------------------------
  ! write regression coefficient
  !---------------------------------------------- 
  filename = 'gen_be_stage2_regcoeff.nc'
  call write_matrix_state(filename, regcoeff, bins, mesh)

  !----------------------------------------------
  ! write ascii file list for automatic ncl plot
  !----------------------------------------------
  filename = "regcoeff.txt"
  call write_ascii_file(filename, regcoeff_name, ncovar_tot)
  

!====================================================================================
! Deallocation section
!====================================================================================
   call deallocate_mesh(mesh)
   call deallocate_field(varce1d)
   call deallocate_field(varce1d_bin2d)
   call deallocate_field(varce3d)
   call deallocate_field(varce3d_inv)
   call deallocate_state(state2)
   call deallocate_state_matrix(regcoeff)
   call deallocate_state_matrix(covarce)

   if ( use_chol_reg ) then
      call deallocate_field(field3d)
      call deallocate_field(varce3d_tild)
      call deallocate_field(var3d_tild)
      call deallocate_field(vorr)
      call deallocate_field(func)
      call deallocate_field(biga8_bin)
      call deallocate_field(p88_bin)
   end if


!====================================================================================
   
end program gen_be_stage2
