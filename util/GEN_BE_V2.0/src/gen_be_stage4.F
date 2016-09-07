

program gen_be_stage4_regional

#ifdef crayx1
#define iargc ipxfargc
#endif

!----------------------------------------------------------------------
! Purpose: Compute distance-binned horizontal correlation and
!          estimate the lenghtscale base on a correlation function
!
! History:
! Date     Author & Comment
! -------------------------
! dd/mm/yyyy Dale Barker
!            Initial version
! 07/04/2011 Syed RH Rizvi , NCAR/NESL/MMM/DAS 
!            Implemented Variance method (ls_method=2) for computing lengthscale
!            from Yann Michel Branch's
! 01/04/2011 J.-F. Caron (Met Office)
!            Adaptation for the UK MetO model and variational system
! 01/07/12   Gael Descombes  NCAR/ESSL/MMM/DAG (new features of gen_be)
!            update the option data_on_levels to compute horizontal lengthscale
!            in the physicals space
! --------   End History
!
!----------------------------------------------------------------------

   use variable_types
   use io_output
   use io_input
   use io_input_model
   use io_output_application


   use da_control, only : stderr, stdout, filename_len, num_fft_factors, pi
   use da_tools_serial, only : da_get_unit,da_advance_cymdh, da_free_unit,&
   da_find_fft_factors, da_find_fft_trig_funcs
   use module_ffts, only : fft551, fft661
   use da_fft_initialize
   use da_gen_be, only : da_get_field
   use da_change_wind_variables, only : da_laplacian_a
   use da_horizontal_covariances

   implicit none

   real , parameter    :: rsmall0 = 10.0e-30
   character*10        :: date, new_date             ! Current date (ccyymmddhh).
   character*10        :: variable, variable_tmp, rainc_variable     ! Variable name
   character(len=filename_len)       :: run_dir      ! Run directory.
   character(len=filename_len)       :: filename     ! Input filename.
   character(len=filename_len)       :: output_file  ! Input filename.
   character*3         :: ck                         ! Level index -> character.
   character*3         :: ci                         ! Bin index -> character.
   integer             :: ni, nj, nk                 ! Dimensions read in.
   integer             :: i,j,b,k,kdum,nkdum,nsig ! Index.
   integer             :: nn                         ! Dimension of radii bins.
   integer             :: sdate, cdate, edate        ! Starting, current ending dates.
   integer             :: member                     ! Loop counters.
   integer             :: jstart, jend               ! Starting and ending j indices
   real                :: count                      ! Counter for times/members.
   integer             :: d2                         ! Distance bin counter
   integer, allocatable:: nr(:,:)                      ! Number of points in each bin.
   real, allocatable   :: field_2d(:,:)              ! Input 2D field.
   real, allocatable   :: field_3d(:,:,:)            ! Input 3D field.
   real(kind=8), allocatable   :: field_2d8(:,:)            ! Input 3D field.
   real, allocatable   :: cov(:,:)                     ! Covariance as a function of distance.
   real, allocatable   :: cor(:,:)                     ! Correlation as a function of distance.
   real, allocatable   :: mean(:,:,:)                  ! Mean values in each distance bin.
   real, allocatable   :: var(:,:,:)                   ! Variance in each distance bin.
   real                :: coeffa,coeffb              ! Accumulating mean coefficients.
   integer             :: ifax1(1:num_fft_factors)   ! FFT factors.
   integer             :: ifax2(1:num_fft_factors)   ! FFT factors.
   integer             :: n1, n2                     ! Padded dimensions (n=dim-1+pad).
   integer             :: poisson_method             ! 1=Spectral, 2=SOR.
!   integer             :: fft_method                 ! For poisson_method=1: 1=FCT, 2=FST.
   real, allocatable   :: trigs1(:)                  ! FFT trig functions.
   real, allocatable   :: trigs2(:)                  ! FFT trig functions.
   real, allocatable   :: fft_coeffs(:,:)            ! FFT coefficients.
   real, allocatable   :: mapfac_m(:,:)              ! Map factor
   real, allocatable   :: xlat(:,:), xlon(:,:), znu(:)
   real, allocatable   :: filter(:,:)                ! Spectral filter
   real, allocatable   :: lap_field_2d(:,:)          ! 2D Laplacian of field
   real, allocatable   :: var_lap_field(:,:,:)       ! 3D Laplacian field
   real, allocatable   :: var_field(:,:,:)           ! Variance
   real, allocatable   :: len_field(:,:,:)           ! 3D Lengthscale
   real, allocatable   :: med_len_field(:)           ! Median field
   real, allocatable   :: oned_len(:)                ! Oned lengthscale array
   real, allocatable   :: med_len_field_tmp(:)           ! Median field
   
   real, allocatable   :: var_bin_field(:,:)          ! perturbation variance
   real, allocatable   :: var_bin_lap_field(:,:)      ! lap perturbation variance
   real, allocatable   :: len_bin_field(:,:)          ! perturbation lengthscale 
   real, allocatable   :: oned_lensup0(:)             ! perturbation lengthscale > 0 
   
   real, allocatable    :: mask(:,:)                 ! 2D rain class of perturbation
   
   real                :: ds                         ! Grid distance in Km.
   real                :: mean1                      ! dummy tmp
!   character (len=4)   :: horizfunct                 ! Horizontal correlation function = gaus or soar.
!   character (len=6)   :: horizvar                   ! Horizontal variable function = covar or correl.
   real(kind=8)        :: ml, sl                     ! Gradient, scalelength.

   type (mesh_type), pointer :: mesh
   type (bins_type), pointer :: bins

   integer :: numarg, iunit, ounit, namelist_unit
   integer, external     :: iargc
   integer :: ll, ll_tot,ll_ind
   real(kind=8)        :: limit0
   logical :: file_here = .false.

   type (field3dReal), pointer :: var3d_filt, var3d
   type (field3dInteger), pointer :: counter3d
   type (field2dInteger), pointer :: counter2d
   logical :: ls_stage4_med = .true.
   integer :: num_bins2d

   real                :: maxval0, maxval1, maxvar0, maxvar1     ! dummy tmp

   namelist / gen_be_stage4_nl / variable, k

   stderr = 0
   stdout = 6

   call da_get_unit(namelist_unit)

   open(unit=namelist_unit, file='gen_be_stage4_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_stage4_nl)
   close(namelist_unit)
   write(UNIT=ck,FMT='(i3.3)') k
   write(*,*)'stage4 process variable k'

!============================================================================================
   write(6,'(a)')' [1] Initialize namelist variables and other scalars.'
!============================================================================================
   run_dir = '../..'
   call da_get_unit(ounit)
   call da_get_unit(iunit)

   call read_namelist(nVar2d,nVar3d)
   write(*,*)'ls_method ',ls_method
   fft_method = 1
   
   call get_vardim('mesh_grid.nc', ni, nj, nk)
   call init_mesh('mesh_grid.nc', mesh, model)
   write(*,*)'mesh%ds%scalar ',mesh%ds%scalar
   write(*,*)'mesh%znu%array ',mesh%znu%array
   allocate(mapfac_m(ni,nj))
   mapfac_m = mesh%mapfac_m%array

   call init_bins('bins.nc', bins)
   bins%bin2d_pts%array(:)     = 0     
   bins%counter2d%array(:,:)   = 0     
   bins%counter3d%array(:,:,:) = 0
   num_bins2d = bins%num_bins2d
   if ( use_global_bin ) then
      bins%bin2d%array = 1
      bins%num_bins2d  = 1
      bins%bin_type    = 5 
   end if

   if (trim(horizvar).eq.'covar') then
      write(UNIT=6,FMT='(a)') " horizvar = Covariances"
   else
      write(UNIT=6,FMT='(a)') " horizvar = Correlation"
   endif

   if (horizfunct.eq.'gaus') then
      write(UNIT=6,FMT='(a)') " horizfunct = Gaussian"
   else
      write(UNIT=6,FMT='(a)') " horizfunct = SOAR"
   endif
   write (*,*)'ls_method ',ls_method

   write(UNIT=6,FMT='(4a)') &
      ' Computing error correlation scales for dates ',&
      start_date, ' to ', end_date
   write(UNIT=6,FMT='(a,i8,a)') &
      ' Interval between dates = ', interval, 'hours.'
   write(UNIT=6,FMT='(a,i8)') &
      ' Number of ensemble members at each time = ', ne
   write(UNIT=6,FMT='(a,i8)') &
      ' Stride over which to jump points in correlation calculation = ', stride
   write(6,'(2a)') ' rundir is ',trim(run_dir)

   ds=mesh%ds%scalar !*1000.0 ! km-> m
   sdate = 9999
   edate = 9999
   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
   date = start_date
   cdate = sdate  

!============================================================================================
! specific allocation 
!============================================================================================

   ! to initialize logical flag dyn_mask
   member=1
   write(UNIT=ce,FMT='(i3.3)')member

   call update_dynamic_mask(bins, mesh, date, ce, dyn_mask)
   allocate( mask(1:ni,1:nj) )
   mask=0

   kdum = nk
   jstart = 1
   jend   = nj
   nn = ni * ni + nj * nj

   if ( ls_method == 1 ) then    ! sl computation via guassian curve fitting

       allocate(field_2d(1:ni,1:nj))
       allocate(nr(bins%num_bins2d,0:nn))
       allocate(cov(bins%num_bins2d,0:nn))
       cov(bins%num_bins2d,0:nn) = 0.0
       allocate(var(bins%num_bins2d,0:nn,1:2))
       var(bins%num_bins2d,0:nn,1:2) = 0.0
       allocate(mean(bins%num_bins2d,0:nn,1:2))
       mean(bins%num_bins2d,0:nn,1:2) = 0.0            ! mean = 0 harcoded here
       allocate(cor(bins%num_bins2d,0:nn))
       cor(:,0:nn) = 0.0

    else if (ls_method == 2 ) then  ! sl computation via Laplacian solver method

       ! fields 2d
       allocate( lap_field_2d(1:ni,1:nj) )
       ! fields 3d
       allocate( field_3d(1:ni,1:nj,1:nk) )

       ! variances, laplacian, lengthscale
       allocate( var_bin_field(1:bins%num_bins2d,1:nk) )
       allocate( var_bin_lap_field(1:bins%num_bins2d,1:nk) )
       allocate( len_bin_field(1:bins%num_bins2d,1:nk) )
       var_bin_field = 0.0
       var_bin_lap_field = 0.0
       len_bin_field = 0.0
      
       allocate( var_field(1:ni,1:nj,1:nk) )
       allocate( var_lap_field(1:ni,1:nj,1:nk) )
       allocate( len_field(1:ni,1:nj,1:nk) )
       len_field = 0.0
       var_lap_field = 0.0
       var_field = 0.0

       ! median lengthscale
       allocate( med_len_field(1:nk) )
       allocate( med_len_field_tmp(1:nk) )
       ! oned for median resarch
       allocate( oned_len(1:(ni*nj)))
       allocate( filter(1:ni,1:nj) )

       !  Initialize FFT coefficients:
       call da_fft_initialize1( ni, nj, n1, n2, ifax1, ifax2 )
       allocate( trigs1(1:3*n1) )
       allocate( trigs2(1:3*n2) )
       allocate( fft_coeffs(1:(n1+1),1:(n2+1)) )
       call da_fft_initialize2( n1, n2, ds, trigs1, trigs2, fft_coeffs )

   else
       write(6,*)' Improper ls_method ',ls_method
       stop 
   end if   
   allocate( field_2d8(1:ni,1:nj))


!============================================================================================
! End Allocation
!============================================================================================

!=======================================================================================================
   write(6,'(a)')' [2] Input fields and calculate correlation as a function of distance between points.'
!=======================================================================================================
   count = 1.0


   do while ( cdate <= edate )
      do member = 1, ne

         write(UNIT=6,FMT='(5a,i4)')'    Date = ', date, ', variable ', trim(variable), &
                           ' and member ', member
         write(UNIT=ce,FMT='(i3.3)')member

         filename = trim(run_dir)//'/'//trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce//'.'//ck
         file_here = .false.
         inquire(file=trim(filename),exist=file_here)
         write(*,*)'filename ', trim(filename), file_here
         
         if (file_here) then
         
         coeffa = 1.0 / real(count)
         coeffb = real(count-1) * coeffa

         call update_dynamic_mask(bins, mesh, date, ce, dyn_mask)
         
         !-----------------------------------------------------------------------
         ! Calculate spatial correlation (ls_method = 1)
         !-----------------------------------------------------------------------
         if( ls_method == 1 ) then    ! sl computation via guassian curve fitting 
	   
            ! Read EOF unbalanced field:
            filename = trim(run_dir)//'/'//trim(variable)//'/'//date(1:10)
            filename = trim(filename)//'.'//trim(variable)//'.e'//ce//'.'//ck
            open(iunit, file = trim(filename), form='unformatted')
            read(UNIT=iunit)ni, nj, kdum
            read(UNIT=iunit)field_2d8
            close(UNIT=iunit)
            field_2d(:,:) = field_2d8
	   
            do b = 1, bins%num_bins2d
            	  mask=0.0
            	  ! define mask fields following current bin
            	  where ( bins%bin2d%array==b)
               		mask=1.0
            	  elsewhere
               	  	mask=0.0
            	  end where
            	  call get_grid_info_masked( ni, nj, nn, stride, nr(b,:), jstart, jend, mask )
            	  ! Calculate spatial correlation:
            	  call get_covariance_masked( ni, nj, nn, stride, count, nr(b,:), jstart, jend, field_2d, mask, cov)
            end do

         else
           !-----------------------------------------------------------------------   
           ! sl computation Laplacian method  (ls_method = 2)            
           !-----------------------------------------------------------------------   
           nsig = nk
           if ( trim(variable) == 'ps_u' .or. trim(variable) == 'ps' ) nsig = 1

           do kk = 1, nsig  
              write(UNIT=ck,FMT='(i3.3)') kk

              ! Read EOF unbalanced field:
              filename = trim(run_dir)//'/'//trim(variable)//'/'//date(1:10)
              filename = trim(filename)//'.'//trim(variable)//'.e'//ce//'.'//ck
              write(*,*)'-->read ',filename
              open(iunit, file = trim(filename), form='unformatted')
              read(iunit)ni, nj, nkdum
              read(iunit)field_2d8(:,:)
              field_3d(:,:,kk) = field_2d8

              ! Computes Laplacian fields
              call da_laplacian_a(ni, nj, n1, n2, fft_method, ifax1, ifax2, &
                   trigs1, trigs2, fft_coeffs, mapfac_m, field_3d(:,:,kk), lap_field_2d )
              write(*,*)'lap_field_2d ',lap_field_2d(10,10)
		   
              do j = 1, nj
                 do i = 1, ni
                     b = bins%bin2d%array(i,j)
                     !if ( pass_filter(variable,field_3d(i,j,kk))) then
                     !if ( lap_field_2d(i,j) .gt. 1e-13 ) then
                     if ( lap_field_2d(i,j) .gt. 1e-13 ) then
                        bins%counter2d%array(b,kk) = bins%counter2d%array(b,kk) + 1
                        coeffa = 1.0 / real(bins%counter2d%array(b,kk))
                        coeffb = real(bins%counter2d%array(b,kk)-1) * coeffa
                        var_bin_field(b,kk)     = var_bin_field(b,kk) + field_3d(i,j,kk)**2
                        var_bin_lap_field(b,kk) = var_bin_lap_field(b,kk) + lap_field_2d(i,j)**2
                        ! Accumulates variances for 3d fields
                        var_field(i,j,kk)     = var_field(i,j,kk) +field_3d(i,j,kk)**2
                        ! Accumulates variances for 3d derived field
                        var_lap_field(i,j,kk) = var_lap_field(i,j,kk) + lap_field_2d(i,j)**2
                     end if
                     !end if
                  end do 
              end do
              !Accumulates variances for 3d fields
              !var_field(:,:,kk)     = var_field(:,:,kk) +field_3d(:,:,kk)**2
              !Accumulates variances for 3d derived field
              !var_lap_field(:,:,kk) = var_lap_field(:,:,kk) + lap_field_2d(:,:)**2
              
           end do
           write(*,*)'field_3d(10,10,:) ',field_3d(10,10,:)

         end if  !  end if ls_method

         count = count + 1.0

         end if ! end if file_here


      end do ! End loop over members.

      ! Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do


  if ( ls_method == 1) then
  !----------------------------------------------------------------------------------------------
     write(UNIT=6,FMT='(a)')' [3] Compute fit of correlation to a straight line.'
  !---------------------------------------------------------------------------------------------

  do b = 1, bins%num_bins2d
     ! Transform covariances into correlations
     do d2 = 0, nn
        if ( nr(b,d2) > 0 ) then
           if (horizvar .eq. 'covar') then
              cor(b,d2)=cov(b,d2)/cov(b,0)                              ! pseudo-correlation
           else
              cor(b,d2)=cov(b,d2)/(sqrt(var(b,d2,1))*sqrt(var(b,d2,2)))   ! correlation
           end if
        end if
     end do

     call gauss_scale_length( sl, variable, ck, nn, nr(b,:), cor(b,:) )

     if (horizfunct .eq. "soar" ) then
        ! sl from gauss_scale_length is used as first guess for soar fitting
        call soar_scale_length( sl, variable, ck, nn, nr(b,:), cor(b,:) )
     end if

     ! write scale length
     if (sl/=0.0) then  
        ml = 1.0 / sl
     else
        ml = 0.0
     end if

     write(ci,fmt='(i3.3)')b
     filename = 'sl_print.b'//ci//'.l'//ck//'.'//trim(variable)
     open( unit=ounit, file=trim(filename), form='formatted', &
         action='write', access='sequential', status='replace')
     write(UNIT=ounit,FMT='(a,2e20.8)') ck, ml, sl
     close(unit=ounit, status='keep')

     deallocate (field_2d) 
     deallocate (cov)
     deallocate (cor)
     deallocate (mean)
     deallocate (var) 
  
   end do
 
  else ! ls_method == 2

     !---------------------------------------------------------------------------------------------
     write(6,'(a)')' [3] Write raw variance/lengthscale files'
     !---------------------------------------------------------------------------------------------
   
     if ( .not. use_med_ls ) then 

     write(*,*)'Compute length scales by bins'
     ! Calculate local lengthscales - Wu formula
     do b=1, num_bins2d
        write(ci,fmt='(i3.3)') b
        filename = 'sl_print.b'//ci//'.'//trim(variable)
        open( unit=ounit, file=trim(filename), form='formatted', &
             action='write', access='sequential', status='replace')
        write(6,fmt='(2a)') " [3.1] Write lengthscale into ",trim(filename)
        write(*,*)"nsig ",nsig

        maxval0 = maxval(bins%counter2d%array)
        maxvar0 = maxval(var_bin_field(b,:))

        do kk = 1,nsig

           maxval1 = bins%counter2d%array(b,kk) / maxval0
           maxvar1 = var_bin_field(b,kk) / maxvar0
           write(*,*)'maxval1 maxvar1 : ',maxval1,maxvar1
           if ( ( var_bin_lap_field(b,kk) < rsmall0 ) .or. ( maxvar1 < 1e-2 ))  then
!           if ( ( var_bin_lap_field(b,kk) < rsmall0 ) )  then
              len_bin_field(b,kk) = 0.
           else 
              len_bin_field(b,kk)=(8.0*var_bin_field(b,kk)/var_bin_lap_field(b,kk))**0.25
           end if

           if ( use_global_bin ) len_bin_field(b,kk) = len_bin_field(1,kk)
           write(unit=6,fmt='(a,i4,3e15.5)') " Level ",kk,var_bin_field(b,kk), var_bin_lap_field(b,kk),len_bin_field(b,kk)/ds 
           if (len_bin_field(b,kk)==0) then
              write(ounit,'(i2.2,2e20.5)') kk,-999.,len_bin_field(b,kk)/ds
           else
              write(ounit,'(i2.2,2e20.5)') kk,ds/len_bin_field(b,kk),len_bin_field(b,kk)/ds 
           end if

        end do
        close(ounit)
     end do

     
  else


     ! Output local variances and lengthscales, laplacian 
     ! Writes output
     write(6,'(2a)') trim(variable),' median lengthscale '

     do b=1, num_bins2d

        !var_field = max(rsmall,var_field)
        !var_lap_field = max(rsmall0,var_lap_field)
        var_lap_field = var_lap_field / float(ne)
        var_field     = var_field / float(ne)

        len_field = (8.0*var_field/var_lap_field)**0.25
        ! Writes variances
        write(6,'(2a)') variable,' variance '
        write(6,'(i2.2,e15.5)') kk,sum(var_field(:,:,kk))/real(ni*nj)
        write(6,'(3a)') 'lap ',variable,' variance '
        write(6,'(i2.2,e15.5)') kk,sum(var_lap_field(:,:,kk))/real(ni*nj)
        write(ci,fmt='(i3.3)') b
        filename = 'sl_print.b'//ci//'.'//trim(variable)
        open( unit=ounit, file=trim(filename), form='formatted', &
           action='write', access='sequential', status='replace')
           write(6,fmt='(2a)') " [3.1] Write lengthscale into ",trim(filename)

        limit0 = ds/1000 

        do kk=1,nsig


           if ( bins%num_bins2d .gt. 1 ) then
              where ( bins%bin2d%array .ne. b)
                 len_field(:,:,kk) = -999.
              end where
           end if  

	   oned_len=reshape(len_field(:,:,kk),(/ni*nj/))
           ll_tot = 0
           do ll=1, ni*nj
              if ( oned_len(ll) .gt. limit0 ) then
                 ll_tot = ll_tot + 1
              end if
           end do

           allocate(oned_lensup0(1:ll_tot))
           ll_ind = 0 
           do ll=1, ni*nj
             if ( oned_len(ll) .gt. limit0 ) then
                ll_ind = ll_ind +1
                oned_lensup0(ll_ind) = oned_len(ll)
             end if
           end do
           !write(*,*)'***************************************************'
           call median(oned_lensup0 , ll_tot, med_len_field(kk))
           !write(*,*)'median ',oned_lensup0
           !write(*,*)'median ',med_len_field(kk)/ds
           !write(*,*)'***************************************************'

           deallocate(oned_lensup0)

           med_len_field_tmp(kk) = med_len_field(kk)
           if ( med_len_field_tmp(kk) .eq. 0.0 ) then
              med_len_field_tmp(kk) = -999.
           else
             med_len_field_tmp(kk) = ds/med_len_field(kk)
           end if
           
           write(6,*) kk, ll_tot, ni*nj, med_len_field(kk)
           write(ounit,'(i2.2,2e20.5)') kk,med_len_field_tmp(kk),med_len_field(kk)/ds
           write(6,'(i2.2,2e20.5)') kk,med_len_field_tmp(kk),med_len_field(kk)/ds

           if ((use_global_bin).and.(num_bins2d.gt.1)) then
              do ii=2, num_bins2d             
                 filename = 'sl_print.b'//ci//'.'//trim(variable)
                 open( unit=ounit, file=trim(filename), form='formatted', &
                 action='write', access='sequential', status='replace')
                 write(6,fmt='(2a)') " [3.1] Write lengthscale into ",trim(filename)
                 write(6,*) kk, ll_tot, ni*nj, med_len_field(kk)
                 write(ounit,'(i2.2,2e20.5)')kk,med_len_field_tmp(kk),med_len_field(kk)/ds
                 write(6,'(i2.2,2e20.5)')kk,med_len_field_tmp(kk),med_len_field(kk)/ds
              end do
           end if

       end do
       close(unit=ounit, status='keep')
     end do

   end if ! end condition median


end if   !  end if for ls_method



!============================================================================================
write(6,'(a)') 'End of program gen_be_stage4_regional'
!============================================================================================

end program gen_be_stage4_regional
