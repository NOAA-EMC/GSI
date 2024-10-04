module general_read_wrf_mass
!
!
!
   use kinds, only: r_kind,r_single,i_kind
   use constants, only: zero,one

   implicit none

   public general_read_wrf_mass_save
   public general_read_wrf_mass_dim_eta
   public cal_ensperts

   private

   integer :: nlat,nlon,nsig
   real(r_kind),allocatable::  eta1_ll(:)          !
   real(r_kind),allocatable:: aeta1_ll(:)          !
   real(r_kind),allocatable::  eta2_ll(:)          !
   real(r_kind),allocatable:: aeta2_ll(:)          !
   real(r_kind) pt_ll

contains

  subroutine general_read_wrf_mass_save(filename,q_hyb_ens,nnn)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    general_read_wrf_mass  read arw model ensemble members
  !   prgmmr: mizzi            org: ncar/mmm            date: 2010-08-11
  !
  ! abstract: read ensemble members from the arw model in "wrfout" netcdf format
  !           for use with hybrid ensemble option. 
  !
  ! program history log:
  !   2010-08-11  parrish, initial documentation
  !   2010-09-10  parrish, modify so ensemble variables are read in the same way as in
  !               subroutines convert_netcdf_mass and read_wrf_mass_binary_guess.
  !               There were substantial differences due to different opinion about what
  !               to use for surface pressure.  This issue should be resolved by coordinating
  !               with Ming Hu (ming.hu@noaa.gov).  At the moment, these changes result in
  !               agreement to single precision between this input method and the guess input
  !               procedure when the same file is read by both methods.
  !   2012-03-12  whitaker:  read data on root, distribute with scatterv.
  !                          remove call to general_reload.
  !                          simplify, fix memory leaks, reduce memory footprint.
  !                          use genqsat, remove genqsat2_regional.
  !                          replace bare 'stop' statements with call stop2(999).
  !   2017-03-23  Hu      - add code to use hybrid vertical coodinate in WRF MASS core
  !
  !   input argument list:
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$ end documentation block
  
      use netcdf, only: nf90_nowrite
      use netcdf, only: nf90_open,nf90_close
      use netcdf, only: nf90_inq_dimid,nf90_inquire_dimension
      use netcdf, only: nf90_inq_varid,nf90_inquire_variable,nf90_get_var
      use kinds, only: r_kind,r_single,i_kind
      use constants, only: zero,one,fv,zero_single,rd_over_cp_mass,one_tenth,h300
      use netcdf_mod, only: nc_check
  
      implicit none
  !
  ! Declare passed variables
 !     class(get_wrf_mass_ensperts_class), intent(inout) :: this
      character(255),intent(in):: filename
      logical, intent(in) :: q_hyb_ens
      integer, intent(in) :: nnn
  !
  ! Declare local parameters
      real(r_kind),parameter:: r0_01 = 0.01_r_kind
      real(r_kind),parameter:: r10   = 10.0_r_kind
      real(r_kind),parameter:: r100  = 100.0_r_kind
  !
  !   Declare local variables
      real(r_single),allocatable,dimension(:):: temp_1d
      real(r_single),allocatable,dimension(:,:):: temp_2d,temp_2d2
      real(r_single),allocatable,dimension(:,:,:):: temp_3d
      real(r_kind),allocatable,dimension(:):: p_top
      real(r_kind),allocatable,dimension(:,:):: q_integral,gg_ps,q_integralc4h
      real(r_kind),allocatable,dimension(:,:,:):: tsn,qst,prsl,&
       gg_u,gg_v,gg_tv,gg_rh
      real(r_kind),allocatable,dimension(:):: wrk_fill_2d
      integer(i_kind),allocatable,dimension(:):: dim,dim_id
  
      integer(i_kind):: nx,ny,nz,i,j,k,d_max,file_id,var_id,ndim,mype
      integer(i_kind):: Time_id,s_n_id,w_e_id,b_t_id,s_n_stag_id,w_e_stag_id,b_t_stag_id
      integer(i_kind):: Time_len,s_n_len,w_e_len,b_t_len,s_n_stag_len,w_e_stag_len,b_t_stag_len
      integer(i_kind) iderivative
  
      real(r_kind):: deltasigma
      real(r_kind) psfc_this_dry,psfc_this
      real(r_kind) work_prslk,work_prsl
  
      logical :: ice

      character(len=24),parameter :: myname_ = 'general_read_wrf_mass'

  !
  ! OPEN ENSEMBLE MEMBER DATA FILE
    if (mype==0) then ! only read data on root proc
      allocate(gg_u(nlat,nlon,nsig))
      allocate(gg_v(nlat,nlon,nsig))
      allocate(gg_tv(nlat,nlon,nsig))
      allocate(gg_rh(nlat,nlon,nsig))
      allocate(gg_ps(nlat,nlon))
      call nc_check( nf90_open(trim(filename),nf90_nowrite,file_id),&
          myname_,'open '//trim(filename) )
  !
  ! WRF FILE DIMENSIONS
      call nc_check( nf90_inq_dimid(file_id,'Time',Time_id),&
          myname_,'inq_dimid Time '//trim(filename) )
      call nc_check( nf90_inq_dimid(file_id,'south_north',s_n_id),&
          myname_,'inq_dimid south_north '//trim(filename) )
      call nc_check( nf90_inq_dimid(file_id,'west_east',w_e_id),&
          myname_,'inq_dimid west_east '//trim(filename) )
      call nc_check( nf90_inq_dimid(file_id,'bottom_top',b_t_id),&
          myname_,'inq_dimid bottom_top '//trim(filename) )
      call nc_check( nf90_inq_dimid(file_id,'south_north_stag',s_n_stag_id),&
          myname_,'inq_dimid south_north_stag '//trim(filename) )
      call nc_check( nf90_inq_dimid(file_id,'west_east_stag',w_e_stag_id),&
          myname_,'inq_dimid west_east_stag '//trim(filename) )
      call nc_check( nf90_inq_dimid(file_id,'bottom_top_stag',b_t_stag_id),&
          myname_,'inq_dimid bottom_top_stag '//trim(filename) )
  
      d_max=max(Time_id, s_n_id, w_e_id, b_t_id, s_n_stag_id, w_e_stag_id, b_t_stag_id)
      allocate(dim(d_max))
      dim(:)=-999
  
      call nc_check( nf90_inquire_dimension(file_id,Time_id,len=Time_len),&
          myname_,'inquire_dimension Time '//trim(filename) )
      call nc_check( nf90_inquire_dimension(file_id,s_n_id,len=s_n_len),&
          myname_,'inquire_dimension south_north '//trim(filename) )
      call nc_check( nf90_inquire_dimension(file_id,w_e_id,len=w_e_len),&
          myname_,'inquire_dimension west_east '//trim(filename) )
      call nc_check( nf90_inquire_dimension(file_id,b_t_id,len=b_t_len),&
          myname_,'inquire_dimension bottom_top '//trim(filename) )
      call nc_check( nf90_inquire_dimension(file_id,s_n_stag_id,len=s_n_stag_len),&
          myname_,'inquire_dimension south_north_stag '//trim(filename) )
      call nc_check( nf90_inquire_dimension(file_id,w_e_stag_id,len=w_e_stag_len),&
          myname_,'inquire_dimension west_east_stag '//trim(filename) )
      call nc_check( nf90_inquire_dimension(file_id,b_t_stag_id,len=b_t_stag_len),&
          myname_,'inquire_dimension bottom_top_stag '//trim(filename) )
  
      nx=w_e_len
      ny=s_n_len
      nz=b_t_len
      if (nx /= nlon .or. ny /= nlat .or. nz /= nsig) then
       print *,'incorrect grid size in netcdf file'
       print *,'nx,ny,nz,nlon,nlat,nsig',nx,ny,nz,nlon,nlat,nsig
       call stop2(999)
      endif
  
      dim(Time_id)=Time_len
      dim(s_n_id)=s_n_len
      dim(w_e_id)=w_e_len
      dim(b_t_id)=b_t_len
      dim(s_n_stag_id)=s_n_stag_len
      dim(w_e_stag_id)=w_e_stag_len
      dim(b_t_stag_id)=b_t_stag_len
  !
  ! READ PERTURBATION POTENTIAL TEMPERATURE (K)
  !    print *, 'read T ',filename
      call nc_check( nf90_inq_varid(file_id,'T',var_id),&
          myname_,'inq_varid T '//trim(filename) )
  
      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable T '//trim(filename) )
      allocate(dim_id(ndim))
  
      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable T '//trim(filename) )
      allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))
  
      call nc_check( nf90_get_var(file_id,var_id,temp_3d),&
          myname_,'get_var T '//trim(filename) )
      allocate(tsn(dim(dim_id(2)),dim(dim_id(1)),dim(dim_id(3))))
      tsn = reshape(temp_3d,(/dim(dim_id(2)),dim(dim_id(1)),dim(dim_id(3))/),order=(/2,1,3/))
      deallocate(temp_3d)
      deallocate(dim_id)
  
  !  READ MU, MUB, P_TOP  (construct psfc as done in gsi--gives different result compared to PSFC)
  
      call nc_check( nf90_inq_varid(file_id,'P_TOP',var_id),&
          myname_,'inq_varid P_TOP '//trim(filename) )
  
      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable P_TOP '//trim(filename) )
      allocate(dim_id(ndim))
  
      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable P_TOP '//trim(filename) )
      allocate(temp_1d(dim(dim_id(1))))
  
      call nc_check( nf90_get_var(file_id,var_id,temp_1d),&
          myname_,'get_var P_TOP '//trim(filename) )
      allocate(p_top(dim(dim_id(1))))
      do i=1,dim(dim_id(1))
         p_top(i)=temp_1d(i)
      enddo
      deallocate(dim_id)
  
      call nc_check( nf90_inq_varid(file_id,'MUB',var_id),&
          myname_,'inq_varid MUB '//trim(filename) )
  
      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable MUB '//trim(filename) )
      allocate(dim_id(ndim))
  
      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable MUB '//trim(filename) )
      allocate(temp_2d(dim(dim_id(1)),dim(dim_id(2))))
  
      call nc_check( nf90_get_var(file_id,var_id,temp_2d),&
          myname_,'get_var MUB '//trim(filename) )
      deallocate(dim_id)
  
      call nc_check( nf90_inq_varid(file_id,'MU',var_id),&
          myname_,'inq_varid MU '//trim(filename) )
  
      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable MU '//trim(filename) )
      allocate(dim_id(ndim))
  
      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable MU '//trim(filename) )
      allocate(temp_2d2(dim(dim_id(1)),dim(dim_id(2))))
  
      call nc_check( nf90_get_var(file_id,var_id,temp_2d2),&
          myname_,'get_var MU '//trim(filename) )
  
      do j=1,dim(dim_id(2))
         do i=1,dim(dim_id(1))
            temp_2d2(i,j)=temp_2d(i,j)+temp_2d2(i,j)+temp_1d(1)
            gg_ps(j,i)=temp_2d2(i,j)
         enddo
      enddo
      print *,'min/max ps',minval(gg_ps),maxval(gg_ps)
      deallocate(temp_2d,temp_2d2,temp_1d,dim_id)
  
  !
  ! READ U (m/s)
      !print *, 'read U ',filename
      call nc_check( nf90_inq_varid(file_id,'U',var_id),&
          myname_,'inq_varid U '//trim(filename) )
  
      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable U '//trim(filename) )
      allocate(dim_id(ndim))
  
      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable U '//trim(filename) )
      allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))
  
      call nc_check( nf90_get_var(file_id,var_id,temp_3d),&
          myname_,'get_var U '//trim(filename) )
  !
  ! INTERPOLATE TO MASS GRID
      do k=1,dim(dim_id(3))
         do j=1,dim(dim_id(2))
            do i=1,dim(dim_id(1))-1
               gg_u(j,i,k)=.5*(temp_3d(i,j,k)+temp_3d(i+1,j,k))
            enddo
         enddo
      enddo
      deallocate(temp_3d)
      deallocate(dim_id)
  !
  ! READ V (m/s)
      !print *, 'read V ',filename
      call nc_check( nf90_inq_varid(file_id,'V',var_id),&
          myname_,'inq_varid V '//trim(filename) )
  
      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable V '//trim(filename) )
      allocate(dim_id(ndim))
  
      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable V '//trim(filename) )
      allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))
  
      call nc_check( nf90_get_var(file_id,var_id,temp_3d),&
          myname_,'get_var V '//trim(filename) )
  !
  ! INTERPOLATE TO MASS GRID
      do k=1,dim(dim_id(3))
         do j=1,dim(dim_id(2))-1
            do i=1,dim(dim_id(1))
               gg_v(j,i,k)=.5*(temp_3d(i,j,k)+temp_3d(i,j+1,k))
            enddo
         enddo
      enddo
      deallocate(temp_3d)
      deallocate(dim_id)
      print *,'min/max u',minval(gg_u),maxval(gg_u)
      print *,'min/max v',minval(gg_v),maxval(gg_v)
  !
  ! READ QVAPOR (kg/kg)
      !print *, 'read QVAPOR ',filename
      call nc_check( nf90_inq_varid(file_id,'QVAPOR',var_id),&
          myname_,'inq_varid QVAPOR '//trim(filename) )
  
      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable QVAPOR '//trim(filename) )
      allocate(dim_id(ndim))
  
      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable QVAPOR '//trim(filename) )
      allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))
  
      call nc_check( nf90_get_var(file_id,var_id,temp_3d),&
          myname_,'get_var QVAPOR '//trim(filename) )
      gg_rh = reshape(temp_3d,(/dim(dim_id(2)),dim(dim_id(1)),dim(dim_id(3))/),order=(/2,1,3/))
      deallocate(temp_3d)
      deallocate(dim_id,dim)
  
      call nc_check( nf90_close(file_id),&
          myname_,'close '//trim(filename) )
  !
  ! CALCULATE TOTAL POTENTIAL TEMPERATURE (K)
      !print *, 'calculate total temperature ',filename
      do i=1,nx
         do j=1,ny
            do k=1,nz
              tsn(j,i,k)=tsn(j,i,k)+h300
            enddo
         enddo
      enddo   
  !
  ! INTEGRATE {1 + WATER VAPOR} TO CONVERT DRY AIR PRESSURE    
      !print *, 'integrate 1 + q vertically ',filename
      allocate(q_integral(ny,nx))
      allocate(q_integralc4h(ny,nx))
      q_integral(:,:)=one
      q_integralc4h=0.0_r_single
      do i=1,nx
         do j=1,ny
            do k=1,nz
               deltasigma=eta1_ll(k)-eta1_ll(k+1)
               q_integral(j,i)=q_integral(j,i)+deltasigma*gg_rh(j,i,k)
               q_integralc4h(j,i)=q_integralc4h(j,i)+(eta2_ll(k)-eta2_ll(k+1))*gg_rh(j,i,k)
            enddo
         enddo
      enddo
  !
  ! CONVERT WATER VAPOR MIXING RATIO TO SPECIFIC HUMIDITY
      do i=1,nx
         do j=1,ny
            do k=1,nz
               gg_rh(j,i,k)=gg_rh(j,i,k)/(one+gg_rh(j,i,k))
            enddo
         enddo
      enddo
  
  !  obtaining psfc as done in subroutine read_wrf_mass_netcdf_guess
      do i=1,nx
         do j=1,ny
            psfc_this_dry=r0_01*gg_ps(j,i)
            psfc_this=(psfc_this_dry-pt_ll)*q_integral(j,i)+pt_ll+q_integralc4h(j,i)
            gg_ps(j,i)=one_tenth*psfc_this  ! convert from mb to cb
         end do
      end do
  !
  ! CONVERT POTENTIAL TEMPERATURE TO VIRTUAL TEMPERATURE
      !print *, 'convert potential temp to virtual temp ',filename
      allocate(prsl(ny,nx,nz))
      do k=1,nz
         do i=1,nx
            do j=1,ny
               work_prsl  = one_tenth*(aeta1_ll(k)*(r10*gg_ps(j,i)-pt_ll)+&
                                       aeta2_ll(k) + pt_ll)
               prsl(j,i,k)=work_prsl
               work_prslk = (work_prsl/r100)**rd_over_cp_mass
               ! sensible temp from pot temp
               tsn(j,i,k)     = tsn(j,i,k)*work_prslk
               ! virtual temp from sensible temp
               gg_tv(j,i,k) = tsn(j,i,k) * (one+fv*gg_rh(j,i,k))
               ! recompute sensible temp from virtual temp
               tsn(j,i,k)= gg_tv(j,i,k)/(one+fv*max(zero,gg_rh(j,i,k)))
            end do
         end do
      end do
      print *,'min/max tv',minval(gg_tv),maxval(gg_tv)
  
  !
  ! CALCULATE PSEUDO RELATIVE HUMIDITY IF USING RH VARIABLE
      if (.not.q_hyb_ens) then
         allocate(qst(ny,nx,nz))
         ice=.true. 
         iderivative=0
         call genqsat(qst,tsn,prsl,ny,nx,nsig,ice,iderivative)
         do k=1,nz
            do i=1,nx
               do j=1,ny
                  gg_rh(j,i,k)=gg_rh(j,i,k)/qst(j,i,k)
               enddo
            enddo
         enddo
         print *,'min/max rh',minval(gg_rh),maxval(gg_rh)
         deallocate(qst)
      else
         print *,'min/max q',minval(gg_rh),maxval(gg_rh)
      end if
  
  ! DEALLOCATE REMAINING TEMPORARY STORAGE
      deallocate(tsn,prsl,q_integral,p_top)
      
  ! save the global grid results
      write(900+nnn) nx,ny,nz
      write(900+nnn) gg_ps
      write(900+nnn) gg_tv
      write(900+nnn) gg_u
      write(900+nnn) gg_v
      write(900+nnn) gg_rh

    endif ! done netcdf read on root
  
!    g_oz = 0.; g_cwmr = 0.
    if (mype==0) deallocate(gg_u,gg_v,gg_tv,gg_rh,gg_ps)
  
  return       
  end subroutine general_read_wrf_mass_save


  subroutine general_read_wrf_mass_dim_eta(filename)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    general_read_wrf_mass  read arw model ensemble members
  !   prgmmr: Hu               org: GSD                 date: 2018-07-10
  !
  ! abstract: read hybrid vertical coodinate in WRF MASS core
  !
  ! program history log:
  !   2018-07-10  Hu      - add code to use hybrid vertical coodinate in WRF MASS core
  !
  !   input argument list:
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$ end documentation block
  
      use kinds, only: r_kind,r_single,i_kind
  
      implicit none
  !
  ! Declare passed variables
      character(255),intent(in):: filename
  !
  ! Declare local parameters
      real(r_kind),parameter:: r0_01 = 0.01_r_kind
  !
!  variables for NETCDF IO
      real(r_single) pt_regional 
      real(r_single),allocatable::field1(:)
      real(r_single),allocatable::field1a(:)
!
!  variables for NETCDF IO
      character(len=19)  :: DateStr1
      integer(i_kind)            :: dh1
      integer(i_kind) :: ndim1
      integer(i_kind) :: WrfType
      integer(i_kind), dimension(4)  :: start_index, end_index
      character (len= 4) :: staggering=' N/A'
      character (len= 3) :: ordering

      character (len=80), dimension(3)  ::  dimnames
      character (len=80) :: SysDepInfo
      character (len=31) :: rmse_var

      integer :: wrf_real
!
      integer :: Status,Status_next_time,ierr
      integer :: iyear,imonth,iday,ihour,iminute,isecond
      logical :: print_verbose,wrf_mass_hybridcord
!
      integer :: mype, k
!
!
      print_verbose=.true.
      wrf_real=104
      end_index=0
      start_index=0
      wrf_mass_hybridcord=.false.
      mype=0
  !
    if (mype==0) then ! only read data on root proc
      call ext_ncd_ioinit(sysdepinfo,status)

      call ext_ncd_open_for_read( trim(filename), 0, 0, "", dh1, Status)

      call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
      write(*,*) DateStr1
      write(*,*) trim(filename)
      read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') &
                                  iyear,imonth,iday,ihour,iminute,isecond
      write(6,*)' iy,m,d,h,m,s=',iyear,imonth,iday,ihour,iminute,isecond

      rmse_var='T'

      call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering,&
            start_index,end_index, WrfType, ierr    )
      if(print_verbose)then
          write(6,*)' rmse_var = ',trim(rmse_var),' ndim1 = ',ndim1,' dh1 =',dh1
          write(6,*)' WrfType = ',WrfType,'ierr  = ',ierr 
          write(6,*)' ordering = ',trim(ordering),' staggering =',trim(staggering)
          write(6,*)' start_index = ',start_index,' end_index = ',end_index
      end if

      nlon=end_index(1)
      nlat=end_index(2)
      nsig=end_index(3)
      if(print_verbose)write(6,*)'nlon,nlat,nsig=',nlon,nlat,nsig
      allocate(field1(nsig))
      allocate(field1a(nsig+1))

      rmse_var='P_TOP'
      call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering,&
            start_index,end_index, WrfType, ierr    )
      if(print_verbose)then
          write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
          write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr
          write(6,*)' ordering = ',trim(ordering),' staggering =',trim(staggering)
          write(6,*)' start_index = ',start_index,' end_index = ',end_index
      end if
      call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            pt_regional,WRF_REAL,0,0,0,ordering,          &
            staggering, dimnames ,               &
            start_index,end_index,               & !dom
            start_index,end_index,               & !mem
            start_index,end_index,               & !pat
            ierr                                 )
      if(print_verbose)write(6,*)' p_top=',pt_regional
      pt_ll=r0_01*pt_regional

      allocate(aeta1_ll(nsig),eta1_ll(nsig+1))
      allocate(aeta2_ll(nsig),eta2_ll(nsig+1))

       if(wrf_mass_hybridcord) then
          rmse_var='C3H'
          call ext_ncd_get_var_info(dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
          if(print_verbose)then
             write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
             write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  =',ierr   !DEDE
             write(6,*)' ordering = ',trim(ordering),' staggering =',trim(staggering)
             write(6,*)' start_index = ',start_index,' end_index = ',end_index
          end if
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field1,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,                    &
            start_index,end_index,                   & !dom
            start_index,end_index,                   & !mem
            start_index,end_index,                   & !pat
            ierr                                 )
          if(print_verbose)then
             do k=1,nsig
                write(6,*)' k,c3h(k)=',k,field1(k)
             end do
          end if
          rmse_var='C4H'
          call ext_ncd_get_var_info(dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
          if(print_verbose)then
             write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
             write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  =',ierr   !DEDE
             write(6,*)' ordering = ',trim(ordering),' staggering =',trim(staggering)
             write(6,*)' start_index = ',start_index,' end_index = ',end_index
          end if
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field1a,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,                    &
            start_index,end_index,                   & !dom
            start_index,end_index,                   & !mem
            start_index,end_index,                   & !pat
            ierr                                 )
          if(print_verbose)then
             do k=1,nsig
                write(6,*)' k,c4h(k)=',k,field1a(k)
             end do
          end if
          aeta1_ll=field1(1:nsig)                     !c3h
          aeta2_ll=field1a(1:nsig)*r0_01              ! c4h

          rmse_var='C3F'
          call ext_ncd_get_var_info(dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index, WrfType, ierr    )
          if(print_verbose)then
             write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
             write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  =',ierr   !DEDE
             write(6,*)' ordering = ',trim(ordering),' staggering =',trim(staggering)
             write(6,*)' start_index = ',start_index,' end_index = ',end_index
          end if
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field1,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,                    &
            start_index,end_index,                   & !dom
            start_index,end_index,                   & !mem
            start_index,end_index,                   & !pat
            ierr                                 )
          if(print_verbose)then
             do k=1,nsig+1
                write(6,*)' k,c3f(k)=',k,field1(k)
             end do
          end if
          rmse_var='C4F'
          call ext_ncd_get_var_info(dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index, WrfType, ierr    )
          if(print_verbose)then
             write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
             write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  =',ierr   !DEDE
             write(6,*)' ordering = ',trim(ordering),' staggering =',trim(staggering)
             write(6,*)' start_index = ',start_index,' end_index = ',end_index
          end if
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field1a,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,                    &
            start_index,end_index,                   & !dom
            start_index,end_index,                   & !mem
            start_index,end_index,                   & !pat
            ierr                                 )
          if(print_verbose)then
             do k=1,nsig+1
                write(6,*)' k,c4f(k)=',k,field1a(k)
             end do
          end if
          eta1_ll=field1(1:nsig+1)              !c3f
          eta2_ll=field1a(1:nsig+1)*r0_01       !c4f
       else

          rmse_var='ZNU'
          call ext_ncd_get_var_info(dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
          if(print_verbose)then
             write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
             write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  =',ierr   !DEDE
             write(6,*)' ordering = ',trim(ordering),' staggering =',trim(staggering)
             write(6,*)' start_index = ',start_index,' end_index = ',end_index
          end if
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field1,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,                    &
               start_index,end_index,                   & !dom
               start_index,end_index,                   & !mem
               start_index,end_index,                   & !pat
               ierr                                 )
          if(print_verbose)then
             do k=1,nsig
                write(6,*)' k,znu(k)=',k,field1(k)
             end do
          end if
          aeta1_ll=field1
          aeta2_ll=0.0_r_kind

          rmse_var='ZNW'
          call ext_ncd_get_var_info(dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index, WrfType, ierr    )
          if(print_verbose)then
             write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
             write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  =',ierr   !DEDE
             write(6,*)' ordering = ',trim(ordering),' staggering =',trim(staggering)
             write(6,*)' start_index = ',start_index,' end_index = ',end_index
          end if
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field1a,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,                    &
            start_index,end_index,                   & !dom
            start_index,end_index,                   & !mem
            start_index,end_index,                   & !pat
            ierr                                 )
          if(print_verbose)then
             do k=1,nsig+1
                write(6,*)' k,znw(k)=',k,field1a(k)
             end do
          end if
          eta1_ll=field1a
          eta2_ll=0.0_r_kind
       endif

       deallocate(field1,field1a)
       call ext_ncd_ioclose(dh1, Status)
    endif

  end subroutine general_read_wrf_mass_dim_eta

  subroutine cal_ensperts(n_ens,initialtime,fcsthh)

    implicit none
    integer, intent(in) :: n_ens
    integer, intent(in) :: initialtime,fcsthh

    real(r_kind),allocatable,dimension(:,:,:) :: gg_tv_bar,gg_u_bar,gg_v_bar,gg_rh_bar
    real(r_kind),allocatable,dimension(:,:) :: gg_ps_bar
    integer :: ggnx,ggny,ggnz
    integer :: nx,ny,nz
!
    real(r_kind):: bar_norm,sig_norm
!
    character(len=100) :: ensfile,perbfile
    real(r_kind),allocatable :: fld3d(:,:,:),fld2d(:,:)
    integer :: nnn,iunit,iunit_out
!
!
!
    bar_norm = one/float(n_ens)
    sig_norm=sqrt(one/max(one,n_ens-one))
!
    iunit=10
    do nnn=1,n_ens
       write(ensfile,'(a,I3.3)') 'fort.',900+nnn
       open(iunit,file=trim(ensfile),form='unformatted',convert='BIG_ENDIAN')
         write(*,*) 'read in from file ',trim(ensfile)

          read(iunit) nx,ny,nz
          if(nnn==1) then
             ggnx=nx
             ggny=ny
             ggnz=nz
             allocate(gg_u_bar(ny,nx,nz))
             allocate(gg_v_bar(ny,nx,nz))
             allocate(gg_tv_bar(ny,nx,nz))
             allocate(gg_rh_bar(ny,nx,nz))
             allocate(gg_ps_bar(ny,nx))
             gg_u_bar=zero
             gg_v_bar=zero
             gg_tv_bar=zero
             gg_rh_bar=zero
             gg_ps_bar=zero
          endif

          if(nx==ggnx .and. ny==ggny .and. nz==ggnz) then
             allocate(fld2d(ny,nx))
             read(iunit) fld2d      ! ps
             gg_ps_bar=gg_ps_bar+fld2d
             deallocate(fld2d)

             allocate(fld3d(ny,nx,nz))
             read(iunit) fld3d      ! tv
             gg_tv_bar=gg_tv_bar+fld3d
             read(iunit) fld3d      ! u
             gg_u_bar=gg_u_bar+fld3d
             read(iunit) fld3d      ! v
             gg_v_bar=gg_v_bar+fld3d
             read(iunit) fld3d      ! rh
             gg_rh_bar=gg_rh_bar+fld3d
             deallocate(fld3d)
          else
             write(*,*) 'mismatch dimensions'
             stop 123
          endif
       close(iunit)
    enddo !  nnn

! CALCULATE ENSEMBLE MEAN
    gg_ps_bar=gg_ps_bar*bar_norm
    gg_tv_bar=gg_tv_bar*bar_norm
    gg_u_bar=gg_u_bar*bar_norm
    gg_v_bar=gg_v_bar*bar_norm
    gg_rh_bar=gg_rh_bar*bar_norm
!save ensemble mean
    write(perbfile,'(a,I10,a,I3.3)') 'ensmean_',initialtime,'f',fcsthh
    open (iunit,file=trim(perbfile),form='unformatted',convert='BIG_ENDIAN')
       write(iunit) nx,ny,nz
       write(iunit) gg_ps_bar
       write(iunit) gg_tv_bar
       write(iunit) gg_u_bar
       write(iunit) gg_v_bar
       write(iunit) gg_rh_bar  
    close(iunit)

        write(*,*) nx,ny,nz
        write(*,*) maxval(gg_ps_bar),minval(gg_ps_bar) 
        write(*,*) maxval(gg_tv_bar),minval(gg_tv_bar) 
        write(*,*) maxval(gg_u_bar),minval(gg_u_bar) 
        write(*,*) maxval(gg_v_bar),minval(gg_v_bar) 
        write(*,*) maxval(gg_rh_bar),minval(gg_rh_bar) 
 
! CALCULATE ENSEMBLE perturbations 
    iunit=10
    iunit_out=20
    do nnn=1,n_ens
       write(ensfile,'(a,I3.3)') 'fort.',900+nnn
       write(perbfile,'(a,I10,a,I3.3,a,I4.4)') 'enspert_',initialtime,'f',fcsthh,'_mem',nnn
       open(iunit,file=trim(ensfile),form='unformatted',convert='BIG_ENDIAN')
       open(iunit_out,file=trim(perbfile),form='unformatted',convert='BIG_ENDIAN')
         write(*,*) 'save perturbation to  ',trim(perbfile)

          read(iunit) nx,ny,nz
          write(iunit_out) nx,ny,nz

          if(nx==ggnx .and. ny==ggny .and. nz==ggnz) then
             allocate(fld2d(ny,nx))
             read(iunit) fld2d      ! ps
             fld2d=fld2d-gg_ps_bar
             write(iunit_out) real(fld2d)
             deallocate(fld2d)

             allocate(fld3d(ny,nx,nz))
             read(iunit) fld3d      ! tv
             fld3d=fld3d-gg_tv_bar
             write(iunit_out) real(fld3d)

             read(iunit) fld3d      ! u
             fld3d=fld3d-gg_u_bar
             write(iunit_out) real(fld3d)

             read(iunit) fld3d      ! v
             fld3d=fld3d-gg_v_bar
             write(iunit_out) real(fld3d)

             read(iunit) fld3d      ! rh
             fld3d=fld3d-gg_rh_bar
             write(iunit_out) real(fld3d)

             deallocate(fld3d)
          else
             write(*,*) 'mismatch dimensions'
             stop 123
          endif
       close(iunit)
       close(iunit_out)
    enddo !  nnn

    deallocate(gg_u_bar)
    deallocate(gg_v_bar)
    deallocate(gg_tv_bar)
    deallocate(gg_rh_bar)
    deallocate(gg_ps_bar)

  end subroutine cal_ensperts

end module general_read_wrf_mass
