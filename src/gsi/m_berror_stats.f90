module m_berror_stats
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    module m_berror_stats
!   prgmmr:      j guo <jguo@nasa.gov>
!      org:      NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:      2010-03-24
!
! abstract:  a module of berror_stats input
!
! program history log:
!   2010-03-24  j guo   - added this document block
!   2011-08-01  lueken  - changed F90 to f90 (no machine logic) and fix indentation
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! module interface:

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_berror_stats - a module of berror_stats input
!
! !DESCRIPTION:
!
! !INTERFACE:

   use kinds,          only : i_kind
   use constants,      only: one,zero
   use control_vectors,only: cvars2d,cvars3d
   use mpeu_util,      only: getindex,check_iostat

   implicit none

   private    ! except

!   def usenewgfsberror - use modified gfs berror stats for global and regional.
!                        for global skips extra record
!                        for regional properly defines array sizes, etc.
!   def berror_stats  - reconfigurable filename via NAMELIST/setup/

   public :: usenewgfsberror
   public :: berror_stats,inquire_berror
   ! interfaces to file berror_stats.
   public :: berror_get_dims ! get dimensions, jfunc::createj_func()
   public :: berror_read_bal ! get cross-cov.stats., balmod::prebal()
   public :: berror_read_wgt ! get auto-cov.stats., prewgt()
   public :: berror_set      ! set internal parameters

   ! external interfaces relating to internal procedures.
   interface berror_get_dims; module procedure get_dims; end interface
   interface berror_read_bal; module procedure read_bal; end interface
   interface berror_read_wgt; module procedure read_wgt; end interface
   interface berror_set;      module procedure lset;     end interface

! !REVISION HISTORY:
!       30Jul08 - Jing Guo <guo@gmao.gsfc.nasa.gov>
!               - initial prototype/prolog/code to wrap up all file
!                 "berror_stats" related operations.
!       25Feb10 - Zhu
!               - made changes for generalizing control variables
!               - remove berror_nvars
!       14May13 - Jing Guo <jing.guo@nasa.gov>
!               - added I/O messages to aid run-time error diagnosis.
!       18Dec15 - Rahul.Mahajan <rahul.mahajan@noaa.gov>
!               - replace die calls with check_iostat to clean code
!               - fix haphazard indentation and add return to all sub-routines
!EOP ___________________________________________________________________

   character(len=*),parameter :: myname='m_berror_stats'

   integer(i_kind),parameter :: default_unit_ = 22
   integer(i_kind),parameter :: default_rc_   = 2

   logical,save :: cwcoveqqcov_
   logical usenewgfsberror

   character(len=256),save :: berror_stats = "berror_stats"      ! filename

contains

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: get_dims - get dimensions
!
! !DESCRIPTION:
!
! !INTERFACE:

subroutine get_dims(msig,mlat,mlon,lunit)

   implicit none

   integer(i_kind)         ,intent(  out) :: msig  ! dimension of levels
   integer(i_kind)         ,intent(  out) :: mlat  ! dimension of latitudes
   integer(i_kind),optional,intent(  out) :: mlon  ! dimension of longitudes
   integer(i_kind),optional,intent(in   ) :: lunit ! logical unit [22]

! !REVISION HISTORY:
!       30Jul08 - Jing Guo <guo@gmao.gsfc.nasa.gov>
!               - the main body of the code is extracted from jfunc.f90
!       18Jun10 - todling - turn mlon into optional
!EOP ___________________________________________________________________

   character(len=*),parameter :: myname_=myname//'::get_dims'
   integer(i_kind) :: inerr,mlon_,ier

   ! Read dimension of stats file
   inerr = default_unit_
   if ( present(lunit) ) inerr = lunit
   open(inerr,file=berror_stats,form='unformatted',status='old',iostat=ier)
   call check_iostat(ier,myname_,'open('//trim(berror_stats)//')')
   rewind inerr
   read(inerr,iostat=ier) msig,mlat,mlon_
   call check_iostat(ier,myname_,'read header')
   close(inerr,iostat=ier)
   call check_iostat(ier,myname_,'close('//trim(berror_stats)//')')
   if ( present(mlon) ) mlon = mlon_

   return
end subroutine get_dims

subroutine inquire_berror(lunit,mype)

   use kinds,    only: r_single

   implicit none

   integer(i_kind),intent(in   ) :: lunit ! logical unit [22]
   integer(i_kind),intent(in   ) :: mype

   character(len=*),parameter :: myname_=myname//'::inquire_berror'
   integer(i_kind) :: inerr,msig,mlat,mlon_,ier,errtot,i
   real(r_single),dimension(:),allocatable::  clat_avn,sigma_avn

   ! Read dimension of stats file
   inerr = lunit
   open(inerr,file=berror_stats,form='unformatted',status='old',iostat=ier)
   call check_iostat(ier,myname_,'open('//trim(berror_stats)//')')
   rewind inerr
   read(inerr,iostat=ier) msig,mlat,mlon_
   call check_iostat(ier,myname_,'read header')
   errtot=ier

   errtot=0
   allocate ( clat_avn(mlat) )
   allocate ( sigma_avn(1:msig) )
   read(inerr,iostat=ier)clat_avn,sigma_avn
!  Checking to see if sigma_avn fits required format if so newgfsberror file.
   do i=1,msig-1
     if(sigma_avn(i) < sigma_avn(i+1) .or. sigma_avn(i) < zero .or. sigma_avn(i) > one)then
         errtot=1
     end if
   end do
   deallocate(clat_avn,sigma_avn)
   if(errtot /= 0)then
     usenewgfsberror=.false.
     if(mype == 0)write(6,*) 'usenewgfsberror = .false. old format file '
   else
     usenewgfsberror=.true.
     if(mype == 0)write(6,*) 'usenewgfsberror = .true. new format file '
   end if
   close(inerr,iostat=ier)
   call check_iostat(ier,myname_,'close('//trim(berror_stats)//')')

   return
end subroutine inquire_berror

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: lset - set (logical) parameter options internal to B
!
! !DESCRIPTION:
!
! !INTERFACE:

subroutine lset(opt,value)

   implicit none

   character(len=*),intent(in) :: opt
   logical(i_kind), intent(in) :: value

! !REVISION HISTORY:
!       04Feb14 - todling - set logical parameters internal to this package
!EOP ___________________________________________________________________

   character(len=*),parameter :: myname_=myname//'::lset'
   logical :: found

   found=.false.
   if ( trim(opt)=='cwcoveqqcov' ) then
      cwcoveqqcov_=value
      found=.true.
   endif
   if ( .not. found ) then
      write(6,*) myname_,'(PREBAL):  ***ERROR*** cannot find:', trim(opt)
      call stop2(999)
   endif 

   return
end subroutine lset

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: read_bal - get cross-corr. coefficients
!
! !DESCRIPTION:
!
! !INTERFACE:

subroutine read_bal(agvin,bvin,wgvin,pputin,fut2ps,mype,lunit)

   use kinds,    only: r_single
   use constants,only: zero
   use gridmod,  only: nlat,nsig

   implicit none

   logical                                 ,intent(in   ) :: fut2ps
   real(r_single),dimension(nlat,nsig,nsig),intent(  out) :: agvin
   real(r_single),dimension(nlat,nsig)     ,intent(  out) :: bvin,wgvin
   real(r_single),dimension(nlat,nsig)     ,intent(  out) :: pputin
   integer(i_kind)                         ,intent(in   ) :: mype  ! "my" processor ID
   integer(i_kind),optional                ,intent(in   ) :: lunit ! an alternative unit

! !REVISION HISTORY:
!       30Jul08 - Jing Guo <guo@gmao.gsfc.nasa.gov>
!               - the main body of code for input is extracted from
!                 prebal() in balmod.f90.
!       25Feb10 - Zhu 
!               - change the structure of background error file
!               - read in agvin,wgvin,bvin only
!      09Oct12 - Gu  add fut2ps to project unbalanced temp to surface pressure in static B modeling
!EOP ___________________________________________________________________

   character(len=*),parameter :: myname_=myname//'::read_bal'

   integer(i_kind) :: nsigstat,nlatstat
   integer(i_kind) :: inerr,ier

   ! Open background error statistics file
   inerr = default_unit_
   if ( present(lunit) ) inerr = lunit
   open(inerr,file=berror_stats,form='unformatted',status='old',iostat=ier)
   call check_iostat(ier,myname_,'open('//trim(berror_stats)//')')

   ! Read header.  Ensure that vertical resolution is consistent
   ! with that specified via the user namelist

   rewind inerr
   read(inerr,iostat=ier) nsigstat,nlatstat
   call check_iostat(ier,myname_,'read('//trim(berror_stats)//') for (nsigstat,nlatstat)')

!  dummy read to skip lats,sigma
   if(usenewgfsberror)read(inerr)
   if ( mype==0 ) then
      if ( nsig/=nsigstat .or. nlat/=nlatstat ) then
         write(6,*) myname_,'(PREBAL):  ***ERROR*** resolution of ', &
            '"',trim(berror_stats),'"', &
            'incompatiable with guess'
         write(6,*) myname_,'(PREBAL):  ***ERROR*** nsigstat,nlatstat=', &
            nsigstat,nlatstat
         write(6,*) myname_,'(PREBAL):  ***ERROR*** expects nsig,nlat=', &
            nsig,nlat
         call stop2(default_rc_)
      endif

      write(6,*) myname_,'(PREBAL):  get balance variables', &
         '"',trim(berror_stats),'".  ', &
         'mype,nsigstat,nlatstat =', &
         mype,nsigstat,nlatstat
   endif

   ! Read background error file to get balance variables
   if ( fut2ps )then
      read(inerr,iostat=ier) agvin,bvin,wgvin,pputin
   else
      read(inerr,iostat=ier) agvin,bvin,wgvin
      pputin=zero
   endif
   call check_iostat(ier,myname_,'read('//trim(berror_stats)//') for (agvin,bvin,wgvin)')
   close(inerr,iostat=ier)
   call check_iostat(ier,myname_,'close('//trim(berror_stats)//')')

   return
end subroutine read_bal

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: read_wgt - read auto-corr. coeffs.
!
! !DESCRIPTION:
!
! !INTERFACE:

subroutine read_wgt(corz,corp,hwll,hwllp,vz,corsst,hsst,varq,qoption,varcw,cwoption,mype,lunit)

   use kinds,only : r_single,r_kind
   use gridmod,only : nlat,nlon,nsig
   use radiance_mod, only: n_clouds_fwd,cloud_names_fwd

   implicit none

   real(r_single),dimension(:,:,:),    intent(inout) :: corz 
   real(r_single),dimension(:,:)  ,    intent(inout) :: corp  

   real(r_single),dimension(:,:,:),    intent(inout) :: hwll
   real(r_single),dimension(:,:)  ,    intent(inout) :: hwllp
   real(r_single),dimension(:,:,:),    intent(inout) :: vz

   real(r_single),dimension(nlat,nlon),intent(out  ) :: corsst
   real(r_single),dimension(nlat,nlon),intent(out  ) :: hsst

   real(r_kind),  dimension(:,:)      ,intent(out  ) :: varq
   real(r_kind),  dimension(:,:)      ,intent(out  ) :: varcw

   integer(i_kind)                    ,intent(in   ) :: qoption
   integer(i_kind)                    ,intent(in   ) :: cwoption
   integer(i_kind)                    ,intent(in   ) :: mype  ! "my" processor ID
   integer(i_kind),optional           ,intent(in   ) :: lunit ! an alternative unit

! !REVISION HISTORY:
!       30Jul08 - Jing Guo <guo@gmao.gsfc.nasa.gov>
!               - the main body of the code for input is extracted from
!                 prewgt() in prewgt.f90.
!       25Feb10 - Zhu - change the structure of background error file
!                     - make changes for generalizing control variables
!                     - move varq here from prewgt
!       28May10 - Todling - Obtain variable id's on the fly (add getindex) 
!                         - simpler logics to associate cv w/ berrors
!       14Jun10 - Todling - Allow any 3d berror not in file to be templated 
!       15Dec12 - Zhu - Add varcw and cwoption
!       03Feb14 - Todling - varq & qoption in arg list (remove dep on jfunc)
!       05Feb14 - Todling - Allow for overwrite of cw with q cov
!       07Jun14 - Zhu - set up new error variance and corr. lengths 
!                       of cw for allsky radiance
!       09Sept15 - Zhu - use centralized cloud_names_fwd and n_clouds_fwd to add 
!                        flexibility for all-sky radiance assimilation
!EOP ___________________________________________________________________

   character(len=*),parameter :: myname_=myname//'::read_wgt'

   real(r_single),dimension(nlat,nsig,nsig):: agvin
   real(r_single),dimension(nlat,nsig) :: wgvin,bvin

   integer(i_kind) :: i,n,k,iq,icw,ivar,ic
   integer(i_kind) :: inerr,istat,ier
   integer(i_kind) :: nsigstat,nlatstat,mlon_
   integer(i_kind) :: isig
   real(r_kind) :: corq2x
   character(len=5) :: var
   logical,allocatable,dimension(:) :: found3d
   logical,allocatable,dimension(:) :: found2d

!  real(r_single),allocatable,dimension(:)  :: clat,sigma
   real(r_single),allocatable,dimension(:,:):: hwllin
   real(r_single),allocatable,dimension(:,:):: corzin
   real(r_single),allocatable,dimension(:,:):: corq2
   real(r_single),allocatable,dimension(:,:):: vscalesin

   ! Open background error statistics file
   inerr = default_unit_
   if ( present(lunit) ) inerr = lunit
   open(inerr,file=berror_stats,form='unformatted',status='old',iostat=ier)
   call check_iostat(ier,myname_,'open('//trim(berror_stats)//')')

   ! Read header.  Ensure that vertical resolution is consistent
   ! with that specified via the user namelist

   rewind inerr
   read(inerr,iostat=ier)nsigstat,nlatstat,mlon_
   call check_iostat(ier,myname_,'read('//trim(berror_stats)//') for (nsigstat,nlatstat)')
   if(usenewgfsberror)read(inerr,iostat=ier)

   if ( mype==0 ) then
      if ( nsigstat/=nsig .or. nlatstat/=nlat ) then
         write(6,*)'PREBAL: **ERROR** resolution of berror_stats incompatiable with GSI'
         write(6,*)'PREBAL:  berror nsigstat,nlatstat=', nsigstat,nlatstat, &
            ' -vs- GSI nsig,nlat=',nsig,nlat
         call stop2(101)
      endif

      write(6,*) myname_,'(PREWGT):  read error amplitudes ', &
         '"',trim(berror_stats),'".  ', &
         'mype,nsigstat,nlatstat =', &
         mype,nsigstat,nlatstat
   endif
   read(inerr,iostat=ier) agvin,bvin,wgvin
   call check_iostat(ier,myname_,'read('//trim(berror_stats)//') for (agvin,bvin,wgvin)')

   ! Read amplitudes
   allocate(found3d(size(cvars3d)),found2d(size(cvars2d)))
   found3d=.false.
   found2d=.false.
   readloop: do
      read(inerr,iostat=istat) var, isig
      if ( istat/=0 ) exit

      allocate(corzin(nlat,isig))
      if ( var=='q' .or. var=='cw' ) allocate(corq2(nlat,isig))
      allocate(hwllin(nlat,isig))
      if ( isig>1 ) allocate(vscalesin(nlat,isig))

      if ( var/='sst' ) then
         if ( var=='q' .or. var=='Q' .or. (var=='cw' .and. cwoption==2) ) then
            read(inerr,iostat=ier) corzin,corq2
            call check_iostat(ier,myname_,'read('//trim(berror_stats)//') for (corzin,corq2)')
         else
            read(inerr,iostat=ier) corzin
            call check_iostat(ier,myname_,'read('//trim(berror_stats)//') for (corzin)')
         endif
         read(inerr,iostat=ier) hwllin
         call check_iostat(ier,myname_,'read('//trim(berror_stats)//') for (hwllin)')
         if ( isig>1 ) then
            read(inerr,iostat=ier) vscalesin
            call check_iostat(ier,myname_,'read('//trim(berror_stats)//') for (vscalein)')
         endif
      else
         read(inerr,iostat=ier) corsst
         call check_iostat(ier,myname_,'read('//trim(berror_stats)//') for (corsst)')
         read(inerr,iostat=ier) hsst
         call check_iostat(ier,myname_,'read('//trim(berror_stats)//') for (hsst)')
      endif

      if ( isig>1 ) then
         n=getindex(cvars3d,var)
         if ( n>0 ) then
            found3d(n)=.true.
            do k=1,isig
               do i=1,nlat
                  corz(i,k,n)=corzin(i,k)
                  vz(k,i,n)=vscalesin(i,k)
               enddo
            enddo
            if ( var=='q' .and. qoption==2 ) then
               do k=1,isig
                  do i=1,nlat
                     corq2x=corq2(i,k)
                     varq(i,k)=min(max(corq2x,0.00015_r_kind),one)
                  enddo
               enddo
               do k=1,isig
                  do i=1,nlat
                     corz(i,k,n)=one
                  enddo
               enddo
            endif
            if ( var=='cw' .and. cwoption==2 ) then
               do k=1,isig
                  do i=1,nlat
                     corq2x=corq2(i,k)
                     varcw(i,k)=max(corq2x,zero)
                  enddo
               enddo
               do k=1,isig
                  do i=1,nlat
                     corz(i,k,n)=one
                  enddo
               enddo
            endif
            do k=1,isig
               do i=1,nlat
                  hwll(i,k,n)=hwllin(i,k)
               enddo
            enddo
         endif ! n>0
      endif ! end of isig

      if ( isig==1 ) then
         n = getindex(cvars2d,var)
         if ( n>0 .and. var/='sst' ) then
            found2d(n)=.true.
            do i=1,nlat
               corp(i,n)=corzin(i,1)
               hwllp(i,n)=hwllin(i,1)
            enddo
         endif ! n>0
      endif ! isig=1

      deallocate(corzin,hwllin)
      if ( isig>1 ) deallocate(vscalesin)
      if ( var=='q' .or. var=='cw' ) deallocate(corq2)
   enddo readloop 
   close(inerr)

   ! corz, hwll & vz for undefined 3d variables
   do n=1,size(cvars3d)
      if ( .not.found3d(n) ) then
         if ( n>0 ) then
            if ( cvars3d(n)=='oz' ) then
               call setcoroz_(corz(:,:,n),mype)
            else
               call setcorchem_(cvars3d(n),corz(:,:,n),ier)
               if ( ier/=0 ) cycle ! if this happens, code will crash later
            endif
            call sethwlloz_(hwll(:,:,n),mype)
            call setvscalesoz_(vz(:,:,n))
         endif
         if ( mype==0 ) write(6,*) myname_, ': WARNING, using general Berror template for ', cvars3d(n)
      endif
   enddo

!  if so, overwrite cw-cov with q-cov
   iq=-1;icw=-1
   do n=1,size(cvars3d)
      if(trim(cvars3d(n))=='q' ) iq =n
      if(trim(cvars3d(n))=='cw') icw=n
   enddo
   if (cwcoveqqcov_) then
      if(iq>0.and.icw>0) then
        hwll(:,:,icw)=hwll(:,:,iq)
        vz  (:,:,icw)=vz  (:,:,iq)
      end if
   end if
   if (cwoption==1 .or. cwoption==3) then
      if (iq>0.and.icw>0) then
         do k=1,nsig
            do i=1,nlat
               corz(i,k,icw)=one
            end do
         end do
         hwll(:,:,icw)=0.5_r_kind*hwll(:,:,iq)
         vz  (:,:,icw)=0.5_r_kind*vz  (:,:,iq)
      end if 

      if (n_clouds_fwd>0 .and. icw<=0) then
         do n=1,size(cvars3d)
            do ic=1,n_clouds_fwd
               if(trim(cvars3d(n))==trim(cloud_names_fwd(ic))) then
                  ivar=n
                  do k=1,nsig
                     do i=1,nlat
                        corz(i,k,ivar)=one
                     end do
                  end do
                  hwll(:,:,ivar)=0.5_r_kind*hwll(:,:,iq)
                  vz  (:,:,ivar)=0.5_r_kind*vz  (:,:,iq)
                  exit
               end if   
            end do
         end do
      end if
   endif

   ! need simliar general template for undefined 2d variables ...

   deallocate(found3d,found2d)

  return
end subroutine read_wgt

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: setcoroz_ - a modeled corr.coeffs. of ozone
!
! !DESCRIPTION:
!
! !INTERFACE:

subroutine setcoroz_(coroz,mype)

   use kinds,    only: r_single,r_kind
   use constants,only: zero,rozcon,one
   use mpimod,   only: npe,mpi_rtype,mpi_sum,mpi_comm_world

   use gridmod,only: nlat,nsig
   use gridmod,only: lon1,lat1

   use guess_grids,only: ntguessig
   use guess_grids,only: ges_prsi ! interface pressures (kPa)

   use gsi_bundlemod,   only: gsi_bundlegetpointer
   use gsi_metguess_mod,only: gsi_metguess_bundle

   implicit none

   real(r_single),dimension(nlat,nsig),intent(  out) :: coroz ! of ozone
   integer(i_kind)                    ,intent(in   ) :: mype  ! ID of this processor

! !REVISION HISTORY:
!       31Jul08 - Jing Guo <guo@gmao.gsfc.nasa.gov>
!               - adopted from PREWGT of previous version
!       2013-10-19 oz guess field in metguess now 
!EOP ___________________________________________________________________

   character(len=*),parameter :: myname_=myname//'::setcoroz_'

   real(r_kind),parameter :: r25 = one/25.0_r_kind
   real(r_kind),dimension(:,:,:),pointer :: ges_oz=>NULL()

   real(r_kind),dimension(nsig+1,npe) :: work_oz,work_oz1
   real(r_kind),dimension(nsig) :: ozmz
   real(r_kind) :: asum,bsum

   integer(i_kind) :: mlat,msig
   integer(i_kind) :: i,j,k,n,mm1
   integer(i_kind) :: ierror

   call gsi_bundlegetpointer(gsi_metguess_bundle(ntguessig),'oz',ges_oz,ierror)
   if ( ierror/=0 ) return ! nothing to do

   ! sanity check
   if ( mype==0 ) write(6,*) myname_,'(PREWGT): mype = ',mype

   mlat=size(coroz,1)
   msig=size(coroz,2)
   if ( mlat/=nlat .or. msig/=nsig ) then
      write(6,*) myname_,'(PREWGT): shape mismatching on PE ',mype
      write(6,*) myname_,'(PREWGT): shape(coroz) = ',shape(coroz)
      write(6,*) myname_,'(PREWGT): while expecting nlat = ',nlat
      write(6,*) myname_,'(PREWGT): while expecting nsig = ',nsig
      call stop2(default_rc_)
   endif

   ! The first part is taken from read_guess().

   ! Calculate global means for ozone
   ! Calculate sums for ozone to estimate variance.
   mm1=mype+1
   work_oz = zero
   do k=1,nsig
      do j=2,lon1+1
         do i=2,lat1+1
            work_oz(k,mm1) = work_oz(k,mm1) + ges_oz(i,j,k)* &
               rozcon*(ges_prsi(i,j,k,ntguessig)-ges_prsi(i,j,k+1,ntguessig))
         enddo
      enddo
   enddo
   work_oz(nsig+1,mm1)=float(lon1*lat1)

   call mpi_allreduce(work_oz,work_oz1,(nsig+1)*npe,mpi_rtype,mpi_sum,&
      mpi_comm_world,ierror)
   if ( ierror/=0 ) then
      write(6,*) myname_,'(PREWGT): MPI_allreduce() error on PE ',mype
      call stop2(ierror)
   endif

   ! All it does above, through mm1 plus mpi_allreduce() to work_oz1[],
   ! seems no more than a mpi_allgatherv() to me.  The "reduce" part is
   ! actually done below ...

   bsum=zero
   do n=1,npe
      bsum=bsum+work_oz1(nsig+1,n)
   enddo
   do k=1,nsig
      ozmz(k)=zero
      asum=zero
      do n=1,npe
         asum=asum+work_oz1(k,n)
      enddo
      if ( bsum>zero ) ozmz(k)=asum/bsum
   enddo

   ! now this part is taken from prewgt().

   ! load variances onto subdomains
   do k=1,nsig
      coroz(:,k) = max(ozmz(k),0.0002_r_kind)*r25
   enddo

   return
end subroutine setcoroz_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: sethwlloz_ - a modeled hwll of ozone
!
! !DESCRIPTION:
!
! !INTERFACE:

subroutine sethwlloz_(hwlloz,mype)

   use kinds,    only: r_single,r_kind
   use mpimod,   only: levs_id
   use gridmod,  only: nnnn1o,nsig,nlon,nlat
   use constants,only: two,three,pi,rearth_equator
   
   implicit none

   real(r_single),dimension(nlat,nsig),intent(  out) :: hwlloz
   integer(i_kind)                    ,intent(in   ) :: mype ! ID of this processor

! !REVISION HISTORY:
!       31Jul08 - Jing Guo <guo@gmao.gsfc.nasa.gov>
!               - initial prototype/prolog/code
!EOP ___________________________________________________________________

   character(len=*),parameter :: myname_=myname//'::sethwlloz_'

   real(r_kind),parameter :: r400=400._r_kind
   real(r_kind),parameter :: r800=800._r_kind
   real(r_kind),parameter :: r40000=40000._r_kind

   integer(i_kind) :: k,k1
   real(r_kind) :: fact
   real(r_kind) :: s2u
    
   if ( mype==0 ) write(6,*) myname_,'(PREWGT): mype = ',mype

   s2u=(two*pi*rearth_equator)/nlon
   do k=1,nnnn1o
      k1=levs_id(k)
      if ( k1>0 ) then
      write(6,*) myname_,'(PREWGT): mype = ',mype, k1
         if ( k1<=nsig*3/4 ) then
           ! fact=1./hwl
           fact=r40000/(r400*nlon)
         else
           fact=r40000/(nlon*(r800-r400*(nsig-k1)/(nsig-nsig*3/4)))
         endif
         fact=fact*three
         hwlloz(:,k1)=s2u/fact
      endif
   enddo

   if ( mype==0 ) write(6,*) myname_,'(PREWGT): mype = ',mype, 'finish sethwlloz_'

   return
end subroutine sethwlloz_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: setvscalesoz_ - a modeled vscales for ozone
!
! !DESCRIPTION:
!
! !INTERFACE:

subroutine setvscalesoz_(vscalesoz)

   use kinds,  only: r_single,r_kind
   use gridmod,only: nlat,nsig

   implicit none

   real(r_single),dimension(nsig,nlat),intent(  out) :: vscalesoz

! !REVISION HISTORY:
!       31Jul08 - Jing Guo <guo@gmao.gsfc.nasa.gov>
!               - initial prototype/prolog/code
!EOP ___________________________________________________________________

   character(len=*),parameter :: myname_=myname//'::setvscalesoz_'

   real(r_kind),parameter :: eight_tenths = 0.8_r_kind

   ! a fixed value is used.
   vscalesoz(:,:)=eight_tenths

   return
end subroutine setvscalesoz_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: setcorchem_ - a modeled corr.coeffs. of chemistry
!
! !DESCRIPTION:
!
! !INTERFACE:

subroutine setcorchem_(cname,corchem,rc)

   use kinds,    only: r_single,r_kind
   use mpimod,   only: mype
   use constants,only: zero,one
   use mpimod,   only: npe,mpi_rtype,mpi_sum,mpi_comm_world

   use gridmod,only: nlat,nsig
   use gridmod,only: lon1,lat1

   use guess_grids,only: ntguessig
   use guess_grids,only: ges_prsi ! interface pressures (kPa)

   use gsi_chemguess_mod,only: gsi_chemguess_bundle
   use gsi_bundlemod,    only: gsi_bundlegetpointer

   implicit none

   character(len=*)                   ,intent(in   ) :: cname   ! constituent name
   real(r_single),dimension(nlat,nsig),intent(  out) :: corchem ! constituent correlations
   integer(i_kind)                    ,intent(  out) :: rc      ! return error code

! !REVISION HISTORY:
!    15Jul20010 - Todling - created from Guo's OZ routine
!
!EOP ___________________________________________________________________

   character(len=*),parameter :: myname_=myname//'::setcorchem_'

   real(r_kind),parameter:: r25 = one/25.0_r_kind

   real(r_kind),dimension(nsig+1,npe) :: work_chem,work_chem1
   real(r_kind),dimension(nsig) :: chemz
   real(r_kind) :: asum,bsum

   integer(i_kind) :: mlat,msig
   integer(i_kind) :: i,j,k,n,iptr,mm1
   integer(i_kind) :: ierror

   rc=0

   ! sanity check
   if ( mype==0 ) write(6,*) myname_,'(PREWGT): mype = ',mype

   ! Get information for how to use CO2
   iptr=-1
   if ( size(gsi_chemguess_bundle)>0 ) then ! check to see if bundle's allocated
       call gsi_bundlegetpointer(gsi_chemguess_bundle(1),cname,iptr,ierror)
      if ( ierror/=0 ) then
         rc=-2  ! field not found
         return 
      endif
   else
      rc=-1     ! chem not allocated
      return
   endif

   mlat=size(corchem,1)
   msig=size(corchem,2)
   if ( mlat/=nlat .or. msig/=nsig ) then
      write(6,*) myname_,'(PREWGT): shape mismatching on PE ',mype
      write(6,*) myname_,'(PREWGT): shape(corchem',trim(cname),') = ',shape(corchem)
      write(6,*) myname_,'(PREWGT): while expecting nlat = ',nlat
      write(6,*) myname_,'(PREWGT): while expecting nsig = ',nsig
      call stop2(default_rc_)
   endif

   ! The first part is taken from read_guess().

   ! Calculate global means for constituent
   ! Calculate sums for constituent to estimate variance.
   mm1=mype+1
   work_chem = zero
   do k=1,nsig
      do j=2,lon1+1
         do i=2,lat1+1
            work_chem(k,mm1) = work_chem(k,mm1) + gsi_chemguess_bundle(ntguessig)%r3(iptr)%q(i,j,k)* &
               (ges_prsi(i,j,k,ntguessig)-ges_prsi(i,j,k+1,ntguessig))
            !_RT not sure yet how to handle scaling factor (rozcon) in general
            !_RT            rozcon*(ges_prsi(i,j,k,ntguessig)-ges_prsi(i,j,k+1,ntguessig))
         enddo
      enddo
   enddo
   work_chem(nsig+1,mm1)=float(lon1*lat1)
  
   call mpi_allreduce(work_chem,work_chem1,(nsig+1)*npe,mpi_rtype,mpi_sum,&
        mpi_comm_world,ierror)
   if ( ierror/=0 ) then
      write(6,*) myname_,'(PREWGT): MPI_allreduce() error on PE ',mype
      call stop2(ierror)
   endif

   ! All it does above, through mm1 plus mpi_allreduce() to work_chem1[],
   ! seems no more than a mpi_allgatherv() to me.  The "reduce" part is
   ! actually done below ...

   bsum=zero
   do n=1,npe
      bsum=bsum+work_chem1(nsig+1,n)
   enddo
   do k=1,nsig
      chemz(k)=zero
      asum=zero
      do n=1,npe
         asum=asum+work_chem1(k,n)
      enddo
      if ( bsum>zero ) chemz(k)=asum/bsum
   enddo

   ! now this part is taken from prewgt().

   ! load variances onto subdomains
   do k=1,nsig
      corchem(:,k) = max(chemz(k),0.0002_r_kind)*r25
   enddo

   if ( mype==0 ) write(6,*) myname_, ': Defined general B cov for: ', trim(cname)

   return
end subroutine setcorchem_

end module m_berror_stats
