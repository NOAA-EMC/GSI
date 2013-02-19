module gfs_stratosphere
!$$$   module documentation block
!                .      .    .                                       .
! module:    gfs_stratosphere   routines for adding/blending high gfs levels in regional model
!   prgmmr: parrish          org: np22                date: 2012-02-07
!
! abstract: This module contains routines used to blend in and add additional levels from a
!             GFS model forecast valid at the same time as a regional guess.  The GFS and regional
!             model levels are blended together in the stratosphere continuing up to the top
!             of the GFS domain, which is usually much higher and/or has higher resolution compared
!             to typical regional model domains.  The reason for adding the extra detail from the
!             GFS model in the stratosphere is to allow direct utilization of satellite radiance
!             bias correction coefficients derived from the global data assimilation system (GDAS).
!             
! program history log:
!   2012-02-07  parrish, initial documentation
!
! subroutines included:
!   sub init_gfs_stratosphere      - initialize module parameters to default values
!   sub mix_gfs_nmmb_vcoords       - create new vertical coordinate that smoothly blends nmmb with gfs.
!   sub broadcast_gfs_stratosphere_vars - broadcast new vertical vars to all processors
!   sub destroy_nmmb_vcoords       - deallocate arrays
!

! Variable Definitions:
!   def yyyy                       - description of yyyy
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

   use kinds, only: r_double,i_kind,i_long,r_single,r_kind

   implicit none

! set default to private
   private
! set subroutines to public
   public :: init_gfs_stratosphere
   public :: mix_gfs_nmmb_vcoords
   public :: broadcast_gfs_stratosphere_vars
   public :: destroy_nmmb_vcoords
! set passed variables to public
   public :: use_gfs_stratosphere
   public :: nsig_max,nsig_save,k0m,k1m,k0r,k1g,k0rm,k1mp,k0rp
   public :: nsigg,ak5,bk5,nsigm
   public :: deta1_save,aeta1_save,eta1_save
   public :: deta2_save,aeta2_save,eta2_save
   public :: pblend0,pblend1
   public :: blend_rm,blend_gm
   public :: ges_tv_r,ges_q_r,ges_u_r,ges_v_r,ges_tsen_r,ges_oz_r
   public :: ges_tv_r_g,ges_q_r_g,ges_u_r_g,ges_v_r_g,ges_tsen_r_g,ges_oz_r_g
   public :: good_o3mr

   integer(i_kind) nsig_max,k0m,k1m,k0r,k1g,k0rm,k1mp,k0rp
   integer(i_kind) nsig_save,nsigg,nsigm
   real(r_kind) pblend0,pblend1
   real(r_kind),dimension(:),allocatable:: deta1_save,aeta1_save,eta1_save            
   real(r_kind),dimension(:),allocatable:: deta2_save,aeta2_save,eta2_save          
   real(r_kind),dimension(:),allocatable :: ak5,bk5
   real(r_kind),dimension(:),allocatable:: blend_rm,blend_gm
   real(r_kind),dimension(:,:,:,:),allocatable:: ges_tv_r_g,gesq_r_g,ges_u_r_g,ges_v_r_g, &
                                                 ges_tsen_r_g,ges_oz_r_g,ges_q_r_g
   real(r_kind),dimension(:,:,:,:),allocatable:: ges_tv_r  ,gesq_r  ,ges_u_r  ,ges_v_r  , &
                                                 ges_tsen_r  ,ges_oz_r,ges_q_r
   logical use_gfs_stratosphere
   logical good_o3mr
   logical zero_bkbridge

contains

   subroutine init_gfs_stratosphere
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_gfs_stratosphere  initialize constants
!   prgmmr: parrish          org: np22                date: 2012-02-10
!
! abstract: initialize various constants.
!
! program history log:
!   2012-02-10  parrish, initial documentation
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      implicit none

      use_gfs_stratosphere=.false.
      good_o3mr=.false.
      nsig_max=80
      k0m=0
      k1m=0
      k0r=0
      k1g=0
      nsig_save=0
      pblend0=152._r_kind
      pblend1=79._r_kind

   end subroutine init_gfs_stratosphere

   subroutine mix_gfs_nmmb_vcoords(deta1 ,aeta1 ,eta1 ,deta2 ,aeta2 ,eta2 ,pdtop,pt,nsigr, &
                                   deta1m,aeta1m,eta1m,deta2m,aeta2m,eta2m,nsigm_out)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    mix_gfs_nmmb_vcoords  make new vert coord from global and regional
!   prgmmr: parrish          org: np22                date: 2012-02-11
!
! abstract: Combine gfs vertical coordinate defined by ak5,bk5, nsigg with regional coordinate, defined
!            by eta1,eta2,pdtop,pt,nsigr, to create a new vertical coordinate in regional format, defined by
!            eta1m,eta2m,k0m,k1m,k0r,k1g,nsigm.  The new coordinate is designed so that
!            for pressures greater than pblend0, the coordinate is identical to the regional coordinate, and
!            for pressures less than pblend1, the coordinate is identical with the gfs vertical coordinate.
!            In the zone from pblend0 > p > pblend1, the coordinates are blended together, changing
!            smoothly from regional at bottom to global at top of blend zone.  This coordinate is created
!            so that global background fields can be combined smoothly with regional fields in the upper
!            atmosphere so that increased vertical resolution near top of global model can be made
!            available in the regional model for the purpose of allowing direct use of global
!            satellite radiance bias corrections in the regional model.
!
! program history log:
!   2012-02-11  parrish, initial documentation
!   2012-10-11  eliu -  modify to work for wrf_nmm_regional (HWRF) 
!   2013-02-15  parrish - change dimension of eta1, eta2, eta1m, eta2m to correct value.
!
!   input argument list:
!     deta1  - all of these are original nmmb vertical coordinate specifications.
!     aeta1  -
!     eta1   -
!     deta2  -
!     aeta2  -
!     eta2   -
!     pdtop  -
!     pt     -
!     nsigr  -
!
!   output argument list:
!     deta1m -  these are the new gfs/nmmb blended vertical coordinate specifications.
!     aeta1m -
!     eta1m  -
!     deta2m -
!     aeta2m -
!     eta2m  -
!     nsigm_out  -
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use sigio_module, only: sigio_intkind,sigio_head,sigio_srhead
      use constants, only: zero,one_tenth,half,one,ten,r0_01
      use blendmod, only: init_blend,blend_f,blend_df
      use guess_grids, only: nfldsig
      use gridmod, only: lat2,lon2
      implicit none

!     Declare passed variables
      real(r_single),dimension(nsigr)        ,intent(in   ) :: deta1,aeta1,deta2,aeta2
      real(r_single),dimension(nsigr+1)      ,intent(in   ) :: eta1,eta2
      real(r_single)                         ,intent(in   ) :: pdtop,pt
      integer(i_kind)                        ,intent(in   ) :: nsigr
      real(r_single),dimension(nsig_max)     ,intent(  out) :: deta1m,aeta1m,deta2m,aeta2m
      real(r_single),dimension(nsig_max+1)   ,intent(  out) :: eta1m,eta2m
      integer(i_kind)                        ,intent(  out) :: nsigm_out

!     Declare local variables
      real(r_kind) ak_r(nsigr+1),bk_r(nsigr+1),p_r(nsigr+1)
      real(r_kind),dimension(:),allocatable:: p_g,dp_g      ! (nsigg+1)
      real(r_kind) psfc
      integer(i_kind) k
      real(r_kind) dp_r(nsigr+1)
      real(r_kind) pref0,pref1
      real(r_kind) delpmin,delp
      real(r_kind) delp_m(nsig_max),wgt_m(nsig_max)
      integer(i_kind) nref
      real(r_kind),allocatable::pref(:),dpref_r(:),dpref_g(:),dpref_m(:)
      real(r_kind),allocatable:: p_m(:)
      integer(i_kind) iord,ierror,kk,knext,j,kkend
      real(r_kind) delta,gwgt,deltap,psum,sum,adjust,pthis,dummy
      real(r_kind),allocatable,dimension(:)::ak_m,bk_m,akm,bkm
      real(r_kind) ak_gthis,bk_gthis,ak_rthis,bk_rthis
      character(24) filename
      integer(sigio_intkind):: lunges = 11
      integer(i_kind) iret
      type(sigio_head):: sighead
      real(r_kind),parameter::  zero_001=0.001_r_kind
      real(r_kind) pdtop_ll,pt_ll

        real(r_single),allocatable:: plotp(:,:)
        real(r_kind) this_psfc

!     First, obtain gfs vertical coordinate information:
      filename='gfs_sigf03'  
      open(lunges,file=trim(filename),form='unformatted')
      call sigio_srhead(lunges,sighead,iret)
      close(lunges)
         write(6,*) ' input filename=',filename  
         write(6,*) ' sighead%fhour,sighead%idate=',sighead%fhour,sighead%idate
         write(6,*) ' sighead%levs=',sighead%levs
         write(6,*) ' sighead%idvc,sighead%nvcoord=',sighead%idvc,sighead%nvcoord
         write(6,*) ' sighead%idsl=',sighead%idsl
         do k=1,sighead%levs+1
            write(6,*)' k,vcoord=',k,sighead%vcoord(k,:)
         end do
      if(sighead%nvcoord > 2) then
         write(6,*)' MIX_GFS_NMMB_VCOORDS: NOT READY YET FOR ak5,bk5,ck5 vert coordinate'
         call stop2(85)
      endif
      if (allocated(p_m))        deallocate(p_m)
      if (allocated(p_g))        deallocate(p_g)
      if (allocated(dp_g))       deallocate(dp_g)
      if (allocated(pref))       deallocate(pref)
      if (allocated(dpref_r))    deallocate(dpref_r)
      if (allocated(dpref_g))    deallocate(dpref_g)
      if (allocated(dpref_m))    deallocate(dpref_m)
      if (allocated(ak_m))       deallocate(ak_m)
      if (allocated(bk_m))       deallocate(bk_m)
      if (allocated(akm))        deallocate(akm)
      if (allocated(bkm))        deallocate(bkm)
      if (allocated(plotp))      deallocate(plotp)
      if (allocated(blend_rm))   deallocate(blend_rm)
      if (allocated(blend_gm))   deallocate(blend_gm)
      if (allocated(deta1_save)) deallocate(deta1_save)
      if (allocated(aeta1_save)) deallocate(aeta1_save)
      if (allocated(eta1_save))  deallocate(eta1_save)
      if (allocated(deta2_save)) deallocate(deta2_save)
      if (allocated(aeta2_save)) deallocate(aeta2_save)
      if (allocated(eta2_save))  deallocate(eta2_save)
      if (allocated(ak5))        deallocate(ak5)
      if (allocated(bk5))        deallocate(bk5)

      nsigg=sighead%levs
      if(nsigg > nsig_max) then
         write(6,*)' MIX_GFS_NMMB_VCOORDS: nsigg > nsig_max, nsigg,nsig_max=',nsigg,nsig_max
         call stop2(85)
      endif
      allocate(ak5(nsigg+1),bk5(nsigg+1))
      do k = 1,nsigg+1
        ak5(k) = sighead%vcoord(k,1)*zero_001
!  for purpose of this routine, convert to mb
        ak5(k)=ten*ak5(k)
        bk5(k) = sighead%vcoord(k,2)
      end do

!  save original deta1,aeta1, etc. as module public variables for later access.
      nsig_save=nsigr
      allocate(deta1_save(nsig_save),aeta1_save(nsig_save),eta1_save(nsig_save+1))
      allocate(deta2_save(nsig_save),aeta2_save(nsig_save),eta2_save(nsig_save+1))
      deta1_save=deta1
      aeta1_save=aeta1
      eta1_save=eta1
      deta2_save=deta2
      aeta2_save=aeta2
      eta2_save=eta2
!  print out what I think deta1,2 and aeta1,2 might be as function of eta1, eta2
      do k=1,nsigr
         write(6,'(" k,deta1,eta1(k)-eta1(k+1),diff=",i4,2f15.6,e11.3)') &
                       k,deta1(k),eta1(k)-eta1(k+1),abs(deta1(k)-eta1(k)+eta1(k+1))
      end do
      do k=1,nsigr
         write(6,'(" k,deta2,eta2(k)-eta2(k+1),diff=",i4,2f15.6,e11.3)') &
                       k,deta2(k),eta2(k)-eta2(k+1),abs(deta2(k)-eta2(k)+eta2(k+1))
      end do
      do k=1,nsigr
         write(6,'(" k,aeta1,half*(eta1(k)+eta1(k+1)),diff=",i4,2f15.6,e11.3)') &
                       k,aeta1(k),half*(eta1(k)+eta1(k+1)),abs(aeta1(k)-half*(eta1(k)+eta1(k+1)))
      end do
      do k=1,nsigr
         write(6,'(" k,aeta2,half*(eta2(k)+eta2(k+1)),diff=",i4,2f15.6,e11.3)') &
                       k,aeta2(k),half*(eta2(k)+eta2(k+1)),abs(aeta2(k)-half*(eta2(k)+eta2(k+1)))
      end do

!  compute ak_r,bk_r from eta1,eta2,pdtop,pt for icase=1

      pdtop_ll=pdtop*r0_01
      pt_ll=pt*r0_01
      allocate(p_g(nsigg+1),dp_g(nsigg+1))
      psfc=1000._r_kind
      do k=1,nsigr+1
         ak_r(k)=eta1(k)*pdtop_ll+pt_ll-eta2(k)*(pdtop_ll+pt_ll)
         bk_r(k)=eta2(k)
         p_r(k)=eta1(k)*pdtop_ll+eta2(k)*(psfc-pdtop_ll-pt_ll)+pt_ll
      end do
      do k=1,nsigg+1
         p_g(k)=ak5(k)+bk5(k)*psfc
      end do

!  construct dp_r, dp_g

      do k=2,nsigr
         dp_r(k)=half*(p_r(k-1)-p_r(k+1))
      end do
      dp_r(1)=p_r(1)-p_r(2)
      dp_r(nsigr+1)=p_r(nsigr)-p_r(nsigr+1)
      do k=2,nsigg
         dp_g(k)=half*(p_g(k-1)-p_g(k+1))
      end do
      dp_g(1)=p_g(1)-p_g(2)
      dp_g(nsigg+1)=p_g(nsigg)-p_g(nsigg+1)

!  construct reference pressures.
!   first get range.

      pref0=p_r(1)
      k0r=1
      do k=nsigr,1,-1
         if(p_r(k)>=pblend0) then
            k0r=max(1,k-1)
            pref0=p_r(k0r)
            exit
         end if
      end do
 
      pref1=pref0
      k1g=nsigg
      do k=1,nsigg+1
         if(p_g(k) <= pblend1) then
            k1g=min(k+1,nsigg)
            pref1=p_g(k1g)
            exit
         end if
      end do
      write(6,*)' pref0,pref1=',pref0,pref1
      write(6,*)' p_r(k0r),p_g(k1g)=',p_r(k0r),p_g(k1g)
      write(6,*)' pblend0,pblend1=',pblend0,pblend1
      zero_bkbridge=bk_r(k0r)==zero.and.bk5(k1g)==zero
      write(6,*)' zero_bkbridge,k0r,k1g,bk_r(k0r),bk5(k1g)=',zero_bkbridge,k0r,k1g,bk_r(k0r),bk5(k1g)

!  obtain min delp over range pref0,pref1
      delpmin=pref0-pref1
      do k=1,nsigr
         if(p_r(k) <= pref0 .and. p_r(k+1) >= pref1) delpmin=min(delpmin,p_r(k)-p_r(k+1))
      end do
      do k=1,nsigg
         if(p_g(k) <= pref0 .and. p_g(k+1) >= pref1) delpmin=min(delpmin,p_g(k)-p_g(k+1))
      end do
      write(6,*)' pref0-pref1,delpmin=',pref0-pref1,delpmin

!  obtain ref pressure pref

      delp=delpmin/ten
      nref=one+(pref0-pref1)/delp
      delp=(pref0-pref1)/(nref-one)
      write(6,*)' nref,delp=',nref,delp
      allocate(pref(-10:nref+10))
      do k=-10,nref+10
         pref(k)=pref0-(k-one)*delp
         write(6,'(" k,pref(k),pref0,pref1=",i5,3f12.3)') k,pref(k),pref0,pref1
      end do

!  obtain dpref_g, dpref_r
      allocate(dpref_g(-10:nref+10))
      allocate(dpref_r(-10:nref+10))
      allocate(dpref_m(-10:nref+10))
      iord=4
      call init_blend(pblend0,pblend1,iord,ierror)

      do k=-10,nref+10
         if(pref(k) < p_g(nsigg+1)) then
            dpref_g(k)=dp_g(nsigg+1)
         else if(pref(k) >= p_g(1)) then
            dpref_g(k)=dp_g(1)
         else
            do kk=1,nsigg
               if(pref(k) < p_g(kk) .and. pref(k) >= p_g(kk+1)) then
                  delta=(dp_g(kk+1)-dp_g(kk))/(p_g(kk+1)-p_g(kk))
                  dpref_g(k)=dp_g(kk)+delta*(pref(k)-p_g(kk))
                  exit
               end if
            end do
         end if
         if(pref(k) < p_r(nsigr+1)) then
            dpref_r(k)=dp_r(nsigr+1)
         else if(pref(k) >= p_r(1)) then
            dpref_r(k)=dp_r(1)
         else
            do kk=1,nsigr
               if(pref(k) < p_r(kk) .and. pref(k) >= p_r(kk+1)) then
                  delta=(dp_r(kk+1)-dp_r(kk))/(p_r(kk+1)-p_r(kk))
                  dpref_r(k)=dp_r(kk)+delta*(pref(k)-p_r(kk))
                  exit
               end if
            end do
         end if
         call blend_f(pref(k),gwgt)
         dpref_m(k)=gwgt*dpref_g(k)+(one-gwgt)*dpref_r(k)
         write(6,'(" k,pref,dpref_g,dpref_r,dpref_m=",i5,4f12.3)') &
                                 k,pref(k),dpref_g(k),dpref_r(k),dpref_m(k)
      end do

!   do initial integration from p_r(k0r) to p_g(k1g) (pref0 to pref1) to get coordinate bridge from
!     regional below to global above.
      allocate(p_m(nref))
      p_m=-999._r_kind
      p_m(1)=pref0
      knext=-10
      do kk=1,nref+9
         if( p_m(kk) <= pref1 .or. knext >= nref+9 ) exit
         k=knext
         do j=k,nref+9
            if( pref(j) >= p_m(kk) .and. pref(j+1) <= p_m(kk) ) then
               delta=(dpref_m(j+1)-dpref_m(j))/(pref(j+1)-pref(j))
               deltap=dpref_m(j)+delta*(p_m(kk)-pref(j))
               pthis=p_m(kk)-deltap
               p_m(kk+1)=pthis
               kkend=kk+1
            !  write(6,'(" j,kkend,pref0,p_m(kkend-1:kkend),pref1=",2i4,4f9.4)') &
            !            j,kkend,pref0,p_m(kkend-1),p_m(kkend),pref1
               knext=j
               exit
            end if
         end do
      end do

!   compute nsigm
      nsigm=k0r+kkend+nsigg-k1g-1
      k0m=k0r
      write(6,'(" nsigg,nsigr,nsigm=",3i4)')nsigg,nsigr,nsigm
      if(nsigm>=nsig_max) then
         write(6,*)' FAILURE IN MERGE_VCOORDS, NSIGM > NSIG_MAX.  ADJUST NSIG_MAX ACCORDINGLY'
         call stop2(99)
      end if

!  make smooth correction so p_m(kkend) == pref1
      sum=zero
      psum=pref1-p_m(kkend)
      do k=1,kkend-1
         delp_m(k)=p_m(k+1)-p_m(k)
         pthis=p_m(k)+half*delp_m(k)
         call blend_df(pthis,dummy,wgt_m(k))
         sum=sum+delp_m(k)*wgt_m(k)
      end do
      adjust=psum/sum
      do k=1,kkend-1
         p_m(k+1)=p_m(k)+delp_m(k)*(one+adjust*wgt_m(k))
         write(6,'(" j,k,pref0-p_m(k),p_m(k:k+1),p_m(k+1)-pref1=",2i4,4f9.4)') &
                         j,k,pref0-p_m(k),p_m(k),p_m(k+1),p_m(k+1)-pref1
      end do

!  interpolate ak_r, bk_r, ak5, bk5 to blended bridge pressure levels and construct
!    ak_m, bk_m, a blended version of original.

      allocate(ak_m(kkend),bk_m(kkend))
      do k=1,kkend
         pthis=p_m(k)
         do kk=1,nsigg
            if(pthis < p_g(kk) .and. pthis >= p_g(kk+1)) then
               delta=(ak5(kk+1)-ak5(kk))/(p_g(kk+1)-p_g(kk))
               ak_gthis=ak5(kk)+delta*(pthis-p_g(kk))
               delta=(bk5(kk+1)-bk5(kk))/(p_g(kk+1)-p_g(kk))
               bk_gthis=bk5(kk)+delta*(pthis-p_g(kk))
               exit
            end if
         end do
         do kk=1,nsigr
            if(pthis < p_r(kk) .and. pthis >= p_r(kk+1)) then
               delta=(ak_r(kk+1)-ak_r(kk))/(p_r(kk+1)-p_r(kk))
               ak_rthis=ak_r(kk)+delta*(pthis-p_r(kk))
               delta=(bk_r(kk+1)-bk_r(kk))/(p_r(kk+1)-p_r(kk))
               bk_rthis=bk_r(kk)+delta*(pthis-p_r(kk))
               exit
            end if
         end do
         call blend_f(pthis,gwgt)
         ak_m(k)=gwgt*ak_gthis+(one-gwgt)*ak_rthis
         bk_m(k)=gwgt*bk_gthis+(one-gwgt)*bk_rthis
         if(zero_bkbridge) bk_m(k)=zero
         write(6,'(" akgrm,bkgrm=",3f15.5,5x,3f15.5)')ak_gthis,ak_rthis,ak_m(k),bk_gthis,bk_rthis,bk_m(k)
      end do

! create full profile of blended ak, bk

      allocate(akm(nsigm+1),bkm(nsigm+1))
      kk=0
      do k=1,k0r
         kk=kk+1
         akm(kk)=ak_r(k)
         bkm(kk)=bk_r(k)
      end do
      do k=2,kkend-1
         kk=kk+1
         akm(kk)=ak_m(k)
         bkm(kk)=bk_m(k)
      end do
      k1m=kk+1
      do k=k1g,nsigg+1
         kk=kk+1
         akm(kk)=ak5(k)
         bkm(kk)=bk5(k)
      end do
      write(6,'(" k0r,k0m,k1g,k1m,nsigg=",5i4)')k0r,k0m,k1g,k1m,nsigg
      write(6,'(" ak_r(k0r),ak_m(k0m),bk_r,bk_m=",4f15.5)') ak_r(k0r),akm(k0m),bk_r(k0r),bkm(k0m)
      write(6,'(" ak_g(k1g),ak_m(k1m),bk_g,bk_m=",4f15.5)') ak5 (k1g),akm(k1m),bk5 (k1g),bkm(k1m)

!  plot pressure profiles as function of ps, from ps=1100 to ps=500 and see if anything strange appears.

      allocate(plotp(61,nsigm+1))
      do kk=1,61
         this_psfc=500+ten*(kk-one)
         do k=1,nsigm+1
            plotp(kk,k)=akm(k)+bkm(k)*this_psfc
         end do
      end do
      call outgrads1(plotp,61,nsigm+1,'pm')

! final step: derive eta1m, eta2m, deta1m, deta2m, aeta1m, aeta2m, blend_rm, blend_gm

      do k=1,nsigm+1
         eta2m(k)=bkm(k)
         eta1m(k)=(akm(k)-pt_ll+eta2m(k)*(pdtop_ll+pt_ll))/pdtop_ll
      end do

      do k=1,nsigm
         deta1m(k)=eta1m(k)-eta1m(k+1)
         deta2m(k)=eta2m(k)-eta2m(k+1)
      end do
      do k=1,nsigm
         aeta1m(k)=half*(eta1m(k)+eta1m(k+1))
         aeta2m(k)=half*(eta2m(k)+eta2m(k+1))
      end do

      do k=1,k0m
         write(6,'(" k,eta1,eta1m,diff=",i4,2f15.7,e11.3)')k,eta1(k),eta1m(k),eta1m(k)-eta1(k)
      end do
      do k=1,k0m
         write(6,'(" k,eta2,eta2m,diff=",i4,2f15.7,e11.3)')k,eta2(k),eta2m(k),eta2m(k)-eta2(k)
      end do

      do k=k1g,nsigg+1
         write(6,'(" k,km,ak5(k),akm(km),diff=",2i4,2f15.7,e11.3)') k,k-k1g+k1m,ak5(k),akm(k-k1g+k1m),&
                                                                            ak5(k)-akm(k-k1g+k1m)
      end do
      do k=k1g,nsigg+1
         write(6,'(" k,km,bk5(k),bkm(km),diff=",2i4,2f15.7,e11.3)') k,k-k1g+k1m,bk5(k),bkm(k-k1g+k1m),&
                                                                            bk5(k)-bkm(k-k1g+k1m)
      end do
      allocate(blend_rm(nsigm),blend_gm(nsigm))
      do k=1,nsigm
         pthis=aeta1m(k)*pdtop_ll+aeta2m(k)*(psfc-pdtop_ll-pt_ll)+pt_ll
         call blend_f(pthis,blend_gm(k))
         blend_rm(k)=one-blend_gm(k)
         write(6,'(" k,pthis,blend_gm,blend_rm=",i4,f15.7,2f13.7)') k,pthis,blend_gm(k),blend_rm(k)
      end do
      nsigm_out=nsigm

      k0rm=max(k0r-2,1)
      k1mp=min(k1m+2,nsigm)
      k0rp=min(k0r+1,nsig_save)

   end subroutine mix_gfs_nmmb_vcoords

   subroutine broadcast_gfs_stratosphere_vars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    broadcast_gfs_stratosphere_vars
!   prgmmr: parrish          org: np22                date: 2012-02-11
!
! abstract:  Broadcast new vertical coordinate variables to all processors.
!
! program history log:
!   2012-09-13  parrish, initial documentation
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use mpimod, only: mype,mpi_integer4,mpi_rtype,mpi_comm_world,ierror
      implicit none

      call mpi_bcast(nsig_max,1,mpi_integer4,0,mpi_comm_world,ierror)
      call mpi_bcast(nsig_save,1,mpi_integer4,0,mpi_comm_world,ierror)
      call mpi_bcast(k0m,1,mpi_integer4,0,mpi_comm_world,ierror)
      call mpi_bcast(k1m,1,mpi_integer4,0,mpi_comm_world,ierror)
      call mpi_bcast(k0r,1,mpi_integer4,0,mpi_comm_world,ierror)
      call mpi_bcast(k1g,1,mpi_integer4,0,mpi_comm_world,ierror)
      call mpi_bcast(k0rm,1,mpi_integer4,0,mpi_comm_world,ierror)
      call mpi_bcast(k1mp,1,mpi_integer4,0,mpi_comm_world,ierror)
      call mpi_bcast(k0rp,1,mpi_integer4,0,mpi_comm_world,ierror)
      call mpi_bcast(nsigg,1,mpi_integer4,0,mpi_comm_world,ierror)
      call mpi_bcast(nsigm,1,mpi_integer4,0,mpi_comm_world,ierror)
      call mpi_bcast(pblend0,1,mpi_rtype,0,mpi_comm_world,ierror)
      call mpi_bcast(pblend1,1,mpi_rtype,0,mpi_comm_world,ierror)
      if(mype/=0) then
         allocate(deta1_save(nsig_save),aeta1_save(nsig_save),eta1_save(nsig_save+1))
         allocate(deta2_save(nsig_save),aeta2_save(nsig_save),eta2_save(nsig_save+1))
         allocate(blend_rm(nsigm),blend_gm(nsigm))
         allocate(ak5(nsigg+1),bk5(nsigg+1))
      end if
      call mpi_bcast(deta1_save,nsig_save,mpi_rtype,0,mpi_comm_world,ierror)
      call mpi_bcast(aeta1_save,nsig_save,mpi_rtype,0,mpi_comm_world,ierror)
      call mpi_bcast(eta1_save,nsig_save+1,mpi_rtype,0,mpi_comm_world,ierror)
      call mpi_bcast(deta2_save,nsig_save,mpi_rtype,0,mpi_comm_world,ierror)
      call mpi_bcast(aeta2_save,nsig_save,mpi_rtype,0,mpi_comm_world,ierror)
      call mpi_bcast(eta2_save,nsig_save+1,mpi_rtype,0,mpi_comm_world,ierror)
      call mpi_bcast(blend_rm,nsigm,mpi_rtype,0,mpi_comm_world,ierror)
      call mpi_bcast(blend_gm,nsigm,mpi_rtype,0,mpi_comm_world,ierror)
      call mpi_bcast(ak5,nsigg+1,mpi_rtype,0,mpi_comm_world,ierror)
      call mpi_bcast(bk5,nsigg+1,mpi_rtype,0,mpi_comm_world,ierror)

   end subroutine broadcast_gfs_stratosphere_vars

   subroutine destroy_nmmb_vcoords

!     deallocate arrays

      deallocate(deta1_save,aeta1_save,eta1_save)
      deallocate(deta2_save,aeta2_save,eta2_save)
      deallocate(ak5,bk5,blend_rm,blend_gm)
      deallocate(ges_tv_r_g,ges_q_r_g,ges_u_r_g,ges_v_r_g,ges_tsen_r_g,ges_oz_r_g)
      deallocate(ges_tv_r  ,ges_q_r  ,ges_u_r  ,ges_v_r  ,ges_tsen_r  ,ges_oz_r  )
   end subroutine destroy_nmmb_vcoords

end module gfs_stratosphere
