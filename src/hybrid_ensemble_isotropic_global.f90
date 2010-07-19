module hybrid_ensemble_isotropic_global
!$$$   module documentation block
!                .      .    .                                       .
! module:    hybrid_ensemble_isotropic_global
!   prgmmr: parrish          org: np22                date: 2009-10-15
!
! abstract: contains routines for localization of the hybrid ensemble
!            control variable a_en.  this application is for
!            localization with a global model with homogeneous scale in horizontal
!            and vertical scale a function of vertical only.
!
! program history log:
!   2009-10-15  parrish, initial documentation.
!   2010-02-20  parrish, remove modules specmod_special, transform_special, spectral_transforms_special,
!                 as part of streamlining code for dual resolution capability.
!   2010-06-29  parrish, modify so localization length can be different for each vertical level.
!                 to do this, add new variable array s_ens_hv(nsig), which is read in if s_ens_h <=0.
!                 Otherwise, s_ens_hv is set equal to s_ens_h.
!
! subroutines included:
!   sub init_sf_xy               - initialize spectral localization routine sf_xy
!   sub sf_xy                    - spectral localization routine
!
! Variable Definitions:
!   def spectral_filter          - spectral transform of horizontal homogeneous isotropic localization,
!                                    now allowed to be different for each vertical level.
!   def k_index                  - contains actual vertical level index of each level of control
!                                   variable a_en, when it is stored in horizontal slab mode.  required
!                                   so proper level of spectral_filter array is used.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_sf_xy
  public :: sf_xy

  real(r_kind),allocatable,dimension(:,:)  :: spectral_filter
  integer(i_kind),allocatable,dimension(:) :: k_index

contains

subroutine init_sf_xy(jcap_in)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_sf_xy
!   prgmmr: parrish          org: np23                date: 2009-12-17
!
! abstract: initialize horizontal spectral localization sf_xy
!
! program history log:
!   2009-12-17  parrish
!   2010-06-29  parrish, modify so localization length can be different for each vertical level.
!                 to do this, add new variable array s_ens_hv(nsig), which is read in if s_ens_h <=0.
!                 Otherwise, s_ens_hv is set equal to s_ens_h.
!
!   input argument list:
!     jcap_in - maximum spectral truncation allowed
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

  use kinds, only: r_kind,i_kind,r_single
  use hybrid_ensemble_parameters,only: s_ens_h,sp_loc,grd_ens,grd_loc
  use general_specmod, only: general_init_spec_vars 
  use constants, only: izero,ione,zero,half,one,two,three,rearth,pi
  use mpimod, only: mype
  implicit none

  integer(i_kind),intent(in   ) :: jcap_in

  integer(i_kind) i,ii,j,k,l,n,nn,jcap
  real(r_kind),allocatable::g(:),gsave(:),errmax(:)
  real(r_kind) factor
  real(r_kind) rkm(grd_ens%nlat),f(grd_ens%nlat,grd_ens%nlon),f0(grd_ens%nlat,grd_ens%nlon)
  real(r_single) out1(grd_ens%nlon,grd_ens%nlat)
  real(r_single),allocatable::pn0_npole(:)
  real(r_kind) s_ens_h_min
  real(r_single) s_ens_hv4(grd_ens%nsig)
  real(r_kind) s_ens_hv(grd_ens%nsig)
  real(r_kind) hmin,hmax
  character(5) mapname
  logical make_test_maps

  make_test_maps=.false.

!   setup array of horizontal localization lengths s_ens_hv

  if(s_ens_h <= zero) then

!    read in horizontal localization lengths which depend on model vertical level:
!
! ????????????? Daryl, I left this for you, unless you want me to do this.  I understand
! ????????????? you have some code now to read in 3d fields of vertical and horizontal
! ????????????? localization lengths.

!!!!!!!!!!!!!! following for test, linear variation from 300km to 5500km
     hmin=300._r_kind     !  too small I think for jcap_in=62, so will test lower bound
     hmax=5501._r_kind    ! slightly too large to test upper bound 
     do k=1,grd_ens%nsig
        s_ens_hv(k)=hmin+(hmax-hmin)*(k-one)/(grd_ens%nsig-one)
     end do
!!!!!!!!!!!!!! preceding for test, linear variation from 300km to 5500km

  else

!          assign all levels to same value, s_ens_h  (ran with this on 20100702 and reproduced results from
!                                                      rungsi62_hyb_dualres.sh)

     s_ens_hv=s_ens_h

  end if


!    make sure s_ens_hv is within allowable range  ( pi*rearth*.001/jcap_in <= s_ens_hv <= 5500 )

  s_ens_h_min=pi*rearth*.001_r_kind/jcap_in
  do k=1,grd_ens%nsig
     if(s_ens_hv(k) <  s_ens_h_min) then
        if(mype == izero) write(6,*)' s_ens_hv(',k,') = ',s_ens_hv(k),' km--too small, min value = ', &
                                        s_ens_h_min,' km.'
        if(mype == izero) write(6,*)' s_ens_hv(',k,') reset to min value'
        s_ens_hv(k)=s_ens_h_min
     else if(s_ens_hv(k) >  5500._r_kind) then
        if(mype == izero) write(6,*)' s_ens_hv(',k,') = ',s_ens_hv(k),' km--too large, max value = 5500 km.'
        if(mype == izero) write(6,*)' s_ens_hv(',k,') reset to max value'
        s_ens_hv(k)=5500._r_kind
     end if
  end do
    
    
  jcap=nint(1.2_r_kind*pi*rearth*.001_r_kind/minval(s_ens_hv))
  jcap=min(jcap,jcap_in)

!  set up spectral variables for jcap

  call general_init_spec_vars(sp_loc,jcap,jcap,grd_ens%nlat,grd_ens%nlon)

!    the following code is used to compute the desired spectrum to get a 
!     gaussian localization of desired length-scale.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   how spectrum is obtained:
!
!     Correlation matrix:  C = Y*D*Ytrans
!
!       where Y is matrix of spherical harmonics, evaluated on gaussian grid, and D is a diagonal matrix
!
!     To obtain D, exploit fact that for D a function only of total wave-number n, then C is homogeneous
!       and isotropic on the sphere.
!
!     So look at the special case of a test point centered on the north pole.  The correlation function
!       is then only a function of latitude, call it c(phi), where c(pi/2) = 1.
!
!     Now we have C = P*D*Ptrans, where we have reduced the problem to 1 dimension, latitude, and in 
!       spectral space, total wave number n.  P is the zonal component only of Y.
!
!     Next, form the product
!                             C*e1 =P*D*Ptrans*e1,
!
!            where e1 is a vector of all 0, except for 1 at the north pole.
!
!     Then have P*D*Ptrans*e1 = sum(n) p(n,j)*d(n)*p(n,1) = c(j,1)
!
!        where j=1 corresponds to north pole point in this formulation.
!
!     Now if we have available C(j,1), a gaussian of desired length-scale evaluated (note, doesn't have to
!            be gaussian!) on the gaussian grid, then applying the inverse transform subroutine g2s0 to 
!     C yields the product
!
!             Chat(n) = d(n)*p(n,1)
!
!     So finally the desired spectrum is 
!     
!               d(n) = chat(n)/p(n,1)
!
!     To create the full spectral transform of the desired correlation, d(n) is copied to all non-zero 
!      zonal wave numbers on lines of constant total wave number n, multiplied by 1/2 to account for
!      two degrees of freedom for non-zero zonal wave numbers.
!
!     Note that while creating this routine, a bug was discovered in s2g0 routine for evaluation of pole 
!       value.  There was a missing factor of 1/sqrt(2) apparently.
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!                 create reference gaussian centered on north pole with desired length-scale.

!     compute latitudes in km from north pole
  rkm(grd_ens%nlat)=zero
  rkm(1)=two*asin(one)*rearth*.001_r_kind
  do i=1,(grd_ens%nlat-2_i_kind)/2
     rkm(grd_ens%nlat-i)=(asin(one)-asin(sp_loc%slat(i)))*rearth*.001_r_kind
     rkm(ione+i)=(asin(one)+asin(sp_loc%slat(i)))*rearth*.001_r_kind
  end do
  if(mype == izero) write(6,*)' in init_sf_xy, lat,max(dlat)=', &
           rkm(ione+(grd_ens%nlat-2_i_kind)/2), &
          -rkm(grd_ens%nlat-(grd_ens%nlat-2_i_kind)/2)+rkm(ione+(grd_ens%nlat-2_i_kind)/2),' km'

  allocate(spectral_filter(sp_loc%nc,grd_ens%nsig))
  allocate(g(sp_loc%nc),gsave(sp_loc%nc))
  allocate(pn0_npole(0:sp_loc%jcap))
  do k=1,grd_ens%nsig
     do i=1,grd_ens%nlat
        f0(i,1)=exp(-half*(rkm(i)/s_ens_hv(k))**2)
     end do
  
     do j=2,grd_ens%nlon
        do i=1,grd_ens%nlat
           f0(i,j)=f0(i,1)
        end do
     end do

     call general_g2s0(grd_ens,sp_loc,g,f0)

     call general_s2g0(grd_ens,sp_loc,g,f)

!    adjust so value at np = 1
     f=f/f(grd_ens%nlat,1)
     call general_g2s0(grd_ens,sp_loc,g,f)
     call general_s2g0(grd_ens,sp_loc,g,f)
     if(mype == izero) write(6,*)' in init_sf_xy, jcap,s_ens_hv(',k,'), max diff(f0-f)=', &
                                            sp_loc%jcap,s_ens_hv(k),maxval(abs(f0-f))

!            correct spectrum by dividing by pn0_npole
     gsave=g

!    obtain pn0_npole
     do n=0,sp_loc%jcap
        g=zero
        g(2*n+ione)=one
        call general_s2g0(grd_ens,sp_loc,g,f)
        pn0_npole(n)=f(grd_ens%nlat,1)
     end do

     g=zero
     do n=0,sp_loc%jcap
        g(2*n+ione)=gsave(2*n+ione)/pn0_npole(n)
     end do

!    obtain spectral_filter

     ii=izero
     do l=0,sp_loc%jcap
        factor=one
        if(l >  izero) factor=half
        do n=l,sp_loc%jcap
           nn=nn+2_i_kind
           ii=ii+ione
           spectral_filter(ii,k)=factor*g(2*n+ione)
           ii=ii+ione
           if(l == izero) then
              spectral_filter(ii,k)=zero
           else
              spectral_filter(ii,k)=factor*g(2*n+ione)
           end if
        end do
     end do
  end do
  deallocate(g,gsave,pn0_npole)

!  assign array k_index for each processor, based on grd_loc%kbegin_loc,grd_loc%kend_loc

  allocate(k_index(grd_loc%kbegin_loc:grd_loc%kend_alloc))
  k_index=0
  do k=grd_loc%kbegin_loc,grd_loc%kend_loc
     k_index(k)=1+mod(k-1,grd_loc%nsig)
     write(6,*) 'k_index(',k,')=',k_index(k)
  end do

  if(make_test_maps) then
   do k=1,grd_ens%nsig
      if(k>=grd_loc%kbegin_loc.and.k<=grd_loc%kend_loc) then
         f=zero
         f(grd_ens%nlat/2,grd_ens%nlon/2)=one
         call sf_xy(f,k,k)
         do j=1,grd_ens%nlon
            do i=1,grd_ens%nlat
               out1(j,i)=f(i,j)
            end do
         end do
         write(mapname,'("out",i2.2)')1+mod(k-1,grd_ens%nsig)
         call outgrads1(out1,grd_ens%nlon,grd_ens%nlat,mapname)
      end if
   end do
  end if

end subroutine init_sf_xy

subroutine sf_xy(f,k_start,k_end)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    sf_xy       spectral isotropic localization for global domain
!
!   prgrmmr:  parrish        org: np22                              date: 2009-09-30
!
! abstract:  use spherical harmonic transform to do horizontal isotropic localization of hybrid
!             ensemble control variable a_en.
!
! program history log:
!   2009-10-16  parrish  initial documentation
!   2010-03-11  parrish - adjust dimensions for f to allow for nlevs=0
!
!   input argument list:
!     f        - input field to be filtered
!     k_start  - starting horizontal slab index
!     k_end    - ending horizontal slab index    (k_end can be less than k_start, meaning there is
!                                                   no work on this processor)
!                 NOTE: above args allow horizontal localization length to vary in vertical
!
!   output argument list:
!     f        - filtered output
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use hybrid_ensemble_parameters, only: grd_ens,sp_loc,grd_loc
  implicit none

  integer(i_kind),intent(in   ) :: k_start,k_end
  real(r_kind)   ,intent(inout) :: f(grd_ens%nlat*grd_ens%nlon,k_start:max(k_start,k_end))

  real(r_kind) g(sp_loc%nc)
  integer(i_kind) k

  do k=k_start,k_end
     call general_s2g0_ad(grd_ens,sp_loc,g,f(:,k))
     g(:)=g(:)*spectral_filter(:,k_index(k))
     call general_s2g0(grd_ens,sp_loc,g,f(:,k))
  end do

end subroutine sf_xy


end module hybrid_ensemble_isotropic_global
