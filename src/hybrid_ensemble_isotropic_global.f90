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
!
! subroutines included:
!   sub init_sf_xy               - initialize spectral localization routine sf_xy
!   sub sf_xy                    - spectral localization routine
!
! Variable Definitions:
!   def spectral_filter          - used to guarantee proper form for zonal spectral coefficients
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

  real(r_kind),allocatable,dimension(:)::spectral_filter

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
  use hybrid_ensemble_parameters,only: s_ens_h,sp_loc,grd_ens
  use general_specmod, only: general_init_spec_vars 
  use constants, only: izero,ione,zero,half,one,two,three,rearth,pi
  use mpimod, only: mype
  implicit none

  integer(i_kind),intent(in   ) :: jcap_in

  integer(i_kind) i,ii,j,l,n,nn,jcap
  real(r_kind),allocatable::g(:),gsave(:),errmax(:)
  real(r_kind) factor
  real(r_kind) rkm(grd_ens%nlat),f(grd_ens%nlat,grd_ens%nlon),f0(grd_ens%nlat,grd_ens%nlon)
  real(r_single) out1(grd_ens%nlon,grd_ens%nlat)
  real(r_single),allocatable::pn0_npole(:)
  real(r_kind) s_ens_h_min
  

!    make sure s_ens_h is within allowable range  ( pi*rearth*.001/jcap_in <= s_ens_h <= 5500 )

  s_ens_h_min=pi*rearth*.001_r_kind/jcap_in
  if(s_ens_h <  s_ens_h_min) then
     if(mype == izero) write(6,*)' s_ens_h = ',s_ens_h,' km--too small, min value = ',s_ens_h_min,' km.'
     if(mype == izero) write(6,*)' s_ens_h reset to min value'
     s_ens_h=s_ens_h_min
  else if(s_ens_h >  5500._r_kind) then
     if(mype == izero) write(6,*)' s_ens_h = ',s_ens_h,' km--too large, max value = 5500 km.'
     if(mype == izero) write(6,*)' s_ens_h reset to max value'
     s_ens_h=5500._r_kind
  end if
    
    
  jcap=nint(1.2_r_kind*pi*rearth*.001_r_kind/s_ens_h)
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

  do i=1,grd_ens%nlat
     f0(i,1)=exp(-half*(rkm(i)/s_ens_h)**2)
  end do
  
  do j=2,grd_ens%nlon
     do i=1,grd_ens%nlat
        f0(i,j)=f0(i,1)
     end do
  end do

  allocate(g(sp_loc%nc),gsave(sp_loc%nc))
  call general_g2s0(grd_ens,sp_loc,g,f0)

  call general_s2g0(grd_ens,sp_loc,g,f)

!    adjust so value at np = 1
  f=f/f(grd_ens%nlat,1)
  call general_g2s0(grd_ens,sp_loc,g,f)
  call general_s2g0(grd_ens,sp_loc,g,f)
  if(mype == izero) write(6,*)' in init_sf_xy, jcap,s_ens_h,max diff(f0-f)=', &
                                            sp_loc%jcap,s_ens_h,maxval(abs(f0-f))

!            correct spectrum by dividing by pn0_npole
  gsave=g

!    obtain pn0_npole
  allocate(pn0_npole(0:sp_loc%jcap))
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

  allocate(spectral_filter(sp_loc%nc))
  ii=izero
  do l=0,sp_loc%jcap
     factor=one
     if(l >  izero) factor=half
     do n=l,sp_loc%jcap
        nn=nn+2_i_kind
        ii=ii+ione
        spectral_filter(ii)=factor*g(2*n+ione)
        ii=ii+ione
        if(l == izero) then
           spectral_filter(ii)=zero
        else
           spectral_filter(ii)=factor*g(2*n+ione)
        end if
     end do
  end do

  deallocate(g,gsave,pn0_npole)

  f=zero
  f(grd_ens%nlat/4,grd_ens%nlon/2)=one
  call sf_xy(f,1)
  do j=1,grd_ens%nlon
     do i=1,grd_ens%nlat
        out1(j,i)=f(i,j)
     end do
  end do
  if(mype == izero) call outgrads1(out1,grd_ens%nlon,grd_ens%nlat,'out1')

end subroutine init_sf_xy

subroutine sf_xy(f,nlevs)
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
!
!   input argument list:
!     f        - input field to be filtered
!     nlevs    -
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
  use hybrid_ensemble_parameters, only: grd_ens,sp_loc
  implicit none

  integer(i_kind),intent(in   ) :: nlevs
  real(r_kind)   ,intent(inout) :: f(grd_ens%nlat*grd_ens%nlon,nlevs)

  real(r_kind) g(sp_loc%nc)
  integer(i_kind) k

  do k=1,nlevs
     call general_s2g0_ad(grd_ens,sp_loc,g,f(:,k))
     g=g*spectral_filter
     call general_s2g0(grd_ens,sp_loc,g,f(:,k))
  end do

end subroutine sf_xy


end module hybrid_ensemble_isotropic_global
