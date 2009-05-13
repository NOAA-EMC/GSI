module raflib
!$$$   module documentation block
!                .      .    .                                       .
! module:    raflib          contains anisotropic filter routines
!   prgmmr: parrish          org: np22                date: 2005-11-30
!
! abstract: contains routines for initializing and applying anisotropic
!             recursive filter.
!
! program history log:
!   2005-11-30  parrish
!   2005-11-30  parrish -- replace blended triad routine gettri4 with
!                          newer version provided by Jim Purser.
!
! subroutines included:
!   sub adjoint_check4           -
!   sub raf4_ad                  -
!   sub raf_sm4_ad               -
!   sub rad_sm24_ad              -
!   sub alpha_beta4              -
!   sub alpha_betaa4             -
!   sub count_strings            -
!   sub gethex                   -
!   sub indexxi4                 -
!   sub indexxi8                 -
!   sub init_raf4                -
!   sub normalize_raf4           -
!   sub one_color4               -
!   sub one_color24              -
!   sub raf4                     -
!   sub raf_sm4                  -
!   sub sort_strings4            -
!   sub string_assemble4         -
!   sub string_label             -
!   sub my_gatherv8              -
!   sub my_scatterv8             -
!   sub what_color_is            -
!
! Variable Type Definition:
!   def filter_cons              - structure variable containing information
!                                  for anisotropic filter--created by init_raf4
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

use kinds,only: r_double,r_quad,r_single,i_byte,i_long,i_llong,i_short
use mpimod,only: mpi_comm_world,mpi_integer,mpi_integer2,mpi_integer4, &
              mpi_integer8,mpi_max,mpi_min,mpi_real4,mpi_real8,mpi_real16,mpi_sum
use constants, only: zero_quad, one_quad,zero_single
implicit none

!  declare type structure for recursive anisotropic filter constants

         type filter_cons

           sequence
           integer(i_long) ngauss
           integer(i_long) npass
           integer(i_long) ifilt_ord
           integer(i_long) npointsmaxall
           integer(i_long) npointsmax
           integer(i_long) npoints_send
           integer(i_long) npoints_recv
           integer(i_long) nstrings
           integer(i_long),pointer::istart(:)
           integer(i_long),pointer::ib(:)
           integer(i_long),pointer::nrecv(:)
           integer(i_long),pointer::ndrecv(:)
           integer(i_long),pointer::nsend(:)
           integer(i_long),pointer::ndsend(:)
           real(r_single),pointer::lnf(:,:,:,:)
           real(r_single),pointer::bnf(:,:,:)
           real(r_single),pointer::amp(:,:,:,:)
           integer(i_short),pointer::ia(:)
           integer(i_short),pointer::ja(:)
           integer(i_short),pointer::ka(:)

         end type filter_cons

contains

subroutine adjoint_check4(filter,ngauss, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             mype, npes)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    adjoint_check4
!
!   prgrmmr:
!
! abstract:  test that filter is symmetric
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     ids, ide, jds, jde, kds, kde      - domain indices
!     ips, ipe, jps, jpe, kps, kpe      - patch indices
!     ims, ime, jms, jme, kms, kme      - memory indices
!     mype                              - mpi task id 
!     npes                              -
!     ngauss                            -
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:
!
!$$$ end documentation block



  INTEGER(i_long), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices

  INTEGER(i_long), INTENT(IN) :: &
     mype, npes,ngauss

  TYPE(filter_cons) filter(7)            ! structure defining recursive filter

  real(r_single) xvec( ngauss,ims:ime, jms:jme, kms:kme )
  real(r_single) yvec( ngauss,ims:ime, jms:jme, kms:kme )
  real(r_single) zvec( ngauss,ims:ime, jms:jme, kms:kme )

  integer(i_long) i,igauss,j,k
  real(r_quad) yty,xtz
  real(r_quad) yty0(npes),xtz0(npes)
  integer(i_long) ierror

  xvec=zero_single
  yvec=zero_single
  zvec=zero_single
  call random_number(xvec)
  yvec=xvec
  call raf_sm4(yvec,filter,ngauss, & 
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             mype, npes)
  zvec=yvec
  call raf_sm4_ad(zvec,filter,ngauss, & 
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             mype, npes)

  yty=zero_quad
  xtz=zero_quad
  do k=kps,kpe
   do j=jps,jpe
    do i=ips,ipe
     do igauss=1,ngauss
      yty=yty+one_quad*yvec(igauss,i,j,k)**2
      xtz=xtz+one_quad*xvec(igauss,i,j,k)*zvec(igauss,i,j,k)
     end do
    end do
   end do
  end do
  call mpi_gather(yty,1,mpi_real16,yty0,1,mpi_real16,0,mpi_comm_world,ierror)
  call mpi_gather(xtz,1,mpi_real16,xtz0,1,mpi_real16,0,mpi_comm_world,ierror)
  if(mype.eq.0) then
   yty=zero_quad
   xtz=zero_quad
   do i=1,npes
    yty=yty+yty0(i)
    xtz=xtz+xtz0(i)
   end do
   write(6,*)' adjoint check for raf,ad_raf, yty,xtz=',yty,xtz
  end if

end subroutine adjoint_check4

SUBROUTINE raf4_ad(g,filter,ngauss, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             mype, npes)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    raf4_ad
!
!   prgrmmr:
!
! abstract:  2nd half of recursive anisotropic self-adjoint filter (full-strings version)
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     ids, ide, jds, jde, kds, kde      - domain indices
!     ips, ipe, jps, jpe, kps, kpe      - patch indices
!     ims, ime, jms, jme, kms, kme      - memory indices
!     ngauss                            -
!     mype                              - mpi task id
!     npes                              -
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:
!
!$$$ end documentation block

  implicit none

  INTEGER(i_long), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices

  INTEGER(i_long), INTENT(IN) :: &
     ngauss,mype, npes

  real(r_single), DIMENSION( ngauss,ims:ime, jms:jme, kms:kme ), INTENT(INOUT) :: &
            g                      !  input--field to be filtered, output--filtered field

  TYPE(filter_cons) filter(7)            ! structure defining recursive filter

  integer(i_long) i,icolor,igauss,ipass,j,k


  if(filter(1)%npass.gt.0) then
   do ipass=filter(1)%npass,1,-1

    do icolor=1,7

     if(filter(icolor)%npointsmaxall.gt.0) &
         call one_color4(g,filter(icolor),ngauss,ipass,filter(1)%ifilt_ord, &
             filter(icolor)%nstrings,filter(icolor)%istart, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             mype, npes)
   
    end do

   end do
  end if

!   finally multiply by amp

  do k=kps,kpe
   do j=jps,jpe
    do i=ips,ipe
     do igauss=1,ngauss
      g(igauss,i,j,k)=filter(1)%amp(igauss,i,j,k)*g(igauss,i,j,k)
     end do
    end do
   end do
  end do

end subroutine raf4_ad

SUBROUTINE raf_sm4_ad(g,filter,ngauss, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             mype, npes)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    raf_sm4_ad
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     ids, ide, jds, jde, kds, kde      - domain indices
!     ips, ipe, jps, jpe, kps, kpe      - patch indices
!     ims, ime, jms, jme, kms, kme      - memory indices
!     mype                              - mpi task id
!     npes                              -
!     ngauss                            -
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:
!
!$$$ end documentation block

!  2nd half of recursive anisotropic self-adjoint filter (full-strings version)
!        (no amplitude factor--used only for local weighted averages)

  implicit none

  INTEGER(i_long), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices

  INTEGER(i_long), INTENT(IN) :: &
     mype, npes,ngauss

  real(r_single), DIMENSION( ngauss,ims:ime, jms:jme, kms:kme ), INTENT(INOUT) :: &
            g                      !  input--field to be filtered, output--filtered field

  TYPE(filter_cons) filter(7)            ! structure defining recursive filter

  integer(i_long) icolor,ipass

  if(filter(1)%npass.gt.0) then
   do ipass=filter(1)%npass,1,-1

    do icolor=1,7

     if(filter(icolor)%npointsmaxall.gt.0) &
         call one_color4(g,filter(icolor),ngauss,ipass,filter(1)%ifilt_ord, &
             filter(icolor)%nstrings,filter(icolor)%istart, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             mype, npes)
   
    end do

   end do
  end if

end subroutine raf_sm4_ad

SUBROUTINE rad_sm24_ad(g,filter,ngauss, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             mype, npes)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    rad_sm24_ad
!
!   prgrmmr:
!
! abstract:     do two fields at same time, each real(4)
!  2nd half of recursive anisotropic self-adjoint filter (full-strings version)
!        (no amplitude factor--used only for local weighted averages)
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     ids, ide, jds, jde, kds, kde      - domain indices
!     ips, ipe, jps, jpe, kps, kpe      - patch indices
!     ims, ime, jms, jme, kms, kme      - memory indices
!     mype                              - mpi task id
!     npes                              - 
!     ngauss                            -
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:
!
!$$$

  implicit none

  INTEGER(i_long), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices

  INTEGER(i_long), INTENT(IN) :: &
     mype, npes,ngauss

  real(r_single), DIMENSION(2,ngauss,ims:ime, jms:jme, kms:kme ), INTENT(INOUT) :: &
            g                      !  input--field to be filtered, output--filtered field

  TYPE(filter_cons) filter(7)            ! structure defining recursive filter

  integer(i_long) icolor,ipass

  if(filter(1)%npass.gt.0) then
   do ipass=filter(1)%npass,1,-1

    do icolor=1,7

     if(filter(icolor)%npointsmaxall.gt.0) &
         call one_color24(g,filter(icolor),ngauss,ipass,filter(1)%ifilt_ord, &
             filter(icolor)%nstrings,filter(icolor)%istart, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             mype, npes)
   
    end do

   end do
  end if

end subroutine rad_sm24_ad

subroutine alpha_beta4(info_string,aspect_full,rgauss,lnf,bnf,igauss,ngauss, &
                     istart_out,npoints_mype,binomial,npass,ifilt_ord, &
                     lenbar,lenmax,lenmin,npoints1, &
                     ids, ide, jds, jde, kds, kde, &                          ! domain indices
                     ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                     ims, ime, jms, jme, kms, kme, &                          ! memory indices
                     mype, npes,nvars)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    alpha_beta4
!
!   prgrmmr:
!
! abstract:   compute recursion constants alpha and beta along unbroken strings
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     ids, ide, jds, jde, kds, kde      - domain indices
!     ips, ipe, jps, jpe, kps, kpe      - patch indices
!     ims, ime, jms, jme, kms, kme      - memory indices
!     mype                              - mpi task id
!     npes                              -
!     nvars                             -
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:
!
!$$$ end documentation block

  IMPLICIT NONE

  INTEGER(i_long), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices
  INTEGER(i_long), INTENT(IN) :: &
     mype, npes,nvars

  INTEGER(i_long), INTENT(IN) :: npoints_mype,npass,ifilt_ord

  REAL(r_double), DIMENSION( 10, 10 ), INTENT(IN) :: &
            binomial

  INTEGER(i_short), DIMENSION( 8, npoints_mype ), INTENT(IN) ::  &
            info_string      !      1---- distance from origin to current point
                             !      2,3,4-- origin coordinates
                             !      5,6,7,8-- jumpx,jumpy,jumpz,ivar for this string
  real(r_single), DIMENSION( npoints_mype ) , INTENT(IN) :: &
            aspect_full
  integer(i_long) igauss,ngauss
  real(r_double) rgauss
  real(r_single) lnf(ifilt_ord,npoints_mype,npass,ngauss),bnf(npoints_mype,npass,ngauss)
  integer(i_long) istart_out(*)
  integer(i_long)  lenmax(nvars),lenmin(nvars),npoints1(nvars) !  diagnostic output--to look at string
  real(r_double) lenbar(nvars)

  integer(i_long) i,iend,ipass,istart,ivar
  integer(i_long) nstrings

  nstrings=0

  istart=1
  if(npoints_mype.gt.1) then
   do i=2,npoints_mype
    if(info_string(1,i).ne.info_string(1,i-1)+1.or. &
           info_string(2,i).ne.info_string(2,i-1).or. &
               info_string(3,i).ne.info_string(3,i-1).or. &
                   info_string(4,i).ne.info_string(4,i-1).or. &
                       info_string(5,i).ne.info_string(5,i-1).or. &
                           info_string(6,i).ne.info_string(6,i-1).or. &
                               info_string(7,i).ne.info_string(7,i-1).or. &
                                   info_string(8,i).ne.info_string(8,i-1)) then
     iend=i-1
        if(igauss.eq.1) then
          ivar=info_string(8,iend)
          lenbar(ivar)=lenbar(ivar)+(iend-istart+1)
          lenmax(ivar)=max(iend-istart+1,lenmax(ivar))
          lenmin(ivar)=min(iend-istart+1,lenmin(ivar))
          if(iend.eq.istart) npoints1(ivar)=npoints1(ivar)+1
        end if
     nstrings=nstrings+1
     istart_out(nstrings)=istart
     istart_out(nstrings+1)=iend+1

     do ipass=1,npass
      call alpha_betaa4(aspect_full(istart),rgauss,iend-istart+1,binomial(ipass,npass), &
                     lnf(1,istart,ipass,igauss),bnf(istart,ipass,igauss),ifilt_ord,nstrings)
     end do
     
     istart=iend+1
    end if
   end do
  end if
  iend=npoints_mype
       if(igauss.eq.1) then
          ivar=info_string(8,iend)
          lenbar(ivar)=lenbar(ivar)+(iend-istart+1)
          lenmax(ivar)=max(iend-istart+1,lenmax(ivar))
          lenmin(ivar)=min(iend-istart+1,lenmin(ivar))
          if(iend.eq.istart) npoints1(ivar)=npoints1(ivar)+1
       end if
  nstrings=nstrings+1
  istart_out(nstrings)=istart
  istart_out(nstrings+1)=iend+1
  do ipass=1,npass

   call alpha_betaa4(aspect_full(istart),rgauss,iend-istart+1,binomial(ipass,npass), &
                  lnf(1,istart,ipass,igauss),bnf(istart,ipass,igauss),ifilt_ord,nstrings)
  end do

end subroutine alpha_beta4

subroutine alpha_betaa4(aspect,rgauss,ng,binomial,lnf,bnf,m,nstrings)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    alpha_betaa4
!
!   prgrmmr:
!
! abstract:  compute various constants for Purser 1-d high-order filter
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     aspect   - correlation scale (squared, i think), grid units
!     rgauss   - multiplier of aspect, for multiple gaussians--used for fat-tail filter
!     ng       - length of string
!     binomial - weighting factors (perhaps not needed with high-order filter)
!     m        - filter order
!
!   output argument list:
!     lnf,bnf  - filter parameters
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  IMPLICIT NONE

  INTEGER(i_long), INTENT(IN) :: ng,m
         integer(i_long) nstrings

  REAL(r_double), INTENT(IN) :: binomial

  real(r_single), DIMENSION( ng ), INTENT(IN) :: aspect
  real(r_double) rgauss
  real(r_single) lnf(m,ng),bnf(ng)

  real(r_double) sig(ng),snu(ng)
  real(r_double) lnf8(m,ng),bnf8(ng)
  integer(i_long) i,j

  do i=1,ng
   sig(i)=sqrt(rgauss*aspect(i)*binomial)
   snu(i)=1._r_double
  end do
  call coefrf(sig,snu,ng,m,bnf8,lnf8)
  do i=1,ng
   bnf(i)=bnf8(i)
   do j=1,m
    lnf(j,i)=lnf8(j,i)
   end do
  end do

end subroutine alpha_betaa4

subroutine count_strings(info_string,nstrings,nstrings_var,nvars,npoints_mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    count_strings
!
!   prgrmmr:
!
! abstract:  compute recursion constants alpha and beta along unbroken strings
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     info_string            1       - distance from origin to current point
!                            2,3,4   - origin coordinates
!                            5,6,7,8 - jumpx,jumpy,jumpz,ivar for this string
!     nvars                          -
!     npoints_mype                   -
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block


  IMPLICIT NONE

  INTEGER(i_long), INTENT(IN) :: npoints_mype,nvars

  INTEGER(i_short), DIMENSION( 8, npoints_mype ), INTENT(IN) ::  &
            info_string      !      1---- distance from origin to current point
                             !      2,3,4-- origin coordinates
                             !      5,6,7,8-- jumpx,jumpy,jumpz,ivar for this string
  integer(i_long) nstrings,nstrings_var(nvars)

  integer(i_long) i,iend,istart,ivar

  nstrings=0

  istart=1
  if(npoints_mype.gt.1) then
   do i=2,npoints_mype
    if(info_string(1,i).ne.info_string(1,i-1)+1.or. &
           info_string(2,i).ne.info_string(2,i-1).or. &
               info_string(3,i).ne.info_string(3,i-1).or. &
                   info_string(4,i).ne.info_string(4,i-1).or. &
                       info_string(5,i).ne.info_string(5,i-1).or. &
                           info_string(6,i).ne.info_string(6,i-1).or. &
                               info_string(7,i).ne.info_string(7,i-1).or. &
                                   info_string(8,i).ne.info_string(8,i-1)) then
     iend=i-1
     nstrings=nstrings+1
     ivar=info_string(8,iend)
     nstrings_var(ivar)=nstrings_var(ivar)+1
     istart=iend+1
    end if
   end do
   iend=npoints_mype
   nstrings=nstrings+1
   ivar=info_string(8,iend)
   nstrings_var(ivar)=nstrings_var(ivar)+1
  end if

return
end subroutine count_strings


SUBROUTINE GETHEX(UTARGET,LGUESS,LHEXAD,LUI,WHEXAD,KT)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    GETHEX
!
!   prgrmmr:     Purser 1997
!
! abstract:
!    Apply implicit lattice transformations until the target tensor UTARGET
!  can be represented as a positive combination of the components of a
! "canonical hexad", with coefficients which may each be interpreted as the
! grid-unit "spread" component in the associated generalized grid direction.
! The target tensor is given in basic grid units. If "LBASIS" is
! a triplet of integer 3-vectors, collectively of determinant = +4, and
! with the all Nth component of these vectors either odd or even, the convex
! hull of the six vectors, {LBASIS and -LBASIS}, forms an octahedron. The
! midpoints of the 12 edges form the 2 diametricaly opposite pairs of sets,
! each consisting of 6 distinct integer vectors, LHEXAD, which are the
! hexad of generalized grid steps along which the application of appropriately
! weighted smoothers will result in the target spread. The requisite weights,
! WHEXAD, are the spreads in the individual hexad directions when expressed
! in the natural grid-units of each of these 6 directions. These weights are
! such that the matrix LU multiplied by WHEXAD gives UTARGET, where the Jth
! column of LU is 6-vector representation of the degenerate tensor describing
! a spread in the direction of hexad-J of one unit of the grid spacing in this
! direction. matrix LUI is the transpose of the inverse of LU.
!
! HOW IT WORKS:
!   For the given target tensor, the hexad and weights are defined iteratively.
! A valid "guess" for LHEXAD, LU, LUI, must first be provided. If these are
! not provided by the user, just set the flag LGUESS to 0, and the routine
! will provide feasible default values; otherwise set LGUESS > 0.
!      First let us suppose the given hexad and accompanying LU, LUI are valid.
! Then we may compute the corresponding weights:
!                WHEXAD = (LUI-transpose)*UTARGET
! If the hexad really IS valid, these weights will all be non-negative and
! the task is done. But if some weight is negative, then the hexad is invalid
! and an alternative valid hexad must be sought. In this situation, the
! algorithm first determines the MOST negative weight and its associated
! grid direction and, keeping the other five directions the same, replaces
! the offending one with the unique (up to sign change) alternative such
! that the NEW hexad are also the midpoints of some grid-octahedron formed from
! an integer-vector basis of determinant = 4 and of the required form.
! To preserve algorithmic symmetry, the enumeration of the new grid directions
! undergoes a permutation. Since only one column of LU changes, the work needed
! to update LUI is less that the work needed to compute the new inverse from
! scratch (cf the "simplex method" of linear programming). We can also exploit
! the special property of this problem that the determinant of LU changes from
! +1 to -1 when the offending column is replaced by its alternative. The
! change in the enumeration of the vectors LHEXAD and of the columns of LU and
! LUI can be regarded as a way to preserve the pattern of implied geometrical
! relationships among these vectors, so that the algorithm is relatively simple.
!    By repeating this step, the algorithm eventually homes in on the
! uniquely valid hexad.
!
!   The "cuboctahedron" associated with the default guess basis is shown in 3
!   orthogonal views below, hexad vector shown thus, (); basis vectors, [].
!
!
!      (6)--------(3)
!       |          |  \         (In each view, nearest facet is speckled)
!       |   [1]    |    \[3]
!       |          |      \          <===== top view
!       |          |        \
!      (-4)-------(5)--------(1)                    south view
!         \::::::::|          |                    //
!           \::::::|          |                   //
!         [-3]\::::|     [2]  |                  //         east view
!               \::|          |                 //              ||
!                 (2)--------(-6)              //               ||
!                                             //                \/
!      (-4)------ (2)                        //      (2)------- (5)
!       |          |::\                     //        |          |  \
!       |          |::::\[2]               //         |          |    \
!       |          |::::::\         <=====//          |          |      \
!       |          |::::::::\                         |          |        \
!      (-1)------(-3)--------(-6)                   (-6)--------(1)--------(3)
!         \        |          |                         \::::::::|          |
!           \      |          |                           \::::::|          |
!             \    |          |                             \::::|          |
!               \  |          |                               \::|          |
!                 (-5)-------(4)                                (4)-------(-2)
!
!
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     UTARGET    - 6-vector comprising components of the target aspect tensor
!     LGUESS     - Code to tell whether input LHEXAD are feasible (LGUESS.NE.0)
!     LHEXAD     - 6 integer basis vectors giving the canonical grid steps
!     LUI        - 6 6-vectors dual to those of LU: [LUI]^t*[LU]=[I]
!
!   output argument list:
!     LHEXAD     - 6 integer basis vectors giving the canonical grid steps
!     LUI        - 6 6-vectors dual to those of LU: [LUI]^t*[LU]=[I]
!     WHEXAD     - 6 real "spread" components in generalized grid-step units
!     KT         - The number of iterations it required to find the valid hexad
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  implicit none

  real(r_double) utarget(6),whexad(6)
  integer(i_long) lguess
  integer(i_long) lhexad(3,6),lui(6,6)
  integer(i_long) kt

  integer(i_long) ihexad(3,6),ilui(6,6)        ! defaults
  integer(i_long) newlhex(3,2:6),newlui(6,2:6),lui1(6)
  real(r_double) wt(6)
  integer(i_long) kp(6,6),ksg(6,6)
  real(r_double) bcmin,bcmins,u,w1
  integer(i_long) i,it,j,k,k1or3,kfirst,ksign,l
  
      DATA KP /1,2,6,3,4,5, 2,1,4,6,5,3 & ! Permutation code.
              ,3,4,2,5,6,1, 4,3,6,2,1,5 & ! This line is previous + 2 (modulo 6)
              ,5,6,4,1,2,3, 6,5,2,4,3,1/  ! This line is previous + 2 (modulo 6)
      DATA KSG/1, 1,  1, 1, -1,-1,              1, 1, -1, 1,  1,-1 &
              ,1, 1,  1, 1, -1,-1,              1, 1, -1, 1,  1,-1 &
              ,1, 1,  1, 1, -1,-1,              1, 1, -1, 1,  1,-1/
      DATA IHEXAD/ 1, 0, 0,  0,-1, 1 &
                 , 0, 1, 0,  1, 0,-1 &
                 , 0, 0, 1, -1, 1, 0/

      DATA ILUI/1, 0, 0,  0, 1, 1,    0, 0, 0, -1, 0, 0 &
               ,0, 1, 0,  1, 0, 1,    0, 0, 0,  0,-1, 0 &
               ,0, 0, 1,  1, 1, 0,    0, 0, 0,  0, 0,-1/


  bcmins=-epsilon(utarget) !  a criterion slightly < 0 avoids roundoff worries


      IF(LGUESS.EQ.0)THEN
       DO J=1,6
        DO I=1,3
         LHEXAD(I,J)=IHEXAD(I,J)
        ENDDO
        DO I=1,6
         LUI(I,J)=ILUI(I,J)
        ENDDO
       ENDDO
      ENDIF

! Use initial estimate of hexad to compute implied weights directly.
! (Subsequent updates of these weights are done perturbatively to save time).
      DO I=1,6
        WHEXAD(I)=0._r_double
      ENDDO
      DO I=1,6
        U=UTARGET(I)
        DO J=1,6
         WHEXAD(J)=WHEXAD(J)+LUI(I,J)*U
        ENDDO
      ENDDO
      K1OR3=1              !  At iteration 1, WHEXAD(1) and (2) might be < 0.

      DO IT=1,4000          !  this should be ample
       KT=IT               !  report back how many iterations were needed
       L=0
       BCMIN=BCMINS
       DO K=K1OR3,6
        IF(WHEXAD(K).LT.BCMIN)THEN
         L=K
         BCMIN=WHEXAD(L)
        ENDIF
       ENDDO
       IF(L.EQ.0)RETURN ! If there are no negetive weights to offend, return
!  Permute the columns of LHEXAD and of LUI according to the permutation
!  scheme encoded by KP(J,L):
       DO J=2,6    ! J=1 corresponds to the NEW direction. (Treat separately).
        K=KP(J,L)
        KSIGN=KSG(J,L)
        WT(J)=WHEXAD(K)
        DO I=1,3
         NEWLHEX(I,J)=KSIGN*LHEXAD(I,K)
        ENDDO
        DO I=1,6
         NEWLUI(I,J)=LUI(I,K)
        ENDDO
       ENDDO

!  Set a temporary vector to what becomes the new column J=1 of LUI
       DO I=1,6
        LUI1(I)=-LUI(I,L)
       ENDDO

!  Replace the first hexad member, J=1, in this new arrangement:
       DO I=1,3
        LHEXAD(I,1)=NEWLHEX(I,4)+NEWLHEX(I,5) ! [  = NEWLHEX(I,3)-NEWLHEX(6) ]
       ENDDO

!  ..and make the corresponding update to the inverse-transpose of the
!  aspect-tensor basis LUI implied by the hexad:
       W1=-WHEXAD(L) ! new weight for J=1
       DO J=3,6
        WHEXAD(J)=WT(J)-W1 ! These weights become more negative than before..
        DO I=1,6
         LUI(I,J)=NEWLUI(I,J)-LUI1(I)
        ENDDO
       ENDDO
       WHEXAD(2)=WT(2)+W1  ! ..this one becomes more positive than before..
       WHEXAD(1)=W1        ! ..and this one simply switches sign to "positive"
       DO I=1,6
        LUI(I,2)=NEWLUI(I,2)+LUI1(I)
        LUI(I,1)=LUI1(I)
       ENDDO

!  copy the remaining new hexad of grid-steps back to array LHEXAD:
       DO J=2,6      !  (data for J=1 are already in place)
        DO I=1,3
         LHEXAD(I,J)=NEWLHEX(I,J)
        ENDDO
       ENDDO
       KFIRST=3 ! After iteration 1, WHEXAD(1) and (2) are always > 0.
      ENDDO
      write(6,*)'INDEXXI4:  ALL ITERATIONS USED UP.  This should never happen'
      call stop2(68)
      END subroutine gethex

subroutine indexxi4(n,arrin4,indx)

  !-------- indexes an array arrin of length n, i.e. outputs the array indx
  !-------- such that arrin(indx(j)) is in ascending order for j=1,2,...,n.  The
  !-------- input quantities n and arrin are not changed.

  implicit none

  integer(i_long) n
  integer(i_long) arrin4(n)
  integer(i_long) indx(n)

  integer(i_long) i,indxt,ir,j,l,q4

  do j=1,n
   indx(j)=j
  end do
  if(n.eq.1) return

  l=n/2+1
  ir=n

  10 continue

    if(l.gt.1) then
     l=l-1
     indxt=indx(l)
     q4=arrin4(indxt)
    else
     indxt=indx(ir)
     q4=arrin4(indxt)
     indx(ir)=indx(1)
     ir=ir-1
     if(ir.eq.1) then
      indx(1)=indxt
      return
     end if
    end if

    i=l
    j=l+l

    20 continue

      if(j.le.ir) then
       if(j.lt.ir) then
        if(arrin4(indx(j)).lt.arrin4(indx(j+1)))j=j+1
       end if
       if(q4.lt.arrin4(indx(j))) then
        indx(i)=indx(j)
        i=j
        j=j+j
       else
        j=ir+1
       end if
       go to 20

      end if

      indx(i)=indxt
      go to 10

end subroutine indexxi4

subroutine indexxi8(n,arrin8,indx)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    indexxi8
!
!   prgrmmr:
!
! abstract:  indexes an array arrin of length n, i.e. outputs the array indx
!            such that arrin(indx(j)) is in ascending order for j=1,2,...,n.
!            The input quantities n and arrin are not changed.
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     n      -  length of input array
!     arrin  -  input array
!
!   output argument list:
!     indx   - array index
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  implicit none

  integer(i_long) n
  integer(i_llong) arrin8(n)
  integer(i_long) indx(n)

  integer(i_llong) q8
  integer(i_long) i,indxt,ir,j,l

  do j=1,n
   indx(j)=j
  end do
  if(n.eq.1) return

  l=n/2+1
  ir=n

  10 continue

    if(l.gt.1) then
     l=l-1
     indxt=indx(l)
     q8=arrin8(indxt)
    else
     indxt=indx(ir)
     q8=arrin8(indxt)
     indx(ir)=indx(1)
     ir=ir-1
     if(ir.eq.1) then
      indx(1)=indxt
      return
     end if
    end if

    i=l
    j=l+l

    20 continue

      if(j.le.ir) then
       if(j.lt.ir) then
        if(arrin8(indx(j)).lt.arrin8(indx(j+1)))j=j+1
       end if
       if(q8.lt.arrin8(indx(j))) then
        indx(i)=indx(j)
        i=j
        j=j+j
       else
        j=ir+1
       end if
       go to 20

      end if

      indx(i)=indxt
      go to 10

end subroutine indexxi8

SUBROUTINE init_raf4(aspect,triad4,ngauss,rgauss,npass,normal,binom,ifilt_ord,filter, &
                 nvars,idvar,kvar_start,kvar_end,var_names, &
                 ids, ide, jds, jde, kds, kde, &         ! domain indices
                 ips, ipe, jps, jpe, kps, kpe, &         ! patch indices
                 ims, ime, jms, jme, kms, kme, &                     ! memory indices
                 mype, npes)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    init_raf4
!
!   prgrmmr:
!
! abstract:  Obtain filtering constants for recursive anisotropic filter.
!     This form is based on assembling full strings, distributed evenly over processors.
!     No attempt is made in this version to treat any points specially when gathering 
!     the full strings required for each stage of the filter.  This is the simplest and 
!     probably least efficient parallel version of the recursive anisotropic filter.
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     aspect                           - aspect tensor for each point (destroyed)
!     triad4                           - switch to turn on 4 color triad smoothing 
!                                         for 2-dim variables
!     ngauss                           - number of gaussians to be added together
!     rgauss(ngauss)                   - multipying factors on aspect tensor for each term
!     npass                            - 1/2 num of binomial weighted filter apps--npass <= 10
!     normal                           - number of stocastic samples to use for normalization
!     binom                            - .false., then uniform factors,
!                                         .true., then binomial weighted factors
!     ifilt_ord                        - filter order
!     nvars                            - number of variables
!     idvar(kds:kde)                   - variable number of each level
!     kvar_start(nvars)                - starting global vertical index for each variable
!     kvar_end(nvars)                  - ending global vertical index for each variable
!     var_names(nvars)                 - descriptive name of each variable
!     ids, ide, jds, jde, kds, kde     - domain indices
!     ips, ipe, jps, jpe, kps, kpe     - patch indices
!     ims, ime, jms, jme, kms, kme     - memory indices
!     mype                             - mpi task id
!     npes                             -
!
!   output argument list:
!     aspect                           - aspect tensor for each point (destroyed)
!     filter                           - structure which contains everything necessary to
!                                         apply recursive anisotropic filter based on input
!                                         aspect tensor
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  INTEGER(i_long), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices
  INTEGER(i_long), INTENT(IN) :: &
     mype, npes

  TYPE(filter_cons), DIMENSION(7), INTENT(OUT) :: &
                       filter         !  structure which contains everything necessary to
                                      !     apply recursive anisotropic filter based on input
                                      !     aspect tensor

  logical triad4                           !  switch to turn on 4 color triad smoothing for 2-dim variables
  integer(i_long), intent(in) :: ngauss    ! number of gaussians to be added together
  real(r_double), intent(in) ::    rgauss(ngauss)  ! multipying factors on aspect tensor for each term
  INTEGER(i_long), INTENT(IN) :: npass     ! 1/2 num of binomial weighted filter apps--npass <= 10
  integer(i_long), intent(in) :: normal    ! number of stocastic samples to use for normalization
                                           !  (if < 0, then use more expensive form which is 
                                           !   independent of number of processors)
                                           !  (if = 0, then bypass normalization)
  integer(i_long), intent(in) :: ifilt_ord ! filter order
  logical, intent(in) :: binom        !   .false., then uniform factors,
                                      !   .true., then binomial weighted factors

  real(r_single), DIMENSION( 7, ips:ipe, jps:jpe, kps:kpe ), INTENT(INOUT) :: &
            aspect                 ! aspect tensor for each point (destroyed)
                                   !    (1-xx,2--yy,3-zz,4-yz,5-xz,6-xy)
  integer(i_long) nvars                       ! number of variables
  integer(i_long) idvar(kds:kde)              ! variable number of each level
  integer(i_long) kvar_start(nvars)           ! starting global vertical index for each variable
  integer(i_long) kvar_end(nvars)             ! ending global vertical index for each variable
  character(80) var_names(nvars)              ! descriptive name of each variable

  INTEGER(i_short), DIMENSION( 3, (ipe-ips+1)*(jpe-jps+1)*(kpe-kps+1) ) :: &
            i1filter              !  i1filter(1-3,.)=jumpx,jumpy,jumpz

  INTEGER(i_short), DIMENSION( 5, (ipe-ips+1)*(jpe-jps+1)*(kpe-kps+1) ) :: &
            i2filter               !  i2filter(1-5,.)=beginx,beginy,beginz,lenstring,ivar

  INTEGER(i_long) nstrings

  REAL(r_double) binomial0(20,19),sumbin(19)
  REAL(r_double) binomial(10,10)
  real(r_double) factor_binom
  INTEGER(i_short) lhexadx(ips-1:ipe+1,jps-1:jpe+1,kps-1:kpe+1,7)
  INTEGER(i_short) lhexady(ips-1:ipe+1,jps-1:jpe+1,kps-1:kpe+1,7)
  INTEGER(i_short) lhexadz(ips-1:ipe+1,jps-1:jpe+1,kps-1:kpe+1,7)
  INTEGER(i_long) lhexadlast(3,6),lui(6,6)
  integer(i_long) ltriadlast(2,4),lui_triad(3,3)
  INTEGER(i_short), DIMENSION( 6, (ipe-ips+1)*(jpe-jps+1)*(kpe-kps+1) ) :: label_string
  REAL(r_double) aspect8(6),whexad8(6)

  integer(i_long) npoints_recv(0:npes-1)
  integer(i_short),allocatable:: info_string(:,:)
  real(r_single),allocatable:: aspect_full(:)

  integer(i_long) i,icolor,ierr,im,itest,ivar,ivar_end,ivar_start, &
             ixend,ixinc,ixstart,ixtemp, &
             j,jm,jtest,jumpx,jumpy,jumpz,k, &
             kk,km,kt,ktest,len,lentest,lguess,m,npoints_send,nstringsall

  integer(i_long) nstrings_var(nvars),nstrings_varall(nvars,7)
  integer(i_long) lenmax(nvars),lenmin(nvars),npoints1(nvars)
  integer(i_long) lenmaxall(nvars,7),lenminall(nvars,7)
  integer(i_long) totalpoints,totalpoints1(nvars,7)
  integer(i_long) jumpxmax(nvars),jumpxmaxall(nvars,7)
  integer(i_long) jumpymax(nvars),jumpymaxall(nvars,7)
  integer(i_long) jumpzmax(nvars),jumpzmaxall(nvars,7)
  integer(i_long) jumpxmin(nvars),jumpxminall(nvars,7)
  integer(i_long) jumpymin(nvars),jumpyminall(nvars,7)
  integer(i_long) jumpzmin(nvars),jumpzminall(nvars,7)
  real(r_double) lenbar(nvars),lenbarall(nvars,7)
  real(r_double) epstest
  integer(i_long) igauss
  real(r_single) aspect_max(nvars,3),aspect_min(nvars,3)
  real(r_single) aspect_max_all(nvars,3),aspect_min_all(nvars,3)
  real(r_single) srgauss_min,srgauss_max

  filter(1)%ngauss=ngauss
  filter(1)%npass=npass
  filter(1)%ifilt_ord=ifilt_ord

!  compute binomial coefficients

  factor_binom=1._r_double
  if(.not.binom) factor_binom=0._r_double
  binomial0=0._r_double
  binomial0(1,1)=1._r_double
  binomial0(2,1)=1._r_double
  sumbin(1)=2._r_double
  do k=2,19
   binomial0(1,k)=1._r_double
   binomial0(k+1,k)=1._r_double
   do i=2,k
    binomial0(i,k)=binomial0(i-1,k-1)+binomial0(i,k-1)*factor_binom
   end do
   sumbin(k)=0._r_double
   do i=1,k+1
    sumbin(k)=sumbin(k)+binomial0(i,k)
   end do
  end do
  do k=1,19
   binomial0(:,k)=binomial0(:,k)/sumbin(k)
  end do

  kk=0
  binomial=0._r_double
  do k=1,19,2
   kk=kk+1
   binomial(1:kk,kk)=binomial0(1:kk,k)
  end do
        if(mype.eq.0) write(6,*)'INIT_RAF4:  binomial weightings used:',binomial(1:npass,npass)

!  gather some stats on input aspect tensor for informational purposes

  aspect_max=0
  aspect_min=huge(aspect_min)
  do k=kps,kpe
   ivar=idvar(k)
   do j=jps,jpe
    do i=ips,ipe
  !    if(aspect(1,i,j,k).lt..001) write(6,*)' ivar,i,j,k,aspect(1,i,j,k)=',ivar,i,j,k,aspect(1,i,j,k)
     aspect_max(ivar,1)=max(aspect_max(ivar,1),sqrt(aspect(1,i,j,k)))
     aspect_min(ivar,1)=min(aspect_min(ivar,1),sqrt(aspect(1,i,j,k)))
     aspect_max(ivar,2)=max(aspect_max(ivar,2),sqrt(aspect(2,i,j,k)))
     aspect_min(ivar,2)=min(aspect_min(ivar,2),sqrt(aspect(2,i,j,k)))
     aspect_max(ivar,3)=max(aspect_max(ivar,3),sqrt(aspect(3,i,j,k)))
     aspect_min(ivar,3)=min(aspect_min(ivar,3),sqrt(aspect(3,i,j,k)))
    end do
   end do
  end do
  call mpi_reduce(aspect_max,aspect_max_all,3*nvars,mpi_real4,mpi_max,0,mpi_comm_world,ierr)
  call mpi_reduce(aspect_min,aspect_min_all,3*nvars,mpi_real4,mpi_min,0,mpi_comm_world,ierr)
  if(mype.eq.0) then

         write(6,*)' corlen multipliers for additive gaussians: '
            do igauss=1,ngauss
              write(6,*)'  sqrt(rgauss(',igauss,') = ',sqrt(rgauss(igauss))
            end do
            srgauss_min=sqrt(minval(rgauss))
            srgauss_max=sqrt(maxval(rgauss))
    do ivar=1,nvars
      if(kvar_start(ivar).eq.kvar_end(ivar)) then
         write(6,*)' corlen ranges for 2d variable ',trim(var_names(ivar))
         write(6,*)'  min-min, min, max, max-max (1) = ', &
                                     srgauss_min*aspect_min_all(ivar,1), &
                                                 aspect_min_all(ivar,1), &
                                                 aspect_max_all(ivar,1), &
                                     srgauss_max*aspect_max_all(ivar,1)
         write(6,*)'  min-min, min, max, max-max (2) = ', &
                                     srgauss_min*aspect_min_all(ivar,2), &
                                                 aspect_min_all(ivar,2), &
                                                 aspect_max_all(ivar,2), &
                                     srgauss_max*aspect_max_all(ivar,2)
      else
         write(6,*)' corlen ranges for 3d variable ',trim(var_names(ivar))
         write(6,*)'  min-min, min, max, max-max (1) = ', &
                                     srgauss_min*aspect_min_all(ivar,1), &
                                                 aspect_min_all(ivar,1), &
                                                 aspect_max_all(ivar,1), &
                                     srgauss_max*aspect_max_all(ivar,1)
         write(6,*)'  min-min, min, max, max-max (2) = ', &
                                     srgauss_min*aspect_min_all(ivar,2), &
                                                 aspect_min_all(ivar,2), &
                                                 aspect_max_all(ivar,2), &
                                     srgauss_max*aspect_max_all(ivar,2)
         write(6,*)'  min-min, min, max, max-max (3) = ', &
                                     srgauss_min*aspect_min_all(ivar,3), &
                                                 aspect_min_all(ivar,3), &
                                                 aspect_max_all(ivar,3), &
                                     srgauss_max*aspect_max_all(ivar,3)
      end if
    end do
  end if

!  get all directions and smoothing coefficients

  lhexadx=0 ; lhexady=0 ; lhexadz=0
  epstest=10._r_double*epsilon(epstest)
  do k=kps,kpe
   ivar=idvar(k)
   ivar_start=kvar_start(ivar)
   ivar_end=kvar_end(ivar)
   ixstart=ipe ; ixend=ips ; ixinc=-1
   lguess=0
   do j=jps,jpe
    ixtemp=ixstart ; ixstart=ixend ; ixend=ixtemp ; ixinc=-ixinc
    do i=ixstart,ixend,ixinc
     if(triad4.and.ivar_start.eq.ivar_end) then
      aspect8(1)=aspect(1,i,j,k)
      aspect8(2)=aspect(2,i,j,k)
      aspect8(3)=aspect(6,i,j,k)
      call gettri4(aspect8,lguess,ltriadlast,lui_triad,whexad8)
      lhexadlast=0
      do kk=1,4
       lhexadlast(1,kk)=ltriadlast(1,kk)
       lhexadlast(2,kk)=ltriadlast(2,kk)
       lhexadlast(3,kk)=0
      end do
      whexad8(5)=0._8
      whexad8(6)=0._8
     else
      aspect8(1:6)=aspect(1:6,i,j,k)
      call gethex(aspect8,lguess,lhexadlast,lui,whexad8,kt)
     end if
     aspect(1:7,i,j,k)=0._r_double
     do kk=1,6
      if(whexad8(kk).gt.epstest) then
       jumpx=lhexadlast(1,kk)       !  make all directions positive and
       jumpy=lhexadlast(2,kk)       !  assign color
       jumpz=lhexadlast(3,kk)
       if(jumpz.ne.0.and.ivar_start.eq.ivar_end) go to 980 ! if 2-d, strings of interest are x-y only
       if(jumpz.lt.0) then
        jumpx=-jumpx ; jumpy=-jumpy ; jumpz=-jumpz
       end if
       if(jumpz.eq.0) then
        if(jumpy.lt.0) then
         jumpx=-jumpx ; jumpy=-jumpy
        end if
        if(jumpy.eq.0.and.jumpx.lt.0) jumpx=-jumpx
       end if
       if(triad4.and.ivar_start.eq.ivar_end) then
        call what_color_is_triad(jumpx,jumpy,icolor,3)
       else
        call what_color_is(jumpx,jumpy,jumpz,icolor)
       end if
       lhexadx(i,j,k,icolor)=jumpx ; lhexady(i,j,k,icolor)=jumpy ; lhexadz(i,j,k,icolor)=jumpz
       aspect(icolor,i,j,k)=whexad8(kk)
      end if
980   continue
     end do
     lguess=1
    end do
   end do
  end do

!                 big loop over all colors

  do icolor=1,7

!  get all string starting addresses and lengths

   m=0
   do k=kps,kpe
    ivar=idvar(k)
    ivar_start=kvar_start(ivar)
    do j=jps,jpe
     do i=ips,ipe
      jumpz=lhexadz(i,j,k,icolor)
      km=k-jumpz   !  note jumpz always >= 0 by construction
      jumpx=lhexadx(i,j,k,icolor) ; jumpy=lhexady(i,j,k,icolor)
      if(km.lt.ivar_start) then
       m=m+1
       i1filter(1,m)=jumpx ; i1filter(2,m)=jumpy ; i1filter(3,m)=jumpz
       i2filter(1,m)=i ; i2filter(2,m)=j ; i2filter(3,m)=k ; i2filter(5,m)=ivar
       cycle
      end if
      if(jumpx.ne.0.or.jumpy.ne.0.or.jumpz.ne.0) then
       im=max(ips-1,min(i-jumpx,ipe+1))
       jm=max(jps-1,min(j-jumpy,jpe+1))
       if(lhexadx(im,jm,km,icolor).ne.jumpx.or.lhexady(im,jm,km,icolor).ne.jumpy.or. &
             lhexadz(im,jm,km,icolor).ne.jumpz) then
        m=m+1
        i1filter(1,m)=jumpx ; i1filter(2,m)=jumpy ; i1filter(3,m)=jumpz
        i2filter(1,m)=i ; i2filter(2,m)=j ; i2filter(3,m)=k ; i2filter(5,m)=ivar
       end if
      end if
     end do
    end do
   end do
   nstrings=m
   if(npes.eq.1) then
     nstringsall=nstrings
   else
     call mpi_allreduce(nstrings,nstringsall,1,mpi_integer4,mpi_sum,mpi_comm_world,ierr)
   end if
   npoints_send=0
   npoints_recv=0
   nstrings_var=0
   filter(icolor)%nstrings=0
   filter(icolor)%npoints_send=0
   filter(icolor)%npoints_recv=0
   filter(icolor)%npointsmax=0
   filter(icolor)%npointsmaxall=0
   lenbar=0._r_double
   lenmax=-huge(lenmax)
   lenmin=huge(lenmin)
   npoints1=0
   jumpxmax=-huge(jumpxmax) ; jumpymax=-huge(jumpxmax) ; jumpzmax=-huge(jumpxmax)
   jumpxmin= huge(jumpxmin) ; jumpymin= huge(jumpxmin) ; jumpzmin= huge(jumpxmin)
   if(nstringsall.gt.0) then
    if(nstrings.gt.0) then
     do m=1,nstrings
      len=1
      jumpx=i1filter(1,m) ; jumpy=i1filter(2,m) ; jumpz=i1filter(3,m)
      ivar=i2filter(5,m)
      jumpxmax(ivar)=max(jumpx,jumpxmax(ivar))
      jumpymax(ivar)=max(jumpy,jumpymax(ivar))
      jumpzmax(ivar)=max(jumpz,jumpzmax(ivar))
      jumpxmin(ivar)=min(jumpx,jumpxmin(ivar))
      jumpymin(ivar)=min(jumpy,jumpymin(ivar))
      jumpzmin(ivar)=min(jumpz,jumpzmin(ivar))
      i=i2filter(1,m) ; j=i2filter(2,m) ; k=i2filter(3,m)
      ivar_end=kvar_end(ivar)
      do
       lentest=len+1
       ktest=k+jumpz
       if(ktest.gt.ivar_end) then
        i2filter(4,m)=len
        npoints_send=npoints_send+len
        exit
       end if
       itest=max(ips-1,min(i+jumpx,ipe+1))
       jtest=max(jps-1,min(j+jumpy,jpe+1))
       if(jumpx.ne.lhexadx(itest,jtest,ktest,icolor).or. &
              jumpy.ne.lhexady(itest,jtest,ktest,icolor).or. &
                 jumpz.ne.lhexadz(itest,jtest,ktest,icolor)) then
        i2filter(4,m)=len
        npoints_send=npoints_send+len
        exit
       end if
       len=lentest
       i=itest ; j=jtest ; k=ktest
      end do
     end do
    end if

!         Begin computation of alpha, beta, the recursive filter coefficients.
!           It is necessary to have contiguous strings for this process, so first
!             must label strings and assign them to processors so the load is evenly distributed
!             when string pieces are gathered together into full strings


!         assign global label, origin and destination pe number to each string piece
!            (global label is starting i,j,k closest to edge of global domain)

    call string_label(i1filter,i2filter,nstrings,label_string,npoints_recv, &
                     nvars,idvar,kvar_start,kvar_end, &
                     ids, ide, jds, jde, kds, kde, &         ! domain indices
                     ips, ipe, jps, jpe, kps, kpe, &         ! patch indices
                     ims, ime, jms, jme, kms, kme,mype,npes,icolor)          ! memory indices

    filter(icolor)%npointsmax=max(npoints_recv(mype),npoints_send)
    if(npes.eq.1) then
     filter(icolor)%npointsmaxall=filter(icolor)%npointsmax
    else
     call mpi_allreduce(filter(icolor)%npointsmax, &
                       filter(icolor)%npointsmaxall,1,mpi_integer4,mpi_max,mpi_comm_world,ierr)
    end if
    filter(icolor)%npoints_send=npoints_send
    filter(icolor)%npoints_recv=npoints_recv(mype)
    allocate(info_string(8,max(1,npoints_recv(mype))))
    allocate(aspect_full(max(1,npoints_recv(mype))))

!       assemble full strings

    deallocate(filter(icolor)%nsend,filter(icolor)%ndsend)
    deallocate(filter(icolor)%nrecv,filter(icolor)%ndrecv)
    deallocate(filter(icolor)%ia,filter(icolor)%ja,filter(icolor)%ka)
    allocate(filter(icolor)%nsend(0:npes-1))
    allocate(filter(icolor)%ndsend(0:npes))
    allocate(filter(icolor)%nrecv(0:npes-1))
    allocate(filter(icolor)%ndrecv(0:npes))
    allocate(filter(icolor)%ia(max(1,npoints_send)))
    allocate(filter(icolor)%ja(max(1,npoints_send)))
    allocate(filter(icolor)%ka(max(1,npoints_send)))
    call string_assemble4(i1filter,i2filter,nstrings,label_string, &
                     npoints_send,npoints_recv(mype),aspect,icolor, &
                     info_string,aspect_full, &
                     filter(icolor)%nsend,filter(icolor)%ndsend, &
                     filter(icolor)%nrecv,filter(icolor)%ndrecv, &
                     filter(icolor)%ia,filter(icolor)%ja,filter(icolor)%ka, &
                     ids, ide, jds, jde, kds, kde, &                          ! domain indices
                     ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                     ims, ime, jms, jme, kms, kme, &                          ! memory indices
                     mype, npes)

!       organize full strings for processing

    deallocate(filter(icolor)%ib)
    allocate(filter(icolor)%ib(max(1,npoints_recv(mype))))
    if(npoints_recv(mype).gt.0) then
     call sort_strings4(info_string,aspect_full, &
                     npoints_recv(mype),filter(icolor)%ib, &
                     ids, ide, jds, jde, kds, kde, &                          ! domain indices
                     ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                     ims, ime, jms, jme, kms, kme, &                          ! memory indices
                     mype, npes)

!      count number of strings

    call count_strings(info_string,filter(icolor)%nstrings,nstrings_var,nvars,npoints_recv(mype))

!      compute desired alpha and beta for final filter

    deallocate(filter(icolor)%istart,filter(icolor)%lnf,filter(icolor)%bnf)

    allocate(filter(icolor)%istart(filter(icolor)%nstrings+1))
    allocate(filter(icolor)%lnf(ifilt_ord,max(1,npoints_recv(mype)),npass,ngauss))
    allocate(filter(icolor)%bnf(max(1,npoints_recv(mype)),npass,ngauss))
    do igauss=1,ngauss
     if(npoints_recv(mype).gt.0) &
      call alpha_beta4(info_string,aspect_full,rgauss(igauss), &
                     filter(icolor)%lnf,filter(icolor)%bnf,igauss,ngauss, &
                     filter(icolor)%istart,npoints_recv(mype),binomial,npass, &
                     ifilt_ord, &
                     lenbar,lenmax,lenmin,npoints1, &
                     ids, ide, jds, jde, kds, kde, &                          ! domain indices
                     ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                     ims, ime, jms, jme, kms, kme, &                          ! memory indices
                     mype, npes,nvars)

    end do
            
            
    end if
    deallocate(info_string)
    deallocate(aspect_full)

   else

    filter(icolor)%npoints_send=0       !  here if no strings for this color
    filter(icolor)%npoints_recv=0
    filter(icolor)%npointsmax=0
    filter(icolor)%npointsmaxall=0

   end if
         if(npes.eq.1) then
          lenbarall(:,icolor)=lenbar
          nstrings_varall(:,icolor)=nstrings_var
          lenmaxall(:,icolor)=lenmax
          lenminall(:,icolor)=lenmin
          totalpoints1(:,icolor)=npoints1
          jumpxmaxall(:,icolor)=jumpxmax
          jumpymaxall(:,icolor)=jumpymax
          jumpzmaxall(:,icolor)=jumpzmax
          jumpxminall(:,icolor)=jumpxmin
          jumpyminall(:,icolor)=jumpymin
          jumpzminall(:,icolor)=jumpzmin
         else
          call mpi_reduce(lenbar,lenbarall(1,icolor),nvars,mpi_real8,mpi_sum,0,mpi_comm_world,ierr)
          call mpi_reduce(nstrings_var,nstrings_varall(1,icolor),nvars, &
                       mpi_integer4,mpi_sum,0,mpi_comm_world,ierr)
          call mpi_reduce(lenmax,lenmaxall(1,icolor),nvars,mpi_integer4,mpi_max,0,mpi_comm_world,ierr)
          call mpi_reduce(lenmin,lenminall(1,icolor),nvars,mpi_integer4,mpi_min,0,mpi_comm_world,ierr)
          call mpi_reduce(npoints1,totalpoints1(1,icolor),nvars,mpi_integer4,mpi_sum,0,mpi_comm_world,ierr)
          call mpi_reduce(jumpxmax,jumpxmaxall(1,icolor),nvars,mpi_integer4,mpi_max,0,mpi_comm_world,ierr)
          call mpi_reduce(jumpymax,jumpymaxall(1,icolor),nvars,mpi_integer4,mpi_max,0,mpi_comm_world,ierr)
          call mpi_reduce(jumpzmax,jumpzmaxall(1,icolor),nvars,mpi_integer4,mpi_max,0,mpi_comm_world,ierr)
          call mpi_reduce(jumpxmin,jumpxminall(1,icolor),nvars,mpi_integer4,mpi_min,0,mpi_comm_world,ierr)
          call mpi_reduce(jumpymin,jumpyminall(1,icolor),nvars,mpi_integer4,mpi_min,0,mpi_comm_world,ierr)
          call mpi_reduce(jumpzmin,jumpzminall(1,icolor),nvars,mpi_integer4,mpi_min,0,mpi_comm_world,ierr)
         end if

  end do         !     end big loop over all colors

!  print out diagnostics for each variable

  if(mype.eq.0) then
    do ivar=1,nvars

      write(6,*)' STRING STATS FOLLOW FOR VARIABLE #',ivar,':  ',trim(var_names(ivar))
      do icolor=1,7

        if(nstrings_varall(ivar,icolor).gt.0) then
          totalpoints=nint(lenbarall(ivar,icolor))
          lenbarall(ivar,icolor)=lenbarall(ivar,icolor)/nstrings_varall(ivar,icolor)
          write(6,*)'  string stats for ivar,icolor=',ivar,icolor
          write(6,'("        ave string length=",f8.2)')lenbarall(ivar,icolor)
          write(6,'("        max string length=",i8)')lenmaxall(ivar,icolor)
          write(6,'("        min string length=",i8)')lenminall(ivar,icolor)
          write(6,'("        total num strings=",i9)')nstrings_varall(ivar,icolor)
          write(6,'("        total num  points=",i9)')totalpoints
          write(6,'("        num len 1 strings=",i9)')totalpoints1(ivar,icolor)
          write(6,'("             jumpxmin,max=",2i12)')jumpxminall(ivar,icolor),jumpxmaxall(ivar,icolor)
          write(6,'("             jumpymin,max=",2i12)')jumpyminall(ivar,icolor),jumpymaxall(ivar,icolor)
          write(6,'("             jumpzmin,max=",2i12)')jumpzminall(ivar,icolor),jumpzmaxall(ivar,icolor)
        end if

      end do
    end do
  end if

!     get filter normalization

      !       call adjoint_check4(filter, &
      !              ids, ide, jds, jde, kds, kde, &                    ! domain indices
      !              ips, ipe, jps, jpe, kps, kpe, &                    ! patch indices
      !              ims, ime, jms, jme, kms, kme, &                    ! memory indices
      !              mype, npes)
  call normalize_raf4(filter,filter(1)%ngauss,normal, &
                     ids, ide, jds, jde, kds, kde, &                    ! domain indices
                     ips, ipe, jps, jpe, kps, kpe, &                    ! patch indices
                     ims, ime, jms, jme, kms, kme, &                    ! memory indices
                     mype, npes)


return
end subroutine init_raf4

subroutine normalize_raf4(filter,ngauss,normal, &
                         ids, ide, jds, jde, kds, kde, &                ! domain indices
                         ips, ipe, jps, jpe, kps, kpe, &                ! patch indices
                         ims, ime, jms, jme, kms, kme, &                ! memory indices
                         mype, npes)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    normalize_raf4
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     ngauss                        -
!     normal                        -
!     ids, ide, jds, jde, kds, kde  - domain indices
!     ips, ipe, jps, jpe, kps, kpe  - patch indices
!     ims, ime, jms, jme, kms, kme  - memory indices
!     mype                          - mpi task id
!     npes                          -
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block


  INTEGER(i_long), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices

  INTEGER(i_long), INTENT(IN) :: &
     ngauss,normal,mype, npes

  TYPE(filter_cons) filter(7)

  real(r_single) ranvec(2, ngauss,ims:ime, jms:jme, kms:kme )
  real(r_single) bigg( ngauss,ims:ime, jms:jme, kms:kme )

  integer(i_long) i,igauss,j,k,loop,nsamples,ierror
! real(r_double) timef
! real(r_double) t0,t1,timerand,timetot,timerand0,timetot0
  integer(i_long) kbegin,kend
  logical independent_of_npes
  real(4) this_one1,this_one2
                 real(4) work(2,ips:ipe,jps:jpe)

  real(8) seeds(5,0:npes-1)
  integer nseeds
  integer(i_long) jseeds(5,0:npes-1)

  deallocate(filter(1)%amp)
  allocate(filter(1)%amp(ngauss,ips:ipe,jps:jpe,kps:kpe))

  if(normal.eq.0) then
   filter(1)%amp=1.
   return
  end if

  independent_of_npes=normal.lt.0
  nsamples=abs(normal)/2

  call random_number(seeds)
  call random_seed(size=nseeds)
  jseeds=seeds*2147483397
  if(independent_of_npes) then
   call random_seed(put=jseeds(1:nseeds,0))
  else
   call random_seed(put=jseeds(1:nseeds,mype))
  end if

  kbegin=kps ; kend=kpe
  if(independent_of_npes) then
   kbegin=kds ; kend=kde
  end if
  bigg=0._r_double
! timerand=0._r_double
! t0=timef()
  do loop=1,nsamples
   ranvec=0._r_single
!  t1=timef()
   do k=kbegin,kend
    call random_number(work)
    if(k.lt.kps.or.k.gt.kpe) cycle
    do j=jps,jpe
     do i=ips,ipe
      this_one1=1.
      if(work(1,i,j).lt..5) this_one1=-1.
      this_one2=1.
      if(work(2,i,j).lt..5) this_one2=-1.
      do igauss=1,ngauss
       ranvec(1,igauss,i,j,k)=this_one1
       ranvec(2,igauss,i,j,k)=this_one2
      end do
     end do
    end do
   end do

!  timerand=timerand+timef()-t1
  
   call rad_sm24_ad(ranvec,filter,ngauss, &
            ids, ide, jds, jde, kds, kde, &                          ! domain indices
            ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
            ims, ime, jms, jme, kms, kme, &                          ! memory indices
            mype, npes)
   do k=kps,kpe
    do j=jps,jpe
     do i=ips,ipe
      do igauss=1,ngauss
       bigg(igauss,i,j,k)=bigg(igauss,i,j,k)+ranvec(1,igauss,i,j,k)**2+ranvec(2,igauss,i,j,k)**2
      end do
     end do
    end do
   end do
  end do
! timetot=timef()-t0
! call mpi_reduce(timetot,timetot0,1,mpi_real8,mpi_max,0,mpi_comm_world,ierror)
! call mpi_reduce(timerand,timerand0,1,mpi_real8,mpi_max,0,mpi_comm_world,ierror)
! if(mype.eq.0) write(6,'(" total time for normalization = ",f15.3)').001_8*timetot0
! if(mype.eq.0) write(6,'(" total time for random_number = ",f15.3)').001_8*timerand0

  do k=kps,kpe
   do j=jps,jpe
    do i=ips,ipe
     do igauss=1,ngauss
      filter(1)%amp(igauss,i,j,k)=1._r_double/sqrt(bigg(igauss,i,j,k)/(2._r_double*nsamples))
     end do
    end do
   end do
  end do

end subroutine normalize_raf4

subroutine one_color4(g,filter,ngauss,ipass,ifilt_ord, &
             nstrings,istart, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             mype, npes)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    one_color4
!
!   prgrmmr:
!
! abstract:  apply one forward-backward recursive filter for one color
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     g                             -  input--field on grid, output--filtered field on grid
!     filter                        -
!     ngauss                        -
!     ipass                         - total number of contiguous string points
!     ifilt_ord                     -
!     nstrings                      -
!     istart                        -
!     ids, ide, jds, jde, kds, kde  - domain indices
!     ips, ipe, jps, jpe, kps, kpe  - patch indices
!     ims, ime, jms, jme, kms, kme  - memory indices
!     mype                          - mpi task id
!     npes                          -
!
!   output argument list:
!     g        -  input--field on grid, output--filtered field on grid
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  INTEGER(i_long), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices

  INTEGER(i_long), INTENT(IN) :: &
     mype, npes,ngauss

  INTEGER(i_long), INTENT(IN) :: &
            ipass          !  total number of contiguous string points
  integer(i_long),intent(in):: ifilt_ord

  real(r_single), DIMENSION( ngauss,ims:ime, jms:jme, kms:kme ), INTENT(INOUT) :: &
            g                      !  input--field on grid, output--filtered field on grid

  integer(i_long),intent(in):: nstrings
  integer(i_long),intent(in):: istart(nstrings+1)
  type(filter_cons) filter

  real(r_single) work(ngauss,max(1,filter%npointsmax),2)
  real(r_single) work2(max(1,filter%npointsmax))

  integer(i_long) i,ierr,igauss,j,l,mpi_string

!-- gather up strings

  call mpi_type_contiguous(ngauss,mpi_real4,mpi_string,ierr)
  call mpi_type_commit(mpi_string,ierr)

  if(filter%npoints_send.gt.0) then
   do i=1,filter%npoints_send
    do igauss=1,ngauss
     work(igauss,i,1)=g(igauss,filter%ia(i),filter%ja(i),filter%ka(i))
    end do
   end do
  end if
  if(npes.eq.1.and.filter%npoints_recv.gt.0) then
   do i=1,filter%npoints_recv
    do igauss=1,ngauss
     work(igauss,i,2)=work(igauss,i,1)
    end do
   end do
  else
   call mpi_alltoallv(work(1,1,1),filter%nsend,filter%ndsend,mpi_string, &
                   work(1,1,2),filter%nrecv,filter%ndrecv,mpi_string,mpi_comm_world,ierr)
  end if
  if(filter%npoints_recv.gt.0) then
   do igauss=1,ngauss
    do i=1,filter%npoints_recv
     work2(i)=work(igauss,filter%ib(i),2)
    end do

    do j=1,nstrings
     do i=istart(j)+1,istart(j+1)-1
      do l=1,min(ifilt_ord,i-istart(j))
       work2(i)=work2(i)-filter%lnf(l,i,ipass,igauss)*work2(i-l)
      end do
     end do
     do i=istart(j),istart(j+1)-1
      work2(i)=filter%bnf(i,ipass,igauss)*work2(i)
     end do
     do i=istart(j+1)-2,istart(j),-1
      do l=1,min(ifilt_ord,istart(j+1)-i-1)
       work2(i)=work2(i)-filter%lnf(l,i+l,ipass,igauss)*work2(i+l)
      end do
     end do
    end do

    do i=1,filter%npoints_recv
     work(igauss,i,1)=work2(i)
    end do
   end do

!-- send strings back

   do i=1,filter%npoints_recv
    do igauss=1,ngauss
     work(igauss,filter%ib(i),2)=work(igauss,i,1)
    end do
   end do
  end if
  if(npes.eq.1.and.filter%npoints_recv.gt.0) then
   do i=1,filter%npoints_recv
    do igauss=1,ngauss
     work(igauss,i,1)=work(igauss,i,2)
    end do
   end do
  else
   call mpi_alltoallv(work(1,1,2),filter%nrecv,filter%ndrecv,mpi_string, &
                     work(1,1,1),filter%nsend,filter%ndsend,mpi_string,mpi_comm_world,ierr)
  end if
  if(filter%npoints_send.gt.0) then
   do i=1,filter%npoints_send
    do igauss=1,ngauss
     g(igauss,filter%ia(i),filter%ja(i),filter%ka(i))=work(igauss,i,1)
    end do
   end do
  end if

  call mpi_type_free(mpi_string,ierr)

end subroutine one_color4

subroutine one_color24(g,filter,ngauss,ipass,ifilt_ord, &
             nstrings,istart, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             mype, npes)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    one_color24
!
!   prgrmmr:
!
! abstract:  apply one forward-backward recursive filter for one color
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     g                             -  input--field on grid, output--filtered field on grid
!     filter                        -
!     ngauss                        -
!     ipass                         - total number of contiguous string points
!     ifilt_ord                     -
!     nstrings                      -
!     istart                        -
!     ids, ide, jds, jde, kds, kde  - domain indices
!     ips, ipe, jps, jpe, kps, kpe  - patch indices
!     ims, ime, jms, jme, kms, kme  - memory indices
!     mype                          - mpi task id
!     npes                          -
!
!   output argument list:
!     g                             -  input--field on grid, output--filtered field on grid
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  INTEGER(i_long), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices

  INTEGER(i_long), INTENT(IN) :: &
     mype, npes,ngauss

  INTEGER(i_long), INTENT(IN) :: &
            ipass          !  total number of contiguous string points
  integer(i_long),intent(in):: ifilt_ord

  real(r_single), DIMENSION(2,ngauss, ims:ime, jms:jme, kms:kme ), INTENT(INOUT) :: &
            g                      !  input--field on grid, output--filtered field on grid

  integer(i_long),intent(in):: nstrings
  integer(i_long),intent(in):: istart(nstrings+1)
  type(filter_cons) filter

  real(r_single) work(2,ngauss,max(1,filter%npointsmax),2)
  real(r_single) work2(max(1,filter%npointsmax))

  integer(i_long) i,ierr,igauss,ii,ishort_end,j,l,mpi_string

!-- gather up strings

  call mpi_type_contiguous(2*ngauss,mpi_real4,mpi_string,ierr)
  call mpi_type_commit(mpi_string,ierr)

  if(filter%npoints_send.gt.0) then
   do i=1,filter%npoints_send
    do igauss=1,ngauss
     work(1,igauss,i,1)=g(1,igauss,filter%ia(i),filter%ja(i),filter%ka(i))
     work(2,igauss,i,1)=g(2,igauss,filter%ia(i),filter%ja(i),filter%ka(i))
    end do
   end do
  end if
  if(npes.eq.1.and.filter%npoints_recv.gt.0) then
   do i=1,filter%npoints_recv
    do igauss=1,ngauss
     work(1,igauss,i,2)=work(1,igauss,i,1)
     work(2,igauss,i,2)=work(2,igauss,i,1)
    end do
   end do
  else
   call mpi_alltoallv(work(1,1,1,1),filter%nsend,filter%ndsend,mpi_string, &
                   work(1,1,1,2),filter%nrecv,filter%ndrecv,mpi_string,mpi_comm_world,ierr)
  end if
  if(filter%npoints_recv.gt.0) then
   do igauss=1,ngauss
    do ii=1,2
     do i=1,filter%npoints_recv
      work2(i)=work(ii,igauss,filter%ib(i),2)
     end do

     do j=1,nstrings
      do i=istart(j)+1,istart(j+1)-1
       do l=1,min(ifilt_ord,i-istart(j))
        work2(i)=work2(i)-filter%lnf(l,i,ipass,igauss)*work2(i-l)
       end do
      end do
      do i=istart(j),istart(j+1)-1
       work2(i)=filter%bnf(i,ipass,igauss)*work2(i)
      end do
      do i=istart(j+1)-2,istart(j),-1
       do l=1,min(ifilt_ord,istart(j+1)-i-1)
        work2(i)=work2(i)-filter%lnf(l,i+l,ipass,igauss)*work2(i+l)
       end do
      end do
     end do

     do i=1,filter%npoints_recv
      work(ii,igauss,i,1)=work2(i)
     end do
    end do
   end do

!-- send strings back

   do i=1,filter%npoints_recv
    do igauss=1,ngauss
     work(1,igauss,filter%ib(i),2)=work(1,igauss,i,1)
     work(2,igauss,filter%ib(i),2)=work(2,igauss,i,1)
    end do
   end do
  end if
  if(npes.eq.1.and.filter%npoints_recv.gt.0) then
   do i=1,filter%npoints_recv
    do igauss=1,ngauss
     work(1,igauss,i,1)=work(1,igauss,i,2)
     work(2,igauss,i,1)=work(2,igauss,i,2)
    end do
   end do
  else
   call mpi_alltoallv(work(1,1,1,2),filter%nrecv,filter%ndrecv,mpi_string, &
                     work(1,1,1,1),filter%nsend,filter%ndsend,mpi_string,mpi_comm_world,ierr)
  end if
  if(filter%npoints_send.gt.0) then
   do i=1,filter%npoints_send
    do igauss=1,ngauss
     g(1,igauss,filter%ia(i),filter%ja(i),filter%ka(i))=work(1,igauss,i,1)
     g(2,igauss,filter%ia(i),filter%ja(i),filter%ka(i))=work(2,igauss,i,1)
    end do
   end do
  end if

  call mpi_type_free(mpi_string,ierr)

end subroutine one_color24

SUBROUTINE raf4(g,filter,ngauss, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             mype, npes)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    raf4
!
!   prgrmmr:
!
! abstract:  1st half of recursive anisotropic self-adjoint filter (full-strings version)
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     g                             -  input--field on grid, output--filtered field on grid
!     filter                        -
!     ngauss                        -
!     ids, ide, jds, jde, kds, kde  - domain indices
!     ips, ipe, jps, jpe, kps, kpe  - patch indices
!     ims, ime, jms, jme, kms, kme  - memory indices
!     mype                          - mpi task id
!     npes                          -
!
!   output argument list:
!     g                             -  input--field on grid, output--filtered field on grid
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block


  implicit none

  INTEGER(i_long), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices

  INTEGER(i_long), INTENT(IN) :: &
     mype, npes,ngauss

  real(r_single), DIMENSION(ngauss, ims:ime, jms:jme, kms:kme ), INTENT(INOUT) :: &
            g                      !  input--field to be filtered, output--filtered field

  TYPE(filter_cons) filter(7)             !  structure defining recursive filter

  integer(i_long) i,icolor,ipass,igauss,j,k

!   multiply by amp first

           !  write(6,*)' at 1 in raf4, ngauss,npass,i,j,kps,pe=',ngauss,filter(1)%npass,&
           !            ips,ipe,jps,jpe,kps,kpe
  do k=kps,kpe
        ! write(6,*)' k,min,max g=',k,minval(g(1:ngauss,ips:ipe,jps:jpe,k)), &
        !                             maxval(g(1:ngauss,ips:ipe,jps:jpe,k))
   do j=jps,jpe
    do i=ips,ipe
     do igauss=1,ngauss
      g(igauss,i,j,k)=filter(1)%amp(igauss,i,j,k)*g(igauss,i,j,k)
     end do
    end do
   end do
  end do

  if(filter(1)%npass.gt.0) then
   do ipass=1,filter(1)%npass

    do icolor=7,1,-1

        !     if(mype.eq.0) write(6,*)' at 2 in raf4, icolor,npointsmaxall=', &
        !               icolor,filter(icolor)%npointsmaxall
     if(filter(icolor)%npointsmaxall.gt.0) &
         call one_color4(g,filter(icolor),ngauss,ipass,filter(1)%ifilt_ord, &
             filter(icolor)%nstrings,filter(icolor)%istart, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             mype, npes)
   
    end do

   end do
  end if

end subroutine raf4

SUBROUTINE raf_sm4(g,filter,ngauss, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             mype, npes)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    raf_sm4
!
!   prgrmmr:
!
! abstract:  1st half of recursive anisotropic self-adjoint filter (full-strings version)
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     g                             -  input--field on grid, output--filtered field on grid
!     filter                        -
!     ngauss                        -
!     ids, ide, jds, jde, kds, kde  - domain indices
!     ips, ipe, jps, jpe, kps, kpe  - patch indices
!     ims, ime, jms, jme, kms, kme  - memory indices
!     mype                          - mpi task id
!     npes                          -
!
!   output argument list:
!     g                             -  input--field on grid, output--filtered field on grid
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  implicit none

  INTEGER(i_long), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices

  INTEGER(i_long), INTENT(IN) :: &
     mype, npes,ngauss

  real(r_single), DIMENSION( ngauss,ims:ime, jms:jme, kms:kme ), INTENT(INOUT) :: &
            g                      !  input--field to be filtered, output--filtered field

  TYPE(filter_cons) filter(7)             !  structure defining recursive filter

  integer(i_long) icolor,ipass

  if(filter(1)%npass.gt.0) then
   do ipass=1,filter(1)%npass

    do icolor=7,1,-1

     if(filter(icolor)%npointsmaxall.gt.0) &
         call one_color4(g,filter(icolor),ngauss,ipass,filter(1)%ifilt_ord, &
             filter(icolor)%nstrings,filter(icolor)%istart, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             mype, npes)
   
    end do

   end do
  end if

end subroutine raf_sm4

subroutine sort_strings4(info_string,aspect_full, &
                     npoints_recv,ib, &
                     ids, ide, jds, jde, kds, kde, &                          ! domain indices
                     ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                     ims, ime, jms, jme, kms, kme, &                          ! memory indices
                     mype, npes)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    sort_strings4
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     info_string                   - 1       - distance from origin to current point
!                                     2,3,4   - origin coordinates
!                                     5,6,7,8 - jumpx,jumpy,jumpz,ivar for this string
!     aspect_full                   -
!     npoints_recv                  -
!     ids, ide, jds, jde, kds, kde  - domain indices
!     ips, ipe, jps, jpe, kps, kpe  - patch indices
!     ims, ime, jms, jme, kms, kme  - memory indices
!     mype                          - mpi task id
!     npes                          -
!
!   output argument list:
!     info_string                   -
!     aspect_full                   -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  !   sort strings by string id and distance

  IMPLICIT NONE

  INTEGER(i_long), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices
  INTEGER(i_long), INTENT(IN) :: &
     mype, npes

  INTEGER(i_long), INTENT(IN) :: npoints_recv


  INTEGER(i_short), DIMENSION( 8, npoints_recv ), INTENT(INOUT) ::  &
            info_string      !      1---- distance from origin to current point
                             !      2,3,4-- origin coordinates
                             !      5,6,7,8-- jumpx,jumpy,jumpz,ivar for this string
  real(r_single), DIMENSION( npoints_recv ) , INTENT(INOUT) :: &
            aspect_full

  integer(i_long) ib(npoints_recv)

  integer(i_long) ij_origin(npoints_recv)
  integer(i_short) iwork(npoints_recv)
  real(r_single) work(npoints_recv)

  integer(i_long) i,idist,idistlen,idistmax,idistmin,idjxlen,idjxylen,idjxyzlen,idjxyzx0len,idjxyzxy0len
  integer(i_long) idjxyzxyz0len
  integer(i_long) ivar,ivarmax,ivarmin
  integer(i_long) ix0,ix0len,ix0max,ix0min,iy0,iy0len,iy0max,iy0min,iz0,iz0len,iz0max
  integer(i_long) iz0min,j,jumpx,jumpxlen,jumpxmax,jumpxmin,jumpy,jumpylen,jumpymax,jumpymin
  integer(i_long) jumpz,jumpzlen,jumpzmax,jumpzmin

!   obtain range of jumpx,jumpy,originx,originy

  jumpxmin=huge(jumpxmin) ; jumpxmax=-jumpxmin
  jumpymin=jumpxmin ; jumpymax=jumpxmax
  jumpzmin=jumpxmin ; jumpzmax=jumpxmax
  ix0min=jumpxmin ; ix0max=jumpxmax
  iy0min=jumpxmin ; iy0max=jumpxmax
  iz0min=jumpxmin ; iz0max=jumpxmax
  idistmin=jumpxmin ; idistmax=jumpxmax
  ivarmin=jumpxmin ; ivarmax=jumpxmax
  do i=1,npoints_recv
   jumpx=info_string(5,i) ; jumpy=info_string(6,i) ; jumpz=info_string(7,i)
   ix0=info_string(2,i) ; iy0=info_string(3,i) ; iz0=info_string(4,i)
   idist=info_string(1,i) ; ivar=info_string(8,i)
   jumpxmin=min(jumpx,jumpxmin) ; jumpxmax=max(jumpx,jumpxmax)
   jumpymin=min(jumpy,jumpymin) ; jumpymax=max(jumpy,jumpymax)
   jumpzmin=min(jumpz,jumpzmin) ; jumpzmax=max(jumpz,jumpzmax)
   ivarmin=min(ivar,ivarmin)
   ix0min=min(ix0,ix0min) ; ix0max=max(ix0,ix0max)
   iy0min=min(iy0,iy0min) ; iy0max=max(iy0,iy0max)
   iz0min=min(iz0,iz0min) ; iz0max=max(iz0,iz0max)
   idistmin=min(idist,idistmin) ; idistmax=max(idist,idistmax)
  end do
  jumpxlen=jumpxmax-jumpxmin+1 ; jumpylen=jumpymax-jumpymin+1 ; jumpzlen=jumpzmax-jumpzmin+1
  idistlen=idistmax-idistmin+1
  ix0len=ix0max-ix0min+1 ; iy0len=iy0max-iy0min+1 ; iz0len=iz0max-iz0min+1
  idjxlen=idistlen*jumpxlen
  idjxylen=idjxlen*jumpylen
  idjxyzlen=idjxylen*jumpzlen
  idjxyzx0len=idjxyzlen*ix0len
  idjxyzxy0len=idjxyzx0len*iy0len
  idjxyzxyz0len=idjxyzxy0len*iz0len

  do i=1,npoints_recv
   jumpx=info_string(5,i) ; jumpy=info_string(6,i) ; jumpz=info_string(7,i)
   ix0=info_string(2,i) ; iy0=info_string(3,i) ; iz0=info_string(4,i)
   idist=info_string(1,i) ; ivar=info_string(8,i)
   ij_origin(i)=idist-idistmin &
                  +idistlen*(jumpx-jumpxmin) &
                     +idjxlen*(jumpy-jumpymin) &
                        +idjxylen*(jumpz-jumpzmin) &
                           +idjxyzlen*(ix0-ix0min) &
                              +idjxyzx0len*(iy0-iy0min) &
                                 +idjxyzxy0len*(iz0-iz0min) &
                                 +idjxyzxyz0len*(ivar-ivarmin)
  end do
  call indexxi4(npoints_recv,ij_origin,ib)

  do j=1,8
   do i=1,npoints_recv
    iwork(i)=info_string(j,ib(i))
   end do
   do i=1,npoints_recv
    info_string(j,i)=iwork(i)
   end do
  end do
  do i=1,npoints_recv
   work(i)=aspect_full(ib(i))
  end do
  do i=1,npoints_recv
   aspect_full(i)=work(i)
  end do

end subroutine sort_strings4


SUBROUTINE string_assemble4(i1filter,i2filter,nstrings,label_string, &
                     npoints_send,npoints_recv,aspect,icolor, &
                     info_string,aspect_full,nsend,ndsend,nrecv,ndrecv,ia,ja,ka, &
                     ids, ide, jds, jde, kds, kde, &                          ! domain indices
                     ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                     ims, ime, jms, jme, kms, kme, &                          ! memory indices
                     mype, npes)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    string_assemble4
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     i1filter                     - i1filter(1-3,.)=jumpx,jumpy,jumpz
!     i2filter                     - i2filter(1-5,.)=beginx,beginy,beginz,lenstring,ivar
!     nstrings                     -
!     label_string                 -  label_string(1-3,.)=originx,originy,originz
!     npoints_send                 -  number of points to send for assembling strings
!     npoints_recv                 -  number of points for assembled strings
!     aspect                       - aspect tensor numbers
!     icolor                       -
!     ids, ide, jds, jde, kds, kde - domain indices
!     ips, ipe, jps, jpe, kps, kpe - patch indices
!     ims, ime, jms, jme, kms, kme - memory indices
!     mype                         - mpi task id
!     npes                         -
!
!   output argument list:
!     info_string                  - 1---- distance from origin to current point
!                                  - 2,3,4-- origin coordinates
!                                  - 5,6,7,8-- jumpx,jumpy,jumpz,ivar for this string
!     aspect_full                  -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

!   assemble groups of unbroken strings approximately evenly distributed over all processors


  INTEGER(i_long), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices
  INTEGER(i_long), INTENT(IN) :: &
     mype, npes

  INTEGER(i_long), INTENT(IN) :: nstrings,icolor

  INTEGER(i_short), DIMENSION( 3, * ), INTENT(IN) :: &
            i1filter                       !  i1filter(1-3,.)=jumpx,jumpy,jumpz

  INTEGER(i_short), DIMENSION( 5, * ), INTENT(IN) :: &
            i2filter                       !  i2filter(1-5,.)=beginx,beginy,beginz,lenstring,ivar

  INTEGER(i_short), DIMENSION( 6, * ), INTENT(IN) :: &
            label_string            !  label_string(1-3,.)=originx,originy,originz
                                    !  label_string(4,.)=distance from origin to start of string
                                    !  label_string(5-6,.)=dest pe, ivar

  INTEGER(i_long), INTENT(IN) :: &
            npoints_send, &         !  number of points to send for assembling strings
            npoints_recv            !  number of points for assembled strings

  real(r_single), DIMENSION( 7, ips:ipe, jps:jpe, kps:kpe ), INTENT(IN) :: &
            aspect                   !  aspect tensor numbers (recursive filter parameters derived
                                     !            from these)
  INTEGER(i_short), DIMENSION( 8, max(1,npoints_recv) ), INTENT(OUT) ::  &
            info_string      !      1---- distance from origin to current point
                             !      2,3,4-- origin coordinates
                             !      5,6,7,8-- jumpx,jumpy,jumpz,ivar for this string
  real(r_single), DIMENSION( max(1,npoints_recv) ) , INTENT(OUT) :: &
            aspect_full
  integer(i_long) nsend(0:npes-1),ndsend(0:npes),nrecv(0:npes-1),ndrecv(0:npes)
  integer(i_short) ia(npoints_send),ja(npoints_send),ka(npoints_send)

  integer(i_long) idest(npoints_send)
  integer(i_long) indx(npoints_send)
  integer(i_short) iwork(npoints_send)
  integer(i_short) string_info(8,npoints_send)
  real(r_single) full_aspect(npoints_send),work(npoints_send)

  integer(i_long) i,i0,idestpe,idist,ierr,ivar,j,j0,jumpx,jumpy,jumpz,k,k0,kk,len,m,mbuf,mpe,mpi_string1

  mbuf=0

!       setup string_info array

  nsend=0
  if(nstrings.gt.0) then
   do m=1,nstrings
    len=i2filter(4,m)
    jumpx=i1filter(1,m) ; jumpy=i1filter(2,m) ; jumpz=i1filter(3,m)
    i=i2filter(1,m) ; j=i2filter(2,m) ; k=i2filter(3,m) ; ivar=i2filter(5,m)
    i0=label_string(1,m) ; j0=label_string(2,m) ; k0=label_string(3,m)
    idist=label_string(4,m)
    idestpe=label_string(5,m)
    do kk=1,len
     mbuf=mbuf+1
     string_info(1,mbuf)=idist
     string_info(2,mbuf)=i0 ; string_info(3,mbuf)=j0 ; string_info(4,mbuf)=k0
     string_info(5,mbuf)=jumpx ; string_info(6,mbuf)=jumpy ; string_info(7,mbuf)=jumpz
     string_info(8,mbuf)=ivar
     ia(mbuf)=i ; ja(mbuf)=j ; ka(mbuf)=k
     idest(mbuf)=idestpe
     nsend(idestpe)=nsend(idestpe)+1
     full_aspect(mbuf)=aspect(icolor,i,j,k)
     i=i+jumpx ; j=j+jumpy ; k=k+jumpz
     if(idist.ge.0) idist=idist+1
    end do
   end do
  end if
        if(mbuf.ne.npoints_send) then
           write(6,*)'STRING_ASSEMBLE4:   ***PROBLEM***  mbuf ne npoints_send, mype,mbuf,npoints_send=', &
                mype,mbuf,npoints_send
           call stop2(68)
        end if

!    sort destination pe numbers from smallest to largest

  if(mbuf.gt.0) then
   call indexxi4(mbuf,idest,indx)

!     use sort index to reorder everything

   do j=1,8
    do i=1,mbuf
     iwork(i)=string_info(j,indx(i))
    end do
    do i=1,mbuf
     string_info(j,i)=iwork(i)
    end do
   end do
   do i=1,mbuf
    iwork(i)=ia(indx(i))
   end do
   do i=1,mbuf
    ia(i)=iwork(i)
   end do
   do i=1,mbuf
    iwork(i)=ja(indx(i))
   end do
   do i=1,mbuf
    ja(i)=iwork(i)
   end do
   do i=1,mbuf
    iwork(i)=ka(indx(i))
   end do
   do i=1,mbuf
    ka(i)=iwork(i)
   end do

   do i=1,mbuf
    work(i)=full_aspect(indx(i))
   end do
   do i=1,mbuf
    full_aspect(i)=work(i)
   end do
  end if

!  now get remaining info necessary for using alltoall command

  ndsend(0)=0
  do mpe=1,npes
   ndsend(mpe)=ndsend(mpe-1)+nsend(mpe-1)
  end do
   
  if(npes.eq.1) then
   nrecv(0)=nsend(0)
  else
   call mpi_alltoall(nsend,1,mpi_integer, &
       nrecv,1,mpi_integer,mpi_comm_world,ierr)
  end if
  ndrecv(0)=0
  do mpe=1,npes
   ndrecv(mpe)=ndrecv(mpe-1)+nrecv(mpe-1)
  end do
  if(npes.eq.1) then
   do j=1,max(1,npoints_recv)
    do i=1,8
     info_string(i,j)=string_info(i,j)
    end do
    aspect_full(j)=full_aspect(j)
   end do
  else
   call mpi_type_contiguous(8,mpi_integer2,mpi_string1,ierr)
   call mpi_type_commit(mpi_string1,ierr)
   call mpi_alltoallv(string_info,nsend,ndsend,mpi_string1, &
                      info_string,nrecv,ndrecv,mpi_string1,mpi_comm_world,ierr)
   call mpi_type_free(mpi_string1,ierr)
   call mpi_alltoallv(full_aspect,nsend,ndsend,mpi_real4, &
                      aspect_full,nrecv,ndrecv,mpi_real4,mpi_comm_world,ierr)
  end if

end subroutine string_assemble4

SUBROUTINE string_label(i1filter,i2filter,nstrings,label_string,npoints_recv, &
                     nvars,idvar,kvar_start,kvar_end, &
                     ids, ide, jds, jde, kds, kde, &                          ! domain indices
                     ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                     ims, ime, jms, jme, kms, kme, mype, npes,icolor )               ! memory indices
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    string_label
!
!   prgrmmr:
!
! abstract:  assign global string labels to each string
!             (global label is first i,j,k inside global domain)
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     i1filter                     - i1filter(1-3,.)=jumpx,jumpy,jumpz
!     i2filter                     - i2filter(1-5,.)=beginx,beginy,beginz,lenstring,ivar
!     nstrings                     -
!     npoints_send                 -  number of points to send for assembling strings
!     npoints_recv                 -  number of points for assembled strings
!     icolor                       -
!     ids, ide, jds, jde, kds, kde - domain indices
!     ips, ipe, jps, jpe, kps, kpe - patch indices
!     ims, ime, jms, jme, kms, kme - memory indices
!     mype                         - mpi task id
!     npes                         -
!
!   output argument list:
!     label_string                 -  label_string(1-3,.)=originx,originy,originz
!     aspect_full                  -
!     mype     - mpi task id
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

              integer(i_long) icolor
  INTEGER(i_long), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices

  INTEGER(i_long) nstrings

  INTEGER(i_short), DIMENSION( 3, * ), INTENT(IN) :: &
            i1filter                       !  i1filter(1-3,.)=jumpx,jumpy,jumpz

  INTEGER(i_short), DIMENSION( 5, * ), INTENT(IN) :: &
            i2filter                       !  i2filter(1-5,.)=beginx,beginy,beginz,lenstring,ivar

  INTEGER(i_short), DIMENSION( 6, * ), INTENT(OUT) :: &
            label_string            !  label_string(1-3,.)=originx,originy,originz
                                    !  label_string(4,.)=distance from origin to start of string
                                    !  label_string(5,.)=destination pe for string piece
                                    !  label_string(6,.)=ivar

  integer(i_long) i,idist,idisttest,ierr,istring_pe,itest,ivar,ivar_end,ivar_start
  integer(i_long) j,jtest,jumpx,jumpy,jumpz,k,ktest,mpe,mype,n,npes,nstrings0
  integer(i_llong) lastlabel

  integer(i_long), intent(out):: npoints_recv(0:npes-1)
  integer(i_long) nvars
  integer(i_long) idvar(kds:kde),kvar_start(nvars),kvar_end(nvars)

  integer(i_llong) labelijk(max(1,nstrings))
  integer(i_long) nrecv(0:npes-1),ndrecv(0:npes)
  integer(i_llong),allocatable::labelijk0(:)
  integer(i_long),allocatable::index(:)

     !  if(mype.eq.0.and.icolor.eq.3) write(6,*)' at 1 in string_label, nstrings=',nstrings
  if(nstrings.gt.0) then
   do n=1,nstrings

    jumpx=i1filter(1,n) ; jumpy=i1filter(2,n) ; jumpz=i1filter(3,n)
    i=i2filter(1,n) ; j=i2filter(2,n) ; k=i2filter(3,n) ; ivar=i2filter(5,n)
    ivar_start=kvar_start(ivar)
    ivar_end=kvar_end(ivar)
    idist=0
    do 
     idisttest=idist+1
     ktest=k-jumpz
     if(ktest.lt.ivar_start) then
      label_string(1,n)=i ; label_string(2,n)=j ; label_string(3,n)=k
      label_string(4,n)=idist ; label_string(6,n)=ivar
      exit
     end if
     itest=i-jumpx
     if(itest.lt.ids.or.itest.gt.ide) then
      label_string(1,n)=i ; label_string(2,n)=j ; label_string(3,n)=k
      label_string(4,n)=idist ; label_string(6,n)=ivar
      exit
     end if
     jtest=j-jumpy
     if(jtest.lt.jds.or.jtest.gt.jde) then
      label_string(1,n)=i ; label_string(2,n)=j ; label_string(3,n)=k
      label_string(4,n)=idist ; label_string(6,n)=ivar
      exit
     end if
     i=itest ; j=jtest ; k=ktest
     idist=idisttest
    end do
    ivar=label_string(6,n)
    labelijk(n)=label_string(1,n)+(ide-ids+1)*(label_string(2,n)-1+(jde-jds+1)*(label_string(3,n)-1 &
                            +(kde-kds+1)*(ivar-1)))

   end do
  end if

!--  assemble all string labels to pe 0, for assignment of pe numbers

  nrecv=0
  if(npes.eq.1) then
   nrecv(0)=nstrings
  else
   call mpi_allgather(nstrings,1,mpi_integer4,nrecv,1,mpi_integer4,mpi_comm_world,ierr)
  end if
  ndrecv(0)=0
  do i=1,npes
   ndrecv(i)=ndrecv(i-1)+nrecv(i-1)
  end do
  nstrings0=ndrecv(npes)
  allocate(labelijk0(nstrings0))
  if(npes.eq.1) then
   do i=1,nstrings0
    labelijk0(i)=labelijk(i)
   end do
  else
   call my_gatherv8(labelijk,nstrings,labelijk0,nstrings0,nrecv,ndrecv,mype,npes)
!  call mpi_gatherv(labelijk,nstrings,mpi_integer8,labelijk0,nrecv,ndrecv,mpi_integer8,0,mpi_comm_world,ierr)
  end if

!------ sort strings so strings with same labels are adjacent, then assign adjacent strings to same pe.
!------   then when we assemble all pieces, it is guaranteed that all pieces of every contiguous string
!------   will end up on the same processor.

  if(mype.eq.0) then
   allocate(index(nstrings0))
   call indexxi8(nstrings0,labelijk0,index)
   lastlabel=-huge(lastlabel)
   istring_pe=0
   do i=1,nstrings0
    j=index(i)
    if(labelijk0(j).ne.lastlabel) then
     lastlabel=labelijk0(j)
     istring_pe=mod(istring_pe+1,npes)
    end if
    labelijk0(j)=istring_pe
   end do
   deallocate(index)
  end if

!---- now scatter pe destination numbers back
 
  if(npes.eq.1) then
   do i=1,nstrings0
    labelijk(i)=labelijk0(i)
   end do
  else
   call my_scatterv8(labelijk0,nstrings0,labelijk,nstrings,nrecv,ndrecv,mype,npes)
!  call mpi_scatterv(labelijk0,nrecv,ndrecv,mpi_integer8,labelijk,nstrings,mpi_integer8,0,mpi_comm_world,ierr)
  end if
  deallocate(labelijk0)

!---- assign destination pe numbers and count up number of points at each destination pe

  nrecv=0
  if(nstrings.gt.0) then
   do i=1,nstrings
    mpe=labelijk(i)
    nrecv(mpe)=nrecv(mpe)+i2filter(4,i)
    label_string(5,i)=mpe
   end do
  end if
  if(npes.eq.1) then
   npoints_recv=nrecv
  else
   call mpi_allreduce(nrecv,npoints_recv,npes,mpi_integer4,mpi_sum,mpi_comm_world,ierr)
  end if

end subroutine string_label

subroutine my_gatherv8(local,nlocal,global,nglobal,nrecv,ndrecv,mype,npes)

!    workaround for possible problem with mpi_gatherv failure when nlocal = 0 for some processors

integer(i_long) nlocal,mype,npes,nglobal
integer(i_llong) local(max(1,nlocal))
integer(i_llong) global(max(1,nglobal))
integer(i_long) nrecv(0:npes-1),ndrecv(0:npes)

integer(i_long) nrecv1(0:npes-1),ndrecv1(0:npes)
integer(i_long) i,n,nlocal1,ierr
integer(i_llong) local1(max(1,nlocal))
integer(i_llong) global1(nglobal+npes+1)

do i=0,npes-1
 nrecv1(i)=max(nrecv(i),1)
end do
ndrecv1(0)=0
do i=1,npes
 ndrecv1(i)=ndrecv1(i-1)+nrecv1(i-1)
end do
local1(1)=0
if(nlocal.gt.0) then
 do i=1,nlocal
  local1(i)=local(i)
 end do
end if
nlocal1=max(1,nlocal)
call mpi_gatherv(local1,nlocal1,mpi_integer8,global1,nrecv1,ndrecv1,mpi_integer8,0,mpi_comm_world,ierr)
do n=0,npes-1
  if(nrecv(n).gt.0) then
    do i=1,nrecv(n)
      global(i+ndrecv(n))=global1(i+ndrecv1(n))
    end do
  end if
end do

end subroutine my_gatherv8
subroutine my_scatterv8(global,nglobal,local,nlocal,nrecv,ndrecv,mype,npes)

!    workaround for possible problem with mpi_scatterv failure when nlocal = 0 for some processors

integer(i_long) nlocal,mype,npes,nglobal
integer(i_llong) local(max(1,nlocal))
integer(i_llong) global(max(1,nglobal))
integer(i_long) nrecv(0:npes-1),ndrecv(0:npes)

integer(i_long) nrecv1(0:npes-1),ndrecv1(0:npes)
integer(i_long) i,n,nlocal1,ierr
integer(i_llong) local1(max(1,nlocal))
integer(i_llong) global1(nglobal+npes+1)

do i=0,npes-1
 nrecv1(i)=max(nrecv(i),1)
end do
ndrecv1(0)=0
do i=1,npes
 ndrecv1(i)=ndrecv1(i-1)+nrecv1(i-1)
end do
do n=0,npes-1
  if(nrecv(n).gt.0) then
    do i=1,nrecv(n)
      global1(i+ndrecv1(n))=global(i+ndrecv(n))
    end do
  else
    global1(1+ndrecv1(n))=0
  end if
end do
nlocal1=max(1,nlocal)
call mpi_scatterv(global1,nrecv1,ndrecv1,mpi_integer8,local1,nlocal1,mpi_integer8,0,mpi_comm_world,ierr)
if(nlocal.gt.0) then
 do i=1,nlocal
  local(i)=local1(i)
 end do
else
 local(1)=0
end if

end subroutine my_scatterv8

subroutine what_color_is(i1,i2,i3,color)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    what_color_is
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     i1, i2, i3 -          
!
!   output argument list:
!     color      -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

implicit none
integer(i_long),intent(IN):: i1,i2,i3
integer(i_long),intent(OUT):: color
integer(i_long),dimension(3):: v,vh,vh2,b124
logical same
integer(i_long):: i,itest
data b124/1,2,4/
!----------------------------------------------------------------
vh(1)=i1; vh(2)=i2; vh(3)=i3
do itest=1,20
   v=vh; vh=v/2; vh2=vh*2
!  if(.NOT.same(vh2,v,3))exit
   same=.true. ; do i=1,3; if(vh2(i).ne.v(i)) same=.false. ; enddo
   if(.not.same) exit
enddo
v=modulo(v,2)
color=dot_product(v,b124)
end subroutine what_color_is

end module raflib


SUBROUTINE EIGEN(A,R,N,MV)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    EIGEN
!
!   prgrmmr:     R. J. Purser, NCEP 2005 
!
! abstract:  COMPUTE EIGENVALUES AND EIGENVECTORS OF A REAL SYMMETRIC
!            MATRIX
!
!        REMARKS
!           ORIGINAL MATRIX A MUST BE REAL SYMMETRIC (STORAGE MODE=1)
!           MATRIX A CANNOT BE IN THE SAME LOCATION AS MATRIX R
!
!        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
!           NONE
!
!        METHOD
!           DIAGONALIZATION METHOD ORIGINATED BY JACOBI AND ADAPTED
!           BY VON NEUMANN FOR LARGE COMPUTERS AS FOUND IN 'MATHEMATICAL
!           METHODS FOR DIGITAL COMPUTERS', EDITED BY A. RALSTON AND
!           H.S. WILF, JOHN WILEY AND SONS, NEW YORK, 1962, CHAPTER 7
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     A - ORIGINAL MATRIX (SYMMETRIC), DESTROYED IN COMPUTATION.
!         RESULTANT EIGENVALUES ARE DEVELOPED IN DIAGONAL OF
!         MATRIX A IN DESCENDING ORDER.
!     R - RESULTANT MATRIX OF EIGENVECTORS (STORED COLUMNWISE,
!         IN SAME SEQUENCE AS EIGENVALUES)
!     N - ORDER OF MATRICES A AND R
!     MV- INPUT CODE
!         0   COMPUTE EIGENVALUES AND EIGENVECTORS
!         1   COMPUTE EIGENVALUES ONLY (R NEED NOT BE
!             DIMENSIONED BUT MUST STILL APPEAR IN CALLING SEQUENCE)
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

!     REAL(4) A(1),R(1)
!
!        ...............................................................
!
!        IF A DOUBLE PRECISION VERSION OF THIS ROUTINE IS DESIRED, THE
!        C IN COLUMN 1 SHOULD BE REMOVED FROM THE DOUBLE PRECISION
!        STATEMENT WHICH FOLLOWS.
!
      REAL(8) A(1),R(1),ANORM,ANRMX,THR,X,Y,SINX,SINX2,COSX, &
                       COSX2,SINCS,RANGE
!
!        THE C MUST ALSO BE REMOVED FROM DOUBLE PRECISION STATEMENTS
!        APPEARING IN OTHER ROUTINES USED IN CONJUNCTION WITH THIS
!        ROUTINE.
!
!        THE DOUBLE PRECISION VERSION OF THIS SUBROUTINE MUST ALSO
!        CONTAIN DOUBLE PRECISION FORTRAN FUNCTIONS.  SQRT IN STATEMENTS
!        40, 68, 75, AND 78 MUST BE CHANGED TO DSQRT.  ABS IN STATEMENT
!        62 MUST BE CHANGED TO DABS. THE CONSTANT IN STATEMENT 5 SHOULD
!        BE CHANGED TO 1.0D-12.
!
!        ...............................................................
!
!        GENERATE IDENTITY MATRIX
!
    5 continue
      RANGE=1.0E-12_8
      if(mv.eq.1) go to 25
      IQ=-N
      DO J=1,N
       IQ=IQ+N
       DO I=1,N
        IJ=IQ+I
        R(IJ)=0.0
        if(i.ne.j) go to 20
        R(IJ)=1.0_8
   20   CONTINUE
       end do
      end do
!
!        COMPUTE INITIAL AND FINAL NORMS (ANORM AND ANORMX)
!
   25 continue
      ANORM=0.0_8
      DO I=1,N
       DO J=I,N
        if(i.eq.j) go to 35
        IA=I+(J*J-J)/2
        ANORM=ANORM+A(IA)*A(IA)
   35   CONTINUE
       end do
      end do
      if(anorm.le.0.) go to 165
      ANORM=1.414_8*SQRT(ANORM)
      ANRMX=ANORM*RANGE/FLOAT(N)
!
!        INITIALIZE INDICATORS AND COMPUTE THRESHOLD, THR
!
      IND=0
      THR=ANORM
   45 continue
      THR=THR/FLOAT(N)
   50 continue
      L=1
   55 continue
      M=L+1
!
!        COMPUTE SIN AND COS
!
   60 continue
      MQ=(M*M-M)/2
      LQ=(L*L-L)/2
      LM=L+MQ
   62 continue
      if(abs(a(lm))-thr.lt.0._8) go to 130
      IND=1
      LL=L+LQ
      MM=M+MQ
      X=0.5_8*(A(LL)-A(MM))
   68 continue
      Y=-A(LM)/ SQRT(A(LM)*A(LM)+X*X)
      if(x.ge.0._8) go to 75
      Y=-Y
!DP75 SINX=Y/ SQRT(2.0*(1.0+( SQRT(1.0-Y*Y))))
   75 continue
      SINX=Y/ SQRT(2.0_8*(1.0_8+( SQRT(MAX(0._8,1.0_8-Y*Y)))))
      ONEMYY=1.0_8-Y*Y
      IF(1.0_8-Y*Y.LT.0._8) write(6,*)' IN EIGEN, 1-Y*Y=',ONEMYY
      SINX2=SINX*SINX
!DP78 COSX= SQRT(1.0-SINX2)
   78 continue
      COSX= SQRT(MAX(0._8,1.0_8-SINX2))
      COSX2=COSX*COSX
      SINCS =SINX*COSX
!
!        ROTATE L AND M COLUMNS
!
      ILQ=N*(L-1)
      IMQ=N*(M-1)
      DO 125 I=1,N
       IQ=(I*I-I)/2
       if(i.eq.l) go to 115
       if(i.eq.m) go to 115
       if(i.gt.m) go to 90
       IM=I+MQ
       GO TO 95
    90 continue
       IM=M+IQ
    95 continue
       if(i.ge.l) go to 105
       IL=I+LQ
       GO TO 110
   105 continue
       IL=L+IQ
   110 continue
       X=A(IL)*COSX-A(IM)*SINX
       A(IM)=A(IL)*SINX+A(IM)*COSX
       A(IL)=X
   115 continue
       if(mv.eq.1) go to 125
       ILR=ILQ+I
       IMR=IMQ+I
       X=R(ILR)*COSX-R(IMR)*SINX
       R(IMR)=R(ILR)*SINX+R(IMR)*COSX
       R(ILR)=X
  125 CONTINUE
      X=2.0_8*A(LM)*SINCS
      Y=A(LL)*COSX2+A(MM)*SINX2-X
      X=A(LL)*SINX2+A(MM)*COSX2+X
      A(LM)=(A(LL)-A(MM))*SINCS+A(LM)*(COSX2-SINX2)
      A(LL)=Y
      A(MM)=X
!
!        TESTS FOR COMPLETION
!
!        TEST FOR M = LAST COLUMN
!
  130 continue
      if(m.eq.n) go to 140
      M=M+1
      GO TO 60
!
!        TEST FOR L = SECOND FROM LAST COLUMN
!
  140 continue
      if(l.eq.n-1) go to 150
      L=L+1
      GO TO 55
  150 continue
      if(ind.ne.1) go to 160
      IND=0
      GO TO 50
!
!        COMPARE THRESHOLD WITH FINAL NORM
!
  160 continue
      if(thr.gt.anrmx) go to 45
!
!        SORT EIGENVALUES AND EIGENVECTORS
!
  165 continue
      IQ=-N
      DO I=1,N
       IQ=IQ+N
       LL=I+(I*I-I)/2
       JQ=N*(I-2)
       DO J=I,N
        JQ=JQ+N
        MM=J+(J*J-J)/2
        if(a(ll).ge.a(mm)) go to 1970
        X=A(LL)
        A(LL)=A(MM)
        A(MM)=X
        if(mv.eq.1) go to 1970
        DO K=1,N
         ILR=IQ+K
         IMR=JQ+K
         X=R(ILR)
         R(ILR)=R(IMR)
         R(IMR)=X
        end do
1970    continue
       end do
      end do
      RETURN
      END


SUBROUTINE gettri4(us,lguess,lv,lui,w4)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    gettri4
!
!   prgrmmr:     Purser 2003
!
! abstract:  Blended 4-color triads with bridging function of 2nd degree
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     US:      3-vector comprising components of the target aspect tensor
!     LGUESS:  Code to tell whether input LTRIAD are feasible (LGUESS.NE.0)
!     LV:      4 integer basis 2-vectors giving the canonical grid steps
!     LUI:     3 3-vectors dual to those of LU: [LUI]^t*[LU]=[I]
!                where LU are the aspect vectors of the first three of LV.
!
!   output argument list:
!     LUI:     3 3-vectors dual to those of LU: [LUI]^t*[LU]=[I]
!                where LU are the aspect vectors of the first three of LV.
!     W4:      4 real "spread" weights, in generalized grid-step units, that
!                correspond to the 4 grid-step generators, LV.
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

IMPLICIT NONE
REAL(8),DIMENSION(3),  INTENT(IN   ):: us
INTEGER,               INTENT(IN   ):: lguess
INTEGER,DIMENSION(2,4),INTENT(INOUT):: lv
INTEGER,DIMENSION(3,3),INTENT(INOUT):: lui
REAL(8),DIMENSION(4)  ,INTENT(  OUT):: w4
!-----------------------------------------------------------------------------
REAL(8)                             :: c,aoc,boc,d,dlim
REAL(8),DIMENSION(3)                :: v
REAL(8),DIMENSION(4)                :: w4c
REAL(8),DIMENSION(3,3)              :: b123
REAL(8),DIMENSION(4,3)              :: w4l
INTEGER                             :: kt
DATA b123/.5d0,0.d0,.5d0,  -.5d0,0.d0,.5d0,  0.d0,1.d0,1.d0/
DATA w4c/ 1.d0,1.d0,-.5d0,-.5d0/
DATA w4l/ 1.d0,-1.d0,0.d0,0.d0, 0.d0,0.d0,.5d0,-.5d0, -1.d0,-1.d0,1.d0,1.d0/
!=============================================================================
CALL gettri3(us,lguess,lv(:,1:3),lui,v,kt)
lv(:,4)=lv(:,1)-lv(:,2)
v=MATMUL(b123,v)
c=v(3)
aoc=v(1)/c
boc=v(2)/c
d=boc/(2-boc)
dlim=(1-abs(aoc))/(3+abs(aoc))
IF(d<dlim)THEN
   v(3)=(2+dlim+d*d/dlim)*.25d0
ELSE
   v(3)=(1+d)*.5d0
ENDIF
v(1)=aoc*v(3)
v(2)=boc*v(3)
c=c/v(3)
w4=(w4c+MATMUL(w4l,v))*c
END SUBROUTINE gettri4


SUBROUTINE gettri3(utarget,lguess,ltriad,lui,wtriad,kt)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    gettri3
!
!   prgrmmr:     Purser 1997
!
! abstract:
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     UTARGET: 3-vector comprising components of the target aspect tensor
!     LGUESS:  Code to tell whether input LTRIAD are feasible (LGUESS.NE.0)
!     LTRIAD:  3 integer basis 2-vectors giving the canonical grid steps
!     LUI:     3 3-vectors dual to those of LU: [LUI]^t*[LU]=[I]
!
!   output argument list:
!     LTRIAD:  3 integer basis 2-vectors giving the canonical grid steps
!     LUI:     3 3-vectors dual to those of LU: [LUI]^t*[LU]=[I]
!     WTRIAD:  3 real "spread" components in generalized grid-step units
!     KT:      The number of iterations it required to find the valid triad
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

IMPLICIT NONE
REAL(8),DIMENSION(3),  INTENT(IN   ):: utarget
INTEGER,               INTENT(IN   ):: lguess
INTEGER,DIMENSION(2,3),INTENT(INOUT):: ltriad
INTEGER,DIMENSION(3,3),INTENT(INOUT):: lui
REAL(8),DIMENSION(3),  INTENT(OUT  ):: wtriad
INTEGER,               INTENT(OUT  ):: kt
!-----------------------------------------------------------------------------
INTEGER,DIMENSION(2,3):: itriad
INTEGER,DIMENSION(3,3):: ilui,nj
INTEGER,DIMENSION(3)  :: luil,kl,ml
REAL(8),PARAMETER     :: bcmins=-1.d-14
REAL(8)               :: u,wl
INTEGER               :: i,j,k,l,m,n,it
DATA itriad/ 1, 0,  0,1,	-1,-1/
DATA kl/3,1,2/,ml/2,3,1/,nj/-2,2,2,2,-2,2,2,2,-2/
DATA ilui/1, 0,-1,  0, 1,-1,  0, 0, 1/
!=============================================================================
IF(lguess==0)THEN
   DO j=1,3
      DO i=1,2
	 ltriad(i,j)=itriad(i,j)
      ENDDO
      DO i=1,3
	 lui(i,j)=ilui(i,j)
      ENDDO
   ENDDO
ENDIF

! Use initial estimate of triad to compute implied weights directly.
! (Subsequent updates of these weights are done perturbatively to save time).
DO i=1,3
   wtriad(i)=0.
ENDDO
DO i=1,3
   u=utarget(i)
   DO j=1,3
      wtriad(j)=wtriad(j)+lui(i,j)*u
   ENDDO
ENDDO

DO it=1,4000       !  this should be ample
   DO l=1,3
      IF(wtriad(l)<bcmins)THEN
	 k=kl(l)
	 m=ml(l)
	 DO i=1,2
            ltriad(i,l)=ltriad(i,m)-ltriad(i,k)
            ltriad(i,m)=-ltriad(i,m)
	 ENDDO
	 DO i=1,3
            luil(i)=lui(i,l)
	 ENDDO
	 wl=wtriad(l)
	 DO j=1,3
            n=nj(j,l)
            DO i=1,3
               lui(i,j)=lui(i,j)+luil(i)*n
            ENDDO
            wtriad(j)=wtriad(j)+wl*n
	 ENDDO
	 GOTO 300
      ENDIF
   ENDDO
   kt=it		   !  report back how many iterations were needed
! Rotate triad so that smallest wtriad is associated with third member:
   i=1
   IF(wtriad(2)<wtriad(1))i=2
   IF(wtriad(3)>wtriad(i))THEN
      DO j=1,2
         l=ltriad(j,i)
         ltriad(j,i)=ltriad(j,3)
         ltriad(j,3)=l
      ENDDO
      DO j=1,3
         l=lui(j,i)
         lui(j,i)=lui(j,3)
         lui(j,3)=l
      ENDDO
      wl=wtriad(i)
      wtriad(i)=wtriad(3)
      wtriad(3)=wl
   ENDIF
   RETURN
300 CONTINUE
ENDDO
write(6,*)'GETTRI3:  ALL 40 ITERATIONS USED UP.  This should never happen'
call stop2(68)
END SUBROUTINE gettri3


SUBROUTINE getcol4(lv,color)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    getcol4
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     lv    -
!
!   output argument list:
!     color -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

IMPLICIT NONE
INTEGER,DIMENSION(2,4),INTENT(IN ):: lv
INTEGER,DIMENSION(4)  ,INTENT(OUT):: color
!----------------------------------------------------------------------------
INTEGER                           :: k
!=============================================================================
DO k=1,4
   CALL what_color_is_triad(lv(1,k),lv(2,k),color(k),3)
ENDDO
END SUBROUTINE getcol4


SUBROUTINE what_color_is_triad(i1,i2,color,p)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    what_color_is_triad
!
!   prgrmmr:     R. J. Purser, NCEP, August 2001
!
! abstract:  Find the color index associated with the given grid displacement vector
! for the chromatic triad of type "p", where p is prime.
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     i1,i2   - displacement indices in x, y.
!     p       - is the "chromatic prime index", presently one of {2, 3, 5}.
!
!   output argument list:
!     color   - color index in the range [1, (p +1)], where,
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

IMPLICIT NONE
INTEGER,INTENT(IN )   :: i1,i2,p
INTEGER,INTENT(OUT)   :: color
!----------------------------------------------------------------
INTEGER,DIMENSION(2)  :: v,vf,vfp,bxy2,bxy3,bxy5
INTEGER,DIMENSION(8) :: color3
INTEGER,DIMENSION(24):: color5
LOGICAL same4
INTEGER:: itest
DATA bxy2/1,2/,bxy3/1,3/,bxy5/1,5/
DATA color3 &
/ 1,1,  2, 3, 4,  2, 4, 3/
DATA color5                                                                   &
/  1, 1, 1, 1,  2, 3, 4, 5, 6,  2, 5, 3, 6, 4,  2, 4, 6, 3, 5,  2, 6, 5, 4, 3/
!=============================================================================
vf(1)=i1; vf(2)=i2
DO itest=1,20
   v=vf; vf=v/p; vfp=vf*p; IF(.NOT.same4(vfp,v,2))EXIT
ENDDO
v=MODULO(v,p)
SELECT CASE(p)
CASE (2)
   color=dot_PRODUCT(v,bxy2)
CASE (3)
   color=color3(dot_PRODUCT(v,bxy3))
CASE (5)
   color=color5(dot_PRODUCT(v,bxy5))
CASE default
   write(6,*)'WHAT_COLOR_IS_TRIAD:  ***PROBLEM***  no tabulation available for p-value supplied'
   call stop2(68)
END SELECT
END SUBROUTINE what_color_is_triad


FUNCTION same4(v1,v2,n)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    same4
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     v1, v2   -
!     n        -
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

LOGICAL                        :: same4
INTEGER,             INTENT(IN):: n
INTEGER,DIMENSION(n),INTENT(IN):: v1,v2
INTEGER                        :: i
!=============================================================================
same4=.TRUE.
DO i=1,n; IF(v1(i) /= v2(i))same4=.FALSE.; ENDDO
END FUNCTION same4


