module convthin
!$$$   module documentation block
!                .      .    .                                   .
! module:  convthin
!  prgmmr:
!
! abstract:
!
! program history log:
!   2008-06-04  safford - add module doc block
!
! subroutines included:
!   make3grids
!   map3grids
!   del3grids
!
! variable definitions:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  implicit none

! set default to private
  private
! set subroutines to public
  public :: make3grids
  public :: map3grids
  public :: del3grids
! set passed variables to public
  public :: use_all

  integer(i_kind):: mlat
  integer(i_kind),allocatable,dimension(:):: mlon
  integer(i_kind),allocatable,dimension(:,:):: icount,ibest_obs

  real(r_kind),allocatable,dimension(:):: glat
  real(r_kind),allocatable,dimension(:,:):: glon,hll,score_crit
  logical use_all

contains

  subroutine make3grids(rmesh,nlevp)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    make3grids                            
!     prgmmr:    treadon     org: np23                date: 2002-10-17
!
! abstract:  This routine sets up dimensions for and allocates
!            thinning grids.
!
! program history log:
!   2002-10-17  treadon
!   2004-06-22  treadon - update documentation
!   2004-12-09  treadon - allocate thinning grids consistent with analysis domain
!   2006-01-27  kistler - added vertical dimension
!   2007-11-03       su - added vertical p level array 
!   2008-06-04  safford - rm unused vars and uses
!
!   input argument list:
!     rmesh - mesh size (km) of thinning grid.  If (rmesh <= one), 
!             then no thinning of the data will occur.  Instead,
!             all data will be used without thinning.
!     nlevp -  vertical levels
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use constants, only: izero,ione,rearth_equator,two,deg2rad,zero,half,one,pi
    use satthin, only:dlat_grid,dlon_grid,rlat_min,rlon_min

    implicit none

    real(r_kind),parameter:: r360 = 360.0_r_kind

    real(r_kind),intent(in):: rmesh
    integer(i_kind),intent(in):: nlevp

    integer(i_kind) i,j
    integer(i_kind) mlonx,mlonj,itxmax

    real(r_kind) delonx,delat,dgv,halfpi,dx,dy
    real(r_kind) twopi
    real(r_kind) factor,delon
    real(r_kind) rkm2dg,glatm

!   If there is to be no thinning, simply return to calling routine
    use_all=.false.
    if(abs(rmesh) <= one)then
       use_all=.true.
       itxmax=2.e6_i_kind
       return
    end if

!   Set constants
    halfpi = half*pi
    twopi  = two*pi
    rkm2dg = r360/(twopi*rearth_equator)*1.e3_r_kind

!   Set up dimensions and allocate arrays for thinning grids
!	horizontal
    if (rmesh<zero) rkm2dg=one
    dx    = rmesh*rkm2dg
    dy    = dx
    mlat  = dlat_grid/dy + half
    mlonx = dlon_grid/dx + half
	delat = dlat_grid/mlat
	delonx= dlon_grid/mlonx
    dgv  = delat*half
    mlat=max(2_i_kind,mlat);   mlonx=max(2_i_kind,mlonx)

    allocate(mlon(mlat),glat(mlat),glon(mlonx,mlat),hll(mlonx,mlat))


!   Set up thinning grid lon & lat.  The lon & lat represent the location of the
!   lower left corner of the thinning grid box.
    itxmax=izero
    do j = 1,mlat
       glat(j) = rlat_min + (j-ione)*delat
       glat(j) = glat(j)*deg2rad
       glatm = glat(j) + dgv*deg2rad
       
       factor = abs(cos(abs(glatm)))
       if (rmesh>zero) then
          mlonj   = nint(mlonx*factor)	
          mlon(j) = max(2_i_kind,mlonj)
          delon = dlon_grid/mlon(j)
       else
          delon = factor*rmesh
          delon = min(delon,r360)
          mlon(j) = dlon_grid/delon
       endif
       
       glat(j) = min(max(-halfpi,glat(j)),halfpi)
       do i = 1,mlon(j)
          itxmax=itxmax+ione
          hll(i,j)=itxmax
          glon(i,j) = rlon_min + (i-ione)*delon
          glon(i,j) = glon(i,j)*deg2rad
          glon(i,j) = min(max(zero,glon(i,j)),twopi)
       enddo
       
    end do

!   Allocate  and initialize arrays
    allocate(icount(itxmax,nlevp))
    allocate(ibest_obs(itxmax,nlevp))
    allocate(score_crit(itxmax,nlevp))

    do j=1,nlevp
       do i=1,itxmax
          icount(i,j) = izero
          ibest_obs(i,j)= izero
          score_crit(i,j)= 9.99e6_r_kind
       end do
    end do

    return
  end subroutine make3grids

  subroutine map3grids(pflag,pcoord,nlevp,dlat_earth,dlon_earth,pob,crit1,ithin,iobs,iobsout,iuse)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    map3grids
!     prgmmr:    treadon     org: np23                date: 2002-10-17
!
! abstract:  This routine maps convential observations to a 3d thinning grid.
!
! program history log:
!   2002-10-17  treadon
!   2004-06-22  treadon - update documentation
!   2004-07-23  derber - modify code to thin obs as read in
!   2004-12-08  li, xu - fix bug --> set iuse=.true. when use_all=.true.
!   2005-10-14  treadon - variable name change (dlat0,dlon0) --> d*_earth
!   2006-01-25  kistler - extend 2d to 3d 
!   2008-06-04  safford - rm unused vars
!
!   input argument list:
!     pflag - type of pressure-type levels; 0 : sigma level, 1 : determined by convinfo file
!     pcoord     - veritical coordinate values
!     nlevp       - number of vertical levels
!     dlat_earth - earth relative observation latitude (radians)
!     dlon_earth - earth relative observation longitude (radians)
!     pob        - observation pressure ob
!     crit1      - quality indicator for observation (smaller = better)
!     ithin      - number of obs to retain per thinning grid box
!
!   output argument list:
!     iobs  - observation counter
!     itx   - combined (i,j) index of observation on thinning grid
!     itt   - superobs thinning counter
!     iobsout- location for observation to be put
!     ip    - vertical index
!     iuse  - .true. if observation should be used
!     
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use constants, only: ione,izero,one, half,two,three,ione
    implicit none
    
    logical ,intent(out):: iuse
    integer(i_kind),intent(in):: ithin,nlevp,pflag
    integer(i_kind),intent(inout):: iobs
    integer(i_kind),intent(out):: iobsout
    real(r_kind),intent(in):: dlat_earth,dlon_earth,crit1,pob
    real(r_kind),dimension(nlevp),intent(in):: pcoord
    
    integer(i_kind):: ip,itt,itx
    integer(i_kind) ix,iy
    integer(i_kind),dimension(0:51):: istart_val

    real(r_kind) dlat1,dlon1,pob1
    real(r_kind) dx,dy,dp,dxx,dyy,dpp
    real(r_kind) crit,dist1


!   If using all data (no thinning), simply return to calling routine
    if(use_all)then
       iuse=.true.
       iobs=iobs+ione
       iobsout=iobs
       return
    end if

!   Compute (i,j,k) indices of coarse mesh grid (grid number 1) which 
!   contains the current observation.
    dlat1=dlat_earth
    dlon1=dlon_earth
    pob1=pob

    call grdcrd(pob1,ione,pcoord,nlevp,-ione)
    ip=int(pob1)
    dp=pob1-ip
    ip=max(ione,min(ip,nlevp))
    
    call grdcrd(dlat1,ione,glat,mlat,ione)
    iy=int(dlat1)
    dy=dlat1-iy
    iy=max(ione,min(iy,mlat))
    
    call grdcrd(dlon1,ione,glon(1,iy),mlon(iy),ione)
    ix=int(dlon1)
    dx=dlon1-ix
    ix=max(ione,min(ix,mlon(iy)))
    
    dxx=half-min(dx,one-dx)
    dyy=half-min(dy,one-dy)
    if( pflag == ione) then 
       dpp=half-min(dp,one-dp)
    else
       dpp=min(dp,one-dp)
    endif

    itx=hll(ix,iy)
    itt=istart_val(ithin)+itx
    if(ithin == izero) itt=izero

!   Compute distance metric (smaller is closer to center of cube)
    dist1=(dxx*dxx+dyy*dyy+dpp*dpp)*two/three+half


!   Examine various cases regarding what to do with current obs.
!   Start by assuming observation will be selected.  
    iuse=.true.

!   Determine "score" for observation.  Lower score is better.
    crit = crit1*dist1

!   Case:  obs score > best value at this location, 
!     -->  do not use this obs, return to calling program.
    if(crit > score_crit(itx,ip) .and. icount(itx,ip) > izero) then
       iuse=.false.
       return

!   Case:  obs score < best value at this location, 
!     -->  update score, count, and best obs counters
    elseif (icount(itx,ip) > izero .and. crit < score_crit(itx,ip)) then
       score_crit(itx,ip)= crit
       iobsout=ibest_obs(itx,ip)
       icount(itx,ip)=icount(itx,ip)+ione

!   Case:  first obs at this location, 
!     -->  keep this obs as starting point
    elseif (icount(itx,ip)==izero) then
       iobs=iobs+ione
       iobsout=iobs
       score_crit(itx,ip)= crit
       ibest_obs(itx,ip) = iobs
       icount(itx,ip)=icount(itx,ip)+ione

!   Case:  none of the above cases are satisified, 
!     -->  don't use this obs
    else
       iuse = .false.
    end if

    return
  end subroutine map3grids


  subroutine del3grids
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    del3grids                            
!     prgmmr:    kistler     org: np23                date: 2006-01-25
!
! abstract:  This routine deallocates arrays used in 3d thinning
!
! program history log:
!   2006-01-25  kistler - original routine
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none

    if (.not.use_all) then
       deallocate(mlon,glat,glon,hll)
       deallocate(icount)
       deallocate(ibest_obs)
       deallocate(score_crit)
    endif
  end subroutine del3grids

end module convthin
