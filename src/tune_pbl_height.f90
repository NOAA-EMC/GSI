subroutine tune_pbl_height(mype,station_id,dx,dy,prestsfc,thisPBL_height,diffsfc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tune_pbl_height
!   prgmmr: Ming Hu          org: GSD                date: 2011-06-22
!
! abstract: tune PBL height based on surface temperature residue
!           n levs
!
! program history log:
!
!   input argument list:
!     dx,dy    - input x,y,z-coords of interpolation points (grid units)
!     mype     - mpi task id
!     prestsfc - surface pressure
!     thisPBL_height - PBL heigh
!     diffsfc - surface temperature residue
!
!   output argument list:
!     g        - output interpolatees
!
! attributes:
!   language: f90
!   machine:  JET
!
!$$$



  use kinds, only: r_kind,r_single,r_double,i_kind
  use gridmod, only: istart,jstart,nlon,nlat,lon1,lon2,lat2
  use constants, only: zero, one,rd_over_cp,ten
  use guess_grids, only: pbl_height, ges_ps
  implicit none

!
! Declare passed variables
  integer(i_kind)                              ,intent(in   ) :: mype
  character(8)                                 ,intent(in   ) :: station_id
  real(r_kind)                                 ,intent(in   ) :: dx,dy
  real(r_kind)                                 ,intent(in   ) :: prestsfc
  real(r_kind)                                 ,intent(inout) :: thisPBL_height
  real(r_kind)                                 ,intent(in   ) :: diffsfc

! Declare local variables
  real(r_kind) :: pblfact_cool
  integer(i_kind) m1,i,ix1,iy1,ix,ixp,iyp,iy,itime
  real(r_kind) :: oldPBL_height

  oldPBL_height=thisPBL_height
  m1=mype+1
  ix1=int(dx)
  iy1=int(dy)
  ix1=max(1,min(ix1,nlat))

  ix=ix1-istart(m1)+2_i_kind
  iy=iy1-jstart(m1)+2_i_kind
  if(iy<1) then
     iy1=iy1+nlon
     iy=iy1-jstart(m1)+2_i_kind
  end if
  if(iy>lon1+1) then
     iy1=iy1-nlon
     iy=iy1-jstart(m1)+2_i_kind
  end if
  ixp=ix+1; iyp=iy+1
  if(ix1==nlat) then
     ixp=ix
  end if

! --- reduce depth over which pseudo-obs will be
!       created if resid(theta-v) at surface
!        is negative (depending on how large)
!           -1 C - no reduction
!           -4 C - full reduction
  if (diffsfc >= -1.0_r_kind) return 
  pblfact_cool=1.0_r_kind - max(zero,min(one,(-diffsfc-1.0_r_kind)/3.0_r_kind))

  itime=1
  thisPBL_height           = prestsfc - max(zero,pblfact_cool*(prestsfc-thisPBL_height))
  pbl_height(ix,iy,itime)  = ges_ps(ix,iy,itime)*ten - max(zero, &
                             pblfact_cool*(ges_ps(ix,iy,itime)*ten-pbl_height(ix,iy,itime)))
  pbl_height(ixp,iy,itime) = ges_ps(ixp,iy,itime)*ten - max(zero,& 
                             pblfact_cool*(ges_ps(ixp,iy,itime)*ten-pbl_height(ixp,iy,itime)))
  pbl_height(ix,iyp,itime) = ges_ps(ix,iyp,itime)*ten - max(zero,&
                             pblfact_cool*(ges_ps(ix,iyp,itime)*ten-pbl_height(ix,iyp,itime)))
  pbl_height(ixp,iyp,itime)= ges_ps(ixp,iyp,itime)*ten - max(zero,&
                             pblfact_cool*(ges_ps(ixp,iyp,itime)*ten-pbl_height(ixp,iyp,itime)))
  write(*,'(a,a8,10f10.2)') 'tuned PBL height=', station_id,diffsfc,pblfact_cool,prestsfc,thisPBL_height,oldPBL_height 

end subroutine tune_pbl_height
