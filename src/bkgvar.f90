subroutine bkgvar(cstate,sst,slndt,sicet,iflg)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    bkgvar      apply background error variances
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract: apply latitudinal background error variances & manipulate
!           skin temp <--> sst,sfc temp, and ice temp fields
!
! program history log:
!   1990-10-06  parrish
!   2004-08-24  kleist - hoper & htoper replaced
!   2004-11-16  treadon - add longitude dimension to variance array dssv
!   2004-11-22  derber - modify for openMP
!   2005-01-22  parrish - add "use balmod"
!   2005-07-14  wu - add max bound to l2
!   2007-03-13  derber - modify to allow use qvar3d array
!   2007-07-03  kleist - add full 2d error array for surface pressure (global only)
!   2007-11-26  s.liu - correct bug in water point skin temperature variances
!   2010-03-01  zhu   - replace explicit use of each control variable by one array
!                       'cstate' and use nrf* for generalized control variable
!                     - merge global and regional cases
!
!   input argument list:
!     t        - t grid values
!     p        - p surface grid values
!     q        - q grid values
!     oz       - ozone grid values
!     skint    - skin temperature grid values
!     cwmr     - cloud water mixing ratio grid values
!     st       - streamfunction grid values
!     vp       - velocity potential grid values
!     sst      - sst grid values
!     slndt    - land surface temperature grid values
!     sicet    - snow/ice covered surface temperature grid values
!     iflg     - flag for skin temperature manipulation
!                0: skint --> sst,slndt,sicet
!                1: sst,slndt,sicet --> skint
!
!   output argument list:
!     t        - t grid values
!     p        - p surface grid values
!     q        - q grid values
!     oz       - ozone grid values
!     skint    - skin temperature grid values
!     cwmr     - cloud water mixing ratio grid values
!     st       - streamfunction grid values
!     vp       - velocity potential grid values
!     sst      - sst grid values
!     slndt    - land surface temperature grid values
!     sicet    - snow/ice covered surface temperature grid values
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: izero,ione,one
  use balmod, only: rllat1,llmax
  use berror, only: dssv,dssvs
  use gridmod, only: nsig,regional,lat2,lon2
  use guess_grids, only: isli2
  use jfunc, only: nval_levs
  use control_vectors, only: nrf3,nrf2,nrf2_sst,nrf3_q,nrf2_loc,nrf3_loc,nrf_levb
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: iflg
  real(r_kind),dimension(lat2,lon2,nval_levs),intent(inout) :: cstate
  real(r_kind),dimension(lat2,lon2),intent(inout) :: sst,slndt,sicet

! Declare local variables
  integer(i_kind) i,j,k,n,nk,l,l2,loc,nn
  real(r_kind) dl1,dl2

! Multipy by variances
!$omp parallel do  schedule(dynamic,1) private(n,nn,nk,k,i,j,loc)
  do nn=1,nrf3+nrf2
! 3-D fields
     if(nn <= nrf3)then
       n=nn
       loc=nrf3_loc(n)
       nk=nrf_levb(loc)
       do k=1,nsig
          do i=1,lon2
             do j=1,lat2
                cstate(j,i,nk)  =cstate(j,i,nk)*dssv(j,i,k,n)
             end do
          enddo
          nk=nk+1
       enddo
     else

! Surface fields

       n=nn-nrf3
       loc=nrf2_loc(n)
       nk=nrf_levb(loc)
       if (n/=nrf2_sst) then
          do i=1,lon2
             do j=1,lat2
                cstate(j,i,nk)=cstate(j,i,nk)*dssvs(j,i,n)
             end do
          end do
       else
          if (iflg == izero) then
!         Break skin temperature into components
              do i=1,lon2
                 do j=1,lat2
                    if(isli2(j,i) == ione) then
                       slndt(j,i)=cstate(j,i,nk)*dssvs(j,i,nrf2+1)
                    else if(isli2(j,i) == 2_i_kind) then
                       sicet(j,i)=cstate(j,i,nk)*dssvs(j,i,nrf2+2)
                    else
                       sst(j,i)  =cstate(j,i,nk)*dssvs(j,i,n)
                    end if
                 end do
              end do
           else
!          Combine sst,slndt, and sicet into skin temperature field
              do i=1,lon2
                 do j=1,lat2
                    if(isli2(j,i) == ione) then
                       cstate(j,i,nk)=slndt(j,i)*dssvs(j,i,nrf2+1)
                    else if(isli2(j,i) == 2_i_kind) then
                       cstate(j,i,nk)=sicet(j,i)*dssvs(j,i,nrf2+2)
                    else
                       cstate(j,i,nk)=sst(j,i)*dssvs(j,i,n)
                    end if
                 end do
              end do
           end if
       end if
     end if
  end do

  return
end subroutine bkgvar
