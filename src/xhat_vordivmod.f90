module xhat_vordivmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   xhat_vordivmod
!  prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-14  lueken - added module doc block
!   2010-04-01  treadon - move strip,reorder,reorder2 to gridmod
!
! subroutines included:
!   sub init_
!   sub clean_
!   sub calc_
!   sub calc2_
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use mpimod, only: iscuv_s,ierror,mpi_comm_world,irduv_s,ircuv_s,&
       isduv_g,iscuv_g,nnnuvlevs,nuvlevs,irduv_g,ircuv_g,mpi_rtype,isduv_s
  use constants, only: zero
  use gridmod, only: lat1,lon1,lat2,lon2,itotsub,nsig,&
       regional,strip,reorder,reorder2
  use compact_diffs, only: uv2vordiv
  use gsi_4dvar, only: nobs_bins
  use state_vectors

  implicit none
  private

  public xhat_vordiv_init
  public xhat_vordiv_calc
  public xhat_vordiv_calc2
  public xhat_vordiv_clean

  interface xhat_vordiv_init;  module procedure init_ ; end interface
  interface xhat_vordiv_calc;  module procedure calc_ ; end interface
  interface xhat_vordiv_calc2; module procedure calc2_; end interface
  interface xhat_vordiv_clean; module procedure clean_; end interface

  real(r_kind),public,allocatable,dimension(:,:,:,:):: xhat_vor
  real(r_kind),public,allocatable,dimension(:,:,:,:):: xhat_div

CONTAINS

subroutine init_
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-14  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  allocate(xhat_vor(lat2,lon2,nsig,nobs_bins))
  allocate(xhat_div(lat2,lon2,nsig,nobs_bins))
end subroutine init_

subroutine clean_
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    clean_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-14  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  deallocate(xhat_div)
  deallocate(xhat_vor)
end subroutine clean_

subroutine calc_(sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    update_guess          add analysis increment to guess
!   prgmmr: todling          org: np22                date: 2007-07-05
!
! abstract:  Caculate increment of vorticity divergence
!
! program history log:
!   2007-07-05  todling - intial code; stripped off from update_guess
!
!   input argument list:
!     sval     - analysis increment in grid space
!
!   output argument list:
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  implicit none

! Declare passed variables
  type(state_vector), intent(in   ) :: sval(nobs_bins)

! Declare local variables
  integer(i_kind) i,j,k,ii
  real(r_kind),dimension(lat1,lon1,nsig):: usm,vsm
  real(r_kind),dimension(itotsub,nuvlevs):: work1,work2

!*******************************************************************************

! Initialize local arrays
  do ii=1,nobs_bins
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              xhat_vor(i,j,k,ii) = zero
              xhat_div(i,j,k,ii) = zero
           end do
        end do
     end do
  end do

! The GSI analyzes stream function (sf) and velocity potential (vp).  
! Wind field observations are in terms of zonal (u) and meridional 
! (v) wind components or wind speed.  Thus, the GSI carries wind 
! increments in terms of both u,v and sf,vp.  
!
! The NCEP GFS (global) model uses vorticity and divergence as
! wind field variable.  The code below converts increments in 
! u and v to those in vorticity and divergence.  The wind variables
! in the NCEP regional model are u and v.  Hence, the block of code
! below is only used for the NCEP GFS (.not.regional). 

! Other users may need to change the logical below to obtain the
! proper behavior for their specific guess (model background)

! For NCEP GFS convert increment in u,v to increments in vor,div
  if (.not.regional) then

     do ii=1,nobs_bins
!       NCEP GFS interface
!       Zero work arrays
        do k=1,nuvlevs
           do j=1,itotsub
              work1(j,k)=zero
              work2(j,k)=zero
           end do
        end do
  
!       Strip off halo for u,v grids on subdomains
        call strip(sval(ii)%u,usm,nsig)
        call strip(sval(ii)%v,vsm,nsig)

!       Put u,v subdomains on global slabs
!       Note:  u --> work1, v --> work2
        call mpi_alltoallv(usm,iscuv_g,isduv_g,&
             mpi_rtype,work1,ircuv_g,irduv_g,mpi_rtype,&
             mpi_comm_world,ierror)
        call mpi_alltoallv(vsm,iscuv_g,isduv_g,&
             mpi_rtype,work2,ircuv_g,irduv_g,mpi_rtype,&
             mpi_comm_world,ierror)

!       Reorder work arrays before converting u,v to vor,div
        call reorder(work1,nuvlevs,nnnuvlevs)
        call reorder(work2,nuvlevs,nnnuvlevs)
 
!       Call u,v --> vor,div routine (conversion uses compact differences)
        do k=1,nnnuvlevs
           call uv2vordiv(work1(1,k),work2(1,k))
        end do

!       Reorder work arrays for mpi communication
        call reorder2(work1,nuvlevs,nnnuvlevs)
        call reorder2(work2,nuvlevs,nnnuvlevs)

!       Get vor,div on subdomains
!       Note:  work1 --> vor, work2 --> div
        call mpi_alltoallv(work1,iscuv_s,isduv_s,&
             mpi_rtype,xhat_vor(1,1,1,ii),ircuv_s,irduv_s,mpi_rtype,&
             mpi_comm_world,ierror)
        call mpi_alltoallv(work2,iscuv_s,isduv_s,&
             mpi_rtype,xhat_div(1,1,1,ii),ircuv_s,irduv_s,mpi_rtype,&
             mpi_comm_world,ierror)

!    End of NCEP GFS block
     end do

  endif

  return
end subroutine calc_

subroutine calc2_(u,v,vor,div)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    update_guess          add analysis increment to guess
!   prgmmr: todling          org: np22                date: 2007-07-05
!
! abstract:  Caculate increment of vorticity divergence
!
! program history log:
!   2007-07-05  todling - intial code; stripped off from update_guess
!   2009-02-23  todling - this is a variation of calc_ above
!
!   input argument list:
!     u,v     - increment in grid space
!
!   output argument list:
!     vor,div - increment in grid space
!
!   comments:
!     DO NOT OVERLOAD calc (that is, DON'T ADD INTERFACE FOR 2 ROUTINES)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in   ) :: u,v
  real(r_kind),dimension(lat2,lon2,nsig),intent(  out) :: vor,div

! Declare local variables
  integer(i_kind) i,j,k
  real(r_kind),dimension(lat1,lon1,nsig):: usm,vsm
  real(r_kind),dimension(itotsub,nuvlevs):: work1,work2

!*******************************************************************************

! Initialize local arrays
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           vor(i,j,k) = zero
           div(i,j,k) = zero
        end do
     end do
  end do

! The GSI analyzes stream function (sf) and velocity potential (vp).  
! Wind field observations are in terms of zonal (u) and meridional 
! (v) wind components or wind speed.  Thus, the GSI carries wind 
! increments in terms of both u,v and sf,vp.  
!
! The NCEP GFS (global) model uses vorticity and divergence as
! wind field variable.  The code below converts increments in 
! u and v to those in vorticity and divergence.  The wind variables
! in the NCEP regional model are u and v.  Hence, the block of code
! below is only used for the NCEP GFS (.not.regional). 

! Other users may need to change the logical below to obtain the
! proper behavior for their specific guess (model background)

! For NCEP GFS convert increment in u,v to increments in vor,div
  if (.not.regional) then

!    NCEP GFS interface
!    Zero work arrays
     do k=1,nuvlevs
        do j=1,itotsub
           work1(j,k)=zero
           work2(j,k)=zero
        end do
     end do
  
!    Strip off halo for u,v grids on subdomains
     call strip(u,usm,nsig)
     call strip(v,vsm,nsig)

!    Put u,v subdomains on global slabs
!    Note:  u --> work1, v --> work2
     call mpi_alltoallv(usm,iscuv_g,isduv_g,&
          mpi_rtype,work1,ircuv_g,irduv_g,mpi_rtype,&
          mpi_comm_world,ierror)
     call mpi_alltoallv(vsm,iscuv_g,isduv_g,&
          mpi_rtype,work2,ircuv_g,irduv_g,mpi_rtype,&
          mpi_comm_world,ierror)

!    Reorder work arrays before converting u,v to vor,div
     call reorder(work1,nuvlevs,nnnuvlevs)
     call reorder(work2,nuvlevs,nnnuvlevs)

!    Call u,v --> vor,div routine (conversion uses compact differences)
     do k=1,nnnuvlevs
        call uv2vordiv(work1(1,k),work2(1,k))
     end do

!    Reorder work arrays for mpi communication
     call reorder2(work1,nuvlevs,nnnuvlevs)
     call reorder2(work2,nuvlevs,nnnuvlevs)

!    Get vor,div on subdomains
!    Note:  work1 --> vor, work2 --> div
     call mpi_alltoallv(work1,iscuv_s,isduv_s,&
          mpi_rtype,vor,ircuv_s,irduv_s,mpi_rtype,&
          mpi_comm_world,ierror)
     call mpi_alltoallv(work2,iscuv_s,isduv_s,&
          mpi_rtype,div,ircuv_s,irduv_s,mpi_rtype,&
          mpi_comm_world,ierror)

! End of NCEP GFS block

  endif

  return
end subroutine calc2_

end module xhat_vordivmod
