 subroutine update_geswtend(xut,xvt,xtt,xqt,xozt,xcwt,xpt)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    update_geswtend              add tendency to analysis
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract:  This routine adds the tendency corrections to the analysis.
!            It should be called only after guess is updated.
!
!            Stream function and velocity potential are converted into 
!            vorticity and divergence, the guess variables.
!
! program history log:
!   2007-02-15  rancic -  add foto
!   2008-12-02  todling - separated this routine from update_guess
!   2010-04-01  treadon - move strip,reorder,reorder2 to gridmod
!
!   input argument list:
!     xut,xvt,xtt,xqt,xozt,xcwt,xpt - tendencies
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
  use kinds, only: r_kind,i_kind
  use mpimod, only: iscuv_s,ierror,mpi_comm_world,irduv_s,ircuv_s,&
       isduv_g,iscuv_g,nnnuvlevs,nuvlevs,irduv_g,ircuv_g,mpi_rtype,isduv_s
  use constants, only: zero, one, fv, r3600
  use jfunc, only: l_foto
  use gridmod, only: lat1,lon1,lat2,lon2,itotsub,nsig,&
       regional,strip,reorder,reorder2
  use guess_grids, only: ges_div,ges_vor,ges_ps,ges_cwmr,ges_tv,ges_q,&
       ges_tsen,ges_oz,ges_u,ges_v,&
       nfldsig,hrdifsig
  use compact_diffs, only: uv2vordiv

  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in   ) :: xut,xvt,xtt,xqt, &
                                                          xozt,xcwt
  real(r_kind),dimension(lat2,lon2)     ,intent(in   ) :: xpt

! Declare local variables
  integer(i_kind) i,j,k,it
  real(r_kind),dimension(lat1,lon1,nsig):: usm,vsm
  real(r_kind),dimension(lat2,lon2,nsig):: dvor_t,ddiv_t
  real(r_kind),dimension(itotsub,nuvlevs):: work1,work2


  real(r_kind) tcon

  if (.not.l_foto) return

! Initialize local arrays
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           dvor_t(i,j,k) = zero
           ddiv_t(i,j,k) = zero
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

!  Do time derivative of vorticity and divergence

!    Zero work arrays
     do k=1,nuvlevs
        do j=1,itotsub
           work1(j,k)=zero
           work2(j,k)=zero
        end do
     end do
  
!    Strip off halo for u,v grids on subdomains
     call strip(xut,usm,nsig)
     call strip(xvt,vsm,nsig)

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
          mpi_rtype,dvor_t,ircuv_s,irduv_s,mpi_rtype,&
          mpi_comm_world,ierror)
     call mpi_alltoallv(work2,iscuv_s,isduv_s,&
          mpi_rtype,ddiv_t,ircuv_s,irduv_s,mpi_rtype,&
          mpi_comm_world,ierror)


!   End of NCEP GFS block

  endif

  
  do it=1,nfldsig
     tcon=hrdifsig(it)*r3600
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              ges_u(i,j,k,it)    = ges_u(i,j,k,it) + xut(i,j,k)*tcon
              ges_v(i,j,k,it)    = ges_v(i,j,k,it) + xvt(i,j,k)*tcon
              ges_tv(i,j,k,it)   = ges_tv(i,j,k,it)+ xtt(i,j,k)*tcon
              ges_q(i,j,k,it)    = ges_q(i,j,k,it) + xqt(i,j,k)*tcon

!  produce sensible temperature
              ges_tsen(i,j,k,it) = ges_tv(i,j,k,it)/(one+fv*max(zero,ges_q(i,j,k,it)))

!             Note:  Below variables only used in NCEP GFS model

              ges_oz(i,j,k,it)   = ges_oz(i,j,k,it) + xozt(i,j,k)*tcon
              ges_cwmr(i,j,k,it) = ges_cwmr(i,j,k,it)+ xcwt(i,j,k)*tcon
              ges_div(i,j,k,it)  = ges_div(i,j,k,it) + ddiv_t(i,j,k)*tcon
              ges_vor(i,j,k,it)  = ges_vor(i,j,k,it) + dvor_t(i,j,k)*tcon
           end do
        end do
     end do
     do j=1,lon2
        do i=1,lat2
           ges_ps(i,j,it) = ges_ps(i,j,it) + xpt(i,j)*tcon
        end do
     end do
  end do

  return
end subroutine update_geswtend
