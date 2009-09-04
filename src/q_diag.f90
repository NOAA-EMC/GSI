subroutine q_diag(mype)
!$$$  subroutine documentation block
!                .      .    .                                       .
! subprogram:    q_diag        get moisture diagnostics
!
!   prgmmr: kleist           org: np20                date: 2005-11-21
!
! abstract: compute statistics for negative and supersatured moisture points
!
! program history log:
!   2005-11-21  kleist
!   2007-08-08  derber - optimize, remove 1 mpi call.
!   2008-02-13  treadon - add pdryin computation
!   2008-04-23  safford - comment out unused local parameter
!   2009-04-21  derber  - fix ierror error
!
!   input argument list:
!    mype       - mpi task id
! 
!   output argument list
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use guess_grids, only: ges_q,ntguessig,ges_cwmr,ges_ps,ges_prsi
  use jfunc, only: qsatg,iout_iter,rhgues
  use mpimod, only: mpi_rtype,mpi_comm_world,mpi_sum,strip,ierror
  use constants,only: zero,two,one,half
  use gridmod, only: lat2,lon2,nsig,nlat,nlon,lat1,lon1,iglobal,&
       displs_g,ijn,wgtlats,itotsub,load_grid

  implicit none

! Declare local parameters
! real(r_kind),parameter:: r1000=1000.0_r_kind

! Declare passed variables
  integer(i_kind),intent(in):: mype

! Declare local variables
  integer(i_kind):: it,i,j,jj,k,mype_out,mm1
  real(r_kind):: qrms_neg,qrms_sat,rhrms_neg,rhrms_sat
  real(r_kind):: globps,globpw,fmeanps,fmeanpw,pdryini,rlon
  real(r_kind),dimension(2,4):: qrms,qrms0
  real(r_kind),dimension(lat2,lon2):: pw
  real(r_kind),dimension(lat1*lon1):: psm,pwm
  real(r_kind),dimension(max(iglobal,itotsub)):: work_ps,work_pw
  real(r_kind),dimension(nlon,nlat-2):: grid_ps,grid_pw

  it=ntguessig
  mype_out=0
  mm1=mype+1

  qrms=zero
  pw=zero
  do k=1,nsig
    do j=2,lon2-1
      do i=2,lat2-1
        if (ges_q(i,j,k,it) < zero) then
          qrms(1,1)=qrms(1,1) + ges_q(i,j,k,it)**two
          qrms(1,2)=qrms(1,2) + one
        else if (ges_q(i,j,k,it) > qsatg(i,j,k)) then
          qrms(2,1)=qrms(2,1) + (ges_q(i,j,k,it)-qsatg(i,j,k))**two
          qrms(2,2)=qrms(2,2) + one
        end if
        if (rhgues(i,j,k) < zero) then
          qrms(1,3)=qrms(1,3)+ rhgues(i,j,k)**two
          qrms(1,4)=qrms(1,4) + one
        else if (rhgues(i,j,k) > one) then
          qrms(2,3)=qrms(2,3)+ (rhgues(i,j,k)-1)**two
          qrms(2,4)=qrms(2,4) + one
        end if
        pw(i,j)=pw(i,j)+(ges_prsi(i,j,k,it)-ges_prsi(i,j,k+1,it))* &
             (ges_q(i,j,k,it)+ges_cwmr(i,j,k,it))
      end do
    end do
  end do

  call strip(ges_ps(1,1,it),psm,1)
  call strip(pw,pwm,1)

  call mpi_reduce(qrms,qrms0,8,mpi_rtype,mpi_sum,mype_out,mpi_comm_world,ierror)

  call mpi_gatherv(psm,ijn(mm1),mpi_rtype,work_ps,ijn,displs_g,mpi_rtype,&
       mype_out,mpi_comm_world,ierror)
  call mpi_gatherv(pwm,ijn(mm1),mpi_rtype,work_pw,ijn,displs_g,mpi_rtype,&
       mype_out,mpi_comm_world,ierror)


  if(mype == mype_out) then
     qrms_neg = zero
     qrms_sat = zero
     rhrms_neg = zero
     rhrms_sat = zero
     if(qrms0(1,2)>zero) qrms_neg=sqrt(qrms0(1,1)/qrms0(1,2))
     if(qrms0(1,4)>zero) rhrms_neg=sqrt(qrms0(1,3)/qrms0(1,4))
     if(qrms0(2,2)>zero) qrms_sat=sqrt(qrms0(2,1)/qrms0(2,2))
     if(qrms0(2,4)>zero) rhrms_sat=sqrt(qrms0(2,3)/qrms0(2,4))
     write(iout_iter,100) nint(qrms0(1,2)),qrms_neg,nint(qrms0(1,4)),rhrms_neg, &
                          nint(qrms0(2,2)),qrms_sat,nint(qrms0(2,4)),rhrms_sat
100  format(' Q_DIAG:  NEG Q  COUNT,RMS=',i9,1x,g12.6,/, &
            '          NEG RH COUNT,RMS=',i9,1x,g12.6,/, &
            '     SUPERSAT Q  COUNT,RMS=',i9,1x,g12.6,/, &
            '     SUPERSAT RH COUNT,RMS=',i9,1x,g12.6)

     call load_grid(work_ps,grid_ps)
     call load_grid(work_pw,grid_pw)
     globps=zero
     globpw=zero
     rlon=one/float(nlon)
     do jj=2,nlat-1
        j=jj-1
        fmeanps=zero
        fmeanpw=zero
        do i=1,nlon
           fmeanps=fmeanps+grid_ps(i,j)
           fmeanpw=fmeanpw+grid_pw(i,j)
        enddo
        fmeanps=fmeanps*rlon
        fmeanpw=fmeanpw*rlon
        globps=globps+fmeanps*wgtlats(jj)*half
        globpw=globpw+fmeanpw*wgtlats(jj)*half
     enddo
     globps=globps
     globpw=globpw
     pdryini=globps-globpw
     write(iout_iter,110) globps,globpw,pdryini
110  format(' Q_DIAG:  mean_ps, mean_pw, pdryini=',3(g12.6,1x))
  end if

  return
end subroutine q_diag
