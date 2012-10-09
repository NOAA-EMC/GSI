subroutine compute_qvar3d

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    compute_qvar3d
!   prgmmr: zhu               org: np22                date: 2010-03-15
!
! abstract: compute rhgues and qvar3d
!
! program history log:
! 2010-03-15 zhu - extracted out from compute_derived
! 2010-04-10 parrish - make rhgues local, since removed from jfunc by derber (no longer used)
! 2010-05-28 todling - obtain variable id's on the fly (add getindex)
! 2011-08-17 zhu  - add handling of dssv(:,:,:,nrf3_cw) for regional when total condensate is control variable 
! 2011-11-01 eliu - add qmin 
! 2012-02-08 kleist  - add computation of ges_qsat over nfldsig bins
! 2012-02-08 parrish - remove integer constants izero, ione
! 2012-02-08 parrish - replace nn_i_kind with nn, for nn any integer.
!
!   input argument list:
!
!   output argument list:
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_single
  use berror, only: dssv
  use jfunc, only: qsatg,qgues,varq,qoption
  use control_vectors, only: cvars3d
  use gridmod, only: lat2,lon2,nsig,lat1,lon1,istart,ltosi,ltosj,iglobal, &
                     itotsub,ijn,displs_g,nlat,regional
  use constants, only: zero,one,fv,r100,qmin
  use guess_grids, only: fact_tv,ges_q,ntguessig,nfldsig,ges_tsen,ges_prsl,ges_qsat
  use mpeu_util, only: getindex
  use mpimod, only: npe,mpi_integer,mpi_rtype,mpi_sum,mpi_comm_world,mype
  use gsi_metguess_mod,  only: gsi_metguess_get,gsi_metguess_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer

  implicit none

! Declare local variables
  logical ice
  integer(i_kind) :: i,j,k,it,n,np,iderivative,nrf3_q,nrf3_cw
  real(r_kind) d,dn1,dn2
  real(r_kind),allocatable,dimension(:,:,:):: rhgues

  integer(i_kind):: nwk,istatus,ier,ierror,mm1,il,nk,nguess
  integer(i_kind),dimension(nsig):: ntmp,ntmp1
  integer(i_kind),dimension(nlat):: ntp
  real(r_kind):: rhtmp,coef,cwtmp,maxamy
  real(r_kind),dimension(nsig):: work_cw,work_cw1,amz
  real(r_kind),dimension(nlat):: wk_cw,amy0
  real(r_kind),dimension(lat2):: amy
  real(r_kind),dimension(lat1,lon1):: work2
  real(r_kind),dimension(max(iglobal,itotsub)):: work1
  real(r_kind),pointer,dimension(:,:,:):: ges_ql
  real(r_kind),pointer,dimension(:,:,:):: ges_qi


  nrf3_q=getindex(cvars3d,'q')
  nrf3_cw=getindex(cvars3d,'cw')

! Limit q to be >= qmin
  do it=1,nfldsig
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              ges_q(i,j,k,it)=max(ges_q(i,j,k,it),qmin)
           end do
        end do
     end do
  end do

! Load guess q.  Initialize saturation array to guess.
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           qgues(i,j,k)=ges_q(i,j,k,ntguessig) ! q guess
           qsatg(i,j,k)=ges_q(i,j,k,ntguessig) ! q guess
           fact_tv(i,j,k)=one/(one+fv*qsatg(i,j,k))      ! factor for tv to tsen conversion
        end do
     end do
  end do

! Compute saturation specific humidity.  Set up normalization factor
! for limq routines (1/qs*2)
  iderivative = 0
  if(qoption == 1)then
      iderivative = 1
  else
      iderivative = 2
  end if

  ice=.true.
  call genqsat(qsatg,ges_tsen(1,1,1,ntguessig),ges_prsl(1,1,1,ntguessig),lat2,lon2,nsig,ice,iderivative)

  iderivative = 0
  do it=1,nfldsig
    call genqsat(ges_qsat(1,1,1,it),ges_tsen(1,1,1,it),ges_prsl(1,1,1,it),lat2,lon2, &
           nsig,ice,iderivative)
  end do

  allocate(rhgues(lat2,lon2,nsig))
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           rhgues(i,j,k)=qgues(i,j,k)/qsatg(i,j,k)
        end do
     end do
  end do

  if (qoption==2) then
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              d=20.0_r_kind*rhgues(i,j,k) + one
              n=int(d)
              np=n+1
              dn2=d-float(n)
              dn1=one-dn2
              n=min0(max(1,n),25)
              np=min0(max(1,np),25)
              dssv(i,j,k,nrf3_q)=(varq(n,k)*dn1 + varq(np,k)*dn2)*dssv(i,j,k,nrf3_q)
           end do
        end do
     end do
  end if

  deallocate(rhgues)

  if (regional .and. nrf3_cw>0) then 
     call gsi_metguess_get('dim',nguess,ier)
     if (nguess<=0) return

     call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'ql',ges_ql,istatus);ier=istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'qi',ges_qi,istatus);ier=ier+istatus
     if (ier==0) then 
!       compute mean at each vertical level 
        ntmp=0
        work_cw =zero
        work_cw1=zero
        do k = 1,nsig
           do j = 2,lon2-1
              do i = 2,lat2-1
                 cwtmp=ges_ql(i,j,k)+ges_qi(i,j,k)
                 if (cwtmp>1.0e-10_r_kind) then
                    work_cw(k) = work_cw(k) + cwtmp
                    ntmp(k)=ntmp(k)+1
                 end if
              end do
           end do
        end do

        call mpi_allreduce(work_cw,work_cw1,nsig,mpi_rtype,mpi_sum,mpi_comm_world,ierror)
        call mpi_allreduce(ntmp,ntmp1,nsig,mpi_integer,mpi_sum,mpi_comm_world,ierror)
  
        amz=1.0e-10_r_kind
        do k=1,nsig
           if (ntmp1(k)>0) amz(k)=max(work_cw1(k)/float(ntmp1(k)),1.0e-10_r_kind)
        enddo

!       get variation with latitudes
        mm1=mype+1
        nk=int(float(nsig)/5.0_r_kind)
        do i = 2,lat2-1
           do j = 2,lon2-1
              work2(i-1,j-1)=ges_ql(i,j,nk)+ges_qi(i,j,nk)
           end do
        end do
        call mpi_allgatherv(work2,ijn(mm1),mpi_rtype,work1,ijn,displs_g,mpi_rtype,&
             mpi_comm_world,ierror)

        ntp=0
        wk_cw=zero
        do k=1,iglobal
           i=ltosi(k) ; j=ltosj(k)
           cwtmp=work1(k)
           if (cwtmp>1.0e-10_r_kind) then
              wk_cw(i)=wk_cw(i)+cwtmp
              ntp(i)=ntp(i)+1 
           end if 
        end do

        amy0=one
        maxamy=zero
        do i=1,nlat
           if (ntp(i)>0) then 
              amy0(i)=max(wk_cw(i)/float(ntp(i)),1.0e-10_r_kind)
              if (amy0(i)>maxamy) maxamy=amy0(i)
           end if
        end do
        do i=1,nlat 
           if (ntp(i)>0) amy0(i)=amy0(i)/maxamy
        end do

        do i=1,lat2
           il=i+istart(mm1)-2
           il=min0(max0(1,il),nlat)
           amy(i)=amy0(il)
        end do

!       apply the coefficients to dssv of cw
        do k=1,nsig
           do i=1,lat2
              coef=amz(k)*amy(i)
              do j=1,lon2
                 dssv(i,j,k,nrf3_cw)=coef*dssv(i,j,k,nrf3_cw)
              end do
           end do
        end do
     end if ! end of ier
  end if ! end of nrf3_cw


end subroutine compute_qvar3d

