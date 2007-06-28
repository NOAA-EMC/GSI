subroutine linbal(psi,phib)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    linbal     
!
!   prgmmr: kleist           org: np22                date: 2006-09-25
!
! abstract: apply linear balance equation to derive balanced 
!           balanced geopotential increment
!
! program history log:
!   2006-09-025  kleist
!
!   input argument list:
!     psi       - streamfunction increment
!
!   output argument list:
!     phib      - balanced geopotential increment
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: lat2,nsig,iglobal,lon1,itotsub,lon2,lat1,&
       nlat,nlon,ltosi,ltosj,ltosi_s,ltosj_s,corlats
  use mpimod, only: iscuv_s,ierror,mpi_comm_world,irduv_s,ircuv_s,&
       isduv_s,isduv_g,iscuv_g,nuvlevs,irduv_g,ircuv_g,mpi_rtype,&
       strip,reorder,reorder2
  use specmod, only: enn1,ncd2,nc
  use compact_diffs, only: compact_grad,compact_div
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: psi
  real(r_kind),dimension(lat2,lon2,nsig),intent(out):: phib

! Declare local variables
  integer(i_kind) i,j,k,kk,isize,ni1,ni2

  real(r_kind),dimension(lat1,lon1,nsig):: psism
  real(r_kind),dimension(itotsub,nuvlevs):: work1
  real(r_kind),dimension(nlat,nlon):: work3,psix,psiy,grdtmp
  real(r_kind),dimension(nc):: spc1

! Initialize variables
  isize=max(iglobal,itotsub)

! zero out output arrays
  phib=zero

! zero out work arrays
  do k=1,nuvlevs
    do j=1,isize
      work1(j,k)=zero
    end do
  end do

!   strip off endpoints arrays on subdomains
    call strip(psi,psism,nsig)

!   put st/vp on global slabs
    call mpi_alltoallv(psism(1,1,1),iscuv_g,isduv_g,&
         mpi_rtype,work1(1,1),ircuv_g,irduv_g,mpi_rtype,&
         mpi_comm_world,ierror)

    call reorder(work1,nuvlevs)

! work 1 contains global slaps of streamfunction increment now
!   perform scalar g2s on work array
    do k=1,nuvlevs
      spc1=zero ; work3=zero ; psix=zero ; psiy=zero
      
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        work3(ni1,ni2)=work1(kk,k)
      end do

      call compact_grad(work3,psix,psiy)
      do j=1,nlon
        do i=1,nlat
          psix(i,j)=corlats(i)*psix(i,j)
          psiy(i,j)=corlats(i)*psiy(i,j)
        end do
      end do
      
      grdtmp=zero
      call compact_div(psix,psiy,grdtmp)

! need inverse laplacian of grdtmp, perform spectrally
      call g2s0(spc1,grdtmp)

!   inverse laplacian
      do i=2,ncd2
        spc1(2*i-1)=spc1(2*i-1)/(-enn1(i))
        spc1(2*i)=spc1(2*i)/(-enn1(i))
      end do

      work3=zero
      call s2g0(spc1,work3)

      do kk=1,itotsub
        ni1=ltosi_s(kk); ni2=ltosj_s(kk)
        work1(kk,k)=work3(ni1,ni2)
      end do
    end do  !end do nuvlevs

!   reorder the work array for the mpi communication
    call reorder2(work1,nuvlevs)

!   get u,v back on subdomains
    call mpi_alltoallv(work1(1,1),iscuv_s,isduv_s,&
         mpi_rtype,phib(1,1,1),ircuv_s,irduv_s,mpi_rtype,&
         mpi_comm_world,ierror)
    
  return
end subroutine linbal

subroutine linbal_ad(psitmp,phib)
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: lat2,nsig,iglobal,lon1,itotsub,lon2,lat1,&
       nlat,nlon,ltosi,ltosj,ltosi_s,ltosj_s,corlats
  use mpimod, only: iscuv_s,ierror,mpi_comm_world,irduv_s,ircuv_s,&
       isduv_s,isduv_g,iscuv_g,nuvlevs,irduv_g,ircuv_g,mpi_rtype,&
       strip,reorder,reorder2
  use specmod, only: enn1,ncd2,nc
  use compact_diffs, only: compact_grad_ad,compact_div_ad
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: phib
  real(r_kind),dimension(lat2,lon2,nsig),intent(out):: psitmp

! Declare local variables
  integer(i_kind) i,j,k,kk,isize,ni1,ni2

  real(r_kind),dimension(lat1,lon1,nsig):: phism
  real(r_kind),dimension(itotsub,nuvlevs):: work1
  real(r_kind),dimension(nlat,nlon):: work3,psix,psiy,grdtmp
  real(r_kind),dimension(nc):: spc1

! Initialize variables
  isize=max(iglobal,itotsub)
  psitmp=zero

! zero out work arrays
  do k=1,nuvlevs
    do j=1,isize
      work1(j,k)=zero
    end do
  end do

!   strip off endpoints arrays on subdomains
  call strip(phib,phism,nsig)

!   put on global slabs
  call mpi_alltoallv(phism(1,1,1),iscuv_g,isduv_g,&
       mpi_rtype,work1(1,1),ircuv_g,irduv_g,mpi_rtype,&
       mpi_comm_world,ierror)

!   reorder work arrays before calling of
  call reorder(work1,nuvlevs)

!   perform scalar g2s on work array
  do k=1,nuvlevs
    spc1=zero ; work3=zero

    do kk=1,iglobal
      ni1=ltosi(kk); ni2=ltosj(kk)
      work3(ni1,ni2)=work1(kk,k)
    end do

    call s2g0_ad(spc1,work3)

! adjoint of inverse laplacian
    do i=2,ncd2
      spc1(2*i-1)=spc1(2*i-1)/(-enn1(i))
      spc1(2*i)=spc1(2*i)/(-enn1(i))
    end do

    work3=zero
    call g2s0_ad(spc1,work3)

    call compact_div_ad(psix,psiy,work3)

    do j=1,nlon
      do i=1,nlat
        psix(i,j)=corlats(i)*psix(i,j)
        psiy(i,j)=corlats(i)*psiy(i,j)
      end do
    end do

    call compact_grad_ad(work3,psix,psiy)

    do kk=1,itotsub
      ni1=ltosi_s(kk); ni2=ltosj_s(kk)
      work1(kk,k)=work3(ni1,ni2)
    end do

  end do  !end do nuvlevs

  call reorder2(work1,nuvlevs)

!   get back on subdomains
  call mpi_alltoallv(work1(1,1),iscuv_s,isduv_s,&
       mpi_rtype,psitmp(1,1,1),ircuv_s,irduv_s,mpi_rtype,&
       mpi_comm_world,ierror)

  return
end subroutine linbal_ad
