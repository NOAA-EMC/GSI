subroutine getphi(tv,pri,zsfc,phl)
  use kinds, only: r_kind,i_kind
  use constants, only: rd,grav,half,one,zero,two,ione
  use gridmod, only: lat2,lon2,nsig
  use tendsmod, only: prdif9,r_prsum9,r_prdif9
  use tends4pertmod, only: prsum9
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig)     ,intent(in   ):: tv
  real(r_kind),dimension(lat2,lon2,nsig+ione),intent(in   ):: pri
  real(r_kind),dimension(lat2,lon2)          ,intent(in   ):: zsfc
  real(r_kind),dimension(lat2,lon2,nsig)     ,intent(  out):: phl

! Declare local variables
  real(r_kind),dimension(lat2,lon2,nsig+ione):: phi
  real(r_kind) r_psum9,pdif9,tword
  integer(i_kind) i,j,k

  tword=two*rd

  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        prsum9(i,j,k)=pri(i,j,k)+pri(i,j,k+ione)
        prdif9(i,j,k)=pri(i,j,k)-pri(i,j,k+ione)
        r_prsum9(i,j,k)=one/prsum9(i,j,k)
        r_prdif9(i,j,k)=one/prdif9(i,j,k)
      end do
    end do
  end do

  phi=zero ; phl=zero

 do j=1,lon2
    do i=1,lat2
      phi(i,j,ione)=grav*zsfc(i,j)
      do k=2,nsig+ione
        pdif9=prdif9(i,j,k-ione)
        r_psum9=r_prsum9(i,j,k-ione)
        phi(i,j,k) = phi(i,j,k-ione) + tword*pdif9*r_psum9*tv(i,j,k-ione)
      end do
    end do
  end do
  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        phl(i,j,k) = half*(phi(i,j,k)+phi(i,j,k+ione))
      end do
    end do
  end do

  return
end subroutine getphi

subroutine getphi_tl(tv,pri,zsfc_tl,phl)
  use kinds, only: r_kind,i_kind
  use constants, only: rd,grav,half,one,zero,two,ione
  use gridmod, only: lat2,lon2,nsig
  use nonlinmod, only: prsum_bck,prdif_bck,r_prsum_bck,r_prdif_bck,bck_tv
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig)     ,intent(in   ):: tv
  real(r_kind),dimension(lat2,lon2,nsig+ione),intent(in   ):: pri
  real(r_kind),dimension(lat2,lon2)          ,intent(in   ):: zsfc_tl
  real(r_kind),dimension(lat2,lon2,nsig)     ,intent(  out):: phl

! Declare local variables
  real(r_kind),dimension(lat2,lon2,nsig+ione):: phi
  real(r_kind) pdif,psum,tword,r_psum9,aux19,aux29
  integer(i_kind) i,j,k

  tword=two*rd

  phi=zero ; phl=zero

 do j=1,lon2
    do i=1,lat2
        phi(i,j,1)=zero    !!  ( zsfc_tl ) 
      do k=2,nsig+ione
        pdif=pri(i,j,k-ione)-pri(i,j,k)
        psum=pri(i,j,k-ione)+pri(i,j,k)
          r_psum9=r_prsum_bck(i,j,k-ione)
          aux19=r_psum9*prdif_bck(i,j,k-ione)
          aux29=r_psum9*bck_tv(i,j,k-ione)
        phi(i,j,k)=phi(i,j,k-ione) + tword*( &
                      aux19*tv(i,j,k-ione) &
                     +aux29*pdif &
                     -aux19*aux29*psum)
      end do
    end do
  end do
  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        phl(i,j,k) = half*(phi(i,j,k)+phi(i,j,k+ione))
      end do
    end do
  end do

  return
end subroutine getphi_tl

subroutine getphi_ad(tv,pri,zsfc_ad,phl)
  use kinds, only: r_kind,i_kind
  use constants, only: rd,grav,half,one,zero,two,ione
  use gridmod, only: lat2,lon2,nsig
  use nonlinmod, only: prsum_bck,prdif_bck,r_prsum_bck,r_prdif_bck,bck_tv
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig)     ,intent(inout):: tv
  real(r_kind),dimension(lat2,lon2,nsig+ione),intent(inout):: pri
  real(r_kind),dimension(lat2,lon2)          ,intent(inout):: zsfc_ad
  real(r_kind),dimension(lat2,lon2,nsig)     ,intent(in   ):: phl

! Declare local variables
  real(r_kind),dimension(lat2,lon2,nsig+ione):: phi
  real(r_kind) r_psum,pdif,psum,tword,pdif9,r_psum9,rgrav,aux19,aux29,tword_phi
  integer(i_kind) i,j,k

  tword=two*rd
  rgrav=one/grav

    phi(:,:,nsig+ione)=zero
  do k=nsig,1,-ione
    do j=1,lon2
      do i=1,lat2
        phi(i,j,k  )=half*phl(i,j,k)
        phi(i,j,k+ione)=half*phl(i,j,k)+phi(i,j,k+ione)
      end do
    end do
  end do

 do j=1,lon2
    do i=1,lat2
        pri(i,j,nsig+ione)=zero
      do k=nsig+ione,2,-ione
          r_psum9=r_prsum_bck(i,j,k-ione)
          aux19=r_psum9*prdif_bck(i,j,k-ione)
          aux29=r_psum9*bck_tv(i,j,k-ione)
          tword_phi=tword*phi(i,j,k)
        phi(i,j,k-ione)=phi(i,j,k)+phi(i,j,k-ione)
        tv(i,j,k-ione)=tword_phi*aux19+tv(i,j,k-ione)
        pdif =      tword_phi*aux29
        psum =     -tword_phi*aux19*aux29
        pri(i,j,k  )=-pdif+psum+pri(i,j,k)
        pri(i,j,k-ione)= pdif+psum+pri(i,j,k-ione)
      end do
        zsfc_ad(i,j)=phi(i,j,ione)*rgrav
    end do
  end do
        
 
  return
end subroutine getphi_ad
