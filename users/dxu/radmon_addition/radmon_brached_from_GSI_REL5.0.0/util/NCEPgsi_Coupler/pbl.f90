subroutine pbl(uges,vges,tges,psurf,jstart,jstop)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    pbl
!
! prgrmmr:  m. rancic
!
! abstract:  update wind and virtual temperature due to vertical
!            turbulent mixing. crank-nicolson time algorithm is
!            applied to laroche et al. (2002) simplified pbl 
!            parameterization
!
! program history log:
!   2008-04-02  safford -- add subprogram doc block, rm unused uses
!   2011-04-25  rancic - added jstart and jstop for threading use
!
!   input argument list:
!     pges     -
!     uges     - 
!     vges     - 
!     tges     - 
!     jstart   - starting point of j look
!     jstop    - stopping point of j look
!
!   output argument list:
!     uges     - 
!     vges     - 
!     tges     - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$

  use kinds,only: r_kind,i_kind
  use constants, only: one,zero,two,five,half,rd_over_g,rd_over_cp,grav
  use gridmod, only: lat2,lon2,nsig
  use guess_grids, only: ges_z,ntguessig
  use pblmod, only: dudz,dvdz,dodz,zi,rdzi,rdzl,eps_m,nsig_pbl
  use pblmod, only: lmbd,karm,karm0,alph,beta,epxilon
  use pblmod, only: uges0,vges0,oges0,pges0,tges0,uges1,vges1,oges1
  use tends4pertmod, only: time_step_half
  implicit none

! Declare local parameters
  real(r_kind),parameter:: r200 = 200.0_r_kind
  real(r_kind),parameter:: r20 = 20.0_r_kind

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: uges,vges,tges
  real(r_kind),dimension(lat2,lon2)     ,intent(in   ):: psurf
  integer(i_kind)                       ,intent(in   ):: jstart,jstop

! Declare local variables
  real(r_kind),dimension(lat2,lon2,nsig+1):: pges
  real(r_kind),dimension(nsig_pbl):: zl
  real(r_kind),dimension(nsig_pbl):: uloc,vloc,oloc,tloc
  real(r_kind),dimension(nsig_pbl+1):: ploc,ck

  real(r_kind) rdzik,rdzlk,ssq,aux,ckplus,ckmnus
  real(r_kind) zmix,rho
  real(r_kind) lmix,km,ri
  real(r_kind):: acoef,bcoef,ccoef

  real(r_kind),dimension(nsig_pbl):: acoef0,bcoef0,ccoef0
  real(r_kind),dimension(nsig_pbl):: acoef1,bcoef1,ccoef1
  real(r_kind),dimension(nsig_pbl):: fcoef

  integer(i_kind) i,j,k,it
  

  it=ntguessig

        call getprs_bck(psurf,tges,pges)

  do k=1,nsig_pbl
    do j=jstart,jstop
      do i=1,lat2
        uges0(i,j,k)=uges(i,j,k)
        vges0(i,j,k)=vges(i,j,k)
        oges0(i,j,k)=tges(i,j,k)*(                  &
                 r200/( pges(i,j,k)+pges(i,j,k+1) ))**rd_over_cp
        tges0(i,j,k)=tges(i,j,k)
        pges0(i,j,k)=pges(i,j,k)
      end do
    end do
  end do
    do j=jstart,jstop
      do i=1,lat2
        pges0(i,j,nsig_pbl+1)=pges(i,j,nsig_pbl+1)
      end do
    end do

  do j=jstart,jstop
    do i=1,lat2

      do k=1,nsig_pbl
        tloc(k)=tges(i,j,k)
        uloc(k)=uges(i,j,k)
        vloc(k)=vges(i,j,k)
        ploc(k)=pges(i,j,k)
        oloc(k)=oges0(i,j,k)
      end do
        ploc(nsig_pbl+1)=pges(i,j,nsig_pbl+1)

        zi(i,j,1) = ges_z(i,j,it)
      do k=1,nsig_pbl
        zi(i,j,k+1)=zi(i,j,k)-rd_over_g*two*tloc(k) &
                 *(ploc(k+1)-ploc(k))/(ploc(k+1)+ploc(k))
        zl(k)=half*(zi(i,j,k+1)+zi(i,j,k))
        rdzi(i,j,k)=one/(zi(i,j,k+1)-zi(i,j,k))
      end do

      do k=2,nsig_pbl
        rdzl(i,j,k)=one/(zl(k)-zl(k-1))
      end do

      do k=2,nsig_pbl
        rdzlk=rdzl(i,j,k)
        dodz(i,j,k)=(oloc(k)-oloc(k-1))*rdzlk
        dudz(i,j,k)=(uloc(k)-uloc(k-1))*rdzlk
        dvdz(i,j,k)=(vloc(k)-vloc(k-1))*rdzlk
      end do

      do k=2,nsig_pbl
        zmix=zi(i,j,k)-zi(i,j,1)
        lmix=karm*zmix/(one+karm0*zmix)
        ssq=dudz(i,j,k)**2+dvdz(i,j,k)**2
         if(ssq < eps_m) then
           ri=two*grav*dodz(i,j,k)/((oloc(k)+oloc(k-1))*eps_m)
         else
           ri=two*grav*dodz(i,j,k)/((oloc(k)+oloc(k-1))*ssq)
         end if 
            if( ri < zero) then
              rho=sqrt(one-r20*ri)
            else
              rho=one/(one+five*ri)**2
            end if
         if(ssq < eps_m) then
            km=lmix**2 *sqrt(eps_m)*rho
         else
            km=lmix**2 *sqrt(ssq)*rho
         end if 
        ck(k)=km*rdzl(i,j,k)
      end do

        ck(1)=zero
        ck(nsig_pbl+1)=zero


      do k=1,nsig_pbl
        aux=time_step_half*rdzi(i,j,k)
        ckplus=ck(k+1)+ck(k)
        ckmnus=(ck(k+1)-ck(k))*epxilon
          acoef=aux*(ckplus-ckmnus)
          ccoef=aux*(ckplus+ckmnus)
          bcoef=-acoef-ccoef
          
          acoef0(k)=acoef*alph
          bcoef0(k)=bcoef*alph-one
          ccoef0(k)=ccoef*alph
     
          acoef1(k)=-acoef*beta
          bcoef1(k)=-bcoef*beta-one
          ccoef1(k)=-ccoef*beta
      end do

      call multi_tridiag(acoef1,bcoef1,ccoef1,uloc,fcoef,nsig_pbl)
      call tridiag(acoef0,bcoef0,ccoef0,fcoef,nsig_pbl)
         uges(i,j,1:nsig_pbl)=fcoef(1:nsig_pbl)

      call multi_tridiag(acoef1,bcoef1,ccoef1,vloc,fcoef,nsig_pbl)
      call tridiag(acoef0,bcoef0,ccoef0,fcoef,nsig_pbl)
         vges(i,j,1:nsig_pbl)=fcoef(1:nsig_pbl)

      call multi_tridiag(acoef1,bcoef1,ccoef1,oloc,fcoef,nsig_pbl)
      call tridiag(acoef0,bcoef0,ccoef0,fcoef,nsig_pbl)
      do k=1,nsig_pbl
         tges(i,j,k)=fcoef(k)/(       &
                 r200/( pges(i,j,k)+pges(i,j,k+1) ))**rd_over_cp
      end do

      uges1(i,j,1:nsig_pbl)=uges(i,j,1:nsig_pbl)
      vges1(i,j,1:nsig_pbl)=vges(i,j,1:nsig_pbl)
      oges1(i,j,1:nsig_pbl)=fcoef(1:nsig_pbl)

    end do
  end do

  return
end subroutine pbl

