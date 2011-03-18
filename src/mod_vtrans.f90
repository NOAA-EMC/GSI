module mod_vtrans
!$$$   module documentation block
!                .      .    .                                       .
! module:    mod_vtrans      contains all routines for vertical modes
!
!   prgmmr: parrish          org: np23                date: 2006-06-26
!
! abstract:  contains routines and variables for vertical mode 
!            generation and use in vertical transforms
!
! program history log:
!   2006-06-26
!   2007-05-08   kleist - finish vertical coordinate generalization
!
! subroutines included:
!   sub init_vtrans              - initialize vertical mode related variables
!   sub create_vtrans            - allocate and load vert mode variables
!   sub destroy_vtrans           - deallocate vert mode variables
!   sub getabc                   -
!   sub vtrans_inv               - physical space u,v,T,p --> vert transformed u,v,phi
!   sub vtrans_inv_ad            - adjoint of vtrans_inv
!   sub vtrans                   - vert transformed u,v,phi --> physical space u,v,T,p
!   sub vtrans_ad                - adjoint of vtrans
!
! Variable Definitions:
!   def nvmodes_keep             - number of vertical modes to keep ( <= nsig )
!   def speeds                   - phase speeds of vertical modes
!   def vmodes                   - vertical modes
!   def dualmodes                - dual vertical modes
!   def phihat2t                 - matrix operator to convert phihat to T
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds,only: r_kind,i_kind
  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_vtrans
  public :: create_vtrans
  public :: destroy_vtrans
  public :: getabc
  public :: vtrans_inv
  public :: vtrans_inv_ad
  public :: vtrans
  public :: vtrans_ad
! set passed variables to public
  public :: nvmodes_keep,depths,speeds

  integer(i_kind) nvmodes_keep
  real(r_kind),dimension(:),allocatable:: depths,speeds
  real(r_kind),dimension(:,:),allocatable:: vmodes,phihat2t,dualmodes
  real(r_kind),dimension(:,:),allocatable:: t2phihat
  real(r_kind),dimension(:),allocatable:: p2phihat,phihat2p


contains

  subroutine init_vtrans
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_vtrans     set default values for vtrans variables
!   prgmmr: parrish         org: np23                date: 2006-06-26
!
! abstract: set default values for vtrans variables
!
! program history log:
!   2006-06-26  parrish
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
    implicit none

    nvmodes_keep=0

  end subroutine init_vtrans

  subroutine create_vtrans(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_vtrans   get vertical functions for dynamic constraint
!   prgmmr: parrish        org: np23         date:  2006-06-26
!
! abstract:  using linearization of dynamics about rest state, derive
!             coupling matrix and obtain eigenvectors/values
!             (linearization follows Juang, 2005, NCEP Office Note 445)
!            The conversion from mass variable to T,p follows the 
!            Machenhauer-Phillips approach, pointed out by R. Errico
!            in a conversation on 7-11-2006.
!
!           Briefly, the linear equations used to define the vertical
!            modes are
!
!             dD/dt = -laplacian ( H*p + A*T )                          (1)
!
!             dT/dt = -B*D                                              (2)
!
!             dp/dt = -S*D                                              (3)
!
!                 where D is divergence, p is surface pressure, T is virtual temperature,
!
!                    and the matrices H, A, B, S are as defined in ON 445.
!
!           Taking the time derivative of (1) and substituting from (2) and (3)
!           yields an equation for just divergence,
!
!              d2D/dt2 = -Q * laplacian(D)
!
!                 Q = H*S + A*B
!
!           The vertical modes are the right eigenvectors of Q and the
!           scale geopotential values for each vertical mode are the eigenvalues of Q.
!
!                 Q = U*E*V(transpose)
!
!           To transform from physical space to vertical modes, first form
!           the mass variable
!
!                 M = H*p + A*T
!
!           Then the transform variables are
!
!            (Mhat,uhat,vhat) = V(transpose)*(M,u,v)
!
!           To return from mode space to physical space, we have
!
!             (M,u,v) = U*(Mhat,uhat,vhat)
!
!           Finally, to get T,p from M using the Machenhauer-Phillips approach,
!
!              T = B*Q**(-1)*M
!
!              p = S*Q**(-1)*M
!
!           The above is only strictly valid for T and p small perturbations in gravity modes
!           only, but that is the application for which this code is intended.
!
!
! program history log:
!   2006-06-26  parrish
!   2007-02-26  yang    - replace IBM subroutine of dgeev by GSI dgeev
!   2010-04-01  treadon - move strip to gridmod
!
!
! usage:
!   input argument list:
!       mype     - current processor number
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
    use constants,only: zero,half,one,quarter,one_tenth,ten
    use gridmod,only: lat2,lon2,nsig,nlat,nlon,lat1,lon1,&
         ltosi,ltosj,iglobal,ijn,displs_g,strip
    use mpimod,only: mpi_rtype,mpi_comm_world,ierror
    use guess_grids, only: ges_tv,ges_ps,ntguessig
    use m_dgeevx,only : dgeevx
    implicit none

!   Declare passed variables
    integer(i_kind),intent(in   ) :: mype

!   Declare local variables
    integer(i_kind) i,j,k,n,kk,mm1,info
    real(r_kind) count,factor,psbar
    real(r_kind) factord,sum,errormax
    real(r_kind),dimension(lat2,lon2):: ps
    real(r_kind),dimension(nsig+1)::pbar
    real(r_kind),dimension(nsig)::tbar
    real(r_kind),dimension(nsig+1)::ahat,bhat,chat
    real(r_kind),dimension(nsig)::hmat,smat,sqmatinv
    real(r_kind),dimension(nsig,nsig)::amat,bmat,qmat,qmatinv,bqmatinv
    real(r_kind),dimension(2,nsig):: www,wwwd
    real(r_kind),dimension(2,nsig,nsig):: zzz,zzzd
    real(r_kind),dimension(lat1*lon1):: zsm
    real(r_kind),dimension(iglobal):: work1
    real(r_kind),dimension(nlat,nlon):: sumall


!    allocate variables used in vertical mode transformations:

    allocate(depths(nvmodes_keep),speeds(nvmodes_keep))
    allocate(vmodes(nsig,nvmodes_keep),dualmodes(nsig,nvmodes_keep))
    allocate(phihat2t(nsig,nvmodes_keep),phihat2p(nvmodes_keep))
    allocate(p2phihat(nvmodes_keep),t2phihat(nsig,nvmodes_keep))

!    obtain vertical coordinate constants ahat,bhat,chat
    call getabc(ahat,bhat,chat)

!   get global mean T and ps

    do j=1,lon2
       do i=1,lat2
          ps(i,j)=ges_ps(i,j,ntguessig)
       end do
    end do

!   count:
!  Not clear if area weighting would be better.
    count=one/float(nlat*nlon)

!   psbar:
    mm1=mype+1
    do j=1,lon1*lat1
       zsm(j)=zero
    end do

    call strip(ps,zsm,1)

    call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
       work1,ijn,displs_g,mpi_rtype,mpi_comm_world,ierror)

    do k=1,iglobal
       i=ltosi(k) ; j=ltosj(k)
       sumall(i,j)=work1(k)
    end do

    psbar=zero
    do j=1,nlon
       do i=1,nlat
          psbar=psbar+sumall(i,j)
       end do
    end do
    psbar=ten*count*psbar
    do k=1,nsig+1
       pbar(k)=ahat(k)+bhat(k)*psbar     !  + chat(k)*(T/T0)**(1/kappa)   --- add later
    end do

!   tbar:
    do k=1,nsig
       do j=1,lon1*lat1
          zsm(j)=zero
       end do

       call strip(ges_tv(1,1,k,ntguessig),zsm,1)

       call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
          work1,ijn,displs_g,mpi_rtype,&
          mpi_comm_world,ierror)

       do kk=1,iglobal
          i=ltosi(kk) ; j=ltosj(kk)
          sumall(i,j)=work1(kk)
       end do

       tbar(k)=zero
       do j=1,nlon
          do i=1,nlat
             tbar(k)=tbar(k)+sumall(i,j)
          end do
       end do
    end do
    do k=1,nsig
       tbar(k)=count*tbar(k)
    end do
  
    if(mype==0) then
       do k=1,nsig
          write(6,'(" k,pbar,tbar = ",i5,2f15.2)')k,pbar(k),tbar(k)
       end do
       k=nsig+1
       write(6,'(" k,pbar      = ",i5,f15.2)')k,pbar(k)
    end if

    hmat=zero ; smat=zero ; amat=zero ; bmat=zero

! Get matrices for variable transforms/vertical modes
    call get_semimp_mats(tbar,pbar,bhat,chat,amat,bmat,hmat,smat)

!   qmat = hmat*smat + amat*bmat

    do j=1,nsig
       do i=1,nsig
          qmat(i,j)=hmat(i)*smat(j)
          do k=1,nsig
             qmat(i,j)=qmat(i,j)+amat(i,k)*bmat(k,j)
          end do
       end do
    end do

!     next get eigenvalues, eigenvectors and compare to singular values, vectors.

    call dgeevx(nsig,qmat,size(qmat,1),www,wwwd,zzz,size(zzz,2),zzzd,size(zzzd,2),info,mype)

    if (mype ==0) write (6,*) ' AFTER CALL DGEEV', 'status: info =  ', info

! checks and print out eigenvalues
!
    do k=1,nvmodes_keep
       depths(k)=(wwwd(1,k)**2+wwwd(2,k)**2)**half
       speeds(k)=sqrt(depths(k))
    end do
   
    if(mype == 0)then
      do k=1,nsig
         write(6,*)' c,eigenvalue(',k,') = ',(www(1,k)**2+www(2,k)**2)**quarter,www(1,k),www(2,k)
      end do

!     do k=1,nsig
!        write(6,'(" p, eigvectors 1-4 = ",f10.2,4f12.3)')half*(pbar(k)+pbar(k+1)),zzz(1,k,1:4)
!     end do

!     do k=1,nsig
!      write(6,*)' c,eigenvalue(',k,') = ',(wwwd(1,k)**2+wwwd(2,k)**2)**quarter,wwwd(1,k),wwwd(2,k)
!     end do

!     do k=1,nsig
!        write(6,'(" p, dualvectors 1-4 = ",f10.2,4f12.3)')half*(pbar(k)+pbar(k+1)),zzzd(1,k,1:4)
!     end do
    end if

!  normalize and check for biorthogonality

    do j=1,nsig
       sum=zero
       do i=1,nsig
          sum=sum+zzz(1,i,j)*zzzd(1,i,j)
       end do
       factor=one/sqrt(abs(sum))
       factord=factor
       if(sum<zero) factord=-factor
       do i=1,nsig
          zzz(1,i,j)=factor*zzz(1,i,j)
          zzzd(1,i,j)=factord*zzzd(1,i,j)
       end do
    end do
    do j=1,nvmodes_keep
       do i=1,nsig
          vmodes(i,j)=zzz(1,i,j)
          dualmodes(i,j)=zzzd(1,i,j)
       end do
    end do


! check orthogonality

    if(mype==0) then
      errormax=zero
      do j=1,nsig
         do i=1,nsig
            sum=zero
            if(i==j) sum=-one
            do k=1,nsig
               sum=sum+zzz(1,k,i)*zzzd(1,k,j)
            end do
            errormax=max(abs(sum),errormax)
         end do
      end do
      write(6,*)' biorthonormal error = ',errormax
    end if

!   now get inverse of Q, using eigenvector/value decomposition

    do j=1,nsig
       do i=1,nsig
          qmatinv(i,j)=zero
          do k=1,nsig
             qmatinv(i,j)=qmatinv(i,j)+zzz(1,i,k)*zzzd(1,j,k)/(wwwd(1,k)**2+wwwd(2,k)**2)**half
          end do
       end do
    end do

!   check inverse

    if(mype == 0)then
      errormax=zero
      do j=1,nsig
         do i=1,nsig
            sum=zero
            if(i==j) sum=-one
            do k=1,nsig
               sum=sum+qmat(i,k)*qmatinv(k,j)
            end do
            errormax=max(abs(sum),errormax)
         end do
      end do
      write(6,*)' error in qmatinv =',errormax
    end if

!   next compute p2phihat and t2phihat

    t2phihat=zero
    p2phihat=zero
    do n=1,nvmodes_keep
       do k=1,nsig
          do j=1,nsig
             t2phihat(k,n)=t2phihat(k,n)+dualmodes(j,n)*amat(j,k)
          end do
          p2phihat(n)=p2phihat(n)+dualmodes(k,n)*hmat(k)
       end do
    end do
    p2phihat=ten*p2phihat ! in this code, p is in units of mb, but in gsi, p is in cb -- change later

!   finally compute phihat2p, phihat2t

    do j=1,nsig
       do i=1,nsig
          bqmatinv(i,j)=zero
          do k=1,nsig
             bqmatinv(i,j)=bqmatinv(i,j)+bmat(i,k)*qmatinv(k,j)
          end do
       end do
    end do
    do j=1,nsig
       sqmatinv(j)=zero
       do i=1,nsig
          sqmatinv(j)=sqmatinv(j)+smat(i)*qmatinv(i,j)
       end do
    end do

    do j=1,nvmodes_keep
       sum=zero
       do k=1,nsig
          sum=sum+sqmatinv(k)*vmodes(k,j)
       end do
       phihat2p(j)=sum
       do i=1,nsig
          sum=zero
          do k=1,nsig
             sum=sum+bqmatinv(i,k)*vmodes(k,j)
          end do
          phihat2t(i,j)=sum
       end do
    end do
    phihat2p=one_tenth*phihat2p ! local units are mb, but gsi units are cb--fix later

!      print out phihat2t

!   if(mype==0) then
!      write(6,*)' phihat2p = ',phihat2p(1:min(4,nvmodes_keep))
!      do i=1,nsig
!         write(6,*)' i,phihat2t=',i,phihat2t(i,1:min(4,nvmodes_keep))
!      end do
!   end if

  end subroutine create_vtrans

  subroutine destroy_vtrans
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_vtrans   remove space used by vtrans routines
!   prgmmr: parrish        org: np23         date:  2006-06-26
!
! abstract:  remove space used in vertical mode transformations
!
! program history log:
!   2006-06-26  parrish
!
!
! usage:
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
    implicit none

!    deallocate variables used in vertical mode transformations:

    deallocate(depths,speeds,vmodes,dualmodes)
    deallocate(phihat2t,phihat2p,p2phihat,t2phihat)

  end subroutine destroy_vtrans

  subroutine getabc(ahat,bhat,chat)
!$$$  subprogram documentation block
!
! subprogram:    getabc       get pressure constants
!
!   prgmmr: parrish          org: np22                date: 2006-05-04
!
! abstract: return constants used to get 3d pressure field at interfaces based on
!            generalized vertical coordinate
!
! program history log:
!   2006-05-04  kleist
!   2010-12-17  pagowski - add cmaq
!
! usage:
!   input argument list:
!
!   output argument list:
!     ahat       -   p(i,j,k)  = ahat(k) + bhat(k)*psfc(i,j)+chat(k)*(T(i,j,k)/T0(k))**(1/kappa)
!     bhat       -
!     chat       -
!
!$$$ end documentation block
    use constants,only: zero,ten
    use gridmod,only: nsig,ak5,bk5,ck5
    use gridmod,only: wrf_nmm_regional,nems_nmmb_regional,eta1_ll,eta2_ll,pdtop_ll,pt_ll,cmaq_regional
    implicit none

!   Declare passed variables
    real(r_kind),dimension(nsig+1),intent(  out) :: ahat,bhat,chat

!   Declare local variables
    integer(i_kind) k

    if(wrf_nmm_regional.or.nems_nmmb_regional.or.cmaq_regional) then
       do k=1,nsig+1
          ahat(k)=eta1_ll(k)*pdtop_ll-eta2_ll(k)*(pdtop_ll+pt_ll)+pt_ll
          bhat(k)=eta2_ll(k)
          chat(k)=zero
       end do
    else
       do k=1,nsig+1
          ahat(k)=ak5(k)*ten
          bhat(k)=bk5(k)
          chat(k)=ck5(k)*ten
       end do
    end if

    return
  end subroutine getabc

  subroutine vtrans_inv(uhat,vhat,phihat,u,v,t,p)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    vtrans_inv
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-05-29  safford -- add subprogram doc block
!
!   input argument list:
!     uhat,vhat,phihat
!
!   output argument list:
!     u,v,t
!     p
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    use gridmod,only: lat2,lon2,nsig
    use constants,only: zero
    implicit none

    real(r_kind),dimension(lat2,lon2,nvmodes_keep),intent(in   ) :: uhat,vhat,phihat
    real(r_kind),dimension(lat2,lon2,nsig)        ,intent(  out) :: u,v,t
    real(r_kind),dimension(lat2,lon2)             ,intent(  out) :: p

    integer(i_kind) i,j,k,n


!$omp parallel do schedule(dynamic,1) private(n,i,j,k)
    do k=1,nsig
       do n=1,nvmodes_keep
          do j=1,lon2
             do i=1,lat2
                u(i,j,k)=u(i,j,k)+vmodes(k,n)*uhat(i,j,n)
                v(i,j,k)=v(i,j,k)+vmodes(k,n)*vhat(i,j,n)
                t(i,j,k)=t(i,j,k)+phihat2t(k,n)*phihat(i,j,n)
             end do
          end do
          if( k == 1)then
             do j=1,lon2
                do i=1,lat2
                   p(i,j)=p(i,j)+phihat2p(n)*phihat(i,j,n)
                end do
             end do
          end if
       end do
    end do

  end subroutine vtrans_inv

  subroutine vtrans_inv_ad(uhat,vhat,phihat,u,v,t,p)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    vtrans_inv_ad
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-05-29  safford -- add subprogram doc block
!
!   input argument list:
!     uhat,vhat,phihat
!     u,v,t
!     p
!
!   output argument list:
!     uhat,vhat,phihat
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
    use gridmod,only: lat2,lon2,nsig
    use constants,only: zero
    implicit none

    real(r_kind),dimension(lat2,lon2,nvmodes_keep),intent(inout) :: uhat,vhat,phihat
    real(r_kind),dimension(lat2,lon2,nsig)        ,intent(in   ) :: u,v,t
    real(r_kind),dimension(lat2,lon2)             ,intent(in   ) :: p

    integer(i_kind) i,j,k,n

!$omp parallel do schedule(dynamic,1) private(n,i,j,k)
    do n=1,nvmodes_keep
       do k=1,nsig
          do j=1,lon2
             do i=1,lat2
                uhat(i,j,n)=uhat(i,j,n)+vmodes(k,n)*u(i,j,k)
                vhat(i,j,n)=vhat(i,j,n)+vmodes(k,n)*v(i,j,k)
                phihat(i,j,n)=phihat(i,j,n)+phihat2t(k,n)*t(i,j,k)
             end do
          end do
       end do
       do j=1,lon2
          do i=1,lat2
             phihat(i,j,n)=phihat(i,j,n)+phihat2p(n)*p(i,j)
          end do
       end do
    end do

  end subroutine vtrans_inv_ad

  subroutine vtrans(u,v,t,p,uhat,vhat,phihat)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    vtrans
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-05-29  safford -- add subprogram doc block
!
!   input argument list:
!     u,v,t
!     p
!
!   output argument list:
!     uhat,vhat,phihat
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    use gridmod,only: lat2,lon2,nsig
    use constants,only: zero
    implicit none

    real(r_kind),dimension(lat2,lon2,nsig)        ,intent(in   ) :: u,v,t
    real(r_kind),dimension(lat2,lon2)             ,intent(in   ) :: p
    real(r_kind),dimension(lat2,lon2,nvmodes_keep),intent(  out) :: uhat,vhat,phihat

    integer(i_kind) i,j,k,n


!$omp parallel do schedule(dynamic,1) private(n,i,j,k)
    do n=1,nvmodes_keep
       do j=1,lon2
          do i=1,lat2
             uhat(i,j,n)=zero
             vhat(i,j,n)=zero
             phihat(i,j,n)=p2phihat(n)*p(i,j)
          enddo
       enddo
       do k=1,nsig
          do j=1,lon2
             do i=1,lat2
                uhat(i,j,n)=uhat(i,j,n)+dualmodes(k,n)*u(i,j,k)
                vhat(i,j,n)=vhat(i,j,n)+dualmodes(k,n)*v(i,j,k)
                phihat(i,j,n)=phihat(i,j,n)+t2phihat(k,n)*t(i,j,k)
             end do
          end do
       end do
    end do

  end subroutine vtrans

  subroutine vtrans_ad(u,v,t,p,uhat,vhat,phihat)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    vtrans_ad
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-05-29  safford -- add subprogram doc block
!
!   input argument list:
!     u,v,t
!     p
!     uhat,vhat,phihat
!
!   output argument list:
!     u,v,t
!     p
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    use gridmod,only: lat2,lon2,nsig
    implicit none
 
    real(r_kind),dimension(lat2,lon2,nsig)        ,intent(inout) :: u,v,t
    real(r_kind),dimension(lat2,lon2)             ,intent(inout) :: p
    real(r_kind),dimension(lat2,lon2,nvmodes_keep),intent(in   ) :: uhat,vhat,phihat

    integer(i_kind) i,j,k,n

!$omp parallel do schedule(dynamic,1) private(n,i,j,k)
    do k=1,nsig
       do n=1,nvmodes_keep
          do j=1,lon2
             do i=1,lat2
                u(i,j,k)=u(i,j,k)+dualmodes(k,n)*uhat(i,j,n)
                v(i,j,k)=v(i,j,k)+dualmodes(k,n)*vhat(i,j,n)
                t(i,j,k)=t(i,j,k)+t2phihat(k,n)*phihat(i,j,n)
             end do
          end do
          if(k == 1)then
             do j=1,lon2
               do i=1,lat2
                  p(i,j)=p(i,j)+p2phihat(n)*phihat(i,j,n)
               end do
             end do
          end if
       end do
    end do

  end subroutine vtrans_ad

end module mod_vtrans
