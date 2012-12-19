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
!   2011-07-04  todling  - fixes to run either single or double precision
!   2012-11-19  parrish - compute vertical mode information on pe 0 only, and
!                          then broadcast results to other processors.
!                          This fixes a problem with reproducibility encountered on tide.
!   2012-12-18  parrish - problem with reproducibility reappeared.  Even with all vertical mode
!                           computations done on pe 0, the library routine dgeev supplied on the WCOSS
!                           machine uses threading internally in ways that prevent reproducibility,
!                           even when computation is nominally restricted to one processor.  To fix this,
!                           available local subroutines within gsi src directory have been used in place of
!                           dgeev.  Subroutine eigen, located in raflib.f90, computes eigenvalues and
!                           eigenvectors of symmetric matrices.  The matrix qmat is not symmetric, but a
!                           rescaling produces an almost symmetric matrix.  The symmetric average of the
!                           rescaled qmat has eigenvalues and eigenvectors that differ from those of qmat
!                           by only a few percent.  Using these as starting values, inverse iteration can
!                           retrieve the desired eigenvalues and eigenvectors of qmat.  To get 15 digit
!                           agreement with those produced by dgeev, an r_quad precision version of
!                           subroutine eigen (eigen_quad, added to end of this module) was created.  Also,
!                           qmatinv, which was previously computed from the eigenvalues and eigenvectors of
!                           qmatinv, is now computed, using a quad precision version of iminv, a general
!                           matrix inversion routine available in get_semimp_mats.f90.  The quad
!                           precision computations only add about 1 second to the run time.
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
!   2012-11-19  parrish - compute vertical mode information on pe 0 only, and
!                          then broadcast results to other processors.
!                          This fixes a problem with reproducibility encountered on tide.
!   2012-12-18  parrish - substantial rewrite of this code to allow use of all local code.
!                          r_quad precision has been used in eigenvalue/eigenvector computation 
!                          to get agreement with eigenvalues/eigenvectors from library routine dgeev,
!                          which has been replaced in this version.  It appears that reproducibility
!                          has been restored.  (See more extensive comment at top of this module.)
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
    use kinds, only: r_quad
    use constants,only: zero,one,one_tenth,ten
    use gridmod,only: lat2,lon2,nsig,nlat,nlon
    use mpimod,only: mpi_rtype,mpi_comm_world,ierror,mpi_integer
    use guess_grids, only: ges_tv,ges_ps,ntguessig
    use general_sub2grid_mod, only: general_sub2grid
    use general_commvars_mod, only: g1
    implicit none

!   Declare passed variables
    integer(i_kind),intent(in   ) :: mype

!   Declare local variables
    integer(i_kind) i,j,k,n,kk,info
    real(r_kind) count,factor,psbar
    real(r_kind) factord,sum,errormax
    real(r_kind),dimension(1,lat2,lon2,1):: worksub
    real(r_kind),allocatable,dimension(:,:,:,:):: hwork
    real(r_kind),dimension(nsig+1)::pbar
    real(r_kind),dimension(nsig)::tbar
    real(r_kind),dimension(nsig+1)::ahat,bhat,chat
    real(r_kind),dimension(nsig)::hmat,smat,sqmatinv
    real(r_kind),dimension(nsig,nsig)::amat,bmat,qmat,qmatinv,bqmatinv
    real(r_kind),allocatable,dimension(:):: swww,swwwd
    real(r_kind),allocatable,dimension(:,:):: szzz,szzzd
    integer(i_kind),allocatable:: numlevs(:)
    integer(i_kind) workpe
    integer(i_kind) lpivot(nsig),mpivot(nsig)
    real(r_quad) qmatinv_quad(nsig,nsig),detqmat_quad
    real(r_kind) t1,t2

!   get work pe:

    allocate(numlevs(0:g1%npe-1))
    numlevs(0:g1%npe-1)=g1%kend(0:g1%npe-1)-g1%kbegin(0:g1%npe-1)+1
    if(g1%mype==0) then
        workpe=-1
        do i=0,g1%npe-1
   !       write(6,*)' i,numlevs(i)=',i,numlevs(i)
           if(numlevs(i)==1) workpe=i
        end do
   !    write(6,*)' workpe=',workpe
    end if
    call mpi_bcast(workpe,1,mpi_integer,0,mpi_comm_world,ierror)
   !write(6,*)' mype,workpe=',mype,workpe

!    obtain vertical coordinate constants ahat,bhat,chat
    if(mype==workpe) call getabc(ahat,bhat,chat)

!   get global mean T and ps

    allocate(hwork(g1%inner_vars,g1%nlat,g1%nlon,g1%kbegin_loc:g1%kend_alloc))

!   count:
!  Not clear if area weighting would be better.
    count=one/float(nlat*nlon)

!   psbar:

    do j=1,lon2
       do i=1,lat2
          worksub(1,i,j,1)=ges_ps(i,j,ntguessig)
       end do
    end do
    call general_sub2grid(g1,worksub,hwork)
    if(g1%mype==workpe) then
       psbar=zero
       do j=1,nlon
          do i=1,nlat
             psbar=psbar+hwork(1,i,j,1)
          end do
       end do
       psbar=ten*count*psbar
       do k=1,nsig+1
          pbar(k)=ahat(k)+bhat(k)*psbar     !  + chat(k)*(T/T0)**(1/kappa)   --- add later
       end do
    end if

!   tbar:

    do k=1,nsig
       do j=1,lon2
          do i=1,lat2
             worksub(1,i,j,1)=ges_tv(i,j,k,ntguessig)
          end do
       end do
       call general_sub2grid(g1,worksub,hwork)
       if(g1%mype==workpe) then
          tbar(k)=zero
          do j=1,nlon
             do i=1,nlat
                tbar(k)=tbar(k)+hwork(1,i,j,1)
             end do
          end do
          tbar(k)=count*tbar(k)
       end if
    end do
  
    if(g1%mype==workpe) then
       do k=1,nsig
          write(6,'(" k,pbar,tbar = ",i5,2f18.9)')k,pbar(k),tbar(k)
       end do
       k=nsig+1
       write(6,'(" k,pbar      = ",i5,f18.9)')k,pbar(k)
    end if

!    allocate variables used in vertical mode transformations:

    allocate(depths(nvmodes_keep),speeds(nvmodes_keep))
    allocate(vmodes(nsig,nvmodes_keep),dualmodes(nsig,nvmodes_keep))
    allocate(phihat2t(nsig,nvmodes_keep),phihat2p(nvmodes_keep))
    allocate(p2phihat(nvmodes_keep),t2phihat(nsig,nvmodes_keep))

if(mype==workpe) then    ! BEGIN MYPE=workpe SECTION !!!!!!!!!!!!!

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

!  get inverse of Q using iminv_quad

    qmatinv_quad=qmat
    call iminv_quad(qmatinv_quad,nsig,detqmat_quad,lpivot,mpivot)
    qmatinv=qmatinv_quad

!   check inverse

 !errormax=zero
 !do j=1,nsig
 !   do i=1,nsig
 !      sum=zero
 !      if(i==j) sum=-one
 !      do k=1,nsig
 !         sum=sum+qmat(i,k)*qmatinv(k,j)
 !      end do
 !      errormax=max(abs(sum),errormax)
 !   end do
 !end do
 !write(6,*)' error in qmatinv =',errormax

!     next get eigenvalues and eigenvectors.

   allocate(swww(nvmodes_keep),swwwd(nvmodes_keep))
   allocate(szzz(nsig,nvmodes_keep),szzzd(nsig,nvmodes_keep))
   call special_eigvv(qmat,hmat,smat,nsig,swww,szzz,swwwd,szzzd,nvmodes_keep)

   do k=1,nvmodes_keep
      depths(k)=swww(k)
      speeds(k)=sqrt(depths(k))
      do j=1,nsig
         vmodes(j,k)=szzz(j,k)
         dualmodes(j,k)=szzzd(j,k)
      end do
   end do

!   next compute p2phihat and t2phihat

  t2phihat=zero
  p2phihat=zero
  do n=1,nvmodes_keep
     do k=1,nsig
        do j=1,nsig
           t2phihat(k,n)=t2phihat(k,n)+szzzd(j,n)*amat(j,k)
        end do
        p2phihat(n)=p2phihat(n)+szzzd(k,n)*hmat(k)
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
        sum=sum+sqmatinv(k)*szzz(k,j)
     end do
     phihat2p(j)=sum
     do i=1,nsig
        sum=zero
        do k=1,nsig
           sum=sum+bqmatinv(i,k)*szzz(k,j)
        end do
        phihat2t(i,j)=sum
     end do
  end do
  phihat2p=one_tenth*phihat2p ! local units are mb, but gsi units are cb--fix later
end if  ! END MYPE=workpe SECTION !!!!!!!!!!!!!

!  BROADCAST RESULTS FROM ABOVE SECTION TO ALL PES

    call mpi_bcast(depths,nvmodes_keep,mpi_rtype,workpe,mpi_comm_world,ierror)
    call mpi_bcast(speeds,nvmodes_keep,mpi_rtype,workpe,mpi_comm_world,ierror)
    call mpi_bcast(vmodes,nsig*nvmodes_keep,mpi_rtype,workpe,mpi_comm_world,ierror)
    call mpi_bcast(phihat2t,nsig*nvmodes_keep,mpi_rtype,workpe,mpi_comm_world,ierror)
    call mpi_bcast(dualmodes,nsig*nvmodes_keep,mpi_rtype,workpe,mpi_comm_world,ierror)
    call mpi_bcast(t2phihat,nsig*nvmodes_keep,mpi_rtype,workpe,mpi_comm_world,ierror)
    call mpi_bcast(p2phihat,nvmodes_keep,mpi_rtype,workpe,mpi_comm_world,ierror)
    call mpi_bcast(phihat2p,nvmodes_keep,mpi_rtype,workpe,mpi_comm_world,ierror)

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

subroutine special_eigvv(qmat0,hmat0,smat0,nmat,swww0,szzz0,swwwd0,szzzd0,nvmodes_keep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    special_eigvv   obtain eigenvalues and eigenvecgtors of qmat0
!   prgmmr: parrish        org: np22         date:  2012-12-18
!
! abstract:  Compute eigenvalues and eigenvectors of matrix qmat0.  Input and output
!            variables are r_kind, but internal calculations are done in r_quad to eliminate
!            possible failures in the inverse iteration algorithm used to refine
!            the accuracy of the eigenvectors and eigenvalues so as to agree
!            with those produced previously by library routine dgeev to 15
!            digits.
!            
!
! program history log:
!   2012-12-18  parrish
!
!
! usage:
!   input argument list:
!     qmat0,hmat0,smat0,nmat,nvmodes_keep
!
!   output argument list:
!     swww0,szzz0,swwwd0,szzzd0
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind,r_quad

  implicit none

  integer(i_kind),intent(in)::nmat,nvmodes_keep
  real(r_kind),intent(in)::qmat0(nmat,nmat),hmat0(nmat),smat0(nmat)
  real(r_kind),intent(inout):: swww0(nvmodes_keep),swwwd0(nvmodes_keep)
  real(r_kind),intent(inout):: szzz0(nmat,nvmodes_keep),szzzd0(nmat,nvmodes_keep)


  real(r_quad) qmat(nmat,nmat),hmat(nmat),smat(nmat)
  real(r_quad) swww(nvmodes_keep),swwwd(nvmodes_keep)
  real(r_quad) szzz(nmat,nvmodes_keep),szzzd(nmat,nvmodes_keep)
  real(r_quad) rmat(nmat),qtildemat(nmat,nmat),atemp(nmat*nmat),btemp(nmat,nmat)
  real(r_quad) eigvals(nmat)
  integer(i_kind) i,j,k,ia,mv,jloop,iret,istop
  real(r_quad) orthoerror,sum
  real(r_quad) rnorm,sumd,rnormd,term,term2
  real(r_quad) aminv(nmat,nmat),aminvt(nmat,nmat)
  real(r_quad) eigval_this,eigval_next
  real(r_quad) zero_quad,half_quad,one_quad

  zero_quad=0.0_r_quad
  half_quad=0.5_r_quad
  one_quad =1.0_r_quad
  qmat=qmat0
  hmat=hmat0
  smat=smat0
  do i=1,nmat
     rmat(i)=sqrt(hmat(i)*smat(i))
  end do
  do j=1,nmat
     do i=1,nmat
        qtildemat(i,j)=rmat(i)*rmat(j)*qmat(i,j)/(hmat(i)*smat(j))
     end do
  end do

!  get eigenvalues, eigenvectors of symmetrized version 
!    of qtildemat (gives most accurate eigenvalue estimates):

  do i=1,nmat
     do j=i,nmat
        ia=i+(j*j-j)/2
        atemp(ia)=half_quad*(qtildemat(j,i)+qtildemat(i,j))
     end do
  end do
  mv=0
  call eigen_quad(atemp,btemp,nmat,mv)
  do i=1,nvmodes_keep
     j=i
     ia=i+(j*j-j)/2
     eigvals(i)=atemp(ia)
  !  write(6,*)' i,ave eigvals(i)=',i,eigvals(i)
  end do

!      iterative improvement to get eigvals, eigvects of qtildemat

  do i=1,nvmodes_keep
     szzz(:,i)=btemp(:,i)
     szzzd(:,i)=btemp(:,i)
!  renormalize szzz and szzzd
     sum=zero_quad
     do j=1,nmat
        sum=sum+szzz(j,i)**2
     end do
     sum=one_quad/sqrt(sum)
     if(szzz(1,i)<zero_quad) sum=-sum
     do j=1,nmat
        szzz(j,i)=sum*szzz(j,i)
     end do
     sum=zero_quad
     do j=1,nmat
        sum=sum+szzzd(j,i)**2
     end do
     sum=one_quad/sqrt(sum)
     if(szzzd(1,i)<zero_quad) sum=-sum
     do j=1,nmat
        szzzd(j,i)=sum*szzzd(j,i)
     end do
     eigval_this=eigvals(i)
     jloop=0
     iret=0
     do j=1,10 
        if(iret==0) call iterative_improvement0(qtildemat,eigval_this,aminv,aminvt,nmat,iret)
        if(iret==1) then
           write(6,*)' det=0 in iterative_improvement0, eigenvalue converged'
           exit
        end if
        call iterative_improvement(eigval_this,eigval_next,aminv,aminvt,szzz(:,i),szzzd(:,i),nmat,istop)
        jloop=jloop+1
        if(eigval_this==eigval_next) then
           write(6,*)' no change in eigenvalue, convergence to machine precision achieved'
           exit
        end if
        eigval_this=eigval_next
        if(istop==1) then
           write(6,*)' eigval relative change less than 10**(-24), no further iteration necessary'
           exit
        end if
     end do
     swww(i)=eigval_next
     swwwd(i)=eigval_next
     write(6,*)' i,jloop,swww(i)=',i,jloop,swww(i)
  end do

!  compute unscaled left and right eigenvectors of "corrected" matrix qmat_c

  do k=1,nvmodes_keep
     do i=1,nmat
        szzz(i,k)=hmat(i)*szzz(i,k)/rmat(i)
        szzzd(i,k)=smat(i)*szzzd(i,k)/rmat(i)
     end do
  end do

!  renormalize szzz and szzzd

  do i=1,nvmodes_keep
     sum=zero_quad
     do j=1,nmat
        sum=sum+szzz(j,i)**2
     end do
     sum=one_quad/sqrt(sum)
     if(szzz(1,i)<zero_quad) sum=-sum
     do j=1,nmat
        szzz(j,i)=sum*szzz(j,i)
     end do
     sum=zero_quad
     do j=1,nmat
        sum=sum+szzzd(j,i)**2
     end do
     sum=one_quad/sqrt(sum)
     if(szzzd(1,i)<zero_quad) sum=-sum
     do j=1,nmat
        szzzd(j,i)=sum*szzzd(j,i)
     end do
  end do

!  joint normalization:

  do i=1,nvmodes_keep
     sum=zero_quad
     do j=1,nmat
        sum=sum+szzz(j,i)*szzzd(j,i)
     end do
     sum=one_quad/sqrt(abs(sum))
     do j=1,nmat
        szzz(j,i)=sum*szzz(j,i)
        szzzd(j,i)=sum*szzzd(j,i)
     end do
  end do

 !orthoerror=zero_quad
 !do i=1,nvmodes_keep
 !   do j=1,nvmodes_keep
 !      sum=zero_quad
 !      if(i==j) sum=-one_quad
 !      do k=1,nmat
 !         sum=sum+szzz(k,i)*szzzd(k,j)
 !      end do
 !      orthoerror=max(abs(sum),orthoerror)
 !   end do
 !end do
  write(6,*)' orthoerror for szzz,szzzd=',orthoerror

! check error in qmat*szzz - swww*szzz  and qmat_trans*szzzd - swww*szzzd
  
 !do i=1,nvmodes_keep
 !   sum=zero_quad
 !   rnorm=zero_quad
 !   sumd=zero_quad
 !   rnormd=zero_quad
 !   do j=1,nmat
 !      term=-swww(i)*szzz(j,i)
 !      term2=term
 !      do k=1,nmat
 !         term=term+qmat(j,k)*szzz(k,i)
 !      end do
 !      sum=sum+term**2
 !      rnorm=rnorm+term2**2
 !      term=-swww(i)*szzzd(j,i)
 !      term2=term
 !      do k=1,nmat
 !         term=term+qmat(k,j)*szzzd(k,i)
 !      end do
 !      sumd=sumd+term**2
 !      rnormd=rnormd+term2**2
 !   end do
 !   write(6,*)' i,eigval/vect error check: vec,dual errs=',i,sqrt(sum/rnorm),sqrt(sumd/rnormd)
 !end do
  swww0 =swww
  swwwd0=swwwd
  szzz0 =szzz
  szzzd0=szzzd

end subroutine special_eigvv

subroutine iterative_improvement0(a,mu,aminv,aminvt,na,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    iterative_improvement0  compute inverse of a - mu*I
!   prgmmr: parrish        org: np22         date:  2012-12-18
!
! abstract:  Compute inverse of matrix a - mu*I, where mu is current estimate of eigenvalue
!            in inverse iteration algorithm. 
!            
!
! program history log:
!   2012-12-18  parrish
!
!
! usage:
!   input argument list:
!     a,mu,na
!
!   output argument list:
!     aminv,aminvt,iret
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_quad,i_kind
  implicit none

  integer(i_kind),intent(in)::na
  real(r_quad),intent(in)::a(na,na),mu
  real(r_quad),intent(inout)::aminv(na,na)
  real(r_quad),intent(inout)::aminvt(na,na)
  integer(i_kind),intent(out)::iret

  real(r_quad) am(na,na),amwork(na,na)
  real(r_quad) errormax,sum,detam,errlimit
  integer(i_kind) i,j,k,lpivot(na),mpivot(na)
  real(r_quad) zero_quad,one_quad

  errlimit=1000._r_quad
  zero_quad=0._r_quad
  one_quad=1._r_quad

!  compute (A-mu*I)**(-1)

  iret=0

  am=zero_quad
  do j=1,na
     am(j,j)=-mu
  end do
  do j=1,na
     do i=1,na
        am(i,j)=am(i,j)+a(i,j)
     end do
  end do
  aminv=am
  call iminv_quad(aminv,na,detam,lpivot,mpivot)
  if(detam==zero_quad) then
     iret=1
     return
  end if
  do j=1,na
     do i=1,na
        aminvt(i,j)=aminv(j,i)
     end do
  end do

!   check inverse

  errormax=zero_quad
  do j=1,na
     do i=1,na
        sum=zero_quad
        if(i==j) sum=-one_quad
        do k=1,na
           sum=sum+am(i,k)*aminv(k,j)
        end do
        errormax=max(abs(sum),errormax)
     end do
  end do
  write(6,*)' error in aminv =',errormax
  if(errormax>errlimit) iret=-1

end subroutine iterative_improvement0

subroutine iterative_improvement(mu,mu_next,aminv,aminvt,bv,bw,na,istop)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    iterative_improvement  complete one iteration of inverse iteration
!   prgmmr: parrish        org: np22         date:  2012-12-18
!
! abstract:  Apply one iteration of inverse iteration, producing a new estimate for eigenvalue
!            and eigenvector.
!            
!
! program history log:
!   2012-12-18  parrish
!
!
! usage:
!   input argument list:
!     mu,aminv,aminvt,bv,bw,na
!
!   output argument list:
!     mu_next,bv,bw,istop
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_quad,i_kind
  implicit none

  integer(i_kind),intent(in)::na
  integer(i_kind),intent(inout)::istop
  real(r_quad),intent(in)::mu
  real(r_quad),intent(in)::aminv(na,na),aminvt(na,na)
  real(r_quad),intent(inout):: mu_next
  real(r_quad),intent(inout)::bv(na),bw(na)

  real(r_quad) bnextv(na),bnextw(na)
  real(r_quad) factorv,factorw,sumv,sumw
  real(r_quad) zero_quad,half_quad,one_quad,two_quad,tol_quad
  integer(i_kind) i,j

  istop=0
  zero_quad=0.0_r_quad
  half_quad=0.5_r_quad
  one_quad=1.0_r_quad
  two_quad=2.0_r_quad
  tol_quad=10.0_r_quad**(-24)
  bnextv=zero_quad
  bnextw=zero_quad
  do i=1,na
     do j=1,na
        bnextv(i)=bnextv(i)+aminv(i,j)*bv(j)
        bnextw(i)=bnextw(i)+aminvt(i,j)*bw(j)
     end do
  end do

!  compute dot prod of bnext with b

  sumv=zero_quad
  sumw=zero_quad
  do i=1,na
     sumv=sumv+bnextv(i)*bv(i)
     sumw=sumw+bnextw(i)*bw(i)
  end do
  mu_next=mu+half_quad/sumv+half_quad/sumw
  if(two_quad*abs(mu_next-mu)/abs(mu+mu_next) < tol_quad) istop=1
     

!   normalize bnextv and bnextw

  factorv=zero_quad
  factorw=zero_quad
  do i=1,na
     factorv=factorv+bnextv(i)**2
     factorw=factorw+bnextw(i)**2
  end do
  factorv=one_quad/sqrt(factorv)
  factorw=one_quad/sqrt(factorw)
  if(bnextv(1)<zero_quad) factorv=-factorv
  if(bnextw(1)<zero_quad) factorw=-factorw
  do i=1,na
     bv(i)=factorv*bnextv(i)
     bw(i)=factorw*bnextw(i)
  end do

end subroutine iterative_improvement

      subroutine iminv_quad (a,n,d,l,m)                                              
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    iminv    invert a matrix
!
!   prgrmmr:
!
! abstract:      the standard gauss-jordan method is used. the determinant           
!                is also calculated. a determinant of zero indicates that            
!                the matrix is singular.                                             
!                                                                               
! remarks        matrix a must be a general matrix                                   
!
! program history log:
!   2008-06-04  safford -- add subprogram doc block
!   2012-12-18  parrish -- create r_quad precision version
!
!   input argument list:
!     a - input matrix, destroyed in computation and replaced by resultant inverse
!     n - order of matrix a                                               
!     d - resultant determinant                                           
!     l - work vector of length n                                         
!     m - work vector of length n                                         
!
!   output argument list:
!     a - input matrix, destroyed in computation and replaced by resultant inverse
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

        use kinds,only: r_quad,i_kind
        implicit none

        integer(i_kind)             ,intent(in   ) :: n
        integer(i_kind),dimension(n),intent(inout) :: l,m
        real(r_quad)                ,intent(inout) :: d
        real(r_quad),dimension(n*n) ,intent(inout) :: a

        integer(i_kind):: nk,k,j,iz,i,ij
        integer(i_kind):: kj,ik,jr,jq,jk,ki,kk,jp,ji

        real(r_quad):: biga,hold
        real(r_quad):: zero_quad,one_quad
!                                                                               
!        if a double precision version of this routine is desired, the          
!        ! in column 1 should be removed from the double precision              
!        statement which follows.                                               
!                                                                               
!     double precision a, d, biga, hold                                         
!                                                                               
!        the ! must also be removed from double precision statements            
!        appearing in other routines used in conjunction with this              
!        routine.                                                               
!                                                                               
!        the double precision version of this sr........ must also              
!        contain double precision fortran functions.  abs in statemen           
!        10 must be changed to dabs  .                                          
!                                                                               
!        ...............................................................        

         zero_quad=0.0_r_quad
         one_quad=1.0_r_quad
!                                                                               
!        search for largest element                                             
!                                                                               
         d=one_quad
         nk=-n
         do 80 k=1,n
            nk=nk+n
            l(k)=k
            m(k)=k
            kk=nk+k
            biga=a(kk)
            do 20 j=k,n
               iz=n*(j-1)
               do 20 i=k,n
                  ij=iz+i
   10             if(abs(biga)-abs(a(ij))) 15,20,20
   15             biga=a(ij)
                  l(k)=i
                  m(k)=j
   20       continue
!
!        interchange rows
!
            j=l(k)
            if(j-k) 35,35,25
   25       ki=k-n
            do 30 i=1,n
               ki=ki+n
               hold=-a(ki)
               ji=ki-k+j
               a(ki)=a(ji)
   30          a(ji) =hold
!
!        interchange columns
!
   35          i=m(k)
               if(i-k) 45,45,38
   38          jp=n*(i-1)
               do 40 j=1,n
                  jk=nk+j
                  ji=jp+j
                  hold=-a(jk)
                  a(jk)=a(ji)
   40             a(ji) =hold
!
!        divide column by minus pivot (value of pivot element is
!        contained in biga)
!
   45             if(biga) 48,46,48
   46             d=zero_quad
                  return
   48             do 55 i=1,n
                     if(i-k) 50,55,50
   50                ik=nk+i
                     a(ik)=a(ik)/(-biga)
   55             continue
!
!        reduce matrix
!
                  do 65 i=1,n
                     ik=nk+i
                     ij=i-n
                     do 65 j=1,n
                        ij=ij+n
                        if(i-k) 60,65,60
   60                   if(j-k) 62,65,62
   62                   kj=ij-i+k
                        a(ij)=a(ik)*a(kj)+a(ij)
   65             continue
!
!        divide row by pivot
!
                  kj=k-n
                  do 75 j=1,n
                     kj=kj+n
                     if(j-k) 70,75,70
   70                a(kj)=a(kj)/biga
   75             continue
!
!        product of pivots
!
                  d=d*biga
!
!        replace pivot by reciprocal
!
                  a(kk)=one_quad/biga
   80    continue
!
!        final row and column interchange
!
         k=n
  100    k=(k-1)
         if(k) 150,150,105
  105    i=l(k)
         if(i-k) 120,120,108
  108    jq=n*(k-1)
         jr=n*(i-1)
         do 110 j=1,n
            jk=jq+j
            hold=a(jk)
            ji=jr+j
            a(jk)=-a(ji)
  110       a(ji) =hold
  120       j=m(k)
            if(j-k) 100,100,125
  125       ki=k-n
            do 130 i=1,n
               ki=ki+n
               hold=a(ki)
               ji=ki-k+j
               a(ki)=-a(ji)
  130          a(ji) =hold
               go to 100
  150          return
      end subroutine iminv_quad

SUBROUTINE EIGEN_quad(A,R,N,MV)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    EIGEN
!
!   prgrmmr:     R. J. Purser, NCEP 2005
!
! abstract:  COMPUTE EIGENVALUES AND EIGENVECTORS OF A REAL SYMMETRIC
!            MATRIX
!
!        REMARKS
!           ORIGINAL MATRIX A MUST BE REAL SYMMETRIC (STORAGE MODE=1)
!           MATRIX A CANNOT BE IN THE SAME LOCATION AS MATRIX R
!
!        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
!           NONE
!
!        METHOD
!           DIAGONALIZATION METHOD ORIGINATED BY JACOBI AND ADAPTED
!           BY VON NEUMANN FOR LARGE COMPUTERS AS FOUND IN 'MATHEMATICAL
!           METHODS FOR DIGITAL COMPUTERS', EDITED BY A. RALSTON AND
!           H.S. WILF, JOHN WILEY AND SONS, NEW YORK, 1962, CHAPTER 7
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!   2012-12-18  parrish -- create r_quad precision version
!
!   input argument list:
!     A - ORIGINAL MATRIX (SYMMETRIC), DESTROYED IN COMPUTATION.
!         RESULTANT EIGENVALUES ARE DEVELOPED IN DIAGONAL OF
!         MATRIX A IN DESCENDING ORDER.
!     R - RESULTANT MATRIX OF EIGENVECTORS (STORED COLUMNWISE,
!         IN SAME SEQUENCE AS EIGENVALUES)
!     N - ORDER OF MATRICES A AND R
!     MV- INPUT CODE
!         0   COMPUTE EIGENVALUES AND EIGENVECTORS
!         1   COMPUTE EIGENVALUES ONLY (R NEED NOT BE
!             DIMENSIONED BUT MUST STILL APPEAR IN CALLING SEQUENCE)
!
!   output argument list:
!     A - ORIGINAL MATRIX (SYMMETRIC), DESTROYED IN COMPUTATION.
!         RESULTANT EIGENVALUES ARE DEVELOPED IN DIAGONAL OF
!         MATRIX A IN DESCENDING ORDER.
!     R - RESULTANT MATRIX OF EIGENVECTORS (STORED COLUMNWISE,
!         IN SAME SEQUENCE AS EIGENVALUES)
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
      use kinds, only: r_quad,i_kind
      implicit none

!     REAL(4) A(1),R(1)
!
!        ...............................................................
!
!        IF A DOUBLE PRECISION VERSION OF THIS ROUTINE IS DESIRED, THE
!        C IN COLUMN 1 SHOULD BE REMOVED FROM THE DOUBLE PRECISION
!        STATEMENT WHICH FOLLOWS.
!
      real(r_quad)   ,intent(inout) :: A(1), R(1)
      integer(i_kind),intent(in   ) :: N, MV

      REAL(r_quad) ANORM,ANRMX,THR,X,Y,SINX,SINX2,COSX, &
                       COSX2,SINCS,RANGE,ONEMYY
      real(r_quad) zero_quad,half_quad,one_quad,two_quad
      integer(i_kind) I,J,K,IA,IQ,IJ,IL,IM,ILR,IMR,IND,L,M,MQ,LQ,LM, &
                       JQ,MM,LL,ILQ,IMQ

!
!        THE C MUST ALSO BE REMOVED FROM DOUBLE PRECISION STATEMENTS
!        APPEARING IN OTHER ROUTINES USED IN CONJUNCTION WITH THIS
!        ROUTINE.
!
!        THE DOUBLE PRECISION VERSION OF THIS SUBROUTINE MUST ALSO
!        CONTAIN DOUBLE PRECISION FORTRAN FUNCTIONS.  SQRT IN STATEMENTS
!        40, 68, 75, AND 78 MUST BE CHANGED TO DSQRT.  ABS IN STATEMENT
!        62 MUST BE CHANGED TO DABS. THE CONSTANT IN STATEMENT 5 SHOULD
!        BE CHANGED TO 1.0D-12.
!
!        ...............................................................
      zero_quad=0.0_r_quad
      half_quad=0.5_r_quad
      one_quad =1.0_r_quad
      two_quad =2.0_r_quad
!
!        GENERATE IDENTITY MATRIX
!
    5 continue
      RANGE=1.0E-22_r_quad
      if(mv==1) go to 25
         IQ=-N
         DO J=1,N
            IQ=IQ+N
            DO I=1,N
               IJ=IQ+I
               R(IJ)=zero_quad
               if(i/=j) go to 20
                  R(IJ)=one_quad
          20   CONTINUE
            end do
         end do
!
!        COMPUTE INITIAL AND FINAL NORMS (ANORM AND ANORMX)
!
   25 continue
      ANORM=zero_quad
      DO I=1,N
         DO J=I,N
            if(i==j) go to 35
               IA=I+(J*J-J)/2
               ANORM=ANORM+A(IA)*A(IA)
   35       CONTINUE
         end do
      end do
      if(anorm<=zero_quad) go to 165
         ANORM=1.414_r_quad*SQRT(ANORM)
         ANRMX=ANORM*RANGE/FLOAT(N)
!
!        INITIALIZE INDICATORS AND COMPUTE THRESHOLD, THR
!
         IND=0
         THR=ANORM
   45    continue
         THR=THR/FLOAT(N)
   50    continue
         L=1
   55    continue
         M=L+1
!
!        COMPUTE SIN AND COS
!
   60    continue
         MQ=(M*M-M)/2
         LQ=(L*L-L)/2
         LM=L+MQ
   62    continue
         if(abs(a(lm))-thr<zero_quad) go to 130
            IND=1
            LL=L+LQ
            MM=M+MQ
            X=half_quad*(A(LL)-A(MM))
   68       continue
            Y=-A(LM)/ SQRT(A(LM)*A(LM)+X*X)
            if(x>=zero_quad) go to 75
               Y=-Y
!DP75 SINX=Y/ SQRT(two_quad*(one_quad+( SQRT(one_quad-Y*Y))))
   75       continue
            SINX=Y/ SQRT(two_quad*(one_quad+( SQRT(MAX(zero_quad,one_quad-Y*Y)))))
            ONEMYY=one_quad-Y*Y
            IF(one_quad-Y*Y<zero_quad) write(6,*)' IN EIGEN, 1-Y*Y=',ONEMYY
            SINX2=SINX*SINX
!DP78 COSX= SQRT(one_quad-SINX2)
   78       continue
            COSX= SQRT(MAX(zero_quad,one_quad-SINX2))
            COSX2=COSX*COSX
            SINCS =SINX*COSX
!
!        ROTATE L AND M COLUMNS
!
            ILQ=N*(L-1)
            IMQ=N*(M-1)
            DO 125 I=1,N
               IQ=(I*I-I)/2
               if(i==l) go to 115
               if(i==m) go to 115
                  if(i>m) go to 90
                     IM=I+MQ
                     GO TO 95
    90            continue
                  IM=M+IQ
    95            continue
                  if(i>=l) go to 105
                     IL=I+LQ
                     GO TO 110
   105            continue
                  IL=L+IQ
   110            continue
                  X=A(IL)*COSX-A(IM)*SINX
                  A(IM)=A(IL)*SINX+A(IM)*COSX
                  A(IL)=X
   115         continue
               if(mv==1) go to 125
                  ILR=ILQ+I
                  IMR=IMQ+I
                  X=R(ILR)*COSX-R(IMR)*SINX
                  R(IMR)=R(ILR)*SINX+R(IMR)*COSX
                  R(ILR)=X
   125      CONTINUE
            X=two_quad*A(LM)*SINCS
            Y=A(LL)*COSX2+A(MM)*SINX2-X
            X=A(LL)*SINX2+A(MM)*COSX2+X
            A(LM)=(A(LL)-A(MM))*SINCS+A(LM)*(COSX2-SINX2)
            A(LL)=Y
            A(MM)=X
!
!        TESTS FOR COMPLETION
!
!        TEST FOR M = LAST COLUMN
!
  130    continue
         if(m==n) go to 140
            M=M+1
            GO TO 60
!
!        TEST FOR L = SECOND FROM LAST COLUMN
!
  140    continue
         if(l==n-1) go to 150
            L=L+1
            GO TO 55
  150    continue
         if(ind/=1) go to 160
            IND=0
            GO TO 50
!
!        COMPARE THRESHOLD WITH FINAL NORM
!
  160    continue
         if(thr>anrmx) go to 45
!
!        SORT EIGENVALUES AND EIGENVECTORS
!
  165 continue
      IQ=-N
      DO I=1,N
         IQ=IQ+N
         LL=I+(I*I-I)/2
         JQ=N*(I-2)
         DO J=I,N
            JQ=JQ+N
            MM=J+(J*J-J)/2
            if(a(ll)>=a(mm)) go to 1970
               X=A(LL)
               A(LL)=A(MM)
               A(MM)=X
            if(mv==1) go to 1970
               DO K=1,N
                  ILR=IQ+K
                  IMR=JQ+K
                  X=R(ILR)
                  R(ILR)=R(IMR)
                  R(IMR)=X
               end do
1970        continue
         end do
      end do
      RETURN
END SUBROUTINE EIGEN_quad
