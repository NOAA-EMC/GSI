  subroutine prad_bias
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    prad_bias    compute bias coefficients for passive radiances
!   prgmmr: zhu           org: np23                date: 2010-05-15
!
! abstract: compute bias coefficients for passive radiances 
!
! program history log:
!   2010-05-15  zhu
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind,r_quad
  use mpimod, only: mype
  use radinfo, only: npred,jpch_rad,iuse_rad,predx
  use gsi_4dvar, only: nobs_bins
  use obsmod, only: radheadm,radptrm,destroyobs_passive
  use berror, only: varprd
  use mpl_allreducemod, only: mpl_allreduce
  use timermod, only: timer_ini,timer_fnl
  use constants, only : zero,one,zero_quad
  implicit none

  integer(i_kind),parameter :: nthreshold=200
  integer(i_kind) i,n,j,ii,jj,jpassive,ibin,ic,mp,mm,kpred
  integer(i_kind),dimension(jpch_rad) :: icp
  integer(i_kind),dimension(npred)    :: iorder
  real(r_kind) varc
  real(r_quad),dimension(npred) :: work
  real(r_quad),dimension(npred) :: btmp
  real(r_quad),dimension(npred,npred) :: Atmp

  real(r_kind),allocatable,dimension(:) :: iobs
  real(r_quad),allocatable,dimension(:,:,:) :: A
  real(r_quad),allocatable,dimension(:,:) :: b
  real(r_kind),allocatable,dimension(:,:) :: AA
  real(r_kind),allocatable,dimension(:) :: be


! Initialize timer
  call timer_ini('prad_bias')

! Allocate arrays
  icp      = 0
  jpassive = 0
  do j=1,jpch_rad
     if (iuse_rad(j)==-1) then 
        jpassive=jpassive+1
        icp(j) = jpassive
     end if
  end do
  if (mype==0) write(6,*) 'prad_bias: number of passive channels=', jpassive 

! Allocate arrays and initialize
  allocate(A(npred,npred,jpassive),b(npred,jpassive))
  allocate(iobs(jpassive))
  do n=1,jpassive
     iobs(n)=zero
     do j=1,npred
        b(j,n)=zero_quad
        do i=1,npred
           A(i,j,n)=zero_quad
        end do
     end do
  end do

! Big loop for observations
  do ibin=1,nobs_bins
     radptrm => radheadm(ibin)%head
     do while (associated(radptrm))

        if (radptrm%luse) then

!          begin channel specific calculations
           do n=1,radptrm%nchan
              ic=radptrm%icx(n)
              mp=icp(ic)
              iobs(mp)=iobs(mp)+one

!             Assign arrays Ax=b              
              varc=radptrm%err2(n)*radptrm%raterr2(n)
              do i=1,npred
                 do j=1,npred
                    A(i,j,mp)=A(i,j,mp)+radptrm%pred(i,n)*radptrm%pred(j,n)*varc
                 end do
              end do
              do i=1,npred
                 b(i,mp)=b(i,mp)+radptrm%pred(i,n)*radptrm%res(n)*varc
              end do

           end do  ! <n channel loop>
        end if  ! <luse>

        radptrm => radptrm%llpoint
     end do
  end do  ! <ibin loop>


! Collect data from all processors
  call mpl_allreduce(jpassive,iobs)

  do n = 1,jpassive
     if (iobs(n)<nthreshold) cycle

     do i=1,jpch_rad
        if (icp(i)==n) then
           mm=i
           exit
        end if
     end do

!    Collect data from all processors for each channel
     Atmp(:,:) = A(:,:,n)
     btmp(:)   = b(:,n)
     call mpl_allreduce(npred,npred,Atmp)
     call mpl_allreduce(npred,btmp)

!    Solve linear system
     iorder=0
     kpred=0
     do i=1,npred
        do j=1,npred
           work(j)=Atmp(i,j)
        end do
        if (.not. (all(work==zero_quad))) then 
           kpred = kpred+1
           iorder(kpred) = i
        end if
     end do

     if (kpred==0) cycle

     allocate(AA(kpred,kpred),be(kpred))
     do i = 1,kpred
        ii=iorder(i)
        be(i) = btmp(ii)

        do j = 1,kpred
           jj=iorder(j)
           AA(i,j) = Atmp(ii,jj)
        end do
     end do


     do i = 1,kpred
        ii=iorder(i)
        jj=(mm-1)*npred+ii
        AA(i,i) = AA(i,i)+one/varprd(jj)
     end do
     call linmm(AA,be,kpred,1,kpred,kpred)


!    Update bias coefficients for passive channels
     do i=1,kpred
        ii=iorder(i)
        predx(ii,mm)=predx(ii,mm)+be(i)
     end do

     deallocate(AA,be)
  end do  ! end of jpassive loop

  call destroyobs_passive
  deallocate(A,b,iobs)

! Finalize timer
  call timer_fnl('prad_bias')

! End of routine
  return
  end subroutine prad_bias
