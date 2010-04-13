module projmethod_support
!$$$  module documentation block
!                .      .    .                                       .
! module:    projmethod_support
!
! abstract:
!
! program history log:
!   2007-10-01  pondeca
!
! subroutines included:
!   sub init_mgram_schmidt
!   sub destroy_mgram_schmidt
!   sub mgram_schmidt
!
! functions included:
!   dplev_mask
!   fast_dplev
!   dplev5
!
! variable definition:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

! Uses:
  use kinds, only: r_kind
  use control_vectors

  implicit none

PRIVATE
PUBLIC init_mgram_schmidt, mgram_schmidt, &
       destroy_mgram_schmidt

! Declare local variables
  real(r_kind),allocatable,dimension(:,:)::gx,gy

contains

subroutine init_mgram_schmidt
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_mgram_schmidt
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-22  lueken - added subprogram doc block
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

  use jfunc, only: nclen,jiter,niter

  implicit none

! Declare passed variables

! Declare local variables

  allocate(gx(nclen,0:niter(jiter)))
  allocate(gy(nclen,0:niter(jiter)))

end subroutine init_mgram_schmidt

subroutine destroy_mgram_schmidt
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_mgram_schmidt
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-22  lueken - added subprogram doc block
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

! Declare passed variables

  deallocate(gx)
  deallocate(gy)

end subroutine destroy_mgram_schmidt

subroutine mgram_schmidt(gradx,grady)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    mgram_schmidt
!   prgmmr: pondeca          org: np22                date: 2007-08-01
!
! abstract: apply modified gram-schmidt orthoginalization procedure
!           to set of bi-orthogonal vectors
!
! program history log:
!   2007-08-01  pondeca
!
! input argument list:
!     gradx    - gradient of the cost function w.r.t control variable
!     grady    - B*(gradx) where B is background error covariance matrix
!
!
! output argument list:      
!     gradx    - modified gradient of the cost function
!     grady    - modified B*(gradx)
!
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind
  use jfunc, only: iter,nclen
  use constants, only: izero,ione,tiny_r_kind
  use mpimod, only: mype

  implicit none

! Declare passed variables
  type(control_vector),intent(inout) :: gradx,grady

! Declare local variables  
  integer(i_kind) i,k
  real(r_kind) prd0
!**********************************************************************

  gx(1:nclen,iter)=gradx%values(1:nclen)
  gy(1:nclen,iter)=grady%values(1:nclen)

!==> orthogonalization + renormalization

  do k=1,2
     do i=izero,iter-ione
        prd0=dplev_mask(gy(1,iter),gx(1,i),mype)
        gx(1:nclen,iter)=gx(1:nclen,iter)-gx(1:nclen,i)*prd0
        gy(1:nclen,iter)=gy(1:nclen,iter)-gy(1:nclen,i)*prd0
     enddo
     prd0=dplev_mask(gx(1,iter),gy(1,iter),mype)
     if (prd0 <= tiny_r_kind) then
        if (mype==izero) then 
           print*,'in mgram_schmidt: unable to bi-orthogonalize due to round-off error for iter,k=',iter,k
           print*,'in mgram_schmidt: likely to happen when using fast version of inner product'
           print*,'in mgram_schmidt: iter,k,prd0=',iter,k,prd0
        endif
        goto 100
     endif
     gx(1:nclen,iter)=gx(1:nclen,iter)/sqrt(prd0)
     gy(1:nclen,iter)=gy(1:nclen,iter)/sqrt(prd0)
  enddo

!==> update gradx and grady and put correct B-norm back

  prd0=dplev_mask(gradx%values,grady%values,mype)
  gradx%values(1:nclen)=gx(1:nclen,iter)*sqrt(prd0)
  grady%values(1:nclen)=gy(1:nclen,iter)*sqrt(prd0)

100 continue

contains

real(r_kind) function dplev_mask(dx,dy,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dplev_mask
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-22  lueken - added subprogram doc block
!
!   input argument list:
!    mype
!    dx,dy
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use jfunc, only:  nval_levs
  use gridmod, only:  lat2,lon2,twodvar_regional
  implicit none

! Declar passed variables
  real(r_kind),dimension(lat2,lon2,nval_levs),intent(in   ) :: dx,dy
  integer(i_kind)                            ,intent(in   ) :: mype

! Declare local variables
  logical mask(nval_levs)
  logical fast

  fast=.false.
  mask=.true.
!                set fast to .true. for twodvar_regional,
!                  substantially faster, but no roundoff error reduction and
!                  results differ for different number of processors.
  if(twodvar_regional) then
!    fast=.true.
     mask(5)=.false.
     mask(6)=.false.
     mask(8)=.false.
  end if

  if(fast) then
     dplev_mask=fast_dplev(dx,dy,mask)
  else
     dplev_mask=dplev5(dx,dy,mype,mask)
  end if

end function dplev_mask

real(r_kind) function fast_dplev(dx,dy,mask)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fast_dplev
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-22  lueken - added subprogram doc block
!
!   input argument list:
!    dx,dy
!    mask
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use jfunc, only:  nval_levs
  use gridmod, only:  lat2,lon2
  use mpimod, only: npe,mpi_comm_world,ierror,mpi_rtype
  use constants, only:  zero
  implicit none

! Declar passed variables
  real(r_kind),dimension(lat2,lon2,nval_levs),intent(in   ) :: dx,dy
  logical                                    ,intent(in   ) :: mask(nval_levs)

! Declare local variables
  real(r_kind),dimension(npe):: sumall

  integer(i_kind) i,j,k
  real(r_kind) sum

  sum=zero
  do k=1,nval_levs
     if(.not.mask(k)) cycle
     do j=2,lon2-ione
        do i=2,lat2-ione
           sum=sum+dx(i,j,k)*dy(i,j,k)
        end do
     end do
  end do

  call mpi_allgather(sum,ione,mpi_rtype,sumall,ione,mpi_rtype,mpi_comm_world,ierror)
  fast_dplev=zero
  do i=1,npe
     fast_dplev=fast_dplev+sumall(i)
  end do

end function fast_dplev

real(r_kind) function dplev5(dx,dy,mype,mask)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dplev   calculates dot product for data on subdomain
!   prgmmr: derber           org: np23                date: 2004-05-13
!
! abstract: calculates dot product for data on subdomain.  Note loops over
!           streamfunction, velocity potential, temperature, etc. Also, only
!           over interior of subdomain.
!
! program history log:
!   2004-05-13  derber, document
!   2004-07-28  treadon - add only on use declarations; add intent in/out
!   2010-04-01  treadon - move strip to grimod
!
!   input argument list:
!     dx       - input vector 1
!     dy       - input vector 2
!     mype
!     mask
!
!   output argument list
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use jfunc, only: nval_levs
  use gridmod, only: nlat,nlon,lat2,lon2,lat1,lon1,&
     ltosi,ltosj,iglobal,ijn,displs_g,strip
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  use constants, only: zero
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nval_levs),intent(in   ) :: dx,dy
  integer(i_kind)                            ,intent(in   ) :: mype
  logical                                    ,intent(in   ) :: mask(nval_levs)

! Declare local variables
  real(r_kind),dimension(lat1*lon1):: zsm
  real(r_kind),dimension(iglobal):: work1
  real(r_kind),dimension(lat2,lon2):: sum
  real(r_kind),dimension(nlat,nlon):: sumall

  integer(i_kind) i,j,k,mm1
  real(r_kind) e,y,temp

  mm1=mype+ione
  sum=zero
  do k=1,nval_levs
     if(.not.mask(k)) cycle
     do j=1,lon2
        do i=1,lat2
           sum(i,j)=sum(i,j)+dx(i,j,k)*dy(i,j,k)
        end do
     end do
  end do
  do j=1,lon1*lat1
     zsm(j)=zero
  end do

  call strip(sum,zsm,ione)

  call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
     work1,ijn,displs_g,mpi_rtype,&
     mpi_comm_world,ierror)

  do k=1,iglobal
     i=ltosi(k) ; j=ltosj(k)
     sumall(i,j)=work1(k)
  end do
  dplev5=zero
  e=zero
  do j=1,nlon
     do i=1,nlat
!       Compensated summation version of sum
        temp=dplev5
        y=sumall(i,j)+e
        dplev5=temp+y
        e=(temp-dplev5)+y
!       dplev=dplev+sumall(i,j)
     end do
  end do

end function dplev5

end subroutine mgram_schmidt

end module projmethod_support

subroutine writeout_gradients(dx,dy,nv,alpha,gamma,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    writeout_gradients
!   prgmmr: pondeca           org: np23                  date: 2006-10-17
!
! abstract: writes out the x and y gradidents of the costfunction
!           also writes out B*y
!
! program history log:
!   2006-10-17  pondeca, document
!   2010-03-29  zhu      - make changes for generalizing control variables
!   2010-04-01  treadon - move strip to grimod
!
!   input argument list:
!     nv
!     mype
!     dx       - input vector 1
!     dy       - input vector 2
!     alpha
!     gamma
!
!   output argument list
!
!attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!*************************************************************************
  use kinds, only: r_kind,i_kind
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  use gridmod, only: nsig,lat1,lon1,lat2,lon2,nlon,nlat,itotsub,iglobal, & 
                     ltosi,ltosj,latlon11,ijn,displs_g,strip
  use radinfo, only: npred,jpch_rad
  use pcpinfo, only: npredp,npcptype
  use jfunc, only: iter,jiter
  use control_vectors
  use constants, only: izero,ione
  implicit none

! Declare passed variables
  integer(i_kind)     ,intent(in   ) :: nv,mype  	
  type(control_vector),intent(in   ) :: dx,dy
  real(r_kind)        ,intent(in   ) :: alpha,gamma


! Declare local variables
  integer(i_kind) i,k,k1,k2,lun,ifield,icase,ii
  real(r_kind),allocatable,dimension(:)::tempa
  real(r_kind),allocatable,dimension(:,:)::slab
  real(r_kind),allocatable,dimension(:)::strp
  real(r_kind),allocatable,dimension(:)::field
  type(control_vector)::dz
  character(2) clun1
  character(3) clun2
!*************************************************************************
  call allocate_cv(dz)
  allocate(tempa(itotsub))
  allocate(slab(nlon,nlat))
  allocate(strp(lat1*lon1))
  allocate(field(lat2*lon2*nsig))

  write (clun1(1:2),'(i2.2)') jiter
  write (clun2(1:3),'(i3.3)') iter

  lun=19_i_kind
  do icase=1,2
 
     if (icase==ione) then 
        dz=dx
        open (lun,file='gradx.dat_'//clun1//'_'//clun2,form='unformatted')
     else if (icase==2_i_kind) then 
        dz=dy
        open (lun,file='grady.dat_'//clun1//'_'//clun2,form='unformatted')
     endif

     write (lun) nlon,nlat,nsig,jpch_rad,npred,npcptype,npredp,jiter,nv,alpha,gamma

     ii=ione
     do ifield=1,nrf3
        if (ifield==nrf3_sf)     field=dz%step(ii)%st
        if (ifield==nrf3_vp)     field=dz%step(ii)%vp
        if (ifield==nrf3_t)      field=dz%step(ii)%t
        if (ifield==nrf3_q)      field=dz%step(ii)%rh
        if (ifield==nrf3_oz)     field=dz%step(ii)%oz
        if (ifield==nrf3_cw)     field=dz%step(ii)%cw

        do k=1,nsig
           k1=ione+(k-ione)*latlon11
           k2=k1+latlon11-ione
           call strip(field(k1:k2),strp,ione)

           call mpi_gatherv(strp,ijn(mype+ione),mpi_rtype, &
                tempa,ijn,displs_g,mpi_rtype,izero,mpi_comm_world,ierror)

           if(mype == izero) then
              do i=1,iglobal
                 slab(ltosj(i),ltosi(i))=tempa(i)
              end do
              write(lun) slab
           endif

        end do
     enddo !ifield

!                               gradient wrt sfcp
     call strip(dz%step(ii)%p,strp,ione)
     call mpi_gatherv(strp,ijn(mype+ione),mpi_rtype, &
          tempa,ijn,displs_g,mpi_rtype,izero,mpi_comm_world,ierror)

     if(mype == izero) then
        do i=1,iglobal
           slab(ltosj(i),ltosi(i))=tempa(i)
        end do
        write(lun) slab
     endif

!                               gradient wrt sfct
     if (nrf2_sst>izero) then
        call strip(dz%step(ii)%sst,strp,ione)
        call mpi_gatherv(strp,ijn(mype+ione),mpi_rtype, &
            tempa,ijn,displs_g,mpi_rtype,izero,mpi_comm_world,ierror)

        if(mype == izero) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif

!                   gradient wrt satellite radiance bias correction coefficients
     if (mype==izero) write(lun) dz%predr

!                   gradient wrt precipitation bias correction coefficients
     if (mype==izero)write(lun) dz%predp

     close(lun)
  end do ! icase

  call deallocate_cv(dz)
  deallocate(tempa)
  deallocate(slab)
  deallocate(strp)
  deallocate(field)

  return

end subroutine writeout_gradients
