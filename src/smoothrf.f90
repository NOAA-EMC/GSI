subroutine smoothrf(work,nsc,nlevs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    smoothrf    perform horizontal part of background error
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: smoothrf perform horizontal part of background error
!
! program history log:
!   2000-03-15  wu
!   2004-05-06  derber - combine regional, add multiple layers
!   2004-08-27  kleist - new berror variable
!   2004-10-26  wu - give smallest RF half weight for regional wind variables
!   2004-11-03  treadon - pass horizontal scale weighting factors through berror
!   2004-11-22  derber - add openMP
!   2005-03-09  wgu/kleist - square hzscl in totwgt calculation
!   2005-05-27  kleist/parrish - add option to use new patch interpolation
!               if (norsp==0) will default to polar cascade
!   2005-11-16  wgu - set nmix=nr+1+(ny-nlat)/2 to make sure
!               nmix+nrmxb=nr no matter what number nlat is.   
!   input argument list:
!     work     - horizontal fields to be smoothed
!     nsc      - number of horizontal scales to smooth over 
!     nlevs    - number of vertical levels for smoothing
!
!   output argument list:
!     work     - smoothed horizontal field
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon,regional
  use constants, only: izero,ione,zero,half
  use berror, only: wtaxs,wtxrs,inaxs,inxrs,bl,bl2,ii,jj,ii1,jj1,&
       ii2,jj2,slw,slw1,slw2,norh,nx,ny,mr,nr,nf,hzscl,hswgt
  use mpimod, only:  nvar_id
  use smooth_polcarf, only: norsp,smooth_polcas,smooth_polcasa
  implicit none

! Declare passed variables
  integer(i_kind)                        ,intent(in   ) :: nsc,nlevs
  real(r_kind),dimension(nlat,nlon,nlevs),intent(inout) :: work

! Declare local variables
  integer(i_kind) ndx,ndy,nxe,nmix,nfg
  integer(i_kind) j,naxr,i,nrmxb,nmixp,nymx,norm,nxem
  integer(i_kind) ndx2,nlatxb,nfnf
  integer(i_kind) i1,i2,j1,k

  real(r_kind),dimension(nsc):: totwgt
  real(r_kind),dimension(ny,nx):: p1all
  real(r_kind),dimension(nlon+ione,mr:nr):: p2all,p3all
  real(r_kind),dimension(-nf:nf,-nf:nf):: afg1


! Regional case
  if(regional)then
!_$omp parallel do  schedule(dynamic,1) private(k,j,totwgt)
     do k=1,nlevs

!       apply horizontal recursive filters
        do j=1,nsc
           totwgt(j)=hswgt(j)*hzscl(j)*hzscl(j)
        end do
        
        if(nvar_id(k)<3_i_kind)then
           totwgt(3)=half*totwgt(3)
        end if
        
        call rfxyyx(work(1,1,k),ny,nx,ii(1,1,1,k),&
             jj(1,1,1,k),slw(1,k),nsc,totwgt)
        
     end do

! Global case
  else

     do j=1,nsc
        totwgt(j)=hswgt(j)*hzscl(j)*hzscl(j)
     end do
     
     ndx=(nx-nlon)/2
     ndy=(nlat-ny)/2
     ndx2=2*ndx
     norm=norh*2-ione
     nxe=nlon/8
     nxem=nxe-ione
     nmix=nr+ione+(ny-nlat)/2
     naxr=nlon+ione
     nfg=nf*2+ione
     nrmxb=ndy-ione
     nlatxb=nlat-nrmxb
     nmixp=nmix+ione
     nymx=ny-nmix
     nfnf=(2*nf+ione)*(2*nf+ione)
     
!_$omp parallel do  schedule(dynamic,1) private(k) &
!_$omp private(i,j,i1,i2,j1,p1all,p2all,p3all,afg1)
     do k=1,nlevs

!       Zero p1, p2, and p3
        do j=1,nx
           do i=1,ny
              p1all(i,j)=zero
           end do
        end do
        
!       Extract central patch (band) from full grid (work --> p1)
!       Blending zones
        do i=1,ndx
           i1=i-ndx+nlon
           i2=nx-ndx+i
           do j=1,ny
              j1=j+ndy
              p1all(j,i) =work(j1,i1,k)      ! left (west) blending zone
              p1all(j,i2)=work(j1,i,k)       ! right (east) blending zone
           enddo
        enddo

!       Middle zone (no blending)
        do i=ndx+ione,nx-ndx
           i1=i-ndx
           do j=1,ny
              p1all(j,i)=work(j+ndy,i1,k)
           enddo
        enddo
        
!       Apply blending coefficients to central patch
        do i=1,ndx2
           i1=ndx2+ione-i
           i2=nx-ndx2+i
           do j=1,ny
              p1all(j,i) =p1all(j,i) *bl(i1)  ! left (west) blending zone
              p1all(j,i2)=p1all(j,i2)*bl(i)   ! right (east) blending zone
           enddo
        enddo
        
!       bl2 of p1
        do i=1,nx
           do j=1,nmix
              p1all(j,i)=p1all(j,i)*bl2(nmixp-j)
           enddo
           do j=nymx+ione,ny
              p1all(j,i)=p1all(j,i)*bl2(j-nymx)
           enddo
        enddo

!       Handle polar patches 
        do j=mr,nr
           do i=1,naxr
              p2all(i,j)=zero
              p3all(i,j)=zero
           end do
        end do
        
!       North pole patch(p2) -- blending and transfer to grid
!       South pole patch(p3) -- blending and transfer to grid

        do i=1,nlon
!          Load field into patches
           do j=mr,nrmxb+nmix
              p2all(i,j)=work(nlat-j,i,k)
              p3all(i,j)=work(j+1,i,k)
           enddo
        enddo

!       Apply blending coefficients
        do j=nrmxb+ione,nrmxb+nmix
           j1=j-nrmxb
           do i=1,nlon
              p2all(i,j)=p2all(i,j)*bl2(j1)
              p3all(i,j)=p3all(i,j)*bl2(j1)
           enddo
        enddo
        
!       Recursive filter applications

!       First do equatorial/mid-latitude band
        call rfxyyx(p1all,ny,nx,ii(1,1,1,k),jj(1,1,1,k),slw(1,k),nsc,totwgt)

!       North pole patch --interpolate - recursive filter - adjoint interpolate
        if(norsp>izero) then
           call smooth_polcasa(afg1,p2all)
        else
           call polcasa(afg1,p2all,nxem,norm,nlon,wtaxs,wtxrs,inaxs,inxrs,nf,mr,nr)
        end if
        call rfxyyx(afg1,nfg,nfg,ii1(1,1,1,k),jj1(1,1,1,k),slw1(1,k),nsc,totwgt)
        if(norsp>izero) then
           call smooth_polcas(afg1,p2all)
        else
           call polcas(afg1,p2all,nxem,norm,nlon,wtaxs,wtxrs,inaxs,inxrs,nf,mr,nr)
        end if

!       South pole patch --interpolate - recursive filter - adjoint interpolate
        if(norsp>izero) then
           call smooth_polcasa(afg1,p3all)
        else
           call polcasa(afg1,p3all,nxem,norm,nlon,wtaxs,wtxrs,inaxs,inxrs,nf,mr,nr)
        end if
        call rfxyyx(afg1,nfg,nfg,ii2(1,1,1,k),jj2(1,1,1,k),slw2(1,k),nsc,totwgt)
        if(norsp>izero) then
           call smooth_polcas(afg1,p3all)
        else
           call polcas(afg1,p3all,nxem,norm,nlon,wtaxs,wtxrs,inaxs,inxrs,nf,mr,nr)
        end if


!       Equatorial patch
!       Adjoint of central patch blending on left/right sides of patch
        do i=1,ndx2
           i1=ndx2+ione-i
           i2=nx-ndx2+i
           do j=1,ny
              p1all(j,i) =p1all(j,i) *bl(i1)   ! left (west) blending zone
              p1all(j,i2)=p1all(j,i2)*bl(i)    ! right (east) blending zone
           enddo
        enddo
        
!       bl2 of p1
        do i=1,nx
           do j=1,nmix
              p1all(j,i)=p1all(j,i)*bl2(nmixp-j)
           enddo
           do j=nymx+ione,ny
              p1all(j,i)=p1all(j,i)*bl2(j-nymx)
           enddo
        enddo

!       zero output array
        do i=1,nlon
           do j=1,nlat
              work(j,i,k)=zero
           end do
        end do

!       Adjoint of transfer between central band and full grid (p1 --> work)
        do i=1,ndx
           i1=i-ndx+nlon
           i2=nx-ndx+i
           do j=1,ny
              j1=j+ndy
              work(j1,i1,k)=work(j1,i1,k)+p1all(j,i)  ! left (west) blending zone
              work(j1,i,k) =work(j1,i,k) +p1all(j,i2) ! right (east) blending zone
           enddo
        enddo

!       Middle zone (no blending)
        do i=ndx+ione,nx-ndx
           i1=i-ndx
           do j=1,ny
              j1=j+ndy
              work(j1,i1,k)=work(j1,i1,k)+p1all(j,i)
           enddo
        enddo
        
!       Adjoint of North pole patch(p2) -- blending and transfer to grid
!       Adjoint of South pole patch(p3) -- blending and transfer to grid

        do j=nlatxb-nmix,nlatxb-ione

!          Adjoint of blending
           do i=1,nlon
              p2all(i,nlat-j)=p2all(i,nlat-j)*bl2(nlatxb-j)
           enddo
        end do
        do j=nrmxb+ione,nrmxb+nmix

!          Adjoint of blending
           do i=1,nlon
              p3all(i,j)=p3all(i,j)*bl2(j-nrmxb)
           enddo
        enddo
        do i=1,nlon

!          Adjoint of transfer
           do j=mr,nrmxb+nmix
              work(j+ione,i,k)=work(j+ione,i,k)+p3all(i,j)
           enddo
           do j=nlatxb-nmix,nlat-mr
              work(j,i,k)=work(j,i,k)+p2all(i,nlat-j)
           enddo
        enddo

!    End of k loop over nlevs
     end do

! End of global block
  end if

  return
end subroutine smoothrf
! -----------------------------------------------------------------------------
subroutine rfxyyx(p1,nx,ny,iix,jjx,dssx,nsc,totwgt)
  
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    rfxyyx      perform horizontal smoothing
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: smoothrf perform self-adjoint horizontal smoothing. nsloop
!           smoothing fields.
!
! program history log:
!   2000-03-15  wu
!   2004-08-24  derber - change indexing add rfhyt to speed things up
!
!   input argument list:
!     p1       - horizontal field to be smoothed
!     nx       - first dimension of p1
!     ny       - second dimension of p1
!     iix      - array of pointers for smoothing table (first dimension)
!     jjx      - array of pointers for smoothing table (second dimension)
!     dssx     - renormalization constants including variance
!     wgt      - weight (empirical*expected)
!
!   output argument list:
!                 all after horizontal smoothing
!     p1       - horizontal field which has been smoothed
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only:  zero
  use berror, only: be,table,ndeg
  implicit none

! Declare passed variables
  integer(i_kind)                     ,intent(in   ) :: nx,ny,nsc
  integer(i_kind),dimension(nx,ny,nsc),intent(in   ) :: iix,jjx
  real(r_kind),dimension(nx,ny)       ,intent(inout) :: p1
  real(r_kind),dimension(nx,ny)       ,intent(in   ) :: dssx
  real(r_kind),dimension(nsc)         ,intent(in   ) :: totwgt

! Declare local variables
  integer(i_kind) ix,iy,i,j,im,n

  real(r_kind),dimension(nx,ny):: p2,p1out,p1t
  real(r_kind),dimension(ndeg,ny):: gax2,dex2
  real(r_kind),dimension(nx,ny,ndeg):: alx,aly

! Zero local arrays
  do iy=1,ny
     do ix=1,nx
        p1out(ix,iy)=zero
     enddo
  enddo

! Loop over number of scales
 
  do n=1,nsc

     do j=1,ny
        do i=1,ndeg
           gax2(i,j)=zero
           dex2(i,j)=zero
        end do
     end do
     do iy=1,ny
        do ix=1,nx
           p2(ix,iy)=zero
        enddo
     enddo
     do im=1,ndeg
        do j=1,ny
           do i=1,nx
              alx(i,j,im)=table(iix(i,j,n),im)
              aly(i,j,im)=table(jjx(i,j,n),im)
           enddo
        enddo
     enddo

!    IX < 0     |          |     IX > NX
!   ---------------------------------------
!       .       |     .	   |  .            <-- IY > NY
!       .       |    P1	   |  .
!       .       |     .	   |  .            <-- IY < 0
!   ---------------------------------------


     call rfhx0(p1,p2,gax2,dex2,nx,ny,ndeg,alx,be)


!    IX < 0     |          |     IX > NX
!   ---------------------------------------
!       .       |     .	   |  .            <-- IY > NY
!       DEX2    |    P2	   | GAX2
!       .       |     .	   |  .            <-- IY < 0
!   ---------------------------------------

     call rfhyt(p2,p1t,nx,ny,ndeg,aly,be)


!    IX < 0     |          |     IX > NX
!   ---------------------------------------
!       DEGAXY1 |   GAY1   |GAGAXY1        <-- IY > NY
!         DEX1  |    P1	   | GAX1
!       DEDEXY1 |   DEY1   |GADEXY1        <-- IY < 0
!   ---------------------------------------


     do iy=1,ny
        do ix=1,nx
           p1t(ix,iy)=p1t(ix,iy)*dssx(ix,iy)*totwgt(n)
        enddo
     enddo


!    IX < 0     |          |     IX > NX
!   ---------------------------------------
!       GADEXY1 |   DEY1   |DEDEXY1        <-- IY > NY
!         GAX1  |    P1	   | DEX1
!       GAGAXY1 |   GAY1   |DEGAXY1        <-- IY < 0
!   ---------------------------------------

     call rfhy(p1t,p2,dex2,gax2,nx,ny,ndeg,ndeg,aly,be)


!    IX < 0     |          |     IX > NX
!   ---------------------------------------
!           .   |     .    |   .           <-- IY > NY
!         GAX2  |    P2	   | DEX2
!           .   |     .    |   .           <-- IY < 0
!  ---------------------------------------

     call rfhx0(p2,p1out,gax2,dex2,nx,ny,ndeg,alx,be)

!    IX < 0     |          |     IX > NX
!   ---------------------------------------
!           .   |     .	   |  .            <-- IY > NY
!           .   |    P1	   |  .
!           .   |     .	   |  .            <-- IY < 0
!  ---------------------------------------

! end loop over number of horizontal scales
  end do

  do iy=1,ny
     do ix=1,nx
        p1(ix,iy)=p1out(ix,iy)
     enddo
  enddo

  return
end subroutine rfxyyx
! -----------------------------------------------------------------------------
subroutine rfhx0(p1,p2,gap,dep,nx,ny,ndeg,alx,be)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   rfhx0        performs x component of recursive filter
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: performs x component of recursive filter
!
! program history log:
!   2000-03-15  wu
!   2004-05-06  derber  combine regional, add multiple layers
!   2004-08-24  derber change indexing to 1-nx,1-ny
!
!   input argument list:
!     p1       - field to be smoothed
!     nx       - first dimension of p1
!     ny       - second dimension of p1
!     ndeg     - degree of smoothing   
!     alx      - smoothing coefficients
!     be       - smoothing coefficients
!     gap      - boundary field (see rfxyyx) 
!     dep      - boundary field (see rfxyyx) 
!
!   output argument list:
!     p2       - field after smoothing
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: ione
  implicit none

  integer(i_kind)                   ,intent(in   ) :: nx,ny,ndeg
  real(r_kind),dimension(ndeg,ny)   ,intent(inout) :: gap,dep
  real(r_kind),dimension(nx,ny)     ,intent(in   ) :: p1
  real(r_kind),dimension(ndeg)      ,intent(in   ) :: be
  real(r_kind),dimension(nx,ny,ndeg),intent(in   ) :: alx
  real(r_kind),dimension(nx,ny)     ,intent(  out) :: p2

  integer(i_kind) kmod2,ix,iy,kr,ki

  real(r_kind) gakr,gaki,dekr,deki,bekr,beki

  kmod2=mod(ndeg,2_i_kind)

  if (kmod2 == ione) then  

!    Advancing filter:
     do ix=1,nx
        do iy=1,ny
           gap(1,iy)=alx(ix,iy,1)*gap(1,iy)+be(1)*p1(ix,iy)
           p2(ix,iy)=p2(ix,iy)+gap(1,iy)
        enddo

                           ! treat remaining complex roots:
        do kr=kmod2+ione,ndeg,2  ! <-- index of "real" components
           ki=kr+ione      ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do iy=1,ny
              gakr=gap(kr,iy)
              gaki=gap(ki,iy)
              gap(kr,iy)=alx(ix,iy,kr)*gakr&
                   -alx(ix,iy,ki)*gaki+bekr*p1(ix,iy)
              gap(ki,iy)=alx(ix,iy,ki)*gakr&
                   +alx(ix,iy,kr)*gaki+beki*p1(ix,iy)
              p2(ix,iy)=p2(ix,iy)+gap(kr,iy)
           enddo
        enddo
     enddo

! Backing filter:
     do ix=nx,1,-1
!       treat real roots
        do iy=1,ny
           p2(ix,iy)=p2(ix,iy)+dep(1,iy)
           dep(1,iy)=alx(ix,iy,1)*(dep(1,iy)+be(1)*p1(ix,iy))
        enddo
                           ! treat remaining complex roots:
        do kr=kmod2+ione,ndeg,2   ! <-- index of "real" components
           ki=kr+ione      ! <-- index of "imag" components
           do iy=1,ny
              p2(ix,iy)=p2(ix,iy)+dep(kr,iy)
              dekr=dep(kr,iy)+bekr*p1(ix,iy)
              deki=dep(ki,iy)+beki*p1(ix,iy)
              dep(kr,iy)=alx(ix,iy,kr)*dekr-alx(ix,iy,ki)*deki
              dep(ki,iy)=alx(ix,iy,ki)*dekr+alx(ix,iy,kr)*deki
           enddo
        enddo
     enddo

  else
     do iy=1,ny

        !       Advancing filter
        ! treat remaining complex roots:
        do kr=kmod2+ione,ndeg,2  ! <-- index of "real" components
           ki=kr+ione      ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do ix=1,nx
              gakr=gap(kr,iy)
              gaki=gap(ki,iy)
              gap(kr,iy)=alx(ix,iy,kr)*gakr&
                   -alx(ix,iy,ki)*gaki+bekr*p1(ix,iy)
              gap(ki,iy)=alx(ix,iy,ki)*gakr&
                   +alx(ix,iy,kr)*gaki+beki*p1(ix,iy)
              p2(ix,iy)=p2(ix,iy)+gap(kr,iy)
              
           end do
           
        !       Backing filter:
        ! treat remaining complex roots:
           do ix=nx,1,-1
              p2(ix,iy)=p2(ix,iy)+dep(kr,iy)
              dekr=dep(kr,iy)+bekr*p1(ix,iy)
              deki=dep(ki,iy)+beki*p1(ix,iy)
              dep(kr,iy)=alx(ix,iy,kr)*dekr-alx(ix,iy,ki)*deki
              dep(ki,iy)=alx(ix,iy,ki)*dekr+alx(ix,iy,kr)*deki
              
           enddo
        end do
     end do
  endif
  return
end subroutine rfhx0
! -----------------------------------------------------------------------------
subroutine rfhyt(p1,p2,nx,ny,ndegy,aly,be)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   rfhyt        performs x component of recursive filter
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: performs x component of recursive filter
!
! program history log:
!   2000-03-15  wu
!   2004-05-06  derber  combine regional, add multiple layers
!   2004-08-24  derber create rfhyt from rfhy - remove unnecessary computations
!                      remove unused parameters - change indexing
!
!   input argument list:
!     p1       - field to be smoothed
!     nx       - first dimension of p1
!     ny       - second dimension of p1
!     ndegy    - degree of smoothing y direction
!     aly      - smoothing coefficients y direction
!     be       - smoothing coefficients
!
!   output argument list:
!     p2       - field after smoothing
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$

  use kinds, only: r_kind,i_kind
  use constants, only: ione,zero
  implicit none

  integer(i_kind)                    ,intent(in   ) :: nx,ny,ndegy
  real(r_kind),dimension(nx,ny)      ,intent(in   ) :: p1
  real(r_kind),dimension(nx,ny,ndegy),intent(in   ) :: aly
  real(r_kind),dimension(ndegy)      ,intent(in   ) :: be
  real(r_kind),dimension(nx,ny)      ,intent(  out) :: p2

  integer(i_kind) kmod2,ix,iy,kr,ki,ly

  real(r_kind),dimension(nx,ndegy):: gap,dep
  real(r_kind) gakr,gaki,dekr,deki
  real(r_kind) beki,bekr

  kmod2=mod(ndegy,2_i_kind)

  do iy=1,ny
     do ix=1,nx
        p2(ix,iy)=zero
     enddo
  enddo
  do ly=1,ndegy
     do ix=1,nx
        gap(ix,ly)=zero
        dep(ix,ly)=zero
     enddo
  enddo

  if (kmod2 == ione) then

! Advancing filter:
     do iy=1,ny
!       treat the real root:
        do ix=1,nx
           gap(ix,1)=aly(ix,iy,1)*gap(ix,1)+be(1)*p1(ix,iy)
           p2(ix,iy)=p2(ix,iy)+gap(ix,1)
        enddo
                           ! treat remaining complex roots:
        do kr=kmod2+ione,ndegy,2  ! <-- index of "real" components
           ki=kr+ione      ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do ix=1,nx
              gakr=gap(ix,kr)
              gaki=gap(ix,ki)
              gap(ix,kr)=aly(ix,iy,kr)*gakr&
                   -aly(ix,iy,ki)*gaki+bekr*p1(ix,iy)
              gap(ix,ki)=aly(ix,iy,ki)*gakr&
                   +aly(ix,iy,kr)*gaki+beki*p1(ix,iy)
              p2(ix,iy)=p2(ix,iy)+gap(ix,kr)
           enddo
        enddo
     enddo

! Backing filter:
     do iy=ny,1,-1
!       treat the real root:
        do ix=1,nx
           p2(ix,iy)=p2(ix,iy)+dep(ix,1)
           dep(ix,1)=aly(ix,iy,1)*(dep(ix,1)+be(1)*p1(ix,iy))
        enddo
                           ! treat remaining complex roots:
        do kr=kmod2+ione,ndegy,2  ! <-- index of "real" components
           ki=kr+ione      ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do ix=1,nx
              p2(ix,iy)=p2(ix,iy)+dep(ix,kr)
              dekr=dep(ix,kr)+bekr*p1(ix,iy)
              deki=dep(ix,ki)+beki*p1(ix,iy)
              dep(ix,kr)=aly(ix,iy,kr)*dekr-aly(ix,iy,ki)*deki
              dep(ix,ki)=aly(ix,iy,ki)*dekr+aly(ix,iy,kr)*deki
           enddo
        enddo
     enddo

  else  

!    Advancing filter:
     do iy=1,ny
        ! treat remaining complex roots:
        do kr=kmod2+ione,ndegy,2  ! <-- index of "real" components
           ki=kr+ione      ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do ix=1,nx
              gakr=gap(ix,kr)
              gaki=gap(ix,ki)
              gap(ix,kr)=aly(ix,iy,kr)*gakr&
                   -aly(ix,iy,ki)*gaki+bekr*p1(ix,iy)
              gap(ix,ki)=aly(ix,iy,ki)*gakr&
                   +aly(ix,iy,kr)*gaki+beki*p1(ix,iy)
              p2(ix,iy)=p2(ix,iy)+gap(ix,kr)
           enddo
        enddo
     enddo
     
!    Backing filter:
     do iy=ny,1,-1
        ! treat remaining complex roots:
        do kr=kmod2+ione,ndegy,2  ! <-- index of "real" components
           ki=kr+ione      ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do ix=1,nx
              p2(ix,iy)=p2(ix,iy)+dep(ix,kr)
              dekr=dep(ix,kr)+bekr*p1(ix,iy)
              deki=dep(ix,ki)+beki*p1(ix,iy)
              dep(ix,kr)=aly(ix,iy,kr)*dekr-aly(ix,iy,ki)*deki
              dep(ix,ki)=aly(ix,iy,ki)*dekr+aly(ix,iy,kr)*deki
           enddo
        enddo
     enddo
  endif
  return
end subroutine rfhyt
! -----------------------------------------------------------------------------
subroutine rfhy(p1,p2,en2,e02,nx,ny,ndegx,ndegy,aly,be)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   rfhy         performs x component of recursive filter
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: performs x component of recursive filter
!
! program history log:
!   2000-03-15  wu
!   2004-05-06  derber  combine regional, add multiple layers
!   2004-08-24  derber  remove unused parameters and unnecessary computation
!                       change indexing
!
!   input argument list:
!     p1       - field to be smoothed
!     nx       - first dimension of p1
!     ny       - second dimension of p1
!     ndegx    - degree of smoothing x direction
!     ndegy    - degree of smoothing y direction
!     aly      - smoothing coefficients y direction
!     be       - smoothing coefficients
!
!   output argument list:
!     p2       - field after smoothing
!     en2      - boundary field (see rfxyyx) 
!     e02      - boundary field (see rfxyyx) 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$

  use kinds, only: r_kind,i_kind
  use constants, only: ione,zero
  implicit none

  integer(i_kind)                    ,intent(in   ) :: nx,ny,ndegx,ndegy
  real(r_kind),dimension(nx,ny)      ,intent(in   ) :: p1
  real(r_kind),dimension(nx,ny,ndegy),intent(in   ) :: aly
  real(r_kind),dimension(ndegy)      ,intent(in   ) :: be
  real(r_kind),dimension(nx,ny)      ,intent(  out) :: p2
  real(r_kind),dimension(ndegx,ny)   ,intent(  out) :: e02,en2

  integer(i_kind) kmod2,ix,iy,lx,kr,ki,ly

  real(r_kind) al0kr,al0ki,gakr,gaki,dekr,deki,alnkr,alnki
  real(r_kind) al01,aln1,beki,bekr

  real(r_kind),dimension(nx,ndegy):: gap,dep
  real(r_kind),dimension(ndegx,ndegy):: gae0,dee0,gaen,deen

  kmod2=mod(ndegy,2_i_kind)

  do iy=1,ny
     do ix=1,nx
        p2(ix,iy)=zero
     enddo
  enddo
  do iy=1,ny
     do lx=1,ndegx
        e02(lx,iy)=zero
        en2(lx,iy)=zero
     enddo
  enddo
  do ly=1,ndegy
     do ix=1,nx
        gap(ix,ly)=zero
        dep(ix,ly)=zero
     enddo
  enddo
  do ly=1,ndegy
     do lx=1,ndegx
        gae0(lx,ly)=zero
        dee0(lx,ly)=zero
        gaen(lx,ly)=zero
        deen(lx,ly)=zero
     end do
  end do

  if (kmod2 == ione) then

! Advancing filter:
     do iy=1,ny
!       treat the real root:
        do ix=1,nx
           gap(ix,1)=aly(ix,iy,1)*gap(ix,1)+be(1)*p1(ix,iy)
           p2(ix,iy)=p2(ix,iy)+gap(ix,1)
        enddo
        al01=aly( 1,iy,1)
        aln1=aly(nx,iy,1)
        do lx=1,ndegx
           gae0(lx,1)=al01*gae0(lx,1)
           e02(lx,iy)=e02(lx,iy)+gae0(lx,1)
           gaen(lx,1)=aln1*gaen(lx,1)
           en2(lx,iy)=en2(lx,iy)+gaen(lx,1)
        enddo
                           ! treat remaining complex roots:
        do kr=kmod2+ione,ndegy,2  ! <-- index of "real" components
           ki=kr+ione      ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do ix=1,nx
              gakr=gap(ix,kr)
              gaki=gap(ix,ki)
              gap(ix,kr)=aly(ix,iy,kr)*gakr&
                   -aly(ix,iy,ki)*gaki+bekr*p1(ix,iy)
              gap(ix,ki)=aly(ix,iy,ki)*gakr&
                   +aly(ix,iy,kr)*gaki+beki*p1(ix,iy)
              p2(ix,iy)=p2(ix,iy)+gap(ix,kr)
           enddo
           al0kr=aly( 1,iy,kr)
           al0ki=aly( 1,iy,ki)
           alnkr=aly(nx,iy,kr)
           alnki=aly(nx,iy,ki)
           do lx=1,ndegx
              gakr=gae0(lx,kr)
              gaki=gae0(lx,ki)
              gae0(lx,kr)=al0kr*gakr-al0ki*gaki
              gae0(lx,ki)=al0ki*gakr+al0kr*gaki
              e02(lx,iy)=e02(lx,iy)+gae0(lx,kr)
              gakr=gaen(lx,kr)
              gaki=gaen(lx,ki)
              gaen(lx,kr)=alnkr*gakr-alnki*gaki
              gaen(lx,ki)=alnki*gakr+alnkr*gaki
              en2(lx,iy)=en2(lx,iy)+gaen(lx,kr)
           enddo
        enddo
     enddo

! Backing filter:
     do iy=ny,1,-1
!       treat the real root:
        do ix=1,nx
           p2(ix,iy)=p2(ix,iy)+dep(ix,1)
           dep(ix,1)=aly(ix,iy,1)*(dep(ix,1)+be(1)*p1(ix,iy))
        enddo
        al01=aly( 1,iy,1)
        aln1=aly(nx,iy,1)
        do lx=1,ndegx
           e02(lx,iy)=e02(lx,iy)+dee0(lx,1)
           dee0(lx,1)=al01*dee0(lx,1)
           en2(lx,iy)=en2(lx,iy)+deen(lx,1)
           deen(lx,1)=aln1*deen(lx,1)
        enddo
                           ! treat remaining complex roots:
        do kr=kmod2+ione,ndegy,2  ! <-- index of "real" components
           ki=kr+ione      ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do ix=1,nx
              p2(ix,iy)=p2(ix,iy)+dep(ix,kr)
              dekr=dep(ix,kr)+bekr*p1(ix,iy)
              deki=dep(ix,ki)+beki*p1(ix,iy)
              dep(ix,kr)=aly(ix,iy,kr)*dekr-aly(ix,iy,ki)*deki
              dep(ix,ki)=aly(ix,iy,ki)*dekr+aly(ix,iy,kr)*deki
           enddo
           al0kr=aly( 1,iy,kr)
           al0ki=aly( 1,iy,ki)
           alnkr=aly(nx,iy,kr)
           alnki=aly(nx,iy,ki)
           do lx=1,ndegx
              e02(lx,iy)=e02(lx,iy)+dee0(lx,kr)
              dekr=dee0(lx,kr)
              deki=dee0(lx,ki)
              dee0(lx,kr)=al0kr*dekr-al0ki*deki
              dee0(lx,ki)=al0ki*dekr+al0kr*deki
              en2(lx,iy)=en2(lx,iy)+deen(lx,kr)
              dekr=deen(lx,kr)
              deki=deen(lx,ki)
              deen(lx,kr)=alnkr*dekr-alnki*deki
              deen(lx,ki)=alnki*dekr+alnkr*deki
           enddo
        enddo
     enddo

  else  

!    Advancing filter:
     do iy=1,ny
        ! treat remaining complex roots:
        do kr=kmod2+ione,ndegy,2  ! <-- index of "real" components
           ki=kr+ione      ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do ix=1,nx
              gakr=gap(ix,kr)
              gaki=gap(ix,ki)
              gap(ix,kr)=aly(ix,iy,kr)*gakr&
                   -aly(ix,iy,ki)*gaki+bekr*p1(ix,iy)
              gap(ix,ki)=aly(ix,iy,ki)*gakr&
                   +aly(ix,iy,kr)*gaki+beki*p1(ix,iy)
              p2(ix,iy)=p2(ix,iy)+gap(ix,kr)
           enddo
           al0kr=aly( 1,iy,kr)
           al0ki=aly( 1,iy,ki)
           alnkr=aly(nx,iy,kr)
           alnki=aly(nx,iy,ki)
           do lx=1,ndegx
              gakr=gae0(lx,kr)
              gaki=gae0(lx,ki)
              gae0(lx,kr)=al0kr*gakr-al0ki*gaki
              gae0(lx,ki)=al0ki*gakr+al0kr*gaki
              e02(lx,iy)=e02(lx,iy)+gae0(lx,kr)
              gakr=gaen(lx,kr)
              gaki=gaen(lx,ki)
              gaen(lx,kr)=alnkr*gakr-alnki*gaki
              gaen(lx,ki)=alnki*gakr+alnkr*gaki
              en2(lx,iy)=en2(lx,iy)+gaen(lx,kr)
           enddo
        enddo
     enddo
     
!    Backing filter:
     do iy=ny,1,-1
        ! treat remaining complex roots:
        do kr=kmod2+ione,ndegy,2  ! <-- index of "real" components
           ki=kr+ione      ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do ix=1,nx
              p2(ix,iy)=p2(ix,iy)+dep(ix,kr)
              dekr=dep(ix,kr)+bekr*p1(ix,iy)
              deki=dep(ix,ki)+beki*p1(ix,iy)
              dep(ix,kr)=aly(ix,iy,kr)*dekr-aly(ix,iy,ki)*deki
              dep(ix,ki)=aly(ix,iy,ki)*dekr+aly(ix,iy,kr)*deki
           enddo
           al0kr=aly( 1,iy,kr)
           al0ki=aly( 1,iy,ki)
           alnkr=aly(nx,iy,kr)
           alnki=aly(nx,iy,ki)
           do lx=1,ndegx
              e02(lx,iy)=e02(lx,iy)+dee0(lx,kr)
              dekr=dee0(lx,kr)
              deki=dee0(lx,ki)
              dee0(lx,kr)=al0kr*dekr-al0ki*deki
              dee0(lx,ki)=al0ki*dekr+al0kr*deki
              en2(lx,iy)=en2(lx,iy)+deen(lx,kr)
              dekr=deen(lx,kr)
              deki=deen(lx,ki)
              deen(lx,kr)=alnkr*dekr-alnki*deki
              deen(lx,ki)=alnki*dekr+alnkr*deki
           enddo
        enddo
     enddo
  endif
  return
end subroutine rfhy
! ------------------------------------------------------------------------------
subroutine sqrt_smoothrf(z,work,nsc,nlevs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    sqrt_smoothrf    perform sqrt horizontal part of background error
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: smoothrf perform horizontal part of background error
!
! program history log:
!   2000-03-15  wu
!   2004-05-06  derber - combine regional, add multiple layers
!   2004-08-27  kleist - new berror variable
!   2004-10-26  wu - give smallest RF half weight for regional wind variables
!   2004-11-03  treadon - pass horizontal scale weighting factors through berror
!   2004-11-22  derber - add openMP
!   2005-03-09  wgu/kleist - square hzscl in totwgt calculation
!   2005-05-27  kleist/parrish - add option to use new patch interpolation
!               if (norsp==0) will default to polar cascade
!   2005-11-16  wgu - set nmix=nr+1+(ny-nlat)/2 to make sure
!               nmix+nrmxb=nr no matter what number nlat is.   
!   input argument list:
!     work     - horizontal fields to be smoothed
!     nsc      - number of horizontal scales to smooth over 
!     nlevs    - number of vertical levels for smoothing
!
!   output argument list:
!     work     - smoothed horizontal field
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon,regional,nnnn1o
  use jfunc,only: nval_lenz
  use constants, only: izero,ione,zero,half
  use berror, only: wtaxs,wtxrs,inaxs,inxrs,bl,bl2,ii,jj,ii1,jj1,&
       ii2,jj2,slw,slw1,slw2,norh,nx,ny,mr,nr,nf,hzscl,hswgt
  use mpimod, only:  nvar_id
  use smooth_polcarf, only: norsp,smooth_polcas
  implicit none

! Declare passed variables
  integer(i_kind)                        ,intent(in   ) :: nsc,nlevs
  real(r_kind),dimension(nval_lenz)      ,intent(in   ) :: z
  real(r_kind),dimension(nlat,nlon,nlevs),intent(inout) :: work

! Declare local variables
  integer(i_kind) ndx,ndy,nxe,nmix,nfg
  integer(i_kind) j,naxr,i,nrmxb,nmixp,nymx,norm,nxem
  integer(i_kind) ndx2,nlatxb,nfnf
  integer(i_kind) i1,i2,j1,k,iz

  real(r_kind),dimension(nsc):: totwgt
  real(r_kind),dimension(ny,nx):: p1all
  real(r_kind),dimension(nlon+ione,mr:nr):: p2all,p3all
  real(r_kind),dimension(-nf:nf,-nf:nf):: afg1
  real(r_kind),dimension(nlat*nlon,nsc):: zloc
  real(r_kind),dimension(ny*nx,nsc):: zloc1
  real(r_kind),dimension((2*nf+ione)*(2*nf+ione),nsc):: zloc2,zloc3

! Regional case
  if(regional)then
!_$omp parallel do  schedule(dynamic,1) private(k,j,totwgt)
     do k=1,nlevs

!       apply horizontal recursive filters
        do j=1,nsc
           totwgt(j)=sqrt(hswgt(j)*hzscl(j)*hzscl(j))
        end do
        
        if(nvar_id(k)<3_i_kind)then
           totwgt(3)=sqrt(half)*totwgt(3)
        end if

        do j=1,nsc
           iz=nlat*nlon*(k-ione)+nlat*nlon*nnnn1o*(j-ione)
           do i=1,nlat*nlon
              zloc(i,j)=z(i+iz)
           end do
        end do
        
        call sqrt_rfxyyx(zloc,work(1,1,k),ny,nx,ii(1,1,1,k),&
             jj(1,1,1,k),slw(1,k),nsc,totwgt)
        
     end do

! Global case
  else

     do j=1,nsc
        totwgt(j)=sqrt(hswgt(j)*hzscl(j)*hzscl(j))
     end do
     
     ndx=(nx-nlon)/2
     ndy=(nlat-ny)/2
     ndx2=2*ndx
     norm=norh*2-ione
     nxe=nlon/8
     nxem=nxe-ione
     nmix=nr+ione+(ny-nlat)/2
     naxr=nlon+ione
     nfg=nf*2+ione
     nrmxb=ndy-ione
     nlatxb=nlat-nrmxb
     nmixp=nmix+ione
     nymx=ny-nmix
     nfnf=(2*nf+ione)*(2*nf+ione)
     
!_$omp parallel do  schedule(dynamic,1) private(k) &
!_$omp private(i,j,i1,i2,j1,p1all,p2all,p3all,afg1)
     do k=1,nlevs

!       Zero p1, p2, and p3
        do j=1,nx
           do i=1,ny
              p1all(i,j)=zero
           end do
        end do
        
        do j=1,nsc
           iz=(ny*nx+2*nfnf)*(k-ione)+(ny*nx+2*nfnf)*nnnn1o*(j-ione)
           do i=1,ny*nx
              zloc1(i,j)=z(i+iz)
           end do
           iz=iz+ny*nx
           do i=1,nfnf
              zloc2(i,j)=z(i+iz)
           end do
           iz=iz+nfnf
           do i=1,nfnf
              zloc3(i,j)=z(i+iz)
           end do
        end do

!       Recursive filter applications

!       First do equatorial/mid-latitude band
        call sqrt_rfxyyx(zloc1,p1all,ny,nx,ii(1,1,1,k),jj(1,1,1,k),slw(1,k),nsc,totwgt)

!       North pole patch --interpolate - recursive filter - adjoint interpolate

        call sqrt_rfxyyx(zloc2,afg1,nfg,nfg,ii1(1,1,1,k),jj1(1,1,1,k),slw1(1,k),nsc,totwgt)
        if(norsp>izero) then
           call smooth_polcas(afg1,p2all)
        else
           call polcas(afg1,p2all,nxem,norm,nlon,wtaxs,wtxrs,inaxs,inxrs,nf,mr,nr)
        end if

!       South pole patch --interpolate - recursive filter - adjoint interpolate
        call sqrt_rfxyyx(zloc3,afg1,nfg,nfg,ii2(1,1,1,k),jj2(1,1,1,k),slw2(1,k),nsc,totwgt)
        if(norsp>izero) then
           call smooth_polcas(afg1,p3all)
        else
           call polcas(afg1,p3all,nxem,norm,nlon,wtaxs,wtxrs,inaxs,inxrs,nf,mr,nr)
        end if

!       Equatorial patch
!       Adjoint of central patch blending on left/right sides of patch
        do i=1,ndx2
           i1=ndx2+ione-i
           i2=nx-ndx2+i
           do j=1,ny
              p1all(j,i) =p1all(j,i) *bl(i1)   ! left (west) blending zone
              p1all(j,i2)=p1all(j,i2)*bl(i)    ! right (east) blending zone
           enddo
        enddo

!       bl2 of p1
        do i=1,nx
           do j=1,nmix
              p1all(j,i)=p1all(j,i)*bl2(nmixp-j)
           enddo
           do j=nymx+ione,ny
              p1all(j,i)=p1all(j,i)*bl2(j-nymx)
           enddo
        enddo

!       zero output array
        do i=1,nlon
           do j=1,nlat
              work(j,i,k)=zero
           end do
        end do

!       Adjoint of transfer between central band and full grid (p1 --> work)
        do i=1,ndx
           i1=i-ndx+nlon
           i2=nx-ndx+i
           do j=1,ny
              j1=j+ndy
              work(j1,i1,k)=work(j1,i1,k)+p1all(j,i)  ! left (west) blending zone
              work(j1,i,k) =work(j1,i,k) +p1all(j,i2) ! right (east) blending zone
           enddo
        enddo

!       Middle zone (no blending)
        do i=ndx+ione,nx-ndx
           i1=i-ndx
           do j=1,ny
              j1=j+ndy
              work(j1,i1,k)=work(j1,i1,k)+p1all(j,i)
           enddo
        enddo
        
!       Adjoint of North pole patch(p2) -- blending and transfer to grid
!       Adjoint of South pole patch(p3) -- blending and transfer to grid

        do j=nlatxb-nmix,nlatxb-ione

!          Adjoint of blending
           do i=1,nlon
              p2all(i,nlat-j)=p2all(i,nlat-j)*bl2(nlatxb-j)
           enddo
        end do
        do j=nrmxb+ione,nrmxb+nmix

!          Adjoint of blending
           do i=1,nlon
              p3all(i,j)=p3all(i,j)*bl2(j-nrmxb)
           enddo
        enddo
        do i=1,nlon

!          Adjoint of transfer
           do j=mr,nrmxb+nmix
              work(j+ione,i,k)=work(j+ione,i,k)+p3all(i,j)
           enddo
           do j=nlatxb-nmix,nlat-mr
              work(j,i,k)=work(j,i,k)+p2all(i,nlat-j)
           enddo
        enddo

!    End of k loop over nlevs
     end do

! End of global block
  end if

  return
end subroutine sqrt_smoothrf
! ------------------------------------------------------------------------------
subroutine sqrt_smoothrf_ad(z,work,nsc,nlevs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    smoothrf    perform horizontal part of background error
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: smoothrf perform horizontal part of background error
!
! program history log:
!   2000-03-15  wu
!   2004-05-06  derber - combine regional, add multiple layers
!   2004-08-27  kleist - new berror variable
!   2004-10-26  wu - give smallest RF half weight for regional wind variables
!   2004-11-03  treadon - pass horizontal scale weighting factors through berror
!   2004-11-22  derber - add openMP
!   2005-03-09  wgu/kleist - square hzscl in totwgt calculation
!   2005-05-27  kleist/parrish - add option to use new patch interpolation
!               if (norsp==0) will default to polar cascade
!   2005-11-16  wgu - set nmix=nr+1+(ny-nlat)/2 to make sure
!               nmix+nrmxb=nr no matter what number nlat is.   
!   input argument list:
!     work     - horizontal fields to be smoothed
!     nsc      - number of horizontal scales to smooth over 
!     nlevs    - number of vertical levels for smoothing
!
!   output argument list:
!     work     - smoothed horizontal field
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon,nnnn1o,regional
  use jfunc,only: nval_lenz
  use constants, only: izero,ione,zero,half
  use berror, only: wtaxs,wtxrs,inaxs,inxrs,bl,bl2,ii,jj,ii1,jj1,&
       ii2,jj2,slw,slw1,slw2,norh,nx,ny,mr,nr,nf,hzscl,hswgt
  use mpimod, only:  nvar_id
  use smooth_polcarf, only: norsp,smooth_polcasa
  implicit none

! Declare passed variables
  integer(i_kind)                        ,intent(in   ) :: nsc,nlevs
  real(r_kind),dimension(nval_lenz)      ,intent(inout) :: z
  real(r_kind),dimension(nlat,nlon,nlevs),intent(inout) :: work

! Declare local variables
  integer(i_kind) ndx,ndy,nxe,nmix,nfg
  integer(i_kind) j,naxr,i,nrmxb,nmixp,nymx,norm,nxem
  integer(i_kind) ndx2,nlatxb,nfnf
  integer(i_kind) i1,i2,j1,k,iz

  real(r_kind),dimension(nsc):: totwgt
  real(r_kind),dimension(ny,nx):: p1all
  real(r_kind),dimension(nlon+ione,mr:nr):: p2all,p3all
  real(r_kind),dimension(-nf:nf,-nf:nf):: afg1
  real(r_kind),dimension(nlat*nlon,nsc):: zloc
  real(r_kind),dimension(ny*nx,nsc):: zloc1
  real(r_kind),dimension((2*nf+ione)*(2*nf+ione),nsc):: zloc2,zloc3


! Regional case
  if(regional)then
!_$omp parallel do  schedule(dynamic,1) private(k,j,totwgt)
     do k=1,nlevs

!       apply horizontal recursive filters
        do j=1,nsc
           totwgt(j)=sqrt(hswgt(j)*hzscl(j)*hzscl(j))
        end do

        if(nvar_id(k)<3_i_kind)then
           totwgt(3)=sqrt(half)*totwgt(3)
        end if
		
        call sqrt_rfxyyx_ad(zloc,work(1,1,k),ny,nx,ii(1,1,1,k),&
             jj(1,1,1,k),slw(1,k),nsc,totwgt)

        do j=1,nsc
           iz=nlat*nlon*(k-ione)+nlat*nlon*nnnn1o*(j-ione)
           do i=1,nlat*nlon
              z(i+iz)=zloc(i,j)
           end do
        end do
        
     end do

! Global case
  else

     do j=1,nsc
        totwgt(j)=sqrt(hswgt(j)*hzscl(j)*hzscl(j))
     end do
     
     ndx=(nx-nlon)/2
     ndy=(nlat-ny)/2
     ndx2=2*ndx
     norm=norh*2-ione
     nxe=nlon/8
     nxem=nxe-ione
     nmix=nr+ione+(ny-nlat)/2
     naxr=nlon+ione
     nfg=nf*2+ione
     nrmxb=ndy-ione
     nlatxb=nlat-nrmxb
     nmixp=nmix+ione
     nymx=ny-nmix
     nfnf=(2*nf+ione)*(2*nf+ione)

!  suspect a bug in this threading     
!_$omp parallel do  schedule(dynamic,1) private(k) &
!_$omp private(i,j,i1,i2,j1,p1all,p2all,p3all,afg1)
     do k=1,nlevs

!       Zero p1, p2, and p3
        do j=1,nx
           do i=1,ny
              p1all(i,j)=zero
           end do
        end do
        
!       Extract central patch (band) from full grid (work --> p1)
!       Blending zones
        do i=1,ndx
           i1=i-ndx+nlon
           i2=nx-ndx+i
           do j=1,ny
              j1=j+ndy
              p1all(j,i) =work(j1,i1,k)      ! left (west) blending zone
              p1all(j,i2)=work(j1,i,k)       ! right (east) blending zone
           enddo
        enddo

!       Middle zone (no blending)
        do i=ndx+ione,nx-ndx
           i1=i-ndx
           do j=1,ny
              p1all(j,i)=work(j+ndy,i1,k)
           enddo
        enddo
        
!       Apply blending coefficients to central patch
        do i=1,ndx2
           i1=ndx2+ione-i
           i2=nx-ndx2+i
           do j=1,ny
              p1all(j,i) =p1all(j,i) *bl(i1)  ! left (west) blending zone
              p1all(j,i2)=p1all(j,i2)*bl(i)   ! right (east) blending zone
           enddo
        enddo
        
!       bl2 of p1
        do i=1,nx
           do j=1,nmix
              p1all(j,i)=p1all(j,i)*bl2(nmixp-j)
           enddo
           do j=nymx+ione,ny
              p1all(j,i)=p1all(j,i)*bl2(j-nymx)
           enddo
        enddo

!       Handle polar patches 
        do j=mr,nr
           do i=1,naxr
              p2all(i,j)=zero
              p3all(i,j)=zero
           end do
        end do
        
!       North pole patch(p2) -- blending and transfer to grid
!       South pole patch(p3) -- blending and transfer to grid

        do i=1,nlon
!          Load field into patches
           do j=mr,nrmxb+nmix
              p2all(i,j)=work(nlat-j,i,k)
              p3all(i,j)=work(j+1,i,k)
           enddo
        enddo

!       Apply blending coefficients
        do j=nrmxb+ione,nrmxb+nmix
           j1=j-nrmxb
           do i=1,nlon
              p2all(i,j)=p2all(i,j)*bl2(j1)
              p3all(i,j)=p3all(i,j)*bl2(j1)
           enddo
        enddo
        
!       Recursive filter applications

!       First do equatorial/mid-latitude band
        call sqrt_rfxyyx_ad(zloc1,p1all,ny,nx,ii(1,1,1,k),jj(1,1,1,k),slw(1,k),nsc,totwgt)

!       North pole patch --interpolate - recursive filter - adjoint interpolate
        if(norsp>izero) then
           call smooth_polcasa(afg1,p2all)
        else
           call polcasa(afg1,p2all,nxem,norm,nlon,wtaxs,wtxrs,inaxs,inxrs,nf,mr,nr)
        end if
        call sqrt_rfxyyx_ad(zloc2,afg1,nfg,nfg,ii1(1,1,1,k),jj1(1,1,1,k),slw1(1,k),nsc,totwgt)

!       South pole patch --interpolate - recursive filter - adjoint interpolate
        if(norsp>izero) then
           call smooth_polcasa(afg1,p3all)
        else
           call polcasa(afg1,p3all,nxem,norm,nlon,wtaxs,wtxrs,inaxs,inxrs,nf,mr,nr)
        end if
        call sqrt_rfxyyx_ad(zloc3,afg1,nfg,nfg,ii2(1,1,1,k),jj2(1,1,1,k),slw2(1,k),nsc,totwgt)

        do j=1,nsc
           iz=(ny*nx+2*nfnf)*(k-ione)+(ny*nx+2*nfnf)*nnnn1o*(j-ione)
           do i=1,ny*nx
              z(i+iz)=z(i+iz)+zloc1(i,j)
           end do
           iz=iz+ny*nx
           do i=1,nfnf
              z(i+iz)=z(i+iz)+zloc2(i,j)
           end do
           iz=iz+nfnf
           do i=1,nfnf
              z(i+iz)=z(i+iz)+zloc3(i,j)
           end do
        end do

!    End of k loop over nlevs
     end do

! End of global block
  end if

  return
end subroutine sqrt_smoothrf_ad
! ------------------------------------------------------------------------------
subroutine sqrt_rfxyyx(z,p1,nx,ny,iix,jjx,dssx,nsc,totwgt)
  
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    sqrt_rfxyyx   sqrt perform horizontal smoothing
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: smoothrf perform self-adjoint horizontal smoothing. nsloop
!           smoothing fields.
!
! program history log:
!   2000-03-15  wu
!   2004-08-24  derber - change indexing add rfhyt to speed things up
!
!   input argument list:
!     p1       - horizontal field to be smoothed
!     nx       - first dimension of p1
!     ny       - second dimension of p1
!     iix      - array of pointers for smoothing table (first dimension)
!     jjx      - array of pointers for smoothing table (second dimension)
!     dssx     - renormalization constants including variance
!     wgt      - weight (empirical*expected)
!
!   output argument list:
!                 all after horizontal smoothing
!     p1       - horizontal field which has been smoothed
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only:  zero
  use berror, only: be,table,ndeg
  implicit none

! Declare passed variables
  integer(i_kind)                     ,intent(in   ) :: nx,ny,nsc
  integer(i_kind),dimension(nx,ny,nsc),intent(in   ) :: iix,jjx
  real(r_kind)   ,dimension(nx,ny,3)  ,intent(in   ) :: z
  real(r_kind)   ,dimension(nx,ny)    ,intent(inout) :: p1
  real(r_kind)   ,dimension(nx,ny)    ,intent(in   ) :: dssx
  real(r_kind)   ,dimension(nsc)      ,intent(in   ) :: totwgt

! Declare local variables
  integer(i_kind) ix,iy,i,j,im,n

  real(r_kind),dimension(nx,ny):: p2,p1out,p1t
  real(r_kind),dimension(ndeg,ny):: gax2,dex2
  real(r_kind),dimension(nx,ny,ndeg):: alx,aly

! Zero local arrays
  do iy=1,ny
     do ix=1,nx
        p1out(ix,iy)=zero
     enddo
  enddo

! Loop over number of scales
 
  do n=1,nsc

     do im=1,ndeg
        do j=1,ny
           do i=1,nx
              alx(i,j,im)=table(iix(i,j,n),im)
              aly(i,j,im)=table(jjx(i,j,n),im)
           enddo
        enddo
     enddo

     do iy=1,ny
        do ix=1,nx
           p1t(ix,iy)=z(ix,iy,n)*sqrt(dssx(ix,iy))*totwgt(n)
        enddo
     enddo


!    IX < 0     |          |     IX > NX
!   ---------------------------------------
!       GADEXY1 |   DEY1   |DEDEXY1        <-- IY > NY
!         GAX1	|    P1	   | DEX1
!       GAGAXY1 |   GAY1   |DEGAXY1        <-- IY < 0
!   ---------------------------------------

     call rfhy(p1t,p2,dex2,gax2,nx,ny,ndeg,ndeg,aly,be)


!    IX < 0     |          |     IX > NX
!   ---------------------------------------
!           .   |     .    |   .           <-- IY > NY
!         GAX2  |    P2	   | DEX2
!           .   |     .    |   .           <-- IY < 0
!   ---------------------------------------

     call rfhx0(p2,p1out,gax2,dex2,nx,ny,ndeg,alx,be)

!    IX < 0     |          |     IX > NX
!   ---------------------------------------
!           .   |     .	   |  .            <-- IY > NY
!           .   |    P1	   |  .
!           .   |     .	   |  .            <-- IY < 0
!   ---------------------------------------

! end loop over number of horizontal scales
  end do

  do iy=1,ny
     do ix=1,nx
        p1(ix,iy)=p1out(ix,iy)
     enddo
  enddo

  return
end subroutine sqrt_rfxyyx
! ------------------------------------------------------------------------------
subroutine sqrt_rfxyyx_ad(z,p1,nx,ny,iix,jjx,dssx,nsc,totwgt)
  
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    rfxyyx      perform horizontal smoothing
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: smoothrf perform self-adjoint horizontal smoothing. nsloop
!           smoothing fields.
!
! program history log:
!   2000-03-15  wu
!   2004-08-24  derber - change indexing add rfhyt to speed things up
!
!   input argument list:
!     p1       - horizontal field to be smoothed
!     nx       - first dimension of p1
!     ny       - second dimension of p1
!     iix      - array of pointers for smoothing table (first dimension)
!     jjx      - array of pointers for smoothing table (second dimension)
!     dssx     - renormalization constants including variance
!     wgt      - weight (empirical*expected)
!
!   output argument list:
!                 all after horizontal smoothing
!     p1       - horizontal field which has been smoothed
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only:  zero
  use berror, only: be,table,ndeg
  implicit none

! Declare passed variables
  integer(i_kind)                     ,intent(in   ) :: nx,ny,nsc
  integer(i_kind),dimension(nx,ny,nsc),intent(in   ) :: iix,jjx
  real(r_kind)   ,dimension(nx,ny,3)  ,intent(  out) :: z
  real(r_kind)   ,dimension(nx,ny)    ,intent(inout) :: p1
  real(r_kind)   ,dimension(nx,ny)    ,intent(in   ) :: dssx
  real(r_kind)   ,dimension(nsc)      ,intent(in   ) :: totwgt

! Declare local variables
  integer(i_kind) ix,iy,i,j,im,n

  real(r_kind),dimension(nx,ny):: p2,p1t
  real(r_kind),dimension(ndeg,ny):: gax2,dex2
  real(r_kind),dimension(nx,ny,ndeg):: alx,aly

! Loop over number of scales
 
  do n=1,nsc

     do j=1,ny
        do i=1,ndeg
           gax2(i,j)=zero
           dex2(i,j)=zero
        end do
     end do
     do iy=1,ny
        do ix=1,nx
           p2(ix,iy)=zero
        enddo
     enddo
     do im=1,ndeg
        do j=1,ny
           do i=1,nx
              alx(i,j,im)=table(iix(i,j,n),im)
              aly(i,j,im)=table(jjx(i,j,n),im)
           enddo
        enddo
     enddo

!    IX < 0     |          |     IX > NX
!   ---------------------------------------
!           .   |     .	   |  .            <-- IY > NY
!           .   |    P1	   |  .
!           .   |     .	   |  .            <-- IY < 0
!   ---------------------------------------


     call rfhx0(p1,p2,gax2,dex2,nx,ny,ndeg,alx,be)


!    IX < 0     |          |     IX > NX
!   ---------------------------------------
!           .   |     .	   |  .            <-- IY > NY
!	  DEX2  |    P2	   | GAX2
!           .   |     .	   |  .            <-- IY < 0
!   ---------------------------------------

     call rfhyt(p2,p1t,nx,ny,ndeg,aly,be)


!    IX < 0     |          |     IX > NX
!   ---------------------------------------
!       DEGAXY1 |   GAY1   |GAGAXY1        <-- IY > NY
!         DEX1  |    P1	   | GAX1
!       DEDEXY1 |   DEY1   |GADEXY1        <-- IY < 0
!   ---------------------------------------


     do iy=1,ny
        do ix=1,nx
           z(ix,iy,n)=p1t(ix,iy)*sqrt(dssx(ix,iy))*totwgt(n)
        enddo
     enddo

! end loop over number of horizontal scales
  end do

  return
end subroutine sqrt_rfxyyx_ad
! ------------------------------------------------------------------------------
