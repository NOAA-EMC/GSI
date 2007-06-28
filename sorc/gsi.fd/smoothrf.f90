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
  use gridmod, only: nlat,nlon,nsig1o,regional
  use constants, only:  zero,half
  use berror, only: wtaxs,wtxrs,inaxs,inxrs,bl,bl2,ii,jj,ii1,jj1,&
       ii2,jj2,slw,slw1,slw2,norh,nx,ny,mr,nr,nf,hzscl,nlath,hswgt
  use mpimod, only:  levs_id,nvar_id
  use smooth_polcarf, only: norsp,smooth_polcas,smooth_polcasa
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: nsc,nlevs
  real(r_kind),dimension(nlat,nlon,nsig1o),intent(inout):: work

! Declare local variables
  integer(i_kind) ndx,ndy,nxe,nmix,narx,nfg
  integer(i_kind) j,naxr,i,nrmxb,nmixp,nymx,norm,nxem
  integer(i_kind) ndx2,nlatxb,mmm,nfnf
  integer(i_kind) ix,iy,i1,i2,j1,k

  real(r_kind),dimension(nsc):: totwgt
  real(r_kind),dimension(ny,nx):: p1all
  real(r_kind),dimension(nlon+1,mr:nr):: p2all,p3all
  real(r_kind),dimension(-nf:nf,-nf:nf):: afg1


! Regional case
  if(regional)then
!$omp parallel do  schedule(dynamic,1) private(k,j,totwgt)
     do k=1,nlevs

!       apply horizontal recursive filters
        do j=1,nsc
           totwgt(j)=hswgt(j)*hzscl(j)*hzscl(j)
        end do
        
        if(nvar_id(k)<3)then
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
     norm=norh*2-1
     nxe=nlon/8
     nxem=nxe-1
     nmix=nr+1+(ny-nlat)/2
     naxr=nlon+1 
     nfg=nf*2+1
     nrmxb=ndy-1
     nlatxb=nlat-nrmxb
     nmixp=nmix+1
     nymx=ny-nmix
     nfnf=(2*nf+1)*(2*nf+1)
     
!$omp parallel do  schedule(dynamic,1) private(k) &
!$omp private(i,j,i1,i2,j1,p1all,p2all,p3all,afg1)
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
        do i=ndx+1,nx-ndx
           i1=i-ndx
           do j=1,ny
              p1all(j,i)=work(j+ndy,i1,k)
           enddo
        enddo
        
!       Apply blending coefficients to central patch
        do i=1,ndx2
           i1=ndx2+1-i
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
           do j=nymx+1,ny
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
        do j=nrmxb+1,nrmxb+nmix
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
        if(norsp.gt.0) then
          call smooth_polcasa(afg1,p2all)
        else
          call polcasa(afg1,p2all,nxem,norm,nlon,wtaxs,wtxrs,inaxs,inxrs,nf,mr,nr)
        end if
        call rfxyyx(afg1,nfg,nfg,ii1(1,1,1,k),jj1(1,1,1,k),slw1(1,k),nsc,totwgt)
        if(norsp.gt.0) then
          call smooth_polcas(afg1,p2all)
        else
          call polcas(afg1,p2all,nxem,norm,nlon,wtaxs,wtxrs,inaxs,inxrs,nf,mr,nr)
        end if

!       South pole patch --interpolate - recursive filter - adjoint interpolate
        if(norsp.gt.0) then
          call smooth_polcasa(afg1,p3all)
        else
          call polcasa(afg1,p3all,nxem,norm,nlon,wtaxs,wtxrs,inaxs,inxrs,nf,mr,nr)
        end if
        call rfxyyx(afg1,nfg,nfg,ii2(1,1,1,k),jj2(1,1,1,k),slw2(1,k),nsc,totwgt)
        if(norsp.gt.0) then
          call smooth_polcas(afg1,p3all)
        else
          call polcas(afg1,p3all,nxem,norm,nlon,wtaxs,wtxrs,inaxs,inxrs,nf,mr,nr)
        end if


!       Equatorial patch
!       Adjoint of central patch blending on left/right sides of patch
        do i=1,ndx2
           i1=ndx2+1-i
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
           do j=nymx+1,ny
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
        do i=ndx+1,nx-ndx
           i1=i-ndx
           do j=1,ny
              j1=j+ndy
              work(j1,i1,k)=work(j1,i1,k)+p1all(j,i)
           enddo
        enddo
        
!       Adjoint of North pole patch(p2) -- blending and transfer to grid
!       Adjoint of South pole patch(p3) -- blending and transfer to grid

        do j=nlatxb-nmix,nlatxb-1

!          Adjoint of blending
           do i=1,nlon
              p2all(i,nlat-j)=p2all(i,nlat-j)*bl2(nlatxb-j)
           enddo
        end do
        do j=nrmxb+1,nrmxb+nmix

!          Adjoint of blending
           do i=1,nlon
              p3all(i,j)=p3all(i,j)*bl2(j-nrmxb)
           enddo
        enddo
        do i=1,nlon

!          Adjoint of transfer
           do j=mr,nrmxb+nmix
              work(j+1,i,k)=work(j+1,i,k)+p3all(i,j)
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
  integer(i_kind),intent(in):: nx,ny,nsc
  integer(i_kind),dimension(nx,ny,nsc),intent(in):: iix,jjx
  real(r_kind),dimension(nx,ny),intent(inout):: p1
  real(r_kind),dimension(nx,ny),intent(in):: dssx
  real(r_kind),dimension(nsc),intent(in):: totwgt

! Declare local variables
  integer(i_kind) ix,iy,i,j,im,n

  real(r_kind),dimension(nx,ny):: p2,p1out,p1t
  real(r_kind),dimension(ndeg,ny):: gax1,dex1,gax2,dex2
  real(r_kind),dimension(nx,ny,ndeg):: alx,aly
  real(r_kind) wgt

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

!   IX < 0	|	   |	 IX > NX
!  ---------------------------------------
!	    .	|     .	   |  . 	   <-- IY > NY
!	    .	|    P1	   |  .
!	    .	|     .	   |  . 	   <-- IY < 0
!  ---------------------------------------


    call rfhx0(p1,p2,gax2,dex2,nx,ny,ndeg,alx,be)


!   IX < 0	|	   |	 IX > NX
!  ---------------------------------------
!	    .	|     .	   |  . 	   <-- IY > NY
!	  DEX2	|    P2	   | GAX2
!	    .	|     .	   |  . 	   <-- IY < 0
!  ---------------------------------------

    call rfhyt(p2,p1t,nx,ny,ndeg,ndeg,aly,be)


!   IX < 0	|	   |	 IX > NX
!  ---------------------------------------
!	DEGAXY1 |   GAY1   |GAGAXY1	   <-- IY > NY
!	  DEX1	|    P1	   | GAX1
!	DEDEXY1 |   DEY1   |GADEXY1	   <-- IY < 0
!  ---------------------------------------


    do iy=1,ny
      do ix=1,nx
        p1t(ix,iy)=p1t(ix,iy)*dssx(ix,iy)*totwgt(n)
      enddo
    enddo


!   IX < 0	|	   |	 IX > NX
!  ---------------------------------------
!	GADEXY1 |   DEY1   |DEDEXY1	   <-- IY > NY
!	  GAX1	|    P1	   | DEX1
!	GAGAXY1 |   GAY1   |DEGAXY1	   <-- IY < 0
!  ---------------------------------------

    call rfhy(p1t,p2,dex2,gax2,nx,ny,ndeg,ndeg,aly,be)


!   IX < 0	|	   |	 IX > NX
!  ---------------------------------------
!	    .	|     .    |   .	   <-- IY > NY
!	  GAX2	|    P2	   | DEX2
!	    .	|     .    |   .	   <-- IY < 0
!  ---------------------------------------

    call rfhx0(p2,p1out,gax2,dex2,nx,ny,ndeg,alx,be)

!   IX < 0	|	   |	 IX > NX
!  ---------------------------------------
!	    .	|     .	   |  . 	   <-- IY > NY
!	    .	|    P1	   |  .
!	    .	|     .	   |  . 	   <-- IY < 0
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
  implicit none

  integer(i_kind),intent(in):: nx,ny,ndeg
  real(r_kind),intent(inout),dimension(ndeg,ny):: gap,dep
  real(r_kind),intent(in),dimension(nx,ny):: p1
  real(r_kind),intent(in),dimension(ndeg):: be
  real(r_kind),intent(in),dimension(nx,ny,ndeg):: alx
  real(r_kind),intent(out),dimension(nx,ny):: p2

  integer(i_kind) kmod2,ix,iy,kr,ki

  real(r_kind) gakr,gaki,bekr,beki
  real(r_kind) oldr, oldi, oldr0, oldr1, oldi0, oldi1
  real(r_kind) newr, newi, newr0, newr1, newi0, newi1
  real(r_kind) depr, depi, depr0, depr1, depi0, depi1
  real(r_kind) dekr, deki, dekr0, dekr1, deki0, deki1

  kmod2=mod(ndeg,2)

  if (kmod2 == 1) then  

!    Advancing filter:
     do ix=1,nx
        do iy=1,ny
           gap(1,iy)=alx(ix,iy,1)*gap(1,iy)+be(1)*p1(ix,iy)
           p2(ix,iy)=p2(ix,iy)+gap(1,iy)
        enddo

			   ! treat remaining complex roots:
        do kr=kmod2+1,ndeg,2  ! <-- index of "real" components
           ki=kr+1 	   ! <-- index of "imag" components
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
        do kr=kmod2+1,ndeg,2   ! <-- index of "real" components
           ki=kr+1 	   ! <-- index of "imag" components
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
!YKT unroll 2x by hand
     do iy=1,ny-1,2

        !       Advancing filter
        ! treat remaining complex roots:
        do kr=kmod2+1,ndeg,2  ! <-- index of "real" components
           ki=kr+1 	   ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           oldr0 = gap(kr,iy)
           oldi0 = gap(ki,iy)
           oldr1 = gap(kr,iy+1)
           oldi1 = gap(ki,iy+1)
           do ix=1,nx
              newr0 = alx(ix,iy  ,kr)*oldr0 - alx(ix,iy  ,ki)*oldi0 + bekr*p1(ix,iy  )
              newi0 = alx(ix,iy  ,ki)*oldr0 + alx(ix,iy  ,kr)*oldi0 + beki*p1(ix,iy  )
              newr1 = alx(ix,iy+1,kr)*oldr1 - alx(ix,iy+1,ki)*oldi1 + bekr*p1(ix,iy+1)
              newi1 = alx(ix,iy+1,ki)*oldr1 + alx(ix,iy+1,kr)*oldi1 + beki*p1(ix,iy+1)
              p2(ix,iy  ) = p2(ix,iy  ) + newr0
              p2(ix,iy+1) = p2(ix,iy+1) + newr1
              oldr0 = newr0
              oldi0 = newi0
              oldr1 = newr1
              oldi1 = newi1
           end do
           gap(kr,iy  ) = oldr0
           gap(ki,iy  ) = oldi0
           gap(kr,iy+1) = oldr1
           gap(ki,iy+1) = oldi1
           
        !       Backing filter:
        ! treat remaining complex roots:
           depr0 = dep(kr,iy  )
           depi0 = dep(ki,iy  )
           depr1 = dep(kr,iy+1)
           depi1 = dep(ki,iy+1)
           do ix=nx,1,-1
              p2(ix,iy  ) = p2(ix,iy  ) + depr0
              p2(ix,iy+1) = p2(ix,iy+1) + depr1
              dekr0 = depr0 + bekr*p1(ix,iy  )
              deki0 = depi0 + beki*p1(ix,iy  )
              dekr1 = depr1 + bekr*p1(ix,iy+1)
              deki1 = depi1 + beki*p1(ix,iy+1)
              depr0 = alx(ix,iy,kr)*dekr0 - alx(ix,iy,ki)*deki0
              depi0 = alx(ix,iy,ki)*dekr0 + alx(ix,iy,kr)*deki0
              depr1 = alx(ix,iy+1,kr)*dekr1 - alx(ix,iy+1,ki)*deki1
              depi1 = alx(ix,iy+1,ki)*dekr1 + alx(ix,iy+1,kr)*deki1
           enddo
           dep(kr,iy  ) = depr0
           dep(ki,iy  ) = depi0
           dep(kr,iy+1) = depr1
           dep(ki,iy+1) = depi1
        end do
     end do
     do iy=iy,ny

        !       Advancing filter
        ! treat remaining complex roots:
        do kr=kmod2+1,ndeg,2  ! <-- index of "real" components
           ki=kr+1 	   ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           oldr = gap(kr,iy)
           oldi = gap(ki,iy)
           do ix=1,nx
              newr = alx(ix,iy,kr)*oldr - alx(ix,iy,ki)*oldi + bekr*p1(ix,iy)
              newi = alx(ix,iy,ki)*oldr + alx(ix,iy,kr)*oldi + beki*p1(ix,iy)
              p2(ix,iy) = p2(ix,iy) + newr
              oldr = newr
              oldi = newi
           end do
           gap(kr,iy) = oldr
           gap(ki,iy) = oldi
           
        !       Backing filter:
        ! treat remaining complex roots:
           depr = dep(kr,iy)
           depi = dep(ki,iy)
           do ix=nx,1,-1
              p2(ix,iy) = p2(ix,iy) + depr
              dekr = depr + bekr*p1(ix,iy)
              deki = depi + beki*p1(ix,iy)
              depr = alx(ix,iy,kr)*dekr - alx(ix,iy,ki)*deki
              depi = alx(ix,iy,ki)*dekr + alx(ix,iy,kr)*deki
           enddo
           dep(kr,iy) = depr
           dep(ki,iy) = depi
        end do
     end do
  endif
  return
end subroutine rfhx0

subroutine rfhyt(p1,p2,nx,ny,ndegx,ndegy,aly,be)

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
!     ndegx    - degree of smoothing x direction
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
  use constants, only:  zero
  implicit none

  integer(i_kind),intent(in):: nx,ny,ndegx,ndegy
  real(r_kind),intent(in),dimension(nx,ny):: p1
  real(r_kind),intent(in),dimension(nx,ny,ndegy):: aly
  real(r_kind),intent(in),dimension(ndegy):: be
  real(r_kind),intent(out),dimension(nx,ny):: p2

  integer(i_kind) kmod2,ix,iy,lx,kr,ki,ly

  real(r_kind),dimension(nx,ndegy):: gap,dep
  real(r_kind) gakr,gaki,dekr,deki
  real(r_kind) beki,bekr
  real(r_kind) gakr0, gakr1, gaki0, gaki1
  real(r_kind) dekr0, dekr1, deki0, deki1

  kmod2=mod(ndegy,2)

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

  if (kmod2 == 1) then

! Advancing filter:
     do iy=1,ny
!       treat the real root:
        do ix=1,nx
           gap(ix,1)=aly(ix,iy,1)*gap(ix,1)+be(1)*p1(ix,iy)
           p2(ix,iy)=p2(ix,iy)+gap(ix,1)
        enddo
			   ! treat remaining complex roots:
        do kr=kmod2+1,ndegy,2  ! <-- index of "real" components
           ki=kr+1 	   ! <-- index of "imag" components
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
        do kr=kmod2+1,ndegy,2  ! <-- index of "real" components
           ki=kr+1 	   ! <-- index of "imag" components
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
        do kr=kmod2+1,ndegy,2  ! <-- index of "real" components
           ki=kr+1 	   ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
!YKT unroll 2x by hand
           do ix=1,nx-1,2
              gakr0 = gap(ix  ,kr)
              gakr1 = gap(ix+1,kr)
              gaki0 = gap(ix  ,ki)
              gaki1 = gap(ix+1,ki)
              gap(ix  ,kr) = aly(ix  ,iy,kr)*gakr0 - aly(ix  ,iy,ki)*gaki0 + bekr*p1(ix  ,iy)
              gap(ix+1,kr) = aly(ix+1,iy,kr)*gakr1 - aly(ix+1,iy,ki)*gaki1 + bekr*p1(ix+1,iy)
              gap(ix  ,ki) = aly(ix  ,iy,ki)*gakr0 + aly(ix  ,iy,kr)*gaki0 + beki*p1(ix  ,iy)
              gap(ix+1,ki) = aly(ix+1,iy,ki)*gakr1 + aly(ix+1,iy,kr)*gaki1 + beki*p1(ix+1,iy)
              p2(ix  ,iy) = p2(ix  ,iy) + gap(ix  ,kr)
              p2(ix+1,iy) = p2(ix+1,iy) + gap(ix+1,kr)
           enddo
           do ix=ix,nx
              gakr = gap(ix,kr)
              gaki = gap(ix,ki)
              gap(ix,kr) = aly(ix,iy,kr)*gakr - aly(ix,iy,ki)*gaki + bekr*p1(ix,iy)
              gap(ix,ki) = aly(ix,iy,ki)*gakr + aly(ix,iy,kr)*gaki + beki*p1(ix,iy)
              p2(ix,iy) = p2(ix,iy) + gap(ix,kr)
           enddo
        enddo
     enddo
     
!    Backing filter:
     do iy=ny,1,-1
        ! treat remaining complex roots:
        do kr=kmod2+1,ndegy,2  ! <-- index of "real" components
           ki=kr+1 	   ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
!YKT unroll 2x by hand
           do ix=1,nx-1,2
              p2(ix  ,iy) = p2(ix  ,iy) + dep(ix  ,kr)
              p2(ix+1,iy) = p2(ix+1,iy) + dep(ix+1,kr)
              dekr0 = dep(ix  ,kr) + bekr*p1(ix  ,iy)
              dekr1 = dep(ix+1,kr) + bekr*p1(ix+1,iy)
              deki0 = dep(ix  ,ki) + beki*p1(ix  ,iy)
              deki1 = dep(ix+1,ki) + beki*p1(ix+1,iy)
              dep(ix  ,kr) = aly(ix  ,iy,kr)*dekr0 - aly(ix  ,iy,ki)*deki0
              dep(ix+1,kr) = aly(ix+1,iy,kr)*dekr1 - aly(ix+1,iy,ki)*deki1
              dep(ix  ,ki) = aly(ix  ,iy,ki)*dekr0 + aly(ix  ,iy,kr)*deki0
              dep(ix+1,ki) = aly(ix+1,iy,ki)*dekr1 + aly(ix+1,iy,kr)*deki1
           enddo
           do ix=ix,nx
              p2(ix,iy) = p2(ix,iy) + dep(ix,kr)
              dekr = dep(ix,kr) + bekr*p1(ix,iy)
              deki = dep(ix,ki) + beki*p1(ix,iy)
              dep(ix,kr) = aly(ix,iy,kr)*dekr - aly(ix,iy,ki)*deki
              dep(ix,ki) = aly(ix,iy,ki)*dekr + aly(ix,iy,kr)*deki
           enddo
        enddo
     enddo
  endif
  return
end subroutine rfhyt

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
  use constants, only:  zero
  implicit none

  integer(i_kind),intent(in):: nx,ny,ndegx,ndegy
  real(r_kind),intent(in),dimension(nx,ny):: p1
  real(r_kind),intent(in),dimension(nx,ny,ndegy):: aly
  real(r_kind),intent(in),dimension(ndegy):: be
  real(r_kind),intent(out),dimension(nx,ny):: p2
  real(r_kind),intent(out),dimension(ndegx,ny):: e02,en2

  integer(i_kind) kmod2,ix,iy,lx,kr,ki,ly

  real(r_kind) al0kr,al0ki,gakr,gaki,dekr,deki,alnkr,alnki
  real(r_kind) al01,aln1,be1,beki,bekr

  real(r_kind),dimension(nx,ndegy):: gap,dep
  real(r_kind),dimension(ndegx,ndegy):: gae0,dee0,gaen,deen
  real(r_kind) gakr0, gakr1, gaki0, gaki1
  real(r_kind) dekr0, dekr1, deki0, deki1

  kmod2=mod(ndegy,2)

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

  if (kmod2 == 1) then

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
        do kr=kmod2+1,ndegy,2  ! <-- index of "real" components
           ki=kr+1 	   ! <-- index of "imag" components
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
        do kr=kmod2+1,ndegy,2  ! <-- index of "real" components
           ki=kr+1 	   ! <-- index of "imag" components
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
        do kr=kmod2+1,ndegy,2  ! <-- index of "real" components
           ki=kr+1 	   ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
!YKT unroll 2x by hand
           do ix=1,nx-1,2
              gakr0 = gap(ix  ,kr)
              gakr1 = gap(ix+1,kr)
              gaki0 = gap(ix  ,ki)
              gaki1 = gap(ix+1,ki)
              gap(ix  ,kr) = aly(ix  ,iy,kr)*gakr0 - aly(ix  ,iy,ki)*gaki0 + bekr*p1(ix  ,iy)
              gap(ix+1,kr) = aly(ix+1,iy,kr)*gakr1 - aly(ix+1,iy,ki)*gaki1 + bekr*p1(ix+1,iy)
              gap(ix  ,ki) = aly(ix  ,iy,ki)*gakr0 + aly(ix  ,iy,kr)*gaki0 + beki*p1(ix  ,iy)
              gap(ix+1,ki) = aly(ix+1,iy,ki)*gakr1 + aly(ix+1,iy,kr)*gaki1 + beki*p1(ix+1,iy)
              p2(ix  ,iy) = p2(ix  ,iy) + gap(ix  ,kr)
              p2(ix+1,iy) = p2(ix+1,iy) + gap(ix+1,kr)
           enddo
           do ix=ix,nx
              gakr=gap(ix,kr)
              gaki=gap(ix,ki)
              gap(ix,kr) = aly(ix,iy,kr)*gakr - aly(ix,iy,ki)*gaki + bekr*p1(ix,iy)
              gap(ix,ki) = aly(ix,iy,ki)*gakr + aly(ix,iy,kr)*gaki + beki*p1(ix,iy)
              p2(ix,iy) = p2(ix,iy) + gap(ix,kr)
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
        do kr=kmod2+1,ndegy,2  ! <-- index of "real" components
           ki=kr+1 	   ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
!YKT unroll 2x by hand
           do ix=1,nx-1,2
              p2(ix  ,iy) = p2(ix  ,iy) + dep(ix  ,kr)
              p2(ix+1,iy) = p2(ix+1,iy) + dep(ix+1,kr)
              dekr0 = dep(ix  ,kr) + bekr*p1(ix  ,iy)
              dekr1 = dep(ix+1,kr) + bekr*p1(ix+1,iy)
              deki0 = dep(ix  ,ki) + beki*p1(ix  ,iy)
              deki1 = dep(ix+1,ki) + beki*p1(ix+1,iy)
              dep(ix  ,kr) = aly(ix  ,iy,kr)*dekr0 - aly(ix  ,iy,ki)*deki0
              dep(ix+1,kr) = aly(ix+1,iy,kr)*dekr1 - aly(ix+1,iy,ki)*deki1
              dep(ix  ,ki) = aly(ix  ,iy,ki)*dekr0 + aly(ix  ,iy,kr)*deki0
              dep(ix+1,ki) = aly(ix+1,iy,ki)*dekr1 + aly(ix+1,iy,kr)*deki1
           enddo
           do ix=ix,nx
              p2(ix,iy) = p2(ix,iy) + dep(ix,kr)
              dekr = dep(ix,kr) + bekr*p1(ix,iy)
              deki = dep(ix,ki) + beki*p1(ix,iy)
              dep(ix,kr) = aly(ix,iy,kr)*dekr - aly(ix,iy,ki)*deki
              dep(ix,ki) = aly(ix,iy,ki)*dekr + aly(ix,iy,kr)*deki
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

