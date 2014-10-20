module specgrid
  use kinds, only: r_kind,r_double
  implicit none

  integer jcap,jcapin,jcapsmooth,nc,ncin,ncd2
  integer iromb,idrt,imax,jmax,ijmax,jn,js,kw,jb,je,jc,ioffset
  real(r_kind),allocatable,dimension(:):: factsml,factvml
  real(r_kind),allocatable,dimension(:):: eps,epstop,enn1,elonn1,eon,eontop
  real(r_kind),allocatable,dimension(:):: clat,slat,wlat
  real(r_kind),allocatable,dimension(:,:):: pln,plntop
  real(r_double),allocatable,dimension(:):: afft

contains

  subroutine init_spec_vars(nlat,nlon,nsig)
    implicit none

    integer,intent(in):: nlat,nlon,nsig
    integer ii,ii1,l,m,ncpus
    real(r_kind) zero1

!   Set constants
    nc=(jcap+1)*(jcap+2)
    ncin=(jcapin+1)*(jcapin+2)
    ncd2=nc/2

!   Allocate more arrays related to transforms
    allocate(factsml(nc),factvml(nc))
!   Set up factsml and factvml
    ii=-1; ii1=0
    do l=0,jcap
       zero1=float(min(1,l))
       do m=0,jcap-l
          ii=ii+2; ii1=ii1+2
          factsml(ii)=1.; factsml(ii1)=zero1
          factvml(ii)=1.; factvml(ii1)=zero1
       end do
    end do
    factvml(1)=0.

!   Set other constants used in transforms
    idrt=4
    imax=nlon
    jmax=nlat-2
    ijmax=imax*jmax
    ioffset=imax*(jmax-1)
    jn=imax
    js=-jn
    kw=2*ncd2
    jb=1
    je=(jmax+1)/2
    jc=ncpus()

!   Allocate arrays
    allocate( eps(ncd2) )
    allocate( epstop(jcap+1) )
    allocate( enn1(ncd2) )
    allocate( elonn1(ncd2) )
    allocate( eon(ncd2) )
    allocate( eontop(jcap+1) )
    allocate( afft(50000+4*imax) )
    allocate( clat(jb:je) )
    allocate( slat(jb:je) )
    allocate( wlat(jb:je) )
    allocate( pln(ncd2,jb:je) )
    allocate( plntop(jcap+1,jb:je) )

!   Initialize arrays used in transforms
    call sptranf0(iromb,jcap,idrt,imax,jmax,jb,je, &
       eps,epstop,enn1,elonn1,eon,eontop, &
       afft,clat,slat,wlat,pln,plntop)

    return
  end subroutine init_spec_vars

  subroutine sptez_s(wave,grid,idir)
    use kinds, only: r_kind
    implicit none

! Declare passed variables
    integer,intent(in):: idir
    real(r_kind),dimension(nc),intent(inout):: wave
    real(r_kind),dimension(ijmax),intent(inout):: grid

! Declare local variables
    integer i

! Zero appropriate output array based on direction of transform
    if (idir<0) then
      do i=1,nc
        wave(i)=0.
      end do
    elseif (idir>0) then
      do i=1,ijmax
        grid(i)=0.
      end do
    endif

! Call spectral <--> grid transform
    call sptranf_s(wave,grid,grid,idir)

    return
  end subroutine sptez_s

  subroutine destroy_spec_vars
    deallocate(factsml,factvml)
    deallocate(eps,epstop,enn1,elonn1,eon,eontop,afft,&
       clat,slat,wlat,pln,plntop)
    return
  end subroutine destroy_spec_vars


  subroutine sptranf_s(wave,gridn,grids,idir)
    use kinds, only: r_kind
    implicit none

! Declare passed variables
    integer,intent(in):: idir
    real(r_kind),dimension(nc),intent(inout):: wave
    real(r_kind),dimension(ijmax),intent(inout):: gridn
    real(r_kind),dimension(ijmax),intent(inout):: grids

! Declare local variables
    integer i,j,jj,ij,ijn,ijs,mp
    real(r_kind),dimension(2*(jcap+1)):: wtop
    real(r_kind),dimension(imax,2):: g

! Initialize local variables
    mp=0
    
    do i=1,2*(jcap+1)
      wtop(i)=0.
    end do

! Transform wave to grid
    if(idir.gt.0) then
      do j=jb,je
        call sptranf1(iromb,jcap,idrt,imax,jmax,j,j, &
             eps,epstop,enn1,elonn1,eon,eontop, &
             afft,clat(j),slat(j),wlat(j), &
             pln(1,j),plntop(1,j),mp, &
             wave,wtop,g,idir)
        do i=1,imax
          jj  = j-jb
          ijn = i + jj*jn
          ijs = i + jj*js + ioffset
          gridn(ijn)=g(i,1)
          grids(ijs)=g(i,2)
        enddo
      enddo
! Transform grid to wave
    else
      do j=jb,je
        if(wlat(j).gt.0.) then
          do i=1,imax
            jj  = j-jb
            ijn = i + jj*jn
            ijs = i + jj*js + ioffset
            g(i,1)=gridn(ijn)
            g(i,2)=grids(ijs)
          enddo
          call sptranf1(iromb,jcap,idrt,imax,jmax,j,j, &
                eps,epstop,enn1,elonn1,eon,eontop, &
                afft,clat(j),slat(j),wlat(j), &
                pln(1,j),plntop(1,j),mp, &
                wave,wtop,g,idir)
        endif
      enddo
    endif
    return
  end subroutine sptranf_s

  subroutine fill_ns(grid_in,grid_out)
    use variables, only: nlat,nlon,zero,ltosi,ltosj,iglobal
    use kinds, only: i_kind,r_kind
    implicit none

    real(r_kind),dimension(nlon,nlat-2),intent(in):: grid_in  ! input grid
    real(r_kind),dimension(iglobal),intent(out):: grid_out  ! output grid

!  Declare local variables
    integer(i_kind) i,j,k,jj,nlatm2
    real(r_kind) rnlon,sumn,sums
    real(r_kind),dimension(nlat,nlon):: gridtmp

!  Transfer contents of input grid to local work array
!  Reverse ordering in j direction from n-->s to s-->n
    do j=2,nlat-1
      jj=nlat-j
      do i=1,nlon
        gridtmp(j,i)=grid_in(i,jj)
      end do
    end do

!  Compute mean along southern and northern latitudes
    sumn=zero
    sums=zero
    nlatm2=nlat-2
    do i=1,nlon
      sumn=sumn+grid_in(i,1)
      sums=sums+grid_in(i,nlatm2)
    end do
    rnlon=1./float(nlon)
    sumn=sumn*rnlon
    sums=sums*rnlon

!  Load means into local work array
    do i=1,nlon
      gridtmp(1,i)   =sums
      gridtmp(nlat,i)=sumn
    end do

!  Transfer local work array to output grid
   do k=1,iglobal
      i=ltosi(k)
      j=ltosj(k)
      grid_out(k)=gridtmp(i,j)
   end do


    return
  end subroutine fill_ns

  subroutine load_grid(grid_in,grid_out)
    use variables, only: nlat,nlon
    use kinds, only: r_kind,i_kind
    implicit none

    real(r_kind),dimension(nlat,nlon),intent(in):: grid_in        ! input grid
    real(r_kind),dimension(nlon,nlat-2),intent(out):: grid_out    ! output grid

    integer(i_kind) i,j,k,nlatm1,jj,j2

!  Transfer contents of local array to output array.
    nlatm1=nlat-1
    do j=2,nlatm1
      jj=nlat-j+1
      j2=j-1
      do i=1,nlon
        grid_out(i,j2)=grid_in(jj,i)
      end do
    end do

    return
 end subroutine load_grid

  subroutine unload_grid(grid_in,grid_out)
    use variables, only: nlat,nlon
    use kinds, only: r_kind,i_kind
    implicit none

    real(r_kind),dimension(nlon,nlat-2),intent(in):: grid_in        ! input grid
    real(r_kind),dimension(nlat,nlon),intent(out):: grid_out    ! output grid

    integer(i_kind) i,j,k,nlatm1,jj,j2

!  Transfer contents of local array to output array.
    do j=2,nlat-1
      jj=nlat-j
      do i=1,nlon
        grid_out(j,i)=grid_in(i,jj)
      end do
    end do

! for now attempt something stupid at pole
    do j=1,nlon
      grid_out(1,j)=grid_out(2,j)
      grid_out(nlat,j)=grid_out(nlat-1,j)
    end do

    return
 end subroutine unload_grid


 subroutine jcaptrans(z,fact,z4)
   use kinds, only: r_kind,r_single
   implicit none
   integer j,iiin,iiout,l,m
   real(r_kind),dimension(nc):: z,fact
   real(r_single),dimension(ncin):: z4
   do j=1,nc
     z(j)=0.0
   end do
   iiin=1
   iiout=1
   do l=0,min(jcap,jcapin)
      do m=0,min(jcap,jcapin)-l
        if(m < jcapsmooth .and. l < jcapsmooth)then
         z(iiout+2*m)  =fact(iiout+2*m)*z4(iiin+2*m)
         z(iiout+2*m+1)=fact(iiout+2*m+1)*z4(iiin+2*m+1)
        else
         z(iiout+2*m)  =0.0
         z(iiout+2*m+1)=0.0
        end if
      end do
      iiin=iiin+2*(jcapin-l+1)
      iiout=iiout+2*(jcap-l+1)
   end do
   return
 end subroutine jcaptrans


end module specgrid

