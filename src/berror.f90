module berror
!$$$   module documentation block
!                .      .    .                                       .
! module:    berror    contains information pertaining to the background error
!   prgmmr: kleist           org: np20                date: 2004-01-01
!
! abstract: contains information pertaining to the background error
!
! program history log:
!   2004-01-01  kleist
!   2004-11-03  treadon - add horizontal scale weighting factors
!   2004-11-16  treadon - add longitude dimension to variance (dssv) array
!   2004-11-22  derber  - modify horizontal table definition
!   2005-01-22  parrish - clean up code by using module balmod
!   2005-02-23  wu      - add subroutine set_nrh_var
!   2005-05-24  pondeca - accommodate 2dvar only surface analysis option
!                         in set_predictors_var and create_berror_vars_reg
!   2005-06-06  wu - initialize logical fstat
!   2005-10-06  wu - set ozmz values in regional mode
!   2005-11-16  wgu - set nmix=nr+1+(ny-nlat)/2 to make sure
!               nmix+nrmxb=nr in routine smoothrf no matter what
!               number nlat is
!   2005-11-16  wgu - set ny to be odd number if nlat is odd number
!               to support GMAO grid 
!   2005-11-29  derber -  remove set_ozone_var (included in prewgt_reg and prewgt)
!   2006-01-09  derber - remove set_nrh_var and move capability to compute_derived
!   2006-04-21  kleist - add capability to perturb background error parameters 
!   2006-11-30  todling - add fpsproj to control full nsig projection onto ps
!   2007-03-13  derber - add qvar3d array to allow qoption=2 to work similar to others
!   2007-07-03  kleist - add variables for flow-dependent background error variances
!
! subroutines included:
!   sub init_berror         - initialize background error related variables
!   sub create_berror_vars  - allocate global background error related variables
!   sub destroy_berror_vars - deallocate global background error 
!                                    related variables
!   sub set_predictor_var   - set background variances for bias correction coefs
!   sub init_rftable        - load global/global pointers and tables for 
!                                    recursive filters
!   sub initable            - initialize tables/pointers for recursive filters
!   sub create_berror_vars_reg - allocate regional background error 
!                                    related variables
!   sub destroy_berror_vars_reg - deallocate regional background error 
!                                    related variables
!
! Variable Definitions:
!   def norh      - order of interpolations in smoother
!   def ndeg      - degree of smoothing
!   def nta       - first dimension of table (granularity of smoothing coef)
!   def nx        - number longitudes for rf patches
!   def ny        - number latitudes for rf patches
!   def mr        - subdomain dimension for patches
!   def nr        - subdomain dimension for patches
!   def nf        - subdomain dimension for patches
!   def nlath     - half the number of latitudes
!   def ii        - array that point to the x location in table for smoothing
!   def jj        - array that point to the y location in table for smoothing
!   def ii1       - array that point to the x location in table for smoothing
!                 - for northern pole patch
!   def jj1       - array that point to the y location in table for smoothing
!                 - for northern pole patch
!   def ii2       - array that point to the x location in table for smoothing
!                 - for southern pole patch
!   def jj2       - array that point to the y location in table for smoothing
!                 - for southern pole patch
!   def aw        - array that point to the x location in table for smoothing
!   def bw        - factor in background error calculation
!   def as        - normalized scale factor for background error (see namelist setup)
!   def vs        - scale factor for background error vertical scales
!   def be        - smoother coefficients
!   def bl        - blending coefficients for lat/lon boundaries
!   def bl2       - blending coefficients lat/lon boundaries
!   def varprd    - variance for predictors
!   def tsfc_sdv  - standard deviation for land (1) and ice (2) surface temperature
!   def wmask     - not used, function of land-sea mask
!   def table     - table of coeffients for smoothing
!   def slw       - horizontal scale info for 1st patch
!   def slw1      - horizontal scale info for 2nd patch
!   def slw2      - horizontal scale info for 3rd patch
!   def wtaxs     - weights for polar-cascade interpolation
!   def wtxrs     - weights for the step, hybrid to polar grid
!   def inaxs     - index for polar-cascade interpolation
!   def inxrs     - index for polar-cascade interpolation
!   def dssv      - vertical smoother coefficients including variances
!   def qvar3d    - 3d q variance for qoption =2
!   def dssvp     - variances (lat,lon) for surface pressure
!   def dssvt     - variances (lat,lon) for surface temperature
!   def alv       - vertical smoother coefficients
!   def hzscl     - scale factor for background error horizontal scales
!   def hswgt     - empirical weights to apply to each horizontal scale
!   def pert_berr - logical for turning on background error parameter perturbations
!   def pert_berr_fct - scaling factor for randum numbers for berror perturbations 
!   def bkgv_flowdep  - logical to turn on flow-dependence to background error variances
!   def bkgv_rewgtfct - scaling factor to reweight flow-dependent background error variances
!   def fpsproj   - controls full nsig projection onto surface pressure
!   def bkgv_write- logical to turn on/off generation of binary file with reweighted variances
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_berror
  public :: create_berror_vars
  public :: destroy_berror_vars
  public :: set_predictors_var
  public :: init_rftable
  public :: initable
  public :: create_berror_vars_reg
  public :: destroy_berror_vars_reg
! set passed variables to public
  public :: qvar3d,nr,nf,varprd,fpsproj,bkgv_flowdep,tsfc_sdv
  public :: dssvt,dssvp,dssv,bkgv_write,bkgv_rewgtfct,hswgt
  public :: hzscl,bw,pert_berr_fct,pert_berr,ndeg,norh,as,vs
  public :: bl,bl2,be,slw2,slw1,slw,mr,inaxs,wtxrs,wtaxs,nx,ny
  public :: inxrs,jj1,ii2,jj2,ii,jj,ii1,table,alv

  integer(i_kind) norh,ndeg,nta,nlath
  integer(i_kind) nx,ny,mr,nr,nf
  integer(i_kind),allocatable,dimension(:,:):: inaxs,inxrs
  integer(i_kind),allocatable,dimension(:,:,:,:):: ii,jj,ii1,jj1,ii2,jj2

  real(r_kind) bw,vs
  real(r_kind),dimension(10):: as
  real(r_kind),dimension(3):: hzscl,hswgt
  real(r_kind),dimension(2):: tsfc_sdv

  real(r_kind),allocatable,dimension(:):: be,bl,bl2,varprd
  real(r_kind),allocatable,dimension(:,:):: table,&
       slw,slw1,slw2
  real(r_kind),allocatable,dimension(:,:):: dssvp
  real(r_kind),allocatable,dimension(:,:,:):: wtaxs,wtxrs,qvar3d,dssvt
  real(r_kind),allocatable,dimension(:,:,:,:):: alv,dssv

  logical pert_berr,bkgv_flowdep,bkgv_write
  real(r_kind) pert_berr_fct,bkgv_rewgtfct

  logical,save :: fpsproj

contains

  subroutine init_berror
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_berror    initializes constants for the background error
!   prgmmr: kleist           org: np20                date: 2004-01-01
!
! abstract: intializes constants for the background error
!
! program history log:
!   2004-01-01  kleist
!   2004-11-03  treadon - add default definition for horizontal scale weighting factors
!   2005-06-06  wu - add logical fstat
!   2006-11-30  todling - add logical fpsproj
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use constants, only:  zero,one,three
    use balmod, only: fstat
    implicit none
    integer(i_kind) i

    fstat = .false.
    pert_berr = .false.
    bkgv_flowdep = .false.
    bkgv_write = .false.
    pert_berr_fct = zero
    bkgv_rewgtfct = zero
    norh=2_i_kind
    ndeg=4_i_kind
    nta=50000_i_kind
    nlath=48_i_kind

    bw=zero

    fpsproj = .true.

    do i=1,10
       as(i)=0.60_r_kind
    end do

    do i=1,3
      hzscl(i)=one
      hswgt(i)=one/three
    end do
    vs=one/1.5_r_kind

    do i=1,2
       tsfc_sdv(i)=one
    end do

  return
  end subroutine init_berror


  subroutine create_berror_vars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_berror_vars    create arrays for global background error
!   prgmmr: kleist           org: np20                date: 2004-01-01
!
! abstract: creates arrays for global background error
!
! program history log:
!   2004-01-01  kleist
!   2004-07-28  treadon - remove subroutine argument list to --> use modules
!   2004-11-16  treadon - add longitude dimension to array dssv
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use balmod, only: llmin,llmax
  use gridmod, only: nlat,nlon,lat2,lon2,nsig,nnnn1o
  use jfunc, only: nrclen
  use constants, only: izero,ione,zero
  implicit none
  
  llmin=ione
  llmax=nlat

! Grid constant to transform to 3 pieces

  nx=nlon*3/2  
  nx=nx/2*2
  ny=nlat*8/9
  ny=ny/2*2
  if(mod(nlat,2)/=izero)ny=ny+ione
  mr=izero
  nr=nlat/4
  nf=nr
  nlath=nlat/2
  if(mod(nlat,2)/=izero) nlath=nlath+ione

  allocate(wtaxs(0:norh*2-ione,nf,0:(nlon/8)-ione), &
           wtxrs(0:norh*2-ione,0:(nlon/8)-ione,mr:nr), &
           be(ndeg), &
           bl(nx-nlon), &
           bl2(nr+ione+(ny-nlat)/2), &
           alv(lat2,ndeg,nsig,6), &
           dssv(6,lat2,lon2,nsig),&
           qvar3d(lat2,lon2,nsig),&
           dssvp(lat2,lon2),&
           dssvt(lat2,lon2,3))
  dssvt = zero
  allocate(varprd(nrclen))
  allocate(inaxs(nf,nlon/8), &
           inxrs(nlon/8,mr:nr) )

  allocate(slw(ny*nx,nnnn1o),&
           slw1((2*nf+ione)*(2*nf+ione),nnnn1o),&
           slw2((2*nf+ione)*(2*nf+ione),nnnn1o))
  allocate(ii(ny,nx,3,nnnn1o),jj(ny,nx,3,nnnn1o),&
           ii1(2*nf+ione,2*nf+ione,3,nnnn1o),jj1(2*nf+ione,2*nf+ione,3,nnnn1o),&
           ii2(2*nf+ione,2*nf+ione,3,nnnn1o),jj2(2*nf+ione,2*nf+ione,3,nnnn1o))

  return
 end subroutine create_berror_vars


  subroutine destroy_berror_vars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_berror_vars  deallocates global background error arrays
!   prgmmr: kleist           org: np20                date: 2004-01-01
!
! abstract: deallocates global background error arrays
!
! program history log:
!   2004-01-01  kleist
!   2005-03-03  treadon - add implicit none
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    implicit none
    deallocate(wtaxs,wtxrs,be,table,bl,bl2,alv,&
               dssv,qvar3d,dssvp,dssvt,inaxs,inxrs,&
               varprd)
    deallocate(slw,slw1,slw2)
    deallocate(ii,jj,ii1,jj1,ii2,jj2)
    return
  end subroutine destroy_berror_vars


  subroutine set_predictors_var
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    set_predictors_var sets variances for bias correction predictors
!   prgmmr: kleist           org: np20                date: 2004-01-01
!
! abstract: sets variances for bias correction predictors
!
! program history log:
!   2004-01-01  kleist
!   2004-07-28  treadon - remove subroutine calling list, pass through module
!   2005-05-24  pondeca - take into consideration that nrclen=0 for 2dvar only
!                         surface analysis option
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use constants, only:  ione,one,one_tenth
    use jfunc, only: nrclen
    implicit none

    integer(i_kind) i
    real(r_kind) stndev
    
    stndev = one/one_tenth       ! 0.316 K background error
    do i=1,max(ione,nrclen)
       varprd(i)=stndev
    end do
    return
  end subroutine set_predictors_var


  subroutine init_rftable(mype,rate,nnn,sli,sli1,sli2)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_rftable    initializes constants for the background error
!   prgmmr: kleist           org: np20                date: 2004-01-01
!
! abstract: intializes constants for the global background error
!
! program history log:
!   2004-01-01  kleist
!   2004-11-22  derber - modify to do both global and regional, make table
!                        reproducible with different number of processors and
!                        save only those elements of table which are used
!   2005-06-10  devenyi/treadon - remove mype from call rfdparv
!   2008-06-05  safford - rm unused uses
!   2009-03-09  derber  - modify to make arrays smaller
!
!   input argument list:
!     sli      - horizontal scale info for 1st patch
!     sli1     - horizontal scale info for 2nd patch (optional)
!     sli2     - horizontal scale info for 3rd patch (optional)
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use gridmod, only:  regional
    use constants, only: izero,ione,zero,one
    implicit none

    real(r_kind),parameter:: tin = 0.2e-3_r_kind

    integer(i_kind) i,j,k,n,nynx,nfnf
    integer(i_kind) nnn,mype,ihwlb
    integer(i_kind) nfg,ntax,iloc
    
    real(r_kind),optional,dimension((2*nf+ione)*(2*nf+ione),2,nnn):: sli1,sli2
    real(r_kind),dimension(ny*nx,2,nnn):: sli
    real(r_kind),dimension(ndeg):: rate
    real(r_kind):: hwlmax,hwlmin,hwlb,hwle,wni2
    real(r_kind),parameter:: r999         = 999.0_r_kind
    real(r_kind),allocatable,dimension(:):: dsh
    logical,allocatable,dimension(:):: iuse
    integer(i_kind),allocatable,dimension(:):: ipoint


    nynx=ny*nx
    if(.not. regional)then
      if(.not. present(sli1) .or. .not. present(sli2))then
         write(6,*)'INIT_RFTABLE:  ***ERROR*** sli1 or sli2 not present'
         call stop2(34)
      end if
      nfg=nf*2+ione
      nfnf=nfg*nfg
    end if

! Determine lower/upper bounds on scales
    hwlmax=-r999
    hwlmin=r999
    do k=1,nnn

        do j=1,nynx
           hwlmax=max(hwlmax,sli(j,1,k),sli(j,2,k))
           hwlmin=min(hwlmin,sli(j,1,k),sli(j,2,k))
!          hwlmax=max(hwlmax,sli(j,1,k))
!          hwlmin=min(hwlmin,sli(j,1,k))
!          hwlmax=max(hwlmax,sli(j,2,k))
!          hwlmin=min(hwlmin,sli(j,2,k))
        end do
        if(.not. regional)then
          do j=1,nfnf
           hwlmax=max(hwlmax,sli1(j,1,k),sli2(j,1,k),sli1(j,2,k),sli2(j,2,k))
           hwlmin=min(hwlmin,sli1(j,1,k),sli2(j,1,k),sli1(j,2,k),sli2(j,2,k))
!          hwlmax=max(hwlmax,sli1(j,1,k))
!          hwlmin=min(hwlmin,sli1(j,1,k))
!          hwlmax=max(hwlmax,sli2(j,1,k))
!          hwlmin=min(hwlmin,sli2(j,1,k))
!          hwlmax=max(hwlmax,sli1(j,2,k))
!          hwlmin=min(hwlmin,sli1(j,2,k))
!          hwlmax=max(hwlmax,sli2(j,2,k))
!          hwlmin=min(hwlmin,sli2(j,2,k))
          end do
        end if
    enddo

! factor from multi-Gaussian RF
!   write(6,*)'INIT_RFTABLE:  hwlmax...=',hwlmax,hwlmin,&
!        hzscl(1),hzscl(2),hzscl(3),mype,nynx,nnn
    hwlmax=hwlmax*max(hzscl(1),hzscl(2),hzscl(3))
    hwlmin=hwlmin*min(hzscl(1),hzscl(2),hzscl(3))

! setup smoother coef and scale
    if (hwlmax==zero .or. hwlmin==zero) then
       write(6,*)'INIT_RFTABLE:  ***ERROR*** illegal value for min,max scale.',&
            '  hwlmin,hwlmax=',hwlmin,hwlmax,mype
       call stop2(41)
    endif
    hwlb=one/hwlmax
    hwle=one/hwlmin

    ihwlb=hwlb/tin
    hwlb=ihwlb*tin
!   tin=(hwle-hwlb)/float(nta-ione)
    ntax=(hwle-hwlb)/tin+2_i_kind
!   write(6,*)'INIT_RFTABLE:  tin ',ntax,ihwlb,tin,hwlb,hwle

    allocate(iuse(ntax))

    iuse=.false.
    wni2=one/tin
    do k=1,nnn
        do n=1,2
           do i=1,nynx
             do j=1,3
               iloc=min(ntax,nint(one-ihwlb+wni2/(hzscl(j)*sli(i,n,k))))
               iloc=max(iloc,ione)
               iuse(iloc)=.true.
             enddo
           enddo

           if(.not. regional)then
             do i=1,nfnf
               do j=1,3
               iloc=min(ntax,nint(one-ihwlb+wni2/(hzscl(j)*sli1(i,n,k))))
               iloc=max(iloc,ione)
               iuse(iloc)=.true.
               iloc=min(ntax,nint(one-ihwlb+wni2/(hzscl(j)*sli2(i,n,k))))
               iloc=max(iloc,ione)
               iuse(iloc)=.true.
               enddo
             enddo
           end if

        enddo
    enddo
    nta=izero
    allocate(dsh(ntax),ipoint(ntax))
    ipoint=izero
    do i=1,ntax
      if(iuse(i))then
        nta=nta+ione
        ipoint(i)=nta
        dsh(nta)=one/(float(i-ione+ihwlb)*tin)
      end if
    end do
!   write(6,*)'INIT_RFTABLE:  ntax,nta = ',ntax,nta

!   Loop over number of sigma levels per task    
    do k=1,nnn

!      Load pointers into table array
       do j=1,3

         call initable(ny,nx,sli(1,1,k),ntax,ihwlb,&
            ii(1,1,j,k),jj(1,1,j,k),hzscl(j),tin,ipoint)

         if(.not. regional)then
           call initable(nfg,nfg,sli1(1,1,k),ntax,ihwlb,&
            ii1(1,1,j,k),jj1(1,1,j,k),hzscl(j),tin,ipoint)

           call initable(nfg,nfg,sli2(1,1,k),ntax,ihwlb,&
            ii2(1,1,j,k),jj2(1,1,j,k),hzscl(j),tin,ipoint)
         end if

       end do

!   End of loop over number of sigma levels per mpi task       
    end do

    deallocate(iuse,ipoint)

    allocate(table(nta,ndeg)) 

    call rfdparv(dsh,rate,table,nta,ndeg)

    deallocate(dsh)

    return
  end subroutine init_rftable

  
  subroutine initable(nxdim,nydim,sli,ntax,ihwlb,iix,jjx,factor,tin,ipoint)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    initable    initializes pointers to table for background error
!   prgmmr: kleist           org: np20                date: 2004-01-01
!
! abstract: initializes pointers to table for global and regional background error
!
! program history log:
!   2004-01-01  kleist
!   2004-11-22  derber  - modify to be consistent with init_rftable
!   2008-06-05  safford - rm unused var
!
!   input argument list:
!     nxdim    - 1st dimension of arrays
!     nydim    - 2nd dimension of arrays
!     sli      - horizontal scale info
!     ntax     - number of possible entries in table array
!     ihwlb    - minimum of possible coefficient values
!     factor   - horizontal scale factor
!     tin      - table interval
!     ipoint   - pointer at table locations
!
!   output argument list:
!     iix      - first index pointer array to table
!     jjx      - second index pointer array to table
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use constants, only: ione,one
    implicit none

    integer(i_kind) iy,nydim,ix,nxdim,iloc,ntax
    integer(i_kind),dimension(nxdim,nydim):: iix,jjx
    integer(i_kind),dimension(ntax):: ipoint
    integer(i_kind) ihwlb

    real(r_kind) wni2,factor,tin
    real(r_kind),dimension(nxdim,nydim,2):: sli

!   Load pointers for table array
    wni2=one/tin
    do iy=1,nydim
       do ix=1,nxdim
         iloc=min(ntax, max(ione, nint(one-ihwlb+wni2/(sli(ix,iy,1)*factor))))
         iix(ix,iy)=ipoint(iloc)
         iloc=min(ntax, max(ione, nint(one-ihwlb+wni2/(sli(ix,iy,2)*factor))))
         jjx(ix,iy)=ipoint(iloc)
       enddo
    enddo

    return
  end subroutine initable


  subroutine create_berror_vars_reg
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_berror_vars_reg  create arrays for reg background error
!   prgmmr: kleist           org: np20                date: 2004-01-01
!
! abstract: creates arrays for regional background error
!
! program history log:
!   2004-01-01  kleist
!   2004-07-28  treadon - simplify subroutine calling list
!   2004-11-16  treadon - add longitude dimension to array dssv
!   2005-05-24  pondeca - take into consideration that npred=npredp=0
!                         for 2dvar only surface analysis option
!   2005-06-23  middlecoff/treadon - iniitalize mr,nr,nf
!   2009-01-04  todling - remove mype
!
!   input argument list:
!
!   output argument list:
!     mype     - mpi task id
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use constants, only: ione
    use balmod, only: llmin,llmax
    use gridmod, only: nlat,nlon,nsig,nnnn1o,lat2,lon2
    use jfunc, only: nrclen
    implicit none
    
    nx=nlon
    ny=nlat
    mr=ione
    nr=ione
    nf=nr
    
!   Grid constant for background error

    allocate(be(ndeg), &
         alv(llmin:llmax,ndeg,nsig,6), &
         dssv(6,llmin:llmax,lon2,nsig), &
         qvar3d(lat2,lon2,nsig), &
         dssvp(llmin:llmax,lon2),&
         dssvt(llmin:llmax,lon2,3))
    
    allocate(varprd(max(ione,nrclen) ) )     

    allocate(slw(ny*nx,nnnn1o) )
    allocate(ii(ny,nx,3,nnnn1o),jj(ny,nx,3,nnnn1o) )
    
    
    return
  end subroutine create_berror_vars_reg


  subroutine destroy_berror_vars_reg
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_berror_vars_reg  deallocate reg background error arrays
!   prgmmr: kleist           org: np20                date: 2004-01-01
!
! abstract: deallocates regional background error arrays
!
! program history log:
!   2004-01-01  kleist
!   2005-03-03  treadon - add implicit none
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    implicit none
    deallocate(be,table,alv,&
               dssv,qvar3d,dssvp,&
               dssvt,varprd)
    deallocate(slw)
    deallocate(ii,jj)
    return
  end subroutine destroy_berror_vars_reg

end module berror
