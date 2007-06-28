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
!   def dssvl     - variances for single level variables
!   def dssv2     - variances (lat,lon) for sea surface temperature
!   def alv       - vertical smoother coefficients
!   def hzscl     - scale factor for background error horizontal scales
!   def hswgt     - empirical weights to apply to each horizontal scale
!   def pert_berr - logical for turning on background error parameter perturbations
!   def pert_berr_fct - scaling factor for randum numbers for berror perturbations 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  implicit none

  integer(i_kind) norh,ndeg,nta,nlath
  integer(i_kind) nx,ny,mr,nr,nf
  integer(i_kind),allocatable,dimension(:,:):: inaxs,inxrs
  integer(i_kind),allocatable,dimension(:,:,:,:):: ii,jj,ii1,jj1,ii2,jj2

  real(r_kind) bw,vs
  real(r_kind),dimension(10):: as
  real(r_kind),dimension(3):: hzscl,hswgt

  real(r_kind),allocatable,dimension(:):: be,bl,bl2,varprd
  real(r_kind),allocatable,dimension(:,:):: table,&
       slw,slw1,slw2
  real(r_kind),allocatable,dimension(:,:):: dssvl,dssv2
  real(r_kind),allocatable,dimension(:,:,:):: wtaxs,wtxrs
  real(r_kind),allocatable,dimension(:,:,:,:):: alv,dssv

  logical pert_berr
  real(r_kind) pert_berr_fct


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
    use kinds, only: i_kind
    use constants, only:  zero,one,three
    use balmod, only: fstat
    implicit none
    integer(i_kind) i

    fstat = .false.
    pert_berr = .false.
    pert_berr_fct = zero
    norh=2
    ndeg=4
    nta=50000
    nlath=48

    bw=zero

    do i=1,10
       as(i)=0.60_r_kind
    end do

    do i=1,3
      hzscl(i)=one
      hswgt(i)=one/three
    end do
    vs=one/1.5_r_kind

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
  use gridmod, only: nlat,nlon,lat2,lon2,nsig,nsig1o
  use radinfo, only: jpch,npred
  use pcpinfo, only: jtype,npredp
  implicit none
  
  llmin=1
  llmax=nlat

! Grid constant to transform to 3 pieces

  nx=nlon*3/2  
  nx=nx/2*2
  ny=nlat*8/9
  ny=ny/2*2
  if(mod(nlat,2)/=0)ny=ny+1
  mr=0
  nr=nlat/4
  nf=nr
  nlath=nlat/2
  if(mod(nlat,2)/=0) nlath=nlath+1

  allocate(wtaxs(0:norh*2-1,nf,0:(nlon/8)-1), &
           wtxrs(0:norh*2-1,0:(nlon/8)-1,mr:nr), &
           be(ndeg), &
           bl(nx-nlon), &
           bl2(nr+1+(ny-nlat)/2), &
           alv(lat2,ndeg,nsig,6), &
           dssv(6,lat2,lon2,nsig),&
           dssvl(lat2,3),&
           dssv2(lat2,lon2))
  allocate(varprd(jpch*npred+jtype*npredp))
  allocate(inaxs(nf,nlon/8), &
           inxrs(nlon/8,mr:nr) )

  allocate(slw(ny*nx,nsig1o),&
           slw1((2*nf+1)*(2*nf+1),nsig1o),&
           slw2((2*nf+1)*(2*nf+1),nsig1o))
  allocate(ii(ny,nx,3,nsig1o),jj(ny,nx,3,nsig1o),&
           ii1(2*nf+1,2*nf+1,3,nsig1o),jj1(2*nf+1,2*nf+1,3,nsig1o),&
           ii2(2*nf+1,2*nf+1,3,nsig1o),jj2(2*nf+1,2*nf+1,3,nsig1o))

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
               dssv,dssvl,dssv2,inaxs,inxrs,&
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
    use kinds, only: i_kind,r_kind
    use constants, only:  one,one_tenth
    use radinfo, only: npred,jpch
    use pcpinfo, only: npredp,jtype
    use jfunc, only: nrclen
    implicit none

    integer(i_kind) i
    real(r_kind) stndev
    
    stndev = one/one_tenth       ! 0.316 K background error
    do i=1,max(1,nrclen)
       varprd(i)=stndev
    end do
    return
  end subroutine set_predictors_var

  subroutine init_rftable(mype,rate,sli,sli1,sli2)
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
    use kinds, only: i_kind,r_kind
    use gridmod, only:  nsig,nsig1o,regional
    use mpimod, only:  levs_id,npe
    use constants, only: zero,one,two,four
    implicit none

    real(r_kind),parameter:: tin = 0.2e-3_r_kind

    integer(i_kind) i,j,k,n,nynx,nfnf
    integer(i_kind) nnn,mype,ihwlb
    integer(i_kind) nfg,ntax,iloc
    
    real(r_kind),optional,dimension((2*nf+1)*(2*nf+1),3,nsig1o):: sli1,sli2
    real(r_kind),dimension(ny*nx,3,nsig1o):: sli
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
      nfg=nf*2+1
      nfnf=nfg*nfg
    end if

    nnn=0
    do k=1,nsig1o
      if (levs_id(k)/=0) nnn=nnn+1
    end do

! Determine lower/upper bounds on scales
    hwlmax=-r999
    hwlmin=r999
    do k=1,nnn

!       Load slw arrays
        do j=1,nynx
           slw(j,k)=sli(j,1,k)
        end do
        do j=1,nynx
           hwlmax=max(hwlmax,sli(j,2,k))
           hwlmin=min(hwlmin,sli(j,2,k))
        end do
        do j=1,nynx
           hwlmax=max(hwlmax,sli(j,3,k))
           hwlmin=min(hwlmin,sli(j,3,k))
        end do
        if(.not. regional)then
          do j=1,nfnf
           slw1(j,k) = sli1(j,1,k)
           slw2(j,k) = sli2(j,1,k)
          end do
          do j=1,nfnf
           hwlmax=max(hwlmax,sli1(j,2,k))
           hwlmin=min(hwlmin,sli1(j,2,k))
           hwlmax=max(hwlmax,sli2(j,2,k))
           hwlmin=min(hwlmin,sli2(j,2,k))
          end do
          do j=1,nfnf
           hwlmax=max(hwlmax,sli1(j,3,k))
           hwlmin=min(hwlmin,sli1(j,3,k))
           hwlmax=max(hwlmax,sli2(j,3,k))
           hwlmin=min(hwlmin,sli2(j,3,k))
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
!   tin=(hwle-hwlb)/float(nta-1)
    ntax=(hwle-hwlb)/tin+2
!   write(6,*)'INIT_RFTABLE:  tin ',ntax,ihwlb,tin,hwlb,hwle

    allocate(iuse(ntax))

    iuse=.false.
    wni2=one/tin
    do k=1,nnn
        do n=2,3
           do i=1,nynx
             do j=1,3
               iloc=min(ntax,nint(one+wni2*(one/(hzscl(j)*sli(i,n,k))-hwlb)))
               iuse(iloc)=.true.
             enddo
           enddo

           if(.not. regional)then
             do i=1,nfg*nfg
               do j=1,3
               iloc=min(ntax,nint(one+wni2*(one/(hzscl(j)*sli1(i,n,k))-hwlb)))
               iuse(iloc)=.true.
               iloc=min(ntax,nint(one+wni2*(one/(hzscl(j)*sli2(i,n,k))-hwlb)))
               iuse(iloc)=.true.
               enddo
             enddo
           end if

        enddo
    enddo
    nta=0
    allocate(dsh(ntax),ipoint(ntax))
    ipoint=0
    do i=1,ntax
      if(iuse(i))then
        nta=nta+1
        ipoint(i)=nta
        dsh(nta)=one/(float(i-1)*tin+hwlb)
      end if
    end do
!   write(6,*)'INIT_RFTABLE:  ntax,nta = ',ntax,nta

!   Loop over number of sigma levels per task    
    do k=1,nnn

!      Load pointers into table array
       do j=1,3

         call initable(ny,nx,sli(1,1,k),nta,ntax,hwlb,&
            ii(1,1,j,k),jj(1,1,j,k),hzscl(j),tin,ipoint)

         if(.not. regional)then
           call initable(nfg,nfg,sli1(1,1,k),nta,ntax,hwlb,&
            ii1(1,1,j,k),jj1(1,1,j,k),hzscl(j),tin,ipoint)

           call initable(nfg,nfg,sli2(1,1,k),nta,ntax,hwlb,&
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

  
  subroutine initable(nxdim,nydim,sli,nta,ntax,hwlb,iix,jjx,factor,tin,ipoint)
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
!
!   input argument list:
!     nxdim    - 1st dimension of arrays
!     nydim    - 2nd dimension of arrays
!     sli      - horizontal scale info
!     nta      - number of entries in table array
!     ntax     - number of possible entries in table array
!     hwlb     - minimum of possible coefficient values
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
    use kinds, only: r_kind,i_kind
    use constants, only: one
    implicit none

    integer(i_kind) iy,nydim,ix,nxdim,im,nta,iloc,ntax
    integer(i_kind),dimension(nxdim,nydim):: iix,jjx
    integer(i_kind),dimension(ntax):: ipoint

    real(r_kind) wni2,hwlb,factor,tin
    real(r_kind),dimension(nxdim,nydim,3):: sli

!   Load pointers for table array
    wni2=one/tin
    do iy=1,nydim
       do ix=1,nxdim
         iloc=min(ntax, max(1, nint(one+wni2*(one/(sli(ix,iy,2)*factor)-hwlb))))
         iix(ix,iy)=ipoint(iloc)
         iloc=min(ntax, max(1, nint(one+wni2*(one/(sli(ix,iy,3)*factor)-hwlb))))
         jjx(ix,iy)=ipoint(iloc)
       enddo
    enddo

    return
  end subroutine initable

  subroutine create_berror_vars_reg(mype)
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
    use kinds, only: i_kind
    use balmod, only: llmin,llmax
    use gridmod, only: nlat,nlon,nsig,nsig1o,lat2,lon2
    use radinfo, only: jpch,npred
    use pcpinfo, only: jtype,npredp
    implicit none
    
    integer(i_kind),intent(in):: mype
    
    nx=nlon
    ny=nlat
    mr=1
    nr=1
    nf=nr
    
!   Grid constant for background error

    allocate(be(ndeg), &
         alv(llmin:llmax,ndeg,nsig,6), &
         dssv(6,llmin:llmax,lon2,nsig), &
         dssvl(llmin:llmax,3), &
         dssv2(lat2,lon2))
    
    allocate(varprd(max(1,jpch*npred+jtype*npredp) ) )     

    allocate(slw(ny*nx,nsig1o) )
    allocate(ii(ny,nx,3,nsig1o),jj(ny,nx,3,nsig1o) )
    
    
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
               dssv,dssvl,dssv2,&
               varprd)
    deallocate(slw)
    deallocate(ii,jj)
    return
  end subroutine destroy_berror_vars_reg

end module berror
