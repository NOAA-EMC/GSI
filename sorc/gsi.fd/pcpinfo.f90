module pcpinfo
!$$$   module documentation block
!                .      .    .                                       .
! module:  pcpinfo
! prgmmr:  treadon           org: np23                date: 2003-09-25
!
! abstract: This moduce contains variables pertinent to
!           assimilation of precipitation rates
!
! program history log:
!   2004-05-13  kleist, documentation
!   2004-06-15  treadon, reformat documentation
!   2004-12-22  treadon - rename logical "idiag_pcp" to "diag_pcp"
!   2005-09-28  derber - modify pcpinfo input and add qc input
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-04-27  derber - remove jppfp
!   2007-01-19  treadon - remove tinym1_obs since no longer used
!
! Subroutines Included:
!   sub init_pcp          - initialize pcp related variables to defaults
!   sub pcpinfo_read      - read in pcp info and biases
!   sub pcpinfo_write     - write out pcp biases
!   sub create_pcp_random - generate random number for precip. assimilation
!   sub destroy_pcp_random- deallocate random number array
!
! Variable Definitions
!   def diag_pcp    - flag to toggle creation of precipitation diagnostic file
!   def npredp      - number of predictors in precipitation bias correction
!   def jtype       - maximum number of precipitation data types
!   def mype_pcp    - task id for writing out pcp diagnostics
!   def deltim      - model timestep
!   def dtphys      - relaxation time scale for convection
!   def tiny_obs    - used to check whether or not to include pcp forcing
!   def varchp      - precipitation rate observation error
!   def gross_pcp   - gross error for precip obs      
!   def b_pcp       - b value for variational QC      
!   def pg_pcp      - pg value for variational QC      
!   def predxp      - precipitation rate bias correction coefficients
!   def xkt2d       - random numbers used in SASCNV cloud top selection
!   def nupcp       - satellite/instrument                
!   def iusep       - use to turn off pcp data
!   def ibias       - pcp bias flag, used for IO
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  implicit none

  logical diag_pcp
  integer(i_kind) npredp,jtype,mype_pcp
  real(r_kind) deltim,dtphys
  real(r_kind) tiny_obs
  real(r_kind),allocatable,dimension(:):: varchp,gross_pcp,b_pcp,pg_pcp
  real(r_kind),allocatable,dimension(:,:):: predxp ,xkt2d
  integer(i_kind),allocatable,dimension(:):: iusep,ibias
  character(len=20),allocatable,dimension(:):: nupcp
  
contains
  
  subroutine init_pcp
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_pcp
!     prgmmr:    treadon     org: np23                date: 2003-09-25
!
! abstract:  set defaults for variables used in precipitation rate 
!            assimilation routines
!
! program history log:
!   2003-09-25  treadon
!   2004-05-13  treadon, documentation
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use constants, only: one
    implicit none
	real(r_kind),parameter:: r1200=1200.0_r_kind
	real(r_kind),parameter:: r3600=3600.0_r_kind

    npredp    = 6      ! number of predictors in precipitation bias correction
    jtype     = 2      ! maximum number of precipitation data types
    deltim    = r1200  ! model timestep
    dtphys    = r3600  ! relaxation time scale for convection
    diag_pcp =.true.   ! flag to toggle creation of precipitation diagnostic file
    mype_pcp  = 0      ! task to print pcp info to.  Note that mype_pcp MUST equal
                       !    mype_rad (see radinfo.f90) in order for statspcp.f90
                       !    to print out the correct information          
	tiny_obs = 1.e-9_r_kind   ! "small" observation
  end subroutine init_pcp

  subroutine pcpinfo_read(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    pcpinfo_read
!     prgmmr:    treadon     org: np23                date: 2003-09-25
!
! abstract:  read text file containing information (satellite id, error, 
!            usage flags) for precipitation rate observations.  This
!            routine also reads (optional) precipitation rate bias 
!            coefficients
!
! program history log:
!   2003-09-25  treadon
!   2004-05-13  treadon, documentation
!   2004-08-04  treadon - add only on use declarations; add intent in/out
!   2005-10-11  treadon - change pcpinfo read to free format
!
!   input argument list:
!      mype - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use kinds, only: r_kind,i_kind
    use constants, only: zero
    use obsmod, only: iout_pcp
    implicit none

! Declare passed variables
    integer(i_kind),intent(in):: mype

! Declare local varianbes
    integer(i_kind) lunit,i,j,jtyp,ii,ip,n
    real(r_kind),dimension(npredp):: predrp
    
    lunit=48
    allocate(nupcp(jtype),iusep(jtype),ibias(jtype), &
         varchp(jtype),gross_pcp(jtype),b_pcp(jtype),pg_pcp(jtype))
    if(mype==mype_pcp)open(iout_pcp)
    

! Read fixed information for observation platforms
    open(lunit,file='pcpinfo',form='formatted')
    do j=1,jtype
       read(lunit,*,err=1007,end=1008) nupcp(j),&
            iusep(j),ibias(j),varchp(j),gross_pcp(j),b_pcp(j),pg_pcp(j)
       if(mype==mype_pcp ) &
            write(iout_pcp,1005) nupcp(j),iusep(j),ibias(j), &
                 varchp(j),gross_pcp(j),b_pcp(j),pg_pcp(j)
    end do
    close(lunit)
1005 format('PCPINFO: ', a20,    &
          ' iusep = ',i2,   ' ibias = ',i2, &
          ' var   = ',f7.3,' gross = ',f7.3,' b_pcp = ',f7.3, ' pg_pcp = ',f7.3)
    if(mype==0) write(6,*)'PCPINFO:  read pcpinfo on mype=',mype
    close(lunit)
    
    allocate(predxp(jtype,npredp))
    do j=1,npredp
       do i=1,jtype
          predxp(i,j)=zero
       end do
    end do
    
    open(lunit,file='pcpbias_in' ,form='formatted')
    if(mype==mype_pcp) then
       write(iout_pcp,*)'PCPINFO:  read pcpbias coefs from lunit=',lunit,&
            ' with npredp=',npredp
    endif
    do jtyp=1,jtype
       read(lunit,'(I5,10f12.6)',end=1021) ii,(predrp(ip),ip=1,npredp)
       do i=1,npredp
          predxp(ii,i)=predrp(i)
       end do
       if(mype==mype_pcp) write(iout_pcp,1011) jtyp,(predxp(jtyp,n),n=1,npredp)
    end do
1011 format(1x,'jtype=',i3,10f12.6)
1021 continue
    close(lunit)
    close(iout_pcp)
    
    return
    
! Error conditions.
1007 continue
    if(mype==0) write(6,*)'PCPINFO:  ***ERROR*** reading pcp info file'
    close(lunit)
    call stop2(77)
    
1008 continue
    if(mype==0) write(6,*)'PCPINFO:  less info in pcp file than jtype=',jtype
    close(lunit)
    call stop2(78)
    
    return
  end subroutine pcpinfo_read

  subroutine pcpinfo_write
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    pcpinfo_write
!     prgmmr:    treadon     org: np23                date: 2003-09-25
!
! abstract:  write precipitation rate bias correction coefficients
!
! program history log:
!   2003-09-25  treadon
!   2004-05-13  treadon, documentation
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none
    integer(i_kind) iobcof,itype,ityp,ip

    iobcof=52
    open(iobcof,file='pcpbias_out',form='formatted')
    rewind iobcof
    do ityp=1,jtype
       write(iobcof,'(I5,10f12.6)') ityp,(predxp(ityp,ip),ip=1,npredp)
    end do
    close(iobcof)
    return
  end subroutine pcpinfo_write

  subroutine create_pcp_random(iadate,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_pcp_random
!     prgmmr:    treadon     org: np23                date: 2003-09-25
!
! abstract:  generate random numbers for cloud selction application
!            in GFS convective parameterization (SASCNV)
!
! program history log:
!   2003-09-25  treadon
!   2004-05-13  treadon, documentation
!   2004-12-03  treadon - replace mpe_iscatterv (IBM extension) with
!                         standard mpi_scatterv
!   2005-12-12  treadon - remove IBM specific call to random_seed(generator)
!   2006-01-10  treadon - move myper inside routine
!
!   input argument list:
!      iadate - analysis date (year, month, day, hour, minute)
!      mype   - mpi task id 
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use gridmod, only: ijn_s,ltosj_s,ltosi_s,displs_s,itotsub,&
       lat2,lon2,nlat,nlon
    use mpimod, only: mpi_comm_world,ierror,mpi_rtype,npe
    implicit none

! Declare passed variables
	integer(i_kind),intent(in):: mype
	integer(i_kind),intent(in),dimension(5):: iadate    

! Declare local variables
    integer(i_kind) krsize,i,j,k,mm1,myper
    integer(i_kind),allocatable,dimension(:):: nrnd
    
    real(r_kind) rseed
    real(r_kind),allocatable,dimension(:):: rwork
    real(r_kind),allocatable,dimension(:,:):: rgrid

! Compute random number for precipitation forward model.  
    mm1=mype+1
    allocate(rwork(itotsub),xkt2d(lat2,lon2))
    myper=npe-1
    if (mype==myper) then
       allocate(rgrid(nlat,nlon))
       call random_seed(size=krsize)
       allocate(nrnd(krsize))
       rseed = 1e6_r_kind*iadate(1) + 1e4_r_kind*iadate(2) &
          + 1e2_r_kind*iadate(3) + iadate(4)
       write(6,*)'CREATE_PCP_RANDOM:  rseed,krsize=',rseed,krsize
       do i=1,krsize
          nrnd(i) = rseed
       end do
       call random_seed(put=nrnd)
       deallocate(nrnd)
       call random_number(rgrid)
       do k=1,itotsub
          i=ltosi_s(k); j=ltosj_s(k)
          rwork(k)=rgrid(i,j)
       end do
       deallocate(rgrid)
    endif
    call mpi_scatterv(rwork,ijn_s,displs_s,mpi_rtype,xkt2d,ijn_s(mm1),&
         mpi_rtype,myper,mpi_comm_world,ierror)
    deallocate(rwork)
    return
  end subroutine create_pcp_random


  subroutine destroy_pcp_random
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_pcp_random
!     prgmmr:    treadon     org: np23                date: 2003-09-25
!
! abstract:  deallocate array to contain random numbers for SASCNV
!
! program history log:
!   2003-09-25  treadon
!   2004-05-13  treadon, documentation
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
     deallocate(xkt2d)
     return
  end subroutine destroy_pcp_random
  
end module pcpinfo
