module anberror
!$$$   module documentation block
!                .      .    .                                       .
! module:    anberror   holds info related to anisotropic background error
!   prgmmr: parrish          org: np23                date: 2005-02-08
!
! abstract: contains information pertaining to the background error
!
! program history log:
!   2005-02-08  parrish
!   2005-05-24  pondeca - accommodate 2dvar only surface analysis option
!                         in create_anberror_vars_reg
!   2005-11-29  derber - remove anset_ozone_var (included in anprewgt_reg)
!   2007-08-21  pondeca - add qvar3d allocate (bug fix)
!
! subroutines included:
!   sub init_anberror             - initialize extra anisotropic background error
!                                       related variables
!   sub create_anberror_vars      - allocate global anisotropic background error
!                                       related variables
!   sub destroy_anberror_vars     - deallocate global anisotropic background error 
!                                       related variables
!   sub create_anberror_vars_reg  - allocate regional anisotropic background error 
!                                       related variables
!   sub destroy_anberror_vars_reg - deallocate regional anisotropic background error 
!                                       related variables
!
! Variable Definitions:
!   def anisotropic - if true, then use anisotropic background error
!   def ids         -
!   def ide         -
!   def ims         -
!   def ime         -
!   def ips         -
!   def ipe         -
!   def jds         -
!   def jde         -
!   def jms         -
!   def jme         -
!   def jps         -
!   def jpe         -
!   def kds         -
!   def kde         -
!   def jms         -
!   def kme         -
!   def kps         -
!   def kpe         -
!   def nvars       - number of analysis variables
!   def idvar       - used by anisotropic filter code 
!   def jdvar       -  to apply filter simultaneously
!   def kvar_start  -  to all variables, when stored
!   def kvar_end    -  in horizontal slab mode.
!   def var_names   -  descriptive names of variables
!   def clenmax     - max allowable correlation length in grid units
!   def clenmaxi    - 1/clenmax
!   def smooth_len  - length scale for horizontal and vertical smoothing
!                     of background, in analysis grid units.  The smoothed
!                     background is used in a variety of ways to define
!                     anisotropic error correlations.
!   def triad4      - for 2d variables, if true, use blended triad algorithm
!   def ifilt_ord   - filter order for anisotropic filters
!   def npass       - 2*npass = number of factors in background error
!   def normal      - number of random vectors to use for filter normalization
!                       ( if < 0 then slightly slower, but results independent of 
!                         number of processors)
!   def binom       - if true, weight correlation lengths of factors using binomial
!                      distribution, with shortest scales on outside, longest scales
!                      on inside.  This can help to produce smoother correlations in the
!                      presence of strong anisotrophy
!   def ngauss      - number of gaussians to add together in each factor
!   def rgauss      - multipliers on reference aspect tensor for each gaussian factor
!   def an_amp      - multiplying factors on reference background error variances
!                      an_amp(k, 1) - streamfunction          (k=1,ngauss)
!                      an_amp(k, 2) - velocity potential
!                      an_amp(k, 3) - log(ps)
!                      an_amp(k, 4) - temperature
!                      an_amp(k, 5) - specific humidity
!                      an_amp(k, 6) - ozone
!                      an_amp(k, 7) - sea surface temperature
!                      an_amp(k, 8) - cloud condensate mixing ratio
!                      an_amp(k, 9) - land surface temperature
!                      an_amp(k,10) - ice surface temperature
!   def an_vs       - scale factor for background error vertical scales (temporary carry over from
!                      isotropic inhomogeneous option)
!   def grid_ratio  - ratio of coarse to fine grid, in fine grid units (coarse grid
!                      used when applying recursive filter)
!   def an_flen_u   - coupling parameter for connecting horizontal wind to background error
!   def an_flen_t   - coupling parameter for connecting potential temperature gradient to background error
!   def an_flen_z   - coupling parameter for connecting terrain gradient to background error
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind,i_long,r_double
  use raflib, only: filter_cons
  use berror, only: qvar3d
  use gridmod, only: lat2,lon2,nsig 
  implicit none

  logical anisotropic
  integer(i_kind) ids,ide,jds,jde,kds,kde ! full domain lat, lon, vert grid indices
  integer(i_kind) ips,ipe,jps,jpe,kps,kpe ! subdomain lat, lon, vert grid indices (full units)
  integer(i_kind) ims,ime,jms,jme,kms,kme ! subdomain + halo lat, lon, vert grid indices (full units)
  integer(i_kind) nvars
  integer(i_kind),allocatable::idvar(:),jdvar(:),kvar_start(:),kvar_end(:)
  character(80),allocatable::var_names(:)
  real(r_kind) clenmax,clenmaxi,smooth_len
  type(filter_cons) filter_all(7)
  logical triad4,binom
  integer(i_long) ifilt_ord,npass,ngauss,normal
  real(r_double) rgauss(20)
  real(r_double) an_amp(20,10)
  real(r_double) an_vs
  real(r_kind) grid_ratio
  real(r_double) an_flen_u,an_flen_t,an_flen_z


contains

  subroutine init_anberror
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_anberror    set constants for anisotropic background error
!   prgmmr: parrish          org: np23                date: 2005-02-08
!
! abstract: intializes extra constants needed for the anisotropic 
!               global mode background error
!
! program history log:
!   2005-02-08  parrish
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

    use kinds, only: i_kind
    use constants, only:  zero,half,one,two,three
    implicit none

    integer(i_kind) k

    anisotropic=.false.
    clenmax=120.0_r_kind
    clenmaxi=one/clenmax
    smooth_len=4._r_kind
    ids=0 ; ide=0 ; ims=0 ; ime=0 ; ips=0 ; ipe=0
    jds=0 ; jde=0 ; jms=0 ; jme=0 ; jps=0 ; jpe=0
    kds=0 ; kde=0 ; kms=0 ; kme=0 ; kps=0 ; kpe=0

!   allocate filter_all:

    do k=1,7
      allocate(filter_all(k)%istart(2),filter_all(k)%ib(2))
      allocate(filter_all(k)%nrecv(2),filter_all(k)%ndrecv(2))
      allocate(filter_all(k)%nsend(2),filter_all(k)%ndsend(2))
      allocate(filter_all(k)%lnf(2,2,2,2),filter_all(k)%bnf(2,2,2))
      allocate(filter_all(k)%amp(2,2,2,2),filter_all(k)%ia(2))
      allocate(filter_all(k)%ja(2),filter_all(k)%ka(2))

    end do

!    set other parameters to default values

    npass=1
    ifilt_ord=4
    triad4=.true.
    binom=.true.
    normal=-200
    ngauss=3
    rgauss=zero
    an_amp=one/three
    an_vs=one
    grid_ratio=2._r_kind
    an_flen_u=-one      ! this turns off anisotropic coupling to horizontal wind
    an_flen_t=-one      ! this turns off anisotropic coupling to grad(pot temp)
    an_flen_z=-one      ! this turns off anisotropic coupling to grad(terrain)
    rgauss(1)=half
    rgauss(2)=one
    rgauss(3)=two

  end subroutine init_anberror


  subroutine create_anberror_vars(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_anberror_vars  create arrays for anisotropic global background error
!   prgmmr: parrish          org: np23                date: 2005-02-08
!
! abstract: creates arrays for anisotropic global background error
!
! program history log:
!   2004-01-01  parrish
!
!   input argument list:
!    mype     - mpi task id!
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

    use jfunc, only: nrclen
    use berror, only: varprd
    implicit none

    integer(i_kind),intent(in):: mype
  
    allocate(varprd(nrclen))

!  compute vertical partition variables used by anisotropic filter code

    call anberror_vert_partition(mype)

  end subroutine create_anberror_vars


  subroutine destroy_anberror_vars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_anberror_vars  deallocates anisotropic global background error arrays
!   prgmmr: parrish          org: np23                date: 2005-02-08
!
! abstract: deallocates global anisotropic background error arrays
!
! program history log:
!   2005-02-08  parrish
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
  end subroutine destroy_anberror_vars


  subroutine create_anberror_vars_reg(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_anberror_vars_reg  create arrays for anisotropic reg background error
!   prgmmr: parrish          org: np23                date: 2005-02-08
!
! abstract: creates arrays for regional background error
!
! program history log:
!   2005-02-08  parrish
!   2005-05-24  pondeca - take into consideration that nrclen=0 for 2dvar only
!                         surface analysis option!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: i_kind
    use fgrid2agrid_mod, only: nlatf,nlonf, &
        create_fgrid2agrid
    use jfunc, only: nrclen
    use berror, only: varprd
    implicit none
  
    integer(i_kind),intent(in):: mype
    
    allocate(varprd(max(1,nrclen)))

!   compute vertical partition variables used by anisotropic filter code

    call anberror_vert_partition(mype)

!   initialize fgrid2agrid interpolation constants

    call create_fgrid2agrid(grid_ratio)

    ids=1 ; ide=nlatf
    jds=1 ; jde=nlonf
    ims=ids ; ime=ide ; ips=ids ; ipe=ide
    jms=jds ; jme=jde ; jps=jds ; jpe=jde

    allocate(qvar3d(lat2,lon2,nsig))

  end subroutine create_anberror_vars_reg


  subroutine anberror_vert_partition(mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    anberror_vert_partition
!
!   prgrmmr:
!
! abstract:      using existing vertical ordering of variables, create 
!                modified indexing compatable with anisotropic filter code.
!
! program history log:
!   2008-06-05  safford -- add subprogram doc block
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    use kinds, only: i_kind
    use gridmod, only: nsig,nsig1o
    use mpimod, only: levs_id,nvar_id,npe,ierror,mpi_comm_world, &
           mpi_max,mpi_integer4
    implicit none

    integer(i_kind),intent(in):: mype

    integer(i_kind) idvar_last,k,kk,vlevs
    integer(i_kind) nlevs0(0:npe-1),nlevs1(0:npe-1),nvar_id0(nsig1o*npe),nvar_id1(nsig1o*npe)

    vlevs=6*nsig+4       !  all variables
    kds=1 ; kde=vlevs

!  initialize nvars,idvar,kvar_start,kvar_end
! Determine how many vertical levels each mpi task will
! handle in the horizontal smoothing
    nvars=10
    allocate(idvar(kds:kde),jdvar(kds:kde),kvar_start(nvars),kvar_end(nvars))
    allocate(var_names(nvars))
    var_names( 1)="st"
    var_names( 2)="vp"
    var_names( 3)="ps"
    var_names( 4)="tv"
    var_names( 5)="q"
    var_names( 6)="oz"
    var_names( 7)="sst"
    var_names( 8)="stl"
    var_names( 9)="sti"
    var_names(10)="cw"

!                     idvar  jdvar
!                       1      1       stream function
!                       2      2       velocity potential
!                       3      3       surface pressure
!                       4      4       virtual temperature
!                       5      5       specific humidity
!                       6      6       ozone
!                       7      7       sst
!                      10      8       cloud water
!                       8      9       surface temp (land)
!                       9     10       surface temp (ice)

    nlevs0=0
    do k=1,nsig1o
       if (levs_id(k)/=0) nlevs0(mype)=nlevs0(mype)+1
             if(k.eq.1.or.k.ge.nsig1o-2) write(6,*)' k,levs_id(k)=',k,levs_id(k)
    end do
    call mpi_allreduce(nlevs0,nlevs1,npe,mpi_integer4,mpi_max,mpi_comm_world,ierror)
    nvar_id0=0
    do k=1,nsig1o
     nvar_id0(mype*nsig1o+k)=nvar_id(k)
    end do
    call mpi_allreduce(nvar_id0,nvar_id1,npe*nsig1o,mpi_integer4,mpi_max,mpi_comm_world,ierror)

    kk=0
    do k=1,npe*nsig1o
     if(nvar_id1(k).gt.0) then
      kk=kk+1
      jdvar(kk)=nvar_id1(k)
     end if
    end do
    idvar_last=0
    kk=0
    do k=kds,kde
     if(jdvar(k).ne.idvar_last) then
      idvar_last=jdvar(k)
      kk=kk+1
     end if
     idvar(k)=kk
    end do
    idvar_last=0
    do k=kds,kde
     if(idvar(k).ne.idvar_last) then
      idvar_last=idvar(k)
      kvar_start(idvar_last)=k
     end if
    end do
    idvar_last=0
    do k=kde,kds,-1
     if(idvar(k).ne.idvar_last) then
      idvar_last=idvar(k)
      kvar_end(idvar_last)=k
     end if
    end do

          if(mype.eq.0) then
              do k=kds,kde
               write(6,*)' in anberror_vert_partition, k,idvar(k),jdvar(k)=',k,idvar(k),jdvar(k)
              end do
              do k=1,nvars
               write(6,*)' k,kvar_start,end(k)=',k,kvar_start(k),kvar_end(k)
              end do
          end if
    kpe=0
    do k=0,mype
     kpe=kpe+nlevs1(k)
    end do
    kps=kpe-nlevs1(mype)+1
    kms=kps ; kme=kpe
            write(6,*)' in anberror_vert_partition, kps,kms,kpe,kme=',kps,kms,kpe,kme

  end subroutine anberror_vert_partition


  subroutine destroy_anberror_vars_reg
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_berror_vars_reg  deallocate reg anisotropic background 
!                                         error arrays
!   prgmmr: parrish          org: np23                date: 2005-02-08
!
! abstract: deallocates regional anisotropic background error arrays
!
! program history log:
!   2005-02-08  parrish
!   2007-08-21  pondeca - add qvar3d deallocate
!   2008-06-05  safford - rm unused var
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

    deallocate(qvar3d)
  end subroutine destroy_anberror_vars_reg

end module anberror
