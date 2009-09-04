module qcmod
!$$$  module documentation block
!                .      .    .                                       .
! module:    qcmod
!   prgmmr: kleist           org: w/nmc20             date: 2003-09-30
!
! abstract: module containing data quality control variables
!
! program history log:
!   2003-09-30  kleist
!   2004-05-18  kleist, documentation
!   2004-07-23  derber - modify to include conventional sst
!   2004-10-12  parrish - modifications for nonlinear qc
!   2004-12-02  treadon - initialize b_ref and pg_ref
!   2005-01-20  okamoto - add ermax for ssmi/amsre/ssmis
!   2005-04-07  treadon - add logical flags to indicate nonlinear qc
!                         is on (=.true.) or off (=.false.)
!   2005-05-27  derber  - level output change
!   2005-08-03  derber  - remove qc parameters for conventional data
!   2005-09-29  derber  - remove qc parameters for sat and pcp data, move cg_term to constants 
!   2006-01-31  derber  - correct bug in upprof and dwprof loop logical test
!   2006-05-24  treadon - add vadfile to carry name of vad wind bufr file
!   2006-05-22  su - add noiqc flag
!   2006-07-28  derber  - add dfact1, initialize
!   2006-08-07  treadon - remove nlnqc_oz (not used)
!   2007-04-16       su - add c_varqc for determining the spped to turn on var. qc
!   2008-06-03  treadon - add use_poq7
!
! remarks: variable definitions below
!   def dfact           - factor for duplicate obs at same location for conv. data
!   def dfact1          - time factor for duplicate obs at same location for conv. data
!   def repe_dw         - factor for error in radar doppler winds
!   def repe_gps        - factor for error in gps local refractivity or bending angle
!   def erradar_inflate - radar error inflation factor
!   def npres_print     - number of levels for print
!   def ptop,pbot       - arrays containing top pressure and bottom pressure of print levels
!   def ptopq,pbotq     - arrays containing top pressure and bottom pressure of print levels for q
!   def ptopo3,pboto3   - arrays containing top pressure and bottom pressure of print levels for o3 levels
!   def vadfile         - local name of bufr file containing vad winds (used by read_radar)
!   def use_poq7        - if true, accept sbuv/2 obs with profile ozone quality flag 7
!
!    following used for nonlinear qc:
!
!   def nlnqc_iter   - logical flag (T=nonlinear qc on, F=nonlinear qc off) for iteration
!
!   def noiqc        - logic flag for oiqc, noiqc='false' with oiqc on
!
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$ end documentation block

  use kinds, only: i_kind,r_kind
  implicit none

  logical nlnqc_iter
  logical noiqc
  logical use_poq7

  character(10):: vadfile
  integer(i_kind) npres_print
  real(r_kind) dfact,dfact1,repe_dw,repe_gps,erradar_inflate,c_varqc
  real(r_kind) varqc_iter
  real(r_kind),allocatable,dimension(:)::ptop,pbot,ptopq,pbotq,ptopo3,pboto3

contains
 
  subroutine init_qcvars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_qcvars
!   prgmmr: kleist           org: np20                date: 2003-09-30
!
! abstract: initialize variables used in data quality control
!
! program history log:
!   2003-09-30  kleist
!   2004-05-18  kleist, documentation
!   2004-07-23  derber  - modify to include conventional sst
!   2005-01-20  okamoto - add ermax for ssmi/amsre/ssmis
!   2005-02-18  treadon - reduce ps gross limit from 10.0 to 5.0
!   2005-03-08  cucurull - reduce gps ro gross limit from 10.0 to 3.0
!   2005-06-03  cucurull - increase gps ro gross limit from 3.0 to 10.0
!   2007-01-09  sienkiewicz - new levels for ozone stat printout
!   2008-04-23  safford  - rm unused parameter
!   2008-09-05  lueken   - merged ed's changes into q1fy09 code
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
    use constants, only: zero,one,three,four,r1000,h300
    implicit none
!   real(r_kind),parameter:: ten=10.0_r_kind

    npres_print = 12
    allocate(ptop(npres_print),pbot(npres_print),ptopq(npres_print), &
             pbotq(npres_print),ptopo3(npres_print),pboto3(npres_print))
    
! Set pressure level groupings.  There are npres_print groupings
    ptop(1) = r1000;   pbot(1)=  1200.0
    ptop(2) = 900.0;    pbot(2)=  999.9
    ptop(3) = 800.0;    pbot(3)=  899.9
    ptop(4) = 600.0;    pbot(4)=  799.9
    ptop(5) = 400.0;    pbot(5)=  599.9
    ptop(6) = h300;     pbot(6)=  399.9
    ptop(7) = 250.0;    pbot(7)=  299.9
    ptop(8) = 200.0;    pbot(8)=  249.9
    ptop(9) = 150.0;    pbot(9)=  199.9
    ptop(10)= 100.0;    pbot(10)= 149.9
    ptop(11)= 50.0;     pbot(11)= 99.9
    ptop(12)= zero;     pbot(12)= 2000.0

    ptopq(1)=  r1000;  pbotq(1)=  1200.0
    ptopq(2)=  950.0;   pbotq(2)=  999.9
    ptopq(3)=  900.0;   pbotq(3)=  949.9
    ptopq(4)=  850.0;   pbotq(4)=  899.9
    ptopq(5)=  800.0;   pbotq(5)=  849.9
    ptopq(6)=  700.0;   pbotq(6)=  799.9
    ptopq(7)=  600.0;   pbotq(7)=  699.9
    ptopq(8)=  500.0;   pbotq(8)=  599.9
    ptopq(9)=  400.0;   pbotq(9)=  499.9
    ptopq(10)= h300;    pbotq(10)= 399.9
    ptopq(11)= zero;    pbotq(11)= 299.9
    ptopq(12)= zero;    pbotq(12)= 2000.0

    ptopo3(1) = 120.0;  pboto3(1) = h300
    ptopo3(2) =  70.0;  pboto3(2) = 119.9
    ptopo3(3) =  40.0;  pboto3(3) =  69.9
    ptopo3(4) =  25.0;  pboto3(4) =  39.9
    ptopo3(5) =  12.0;  pboto3(5) =  24.99
    ptopo3(6) =   7.0;  pboto3(6) =  11.99
    ptopo3(7) =  four;  pboto3(7) =   6.99
    ptopo3(8) =   2.5;  pboto3(8) =   3.99
    ptopo3(9) =   1.2;  pboto3(9) =  2.499
    ptopo3(10) =  0.7;  pboto3(10) =  1.199
    ptopo3(11) =  0.4;  pboto3(11) =  0.699
    ptopo3(12) = zero;  pboto3(12) = 2000.0

    dfact    = zero
    dfact1   = three
    repe_dw  = one
    repe_gps = one
    varqc_iter=one

    erradar_inflate   = one

    nlnqc_iter= .false.
    noiqc = .false.
    c_varqc=one

    vadfile='none'

    use_poq7 = .false.

    return
  end subroutine init_qcvars

  subroutine errormod(pq,vq,levs,plevs,errout,k,presl,dpres,nsig,lim_qm)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    errormod
!   prgmmr: derber           org: np23                date: 2003-09-30
!
! abstract: adjust observation error for conventional obs
!
! program history log:
!   2003-09-30  derber
!   2004-05-18  kleist, documentation
!   2004-10-26  kleist - add 0.5 half-layer factor
!   2006-02-15  treadon - add (l==levs,1) exit to upprof and dwprof loops
!   2006-12-20  Sienkiewicz  multiply tiny_r_kind in errout div-by-zero
!                            check by expected largest value for numerator
!                            max(2*vmax) = max(dpres) ~= 5 cb
!   2008-04-23  safford - rm unused vars and uses
!   2008-09-05  lueken  - merged ed's changes into q1fy09 code
!
!   input argument list:
!     pq     - pressure quality mark
!     vq     - observation quality mark (t,q,wind)
!     levs   - number of levels in profile for observation
!     plevs  - observation pressures
!     errout - observation error 
!     k      - observation level 
!     presl  - model pressure at half sigma levels
!     dpres  - delta pressure between model pressure levels
!     nsig   - number of vertical levels
!     lim_qm - qc limit 
!
!   output argument list:
!     errout - adjusted observation error
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use constants, only: one,two,tiny_r_kind,half,rd,grav,five
    implicit none

    integer(i_kind),intent(in):: levs,k,nsig,lim_qm
    real(r_kind),dimension(255),intent(in):: plevs
    real(r_kind),dimension(nsig),intent(in):: presl
    real(r_kind),dimension(nsig-1),intent(in):: dpres
    integer(i_kind),dimension(255),intent(in):: pq,vq
    real(r_kind),intent(inout):: errout

    integer(i_kind) n,l,ilev
    real(r_kind):: vmag,pdiffu,pdiffd,con
    
    errout=one
    if(levs == 1)return
    ilev=1
    do n=2,nsig-1
      if(plevs(k) < presl(n))ilev=n
    end do
    con=grav*500._r_kind/(273._r_kind*rd)
    vmag=min(max(half*dpres(ilev),0.02_r_kind*presl(ilev)),con*plevs(k))

!   vmag=max(half*dpres(ilev),0.02_r_kind*presl(ilev))
    pdiffu=vmag
    pdiffd=vmag
    if(pq(k) < lim_qm .and. vq(k) < lim_qm)then
! Move up through the profile.  
      l=k

! Array plevs is only defined from l=1 to l=levs.  Hence the check below
      if (l+1<=levs) then
        upprof: do while (abs(plevs(k)-plevs(l+1)) < vmag .and. l <= levs-1) 
          l=l+1
          if(pq(l) < lim_qm .and. vq(l) < lim_qm)then
            pdiffu=abs(plevs(k)-plevs(l))
            exit upprof
          end if
          if (l==levs) exit upprof
        end do upprof
      endif
        
! Reset the level and move down through the profile
      l=k

! The check (l>=2) ensures that plevs(l-1) is defined
      if (l>=2) then
         dwprof: do while (abs(plevs(l-1)-plevs(k)) < vmag .and. l >= 2) 
          l=l-1
          if(pq(l) < lim_qm .and. vq(l) < lim_qm)then
            pdiffd=abs(plevs(l)-plevs(k))
            exit dwprof
          end if
          if (l==1) exit dwprof
        end do dwprof
      endif

! Set adjusted error
      errout=sqrt(two*vmag/max(pdiffd+pdiffu,five*tiny_r_kind))

! Quality marks indicate bad data.  Set error to large value.
    else
      errout=1.e6_r_kind
    end if

    return
  end subroutine errormod
end module qcmod
