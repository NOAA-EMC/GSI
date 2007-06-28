!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_utests - unit-testing environment for fvAnaGrid or gsiGuess
!
! !DESCRIPTION:
!
! !INTERFACE:
!#include "regime.H"

    module m_utests
      implicit none
      private	! except

#ifdef _GMAO_FVGSI_
      public :: utests_init
      public :: utests_clean

    interface utests_init ; module procedure init_ ; end interface
    interface utests_clean; module procedure clean_; end interface

! !REVISION HISTORY:
! 	09Dec04	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!   2006-04-12  treadon - replace sigi with bk5; remove sigl
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='m_utests'

contains
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: init_ - initialize the environment for testing
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine init_(nsig,jcap,root,comm)
      use jfunc, only : jiterstart
      use m_mpif90,only : MP_comm_size
      use m_mpif90,only : MP_comm_rank
      use m_die,only : MP_die
      implicit none
      integer,intent(in) :: nsig	! case of vertical levels
      integer,intent(in) :: jcap	! case of spectal resolutions
      integer,intent(in) :: root
      integer,intent(in) :: comm

! !REVISION HISTORY:
! 	09Dec04	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::init_'
  integer :: mPEs
  integer :: myPE
  integer :: ier

  call MP_comm_size(comm,mPEs,ier)
  	if(ier/=0) call MP_die(myname_,'MP_comm_size()',ier)
  call MP_comm_rank(comm,myPE,ier)
  	if(ier/=0) call MP_die(myname_,'MP_comm_rank()',ier)

  jiterstart=1

  call set_sigma_(nsig,mPEs,myPE)
  call set_subdomains_(jcap,mPEs,myPE)

contains

subroutine set_sigma_(nsig_,mPEs,myPE)

  use gridmod,only : nsig,bk5
  use gridmod,only : hybrid,bk5

  use guess_grids, only : nfldsig,ifilesig
  use guess_grids, only : ntguessig

  use m_die,only : die
  implicit none
  integer,intent(in) :: nsig_
  integer,intent(in) :: mPEs
  integer,intent(in) :: myPE
  integer:: k

  nsig=nsig_
  hybrid=.false.

  	! nfldsig defines the size of ges_(:,:,:,nfldsig) variables.

  nfldsig  =1
  ifilesig(1)=0
  ntguessig=1		! where the analysis is.

  	allocate(bk5(nsig+1))
	allocate(ak5(nsig+1))

  do k=1,nsig+1
     ak5(k)= 0.000000
  end do

  select case(nsig)
    case(28)
      bk5(01) = 1.000000
      bk5(02) = 0.990000
      bk5(03) = 0.974180
      bk5(04) = 0.954590
      bk5(05) = 0.930540
      bk5(06) = 0.901350
      bk5(07) = 0.866420
      bk5(08) = 0.825270
      bk5(09) = 0.777730
      bk5(10) = 0.724010
      bk5(11) = 0.664820
      bk5(12) = 0.601350
      bk5(13) = 0.535290
      bk5(14) = 0.468600
      bk5(15) = 0.403340
      bk5(16) = 0.341370
      bk5(17) = 0.284210
      bk5(18) = 0.232860
      bk5(19) = 0.187830
      bk5(20) = 0.149160
      bk5(21) = 0.116540
      bk5(22) = 0.089450
      bk5(23) = 0.067230
      bk5(24) = 0.049200
      bk5(25) = 0.034690
      bk5(26) = 0.023090
      bk5(27) = 0.013860
      bk5(28) = 0.006570
      bk5(29) = 0.000000

    case(42)
      bk5(01) = 1.00000000
      bk5(02) = 0.99197000
      bk5(03) = 0.98273998
      bk5(04) = 0.97215998
      bk5(05) = 0.96006000
      bk5(06) = 0.94625998
      bk5(07) = 0.93061000
      bk5(08) = 0.91293001
      bk5(09) = 0.89306003
      bk5(10) = 0.87085998
      bk5(11) = 0.84619999
      bk5(12) = 0.81902999
      bk5(13) = 0.78930998
      bk5(14) = 0.75708002
      bk5(15) = 0.72245997
      bk5(16) = 0.68564999
      bk5(17) = 0.64691001
      bk5(18) = 0.60661000
      bk5(19) = 0.56515998
      bk5(20) = 0.52305001
      bk5(21) = 0.48076999
      bk5(22) = 0.43886000
      bk5(23) = 0.39780000
      bk5(24) = 0.35804999
      bk5(25) = 0.32001001
      bk5(26) = 0.28400999
      bk5(27) = 0.25029001
      bk5(28) = 0.21901000
      bk5(29) = 0.19025999
      bk5(30) = 0.16406000
      bk5(31) = 0.14036000
      bk5(32) = 0.11906000
      bk5(33) = 0.10005000
      bk5(34) = 0.08316000
      bk5(35) = 0.06824000
      bk5(36) = 0.05512000
      bk5(37) = 0.04362000
      bk5(38) = 0.03357000
      bk5(39) = 0.02482000
      bk5(40) = 0.01722000
      bk5(41) = 0.01063000
      bk5(42) = 0.00492000
      bk5(43) = 0.00000000

    case(64)
      bk5(01) = 1.
      bk5(02) = 0.994670987
      bk5(03) = 0.988632023
      bk5(04) = 0.98180002
      bk5(05) = 0.974083006
      bk5(06) = 0.96538502
      bk5(07) = 0.955603004
      bk5(08) = 0.94463098
      bk5(09) = 0.932359993
      bk5(10) = 0.918677986
      bk5(11) = 0.903479993
      bk5(12) = 0.88666302
      bk5(13) = 0.868139029
      bk5(14) = 0.847829998
      bk5(15) = 0.825685024
      bk5(16) = 0.801676989
      bk5(17) = 0.775811017
      bk5(18) = 0.748133004
      bk5(19) = 0.718729019
      bk5(20) = 0.687731028
      bk5(21) = 0.655315995
      bk5(22) = 0.621704996
      bk5(23) = 0.587159991
      bk5(24) = 0.551973999
      bk5(25) = 0.516462982
      bk5(26) = 0.480955005
      bk5(27) = 0.445778012
      bk5(28) = 0.411249012
      bk5(29) = 0.377658993
      bk5(30) = 0.345268995
      bk5(31) = 0.314300001
      bk5(32) = 0.284927994
      bk5(33) = 0.257283986
      bk5(34) = 0.231454
      bk5(35) = 0.207481995
      bk5(36) = 0.185371995
      bk5(37) = 0.165098995
      bk5(38) = 0.146607995
      bk5(39) = 0.129822999
      bk5(40) = 0.114655003
      bk5(41) = 0.101002
      bk5(42) = 8.875600249E-2
      bk5(43) = 7.780800015E-2
      bk5(44) = 6.804899871E-2
      bk5(45) = 5.936999992E-2
      bk5(46) = 5.167099833E-2
      bk5(47) = 4.485499859E-2
      bk5(48) = 3.883099928E-2
      bk5(49) = 3.351499885E-2
      bk5(50) = 2.882999927E-2
      bk5(51) = 2.470799908E-2
      bk5(52) = 2.108399943E-2
      bk5(53) = 1.790099964E-2
      bk5(54) = 1.510700025E-2
      bk5(55) = 1.265799999E-2
      bk5(56) = 1.051099971E-2
      bk5(57) = 8.631000295E-3
      bk5(58) = 6.984999869E-3
      bk5(59) = 5.54399984E-3
      bk5(60) = 4.284000024E-3
      bk5(61) = 3.183000023E-3
      bk5(62) = 2.219999908E-3
      bk5(63) = 1.378000015E-3
      bk5(64) = 6.419999991E-4
      bk5(65) = 0.E+0

    case default
      call die(myname_,'unsupported case of nsig',nsig)
  end select
end subroutine set_sigma_

subroutine set_subdomains_(jcap,mPEs,myPE)
  use gridmod,only : nlon,jstart,jlon1,lon1,lon2
  use gridmod,only : nlat,istart,ilat1,lat1,lat2

  use m_mpif90,only : MP_type
  use m_mpif90,only : MP_dims_create
  use m_die,only : MP_die
  implicit none

  integer,intent(in) :: jcap
  integer,intent(in) :: mPEs
  integer,intent(in) :: myPE

  integer,dimension(2) :: ldims
  integer :: niPE,iPE,iGloc,iGlen
  integer :: njPE,jPE,jGloc,jGlen
  integer :: mType

	! nlat is expected to be the full size, including poles.

  nlat=(jcap+1)*3/2
  nlat=nlat+2		! to include the polse
  nlon=nlat*2
  		! jcap= 62, nlat= 96, nlon=192
  		! jcap=126, nlat=192, nlon=384
  		! jcap=170, nlat=258, nlon=516
  		! jcap=254, nlat=384, nlon=768

		! Note there are small differences from what GSI
		! may have now.
!________________________________________

	ldims(1:2)=0
	call MP_dims_create(mPEs,2,ldims,ier)
  		if(ier/=0) call MP_die(myname_,'MP_dims_create()',ier)

  	! ldims(1:2) could be as bad as niPE X njPE = 1 X mPEs

  niPE=ldims(1)
  njPE=ldims(2)

	! partition for a 2-d working subdomain distribution.

  jPE=    myPE/niPE	! latitudinal
  iPE=mod(myPE,niPE)	! longitudinal

  call SimplePartition_(nlon,niPE,iPE,count=iGlen,displ=iGloc)
  call SimplePartition_(nlat,njPE,jPE,count=jGlen,displ=jGloc)

  iGloc=iGloc+1
  jGloc=jGloc+1

    	allocate(jstart(mPEs),jlon1(mPEs))
    	allocate(istart(mPEs),ilat1(mPEs))

  mType=MP_type(iGloc)
  call MPI_allgather(iGloc,1,mtype,jstart,1,mtype,comm,ier)
  	if(ier/=0) call MP_die(myname_,'MPI_allgather(jstart)',ier)
  call MPI_allgather(iGlen,1,mtype,jlon1 ,1,mtype,comm,ier)
  	if(ier/=0) call MP_die(myname_,'MPI_allgather(jlon1)',ier)
  call MPI_allgather(jGloc,1,mtype,istart,1,mtype,comm,ier)
  	if(ier/=0) call MP_die(myname_,'MPI_allgather(istart)',ier)
  call MPI_allgather(jGlen,1,mtype,ilat1 ,1,mtype,comm,ier)
  	if(ier/=0) call MP_die(myname_,'MPI_allgather(ilat1)',ier)

  lon1=jlon1(myPE+1)
  lon2=lon1+2

  lat1=ilat1(myPE+1)
  lat2=lat1+2

end subroutine set_subdomains_
end subroutine init_

subroutine simplePartition_(ngrid,nproc,iproc,count,displ)
  implicit none
  integer,intent(in) :: ngrid
  integer,intent(in) :: nproc
  integer,intent(in) :: iproc
  integer,intent(out) :: count
  integer,intent(out) :: displ

  integer :: resid

  resid=mod(ngrid,nproc)

  count=ngrid/nproc
  if(iproc< resid) count=count+1

  displ=count*iproc
  if(iproc>=resid) displ=displ+resid
end subroutine simplePartition_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: clean_ - clean up dynamic variables
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine clean_()
      use gridmod,only : bk5,ak5
      use gridmod,only : jstart,jlon1
      use gridmod,only : istart,ilat1
      implicit none

! !REVISION HISTORY:
! 	09Dec04	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::clean_'

  deallocate(bk5)
  deallocate(ak5)

  deallocate(jstart)
  deallocate(jlon1 )
  deallocate(istart)
  deallocate(ilat1 )
end subroutine clean_
#endif
end module m_utests
