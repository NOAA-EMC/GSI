subroutine obs_para(ndata,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    obs_para    assign and distribute observations to subdomains
!   prgmmr: weiyu yang       org: np20                date: 1998-05-27
!
! abstract: Based on observation location and domain decomposition, assign
!           and distribute observations to subdomains
!
! program history log:
!   1998-05-27  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-15  treadon, reformat documenation
!   2004-07-23  derber - modify to include conventional sst
!   2004-07-28  treadon - add only to module use, add intent in/out
!   2004-11-19  derber - modify to eliminate file and change to use logical 
!                        rather than weights
!   2005-06-14  wu      - add OMI total ozone
!   2005-09-08  derber - simplify data set handling
!   2005-10-17  treadon - fix bug in defnition of mype_diaghdr
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-07-28  derber  - eliminate counting of data and clean up
!   2008-04-29  safford - rm unused vars and uses
!   2008-06-30  derber  - optimize calculation of criterion
!   2008-09-08  lueken  - merged ed's changes into q1fy09 code
!   2009-04-21  derber  - reformulate to remove communication
!   2008-05-10  meunier - handle for lagrangian data
!
!   input argument list:
!     ndata(*,1)- number of prefiles retained for further processing
!     ndata(*,2)- number of observations read
!     ndata(*,3)- number of observations keep after read
!     mype     - mpi task number
!     ipoint   - pointer in array containing information about all obs type to process
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind
  use constants, only: izero,ione,zero
  use jfunc, only: factqmin,factqmax
  use mpimod, only: npe
  use obsmod, only: obs_setup,dtype,mype_diaghdr,ndat,nsat1, &
              obsfile_all,dplat
  implicit none

! Declare passed variables
  integer(i_kind)                  ,intent(in   ) :: mype
  integer(i_kind),dimension(ndat,3),intent(in   ) :: ndata

! Declare local variables
  integer(i_kind) lunout,is,ii
  integer(i_kind) mm1
  integer(i_kind) ndatax_all
  integer(i_kind),dimension(npe):: nobs_s

!
!****************************************************************
! Begin obs_para here
!
! Distribute observations as a function of pe number.
  nsat1=izero
  mype_diaghdr = -999_i_kind
  mm1=mype+ione
  ndatax_all=izero
  lunout=55_i_kind

!
! Set number of obs on each pe for each data type
  open(lunout,file=obs_setup,form='unformatted')
  rewind lunout
  do is=1,ndat

     if(dtype(is) /= ' ' .and. ndata(is,1) > izero)then

        ndatax_all=ndatax_all + ndata(is,1)

        if (dtype(is)=='lag') then    ! lagrangian data
           call dislag(ndata(is,1),mm1,lunout,obsfile_all(is),dtype(is),&
                nobs_s)
        else                          ! classical observations
           call disobs(ndata(is,1),mm1,lunout,obsfile_all(is),dtype(is), &
                 mype_diaghdr(is),nobs_s)
        end if
        nsat1(is)=nobs_s(mm1)
        if(mm1 == npe)then
           write(6,1000)dtype(is),dplat(is),(nobs_s(ii),ii=1,npe)
1000       format('OBS_PARA: ',2A10,8I10,/,(10X,10I10))
        end if
        
     end if

  end do
  close(lunout)


! If there are no obs available, turn off moisture constraint.  
! If the user still wants the moisture constraint active when no obs are
! present, comment out the block of code below.
  if (ndatax_all == izero) then
     factqmin=zero
     factqmax=zero
     if (mype==izero) write(6,*)'OBS_PARA:  ***WARNING*** no observations to be  ',&
          ' assimilated. reset factqmin,factqmax=',factqmin,factqmax
  endif


  return
end subroutine obs_para

subroutine disobs(ndata,mm1,lunout,obsfile,obstypeall,mype_diag,nobs_s)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    disobs  distribute observations into each pe subdomain
!   prgmmr: weiyu yang                                date: 1998-04-06
!
! abstract: distribute observations into each pe subdomain
!
! program history log:
!   1998-04-06  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-15  treadon, reformat documenation
!   2004-07-28  treadon - add only to module use, add intent in/out
!   2004-11-19  derber - change to eliminate additional file and use logical
!                        rather than weights
!   2005-10-17  treadon - remove conversion to grid-relative cooridnates
!   2006-04-06  middlecoff - changed lunin from 15 to 11
!   2008-04-29  safford - rm unused vars and uses
!   2008-09-08  lueken  - merged ed's changes into q1fy09 code
!   2009-04-21  derber  - reformulate to remove communication
!
!   input argument list:
!     nn_obs   - number of an observation data
!     ndata    - number of observations
!     lunin    - unit from which to read all obs of a given type
!     lunout   - unit to which to write subdomain specific observations
!     mm1      - mpi task number + 1
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: izero,ione
  use gridmod, only: periodic_s,nlon,nlat,jlon1,ilat1,istart,jstart
  use mpimod, only: npe
  implicit none

! Declare passed variables
  integer(i_kind)               ,intent(in   ) :: ndata,lunout,mm1
  integer(i_kind),dimension(npe),intent(inout) :: nobs_s
  integer(i_kind)               ,intent(  out) :: mype_diag
  character(14)                 ,intent(in   ) :: obsfile
  character(10)                 ,intent(in   ) :: obstypeall

! Declare local variables
  integer(i_kind) lon,lat,lat_data,lon_data,n,k,lunin
  integer(i_kind) jj,nreal,nchanl,nn_obs,ndatax
  integer(i_kind) ndata_s,klim
  integer(i_kind),dimension(npe):: ibe,ibw,ibn,ibs
  logical,allocatable,dimension(:):: luse,luse_s
  real(r_kind),allocatable,dimension(:,:):: obs_data,data1_s
  integer(i_kind),allocatable,dimension(:):: nprocs
  character(10):: obstype
  character(20):: isis

  mype_diag=npe

! Read and write header

  do k=1,npe

!    ibw,ibe,ibs,ibn west,east,south and north boundaries of total region
     ibw(k)=jstart(k)-ione
     ibe(k)=jstart(k)+jlon1(k)-ione
     ibs(k)=istart(k)-ione
     ibn(k)=istart(k)+ilat1(k)-ione

  end do

  lunin=11_i_kind
  open(lunin,file=obsfile,form='unformatted')
  read(lunin)obstype,isis,nreal,nchanl,lat_data,lon_data
  if(obstype /=obstypeall) &
        write(6,*)'DISOBS:  ***ERROR***   obstype,obstypeall=',obstype,obstypeall

  nn_obs = nreal + nchanl

  allocate(obs_data(nn_obs,ndata))
!  Read in all observations of a given type along with subdomain flags
  read(lunin) obs_data
  close(lunin)

  allocate(luse(ndata),nprocs(ndata))
  luse=.false.
  nprocs=999999_i_kind
  nobs_s=izero

! Loop over all observations.  Locate each observation with respect
! to subdomains.
  do n=1,ndata
     lat=obs_data(lat_data,n)
     lat=min(max(ione,lat),nlat)

     klim=max(mm1,mype_diag)
     do k=1,klim
        if(lat>=ibs(k).and.lat<=ibn(k)) then
           lon=obs_data(lon_data,n)
           lon=min(max(izero,lon),nlon)
           if((lon >= ibw(k).and. lon <=ibe(k))  .or.  &
              (lon == izero   .and. ibe(k) >=nlon) .or.  &
              (lon == nlon    .and. ibw(k) <=ione) .or. periodic_s(k)) then
              nobs_s(k)=nobs_s(k)+ione
              nprocs(n)=min(nprocs(n),k)
              mype_diag=min(mype_diag,k-ione)
              if(k == mm1)luse(n)=.true.
           end if
        end if
     end do
  end do 
  ndata_s = nobs_s(mm1)
     

  if(ndata_s > izero)then
     allocate(data1_s(nn_obs,ndata_s),luse_s(ndata_s))
     ndatax=izero
     do n=1,ndata

        if(luse(n))then

           ndatax=ndatax+ione
           luse_s(ndatax)= mm1 == nprocs(n)

           do jj= 1,nn_obs
              data1_s(jj,ndatax) = obs_data(jj,n)
           end do

        end if

     end do


! Write observations for given task to output file
     write(lunout) obstypeall,isis,nreal,nchanl
     write(lunout) data1_s,luse_s
     deallocate(data1_s,luse_s)
  endif
  deallocate(obs_data,luse,nprocs)


  return
end subroutine disobs

! ------------------------------------------------------------------------
subroutine dislag(ndata,mm1,lunout,obsfile,obstypeall,ndata_s)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dislag  distribute lagrangian observations into each pe 
!                subdomain
!   prgmmr: lmeunier                                  date: 2009-03-12
!
! abstract: distribute lagrangian observations into each pe subdomain
!           (based on disobs). All observations of one balloon are
!           are associated with a given processor
!
! program history log:
!   2009-03-12  lmeunier
!
!   input argument list:
!     ndata_s  - number of observations in each pe sub-domain
!     ndata    - number of observations
!     obsfile  - unit from which to read all obs of a given type
!     lunout   - unit to which to write subdomain specific observations
!     mm1      - mpi task number + 1
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: izero,ione
  use lag_fields, only: orig_lag_num
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: ndata,lunout,mm1
  integer(i_kind),intent(inout) :: ndata_s
  character(14)  ,intent(in   ) :: obsfile
  character(10)  ,intent(in   ) :: obstypeall

! Declare local variables
  integer(i_kind) num_data,n,lunin,num
  integer(i_kind) jj,nreal,nchanl,nn_obs,ndatax
  logical,allocatable,dimension(:):: luse,luse_s,luse_x
  real(r_kind),allocatable,dimension(:,:):: obs_data,data1_s
  character(10):: obstype
  character(20):: isis

  ndata_s=izero

  lunin=11_i_kind
  open(lunin,file=obsfile,form='unformatted')
  read(lunin)obstype,isis,nreal,nchanl,num_data
  if(obstype /=obstypeall) &
        write(6,*)'DISLAG:  ***ERROR***   obstype,obstypeall=',obstype,obstypeall

  nn_obs = nreal + nchanl

  allocate(obs_data(nn_obs,ndata))
! Read in all observations of a given type along with subdomain flags
  read(lunin) obs_data
  close(lunin)

  allocate(luse(ndata),luse_x(ndata))
  luse=.false.
  luse_x=.false.

! Loop over all observations.  Locate each observation with respect
! to subdomains.
  use: do n=1,ndata

!    Does the observation belong to the subdomain for this task?
     num=int(obs_data(num_data,n),i_kind)  
     if ((mm1-ione)==orig_lag_num(num,2)) then
        ndata_s=ndata_s+ione
        luse(n)=.true.
        luse_x(n)=.true.  ! Never on another subdomain
     end if

  end do use

  if(ndata_s > izero)then
     allocate(data1_s(nn_obs,ndata_s),luse_s(ndata_s))
     ndatax=izero
     do n=1,ndata

        if(luse(n))then

           ndatax=ndatax+ione
           luse_s(ndatax)=luse_x(n)
 
           do jj= 1,nn_obs
              data1_s(jj,ndatax) = obs_data(jj,n)
           end do

        end if

     end do

!    Write observations for given task to output file
     write(lunout) obstypeall,isis,nreal,nchanl
     write(lunout) data1_s,luse_s
     deallocate(data1_s,luse_s)
  endif
  deallocate(obs_data,luse,luse_x)

  return
end subroutine dislag
