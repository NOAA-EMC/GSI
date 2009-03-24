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
  use kinds, only: r_kind,i_kind
  use constants, only: zero,izero
  use jfunc, only: factqmin,factqmax
  use mpimod, only: mpi_comm_world,ierror,mpi_sum,npe,mpi_integer
  use obsmod, only: obs_setup,dtype,mype_diaghdr,ndat,nsat1, &
              obsfile_all,dplat
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype
  integer(i_kind),dimension(ndat,3),intent(in):: ndata

! Declare local variables
  integer(i_kind) lunout,is,ii
  integer(i_kind) mm1
  integer(i_kind) nobsp,ndatax_all
  integer(i_kind),dimension(npe,ndat):: nobs_s,nobs_s1

!
!****************************************************************
! Begin obs_para here
!
! Distribute observations as a function of pe number.
  nsat1=izero
  nobs_s = izero
  nobs_s1 = izero
  mype_diaghdr = -999
  mm1=mype+1
  ndatax_all=izero
  lunout=55

!
! Set number of obs on each pe for each data type
  open(lunout,file=obs_setup,form='unformatted')
  rewind lunout
  do is=1,ndat

     if(dtype(is) /= ' ' .and. ndata(is,1) > izero)then

        ndatax_all=ndatax_all + ndata(is,1)
        call disobs(ndata(is,1),nobsp,mm1,lunout,obsfile_all(is),dtype(is))

        nobs_s(mm1,is)=nobsp
        nsat1(is)=nobsp
        
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

  call mpi_allreduce(nobs_s,nobs_s1,ndat*npe,mpi_integer,mpi_sum, &
       mpi_comm_world,ierror)

! Determine first task with data for each observation type
  do is=1,ndat
    if(dtype(is) /= ' ' .and. ndata(is,1) > izero)then
        do ii=1,npe
           if (nobs_s1(ii,is)>izero) then
              mype_diaghdr(is) = ii-1
              exit
           end if
        end do
        if(mype==izero)then
          write(6,1000)dtype(is),dplat(is),(nobs_s1(ii,is),ii=1,npe)
1000      format('OBS_PARA: ',2A10,8I10,/,(10X,10I10))

        end if
    end if
  end do


  return
end subroutine obs_para

subroutine disobs(ndata,ndata_s,mm1,lunout,obsfile,obstypeall)

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
!
!   input argument list:
!     nn_obs   - number of an observation data
!     ndata_s  - number of observations in each pe sub-domain
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
  use constants, only: izero,one
  use gridmod, only: periodic_s,nlon,nlat,jlon1,ilat1,istart,jstart
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: ndata,lunout,mm1
  integer(i_kind),intent(inout):: ndata_s
  character(14),intent(in):: obsfile
  character(10),intent(in):: obstypeall

! Declare local variables
  integer(i_kind) j,lon,lat,lat_data,lon_data,n,k,lunin
  integer(i_kind) jj,nreal,nchanl,nn_obs,ndatax
  integer(i_kind),dimension(mm1):: ibe,ibw,ibn,ibs
  real(r_kind):: dlat,dlon
  logical,allocatable,dimension(:):: luse,luse_s,luse_x
  real(r_kind),allocatable,dimension(:,:):: obs_data,data1_s
  character(10):: obstype
  character(20):: isis

  ndata_s=0

! Read and write header

  do k=1,mm1

!    ibw,ibe,ibs,ibn west,east,south and north boundaries of total region
     ibw(k)=jstart(k)-1
     ibe(k)=jstart(k)+jlon1(k)-1
     ibs(k)=istart(k)-1
     ibn(k)=istart(k)+ilat1(k)-1

  end do

  lunin=11
  open(lunin,file=obsfile,form='unformatted')
  read(lunin)obstype,isis,nreal,nchanl,lat_data,lon_data
  if(obstype /=obstypeall) &
        write(6,*)'DISOBS:  ***ERROR***   obstype,obstypeall=',obstype,obstypeall

  nn_obs = nreal + nchanl

  allocate(obs_data(nn_obs,ndata))
!  Read in all observations of a given type along with subdomain flags
  read(lunin) obs_data
  close(lunin)

  allocate(luse(ndata),luse_x(ndata))
  luse=.false.
  luse_x=.false.

! Loop over all observations.  Locate each observation with respect
! to subdomains.
  use: do n=1,ndata

     lat=obs_data(lat_data,n)
     lat=min(max(1,lat),nlat)

!    Does the observation belong to the subdomain for this task?

     if(lat>=ibs(mm1).and.lat<=ibn(mm1)) then
        lon=obs_data(lon_data,n)
        lon=min(max(0,lon),nlon)
        if((lon >= ibw(mm1).and. lon <=ibe(mm1))  .or.  &
              (lon == 0       .and. ibe(mm1) >=nlon) .or.  &
              (lon == nlon    .and. ibw(mm1) <= 1)   .or. periodic_s(mm1)) then

            ndata_s=ndata_s+1
            luse(n)=.true.
!           Does the observation belong to the subdomain for other tasks?
            use_obs: do k=1,mm1-1
              if(lat>=ibs(k).and.lat<=ibn(k)) then
                if((lon>=  ibw(k).and. lon<=ibe(k))   .or.  &
                   (lon == 0     .and. ibe(k) >=nlon) .or.  &
                   (lon == nlon  .and. ibw(k) <= 1)   .or. periodic_s(k))  & 
                      cycle use
              end if
            end do use_obs
            luse_x(n)=.true.
        end if
     end if
  end do use
  if(ndata_s > izero)then
     allocate(data1_s(nn_obs,ndata_s),luse_s(ndata_s))
     ndatax=0
     do n=1,ndata

       if(luse(n))then

          ndatax=ndatax+1
          luse_s(ndatax)=luse_x(n)

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
  deallocate(obs_data,luse,luse_x)


  return
end subroutine disobs
