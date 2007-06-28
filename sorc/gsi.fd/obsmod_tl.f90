module obsmod_tl
!$$$   module documentation block
!                .      .    .                                       .
! module:  obsmod_tl
! prgmmr:  yanqiu zhu             org: GMAO               date: 2005-03-16
!
! abstract: This module contains tangent linear variables and arrays pertinent for
!           observational data.
!
! program history log:
!   2005-04-07  yanqiu zhu -  tangent linear of obsmod
!   2005-06-14  wu -  add OMI toz data
!   2006-07-28  derber  - modify to use new inner loop obs data structure
! 
! Subroutines Included:
!   sub init_obsmod_dflts_tl   - initialize obs related variables to default values
!   sub makeobs_tl             - allocate satellite obs related arrays
!   sub makecobs_tl            - allocate obs ges/weighting arrays
!   sub destroyobs_tl          - deallocate obs related arrays
!
! Variable Definitions:
!   def ntdata_dim_tl   - max(1,ntdata) - ensure non-zero array dimension
!   def nwdata_dim_tl   - max(1,nwdata) - ensure non-zero array dimension
!   def nsrwdata_dim_tl - max(1,nsrwdata) - ensure non-zero array dimension
!   def npdata_dim_tl   - max(1,npdata) - ensures non-zero array dimension 
!   def nqdata_dim_tl   - max(1,nqdata) - ensure non-zero array dimension
!   def npwdat_dim_tl   - max(1,npwdata) - ensure non-zero array dimension
!   def nozdata_dim_tl  - max(1,nozdata) - ensure non-zero array dimension
!   def nozodata_dim_tl - max(1,nozodata) - ensure non-zero array dimension
!   def nchantotal      - max(1,nsatdat_s) - ensure non-zero array dimension
!   def npcpdat_dim_tl  - max(1,npcpdat_s) - ensure non-zero array dimension
!   def nspddat_dim_tl  - max(1,nspddata) - ensure non-zero array dimension
!   def ndwdat_dim_tl   - max(1,ndwdata) - ensure non-zero array dimension
!   def nrwdat_dim_tl   - max(1,nrwdata) - ensure non-zero array dimension
!   def ngpsdat_dim_tl  - max(1,ngpsdata) - ensure non-zero array dimension
!   def nsstdat_dim_tl  - max(1,nsstdata) - ensure non-zero array dimension
!   def tdataerr_tl     - tangent linear of temperature error*residual
!   def ures_tl         - tangent linear of u residual*errors for conventional data
!   def vres_tl         - tangent linear of v residual*errors for conventional data
!   def srw1res_tl      - tangent linear of super wind 1 residual*errors for conventional data
!   def srw2res_tl      - tangent linear of super wind 2 residual*errors for conventional data
!   def spdres_tl       - tangent linear of wind speed residual*errors for non-conv. data
!   def uges_tl         - tangent linear of u guess at observation location 
!   def vges_tl         - tangent linear of v guess at observation location
!   def srw1ges_tl      - tangent linear of super wind 1 guess at observation location
!   def srw2ges_tl      - tangent linear of super wind 2 guess at observation location
!   def uverr_tl        - tangent linear of wind error*residual
!   def srwerr_tl       - tangent linear of super wind error*residual
!   def spderr_tl       - tangent linear of wind speed error*residual
!   def presier_tl      - tangent linear of surface pressure error*residual
!   def qdataerr_tl     - tangent linear of moisture error*residual
!   def pwdataerr_tl    - tangent linear of precipitable water error*residual
!   def gpsdataerr_tl   - tangent linear of gps error*residual
!   def sstdataerr_tl   - tangent linear of sst error*residual
!   def pcpobs_tl       - tangent linear of observed value for precipitation
!   def pcpges_tl      - tangent linear of guess value at obs location for precipitation
!   def usges_tl        - tangent linear of u guess at obs location for wind speed obs
!   def vsges_tl        - tangent linear of v guess at obs location for wind speed obs
!   def rad_inv_tl      - tangent linear of residuals for satellite radiances
!   def rdw_tl          - tangent linear of forward model for dopper wind at obs location
!   def rrw_tl          - tangent linear of forward model for radial wind at obs location
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: zero

  real(r_kind),allocatable,dimension(:)::  &
       tdataerr_tl,ures_tl,vres_tl,uges_tl,vges_tl,presier_tl, &
       qdataerr_tl,pwdataerr_tl,pcpobs_tl,pcpges_tl,gpsdataerr_tl, &
       sstdataerr_tl,srw1res_tl,srw2res_tl,rdw_tl,rrw_tl
  real(r_kind),allocatable,dimension(:):: spdres_tl,usges_tl,vsges_tl,rad_inv_tl
  real(r_kind),allocatable,dimension(:):: ozo_inv_tl
  real(r_kind),allocatable,dimension(:,:):: oz_inv_tl
  integer(i_kind) ntdata_dim_tl,nwdata_dim_tl,nsrwdata_dim_tl,npdata_dim_tl,nqdata_dim_tl, &
       npwdat_dim_tl,nozdata_dim_tl,nchantotal,npcpdat_dim_tl,nspddat_dim_tl,&
       ndwdat_dim_tl,nrwdat_dim_tl,ngpsdat_dim_tl,nsstdat_dim_tl,nozodata_dim_tl

contains

  subroutine init_obsmod_dflts_tl
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_obsmod_dflts_tl
!     prgmmr:    yanqiu zhu            org: GMAO           date: 2005-04-07
!
! abstract:  set defaults for observation related tangent linear variables
!
! program history log:
!   2005-04-07  yanqiu zhu - tangent linear of init_obsmod_dflts
!   2005-06-14  wu -  add OMI toz data
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

    ntdata_dim_tl  = 0
    nwdata_dim_tl  = 0
    nsrwdata_dim_tl= 0
    npdata_dim_tl  = 0
    nqdata_dim_tl  = 0
    npwdat_dim_tl  = 0
    nozdata_dim_tl = 0
    nozodata_dim_tl = 0
    nchantotal = 0
    npcpdat_dim_tl = 0
    ndwdat_dim_tl  = 0
    nrwdat_dim_tl  = 0
    ngpsdat_dim_tl = 0


    return
  end subroutine init_obsmod_dflts_tl
  
  
  subroutine makecobs_tl(no_allocate)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    makecobs_tl
!     prgmmr:    yanqiu zhu            org: GMAO           date: 2005-04-07
!
! abstract:  allocate tangent linear arrays to hold conventional observation 
!            information used in inner and outer loops
!
! program history log:
!   2005-04-07  yanqiu zhu - tangent linear of makecobs
!   2005-06-14  wu -  add OMI toz data
!
!   input argument list:
!     no_allocate - skip array allocation if .true.
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use obsmod, only: nloz
    use obsmod, only: nchan_total
    use obsmod, only: pshead, radhead, pcphead, thead, whead, qhead, spdhead, &
                      srwhead, rwhead, dwhead, ssthead, pwhead, ozhead, & 
                      gpshead, ozohead
    use obsmod, only: psptr , radptr,  pcpptr,  tptr, wptr , qptr , spdptr , &
                      srwptr , rwptr , dwptr , sstptr, pwptr, ozptr, gpsptr, &
                      ozoptr
    implicit none
    logical no_allocate

    radptr=>radhead
    do while(associated(radptr))
      nchantotal=nchantotal+radptr%nchan
      radptr=>radptr%llpoint
    end do

    pcpptr=>pcphead
    do while(associated(pcpptr))
      npcpdat_dim_tl=npcpdat_dim_tl+1
      pcpptr=>pcpptr%llpoint
    end do
  
    psptr=>pshead
    do while(associated(psptr))
      npdata_dim_tl=npdata_dim_tl+1
      psptr=>psptr%llpoint
    end do
  
    tptr=>thead
    do while(associated(tptr))
      ntdata_dim_tl=ntdata_dim_tl+1
      tptr=>tptr%llpoint
    end do
  
    wptr=>whead
    do while(associated(wptr))
      nwdata_dim_tl=nwdata_dim_tl+1
      wptr=>wptr%llpoint
    end do
  
    qptr=>qhead
    do while(associated(qptr))
      nqdata_dim_tl=nqdata_dim_tl+1
      qptr=>qptr%llpoint
    end do
  
    spdptr=>spdhead
    do while(associated(spdptr))
      nspddat_dim_tl=nspddat_dim_tl+1
      spdptr=>spdptr%llpoint
    end do
  
    srwptr=>srwhead
    do while(associated(srwptr))
      nsrwdata_dim_tl=nsrwdata_dim_tl+1
      srwptr=>srwptr%llpoint
    end do
  
    rwptr=>rwhead
    do while(associated(rwptr))
      nrwdat_dim_tl=nrwdat_dim_tl+1
      rwptr=>rwptr%llpoint
    end do
  
    dwptr=>dwhead
    do while(associated(dwptr))
      ndwdat_dim_tl=ndwdat_dim_tl+1
      dwptr=>dwptr%llpoint
    end do
  
    sstptr=>ssthead
    do while(associated(sstptr))
      nsstdat_dim_tl=nsstdat_dim_tl+1
      sstptr=>sstptr%llpoint
    end do

    ozptr=>ozhead
    do while(associated(ozptr))
      nozdata_dim_tl=nozdata_dim_tl+1
      ozptr=>ozptr%llpoint
    end do

    ozoptr=>ozohead
    do while(associated(ozoptr))
      nozodata_dim_tl=nozodata_dim_tl+1
      ozoptr=>ozoptr%llpoint
    end do

    gpsptr=>gpshead
    do while(associated(gpsptr))
      ngpsdat_dim_tl=ngpsdat_dim_tl+1
      gpsptr=>gpsptr%llpoint
    end do

    pwptr=>pwhead
    do while(associated(pwptr))
      npwdat_dim_tl=npwdat_dim_tl+1
      pwptr=>pwptr%llpoint
    end do
  
    if (no_allocate) return
        
    if(nchan_total > 0)then
        allocate(rad_inv_tl(nchan_total))
        rad_inv_tl = zero
    end if
    
    if(npcpdat_dim_tl > 0)then
        allocate(pcpges_tl(npcpdat_dim_tl),pcpobs_tl(npcpdat_dim_tl))
        pcpges_tl = zero
        pcpobs_tl = zero
    end if

    if(ntdata_dim_tl > 0)then
       allocate(tdataerr_tl(ntdata_dim_tl))
       tdataerr_tl = zero
    end if

!
    if(nwdata_dim_tl > 0)then
       allocate(ures_tl(nwdata_dim_tl),vres_tl(nwdata_dim_tl), &
             uges_tl(nwdata_dim_tl),vges_tl(nwdata_dim_tl))
       ures_tl = zero
       vres_tl = zero
       uges_tl = zero
       vges_tl = zero
    end if

    if(nsrwdata_dim_tl > 0)then
       allocate(srw1res_tl(nsrwdata_dim_tl),srw2res_tl(nsrwdata_dim_tl))
       srw1res_tl = zero
       srw2res_tl = zero
    end if

    if(nspddat_dim_tl > 0)then
       allocate(spdres_tl(nspddat_dim_tl),usges_tl(nspddat_dim_tl),vsges_tl(nspddat_dim_tl))
       spdres_tl = zero
       usges_tl = zero
       vsges_tl = zero
    end if
 
    if(npdata_dim_tl > 0)then
       allocate(presier_tl(npdata_dim_tl))
       presier_tl = zero 
    end if

    if(nqdata_dim_tl > 0)then
       allocate(qdataerr_tl(nqdata_dim_tl))
       qdataerr_tl = zero
    end if
    
    if(npwdat_dim_tl > 0)then
       allocate(pwdataerr_tl(npwdat_dim_tl))
       pwdataerr_tl = zero
    end if

    if(nozdata_dim_tl > 0)then
       allocate(oz_inv_tl(nloz+1,nozdata_dim_tl))
       oz_inv_tl = zero
    end if

    if(nozodata_dim_tl > 0)then
       allocate(ozo_inv_tl(nozodata_dim_tl))
       ozo_inv_tl = zero
    end if

    if(ndwdat_dim_tl > 0)then
       allocate(rdw_tl(ndwdat_dim_tl))
       rdw_tl = zero
    end if

    if(nrwdat_dim_tl > 0)then
       allocate(rrw_tl(nrwdat_dim_tl))
       rrw_tl = zero
    end if

    if(ngpsdat_dim_tl > 0)then
       allocate(gpsdataerr_tl(ngpsdat_dim_tl))
       gpsdataerr_tl = zero
    end if

    if(nsstdat_dim_tl > 0)then
       allocate(sstdataerr_tl(nsstdat_dim_tl))
       sstdataerr_tl = zero
    end if

    return
  end subroutine makecobs_tl
  
  subroutine destroyobs_tl
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroyobs_tl
!     prgmmr:    yanqiu zhu            org: GMAO           date: 2005-04-07
!
! abstract:  deallocate tangent linear arrays that hold observation information 
!            for use in outer and inner loops
!
! program history log:
!   2005-04-07  yanqiu zhu - tangent linear of destroyobs
!   2005-06-14  wu -  add OMI toz data
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

    if(ntdata_dim_tl > 0)deallocate(tdataerr_tl)

    if(nwdata_dim_tl > 0)deallocate(ures_tl,vres_tl,uges_tl,vges_tl)

    if(nsrwdata_dim_tl > 0)deallocate(srw1res_tl,srw2res_tl)

    if(nspddat_dim_tl > 0)deallocate(spdres_tl,usges_tl,vsges_tl)

    if(nqdata_dim_tl > 0)deallocate(qdataerr_tl)

    if(npwdat_dim_tl > 0)deallocate(pwdataerr_tl)

    if(npdata_dim_tl > 0)deallocate(presier_tl)

    if(nozdata_dim_tl > 0)deallocate(oz_inv_tl)

    if(nozodata_dim_tl > 0)deallocate(ozo_inv_tl)

    if(nchantotal > 0)deallocate(rad_inv_tl)

    if(npcpdat_dim_tl > 0)deallocate(pcpges_tl,pcpobs_tl)

    if(ndwdat_dim_tl > 0) deallocate(rdw_tl)

    if(nrwdat_dim_tl > 0) deallocate(rrw_tl)

    if(ngpsdat_dim_tl > 0) deallocate(gpsdataerr_tl)

    if(nsstdat_dim_tl > 0)deallocate(sstdataerr_tl)

    return
  end subroutine destroyobs_tl
  
end module obsmod_tl


