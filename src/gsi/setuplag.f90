module lag_setup
  implicit none
  private
  public:: setup
        interface setup; module procedure setuplag; end interface

contains
subroutine setuplag(obsLL,odiagLL,lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setuplag    compute rhs of oi for lagrangian data    
!   prgmmr: lmeunier         org:                     date: 2009-03-12
!
!
! abstract:  
!
! program history log:
!   2009-03-12  lmeunier
!   2010-07-14  todling - use die to abort
!   2011-08-01  lueken  - replaced F90 with f90 (no machine logic) and removed double &
!   2014-01-28  todling - write sensitivity slot indicator (ioff) to header of diagfile
!   2014-12-30  derber - Modify for possibility of not using obsdiag
!   2015-10-01  guo   - full res obvsr: index to allow redistribution of obsdiags
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!   2016-06-24  guo     - fixed the default value of obsdiags(:,:)%tail%luse to luse(i)
!                       . removed (%dlat,%dlon) debris.
!   2017-02-09  guo     - Remove m_alloc, n_alloc.
!                       . Remove my_node with corrected typecast().
!
!   input argument list:
!     lunin    - unit from which to read observations
!     mype     - mpi task id
!     nele     - number of data elements per observation
!     nobs     - number of observations
!
!   output argument list:
!     bwork    - array containing information about obs-ges statistics
!     awork    - array containing information for data counts and gross checks
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use mpeu_util, only: die,perr
  use kinds, only: r_kind,r_single,r_double,i_kind
  use m_obsdiagNode, only: obs_diag
  use m_obsdiagNode, only: obs_diags
  use m_obsdiagNode, only: obsdiagLList_nextNode
  use m_obsdiagNode, only: obsdiagNode_set
  use m_obsdiagNode, only: obsdiagNode_get
  use m_obsdiagNode, only: obsdiagNode_assert

  use obsmod, only: &
      lobsdiagsave,nobskeep,lobsdiag_allocated,&
      time_offset
  use m_obsNode, only: obsNode
  use m_lagNode, only: lagNode
  use m_lagNode, only: lagNode_appendto
  use m_obsLList,only: obsLLIst
  use obsmod, only: luse_obsdiag

  use nc_diag_write_mod, only: nc_diag_init, nc_diag_header, nc_diag_metadata, &
       nc_diag_write, nc_diag_data2d
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_get_dim, nc_diag_read_close
  use gsi_4dvar, only: nobs_bins,hr_obsbin,l4dvar
  use guess_grids, only: nfldsig,hrdifsig
  use gridmod, only: nsig
  use qcmod, only: npres_print,ptop,pbot
  use constants, only: wgtlim,&
      zero,two,one,deg2rad,r3600,&
      tiny_r_kind,half,cg_term,huge_single,rearth,pi,rad2deg
  use jfunc, only: jiter,last,miter
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  use convinfo, only: icsubtype,icuse

  use m_dtime,only: dtime_setup,dtime_check

  use lag_fields, only: orig_lag_num,lag_kfirst
  use lag_fields, only: lag_nl_vec,lag_u_full,lag_v_full
  use lag_fields, only: lag_vorcore_stderr_b,lag_vorcore_stderr_a
  use lag_traj, only: lag_rk2itenpara_i,lag_rk2itenpara_r,lag_rk2iter_nl
  ! use lag_traj, only: lag_rk4itenpara_i,lag_rk4itenpara_r,lag_rk4iter_nl
  use lag_traj, only: lag_trajfail
  use lag_traj, only: lag_d_haversin
  use lag_interp, only: lag_gridrel_ijk
  implicit none

! Declare passed variables
  type(obsLList ),target,dimension(:),intent(in):: obsLL
  type(obs_diags),target,dimension(:),intent(in):: odiagLL

  logical                                          ,intent(in   ) :: conv_diagsave
  integer(i_kind)                                  ,intent(in   ) :: lunin,mype,nele,nobs
  real(r_kind),dimension(7*nsig+100)               ,intent(inout) :: awork
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork
  integer(i_kind)                                  ,intent(in   ) :: is ! ndat index

! Declare local parameters

  integer(i_kind),parameter:: iv_debug = 1

  character(len=*),parameter:: myname='setuplag'

! Declare local variables
  real(r_double):: rstation_id
  real(r_kind):: scale,ratio_lon,ratio_lat,error_lon,error_lat
  real(r_kind):: obserror_lon,obserrlm_lon,obserror_lat,obserrlm_lat
  real(r_kind):: reslon,reslat,val,val1,val2,ressw,ress,error,valqc
  real(r_kind):: ratio_errors
  real(r_kind):: cg_srw,wgross,wnotgross,wgt,arg,exp_arg,rwgt,term,rat_err2
  real(r_kind):: errinv_input,errinv_final_lon,errinv_final_lat
  real(r_kind):: err_input,err_final_lon,err_final_lat
  real(r_single),allocatable,dimension(:,:):: rdiagbuf
  real(r_kind),dimension(nele,nobs):: data
  real(r_kind):: dlat,dlon,dpres,dtime,dpresR
  real(r_kind):: lonfcst,latfcst,pfcst,hsteptime
  real(r_kind):: rmute,rsig
  real(r_kind),allocatable,dimension(:):: tlspecr

  integer(i_kind):: jsig,ibin,ioff,ioff0
  integer(i_kind):: i,nchar,nreal,k,ii,jj,istat,nn
  integer(i_kind):: inum,itime,ilon,ilat,ilone,ilate,ipress,ikxx,ier
  integer(i_kind):: dnum,ikx,laglocnum,mm1
  integer(i_kind),allocatable,dimension(:):: tlspeci

  real(r_kind)::fieldtime
  integer(i_kind)::fieldindex

  character(8),allocatable,dimension(:):: cdiagbuf

  logical,dimension(nobs):: luse,muse
  integer(i_kind),dimension(nobs):: ioid ! initial (pre-distribution) obs ID
  logical:: in_curbin, in_anybin
  type(lagNode),pointer :: my_head
  type(obs_diag),pointer :: my_diag
  type(obs_diag),pointer :: my_diagLon,my_diagLat
  type(obs_diags),pointer :: my_diagLL
  type(obsLList),pointer,dimension(:):: laghead
  laghead => obsLL(:)

  call die('setuplag','I don''t believe this code is working -- J.Guo')
  ! Problems include, data(ilone) and data(ilate) are expected to be in degrees
  ! here, according to the code comment.  However, they were set to in radians
  ! in read_lag().  In particular, they should have been set to in degrees to
  ! be correctly located on the grid.

!******************************************************************************
! Read and reformat observations in work arrays.
  read(lunin)data,luse,ioid

!    index information for data array (see reading routine)

  inum=1      ! index of the balloon number in array
  itime=2     ! index of observation time (sec)
  ilon=3      ! index of grid relative obs location (x)
  ilat=4      ! index of grid relative obs location (y)
  ilone=5     ! index of longitude (degrees)
  ilate=6     ! index of latitude (degrees)
  ipress=7    ! index of pressure (hPa)
  ikxx=8      ! index of ob type
  ier=9       ! index of error

! If requested, save select data for output to diagnostic file
  if(conv_diagsave)then
     ii=0
     nchar=1
     ioff0=17
     nreal=ioff0
     if (lobsdiagsave) nreal=nreal+7*miter+2
     allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
  end if
  scale=one
  rsig=real(nsig,r_kind)
  mm1=mype+1

  call dtime_setup()
  do i=1,nobs

     dtime=data(itime,i)
     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle

     if(in_curbin) then
        dnum=int(data(inum,i),i_kind)
        dlat=data(ilate,i)
        dlon=data(ilone,i)
        dpres=data(ipress,i)

        ikx=int(data(ikxx,i),i_kind)
        error=data(ier,i)

        ! Ini muse
        muse(i)=luse(i).and.(icuse(ikx)>0)
     endif

!    Link observation to appropriate observation bin
     if (nobs_bins>1) then
        ibin = int( dtime/hr_obsbin ) + 1
      ! ibin = NINT( dtime/hr_obsbin ) + 1
     else
        ibin = 1
     endif
     if (ibin<1.OR.ibin>nobs_bins) &
        write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

     if (iv_debug>=2) then
        print '(A,I2.2,A,I4.4,A,I4.4,A)'  ,'mype ',mype,' data ',i,' on ',nobs,' read'
        print '(A,I2.2,A,I4.4,A,I4)'      ,'mype ',mype,' data ',i,' dnum ',dnum
        print '(A,I2.2,A,I4.4,A,F12.6,F12.6,F12.6)','mype ',mype,' data ',i,&
           ' lon/lat/pres ',dlon,dlat,dpres
        print '(A,I2.2,A,I4.4,A,F12.6)'   ,'mype ',mype,' data ',i,' time ',dtime
        print '(A,I2.2,A,I4.4,A,I4,F12.6)','mype ',mype,' data ',i,' ikx,error ',ikx,error
        print '(A,I2.2,A,I4.4,A,I4)'      ,'mype ',mype,' data ',i,' obsbin ',ibin
     end if

     if (luse_obsdiag) my_diagLL => odiagLL(ibin)

!    Link obs to diagnostics structure
     if (luse_obsdiag) then
        do jj=1,2
          my_diag => obsdiagLList_nextNode(my_diagLL    ,&
                create = .not.lobsdiag_allocated        ,&
                   idv = is             ,&
                   iob = ioid(i)        ,&
                   ich = jj             ,&
                  elat = data(ilate,i)  ,&
                  elon = data(ilone,i)  ,&
                  luse = luse(i)        ,&
                 miter = miter          )

          if(.not.associated(my_diag)) then
            call perr(myname,'obsdiagLList_nextNode(), create =', .not.lobsdiag_allocated)
            call perr(myname,'                            ich =', jj)
            call  die(myname)
          endif

          select case(jj)
          case(1); my_diagLon => my_diag
          case(2); my_diagLat => my_diag
          end select
          my_diag => null()
        end do
     end if

!--------
! Skip this o-g calculation, if there is no ges field to use.

     if(.not. in_curbin) cycle

!    Pressure grid relative 
     call lag_gridrel_ijk(dlon,dlat,dpres,rmute,rmute,dpresR)
     dpresR=lag_kfirst-1+dpresR

!    Local number for this balloon
     laglocnum=orig_lag_num(dnum,3)

!    Allocation of TL parameters arrays
     if (.not.allocated(tlspeci)) allocate(tlspeci(lag_rk2itenpara_i))
     if (.not.allocated(tlspecr)) allocate(tlspecr(lag_rk2itenpara_r))

!    4d var : computation done using the obsbins
     if (l4dvar) then

        fieldtime=(ibin-1)*hr_obsbin
        if(fieldtime>=hrdifsig(1) .and. fieldtime<=hrdifsig(nfldsig)) then
         ! Which guess field to use ?
           do k=1,nfldsig-1
              if(fieldtime >= hrdifsig(k) .and. fieldtime < hrdifsig(k+1)) then
                 fieldindex=k
              end if
           end do
        else
           call die('setuplag: Inapropriate velocity guess fields')
        end if
 
        hsteptime = (dtime - (ibin-1)*hr_obsbin)* r3600
 
        lonfcst=lag_nl_vec(laglocnum,ibin,1)
        latfcst=lag_nl_vec(laglocnum,ibin,2)
        pfcst  =lag_nl_vec(laglocnum,ibin,3)
 
!    3d var : we use the previous wind field availlable within the unique obsbin
     else

        fieldtime=dtime
        if(fieldtime>=hrdifsig(1) .and. fieldtime<=hrdifsig(nfldsig)) then
         ! Which guess field to use ?
           do k=1,nfldsig-1
              if(fieldtime >= hrdifsig(k) .and. fieldtime < hrdifsig(k+1)) then
                 fieldindex=k
              end if
           end do
        else
           call die('setuplag: Inapropriate velocity guess fields')
        end if
 
        hsteptime = (dtime - hrdifsig(fieldindex))* r3600

        lonfcst=lag_nl_vec(laglocnum,fieldindex,1)
        latfcst=lag_nl_vec(laglocnum,fieldindex,2)
        pfcst  =lag_nl_vec(laglocnum,fieldindex,3)
 
     end if

     call lag_rk2iter_nl(lonfcst,latfcst,pfcst,&
        lag_u_full(:,:,fieldindex),lag_v_full(:,:,fieldindex),&
        hsteptime,tlspeci,tlspecr)

!    Calculate the residuals (distance between observation and guess)
     if (lonfcst==lag_trajfail .or. latfcst==lag_trajfail) then
        reslon=lag_trajfail; reslat=lag_trajfail
     else
        reslon=dlon-lonfcst
        if (reslon>pi  ) reslon=reslon-2*pi
        if (reslon<=-pi) reslon=reslon+2*pi
        reslat=dlat-latfcst
     end if
 

!    Increment obs counter
     if(luse(i))then
        awork(1)=awork(1)+one
     end if

! Adjust observation error.
     ! If the error wasn't read
     if (error==zero) then
        error=lag_vorcore_stderr_b  ! use the standard error
        if (l4dvar) then
           error=error + dtime *lag_vorcore_stderr_a  ! adjust it by the time step
        else
           error=error + hsteptime/r3600 *lag_vorcore_stderr_a
        end if
     end if
     error_lon=error/(rearth*cos(dlat))
     error_lat=error/rearth
     ratio_errors = one
     error_lon = one/error_lon
     error_lat = one/error_lat
 
     if (iv_debug>=1) then
        print '(A,I2.2,A,I4.4,A,I4)','mype ',mype,' data ',i,' guess used',fieldindex
        print '(A,I2.2,A,I4.4,A,F12.2)','mype ',mype,' data ',i,' timestep ',hsteptime
        print '(A,I2.2,A,I4.4,A,F12.6)','mype ',mype,' data ',i,' obs lon  ',dlon
        print '(A,I2.2,A,I4.4,A,F12.6)','mype ',mype,' data ',i,' obs lat  ',dlat
        print '(A,I2.2,A,I4.4,A,F12.6)','mype ',mype,' data ',i,' res lon  ',reslon
        print '(A,I2.2,A,I4.4,A,F12.6)','mype ',mype,' data ',i,' res lat  ',reslat
        print '(A,I2.2,A,I4.4,A,F12.6,F12.6)','mype ',mype,' data ',i,' errors lon/lat',one/error_lon,one/error_lat
     end if
     if (iv_debug>=2) then
        print '(A,I2.2,A,I4.4,A,F12.6)','mype ',mype,' data ',i,' guess lon',lonfcst
        print '(A,I2.2,A,I4.4,A,F12.6)','mype ',mype,' data ',i,' guess lat',latfcst
     end if
 
     ! Gross error checks
     obserror_lon=one/max(ratio_errors*error_lon,tiny_r_kind)
     obserrlm_lon=max(&
        cermin(ikx)*1e3_r_kind/(rearth*cos(dlat)),&
        min(cermax(ikx)*1e3_r_kind/(rearth*cos(dlat)),obserror_lon))
     obserror_lat=one/max(ratio_errors*error_lat,tiny_r_kind)
     obserrlm_lat=max(&
        cermin(ikx)*1e3_r_kind/rearth,&
        min(cermax(ikx)*1e3_r_kind/rearth,obserror_lat))
     ratio_lon=abs(reslon/obserrlm_lon)
     ratio_lat=abs(reslat/obserrlm_lat)
     if ((ratio_lon > cgross(ikx) .or. ratio_errors < tiny_r_kind) .or. &
         (ratio_lat > cgross(ikx) .or. ratio_errors < tiny_r_kind)) error=zero

     ! If the trajectory model fail don't use
     if (lonfcst==lag_trajfail .or. latfcst==lag_trajfail) error=zero
 
     ! If the obs and ges are in oposition to the pole, don't use
     if (abs(reslon)>=160_r_kind*deg2rad) error=zero
 
     ! If not used increment counter and 0 other variables
     if (error==zero) then
        if(luse(i)) awork(4)=awork(4)+one
        error_lon=zero
        error_lat=zero
        ratio_errors=zero
     end if
     
     if ((ratio_errors*error_lat <= tiny_r_kind) .or. &
         (ratio_errors*error_lon <= tiny_r_kind)) muse(i)=.false.
     if (nobskeep>0.and.luse_obsdiag) call obsdiagNode_get(my_diagLat, jiter=nobskeep, muse=muse(i))
 
     if (iv_debug>=1) then
        print '(A,I2.2,A,I4.4,A,F12.6,F12.6)','mype ',mype,' data ',i,' ratios ',ratio_lon,ratio_lat
        print '(A,I2.2,A,I4.4,A,L7)','mype ',mype,' data ',i,' muse ' ,muse(i)
     end if
 
!    Compute penalty terms
     val1 = reslon*error_lon
     val2 = reslat*error_lat
     if(luse(i))then
        val      = val1*val1 + val2*val2
        exp_arg  = -half*val
        rat_err2 = ratio_errors**2
        if (cvar_pg(ikx) > tiny_r_kind .and. error > tiny_r_kind) then
           arg  = exp(exp_arg)
           wnotgross= one-cvar_pg(ikx)
           cg_srw=cvar_b(ikx)
           wgross = cg_term*cvar_pg(ikx)/(cg_srw*wnotgross)
           term = log((arg+wgross)/(one+wgross))
           wgt  = one-wgross/(arg+wgross)
           rwgt = wgt/wgtlim
        else
           term = exp_arg
           wgt  = wgtlim
           rwgt = wgt/wgtlim
        endif
        valqc = -two*rat_err2*term
 
!       Accumulate statistics for obs belonging to this task
        if(muse(i))then
           if(rwgt < one) awork(21) = awork(21)+one
           jsig=int(dpresR,i_kind)
           jsig=max(1,min(jsig,nsig))
           awork(4*nsig+jsig+100)=awork(4*nsig+jsig+100)+val1*val1*rat_err2
           awork(5*nsig+jsig+100)=awork(5*nsig+jsig+100)+val2*val2*rat_err2
           awork(6*nsig+jsig+100)=awork(6*nsig+jsig+100)+one
           awork(3*nsig+jsig+100)=awork(3*nsig+jsig+100)+valqc
        endif

        ress = scale*lag_d_haversin(dlon,dlat,lonfcst,latfcst)*1e-3_r_kind
        ressw= ress*ress
        nn=1
        if (.not. muse(i)) then
           nn=2
           if (ratio_errors*error_lon >=tiny_r_kind .or. &
               ratio_errors*error_lat >=tiny_r_kind) nn=3
        end if
        do k = 1,npres_print
           if(dpres > ptop(k) .and. dpres <= pbot(k))then
              bwork(k,ikx,1,nn) = bwork(k,ikx,1,nn)+one          !count
              bwork(k,ikx,2,nn) = bwork(k,ikx,2,nn)+ress         !(o-g)     (in km)
              bwork(k,ikx,3,nn) = bwork(k,ikx,3,nn)+ressw        !(o-g)**2  (in km^2)
              bwork(k,ikx,4,nn) = bwork(k,ikx,4,nn)+val*rat_err2 !penalty
              bwork(k,ikx,5,nn) = bwork(k,ikx,5,nn)+valqc        !nonlin qc penalty
           end if
        end do
 
     endif

     if (luse_obsdiag) then
       ! lon
        call obsdiagNode_set(my_diagLon,wgtjo=(error_lon*ratio_errors)**2, &
                jiter=jiter,muse=muse(i),nldepart=reslon)
       ! lat
        call obsdiagNode_set(my_diagLat,wgtjo=(error_lat*ratio_errors)**2, &
                jiter=jiter,muse=muse(i),nldepart=reslat)
     endif
 
     if (iv_debug>=1) then
        print '(A,I2.2,A,I4.4,A,F12.6)','mype ',mype,' data ',i,' jo lon ',&
           reslon*reslon* (error_lon*ratio_errors)**2
        print '(A,I2.2,A,I4.4,A,F12.6)','mype ',mype,' data ',i,' jo lat ',&
           reslat*reslat* (error_lat*ratio_errors)**2
     end if

!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
     if (.not. last .and. muse(i)) then
 
        allocate(my_head)
        call lagNode_appendto(my_head,laghead(ibin))

        my_head%idv = is
        my_head%iob = ioid(i)
        my_head%elat= data(ilate,i)
        my_head%elon= data(ilone,i)

        allocate(my_head%speci(lag_rk2itenpara_i),stat=istat)
        if(istat /= 0)write(6,*)' failure to allocate lagtail%speci '
        allocate(my_head%specr(lag_rk2itenpara_r),stat=istat)
        if(istat /= 0)write(6,*)' failure to allocate lagtail%specr '
 
        my_head%res_lon=reslon
        my_head%res_lat=reslat
        my_head%err2_lon=error_lon**2
        my_head%err2_lat=error_lat**2
        my_head%raterr2=ratio_errors**2    
        my_head%obslon=dlon
        my_head%obslat=dlat
        my_head%geslon=lonfcst
        my_head%geslat=latfcst
        my_head%intnum=dnum
        my_head%speci=tlspeci
        my_head%specr=tlspecr
        my_head%time=dtime
        my_head%b=cvar_b(ikx)
        my_head%pg=cvar_pg(ikx)
        my_head%luse=luse(i)
        my_head%diag_lon => null()
        my_head%diag_lat => null()

        if (luse_obsdiag) then
           call obsdiagNode_assert(my_diagLon, my_head%idv,my_head%iob,1,myname,'my_diagLon:my_head')
           call obsdiagNode_assert(my_diagLat, my_head%idv,my_head%iob,2,myname,'my_diagLat:my_head')

           my_head%diag_lon => my_diagLon
           my_head%diag_lat => my_diagLat
        endif

        my_head => null()
     end if

! Save select output for diagnostic file
     if(conv_diagsave)then
        ii=ii+1
        rstation_id = orig_lag_num(dnum,1)
        err_input   = data(ier,i)
        if (ratio_errors*error_lon>tiny_r_kind .and. ratio_errors*error_lat>tiny_r_kind) then
           err_final_lon = one/(ratio_errors*error_lon)*rad2deg
           err_final_lat = one/(ratio_errors*error_lat)*rad2deg
        else
           err_final_lon = huge_single
           err_final_lat = huge_single
        endif
        errinv_input = huge_single
        errinv_final_lon = huge_single
        errinv_final_lat = huge_single
        if (err_input>tiny_r_kind) errinv_input=one/err_input
        if (err_final_lon>tiny_r_kind) errinv_final_lon=one/err_final_lon
        if (err_final_lat>tiny_r_kind) errinv_final_lat=one/err_final_lat
 
        write(cdiagbuf(ii),fmt='(I5.5)') int(rstation_id)
 
        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype
   
        rdiagbuf(3,ii) = rstation_id         ! number of the sensor/balloon
        rdiagbuf(4,ii)  = dlon*rad2deg       ! observation longitude (degrees)
        rdiagbuf(5,ii)  = dlat*rad2deg       ! observation latitude (degrees)
        rdiagbuf(6,ii)  = dpres              ! observation pressure (hPa)
        rdiagbuf(7,ii)  = dtime-time_offset  ! obs time (hours relative to analysis time)
 
        if(muse(i)) then
           rdiagbuf(8,ii) = one             ! analysis usage flag (1=use, -1=not used)
        else
           rdiagbuf(8,ii) = -one
        endif

        rdiagbuf(9,ii) = rwgt               ! nonlinear qc relative weight
        rdiagbuf(10,ii)= errinv_input       ! prepbufr inverse obs error
        rdiagbuf(11,ii)= errinv_final_lon   ! final inverse observation error
        rdiagbuf(12,ii)= errinv_final_lat   ! final inverse observation error

        rdiagbuf(13,ii) = lonfcst*rad2deg    ! ges lon
        rdiagbuf(14,ii) = latfcst*rad2deg    ! ges lat
        rdiagbuf(15,ii) = ress                          ! obs-ges in distance (m)
        rdiagbuf(16,ii) = reslon*rad2deg                ! omf for longitude (m)
        rdiagbuf(17,ii) = reslat*rad2deg                ! omf for lattitude (m)

        ioff=ioff0
        if (lobsdiagsave) then
          associate(odiag => my_diagLat)
                ! Logic here seems to be only for one of two diag components,
                ! according to its original implementation, for my_diagLat only.
                ! Is it the original intention, or just a bug?
           do jj=1,miter
              ioff=ioff+1
              if (odiag%muse(jj)) then
                 rdiagbuf(ioff,ii) = one
              else
                 rdiagbuf(ioff,ii) = -one
              endif
           enddo
           do jj=1,miter+1
              ioff=ioff+1
              rdiagbuf(ioff,ii) = odiag%nldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = odiag%tldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = odiag%obssen(jj)
           enddo
          end associate ! odiag
        endif

     end if
 
  end do

! Write information to diagnostic file
  if(conv_diagsave .and. ii>0)then
     write(7)'lag',nchar,nreal,ii,mype,ioff0
     write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
     deallocate(cdiagbuf,rdiagbuf)
  end if

! End of routine
contains
  subroutine init_netcdf_diag_
  end subroutine init_netcdf_diag_
  subroutine contents_binary_diag_
  end subroutine contents_binary_diag_
  subroutine contents_netcdf_diag_
! Observation class
  character(7),parameter     :: obsclass = '    lag'
  end subroutine contents_netcdf_diag_
end subroutine setuplag
end module lag_setup
