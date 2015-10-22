module radiance_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:    radiance_mod
!
!   prgrmmr:    yanqiu zhu      org: np23                date: 2015-07-20
!
! abstract:  This module contains variables and routines related
!            to cloud and aerosol usages for radiance assimilation
!
! program history log:
!   2015-07-20 Yanqiu Zhu
!
! subroutines included:
!   sub radiance_mode_init           -  guess init
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

! !USES:

  use kinds, only: r_kind,i_kind
  use constants, only: zero,half
  use mpimod, only: mype
  implicit none
  save

! set subroutines to public
  public :: radiance_mode_init
  public :: radiance_mode_destroy
  public :: radiance_obstype_init
  public :: radiance_obstype_search
  public :: radiance_obstype_destroy
  public :: radiance_parameter_cloudy_init
  public :: radiance_parameter_aero_init
  public :: radiance_ex_obserr
  public :: radiance_ex_biascor

  public :: icloud_forward,icloud_cv,iallsky,cw_cv
  public :: n_actual_clouds,n_clouds_for,n_clouds_jac
  public :: cloud_names,cloud_names_jac,cloud_names_for
  public :: idx_cw,idx_ql,idx_qi,idx_qr,idx_qs,idx_qg,idx_qh

  public :: iaerosol_forward,iaerosol_cv,iaerosol
  public :: n_actual_aerosols,n_aerosols_for,n_aerosols_jac
  public :: aero_names,aero_names_for,aero_names_jac

  public :: total_rad_type
  public :: rad_type_info
  public :: cloudy_amsua

  public :: rad_obs_type
  public :: amsua_type

  character(len=20),save,allocatable,dimension(:) :: cloud_names
  character(len=20),save,allocatable,dimension(:) :: cloud_names_for
  character(len=20),save,allocatable,dimension(:) :: cloud_names_jac
  character(len=20),save,allocatable,dimension(:) :: aero_names
  character(len=20),save,allocatable,dimension(:) :: aero_names_for
  character(len=20),save,allocatable,dimension(:) :: aero_names_jac
  logical :: icloud_forward,icloud_cv,iallsky,cw_cv
  logical :: iaerosol_forward,iaerosol_cv,iaerosol
  integer(i_kind) :: n_actual_clouds,n_clouds_jac,n_clouds_for
  integer(i_kind) :: n_actual_aerosols,n_aerosols_for,n_aerosols_jac
  integer(i_kind) :: idx_cw,idx_ql,idx_qi,idx_qr,idx_qs,idx_qg,idx_qh

  integer(i_kind) :: total_rad_type

  type rad_obs_type
    character(10) :: rtype            ! instrument
    integer(i_kind) :: nchannel       ! total channel number
    logical :: cld_sea_only           ! .true. only perform all-sky over ocean
    logical :: ex_obserr              ! .true. for special obs error assignment
    logical :: ex_biascor             ! .true. for special bias correction
    logical :: cld_effect             ! .true. additional cloud effect quality control
    logical :: lcloud_forward,lallsky
    integer(i_kind),pointer,dimension(:) :: lcloud4crtm=> NULL()    ! -1 clear-sky; 0 forwad operator only; 1 iallsky
    logical :: laerosol_forward,laerosol
    integer(i_kind),pointer,dimension(:) :: laerosol4crtm => NULL() ! -1 no aero used; 0 forwad operator only; 1 iaerosol 
  end type rad_obs_type

  type,EXTENDS(rad_obs_type) :: amsua_type
    real(r_kind),allocatable,dimension(:) :: cclr
    real(r_kind),allocatable,dimension(:) :: ccld
  end type amsua_type

  type(rad_obs_type),save,dimension(:),allocatable :: rad_type_info
  type(amsua_type),save :: cloudy_amsua

contains

  subroutine radiance_mode_init
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    radiance_mode_init
!
!   prgrmmr:    yanqiu zhu      org: np23                date: 2015-07-20
!
! abstract:  This routine sets default values for variables used in
!            the radiance processing routines.
!
! program history log:
!   2015-07-20  zhu     
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

    use kinds, only: i_kind,r_kind
    use gsi_metguess_mod, only: gsi_metguess_get
    use gsi_chemguess_mod, only: gsi_chemguess_get
    use mpeu_util, only: getindex
    use control_vectors, only: cvars3d
    implicit none

    integer(i_kind) icw_av,iql_av,iqi_av,iqtotal,ier
    integer(i_kind) indx_p25,indx_dust1,indx_dust2,ip25_av,idust1_av,idust2_av

!   initialize variables
    icloud_forward=.false.
    icloud_cv=.false.
    iallsky=.false.
    cw_cv=.false.

    n_actual_clouds=0
    n_clouds_for=0 
    n_clouds_jac=0

    iaerosol_forward=.false.
    iaerosol_cv=.false.
    iaerosol=.false.

    n_actual_aerosols=0
    n_aerosols_for=0
    n_aerosols_jac=0

!   inquire number of clouds 
    call gsi_metguess_get ( 'clouds::3d', n_actual_clouds, ier )
    if (n_actual_clouds>0) then
       allocate(cloud_names(n_actual_clouds))
       call gsi_metguess_get ('clouds::3d', cloud_names, ier)
       call gsi_metguess_get ( 'clouds_4crtm_for::3d', n_clouds_for, ier )
       n_clouds_for=max(0,n_clouds_for)
       if (n_clouds_for>0) then
          icloud_forward=.true.
          allocate(cloud_names_for(max(n_clouds_for,1)))
          call gsi_metguess_get ('clouds_4crtm_for::3d', cloud_names_for, ier)

          call gsi_metguess_get ('clouds_4crtm_jac::3d', n_clouds_jac, ier )
          n_clouds_jac=max(0,n_clouds_jac)
          if (n_clouds_jac>0) then
             allocate(cloud_names_jac(max(n_clouds_jac,1)))
             call gsi_metguess_get ('clouds_4crtm_jac::3d', cloud_names_jac, ier)
          end if
       end if

!      inquire number of clouds to participate in CRTM calculations
       call gsi_metguess_get ( 'i4crtm::ql', idx_ql, ier )
       call gsi_metguess_get ( 'i4crtm::qi', idx_qi, ier )
       call gsi_metguess_get ( 'i4crtm::qr', idx_qr, ier )
       call gsi_metguess_get ( 'i4crtm::qs', idx_qs, ier )
       call gsi_metguess_get ( 'i4crtm::qg', idx_qg, ier )
       call gsi_metguess_get ( 'i4crtm::qh', idx_qh, ier )
!      if (idx_ql>10 .or. idx_qi>10 .or. idx_qr>10 .or. idx_qs>10 &
!         .or. idx_qg>10 .or. idx_qh>10) icloud_forward=.true.

!      Determine whether or not cloud-condensate is the control variable
!      (ges_cw=ges_ql+ges_qi)
       icw_av=getindex(cvars3d,'cw')
       iql_av=getindex(cvars3d,'ql')
       iqi_av=getindex(cvars3d,'qi')

!      Determine whether or not total moisture (water vapor+total cloud
!      condensate) is the control variable
       iqtotal=getindex(cvars3d,'qt')

       if (icw_av>0) cw_cv=.true.
       if (icw_av>0 .or. iql_av>0 .or. iqi_av>0 .or. iqtotal>0) icloud_cv=.true.
       if (icloud_cv .and. icloud_forward) iallsky=.true.

    end if  ! end of (n_actual_clouds>0)


!   inquire number of aerosols
    call gsi_chemguess_get ( 'aerosols::3d', n_actual_aerosols, ier )
    if (n_actual_aerosols > 0) then
       iaerosol_forward=.true.
       allocate(aero_names(n_actual_aerosols))
       call gsi_chemguess_get ('aerosols::3d',aero_names,ier)
       indx_p25   = getindex(aero_names,'p25')
       indx_dust1 = getindex(aero_names,'dust1')
       indx_dust2 = getindex(aero_names,'dust2')

       call gsi_chemguess_get ( 'aerosols_4crtm::3d', n_aerosols_for, ier )
       if (n_aerosols_for >0) then
          allocate(aero_names_for(n_aerosols_for))
          call gsi_chemguess_get ( 'aerosols_4crtm::3d', aero_names_for, ier)  
       end if
       call gsi_chemguess_get ( 'aerosols_4crtm_jac::3d', n_aerosols_jac, ier )
       if (n_aerosols_jac >0) then
          allocate(aero_names_jac(n_aerosols_jac))
          call gsi_chemguess_get ( 'aerosols_4crtm_jac::3d', aero_names_jac, ier)  
       end if
    endif

!   Determine whether aerosols are control variables
    ip25_av=getindex(cvars3d,'p25')
    idust1_av=getindex(cvars3d,'dust1')
    idust2_av=getindex(cvars3d,'dust2')
    if (ip25_av>0 .or. idust1_av>0 .or. idust2_av>0) iaerosol_cv=.true.

    if (iaerosol_cv .and. iaerosol_forward) iaerosol=.true.

    if (mype==0) then
       write(6,*) 'radiance_mode_init: icloud_forward=',icloud_forward,' iallsky=',iallsky, &
                  ' cw_cv=',cw_cv,' iaerosol_forward=',iaerosol_forward,' iaerosol=',iaerosol
       write(6,*) 'radiance_mode_init: n_actual_clouds=',n_actual_clouds
       if (n_actual_clouds>0) write(6,*) 'radiance_mode_init: cloud_names=',cloud_names  
       write(6,*) 'radiance_mode_init: n_clouds_for=',n_clouds_for
       if (n_clouds_for>0) write(6,*) 'radiance_mode_init: cloud_names_for=',cloud_names_for
       write(6,*) 'radiance_mode_init: n_clouds_jac=',n_clouds_jac
       if (n_clouds_jac>0) write(6,*) 'radiance_mode_init: cloud_names_jac=',cloud_names_jac
       write(6,*) 'radiance_mode_init: n_actual_aerosols=',n_actual_aerosols
       if (n_actual_aerosols>0) write(6,*) 'radiance_mode_init: aero_names=',aero_names
       write(6,*) 'radiance_mode_init: n_aerosols_for=',n_aerosols_for
       if (n_aerosols_for>0) write(6,*) 'radiance_mode_init: aero_names_for=',aero_names_for
       write(6,*) 'radiance_mode_init: n_aerosols_jac=',n_aerosols_jac
       if (n_aerosols_jac>0) write(6,*) 'radiance_mode_init: aero_names_jac=',aero_names_jac
    end if
    
  end subroutine radiance_mode_init

  subroutine radiance_mode_destroy
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    radiance_mode_destroy
!
!   prgrmmr:     yanqiu zhu      org: np23                date: 2015-07-20
!
! abstract:  This routine deallocate arrays
!
! program history log:
!   2015-07-20  zhu     
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

    implicit none
 
    if(allocated(cloud_names)) deallocate(cloud_names)
    if(allocated(cloud_names_for)) deallocate(cloud_names_for)
    if(allocated(cloud_names_jac)) deallocate(cloud_names_jac)
  
    if(allocated(aero_names)) deallocate(aero_names)
    if(allocated(aero_names_for)) deallocate(aero_names_for)
    if(allocated(aero_names_jac)) deallocate(aero_names_jac)

  end subroutine radiance_mode_destroy

  subroutine radiance_obstype_init
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    radiance_obstype_init
!
!   prgrmmr:    yanqiu zhu      org: np23                date: 2015-07-20
!
! abstract:  This routine sets default values for variables used in
!            the cloudy/with aerosol radiance processing routines.
!
! program history log:
!   2015-07-20  zhu     
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

    use kinds, only: i_kind,r_kind
    use radinfo, only: nusis,jpch_rad,nuchan,icloud4crtm,iaerosol4crtm
    use obsmod, only: ndat,dtype,dsis
    implicit none

    logical :: first,diffistr,found
    integer(i_kind) :: i,j,k,ii,nn1,nn2,nn
    integer(i_kind),dimension(ndat) :: k2i
    character(10),dimension(ndat) :: rtype,rrtype,drtype

!   Cross-check 
    do j=1,jpch_rad
       if (icloud4crtm(j)>=0) then
          if (.not. iallsky) icloud4crtm(j)=0
          if (.not. icloud_forward) icloud4crtm(j)=-1
       end if
       if (iaerosol4crtm(j)>=0) then
          if (.not. iaerosol) iaerosol4crtm(j)=0
          if (.not. iaerosol_forward) iaerosol4crtm(j)=-1
       end if
    end do

    if (icloud_forward .and. all(icloud4crtm<0)) then 
       icloud_forward=.false.
       iallsky=.false.
       n_clouds_for=0
       n_clouds_jac=0       
       cloud_names_for=' '
       cloud_names_jac=' '
    end if

    if (iaerosol_forward .and. all(iaerosol4crtm<0)) then
       iaerosol_forward=.false.
       iaerosol=.false.
       n_aerosols_for=0
       n_aerosols_jac=0   
       aero_names_for=' '
       aero_names_jac=' '
    end if

    if (iallsky .and. all(icloud4crtm<1)) then
       iallsky=.false.
       n_clouds_jac=0
       cloud_names_jac=' '
    end if

    if (iaerosol .and. all(iaerosol4crtm<1)) then
       iaerosol=.false.
       n_aerosols_jac=0
       aero_names_jac=' '
    end if

!   determine rads type
    drtype='other'
    do i=1,ndat
       rtype(i)=dtype(i)                   !     rtype  - observation types to process
       if (index(dtype(i),'amsre') /= 0)  rtype(i)='amsre'
       if (index(dtype(i),'ssmis') /= 0)  rtype(i)='ssmis'
       if (index(dtype(i),'sndr') /= 0)   rtype(i)='sndr'
       if (index(dtype(i),'hirs') /= 0)   rtype(i)='hirs'
       if (index(dtype(i),'avhrr') /= 0)  rtype(i)='avhrr'
       if (index(dtype(i),'modis') /= 0)  rtype(i)='modis'
       if (index(dtype(i),'seviri') /= 0) rtype(i)='seviri'

       if(rtype(i) == 'hirs'   .or. rtype(i) == 'sndr'     .or.  rtype(i) == 'seviri' .or. &
          rtype(i) == 'airs'   .or. rtype(i) == 'amsua'    .or.  rtype(i) == 'msu'    .or. & 
          rtype(i) == 'iasi'   .or. rtype(i) == 'amsub'    .or.  rtype(i) == 'mhs'    .or. &
          rtype(i) == 'hsb'    .or. rtype(i) == 'goes_img' .or.  rtype(i) == 'ahi'    .or. &
          rtype(i) == 'avhrr'  .or. rtype(i) == 'amsre'    .or.  rtype(i) == 'ssmis'  .or. & 
          rtype(i) == 'ssmi'   .or. rtype(i) == 'atms'     .or.  rtype(i) == 'cris'   .or. & 
          rtype(i) == 'amsr2'  .or. rtype(i) == 'gmi'      .or.  rtype(i) == 'saphir'   ) then
          drtype(i)='rads'
       end if
    end do
 
!   Determine total rad types
    k=0
    k2i=0
    first=.true.
    rrtype=''
    do i=1,ndat
       if (drtype(i) /= 'rads') cycle
 
       found=.false.
       if (first) then
          k=k+1
          rrtype(k)=rtype(i) 
          k2i(k)=i
          first=.false.
       else
          do j=1,k
             if (trim(rtype(i)) == trim(rrtype(j))) then 
                found=.true.
                exit
             end if
          end do
          if (.not. found) then
             k=k+1
             rrtype(k)=rtype(i)
             k2i(k)=i
          end if
       end if
    end do
    total_rad_type=k
    if (mype==0) write(6,*) 'radiance_obstype_init: total_rad_type=', k,' types are: ', rrtype(1:total_rad_type)

    if (total_rad_type<=0) return

    allocate(rad_type_info(total_rad_type)) 

    do k=1, total_rad_type
       rad_type_info(k)%rtype=rrtype(k)
       rad_type_info(k)%cld_sea_only=.false.
       rad_type_info(k)%ex_obserr=.false.
       rad_type_info(k)%ex_biascor=.false.
       rad_type_info(k)%cld_effect=.false.
       rad_type_info(k)%lcloud_forward=.false.
       rad_type_info(k)%lallsky=.false.
       rad_type_info(k)%laerosol_forward=.false.
       rad_type_info(k)%laerosol=.false.

       ii=k2i(k)
       first=.true.
       nn1=0
       nn2=0
       do j=1,jpch_rad
          if (j==jpch_rad) then
             diffistr = .true.
          else
             diffistr = trim(nusis(j))/=trim(nusis(j+1))
          end if
!         if (trim(dsis(ii))==trim(nusis(j))) then
          if (index(trim(nusis(j)),trim(rrtype(k))) /= 0) then
             if (first) then
                nn1=j
                first=.false.
             else
                nn2=j
             end if
             if (diffistr) exit
          end if
       end do
       if (nn1/=0 .and. nn2/=0) then
          rad_type_info(k)%nchannel=nn2-nn1+1
       else
          cycle
       end if

!      determine usages of cloud and aerosol in each type
       allocate(rad_type_info(k)%lcloud4crtm(rad_type_info(k)%nchannel)) 
       allocate(rad_type_info(k)%laerosol4crtm(rad_type_info(k)%nchannel)) 
       nn=0
       do j=nn1,nn2
          nn=nn+1
          rad_type_info(k)%lcloud4crtm(nn)=icloud4crtm(j)
          rad_type_info(k)%laerosol4crtm(nn)=iaerosol4crtm(j)

          if (icloud4crtm(j)<0 .and. iaerosol4crtm(j)<0) cycle
          if (.not. rad_type_info(k)%lallsky) then
             if (icloud4crtm(j)==1) then 
                rad_type_info(k)%lallsky=.true.
                rad_type_info(k)%lcloud_forward=.true.
             end if
          end if
          if (.not. rad_type_info(k)%lcloud_forward) then
             if (icloud4crtm(j)==0) rad_type_info(k)%lcloud_forward=.true.
          end if
          if (.not. rad_type_info(k)%laerosol) then
             if (iaerosol4crtm(j)==1) then 
                rad_type_info(k)%laerosol=.true.
                rad_type_info(k)%laerosol_forward=.true.
             end if
          end if
          if (.not. rad_type_info(k)%laerosol_forward) then
             if (iaerosol4crtm(j)==0) rad_type_info(k)%laerosol_forward=.true.
          end if
       end do
       if (mype==0) write(6,*) 'radiance_obstype_init: type=', rad_type_info(k)%rtype, &
                               ' nch=',rad_type_info(k)%nchannel, &
                               ' lcloud_forward=',rad_type_info(k)%lcloud_forward, &
                               ' lallsky=',rad_type_info(k)%lallsky, &
                               ' laerosol_forward=',rad_type_info(k)%laerosol_forward, &
                               ' laerosol=',rad_type_info(k)%laerosol
    end do ! end total_rad_type

  end subroutine radiance_obstype_init

  subroutine radiance_obstype_search(obstype,radmod)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    radiance_obstype_search   find the rad_type_info(i) that
!                                          matches the input obstype
!
!   prgrmmr:    yanqiu zhu      org: np23                date: 2015-08-20
!
! abstract:
!
! program history log:
!   2015-08-20  zhu
!
!   input argument list:
!         obstype
!
!   output argument list:
!         radmod
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block
    implicit none
    character(10) :: obstype
    type(rad_obs_type) :: radmod
    integer i,j
    
    do i=1,total_rad_type
       if (index(trim(obstype),trim(rad_type_info(i)%rtype)) /= 0) then
          if (mype==0) write(6,*) 'radiance_obstype_search: obstype=',obstype, &
                                  ' rtype=',rad_type_info(i)%rtype
          radmod%rtype = rad_type_info(i)%rtype
          radmod%nchannel = rad_type_info(i)%nchannel
          radmod%cld_sea_only = rad_type_info(i)%cld_sea_only
          radmod%cld_effect = rad_type_info(i)%cld_effect
          radmod%ex_obserr = rad_type_info(i)%ex_obserr
          radmod%ex_biascor = rad_type_info(i)%ex_biascor

          radmod%lcloud_forward = rad_type_info(i)%lcloud_forward
          radmod%lallsky = rad_type_info(i)%lallsky
          radmod%lcloud4crtm => rad_type_info(i)%lcloud4crtm

          radmod%laerosol_forward = rad_type_info(i)%laerosol_forward
          radmod%laerosol = rad_type_info(i)%laerosol
          radmod%laerosol4crtm => rad_type_info(i)%laerosol4crtm
          exit
       end if
    end do

  end subroutine radiance_obstype_search


  subroutine radiance_obstype_destroy
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    radiance_obstype_destroy
!
!   prgrmmr:    yanqiu zhu      org: np23                date: 2015-07-20
!
! abstract:  
!
! program history log:
!   2015-07-20  zhu
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block
    implicit none

    integer(i_kind) :: k

    do k=1, total_rad_type
       if(associated(rad_type_info(k)%lcloud4crtm)) deallocate(rad_type_info(k)%lcloud4crtm)
       if(associated(rad_type_info(k)%laerosol4crtm)) deallocate(rad_type_info(k)%laerosol4crtm)
    end do
    if(allocated(rad_type_info)) deallocate(rad_type_info)

  end subroutine radiance_obstype_destroy


  subroutine radiance_parameter_cloudy_init
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    radiance_parameter_cloudy_init
!
!   prgrmmr:    yanqiu zhu      org: np23                date: 2015-07-20
!
! abstract:  This routine sets default values for variables used in
!            the cloudy radiance processing routines.
!
! program history log:
!   2015-07-20  zhu
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

    use kinds, only: i_kind,r_kind
    implicit none

    integer(i_kind) :: lunin
    character(len=10) :: obsname
    character(len=10) :: obsloc   ! global, sea, or, land ...
    logical :: obserr,biascor,cldeff
    logical :: pcexist

    integer(i_kind) i
    character(len=20) :: filename

    namelist/obs_amsua/obsname,obsloc,obserr,biascor,cldeff

    inquire(file='cloudy_radiance_info.txt',exist=pcexist)
    if (.not. pcexist) return
    lunin=11
    open(lunin,file='cloudy_radiance_info.txt',form='formatted')

!-- amsua
    read(lunin,obs_amsua)
    do i=1,total_rad_type
       if (index(trim(rad_type_info(i)%rtype),trim(obsname)) /= 0) then
          if (trim(obsloc)=='sea') rad_type_info(i)%cld_sea_only=.true.
          rad_type_info(i)%ex_obserr=obserr
          rad_type_info(i)%ex_biascor=biascor
          rad_type_info(i)%cld_effect=cldeff

          if (.not. rad_type_info(i)%lcloud_forward) then 
             rad_type_info(i)%cld_sea_only=.false.
             rad_type_info(i)%cld_effect=.false.
             rad_type_info(i)%ex_obserr=.false.
             rad_type_info(i)%ex_biascor=.false.
          end if

          if (trim(obsname)=='amsua') then
             cloudy_amsua%nchannel=rad_type_info(i)%nchannel
             cloudy_amsua%cld_sea_only=rad_type_info(i)%cld_sea_only
             cloudy_amsua%cld_effect=rad_type_info(i)%cld_effect
             cloudy_amsua%ex_obserr=rad_type_info(i)%ex_obserr
             cloudy_amsua%ex_biascor=rad_type_info(i)%ex_biascor

             cloudy_amsua%lcloud_forward=rad_type_info(i)%lcloud_forward
             cloudy_amsua%lallsky=rad_type_info(i)%lallsky
             cloudy_amsua%lcloud4crtm=>rad_type_info(i)%lcloud4crtm
             cloudy_amsua%laerosol_forward=rad_type_info(i)%laerosol_forward
             cloudy_amsua%laerosol=rad_type_info(i)%laerosol
             cloudy_amsua%laerosol4crtm=>rad_type_info(i)%laerosol4crtm
             if (mype==0) write(6,*) 'amsua: nchannel= ',cloudy_amsua%nchannel, &
                          ' cld_sea_only=',cloudy_amsua%cld_sea_only, &
                          ' cld_effect=',cloudy_amsua%cld_effect, &
                          ' ex_obserr=',cloudy_amsua%ex_obserr, &
                          ' ex_biascor=',cloudy_amsua%ex_biascor
          end if
       end if
    end do

!   allocate space for entries from table
!   Obtain table contents
    allocate(cloudy_amsua%cclr(cloudy_amsua%nchannel), & 
             cloudy_amsua%ccld(cloudy_amsua%nchannel))
    filename='obs_'//trim(obsname)
    call amsua_table(filename,lunin,cloudy_amsua%nchannel,cloudy_amsua%cclr,cloudy_amsua%ccld)
!-- end of amsua

    close(lunin)
  end subroutine radiance_parameter_cloudy_init


  subroutine amsua_table(filename,lunin,nchal,cclr,ccld)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    amsua_table
!
!   prgrmmr:    yanqiu zhu      org: np23                date: 2015-09-10
!
! abstract:  This routine retrieves parameters used for AMSUA all-sky radiance
!
! program history log:
!   2015-09-10  zhu
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block
    use kinds, only: i_kind,r_kind
    use mpeu_util, only: gettablesize
    use mpeu_util, only: gettable
    implicit none

    character(len=*), intent(in) :: filename
    integer(i_kind) , intent(in) :: lunin
    integer(i_kind) , intent(in) :: nchal
    real(r_kind)    , dimension(nchal), intent(inout) :: cclr,ccld

    integer(i_kind) i,ii,ntot,nrows,ich0
    real(r_kind) cclr0,ccld0
    character(len=256),allocatable,dimension(:):: utable

!   Initialize the arrays
    cclr(:)=zero
    ccld(:)=zero

!   Scan file for desired table first
!   and get size of table
    call gettablesize(filename,lunin,ntot,nrows)
    if (mype==0) write(6,*) 'amsua_table: ',filename, nrows
    if(nrows==0) then
       return
    endif

!   Get contents of table
    allocate(utable(nrows))
    call gettable(filename,lunin,ntot,nrows,utable)

!   Retrieve each token of interest from table
    do ii=1,nrows
       read(utable(ii),*) ich0,cclr0,ccld0
       cclr(ich0)=cclr0
       ccld(ich0)=ccld0
    enddo
    deallocate(utable)

    if (mype==0) then
       write(6,*) 'amsua_table: ich  cclr  ccld '
       do ii=1,nchal
          write(6,*) ii,cclr(ii),ccld(ii)
       end do
    end if

  end subroutine amsua_table

  subroutine radiance_parameter_cloudy_destroy
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    radiance_parameter_cloudy_init
!
!   prgrmmr:    yanqiu zhu      org: np23                date: 2015-07-20
!
! abstract:  This routine sets default values for variables used in
!            the cloudy radiance processing routines.
!
! program history log:
!   2015-07-20  zhu
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block
    
    if (allocated(cloudy_amsua%cclr)) deallocate(cloudy_amsua%cclr)
    if (allocated(cloudy_amsua%ccld)) deallocate(cloudy_amsua%ccld)

  end subroutine radiance_parameter_cloudy_destroy

  subroutine radiance_parameter_aero_init
    implicit none
  end subroutine radiance_parameter_aero_init

  subroutine radiance_ex_obserr(radmod,nchanl,clwp_amsua,clw_guess_retrieval, &
                                tnoise,tnoise_cld,error0)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    radiance_ex_obserr
!
!   prgrmmr:    yanqiu zhu      org: np23                date: 2015-09-10
!
! abstract:  This routine includes extra observation error assignment routines.
!
! program history log:
!   2015-09-10  zhu
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block
    use kinds, only: i_kind,r_kind
    implicit none
    
    integer(i_kind),intent(in) :: nchanl
    real(r_kind),intent(in) :: clwp_amsua,clw_guess_retrieval
    real(r_kind),dimension(nchanl),intent(in):: tnoise,tnoise_cld
    real(r_kind),dimension(nchanl),intent(inout) :: error0
    type(rad_obs_type),intent(in) :: radmod 

    integer(i_kind) :: i
    real(r_kind) :: clwtmp

    if (.not. radmod%ex_obserr) return  

! -- amsua
    if (trim(radmod%rtype) =='amsua') then
       do i=1,nchanl
          clwtmp=half*(clwp_amsua+clw_guess_retrieval)
          if(clwtmp <= cloudy_amsua%cclr(i)) then
             error0(i) = tnoise(i)
          else if(clwtmp > cloudy_amsua%cclr(i) .and. clwtmp < cloudy_amsua%ccld(i)) then
             error0(i) = tnoise(i) + (clwtmp-cloudy_amsua%cclr(i))* &
                         (tnoise_cld(i)-tnoise(i))/(cloudy_amsua%ccld(i)-cloudy_amsua%cclr(i))
          else
             error0(i) = tnoise_cld(i)
          endif
       end do
       return
    end if

  end subroutine radiance_ex_obserr

  subroutine radiance_ex_biascor(radmod,nchanl,tsim_bc,tsavg5,zasat, & 
                       clw_guess_retrieval,clwp_amsua,cld_rbc_idx,ierrret,scat)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    radiance_ex_biascor
!
!   prgrmmr:    yanqiu zhu      org: np23                date: 2015-09-20
!
! abstract:  This routine include extra radiance bias correction routines.
!
! program history log:
!   2015-09-20  zhu
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block
    use kinds, only: i_kind,r_kind
    use clw_mod, only: ret_amsua
    implicit none

    integer(i_kind)                   ,intent(in   ) :: nchanl
    real(r_kind),dimension(nchanl)    ,intent(in   ) :: tsim_bc
    real(r_kind)                      ,intent(in   ) :: tsavg5,zasat
    real(r_kind),dimension(nchanl)    ,intent(inout) :: cld_rbc_idx
    real(r_kind)                      ,intent(inout) :: clwp_amsua
    real(r_kind)                      ,intent(inout) :: clw_guess_retrieval
    real(r_kind),optional             ,intent(inout) :: scat
    type(rad_obs_type)                ,intent(in)    :: radmod
    integer(i_kind)                   ,intent(  out) :: ierrret

    integer(i_kind) :: i

! -- amsua
    if (trim(radmod%rtype) =='amsua') then
!      call ret_amsua(tb_obs,nchanl,tsavg5,zasat,clwp_amsua,ierrret,scat) 
       call ret_amsua(tsim_bc,nchanl,tsavg5,zasat,clw_guess_retrieval,ierrret)
       do i=1,nchanl
          if ((clwp_amsua-cloudy_amsua%cclr(i))*(clw_guess_retrieval-cloudy_amsua%cclr(i))<zero  &
             .and. abs(clwp_amsua-clw_guess_retrieval)>=0.005_r_kind) cld_rbc_idx(i)=zero
       end do
       return
    end if

  end subroutine radiance_ex_biascor
end module radiance_mod

