module sfcobsqc
!$$$ module documentation block
!           .      .    .                                       .
! module:   sfcobsqc
!   prgmmr: pondeca
!
! abstract: contains subroutines for the qc of surface obs based
!           on (i) the provider uselist for mesonet winds, (ii) 
!           station uselist for mesonet winds, and (iii) rejectlists 
!           for any ob type (u and v-wind, wind speed, temperatature, 
!           moisture, surface pressure). the code inquires for the 
!           existence of thsese lists in the gsi working directorty 
!           and applies them if present.
!           
!
! program history log:
!   2007-10-19  pondeca
!   2011-02-15  zhu - add get_gustqm
!
! subroutines included:
!   sub init_rjlists
!   sub get_usagerj
!   sub get_gustqm
!   readin_rjlists
!   sub destroy_rjlists
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: i_kind,r_kind,r_single,r_double

  implicit none

  private

  character(16),allocatable,dimension(:)::cprovider
  character(5),allocatable,dimension(:)::csta_winduse
  character(80),allocatable,dimension(:)::w_rjlist,t_rjlist,p_rjlist,q_rjlist
  character(80),allocatable,dimension(:)::t_day_rjlist,t_night_rjlist
  character(80),allocatable,dimension(:)::q_day_rjlist,q_night_rjlist
  character(8),allocatable,dimension(:,:)::csta_windbin

  integer(i_kind) nprov,nwrjs,ntrjs,nprjs,nqrjs,nsta_mesowind_use
  integer(i_kind) ntrjs_day,ntrjs_night
  integer(i_kind) nqrjs_day,nqrjs_night
  integer(i_kind) nbins
  integer(i_kind),allocatable,dimension(:)::nwbaccpts

  logical listexist
  logical wlistexist
  logical tlistexist
  logical plistexist
  logical qlistexist
  logical listexist2
  logical t_day_listexist
  logical t_night_listexist
  logical q_day_listexist
  logical q_night_listexist
  logical wbinlistexist

  public init_rjlists
  public get_usagerj
  public get_gustqm
  public readin_rjlists
  public get_sunangle
  public get_wbinid
  public destroy_rjlists

  logical :: verbose = .false.
contains

subroutine init_rjlists
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_rjlists
!   prgmmr:
!
! abstract: initialize qc lists 
!
! program history log:
!   2009-10-01  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

! Declare passed variables

! Declare local variables
  integer(i_kind) meso_unit,ncount
  integer(i_kind) ibin
  real(r_single) aa1,aa2
  character(80) cstring
  character(80) clistname
  character(8) ach8,bch8
  character(16) ch16

  integer(i_kind), parameter::nmax=100000
  integer(i_kind) nmax2

  data meso_unit / 20 /
!**************************************************************************
  allocate(cprovider(500))
  allocate(w_rjlist(nmax))
  allocate(t_rjlist(nmax))
  allocate(p_rjlist(nmax))
  allocate(q_rjlist(nmax))
  allocate(csta_winduse(nmax))
  allocate(t_day_rjlist(nmax))
  allocate(t_night_rjlist(nmax))
  allocate(q_day_rjlist(nmax))
  allocate(q_night_rjlist(nmax))

!==> Read mesonet provider names from the uselist
 clistname='mesonetuselist'
 call readin_rjlists(clistname,listexist,cprovider,500,nprov)
 if(verbose)&
 print*,'mesonetuselist: listexist,nprov=',listexist,nprov


 
!==> Read in station names from the reject list for wind
 clistname='w_rejectlist'
 call readin_rjlists(clistname,wlistexist,w_rjlist,nmax,nwrjs)
 if(verbose)&
 print*,'w_rejectlist: wlistexist,nwrjs=',wlistexist,nwrjs


 
!==> Read in station names from the reject lists for temperature
 clistname='t_rejectlist'
 call readin_rjlists(clistname,tlistexist,t_rjlist,nmax,ntrjs)
 if(verbose)&
 print*,'t_rejectlist: tlistexist,ntrjs=',tlistexist,ntrjs


 
 clistname='t_day_rejectlist'
 call readin_rjlists(clistname,t_day_listexist,t_day_rjlist,nmax,ntrjs_day)
 if(verbose)&
 print*,'t_day_rejectlist: t_day_listexist,ntrjs_day=',t_day_listexist,ntrjs_day


 
 clistname='t_night_rejectlist'
 call readin_rjlists(clistname,t_night_listexist,t_night_rjlist,nmax,ntrjs_night)
 if(verbose)&
 print*,'t_night_rejectlist: t_night_listexist,ntrjs_night=',t_night_listexist,ntrjs_night


 
!==> Read in station names from the reject list for surface pressure
 clistname='p_rejectlist'
 call readin_rjlists(clistname,plistexist,p_rjlist,nmax,nprjs)
 if(verbose)&
 print*,'p_rejectlist: plistexist,nprjs=',plistexist,nprjs


 
!==> Read in station names from the reject lists for specific humidity
 clistname='q_rejectlist'
 call readin_rjlists(clistname,qlistexist,q_rjlist,nmax,nqrjs)
 if(verbose)&
 print*,'q_rejectlist: qlistexist,nqrjs=',qlistexist,nqrjs


 
 clistname='q_day_rejectlist'
 call readin_rjlists(clistname,q_day_listexist,q_day_rjlist,nmax,nqrjs_day)
 if(verbose)&
 print*,'q_day_rejectlist: q_day_listexist,nqrjs_day=',q_day_listexist,nqrjs_day

  

 clistname='q_night_rejectlist'
 call readin_rjlists(clistname,q_night_listexist,q_night_rjlist,nmax,nqrjs_night)
 if(verbose)&
 print*,'q_night_rejectlist: q_night_listexist,nqrjs_night=',q_night_listexist,nqrjs_night


 
!==> Read in 'good' mesonet station names from the station uselist
 inquire(file='mesonet_stnuselist',exist=listexist2)
 if(listexist2) then
    open (meso_unit,file='mesonet_stnuselist',form='formatted')
    ncount=0
180 continue
    ncount=ncount+1
    read(meso_unit,'(a5,a80)',end=181) csta_winduse(ncount),cstring
    goto 180
181 continue
    nsta_mesowind_use=ncount-1
    if(verbose)&
    print*,'mesonet_stnuselist: nsta_mesowind_use=',nsta_mesowind_use
 endif
 close(meso_unit)

!==> Read in wind direction stratified wind accept lists
 inquire(file='wbinuselist',exist=wbinlistexist)
 if(verbose)&
 print*,'wdirbinuselist: wbinuselist=',wbinlistexist

 nbins=0
 if(wbinlistexist) then
    open (meso_unit,file='wbinuselist',form='formatted')
    read(meso_unit,'(a16,i2)',end=191) ch16,nbins
    allocate(nwbaccpts(max(1,nbins)))
    nwbaccpts(:)=0
190 continue    
    read(meso_unit,'(a8,3x,a8,3x,f7.4,2x,f9.4,3x,i2)',end=191) ach8,bch8,aa1,aa2,ibin
    nwbaccpts(ibin)=nwbaccpts(ibin)+1 
    goto 190
191 continue
    if(verbose)then
       print*,'wdirbinuselist: number of bins=',nbins 
       print*,'wdirbinuselist: (nwbaccpts(ibin),ibin=1,nbins)=',(nwbaccpts(ibin),ibin=1,nbins)
    endif

    nmax2=maxval(nwbaccpts(:))
    if(verbose)&
    print*,'wdirbinuselist: maximum number of obs in a single bin=',nmax2

    allocate(csta_windbin(max(1,nmax2),max(1,nbins)))

    rewind(meso_unit)
    read(meso_unit,'(a16,i2)',end=193) ch16,nbins
    nwbaccpts(:)=0
192 continue    
    read(meso_unit,'(a8,3x,a8,3x,f7.4,2x,f9.4,3x,i2)',end=193) ach8,bch8,aa1,aa2,ibin
    nwbaccpts(ibin)=nwbaccpts(ibin)+1 
    csta_windbin(nwbaccpts(ibin),ibin)=ach8
    goto 192
193 continue
 endif
 close(meso_unit)

end subroutine init_rjlists

subroutine get_usagerj(kx,obstype,c_station_id,c_prvstg,c_sprvstg, &
                       dlon,dlat,idate,dtime,udbl,vdbl,usage_rj)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_usagerg
!   prgmmr:
!
! abstract: determine the usage value of read_prepbufr for surface obs. the following
!           is done: (i) if incoming usage value is >=100. then do nothing, since
!           read_prepbufr has already flagged this ob and assigned a specific usage 
!           value to it. (ii) use usage=500. for temperature, moisture, or surface pressure
!           obs which are found in the rejectlist. (iii) 
!
! program history log:
!   2009-10-01  lueken - added subprogram doc block
!
!   input argument list:
!    kx
!    obstype
!    c_station_id
!    c_prvstg,c_sprvstg
!    dlon
!    dlat
!    idate
!    udbl
!    vdbl
!
!   output argument list:
!    usage_rj
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use constants, only: zero_single
  implicit none

  integer(i_kind),intent(in   ) :: kx
  integer(i_kind),intent(in   ) :: idate
  character(10)  ,intent(in   ) :: obstype
  character(8)   ,intent(in   ) :: c_station_id
  character(8)   ,intent(in   ) :: c_prvstg,c_sprvstg
  real(r_kind)   ,intent(in   ) :: dlon
  real(r_kind)   ,intent(in   ) :: dlat
  real(r_kind)   ,intent(in   ) :: dtime
  real(r_double) ,intent(in   ) :: udbl,vdbl
  real(r_kind)   ,intent(inout) :: usage_rj

! Declare local variables
  integer(i_kind) m,nlen
  integer(i_kind) ibin
  character(8)  ch8
  real(r_kind) usage_rj0
  real(r_single) sunangle

! Declare local parameters
  real(r_kind),parameter:: r6    = 6.0_r_kind
  real(r_kind),parameter:: r5000 = 5000._r_kind
  real(r_kind),parameter:: r5100 = 5100._r_kind
  real(r_kind),parameter:: r6000 = 6000._r_kind
  real(r_kind),parameter:: r6100 = 6100._r_kind
  real(r_kind),parameter:: r6200 = 6200._r_kind

  if (usage_rj >= r6) return

  usage_rj0=usage_rj

  if (kx<200) then  !<==mass obs

    if(obstype=='t') then

      if(tlistexist ) then
        do m=1,ntrjs
           ch8(1:8)=t_rjlist(m)(1:8)
           nlen=len_trim(ch8)
           if ((trim(c_station_id) == trim(ch8)) .or. &
               ((kx==188.or.kx==195).and.c_station_id(1:nlen)==ch8(1:nlen))) then !handle wfo's mesonets which never end with
              usage_rj=r5000                                           !an "a" or "x" in the eight position following blanks
              exit
           endif
        enddo
      endif

      if((t_day_listexist .or. t_night_listexist) .and. usage_rj==usage_rj0) then
           call get_sunangle(idate,dtime,dlon,dlat,sunangle)
           if (sunangle > zero_single) then
              do m=1,ntrjs_day
                 ch8(1:8)=t_day_rjlist(m)(1:8)
                 nlen=len_trim(ch8)
                 if ((trim(c_station_id) == trim(ch8)) .or. &
                     ((kx==188.or.kx==195).and.c_station_id(1:nlen)==ch8(1:nlen))) then
                    usage_rj=r5100
                    exit
                 endif
              enddo
             else 
              do m=1,ntrjs_night
                 ch8(1:8)=t_night_rjlist(m)(1:8)
                 nlen=len_trim(ch8)
                 if ((trim(c_station_id) == trim(ch8)) .or. &
                     ((kx==188.or.kx==195).and.c_station_id(1:nlen)==ch8(1:nlen))) then
                    usage_rj=r5100
                    exit
                 endif
              enddo
           endif 
      endif

     elseif(obstype=='q') then

      if(qlistexist ) then
        do m=1,nqrjs
           ch8(1:8)=q_rjlist(m)(1:8)
           nlen=len_trim(ch8)
           if ((trim(c_station_id) == trim(ch8)) .or. &
               ((kx==188.or.kx==195).and.c_station_id(1:nlen)==ch8(1:nlen))) then
              usage_rj=r5000
              exit
           endif
        enddo
      endif

      if((q_day_listexist .or. q_night_listexist) .and. usage_rj==usage_rj0) then
           call get_sunangle(idate,dtime,dlon,dlat,sunangle)
           if (sunangle > zero_single) then
              do m=1,nqrjs_day
                 ch8(1:8)=q_day_rjlist(m)(1:8)
                 nlen=len_trim(ch8)
                 if ((trim(c_station_id) == trim(ch8)) .or. &
                     ((kx==188.or.kx==195).and.c_station_id(1:nlen)==ch8(1:nlen))) then
                    usage_rj=r5100
                    exit
                 endif
              enddo
             else 
              do m=1,nqrjs_night
                 ch8(1:8)=q_night_rjlist(m)(1:8)
                 nlen=len_trim(ch8)
                 if ((trim(c_station_id) == trim(ch8)) .or. &
                     ((kx==188.or.kx==195).and.c_station_id(1:nlen)==ch8(1:nlen))) then
                    usage_rj=r5100
                    exit
                 endif
              enddo
           endif 
      endif

     elseif(obstype=='ps' .and. plistexist ) then
        do m=1,nprjs
           ch8(1:8)=p_rjlist(m)(1:8)
           nlen=len_trim(ch8)
           if ((trim(c_station_id) == trim(ch8)) .or. &
               ((kx==188.or.kx==195).and.c_station_id(1:nlen)==ch8(1:nlen))) then
              usage_rj=r5000
              exit
           endif
        enddo
     end if
  endif

  if (kx>=200) then !<==wind obs

     if (kx==288.or.kx==295) then
        usage_rj=r6000
        if (listexist) then !note that uselists must precede the rejectlist
           do m=1,nprov
!             if (trim(c_prvstg//c_sprvstg) == trim(cprovider(m))) then
              if (c_prvstg(1:8) == cprovider(m)(1:8) .and. (c_sprvstg(1:8) == cprovider(m)(9:16)  &
                                                      .or. cprovider(m)(9:16) == 'allsprvs') ) then
                 usage_rj=usage_rj0
                 exit
              endif
           enddo
        endif
        if (listexist2 .and. usage_rj/=usage_rj0) then
           do m=1,nsta_mesowind_use
              if (c_station_id(1:5) == csta_winduse(m)(1:5)) then
                 usage_rj=usage_rj0
                 exit
              endif
           enddo
        endif
        if(wbinlistexist .and. usage_rj/=usage_rj0) then
          call get_wbinid(udbl,vdbl,nbins,ibin)
          do m=1,nwbaccpts(ibin)
             ch8(1:8)=csta_windbin(m,ibin)(1:8)
             nlen=len_trim(ch8)
             if (c_station_id(1:nlen)==ch8(1:nlen)) then
                usage_rj=usage_rj0
                exit
             endif
          enddo
        endif
     endif

     if(obstype=='uv' .and. wlistexist ) then
        do m=1,nwrjs
           ch8(1:8)=w_rjlist(m)(1:8)
           nlen=len_trim(ch8)
           if ((trim(c_station_id) == trim(ch8)) .or. &
              ((kx==288.or.kx==295).and. c_station_id(1:nlen)==ch8(1:nlen))) then
              if (kx/=288.and.kx/=295) then
                 usage_rj=r5000
              else
                 if (usage_rj==usage_rj0)    usage_rj=r6100 !ob is in at least one of the above three uselists
                 if (usage_rj==r6000)        usage_rj=r6200 !ob is in none of the above three uselists
              endif
              exit
           endif
        enddo
     endif

  end if
end subroutine get_usagerj


subroutine get_gustqm(kx,c_station_id,c_prvstg,c_sprvstg,gustqm)
!$$$  module documentation block
! abstract: determine the qm for gust 1) for gust that is not on the uselist qm=9
!                                     2) for gust that is on the rejectlist qm=3 or 15
! program history log:
!   2009-01-29  zhu
!
  implicit none

  integer(i_kind) kx
  integer(i_kind) nlen
  integer(i_kind) gustqm
  character(8),intent(in)::  c_station_id
  character(8),intent(in)::  c_prvstg,c_sprvstg

! Declare local variables
  integer(i_kind) m
  character(8)  ch8

  gustqm=9
  if (listexist) then
     do m=1,nprov
!       if (trim(c_prvstg//c_sprvstg) == trim(cprovider(m))) then
        if (c_prvstg(1:8) == cprovider(m)(1:8) .and. (c_sprvstg(1:8) == cprovider(m)(9:16)  &
                                               .or. cprovider(m)(9:16) == 'allsprvs') ) then
           gustqm=0
           exit
         endif
     enddo
  endif
  if (listexist2) then
     do m=1,nsta_mesowind_use
        if (c_station_id(1:5) == csta_winduse(m)(1:5)) then
           gustqm=0
           exit
         endif
     enddo
  endif

  if(wlistexist ) then
     do m=1,nwrjs
        ch8(1:8)=w_rjlist(m)(1:8)
        nlen=len_trim(ch8)
        if ((trim(c_station_id) == trim(ch8)) .or. &
            ((kx==288.or.kx==295).and.c_station_id(1:nlen)==ch8(1:nlen))) then
            if (gustqm==0) then
               gustqm=3
            else
               gustqm=15
            end if
            exit
        endif
     enddo
  endif
end subroutine get_gustqm


subroutine readin_rjlists(clistname,fexist,clist,ndim,ncount)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    readin_rjlists
!   prgmmr:
!
! abstract: read in accept or reject list
!
! program history log:
!   2012-17-02  pondeca
!
!   input argument list:
!   clistname
!   ndim
!
!   output argument list:
!   clist
!   ncount
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: ndim
  character(80),intent(in):: clistname

  integer(i_kind),intent(out):: ncount
  character(*),intent(out):: clist(ndim)
  logical,intent(out)::fexist

! Declare local variables
  integer(i_kind) meso_unit,n,m
  character(80) cstring

  data meso_unit / 20 /
!**************************************************************************
 ncount=0

 inquire(file=trim(clistname),exist=fexist)
 if(fexist) then
    open (meso_unit,file=trim(clistname),form='formatted')
    do m=1,3
       read(meso_unit,*,end=131) cstring
    enddo
    n=0
130 continue
    n=n+1
    read(meso_unit,*,end=131) clist(n)
    goto 130
131 continue
    ncount=n-1
    close(meso_unit)
 endif
end subroutine readin_rjlists


subroutine destroy_rjlists
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_rjlists
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-10-01  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  deallocate(cprovider)
  deallocate(w_rjlist)
  deallocate(t_rjlist)
  deallocate(p_rjlist)
  deallocate(q_rjlist)
  deallocate(csta_winduse)
  deallocate(t_day_rjlist)
  deallocate(t_night_rjlist)
  deallocate(q_day_rjlist)
  deallocate(q_night_rjlist)
  if(wbinlistexist) deallocate(nwbaccpts)
  if(wbinlistexist) deallocate(csta_windbin)
end subroutine destroy_rjlists


subroutine get_sunangle(idate,dtime,dlon,dlat,sunangle) 

  use constants, only: deg2rad
  implicit none

  integer(i_kind),intent(in)::idate
  real(r_kind),intent(in)::dtime,dlon,dlat
  real(r_single),intent(out)::sunangle

  real(r_single),parameter:: s24 = 24._r_single
  real(r_single),parameter:: s180 = 180._r_single
  real(r_single),parameter:: s360 = 360._r_single

  integer(i_kind) iyear,imonth,iday,ihour
  integer(i_kind) iw3jdn
  integer(i_kind) jday2,iyear3,imonth3,iday3,idaywk3,idayyr3
  integer(i_kind) ndays
  real(r_single) rlat,rlon
  real(r_single) time
  character(10) date

  write(date,'( i10)') idate
  read (date,'(i4,3i2)') iyear,imonth,iday,ihour
  jday2=iw3jdn(iyear,imonth,iday)

  time=real(ihour,kind=r_single)+real(dtime,kind=r_single)

  if (time.gt.s24) then
     time = time-s24
     jday2=jday2+1
   elseif (time.lt.0) then
     time=time+s24
     jday2=jday2-1
  endif

  call w3fs26(jday2,iyear3,imonth3,iday3,idaywk3,idayyr3)
  !account for leap year
  if(mod(iyear3,4).eq.0) then
     ndays=366
  else
     ndays=365
  endif

  rlon=real(dlon/deg2rad,kind=r_single) ; if (rlon > s180) rlon=rlon-s360
  rlat=real(dlat/deg2rad,kind=r_single)

  call calcsun(idayyr3,ndays,time,rlat,rlon,sunangle)

end subroutine get_sunangle

subroutine get_wbinid(udbl,vdbl,nbins,ibin)

  use constants, only: zero

  implicit none

  integer(i_kind),intent(in   ):: nbins
  real(r_double) ,intent(in   ):: udbl,vdbl
  integer(i_kind),intent(out  ):: ibin

! Declare local variables
  integer(i_kind) n
  real(r_kind) binwidth
  real(r_kind) ue,ve,wdir

  real(r_kind),parameter::r360=360._r_kind

  ue=real(udbl,kind=r_kind)
  ve=real(vdbl,kind=r_kind)

  binwidth=r360/real(max(1,nbins),kind=r_kind)

  call getwdir(ue,ve,wdir)

  if (wdir==zero .or. wdir==r360) then  
     ibin=nbins
   else
     do n=1,nbins
        if ( wdir >= float(n-1)*binwidth .and. wdir < float(n)*binwidth ) then 
            ibin=n
            exit
         endif
      enddo
   endif

end subroutine get_wbinid

end module sfcobsqc

subroutine calcsun (idayr,idaysy,baltm,xlon,ylat,solar)

  implicit none

  integer(4),intent(in)::idayr,idaysy
  real(4),intent(in)::baltm,xlon,ylat
  real(4),intent(out)::solar
  real(4)::dangl,sdgl,cdgl,tsnoon,afy,safy,cafy,ssod,alon,sha,rlat

  dangl = 6.2831853 * (real(idayr) - 79.)/real(idaysy)
  sdgl = sin(dangl)
  cdgl = cos(dangl)
  !solar noon in utc
  tsnoon = -.030*sdgl-.120*cdgl+.330*sdgl*cdgl+.0016*sdgl**2-.0008
  !afy = angular fraction of year
  afy=6.2831853*(real(idayr)-1. + (baltm/24.))/real(idaysy)
  safy=sin(afy)
  cafy=cos(afy)
  !ssod = sin of dolar declination
  ssod = 0.3978492*sin(4.88578+afy+(.033420*safy)-(0.001388*cafy)+(.000696*safy*cafy)+(.000028*(safy**2-cafy**2)))
  alon=mod(720.+360.-xlon,360.)
  !sha = solar hour angle
  sha=0.0174532925*((15.*(tsnoon+baltm+36.))-alon)
  rlat=0.0174532925*ylat
  solar=57.29578*asin((ssod*sin(rlat))+sqrt(1.0-ssod**2)*cos(rlat)*cos(sha))
  return

end subroutine calcsun
