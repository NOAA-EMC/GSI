module oneobmod
!$$$   module documentation block
!                .      .    .                                       .
! module:    oneobmod
!   prgmmr: kleist           org: np20                date: 2003-10-20
!
! abstract: module contains everything necessary for running single
!           observation experiments
!
! program history log:
!   2003-10-20  kleist
!   2004-05-13  kleist, documentation
!   2005-04-04  todling, fixed little endian ouput of prepqc file
!   2009-04-28  sienkiewicz - add text output for ozone level obs testing
!
! subroutines included:
!   init_oneobmod
!   oneobmakebufr
!
! variable definitions:
!   def maginnov   - magnitude of innovation for one ob exp
!   def magoberr   - magnitude of observational error for one ob exp
!   def oblat      - observation latitude for one ob exp
!   def oblon      - observation longitude for one ob exp
!   def obhourset  - observation delta time from analysis time for 
!                    one ob exp
!   def obpres     - observation pressure (hPa) or one ob exp
!   def obdattim   - observation date for one ob exp
!   def oneob_type - observation type for one ob exp
!   def oneobtest  - single observation test flag (true=on)
!   def pctswitch  - if true, innovation and error expressed as percentage
!                        of background value
!
!$$$
  use kinds, only: r_kind,i_kind

  real(r_kind) maginnov, magoberr, oblat, oblon,&
    obhourset, obpres
  integer(i_kind) obdattim
  character(10) oneob_type
  logical oneobtest
  logical pctswitch

contains

  subroutine init_oneobmod
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_oneobmod
!   prgmmr: kleist           org: np20                date: 2003-10-20
!
! abstract: initialize defaults for single ob experiment vars
!
! program history log:
!   2003-10-20  kleist
!   2004-05-13  kleist, documentation
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
    use constants, only: zero, one
    implicit none

    oneobtest=.false.
    maginnov=one
    magoberr=one
    oneob_type=' '
    oblat=zero
    oblon=zero
    obpres=1000.0_r_kind
    obdattim=2000010100
    obhourset=zero
    pctswitch=.false.

    return
  end subroutine init_oneobmod

  subroutine oneobmakebufr
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    oneobmakebufr
!   prgmmr: kleist           org: np20                date: 2003-10-20
!
! abstract: create prepbufr file for single ob experiment
!
! program history log:
!   2003-10-20  kleist
!   2004-05-13  kleist  documentation
!   2006-04-06  middlecoff - changed lumk from 52 to lendian_in so one-obs prepqc 
!                            file can be read as little endian
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
    use constants, only: zero, one, five
    use gsi_io, only: lendian_in
    use obsmod, only: offtime_data,iadate
    implicit none

    real(r_kind),parameter:: r0_01=0.01_r_kind
    real(r_kind),parameter:: r0_1=0.1_r_kind
    real(r_kind),parameter:: r20=20.0_r_kind
    real(r_kind),parameter:: r100=100.0_r_kind

    integer(i_kind) ludx,nobs,nlev,idate
    character(8) subset,sid(1)
    real(r_kind),dimension(1):: typ
    real(r_kind),dimension(1,1):: qob,tob,zob,uob,vob,cat
    real(r_kind),dimension(1,1):: pqm,qqm,tqm,zqm,wqm
    real(r_kind),dimension(1,1):: poe,qoe,toe,woe
    real(r_kind),dimension(1):: xob,yob,dhr
    real(r_kind),dimension(1,1):: pob
    integer(i_kind) i,n,k,iret
    real(r_kind):: bmiss=10.e10
    real(r_kind) hdr(10),obs(10,255),qms(10,255),err(10,255)
    character(80):: hdrstr='SID XOB YOB DHR TYP'
    character(80):: obsstr='POB QOB TOB ZOB UOB VOB CAT'
    character(80):: qmsstr='PQM QQM TQM ZQM WQM'
    character(80):: errstr='POE QOE TOE WOE'

    if (oneob_type .eq. 'o3lev') then
       call oneobo3lv
       return
    end if
! set values from parameter list
    xob=oblon
    yob=oblat
    dhr=obhourset
    idate=iadate(1)*1000000+iadate(2)*10000+iadate(3)*100+iadate(4)
    write(6,*)idate
    pob=obpres
! set default values for this routine
    ludx=22
    nobs=1
    nlev=1
    subset='ADPUPA'
    sid='SID00001'
    qob=r100
    tob=r20
    zob=zero
    uob=five
    vob=five
    pqm=one
    qqm=one
    tqm=one
    zqm=one
    wqm=one
    offtime_data=.true.
    if (oneob_type.eq.'ps') then
      typ(1)=87.
      cat(1,1)=zero
    else
      typ(1)=20.
      cat(1,1)=one
    endif
! keep errs small so the single ob passes the QC check
    poe=r0_01
    qoe=r0_1
    toe=r0_1
    woe=r0_1

    open(ludx,file='prepobs_prep.bufrtable',action='read')
#if defined(__osf__) || defined(__ia64__) && (__INTEL_COMPILER>799)
    open(lendian_in,file='prepqc',action='write',form='unformatted',convert='little_endian')
#else
    open(lendian_in,file='prepqc',action='write',form='unformatted')
#endif

    call datelen(10)
    call openbf(lendian_in,'OUT',ludx)
    do n=1,nobs
      hdr(1)=transfer(sid(n),hdr(1))
      hdr(2)=xob(n)
      hdr(3)=yob(n)
      hdr(4)=dhr(n)
      hdr(5)=100+typ(n)
      obs=bmiss
      qms=bmiss
      err=bmiss
      do k=1,nlev
        obs(1,k)=pob(k,n)
        obs(2,k)=qob(k,n)
        obs(3,k)=tob(k,n)
        obs(4,k)=zob(k,n)
        obs(7,k)=cat(k,n)
        qms(1,k)=pqm(k,n)
        qms(2,k)=qqm(k,n)
        qms(3,k)=tqm(k,n)
        qms(4,k)=zqm(k,n)
        err(1,k)=poe(k,n)
        err(2,k)=qoe(k,n)
        err(3,k)=toe(k,n)
      enddo
      call openmb(lendian_in,subset,idate)
      call ufbint(lendian_in,hdr,10,   1,iret,hdrstr)
      call ufbint(lendian_in,obs,10,nlev,iret,obsstr)
      call ufbint(lendian_in,qms,10,nlev,iret,qmsstr)
      call ufbint(lendian_in,err,10,nlev,iret,errstr)
      call writsb(lendian_in)
      hdr(1)=transfer(sid(n),hdr(1))
      hdr(2)=xob(n)
      hdr(3)=yob(n)
      hdr(4)=dhr(n)
      hdr(5)=200+typ(n)
      obs=bmiss
      qms=bmiss
      err=bmiss
      do k=1,nlev
        obs(1,k)=pob(k,n)
        obs(5,k)=uob(k,n)
        obs(6,k)=vob(k,n)
        obs(7,k)=cat(k,n)
        qms(1,k)=pqm(k,n)
        qms(5,k)=wqm(k,n)
        err(1,k)=poe(k,n)
        err(4,k)=woe(k,n)
      enddo
      call openmb(lendian_in,subset,idate)
      call ufbint(lendian_in,hdr,10,   1,iret,hdrstr)
      call ufbint(lendian_in,obs,10,nlev,iret,obsstr)
      call ufbint(lendian_in,qms,10,nlev,iret,qmsstr)
      call ufbint(lendian_in,err,10,nlev,iret,errstr)
      call writsb(lendian_in)
    enddo
    call closbf(lendian_in)

    return
  end subroutine oneobmakebufr

  subroutine oneobo3lv
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    oneobmls
!
! abstract: create ozone level text file for single ob experiment
!
! program history log:
!   2007-09-11  Sienkiewicz - extend to create MLS text file on request
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ?
!
!$$$
    implicit none

    integer(i_kind) lumk                          ! output unit
    integer(i_kind) ilev, isnd
    integer(i_kind) ildat(8),jldat(8)             ! "local" date/time
    real(r_kind)    rsec,rlnc(5)
    real(r_kind)    ppmv

2   format(i5,4i3,f6.2,i7,i5,f10.4,f11.4,e16.7,i7,i5,g16.7,g15.7,f6.3)

    lumk = 22
    ilev = 1                  ! ilev > 24 is passive
    isnd = 1
    ppmv = 1.0                ! dummy value 

!    obdattim=2000010100

    rlnc = 0.0
    rlnc(2) = obhourset
    ildat(1) = obdattim  / 1000000            ! year
    ildat(2) = mod(obdattim,1000000)/10000    ! month
    ildat(3) = mod(obdattim,10000)/100        ! day
    ildat(4) = 0
    ildat(5) = mod(obdattim,100)              ! hour

    ildat(6:8) = 0                            ! (no minute/sec in obdattim)

    call w3movdat(rlnc,ildat,jldat)

    rsec = jldat(7)+jldat(8)*1.e-3_r_kind

! open data file for output.  for oneobtype gsimain sets the dfile(1) 
! to be prepqc
    open(unit=lumk,file='prepqc',form='formatted')

    write(lumk,2) jldat(1),jldat(2),jldat(3),jldat(5),jldat(6),rsec,isnd,&
         ilev,oblat,oblon,ppmv,isnd,isnd,magoberr,obpres,magoberr
    close(lumk)

    return

  end subroutine oneobo3lv

end module oneobmod
