subroutine gesinfo(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  gesinfo                  get information from model guess files
!   prgmmr: treadon          org: np23                date: 2006-01-10
!
! abstract: This subroutine gets date/time, vertical coordinate, and other
!           information from model guess file(s)
!
! program history log:
!   2006-01-10  treadon
!   2006-04-14  treadon - remove sigi,sigl; add ntracer,ncloud,ck5
!
!   input argument list:
!     mype - mpi task id
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind,r_kind
  use obsmod, only: iadate
  use mpimod, only: mpi_comm_world,npe
  use gridmod, only: hybrid,ak5,bk5,ck5,&
       regional,nhr_assimilation,nsig,regional_fhr,regional_time,&
       wrf_nmm_regional,wrf_mass_regional,twodvar_regional,&
       gmao_intfc,ntracer,ncloud
  use specmod, only: jcap
  use sigio_module, only: sigio_head,sigio_srhead,sigio_sclose,&
       sigio_sropen
  use m_fvAnaGrid,only : fvAnaGrid_setup,fvAnaGrid_allgetlist
  use constants, only: izero,zero

  implicit none

! Declare local parameters
  integer(i_kind),parameter:: lunges=11
  real(r_kind),parameter::  zero_001=0.001_r_kind


! Declare passed variables
  integer(i_kind),intent(in):: mype


! Declare local variables
  logical fexist
  character(6) filename

  integer(i_kind) jcapg,nsigg,myper,iyr,ihourg,i
  integer(i_kind) ndatppe,mypein,ithinx,iret
  integer(i_kind),dimension(4):: idate4
  integer(i_kind),dimension(8):: ida,jda
  integer(i_kind) :: imo,idy,ihr

  real(r_kind) hourg,ps0
  real(r_kind),dimension(5):: fha

  type(sigio_head):: gfsatm_head
  

!---------------------------------------------------------------------
! Get guess date and vertical coordinate structure from atmospheric
! guess file

  mypein=npe/2

! Handle non-GMAO interface (ie, NCEP interface)
  if(.not.gmao_intfc) then
     write(filename,'("sigf",i2.2)')nhr_assimilation
     inquire(file=filename,exist=fexist)
     if(.not.fexist) then
        write(6,*)' GESINFO:  GUESS FILE NOT AVAILABLE: PROGRAM STOPS'
        call stop2(99)
        stop
     end if
     if(regional) then
        idate4(1)=regional_time(4)  !  hour
        idate4(2)=regional_time(2)  !  month
        idate4(3)=regional_time(3)  !  day
        idate4(4)=regional_time(1)  !  year
        hourg=regional_fhr          !  fcst hour
        nsigg=nsig
        jcapg=0

     else
        call sigio_sropen(lunges,filename,iret)
        call sigio_srhead(lunges,gfsatm_head,iret)
        
!       Load vertical coordinate structure
        if (hybrid) then
           do i = 1,nsig+1
              ak5(i)=gfsatm_head%ak(i)*zero_001
              bk5(i)=gfsatm_head%bk(i)
              ck5(i)=zero
           end do
        else
           do i=1,nsig+1
              ak5(i)=zero
              bk5(i)=gfsatm_head%si(i)
              ck5(i)=zero
           end do
        end if

        hourg    = gfsatm_head%fhour
        idate4(1)= gfsatm_head%idate(1)
        idate4(2)= gfsatm_head%idate(2)
        idate4(3)= gfsatm_head%idate(3)
        idate4(4)= gfsatm_head%idate(4)
        jcapg    = gfsatm_head%jcap
        nsigg    = gfsatm_head%levs
        ntracer  = gfsatm_head%ntrac
        ncloud   = gfsatm_head%ncldt

        call sigio_sclose(lunges,iret)
     end if

     ihourg=hourg
     if(mype==mypein) write(6,*)'GESINFO:  Guess resolution is ',jcapg,nsigg
     if ((jcapg/=jcap.and..not.regional) .or. nsigg/=nsig) then
        write(6,*)'GESINFO:  ***ERROR*** guess res. inconsistent with namelist'
        write(6,*)'      guess jcap,nsig=',jcapg,nsigg
        write(6,*)'   namelist jcap,nsig=',jcap,nsig
        call stop2(85)
     endif
     
!    Compute analysis time from guess date and forecast length.
     iyr=idate4(4)
     if(iyr>=izero.and.iyr<=99) then
        if(iyr>51) then
           iyr=iyr+1900
        else
           iyr=iyr+2000
        end if
     end if
     fha=zero; ida=0; jda=0
     fha(2)=ihourg    ! relative time interval in hours
     ida(1)=iyr       ! year
     ida(2)=idate4(2) ! month
     ida(3)=idate4(3) ! day
     ida(4)=0         ! time zone
     ida(5)=idate4(1) ! hour
     call w3movdat(fha,ida,jda)
     iadate(1)=jda(1) ! year
     iadate(2)=jda(2) ! mon
     iadate(3)=jda(3) ! day
     iadate(4)=jda(5) ! hour
     iadate(5)=0      ! minute

!    Compute grid latitude, longitude, factors, and weights.
     call gengrid_vars

!    Get information about date/time and number of guess files
     if (regional) then
        if(wrf_nmm_regional) then
           call read_wrf_nmm_files(mype)
        else if(wrf_mass_regional) then
           call read_wrf_mass_files(mype)
        else if(twodvar_regional) then
           call read_2d_files(mype)
        end if
     else
        call read_files(mype)
     endif


! Handle GMAO inferface
  else ! if (gmao_intfc) .. set/get FV date-time-grid information

!    For the given time tag, nhr_assimilation, read input guess
!    data header to generate required grid specifications for the
!    GSI analysis grid data.
     call fvAnaGrid_setup(nhr_assimilation,iyr,imo,idy,ihr,ihourg, &
          MPI_comm_world)

!    hourg is for information output; while ihourg has no use.
     hourg =ihourg/10000.
     ihourg= hourg

!    Move date-time backword by nhr_assimilation hours

     fha(:) = 0.                        ! set 1:5
     fha(2) = - hourg           ! reset 2
     jda(1:8) = (/iyr,imo,idy,0,ihr,0,0,0/)
     call w3movdat(fha,jda,ida)

!    hourg,idate4(:) seems for information output only.
     idate4(1:4)=(/ida(5),ida(2),ida(3),ida(1)/)

!    Move date-time forword by nhr_assimilation hours
     fha(2)=-fha(2)
     call w3movdat(fha,ida,jda)

!    iadate(:) is for obsmod.
     iadate(1:5)=(/jda(1),jda(2),jda(3),jda(5),0/)

!    Get information about date/time and number of guess files
     call fvAnaGrid_allgetlist(MPI_comm_world)

  endif

  if(mype==mypein) then
     write(6,*)'GESINFO:  Guess date is ',idate4,hourg
     write(6,*)'GESINFO:  Analysis date is ',iadate
  endif

  return
end subroutine gesinfo
