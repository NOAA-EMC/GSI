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
!   2007-03-16  moorthi - replace gfsatm_head%ak,%bk with %vcoord
!   2007-05-07  treadon - add gfsio interface
!   2007-05-08  kleist  - add capability to handle fully generalized coordinate
!   2008-06-04  safford - rm unused use one
!   2009-01-07  todling - add logics to determine begin/end of analysis
!   2009-01-28  todling - remove original GMAO interface
!   2009-10-09  wu      - replace nhr_offset with min_offset since it's 1.5 hr for regional
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
  use obsmod, only: iadate,ianldate,time_offset
  use gsi_4dvar, only: ibdate, iedate, iadatebgn, iadateend, iwinbgn,time_4dvar
  use gsi_4dvar, only: nhr_assimilation,min_offset
  use mpimod, only: npe
  use gridmod, only: idvc5,ak5,bk5,ck5,tref5,&
       regional,nsig,regional_fhr,regional_time,&
       wrf_nmm_regional,wrf_mass_regional,twodvar_regional,nems_nmmb_regional,&
       ntracer,ncloud,ncep_sigio,nlat,nlon,idvm5,&
       ncepgfs_head,ncepgfs_headv,idpsfc5,idthrm5,idsl5,cp5
  use specmod, only: jcap_b
  use sigio_module, only: sigio_head,sigio_srhead,sigio_sclose,&
       sigio_sropen
  use gfsio_module, only: gfsio_gfile,gfsio_open,gfsio_close,&
       gfsio_init,gfsio_finalize,gfsio_getfilehead

  use constants, only: izero,ione,zero,h300,r60

  implicit none

! Declare local parameters
  integer(i_kind),parameter:: lunges=11_i_kind
  real(r_kind),parameter::  zero_001=0.001_r_kind


! Declare passed variables
  integer(i_kind),intent(in):: mype


! Declare local variables

  logical fexist
  character(6) filename
  character(8) filetype

  integer(i_kind) iyr,ihourg,k
  integer(i_kind) mype_out,iret,iret2,intype
  integer(i_kind),dimension(4):: idate4
  integer(i_kind),dimension(8):: ida,jda
  integer(i_kind) :: nmin_an

  real(r_kind) hourg
  real(r_kind),dimension(5):: fha

  type(sigio_head):: sighead
  type(gfsio_gfile) :: gfile
  type(ncepgfs_head):: gfshead
  type(ncepgfs_headv):: gfsheadv



!---------------------------------------------------------------------
! Get guess date and vertical coordinate structure from atmospheric
! guess file

  mype_out=npe/2


! Handle non-GMAO interface (ie, NCEP interface)
     write(filename,'("sigf",i2.2)')nhr_assimilation
     inquire(file=filename,exist=fexist)
     if(.not.fexist) then
        write(6,*)' GESINFO:  GUESS FILE NOT AVAILABLE: PROGRAM STOPS'
        call stop2(99)
        stop
     end if

!    Handle NCEP regional case
     if(regional) then
        idate4(1)=regional_time(4)  !  hour
        idate4(2)=regional_time(2)  !  month
        idate4(3)=regional_time(3)  !  day
        idate4(4)=regional_time(1)  !  year
        hourg=regional_fhr          !  fcst hour

!    Handle NCEP global cases
     else

!       Determine NCEP atmospheric guess file format
        intype = izero
        call sigio_sropen(lunges,filename,iret)
        call sigio_srhead(lunges,sighead,iret2)
        if (iret==izero .and. iret2==izero) then
           intype = ione
        else
           call gfsio_init(iret)
           call gfsio_open(gfile,trim(filename),'read',iret)
           call gfsio_getfilehead(gfile,iret=iret2,gtype=filetype)
           if (iret==izero .and. iret2==izero .and. filetype=='GFSIOATM') intype = 2_i_kind
        endif
        if (intype==ione) then
           ncep_sigio=.true.
        elseif (intype==2_i_kind) then
           ncep_sigio=.false.
        else
           write(6,*)' GESINFO:  UNKNOWN FORMAT FOR NCEP ATM GUESS FILE ',filename
           call stop2(99)
           stop
        endif

        if (mype==mype_out) then
           if (ncep_sigio) then
              write(6,*)'GESINFO:  Read NCEP sigio format file, ',filename
           else
              write(6,*)'GESINFO:  Read NCEP gfsio format file, ',filename
           endif
        endif
           
!       Extract information from NCEP atmospheric guess using sigio
        if (ncep_sigio) then

!          Fill structure with NCEP sigio header information
           gfshead%fhour=sighead%fhour
           gfshead%idate=sighead%idate
           gfshead%latb=sighead%latb
           gfshead%lonb=sighead%lonb
           gfshead%levs=sighead%levs
           gfshead%jcap=sighead%jcap
           gfshead%ntrac=sighead%ntrac
           gfshead%idvc=sighead%idvc
           gfshead%idvm=sighead%idvm
           gfshead%idsl=sighead%idsl
           gfshead%ncldt=sighead%ncldt
           gfshead%nvcoord=sighead%nvcoord

           allocate(gfsheadv%vcoord(gfshead%levs+ione,gfshead%nvcoord))
           gfsheadv%vcoord=sighead%vcoord

           allocate(gfsheadv%cpi(gfshead%ntrac+ione))
           if (mod(gfshead%idvm/10,10) == 3_i_kind) then
              do k=1,gfshead%ntrac+ione
                 gfsheadv%cpi(k)=sighead%cpi(k)
              end do
           else
              do k=1,gfshead%ntrac+ione
                 gfsheadv%cpi(k)=zero
              end do
           endif


           call sigio_sclose(lunges,iret)

!          Check for consistency:  jcap, levs
           if (gfshead%jcap/=jcap_b .or. gfshead%levs/=nsig) then
              write(6,*)'GESINFO:  ***ERROR*** sigio (jcap_b,levs)=',&
                   gfshead%jcap,gfshead%levs, ' do not equal ',&
                   ' user (jcap_b,nsig)=',jcap_b,nsig
              call stop2(85)
           endif


!       Extract information from NCEP atmospheric guess using gfsio
        else
           call gfsio_getfilehead(gfile,iret=iret,&
                fhour=gfshead%fhour,&
                idate=gfshead%idate,&
                latb=gfshead%latb,&
                lonb=gfshead%lonb,&
                levs=gfshead%levs,&
                jcap=gfshead%jcap,&
                ntrac=gfshead%ntrac,&
                idvc=gfshead%idvc,&
                idvm=gfshead%idvm,&
                idsl=gfshead%idsl,&
                ncldt=gfshead%ncldt,&
                nvcoord=gfshead%nvcoord)


!          Extract horizontal and vertical coordinate structure
           allocate(gfsheadv%vcoord(gfshead%levs+ione,gfshead%nvcoord))
           call gfsio_getfilehead(gfile,iret=iret,vcoord=gfsheadv%vcoord)

           allocate(gfsheadv%cpi(gfshead%ntrac+ione))
           do k=1,gfshead%ntrac+ione
              gfsheadv%cpi(k)=zero
           end do

           call gfsio_close(gfile,iret)
           call gfsio_finalize()

!          Check for consistency:  jcap, levs, latb,lonb
           if (gfshead%latb+2_i_kind/=nlat .or. gfshead%lonb/=nlon .or. &
                gfshead%levs/=nsig ) then
              write(6,*)'GESINFO:  ***ERROR*** gfsio (latb+2,lonb,levs)=',&
                   gfshead%latb+2_i_kind,gfshead%lonb,gfshead%levs, ' do not equal ',&
                   ' user (nlat,nlon,nsig)=',nlat,nlon,nsig
              call stop2(85)
           endif

        endif

!       Extract header information
        hourg    = gfshead%fhour
        idate4(1)= gfshead%idate(1)
        idate4(2)= gfshead%idate(2)
        idate4(3)= gfshead%idate(3)
        idate4(4)= gfshead%idate(4)
        ntracer  = gfshead%ntrac
        ncloud   = gfshead%ncldt


!       Load vertical coordinate structure
        idvc5=gfshead%idvc
        idsl5=gfshead%idsl
        do k=1,nsig+ione
           ak5(k)=zero
           bk5(k)=zero
           ck5(k)=zero
        end do

        if (gfshead%nvcoord == ione) then
           do k=1,nsig+ione
              bk5(k) = gfsheadv%vcoord(k,1)
           end do
        elseif (gfshead%nvcoord == 2_i_kind) then
           do k = 1,nsig+ione
              ak5(k) = gfsheadv%vcoord(k,1)*zero_001
              bk5(k) = gfsheadv%vcoord(k,2)
           end do
        elseif (gfshead%nvcoord == 3_i_kind) then
           do k = 1,nsig+ione
              ak5(k) = gfsheadv%vcoord(k,1)*zero_001
              bk5(k) = gfsheadv%vcoord(k,2)
              ck5(k) = gfsheadv%vcoord(k,3)*zero_001
           end do
        else
           write(6,*)'GESINFO:  ***ERROR*** INVALID value for nvcoord=',gfshead%nvcoord
           call stop2(85)
        endif
           
!       Load reference temperature array (used by general coordinate)        
        do k=1,nsig
           tref5(k)=h300
        end do

!       Load surface pressure and thermodynamic variable ids
        idvm5   = gfshead%idvm
        idpsfc5 = mod ( gfshead%idvm,10 )
        idthrm5 = mod ( gfshead%idvm/10,10 )

!       Load specific heat for tracers
        if (allocated(cp5)) deallocate(cp5)
        allocate(cp5(gfshead%ntrac+ione))
        do k=1,gfshead%ntrac+ione
           cp5(k)=gfsheadv%cpi(k)
        end do

!       Check for consistency with namelist settings           
        if ((gfshead%jcap/=jcap_b.and..not.regional) .or. gfshead%levs/=nsig) then
           write(6,*)'GESINFO:  ***ERROR*** guess res. inconsistent with namelist'
           write(6,*)'      guess jcap_b,nsig=',gfshead%jcap,gfshead%levs
           write(6,*)'   namelist jcap_b,nsig=',jcap_b,nsig
           call stop2(85)
        endif


!       Echo select header information to stdout
        if(mype==mype_out) then
           write(6,100) gfshead%jcap,gfshead%levs,gfshead%latb,gfshead%lonb,&
                gfshead%ntrac,gfshead%ncldt,idvc5,gfshead%nvcoord,&
                idvm5,idsl5,idpsfc5,idthrm5
100        format('GESINFO:  jcap_b=',i4,', levs=',i3,', latb=',i5,&
                ', lonb=',i5,', ntrac=',i3,', ncldt=',i3,', idvc=',i3,&
                ', nvcoord=',i3,', idvm=',i3,', idsl=',i3,', idpsfc=',i3,&
                ', idthrm=',i3)
           do k=1,nsig
              write(6,110) k,ak5(k),bk5(k),ck5(k),tref5(k)
           end do
           k=nsig+ione
           write(6,110) k,ak5(k),bk5(k),ck5(k)
110        format(3x,'k,ak,bk,ck,tref=',i3,1x,4(g18.12,1x))
        endif



!    End of NCEP global block
     endif


!    Compute grid latitude, longitude, factors, and weights.
     call gengrid_vars



!    Compute analysis time from guess date and forecast length.
     iyr=idate4(4)
     ihourg=hourg
     if(iyr>=izero.and.iyr<=99_i_kind) then
        if(iyr>51_i_kind) then
           iyr=iyr+1900_i_kind
        else
           iyr=iyr+2000_i_kind
        end if
     end if
     fha=zero; ida=izero; jda=izero
     fha(2)=ihourg    ! relative time interval in hours
     ida(1)=iyr       ! year
     ida(2)=idate4(2) ! month
     ida(3)=idate4(3) ! day
     ida(4)=izero     ! time zone
     ida(5)=idate4(1) ! hour
     call w3movdat(fha,ida,jda)
     iadate(1)=jda(1) ! year
     iadate(2)=jda(2) ! mon
     iadate(3)=jda(3) ! day
     iadate(4)=jda(5) ! hour
     iadate(5)=izero  ! minute
     ianldate =jda(1)*1000000+jda(2)*10000+jda(3)*100+jda(5)

!  Determine date and time at start of assimilation window
   ida(:)=izero
   jda(:)=izero
   fha(:)=zero
   fha(2)=-float(int(min_offset/60_i_kind))
   fha(3)=-(min_offset+fha(2)*r60)
   ida(1:3)=iadate(1:3)
   ida(5:6)=iadate(4:5)
   call w3movdat(fha,ida,jda)

   ibdate(1:5)=(/jda(1),jda(2),jda(3),jda(5),jda(6)/)
   iadatebgn=jda(1)*1000000+jda(2)*10000+jda(3)*100+jda(5)

! Set the analysis time - this is output info...
! w3fs21(NCEP-w3) converts analysis time to minutes relative to a fixed date.
   call w3fs21(ibdate,nmin_an)
   iwinbgn = nmin_an

!  Determine date and time at end of assimilation window
   ida(:)=jda(:)
   jda(:)=izero
   fha(:)=zero
   if ( min_offset == izero ) then
        fha(2)=zero
   else
        fha(2)=nhr_assimilation
   endif
   call w3movdat(fha,ida,jda)

   iedate(1:5)=(/jda(1),jda(2),jda(3),jda(5),jda(6)/)
   iadateend=jda(1)*1000000+jda(2)*10000+jda(3)*100+jda(5)

!  Get time offset
   call time_4dvar(ianldate,time_offset)
   if (regional)then
   fha(2)=float(int(min_offset/60_i_kind))
   fha(3)=(min_offset-fha(2)*r60)
   time_offset=time_offset+fha(3)/r60
   endif

!    Get information about date/time and number of guess files
     if (regional) then
        if(wrf_nmm_regional) then
           call read_wrf_nmm_files(mype)
        else if(nems_nmmb_regional) then
           call read_nems_nmmb_files(mype)
        else if(wrf_mass_regional) then
           call read_wrf_mass_files(mype)
        else if(twodvar_regional) then
           call read_2d_files(mype)
        end if
     else
        call read_files(mype)
     endif
     

  if(mype==mype_out) then
     write(6,*)'GESINFO:  Guess date is ',idate4,hourg
     write(6,*)'GESINFO:  Analysis date is ',iadate,ianldate,time_offset
  endif

  return
end subroutine gesinfo
