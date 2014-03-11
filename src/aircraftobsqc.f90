module aircraftobsqc
!$$$ module documentation block
!           .      .    .                                       .
! module:   aircraftobsqc
!   prgmmr: Ming Hu
!
! abstract: contains subroutines for the qc of aircraft obs based
!           on (i) rejectlists for temperature, wind and RH   
!           the code inquires for the 
!           existence of thsese lists in the gsi working directorty 
!           and applies them if present.
!           
!
! program history log:
!   2010-10-28  Hu
!
! subroutines included:
!   sub init_aircraft_rjlists
!   sub get_aircraft_usagerj
!   sub destroy_aircraft_rjlists
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: i_kind,r_kind

  implicit none

  private

  character(8),allocatable,dimension(:,:)::w_aircraft_rjlist
  character(8),allocatable,dimension(:,:)::t_aircraft_rjlist
  character(8),allocatable,dimension(:,:)::q_aircraft_rjlist

  integer(i_kind) nwrjs_aircraft,ntrjs_aircraft,nqrjs_aircraft

  logical listexist_aircraft


! Xue
  integer(i_kind) nt_aircraft
  character(8),allocatable,dimension(:)::t_aircraft_tail
  character(8),allocatable,dimension(:)::t_aircraft_mdcrs
  character(8),allocatable,dimension(:)::t_aircraft_bias_gt_300
  character(8),allocatable,dimension(:)::t_aircraft_bias_le_300

  public read_aircraft_t_bias
  public correct_aircraft_t

  public init_aircraft_rjlists
  public get_aircraft_usagerj
  public destroy_aircraft_rjlists

contains

!!!!!!!!!!!!!==================================xue 
subroutine read_aircraft_t_bias
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_aircraft_t_bias
!   prgmmr:
!
! abstract: read in bias correction values
!           for aircraft T obs
!
! program history log:
!   2013-02-21  Xue
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

  integer(i_kind) aircraft_unit1,m
  character(80) cstring

  integer(i_kind), parameter::nmax=5000_i_kind

  data aircraft_unit1 / 201_i_kind /
!**************************************************************************
  nt_aircraft=0

  allocate(t_aircraft_tail(nmax))
  allocate(t_aircraft_mdcrs(nmax))
  allocate(t_aircraft_bias_gt_300(nmax))
  allocate(t_aircraft_bias_le_300(nmax))

  inquire(file='current_T_bias.txt',exist=listexist_aircraft)
  if(listexist_aircraft) then
    open (aircraft_unit1,file='current_T_bias.txt',form='formatted')
    do m=1,5
       read(aircraft_unit1,*,end=1141)
    enddo
1140 continue
    read(aircraft_unit1,'(a49)',end=1141) cstring

    nt_aircraft=nt_aircraft+1
    t_aircraft_tail(nt_aircraft)=cstring(2:9)
    t_aircraft_mdcrs(nt_aircraft)=cstring(22:29)
    t_aircraft_bias_gt_300(nt_aircraft)=cstring(32:39)
    t_aircraft_bias_le_300(nt_aircraft)=cstring(42:49)
    goto 1140
1141 continue

 endif
 close(aircraft_unit1)

end subroutine read_aircraft_t_bias

subroutine correct_aircraft_t(c_station_id,p_mb,new_t)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    correct_aircraft_t
!   prgmmr:
!
! abstract: do bias corretion for aircraft T obs 
!
! program history log:
!   2013-02-21  Xue 
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

  character(8)   ,intent(in   ) :: c_station_id
  real(r_kind)   ,intent(in) :: p_mb
  real(r_kind)   ,intent(inout) :: new_t

! Declare local variables
  integer(i_kind) m,nlen
  character(8)  ch8,ch8MDCRS
  real(r_kind)  t1,t2

! Declare local parameters
  real(r_kind),parameter:: r6    = 6.0_r_kind
  real(r_kind),parameter:: r450  = 450._r_kind


  if((nt_aircraft > 0) ) then
     do m=1,nt_aircraft
         ch8=t_aircraft_tail(m)
         ch8MDCRS=t_aircraft_mdcrs(m)
         nlen=len_trim(ch8)
         if ((trim(c_station_id) == trim(ch8)).or.(trim(c_station_id) == trim(ch8MDCRS))) then
            read(t_aircraft_bias_gt_300(m),*) t1
            read(t_aircraft_bias_le_300(m),*) t2

            if (p_mb >300) then
               new_t = new_t - t1 
            else
               new_t = new_t - t2
            endif
            exit
         endif
      enddo
   end if

end subroutine correct_aircraft_t
!===================================!!!!!!!!!!!!end xue
subroutine init_aircraft_rjlists
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_aircraft_rjlists
!   prgmmr:
!
! abstract: initialize qc lists 
!
! program history log:
!   2010-10-28  Hu
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
  integer(i_kind) aircraft_unit,m
  character(80) cstring

  integer(i_kind), parameter::nmax=500

  data aircraft_unit / 20 /
!**************************************************************************
  nwrjs_aircraft=0
  ntrjs_aircraft=0
  nqrjs_aircraft=0

  allocate(w_aircraft_rjlist(nmax,2))
  allocate(t_aircraft_rjlist(nmax,2))
  allocate(q_aircraft_rjlist(nmax,2))
!
!==> Read in station names from the reject list for 
!    wind,temperature, and humidity if it exists

 inquire(file='current_bad_aircraft',exist=listexist_aircraft)
 if(listexist_aircraft) then
    open (aircraft_unit,file='current_bad_aircraft',form='formatted')
    do m=1,16
       read(aircraft_unit,*,end=141)
    enddo
140 continue
    read(aircraft_unit,'(a30)',end=141) cstring
    if(cstring(11:11) == 'T') then
       ntrjs_aircraft=ntrjs_aircraft+1
       t_aircraft_rjlist(ntrjs_aircraft,1)=cstring(1:8)
       t_aircraft_rjlist(ntrjs_aircraft,2)=cstring(22:29)
    endif
    if(cstring(13:13) == 'W') then
       nwrjs_aircraft=nwrjs_aircraft+1
       w_aircraft_rjlist(nwrjs_aircraft,1)=cstring(1:8)
       w_aircraft_rjlist(nwrjs_aircraft,2)=cstring(22:29)
    endif
    if(cstring(15:15) == 'R') then
       nqrjs_aircraft=nqrjs_aircraft+1
       q_aircraft_rjlist(nqrjs_aircraft,1)=cstring(1:8)
       q_aircraft_rjlist(nqrjs_aircraft,2)=cstring(22:29)
    endif
    goto 140
141 continue
    print*,'aircraft_rejectlist: T, W, R=', ntrjs_aircraft,nwrjs_aircraft,nqrjs_aircraft
 endif
 close(aircraft_unit)
!
end subroutine init_aircraft_rjlists

subroutine get_aircraft_usagerj(kx,obstype,c_station_id,usage_rj)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_aircraft_usagerj
!   prgmmr:
!
! abstract: determine the usage value of read_prepbufr for aircraft obs. the following
!           is done: (i) if incoming usage value is >=100. then do nothing, since
!           read_prepbufr has already flagged this ob and assigned a specific usage 
!           value to it. (ii) use usage=500. for temperature, moisture, or surface pressure
!           obs which are found in the rejectlist. (iii) 
!
! program history log:
!   2010-10-28  Hu
!
!   input argument list:
!    kx
!    obstype
!    c_station_id
!
!   output argument list:
!    usage_rj
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

  integer(i_kind),intent(in   ) :: kx
  character(10)  ,intent(in   ) :: obstype
  character(8)   ,intent(in   ) :: c_station_id
  real(r_kind)   ,intent(inout) :: usage_rj

! Declare local variables
  integer(i_kind) m,nlen
  character(8)  ch8,ch8MDCRS
  real(r_kind) usage_rj0

! Declare local parameters
  real(r_kind),parameter:: r6    = 6.0_r_kind
  real(r_kind),parameter:: r450  = 450._r_kind

  if (usage_rj >= r6) return

  usage_rj0=usage_rj

  if (kx<190) then  !<==mass obs

     if(obstype=='t' .and. (ntrjs_aircraft > 0) ) then
        do m=1,ntrjs_aircraft
           ch8=t_aircraft_rjlist(m,1)
           ch8MDCRS=t_aircraft_rjlist(m,2)
           nlen=len_trim(ch8)
           if ((trim(c_station_id) == trim(ch8)).or.(trim(c_station_id) == trim(ch8MDCRS))) then
              usage_rj=r450
              exit
           endif
        enddo
     elseif(obstype=='q' .and. (nqrjs_aircraft > 0) ) then
        do m=1,nqrjs_aircraft
           ch8=q_aircraft_rjlist(m,1)
           ch8MDCRS=q_aircraft_rjlist(m,2)
           nlen=len_trim(ch8)
           if ((trim(c_station_id) == trim(ch8)).or.(trim(c_station_id) == trim(ch8MDCRS))) then
              usage_rj=r450
              exit
           endif
        enddo
     end if

  elseif (kx>=190) then !<==wind obs

     if(obstype=='uv' .and. (nwrjs_aircraft > 0) ) then
        do m=1,nwrjs_aircraft
           ch8=w_aircraft_rjlist(m,1)
           ch8MDCRS=w_aircraft_rjlist(m,2)
           nlen=len_trim(ch8)
           if ((trim(c_station_id) == trim(ch8)).or.(trim(c_station_id) == trim(ch8MDCRS))) then
              usage_rj=r450
              exit
           endif
        enddo
     endif

  end if
end subroutine get_aircraft_usagerj

subroutine destroy_aircraft_rjlists
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_aircraft_rjlists
!   prgmmr:
!
! abstract:
!
! program history log:
!   2010-10-28  Hu
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

  deallocate(w_aircraft_rjlist)
  deallocate(t_aircraft_rjlist)
  deallocate(q_aircraft_rjlist)

end subroutine destroy_aircraft_rjlists

end module aircraftobsqc
