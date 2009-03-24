!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_sitmpl - satinfo si template file IO
!
! !DESCRIPTION:
!
! !INTERFACE:

    module m_sitmpl
      implicit none
      private	! except

      public :: sitmpl_get
      public :: sitmpl_put
      public :: sitmpl_clean

    interface sitmpl_get; module procedure get_; end interface
    interface sitmpl_put; module procedure put_; end interface
    interface sitmpl_clean; module procedure clean_; end interface

! !REVISION HISTORY:
! 	08Jun07	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='m_sitmpl'

contains
subroutine get_(fname,jpch,nusis,nusat,nuchan,iuse_rad, &
                varch,ermax_rad,b_rad,pg_rad,polar,vern,append)
!--  Read in satinfo template file here
  use satinfo_util,only : alloc,realloc
  use satinfo_util,only : luavail,stdout
  use satinfo_util,only : die,perr,tell
  implicit none
  character(len=*),intent(in) :: fname	! input satinfo.rc.tmpl
	! satinfo table
  integer,intent(out) :: jpch		    ! entry count
  character(len=*),pointer,dimension(:) :: nusis ! Sensor-Instr.-Satil.
  integer,pointer,dimension(:) :: nusat     ! old sensor_sat key
  integer,pointer,dimension(:) :: nuchan    ! satellite channel
  integer,pointer,dimension(:) :: iuse_rad  ! channel in-use flag
  real   ,pointer,dimension(:) :: varch     ! variance for each channel
  real   ,pointer,dimension(:) :: ermax_rad ! error maximum (qc)
  real   ,pointer,dimension(:) :: b_rad     ! variational b value
  real   ,pointer,dimension(:) :: pg_rad    ! variational pg value
  real   ,pointer,dimension(:) :: polar     ! polar for each channel
  integer,intent(out) :: vern	! version of the input template file format
  logical,optional,intent(in) :: append
  
  character(len=*),parameter :: myname_=myname//"::get_"
  character(len=*),parameter :: proc_=myname_//'(): '

  integer :: lu,ios,j,ir
  character(len=512) :: line
  character(len=1) :: key
  logical :: append_,skipline
  append_=.false.; if(present(append)) append_=append

  vern=-1
!! Open ...
  lu=luavail()
  open(lu,file=fname,form='formatted',status='old',iostat=ios)
    if (ios /= 0) call die(myname_, &
      'open("'//trim(fname)//'") for input, iostat =',ios)

  call tell(myname_,'reading "'//trim(fname)//'" for input')

  if(.not.append_) then
    call alloc(nusis    ,jpch)
    call alloc(nusat    ,jpch)
    call alloc(nuchan   ,jpch)
    call alloc(iuse_rad ,jpch)
    call alloc(varch    ,jpch)
    call alloc(polar    ,jpch)
    call alloc(ermax_rad,jpch)
    call alloc(b_rad    ,jpch)
    call alloc(pg_rad   ,jpch)
  endif
  j=jpch

	   nusis(j+1:)='.undef.'
	   nusat(j+1:)=HUGE(j)
	  nuchan(j+1:)=HUGE(j)
	iuse_rad(j+1:)=HUGE(j)
	   varch(j+1:)=HUGE(1.)
	   polar(j+1:)=HUGE(1.)
       ermax_rad(j+1:)=HUGE(1.)
	   b_rad(j+1:)=HUGE(1.)
          pg_rad(j+1:)=HUGE(1.)

	ir=0
	read(lu,'(a)',iostat=ios) line
	do while(ios==0)
	  ir=ir+1

          call realloc(nusis    ,j,incr=1)
          call realloc(nusat    ,j,incr=1)
          call realloc(nuchan   ,j,incr=1)
          call realloc(iuse_rad ,j,incr=1)
          call realloc(varch    ,j,incr=1)
          call realloc(polar    ,j,incr=1)
          call realloc(ermax_rad,j,incr=1)
          call realloc(b_rad    ,j,incr=1)
          call realloc(pg_rad   ,j,incr=1)

	  key='?'
	  skipline=.false.
	  read(line,*,iostat=ios) key
	  select case(key(1:1))
	  case('?')
	    call die(myname_,'cann''t read, rec # =',ir)

	  case('!')
	    skipline=.true.

	  case('0':'9')		! in the "old" format
	    if(vern==-1) vern=1
	    j=j+1
            read(line,*,iostat=ios) nusat(j),nuchan(j),iuse_rad(j),&
	      varch(j),polar(j),ermax_rad(j),b_rad(j),pg_rad(j)

	    if( ios/=0 .and.  nusat(j)/=HUGE(j)  .and. &
	                     nuchan(j)/=HUGE(j)  .and. &
	                   iuse_rad(j)/=HUGE(j)  .and. &
	                      varch(j)/=HUGE(1.) .and. &
	                      polar(j)/=HUGE(1.) .and. &
	                  ermax_rad(j)/=HUGE(1.) .and. &
		              b_rad(j)/=HUGE(1.) ) then
		! repair from the input error
	      pg_rad(j)=b_rad(j)
	      b_rad(j)=ermax_rad(j)
	      ermax_rad(j)=polar(j)
	      polar(j)=HUGE(1.)
	      ios=0
	    endif
	    nusis(j)='.undef.'
	    call sat2sis_(nusat(j),nusis(j))

	  case default		! in the "new" format
	    if(vern==-1) vern=2
	    j=j+1
	    polar(j)=HUGE(1.)
            read(line,*,iostat=ios) nusis(j),nuchan(j),iuse_rad(j),&
	      varch(j),ermax_rad(j),b_rad(j),pg_rad(j)

	  	! If this record is an expected end-of-table mark,
		! end the read loop.
	    if( ios/=0 .or. nusis(j)=='sensor'.or.nusis(j)=='sat' ) exit
	    nusat(j)=-1
	  end select

	  	! If the input line contains unexpected values,
		! which is often the result of a failed "listed-
		! directed" read (i.e. fmt=*), echo the input
		! then die().
	  if(.not.skipline) then
	    if(nuchan(j)==HUGE(j)    .or. &
	     iuse_rad(j)==HUGE(j)    .or. &
	        varch(j)==HUGE(1.)   .or. &
            ermax_rad(j)==HUGE(1.)   .or. &
	        b_rad(j)==HUGE(1.)   .or. &
               pg_rad(j)==HUGE(1.)   ) then

	      write(stdout,*) nusis(j),nusat(j),nuchan(j),iuse_rad(j), &
	        varch(j),polar(j),ermax_rad(j),b_rad(j),pg_rad(j)
	      call perr(myname_,'"'//trim(line)//'"')
	      call die(myname_,'unexpected satinfo entry at rec # =',j)
	    endif

	  	! Count in the record as a good table entry, and
		! set the use_rad flag to _off_.

	    jpch=j
	    iuse_rad(j)=-1
	  endif

		! Read the next record
	  read(lu,'(a)',iostat=ios) line
        enddo
        close(lu)

	call tell(myname_,'number of channels, jpch =',jpch)

    if(jpch==0) call die(myname_, &
      'no coefficient found in "'//trim(fname)//'", jpch =',jpch)
end subroutine get_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: put_ - write out satinfo table for GSI
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine put_(fname,jpch,nusis,nusat,nuchan,iuse_rad, &
                    varch,ermax_rad,b_rad,pg_rad,polar,vern,nymd,nhms)
      use satinfo_util,only : luavail
      use satinfo_util,only : die,tell
      implicit none
      character(len=*),intent(in) :: fname      ! output satinfo.txt
                                                ! satinfo table
      integer,intent(in) :: jpch		! entry count
      character(len=*),dimension(:),intent(in) :: nusis ! Sensor-Instr.-Satil.
      integer,dimension(:),intent(in) :: nusat     ! old sensor_sat key
      integer,dimension(:),intent(in) :: nuchan    ! satellite channel
      integer,dimension(:),intent(in) :: iuse_rad  ! channel in-use flag
      real   ,dimension(:),intent(in) :: varch     ! variance for each channel
      real   ,dimension(:),intent(in) :: ermax_rad ! error maximum (qc)
      real   ,dimension(:),intent(in) :: b_rad     ! variational b value
      real   ,dimension(:),intent(in) :: pg_rad    ! variational pg value
      real   ,dimension(:),intent(in) :: polar     ! _polar_ for each channel
      integer,intent(in) :: vern ! vertion of the output template file format
      integer,intent(in) :: nymd,nhms

! !REVISION HISTORY:
! 	08Jun07	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::put_'
  integer :: lu,ios,j,l
  integer :: nusatj
  character(len=max(20,len(nusis))) :: nusisj

        ! Modify original satinfo file according to the satinfo_base.rc
        !===============================================================
        
  call tell(myname_,'writing "'//trim(fname)//'" for output')

	lu=luavail()
        open(lu,file=fname,form='formatted',status='unknown',iostat=ios)
	  if(ios/=0) call die(myname_, &
	    'open("'//trim(fname)//'") for output, iostat =',ios)

        do j = 1,jpch
	  nusisj=nusis(j)
	  nusatj=nusat(j)
	  !!if(nusatj>0) call sat2sis_(nusatj,nusisj)

	  select case(vern)
	  case(1)
            write(lu,115,iostat=ios) nusatj,nuchan(j),iuse_rad(j),varch(j), &
                                 polar(j),ermax_rad(j),b_rad(j),pg_rad(j) 
	    if(ios/=0) call die(myname_, &
	      'write-115("'//trim(fname)//'") for output, iostat =',ios)

	  case(2)
	    if(nusatj==-1) then
	      l=max(18,len_trim(nusisj))
              write(lu,110,iostat=ios) nusisj(1:l),nuchan(j),iuse_rad(j),varch(j), &
                                 ermax_rad(j),b_rad(j),pg_rad(j) 
	      if(ios/=0) call die(myname_, &
	        'write-110("'//trim(fname)//'") for output, iostat =',ios)
	    else
              write(lu,114,iostat=ios) nusatj,nuchan(j),iuse_rad(j),varch(j), &
                                 ermax_rad(j),b_rad(j),pg_rad(j) 
	      if(ios/=0) call die(myname_, &
	        'write-114("'//trim(fname)//'") for output, iostat =',ios)
	    endif
	  endselect
	  
        enddo
	select case(vern)
	case(1)
          write(lu,'(1x,a,i5,a,i8.8,1x,i6.6,a)') &
	    '  sat chan iuse   err   pol   ermax  var_b var_pg # ', &
	    jpch,' total (',nymd,nhms,')'
	case(2)
          write(lu,'(a,i5,a,i8.8,1x,i6.6,a)') &
	    '!sensor/instr/sat   chan iuse  error  ermax  var_b  var_pg # ', &
	            jpch, ' total (',nymd,nhms,')'
	endselect
        close(lu)

110     format(1x,a,i5,i5,f7.3,f7.3,f7.2,f8.3) 
114     format(i6,i5,i5,f7.3,f7.3,f7.2,f8.3) 
115     format(i6,i5,i5,f7.3,f7.3,f7.3,f7.2,f8.3) 
111     format(' Channel Index :',/,30(10i6/)) 
112     format(' Channel Number:',/,30(10i6/)) 
113     format(' Sat/Inst ID   :',/,30(10i6/)) 
end subroutine put_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: clean_ - clean pointers
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine clean_(jpch,nusis,nusat,nuchan,iuse_rad, &
                      varch,ermax_rad,b_rad,pg_rad,polar)
      use satinfo_util,only : dealloc
      implicit none
      integer,intent(out) :: jpch		    ! entry count
      character(len=*),pointer,dimension(:) :: nusis ! Sensor-Instr.-Satil.
      integer,pointer,dimension(:) :: nusat     ! old sensor_sat key
      integer,pointer,dimension(:) :: nuchan    ! satellite channel
      integer,pointer,dimension(:) :: iuse_rad  ! channel in-use flag
      real   ,pointer,dimension(:) :: varch     ! variance for each channel
      real   ,pointer,dimension(:) :: ermax_rad ! error maximum (qc)
      real   ,pointer,dimension(:) :: b_rad     ! variational b value
      real   ,pointer,dimension(:) :: pg_rad    ! variational pg value
      real   ,pointer,dimension(:) :: polar     ! ... for each channel

! !REVISION HISTORY:
! 	08Jun07	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::clean_'
  integer :: j

  j=jpch
  call dealloc(nusis    ,j)
  call dealloc(nusat    ,j)
  call dealloc(nuchan   ,j)
  call dealloc(iuse_rad ,j)
  call dealloc(varch    ,j)
  call dealloc(polar    ,j)
  call dealloc(ermax_rad,j)
  call dealloc(b_rad    ,j)
  call dealloc(pg_rad   ,j)
  jpch=-1

end subroutine clean_
subroutine sat2sis_(nusat,sisname)
!-- map old numerical nusat values to new sis values.
  implicit none
  integer,intent(in) :: nusat
  character(len=*),intent(out) :: sisname

  integer :: isat

select case(nusat)
case(5:12,14)
  isat=nusat
  write(sisname,'(a,i2.2)') 'hirs2_n',isat
  !nusat=-1
case(15:17)
  isat=nusat
  write(sisname,'(a,i2.2)') 'hirs3_n',isat
  !nusat=-1
case(18)
  isat=nusat
  write(sisname,'(a,i2.2)') 'hirs4_n',isat
  !nusat=-1
case(49)
  isat=nusat
  sisname='airs281SUBSET_aqua'
  !nusat=-1
case(58,60,62)
  isat=nusat-50
  write(sisname,'(a,i2.2)') 'sndr_g',isat
  !nusat=-1
case(205:212,214)
  isat=nusat-200
  write(sisname,'(a,i2.2)') 'msu_n',isat
  !nusat=-1
case(258,260,262)
  isat=nusat-250
  write(sisname,'(a,i2.2)') 'imgr_g',isat
  !nusat=-1
case(305:312,314)
  isat=nusat-300
  write(sisname,'(a,i2.2)') 'ssu_n',isat
  !nusat=-1
case(315:317)
  isat=nusat-300
  write(sisname,'(a,i2.2)') 'amsua_n',isat
  !nusat=-1
case(318)
  isat=nusat-300
  write(sisname,'(a,i2.2)') 'amsua_n',isat
  !nusat=-1
case(349)
  isat=nusat-300
  sisname='amsua_aqua'
  !nusat=-1
case(415:417)
  isat=nusat-400
  write(sisname,'(a,i2.2)') 'amsub_n',isat
  !nusat=-1
case(418)
  isat=nusat-400
  write(sisname,'(a,i2.2)') 'mhs_n',isat
  !nusat=-1
case(449)
  isat=nusat-400
  sisname='hsb_aqua'
  !nusat=-1
case(516)
  isat=nusat-500
  write(sisname,'(a,i2.2)') 'ssmis_f16'
  !nusat=-1
case(549)
  isat=nusat-500
  sisname='amsre_aqua'
  !nusat=-1
case(616:618)
  isat=nusat-600
  write(sisname,'(a,i2.2)') 'avhrr3_n',isat
  !nusat=-1
case(708,710,711,713:715)
  isat=nusat-700
  write(sisname,'(a,i2.2)') 'ssmi_f',isat
  !nusat=-1
case default
  isat=nusat
  write(sisname,'(a,i3.3)') 'unknown_',isat
  !nusat=-1
end select
end subroutine sat2sis_
end module m_sitmpl
