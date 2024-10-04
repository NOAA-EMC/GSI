program main
!$$$  main program documentation block
!                .      .    .                                       .
! main program: GLOBAL_ANGUPATE
!   PRGMMR: TREADON           ORG: NP20        DATE: 2005-07-22
!
! abstract:  This program reads radiance diagnostic files, computes
!   the scan angle dependent component of the bias, and then combines
!   it as a weighted average with an input angle dependent file.
!
! program history log:
!   01-01-09  treadon    initial code
!   02-01-12  treadon    major rewrite to accomodate new diagnostic
!                        file format and use mpi
!   2005-07-22 treadon   major rewrite to simplify and clean up
!   2006-03-02 treadon   simplify code and rewrite i/o to handle 
!                        ncep-gsi-2006_03 gsi release format
!   2012-11-08 acollard  Allow number of scan positions to be specified in the
!                        satang file itself.
!
! usage:
!   input files:
!     lnangl   - scan angle dependent bias correction file
!     lndiag   - radiance diagnostic file
!
!   scratch files:
!     lntemp   - contain sample sum and sample size
!
!   output files:
!     lnupdt   - updated scan angle dependent bias correction file
!
!   subprograms called:
!     determine_time_levels - determine number of time levels
!     read_diag_data        - read diagnostic file
!
!     library:
!       mpi, w3
!
!   exit states:
!     cond =   0 - successful run
!          =  98 - inconsistent satellite/sensor identifiers
!          =  99 - problem reading input satang file
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use mpi
  use read_diag
  implicit none

! Declare local parameters
  integer,parameter:: lnangl = 11
  integer,parameter:: lnmlst = 12
  integer,parameter:: lndiag = 21
  integer,parameter:: lntemp = 51 
  integer,parameter:: lnupdt = 52

  integer,parameter:: maxtim = 1500
  integer,parameter:: maxchn = 3000
  integer,parameter:: maxdat = 100

  real,parameter:: zero = 0.0
  real,parameter:: one = 1.0


! Declare local variables
  logical outask,lexist,done,update,restart,retrieval

  character(6) :: word
  character(10):: obstype,platid
  character(11):: dstring
  character(20):: satsens,satsens_id
  character(25):: fstring
  character(15):: string
  character(50):: diag_rad,dname,sname,fname
  character(10),dimension(maxdat):: dtype,dplat
  character(20),dimension(maxdat):: dsis
  character(20),allocatable,dimension(:):: satsensor0,satsensor2,satsensor3

  integer:: ix,ii,iii,ndat,ich,ndatppe
  integer:: nstep = 90 ! Default value which may be over-written by namelist
                       ! and by the satang file itself
  integer:: i,j,mype_out,nsize,n_chan
  integer:: jpch,ierror,npe,mype,jj,ntime,it,ierror_code
  integer:: iyy1,imm1,idd1,ihh1,iyy2,imm2,idd2,ihh2
  integer:: istatus,ispot,iuseqc
  integer:: npred,ipchan
  integer,dimension(maxchn):: io_chan
  integer,dimension(maxtim):: iyy,imm,idd,ihh
  integer,dimension(maxdat):: ipoint
  integer,allocatable,dimension(:):: jchanum0,jchanum2,jchanum3

  real:: wgtang,wgtlap,scan,dtmax,bias,tlap,x,y,dth,ratio,rsize,errinv
  real,allocatable,dimension(:):: tsum,tlap0,tlap1,tlap2,tlap3,tcnt
  real,allocatable,dimension(:,:):: csum,count,c_ang0,c_ang1,c_ang2,c_ang3


! Declare types used for reading satellite data
  type(diag_header_fix_list )             :: header_fix
  type(diag_header_chan_list),allocatable :: header_chan(:)
  type(diag_data_name_list  )             :: data_name
  type(diag_data_fix_list   )             :: data_fix
  type(diag_data_chan_list  ),allocatable :: data_chan(:)
  type(diag_data_extra_list ),allocatable :: data_extra(:,:)


! Declare namelists for user input
  namelist /setup/ jpch,nstep,nsize,wgtang,wgtlap,iuseqc,dtmax,&
       iyy1,imm1,idd1,ihh1,iyy2,imm2,idd2,ihh2,dth,ndat

  namelist /obs_input/ dtype,dplat,dsis

!************************************************************************
! MPI setup
  call mpi_init(ierror)
  call mpi_comm_size(mpi_comm_world,npe,ierror)
  call mpi_comm_rank(mpi_comm_world,mype,ierror)
  mype_out=npe-1

  npred=7               ! number of bias correction predictors
  retrieval=.false.     ! .true. if bisst present

  outask=.false.
  if (mype==mype_out) outask=.true.
  if (outask) call w3tagb('GLOBAL_ANGUPDATE',1999,0232,0055,'NP23')


! Read and echo namelist input
#ifdef ibm_sp
  read(5,setup)
  read(5,obs_input)
#else
  open(unit=lnmlst,file='global_angupdate.namelist')
  read(lnmlst,setup)
  read(lnmlst,obs_input)
  close(lnmlst)
#endif

  rsize=zero
  if (nsize>0) rsize=one/float(nsize)
  if (outask) then
     write(6,setup)
     write(6,*)'nsize,rsize=',nsize,rsize
     do i=1,ndat
        write(6,*) i,dtype(i),dplat(i),dsis(i)
     end do
  endif


! Loop from start to end times to determine how many time levels
  call determine_time_levels(iyy1,imm1,idd1,ihh1,&
       iyy2,imm2,idd2,ihh2,dth,ntime,iyy,imm,idd,ihh,&
       maxtim)
  restart=.false.
  if (ntime>1) restart=.true.
  if (outask) then
     write(6,*)' '
     write(6,*)'Start time:  ',iyy1,imm1,idd1,ihh1
     write(6,*)'End time  :  ',iyy2,imm2,idd2,ihh2
     write(6,*)'Time incr :  ',dth
     write(6,*)'ntime = ',ntime
     do it=1,ntime
        write(6,*)'it,idate=',it,iyy(it),imm(it),idd(it),ihh(it)
     end do
     write(6,*)' '

     if (ntime>1) then
        write(6,*)' '
        write(6,*)'Since ntime=',ntime,' > 1, set restart=',restart,' --> ignore input satang'
        write(6,*)' '
     endif
  endif


! Allocate and initialize data arrays
  allocate(tsum(jpch),tlap1(jpch),tcnt(jpch))

! Read input satang file
  open(lnangl,file='satbias_ang.in',form='formatted')
  read(lnangl,'(a6)') word
  rewind(lnangl)
  if (word == 'nscan=') read(lnangl,'(6x,i8)') nstep
  
  if (nstep <= 0 .OR. nstep > 1000) then
     write(6,*)'GLOBAL_ANGUPDATE: nscan out of range: ',nstep
     stop
  endif

  allocate(csum(nstep,jpch),c_ang1(nstep,jpch))
  allocate(count(nstep,jpch))
  allocate(satsensor0(jpch),jchanum0(jpch),tlap0(jpch),c_ang0(nstep,jpch))
  allocate(satsensor2(jpch),jchanum2(jpch),tlap2(jpch),c_ang2(nstep,jpch))
  allocate(satsensor3(jpch),jchanum3(jpch),tlap3(jpch),c_ang3(nstep,jpch))

 do j=1,jpch
     tsum(j)=zero
     tcnt(j)=zero
     tlap0(j)=zero
     tlap1(j)=zero
     tlap2(j)=zero
     tlap3(j)=zero
     do i=1,nstep
        csum(i,j)=zero
        count(i,j)=zero
        c_ang0(i,j)=zero
        c_ang1(i,j)=zero
        c_ang2(i,j)=zero
        c_ang3(i,j)=zero
     end do
  end do
  do j=1,jpch
     read(lnangl,110,err=120,end=120) ich,satsensor0(j),&
          jchanum0(j),tlap0(j),(c_ang0(i,j),i=1,nstep)
  end do
110 format(I5,1x,A20,1x,I5,e15.6/100(4x,10f7.3/))

! If restart, zero tlap0 and c_ang0
  if (restart) then
     do j=1,jpch
        tlap0(j)=zero
     end do
     do j=1,jpch
        do i=1,nstep
           c_ang0(i,j)=zero
        end do
     end do
  endif

  goto 130

120 continue
  write(6,*)'***WARNING*** EOF or ERROR reading input satang file'
  stop 99

130 continue
  close(lnangl)


! Assign each satellite/sensor to a mpi task
  ndatppe=0
  ix=0
  ipoint=0
  do i=1,ndat
     if(ix >= npe )ix=ix-npe
     if(ix == mype)then
        ndatppe=ndatppe+1
        ipoint(ndatppe)=i
     end if
     ix=ix+1
  end do


! Loop over mpi tasks.  Each task processes a given set of satellite/sensors
  do ii=1,ndatppe
     iii=ipoint(ii)
     obstype=dtype(iii)
     platid=dplat(iii)
     satsens_id=dsis(iii)

     fstring = 'diag_' // trim(dtype(iii)) // '_' // trim(dplat(iii))

     tsum=zero
     tcnt=zero
     tlap1=zero
     tlap2=zero
     csum=zero
     count=zero
     c_ang1=zero
     c_ang2=zero

     update=.false.
!    Loop over times
     loopt:  do it=1,ntime

!       Create diagnostic filename for it-th date/time
        write(dstring,140) iyy(it),imm(it),idd(it),ihh(it)
140     format('.',i4.4,3i2.2)
        diag_rad = trim(fstring) // trim(dstring)

!       See if diagnostic file exists
        inquire(file=diag_rad,exist=lexist)
        string = ' skipping file '
        if (lexist) string = ' processing '
        write(6,*)' Task ',mype,string,trim(diag_rad),' with exist=',lexist
        if (.not.lexist) cycle loopt

!       Open file and read header
        open(lndiag,file=diag_rad,form='unformatted',status='old',iostat=istatus)
        if (istatus/=0) then
           write(6,*)' Task ',mype,' problem opening file ',trim(diag_rad),' iostat=',istatus
           close(lndiag)
           cycle loopt
        endif

        call read_radiag_header(lndiag,npred,retrieval,header_fix,header_chan,data_name,istatus)
        if (istatus/=0) then
           write(6,*)' Task ',mype,' problem reading header for file ',trim(diag_rad),' iostat=',istatus
           close(lndiag)
           cycle loopt
        endif

!       Process file
        update = .true.
        satsens = header_fix%isis
        n_chan = header_fix%nchan
        ipchan = header_fix%ipchan

!       Check for consistency between specified and retrieved satellite id
        if (satsens /= satsens_id) then
           write(6,*)'***ERROR*** inconsistent satellite ids'
           write(6,*)'  diag_rad= ',trim(diag_rad)
           write(6,*)'  satsens,satsens_id=',satsens,satsens_id
           ierror_code=99
           call mpi_abort(mpi_comm_world,ierror_code,ierror)
           stop 98
        endif

!       Extract satinfo relative index
        do j=1,n_chan
           io_chan(j) = real( header_chan(j)%iochan, 4 )
        end do
        
!       Loop to read diagnostic file
        istatus = 0
        loopd:  do while (istatus == 0)

!          Read a record.  If read flag, istatus does not equal zero, exit loopd
           call read_radiag_data( lndiag, header_fix, retrieval, data_fix, data_chan, data_extra, istatus )
           if( istatus /= 0 ) exit loopd

!          Extract scan angle, lat, lon
           scan   = data_fix%senscn_pos
           ispot  = nint(scan)

!          Channel loop
           loopc:  do j = 1, n_chan

!             Check for reasonable obs-ges and observed Tb.
!             If the o-g difference is too large (> 200 K, very genereous!)
!             of the observation is too cold (<50 K) or too warm (>500 K),
!             do not use this observation in computing the update to the
!             angle dependent bias.
              if( ( abs(data_chan(j)%omgnbc) > 200. .or. &
                   data_chan(j)%tbobs < 50. .or. &
                   data_chan(j)%tbobs > 500. ) ) then
                 cycle loopc
              end if

!             If iuseqc flag is >0 (i.e., 1), check variance.  If 
!             errinv= (1 /(obs error)) is small (small = less than 1.e-6)
!             the observation did not pass quality control.  In this
!             case, do not use this observation in computing the update
!             to the angle dependent bias
              if ( (iuseqc>0) .and. (data_chan(j)%errinv<1.e-6) ) then
                 cycle loopc
              end if

!             If iuseqc flag is <=0 (i.e., 0 or -1), ensure (o-g)<dtmax.
!             If the user says to ingore the qc flag, check the the o-g
!             difference falls within the user specify maximum allowable
!             difference.  If the o-g lies outside this bound, do not use
!             this observation in computing the update to the angle 
!             dependent bias.
              if ( (iuseqc<=0) .and. abs(data_chan(j)%omgnbc)>dtmax ) then
                 cycle loopc
              end if

!                Add values to running sums.

!             NOTE the modification for the goes imager radiance:  For 
!             first 10 and last 30 scan position (nstep=90 so 60 to nstep is 30),
!             accumulate sum over all positions in range 1-10, 60-step.  This is 
!             done to increase the sample size for the statistics.

              x = data_chan(j)%tbobs - data_chan(j)%omgnbc    ! simulated Tb (no bias correction)
              y = data_chan(j)%tbobs                          ! observed Tb
              errinv = data_chan(j)%errinv
              if (iuseqc<=0) errinv=one
              
              jj   = io_chan(j)
              bias = (y - x) - c_ang0(ispot,jj)
              tlap = data_chan(j)%tlap - tlap0(jj)
              
              if (obstype=='goesimg') then
                 if (ispot<=10) then
                    do i=1,10
                       c_ang1(i,jj) = c_ang1(i,jj) + bias*errinv
                       csum(i,jj)   = csum(i,jj)   + errinv
                       count(i,jj)  = count(i,jj)  + one
                    end do
                    tlap1(jj)       = tlap1(jj)    + tlap*errinv
                    tsum(jj)        = tsum(jj)     + errinv
                    tcnt(jj)        = tcnt(jj)     + one
                 else if(ispot >=60) then
                    do i=60,nstep
                       c_ang1(i,jj) = c_ang1(i,jj) + bias*errinv
                       csum(i,jj)   = csum(i,jj)   + errinv
                       count(i,jj)  = count(i,jj)  + one
                    end do
                    tlap1(jj)       = tlap1(jj)    + tlap*errinv
                    tsum(jj)        = tsum(jj)     + errinv
                    tcnt(jj)        = tcnt(jj)     + one
                 endif
              else
                 c_ang1(ispot,jj)= c_ang1(ispot,jj)+ bias*errinv
                 csum(ispot,jj)  = csum(ispot,jj)  + errinv
                 count(ispot,jj) = count(ispot,jj) + one
                 tlap1(jj)       = tlap1(jj)       + tlap*errinv
                 tsum(jj)        = tsum(jj)        + errinv
                 tcnt(jj)        = tcnt(jj)        + one
              end if
              
           enddo loopc ! channel loop

!       End of loop over diagnostic file
        enddo loopd
        close(lndiag)

!    End of loop over time
     end do loopt

!    If diagnostic file for at least one time was read, compute statistcs
     if (update) then

!       Compute mean
        do j = 1,n_chan
           jj=io_chan(j)

           if(tcnt(jj) >= nsize)  then
              tlap2(jj) = tlap0(jj) + wgtlap*tlap1(jj)/tsum(jj)
           elseif (tcnt(jj)>0) then
              ratio = max(zero,min(tcnt(jj)*rsize,one))
              tlap2(jj) = tlap0(jj) + ratio*wgtlap*tlap1(jj)/tsum(jj)
           else
              tlap2(jj) = tlap0(jj)
           endif

           do i=1,nstep
              if(count(i,jj) >= nsize) then
                 c_ang2(i,jj)=c_ang0(i,jj) + wgtang*c_ang1(i,jj)/csum(i,jj)
              elseif (count(i,jj)>0) then
                 ratio = max(zero,min(count(i,jj)*rsize,one))
                 c_ang2(i,jj)=c_ang0(i,jj) + ratio*wgtang*c_ang1(i,jj)/csum(i,jj)
              else
                 c_ang2(i,jj)=c_ang0(i,jj)
              endif

           end do
        end do
           

!       Write updated angle bias and sample size to scratch files
        dname = 'update_' // trim(obstype) // '_' // trim(platid)
        open(lntemp,file=dname,form='formatted')
        do j=1,n_chan
           jj=io_chan(j)
           write(lntemp,210) jj,satsensor0(jj),jchanum0(jj),&
                tlap2(jj),(c_ang2(i,jj),i=1,nstep)
210        format(I5,1x,A20,1x,I5,e15.6/9(4x,10e13.6/))
        end do
        close(lntemp) 

!       Sample size scratch file only for additional information.
        sname = 'sample_' // trim(obstype) // '_' // trim(platid)
        open(lntemp,file=sname,form='formatted')
        do j=1,n_chan
           jj=io_chan(j)
           write(lntemp,210) jj,satsensor0(jj),jchanum0(jj),&
                tlap2(jj),(csum(i,jj),i=1,nstep)
        end do
        close(lntemp)
        
!    End of update block
     endif

! End of loop over satellite/sensor types
  end do


! Wait for all mpi tasks to finish processing the 
! satellite/sensors assigned to them.

  write(6,*)' Wait after satellite/sensor loop'
  call mpi_barrier(mpi_comm_world,ierror)


! Let a single task combine the satellite/sensor specific
! update files together
  if (outask) then

!    Initialize output arrays to intial satang values read in
     write(6,*)' '
     write(6,*)' Initialize output arrays to input values'
     tlap3=tlap0
     c_ang3=c_ang0
     satsensor3=satsensor0
     jchanum3=jchanum0

!    Loop over the satellite/sensors.  Read in each update
!    scratch file and load into proper location in output
!    arrays.  Special logic is required for AMSRE and SSMIS
!    due to their information being spread acorss different
!    radiance diagnostic files.
!
     do i=1,ndat
        fname = 'update_' // trim(dtype(i)) // '_' // trim(dplat(i))
        inquire(file=fname,exist=lexist)
        string = ' skipping '
        if (lexist) string = ' processing '
        write(6,*) string,' update file i=',i,' with fname=',trim(fname),' ',lexist

!       Process the scratch update file
        if (lexist) then

           io_chan=0
           satsensor2=' '
           jchanum2=0
           tlap2=zero
           c_ang2=zero

!          Read data from scratch file
           open(lntemp,file=fname,form='formatted')
           done=.false.
           j=1
           do while (.not.done)
              read(lntemp,210,end=160) io_chan(j),satsensor2(j),jchanum2(j),&
                   tlap2(j),(c_ang2(ii,j),ii=1,nstep)
              j=j+1
              goto 170
160           continue
              done=.true.
170           continue
           end do
           n_chan=j-1
           close(lntemp)

!          Transfer to output arrays.  Note special treatement for
!          amsre and ssmis
           if (index(dtype(i),'amsre_')==0 .and. index(dtype(i),'ssmis_')==0) then
              do j=1,n_chan
                 jj=io_chan(j)
                 satsensor3(jj)=satsensor2(j)
                 jchanum3(jj)  =jchanum2(j)
                 tlap3(jj)     =tlap2(j)
                 do ii=1,nstep
                    c_ang3(ii,jj)=c_ang2(ii,j)
                 end do
              end do

!          Special block for AMSRE.  AMSRE simulated Tb are
!          spilt into three groupings as shown below:
!            amsre_low:  channels 1-4
!            amsre_mid:  channels 5-10
!            amsre_hig:  channels 11-12

           elseif (index(dtype(i),'amsre_')/=0) then
              if (dtype(i)=='amsre_low') then
                 do j=1,4
                    jj=io_chan(j)
                    satsensor3(jj)=satsensor2(j)
                    jchanum3(jj)  =jchanum2(j)
                    tlap3(jj)     =tlap2(j)
                    do ii=1,nstep
                       c_ang3(ii,jj)=c_ang2(ii,j)
                    end do
                 end do
              elseif (dtype(i)=='amsre_mid') then
                 do j=5,10
                    jj=io_chan(j)
                    satsensor3(jj)=satsensor2(j)
                    jchanum3(jj)  =jchanum2(j)
                    tlap3(jj)     =tlap2(j)
                    do ii=1,nstep
                       c_ang3(ii,jj)=c_ang2(ii,j)
                    end do
                 end do
              elseif (dtype(i)=='amsre_hig') then
                 do j=11,12
                    jj=io_chan(j)
                    satsensor3(jj)=satsensor2(j)
                    jchanum3(jj)  =jchanum2(j)
                    tlap3(jj)     =tlap2(j)
                    do ii=1,nstep
                       c_ang3(ii,jj)=c_ang2(ii,j)
                    end do
                 end do
              endif

!          Special block for SSMIS.  SSMIS simulated Tb are
!          split into four groupings as shown below:
!            ssmis_img:  channels 8-11,17-18
!            ssmis_env:  channels 12-16
!            ssmis_las:  channels 1-7,24
!            ssmis_uas:  channels 19-23

           elseif (index(dtype(i),'ssmis_')/=0) then
              if (dtype(i)=='ssmis_img') then
                 do j=8,11
                    jj=io_chan(j)
                    satsensor3(jj)=satsensor2(j)
                    jchanum3(jj)  =jchanum2(j)
                    tlap3(jj)     =tlap2(j)
                    do ii=1,nstep
                       c_ang3(ii,jj)=c_ang2(ii,j)
                    end do
                 end do
                 do j=17,18
                    jj=io_chan(j)
                    satsensor3(jj)=satsensor2(j)
                    jchanum3(jj)  =jchanum2(j)
                    tlap3(jj)     =tlap2(j)
                    do ii=1,nstep
                       c_ang3(ii,jj)=c_ang2(ii,j)
                    end do
                 end do
              elseif (dtype(i)=='ssmis_env') then
                 do j=12,16
                    jj=io_chan(j)
                    satsensor3(jj)=satsensor2(j)
                    jchanum3(jj)  =jchanum2(j)
                    tlap3(jj)     =tlap2(j)
                    do ii=1,nstep
                       c_ang3(ii,jj)=c_ang2(ii,j)
                    end do
                 end do
              elseif (dtype(i)=='ssmis_las') then
                 do j=1,7
                    jj=io_chan(j)
                    satsensor3(jj)=satsensor2(j)
                    jchanum3(jj)  =jchanum2(j)
                    tlap3(jj)     =tlap2(j)
                    do ii=1,nstep
                       c_ang3(ii,jj)=c_ang2(ii,j)
                    end do
                 end do
                 do j=24,24
                    jj=io_chan(j)
                    satsensor3(jj)=satsensor2(j)
                    jchanum3(jj)  =jchanum2(j)
                    tlap3(jj)     =tlap2(j)
                    do ii=1,nstep
                       c_ang3(ii,jj)=c_ang2(ii,j)
                    end do
                 end do
              elseif (dtype(i)=='ssmis_uas') then
                 do j=19,23
                    jj=io_chan(j)
                    satsensor3(jj)=satsensor2(j)
                    jchanum3(jj)  =jchanum2(j)
                    tlap3(jj)     =tlap2(j)
                    do ii=1,nstep
                       c_ang3(ii,jj)=c_ang2(ii,j)
                    end do
                 end do
              endif

!          End of transfer block
           endif

!       End of lexist block
        endif

!    End of loop over satellite/sensor data types
     end do


!    Write updated satang statistics to output file
     open(lnupdt,file='satbias_ang.out',form='formatted')
     if (word == 'nscan=') write(lnupdt,'("nscan=",i8)') nstep
     do j=1,jpch
        write(lnupdt,110) j,satsensor3(j),jchanum3(j),tlap3(j),(c_ang3(i,j),i=1,nstep)
     end do
     close(lnupdt)

! End of block for ouput mpi task     
  endif

! Deallocate data arrays
  deallocate(tsum,tlap1,tcnt)
  deallocate(csum,c_ang1)
  deallocate(count)
  deallocate(satsensor0,jchanum0,tlap0,c_ang0)
  deallocate(satsensor2,jchanum2,tlap2,c_ang2)
  deallocate(satsensor3,jchanum3,tlap3,c_ang3)

! End of program
  if (outask) call w3tage('GLOBAL_ANGUPDATE')
  call mpi_finalize(ierror)

end program main



