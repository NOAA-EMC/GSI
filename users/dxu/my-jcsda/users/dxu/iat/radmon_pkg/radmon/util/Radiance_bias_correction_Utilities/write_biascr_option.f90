    program main

!  HISTORY:
!    03June2010 Zhu, Yanqiu 
!

    implicit none

!   Declare local variables
    integer lunin,lunout,lunout_pc,ip,istat,n,ichan,ich
    integer npred,i,ii,npred_new,angord,count_tlap
    real*8 tlapm
    real*8 tsum
    real*8 ostats
    real*8,dimension(90)::cbiasx
    real*8 tlapmean(4000),chn(4000)
    real*8,allocatable,dimension(:):: predr0,predr,varx
    logical :: newpc4pred,adp_anglebc
    character(len=20) :: isis
    character(len=20),dimension(4000) :: nsis

    integer iarg,argc
    integer iargc
    character(len=256) argv

    argc = iargc()
    if ( argc < 1 ) then
       print *, "Usage: write_biascr_option.x -newpc4pred -adp_anglebc angord"
       print *
       print *, "Defaul: newpc4pred=.false. adp_anglebc=.false."
       print *, "Do nothing"
       print *
       stop
    end if

    newpc4pred=.false.
    adp_anglebc=.false.
    angord=4

    do i=1,argc
       call GetArg( i, argv)
       if (trim(argv) == '-newpc4pred') then 
          newpc4pred=.true.
       else if (trim(argv) == '-adp_anglebc') then 
          adp_anglebc=.true.
       else
          read(argv,*) angord
          print * , "polynomial order of angle bias correction= ", angord
       end if
    end do
    if ((.not. newpc4pred) .and. (.not. adp_anglebc)) stop

    npred=5
    lunin=11
    lunout=12
    lunout_pc=13

    if (newpc4pred .and. (.not. adp_anglebc)) then
       open(lunin,file='satbias_angle',form='formatted')
       open(lunout,file='satbias_angle.new',form='formatted')
       read2: do
          read(lunin,'(I5,1x,A20,2x,I4,e15.6/9(4x,10f7.3/))',iostat=istat) &
               ich,isis,ichan,tlapm,(cbiasx(ip),ip=1,90)
          if (istat /= 0) exit
          tlapm=tlapm*100.0
          write(lunout,'(I5,1x,A20,2x,I4,e15.6/9(4x,10f7.3/))',iostat=istat) &
               ich,isis,ichan,tlapm,(cbiasx(ip),ip=1,90)
       end do read2
       close(lunin)
       close(lunout)

       allocate(predr0(npred),predr(npred))
       open(lunin,file='satbias_in' ,form='formatted')
       open(lunout,file='satbias_in.new' ,form='formatted')
       open(lunout_pc,file='satbias_pc' ,form='formatted')
       read3: do
          read(lunin,'(I5,1x,A20,1x,I5,10f12.6)',iostat=istat) ich,isis,&
                  ichan,(predr0(ip),ip=1,npred)
          if (istat /= 0) exit
          predr=0.0
          write(lunout,'(I5,1x,A20,1x,I5,10f12.6)',iostat=istat) ich,isis,&
                  ichan,(predr(ip),ip=1,npred)


          ostats=0.0
          varx=0.0
          write(lunout_pc,'(I5,1x,A20,1x,I5,e15.7/2(4x,10e15.7/))',iostat=istat) &
                     ich,isis,ichan,ostats,(varx(ip),ip=1,npred)
       end do read3
       deallocate(predr0,predr)
       close(lunin)
       close(lunout)
       close(lunout_pc)
    end if


    if (newpc4pred .and. adp_anglebc) then
       npred_new=npred+angord+3
       tlapm=0.0
       tsum=0.0
       count_tlap = 0
       open(lunin,file='satbias_angle',form='formatted')
       open(lunout,file='satbias_angle.new',form='formatted')
       ii=0
       read4: do
          read(lunin,'(I5,1x,A20,2x,I4,e15.6/9(4x,10f7.3/))',iostat=istat) &
               ich,isis,ichan,tlapm,(cbiasx(ip),ip=1,90)
          if (istat /= 0) exit
          tlapm=tlapm*100.0

          ii=ii+1
          nsis(ii)=isis
          tlapmean(ii)=tlapm
          chn(ii)=ichan

!         not used in the GSI run, but write out anyway
          write(lunout,'(I5,1x,A20,2x,I4,e15.6/9(4x,10f7.3/))',iostat=istat) &
               ich,isis,ichan,tlapm,(cbiasx(ip),ip=1,90)

       end do read4
       close(lunin)
!      close(lunout)

       allocate(predr0(npred),predr(npred_new),varx(npred_new))
       open(lunin,file='satbias_in' ,form='formatted')
       open(lunout,file='satbias_in.new' ,form='formatted')
       open(lunout_pc,file='satbias_pc' ,form='formatted')
       read5: do
          read(lunin,'(I5,1x,A20,1x,I5,10f12.6)',iostat=istat) ich,isis,&
                  ichan,(predr0(ip),ip=1,npred)
          if (istat /= 0) exit
          predr=0.0
          do i=1,ii
             if (trim(isis)==trim(nsis(i)) .and. ichan==chn(i)) then
                tlapm=tlapmean(i)
                tsum=1000000.0
                count_tlap = 999
                if (abs(tlapm) < 1.0e-5) then 
                   count_tlap = 0
                   tsum=0.0
                end if
             end if
          end do
          write(lunout,'(I5,1x,A20,1x,I5,2e15.6,1x,I5/2(4x,10f12.6/))',iostat=istat) ich,isis,&
                  ichan,tlapm,tsum,count_tlap,(predr(ip),ip=1,npred_new)


          ostats=0.0
          varx=0.0
          write(lunout_pc,'(I5,1x,A20,1x,I5,e15.7/2(4x,10e15.7/))',iostat=istat) &
                     ich,isis,ichan,ostats,(varx(ip),ip=1,npred_new)
       end do read5
       deallocate(predr0,predr,varx)
       close(lunin)
       close(lunout)
       close(lunout_pc)
    end if

    end 

