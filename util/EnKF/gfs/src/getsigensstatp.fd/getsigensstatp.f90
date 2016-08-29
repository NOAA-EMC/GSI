!< --- next few lines under version control, D O  N O T  E D I T --->
! $Date$
! $Revision$
! $Author$
! $Id$
!<------------------------------------------------------------------>
program getsigensstatp
!$$$  main program documentation block
!
! program:  getsigensstatp           compute ensemble mean and spread
!
! prgmmr: mahajan          org: emc/ncep               date: 2014-10-01
!
! abstract:  compute ensemble mean and spread from NCEP GFS spectral or nemsio binary files.
!
! program history log:
!   2014-08-23  Initial version.
!
! usage:
!   input files:
!
!   output files:
!
! attributes:
!   language: f95
!
!$$$

    use mpi
    use sigio_module,  only: sigio_head,sigio_data,sigio_srohdc, &
                             sigio_axdata,sigio_sclose
    use nemsio_module, only: nemsio_init,nemsio_open,nemsio_close, &
                             nemsio_gfile,nemsio_getfilehead,nemsio_charkind8, &
                             nemsio_readrec,nemsio_readrecv

    implicit none

    integer,parameter :: r_single=4,r_double=8
    integer,parameter :: iunit=21 
    integer,parameter :: idrt=4
    character(nemsio_charkind8) :: dtype
    character(len=3)   :: charnanal
    character(len=500) :: filenamein,datapath,filepref
    integer :: nanals,nlevs,ntrac,ntrunc,latb,lonb,iret
    integer :: k,krecu,krecv,krect,krecq,krecoz,kreccwmr
    integer :: nsize,npts,nrec,nflds
    real(r_double) :: rnanals,rnanalsm1
    character(len=16),allocatable,dimension(:) :: recnam
    integer :: mype,mype1,npe,orig_group,new_group,new_comm
    integer,dimension(:),allocatable :: new_group_members,reclev
    real(r_single),allocatable,dimension(:,:) :: rwork_mem,rwork_avg
    real(r_single),allocatable,dimension(:) :: glats,gwts
    logical :: sigio,nemsio

    type(sigio_head)   :: sigheadi
    type(sigio_data)   :: sigdatai
    type(nemsio_gfile) :: gfile

    ! Initialize mpi, mype is process number, npe is total number of processes.
    call mpi_init(iret)
    call MPI_Comm_rank(MPI_COMM_WORLD,mype,iret)
    call MPI_Comm_size(MPI_COMM_WORLD,npe,iret)
    mype1 = mype + 1

    if ( mype == 0 ) call w3tagb('GETSIGENSSTATP',2014,1025,0055,'NP25')

    ! Get user input from command line
    call getarg(1,datapath)
    call getarg(2,filepref)
    call getarg(3,charnanal)
    if ( iargc() < 3 ) then
        write(6,'(a)') "USAGE: ./getsigensstatp.x datapath filepref nanals"
        call mpi_abort(mpi_comm_world,99,iret)
        stop
    endif
    read(charnanal,'(i3)') nanals
    rnanals=nanals
    rnanals=1.0_r_double/rnanals
    rnanalsm1=nanals-1.0_r_double
    rnanalsm1=1.0_r_double/rnanalsm1

    if ( mype == 0 ) then
        write(6,'(1a)')'Command line input'
        write(6,'(2a)')' datapath      = ',trim(datapath)
        write(6,'(2a)')' fileprefix    = ',trim(filepref)
        write(6,'(2a)')' nanals        = ',trim(charnanal)
        write(6,'(3a)')' emean fileout = ',trim(adjustl(filepref))//'_ensmean'
        write(6,'(3a)')' esprd fileout = ',trim(adjustl(filepref))//'_ensspread'
        write(6,'(1a)')' '
    endif

    if ( npe < nanals ) then
        write(6,'(2(a,i4))')'***ERROR***  npe too small.  npe = ',npe,' < nanals = ',nanals
        call mpi_abort(mpi_comm_world,99,iret)
        stop
    end if

! Create sub-communicator to handle number of cases (nanals)
    call mpi_comm_group(mpi_comm_world,orig_group,iret)

    allocate(new_group_members(nanals))
    do k=1,nanals
        new_group_members(k)=k-1
    enddo
    if ( mype1 <= nanals ) then
        call mpi_group_incl(orig_group,nanals,new_group_members,new_group,iret)
    endif
    call mpi_comm_create(mpi_comm_world,new_group,new_comm,iret)
    if ( iret /= 0 ) then
        write(6,'(a,i5)')'***ERROR*** after mpi_comm_create with iret = ',iret
        call mpi_abort(mpi_comm_world,99,iret)
        stop
    endif

    nemsio = .false.
    sigio  = .false.

! Process input files (one file per task)
    if ( mype1 <= nanals ) then

        call nemsio_init(iret)

        write(charnanal,'(i3.3)') mype1
        filenamein = trim(adjustl(datapath)) // trim(adjustl(filepref)) // '_mem' // charnanal

        ! Read each ensemble member
        call sigio_srohdc(iunit,trim(adjustl(filenamein)),sigheadi,sigdatai,iret)
        if ( iret == 0 ) then
            sigio = .true.
            write(6,'(3a,i5)')'Read sigio ',trim(filenamein),' iret = ',iret
            ntrunc  = sigheadi%jcap
            ntrac   = sigheadi%ntrac
            nlevs   = sigheadi%levs
            latb    = sigheadi%latf
            lonb    = sigheadi%lonf
        else
            call nemsio_open(gfile,trim(filenamein),'READ',iret=iret)
            if ( iret == 0 ) then
                nemsio = .true.
                call nemsio_getfilehead(gfile, nrec=nrec, jcap=ntrunc, &
                dimx=lonb, dimy=latb, dimz=nlevs, ntrac=ntrac, gdatatype=dtype, iret=iret)
                write(6,'(5a,i5)')'Read nemsio ',trim(filenamein), ' dtype = ', trim(adjustl(dtype)),' iret = ',iret
                allocate(reclev(nrec),recnam(nrec))
                call nemsio_getfilehead(gfile,reclev=reclev,iret=iret)
                call nemsio_getfilehead(gfile,recname=recnam,iret=iret)
            else
                write(6,'(3a)')'***ERROR*** ',trim(filenamein),' contains unrecognized file format. ABORT!'
                call mpi_abort(mpi_comm_world,99,iret)
                stop
            endif
        endif

        if ( mype == 0 ) then
            write(6,'(a)')   ' '
            write(6,'(2(a,l1))')'Computing ensemble mean and spread with nemsio = ',nemsio,' , sigio = ',sigio
            write(6,'(a)')   ' '
        endif

        if ( mype == 0 ) then
            write(6,'(a)')   ' '
            write(6,'(2a)')  'Read header information from ',trim(filenamein)
            write(6,'(a,i5)')' ntrunc  = ',ntrunc
            write(6,'(a,i5)')' ntrac   = ',ntrac
            write(6,'(a,i5)')' nlevs   = ',nlevs
            write(6,'(a,i5)')' lonb    = ',lonb
            write(6,'(a,i5)')' latb    = ',latb
            write(6,'(a)')   ' '
        endif

        npts  = latb*lonb
        nflds = 1 + 6*nlevs
        nsize = npts*nflds

        if ( mype == 0 ) then
            allocate(glats(latb),gwts(latb))
            call splat(idrt,latb,glats,gwts)
            glats = 180.0_r_single / acos(-1.0_r_single) * asin(glats(latb:1:-1))
            deallocate(gwts)
        endif

        allocate(rwork_mem(npts,nflds))
        allocate(rwork_avg(npts,nflds))

        rwork_mem = 0.0_r_single
        rwork_avg = 0.0_r_single

        if ( sigio ) then

            call sptez(0,ntrunc,idrt,lonb,latb,sigdatai%ps,rwork_mem(:,1),1)
            do k = 1,nlevs
                krecu    = 1 + 0*nlevs + k
                krecv    = 1 + 1*nlevs + k
                krect    = 1 + 2*nlevs + k
                krecq    = 1 + 3*nlevs + k
                krecoz   = 1 + 4*nlevs + k
                kreccwmr = 1 + 5*nlevs + k
                call sptezv(0,ntrunc,idrt,lonb,latb,&
                            sigdatai%d(:,k),sigdatai%z(:,k),&
                            rwork_mem(:,krecu),rwork_mem(:,krecv),1)
                call sptez(0,ntrunc,idrt,lonb,latb,sigdatai%t(:,k  ),rwork_mem(:,krect   ),1)
                call sptez(0,ntrunc,idrt,lonb,latb,sigdatai%q(:,k,1),rwork_mem(:,krecq   ),1)
                call sptez(0,ntrunc,idrt,lonb,latb,sigdatai%q(:,k,2),rwork_mem(:,krecoz  ),1)
                call sptez(0,ntrunc,idrt,lonb,latb,sigdatai%q(:,k,3),rwork_mem(:,kreccwmr),1)
            enddo

            call sigio_axdata(sigdatai,iret)

        elseif ( nemsio ) then

            call nemsio_readrecv(gfile,'pres','mid layer',1,rwork_mem(:,1),iret)
            do k = 1,nlevs
                krecu    = 1 + 0*nlevs + k
                krecv    = 1 + 1*nlevs + k
                krect    = 1 + 2*nlevs + k
                krecq    = 1 + 3*nlevs + k
                krecoz   = 1 + 4*nlevs + k
                kreccwmr = 1 + 5*nlevs + k
                call nemsio_readrecv(gfile,'ugrd', 'mid layer',k,rwork_mem(:,krecu),   iret)
                call nemsio_readrecv(gfile,'vgrd', 'mid layer',k,rwork_mem(:,krecv),   iret)
                call nemsio_readrecv(gfile,'tmp',  'mid layer',k,rwork_mem(:,krect),   iret)
                call nemsio_readrecv(gfile,'spfh', 'mid layer',k,rwork_mem(:,krecq),   iret)
                call nemsio_readrecv(gfile,'o3mr', 'mid layer',k,rwork_mem(:,krecoz),  iret)
                call nemsio_readrecv(gfile,'clwmr','mid layer',k,rwork_mem(:,kreccwmr),iret)
            enddo
            call nemsio_close(gfile,iret)

        endif

        call mpi_allreduce(rwork_mem,rwork_avg,nsize,mpi_real4,mpi_sum,new_comm,iret)

        rwork_avg = rwork_avg * rnanals

        if ( mype == 0 ) call write_to_disk('mean')

        rwork_mem = (rwork_mem - rwork_avg) * (rwork_mem - rwork_avg)

        call mpi_allreduce(rwork_mem,rwork_avg,nsize,mpi_real4,mpi_sum,new_comm,iret)

        rwork_avg = sqrt(rwork_avg * rnanalsm1)

        if ( mype == 0 ) call write_to_disk('spread')

        deallocate(rwork_mem,rwork_avg)

    ! Jump here if more mpi processors than files to process
    else
        write(6,'(a,i5)') 'No files to process for mpi task = ',mype
    endif

    call mpi_barrier(mpi_comm_world,iret)

    deallocate(new_group_members)
    if ( mype == 0 ) deallocate(glats)

    if ( mype == 0 ) call w3tage('GETSIGENSSTATP')

999 continue

    call mpi_finalize(iret)

    stop

contains

subroutine write_to_disk(statstr)

   implicit none

   integer, parameter :: lunit=63
   character(len=*), intent(in) :: statstr

   character(len=500) :: filenameout
   integer :: kbeg,kend

   filenameout = trim(adjustl(datapath)) // trim(adjustl(filepref)) // '_ens' // trim(adjustl(statstr))

   call baopenwt(lunit,trim(adjustl(filenameout)) // '.bin4',iret)
   write(6,'(3a,i5)')'Write ',trim(adjustl(filenameout))//'.bin4',' iret=',iret
   kbeg = 1 ; kend = 1
   call wryte(lunit,r_single*lonb*latb,      rwork_avg(:,kbeg:kend))
   kbeg = kend + 1 ; kend = kend + nlevs
   call wryte(lunit,r_single*lonb*latb*nlevs,rwork_avg(:,kbeg:kend))
   kbeg = kend + 1 ; kend = kend + nlevs
   call wryte(lunit,r_single*lonb*latb*nlevs,rwork_avg(:,kbeg:kend))
   kbeg = kend + 1 ; kend = kend + nlevs
   call wryte(lunit,r_single*lonb*latb*nlevs,rwork_avg(:,kbeg:kend))
   kbeg = kend + 1 ; kend = kend + nlevs
   call wryte(lunit,r_single*lonb*latb*nlevs,rwork_avg(:,kbeg:kend))
   kbeg = kend + 1 ; kend = kend + nlevs
   call wryte(lunit,r_single*lonb*latb*nlevs,rwork_avg(:,kbeg:kend))
   kbeg = kend + 1 ; kend = kend + nlevs
   call wryte(lunit,r_single*lonb*latb*nlevs,rwork_avg(:,kbeg:kend))
   call baclose(lunit,iret)

   open(lunit,file=trim(adjustl(filenameout)) // '.ctl',form='formatted',status='replace',iostat=iret)
   write(lunit,'("DSET ^",a)') trim(adjustl(filepref))//'_ens'//trim(adjustl(statstr))//'.bin4'
   write(lunit,'("OPTIONS yrev")')
   write(lunit,'("UNDEF -9.99E+33")')
   write(lunit,'("TITLE ensemble",1x,a)') trim(adjustl(statstr))
   write(lunit,'("XDEF",i6," LINEAR",2f12.6)') lonb,0.0,360.0/lonb
   write(lunit,'("YDEF",i6," LEVELS")') latb
   write(lunit,'(5f12.6)') glats
   write(lunit,'("ZDEF",i6," LINEAR 1 1")') nlevs
   write(lunit,'("TDEF",i6,1x,"LINEAR",1x,"00Z01Jan2000",1x,i3,"hr")') 1,12
   write(lunit,'("VARS",i6)') 7
   write(lunit,'("PS  ",i3," 99 surface pressure (Pa)")') 0
   write(lunit,'("U   ",i3," 99 zonal wind (m/s)")') nlevs
   write(lunit,'("V   ",i3," 99 meridional wind (m/s)")') nlevs
   write(lunit,'("T   ",i3," 99 temperature (K)")') nlevs
   write(lunit,'("Q   ",i3," 99 specific humidity (kg/kg)")') nlevs
   write(lunit,'("OZ  ",i3," 99 ozone concentration (kg/kg)")') nlevs
   write(lunit,'("CW  ",i3," 99 cloud water mixing ratio (kg/kg)")') nlevs
   write(lunit,'(a)') 'ENDVARS'
   close(lunit)

   return

end subroutine write_to_disk

end program getsigensstatp
