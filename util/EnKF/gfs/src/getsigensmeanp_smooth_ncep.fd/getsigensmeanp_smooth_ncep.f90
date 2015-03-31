program getsigensmeanp_smooth
!$$$  main program documentation block
!
! program:  getsigensmean              compute ensemble mean
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract:  create ensemble mean NCEP GFS spectral sigma file.
!
! program history log:
!   2009-02-23  Initial version.
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
  
  use sigio_module
  use nemsio_module, only:  nemsio_init,nemsio_open,nemsio_close
  use nemsio_module, only:  nemsio_gfile,nemsio_getfilehead,nemsio_readrec,&
       nemsio_writerec,nemsio_readrecv,nemsio_writerecv,nemsio_charkind8

  implicit none

  real,parameter :: zero=0.0_4

  logical lexist,dosmooth,nemsio,sigio
  character(nemsio_charkind8):: dtype
  character(len=3) charnanal
  character(len=500) filenamein,filenameout,filenameouts,datapath,fileprefix,fname
  integer iret,nlevs,ntrac,ntrunc,nanals,ngrd,k,iunit,window
  integer nsize,nsize2,nsize3,nsize3t
  integer, dimension(:), allocatable :: smoothparm
  integer mype,mype1,npe,orig_group, new_group, new_comm
  integer:: nrec,latb,lonb,npts,n
  integer,dimension(:),allocatable:: new_group_members,reclev
  real(8) rnanals
  real(8),dimension(:,:,:), allocatable :: smoothfact
  real(4),dimension(:),allocatable :: sigdatapert_ps,sigdatapert_z,sigdatapert_d,&
       sigdatapert_t,sigdatapert_q,sigdatapert_oz,sigdatapert_cw
  real(4),allocatable,dimension(:,:):: rwork_mem,rwork_avg
  real(4),allocatable,dimension(:):: rwork_lev

  type(sigio_head) :: sigheadi,sigheadm
  type(sigio_data) :: sigdatai,sigdatam

  type(nemsio_gfile) :: gfile, gfileo, gfilei, gfileos

! mpi definitions.
  include 'mpif.h'

! Initialize mpi
!  mype is process number, npe is total number of processes.
  call mpi_init(iret)
  call MPI_Comm_rank(MPI_COMM_WORLD,mype,iret)
  call MPI_Comm_size(MPI_COMM_WORLD,npe,iret)
  mype1=mype+1

  if (mype==0) call w3tagb('GETSIGENSMEAN_SMOOTH',2011,0319,0055,'NP25')

! Get user input from command line
  call getarg(1,datapath)
  call getarg(2,filenameout)
  call getarg(3,fileprefix)
  call getarg(4,charnanal)
  read(charnanal,'(i3)') nanals
  rnanals=nanals
  rnanals=1.0_8/rnanals
  filenameout = trim(adjustl(datapath))//filenameout

  if (mype==0) then
     write(6,*)' '
     write(6,*)'Command line input'
     write(6,*)' datapath      = ',trim(datapath)
     write(6,*)' filenameout   = ',trim(filenameout)
     write(6,*)' fileprefix    = ',trim(fileprefix)
     write(6,*)' nanals,rnanals= ',nanals,rnanals
  endif
  
  if (npe < nanals) then
     write(6,*)'***ERROR***  npe too small.  npe=',npe,' < nanals=',nanals
     call MPI_Abort(MPI_COMM_WORLD,99,iret)
     stop
  end if
  
  iunit = 21
  window = 1 ! cosine bell window for smoothing

! Create sub-communicator to handle number of cases (nanals)
  call mpi_comm_group(mpi_comm_world,orig_group,iret)

  allocate(new_group_members(nanals))
  do k=1,nanals
     new_group_members(k)=k-1
  end do

  call mpi_group_incl(orig_group,nanals,new_group_members,new_group,iret)
  call mpi_comm_create(mpi_comm_world,new_group,new_comm,iret)

  if (iret.ne.0) then
     write(6,*)'***ERROR*** after mpi_comm_create with iret=',iret
     call mpi_abort(mpi_comm_world,101,iret)
  endif

  sigio=.false.
  nemsio=.false.

! Process input files (one file per task)
  if (mype1 <= nanals) then

     call nemsio_init(iret)
     
     write(charnanal,'(i3.3)') mype1
     filenamein = trim(adjustl(datapath))// &
          trim(adjustl(fileprefix))//'_mem'//charnanal
     
!    Read each ensemble member FHDFI forecast.
     call sigio_srohdc(iunit,trim(filenamein),sigheadi,sigdatai,iret)
     if (iret == 0 ) then
        sigio = .true.
        write(6,*)'Read sigio ',trim(filenamein),' iret=',iret
        ntrunc  = sigheadi%jcap
        ntrac   = sigheadi%ntrac
        nlevs   = sigheadi%levs
     else
        call nemsio_open(gfile,trim(filenamein),'READ',iret=iret)
        if (iret == 0 ) then
           nemsio = .true.
           write(6,*)'Read nemsio ',trim(filenamein),' iret=',iret
           call nemsio_getfilehead(gfile, nrec=nrec, jcap=ntrunc, &
                dimx=lonb, dimy=latb, dimz=nlevs, ntrac=ntrac, gdatatype=dtype, iret=iret)
        else
           write(6,*)'***ERROR*** ',trim(filenamein),' contains unrecognized format.  ABORT'
        endif
     endif
     if (.not.nemsio .and. .not.sigio) goto 100
     if (mype==0) write(6,*)'computing mean with nemsio=',nemsio,' sigio=',sigio


     nsize2  = (ntrunc+1)*(ntrunc+2)
     nsize3  = nsize2*nlevs
     nsize3t = nsize3*ntrac
     if (mype==0) then
        write(6,*)'Read header information from ',trim(filenamein)
        write(6,*)' ntrunc = ',ntrunc
        write(6,*)' ntrac  = ',ntrac
        write(6,*)' nlevs  = ',nlevs
        write(6,*)' nsize2 = ',nsize2
        write(6,*)' nsize3 = ',nsize3
        write(6,*)' nsize3t= ',nsize3t
        if (nemsio) then
           write(6,*)' lonb   = ',lonb
           write(6,*)' latb   = ',latb
           write(6,*)' nrec   = ',nrec
        endif
     endif

     if (sigio) then

!       Compute ensemble sums.
        call sigio_aldata(sigheadi,sigdatam,iret)
        call mpi_allreduce(sigdatai%z,sigdatam%z,  nsize3, mpi_real,mpi_sum,new_comm,iret)
        call mpi_allreduce(sigdatai%d,sigdatam%d,  nsize3, mpi_real,mpi_sum,new_comm,iret)
        call mpi_allreduce(sigdatai%t,sigdatam%t,  nsize3, mpi_real,mpi_sum,new_comm,iret)
        call mpi_allreduce(sigdatai%q,sigdatam%q,  nsize3t,mpi_real,mpi_sum,new_comm,iret)
        call mpi_allreduce(sigdatai%ps,sigdatam%ps,nsize2, mpi_real,mpi_sum,new_comm,iret)
     
!       Compute ensemble mean on all tasks
        sigdatam%hs = sigdatai%hs
        sigdatam%ps = sigdatam%ps*rnanals
        sigdatam%z  = sigdatam%z*rnanals
        sigdatam%d  = sigdatam%d*rnanals
        sigdatam%t  = sigdatam%t*rnanals
        sigdatam%q  = sigdatam%q*rnanals

!       Write ensemble mean from task 0
        if (mype==0) then
           sigheadm = sigheadi
           ngrd = sigheadi%nxgr
           if (ngrd>0) sigdatam%xgr = sigdatai%xgr
           
           sigheadm%iens(1) = 1 ! unperturbed control
           sigheadm%iens(2) = 2 ! low res control
           sigheadm%icen2 = 2 ! sub-center, must be 2 or ens info not used
           call sigio_swohdc(iunit,filenameout,sigheadm,sigdatam,iret)
           write(6,*)'Write ensemble mean ',trim(filenameout),' iret=',iret
        endif

        call sigio_axdata(sigdatai,iret)

     elseif (nemsio) then
        npts=lonb*latb
        nsize=npts*nrec
        allocate(rwork_mem(npts,nrec))
        allocate(rwork_avg(npts,nrec))
        allocate(rwork_lev(npts))

        rwork_mem=zero
        do n=1,nrec
           call nemsio_readrec(gfile,n,rwork_mem(:,n),iret=iret)
        end do
        rwork_avg=zero
        call mpi_allreduce(rwork_mem,rwork_avg,nsize,mpi_real,mpi_sum,new_comm,iret)
        rwork_avg = rwork_avg * rnanals

        if (mype==0) then
           gfileo=gfile
           call nemsio_open(gfileo,trim(filenameout),'WRITE',iret=iret )
           do n=1,nrec
              call nemsio_writerec(gfileo,n,rwork_avg(:,n),iret=iret)
           end do
           call nemsio_readrecv(gfile,'hgt','sfc',1,rwork_lev,iret)
           call nemsio_writerecv(gfileo,'hgt','sfc',1,rwork_lev,iret)
           call nemsio_close(gfileo,iret)
           write(6,*)'Write ensmemble mean ',trim(filenameout),' iret=',iret
        endif
        call nemsio_close(gfile,iret)

     endif

!    Read smoothing parameters, if available
     fname='hybens_smoothinfo'
     inquire(file=trim(fname),exist=lexist)
     if ( lexist ) then
        allocate(smoothparm(nlevs))
        smoothparm = -1
        open(9,form='formatted',file=fname)
        do k=1,nlevs
           read(9,'(i3)') smoothparm(k)
        enddo
        close(9)
        dosmooth = maxval(smoothparm)>0
     else
        if (mype==0) write(6,*)'***WARNING***  hybens_smoothinfo not found - no smoothing'
        dosmooth = .false.
     endif
     if (mype==0) write(6,*)'Dosmooth=',dosmooth
     
!    If smoothing requested, loop over and smooth analysis files
     if (dosmooth) then

!       Set up smoother
        allocate(smoothfact(0:ntrunc,0:ntrunc,nlevs))
        smoothfact=1.0_8
        call setup_smooth(ntrunc,nlevs,smoothparm,window,smoothfact)
        
        write(charnanal,'(i3.3)') mype1
        filenamein = trim(adjustl(datapath))// &
             trim(adjustl(fileprefix))//'_mem'//charnanal
        
        filenameouts= trim(adjustl(datapath))// &
             trim(adjustl(fileprefix)) // 's' //'_mem'//charnanal

        if (sigio) then
!          Read each ensemble member FHDFI forecast.
           call sigio_srohdc(iunit,trim(filenamein),sigheadi,sigdatai,iret)
        
           allocate(sigdatapert_z(nsize2),sigdatapert_d(nsize2),sigdatapert_t(nsize2),&
                sigdatapert_q(nsize2),sigdatapert_oz(nsize2),sigdatapert_cw(nsize2),&
                sigdatapert_ps(nsize2))
           
           k=1
           if (smoothparm(k)>0) then
              sigdatapert_ps  = sigdatai%ps(:)  - sigdatam%ps(:)
              call smooth(sigdatapert_ps,ntrunc,smoothfact(:,:,k))
              sigdatai%ps(:) = sigdatam%ps(:) + sigdatapert_ps
           endif
           
           do k=2,nlevs
              if (smoothparm(k)>0) then
                 sigdatapert_z  = sigdatai%z(:,k)   - sigdatam%z(:,k)
                 sigdatapert_d  = sigdatai%d(:,k)   - sigdatam%d(:,k)
                 sigdatapert_t  = sigdatai%t(:,k)   - sigdatam%t(:,k)
                 sigdatapert_q  = sigdatai%q(:,k,1) - sigdatam%q(:,k,1)
                 sigdatapert_oz = sigdatai%q(:,k,2) - sigdatam%q(:,k,2)
                 sigdatapert_cw = sigdatai%q(:,k,3) - sigdatam%q(:,k,3)
                 
                 call smooth(sigdatapert_z, ntrunc,smoothfact(:,:,k))
                 call smooth(sigdatapert_d, ntrunc,smoothfact(:,:,k))
                 call smooth(sigdatapert_t, ntrunc,smoothfact(:,:,k))
                 call smooth(sigdatapert_q, ntrunc,smoothfact(:,:,k))
                 call smooth(sigdatapert_oz,ntrunc,smoothfact(:,:,k))
                 call smooth(sigdatapert_cw,ntrunc,smoothfact(:,:,k))
                 
                 sigdatai%z(:,k)   = sigdatam%z(:,k)   + sigdatapert_z
                 sigdatai%d(:,k)   = sigdatam%d(:,k)   + sigdatapert_d
                 sigdatai%t(:,k)   = sigdatam%t(:,k)   + sigdatapert_t
                 sigdatai%q(:,k,1) = sigdatam%q(:,k,1) + sigdatapert_q
                 sigdatai%q(:,k,2) = sigdatam%q(:,k,2) + sigdatapert_oz
                 sigdatai%q(:,k,3) = sigdatam%q(:,k,3) + sigdatapert_cw
                 
              endif
           end do
           deallocate(sigdatapert_z,sigdatapert_d,sigdatapert_t,sigdatapert_q,&
                sigdatapert_oz,sigdatapert_cw,sigdatapert_ps)
        
!          Write out
           call sigio_swohdc(iunit,trim(filenameouts),sigheadi,sigdatai,iret)

           call sigio_axdata(sigdatai,iret)
           call sigio_axdata(sigdatam,iret)

        elseif (nemsio) then
!          Read ensemble member forecast
           call nemsio_open(gfilei,trim(filenamein),'READ',iret=iret)
             call nemsio_getfilehead(gfilei, nrec=nrec, jcap=ntrunc, &
                dimx=lonb, dimy=latb, dimz=nlevs, ntrac=ntrac, iret=iret)
           allocate(reclev(nrec))
           call nemsio_getfilehead(gfilei,reclev=reclev,iret=iret)

           rwork_mem=zero
           do n=1,nrec
              call nemsio_readrec(gfilei,n,rwork_mem(:,n),iret)
           end do

!          Smoothing loop over fields
           do n=1,nrec
              k=reclev(n)
              if (smoothparm(k)>0) then
                 rwork_lev=rwork_mem(:,n)-rwork_avg(:,n)

!!               Need to decide if/how to smooth nemsio grids.  Two
!!               options listed below
!!                 1) transform to spectral, smooth, transform to grid
!!                 2) add grid space smoother
!!               call smooth(rwork_lev,ntrunc,smoothfact(:,:,k))

                 rwork_mem(:,n)=rwork_avg(:,n)+rwork_lev
              end if
           end do

!          Write smoothed member forecast
           gfileos=gfilei
           call nemsio_open(gfileos,trim(filenameouts),'WRITE',iret=iret )
           do n=1,nrec
              call nemsio_writerec(gfileos,n,rwork_mem(:,n),iret)
           end do

!          Write unsmoothed member orography to smoothed output file
           call nemsio_readrecv(gfilei,'hgt','sfc',1,rwork_lev,iret)
           call nemsio_writerecv(gfileos,'hgt','sfc',1,rwork_lev,iret)

           call nemsio_close(gfilei,iret)
           call nemsio_close(gfileos,iret)

        endif
        write(6,*)'Write smoothed ',trim(filenameouts),' iret=',iret

!       Deallocate smoothing factors
        deallocate(smoothfact)

!    End of smoothing block
     endif

     if (allocated(rwork_mem)) deallocate(rwork_mem)
     if (allocated(rwork_avg)) deallocate(rwork_avg)
     if (allocated(rwork_lev)) deallocate(rwork_lev)


! Jump here if more mpi processors than files to process
  else
     write(6,*) 'No files to process for mpi task = ',mype
  endif

100 continue
  call mpi_barrier(mpi_comm_world,iret)
  
  if (mype1 <= nanals .and. .not.nemsio .and. .not.sigio) then
     write(6,*)'***ERROR***  invalid atmospheric file format'
     call MPI_Abort(MPI_COMM_WORLD,98,iret)
     stop
  endif

  if (mype==0) then
     write(6,*) 'all done!'
     call w3tage('GETSIGENSMEAN_SMOOTH')
  endif
  
 deallocate(new_group_members)
 
 call mpi_finalize(iret)
 

end program getsigensmeanp_smooth

subroutine smooth(specdat,ntrunc,smoothfact)
  implicit none
  integer :: m,nm,n
  integer, intent(in) :: ntrunc
  real(8),intent(in)  ::  smoothfact(0:ntrunc,0:ntrunc) ! smoothing factor
  real(4),intent(inout) :: specdat((ntrunc+1)*(ntrunc+2))
  
  nm = 1
  m_loop: do m=0,ntrunc
     n_loop: do n=m,ntrunc
        specdat(nm)   = smoothfact(n,m)*specdat(nm)
        specdat(nm+1) = smoothfact(n,m)*specdat(nm+1)
        nm = nm + 2
     enddo n_loop
  enddo m_loop
end subroutine smooth

subroutine setup_smooth(ntrunc,nlevs,smoothparm,window,smoothfact)
  implicit none
  integer :: m,n,k
  integer, intent(in) :: ntrunc,nlevs,window
  integer,dimension(nlevs),intent(in) ::  smoothparm ! smoothing parameter.
  real(8),dimension(0:ntrunc,0:ntrunc,nlevs),intent(out) :: smoothfact
  real(8) zero,half,one,pi,smoothval,rsmoothval,waven

  zero = 0.0_8
  half = 0.5_8
  one  = 1.0_8
  pi   = 4.0_8*atan(one)

  k_loop: do k=1,nlevs
     if (smoothparm(k) .le. 0.) cycle k_loop
     smoothval=smoothparm(k)
     rsmoothval=one/smoothval
     m_loop: do m=0,ntrunc
        n_loop: do n=m,ntrunc
           waven=real(n)
           if (window .eq. 1) then
              ! Hann window (cosine bell).
              if (n <= smoothparm(k)) then
                 smoothfact(n,m,k) = half*(one + cos(pi*waven*rsmoothval))
              else
                 smoothfact(n,m,k) = zero
              endif
           else if (window .eq. 2) then
              ! gaussian window.
              smoothfact(n,m,k) = exp(-(waven*rsmoothval)**2)
           else
              ! rectangular window (simple truncation)
              if (n <= smoothparm(k)) then
                 smoothfact(n,m,k) = one
              else
                 smoothfact(n,m,k) = zero
              endif
           endif
        enddo n_loop
     enddo m_loop
  enddo k_loop
end subroutine setup_smooth

