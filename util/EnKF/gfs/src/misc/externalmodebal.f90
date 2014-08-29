program externalmodebal
! force analysis to have same vertically integrated mass flux divergence
! as filtered background. Follows approach described in 
! Technical Report Series on Global Modeling and Data Assimilation, Volume 27
! Max J. Suarez, Editor
! The GEOS-5 Data Assimilation System?Documentation of Versions 5.0.1, 5.1.0,
! and 5.2.0 (section 4.3 "Balancing vertically integrated mass divergence
! from analysis increments")
 USE SIGIO_MODULE
 use constants, only: rd, grav, cp, rearth, init_constants, init_constants_derived, pi
 use specmod
 implicit none
 type(sigio_head) :: sighead
 type(sigio_data) :: sigdata
 character(len=500) filename
 character(len=4) charnanal
 character(len=1) charfhr
 character(len=10) datestring
 real, dimension(:),allocatable :: wts
 real, dimension(:,:), allocatable :: &
 vmassdivanal,vmassdivfg,vmass,vrtspec,divspec
 real, dimension(:,:,:), allocatable :: &
 ug,vg,dp,uanal,vanal,ufg,vfg,pressanal,pressfg,divg,deltaug,deltavg
 real sumwts,uincmax,vincmax,uincmaxk,vincmaxk
 integer nlons,nlats,nlevs,nanal,nproc,nanals,&
 kmaxu,kmaxv,numproc,ierr,iunit,ntrunc,nhr,n,k,nhr1,nhr2

! mpi definitions.
 include 'mpif.h'
 call MPI_Init(ierr)
! nproc is process number, numproc is total number of processes.
 call MPI_Comm_rank(MPI_COMM_WORLD,nproc,ierr)
 call MPI_Comm_size(MPI_COMM_WORLD,numproc,ierr)

 iunit = 7
 ! get nanals,datestring from command line.
 call getarg(1,charnanal)
 read(charnanal,'(i4)') nanals
 call getarg(2,datestring)
 call getarg(3,charfhr)
 read(charfhr,'(i1)') nhr1
 call getarg(4,charfhr)
 read(charfhr,'(i1)') nhr2

 if (numproc .lt. nanals) then
    print *,numproc,nanals
    print *,'warning, numproc too small!'
    flush(6)
    flush(0)
    call MPI_Abort(MPI_COMM_WORLD,101,ierr)
    stop
 end if

 call init_constants(.false.)
 call init_constants_derived()

 ! read header from ensemble member 1.
 filename = "sfg_"//datestring//"_fhr06_mem001"
 iunit = 7
 call sigio_sropen(iunit,trim(filename),ierr)
 if (ierr .ne. 0) then
    print *,'cannot read file ',filename,ierr
    flush(6)
    flush(0)
    call MPI_Abort(MPI_COMM_WORLD,101,ierr)
    stop
 end if
 call sigio_srhead(iunit,sighead,ierr)
 call sigio_sclose(iunit,ierr)

 ntrunc = sighead%jcap
 nlats = sighead%latf
 nlons = sighead%lonf
 nlevs = sighead%levs

 nanal = nproc + 1
 write(charnanal,'(i3.3)') nanal

 allocate(ufg(nlons,nlats,nlevs))
 allocate(vfg(nlons,nlats,nlevs))
 allocate(pressfg(nlons,nlats,nlevs+1))
 allocate(uanal(nlons,nlats,nlevs))
 allocate(vanal(nlons,nlats,nlevs))
 allocate(pressanal(nlons,nlats,nlevs+1))
 allocate(dp(nlons,nlats,nlevs))
 allocate(ug(nlons,nlats,nlevs))
 allocate(vg(nlons,nlats,nlevs))
 allocate(deltaug(nlons,nlats,nlevs))
 allocate(deltavg(nlons,nlats,nlevs))
 allocate(divg(nlons,nlats,nlevs))
 allocate(divspec((ntrunc+1)*(ntrunc+2),nlevs))
 allocate(vrtspec((ntrunc+1)*(ntrunc+2),nlevs))
 allocate(vmassdivfg(nlons,nlats))
 allocate(vmassdivanal(nlons,nlats))
 allocate(vmass(nlons,nlats))

 call init_spec_vars(nlons,nlats,ntrunc,4)

! binomial (1-2-1) filter (weights from Pascal's triangle) if nhr1!=nhr2!=6
 if (nhr1 .eq. 5 .and. nhr2 .eq. 7) then
    ! use single application of 1-2-1 filter
    allocate(wts(3))
    wts(1)=1.; wts(2)=2.; wts(3)=1.
    if (nproc .eq. 0) print *,'using 1-2-1 time filter on background'
 else if (nhr1 .eq. 4 .and. nhr2 .eq. 8) then
    ! (1-2-1)(1-2-1) (2 applications)
    allocate(wts(5))
    wts(1)=1.; wts(2)=4.; wts(3)=6; wts(4)=4.; wts(5)=1.
    if (nproc .eq. 0) print *,'using double 1-2-1 time filter on background'
 else if (nhr1 .eq. 3 .and. nhr2 .eq. 9) then
    ! (1-2-1)(1-2-1)(1-2-1) (3 applications)
    allocate(wts(7))
    wts(1)=1.; wts(2)=6.; wts(3)=15.; wts(4)=20.
    wts(7)=1.; wts(6)=6.; wts(5)=15.
    if (nproc .eq. 0) print *,'using triple 1-2-1 time filter on background'
 else if (nhr1 .eq. 6 .and. nhr2 .eq. 6) then
    ! only read 6-h forecast, no time filtering.
    if (nproc .eq. 0) print *,'no filtering of background'
    allocate(wts(1))
    wts(1) = 1.
 else
    print *,'illegal values for nhr1,nhr2',nhr1,nhr2
    flush(6)
    flush(0)
    call MPI_Abort(MPI_COMM_WORLD,101,ierr)
    stop
 endif
 sumwts = sum(wts)
 wts = wts/sumwts

 ! read each ensemble member forecasts (hourly from nhr1 to nhr2)
 ! compute time filtered background vertically integrated mass flux divergence
 ! from hourly output.
 vmassdivfg = 0.
 n = 1
 do nhr=nhr1,nhr2
 write(charfhr,'(i1)') nhr
 filename = "sfg_"//datestring//"_fhr0"//charfhr//"_mem"//charnanal
 call getsigdata(filename,ufg,vfg,pressfg,nlons,nlats,nlevs,ntrunc)
!$omp parallel do private(k)
 do k=1,nlevs
    dp(:,:,k) = pressfg(:,:,k)-pressfg(:,:,k+1)
    ug(:,:,k) = ufg(:,:,k)*dp(:,:,k); vg(:,:,k) = vfg(:,:,k)*dp(:,:,k)
    call sptezv_s(divspec(:,k),vrtspec(:,k),ug(:,:,k),vg(:,:,k),-1) ! u,v to div,vrt
    call sptez_s(divspec(:,k),divg(:,:,k),1) ! divspec to divgrd
 enddo
!$omp end parallel do
 do k=1,nlevs
    vmassdivfg = vmassdivfg + wts(n)*divg(:,:,k)
 enddo 
 n = n + 1
 enddo ! nhr loop
 deallocate(wts)

 ! compute vertically integrated mass flux divergence of analysis.
 filename = "sanl_"//datestring//"_mem"//charnanal
 call getsigdata(filename,uanal,vanal,pressanal,nlons,nlats,nlevs,ntrunc)
 vmassdivanal = 0.
 vmass = 0.
!$omp parallel do private(k)
 do k=1,nlevs
    dp(:,:,k) = pressanal(:,:,k)-pressanal(:,:,k+1)
    ug(:,:,k) = uanal(:,:,k)*dp(:,:,k); vg(:,:,k) = vanal(:,:,k)*dp(:,:,k)
    call sptezv_s(divspec(:,k),vrtspec(:,k),ug(:,:,k),vg(:,:,k),-1) ! u,v to div,vrt
    call sptez_s(divspec(:,k),divg(:,:,k),1) ! divspec to divgrd
 enddo
!$omp end parallel do
 do k=1,nlevs
    !dp = pressanal(:,:,k)-pressanal(:,:,k+1)
    vmassdivanal = vmassdivanal + divg(:,:,k)
    vmass = vmass + dp(:,:,k)**2
 enddo
 print *,'fg mass div',nanal,minval(vmassdivfg),maxval(vmassdivfg)
 print *,'anal mass div',nanal,minval(vmassdivanal),maxval(vmassdivanal)

 if (nproc .eq. 0) print *,'wind adjustments by level'
 uincmax=-9.9e31;vincmax=-9.9e31
!$omp parallel do private(k)
 do k=1,nlevs
    !divg(:,:,k) = (vmassdivfg-vmassdivanal)/nlevs ! case 1 (4.3.1.1)- don't use!
    divg(:,:,k) = (vmassdivfg - vmassdivanal)*dp(:,:,k)**2/vmass ! case 2 (4.3.1.2)
    call sptez_s(divspec(:,k),divg(:,:,k),-1) ! divgrd to divspec
    vrtspec(:,k) = 0.
    call sptezv_s(divspec(:,k),vrtspec(:,k),deltaug(:,:,k),deltavg(:,:,k),1) ! div,vrt to u,v
 enddo
!$omp end parallel do
 do k=1,nlevs
    deltaug(:,:,k) = deltaug(:,:,k)/dp(:,:,k)
    deltavg(:,:,k) = deltavg(:,:,k)/dp(:,:,k)
    if (nproc .eq. 0) print *,k,minval(deltaug(:,:,k)),maxval(deltaug(:,:,k)),minval(deltavg(:,:,k)),maxval(deltavg(:,:,k))
    uincmaxk = maxval(abs(deltaug(:,:,k))); vincmaxk = maxval(abs(deltavg(:,:,k)))
    if (uincmaxk .gt. uincmax) then
       uincmax = uincmaxk
       kmaxu = k
    endif
    if (vincmaxk .gt. vincmax) then
       vincmax = vincmaxk
       kmaxv = k
    endif
    uanal(:,:,k) = uanal(:,:,k) + deltaug(:,:,k)
    vanal(:,:,k) = vanal(:,:,k) + deltavg(:,:,k)
 enddo
 print *,'ens mem',nanal,'max u increment = ',uincmax,' at level k ',kmaxu
 print *,'ens mem',nanal,'max v increment = ',vincmax,' at level k ',kmaxv
 vmassdivanal = 0.
!$omp parallel do private(k)
 do k=1,nlevs
    ug(:,:,k) = uanal(:,:,k)*dp(:,:,k); vg(:,:,k) = vanal(:,:,k)*dp(:,:,k)
    call sptezv_s(divspec(:,k),vrtspec(:,k),ug(:,:,k),vg(:,:,k),-1) ! u,v to div,vrt
    call sptez_s(divspec(:,k),divg(:,:,k),1) ! divspec to divgrd
 enddo
!$omp end parallel do
 do k=1,nlevs
    vmassdivanal = vmassdivanal + divg(:,:,k)
 enddo
 if (nanal .eq. 1) print *,'final mass div',nanal,minval(vmassdivanal),maxval(vmassdivanal)
 if (nanal .eq. 1) print *,'mass div increment',nanal,minval(vmassdivanal-vmassdivfg),maxval(vmassdivanal-vmassdivfg)

 ! read in analysis again
 call sigio_srohdc(iunit,trim(filename),sighead,sigdata,ierr)
 ! overwrite vorticity and div
!$omp parallel do private(k) 
 do k=1,nlevs
    call sptezv_s(divspec(:,k),vrtspec(:,k),uanal(:,:,k),vanal(:,:,k),-1) ! u,v to div,vrt
 enddo
!$omp end parallel do
 sigdata%d = divspec
 sigdata%z = vrtspec
 ! write out.
 call sigio_swohdc(iunit,filename,sighead,sigdata,ierr)
 call sigio_axdata(sigdata,ierr)

 ! cleanup and quit.
 deallocate(ufg,vfg,uanal,vanal)
 deallocate(pressfg,pressanal)
 deallocate(dp,ug,vg,deltaug,deltavg,divg,divspec,vrtspec)
 deallocate(vmassdivfg,vmassdivanal,vmass)

 call MPI_Barrier(MPI_COMM_WORLD,ierr)
 if (nproc .eq. 0) write(6,*) 'all done!'
 call MPI_Finalize(ierr)
 if (nproc .eq. 0 .and. ierr .ne. 0) then
  print *, 'MPI_Finalize error status = ',ierr
 end if

end program externalmodebal

subroutine getsigdata(filename,ug,vg,press,nlons,nlats,nlevs,ntrunc)
! f2py --fcompiler=intelem -c -m readsigdata readsigdata.f90 sigio_module.o
! specmod.o constants.o -L../lib -lsp_4s
  use sigio_module
  use specmod
  use constants, only: rd, grav, cp, rearth, init_constants, init_constants_derived
  implicit none
  type (sigio_data) :: sigdata
  type (sigio_head) :: sighead
  character(len=500),intent(in) :: filename
  real, dimension(nlons,nlats,nlevs), intent(out) :: ug,vg
  real, dimension(nlons,nlats,nlevs+1), intent(out) :: press
  real, dimension(nlons,nlats) :: psg
  real ak(nlevs+1),bk(nlevs+1)
  integer, intent(in) :: nlons,nlats,nlevs,ntrunc
  integer k,ierr,iunit
  real kap,kapr,kap1

  iunit = 7
   
  kap = rd/cp
  kapr = cp/rd
  kap1 = kap + 1.

  if (.not. isinitialized) call init_spec_vars(nlons,nlats,ntrunc,4)
  call sigio_srohdc(iunit,trim(filename),sighead,sigdata,ierr)
  !==> get winds on gaussian grid.
!$omp parallel do private(k)
  do k=1,nlevs
     call sptezv_s(sigdata%d(:,k),sigdata%z(:,k),ug(:,:,k),vg(:,:,k),1)
  enddo
!$omp end parallel do
  !==> get pressures on model levels.
  if (sighead%idvc .eq. 0) then ! sigma coordinate, old file format.
     ak = 0.
     bk = sighead%si(1:nlevs+1)
  else if (sighead%idvc .eq. 1) then ! sigma coordinate
     ak = 0.
     bk = sighead%vcoord(1:nlevs+1,2)
  else if (sighead%idvc .eq. 2 .or. sighead%idvc .eq. 3) then ! hybrid coordinate
     bk = sighead%vcoord(1:nlevs+1,2) 
     ak = 0.01*sighead%vcoord(1:nlevs+1,1)  ! convert to mb
  else
     print *,'unknown vertical coordinate type',sighead%idvc
  end if
  call sptez_s(sigdata%ps,psg,1)
  psg = 10.*exp(psg)
  do k=1,nlevs+1
     press(:,:,k) = ak(k)+bk(k)*psg
  enddo

  call sigio_axdata(sigdata,ierr)
  call sigio_sclose(iunit,ierr)

end subroutine getsigdata
