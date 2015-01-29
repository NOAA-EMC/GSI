program calc_corrlength
! mpif90 -O3 -xHOST -traceback -assume byterecl -openmp -o calc_corrlength.x calc_corrlength.f90 specmod.o sigio_module_gfs.o constants.o ../../../gfsenkf/lib/libsp_4s.a
! uses Gaussian-based length scale estimate (eq 5 in Pannekoucke, Berre and
! Desroziers paper QJR 2008 497-708)
 USE SIGIO_MODULE
 use constants, only: rd, grav, cp, rearth, init_constants, init_constants_derived, pi
 use specmod
 implicit none
 real, parameter :: clip=1.e-6
 ! if use_height is .true., use geopot height to compute length scales.
 logical, parameter :: use_height=.false. 
 ! if use_height is .false. and use_psi is .true., use streamfunction.
 ! if both are .false., zonal wind will be used.
 logical, parameter :: use_psi=.true.
 TYPE(SIGIO_HEAD) :: sigheadi
 TYPE(SIGIO_DATA) :: sigdatai
 character(len=500) filenamein
 character(len=10) datestring
 integer iret,nlats,nlons,nlevs,ntrac,ntrunc,k,ierr,nanals,&
              nanal,numproc,nproc,iunit,&
              i,j,nskip
 real delta,coslat
 character(len=4) charnlons,charnlats,charnanal
 real, dimension(:,:), allocatable :: deltav, psg, psgvar,&
 psgmean, psgcov, psgcorr
 real, dimension(:,:,:), allocatable :: psigshiftn,psigshifts,&
 psig,psigmean,psigvar,psigcorrn,psigcorrs,pressl,vlengthscale,&
 psigshiftnvar,psigshiftsvar,psigshiftncov,psigshiftscov,lengthscalex,lengthscaley
! mpi definitions.
 include 'mpif.h'

 call MPI_Init(ierr)
 ! nproc is process number, numproc is total number of processes.
 call MPI_Comm_rank(MPI_COMM_WORLD,nproc,ierr)
 call MPI_Comm_size(MPI_COMM_WORLD,numproc,ierr)

 ! get nanals,datestring from command line.
 call getarg(1,charnlons)
 read(charnlons,'(i4)') nanals
 call getarg(2,datestring)
 call getarg(3,charnlons)
 call getarg(4,charnlats)
 read(charnlons,'(i4)') nlons
 read(charnlats,'(i4)') nlats

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
 filenamein = "sfg_"//datestring//"_fhr06_mem001"
 iunit = 7
 call sigio_sropen(iunit,trim(filenamein),ierr)
 if (ierr .ne. 0) then
    print *,'cannot read file ',filenamein,ierr
    flush(6)
    flush(0)
    call MPI_Abort(MPI_COMM_WORLD,101,ierr)
    stop
 end if
 call sigio_srhead(iunit,sigheadi,ierr)
 call sigio_sclose(iunit,ierr)

 ntrunc = sigheadi%jcap
 ntrac = sigheadi%ntrac
 ! if nlons, nlats not given by env vars, use value in sigma file.
 if (nlons <= 0 .or. nlats <= 0) then
    nlats = sigheadi%latf
    nlons = sigheadi%lonf
 endif
 nlevs = sigheadi%levs
 !if (nproc .eq. 0) then
 !   print *,filenamein
 !   print *,'nlons,nlats,nlevs,ntrunc,ntrac=',nlons,nlats,nlevs,ntrunc,ntrac
 !endif

 nanal = nproc + 1
 write(charnanal,'(i3.3)') nanal

 allocate(psig(nlons,nlats,nlevs))
 allocate(psg(nlons,nlats))
 allocate(psgmean(nlons,nlats))
 allocate(psgvar(nlons,nlats))
 allocate(psgcorr(nlons,nlats))
 allocate(psgcov(nlons,nlats))
 allocate(pressl(nlons,nlats,nlevs))
 allocate(psigmean(nlons,nlats,nlevs))
 allocate(psigvar(nlons,nlats,nlevs))
 allocate(psigshiftn(nlons,nlats,nlevs))
 allocate(psigshifts(nlons,nlats,nlevs))
 allocate(psigshiftnvar(nlons,nlats,nlevs))
 allocate(psigshiftsvar(nlons,nlats,nlevs))
 allocate(psigshiftncov(nlons,nlats,nlevs))
 allocate(psigshiftscov(nlons,nlats,nlevs))
 allocate(psigcorrn(nlons,nlats,nlevs))
 allocate(psigcorrs(nlons,nlats,nlevs))
 allocate(lengthscalex(nlons,nlats,nlevs))
 allocate(lengthscaley(nlons,nlats,nlevs))
 allocate(vlengthscale(nlons,nlats,nlevs))
 allocate(deltav(nlons,nlats))

 call init_spec_vars(nlons,nlats,ntrunc,4)

! read each ensemble member FHDFI forecast.

 filenamein = "sfg_"//datestring//"_fhr06_mem"//charnanal
 !print *,trim(filenamein)
 call sigio_srohdc(iunit,trim(filenamein),sigheadi,sigdatai,iret)
 call getsigdata(sigheadi,sigdatai,psig,psg,pressl,nlons,nlats,nlevs,sigheadi%jcap,mpi_comm_world,use_height,use_psi)
 !print *,'nanal =',nanal,' min/max psig, pressl = ',&
 !minval(psig),maxval(psig),minval(pressl),maxval(pressl)
 call sigio_axdata(sigdatai,ierr)
 call sigio_sclose(iunit,ierr)
!  compute ensemble means
 call mpi_allreduce(psig,psigmean,nlons*nlats*nlevs,mpi_real,mpi_sum,mpi_comm_world,ierr)
 psigmean = psigmean/float(nanals)
! compute perturbations.
 psig = psig-psigmean

! compute horizontal length scale.
! shift north and south by 1 grid point.
 do j=2,nlats
    psigshiftn(:,j,:) = psig(:,j-1,:)
 enddo
 do j=1,nlats-1
    psigshifts(:,j,:) = psig(:,j+1,:)
 enddo
 psigshiftn(:,1,:) = psigshifts(:,1,:)
 psigshifts(:,nlats,:) = psigshiftn(:,nlats,:)
! calculate variances
 call mpi_allreduce(psig**2,psigvar,nlons*nlats*nlevs,mpi_real,mpi_sum,mpi_comm_world,ierr)
 psigvar = psigvar/float(nanals-1)
 where(psigvar < clip) psigvar = clip
 psigvar = sqrt(psigvar)
 where(psigvar < clip) psigvar = clip
 call mpi_allreduce(psigshiftn**2,psigshiftnvar,nlons*nlats*nlevs,mpi_real,mpi_sum,mpi_comm_world,ierr)
 psigshiftnvar = psigshiftnvar/float(nanals-1)
 call mpi_allreduce(psigshifts**2,psigshiftsvar,nlons*nlats*nlevs,mpi_real,mpi_sum,mpi_comm_world,ierr)
 psigshiftsvar = psigshiftsvar/float(nanals-1)
 where(psigshiftnvar < clip) psigshiftnvar = clip
 psigshiftnvar = sqrt(psigshiftnvar)
 where(psigshiftnvar < clip) psigshiftnvar = clip
 where(psigshiftsvar < clip) psigshiftsvar = clip
 psigshiftsvar = sqrt(psigshiftsvar)
 where(psigshiftsvar < clip) psigshiftsvar = clip
! calculate covariances
 call mpi_allreduce(psigshiftn*psig,psigshiftncov,nlons*nlats*nlevs,mpi_real,mpi_sum,mpi_comm_world,ierr)
 psigshiftncov = psigshiftncov/float(nanals-1)
 call mpi_allreduce(psigshifts*psig,psigshiftscov,nlons*nlats*nlevs,mpi_real,mpi_sum,mpi_comm_world,ierr)
 psigshiftscov = psigshiftscov/float(nanals-1)
! compute correlation
 psigcorrn = psigshiftncov/(psigshiftnvar*psigvar)
 where (psigcorrn < clip) psigcorrn = clip
 where (psigcorrn > 1.-clip) psigcorrn = 1.-clip
 psigcorrs = psigshiftscov/(psigshiftsvar*psigvar)
 where (psigcorrs < clip) psigcorrs = clip
 where (psigcorrs > 1.-clip) psigcorrs = 1.-clip
! calculate length scale (eqn 5 in Pannekoucke, Berre and Desroziers)
 delta = 0. ! av lat spacing in radians (nearly constant)
 do j=2,nlats
    delta = delta + abs(asin(gaulats(j))-asin(gaulats(j-1)))/(nlats-1)
    !if (nproc .eq. 0) print *,j,abs(asin(gaulats(j))-asin(gaulats(j-1)))
 enddo
 !if (nproc .eq. 0) print *,'delta = ',delta
 lengthscaley = 0.5*rearth*delta*((1./sqrt(-2.*log(psigcorrn))) + &
                           (1./sqrt(-2.*log(psigcorrs))))

! compute horizontal length scale.
! shift east and west by nskip grid points
! nskip set so that deltax is nearly constant with latitude.
 do j=1,nlats
    coslat = sqrt(1.-gaulats(j)**2)
    nskip = int(1.0/coslat)
    if (nskip .lt. 1) nskip = 1
    do i=1,nlons-nskip
       psigshiftn(i,j,:) = psig(i+nskip,j,:)
    enddo
    do i=nlons-nskip+1,nlons
       psigshiftn(i,j,:) = psig(nlons-i+1,j,:)
    enddo
    do i=1+nskip,nlons
       psigshifts(i,j,:) = psig(i-nskip,j,:)
    enddo
    do i=1,nskip
       psigshifts(i,j,:) = psig(nlons-i+1,j,:)
    enddo
 enddo
! do i=1,nlons-1
!    psigshiftn(i,:,:) = psig(i+1,:,:)
! enddo
! psigshiftn(nlons,:,:) = psig(1,:,:)
! do i=2,nlons
!    psigshiftn(i,:,:) = psig(i-1,:,:)
! enddo
! psigshiftn(1,:,:) = psig(nlons,:,:)
!! calculate variances
 call mpi_allreduce(psig**2,psigvar,nlons*nlats*nlevs,mpi_real,mpi_sum,mpi_comm_world,ierr)
 psigvar = psigvar/float(nanals-1)
 where(psigvar < clip) psigvar = clip
 psigvar = sqrt(psigvar)
 where(psigvar < clip) psigvar = clip
 call mpi_allreduce(psigshiftn**2,psigshiftnvar,nlons*nlats*nlevs,mpi_real,mpi_sum,mpi_comm_world,ierr)
 psigshiftnvar = psigshiftnvar/float(nanals-1)
 call mpi_allreduce(psigshifts**2,psigshiftsvar,nlons*nlats*nlevs,mpi_real,mpi_sum,mpi_comm_world,ierr)
 psigshiftsvar = psigshiftsvar/float(nanals-1)
 where(psigshiftnvar < clip) psigshiftnvar = clip
 psigshiftnvar = sqrt(psigshiftnvar)
 where(psigshiftnvar < clip) psigshiftnvar = clip
 where(psigshiftsvar < clip) psigshiftsvar = clip
 psigshiftsvar = sqrt(psigshiftsvar)
 where(psigshiftsvar < clip) psigshiftsvar = clip
! calculate covariances
 call mpi_allreduce(psigshiftn*psig,psigshiftncov,nlons*nlats*nlevs,mpi_real,mpi_sum,mpi_comm_world,ierr)
 psigshiftncov = psigshiftncov/float(nanals-1)
 call mpi_allreduce(psigshifts*psig,psigshiftscov,nlons*nlats*nlevs,mpi_real,mpi_sum,mpi_comm_world,ierr)
 psigshiftscov = psigshiftscov/float(nanals-1)
! compute correlation
 psigcorrn = psigshiftncov/(psigshiftnvar*psigvar)
 where (psigcorrn < clip) psigcorrn = clip
 where (psigcorrn > 1.-clip) psigcorrn = 1.-clip
 psigcorrs = psigshiftscov/(psigshiftsvar*psigvar)
 where (psigcorrs < clip) psigcorrs = clip
 where (psigcorrs > 1.-clip) psigcorrs = 1.-clip
 do j=1,nlats
    coslat = sqrt(1.-gaulats(j)**2)
    nskip = int(1.0/coslat)
    if (nskip .lt. 1) nskip = 1
    delta = 2.*pi*coslat*nskip/nlons
    lengthscalex(:,j,:) = 0.5*rearth*delta*((1./sqrt(-2.*log(psigcorrn(:,j,:)))) + &
                                     (1./sqrt(-2.*log(psigcorrs(:,j,:)))))
 enddo


! now compute vertical length scale.
! shift up and down by 1 grid point.
 do k=2,nlevs
    psigshiftn(:,:,k) = psig(:,:,k-1)
 enddo
 do k=1,nlevs-1
    psigshifts(:,:,k) = psig(:,:,k+1)
 enddo
 psigshiftn(:,:,1) = psigshifts(:,:,1)
 psigshifts(:,nlevs,k) = psigshiftn(:,nlevs,k)
 call mpi_allreduce(psigshiftn**2,psigshiftnvar,nlons*nlats*nlevs,mpi_real,mpi_sum,mpi_comm_world,ierr)
 psigshiftnvar = psigshiftnvar/float(nanals-1)
 call mpi_allreduce(psigshifts**2,psigshiftsvar,nlons*nlats*nlevs,mpi_real,mpi_sum,mpi_comm_world,ierr)
 psigshiftsvar = psigshiftsvar/float(nanals-1)
 where(psigshiftnvar < clip) psigshiftnvar = clip
 psigshiftnvar = sqrt(psigshiftnvar)
 where(psigshiftnvar < clip) psigshiftnvar = clip
 where(psigshiftsvar < clip) psigshiftsvar = clip
 psigshiftsvar = sqrt(psigshiftsvar)
 where(psigshiftsvar < clip) psigshiftsvar = clip
! calculate covariances
 call mpi_allreduce(psigshiftn*psig,psigshiftncov,nlons*nlats*nlevs,mpi_real,mpi_sum,mpi_comm_world,ierr)
 psigshiftncov = psigshiftncov/float(nanals-1)
 call mpi_allreduce(psigshifts*psig,psigshiftscov,nlons*nlats*nlevs,mpi_real,mpi_sum,mpi_comm_world,ierr)
 psigshiftscov = psigshiftscov/float(nanals-1)
! compute correlation
 psigcorrn = psigshiftncov/(psigshiftnvar*psigvar)
 where (psigcorrn < clip) psigcorrn = clip
 where (psigcorrn > 1.-clip) psigcorrn = 1.-clip
 psigcorrs = psigshiftscov/(psigshiftsvar*psigvar)
 where (psigcorrs < clip) psigcorrs = clip
 where (psigcorrs > 1.-clip) psigcorrs = 1.-clip
! calculate length scale (eqn 5 in Pannekoucke, Berre and Desroziers)
 do k=2,nlevs
    deltav = abs(pressl(:,:,k)-pressl(:,:,k-1))
    vlengthscale(:,:,k) = deltav*(1./sqrt(-2.*log(psigcorrn(:,:,k)))) 
 enddo
 do k=1,nlevs-1
    deltav = abs(pressl(:,:,k)-pressl(:,:,k+1))
    if (k .eq. 1) then
    vlengthscale(:,:,k) = deltav*(1./sqrt(-2.*log(psigcorrs(:,:,k)))) 
    else
    vlengthscale(:,:,k) = 0.5*vlengthscale(:,:,k) + &
                          0.5*deltav*(1./sqrt(-2.*log(psigcorrs(:,:,k)))) 
    endif
 enddo

 if (nproc .eq. 0) then
 do k=1,nlevs
  print *,k,'lengthscalex',minval(lengthscalex(:,:,k)),maxval(lengthscalex(:,:,k))
  print *,k,'lengthscaley',minval(lengthscaley(:,:,k)),maxval(lengthscaley(:,:,k))
  print *,k,'vlengthscale',minval(vlengthscale(:,:,k)),maxval(vlengthscale(:,:,k))
 enddo
 if (use_height) then
    open(9,form='unformatted',access='direct',file='lscalez.dat',recl=4*nlons*nlats*nlevs)
 else if (use_psi) then
    open(9,form='unformatted',access='direct',file='lscalepsi.dat',recl=4*nlons*nlats*nlevs)
 else
    open(9,form='unformatted',access='direct',file='lscaleu.dat',recl=4*nlons*nlats*nlevs)
 endif
 write(9,rec=1) lengthscalex
 write(9,rec=2) lengthscaley
 write(9,rec=3) vlengthscale
 close(9)
 endif 

 deallocate(psg,psgvar,psgcov,psgcorr)
 deallocate(psig,pressl,psigmean,psigvar)
 deallocate(psigshiftn,psigshifts,psigshiftnvar,psigshiftsvar)
 deallocate(psigshiftncov,psigshiftscov,psigcorrn,psigcorrs)
 deallocate(lengthscalex,lengthscaley,vlengthscale)
 deallocate(deltav)

 call MPI_Barrier(MPI_COMM_WORLD,ierr)
 if (nproc .eq. 0) write(6,*) 'all done!'
 call MPI_Finalize(ierr)
 if (nproc .eq. 0 .and. ierr .ne. 0) then
  print *, 'MPI_Finalize error status = ',ierr
 end if
end program calc_corrlength

 subroutine getsigdata(sighead,sigdata,psig,psg,pressl,nlons,nlats,nlevs,ntrunc,mpi_comm_world,use_height,use_psi)
  use sigio_module
  use specmod
  use constants, only: rd, grav, cp, rearth, init_constants, init_constants_derived
  implicit none
  type (sigio_data), intent(in out) :: sigdata
  type (sigio_head), intent(in) :: sighead
  real, dimension(nlons,nlats,nlevs), intent(out) :: psig,pressl
  real, dimension(nlons,nlats,nlevs) :: tempg
  real, dimension(nlons,nlats,nlevs+1) :: pressi
  real, dimension(nlons,nlats), intent(out) :: psg
  real, dimension(nlons,nlats) :: zsg
  real ak(nlevs+1),bk(nlevs+1)
  real invlap((ntrunc+1)*(ntrunc+2))
  integer, intent(in) :: ntrunc,nlevs,nlons,nlats,mpi_comm_world
  integer k,ierr,n,m,nm
  real kap,kapr,kap1
  logical,intent(in) :: use_height, use_psi
   
  kap = rd/cp
  kapr = cp/rd
  kap1 = kap + 1.
  if (ntrunc .lt. 0) then
    print *,'illegal ntrunc = ',ntrunc
    call MPI_Abort(MPI_COMM_WORLD,101,ierr)
    stop
  endif
  call init_spec_vars(nlons,nlats,ntrunc,4)
  if (.not. use_height .and. use_psi) then
     nm = 1
     do m=0,ntrunc
        do n=m,ntrunc
           if (n .gt. 0) then
              !invlap(nm) = -rearth**2/(real(n)*real(n+1))
              invlap(nm) = -rearth**2/sqrt(real(n)*real(n+1))
           else
              invlap(nm) = 0.
           end if
           invlap(nm+1) = invlap(nm)
           nm = nm + 2
        enddo
     enddo
  endif
  !==> get streamfunction on gaussian grid.
  do k=1,nlevs
     if (use_height) then
        call sptez_s(sigdata%t(:,k),tempg(:,:,k),1)
     else
        if (use_psi) then
           sigdata%z(:,k) = sigdata%z(:,k)*invlap(:)
           call sptez_s(sigdata%z(:,k),psig(:,:,k),1)
        else
           ! zonal wind
           call sptezv_s(sigdata%d(:,k),sigdata%z(:,k),psig(:,:,k),tempg(:,:,k),1)
        endif
     endif
  enddo
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
  if (use_height) call sptez_s(sigdata%hs,zsg,1)
  psg = 10.*exp(psg)
  do k=1,nlevs+1
     pressi(:,:,k) = ak(k)+bk(k)*psg
  enddo
  do k=1,nlevs
    ! layer pressure from Phillips vertical interpolation.
    pressl(:,:,k) = ((pressi(:,:,k)**kap1-pressi(:,:,k+1)**kap1)/&
                  (kap1*(pressi(:,:,k)-pressi(:,:,k+1))))**kapr
  end do
  ! use geopotential instead of streamfunction.
  if (use_height) then
     call temptoz(nlons,nlats,nlevs,real(rd),real(cp),real(grav),pressi,pressl,zsg,tempg,psig)
  endif
  ! convert to -log(p/ps)
  do k=1,nlevs
     pressl(:,:,k) = -log(pressl(:,:,k)/psg)
  enddo

end subroutine getsigdata

 subroutine temptoz(nlons,nlats,nlevs,rgas,cp,grav,pint,pl,zs,tv,z)
! compute z (geopot height) on interfaces, given 
! pint (interface pressure in hPa),
! pl (pressure at mid-layers in hPa), tv (virtual temp at mid-layers) and
! zs (surface orog). rgas,cp,grav are gas constant, specific heat and gravity.
! z does not include surface height (k=1 is 1st level, k=nlevs is model top)
! uses hydrostatic eqn d(phi)/d(pi) = -thetav, where phi is geopot. height,
! pi is exner function and thetav is virtual potential temp.
  implicit none
  integer, intent(in) :: nlons,nlats,nlevs
  real, dimension(nlons,nlats, nlevs) :: thetav,pil
  real, dimension(nlons,nlats, nlevs+1) :: pii
  real, intent(in), dimension(nlons,nlats,nlevs) :: tv,pl
  real, intent(in), dimension(nlons,nlats,nlevs+1) :: pint
  real, intent(out), dimension(nlons,nlats,nlevs) :: z
  real, intent(in), dimension(nlons,nlats) :: zs
  real, intent(in) :: rgas,cp,grav
  integer i,j,k
  real dz
 
  pii = cp*(pint/1.e3)**(rgas/cp)
  pil = cp*(pl/1.e3)**(rgas/cp)
  thetav = cp*tv/pil
  do j=1,nlats
  do i=1,nlons
     dz = -thetav(i,j,1) * (pii(i,j,2)-pii(i,j,1))
     z(i,j,1) = grav*zs(i,j) + dz
     do k=3,nlevs+1
        dz = -thetav(i,j,k-1) * (pii(i,j,k)-pii(i,j,k-1))
        z(i,j,k-1) = z(i,j,k-2) + dz
     end do
  end do
  enddo
  z = z/grav
 
 end subroutine temptoz
