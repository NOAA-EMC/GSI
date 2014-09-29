program getsigensspread

 USE SIGIO_MODULE
 use specmod

 implicit none
 TYPE(SIGIO_HEAD) :: sigheadi,sigheadmean
 TYPE(SIGIO_DATA) :: sigdatai,sigdatamean
 character(len=500) filenamein,filenameout,datapath,fileprefix
 character(len=500) filenamemean
 integer iret,nlats,nlons,nlevs,ntrac,ntrunc,nanals,&
         i,k,nanal,numproc,nproc,iunit,iunit2,iunit3
 character(len=3) charnanal
 integer:: orig_group, new_group, new_comm
 integer,dimension(:),allocatable:: members
 real pi

 integer ncfggg,iunit4
 character(500):: grdfile

 real, dimension(:,:), allocatable :: psg,psg9,sprd
 real, dimension(:,:,:), allocatable :: ug,ug9,vg,vg9,tg,tg9

! mpi definitions.
 include 'mpif.h'

 call MPI_Init(iret)
 ! nproc is process number, numproc is total number of processes.
 call MPI_Comm_rank(MPI_COMM_WORLD,nproc,iret)
 call MPI_Comm_size(MPI_COMM_WORLD,numproc,iret)

 call getarg(1,datapath)
 call getarg(2,filenamemean)
 call getarg(3,filenameout)
 call getarg(4,fileprefix)
 call getarg(5,charnanal)
 read(charnanal,'(i3)') nanals
 filenameout = trim(adjustl(datapath))//filenameout

 pi = 4.*atan(1.0)

 if (numproc .lt. nanals) then
    print *,numproc,nanals
    print *,'warning, numproc too small!'
    flush(6)
    flush(0)
    call MPI_Abort(MPI_COMM_WORLD,101,iret)
    stop
 end if

 nanal = nproc+1

! Create sub-communicator to handle number of cases (nanals)
 call mpi_comm_group(mpi_comm_world,orig_group,iret)

 allocate(members(nanals))
 do i=1,nanals
    members(i)=i-1
 end do
 if (nanal .le. nanals) then
    call mpi_group_incl(orig_group,nanals,members,new_group,iret)
 endif
 call mpi_comm_create(mpi_comm_world,new_group,new_comm,iret)
 if (iret.ne.0) then
    write(6,*)'***ERROR*** after mpi_comm_create with iret=',iret
    flush(6)
    flush(0)
    call mpi_abort(mpi_comm_world,101,iret)
 endif


!! Have nproc zero read in ensemble mean
 if (nproc.eq.0) then
   iunit3 = 22
   write(6,*) 'NRPCOC, READ ENSEMBLE MEAN FILE : ',nproc,trim(filenamemean)
   call sigio_srohdc(iunit3,trim(filenamemean),sigheadmean,sigdatamean,iret)
   if (iret.ne.0) then
      write(6,*) 'PROBLEM READING ENSEMBLE MEAN FILE, NPROC : ',iret,nproc
      stop
   end if

   ntrunc = sigheadmean%jcap
   ntrac = sigheadmean%ntrac
   nlats = sigheadmean%latf
   nlons = sigheadmean%lonf
   nlevs = sigheadmean%levs
   
   call init_spec_vars(nlons,nlats,ntrunc,4)

   allocate(psg9(nlons,nlats),tg9(nlons,nlats,nlevs),ug9(nlons,nlats,nlevs), &
        vg9(nlons,nlats,nlevs))
   
   call sptez_s(sigdatamean%ps,psg9,1)
   do k=1,nlevs
      call sptez_s(sigdatamean%t(:,k),tg9(:,:,k),1)
      call sptezv_s(sigdatamean%d(:,k),sigdatamean%z(:,k),ug9(:,:,k),vg9(:,:,k),1)
   end do

 end if
 call mpi_barrier(mpi_comm_world,iret)


 if (nanal.le.nanals) then

 write(charnanal,'(i3.3)') nanal

 filenamein = trim(adjustl(datapath))// &
 trim(adjustl(fileprefix))//'_mem'//charnanal
 write(6,*)'process nanal=',nanal,' filenamein=',trim(filenamein)

! read each ensemble member FHDFI forecast.

 iunit = 21; iunit2 = 61
 call sigio_srohdc(iunit,trim(filenamein),sigheadi,sigdatai,iret)

 ntrunc = sigheadi%jcap
 ntrac = sigheadi%ntrac
 nlats = sigheadi%latf
 nlons = sigheadi%lonf
 nlevs = sigheadi%levs
 if (nproc .eq. 0) then
    print *,filenamein
    print *,'nlons,nlats,nlevs,ntrunc,ntrac=',nlons,nlats,nlevs,ntrunc,ntrac
 end if

 if (nproc.ne.0) then
    call init_spec_vars(nlons,nlats,ntrunc,4)
    call sigio_aldata(sigheadi,sigdatamean,iret)
    allocate(psg9(nlons,nlats),tg9(nlons,nlats,nlevs),ug9(nlons,nlats,nlevs), &
        vg9(nlons,nlats,nlevs))
 end if

! Broadcast ensemble mean stuff
 call mpi_bcast(psg9,nlons*nlats,mpi_real,0,new_comm,iret)
 call mpi_bcast(tg9,nlons*nlats*nlevs,mpi_real,0,new_comm,iret)
 call mpi_bcast(ug9,nlons*nlats*nlevs,mpi_real,0,new_comm,iret)
 call mpi_bcast(vg9,nlons*nlats*nlevs,mpi_real,0,new_comm,iret)

! Compute perturbations
 allocate(psg(nlons,nlats),tg(nlons,nlats,nlevs),ug(nlons,nlats,nlevs), &
        vg(nlons,nlats,nlevs),sprd(nlons,nlats))

 call sptez_s(sigdatai%ps,psg,1)
 do k=1,nlevs
    call sptez_s(sigdatai%t(:,k),tg(:,:,k),1)
    call sptezv_s(sigdatai%d(:,k),sigdatai%z(:,k),ug(:,:,k),vg(:,:,k),1)
 end do

 sprd(:,:) = (psg(:,:)-psg9(:,:))*(psg(:,:)-psg9(:,:))
 psg(:,:) = sprd(:,:)

 do k=1,nlevs
     sprd(:,:) = (ug(:,:,k)-ug9(:,:,k))*(ug(:,:,k)-ug9(:,:,k))
     ug(:,:,k) = sprd(:,:)

     sprd(:,:) = (vg(:,:,k)-vg9(:,:,k))*(vg(:,:,k)-vg9(:,:,k))
     vg(:,:,k) = sprd(:,:)

     sprd(:,:) = (tg(:,:,k)-tg9(:,:,k))*(tg(:,:,k)-tg9(:,:,k))
     tg(:,:,k) = sprd(:,:)
 end do

 tg9=0.
 ug9=0.
 vg9=0.
 psg9=0.

! compute ensemble perturbation sums.
 call mpi_allreduce(tg,tg9,nlons*nlats*nlevs,mpi_real,mpi_sum,new_comm,iret)
 call mpi_allreduce(ug,ug9,nlons*nlats*nlevs,mpi_real,mpi_sum,new_comm,iret)
 call mpi_allreduce(vg,vg9,nlons*nlats*nlevs,mpi_real,mpi_sum,new_comm,iret)
 call mpi_allreduce(psg,psg9,nlons*nlats,mpi_real,mpi_sum,new_comm,iret)

 if (nproc .eq. 0) then

! convert sums to means. 
    psg(:,:) = sqrt(psg9(:,:)/float(nanals))
    tg(:,:,:) = sqrt(tg9(:,:,:)/float(nanals))
    ug(:,:,:) = sqrt(ug9(:,:,:)/float(nanals))
    vg(:,:,:) = sqrt(vg9(:,:,:)/float(nanals))

! write out to binary file now
    grdfile=filenameout
    ncfggg=len_trim(grdfile)
    iunit4=63
    call baopenwt(iunit4,grdfile(1:ncfggg),iret)
    call wryte(iunit4,nlons*nlats*nlevs*4,tg)
    call wryte(iunit4,nlons*nlats*nlevs*4,ug)
    call wryte(iunit4,nlons*nlats*nlevs*4,vg)
    call wryte(iunit4,nlons*nlats*4,psg)
    call baclose(iunit4,iret)

! write out.
!!    sigheado%iens(1) = 1 ! unperturbed control
!!    sigheado%iens(2) = 2 ! low res control
!!    sigheado%icen2 = 2 ! sub-center, must be 2 or ens info not used
!!    call sigio_swohdc(iunit2,filenameout,sigheado,sigdatao,iret)
 end if 

 call sigio_axdata(sigdatai,iret)
 call sigio_sclose(iunit,iret)
!! call sigio_axdata(sigdatao,iret)
!! call sigio_sclose(iunit2,iret)


! Jump here if more mpi processors than files to process
 else
    write(6,*) 'no files to process for mpi task = ',nproc
 endif
   
 call mpi_barrier(mpi_comm_world,iret)
 if (nproc .eq. 0) write(6,*) 'all done!'

 deallocate(members)

 call mpi_finalize(iret)
 if (nproc .eq. 0 .and. iret .ne. 0) then
  print *, 'mpi_finalize error status = ',iret
 end if

end program getsigensspread
