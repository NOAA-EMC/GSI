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
! abstract:  create ensemble mean and spread from NCEP GFS spectral files.
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
  use specmod
  use kinds, only: r_kind
  implicit none
  
  character(len=3) charnanal
  character(len=500) filenamein,filenameout,datapath,fileprefix
  integer iret,nlevs,ntrac,ntrunc,nanals,k,iunit,nlats,nlons
  integer nsize2,nsize3
  integer mype,mype1,npe,orig_group, new_group, new_comm
  integer,dimension(:),allocatable:: new_group_members
  real :: rnanals, rnanalsm1
  type(sigio_head) :: sigheadi
  type(sigio_data) :: sigdatai
  real, dimension(:),allocatable ::slats,swts
  real :: fha(5)
  integer :: idat(8), jdat(8)
  character(len=10) :: cdat(8)
  real(r_kind),dimension(:),allocatable:: tmpspec,divspec,vrtspec
  real(r_kind),dimension(:,:),allocatable::  psgi,psgs
  real(r_kind),dimension(:,:,:),allocatable:: ugi,ugs,vgi,vgs,tgi,tgs,qgi,qgs
  real(r_kind),dimension(:,:,:),allocatable::ozgi,ozgs,cwgi,cwgs
! mpi definitions.
  include 'mpif.h'

! Initialize mpi
!  mype is process number, npe is total number of processes.
  call mpi_init(iret)
  call MPI_Comm_rank(MPI_COMM_WORLD,mype,iret)
  call MPI_Comm_size(MPI_COMM_WORLD,npe,iret)
  mype1=mype+1

  if (mype==0) call w3tagb('GETSIGENSSTATP',2014,1025,0055,'NP25')

! Get user input from command line
  call getarg(1,datapath)
  call getarg(2,fileprefix)
  call getarg(3,charnanal)
  read(charnanal,'(i3)') nanals
  rnanals=nanals
  rnanals=1.0_8/rnanals
  rnanalsm1=nanals-1.0_8
  rnanalsm1=1.0_8/rnanalsm1

  if (mype==0) then
     write(6,*)' '
     write(6,*)'Command line input'
     write(6,*)' datapath      = ',trim(datapath)
     write(6,*)' fileprefix    = ',trim(fileprefix)
     write(6,*)' nanals,rnanals= ',nanals,rnanals
     write(6,*)' emean fileout = ',trim(fileprefix)//'_ensmean'
     write(6,*)' esprd fileout = ',trim(fileprefix)//'_enssprd'
  endif
  
  if (npe < nanals) then
     write(6,*)'***ERROR***  npe too small.  npe=',npe,' < nanals=',nanals
     call MPI_Abort(MPI_COMM_WORLD,99,iret)
     stop
  end if
  
  iunit = 21

! Create sub-communicator to handle number of cases (nanals)
  call mpi_comm_group(mpi_comm_world,orig_group,iret)

  allocate(new_group_members(nanals))
  do k=1,nanals
     new_group_members(k)=k-1
  end do
  if (mype1 <= nanals) then
     call mpi_group_incl(orig_group,nanals,new_group_members,new_group,iret)
  endif
  call mpi_comm_create(mpi_comm_world,new_group,new_comm,iret)
  if (iret.ne.0) then
     write(6,*)'***ERROR*** after mpi_comm_create with iret=',iret
     call mpi_abort(mpi_comm_world,101,iret)
  endif

! Process input files (one file per task)
  if (mype1 <= nanals) then
     
     write(charnanal,'(i3.3)') mype1
     filenamein = trim(adjustl(datapath))// &
          trim(adjustl(fileprefix))//'_mem'//charnanal
     
!    Read each ensemble member FHDFI forecast.
     call sigio_srohdc(iunit,trim(filenamein),sigheadi,sigdatai,iret)
     write(6,*)'Read ',trim(filenamein),' iret=',iret
     
     ntrunc  = sigheadi%jcap
     ntrac   = sigheadi%ntrac
     nlats   = sigheadi%latf
     nlons   = sigheadi%lonf
     nlevs   = sigheadi%levs
     nsize2  = nlons*nlats
     nsize3  = nsize2*nlevs

     if (mype==0) then
        write(6,*)'Read header information from ',trim(filenamein)
        write(6,*)' ntrunc = ',ntrunc
        write(6,*)' ntrac  = ',ntrac
        write(6,*)' nlats  = ',nlats
        write(6,*)' nlons  = ',nlons
        write(6,*)' nlevs  = ',nlevs
        write(6,*)' nsize2 = ',nsize2
        write(6,*)' nsize3 = ',nsize3
     endif

     call init_spec_vars(nlons,nlats,ntrunc,4)

     if (mype==0) then
        fha(:)=0.
        idat(:)=0
        jdat(:)=0
        fha(2) =sigheadi%fhour
        idat(1)=sigheadi%idate(4) ! year
        idat(2)=sigheadi%idate(2) ! month
        idat(3)=sigheadi%idate(3) ! day
        idat(4)=0                 ! time zone
        idat(5)=sigheadi%idate(1) ! hour
        call w3movdat(fha,idat,jdat)
        call w3pradat(jdat,cdat)

        allocate(slats(nlats))
        allocate(swts(nlats))
        call splat(4,nlats,slats,swts)
     endif

     allocate(psgi(nlons,nlats))
     allocate( ugi(nlons,nlats,nlevs))
     allocate( vgi(nlons,nlats,nlevs))
     allocate( tgi(nlons,nlats,nlevs))
     allocate( qgi(nlons,nlats,nlevs))
     allocate(ozgi(nlons,nlats,nlevs))
     allocate(cwgi(nlons,nlats,nlevs))

     allocate(psgs(nlons,nlats))
     allocate( ugs(nlons,nlats,nlevs))
     allocate( vgs(nlons,nlats,nlevs))
     allocate( tgs(nlons,nlats,nlevs))
     allocate( qgs(nlons,nlats,nlevs))
     allocate(ozgs(nlons,nlats,nlevs))
     allocate(cwgs(nlons,nlats,nlevs))

     ! convert spectral coeff's into grid-point values
     allocate(tmpspec((ntrunc+1)*(ntrunc+2)))
     allocate(divspec((ntrunc+1)*(ntrunc+2)))
     allocate(vrtspec((ntrunc+1)*(ntrunc+2)))
     tmpspec = sigdatai%ps
     call sptez_s(tmpspec,psgi,1)
     do k = 1,nlevs
        divspec = sigdatai%d(:,k) ; vrtspec = sigdatai%z(:,k)
        call sptezv_s(divspec,vrtspec,ugi(:,:,k),vgi(:,:,k),1)
        tmpspec = sigdatai%t(:,k)
        call sptez_s(tmpspec,   tgi(:,:,k),1)
        tmpspec = sigdatai%q(:,k,1)
        call sptez_s(tmpspec, qgi(:,:,k),1)
        tmpspec = sigdatai%q(:,k,2)
        call sptez_s(tmpspec,ozgi(:,:,k),1)
        tmpspec = sigdatai%q(:,k,3)
        call sptez_s(tmpspec,cwgi(:,:,k),1)
     enddo
     deallocate(tmpspec,divspec,vrtspec)
     call sigio_axdata(sigdatai,iret)
     call sigio_sclose(iunit,iret)

!    Compute ensemble sums.
     call mpi_allreduce(psgi,psgs,nsize2,mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce( ugi, ugs,nsize3,mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce( vgi, vgs,nsize3,mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce( tgi, tgs,nsize3,mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce( qgi, qgs,nsize3,mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(ozgi,ozgs,nsize3,mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(cwgi,cwgs,nsize3,mpi_real,mpi_sum,new_comm,iret)
     
!    Compute ensemble mean on all tasks
     psgs = psgs * rnanals
     ugs  =  ugs * rnanals
     vgs  =  vgs * rnanals
     tgs  =  tgs * rnanals
     qgs  =  qgs * rnanals
     ozgs = ozgs * rnanals
     cwgs = cwgs * rnanals

     if (mype==0) call write_to_disk('mean')

!    Compute ensemble perturbation squared on all tasks
     psgi = (psgi - psgs) * (psgi - psgs)
     ugi  = ( ugi -  ugs) * ( ugi -  ugs)
     vgi  = ( vgi -  vgs) * ( vgi -  vgs)
     tgi  = ( tgi -  tgs) * ( tgi -  tgs)
     qgi  = ( qgi -  qgs) * ( qgi -  qgs)
     ozgi = (ozgi - ozgs) * (ozgi - ozgs)
     cwgi = (cwgi - cwgs) * (cwgi - cwgs)

!    Compute ensemble perturbation squared sums.
     call mpi_allreduce(psgi,psgs,nsize2,mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce( ugi, ugs,nsize3,mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce( vgi, vgs,nsize3,mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce( tgi, tgs,nsize3,mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce( qgi, qgs,nsize3,mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(ozgi,ozgs,nsize3,mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(cwgi,cwgs,nsize3,mpi_real,mpi_sum,new_comm,iret)
     
!    Compute ensemble spread on all tasks
     psgs = sqrt(psgs * rnanalsm1)
     tgs  = sqrt( tgs * rnanalsm1)
     ugs  = sqrt( ugs * rnanalsm1)
     vgs  = sqrt( vgs * rnanalsm1)
     qgs  = sqrt( qgs * rnanalsm1)
     ozgs = sqrt(ozgs * rnanalsm1)
     cwgs = sqrt(cwgs * rnanalsm1)

     if (mype==0) call write_to_disk('spread')

! Jump here if more mpi processors than files to process
  else
     write(6,*) 'No files to process for mpi task = ',mype
  endif

  call mpi_barrier(mpi_comm_world,iret)

  if (mype1 <= nanals) then
     if (mype==0) deallocate(slats,swts)

     deallocate(psgi,psgs)
     deallocate( ugi, ugs)
     deallocate( vgi, vgs)
     deallocate( tgi, tgs)
     deallocate( qgi, qgs)
     deallocate(ozgi,ozgs)
     deallocate(cwgi,cwgs)

  endif

  if (mype==0) call w3tage('GETSIGENSSTATP')
  
 deallocate(new_group_members)
 
 call mpi_finalize(iret)
 stop

 contains

 subroutine write_to_disk(statstr)
   implicit none

   character(len=*), intent(in) :: statstr
   integer :: lunit

   filenameout = trim(adjustl(datapath))//trim(adjustl(fileprefix))//'_ens'//trim(adjustl(statstr))

   lunit = 63
   call baopenwt(lunit,trim(adjustl(filenameout))//'.bin',iret)
   write(6,*)'Write ',trim(adjustl(filenameout))//'.bin',' iret=',iret
   call wryte(lunit,4*nlons*nlats,      psgs)
   call wryte(lunit,4*nlons*nlats*nlevs, ugs)
   call wryte(lunit,4*nlons*nlats*nlevs, vgs)
   call wryte(lunit,4*nlons*nlats*nlevs, tgs)
   call wryte(lunit,4*nlons*nlats*nlevs, qgs)
   call wryte(lunit,4*nlons*nlats*nlevs,ozgs)
   call wryte(lunit,4*nlons*nlats*nlevs,cwgs)
   call baclose(lunit,iret)

   lunit = 64
   open(lunit,file=trim(adjustl(filenameout))//'.ctl',form='formatted',status='replace',iostat=iret)
   write(lunit,'("DSET ^",a)') trim(adjustl(fileprefix))//'_ens'//trim(adjustl(statstr))//'.bin'
   write(lunit,'("OPTIONS yrev")')
   write(lunit,'("UNDEF -9.99E+33")')
   write(lunit,'("TITLE ensemble",1x,a)') trim(adjustl(statstr))
   write(lunit,'("XDEF",i6," LINEAR",2f12.6)') nlons,0.0,360.0/nlons
   write(lunit,'("YDEF",i6," LEVELS")') nlats
   write(lunit,'(5f12.6)') 180.0/acos(-1.0)*asin(dble(slats(nlats:1:-1)))
   write(lunit,'("ZDEF",i6," LINEAR 1 1")') nlevs
   write(lunit,'("TDEF",i6," LINEAR ",i2.2,"Z",i2.2,a3,i4.4,1x,i6,"hr")')&
   1,jdat(5),jdat(3),cdat(2)(1:3),jdat(1),12
   write(lunit,'("VARS",i6)') 7
   write(lunit,'("PS  ",i3," 99 surface pressure (Pa)")') 1
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
