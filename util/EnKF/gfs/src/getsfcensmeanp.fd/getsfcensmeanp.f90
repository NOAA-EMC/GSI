program getsfcensmeanp
!$$$  main program documentation block
!
! program:  getsfcensmean              compute ensemble mean
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract:  compute ensemble mean surface file
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

! create ensemble mean NCEP GFS surface file.
  use sfcio_module
  implicit none

  character*500 filenamein,filenameout,datapath,fileprefix
  character*3 charnanal
  integer lunin,lunout,iret,nanals,k
  integer mype,mype1,npe,orig_group, new_group, new_comm
  integer,dimension(:),allocatable:: new_group_members
  real(8) rnanals

  type(sfcio_head):: sfcheadi, sfcheado
  type(sfcio_data):: sfcdatai, sfcdatao

! mpi definitions.
  include 'mpif.h'

! Initialize mpi
!  mype is process number, npe is total number of processes.
  call mpi_init(iret)
  call MPI_Comm_rank(MPI_COMM_WORLD,mype,iret)
  call MPI_Comm_size(MPI_COMM_WORLD,npe,iret)
  mype1=mype+1

  if (mype==0) call w3tagb('GETSFCENSMEAN',2011,0319,0055,'NP25')

! Get user input from command line
  call getarg(1,datapath)
  call getarg(2,filenameout)
  call getarg(3,fileprefix)
  call getarg(4,charnanal)
  read(charnanal,'(i2)') nanals
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

  lunin=21
  lunout=61

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

! Process input files (one file per task)
  if (mype1 <= nanals) then

     write(charnanal,'(i3.3)') mype1
     filenamein = trim(adjustl(datapath))// &
          trim(adjustl(fileprefix))//'_mem'//charnanal
     call sfcio_srohdc(lunin,filenamein,sfcheadi,sfcdatai,iret)
     write(6,*)'Read ',trim(filenamein),' iret=',iret

!   sfcio_data        Surface file data fields
!     tsea              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       surface temperature in K
!     smc               Real(sfcio_realkind)(:,:,:) pointer to lonb*latb*lsoil
!                       soil volumetric water content in fraction
!     sheleg            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       snow depth in m
!     stc               Real(sfcio_realkind)(:,:,:) pointer to lonb*latb*lsoil
!                       soil temperature in K
!     tg3               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       deep soil temperature in K
!     zorl              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       roughness in cm
!     cv                Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       convective cloud cover in fraction
!     cvb               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       convective cloud bottom in kpa
!     cvt               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       convective cloud top in kpa
!     alvsf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       albedo for visible scattered in fraction
!     alvwf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       albedo for visible beam in fraction
!     alnsf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       albedo for near-IR scattered in fraction
!     alnwf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       albedo for near-IR beam in fraction
!     slmsk             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       sea-land-ice mask (0-sea, 1-land, 2-ice)
!     vfrac             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       vegetation fraction in fraction
!     canopy            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       canopy water in m
!     f10m              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       10-meter wind speed over lowest model wind speed
!     t2m               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       2-meter temperature in K
!     q2m               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       2-meter specific humidity in kg/kg
!     vtype             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       vegetation type in integer 1-13
!     stype             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       soil type in integer 1-9
!     facsf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in fraction
!     facwf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in fraction
!     uustar            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     ffmm              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     ffhh              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     hice              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     fice              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     tisfc             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     tprcp             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     srflag            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     snwdph            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     slc               Real(sfcio_realkind)(:,:,:) pointer to lonb*latb*lsoil
!                       xxx in xxx
!     shdmin            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     shdmax            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     slope             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       slope type
!     snoalb            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     orog              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       orography in m

     call sfcio_aldata(sfcheadi,sfcdatao,iret)
     sfcheado = sfcheadi

!    These fields are fixed.  Do not compute mean
     sfcdatao%slmsk  = sfcdatai%slmsk
     sfcdatao%vtype = sfcdatai%vtype
     sfcdatao%stype = sfcdatai%stype
     sfcdatao%slope = sfcdatai%slope
     sfcdatao%orog  = sfcdatai%orog

!    Use mpi_reduce to sum fields.  Compute mean
     call mpi_allreduce(sfcdatai%tsea,sfcdatao%tsea,    size(sfcdatai%tsea),  mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%smc,sfcdatao%smc,      size(sfcdatai%smc),   mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%sheleg,sfcdatao%sheleg,size(sfcdatai%sheleg),mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%stc,sfcdatao%stc,      size(sfcdatai%stc),   mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%tg3,sfcdatao%tg3,      size(sfcdatai%tg3),   mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%zorl,sfcdatao%zorl,    size(sfcdatai%zorl),  mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%cv,sfcdatao%cv,        size(sfcdatai%cv),    mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%cvb,sfcdatao%cvb,      size(sfcdatai%cvb),   mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%cvt,sfcdatao%cvt,      size(sfcdatai%cvt),   mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%alvsf,sfcdatao%alvsf,  size(sfcdatai%alvsf), mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%alvwf,sfcdatao%alvwf,  size(sfcdatai%alvwf), mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%alnsf,sfcdatao%alnsf,  size(sfcdatai%alnsf), mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%alnwf,sfcdatao%alnwf,  size(sfcdatai%alnwf), mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%vfrac,sfcdatao%vfrac,  size(sfcdatai%vfrac), mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%canopy,sfcdatao%canopy,size(sfcdatai%canopy),mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%f10m,sfcdatao%f10m,    size(sfcdatai%f10m),  mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%t2m,sfcdatao%t2m,      size(sfcdatai%t2m),   mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%q2m,sfcdatao%q2m,      size(sfcdatai%q2m),   mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%facsf,sfcdatao%facsf,  size(sfcdatai%facsf), mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%facwf,sfcdatao%facwf,  size(sfcdatai%facwf), mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%uustar,sfcdatao%uustar,size(sfcdatai%uustar),mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%ffmm,sfcdatao%ffmm,    size(sfcdatai%ffmm),  mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%ffhh,sfcdatao%ffhh,    size(sfcdatai%ffhh),  mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%hice,sfcdatao%hice,    size(sfcdatai%hice),  mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%fice,sfcdatao%fice,    size(sfcdatai%fice),  mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%tisfc,sfcdatao%tisfc,  size(sfcdatai%tisfc), mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%tprcp,sfcdatao%tprcp,  size(sfcdatai%tprcp), mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%srflag,sfcdatao%srflag,size(sfcdatai%srflag),mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%snwdph,sfcdatao%snwdph,size(sfcdatai%snwdph),mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%slc,sfcdatao%slc,      size(sfcdatai%slc),   mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%shdmin,sfcdatao%shdmin,size(sfcdatai%shdmin),mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%shdmax,sfcdatao%shdmax,size(sfcdatai%shdmax),mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(sfcdatai%snoalb,sfcdatao%snoalb,size(sfcdatai%snoalb),mpi_real,mpi_sum,new_comm,iret)


     sfcdatao%tsea	=sfcdatao%tsea   * rnanals
     sfcdatao%smc	=sfcdatao%smc	 * rnanals
     sfcdatao%sheleg	=sfcdatao%sheleg * rnanals
     sfcdatao%stc	=sfcdatao%stc	 * rnanals
     sfcdatao%tg3 	=sfcdatao%tg3	 * rnanals
     sfcdatao%zorl	=sfcdatao%zorl   * rnanals
     sfcdatao%cv	=sfcdatao%cv	 * rnanals
     sfcdatao%cvb	=sfcdatao%cvb	 * rnanals
     sfcdatao%cvt	=sfcdatao%cvt	 * rnanals
     sfcdatao%alvsf	=sfcdatao%alvsf  * rnanals
     sfcdatao%alvwf	=sfcdatao%alvwf  * rnanals
     sfcdatao%alnsf	=sfcdatao%alnsf  * rnanals
     sfcdatao%alnwf	=sfcdatao%alnwf  * rnanals
     sfcdatao%vfrac	=sfcdatao%vfrac  * rnanals
     sfcdatao%canopy	=sfcdatao%canopy * rnanals
     sfcdatao%f10m	=sfcdatao%f10m   * rnanals
     sfcdatao%t2m	=sfcdatao%t2m    * rnanals
     sfcdatao%q2m	=sfcdatao%q2m    * rnanals
     sfcdatao%facsf	=sfcdatao%facsf  * rnanals
     sfcdatao%facwf	=sfcdatao%facwf  * rnanals
     sfcdatao%uustar	=sfcdatao%uustar * rnanals
     sfcdatao%ffmm	=sfcdatao%ffmm   * rnanals
     sfcdatao%ffhh	=sfcdatao%ffhh   * rnanals
     sfcdatao%hice	=sfcdatao%hice   * rnanals
     sfcdatao%fice	=sfcdatao%fice   * rnanals
     sfcdatao%tisfc	=sfcdatao%tisfc  * rnanals
     sfcdatao%tprcp      =sfcdatao%tprcp * rnanals
     sfcdatao%srflag	=sfcdatao%srflag * rnanals
     sfcdatao%snwdph	=sfcdatao%snwdph * rnanals
     sfcdatao%slc	=sfcdatao%slc    * rnanals
     sfcdatao%shdmin	=sfcdatao%shdmin * rnanals
     sfcdatao%shdmax	=sfcdatao%shdmax * rnanals
     sfcdatao%snoalb	=sfcdatao%snoalb * rnanals

     call sfcio_axdata(sfcdatai,iret)

     if (mype==0) then
        call sfcio_swohdc(lunout,filenameout,sfcheado,sfcdatao,iret)
        write(6,*)'Write ensemble mean ',trim(filenameout),' iret=',iret
     endif

! Jump here if more mpi processors than files to process
  else
     write(6,*) 'No files to process for mpi task = ',mype
  endif

  call mpi_barrier(mpi_comm_world,iret)

  if (mype==0) then
     write(6,*) 'all done!'
     call w3tage('GETSFCENSMEAN')
  endif

  deallocate(new_group_members)

  call mpi_finalize(iret)

END program getsfcensmeanp
