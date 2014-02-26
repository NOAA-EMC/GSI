program rdr2tdr_fy3ri_mwri

!-------------------------------------------------------------------------------
!program to transfer the raw L1 FY3-MWRI data(in HDf4 format,now 
!name it as RDR dataset) into the data format needed in MIRS 
!system(so called TDR format)
!
!input:
!
!
!
!output:
!
!
!
!module used:
!     iflib
!     file_utility: use get_lun()function included in the module 
!
!used by
!     none
!
!author                 date             ver        modify
!Tiger.Yang@imsg        02/04/2009       1.0    
!
!-------------------------------------------------------------------------------
  !module used
  USE Consts
  USE misc
  USE IO_Misc
  USE IO_MeasurData
  USE ErrorHandling
  implicit none

  !----------------------------------------------------------------------------------
  !...intrinsic/external functions
  intrinsic          :: FLOOR, MOD, TRIM, ABS
  external           :: PACKTIMEQQ, UNPACKTIMEQQ
  
  !----------------------------------------------------------------------------------
  !...following is the function and key words definition of hdf4 Libarary

  !-----------------------Kdy words--------------------------------------------------
  integer            ::    ds_type,MVC_DS_TYPE,MVC_DS_ATTS,ds_atts,status
  integer            ::    valid_range,maxs,mins
  integer            ::    n_records, interlace_mode, vdata_size,vdata_ref,file_id_A,file_id_d
  integer            ::    vdata_id_A,vdata_id_d,record_index,record_pos,num_of_records

  !---------------------------------------------------------------------------------------
  integer(4)         ::    sd_id,sds_id,sds_idx
  integer(4)         ::    ds_dims(2),ds_start(2),ds_edges(2),ds_dims_3d(3),ds_start_3d(3),ds_edges_3d(3)
  integer(4)         ::    ds_stride(2),ds_stride_3d(3),ds_ndims

  !-----------------------functions-------------------------------------------------------
  integer            ::   iu,iu_rdr,iu_tdr
  integer            ::   N_VALUES
  integer ,external  ::   hopen,hclose,sfginfo,sfstart, sfcreate, sfn2index,sfselect,sfendacc 
  integer ,external  ::   sfend,sfwdata,sfrdata,sfrcdata,sfwcdata,sfscatt,sfsnatt,sfsdtstr
  integer ,external  ::   sfsrange,sfscal
  integer ,external  ::   vfstart, vsfatch, vsfgid, vsfinq,vsfrd, vsfisat, vsfdtch, vfend, vsfsfld, vsffnd, vsfseek

  !-----------------------hdf function strings--------------------------------------------
  character(len=60)  ::     sds_name
  character(len=64)  ::     vdata_name
  character(len=20)  ::     FILE_ATTR_NAME,SDS_ATTR_NAME
  character(len=250) ::     FILE_VALUES,sds_values
  character(len=250) ::     label,units,formats,coord_sys

  !-----------------------parameters---------------------------------------------
  integer,parameter  ::     DFACC_CREATE = 4, DFACC_WRITE = 2, DFACC_READ = 1, DFNT_INT16 = 22
  integer,parameter  ::     DFNT_UCHAR8 = 3 , DFNT_CHAR8 = 4, FULL_INTERLACE = 0, FIELD_SIZE = 80 
  integer,parameter  ::     DFNT_FLOAT64 = 6, DFNT_FLOAT32 = 5, DFNT_INT32 = 24 

  !-------------------------------------------------------------------------------  
  integer              ::    node,iscan,nscan,nsmps
  integer,allocatable  ::    qc(:)
  real(8),allocatable  ::    obs_time(:,:)       !.dimension is 4 x scannum
  real,allocatable     ::    lat(:,:),lon(:,:)   !.dimension is 240 x scannum
  real,allocatable     ::    sataltang(:,:),satazmang(:,:),sunaltang(:,:),sunazmang(:,:)
  real,dimension(243)  ::    BT10v,BT10h,BT18v,BT18h,BT23v,BT23h,BT36v,BT36h,BT89v,BT89h
  real,allocatable     ::    BT(:,:,:)
  character(len=1)     ::    node_type
  character(len=120)   ::    buffer,formstr,processJdate
  character(len=250)   ::    path_tdr,ACCESSSTR,FORMSTRr, &
                             path_rdr,mwri_L1,mwri_tdr,rdrfile,tdrfile,tempchar

  !------------------------------------------------------------------------------
  integer(4)            ::    tim_off,timsecs_now,timsecs_1st,pass_secs
  integer(2)            ::    iyr,imon,iday,ihr,imin,isec
  integer(2)            ::    daycount
  real(4)               ::    daysects,daysec,obstim
  type timetype
    integer ::  year
    integer ::  Jday
    integer ::  secs
  end type

  !-------------------------------------------------------------------------------
  !...TDR Header data format definition
  type TDRHeader
        integer               ::   nscanL
        integer               ::   nFovs
        integer               ::   nqc
        integer               ::   nchan
        real,dimension(10)    ::   CFreq
        integer,dimension(10) ::   pol
  end type TDRHeader
  type(TDRHeader) ::  mwriTDRHdr
  !------------------------------------------------------------------------------

  type(timetype) :: scanT
  logical :: alive

  !-------------------------------------------------------------------------------
  ! to be consistent with MIRS systems
  !-------------------------------------------------------------------------------
  CHARACTER(LEN=250), DIMENSION(:), POINTER  :: rdrFilesFy3,tdrFilesFy3
  INTEGER :: ifile, nFiles, iu_list,numrsmps,numrscans
  !---Namelist data 
  CHARACTER(LEN=250) :: rdrfileList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250) :: pathTDR=DEFAULT_VALUE_STR4
  CHARACTER(LEN=10)  :: cnumrsmps,cnumrscans

  NAMELIST /ContrlRDR2TDR/rdrfileList,pathTDR
  !-------------------------------------------------------------------------------------
  !   Read control-data from namelist
  !-------------------------------------------------------------------------------------
  READ(*,NML=ContrlRDR2TDR)
  !write(*,NML=ContrlRDR2TDR)
  call ReadList2(iu_list,trim(rdrfileList),rdrFilesFy3,nFiles,tdrFilesFy3,pathTDR,'TDR_','.DAT',node_type)
  IF (nfiles .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'')
  !-----------------------------------------------
  !...setting the resampling parameters
  numrsmps  = 2
  numrscans = 2
  !...mwri instrument parameters
  mwriTDRHdr%nFovs     = floor(240.0/numrsmps)    !....resample every two pixels and every two lines
  mwriTDRHdr%nqc       = 1
  mwriTDRHdr%nchan     = 10
  mwriTDRHdr%CFreq(1)  = 10.65
  mwriTDRHdr%CFreq(2)  = 10.65
  mwriTDRHdr%CFreq(3)  = 18.7
  mwriTDRHdr%CFreq(4)  = 18.7
  mwriTDRHdr%CFreq(5)  = 23.8
  mwriTDRHdr%CFreq(6)  = 23.8
  mwriTDRHdr%CFreq(7)  = 36.5
  mwriTDRHdr%CFreq(8)  = 36.5
  mwriTDRHdr%CFreq(9)  = 89.0
  mwriTDRHdr%CFreq(10) = 89.0
  !...polarization information,v=4,h=5 ?
  mwriTDRHdr%pol(1)  = 4
  mwriTDRHdr%pol(2)  = 5
  mwriTDRHdr%pol(3)  = 4
  mwriTDRHdr%pol(4)  = 5
  mwriTDRHdr%pol(5)  = 4
  mwriTDRHdr%pol(6)  = 5
  mwriTDRHdr%pol(7)  = 4
  mwriTDRHdr%pol(8)  = 5
  mwriTDRHdr%pol(9)  = 4
  mwriTDRHdr%pol(10) = 5

  filesLoop: DO ifile=1,nFiles
    write(*,*)
    write(*,*)'Reading FY3_MWRI L1 file:'
    !---------------------------------------------------------------------------
    !...following is to open and read out the mwri L1 hdf data
    mwri_L1 = TRIM(rdrFilesFy3(ifile))
    write(*,'(A)')  TRIM(mwri_L1)

    inquire(file = TRIM(mwri_L1),exist=alive)
    if( .NOT. alive ) goto 100
    sd_id   = sfstart(TRIM(mwri_L1),DFACC_READ)
    if(sd_id<0)then
       write(*,*) 'Error open mwri_L1 HDF file !'
       stop
    endif

    !-------scan time sds-------------------------------------------------------
    sds_name = 'SCAN_TIME'
    sds_idx  = sfn2index(sd_id,sds_name)
    sds_id   = sfselect(sd_id,sds_idx)
    if(sds_id<0)then
       write(*,*) 'Error open mwri L1 sds:',sds_name
    end if
    status  = sfginfo(sds_id,sds_name,ds_ndims,ds_dims,ds_type,ds_atts)
    nscan   = ds_dims(2)
    nsmps   = ds_dims(1)
    if(numrscans>1)then
    if(mod(nscan,2)/=0)then !....nscan is odd number
       nscan = floor(nscan/2.0) + 1
    else
       nscan = nscan/2   !...nscan line is even number
    end if
    end if

    ds_start  = 0
    ds_stride(1) = 1
    ds_stride(2) = numrscans
    ds_edges(1)  = ds_dims(1)
    ds_edges(2)  = nscan
    allocate(obs_time(nsmps,nscan)) 
    status    = sfrdata(sds_id,ds_start,ds_stride,ds_edges,obs_time)
    status    = sfendacc(sds_id)
    !write(*,*) 'successfully reading sds--->',sds_name

    mwriTDRHdr%nscanL = nscan
    !write(*,*) 'The number of scans of this orbit is:',nscan
    !---------------------------------------------------------------------------
    !...now specify the n samples
    nsmps = mwriTDRHdr%nFovs
    !------Geolocation datasets-------------------------------------------------
    sds_name = 'Latitude'
    sds_idx  = sfn2index(sd_id,sds_name)
    sds_id   = sfselect(sd_id,sds_idx)
    if(sds_id<0)then
       write(*,*) 'Error open mwri L1 sds:',sds_name
    end if
    status = sfginfo(sds_id,sds_name,ds_ndims,ds_dims,ds_type,ds_atts)
    ds_start     = 0
    ds_stride(1) = numrsmps
    ds_stride(2) = numrscans 
    ds_edges(1)  = nsmps
    ds_edges(2)  = nscan

    allocate(Lat(nsmps,nscan)) 
    status    = sfrdata(sds_id,ds_start,ds_stride,ds_edges,Lat)
    status    = sfendacc(sds_id)
    !write(*,*) 'successfully reading sds--->',sds_name
    where( abs(lat - 999.9) .lt. 0.000001 ) lat = -999.0 !...for consistant with mirs
    sds_name = 'Longitude'
    sds_idx  = sfn2index(sd_id,sds_name)
    sds_id   = sfselect(sd_id,sds_idx)
    if(sds_id<0)then
       write(*,*) 'Error open mwri L1 sds:',sds_name
    end if
    status = sfginfo(sds_id,sds_name,ds_ndims,ds_dims,ds_type,ds_atts)
    ds_start  = 0
    ds_stride(1) = numrsmps
    ds_stride(2) = numrscans 
    ds_edges(1)  = nsmps
    ds_edges(2)  = nscan
    allocate(Lon(nsmps,nscan)) 
    status    = sfrdata(sds_id,ds_start,ds_stride,ds_edges,Lon)
    where( abs(lon-999.9) .lt. 0.000001 ) lon = -999.9
    status    = sfendacc(sds_id)
    !write(*,*) 'successfully reading sds--->',sds_name

    !---------------------------------------------------------------------------
    !...the angle information is now written in the hdf file,since the hdf file 
    !...is not the daily-runing results. it will be added in in future version
    allocate(sataltang(nsmps,nscan))
    allocate(satazmang(nsmps,nscan))
    allocate(sunaltang(nsmps,nscan))
    allocate(sunazmang(nsmps,nscan))
    allocate(qc(mwriTDRHdr%nqc))
    !...initialize the angle array
    sataltang = 52.5
    satazmang = 0
    sunaltang = 0
    sunazmang = 0
    !---------------------------------------------------------------------------
    sds_name = 'Earth_Observation_BT_10_89GHz(K)'
    sds_idx  = sfn2index(sd_id,sds_name)
    sds_id   = sfselect(sd_id,sds_idx)
    if(sds_id<0) then
      write(*,*) 'Error open mwri L1 sds:',sds_name
    end if
    status = sfginfo(sds_id,sds_name,ds_ndims,ds_dims_3d,ds_type,ds_atts)
    ds_start_3d    = 0
    ds_stride_3d(1)= numrsmps
    ds_stride_3d(2)= numrscans
    ds_stride_3d(3)= 1
    ds_edges_3d(1) = nsmps
    ds_edges_3d(2) = nscan
    ds_edges_3d(3) = ds_dims_3d(3)
    allocate(BT(nsmps,nscan,ds_dims_3d(3)))
    status         = sfrdata(sds_id,ds_start_3d,ds_stride_3d,ds_edges_3d,BT)
    status         = sfendacc(sds_id)
    !write(*,*) 'successfully reading sds--->',sds_name

    !---------------------------------------------------------------------------
    !...close the mwri L1 hdf file
    status         = sfend(sd_id)
    !---------------------------------------------------------------------------
    !...following is to open TDR file to store the rdr datase
    !iu_tdr = get_lun()

    mwri_tdr = TRIM(tdrFilesFy3(ifile))
    !open(iu_tdr,file = mwri_tdr,form='unformatted',status='unknown')
    !open(iu_tdr,file = mwri_tdr,form = 'binary', access = 'sequential',status = 'replace')

    write(*,*) '**********************************************'
    write(*,*) 'Writing TDR file:'
    write(*,'(A)') TRIM(mwri_tdr)

    !...following is to writting the rdr datasets into TDR file
    !----------------------------------------------------------
    !...writting the Header into file
    !write(iu_tdr) mwriTDRHdr%nscanL, 
    !write(iu_tdr) mwriTDRHdr%nFovs
    !write(iu_tdr) mwriTDRHdr%nchan
    !write(iu_tdr) mwriTDRHdr%nqc
    !write(iu_tdr) mwriTDRHdr%CFreq
    !write(iu_tdr) mwriTDRHdr%pol
    !-------------------------------------------------------------------------------------
    !the subroutine is under this directory: ~/mirs_working/src/lib/io/IO_MeasurData.f90
    call WriteRadHdrScanLMode(mwri_tdr,iu_tdr,mwriTDRHdr%nscanL,mwriTDRHdr%nFovs, &
         mwriTDRHdr%nqc, mwriTDRHdr%nchan,mwriTDRHdr%CFreq,mwriTDRHdr%pol)

    !---------------------------------------------------------
    ! node_type = rdrfile(10:10)
    !write(*,*)node_type
    if(node_type .eq. 'A')then
        node = 0
    else
        node = 1
    end if
    !write(*,*) node
   
    !--------------------------------------------------------------------------------
    !...now writting the data into tdr file line by line
    scanLoop: do iscan = 1, nscan
       ! BT10v(:) = BT(:,iscan,1)
       ! BT10h(:) = BT(:,iscan,2)
       ! BT18v(:) = BT(:,iscan,3)
       ! BT18h(:) = BT(:,iscan,4)
       ! BT23v(:) = BT(:,iscan,5)
       ! BT23h(:) = BT(:,iscan,6)
       ! BT36v(:) = BT(:,iscan,7)
       ! BT36h(:) = BT(:,iscan,8)
       ! BT89v(:) = BT(:,iscan,9)
       ! BT89h(:) = BT(:,iscan,10)
       !-----------------------------------------------------------------------
       !...following is to transfer the time into UTC
       !...year is the julian year
       !...day  is the julian Day
       !...sec is the accumulate seconds in the day,no more than 86400secs
       !-----------------------------------------------------------------------
       !...the mwri timing is begin from 2000,01,01,12,0,0
       !...first compute the time intercept from 2000,1,1,12. timing begin is 
       ! from 1970,1,1,0,0,0
       ! write(*,*) 'begin to time transfer'
       call packtimeqq(tim_off,2000,1,1,12,0,0)
       ! write(*,*) 'tim_off = ',tim_off
       !...compute the accumulate seconds since 1970,1,1,0,0,0
       timsecs_now = tim_off + obs_time(1,iscan)*86400 + obs_time(2,iscan)*0.001
       !...transfer from seconds to year,mon,day,hour,min,sec format
       call unpacktimeqq(timsecs_now,iyr,imon,iday,ihr,imin,isec)
       !write(*,*) 'Jdate = ',iyr,imon,iday,ihr,imin,isec
       scanT%year = iyr
       !write(*,*) scanT%year
       !...following is to compute the accumulate day number
       call packtimeqq(timsecs_1st,iyr,1,1,0,0,0)
       pass_secs = timsecs_now - timsecs_1st
       scanT%Jday = floor(pass_secs/86400.0)+1
       scanT%secs = mod(pass_secs,86400)
       !print *,scanT%secs/3600.
       !----------------------------------------------------------------------
       !---------------------------------------------------------------------
       !-mwri time is in jd2000 time format,transfer to the hour past midnight
       !-by following method
       !daycount = obs_time(1,iscan)
       !daysects = obs_time(2,iscan)  !-miliseconts from midnight
       !daysec = mod(daycount*86400.0 + daysects*0.001 + 43200.,86400.)
       !obstim = daysec/3600.
       !print *,scanT%secs/3600.,obstim
       !----------------------------------------------------------------------
       !***the results of the upper two time transfer method is same.
       !----------------------------------------------------------------------
    
       !...writting the Header into file
       !write(iu_tdr) mwriTDRHdr%nscanL,
       !write(iu_tdr) mwriTDRHdr%nFovs
       !write(iu_tdr) mwriTDRHdr%nchan
       !write(iu_tdr) mwriTDRHdr%nqc
       !write(iu_tdr) mwriTDRHdr%CFreq
       !write(iu_tdr) mwriTDRHdr%pol
       
       !-------------------------------------------------------------------------------------
       qc = 0
       call  WriteRadMeasScanLMode(iu_tdr,mwriTDRHdr%nqc,qc,mwriTDRHdr%nchan,mwriTDRHdr%nFovs,&
             sataltang(:,iscan),BT(:,iscan,:),lat(:,iscan),lon(:,iscan),node,&
             scanT%secs,scanT%Jday,scanT%year,satazmang(:,iscan),sunazmang(:,iscan))

       ! wirite
       ! write(iu_tdr) scanT%Jday
       ! write(iu_tdr) scanT%year
       ! write(iu_tdr) scanT%secs
       ! write(iu_tdr) lat(:,iscan)
       ! write(iu_tdr) lon(:,iscan)
       ! write(iu_tdr) sataltang(:,iscan)
       ! write(iu_tdr) satazmang(:,iscan)
       ! write(iu_tdr) sunazmang(:,iscan)
       ! write(iu_tdr) BT10v
       ! write(iu_tdr) BT10h
       ! write(iu_tdr) BT18v
       ! write(iu_tdr) BT18h
       ! write(iu_tdr) BT23v
       ! write(iu_tdr) BT23h
       ! write(iu_tdr) BT36v
       ! write(iu_tdr) BT36h
       ! write(iu_tdr) BT89v
       ! write(iu_tdr) BT89h
       ! write(iu_tdr) qc
       ! write(*,*)'processing scan number--->', iscan
    
    end do scanLoop
    close(iu_tdr)

    !---------------------------------------------------------------------------
    deallocate(obs_time)
    deallocate(lat)
    deallocate(lon)
    deallocate(sataltang)
    deallocate(satazmang)
    deallocate(sunaltang)
    deallocate(sunazmang)
    deallocate(BT)
    deallocate(qc)

100  continue

  END DO filesLoop

  DEALLOCATE(rdrFilesFy3,tdrFilesFy3)
 
 
end program rdr2tdr_fy3ri_mwri

