 module input_data

 use utils
 use setup
 use module_fv3gfs_ncio

 implicit none

 private

 integer, public                              :: idvc, idsl, idvm, nvcoord
 integer, public                              :: ntrac, ncldt,icldamt
 integer, public                              :: ij_input, kgds_input(200)
 integer, public                              :: i_input, j_input, lev
 integer, public                              :: idate(6)


 real, allocatable, public                    :: vcoord(:,:)
 real, allocatable, public                    :: clwmr_input(:,:)
 real, allocatable, public                    :: dzdt_input(:,:)
 real, allocatable, public                    :: grle_input(:,:)
 real, allocatable, public                    :: cldamt_input(:,:) 
 real, allocatable, public                    :: hgt_input(:)
 real, allocatable, public                    :: icmr_input(:,:)
 real, allocatable, public                    :: o3mr_input(:,:)
 real, allocatable, public                    :: rwmr_input(:,:)
 real, allocatable, public                    :: sfcp_input(:)
 real, allocatable, public                    :: snmr_input(:,:)
 real, allocatable, public                    :: spfh_input(:,:)
 real, allocatable, public                    :: tmp_input(:,:)
 real, allocatable, public                    :: ugrd_input(:,:)
 real, allocatable, public                    :: vgrd_input(:,:)

 public                                       :: read_input_data
 public                                       :: read_vcoord_info

 contains

 subroutine read_input_data

!-------------------------------------------------------------------------------------
! Read input grid data from a netcdf file.
!-------------------------------------------------------------------------------------

 implicit none

 integer :: vlev
 type(Dataset) :: indset
 type(Dimension) :: ncdim
 real, allocatable                            :: work2d(:,:),work3d(:,:,:)

 print*
 print*,"OPEN INPUT FILE: ",trim(input_file)
 indset = open_dataset(input_file)

 print*,"GET INPUT FILE HEADER"
 ncdim = get_dim(indset, 'grid_xt'); i_input = ncdim%len
 ncdim = get_dim(indset, 'grid_yt'); j_input = ncdim%len
 ncdim = get_dim(indset, 'pfull'); lev = ncdim%len
 idate = get_idate_from_time_units(indset)

 print*,'DIMENSIONS OF DATA ARE: ', i_input, j_input, lev
 print*,'DATE OF DATA IS:        ', idate

 ij_input = i_input * j_input


 print*
 print*,"READ SURFACE PRESSURE"
 call read_vardata(indset, 'pressfc', work2d)

 allocate(sfcp_input(ij_input))
 sfcp_input = reshape(work2d,(/ij_input/))
 print*,'MAX/MIN SURFACE PRESSURE: ',maxval(sfcp_input), minval(sfcp_input)

 print*
 print*,"READ SURFACE HEIGHT"
 call read_vardata(indset, 'hgtsfc', work2d)

 allocate(hgt_input(ij_input))
 hgt_input = reshape(work2d,(/ij_input/))
 print*,'MAX/MIN SURFACE HEIGHT: ',maxval(hgt_input), minval(hgt_input)

 print*
 print*,"READ U WIND"
 allocate(ugrd_input(ij_input,lev))
 call read_vardata(indset, 'ugrd', work3d)
 do vlev = 1, lev
   ugrd_input(:,vlev) = reshape(work3d(:,:,vlev),(/ij_input/)) 
   print*,'MAX/MIN U WIND AT LEVEL ',vlev, "IS: ", maxval(ugrd_input(:,vlev)), minval(ugrd_input(:,vlev))
 enddo

 print*
 print*,"READ V WIND"
 allocate(vgrd_input(ij_input,lev))
 call read_vardata(indset, 'vgrd', work3d)
 do vlev = 1, lev
   vgrd_input(:,vlev) = reshape(work3d(:,:,vlev),(/ij_input/)) 
   print*,'MAX/MIN V WIND AT LEVEL ', vlev, "IS: ", maxval(vgrd_input(:,vlev)), minval(vgrd_input(:,vlev))
 enddo

 print*
 print*,"READ TEMPERATURE"
 allocate(tmp_input(ij_input,lev))
 call read_vardata(indset, 'tmp', work3d)
 do vlev = 1, lev
   tmp_input(:,vlev) = reshape(work3d(:,:,vlev),(/ij_input/)) 
   print*,'MAX/MIN TEMPERATURE AT LEVEL ', vlev, 'IS: ', maxval(tmp_input(:,vlev)), minval(tmp_input(:,vlev))
 enddo

 print*
 print*,"READ SPECIFIC HUMIDITY"
 allocate(spfh_input(ij_input,lev))
 call read_vardata(indset, 'spfh', work3d)
 do vlev = 1, lev
   spfh_input(:,vlev) = reshape(work3d(:,:,vlev),(/ij_input/)) 
   print*,'MAX/MIN SPECIFIC HUMIDITY AT LEVEL ', vlev, 'IS: ', maxval(spfh_input(:,vlev)), minval(spfh_input(:,vlev))
 enddo

 print*
 print*,"READ CLOUD LIQUID WATER"
 allocate(clwmr_input(ij_input,lev))
 call read_vardata(indset, 'clwmr', work3d)
 do vlev = 1, lev
   clwmr_input(:,vlev) = reshape(work3d(:,:,vlev),(/ij_input/)) 
   print*,'MAX/MIN CLOUD LIQUID WATER AT LEVEL ', vlev, 'IS: ', maxval(clwmr_input(:,vlev)), minval(clwmr_input(:,vlev))
 enddo

 print*
 print*,"READ OZONE"
 allocate(o3mr_input(ij_input,lev))
 call read_vardata(indset, 'o3mr', work3d)
 do vlev = 1, lev
   o3mr_input(:,vlev) = reshape(work3d(:,:,vlev),(/ij_input/))
   print*,'MAX/MIN OZONE AT LEVEL ', vlev, 'IS: ', maxval(o3mr_input(:,vlev)), minval(o3mr_input(:,vlev))
 enddo

 print*
 print*,"READ DZDT"
 allocate(dzdt_input(ij_input,lev))
 call read_vardata(indset, 'dzdt', work3d)
 do vlev = 1, lev
   dzdt_input(:,vlev) = reshape(work3d(:,:,vlev),(/ij_input/))
   print*,'MAX/MIN DZDT AT LEVEL ', vlev, 'IS: ', maxval(dzdt_input(:,vlev)), minval(dzdt_input(:,vlev))
 enddo


 print*
 print*,"READ RWMR"
 allocate(rwmr_input(ij_input,lev))
 call read_vardata(indset, 'rwmr', work3d)
 do vlev = 1, lev
   rwmr_input(:,vlev) = reshape(work3d(:,:,vlev),(/ij_input/))
   print*,'MAX/MIN RWMR AT LEVEL ', vlev, 'IS: ', maxval(rwmr_input(:,vlev)), minval(rwmr_input(:,vlev))
 enddo

 print*
 print*,"READ ICMR"
 allocate(icmr_input(ij_input,lev))
 call read_vardata(indset, 'icmr', work3d)
 do vlev = 1, lev
   icmr_input(:,vlev) = reshape(work3d(:,:,vlev),(/ij_input/)) 
   print*,'MAX/MIN ICMR AT LEVEL ', vlev, 'IS: ', maxval(icmr_input(:,vlev)), minval(icmr_input(:,vlev))
 enddo

 print*
 print*,"READ SNMR"
 allocate(snmr_input(ij_input,lev))
 call read_vardata(indset, 'snmr', work3d)
 do vlev = 1, lev
   snmr_input(:,vlev) = reshape(work3d(:,:,vlev),(/ij_input/)) 
   print*,'MAX/MIN SNMR AT LEVEL ', vlev, 'IS: ', maxval(snmr_input(:,vlev)), minval(snmr_input(:,vlev))
 enddo

 print*
 print*,"READ GRLE"
 allocate(grle_input(ij_input,lev))
 call read_vardata(indset, 'grle', work3d)
 do vlev = 1, lev
   grle_input(:,vlev) = reshape(work3d(:,:,vlev),(/ij_input/)) 
   print*,'MAX/MIN GRLE AT LEVEL ', vlev, 'IS: ', maxval(grle_input(:,vlev)), minval(grle_input(:,vlev))
 enddo

 print*,"CLOSE FILE"
 call close_dataset(indset)

!---------------------------------------------------------------------------------------
! Set the grib 1 grid description array need by the NCEP IPOLATES library.
!---------------------------------------------------------------------------------------

 call calc_kgds(i_input, j_input, kgds_input)

 return

 end subroutine read_input_data

 subroutine read_vcoord_info

!---------------------------------------------------------------------------------
! Read vertical coordinate information.
!---------------------------------------------------------------------------------

 implicit none

 integer                    :: istat, levs_vcoord, n, k

 print*
 print*,"OPEN VERTICAL COORD FILE: ", trim(vcoord_file)
 open(14, file=trim(vcoord_file), form='formatted', iostat=istat)
 if (istat /= 0) then
   print*,"FATAL ERROR OPENING FILE. ISTAT IS: ", istat
   call errexit(4)
 endif

 read(14, *, iostat=istat) nvcoord, levs_vcoord
 if (istat /= 0) then
   print*,"FATAL ERROR READING FILE HEADER. ISTAT IS: ",istat
   call errexit(5)
 endif

!---------------------------------------------------------------------------------
! The last value in the file is not used for the fv3 core.  Only read the first 
! (lev + 1) values.
!---------------------------------------------------------------------------------

 allocate(vcoord(lev+1, nvcoord))
 read(14, *, iostat=istat) ((vcoord(n,k), k=1,nvcoord), n=1,lev+1)
 if (istat /= 0) then
   print*,"FATAL ERROR READING FILE. ISTAT IS: ",istat
   call errexit(6)
 endif

 print*
 do k = 1, (lev+1)
   print*,'VCOORD FOR LEV ', k, 'IS: ', vcoord(k,:)
 enddo

 close(14)

 end subroutine read_vcoord_info

 end module input_data
