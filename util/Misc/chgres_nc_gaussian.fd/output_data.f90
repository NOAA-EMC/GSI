 module output_data

 use module_fv3gfs_ncio

 implicit none

 private

 integer, public                   :: kgds_output(200)

! data on the output grid.
 real, allocatable, public         :: hgt_output(:)  ! interpolated from input grid
 real, allocatable, public         :: hgt_external_output(:)
 real, allocatable, public         :: sfcp_output(:)
 real, allocatable, public         :: tmp_output(:,:)
 real, allocatable, public         :: clwmr_output(:,:)
 real, allocatable, public         :: delz_output(:,:)
 real, allocatable, public         :: dpres_output(:,:)
 real, allocatable, public         :: dzdt_output(:,:)
 real, allocatable, public         :: o3mr_output(:,:)
 real, allocatable, public         :: spfh_output(:,:)
 real, allocatable, public         :: ugrd_output(:,:)
 real, allocatable, public         :: vgrd_output(:,:)
 real, allocatable, public         :: rwmr_output(:,:)
 real, allocatable, public         :: icmr_output(:,:)
 real, allocatable, public         :: snmr_output(:,:)
 real, allocatable, public         :: grle_output(:,:)
 real, allocatable, public         :: cldamt_output(:,:)
 real, allocatable, public         :: rlat_output(:)
 real, allocatable, public         :: rlon_output(:)

 public                            :: set_output_grid
 public                            :: write_output_data


 contains

 subroutine set_output_grid

!-------------------------------------------------------------------
! Set grid specs on the output grid.
!-------------------------------------------------------------------

 use setup
 use input_data
 use utils

 implicit none


 type(Dataset) :: indset
 type(Dimension) :: ncdim
 real, allocatable                            :: work2d(:,:),work3d(:,:,:)



 print*
 print*,"OUTPUT GRID I/J DIMENSIONS: ", i_output, j_output

!-------------------------------------------------------------------
! Set the grib 1 grid description section, which is needed
! by the IPOLATES library.
!-------------------------------------------------------------------

 kgds_output = 0

 call calc_kgds(i_output, j_output, kgds_output)

!-------------------------------------------------------------------
! Read the terrain on the output grid.  To ensure exact match,
! read it from an existing netcdf file.
!-------------------------------------------------------------------

 print*
 print*,"OPEN OUTPUT GRID TERRAIN FILE: ", trim(terrain_file)
 indset = open_dataset(terrain_file)

 allocate(hgt_external_output(ij_output))

 print*
 print*,"READ SURFACE HEIGHT"
 call read_vardata(indset, 'hgtsfc', work2d)

 hgt_external_output = reshape(work2d,(/ij_output/)) 

 call close_dataset(indset)

 end subroutine set_output_grid

 subroutine write_output_data

!-------------------------------------------------------------------
! Write output grid data to a nemsio file.
!-------------------------------------------------------------------

 use input_data
 use setup

 implicit none

 integer :: n


!-------------------------------------------------------------------
! Set up some header info.
!-------------------------------------------------------------------

 call header_set

!-------------------------------------------------------------------
! Open and write file.
!-------------------------------------------------------------------



 print*
 print*,'OPEN OUTPUT FILE: ',trim(output_file)
 !call nemsio_open(gfile, output_file, gaction, iret=iret, gdatatype="bin4", &
 !                 nmeta=8, modelname="FV3GFS", nrec=nrec, &
 !                 idate=idate, dimx=i_output, &
 !                 dimy=j_output, dimz=lev, ntrac=ntrac, & 
 !                 ncldt=ncldt, idvc=idvc, idsl=idsl, idvm=idvm, &
 !                 idrt=4, recname=recname, reclevtyp=reclevtyp, &
 !                 reclev=reclev,vcoord=vcoord_header, &
 !                 lat=lat, lon=lon)

 !deallocate(lon, lat, recname, reclevtyp, reclev, vcoord_header)

 print*,"WRITE SURFACE HEIGHT"
 deallocate(hgt_external_output)

 print*,"WRITE SURFACE PRESSURE"
 !dummy = sfcp_output
 deallocate(sfcp_output)

 print*,"WRITE TEMPERATURE"
 do n = 1, lev
   !dummy = tmp_output(:,n)
 enddo
 deallocate(tmp_output)

 print*,"WRITE CLOUD LIQUID WATER"
 do n = 1, lev
   !dummy = clwmr_output(:,n)
 enddo
 deallocate(clwmr_output)

 print*,"WRITE SPECIFIC HUMIDITY"
 do n = 1, lev
   !dummy = spfh_output(:,n)
 enddo
 deallocate(spfh_output)

 print*,"WRITE OZONE"
 do n = 1, lev
   !dummy = o3mr_output(:,n)
 enddo
 deallocate(o3mr_output)

 print*,"WRITE U-WINDS"
 do n = 1, lev
   !dummy = ugrd_output(:,n)
 enddo
 deallocate(ugrd_output)

 print*,"WRITE V-WINDS"
 do n = 1, lev
   !dummy = vgrd_output(:,n)
 enddo
 deallocate(vgrd_output)

 print*,"WRITE DZDT"
 do n = 1, lev
   !dummy = dzdt_output(:,n)
 enddo
 deallocate(dzdt_output)

 print*,"WRITE DPRES"
 do n = 1, lev
   !dummy = dpres_output(:,n)
 enddo
 deallocate(dpres_output)

 print*,"WRITE DELZ"
 do n = 1, lev
   !dummy = delz_output(:,n)
 enddo
 deallocate(delz_output)


   print*,"WRITE RAIN WATER"
   do n = 1, lev
     !dummy = rwmr_output(:,n)
   enddo
   deallocate(rwmr_output)

   print*,"WRITE SNOW WATER"
   do n = 1, lev
     !dummy = snmr_output(:,n)
   enddo
   deallocate(snmr_output)

   print*,"WRITE ICE WATER"
   do n = 1, lev
     !dummy = icmr_output(:,n)
   enddo
   deallocate(icmr_output)

   print*,"WRITE GRAUPEL"
   do n = 1, lev
     !dummy = grle_output(:,n)
   enddo
   deallocate(grle_output)

   if (icldamt == 1) then
      print*,"WRITE CLD_AMT"
      do n = 1, lev
         !dummy = cldamt_output(:,n)
      enddo
      deallocate(cldamt_output)
   endif
   





 return

 end subroutine write_output_data

 subroutine header_set

!-------------------------------------------------------------------
! copy dimensions and metadata to the output file from the
! input terrain (output res) file
!-------------------------------------------------------------------

 use input_data
 use setup

 implicit none

 type(Dataset) :: indset, outdset

 print*
 print*,"SET HEADER INFO FOR OUTPUT FILE."

 indset = open_dataset(terrain_file)
 outdset = create_dataset(output_file, indset)

 ! need to write out time, pfull,phalf,lat/lon,etc
 
 end subroutine header_set

 end module output_data
