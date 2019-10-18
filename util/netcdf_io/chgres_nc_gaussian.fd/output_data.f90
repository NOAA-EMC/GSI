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
 type(Dataset) :: indset, outdset


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
 real, allocatable                            :: work2d(:,:)



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
! Write output grid data to a netcdf file.
!-------------------------------------------------------------------

 use input_data
 use setup
 use netcdf, only : nf90_max_name

 implicit none

 integer :: n,nrev
 real, allocatable, dimension (:,:) :: out2d
 real, allocatable, dimension (:,:,:) :: out3d
 character(len=nf90_max_name) varname


!-------------------------------------------------------------------
! Set up some header info.
!-------------------------------------------------------------------

 call header_set

!-------------------------------------------------------------------
! Open and write file.
!-------------------------------------------------------------------
! TODO: note there can be compression applied to this output file if necessary
!       see how it's done in the GSI EnKF for example


 print*
 print*,'OPEN OUTPUT FILE: ',trim(output_file)
 allocate(out2d(i_output,j_output))
 allocate(out3d(i_output,j_output,lev))

 print*,"WRITE SURFACE HEIGHT"
 out2d = reshape(hgt_external_output, (/i_output,j_output/))
 call write_vardata(outdset, 'hgtsfc', out2d)
 deallocate(hgt_external_output)

 print*,"WRITE SURFACE PRESSURE"
 out2d = reshape(sfcp_output, (/i_output,j_output/))
 call write_vardata(outdset, 'pressfc', out2d)
 deallocate(sfcp_output)

 print*,"WRITE TEMPERATURE"
 do n=1,lev
    nrev = lev+1-n
    out3d(:,:,n) = reshape(tmp_output(:,nrev), (/i_output,j_output/))
 end do
 varname = 'tmp'
 call quantize_and_write(outdset, varname, out3d)
 deallocate(tmp_output)

 print*,"WRITE CLOUD LIQUID WATER"
 do n=1,lev
    nrev = lev+1-n
    out3d(:,:,n) = reshape(clwmr_output(:,nrev), (/i_output,j_output/))
 end do
 varname = 'clwmr'
 call quantize_and_write(outdset, varname, out3d)
 deallocate(clwmr_output)

 print*,"WRITE SPECIFIC HUMIDITY"
 do n=1,lev
    nrev = lev+1-n
    out3d(:,:,n) = reshape(spfh_output(:,nrev), (/i_output,j_output/))
 end do
 varname = 'spfh'
 call quantize_and_write(outdset, varname, out3d)
 deallocate(spfh_output)

 print*,"WRITE OZONE"
 do n=1,lev
    nrev = lev+1-n
    out3d(:,:,n) = reshape(o3mr_output(:,nrev), (/i_output,j_output/))
 end do
 varname = 'o3mr'
 call quantize_and_write(outdset, varname, out3d)
 deallocate(o3mr_output)

 print*,"WRITE U-WINDS"
 do n=1,lev
    nrev = lev+1-n
    out3d(:,:,n) = reshape(ugrd_output(:,nrev), (/i_output,j_output/))
 end do
 varname = 'ugrd'
 call quantize_and_write(outdset, varname, out3d)
 deallocate(ugrd_output)

 print*,"WRITE V-WINDS"
 do n=1,lev
    nrev = lev+1-n
    out3d(:,:,n) = reshape(vgrd_output(:,nrev), (/i_output,j_output/))
 end do
 varname = 'vgrd'
 call quantize_and_write(outdset, varname, out3d)
 deallocate(vgrd_output)

 if (idzdt == 1) then
 print*,"WRITE DZDT"
 do n=1,lev
    nrev = lev+1-n
    out3d(:,:,n) = reshape(dzdt_output(:,nrev), (/i_output,j_output/))
 end do
 varname = 'dzdt'
 call quantize_and_write(outdset, varname, out3d)
 deallocate(dzdt_output)
 endif

 if (idpres == 1) then
 print*,"WRITE DPRES"
 do n=1,lev
    nrev = lev+1-n
    out3d(:,:,n) = reshape(dpres_output(:,nrev), (/i_output,j_output/))
 end do
 varname = 'dpres'
 call quantize_and_write(outdset, varname, out3d)
 endif
 deallocate(dpres_output)

 if (idelz == 1) then
 print*,"WRITE DELZ"
 do n=1,lev
    nrev = lev+1-n
    out3d(:,:,n) = reshape(delz_output(:,nrev), (/i_output,j_output/))
 end do
 varname = 'delz'
 call quantize_and_write(outdset, varname, out3d)
 endif
 deallocate(delz_output)

 if (irwmr == 1) then
 print*,"WRITE RAIN WATER"
 do n=1,lev
    nrev = lev+1-n
    out3d(:,:,n) = reshape(rwmr_output(:,nrev), (/i_output,j_output/))
 end do
 varname = 'rwmr'
 call quantize_and_write(outdset, varname, out3d)
 deallocate(rwmr_output)
 endif

 if (isnmr == 1) then
 print*,"WRITE SNOW WATER"
 do n=1,lev
    nrev = lev+1-n
    out3d(:,:,n) = reshape(snmr_output(:,nrev), (/i_output,j_output/))
 end do
 varname = 'snmr'
 call quantize_and_write(outdset, varname, out3d)
 deallocate(snmr_output)
 endif

 if (iicmr == 1) then
 print*,"WRITE ICE WATER"
 do n=1,lev
    nrev = lev+1-n
    out3d(:,:,n) = reshape(icmr_output(:,nrev), (/i_output,j_output/))
 end do
 varname = 'icmr'
 call quantize_and_write(outdset, varname, out3d)
 deallocate(icmr_output)
 endif

 if (igrle == 1) then
 print*,"WRITE GRAUPEL"
 do n=1,lev
    nrev = lev+1-n
    out3d(:,:,n) = reshape(grle_output(:,nrev), (/i_output,j_output/))
 end do
 varname = 'grle'
 call quantize_and_write(outdset, varname, out3d)
 deallocate(grle_output)
 endif

 if (icldamt == 1) then
 print*,"WRITE CLD_AMT"
 do n = 1, lev
    nrev = lev+1-n
    out3d(:,:,n) = reshape(cldamt_output(:,nrev), (/i_output,j_output/))
 end do
 varname = 'cld_amt'
 call quantize_and_write(outdset, varname, out3d)
 deallocate(cldamt_output)
 endif


 deallocate(out2d,out3d)

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

 print*
 print*,"SET HEADER INFO FOR OUTPUT FILE."

 indset = open_dataset(terrain_file)
 outdset = create_dataset(output_file, indset)

 end subroutine header_set

 subroutine quantize_and_write(outdset, varname, out3d)
 type(Dataset), intent(inout) :: outdset
 real, intent(inout), dimension (:,:,:) :: out3d
 character(len=*), intent(in) :: varname
 real, allocatable, dimension(:,:,:) :: out3d_save
 integer nbits
 real compress_err
 allocate(out3d_save, mold=out3d)
 if (has_attr(outdset, 'nbits', trim(varname))) then
   call read_attribute(outdset, 'nbits', nbits, trim(varname))
   out3d_save = out3d
   call quantize_data(out3d_save, out3d, nbits, compress_err)
   call write_attribute(outdset,'max_abs_compression_error',compress_err,trim(varname))
 endif
 call write_vardata(outdset, trim(varname), out3d)
 deallocate(out3d_save)
 end subroutine quantize_and_write

 end module output_data
