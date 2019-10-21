module write_incr
!$$$ module documentation block
!           .      .    .                                       .
! module:   write_incr 
!   prgmmr: Martin       org:                 date: 2019-09-04
!
! abstract: This module contains routines which write out  
!           the atmospheric increment rather than analysis
!
! program history log:
!   2019-09-04 Martin    Initial version.  Based on ncepnems_io
!   2019-09-13  martin  added option to zero out certain increment fields 
!
! Subroutines Included:
!   sub write_fv3_increment - writes netCDF increment for FV3 global model
!
!$$$ end documentation block

  implicit none
  private
  public write_fv3_increment

  interface write_fv3_increment
     module procedure write_fv3_inc_
  end interface

contains

  subroutine write_fv3_inc_ (grd,sp_a,filename,mype_out,gfs_bundle,ibin)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    write_fv3_increment
!
!   prgmmr: Martin            org:                 date: 2019-09-04
!
! abstract: This routine takes GSI analysis increments and writes
!           to a netCDF file on the analysis resolution for use by FV3-GFS
!
! program history log:
!   2019-09-04  martin  Initial version. Based on write_atm_nemsio 
!   2019-09-13  martin  added option to zero out certain increment fields 
!
!   input argument list:
!     filename  - file to open and write to
!     mype_out  - mpi task to write output file
!    gfs_bundle - bundle containing fields on subdomains
!     ibin      - time bin
!
!   output argument list:
!
! attributes:
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!$$$ end documentation block

! !USES:
    use netcdf
    use kinds, only: r_kind,i_kind

    use mpimod, only: mpi_rtype
    use mpimod, only: mpi_comm_world
    use mpimod, only: ierror
    use mpimod, only: mype

    use gridmod, only: strip, rlats, rlons, bk5

    use general_commvars_mod, only: load_grid
    use general_specmod, only: spec_vars
    use general_sub2grid_mod, only: sub2grid_info

    use gsi_bundlemod, only: gsi_bundle, gsi_bundlegetpointer
    use control_vectors, only: control_vector, incvars_to_zero

    use constants, only: one, fv, rad2deg, r1000

    use gsi_4dcouplermod, only : gsi_4dcoupler_grtests
    use gsi_4dvar, only: nobs_bins, l4dvar, nsubwin, lwrite4danl, l4densvar
    use hybrid_ensemble_parameters, only: l_hyb_ens, ntlevs_ens
    use bias_predictors, only: predictors, allocate_preds, deallocate_preds
    use jfunc, only: xhatsave, iter

    use guess_grids, only: load_geop_hgt, geop_hgti, ges_geopi, ges_tsen, ges_tsen1, ges_q1
    use state_vectors, only: allocate_state, deallocate_state

    implicit none

! !INPUT PARAMETERS:

    type(sub2grid_info), intent(in) :: grd
    type(spec_vars),     intent(in) :: sp_a
    character(len=24),   intent(in) :: filename  ! file to open and write to
    integer(i_kind),     intent(in) :: mype_out  ! mpi task to write output file
    type(gsi_bundle),    intent(in) :: gfs_bundle
    integer(i_kind),     intent(in) :: ibin      ! time bin

!-------------------------------------------------------------------------

    character(len=120) :: my_name = 'WRITE_FV3INCR'

    real(r_kind),pointer,dimension(:,:,:) :: sub_u,sub_v
    real(r_kind),pointer,dimension(:,:,:) :: sub_qanl,sub_oz
    real(r_kind),pointer,dimension(:,:,:) :: sub_ql, sub_qi
    real(r_kind),pointer,dimension(:,:) :: sub_ps

    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig) :: sub_dzb,sub_dza, sub_tsen, sub_q

    real(r_kind),dimension(grd%lat1*grd%lon1)     :: pssm
    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig):: sub_dp
    real(r_kind),dimension(grd%lat1*grd%lon1,grd%nsig):: tsensm,tvsm, prslm, usm, vsm
    real(r_kind),dimension(grd%lat1*grd%lon1,grd%nsig):: dpsm, qsm, ozsm
    real(r_kind),dimension(grd%lat1*grd%lon1,grd%nsig):: qism, qlsm 
    real(r_kind),dimension(grd%lat1*grd%lon1,grd%nsig):: dzsm
    real(r_kind),dimension(max(grd%iglobal,grd%itotsub)) :: work1,work2
    real(r_kind),dimension(grd%nlon,grd%nlat-2):: grid, gridrev
    real(r_kind),dimension(grd%nlon,grd%nlat-2,grd%nsig):: delp
    real(r_kind),dimension(grd%nlon) :: deglons
    real(r_kind),dimension(grd%nlat-2) :: deglats
    real(r_kind),dimension(grd%nsig) :: levsout
    real(r_kind),dimension(grd%nsig+1) :: ilevsout

    integer(i_kind) :: mm1, i, j, k, iii
    integer(i_kind) :: iret, istatus 
    integer(i_kind) :: ibin2
    integer(i_kind) :: ncid_out, lon_dimid, lat_dimid, lev_dimid, ilev_dimid
    integer(i_kind) :: lonvarid, latvarid, levvarid, pfullvarid, ilevvarid, &
                       hyaivarid, hybivarid, uvarid, vvarid, delpvarid, delzvarid, &
                       tvarid, sphumvarid, liqwatvarid, o3varid, icvarid
    integer(i_kind) :: dimids3(3),nccount(3),ncstart(3)

    type(gsi_bundle) :: svalinc(nobs_bins)
    type(gsi_bundle) :: evalinc(ntlevs_ens)
    type(gsi_bundle) :: mvalinc(nsubwin)
    type(predictors) :: sbiasinc
    logical llprt

!*************************************************************************
!   Initialize local variables
    mm1=mype+1
    llprt=(mype==0).and.(iter<=1)
    ibin2 = ibin
    if (.not. l4densvar) ibin2 = 1

!   set up state space based off of xhatsave
!   Convert from control space directly to physical
!   space for comparison with obs.
    call allocate_preds(sbiasinc)
    do iii=1,nobs_bins
       call allocate_state(svalinc(iii))
    end do
    do iii=1,nsubwin
       call allocate_state(mvalinc(iii))
    end do
    do iii=1,ntlevs_ens
       call allocate_state(evalinc(iii))
    end do
    call control2state(xhatsave,mvalinc,sbiasinc)

    if (l4dvar) then
       if (l_hyb_ens) then
          call ensctl2state(xhatsave,mvalinc(1),evalinc)
          mvalinc(1)=evalinc(1)
       end if

!      Perform test of AGCM TLM and ADM
       call gsi_4dcoupler_grtests(mvalinc,svalinc,nsubwin,nobs_bins)

!      Run TL model to fill sval
       call model_tl(mvalinc,svalinc,llprt)
    else
       if (l_hyb_ens) then
          call ensctl2state(xhatsave,mvalinc(1),evalinc)
          do iii=1,nobs_bins
             svalinc(iii)=evalinc(iii)
          end do
       else
          do iii=1,nobs_bins
             svalinc(iii)=mvalinc(1)
          end do
       end if
    end if

    istatus=0
    call gsi_bundlegetpointer(gfs_bundle,'q', sub_qanl, iret); istatus=istatus+iret
    call gsi_bundlegetpointer(svalinc(ibin2),'ql', sub_ql, iret); istatus=istatus+iret
    call gsi_bundlegetpointer(svalinc(ibin2),'qi', sub_qi, iret); istatus=istatus+iret
    call gsi_bundlegetpointer(svalinc(ibin2),'oz', sub_oz, iret); istatus=istatus+iret
    call gsi_bundlegetpointer(svalinc(ibin2),'u', sub_u, iret); istatus=istatus+iret
    call gsi_bundlegetpointer(svalinc(ibin2),'v', sub_v, iret); istatus=istatus+iret
    call gsi_bundlegetpointer(svalinc(ibin2),'ps', sub_ps, iret); istatus=istatus+iret ! needed for delp
    if ( istatus /= 0 ) then
       if ( mype == 0 ) then
         write(6,*) 'write_fv3_incr_: ERROR'
         write(6,*) 'Missing some of the required fields'
         write(6,*) 'Aborting ... '
      endif
      call stop2(999)
    end if
    
    ! Single task writes increment to file
    if ( mype == mype_out ) then
      ! create the output netCDF file
      call nccheck_incr(nf90_create(path=trim(filename)//".nc", cmode=ior(nf90_clobber,nf90_64bit_offset), ncid=ncid_out))
      ! create dimensions based on analysis resolution, not guess
      call nccheck_incr(nf90_def_dim(ncid_out, "lon", grd%nlon, lon_dimid))
      call nccheck_incr(nf90_def_dim(ncid_out, "lat", grd%nlat-2, lat_dimid))
      call nccheck_incr(nf90_def_dim(ncid_out, "lev", grd%nsig, lev_dimid))
      call nccheck_incr(nf90_def_dim(ncid_out, "ilev", grd%nsig+1, ilev_dimid))
      dimids3 = (/ lon_dimid, lat_dimid, lev_dimid /)
      ! create variables
      call nccheck_incr(nf90_def_var(ncid_out, "lon", nf90_real, (/lon_dimid/), lonvarid))
      call nccheck_incr(nf90_def_var(ncid_out, "lat", nf90_real, (/lat_dimid/), latvarid))
      call nccheck_incr(nf90_def_var(ncid_out, "lev", nf90_real, (/lev_dimid/), levvarid))
      call nccheck_incr(nf90_def_var(ncid_out, "pfull", nf90_real, (/lev_dimid/), pfullvarid))
      call nccheck_incr(nf90_def_var(ncid_out, "ilev", nf90_real, (/ilev_dimid/), ilevvarid))
      call nccheck_incr(nf90_def_var(ncid_out, "hyai", nf90_real, (/ilev_dimid/), hyaivarid))
      call nccheck_incr(nf90_def_var(ncid_out, "hybi", nf90_real, (/ilev_dimid/), hybivarid))
      call nccheck_incr(nf90_def_var(ncid_out, "u_inc", nf90_real, dimids3, uvarid)) 
      call nccheck_incr(nf90_def_var(ncid_out, "v_inc", nf90_real, dimids3, vvarid)) 
      call nccheck_incr(nf90_def_var(ncid_out, "delp_inc", nf90_real, dimids3, delpvarid)) 
      call nccheck_incr(nf90_def_var(ncid_out, "delz_inc", nf90_real, dimids3, delzvarid)) 
      call nccheck_incr(nf90_def_var(ncid_out, "T_inc", nf90_real, dimids3, tvarid)) 
      call nccheck_incr(nf90_def_var(ncid_out, "sphum_inc", nf90_real, dimids3, sphumvarid)) 
      call nccheck_incr(nf90_def_var(ncid_out, "liq_wat_inc", nf90_real, dimids3, liqwatvarid)) 
      call nccheck_incr(nf90_def_var(ncid_out, "o3mr_inc", nf90_real, dimids3, o3varid)) 
      call nccheck_incr(nf90_def_var(ncid_out, "icmr_inc", nf90_real, dimids3, icvarid)) 
      ! place global attributes to parallel calc_increment output
      call nccheck_incr(nf90_put_att(ncid_out, nf90_global, "source", "GSI"))
      call nccheck_incr(nf90_put_att(ncid_out, nf90_global, "comment", &
                                      "global analysis increment from write_fv3_increment"))
      ! add units to lat/lon because that's what the calc_increment utility has
      call nccheck_incr(nf90_put_att(ncid_out, lonvarid, "units", "degrees_east"))
      call nccheck_incr(nf90_put_att(ncid_out, latvarid, "units", "degrees_north"))
      ! end the netCDF file definition
      call nccheck_incr(nf90_enddef(ncid_out))
    end if

    ! compute delz
    do k=1,grd%nsig
       sub_dzb(:,:,k) = ges_geopi(:,:,k+1,ibin) - ges_geopi(:,:,k,ibin)
    enddo

    call load_geop_hgt
    do k=1,grd%nsig
       sub_dza(:,:,k) = geop_hgti(:,:,k+1,ibin) - geop_hgti(:,:,k,ibin)
    enddo

    sub_dza = sub_dza - sub_dzb !sub_dza is increment

    ! compute sensible T increment
    sub_tsen = ges_tsen(:,:,:,ibin) - ges_tsen1(:,:,:,ibin)

    ! compute q increment
    sub_q = sub_qanl(:,:,:) - ges_q1(:,:,:,ibin)

    ! Strip off boundary points from subdomains
    call strip(sub_tsen  ,tsensm  ,grd%nsig)
    call strip(sub_q   ,qsm   ,grd%nsig)
    call strip(sub_ql  ,qlsm  ,grd%nsig)
    call strip(sub_qi  ,qism  ,grd%nsig)
    call strip(sub_oz  ,ozsm  ,grd%nsig)
    call strip(sub_ps  ,pssm  )
    call strip(sub_u   ,usm   ,grd%nsig)
    call strip(sub_v   ,vsm   ,grd%nsig)
    call strip(sub_dza, dzsm, grd%nsig)

    nccount = (/ grd%nlon, grd%nlat-2, 1 /)
    if (mype == mype_out) then
       ! latitudes
       do j=2,grd%nlat-1
          deglats(j) = rlats(j)*rad2deg
       end do
       ! write to file
       call nccheck_incr(nf90_put_var(ncid_out, latvarid, sngl(deglats), &
                         start = (/1/), count = (/grd%nlat-2/)))
       ! longitudes
       do i=1,grd%nlon
          deglons(i) = rlons(i)*rad2deg
       end do
       ! write to file
       call nccheck_incr(nf90_put_var(ncid_out, lonvarid, sngl(deglons), &
                         start = (/1/), count = (/grd%nlon/)))
       ! levels
       do k=1,grd%nsig
         levsout(k) = float(k)
         ilevsout(k) = float(k)
       end do
       ilevsout(grd%nsig+1) = float(grd%nsig+1)
       ! write to file
       call nccheck_incr(nf90_put_var(ncid_out, levvarid, sngl(levsout), &
                         start = (/1/), count = (/grd%nsig/)))
       ! pfull
       call nccheck_incr(nf90_put_var(ncid_out, pfullvarid, sngl(levsout), &
                         start = (/1/), count = (/grd%nsig/)))
       ! ilev
       call nccheck_incr(nf90_put_var(ncid_out, ilevvarid, sngl(ilevsout), &
                         start = (/1/), count = (/grd%nsig+1/)))
       ! hyai
       call nccheck_incr(nf90_put_var(ncid_out, hyaivarid, sngl(ilevsout), &
                         start = (/1/), count = (/grd%nsig+1/)))
       ! hybi
       call nccheck_incr(nf90_put_var(ncid_out, hybivarid, sngl(ilevsout), &
                         start = (/1/), count = (/grd%nsig+1/)))
    end if
    ! u increment
    ncstart = (/ 1, 1, grd%nsig /)
    do k=1,grd%nsig
       call mpi_gatherv(usm(1,k),grd%ijn(mm1),mpi_rtype,&
            work1,grd%ijn,grd%displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype == mype_out) then
          call load_grid(work1,gridrev)
          ! GSI is N->S, we want S->N 
          do j=1,grd%nlat-2
             grid(:,j) = gridrev(:,grd%nlat-1-j)
          end do
          if (should_zero_increments_for('u_inc')) grid = 0.0_r_kind
          ! write to file
          call nccheck_incr(nf90_put_var(ncid_out, uvarid, sngl(grid), &
                            start = ncstart, count = nccount))
       endif
       ncstart(3) = grd%nsig-k ! GSI is sfc->top, FV3 is top->sfc
    end do
    ! v increment
    ncstart = (/ 1, 1, grd%nsig /)
    do k=1,grd%nsig
       call mpi_gatherv(vsm(1,k),grd%ijn(mm1),mpi_rtype,&
            work1,grd%ijn,grd%displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype == mype_out) then
          call load_grid(work1,gridrev)
          ! GSI is N->S, we want S->N 
          do j=1,grd%nlat-2
             grid(:,j) = gridrev(:,grd%nlat-1-j)
          end do
          if (should_zero_increments_for('v_inc')) grid = 0.0_r_kind
          ! write to file
          call nccheck_incr(nf90_put_var(ncid_out, vvarid, sngl(grid), &
                            start = ncstart, count = nccount))
       endif
       ncstart(3) = grd%nsig-k ! GSI is sfc->top, FV3 is top->sfc
    end do
    ! delp increment
    ncstart = (/ 1, 1, grd%nsig /)
    call mpi_gatherv(pssm(1),grd%ijn(mm1),mpi_rtype,&
         work1,grd%ijn,grd%displs_g,mpi_rtype,&
         mype_out,mpi_comm_world,ierror)
    do k=1,grd%nsig
       if (mype == mype_out) then
          call load_grid(work1,gridrev)
          ! GSI is N->S, we want S->N 
          do j=1,grd%nlat-2
             grid(:,j) = gridrev(:,grd%nlat-1-j)
          end do
          delp(:,:,k) = grid * (bk5(k)-bk5(k+1)) * r1000
          if (should_zero_increments_for('delp_inc')) delp(:,:,k) = 0.0_r_kind
          ! write to file
          call nccheck_incr(nf90_put_var(ncid_out, delpvarid, sngl(delp(:,:,k)), &
                            start = ncstart, count = nccount))
       endif
       ncstart(3) = grd%nsig-k ! GSI is sfc->top, FV3 is top->sfc
    end do
    ! delz increment
    ncstart = (/ 1, 1, grd%nsig /)
    do k=1,grd%nsig
       call mpi_gatherv(dzsm(1,k),grd%ijn(mm1),mpi_rtype,&
            work1,grd%ijn,grd%displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype == mype_out) then
          call load_grid(work1,gridrev)
          ! GSI is N->S, we want S->N 
          do j=1,grd%nlat-2
             grid(:,j) = gridrev(:,grd%nlat-1-j) * -1.0_r_kind ! flip sign
          end do
          if (should_zero_increments_for('delz_inc')) grid = 0.0_r_kind
          ! write to file
          call nccheck_incr(nf90_put_var(ncid_out, delzvarid, sngl(grid), &
                            start = ncstart, count = nccount))
       endif
       ncstart(3) = grd%nsig-k ! GSI is sfc->top, FV3 is top->sfc
    end do
    ! Temperature Increment
    ncstart = (/ 1, 1, grd%nsig /)
    do k=1,grd%nsig
       call mpi_gatherv(tsensm(1,k),grd%ijn(mm1),mpi_rtype,&
            work1,grd%ijn,grd%displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype == mype_out) then
          call load_grid(work1,gridrev)
          ! GSI is N->S, we want S->N 
          do j=1,grd%nlat-2
             grid(:,j) = gridrev(:,grd%nlat-1-j)
          end do
          if (should_zero_increments_for('T_inc')) grid = 0.0_r_kind
          ! write to file
          call nccheck_incr(nf90_put_var(ncid_out, tvarid, sngl(grid), &
                            start = ncstart, count = nccount))
       endif
       ncstart(3) = grd%nsig-k ! GSI is sfc->top, FV3 is top->sfc
    end do
    ! specific humidity increment
    ncstart = (/ 1, 1, grd%nsig /)
    do k=1,grd%nsig
       call mpi_gatherv(qsm(1,k),grd%ijn(mm1),mpi_rtype,&
            work1,grd%ijn,grd%displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype == mype_out) then
          call load_grid(work1,gridrev)
          ! GSI is N->S, we want S->N 
          do j=1,grd%nlat-2
             grid(:,j) = gridrev(:,grd%nlat-1-j)
          end do
          if (should_zero_increments_for('sphum_inc')) grid = 0.0_r_kind
          ! write to file
          call nccheck_incr(nf90_put_var(ncid_out, sphumvarid, sngl(grid), &
                            start = ncstart, count = nccount))
       endif
       ncstart(3) = grd%nsig-k ! GSI is sfc->top, FV3 is top->sfc
    end do
    ! liquid water increment
    ncstart = (/ 1, 1, grd%nsig /)
    do k=1,grd%nsig
       call mpi_gatherv(qlsm(1,k),grd%ijn(mm1),mpi_rtype,&
            work1,grd%ijn,grd%displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype == mype_out) then
          call load_grid(work1,gridrev)
          ! GSI is N->S, we want S->N 
          do j=1,grd%nlat-2
             grid(:,j) = gridrev(:,grd%nlat-1-j)
          end do
          if (should_zero_increments_for('liq_wat_inc')) grid = 0.0_r_kind
          ! write to file
          call nccheck_incr(nf90_put_var(ncid_out, liqwatvarid, sngl(grid), &
                            start = ncstart, count = nccount))
       endif
       ncstart(3) = grd%nsig-k ! GSI is sfc->top, FV3 is top->sfc
    end do
    ! ozone increment
    ncstart = (/ 1, 1, grd%nsig /)
    do k=1,grd%nsig
       call mpi_gatherv(ozsm(1,k),grd%ijn(mm1),mpi_rtype,&
            work1,grd%ijn,grd%displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype == mype_out) then
          call load_grid(work1,gridrev)
          ! GSI is N->S, we want S->N 
          do j=1,grd%nlat-2
             grid(:,j) = gridrev(:,grd%nlat-1-j)
          end do
          if (should_zero_increments_for('o3mr_inc')) grid = 0.0_r_kind
          ! write to file
          call nccheck_incr(nf90_put_var(ncid_out, o3varid, sngl(grid), &
                            start = ncstart, count = nccount))
       endif
       ncstart(3) = grd%nsig-k ! GSI is sfc->top, FV3 is top->sfc
    end do
    ! ice mixing ratio increment
    ncstart = (/ 1, 1, grd%nsig /)
    do k=1,grd%nsig
       call mpi_gatherv(qism(1,k),grd%ijn(mm1),mpi_rtype,&
            work1,grd%ijn,grd%displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype == mype_out) then
          call load_grid(work1,gridrev)
          ! GSI is N->S, we want S->N 
          do j=1,grd%nlat-2
             grid(:,j) = gridrev(:,grd%nlat-1-j)
          end do
          if (should_zero_increments_for('icmr_inc')) grid = 0.0_r_kind
          ! write to file
          call nccheck_incr(nf90_put_var(ncid_out, icvarid, sngl(grid), &
                            start = ncstart, count = nccount))
       endif
       ncstart(3) = grd%nsig-k ! GSI is sfc->top, FV3 is top->sfc
    end do
    ! cleanup and exit
    if ( mype == mype_out ) then
       call nccheck_incr(nf90_close(ncid_out))
       write(6,*) "FV3 netCDF increment written, file= "//trim(filename)//".nc"
    end if

    ! deallocate preds/state
    call deallocate_preds(sbiasinc)
    do iii=1,nobs_bins
       call deallocate_state(svalinc(iii))
    end do
    do iii=1,nsubwin
       call deallocate_state(mvalinc(iii))
    end do
    do iii=1,ntlevs_ens
       call deallocate_state(evalinc(iii))
    end do

  end subroutine write_fv3_inc_

  !=======================================================================

  !! Is this variable in incvars_to_zero?
  logical function should_zero_increments_for(check_var)
    use control_vectors, only : nvars, incvars_to_zero

    character(len=*), intent(in) :: check_var !! Variable to search for

    ! Local variables

    character(len=10) :: varname ! temporary string for storing variable names
    integer :: i ! incvars_to_zero loop index

    should_zero_increments_for=.false.

    zeros_loop: do i=1,nvars
       varname = incvars_to_zero(i)
       if ( trim(varname) == check_var ) then
          should_zero_increments_for=.true.
          return
       endif
    end do zeros_loop

  end function should_zero_increments_for


  subroutine nccheck_incr(status)
    use netcdf
    integer, intent (in   ) :: status
    if (status /= nf90_noerr) then
      print *, "fv3_increment netCDF error", trim(nf90_strerror(status))
      call stop2(999)
    end if
  end subroutine nccheck_incr

end module write_incr
