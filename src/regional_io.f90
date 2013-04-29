module regional_io
!$$$   module documentation block
!                .      .    .                                       .
! module:  regional_io
! prgmmr:  treadon           org: np23                date: 2004-12-29
!
! abstract: This module contains routines that handle the input/output
!           of regional gsi guess(analysis) grids
!
! program history log:
!   2004-12-29  treadon
!   2005-05-24  pondeca - add 2dvar only surface analysis option
!   2005-07-06  parrish - add variable update_pint
!   2005-10-17  parrish - add ctph0,stph0,tlm0 
!   2010-09-15  pagowski - add cmaq
!   
! Subroutines Included:
!   sub convert_regional_guess  - convert regional guess to internal format
!   sub write_regional_analysis - write regional analysis
!
! variable definitions:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use gridmod, only: wrf_mass_regional,wrf_nmm_regional,&
       nems_nmmb_regional,cmaq_regional,&
       twodvar_regional,netcdf
  use mpimod, only: mpi_comm_world,ierror
  implicit none

! set default to private
  private
! set subroutines to public
  public :: convert_regional_guess
  public :: write_regional_analysis
! set passed variables to public
  public :: update_pint,preserve_restart_date

  logical update_pint            !  if true, then this is nmm run with pint variable, so update pint
                                 !    (where pint is non-hydrostatic 3-d pressure variable)
  logical preserve_restart_date  !  if true, then do not update date information on restart file

contains


  subroutine convert_regional_guess(mype,ctph0,stph0,tlm0)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convert_regional_guess
!     prgmmr:    treadon     org: np23                date: 2004-12-29
!
! abstract:  converts wrf input guess file to internal gsi format
!
! program history log:
!   2004-12-29  treadon
!   2005-05-24  pondeca - add 2dvar only surface analysis option
!   2005-07-06  parrish - add variable update_pint
!
!   input argument list:
!      mype - mpi task id
!
!   output argument list:
!      ctph0,stph0,tlm0
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$ end documentation block

    use kinds, only: i_kind,r_kind
    use constants, only: izero,ione
    use mpimod, only: mpi_integer4,mpi_rtype
    use hybrid_ensemble_parameters, only: l_hyb_ens,regional_ensemble_option
    implicit none

!   Declare passed variables
    integer(i_kind),intent(in   ) :: mype
    real(r_kind)   ,intent(  out) :: ctph0,stph0,tlm0


!   Convert nmm guess file to internal gsi format.  Consider
!   two possible input formats:  netcdf or binary

    update_pint=.false.
    if (wrf_nmm_regional) then
       if (mype==izero) then
          if (netcdf) then
             call convert_netcdf_nmm(update_pint,ctph0,stph0,tlm0)
             if (l_hyb_ens .and. regional_ensemble_option == 2)then
                call convert_netcdf_nmm_ens
             end if
          else
             call convert_binary_nmm(update_pint,ctph0,stph0,tlm0)
             if (l_hyb_ens .and. regional_ensemble_option == 2)then
                call convert_binary_nmm_ens
             end if
          end if
       end if
       call mpi_barrier(mpi_comm_world,ierror)
       call mpi_bcast(update_pint,ione,mpi_integer4,izero,mpi_comm_world,ierror)
       call mpi_bcast(ctph0,ione,mpi_rtype,izero,mpi_comm_world,ierror)
       call mpi_bcast(stph0,ione,mpi_rtype,izero,mpi_comm_world,ierror)
       call mpi_bcast(tlm0,ione,mpi_rtype,izero,mpi_comm_world,ierror)


!   Convert mass guess file to internal gsi format.  Consider
!   two possible input formats:  netcdf or binary

    elseif (wrf_mass_regional) then
       if (mype==izero) then
          if (netcdf) then
             call convert_netcdf_mass
          else
             call convert_binary_mass
          end if
       end if
       call mpi_barrier(mpi_comm_world,ierror)

    elseif (cmaq_regional) then
       if (mype==izero) then
!cmaq binary is read in directly, only need to link file to sigf
          call make_sigf
       end if
       
       call mpi_barrier(mpi_comm_world,ierror)
       
!   Convert nems nmmb guess file to internal gsi format.

    elseif (nems_nmmb_regional) then
       if (mype==izero) then
          call convert_nems_nmmb(update_pint,ctph0,stph0,tlm0)
       end if
       call mpi_barrier(mpi_comm_world,ierror)
       call mpi_bcast(update_pint,ione,mpi_integer4,izero,mpi_comm_world,ierror)
       call mpi_bcast(ctph0,ione,mpi_rtype,izero,mpi_comm_world,ierror)
       call mpi_bcast(stph0,ione,mpi_rtype,izero,mpi_comm_world,ierror)
       call mpi_bcast(tlm0,ione,mpi_rtype,izero,mpi_comm_world,ierror)

!   Convert binary twodvar guess file to internal gsi format.

    elseif (twodvar_regional) then
       if (mype==izero) then
          call convert_binary_2d
       end if
       call mpi_barrier(mpi_comm_world,ierror)
    end if

    return
  end subroutine convert_regional_guess


  subroutine write_regional_analysis(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    write_regional_analysis
!     prgmmr:    treadon     org:  np23               date: 2004-12-29
!
! abstract:  write regional analysis grid to output file
!
! program history log:
!   2004-12-29  treadon
!   2005-05-24  pondeca - add 2dvar only surface analysis option
!
!   input argument list:
!      mype - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$ end documentation block

    use kinds, only: i_kind
    use constants, only: izero
    implicit none

!   Declare passed variables
    integer(i_kind),intent(in):: mype

!   Write nmm analysis file.  Consider two possible
!   output formats:  netcdf or binary
    if (wrf_nmm_regional) then
       if (netcdf) then
          call wrwrfnmma_netcdf(mype)
          if (mype==izero) then
             call update_netcdf_nmm
          end if
          call mpi_barrier(mpi_comm_world,ierror)
       else
          call wrwrfnmma_binary(mype)
       end if
    end if

!   Write mass analysis file.  Consider two possible
!   output formats:  netcdf or binary
    if (wrf_mass_regional) then
       if(netcdf) then
          call wrwrfmassa_netcdf(mype)
          if (mype==izero) then
             call update_netcdf_mass
          endif
          call mpi_barrier(mpi_comm_world,ierror)
       else
          call wrwrfmassa_binary(mype)
       end if
    end if

!write cmaq analysis

    if (cmaq_regional) call write_cmaq(mype)

!   Write nems nmmb analysis file.

    if (nems_nmmb_regional) call wrnemsnmma_binary(mype)

!   Write 2d analysis file
!   output format: binary
    if (twodvar_regional) call wr2d_binary(mype)

    return
  end subroutine write_regional_analysis
  
end module regional_io
