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
!   
! Subroutines Included:
!   convert_regional_guess  - convert regional guess to internal format
!   write_regional_analysis - write regional analysis
!
! variable definitions:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use gridmod, only: wrf_mass_regional,wrf_nmm_regional,nems_nmmb_regional,&
       twodvar_regional,netcdf
  use mpimod, only: mpi_comm_world,ierror
  implicit none

  logical update_pint        !  if true, then this is nmm run with pint variable, so update pint
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
    use mpimod, only: mpi_integer4,mpi_rtype
    implicit none

!   Declare passed variables
    integer(i_kind),intent(in):: mype
    real(r_kind),intent(out):: ctph0,stph0,tlm0


!   Convert nmm guess file to internal gsi format.  Consider
!   two possible input formats:  netcdf or binary

    update_pint=.false.
    if (wrf_nmm_regional) then
       if (mype==0) then
          if (netcdf) then
             call convert_netcdf_nmm(update_pint,ctph0,stph0,tlm0)
          else
             call convert_binary_nmm(update_pint,ctph0,stph0,tlm0)
          end if
       end if
       call mpi_barrier(mpi_comm_world,ierror)
       call mpi_bcast(update_pint,1,mpi_integer4,0,mpi_comm_world,ierror)
       call mpi_bcast(ctph0,1,mpi_rtype,0,mpi_comm_world,ierror)
       call mpi_bcast(stph0,1,mpi_rtype,0,mpi_comm_world,ierror)
       call mpi_bcast(tlm0,1,mpi_rtype,0,mpi_comm_world,ierror)


!   Convert mass guess file to internal gsi format.  Consider
!   two possible input formats:  netcdf or binary

    elseif (wrf_mass_regional) then
       if (mype==0) then
          if (netcdf) then
             call convert_netcdf_mass
          else
             call convert_binary_mass
          end if
       end if
       call mpi_barrier(mpi_comm_world,ierror)

!   Convert nems nmmb guess file to internal gsi format.

    elseif (nems_nmmb_regional) then
       if (mype==0) then
         call convert_nems_nmmb(update_pint,ctph0,stph0,tlm0)
       end if
       call mpi_barrier(mpi_comm_world,ierror)
       call mpi_bcast(update_pint,1,mpi_integer4,0,mpi_comm_world,ierror)
       call mpi_bcast(ctph0,1,mpi_rtype,0,mpi_comm_world,ierror)
       call mpi_bcast(stph0,1,mpi_rtype,0,mpi_comm_world,ierror)
       call mpi_bcast(tlm0,1,mpi_rtype,0,mpi_comm_world,ierror)

!   Convert binary twodvar guess file to internal gsi format.

    elseif (twodvar_regional) then
       if (mype==0) then
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
    implicit none

!   Declare passed variables
    integer(i_kind),intent(in):: mype

!   Write nmm analysis file.  Consider two possible
!   output formats:  netcdf or binary
    if (wrf_nmm_regional) then
       if (netcdf) then
          call wrwrfnmma_netcdf(mype)
          if (mype==0) then
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
          if (mype==0) then
             call update_netcdf_mass
          endif
          call mpi_barrier(mpi_comm_world,ierror)
       else
          call wrwrfmassa_binary(mype)
       end if
    end if

!   Write nems nmmb analysis file.

    if (nems_nmmb_regional) call wrnemsnmma_binary(mype)

!   Write 2d analysis file
!   output format: binary
    if (twodvar_regional) call wr2d_binary(mype)

    return
  end subroutine write_regional_analysis
  
end module regional_io
