subroutine prt_guess(sgrep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    prt_guess
!  prgmmr:       tremolet
!
! abstract: Print some diagnostics about the guess arrays
!
! program history log:
!   2007-04-13  tremolet - initial code
!   2007-04-17  todling  - time index to summarize; bound in arrays
!   2009-01-17  todling  - update tv/tsen names
!
!   input argument list:
!    sgrep  - prefix for write statement
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,npe,mype
  use constants, only: izero,ione,zero
  use gridmod, only: lat1,lon1,itotsub,nsig
  use guess_grids, only: ges_div,ges_vor,ges_ps,ges_cwmr,ges_tv,ges_q,&
       ges_tsen,ges_oz,ges_u,ges_v,ges_prsl,sfct
  use guess_grids, only: ntguessig,ntguessfc
  use radinfo, only: predx
  use pcpinfo, only: predxp
  use jfunc, only: npclen,nsclen

  implicit none

! Declare passed variables
  character(len=*), intent(in) :: sgrep

! Declare local variables
  integer(i_kind), parameter :: nvars=12_i_kind
  integer(i_kind) ii
  integer(i_kind) ntsig
  integer(i_kind) ntsfc
  real(r_kind) :: zloc(3*nvars+2_i_kind),zall(3*nvars+2_i_kind,npe),zz
  real(r_kind) :: zmin(nvars+2_i_kind),zmax(nvars+2_i_kind),zavg(nvars+2_i_kind)
  character(len=4) :: cvar(nvars+2_i_kind)

!*******************************************************************************

  ntsig = ntguessig
  ntsfc = ntguessfc

  cvar( 1)='U   '
  cvar( 2)='V   '
  cvar( 3)='TV  '
  cvar( 4)='Q   '
  cvar( 5)='TSEN'
  cvar( 6)='OZ  '
  cvar( 7)='CW  '
  cvar( 8)='DIV '
  cvar( 9)='VOR '
  cvar(10)='PRSL'
  cvar(11)='PS  '
  cvar(12)='SST '
  cvar(13)='radb'
  cvar(14)='pcpb'

  zloc(1)                 = sum   (ges_u   (2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(2)                 = sum   (ges_v   (2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(3)                 = sum   (ges_tv  (2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(4)                 = sum   (ges_q   (2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(5)                 = sum   (ges_tsen(2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(6)                 = sum   (ges_oz  (2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(7)                 = sum   (ges_cwmr(2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(8)                 = sum   (ges_div (2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(9)                 = sum   (ges_vor (2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(10)                = sum   (ges_prsl(2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(11)                = sum   (ges_ps  (2:lat1+ione,2:lon1+ione,       ntsig))
  zloc(12)                = sum   (sfct    (2:lat1+ione,2:lon1+ione,       ntsfc))
  zloc(nvars+ione)        = minval(ges_u   (2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(nvars+2_i_kind)    = minval(ges_v   (2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(nvars+3_i_kind)    = minval(ges_tv  (2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(nvars+4_i_kind)    = minval(ges_q   (2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(nvars+5_i_kind)    = minval(ges_tsen(2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(nvars+6_i_kind)    = minval(ges_oz  (2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(nvars+7_i_kind)    = minval(ges_cwmr(2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(nvars+8_i_kind)    = minval(ges_div (2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(nvars+9_i_kind)    = minval(ges_vor (2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(nvars+10_i_kind)   = minval(ges_prsl(2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(nvars+11_i_kind)   = minval(ges_ps  (2:lat1+ione,2:lon1+ione,       ntsig))
  zloc(nvars+12_i_kind)   = minval(sfct    (2:lat1+ione,2:lon1+ione,       ntsfc))
  zloc(2*nvars+ione)      = maxval(ges_u   (2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(2*nvars+2_i_kind)  = maxval(ges_v   (2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(2*nvars+3_i_kind)  = maxval(ges_tv  (2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(2*nvars+4_i_kind)  = maxval(ges_q   (2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(2*nvars+5_i_kind)  = maxval(ges_tsen(2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(2*nvars+6_i_kind)  = maxval(ges_oz  (2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(2*nvars+7_i_kind)  = maxval(ges_cwmr(2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(2*nvars+8_i_kind)  = maxval(ges_div (2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(2*nvars+9_i_kind)  = maxval(ges_vor (2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(2*nvars+10_i_kind) = maxval(ges_prsl(2:lat1+ione,2:lon1+ione,1:nsig,ntsig))
  zloc(2*nvars+11_i_kind) = maxval(ges_ps  (2:lat1+ione,2:lon1+ione,       ntsig))
  zloc(2*nvars+12_i_kind) = maxval(sfct    (2:lat1+ione,2:lon1+ione,       ntsfc))
  zloc(3*nvars+ione)      = real(lat1*lon1*nsig*ntsig,r_kind)
  zloc(3*nvars+2_i_kind)  = real(lat1*lon1*ntsig,r_kind)

! Gather contributions
  call mpi_allgather(zloc,3*nvars+2_i_kind,mpi_rtype, &
                   & zall,3*nvars+2_i_kind,mpi_rtype, mpi_comm_world,ierror)

  if (mype==izero) then
    zmin=zero
    zmax=zero
    zavg=zero
    zz=SUM(zall(3*nvars+ione,:))
    do ii=1,nvars-2_i_kind
       zavg(ii)=SUM(zall(ii,:))/zz
    enddo
    zz=SUM(zall(3*nvars+2_i_kind,:))
    do ii=nvars-ione,nvars
       zavg(ii)=SUM(zall(ii,:))/zz
    enddo
    do ii=1,nvars
       zmin(ii)=MINVAL(zall(  nvars+ii,:))
       zmax(ii)=MAXVAL(zall(2*nvars+ii,:))
    enddo

!   Duplicated part of vector
    if (nsclen>izero) then
       zmin(nvars+ione)  = minval(predx(:,:))
       zmax(nvars+ione)  = maxval(predx(:,:))
       zavg(nvars+ione)  = sum(predx(:,:))/nsclen
    endif
    if (npclen>izero) then
       zmin(nvars+2_i_kind) = minval(predxp(:,:))
       zmax(nvars+2_i_kind) = maxval(predxp(:,:))
       zavg(nvars+2_i_kind) = sum(predxp(:,:))/npclen
    endif

    write(6,'(80a)') ('=',ii=1,80)
    write(6,'(a,2x,a,10x,a,17x,a,20x,a)') 'Status ', 'Var', 'Mean', 'Min', 'Max'
    do ii=1,nvars+2_i_kind
       write(6,999)sgrep,cvar(ii),zavg(ii),zmin(ii),zmax(ii)
    enddo
    write(6,'(80a)') ('=',ii=1,80)
  endif
999 format(A,1X,A,3(1X,ES20.12))

  return
end subroutine prt_guess

subroutine prt_guessfc(sgrep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    prt_guessfc
!   prgmmr:      todling
!
! abstract: Print some diagnostics about the guess arrays
!
! program history log:
!   2009-01-23  todling - create based on prt_guess
!
!   input argument list:
!    sgrep  - prefix for write statement
!
!   output argument list
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,npe,mype
  use constants, only: izero,ione
  use gridmod, only: lat1,lon1
  use guess_grids, only: isli,fact10,veg_type,veg_frac,sfc_rough,&
        soil_type,soil_temp,soil_moi
  use guess_grids, only: ntguessfc

  implicit none

! Declare passed variables
  character(len=*), intent(in) :: sgrep

! Declare local variables
  integer(i_kind), parameter :: nvars=8_i_kind
  integer(i_kind) ii
  integer(i_kind) ntsfc
  real(r_kind) :: zloc(3*nvars+ione),zall(3*nvars+ione,npe),zz
  real(r_kind) :: zmin(nvars+ione),zmax(nvars+ione),zavg(nvars+ione)
  character(len=4) :: cvar(nvars+ione)

!*******************************************************************************

  ntsfc = ntguessfc

  cvar( 1)='ISLI'
  cvar( 2)='FC10'
  cvar( 3)='VTYP'
  cvar( 4)='VFRC'
  cvar( 5)='SRGH'
  cvar( 6)='STYP'
  cvar( 7)='STMP'
  cvar( 8)='SMST'

  zloc(1)                 = sum   (isli     (2:lat1+ione,2:lon1+ione,ntsfc))
  zloc(2)                 = sum   (fact10   (2:lat1+ione,2:lon1+ione,ntsfc))
  zloc(3)                 = sum   (veg_type (2:lat1+ione,2:lon1+ione,ntsfc))
  zloc(4)                 = sum   (veg_frac (2:lat1+ione,2:lon1+ione,ntsfc))
  zloc(5)                 = sum   (sfc_rough(2:lat1+ione,2:lon1+ione,ntsfc))
  zloc(6)                 = sum   (soil_type(2:lat1+ione,2:lon1+ione,ntsfc))
  zloc(7)                 = sum   (soil_temp(2:lat1+ione,2:lon1+ione,ntsfc))
  zloc(8)                 = sum   (soil_moi (2:lat1+ione,2:lon1+ione,ntsfc))
  zloc(nvars+ione)        = minval(isli     (2:lat1+ione,2:lon1+ione,ntsfc))
  zloc(nvars+2_i_kind)    = minval(fact10   (2:lat1+ione,2:lon1+ione,ntsfc))
  zloc(nvars+3_i_kind)    = minval(veg_type (2:lat1+ione,2:lon1+ione,ntsfc))
  zloc(nvars+4_i_kind)    = minval(veg_frac (2:lat1+ione,2:lon1+ione,ntsfc))
  zloc(nvars+5_i_kind)    = minval(sfc_rough(2:lat1+ione,2:lon1+ione,ntsfc))
  zloc(nvars+6_i_kind)    = minval(soil_type(2:lat1+ione,2:lon1+ione,ntsfc))
  zloc(nvars+7_i_kind)    = minval(soil_temp(2:lat1+ione,2:lon1+ione,ntsfc))
  zloc(nvars+8_i_kind)    = minval(soil_moi (2:lat1+ione,2:lon1+ione,ntsfc))
  zloc(2*nvars+ione)      = maxval(isli     (2:lat1+ione,2:lon1+ione,ntsfc))
  zloc(2*nvars+2_i_kind)  = maxval(fact10   (2:lat1+ione,2:lon1+ione,ntsfc))
  zloc(2*nvars+3_i_kind)  = maxval(veg_type (2:lat1+ione,2:lon1+ione,ntsfc))
  zloc(2*nvars+4_i_kind)  = maxval(veg_frac (2:lat1+ione,2:lon1+ione,ntsfc))
  zloc(2*nvars+5_i_kind)  = maxval(sfc_rough(2:lat1+ione,2:lon1+ione,ntsfc))
  zloc(2*nvars+6_i_kind)  = maxval(soil_type(2:lat1+ione,2:lon1+ione,ntsfc))
  zloc(2*nvars+7_i_kind)  = maxval(soil_temp(2:lat1+ione,2:lon1+ione,ntsfc))
  zloc(2*nvars+8_i_kind)  = maxval(soil_moi (2:lat1+ione,2:lon1+ione,ntsfc))
  zloc(3*nvars+ione)      = real(SIZE(isli),r_kind)

! Gather contributions
  call mpi_allgather(zloc,3*nvars+ione,mpi_rtype, &
                   & zall,3*nvars+ione,mpi_rtype, mpi_comm_world,ierror)

  zz=SUM(zall(3*nvars+ione,:))
  do ii=1,nvars
     zavg(ii)=SUM(zall(ii,:))/zz
  enddo
  do ii=1,nvars
     zmin(ii)=MINVAL(zall(  nvars+ii,:))
     zmax(ii)=MAXVAL(zall(2*nvars+ii,:))
  enddo

  if (mype==izero) then
     write(6,'(80a)') ('=',ii=1,80)
     write(6,'(a,2x,a,10x,a,17x,a,20x,a)') 'Status ', 'Var', 'Mean', 'Min', 'Max'
     do ii=1,nvars
        write(6,999)sgrep,cvar(ii),zavg(ii),zmin(ii),zmax(ii)
     enddo
     write(6,'(80a)') ('=',ii=1,80)
  endif
999 format(A,1X,A,3(1X,ES20.12))

  return
end subroutine prt_guessfc


subroutine prt_guessfc2(sgrep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    prt_guessfc2
!   pgrmmr:      todling
!
! abstract: Print some diagnostics about the guess arrays
!
! program history log:
!   2009-01-23  todling  - create based on prt_guess
!
!   input argument list:
!    sgrep  - prefix for write statement
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use mpimod, only: mype
  use constants, only: izero,ione
  use satthin, only: isli_full,fact10_full,soil_moi_full,soil_temp_full,veg_frac_full,&
       soil_type_full,veg_type_full,sfc_rough_full,sst_full,sno_full
  use guess_grids, only: ntguessfc

  implicit none

! Declare passed variables
  character(len=*), intent(in) :: sgrep

! Declare local variables
  integer(i_kind), parameter :: nvars=10_i_kind
  integer(i_kind) ii
  integer(i_kind) ntsfc
  real(r_kind) :: zall(3*nvars+2_i_kind),zz
  real(r_kind) :: zmin(nvars+2_i_kind),zmax(nvars+2_i_kind),zavg(nvars+2_i_kind)
  character(len=4) :: cvar(nvars+2_i_kind)

!*******************************************************************************

  if (mype==izero) then
     ntsfc = ntguessfc

     cvar( 1)='FC10'
     cvar( 2)='VTYP'
     cvar( 3)='VFRC'
     cvar( 4)='SRGH'
     cvar( 5)='STMP'
     cvar( 6)='SMST'
     cvar( 7)='SST '
     cvar( 8)='SNOW'
     cvar( 9)='ISLI'
     cvar(10)='STYP'

     zall(1)                 = sum   (fact10_full   )
     zall(2)                 = sum   (veg_type_full )
     zall(3)                 = sum   (veg_frac_full )
     zall(4)                 = sum   (sfc_rough_full)
     zall(5)                 = sum   (soil_temp_full)
     zall(6)                 = sum   (soil_moi_full )
     zall(7)                 = sum   (sst_full      )
     zall(8)                 = sum   (sno_full      )
     zall(9)                 = sum   (isli_full     )
     zall(10)                = sum   (soil_type_full)
     zall(nvars+ione)        = minval(fact10_full   )
     zall(nvars+2_i_kind)    = minval(veg_type_full )
     zall(nvars+3_i_kind)    = minval(veg_frac_full )
     zall(nvars+4_i_kind)    = minval(sfc_rough_full)
     zall(nvars+5_i_kind)    = minval(soil_temp_full)
     zall(nvars+6_i_kind)    = minval(soil_moi_full )
     zall(nvars+7_i_kind)    = minval(sst_full      )
     zall(nvars+8_i_kind)    = minval(sno_full      )
     zall(nvars+9_i_kind)    = minval(isli_full     )
     zall(nvars+10_i_kind)   = minval(soil_type_full)
     zall(2*nvars+ione)      = maxval(fact10_full   )
     zall(2*nvars+2_i_kind)  = maxval(veg_type_full )
     zall(2*nvars+3_i_kind)  = maxval(veg_frac_full )
     zall(2*nvars+4_i_kind)  = maxval(sfc_rough_full)
     zall(2*nvars+5_i_kind)  = maxval(soil_temp_full)
     zall(2*nvars+6_i_kind)  = maxval(soil_moi_full )
     zall(2*nvars+7_i_kind)  = maxval(sst_full      )
     zall(2*nvars+8_i_kind)  = maxval(sno_full      )
     zall(2*nvars+9_i_kind)  = maxval(isli_full     )
     zall(2*nvars+10_i_kind) = maxval(soil_type_full)
     zall(3*nvars+ione)      = real(SIZE(fact10_full),r_kind)
     zall(3*nvars+2_i_kind)  = real(SIZE(isli_full),r_kind)

     zz=zall(3*nvars+ione)
     do ii=1,nvars-2_i_kind
        zavg(ii)=zall(ii)/zz
     enddo
     zz=zall(3*nvars+2_i_kind)
     do ii=nvars-ione,nvars
        zavg(ii)=zall(ii)/zz
     enddo
     do ii=1,nvars
        zmin(ii)=zall(  nvars+ii)
        zmax(ii)=zall(2*nvars+ii)
     enddo

     if (mype==izero) then
        write(6,'(80a)') ('=',ii=1,80)
        write(6,'(a,2x,a,10x,a,17x,a,20x,a)') 'Status ', 'Var', 'Mean', 'Min', 'Max'
        do ii=1,nvars
           write(6,999)sgrep,cvar(ii),zavg(ii),zmin(ii),zmax(ii)
        enddo
        write(6,'(80a)') ('=',ii=1,80)
     endif
999 format(A,1X,A,3(1X,ES20.12))

  endif

  return
end subroutine prt_guessfc2
