module convinfo 
!$$$   module documentation block
!                .      .    .                                       .
! module:    convinfo
!   prgmmr: derber          org: np2                date: 2005-02-08
!
! abstract:  This module contains variables and routines related
!            to the assimilation of conventional observations 
!
! program history log:
!   2005-02-08  derber  - original code - consolidated from read routines
!
! Subroutines Included:
!   sub convinfo_read   - allocate arrays for and read in conventional info
!   sub convinfo_destroy - destroy conventional info arrays
!
! Variable Definitions:
!   def nconvtype      - number of input conventional types
!   def ictype         - observation type
!   def icsubtype      - observation subtype                           
!   def icuse          - use flag                                        
!   def ctwind         - time window (absolute value)            
!   def ncnumgrp       - cross validation parameter - number of groups
!   def ncgroup        - cross validation parameter - group to remove from data use
!   def ncmiter        - cross validation parameter - external iteration to introduce removed data
!   def cgross         - gross error parameter - gross error
!   def cermax         - gross error parameter - max error
!   def cermin         - gross error parameter - min error
!   def cvar_b         - variational quality control parameter -  b parameter
!   def cvar_pg        - variational quality control parameter -  pg parameter
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only:r_kind,i_kind

  integer(i_kind) nconvtype
  real(r_kind),allocatable,dimension(:)::ctwind,cgross,cermax,cermin,cvar_b,cvar_pg
  integer(i_kind),allocatable,dimension(:):: ncmiter,ncgroup,ncnumgrp,icuse,ictype,icsubtype
  character(len=16),allocatable,dimension(:)::ioctype

contains

  subroutine convinfo_read(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convinfo_read      read conventional information file
!     prgmmr:    derber    org: np2                date: 2006-02-08
!
! abstract:  This routine reads the conventional information file
!
! program history log:
!   2006-02-08  derber 
!
!   input argument list:
!     mype - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use kinds, only: r_kind,i_kind
    use constants, only: zero, izero
    implicit none
    
    integer(i_kind),intent(in) ::mype
    character(len=1)cflg
    character(len=7) iotype
    character(len=120) crecord
    integer(i_kind) lunin,i,nc

    lunin = 47
    open(lunin,file='convinfo',form='formatted')
    rewind(lunin)
    nconvtype=0

    do i=1,1010
        read(lunin,1030,end=130)cflg
1030    format(a1,a7,2x,a120)
        if(cflg == '!')cycle
        nconvtype=nconvtype+1
    end do
 130 continue
    if(nconvtype == 0) then
       write(6,*) 'READCONVINFO: NO CONVENTIONAL DATA USED'
       return
    end if
  
    allocate(ctwind(nconvtype),cgross(nconvtype),cermax(nconvtype),cermin(nconvtype), &
             cvar_b(nconvtype),cvar_pg(nconvtype),ncmiter(nconvtype),ncgroup(nconvtype), &
             ncnumgrp(nconvtype),icuse(nconvtype),ictype(nconvtype),icsubtype(nconvtype), &
             ioctype(nconvtype))
    nc=zero
    rewind(lunin)
    do i=1,1010
       read(lunin,1030,end=131)cflg,iotype,crecord
       if(cflg == '!')cycle
       nc=nc+1
       ioctype(nc)=iotype
       read(crecord,*)ictype(nc),icsubtype(nc),icuse(nc),ctwind(nc),ncnumgrp(nc), &
            ncgroup(nc),ncmiter(nc),cgross(nc),cermax(nc),cermin(nc),cvar_b(nc),cvar_pg(nc)
       if(mype == izero)write(6,1031)ioctype(nc),ictype(nc),icsubtype(nc),icuse(nc),ctwind(nc),ncnumgrp(nc), &
            ncgroup(nc),ncmiter(nc),cgross(nc),cermax(nc),cermin(nc),cvar_b(nc),cvar_pg(nc)
1031   format('READ_CONVINFO: ',a7,1x,i3,1x,i4,1x,i2,1x,g12.6,1x,3(I3,1x),6g12.6)
    end do
 131 continue
    close(lunin)
    
    return
  end subroutine convinfo_read
  subroutine convinfo_destroy
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convinfo_destroy      destroy conventional information file
!     prgmmr:    derber    org: np2                date: 2006-02-08
!
! abstract:  This routine destroys arrays from convinfo file
!
! program history log:
!   2006-02-08  derber 
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

     deallocate(ictype,icsubtype,icuse,ctwind,ncnumgrp,ncgroup,ncmiter,cgross,cermax, &
                cermin,cvar_b,cvar_pg)
     return
  end subroutine convinfo_destroy
  
end module convinfo
