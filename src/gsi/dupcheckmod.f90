module dupcheckmod
!$$$  module documentation block
!                .      .    .                                       .
! module:    dupcheckmod
!   prgmmr: pondeca/morris   org: ncep/emc           date: 2026-09-16
!
! abstract: module containing duplicate observation check code for
!           conventional observations
!
! program history log:
!   2026-09-16  pondeca/morris - original code, extracted from setup routines
!
! subroutines included:
!   def dupcheck - perform duplicate observation check
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  implicit none
  private
  public :: dupcheck

contains

subroutine dupcheck(nobs, nele, data, muse, dup, &
                    ier, itime, ilate, ilone, id, &
                    l_closeobs, min_offset, &
                    ipres, skip_pres_check)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dupcheck  perform duplicate observation check
!   prgmmr: pondeca/morris  org: ncep/emc           date: 2026-09-16
!
! abstract: check for duplicate observations at same location using
!           true lat/lon values (ilate/ilone) and station id matching.
!           duplicate stations can have lat/lon specs differing by as
!           much as epsdup (~0.005 deg).  a station can also appear as
!           a TAC station and BUFR station with lat/lon specs differing
!           by as much as epsdup_2 (~0.1 deg); the station id match
!           option handles this situation.  with l_closeobs, only the
!           observation closest to the analysis time is retained;
!           otherwise a duplicate weighting scheme is applied.
!           an optional pressure-level matching check can be applied
!           per observation via the skip_pres_check argument.
!
! program history log:
!   2026-09-16  pondeca/morris - original code extracted from setup routines
!
!   input argument list:
!     nobs     - number of observations
!     nele     - number of data elements per observation
!     data     - observation data array (nele,nobs)
!     muse     - logical array indicating if obs is used (intent inout)
!     dup      - duplicate weight array (intent inout)
!     ier      - index of observation error in data array
!     itime    - index of observation time in data array
!     ilate    - index of observation latitude (degrees) in data array
!     ilone    - index of observation longitude (degrees) in data array
!     id       - index of station id in data array
!     l_closeobs - if .true., retain only obs closest to analysis time
!     min_offset - offset of analysis time from reference time (minutes)
!     ipres    - (optional) index of pressure in data array
!     skip_pres_check - (optional) logical array of length nobs;
!                       if element k is .true., skip pressure check
!                       for obs k; defaults to .true. for all obs
!                       (i.e., no pressure check applied by default)
!
!   output argument list:
!     muse     - updated logical array indicating if obs is used
!     dup      - updated duplicate weight array
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind, i_kind, r_double
  use constants, only: one, r1000
  use qcmod, only: dfact, dfact1, epsdup, epsdup_2
! epsdup and epsdup_2 are initialized to zero in qcmod and may be set via
! the OBSQC namelist group (e.g., epsdup~0.005 deg, epsdup_2~0.1 deg)

  integer(i_kind), intent(in)    :: nobs, nele
  integer(i_kind), intent(in)    :: ier, itime, ilate, ilone, id
  real(r_kind),    intent(in)    :: min_offset
  real(r_kind),    intent(in)    :: data(nele,nobs)
  logical,         intent(inout) :: muse(nobs)
  real(r_kind),    intent(inout) :: dup(nobs)
  logical,         intent(in)    :: l_closeobs
  integer(i_kind), optional, intent(in) :: ipres
  logical,         optional, intent(in) :: skip_pres_check(:)

  integer(i_kind)            :: i, k, l, nlen, nlen2, ipres_loc
  real(r_kind)               :: hr_offset, tfact
  real(r_double)             :: rstn1, rstn2
  character(len=8)           :: cstn1, cstn2
  character(len=1), parameter :: cblank = ' '
  logical :: duplogic, duplogic_1, duplogic_2, apply_pres_k

  equivalence(rstn1, cstn1)
  equivalence(rstn2, cstn2)

  if (present(ipres)) then
     ipres_loc = ipres
  else
     ipres_loc = 0
  end if

  hr_offset = min_offset/60.0_r_kind
  dup = one

  kloop: do k=1,nobs
     if (.not. muse(k)) cycle kloop

!    Determine if pressure check applies for this observation
!    (requires both ipres and skip_pres_check to be present)
     if (present(skip_pres_check) .and. present(ipres)) then
        apply_pres_k = .not. skip_pres_check(k)
     else
        apply_pres_k = .false.
     end if

     rstn1 = data(id,k)
     nlen=0
     do i=1,8
        if (cstn1(i:i)==cblank) exit    !stop at first blank; for mesonet station ids of the form
        nlen=nlen+1                      !"STNIxxxxa" the trailing "a" in position 8 (preceded by blanks)
     enddo                               !is intentionally excluded from the comparison

     lloop: do l=k+1,nobs
        if (.not. muse(l)) cycle lloop
        rstn2 = data(id,l)
        nlen2=0
        do i=1,8
           if (cstn2(i:i)==cblank) exit
           nlen2=nlen2+1
        enddo

        duplogic_1=abs(data(ilate,k)-data(ilate,l))<=epsdup .and.  &  !duplicate stations can have lat/lon specs
        abs(data(ilone,k)-data(ilone,l))<=epsdup                      !differing by as much as epsdup (~0.005 deg)

        duplogic_2=abs(data(ilate,k)-data(ilate,l))<=epsdup_2 .and.  & !station can appear as TAC station and BUFR station
        abs(data(ilone,k)-data(ilone,l))<=epsdup_2 .and.  &            !with lat/lon specs differing by as much as epsdup_2 (~0.1 deg)
        (nlen==nlen2.and.cstn1(1:nlen)==cstn2(1:nlen))                 !this logic addresses this situation, but only when the station ids
                                                                       !are the same. when they are different, the duplicate obs will slip in

        if (apply_pres_k) then
           duplogic=(duplogic_1.or.duplogic_2).and.&
           data(ipres_loc,k) == data(ipres_loc,l) .and. &
           data(ier,k) < r1000 .and. data(ier,l) < r1000
        else
           duplogic=(duplogic_1.or.duplogic_2).and.&
           data(ier,k) < r1000 .and. data(ier,l) < r1000
        end if

        if (duplogic) then
           if(l_closeobs) then
              if(abs(data(itime,k)-hr_offset)<abs(data(itime,l)-hr_offset)) then
                  muse(l)=.false.
              else
                  muse(k)=.false.
                  exit lloop
              endif
           else
              tfact=min(one,abs(data(itime,k)-data(itime,l))/dfact1)
              dup(k)=dup(k)+one-tfact*tfact*(one-dfact)
              dup(l)=dup(l)+one-tfact*tfact*(one-dfact)
           endif
        end if
     end do lloop
  end do kloop

end subroutine dupcheck

end module dupcheckmod
