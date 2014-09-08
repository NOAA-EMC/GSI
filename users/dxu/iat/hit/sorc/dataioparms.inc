!     Parameter file for dataio.f.
!
!     Separated from the main routine to allow one time maintenance
!     of the values.  This whole file is included in any subroutine
!     that needs any of these parameters.

!       numtau  is  the number of synoptic periods ahead for which
!                   tropical cyclone position forecasts are done at a
!                   single forecast time, at this writing, ten.  For
!                   all aids except cliper-120 the taus are 12, 24,
!                   36, 48, and 72.  Only the first five taus are used.
!                   For cliper-120 the taus are 12, 24, 36, 48, 60,
!                   72, 84, 96, 108, and 120.  All ten taus are used.

      integer   numtau
      parameter (numtau=10)

!        newnumtau is used for the newWriteAidRcd routine.  This
!                     includes the above mentioned forecast periods
!                     and also include tau 0.  So the taus are:
!                     0, 12, 24, 36, 48, 60, 72, 84, 96, 108 and 120.

      integer   newnumtau
      parameter (newnumtau=11)

!       llw    is   the second dimension of the ltlnwnd array.  This
!                   is for the lat, lon and wind.

      integer   llw
      parameter (llw=3)

