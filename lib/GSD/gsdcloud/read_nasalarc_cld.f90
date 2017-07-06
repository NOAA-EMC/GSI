SUBROUTINE read_NASALaRC(mype,lunin,numLaRC,regional_time,istart,jstart,  &
                         nlon,nlat,nasalarc)
!
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_NASALaRC        read in nasalarc cloud 
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2009-09-21
!
! ABSTRACT: 
!  This subroutine read in nasalarc
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     mype        - processor ID
!     lunin       - unit in which data are read in
!     numLaRC    - number of observation
!     regional_time - analysis time
!     jstart      - start lon of the whole array on each pe
!     istart      - start lat of the whole array on each pe
!     nlon        - no. of lons on subdomain (buffer points on ends)
!     nlat        - no. of lats on subdomain (buffer points on ends)
!
!   output argument list:
!     nasalarc   - nasalarc cloud  in analysis grid
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!
!_____________________________________________________________________
!

  use kinds, only: r_kind,i_kind, r_single
  implicit none

  integer(i_kind),intent(in) :: lunin
  integer(i_kind),intent(in) :: mype
  INTEGER(i_kind),intent(in) :: numLaRC 
  INTEGER(i_kind),intent(in) :: nlon,nlat
  integer(i_kind),intent(in) :: regional_time(6)
  integer(i_kind),intent(in) :: istart
  integer(i_kind),intent(in) :: jstart

  real(r_single), intent(out) :: nasalarc(nlon,nlat,5)
!
!  local
!
  real(r_kind),allocatable :: nasalarc_in(:,:)

  character(10)  :: obstype
  integer(i_kind):: nreal,nchanl,ilat1s,ilon1s
  character(20)  :: isis

  INTEGER(i_kind) :: i,j, ii,jj,k2, k
  INTEGER(i_kind) :: ib,jb

  REAL(r_kind)    :: miss_obs_real
  PARAMETER ( miss_obs_real = -99999.0_r_kind )

!
  ib=jstart   ! begin i point of this domain
  jb=istart   ! begin j point of this domain

  ilon1s=1
  ilat1s=2

  read(lunin) obstype,isis,nreal,nchanl

  allocate( nasalarc_in(nreal,numLaRC) )
  nasalarc_in=miss_obs_real

  read(lunin)  nasalarc_in
  DO i=1,numLaRC
    ii=int(nasalarc_in(ilon1s,i)+0.001_r_kind) - ib + 2
    jj=int(nasalarc_in(ilat1s,i)+0.001_r_kind) - jb + 2
    if( ii < 1 .or. ii > nlon ) write(6,*) 'read_nasalarc_cld: ', &
                                'Error in read in nasa ii:',mype,ii,jj,i,ib,jb
    if( jj < 1 .or. jj > nlat ) write(6,*) 'read_nasalarc_cld: ', &
                                'Error in read in nasa jj:',mype,ii,jj,i,ib,jb
    DO k=1,2
      if(nasalarc_in(k+2,i) > 8888.0_r_kind ) then
        nasalarc(ii,jj,k)=miss_obs_real
      else
        nasalarc(ii,jj,k)=nasalarc_in(k+2,i)   ! k=1 w_pcld, 2=w_tcld
      endif
    enddo  ! k

    if(nasalarc_in(5,i) > 8888.0_r_kind ) then
        nasalarc(ii,jj,3)=miss_obs_real
    else
        nasalarc(ii,jj,3)=nasalarc_in(5,i)/100.0_r_kind   ! w_frac
    endif

    if(nasalarc_in(6,i) > 8888.0_r_kind) then
        nasalarc(ii,jj,4)=miss_obs_real
    else
        nasalarc(ii,jj,4)=nasalarc_in(6,i)/1000.0_r_kind  ! w_lwp
    endif

    if(nasalarc_in(7,i) > 8888.0_r_kind ) then
        nasalarc(ii,jj,5)=miss_obs_real
    else
        nasalarc(ii,jj,5)=nasalarc_in(7,i)   ! nlv_cld
    endif
  ENDDO
  deallocate(nasalarc_in)
!
!  filling boundarys
!
  DO k=1,5
    DO i=2,nlon-1
      nasalarc(i,1,k)=nasalarc(i,2,k)
      nasalarc(i,nlat,k)=nasalarc(i,nlat-1,k)
    enddo
    DO j=2,nlat-1
      nasalarc(1,j,k)=nasalarc(2,j,k)
      nasalarc(nlon,j,k)=nasalarc(nlon-1,j,k)
    enddo
    nasalarc(1,1,k)=nasalarc(2,2,k)
    nasalarc(1,nlat,k)=nasalarc(2,nlat-1,k)
    nasalarc(nlon,1,k)=nasalarc(nlon-1,2,k)
    nasalarc(nlon,nlat,k)=nasalarc(nlon-1,nlat-1,k)
  ENDDO


END SUBROUTINE read_NASALaRC
