module readsatobs

!  This program contains modules to read data from satellite 
!  radiance diag files written out by GSI forward operator code.
!  It is based on src/enkf/readsatobs.f90
!  Kristen Bathmann
!  2-2019

use ckinds, only: r_kind,r_radstat,i_kind,r_radstat,r_double
use read_diag
implicit none

public :: get_satobs_data, get_chaninfo
public :: indR, chaninfo, errout
public :: nch_active,nctot
public :: RadData

integer(i_kind),dimension(:),allocatable:: indR  !indices of the assimlated channels
real(r_kind),dimension(:),allocatable:: chaninfo !wavenumbers of assimilated channels
real(r_kind),dimension(:),allocatable:: errout   !satinfo obs errors of assimilated channels
integer(i_kind):: nch_active                     !number of actively assimilated channels
integer(i_kind):: nctot                          !total number of channels (passive+active)
integer(i_kind), parameter:: full_chan=1
integer(i_kind), parameter:: Sea=1
integer(i_kind), parameter:: Land =2
integer(i_kind), parameter:: Snow=3
integer(i_kind), parameter:: Mixed=4
integer(i_kind), parameter:: Ice=5
integer(i_kind), parameter:: Snow_and_Ice=6
integer(i_kind), parameter:: Clear_FOV=1
integer(i_kind), parameter:: Clear_Channel=2
real(r_kind), parameter:: clear_threshold=0.01_r_kind     !if using clear sky data, do not use if above this threshold
real(r_kind), parameter:: sea_threshold=0.99_r_kind       !if using sea data, do not use if below this threshold
real(r_kind), parameter:: lower_sea_threshold=0.9_r_kind  !if using mixed data, do not use if above this threshold
real(r_kind), parameter:: lower_land_threshold=0.9_r_kind !if using mixed data, do not use if above this threshold
real(r_kind), parameter:: lower_ice_threshold=0.9_r_kind  !if using mixed data, do not use if above this threshold
real(r_kind), parameter:: lower_snow_threshold=0.9_r_kind !if using mixed data, do not use if above this threshold
real(r_kind), parameter:: land_threshold=0.99_r_kind      !if using land data, do not use if above this threshold
real(r_kind), parameter:: ice_threshold=0.95_r_kind       !if using ice data, do not use if below this threshold
real(r_kind), parameter:: snow_threshold=0.99_r_kind      !if using snow data, do not use if below this threshold
   
type:: RadData
   real(r_radstat),dimension(:,:),allocatable:: omg
   real(r_radstat),dimension(:,:),allocatable:: latlon
   real(r_radstat),dimension(:),allocatable:: timeobs
end type RadData
contains

!get information on the activley assimilated channels
subroutine get_chaninfo(filename,netcdf,chan_choice)
  implicit none

   character(len=9), intent(in)  :: filename
   logical, intent(in) :: netcdf
   integer(i_kind),intent(in):: chan_choice

   if (netcdf) then
      call get_chaninfo_nc(filename,chan_choice)
   else
      call get_chaninfo_bin(filename,chan_choice)
   endif
end subroutine get_chaninfo

! get information on the actively assimilated channels from binary file
subroutine get_chaninfo_bin(filename,chan_choice)
  use read_diag, only: diag_header_fix_list,diag_header_chan_list,diag_data_name_list
  implicit none

   character(len=9), intent(in)  :: filename
   integer(i_kind), intent(in):: chan_choice
   integer(i_kind) iunit, iflag, n, i,istatus
   type(diag_header_fix_list )         :: header_fix0
   type(diag_header_chan_list),allocatable :: header_chan0(:)
   type(diag_data_name_list)           :: data_name0

   iunit = 7
   call open_radiag(trim(filename),iunit,istatus)
   call read_radiag_header(iunit,.false.,header_fix0,header_chan0,data_name0,iflag,.false.)
   nctot=header_fix0%nchan
   if (chan_choice==full_chan) then
      nch_active=nctot
   else
      nch_active=0
      do n=1,header_fix0%nchan
         if(header_chan0(n)%iuse<1) cycle
         nch_active=nch_active+1
      end do 
   endif
   allocate(indR(nch_active),chaninfo(nch_active),errout(nch_active))
   i=0
   do n=1,header_fix0%nchan
      if (chan_choice==full_chan) then
         indR(n)=n
      else if (header_chan0(n)%iuse>0) then
         i=i+1
         indR(i)=n
      endif
   end do
   do n=1,nch_active
      chaninfo(n)=header_chan0(indR(n))%wave
      errout(n)=header_chan0(indR(n))%varch
   end do
   call close_radiag(filename,iunit)
end subroutine get_chaninfo_bin

subroutine get_chaninfo_nc(filename,chan_choice)
  use nc_diag_read_mod, only: nc_diag_read_get_var
  use nc_diag_read_mod, only: nc_diag_read_get_dim
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_close
  implicit none

   character(len=9), intent(in)  :: filename
   integer(i_kind), intent(in):: chan_choice
   integer(i_kind) iunit, nobs, i,j
   integer(i_kind), dimension(:), allocatable ::Use_Flag,chind
   real(r_double), dimension(:), allocatable:: Waves,Inv_Errors
   real(r_kind), dimension(:), allocatable:: Wave,Inv_Error

   iunit = 7
   call nc_diag_read_init(filename, iunit)
   nobs = nc_diag_read_get_dim(iunit,'nobs')
   if (nobs <= 0) call nc_diag_read_close(filename)
   nctot = nc_diag_read_get_dim(iunit,'nchans')
   allocate(Use_Flag(nctot),chind(nobs),Wave(nctot),Inv_Error(nctot))
   allocate(Waves(nctot),Inv_Errors(nctot))
   call nc_diag_read_get_var(iunit, 'use_flag', Use_Flag)
   call nc_diag_read_get_var(iunit, 'Channel_Index', chind)
   call nc_diag_read_get_var(iunit, 'wavenumber', Waves)
   call nc_diag_read_get_var(iunit, 'error_variance', Inv_Errors)
   Wave=real(Waves,r_kind)
   Inv_Error=real(Inv_Errors,r_kind)
   call nc_diag_read_close(filename)
   if (chan_choice==full_chan) then
      nch_active=nctot
   else
      nch_active=0
      do i=1,nctot
         if(Use_Flag(chind(i)) < 1 ) cycle 
         nch_active=nch_active+1
      enddo
   endif
   allocate(indR(nch_active),chaninfo(nch_active),errout(nch_active))
   i=0
   do j=1,nctot
      if (chan_choice==full_chan) then
         indR(j)=j
      else if (Use_Flag(chind(j))>0) then
         i=i+1
         indR(i)=j
      end if
   end do
   do j=1,nch_active
      chaninfo(j)=Wave(chind(indR(j)))
      errout(j)=Inv_Error(chind(indR(j)))
   end do
   deallocate(Use_flag,chind,Wave,Inv_Error,Waves,Inv_Errors)
end subroutine get_chaninfo_nc

! read radiance data
subroutine get_satobs_data(filename,netcdf_diag,nobs_max,Surface_Type,Cloud_Type,satang,Rad,ng)
  implicit none

   character*9, intent(in):: filename
   integer(i_kind), intent(in):: nobs_max,Surface_Type,Cloud_Type
   real(r_kind), intent(in)::satang
   integer(i_kind), intent(out) :: ng
   type(RadData),intent(inout):: Rad
   logical,intent(in):: netcdf_diag

   if (netcdf_diag) then
      call get_satobs_data_nc(filename,nobs_max,Surface_Type,Cloud_Type,satang,Rad,ng)
   else
      call get_satobs_data_bin(filename,nobs_max,Surface_Type,Cloud_Type,satang,Rad,ng)
   endif
end subroutine get_satobs_data

! read radiance data from binary file
subroutine get_satobs_data_bin(filename,nobs_max,Surface_Type,Cloud_Type,satang,Rad,ng)
  implicit none

   character*9, intent(in):: filename
   integer(i_kind), intent(in):: nobs_max,Surface_Type,Cloud_Type
   real(r_kind), intent(in)::satang
   type(RadData),intent(inout):: Rad
   integer(i_kind), intent(out):: ng
   integer(i_kind):: iunit, iflag,nob,n
   integer(i_kind):: istatus,nc,nc2
   real(r_kind):: errorlimit
   type(diag_header_fix_list):: header_fix
   type(diag_header_chan_list),allocatable:: header_chan(:)
   type(diag_data_fix_list):: data_fix
   type(diag_data_chan_list),allocatable:: data_chan(:)
   type(diag_data_extra_list) ,allocatable:: data_extra(:,:)
   type(diag_data_name_list):: data_name

! make consistent with screenobs
   errorlimit=1._r_kind/sqrt(1.e9_r_kind)
   iunit = 7
   nob = 0
   call open_radiag(filename,iunit,istatus)
   call read_radiag_header(iunit,.false.,header_fix,header_chan,data_name,iflag,.false.)
   allocate(data_chan(header_fix%nchan),data_extra(header_fix%iextra,header_fix%nchan))
   do
      call read_radiag_data(iunit,header_fix,.false.,data_fix,data_chan,data_extra,iflag )
      if( iflag /= 0 ) exit
      if ((Surface_Type==Sea).and.(data_fix%water_frac<sea_threshold)) cycle 
      if ((Surface_Type==Land).and.(data_fix%land_frac<land_threshold)) cycle 
      if ((Surface_Type==Snow_And_Ice).and.((data_fix%snow_frac<snow_threshold).and. &
         (data_fix%ice_frac<ice_threshold))) cycle
      if ((Surface_Type==Snow).and.(data_fix%snow_frac<snow_threshold)) cycle
      if ((Surface_Type==Ice).and.(data_fix%ice_frac<ice_threshold))  cycle 
      if ((Surface_Type==Mixed).and.(data_fix%water_frac>=lower_sea_threshold)) cycle 
      if ((Surface_Type==Mixed).and.(data_fix%land_frac>=lower_land_threshold)) cycle 
      if ((Surface_Type==Mixed).and.(data_fix%ice_frac>=lower_ice_threshold)) cycle
      if ((Surface_Type==Mixed).and.(data_fix%snow_frac>=lower_snow_threshold)) cycle
      if ((Cloud_Type==Clear_FOV).and.(data_fix%qcdiag1>clear_threshold)) cycle 
      if (abs(data_fix%satzen_ang)>satang) cycle
      nc=0
      nc2=0
      nob=nob+1
      chan:do n=1,header_fix%nchan
         if((header_chan(n)%iuse<1).and.(nch_active<header_fix%nchan)) cycle chan
         nc=nc+1
         if ((abs(data_chan(n)%qcmark)>0).or.(data_chan(n)%errinv < errorlimit) ) cycle chan
         Rad%omg(nob,nc)=data_chan(n)%omgbc
         nc2=nc2+1
      enddo chan
      if ((nc2<1).and.(nob>0)) then
         nob=nob-1
      else
         Rad%latlon(nob,1)=data_fix%lat
         Rad%latlon(nob,2)=data_fix%lon
         Rad%timeobs(nob)=data_fix%obstime
      endif
      if (nob==nobs_max) then
         print *, 'Warning:  Number of obs meeting criteria exceeds dsize. Consider increasing dsize'
         exit
      endif
   enddo
   ng=nob
   call close_radiag(filename,iunit)
   deallocate(data_chan,data_extra)
 end subroutine get_satobs_data_bin

! read radiance data from netcdf file
subroutine get_satobs_data_nc(filename,nobs_max,Surface_Type,Cloud_Type,satang,Rad,ng)
  use nc_diag_read_mod, only: nc_diag_read_get_var
  use nc_diag_read_mod, only: nc_diag_read_get_dim, nc_diag_read_get_global_attr
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_close

  implicit none

   character*9, intent(in):: filename
   integer(i_kind), intent(in):: nobs_max,Surface_Type,Cloud_Type
   real(r_kind), intent(in)::satang
   type(RadData), intent(inout):: Rad
   integer(i_kind), intent(out):: ng
   integer(i_kind):: iunit, nobs, i, nchans,nob,nc,nct,nc2
   real(r_kind) :: errorlimit
   integer(i_kind), dimension(:), allocatable :: Use_Flag, chind
   real(r_radstat), dimension(:), allocatable :: Inv_Error,QC_Flag
   real(r_radstat), dimension(:), allocatable :: Latitude, Longitude, Time
   real(r_radstat), dimension(:), allocatable :: Obs_Minus_Forecast_adjusted,satzen_ang
   real(r_radstat), dimension(:), allocatable :: fwater,fland,fsnow,fice,cldfrac

! make consistent with screenobs
   errorlimit=1._r_kind/sqrt(1.e9_r_kind)
   nob = 0
   call nc_diag_read_init(trim(filename), iunit)
   nobs = nc_diag_read_get_dim(iunit,'nobs')
   if (nobs <= 0) call nc_diag_read_close(trim(filename))
   nchans = nc_diag_read_get_dim(iunit,'nchans')
   allocate(Use_Flag(nchans))
   allocate(QC_Flag(nobs), Inv_Error(nobs), Latitude(nobs), &
            Longitude(nobs), Time(nobs), chind(nobs),    &
            Obs_Minus_Forecast_adjusted(nobs),fwater(nobs),fland(nobs),&
            fice(nobs),fsnow(nobs),cldfrac(nobs),satzen_ang(nobs))
   call nc_diag_read_get_var(iunit, 'use_flag', Use_Flag)
   call nc_diag_read_get_var(iunit, 'Channel_Index', chind)
   call nc_diag_read_get_var(iunit, 'QC_Flag', QC_Flag)
   call nc_diag_read_get_var(iunit, 'Inverse_Observation_Error', Inv_Error)
   call nc_diag_read_get_var(iunit, 'Latitude', Latitude)
   call nc_diag_read_get_var(iunit, 'Longitude', Longitude)
   call nc_diag_read_get_var(iunit, 'Obs_Time', Time)
   call nc_diag_read_get_var(iunit, 'Obs_Minus_Forecast_adjusted', Obs_Minus_Forecast_adjusted)
   call nc_diag_read_get_var(iunit, 'Water_Fraction',fwater)
   call nc_diag_read_get_var(iunit, 'Land_Fraction',fland)
   call nc_diag_read_get_var(iunit, 'Snow_Fraction',fsnow)
   call nc_diag_read_get_var(iunit, 'Ice_Fraction',fice)
   call nc_diag_read_get_var(iunit, 'Cloud_Frac',cldfrac)
   call nc_diag_read_get_var(iunit, 'Sat_Zenith_Angle',satzen_ang)
   call nc_diag_read_close(filename)

   nct=0 !ranges from 0 to total number of channels (active+passive)
   nc=0  !ranges from 0 to total number of active channels
   nob=1
   nc2=0 !counts how many channels actually pass qc
   do i=1,nobs
      nct=nct+1
      if (nct>nchans) then
         nct=1
         if (nc2>0) nob=nob+1
         nc=0
         nc2=0
      endif
      if ((Surface_Type==Sea).and.(fwater(i)<sea_threshold)) cycle
      if ((Surface_Type==Land).and.(fland(i)<land_threshold)) cycle
      if ((Surface_Type==Snow_And_Ice).and.((fsnow(i)<snow_threshold).and.(fice(i)<ice_threshold))) cycle
      if ((Surface_Type==Snow).and.(fsnow(i)<snow_threshold)) cycle
      if ((Surface_Type==Ice).and.(fice(i)<ice_threshold))  cycle
      if ((Surface_Type==Mixed).and.(fwater(i)>=lower_sea_threshold)) cycle
      if ((Surface_Type==Mixed).and.(fland(i)>=lower_land_threshold)) cycle
      if ((Surface_Type==Mixed).and.(fice(i)>=lower_ice_threshold)) cycle
      if ((Surface_Type==Mixed).and.(fsnow(i)>=lower_snow_threshold)) cycle
      if ((Cloud_Type==Clear_FOV).and.(cldfrac(i)>clear_threshold)) cycle
      if (abs(satzen_ang(i))>satang) cycle
      if ((Use_Flag(chind(i))<1).and.(nch_active < nchans)) cycle
      nc=nc+1
      if ((abs(QC_Flag(i))>0).or.(Inv_Error(i)<errorlimit)) cycle
      nc2=nc2+1
      Rad%omg(nob,nc)=Obs_Minus_Forecast_adjusted(i) !is this correct?
      if (nc==1) then
         Rad%latlon(nob,1)=Latitude(i)
         Rad%latlon(nob,2)=Longitude(i)
         Rad%timeobs(nob)=Time(i)
      endif
      if (((nob==nobs_max).and.(nc==nch_active)).or.(nob>nobs_max)) then
         print *, 'Warning:  Number of obs meeting criteria exceeds dsize. Consider increasing dsize'
         exit
      endif
   enddo
   ng=nob
   deallocate(QC_Flag, Inv_Error, Latitude, Longitude, Time, &
              chind, Obs_Minus_Forecast_adjusted,Use_Flag, &
              fwater,fland,fsnow,fice,cldfrac,satzen_ang)
end subroutine get_satobs_data_nc

end module readsatobs
