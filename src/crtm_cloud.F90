!-----------------------------------------------------------------------------
!BOP
 
! !MODULE: crtm_cloud --- Implements the Cloud-CRTM Interface
!
! !INTERFACE:
!

module crtm_cloud

! !USES:

  use kinds, only: i_kind,r_kind
  use constants, only: zero,one,two,five,r0_05,t0c,fv
  use CRTM_Cloud_Define, only: CRTM_Cloud_type
  use CRTM_Cloud_Define, only: WATER_CLOUD,ICE_CLOUD,RAIN_CLOUD, &
                               SNOW_CLOUD,GRAUPEL_CLOUD,HAIL_CLOUD 

  use mpeu_util,         only: die

  implicit none

! !PUBLIC METHODS:

  public setCloud

! !REVISION HISTORY:
!
! 14May2011  Todling   Initial version, based on da Silva's Aerosols interface.
!
!EOP
!-----------------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------------
!BOP
 
! !IROUTINE: setCloud --- Set CRTM Cloud Object 
!
! !INTERFACE:
!

  subroutine setCloud (cloud_name, icmask, cloud_cont, dp, tp, pr, qh, cloud)

! !ARGUMENTS:

  character(len=*), intent(in)    :: cloud_name(:)     ! [nc]    Model cloud names: Water, Ice, etc.
  logical,          intent(in)    :: icmask            !         mask for where to consider clouds  
  real(r_kind),     intent(in)    :: cloud_cont(:,:)   ! [km,nc] cloud contents  (kg/m2)
  real(r_kind),     intent(in)    :: dp(:)             ! [km]    layer thickness   
  real(r_kind),     intent(in)    :: tp(:)             ! [km]    atmospheric temperature (K)
  real(r_kind),     intent(in)    :: pr(:)             ! [km]    atmospheric pressure (??)
  real(r_kind),     intent(in)    :: qh(:)             ! [km]    atmospheric specific humidity (??)

  type(CRTM_Cloud_type), intent(inout) :: cloud(:)     ! [nc]   CRTM Cloud object

! !DESCRIPTION: Set the CRTM Cloud object given Model cloud properties.
!
! !REVISION HISTORY:
!
! 03May2011  Min-Jeong  Initial version.
! 14May2011  Todling    Encapsulate Min-Jeong's code in present module.
!
!EOP
!-----------------------------------------------------------------------------

  character(len=*), parameter :: myname = 'setCloud'
  integer(i_kind) :: na, nc, km, n, k, rc, iTable
  real(r_kind)    :: tem1,tem2,tem3,tem4

  km = size(cloud_cont,1)
  nc = size(cloud_cont,2)
  na = size(cloud_name)

! The only case when cloud-water is split by hand that is acceptable 
! right now is when only a single cloud-water variable participates 
! in the guess; otherwise, clouds and actual clouds must equal (nc=na)
  if(nc/=na.and.na/=1) then
     rc=1
     call die(myname,'unable to handle user cloud-number',rc)
  endif

! Handle hand-split case as particular case
! -----------------------------------------
  if (na==1.and.nc==2) then

     cloud(1)%Type = 1
     cloud(2)%Type = 2
     if(icmask) then
        Cloud(1)%water_content(:) = cloud_cont(:,1)
        Cloud(2)%water_content(:) = cloud_cont(:,2)
     else
        Cloud(1)%water_content(:) = zero
        Cloud(2)%water_content(:) = zero
     endif

!    Calculate effective radius for each cloud component (wired to 2)
!    ----------------------------------------------------------------   
     if(icmask) then

        do k=1,km
           ! liquid water cloud drop size
           tem4=max(zero,(t0c-tp(k))*r0_05)
           cloud(1)%effective_radius(k) = five + five * min(one, tem4)
      
           ! ice water cloud particle size
           tem2 = tp(k) - t0c
                       ! WARNING: tem1 was defined nowhere in original code!! (so it was in original code)
           tem1 = tem2 ! _RT4Min-Jeong NEEDS CORRECTION
           tem3 = tem1 * cloud(2)%water_content(k) * (pr(k)/dp(k)) &
                 /tp(k) * (one + fv * qh(k))
   
           if (tem2 < -50.0_r_kind ) then
              cloud(2)%effective_radius(k) =  (1250._r_kind/9.917_r_kind)*tem3**0.109_r_kind
           elseif (tem2 < -40.0_r_kind ) then
              cloud(2)%effective_radius(k) =  (1250._r_kind/9.337_r_kind)*tem3**0.08_r_kind
           elseif (tem2 < -30.0_r_kind ) then
              cloud(2)%effective_radius(k) =  (1250._r_kind/9.208_r_kind)*tem3**0.055_r_kind
           else
              cloud(2)%effective_radius(k) =  (1250._r_kind/9.387_r_kind)*tem3**0.031_r_kind
           endif
   
           cloud(1)%effective_radius(k)=max(zero, cloud(1)%effective_radius(k))
           cloud(2)%effective_radius(k)=max(zero, cloud(2)%effective_radius(k))
   
        end do
        cloud(1)%effective_variance(:) = two
        cloud(2)%effective_variance(:) = two
   
     else
        cloud(1)%effective_radius  (:) = zero
        cloud(2)%effective_radius  (:) = zero
        cloud(1)%effective_variance(:) = two
        cloud(2)%effective_variance(:) = two
     endif

  else ! Handle general case with arbitray number of clouds
       ! --------------------------------------------------

!    Loop over clouds ...
!    --------------------
     do n = 1, nc

!       Map Model cloud names into CRTM Cloud indices
!       ---------------------------------------------   
        Cloud(n)%Type = CloudType_(cloud_name(n))

        if(icmask) then
           Cloud(n)%water_content(:) = cloud_cont(:,n)
        else
           Cloud(n)%water_content(:) = zero
        endif

!       Calculate effective radius of given cloud type 
!       ----------------------------------------------
        cloud(n)%Effective_Radius(:) = EftSize_(cloud_name(n))

     enddo

  endif

CONTAINS

  function CloudType_(name) Result(ctype)
    character(len=*) :: name  ! Model cloud name
    integer(i_kind)  :: ctype ! CRTM cloud type
    
    if ( trim(name) == 'ql' ) then
       ctype = WATER_CLOUD
    else if ( trim(name) == 'qi' ) then
       ctype = ICE_CLOUD
    else if ( trim(name) == 'qh' ) then
       ctype = HAIL_CLOUD
    else if ( trim(name) == 'qg' ) then
       ctype = GRAUPEL_CLOUD
    else if ( trim(name) == 'qr' ) then
       ctype = RAIN_CLOUD
    else if ( trim(name) == 'qs' ) then
       ctype = SNOW_CLOUD
       
    else 
       call die(myname,"cannot recognize cloud name <"//trim(name)//">")
    end if
    
  end function CloudType_

  function EftSize_(name) Result(csize)
    character(len=*) :: name  ! Model cloud name
    real(r_kind)     :: csize ! CRTM cloud type
    
! Note: Values below from Tom Auligne
    if ( trim(name) == 'ql' ) then
       csize = 10.0_r_kind  ! in micron
    else if ( trim(name) == 'qi' ) then
       csize = 30.0_r_kind
    else if ( trim(name) == 'qh' ) then
       csize = zero ! RT: can somebody fill this in?
    else if ( trim(name) == 'qg' ) then
       csize = 600.0_r_kind
    else if ( trim(name) == 'qr' ) then
       csize = 300.0_r_kind
    else if ( trim(name) == 'qs' ) then
       csize = 600.0_r_kind
       
    else 
       call die(myname,"cannot recognize cloud name <"//trim(name)//">")
    end if
    
  end function EftSize_

end subroutine setCloud

end module crtm_cloud
