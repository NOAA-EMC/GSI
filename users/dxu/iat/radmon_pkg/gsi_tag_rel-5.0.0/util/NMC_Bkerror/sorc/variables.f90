module variables
!$$$ module documentation block
  use kinds, only: r_kind,r_double,i_kind
  implicit none

! general
  integer maxcases,filunit1,filunit2
  real(r_kind) smoothdeg

! forecast pair file variables
  character(100),allocatable,dimension(:):: filename
  integer,allocatable,dimension(:):: na,nb

! from GSI gridmod:
  logical hybrid,db_prec,biasrm,vertavg
  integer nlat,nlon,nsig,dimbig,option,noq,lat1,lon1
  integer ntrac5,idvc5,idvm5,idpsfc5,idthrm5
  real(r_kind),allocatable,dimension(:):: rlons,ak5,bk5,ck5,cp5
  real(r_double),allocatable,dimension(:):: wgtlats,rlats

! MPI related stuff
  integer mype,npe,iglobal,itotsub
  integer(i_kind),allocatable,dimension(:):: jstart  ! start lon of the whole array on each pe
  integer(i_kind),allocatable,dimension(:):: istart  ! start lat of the whole array on each pe
  integer(i_kind),allocatable,dimension(:):: ilat1   ! no. of lats for each subdomain (no buffer)
  integer(i_kind),allocatable,dimension(:):: jlon1   ! no. of lons for each subdomain (no buffer)
  integer(i_kind),allocatable,dimension(:):: ijn_s   ! no. of horiz. points for each subdomain (with buffer)
  integer(i_kind),allocatable,dimension(:):: ijn     ! no. of horiz. points for each subdomain (no buffer)
  integer(i_kind),allocatable,dimension(:):: isc_g   ! no. array, count for send to global; size of subdomain

                                               ! comm. array ...
  integer(i_kind),allocatable,dimension(:):: irc_s     !   count for receive on subdomain
  integer(i_kind),allocatable,dimension(:):: ird_s     !   displacement for receive on subdomain
  integer(i_kind),allocatable,dimension(:):: isd_g     !   displacement for send to global
  integer(i_kind),allocatable,dimension(:):: displs_s  !   displacement for send from subdomain
  integer(i_kind),allocatable,dimension(:):: displs_g  !   displacement for receive on global grid

                                             ! array element indices for location of ...
  integer(i_kind),allocatable,dimension(:):: ltosi   !   lats in iglobal array excluding buffer
  integer(i_kind),allocatable,dimension(:):: ltosj   !   lons in iglobal array excluding buffer
  integer(i_kind),allocatable,dimension(:):: ltosi_s !   lats in itotsub array including buffer
  integer(i_kind),allocatable,dimension(:):: ltosj_s !   lons in itotsub array including buffer


! allocateable arrays
  real(r_kind),allocatable,dimension(:):: sweight

! Bias correction arrays
  real(r_kind),allocatable,dimension(:,:,:):: bbiasz,bbiasd,bbiast,bcorrz,bcorrd,bcorrt
  real(r_kind),allocatable,dimension(:,:):: bbiasp,bcorrp

! variances
  real(r_kind),allocatable,dimension(:,:):: sfvar,vpvar,tvar,qvar,ozvar,cvar,nrhvar
  real(r_kind),allocatable,dimension(:):: psvar

! horizontal length scales
  real(r_kind),allocatable,dimension(:,:):: sfhln,vphln,thln,qhln,ozhln,chln
  real(r_kind),allocatable,dimension(:):: pshln

! vertical length scales
  real(r_kind),allocatable,dimension(:,:):: sfvln,vpvln,tvln,qvln,ozvln,cvln

! balance constraints
  real(r_kind),allocatable,dimension(:,:,:):: tcon
  real(r_kind),allocatable,dimension(:,:):: pscon,vpcon

! other variables
  real(r_double),allocatable,dimension(:):: coef,coriolis

! from GSI constants:
  integer izero
  real(r_kind) rd,rv,cvap,cliq,zero,one,hvap,&
                psat,ttp,fv,pi,hfus,csol,deg2rad,grav,&
                half,two,omega
  real(r_double) rearth
  integer(i_kind) ione

! define parms
  parameter(rearth = 6.3712e+6_r_double)   !  radius of earth                 (m)
  parameter(omega  = 7.2921e-5_r_kind)  !  angular velocity of earth       (1/s)
  parameter(rd     = 2.8705e+2_r_kind)  !  gas constant of dry air         (J/kg/K)
  parameter(rv     = 4.6150e+2_r_kind)  !  gas constant of h2o vapor       (J/kg/K)
  parameter(cvap   = 1.8460e+3_r_kind)  !  specific heat of h2o vapor      (J/kg/K)
  parameter(cliq   = 4.1855e+3_r_kind)  !  specific heat of liquid h2o     (J/kg/K)
  parameter(hvap   = 2.5000e+6_r_kind)  !  latent heat of h2o condensation (J/kg)
  parameter(psat   = 6.1078e+2_r_kind)  !  pressure at h2o triple point    (Pa)
  parameter(csol   = 2.1060e+3_r_kind)  !  specific heat of solid h2o (ice)(J/kg/K)
  parameter(ttp    = 2.7316e+2_r_kind)  !  temperature at h2o triple point (K)
  parameter(hfus   = 3.3358e+5_r_kind)  !  latent heat of h2o fusion       (J/kg)
  parameter(pi     = 3.141593e+0_r_kind)!  pi                              ()
  parameter(grav   = 9.80665_r_kind)    ! gravity
  parameter(izero  = 0)
  parameter(zero   = 0.0_r_kind)
  parameter(one    = 1.0_r_kind)
  parameter(two    = 2.0_r_kind)
  parameter(half   = one/two)
  parameter(ione   = 1_i_kind)

! Derived constants
  parameter(fv = rv/rd-1._r_kind)       ! used in virtual temp. equation   ()

contains 

  subroutine init_defaults

    implicit none
    
    nsig=64
    maxcases=10
    nlat=258
    nlon=512
    hybrid=.false.
    biasrm=.false.
    vertavg=.false.
    smoothdeg=4.0
    dimbig=5000
    noq=5

  end subroutine init_defaults

  subroutine create_grids
    implicit none
    
    allocate(coef(3*nlat+4*(2*(noq+5)+1)*(nlat+nlon/2)))
    allocate(coriolis(nlat))

    allocate(sfvar(nlat,nsig),vpvar(nlat,nsig),&
             tvar(nlat,nsig),qvar(nlat,nsig),&
             ozvar(nlat,nsig),cvar(nlat,nsig),psvar(nlat))

    allocate(nrhvar(nlat,nsig))

    allocate(sfhln(nlat,nsig),vphln(nlat,nsig),&
             thln(nlat,nsig),qhln(nlat,nsig),&
             ozhln(nlat,nsig),chln(nlat,nsig),pshln(nlat))

    allocate(sfvln(nlat,nsig),vpvln(nlat,nsig),&
             tvln(nlat,nsig),qvln(nlat,nsig),&
             ozvln(nlat,nsig),cvln(nlat,nsig))

    allocate(tcon(nlat,nsig,nsig),vpcon(nlat,nsig),&
             pscon(nlat,nsig))

! initialize all variables to zero
    sfvar=zero ; vpvar=zero ; tvar=zero ; qvar=zero ; ozvar=zero
    cvar=zero ; psvar =zero ; nrhvar=zero
    sfhln=zero ; vphln=zero ; thln=zero ; qhln=zero ; ozhln=zero 
    chln=zero ; pshln=zero
    sfvln=zero ; vpvln=zero ; tvln=zero ; qvln=zero ; ozvln=zero
    cvln=zero 
    tcon=zero ; vpcon=zero ; pscon=zero

  end subroutine create_grids

  subroutine create_mapping
    implicit none
    integer(i_kind) i

    allocate(jstart(npe),istart(npe),&
         ilat1(npe),jlon1(npe),&
       ijn_s(npe),irc_s(npe),ird_s(npe),displs_s(npe),&
       ijn(npe),isc_g(npe),isd_g(npe),displs_g(npe))

    do i=1,npe
      jstart(i)    = izero
      istart(i)    = izero
      ilat1(i)     = izero
      jlon1(i)     = izero
      ijn_s(i)     = izero
      irc_s(i)     = izero
      ird_s(i)     = izero
      displs_s(i)  = izero
      ijn(i)       = izero
      isc_g(i)     = izero
      isd_g(i)     = izero
      displs_g(i)  = izero
    end do

    return
  end subroutine create_mapping

  subroutine destroy_mapping
    implicit none
    deallocate(ltosi,ltosj,ltosi_s,ltosj_s)
    deallocate(jstart,istart,ilat1,jlon1,&
       ijn_s,displs_s,&
       ijn,isc_g,isd_g,displs_g)

    return
  end subroutine destroy_mapping

  subroutine destroy_biasrm
    implicit none
    deallocate(bbiasz,bbiasd,bbiast,bcorrz,bcorrd,bcorrt)
    deallocate(bbiasp,bcorrp)

    return
  end subroutine destroy_biasrm

  subroutine destroy_grids
    implicit none
 
    deallocate(filename,na,nb)
    deallocate(sfvar,vpvar,tvar,qvar,ozvar,cvar,psvar)
    deallocate(sfhln,vphln,thln,qhln,ozhln,chln,pshln)
    deallocate(sfvln,vpvln,tvln,qvln,ozvln,cvln)
    deallocate(tcon,vpcon,pscon)
    deallocate(nrhvar)
  end subroutine destroy_grids

  subroutine destroy_variables
    deallocate(rlats,rlons)
    deallocate(ak5,bk5,ck5,cp5)
    deallocate(coef)
    deallocate(coriolis)

    return
  end subroutine destroy_variables

end module variables
