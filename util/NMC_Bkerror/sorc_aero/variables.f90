module variables
!$$$ module documentation block
  use kinds, only: r_kind,r_single,r_double,i_kind
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
  real(r_single),allocatable,dimension(:):: rlons,ak5,bk5,ck5,cp5
  real(r_double),allocatable,dimension(:):: wgtlats,rlats
  logical use_nemsio

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
  real(r_kind),allocatable,dimension(:,:):: d1var,d2var,d3var,d4var,d5var, &
                                            s1var,s2var,s3var,s4var, &
                                            so4var,oc1var,oc2var,bc1var,bc2var

! horizontal length scales
  real(r_kind),allocatable,dimension(:,:):: d1hln,d2hln,d3hln,d4hln,d5hln, &
                                            s1hln,s2hln,s3hln,s4hln, &
                                            so4hln,oc1hln,oc2hln,bc1hln,bc2hln

! vertical length scales
  real(r_kind),allocatable,dimension(:,:):: d1vln,d2vln,d3vln,d4vln,d5vln, &
                                            s1vln,s2vln,s3vln,s4vln, &
                                            so4vln,oc1vln,oc2vln,bc1vln,bc2vln

! balance constraints
!  real(r_kind),allocatable,dimension(:,:,:):: tcon
!  real(r_kind),allocatable,dimension(:,:):: pscon,vpcon

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

  real(r_kind),parameter::  r60       = 60._r_kind
  real(r_kind),parameter::  r3600     = 3600._r_kind

! Derived constants
  parameter(fv = rv/rd-1._r_kind)       ! used in virtual temp. equation   ()

! aerosol name in different model
  character(len=4) :: modelname
  character(len=8),dimension(15):: aeroname,ngacaeroname,fv3aeroname
  data ngacaeroname /'du001','du002','du003','du004','du005', &
                     'se001','se002','se003','se004','se005', &
                     'so4','ocphobic','ocphilic','bcphobic','bcphilic'/
  data fv3aeroname /'dust1','dust2','dust3','dust4','dust5', &
                     'seas1','seas2','seas3','seas4','seas5', &
                     'sulf','oc1','oc2','bc1','bc2'/

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
    use_nemsio=.false.
    modelname='ngac'

  end subroutine init_defaults

  subroutine create_grids
    implicit none
    
    allocate(coef(3*nlat+4*(2*(noq+5)+1)*(nlat+nlon/2)))
    allocate(coriolis(nlat))

    allocate(d1var(nlat,nsig),d2var(nlat,nsig),d3var(nlat,nsig),d4var(nlat,nsig),d5var(nlat,nsig), &
             s1var(nlat,nsig),s2var(nlat,nsig),s3var(nlat,nsig),s4var(nlat,nsig),so4var(nlat,nsig), &
             oc1var(nlat,nsig),oc2var(nlat,nsig),bc1var(nlat,nsig),bc2var(nlat,nsig))

    allocate(d1hln(nlat,nsig),d2hln(nlat,nsig),d3hln(nlat,nsig),d4hln(nlat,nsig),d5hln(nlat,nsig), &
             s1hln(nlat,nsig),s2hln(nlat,nsig),s3hln(nlat,nsig),s4hln(nlat,nsig),so4hln(nlat,nsig), &
             oc1hln(nlat,nsig),oc2hln(nlat,nsig),bc1hln(nlat,nsig),bc2hln(nlat,nsig))

    allocate(d1vln(nlat,nsig),d2vln(nlat,nsig),d3vln(nlat,nsig),d4vln(nlat,nsig),d5vln(nlat,nsig), &
             s1vln(nlat,nsig),s2vln(nlat,nsig),s3vln(nlat,nsig),s4vln(nlat,nsig),so4vln(nlat,nsig), &
             oc1vln(nlat,nsig),oc2vln(nlat,nsig),bc1vln(nlat,nsig),bc2vln(nlat,nsig))


! initialize all variables to zero
    d1var=zero ; d2var=zero ; d3var=zero ; d4var=zero ; d5var=zero 
    s1var=zero ; s2var=zero ; s3var=zero ; s4var=zero ; so4var=zero 
    oc1var=zero ; oc2var=zero ; bc1var=zero ; bc2var=zero 

    d1hln=zero ; d2hln=zero ; d3hln=zero ; d4hln=zero ; d5hln=zero 
    s1hln=zero ; s2hln=zero ; s3hln=zero ; s4hln=zero ; so4hln=zero 
    oc1hln=zero ; oc2hln=zero ; bc1hln=zero ; bc2hln=zero 

    d1vln=zero ; d2vln=zero ; d3vln=zero ; d4vln=zero ; d5vln=zero 
    s1vln=zero ; s2vln=zero ; s3vln=zero ; s4vln=zero ; so4vln=zero 
    oc1vln=zero ; oc2vln=zero ; bc1vln=zero ; bc2vln=zero 

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
    deallocate(d1var,d2var,d3var,d4var,d5var, &
               s1var,s2var,s3var,s4var,so4var, &
               oc1var,oc2var,bc1var,bc2var)
    deallocate(d1hln,d2hln,d3hln,d4hln,d5hln, &
               s1hln,s2hln,s3hln,s4hln,so4hln, &
               oc1hln,oc2hln,bc1hln,bc2hln)
    deallocate(d1vln,d2vln,d3vln,d4vln,d5vln, &
               s1vln,s2vln,s3vln,s4vln,so4vln, &
               oc1vln,oc2vln,bc1vln,bc2vln)


  end subroutine destroy_grids

  subroutine destroy_variables
    deallocate(rlats,rlons)
    deallocate(ak5,bk5,ck5,cp5)
    deallocate(coef)
    deallocate(coriolis)

    return
  end subroutine destroy_variables

end module variables
