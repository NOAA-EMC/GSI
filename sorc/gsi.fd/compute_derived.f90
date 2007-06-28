subroutine compute_derived(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    compute_derived     compute derived quantites from current solution
!   prgmmr: derber           org: np2                 date: 2005-11-29
!
! abstract:  This routine performs various functions, all related in one
!            way or the other to the model guess or current solution.  
!
!            Functions performed in this routine include the following
!              a) compute guess-derived fields required by pcp forward model
!              b) compute 3d pressure grids
!              c) compute saturation specific humidity.  on first outer iteration
!                 save qs for normalization in background error.  qs for limq
!                 is updated each outer iteration.
!              d) compute 3d geopotential height
!              e) compute 2d tropopause pressure map
!
! program history log:
!   2005-11-21  derber  - new routine from read_*.f90 routines
!   2005-11-22  wu - bug fix qoption=2 for regional nmm on dqdp and 
!                    set dqdt=0 above sigma=0.15; for regional mass
!                    set dqdt=dqdp=0 above sigma=0.15
!   2005-12-09  guo - remove GMAO derivative computation code.  Use
!                         unified NCEP compact_diff procedures.
!   2006-01-09  derber - include calculation of sigsum add capability of set_nrh_var
!   2006-01-30  kleist - correct tropprs unit error in qoption=2 q/t decoupling
!   2006-02-02  treadon - consolidate/unify use of guess pressure arrays
!   2006-02-03  derber - clean up RH statistics printout
!   2006-03-07  treadon - remove ges_prslk and related code
!   2006-03-27  treadon - remove guess bias correction arrays since not used
!   2006-04-17  treadon - replace sigi with bk5; replace sigl with
!                         ges_prslavg/ges_psfcavg
!   2006-04-21  kleist - modify call to calctends
!   2006-07-31  kleist - changes to use ps instead of ln(ps)
!   2006-09-29  treadon - add option to compute 10m wind factor fields
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use m_fvAnaGrid,only : fvAnaGrid_read
  use m_fvAnaGrid,only : fvAnaGrid_surface_read
  use m_ggGradient,only : ggGradient,ggGrad
  use m_ggGradient,only : ggGradient_init,clean
  use m_die,only : die

  use kinds, only: r_kind,r_single,i_kind
  use jfunc, only: qsatg,qgues,qsinv2,jiter,jiterstart,&
       dqdt,dqdrh,dqdp,qoption,switch_on_derivatives,&
       tendsflag,varq
  use jcmod, only: jcdivt
  use mpimod, only: ierror,mpi_rtype,mpi_sum,mpi_comm_world,levs_id
  use guess_grids, only: ges_z,ges_ps,ges_u,ges_v,ges_vor,ges_div,&
       ges_tv,ges_q,ges_oz,ges_cwmr,sfct,fact10,veg_type,sno,xncld,&
       pdryini,ifilesfc,nfldsfc,isli_g,isli,tropprs,ges_prsi,ges_prsl,soil_type,&
       veg_frac,soil_moi,soil_temp,ntguessig,vtid,tracers,ifilesig,&
       nfldsig,load_prsges,load_fact10,sfcmod_gfs,sfcmod_mm5,&
       load_geop_hgt,geop_hgtl,geop_hgti, &
       ges_u_lon,ges_v_lon,ges_tvlon,ges_ps_lon,ges_qlon,ges_ozlon,sfct_lon,ges_cwmr_lon, &
       ges_u_lat,ges_v_lat,ges_tvlat,ges_ps_lat,ges_qlat,ges_ozlat,sfct_lat,ges_cwmr_lat
  use guess_grids, only: ges_u_ten,ges_v_ten,ges_tv_ten,ges_prs_ten,ges_q_ten,&
       ges_oz_ten,ges_cwmr_ten,ges_div_ten,ges_agv_ten
  use guess_grids, only: ges_z_lon,ges_z_lat
  use guess_grids, only: ntguessfc
  use guess_grids, only: ges_qlon    ,ges_qlat
  use guess_grids, only: ges_tvlon   ,ges_tvlat
  use guess_grids, only: ges_prslavg,ges_psfcavg
  use gridmod, only: lat2,lon2,nsig,gmao_intfc,nsig1o,aeta2_ll
  use gridmod, only: jstart,jlon1,wrf_mass_regional,regional,wrf_nmm_regional
  use gridmod, only: istart,ilat1,twodvar_regional,sigsum,bk5,eta2_ll
  use gridmod, only: nlon,nlat,rlats,wrf_nmm_regional,wrf_mass_regional
  use constants, only: zero,one,cp_mass,rd,one_tenth
  implicit none

! Declare local parameters
  real(r_kind),parameter:: r015 = 0.15_r_kind



! Declare passed variables
  integer(i_kind),intent(in):: mype

! Declare local variables
  logical ice
  integer(i_kind) i,j,k,it,nnn,k150,kpres,n,np
  
  real(r_kind) drh,rd_over_cp_mass_core,d,dl1,dl2,psfc015
  real(r_kind),dimension(5):: stat,stat1
  real(r_kind),allocatable,dimension(:,:,:):: dlnesdtv,dmax


  type(ggGradient) :: grad

!-----------------------------------------------------------------------------------
! Compute derivatives for .not. twodvar_regional case
  if (.not. twodvar_regional)then

     if (switch_on_derivatives) then

!       Instead, update gradients of all guess fields.  these will
!       be used for forward models that need gradient of background field,
!       and for getting time derivatives of prognostic variables for
!       time extrapolation and non-linear balance constraints.

        nnn=0
        do k=1,nsig1o
           if (levs_id(k)/=0) nnn=nnn+1
        end do
        call get_derivatives(ges_u,ges_v,ges_tv,ges_ps,ges_q,&
             ges_oz,sfct,ges_cwmr, &
             ges_u_lon,ges_v_lon,ges_tvlon,ges_ps_lon,ges_qlon,&
             ges_ozlon,sfct_lon,ges_cwmr_lon, &
             ges_u_lat,ges_v_lat,ges_tvlat,ges_ps_lat,ges_qlat,&
             ges_ozlat,sfct_lat,ges_cwmr_lat, &
             nnn,mype,nfldsig)
        if(.not. wrf_mass_regional)then
           call get_zderivs(ges_z,ges_z_lon,ges_z_lat,mype,nfldsig)

! now that we have derivs, get time tendencies if necessary
          if (tendsflag) then
            it=ntguessig
            call calctends(ges_u(1,1,1,it),ges_v(1,1,1,it),ges_tv(1,1,1,it), &
               ges_ps(1,1,it),ges_q(1,1,1,it),ges_oz(1,1,1,it),ges_cwmr(1,1,1,it),&
               ges_u_lon(1,1,1,it),ges_u_lat(1,1,1,it),ges_v_lon(1,1,1,it),&
               ges_v_lat(1,1,1,it),ges_tvlon(1,1,1,it),ges_tvlat(1,1,1,it),ges_ps_lon(1,1,it), &
               ges_ps_lat(1,1,it),ges_qlon(1,1,1,it),ges_qlat(1,1,1,it),ges_ozlon(1,1,1,it),&
               ges_ozlat(1,1,1,it),ges_cwmr_lon(1,1,1,it),ges_cwmr_lat(1,1,1,it),ges_z_lon(1,1,it),&
               ges_z_lat(1,1,it),jcdivt,mype,ges_u_ten,ges_v_ten,ges_tv_ten,ges_prs_ten,ges_q_ten,&
               ges_oz_ten,ges_cwmr_ten,ges_div_ten,ges_agv_ten)
          end if
        end if
     end if
  endif


! The 3d pressure and geopotential grids are initially loaded at
! the end of the call to read_guess.  Thus, we don't need to call 
! load_prsges and load_geop_hgt on the first outer loop.  We need 
! to update these 3d pressure arrays on all subsequent outer loops.
! Hence, the conditional call to load_prsges and load_geop_hgt

  if (jiter>jiterstart) then
     call load_prsges
     call load_geop_hgt
     if (sfcmod_gfs .or. sfcmod_mm5) then
        if (mype==0) write(6,*)'COMPUTE_DERIVED:  call load_fact10'
        call load_fact10
     endif
  endif


! Compute tropopause level (in pressure, hPa).  The 'pvoz'
! string means compute tropopause using potential vorticity
! and ozone. The 'temp' string means compute tropopause 
! using WMO temperature lapse rate method.

! NOTE:  tropopause pressure is not needed for 2dvar option

  if (.not.twodvar_regional) then
     if(regional)then
        call tpause(mype,'temp')
     else
        call tpause(mype,'pvoz')
     end if

!    *** NOTE ***
!     The tropopause pressures are used to deflate the
!     moisture sensitivity vectors for satellite radiance
!     data and for IR quality control;
!     here we are setting bounds on the tropopause
!     pressure to make sure we are deflating at the very
!     minimum above 150 mb, and nowhere below 350 mb

     do j=1,lon2
        do i=1,lat2
           tropprs(i,j)=max(150.0_r_kind,min(350.0_r_kind,tropprs(i,j)))
        end do
     end do

  endif

! Load guess q for use in limq.  Initialize saturation array to guess.
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           qgues(i,j,k)=ges_q(i,j,k,ntguessig) ! q guess
           qsatg(i,j,k)=max(zero,ges_q(i,j,k,ntguessig)) ! q guess
        end do
     end do
  end do


! Compute saturation specific humidity.  Set up normalization factor 
! for limq routines (1/qs*2)
  ice=.true.
  allocate(dlnesdtv(lat2,lon2,nsig),dmax(lat2,lon2,nsig))
  call genqsat(qsatg,ice,ntguessig,dlnesdtv,dmax)

  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           qsinv2(i,j,k)=one/(qsatg(i,j,k)*qsatg(i,j,k))
        end do
     end do
  end do


! Load arrays based on option for moisture background error

! qoption 1:  use psuedo-RH
  if(qoption==1)then

!    On first outer iteration only, load array used for pseudo-RH
!    moisture analysis variable.  
     if (jiter==jiterstart) then
        do k=1,nsig
           do j=1,lon2
              do i=1,lat2
                 dqdrh(i,j,k)= qsatg(i,j,k)
                 dqdt(i,j,k) = zero
                 dqdp(i,j,k) = zero
              end do
           end do
        end do
     endif

! qoption 2:  use normalized RH
  else

! Load sigsum array, which is used to estimate 3d pressure
! increment about the guess solution (layer average)
     do k=1,nsig
       if (wrf_nmm_regional) then
         sigsum(k)=eta2_ll(k)+eta2_ll(k+1)
       else if (.not.regional) then
         sigsum(k)=bk5(k)+bk5(k+1)
       else
         write(6,*) 'NORMAL_RH_TO_Q: ERROR, NOT SETUP TO RUN QOPT2 WITH THIS GRID CONFIG'
         sigsum(k)=zero
       end if
     end do

!    dqdrh used as work array for guess RH
!    for change of q variable, dqdt=d(ln(es))/d(tv) * q, dqdp=qgues
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
             d=20.0_r_kind*qgues(i,j,k)/qsatg(i,j,k) + one
             n=int(d)
             np=n+1
             dl2=d-float(n)
             dl1=one-dl2
             n=min0(max(1,n),25)
             np=min0(max(1,np),25)
             dqdrh(i,j,k)=qsatg(i,j,k)*(varq(n,k)*dl1 + varq(np,k)*dl2 )
             dqdt(i,j,k)=dmax(i,j,k)*dlnesdtv(i,j,k)*qgues(i,j,k)
             dqdp(i,j,k)=dmax(i,j,k)*qgues(i,j,k)/ges_prsl(i,j,k,ntguessig)
           end do
        end do
     end do

!    Special block to decouple temperature and pressure from moisture
!    above specified levels.  For mass core decouple T and p above 
!    same level (approximately 150 hPa).  For nmm core decouple T
!    above ~150 hPa and p above level where aeta2_ll goes to zero

     if (regional) then

!       Determine k index of approximate 150 hPa layer
        psfc015=r015*ges_psfcavg
        k150 = nsig
        do k=1,nsig
           if (ges_prslavg(k)<psfc015) then
              k150 = k
              exit
           endif
        end do


!       For mass core, decouple T and p above 150 hPa
        if (wrf_mass_regional) then
           do k=k150,nsig
              do j=1,lon2
                 do i=1,lat2
                    dqdt(i,j,k)=zero
                    dqdp(i,j,k)=zero
                 end do
              end do
           end do

!       Decouple T and p at different levels for nmm core
        elseif (wrf_nmm_regional) then
           kpres = nsig
           do k=1,nsig
              if (aeta2_ll(k)==zero) then
                 kpres = k
                 exit
              endif
           end do
           do k=k150,nsig
              do j=1,lon2
                 do i=1,lat2
                    dqdt(i,j,k)=zero
                 end do
              end do
           end do
           do k=kpres,nsig
              do j=1,lon2
                 do i=1,lat2
                    dqdp(i,j,k)=zero
                 end do
              end do
           end do
        endif

!    End of regional block

     else                      !  for global 
       do k=1,nsig
         do j=1,lon2
           do i=1,lat2
!            Decouple Q from T above the tropopause for global
             if ( (ges_prsl(i,j,k,ntguessig)) < (one_tenth*tropprs(i,j)) ) dqdt(i,j,k)=zero
           end do
         end do
       end do
 
     endif

! End of qoption block
  endif
  
  deallocate(dlnesdtv,dmax)

! Generate and output RH stats
  stat1=zero
  stat=zero
  stat(5)=nsig*(lat2-2)*(lon2-2)
  do k=1,nsig
     do j=2,lon2-1
        do i=2,lat2-1
           drh=qgues(i,j,k)/qsatg(i,j,k)
           if(drh > one)then
              stat(1)=stat(1)+one
              stat(2)=stat(2)+(drh - one)**2
           elseif(drh < zero)then
              stat(3)=stat(3)+one
              stat(4)=stat(4)+drh**2
           endif
        end do
     end do
  end do
  call mpi_allreduce(stat,stat1,5,mpi_rtype,&
       mpi_sum,mpi_comm_world,ierror)
  if(stat1(1) > zero)   stat1(2)=sqrt(stat1(2)/stat1(1))
  if(stat1(3) > zero)   stat1(4)=sqrt(stat1(4)/stat1(3))
  if(mype==0)write(6,*)'COMPUTE_DERIVED:  stats of RH=',&
       nint(stat1(1)),stat1(2),nint(stat1(3)),stat1(4),nint(stat1(5))
  
  
! End of routine
  return
end subroutine compute_derived
