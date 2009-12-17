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
!   2007-03-13  derber  - add changes to make qoption=2 variances work as others
!   2007-05-08  kleist  - remove jcdivt from use list
!   2007-06-21  rancic - add pbl code
!   2007-07-26  cucurull - call gesprs, add ges_3dp and remove ps 
!                          in calctends argument list 
!   2007-08-08  derber - pass ges_teta to calctends rather than calculate seperately
!   2008-06-05  safford - rm unused uses
!   2008-10-10  derber  - add calculation of fact_tv
!   2008-11-03  sato - add anisotropic mode procedures
!   2008-12-08  todling - move 3dprs/geop-hght calculation from here into setuprhsall
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

  use kinds, only: r_kind,i_kind
  use jfunc, only: qsatg,qgues,rhgues,jiter,jiterstart,&
       dqdt,dqdrh,dqdp,qoption,switch_on_derivatives,&
       tendsflag,varq
  use mpimod, only: levs_id
  use guess_grids, only: ges_z,ges_ps,ges_u,ges_v,&
       ges_tv,ges_q,ges_oz,ges_cwmr,sfct,&
       tropprs,ges_prsl,ntguessig,&
       nfldsig,&
       ges_teta,fact_tv, &
       ges_u_lon,ges_v_lon,ges_tvlon,ges_ps_lon,ges_qlon,ges_ozlon,ges_cwmr_lon, &
       ges_u_lat,ges_v_lat,ges_tvlat,ges_ps_lat,ges_qlat,ges_ozlat,ges_cwmr_lat
  use guess_grids, only: ges_u_ten,ges_v_ten,ges_tv_ten,ges_prs_ten,ges_q_ten,&
       ges_oz_ten,ges_cwmr_ten
  use guess_grids, only: ges_prslavg,ges_psfcavg
  use gridmod, only: lat2,lon2,nsig,nnnn1o,aeta2_ll,nsig1o
  use gridmod, only: regional
  use gridmod, only: twodvar_regional
  use gridmod, only: wrf_nmm_regional,wrf_mass_regional,nems_nmmb_regional
  use berror, only: qvar3d,dssv,hswgt
  use balmod, only: rllat1,llmax
  use mod_strong, only: jcstrong,baldiag_full
  use obsmod, only: write_diag

  use constants, only: ione,zero,one,one_tenth,half,fv

! for anisotropic mode
  use sub2fslab_mod, only: setup_sub2fslab, sub2fslab, sub2fslab_glb, destroy_sub2fslab
  use anberror, only: anisotropic, idvar, kvar_start, an_amp0, ngauss, indices, indices_p, &
                    & filter_all,   filter_p2,   filter_p3, &
                    & pf2aP1, pf2aP2, pf2aP3, rtma_subdomain_option
  use anisofilter, only: rh0f, corz, ensamp, mlat, rllatf, fact_qopt2
  use anisofilter_glb, only: rh2f, rh3f, ensamp0f, ensamp2f, ensamp3f, &
                             p0ilatf, p2ilatf, p3ilatf, p2ilatfm, p3ilatfm, get_stat_factk

  implicit none

! Declare local parameters
  real(r_kind),parameter:: r015 = 0.15_r_kind

! Declare passed variables
  integer(i_kind),intent(in):: mype

! Declare local variables
  logical ice,fullfield
  integer(i_kind) i,j,k,it,k150,kpres,n,np,l,l2
  
  real(r_kind) d,dl1,dl2,psfc015,dn1,dn2
  real(r_kind),allocatable,dimension(:,:,:):: dlnesdtv,dmax
  real(r_kind),dimension(lat2,lon2,nsig+ione):: ges_3dp
  real(r_kind),dimension(lat2,lon2,nfldsig):: sfct_lat,sfct_lon

! for anisotropic mode
  integer(i_kind):: k1,ivar,kvar,igauss
  real(r_kind):: factor,factk,hswgtsum

! Limit q to be >= 1.e-10_r_kind
  do it=1,nfldsig
   do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           ges_q(i,j,k,it)=max(ges_q(i,j,k,it),1.e-10_r_kind)
        end do
     end do
   end do
  end do
!-----------------------------------------------------------------------------------
! Compute derivatives for .not. twodvar_regional case
  if (.not. twodvar_regional)then

     if (switch_on_derivatives) then

!       Instead, update gradients of all guess fields.  these will
!       be used for forward models that need gradient of background field,
!       and for getting time derivatives of prognostic variables for
!       time extrapolation and non-linear balance constraints.

        
        it=ntguessig
        call get_derivatives(ges_u(1,1,1,it),ges_v(1,1,1,it), &
             ges_tv(1,1,1,it),ges_ps,ges_q(1,1,1,it),&
             ges_oz(1,1,1,it),sfct(1,1,it),ges_cwmr(1,1,1,it), &
             ges_u_lon,ges_v_lon,ges_tvlon,ges_ps_lon,ges_qlon,&
             ges_ozlon,sfct_lon,ges_cwmr_lon, &
             ges_u_lat,ges_v_lat,ges_tvlat,ges_ps_lat,ges_qlat,&
             ges_ozlat,sfct_lat,ges_cwmr_lat, &
             nnnn1o,nfldsig)

        if(.not. wrf_mass_regional .and. tendsflag)then

! now that we have derivs, get time tendencies if necessary

            call getprs(ges_ps(1,1,it),ges_3dp)

            call calctends(ges_u(1,1,1,it),ges_v(1,1,1,it),ges_tv(1,1,1,it), &
               ges_q(1,1,1,it),ges_oz(1,1,1,it),ges_cwmr(1,1,1,it),&
               ges_teta(1,1,1,it),ges_z(1,1,it), &
               ges_u_lon,ges_u_lat,ges_v_lon,&
               ges_v_lat,ges_tvlon,ges_tvlat,ges_ps_lon(1,1,it), &
               ges_ps_lat(1,1,it),ges_qlon,ges_qlat,ges_ozlon,&
               ges_ozlat,ges_cwmr_lon,ges_cwmr_lat,&
               mype,ges_u_ten,ges_v_ten,ges_tv_ten,ges_prs_ten,ges_q_ten,&
               ges_oz_ten,ges_cwmr_ten,ges_3dp)

            if(jcstrong .and. write_diag(jiter) .and. baldiag_full) then
                fullfield=.true.


                call strong_bal_correction(ges_u_ten,ges_v_ten,ges_tv_ten,ges_prs_ten,mype, &
                                           ges_u,ges_v,ges_tv,ges_ps,.true.,fullfield,.false.)
            end if
        end if
     end if

! Compute tropopause level (in pressure, hPa).  The 'pvoz'
! string means compute tropopause using potential vorticity
! and ozone. The 'temp' string means compute tropopause 
! using WMO temperature lapse rate method.

! NOTE:  tropopause pressure is not needed for 2dvar option

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
           qsatg(i,j,k)=ges_q(i,j,k,ntguessig) ! q guess
           fact_tv(i,j,k)=one/(one+fv*qsatg(i,j,k))      ! factor for tv to tsen conversion
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
           rhgues(i,j,k)=qgues(i,j,k)/qsatg(i,j,k)
        end do
     end do
  end do


! Load arrays based on option for moisture background error

! qoption 1:  use psuedo-RH
  if(qoption==ione)then

!    On first outer iteration only, load array used for pseudo-RH
!    moisture analysis variable.  
     if (jiter==jiterstart) then
        do k=1,nsig
           do j=1,lon2
              do i=1,lat2
                 if(regional)then
                    l=int(rllat1(i,j))
                    l2=min0(l+ione,llmax)
                    dl2=rllat1(i,j)-float(l)
                    dl1=one-dl2
                    if(.not.twodvar_regional)then
                       qvar3d(i,j,k)=dl1*dssv(4,l,j,k)+dl2*dssv(4,l2,j,k)
                    endif
                 else
                    qvar3d(i,j,k)=dssv(4,i,j,k)
                 end if
                 dqdrh(i,j,k)= qsatg(i,j,k)
                 dqdt(i,j,k) = zero
                 dqdp(i,j,k) = zero
              end do
           end do
        end do
     endif

! qoption 2:  use normalized RH
  else

!    dqdrh used as work array for guess RH
!    for change of q variable, dqdt=d(ln(es))/d(tv) * q, dqdp=qgues
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
             if (jiter==jiterstart) then
               d=20.0_r_kind*rhgues(i,j,k) + one
               n=int(d)
               np=n+ione
               dn2=d-float(n)
               dn1=one-dn2
               n=min0(max(ione,n),25_i_kind)
               np=min0(max(ione,np),25_i_kind)
               if(regional)then
                 l=int(rllat1(i,j))
                 l2=min0(l+ione,llmax)
                 dl2=rllat1(i,j)-float(l)
                 dl1=one-dl2
                 if(.not.twodvar_regional)then
                    qvar3d(i,j,k)=(varq(n,k)*dn1 + varq(np,k)*dn2)* &
                      (dl1*dssv(4,l,j,k)+dl2*dssv(4,l2,j,k))
                 endif 
               else
                 qvar3d(i,j,k)=(varq(n,k)*dn1 + varq(np,k)*dn2)*dssv(4,i,j,k) 
               end if 
             end if
             dqdrh(i,j,k)=qsatg(i,j,k)
             dqdt(i,j,k)=dmax(i,j,k)*dlnesdtv(i,j,k)*qgues(i,j,k)
             dqdp(i,j,k)=dmax(i,j,k)*half*qgues(i,j,k)/ges_prsl(i,j,k,ntguessig)
           end do
        end do
     end do

! variance update for anisotropic mode
     if( anisotropic .and. .not.rtma_subdomain_option ) then
       hswgtsum=sum(hswgt(1:ngauss))
       call setup_sub2fslab
       if( regional ) then
         allocate(rh0f(pf2aP1%nlatf,pf2aP1%nlonf,nsig1o))
         call sub2fslab(rhgues,rh0f)
         do k=indices%kps,indices%kpe
           ivar=idvar(k)
           if(ivar==5_i_kind) then
             kvar=k-kvar_start(ivar)+ione
             do k1=1,nsig1o
               if(levs_id(k1)==kvar) exit
             end do
             do j=indices%jps,indices%jpe
             do i=indices%ips,indices%ipe
               l =max(min(int(rllatf(i,j)),mlat),ione)
               l2=min((l+ione),mlat)
               dl2=rllatf(i,j)-float(l)
               dl1=one-dl2

               factk=dl1*corz(l,kvar,4)+dl2*corz(l2,kvar,4)
               call fact_qopt2(factk,rh0f(i,j,k1),kvar)

               do igauss=1,ngauss
                 factor=hswgt(igauss)*factk*an_amp0(ivar)/sqrt(hswgtsum)
                 filter_all(1)%amp(igauss,i,j,k)=factor*filter_all(2)%amp(igauss,i,j,k)
                 if (allocated(ensamp)) then
                   filter_all(1)%amp(igauss,i,j,k)=filter_all(1)%amp(igauss,i,j,k)*ensamp(i,j,k1)
                 end if
               end do
             end do
             end do
           end if
         end do
         deallocate(rh0f)
       else
         allocate(rh0f(pf2aP1%nlatf,pf2aP1%nlonf,nsig1o))
         allocate(rh2f(pf2aP2%nlatf,pf2aP2%nlonf,nsig1o))
         allocate(rh3f(pf2aP3%nlatf,pf2aP3%nlonf,nsig1o))

         call sub2fslab_glb (rhgues,rh0f,rh2f,rh3f)
         do k=indices%kps,indices%kpe
           ivar=idvar(k)
           if(ivar==5_i_kind) then
             kvar=k-kvar_start(ivar)+ione
             do k1=1,nsig1o
               if(levs_id(k1)==kvar) exit
             end do
             ! zonal patch
             do j=indices%jps,indices%jpe
             do i=indices%ips,indices%ipe
               call get_stat_factk(p0ilatf(i),ivar,kvar,factk, &
                                   rh0f(i,j,k1),one)
               do igauss=1,ngauss
                 factor=hswgt(igauss)*factk*an_amp0(ivar)/sqrt(hswgtsum)
                 filter_all(1)%amp(igauss,i,j,k)=factor*filter_all(2)%amp(igauss,i,j,k)
                 if (allocated(ensamp0f)) then
                   filter_all(1)%amp(igauss,i,j,k)=filter_all(1)%amp(igauss,i,j,k)*ensamp0f(i,j,k1)
                 end if
               end do

             end do
             end do
             ! polar patches
             do j=indices_p%jps,indices_p%jpe
             do i=indices_p%ips,indices_p%ipe
               ! north polar
               if(p2ilatf(i,j)/=zero) then
                 call get_stat_factk(p2ilatf(i,j),ivar,kvar,factk, &
                                     rh2f(i,j,k1),one)
               else
                 call get_stat_factk(p2ilatfm    ,ivar,kvar,factk, &
                                     rh2f(i,j,k1),one)
               end if
               do igauss=1,ngauss
                 factor=hswgt(igauss)*factk*an_amp0(ivar)/sqrt(hswgtsum)
                 filter_p2(1)%amp(igauss,i,j,k)=factor*filter_p2(2)%amp(igauss,i,j,k)
                 if(allocated(ensamp2f)) then
                   filter_p2(1)%amp(igauss,i,j,k)=filter_p2(1)%amp(igauss,i,j,k)*sqrt(ensamp2f(i,j,k))
                 end if
               end do
               ! south polar
               if(p3ilatf(i,j)/=zero) then
                 call get_stat_factk(p3ilatf(i,j),ivar,kvar,factk, &
                                     rh3f(i,j,k1),one)
               else
                 call get_stat_factk(p3ilatfm    ,ivar,kvar,factk, &
                                     rh3f(i,j,k1),one)
               end if
               do igauss=1,ngauss
                 factor=factk*an_amp0(ivar)/sqrt(real(ngauss,r_kind))
                 filter_p3(1)%amp(igauss,i,j,k)=factor*filter_p3(2)%amp(igauss,i,j,k)
                 if(allocated(ensamp3f)) then
                   filter_p3(1)%amp(igauss,i,j,k)=filter_p3(1)%amp(igauss,i,j,k)*sqrt(ensamp3f(i,j,k))
                 end if
               end do

             end do
             end do

           end if
         end do
         deallocate(rh0f,rh2f,rh3f)
       end if
       call destroy_sub2fslab
     end if

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
        elseif (wrf_nmm_regional.or.nems_nmmb_regional) then
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
             if ( (ges_prsl(i,j,k,ntguessig)) < (one_tenth*tropprs(i,j)) ) then
               dqdt(i,j,k)=zero
               dqdp(i,j,k)=zero
             end if
           end do
         end do
       end do
 
     endif

! End of qoption block
  endif
  
  deallocate(dlnesdtv,dmax)

  call q_diag(mype)
  
! End of routine
  return
end subroutine compute_derived
