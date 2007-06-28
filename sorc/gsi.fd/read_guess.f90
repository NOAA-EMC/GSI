subroutine read_guess(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_guess          read/compute various guess fields
!   prgmmr: parrish          org: np22                date: 1994-02-11
!
! abstract:  This routine performs various functions, all related in one
!            way or the other to the model guess.  Note that this routine
!            is for the global mode of the gsi.  Separate read_guess type
!            routines exist for the regional gsi.
!
!            Functions performed in this routine include the following
!              a) read atmospheric guess bias correction fields (optional)
!              b) read atmospheric guess fields (optionally update with bias correction)
!              c) read surface guess fields
!              d) compute average ozone at each level                           
!
!
! program history log:
!   1994-02-11  parrish
!   1998-04-03  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-16  treadon - update documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-12-15  treadon - remove variable mype from call load_geop_hgt
!   2005-01-27  treadon - replace inguesfc with rdgesfc
!   2005-02-23  wu - setup for qoption=2 and output stats of RH
!   2005-03-07  dee - support gmao model interface
!   2005-03-30  treadon - reformat code (cosmetic changes only)
!   2005-05-27  parrish - add call get_derivatives
!   2005-06-27  guo     - support of GMAO gridded fields
!   2005-07-28  guo     - added sfc component for GMAO grided fields
!   2005-09-29  kleist  - get derivatives of surface terrain for Jc term
!   2005-11-21  kleist  - expand calls to new genqsat and calctends
!   2005-11-21  derber  - modify qoption =1 to work consistently with =2
!   2005-11-29  derber - remove external iteration dependent calculations
!   2005-11-29  derber - add ozmz calculation                             
!   2005-12-09  guo     - remove GMAO derivative computation code.  Use
!                         unified NCEP compact_diff procedures.
!   2006-01-10  treadon - consolidate all read*guess calls into this routine
!   2006-02-02  treadon - load 3d pressure guess pressure and geopotential 
!                         height grids
!   2006-02-03  derber  - modify to increase reproducibility (ozmz)
!   2006-03-13  treadon - increase filename to 24 characters
!   2006-04-14  treadon - replace call read_gfsatm for bias with read_bias
!   2006-07-28  derber  - include sensible temperature
!   2006-07-31  kleist  - use ges_ps instead of lnps
!   2006-09-28  treadon - add sfc_rough and load_fact10
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
  use m_die,only : die

  use kinds, only: r_kind,r_single,i_kind
  use jfunc, only: biascor
  use mpimod, only: ierror,mpi_rtype,mpi_sum,mpi_comm_world,levs_id,npe
  use guess_grids, only: ges_z,ges_ps,ges_u,ges_v,ges_vor,ges_div,&
       ges_tv,ges_tsen,ges_q,ges_oz,ges_cwmr,sfct,fact10,veg_type,sno,&
       ifilesfc,nfldsfc,isli_g,isli,soil_type,sfc_rough,&
       veg_frac,soil_moi,soil_temp,ntguessig,ifilesig,&
       nfldsig,bias_q,bias_tv,bias_cwmr,bias_oz,bias_v,bias_ps,&
       bias_vor,bias_u,bias_div,ges_prsi,load_prsges,load_geop_hgt,&
       bias_tskin,bias_u,bias_v,load_fact10,sfcmod_gfs,sfcmod_mm5
  use gsi_io, only: read_bias
  use guess_grids, only: ozmz
  use guess_grids, only: ntguessfc
  use gridmod, only: lat2,lon2,gmao_intfc,nsig1o
  use gridmod, only: jstart,jlon1
  use gridmod, only: istart,ilat1
  use gridmod, only: nsig,lat1,lon1
  use gridmod, only: wrf_mass_regional,wrf_nmm_regional,&
       twodvar_regional,netcdf,regional
  use gridmod, only: aeta1_ll,aeta2_ll,pdtop_ll,pt_ll

  use constants, only: izero,zero,one,rozcon,fv
  use ncepgfs_io, only: read_gfsatm,read_gfssfc

  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype

! Declare local variables
  character(24) filename
  integer(i_kind) i,j,k,it,iret,iret_bias,mm1,n

  real(r_kind):: wspd,asum,bsum,sum
  real(r_kind),dimension(lat2,lon2):: work,work_lnps
  real(r_kind),dimension(nsig+1,npe):: work_oz,work_oz1
  real(r_kind),allocatable,dimension(:,:) :: dummysfc

!-----------------------------------------------------------------------------------
! Certain functions are only done once --> on the first outer iteration. 
! One-time functions include
!    a) read atmospheric guess fields (optionally add bias correction)
!    b) read surface guess fields
!

! Handle .not. GMAO interfaces
  if(.not.gmao_intfc) then

!    Handle regional interfaces
     if (regional)then
        if (wrf_nmm_regional) then
           if(netcdf) then
              call read_wrf_nmm_netcdf_guess(mype)
           else
              call read_wrf_nmm_binary_guess(mype)
           end if
        else if (wrf_mass_regional) then
           if(netcdf) then
              call read_wrf_mass_netcdf_guess(mype)
           else
              call read_wrf_mass_binary_guess(mype)
           end if
        else if(twodvar_regional) then
           call read_2d_guess(mype)
        end if
     

!    Otherwise, handle global interface (ie, NCEP GFS)
     else

!       If requested, read bias correction fields
        iret_bias=izero
        if (biascor > zero) then
           filename='biascor_in'
           call read_bias(filename,mype,work,bias_ps,bias_tskin,&
                bias_vor,bias_div,bias_u,bias_v,bias_tv,bias_q,&
                bias_cwmr,bias_oz,iret_bias)
        endif
        
!       Read atmospheric fields
        do it=1,nfldsig
           write(filename,100) ifilesig(it)
100        format('sigf',i2.2)
           call read_gfsatm(filename,mype,&
                ges_z(1,1,it),ges_ps(1,1,it),&
                ges_vor(1,1,1,it),ges_div(1,1,1,it),&
                ges_u(1,1,1,it),ges_v(1,1,1,it),&
                ges_tv(1,1,1,it),ges_q(1,1,1,it),&
                ges_cwmr(1,1,1,it),ges_oz(1,1,1,it),iret)


!          If requested and read in and add bias correction to guess fields
           if (biascor > zero .and. iret_bias==izero) then
              if (mype==0) write(6,*)'READ_GUESS:   add bias correction to guess field ',&
                   filename
              do j=1,lon2
                 do i=1,lat2
                    ges_ps(i,j,it)= ges_ps(i,j,it) + bias_ps(i,j)
                 end do
              end do
              do k=1,nsig
                 do j=1,lon2
                    do i=1,lat2
                       ges_u(i,j,k,it)   = ges_u(i,j,k,it) + bias_u(i,j,k)
                       ges_v(i,j,k,it)   = ges_v(i,j,k,it) + bias_v(i,j,k)
                       ges_vor(i,j,k,it) = ges_vor(i,j,k,it) + bias_vor(i,j,k)
                       ges_div(i,j,k,it) = ges_div(i,j,k,it) + bias_div(i,j,k)
                       ges_cwmr(i,j,k,it)= ges_cwmr(i,j,k,it) + bias_cwmr(i,j,k)
                       ges_q(i,j,k,it)   = ges_q(i,j,k,it) + bias_q(i,j,k)
                       ges_oz(i,j,k,it)  = ges_oz(i,j,k,it) + bias_oz(i,j,k)
                       ges_tv(i,j,k,it)  = ges_tv(i,j,k,it) + bias_tv(i,j,k)
                    end do
                 end do
              end do
           endif
           

!  Get sensible temperature

           do k=1,nsig
              do j=1,lon2
                 do i=1,lat2
                    ges_tsen(i,j,k,it)= ges_tv(i,j,k,it)/(one+fv*max(zero,ges_q(i,j,k,it)))
                 end do
              end do
           end do

!       End of loop over atmospheric guess files
        end do
        
!       Read surface fields
        do it=1,nfldsfc
           write(filename,200)ifilesfc(it)
200        format('sfcf',i2.2)
           call read_gfssfc(filename,mype,&
                fact10(1,1,it),sfct(1,1,it),sno(1,1,it),veg_type(1,1,it),&
                veg_frac(1,1,it),soil_type(1,1,it),soil_temp(1,1,it),&
                soil_moi(1,1,it),isli(1,1,it),isli_g(1,1,it),sfc_rough(1,1,it))

!          If requested and read in and add bias correction to guess fields
           if (biascor > zero .and. iret_bias==izero) then
              if (mype==0) write(6,*)'READ_GUESS:   add bias correction to guess field ',&
                   filename
              do j=1,lon2
                 do i=1,lat2
                    sfct(i,j,it)= sfct(i,j,it) + bias_tskin(i,j)
                 end do
              end do
           endif

        end do
        

!    End of non-GMAO global interfaces
     endif
        
! Handle GMAO interface        
  else ! if (gmao_intfc) .. read FV state and surface data files
     if(nfldsig/=nfldsfc)	&
          call die('glbsoi','unexpected, nfldsig',nfldsig,	&
          'nfldsfc',nfldsfc)
     if(ntguessig/=ntguessfc)	&
          call die('glbsoi','unexpected, ntguessig',ntguessig,  &
          'ntguessfc',ntguessfc)
     
     do it=1,nfldsig
        call fvAnaGrid_read(it,ifilesig(it),		&
             ges_z   (:,:,  it),work_lnps,          &
             ges_vor (:,:,:,it),ges_div (:,:,:,it),	&
             ges_u   (:,:,:,it),ges_v   (:,:,:,it),	&
             ges_tv  (:,:,:,it),ges_q   (:,:,:,it),	&
             ges_cwmr(:,:,:,it),ges_oz  (:,:,:,it),	&
             sfct    (:,:,  it), MPI_comm_world)
     end do
     ges_ps(:,:,it) = exp(work_lnps(:,:))
     ges_tsen(:,:,:,it) = ges_tv(:,:,:,it)/(one+fv*max(ges_q(:,:,:,it),zero))


     if(any(ges_q < zero)) then
        write(6,*)'READ_GUESS: negative q values removed'
        where (ges_q < zero) ges_q = zero
     endif
        
!    Read surface fields
     allocate(dummysfc(size(sfct,1),size(sfct,2)))

     do it=1,nfldsfc

!       Read surface data at it, from a fv-surface data file
!       and a NCEP-surface data file.
        call fvAnaGrid_surface_read(it,ifilesfc(it),    &
             fact10(:,:,it),dummysfc(:,:),sno(:,:,it),  &
             veg_type(:,:,it),veg_frac(:,:,it),         &
             soil_type(:,:,it),soil_temp(:,:,it),       &
             soil_moi(:,:,it),                          &
             isli(:,:,it),isli_g(:,:,it), MPI_comm_world)

!       At return, fact10(:,:)==sqrt(u10m^2+v10m^2), which could be
!       zero somewhere.  To compute factor at 10m, wind speed at the
!       bottom of the atmosphere is needed, which is level 1 of the
!       GSI grid.  It is debateable what fact10 should be where the
!       denominator is zero.  I don't have any good solution, but
!       simply mimic what has been done before.

        do j=lbound(fact10,2),ubound(fact10,2)
           do i=lbound(fact10,1),ubound(fact10,1)
              wspd=sqrt(ges_u(i,j,1,it)*ges_u(i,j,1,it) + &
                   ges_v(i,j,1,it)*ges_v(i,j,1,it))
              
!                Reset fact10 if fact10 > zero
              if(zero < fact10(i,j,it)) then
                 if(fact10(i,j,it) <= wspd) then
                    fact10(i,j,it)=fact10(i,j,it)/wspd
                 else
                    fact10(i,j,it)=one
                 endif
              endif
           end do
        end do
        
     end do
     
     deallocate(dummysfc)

! End of GMAO interface block
  endif

! Load 3d subdomain pressure arrays from the guess fields
  call load_prsges

! Compute 3d subdomain geopotential heights from the guess fields
  call load_geop_hgt
  
! Compute 10m wind factor
  if (sfcmod_gfs .or. sfcmod_mm5) then
     if (mype==0) write(6,*)'READ_GUESS:  call load_fact10'
     call load_fact10
  endif

! Calculate global means for ozone
! Calculate sums for ozone to estimate variance.
  mm1=mype+1
  work_oz = zero
  do k = 1,nsig
     do j = 2,lon1+1
        do i = 2,lat1+1
           work_oz(k,mm1) = work_oz(k,mm1) + ges_oz(i,j,k,ntguessig)* &
                rozcon*(ges_prsi(i,j,k,ntguessig)-ges_prsi(i,j,k+1,ntguessig))
        end do
     end do
  end do
  work_oz(nsig+1,mm1)=float(lon1*lat1)
  
  call mpi_allreduce(work_oz,work_oz1,(nsig+1)*npe,mpi_rtype,mpi_sum,&
       mpi_comm_world,ierror)
  
  bsum=zero
  do n=1,npe
    bsum=bsum+work_oz1(nsig+1,n)
  end do
  do k=1,nsig
     ozmz(k)=zero
     asum=zero
     do n=1,npe
       asum=asum+work_oz1(k,n)
     end do
     if (bsum>zero) ozmz(k)=asum/bsum
  enddo

  
  return
end subroutine read_guess
