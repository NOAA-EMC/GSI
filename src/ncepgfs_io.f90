module ncepgfs_io
!$$$ module documentation block
!           .      .    .                                       .
! module:   ncepgfs_io
!   prgmmr: treadon     org: np23                date: 2006-01-10
!
! abstract: This module contains routines which handle input/output
!           operations for NCEP GFS atmospheric and surface files.
!
! program history log:
!   2006-01-10 treadon
!
! Subroutines Included:
!   sub read_gfsatm       - read ncep gfs atmospheric ("sigma") file, scatter
!                           on grid to analysis subdomains
!   sub read_gfssfc       - read ncep gfs surface file, scatter on grid to 
!                           analysis subdomains
!   sub sfc_interpolate   - interpolate from gfs atm grid to gfs sfc grid
!   sub write_gfs         - driver to write ncep gfs atmospheric and surface
!                           analysis files
!   sub write_gfsatm      - gather on grid, transform to spectral, write ncep
!                           gfs atmospheric analysis file
!   sub write_gfssfc      - gather/write on grid ncep surface analysis file
!
! Variable Definitions:
!   none
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

  private
  public read_gfsatm
  public read_gfssfc
  public write_gfs
  public write_gfsatm
  public write_gfssfc
  public sfc_interpolate

contains

  subroutine read_gfsatm(filename,mype,g_z,g_ps,g_vor,g_div,g_u,g_v,&
       g_tv,g_q,g_cwmr,g_oz,iret_read)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_gfsatm         read gfs atm, convert to grid and
!                                    send to all mpi tasks
!   prgmmr: parrish          org: np22                date: 1990-10-10
!
! abstract: read ncep gfs atmospheric guess, convert to grid, and 
!           scatter to subdomains
!
! program history log:
!   1990-10-10  parrish
!   1997-09-23  weiyu yang
!   1998-05-15  weiyu yang       mpp version
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-05-18  kleist, documentation
!   2004-05-15  treadon - transform spectral coef to grid, 
!                         communicate grids to all tasks
!   2004-06-17  treadon - update documentation
!   2004-08-03  treadon - add only to module use, add intent in/out
!   2004-08-23  treadon - declare tracers,vtid,pdryini,xncld as real(single)
!   2004-08-27  treadon - use splib routines for grid <---> spectral transforms
!   2005-03-07  dee     - support gmao model interface
!   2005-03-30  treadon - clean up formatting of write statement
!   2005-12-09  guo     - removed special GMAO spectral input format
!   2006-01-09  treadon - use sigio
!   2006-03-13  treadon - increase filename to 24 characters
!   2006-09-18  treadon - replace lnps with ps
!   2007-05-07  treadon - add gfsio 
!   2007-05-08  kleist - add option for lnps or ps
!   2008-05-28  safford - rm unused vars
!
!   input argument list:
!     inges    - unit number of guess coefs
!     mype     - mpi task id
!
!   output argument list:
!     hourg    - guess forecast hour
!     idateg   - initial date of guess
!     g_*      - guess fields
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: r_kind,r_single,i_kind
    use gridmod, only: displs_s,irc_s,ijn_s,&
         ird_s,nsig,nlat,nlon,lat2,lon2,&
         itotsub,fill_ns,filluv_ns,ncep_sigio,ncepgfs_head,idpsfc5,idthrm5,&
         ntracer,idvc5,cp5,idvm5
    use specmod, only: factsml_b,factvml_b,jcap_b,nc_b
    use mpimod, only: npe,mpi_comm_world,ierror,mpi_rtype,reload
    use constants, only: izero,ione,zero,one,fv
    use sigio_module, only: sigio_intkind,sigio_head,sigio_data,&
         sigio_srohdc,sigio_axdata
    use gfsio_module, only: gfsio_gfile,gfsio_open,gfsio_close,&
       gfsio_init,gfsio_getfilehead,gfsio_readrecv

    implicit none
    
!   Declare local parameters
    integer(sigio_intkind):: lunges = 11
    real(r_kind),parameter:: r0_01  = 0.01_r_kind
    real(r_kind),parameter:: r0_001 = 0.001_r_kind
    real(r_kind),parameter:: qsmall = 1.e-11_r_kind

!   Declare passed variables
    character(24),intent(in):: filename
    integer(i_kind),intent(in):: mype
    integer(i_kind),intent(out):: iret_read
    real(r_kind),dimension(lat2,lon2),intent(out):: g_z,g_ps
    real(r_kind),dimension(lat2,lon2,nsig),intent(out):: g_u,g_v,&
         g_vor,g_div,g_cwmr,g_q,g_oz,g_tv
    
!   Declare local variables
    integer(i_kind):: iret,nlatm2,ij,n,ii1,l,m
    integer(i_kind) i,j,k,icount,icount_prev,mm1
    integer(i_kind) mype_hs,mype_ps
    real(r_kind),dimension(nlon,nlat-2_i_kind):: grid,grid_u,grid_v,&
         grid_vor,grid_div,grid2,grid3
    real(r_kind),dimension(nlon,nlat-2_i_kind,ntracer):: grid_q
    real(r_kind),dimension(nc_b):: spec_work,spec_vor,spec_div
    real(r_kind),dimension(itotsub):: work,work_vor,work_div,&
         work_u,work_v
    real(r_kind),dimension(lat2*lon2,max(2*nsig,npe)):: sub,sub_div,sub_vor,&
         sub_u,sub_v
    real(r_kind),dimension(nlat-2_i_kind):: qlatmean
    real(r_kind),dimension(2):: qminlev,qmaxlev
    real(r_kind):: rnlon,fact
    
    type(sigio_head):: sighead
    type(sigio_data):: sigdata
    type(gfsio_gfile) :: gfile
    type(ncepgfs_head):: gfshead


    type:: gfsio_data
       real(r_single),allocatable:: hs(:)      !surface height, m
       real(r_single),allocatable:: ps(:)      !surface pressure, pa
       real(r_single),allocatable:: t(:,:)     !layer temperature, k
       real(r_single),allocatable:: u(:,:)     !layer zonal wind, m/s
       real(r_single),allocatable:: v(:,:)     !layer meridional wind, m/s
       real(r_single),allocatable:: q(:,:,:)   !tracers, 1-spfh; 2-O3; 3-CLW , kg/kg
    end type gfsio_data
    type(gfsio_data):: gfsdata

!******************************************************************************  
!   Initialize variables used below
    mm1=mype+ione
    mype_hs=izero
    mype_ps=npe-ione
    iret_read=izero
    nlatm2=nlat-2_i_kind
    rnlon=one/float(nlon)


!   Read NCEP gfs guess file using appropriate io module
    if (ncep_sigio) then
       call sigio_srohdc(lunges,filename,sighead,sigdata,iret)
       gfshead%fhour   = sighead%fhour
       gfshead%idate   = sighead%idate
       gfshead%levs    = sighead%levs
       gfshead%ntrac   = sighead%ntrac
       gfshead%ncldt   = sighead%ncldt
       gfshead%lonb    = nlon
       gfshead%latb    = nlatm2


    else

       call gfsio_init(iret)
       call gfsio_open(gfile,filename,'read',iret)
       call gfsio_getfilehead(gfile,iret=iret, &
            fhour=gfshead%fhour, &
            idate=gfshead%idate, &
            levs=gfshead%levs, &
            ntrac=gfshead%ntrac, &
            ncldt=gfshead%ncldt, &
            lonb=gfshead%lonb, &
            latb=gfshead%latb)

       allocate(gfsdata%hs(gfshead%latb*gfshead%lonb))
       allocate(gfsdata%ps(gfshead%latb*gfshead%lonb))
       allocate(gfsdata%t(gfshead%latb*gfshead%lonb,gfshead%levs))
       allocate(gfsdata%u(gfshead%latb*gfshead%lonb,gfshead%levs))
       allocate(gfsdata%v(gfshead%latb*gfshead%lonb,gfshead%levs))
       allocate(gfsdata%q(gfshead%latb*gfshead%lonb,gfshead%levs,gfshead%ntrac))
       
       call gfsio_readrecv(gfile,'hgt','sfc',ione,gfsdata%hs,iret)
       iret_read=iret_read+iret

       call gfsio_readrecv(gfile,'pres','sfc',ione,gfsdata%ps,iret)
       iret_read=iret_read+iret

       do k=1,gfshead%levs
          call gfsio_readrecv(gfile,'tmp','layer',k,gfsdata%t(:,k),iret)
          iret_read=iret_read+iret
       enddo

       do k=1,gfshead%levs
          call gfsio_readrecv(gfile,'ugrd','layer',k,gfsdata%u(:,k),iret)
          iret_read=iret_read+iret
       enddo

       do k=1,gfshead%levs
          call gfsio_readrecv(gfile,'vgrd','layer',k,gfsdata%v(:,k),iret)
          iret_read=iret_read+iret
       enddo

       do k=1,gfshead%levs
          call gfsio_readrecv(gfile,'spfh','layer',k,gfsdata%q(:,k,1),iret)
          iret_read=iret_read+iret
       enddo

       do k=1,gfshead%levs
          call gfsio_readrecv(gfile,'o3mr','layer',k,gfsdata%q(:,k,2),iret)
          iret_read=iret_read+iret
       enddo

       do k=1,gfshead%levs
          call gfsio_readrecv(gfile,'clwmr','layer',k,gfsdata%q(:,k,3),iret)
          iret_read=iret_read+iret
       enddo

       call gfsio_close(gfile,iret)
       iret_read=iret_read+iret
    endif

    if (iret_read /= izero) goto 1000


!   Process guess fields according to type of input file.   NCEP_SIGIO files
!   are spectral coefficient files and need to be transformed to the grid.
!   NCEP_GFSIO files are grid files.    Both formats need to be scattered 
!   from the full domain to sub-domains.

!   Terrain:  spectral --> grid transform, scatter to all mpi tasks
    if (mype==mype_hs) then
       if (ncep_sigio) then
          do i=1,nc_b
             spec_work(i)=sigdata%hs(i)
             if(factsml_b(i))spec_work(i)=zero
          end do
          call sptez_s_bkg(spec_work,grid,ione)
       else
          ij=izero
          do j=1,gfshead%latb
             do i=1,gfshead%lonb
                ij=ij+ione
              grid(i,j)=gfsdata%hs(ij)
             end do
          end do
       endif
       call fill_ns(grid,work)
    endif
    call mpi_scatterv(work,ijn_s,displs_s,mpi_rtype,&
         g_z,ijn_s(mm1),mpi_rtype,mype_hs,mpi_comm_world,ierror)


!   Surface pressure:  same procedure as terrain, but handled by task mype_ps
!   NCEP SIGIO has two options for surface pressure.  Variable idpsfc5 
!   indicates the type:   
!      idpsfc5= 0,1 for ln(psfc)
!      idpsfc5= 2 for psfc
!   
    if (mype==mype_ps) then
       if (ncep_sigio) then
          do i=1,nc_b
             spec_work(i)=sigdata%ps(i)
             if(factsml_b(i))spec_work(i)=zero
          end do
          call sptez_s_bkg(spec_work,grid,ione)
          call fill_ns(grid,work)

!         If ln(ps), take exponential to convert to ps in cb
          if (idpsfc5 /= 2_i_kind) then
             do i=1,itotsub
                work(i)=exp(work(i))
             end do
          endif

       else
          ij=izero
          do j=1,gfshead%latb
             do i=1,gfshead%lonb
                ij=ij+ione
                grid(i,j) = r0_001*gfsdata%ps(ij)  ! convert Pa to cb
             end do
          end do
          call fill_ns(grid,work)
       endif
    endif
    call mpi_scatterv(work,ijn_s,displs_s,mpi_rtype,&
         g_ps,ijn_s(mm1),mpi_rtype,mype_ps,mpi_comm_world,ierror)
    
    
!   Thermodynamic variable:  s-->g transform, communicate to all tasks
!   For multilevel fields, each task handles a given level.  Periodic
!   mpi_alltoallv calls communicate the grids to all mpi tasks.  
!   Finally, the grids are loaded into guess arrays used later in the 
!   code.
    sub=zero
    icount=izero
    icount_prev=ione
    do k=1,gfshead%levs
       icount=icount+ione
       if (mype==mod(icount-ione,npe)) then
          if (ncep_sigio) then
             do i=1,nc_b
                spec_work(i)=sigdata%t(i,k)
                if(factsml_b(i))spec_work(i)=zero
             end do
             call sptez_s_bkg(spec_work,grid,ione)

!            SIGIO has three possible thermodynamic variables
!            Variable idthrm5 indicates the type
!               idthrm5 = 0,1 = virtual temperature (Tv)
!               idthrm5 = 2   = sensible (dry) temperature (T)
!               idthrm5 = 3   = enthalpy (h=CpT)
!            The GSI analysis variable is Tv

!            If needed, convert T or h to Tv

             if (idthrm5==2_i_kind .or. idthrm5==3_i_kind) then

!               Convert tracers from spectral coefficients to grid
                do n=1,ntracer
                   do i=1,nc_b
                      spec_work(i)=sigdata%q(i,k,n)
                      if(factsml_b(i))spec_work(i)=zero
                   end do
                   call sptez_s_bkg(spec_work,grid_q(1,1,n),ione)
                end do

!               Convert input thermodynamic variable to dry temperature
                call sigio_cnvtdv8(nlon*nlatm2,nlon*nlatm2,ione,idvc5,&
                     idvm5,ntracer,iret,grid,grid_q,cp5,ione)

!               Convert dry temperature to virtual
                do j=1,nlatm2
                   do i=1,nlon
                      grid(i,j) = grid(i,j)*(one+fv*grid_q(i,j,1))
                   end do
                end do

             endif

          else
!            GFSIO thermodynamic variable is sensible temperature (T).
!            The GSI analysis variable is virtual temperature (Tv).   
!            Convert T to Tv in the loop below.
             ij=izero
             do j=1,gfshead%latb
                do i=1,gfshead%lonb
                   ij=ij+ione
                   grid(i,j)=gfsdata%t(ij,k)*(one+fv*gfsdata%q(ij,k,1))
                end do
             end do
          endif

!         Load values into rows for south and north pole
          call fill_ns(grid,work)
       endif

       if (mod(icount,npe)==izero .or. icount==gfshead%levs) then
          call mpi_alltoallv(work,ijn_s,displs_s,mpi_rtype,&
               sub(1,icount_prev),irc_s,ird_s,mpi_rtype,&
               mpi_comm_world,ierror)
          icount_prev=icount+ione
       endif
    end do
    call reload(sub,g_tv)



!   Divergence and voriticity.  Compute u and v from div and vor
    sub_vor=zero
    sub_div=zero
    sub_u=zero
    sub_v=zero
    icount=izero
    icount_prev=ione
    do k=1,gfshead%levs
       icount=icount+ione

!      The work in the loop below is spread over all mpi tasks
       if (mype==mod(icount-ione,npe)) then

!         Convert spectral coefficients of div and vor to grid space
          if (ncep_sigio) then
             do i=1,nc_b
                spec_div(i)=sigdata%d(i,k)   !div
                spec_vor(i)=sigdata%z(i,k)   !vor
                if(factvml_b(i))then
                   spec_div(i)=zero
                   spec_vor(i)=zero
                end if
             end do
             call sptez_s_bkg(spec_div,grid_div,ione)
             call sptez_s_bkg(spec_vor,grid_vor,ione)
             call sptez_v_bkg(spec_div,spec_vor,grid_u,grid_v,ione)

!         Convert grid u,v to div and vor
          else
             ij=izero
             do j=1,gfshead%latb
                do i=1,gfshead%lonb
                   ij=ij+ione
                   grid_u(i,j)=gfsdata%u(ij,k)
                   grid_v(i,j)=gfsdata%v(ij,k)
                end do
             end do
             call sptez_v_bkg(spec_div,spec_vor,grid_u,grid_v,-ione)
             call sptez_s_bkg(spec_div,grid_div,ione)
             call sptez_s_bkg(spec_vor,grid_vor,ione)
          endif
          
          call fill_ns(grid_div,work_div)
          call fill_ns(grid_vor,work_vor)
          call filluv_ns(grid_u,grid_v,work_u,work_v)
          
       endif

!      Periodically exchange vor,div,u,v between all mpi tasks.
       if (mod(icount,npe)==izero .or. icount==gfshead%levs) then
          call mpi_alltoallv(work_vor,ijn_s,displs_s,mpi_rtype,&
               sub_vor(1,icount_prev),irc_s,ird_s,mpi_rtype,&
               mpi_comm_world,ierror)
          call mpi_alltoallv(work_div,ijn_s,displs_s,mpi_rtype,&
               sub_div(1,icount_prev),irc_s,ird_s,mpi_rtype,&
               mpi_comm_world,ierror)
          call mpi_alltoallv(work_u,ijn_s,displs_s,mpi_rtype,&
               sub_u(1,icount_prev),irc_s,ird_s,mpi_rtype,&
               mpi_comm_world,ierror)
          call mpi_alltoallv(work_v,ijn_s,displs_s,mpi_rtype,&
               sub_v(1,icount_prev),irc_s,ird_s,mpi_rtype,&
               mpi_comm_world,ierror)
          icount_prev=icount+ione
       endif
    end do
    
!   Transfer vor,div,u,v into real(r_kind) guess arrays
    call reload(sub_vor,g_vor)
    call reload(sub_div,g_div)
    call reload(sub_u,g_u)
    call reload(sub_v,g_v)


!   Specific humidity
    sub=zero
    icount=izero
    icount_prev=ione
    do k=1,gfshead%levs
       icount=icount+ione
       if (mype==mod(icount-ione,npe)) then
          if (ncep_sigio) then
             do i=1,nc_b
                spec_work(i)=sigdata%q(i,k,1)
                if(factsml_b(i))spec_work(i)=zero
             end do
             call sptez_s_bkg(spec_work,grid,ione)
          else
             ij=izero
             do j=1,gfshead%latb
                do i=1,gfshead%lonb
                   ij=ij+ione
                   grid(i,j)=gfsdata%q(ij,k,1)
                end do
             end do
          endif

!         Adjust zonal mean specific humidity
          qminlev= 999.0_r_kind
          qmaxlev=-999.0_r_kind
          do j=1,nlatm2
             qlatmean(j)=zero
             do i=1,nlon
                qlatmean(j)=qlatmean(j)+grid(i,j)
             end do
             qlatmean(j)=qlatmean(j)*rnlon-1.e-9_r_kind
             qminlev(1)=min(qlatmean(j),qminlev(1))
             qmaxlev(1)=max(qlatmean(j),qmaxlev(1))
          end do
          if(qminlev(1) < 1.e-9_r_kind)then
             do j=1,nlatm2
                do i=1,nlon
                   grid2(i,j)=zero
                   grid3(i,j)=grid(i,j)
                   if (grid(i,j)<qsmall) grid2(i,j)=qsmall-grid(i,j)
                end do
             end do
             call sptez_s_bkg(spec_work,grid2,-ione)
             ii1=izero
             do l=izero,jcap_b
                do m=izero,jcap_b-l
                   ii1=ii1+2_i_kind
                   fact=exp(-r0_01*float(l+m)**2)
                   spec_work(ii1)  =fact*spec_work(ii1)
                   spec_work(ii1-ione)=fact*spec_work(ii1-ione)
!                  if (l+m>10_i_kind) then
!                     spec_work(ii1)  =zero
!                     spec_work(ii1-ione)=zero
!                  end if
                   if (l==izero) spec_work(ii1)=zero
                end do
             end do
             call sptez_s_bkg(spec_work,grid2,ione)
             do j=1,nlatm2
                do i=1,nlon
                   grid3(i,j)=grid3(i,j)+grid2(i,j)
                end do
             end do
             do j=1,nlatm2
                qlatmean(j)=zero
                do i=1,nlon
                   qlatmean(j)=qlatmean(j)+grid3(i,j)
                end do
                qlatmean(j)=qlatmean(j)*rnlon-1.e-9_r_kind
                qminlev(2)=min(qlatmean(j),qminlev(2))
                qmaxlev(2)=max(qlatmean(j),qmaxlev(2))
             end do
             write(6,*) 'READ_GFSATM:  k,qminlev12,qmaxlev12 ',k,qminlev(1),&
                  qminlev(2),qmaxlev(1),qmaxlev(2),' ***DIAG ONLY***'
          end if

          call fill_ns(grid,work)
       endif
       if (mod(icount,npe)==izero .or. icount==gfshead%levs) then
          call mpi_alltoallv(work,ijn_s,displs_s,mpi_rtype,&
               sub(1,icount_prev),irc_s,ird_s,mpi_rtype,&
               mpi_comm_world,ierror)
          icount_prev=icount+ione
       endif
    end do
    call reload(sub,g_q)


!   Ozone mixing ratio
    sub=zero
    icount=izero
    icount_prev=ione
    do k=1,gfshead%levs
       icount=icount+ione
       if (mype==mod(icount-ione,npe)) then
          if (ncep_sigio) then
             do i=1,nc_b
                spec_work(i)=sigdata%q(i,k,2)
                if(factsml_b(i))spec_work(i)=zero
             end do
             call sptez_s_bkg(spec_work,grid,ione)
          else
             ij=izero
             do j=1,gfshead%latb
                do i=1,gfshead%lonb
                   ij=ij+ione
                   grid(i,j)=gfsdata%q(ij,k,2)
                end do
             end do
          endif
          call fill_ns(grid,work)
       endif
       if (mod(icount,npe)==izero .or. icount==gfshead%levs) then
          call mpi_alltoallv(work,ijn_s,displs_s,mpi_rtype,&
               sub(1,icount_prev),irc_s,ird_s,mpi_rtype,&
               mpi_comm_world,ierror)
          icount_prev=icount+ione
       endif
    end do
    call reload(sub,g_oz)
    

!   Cloud condensate mixing ratio.
    if (gfshead%ntrac>2_i_kind .or. gfshead%ncldt>=ione) then
       sub=zero
       icount=izero
       icount_prev=ione
       do k=1,gfshead%levs
          icount=icount+ione
          if (mype==mod(icount-ione,npe)) then
             if (ncep_sigio) then
                do i=1,nc_b
                   spec_work(i)=sigdata%q(i,k,3)
                   if(factsml_b(i))spec_work(i)=zero
                end do
                call sptez_s_bkg(spec_work,grid,ione)
             else
                ij=izero
                do j=1,gfshead%latb
                   do i=1,gfshead%lonb
                      ij=ij+ione
                      grid(i,j)=gfsdata%q(ij,k,3)
                   end do
                end do
             endif
             call fill_ns(grid,work)
          endif
          if (mod(icount,npe)==izero .or. icount==gfshead%levs) then
             call mpi_alltoallv(work,ijn_s,displs_s,mpi_rtype,&
                  sub(1,icount_prev),irc_s,ird_s,mpi_rtype,&
                  mpi_comm_world,ierror)
             icount_prev=icount+ione
          endif
       end do
       call reload(sub,g_cwmr)
    else
       do k=1,gfshead%levs
          do j=1,lon2
             do i=1,lat2
                g_cwmr(i,j,k)=zero
             end do
          end do
       end do
    endif
    
!   Deallocate sigio data array
    if (ncep_sigio) call sigio_axdata(sigdata,iret)
    iret_read=iret_read+iret


!   Print date/time stamp 
    if(mype==izero) then
       write(6,700) gfshead%lonb,gfshead%latb,gfshead%levs,&
            gfshead%fhour,gfshead%idate
700    format('READ_GFSATM:  ges read/scatter, lonb,latb,levs=',&
            3i6,', hour=',f10.1,', idate=',4i5)
    end if

    return


!   ERROR detected while reading file
1000 continue
    if (mype==izero) write(6,*)'READ_GFSATM:  ***ERROR*** while reading ',&
         filename,' from unit ',lunges,'.   iret=',iret
    call sigio_axdata(sigdata,iret)
    iret_read=iret_read+iret

    
!   End of routine.  Return
    return
  end subroutine read_gfsatm



  subroutine read_gfssfc(filename,mype,fact10,sfct,sno,veg_type,&
       veg_frac,soil_type,soil_temp,soil_moi,isli,sfc_rough,terrain)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_gfssfc     read gfs surface file
!   prgmmr: treadon          org: np23                date: 2003-04-10
!
! abstract: read gfs surface file
!
! program history log:
!   2003-04-10  treadon
!   2004-05-18  kleist, add global isli & documentation
!   2004-09-07  treadon fix mpi bug when npe > nsfc
!   2005-01-27  treadon - rewrite to make use of sfcio module
!   2005-03-07  todling - die gracefully when return error from sfcio
!   2006-09-28  treadon - pull out surface roughness
!   2008-05-28  safford - rm unused vars
!   2009-01-12  gayno   - add read of terrain height
!
!   input argument list:
!     filename - name of surface guess file
!     mype     - mpi task id
!
!   output argument list:
!     fact10    - 10 meter wind factor
!     sfct      - surface temperature (skin temp)
!     sno       - snow depth
!     veg_type  - vegetation type
!     veg_frac  - vegetation fraction
!     soil_type - soil type
!     soil_temp - soil temperature of first layer
!     soil_moi  - soil moisture of first layer
!     isli      - sea/land/ice mask (subdomain)
!     isli_g    - global sea/land/ice mask
!     sfc_rough - surface roughness
!     terrain   - terrain height
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: r_kind,i_kind
    use gridmod, only: nlat_sfc,nlon_sfc
    use sfcio_module, only: sfcio_intkind,sfcio_head,sfcio_data,&
         sfcio_srohdc,sfcio_axdata
    use constants, only: izero,ione,zero
    implicit none

!   Declare passed variables
    character(24),intent(in):: filename
    integer(i_kind),intent(in):: mype
    integer(i_kind),dimension(nlat_sfc,nlon_sfc),intent(out):: isli
    real(r_kind),dimension(nlat_sfc,nlon_sfc),intent(out):: fact10,sfct,sno,&
         veg_type,veg_frac,soil_type,soil_temp,soil_moi,sfc_rough,terrain

!   Declare local parameters
    integer(sfcio_intkind):: lunges = 11
    integer(i_kind),parameter:: nsfc=11_i_kind

!   Declare local variables
    integer(i_kind) i,j,k,latb,lonb,mm1
    integer(sfcio_intkind):: irets
    real(r_kind) sumn,sums
    real(r_kind),allocatable,dimension(:,:,:):: work,sfcges

    type(sfcio_head):: sfc_head
    type(sfcio_data):: sfc_data

    mm1=mype+ione
!-----------------------------------------------------------------------------
!     Read surface file
      call sfcio_srohdc(lunges,filename,sfc_head,sfc_data,irets)


!     Check for possible problems
      if (irets /= izero) then
       write(6,*)'READ_GFSSFC:  ***ERROR*** problem reading ',filename,&
            ', irets=',irets
       call sfcio_axdata(sfc_data,irets)
       call stop2(80)
      endif
      latb=sfc_head%latb
      lonb=sfc_head%lonb
      if ( (latb /= nlat_sfc-2_i_kind) .or. &
           (lonb /= nlon_sfc) ) then
         write(6,*)'READ_GFSSFC:  ***ERROR*** inconsistent grid dimensions.  ',&
              ', nlon,nlat-2=',nlon_sfc,nlat_sfc-2_i_kind,' -vs- sfc file lonb,latb=',&
              lonb,latb
         call sfcio_axdata(sfc_data,irets)
         call stop2(80)
      endif

!     Load surface fields into local work array
      allocate(work(lonb,latb,nsfc),sfcges(latb+2_i_kind,lonb,nsfc))
      do k=1,nsfc
        do j=1,latb
          do i=1,lonb
             work(i,j,k) = zero
          end do
        end do
      end do
      do j=1,latb
         do i=1,lonb
            work(i,j,1)  = sfc_data%tsea  (i,j)    ! skin temperature
            work(i,j,2)  = sfc_data%smc (i,j,1)    ! soil moisture
            work(i,j,3)  = sfc_data%sheleg(i,j)    ! snow depth
            work(i,j,4)  = sfc_data%stc (i,j,1)    ! soil temperature
            work(i,j,5)  = sfc_data%slmsk (i,j)    ! sea/land/ice mask
            work(i,j,6)  = sfc_data%vfrac (i,j)    ! vegetation cover
            work(i,j,7)  = sfc_data%f10m  (i,j)    ! 10m wind factor
            work(i,j,8)  = sfc_data%vtype (i,j)    ! vegetation type
            work(i,j,9)  = sfc_data%stype (i,j)    ! soil type
            work(i,j,10) = sfc_data%zorl  (i,j)    ! surface roughness length (cm)
            work(i,j,11) = sfc_data%orog  (i,j)    ! terrain
         end do
      end do

!     Fill surface guess array
      do k=1,nsfc

!        Compute mean for southern- and northern-most rows
!        of surface guess array
         sumn = zero
         sums = zero
         do i=1,lonb
            sumn = work(i,1,k)    + sumn
            sums = work(i,latb,k) + sums
         end do
         sumn = sumn/float(lonb)
         sums = sums/float(lonb)

!        Transfer from local work array to surface guess array
         do j = 1,lonb
            sfcges(1,j,k)=sums
            sfcges(latb+2_i_kind,j,k)=sumn
            do i=2,latb+ione
              sfcges(i,j,k) = work(j,latb+2_i_kind-i,k)
            end do
          end do

!     End of loop over data records
      end do

!     Deallocate local work arrays
      deallocate(work)
!   Load data into output arrays
    do j=1,lonb
      do i=1,latb+2_i_kind
        sfct(i,j)      = sfcges(i,j,1)
        soil_moi(i,j)  = sfcges(i,j,2)
        sno(i,j)       = sfcges(i,j,3)
        soil_temp(i,j) = sfcges(i,j,4)
        isli(i,j)      = nint(sfcges(i,j,5)+0.0000001_r_kind)
        veg_frac(i,j)  = sfcges(i,j,6)
        fact10(i,j)    = sfcges(i,j,7)
        veg_type(i,j)  = sfcges(i,j,8)
        soil_type(i,j) = sfcges(i,j,9)
        sfc_rough(i,j) = sfcges(i,j,10)
        terrain(i,j)   = sfcges(i,j,11)
      end do
    end do
    deallocate(sfcges)

!   Print date/time stamp
    if(mype==izero) then
       write(6,700) latb,lonb,sfc_head%fhour,sfc_head%idate
700    format('READ_GFSSFC:  ges read/scatter, nlat,nlon=',&
            2i6,', hour=',f10.1,', idate=',4i5)
    end if

    return
  end subroutine read_gfssfc


  subroutine write_gfs(increment,mype,mype_atm,mype_sfc)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    write_gfs
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2006-07-31  kleist - pass ges_ps instead of ges_lnps
!   2006-10-11  treadon - update 10m wind factor in sfc file
!   2008-05-28  safford - rm unused vars, add doc block
!   2008-12-05  todling - adjustment for dsfct time dimension addition
!   2009-11-28  todling - add increment option (hook-only for now)
!
!   input argument list:
!     increment          - when .t. will name files as increment files
!     mype               - mpi task id
!     mype_atm,mype_sfc  -
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    use kinds, only: i_kind
    use constants, only: izero
    use guess_grids, only: ges_z,ges_ps,ges_vor,ges_div,&
         ges_tv,ges_q,ges_oz,ges_cwmr,ges_prsl,&
         ges_u,ges_v,ges_prsi,dsfct
    use guess_grids, only: ntguessig,ntguessfc

    implicit none

    logical,intent(in)::increment
    integer(i_kind),intent(in):: mype,mype_atm,mype_sfc
    character(24):: filename
!   Write atmospheric analysis file
    if (increment) then
      filename='siginc'
      if(mype==izero) write(6,'(a)') 'write_gfs: sorry, I do not know how to write increments yet'
      return
    else
      filename='siganl'
    endif
    call write_gfsatm(filename,mype,mype_atm,&
         ges_z(1,1,ntguessig),ges_ps(1,1,ntguessig),&
         ges_vor(1,1,1,ntguessig),ges_div(1,1,1,ntguessig),&
         ges_tv(1,1,1,ntguessig),ges_q(1,1,1,ntguessig),&
         ges_oz(1,1,1,ntguessig),ges_cwmr(1,1,1,ntguessig),&
         ges_prsl(1,1,1,ntguessig),ges_u(1,1,1,ntguessig),&
         ges_v(1,1,1,ntguessig),ges_prsi(1,1,1,ntguessig))

!   Write surface analysis file
    if (increment) then
      filename='sfcinc.gsi'
    else
      filename='sfcanl.gsi'
    endif
    call write_gfssfc(filename,mype,mype_sfc,dsfct(1,1,ntguessfc))
  end subroutine write_gfs


  subroutine write_gfsatm(filename,mype,mype_out,sub_z,sub_ps,&
       sub_vor,sub_div,sub_tv,sub_q,sub_oz,sub_cwmr,sub_prsl,&
       sub_u,sub_v,sub_prsi)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    write_gfsatm --- Gather, transform, and write out 
!                                 spectal coefficients
!   prgrmmr:     parrish    - author; org: np22
!
! abstract: This routine gathers fields needed for the GSI analysis
!           file from subdomains and then transforms the fields from
!           grid to spectral space.  The spectral coefficients are 
!           then written to an atmospheric analysis file.
!
! program history log:
!   1998-07-10  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2003-10-31  kleist, d. - add capability to generate output file for 
!                            either hybrid or sigma vertical coordinate
!   2004-06-15  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue; added intent/only's
!   2004-08-27  treadon - use splib routine for grid <---> spectral transforms
!   2005-03-07  dee     - support gmao model interface
!   2005-03-10  treadon - remove iadate from calling list, access via obsmod
!   2005-04-05  wgu     - bug fix: modified iadate not properly merge w/ gmao_intfc case
!   2005-10-13  treadon - properly specify vcid4 in NCEP sigma file header
!   2005-12-09  guo     - removed special GMAO spectral output format
!   2006-01-09  treadon - use sigio
!   2006-09-18  treadon - convert ps to lnps
!   2007-05-07  treadon - add gfsio
!   2007-05-08  kleist  - add options for ps or lnps
!   2008-05-28  safford - rm unused vars and uses
!   2009-06-11  kleist  - add sppad for multiple spectral resolutions
!
!   input argument list:
!     filename  - file to open and write to
!     mype      - mpi task number
!     mype_out  - mpi task to write output file
!     sub_z     - GFS terrain field on subdomains
!     sub_ps    - surface pressure on subdomains
!     sub_vor   - vorticity on subdomains
!     sub_div   - divergence on subdomains
!     sub_tv    - virtual temperature on subdomains
!     sub_q     - specific humidity on subdomains
!     sub_oz    - ozone on subdomains
!     sub_cwmr  - cloud condensate mixing ratio on subdomains
!     sub_prsl  - layer midpoint pressure
!     sub_u     - zonal wind
!     sub_v     - meridional wind
!     sub_prsi  - interface  pressure
!
!   output argument list:
!
! attributes:
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!$$$ end documentation block

! !USES:
    use kinds, only: r_kind,i_kind,r_single
    
    use constants, only: izero,ione,zero_single,r1000,fv,one,zero
  
    use mpimod, only: mpi_rtype,mpi_rtype4
    use mpimod, only: mpi_comm_world
    use mpimod, only: strip
    use mpimod, only: ierror
    use mpimod, only: mpi_status_size
    use mpimod, only: npe
    
    use guess_grids, only: ntguessig,ifilesig
    
    use gridmod, only: nlat, nlon     ! no. lat/lon
    use gridmod, only: lat1, lon1     ! no. lat/lon on subdomain (no buffer)
    use gridmod, only: lat2, lon2     ! no. lat/lon on subdomain (buffer pnts on ends)
    use gridmod, only: nsig           ! no. levels
    use gridmod, only: iglobal        ! no. of horizontal points on global grid
    use gridmod, only: ijn            ! no. of horiz. pnts for each subdomain (no buffer)
    use gridmod, only: displs_g       ! comm. array, displacement for receive on global grid
    use gridmod, only: itotsub        ! no. of horizontal points of all subdomains combined
    use gridmod, only: load_grid
    use gridmod, only: idpsfc5        ! identifier for ps vs ln(ps)
    use gridmod, only: idthrm5        ! identifier for thermodynamic variable
    use gridmod, only: cp5
    use gridmod, only: idvc5
    use gridmod, only: idvm5
    use gridmod, only: ntracer
    use gridmod, only: ncloud
    use gridmod, only: ncep_sigio
    use gridmod, only: ncepgfs_head
    
    use obsmod, only: iadate
    
    use specmod, only: nc_b,nc
    use specmod, only: jcap_b,jcap
    use specmod, only: factsml_b
    use specmod, only: factvml_b
    
    use sigio_module, only: sigio_intkind,sigio_head,sigio_data,&
         sigio_swopen,sigio_swhead,sigio_swdata,sigio_axdata,&
         sigio_srohdc,sigio_realkind

    use gfsio_module, only: gfsio_gfile,gfsio_open,gfsio_init,&
         gfsio_getfilehead,gfsio_close,gfsio_writerecv

  
    implicit none

! !INPUT PARAMETERS:

    character(24),intent(in):: filename     ! file to open and write to

    integer(i_kind),intent(in) :: mype      ! mpi task number
    integer(i_kind),intent(in) :: mype_out  ! mpi task to write output file
    
    real(r_kind),dimension(lat2,lon2),     intent(in):: sub_z    ! GFS terrain field on subdomains
    real(r_kind),dimension(lat2,lon2),     intent(in):: sub_ps   ! surface pressure on subdomains
    real(r_kind),dimension(lat2,lon2,nsig),intent(in):: sub_vor  ! vorticity on subdomains
    real(r_kind),dimension(lat2,lon2,nsig),intent(in):: sub_div  ! divergence on subdomains
    real(r_kind),dimension(lat2,lon2,nsig),intent(in):: sub_tv   ! virtual temperature on subdomains
    real(r_kind),dimension(lat2,lon2,nsig),intent(in):: sub_q    ! specific humidity on subdomains
    real(r_kind),dimension(lat2,lon2,nsig),intent(in):: sub_oz   ! ozone on subdomains
    real(r_kind),dimension(lat2,lon2,nsig),intent(in):: sub_cwmr ! cloud condensate mixing ratio on subdomains
    real(r_kind),dimension(lat2,lon2,nsig),intent(in):: sub_prsl ! layer midpoint pressure
    real(r_kind),dimension(lat2,lon2,nsig),intent(in):: sub_u    ! zonal wind
    real(r_kind),dimension(lat2,lon2,nsig),intent(in):: sub_v    ! meridional wind
    real(r_kind),dimension(lat2,lon2,nsig+1),intent(in):: sub_prsi ! interface  pressure

!-------------------------------------------------------------------------

    integer(i_kind),parameter::  lunges = 11_i_kind
    integer(i_kind),parameter::  lunanl = 51_i_kind

    character(5):: string
    character(6):: fname_ges
    integer(i_kind) i,j,ij,k,mm1,nlatm2
    integer(sigio_intkind):: iret
    
    real(r_kind),dimension(lat1*lon1):: hsm,psm
    real(r_kind),dimension(lat2,lon2,nsig):: sub_dp
    real(r_kind),dimension(lat1*lon1,nsig):: tvsm,vorsm,divsm
    real(r_kind),dimension(lat1*lon1,nsig):: prslm,usm,vsm,dpsm
    real(r_kind),dimension(lat1*lon1,nsig,ntracer):: qsm
    real(r_kind),dimension(max(iglobal,itotsub)):: work1
    real(r_kind),dimension(max(iglobal,itotsub),nsig):: work1_k
    real(r_kind),dimension(nlon,nlat-2_i_kind):: grid,grid2
    real(r_kind),dimension(nc_b):: spec_work
    real(r_kind),dimension(nc):: spec_work_sm


    type(sigio_head):: sighead
    type(sigio_data):: sigdata
    type(gfsio_gfile) :: gfilei,gfileo
    type(ncepgfs_head)  :: gfshead

    type:: gfsio_data
       real(r_single),allocatable:: hs(:)      !surface height, m
       real(r_single),allocatable:: ps(:)      !surface pressure, pa
       real(r_single),allocatable:: p(:,:)     !layer pressure, pa
       real(r_single),allocatable:: dp(:,:)    !layer pressure thickness, pa
       real(r_single),allocatable:: t(:,:)     !layer temperature, k
       real(r_single),allocatable:: u(:,:)     !layer zonal wind, m/s
       real(r_single),allocatable:: v(:,:)     !layer meridional wind, m/s
       real(r_single),allocatable:: q(:,:,:)   !tracers, 1-spfh; 2-O3; 3-CLW , kg/kg
    end type gfsio_data
    type(gfsio_data)  :: gfsdata

  integer(i_kind) :: mype_th,mype_sh,mype_oz,mype_clc,mype_div,mype_vort
  integer(i_kind) :: itag_th,itag_sh,itag_oz,itag_clc,itag_div,itag_vort
  integer(i_kind) :: status(mpi_status_size),istat,pe_stride
  real(kind=sigio_realkind),allocatable :: temp(:,:)
 
!*************************************************************************
!   Initialize local variables
    mm1=mype+ione
    nlatm2=nlat-2_i_kind

!   Set mpi tasks and tags for PE's to do grid transformations
    pe_stride   = max(izero,(npe-2_i_kind)/6)

    mype_th   = min(2_i_kind,npe-ione)
    itag_th   = 10000_i_kind
    mype_sh   = mype_th+pe_stride
    itag_sh   = 10001_i_kind
    mype_oz   = mype_sh+pe_stride
    itag_oz   = 10002_i_kind
    mype_clc  = mype_oz+pe_stride
    itag_clc  = 10003_i_kind
    mype_div  = mype_clc+pe_stride
    itag_div  = 10004_i_kind
    mype_vort = mype_div+pe_stride
    itag_vort = 10005_i_kind


!   Strip off boundary points from subdomains
    call strip(sub_z   ,hsm       ,ione)
    call strip(sub_ps  ,psm       ,ione)
    call strip(sub_vor ,vorsm     ,nsig)
    call strip(sub_div ,divsm     ,nsig)
    call strip(sub_tv  ,tvsm      ,nsig)
    call strip(sub_q   ,qsm(1,1,1),nsig)
    call strip(sub_oz  ,qsm(1,1,2),nsig)
    call strip(sub_cwmr,qsm(1,1,3),nsig)
!   Read guess file.   Pull out header information.  
!   Update header and write out to analysis file.
!   These operations only need to be done on the
!   analysis output task, mype_out

    if (ncep_sigio) then
      if (mype==mype_out) then
!       Set guess file name
        write(fname_ges,100) ifilesig(ntguessig)
100     format('sigf',i2.2)
!       Handle case of NCEP SIGIO
!       Read header and spectral coefficients from guess
        call sigio_srohdc(lunges,fname_ges,sighead,sigdata,iret)
!send data to compute pes
        call mpi_send(sigdata%t,nc_b*nsig,mpi_rtype4,mype_th,&
                      itag_th,mpi_comm_world,ierror)
        call mpi_send(sigdata%q(1,1,1),nc_b*nsig,mpi_rtype4,mype_sh,&
                      itag_sh,mpi_comm_world,ierror)
        call mpi_send(sigdata%q(1,1,2),nc_b*nsig,mpi_rtype4,mype_oz,&
                      itag_oz,mpi_comm_world,ierror)
        if (ntracer>2_i_kind .or. ncloud>=ione) then
          call mpi_send(sigdata%q(1,1,3),nc_b*nsig,mpi_rtype4,mype_clc,&
                        itag_clc,mpi_comm_world,ierror)
        endif
        call mpi_send(sigdata%d,nc_b*nsig,mpi_rtype4,mype_div,&
                      itag_div,mpi_comm_world,ierror)

        call mpi_send(sigdata%z,nc_b*nsig,mpi_rtype4,mype_vort,&
                      itag_vort,mpi_comm_world,ierror)
!

!       Replace header record date with analysis time
        sighead%fhour    = zero_single
        sighead%idate(1) = iadate(4) !hour
        sighead%idate(2) = iadate(2) !month
        sighead%idate(3) = iadate(3) !day
        sighead%idate(4) = iadate(1) !year

!       Load grid dimension and other variables used below
!       into local header structure
        gfshead%fhour   = sighead%fhour   
        gfshead%idate   = sighead%idate
        gfshead%levs    = sighead%levs
        gfshead%ntrac   = sighead%ntrac
        gfshead%ncldt   = sighead%ncldt
        gfshead%jcap    = sighead%jcap
        gfshead%lonb    = nlon
        gfshead%latb    = nlatm2
        gfshead%idrt    = 4_i_kind

!       Write header to analysis file
        call sigio_swopen(lunanl,filename,iret)
        call sigio_swhead(lunanl,sighead,iret)
      else
        if (mype==mype_th) then
          allocate (temp(nc_b,nsig),stat=istat)
          call mpi_recv(temp,nc_b*nsig,mpi_rtype4,mype_out,&
                        itag_th,mpi_comm_world,status,ierror)
        endif
        if (mype==mype_sh) then
          allocate (temp(nc_b,nsig),stat=istat)
          call mpi_recv(temp,nc_b*nsig,mpi_rtype4,mype_out,&
                        itag_sh,mpi_comm_world,status,ierror)
        endif
        if (mype==mype_oz) then
          allocate (temp(nc_b,nsig),stat=istat)
          call mpi_recv(temp,nc_b*nsig,mpi_rtype4,mype_out,&
                        itag_oz,mpi_comm_world,status,ierror)
        endif
        if (mype==mype_clc) then
          if (ntracer>2_i_kind .or. ncloud>=ione) then
            allocate (temp(nc_b,nsig),stat=istat)
            call mpi_recv(temp,nc_b*nsig,mpi_rtype4,mype_out,&
                          itag_clc,mpi_comm_world,status,ierror)
          endif
        endif
        if (mype==mype_div) then
          allocate (temp(nc_b,nsig),stat=istat)
          call mpi_recv(temp,nc_b*nsig,mpi_rtype4,mype_out,&
                        itag_div,mpi_comm_world,status,ierror)
        endif
        if (mype==mype_vort) then
          allocate (temp(nc_b,nsig),stat=istat)
          call mpi_recv(temp,nc_b*nsig,mpi_rtype4,mype_out,&
                        itag_vort,mpi_comm_world,status,ierror)
        endif
      endif
!     Handle case of NCEP GFSIO
    else
      if (mype==mype_out) then
        do k=1,nsig
          do j=1,lon2
             do i=1,lat2
                sub_dp(i,j,k) = sub_prsi(i,j,k)-sub_prsi(i,j,k+1)
             end do
          end do
        end do
        call strip(sub_dp,dpsm,nsig)
        call strip(sub_prsl,prslm,nsig)
        call strip(sub_u,usm,nsig)
        call strip(sub_v,vsm,nsig)

!       Read header record from guess
        call gfsio_init(iret)
        call gfsio_open(gfilei,fname_ges,'read',iret)
        call gfsio_getfilehead(gfilei,&
             version=gfshead%version,&
             fhour=gfshead%fhour,&
             idate=gfshead%idate,&
             levs=gfshead%levs, &
             ntrac=gfshead%ntrac, &
             ncldt=gfshead%ncldt, &
             lonb=gfshead%lonb, &
             latb=gfshead%latb, &
             jcap=gfshead%jcap, &
             idrt=gfshead%idrt, &
             iret=iret)
        gfileo=gfilei
        call gfsio_close(gfilei,iret)
          
!       Replace header record date with analysis time
        gfshead%fhour = zero_single
        gfshead%idate(1) = iadate(4)  !hour
        gfshead%idate(3) = iadate(3)  !month
        gfshead%idate(2) = iadate(2)  !day
        gfshead%idate(4) = iadate(1)  !year

!       Write header record to gfsio output file
        call gfsio_init(iret)
        call gfsio_open(gfileo,filename,'write',&
             fhour=gfshead%fhour,&
             idate=gfshead%idate,&
             iret=iret)
        if (iret/=izero) then
           write(6,*)'WRITE_GFSATM:  ***ERROR***  with write to gfsio_open ',&
                filename,iret
           call stop2(99)
        endif

!       Allocate structure arrays to hold data
        allocate(gfsdata%hs(gfshead%latb*gfshead%lonb))
        allocate(gfsdata%ps(gfshead%latb*gfshead%lonb))
        allocate(gfsdata%p(gfshead%latb*gfshead%lonb,gfshead%levs))
        allocate(gfsdata%dp(gfshead%latb*gfshead%lonb,gfshead%levs))
        allocate(gfsdata%t(gfshead%latb*gfshead%lonb,gfshead%levs))
        allocate(gfsdata%u(gfshead%latb*gfshead%lonb,gfshead%levs))
        allocate(gfsdata%v(gfshead%latb*gfshead%lonb,gfshead%levs))
        allocate(gfsdata%q(gfshead%latb*gfshead%lonb,gfshead%levs,gfshead%ntrac))
      endif
    end if
!gather the fields on the processors that will perform grid transforms
!   Thermodynamic variable
!   The GSI analysis variable is virtual temperature (Tv).   For GFSIO
!   output we need the sensible temperature.  For SIGIO we have three
!   possibilities:  Tv, sensible temperature (T), or enthalpy (h=CpT)
    if (ncep_sigio) then
       if (idthrm5==2_i_kind .or. idthrm5==3_i_kind) then

!         Convert Tv to T
          do k=1,nsig
             do i=1,lat1*lon1
                tvsm(i,k)=tvsm(i,k)/(one+fv*qsm(i,k,1))
             end do
          end do

!         If CpT is requested, call function to make conversion
          if (idthrm5==3_i_kind) call sigio_cnvtdv8(lat1*lon1,lat1*lon1,&
               nsig,idvc5,idvm5,ntracer,iret,tvsm,qsm,cp5,-ione)
       endif
    else
!   Handle the case of GFSIO.    Convert Tv to T
       do k=1,nsig
          do i=1,lat1*lon1
             tvsm(i,k) = tvsm(i,k)/(one+fv*qsm(i,k,1))
          end do
       end do
    endif
!   create global grid by gathering from subdomains
    if (ncep_sigio) then
      do k=1,nsig
        call mpi_gatherv(tvsm(1,k),ijn(mm1),mpi_rtype,&
             work1_k(1,k),ijn,displs_g,mpi_rtype,&
             mype_th,mpi_comm_world,ierror)
      end do
!     Specific humidity
      do k=1,nsig
        call mpi_gatherv(qsm(1,k,1),ijn(mm1),mpi_rtype,&
             work1_k(1,k),ijn,displs_g,mpi_rtype,&
             mype_sh,mpi_comm_world,ierror)
      end do
!     Ozone
      do k=1,nsig
        call mpi_gatherv(qsm(1,k,2),ijn(mm1),mpi_rtype,&
             work1_k(1,k),ijn,displs_g,mpi_rtype,&
             mype_oz,mpi_comm_world,ierror)
      end do
!     Cloud condensate mixing ratio
      if (ntracer>2_i_kind .or. ncloud>=ione) then
        do k=1,nsig
          call mpi_gatherv(qsm(1,k,3),ijn(mm1),mpi_rtype,&
               work1_k(1,k),ijn,displs_g,mpi_rtype,&
               mype_clc,mpi_comm_world,ierror)
        end do
      endif
!     Horizontal divergence and voriticy
      do k=1,nsig
          call mpi_gatherv(divsm(1,k),ijn(mm1),mpi_rtype,&
               work1_k(1,k),ijn,displs_g,mpi_rtype,&
               mype_div,mpi_comm_world,ierror)
      end do
      do k=1,nsig
          call mpi_gatherv(vorsm(1,k),ijn(mm1),mpi_rtype,&
               work1_k(1,k),ijn,displs_g,mpi_rtype,&
               mype_vort,mpi_comm_world,ierror)
      end do
    endif

!   Generate and write analysis fields

!   For each output grid, the following steps are repeated
!     1) create global grid by gathering from subdomains
!     2) transfrom from grid space representation to spectral coefficients
!     3) apply factor to ensure certain coefficients are zero
!     4) write spectral coefficients to output file
!   Note that steps 2-4 are done on a single task (here mpi task 0)

!   Terrain
    call mpi_gatherv(hsm,ijn(mm1),mpi_rtype,&
         work1,ijn,displs_g,mpi_rtype,&
         mype_out,mpi_comm_world,ierror)
    if (mype==mype_out) then
       call load_grid(work1,grid)
       if (ncep_sigio) then
          do i=1,nc_b
             spec_work(i) = sigdata%hs(i)
             if(factsml_b(i))spec_work(i)=zero
          end do
          call sptez_s_bkg(spec_work,grid2,ione)
          grid=grid-grid2
          call sptez_s(spec_work_sm,grid,-ione)
          call sppad(izero,jcap,spec_work_sm,izero,jcap_b,spec_work)
          do i=1,nc_b
             sigdata%hs(i)=sigdata%hs(i)+spec_work(i)
             if(factsml_b(i))sigdata%hs(i)=zero_single
          end do
       else
          ij=izero
          do j=1,nlatm2
             do i=1,nlon
                ij=ij+ione
                gfsdata%hs(ij)=grid(i,j)
             end do
          end do
          call gfsio_writerecv(gfileo,'hgt','sfc',ione,gfsdata%hs,iret,idrt=gfshead%idrt)
       endif
    endif

!   Surface pressure.  
!   NCEP SIGIO outputs surface pressure or ln(surface pressure)
    if (ncep_sigio) then
       if (idpsfc5 /= 2_i_kind) then
          do i=1,lat1*lon1
             psm(i)=log(psm(i))
          end do
       endif
    endif
    call mpi_gatherv(psm,ijn(mm1),mpi_rtype,&
         work1,ijn,displs_g,mpi_rtype,&
         mype_out,mpi_comm_world,ierror)
    if (mype==mype_out) then
       call load_grid(work1,grid)
       if (ncep_sigio) then
          do i=1,nc_b
             spec_work(i) = sigdata%ps(i)
             if(factsml_b(i))spec_work(i)=zero
          end do
          call sptez_s_bkg(spec_work,grid2,ione)
          grid=grid-grid2
          call sptez_s(spec_work_sm,grid,-ione)
          call sppad(izero,jcap,spec_work_sm,izero,jcap_b,spec_work)
          do i=1,nc_b
             sigdata%ps(i)=sigdata%ps(i)+spec_work(i)
             if(factsml_b(i))sigdata%ps(i)=zero_single
          end do
       else
          ij=izero
          do j=1,nlatm2
             do i=1,nlon
                ij=ij+ione
                gfsdata%ps(ij)=grid(i,j)*r1000
             end do
          end do
          call gfsio_writerecv(gfileo,'pres','sfc',ione,gfsdata%ps,iret,idrt=gfshead%idrt)
       endif
    endif

!   Thermodynamic variable
    if (ncep_sigio) then
      if (mype==mype_th) then
!$omp parallel do private(k,grid,i,spec_work,grid2)
        do k=1,nsig
          call load_grid(work1_k(1,k),grid)
          do i=1,nc_b
             spec_work(i) = temp(i,k)
             if(factsml_b(i))spec_work(i)=zero
          end do
          call sptez_s_bkg(spec_work,grid2,ione)
          grid=grid-grid2
          call sptez_s(spec_work_sm,grid,-ione)
          call sppad(izero,jcap,spec_work_sm,izero,jcap_b,spec_work)
          do i=1,nc_b
             temp(i,k)=temp(i,k)+spec_work(i)
             if(factsml_b(i))temp(i,k)=zero_single
          end do
        end do
!$omp end parallel do
!send temperature back to mype_out
        call mpi_send(temp,nc_b*nsig,mpi_rtype4,mype_out,&
                      itag_th,mpi_comm_world,ierror)
      endif
    else                 !GFS I/O
      do k=1,nsig
        call mpi_gatherv(tvsm(1,k),ijn(mm1),mpi_rtype,&
             work1,ijn,displs_g,mpi_rtype,&
             mype_out,mpi_comm_world,ierror)
        if (mype == mype_out) then
           call load_grid(work1,grid)
           ij=izero
           do j=1,nlatm2
              do i=1,nlon
                 ij=ij+ione
                 gfsdata%t(ij,k)=grid(i,j)
              end do
           end do
           call gfsio_writerecv(gfileo,'tmp','layer',k,gfsdata%t(:,k),iret,&
                idrt=gfshead%idrt)
        endif
      end do
    endif
!   Specific humidity
    if (ncep_sigio) then
      if (mype==mype_sh) then
!$omp parallel do private(k,grid,i,spec_work,grid2)
        do k=1,nsig
          call load_grid(work1_k(1,k),grid)
          do i=1,nc_b
             spec_work(i) = temp(i,k)
             if(factsml_b(i))spec_work(i)=zero
          end do
          call sptez_s_bkg(spec_work,grid2,ione)
          call load_grid(work1_k(1,k),grid)
          grid=grid-grid2
          call sptez_s(spec_work_sm,grid,-ione)
          call sppad(izero,jcap,spec_work_sm,izero,jcap_b,spec_work)
          do i=1,nc_b
             temp(i,k) =temp(i,k)+spec_work(i)
             if(factsml_b(i))temp(i,k)=zero_single
          end do
        end do
!$omp end parallel do
!send sh back to mype_out
        call mpi_send(temp,nc_b*nsig,mpi_rtype4,mype_out,&
                      itag_sh,mpi_comm_world,ierror)
      endif
    else             !GFS I/O
      do k=1,nsig
        call mpi_gatherv(qsm(1,k,1),ijn(mm1),mpi_rtype,&
             work1,ijn,displs_g,mpi_rtype,&
             mype_out,mpi_comm_world,ierror)
        if (mype == mype_out) then
           call load_grid(work1,grid)
           ij=izero
           do j=1,nlatm2
              do i=1,nlon
                 ij=ij+ione
                 gfsdata%q(ij,k,1)=grid(i,j)
              end do
           end do
           call gfsio_writerecv(gfileo,'spfh','layer',k,gfsdata%q(:,k,1),iret,&
                idrt=gfshead%idrt)
        endif
      end do
    endif

!   Ozone
    if (ncep_sigio) then
      if (mype==mype_oz) then
!$omp parallel do private(k,grid,i,spec_work,grid2)
        do k=1,nsig
          call load_grid(work1_k(1,k),grid)
          do i=1,nc_b
             spec_work(i) = temp(i,k)
             if(factsml_b(i))spec_work(i)=zero
          end do
          call sptez_s_bkg(spec_work,grid2,ione)
          call load_grid(work1_k(1,k),grid)
          grid=grid-grid2
          call sptez_s(spec_work_sm,grid,-ione)
          call sppad(izero,jcap,spec_work_sm,izero,jcap_b,spec_work)
          do i=1,nc_b
             temp(i,k) =temp(i,k) + spec_work(i)
             if(factsml_b(i))temp(i,k)=zero_single
          end do
        end do
!$omp end parallel do
!send sh back to mype_out
        call mpi_send(temp,nc_b*nsig,mpi_rtype4,mype_out,&
                      itag_oz,mpi_comm_world,ierror)
      endif
    else          !GFS I/O
      do k=1,nsig
        call mpi_gatherv(qsm(1,k,2),ijn(mm1),mpi_rtype,&
             work1,ijn,displs_g,mpi_rtype,&
             mype_out,mpi_comm_world,ierror)
        if (mype == mype_out) then
          call load_grid(work1,grid)
          ij=izero
          do j=1,nlatm2
             do i=1,nlon
                ij=ij+ione
                gfsdata%q(ij,k,2)=grid(i,j)
             end do
          end do
          call gfsio_writerecv(gfileo,'o3mr','layer',k,gfsdata%q(:,k,2),iret,&
               idrt=gfshead%idrt)
        endif
      end do
    endif
       
!   Cloud condensate mixing ratio
    if (ntracer>2_i_kind .or. ncloud>=ione) then
      if (ncep_sigio) then
        if (mype==mype_clc) then
!$omp parallel do private(k,grid,i,spec_work,grid2)
          do k=1,nsig
             call load_grid(work1_k(1,k),grid)
             do i=1,nc_b
                spec_work(i) = temp(i,k)
                if(factsml_b(i))spec_work(i)=zero
             end do
             call sptez_s_bkg(spec_work,grid2,ione)
             call load_grid(work1_k(1,k),grid)
             grid=grid-grid2
             call sptez_s(spec_work_sm,grid,-ione)
             call sppad(izero,jcap,spec_work_sm,izero,jcap_b,spec_work)
             do i=1,nc_b
                temp(i,k) =temp(i,k)+spec_work(i)
                if(factsml_b(i))temp(i,k)=zero_single
             end do
          end do
!$omp end parallel do
!send sh back to mype_out
          call mpi_send(temp,nc_b*nsig,mpi_rtype4,mype_out,&
                        itag_clc,mpi_comm_world,ierror)
        endif
      else          !GFS I/O
        do k=1,nsig
          call mpi_gatherv(qsm(1,k,3),ijn(mm1),mpi_rtype,&
               work1,ijn,displs_g,mpi_rtype,&
               mype_out,mpi_comm_world,ierror)
          if (mype == mype_out) then
                ij=izero
                do j=1,nlatm2
                   do i=1,nlon
                      ij=ij+ione
                      gfsdata%q(ij,k,3)=grid(i,j)
                   end do
                end do
                call gfsio_writerecv(gfileo,'clwmr','layer',k,gfsdata%q(:,k,3),iret,&
                     idrt=gfshead%idrt)
          endif
        end do
      endif
    endif

!   NCEP_SIGIO specific output
    if (ncep_sigio) then

!     Horizontal divergence and voriticy
      if (mype==mype_div) then
!$omp parallel do private(k,grid,i,spec_work,grid2)
        do k=1,nsig
             do i=1,nc_b
                spec_work(i) = temp(i,k)
                if(factsml_b(i))spec_work(i)=zero
             end do
             call sptez_s_bkg(spec_work,grid2,ione)
             call load_grid(work1_k(1,k),grid)
             grid=grid-grid2
             call sptez_s(spec_work_sm,grid,-ione)
             call sppad(izero,jcap,spec_work_sm,izero,jcap_b,spec_work)
             do i=1,nc_b
                temp(i,k) = temp(i,k) + spec_work(i)
                if(factvml_b(i))temp(i,k)=zero_single
             end do
        end do
!$omp end parallel do
!send sh back to mype_out
        call mpi_send(temp,nc_b*nsig,mpi_rtype4,mype_out,&
                      itag_div,mpi_comm_world,ierror)
      endif

      if (mype==mype_vort) then
!$omp parallel do private(k,grid,i,spec_work,grid2)
        do k=1,nsig
             do i=1,nc_b
                spec_work(i) = temp(i,k)
                if(factsml_b(i))spec_work(i)=zero
             end do
             call sptez_s_bkg(spec_work,grid2,ione)
             call load_grid(work1_k(1,k),grid)
             grid=grid-grid2
             call sptez_s(spec_work_sm,grid,-ione)
             call sppad(izero,jcap,spec_work_sm,izero,jcap_b,spec_work)
             do i=1,nc_b
                temp(i,k) =temp(i,k) + spec_work(i)
                if(factvml_b(i))temp(i,k)=zero_single
             end do
        end do
!$omp end parallel do
        call mpi_send(temp,nc_b*nsig,mpi_rtype4,mype_out,&
                      itag_vort,mpi_comm_world,ierror)
      endif
    endif
    
    
!   GFSIO specific output
    if (.not.ncep_sigio) then

!      Pressure depth
       do k=1,nsig
          call mpi_gatherv(dpsm(1,k),ijn(mm1),mpi_rtype,&
               work1,ijn,displs_g,mpi_rtype,&
               mype_out,mpi_comm_world,ierror)
          if (mype==mype_out) then
             call load_grid(work1,grid)
             ij=izero
             do j=1,nlatm2
                do i=1,nlon
                   ij=ij+ione
                   gfsdata%dp(ij,k)=grid(i,j)*r1000
                end do
             end do
             call gfsio_writerecv(gfileo,'dpres','layer',k,gfsdata%dp(:,k),iret,&
                  idrt=gfshead%idrt)
          endif
       end do

!      Layer mean pressure
       do k=1,nsig
          call mpi_gatherv(prslm(1,k),ijn(mm1),mpi_rtype,&
               work1,ijn,displs_g,mpi_rtype,&
               mype_out,mpi_comm_world,ierror)
          if (mype==mype_out) then
             call load_grid(work1,grid)
             ij=izero
             do j=1,nlatm2
                do i=1,nlon
                   ij=ij+ione
                   gfsdata%p(ij,k)=grid(i,j)*r1000
                end do
             end do
             call gfsio_writerecv(gfileo,'pres','layer',k,gfsdata%p(:,k),iret,&
                  idrt=gfshead%idrt)
          endif
       end do
!      Zonal wind
       do k=1,nsig
          call mpi_gatherv(usm(1,k),ijn(mm1),mpi_rtype,&
               work1,ijn,displs_g,mpi_rtype,&
               mype_out,mpi_comm_world,ierror)
          if (mype==mype_out) then
             call load_grid(work1,grid)
             ij=izero
             do j=1,nlatm2
                do i=1,nlon
                   ij=ij+ione
                   gfsdata%u(ij,k)=grid(i,j)
                end do
             end do
             call gfsio_writerecv(gfileo,'ugrd','layer',k,gfsdata%u(:,k),iret,&
                  idrt=gfshead%idrt)
          endif
       end do
!      Meridional wind
       do k=1,nsig
          call mpi_gatherv(vsm(1,k),ijn(mm1),mpi_rtype,&
               work1,ijn,displs_g,mpi_rtype,&
               mype_out,mpi_comm_world,ierror)
          if (mype==mype_out) then
             call load_grid(work1,grid)
             ij=izero
             do j=1,nlatm2
                do i=1,nlon
                   ij=ij+ione
                   gfsdata%v(ij,k)=grid(i,j)
                end do
             end do
             call gfsio_writerecv(gfileo,'vgrd','layer',k,gfsdata%v(:,k),iret,&
                  idrt=gfshead%idrt)
          endif
       end do
    endif
    

!   Single task writes analysis data to analysis file
    if (mype==mype_out) then
      if (ncep_sigio) then
!receive temperature from mype_th
        call mpi_recv(sigdata%t,nc_b*nsig,mpi_rtype4,mype_th,&
                      itag_th,mpi_comm_world,status,ierror)
!receive specific humidity from mype_sh
        call mpi_recv(sigdata%q(1,1,1),nc_b*nsig,mpi_rtype4,mype_sh,&
                      itag_sh,mpi_comm_world,status,ierror)
!receive ozone from mype_oz
        call mpi_recv(sigdata%q(1,1,2),nc_b*nsig,mpi_rtype4,mype_oz,&
                      itag_oz,mpi_comm_world,status,ierror)
!receive cloud condensate mixing ratio from mype_clc
        if (ntracer>2_i_kind .or. ncloud>=ione) then
          call mpi_recv(sigdata%q(1,1,3),nc_b*nsig,mpi_rtype4,mype_clc,&
                        itag_clc,mpi_comm_world,status,ierror)
        endif 
!receive divergence from mype_div
        call mpi_recv(sigdata%d,nc_b*nsig,mpi_rtype4,mype_div,&
                      itag_div,mpi_comm_world,status,ierror)
!receive vorticity from mype_vort
        call mpi_recv(sigdata%z,nc_b*nsig,mpi_rtype4,mype_vort,&
                      itag_vort,mpi_comm_world,status,ierror)
!
        string='sigio'
        call sigio_swdata(lunanl,sighead,sigdata,iret)
        call sigio_axdata(sigdata,iret)
      else        !GFS IO
        string='gfsio'
        call gfsio_close(gfileo,iret)
      endif
      write(6,110) string,gfshead%jcap,gfshead%latb,gfshead%lonb,&
           gfshead%levs,gfshead%fhour,gfshead%idate
110   format('WRITE_GFSATM:  NCEP ',a5,&
           ' atm anal written for jcap,latb,lonb,levs= ',4i6,&
           ' valid hour,idate= ',f3.1,4(i4,1x))
    endif

    return
  end subroutine write_gfsatm


  subroutine write_gfssfc(filename,mype,mype_sfc,dsfct)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    write_gfssfc --- Write surface analysis to file
!
!   prgrmmr:     treadon -  initial version; org: np22
!
! abstract:     This routine writes the updated surface analysis.  At
!               this point (20040615) the only surface field update by 
!               the gsi is the skin temperature.  The current (20040615)
!               GDAS setup does use the updated surface file.  Rather,
!               the output from surface cycle is used as the surface
!               analysis for subsequent GFS runs.
!
!               The routine gathers surface fields from subdomains, 
!               reformats the data records, and then writes each record
!               to the output file.  
!
!               Since the gsi only update the skin temperature, all
!               other surface fields are simply read from the guess
!               surface file and written to the analysis file.
!
!   Structure of GFS surface file  
!       data record  1    label
!       data record  2    date, dimension, version, lons/lat record
!       data record  3    tsf
!       data record  4    soilm(two layers)
!       data record  5    snow
!       data record  6    soilt(two layers)
!       data record  7    tg3
!       data record  8    zor
!       data record  9    cv
!       data record 10    cvb
!       data record 11    cvt
!       data record 12    albedo (four types)
!       data record 13    slimsk
!       data record 14    vegetation cover
!       data record 15    plantr
!       data record 16    f10m
!       data record 17    canopy water content (cnpanl)
!       data record 18    vegetation type
!       data record 19    soil type
!       data record 20    zenith angle dependent vegetation fraction (two types)
!
! program history log:
!   2004-06-15  treadon -  updated documentation
!   2004-07-15  todling -  protex-compliant prologue; added intent/only's
!   2004-12-03  treadon -  replace mpe_igatherv (IBM extension) with
!                          standard mpi_gatherv
!   2005-01-27  treadon - rewrite to make use of sfcio module
!   2005-02-09  kleist  - clean up unit number and filename for updated surface file
!   2005-03-07  todling -  die gracefully when return error from sfcio
!   2005-03-10  treadon - remove iadate from calling list, access via obsmod
!   2006-10-11  treadon - update 10m wind factor in sfc file
!   2008-05-28  safford - rm unused vars
!
!   input argument list:
!     filename  - file to open and write to
!     dsfct     - delta skin temperature
!     mype      - mpi task number
!     mype_sfc  - mpi task to write output file
!
!   output argument list:
!
! attributes:
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!$$$ end documentation block

! !USES:
    use kinds, only: r_kind,r_single,i_kind
  
    use mpimod, only: mpi_rtype
    use mpimod, only: mpi_comm_world
    use mpimod, only: ierror
    
    use gridmod, only: nlat,nlon
    use gridmod, only: lat1,lon1
    use gridmod, only: lat2,lon2
    use gridmod, only: iglobal
    use gridmod, only: ijn
    use gridmod, only: ltosi,ltosj
    use gridmod, only: displs_g
    use gridmod, only: itotsub
    
    use obsmod, only: iadate
    
    use constants, only: izero,ione,zero_single
    
    use sfcio_module, only: sfcio_intkind,sfcio_head,sfcio_data,&
         sfcio_srohdc,sfcio_swohdc,sfcio_axdata
    
    implicit none

! !INPUT PARAMETERS:
    character(24),intent(in):: filename  ! file to open and write to

    real(r_kind),dimension(lat2,lon2), intent(in) :: dsfct   ! delta skin temperature

    integer(i_kind),              intent(in)  :: mype     ! mpi task number
    integer(i_kind),              intent(in)  :: mype_sfc ! mpi task to write output file

! !OUTPUT PARAMETERS:

!-------------------------------------------------------------------------

!   Declare local parameters
    character( 6),parameter:: fname_ges='sfcf06'
    integer(sfcio_intkind),parameter:: ioges = 12
    integer(sfcio_intkind),parameter:: ioanl = 52

    real(r_kind),parameter :: houra = zero_single

!   Declare local variables
    integer(sfcio_intkind):: iret
    integer(i_kind) latb,lonb,nlatm2
    integer(i_kind) i,j,ip1,jp1,ilat,ilon,jj,mm1

    real(r_kind),dimension(nlon,nlat):: buffer
    real(r_kind),dimension(lat1,lon1):: sfcsub
    real(r_kind),dimension(nlon,nlat):: grid
    real(r_kind),dimension(max(iglobal,itotsub)):: sfcall
    real(r_single),allocatable,dimension(:,:):: buffer2

    type(sfcio_head):: head
    type(sfcio_data):: data

  
!*****************************************************************************

!   Initialize local variables
    mm1=mype+ione
    nlatm2=nlat-2_i_kind

!   Gather skin temperature information from all tasks.  
    do j=1,lon1
       jp1 = j+ione
       do i=1,lat1
          ip1 = i+ione
          sfcsub(i,j)=dsfct(ip1,jp1)
       end do
    end do
    call mpi_gatherv(sfcsub,ijn(mm1),mpi_rtype,&
         sfcall,ijn,displs_g,mpi_rtype,mype_sfc,&
         mpi_comm_world,ierror)

!   Only MPI task mype_sfc writes the surface file.
    if (mype==mype_sfc) then

!      Reorder updated skin temperature to output format
       do i=1,iglobal
          ilon=ltosj(i)
          ilat=ltosi(i)
          grid(ilon,ilat)=sfcall(i)
       end do
       do j=1,nlat
          jj=nlat-j+ione
          do i=1,nlon
             buffer(i,j)=grid(i,jj)
          end do
       end do

!      For now, rather than carry around all the surface fields in memory from
!      the read in ingesfc, just read fields from surface file.  Also, for
!      now, only update the 6-hour forecast surface guess file.

!      Read surface guess file
       call sfcio_srohdc(ioges,fname_ges,head,data,iret)
       if (iret /= izero) then
          write(6,*)'WRITE_GFSSFC:  ***ERROR*** problem reading ',fname_ges,&
               ', iret=',iret
          call sfcio_axdata(data,iret)
          call stop2(80)
       endif
       latb=head%latb
       lonb=head%lonb
       allocate(buffer2(lonb,latb))
       if ( (latb /= nlatm2) .or. &
            (lonb /= nlon) ) then
          write(6,*)'WRITE_GFSSFC:  different grid dimensions analysis vs sfc. interpolating sfc temperature  ',&
               ', nlon,nlat-2=',nlon,nlatm2,' -vs- sfc file lonb,latb=',&
               lonb,latb
          call sfc_interpolate(buffer,nlon,nlat,buffer2,lonb,latb)
       else
          do j=1,latb
            do i=1,lonb
              buffer2(i,j)=buffer(i,j+ione)
            end do
          end do
       endif

!      Update guess date/time to analysis date/time
       head%fhour = houra       ! forecast hour
       head%idate(1)=iadate(4)  ! hour
       head%idate(2)=iadate(2)  ! month
       head%idate(3)=iadate(3)  ! day
       head%idate(4)=iadate(1)  ! year


       do j=1,latb
          do i=1,lonb
             data%tsea(i,j) = data%tsea(i,j)+buffer2(i,j)
          end do
       end do
       deallocate(buffer2)

!      Write updated information to surface analysis file
       call sfcio_swohdc(ioanl,filename,head,data,iret)


!      Deallocate local work arrays
       call sfcio_axdata(data,iret)

       write(6,100) lonb,latb,houra,iadate(1:4),iret
100    format(' WRITE_GFSSFC:  sfc analysis written  for ',&
            2i6,1x,f3.1,4(i4,1x),' with iret=',i2)

    endif
    
!   End of routine
    return
  end subroutine write_gfssfc


  subroutine reorder_gfsgrib(nx,ny,grid_1d,grid_2d)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    reorder_gfsgrib --- transfer gfsgrib data 1d <--> 2d
!
!   prgrmmr:     treadon -  initial version; org: np23
!
! abstract:      This routine transfers the contents of gfs grib arrays
!                between 1d and 2d.
!
! program history log:
!   2007-04-30  treadon -  original routine
!   2008-05-28  safford -- add subprogram doc block
!
!   input argument list:
!     nx        - number of grid points in zonal direction 
!     ny        - number of grid points in meridional direction
!     grid_1d   - 1d array
!
!   output argument list:
!     grid_2d   - 2d array
!
! attributes:
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!$$$ end documentation block

! !USES:
    use kinds, only: r_kind,i_kind
    use constants, only: izero,ione
    implicit none
! !INPUT PARAMETERS:
    integer(i_kind),intent(in):: nx      ! number of grid points in zonal direction 
    integer(i_kind),intent(in):: ny      ! number of grid points in meridional direction

    real(r_kind),dimension(nx*ny), intent(in)::  grid_1d   ! 1d array

! !OUTPUT PARAMETERS:
    real(r_kind),dimension(nx,ny), intent(out):: grid_2d   ! 2d array

!-------------------------------------------------------------------------

!   Declare local variables
    integer(i_kind) i,j,ij

!*****************************************************************************

!   Loop to transfer array contents
    ij=izero
    do j=1,ny
       do i=1,nx
          ij=ij+ione
          grid_2d(i,j)=grid_1d(ij)
       end do
    end do

    
!   End of routine
    return
  end subroutine reorder_gfsgrib


  subroutine sfc_interpolate(a,na_lon,na_lat,b,ns_lon,ns_lat)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    sfc_interpolate --- interpolates from analysis grid to 
!                                    surface grid
!   prgrmmr:     derber -  initial version; org: np2
!
! abstract:      This routine interpolates a on analysis grid to b on 
!                surface grid
!
! program history log:
!   2008-02-26  derber  - original routine
!   2008-05-28  safford - add subprogram doc block, rm unused uses
!
!   input argument list:
!     na_lon  - number of longitude grid analysis 
!     na_lat  - number of latitude grid analysis
!     ns_lon  - number of longitude grid sfc 
!     ns_lat  - number of latitude grid sfc
!     a       - analysis values
!
!   output argument list:
!     b       - surface values
!
! attributes:
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!$$$ end documentation block

! !USES:
    use kinds, only: r_kind,i_kind,r_single
    use constants, only: izero,ione,zero,one
    use gridmod, only: rlats,rlons,rlats_sfc,rlons_sfc
    
    implicit none

! !INPUT PARAMETERS:
    integer(i_kind),intent(in):: na_lon  ! number of longitude grid analysis 
    integer(i_kind),intent(in):: na_lat  ! number of latitude grid analysis
    integer(i_kind),intent(in):: ns_lon  ! number of longitude grid sfc 
    integer(i_kind),intent(in):: ns_lat  ! number of latitude grid sfc

    real(r_kind),dimension(na_lon,na_lat), intent(in)::  a   ! analysis values

! !OUTPUT PARAMETERS:
    real(r_single),dimension(ns_lon,ns_lat), intent(out):: b   ! surface values


!   Declare local variables
    integer(i_kind) i,j,ix,iy,ixp,iyp
    real(r_kind) dx1,dy1,dx,dy,w00,w01,w10,w11,bout,dlat,dlon

!*****************************************************************************

    b=zero
!   Loop over all points to get interpolated value
    do j=1,ns_lat
       dlat=rlats_sfc(j)
       call grdcrd(dlat,ione,rlats,na_lat,ione)
       iy=int(dlat)
       iy=min(max(ione,iy),na_lat)
       dy  =dlat-iy
       dy1 =one-dy
       iyp=min(na_lat,iy+ione)


       do i=1,ns_lon
         dlon=rlons_sfc(i)
         call grdcrd(dlon,ione,rlons,na_lon,ione)
         ix=int(dlon)
         dx  =dlon-ix
         dx=max(zero,min(dx,one))
         dx1 =one-dx
         w00=dx1*dy1; w10=dx1*dy; w01=dx*dy1; w11=dx*dy

         ix=min(max(izero,ix),na_lon)
         ixp=ix+ione
         if(ix==izero) ix=na_lon
         if(ixp==na_lon+ione) ixp=ione
         bout=w00*a(ix,iy)+w01*a(ix,iyp)+w10*a(ixp,iy)+w11*a(ixp,iyp)
         b(i,j)=bout

       end do
    end do

    
!   End of routine
    return
  end subroutine sfc_interpolate


!-------------------------------------------------------------------------------
  subroutine sigio_cnvtdv8(im,ix,km,idvc,idvm,ntrac,iret,t,q,cpi,cnflg)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    sigio_cnvtdv8
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-05-28  safford -- add subprogram doc block
!
!   input argument list:
!     im,ix,km,idvc,idvm,ntrac,cnflg
!     q, cpi
!     t
!
!   output argument list:
!     iret
!     t
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    use kinds, only: i_kind,r_kind
    use constants, only: izero,zero,one,fv
    implicit none
    integer(i_kind),intent(in):: im,ix,km,idvc,idvm,ntrac,cnflg
    integer(i_kind),intent(out):: iret
    real(r_kind),intent(in)          :: q(ix,km,ntrac), cpi(0:ntrac)
    real(r_kind),intent(inout)       :: t(ix,km)
    integer(i_kind)                  :: thermodyn_id, n
    real(r_kind)                     :: xcp(ix,km), sumq(ix,km)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=izero
    thermodyn_id = mod(IDVM/10,10)
!
    if (thermodyn_id == 3_i_kind .and. idvc == 3_i_kind) then
      xcp(1:im,:)  = zero
      sumq(1:im,:) = zero
      do n=1,NTRAC
        if( cpi(n) /= zero) then
           xcp(1:im,:)  = xcp(1:im,:)  + q(1:im,:,n) * cpi(n)
           sumq(1:im,:) = sumq(1:im,:) + q(1:im,:,n)
        endif
      enddo
      xcp(1:im,:)  = (one-sumq(1:im,:))*cpi(0) + xcp(1:im,:)   ! Mean Cp
!
    else
      xcp(1:im,:) = one + fv*Q(1:im,:,1)        ! Virt factor
    endif
    if (cnflg > izero) then
      t(1:im,:) = t(1:im,:) / xcp(1:im,:)
    else
      t(1:im,:) = t(1:im,:) * xcp(1:im,:)
    endif
!
    return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine sigio_cnvtdv8


end module ncepgfs_io
