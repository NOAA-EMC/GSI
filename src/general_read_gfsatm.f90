subroutine general_read_gfsatm(grd,sp,filename,mype,g_z,g_ps,g_vor,g_div,g_u,g_v,&
       g_tv,g_q,g_cwmr,g_oz,iret_read)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_read_gfsatm  adaptation of read_gfsatm for general resolutions
!   prgmmr: parrish          org: np22                date: 1990-10-10
!
! abstract: copied from read_gfsatm, primarily for reading in gefs sigma files, where the
!            input resolution and the grid that variables are reconstructed on can be
!            different from the analysis grid/resolution.
!
! program history log:
!   2010-02-25  parrish
!
!   input argument list:
!     grd      - structure variable containing information about grid
!                    (initialized by general_sub2grid_create_info, located in general_sub2grid_mod.f90)
!     sp       - structure variable containing spectral information
!                    (initialized by general_init_spec_vars, located in general_specmod.f90)
!     filename - input sigma file name
!     mype     - mpi task id
!
!   output argument list:
!     g_*      - guess fields
!     iret_read - return code, 0 for successful read.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: r_kind,r_single,i_kind
    use gridmod, only: &
         ncep_sigio,ncepgfs_head,idpsfc5,idthrm5,&
         ntracer,idvc5,cp5,idvm5
    use general_sub2grid_mod, only: sub2grid_info
    use general_specmod, only: spec_vars
    use mpimod, only: npe,mpi_comm_world,ierror,mpi_rtype
    use constants, only: izero,ione,zero,one,fv,r0_01
    use sigio_module, only: sigio_intkind,sigio_head,sigio_data,&
         sigio_srohdc,sigio_axdata
    use gfsio_module, only: gfsio_gfile,gfsio_open,gfsio_close,&
       gfsio_init,gfsio_getfilehead,gfsio_readrecv
    use ncepgfs_io, only: sigio_cnvtdv8

    implicit none
    
!   Declare local parameters
    integer(sigio_intkind):: lunges = 11
    real(r_kind),parameter:: r0_001 = 0.001_r_kind
    real(r_kind),parameter:: qsmall = 1.e-11_r_kind

!   Declare passed variables
    type(sub2grid_info)                   ,intent(in   ) :: grd
    type(spec_vars)                       ,intent(in   ) :: sp
    character(24)                         ,intent(in   ) :: filename
    integer(i_kind)                       ,intent(in   ) :: mype
    integer(i_kind)                       ,intent(  out) :: iret_read
    real(r_kind),dimension(grd%lat2,grd%lon2)     ,intent(  out) :: g_z,g_ps
    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig),intent(  out) :: g_u,g_v,&
         g_vor,g_div,g_cwmr,g_q,g_oz,g_tv
    
!   Declare local variables
    integer(i_kind):: iret,nlatm2,ij,n,ii1,l,m
    integer(i_kind) i,j,k,icount,icount_prev,mm1
    integer(i_kind) mype_hs,mype_ps
    real(r_kind),dimension(grd%nlon,grd%nlat-2_i_kind):: grid,grid_u,grid_v,&
         grid_vor,grid_div,grid2,grid3
    real(r_kind),dimension(grd%nlon,grd%nlat-2_i_kind,ntracer):: grid_q
    real(r_kind),dimension(sp%nc):: spec_work,spec_vor,spec_div
    real(r_kind),dimension(grd%itotsub):: work,work_vor,work_div,&
         work_u,work_v
    real(r_kind),dimension(grd%lat2*grd%lon2,max(2*grd%nsig,npe)):: sub,sub_div,sub_vor,&
         sub_u,sub_v
    real(r_kind),dimension(grd%nlat-2_i_kind):: qlatmean
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
    nlatm2=grd%nlat-2_i_kind
    rnlon=one/float(grd%nlon)


!   Read NCEP gfs guess file using appropriate io module !NOTE: for now only allow ncep_sigio.
  ! if (ncep_sigio) then
       call sigio_srohdc(lunges,filename,sighead,sigdata,iret)
       gfshead%fhour   = sighead%fhour
       gfshead%idate   = sighead%idate
       gfshead%levs    = sighead%levs
       gfshead%ntrac   = sighead%ntrac
       gfshead%ncldt   = sighead%ncldt
       gfshead%lonb    = grd%nlon
       gfshead%latb    = nlatm2


  ! else

  !    call gfsio_init(iret)
  !    call gfsio_open(gfile,filename,'read',iret)
  !    call gfsio_getfilehead(gfile,iret=iret, &
  !         fhour=gfshead%fhour, &
  !         idate=gfshead%idate, &
  !         levs=gfshead%levs, &
  !         ntrac=gfshead%ntrac, &
  !         ncldt=gfshead%ncldt, &
  !         lonb=gfshead%lonb, &
  !         latb=gfshead%latb)

  !    allocate(gfsdata%hs(gfshead%latb*gfshead%lonb))
  !    allocate(gfsdata%ps(gfshead%latb*gfshead%lonb))
  !    allocate(gfsdata%t(gfshead%latb*gfshead%lonb,gfshead%levs))
  !    allocate(gfsdata%u(gfshead%latb*gfshead%lonb,gfshead%levs))
  !    allocate(gfsdata%v(gfshead%latb*gfshead%lonb,gfshead%levs))
  !    allocate(gfsdata%q(gfshead%latb*gfshead%lonb,gfshead%levs,gfshead%ntrac))
       
  !    call gfsio_readrecv(gfile,'hgt','sfc',ione,gfsdata%hs,iret)
  !    iret_read=iret_read+iret

  !    call gfsio_readrecv(gfile,'pres','sfc',ione,gfsdata%ps,iret)
  !    iret_read=iret_read+iret

  !    do k=1,gfshead%levs
  !       call gfsio_readrecv(gfile,'tmp','layer',k,gfsdata%t(:,k),iret)
  !       iret_read=iret_read+iret
  !    enddo

  !    do k=1,gfshead%levs
  !       call gfsio_readrecv(gfile,'ugrd','layer',k,gfsdata%u(:,k),iret)
  !       iret_read=iret_read+iret
  !    enddo

  !    do k=1,gfshead%levs
  !       call gfsio_readrecv(gfile,'vgrd','layer',k,gfsdata%v(:,k),iret)
  !       iret_read=iret_read+iret
  !    enddo

  !    do k=1,gfshead%levs
  !       call gfsio_readrecv(gfile,'spfh','layer',k,gfsdata%q(:,k,1),iret)
  !       iret_read=iret_read+iret
  !    enddo

  !    do k=1,gfshead%levs
  !       call gfsio_readrecv(gfile,'o3mr','layer',k,gfsdata%q(:,k,2),iret)
  !       iret_read=iret_read+iret
  !    enddo

  !    do k=1,gfshead%levs
  !       call gfsio_readrecv(gfile,'clwmr','layer',k,gfsdata%q(:,k,3),iret)
  !       iret_read=iret_read+iret
  !    enddo

  !    call gfsio_close(gfile,iret)
  !    iret_read=iret_read+iret
  ! endif

    if (iret_read /= izero) goto 1000


!   Process guess fields according to type of input file.   NCEP_SIGIO files
!   are spectral coefficient files and need to be transformed to the grid.
!   NCEP_GFSIO files are grid files.    Both formats need to be scattered 
!   from the full domain to sub-domains.

!   Terrain:  spectral --> grid transform, scatter to all mpi tasks
    if (mype==mype_hs) then
      !if (ncep_sigio) then
          do i=1,sp%nc
             spec_work(i)=sp%test_mask(i)*sigdata%hs(i)
             if(sp%factsml(i))spec_work(i)=zero
          end do
          call general_sptez_s(sp,spec_work,grid,ione)
      !else
      !   ij=izero
      !   do j=1,gfshead%latb
      !      do i=1,gfshead%lonb
      !         ij=ij+ione
      !       grid(i,j)=gfsdata%hs(ij)
      !      end do
      !   end do
      !endif
       call general_fill_ns(grd,grid,work)
    endif
    call mpi_scatterv(work,grd%ijn_s,grd%displs_s,mpi_rtype,&
         g_z,grd%ijn_s(mm1),mpi_rtype,mype_hs,mpi_comm_world,ierror)


!   Surface pressure:  same procedure as terrain, but handled by task mype_ps
!   NCEP SIGIO has two options for surface pressure.  Variable idpsfc5 
!   indicates the type:   
!      idpsfc5= 0,1 for ln(psfc)
!      idpsfc5= 2 for psfc
!   
    if (mype==mype_ps) then
     ! if (ncep_sigio) then
          do i=1,sp%nc
             spec_work(i)=sp%test_mask(i)*sigdata%ps(i)
             if(sp%factsml(i))spec_work(i)=zero
          end do
          call general_sptez_s(sp,spec_work,grid,ione)
          call general_fill_ns(grd,grid,work)

!         If ln(ps), take exponential to convert to ps in cb
          if (idpsfc5 /= 2_i_kind) then
             do i=1,grd%itotsub
                work(i)=exp(work(i))
             end do
          endif

     ! else
     !    ij=izero
     !    do j=1,gfshead%latb
     !       do i=1,gfshead%lonb
     !          ij=ij+ione
     !          grid(i,j) = r0_001*gfsdata%ps(ij)  ! convert Pa to cb
     !       end do
     !    end do
     !    call general_fill_ns(grd,grid,work)
     ! endif
    endif
    call mpi_scatterv(work,grd%ijn_s,grd%displs_s,mpi_rtype,&
         g_ps,grd%ijn_s(mm1),mpi_rtype,mype_ps,mpi_comm_world,ierror)
    
    
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
         !if (ncep_sigio) then
             do i=1,sp%nc
                spec_work(i)=sp%test_mask(i)*sigdata%t(i,k)
                if(sp%factsml(i))spec_work(i)=zero
             end do
             call general_sptez_s(sp,spec_work,grid,ione)

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
                   do i=1,sp%nc
                      spec_work(i)=sp%test_mask(i)*sigdata%q(i,k,n)
                      if(sp%factsml(i))spec_work(i)=zero
                   end do
                   call general_sptez_s(sp,spec_work,grid,ione)
                end do

!               Convert input thermodynamic variable to dry temperature
                call sigio_cnvtdv8(grd%nlon*nlatm2,grd%nlon*nlatm2,ione,idvc5,&
                     idvm5,ntracer,iret,grid,grid_q,cp5,ione)

!               Convert dry temperature to virtual
                do j=1,nlatm2
                   do i=1,grd%nlon
                      grid(i,j) = grid(i,j)*(one+fv*grid_q(i,j,1))
                   end do
                end do

             endif

         !else
!        !   GFSIO thermodynamic variable is sensible temperature (T).
!        !   The GSI analysis variable is virtual temperature (Tv).   
!        !   Convert T to Tv in the loop below.
         !   ij=izero
         !   do j=1,gfshead%latb
         !      do i=1,gfshead%lonb
         !         ij=ij+ione
         !         grid(i,j)=gfsdata%t(ij,k)*(one+fv*gfsdata%q(ij,k,1))
         !      end do
         !   end do
         !endif

!         Load values into rows for south and north pole
          call general_fill_ns(grd,grid,work)
       endif

       if (mod(icount,npe)==izero .or. icount==gfshead%levs) then
          call mpi_alltoallv(work,grd%ijn_s,grd%displs_s,mpi_rtype,&
               sub(1,icount_prev),grd%irc_s,grd%ird_s,mpi_rtype,&
               mpi_comm_world,ierror)
          icount_prev=icount+ione
       endif
    end do
    call general_reload(grd,sub,g_tv)



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
         !if (ncep_sigio) then
             do i=1,sp%nc
                spec_div(i)=sp%test_mask(i)*sigdata%d(i,k)   !div
                spec_vor(i)=sp%test_mask(i)*sigdata%z(i,k)   !vor
                if(sp%factvml(i))then
                   spec_div(i)=zero
                   spec_vor(i)=zero
                end if
             end do
             call general_sptez_s(sp,spec_div,grid_div,ione)
             call general_sptez_s(sp,spec_vor,grid_vor,ione)
             call general_sptez_v(sp,spec_div,spec_vor,grid_u,grid_v,ione)


!         Convert grid u,v to div and vor
         !else
         !   ij=izero
         !   do j=1,gfshead%latb
         !      do i=1,gfshead%lonb
         !         ij=ij+ione
         !         grid_u(i,j)=gfsdata%u(ij,k)
         !         grid_v(i,j)=gfsdata%v(ij,k)
         !      end do
         !   end do
         !   if (hires_b) then
         !      call sptez_v_b(spec_div,spec_vor,grid_u,grid_v,-ione)
         !      call sptez_s_b(spec_div,grid_div,ione)
         !      call sptez_s_b(spec_vor,grid_vor,ione)
         !   else
         !      call sptez_v(spec_div,spec_vor,grid_u,grid_v,-ione)
         !      call sptez_s(spec_div,grid_div,ione)
         !      call sptez_s(spec_vor,grid_vor,ione)
         !   endif
         !endif
          
          call general_fill_ns(grd,grid_div,work_div)
          call general_fill_ns(grd,grid_vor,work_vor)
          call general_filluv_ns(grd,sp,grid_u,grid_v,work_u,work_v)
          
       endif

!      Periodically exchange vor,div,u,v between all mpi tasks.
       if (mod(icount,npe)==izero .or. icount==gfshead%levs) then
          call mpi_alltoallv(work_vor,grd%ijn_s,grd%displs_s,mpi_rtype,&
               sub_vor(1,icount_prev),grd%irc_s,grd%ird_s,mpi_rtype,&
               mpi_comm_world,ierror)
          call mpi_alltoallv(work_div,grd%ijn_s,grd%displs_s,mpi_rtype,&
               sub_div(1,icount_prev),grd%irc_s,grd%ird_s,mpi_rtype,&
               mpi_comm_world,ierror)
          call mpi_alltoallv(work_u,grd%ijn_s,grd%displs_s,mpi_rtype,&
               sub_u(1,icount_prev),grd%irc_s,grd%ird_s,mpi_rtype,&
               mpi_comm_world,ierror)
          call mpi_alltoallv(work_v,grd%ijn_s,grd%displs_s,mpi_rtype,&
               sub_v(1,icount_prev),grd%irc_s,grd%ird_s,mpi_rtype,&
               mpi_comm_world,ierror)
          icount_prev=icount+ione
       endif
    end do
    
!   Transfer vor,div,u,v into real(r_kind) guess arrays
    call general_reload(grd,sub_vor,g_vor)
    call general_reload(grd,sub_div,g_div)
    call general_reload(grd,sub_u,g_u)
    call general_reload(grd,sub_v,g_v)


!   Specific humidity
    sub=zero
    icount=izero
    icount_prev=ione
    do k=1,gfshead%levs
       icount=icount+ione
       if (mype==mod(icount-ione,npe)) then
         !if (ncep_sigio) then
             do i=1,sp%nc
                spec_work(i)=sp%test_mask(i)*sigdata%q(i,k,1)
                if(sp%factsml(i))spec_work(i)=zero
             end do
             call general_sptez_s(sp,spec_work,grid,ione)
         !else
         !   ij=izero
         !   do j=1,gfshead%latb
         !      do i=1,gfshead%lonb
         !         ij=ij+ione
         !         grid(i,j)=gfsdata%q(ij,k,1)
         !      end do
         !   end do
         !endif

!         Adjust zonal mean specific humidity
          qminlev= 999.0_r_kind
          qmaxlev=-999.0_r_kind
          do j=1,nlatm2
             qlatmean(j)=zero
             do i=1,grd%nlon
                qlatmean(j)=qlatmean(j)+grid(i,j)
             end do
             qlatmean(j)=qlatmean(j)*rnlon-1.e-9_r_kind
             qminlev(1)=min(qlatmean(j),qminlev(1))
             qmaxlev(1)=max(qlatmean(j),qmaxlev(1))
          end do
          if(qminlev(1) < 1.e-9_r_kind)then
             do j=1,nlatm2
                do i=1,grd%nlon
                   grid2(i,j)=zero
                   grid3(i,j)=grid(i,j)
                   if (grid(i,j)<qsmall) grid2(i,j)=qsmall-grid(i,j)
                end do
             end do
             call general_sptez_s(sp,spec_work,grid2,-ione)
             ii1=izero
             do l=izero,sp%jcap
                do m=izero,sp%jcap-l
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
             call general_sptez_s(sp,spec_work,grid2,ione)
             do j=1,nlatm2
                do i=1,grd%nlon
                   grid3(i,j)=grid3(i,j)+grid2(i,j)
                end do
             end do
             do j=1,nlatm2
                qlatmean(j)=zero
                do i=1,grd%nlon
                   qlatmean(j)=qlatmean(j)+grid3(i,j)
                end do
                qlatmean(j)=qlatmean(j)*rnlon-1.e-9_r_kind
                qminlev(2)=min(qlatmean(j),qminlev(2))
                qmaxlev(2)=max(qlatmean(j),qmaxlev(2))
             end do
             write(6,*) 'READ_GFSATM:  k,qminlev12,qmaxlev12 ',k,qminlev(1),&
                  qminlev(2),qmaxlev(1),qmaxlev(2),' ***DIAG ONLY***'
          end if

          call general_fill_ns(grd,grid,work)
       endif
       if (mod(icount,npe)==izero .or. icount==gfshead%levs) then
          call mpi_alltoallv(work,grd%ijn_s,grd%displs_s,mpi_rtype,&
               sub(1,icount_prev),grd%irc_s,grd%ird_s,mpi_rtype,&
               mpi_comm_world,ierror)
          icount_prev=icount+ione
       endif
    end do
    call general_reload(grd,sub,g_q)


!   Ozone mixing ratio
    sub=zero
    icount=izero
    icount_prev=ione
    do k=1,gfshead%levs
       icount=icount+ione
       if (mype==mod(icount-ione,npe)) then
         !if (ncep_sigio) then
             do i=1,sp%nc
                spec_work(i)=sp%test_mask(i)*sigdata%q(i,k,2)
                if(sp%factsml(i))spec_work(i)=zero
             end do
             call general_sptez_s(sp,spec_work,grid,ione)
         !else
         !   ij=izero
         !   do j=1,gfshead%latb
         !      do i=1,gfshead%lonb
         !         ij=ij+ione
         !         grid(i,j)=gfsdata%q(ij,k,2)
         !      end do
         !   end do
         !endif
          call general_fill_ns(grd,grid,work)
       endif
       if (mod(icount,npe)==izero .or. icount==gfshead%levs) then
          call mpi_alltoallv(work,grd%ijn_s,grd%displs_s,mpi_rtype,&
               sub(1,icount_prev),grd%irc_s,grd%ird_s,mpi_rtype,&
               mpi_comm_world,ierror)
          icount_prev=icount+ione
       endif
    end do
    call general_reload(grd,sub,g_oz)
    

!   Cloud condensate mixing ratio.
    if (gfshead%ntrac>2_i_kind .or. gfshead%ncldt>=ione) then
       sub=zero
       icount=izero
       icount_prev=ione
       do k=1,gfshead%levs
          icount=icount+ione
          if (mype==mod(icount-ione,npe)) then
            !if (ncep_sigio) then
                do i=1,sp%nc
                   spec_work(i)=sp%test_mask(i)*sigdata%q(i,k,3)
                   if(sp%factsml(i))spec_work(i)=zero
                end do
                call general_sptez_s(sp,spec_work,grid,ione)
            !else
            !   ij=izero
            !   do j=1,gfshead%latb
            !      do i=1,gfshead%lonb
            !         ij=ij+ione
            !         grid(i,j)=gfsdata%q(ij,k,3)
            !      end do
            !   end do
            !endif
             call general_fill_ns(grd,grid,work)
          endif
          if (mod(icount,npe)==izero .or. icount==gfshead%levs) then
             call mpi_alltoallv(work,grd%ijn_s,grd%displs_s,mpi_rtype,&
                  sub(1,icount_prev),grd%irc_s,grd%ird_s,mpi_rtype,&
                  mpi_comm_world,ierror)
             icount_prev=icount+ione
          endif
       end do
       call general_reload(grd,sub,g_cwmr)
    else
       do k=1,gfshead%levs
          do j=1,grd%lon2
             do i=1,grd%lat2
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
end subroutine general_read_gfsatm

subroutine general_reload(grd,work_in,work_out)

! !USES:

  use kinds, only: r_kind,i_kind
  use constants, only: izero,ione
  use general_sub2grid_mod, only: sub2grid_info
  implicit none

! !INPUT PARAMETERS:

  type(sub2grid_info)                   ,intent(in   ) :: grd
  real(r_kind),dimension(grd%lat2*grd%lon2,grd%nsig),intent(in   ) :: work_in   ! 2-d array

! !OUTPUT PARAMETERS:

  real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig),intent(  out) :: work_out  ! 3-d array

! !DESCRIPTION: Transfer contents of 2-d array to 3-d array
!
! !REVISION HISTORY:
!   2004-05-14  treadon
!   2004-07-15  todling, protex-compliant prologue
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR:
!   treadon          org: np23                date: 2004-05-14
!
!EOP
!-------------------------------------------------------------------------

  integer(i_kind) i,j,k,ij

  do k=1,grd%nsig
     ij=izero
     do j=1,grd%lon2
        do i=1,grd%lat2
           ij=ij+ione
           work_out(i,j,k)=work_in(ij,k)
        end do
     end do
  end do
  return
end subroutine general_reload

 subroutine general_fill_ns(grd,grid_in,grid_out)

! !USES:

   use kinds, only: r_kind,i_kind
   use constants, only: ione,zero,one
   use general_sub2grid_mod, only: sub2grid_info
   implicit none

! !INPUT PARAMETERS:

   type(sub2grid_info)                   ,intent(in   ) :: grd
   real(r_kind),dimension(grd%nlon,grd%nlat-2_i_kind),intent(in   ) :: grid_in  ! input grid
   real(r_kind),dimension(grd%itotsub)           ,intent(  out) :: grid_out ! output grid

! !DESCRIPTION: This routine adds a southern and northern latitude
!               row to the input grid.  The southern row contains
!               the longitudinal mean of the adjacent latitude row.
!               The northern row contains the longitudinal mean of
!               the adjacent northern row.
!
!               The added rows correpsond to the south and north poles.
!
!               In addition to adding latitude rows corresponding to the
!               south and north poles, the routine reorder the output
!               array so that it is a one-dimensional array read in
!               an order consisten with that assumed for total domain
!               gsi grids.
!
!               The assumed order for the input grid is longitude as
!               the first dimension with array index increasing from
!               east to west.  The second dimension is latitude with
!               the index increasing from north to south.  This ordering
!               differs from that used in the GSI.
!
!               The GSI ordering is latitude first with the index
!               increasing from south to north.  The second dimension is
!               longitude with the index increasing from east to west.
!
!               Thus, the code below also rearranges the indexing and
!               order of the dimensions to make the output grid
!               consistent with that which is expected in the rest of
!               gsi.
!
!
! !REVISION HISTORY:
!   2004-08-27  treadon
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000
!
! !AUTHOR:
!   treadon          org: np23                date: 2004-08-27
!
!EOP
!-------------------------------------------------------------------------
!  Declare local variables
   integer(i_kind) i,j,k,jj,nlatm2
   real(r_kind) rnlon,sumn,sums
   real(r_kind),dimension(grd%nlon,grd%nlat):: grid

!  Transfer contents of input grid to local work array
!  Reverse ordering in j direction from n-->s to s-->n
   do j=2,grd%nlat-ione
      jj=grd%nlat-j
      do i=1,grd%nlon
         grid(i,j)=grid_in(i,jj)
      end do
   end do

!  Compute mean along southern and northern latitudes
   sumn=zero
   sums=zero
   nlatm2=grd%nlat-2_i_kind
   do i=1,grd%nlon
      sumn=sumn+grid_in(i,1)
      sums=sums+grid_in(i,nlatm2)
   end do
   rnlon=one/float(grd%nlon)
   sumn=sumn*rnlon
   sums=sums*rnlon

!  Load means into local work array
   do i=1,grd%nlon
      grid(i,1)   =sums
      grid(i,grd%nlat)=sumn
   end do

!  Transfer local work array to output grid
   do k=1,grd%itotsub
      i=grd%ltosi_s(k)
      j=grd%ltosj_s(k)
      grid_out(k)=grid(j,i)
   end do

   return
 end subroutine general_fill_ns

 subroutine general_filluv_ns(grd,sp,gridu_in,gridv_in,gridu_out,gridv_out)

! !USES:

   use kinds, only: r_kind,i_kind
   use constants, only: ione,zero
   use general_sub2grid_mod, only: sub2grid_info
   use general_specmod, only: spec_vars
   implicit none

! !INPUT PARAMETERS:

   type(sub2grid_info)                   ,intent(in   ) :: grd
   type(spec_vars)                       ,intent(in   ) :: sp
   real(r_kind),dimension(grd%nlon,grd%nlat-2_i_kind),intent(in   ) :: gridu_in,gridv_in   ! input grid
   real(r_kind),dimension(grd%itotsub)           ,intent(  out) :: gridu_out,gridv_out ! output grid

! !DESCRIPTION: This routine adds a southern and northern latitude
!               row to the input grid.  The southern row contains
!               the longitudinal mean of the adjacent latitude row.
!               The northern row contains the longitudinal mean of
!               the adjacent northern row.
!
!               The added rows correpsond to the south and north poles.
!
!               In addition to adding latitude rows corresponding to the
!               south and north poles, the routine reorder the output
!               array so that it is a one-dimensional array read in
!               an order consisten with that assumed for total domain
!               gsi grids.
!
!               The assumed order for the input grid is longitude as
!               the first dimension with array index increasing from
!               east to west.  The second dimension is latitude with
!               the index increasing from north to south.  This ordering
!               differs from that used in the GSI.
!
!               The GSI ordering is latitude first with the index
!               increasing from south to north.  The second dimension is
!               longitude with the index increasing from east to west.
!
!               Thus, the code below also rearranges the indexing and
!               order of the dimensions to make the output grid
!               consistent with that which is expected in the rest of
!               gsi.
!
!
! !REVISION HISTORY:
!   2004-08-27  treadon
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000
!
! !AUTHOR:
!   treadon          org: np23                date: 2004-08-27
!
!EOP
!-------------------------------------------------------------------------
!  Declare local variables
   integer(i_kind) i,j,k,jj
   real(r_kind) polnu,polnv,polsu,polsv
   real(r_kind),dimension(grd%nlon,grd%nlat):: grid,grid2

!  Transfer contents of input grid to local work array
!  Reverse ordering in j direction from n-->s to s-->n
   do j=2,grd%nlat-ione
      jj=grd%nlat-j
      do i=1,grd%nlon
         grid(i,j)=gridu_in(i,jj)
         grid2(i,j)=gridv_in(i,jj)
      end do
   end do

!  Compute mean along southern and northern latitudes
   polnu=zero
   polnv=zero
   polsu=zero
   polsv=zero
   do i=1,grd%nlon
      polnu=polnu+grid(i,grd%nlat-ione)*sp%clons(i)-grid2(i,grd%nlat-ione)*sp%slons(i)
      polnv=polnv+grid(i,grd%nlat-ione)*sp%slons(i)+grid2(i,grd%nlat-ione)*sp%clons(i)
      polsu=polsu+grid(i,2        )*sp%clons(i)+grid2(i,2        )*sp%slons(i)
      polsv=polsv+grid(i,2        )*sp%slons(i)-grid2(i,2        )*sp%clons(i)
   end do
   polnu=polnu/float(grd%nlon)
   polnv=polnv/float(grd%nlon)
   polsu=polsu/float(grd%nlon)
   polsv=polsv/float(grd%nlon)
   do i=1,grd%nlon
      grid (i,grd%nlat)= polnu*sp%clons(i)+polnv*sp%slons(i)
      grid2(i,grd%nlat)=-polnu*sp%slons(i)+polnv*sp%clons(i)
      grid (i,1   )= polsu*sp%clons(i)+polsv*sp%slons(i)
      grid2(i,1   )= polsu*sp%slons(i)-polsv*sp%clons(i)
   end do

!  Transfer local work array to output grid
   do k=1,grd%itotsub
      i=grd%ltosi_s(k)
      j=grd%ltosj_s(k)
      gridu_out(k)=grid(j,i)
      gridv_out(k)=grid2(j,i)
   end do

   return
 end subroutine general_filluv_ns
