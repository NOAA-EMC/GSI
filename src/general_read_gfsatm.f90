subroutine general_read_gfsatm(grd,sp_a,sp_b,filename,mype,uvflag,vordivflag,zflag, &
       g_z,g_ps,g_vor,g_div,g_u,g_v,&
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
!   2010-03-29  kleist     - modifications to allow for st/vp perturbations instead of u,v
!   2012-01-17  wu         - increase character length for variable "filename"
!   2014-12-03  derber     - introduce vordivflag, zflag and optimize routines
!
!   input argument list:
!     grd      - structure variable containing information about grid
!                    (initialized by general_sub2grid_create_info, located in general_sub2grid_mod.f90)
!     sp_a     - structure variable containing spectral information for analysis
!                    (initialized by general_init_spec_vars, located in general_specmod.f90)
!     sp_b     - structure variable containing spectral information for input
!                     fields
!                    (initialized by general_init_spec_vars, located in general_specmod.f90)
!     filename - input sigma file name
!     mype     - mpi task id
!     uvflag   - logical to use u,v (.true.) or st,vp (.false.) perturbations
!     vordivflag - logical to determine if routine should output vorticity and
!                  divergence
!     zflag    - logical to determine if surface height field should be output
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
         ncepgfs_head,idpsfc5,idthrm5,&
         ntracer,idvc5,cp5,idvm5
    use general_sub2grid_mod, only: sub2grid_info
    use general_specmod, only: spec_vars
    use mpimod, only: npe
    use constants, only: zero,one,fv,r0_01
    use sigio_module, only: sigio_intkind,sigio_head,sigio_alhead
    use sigio_r_module, only: sigio_dbti,sigio_rrhead,sigio_rropen,&
        sigio_axdbti,sigio_rrdbti,sigio_aldbti,sigio_rclose
    use ncepgfs_io, only: sigio_cnvtdv8

    implicit none
    
!   Declare local parameters
    integer(sigio_intkind):: lunges = 11
!   real(r_kind),parameter:: r0_001 = 0.001_r_kind

!   Declare passed variables
    type(sub2grid_info)                   ,intent(in   ) :: grd
    type(spec_vars)                       ,intent(in   ) :: sp_a,sp_b
    character(*)                          ,intent(in   ) :: filename
    integer(i_kind)                       ,intent(in   ) :: mype
    logical                               ,intent(in   ) :: uvflag,zflag,vordivflag
    integer(i_kind)                       ,intent(  out) :: iret_read
    real(r_kind),dimension(grd%lat2,grd%lon2)     ,intent(  out) :: g_ps
    real(r_kind),dimension(grd%lat2,grd%lon2)     ,intent(inout) :: g_z
    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig),intent(  out) :: g_u,g_v,&
         g_vor,g_div,g_cwmr,g_q,g_oz,g_tv
    
!   Declare local variables
    integer(i_kind):: iret,nlatm2,nlevs
    integer(i_kind) i,j,k,icount,nflds
    integer(i_kind),dimension(npe)::ilev,iflag
    real(r_kind),dimension(grd%nlon,grd%nlat-2):: grid

    real(r_kind),dimension(sp_b%nc),target ::  specwrk_4,specdiv_4
    real(r_kind),dimension(sp_b%nc):: spec_work

    real(r_kind),dimension(grd%itotsub):: work
    real(r_kind),allocatable,dimension(:):: spec_div
    real(r_kind),allocatable,dimension(:,:):: grid_v
        
    type(sigio_head):: sighead
    type(sigio_dbti):: sigdati

!******************************************************************************  
!   Initialize variables used below
    iret_read=0
    iret=0
    nlatm2=grd%nlat-2
    i=1
    nflds=5*grd%nsig+1
    if(zflag) nflds=nflds+1
    if(vordivflag .or. .not. uvflag)nflds=nflds+2*grd%nsig

    nlevs=grd%nsig
    if(mype < nflds)then
!   All tasks open and read header with RanRead
       rewind(lunges)
       call sigio_rropen(lunges,filename,iret)
       call sigio_rrhead(lunges,sighead,iret_read)
       nlevs=sighead%levs
       if(nlevs /= grd%nsig)go to 1000
       if (iret_read /=0) goto 1000
    end if

    icount=0

!   Process guess fields according to type of input file.   NCEP_SIGIO files
!   are spectral coefficient files and need to be transformed to the grid.
!   Once on the grid, fields need to be scattered from the full domain to 
!   sub-domains.

!  Only read Terrain when zflag is true.
    if(zflag)then
!   Terrain:  spectral --> grid transform, scatter to all mpi tasks
       icount=icount+1
       iflag(icount)=1
       ilev(icount)=1
       if (mype==icount-1) then

! read hs
         sigdati%i = 1                                           ! hs
         sigdati%f => specwrk_4
         call sigio_rrdbti(lunges,sighead,sigdati,iret)

          do i=1,sp_b%nc
             spec_work(i)=sp_b%test_mask(i)*specwrk_4(i)
          end do
          do i=1,sp_b%nc
             if(sp_b%factsml(i))spec_work(i)=zero
          end do
          call general_sptez_s_b(sp_a,sp_b,spec_work,grid,1)
          call general_fill_ns(grd,grid,work)
       endif
       if(icount == npe)then
          call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
              icount,iflag,ilev,work,uvflag,vordivflag)
       end if
    end if


!   Surface pressure:  same procedure as terrain
    icount=icount+1
    iflag(icount)=2
    ilev(icount)=1
    if (mype==icount-1) then

! read ps
      sigdati%i = 2                                           ! ps
      sigdati%f => specwrk_4
      call sigio_rrdbti(lunges,sighead,sigdati,iret)

       do i=1,sp_b%nc
          spec_work(i)=sp_b%test_mask(i)*specwrk_4(i)
       end do
       do i=1,sp_b%nc
          if(sp_b%factsml(i))spec_work(i)=zero
       end do
       call general_sptez_s_b(sp_a,sp_b,spec_work,grid,1)
       call general_fill_ns(grd,grid,work)
    endif
    if(icount == npe)then
       call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
           icount,iflag,ilev,work,uvflag,vordivflag)
    end if
    
!   Thermodynamic variable:  s-->g transform, communicate to all tasks
!   For multilevel fields, each task handles a given level.  Periodic
!   mpi_alltoallv calls communicate the grids to all mpi tasks.  
!   Finally, the grids are loaded into guess arrays used later in the 
!   code.
    do k=1,nlevs
       icount=icount+1
       iflag(icount)=3
       ilev(icount)=k
       if (mype==icount-1) then

! read T/Tv/etc.
       sigdati%i = 2+k                                           ! hs
       sigdati%f => specwrk_4
       call sigio_rrdbti(lunges,sighead,sigdati,iret)

          do i=1,sp_b%nc
             spec_work(i)=sp_b%test_mask(i)*specwrk_4(i)
          end do
          do i=1,sp_b%nc
             if(sp_b%factsml(i))spec_work(i)=zero
          end do
          call general_sptez_s_b(sp_a,sp_b,spec_work,grid,1)

!         Load values into rows for south and north pole
          call general_fill_ns(grd,grid,work)
       end if
       if (icount == npe) then
          call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
               icount,iflag,ilev,work,uvflag,vordivflag)
       end if
    end do
    if(vordivflag .or. .not. uvflag) then
       do k=1,nlevs
          icount=icount+1
          iflag(icount)=4
          ilev(icount)=k
          if (mype==icount-1) then
!  Vorticity
             sigdati%i = nlevs + 2 + (k-1) * 2 + 2     ! Vorticity
             sigdati%f => specwrk_4
             call sigio_rrdbti(lunges,sighead,sigdati,iret)

!         Convert spectral coefficients of vor to grid space
             do i=1,sp_b%nc
                spec_work(i)=sp_b%test_mask(i)*specwrk_4(i)   !vor
             end do
             do i=1,sp_b%nc
                if(sp_b%factvml(i))spec_work(i)=zero
             end do
             call general_sptez_s_b(sp_a,sp_b,spec_work,grid,1)

!            Convert grid u,v to div and vor
             call general_fill_ns(grd,grid,work)

          end if
          if (icount == npe) then
              call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                  icount,iflag,ilev,work,uvflag,vordivflag)
          end if
       end do
       do k=1,nlevs
          icount=icount+1
          iflag(icount)=5
          ilev(icount)=k
          if (mype==icount-1) then
!   Divergence 
             sigdati%i = nlevs + 2 + (k-1) * 2 + 1     ! Divergence
             sigdati%f => specwrk_4
             call sigio_rrdbti(lunges,sighead,sigdati,iret)

!            Convert spectral coefficients of div to grid space
             do i=1,sp_b%nc
                spec_work(i)=sp_b%test_mask(i)*specwrk_4(i)   !div
             end do
             do i=1,sp_b%nc
                if(sp_b%factvml(i))spec_work(i)=zero
             end do
             
             call general_sptez_s_b(sp_a,sp_b,spec_work,grid,1)

!            Convert grid u,v to div and vor
             call general_fill_ns(grd,grid,work)

          end if
          if (icount == npe) then
             call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                 icount,iflag,ilev,work,uvflag,vordivflag)
          end if
       end do
    end if
    if (uvflag) then
       do k=1,nlevs
          icount=icount+1
          iflag(icount)=6
          ilev(icount)=k
          if (mype==icount-1) then


!   U  Compute u and v from div and vor

!               Divergence
             sigdati%i = nlevs + 2 + (k-1) * 2 + 1     ! Divergence
             sigdati%f => specdiv_4
             call sigio_rrdbti(lunges,sighead,sigdati,iret)

!               Vorticity
             sigdati%i = nlevs + 2 + (k-1) * 2 + 2     ! Vorticity
             sigdati%f => specwrk_4
             call sigio_rrdbti(lunges,sighead,sigdati,iret)

             allocate(spec_div(sp_b%nc),grid_v(grd%nlon,grd%nlat-2))
             do i=1,sp_b%nc
                spec_div(i)=sp_b%test_mask(i)*specdiv_4(i)   !div
                spec_work(i)=sp_b%test_mask(i)*specwrk_4(i)   !vor
             end do
             do i=1,sp_b%nc
                if(sp_b%factvml(i))then
                   spec_div(i)=zero
                   spec_work(i)=zero
                end if
             end do
             call general_sptez_v_b(sp_a,sp_b,spec_div,spec_work,grid,grid_v,1,1)
             call general_fillu_ns(grd,sp_a,grid,grid_v,work)
             deallocate(spec_div,grid_v)
          end if
          if (icount == npe) then
              call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                  icount,iflag,ilev,work,uvflag,vordivflag)
          end if
       end do
       do k=1,nlevs
          icount=icount+1
          iflag(icount)=7
          ilev(icount)=k
          if (mype==icount-1) then
!   V  Compute u and v from div and vor

!              Divergence
             sigdati%i = nlevs + 2 + (k-1) * 2 + 1     ! Divergence
             sigdati%f => specdiv_4
             call sigio_rrdbti(lunges,sighead,sigdati,iret)

!              Vorticity
             sigdati%i = nlevs + 2 + (k-1) * 2 + 2     ! Vorticity
             sigdati%f => specwrk_4
             call sigio_rrdbti(lunges,sighead,sigdati,iret)

             allocate(spec_div(sp_b%nc),grid_v(grd%nlon,grd%nlat-2))
             do i=1,sp_b%nc
                spec_div(i)=sp_b%test_mask(i)*specdiv_4(i)   !div
                spec_work(i)=sp_b%test_mask(i)*specwrk_4(i)   !vor
             end do
             do i=1,sp_b%nc
                if(sp_b%factvml(i))then
                   spec_div(i)=zero
                   spec_work(i)=zero
                end if
             end do
             call general_sptez_v_b(sp_a,sp_b,spec_div,spec_work,grid,grid_v,1,-1)
             call general_fillv_ns(grd,sp_a,grid,grid_v,work)
             deallocate(spec_div,grid_v)
          end if
          if (icount == npe) then
              call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                  icount,iflag,ilev,work,uvflag,vordivflag)
          end if
       end do
    end if
    do k=1,nlevs
       icount=icount+1
       iflag(icount)=8
       ilev(icount)=k
       if (mype==icount-1) then

!   Specific humidity
          sigdati%i = nlevs * (2+1) + 2 + k    ! q
          sigdati%f => specwrk_4
          call sigio_rrdbti(lunges,sighead,sigdati,iret)

          do i=1,sp_b%nc
             spec_work(i)=sp_b%test_mask(i)*specwrk_4(i)
          end do
          do i=1,sp_b%nc
             if(sp_b%factsml(i))spec_work(i)=zero
          end do
          call general_sptez_s_b(sp_a,sp_b,spec_work,grid,1)
          call general_fill_ns(grd,grid,work)
       end if
       if (icount == npe) then
          call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
              icount,iflag,ilev,work,uvflag,vordivflag)
       end if
    end do
    do k=1,nlevs
       icount=icount+1
       iflag(icount)=9
       ilev(icount)=k
       if (mype==icount-1) then
!   Ozone mixing ratio
          sigdati%i = nlevs * (2+2) + 2 + k    ! oz
          sigdati%f => specwrk_4
          call sigio_rrdbti(lunges,sighead,sigdati,iret)

          do i=1,sp_b%nc
             spec_work(i)=sp_b%test_mask(i)*specwrk_4(i)
          end do
          do i=1,sp_b%nc
             if(sp_b%factsml(i))spec_work(i)=zero
          end do
          call general_sptez_s_b(sp_a,sp_b,spec_work,grid,1)
          call general_fill_ns(grd,grid,work)

       end if
       if (icount == npe) then
           call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
               icount,iflag,ilev,work,uvflag,vordivflag)
       end if
    end do
    do k=1,nlevs
       icount=icount+1
       iflag(icount)=10
       ilev(icount)=k
       if (mype==icount-1) then
!   Cloud condensate mixing ratio.
         if (sighead%ntrac>2 .or. sighead%ncldt>=1) then
! 
          sigdati%i = nlevs * (2+3) + 2 + k    ! cw, 3rd tracer
          sigdati%f => specwrk_4
          call sigio_rrdbti(lunges,sighead,sigdati,iret)

            do i=1,sp_b%nc
               spec_work(i)=sp_b%test_mask(i)*specwrk_4(i)
            end do
            do i=1,sp_b%nc
               if(sp_b%factsml(i))spec_work(i)=zero
            end do
            call general_sptez_s_b(sp_a,sp_b,spec_work,grid,1)
            call general_fill_ns(grd,grid,work)
         else
             work=zero
         endif
       endif

       if (icount == npe .or. k == nlevs) then
           call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
               icount,iflag,ilev,work,uvflag,vordivflag)
       end if
    end do
    
    if(mype < nflds)then
!     Deallocate sigio data array
      call sigio_rclose(lunges,iret)
      deallocate(sighead%vcoord,sighead%cfvars)
    end if

!   Surface pressure.
!   NCEP SIGIO has two options for surface pressure.  Variable idpsfc5
!   indicates the type:
!      idpsfc5= 0,1 for ln(psfc)
!      idpsfc5= 2 for psfc
!   If input ln(ps), take exponential to convert to ps in cb
    if (idpsfc5 /= 2) then
       do j=1,grd%lon2
          do i=1,grd%lat2
             g_ps(i,j)=exp(g_ps(i,j))
          end do
       end do
    endif

!   NCEP SIGIO has three possible thermodynamic variables
!     Variable idthrm5 indicates the type
!       idthrm5 = 0,1 = virtual temperature (Tv)
!       idthrm5 = 2   = sensible (dry) temperature (T)
!       idthrm5 = 3   = enthalpy (h=CpT)
!     The GSI analysis variable is Tv

    if (idthrm5==2 .or. idthrm5==3) then

!      Convert input enthalpy to dry temperature
       if (idthrm5==3) then
          call sigio_cnvtdv8(grd%lat2*grd%lon2,grd%lat2*grd%lon2,&
               grd%nsig,idvc5,idvm5,ntracer,iret,g_tv,g_q,cp5,1)
       endif

!      Convert dry temperature to virtual temperature
       do k=1,grd%nsig
          do j=1,grd%lon2
             do i=1,grd%lat2
                g_tv(i,j,k) = g_tv(i,j,k)*(one+fv*g_q(i,j,k))
             end do
          end do
       end do
    endif

!   Print date/time stamp 
    if(mype==0) then
       write(6,700) sighead%lonb,sighead%latb,nlevs,grd%nlon,nlatm2,&
            sighead%fhour,sighead%idate
700    format('GENERAL_READ_GFSATM:  ges read/scatter, lonb,latb,levs=',&
            3i6,', nlon,nlat=',2i6,', hour=',f10.1,', idate=',4i5)
    end if

    return


!   ERROR detected while reading file
1000 continue
     write(6,*)'GENERAL_READ_GFSATM:  ***ERROR*** reading ',&
         trim(filename),' mype,iret_read=',mype,iret_read,grd%nsig,nlevs
     return

!   End of routine.  Return

    return
end subroutine general_read_gfsatm

subroutine general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
           icount,iflag,ilev,work,uvflag,vdflag)

! !USES:

  use kinds, only: r_kind,i_kind
  use mpimod, only: npe,mpi_comm_world,ierror,mpi_rtype
  use general_sub2grid_mod, only: sub2grid_info
  implicit none

! !INPUT PARAMETERS:

  type(sub2grid_info)                   ,intent(in   ) :: grd
  integer(i_kind),intent(inout) ::icount
  integer(i_kind),dimension(npe),intent(inout):: ilev,iflag
  real(r_kind),dimension(grd%itotsub),intent(in) :: work
  logical,intent(in) :: uvflag,vdflag

! !OUTPUT PARAMETERS:

  real(r_kind),dimension(grd%lat2,grd%lon2)     ,intent(  out) :: g_ps
  real(r_kind),dimension(grd%lat2,grd%lon2)     ,intent(inout) :: g_z
  real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig),intent(  out) :: g_u,g_v,&
       g_vor,g_div,g_cwmr,g_q,g_oz,g_tv


! !DESCRIPTION: Transfer contents of 2-d array global to 3-d subdomain array
!
! !REVISION HISTORY:
!   2004-05-14  treadon
!   2004-07-15  todling, protex-compliant prologue
!   2014-12-03  derber     - introduce vdflag and optimize routines
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

  integer(i_kind) i,j,k,ij,klev
  real(r_kind),dimension(grd%lat2*grd%lon2,npe):: sub

  call mpi_alltoallv(work,grd%ijn_s,grd%displs_s,mpi_rtype,&
       sub,grd%irc_s,grd%ird_s,mpi_rtype,&
       mpi_comm_world,ierror)
!$omp parallel do  schedule(dynamic,1) private(k,i,j,ij,klev)
  do k=1,icount
     klev=ilev(k)
     if(iflag(k) == 1)then
        ij=0
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              g_z(i,j)=sub(ij,k)
           end do
        end do
     else if(iflag(k) == 2)then
        ij=0
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              g_ps(i,j)=sub(ij,k)
           end do
        end do
     else if(iflag(k) == 3)then
        ij=0
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              g_tv(i,j,klev)=sub(ij,k)
           end do
        end do
     else if(iflag(k) == 4)then
        if(vdflag)then
          ij=0
          do j=1,grd%lon2
             do i=1,grd%lat2
                ij=ij+1
                g_vor(i,j,klev)=sub(ij,k)
             end do
          end do
        end if
        if(.not. uvflag)then
          ij=0
          do j=1,grd%lon2
             do i=1,grd%lat2
                ij=ij+1
                g_u(i,j,klev)=sub(ij,k)
             end do
          end do
        end if
     else if(iflag(k) == 5)then
        if(vdflag)then
          ij=0
          do j=1,grd%lon2
             do i=1,grd%lat2
                ij=ij+1
                g_div(i,j,klev)=sub(ij,k)
             end do
          end do
        end if
        if(.not. uvflag)then
          ij=0
          do j=1,grd%lon2
             do i=1,grd%lat2
                ij=ij+1
                g_v(i,j,klev)=sub(ij,k)
             end do
          end do
        end if
     else if(iflag(k) == 6)then
        if(.not. uvflag) then
          write(6,*) 'error in general_reload  u '
        end if
        ij=0
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              g_u(i,j,klev)=sub(ij,k)
           end do
        end do
     else if(iflag(k) == 7)then
        if(.not. uvflag) then
          write(6,*) 'error in general_reload  v '
        end if
        ij=0
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              g_v(i,j,klev)=sub(ij,k)
           end do
        end do
     else if(iflag(k) == 8)then
        ij=0
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              g_q(i,j,klev)=sub(ij,k)
           end do
        end do
     else if(iflag(k) == 9)then
        ij=0
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              g_oz(i,j,klev)=sub(ij,k)
           end do
        end do
     else if(iflag(k) == 10)then
        ij=0
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              g_cwmr(i,j,klev)=sub(ij,k)
           end do
        end do
     end if
  end do
  icount=0
  ilev=0
  iflag=0
  return
end subroutine general_reload

 subroutine general_fill_ns(grd,grid_in,grid_out)

! !USES:

   use kinds, only: r_kind,i_kind
   use constants, only: zero,one
   use general_sub2grid_mod, only: sub2grid_info
   implicit none

! !INPUT PARAMETERS:

   type(sub2grid_info)                   ,intent(in   ) :: grd
   real(r_kind),dimension(grd%nlon,grd%nlat-2),intent(in   ) :: grid_in  ! input grid
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
   integer(i_kind) i,j,k,nlatm2
   real(r_kind) rnlon,sumn,sums

!  Compute mean along southern and northern latitudes
   sumn=zero
   sums=zero
   nlatm2=grd%nlat-2
   do i=1,grd%nlon
      sumn=sumn+grid_in(i,1)
      sums=sums+grid_in(i,nlatm2)
   end do
   rnlon=one/float(grd%nlon)
   sumn=sumn*rnlon
   sums=sums*rnlon

!  Transfer local work array to output grid
   do k=1,grd%itotsub
      j=grd%nlat-grd%ltosi_s(k)
      if(j == grd%nlat-1) then
        grid_out(k)=sums
      else if(j == 0) then
        grid_out(k) = sumn
      else
        i=grd%ltosj_s(k)
        grid_out(k)=grid_in(i,j)
      end if
   end do

   return
 end subroutine general_fill_ns
 subroutine general_filluv_ns(grd,sp,gridu_in,gridv_in,gridu_out,gridv_out)

! !USES:

   use kinds, only: r_kind,i_kind
   use constants, only: zero
   use general_sub2grid_mod, only: sub2grid_info
   use general_specmod, only: spec_vars
   implicit none

! !INPUT PARAMETERS:

   type(sub2grid_info)                   ,intent(in   ) :: grd
   type(spec_vars)                       ,intent(in   ) :: sp
   real(r_kind),dimension(grd%nlon,grd%nlat-2),intent(in   ) :: gridu_in,gridv_in   ! input grid
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
   integer(i_kind) i,j,k,nlatm2
   real(r_kind) polnu,polnv,polsu,polsv


!  Compute mean along southern and northern latitudes
   polnu=zero
   polnv=zero
   polsu=zero
   polsv=zero
   nlatm2=grd%nlat-2
   do i=1,grd%nlon
      polnu=polnu+gridu_in(i,1     )*sp%clons(i)-gridv_in(i,1     )*sp%slons(i)
      polnv=polnv+gridu_in(i,1     )*sp%slons(i)+gridv_in(i,1     )*sp%clons(i)
      polsu=polsu+gridu_in(i,nlatm2)*sp%clons(i)+gridv_in(i,nlatm2)*sp%slons(i)
      polsv=polsv+gridu_in(i,nlatm2)*sp%slons(i)-gridv_in(i,nlatm2)*sp%clons(i)
   end do
   polnu=polnu/float(grd%nlon)
   polnv=polnv/float(grd%nlon)
   polsu=polsu/float(grd%nlon)
   polsv=polsv/float(grd%nlon)

!  Transfer local work array to output grid
   do k=1,grd%itotsub
      j=grd%nlat-grd%ltosi_s(k)
      i=grd%ltosj_s(k)
      if(j == grd%nlat-1)then
        gridu_out(k) = polsu*sp%clons(i)+polsv*sp%slons(i)
        gridv_out(k) = polsu*sp%slons(i)-polsv*sp%clons(i)
      else if(j == 0) then
        gridu_out(k) = polnu*sp%clons(i)+polnv*sp%slons(i)
        gridv_out(k) = -polnu*sp%slons(i)+polnv*sp%clons(i)
      else
        gridu_out(k)=gridu_in(i,j)
        gridv_out(k)=gridv_in(i,j)
      end if
   end do

   return
 end subroutine general_filluv_ns
 subroutine general_fillu_ns(grd,sp,gridu_in,gridv_in,gridu_out)

! !USES:

   use kinds, only: r_kind,i_kind
   use constants, only: zero
   use general_sub2grid_mod, only: sub2grid_info
   use general_specmod, only: spec_vars
   implicit none

! !INPUT PARAMETERS:

   type(sub2grid_info)                   ,intent(in   ) :: grd
   type(spec_vars)                       ,intent(in   ) :: sp
   real(r_kind),dimension(grd%nlon,grd%nlat-2),intent(in   ) :: gridu_in,gridv_in   ! input grid
   real(r_kind),dimension(grd%itotsub)           ,intent(  out) :: gridu_out ! output grid

! !DESCRIPTION: This routine adds a southern and northern latitude
!               row to the u input grid.  The southern row contains
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
!   2014-12-03  derber     - create specialized routine to just update u
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
   integer(i_kind) i,j,k,nlatm2
   real(r_kind) polnu,polnv,polsu,polsv


!  Compute mean along southern and northern latitudes
   polnu=zero
   polnv=zero
   polsu=zero
   polsv=zero
   nlatm2=grd%nlat-2
   do i=1,grd%nlon
      polnu=polnu+gridu_in(i,1     )*sp%clons(i)-gridv_in(i,1     )*sp%slons(i)
      polnv=polnv+gridu_in(i,1     )*sp%slons(i)+gridv_in(i,1     )*sp%clons(i)
      polsu=polsu+gridu_in(i,nlatm2)*sp%clons(i)+gridv_in(i,nlatm2)*sp%slons(i)
      polsv=polsv+gridu_in(i,nlatm2)*sp%slons(i)-gridv_in(i,nlatm2)*sp%clons(i)
   end do
   polnu=polnu/float(grd%nlon)
   polnv=polnv/float(grd%nlon)
   polsu=polsu/float(grd%nlon)
   polsv=polsv/float(grd%nlon)

!  Transfer local work array to output grid
   do k=1,grd%itotsub
      j=grd%nlat-grd%ltosi_s(k)
      i=grd%ltosj_s(k)
      if(j == grd%nlat-1)then
        gridu_out(k) = polsu*sp%clons(i)+polsv*sp%slons(i)
      else if(j == 0) then
        gridu_out(k) = polnu*sp%clons(i)+polnv*sp%slons(i)
      else
        gridu_out(k)=gridu_in(i,j)
      end if
   end do

   return
 end subroutine general_fillu_ns
 subroutine general_fillv_ns(grd,sp,gridu_in,gridv_in,gridv_out)

! !USES:

   use kinds, only: r_kind,i_kind
   use constants, only: zero
   use general_sub2grid_mod, only: sub2grid_info
   use general_specmod, only: spec_vars
   implicit none

! !INPUT PARAMETERS:

   type(sub2grid_info)                   ,intent(in   ) :: grd
   type(spec_vars)                       ,intent(in   ) :: sp
   real(r_kind),dimension(grd%nlon,grd%nlat-2),intent(in   ) :: gridu_in,gridv_in   ! input grid
   real(r_kind),dimension(grd%itotsub)           ,intent(  out) :: gridv_out ! output grid

! !DESCRIPTION: This routine adds a southern and northern latitude
!               row to the v input grid.  The southern row contains
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
!   2014-12-03  derber     - create specialized routine to just update v
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
   integer(i_kind) i,j,k,nlatm2
   real(r_kind) polnu,polnv,polsu,polsv


!  Compute mean along southern and northern latitudes
   polnu=zero
   polnv=zero
   polsu=zero
   polsv=zero
   nlatm2=grd%nlat-2
   do i=1,grd%nlon
      polnu=polnu+gridu_in(i,1     )*sp%clons(i)-gridv_in(i,1     )*sp%slons(i)
      polnv=polnv+gridu_in(i,1     )*sp%slons(i)+gridv_in(i,1     )*sp%clons(i)
      polsu=polsu+gridu_in(i,nlatm2)*sp%clons(i)+gridv_in(i,nlatm2)*sp%slons(i)
      polsv=polsv+gridu_in(i,nlatm2)*sp%slons(i)-gridv_in(i,nlatm2)*sp%clons(i)
   end do
   polnu=polnu/float(grd%nlon)
   polnv=polnv/float(grd%nlon)
   polsu=polsu/float(grd%nlon)
   polsv=polsv/float(grd%nlon)

!  Transfer local work array to output grid
   do k=1,grd%itotsub
      j=grd%nlat-grd%ltosi_s(k)
      i=grd%ltosj_s(k)
      if(j == grd%nlat-1)then
        gridv_out(k) = polsu*sp%slons(i)-polsv*sp%clons(i)
      else if(j == 0) then
        gridv_out(k) = -polnu*sp%slons(i)+polnv*sp%clons(i)
      else
        gridv_out(k)=gridv_in(i,j)
      end if
   end do

   return
 end subroutine general_fillv_ns

