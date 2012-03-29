subroutine general_read_gfsatm(grd,sp,filename,mype,uvflag,g_z,g_ps,g_vor,g_div,g_u,g_v,&
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
!
!   input argument list:
!     grd      - structure variable containing information about grid
!                    (initialized by general_sub2grid_create_info, located in general_sub2grid_mod.f90)
!     sp       - structure variable containing spectral information
!                    (initialized by general_init_spec_vars, located in general_specmod.f90)
!     filename - input sigma file name
!     mype     - mpi task id
!     uvflag   - logical to use u,v (.true.) or st,vp (.false.) perturbations
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
    use sigio_module, only: sigio_intkind,sigio_head,sigio_data,&
         sigio_srohdc,sigio_axdata
    use ncepgfs_io, only: sigio_cnvtdv8

    implicit none
    
!   Declare local parameters
    integer(sigio_intkind):: lunges = 11
!   real(r_kind),parameter:: r0_001 = 0.001_r_kind

!   Declare passed variables
    type(sub2grid_info)                   ,intent(in   ) :: grd
    type(spec_vars)                       ,intent(in   ) :: sp
    character(*)                          ,intent(in   ) :: filename
    integer(i_kind)                       ,intent(in   ) :: mype
    logical                               ,intent(in   ) :: uvflag
    integer(i_kind)                       ,intent(  out) :: iret_read
    real(r_kind),dimension(grd%lat2,grd%lon2)     ,intent(  out) :: g_z,g_ps
    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig),intent(  out) :: g_u,g_v,&
         g_vor,g_div,g_cwmr,g_q,g_oz,g_tv
    
!   Declare local variables
    integer(i_kind):: iret,nlatm2,ij,n,ii1,l,m
    integer(i_kind) i,j,k,icount
    integer(i_kind),dimension(npe)::ilev,iflag
    real(r_kind),dimension(grd%nlon,grd%nlat-2):: grid,grid_u,grid_v,&
         grid_div,grid2,grid3
    real(r_kind),dimension(grd%nlon,grd%nlat-2,ntracer):: grid_q
    real(r_kind),dimension(sp%nc):: spec_work,spec_vor,spec_div
    real(r_kind),dimension(grd%itotsub):: work,work_x
        
    type(sigio_head):: sighead
    type(sigio_data):: sigdata
    type(ncepgfs_head):: gfshead


!******************************************************************************  
!   Initialize variables used below
    iret_read=0
    nlatm2=grd%nlat-2

!   Read NCEP gfs guess file using appropriate io module
    call sigio_srohdc(lunges,filename,sighead,sigdata,iret_read)
    gfshead%fhour   = sighead%fhour
    gfshead%idate   = sighead%idate
    gfshead%levs    = sighead%levs
    gfshead%ntrac   = sighead%ntrac
    gfshead%ncldt   = sighead%ncldt
    gfshead%lonb    = grd%nlon
    gfshead%latb    = nlatm2

    if (iret_read /= 0) goto 1000


    icount=0

!   Process guess fields according to type of input file.   NCEP_SIGIO files
!   are spectral coefficient files and need to be transformed to the grid.
!   Once on the grid, fields need to be scattered from the full domain to 
!   sub-domains.

!   Terrain:  spectral --> grid transform, scatter to all mpi tasks
    icount=icount+1
    iflag(icount)=1
    ilev(icount)=1
    if (mype==icount-1) then
       do i=1,sp%nc
          spec_work(i)=sp%test_mask(i)*sigdata%hs(i)
          if(sp%factsml(i))spec_work(i)=zero
       end do
       call general_sptez_s(sp,spec_work,grid,1)
       call general_fill_ns(grd,grid,work)
    endif
    if(icount == npe)then
       call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
           icount,iflag,ilev,work)
    end if


!   Surface pressure:  same procedure as terrain
!   NCEP SIGIO has two options for surface pressure.  Variable idpsfc5 
!   indicates the type:   
!      idpsfc5= 0,1 for ln(psfc)
!      idpsfc5= 2 for psfc
!   
    icount=icount+1
    iflag(icount)=2
    ilev(icount)=1
    if (mype==icount-1) then
       do i=1,sp%nc
          spec_work(i)=sp%test_mask(i)*sigdata%ps(i)
          if(sp%factsml(i))spec_work(i)=zero
       end do
       call general_sptez_s(sp,spec_work,grid,1)
       call general_fill_ns(grd,grid,work)

!      If ln(ps), take exponential to convert to ps in cb
       if (idpsfc5 /= 2) then
          do i=1,grd%itotsub
             work(i)=exp(work(i))
          end do
       endif
    endif
    if(icount == npe)then
       call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
           icount,iflag,ilev,work)
    end if
    
!   Thermodynamic variable:  s-->g transform, communicate to all tasks
!   For multilevel fields, each task handles a given level.  Periodic
!   mpi_alltoallv calls communicate the grids to all mpi tasks.  
!   Finally, the grids are loaded into guess arrays used later in the 
!   code.
    do k=1,gfshead%levs
       icount=icount+1
       iflag(icount)=3
       ilev(icount)=k
       if (mype==icount-1) then
          do i=1,sp%nc
             spec_work(i)=sp%test_mask(i)*sigdata%t(i,k)
             if(sp%factsml(i))spec_work(i)=zero
          end do
          call general_sptez_s(sp,spec_work,grid,1)

!         SIGIO has three possible thermodynamic variables
!         Variable idthrm5 indicates the type
!            idthrm5 = 0,1 = virtual temperature (Tv)
!            idthrm5 = 2   = sensible (dry) temperature (T)
!            idthrm5 = 3   = enthalpy (h=CpT)
!         The GSI analysis variable is Tv

!         If needed, convert T or h to Tv

          if (idthrm5==2 .or. idthrm5==3) then

!            Convert tracers from spectral coefficients to grid
             do n=1,ntracer
                do i=1,sp%nc
                   spec_work(i)=sp%test_mask(i)*sigdata%q(i,k,n)
                   if(sp%factsml(i))spec_work(i)=zero
                end do
                call general_sptez_s(sp,spec_work,grid_q(1,1,n),1)
             end do

!            Convert input thermodynamic variable to dry temperature
             call sigio_cnvtdv8(grd%nlon*nlatm2,grd%nlon*nlatm2,1,idvc5,&
                  idvm5,ntracer,iret,grid,grid_q,cp5,1)
                            iret_read=iret_read+iret
                            if (iret_read /= 0) goto 1000

!            Convert dry temperature to virtual
             do j=1,nlatm2
                do i=1,grd%nlon
                   grid(i,j) = grid(i,j)*(one+fv*grid_q(i,j,1))
                end do
             end do

          endif

!         Load values into rows for south and north pole
          call general_fill_ns(grd,grid,work)
       end if
       if (icount == npe) then
          call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
               icount,iflag,ilev,work)
       end if
    end do
    do k=1,gfshead%levs
       icount=icount+1
       iflag(icount)=4
       ilev(icount)=k
       if (mype==icount-1) then
!  Vorticity
!         Convert spectral coefficients of vor to grid space
          do i=1,sp%nc
             spec_vor(i)=sp%test_mask(i)*sigdata%z(i,k)   !vor
             if(sp%factvml(i))spec_vor(i)=zero
          end do
          call general_sptez_s(sp,spec_vor,grid,1)


!         Convert grid u,v to div and vor
          call general_fill_ns(grd,grid,work)

       end if
       if (icount == npe) then
           call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
               icount,iflag,ilev,work)
       end if
    end do
    do k=1,gfshead%levs
       icount=icount+1
       iflag(icount)=5
       ilev(icount)=k
       if (mype==icount-1) then
!   Divergence 
!         Convert spectral coefficients of div to grid space
          do i=1,sp%nc
             spec_div(i)=sp%test_mask(i)*sigdata%d(i,k)   !div
             if(sp%factvml(i))spec_div(i)=zero
          end do
             
          call general_sptez_s(sp,spec_div,grid_div,1)

!         Convert grid u,v to div and vor
          call general_fill_ns(grd,grid_div,work)

       end if
       if (icount == npe) then
          call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
              icount,iflag,ilev,work)
       end if
    end do
    do k=1,gfshead%levs
       icount=icount+1
       iflag(icount)=6
       ilev(icount)=k
       if (mype==icount-1) then
!   U  Compute u and v from div and vor
!         Convert spectral coefficients of div and vor to grid space
          if (uvflag) then
             do i=1,sp%nc
                spec_div(i)=sp%test_mask(i)*sigdata%d(i,k)   !div
                spec_vor(i)=sp%test_mask(i)*sigdata%z(i,k)   !vor
                if(sp%factvml(i))then
                   spec_div(i)=zero
                   spec_vor(i)=zero
                end if
             end do
             call general_sptez_v(sp,spec_div,spec_vor,grid_u,grid_v,1)
             call general_filluv_ns(grd,sp,grid_u,grid_v,work,work_x)
! if streamfunction/velocity potential:
          else
             do i=1,sp%nc
                spec_vor(i)=sp%test_mask(i)*sigdata%z(i,k)   !vor
                if(sp%factvml(i))spec_vor(i)=zero
             end do
             do i=2,sp%ncd2
                spec_vor(2*i-1)=spec_vor(2*i-1)/(-sp%enn1(i))
                spec_vor(2*i)=spec_vor(2*i)/(-sp%enn1(i))
             end do
             call general_sptez_s(sp,spec_vor,grid_u,1)
             call general_fill_ns(grd,grid_u,work)
          end if
       end if
       if (icount == npe) then
           call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
               icount,iflag,ilev,work)
       end if
    end do
    do k=1,gfshead%levs
       icount=icount+1
       iflag(icount)=7
       ilev(icount)=k
       if (mype==icount-1) then
!   Divergence and voriticity.  Compute u and v from div and vor
!         Convert spectral coefficients of div and vor to grid space
          if (uvflag) then
             do i=1,sp%nc
                spec_div(i)=sp%test_mask(i)*sigdata%d(i,k)   !div
                spec_vor(i)=sp%test_mask(i)*sigdata%z(i,k)   !vor
                if(sp%factvml(i))then
                   spec_div(i)=zero
                   spec_vor(i)=zero
                end if
             end do
             call general_sptez_v(sp,spec_div,spec_vor,grid_u,grid_v,1)
             call general_filluv_ns(grd,sp,grid_u,grid_v,work_x,work)
! if velocity potential:
          else
             do i=1,sp%nc
                spec_div(i)=sp%test_mask(i)*sigdata%d(i,k)   !div
                if(sp%factvml(i))spec_div(i)=zero
             end do
             do i=2,sp%ncd2
                spec_div(2*i-1)=spec_div(2*i-1)/(-sp%enn1(i))
                spec_div(2*i)=spec_div(2*i)/(-sp%enn1(i))
             end do
             call general_sptez_s(sp,spec_div,grid_v,1)
             call general_fill_ns(grd,grid_v,work)
          end if
       end if
       if (icount == npe) then
           call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
               icount,iflag,ilev,work)
       end if
    end do
    do k=1,gfshead%levs
       icount=icount+1
       iflag(icount)=8
       ilev(icount)=k
       if (mype==icount-1) then
!   Specific humidity
          do i=1,sp%nc
             spec_work(i)=sp%test_mask(i)*sigdata%q(i,k,1)
             if(sp%factsml(i))spec_work(i)=zero
          end do
          call general_sptez_s(sp,spec_work,grid,1)
          call general_fill_ns(grd,grid,work)
       end if
       if (icount == npe) then
          call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
              icount,iflag,ilev,work)
       end if
    end do
    do k=1,gfshead%levs
       icount=icount+1
       iflag(icount)=9
       ilev(icount)=k
       if (mype==icount-1) then
!   Ozone mixing ratio
          do i=1,sp%nc
             spec_work(i)=sp%test_mask(i)*sigdata%q(i,k,2)
             if(sp%factsml(i))spec_work(i)=zero
          end do
          call general_sptez_s(sp,spec_work,grid,1)
          call general_fill_ns(grd,grid,work)

       end if
       if (icount == npe) then
           call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
               icount,iflag,ilev,work)
       end if
    end do
    do k=1,gfshead%levs
       icount=icount+1
       iflag(icount)=10
       ilev(icount)=k
       if (mype==icount-1) then
!   Cloud condensate mixing ratio.
         if (gfshead%ntrac>2 .or. gfshead%ncldt>=1) then
            do i=1,sp%nc
               spec_work(i)=sp%test_mask(i)*sigdata%q(i,k,3)
               if(sp%factsml(i))spec_work(i)=zero
            end do
            call general_sptez_s(sp,spec_work,grid,1)
            call general_fill_ns(grd,grid,work)
         else
             work=zero
         endif
       endif

       if (icount == npe .or. k == gfshead%levs) then
           call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
               icount,iflag,ilev,work)
       end if
    end do
    
!   Deallocate sigio data array
    call sigio_axdata(sigdata,iret)
    iret_read=iret_read+iret

!   Print date/time stamp 
    if(mype==0) then
       write(6,700) gfshead%lonb,gfshead%latb,gfshead%levs,&
            gfshead%fhour,gfshead%idate
700    format('READ_GFSATM:  ges read/scatter, lonb,latb,levs=',&
            3i6,', hour=',f10.1,', idate=',4i5)
    end if

    return


!   ERROR detected while reading file
1000 continue
    if (mype==0) write(6,*)'READ_GFSATM:  ***ERROR*** while reading ',&
         filename,' from unit ',lunges,'.   iret_read=',iret_read
    call sigio_axdata(sigdata,iret)
    iret_read=iret_read+iret

    
!   End of routine.  Return
    return
end subroutine general_read_gfsatm

subroutine general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
           icount,iflag,ilev,work)

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

! !OUTPUT PARAMETERS:

  real(r_kind),dimension(grd%lat2,grd%lon2)     ,intent(  out) :: g_z,g_ps
  real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig),intent(  out) :: g_u,g_v,&
       g_vor,g_div,g_cwmr,g_q,g_oz,g_tv


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
  real(r_kind),dimension(grd%lat2*grd%lon2,npe):: sub

  call mpi_alltoallv(work,grd%ijn_s,grd%displs_s,mpi_rtype,&
       sub,grd%irc_s,grd%ird_s,mpi_rtype,&
       mpi_comm_world,ierror)
!$omp parallel do  schedule(dynamic,1) private(k,i,j,ij)
  do k=1,icount
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
              g_tv(i,j,ilev(k))=sub(ij,k)
           end do
        end do
     else if(iflag(k) == 4)then
        ij=0
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              g_vor(i,j,ilev(k))=sub(ij,k)
           end do
        end do
     else if(iflag(k) == 5)then
        ij=0
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              g_div(i,j,ilev(k))=sub(ij,k)
           end do
        end do
     else if(iflag(k) == 6)then
        ij=0
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              g_u(i,j,ilev(k))=sub(ij,k)
           end do
        end do
     else if(iflag(k) == 7)then
        ij=0
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              g_v(i,j,ilev(k))=sub(ij,k)
           end do
        end do
     else if(iflag(k) == 8)then
        ij=0
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              g_q(i,j,ilev(k))=sub(ij,k)
           end do
        end do
     else if(iflag(k) == 9)then
        ij=0
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              g_oz(i,j,ilev(k))=sub(ij,k)
           end do
        end do
     else if(iflag(k) == 10)then
        ij=0
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              g_cwmr(i,j,ilev(k))=sub(ij,k)
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
   integer(i_kind) i,j,k,jj,nlatm2
   real(r_kind) rnlon,sumn,sums
   real(r_kind),dimension(grd%nlon,grd%nlat):: grid

!  Transfer contents of input grid to local work array
!  Reverse ordering in j direction from n-->s to s-->n
   do j=2,grd%nlat-1
      jj=grd%nlat-j
      do i=1,grd%nlon
         grid(i,j)=grid_in(i,jj)
      end do
   end do

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
   integer(i_kind) i,j,k,jj
   real(r_kind) polnu,polnv,polsu,polsv
   real(r_kind),dimension(grd%nlon,grd%nlat):: grid,grid2

!  Transfer contents of input grid to local work array
!  Reverse ordering in j direction from n-->s to s-->n
   do j=2,grd%nlat-1
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
      polnu=polnu+grid(i,grd%nlat-1)*sp%clons(i)-grid2(i,grd%nlat-1)*sp%slons(i)
      polnv=polnv+grid(i,grd%nlat-1)*sp%slons(i)+grid2(i,grd%nlat-1)*sp%clons(i)
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
