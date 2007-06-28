module ncepgfs_io
!                .      .    .                                       .
! module:  ncepgfs_io
! prgmmr:  treadon           org: np23                date: 2006-01-10
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
!   sub read_gfssfc_full  - read ncep gfs surface file, keep on full model grid
!   sub write_gfs         - driver to write ncep gfs atmospheric and surface
!                           analysis files
!   sub write_gfsatm      - gather on grid, transform to spectral, write ncep
!                           gfs atmospheric analysis file
!   sub write_gfssfc      - gather/write on grid ncep surface analysis file
!
! Variable Definitions:
!   none
!
!$$$ end documentation block

  implicit none

  private
  public read_gfsatm
  public read_gfssfc
  public read_gfssfc_full
  public write_gfs
  public write_gfsatm
  public write_gfssfc

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
    use gridmod, only: displs_s,irc_s,ltosj_s,ijn_s,ltosi,ltosj,&
         ird_s,iglobal,nsig,nlat,nlon,lat2,lon2,hybrid,ltosi_s,&
         itotsub,fill_ns
    use specmod, only: factsml,factvml,jcap,nc,idrt,imax,jmax
    use mpimod, only: npe,mpi_comm_world,ierror,mpi_rtype,reload
    use constants, only: izero,zero,one
    use sigio_module, only: sigio_intkind,sigio_head,sigio_data,&
         sigio_srohdc,sigio_axdata
    implicit none
    
!   Declare local parameters
    integer(sigio_intkind):: lunges = 11

!   Declare passed variables
    character(24),intent(in):: filename
    integer(i_kind),intent(in):: mype
    integer(i_kind),intent(out):: iret_read
    real(r_kind),dimension(lat2,lon2),intent(out):: g_z,g_ps
    real(r_kind),dimension(lat2,lon2,nsig),intent(out):: g_u,g_v,&
         g_vor,g_div,g_cwmr,g_q,g_oz,g_tv
    
!   Declare local variables
    integer(sigio_intkind):: iret
    integer(i_kind) i,j,k,icount,icount_prev,mm1,ncloud,ntracer
    integer(i_kind) mype_zs,mype_ps
    real(r_kind),dimension(nlon,nlat-2):: grid,grid_u,grid_v
    real(r_kind),dimension(nc):: spec_work,spec_vor,spec_div
    real(r_kind),dimension(itotsub):: work,work_vor,work_div,&
         work_u,work_v
    real(r_kind),dimension(lat2*lon2,max(2*nsig,npe)):: sub,sub_div,sub_vor,&
         sub_u,sub_v
    
    type(sigio_head):: atm_head
    type(sigio_data):: atm_data
    
!******************************************************************************  
!   Initialize variables used below
    mm1=mype+1
    mype_zs=izero
    mype_ps=npe-1
    iret_read=izero

!   Open, read header & data, and close gfs atmospheric file
    call sigio_srohdc(lunges,filename,atm_head,atm_data,iret)
    iret_read=iret
    if (iret /= izero) goto 1000
    
!   Extract number of traces and cloud types
    ntracer = atm_head%ntrac
    ncloud  = atm_head%ncldt

!   Terrain:  spectral --> grid transform, scatter to all mpi tasks
    if (mype==mype_zs) then
       do i=1,nc
          spec_work(i)=factsml(i)*atm_data%hs(i)
       end do
       call sptez_s(spec_work,grid,1)
       call fill_ns(grid,work)
    endif
    call mpi_scatterv(work,ijn_s,displs_s,mpi_rtype,&
         g_z,ijn_s(mm1),mpi_rtype,mype_zs,mpi_comm_world,ierror)


!   Log(ps):  same procedure as terrain, but handled by task mype_ps
    if (mype==mype_ps) then
       do i=1,nc
          spec_work(i)=factsml(i)*atm_data%ps(i)
       end do
       call sptez_s(spec_work,grid,1)
       call fill_ns(grid,work)
       do i=1,itotsub
          work(i)=exp(work(i))
       end do
    endif
    call mpi_scatterv(work,ijn_s,displs_s,mpi_rtype,&
         g_ps,ijn_s(mm1),mpi_rtype,mype_ps,mpi_comm_world,ierror)
    
    
!   (Virtual) temperature:  s-->g transform, communicate to all tasks
!   For multilevel fields, each task handles a given level.  Periodic
!   mpi_alltoallv calls communicate the grids to all mpi tasks.  
!   Finally, the grids are loaded into guess arrays used later in the 
!   code.
    sub=zero
    icount=0
    icount_prev=1
    do k=1,nsig
       icount=icount+1
       if (mype==mod(icount-1,npe)) then
          do i=1,nc
             spec_work(i)=factsml(i)*atm_data%t(i,k)
          end do
          call sptez_s(spec_work,grid,1)
          call fill_ns(grid,work)
       endif
       if (mod(icount,npe)==0 .or. icount==nsig) then
          call mpi_alltoallv(work,ijn_s,displs_s,mpi_rtype,&
               sub(1,icount_prev),irc_s,ird_s,mpi_rtype,&
               mpi_comm_world,ierror)
          icount_prev=icount+1
       endif
    end do
    call reload(sub,g_tv)


!   Divergence and voriticity.  Compute u and v from div and vor
    sub_vor=zero
    sub_div=zero
    sub_u=zero
    sub_v=zero
    icount=0
    icount_prev=1
    do k=1,nsig
       icount=icount+1

!      The work in the loop below is spread over all mpi tasks
       if (mype==mod(icount-1,npe)) then

!         Convert spectral coefficients of div and vor to grid space
          do i=1,nc
             spec_div(i)=factvml(i)*atm_data%d(i,k)   !div
             spec_vor(i)=factvml(i)*atm_data%z(i,k)   !vor
          end do
          call sptez_s(spec_div,grid,1)
          call fill_ns(grid,work_div)
          
          call sptez_s(spec_vor,grid,1)
          call fill_ns(grid,work_vor)
          
          call sptez_v(spec_div,spec_vor,grid_u,grid_v,1)
          call fill_ns(grid_u,work_u)
          call fill_ns(grid_v,work_v)
          
       endif

!      Periodically exchange vor,div,u,v between all mpi tasks.
       if (mod(icount,npe)==0 .or. icount==nsig) then
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
          icount_prev=icount+1
       endif
    end do
    
!   Transfer vor,div,u,v into real(r_kind) guess arrays
    call reload(sub_vor,g_vor)
    call reload(sub_div,g_div)
    call reload(sub_u,g_u)
    call reload(sub_v,g_v)


!   Water vapor mixing ratio
    sub=zero
    icount=0
    icount_prev=1
    do k=1,nsig
       icount=icount+1
       if (mype==mod(icount-1,npe)) then
          do i=1,nc
             spec_work(i)=factsml(i)*atm_data%q(i,k,1)
          end do
          call sptez_s(spec_work,grid,1)
          call fill_ns(grid,work)
       endif
       if (mod(icount,npe)==0 .or. icount==nsig) then
          call mpi_alltoallv(work,ijn_s,displs_s,mpi_rtype,&
               sub(1,icount_prev),irc_s,ird_s,mpi_rtype,&
               mpi_comm_world,ierror)
          icount_prev=icount+1
       endif
    end do
    call reload(sub,g_q)


!   Ozone mixing ratio
    sub=zero
    icount=0
    icount_prev=1
    do k=1,nsig
       icount=icount+1
       if (mype==mod(icount-1,npe)) then
          do i=1,nc
             spec_work(i)=factsml(i)*atm_data%q(i,k,2)
          end do
          call sptez_s(spec_work,grid,1)
          call fill_ns(grid,work)
       endif
       if (mod(icount,npe)==0 .or. icount==nsig) then
          call mpi_alltoallv(work,ijn_s,displs_s,mpi_rtype,&
               sub(1,icount_prev),irc_s,ird_s,mpi_rtype,&
               mpi_comm_world,ierror)
          icount_prev=icount+1
       endif
    end do
    call reload(sub,g_oz)
    

!   Cloud condensate mixing ratio.
    if (ntracer>2 .or. ncloud>=1) then
       sub=zero
       icount=0
       icount_prev=1
       do k=1,nsig
          icount=icount+1
          if (mype==mod(icount-1,npe)) then
             do i=1,nc
                spec_work(i)=factsml(i)*atm_data%q(i,k,3)
             end do
             call sptez_s(spec_work,grid,1)
             call fill_ns(grid,work)
          endif
          if (mod(icount,npe)==0 .or. icount==nsig) then
             call mpi_alltoallv(work,ijn_s,displs_s,mpi_rtype,&
                  sub(1,icount_prev),irc_s,ird_s,mpi_rtype,&
                  mpi_comm_world,ierror)
             icount_prev=icount+1
          endif
       end do
       call reload(sub,g_cwmr)
    else
       do k=1,nsig
          do j=1,lon2
             do i=1,lat2
                g_cwmr(i,j,k)=zero
             end do
          end do
       end do
    endif
    
!   Deallocate sigio data array
    call sigio_axdata(atm_data,iret)
    iret_read=iret_read+iret

!   Print date/time stamp 
    if(mype==izero) then
       write(6,700) jcap,nsig,atm_head%fhour,atm_head%idate
700    format('READ_GFSATM:  ges read/transform/scatter, jcap,nsig=',&
            2i6,', hour=',f10.1,', idate=',4i5)
    end if
    return


!   ERROR detected while reading file
1000 continue
    if (mype==izero) write(6,*)'READ_GFSATM:  ***ERROR*** while reading ',&
         filename,' from unit ',lunges,'.   iret=',iret
    call sigio_axdata(atm_data,iret)
    iret_read=iret_read+iret

    
!   End of routine.  Return
    return
  end subroutine read_gfsatm


  subroutine read_gfssfc(filename,mype,fact10,sfct,sno,veg_type,&
       veg_frac,soil_type,soil_temp,soil_moi,isli,isli_gl,sfc_rough)
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
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: r_kind,i_kind
    use mpimod, only: ierror,mpi_rtype,mpi_comm_world,npe
    use gridmod, only: ijn_s,ird_s,irc_s,displs_s,ltosj_s,nlat,lon2,&
         lat2,ltosi_s,itotsub,nlon
    use sfcio_module, only: sfcio_intkind,sfcio_head,sfcio_data,&
         sfcio_srohdc,sfcio_axdata
    use constants, only: izero,zero
    implicit none

!   Declare passed variables
    character(24),intent(in):: filename
    integer(i_kind),intent(in):: mype
    integer(i_kind),dimension(lat2*lon2),intent(out):: isli
    integer(i_kind),dimension(nlat,nlon),intent(out):: isli_gl
    real(r_kind),dimension(lat2*lon2),intent(out):: fact10,sfct,sno,&
         veg_type,veg_frac,soil_type,soil_temp,soil_moi,sfc_rough
    
!   Declare local parameters
    integer(sfcio_intkind):: lunges = 11
    integer(i_kind),parameter:: nsfc=10

!   Declare local variables
    integer(i_kind) ni1,ni2,i,j,k,icount,icount_prev,latb,lonb
    integer(sfcio_intkind):: irets
    real(r_kind) sumn,sums
    real(r_kind),dimension(itotsub):: buff
    real(r_kind),dimension(lat2*lon2):: sli
    real(r_kind),dimension(lat2*lon2,max(2*nsfc,npe)):: sfcsub
    real(r_kind),dimension(itotsub,nsfc):: sfcges
    real(r_kind),allocatable,dimension(:,:,:):: work
    
    type(sfcio_head):: sfc_head
    type(sfcio_data):: sfc_data

!-----------------------------------------------------------------------------
!   Read surface file
    call sfcio_srohdc(lunges,filename,sfc_head,sfc_data,irets)


!   Check for possible problems
    if (irets /= izero) then
       write(6,*)'READ_GFSSFC:  ***ERROR*** problem reading ',filename,&
            ', irets=',irets
       call sfcio_axdata(sfc_data,irets)
       call stop2(80)
    endif
    latb=sfc_head%latb
    lonb=sfc_head%lonb
    if ( (latb /= nlat-2) .or. &
         (lonb /= nlon) ) then
       write(6,*)'READ_GFSSFC:  ***ERROR*** inconsistent grid dimensions.  ',&
            ', nlon,nlat-2=',nlon,nlat-2,' -vs- sfc file lonb,latb=',&
            lonb,latb
       call sfcio_axdata(sfc_data,irets)
       call stop2(80)
    endif


!   Load surface fields into local work array
    allocate(work(lonb,latb,nsfc))
    do k=1,nsfc
       do j=1,latb
          do i=1,lonb
             work(i,j,k) = zero
          end do
       end do
    end do
    do j=1,latb
       do i=1,lonb
          work(i,j,1) = sfc_data%tsea(i,j)     ! skin temperature
          work(i,j,2) = sfc_data%smc(i,j,1)    ! soil moisture
          work(i,j,3) = sfc_data%sheleg(i,j)   ! snow depth
          work(i,j,4) = sfc_data%stc(i,j,1)    ! soil temperature
          work(i,j,5) = sfc_data%slmsk(i,j)    ! sea/land/ice mask
          work(i,j,6) = sfc_data%vfrac(i,j)    ! vegetation cover
          work(i,j,7) = sfc_data%f10m(i,j)     ! 10m wind factor
          work(i,j,8) = sfc_data%vtype(i,j)    ! vegetation type
          work(i,j,9) = sfc_data%stype(i,j)    ! soil type
          work(i,j,10) = sfc_data%zorl(i,j)    ! surface roughness length (cm)
       end do
    end do
    
!   Fill surface guess array
    do k=1,nsfc

!      Compute mean for southern- and northern-most rows 
!      of surface guess array
       sumn = zero
       sums = zero
       do i=1,nlon
          sumn = work(i,1,k)    + sumn
          sums = work(i,latb,k) + sums
       end do
       sumn = sumn/nlon
       sums = sums/nlon

!      Transfer from local work array to surface guess array
       do i = 1,itotsub
          sfcges(i,k) = zero
          ni1=ltosi_s(i)
          ni2=ltosj_s(i)
          if (ni1>1 .and. ni1<nlat) sfcges(i,k) = work(ni2,nlat-ni1,k)
          if (ni1==1) sfcges(i,k)=sums
          if (ni1==nlat) sfcges(i,k)=sumn
       end do
       
!   End of loop over data records
    end do

!   Deallocate local work arrays
    deallocate(work)
    call sfcio_axdata(sfc_data,irets)


!   Load the sea/land/ice mask into nlat x nlon array
    do i=1,itotsub
       ni1=ltosi_s(i); ni2=ltosj_s(i)
       isli_gl(ni1,ni2)=sfcges(i,5)
    end do
    

!   Scatter these fields to all tasks
    sfcsub=0
    icount=0
    icount_prev=1
    do k=1,nsfc
       icount=icount+1
       if (mype==mod(icount-1,npe)) then
          do i=1,itotsub
             buff(i)=sfcges(i,k)
          end do
       endif
       if (mod(icount,npe)==0 .or. icount==nsfc) then
          call mpi_alltoallv(buff,ijn_s,displs_s,mpi_rtype,&
               sfcsub(1,icount_prev),irc_s,ird_s,mpi_rtype,&
               mpi_comm_world,ierror)
          icount_prev=icount+1
       endif
    end do
    
!   Load data into output arrays
    do k=1,lat2*lon2
       sfct(k)      = sfcsub(k,1)
       soil_moi(k)  = sfcsub(k,2)
       sno(k)       = sfcsub(k,3)
       soil_temp(k) = sfcsub(k,4)
       isli(k)      = nint(sfcsub(k,5)+0.0000001)
       veg_frac(k)  = sfcsub(k,6)
       fact10(k)    = sfcsub(k,7)
       veg_type(k)  = sfcsub(k,8)
       soil_type(k) = sfcsub(k,9)
       sfc_rough(k) = sfcsub(k,10)
    end do

!   Print date/time stamp
    if(mype==izero) then
       write(6,700) nlat,nlon,sfc_head%fhour,sfc_head%idate
700    format('READ_GFSSFC:  ges read/scatter, nlat,nlon=',&
            2i6,', hour=',f10.1,', idate=',4i5)
    end if
    
    return
  end subroutine read_gfssfc


  subroutine read_gfssfc_full(field_g,tag)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    rdbges                     read and reorder bges file   
!   prgmmr: derber           org: np23                date: 1992-09-08
!
! abstract:  This routine extracts a specified record from a GFS
!            surface file.  
!
! program history log:
!   1992-09-08  derber 
!   1995-07-17  derber
!   1998-04-17  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-22  treadon - update documentation
!   2004-08-03  treadon - add only to module use, add intent in/out
!   2004-12-23  treadon - remove write(6,*) of hour & date information
!   2005-01-27  treadon - rewrite to make use of sfcio module
!   2005-02-18  todling - added protection to dealloc() of sfc-data array
!   2006-04-06  middlecoff - change lunges from 15 to 11
!
!   input argument list:
!     tag     - character string idenfying field to extract
!
!   output argument list:
!     field_g - irec-th record from GFS surface file.  The suffix
!                 "_g" indicates that the extracted record is on
!                  the global (full domain) grid.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: i_kind,r_kind,r_single
    use constants, only: zero
    use gridmod, only: nlat,nlon
    use sfcio_module, only: sfcio_intkind,sfcio_head,sfcio_data,&
         sfcio_srohdc,sfcio_axdata
    implicit none
  
!   Declare local parameters
    character(6),parameter:: sfcfile = 'sfcf06'  ! name of surface guess file
    integer(sfcio_intkind),parameter:: lunges=11 ! unit for surface file


!   Below are names of fields that may currently be extracted
!   from surface file.  There are more fields in surface file.
!   The three fields below are the only ones currently used

    character(6),parameter:: tsea   = 'tsea'     ! skin 
    character(6),parameter:: slmsk  = 'slmsk'
    character(6),parameter:: sheleg = 'sheleg'

!   Declare passed variables
    character(6),intent(in):: tag
    real(r_kind),dimension(nlat,nlon),intent(out):: field_g

!   Declare local variables  
    integer(sfcio_intkind):: iret
    integer(i_kind) latb,lonb,i,j
    real(r_kind) sums,sumn
    real(r_kind),allocatable,dimension(:,:):: work
    
    type(sfcio_head):: sfc_head
    type(sfcio_data):: sfc_data

!-----------------------------------------------------------------------------
!   Zero output array
    field_g=zero

!   Read surface file
    call sfcio_srohdc(lunges,sfcfile,sfc_head,sfc_data,iret)

!   Check for possible problems
    if (iret /= 0) then
       write(6,*)'READ_GFSSFC_FULL:  ***ERROR*** problem reading ',sfcfile,&
            ', iret=',iret
       call stop2(80)
    endif
    latb=sfc_head%latb
    lonb=sfc_head%lonb
    if ( (latb /= nlat-2) .or. &
         (lonb /= nlon) ) then
       write(6,*)'READ_GFSSFC_FULL:  ***ERROR*** inconsistent grid dimensions.  ',&
            ', nlon,nlat=',nlon,nlat-2,' -vs- sfc file lonb,latb=',&
            lonb,latb
       call stop2(80)
    endif
    
!   Allocate work array and extract requested field
    allocate(work(lonb,latb))
    if (trim(adjustl(tag)) == tsea) then
       do j=1,latb
          do i=1,lonb
             work(i,j) = sfc_data%tsea(i,j)
          end do
       end do
       
    elseif (trim(adjustl(tag)) == sheleg) then
       do j=1,latb
          do i=1,lonb
             work(i,j) = sfc_data%sheleg(i,j)
          end do
       end do
       
    elseif (trim(adjustl(tag)) == slmsk) then
       do j=1,latb
          do i=1,lonb
             work(i,j) = sfc_data%slmsk(i,j)
          end do
       end do
       
    else
       write(6,*)'READ_GFSSFC_FULL:  ***ERROR*** passed tag = ',tag,&
            ' is not yet supported'
       call stop2(81)
    endif

!   Fill in southern and northern rows of output array
    sumn = zero
    sums = zero
    do i=1,nlon
       sumn = work(i,1)    + sumn
       sums = work(i,latb) + sums
    end do
    sumn = sumn/nlon
    sums = sums/nlon
    
!   Load output array
    do j=1,nlon
       do i=1,nlat-2
          field_g(i+1,j) = work(j,nlat-1-i)
       end do
    end do
    
    do j=1,nlon
       field_g(1,j)    = sums
       field_g(nlat,j) = sumn
    end do
    
!   Deallocate work arrays
    deallocate(work)
    call sfcio_axdata(sfc_data,iret)

!   Check for possible problems
    if (iret /= 0) then
       write(6,*)'READ_GFSSFC_FULL:  ***ERROR*** ',&
            'problem dealloc mem for sfc-data iret=',iret
       call stop2(80)
    endif
    
    return    
  end subroutine read_gfssfc_full
  
  subroutine write_gfs(mype,mype_atm,mype_sfc)

!
!   2006-07-31  kleist - pass ges_ps instead of ges_lnps
!   2006-10-11  treadon - update 10m wind factor in sfc file
!
    use kinds, only: i_kind,r_kind
    use guess_grids, only: ges_z,ges_ps,ges_vor,ges_div,&
         ges_tv,ges_q,ges_oz,ges_cwmr,sfct,fact10
    use guess_grids, only: ntguessig,ntguessfc
    use gridmod, only: lat2,lon2    

    implicit none

    integer(i_kind),intent(in):: mype,mype_atm,mype_sfc
    integer(i_kind) i,j
    character(24):: filename

!   Write atmospheric analysis file
    filename='siganl'
    call write_gfsatm(filename,mype,mype_atm,&
         ges_z(1,1,ntguessig),ges_ps(1,1,ntguessig),&
         ges_vor(1,1,1,ntguessig),ges_div(1,1,1,ntguessig),&
         ges_tv(1,1,1,ntguessig),ges_q(1,1,1,ntguessig),&
         ges_oz(1,1,1,ntguessig),ges_cwmr(1,1,1,ntguessig))

!   Write surface analysis file
    filename='sfcanl.gsi'
    call write_gfssfc(filename,mype,mype_sfc,&
         sfct(1,1,ntguessfc),fact10(1,1,ntguessfc))
  end subroutine write_gfs

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  write_gfsatm --- Gather, transform, and write out spectal coefficients
!
! !INTERFACE:
!

  subroutine write_gfsatm(filename,mype,mype_out,sub_z,sub_ps,&
       sub_vor,sub_div,sub_tv,sub_q,sub_oz,sub_cwmr)
!
! !USES:
!
    use kinds, only: r_kind,i_kind
    
    use constants, only: zero_single
  
    use mpimod, only: mpi_rtype
    use mpimod, only: mpi_comm_world
    use mpimod, only: strip
    use mpimod, only: ierror
    
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
    
    use obsmod, only: iadate
    
    use specmod, only: nc
    use specmod, only: jcap
    use specmod, only: factsml
    use specmod, only: factvml
    
    use sigio_module, only: sigio_intkind,sigio_head,sigio_data,&
         sigio_sropen,sigio_srhead,sigio_sclose,sigio_aldata,&
         sigio_swopen,sigio_swhead,sigio_swdata,sigio_axdata

  
    implicit none

!
! !LOCAL PARAMETER:
! 
!
! !INPUT PARAMETERS:
!

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
    
!
! !OUTPUT PARAMETERS:
!

! !DESCRIPTION: This routine gathers fields needed for the GSI analysis
!           file from subdomains and then transforms the fields from
!           grid to spectral space.  The spectral coefficients are 
!           then written to an atmospheric analysis file.
!
! !REVISION HISTORY:
!
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
!
! !REMARKS:
!
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
! !AUTHOR:
!
!   1990-10-10  parrish    - author; org: np22
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind),parameter::  lunges = 11
    integer(i_kind),parameter::  lunanl = 51

    character(6):: fname_ges
    integer(i_kind) i,k,mm1,ii,ncloud,ntracer
    integer(sigio_intkind):: iret
    
    real(r_kind),dimension(lat1*lon1):: zsm,psm,lnpsm
    real(r_kind),dimension(lat1*lon1,nsig):: tvsm,vorsm,divsm,qsm,ozsm,cwmrsm
    real(r_kind),dimension(max(iglobal,itotsub)):: work1
    real(r_kind),dimension(nlon,nlat-2):: grid
    real(r_kind),dimension(nc):: spec_work
    
    type(sigio_head):: atm_head
    type(sigio_data):: atm_data

!*************************************************************************

!   Initialize local variables
    mm1=mype+1

!   Let all tasks read header record from guess file
    write(fname_ges,100) ifilesig(ntguessig)
100 format('sigf',i2.2)
    call sigio_sropen(lunges,fname_ges,iret)
    call sigio_srhead(lunges,atm_head,iret)
    call sigio_sclose(lunges,iret)

!   Extract number of traces and cloud types
    ntracer = atm_head%ntrac
    ncloud  = atm_head%ncldt

!   Construct and write header record.  Only mpi task mype_out does this.
    if(mype==mype_out) then

!      Replace header record date with analysis time
       atm_head%fhour    = zero_single
       atm_head%idate(1) = iadate(4) !hour
       atm_head%idate(2) = iadate(2) !month
       atm_head%idate(3) = iadate(3) !day
       atm_head%idate(4) = iadate(1) !year
       
!      Write header to analysis file
       call sigio_swopen(lunanl,filename,iret)
       call sigio_swhead(lunanl,atm_head,iret)
       
!      Allocate structure arrays to hold data
       call sigio_aldata(atm_head,atm_data,iret)

    end if


!   Strip off boundary points from subdomains
    call strip(sub_z,zsm,1)
    call strip(sub_ps,psm,1)
    call strip(sub_vor,vorsm,nsig)
    call strip(sub_div,divsm,nsig)
    call strip(sub_tv,tvsm,nsig)
    call strip(sub_q,qsm,nsig)
    call strip(sub_oz,ozsm,nsig)
    call strip(sub_cwmr,cwmrsm,nsig)
  

!   For each output grid, the following steps are repeated
!     1) create global grid by gathering from subdomains
!     2) transfrom from grid space representation to spectral coefficients
!     3) apply factor to ensure certain coefficients are zero
!     4) write spectral coefficients to output file
!   Note that steps 2-4 are done on a single task (here mpi task 0)


!   Terrain
    call mpi_gatherv(zsm,ijn(mm1),mpi_rtype,&
         work1,ijn,displs_g,mpi_rtype,&
         mype_out,mpi_comm_world,ierror)
    if (mype==mype_out) then
       call load_grid(work1,grid)
       call sptez_s(spec_work,grid,-1)
       ii=0
       do i=1,nc-1,2  ! unroll loop once to improve computational efficiency
          ii=ii+2
          atm_data%hs(i) =spec_work(i) *factsml(i)
          atm_data%hs(ii)=spec_work(ii)*factsml(ii)
       end do
       if (mod(nc,2)==1) atm_data%hs(nc)=spec_work(nc)*factsml(nc)
    endif
    

!   Surface pressure to ln(surface pressure)
    do i=1,lat1*lon1
       lnpsm(i)=log(psm(i))
    end do
    call mpi_gatherv(lnpsm,ijn(mm1),mpi_rtype,&
         work1,ijn,displs_g,mpi_rtype,&
         mype_out,mpi_comm_world,ierror)
    if (mype==mype_out) then
       call load_grid(work1,grid)
       call sptez_s(spec_work,grid,-1)
       ii=0
       do i=1,nc-1,2
          ii=ii+2
          atm_data%ps(i) =spec_work(i) *factsml(i)
          atm_data%ps(ii)=spec_work(ii)*factsml(ii)
       end do
       if (mod(nc,2)==1) atm_data%ps(nc)=spec_work(nc)*factsml(nc)
    endif
    

!   Virtual temperature
    do k=1,nsig
       call mpi_gatherv(tvsm(1,k),ijn(mm1),mpi_rtype,&
            work1,ijn,displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype==mype_out) then
          call load_grid(work1,grid)
          call sptez_s(spec_work,grid,-1)
          ii=0
          do i=1,nc-1,2
             ii=ii+2
             atm_data%t(i,k) =spec_work(i) *factsml(i)
             atm_data%t(ii,k)=spec_work(ii)*factsml(ii)
          end do
          if (mod(nc,2)==1) atm_data%t(nc,k)=spec_work(nc)*factsml(nc)
       endif
    end do

  
!   Horizontal divergence and voriticy
    do k=1,nsig
       call mpi_gatherv(divsm(1,k),ijn(mm1),mpi_rtype,&
            work1,ijn,displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype==mype_out) then
          call load_grid(work1,grid)
          call sptez_s(spec_work,grid,-1)
          ii=0
          do i=1,nc-1,2
             ii=ii+2
             atm_data%d(i,k) =spec_work(i) *factvml(i)
             atm_data%d(ii,k)=spec_work(ii)*factvml(ii)
          end do
          if (mod(nc,2)==1) atm_data%d(nc,k)=spec_work(nc)*factvml(nc)
       endif
       
       call mpi_gatherv(vorsm(1,k),ijn(mm1),mpi_rtype,&
            work1,ijn,displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype==mype_out) then
          call load_grid(work1,grid)
          call sptez_s(spec_work,grid,-1)
          ii=0
          do i=1,nc-1,2
             ii=ii+2
             atm_data%z(i,k) =spec_work(i) *factvml(i)
             atm_data%z(ii,k)=spec_work(ii)*factvml(ii)
          end do
          if (mod(nc,2)==1) atm_data%z(nc,k)=spec_work(nc)*factvml(nc)
       endif
    end do
    

!   Specific humidity
    do k=1,nsig
       call mpi_gatherv(qsm(1,k),ijn(mm1),mpi_rtype,&
            work1,ijn,displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype==mype_out) then
          call load_grid(work1,grid)
          call sptez_s(spec_work,grid,-1)
          ii=0
          do i=1,nc-1,2
             ii=ii+2
             atm_data%q(i,k,1) =spec_work(i) *factsml(i)
             atm_data%q(ii,k,1)=spec_work(ii)*factsml(ii)
          end do
          if (mod(nc,2)==1) atm_data%q(nc,k,1)=spec_work(nc)*factsml(nc)
       endif
    end do
    

!   Ozone
    do k=1,nsig
       call mpi_gatherv(ozsm(1,k),ijn(mm1),mpi_rtype,&
            work1,ijn,displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype==mype_out) then
          call load_grid(work1,grid)
          call sptez_s(spec_work,grid,-1)
          ii=0
          do i=1,nc-1,2
             ii=ii+2
             atm_data%q(i,k,2) =spec_work(i) *factsml(i)
             atm_data%q(ii,k,2)=spec_work(ii)*factsml(ii)
          end do
          if (mod(nc,2)==1) atm_data%q(nc,k,2)=spec_work(nc)*factsml(nc)
       endif
    end do
    

!   Cloud condensate mixing ratio
    if (ntracer>2 .or. ncloud>=1) then
       do k=1,nsig
          call mpi_gatherv(cwmrsm(1,k),ijn(mm1),mpi_rtype,&
               work1,ijn,displs_g,mpi_rtype,&
               mype_out,mpi_comm_world,ierror)
          if (mype==mype_out) then
             call load_grid(work1,grid)
             call sptez_s(spec_work,grid,-1)
             ii=0
             do i=1,nc-1,2
                ii=ii+2
                atm_data%q(i,k,3) =spec_work(i) *factsml(i)
                atm_data%q(ii,k,3)=spec_work(ii)*factsml(ii)
             end do
             if (mod(nc,2)==1) atm_data%q(nc,k,3)=spec_work(nc)*factsml(nc)
          endif
       end do
    endif
    

!   Single task writes analysis data to analysis file
    if (mype==mype_out) then
       call sigio_swdata(lunanl,atm_head,atm_data,iret)
       
       write(6,110) jcap,nsig,atm_head%fhour,atm_head%idate,iret
110    format('WRITE_GFSATM:  atm analysis written for ',&
            2i6,1x,f3.1,4(i4,1x),' with iret=',i2)
       
       call sigio_axdata(atm_data,iret)
    endif
    
    return
  end subroutine write_gfsatm

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  write_gfssfc --- Write surface analysis to file
!
! !INTERFACE:
!

  subroutine write_gfssfc(filename,mype,mype_sfc,sfct,fact10)

!
! !USES:
!
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
    
    use constants, only: zero_single
    
    use sfcio_module, only: sfcio_intkind,sfcio_head,sfcio_data,&
         sfcio_srohdc,sfcio_swohdc,sfcio_axdata
    
    implicit none
!
! !INPUT PARAMETERS:
!
    character(24),intent(in):: filename  ! file to open and write to

    real(r_kind),dimension(lat2,lon2), intent(in) :: sfct   ! updated skin temperature
    real(r_kind),dimension(lat2,lon2), intent(in) :: fact10 ! updated 10m wind factor

    integer(i_kind),              intent(in)  :: mype     ! mpi task number
    integer(i_kind),              intent(in)  :: mype_sfc ! mpi task to write output file
!
! !OUTPUT PARAMETERS:
!

! !DESCRIPTION: This routine writes the updated surface analysis.  At
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
! !REVISION HISTORY:
!
!   2004-06-15  treadon -  updated documentation
!   2004-07-15  todling -  protex-compliant prologue; added intent/only's
!   2004-12-03  treadon -  replace mpe_igatherv (IBM extension) with
!                          standard mpi_gatherv
!   2005-01-27  treadon - rewrite to make use of sfcio module
!   2005-02-09  kleist  - clean up unit number and filename for updated surface file
!   2005-03-07  todling -  die gracefully when return error from sfcio
!   2005-03-10  treadon - remove iadate from calling list, access via obsmod
!   2006-10-11  treadon - update 10m wind factor in sfc file
!
! !REMARKS:
!
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
! !AUTHOR:
!
!   2003-07-03  treadon -  initial version; org: np22
!
!EOP
!-------------------------------------------------------------------------

!   Declare local parameters
    character( 6),parameter:: fname_ges='sfcf06'
    integer(sfcio_intkind),parameter:: ioges = 12
    integer(sfcio_intkind),parameter:: ioanl = 52
    integer(i_kind),parameter:: nsfc=2
    integer(i_kind),parameter:: itsk=1
    integer(i_kind),parameter:: if10=2


    real(r_kind),parameter :: houra = zero_single

!   Declare local variables
    integer(sfcio_intkind):: iret
    integer(i_kind) latb,lonb,nlatm2
    integer(i_kind) latd,lonl,version
    integer(i_kind) i,j,k,ip1,jp1,ilat,ilon,jj,mm1

    real(r_single),dimension(nlon,nlat-2,nsfc):: buffer
    real(r_kind),dimension(lat1,lon1,nsfc):: sfcsub
    real(r_kind),dimension(nlon,nlat):: grid
    real(r_kind),dimension(max(iglobal,itotsub),nsfc):: sfcall

    type(sfcio_head):: head
    type(sfcio_data):: data

  
!*****************************************************************************

!   Initialize local variables
    mm1=mype+1
    nlatm2=nlat-2

!   Gather skin temperature information from all tasks.  
    do j=1,lon1
       jp1 = j+1
       do i=1,lat1
          ip1 = i+1
          sfcsub(i,j,itsk)=sfct(ip1,jp1)
          sfcsub(i,j,if10)=fact10(ip1,jp1)
       end do
    end do
    do k=1,nsfc
       call mpi_gatherv(sfcsub(1,1,k),ijn(mm1),mpi_rtype,&
            sfcall(1,k),ijn,displs_g,mpi_rtype,mype_sfc,&
            mpi_comm_world,ierror)
    end do

    

!   Only MPI task mype_sfc writes the surface file.
    if (mype==mype_sfc) then


!      For now, rather than carry around all the surface fields in memory from
!      the read in ingesfc, just read fields from surface file.  Also, for
!      now, only update the 6-hour forecast surface guess file.

!      Read surface guess file
       call sfcio_srohdc(ioges,fname_ges,head,data,iret)
       if (iret /= 0) then
          write(6,*)'WRITE_GFSSFC:  ***ERROR*** problem reading ',fname_ges,&
               ', iret=',iret
          call sfcio_axdata(data,iret)
          call stop2(80)
       endif
       latb=head%latb
       lonb=head%lonb
       if ( (latb /= nlatm2) .or. &
            (lonb /= nlon) ) then
          write(6,*)'WRITE_GFSSFC:  ***ERROR*** inconsistent grid dimensions.  ',&
               ', nlon,nlat-2=',nlon,nlatm2,' -vs- sfc file lonb,latb=',&
               lonb,latb
          call sfcio_axdata(data,iret)
          call stop2(80)
       endif

!      Update guess date/time to analysis date/time
       head%fhour = houra       ! forecast hour
       head%idate(1)=iadate(4)  ! hour
       head%idate(2)=iadate(2)  ! month
       head%idate(3)=iadate(3)  ! day
       head%idate(4)=iadate(1)  ! year

!      Reorder updated skin temperature to output format
       do k=1,nsfc
          do i=1,iglobal
             ilon=ltosj(i)
             ilat=ltosi(i)
             grid(ilon,ilat)=sfcall(i,k)
          end do
          do j=1,nlatm2
             jj=nlat-j
             do i=1,nlon
                buffer(i,j,k)=grid(i,jj)
             end do
          end do
       end do

       do j=1,nlatm2
          do i=1,nlon
             data%tsea(i,j) = buffer(i,j,itsk)
             data%f10m(i,j) = buffer(i,j,if10)
          end do
       end do

!      Write updated information to surface analysis file
       call sfcio_swohdc(ioanl,filename,head,data,iret)


!      Deallocate local work arrays
       call sfcio_axdata(data,iret)

       write(6,100) nlon,nlatm2,houra,iadate(1:4),iret
100    format(' WRITE_GFSSFC:  sfc analysis written  for ',&
            2i6,1x,f3.1,4(i4,1x),' with iret=',i2)

    endif
    
!   End of routine
    return
  end subroutine write_gfssfc

end module ncepgfs_io
