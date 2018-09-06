#ifdef WRF

subroutine read_wrf_mass_netcdf_guess4ens(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_wrf_mass_guess      read wrf_mass interface file
!   prgmmr: parrish          org: np22                date: 2003-09-05
!
! abstract: in place of read_guess for global application, read guess
!             from regional model, in this case the wrf mass core model.
!             This version reads a binary file created
!             in a previous step that interfaces with the wrf infrastructure.
!             A later version will read directly from the wrf restart file.
!             The guess is read in by complete horizontal fields, one field
!             per processor, in parallel.  Each horizontal input field is 
!             converted from the staggered c-grid to an unstaggered a-grid.
!             On the c-grid, u is shifted 1/2 point in the negative x direction
!             and v 1/2 point in the negative y direction, but otherwise the
!             three grids are regular.  When the fields are read in, nothing
!             is done to mass variables, but wind variables are interpolated to
!             mass points.
!
! program history log:
!   2014-12-23  Hu
!
!   input argument list:
!     mype     - pe number
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_single,i_kind
  use mpimod, only: mpi_sum,mpi_integer,mpi_real4,mpi_comm_world,npe,ierror
  use mpimod, only: npe
  use guess_grids, only: nfldsig,ifilesig,ntguessig
  use gridmod, only: lat2,lon2,nlat_regional,nlon_regional,nlon, nlat,&
       nsig,nsig_soil,ijn_s,displs_s,eta1_ll,pt_ll,itotsub,aeta1_ll
  use constants, only: zero,one,grav,fv,zero_single,rd_over_cp_mass,one_tenth,r10,r100
  use constants, only: r0_01, tiny_r_kind
  use gsi_io, only: lendian_in
  use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info
  use general_sub2grid_mod, only: general_grid2sub,general_sub2grid
  use gsi_bundlemod, only: GSI_BundleGetPointer
  use gsi_metguess_mod, only: gsi_metguess_get,GSI_MetGuess_Bundle
  use mpeu_util, only: die
  use mod_wrfmass_to_a, only: wrfmass_h_to_a4

  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype

! Declare local parameters
  real(r_kind),parameter:: rough_default=0.05_r_kind
  character(len=*),parameter::myname='read_wrf_mass_netcdf_guess::'

! Declare local variables

  real(r_kind), pointer :: ges_ps_it (:,:  )=>NULL()
  real(r_kind), pointer :: ges_z_it  (:,:  )=>NULL()

  real(r_single) :: ges_vpt_it  (lat2,lon2 )
! other internal variables
  type(sub2grid_info) grd
  real(r_single),allocatable::temp1(:,:)
  character(6) filename 
  integer(i_kind) ifld,im,jm,lm,num_mass_fields
  integer(i_kind) i,icount,icount_prev,it,j,k
  real(r_kind) psfc_this,psfc_this_dry,sm_this,xice_this
  real(r_kind),dimension(lat2,lon2):: q_integral
  real(r_kind) deltasigma
  real(r_kind):: work_prsl,work_prslk
  integer(i_kind) ier, istatus
  integer(i_kind) nguess
  logical regional
  logical,allocatable :: vector(:)
  integer(i_kind) inner_vars,num_fields
  real(r_kind),allocatable :: work_sub(:,:,:,:),work_reg(:,:,:,:)
  real(r_single) :: ges_ps(lat2,lon2)
  real(r_single) :: bb(nlon,nlat)

!  WRF MASS input grid dimensions in module gridmod
!      These are the following:
!          im -- number of x-points on C-grid
!          jm -- number of y-points on C-grid
!          lm -- number of vertical levels ( = nsig for now)

     if(mype==0) write(6,*)' at 0 in read_wrf_mass_guess4ens'
     regional=.true.

! Big section of operations done only on first outer iteration

     im=nlon_regional
     jm=nlat_regional
     lm=nsig
     nfldsig=1

     do it=1,nfldsig
        write(filename,'("sigf",i2.2)') it
        open(lendian_in,file=filename,form='unformatted') ; rewind lendian_in
        write(6,*)'READ_WRF_MASS_GUESS:  open lendian_in=',lendian_in,' to file=',filename

! get pointers for typical meteorological fields
        ier=0
        call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it),'ps',ges_ps_it, &
                                                 istatus );ier=ier+istatus
        call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it),'z', ges_z_it, &
                                                 istatus );ier=ier+istatus
        if (ier/=0) call die(trim(myname),'cannot get pointers for met-fields,ier =',ier)
!
! skip some record
        read(lendian_in)   ! head
        read(lendian_in)   ! aeta1
        read(lendian_in)   ! eta1
        read(lendian_in)   ! glat,dx_mc
        read(lendian_in)   ! glon,dy_mc

     enddo
! 
     allocate(temp1(im,jm))
     
     inner_vars=1
     num_fields=1 ! mu and qall
     allocate(vector(num_fields))
     vector=.false.
     call general_sub2grid_create_info(grd,inner_vars,nlat,nlon,1,num_fields,regional,vector)
     allocate(work_reg(grd%nlat,grd%nlon,grd%kbegin_loc:grd%kend_alloc,1))
     allocate(work_sub(grd%lat2,grd%lon2,num_fields,1))

! read surface dry pressure:
       read(lendian_in) ((temp1(i,j),i=1,im),j=1,jm)
       if(nlon == nlon_regional .and. nlat == nlat_regional) then
          bb=temp1
       else
          call wrfmass_h_to_a4(temp1,bb)
       endif
        
       do j=1,grd%nlat
       do i=1,grd%nlon
         work_reg(j,i,grd%kbegin_loc:grd%kend_alloc,1)=bb(i,j)
       enddo
       enddo
! next general_grid2sub to go to regional grid subdomains.
    call general_grid2sub(grd,work_reg,work_sub)
    ges_ps(:,:)=work_sub(:,:,1,1)
    write(*,'(a,I5,2f15.7)') 'ges_ps=',mype,maxval(ges_ps_it), &
                                minval(ges_ps_it)

! read qvapor total
       read(lendian_in) ((temp1(i,j),i=1,im),j=1,jm)
       if(nlon == nlon_regional .and. nlat == nlat_regional) then
          bb=temp1
       else
          call wrfmass_h_to_a4(temp1,bb)
       endif

        do j=1,grd%nlat
        do i=1,grd%nlon
          work_reg(j,i,grd%kbegin_loc:grd%kend_alloc,1)=bb(i,j)
        enddo
        enddo
! next general_grid2sub to go to regional grid subdomains.
    call general_grid2sub(grd,work_reg,work_sub)
    ges_vpt_it(:,:)=work_sub(:,:,1,1)
    write(*,'(a,I5,2f15.7)') 'ges_vpt_it=',mype,maxval(ges_vpt_it), &
                                minval(ges_vpt_it)

! read topo
       read(lendian_in) ((temp1(i,j),i=1,im),j=1,jm)
       if(nlon == nlon_regional .and. nlat == nlat_regional) then
          bb=temp1
       else
          call wrfmass_h_to_a4(temp1,bb)
       endif

        do j=1,grd%nlat
        do i=1,grd%nlon
          work_reg(j,i,grd%kbegin_loc:grd%kend_alloc,1)=bb(i,j)
        enddo
        enddo
! next general_grid2sub to go to regional grid subdomains.
    call general_grid2sub(grd,work_reg,work_sub)
    ges_z_it(:,:)=work_sub(:,:,1,1)/grav
!    write(*,'(a,I5,2f15.7)') 'ges_z_it=',mype,maxval(ges_z_it), &
!                                minval(ges_z_it)

    close(lendian_in)
    it=1
    do i=1,lon2
       do j=1,lat2
!       Convert psfc units of mb and then convert to log(psfc) in cb
           psfc_this_dry=r0_01*ges_ps(j,i)
           psfc_this=(psfc_this_dry-pt_ll)*ges_vpt_it(j,i)+pt_ll
           ges_ps_it(j,i)=one_tenth*psfc_this   ! convert from mb to cb
       end do
    end do
!    write(*,*) 'final ps==',mype,maxval(ges_ps_it(:,:)),minval(ges_ps_it(:,:))
        
    deallocate(work_reg)
    deallocate(work_sub)
    deallocate(temp1)

    return
end subroutine read_wrf_mass_netcdf_guess4ens
#else /* Start no WRF-library block */
subroutine read_wrf_mass_netcdf_guess4ens(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_wrf_mass_netcdf_guess
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-12-07  lueken - added subprogram doc block and implicit none
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds,only: i_kind
  implicit none
  integer(i_kind),intent(in)::mype
  write(6,*)'READ_WRF_MASS_NETCDF_GUESS:  dummy routine, does nothing!'
end subroutine read_wrf_mass_netcdf_guess4ens
#endif /* End no WRF-library block */
