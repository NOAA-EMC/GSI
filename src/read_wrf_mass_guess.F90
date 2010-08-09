#ifdef WRF
subroutine read_wrf_mass_binary_guess(mype)
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
!   2004--7-15  parrish
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-09-10  parrish - correct error in land-sea mask interpretation
!   2004-11-08  parrish - change to mpi-io for binary file format
!   2004-12-15  treadon - remove variable mype from call load_geop_hgt, 
!                         rename variable wrf_ges_filename as wrfges
!   2005-02-17  todling - ifdef'ed wrf code out
!   2005-02-23  wu - setup for qoption=2 and output stats of RH
!   2005-04-01  treadon - add initialization of ges_oz, prsi_oz; comestic format changes
!   2005-05-27  parrish - add call get_derivatives
!   2005-11-21  kleist - new call to genqsat
!   2005-11-21  derber - make qoption=1 work same as qoption=2
!   2005-11-29  derber - remove external iteration dependent calculations
!   2005-12-15  treadon - remove initialization of certain guess arrays (done elsewhere)
!   2006-02-02  treadon - remove load_prsges,load_geop_hgt,prsl,prslk (not used)
!   2006-02-15  treadon - convert moisture mixing ratio to specific humidity
!   2006-03-07  treadon - convert guess potential temperature to vritual temperature
!   2006-04-06  middlecoff - changed nfcst from 11 to lendian_in
!   2006-07-28  derber  - include sensible temperatures
!   2006-07-31  kleist - change to use ges_ps instead of lnps
!   2007-03-13  derber - remove unused qsinv2 from jfunc use list
!   2007-04-12  parrish - add modifications to allow any combination of ikj or ijk
!                          grid ordering for input 3D fields
!   2008-04-16  safford - rm unused uses
!   2010-06-24  hu     - add code to read in cloud/hydrometeor fields
!                             and distributed them to all processors
!
!   input argument list:
!     mype     - pe number
!
!     NOTES:  need to pay special attention to various surface fields to make sure
!             they are correct (check units).  fact10 needs to be computed from 
!             10m wind, which is included in wrf mass interface file.
!
!             Cloud water currently set to zero.  Need to fix before doing precip
!             assimilation--wait until global adopts Brad Ferrier's multi-species 
!             scheme??
!
!             Ozone currently set to zero.  No ozone variable in wrf mass core--climatology
!             instead.  Do we use climatology?
!
!             No background bias yet. (biascor ignored)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$  end documentation block
  use kinds, only: r_kind,r_single,i_long,i_llong,i_kind
  use mpimod, only: mpi_sum,mpi_integer,mpi_comm_world,npe,ierror, &
       mpi_offset_kind,mpi_info_null,mpi_mode_rdonly,mpi_status_size
  use guess_grids, only: ges_z,ges_ps,ges_tv,ges_q,ges_u,ges_v,&
       fact10,soil_type,veg_frac,veg_type,sfct,sno,soil_temp,soil_moi,&
       isli,nfldsig,ifilesig,ges_tsen
  use guess_grids, only: ges_qc,ges_qi,ges_qr,ges_qs,ges_qg,   &
       ges_xlon,ges_xlat,soil_temp_cld,isli_cld,ges_tten
  use gridmod, only: lat2,lon2,nlat_regional,nlon_regional,&
       nsig,eta1_ll,pt_ll,itotsub,aeta1_ll
  use constants, only: izero,ione,zero,one,grav,fv,zero_single,rd_over_cp_mass,one_tenth,h300
  use gsi_io, only: lendian_in
  use rapidrefresh_cldsurf_mod, only: l_cloud_analysis
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype

! Declare local parameters
  real(r_kind),parameter:: r0_01 = 0.01_r_kind
  real(r_kind),parameter:: r10   = 10.0_r_kind
  real(r_kind),parameter:: r100  = 100.0_r_kind


! Declare local variables
  integer(i_kind) kt,kq,ku,kv

! MASS variable names stuck in here
  integer(i_kind) mfcst

! other internal variables
  real(r_single),allocatable::tempa(:,:)
  real(r_single),allocatable::temp1(:,:),temp1u(:,:),temp1v(:,:)
  real(r_single),allocatable::all_loc(:,:,:)
  integer(i_kind),allocatable::igtype(:),kdim(:),kord(:)
  integer(kind=mpi_offset_kind),allocatable::offset(:)
  integer(kind=mpi_offset_kind) this_offset
  integer(i_kind),allocatable::length(:)
  integer(i_kind) this_length
  character(6) filename 
  character(9) wrfges
  integer(i_kind) ifld,im,jm,lm,num_mass_fields
  integer(i_kind) num_loc_groups,num_j_groups
  integer(i_kind) i,it,j,k
  integer(i_kind) i_mub,i_mu,i_fis,i_t,i_q,i_u,i_v,i_sno,i_u10,i_v10,i_smois,i_tslb
  integer(i_kind) i_sm,i_xice,i_sst,i_tsk,i_ivgtyp,i_isltyp,i_vegfrac
  integer(i_kind) isli_this
  real(r_kind) psfc_this,psfc_this_dry,sm_this,xice_this
  real(r_kind),dimension(lat2,lon2):: q_integral
  real(r_kind),dimension(lat2,lon2,nsig):: ges_pot
  integer(i_kind) num_doubtful_sfct,num_doubtful_sfct_all
  real(r_kind) deltasigma
  integer(i_llong) n_position
  integer(i_kind) iskip,ksize,jextra,nextra
  integer(i_kind) status(mpi_status_size)
  integer(i_kind) jbegin(0:npe),jend(0:npe-ione),jend2(0:npe-ione)
  integer(i_kind) kbegin(0:npe),kend(0:npe-ione)
  integer(i_long),allocatable:: ibuf(:,:)
  integer(i_long),allocatable:: jbuf(:,:,:)
  integer(i_long) dummy9(9)
  real(r_single) pt_regional_single
  real(r_kind):: work_prsl,work_prslk
  integer(i_kind) i_qc,i_qi,i_qr,i_qs,i_qg,kqc,kqi,kqr,kqs,kqg,i_xlon,i_xlat,i_tt,ktt


  integer(i_kind) iadd
  character(132) memoryorder

!  WRF MASS input grid dimensions in module gridmod
!      These are the following:
!          im -- number of x-points on C-grid
!          jm -- number of y-points on C-grid
!          lm -- number of vertical levels ( = nsig for now)

     num_doubtful_sfct=izero



     im=nlon_regional
     jm=nlat_regional
     lm=nsig
     if(jm<=npe)then
        write(6,*)' in read_wrf_mass_binary_guess, jm <= npe, ',&
                   'so program will end.'
        call stop2(1)
     endif

     if(mype==izero) write(6,*)' in read_wrf_mass_binary_guess, im,jm,lm=',im,jm,lm

!    Following is for convenient WRF MASS input
     num_mass_fields=21_i_kind+5_i_kind*lm+2_i_kind+2_i_kind
     if(l_cloud_analysis) num_mass_fields=21_i_kind+5_i_kind*lm+2_i_kind+2_i_kind+6_i_kind*lm+2_i_kind
     num_loc_groups=num_mass_fields/npe
     if(mype==izero) write(6,'(" at 1 in read_wrf_mass_guess, lm            =",i6)')lm
     if(mype==izero) write(6,'(" at 1 in read_wrf_mass_guess, num_mass_fields=",i6)')num_mass_fields
     if(mype==izero) write(6,'(" at 1 in read_wrf_mass_guess, nfldsig       =",i6)')nfldsig
     if(mype==izero) write(6,'(" at 1 in read_wrf_mass_guess, npe           =",i6)')npe
     if(mype==izero) write(6,'(" at 1 in read_wrf_mass_guess, num_loc_groups=",i6)')num_loc_groups

     allocate(offset(num_mass_fields))
     allocate(igtype(num_mass_fields),kdim(num_mass_fields),kord(num_mass_fields))
     allocate(length(num_mass_fields))

     
!    igtype is a flag indicating whether each input MASS field is h-, u-, or v-grid
!    and whether integer or real
!     abs(igtype)=1 for h-grid
!                =2 for u-grid
!                =3 for v-grid
!               
!     igtype < 0 for integer field
!     igtype = 0  for dummy (nothing to read)

!    offset is the byte count preceding each record to be read from the wrf binary file.
!       used as individual file pointers by mpi_file_read_at

     do it=1,nfldsig
        write(filename,'("sigf",i2.2)')ifilesig(it)
        open(lendian_in,file=filename,form='unformatted') ; rewind lendian_in
        write(6,*)'READ_WRF_MASS_BINARY_GUESS:  open lendian_in=',lendian_in,&
             ' to filename=',filename,' on it=',it
        if(mype == izero) write(6,*)'READ_WRF_MASS_OFFSET_FILE:  open lendian_in=',lendian_in,' to file=',filename
        read(lendian_in) dummy9,pt_regional_single
        write(6,*)'READ_WRF_MASS_BINARY_GUESS:  dummy9=',dummy9

! for cloud analysis
        if(l_cloud_analysis) then
           i=izero
           allocate(tempa(nlon_regional,nlat_regional))
           do iskip=2,3
              read(lendian_in)
           end do
           i=i+ione ; i_xlat=i                                                ! xlat
           read(lendian_in) tempa,tempa,n_position
           offset(i)=n_position ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=ione
           if(mype == izero) write(6,*)' xlat, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
 
           i=i+ione ; i_xlon=i                                                ! xlon
           read(lendian_in) tempa,tempa,n_position
           offset(i)=n_position ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=ione
           if(mype == izero) write(6,*)' xlon, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
           deallocate(tempa)
        else
           do iskip=2,5
              read(lendian_in)
           end do
           i=izero
        endif
        read(lendian_in) wrfges
        read(lendian_in) ! n_position          !  offset for START_DATE record
        write(6,*)'READ_WRF_MASS_BINARY_GUESS:  read wrfges,n_position= ',wrfges,' ',n_position
        
        i=i+ione ; i_mub=i                                                ! mub
        read(lendian_in) n_position
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=ione
        if(mype == izero) write(6,*)' mub, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+ione ; i_mu =i                                                ! mu
        read(lendian_in) n_position
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=ione
        if(mype == izero) write(6,*)' mu, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i_fis=i+ione                                               ! sfc geopotential
        read(lendian_in) n_position,memoryorder
        do k=1,lm+ione
           i=i+ione
           if(trim(memoryorder)=='XZY') then
              iadd=izero
              kord(i)=lm+ione
           else
              iadd=(k-ione)*im*jm*4
              kord(i)=ione
           end if
           offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=lm+ione
           if(mype == izero.and.k==ione) write(6,*)' sfc geopot i,igtype(i),offset(i),kord(i) = ', &
                                                                   i,igtype(i),offset(i),kord(i)
        end do
        
        i_t=i+ione
        read(lendian_in) n_position,memoryorder
        do k=1,lm
           i=i+ione                                                       ! theta(k)  (pot temp)
           if(trim(memoryorder)=='XZY') then
              iadd=izero
              kord(i)=lm
           else
              iadd=(k-ione)*im*jm*4
              kord(i)=ione
           end if
           offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=lm
           if(mype == izero.and.k==ione) write(6,*)' temp i,igtype(i),offset(i),kord(i) = ', &
                                                             i,igtype(i),offset(i),kord(i)
        end do
        
        i_q=i+ione
        read(lendian_in) n_position,memoryorder
        do k=1,lm
           i=i+ione                                                       ! q(k)
           if(trim(memoryorder)=='XZY') then
              iadd=izero
              kord(i)=lm
           else
              iadd=(k-ione)*im*jm*4
              kord(i)=ione
           end if
           offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=lm
           if(mype == izero.and.k==ione) write(6,*)' q i,igtype(i),offset(i),kord(i) = ', &
                                                             i,igtype(i),offset(i),kord(i)
        end do
        
        i_u=i+ione
        read(lendian_in) n_position,memoryorder
        do k=1,lm
           i=i+ione                                                       ! u(k)
           if(trim(memoryorder)=='XZY') then
              iadd=izero
              kord(i)=lm
           else
              iadd=(k-ione)*(im+ione)*jm*4
              kord(i)=ione
           end if
           offset(i)=n_position+iadd
           igtype(i)=2_i_kind ; kdim(i)=lm
           length(i)=(im+ione)*jm
           if(mype == izero.and.k==ione) write(6,*)' u i,igtype(i),offset(i),kord(i) = ', &
                                                             i,igtype(i),offset(i),kord(i)
        end do
        
        i_v=i+ione
        read(lendian_in) n_position,memoryorder
        do k=1,lm
           i=i+ione                                                       ! v(k)
           if(trim(memoryorder)=='XZY') then
              iadd=izero
              kord(i)=lm
           else
              iadd=(k-ione)*im*(jm+ione)*4
              kord(i)=ione
           end if
           offset(i)=n_position+iadd ; length(i)=im*(jm+ione) ; igtype(i)=3_i_kind ; kdim(i)=lm
           if(mype == izero.and.k==ione) write(6,*)' v i,igtype(i),offset(i),kord(i) = ', &
                                                             i,igtype(i),offset(i),kord(i)
        end do
        
        i=i+ione   ; i_sm=i                                              ! landmask
        read(lendian_in) n_position
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=ione
        if(mype == izero) write(6,*)' landmask i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+ione ; i_xice=i                                              ! xice
        read(lendian_in) n_position
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=ione
        if(mype == izero) write(6,*)' xice i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+ione ; i_sst=i                                               ! sst
        read(lendian_in) n_position
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=ione
        if(mype == izero) write(6,*)' sst i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+ione ; i_ivgtyp=i                                            ! ivgtyp
        read(lendian_in) n_position
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=-ione ; kdim(i)=ione
        if(mype == izero) write(6,*)' ivgtyp i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+ione ; i_isltyp=i                                            ! isltyp
        read(lendian_in) n_position
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=-ione ; kdim(i)=ione
        if(mype == izero) write(6,*)' isltyp i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+ione ; i_vegfrac=i                                           ! vegfrac
        read(lendian_in) n_position
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=ione
        if(mype == izero) write(6,*)' vegfrac i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+ione ; i_sno=i                                               ! sno
        read(lendian_in) n_position
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=ione
        if(mype == izero) write(6,*)' sno i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+ione ; i_u10=i                                               ! u10
        read(lendian_in) n_position
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=ione
        if(mype == izero) write(6,*)' u10 i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+ione ; i_v10=i                                               ! v10
        read(lendian_in) n_position
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=ione
        if(mype == izero) write(6,*)' v10 i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+ione ; i_smois=i                                             ! smois
        read(lendian_in) n_position,ksize,memoryorder
        if(trim(memoryorder)=='XZY') then
           kord(i)=ksize
        else
           kord(i)=ione
        end if
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=ksize
        if(mype == izero) write(6,*)' smois i,igtype(i),offset(i),kord(i) = ', &
                                                             i,igtype(i),offset(i),kord(i)
        do k=2,ksize
           i=i+ione
           if(trim(memoryorder)=='XZY') then
              iadd=izero
              kord(i)=ksize
           else
              iadd=(k-ione)*im*jm*4
              kord(i)=ione
           end if
           offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=ksize
        end do
        
        i=i+ione ; i_tslb=i                                              ! tslb
        read(lendian_in) n_position,ksize,memoryorder
        if(trim(memoryorder)=='XZY') then
           kord(i)=ksize
        else
           kord(i)=ione
        end if

        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=ksize
        if(mype == izero) write(6,*)' tslb i,igtype(i),offset(i),kord(i) = ', &
                                                             i,igtype(i),offset(i),kord(i)
        do k=2,ksize
           i=i+ione
           if(trim(memoryorder)=='XZY') then
              iadd=izero
              kord(i)=ksize
           else
              iadd=(k-ione)*im*jm*4
              kord(i)=ione
           end if

           offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=ksize
           if(mype == izero) write(6,*)' i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        end do

        i=i+ione ; i_tsk=i                                               ! tsk

        read(lendian_in) n_position
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=ione
        if(mype == izero) write(6,*)' tsk i,igtype(i),offset(i) = ',i,igtype(i),offset(i)

! for cloud array
        if(l_cloud_analysis) then

           i_qc=i+ione
           read(lendian_in) n_position,memoryorder
           do k=1,lm
              i=i+ione                                                       ! qc(k)
              if(trim(memoryorder)=='XZY') then
                 iadd=izero
                 kord(i)=lm
              else
                 iadd=(k-ione)*im*jm*4
                 kord(i)=ione
              end if
              offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=lm
              if(mype == izero.and.k==ione) write(6,*)' qc i,igtype(i),offset(i),kord(i) = ', &
                                                              i,igtype(i),offset(i),kord(i)
           end do
  
           i_qr=i+ione
           read(lendian_in) n_position,memoryorder
           do k=1,lm
              i=i+ione                                                       ! qr(k)
              if(trim(memoryorder)=='XZY') then
                 iadd=izero
                 kord(i)=lm
              else
                 iadd=(k-ione)*im*jm*4
                 kord(i)=ione
              end if
              offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=lm
              if(mype == izero.and.k==ione) write(6,*)' qr i,igtype(i),offset(i),kord(i) = ', &
                                                              i,igtype(i),offset(i),kord(i)
           end do
  
           i_qi=i+ione
           read(lendian_in) n_position,memoryorder
           do k=1,lm
              i=i+ione                                                       ! qi(k)
              if(trim(memoryorder)=='XZY') then
                 iadd=izero
                 kord(i)=lm
              else
                 iadd=(k-ione)*im*jm*4
                 kord(i)=ione
              end if
              offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=lm
              if(mype == izero.and.k==ione) write(6,*)' qi i,igtype(i),offset(i),kord(i) = ', &
                                                              i,igtype(i),offset(i),kord(i)
           end do
  
           i_qs=i+ione
           read(lendian_in) n_position,memoryorder
           do k=1,lm
              i=i+ione                                                       ! qs(k)
              if(trim(memoryorder)=='XZY') then
                 iadd=izero
                 kord(i)=lm
              else
                 iadd=(k-ione)*im*jm*4
                 kord(i)=ione
              end if
              offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=lm
              if(mype == izero.and.k==ione) write(6,*)' qs i,igtype(i),offset(i),kord(i) = ', &
                                                              i,igtype(i),offset(i),kord(i)
           end do
  
           i_qg=i+ione
           read(lendian_in) n_position,memoryorder
           do k=1,lm
              i=i+ione                                                       ! qg(k)
              if(trim(memoryorder)=='XZY') then
                 iadd=izero
                 kord(i)=lm
              else
                 iadd=(k-ione)*im*jm*4
                 kord(i)=ione
              end if
              offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=lm
              if(mype == izero.and.k==ione) write(6,*)' qg i,igtype(i),offset(i),kord(i) = ', &
                                                              i,igtype(i),offset(i),kord(i)
           end do
  
           i_tt=i+ione
           read(lendian_in) n_position,memoryorder
           do k=1,lm
              i=i+ione                                                       ! tt(k)
              if(trim(memoryorder)=='XZY') then
                 iadd=izero
                 kord(i)=lm
              else
                 iadd=(k-ione)*im*jm*4
                 kord(i)=ione
              end if
              offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=lm
              if(mype == izero.and.k==ione) write(6,*)' tt i,igtype(i),offset(i),kord(i) = ', &
                                                              i,igtype(i),offset(i),kord(i)
           end do
  
        endif

        close(lendian_in)
        
!    End of stuff from MASS restart file

!          set up evenly distributed index range over all processors for all input fields

     
        num_loc_groups=num_mass_fields/npe
        nextra=num_mass_fields-num_loc_groups*npe
        kbegin(0)=ione
        if(nextra > izero) then
           do k=1,nextra
              kbegin(k)=kbegin(k-ione)+ione+num_loc_groups
           end do
        end if
        do k=nextra+ione,npe
           kbegin(k)=kbegin(k-ione)+num_loc_groups
        end do
        do k=0,npe-ione
           kend(k)=kbegin(k+ione)-ione
        end do
        if(mype == izero) then
           write(6,*)' kbegin=',kbegin
           write(6,*)' kend= ',kend
        end if
        num_j_groups=jm/npe
        jextra=jm-num_j_groups*npe
        jbegin(0)=ione
        if(jextra > izero) then
           do j=1,jextra
              jbegin(j)=jbegin(j-ione)+ione+num_j_groups
           end do
        end if
        do j=jextra+ione,npe
           jbegin(j)=jbegin(j-ione)+num_j_groups
        end do
        do j=0,npe-ione
           jend(j)=min(jbegin(j+ione)-ione,jm)
        end do
        if(mype == izero) then
           write(6,*)' jbegin=',jbegin
           write(6,*)' jend= ',jend
        end if
        
        allocate(ibuf((im+ione)*(jm+ione),kbegin(mype):kend(mype)))
        call mpi_file_open(mpi_comm_world,trim(wrfges),mpi_mode_rdonly,mpi_info_null,mfcst,ierror)
        
!                                    read geopotential
        if(kord(i_fis)/=ione) then
           allocate(jbuf(im,lm+ione,jbegin(mype):jend(mype)))
           this_offset=offset(i_fis)+(jbegin(mype)-ione)*4*im*(lm+ione)
           this_length=(jend(mype)-jbegin(mype)+ione)*im*(lm+ione)
           call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                 mpi_integer,status,ierror)
           call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                jbegin,jend,kbegin,kend,mype,npe,im,jm,lm+ione,im+ione,jm+ione,i_fis,i_fis)
           deallocate(jbuf)
        end if
        
!                                    read temps
        if(kord(i_t)/=ione) then
           allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
           this_offset=offset(i_t)+(jbegin(mype)-ione)*4*im*lm
           this_length=(jend(mype)-jbegin(mype)+ione)*im*lm
           call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                 mpi_integer,status,ierror)
           call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+ione,jm+ione,i_t,i_t+lm-ione)
           deallocate(jbuf)
        end if

!                                    read q
        if(kord(i_q)/=ione) then
           allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
           this_offset=offset(i_q)+(jbegin(mype)-ione)*4*im*lm
           this_length=(jend(mype)-jbegin(mype)+ione)*im*lm
           call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                 mpi_integer,status,ierror)
           call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+ione,jm+ione,i_q,i_q+lm-ione)
           deallocate(jbuf)
        end if
        
!                                    read u
        if(kord(i_u)/=ione) then
           allocate(jbuf(im+ione,lm,jbegin(mype):jend(mype)))
           this_offset=offset(i_u)+(jbegin(mype)-ione)*4*(im+ione)*lm
           this_length=(jend(mype)-jbegin(mype)+ione)*(im+ione)*lm
           call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                 mpi_integer,status,ierror)
           call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                jbegin,jend,kbegin,kend,mype,npe,im+ione,jm,lm,im+ione,jm+ione,i_u,i_u+lm-ione)
           deallocate(jbuf)
        end if
        
!                                    read v
        if(kord(i_v)/=ione) then
           jend2=jend
!  Account for extra lat for v
           jend2(npe-ione)=jend2(npe-ione)+ione
           allocate(jbuf(im,lm,jbegin(mype):jend2(mype)))
           this_offset=offset(i_v)+(jbegin(mype)-ione)*4*im*lm
           this_length=(jend2(mype)-jbegin(mype)+ione)*im*lm
           call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                 mpi_integer,status,ierror)
           call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend2(mype),ibuf,kbegin(mype),kend(mype), &
                jbegin,jend2,kbegin,kend,mype,npe,im,jm+ione,lm,im+ione,jm+ione,i_v,i_v+lm-ione)
           deallocate(jbuf)
        end if
        
!                                    read smois
        if(kord(i_smois)/=ione) then
           allocate(jbuf(im,ksize,jbegin(mype):jend(mype)))
           this_offset=offset(i_smois)+(jbegin(mype)-ione)*4*im*ksize
           this_length=(jend(mype)-jbegin(mype)+ione)*im*ksize
           call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                 mpi_integer,status,ierror)
           call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                jbegin,jend,kbegin,kend,mype,npe,im,jm,ksize,im+ione,jm+ione,i_smois,i_smois)
           deallocate(jbuf)
        end if

!                                    read tslb
        if(kord(i_tslb)/=ione) then
           allocate(jbuf(im,ksize,jbegin(mype):jend(mype)))
           this_offset=offset(i_tslb)+(jbegin(mype)-ione)*4*im*ksize
           this_length=(jend(mype)-jbegin(mype)+ione)*im*ksize
           call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                 mpi_integer,status,ierror)
           call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                jbegin,jend,kbegin,kend,mype,npe,im,jm,ksize,im+ione,jm+ione,i_tslb,i_tslb)
           deallocate(jbuf)
        end if

! for cloud analysis
        if(l_cloud_analysis) then
!                                    read qc
           if(kord(i_qc)/=ione) then
              allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
              this_offset=offset(i_qc)+(jbegin(mype)-ione)*4*im*lm
              this_length=(jend(mype)-jbegin(mype)+ione)*im*lm
              call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                  mpi_integer,status,ierror)
              call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                 jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+ione,jm+ione,i_qc,i_qc+lm-ione)
              deallocate(jbuf)
           end if

!                                    read qr
           if(kord(i_qr)/=ione) then
              allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
              this_offset=offset(i_qr)+(jbegin(mype)-ione)*4*im*lm
              this_length=(jend(mype)-jbegin(mype)+ione)*im*lm
              call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                  mpi_integer,status,ierror)
              call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                 jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+ione,jm+ione,i_qr,i_qr+lm-ione)
              deallocate(jbuf)
           end if

!                                    read qi
           if(kord(i_qi)/=ione) then
              allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
              this_offset=offset(i_qi)+(jbegin(mype)-ione)*4*im*lm
              this_length=(jend(mype)-jbegin(mype)+ione)*im*lm
              call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                  mpi_integer,status,ierror)
              call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                 jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+ione,jm+ione,i_qi,i_qi+lm-ione)
              deallocate(jbuf)
           end if

!                                    read qs
           if(kord(i_qs)/=ione) then
              allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
              this_offset=offset(i_qs)+(jbegin(mype)-ione)*4*im*lm
              this_length=(jend(mype)-jbegin(mype)+ione)*im*lm
              call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                  mpi_integer,status,ierror)
              call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                 jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+ione,jm+ione,i_qs,i_qs+lm-ione)
              deallocate(jbuf)
           end if

!                                    read qg
           if(kord(i_qg)/=ione) then
              allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
              this_offset=offset(i_qg)+(jbegin(mype)-ione)*4*im*lm
              this_length=(jend(mype)-jbegin(mype)+ione)*im*lm
              call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                  mpi_integer,status,ierror)
              call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                 jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+ione,jm+ione,i_qg,i_qg+lm-ione)
              deallocate(jbuf)
           end if

!                                    read tt  radar temperature tendency
           if(kord(i_tt)/=ione) then
              allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
              this_offset=offset(i_tt)+(jbegin(mype)-ione)*4*im*lm
              this_length=(jend(mype)-jbegin(mype)+ione)*im*lm
              call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                  mpi_integer,status,ierror)
              call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                 jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+ione,jm+ione,i_tt,i_tt+lm-ione)
              deallocate(jbuf)
           end if

        endif  ! l_cloud_analysis

!---------------------- read surface files last
        do k=kbegin(mype),kend(mype)
           if(kdim(k)==ione.or.kord(k)==ione) then
              call mpi_file_read_at(mfcst,offset(k),ibuf(1,k),length(k),mpi_integer,status,ierror)
              if(igtype(k)==ione)     call expand_ibuf(ibuf(1,k),im     ,jm     ,im+ione,jm+ione)
              if(igtype(k)==2_i_kind) call expand_ibuf(ibuf(1,k),im+ione,jm     ,im+ione,jm+ione)
              if(igtype(k)==3_i_kind) call expand_ibuf(ibuf(1,k),im     ,jm+ione,im+ione,jm+ione)
           end if
        end do

        call mpi_file_close(mfcst,ierror)
    
!   next interpolate to analysis grid, then distribute to subdomains
        
        allocate(temp1(im,jm),temp1u(im+ione,jm),temp1v(im,jm+ione))
        allocate(tempa(itotsub,kbegin(mype):kend(mype)))
        do ifld=kbegin(mype),kend(mype)
           if(igtype(ifld) ==  ione) then
              call move_ibuf_hg(ibuf(1,ifld),temp1,im+ione,jm+ione,im,jm)
              call fill_mass_grid2t(temp1,im,jm,tempa(1,ifld),ione)
           else if(igtype(ifld) == -ione) then
              call move_ibuf_ihg(ibuf(1,ifld),temp1,im+ione,jm+ione,im,jm)
              call fill_mass_grid2t(temp1,im,jm,tempa(1,ifld),ione)
           else if(igtype(ifld) == 2_i_kind) then
              call move_ibuf_hg(ibuf(1,ifld),temp1u,im+ione,jm+ione,im+ione,jm)
              call fill_mass_grid2u(temp1u,im,jm,tempa(1,ifld),ione)
           else if(igtype(ifld) == 3_i_kind) then
              call move_ibuf_hg(ibuf(1,ifld),temp1v,im+ione,jm+ione,im,jm+ione)
              call fill_mass_grid2v(temp1v,im,jm,tempa(1,ifld),ione)
           end if
        end do
        deallocate(ibuf)
        deallocate(temp1,temp1u,temp1v)
        allocate(all_loc(lat2,lon2,num_mass_fields))
        call generic_grid2sub(tempa,all_loc,kbegin(mype),kend(mype),kbegin,kend,mype,num_mass_fields)
        

!    Next do conversion of units as necessary and
!    reorganize into WeiYu's format--

        kt=i_t-ione
        kq=i_q-ione
        ku=i_u-ione
        kv=i_v-ione
! hydrometeors
        if(l_cloud_analysis) then
           kqc=i_qc-ione
           kqr=i_qr-ione
           kqs=i_qs-ione
           kqi=i_qi-ione
           kqg=i_qg-ione
           ktt=i_tt-ione
        endif
!             wrf pressure variable is dry air partial pressure--need to add water vapor contribution
!              so accumulate 1 + total water vapor to use as correction factor

        q_integral=one
        do k=1,nsig
           deltasigma=eta1_ll(k)-eta1_ll(k+ione)
           kt=kt+ione
           kq=kq+ione
           ku=ku+ione
           kv=kv+ione
! hydrometeors
           if(l_cloud_analysis) then
              kqc=kqc+ione
              kqr=kqr+ione
              kqs=kqs+ione
              kqi=kqi+ione
              kqg=kqg+ione
              ktt=ktt+ione
           endif
           do i=1,lon2
              do j=1,lat2
                 ges_u(j,i,k,it) = all_loc(j,i,ku)
                 ges_v(j,i,k,it) = all_loc(j,i,kv)
                 ges_q(j,i,k,it) = all_loc(j,i,kq)
                 q_integral(j,i) = q_integral(j,i)+deltasigma*ges_q(j,i,k,it)

!                Convert guess mixing ratio to specific humidity
                 ges_q(j,i,k,it) = ges_q(j,i,k,it)/(one+ges_q(j,i,k,it))

!                Add offset to get guess potential temperature
                 ges_pot(j,i,k)  = all_loc(j,i,kt) + h300
! hydrometeors
                 if(l_cloud_analysis) then
                    ges_qc(j,i,k,it) = all_loc(j,i,kqc)
                    ges_qi(j,i,k,it) = all_loc(j,i,kqi)
                    ges_qr(j,i,k,it) = all_loc(j,i,kqr)
                    ges_qs(j,i,k,it) = all_loc(j,i,kqs)
                    ges_qg(j,i,k,it) = all_loc(j,i,kqg)
                    ges_tten(j,i,k,it) = all_loc(j,i,ktt)
                 endif

              end do
           end do
        end do
        do i=1,lon2
           do j=1,lat2

!             NOTE:  MASS surface elevation is multiplied by g, so divide by g below
              ges_z(j,i,it) = all_loc(j,i,i_fis)/grav

!             Convert psfc units of mb and then convert to log(psfc) in cb
              psfc_this_dry=r0_01*(all_loc(j,i,i_mub)+all_loc(j,i,i_mu)+pt_regional_single)
              psfc_this=(psfc_this_dry-pt_ll)*q_integral(j,i)+pt_ll
              ges_ps(j,i,it)=one_tenth*psfc_this   ! convert from mb to cb
              sno(j,i,it)=all_loc(j,i,i_sno)
              soil_moi(j,i,it)=all_loc(j,i,i_smois)
              soil_temp(j,i,it)=all_loc(j,i,i_tslb)
! for cloud analysis
              if(l_cloud_analysis) then
                 soil_temp_cld(j,i,it)=soil_temp(j,i,it)
                 ges_xlon(j,i,it)=all_loc(j,i,i_xlon)
                 ges_xlat(j,i,it)=all_loc(j,i,i_xlat)
              endif

           end do
        end do


!       Convert potenital temperature to temperature.  Then convert
!       sensible to virtual temperature
        do k=1,nsig
           do i=1,lon2
              do j=1,lat2
                 work_prsl  = one_tenth*(aeta1_ll(k)*(r10*ges_ps(j,i,it)-pt_ll)+pt_ll)
                 work_prslk = (work_prsl/r100)**rd_over_cp_mass
                 ges_tsen(j,i,k,it)= ges_pot(j,i,k)*work_prslk
                 ges_tv(j,i,k,it) = ges_tsen(j,i,k,it) * (one+fv*ges_q(j,i,k,it))
              end do
           end do
        end do


!    Transfer surface fields
        do i=1,lon2
           do j=1,lat2
              fact10(j,i,it)=one    !  later fix this by using correct w10/w(1)
              veg_type(j,i,it)=all_loc(j,i,i_ivgtyp)
              veg_frac(j,i,it)=r0_01*all_loc(j,i,i_vegfrac)
              soil_type(j,i,it)=all_loc(j,i,i_isltyp)
              sm_this=zero
              if(all_loc(j,i,i_sm) /= zero_single) sm_this=one
              xice_this=zero
              if(all_loc(j,i,i_xice) /= zero_single) xice_this=one
              
              isli_this=izero
              if(xice_this==one) isli_this=2_i_kind
              if(xice_this==zero.and.sm_this==one) isli_this=ione
              isli(j,i,it)=isli_this

!?????????????????????????????????check to see if land skin temp is pot temp--if so, need to convert
              sfct(j,i,it)=all_loc(j,i,i_sst)
              if(isli(j,i,it) /= izero) sfct(j,i,it)=all_loc(j,i,i_tsk)
              if(sfct(j,i,it) < one) then

!             For now, replace missing skin temps with 1st sigma level temp
                 sfct(j,i,it)=ges_tsen(j,i,1,it)
                 num_doubtful_sfct=num_doubtful_sfct+ione
                 if(num_doubtful_sfct <= 100) &
                      write(6,*)'READ_WRF_MASS_BINARY_GUESS:  ',&
                      'doubtful skint replaced with 1st sigma level t, j,i,mype,sfct=',&
                      j,i,mype,sfct(j,i,it)
              end if
              if(l_cloud_analysis) then
                 isli_cld(j,i,it)=isli(j,i,it)
              endif
           end do
        end do
     end do

     
     call mpi_reduce(num_doubtful_sfct,num_doubtful_sfct_all,ione,mpi_integer,mpi_sum,&
          izero,mpi_comm_world,ierror)
     if(mype==izero) write(6,*)' in read_wrf_mass_guess, num_doubtful_sfct_all = ',num_doubtful_sfct_all
     if(mype==izero) write(6,*)' in read_wrf_mass_guess, num_doubtful_sfct_all = ',num_doubtful_sfct_all
     if(mype==10_i_kind) write(6,*)' in read_wrf_mass_guess, min,max(sfct)=', &
          minval(sfct),maxval(sfct)
     
     deallocate(all_loc,igtype,kdim,kord)


     return
end subroutine read_wrf_mass_binary_guess

subroutine read_wrf_mass_netcdf_guess(mype)
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
!   2004--7-15  parrish
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-09-10  parrish - correct error in land-sea mask interpretation
!   2005-02-23  wu - setup for qoption=2 and output stats of RH
!   2005-04-01  treadon - add initialization of ges_oz, prsi_oz; comestic format changes
!   2005-05-27  parrish - add call get_derivatives
!   2005-11-21  derber - make qoption=1 work same as qoption=2
!   2005-11-29  derber - remove external iteration dependent calculations
!   2005-12-15  treadon - remove initialization of certain guess arrays (done elsewhere)
!   2006-02-02  treadon - remove load_prsges,load_geop_hgt,prsl,prslk (not used)
!   2006-02-15  treadon - convert moisture mixing ratio to specific humidity
!   2006-03-07  treadon - convert guess potential temperature to vritual temperature
!   2006-04-06  middlecoff - changed nfcst from 11 to lendian_in
!   2006-07-28  derber  - include sensible temperatures
!   2006-07-31  kleist - change to use ges_ps instead of lnps
!   2007-03-13  derber - remove unused qsinv2 from jfunc use list
!   2008-04-16  safford - rm unused uses
!   2010-03-29  hu     - add code to read in cloud/hydrometeor fields 
!                             and distributed them to all processors 
!
!   input argument list:
!     mype     - pe number
!
!     NOTES:  need to pay special attention to various surface fields to make sure
!             they are correct (check units).  fact10 needs to be computed from 
!             10m wind, which is included in wrf mass interface file.
!
!             Cloud water currently set to zero.  Need to fix before doing precip
!             assimilation--wait until global adopts Brad Ferrier's multi-species 
!             scheme??
!
!             Ozone currently set to zero.  No ozone variable in wrf mass core--climatology
!             instead.  Do we use climatology?
!
!             No background bias yet. (biascor ignored)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_single,i_kind
  use mpimod, only: mpi_sum,mpi_integer,mpi_real4,mpi_comm_world,npe,ierror
  use guess_grids, only: ges_z,ges_ps,ges_tv,ges_q,ges_u,ges_v,&
       fact10,soil_type,veg_frac,veg_type,sfct,sno,soil_temp,soil_moi,&
       isli,nfldsig,ifilesig,ges_tsen
  use guess_grids, only: ges_qc,ges_qi,ges_qr,ges_qs,ges_qg,   &
       ges_xlon,ges_xlat,soil_temp_cld,isli_cld,ges_tten
  use gridmod, only: lat2,lon2,nlat_regional,nlon_regional,&
       nsig,ijn_s,displs_s,eta1_ll,pt_ll,itotsub,aeta1_ll
  use constants, only: izero,ione,zero,one,grav,fv,zero_single,rd_over_cp_mass,one_tenth
  use gsi_io, only: lendian_in
  use rapidrefresh_cldsurf_mod, only: l_cloud_analysis
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype

! Declare local parameters
  real(r_kind),parameter:: r0_01=0.01_r_kind
  real(r_kind),parameter:: r10=10.0_r_kind
  real(r_kind),parameter:: r100=100.0_r_kind

! Declare local variables
  integer(i_kind) kt,kq,ku,kv

! MASS variable names stuck in here

! other internal variables
  real(r_single) tempa(itotsub)
  real(r_single),allocatable::temp1(:,:),temp1u(:,:),temp1v(:,:)
  real(r_single),allocatable::all_loc(:,:,:)
  integer(i_kind),allocatable::itemp1(:,:)
  integer(i_kind),allocatable::igtype(:),jsig_skip(:)
  character(60),allocatable::identity(:)
  character(6) filename 
  integer(i_kind) irc_s_reg(npe),ird_s_reg(npe)
  integer(i_kind) ifld,im,jm,lm,num_mass_fields
  integer(i_kind) num_all_fields,num_loc_groups,num_all_pad
  integer(i_kind) i,icount,icount_prev,it,j,k
  integer(i_kind) i_0,i_psfc,i_fis,i_t,i_q,i_u,i_v,i_sno,i_u10,i_v10,i_smois,i_tslb
  integer(i_kind) i_sm,i_xice,i_sst,i_tsk,i_ivgtyp,i_isltyp,i_vegfrac
  integer(i_kind) isli_this
  real(r_kind) psfc_this,psfc_this_dry,sm_this,xice_this
  real(r_kind),dimension(lat2,lon2):: q_integral
  real(r_kind),dimension(lat2,lon2,nsig):: ges_pot
  integer(i_kind) num_doubtful_sfct,num_doubtful_sfct_all
  real(r_kind) deltasigma
  real(r_kind):: work_prsl,work_prslk
  integer(i_kind) i_qc,i_qi,i_qr,i_qs,i_qg,kqc,kqi,kqr,kqs,kqg,i_xlon,i_xlat,i_tt,ktt


!  WRF MASS input grid dimensions in module gridmod
!      These are the following:
!          im -- number of x-points on C-grid
!          jm -- number of y-points on C-grid
!          lm -- number of vertical levels ( = nsig for now)


     num_doubtful_sfct=izero
     if(mype==izero) write(6,*)' at 0 in read_wrf_mass_guess'


! Big section of operations done only on first outer iteration

     if(mype==izero) write(6,*)' at 0.1 in read_wrf_mass_guess'

     im=nlon_regional
     jm=nlat_regional
     lm=nsig

!    Following is for convenient WRF MASS input
     num_mass_fields=14_i_kind+4_i_kind*lm
     if(l_cloud_analysis) num_mass_fields=14_i_kind+4_i_kind*lm+6_i_kind*lm+2_i_kind
     num_all_fields=num_mass_fields*nfldsig
     num_loc_groups=num_all_fields/npe
     if(mype==izero) write(6,'(" at 1 in read_wrf_mass_guess, lm            =",i6)')lm
     if(mype==izero) write(6,'(" at 1 in read_wrf_mass_guess, num_mass_fields=",i6)')num_mass_fields
     if(mype==izero) write(6,'(" at 1 in read_wrf_mass_guess, nfldsig       =",i6)')nfldsig
     if(mype==izero) write(6,'(" at 1 in read_wrf_mass_guess, num_all_fields=",i6)')num_all_fields
     if(mype==izero) write(6,'(" at 1 in read_wrf_mass_guess, npe           =",i6)')npe
     if(mype==izero) write(6,'(" at 1 in read_wrf_mass_guess, num_loc_groups=",i6)')num_loc_groups
     do 
        num_all_pad=num_loc_groups*npe
        if(num_all_pad >= num_all_fields) exit
        num_loc_groups=num_loc_groups+ione
     end do
     if(mype==izero) write(6,'(" at 1 in read_wrf_mass_guess, num_all_pad   =",i6)')num_all_pad
     if(mype==izero) write(6,'(" at 1 in read_wrf_mass_guess, num_loc_groups=",i6)')num_loc_groups

     allocate(all_loc(lat2,lon2,num_all_pad))
     allocate(jsig_skip(num_mass_fields))
     allocate(igtype(num_mass_fields))
     allocate(identity(num_mass_fields))

!    igtype is a flag indicating whether each input MASS field is h-, u-, or v-grid
!    and whether integer or real
!     abs(igtype)=1 for h-grid
!                =2 for u-grid
!                =3 for u-grid
!     igtype < 0 for integer field

     i=izero
! for cloud analysis
     if(l_cloud_analysis) then
        i=i+ione ; i_xlat=i                                                ! xlat
        write(identity(i),'("record ",i3,"--xlat")')i
        jsig_skip(i)=3_i_kind     ! number of files to skip before getting to xlat
        igtype(i)=ione
        i=i+ione ; i_xlon=i                                                ! xlon
        write(identity(i),'("record ",i3,"--xlon")')i
        jsig_skip(i)=izero     ! 
        igtype(i)=ione
     endif

     i=i+ione ; i_psfc=i                                                ! psfc
     write(identity(i),'("record ",i3,"--psfc")')i
     jsig_skip(i)=5_i_kind     ! number of files to skip before getting to psfc
     if(l_cloud_analysis) jsig_skip(i)=0_i_kind ! number of files to skip before getting to psfc
     igtype(i)=ione
     i=i+ione ; i_fis=i                                               ! sfc geopotential
     write(identity(i),'("record ",i3,"--fis")')i
     jsig_skip(i)=izero
     igtype(i)=ione
     i_t=i+ione
     do k=1,lm
        i=i+ione                                                       ! theta(k)  (pot temp)
        write(identity(i),'("record ",i3,"--t(",i2,")")')i,k
        jsig_skip(i)=izero
        igtype(i)=ione
     end do
     i_q=i+ione
     do k=1,lm
        i=i+ione                                                       ! q(k)
        write(identity(i),'("record ",i3,"--q(",i2,")")')i,k
        jsig_skip(i)=izero ; igtype(i)=ione
     end do
     i_u=i+ione
     do k=1,lm
        i=i+ione                                                       ! u(k)
        write(identity(i),'("record ",i3,"--u(",i2,")")')i,k
        jsig_skip(i)=izero ; igtype(i)=2_i_kind
     end do
     i_v=i+ione
     do k=1,lm
        i=i+ione                                                       ! v(k)
        write(identity(i),'("record ",i3,"--v(",i2,")")')i,k
        jsig_skip(i)=izero ; igtype(i)=3_i_kind
     end do
     i=i+ione   ; i_sm=i                                              ! landmask
     write(identity(i),'("record ",i3,"--sm")')i
     jsig_skip(i)=izero ; igtype(i)=ione
     i=i+ione ; i_xice=i                                              ! xice
     write(identity(i),'("record ",i3,"--xice")')i
     jsig_skip(i)=izero ; igtype(i)=ione
     i=i+ione ; i_sst=i                                               ! sst
     write(identity(i),'("record ",i3,"--sst")')i
     jsig_skip(i)=izero ; igtype(i)=ione
     i=i+ione ; i_ivgtyp=i                                            ! ivgtyp
     write(identity(i),'("record ",i3,"--ivgtyp")')i
     jsig_skip(i)=izero ; igtype(i)=-ione
     i=i+ione ; i_isltyp=i                                            ! isltyp
     write(identity(i),'("record ",i3,"--isltyp")')i
     jsig_skip(i)=izero ; igtype(i)=-ione
     i=i+ione ; i_vegfrac=i                                           ! vegfrac
     write(identity(i),'("record ",i3,"--vegfrac")')i
     jsig_skip(i)=izero ; igtype(i)=ione
     i=i+ione ; i_sno=i                                               ! sno
     write(identity(i),'("record ",i3,"--sno")')i
     jsig_skip(i)=izero ; igtype(i)=ione
     i=i+ione ; i_u10=i                                               ! u10
     write(identity(i),'("record ",i3,"--u10")')i
     jsig_skip(i)=izero ; igtype(i)=ione
     i=i+ione ; i_v10=i                                               ! v10
     write(identity(i),'("record ",i3,"--v10")')i
     jsig_skip(i)=izero ; igtype(i)=ione
     i=i+ione ; i_smois=i                                             ! smois
     write(identity(i),'("record ",i3,"--smois(",i2,")")')i,k
     jsig_skip(i)=izero ; igtype(i)=ione
     i=i+ione ; i_tslb=i                                              ! tslb
     write(identity(i),'("record ",i3,"--tslb(",i2,")")')i,k
     jsig_skip(i)=izero ; igtype(i)=ione
     i=i+ione ; i_tsk=i                                               ! tsk
     write(identity(i),'("record ",i3,"--sst")')i
     jsig_skip(i)=izero ; igtype(i)=ione
! for cloud array
     if(l_cloud_analysis) then
        i_qc=i+ione
        do k=1,lm
           i=i+ione                                                      ! qc(k)
           write(identity(i),'("record ",i3,"--qc(",i2,")")')i,k
           jsig_skip(i)=izero ; igtype(i)=ione
        end do
        i_qr=i+ione
        do k=1,lm
           i=i+ione                                                    ! qi(k)
           write(identity(i),'("record ",i3,"--qr(",i2,")")')i,k
           jsig_skip(i)=izero ; igtype(i)=ione
        end do
        i_qs=i+ione
        do k=1,lm
           i=i+ione                                                    ! qr(k)
           write(identity(i),'("record ",i3,"--qs(",i2,")")')i,k
           jsig_skip(i)=izero ; igtype(i)=ione
        end do
        i_qi=i+ione
        do k=1,lm
           i=i+ione                                                    ! qs(k)
           write(identity(i),'("record ",i3,"--qi(",i2,")")')i,k
           jsig_skip(i)=izero ; igtype(i)=ione
        end do
        i_qg=i+ione
        do k=1,lm
           i=i+ione                                                    ! qg(k)
           write(identity(i),'("record ",i3,"--qg(",i2,")")')i,k
           jsig_skip(i)=izero ; igtype(i)=ione
        end do
        i_tt=i+ione
        do k=1,lm
           i=i+ione                                                    ! tt(k)
           write(identity(i),'("record ",i3,"--tt(",i2,")")')i,k
           jsig_skip(i)=izero ; igtype(i)=ione
        end do
     endif

!    End of stuff from MASS restart file

     allocate(temp1(im,jm),itemp1(im,jm),temp1u(im+ione,jm),temp1v(im,jm+ione))
     
     do i=1,npe
        irc_s_reg(i)=ijn_s(mype+ione)
     end do
     ird_s_reg(1)=izero
     do i=1,npe
        if(i /= ione) ird_s_reg(i)=ird_s_reg(i-ione)+irc_s_reg(i-ione)
     end do
     
!    Read wrf MASS fixed format file created from external interface
!    This is done by reading in parallel from every pe, and redistributing
!    to local domains once for every npe fields read in, using 
!    mpi_all_to_allv

     icount=izero
     icount_prev=ione
     do it=1,nfldsig
        write(filename,'("sigf",i2.2)')ifilesig(it)
        open(lendian_in,file=filename,form='unformatted') ; rewind lendian_in
        write(6,*)'READ_WRF_MASS_GUESS:  open lendian_in=',lendian_in,' to file=',filename

!       Read, interpolate, and distribute MASS restart fields
        do ifld=1,num_mass_fields
           icount=icount+ione
           if(jsig_skip(ifld) > izero) then
              do i=1,jsig_skip(ifld)
                 read(lendian_in)
              end do
           end if
           if(mype==mod(icount-ione,npe)) then
              if(igtype(ifld)==ione) then
                 read(lendian_in)((temp1(i,j),i=1,im),j=1,jm)
                 write(6,'(" ifld, temp1(im/2,jm/2)=",i6,e15.5)')ifld,temp1(im/2,jm/2)
                 call fill_mass_grid2t(temp1,im,jm,tempa,ione)
              end if
              if(igtype(ifld)==2_i_kind) then
                 read(lendian_in)((temp1u(i,j),i=1,im+ione),j=1,jm)
                 write(6,'(" ifld, temp1u(im/2,jm/2)=",i6,e15.5)')ifld,temp1u(im/2,jm/2)
                 call fill_mass_grid2u(temp1u,im,jm,tempa,ione)
              end if
              if(igtype(ifld)==3_i_kind) then
                 read(lendian_in)((temp1v(i,j),i=1,im),j=1,jm+ione)
                 write(6,'(" ifld, temp1v(im/2,jm/2)=",i6,e15.5)')ifld,temp1v(im/2,jm/2)
                 call fill_mass_grid2v(temp1v,im,jm,tempa,ione)
              end if
              if(igtype(ifld) < izero) then
                 read(lendian_in)((itemp1(i,j),i=1,im),j=1,jm)
                 do j=1,jm
                    do i=1,im
                       temp1(i,j)=itemp1(i,j)
                    end do
                 end do
                 write(6,'(" ifld, temp1(im/2,jm/2)=",i6,e15.5)')ifld,temp1(im/2,jm/2)
                 call fill_mass_grid2t(temp1,im,jm,tempa,ione)
              end if
           else
              read(lendian_in)
           end if

!          Distribute to local domains everytime we have npe fields
           if(mod(icount,npe) == izero.or.icount==num_all_fields) then
              call mpi_alltoallv(tempa,ijn_s,displs_s,mpi_real4, &
                   all_loc(1,1,icount_prev),irc_s_reg,ird_s_reg,mpi_real4,mpi_comm_world,ierror)
              icount_prev=icount+ione
           end if
        end do
        close(lendian_in)
     end do
!    do kv=i_v,i_v+nsig-ione
!       if(mype==izero) write(6,*)' at 1.15, kv,mype,j,i,v=', &
!          kv,mype,2,1,all_loc(2,1,kv)
!    end do


!    Next do conversion of units as necessary and
!    reorganize into WeiYu's format--

     do it=1,nfldsig
        i_0=(it-ione)*num_mass_fields
        kt=i_0+i_t-ione
        kq=i_0+i_q-ione
        ku=i_0+i_u-ione
        kv=i_0+i_v-ione
! hydrometeors
        if(l_cloud_analysis) then
           kqc=i_0+i_qc-ione
           kqr=i_0+i_qr-ione
           kqs=i_0+i_qs-ione
           kqi=i_0+i_qi-ione
           kqg=i_0+i_qg-ione
           ktt=i_0+i_tt-ione
        endif
!             wrf pressure variable is dry air partial pressure--need to add water vapor contribution
!              so accumulate 1 + total water vapor to use as correction factor

        q_integral=one
        do k=1,nsig
           deltasigma=eta1_ll(k)-eta1_ll(k+ione)
           kt=kt+ione
           kq=kq+ione
           ku=ku+ione
           kv=kv+ione
! hydrometeors
           if(l_cloud_analysis) then
              kqc=kqc+ione
              kqr=kqr+ione
              kqs=kqs+ione
              kqi=kqi+ione
              kqg=kqg+ione
              ktt=ktt+ione
           endif
           do i=1,lon2
              do j=1,lat2
                 ges_u(j,i,k,it) = all_loc(j,i,ku)
                 ges_v(j,i,k,it) = all_loc(j,i,kv)
                 ges_pot(j,i,k)  = all_loc(j,i,kt)
                 ges_q(j,i,k,it) = all_loc(j,i,kq)
                 q_integral(j,i) = q_integral(j,i)+deltasigma*ges_q(j,i,k,it)

!                Convert guess mixing ratio to specific humidity
                 ges_q(j,i,k,it) = ges_q(j,i,k,it)/(one+ges_q(j,i,k,it))
! hydrometeors
                 if(l_cloud_analysis) then
                    ges_qc(j,i,k,it) = all_loc(j,i,kqc)
                    ges_qi(j,i,k,it) = all_loc(j,i,kqi)
                    ges_qr(j,i,k,it) = all_loc(j,i,kqr)
                    ges_qs(j,i,k,it) = all_loc(j,i,kqs)
                    ges_qg(j,i,k,it) = all_loc(j,i,kqg)
                    ges_tten(j,i,k,it) = all_loc(j,i,ktt)
                 endif

              end do
           end do
        end do
        do i=1,lon2
           do j=1,lat2

!             NOTE:  MASS surface elevation is multiplied by g, so divide by g below
              ges_z(j,i,it)    = all_loc(j,i,i_0+i_fis)/grav

!             Convert psfc units of mb and then convert to log(psfc) in cb
              psfc_this_dry=r0_01*all_loc(j,i,i_0+i_psfc)
              psfc_this=(psfc_this_dry-pt_ll)*q_integral(j,i)+pt_ll
              ges_ps(j,i,it)=one_tenth*psfc_this   ! convert from mb to cb
              sno(j,i,it)=all_loc(j,i,i_0+i_sno)
              soil_moi(j,i,it)=all_loc(j,i,i_0+i_smois)
              soil_temp(j,i,it)=all_loc(j,i,i_0+i_tslb)
! for cloud analysis
              if(l_cloud_analysis) then
                 soil_temp_cld(j,i,it)=soil_temp(j,i,it)
                 ges_xlon(j,i,it)=all_loc(j,i,i_0+i_xlon)
                 ges_xlat(j,i,it)=all_loc(j,i,i_0+i_xlat)
              endif

           end do
        end do
        
        if(mype==10_i_kind) write(6,*)' in read_wrf_mass_guess, min,max(soil_moi)=', &
             minval(soil_moi),maxval(soil_moi)
        if(mype==10_i_kind) write(6,*)' in read_wrf_mass_guess, min,max(soil_temp)=', &
             minval(soil_temp),maxval(soil_temp)

!       Convert potenital temperature to temperature
        do k=1,nsig
           do i=1,lon2
              do j=1,lat2
                 work_prsl  = one_tenth*(aeta1_ll(k)*(r10*ges_ps(j,i,it)-pt_ll)+pt_ll)
                 work_prslk = (work_prsl/r100)**rd_over_cp_mass
                 ges_tsen(j,i,k,it)     = ges_pot(j,i,k)*work_prslk
                 ges_tv(j,i,k,it) = ges_tsen(j,i,k,it) * (one+fv*ges_q(j,i,k,it))
              end do
           end do
        end do
     end do

     
!    Transfer surface fields
     do it=1,nfldsig
        i_0=(it-ione)*num_mass_fields
        do i=1,lon2
           do j=1,lat2
              fact10(j,i,it)=one    !  later fix this by using correct w10/w(1)
              veg_type(j,i,it)=all_loc(j,i,i_0+i_ivgtyp)
              veg_frac(j,i,it)=r0_01*all_loc(j,i,i_0+i_vegfrac)
              soil_type(j,i,it)=all_loc(j,i,i_0+i_isltyp)
              sm_this=zero
              if(all_loc(j,i,i_0+i_sm) /= zero_single) sm_this=one
              xice_this=zero
              if(all_loc(j,i,i_0+i_xice) /= zero_single) xice_this=one
              
              isli_this=izero
              if(xice_this==one) isli_this=2_i_kind
              if(xice_this==zero.and.sm_this==one) isli_this=ione
              isli(j,i,it)=isli_this
              
!?????????????????????????????????check to see if land skin temp is pot temp--if so, need to convert
              sfct(j,i,it)=all_loc(j,i,i_0+i_sst)
              if(isli(j,i,it) /= izero) sfct(j,i,it)=all_loc(j,i,i_0+i_tsk)
              if(sfct(j,i,it) < one) then

!             For now, replace missing skin temps with 1st sigma level temp
                 sfct(j,i,it)=all_loc(j,i,i_0+i_t) 
                 write(6,*)' doubtful skint replaced with 1st sigma level t, j,i,mype,sfct=',&
                      j,i,mype,sfct(j,i,it)
                 num_doubtful_sfct=num_doubtful_sfct+ione
              end if
              if(l_cloud_analysis) then
                 isli_cld(j,i,it)=isli(j,i,it)
              endif
           end do
        end do
     end do
     
     call mpi_reduce(num_doubtful_sfct,num_doubtful_sfct_all,ione,mpi_integer,mpi_sum,&
          izero,mpi_comm_world,ierror)
     if(mype==izero) write(6,*)' in read_wrf_mass_guess, num_doubtful_sfct_all = ',num_doubtful_sfct_all
     if(mype==izero) write(6,*)' in read_wrf_mass_guess, num_doubtful_sfct_all = ',num_doubtful_sfct_all
     if(mype==10_i_kind) write(6,*)' in read_wrf_mass_guess, min,max(sfct)=', &
          minval(sfct),maxval(sfct)
     if(mype==10_i_kind) write(6,*)' in read_wrf_mass_guess, min,max(veg_type)=', &
          minval(veg_type),maxval(veg_type)
     if(mype==10_i_kind) write(6,*)' in read_wrf_mass_guess, min,max(veg_frac)=', &
          minval(veg_frac),maxval(veg_frac)
     if(mype==10_i_kind) write(6,*)' in read_wrf_mass_guess, min,max(soil_type)=', &
          minval(soil_type),maxval(soil_type)
     if(mype==10_i_kind) write(6,*)' in read_wrf_mass_guess, min,max(isli)=', &
          minval(isli),maxval(isli)
     
     deallocate(all_loc,jsig_skip,igtype,identity)
     deallocate(temp1,itemp1,temp1u,temp1v)


     return
end subroutine read_wrf_mass_netcdf_guess
#else /* Start no WRF-library block */
subroutine read_wrf_mass_binary_guess()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_wrf_mass_binary_guess
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
  implicit none

  write(6,*)'READ_WRF_MASS_BINARY_GUESS:  dummy routine, does nothing!'
end subroutine read_wrf_mass_binary_guess
subroutine read_wrf_mass_netcdf_guess()
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
  implicit none

  write(6,*)'READ_WRF_MASS_NETCDF_GUESS:  dummy routine, does nothing!'
end subroutine read_wrf_mass_netcdf_guess
#endif /* End no WRF-library block */

subroutine generic_grid2sub(tempa,all_loc,kbegin_loc,kend_loc,kbegin,kend,mype,num_fields)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    generic_grid2sub   converts from full horizontal grid to subdomains
!   prgmmr: parrish          org: np22                date: 2004-11-29
!
! abstract: variation on subroutine grid2sub, with more general distribution of variables
!              along the k index.
!
! program history log:
!   2004-02-03  kleist, new mpi strategy
!   2004-05-06  derber
!   2004-07-15  treadon - handle periodic subdomains
!   2004-07-28  treadon - add only on use declarations; add intent in/out
!   2004-10-26  kleist - u,v removed; periodicity accounted for only in
!               sub2grid routine if necessary
!   2004-11-29  parrish - adapt grid2sub for related use with mpi io.
!
!   input argument list:
!     tempa    - input grid values in horizontal slab mode.
!     kbegin_loc - starting k index for tempa on local processor
!     kend_loc   - ending k index for tempa on local processor
!     kbegin     - starting k indices for tempa for all processors
!     kend       - ending k indices for tempa for all processors
!     mype       - local processor number
!     num_fields - total range of k index (1 <= k <= num_fields)
!
!   output argument list:
!     all_loc  - output grid values in vertical subdomain mode
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use mpimod, only: ierror,mpi_comm_world,mpi_real4,npe
  use gridmod, only: ijn_s,itotsub,lat2,lon2
  use kinds, only: r_single,i_kind
  use constants, only: izero,ione
  implicit none
  
  integer(i_kind),intent(in   ) :: kbegin_loc,kend_loc,mype,num_fields
  integer(i_kind),intent(in   ) :: kbegin(0:npe),kend(0:npe-ione)
  real(r_single) ,intent(in   ) :: tempa(itotsub,kbegin_loc:kend_loc)
  real(r_single) ,intent(  out) :: all_loc(lat2*lon2*num_fields)
  
  integer(i_kind) k
  integer(i_kind) sendcounts(0:npe-ione),sdispls(0:npe),recvcounts(0:npe-ione),rdispls(0:npe)

! first get alltoallv indices
  
  sdispls(0)=izero
  do k=0,npe-ione
     sendcounts(k)=ijn_s(k+ione)*(kend_loc-kbegin_loc+ione) 
     sdispls(k+ione)=sdispls(k)+sendcounts(k)
  end do
  rdispls(0)=izero
  do k=0,npe-ione
     recvcounts(k)=ijn_s(mype+ione)*(kend(k)-kbegin(k)+ione)
     rdispls(k+ione)=rdispls(k)+recvcounts(k)
  end do
  
! then call reorder2

  call reorder2_s(tempa,kend_loc-kbegin_loc+ione)

! then alltoallv and i think we are done??

  call mpi_alltoallv(tempa,sendcounts,sdispls,mpi_real4, &
       all_loc,recvcounts,rdispls,mpi_real4,mpi_comm_world,ierror)

end subroutine generic_grid2sub

subroutine reorder2_s(work,k_in)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    reorder2_s
!
!   prgrmmr:  kleist           org: np20                date: 2004-01-25
!
! abstract:  adapt reorder2 to single precision
!
! program history log:
!   2004-01-25  kleist
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-complaint prologue
!   2004-11-29  parrish, adapt reorder2 to single precision
!   2008-04-16  safford -- add subprogram doc block
!
!   input argument list:
!     k_in    ! number of levs in work array
!     work
!
!   output argument list:
!     work
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
!$$$

! !USES:

  use constants, only: izero,ione,zero_single
  use mpimod, only: npe
  use gridmod, only: ijn_s,itotsub
  use kinds, only: r_single,i_kind
  implicit none
  

! !INPUT PARAMETERS:

  integer(i_kind)                       ,intent(in   ) :: k_in    ! number of levs in work array

! !INPUT/OUTPUT PARAMETERS:

  real(r_single),dimension(itotsub,k_in),intent(inout) :: work


  integer(i_kind) iloc,iskip,i,k,n
  real(r_single),dimension(itotsub*k_in):: temp

! Zero out temp array
  do k=1,itotsub*k_in
     temp(k)=zero_single
  end do
  
! Load temp array in order of subdomains
  iloc=izero
  iskip=izero
  do n=1,npe
     if (n/=ione) then
        iskip=iskip+ijn_s(n-ione)
     end if
     
     do k=1,k_in
        do i=1,ijn_s(n)
           iloc=iloc+ione
           temp(iloc)=work(iskip+i,k)
        end do
     end do
  end do

! Now load the tmp array back into work
  iloc=izero
  do k=1,k_in
     do i=1,itotsub
        iloc=iloc+ione
        work(i,k)=temp(iloc)
     end do
  end do
  
  return
end subroutine reorder2_s

subroutine expand_ibuf(ibuf,im,jm,imp,jmp)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    expand_ibuf    expand array in place
!   prgmmr: parrish          org: np22                date: 2004-11-29
!
! abstract: expand array in place from im,jm to imp,jmp
!
! program history log:
!   2004-11-29  parrish
!   2007-04-12  parrish - replace im+1, jm+1 with inputs imp, jmp to allow
!                           for use with u and v fields, where im=imp or jm=jmp
!
!   input argument list:
!     ibuf     - input grid values in im,jm
!     im       - first grid index
!     jm       - second grid index
!
!   output argument list:
!     ibuf     - output grid values in im+1,jm+1
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

!   field of dim im*jm read into array of dim imp*jmp--need to readjust

  use kinds, only: i_long,i_kind
  use constants, only: izero,ione
  implicit none
  
  integer(i_kind),intent(in   ) :: im,jm,imp,jmp
  integer(i_long),intent(inout) :: ibuf(imp*jmp)
  
  integer(i_long) itemp(imp,jmp)
  integer(i_kind) i,ii,j
  

  do j=1,jmp
     do i=1,imp
        itemp(i,j)=0_i_long
     end do
  end do
  ii=izero
  do j=1,jm
     do i=1,im
        ii=ii+ione
        itemp(i,j)=ibuf(ii)
     end do
  end do
  
  ii=izero
  do j=1,jmp
     do i=1,imp
        ii=ii+ione
        ibuf(ii)=itemp(i,j)
     end do
  end do
  
end subroutine expand_ibuf

subroutine transfer_jbuf2ibuf(jbuf,jbegin_loc,jend_loc,ibuf,kbegin_loc,kend_loc, &
     jbegin,jend,kbegin,kend,mype,npe,im_jbuf,jm_jbuf,lm_jbuf, &
     im_ibuf,jm_ibuf,k_start,k_end)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    transfer_jbuf2ibuf   flip from ikj to ijk 
!   prgmmr: parrish          org: np22                date: 2004-11-29
!
! abstract: redistribute 3-d field from ikj order to ijk order across processors
!
! program history log:
!   2004-11-29  parrish
!   2005-02-16  todling - replaced "use mpi" by use of mpimod
!
!   input argument list:
!     jbuf     - input grid values distributed as all ik and a range of j on each processor
!     jbegin_loc - local processor starting j index
!     jend_loc   - local processor ending j index
!     kbegin_loc - local processor starting k index
!     kend_loc   - local processor ending k index
!     jbegin     - starting j indices for all processors
!     jend       - ending j indices for all processors
!     kbegin     - starting k indices for all processors
!     kend       - ending k indices for all processors
!     mype       - local processor number
!     npe        - total number of processors
!     im_jbuf    - full range of i index for jbuf array
!     jm_jbuf    - full range of j index for jbuf array
!     lm_jbuf    - full range of k index for jbuf array
!     im_ibuf    - full range of i index for ibuf array
!     jm_ibuf    - full range of j index for ibuf array
!     k_start    - beginning index for range of k in ibuf array
!     k_end      - ending index for range of k in ibuf array
!
!   output argument list:
!     ibuf     - output grid redistributed to ijk order (full i, full j, k_start <= k <= k_end)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

!  flip around from ikj to ijk, moving result from jbuf to ibuf

  use mpimod, only: mpi_comm_world,mpi_integer
  use kinds, only: i_long,i_kind
  use constants, only: izero,ione
  implicit none
  
  integer(i_kind),intent(in   ) :: jbegin_loc,jend_loc,kbegin_loc,kend_loc,mype,npe,im_jbuf,jm_jbuf,lm_jbuf
  integer(i_kind),intent(in   ) :: im_ibuf,jm_ibuf,k_start,k_end
  
  integer(i_long),intent(in   ) :: jbuf(im_jbuf,lm_jbuf,jbegin_loc:jend_loc)
  integer(i_long),intent(  out) :: ibuf(im_ibuf,jm_ibuf,kbegin_loc:kend_loc)
  integer(i_kind),intent(in   ) :: jbegin(0:npe),jend(0:npe-ione)
  integer(i_kind),intent(in   ) :: kbegin(0:npe),kend(0:npe-ione)
  
  integer(i_long) sendbuf(im_jbuf*lm_jbuf*(jend_loc-jbegin_loc+2_i_kind))
  integer(i_long) recvbuf(im_jbuf*jm_jbuf*(kend_loc-kbegin_loc+ione))
  integer(i_long) recvcounts(0:npe-ione),displs(0:npe)
  integer(i_kind) i,ipe,j,ierror,k,n,ii,k_t_start,k_t_end,sendcount
  
  do ipe=0,npe-ione
     k_t_start=max(k_start,kbegin(ipe))
     k_t_end=  min(k_end,kend(ipe))
     if(k_t_end < k_t_start) cycle
     
     displs(0)=0_i_long
     do i=0,npe-ione
        recvcounts(i)=im_jbuf*(k_t_end-k_t_start+ione)*(jend(i)-jbegin(i)+ione)
        displs(i+ione)=displs(i)+recvcounts(i)
     end do
     
!   gather everything to ipe
     
     ii=izero
     do k=k_t_start,k_t_end
        do j=jbegin_loc,jend_loc
           do i=1,im_jbuf
              ii=ii+ione
              sendbuf(ii)=jbuf(i,k-k_start+ione,j)
           end do
        end do
     end do
     sendcount=ii
     call mpi_gatherv(sendbuf,sendcount,mpi_integer,recvbuf,recvcounts, &
          displs,mpi_integer,ipe,mpi_comm_world,ierror)
     if(ipe==mype) then
        ii=izero
        do n=0,npe-ione
           do k=k_t_start,k_t_end
              do j=jbegin(n),jend(n)
                 do i=1,im_jbuf
                    ii=ii+ione
                    ibuf(i,j,k)=recvbuf(ii)
                 end do
              end do
           end do
        end do
     end if
     
  end do
  
end subroutine transfer_jbuf2ibuf

subroutine move_ibuf_hg(ibuf,temp1,im_buf,jm_buf,im_out,jm_out)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    move_ibuf_hg  copy from one array to another
!   prgmmr: parrish          org: np22                date: 2004-11-29
!
! abstract: copy from one array to another
!
! program history log:
!   2004-11-29  parrish
!
!   input argument list:
!     ibuf     - input grid values
!     im_buf   - first index of input array buf
!     jm_buf   - second index of input array buf
!     im_out   - first index of output array temp1
!     jm_out   - second index of output array temp1
!
!   output argument list:
!     temp1    - output grid values
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

!        cp buf to temp1

  use kinds, only: r_single,i_kind,i_long
  use constants, only: zero_single
  implicit none
  
  integer(i_kind),intent(in   ) :: im_buf,jm_buf,im_out,jm_out
  integer(i_long),intent(in   ) :: ibuf(im_buf,jm_buf)
  real(r_single) ,intent(  out) :: temp1(im_out,jm_out)

  integer(i_kind) i,j

  do j=1,jm_out
     do i=1,im_out
        temp1(i,j)=transfer(ibuf(i,j),zero_single)
     end do
  end do
  
end subroutine move_ibuf_hg

subroutine move_ibuf_ihg(ibuf,temp1,im_buf,jm_buf,im_out,jm_out)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    move_ibuf_hg  copy from one array to another
!   prgmmr: parrish          org: np22                date: 2004-11-29
!
! abstract: copy from one array to another, converting from int to real
!
! program history log:
!   2004-11-29  parrish
!
!   input argument list:
!     ibuf     - input grid values
!     im_buf   - first index of input array buf
!     jm_buf   - second index of input array buf
!     im_out   - first index of output array temp1
!     jm_out   - second index of output array temp1
!
!   output argument list:
!     temp1    - output grid values
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

!        cp buf to temp1

  use kinds, only: i_long,r_single,i_kind
  implicit none
  
  integer(i_kind),intent(in   ) :: im_buf,jm_buf,im_out,jm_out
  integer(i_long),intent(in   ) :: ibuf(im_buf,jm_buf)
  real(r_single) ,intent(  out) :: temp1(im_out,jm_out)
  
  integer(i_kind) i,j
  
  do j=1,jm_out
     do i=1,im_out
        temp1(i,j)=ibuf(i,j)
     end do
  end do
  
end subroutine move_ibuf_ihg
