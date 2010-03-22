#ifdef WRF
subroutine read_wrf_nmm_binary_guess(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_wrf_nmm_binary_guess        read wrf_nmm interface file
!   prgmmr: parrish          org: np22                date: 2003-09-05
!
! abstract: in place of read_guess for global application, read guess
!             from regional model, in this case the wrf nmm (non-hydrostatic
!             mesoscale model).  This version reads a binary file created
!             in a previous step that interfaces with the wrf infrastructure.
!             A later version will read directly from the wrf restart file.
!             The guess is read in by complete horizontal fields, one field
!             per processor, in parallel.  Each horizontal input field is 
!             converted from the staggered e-grid to an unstaggered a-grid.
!             If filled_grid=.true., then the unstaggered a-grid has all the
!             holes of the e-grid filled, and has twice as many points as the
!             input grid.  If half_grid=.true., then the unstaggered grid is
!             derived from every other row of the input e-grid, and has 1/2 as
!             many points as the input grid.
!
! program history log:
!   2003-09-05  parrish
!   2004-06-22  parrish, document
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-11-20  parrish - change to mpi-io for binary file format
!   2004-12-15  treadon - remove variable mype from call load_geop_hgt,
!                         rename variable wrf_ges_filename as wrfges
!   2005-02-17  todling - ifdef'ed wrf code out
!   2005-02-23  wu - setup for qoption=2 and output stats of RH
!   2005-04-01  treadon - add initialization of ges_oz, prsi_oz; comestic 
!                         format changes
!   2005-05-27  parrish - add call get_derivatives
!   2005-06-13  treadon - remove extra open(nfcst...) statement
!   2005-07-06  parrish - add changes to read pint if available 
!                         (update_pint=.true.)
!   2005-07-22  parrish - read surface roughness length
!   2005-09-29  parrish - add call to get_zderivs
!   2005-11-21  kleist - new calls to genqsat and calctends
!   2005-11-21  derber - make qoption=1 work same as qoption=2
!   2005-11-29  derber - remove external iteration dependent calculations
!   2005-11-29  parrish - correct error in reading of roughness length field
!   2005-12-15  treadon - remove initialization of certain guess arrays (done elsewhere)
!   2006-02-02  treadon - remove load_prsges,load_geop_hgt,prsi,prsl (not used)
!   2006-03-06  parrish - correct u10,v10 grid type flag, igtype.  s/b 1, not 2
!   2006-04-03  derber  - include tuned fact10 for 10m winds
!   2006-04-06  middlecoff - changed nfcst from 11 to lendian_in
!   2006-06-19  wu - changes to allow nfldsig=3 (multiple first guess)
!   2006-07-28  derber  - include sensible temperature
!   2006-07-31  kleist - change to use ges_ps instead of lnps
!   2007-03-13  derber - remove unused qsinv2 from jfunc use list
!   2007-04-12  parrish - add modifications to allow any combination of ikj or ijk
!                          grid ordering for input 3D fields
!   2007-05-02  parrish - fix bug to prevent out of memory reference when pint missing
!   2008-04-16  safford - rm unused uses
!
!   input argument list:
!     mype     - pe number
!
!     NOTES:  need to pay special attention to various surface fields to make sure
!             they are correct (check units).  fact10 needs to be computed from 
!             10m wind, which is included in wrf nmm interface file.
!
!             Cloud water currently set to zero.  Need to fix before doing precip
!             assimilation--wait until global adopts Brad Ferrier's multi-species 
!             scheme??
!
!             Ozone currently set to zero.  No ozone variable in wrf nmm--climatology
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
  use mpimod, only: ierror,mpi_integer,mpi_sum,mpi_comm_world,npe,mpi_rtype, &
       mpi_offset_kind,mpi_info_null,mpi_mode_rdonly,mpi_status_size
  use guess_grids, only: ges_z,ges_ps,ges_pint,ges_pd,ges_tv,ges_q,ges_u,ges_v,&
       fact10,soil_type,veg_frac,veg_type,sfc_rough,sfct,sno,soil_temp,soil_moi,&
       isli,nfldsig,ifilesig,ges_tsen
  use gridmod, only: lat2,lon2,itotsub,&
       pdtop_ll,pt_ll,nlon,nlat,nlon_regional,nsig,nlat_regional,half_grid,&
       filled_grid, &
      displs_s,ijn_s,ltosi_s,ltosj_s,half_nmm_grid2a,fill_nmm_grid2a3
  use constants, only: izero,ione,zero,one_tenth,half,one,grav,fv,zero_single
  use regional_io, only: update_pint
  use gsi_io, only: lendian_in
  implicit none

! Declare passed variables here
  integer(i_kind),intent(in):: mype

! Declare local parameters
  real(r_kind),parameter:: r0_01 = 0.01_r_kind
  real(r_kind),parameter:: r100  = 100.0_r_kind

! Declare local variables
  integer(i_kind) kpint,kt,kq,ku,kv

! NMM variable names stuck in here
  integer(i_kind) mfcst

! other internal variables
  real(r_single),allocatable:: tempa(:,:)
  real(r_single),allocatable::temp1(:,:)
  real(r_single),allocatable::all_loc(:,:,:)
  integer(i_kind),allocatable::igtype(:),kdim(:),kord(:)
  integer(kind=mpi_offset_kind),allocatable::offset(:)
  integer(kind=mpi_offset_kind) this_offset
  integer(i_kind),allocatable::length(:)
  integer(i_kind) this_length
  character(9) wrfges
  character(6) filename 
  integer(i_kind) ifld,im,jm,lm,num_nmm_fields
  integer(i_kind) num_loc_groups,num_j_groups
  integer(i_kind) i,it,j,k
  integer(i_kind) i_pd,i_fis,i_pint,i_t,i_q,i_u,i_v,i_sno,i_u10,i_v10,i_smc,i_stc
  integer(i_kind) i_sm,i_sice,i_sst,i_tsk,i_ivgtyp,i_isltyp,i_vegfrac
  integer(i_kind) isli_this
  real(r_kind) pd,psfc_this,sm_this,sice_this,wmag
  integer(i_kind) num_doubtful_sfct,num_doubtful_sfct_all
  integer(i_llong) n_position
  integer(i_kind) iskip,ksize,jextra,nextra
  integer(i_kind) status(mpi_status_size)
  integer(i_kind) jbegin(0:npe),jend(0:npe-ione)
  integer(i_kind) kbegin(0:npe),kend(0:npe-ione)
  integer(i_long),allocatable:: ibuf(:,:)
  integer(i_long),allocatable:: jbuf(:,:,:)
  real(r_kind) rough0(nlon,nlat)
  real(r_single) rough_in(nlon_regional,nlat_regional)
  real(r_kind) rough_in2(nlon_regional,nlat_regional)
  real(r_kind) work(itotsub)                        
  integer(i_kind) mm1                               
  integer(i_kind) iadd
  character(132) memoryorder

!  NMM input grid dimensions in module reg_glob_ll
!      These are the following:
!          im -- number of NMM longitudes (x-points) on E-grid
!          jm -- number of NMM latitudes (y-points) on E-grid
!          lm -- number of NMM vertical levels ( = nsig for now)


     im=nlon_regional
     jm=nlat_regional
     lm=nsig

!    Following is for convenient NMM/WRF NMM input
     num_nmm_fields=20_i_kind+4_i_kind*lm
     if(update_pint) num_nmm_fields=num_nmm_fields+lm+ione   ! add contribution of PINT
     num_loc_groups=num_nmm_fields/npe
     if(mype == izero) write(6,'(" at 1 in read_wrf_nmm_binary_guess, lm            =",i6)')lm
     if(mype == izero) write(6,'(" at 1 in read_wrf_nmm_binary_guess, num_nmm_fields=",i6)')num_nmm_fields
     if(mype == izero) write(6,'(" at 1 in read_wrf_nmm_binary_guess, nfldsig       =",i6)')nfldsig
     if(mype == izero) write(6,'(" at 1 in read_wrf_nmm_binary_guess, npe           =",i6)')npe
     if(mype == izero) write(6,'(" at 1 in read_wrf_nmm_binary_guess, num_loc_groups=",i6)')num_loc_groups

     allocate(offset(num_nmm_fields))
     allocate(igtype(num_nmm_fields),kdim(num_nmm_fields),kord(num_nmm_fields))
     allocate(length(num_nmm_fields))
     allocate(all_loc(lat2,lon2,num_nmm_fields))
     allocate(temp1(im,jm))

!    igtype is a flag indicating whether each input NMM field is h-grid or v-grid
!    and whether integer or real
!     abs(igtype)=1 for h-grid
!                =2 for v-grid
!     igtype < 0 for integer field

!    offset is the byte count preceding each record to be read from the wrf binary file.
!       used as individual file pointers by mpi_file_read_at

     do it=1,nfldsig
        num_doubtful_sfct=izero
        write(filename,'("sigf",i2.2)')ifilesig(it)
        open(lendian_in,file=filename,form='unformatted') ; rewind lendian_in
        if(mype == izero) write(6,*)'READ_WRF_NMM_OFFSET_FILE:  open lendian_in=',&
             lendian_in,' to file=',filename
        do iskip=1,9
           read(lendian_in)
        end do
        read(lendian_in) wrfges
        if(mype==izero) write(6,*)' in read_wrf_nmm_binary_guess, wrfges=',trim(wrfges)
        read(lendian_in) ! n_position          !  offset for START_DATE record
        
        i=izero
        i=i+ione ; i_pd=i                                                ! pd
        read(lendian_in) n_position
        offset(i)=n_position ; length=im*jm ; igtype(i)=ione ; kdim(i)=ione
        if(mype == izero) write(6,*)' pd, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+ione ; i_fis=i                                                ! fis
        read(lendian_in) n_position
        offset(i)=n_position ; length=im*jm ; igtype(i)=ione ; kdim(i)=ione
        if(mype == izero) write(6,*)' fis, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i_pint=i+ione
        if(update_pint) then
           i_pint=i+ione
           read(lendian_in) n_position,memoryorder
           do k=1,lm+ione
              i=i+ione                                                       ! pint(k)
              if(trim(memoryorder)=='XZY') then
                 iadd=izero
                 kord(i)=lm+ione
              else
                 iadd=(k-ione)*im*jm*4
                 kord(i)=ione
              end if
              offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=lm+ione
              if(mype == izero.and.k==ione) write(6,*)' pint i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
           end do
        end if
        
        i_t=i+ione
        read(lendian_in) n_position,memoryorder
        do k=1,lm
           i=i+ione                                                       ! t(k)
           if(trim(memoryorder)=='XZY') then
              iadd=izero
              kord(i)=lm
           else
              iadd=(k-ione)*im*jm*4
              kord(i)=ione
           end if
           offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=lm
           if(mype == izero.and.k==ione) write(6,*)' temp i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
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
           if(mype == izero.and.k==ione) write(6,*)' q i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        end do
        
        i_u=i+ione
        read(lendian_in) n_position,memoryorder
        do k=1,lm
           i=i+ione                                                       ! u(k)
           if(trim(memoryorder)=='XZY') then
              iadd=izero
              kord(i)=lm
           else
              iadd=(k-ione)*im*jm*4
              kord(i)=ione
           end if
           offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=2_i_kind ; kdim(i)=lm
           if(mype == izero.and.k==ione) write(6,*)' u i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        end do
        
        i_v=i+ione
        read(lendian_in) n_position,memoryorder
        do k=1,lm
           i=i+ione                                                       ! v(k)
           if(trim(memoryorder)=='XZY') then
              iadd=izero
              kord(i)=lm
           else
              iadd=(k-ione)*im*jm*4
              kord(i)=ione
           end if
           offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=2_i_kind ; kdim(i)=lm
           if(mype == izero.and.k==ione) write(6,*)' v i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        end do
        
        i=i+ione ; i_sm=i                                                ! sm
        read(lendian_in) n_position
        offset(i)=n_position ; length=im*jm ; igtype(i)=ione ; kdim(i)=ione
        if(mype == izero) write(6,*)' sm, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+ione ; i_sice=i                                                ! sice
        read(lendian_in) n_position
        offset(i)=n_position ; length=im*jm ; igtype(i)=ione ; kdim(i)=ione
        if(mype == izero) write(6,*)' sice, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+ione ; i_sst=i                                                ! sst
        read(lendian_in) n_position
        offset(i)=n_position ; length=im*jm ; igtype(i)=ione ; kdim(i)=ione
        if(mype == izero) write(6,*)' sst, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+ione ; i_ivgtyp=i                                                ! ivgtyp
        read(lendian_in) n_position
        offset(i)=n_position ; length=im*jm ; igtype(i)=-ione ; kdim(i)=ione
        if(mype == izero) write(6,*)' ivgtyp, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+ione ; i_isltyp=i                                                ! isltyp
        read(lendian_in) n_position
        offset(i)=n_position ; length=im*jm ; igtype(i)=-ione ; kdim(i)=ione
        if(mype == izero) write(6,*)' isltyp, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+ione ; i_vegfrac=i                                                ! vegfrac
        read(lendian_in) n_position
        offset(i)=n_position ; length=im*jm ; igtype(i)=ione ; kdim(i)=ione
        if(mype == izero) write(6,*)' vegfrac, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+ione ; i_sno=i                                                ! sno
        read(lendian_in) n_position
        offset(i)=n_position ; length=im*jm ; igtype(i)=ione ; kdim(i)=ione
        if(mype == izero) write(6,*)' sno, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+ione ; i_u10=i                                                ! u10
        read(lendian_in) n_position
        offset(i)=n_position ; length=im*jm ; igtype(i)=ione ; kdim(i)=ione
        if(mype == izero) write(6,*)' u10, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)

        i=i+ione ; i_v10=i                                                ! v10
        read(lendian_in) n_position
        offset(i)=n_position ; length=im*jm ; igtype(i)=ione ; kdim(i)=ione
        if(mype == izero) write(6,*)' v10, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+ione ; i_smc=i                                             ! smc
        read(lendian_in) n_position,ksize,memoryorder
        if(trim(memoryorder)=='XZY') then
           kord(i)=ksize
        else
           kord(i)=ione
        end if
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=ksize
        if(mype == izero) write(6,*)' smc i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
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
        
        i=i+ione ; i_stc=i                                             ! stc
        read(lendian_in) n_position,ksize,memoryorder
        if(trim(memoryorder)=='XZY') then
           kord(i)=ksize
        else
           kord(i)=ione
        end if
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=ione ; kdim(i)=ksize
        if(mype == izero) write(6,*)' stc i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
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
        
        i=i+ione ; i_tsk=i                                                ! tsk
        read(lendian_in) n_position
        offset(i)=n_position ; length=im*jm ; igtype(i)=ione ; kdim(i)=ione
        if(mype == izero) write(6,*)' tsk, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)

!       bring in z0 (roughness length)
        mm1=mype+ione
        read(lendian_in) rough_in
        if(half_grid) call half_nmm_grid2a(rough_in,nlon_regional,nlat_regional,rough0,ione)
        if(filled_grid) then
           rough_in2=rough_in
           call fill_nmm_grid2a3(rough_in2,nlon_regional,nlat_regional,rough0)
        end if
        do k=1,itotsub
           i=ltosi_s(k)
           j=ltosj_s(k)
           work(k)=rough0(j,i)
        end do
        call mpi_scatterv(work,ijn_s,displs_s,mpi_rtype, &
                       sfc_rough,ijn_s(mm1),mpi_rtype,izero,mpi_comm_world,ierror)

        close(lendian_in)

!    End of stuff from NMM restart file

!          set up evenly distributed index range over all processors for all input fields


        num_loc_groups=num_nmm_fields/npe
        nextra=num_nmm_fields-num_loc_groups*npe
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
        
        allocate(ibuf(im*jm,kbegin(mype):kend(mype)))

        call mpi_file_open(mpi_comm_world,trim(wrfges),mpi_mode_rdonly,mpi_info_null,mfcst,ierror)
        
!                                    read pint
        if(update_pint.and.kord(i_pint)/=ione) then
           allocate(jbuf(im,lm+ione,jbegin(mype):jend(mype)))
           this_offset=offset(i_pint)+(jbegin(mype)-ione)*4*im*(lm+ione)
           this_length=(jend(mype)-jbegin(mype)+ione)*im*(lm+ione)
           call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer, &
                                 status,ierror)
           call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                jbegin,jend,kbegin,kend,mype,npe,im,jm,lm+ione,im,jm,i_pint,i_pint+lm)
           deallocate(jbuf)
        end if
        
!                                    read temps
        if(kord(i_t)/=ione) then
           allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
           this_offset=offset(i_t)+(jbegin(mype)-ione)*4*im*lm
           this_length=(jend(mype)-jbegin(mype)+ione)*im*lm
           call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer, &
                                 status,ierror)
           call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_t,i_t+lm-ione)
           deallocate(jbuf)
        end if

!                                    read q
        if(kord(i_q)/=ione) then
           allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
           this_offset=offset(i_q)+(jbegin(mype)-ione)*4*im*lm
           this_length=(jend(mype)-jbegin(mype)+ione)*im*lm
           call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer, &
                                 status,ierror)
           call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_q,i_q+lm-ione)
           deallocate(jbuf)
        end if

!                                    read u
        if(kord(i_u)/=ione) then
           allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
           this_offset=offset(i_u)+(jbegin(mype)-ione)*4*im*lm
           this_length=(jend(mype)-jbegin(mype)+ione)*im*lm
           call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer, &
                                 status,ierror)
           call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_u,i_u+lm-ione)
           deallocate(jbuf)
        end if

!                                    read v
        if(kord(i_v)/=ione) then
           allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
           this_offset=offset(i_v)+(jbegin(mype)-ione)*4*im*lm
           this_length=(jend(mype)-jbegin(mype)+ione)*im*lm
           call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer, &
                                 status,ierror)
           call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_v,i_v+lm-ione)
           deallocate(jbuf)
        end if

!                                    read smc
        if(kord(i_smc)/=ione) then
           allocate(jbuf(im,ksize,jbegin(mype):jend(mype)))
           this_offset=offset(i_smc)+(jbegin(mype)-ione)*4*im*ksize
           this_length=(jend(mype)-jbegin(mype)+ione)*im*ksize
           call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer, &
                                 status,ierror)
           call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                jbegin,jend,kbegin,kend,mype,npe,im,jm,ksize,im,jm,i_smc,i_smc)
           deallocate(jbuf)
        end if

!                                    read stc
        if(kord(i_stc)/=ione) then
           allocate(jbuf(im,ksize,jbegin(mype):jend(mype)))
           this_offset=offset(i_stc)+(jbegin(mype)-ione)*4*im*ksize
           this_length=(jend(mype)-jbegin(mype)+ione)*im*ksize
           call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer, &
                                 status,ierror)
           call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                jbegin,jend,kbegin,kend,mype,npe,im,jm,ksize,im,jm,i_stc,i_stc)
           deallocate(jbuf)
        end if

!---------------------- read surface files last
        do k=kbegin(mype),kend(mype)
           if(kdim(k)==ione.or.kord(k)==ione) then
              call mpi_file_read_at(mfcst,offset(k),ibuf(1,k),length(k),mpi_integer,status,ierror)
           end if
        end do

        call mpi_file_close(mfcst,ierror)

!   next interpolate to analysis grid, then distribute to subdomains

        allocate(tempa(itotsub,kbegin(mype):kend(mype)))
        do ifld=kbegin(mype),kend(mype)
           if(igtype(ifld) >  izero) then
              call move_ibuf_hg(ibuf(1,ifld),temp1,im,jm,im,jm)
           else
              call move_ibuf_ihg(ibuf(1,ifld),temp1,im,jm,im,jm)
           end if
           if(filled_grid) call fill_nmm_grid2(temp1,im,jm,tempa(1,ifld),abs(igtype(ifld)),ione)
           if(half_grid)   call half_nmm_grid2(temp1,im,jm,tempa(1,ifld),abs(igtype(ifld)),ione)
        end do
        deallocate(ibuf)

        call generic_grid2sub(tempa,all_loc,kbegin(mype),kend(mype),kbegin,kend,mype,num_nmm_fields)
     
        deallocate(tempa)
!    Next do conversion of units as necessary and
!    reorganize into WeiYu's format--

        kt=i_t-ione
        kq=i_q-ione
        ku=i_u-ione
        kv=i_v-ione
        do k=1,nsig
           kt=kt+ione
           kq=kq+ione
           ku=ku+ione
           kv=kv+ione
           do i=1,lon2
              do j=1,lat2
                 ges_u(j,i,k,it) = all_loc(j,i,ku)
                 ges_v(j,i,k,it) = all_loc(j,i,kv)
                 ges_q(j,i,k,it)   = all_loc(j,i,kq)
                 ges_tsen(j,i,k,it)  = all_loc(j,i,kt) ! actually holds sensible temperature
              end do
           end do
        end do
        do i=1,lon2
           do j=1,lat2
              ges_z(j,i,it)    = all_loc(j,i,i_fis)/grav ! NMM surface elevation multiplied by g
              
!             convert wrf nmm pd variable to psfc in mb, and then to log(psfc) in cb
              
              pd=r0_01*all_loc(j,i,i_pd)
              psfc_this=pd+pdtop_ll+pt_ll
              ges_ps(j,i,it)=one_tenth*psfc_this   ! convert from mb to cb
              sno(j,i,it)=all_loc(j,i,i_sno)
              soil_moi(j,i,it)=all_loc(j,i,i_smc)
              soil_temp(j,i,it)=all_loc(j,i,i_stc)
           end do
        end do
        if(update_pint) then
           kpint=i_pint-ione
           do k=1,nsig+ione
              kpint=kpint+ione
              do i=1,lon2
                 do j=1,lat2
                    ges_pint(j,i,k,it)  = all_loc(j,i,kpint)
                 end do
              end do
           end do
           do i=1,lon2
              do j=1,lat2
                 ges_pd(j,i,it)=all_loc(j,i,i_pd)
              end do
           end do
        end if

!       Convert sensible temp to virtual temp
        do k=1,nsig
           do i=1,lon2
              do j=1,lat2
                 ges_tv(j,i,k,it) = ges_tsen(j,i,k,it) * (one+fv*ges_q(j,i,k,it))
              end do
           end do
        end do
     
!    Transfer surface fields
        do i=1,lon2
           do j=1,lat2
              fact10(j,i,it)=one    !  later fix this by using correct w10/w(1)
              wmag=sqrt(ges_u(j,i,1,it)**2+ges_v(j,i,1,it)**2)
              if(wmag > zero)fact10(j,i,it)=sqrt(all_loc(j,i,i_u10)**2 + &
                      all_loc(j,i,i_v10)**2)/wmag
              fact10(j,i,it)=min(max(fact10(j,i,it),half),0.95_r_kind)
              veg_type(j,i,it)=all_loc(j,i,i_ivgtyp)
              veg_frac(j,i,it)=all_loc(j,i,i_vegfrac)
              soil_type(j,i,it)=all_loc(j,i,i_isltyp)
              sm_this=zero
              if(all_loc(j,i,i_sm) /= zero_single) sm_this=one
              sice_this=zero
              if(all_loc(j,i,i_sice) /= zero_single) sice_this=one
              
              isli_this=izero
              if(sice_this == one) isli_this=2_i_kind
              if(sice_this == zero.and.sm_this == zero) isli_this=ione
              isli(j,i,it)=isli_this
              
              sfct(j,i,it)=all_loc(j,i,i_sst)
              if(isli(j,i,it) /= izero) sfct(j,i,it)=all_loc(j,i,i_tsk)
              if(sfct(j,i,it) < one) then

!             For now, replace missing skin temps with 1st sigma level temp
                 sfct(j,i,it)=all_loc(j,i,i_t) 
                 num_doubtful_sfct=num_doubtful_sfct+ione
                 if(num_doubtful_sfct <= 100_i_kind) &
                      write(6,*)' doubtful skint replaced with 1st sigma level t, j,i,mype,sfct=',&
                      j,i,mype,sfct(j,i,it)
              end if
           end do
        end do

     
        call mpi_reduce(num_doubtful_sfct,num_doubtful_sfct_all,ione,mpi_integer,mpi_sum,&
             izero,mpi_comm_world,ierror)
        if(mype == izero) write(6,*)' in read_wrf_nmm_binary_guess, num_doubtful_sfct_all = ',num_doubtful_sfct_all
        if(mype == izero) write(6,*)' in read_wrf_nmm_binary_guess, num_doubtful_sfct_all = ',num_doubtful_sfct_all
     end do ! enddo it     
     deallocate(all_loc)
     deallocate(temp1,igtype,kdim,kord,offset,length)
     

     return 
end subroutine read_wrf_nmm_binary_guess

subroutine read_wrf_nmm_netcdf_guess(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_wrf_nmm_netcdf_guess        read wrf_nmm interface file
!   prgmmr: parrish          org: np22                date: 2003-09-05
!
! abstract: in place of read_guess for global application, read guess
!             from regional model, in this case the wrf nmm (non-hydrostatic
!             mesoscale model).  This version reads a binary file created
!             in a previous step that interfaces with the wrf infrastructure.
!             A later version will read directly from the wrf restart file.
!             The guess is read in by complete horizontal fields, one field
!             per processor, in parallel.  Each horizontal input field is 
!             converted from the staggered e-grid to an unstaggered a-grid.
!             If filled_grid=.true., then the unstaggered a-grid has all the
!             holes of the e-grid filled, and has twice as many points as the
!             input grid.  If half_grid=.true., then the unstaggered grid is
!             derived from every other row of the input e-grid, and has 1/2 as
!             many points as the input grid.
!
! program history log:
!   2003-09-05  parrish
!   2004-06-22  parrish, document
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2005-02-23  wu - setup for qoption=2 and output stats of RH
!   2005-04-01  treadon - add initialization of ges_oz, prsi_oz; comestic format changes
!   2005-05-27  parrish - add call get_derivatives
!   2005-07_06  parrish - add changes to read pint if available (update_pint=.true.)
!   2005-11-21  derber - make qoption=1 work same as qoption=2
!   2005-11-29  derber - remove external iteration dependent calculations
!   2005-12-15  treadon - remove initialization of certain guess arrays (done elsewhere)
!   2006-02-02  treadon - remove load_prsges,load_geop_hgt,prsi,prsl (not used)
!   2006-03-06  parrish - correct u10,v10 grid type flag, igtype.  s/b 1, not 2
!   2006-04-03  derber  - include tuned fact10 for 10m winds
!   2006-04-06  middlecoff - changed nfcst from 11 to lendian_in
!   2006-07-28  derber  - include sensible temperature
!   2006-07-31  kleist - change to use ges_ps instead of lnps
!   2007-03-13  derber - remove unused qsinv2 from jfunc use list
!   2008-04-16  safford - rm unused uses
!
!   input argument list:
!     mype     - pe number
!
!     NOTES:  need to pay special attention to various surface fields to make sure
!             they are correct (check units).  fact10 needs to be computed from 
!             10m wind, which is included in wrf nmm interface file.
!
!             Cloud water currently set to zero.  Need to fix before doing precip
!             assimilation--wait until global adopts Brad Ferrier's multi-species 
!             scheme??
!
!             Ozone currently set to zero.  No ozone variable in wrf nmm--climatology
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
  use mpimod, only: ierror,mpi_integer,mpi_sum,mpi_real4,mpi_comm_world,npe
  use guess_grids, only: ges_z,ges_ps,ges_pint,ges_pd,ges_tv,ges_q,ges_u,ges_v,&
       fact10,soil_type,veg_frac,veg_type,sfct,sno,soil_temp,soil_moi,&
       isli,nfldsig,ifilesig,ges_tsen
  use gridmod, only: lat2,lon2,itotsub,displs_s,ijn_s,&
       pdtop_ll,pt_ll,nlon_regional,nsig,nlat_regional,half_grid,&
       filled_grid
  use constants, only: izero,ione,zero,one_tenth,half,one,grav,fv,zero_single
  use regional_io, only: update_pint
  use gsi_io, only: lendian_in
  implicit none

! Declare passed variables here
  integer(i_kind),intent(in):: mype

! Declare local parameters
  real(r_kind),parameter:: r0_01=0.01_r_kind

! Declare local variables
  integer(i_kind) kpint,kt,kq,ku,kv

! NMM variable names stuck in here

! other internal variables
  real(r_single) tempa(itotsub)
  real(r_single),allocatable::temp1(:,:)
  real(r_single),allocatable::all_loc(:,:,:)
  integer(i_kind),allocatable::itemp1(:,:)
  integer(i_kind),allocatable::igtype(:),jsig_skip(:)
  character(60),allocatable::identity(:)
  character(6) filename 
  integer(i_kind) irc_s_reg(npe),ird_s_reg(npe)
  integer(i_kind) ifld,im,jm,lm,num_nmm_fields
  integer(i_kind) num_all_fields,num_loc_groups,num_all_pad
  integer(i_kind) i,icount,icount_prev,it,j,k
  integer(i_kind) i_0,i_pd,i_fis,i_pint,i_t,i_q,i_u,i_v,i_sno,i_u10,i_v10,i_smc,i_stc
  integer(i_kind) i_sm,i_sice,i_sst,i_tsk,i_ivgtyp,i_isltyp,i_vegfrac
  integer(i_kind) isli_this
  real(r_kind) pd,psfc_this,sm_this,sice_this,wmag
  integer(i_kind) num_doubtful_sfct,num_doubtful_sfct_all

!  NMM input grid dimensions in module reg_glob_ll
!      These are the following:
!          im -- number of NMM longitudes (x-points) on E-grid
!          jm -- number of NMM latitudes (y-points) on E-grid
!          lm -- number of NMM vertical levels ( = nsig for now)


     num_doubtful_sfct=izero

     im=nlon_regional
     jm=nlat_regional
     lm=nsig

!    Following is for convenient NMM/WRF NMM input
     num_nmm_fields=14_i_kind+4_i_kind*lm
     if(update_pint) num_nmm_fields=num_nmm_fields+lm+ione   ! add contribution of PINT
     num_all_fields=num_nmm_fields*nfldsig
     num_loc_groups=num_all_fields/npe
     if(mype == izero) write(6,'(" at 1 in read_wrf_nmm_netcdf_guess, lm            =",i6)')lm
     if(mype == izero) write(6,'(" at 1 in read_wrf_nmm_netcdf_guess, num_nmm_fields=",i6)')num_nmm_fields
     if(mype == izero) write(6,'(" at 1 in read_wrf_nmm_netcdf_guess, nfldsig       =",i6)')nfldsig
     if(mype == izero) write(6,'(" at 1 in read_wrf_nmm_netcdf_guess, num_all_fields=",i6)')num_all_fields
     if(mype == izero) write(6,'(" at 1 in read_wrf_nmm_netcdf_guess, npe           =",i6)')npe
     if(mype == izero) write(6,'(" at 1 in read_wrf_nmm_netcdf_guess, num_loc_groups=",i6)')num_loc_groups
     do 
        num_all_pad=num_loc_groups*npe
        if(num_all_pad >= num_all_fields) exit
        num_loc_groups=num_loc_groups+ione
     end do
     if(mype == izero) write(6,'(" at 1 in read_wrf_nmm_netcdf_guess, num_all_pad   =",i6)')num_all_pad
     if(mype == izero) write(6,'(" at 1 in read_wrf_nmm_netcdf_guess, num_loc_groups=",i6)')num_loc_groups

     allocate(all_loc(lat2,lon2,num_all_pad))
     allocate(jsig_skip(num_nmm_fields))
     allocate(igtype(num_nmm_fields))
     allocate(identity(num_nmm_fields))

!    igtype is a flag indicating whether each input NMM field is h-grid or v-grid
!    and whether integer or real
!     abs(igtype)=1 for h-grid
!                =2 for v-grid
!     igtype < 0 for integer field

     i=izero
     i=i+ione ; i_pd=i                                                ! pd
     write(identity(i),'("record ",i3,"--pd")')i
     jsig_skip(i)=9_i_kind     ! number of files to skip before getting to pd
     igtype(i)=ione
     i=i+ione ; i_fis=i                                               ! fis
     write(identity(i),'("record ",i3,"--fis")')i
     jsig_skip(i)=izero
     igtype(i)=ione

     if(update_pint) then
        i_pint=i+ione
        do k=1,lm+ione
           i=i+ione                                                       ! pint(k)
           write(identity(i),'("record ",i3,"--pint(",i2,")")')i,k
           jsig_skip(i)=izero
           igtype(i)=ione
        end do
     end if

     i_t=i+ione
     do k=1,lm
        i=i+ione                                                       ! t(k)
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
        jsig_skip(i)=izero ; igtype(i)=2_i_kind
     end do
     i=i+ione   ; i_sm=i                                              ! sm
     write(identity(i),'("record ",i3,"--sm")')i
     jsig_skip(i)=izero ; igtype(i)=ione
     i=i+ione ; i_sice=i                                              ! sice
     write(identity(i),'("record ",i3,"--sice")')i
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
     i=i+ione ; i_smc=i                                               ! smc
     write(identity(i),'("record ",i3,"--smc(",i2,")")')i,k
     jsig_skip(i)=izero ; igtype(i)=ione
     i=i+ione ; i_stc=i                                               ! stc
     write(identity(i),'("record ",i3,"--stc(",i2,")")')i,k
     jsig_skip(i)=izero ; igtype(i)=ione
     i=i+ione ; i_tsk=i                                               ! tsk
     write(identity(i),'("record ",i3,"--sst")')i
     jsig_skip(i)=izero ; igtype(i)=ione

!    End of stuff from NMM restart file

     allocate(temp1(im,jm),itemp1(im,jm))
     
     do i=1,npe
        irc_s_reg(i)=ijn_s(mype+ione)
     end do
     ird_s_reg(1)=izero
     do i=1,npe
        if(i /= ione) ird_s_reg(i)=ird_s_reg(i-ione)+irc_s_reg(i-ione)
     end do
     
!    Read wrf NMM fixed format file created from external interface
!    This is done by reading in parallel from every pe, and redistributing
!    to local domains once for every npe fields read in, using 
!    mpi_all_to_allv

     icount=izero
     icount_prev=ione
     do it=1,nfldsig
        write(filename,'("sigf",i2.2)')ifilesig(it)
        open(lendian_in,file=filename,form='unformatted') ; rewind lendian_in

!       Read, interpolate, and distribute NMM restart fields
        do ifld=1,num_nmm_fields
           icount=icount+ione
           if(jsig_skip(ifld) > izero) then
              do i=1,jsig_skip(ifld)
                 read(lendian_in)
              end do
           end if
           if(mype == mod(icount-ione,npe)) then
              if(igtype(ifld) > izero) then
                 read(lendian_in)((temp1(i,j),i=1,im),j=1,jm)
              else
                 read(lendian_in)((itemp1(i,j),i=1,im),j=1,jm)
                 do j=1,jm
                    do i=1,im
                       temp1(i,j)=itemp1(i,j)
                    end do
                 end do
              end if
              write(6,'(" ifld, temp1(im/2,jm/2)=",i6,e15.5)')&
                 ifld,temp1(im/2,jm/2)
              if(filled_grid) call fill_nmm_grid2(temp1,im,jm,tempa,abs(igtype(ifld)),ione)
              if(half_grid)   call half_nmm_grid2(temp1,im,jm,tempa,abs(igtype(ifld)),ione)
! if(igtype(ifld) == 2_i_kind) then
! write(6,*)' at 1.1 in read_wrf_nmm_netcdf_guess, max,min temp1 = ',maxval(temp1),minval(temp1)
! write(6,*)' at 1.1 in read_wrf_nmm_netcdf_guess, max,min tempa = ',maxval(tempa),minval(tempa)
! end if

           else
              read(lendian_in)
           end if
           
!          Distribute to local domains everytime we have npe fields
           if(mod(icount,npe) == izero.or.icount == num_all_fields) then
              call mpi_alltoallv(tempa,ijn_s,displs_s,mpi_real4, &
                   all_loc(1,1,icount_prev),irc_s_reg,ird_s_reg,mpi_real4,mpi_comm_world,ierror)
              icount_prev=icount+ione
           end if

        end do
        close(lendian_in)
     end do
!    do kv=i_v,i_v+nsig-ione
!       if(mype == izero) write(6,*)' at 1.15, kv,mype,j,i,v=', &
!            kv,mype,2,1,all_loc(2,1,kv)
!    end do


!    Next do conversion of units as necessary and
!    reorganize into WeiYu's format--

!    do kv=i_v,i_v+nsig-ione
!       if(mype == izero) write(6,*)' at 1.16, kv,mype,j,i,v=', &
!            kv,mype,2,1,all_loc(2,1,kv)
!    end do

     do it=1,nfldsig
        i_0=(it-ione)*num_nmm_fields
        kt=i_0+i_t-ione
        kq=i_0+i_q-ione
        ku=i_0+i_u-ione
        kv=i_0+i_v-ione

        do k=1,nsig
           kt=kt+ione
           kq=kq+ione
           ku=ku+ione
           kv=kv+ione

           do i=1,lon2
              do j=1,lat2
!                if(mype == izero.and.j == 2_i_kind.and.i == ione) write(6,*)' at 1.2, k,mype,j,i,u,v=', &
!                     k,mype,j,i,all_loc(j,i,ku),all_loc(j,i,kv)
                 ges_u(j,i,k,it) = all_loc(j,i,ku)
                 ges_v(j,i,k,it) = all_loc(j,i,kv)
                 ges_q(j,i,k,it)   = all_loc(j,i,kq)
                 ges_tsen(j,i,k,it)  = all_loc(j,i,kt) ! actually holds sensible temperature
              end do
           end do
        end do

        do i=1,lon2
           do j=1,lat2
              ges_z(j,i,it)    = all_loc(j,i,i_0+i_fis)/grav ! NMM surface elevation multiplied by g

!             convert wrf nmm pd variable to psfc in mb, and then to log(psfc) in cb
              
              pd=r0_01*all_loc(j,i,i_0+i_pd)
              psfc_this=pd+pdtop_ll+pt_ll
              ges_ps(j,i,it)=one_tenth*psfc_this   ! convert from mb to cb
              sno(j,i,it)=all_loc(j,i,i_0+i_sno)
              soil_moi(j,i,it)=all_loc(j,i,i_0+i_smc)
              soil_temp(j,i,it)=all_loc(j,i,i_0+i_stc)
           end do
        end do

        if(mype == 10_i_kind) write(6,*)' in read_wrf_nmm_netcdf_guess, min,max(soil_moi)=', &
             minval(soil_moi),maxval(soil_moi)
        if(mype == 10_i_kind) write(6,*)' in read_wrf_nmm_netcdf_guess, min,max(soil_temp)=', &
             minval(soil_temp),maxval(soil_temp)
        if(update_pint) then
           kpint=i_0+i_pint-ione
           do k=1,nsig+ione
              kpint=kpint+ione
              do i=1,lon2
                 do j=1,lat2
                    ges_pint(j,i,k,it)  = all_loc(j,i,kpint) ! actually holds sensible temperature
                 end do
              end do
           end do
           do i=1,lon2
              do j=1,lat2
                 ges_pd(j,i,it)  = all_loc(j,i,i_0+i_pd)
              end do
           end do
        end if

!       Convert sensible temp to virtual temp
        do k=1,nsig
           do i=1,lon2
              do j=1,lat2
                 ges_tv(j,i,k,it) = ges_tsen(j,i,k,it) * (one+fv*ges_q(j,i,k,it))
              end do
           end do
        end do
     end do

!    Transfer surface fields
     do it=1,nfldsig
        i_0=(it-ione)*num_nmm_fields
        do i=1,lon2
           do j=1,lat2
              fact10(j,i,it)=one    !  later fix this by using correct w10/w(1)
              wmag=sqrt(ges_u(j,i,1,it)**2+ges_v(j,i,1,it)**2)
              if(wmag > zero)fact10(j,i,it)=sqrt(all_loc(j,i,i_0+i_u10)**2 + &
                      all_loc(j,i,i_0+i_v10)**2)/wmag
              fact10(j,i,it)=min(max(fact10(j,i,it),half),0.95_r_kind)
              veg_type(j,i,it)=all_loc(j,i,i_0+i_ivgtyp)
              veg_frac(j,i,it)=all_loc(j,i,i_0+i_vegfrac)
              soil_type(j,i,it)=all_loc(j,i,i_0+i_isltyp)
!             soil_temp(j,i,it)=all_loc(j,i,i_0+i_stc)
!             soil_moi(j,i,it)=all_loc(j,i,i_0+i_smc)
              sm_this=zero
              if(all_loc(j,i,i_0+i_sm) /= zero_single) sm_this=one
              sice_this=zero
              if(all_loc(j,i,i_0+i_sice) /= zero_single) sice_this=one
              
              isli_this=izero
              if(sice_this == one) isli_this=2_i_kind
              if(sice_this == zero.and.sm_this == zero) isli_this=ione
              isli(j,i,it)=isli_this
              
              sfct(j,i,it)=all_loc(j,i,i_0+i_sst)
              if(isli(j,i,it) /= izero) sfct(j,i,it)=all_loc(j,i,i_0+i_tsk)
              if(sfct(j,i,it) < one) then

!             For now, replace missing skin temps with 1st sigma level temp
                 sfct(j,i,it)=all_loc(j,i,i_0+i_t) 
                 write(6,*)' doubtful skint replaced with 1st sigma level t, j,i,mype,sfct=',&
                      j,i,mype,sfct(j,i,it)
                 num_doubtful_sfct=num_doubtful_sfct+ione
              end if
           end do
        end do
     end do
     
     call mpi_reduce(num_doubtful_sfct,num_doubtful_sfct_all,ione,mpi_integer,mpi_sum,&
          izero,mpi_comm_world,ierror)
     if(mype == izero) write(6,*)' in read_wrf_nmm_netcdf_guess, num_doubtful_sfct_all = ',num_doubtful_sfct_all
     if(mype == izero) write(6,*)' in read_wrf_nmm_netcdf_guess, num_doubtful_sfct_all = ',num_doubtful_sfct_all
     if(mype == 10_i_kind) write(6,*)' in read_wrf_nmm_netcdf_guess, min,max(sfct)=', &
          minval(sfct),maxval(sfct)
     if(mype == 10_i_kind) write(6,*)' in read_wrf_nmm_netcdf_guess, min,max(veg_type)=', &
          minval(veg_type),maxval(veg_type)
     if(mype == 10_i_kind) write(6,*)' in read_wrf_nmm_netcdf_guess, min,max(veg_frac)=', &
          minval(veg_frac),maxval(veg_frac)
     if(mype == 10_i_kind) write(6,*)' in read_wrf_nmm_netcdf_guess, min,max(soil_type)=', &
          minval(soil_type),maxval(soil_type)
     if(mype == 10_i_kind) write(6,*)' in read_wrf_nmm_netcdf_guess, min,max(isli)=', &
          minval(isli),maxval(isli)
     
     deallocate(all_loc,jsig_skip,igtype,identity)
     deallocate(temp1,itemp1)


end subroutine read_wrf_nmm_netcdf_guess

subroutine read_nems_nmmb_guess(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_nems_nmmb_guess             read nems_nmmb guess file
!   prgmmr: parrish          org: np22                date: 2003-09-05
!
! abstract: in place of read_guess for global application, read guess
!             from regional model, in this case the nems nmmb (non-hydrostatic
!             mesoscale model).  This version reads directly from the nems input file
!             using nemsio routines.  Each horizontal input field is interpolated
!             from the b-grid to the a-grid with different resolution, determined
!             by parameter grid_ratio_nmmb.
!
! program history log:
!   2009-03-18  parrish
!   2010-03-12  parrish - add option to read ozone from location "o3mr".  If use_gfs_ozone =.true.
!                           then skip reading of ozone, since it will be brought in directly
!                           from gfs sigma file to analysis grid with later call to
!                           read_gfs_ozone_for_regional.
!   2010-03-15  parrish - add option regional_ozone to turn on ozone in regional analysis
!
!   input argument list:
!     mype     - pe number
!
!     NOTES:  need to pay special attention to various surface fields to make sure
!             they are correct (check units).  fact10 needs to be computed from 
!             10m wind, which is included in wrf nmm interface file.
!
!             Cloud water currently set to zero.  Need to fix before doing precip
!             assimilation--wait until global adopts Brad Ferrier's multi-species 
!             scheme??
!
!             Ozone currently set to zero.  No ozone variable in wrf nmm--climatology
!             instead.  Do we use climatology?
!
!             No background bias yet. (biascor ignored)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use mpimod, only: ierror,mpi_comm_world,mpi_integer,mpi_sum
  use guess_grids, only: ges_z,ges_ps,ges_pint,ges_pd,ges_tv,ges_q,ges_u,ges_v,&
       fact10,soil_type,veg_frac,veg_type,sfc_rough,sfct,sno,soil_temp,soil_moi,&
       isli,nfldsig,ges_tsen,ges_oz
  use gridmod, only: lat2,lon2,pdtop_ll,pt_ll,nsig,nmmb_verttype,use_gfs_ozone,regional_ozone
  use constants, only: izero,ione,zero,one_tenth,half,one,fv,rd_over_cp
  use regional_io, only: update_pint
  use gsi_nemsio_mod, only: gsi_nemsio_open,gsi_nemsio_close,gsi_nemsio_read
  implicit none

! Declare passed variables here
  integer(i_kind),intent(in):: mype

! Declare local parameters
  real(r_kind),parameter:: r0_01 = 0.01_r_kind
  real(r_kind),parameter:: r100  = 100.0_r_kind

! Declare local variables

! other internal variables
  character(255) wrfges
  integer(i_kind) i,it,j,k,kr,mype_input
  integer(i_kind) isli_this
  real(r_kind) pd,psfc_this,wmag,pd_to_ps
  integer(i_kind) num_doubtful_sfct,num_doubtful_sfct_all
  real(r_kind),dimension(lat2,lon2):: smthis,sicethis,u10this,v10this,sstthis,tskthis
  logical good_o3mr

!     get conversion factor for pd to psfc

  if(nmmb_verttype=='OLD') then
     pd_to_ps=pdtop_ll+pt_ll
  else
     pd_to_ps=pt_ll
  end if

!        do serial input for now, with mpi_send to put on appropriate processor.

  mype_input=izero
  do it=1,nfldsig
     num_doubtful_sfct=izero
       
     if(mype==mype_input) then
        if(it==ione)then
           wrfges = 'wrf_inout'
        else
           write(wrfges,'("wrf_inou",i1.1)')it
        endif
     end if
     call gsi_nemsio_open(wrfges,'READ', &
                          'READ_NEMS_NMMB_GUESS:  problem with wrfges',mype,mype_input)

!                            ! pd

     call gsi_nemsio_read('dpres','hybrid sig lev','H',ione,ges_pd(:,:,it),mype,mype_input)
     do i=1,lon2
        do j=1,lat2
!               convert wrf nmm pd variable to psfc in mb, and then to log(psfc) in cb
           pd=r0_01*ges_pd(j,i,it)
           psfc_this=pd+pd_to_ps
           ges_ps(j,i,it)=one_tenth*psfc_this
        end do
     end do

!                          !   fis

     call gsi_nemsio_read('hgt','sfc','H',ione,ges_z(:,:,it),mype,mype_input)

!                          !   u,v,q,tsen,tv
     do kr=1,nsig
        k=nsig+ione-kr
        call gsi_nemsio_read('ugrd','mid layer','V',kr,ges_u(:,:,k,it),   mype,mype_input)
        call gsi_nemsio_read('vgrd','mid layer','V',kr,ges_v(:,:,k,it),   mype,mype_input)
        call gsi_nemsio_read('spfh','mid layer','H',kr,ges_q(:,:,k,it),   mype,mype_input)
        call gsi_nemsio_read('tmp' ,'mid layer','H',kr,ges_tsen(:,:,k,it),mype,mype_input)
        do i=1,lon2
           do j=1,lat2
              ges_tv(j,i,k,it) = ges_tsen(j,i,k,it) * (one+fv*ges_q(j,i,k,it))
           end do
        end do
        if(regional_ozone) then
           if(use_gfs_ozone) then
              ges_oz(:,:,k,it)=zero
           else
              good_o3mr=.false.
              call gsi_nemsio_read('o3mr' ,'mid layer','H',kr,ges_oz(:,:,k,it),mype,mype_input,good_o3mr)
              if(.not.good_o3mr) write(6,*)' IN READ_NEMS_NMMB_GUESS, O3MR FIELD NOT YET AVAILABLE'
           end if
        end if
     end do

                                   !   pint
     if(update_pint) then

        do kr=1,nsig+ione
           k=nsig+2_i_kind-kr
           call gsi_nemsio_read('pres' ,'layer','H',kr,ges_pint(:,:,k,it),mype,mype_input)
        end do

     end if

!                            ! sno
     call gsi_nemsio_read('sno' ,'sfc','H',ione,sno(:,:,it),mype,mype_input)

!                            ! surface roughness
     call gsi_nemsio_read('zorl' ,'sfc','H',ione,sfc_rough(:,:,it),mype,mype_input)

!                            ! soil_moisture
     call gsi_nemsio_read('smc' ,'soil layer','H',ione,soil_moi(:,:,it),mype,mype_input)

!                            ! soil_temp
     call gsi_nemsio_read('stc' ,'soil layer','H',ione,soil_temp(:,:,it),mype,mype_input)

!                            ! veg type
     call gsi_nemsio_read('vgtyp' ,'sfc','H',ione,veg_type(:,:,it),mype,mype_input)

    !           because veg_type is integer quantity, do an nint operation to remove fractional values
    !           due to interpolation
     do i=1,lon2
        do j=1,lat2
           veg_type(j,i,it)=float(nint(veg_type(j,i,it)))
        end do
     end do
!                            ! veg frac
     call gsi_nemsio_read('vegfrc' ,'sfc','H',ione,veg_frac(:,:,it),mype,mype_input)


!                            ! soil type
     call gsi_nemsio_read('sltyp' ,'sfc','H',ione,soil_type(:,:,it),mype,mype_input)

    !           because soil_type is integer quantity, do an nint operation to remove fractional values
    !           due to interpolation
     do i=1,lon2
        do j=1,lat2
           soil_type(j,i,it)=float(nint(soil_type(j,i,it)))
        end do
     end do

!                            ! sm
     call gsi_nemsio_read('sm' ,'sfc','H',ione,smthis(:,:),mype,mype_input)

    !           because sm is integer quantity, do an nint operation to remove fractional values
    !           due to interpolation
     do i=1,lon2
        do j=1,lat2
           smthis(j,i)=float(nint(smthis(j,i)))
        end do
     end do

!                            ! sice
     call gsi_nemsio_read('sice' ,'sfc','H',ione,sicethis(:,:),mype,mype_input)

    !           because sice is integer quantity, do an nint operation to remove fractional values
    !           due to interpolation
     do i=1,lon2
        do j=1,lat2
           sicethis(j,i)=float(nint(sicethis(j,i)))
        end do
     end do

!                            ! sst
     call gsi_nemsio_read('tsea' ,'sfc','H',ione,sstthis(:,:),mype,mype_input)

!                            ! tsk
     call gsi_nemsio_read('ths' ,'sfc','H',ione,tskthis(:,:),mype,mype_input)
!                 convert tsk from potential to virtual temperature
     if(mype==izero) write(6,*)' in read_nems_nmmb_guess, rd_over_cp=',rd_over_cp
     do i=1,lon2
        do j=1,lat2
           tskthis(j,i)=tskthis(j,i)*(ges_ps(j,i,it)/r100)**rd_over_cp
        end do
     end do

!                            ! u10,v10
     call gsi_nemsio_read('u10' ,'10 m above gnd','H',ione,u10this(:,:),mype,mype_input)
     call gsi_nemsio_read('v10' ,'10 m above gnd','H',ione,v10this(:,:),mype,mype_input)

     do i=1,lon2
        do j=1,lat2
           fact10(j,i,it)=one    !  later fix this by using correct w10/w(1)
           wmag=sqrt(ges_u(j,i,1,it)**2+ges_v(j,i,1,it)**2)
           if(wmag > zero)fact10(j,i,it)=sqrt(u10this(j,i)**2+v10this(j,i)**2)/wmag
           fact10(j,i,it)=min(max(fact10(j,i,it),half),0.95_r_kind)

           if(smthis(j,i)/=zero) smthis(j,i)=one
           if(sicethis(j,i)/=zero) sicethis(j,i)=one
           isli_this=izero
           if(sicethis(j,i)==one) isli_this=2_i_kind
           if(sicethis(j,i)==zero.and.smthis(j,i)==zero) isli_this=ione
           isli(j,i,it)=isli_this

           sfct(j,i,it)=sstthis(j,i)
           if(isli(j,i,it)/=izero) sfct(j,i,it)=tskthis(j,i)
           if(sfct(j,i,it)<one) then

!             For now, replace missing skin temps with 1st sigma level temp
              sfct(j,i,it)=ges_tsen(j,i,1,it)
              num_doubtful_sfct=num_doubtful_sfct+ione
              if(num_doubtful_sfct <= 100_i_kind) &
                   write(6,*)' doubtful skint replaced with 1st sigma level t, j,i,mype,sfct=',&
                   j,i,mype,sfct(j,i,it)
           end if
        end do
     end do

     call gsi_nemsio_close(wrfges,'READ_NEMS_NMMB_GUESS',mype,mype_input)
     call mpi_reduce(num_doubtful_sfct,num_doubtful_sfct_all,ione,mpi_integer,mpi_sum,&
                     izero,mpi_comm_world,ierror)
     if(mype == izero) write(6,*)' in read_nems_nmmb_binary_guess, num_doubtful_sfct_all = ', &
                                                           num_doubtful_sfct_all
  end do ! enddo it

  return 
end subroutine read_nems_nmmb_guess
#else /* Start no WRF-library block */
subroutine read_wrf_nmm_binary_guess()
  write(6,*)'READ_WRF_NMM_BINARY_GUESS:  dummy routine, does nothing!'
end subroutine read_wrf_nmm_binary_guess
subroutine read_wrf_nmm_netcdf_guess()
  write(6,*)'READ_WRF_NMM_NETCDF_GUESS:  dummy routine, does nothing!'
end subroutine read_wrf_nmm_netcdf_guess
subroutine read_nems_nmmb_guess()
  write(6,*)'READ_NEMS_NMMB_GUESS:  dummy routine, does nothing!'
end subroutine read_nems_nmmb_guess
#endif /* End no WRF-library block */
