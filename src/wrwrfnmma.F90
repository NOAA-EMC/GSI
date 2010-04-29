#ifdef WRF
subroutine wrwrfnmma_binary(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    wrwrfnmma              write out wrf NMM restart file
!   prgmmr: parrish          org: np22                date: 2004-06-23
!
! abstract:  read wrf NMM guess restart interface file, add analysis
!            increment, and write out wrf NMM analysis restart 
!            interface file.
!
! program history log:
!   2004-06-23  parrish, document
!   2004-08-03  treadon - add only to module use, add intent in/out
!   2004-11-22  parrish - rewrite for mpi-io
!   2004-12-15  treadon - write analysis to file "wrf_inout"
!   2005-07-06  parrish - update and write out pint if update_pint=.true.
!   2006-04-06  middlecoff - changed nfcst from 11 to lendian_in
!   2006-07-28  derber - include sensible temperature
!   2006-07-31  kleist - change to use ges_ps instead of lnps
!   2007-04-12  parrish - add modifications to allow any combination of ikj or ijk
!                          grid ordering for input 3D fields
!   2007-05-02  parrish - fix bug to prevent out of memory reference when pint missing
!   2008-04-01  safford - rm unused uses
!   2008-12-05  todling - adjustment for dsfct time dimension addition
!
!   input argument list:
!     mype     - pe number
!
!   output argument list:
!     no output arguments
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_single,i_long,i_llong,i_kind
  use regional_io, only: update_pint
  use guess_grids, only: ges_ps,ges_pint,ges_pd,ges_u,ges_v,ges_q,&
       ntguessfc,ntguessig,ifilesig,dsfct,ges_tsen
  use mpimod, only: mpi_comm_world,ierror,mpi_byte,mpi_integer4,npe, &
       mpi_offset_kind,mpi_info_null,mpi_mode_rdwr,mpi_status_size
  use gridmod, only: iglobal,itotsub,pt_ll,update_regsfc,&
       half_grid,filled_grid,pdtop_ll,nlat_regional,nlon_regional,&
       nsig,lat1,lon1,eta2_ll
  use constants, only: zero_single
  use gsi_io, only: lendian_in
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

! Declare local constants
  real(r_kind),parameter:: r10=10.0_r_kind
  real(r_kind),parameter:: r100=100.0_r_kind
  real(r_kind),parameter:: r225=225.0_r_kind

! Declare local variables
  character(9) wrfanl

  integer(i_kind) im,jm,lm
  real(r_single),allocatable::temp1(:),tempa(:,:),tempb(:,:)
  real(r_single),allocatable::all_loc(:,:,:)
  integer(i_kind),allocatable::igtype(:),kdim(:),kord(:)
  integer(kind=mpi_offset_kind),allocatable::offset(:)
  integer(kind=mpi_offset_kind) this_offset,offset_start_date
  integer(i_kind),allocatable::length(:)
  integer(i_kind) this_length,length_start_date
  character(6) filename
  integer(i_kind) i,j,k,kpint,kt,kq,ku,kv,it,i_pd,i_pint,i_t,i_q,i_u,i_v
  integer(i_kind) i_sst,i_tsk
  integer(i_kind) num_nmm_fields,num_j_groups,num_loc_groups
  real(r_kind) pd,psfc_this
  integer(i_llong) n_position
  integer(i_kind) iskip,jextra,nextra
  integer(i_kind) status(mpi_status_size)
  integer(i_kind) jbegin(0:npe),jend(0:npe-1)
  integer(i_kind) kbegin(0:npe),kend(0:npe-1)
  integer(i_long),allocatable:: ibuf(:,:)
  integer(i_long),allocatable:: jbuf(:,:,:)
  integer(i_kind) ifld,mfcst
  integer(i_long) iyear,imonth,iday,ihour,iminute,isecond
  character(1) chdrbuf(2048)
  integer(i_kind) iadd
  character(132) memoryorder

!   1. get offsets etc only for records to be updated

!        they are PD, T, Q, U, V, skint/sst

  im=nlon_regional
  jm=nlat_regional
  lm=nsig

  num_nmm_fields=3+4*lm
  if(update_pint) num_nmm_fields=num_nmm_fields+lm+1  ! contribution from PINT
  allocate(offset(num_nmm_fields))
  allocate(igtype(num_nmm_fields),kdim(num_nmm_fields),kord(num_nmm_fields))
  allocate(length(num_nmm_fields))

!    igtype is a flag indicating whether each input NMM field is h-, or v-grid
!    and whether integer or real
!     abs(igtype)=1 for h-grid
!                =2 for u-grid
!
!     igtype = -1 for integer field

!    offset is the byte count preceding each record to be read/written from/to the wrf binary file.
!       used as individual file pointers by mpi_file_read/mpi_file_write

  it=ntguessig

  write(filename,'("sigf",i2.2)')ifilesig(it)
  open(lendian_in,file=filename,form='unformatted') ; rewind lendian_in
  if(mype == 0) write(6,*)'READ_WRF_NMM_OFFSET_FILE:  open lendian_in=',lendian_in,' to file=',filename
  read(lendian_in) iyear,imonth,iday,ihour,iminute,isecond

  do iskip=2,9
     read(lendian_in)
  end do
  read(lendian_in) 
  read(lendian_in) n_position          !  offset for START_DATE record
  offset_start_date=n_position
  length_start_date=2048

!     open wrf file for mpi-io reading and writing
  wrfanl = 'wrf_inout'
  call mpi_file_open(mpi_comm_world,trim(wrfanl),mpi_mode_rdwr,mpi_info_null,mfcst,ierror)

!     update START_DATE record so it contains new analysis time in place of old starting time
  call mpi_file_read_at(mfcst,offset_start_date,chdrbuf,length_start_date,mpi_byte,status,ierror)
  if(mype==0)  then
     call update_start_date(chdrbuf,iyear,imonth,iday,ihour,iminute,isecond)
     call mpi_file_write_at(mfcst,offset_start_date,chdrbuf,length_start_date,mpi_byte,status,ierror)
  end if

  if(mype==0) write(6,*)' in read_wrf_nmm_binary_guess, wrfanl=',trim(wrfanl)

  i=0
  i=i+1 ; i_pd=i                                                ! pd
  read(lendian_in) n_position
  offset(i)=n_position ; length=im*jm ; igtype(i)=1 ; kdim(i)=1
  if(mype == 0) write(6,*)' pd, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)

  read(lendian_in)                                                   ! fis

  i_pint=i+1
  if(update_pint) then
     i_pint=i+1
     read(lendian_in) n_position,memoryorder
     do k=1,lm+1
        i=i+1                                                     ! pint(k)
        if(trim(memoryorder)=='XZY') then
           iadd=0
           kord(i)=lm+1
        else
           iadd=(k-1)*im*jm*4
           kord(i)=1
        end if
        offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=lm+1
        if(mype == 0.and.k==1) write(6,*)' temp i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
     end do
  end if

  i_t=i+1
  read(lendian_in) n_position,memoryorder
  do k=1,lm
     i=i+1                                                       ! t(k)
     if(trim(memoryorder)=='XZY') then
        iadd=0
        kord(i)=lm
     else
        iadd=(k-1)*im*jm*4
        kord(i)=1
     end if
     offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=lm
     if(mype == 0.and.k==1) write(6,*)' temp i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
  end do

  i_q=i+1
  read(lendian_in) n_position,memoryorder
  do k=1,lm
     i=i+1                                                       ! q(k)
     if(trim(memoryorder)=='XZY') then
        iadd=0
        kord(i)=lm
     else
        iadd=(k-1)*im*jm*4
        kord(i)=1
     end if
     offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=lm
     if(mype == 0.and.k==1) write(6,*)' q i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
  end do

  i_u=i+1
  read(lendian_in) n_position,memoryorder
  do k=1,lm
     i=i+1                                                       ! u(k)
     if(trim(memoryorder)=='XZY') then
        iadd=0
        kord(i)=lm
     else
        iadd=(k-1)*im*jm*4
        kord(i)=1
     end if
     offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=2 ; kdim(i)=lm
     if(mype == 0.and.k==1) write(6,*)' u i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
  end do

  i_v=i+1
  read(lendian_in) n_position,memoryorder
  do k=1,lm
     i=i+1                                                       ! v(k)
     if(trim(memoryorder)=='XZY') then
        iadd=0
        kord(i)=lm
     else
        iadd=(k-1)*im*jm*4
        kord(i)=1
     end if
     offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=2 ; kdim(i)=lm
     if(mype == 0.and.k==1) write(6,*)' v i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
  end do

  read(lendian_in)                                                    ! sm
  read(lendian_in)                                                    ! sice

  i=i+1 ; i_sst=i                                                ! sst
  read(lendian_in) n_position
  offset(i)=n_position ; length=im*jm ; igtype(i)=1 ; kdim(i)=1
  if(mype == 0) write(6,*)' sst, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)

  read(lendian_in)                                                    ! ivgtyp
  read(lendian_in)                                                    ! isltyp
  read(lendian_in)                                                    ! vegfrac
  read(lendian_in)                                                    ! sno
  read(lendian_in)                                                    ! u10
  read(lendian_in)                                                    ! v10
  read(lendian_in)                                                    ! smc
  read(lendian_in)                                                    ! stc

  i=i+1 ; i_tsk=i                                                ! tsk
  read(lendian_in) n_position
  offset(i)=n_position ; length=im*jm ; igtype(i)=1 ; kdim(i)=1
  if(mype == 0) write(6,*)' tsk, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)

  close(lendian_in)

!          set up evenly distributed index range over all processors for all input fields


  num_loc_groups=num_nmm_fields/npe
  nextra=num_nmm_fields-num_loc_groups*npe
  kbegin(0)=1
  if(nextra > 0) then
     do k=1,nextra
        kbegin(k)=kbegin(k-1)+1+num_loc_groups
     end do
  end if
  do k=nextra+1,npe
     kbegin(k)=kbegin(k-1)+num_loc_groups
  end do
  do k=0,npe-1
     kend(k)=kbegin(k+1)-1
  end do
  if(mype == 0) then
     write(6,*)' kbegin=',kbegin
     write(6,*)' kend= ',kend
  end if
  num_j_groups=jm/npe
  jextra=jm-num_j_groups*npe
  jbegin(0)=1
  if(jextra > 0) then
     do j=1,jextra
        jbegin(j)=jbegin(j-1)+1+num_j_groups
     end do
  end if
  do j=jextra+1,npe
     jbegin(j)=jbegin(j-1)+num_j_groups
  end do
  do j=0,npe-1
     jend(j)=min(jbegin(j+1)-1,jm)
  end do
  if(mype == 0) then
     write(6,*)' jbegin=',jbegin
     write(6,*)' jend= ',jend
  end if
  
! Create all_loc from ges_*
  allocate(all_loc(lat1,lon1,num_nmm_fields))
  all_loc=zero_single
  kt=i_t-1
  kq=i_q-1
  ku=i_u-1
  kv=i_v-1
  do k=1,nsig
     kt=kt+1
     kq=kq+1
     ku=ku+1
     kv=kv+1
     do i=1,lon1
        do j=1,lat1
           all_loc(j,i,ku)=ges_u(j+1,i+1,k,it)
           all_loc(j,i,kv)=ges_v(j+1,i+1,k,it)
           all_loc(j,i,kq)=ges_q(j+1,i+1,k,it)
           all_loc(j,i,kt)=ges_tsen(j+1,i+1,k,it)   ! sensible temperature
        end do
     end do
  end do
  do i=1,lon1
     do j=1,lat1
        psfc_this=r10*ges_ps(j+1,i+1,it)   ! convert from mb to cb
        pd=psfc_this-pdtop_ll-pt_ll
        all_loc(j,i,i_pd)=r100*pd
     end do
  end do
!                    update pint by adding eta2(k)*pdinc
  if(update_pint) then
     kpint=i_pint-1
     do k=1,nsig+1
        kpint=kpint+1
        do i=1,lon1
           do j=1,lat1
              all_loc(j,i,kpint)=ges_pint(j+1,i+1,k,it) &
                          +eta2_ll(k)*(all_loc(j,i,i_pd)-ges_pd(j+1,i+1,it))   ! pint
           end do
        end do
     end do
  end if
  if(update_regsfc) then
     do i=1,lon1
        do j=1,lat1
           all_loc(j,i,i_sst)=dsfct(j+1,i+1,ntguessfc)
           all_loc(j,i,i_tsk)=dsfct(j+1,i+1,ntguessfc)
        end do
     end do
  end if
  
  allocate(tempa(itotsub,kbegin(mype):kend(mype)))
  call generic_sub2grid(all_loc,tempa,kbegin(mype),kend(mype),kbegin,kend,mype,num_nmm_fields)
  deallocate(all_loc)

  allocate(ibuf(im*jm,kbegin(mype):kend(mype)))

!   2.  create ibuf with records to be updated read in

!                                    read pint
  if(update_pint.and.kord(i_pint)/=1) then
     allocate(jbuf(im,lm+1,jbegin(mype):jend(mype)))
     this_offset=offset(i_pint)+(jbegin(mype)-1)*4*im*(lm+1)
     this_length=(jend(mype)-jbegin(mype)+1)*im*(lm+1)
     call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
     call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                        jbegin,jend,kbegin,kend,mype,npe,im,jm,lm+1,im,jm,i_pint,i_pint+lm)
     deallocate(jbuf)
  end if

!                                    read temps
  if(kord(i_t)/=1) then
     allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
     this_offset=offset(i_t)+(jbegin(mype)-1)*4*im*lm
     this_length=(jend(mype)-jbegin(mype)+1)*im*lm
     call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
     call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                        jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_t,i_t+lm-1)
     deallocate(jbuf)
  end if

!                                    read q
  if(kord(i_q)/=1) then
     allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
     this_offset=offset(i_q)+(jbegin(mype)-1)*4*im*lm
     this_length=(jend(mype)-jbegin(mype)+1)*im*lm
     call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
     call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                        jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_q,i_q+lm-1)
     deallocate(jbuf)
  end if

!                                    read u
  if(kord(i_u)/=1) then
     allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
     this_offset=offset(i_u)+(jbegin(mype)-1)*4*im*lm
     this_length=(jend(mype)-jbegin(mype)+1)*im*lm
     call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
     call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                        jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_u,i_u+lm-1)
     deallocate(jbuf)
  end if

!                                    read v
  if(kord(i_v)/=1) then
     allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
     this_offset=offset(i_v)+(jbegin(mype)-1)*4*im*lm
     this_length=(jend(mype)-jbegin(mype)+1)*im*lm
     call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
     call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                        jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_v,i_v+lm-1)
     deallocate(jbuf)
  end if

!---------------------- read surface files last
  do k=kbegin(mype),kend(mype)
     if(kdim(k)==1.or.kord(k)==1) then
        call mpi_file_read_at(mfcst,offset(k),ibuf(1,k),length(k),mpi_integer4,status,ierror)
     end if
  end do

!   5.  tempa --> updated ibuf --> jbuf --> write out

  allocate(tempb(itotsub,kbegin(mype):kend(mype)))
  allocate(temp1(im*jm))
  do ifld=kbegin(mype),kend(mype)
     if((ifld==i_sst.or.ifld==i_tsk).and..not.update_regsfc) cycle
     call move_ibuf_hg(ibuf(1,ifld),temp1,im,jm,im,jm)
     if(filled_grid) call fill_nmm_grid2(temp1,im,jm,tempb(1,ifld),igtype(ifld),2)
     if(half_grid)   call half_nmm_grid2(temp1,im,jm,tempb(1,ifld),igtype(ifld),2)
     if(ifld==i_sst.or.ifld==i_tsk) then
        do i=1,iglobal
           if(tempb(i,ifld) < r225) then
              tempa(i,ifld)=zero_single
           else
              tempa(i,ifld)=tempa(i,ifld)-tempb(i,ifld)
           end if
        end do
     else
        do i=1,iglobal
           tempa(i,ifld)=tempa(i,ifld)-tempb(i,ifld)
        end do
     end if
     if(filled_grid) call unfill_nmm_grid2(tempa(1,ifld),im,jm,temp1,igtype(ifld),2)
     if(half_grid)   call unhalf_nmm_grid2(tempa(1,ifld),im,jm,temp1,igtype(ifld),2)
     call move_hg_ibuf(temp1,ibuf(1,ifld),im,jm,im,jm)
  end do

!           finally write ibuf back out ( ibuf --> jbuf -->  mpi_file_write )

!                                    write pint
  if(update_pint.and.kord(i_pint)/=1) then
     allocate(jbuf(im,lm+1,jbegin(mype):jend(mype)))
     this_length=(jend(mype)-jbegin(mype)+1)*im*(lm+1)

     call transfer_ibuf2jbuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                        jbegin,jend,kbegin,kend,mype,npe,im,jm,lm+1,im,jm,i_pint,i_pint+lm)
     this_offset=offset(i_pint)+(jbegin(mype)-1)*4*im*(lm+1)
     call mpi_file_write_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
     deallocate(jbuf)
  end if

!                                    write temps
  if(kord(i_t)/=1) then
     allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
     this_length=(jend(mype)-jbegin(mype)+1)*im*lm
 
     call transfer_ibuf2jbuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                        jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_t,i_t+lm-1)
     this_offset=offset(i_t)+(jbegin(mype)-1)*4*im*lm
     call mpi_file_write_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
     deallocate(jbuf)
  end if

!                                    write q
  if(kord(i_q)/=1) then
     allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
     call transfer_ibuf2jbuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                        jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_q,i_q+lm-1)
     this_offset=offset(i_q)+(jbegin(mype)-1)*4*im*lm
     call mpi_file_write_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
     deallocate(jbuf)
  end if

!                                    write u
  if(kord(i_u)/=1) then
     allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
     call transfer_ibuf2jbuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                        jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_u,i_u+lm-1)
     this_offset=offset(i_u)+(jbegin(mype)-1)*4*im*lm
     call mpi_file_write_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
     deallocate(jbuf)
  end if

!                                    write v
  if(kord(i_v)/=1) then
     allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
     call transfer_ibuf2jbuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                        jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_v,i_v+lm-1)
     this_offset=offset(i_v)+(jbegin(mype)-1)*4*im*lm
     call mpi_file_write_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
     deallocate(jbuf)
  end if

!---------------------- write surface files last
  do k=kbegin(mype),kend(mype)
     if(kdim(k)==1.or.kord(k)==1) then
        call mpi_file_write_at(mfcst,offset(k),ibuf(1,k),length(k),mpi_integer4,status,ierror)
     end if
  end do

  deallocate(ibuf)
  deallocate(offset)
  deallocate(igtype)
  deallocate(kdim)
  deallocate(kord)
  deallocate(length)
  deallocate(tempa)
  deallocate(tempb)
  deallocate(temp1)

  call mpi_file_close(mfcst,ierror)
  
end subroutine wrwrfnmma_binary

subroutine wrnemsnmma_binary(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    wrwrfnmma              write out wrf NMM restart file
!   prgmmr: parrish          org: np22                date: 2004-06-23
!
! abstract:  read wrf NMM guess restart interface file, add analysis
!            increment, and write out wrf NMM analysis restart 
!            interface file.
!
! program history log:
!   2004-06-23  parrish, document
!   2004-08-03  treadon - add only to module use, add intent in/out
!   2004-11-22  parrish - rewrite for mpi-io
!   2004-12-15  treadon - write analysis to file "wrf_inout"
!   2005-07-06  parrish - update and write out pint if update_pint=.true.
!   2006-04-06  middlecoff - changed nfcst from 11 to lendian_in
!   2006-07-28  derber - include sensible temperature
!   2006-07-31  kleist - change to use ges_ps instead of lnps
!   2007-04-12  parrish - add modifications to allow any combination of ikj or ijk
!                          grid ordering for input 3D fields
!   2007-05-02  parrish - fix bug to prevent out of memory reference when pint missing
!   2008-04-01  safford - rm unused uses
!   2008-12-05  todling - adjustment for dsfct time dimension addition
!   2010-01-18  parrish - add update of 10m wind, 2m pot temp, 2m specific humidity
!   2010-03-12  parrish - add write of ozone to 3d field labeled "o3mr"  (might be changed to "o3")
!   2010-03-15  parrish - add flag regional_ozone to turn on ozone in regional analysis
!
!   input argument list:
!     mype     - pe number
!
!   output argument list:
!     no output arguments
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use regional_io, only: update_pint
  use guess_grids, only: ges_ps,ges_pd,ges_u,ges_v,ges_q,&
        ntguessfc,ntguessig,ges_tsen,dsfct,isli,geop_hgtl,ges_prsl,ges_oz
  use gridmod, only: pt_ll,update_regsfc,pdtop_ll,nsig,lat2,lon2,eta2_ll,nmmb_verttype,&
        use_gfs_ozone,regional_ozone
  use constants, only: zero,half,one,two,rd_over_cp
  use gsi_nemsio_mod, only: gsi_nemsio_open,gsi_nemsio_close,gsi_nemsio_read,gsi_nemsio_write
  use gsi_nemsio_mod, only: gsi_nemsio_update
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype,mpi_integer4,mpi_min,mpi_max,mpi_sum

  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

! Declare local constants
  real(r_kind),parameter:: r10=10.0_r_kind
  real(r_kind),parameter:: r100=100.0_r_kind

! Declare local variables

  character(255) wrfanl
  logical add_saved

  integer(i_kind) i,it,j,k,kr,mype_input
  integer(i_kind) near_sfc,kp
  real(r_kind) pd,psfc_this,pd_to_ps,wmag
  real(r_kind),dimension(lat2,lon2):: work_sub,pd_new,delu10,delv10,u10this,v10this,fact10_local
  real(r_kind),dimension(lat2,lon2):: delt2,delq2,t2this,q2this,fact2t_local,fact2q_local
  real(r_kind),dimension(lat2,lon2,6):: delu,delv,delt,delq,pott
  real(r_kind) hmin,hmax,hmin0,hmax0,ten,wgt1,wgt2
  logical use_fact10,use_fact2
  logical good_u10,good_v10,good_tshltr,good_qshltr,good_o3mr

  use_fact10=.true.
  use_fact2=.false.

!   decide how many near surface layers to save for interpolation/extrapolation to get u10,v10,t2,q2

  near_sfc=1
  do k=1,6
     hmin=minval(geop_hgtl(:,:,k,ntguessig))
     hmax=maxval(geop_hgtl(:,:,k,ntguessig))
     call mpi_allreduce(hmin,hmin0,1,mpi_rtype,mpi_min,mpi_comm_world,ierror)
     call mpi_allreduce(hmax,hmax0,1,mpi_rtype,mpi_max,mpi_comm_world,ierror)
     if(mype == 0) write(6,*)' k,min,max geop_hgtl=',k,hmin0,hmax0
     if(hmin0 < 40._r_kind) near_sfc=k
     hmin=minval(ges_prsl(:,:,k,ntguessig))
     hmax=maxval(ges_prsl(:,:,k,ntguessig))
     call mpi_allreduce(hmin,hmin0,1,mpi_rtype,mpi_min,mpi_comm_world,ierror)
     call mpi_allreduce(hmax,hmax0,1,mpi_rtype,mpi_max,mpi_comm_world,ierror)
     if(mype == 0) write(6,*)' k,min,max ges_prsl=',k,hmin0,hmax0
  end do
  near_sfc=max(near_sfc,2)
  if(mype == 0) write(6,*)' in wrnemsnmma_binary near_sfc=',near_sfc

                  
                     
!     get conversion factor for pd to psfc

  if(nmmb_verttype=='OLD') then
     pd_to_ps=pdtop_ll+pt_ll
  else
     pd_to_ps=pt_ll
  end if

  it=ntguessig
  mype_input=0
  add_saved=.true.

  if(mype==mype_input) wrfanl = 'wrf_inout'

!   update date info so start time is analysis time, and forecast time = 0
  call gsi_nemsio_update(wrfanl,'WRNEMSNMMA_BINARY:  problem with update of wrfanl',mype,mype_input)

!   open output file for read-write so we can update fields.
  call gsi_nemsio_open(wrfanl,'rdwr','WRNEMSNMMA_BINARY:  problem with wrfanl',mype,mype_input)

  do kr=1,nsig

     k=nsig+1-kr
                                   !   u

     call gsi_nemsio_read('ugrd','mid layer','V',kr,work_sub(:,:),mype,mype_input)
     do i=1,lon2
        do j=1,lat2
           work_sub(j,i)=ges_u(j,i,k,it)-work_sub(j,i)
        end do
     end do
     if(k <= near_sfc) then
        do i=1,lon2
           do j=1,lat2
              delu(j,i,k)=work_sub(j,i)
           end do
        end do
     end if
     call gsi_nemsio_write('ugrd','mid layer','V',kr,work_sub(:,:),mype,mype_input,add_saved)

                                   !   v

     call gsi_nemsio_read('vgrd','mid layer','V',kr,work_sub(:,:),mype,mype_input)
     do i=1,lon2
        do j=1,lat2
           work_sub(j,i)=ges_v(j,i,k,it)-work_sub(j,i)
        end do
     end do
     if(k <= near_sfc) then
        do i=1,lon2
           do j=1,lat2
              delv(j,i,k)=work_sub(j,i)
           end do
        end do
     end if
     call gsi_nemsio_write('vgrd','mid layer','V',kr,work_sub(:,:),mype,mype_input,add_saved)

                                   !   q

     call gsi_nemsio_read('spfh','mid layer','H',kr,work_sub(:,:),mype,mype_input)
     do i=1,lon2
        do j=1,lat2
           work_sub(j,i)=ges_q(j,i,k,it)-work_sub(j,i)
        end do
     end do
     if(k <= near_sfc) then
        do i=1,lon2
           do j=1,lat2
              delq(j,i,k)=work_sub(j,i)
           end do
        end do
     end if
     call gsi_nemsio_write('spfh','mid layer','H',kr,work_sub(:,:),mype,mype_input,add_saved)

                                   !   tsen

     call gsi_nemsio_read('tmp','mid layer','H',kr,work_sub(:,:),mype,mype_input)
     do i=1,lon2
        do j=1,lat2
           work_sub(j,i)=ges_tsen(j,i,k,it)-work_sub(j,i)
        end do
     end do
     if(k <= near_sfc) then
        do i=1,lon2
           do j=1,lat2
              delt(j,i,k)=work_sub(j,i)*(r100/ges_prsl(j,i,k,it))**rd_over_cp  ! convert to pot temp
              pott(j,i,k)=ges_tsen(j,i,k,it)*(r100/ges_prsl(j,i,k,it))**rd_over_cp
           end do
        end do
     end if
     call gsi_nemsio_write('tmp','mid layer','H',kr,work_sub(:,:),mype,mype_input,add_saved)

                                   !   ozone

     if(regional_ozone) then
        good_o3mr=.false.
        call gsi_nemsio_read('o3mr','mid layer','H',kr,work_sub(:,:),mype,mype_input,good_o3mr)
        if(good_o3mr) then
           if(use_gfs_ozone) then
!                                  gfs ozone interpolated directly to analysis grid and nmmb guess
!                                   not used, so set work_sub=zero
              work_sub=zero
           end if
           do i=1,lon2
              do j=1,lat2
                 work_sub(j,i)=ges_oz(j,i,k,it)-work_sub(j,i)
              end do
           end do
           call gsi_nemsio_write('o3mr','mid layer','H',kr,work_sub(:,:),mype,mype_input, &
                                 add_saved.and..not.use_gfs_ozone)
        else
           if(mype==0) write(6,*)' O3MR FIELD NOT YET AVAILABLE IN NMMB, OZONE DATA USED BUT NOT UPDATED'
        end if
     end if

  end do

                             ! pd
  do i=1,lon2
     do j=1,lat2
        psfc_this=r10*ges_ps(j,i,it)   ! convert from mb to cb
        pd=psfc_this-pd_to_ps
        pd_new(j,i)=r100*pd
     end do
  end do

  call gsi_nemsio_read('dpres','hybrid sig lev','H',1,work_sub(:,:),mype,mype_input)
  do i=1,lon2
     do j=1,lat2
        work_sub(j,i)=pd_new(j,i)-work_sub(j,i)
     end do
  end do
  call gsi_nemsio_write('dpres','hybrid sig lev','H',1,work_sub(:,:),mype,mype_input,add_saved)


!                    update pint by adding eta2(k)*pdinc
  if(update_pint) then
     do kr=1,nsig+1
        k=nsig+2-kr
        call gsi_nemsio_read('pres','layer','H',kr,work_sub(:,:),mype,mype_input)

        do i=1,lon2
           do j=1,lat2
              work_sub(j,i)=eta2_ll(k)*(pd_new(j,i)-ges_pd(j,i,it))   ! pint analysis increment
           end do
        end do
        call gsi_nemsio_write('pres','layer','H',kr,work_sub(:,:),mype,mype_input,add_saved)
     end do
  end if

  if(update_regsfc) then
!              land points first
     call gsi_nemsio_read('tg'   ,'sfc','H',1,work_sub(:,:),mype,mype_input)
     do i=1,lon2
        do j=1,lat2
           if(isli(j,i,it)/=0) then
!               land points--
              work_sub(j,i)=dsfct(j,i,ntguessfc)
           else
!               water points
              work_sub(j,i)=zero
           end if
        end do
     end do
     call gsi_nemsio_write('tg','sfc','H',1,work_sub(:,:),mype,mype_input,add_saved)
!          now water points
     call gsi_nemsio_read('tsea' ,'sfc','H',1,work_sub(:,:),mype,mype_input)
     do i=1,lon2
        do j=1,lat2
           if(isli(j,i,it)/=0) then
!               land points--
              work_sub(j,i)=zero
           else
!               water points
              work_sub(j,i)=dsfct(j,i,ntguessfc)
           end if
        end do
     end do
     call gsi_nemsio_write('tsea','sfc','H',1,work_sub(:,:),mype,mype_input,add_saved)
  end if

!   fact10 method follows:

  good_u10=.false.
  good_v10=.false.
  call gsi_nemsio_read ('u10' ,'10 m above gnd','H',1,u10this(:,:),mype,mype_input,good_u10)
  call gsi_nemsio_read ('v10' ,'10 m above gnd','H',1,v10this(:,:),mype,mype_input,good_v10)
  if(good_u10.and.good_v10) then
     if(use_fact10) then
!          recompute fact10 (store as fact10_local)  (this code lifted from read_wrf_nmm_guess.F90)
        do i=1,lon2
           do j=1,lat2
              fact10_local(j,i)=one    !  later fix this by using correct w10/w(1)
              wmag=sqrt(ges_u(j,i,1,it)**2+ges_v(j,i,1,it)**2)
              if(wmag > zero)fact10_local(j,i)=sqrt(u10this(j,i)**2+v10this(j,i)**2)/wmag
              fact10_local(j,i)=min(max(fact10_local(j,i),half),0.95_r_kind)
              delu10(j,i)=fact10_local(j,i)*delu(j,i,1)
              delv10(j,i)=fact10_local(j,i)*delv(j,i,1)
           end do
        end do

     else

!    vertical interpolation/extrapolation follows:

        ten=10._r_kind
        do i=1,lon2
           do j=1,lat2
              if(ten <  geop_hgtl(j,i,1,it)) then
                 delu10(j,i)=delu(j,i,1)
                 delv10(j,i)=delv(j,i,1)
              else
                 do k=1,near_sfc-1
                    kp=k+1
                    if(ten >= geop_hgtl(j,i,k,it).and.ten <  geop_hgtl(j,i,kp,it)) then
                       wgt1=(geop_hgtl(j,i,kp,it)-ten)/(geop_hgtl(j,i,kp,it)-geop_hgtl(j,i,k,it))
                       wgt2=one-wgt1
                       delu10(j,i)=wgt1*delu(j,i,k)+wgt2*delu(j,i,kp)
                       delv10(j,i)=wgt1*delv(j,i,k)+wgt2*delv(j,i,kp)
                       exit
                    end if
                 end do
              end if
           end do
        end do

     end if


!         update 10m wind 
!                     (read to work_sub, but only so u10 is saved internally in module gsi_nemsio_mod)
     call gsi_nemsio_read ('u10' ,'10 m above gnd','H',1,work_sub(:,:),mype,mype_input)
!                previously computed 10m u increment added to guess u10 here:
     call gsi_nemsio_write('u10' ,'10 m above gnd','H',1,delu10(:,:),mype,mype_input,add_saved)
!             repeat for 10m v component
     call gsi_nemsio_read ('v10' ,'10 m above gnd','H',1,work_sub(:,:),mype,mype_input)
     call gsi_nemsio_write('v10' ,'10 m above gnd','H',1,delv10(:,:),mype,mype_input,add_saved)

  end if

!         update 2m potential temp and 2m specific humidity

!   fact2 method follows:

  good_tshltr=.false.
  good_qshltr=.false.
  call gsi_nemsio_read ('tshltr' ,'sfc','H',1,t2this(:,:),mype,mype_input,good_tshltr)
  call gsi_nemsio_read ('qshltr' ,'sfc','H',1,q2this(:,:),mype,mype_input,good_qshltr)
  if(good_tshltr.and.good_qshltr) then
     if(use_fact2) then
!       compute fact2t, fact2q
        call gsi_nemsio_read ('tshltr' ,'sfc','H',1,t2this(:,:),mype,mype_input)
        call gsi_nemsio_read ('qshltr' ,'sfc','H',1,q2this(:,:),mype,mype_input)
        do i=1,lon2
           do j=1,lat2
              fact2t_local(j,i)=max(half,min(t2this(j,i)/pott(j,i,1),two))
              fact2q_local(j,i)=max(half,min(q2this(j,i)/ges_q(j,i,1,it),two))
              delt2(j,i)=fact2t_local(j,i)*delt(j,i,1)
              delq2(j,i)=fact2q_local(j,i)*delq(j,i,1)
           end do
        end do

     else

!    vertical interpolation/extrapolation follows:

        do i=1,lon2
           do j=1,lat2
              if(two <  geop_hgtl(j,i,1,it)) then
                 delt2(j,i)=delt(j,i,1)
                 delq2(j,i)=delq(j,i,1)
              else
                 do k=1,near_sfc-1
                    kp=k+1
                    if(two >= geop_hgtl(j,i,k,it).and.two <  geop_hgtl(j,i,kp,it)) then
                       wgt1=(geop_hgtl(j,i,kp,it)-two)/(geop_hgtl(j,i,kp,it)-geop_hgtl(j,i,k,it))
                       wgt2=one-wgt1
                       delt2(j,i)=wgt1*delt(j,i,k)+wgt2*delt(j,i,kp)
                       delq2(j,i)=wgt1*delq(j,i,k)+wgt2*delq(j,i,kp)
                       exit
                    end if
                 end do
              end if
           end do
        end do

     end if


!         update 2m t and q
!                     (read to work_sub, but only so tshltr is saved internally in module gsi_nemsio_mod)
     call gsi_nemsio_read ('tshltr' ,'sfc','H',1,work_sub(:,:),mype,mype_input)
!                previously computed 2m t increment added to guess tshltr here:
     call gsi_nemsio_write('tshltr' ,'sfc','H',1,delt2(:,:),mype,mype_input,add_saved)
!             repeat for 2m q
     call gsi_nemsio_read ('qshltr' ,'sfc','H',1,work_sub(:,:),mype,mype_input)
     call gsi_nemsio_write('qshltr' ,'sfc','H',1,delq2(:,:),mype,mype_input,add_saved)

  end if

  call gsi_nemsio_close(wrfanl,'WRNEMSNMMA_BINARY',mype,mype_input)
  
end subroutine wrnemsnmma_binary

subroutine wrwrfnmma_netcdf(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    wrwrfnmma              write out wrf NMM restart file
!   prgmmr: parrish          org: np22                date: 2004-06-23
!
! abstract:  read wrf NMM guess restart interface file, add analysis
!            increment, and write out wrf NMM analysis restart 
!            interface file.
!
! program history log:
!   2004-06-23  parrish, document
!   2004-08-03  treadon - add only to module use, add intent in/out
!   2005-07-06  parrish - update and write out pint if update_pint=.true.
!   2006-04-06  middlecoff - changed iog  from 11 to lendian_in
!                            changed ioan from 51 to lendian_out
!   2006-07-28  derber - include sensible temperature
!   2006-07-31  kleist - change to use ges_ps instead of lnps
!   2008-04-01  safford - rm unused uses
!   2008-12-05  todling - adjustment for dsfct time dimension addition
!   2010-04-01  treadon - move strip_single to gridmod
!
!   input argument list:
!     mype     - pe number
!
!   output argument list:
!     no output arguments
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_single,i_kind
  use regional_io, only: update_pint
  use guess_grids, only: ges_ps,ges_pint,ges_pd,ges_u,ges_v,ges_q,&
       ntguessfc,ntguessig,ifilesig,dsfct,ges_tsen
  use mpimod, only: mpi_comm_world,ierror,mpi_real4
  use gridmod, only: iglobal,itotsub,pt_ll,update_regsfc,&
       half_grid,filled_grid,pdtop_ll,nlat_regional,nlon_regional,&
       nsig,lat1,lon1,ijn,displs_g,eta2_ll,strip_single
  use constants, only: zero_single
  use gsi_io, only: lendian_in, lendian_out
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

! Declare local constants
  real(r_kind),parameter:: r10=10.0_r_kind
  real(r_kind),parameter:: r100=100.0_r_kind
  real(r_kind),parameter:: r225=225.0_r_kind

! Declare local variables
  integer(i_kind) im,jm,lm
  real(r_single),allocatable::temp1(:),tempa(:),tempb(:)
  real(r_single),allocatable::all_loc(:,:,:)
  real(r_single),allocatable::strp(:)
  character(6) filename
  integer(i_kind) i,j,k,kpint,kt,kq,ku,kv,it,i_pd,i_pint,i_t,i_q,i_u,i_v
  integer(i_kind) i_sst,i_skt
  integer(i_kind) igtypeh,igtypev,num_nmm_fields,num_all_fields,num_all_pad
  integer(i_kind) regional_time0(6),nlon_regional0,nlat_regional0,nsig0
  real(r_kind) pd,psfc_this
  real(r_single) dlmd0,dphd0,pt0,pdtop0
  real(r_single) deta10(nsig),aeta10(nsig),eta10(nsig+1),deta20(nsig),&
       aeta20(nsig),eta20(nsig+1)
  real(r_single) glon0(nlon_regional,nlat_regional),glat0(nlon_regional,nlat_regional)
  real(r_single) dx0_nmm(nlon_regional,nlat_regional),dy0_nmm(nlon_regional,nlat_regional)

  im=nlon_regional
  jm=nlat_regional
  lm=nsig

  num_nmm_fields=3+4*lm
  if(update_pint) num_nmm_fields=num_nmm_fields+lm+1  ! contribution from PINT
  num_all_fields=num_nmm_fields
  num_all_pad=num_all_fields
  allocate(all_loc(lat1+2,lon1+2,num_all_pad))
  allocate(strp(lat1*lon1))

  i_pd=1
  if(update_pint) then
     i_pint=2
     i_t=i_pint+lm+1
  else
     i_t=2
  end if
  i_q=i_t+lm
  i_u=i_q+lm
  i_v=i_u+lm
  i_sst=i_v+lm
  i_skt=i_sst+1
  igtypeh=1
  igtypev=2
  
  allocate(temp1(im*jm))

  if(mype == 0) write(6,*)' at 2 in wrwrfnmma'


  if(mype == 0) then
     write(filename,'("sigf",i2.2)')ifilesig(ntguessig)
     open (lendian_in,file=filename,form='unformatted')
     open (lendian_out,file='siganl',form='unformatted')
     rewind lendian_in ; rewind lendian_out
  end if

! Convert analysis variables to NMM variables
  it=ntguessig

! Create all_loc from ges_*
  if(mype == 0) write(6,*)' at 3 in wrwrfnmma'
  all_loc=zero_single
  kt=i_t-1
  kq=i_q-1
  ku=i_u-1
  kv=i_v-1
  do k=1,nsig
     kt=kt+1
     kq=kq+1
     ku=ku+1
     kv=kv+1
     do i=1,lon1+2
        do j=1,lat1+2
           all_loc(j,i,ku)=ges_u(j,i,k,it)
           if(k == 1.and.abs(all_loc(j,i,ku)) > 1.e15_r_single) &
                write(6,*)' at 3.01 in wrwrfnmma, j,i,ku,all_loc(j,i,ku)=',j,i,ku,all_loc(j,i,ku)
           all_loc(j,i,kv)=ges_v(j,i,k,it)
           all_loc(j,i,kq)=ges_q(j,i,k,it)
           all_loc(j,i,kt)=ges_tsen(j,i,k,it)   ! sensible temperature
        end do
     end do
  end do
  do i=1,lon1+2
     do j=1,lat1+2
        psfc_this=r10*ges_ps(j,i,it)   ! convert from mb to cb
        pd=psfc_this-pdtop_ll-pt_ll
        all_loc(j,i,i_pd)=r100*pd
     end do
  end do
!                    update pint by adding eta2(k)*pdinc
  if(update_pint) then
     kpint=i_pint-1
     do k=1,nsig+1
        kpint=kpint+1
        do i=1,lon1+2
           do j=1,lat1+2
              all_loc(j,i,kpint)=ges_pint(j,i,k,it) &
                          +eta2_ll(k)*(all_loc(j,i,i_pd)-ges_pd(j,i,it))   ! pint
           end do
        end do
     end do
  end if
  
  if(mype == 0) then
     read(lendian_in) regional_time0,nlon_regional0,nlat_regional0,nsig0,dlmd0,dphd0,pt0,pdtop0
     write(lendian_out) regional_time0,nlon_regional0,nlat_regional0,nsig0,dlmd0,dphd0,pt0,pdtop0
     read(lendian_in) deta10
     write(lendian_out) deta10
     read(lendian_in) aeta10
     write(lendian_out) aeta10
     read(lendian_in) eta10
     write(lendian_out) eta10
     read(lendian_in) deta20
     write(lendian_out) deta20
     read(lendian_in) aeta20
     write(lendian_out) aeta20
     read(lendian_in) eta20
     write(lendian_out) eta20
     read(lendian_in) glat0,dx0_nmm
     write(lendian_out) glat0,dx0_nmm
     read(lendian_in) glon0,dy0_nmm
     write(lendian_out) glon0,dy0_nmm
  end if
  
! Update pd
  if(mype == 0) write(6,*)' at 6 in wrwrfnmma'

  allocate(tempa(itotsub),tempb(itotsub))
  if(mype == 0) read(lendian_in)temp1
  if(mype == 0) write(6,*)' at 6.1 in wrwrfnmma,max,min(temp1)=',maxval(temp1),minval(temp1)
  call strip_single(all_loc(1,1,i_pd),strp,1)
  call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
       tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
  if(mype == 0) then
     if(mype == 0) write(6,*)' at 6.2 in wrwrfnmma,max,min(tempa)=',maxval(tempa),minval(tempa)
     if(filled_grid) call fill_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
     if(half_grid)   call half_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
     if(mype == 0) write(6,*)' at 6.3 in wrwrfnmma,max,min(tempb)=',maxval(tempb),minval(tempb)
     do i=1,iglobal
        tempa(i)=tempa(i)-tempb(i)
     end do
     if(mype == 0) write(6,*)' at 6.4 in wrwrfnmma,max,min(tempa)=',maxval(tempa),minval(tempa)
     if(filled_grid) call unfill_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
     if(mype == 0) write(6,*)' at 6.5 in wrwrfnmma'
     if(half_grid)   call unhalf_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
     if(mype == 0) write(6,*)' at 6.6 in wrwrfnmma,max,min(temp1)=',maxval(temp1),minval(temp1)
     write(lendian_out)temp1
  end if

!  FIS read/write
  if(mype == 0) then
     read(lendian_in)temp1
     write(lendian_out)temp1
  end if

! Update pint
  if(update_pint) then
     kpint=i_pint-1
     do k=1,nsig+1
        kpint=kpint+1
        if(mype == 0) read(lendian_in)temp1
        call strip_single(all_loc(1,1,kpint),strp,1)
        call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
             tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
        if(mype == 0) then
           if(filled_grid) call fill_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
           if(half_grid)   call half_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
           do i=1,iglobal
              tempa(i)=tempa(i)-tempb(i)
           end do
           if(filled_grid) call unfill_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
           if(half_grid)   call unhalf_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
           write(lendian_out)temp1
        end if
     end do
  endif

! Update t
  kt=i_t-1
  do k=1,nsig
     kt=kt+1
     if(mype == 0) read(lendian_in)temp1
     call strip_single(all_loc(1,1,kt),strp,1)
     call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
          tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
     if(mype == 0) then
        if(filled_grid) call fill_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
        if(half_grid)   call half_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
        do i=1,iglobal
           tempa(i)=tempa(i)-tempb(i)
        end do
        if(filled_grid) call unfill_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
        if(half_grid)   call unhalf_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
        write(lendian_out)temp1
     end if
  end do
  if(mype == 0) write(6,*)' at 7 in wrwrfnmma'


! Update q
  kq=i_q-1
  do k=1,nsig
     kq=kq+1
     if(mype == 0) read(lendian_in)temp1
     call strip_single(all_loc(1,1,kq),strp,1)
     call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
          tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
     if(mype == 0) then
        if(filled_grid) call fill_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
        if(half_grid)   call half_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
        do i=1,iglobal
           tempa(i)=tempa(i)-tempb(i)
        end do
        if(filled_grid) call unfill_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
        if(half_grid)   call unhalf_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
        write(lendian_out)temp1
     end if
  end do

! Update u
  ku=i_u-1
  do k=1,nsig
     ku=ku+1
     if(mype == 0) read(lendian_in)temp1
     call strip_single(all_loc(1,1,ku),strp,1)
     call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
          tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
     if(mype == 0) write(6,*)' at 7.2 in wrwrfnmma,k,max,min(tempa)=',k,maxval(tempa),minval(tempa)
     if(mype == 0) then
        if(filled_grid) call fill_nmm_grid2(temp1,im,jm,tempb,igtypev,2)
        if(half_grid)   call half_nmm_grid2(temp1,im,jm,tempb,igtypev,2)
        if(mype == 0) write(6,*)' at 7.21 in wrwrfnmma,k,max,min(temp1)=',&
             k,maxval(temp1),minval(temp1)
        if(mype == 0) write(6,*)' at 7.22 in wrwrfnmma,k,max,min(tempb)=',&
             k,maxval(tempb),minval(tempb)
        do i=1,iglobal
           tempa(i)=tempa(i)-tempb(i)
        end do
        if(mype == 0) write(6,*)' at 7.3 in wrwrfnmma,k,max,min(tempa)=',k,maxval(tempa),minval(tempa)
        if(filled_grid) call unfill_nmm_grid2(tempa,im,jm,temp1,igtypev,2)
        if(half_grid)   call unhalf_nmm_grid2(tempa,im,jm,temp1,igtypev,2)
        if(mype == 0) write(6,*)' at 7.4 in wrwrfnmma,k,max,min(temp1)=',k,maxval(temp1),minval(temp1)
        write(lendian_out)temp1
     end if
  end do
  if(mype == 0) write(6,*)' at 8 in wrwrfnmma'

! Update v
  kv=i_v-1
  do k=1,nsig
     kv=kv+1
     if(mype == 0) read(lendian_in)temp1
     call strip_single(all_loc(1,1,kv),strp,1)
     call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
          tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
     if(mype == 0) then
        if(filled_grid) call fill_nmm_grid2(temp1,im,jm,tempb,igtypev,2)
        if(half_grid)   call half_nmm_grid2(temp1,im,jm,tempb,igtypev,2)
        do i=1,iglobal
           tempa(i)=tempa(i)-tempb(i)
        end do
        if(filled_grid) call unfill_nmm_grid2(tempa,im,jm,temp1,igtypev,2)
        if(half_grid)   call unhalf_nmm_grid2(tempa,im,jm,temp1,igtypev,2)
        write(lendian_out)temp1
     end if
  end do

! Load updated skin temperature array if writing out to analysis file
  if (update_regsfc) then
     do i=1,lon1+2
        do j=1,lat1+2
           all_loc(j,i,i_sst)=dsfct(j,i,ntguessfc)
           all_loc(j,i,i_skt)=dsfct(j,i,ntguessfc)
        end do
     end do
  end if

  if(mype == 0) then
! SM
     read(lendian_in)temp1
     write(lendian_out)temp1
! SICE
     read(lendian_in)temp1
     write(lendian_out)temp1
  end if

! SST
  if(update_regsfc) then
     if(mype == 0) read(lendian_in)temp1
     if (mype==0)write(6,*)' at 9.1 in wrwrfnmma,max,min(temp1)=',maxval(temp1),minval(temp1)
     call strip_single(all_loc(1,1,i_sst),strp,1)
     call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
          tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
     if(mype == 0) then
        if(mype == 0) write(6,*)' at 9.2 in wrwrfnmma,max,min(tempa)=',maxval(tempa),minval(tempa)
        if(filled_grid) call fill_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
        if(half_grid)   call half_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
        if(mype == 0) write(6,*)' at 9.3 in wrwrfnmma,max,min(tempb)=',maxval(tempb),minval(tempb)
        do i=1,iglobal
           if(tempb(i) < r225) then
              tempa(i)=zero_single
           else
              tempa(i)=tempa(i)-tempb(i)
           end if
        end do
        if(mype == 0) write(6,*)' at 9.4 in wrwrfnmma,max,min(tempa)=',maxval(tempa),minval(tempa)
        if(filled_grid) call unfill_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
        if(mype == 0) write(6,*)' at 9.5 in wrwrfnmma'
        if(half_grid)   call unhalf_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
        if(mype == 0) write(6,*)' at 9.6 in wrwrfnmma,max,min(temp1)=',maxval(temp1),minval(temp1)
        write(lendian_out)temp1
     end if     !endif mype==0
  else
     if(mype==0) then
        read(lendian_in)temp1
        write(lendian_out)temp1
     end if
  end if   !end if check updatesfc
  
! REST OF FIELDS
  if (mype == 0) then
     do k=4,11
        read(lendian_in)temp1
        write(lendian_out)temp1
     end do
  end if
  
! Update SKIN TEMP
  if(update_regsfc) then
     if(mype == 0) read(lendian_in)temp1
     if (mype==0)write(6,*)' at 10.0 in wrwrfnmma,max,min(temp1)=',maxval(temp1),minval(temp1)
     call strip_single(all_loc(1,1,i_skt),strp,1)
     call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
          tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
     if (mype==0)write(6,*)' at 10.1'
     if(mype == 0) then
        if(filled_grid) call fill_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
        if(half_grid)   call half_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
        do i=1,iglobal
           if(tempb(i) < r225) then
              tempa(i)=zero_single
           else 
              tempa(i)=tempa(i)-tempb(i)
           end if
        end do
        if(filled_grid) call unfill_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
        if(half_grid)   call unhalf_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
        write(lendian_out)temp1
     end if
  else
     if (mype == 0) then
        read(lendian_in)temp1
        write(lendian_out)temp1
     end if
  end if

  if (mype==0) then
     close(lendian_in)
     close(lendian_out)
  endif
  
  deallocate(all_loc)
  deallocate(strp)
  deallocate(temp1)
  deallocate(tempa)
  deallocate(tempb)
  
end subroutine wrwrfnmma_netcdf
#else /* Start no WRF-library block */
subroutine  wrwrfnmma_binary(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    wrwrfnmma              write out wrf NMM restart file
!   prgmmr: parrish          org: np22                date: 2004-06-23
!
! abstract:  dummy call to read wrf NMM guess restart interface file, add 
!            analysis increment, and write out wrf NMM analysis restart
!            interface file.
!
! program history log
!   2005-02-25 todling - add dummy subroutine to skip over wrf code 
!   2005-03-14 treadon - add write statement to note entry into dummy routine
!
!   input argument list:
!     mype     - pe number
!
!   output argument list:
!     no output arguments
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind
  implicit none

  integer(i_kind),intent(in   ) :: mype

  if (mype==0) write(6,*)'WRWRFNMMA_BINARY:  enter dummy call, do nothing'
end subroutine  wrwrfnmma_binary

subroutine wrnemsnmma_binary(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    wrnemsnmma_binary      write out wrf NMM restart file
!   prgmmr: parrish          org: np22                date: 2004-06-23
!
! abstract:  dummy call to read wrf NMM guess restart interface file, add analysis
!            increment, and write out wrf NMM analysis restart
!            interface file.
!
! program history log:
!   2009-08-14  lueken - added dummy subroutine to skip over wrf code
!
!   input argument list:
!     mype     - pe number
!
!   output argument list:
!     no output arguments
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind
  implicit none

  integer(i_kind),intent(in   ) :: mype

  if (mype==0) write(6,*)'WRNEMSNMMA_BINARY:  enter dummy call, do nothing'
end subroutine  wrnemsnmma_binary

subroutine wrwrfnmma_netcdf(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    wrwrfnmma              write out wrf NMM restart file
!   prgmmr: parrish          org: np22                date: 2004-06-23
!
! abstract:  dummy call to read wrf NMM guess restart interface file, 
!            add analysis increment, and write out wrf NMM analysis 
!            restart interface file.
!
!
! program history log
!   2005-02-25 todling - add dummy subroutine to skip over wrf code
!   2005-03-14 treadon - add write statement to note entry into dummy routine
!
!   input argument list:
!     mype     - pe number
!
!   output argument list:
!     no output arguments
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind
  implicit none

  integer(i_kind),intent(in   ) :: mype

  if (mype==0) write(6,*)'WRWRFNMMA_NETCDF:  enter dummy call, do nothing'
end subroutine wrwrfnmma_netcdf
#endif /* End no WRF-library block */
