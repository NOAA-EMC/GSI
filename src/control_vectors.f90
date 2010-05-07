module control_vectors
!$$$ module documentation block
!           .      .    .                                       .
! module:   control_vectors
!   prgmmr: tremolet
!
! abstract: define control vectors and basic operators
!
! program history log:
!   2007-04-13  tremolet - initial code
!   2007-08-03  todling  - using get_lun for file unit definition
!   2008-01-04  tremolet - improve allocate/deallocate
!   2008-12-29  todling  - add omp to various loops
!   2009-01-27  todling  - rename prt_norms to prevent IBM compiler confusion
!   2009-08-12  lueken   - updated documentation
!   2009-09-20  parrish  - add pointer variable a_en to definition of type control_state
!                           also, add module variables n_ens, nlva_en
!   2009-11-10  todling  - remove redundant dot products
!   2010-02-17  treadon  - fix in predictor part of random vector
!   2010-02-20  parrish  - add functions dplevs_ens for use with dual-resolution hybrid ensemble option.
!   2010-03-11  zhu      - add changes for generalizing control variables,i.e.,
!                          anav_info,nvars,nrf*,allocate_cs,deallocate_cs,
!                          assign_cs2array,assign_array2cs
!   2010-05-05  derber - omp commands removed
!
! subroutines included:
!   sub anav_info   
!   sub setup_control_vectors
!   sub allocate_cv
!   sub deallocate_cv
!   sub assign_scalar2cv
!   sub assign_cv2cv
!   sub assign_array2cv
!   sub assign_cv2array
!   sub qdot_prod_vars
!   sub prt_norms
!   sub prt_norms_vars
!   sub axpy
!   sub random_cv
!   sub write_cv
!   sub read_cv
!   sub inquire_cv
!
! functions included:
!   dplevs
!   dplevs_ens
!   qdot_prod_sub
!   dot_prod_cv
!   qdot_prod_cv
!   qdot_prod_cv_eb
!   maxval_cv
!   qdot_product
!
! variable definitions:
!   def n_ens     - number of ensemble perturbations (=0 except when hybrid ensemble option turned on)
!   def nlva_en   - total number of levels for hybrid ensemble control variable a_en
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: r_kind,i_kind,r_quad
use mpimod, only: mpi_comm_world,mpi_max,mpi_rtype,mype,npe,ierror
use constants, only: izero, ione, zero, one, two, zero_quad
use gsi_4dvar, only: iadatebgn
use file_utility, only : get_lun
use mpl_allreducemod, only: mpl_allreduce

implicit none
save
private
public control_state, control_vector, allocate_cv, deallocate_cv, assignment(=), &
     & dot_product, prt_control_norms, axpy, random_cv, setup_control_vectors, &
     & write_cv, read_cv, inquire_cv, maxval, qdot_prod_sub, anav_info,&
     & allocate_cs, deallocate_cs, assign_cs2array, assign_array2cs
public nrf_3d,nrf_var,nrf2_loc,nrf3_loc,nrf_tracer,nrf_levb,nrf_leve, &
     & nrf,nrf2,nrf3,nvars,ntracer,nrf3_sf,nrf3_vp,nrf3_t,nrf3_q,nrf3_oz,nrf3_cw, &
     & nrf2_ps,nrf2_sst

type control_state
   real(r_kind), pointer :: values(:) => NULL()
   real(r_kind), pointer :: st(:)  => NULL()
   real(r_kind), pointer :: vp(:)  => NULL()
   real(r_kind), pointer :: t(:)   => NULL()
   real(r_kind), pointer :: rh(:)  => NULL()
   real(r_kind), pointer :: oz(:)  => NULL()
   real(r_kind), pointer :: cw(:)  => NULL()
   real(r_kind), pointer :: p(:)   => NULL()
   real(r_kind), pointer :: sst(:) => NULL()
  real(r_kind), pointer :: a_en(:)=> NULL()    !  ensemble control variable (only used if l_hyb_ens=.true.)
end type control_state

type control_vector
   integer(i_kind) :: lencv
   real(r_kind), pointer :: values(:) => NULL()
   type(control_state), allocatable :: step(:)
   real(r_kind), pointer :: predr(:) => NULL()
   real(r_kind), pointer :: predp(:) => NULL()
   logical :: lallocated = .false.
end type control_vector

integer(i_kind) :: nclen,nclen1,nsclen,npclen,nrclen,nsubwin,nval_len
integer(i_kind) :: latlon11,latlon1n,lat2,lon2,nsig,n_ens,nlva_en
logical :: lsqrtb

integer(i_kind) :: m_vec_alloc, max_vec_alloc, m_allocs, m_deallocs

logical,allocatable,dimension(:):: nrf_3d
character(len=5),allocatable,dimension(:):: nrf_var
integer(i_kind),allocatable,dimension(:):: nrf2_loc,nrf3_loc
integer(i_kind),allocatable,dimension(:):: nrf_tracer,nrf_levb,nrf_leve
integer(i_kind) nrf,nrf2,nrf3,nvars
integer(i_kind) ntracer
integer(i_kind) nrf3_sf,nrf3_vp,nrf3_t,nrf3_q,nrf3_oz,nrf3_cw
integer(i_kind) nrf2_ps,nrf2_sst

logical :: llinit = .false.

! ----------------------------------------------------------------------
INTERFACE ASSIGNMENT (=)
MODULE PROCEDURE assign_scalar2cv, assign_array2cv, assign_cv2array, assign_cv2cv
END INTERFACE

INTERFACE DOT_PRODUCT
MODULE PROCEDURE dot_prod_cv,qdot_prod_cv,qdot_prod_cv_eb
END INTERFACE

INTERFACE MAXVAL
MODULE PROCEDURE maxval_cv
END INTERFACE

INTERFACE PRT_CONTROL_NORMS
MODULE PROCEDURE prt_norms
END INTERFACE
! ----------------------------------------------------------------------
contains
! ----------------------------------------------------------------------
subroutine setup_control_vectors(ksig,klat,klon,katlon11,katlon1n, &
                               & ksclen,kpclen,kclen,ksubwin,kval_len,ldsqrtb,k_ens)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setup_control_vectors
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!   2009-09-20  parrish - add optional input variable k_ens, which communicates size
!                          of ensemble used when hybrid ensemble option is turned on.
!
!   input argument list:
!    ksig
!    klat,klon
!    katlon11
!    katlon1n
!    ksclen
!    kpclen
!    kclen
!    ksubwin
!    kval_len
!    ldsqrtb
!    k_ens     - optional, if present, then size of ensemble used in hybrid ensemble option
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  integer(i_kind)          , intent(in   ) :: ksig,klat,klon,katlon11,katlon1n, &
                               & ksclen,kpclen,kclen,ksubwin,kval_len
  integer(i_kind), optional, intent(in   ) :: k_ens
  logical                  , intent(in   ) :: ldsqrtb

  nsig=ksig
  lat2=klat
  lon2=klon
  latlon11=katlon11
  latlon1n=katlon1n
  nsclen=ksclen
  npclen=kpclen
  nrclen=nsclen+npclen
  nclen =kclen
  nclen1=nclen-nrclen
  nsubwin=ksubwin
  nval_len=kval_len
  lsqrtb=ldsqrtb
  n_ens=izero
  nlva_en=izero
  if(present(k_ens)) then
     n_ens=k_ens
     nlva_en=n_ens*nsig
  end if

  llinit = .true.
  m_vec_alloc=izero
  max_vec_alloc=izero
  m_allocs=izero
  m_deallocs=izero

  call inquire_cv

  return
end subroutine setup_control_vectors
! ----------------------------------------------------------------------
  subroutine anav_info(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    anav_info
!   prgmmr: zhu          org: np23               date:  2008-03-29
!
! abstract: read in control variables information
!
! program history log:
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use kinds, only: i_kind
    use constants, only: izero,ione
    implicit none

    character(len=1) cf
    character(len=5) cvar
    character(len=120) crecord
    integer(i_kind) mype,clevs,cuse_idx,ctrace_idx
    integer(i_kind) lunin,istat,nvar,loc

    lunin = 47
    open(lunin,file='anavinfo',form='formatted')
    rewind(lunin)

    nvar=izero
    nrf2=izero
    nrf3=izero
    ntracer=izero
    read1: do
       read(lunin,1030,iostat=istat) cf,cvar,crecord
1030   format(a1,a5,2x,a120)
       if (istat /= izero) exit
       if (cf == '!')cycle

       read(crecord,*) cuse_idx,clevs,ctrace_idx
       if (cuse_idx < izero) cycle
       nvar=nvar+ione
       if (clevs > ione) then
          nrf3=nrf3+ione
       else
          nrf2=nrf2+ione
       end if
       if (ctrace_idx > izero) ntracer=ntracer+ione
    enddo read1
    if (istat>izero) then
       write(6,*)'ANAV_INFO:  ***ERROR*** error reading anavinfo, istat=',istat
    end if

    if (nvar==izero) then
       write(6,*)'ANAV_INFO: NO CONTROL VARIABLES ARE SPECIFIED'
       write(6,*)'ANAV_INFO: stop'
       call stop2(76)
    end if

!   Allocate and initialize
    nrf=nvar
    allocate(nrf_var(nrf),nrf_3d(nrf),nrf_levb(nrf),nrf_leve(nrf))
    allocate(nrf2_loc(nrf2),nrf3_loc(nrf3),nrf_tracer(ntracer))
    nrf_3d=.false.
    nrf_var=' '
    nrf_levb=-ione
    nrf_leve=-ione
    nrf2_loc=-ione
    nrf3_loc=-ione
    nrf_tracer=-ione

!   Read in analysis variables
    nvar=izero
    nrf2=izero
    nrf3=izero
    ntracer=izero
    rewind(lunin)
    read2: do
       read(lunin,1031,iostat=istat) cf,cvar,crecord
1031   format(a1,a5,2x,a120)
       if (istat /= izero) exit
       if (cf == '!')cycle

       read(crecord,*) cuse_idx,clevs,ctrace_idx
       if (cuse_idx < izero) cycle
       nvar=nvar+ione
       nrf_var(nvar)=cvar

       if (clevs > ione) then
          nrf_3d(nvar)=.true.
          nrf3=nrf3+ione
          nrf3_loc(nrf3)=nvar
       else
          nrf2=nrf2+ione
          nrf2_loc(nrf2)=nvar
       end if
       if (ctrace_idx > izero) then
          ntracer=ntracer+ione
          nrf_tracer(ntracer)=nvar
       end if
    enddo read2
    close(lunin)

    nrf3_sf=-ione
    nrf3_vp=-ione
    nrf3_t=-ione
    nrf3_q=-ione
    nrf3_oz=-ione
    nrf3_cw=-ione
    nrf2_ps=-ione
    nrf2_sst=-ione
    do nvar=1,nrf3
       loc=nrf3_loc(nvar)
       cvar=nrf_var(loc)
       select case(cvar)
               case ('sf','SF'); nrf3_sf=nvar
               case ('vp','VP'); nrf3_vp=nvar
               case ('t','T')  ; nrf3_t=nvar
               case ('q','Q')  ; nrf3_q=nvar
               case ('oz','OZ'); nrf3_oz=nvar
               case ('cw','CW'); nrf3_cw=nvar
       end select
    end do
    do nvar=1,nrf2
       loc=nrf2_loc(nvar)
       cvar=nrf_var(loc)
       select case(cvar)
               case ('sst','SST'); nrf2_sst=nvar
               case ('ps','PS'); nrf2_ps=nvar
       end select
    end do

    if (nrf2_sst>izero) then
       nvars=nrf+2_i_kind
    else
       nvars=nrf
    end if

    if (mype==0) then
       write(6,*) 'ANAV_INFO: CONTROL VARIABLES ARE ', (nrf_var(nvar),nvar=1,nrf)
    end if

    return
  end subroutine anav_info

! ----------------------------------------------------------------------
subroutine allocate_cs(ycs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    allocate_cs
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2010-02-25  zhu     - use for control state
!   2010-04-15  treadon - remove lsqrtb branch
!
!   input argument list:
!
!   output argument list:
!    ycv
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  type(control_state), intent(  out) :: ycs
  character(len=5) cvar
  integer(i_kind) :: ii,n,ngrid

  ALLOCATE(ycs%values(nval_len))

  ii=izero
  do n=1,nrf
     if (nrf_3d(n)) then
        ngrid=latlon1n
     else
        ngrid=latlon11
     end if

     cvar=nrf_var(n)
     select case(cvar)
        case('sf','SF')
           ycs%st  => ycs%values(ii+ione:ii+ngrid)
        case('vp','VP')
           ycs%vp  => ycs%values(ii+ione:ii+ngrid)
        case('t','T')
           ycs%t   => ycs%values(ii+ione:ii+ngrid)
        case('q','Q')
           ycs%rh  => ycs%values(ii+ione:ii+ngrid)
        case('oz','OZ')
           ycs%oz  => ycs%values(ii+ione:ii+ngrid)
        case('cw','CW')
           ycs%cw  => ycs%values(ii+ione:ii+ngrid)
        case('ps','PS')
           ycs%p   => ycs%values(ii+ione:ii+ngrid)
        case('sst','SST')
           ycs%sst => ycs%values(ii+ione:ii+ngrid)
        case default
           write(6,*) 'allocate_cs: ERROR, unrecognized control variable ',cvar
           call stop2(100)
     end select
     ii=ii+ngrid
  end do
  if(n_ens >  izero) then
     ycs%a_en => ycs%values(ii+1:ii+n_ens*latlon1n)
     ii=ii+n_ens*latlon1n
  end if

  m_allocs=m_allocs+ione

  return
end subroutine allocate_cs

! ----------------------------------------------------------------------
subroutine deallocate_cs(ycs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    deallocate_cs
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2010-03-11 zhu - for control state
!
!   input argument list:
!    yst
!
!   output argument list:
!    ycs
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  type(control_state), intent(inout) :: ycs
  integer n 

  do n=1,nrf
     select case(trim(nrf_var(n)))
        case('sf','SF') ;  NULLIFY(ycs%st )
        case('vp','VP') ;  NULLIFY(ycs%vp )
        case('t','T')   ;  NULLIFY(ycs%t  )
        case('q','Q') ;  NULLIFY(ycs%rh )
        case('oz','OZ') ;  NULLIFY(ycs%oz )
        case('cw','CW') ;  NULLIFY(ycs%cw )
        case('ps','PS')   ;  NULLIFY(ycs%p  )
        case('sst','SST'); NULLIFY(ycs%sst)
     end select
  end do

  if(n_ens >  izero) NULLIFY(ycs%a_en)
  DEALLOCATE(ycs%values)

  m_deallocs=m_deallocs+ione
  return
end subroutine deallocate_cs

! ----------------------------------------------------------------------
subroutine allocate_cv(ycv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    allocate_cv
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!   2009-09-20  parrish - add optional allocation of hybrid ensemble control variable a_en
!   2010-02-20  parrish - add structure variable grd_ens as part of changes for dual-resolution
!                           hybrid ensemble system.
!   2010-02-25  zhu     - use nrf_var and nrf_3d to specify the order control variables
!
!   input argument list:
!
!   output argument list:
!    ycv
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use hybrid_ensemble_parameters, only: grd_ens
  implicit none
  type(control_vector), intent(  out) :: ycv
  character(len=5) cvar
  integer(i_kind) :: ii,jj,n,ngrid

  if (ycv%lallocated) then
     write(6,*)'allocate_cv: vector already allocated'
     call stop2(108)
  end if

  ycv%lallocated=.true.
  ycv%lencv = nclen
  ALLOCATE(ycv%values(ycv%lencv))
  ALLOCATE(ycv%step(nsubwin))

  ii=izero
  do jj=1,nsubwin
     ycv%step(jj)%values => ycv%values(ii+ione:ii+nval_len)

     if (lsqrtb) then
        do n=1,nrf
           cvar=nrf_var(n)
           select case(cvar)
              case('sf','SF')
                 ycv%step(jj)%st  => NULL()
              case('vp','VP')
                 ycv%step(jj)%vp  => NULL()
              case('t','T')
                 ycv%step(jj)%t   => NULL()
              case('q','Q')
                 ycv%step(jj)%rh  => NULL()
              case('oz','OZ')
                 ycv%step(jj)%oz  => NULL()
              case('cw','CW')
                 ycv%step(jj)%cw  => NULL()
              case('ps','PS')
                 ycv%step(jj)%p   => NULL()
              case('sst','SST')
                 ycv%step(jj)%sst => NULL()
              case default
                 write(6,*) 'allocate_cv: ERROR, unrecognized control variable ',cvar
                 call stop2(100)
           end select
        end do
        ii=ii+nval_len
     else
        do n=1,nrf
           if (nrf_3d(n)) then 
              ngrid=latlon1n
           else
              ngrid=latlon11
           end if

           cvar=nrf_var(n)
           select case(cvar)
              case('sf','SF')
                 ycv%step(jj)%st  => ycv%values(ii+ione:ii+ngrid)
              case('vp','VP')
                 ycv%step(jj)%vp  => ycv%values(ii+ione:ii+ngrid)
              case('t','T')
                 ycv%step(jj)%t   => ycv%values(ii+ione:ii+ngrid)
              case('q','Q')
                 ycv%step(jj)%rh  => ycv%values(ii+ione:ii+ngrid)
              case('oz','OZ')
                 ycv%step(jj)%oz  => ycv%values(ii+ione:ii+ngrid)
              case('cw','CW')
                 ycv%step(jj)%cw  => ycv%values(ii+ione:ii+ngrid)
              case('ps','PS')
                 ycv%step(jj)%p   => ycv%values(ii+ione:ii+ngrid)
              case('sst','SST')
                 ycv%step(jj)%sst => ycv%values(ii+ione:ii+ngrid)
              case default
                 write(6,*) 'allocate_cv: ERROR, unrecognized control variable ',cvar
                 call stop2(100)
           end select
           ii=ii+ngrid
        end do
        if(n_ens >  izero) then
           ycv%step(jj)%a_en => ycv%values(ii+1:ii+n_ens*grd_ens%latlon1n)
           ii=ii+n_ens*grd_ens%latlon1n
        end if
     endif
  enddo

  ycv%predr => ycv%values(ii+ione:ii+nsclen)
  ii=ii+nsclen
  ycv%predp => ycv%values(ii+ione:ii+npclen)
  ii=ii+npclen

  if (ii/=nclen) then
     write(6,*)'allocate_mods: error length',ii,nclen
     call stop2(109)
  end if

  m_allocs=m_allocs+ione
  m_vec_alloc=m_vec_alloc+ione
  max_vec_alloc=MAX(max_vec_alloc,m_vec_alloc)

  return
end subroutine allocate_cv
! ----------------------------------------------------------------------
subroutine deallocate_cv(ycv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    deallocate_cv
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!   2009-09-20  parrish - add optional removal of pointer to hybrid ensemble control variable a_en
!   2010-02-25  zhu     - add flexibility to control variable
!
!   input argument list:
!    ycv
!
!   output argument list:
!    ycv
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  type(control_vector), intent(inout) :: ycv
  integer(i_kind) :: ii,n

  if (ycv%lallocated) then
     do ii=1,nsubwin
        do n=1,nrf
           select case(trim(nrf_var(n)))
              case('sf','SF') ;  NULLIFY(ycv%step(ii)%st )
              case('vp','VP') ;  NULLIFY(ycv%step(ii)%vp )
              case('t','T')   ;  NULLIFY(ycv%step(ii)%t  )
              case('q','Q') ;  NULLIFY(ycv%step(ii)%rh )
              case('oz','OZ') ;  NULLIFY(ycv%step(ii)%oz )
              case('cw','CW') ;  NULLIFY(ycv%step(ii)%cw )
              case('ps','PS')   ;  NULLIFY(ycv%step(ii)%p  )
              case('sst','SST'); NULLIFY(ycv%step(ii)%sst)
           end select
        end do

        if(n_ens >  izero) NULLIFY(ycv%step(ii)%a_en)
     end do
     NULLIFY(ycv%predr)
     NULLIFY(ycv%predp)

     DEALLOCATE(ycv%step)
     DEALLOCATE(ycv%values)

     ycv%lallocated=.false.

     m_deallocs=m_deallocs+ione
     m_vec_alloc=m_vec_alloc-ione
  else
     if (mype==izero) write(6,*)'deallocate_cv warning: vector not allocated'
  endif

  return
end subroutine deallocate_cv
! ----------------------------------------------------------------------
subroutine assign_scalar2cv(ycv,pval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    assign_scalar2cv
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    ycv
!    pval
!
!   output argument list:
!    ycv
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  type(control_vector), intent(inout) :: ycv
  real(r_kind)        , intent(in   ) :: pval
  integer(i_kind) :: ii

  DO ii=1,ycv%lencv
     ycv%values(ii)=pval
  ENDDO

  return
end subroutine assign_scalar2cv
! ----------------------------------------------------------------------
subroutine assign_cv2cv(ycv,xcv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    assign_cv2cv
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    ycv
!    xcv
!
!   output argument list:
!    ycv
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  type(control_vector), intent(inout) :: ycv
  type(control_vector), intent(in   ) :: xcv
  integer(i_kind) :: ii

  if (xcv%lencv/=ycv%lencv) then
     write(6,*)'assign_cv2cv: error length',xcv%lencv,ycv%lencv
     call stop2(110)
  end if

  DO ii=1,ycv%lencv
     ycv%values(ii)=xcv%values(ii)
  ENDDO

  return
end subroutine assign_cv2cv
! ----------------------------------------------------------------------
subroutine assign_array2cv(ycv,parray)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    assign_array2cv
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    ycv
!    parray
!
!   output argument list:
!    ycv
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  type(control_vector), intent(inout) :: ycv
  real(r_kind)        , intent(in   ) :: parray(:)
  integer(i_kind) :: ii

  if (size(parray)/=ycv%lencv) then
     write(6,*)'assign_array2cv: array wrong length',size(parray),ycv%lencv
     call stop2(111)
  end if


  DO ii=1,ycv%lencv
     ycv%values(ii)=parray(ii)
  ENDDO

  return
end subroutine assign_array2cv
! ----------------------------------------------------------------------
subroutine assign_cv2array(parray,ycv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    assign_array2cv
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    ycv
!
!   output argument list:
!    parray
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  real(r_kind)        , intent(  out) :: parray(:)
  type(control_vector), intent(in   ) :: ycv
  integer(i_kind) :: ii

  if (size(parray)/=ycv%lencv) then
     write(6,*)'assign_cv2array: array wrong length',size(parray),ycv%lencv
     call stop2(112)
  end if

  DO ii=1,ycv%lencv
     parray(ii)=ycv%values(ii)
  ENDDO

  return
end subroutine assign_cv2array
! ----------------------------------------------------------------------
 subroutine assign_array2cs(ycs,st,vp,t,rh,oz,cw,ps,sst)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    assign_array2cs
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2010-03-15  zhu
!
!   input argument list:
!    ycs
!
!   output argument list:
!    st,vp,t,q,oz,cw,ps,sst
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use gridmod, only: latlon1n,latlon11
  implicit none

  real(r_kind),dimension(latlon1n),intent(in) :: st,vp,t,rh,oz,cw
  real(r_kind),dimension(latlon11),intent(in) :: ps
  real(r_kind),dimension(latlon11),optional,intent(in) :: sst
  type(control_state),intent(inout) :: ycs
  integer(i_kind) :: ii

  DO ii=1,latlon1n
     ycs%st(ii)=st(ii)
     ycs%vp(ii)=vp(ii)
     ycs%t(ii)=t(ii)
     ycs%rh(ii)=rh(ii)
     if (nrf3_oz>izero) ycs%oz(ii)=oz(ii)
     if (nrf3_cw>izero) ycs%cw(ii)=cw(ii)
  ENDDO

  DO ii=1,latlon11
     ycs%p(ii)=ps(ii)
     if (present(sst) .and. nrf2_sst>izero) ycs%sst(ii)=sst(ii)
  ENDDO

  return
end subroutine assign_array2cs

! ----------------------------------------------------------------------
 subroutine assign_cs2array(ycs,st,vp,t,rh,oz,cw,ps,sst)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    assign_cs2array
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2010-03-15  zhu
!
!   input argument list:
!    ycs
!
!   output argument list:
!    st,vp,t,q,oz,cw,ps,sst
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use gridmod, only: latlon1n,latlon11
  implicit none

  type(control_state),intent(in) :: ycs
  real(r_kind),dimension(latlon1n),intent(out) :: st,vp,t,rh,oz,cw
  real(r_kind),dimension(latlon11),intent(out) :: ps
  real(r_kind),dimension(latlon11),optional,intent(out) :: sst
  integer(i_kind) :: ii

  DO ii=1,latlon1n
     st(ii)=ycs%st(ii)
     vp(ii)=ycs%vp(ii)
      t(ii)=ycs%t(ii)
     rh(ii)=ycs%rh(ii)
     if (nrf3_oz>izero) oz(ii)=ycs%oz(ii)
     if (nrf3_cw>izero) cw(ii)=ycs%cw(ii)
  ENDDO

  DO ii=1,latlon11
     ps(ii)=ycs%p(ii)
     if (present(sst) .and. nrf2_sst>izero) sst(ii)=ycs%sst(ii)
  ENDDO

  return
end subroutine assign_cs2array

! ----------------------------------------------------------------------
real(r_quad) function dplevs(nlevs,dx,dy)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dplevs
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    nlevs
!    dx,dy
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  integer(i_kind),intent(in   ) :: nlevs
  real(r_kind)   ,intent(in   ) :: dx(lat2,lon2,nlevs),dy(lat2,lon2,nlevs)

  integer(i_kind) :: ii,jj,kk

  dplevs=zero_quad
  do kk=1,nlevs
     do jj=2,lon2-ione
        do ii=2,lat2-ione
           dplevs=dplevs+dx(ii,jj,kk)*dy(ii,jj,kk)
        end do
     end do
  end do

return
end function dplevs
! ----------------------------------------------------------------------
real(r_quad) function dplevs_ens(nlevs,dx,dy)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dplevs_ens  copy of dplevs for use with ensemble control variable a_en
!   prgmmr: parrish          org: wx22                date: 2010-02-20
!
! abstract:
!
! program history log:
!   2010-02-20  parrish, initial documentation
!
!   input argument list:
!    nlevs
!    dx,dy
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use hybrid_ensemble_parameters, only: grd_ens
  implicit none
  integer(i_kind),intent(in   ) :: nlevs
  real(r_kind)   ,intent(in   ) :: dx(grd_ens%lat2,grd_ens%lon2,nlevs),dy(grd_ens%lat2,grd_ens%lon2,nlevs)

  integer(i_kind) :: ii,jj,kk

  dplevs_ens=zero_quad
  do kk=1,nlevs
     do jj=2,grd_ens%lon2-ione
        do ii=2,grd_ens%lat2-ione
           dplevs_ens=dplevs_ens+dx(ii,jj,kk)*dy(ii,jj,kk)
        end do
     end do
  end do

return
end function dplevs_ens
! ----------------------------------------------------------------------
real(r_quad) function qdot_prod_sub(xcv,ycv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    qdot_prod_sub
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!   2009-09-20  parrish - add hybrid ensemble control variable a_en contribution to dot product
!
!   input argument list:
!    xcv,ycv
!
!   output argument list:
!    prods
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  type(control_vector), intent(in   ) :: xcv, ycv
  integer(i_kind) :: ii,j


  qdot_prod_sub=zero_quad

! Independent part of vector
  if (lsqrtb) then
     do ii=1,nsubwin
        qdot_prod_sub=qdot_prod_sub+qdot_product( xcv%step(ii)%values(:) ,ycv%step(ii)%values(:) )
     end do
  else
     do ii=1,nsubwin
        qdot_prod_sub = qdot_prod_sub + dplevs(nsig,xcv%step(ii)%st(:) ,ycv%step(ii)%st(:))
        qdot_prod_sub = qdot_prod_sub + dplevs(nsig,xcv%step(ii)%vp(:) ,ycv%step(ii)%vp(:))
        qdot_prod_sub = qdot_prod_sub + dplevs(nsig,xcv%step(ii)%t(:)  ,ycv%step(ii)%t(:))
        qdot_prod_sub = qdot_prod_sub + dplevs(nsig,xcv%step(ii)%rh(:) ,ycv%step(ii)%rh(:))
        if (nrf3_oz>izero) qdot_prod_sub = qdot_prod_sub + dplevs(nsig,xcv%step(ii)%oz(:) ,ycv%step(ii)%oz(:))
        if (nrf3_cw>izero) qdot_prod_sub = qdot_prod_sub + dplevs(nsig,xcv%step(ii)%cw(:) ,ycv%step(ii)%cw(:))
        qdot_prod_sub = qdot_prod_sub + dplevs(ione,xcv%step(ii)%p(:)  ,ycv%step(ii)%p(:))
        if (nrf2_sst>izero) qdot_prod_sub = qdot_prod_sub + dplevs(ione,xcv%step(ii)%sst(:),ycv%step(ii)%sst(:))
        if(n_ens >  izero) &
        qdot_prod_sub = qdot_prod_sub + dplevs_ens(nlva_en,xcv%step(ii)%a_en(:)  ,ycv%step(ii)%a_en(:))
     end do
  end if

! Duplicated part of vector
  if(mype == izero)then
     do j=nclen1+ione,nclen
        qdot_prod_sub=qdot_prod_sub+xcv%values(j)*ycv%values(j) 
     end do
  end if

return
end function qdot_prod_sub
! ----------------------------------------------------------------------
subroutine qdot_prod_vars_eb(xcv,ycv,prods,eb)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    qdot_prod_vars_eb  copy of qdot_prod_vars for J_ens
!   prgmmr: parrish          org: np22                date: 2009-09-20
!
! abstract: copy qdot_prod_vars and add extra code to compute J_b or
!            J_ens when running in hybrid_ensemble mode.  extra input
!            character string eb is used to signal if J_b or J_ens is
!            to be computed.
!
! program history log:
!   2009-09-20  parrish - initial documentation
!
!   input argument list:
!    xcv,ycv
!    eb        - eb= 'cost_b' then return J_b in prods
!                  = 'cost_e' then return J_ens in prods
!
!   output argument list:
!    prods
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  type(control_vector), intent(in   ) :: xcv, ycv
  character(len=*)    , intent(in   ) :: eb
  real(r_quad)        , intent(  out) :: prods(nsubwin+ione)

  real(r_quad) :: zz(nsubwin)
  integer(i_kind) :: ii

  prods(:)=zero_quad
  zz(:)=zero_quad

! Independent part of vector
  if (lsqrtb) then
     do ii=1,nsubwin
        zz(ii)=qdot_product( xcv%step(ii)%values(:) ,ycv%step(ii)%values(:) )
     end do
  else
     if(trim(eb) == 'cost_b') then
        do ii=1,nsubwin
           zz(ii) = zz(ii) + dplevs(nsig,xcv%step(ii)%st(:) ,ycv%step(ii)%st(:))
           zz(ii) = zz(ii) + dplevs(nsig,xcv%step(ii)%vp(:) ,ycv%step(ii)%vp(:))
           zz(ii) = zz(ii) + dplevs(nsig,xcv%step(ii)%t(:)  ,ycv%step(ii)%t(:))
           zz(ii) = zz(ii) + dplevs(nsig,xcv%step(ii)%rh(:) ,ycv%step(ii)%rh(:))
           if (nrf3_oz>izero) &
           zz(ii) = zz(ii) + dplevs(nsig,xcv%step(ii)%oz(:) ,ycv%step(ii)%oz(:))
           if (nrf3_cw>izero) &
           zz(ii) = zz(ii) + dplevs(nsig,xcv%step(ii)%cw(:) ,ycv%step(ii)%cw(:))
           zz(ii) = zz(ii) + dplevs(1   ,xcv%step(ii)%p(:)  ,ycv%step(ii)%p(:))
           if (nrf2_sst>izero) &
           zz(ii) = zz(ii) + dplevs(ione,xcv%step(ii)%sst(:),ycv%step(ii)%sst(:))
        end do
     end if
     if(trim(eb) == 'cost_e') then
        do ii=1,nsubwin
           if(n_ens >  izero) &
           zz(ii) = zz(ii) + dplevs_ens(nlva_en,xcv%step(ii)%a_en(:),  ycv%step(ii)%a_en(:))
        end do
     end if
  end if

  call mpl_allreduce(nsubwin,qpvals=zz)
  prods(1:nsubwin) = zz(1:nsubwin)

! Duplicated part of vector
  if(trim(eb) == 'cost_b') then
     if (nsclen>izero) then
        prods(nsubwin+ione) = prods(nsubwin+ione) + qdot_product(xcv%predr(:),ycv%predr(:))
     endif
     if (npclen>izero) then
        prods(nsubwin+ione) = prods(nsubwin+ione) + qdot_product(xcv%predp(:),ycv%predp(:))
     endif
  end if

  return
end subroutine qdot_prod_vars_eb
! ----------------------------------------------------------------------
real(r_kind) function dot_prod_cv(xcv,ycv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dot_prod_cv
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    xcv,ycv
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  type(control_vector), intent(in   ) :: xcv, ycv

! local variables
  real(r_quad) :: dd(ione)
  integer(i_kind) :: ii

  if (xcv%lencv/=ycv%lencv) then
     write(6,*)'dot_prod_cv: error length',xcv%lencv,ycv%lencv
     call stop2(113)
  end if

  dd(ione) = qdot_prod_sub(xcv,ycv)
  call mpl_allreduce(ione,qpvals=dd)
  dot_prod_cv = dd(ione)

return
end function dot_prod_cv
! ----------------------------------------------------------------------
real(r_quad) function qdot_prod_cv(xcv,ycv,kind)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    qdot_prod_cv
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    xcv,ycv
!    kind
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  integer(i_kind)     , intent(in   ) :: kind
  type(control_vector), intent(in   ) :: xcv, ycv

! local variables
  real(r_quad) :: dd(ione)
  integer(i_kind) :: ii

  if (xcv%lencv/=ycv%lencv) then
     write(6,*)'qdot_prod_cv: error length',xcv%lencv,ycv%lencv
     call stop2(114)
  end if

  dd(ione) = qdot_prod_sub(xcv,ycv)
  call mpl_allreduce(ione,qpvals=dd)
  qdot_prod_cv = dd(ione)

return
end function qdot_prod_cv
! ----------------------------------------------------------------------
real(r_quad) function qdot_prod_cv_eb(xcv,ycv,kind,eb)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    qdot_prod_cv_eb  copy of qdot_prod_cv for J_ens
!   prgmmr: parrish          org: np22                date: 2009-09-20
!
! abstract: copy qdot_prod_cv and add extra code to compute J_b or
!            J_ens when running in hybrid_ensemble mode.  extra input
!            character string eb is used to signal if J_b or J_ens is
!            to be computed.
!
! program history log:
!   2009-09-20  parrish - initial documentation
!
!   input argument list:
!    xcv,ycv
!    kind
!    eb        - eb= 'cost_b' then return J_b in prods
!                  = 'cost_e' then return J_ens in prods
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  integer(i_kind)     , intent(in   ) :: kind
  character(len=*)    , intent(in   ) :: eb
  type(control_vector), intent(in   ) :: xcv, ycv

! local variables
  real(r_quad) :: zz(nsubwin+ione)
  integer(i_kind) :: ii

  if (xcv%lencv/=ycv%lencv) then
      write(6,*)'qdot_prod_cv_eb: error length',xcv%lencv,ycv%lencv
      call stop2(114)
  end if

  call qdot_prod_vars_eb(xcv,ycv,zz,eb)

  qdot_prod_cv_eb= zero_quad
  do ii=1,nsubwin+ione
     qdot_prod_cv_eb = qdot_prod_cv_eb + zz(ii)
  enddo

return
end function qdot_prod_cv_eb
! ----------------------------------------------------------------------
subroutine prt_norms(xcv,sgrep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    prt_norms
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    xcv
!    sgrep
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  type(control_vector), intent(in   ) :: xcv
  character(len=*)    , intent(in   ) :: sgrep

  real(r_quad) :: zt

  zt = qdot_prod_cv(xcv,xcv,r_quad)
  zt=sqrt(zt)

  if (mype==izero) then
     write(6,*)sgrep,' global  norm =',real(zt,r_kind)
  endif

!!!  call prt_norms_vars(xcv,sgrep)

  return
end subroutine prt_norms
! ----------------------------------------------------------------------
subroutine prt_norms_vars(xcv,sgrep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    prt_norms_vars
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    xcv
!    sgrep
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use m_stats,only : stats_sum,stats_allreduce
  implicit none
  type(control_vector), intent(in   ) :: xcv
  character(len=*)    , intent(in   ) :: sgrep

  real   (r_kind),dimension(8) :: vdot,vsum,vmin,vmax
  integer(i_kind),dimension(8) :: vnum
  integer :: iw,nsw,iv,ivend,nv
  real(r_kind),pointer,dimension(:) :: piv

  character(len=4),dimension(9) :: vnames = &
                        (/'st  ','vp  ','t   ','rh  ','oz  ','cw  ','p   ','sst ','a_en'/)

  nsw=size(xcv%step)

  ivend=8_i_kind
  if(n_ens >  izero) ivend=9_i_kind
  do iv=1,ivend
     do iw=1,nsw
        piv => null()
        select case(iv)
           case(1); piv => xcv%step(iw)%st
           case(2); piv => xcv%step(iw)%vp
           case(3); piv => xcv%step(iw)%t
           case(4); piv => xcv%step(iw)%rh
           case(5); piv => xcv%step(iw)%oz
           case(6); piv => xcv%step(iw)%cw
           case(7); piv => xcv%step(iw)%p
           case(8); piv => xcv%step(iw)%sst
           case(9); piv => xcv%step(iw)%a_en
        end select

        call stats_sum(piv, &
                       vdot(iv),vsum(iv),vmin(iv),vmax(iv),vnum(iv),add=iw>ione)
     enddo

     call stats_allreduce(vdot(iv),vsum(iv),vmin(iv),vmax(iv),  &
                          vnum(iv),MPI_comm_world)
     nv=max(vnum(iv),ione)
  
     if(mype==izero) then
        write(6,'(2(1x,a),4(1x,ES20.12),1x,i10)')               &
          sgrep,vnames(iv),sqrt(vdot(iv)/nv),vsum(iv)/nv,       &
          vmin(iv),vmax(iv),vnum(iv)
     endif
  end do
  piv => null()
  
end subroutine prt_norms_vars
! ----------------------------------------------------------------------
subroutine axpy(alpha,xcv,ycv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    axpy
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    xcv
!    alpha
!
!   output argument list:
!    ycv
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  real(r_kind)        , intent(in   ) :: alpha
  type(control_vector), intent(in   ) :: xcv
  type(control_vector), intent(inout) :: ycv
  integer(i_kind) :: ii

  if (xcv%lencv/=ycv%lencv) then
     write(6,*)'axpy: error length',xcv%lencv,ycv%lencv
     call stop2(115)
  end if

  DO ii=1,ycv%lencv
     ycv%values(ii) = ycv%values(ii) + alpha * xcv%values(ii)
  ENDDO

  return
end subroutine axpy
! ----------------------------------------------------------------------
subroutine random_cv(ycv,kseed)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    random_cv
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    ycv
!    kseed
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none
type(control_vector)     , intent(inout) :: ycv
integer(i_kind), optional, intent(in   ) :: kseed

integer(i_kind):: ii,jj,iseed
integer, allocatable :: nseed(:) ! Intentionaly default integer
real(r_kind), allocatable :: zz(:)

iseed=iadatebgn
if (present(kseed)) iseed=iseed+kseed
call random_seed(size=jj)
allocate(nseed(jj))
nseed(1:jj)=iseed
! The following because we don't want all procs to get
! exactly the same sequence (which would be repeated in
! the then not so random vector) but it makes the test
! not reproducible if the number of procs is changed.
nseed(1)=iseed+mype
call random_seed(put=nseed)
deallocate(nseed)

allocate(zz(nval_len))
do jj=1,nsubwin
   call random_number(zz)
   do ii=1,nval_len
      ycv%step(jj)%values(ii) = two*zz(ii)-one
   enddo
enddo
deallocate(zz)

if (nsclen>izero) then
   allocate(zz(nsclen))
   call random_number(zz)
   do ii=1,nsclen
      ycv%predr(ii) = two*zz(ii)-one
   enddo
   deallocate(zz)
endif

if (npclen>izero) then
   allocate(zz(npclen))
   call random_number(zz)
   do ii=1,npclen
      ycv%predp(ii) = two*zz(ii)-one
   enddo
   deallocate(zz)
endif

return
end subroutine random_cv
! ----------------------------------------------------------------------
subroutine write_cv(xcv,cdfile)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    write_cv
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    xcv
!    cdfile
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  type(control_vector), intent(in   ) :: xcv
  character(len=*)    , intent(in   ) :: cdfile

  character(len=100) :: clfile
  character(len=5) :: clmype
  integer(i_kind):: iunit

  iunit=get_lun()
  clmype='.YYYY'
  write(clmype(2:5),'(I4.4)')mype
  clfile=trim(cdfile)//clmype
  if (mype==izero) write(6,*)'Writing control vector to file ',clfile

  open(iunit,file=trim(clfile),form='unformatted')
  write(iunit)xcv%lencv
  write(iunit)xcv%values(1:xcv%lencv)
  close(iunit)

  return
end subroutine write_cv
! ----------------------------------------------------------------------
subroutine read_cv(xcv,cdfile)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_cv
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    xcv
!    cdfile
!
!   output argument list:
!    xcv
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  type(control_vector), intent(inout) :: xcv
  character(len=*)    , intent(in   ) :: cdfile

  character(len=100) :: clfile
  character(len=5) :: clmype
  integer(i_kind):: iunit,ilen

  iunit=get_lun()
  clmype='.YYYY'
  write(clmype(2:5),'(I4.4)')mype
  clfile=trim(cdfile)//clmype
  if (mype==izero) write(6,*)'Reading control vector from file ',clfile

  open(iunit,file=trim(clfile),form='unformatted')
  read(iunit)ilen
  if (ilen/=xcv%lencv) then
     write(6,*)'read_cv: wrong length',ilen,xcv%lencv
     call stop2(116)
  end if
  read(iunit)xcv%values(1:xcv%lencv)
  close(iunit)

  return
end subroutine read_cv
! ----------------------------------------------------------------------
subroutine inquire_cv
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inquire_cv
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
implicit none
real(r_kind) :: zz

if (mype==izero) then
   zz=real(max_vec_alloc*nclen,r_kind)*8.0_r_kind/1.048e6_r_kind
   write(6,*)'control_vectors: length=',nclen
   write(6,*)'control_vectors: currently allocated=',m_vec_alloc
   write(6,*)'control_vectors: maximum allocated=',max_vec_alloc
   write(6,*)'control_vectors: number of allocates=',m_allocs
   write(6,*)'control_vectors: number of deallocates=',m_deallocs
   write(6,'(A,F8.1,A)')'control_vectors: Estimated max memory used= ',zz,' Mb'
endif

end subroutine inquire_cv
! ----------------------------------------------------------------------
real(r_kind) function maxval_cv(ycv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    maxval_cv
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    ycv
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none
type(control_vector), intent(in   ) :: ycv
real(r_kind) :: zloc(1),zglo(1)

zloc(1)=maxval(ycv%values(:))

call mpi_allreduce(zloc,zglo,ione,mpi_rtype,mpi_max,mpi_comm_world,ierror)
if (ierror/=izero) then
   write(6,*)'maxval_cv: MPI error',ierror
   call stop2(117)
end if

maxval_cv=zglo(1)

return
end function maxval_cv
! ----------------------------------------------------------------------
real(r_quad) function qdot_product(x,y)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    qdot_product
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    x,y
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  real(r_kind),intent(in   ) :: x(:),y(:)
  real(r_quad):: zz
  integer(i_kind) :: nx,ny,i
  nx=size(x)
  ny=size(y)
  if(nx/=ny) then
     write(6,*)'qdot_product: inconsistent dims',nx,ny
     call stop2(118)
  end if
  zz=zero_quad

  do i=1,nx
     zz = zz + x(i)*y(i)
  enddo
  qdot_product=zz
end function qdot_product
! ----------------------------------------------------------------------
end module control_vectors
