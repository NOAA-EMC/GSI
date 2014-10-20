module comm_mod
  use kinds, only: r_kind,i_kind
  implicit none

  integer(i_kind) nsig1o
                                             ! comm. array, displacement ...
  integer(i_kind),allocatable,dimension(:):: isdsp_g !  for send to nsig1o slabs
  integer(i_kind),allocatable,dimension(:):: irdsp_g !  for receive from nsig1o slabs
  integer(i_kind),allocatable,dimension(:):: isdsp_s !  for send from nsig1o slabs
  integer(i_kind),allocatable,dimension(:):: irdsp_s !  for receive from nsig1o slabs

                                             ! comm. array, count ...
  integer(i_kind),allocatable,dimension(:):: iscnt_g !  for send to nsig1o slabs
  integer(i_kind),allocatable,dimension(:):: ircnt_g !  for receive from nsig1o slabs
  integer(i_kind),allocatable,dimension(:):: iscnt_s !  for send from nsig1o slabs
  integer(i_kind),allocatable,dimension(:):: ircnt_s !  for receive from nsig1o slabs

  integer(i_kind),allocatable,dimension(:):: spec_send !  for receive from nsig1o slabs
  integer(i_kind),allocatable,dimension(:):: disp_spec !  for receive from nsig1o slabs

  integer(i_kind),allocatable,dimension(:):: levs_id
  integer(i_kind),allocatable,dimension(:):: nvar_id


contains

  subroutine init_mpi_vars(nsig,mype)
    use variables, only: izero,ione,npe
    implicit none

    integer(i_kind),intent(in):: nsig    ! number of levels
    integer(i_kind),intent(in):: mype    ! task identifier

    integer n,vlevs,sfs,vps,tvs,rhs,ozs,cws,pss,&
         kk,kchk,mm1,k,varcnt

    allocate(iscnt_g(npe),isdsp_g(npe),ircnt_g(npe),&
       irdsp_g(npe),iscnt_s(npe),isdsp_s(npe),ircnt_s(npe),&
       irdsp_s(npe))

    allocate(spec_send(npe),disp_spec(npe))
    mm1=mype+ione

! Initialize slab/subdomain communicators, redefined in
! init_commvars
    do n=1,npe
      iscnt_g(n)   = izero
      isdsp_g(n)   = izero
      ircnt_g(n)   = izero
      irdsp_g(n)   = izero
      iscnt_s(n)   = izero
      isdsp_s(n)   = izero
      ircnt_s(n)   = izero
      irdsp_s(n)   = izero
      spec_send(n) = izero
      disp_spec(n) = izero
    end do

! Initialize nsig1o to distribute levs/variables
! as evenly as possible over the tasks
    vlevs=(6*nsig)+1
    nsig1o=vlevs/npe
    if(mod(vlevs,npe)/=0) nsig1o=nsig1o+1

! Allocate nsig1o identifiers
    allocate(levs_id(nsig1o),nvar_id(nsig1o))

! Distribute evenly over npe tasks
    sfs=1
    vps=sfs+nsig
    tvs=vps+nsig
    rhs=tvs+nsig
    ozs=rhs+nsig
    cws=ozs+nsig
    pss=cws+nsig

! Need to use a variable to know which tasks have a full nsig1o
! array, and which one have the last level irrelevant
    if (mod((6*nsig)+1,npe)==izero) then
      kchk=npe
    else
      kchk=mod((nsig*6)+1,npe)
    end if

    nvar_id=izero
    levs_id=izero
! Define which variable/level each task has for the
! global slabs (levs_id,nvar_id)
    varcnt=izero
    do n=1,npe
      if(n.le.kchk) then
        kk=nsig1o
      else
        kk=nsig1o-1
      end if
      do k=1,kk
        varcnt=varcnt+1
        if (n==mm1) then
          if (varcnt.lt.vps) then
            nvar_id(k)=1
            levs_id(k)=varcnt
          else if (varcnt.ge.vps .and. varcnt.lt.tvs) then
            nvar_id(k)=2
            levs_id(k)=varcnt-vps+1
          else if (varcnt.ge.tvs .and. varcnt.lt.rhs) then
            nvar_id(k)=3
            levs_id(k)=varcnt-tvs+1
          else if (varcnt.ge.rhs .and. varcnt.lt.ozs) then
            nvar_id(k)=4
            levs_id(k)=varcnt-rhs+1
          else if (varcnt.ge.ozs .and. varcnt.lt.cws) then
            nvar_id(k)=5
            levs_id(k)=varcnt-ozs+1
          else if (varcnt.ge.cws .and. varcnt.lt.pss) then
            nvar_id(k)=6
            levs_id(k)=varcnt-cws+1
          else
            nvar_id(k)=7
            levs_id(k)=1
          end if ! end if for varcnt
        end if ! end if for task id
      end do ! enddo over levs
    end do ! enddo over npe

!    do k=1,nsig1o
!      write(300+mype,*) 'COMM-MOD: k,nvar_id,levs_id = ',k,nvar_id(k),levs_id(k)
!    end do

    return
  end subroutine init_mpi_vars

  subroutine destroy_mpi_vars
    deallocate(iscnt_g,isdsp_g,ircnt_g,&
       irdsp_g,iscnt_s,isdsp_s,ircnt_s,&
       irdsp_s)
    deallocate(spec_send,disp_spec)
    deallocate(levs_id,nvar_id)
    return
  end subroutine destroy_mpi_vars

  subroutine reorder_post(work,k_in)
    use kinds, only: r_kind
    use variables, only: zero,ijn,iglobal,npe
    implicit none

    integer(i_kind), intent(in) ::  k_in    ! number of levs in work array
    real(r_kind),dimension(iglobal*k_in),intent(inout):: work ! array to reorder
    integer(i_kind) iloc,iskip,i,k,n
    real(r_kind),dimension(iglobal,k_in):: temp

! Zero out temp array
!    do k=1,k_in
!       do i=1,iglobal
!          temp(i,k)=zero
!       end do
!    end do

! Load temp array in desired order
    do k=1,k_in
      iskip=0
      iloc=0
      do n=1,npe
        if (n/=1) then
          iskip=iskip+ijn(n-1)*k_in
        end if
        do i=1,ijn(n)
          iloc=iloc+1
          temp(iloc,k)=work(i + iskip + (k-1)*ijn(n))
        end do
      end do
    end do

! Load the temp array back into work
    iloc=0
    do k=1,k_in
      do i=1,iglobal
        iloc=iloc+1
        work(iloc)=temp(i,k)
      end do
    end do

    return
  end subroutine reorder_post

  subroutine reorder_pre(work,k_in)
    use kinds, only: r_kind
    use variables, only: zero,ijn,iglobal,npe
    implicit none

    integer(i_kind), intent(in) ::  k_in    ! number of levs in work array

    real(r_kind),dimension(iglobal,k_in),intent(inout):: work

    integer(i_kind) iloc,iskip,i,k,n
    real(r_kind),dimension(iglobal*k_in):: temp

! Load temp array in order of subdomains
    iloc=0
    iskip=0
    do n=1,npe
      do k=1,k_in
        do i=1,ijn(n)
          iloc=iloc+1
          temp(iloc)=work(iskip+i,k)
        end do
      end do
      iskip=iskip+ijn(n)
    end do

! Now load the tmp array back into work
    iloc=0
    do k=1,k_in
      do i=1,iglobal
        iloc=iloc+1
        work(i,k)=temp(iloc)
      end do
    end do

    return
  end subroutine reorder_pre


  subroutine vectosub(fld_in,nz,fld_out)
    use kinds, only: r_kind
    use variables, only: lat1,lon1
    implicit none

    integer(i_kind), intent(in) ::  nz    ! number of levs in subdomain array
    real(r_kind),dimension(lat1*lon1*nz),intent(in):: fld_in ! subdomain array
                                                             !   in vector form

    real(r_kind),dimension(lat1,lon1,nz),intent(out):: fld_out ! three dimensional
                                                       !  subdomain variable array
!-------------------------------------------------------------------------

    integer(i_kind) i,j,k,iloc

    iloc=0
    do k=1,nz
      do j=1,lon1
        do i=1,lat1
          iloc=iloc+1
          fld_out(i,j,k)=fld_in(iloc)
        end do
      end do
    end do

    return
  end subroutine vectosub
 
  subroutine reload(work_in,work_out)
    use kinds, only: r_kind
    use variables, only: lat1,lon1,nsig
    implicit none

    real(r_kind),dimension(lat1*lon1,nsig),intent(in):: work_in   ! 2-d array
    real(r_kind),dimension(lat1,lon1,nsig),intent(out) :: work_out  ! 3-d array

    integer(i_kind) i,j,k,ij

    do k=1,nsig
       ij=0
       do j=1,lon1
          do i=1,lat1
            ij=ij+1
            work_out(i,j,k)=work_in(ij,k)
          end do
       end do
    end do
    return
  end subroutine reload

  subroutine sub2grid(workout,st,vp,tv,rh,oz,cw,ps)
    use kinds, only: r_kind,i_kind
    use variables, only: iglobal,lat1,lon1,nlat,nlon,nsig,&
         ltosi,ltosj,zero,db_prec
    implicit none
    include 'mpif.h'

! Passed variables
    real(r_kind),dimension(lat1,lon1,nsig),intent(in):: st,vp,tv,rh,oz,cw
    real(r_kind),dimension(lat1,lon1),intent(in):: ps
    real(r_kind),dimension(nlat,nlon,nsig1o),intent(out):: workout

! Declare local variables
    integer(i_kind) j,k,l,ni1,ni2,ierror,mpi_rtype,displ,i,npt
    integer(i_kind) sts,vps,tvs,rhs,ozs,cws,pss
    real(r_kind),dimension(lat1*lon1*(nsig*6+1)):: vector
    real(r_kind),dimension(iglobal,nsig1o):: work1  !  contain nsig1o slab of any variables

  if (db_prec) then
    mpi_rtype=mpi_real8
  else
    mpi_rtype=mpi_real4
  end if

! zero out work arrays
    do k=1,nsig1o
      do j=1,iglobal
        work1(j,k)=zero
      end do
    end do

! Load xhatsm with appropriate elements
    displ=lat1*lon1
    sts=1
    vps=sts+(displ*nsig)
    tvs=vps+(displ*nsig)
    rhs=tvs+(displ*nsig)
    ozs=rhs+(displ*nsig)
    cws=ozs+(displ*nsig)
    pss=cws+(displ*nsig)


    npt=0
    do k=1,nsig
      do j=1,lon1
        do i=1,lat1
          vector(sts+npt)=st(i,j,k)
          vector(vps+npt)=vp(i,j,k)
          vector(tvs+npt)=tv(i,j,k)
          vector(rhs+npt)=rh(i,j,k)
          vector(ozs+npt)=oz(i,j,k)
          vector(cws+npt)=cw(i,j,k)
          npt=npt+1
        end do
      end do
    end do

    npt=0
    do j=1,lon1
      do i=1,lat1
        vector(pss+npt)=ps(i,j)
        npt=npt+1
      end do
    end do

! send subdomain vector to global slabs
    call mpi_alltoallv(vector(1),iscnt_g,isdsp_g,mpi_rtype,&
         work1(1,1),ircnt_g,irdsp_g,mpi_rtype,&
         mpi_comm_world,ierror)

! reorder work1 array post communication
    call reorder_post(work1,nsig1o)

    do k=1,nsig1o
      do l=1,iglobal
        ni1=ltosi(l); ni2=ltosj(l)
        workout(ni1,ni2,k)=work1(l,k)
      end do
    end do

    return
  end subroutine sub2grid

  subroutine grid2sub(workin,st,vp,tv,rh,oz,cw,ps)
    use kinds, only: r_kind,i_kind
    use variables, only: iglobal,lat1,lon1,nlat,nlon,nsig,&
         ltosi,ltosj,zero,db_prec
    implicit none
    include 'mpif.h'

! Passed variables
    real(r_kind),dimension(nlat,nlon,nsig1o),intent(in):: workin
    real(r_kind),dimension(lat1,lon1,nsig),intent(out):: st,vp,tv,rh,oz,cw
    real(r_kind),dimension(lat1,lon1),intent(out):: ps

! Declare local variables
    integer(i_kind) j,k,l,ni1,ni2,ierror,mpi_rtype,i,npt
    integer(i_kind) sts,vps,tvs,rhs,ozs,cws,pss,displ

    real(r_kind),dimension(lat1*lon1*(nsig*6+1)):: vector
    real(r_kind),dimension(iglobal,nsig1o):: work1  !  contain nsig1o slab of any variables

    if (db_prec) then
      mpi_rtype=mpi_real8
    else
      mpi_rtype=mpi_real4
    end if

! Transfer input array to local work array
    do k=1,nsig1o
      do l=1,iglobal
        ni1=ltosi(l); ni2=ltosj(l)
        work1(l,k)=workin(ni1,ni2,k)
      end do
    end do

    call reorder_pre(work1,nsig1o)

! send global slabs to subdomains
    call mpi_alltoallv(work1(1,1),iscnt_s,isdsp_s,&
         mpi_rtype,vector(1),ircnt_s,irdsp_s,&
         mpi_rtype,mpi_comm_world,ierror)

! Define start point of array for each variable
    displ=lat1*lon1
    sts=1
    vps=sts+(displ*nsig)
    tvs=vps+(displ*nsig)
    rhs=tvs+(displ*nsig)
    ozs=rhs+(displ*nsig)
    cws=ozs+(displ*nsig)
    pss=cws+(displ*nsig)

! load the received subdomain vector
    call vectosub(vector(sts),nsig,st)
    call vectosub(vector(vps),nsig,vp)
    call vectosub(vector(tvs),nsig,tv)
    call vectosub(vector(rhs),nsig,rh)
    call vectosub(vector(ozs),nsig,oz)
    call vectosub(vector(cws),nsig,cw)
    call vectosub(vector(pss),1,ps)

    return
  end subroutine grid2sub

end module comm_mod
