module jfunc
!$$$   module documentation block
!                .      .    .                                       .
! module:    jfunc
!   prgmmr: treadon          org: np23                date: 2003-11-24
!
! abstract: module containing variables used in inner loop minimzation
!
! program history log:
!   2003-11-24  treadon
!   2004-05-18  kleist, documentation
!   2004-10-06  kleist, create separate control vector for u,v
!   2004-10-15  parrish, add outer iteration variable for nonlinear qc
!   2004-12-23  treadon - add logical flags first and last
!   2005-02-23  wu - add qoption, dqdt,dqdrh,dqdp and varq for norm RH
!   2005-03-28  wu - replace mlath with mlat             
!   2005-06-03  parrish - add logical switch_on_derivatives
!   2005-09-29  kleist - add pointers for time derivatives
!   2005-11-21  kleist - expand time deriv pointers for tracer tendencies
!   2005-11-29  derber - fix bug in restart
!   2006-02-02  treadon - remove prsi_oz (use ges_prsi array)
!
! Subroutines Included:
!   init_jfunc           - set defaults for cost function variables
!   create_jfunc         - allocate cost function arrays 
!   destroy_jfunc        - deallocate cost function arrays
!   read_guess_solution  - read guess solution
!   write_guess_solution - write guess solution
!   strip2               - strip off halo from subdomain arrays
!   set_pointer          - set location indices for components of vectors
!
! remarks: variable definitions below
!   def first      - logical flag = .true. on first outer iteration
!   def last       - logical flag = .true. following last outer iteration
!   def switch_on_derivatives - .t. = generate horizontal derivatives
!   def iout_iter  - output file number for iteration information
!   def miter      - number of outer iterations
!   def qoption    - option of q analysis variable; 1:q/qsatg 2:norm RH
!   def iguess     - flag for guess solution
!   def biascor    - background error bias correction coefficient 
!   def niter      - number of inner interations (for each other iter.)
!   def niter_no_qc- number of inner interations without nonlinear qc (for each outer iter.)
!   def jiter      - outer iteration counter
!   def jiterstart - first outloop iteration number
!   def iter       - do loop iteration integer
!   def nclen      - length of control (x,y) vectors
!   dev nuvlen     - length of special control vector for u,v
!   dev ntendlen   - length of special control vector for ut,vt,tt,pst (tlm time tendencies)
!   def nval_levs  - number of 2d (x/y) variables
!   def nval_len   - number of 2d variables * subdomain size (with buffer)
!   def nstsm      - starting point for streamfunction in control vector for comm.
!                    from here on down, without buffer points
!   def nvpsm      - starting point for velocity pot. in control vector for comm.
!   def npsm       - starting point for ln(ps) in control vector for comm.
!   def ntsm       - starting point for temperature in control vector for comm.
!   def nqsm       - starting point for moisture in control vector for comm.
!   def nozsm      - starting point for ozone in control vector for comm.
!   def nsstsm     - starting point for sst in control vector for comm.
!   def nsltsm     - starting point for skin/land temp. in control vector for comm.
!   def nsitsm     - starting point for skin/ice temp. in control vector for comm.
!   def ncwsm      - starting point for cloud water in control vector for comm.
!   def nst2       - starting point for streamfunction in control vector for comm.
!                    from here on down, including buffer points
!   def nvp2       - starting point for velocity pot. in control vector for comm.
!   def np2        - starting point for ln(ps) in control vector for comm.
!   def nt2        - starting point for temperature in control vector for comm.
!   def nq2        - starting point for moisture in control vector for comm.
!   def noz2       - starting point for ozone in control vector for comm.
!   def nsst2      - starting point for sst in control vector for comm.
!   def nslt2      - starting point for skin/land temp. in control vector for comm.
!   def nsit2      - starting point for skin/ice temp. in control vector for comm.
!   def ncw2       - starting point for cloud water in control vector for comm.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind

  logical first,last,switch_on_derivatives,tendsflag
  integer(i_kind) iout_iter,miter,iguess,nclen,jiter,jiterstart,iter,qoption
  integer(i_kind) nt,nq,noz,ncw,np,nsst,nst,nvp,nu,nv,nval_len,nval_levs
  integer(i_kind) nut,nvt,ntt,nprst,nqt,nozt,ncwt,ndivt,nagvt
  integer(i_kind) nstsm,nvpsm,npsm,ntsm,nqsm,nozsm,nsstsm,nsltsm,nsitsm,ncwsm
  integer(i_kind) nst2,nvp2,np2,nt2,nq2,noz2,nsst2,nslt2,nsit2,ncw2
  integer(i_kind) nclen1,nclen2,nrclen,nsclen,npclen,nuvlen,ntendlen
  integer(i_kind),dimension(0:50):: niter,niter_no_qc
  real(r_kind) factqmax,factqmin,gnormorig,penorig,biascor
  real(r_kind),allocatable,dimension(:,:,:):: qsatg,qsinv2,qgues,dqdt,dqdrh,dqdp
  real(r_kind),allocatable,dimension(:,:):: varq
  real(r_kind),allocatable,dimension(:):: xhatsave,yhatsave,xhatsave_r,yhatsave_r

contains

  subroutine init_jfunc
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_jfunc
!   prgmmr: treadon          org: np23               date:  2003-11-24
!
! abstract: initialize cost function variables to defaults
!
! program history log:
!   2003-11-24  treadon
!   2004-05-18  kleist, documentation
!   2004-12-23  treadon - initialize first and last
!   2005-06-03  parrish - initialize switch_on_derivatives
!   2005-10-27  kleist  - initialize tendency flag
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
    use constants, only: zero, one
    implicit none
    integer(i_kind) i

    first = .true.
    last  = .false.
    switch_on_derivatives=.false.
    tendsflag=.false.

    factqmin=one
    factqmax=one
    iout_iter=220
    miter=1
    qoption=1
    do i=0,50
      niter(i)=0
      niter_no_qc(i)=1000000
    end do
    jiterstart=1
    jiter=jiterstart
    biascor=-one
    nclen=1
    nuvlen=1
    ntendlen=1

    penorig=zero
    gnormorig=zero

! iguess = -1  do not use guess file
! iguess =  0  write only guess file
! iguess =  1  read and write guess file
! iguess =  2  read only guess file

    iguess=1  

    return
  end subroutine init_jfunc

  subroutine create_jfunc
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_jfunc
!   prgmmr: treadon          org: np23               date:  2003-11-24
!
! abstract: allocate memory for cost function variables
!
! program history log:
!   2003-11-24  treadon
!   2004-05-18  kleist, documentation
!   2004-07-28  treadon - simplify subroutine argument list
!   2005-03-28  wu - replace mlath with mlat, modify dim of varq 
!   2005-06-15  treadon - remove "use guess_grids"
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
    use constants, only: zero
    use gridmod, only: lat2,lon2,nsig
    implicit none

    integer(i_kind) i,j,k,l
    integer(i_kind) inerr,msig,mlat

! Read dimension of stats file
  inerr=22
  open(inerr,file='berror_stats',form='unformatted')
  rewind inerr
  read(inerr)msig,mlat
  close(inerr)

    allocate(xhatsave(nclen),yhatsave(nclen))
    allocate(qsatg(lat2,lon2,nsig),&
         dqdt(lat2,lon2,nsig),dqdrh(lat2,lon2,nsig),&
         varq(1:mlat,1:nsig),dqdp(lat2,lon2,nsig),&
         qsinv2(lat2,lon2,nsig),qgues(lat2,lon2,nsig))

    do i=1,nclen
      xhatsave(i)=zero
      yhatsave(i)=zero
    end do

    if (iguess>0) then
       allocate(xhatsave_r(nclen),yhatsave_r(nclen))
       do i=1,nclen
          xhatsave_r(i)=zero
          yhatsave_r(i)=zero
       end do
    endif
    
    do k=1,nsig
       do j=1,mlat
         varq(j,k)=zero
       end do
    end do

    do k=1,nsig
       do j=1,lon2
          do i=1,lat2
             qsatg(i,j,k)=zero
             dqdt(i,j,k)=zero
             dqdrh(i,j,k)=zero
             dqdp(i,j,k)=zero
             qgues(i,j,k)=zero
             qsinv2(i,j,k)=zero
          end do
       end do
    end do

    return
  end subroutine create_jfunc
    
  subroutine destroy_jfunc
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_jfunc
!   prgmmr: treadon          org: np23               date:  2003-11-24
!
! abstract: deallocate memory from cost function variables
!
! program history log:
!   2003-11-24  treadon
!   2004-05-18  kleist, documentation
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
    deallocate(xhatsave,yhatsave,varq)
    deallocate(dqdt,dqdrh,dqdp,qsatg,qgues,qsinv2)

! NOTE:  xhatsave_r and yhatsave_r are deallocated in
!        pcgsoi following transfer of their contents
!        to xhatsave and yhatsave.  The deallocate is
!        releases this memory since it is no longer needed.

    return
  end subroutine destroy_jfunc

  subroutine read_guess_solution(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_guess_solution
!   prgmmr: treadon          org: np23               date:  2003-11-24
!
! abstract: read in guess solution
!
! program history log:
!   2003-11-24  treadon
!   2004-05-18  kleist, documentation
!   2005-05-05  treadon - read guess solution from 4-byte reals
!
!   input argument list:
!     mype   - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use kinds, only: r_kind,i_kind,r_single
    use mpimod, only: ierror, mpi_comm_world, mpi_rtype,mpi_real4
    use gridmod, only: nlat,nlon,nsig,itotsub,ltosi_s,ltosj_s,&
         displs_s,ijn_s,latlon11,iglobal
    use obsmod, only: iadate
    implicit none

    integer(i_kind),intent(in):: mype

    integer(i_kind) i,j,k,mm1,myper,kk,i1,i2
    integer(i_kind) nlatg,nlong,nsigg,mypeg
    integer(i_kind),dimension(5):: iadateg
    real(r_single),dimension(max(iglobal,itotsub)):: fieldx,fieldy
    real(r_single),dimension(nlat,nlon):: xhatsave_g,yhatsave_g
    real(r_single),dimension(nclen):: xhatsave_r4,yhatsave_r4
    
    jiterstart = 1
    mm1=mype+1
    myper=0

! Open unit to guess solution.  Read header.  If no header, file is
! empty and exit routine
    open(12,file='gesfile_in',form='unformatted')
    iadateg=0
    nlatg=0
    nlong=0
    nsigg=0
    read(12,end=1234)iadateg,nlatg,nlong,nsigg
    if(iadate(1) == iadateg(1) .and. iadate(2) == iadate(2) .and. &
          iadate(3) == iadateg(3) .and. iadate(4) == iadateg(4) .and. &
          iadate(5) == iadateg(5) .and. nlat == nlatg .and. &
          nlon == nlong .and. nsig == nsigg) then
      if(mype == 0) write(6,*)'READ_GUESS_SOLUTION:  read guess solution for ',&
                    iadateg,nlatg,nlong,nsigg
      jiterstart=0
         
! Let all tasks read gesfile_in to pick up bias correction (second read)

! Loop to read input guess fields.  After reading in each field & level,
! scatter the grid to the appropriate location in the xhat and yhatsave
! arrays.
      do k=1,nval_levs
        read(12,end=1236) xhatsave_g,yhatsave_g
        do kk=1,itotsub
          i1=ltosi_s(kk); i2=ltosj_s(kk)
          fieldx(kk)=xhatsave_g(i1,i2)
          fieldy(kk)=yhatsave_g(i1,i2)
        end do
        i=(k-1)*latlon11 + 1
        call mpi_scatterv(fieldx,ijn_s,displs_s,mpi_real4,&
                 xhatsave_r4(i),ijn_s(mm1),mpi_real4,myper,mpi_comm_world,ierror)
        call mpi_scatterv(fieldy,ijn_s,displs_s,mpi_real4,&
                 yhatsave_r4(i),ijn_s(mm1),mpi_real4,myper,mpi_comm_world,ierror)
      end do  !end do over nval_levs

!     Read radiance and precipitation bias correction terms
      read(12,end=1236) (xhatsave_r4(i),i=nclen1+1,nclen),(yhatsave_r4(i),i=nclen1+1,nclen)
      do i=1,nclen
         xhatsave_r(i)=xhatsave_r4(i)
         yhatsave_r(i)=yhatsave_r4(i)
      end do
         
    else
      if(mype == 0) then
        write(6,*) 'READ_GUESS_SOLUTION:  INCOMPATABLE GUESS FILE, gesfile_in'
        write(6,*) 'READ_GUESS_SOLUTION:  iguess,iadate,iadateg=',iguess,iadate,iadateg
        write(6,*) 'READ_GUESS_SOLUTION:  nlat,nlatg,nlon,nlong,nsig,nsigg=',&
                    nlat,nlatg,nlon,nlong,nsig,nsigg
      end if
    endif
    close(12)
    return

! The guess file is empty.  Do not return an error code but print a message to
! standard out.
1234  continue
    if(mype == 0) then
      write(6,*) 'READ_GUESS_SOLUTION:  NO GUESS FILE, gesfile_in'
      write(6,*) 'READ_GUESS_SOLUTION:  iguess,iadate,iadateg=',iguess,iadate,iadateg
      write(6,*) 'READ_GUESS_SOLUTION:  nlat,nlatg,nlon,nlong,nsig,nsigg=',&
                  nlat,nlatg,nlon,nlong,nsig,nsigg
    end if
    close(12)
    return

! Error contition reading level or bias correction data.  Set error flag and
! return to the calling program.
1236  continue
    if (mype==0) write(6,*) 'READ_GUESS_SOLUTION:  ERROR in reading guess'
    close(12)
    call stop2(76)

    return
  end subroutine read_guess_solution
  
  subroutine write_guess_solution(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    write_guess_solution
!   prgmmr: treadon          org: np23               date:  2003-11-24
!
! abstract: write out guess solution (not from spectral forecast)
!
! program history log:
!   2003-11-24  treadon
!   2004-05-18  kleist, documentation
!   2005-03-10  treadon - remove iadate from calling list, access via obsmod
!   2005-05-05  treadon - write guess solution using 4-byte reals
!
!   input argument list:
!     mype   - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use kinds, only: r_kind,i_kind,r_single
    use mpimod, only: ierror, mpi_comm_world, mpi_rtype,mpi_real4
    use gridmod, only: ijn,latlon11,displs_g,ltosj,ltosi,nsig,&
         nlat,nlon,lat1,lon1,itotsub,iglobal
    use obsmod, only: iadate
    use constants, only: zero
    implicit none

    character(11) gesfile
    integer(i_kind),intent(in):: mype

    integer(i_kind) i,j,k,mm1,mypew,kk,i1,i2
    real(r_single),dimension(lat1,lon1,2):: field
    real(r_single),dimension(max(iglobal,itotsub)):: fieldx,fieldy
    real(r_single),dimension(nlat,nlon):: xhatsave_g,yhatsave_g
    real(r_single),dimension(nrclen):: xhatsave4,yhatsave4

    mm1=mype+1
    mypew=0
    
! Write header record to output file
    if (mype==mypew) then
      open(51,file='gesfile_out',form='unformatted')
      write(51) iadate,nlat,nlon,nsig
    endif

! Loop over levels.  Gather guess solution and write to output
    do k=1,nval_levs
      i=(k-1)*latlon11 + 1
      call strip2(xhatsave(i),yhatsave(i),field)
      call mpi_gatherv(field(1,1,1),ijn(mm1),mpi_real4,&
           fieldx,ijn,displs_g,mpi_real4,mypew,&
           mpi_comm_world,ierror)
      call mpi_gatherv(field(1,1,2),ijn(mm1),mpi_real4,&
           fieldy,ijn,displs_g,mpi_real4,mypew,&
           mpi_comm_world,ierror)


! Transfer to global arrays
      do j=1,nlon
        do i=1,nlat
          xhatsave_g(i,j)=zero
          yhatsave_g(i,j)=zero
        end do
      end do
      do kk=1,iglobal
        i1=ltosi(kk); i2=ltosj(kk)
        xhatsave_g(i1,i2)=fieldx(kk)
        yhatsave_g(i1,i2)=fieldy(kk)
      end do

! Write level record
      if (mype==mypew) write(51) xhatsave_g,yhatsave_g
    end do  !end do over nval_levs

! Write radiance and precipitation bias correction terms to output file
    if (mype==mypew) then
       do i=1,nrclen
          xhatsave4(i)=xhatsave(nclen1+i)
          yhatsave4(i)=yhatsave(nclen1+i)
       end do
      write(51) (xhatsave4(i),i=1,nrclen),(yhatsave4(i),i=1,nrclen)
      close(51)
      write(6,*)'WRITE_GUESS_SOLUTION:  write guess solution for ',&
                 iadate,nlat,nlon,nsig
    endif

    return
  end subroutine write_guess_solution

    subroutine strip2(field_in1,field_in2,field_out)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    strip2
!   prgmmr: treadon          org: np23                date: 2003-11-24
!
! abstract: strip off halo from two subdomain arrays & combine into
!           single output array
!
! program history log:
!   2003-11-24  treadon
!   2004-05-18  kleist, documentation
!
!   input argument list:
!     field_in1 - subdomain field one with halo
!     field_in2 - subdomain field two with halo
!
!   output argument list:
!     field_out - combined subdomain fields with halo stripped
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use kinds, only: r_kind,i_kind,r_single
    use gridmod, only: lat1,lon1,lat2,lon2
    implicit none

    integer(i_kind) i,j,jp1
    real(r_single),dimension(lat1,lon1,2):: field_out
    real(r_kind),dimension(lat2,lon2):: field_in1,field_in2

    do j=1,lon1
      jp1 = j+1
      do i=1,lat1
        field_out(i,j,1)=field_in1(i+1,jp1)
        field_out(i,j,2)=field_in2(i+1,jp1)
      end do
    end do

    return
  end subroutine strip2

  subroutine set_pointer
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    set_pointer
!   prgmmr: treadon          org: np23                date: 2004-07-28
!
! abstract: Set length of control vector and other control 
!           vector constants
!
! program history log:
!   2004-07-28  treadon
!   2006-04-21  kleist - include pointers for more time tendency arrays
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
    use kinds, only: r_kind
    use gridmod, only: lat1,lon1,latlon11,latlon1n,nsig
    use radinfo, only: npred,jpch
    use pcpinfo, only: npredp,jtype
    implicit none

    nval_levs=6*nsig+2
    nval_len=nval_levs*latlon11
    nsclen=npred*jpch
    npclen=npredp*jtype
    nclen=nval_len+nsclen+npclen
    nrclen=nsclen+npclen
    nclen1=nclen-nrclen
    nclen2=nclen1+nsclen
    nuvlen=2*latlon1n
    ntendlen=9*latlon1n+latlon11
  
    nst=1                                  ! streamfunction
    nvp=nst+latlon1n                       ! velocity potential
    nt=nvp +latlon1n                       ! t
    nq=nt  +latlon1n                       ! q
    noz=nq +latlon1n                       ! oz
    ncw=noz+latlon1n                       ! cloud water
    np=ncw +latlon1n                       ! surface pressure
    nsst=np+latlon11                       ! skin temperature

! Define pointers for isolated u,v on subdomains work vector
    nu=1                                   ! zonal wind
    nv=nu+latlon1n                         ! meridional wind

! Define pointers for isolated ut,vt,tt,qt,ozt,cwt,pst on subdomains work vector
    nut=1                                  ! zonal wind tend
    nvt=nut+latlon1n                       ! meridional wind tend
    ntt=nvt+latlon1n                       ! temperature tend
    nprst=ntt+latlon1n                     ! 3d-pressure tend (nsig+1 levs)
    nqt=nprst+latlon1n+latlon11            ! q tendency
    nozt=nqt+latlon1n                      ! ozone tendency
    ncwt=nozt+latlon1n                     ! cloud water tendency
    ndivt=ncwt+latlon1n                    ! divergence tendency
    nagvt=ndivt+latlon1n                   ! ageostrophic vorticity tendency

!   For new mpi communication, define vector starting points
!   for each variable type using the subdomains size without 
!   buffer points
    nstsm=1                                ! streamfunction small 
    nvpsm=nstsm  +(lat1*lon1*nsig)         ! vel. pot. small
    npsm=nvpsm   +(lat1*lon1*nsig)         ! sfc. p. small
    ntsm=npsm    +(lat1*lon1)              ! temp. small
    nqsm=ntsm    +(lat1*lon1*nsig)         ! q small
    nozsm=nqsm   +(lat1*lon1*nsig)         ! oz small
    nsstsm=nozsm +(lat1*lon1*nsig)         ! sst small
    nsltsm=nsstsm+(lat1*lon1)              ! land sfc. temp small
    nsitsm=nsltsm+(lat1*lon1)              ! ice sfc. temp small
    ncwsm=nsitsm +(lat1*lon1)              ! cloud water small
    
!   Define vector starting points for subdomains which include
!   buffer points
    nst2=1                               ! streamfunction mpi
    nvp2=nst2  +latlon1n                 ! vel pot mpi
    np2=nvp2   +latlon1n                 ! sfc p mpi
    nt2=np2    +latlon11                 ! temp mpi
    nq2=nt2    +latlon1n                 ! q mpi
    noz2=nq2   +latlon1n                 ! oz mpi
    nsst2=noz2 +latlon1n                 ! sst mpi
    nslt2=nsst2+latlon11                 ! sfc land temp mpi
    nsit2=nslt2+latlon11                 ! ice sfc temp mpi
    ncw2=nsit2 +latlon11                 ! cloud water mpi

  end subroutine set_pointer
end module jfunc
