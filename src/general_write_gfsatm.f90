  subroutine general_write_gfsatm(grd,sp_a,sp_b,filename,mype,mype_out,sub_z,sub_ps,&
       sub_vor,sub_div,sub_tv,sub_q,sub_oz,sub_cwmr,ibin,inithead,iret_write)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_write_gfsatm  adaptation of write_gfsatm for general resolutions
!   prgmmr: parrish          org: np22                date: 1990-10-10
!
! abstract: copied from write_gfsatm, primarily for writing in gefs sigma files, where the
!            input resolution and the grid that variables are reconstructed on can be
!            different from the analysis grid/resolution.
!
! program history log:
!   2010-02-25  parrish
!   2010-03-29  todling - add prologue; load_grid now in commvars
!   2014-12-03  derber - simplify if structure and use guess surface height
!               directly
!
!   input argument list:
!
!     inithead - logical to read header record.  Usually .true. unless
!                repeatedly reading similar files(e.g., ensembles)

!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

    use kinds, only: r_kind,i_kind,r_single
    use sigio_r_module, only: sigio_dbti,sigio_rropen,sigio_rrhead,sigio_rwhead,&
        sigio_rrdbti,sigio_rwdbti,sigio_rwopen,sigio_rclose,sigio_aldbti
    use sigio_module, only: sigio_head,sigio_alhead
    use general_sub2grid_mod, only: sub2grid_info
    use guess_grids, only: ntguessig,ifilesig
    use obsmod, only: iadate
    use mpimod, only: npe
    use general_specmod, only: spec_vars
    use gridmod, only: ntracer,ncepgfs_head,idpsfc5,idthrm5,cp5,idvc5,idvm5
    use general_commvars_mod, only: load_grid
    use ncepgfs_io, only: sigio_cnvtdv8,sighead
    use constants, only: zero,zero_single,one,fv
    use gsi_4dvar, only: ibdate,nhr_obsbin,lwrite4danl
    implicit none

! !INPUT PARAMETERS:
    character(*)                           ,intent(in   ) :: filename     ! file to open and write to
    integer(i_kind)                        ,intent(in   ) :: mype,mype_out      ! mpi task number

    type(sub2grid_info)                    ,intent(in   ) :: grd
    type(spec_vars)                        ,intent(in   ) :: sp_a,sp_b

    real(r_kind),dimension(grd%lat2,grd%lon2)      ,intent(in   ) :: sub_z, sub_ps  !2d
    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig) ,intent(in   ) :: sub_vor,sub_div,sub_tv,sub_q,sub_oz, &
                                                             sub_cwmr
    logical,                                intent(in   ) :: inithead

    integer(i_kind), intent(in)::   ibin
    integer(i_kind), intent(out)::  iret_write

    integer(i_kind),parameter::  lunges = 11
    integer(i_kind),parameter::  lunanl = 51

    character(6):: fname_ges

    real(r_kind),dimension(grd%itotsub):: work
    real(r_kind),dimension(grd%nlon,grd%nlat-2):: grid,grid2
    real(r_kind),dimension(grd%lat2,grd%lon2):: work_ps
    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig):: work_tv

    real(r_kind),dimension(sp_b%nc):: spec_work
    real(r_kind),dimension(sp_a%nc):: spec_work_sm
    real(r_kind),dimension(sp_b%nc),target ::  specges_4

    integer nlatm2,icount,itotflds,i,j,iret,kvar,klev,k,l,n,ks1,ks2
    integer(i_kind),dimension(npe)::ilev,ivar
    integer(i_kind),dimension(5):: mydate

    integer(i_kind),dimension(8) :: ida,jda
    real(r_kind),dimension(5)    :: fha

    type(sigio_dbti):: sigdati

    logical lloop

!*************************************************************************
!   Initialize local variables
    iret_write=0
    nlatm2=grd%nlat-2
    itotflds=6*grd%nsig+2  ! Hardwired for now!  vor,div,tv,q,oz,cwmr,ps,z
    lloop=.true.
 
!   Set guess file name
    write(fname_ges,100) ifilesig(ntguessig)
100    format('sigf',i2.2)
!   Handle case of NCEP SIGIO

    i=1
! Have all files open ges and read header for now with RanRead
    if(mype < itotflds .or. mype == mype_out)then
      call sigio_rropen(lunges,fname_ges,iret)
      if(inithead)call sigio_rrhead(lunges,sighead,iret)

! All tasks should also open output file for random write
      call sigio_rwopen(lunanl,filename,iret_write)
      if (iret_write /=0) goto 1000
    end if

    if (mype==mype_out) then
! Load date
       if (.not.lwrite4danl) then
          mydate=iadate
       else
!  increment mydate ...                                                                                                                              
          mydate=ibdate
          fha(:)=zero ; ida=0; jda=0
          fha(2)=real(nhr_obsbin*(ibin-1))  ! relative time interval in hours
          ida(1)=mydate(1) ! year
          ida(2)=mydate(2) ! month
          ida(3)=mydate(3) ! day
          ida(4)=0         ! time zone
          ida(5)=mydate(4) ! hour

   ! Move date-time forward by nhr_assimilation hours
          call w3movdat(fha,ida,jda)
          mydate(1)=jda(1)
          mydate(2)=jda(2)
          mydate(3)=jda(3)
          mydate(4)=jda(5)
       end if

!    if (mype==mype_out) then
!      Replace header record date with analysis time
       sighead%fhour    = zero_single
       sighead%idate(1) = mydate(4) !hour
       sighead%idate(2) = mydate(2) !month
       sighead%idate(3) = mydate(3) !day
       sighead%idate(4) = mydate(1) !year

!      Load grid dimension and other variables used below
!      into local header structure

!      Write header to analysis file
       call sigio_rwhead(lunanl,sighead,iret)
       iret_write=iret_write+iret
    end if

!   Surface pressure.
!   NCEP SIGIO has two options for surface pressure.  Variable idpsfc5
!   indicates the type:
!      idpsfc5= 0,1 for ln(psfc)
!      idpsfc5= 2 for psfc
    work_ps=sub_ps
!   If output ln(ps), take log of ps in cb
    if (idpsfc5 /= 2) then
       do j=1,grd%lon2
          do i=1,grd%lat2
             if(work_ps(i,j)<=zero)then
                work_ps(i,j)=one
             end if
             work_ps(i,j)=log(work_ps(i,j))
          end do
       end do
    endif

!   Thermodynamic variable
!   The GSI analysis variable is virtual temperature (Tv).  For SIGIO
!   we have three possibilities:  Tv, sensible temperature (T), or
!   enthalpy (h=CpT).  Variable idthrm5 indicates the type
!       idthrm5 = 0,1 = virtual temperature (Tv)
!       idthrm5 = 2   = sensible (dry) temperature (T)
!       idthrm5 = 3   = enthalpy (h=CpT)
    
    work_tv=sub_tv
    if (idthrm5==2 .or. idthrm5==3) then

!      Convert virtual temperature to dry temperature
       do k=1,grd%nsig
          do j=1,grd%lon2
             do i=1,grd%lat2
                work_tv(i,j,k)=work_tv(i,j,k)/(one+fv*sub_q(i,j,k))
             end do
          end do
       end do

!      If output is enthalpy, convert dry temperature to CpT
       if (idthrm5==3) call sigio_cnvtdv8(grd%lat2*grd%lon2,&
            grd%lat2*grd%lon2,grd%nsig,idvc5,idvm5,ntracer,&
            iret,work_tv,sub_q,cp5,-1)
    endif


! Do loop until total fields have been processed.  Stop condition on itotflds

    icount=0
    gfsfields:  do while (lloop)

! First, perform sub2grid for up to npe
       call general_gather(grd,work_ps,work_tv,sub_vor,sub_div,sub_q,sub_oz,&
              sub_cwmr,icount,ivar,ilev,work)

       do k=1,npe  ! loop over pe distributed data
          kvar=ivar(k)

          if(mype == k-1 .and. kvar > 0)then
! HS
            klev=ilev(k)
            if ( kvar==1) then
               sigdati%i = 1                                        ! hs
! PS
            else if ( kvar==2 ) then
               sigdati%i = 2                                        ! ps
! TV
            else if ( kvar==3 ) then
               sigdati%i = 2+klev                                   ! temperature
!  Z
            else if ( kvar==4 ) then
               sigdati%i = sighead%levs + 2 + (klev-1) * 2 + 2      ! vorticity
!  D
            else if ( kvar==5 ) then
               sigdati%i = sighead%levs + 2 + (klev-1) * 2 + 1      ! divergence
!  Q
            else if ( kvar==6 ) then
               sigdati%i = sighead%levs * (2+1) + 2 + klev          ! q
! OZ
            else if ( kvar==7 ) then
               sigdati%i = sighead%levs * (2+2) + 2 + klev          ! oz
! CW
            else if ( kvar==8 ) then
               sigdati%i = sighead%levs * (2+3) + 2 + klev       ! cw, 3rd tracer
            end if

            if ( klev>0 ) then
                sigdati%f => specges_4
!    Read in full resolution guess spectral coefficients
                call sigio_rrdbti(lunges,sighead,sigdati,iret)
                do i=1,sp_b%nc 
                    spec_work(i) = specges_4(i) 
                end do 
!    Ensure coefficients that must be zero are zero 
                do i=1,sp_b%nc 
                  if(sp_b%factsml(i))spec_work(i)=zero 
                end do 
                if(kvar /= 1)then                      ! if sfc elevation field just write out
!    Put current analysis on 2d (full level) grid
                  call load_grid(work,grid)
!   Convert full resolution guess to analysis grid
                  call general_sptez_s_b(sp_a,sp_b,spec_work,grid2,1)
!   Calculation grid increment on analysis grid
                  grid=grid-grid2
!   Convert grid increment to spectral space
                  call general_sptez_s(sp_a,spec_work_sm,grid,-1)
!   Add increment in spectral space (possibly lower resolution) to guess (taken
!   from sppad) 
                  do l=0,min(sp_b%jcap,sp_a%jcap) 
                     do n=l,min(sp_b%jcap,sp_a%jcap) 
                       ks2=l*(2*sp_b%jcap+1-l)+2*n 
                       ks1=l*(2*sp_a%jcap+1-l)+2*n 
                       specges_4(ks2+1)=specges_4(ks2+1)+spec_work_sm(ks1+1) 
                       specges_4(ks2+2)=specges_4(ks2+2)+spec_work_sm(ks1+2) 
                     end do 
                  end do 
                  do i=1,sp_b%nc
                     specges_4(i)=specges_4(i)+spec_work(i)
                  end do
                  if (kvar/=4 .and. kvar/=5) then
                     do i=1,sp_b%nc
                        if(sp_b%factsml(i))specges_4(i)=zero_single
                     end do
                  else
                     do i=1,sp_b%nc
                        if(sp_b%factvml(i))specges_4(i)=zero_single
                     end do
                  endif
                end if


! Write out using RanWrite
                call sigio_rwdbti(lunanl,sighead,sigdati,iret)
                iret_write=iret_write+iret

            endif ! end if pe and ivar check
          end if

       end do  !end do over pes

       if (icount>itotflds) then
          lloop=.false.
          exit gfsfields
       end if

    end do gfsfields

!   Print date/time stamp
    if (mype==mype_out) then
       write(6,700) sighead%jcap,grd%nlon,nlatm2,sighead%levs,&
            sighead%fhour,sighead%idate
700    format('GENERAL_WRITE_GFSATM:  anl write, jcap,lonb,latb,levs=',&
            4i6,', hour=',f10.1,', idate=',4i5)
    endif

    if(mype < itotflds .or. mype == mype_out)then
       call sigio_rclose(lunges,iret)
       call sigio_rclose(lunanl,iret)
       iret_write=iret_write+iret
       if (iret_write /=0) goto 1000
    end if
    return


!   ERROR detected while reading file
1000 continue
     write(6,*)'GENERAL_WRITE_GFSATM:  ***ERROR*** writing ',&
         trim(filename),' mype,iret_write=',mype,iret_write
     return

end subroutine general_write_gfsatm

  subroutine general_write_gfsatm_nems(grd,sp_a,filename,mype,mype_out,sub_ps,&
       sub_u,sub_v,sub_tv,sub_q,sub_oz,sub_cwmr,sub_prsl,sub_prsi,ibin,inithead,iret_write)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_write_gfsatm  adaptation of write_gfsatm for general resolutions
!   prgmmr: parrish          org: np22                date: 1990-10-10
!
! abstract: copied from write_gfsatm, primarily for writing in gefs sigma files, where the
!            input resolution and the grid that variables are reconstructed on can be
!            different from the analysis grid/resolution.
!
! program history log:
!   2010-02-25  parrish
!   2010-03-29  todling - add prologue; load_grid now in commvars
!   2014-12-03  derber - simplify if structure and use guess surface height
!               directly
!
!   input argument list:
!
!     inithead - logical to read header record.  Usually .true. unless
!                repeatedly reading similar files(e.g., ensembles)

!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

    use kinds, only: r_kind,i_kind,r_single
    use nemsio_module, only: nemsio_init,nemsio_open,nemsio_close
    use ncepnems_io, only: error_msg
    use nemsio_module, only: nemsio_gfile,nemsio_getfilehead,nemsio_readrecv,nemsio_writerecv
    use egrid2agrid_mod,only: g_egrid2agrid,g_create_egrid2agrid,destroy_egrid2agrid
    use egrid2agrid_mod,only: g_agrid2egrid
    use general_commvars_mod, only: fill_ns,filluv_ns,fill2_ns,filluv2_ns,ltosj,ltosi,ltosj_s,ltosi_s
    use constants, only: two,pi,half,deg2rad,r60,r3600

    use general_sub2grid_mod, only: sub2grid_info
    use guess_grids, only: ntguessig,ifilesig
    use obsmod, only: iadate
    use mpimod, only: npe
    use general_specmod, only: spec_vars
    use gridmod, only: ntracer,strip,itotsub,iglobal
    use general_commvars_mod, only: load_grid
    use ncepgfs_io, only: sigio_cnvtdv8,sighead,p_high
    use constants, only: zero,zero_single,one,fv
    use gsi_4dvar, only: ibdate,nhr_obsbin,lwrite4danl
    implicit none

! !INPUT PARAMETERS:
    character(*)                           ,intent(in   ) :: filename     ! file to open and write to
    integer(i_kind)                        ,intent(in   ) :: mype,mype_out      ! mpi task number

    type(sub2grid_info)                    ,intent(in   ) :: grd
    type(spec_vars)                        ,intent(in   ) :: sp_a

    real(r_kind),dimension(grd%lat2,grd%lon2)      ,intent(in   ) :: sub_ps  !2d
    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig) ,intent(in   ) :: sub_u,sub_v,sub_tv,sub_q,sub_oz, &
                                                             sub_cwmr,sub_prsl
    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig+1),intent(in   ) :: sub_prsi
    logical,                                intent(in   ) :: inithead

    integer(i_kind), intent(in)::   ibin
    integer(i_kind), intent(out)::  iret_write


    character(len=120) :: my_name = 'general_read_gfsatm_nems'
    character(len=1)   :: null = ' '
    real(r_kind) :: r0_001 = 0.001_r_kind
    real(r_kind) :: r1000 = 1000._r_kind

    character(6):: fname_ges

    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig):: sub_dp
    real(r_kind),dimension(max(grd%iglobal,grd%itotsub))     :: work
    real(r_kind),dimension(grd%nlon,grd%nlat-2):: grid
    real(r_kind),allocatable,dimension(:) :: rwork1d,rwork1d1,rlats,rlons
    real(4),allocatable,dimension(:) :: r4lats,r4lons
    real(r_kind),allocatable,dimension(:,:) :: grid_b,grid_b2
    real(r_kind),allocatable,dimension(:,:,:) :: grid_c, grid3, grid_c2

    integer icount,itotflds,iret,kvar,klev
    integer(i_kind),dimension(npe)::ilev,ivar

    integer(i_kind),dimension(7):: idate, jdate
    integer(i_kind),dimension(4):: odate
    integer(i_kind) :: k, nlatm2, nord_int, i, j, kk
    integer(i_kind) :: lonb, latb, levs
    integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
    integer(i_kind) :: istop = 104
    integer(i_kind),dimension(5):: mydate


    integer(i_kind),dimension(8) :: ida,jda
    real(r_kind),dimension(5)    :: fha
    real(r_kind)    :: fhour

    type(nemsio_gfile) :: gfile,gfileo
    logical diff_res,eqspace
    logical,dimension(1) :: vector

    logical lloop

!*************************************************************************
!   Initialize local variables
    iret_write=0
    nlatm2=grd%nlat-2
    itotflds=8*grd%nsig+2  ! Hardwired for now!  u,v,tv,q,oz,cwmr,ps,z,prsl,prsi
    lloop=.true.
 
    do k=1,grd%nsig
       sub_dp(:,:,k) = sub_prsi(:,:,k)-sub_prsi(:,:,k+1)
    end do

!   Set guess file name
!   if(mype == mype_out)then
       write(fname_ges,100) ifilesig(ntguessig)
100    format('sigf',i2.2)
!   Handle case of NCEP nemsio

!   Read header information from first guess file.
    call nemsio_init(iret)
    if (iret /= 0) call error_msg(0,trim(my_name),null,null,'init',istop,iret)

    call nemsio_open(gfile,trim(fname_ges),'read',iret)
    if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_ges),null,'open',istop,iret)

    call nemsio_getfilehead(gfile, iret=iret, nfhour=nfhour,        &
       nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd, &
       idate=idate, dimx=lonb, dimy=latb, dimz=levs )
    if(grd%nlon /= lonb) then
       if ( mype == 0 ) write(6, &
          '('' dual resolution for nems nlat,nlon = '',4i6)') &
          grd%nlon,grd%nlat,lonb,latb
        diff_res=.true.
    end if

    if ( iret/=0 ) then
       write(6,*)trim(my_name),': problem with nemsio_getfilehead, Status = ',iret
       call stop2(103)
    end if

    if(levs/=grd%nsig) then
       write(6,*)trim(my_name),': problem in data dimension background levs = ',levs,' nsig = ',grd%nsig
       call stop2(103)
    end if
!   copy input header info to output header info
    gfileo=gfile

!   Update header information (with ibdate) and write it to analysis file (w/
!   _open statement).
    if(mype==mype_out)then
       mydate=ibdate
       fha(:)=zero ; ida=0; jda=0
       fha(2)=real(nhr_obsbin*(ibin-1))  ! relative time interval in hours
       ida(1)=mydate(1) ! year
       ida(2)=mydate(2) ! month
       ida(3)=mydate(3) ! day
       ida(4)=0         ! time zone
       ida(5)=mydate(4) ! hour
! Move date-time forward by nhr_assimilation hours
       call w3movdat(fha,ida,jda)

       jdate(1) = jda(1)     ! analysis year
       jdate(2) = jda(2)     ! analysis month
       jdate(3) = jda(3)     ! analysis day
       jdate(4) = jda(5)     ! analysis hour
       jdate(5) = iadate(5)  ! analysis minute
       jdate(6) = 0          ! analysis scaled seconds
       jdate(7) = idate(7)   ! analysis seconds multiplier

       nfhour   =0       !  new forecast hour, zero at analysis time
       nfminute =0
       nfsecondn=0
       nfsecondd=100      ! default for denominator

       fhour = zero
       odate(1) = jdate(4)  !hour
       odate(2) = jdate(2)  !month
       odate(3) = jdate(3)  !day
       odate(4) = jdate(1)  !year

!      open new output file with new header gfileo with "write" access.
!      Use this call to update header as well
!
       call nemsio_open(gfileo,trim(filename),'write',iret=iret, &
          idate=jdate, nfhour=nfhour, nfminute=nfminute, &
          nfsecondn=nfsecondn, nfsecondd=nfsecondd)
    else
       call nemsio_open(gfileo,trim(filename),'write',iret)
    end if
    if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),null,'open',istop,iret)

!      Allocate structure arrays to hold data
    allocate(rwork1d(latb*lonb),rwork1d1(latb*lonb))
    if(diff_res)then
       allocate( grid_b(lonb,latb),grid_c(latb+2,lonb,1),grid3(grd%nlat,grd%nlon,1))
       allocate( grid_b2(lonb,latb),grid_c2(latb+2,lonb,1))
       allocate( rlats(latb+2),rlons(lonb),r4lats(lonb*latb),r4lons(lonb*latb))
       if(inithead)then
          call nemsio_getfilehead(gfile,lat=r4lats,iret=iret)
          call nemsio_getfilehead(gfile,lon=r4lons,iret=iret)
          do j=1,latb
            rlats(latb+2-j)=deg2rad*r4lats(lonb/2+(j-1)*lonb)
          end do
          rlats(1)=-half*pi
          rlats(latb+2)=half*pi
          do j=1,lonb
            rlons(j)=deg2rad*r4lons(j)
          end do
          nord_int=4
          eqspace=.false.
          call g_create_egrid2agrid(grd%nlat,sp_a%rlats,grd%nlon,sp_a%rlons, &
                             latb+2,rlats,lonb,rlons,&
                             nord_int,p_high,eqspace)

          deallocate(rlats,rlons,r4lats,r4lons)
       end if
    end if
!   end if



! Do loop until total fields have been processed.  Stop condition on itotflds

    icount=0
    gfsfields:  do while (lloop)

! First, perform sub2grid for up to npe
       call general_gather_nems(grd,sub_ps,sub_tv,sub_u,sub_v,sub_q,sub_oz,&
              sub_cwmr,sub_prsl,sub_dp,icount,ivar,ilev,work)

       do k=1,npe  ! loop over pe distributed data
          kvar=ivar(k)
          klev=ilev(k)

          if(mype == k-1 .and. kvar > 0 .and. klev > 0)then
! HS

            if(kvar == 1)then                                   ! surface height
               call nemsio_readrecv(gfile,'hgt', 'sfc',1,rwork1d,iret=iret)
               if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'hgt','writeread',istop,iret)
               call nemsio_writerecv(gfileo,'hgt','sfc',klev,rwork1d,iret=iret)
               if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'hgt','write',istop,iret)
            else  
               if(kvar == 2)then                              ! surface pressure
                  call nemsio_readrecv(gfile,'pres','sfc',1,rwork1d,iret=iret)
                  if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'psfc','writeread',istop,iret)
                  rwork1d = r0_001*rwork1d
               else if(kvar == 3)then                              ! temperature
                  call nemsio_readrecv(gfile,'tmp','mid layer',klev,rwork1d,iret=iret)
                  if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'tmp','writeread',istop,iret)
               else if(kvar == 4)then                              ! u
                  call nemsio_readrecv(gfile,'ugrd','mid layer',klev,rwork1d,iret=iret)
                  if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'ugrd','writeread',istop,iret)
               else if(kvar == 5)then                              ! v
                  call nemsio_readrecv(gfile,'vgrd','mid layer',klev,rwork1d,iret=iret)
                  if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'vgrd','writeread',istop,iret)
               else if(kvar == 6)then                              ! q
                  call nemsio_readrecv(gfile,'spfh','mid layer',klev,rwork1d,iret=iret)
                  if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'spfh','writeread',istop,iret)
               else if(kvar == 7)then                              ! oz
                  call nemsio_readrecv(gfile,'o3mr','mid layer',klev,rwork1d,iret=iret)
                  if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'o3mr','writeread',istop,iret)
               else if(kvar == 8)then                              ! cwmr
                  call nemsio_readrecv(gfile,'clwmr','mid layer',klev,rwork1d,iret=iret)
                  if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'clwmr','writeread',istop,iret)
               else if(kvar == 9)then                              ! dprsl
                  call nemsio_readrecv(gfile,'pres','mid layer',klev,rwork1d,iret=iret)
                  if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'pres','writeread',istop,iret)
                  rwork1d = r0_001*rwork1d
               else if(kvar == 10)then                             ! dp
                  call nemsio_readrecv(gfile,'dpres','mid layer',klev,rwork1d,iret=iret)
                  if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'dpres','writeread',istop,iret)
                  rwork1d = r0_001*rwork1d
               end if
               if(diff_res)then
                  grid_b=reshape(rwork1d,(/size(grid_b,1),size(grid_b,2)/))
                  vector(1)=.false.
                  call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
                  call g_egrid2agrid(p_high,grid_c,grid3,1,1,vector)
                  do kk=1,itotsub
                    i=ltosi_s(kk)
                    j=ltosj_s(kk)
                    work(kk)=work(kk)-grid3(i,j,1)
                  end do
                  do kk=1,iglobal
                    i=grd%nlat-ltosi(kk)+1
                    j=ltosj(kk)
                    grid3(j,i,1)=work(kk)
                  end do
                  call g_agrid2egrid(p_high,grid3,grid_c,1,1,vector)
                  do i=1,latb
                     do j=1,lonb
                        grid_b(j,i)=grid_b(j,i)+grid_c(latb+2-i,j,1)
                     end do
                  end do
                  rwork1d = reshape(grid_b,(/size(rwork1d)/))
               else
                  call load_grid(work,grid)
                  rwork1d = reshape(grid,(/size(rwork1d)/))
               end if
               if(kvar == 2)then                             ! surface pressure
                  rwork1d = r1000*rwork1d
                  call nemsio_writerecv(gfileo,'pres','sfc',1,rwork1d,iret=iret)
                  if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'psfc','write',istop,iret)
               else if(kvar == 3)then                             ! temperature
                  call nemsio_writerecv(gfileo,'tmp','mid layer',klev,rwork1d,iret=iret)
                  if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'tmp','write',istop,iret)
               else if(kvar == 4)then                             ! u
                  call nemsio_writerecv(gfileo,'ugrd','mid layer',klev,rwork1d,iret=iret)
                  if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'ugrd','write',istop,iret)
               else if(kvar == 5)then                             ! v
                  call nemsio_writerecv(gfileo,'vgrd','mid layer',klev,rwork1d,iret=iret)
                  if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'vgrd','write',istop,iret)
               else if(kvar == 6)then                             ! q
                  call nemsio_writerecv(gfileo,'spfh','mid layer',klev,rwork1d,iret=iret)
                  if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'spfh','write',istop,iret)
               else if(kvar == 7)then                             ! oz
                  call nemsio_writerecv(gfileo,'o3mr','mid layer',klev,rwork1d,iret=iret)
                  if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'o3mr','write',istop,iret)
               else if(kvar == 8)then                             ! cwmr
                  call nemsio_writerecv(gfileo,'clwmr','mid layer',klev,rwork1d,iret=iret)
                  if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'clwmr','write',istop,iret)
               else if(kvar == 9)then                             ! prsl
                  rwork1d = r1000*rwork1d
                  call nemsio_writerecv(gfileo,'pres','mid layer',klev,rwork1d,iret=iret)
                  if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'pres','write',istop,iret)
               else if(kvar == 10)then                            ! dp
                  rwork1d = r1000*rwork1d
                  call nemsio_writerecv(gfileo,'dpres','mid layer',klev,rwork1d,iret=iret)
                  if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'dpres','write',istop,iret)
               end if


            end if
          endif ! end if pe and ivar check

       end do  !end do over pes

       if (icount>=itotflds) then
          lloop=.false.
          exit gfsfields
       end if

    end do gfsfields

!   Print date/time stamp
!   if (mype==mype_out) then
       if(diff_res) deallocate(grid_b,grid_b2,grid_c,grid_c2,grid3)
       call nemsio_close(gfile,iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_ges),null,'close',istop,iret)

       call nemsio_close(gfileo,iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),null,'close',istop,iret)
!
! Deallocate local array
!
       deallocate(rwork1d,rwork1d1)


    if (mype==mype_out) then
       write(6,700) grd%nlon,nlatm2,lonb,latb,nfhour,odate
700    format('GENERAL_WRITE_GFSATM_nems:  anl write, nlon,nlat,lonb,latb,levs=',&
            4i6,', hour=',f10.1,', idate=',4i5)
    endif

    return


end subroutine general_write_gfsatm_nems

subroutine general_gather(grd,g_ps,g_tv,g_vor,g_div,g_q,g_oz,g_cwmr, &
           icountx,ivar,ilev,work)

! !USES:

  use kinds, only: r_kind,i_kind
  use mpimod, only: npe,mpi_comm_world,ierror,mpi_rtype
  use general_sub2grid_mod, only: sub2grid_info
  use gridmod, only: strip
  use constants, only: zero
  implicit none

! !INPUT PARAMETERS:

  type(sub2grid_info)                   ,intent(in   ) :: grd
  integer(i_kind),intent(inout) :: icountx
  integer(i_kind),dimension(npe),intent(inout):: ivar,ilev
  real(r_kind),dimension(grd%itotsub),intent(out) :: work

! !OUTPUT PARAMETERS:

  real(r_kind),dimension(grd%lat2,grd%lon2)     ,intent(  in) :: g_ps
  real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig),intent(  in) :: g_tv,&
       g_vor,g_div,g_q,g_oz,g_cwmr

! !DESCRIPTION: Transfer contents of 3d subdomains to 2d work arrays over pes
!
! !REVISION HISTORY:
!   2013-06-19  treadon
!   2013-10-24  todling  update interface to strip
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR:
!   kleist           org: np23                date: 2013-06-19
!
!EOP
!-------------------------------------------------------------------------

  integer(i_kind) klev,k,icount
  real(r_kind),dimension(grd%lat1*grd%lon1,npe):: sub

!$omp parallel do  schedule(dynamic,1) private(k,klev,icount)
  do k=1,npe
     icount=icountx+k     

     if(icount == 1)then
        ivar(k)=1
        ilev(k)=1
        sub(:,k)=zero

     else if(icount == 2)then
        ivar(k)=2
        ilev(k)=1
        call strip(g_ps ,sub(:,k))

     else if( icount>= 3 .and. icount<=(grd%nsig+2) )then
        ivar(k)=3
        klev=icount-2
        ilev(k)=klev
        call strip(g_tv(:,:,klev) ,sub(:,k))

     else if( icount>=(grd%nsig)+3 .and. icount<=2*(grd%nsig)+2 )then
        ivar(k)=4
        klev=icount-2-(grd%nsig)
        ilev(k)=klev
        call strip(g_vor(:,:,klev) ,sub(:,k))

     else if( icount>=2*(grd%nsig)+3 .and. icount<=3*(grd%nsig)+2 )then
        ivar(k)=5
        klev=icount-2-2*(grd%nsig)
        ilev(k)=klev
        call strip(g_div(:,:,klev) ,sub(:,k))

    else if( icount>=3*(grd%nsig)+3 .and. icount<=4*(grd%nsig)+2 )then
        ivar(k)=6
        klev=icount-2-3*(grd%nsig)
        ilev(k)=klev
        call strip(g_q(:,:,klev) ,sub(:,k))

    else if( icount>=4*(grd%nsig)+3 .and. icount<=5*(grd%nsig)+2 )then
        ivar(k)=7
        klev=icount-2-4*(grd%nsig)
        ilev(k)=klev
        call strip(g_oz(:,:,klev) ,sub(:,k))

    else if( icount>=5*(grd%nsig)+3 .and. icount<=6*(grd%nsig)+2 )then
        ivar(k)=8
        klev=icount-2-5*(grd%nsig)
        ilev(k)=klev
        call strip(g_cwmr(:,:,klev) ,sub(:,k))
    else
! NULL, No work to be done for this pe
        ivar(k)=-1
        ilev(k)=-1    
     end if
  end do
  icountx=icountx+npe

  call mpi_alltoallv(sub,grd%isc_g,grd%isd_g,mpi_rtype,&
       work,grd%ijn,grd%displs_g,mpi_rtype,&
       mpi_comm_world,ierror)

  return
end subroutine general_gather
subroutine general_gather_nems(grd,g_ps,g_tv,g_u,g_v,g_q,g_oz,g_cwmr, &
           g_prsl,g_dp,icountx,ivar,ilev,work)

! !USES:

  use kinds, only: r_kind,i_kind
  use mpimod, only: npe,mpi_comm_world,ierror,mpi_rtype
  use general_sub2grid_mod, only: sub2grid_info
  use gridmod, only: strip
  use constants, only: zero
  implicit none

! !INPUT PARAMETERS:

  type(sub2grid_info)                   ,intent(in   ) :: grd
  integer(i_kind),intent(inout) :: icountx
  integer(i_kind),dimension(npe),intent(inout):: ivar,ilev
  real(r_kind),dimension(grd%itotsub),intent(out) :: work

! !OUTPUT PARAMETERS:

  real(r_kind),dimension(grd%lat2,grd%lon2)     ,intent(  in) :: g_ps
  real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig),intent(  in) :: g_tv,&
       g_u,g_v,g_q,g_oz,g_cwmr,g_prsl,g_dp

! !DESCRIPTION: Transfer contents of 3d subdomains to 2d work arrays over pes
!
! !REVISION HISTORY:
!   2013-06-19  treadon
!   2013-10-24  todling  update interface to strip
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR:
!   kleist           org: np23                date: 2013-06-19
!
!EOP
!-------------------------------------------------------------------------

  integer(i_kind) klev,k,icount
  real(r_kind),dimension(grd%lat1*grd%lon1,npe):: sub

!$omp parallel do  schedule(dynamic,1) private(k,klev,icount)
  do k=1,npe
     icount=icountx+k     

     if(icount == 1)then
        ivar(k)=1
        ilev(k)=1
        sub(:,k)=zero

     else if(icount == 2)then
        ivar(k)=2
        ilev(k)=1
        call strip(g_ps ,sub(:,k))

     else if( icount>= 3 .and. icount<=(grd%nsig+2) )then
        ivar(k)=3
        klev=icount-2
        ilev(k)=klev
        call strip(g_tv(:,:,klev) ,sub(:,k))

     else if( icount>=(grd%nsig)+3 .and. icount<=2*(grd%nsig)+2 )then
        ivar(k)=4
        klev=icount-2-(grd%nsig)
        ilev(k)=klev
        call strip(g_u(:,:,klev) ,sub(:,k))

     else if( icount>=2*(grd%nsig)+3 .and. icount<=3*(grd%nsig)+2 )then
        ivar(k)=5
        klev=icount-2-2*(grd%nsig)
        ilev(k)=klev
        call strip(g_v(:,:,klev) ,sub(:,k))

    else if( icount>=3*(grd%nsig)+3 .and. icount<=4*(grd%nsig)+2 )then
        ivar(k)=6
        klev=icount-2-3*(grd%nsig)
        ilev(k)=klev
        call strip(g_q(:,:,klev) ,sub(:,k))

    else if( icount>=4*(grd%nsig)+3 .and. icount<=5*(grd%nsig)+2 )then
        ivar(k)=7
        klev=icount-2-4*(grd%nsig)
        ilev(k)=klev
        call strip(g_oz(:,:,klev) ,sub(:,k))

    else if( icount>=5*(grd%nsig)+3 .and. icount<=6*(grd%nsig)+2 )then
        ivar(k)=8
        klev=icount-2-5*(grd%nsig)
        ilev(k)=klev
        call strip(g_cwmr(:,:,klev) ,sub(:,k))

    else if( icount>=6*(grd%nsig)+3 .and. icount<=7*(grd%nsig)+2 )then
        ivar(k)=9
        klev=icount-2-6*(grd%nsig)
        ilev(k)=klev
        call strip(g_prsl(:,:,klev) ,sub(:,k))

    else if( icount>=7*(grd%nsig)+3 .and. icount<=8*(grd%nsig)+2 )then
        ivar(k)=10
        klev=icount-2-7*(grd%nsig)
        ilev(k)=klev
        call strip(g_dp(:,:,klev) ,sub(:,k))

    else
! NULL, No work to be done for this pe
        ivar(k)=-1
        ilev(k)=-1    
     end if
  end do
  icountx=icountx+npe

  call mpi_alltoallv(sub,grd%isc_g,grd%isd_g,mpi_rtype,&
       work,grd%ijn,grd%displs_g,mpi_rtype,&
       mpi_comm_world,ierror)

  return
end subroutine general_gather_nems

