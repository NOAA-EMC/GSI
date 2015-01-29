subroutine read_nonlinear(itime,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_nonlinear     
!   prgmmr: rancic                                    
!
! abstract:    read output from the nonlinear model run
!
! program history log:
!   2010-02-25  rancic
!   2011-05-23  todling - add ability to treat pert models as linear
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds,only: r_kind,i_kind
  use constants, only: zero,one,two,half
  use gridmod,only: nsig,lat2,lon2
  use pblmod, only: dudz,dvdz,dodz,zi,rdzi,rdzl, &
        uges0,vges0,oges0,pges0,tges0,uges1,vges1,oges1, &
        adragu,bdragu,cdragu,ddragu,adragv,bdragv,cdragv,ddragv
  use nonlinmod, only: bck_u,bck_v,bck_tv,bck_q,bck_oz,bck_cw,bck_ps, &
        bck_u_lon,bck_u_lat,bck_v_lon,bck_v_lat,bck_tvlat,bck_tvlon, &
        bck_qlon,bck_qlat,bck_ozlon,bck_ozlat,bck_cwlon,bck_cwlat, &
        what_bck,prsth_bck,prsum_bck,r_prsum_bck,prdif_bck,r_prdif_bck, &
        pr_xsum_bck,pr_ysum_bck,rdtop_bck
  use tends4pertmod,only: itrajfreq
  implicit none 

  integer(i_kind),intent(in):: itime,mype
  character(len=4):: ch_itime,ch_mype
  character(len=10):: fdir
  character(len=200):: fname
  character(len=200):: command
  integer(i_kind):: unit_nlrun
  integer(i_kind),save:: ilinear=0
 
  if(itrajfreq>=0) then
     if(itrajfreq==0) then
        if(ilinear>0) return
        if(mype==0) print*, '-------------------------------'     
        if(mype==0) print*, 'NOTE: Using Linear Pert  Models'
        if(mype==0) print*, '      instead of Tangent Models'
        if(mype==0) print*, '-------------------------------'     
     else if(mod(ilinear,itrajfreq)/=0) then
        return 
     endif
  endif

  write(ch_itime,'(i4.4)') itime
  write(ch_mype,'(i4.4)') mype

  fdir='nldir_'//ch_mype

  unit_nlrun=301+mype

  fname=fdir//'/file.'//ch_itime
  if(mype==0) write(6,*) 'read_nonlinear: reading file ', trim(fname)
  open(unit_nlrun,file=trim(fname),form='unformatted')
   
        rewind(unit_nlrun)
        read(unit_nlrun) what_bck,prsth_bck,prsum_bck,r_prsum_bck,prdif_bck,r_prdif_bck, &
            pr_xsum_bck,pr_ysum_bck,rdtop_bck, &
            bck_u,bck_v,bck_tv,bck_q,bck_oz,bck_cw,bck_ps, &
            dudz,dvdz,dodz,zi,rdzi,rdzl, &
            uges0,vges0,oges0,pges0,tges0,uges1,vges1,oges1, &
            adragu,bdragu,cdragu,ddragu,adragv,bdragv,cdragv,ddragv

  close(unit_nlrun)


        call mp_compact_dlon2(bck_u,bck_u_lon,.false.,nsig,mype)
        call mp_compact_dlat2(bck_u,bck_u_lat,.false.,nsig,mype)
        call mp_compact_dlon2(bck_v,bck_v_lon,.false.,nsig,mype)
        call mp_compact_dlat2(bck_v,bck_v_lat,.false.,nsig,mype)
        call mp_compact_dlon2(bck_tv,bck_tvlon,.false.,nsig,mype)
        call mp_compact_dlat2(bck_tv,bck_tvlat,.false.,nsig,mype)
        call mp_compact_dlon2(bck_q,bck_qlon,.false.,nsig,mype)
        call mp_compact_dlat2(bck_q,bck_qlat,.false.,nsig,mype)
        call mp_compact_dlon2(bck_oz,bck_ozlon,.false.,nsig,mype)
        call mp_compact_dlat2(bck_oz,bck_ozlat,.false.,nsig,mype)
        call mp_compact_dlon2(bck_cw,bck_cwlon,.false.,nsig,mype)
        call mp_compact_dlat2(bck_cw,bck_cwlat,.false.,nsig,mype)

  ilinear=ilinear+1
 
 
end subroutine read_nonlinear
