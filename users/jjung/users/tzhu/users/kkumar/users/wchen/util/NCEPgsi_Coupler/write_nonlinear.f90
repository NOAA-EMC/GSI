subroutine write_nonlinear(bck_u,bck_v,bck_tv,bck_q,bck_oz,bck_cw,bck_ps,itime,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    write_nonlinear     
!   prgmmr: rancic                                    
!
! abstract:    write output of the nonlinear model run
!
! program history log:
!   2010-02-25  rancic
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds,only: r_kind,i_kind
  use gridmod,only: nsig,lat2,lon2
  use tendsmod, only: what9,prsth9,r_prsum9,prdif9,r_prdif9,pr_xsum9,pr_ysum9
  use tends4pertmod, only: prsum9,rdtop9
  use pblmod, only: dudz,dvdz,dodz,zi,rdzi,rdzl, &
        uges0,vges0,oges0,pges0,tges0,uges1,vges1,oges1, &
        adragu,bdragu,cdragu,ddragu,adragv,bdragv,cdragv,ddragv
  use jfunc, only: first
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: bck_u,bck_v,bck_tv, &
                                                      bck_q,bck_oz,bck_cw
  real(r_kind),dimension(lat2,lon2)     ,intent(in):: bck_ps
  integer(i_kind),intent(in):: itime,mype

  character(len=4):: ch_itime,ch_mype
  character(len=10):: fdir
  character(len=200):: fname
  character(len=200):: command
  integer(i_kind):: unit_nlrun,char_length


  write(ch_itime,'(i4.4)') itime
  write(ch_mype,'(i4.4)') mype

  fdir='nldir_'//ch_mype

  if(itime==0 .and. first) then
    command='mkdir '//fdir
    char_length=len_trim(command)
    call system(command(1:char_length))
  else 
     unit_nlrun=301+mype
     fname=fdir//'/file.'//ch_itime
     char_length=len_trim(fname)
     open(unit_nlrun,file=fname(1:char_length),form='unformatted')
           write(unit_nlrun) what9,prsth9,prsum9,r_prsum9,prdif9,r_prdif9, &
              pr_xsum9,pr_ysum9,rdtop9, &
              bck_u,bck_v,bck_tv,bck_q,bck_oz,bck_cw,bck_ps, &
              dudz,dvdz,dodz,zi,rdzi,rdzl, &
              uges0,vges0,oges0,pges0,tges0,uges1,vges1,oges1, &
              adragu,bdragu,cdragu,ddragu, &
              adragv,bdragv,cdragv,ddragv
     close(unit_nlrun)
  end if

end subroutine write_nonlinear
