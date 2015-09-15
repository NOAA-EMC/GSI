! the program read humidity files

   implicit none

  character*200 fname
  character*50 fileo
  character*15 dtype,mtype 

  real*4 tiny,huge,real
  real rlev,rpress

  integer nobs,nreal,ntotal,ngross,nreal_in,insubtype
  integer isubtype,ncount,ncount_vgc,ncount_gros

  integer(4):: ittype,ituse,ntumgrp,ntgroup,ntmiter,iflag
  real(4) :: ttwind,gtross,etrmax,etrmin,vtar_b,vtar_pg

  real(4) :: bmiss,vqclmt,vqclmte

  data bmiss/-999.0/ 


  namelist /input/nreal,mtype,fname,fileo,rlev,insubtype
 

  read (5,input)
!  write(6,input)

  ncount=0
  rpress=bmiss
  ncount_vgc=0
  ncount_gros=0

!  print *,dtype,nreal

  call convinfo_read(mtype,15,insubtype,ituse,ntumgrp,ntgroup,ntmiter,isubtype,&
                     ttwind,gtross,etrmax,etrmin,vtar_b,vtar_pg)

!  print *,'ituse=',ituse,gtross

  if (ituse >0) call read_q(nreal,mtype,fname,fileo,gtross,rlev) 
  if (ituse <0) call read_q_mor(nreal,mtype,fname,fileo,gtross,rlev) 
  
  stop
  end
