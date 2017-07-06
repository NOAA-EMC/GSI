! the program read humidity files

   implicit none

  character*200 fname
  character*50 fileo
  character*15 mtype,dtype 

  real*4 tiny,huge,real
  integer nobs,nreal,ntotal,ngross,nreal_in,insubtype

  integer(4):: ittype,ituse,ntumgrp,ntgroup,ntmiter,iflag
  integer ncount,ncount_vgc,ncount_gros,isubtype

  real(4) :: ttwind,gtross,etrmax,etrmin,vtar_b,vtar_pg
  real rlev,rpress

  real(4) :: bmiss,vqclmt,vqclmte

  data bmiss/-999.0/ 


  namelist /input/nreal,mtype,fname,fileo,rlev,insubtype
 

  read (5,input)
  write(6,input)

  ncount=0
  rpress=bmiss
  ncount_vgc=0
  ncount_gros=0

!  print *,mtype,nreal

  call convinfo_read(mtype,15,insubtype,ituse,ntumgrp,ntgroup,ntmiter,isubtype,&
                     ttwind,gtross,etrmax,etrmin,vtar_b,vtar_pg)

!  print *,'ituse=',ituse,gtross
!  write(6, 'ituse,gross =', ituse,gtross

  if (ituse >0) call read_ps(nreal,mtype,fname,fileo,gtross,rlev) 
  if (ituse <0) call read_ps_mor(nreal,mtype,fname,fileo,gtross,rlev) 
  
  stop
  end
