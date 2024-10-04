!-----------------------------------------------
!  program mainread_q
! 
!-----------------------------------------------

   implicit none

   character*200 fname
   character*50 fileo, grads_info_file
   character*15 dtype,mtype 
  
   real rlev,rpress

   integer nobs,nreal,ntotal,ngross,nreal_in,insubtype
   integer isubtype,ncount,ncount_vgc,ncount_gros

   integer(4):: ittype,ituse,ntumgrp,ntgroup,ntmiter,iflag
   real(4) :: ttwind,gtross,etrmax,etrmin,vtar_b,vtar_pg
   real(4) :: rmiss,vqclmt,vqclmte

   data rmiss/-999.0/ 

   namelist /input/nreal,mtype,fname,fileo,rlev,insubtype, grads_info_file
 
   read (5,input)

   print *, 'input:'
   print *, '        nreal           = ', nreal
   print *, '        mtype           = ', mtype
   print *, '        fname           = ', fname
   print *, '        fileo           = ', fileo
   print *, '        rlev            = ', rlev 
   print *, '        insubtype       = ', insubtype 
   print *, '        grads_info_file = ', grads_info_file

   ncount=0
   rpress=rmiss
   ncount_vgc=0
   ncount_gros=0

!   print *,dtype,nreal

   call convinfo_read(mtype,15,insubtype,ituse,ntumgrp,ntgroup,ntmiter,isubtype,&
                      ttwind,gtross,etrmax,etrmin,vtar_b,vtar_pg)

!   print *,'ituse  = ', ituse
!   print *,'gtross = ', gtross
   
   if (ituse >0) call read_q(nreal,mtype,fname,fileo,gtross,rlev, grads_info_file ) 
   if (ituse <0) call read_q_mor(nreal,mtype,fname,fileo,gtross,rlev, grads_info_file ) 
  
   stop
end
