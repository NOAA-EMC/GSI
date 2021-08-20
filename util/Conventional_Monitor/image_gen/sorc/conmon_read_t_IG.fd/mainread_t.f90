!-------------------------------------------------------
!  program mainread_t
!
!-------------------------------------------------------

   implicit none

   character*200 fname
   character*50 fileo, grads_info_file
   character*15 mtype 

   integer nobs,nreal,ntotal,ngross,nreal_in,insubtype
   integer isubtype,ncount_gros,ncount_vgc,ncount
   integer(4):: ittype,ituse,ntumgrp,ntgroup,ntmiter,iflag
   real rpress,rlev
   real(4) :: ttwind,gtross,etrmax,etrmin,vtar_b,vtar_pg

   real(4) :: rmiss,vqclmt,vqclmte
 
   data rmiss/-999.0/ 

   namelist /input/nreal,mtype,fname,fileo,rlev,insubtype, grads_info_file
 

   read (5,input)
!   write(6,input)

   ncount=0
   rpress=rmiss
   ncount_vgc=0
   ncount_gros=0

!   print *,mtype,nreal

   call convinfo_read(mtype,15,insubtype,ituse,ntumgrp,ntgroup,ntmiter,isubtype,&
                      ttwind,gtross,etrmax,etrmin,vtar_b,vtar_pg)

!   print *,'ituse=',ituse,gtross

   if (ituse >0) call read_t(nreal,mtype,fname,fileo,gtross,rlev, grads_info_file ) 
   if (ituse <0) call read_t_mor(nreal,mtype,fname,fileo,gtross,rlev, grads_info_file ) 
  
   stop
end
