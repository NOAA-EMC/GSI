!--------------------------------------------------------------
! program mainread_uv
!
!--------------------------------------------------------------

   implicit none

   character*200 fname
   character*50  fileo, grads_info_file
   character*15  mtype 

   real rpress,rlev

   integer nobs,nreal,ntotal,ngross,nreal_in,insubtype
   integer(4):: ittype,ituse,ntumgrp,ntgroup,ntmiter,iflag
   integer isubtype,ncount,ncount_vgc,ncount_gros

   real(4) :: ttwind,gtross,etrmax,etrmin,vtar_b,vtar_pg
   real(4) :: rmiss,vqclmt,vqclmte

   data rmiss/-999.0/ 

   namelist /input/ nreal, mtype, fname, fileo, rlev, insubtype, grads_info_file
 
   read (5,input)
   write(6,input)

   ncount=0
   rpress=rmiss
   ncount_vgc=0
   ncount_gros=0

   print *,'mtype, nreal = ', mtype,nreal

   call convinfo_read( mtype, 15, insubtype, ituse, ntumgrp, ntgroup, ntmiter, isubtype,&
                      ttwind, gtross, etrmax, etrmin, vtar_b, vtar_pg)

   print *, 'ituse, gtross    =',ituse,gtross
   print *, 'ntumgrp, ntgroup = ', ntumgrp, ntgroup
   print *, 'ntmiter          = ', ntmiter
   print *, 'ttwind           = ', ttwind
   print *, 'isubtype         = ', isubtype
 
   if (ituse >0) call read_uv(     nreal, mtype, fname, fileo, gtross, rlev, grads_info_file ) 
   if (ituse <0) call read_uv_mor( nreal, mtype, fname, fileo, gtross, rlev, grads_info_file ) 
  
   stop
end
