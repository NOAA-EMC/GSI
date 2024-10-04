!   the subroutine to read convention information file

   subroutine convinfo(iotype_ps,iotype_q,iotype_t,iotype_uv,iotype_gps,&
                       ntype_ps,ntype_q,ntype_t,ntype_uv,ntype_gps,&
                       varqc_ps,varqc_q,varqc_t,varqc_uv,varqc_gps,&
                       ituse_ps,ituse_q,ituse_t,ituse_uv,ituse_gps,&
                       iosubtype_ps,iosubtype_q,iosubtype_t,iosubtype_uv,iosubtype_gps)

   implicit none

   integer,dimension(200) :: iotype_ps,iotype_q,iotype_t,iotype_uv, iotype_gps 
   integer,dimension(200) :: iosubtype_ps,iosubtype_q,iosubtype_t,iosubtype_uv,iosubtype_gps
   integer,dimension(200) :: ituse_ps,ituse_q,ituse_t,ituse_uv,ituse_gps 
   real(4),dimension(200,2) :: varqc_ps,varqc_q,varqc_t,varqc_uv,varqc_gps

   integer ittype,ituse,ntumgrp,ntgroup,ntmiter,isubtype
   integer  lunin,ntype_ps,ntype_q,ntype_t,ntype_uv,ntype_gps,iflag
   real(8) :: ttwind,gtross,etrmax,etrmin,vtar_b,vtar_pg

   real(8) :: rmesh, pmesh, pmot, ptime
   integer    ithin, npred
   integer    gps_ctr

   character(120):: crecord
   character(7) :: ctype 
   character(1) :: cflg

   lunin=11
   ntype_gps=0
   ntype_ps=0
   ntype_q=0
   ntype_t=0
   ntype_uv=0
   iotype_gps=0
   iotype_ps=0
   iotype_q=0
   iotype_t=0
   iotype_uv=0
   ituse_gps=0
   ituse_ps=0
   ituse_q=0
   ituse_t=0
   ituse_uv=0
   varqc_gps=0.0
   varqc_ps=0.0
   varqc_q=0.0
   varqc_t=0.0
   varqc_uv=0.0
   gps_ctr = 0
 
   print *, 'start coninfo subroutine'
   open(lunin,file='convinfo',form='formatted')
   rewind(lunin)

   loopd: do
      read(lunin,1030,IOSTAT=iflag)cflg,ctype,crecord
      if(cflg == '!')cycle

      if( iflag /= 0 ) exit loopd

      read(crecord,*)ittype,isubtype,ituse,ttwind,ntumgrp,ntgroup,ntmiter,&
                      gtross,etrmax,etrmin,vtar_b,vtar_pg, &
                      ithin, rmesh, pmesh, npred, pmot, ptime 
                       
      
      if(trim(ctype) == 'ps' ) then
         ntype_ps=ntype_ps+1
         iotype_ps(ntype_ps)=ittype
         iosubtype_ps(ntype_ps)=isubtype

         varqc_ps(ntype_ps,1)=vtar_b
         varqc_ps(ntype_ps,2)=vtar_pg
         ituse_ps(ntype_ps)=ituse

      else if(trim(ctype) == 'q') then
         ntype_q=ntype_q+1
         iotype_q(ntype_q)=ittype
         iosubtype_q(ntype_q)=isubtype
         varqc_q(ntype_q,1)=vtar_b
         varqc_q(ntype_q,2)=vtar_pg
         ituse_q(ntype_q)=ituse

      else if(trim(ctype) == 't') then
         ntype_t=ntype_t+1
         iotype_t(ntype_t)=ittype
         iosubtype_t(ntype_t)=isubtype
         varqc_t(ntype_t,1)=vtar_b
         varqc_t(ntype_t,2)=vtar_pg
         ituse_t(ntype_t)=ituse

      else if(trim(ctype) == 'uv') then
         ntype_uv=ntype_uv+1
         iotype_uv(ntype_uv)=ittype
         iosubtype_uv(ntype_uv)=isubtype

         varqc_uv(ntype_uv,1)=vtar_b
         varqc_uv(ntype_uv,2)=vtar_pg
         ituse_uv(ntype_uv)=ituse

      else if(trim(ctype) == 'gps') then
         ntype_gps=ntype_gps+1
         iotype_gps(ntype_gps)=ittype
         iosubtype_gps(ntype_gps)=isubtype

         varqc_gps(ntype_gps,1)=vtar_b
         varqc_gps(ntype_gps,2)=vtar_pg
         ituse_gps(ntype_gps)=ituse

      endif

   enddo  loopd

1030 format(a1,a7,2x,a120)

   print *, 'ntype_gps = ', ntype_gps
   print *, 'ntype_ps  = ', ntype_ps
   print *, 'ntype_q   = ', ntype_q 
   print *, 'ntype_t   = ', ntype_t 
   print *, 'ntype_uv  = ', ntype_uv

   print *, 'end convinfo subroutine'

   return

end
