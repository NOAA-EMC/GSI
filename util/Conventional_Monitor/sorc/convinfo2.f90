!   the subroutine to read convention information file

    subroutine convinfo(iotype_ps,iotype_q,iotype_t,iotype_uv,ntype_ps,&
                             ntype_q,ntype_t,ntype_uv,varqc_ps,varqc_q,varqc_t,varqc_uv,&
                             ituse_ps,ituse_q,ituse_t,ituse_uv,&
                             iosubtype_ps,iosubtype_q,iosubtype_t,iosubtype_uv)

       implicit none

  integer,dimension(100) :: iotype_ps,iotype_q,iotype_t,iotype_uv 
  integer,dimension(100) :: iosubtype_ps,iosubtype_q,iosubtype_t,iosubtype_uv 
  integer,dimension(100) :: ituse_ps,ituse_q,ituse_t,ituse_uv 
  real(4),dimension(100,2) :: varqc_ps,varqc_q,varqc_t,varqc_uv

  integer ittype,ituse,ntumgrp,ntgroup,ntmiter,isubtype
  integer  lunin,ntype_ps,ntype_q,ntype_t,ntype_uv,ithin,npred
  real(8) :: ttwind,gtross,etrmax,etrmin,vtar_b,vtar_pg,rmesh,pmesh
  integer iflag

  character(120):: crecord
  character(7) :: ctype 
  character(1) :: cflg

  lunin=11
  ntype_ps=0
  ntype_q=0
  ntype_t=0
  ntype_uv=0
  iotype_ps=0
  iotype_q=0
  iotype_t=0
  iotype_uv=0
  ituse_ps=0
  ituse_q=0
  ituse_t=0
  ituse_uv=0
  varqc_ps=0.0
  varqc_q=0.0
  varqc_t=0.0
  varqc_uv=0.0
  
  

  print *, 'start coninfo subroutine'
  open(lunin,file='convinfo',form='formatted')
  rewind(lunin)

  loopd: do
       read(lunin,1030,IOSTAT=iflag)cflg,ctype,crecord
       if(cflg == '!')cycle
       if( iflag /= 0 ) exit loopd
       read(crecord,*)ittype,isubtype,ituse,ttwind,ntumgrp,ntgroup,ntmiter,&
                      gtross,etrmax,etrmin,vtar_b,vtar_pg,ithin,rmesh,pmesh,npred
!       print *,cflg,ctype,ittype,isubtype,ituse,ntype_ps,ntype_q,ntype_t,ntype_uv
       if(trim(ctype) == 'ps' ) then
          ntype_ps=ntype_ps+1
!          print *,ntype_ps
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
       endif
  enddo  loopd

1030 format(a1,a7,2x,a120)

     return
     end
