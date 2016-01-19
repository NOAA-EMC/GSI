!   the subroutine to read convention information file

    subroutine convinfo_read(dtype,idtype,insubtype,ituse,ntumgrp,ntgroup,ntmiter,isubtype,&
                             ttwind2,gtross2,etrmax2,etrmin2,vtar_b2,vtar_pg2)

       implicit none

  character(120):: crecord
  character(7) :: obstype1
  character(15) :: obstype
  character(1) :: cflg
  character(7):: iotype
  integer idtype,insubtype
  character(idtype) :: dtype

  integer(4):: ittype,ituse,ntumgrp,ntgroup,ntmiter,isubtype
  integer(4) :: lunin,ithin,npred,iflag
  real(8) :: ttwind,gtross,etrmax,etrmin,vtar_b,vtar_pg,rmesh,pmesh
  real(4) :: ttwind2,gtross2,etrmax2,etrmin2,vtar_b2,vtar_pg2


  lunin=11

  write (6,*) 'start convinfo subroutine'
  write (6,*) 'dtype, idtype, insubtype = ', dtype, idtype, insubtype
  write (6,*) 'ituse, ntumgrp, ntgroup  = ', ituse, ntumgrp, ntgroup
  write (6,*) 'ntmiter, isubtype, ttwind2 = ', ntmiter, isubtype, ttwind2
  write (6,*) 'gtross2, etrmax2, etrmin2  = ', gtross2, etrmax2, etrmin2
  write (6,*) 'vtar_b2, vtar_pg2          = ', vtar_b2, vtar_pg2

  open(lunin,file='convinfo',form='formatted')
  rewind(lunin)

  loopd: do
       read(lunin,1030,IOSTAT=iflag)cflg,iotype,crecord
       write (6,*) 'cflg, iflag, iotype  = ', cflg, iflag, iotype

       if(cflg == '!')cycle
       if( iflag /= 0 ) exit loopd
       read(crecord,*)ittype,isubtype,ituse,ttwind,ntumgrp,ntgroup,ntmiter,&
                      gtross,etrmax,etrmin,vtar_b,vtar_pg,ithin,rmesh,pmesh,npred

       write(obstype1,'(i3)') ittype
       obstype=trim(iotype)//trim(obstype1)
!       print *,'obstype,dtype ',obstype,dtype
       write (6,*) 'obstype = ', obstype

       if( trim(obstype) == trim(dtype)  .and. isubtype == insubtype) then
        ttwind2=ttwind
        gtross2=gtross
        etrmax2=etrmax
        etrmin2=etrmin
        vtar_b2=vtar_b
        vtar_pg2=vtar_pg
!         print *,dtype,' ',obstype,ituse
         exit
       endif

  enddo  loopd

1030 format(a1,a7,2x,a120)

  write (6,*) 'end convinfo subroutine'

     return
     end
