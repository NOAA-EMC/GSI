!------------------------------------------------------------------
!   convinfo_read
!
!     This subroutine reads the conventional data (cnvstat) file.
!------------------------------------------------------------------

subroutine convinfo_read( dtype, idtype, insubtype, ituse, ntumgrp, ntgroup, ntmiter, isubtype,&
                             ttwind2, gtross2, etrmax2, etrmin2, vtar_b2, vtar_pg2)

   implicit none

   !--------------
   !  interface
   !
   integer,           intent(in)  :: idtype, insubtype
   character(idtype), intent(in)  :: dtype
   integer(4),        intent(out) :: ituse, ntumgrp, ntgroup, ntmiter, isubtype
   real(4),           intent(out) :: ttwind2, gtross2, etrmax2, etrmin2, vtar_b2, vtar_pg2
   

   !--------------
   !  local vars
   !
   character(120):: crecord
   character(7)  :: obstype1
   character(15) :: obstype
   character(1)  :: cflg
   character(7)  :: iotype

   integer(4) :: ittype
   integer(4) :: lunin,ithin,npred,iflag
   real(8)    :: ttwind,gtross,etrmax,etrmin,vtar_b,vtar_pg,rmesh,pmesh


   lunin=11

   open(lunin,file='convinfo',form='formatted')
   rewind(lunin)

   loopd: do
      read(lunin,1030,IOSTAT=iflag)cflg,iotype,crecord

      if(cflg == '!') cycle
      if( iflag /= 0 ) exit loopd

      read(crecord,*)ittype,isubtype,ituse,ttwind,ntumgrp,ntgroup,ntmiter,&
                      gtross,etrmax,etrmin,vtar_b,vtar_pg,ithin,rmesh,pmesh,npred
      write(obstype1,'(i3)') ittype

      obstype=trim(iotype)//trim(obstype1)

      if( trim(obstype) == trim(dtype)  .and. isubtype == insubtype) then
         ttwind2=ttwind
         gtross2=gtross
         etrmax2=etrmax
         etrmin2=etrmin
         vtar_b2=vtar_b
         vtar_pg2=vtar_pg
         exit
      endif

   enddo  loopd

1030 format(a1,a7,2x,a120)

   return
end
