       subroutine xasc2asc(srcstr,dststr,nchmoved,iretn)
C                                                        15-May-1995/dss
C      ... move character array from source to destination
C      ...   substituting a '.' for any non-printable-ASCII-code.
C      ... For use in printing a dump as characters.
C
C      ... return code = 0    for normal return
C                      < 16   for warnings
C                      >= 16  for abnormal end
C
C      CAUTION:  Since I use the LEN() function to determine how many
C                characters to move, you should use explicit length
C                of char string in call sequence.
C
C Krishna Kumar converted this code from CRAY to IBM RS/6000 1999-07-01
C
       character*(*)  srcstr
       character*(*)  dststr
       integer        nchmoved
       integer      iretn

       integer      sizsorc
       integer      sizdest

       integer      MAXSTR
       parameter     (MAXSTR = 65535)

       character*1    cfill
       data           cfill   / '.' /
       INTEGER        K01X
       DATA           K01X    /   1 /
       INTEGER        K02X
       DATA           K02X    /   2 /
       INTEGER        K10X
       DATA           K10X    /  16 /
       integer        K20X
       data           K20X    /  32 /
       INTEGER        K60X
       DATA           K60X    /  96 /
       INTEGER        K7FX
       DATA           K7FX    / 127 /
       INTEGER        K80X
       DATA           K80X    / 128 /
       
       integer      indx

       integer      iacc

       iretn = 0
       nchmoved = 0
       sizsorc = len(srcstr)
       if (sizsorc .le. 0) then
         iretn = ior(iretn,K10X)
C        ... error stop 16.  given string has zero length.
         go to 999
       else if (sizsorc .gt. MAXSTR) then
         iretn = ior(iretn,K01X)
C        ... WARNING 1! truncating given source at MAXSTR limit...
         sizsorc = MAXSTR
       endif

       sizdest = len(dststr)
       if (sizdest .le. 0) then
         iretn = ior(iretn,K80X)
C        ... error stop 128.  results string has zero length.
         go to 999
       else if (sizdest .gt. MAXSTR) then
         iretn = ior(iretn,K02X)
C        ... WARNING 2! truncating given destination at MAXSTR limit...
         sizdest = MAXSTR
       endif
C
C      ... now I have checked given dimensions,
C      ... I should move translated characters from srcstr(1):(sizsorc)
C                                                to dststr(1):(sizdest)
C      ... but what if sizsorc .GT. sizdest ????
       nch2do = min(sizsorc,sizdest)
       
       dststr(1:nch2do) = srcstr(1:nch2do)

C      ... then test for exceptions, and overwrite ...
       do  la = 1,nch2do
         iacc = mova2i(srcstr(la:la))
         if((iacc .LT. K20X) .OR. (iacc .GE. K7FX)) then
           dststr(la:la) = cfill
         else if(iacc .eq. K60X) then
           dststr(la:la) = cfill
         endif
       enddo
       nchmoved = nch2do
  999  continue
       return
       end
