       subroutine hafpakra(isorcra,nsizsor,idestra,nsizdes,
     1                        nwd_dest,iret_haf)
C      ... half-packer for an array                     25-Oct-1995/dss
C      ... A CRAY version to half-pack the low-order 32-bits
C      ... of each longword of isorcra(nsizsor) into the idestra().
C      ... If you give me an odd-numbered value for nsizsor, then
C      ... the last word in idestra will have the last source item 
C      ... in the hi-order 32-bits, and the low-order 32-bits will
C      ... be zero. 
C      ... User must allocate enough space in destination array;
C      ... or I will error-return without moving any data.
C      ... Krishna Kumar converted this code from CRAY to IBM RS/6000.

       integer     nbitwdsz
       parameter  (nbitwdsz=64)    		!... CRAY int word siz
       integer     nbitshf
       parameter  (nbitshf=nbitwdsz/2)       	!... 64/2 => 32 bits shf
       
       integer     isorcra(nsizsor)
       integer     idestra(nsizdes)
       integer     nwd_dest
       integer     iret_haf
       
       integer     mskrhs 
       data        mskrhs      / X'00000000FFFFFFFF' /
       logical     lonemore
       integer     iacc
       integer     mq
       integer     nwd_required


C      . . . . .   S T A R T   . . . . . . . . . . . . . . . . . . .

       iret_haf = 0
   

       if(nsizdes .LE. 0) then
          nwd_dest = 0
          iret_haf = 1
          go to 999
       endif

       if(nsizsor .LE. 0) then
          nwd_dest = 0
          iret_haf = 2
          go to 999
       endif

C      ... otherwise, nsizsor .GT. 0,  ...
       nwd_dest = nsizsor / 2
       nwd_required = nwd_dest
       lonemore = .FALSE.
       if(mod(nsizsor,2) .NE. 0) then
         lonemore = .TRUE.
         nwd_required = nwd_required + 1
       endif

       if(nwd_required .GT. nsizdes) then
C        ... bad!  Not enough space has been allocated in destination
C        ...       so ERROR EXIT
         nwd_dest = 0
         iret_haf = 3
         go to 999
       endif
C      ... otherwise, there is enough space allocated for results ...

       if(nwd_dest .GT. 0) then
          do  ide = 1,nwd_dest
             isr2 = 2*ide
             isr1 = isr2 - 1

             iacc = ishft(isorcra(isr1),nbitshf)
             mq   = iand(isorcra(isr2),mskrhs)

             idestra(ide) = ior(iacc,mq)
          enddo
       endif

       if(lonemore) then
          nwd_dest = nwd_dest + 1
          isr1 = 2*nwd_dest - 1
          idestra(nwd_dest) = ishft(isorcra(isr1),nbitshf)
       endif

  999  continue
       return
       end       
