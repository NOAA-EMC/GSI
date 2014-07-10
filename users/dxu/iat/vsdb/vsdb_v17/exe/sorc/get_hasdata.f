cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   This program is to check if there is data 
c   Author: Binbin Zhou
c           Mar, 2005

       subroutine get_hasdata(data,numfcst,numvarbl,
     + numlevel,ngrid,levels,tendencymrk,tnd, hasdata)

       INCLUDE 'parm.inc'       
       real data(numfcst,numvarbl,numlevel,ngrid)
       integer hasdata(mxfcst,mxvrbl,maxlvl),levels(mxvrbl)
       integer tendencymrk(mxvrbl), tnd

       do i = 1, numfcst
        do j = 1, numvarbl
         if(tendencymrk(j).eq.tnd) then
            do k = 1, levels(j)
             if(data(i,j,k,1).le.(-999.0)) then
               hasdata(i,j,k) = 0
             end if
            end do
         end if
        end do
       end do

      return
      end
       
           
