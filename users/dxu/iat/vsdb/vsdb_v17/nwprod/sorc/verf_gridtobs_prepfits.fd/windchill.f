      subroutine windchill(tc,u,v,tch)

      real*8 tc,u,v,w,tf

      tf=tc*(9.0/5.0)+32.0
      w=sqrt(u**2+v**2)
      w=w*2.5

c     print*,'tf,w=',tf,w

c     tch = 0.0817(3.71*w**0.5+5.81-0.25*w)*(tf-91.4)+91.4
      if(w.gt.0) then
      tch = 35.74+0.6215*tf-35.75*w**0.16+0.4275*tf*w**0.16
      else
      tch=tf
      endif
c     print*,'tch=',tch
      return
      end
