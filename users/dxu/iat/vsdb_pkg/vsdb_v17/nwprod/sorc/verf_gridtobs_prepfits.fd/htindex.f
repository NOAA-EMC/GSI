      subroutine htindex(tc,p,q,hi)

      real*8 tc,tf,rh,hi,a,q,p,vp,qs
      real tk

c     print*,'tc,p,q=',tc,p,q

      a=0
      qi=q*0.000001
      tf=tc*(9.0/5.0)+32.0
      tk=tc+273.16
      vp=w3fa09(tk)*10.
      qs=.622*vp/(p-.378*vp)
      rh=(qi/qs)*100.
c     print*,'tf,rh=',tf,rh

c     print*,0.345372e-3

c      a=16.923+0.185212*tf+5.37941*rh-0.100254*tf*rh
c      a=a+0.941695e-2*tf**2+0.728898e-2*rh**2+0.345372e-3*tf**2*rh
c      a=a-0.814971e-3*tf*rh**2+0.102102e-4*tf**2*rh**2
c      a=a-0.38646e-4*tf**3+0.291583e-4*rh**3+0.142721e-5*tf**3*rh
c      a=a+0.197483e-6*tf*rh**3-0.218429e-7*tf**3*rh**2
c      a=a+0.843296e-9*tf**2*rh**3-0.481975e-10*tf**3*rh**3
c      hi=a+0.5

c     print*,'hi, 1st method=',hi

      a=-42.379+2.04901523*tf+10.14333127*rh
      a=a-0.22475541*tf*rh-6.83783e-3*tf**2
      a=a-5.481717e-2*rh**2+1.22874e-3*tf**2*rh
      a=a+8.5282e-4*tf*rh**2-1.99e-6*tf**2*rh**2
      hi=a

c     print*,'hi, 2nd method=',hi
       
c     print*,'index=',hi
      return
      end
