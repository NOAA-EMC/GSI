      subroutine obspbl(hpbl)

      use observ

      REAL Q(100),P(100),T(100),PINT(101),Z(100),U(100),V(100)
      REAL q1(100),p1(100),t1(100),z1(100),u1(100),v1(100),d1(100)
      REAL q2(100),p2(100),t2(100),z2(100),u2(100),v2(100)
      REAL a1(100),a2(100)

      DATA bmiss /10E10/

      do k=1,100
      p1(k)=obs(1,k)
      t1(k)=obs(3,k)
      q1(k)=obs(2,k)
      z1(k)=obs(4,k)
      u1(k)=obs(5,k)
      v1(k)=obs(6,k)
      enddo
                                                                                
      p2=bmiss
      t2=bmiss
      q2=bmiss
      z2=bmiss
      u2=bmiss
      v2=bmiss
      do k=1,100
                                                                                
         do ii=1,100
          if(p1(ii).ne.bmiss) then
            allmiss=0.
            goto 500
          else
            allmiss=1.
          endif
         enddo
500       continue
                                                                                
      if(allmiss.ne.1)then
        imx=maxloc(p1, dim=1, mask = p1.ne.bmiss)
        p2(k)=p1(imx)
        p1(imx)=bmiss
        t2(k)=t1(imx)
        q2(k)=q1(imx)
        z2(k)=z1(imx)
        u2(k)=u1(imx)
        v2(k)=v1(imx)
        nump=k
      endif
      enddo
                                                                                
      do ivar=1,5
                                                                                
        if(ivar.eq.1) a2=t2
        if(ivar.eq.2) a2=q2
        if(ivar.eq.3) a2=u2
        if(ivar.eq.4) a2=v2
        if(ivar.eq.5) a2=z2
                                                                                
      a1=bmiss
      do k=1,100
        if(k.ge.nump) goto 501
         if(a2(k).eq.bmiss) then
          km=k
          kp=k
188       km=km-1
           if(km.eq.0) goto 501
           if(a2(km).eq.bmiss) goto 188
189       kp=kp+1
           if(kp.eq.nump) goto 501
           if(a2(kp).eq.bmiss) goto 189
            coef=(alog(p2(k)/p2(km)))/(alog(p2(kp)/p2(km)))
            a1(k)=a2(km)+coef*(a2(kp)-a2(km))
         else
           a1(k)=a2(k)
         endif
501   continue
      enddo
                                                                                
        if(ivar.eq.1) t1=a1
        if(ivar.eq.2) q1=a1
        if(ivar.eq.3) u1=a1
        if(ivar.eq.4) v1=a1
        if(ivar.eq.5) z1=a1
                                                                                
      enddo
                                                                                
      p1=p2
                                                                                
      e0=611
      xll=2500000
      rv=461
                                                                                
      iilev=0
      DO K=1,100
      if(p1(k).ne.BMISS.and.
     *    q1(k).ne.BMISS.and.
     *    t1(k).ne.BMISS.and.
     *    z1(k).ne.BMISS.and.
     *    u1(k).ne.BMISS.and.
     *    v1(k).ne.BMISS) then
        iilev=iilev+1
        P1(iilev)=p1(k)*100.
        Q1(iilev)=q1(k)/1000000.
        T1(iilev)=t1(k)+273.15
        Z1(iilev)=z1(k)
        U1(iilev)=u1(k)
        V1(iilev)=v1(k)
        e=q1(iilev)*p1(iilev)/0.622
        d1(iilev)=1/(1/273.16-(rv/xll)*alog(e/611.))
         if(iilev.gt.1.and.p1(iilev).gt.p1(iilev-1))print*,
     +   '!!!PBL:lev=',iilev-1,'p=',p1(iilev-1),
     +   'lev=',iilev,'p=',p1(iilev)
      endif
      ENDDO
                                                                                
        if(iilev.gt.2) then
        CALL CALPBL(T1,Q1,P1,Z1,U1,V1,IILEV,HPBL,JPBL)
c---- Substract station's elevation from HPBL
        hpbl=hpbl-hdr(5)
                                                                                
        obs(8,1)=hpbl
        endif

        return
        end
