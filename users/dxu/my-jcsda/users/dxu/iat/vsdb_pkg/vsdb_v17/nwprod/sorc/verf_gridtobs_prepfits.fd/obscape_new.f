      subroutine obscapepw(cape,cin,pli,pw)

c
c   Purpose:  Set up the observed profile of temperature, pressure
c   and moisture for the processing of the convective indices CAPE, CIN,
c   and LI.  Returns the observed values of CAPE, CIN, and LI and puts then
c   in the obs array. 
c
c   Author:  Perry Shafran - 19 May 2006
c

      REAL*8 hdr(10), cat(255), obs(10,255), qms(10,255)
      real*8 obs2(10,255),obs3(10,255),obs4(10,255),obs5(10,255)
      real*8 obs6(10,255),obs7(10,255),obs8(10,255),obsi2(6,2)
      real*8 obsi4(3,2)
      REAL*8 bak(10,255),bak2(10,255),bak3(6,2),bak4(3,2)

      real p(200),p1(200),q(200),q1(200),t(200),t1(200)
      real pint(201)

      character*80 headr,qmstr
      character*80 obstr,obstr2,obstr3,obstr4,obstr5,obstr6,obstr7,
     *    obstr8
      character*80 obstrout,obstrout2
      character*8 subset

      real*8 pbnd(6),qbnd(6),tbnd(6),pbint(7)

      COMMON /observ/ hdr, cat, obs, obs2, obs3, obs4, obs5, obs6,
     *     obs7,obs8, obsi2,obsi4, qms,nlev, nlev2,nlev3,nlev4,nlev5,
     *     nlev6,nlev7,nlev8
      COMMON /backgv/ bak, bak2, bak3, bak4, nbak
      common /obstrs/ headr,obstr,qmstr,subset,idate,nsub,obstr2,
     *        obstr3,obstr4,obstr5,obstr6,obstr7,obstr8

      DATA bmiss /10E10/

      ILEV=0
      do k=1,200
       p(k)=0.
       p1(k)=0.
       q(k)=0.
       q1(k)=0.
       t(k)=0.
       t1(k)=0.
      enddo
      slindx=0.
      cape=0.
      cin=0.
      peql=0.
      plcl=0.
      ifcape=0.
c     do iii=9,11
c       bak(iii,2)=bmiss
c       obs(iii,2)=bmiss
c       bak(iii,1)=bmiss
c       obs(iii,1)=bmiss
c     enddo
      DO K=1,200
      if(obs(2,k).ne.bmiss.and.
     *    obs(3,k).ne.bmiss) then
        ilev=ilev+1
        P1(ilev)=OBS(1,k)*100.
        Q1(ilev)=OBS(2,k)/1000000.
        T1(ilev)=OBS(3,k)+273.15
      endif
      ENDDO
      do k=1,ilev
       p(k)=p1(ilev-k+1)
       q(k)=q1(ilev-k+1)
       t(k)=t1(ilev-k+1)
      enddo
      CALL CALCAPE(T,Q,P,pint,ILEV,1,1,ILEV,
     *   CAPE,CIN,PLCL,PEQL,PLI)
c     obs(9,1)=cape
c     obs(10,1)=cin
c     obs(11,1)=pli
      obsi2(2,2)=cape
      obsi2(3,2)=cin
      obsi2(4,2)=pli
c
c  Now calculate the Best Cape - 11 July 2011
c
c  In order to calculate Best Cape the arrays P1D, T1D, and Q1D need to be obtained - borrowing
c  code from the Post
c
c  From BNDLYR
c

       nbnd=6
       dpbnd=30.e2
       pbint(1)=p(nint(ilev)+1)
       do lbnd=2,nbnd+1
        pbint(lbnd)=pbint(lbnd-1)-dpbnd
       enddo

       do lbnd=1,nbnd+1
         print*,'lbnd,pbint(lbnd)=',lbnd,pbint(lbnd)
       enddo

       do lbnd=1,nbnd
         l=200
         pmin=9999999.
         pbnd(lbnd)=(pbint(lbnd)+pbint(lbnd+1))*0.5
 
         do ll=1,ilev
          pm=p(ll)
          delp=abs(pm-pbnd(lbnd))
          if(delp.lt.pmin) then
           pmin=delp
           l=ll
          endif
         enddo

         dp=p(l+1)-p(l)
         tbnd(lbnd)=t(l)
         qbnd(lbnd)=q(l)

       enddo

       do lbnd=1,nbnd
        print*,'lbnd,pbnd,tbnd,qbnd=',lbnd,pbnd(lbnd),tbnd(lbnd),qbnd(lbnd)
       enddo

c      do lbnd=1,nbnd
c        pbnd(lbnd) = 0.0
c        tbnd(lbnd) = 0.0
c        qbnd(lbnd) = 0.0
c        psum(lbmd) = 0.0

c        do l=1,200
c         pm=p(l)
c         if((pbint(lbnd).ge.pm).and.pbint(lbnd+1).le.pm)) then
c           dp=p(l+1)-p(l)
c           psum(lbnd)=psum(lbnd)+dp
c           tbnd(lbnd)=tbnd(lbnd)+t(l)*dp
c           qbnd(lbnd)=qbnd(lbnd)+q(l)*dp
c         endif
c        enddo
c      enddo
c
c       if(psum(lbnd).ne.0) then
c         rpsum=1.0/psum(lbnd)
c         pbnd(lbnd)=(pbint(lbnd)+pbint(lbnd+1))*0.5
c         tbnd(lbnd)=tbnd(lbnd)*rpsum
c         qbnd(lbnd)=qbnd(lbnd)*rpsum
c        endif
c       enddo

       ilev=0
       do k=1,200
       if(obs(2,k).ne.bmiss.and.
     *    obs(3,k).ne.bmiss) then
       ilev=ilev+1
       p1(ilev)=obs(1,k)*100
       q1(ilev)=obs(2,k)/1000000.
       endif
       enddo
       do k=1,ilev
        p(k)=p1(ilev-k+1)
        q(k)=q1(ilev-k+1)
       enddo
       htm=1
       call calpw(pw,p,q,ilev)
       

c     obsi(9,2)=pw
      return
      end
