!! the subroutine is to calculate the statistics

 subroutine stascal(dtype,rdiag,nreal,n,iotype,varqc,ntype,work,worku,&
                      workv,np,ptop,pbot,nregion,mregion,&
                      rlatmin,rlatmax,rlonmin,rlonmax,iosubtype)
   implicit none

   real(4),dimension(nreal,n) :: rdiag
   real(4),dimension(100,2) :: varqc
   real(4),dimension(np,100,6,nregion,3) :: work,worku,workv
   real(4),dimension(np) :: ptop,pbot
   real(4),dimension(mregion):: rlatmin,rlatmax,rlonmin,rlonmax

   character(3) :: dtype
  
   integer,dimension(100) :: iotype,iosubtype
   integer itype,isubtype,ilat,ilon,ipress,iqc,iuse,imuse
   integer iwgt,ierr1,ierr2,ierr3,iobg,iobgu,iobgv,iqsges
   integer iobsu,iobsv,i,nregion,mregion,np,n,nreal,k,j
   integer ltype,ntype,intype,insubtype,nn
   real cg_term,pi,tiny
   real valu,valv,val,val2,gesu,gesv,spdb,exp_arg,arg
   real ress,ressu,ressv,valqc,term,wgross,cg_t,wnotgross
   real cvar_pg,cvar_b,rat_err2

   itype=1;isubtype=2;ilat=3;ilon=4;ipress=6;iqc=9;iuse=11;imuse=12
  iwgt=13;ierr1=14;ierr2=15;ierr3=16;iobg=18;iobgu=18;iobgv=21;iqsges=20
  iobsu=17;iobsv=20

   pi=acos(-1.0)
   cg_term=sqrt(2.0*pi)/2.0
   tiny=1.0e-10

 
!   print *,'start to call stascal'
!   print *,n,nreal,ntype
!   print *,rdiag(itype,1),rdiag(ilat,1),rdiag(ilon,1),rdiag(ipress,1)
!   print *,rdiag(iqc,1),rdiag(iuse,1),rdiag(imuse,1),rdiag(ierr1,1),rdiag(iobg,1)
!   print *,rlatmin(1),rlatmax(1),rlonmin(1),rlonmax(1),np,nregion,ntype 
!   print *,iotype(1),iotype(2),iotype(3),iotype(4),iotype(5),iotype(6)
   
   do i=1,n
     if(trim(dtype) ==  ' uv') then
        valu=rdiag(iobgu,i)*rdiag(ierr1,i)
        valv=rdiag(iobgu,i)*rdiag(ierr1,i)
        val=0.5*(valu*valu+valv*valv)
        val2=val
        gesu=rdiag(iobgu,i)-rdiag(iobsu,i)
        gesv=rdiag(iobgv,i)-rdiag(iobsv,i)
        spdb=sqrt(rdiag(iobsu,i)**2+rdiag(iobsv,i)**2)-sqrt(gesu**2+gesv**2) 
     else
        val=rdiag(iobg,i)*rdiag(ierr1,i)
        val2     = val*val
     endif
     exp_arg  = -0.5*val2
     if ( rdiag(ierr3,i) <1.0e10 .and. rdiag(ierr1,i) <1.0e10 .and. rdiag(ierr1,i) >1.0e-10 ) then
        rat_err2 = (rdiag(ierr3,i)/rdiag(ierr1,i))**2
     else
        rat_err2 =0.0
     endif
  
!     print *,rdiag(imuse,i),rdiag(iobg,i),rdiag(ierr1,i),rdiag(ierr3,i),rat_err2,val2 
     if(rdiag(imuse,i) >0.0) then
        nn=1
     else
        nn=2
        if(rdiag(ierr3,i) >tiny) nn=3
     endif
!      print *,rdiag(itype,i),rdiag(ierr1,i),rdiag(ierr3,i),rdiag(imuse,i),nn
       do ltype=1,ntype
          intype=int(rdiag(itype,i))
          insubtype=int(rdiag(isubtype,i))
          
!            print *,intype,iotype(ltype),ltype
          if(intype == iotype(ltype) .and. insubtype == iosubtype(ltype)) then
!            print *,intype,iotype(ltype),ltype,nn,insubtype,iosubtype(ltype)
             cvar_pg=varqc(ltype,2)
             cvar_b=varqc(ltype,1)
             if (cvar_pg > 0.0 .and. rdiag(imuse,i) >0.0) then
               arg  = exp(exp_arg)
               wnotgross= 1.0-cvar_pg
               cg_t=cvar_b
               wgross = cg_term*cvar_pg/(cg_t*wnotgross)
               term =log((arg+wgross)/(1.0+wgross))
             else
               term = exp_arg
             endif
             valqc = -2.0*rat_err2*term
!             print *, cg_term,cg_t,cvar_pg,term,valqc
             do j=1,nregion
!                print *,rdiag(ilon,i),rdiag(ilat,i),rlonmin(j),rlatmax(j)
                if(rdiag(ilon,i) >180.0) rdiag(ilon,i)=rdiag(ilon,i)-360.0
                if(rdiag(ilon,i)>=rlonmin(j) .and. rdiag(ilon,i)<=rlonmax(j) .and. &
                   rdiag(ilat,i)>=rlatmin(j) .and. rdiag(ilat,i)<=rlatmax(j) ) then
!                   print *,'j=',j
                   do k=1,np
                     if(rdiag(ipress,i) >=ptop(k) .and. rdiag(ipress,i) <= pbot(k))then
                       work(k,ltype,1,j,nn)=work(k,ltype,1,j,nn)+1.0
                       if(rdiag(iwgt,i) <1.0) work(k,ltype,2,j,nn)=work(k,ltype,2,j,nn)+1.0
                       if(trim(dtype) == '  q') then
                         ress=rdiag(iobg,i)*100.0/rdiag(iqsges,i)
                         work(k,ltype,3,j,nn)=work(k,ltype,3,j,nn)+ress
                       else if (trim(dtype) == ' uv') then
                          ress=sqrt(rdiag(iobgu,i)**2+rdiag(iobgv,i)**2)
                          ressu=rdiag(iobgu,i)
                          ressv=rdiag(iobgv,i)
                          work(k,ltype,3,j,nn)=work(k,ltype,3,j,nn)+spdb
                          worku(k,ltype,3,j,nn)=worku(k,ltype,3,j,nn)+ressu
                          workv(k,ltype,3,j,nn)=workv(k,ltype,3,j,nn)+ressv
                          worku(k,ltype,4,j,nn)=worku(k,ltype,4,j,nn)+ressu*ressu
                          workv(k,ltype,4,j,nn)=workv(k,ltype,4,j,nn)+ressv*ressv
                       else
                          ress=rdiag(iobg,i)
                          work(k,ltype,3,j,nn)=work(k,ltype,3,j,nn)+ress
                       endif
                       work(k,ltype,4,j,nn)=work(k,ltype,4,j,nn)+ress*ress
                       if(ltype ==1) then
!                         print *,rdiag(iobg,i),ress,work(k,ltype,4,j,nn),ltype,j,nn,work(k,ltype,1,j,nn) 
                       endif
                       work(k,ltype,5,j,nn)=work(k,ltype,5,j,nn)+val2*rat_err2
                       work(k,ltype,6,j,nn)=work(k,ltype,6,j,nn)+valqc
                     endif       !!! endif k
                   enddo        !!! enddo k
                endif     !!! endif region
             enddo         !!! enddo region
          endif        !!! endif ltype
        enddo         !!  enddo ltype

   enddo            !!! enddo i



    return
    end
