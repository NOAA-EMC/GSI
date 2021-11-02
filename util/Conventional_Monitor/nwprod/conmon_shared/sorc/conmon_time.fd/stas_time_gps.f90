!! the subroutine is to calculate the statistics for gps data

subroutine stascal_gps(dtype,rdiag,nreal,n,iotype,varqc,ntype,work,&
                   np,htop,hbot,nregion,mregion,&
                   rlatmin,rlatmax,rlonmin,rlonmax,iosubtype)

   implicit none

   real(4),dimension(nreal,n) :: rdiag
   real(4),dimension(100,2) :: varqc
   real(4),dimension(np,100,6,nregion,3) :: work
   real(4),dimension(np) :: htop,hbot
   real(4),dimension(mregion):: rlatmin,rlatmax,rlonmin,rlonmax

   character(3) :: dtype
  
   integer,dimension(100) :: iotype,iosubtype
   integer itype,isubtype,ilat,ilon,iheight,iqc,iuse,imuse
   integer iwgt,ierr1,ierr2,ierr3,iobg,iobgu,iobgv,iqsges
   integer iobsu,iobsv,i,nregion,mregion,np,n,nreal,k,j
   integer ltype,ntype,intype,insubtype,nn, test_height
   real cg_term,pi,tiny
   real valu,valv,val,val2,gesu,gesv,spdb,exp_arg,arg
   real ress,ressu,ressv,valqc,term,wgross,cg_t,wnotgross
   real cvar_pg,cvar_b,rat_err2

   itype=1;isubtype=2;ilat=3;ilon=4;iheight=7;iqc=9;iuse=11;imuse=12
   iwgt=13;ierr1=14;ierr2=15;ierr3=16;iobg=18;iobgu=18;iobgv=21;iqsges=20
   iobsu=17;iobsv=20

   pi=acos(-1.0)
   cg_term=sqrt(2.0*pi)/2.0
   tiny=1.0e-10

 
   print *,'--> stascal_gps'
   
   do i=1,n
      val=rdiag(iobg,i)*rdiag(ierr1,i)
      val2     = val*val

      exp_arg  = -0.5*val2
      if ( rdiag(ierr3,i) <1.0e10 .and. rdiag(ierr1,i) <1.0e10 .and. rdiag(ierr1,i) >1.0e-10 ) then
         rat_err2 = (rdiag(ierr3,i)/rdiag(ierr1,i))**2
      else
         rat_err2 =0.0
      endif
  
      if(rdiag(imuse,i) >0.0) then
         nn=1
      else
         nn=2
         if(rdiag(ierr3,i) >tiny) nn=3
      endif

      do ltype=1,ntype
         intype=int(rdiag(itype,i))
         insubtype=int(rdiag(isubtype,i))
          
         if(intype == iotype(ltype)) then

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

            do j=1,nregion
               if(rdiag(ilon,i) >180.0) rdiag(ilon,i)=rdiag(ilon,i)-360.0

               if(rdiag(ilon,i)>=rlonmin(j) .and. rdiag(ilon,i)<=rlonmax(j) .and. &
                  rdiag(ilat,i)>=rlatmin(j) .and. rdiag(ilat,i)<=rlatmax(j) ) then

                  do k=1,np
                     test_height = rdiag(iheight,i)/1000
                     if(test_height >=htop(k) .and. test_height <= hbot(k))then

                        work(k,ltype,1,j,nn)=work(k,ltype,1,j,nn)+1.0
                         
                        if(rdiag(iwgt,i) <1.0) work(k,ltype,2,j,nn)=work(k,ltype,2,j,nn)+1.0

                        ress=rdiag(iobg,i)
                        work(k,ltype,3,j,nn)=work(k,ltype,3,j,nn)+ress
                     endif

                     work(k,ltype,4,j,nn)=work(k,ltype,4,j,nn)+ress*ress
                     work(k,ltype,5,j,nn)=work(k,ltype,5,j,nn)+val2*rat_err2
                     work(k,ltype,6,j,nn)=work(k,ltype,6,j,nn)+valqc
                  enddo  !! enddo k
               endif     !! endif j
            enddo        !! j
         endif           !! endif ltype
      enddo              !! enddo ltype

   enddo            !!! enddo i


   print *,'<-- stascal_gps'
   return
end
