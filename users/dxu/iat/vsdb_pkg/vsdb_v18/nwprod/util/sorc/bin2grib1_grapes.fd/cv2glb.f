!--convert data in regional domain to global lat-lon grid for making grib1 data

!--source data in regional domain
integer , parameter :: imr=629,jmr=399
real(kind=4) , parameter :: xrs=53.13, dxr=0.117, xre=xrs+dxr*imr
real(kind=4) , parameter :: yrs=90+12.97, dyr=0.089, yre=yrs+dyr*jmr
real(kind=4) ::  ur(imr,jmr), xr(imr), yr(jmr)
!--data in global domain  
real(kind=4), allocatable ::  ug(:,:),xg(:),yg(:)

logical log1,log2,log3
real(kind=4) bad                          
data bad/9999.000/

!----------------
do i=1,imr
 xr(i)=xrs+(i-1)*dxr
enddo
do j=1,jmr
 yr(j)=yrs+(j-1)*dyr
enddo
print*,"imr,jmr,dxr,dyr",imr,jmr,dxr,dyr
print*,"xrs,xre,yrs,yre",xrs,xre,yrs,yre
print*,"xr: ",xr
print*,"yr: ",yr

!--determine latitude and longitude points over the globe, 
!--the grid has a mesh close to original data resolution
dxg=0.15; dyg=0.1                
img=int(360.0/dxg); jmg=int(180.0/dyg)+1
print*,"img,jmg,dxg,dyg",img,jmg,dxg,dyg

allocate ( ug(img,jmg),xg(img), yg(jmg) )
do i=1,img
 xg(i)=(i-1)*dxg
enddo
do j=1,jmg
 yg(j)=(j-1)*dyg
enddo
print*,"xg: ",xg
print*,"yg: ",yg

!---
open(1,file="postvar2014052700",form="unformatted", convert="big_endian",status="unknown")
read(1) ((ur(i,j),i=1,imr),j=1,jmr)

!--project regional data to global domain 
ug=bad
is=max(1,int(xrs/dxg)-1); ie=min(img,int(xre/dxg)+1)
js=max(1,int(yrs/dyg)-1); je=min(jmg,int(yre/dyg)+1)
  print*,"is,ie,js,je: ",is,ie,js,je
do m=1,imr-1
do n=1,jmr-1
 log1=(ur(m,n).ne.bad .and. ur(m+1,n).ne.bad .and. ur(m,n+1).ne.bad .and.  ur(m+1,n+1).ne.bad )
if (log1) then
 do i=is,ie
 log2=( xg(i).ge.xr(m) .and. xg(i).lt.xr(m+1) )
  if(log2) then
  do j=js,je
   log3=( yg(j).ge.yr(n) .and. yg(j).lt.yr(n+1) )
   if(log3) then
    ug(i,j)=0.25*(ur(m,n)+ur(m+1,n)+ur(m,n+1)+ur(m+1,n+1))
   endif
  enddo
  endif
 enddo
endif
enddo
enddo

  
open(2,file="postvar2014052700g",form="unformatted", convert="big_endian",status="unknown")
write(2) ((ug(i,j),i=1,img),j=1,jmg)



deallocate ( ug,xg, yg)
end
