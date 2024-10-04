procedure fill_nmm_grid2(fin[*][*]:float,nx:integer,ny:integer,fout[*][*]:float,igtype:integer)
  
local i,im,ip,j,jm,jp,fill,test
  
begin

  gin   = new ((/ny+1,nx+1/), float)
  b     = new ((/ny+1,2*nx/), float)

  gin(1:ny,1:nx)=fin(:,:)

  fill=99999.9
  test=0.95*fill
  do j=1,ny
     do i=1,2*nx-1
        b(j,i)=fill
     end do
  end do
  
; First transfer all staggered points to appropriate
; points on filled output grid
  if(igtype.eq.1) then
     do j=1,ny,2
        do i=1,nx
           b(j,2*i-1)=gin(j,i)
        end do
     end do
     do j=2,ny,2
        do i=1,nx-1
           b(j,2*i)=gin(j,i)
        end do
     end do
  else
     do j=1,ny,2
        do i=1,nx-1
           b(j,2*i)=gin(j,i)
        end do
     end do
     do j=2,ny,2
        do i=1,nx
           b(j,2*i-1)=gin(j,i)
        end do
     end do
  end if

  
;  Now fill in holes

; Top and bottom rows:
  do j=1,ny,ny-1
     do i=1,2*nx-1
        if(b(j,i).gt.test) then
           ip=i+1
           if(ip.gt.2*nx-1) then
             ip=i-1
           end if
           im=i-1
           if(im.lt.1) then
            im=i+1
           end if
           b(j,i)=0.5*(b(j,im)+b(j,ip))
        end if
     end do
  end do

  
; Left and right rows:
  do j=1,ny
     jp=j+1
     if(jp.gt.ny) then
       jp=j-1
     end if
     jm=j-1
     if(jm.lt.1) then
       jm=j+1
     end if
     do i=1,2*nx-1,2*nx-2
        if(b(j,i).gt.test) then
           b(j,i)=0.5*(b(jm,i)+b(jp,i))
        end if
     end do
  end do

  
; Interior points
  do j=1,ny
     jp=j+1
     if(jp.gt.ny) then
        jp=j-1
     end if
     jm=j-1 
     if(jm.lt.1) then
       jm=j+1
     end if
     do i=1,2*nx-1
        if(b(j,i).gt.test) then
           ip=i+1
           if(ip.gt.2*nx-1) then
             ip=i-1
           end if
           im=i-1 
           if(im.lt.1) then
             im=i+1
           end if
           b(j,i)=0.25*(b(j,ip)+b(j,im)+b(jp,i)+b(jm,i))
        end if
     end do
  end do
  
  fout(:,:)=b(3:ny-2,3:2*nx-1-2)

end
