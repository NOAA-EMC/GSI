module obs_tools

use kinds

implicit none
public:: dist
public:: get_filename

contains

real function dist (po1, po2)
!This function takes two points, whose positions are specified by a latitude and
!longitude, and computes the distance, in km, between the two by converting to
!cartesian coordinates
implicit none

real(r_kind),dimension(2),intent(in):: po1,po2       !the two points, given by (lat,lon)
real(r_kind), parameter:: rad=6378.137_r_kind        !radius of the earth
real(r_kind), parameter:: pi=3.1415926535898_r_kind
real(r_kind):: x1,y1,z1, x2, y2, z2                   !cartesian coordinates
real(r_kind)::d1
real(r_kind):: sinphi1, cosphi1, costhe1, sinthe1    !trig functions related to po1
real(r_kind):: sinphi2, cosphi2, costhe2,sinthe2     !trig functions related to po2
real(r_kind),dimension(2):: p1, p2                   !manipulations on po1 and po2


p1(1)=(90.0d0-po1(1))*pi/180.0d0 !phi
p2(1)=(90.0d0-po2(1))*pi/180.0d0 !phi
p1(2)=po1(2)*pi/180.0d0 !theta
p2(2)=po2(2)*pi/180.0d0 !theta

sinphi1=sin(p1(1))
sinphi2=sin(p2(1))
cosphi1=cos(p1(1))
cosphi2=cos(p2(1))
costhe1=cos(p1(2))
costhe2=cos(p2(2))
sinthe1=sin(p1(2))
sinthe2=sin(p2(2))

x1=sinphi1*costhe1
x2=sinphi2*costhe2
y1=sinphi1*sinthe1
y2=sinphi2*sinthe2
z1=cosphi1
z2=cosphi2

d1=(x1-x2)**2+(y1-y2)**2+(z1-z2)**2
dist=rad*sqrt(d1)
end function dist


subroutine get_filename(T,ext,filename)
!At a given time step T, this subroutine outputs the name of the file to be read
!in.  The file will either be an analysis diag file or a ges diag file
!(specified by ext)
implicit none
integer,intent(in):: T                  !Time step of diag file to be read in
character(5),intent(in)::ext            !specifies either anl or ges diag file
character(9),intent(out):: filename 
real:: tem
integer:: t1i,t2i,t3i, t4i
character(1)::t1,t2,t3, t4
integer, parameter::one=1

tem=T/1000
t1i=floor(tem)
tem=(T-1000*t1i)/100
t2i=floor(tem)
tem=(T-1000*t1i-100*t2i)/10
t3i=floor(tem)
t4i=T-1000*t1i-100*t2i-10*t3i
t1=ACHAR(t1i+48)
t2=ACHAR(t2i+48)
t3=ACHAR(t3i+48)
t4=ACHAR(t4i+48)
filename(1:5)=ext
filename(6:6)=t1
filename(7:7)=t2
filename(8:8)=t3
filename(9:9)=t4
end subroutine get_filename
end module obs_tools

