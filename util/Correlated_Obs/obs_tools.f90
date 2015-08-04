!This module contains routines used in cov_calc.f90
!Kristen Bathmann
!5-2015

module obs_tools

use kinds

implicit none
public:: dist
public:: get_filename
public:: iminv

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


subroutine iminv(a,n,d,l,m)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    iminv    invert a matrix
!
!   prgrmmr:
!
! abstract:      the standard gauss-jordan method is used. the determinant           
!                is also calculated. a determinant of zero indicates that            
!                the matrix is singular.                                             
!                                                                               
! remarks        matrix a must be a general matrix                                   
!
! program history log:
!   2008-06-04  safford -- add subprogram doc block
!
!   input argument list:
!     a - input matrix, destroyed in computation and replaced by resultant
!     inverse
!     n - order of matrix a                                               
!     d - resultant determinant                                           
!     l - work vector of length n                                         
!     m - work vector of length n                                         
!
!   output argument list:
!     a - input matrix, destroyed in computation and replaced by resultant
!     inverse
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

        use kinds,only: r_kind,i_kind
        implicit none
        integer(i_kind)             ,intent(in   ) :: n
        integer(i_kind),dimension(n),intent(inout) :: l,m
        real(r_kind)                ,intent(inout) :: d
        real(r_kind),dimension(n*n) ,intent(inout) :: a

        integer(i_kind):: nk,k,j,iz,i,ij
        integer(i_kind):: kj,ik,jr,jq,jk,ki,kk,jp,ji

        real(r_kind):: biga,hold
        real(r_kind):: zero=0.0_r_kind
        real(r_kind):: one=1.0_r_kind
!                                                                               
!        if a double precision version of this routine is desired, the          
!        ! in column 1 should be removed from the double precision              
!        statement which follows.                                               
!                                                                               
!     double precision a, d, biga, hold                                         
!                                                                               
!        the ! must also be removed from double precision statements            
!        appearing in other routines used in conjunction with this              
!        routine.                                                               
!                                                                               
!        the double precision version of this sr........ must also              
!        contain double precision fortran functions.  abs in statemen           
!        10 must be changed to dabs  .                                          
!                                                                               
!        ...............................................................        
!                                                                               
!        search for largest element                                             
!                                                                               
         d=one
         nk=-n
         do 80 k=1,n
            nk=nk+n
            l(k)=k
            m(k)=k
            kk=nk+k
            biga=a(kk)
            do 20 j=k,n
               iz=n*(j-1)
               do 20 i=k,n
                  ij=iz+i
!  10             if (dabs(biga)-dabs(a(ij))) 15,20,20
   10             if(abs(biga)-abs(a(ij))) 15,20,20
   15             biga=a(ij)
                  l(k)=i
                  m(k)=j
   20       continue
!
!        interchange rows
!
            j=l(k)
            if(j-k) 35,35,25
   25       ki=k-n
            do 30 i=1,n
               ki=ki+n
               hold=-a(ki)
               ji=ki-k+j
               a(ki)=a(ji)
   30          a(ji) =hold
!
!        interchange columns
!
   35          i=m(k)
               if(i-k) 45,45,38
   38          jp=n*(i-1)
               do 40 j=1,n
                  jk=nk+j
                  ji=jp+j
                  hold=-a(jk)
                  a(jk)=a(ji)
   40             a(ji) =hold
!        divide column by minus pivot (value of pivot element is
!        contained in biga)
!
   45             if(biga) 48,46,48
   46             d=zero
                  return
   48             do 55 i=1,n
                     if(i-k) 50,55,50
   50                ik=nk+i
                     a(ik)=a(ik)/(-biga)
   55             continue
!
!        reduce matrix
!
                  do 65 i=1,n
                     ik=nk+i
                     ij=i-n
                     do 65 j=1,n
                        ij=ij+n
                        if(i-k) 60,65,60
   60                   if(j-k) 62,65,62
   62                   kj=ij-i+k
                        a(ij)=a(ik)*a(kj)+a(ij)
   65             continue
!
!        divide row by pivot
!
                  kj=k-n
                  do 75 j=1,n
                     kj=kj+n
                     if(j-k) 70,75,70
   70                a(kj)=a(kj)/biga
   75             continue
!
!        product of pivots
!
                  d=d*biga
!
!        replace pivot by reciprocal
!
                  a(kk)=one/biga
   80    continue
!
!        final row and column interchange
!
         k=n
  100    k=(k-1)
         if(k) 150,150,105
  105    i=l(k)
         if(i-k) 120,120,108
  108    jq=n*(k-1)
         jr=n*(i-1)
         do 110 j=1,n
            jk=jq+j
            hold=a(jk)
            ji=jr+j
            a(jk)=-a(ji)
  110       a(ji) =hold
  120       j=m(k)
            if(j-k) 100,100,125
  125       ki=k-n
            do 130 i=1,n
               ki=ki+n
               hold=a(ki)
               ji=ki-k+j
               a(ki)=-a(ji)
  130          a(ji) =hold
!
               go to 100
  150          return
      end subroutine iminv
                                   

end module obs_tools

