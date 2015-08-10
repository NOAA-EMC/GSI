module matrix_tools

implicit none
public:: iminv
public:: eigdecomp
public:: recondition

contains

subroutine iminv(a,n,d,l,m)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    iminv    invert a matrix
!
!   prgrmmr:
!
! abstract:      the standard gauss-jordan method is used. the
! determinant
!                is also calculated. a determinant of zero indicates
!                that
!                the matrix is singular.
!
! remarks        matrix a must be a general matrix
!
! program history log:
!   2008-06-04  safford -- add subprogram doc block
!
!   input argument list:
!     a - input matrix, destroyed in computation and replaced by
!     resultant
!     inverse
!     n - order of matrix a
!     d - resultant determinant
!     l - work vector of length n
!     m - work vector of length n
!
!   output argument list:
!     a - input matrix, destroyed in computation and replaced by
!     resultant
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
                  if(abs(biga)-abs(a(ij))) 15,20,20
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
               go to 100
  150          return
end subroutine iminv

subroutine eigdecomp(Ain,n,D,Q)
!performs an eigendecomposition of a symmetric matrix
!input:  
!Ain, a symmetric matrix of size n
!n, dim of Ain
!
!output: 
!D, vector containing the eigenvalues of Ain, in no particular order
!Q, the columns of Q contain the eigenvectors of Ain
!
!Kristen Bathmann
!8-2015

use kinds, only: r_kind, i_kind
use constants, only: zero, one, four_int, one_hundred
implicit none

real(r_kind),dimension(:,:),intent(in):: Ain
integer(i_kind),intent(in):: n
real(r_kind),dimension(:),intent(out):: D
real(r_kind),dimension(:,:),intent(out):: Q
real(r_kind),dimension(:,:),allocatable:: A
integer(i_kind):: r,c,i,j
real(r_kind):: num, threshold, dd,gg,rr,qq,r1
real(r_kind):: gc,gr,gdd,a1,a2
real(r_kind),dimension(:), allocatable::Bv,Zv
integer(i_kind),parameter:: niter=50
real(r_kind):: tt, ss, cc

allocate(Bv(500),Zv(500))
allocate(A(n,n))
A=Ain
Q=zero
do r=1,n
   Q(r,r)=one
end do

do r=1,n
   Bv(r)=A(r,r)
   D(r)=Bv(r)
   Zv(r)=zero
end do

do i=1,niter
   num=zero
   do r=1,n-1
      do c=r+1,n
         num=num+abs(A(r,c))
      end do
   end do
   if(num==zero) return
   threshold=zero
   if(i<four_int) threshold=0.2*num/n**2
   do r=1,n-1
      do c=r+1,n
         gg=one_hundred*abs(A(r,c))
         gr=gg+abs(D(r))
         gc=gg+abs(D(c))
         if ((gr==abs(D(r))).and.(gc==abs(D(c))).and.(i>four_int)) then
            A(r,c)=zero
         else if (abs(A(r,c))>threshold) then
            dd=D(c)-D(r)
            gdd=abs(dd)+gg
            if (gdd==abs(dd)) then
               rr=A(r,c)/dd
            else
               qq=0.5*dd/A(r,c)
               r1=one+(qq**2)
               rr=one/(abs(qq)+sqrt(r1))
               if (qq<zero) rr=-rr
            end if
            r1=one+(rr**2)
            cc=one/sqrt(r1)
            ss=rr*cc
            tt=ss/(one+cc)
            dd=rr*A(r,c)
            Zv(r)=Zv(r)-dd
            Zv(c)=Zv(c)+dd
            D(r)=D(r)-dd
            D(c)=D(c)+dd
            A(r,c)=zero
            do j=1,r-1
               a1=A(j,r)
               a2=A(j,c)
               A(j,r)=a1-ss*(a2+tt*a1)
               A(j,c)=a2+ss*(a1-tt*a2)
            end do
            do j=r+1,c-1
               a1=A(r,j)
               a2=A(j,c)
               A(r,j)=a1-ss*(a2+a1*tt)
               A(j,c)=a2+ss*(a1-a2*tt)
            end do
            do j=c+1,n
               a1=A(r,j)
               a2=A(c,j)
               A(r,j)=a1-ss*(a2+tt*a1)
               A(c,j)=a2+ss*(a1-tt*a2)
            end do
            do j=1,n
               a1=Q(j,r)
               a2=Q(j,c)
               Q(j,r)=a1-ss*(a2+tt*a1)
               Q(j,c)=a2+ss*(a1-tt*a2)
            end do
          end if
      end do
   end do
   do r=1,n
      Bv(r)=Bv(r)+Zv(r)
      Zv(r)=zero
      D(r)=Bv(r)
   end do

end do
end subroutine eigdecomp

subroutine recondition(Q,D,n,kreq,A)
use kinds, only: r_kind, i_kind
use constants, only: zero
implicit none
real(r_kind),dimension(:),intent(in):: D
real(r_kind),dimension(:,:),intent(in):: Q
real(r_kind),intent(in):: kreq
integer(i_kind),intent(in):: n
real(r_kind),dimension(:,:),allocatable:: Dn
real(r_kind),dimension(:,:),intent(out):: A
real(r_kind):: mx, mn
real(r_kind):: laminc
integer:: i
allocate(Dn(n,n))
Dn=zero
mn=D(1)
mx=D(1)
do i=2,n
   if (D(i)<=mn) mn=D(i)
   if (D(i)>=mx) mx=D(i)
end do
laminc=(mx-mn*kreq)/(kreq-1)
print *, 'max', mx
print *, 'min', mn
print *, 'incr', laminc
do i=1,n
   Dn(i,i)=D(i)+laminc
end do
A=MATMUL(Q,(MATMUL(Dn,TRANSPOSE(Q))))
end subroutine recondition



end module matrix_tools
