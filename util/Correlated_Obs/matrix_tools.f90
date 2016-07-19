module matrix_tools

implicit none
public:: eigdecomp
public:: recondition

contains

subroutine eigdecomp(Ain,n,D,Q)
!this subroutine uses the Jacobi rotation method to
!perform an eigendecomposition of a symmetric matrix
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


subroutine recondition(Q,D,R,n,Xd,kreq,A,method)
!This subroutine reconditions the covariance matrix
!based on a user's choice of method.
!It is necessary to preform an eigendecompositon first
!Kristen Bathmann 
!8-2015
use kinds, only: r_kind, i_kind
use constants, only: zero
implicit none
real(r_kind),dimension(:),intent(in):: D     !eigenvalues
real(r_kind),dimension(:,:),intent(in):: Q   !eigenvectors
real(r_kind),dimension(:,:),intent(in):: R   !original Rcov
real(r_kind),intent(in):: Xd
real(r_kind),intent(in):: kreq               !condition number
integer(i_kind),intent(in):: n               !number of channels
real(r_kind),dimension(:,:),allocatable:: Dn !new eigenvalues
real(r_kind),dimension(:,:),intent(out):: A  !reconditioned covariance
real(r_kind),dimension(:,:),allocatable:: R2
integer,intent(in)::method
real(r_kind):: mx, mn, m,r12,r22,dc,K
real(r_kind):: laminc, a1, a2, nreal
integer:: i,j,coun, dw
integer,parameter:: trace=1
integer,parameter:: weston2=2
integer,parameter:: shrinkage=3
allocate(Dn(n,n))
Dn=zero
mn=D(1)
mx=D(1)
do i=2,n
   if (D(i)<=mn) mn=D(i)
   if (D(i)>=mx) mx=D(i)
end do
if (method==weston2) then
   laminc=(mx-mn*kreq)/(kreq-1)
   do i=1,n
      Dn(i,i)=D(i)+laminc
   end do
   A=MATMUL(Q,(MATMUL(Dn,TRANSPOSE(Q))))
else if (method==trace) then
   K=kreq
   dw=0
   do while (dw==0)
      coun=0
      laminc=mx/K
      do i=1,n
         if (D(i)<=laminc) then
            Dn(i,i)=laminc
            coun=coun+1
         else
            Dn(i,i)=D(i)
         end if
      end do
      dw=1
      if (coun>K) then
         dw=0
         K=K+1
      end if
   end do
      
   A=MATMUL(Q,(MATMUL(Dn,TRANSPOSE(Q))))
else if (method==shrinkage) then
   nreal=real(n,r_kind)
   allocate(R2(n,n))
   R2=R
   m=0
   dc=0
   do i=1,n
      m=m+D(i)
   end do
   r22=Xd
   m=m/nreal
   do i=1,n
      R2(i,i)=R2(i,i)-m
   end do
   R2=MATMUL(R2,TRANSPOSE(R2))
   do i=1,n
      dc=dc+R2(i,i)
   end do
   dc=dc/nreal
   r12=dc-r22
   a1=r22*m/dc
   a2=r12/dc
   do i=1,n
      do j=1,n
         A(i,j)=R(i,j)*a2
      end do
      A(i,i)=A(i,i)+a1
   end do 
end if

end subroutine recondition

end module matrix_tools
