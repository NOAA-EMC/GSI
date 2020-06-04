!
!                                **********************************************
!                                *             MODULE peuc                    *
!                                *  R. J. Purser, NOAA/NCEP/EMC      Oct 2005 * 
!                                *                              18th May 2012 *
!                                *  jim.purser@noaa.gov                       *
!                                *                                            *
!                                **********************************************
!
!  Euclidean geometry, geometric (stereographic) projections,
!              related transformations (Mobius).
! Package for handy vector and matrix operations in Euclidean geometry.
! This package is primarily intended for 3D operations and three of the
! functions (Cross_product, Triple_product and Axial) do not possess simple
! generalizations to a generic number N of dimensions. The others, while
! admitting such N-dimensional generalizations, have not all been provided
! with such generic forms here at the time of writing, though some of these 
! may be added at a future date.
!
! May 2017: Added routines to facilitate manipulation of 3D rotations,
! their representations by axial vectors, and routines to compute the
! exponentials of matrices (without resort to eigen methods). Also added
! Quaternion and spinor representations of 3D rotations, and their 
! conversion routines.
!
!   FUNCTION:
! absv:           Absolute magnitude of vector as its euclidean length
! Normalized:     Normalized version of given real vector
! Orthogonalized: Orthogonalized version of second vector rel. to first unit v.
! Cross_product:  Vector cross-product of the given 2 vectors
! Outer_product:  outer-product matrix of the given 2 vectors
! Triple_product: Scalar triple product of given 3 vectors
! Det:            Determinant of given matrix
! Axial:          Convert axial-vector <--> 2-form (antisymmetric matrix)
! Diag:           Diagnl of given matrix, or diagonal matrix of given elements
! Trace:          Trace of given matrix
! Identity:       Identity 3*3 matrix, or identity n*n matrix for a given n
! Sarea:          Spherical area subtended by three vectors
! Huarea:         Spherical area subtended by right-angled spherical triangle
!   SUBROUTINE:
! Gram:           Right-handed orthogonal basis and rank, nrank. The first 
!                 nrank basis vectors span the column range of matrix given,
!                 OR  ("plain" version) simple unpivoted Gram-Schmidt of a
!                 square matrix.
!
! In addition, we include routines that relate to stereographic projections
! and some associated mobius transformation utilities, since these complex
! operations have a strong geometrical flavor.
!
! DIRECT DEPENDENCIES
! Libraries[their Modules]: pmat[pmat]
! Additional Modules      : kinds, pietc
!
!============================================================================
module peuc
!============================================================================
use kinds, only: sp,dp,dpc,i_kind
implicit none
private
public:: absv,normalized,orthogonalized,                        &
         cross_product,outer_product,triple_product,det,axial,  &
         diag,trace,identity,sarea,huarea,                      &
         normalize,gram,rowops,corral,                          &
         axtoq,qtoax,                                           &
         rottoax,axtorot,spintoq,qtospin,rottoq,qtorot,mulqq,   &
         expmat,zntay,znfun,                                    &
         ctoz,ztoc,setmobius,                                   &
         mobius,mobiusi

interface absv;      module procedure absv_s,absv_d;            end interface
interface normalized;module procedure normalized_s,normalized_d;end interface
interface orthogonalized
   module procedure orthogonalized_s,orthogonalized_d;          end interface
interface cross_product
   module procedure cross_product_s,cross_product_d;            end interface
interface outer_product
   module procedure outer_product_s,outer_product_d,outer_product_i
                                                                end interface
interface triple_product
   module procedure triple_product_s,triple_product_d;          end interface
interface det; module procedure det_s,det_d,det_i,det_id;       end interface
interface axial
   module procedure axial3_s,axial3_d,axial33_s,axial33_d;      end interface
interface diag
   module procedure diagn_s,diagn_d,diagn_i,diagnn_s,diagnn_d,diagnn_i
                                                                end interface
interface trace;    module procedure trace_s,trace_d,trace_i;   end interface
interface identity; module procedure identity_i,identity3_i;    end interface
interface huarea;   module procedure huarea_s,huarea_d;         end interface
interface sarea;    module procedure sarea_s,sarea_d;           end interface
interface normalize;module procedure normalize_s,normalize_d;   end interface
interface gram
   module procedure gram_s,gram_d,graml_d,plaingram_s,plaingram_d,rowgram
                                                                end interface
interface rowops;   module procedure rowops;                    end interface
interface corral;   module procedure corral;                    end interface
interface rottoax;  module procedure rottoax;                   end interface
interface axtorot;  module procedure axtorot;                   end interface
interface spintoq;  module procedure spintoq;                   end interface
interface qtospin;  module procedure qtospin;                   end interface
interface rottoq;   module procedure rottoq;                    end interface
interface qtorot;   module procedure qtorot;                    end interface
interface axtoq;    module procedure axtoq;                     end interface
interface qtoax;    module procedure qtoax;                     end interface
interface setem;    module procedure setem;                     end interface
interface mulqq;    module procedure mulqq;                     end interface
interface expmat;   module procedure expmat,expmatd,expmatdd;   end interface
interface zntay;    module procedure zntay;                     end interface
interface znfun;    module procedure znfun;                     end interface
interface ctoz;     module procedure ctoz;                      end interface
interface ztoc;     module procedure ztoc,ztocd;                end interface
interface setmobius;module procedure setmobius,zsetmobius;      end interface
interface mobius;   module procedure zmobius,cmobius;           end interface
interface mobiusi;  module procedure zmobiusi;                  end interface

contains

!=============================================================================
function absv_s(a)result(s)!                                            [absv]
!=============================================================================
implicit none

real(sp),dimension(:),intent(in):: a
real(sp)                        :: s
s=sqrt(dot_product(a,a))
end function absv_s
!=============================================================================
function absv_d(a)result(s)!                                            [absv]
!=============================================================================
implicit none
real(dp),dimension(:),intent(in):: a
real(dp)                        :: s
s=sqrt(dot_product(a,a))
end function absv_d

!=============================================================================
function normalized_s(a)result(b)!                                [normalized]
!=============================================================================
implicit none
real(sp),dimension(:),intent(IN):: a
real(sp),dimension(size(a))     :: b
real(sp)                        :: s
s=absv_s(a); if(s==0_dp)then; b=0_dp;else;b=a/s;endif
end function normalized_s
!=============================================================================
function normalized_d(a)result(b)!                                [normalized]
!=============================================================================
implicit none
real(dp),dimension(:),intent(IN):: a
real(dp),dimension(size(a))     :: b
real(dp)                        :: s
s=absv_d(a); if(s==0_dp)then; b=0_dp;else;b=a/s;endif
end function normalized_d

!=============================================================================
function orthogonalized_s(u,a)result(b)!                      [orthogonalized]
!=============================================================================
implicit none
real(sp),dimension(:),intent(in):: u,a
real(sp),dimension(size(u))     :: b
real(sp)                        :: s
! Note: this routine assumes u is already normalized
s=dot_product(u,a); b=a-u*s
end function orthogonalized_s
!=============================================================================
function orthogonalized_d(u,a)result(b)!                      [orthogonalized]
!=============================================================================
implicit none
real(dp),dimension(:),intent(in):: u,a
real(dp),dimension(size(u))     :: b
real(dp)                        :: s
! Note: this routine assumes u is already normalized
s=dot_product(u,a); b=a-u*s
end function orthogonalized_d

!=============================================================================
function cross_product_s(a,b)result(c)!                        [cross_product]
!=============================================================================
implicit none
real(sp),dimension(3),intent(in):: a,b
real(sp),dimension(3)           :: c
c(1)=a(2)*b(3)-a(3)*b(2); c(2)=a(3)*b(1)-a(1)*b(3); c(3)=a(1)*b(2)-a(2)*b(1)
end function cross_product_s
!=============================================================================
function cross_product_d(a,b)result(c)!                        [cross_product]
!=============================================================================
implicit none
real(dp),dimension(3),intent(in):: a,b
real(dp),dimension(3)           :: c
c(1)=a(2)*b(3)-a(3)*b(2); c(2)=a(3)*b(1)-a(1)*b(3); c(3)=a(1)*b(2)-a(2)*b(1)
end function cross_product_d

!=============================================================================
function outer_product_s(a,b)result(c)!                        [outer_product]
!=============================================================================
implicit none
real(sp),dimension(:),  intent(in ):: a
real(sp),dimension(:),  intent(in ):: b
real(sp),DIMENSION(size(a),size(b)):: c
integer(i_kind)                    :: nb,i
nb=size(b)
do i=1,nb; c(:,i)=a*b(i); enddo
end function outer_product_s
!=============================================================================
function outer_product_d(a,b)result(c)!                        [outer_product]
!=============================================================================
implicit none
real(dp),dimension(:),  intent(in ):: a
real(dp),dimension(:),  intent(in ):: b
real(dp),dimension(size(a),size(b)):: c
integer(i_kind)                    :: nb,i
nb=size(b)
do i=1,nb; c(:,i)=a*b(i); enddo
end function outer_product_d
!=============================================================================
function outer_product_i(a,b)result(c)!                        [outer_product]
!=============================================================================
implicit none
integer(i_kind),dimension(:),  intent(in ):: a
integer(i_kind),dimension(:),  intent(in ):: b
integer(i_kind),dimension(size(a),size(b)):: c
integer(i_kind)                           :: nb,i
nb=size(b)
do i=1,nb; c(:,i)=a*b(i); enddo
end function outer_product_i

!=============================================================================
function triple_product_s(a,b,c)result(tripleproduct)!        [triple_product]
!=============================================================================
implicit none
real(sp),dimension(3),intent(IN ):: a,b,c
real(sp)                         :: tripleproduct
tripleproduct=dot_product( cross_product(a,b),c )
end function triple_product_s
!=============================================================================
function triple_product_d(a,b,c)result(tripleproduct)!        [triple_product]
!=============================================================================
implicit none
real(dp),dimension(3),intent(IN ):: a,b,c
real(dp)                         :: tripleproduct
tripleproduct=dot_product( cross_product(a,b),c )
end function triple_product_d

!=============================================================================
function det_s(a)result(det)!                                            [det]
!=============================================================================
implicit none
real(sp),dimension(:,:),intent(IN )    ::a
real(sp)                               :: det
real(sp),dimension(size(a,1),size(a,1)):: b
integer(i_kind)                        :: n,nrank
n=size(a,1)
if(n==3)then
   det=triple_product(a(:,1),a(:,2),a(:,3))
else
   call gram(a,b,nrank,det)
   if(nrank<n)det=0_sp
endif
end function det_s
!=============================================================================
function det_d(a)result(det)!                                            [det]
!=============================================================================
implicit none
real(dp),dimension(:,:),intent(IN )    ::a
real(dp)                               :: det
real(dp),dimension(size(a,1),size(a,1)):: b
integer(i_kind)                        :: n,nrank
n=size(a,1)
if(n==3)then
   det=triple_product(a(:,1),a(:,2),a(:,3))
else
   call gram(a,b,nrank,det)
   if(nrank<n)det=0_dp
endif
end function det_d
!=============================================================================
function det_i(a)result(idet)!                                           [det]
!=============================================================================
implicit none
integer(i_kind), dimension(:,:),intent(IN )    :: a
integer(i_kind)                                :: idet
real(dp),        dimension(size(a,1),size(a,2)):: b
real(dp)                                       :: bdet
b=a; bdet=det(b); idet=nint(bdet)
end function det_i

!=============================================================================
function det_id(a)result(idet)!                                          [det]
!=============================================================================
use kinds, only: dp,dpi
implicit none
integer(dpi), dimension(:,:),intent(IN )     :: a
integer(dpi)                                 :: idet
real(dp),     dimension(size(a,1),size(a,2)) :: b
real(dp)                                     :: bdet
b=a; bdet=det(b); idet=nint(bdet)
end function det_id

!=============================================================================
function axial3_s(a)result(b)!                                         [axial]
!=============================================================================
implicit none
real(sp),dimension(3),intent(IN ):: a
real(sp),dimension(3,3)          :: b
b=0;b(3,2)=a(1);b(1,3)=a(2);b(2,1)=a(3);b(2,3)=-a(1);b(3,1)=-a(2);b(1,2)=-a(3)
end function axial3_s
!=============================================================================
function axial3_d(a)result(b)!                                         [axial]
!=============================================================================
implicit none
real(dp),dimension(3),intent(IN ):: a
real(dp),dimension(3,3)          :: b
b=0;b(3,2)=a(1);b(1,3)=a(2);b(2,1)=a(3);b(2,3)=-a(1);b(3,1)=-a(2);b(1,2)=-a(3)
end function axial3_d
!=============================================================================
function axial33_s(b)result(a)!                                        [axial]
!=============================================================================
implicit none
real(sp),dimension(3,3),intent(IN ):: b
real(sp),dimension(3)              :: a
a(1)=(b(3,2)-b(2,3))/2_sp; a(2)=(b(1,3)-b(3,1))/2_sp; a(3)=(b(2,1)-b(1,2))/2_sp
end function axial33_s
!=============================================================================
function axial33_d(b)result(a)!                                        [axial]
!=============================================================================
implicit none
real(dp),dimension(3,3),intent(IN ):: b
real(dp),dimension(3)              :: a
a(1)=(b(3,2)-b(2,3))/2_dp; a(2)=(b(1,3)-b(3,1))/2_dp; a(3)=(b(2,1)-b(1,2))/2_dp
end function axial33_d

!=============================================================================
function diagn_s(a)result(b)!                                           [diag]
!=============================================================================
implicit none
real(sp),dimension(:),intent(IN )  :: a
real(sp),dimension(size(a),size(a)):: b
integer(i_kind)                    :: n,i
n=size(a)
b=0_sp; do i=1,n; b(i,i)=a(i); enddo
end function diagn_s
!=============================================================================
function diagn_d(a)result(b)!                                           [diag]
!=============================================================================
implicit none
real(dp),dimension(:),intent(IN )  :: a
real(dp),dimension(size(a),size(a)):: b
integer(i_kind)                    :: n,i
n=size(a)
b=0_dp; do i=1,n; b(i,i)=a(i); enddo
end function diagn_d
!=============================================================================
function diagn_i(a)result(b)!                                           [diag]
!=============================================================================
implicit none
integer(i_kind),dimension(:),intent(IN )  :: a
integer(i_kind),dimension(size(a),size(a)):: b
integer(i_kind)                           :: n,i
n=size(a)
b=0; do i=1,n; b(i,i)=a(i); enddo
end function diagn_i
!=============================================================================
function diagnn_s(b)result(a)!                                          [diag]
!=============================================================================
implicit none
real(sp),dimension(:,:),intent(IN ):: b
real(sp),dimension(size(b,1))      :: a
integer(i_kind)                    :: n,i
n=size(b,1)
do i=1,n; a(i)=b(i,i); enddo
end function diagnn_s
!=============================================================================
function diagnn_d(b)result(a)!                                          [diag]
!=============================================================================
implicit none
real(dp),dimension(:,:),intent(IN ):: b
real(dp),dimension(size(b,1))      :: a
integer(i_kind)                    :: n,i
n=size(b,1)
do i=1,n; a(i)=b(i,i); enddo
end function diagnn_d
!=============================================================================
function diagnn_i(b)result(a)!                                          [diag]
!=============================================================================
implicit none
integer(i_kind),dimension(:,:),intent(IN ):: b
integer(i_kind),dimension(size(b,1))      :: a
integer(i_kind)                           :: n,i
n=size(b,1)
do i=1,n; a(i)=b(i,i); enddo
end function diagnn_i

!=============================================================================
function trace_s(b)result(s)!                                          [trace]
!=============================================================================
implicit none
real(sp),dimension(:,:),intent(IN ):: b
real(sp)                           :: s
s=sum(diag(b))
end function trace_s
!=============================================================================
function trace_d(b)result(s)!                                          [trace]
!=============================================================================
implicit none
real(dp),dimension(:,:),intent(IN ):: b
real(dp)                           :: s
s=sum(diag(b))
end function trace_d
!=============================================================================
function trace_i(b)result(s)!                                         [trace]
!=============================================================================
implicit none
integer(i_kind),dimension(:,:),intent(IN ):: b
integer(i_kind)                           :: s
s=sum(diag(b))
end function trace_i

!=============================================================================
function identity_i(n)result(a)!                                    [identity]
!=============================================================================
implicit none
integer(i_kind),intent(IN )   :: n
integer(i_kind),dimension(n,n):: a
integer(i_kind)               :: i
a=0; do i=1,n; a(i,i)=1; enddo
end function identity_i
!=============================================================================
function identity3_i()result(a)!                                    [identity]
!=============================================================================
implicit none
integer(i_kind),dimension(3,3):: a
integer(i_kind)               :: i
a=0; do i=1,3; a(i,i)=1; enddo
end function identity3_i

!=============================================================================
function huarea_s(sa,sb)result(area)!                                 [huarea]
!=============================================================================
implicit none
real(sp),intent(IN ):: sa,sb
real(sp)            :: area
real(sp)            :: ca,cb
!-----------------------------------------------------------------------------
ca=sqrt(1_sp-sa**2)
cb=sqrt(1_sp-sb**2)
area=asin(sa*sb/(1_sp+ca*cb))
end function huarea_s

!=============================================================================
function huarea_d(sa,sb)result(area)!                                 [huarea]
!=============================================================================
implicit none
real(dp),intent(IN ):: sa,sb
real(dp)            :: area
real(dp)            :: ca,cb
!-----------------------------------------------------------------------------
ca=sqrt(1_dp-sa**2)
cb=sqrt(1_dp-sb**2)
area=asin(sa*sb/(1_dp+ca*cb))
end function huarea_d

!=============================================================================
function sarea_s(v1,v2,v3)result(area)!                                [sarea]
!=============================================================================
! Compute the area of the spherical triangle, {v1,v2,v3}, measured in the
! right-handed sense, by dropping a perpendicular to u0 on the longest side so
! that the area becomes the sum of areas of the two simpler right-angled
! triangles.
!=============================================================================
implicit none
real(sp),dimension(3),intent(IN ):: v1,v2,v3
real(sp)                         :: area
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
real(sp)                         :: s123,a1,a2,b,d1,d2,d3
real(sp),dimension(3)            :: u0,u1,u2,u3,x,y
!=============================================================================
area=0_sp
u1=normalized(v1); u2=normalized(v2); u3=normalized(v3)
s123=triple_product(u1,u2,u3)
if(s123==0_sp)return

d1=dot_product(u3-u2,u3-u2)
d2=dot_product(u1-u3,u1-u3)
d3=dot_product(u2-u1,u2-u1)

! Triangle that is not degenerate. Cyclically permute, so side 3 is longest:
if(d3<d1 .or. d3<d2)call cyclic(u1,u2,u3,d1,d2,d3)
if(d3<d1 .or. d3<d2)call cyclic(u1,u2,u3,d1,d2,d3)
y=normalized( cross_product(u1,u2) )
b=dot_product(y,u3)
u0=normalized( u3-y*b )
x=cross_product(y,u0) 
a1=-dot_product(x,u1-u0); a2= dot_product(x,u2-u0)
area=huarea(a1,b)+huarea(a2,b)

contains
!-----------------------------------------------------------------------------
   subroutine cyclic(u1,u2,u3,d1,d2,d3)
!-----------------------------------------------------------------------------
   implicit none
   real(sp),dimension(3),intent(INOUT):: u1,u2,u3
   real(sp),             intent(INOUT):: d1,d2,d3
   real(sp),dimension(3)              :: ut
   real(sp)                           :: dt
   dt=d1; d1=d2; d2=d3; d3=dt
   ut=u1; u1=u2; u2=u3; u3=ut
   end subroutine cyclic
end function sarea_s

!=============================================================================
function sarea_d(v1,v2,v3)result(area)!                                [sarea]
!=============================================================================
implicit none
real(dp),dimension(3),intent(IN ):: v1,v2,v3
real(dp)                         :: area
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
real(dp)                         :: s123,a1,a2,b,d1,d2,d3
real(dp),dimension(3)            :: u0,u1,u2,u3,x,y
!=============================================================================
area=0_dp
u1=normalized(v1); u2=normalized(v2); u3=normalized(v3)
s123=triple_product(u1,u2,u3)
if(s123==0_dp)return

d1=dot_product(u3-u2,u3-u2)
d2=dot_product(u1-u3,u1-u3)
d3=dot_product(u2-u1,u2-u1)

! Triangle that is not degenerate. Cyclically permute, so side 3 is longest:
if(d3<d1 .or. d3<d2)call cyclic(u1,u2,u3,d1,d2,d3)
if(d3<d1 .or. d3<d2)call cyclic(u1,u2,u3,d1,d2,d3)
y=normalized( cross_product(u1,u2) )
b=dot_product(y,u3)
u0=normalized( u3-y*b )
x=cross_product(y,u0) 
a1=-dot_product(x,u1-u0); a2= dot_product(x,u2-u0)
area=huarea(a1,b)+huarea(a2,b)

contains
!-----------------------------------------------------------------------------
   subroutine cyclic(u1,u2,u3,d1,d2,d3)
!-----------------------------------------------------------------------------
   implicit none
   real(dp),dimension(3),intent(INOUT):: u1,u2,u3
   real(dp),             intent(INOUT):: d1,d2,d3
   real(dp),dimension(3)              :: ut
   real(dp)                           :: dt
   dt=d1; d1=d2; d2=d3; d3=dt
   ut=u1; u1=u2; u2=u3; u3=ut
   end subroutine cyclic
end function sarea_d

!=============================================================================
subroutine normalize_s(v)!                                         [normalize]
!=============================================================================
implicit none
! Normalize the given vector.
real(sp),dimension(:),intent(inout):: v
real(sp)                           :: s
s=absv(v); if(s==0_sp)then; v=0_sp; v(1)=1_sp; else; v=v/s; endif
end subroutine normalize_s
!=============================================================================
subroutine normalize_d(v)!                                         [normalize]
!=============================================================================
implicit none
real(dp),dimension(:),intent(inout):: v
real(dp)                           :: s
s=absv(v); if(s==0_dp)then; v=0_dp; v(1)=1_dp; else; v=v/s; endif
end subroutine normalize_d

!=============================================================================
subroutine gram_s(as,b,nrank,det)!                                      [gram]
!=============================================================================
implicit none
real(sp),dimension(:,:),intent(IN )      :: as
real(sp),dimension(:,:),intent(OUT)      :: b
integer(i_kind),        intent(OUT)      :: nrank
real(sp),               intent(OUT)      :: det
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
real(sp),parameter                       :: crit=1.e-5_sp
real(sp),dimension(size(as,1),size(as,2)):: a
real(sp),dimension(size(as,2),size(as,1)):: ab
real(sp),dimension(size(as,1))           :: tv,w
real(sp)                                 :: val,s,vcrit
integer(i_kind)                          :: i,j,k,l,m,n
integer(i_kind),dimension(2)             :: ii
!=============================================================================
n=size(as,1)
m=size(as,2)
if(n/=size(b,1) .or. n/=size(b,2))stop 'In gram; incompatible dimensions'
a=as
b=identity(n)
det=1_sp
val=maxval(abs(a))
if(val==0_sp)then
   nrank=0
   return
endif
vcrit=val*crit
nrank=min(n,m)
do k=1,n
   if(k>nrank)exit
   ab(k:m,k:n)=matmul( transpose(a(:,k:m)),b(:,k:n) )
   ii =maxloc( abs( ab(k:m,k:n)) )+k-1
   val=maxval( abs( ab(k:m,k:n)) )
   if(val<=vcrit)then
      nrank=k-1
      exit
   endif
   i=ii(1)
   j=ii(2)
   tv=b(:,j)
   b(:,j)=-b(:,k)
   b(:,k)=tv
   tv=a(:,i)
   a(:,i)=-a(:,k)
   a(:,k)=tv
   w(k:n)=matmul( transpose(b(:,k:n)),tv )
   b(:,k)=matmul(b(:,k:n),w(k:n) )
   s=dot_product(b(:,k),b(:,k))
   s=sqrt(s)
   if(w(k)<0_sp)s=-s
   det=det*s
   b(:,k)=b(:,k)/s
   do l=k,n
      do j=l+1,n
         s=dot_product(b(:,l),b(:,j))
         b(:,j)=normalized( b(:,j)-b(:,l)*s )
      enddo
   enddo
enddo
end subroutine gram_s
   
!=============================================================================
subroutine gram_d(as,b,nrank,det)!                                      [gram]
!=============================================================================
implicit none
real(dp),dimension(:,:),intent(IN )      :: as
real(dp),dimension(:,:),intent(OUT)      :: b
integer(i_kind),        intent(OUT)      :: nrank
real(dp),               intent(OUT)      :: det
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
real(dp),parameter                       :: crit=1.e-9_dp
real(dp),dimension(size(as,1),size(as,2)):: a
real(dp),dimension(size(as,2),size(as,1)):: ab
real(dp),dimension(size(as,1))           :: tv,w
real(dp)                                 :: val,s,vcrit
integer(i_kind)                          :: i,j,k,l,m,n
integer(i_kind),dimension(2)             :: ii
!=============================================================================
n=size(as,1)
m=size(as,2)
if(n/=size(b,1) .or. n/=size(b,2))stop 'In gram; incompatible dimensions'
a=as
b=identity(n)
det=1_dp
val=maxval(abs(a))
if(val==0_dp)then
   nrank=0
   return
endif
vcrit=val*crit
nrank=min(n,m)
do k=1,n
   if(k>nrank)exit
   ab(k:m,k:n)=matmul( transpose(a(:,k:m)),b(:,k:n) )
   ii =maxloc( abs( ab(k:m,k:n)) )+k-1
   val=maxval( abs( ab(k:m,k:n)) )
   if(val<=vcrit)then
      nrank=k-1
      exit
   endif
   i=ii(1)
   j=ii(2)
   tv=b(:,j)
   b(:,j)=-b(:,k)
   b(:,k)=tv
   tv=a(:,i)
   a(:,i)=-a(:,k)
   a(:,k)=tv
   w(k:n)=matmul( transpose(b(:,k:n)),tv )
   b(:,k)=matmul(b(:,k:n),w(k:n) )
   s=dot_product(b(:,k),b(:,k))
   s=sqrt(s)
   if(w(k)<0_dp)s=-s
   det=det*s
   b(:,k)=b(:,k)/s
   do l=k,n
      do j=l+1,n
         s=dot_product(b(:,l),b(:,j))
         b(:,j)=normalized( b(:,j)-b(:,l)*s )
      enddo
   enddo
enddo
end subroutine gram_d
   
!=============================================================================
subroutine graml_d(as,b,nrank,detsign,ldet)!                            [gram]
!=============================================================================
! A version of gram_d where the determinant information is returned in
! logarithmic form (to avoid overflows for large matrices). When the
! matrix is singular, the "sign" of the determinant, detsign, is returned
! as zero (instead of either +1 or -1) and ldet is then just the log of
! the nonzero factors found by the process.
!=============================================================================
implicit none
real(dp),dimension(:,:),intent(IN )      :: as
real(dp),dimension(:,:),intent(OUT)      :: b
integer(i_kind),        intent(OUT)      :: nrank
integer(i_kind),        intent(out)      :: detsign
real(dp),               intent(OUT)      :: ldet
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
real(dp),parameter                       :: crit=1.e-9_dp
real(dp),dimension(size(as,1),size(as,2)):: a
real(dp),dimension(size(as,2),size(as,1)):: ab
real(dp),dimension(size(as,1))           :: tv,w
real(dp)                                 :: val,s,vcrit
integer(i_kind)                          :: i,j,k,l,m,n
integer(i_kind),dimension(2)             :: ii
!=============================================================================
detsign=1_dp
n=size(as,1)
m=size(as,2)
if(n/=size(b,1) .or. n/=size(b,2))stop 'In gram; incompatible dimensions'
a=as
b=identity(n)
!det=1
ldet=0_dp
val=maxval(abs(a))
if(val==0_dp)then
   nrank=0
   return
endif
vcrit=val*crit
nrank=min(n,m)
do k=1,n
   if(k>nrank)exit
   ab(k:m,k:n)=matmul( transpose(a(:,k:m)),b(:,k:n) )
   ii =maxloc( abs( ab(k:m,k:n)) )+k-1
   val=maxval( abs( ab(k:m,k:n)) )
   if(val<=vcrit)then
      nrank=k-1
      exit
   endif
   i=ii(1)
   j=ii(2)
   tv=b(:,j)
   b(:,j)=-b(:,k)
   b(:,k)=tv
   tv=a(:,i)
   a(:,i)=-a(:,k)
   a(:,k)=tv
   w(k:n)=matmul( transpose(b(:,k:n)),tv )
   b(:,k)=matmul(b(:,k:n),w(k:n) )
   s=dot_product(b(:,k),b(:,k))
   s=sqrt(s)
   if(w(k)<0_dp)s=-s
   if(s<0_dp)then
      ldet=ldet+log(-s)
      detsign=-detsign
   elseif(s>0_dp)then
      ldet=ldet+log(s)
   else
      detsign=0
   endif
      
!   det=det*s
   b(:,k)=b(:,k)/s
   do l=k,n
      do j=l+1,n
         s=dot_product(b(:,l),b(:,j))
         b(:,j)=normalized( b(:,j)-b(:,l)*s )
      enddo
   enddo
enddo
end subroutine graml_d
   
!=============================================================================
subroutine plaingram_s(b,nrank)!                                        [gram]
!=============================================================================
! A "plain" (unpivoted) version of Gram-Schmidt, for square matrices only.
implicit none
real(sp),dimension(:,:),intent(INOUT)    :: b
integer(i_kind),        intent(  OUT)    :: nrank
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
real(sp),parameter                       :: crit=1.e-5_sp
real(sp)                                 :: val,vcrit
integer(i_kind)                          :: j,k,n
!=============================================================================
n=size(b,1); if(n/=size(b,2))stop 'In gram; matrix needs to be square'
val=maxval(abs(b))
nrank=0
if(val==0_sp)then
   b=0_sp
   return
endif
vcrit=val*crit
do k=1,n
   val=sqrt(dot_product(b(:,k),b(:,k)))
   if(val<=vcrit)then
      b(:,k:n)=0_sp
      return
   endif
   b(:,k)=b(:,k)/val
   nrank=k
   do j=k+1,n
      b(:,j)=b(:,j)-b(:,k)*dot_product(b(:,k),b(:,j))
   enddo
enddo
end subroutine plaingram_s

!=============================================================================
subroutine plaingram_d(b,nrank)!                                        [gram]
!=============================================================================
! A "plain" (unpivoted) version of Gram-Schmidt, for square matrices only.
implicit none
real(dp),dimension(:,:),intent(INOUT)    :: b
integer(i_kind),        intent(  OUT)    :: nrank
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
real(dp),parameter                       :: crit=1.e-9_dp
real(dp)                                 :: val,vcrit
integer(i_kind)                          :: j,k,n
!=============================================================================
n=size(b,1); if(n/=size(b,2))stop 'In gram; matrix needs to be square'
val=maxval(abs(b))
nrank=0
if(val==0_dp)then
   b=0_dp
   return
endif
vcrit=val*crit
do k=1,n
   val=sqrt(dot_product(b(:,k),b(:,k)))
   if(val<=vcrit)then
      b(:,k:n)=0_dp
      return
   endif
   b(:,k)=b(:,k)/val
   nrank=k
   do j=k+1,n
      b(:,j)=b(:,j)-b(:,k)*dot_product(b(:,k),b(:,j))
   enddo
enddo
end subroutine plaingram_d

!=============================================================================
subroutine rowgram(m,n,a,ipiv,tt,b,rank)!                               [gram]
!=============================================================================
! Without changing (tall) rectangular input matrix a, perform pivoted gram-
! schmidt operations to orthogonalize the rows, until rows that remain become
! negligible. Record the pivoting sequence in ipiv, and the row-normalization
! in tt(j,j) and the row-orthogonalization in tt(i,j), for i>j. Note that
! tt(i,j)=0 for i<j (tt is truncated lower triangular). The orthonormalized
! rows are returned in square array b, which is complete even when the
! effective rank < n. 
! The recorded row operations can be repeated on independent column vectors
! through the use of subroutine ROWOPS (in this module).
! It is recommended to rescale the original matrix A via a call to CORRAL
! (in this module) because the negligibility criterion depends upon an
! "epsilon" value that is fixed (10**(-13)) and assumes elements of a are
! never too different in magnitude from unity, unless they are actually zero.
!=============================================================================
implicit none
integer(i_kind),                intent(IN ):: m,n
real(dp),        dimension(m,n),intent(in ):: a
integer(i_kind), dimension(n),  intent(out):: ipiv
real(dp),        dimension(m,n),intent(out):: tt
real(dp),        dimension(n,n),intent(out):: b
integer(i_kind),                intent(out):: rank
!-----------------------------------------------------------------------------
real(dp),parameter             :: eps=1.e-13_dp,epss=eps**2
real(dp),       dimension(m,n) :: aa
real(dp),       dimension(n)   :: rowv
real(dp),       dimension(m)   :: p
real(dp)                       :: maxp,nepss
integer(i_kind),dimension(1)   :: jloc
integer(i_kind)                :: i,ii,iii,j,maxi
!=============================================================================
if(m<n)stop 'In rowgram; this routines needs m>=n please'
nepss=n*epss
rank=n
aa=a
tt=0_dp
do ii=1,n

! At this stage, all rows less than ii are already orthonormalized and are
! orthogonal to all rows at and beyond ii. Find the norms of these lower
! rows and pivot the largest of them into position ii:
   maxp=0_dp
   maxi=ii
   do i=ii,m
      p(i)=dot_product(aa(i,:),aa(i,:))
      if(p(i)>maxp)then
         maxp=p(i)
         maxi=i
      endif
   enddo
   if(maxp<nepss)then !<- End of gram process; clean up and return
      b=0_dp
      b(1:ii-1,:)=aa(1:ii-1,:)
! fill the remaining rows, ii:n, of b with remaining orthonormal rows at random
      do iii=ii,n
! find the column of b for which the maximum element is the smallest:
         do j=1,n
            rowv(j)=maxval(abs(b(1:iii-1,j)))
         enddo
         jloc=minloc(rowv)
         j=jloc(1)
         b(iii,j)=1
         do i=1,iii-1
            maxp=dot_product(b(i,:),b(iii,:))
            b(iii,:)=b(iii,:)-b(i,:)*maxp
         enddo
         maxp=sqrt(dot_product(b(iii,:),b(iii,:)))
         b(iii,:)=b(iii,:)/maxp
      enddo
      rank=ii-1
      return
   endif
   
   ipiv(ii)=maxi
   if(maxi/=ii)then
      rowv      =aa(ii,  :)
      aa(ii,  :)=aa(maxi,:)
      aa(maxi,:)=rowv
   endif
   maxp=sqrt(maxp)
   tt(ii,ii)=maxp
   aa(ii,:)=aa(ii,:)/maxp
! Adjust all rows below to make them orthogonal to new row ii  
   do i=ii+1,m
      maxp=dot_product(aa(ii,:),aa(i,:))
      tt(i,ii)=maxp
      aa(i,:)=aa(i,:)-aa(ii,:)*maxp
   enddo
enddo
b=aa(1:n,:)
end subroutine rowgram

!=============================================================================
subroutine rowops(m,n,ipiv,tt,v,vv)!                                  [rowops]
!=============================================================================
! Apply the row-operations, implied by ipiv and tt returned by rowgram, to
! the single column vector, v, to produce the transformed vector vv.
!=============================================================================
use kinds, only: dp,i_kind
implicit none
integer(i_kind),                intent(in ):: m,n
integer(i_kind), dimension(n),  intent(in ):: ipiv
real(dp),        dimension(m,n),intent(in ):: tt
real(dp),        dimension(m),  intent(in ):: v
real(dp),        dimension(m),  intent(out):: vv
!-----------------------------------------------------------------------------
integer(i_kind)                            :: i,j,k
real(dp)                                   :: p
!=============================================================================
vv=v
do j=1,n
   k=ipiv(j)
   if(k/=j)then
      p=vv(j)
      vv(j)=vv(k)
      vv(k)=p
   endif
   vv(j)=vv(j)/tt(j,j)
   do i=j+1,m
      vv(i)=vv(i)-vv(j)*tt(i,j)
   enddo
enddo
end subroutine rowops
   
!=============================================================================
subroutine corral(m,n,mask,a,d,aa,e)!                                 [corral]
!=============================================================================
! Find positive diagonals D and E and a Lagrange multiplier F that minimize
! the row-sum +column-sum of masked terms, 
! (D_i +log(|A_ij|) +E_j)^2
! subject to the single constraint, sum_j E_j =0, where the mask permits
! only nonnegligible A_ij to participate in the quadratic quantities. 
! Once a solution for D and E is found, return their exponentials, d and e,
! together with the rescaled matrix aa such that a = d.aa.e when d and e are
! interpreted as diagonal matrices.
!=============================================================================
use pmat, only: inv
implicit none

integer(i_kind),        intent(in ):: m,n
logical, dimension(m,n),intent(in ):: mask
real(dp),dimension(m,n),intent(in ):: a
real(dp),dimension(m  ),intent(out):: d
real(dp),dimension(m,n),intent(out):: aa
real(dp),dimension(  n),intent(out):: e
!-----------------------------------------------------------------------------
real(dp),dimension(0:m+n,0:m+n)    :: g
real(dp),dimension(0:m+n)          :: h
integer(i_kind)                    :: i,j,k,nh
!=============================================================================
nh=1+m+n
aa=0_dp
do j=1,n
   do i=1,m
      if(mask(i,j))aa(i,j)=log(abs(a(i,j)))
   enddo
enddo

h=0_dp
g=0_dp

! Equations on row 0 enforcing Lagrange multiplier F-constraint:
do j=1,n
   k=m+j
   g(0,k)=1_dp
enddo

! Equations on rows 1:m minimizing row sums of quadratic terms:
do i=1,m
   do j=1,n
      k=m+j
      if(mask(i,j))then
         g(i,i)=g(i,i)-1_dp
         g(i,k)=-1_dp
         h(i)=h(i)-aa(i,j)
      endif
   enddo
enddo

! Equations on rows m+1:m+n minimizing col sums subject to constraint
do j=1,n
   k=m+j
   g(k,0)=1_dp
   do i=1,m
      if(mask(i,j))then
         g(k,k)=g(k,k)-1_dp
         g(k,i)=-1_dp
         h(k)=h(k)-aa(i,j)
      endif
   enddo
enddo

! Invert the normal equations:
call inv(g,h)

! Exponentiate the parts that become final scaling diagnonal matrices d and e:
do i=1,m
   d(i)=exp(h(i))
enddo
do j=1,n
   k=m+j
   e(j)=exp(h(k))
enddo

! Compute the rescaled matrix directly:
do j=1,n
   do i=1,m
      aa(i,j)=a(i,j)/(d(i)*e(j))
   enddo
enddo
end subroutine corral

!=============================================================================
subroutine rottoax(orth33,ax3)!                                    [rottoax]
!=============================================================================
! Assuming that given orth33 is a 3*3 proper rotation matrix, derive an axial
! 3-vector, ax3, such that orth33 is implied by ax3 when the latter is
! interpreted as encoding a rotation (as in subroutine axtorot). Note that
! such ax3 are not unique -- adding any multiple of 2*pi times the parallel
! unit vector leads to the same orth33.
!=============================================================================
implicit none
real(dp),       dimension(3,3),intent(in ):: orth33
real(dp),       dimension(3),  intent(out):: ax3
!-----------------------------------------------------------------------------
real(dp),       dimension(3,3)            :: plane
real(dp),       dimension(3)              :: x,y,z
real(dp)               :: s
integer(i_kind),dimension(1)              :: ii
integer(i_kind)                           :: i,j,k
!=============================================================================
plane=orth33-identity()! Columns must be coplanar vectors
do i=1,3; z(i)=dot_product(plane(:,i),plane(:,i)); enddo
ii=minloc(z)
k=ii(1); i=1+mod(k,3); j=1+mod(i,3)
ax3=cross_product(plane(:,i),plane(:,j))
s=absv(ax3); if(s==0_dp)return
ax3=ax3/s ! <- temporarily a unit vector pointing along rotation axis
! Construct a unit 2D basis, x,y, in the plane of rotation
x=normalized(cross_product(ax3,plane(:,j)))
y=cross_product(ax3,x)
z=matmul(orth33,x)! Rotate x by the original rotation matrix
ax3=ax3*atan2(dot_product(y,z),dot_product(x,z))! multiply ax3 by the angle
end subroutine rottoax

!=============================================================================
subroutine axtorot(ax3,orth33)!                                    [axtorot]
!=============================================================================
! Construct the 3*3 orthogonal matrix, orth33, that corresponds to the 
! proper rotation encoded by the 3-vector, ax3. The antisymmetric matrix
! ax33 equivalent to the axial vector ax3 is exponentiated to obtain orth33.
!=============================================================================
implicit none
real(dp),dimension(3),  intent(in ):: ax3
real(dp),dimension(3,3),intent(out):: orth33
!-----------------------------------------------------------------------------
real(dp),dimension(3,3):: ax33
real(dp)               :: d
!=============================================================================
ax33=axial(ax3); call expmat(3,ax33,orth33,d)
end subroutine axtorot

!=============================================================================
subroutine spintoq(cspin,q)!                                         [spintoq]
!=============================================================================
! Go from the spinor to the quaternion representation
!=============================================================================
implicit none
complex(dpc),dimension(2,2),intent(IN ):: cspin
real(dp),    dimension(0:3),intent(OUT):: q
!-------------------------------------------
q(0)=real(cspin(1,1)); q(3)=aimag(cspin(1,1))
q(2)=real(cspin(2,1)); q(1)=aimag(cspin(2,1))
end subroutine spintoq

!==============================================================================
subroutine qtospin(q,cspin)!                                          [qtospin]
!==============================================================================
! Go from the quaternion to the spinor representation
!==============================================================================
implicit none
real(dp),    dimension(0:3),intent(IN ):: q
complex(dpc),dimension(2,2),intent(OUT):: cspin
!-------------------------------------------
cspin(1,1)=cmplx( q(0), q(3))
cspin(2,1)=cmplx( q(2), q(1))
cspin(1,2)=cmplx(-q(2), q(1))
cspin(2,2)=cmplx( q(0),-q(3))
end subroutine qtospin

!=============================================================================
subroutine rottoq(rot,q)!                                             [rottoq]
!=============================================================================
! Go from rotation matrix to quaternion representation
!=============================================================================
implicit none
real(dp),dimension(3,3),intent(IN ):: rot
real(dp),dimension(0:3),intent(OUT):: q
!------------------------------------------------------------------------------
real(dp),       dimension(3,3):: t1,t2
real(dp),       dimension(3)  :: u1,u2
real(dp)                      :: gamma,gammah,s,ss
integer(i_kind)               :: i,j
integer(i_kind),dimension(1)  :: ii
!==============================================================================
! construct the orthogonal matrix, t1, whose third row is the rotation axis
! of rot:
t1=rot; do i=1,3; t1(i,i)=t1(i,i)-1_dp; u1(i)=dot_product(t1(i,:),t1(i,:)); enddo
ii=maxloc(u1); j=ii(1); ss=u1(j)
if(ss<1.e-16_dp)then
   q=0_dp; q(0)=1_dp; return
endif
t1(j,:)=t1(j,:)/sqrt(ss)
if(j/=1)then
   u2     =t1(1,:)
   t1(1,:)=t1(j,:)
   t1(j,:)=u2
endif
do i=2,3
   t1(i,:)=t1(i,:)-dot_product(t1(1,:),t1(i,:))*t1(1,:)
   u1(i)=dot_product(t1(i,:),t1(i,:))
enddo
if(u1(3)>u1(2))then
   j=3
else
   j=2
endif
ss=u1(j)
if(ss==0_dp)stop 'In rotov; invalid rot'
if(j/=2)t1(2,:)=t1(3,:)
t1(2,:)=t1(2,:)/sqrt(ss)

! Form t1(3,:) as the cross product of t1(1,:) and t1(2,:)
t1(3,1)=t1(1,2)*t1(2,3)-t1(1,3)*t1(2,2)
t1(3,2)=t1(1,3)*t1(2,1)-t1(1,1)*t1(2,3)
t1(3,3)=t1(1,1)*t1(2,2)-t1(1,2)*t1(2,1)

! Project rot into the frame whose axes are the rows of t1:
t2=matmul(t1,matmul(rot,transpose(t1)))

! Obtain the rotation angle, gamma, implied by rot, and gammah=gamma/2:
gamma=atan2(t2(2,1),t2(1,1)); gammah=gamma/2_dp

! Hence deduce coefficients (in the form of a real 4-vector) of one of the two
! possible equivalent spinors:
s=sin(gammah)
q(0)=cos(gammah)
q(1:3)=t1(3,:)*s
end subroutine rottoq

!==============================================================================
subroutine qtorot(q,rot)!                                              [qtorot]
!==============================================================================
! Go from quaternion to rotation matrix representations
!==============================================================================
implicit none
real(dp),dimension(0:3),intent(IN ):: q
real(dp),dimension(3,3),intent(OUT):: rot
!=============================================================================
call setem(q(0),q(1),q(2),q(3),rot)
end subroutine qtorot

!=============================================================================
subroutine axtoq(v,q)!                                                 [axtoq]
!=============================================================================
! Go from an axial 3-vector to its equivalent quaternion
!=============================================================================
implicit none
real(dp),dimension(3),  intent(in ):: v
real(dp),dimension(0:3),intent(out):: q
!-----------------------------------------------------------------------------
real(dp),dimension(3,3):: rot
!=============================================================================
call axtorot(v,rot)
call rottoq(rot,q)
end subroutine axtoq

!=============================================================================
subroutine qtoax(q,v)!                                                [qtoax]
!=============================================================================
! Go from quaternion to axial 3-vector 
!=============================================================================
implicit none
real(dp),dimension(0:3),intent(in ):: q
real(dp),dimension(3),  intent(out):: v
!-----------------------------------------------------------------------------
real(dp),dimension(3,3):: rot
!=============================================================================
call qtorot(q,rot)
call rottoax(rot,v)
end subroutine qtoax

!=============================================================================
subroutine setem(c,d,e,g,r)!                                           [setem]
!=============================================================================
implicit none
real(dp),               intent(IN ):: c,d,e,g
real(dp),dimension(3,3),intent(OUT):: r
!-----------------------------------------------------------------------------
real(dp):: cc,dd,ee,gg,de,dg,eg,dc,ec,gc
!=============================================================================
cc=c*c; dd=d*d; ee=e*e; gg=g*g
de=d*e; dg=d*g; eg=e*g
dc=d*c; ec=e*c; gc=g*c
r(1,1)=cc+dd-ee-gg; r(2,2)=cc-dd+ee-gg; r(3,3)=cc-dd-ee+gg
r(2,3)=2_dp*(eg-dc);   r(3,1)=2_dp*(dg-ec);   r(1,2)=2_dp*(de-gc)
r(3,2)=2_dp*(eg+dc);   r(1,3)=2_dp*(dg+ec);   r(2,1)=2_dp*(de+gc)
end subroutine setem

!=============================================================================
function mulqq(a,b)result(c)!                                         [mulqq]
!=============================================================================
! Multiply quaternions, a*b, assuming operation performed from right to left
!=============================================================================
implicit none
real(dp),dimension(0:3),intent(IN ):: a,b
real(dp),dimension(0:3)            :: c
!-------------------------------------------
c(0)=a(0)*b(0) -a(1)*b(1) -a(2)*b(2) -a(3)*b(3)
c(1)=a(0)*b(1) +a(1)*b(0) +a(2)*b(3) -a(3)*b(2)
c(2)=a(0)*b(2) +a(2)*b(0) +a(3)*b(1) -a(1)*b(3)
c(3)=a(0)*b(3) +a(3)*b(0) +a(1)*b(2) -a(2)*b(1)
end function mulqq
!=============================================================================
subroutine expmat(n,a,b,detb)!                                        [expmat]
!=============================================================================
! Evaluate the exponential, b, of a matrix, a, of degree n.
! Apply the iterated squaring method, m times, to the approximation to
! exp(a/(2**m)) obtained as a Taylor expansion of degree L
! See Fung, T. C., 2004, Int. J. Numer. Meth. Engng, 59, 1273--1286.
!=============================================================================
use pietc, only: u1,u2,o2
implicit none
integer(i_kind),                intent(IN ):: n
real(dp),dimension(n,n),intent(IN ):: a
real(dp),dimension(n,n),intent(OUT):: b
real(dp),               intent(OUT):: detb
!-----------------------------------------------------------------------------
integer(i_kind),parameter      :: L=5
real(dp),dimension(n,n)        :: c,p
real(dp)                       :: t
integer(i_kind)                :: i,m
!=============================================================================
m=10+log(u1+maxval(abs(a)))/log(u2)
t=o2**m
c=a*t
p=c
b=p
do i=2,L
   p=matmul(p,c)/i
   b=b+p
enddo
do i=1,m
   b=b*2_dp+matmul(b,b)
enddo
do i=1,n
   b(i,i)=b(i,i)+1_dp
enddo
detb=0_dp; do i=1,n; detb=detb+a(i,i); enddo; detb=exp(detb)
end subroutine expmat

!=============================================================================
subroutine expmatd(n,a,b,bd,detb,detbd)!                              [expmat]
!=============================================================================
! Like expmat, but for the 1st derivatives also.
!=============================================================================
use pietc, only: u1,u2,o2
implicit none
integer(i_kind),                    intent(IN ):: n
real(dp),dimension(n,n),            intent(IN ):: a
real(dp),dimension(n,n),            intent(OUT):: b
real(dp),dimension(n,n,(n*(n+1))/2),intent(OUT):: bd
real(dp),                           intent(OUT):: detb
real(dp),dimension((n*(n+1))/2),    intent(OUT):: detbd
!-----------------------------------------------------------------------------
integer(i_kind),parameter          :: L=5
real(dp),dimension(n,n)            :: c,p
real(dp),dimension(n,n,(n*(n+1))/2):: pd,cd
real(dp)                           :: t
integer(i_kind)                    :: i,j,k,m,n1
!=============================================================================
n1=(n*(n+1))/2
m=10+log(u1+maxval(abs(a)))/log(u2)
t=o2**m
c=a*t
p=c
pd=0_dp
do k=1,n
   pd(k,k,k)=t
enddo
k=n
do i=1,n-1
   do j=i+1,n
      k=k+1
      pd(i,j,k)=t
      pd(j,i,k)=t
   enddo
enddo
if(k/=n1)stop 'In expmatd; n1 is inconsistent with n'
cd=pd
b=p
bd=pd

do i=2,L
   do k=1,n1
      pd(:,:,k)=(matmul(cd(:,:,k),p)+matmul(c,pd(:,:,k)))/i
   enddo
   p=matmul(c,p)/i
   b=b+p
   bd=bd+pd
enddo
do i=1,m
   do k=1,n1
      bd(:,:,k)=2_dp*bd(:,:,k)+matmul(bd(:,:,k),b)+matmul(b,bd(:,:,k))
   enddo
   b=b*2_dp+matmul(b,b)
enddo
do i=1,n
   b(i,i)=b(i,i)+1_dp
enddo
detb=0_dp; do i=1,n; detb=detb+a(i,i); enddo; detb=exp(detb)
detbd=0_dp; do k=1,n; detbd(k)=detb; enddo
end subroutine expmatd

!=============================================================================
subroutine expmatdd(n,a,b,bd,bdd,detb,detbd,detbdd)!                  [expmat]
!=============================================================================
! Like expmat, but for the 1st and 2nd derivatives also.
!=============================================================================
use pietc, only: u1,u2,o2
implicit none
integer(i_kind),                                intent(IN ):: n
real(dp),dimension(n,n),                        intent(IN ):: a
real(dp),dimension(n,n),                        intent(OUT):: b
real(dp),dimension(n,n,(n*(n+1))/2),            intent(OUT):: bd
real(dp),dimension(n,n,(n*(n+1))/2,(n*(n+1))/2),intent(OUT):: bdd
real(dp),                                       intent(OUT):: detb
real(dp),dimension((n*(n+1))/2),                intent(OUT):: detbd
real(dp),dimension((n*(n+1))/2,(n*(n+1))/2),    intent(OUT):: detbdd
!-----------------------------------------------------------------------------
integer(i_kind),parameter                      :: L=5
real(dp),dimension(n,n)                        :: c,p
real(dp),dimension(n,n,(n*(n+1))/2)            :: pd,cd
real(dp),dimension(n,n,(n*(n+1))/2,(n*(n+1))/2):: pdd,cdd
real(dp)                                       :: t
integer(i_kind)                                :: i,j,k,ki,kj,m,n1
!=============================================================================
n1=(n*(n+1))/2
m=10+log(u1+maxval(abs(a)))/log(u2)
t=o2**m
c=a*t
p=c
pd=0_dp
pdd=0_dp
do k=1,n
   pd(k,k,k)=t
enddo
k=n
do i=1,n-1
   do j=i+1,n
      k=k+1
      pd(i,j,k)=t
      pd(j,i,k)=t
   enddo
enddo
if(k/=n1)stop 'In expmatd; n1 is inconsistent with n'
cd=pd
cdd=0_dp
b=p
bd=pd
bdd=0_dp

do i=2,L
   do ki=1,n1
      do kj=1,n1
         pdd(:,:,ki,kj)=(matmul(cd(:,:,ki),pd(:,:,kj)) &
                       + matmul(cd(:,:,kj),pd(:,:,ki)) &
                       + matmul(c,pdd(:,:,ki,kj)))/i
      enddo
   enddo
   do k=1,n1
      pd(:,:,k)=(matmul(cd(:,:,k),p)+matmul(c,pd(:,:,k)))/i
   enddo
   p=matmul(c,p)/i
   b=b+p
   bd=bd+pd
   bdd=bdd+pdd
enddo
do i=1,m
   do ki=1,n1
      do kj=1,n1
         bdd(:,:,ki,kj)=2_dp*bdd(:,:,ki,kj)               &
                        +matmul(bdd(:,:,ki,kj),b)      &
                        +matmul(bd(:,:,ki),bd(:,:,kj)) &
                        +matmul(bd(:,:,kj),bd(:,:,ki)) &
                        +matmul(b,bdd(:,:,ki,kj))
      enddo
   enddo
   do k=1,n1
      bd(:,:,k)=2_dp*bd(:,:,k)+matmul(bd(:,:,k),b)+matmul(b,bd(:,:,k))
   enddo
   b=b*2_dp+matmul(b,b)
enddo
do i=1,n
   b(i,i)=b(i,i)+1_dp
enddo
detb=0_dp;   do i=1,n; detb=detb+a(i,i); enddo; detb=exp(detb)
detbd=0_dp;  do k=1,n; detbd(k)=detb; enddo
detbdd=0_dp; do ki=1,n; do kj=1,n; detbdd(ki,kj)=detb; enddo; enddo
end subroutine expmatdd

!=============================================================================
subroutine zntay(n,z,zn)!                                              [zntay]
!=============================================================================
implicit none
integer(i_kind), intent(IN ):: n
real(dp),intent(IN ):: z
real(dp),intent(OUT):: zn
!-----------------------------------------------------------------------------
integer(i_kind),parameter   :: ni=100
real(dp),parameter          :: eps0=1.e-16_dp
integer(i_kind)             :: i,i2,n2
real(dp)                    :: t,eps,z2
!=============================================================================
z2=z*2
n2=n*2
t=1_dp
do i=1,n
   t=t/(i*2_dp-1_dp)
enddo
eps=t*eps0
zn=t
t=t
do i=1,ni
   i2=i*2
   t=t*z2/(i2*(i2+n2-1_dp))
   zn=zn+t
   if(abs(t)<eps)return
enddo
print'("In zntay;  full complement of iterations used")'
end subroutine zntay

!=============================================================================
subroutine znfun(n,z,zn,znd,zndd,znddd)!                               [znfun]
!=============================================================================
implicit none
integer(i_kind), intent(IN ):: n
real(dp),        intent(IN ):: z
real(dp),        intent(OUT):: zn,znd,zndd,znddd
!-----------------------------------------------------------------------------
real(dp)         :: z2,rz2
integer(i_kind)  :: i,i2p3
!=============================================================================
z2=abs(z*2_dp)
rz2=sqrt(z2)
if(z2<2_dp)then
   call zntay(n  ,z,zn)
   call zntay(n+1,z,znd)
   call zntay(n+2,z,zndd)
   call zntay(n+3,z,znddd)
else
   if(z>0_dp)then
      zn=cosh(rz2)
      znd=sinh(rz2)/rz2
      zndd=(zn-znd)/z2
      znddd=(znd-3_dp*zndd)/z2
      do i=1,n
         i2p3=i*2+3
         zn=znd
         znd=zndd
         zndd=znddd
         znddd=(znd-i2p3*zndd)/z2
      enddo
   else
      zn=cos(rz2)
      znd=sin(rz2)/rz2
      zndd=-(zn-znd)/z2
      znddd=-(znd-3_dp*zndd)/z2
      do i=1,n
         i2p3=i*2+3
         zn=znd
         znd=zndd
         zndd=znddd
         znddd=-(znd-i2p3*zndd)/z2
      enddo
   endif
endif
end subroutine znfun
      
!=============================================================================
! Utility code for various Mobius transformations. If aa1,bb1,cc1,dd1 are
! the coefficients for one transformation, and aa2,bb2,cc2,dd2 are the 
! coefficients for a second one, then the coefficients for the mapping
! of a test point, zz, by aa1 etc to zw, followed by a mapping of zw, by
! aa2 etc to zv, is equivalent to a single mapping zz-->zv by the transformatn
! with coefficients aa3,bb3,cc3,dd3, such that, as 2*2 complex matrices:
!
! [ aa3, bb3 ]   [ aa2, bb2 ]   [ aa1, bb1 ]
! [          ] = [          ] * [          ]
! [ cc3, dd3 ]   [ cc2, dd2 ]   [ cc1, dd1 ] .
!
!  Note that the determinant of these matrices is always +1
!
!=============================================================================
subroutine ctoz(v, z,infz)!                                             [ctoz]
!=============================================================================
implicit none
real(dp),dimension(3),intent(IN ):: v
complex(dpc),         intent(OUT):: z
logical,              intent(OUT):: infz
!-----------------------------------------------------------------------------
real(dp),parameter:: zero=0_dp,one=1_dp
real(dp)          :: rr,zzpi
!=============================================================================
infz=.false.
z=cmplx(v(1),v(2),dpc)
if(v(3)>0_dp)then
   zzpi=one/(one+v(3))
else
   rr=v(1)**2+v(2)**2
   infz=(rr==zero); if(infz)return ! <- The point is mapped to infinity (90S)
   zzpi=(one-v(3))/rr
endif
z=z*zzpi
end subroutine ctoz
   
!=============================================================================
subroutine ztoc(z,infz, v)!                                             [ztoc]
!=============================================================================
implicit none
complex(dpc),         intent(IN ):: z
logical,              intent(IN ):: infz
real(dp),dimension(3),intent(OUT):: v
!-----------------------------------------------------------------------------
real(dp),parameter:: zero=0_dp,one=1_dp
real(dp)          :: r,q,rs,rsc,rsbi
!=============================================================================
if(infz)then; v=(/zero,zero,-one/); return; endif
r=real(z); q=aimag(z); rs=r*r+q*q
rsc=one-rs
rsbi=one/(one+rs)
v(1)=2_dp*rsbi*r
v(2)=2_dp*rsbi*q
v(3)=rsc*rsbi
end subroutine ztoc

!=============================================================================
subroutine ztocd(z,infz, v,vd)!                                         [ztoc]
!=============================================================================
! The convention adopted for the complex derivative is that, for a complex
! infinitesimal map displacement, delta_z, the corresponding infinitesimal
! change of cartesian vector position is delta_v given by:
! delta_v = Real(vd*delta_z).
! Thus, by a kind of Cauchy-Riemann relation, Imag(vd)=v CROSS Real(vd).
! THE DERIVATIVE FOR THE IDEAL POINT AT INFINITY HAS NOT BEEN CODED YET!!!
!=============================================================================
implicit none
complex(dpc),             intent(IN ):: z
logical,                  intent(IN ):: infz
real(dp),dimension(3),    intent(OUT):: v
complex(dpc),dimension(3),intent(OUT):: vd
!-----------------------------------------------------------------------------
real(dp),parameter   :: zero=0,one=1
real(dp)             :: r,q,rs,rsc,rsbi,rsbis
real(dp),dimension(3):: u1,u2
integer(i_kind)      :: i
!=============================================================================
if(infz)then; v=(/zero,zero,-one/); return; endif
r=real(z); q=aimag(z); rs=r*r+q*q
rsc=one-rs
rsbi=one/(one+rs)
rsbis=rsbi**2
v(1)=2_dp*rsbi*r
v(2)=2_dp*rsbi*q
v(3)=rsc*rsbi
u1(1)=2_dp*(one+q*q-r*r)*rsbis
u1(2)=-4_dp*r*q*rsbis
u1(3)=-4_dp*r*rsbis
u2=cross_product(v,u1)
do i=1,3
   vd(i)=cmplx(u1(i),-u2(i),dpc)
enddo
end subroutine ztocd

!============================================================================
subroutine setmobius(xc0,xc1,xc2, aa,bb,cc,dd)!                   [setmobius]
!============================================================================
! Find the Mobius transformation complex coefficients, aa,bb,cc,dd,
! with aa*dd-bb*cc=1, for a standard (north-)polar stereographic transformation
! that takes cartesian point, xc0 to the north pole, xc1 to (lat=0,lon=0),
! xc2 to the south pole (=complex infinity).
!============================================================================
implicit none
real(dp),dimension(3),intent(IN ):: xc0,xc1,xc2
complex(dpc),         intent(OUT):: aa,bb,cc,dd
!----------------------------------------------------------------------------
real(dp),parameter:: zero=0_dp,one=1_dp
logical           :: infz0,infz1,infz2
complex(dpc)      :: z0,z1,z2,z02,z10,z21
!============================================================================
call ctoz(xc0,z0,infz0)
call ctoz(xc1,z1,infz1)
call ctoz(xc2,z2,infz2)
z21=z2-z1
z02=z0-z2
z10=z1-z0

if(  (z0==z1.and.infz0.eqv.infz1).or.&
     (z1==z2.and.infz1.eqv.infz2).or.&
     (z2==z0.and.infz2.eqv.infz0))   &
     stop 'In setmobius; anchor points must be distinct'

if(infz2 .or. (.not.infz0 .and. abs(z0)<abs(z2)))then
! z0 is finite and smaller than z2:
   if(infz1)then
      aa=one/sqrt(z02)        ! <- z1 is infinite
   elseif(infz2)then
      aa=one/sqrt(z10)        ! <- z2 is infinite
   else
      aa=sqrt(-z21/(z02*z10)) ! <- all zs are finite
   endif
   bb=-z0*aa
   if(infz1)then
      cc=aa                   ! <- z1 is infinite
      dd=-z2*aa               !
   elseif(infz2)then
      cc=zero                 ! <- z2 is infinite
      dd=z10*aa               !
   else
      cc=-(z10/z21)*aa        ! <- all zs are finite
      dd= z2*(z10/z21)*aa     !
   endif
else
! z2 is finite and smaller than z0:
   if(infz0)then
      cc=one/sqrt(z21)        ! <- z0 is inifinite
   elseif(infz1)then
      cc=one/sqrt(z02)        ! <- z1 is infinite
   else
      cc=sqrt(-z10/(z02*z21)) ! <- all zs are finite
   endif
   dd=-z2*cc
   if(infz0)then
      aa=zero                 ! <- z0 is inifinite
      bb=-z21*cc              !
   elseif(infz1)then
      aa=cc                   ! <- z1 is infinite
      bb=-z0*cc               !
   else
      aa=(-z21/z10)*cc        ! <- all zs are finite
      bb=z0*(z21/z10)*cc      !
   endif
endif
end subroutine setmobius

!============================================================================
subroutine zsetmobius(z0,infz0, z1,infz1, z2,infz2,  aa,bb,cc,dd)
!                                                                 [setmobius]
!============================================================================
! Find the Mobius transformation complex coefficients, aa,bb,cc,dd,
! with aa*dd-bb*cc=1, 
! that takes polar stereographic  point, z0 to the north pole, 
! z1 to (lat=0,lon=0), z2 to the south pole (=complex infinity).
! Should any one of z0,z1,z2 be itself the "point at infinity" its 
! corresponding infz will be set "true" (and the z value itself not used).
! This routine is like setmobius, except the three fixed points defining
! the mapping are given in standard complex stereographic form, together
! with the logical codes "infzn" that are TRUE if that point is itself
! the projection pole (i.e., the South Pole for a north polar stereographic).
!============================================================================
implicit none
complex(dp),          intent(IN ):: z0,z1,z2
logical,              intent(IN ):: infz0,infz1,infz2
complex(dpc),         intent(OUT):: aa,bb,cc,dd
!----------------------------------------------------------------------------
real(dp),parameter:: zero=0_dp,one=1_dp
complex(dpc)      :: z02,z10,z21
!============================================================================
z21=z2-z1
z02=z0-z2
z10=z1-z0

if(  (z0==z1.and.infz0.eqv.infz1).or.&
     (z1==z2.and.infz1.eqv.infz2).or.&
     (z2==z0.and.infz2.eqv.infz0))   &
     stop 'In setmobius; anchor points must be distinct'

if(infz2 .or. (.not.infz0 .and. abs(z0)<abs(z2)))then
! z0 is finite and smaller than z2:
   if(infz1)then
      aa=one/sqrt(z02)        ! <- z1 is infinite
   elseif(infz2)then
      aa=one/sqrt(z10)        ! <- z2 is infinite
   else
      aa=sqrt(-z21/(z02*z10)) ! <- all zs are finite
   endif
   bb=-z0*aa
   if(infz1)then
      cc=aa                   ! <- z1 is infinite
      dd=-z2*aa               !
   elseif(infz2)then
      cc=zero                 ! <- z2 is infinite
      dd=z10*aa               !
   else
      cc=-(z10/z21)*aa        ! <- all zs are finite
      dd= z2*(z10/z21)*aa     !
   endif
else
! z2 is finite and smaller than z0:
   if(infz0)then
      cc=one/sqrt(z21)        ! <- z0 is inifinite
   elseif(infz1)then
      cc=one/sqrt(z02)        ! <- z1 is infinite
   else
      cc=sqrt(-z10/(z02*z21)) ! <- all zs are finite
   endif
   dd=-z2*cc
   if(infz0)then
      aa=zero                 ! <- z0 is inifinite
      bb=-z21*cc              !
   elseif(infz1)then
      aa=cc                   ! <- z1 is infinite
      bb=-z0*cc               !
   else
      aa=(-z21/z10)*cc        ! <- all zs are finite
      bb=z0*(z21/z10)*cc      !
   endif
endif
end subroutine zsetmobius

!=============================================================================
subroutine zmobius(aa,bb,cc,dd, z,infz, w,infw)!                      [mobius]
!=============================================================================
! Perform a complex Mobius transformation from (z,infz) to (w,infw)
! where the transformation coefficients are the standard aa,bb,cc,dd.
! Infz is .TRUE. only when z is at complex infinity; likewise infw and w.
! For these infinite cases, it is important that numerical z==(0,0).
!=============================================================================
implicit none
complex(dpc),intent(IN ):: aa,bb,cc,dd,z
logical,     intent(IN ):: infz
complex(dpc),intent(OUT):: w
logical,     intent(OUT):: infw
!-----------------------------------------------------------------------------
real(dp),parameter:: zero=0_dp
complex(dpc)      :: top,bot
!=============================================================================
w=0_dpc
infw=.false.
if(infz)then
   top=aa
   bot=cc
else
   top=aa*z+bb
   bot=cc*z+dd
endif

if(abs(bot)==zero)then
   infw=.true.
else
   w=top/bot
endif
end subroutine zmobius

!=============================================================================
subroutine cmobius(aa,bb,cc,dd, vz,vw)!                               [mobius]
!=============================================================================
! Perform a complex Mobius transformation from cartesian vz to cartesian vw
! where the transformation coefficients are the standard aa,bb,cc,dd.
!=============================================================================
implicit none
complex(dpc),         intent(IN ):: aa,bb,cc,dd
real(dp),dimension(3),intent(IN ):: vz
real(dp),dimension(3),intent(OUT):: vw
!-----------------------------------------------------------------------------
complex(dpc):: z,w
logical     :: infz,infw
!=============================================================================
call ctoz(vz, z,infz)
call zmobius(aa,bb,cc,dd, z,infz, w,infw)
call ztoc(w,infw, vw)
end subroutine cmobius

!============================================================================
subroutine zmobiusi(aa,bb,cc,dd, zz,infz, zw,infw)                ! [mobiusi]
!============================================================================
! Perform the inverse of the mobius transformation with coefficients,
! {aa,bb,cc,dd}.
!============================================================================
complex(dpc),intent(IN ):: aa,bb,cc,dd,zz
logical,     intent(IN ):: infz
complex(dpc),intent(OUT):: zw
logical,     intent(OUT):: infw
!----------------------------------------------------------------------------
real(dp),parameter:: one=1_dp
complex(dpc)      :: aai,bbi,cci,ddi,d
!============================================================================
d=one/(aa*dd-bb*cc)
aai=dd*d
ddi=aa*d
bbi=-bb*d
cci=-cc*d
call zmobius(aai,bbi,cci,ddi, zz,infz, zw,infw)
end subroutine zmobiusi


end module peuc
