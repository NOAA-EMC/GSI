module m_rerank
! From Jing Guo - use kinds for GSI consitency (Todling)
! 28Apr2011 - Todling - overload to handle single precision
! 01Aug2011 - Lueken  - changed F90 to f90 (no machine logic)
  use kinds, only : r_kind,r_single,i_kind
  implicit none
  private
  public :: rerank

  interface rerank
     module procedure rerank_2in1r4_
     module procedure rerank_3in1r4_
     module procedure rerank_1in2r4_
     module procedure rerank_1in3r4_

     module procedure rerank_2in1r8_
     module procedure rerank_3in1r8_
     module procedure rerank_1in2r8_
     module procedure rerank_1in3r8_
  end interface rerank

  character(len=*),parameter :: myname='m_rerank'
CONTAINS

  function rerank_2in1r4_(i2) result(i1)
    implicit none
    real(r_single),dimension(:,:),target,intent(in):: i2
    real(r_single),pointer,dimension(:):: i1

    interface
       function rerank_hack_2in1r4_(ln,i2) result(i1)
       use kinds, only: r_single,i_kind
       implicit none
       integer(i_kind),intent(in) :: ln
       real(r_single),dimension(:,:),target,intent(in):: i2
       real(r_single),pointer,dimension(:):: i1
       end function rerank_hack_2in1r4_
    end interface

    i1 => rerank_hack_2in1r4_(size(i2),i2)
  end function rerank_2in1r4_

  function rerank_2in1r8_(i2) result(i1)
    implicit none
    real(r_kind),dimension(:,:),target,intent(in):: i2
    real(r_kind),pointer,dimension(:):: i1

    interface
       function rerank_hack_2in1r8_(ln,i2) result(i1)
       use kinds, only: r_kind,i_kind
       implicit none
       integer(i_kind),intent(in) :: ln
       real(r_kind),dimension(:,:),target,intent(in):: i2
       real(r_kind),pointer,dimension(:):: i1
       end function rerank_hack_2in1r8_
    end interface

    i1 => rerank_hack_2in1r8_(size(i2),i2)
  end function rerank_2in1r8_

  function rerank_3in1r4_(i3) result(i1)
    implicit none
    real(r_single),dimension(:,:,:),target,intent(in):: i3
    real(r_single),pointer,dimension(:):: i1

    interface
       function rerank_hack_2in1r4_(ln,i3) result(i1)
       use kinds, only: r_single,i_kind
       implicit none
       integer(i_kind),intent(in) :: ln
       real(r_single),dimension(:,:,:),target,intent(in):: i3
       real(r_single),pointer,dimension(:):: i1
       end function rerank_hack_2in1r4_
    end interface

    i1 => rerank_hack_2in1r4_(size(i3),i3)
  end function rerank_3in1r4_

  function rerank_3in1r8_(i3) result(i1)
    implicit none
    real(r_kind),dimension(:,:,:),target,intent(in):: i3
    real(r_kind),pointer,dimension(:):: i1

    interface
       function rerank_hack_2in1r8_(ln,i3) result(i1)
       use kinds, only: r_kind,i_kind
       implicit none
       integer(i_kind),intent(in) :: ln
       real(r_kind),dimension(:,:,:),target,intent(in):: i3
       real(r_kind),pointer,dimension(:):: i1
       end function rerank_hack_2in1r8_
    end interface

    i1 => rerank_hack_2in1r8_(size(i3),i3)
  end function rerank_3in1r8_

  function rerank_1in2r4_(i1,mold,shape) result(i2)
    implicit none
    real(r_single),dimension(:),target,intent(in):: i1
    integer(i_kind),dimension(:,:),intent(in):: mold ! here to differentiate interface
    integer(i_kind),dimension(:),intent(in):: shape
    real(r_single),pointer,dimension(:,:):: i2

    interface
       function rerank_hack_1in2r4_(l1,l2,i1) result(i2)
       use kinds, only: r_single,i_kind
       implicit none
       integer(i_kind),intent(in) :: l1,l2
       real(r_single),dimension(l1,l2),target,intent(in):: i1
       real(r_single),pointer,dimension(:,:):: i2
       end function rerank_hack_1in2r4_
    end interface

    character(len=*),parameter:: myname_='rerank_1in2_'
    call assert_eq_(size(shape),2,myname_,'size(shape)==2')
    i2 => rerank_hack_1in2r4_(shape(1),shape(2),i1)
  end function rerank_1in2r4_

  function rerank_1in2r8_(i1,mold,shape) result(i2)
    implicit none
    real(r_kind),dimension(:),target,intent(in):: i1
    integer(i_kind),dimension(:,:),intent(in):: mold ! here to differentiate interface
    integer(i_kind),dimension(:),intent(in):: shape
    real(r_kind),pointer,dimension(:,:):: i2

    interface
       function rerank_hack_1in2r8_(l1,l2,i1) result(i2)
       use kinds, only: r_kind,i_kind
       implicit none
       integer(i_kind),intent(in) :: l1,l2
       real(r_kind),dimension(l1,l2),target,intent(in):: i1
       real(r_kind),pointer,dimension(:,:):: i2
       end function rerank_hack_1in2r8_
    end interface

    character(len=*),parameter:: myname_='rerank_1in2_'
    call assert_eq_(size(shape),2,myname_,'size(shape)==2')
    i2 => rerank_hack_1in2r8_(shape(1),shape(2),i1)
  end function rerank_1in2r8_

  function rerank_1in3r4_(i1,mold,shape) result(i3)
    implicit none
    real(r_single),dimension(:),target,intent(in):: i1
    integer(i_kind),dimension(:,:,:),intent(in):: mold ! here to differentiate interface
    integer(i_kind),dimension(:),intent(in):: shape
    real(r_single),pointer,dimension(:,:,:):: i3

    interface
       function rerank_hack_1in3r4_(l1,l2,l3,i1) result(i3)
       use kinds, only: r_single,i_kind
       implicit none
       integer(i_kind),intent(in) :: l1,l2,l3
       real(r_single),dimension(l1,l2,l3),target,intent(in):: i1
       real(r_single),pointer,dimension(:,:,:):: i3
       end function rerank_hack_1in3r4_
    end interface

    character(len=*),parameter:: myname_='rerank_1in3_'
    call assert_eq_(size(shape),3,myname_,'size(shape)==3')
    i3 => rerank_hack_1in3r4_(shape(1),shape(2),shape(3),i1)
  end function rerank_1in3r4_

  function rerank_1in3r8_(i1,mold,shape) result(i3)
    implicit none
    real(r_kind),dimension(:),target,intent(in):: i1
    integer(i_kind),dimension(:,:,:),intent(in):: mold ! here to differentiate interface
    integer(i_kind),dimension(:),intent(in):: shape
    real(r_kind),pointer,dimension(:,:,:):: i3

    interface
       function rerank_hack_1in3r8_(l1,l2,l3,i1) result(i3)
       use kinds, only: r_kind,i_kind
       implicit none
       integer(i_kind),intent(in) :: l1,l2,l3
       real(r_kind),dimension(l1,l2,l3),target,intent(in):: i1
       real(r_kind),pointer,dimension(:,:,:):: i3
       end function rerank_hack_1in3r8_
    end interface

    character(len=*),parameter:: myname_='rerank_1in3_'
    call assert_eq_(size(shape),3,myname_,'size(shape)==3')
    i3 => rerank_hack_1in3r8_(shape(1),shape(2),shape(3),i1)
  end function rerank_1in3r8_

  subroutine assert_eq_(lsize,lrank,who,what)
  implicit none
  integer(i_kind),intent(in)::lsize,lrank
  character(len=*),intent(in)::who,what
  if(lsize==lrank) return
  write(*,*)' lsize= ',lsize
  write(*,*)' lrank= ',lrank
  write(*,*)' whois= ',who
  write(*,*)' whats= ',what
  call exit(2)
  end subroutine assert_eq_

end module m_rerank

! These must live outside module to trick compiler
function rerank_hack_2in1r8_(ln,i2) result(i1)
  use kinds, only: r_kind,i_kind
  implicit none
  integer(i_kind),intent(in) :: ln
  real(r_kind),dimension(ln),target,intent(in):: i2
  real(r_kind),pointer,dimension(:):: i1
  i1 => i2
end function rerank_hack_2in1r8_

function rerank_hack_2in1r4_(ln,i2) result(i1)
  use kinds, only: r_single,i_kind
  implicit none
  integer(i_kind),intent(in) :: ln
  real(r_single),dimension(ln),target,intent(in):: i2
  real(r_single),pointer,dimension(:):: i1
  i1 => i2
end function rerank_hack_2in1r4_

function rerank_hack_1in2r8_(l1,l2,i1) result(i2)
  use kinds, only: r_kind,i_kind
  implicit none
  integer(i_kind),intent(in) :: l1,l2
  real(r_kind),dimension(l1,l2),target,intent(in):: i1
  real(r_kind),pointer,dimension(:,:):: i2
  i2 => i1
end function rerank_hack_1in2r8_

function rerank_hack_1in2r4_(l1,l2,i1) result(i2)
  use kinds, only: r_single,i_kind
  implicit none
  integer(i_kind),intent(in) :: l1,l2
  real(r_single),dimension(l1,l2),target,intent(in):: i1
  real(r_single),pointer,dimension(:,:):: i2
  i2 => i1
end function rerank_hack_1in2r4_

function rerank_hack_1in3r8_(l1,l2,l3,i1) result(i3)
  use kinds, only: r_kind,i_kind
  implicit none
  integer(i_kind),intent(in) :: l1,l2,l3
  real(r_kind),dimension(l1,l2,l3),target,intent(in):: i1
  real(r_kind),pointer,dimension(:,:,:):: i3
  i3 => i1
end function rerank_hack_1in3r8_

function rerank_hack_1in3r4_(l1,l2,l3,i1) result(i3)
  use kinds, only: r_single,i_kind
  implicit none
  integer(i_kind),intent(in) :: l1,l2,l3
  real(r_single),dimension(l1,l2,l3),target,intent(in):: i1
  real(r_single),pointer,dimension(:,:,:):: i3
  i3 => i1
end function rerank_hack_1in3r4_
