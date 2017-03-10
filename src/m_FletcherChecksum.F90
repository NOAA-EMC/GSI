module m_FletcherChecksum
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module m_FletcherChecksum
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 610.3
!     date:	 2015-08-06
!
! abstract: compose Fletcher''s checksum
!
! program history log:
!   2015-08-06  j guo   - added this document block
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! module interface:

  implicit none
  private	! except
  ! Fletcher''s Checksum = flchsum

  public :: FLCHSUM_KIND        ! KIND of a flchsum() value.
  public :: flchsum             ! convert a message to a single checksum value
  public :: flchsum_reduce      ! perform a flchsum(convert a message to a single checksum value
  public :: flchsum_dump        ! convert a message to a single 'z8.8' hex string
  public :: flchsum_mold        ! a mold for transfer(), as expected by modsum()
                                ! or modsum_dump(), e.g., given any message v,
                                !
                                ! > use m_FletcherChecksum, only: modsum
                                ! > use m_FletcherChecksum, only: modsum_dump
                                ! > use m_FletcherChecksum, only: modsum_mold
                                ! > [...]
                                ! > write(*,'(z8.8,2x,a)') &
                                ! >     modsum     (transfer(v,mold=modsum_mold)), &
                                ! >     modsum_dump(transfer(v,mold=modsum_mold))

  public :: flchsum_show

        interface flchsum; module procedure &
          modsum_i32r0, &
          modsum_i32r1, &
          modsum_i32r2, &
          modsum_i64r0, &
          modsum_i64r1, &
          modsum_i64r2, &
          modsum_r32r0, &
          modsum_r32r1, &
          modsum_r32r2, &
          modsum_r64r0, &
          modsum_r64r1, &
          modsum_r64r2, &
          modsum_r128r0, &
          modsum_r128r1, &
          modsum_r128r2, &
          modsum_; end interface
        interface flchsum_reduce; module procedure &
                reduce_rank2_, &
                reduce_rank1_, &
                reduce_rank0_; end interface

        interface flchsum_dump; module procedure dump_; end interface
        interface flchsum_show; module procedure show_; end interface

        interface checkpair; module procedure &
                checkpair_pick_, &
                checkpair_; end interface
  integer*1,parameter:: I08=0
  integer*2,parameter:: I16=0
  integer*4,parameter:: I32=0
  integer*8,parameter:: I64=0

  integer,parameter:: KI08=kind(I08)
  integer,parameter:: KI16=kind(I16)
  integer,parameter:: KI32=kind(I32)
  integer,parameter:: KI64=kind(I64)

  real*4 ,parameter:: R32 =0.
  real*8 ,parameter:: R64 =0.
  real*16,parameter:: R128=0.
  integer,parameter:: KR32 =kind(R32)
  integer,parameter:: KR64 =kind(R64)
  integer,parameter:: KR128=kind(R128)

  integer,parameter:: KIND_FSUM=KI32
  integer,parameter:: KIND_MOLD=KI16

  integer(kind=KIND_MOLD),dimension(1):: flchsum_mold  
  integer,parameter:: FLCHSUM_KIND = KIND_FSUM

  integer(kind=KI32),parameter:: U16_MAX=2_KI32**16        ! or (huge(I16)+1_KI32)*2
  integer(kind=KI32),parameter:: U16_MOD=U16_MAX-1         ! a recommended modulus by wiki
  integer(kind=KI32),parameter:: U16_NBITS=bit_size(I16)
  integer(kind=KI32),parameter:: HGH_BITS=z'ffff0000'
  integer(kind=KI32),parameter:: LOW_BITS=z'0000ffff'
  integer(kind=KI64),parameter:: U32_MAX=2_KI64**32
  integer(kind=KI64),parameter:: U32_MOD=U32_MAX-1

  ! classes of kind values
  ! (1) kind_FSUM, of flchsum() return integer, which is fixed to 32 bits;
  ! (2) kind_MOLD, of flchsum_mold(1) integer, which is fixed to 16 bits;
  ! (3) KI16, KI32, KI64, kinds of flchsum() argument v, for all different
  !     argument possibilities.  Also include, KIND_MOLD, KR32, and KR64
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname='m_FletcherChecksum'

#include "myassert.H"

contains
pure function unsign_(i) result(ii)
  implicit none
  integer(kind=KI32):: ii
  integer(kind=KI16),intent(in):: i
  ii=i
  if(ii<0) ii=ii+U16_MAX
end function unsign_
pure function unsign64_(i) result(ii)
  implicit none
  integer(kind=KI64):: ii
  integer(kind=KI32),intent(in):: i
  ii=i
  if(ii<0) ii=ii+U32_MAX
end function unsign64_

pure function ushift_(i)
  implicit none
  integer(kind=KI32):: ushift_
  integer(kind=KI32),intent(in):: i
  !ushift_=mod(i,U16_MOD)*U16_MAX
  ushift_=ishft(i,U16_NBITS)
end function ushift_

subroutine show_()
  implicit none
  integer(KI32):: i,ms1,ms2
  integer:: nshift=BIT_SIZE(I16)
  print'(z8.8,i12,2x,a)',U16_MOD,U16_MOD,'| U16_MOD'
  print'(z8.8,i12,2x,a)',ushift_(U16_MOD),ushift_(U16_MOD),'| ushift(U16_MOD)'

  print'(z8.8,i12,2x,a)',U16_MAX,U16_MAX,'| U16_MAX'
  print'(z8.8,i12,2x,a)',ushift_(U16_MAX),ushift_(U16_MAX),'| ushift(U16_MAX)'

  print'(z8.8,i12,2x,a)',(U16_MOD-1),(U16_MOD-1),'| U16_MOD-1'
  print'(z8.8,i12,2x,a)',ushift_(U16_MOD-1),ushift_(U16_MOD-1),'| ushift(U16_MOD-1)'

  print'(z8.8,i12,2x,a)',huge(I16),huge(I16),'| HUGE(I16)'

  print'(z8.8,i12,2x,a)',huge(I16)+1_KI32,huge(I16)+1_KI32,'| HUGE(I16)+1_KI32'
  print'(z8.8,i12,2x,a)',huge(I16)+1_KI16,huge(I16)+1_KI16,'| HUGE(I16)+1_KI16'
  print'(z8.8,i12,2x,a)',unsign_(huge(I16)+1_KI16),unsign_(huge(I16)+1_KI16),'| unsign(HUGE(I16)+1_KI16)'

  print'(z8.8,i12,2x,a)',huge(I16)+2_KI32,huge(I16)+2_KI32,'| HUGE(I16)+2_KI32'
  print'(z8.8,i12,2x,a)',huge(I16)+2_KI16,huge(I16)+2_KI16,'| HUGE(I16)+2_KI16'
  print'(z8.8,i12,2x,a)',unsign_(huge(I16)+2_KI16),unsign_(huge(I16)+2_KI16),'| unsign(HUGE(I16)+2_KI16)'
  !print'(z8.8,i12,2x,a)',huge(I16)+2_KI16,huge(I16)+2_KI16,'| HUGE(I16)+2_KI16'
  !print'(z8.8,i12,2x,a)',huge(I16)+2_KI16+U16_MAX,huge(I16)+2_KI16+U16_MAX,'| unsigned HUGE(I16)+2_KI16'

  i=unsign_(-1_KI16)
  i=U16_MOD

  print'(z8.8,i12,2x,a)', i,i
  print'(z8.8,i12,2x,a)', ishft(i, nshift),ishft(i, nshift)
  print'(z8.8,i12,2x,a)', ishft(i,-nshift),ishft(i,-nshift)
  print'(z8.8,i12,2x,a)', ishftc(i, nshift),ishftc(i, nshift)
  print'(z8.8,i12,2x,a)', ishftc(i,-nshift),ishftc(i,-nshift)

#ifdef NOT_INCLUDED
  print'(z8.8,i12,2x,a)', i*U16_MAX,i*U16_MAX
  i=i*U16_MAX
  print'(z8.8,i12,2x,a)', i,i

  print'(z8.8,i12,2x,a)', i/U16_MAX,i/U16_MAx
  i=i/U16_MAX
  print'(z8.8,i12,2x,a)', i,i

  print'(z8.8,i12,2x,a)', (unsign_(-1_KI16)*U16_MAX), (unsign_(-1_KI16)*U16_MAX)
  print'(z8.8,i12,2x,a)', (unsign_(-1_KI16)*U16_MAX)/U16_MAX, (unsign_(-1_KI16)*U16_MAX)/U16_MAX
#endif

  call bitsize_test()
  call addto_test()
  call orderflipping_test()
  call zerosums_test()
  call checkbytes_test()
  call interfaces_test()

contains
subroutine bitsize_test()
  ! check the compiler environment of the deployment
  implicit none
  print'(a)','bitsize_test(): check the compiler environment'
  print'(1x,l8  ,a)',bit_size(KIND_FSUM)==2*bit_size(KIND_MOLD) &
                                                ,'  | bit_size(KIND_FSUM)==2*bit_size(KIND_MOLD), where'
  print'(1x,i8  ,a)',bit_size(KIND_FSUM)        ,'  |   bit_size(KIND_FSUM)'
  print'(1x,i8  ,a)',bit_size(KIND_MOLD)        ,'  |   bit_size(KIND_MOLD)'
end subroutine bitsize_test

subroutine addto_test()
  ! flchsum([a,b]) == flchsum(b,addto=flchsum(a)) := flchsum(a) .add. flchsum(b)
  implicit none
  integer(KI32):: ms1,ms2,ms3

  ms1=flchsum((/1,2,3,4/),addto=0)
  ms2=flchsum((/1,2/))
  ms2=flchsum((/3,4/),addto=ms2)
  !ms3=flchadd(flchsum((/1,2/)),flchsum((/3,4/))

  print'(a)','addto_test(): flchsum([a,b]) == flchsum(b,addto=flchsum(a))'
  print'(1x,l8  ,a)',ms1==ms2                   ,'  | ms1==ms2, where'
  print'(1x,z8.8,a)',ms1                        ,'  |   ms1 = flchsum((/1,2,3,4/))'
  print'(1x,z8.8,a)',ms2                        ,'  |   ms2 = flchsum((/3,4/),addto=flchsum((/1,2/))'
end subroutine addto_test

subroutine orderflipping_test()
  ! flchsum([a,b]) /= flchsum([b,a])
  implicit none
  integer(KI32):: ms1,ms1_0,ms1_1
  integer(KI32):: ms2,ms2_0,ms2_1

  ms1=flchsum((/1,2,3,4/))
  ms2=flchsum((/1,3,2,4/))
  call split_(ms1,ms1_0,ms1_1)
  call split_(ms2,ms2_0,ms2_1)

  print'(a)','orderflipping_test(): flchsum([a,b]) /= flchsum([b,a])'
  print'(1x,l8  ,a)',ms1 /=ms2.and.ms1_0==ms2_0 ,'  | ms1/=ms2 .and. ms1_0==ms2_0, where'
  print'(1x,z8.8,a)',ms1                        ,'  |   ms1 = flchsum((/1,2,3,4/))'
  print'(1x,z8.8,a)',ms2                        ,'  |   ms2 = flchsum((/1,3,2,4/))'
  print'(1x,z8.4,a)',ms1_0                      ,'  |   ms1_0 := simple_bits(ms1)'
  print'(1x,z8.4,a)',ms2_0                      ,'  |   ms2_0 := simple_bits(ms2)'
  print'(1x,z8.4,a)',ms1_1                      ,'  |   ms1_1 := fletcher_bits(ms1)'
  print'(1x,z8.4,a)',ms2_1                      ,'  |   ms2_1 := fletcher_bits(ms2)'
end subroutine orderflipping_test

subroutine zerosums_test()
  ! all flchsum() cases known to produce zeroes
  implicit none
  integer(KI32):: i,mz1,mz2,mz3,mz4,mz5,mz6

        ! zero-sized arrays
  mz1=flchsum(transfer("",mold=flchsum_mold))
  mz2=flchsum((/(i,i=1,0)/))
  mz3=flchsum((/(i*.1,i=1,0)/))
        ! not zero-sized, null values
  mz4=flchsum(transfer((/achar(0),achar(0),achar(0),achar(0)/),mold=flchsum_mold))
  mz5=flchsum((/(0,i=1,4)/))
  mz6=flchsum((/(0.,i=1,4)/))

  print'(a)','zerosums_test(): flchsum() == 0'
  print'(1x,l8  ,a)', all((/mz1,mz2,mz3,mz4,mz5,mz6/)==0) &
                                                ,'  | all((/mz1,...,mz6/)==0), where'
  print'(1x,z8.8,a)',mz1                        ,'  |   mz1 = flchsum(transfer("",...))'
  print'(1x,z8.8,a)',mz2                        ,'  |   mz2 = flchsum((/(i,i=1,0)/))'
  print'(1x,z8.8,a)',mz3                        ,'  |   mz3 = flchsum((/(i*.1,i=1,0)/))'

  print'(1x,z8.8,a)',mz4                        ,'  |   mz4 = flchsum(transfer((/(4x)achar(0)/),...))'
  print'(1x,z8.8,a)',mz5                        ,'  |   mz5 = flchsum((/(4x)*0/))'
  print'(1x,z8.8,a)',mz6                        ,'  |   mz6 = flchsum((/(4x)*0./))'
end subroutine zerosums_test

subroutine checkbytes_test()
  implicit none
  integer(KI32):: ms,mz,f0,f1
  integer(kind=KIND_MOLD):: c0,c1
  ms=flchsum((/1,3,2,4/))
  mz=flchsum(checkpair(ms),addto=ms)

  !call split_(ms,f0,f1)
  !c0=int(U16_MOD - mod(f0+f1,U16_MOD),kind=kind(c0))
  !c1=int(U16_MOD - mod(f0+c0,U16_MOD),kind=kind(c1))
  !mz=flchsum(joint_(c1,c0),addto=ms)   ! endianness issue: adding c0, then c1
  !mz=flchsum((/c0,c1/),addto=ms)
  !mz=flchsum((/c0/),addto=ms)
  !mz=flchsum((/c1/),addto=mz)

  print'(a)','checkbytes_test(): paddings to make flchsum([any,pad]) == 0'
  print'(1x,l8  ,a)',mz==0                      ,'  | zero sum test of mz == 0, where'
  print'(1x,z8.8,a)',ms                         ,'  |   ms = flchsum(<something>)'
  print'(1x,z8.8,a)',mz                         ,'  |   mz = flchsum(checkpair(ms),addto=ms)'
  print'(1x,z8.4,a)',checkpair(ms,0)            ,'  |   checkpair(ms,0)'
  print'(1x,z8.4,a)',checkpair(ms,1)            ,'  |   checkpair(ms,1)'
  print'(1x,z8.4,a)',checkpair(mz,0)            ,'  |   checkpair(mz,0)'
  print'(1x,z8.4,a)',checkpair(mz,1)            ,'  |   checkpair(mz,1)'

  ms=flchsum((/0,0,0,0/))
  mz=flchsum(checkpair(ms),addto=ms)
  print'(1x,l8  ,a)',mz==0                      ,'  | zero sum test of mz == 0, where'
  print'(1x,z8.8,a)',ms                         ,'  |   ms = flchsum(<something>)'
  print'(1x,z8.8,a)',mz                         ,'  |   mz = flchsum(checkpair(ms),addto=ms)'
  print'(1x,z8.4,a)',checkpair(ms,0)            ,'  |   checkpair(ms,0)'
  print'(1x,z8.4,a)',checkpair(ms,1)            ,'  |   checkpair(ms,1)'
  print'(1x,z8.4,a)',checkpair(mz,0)            ,'  |   checkpair(mz,0)'
  print'(1x,z8.4,a)',checkpair(mz,1)            ,'  |   checkpair(mz,1)'

  !print'(1x,z8.4,a)',f0                         ,'  |   f0: split_(ms,f0,f1)'
  !print'(1x,z8.4,a)',f1                         ,'  |   f1: split_(ms,f0,f1)'
  !print'(1x,z8.4,a)',c0                         ,'  |   c0: checkbytes(ms,c0,c1)'
  !print'(1x,z8.4,a)',c1                         ,'  |   c1: checkbytes(ms,c0,c1)'

  ! ms=flchsum(shape(v),addto=0)        ! flchsum-in shape(v)
  ! ms=flchsum(v,addto=ms)              ! flchsum-in v
  ! ms=flchsum(checkpair_(ms),addto=ms) ! flchsum-in checkbytes(ms)

end subroutine checkbytes_test

subroutine interfaces_test()
  use kinds, only: r_single,r_double,r_quad
  implicit none
  real(r_single),dimension(4):: rx=(/0.,1.,2.,3./)
  real(r_double),dimension(4):: rd=(/0.,1.,2.,3./)
  real(r_quad  ),dimension(4):: rq=(/0.,1.,2.,3./)
  integer(KIND_FSUM):: mzx,mzd,mzq


  mzx=flchsum(rx)
  mzd=flchsum(rd)
  mzq=flchsum(rq)
  print'(a)','interfaces_test(): flchsum(r_single), flchsum(r_double), and flchsum(r_quad)'
  print'(1x,z8.8,a)',mzx                        ,'  |   mzx = flchsum(<r_single>)'
  print'(1x,z8.8,a)',mzd                        ,'  |   mzd = flchsum(<r_double>)'
  print'(1x,z8.8,a)',mzq                        ,'  |   mzq = flchsum(<r_quad>)'

end subroutine interfaces_test

end subroutine show_

pure function checkpair_pick_(ms,pick) result(ipick)
  implicit none
  integer(kind=KIND_MOLD):: ipick
  integer(kind=KIND_FSUM),intent(in):: ms
  integer(kind=KI32),intent(in):: pick

  integer(kind=KIND_MOLD),dimension(0:1):: ipair
  ipair = checkpair_(ms)
  ipick = -1_KIND_MOLD
  select case(pick)
  case(0); ipick = ipair(0)
  case(1); ipick = ipair(1)
  end select
end function checkpair_pick_

pure function checkpair_(ms) result(pair)
  implicit none
  integer(kind=KIND_MOLD),dimension(0:1):: pair
  integer(kind=KIND_FSUM),intent(in):: ms
  integer(kind=KIND_FSUM):: f0,f1
  call split_(ms,f0,f1)
  pair(0)=int(U16_MOD - mod(f0+f1     ,U16_MOD),kind=KIND_MOLD)
  pair(1)=int(U16_MOD - mod(f0+pair(0),U16_MOD),kind=KIND_MOLD)
end function checkpair_

pure function joint_(m0,m1) result(jo)
        ! Compose the lower halves of two unsigned integer into the
        ! higher half and the lower half of a single unsigned integers.
  implicit none
  integer(kind=KIND_FSUM):: jo
  integer(kind=KIND_FSUM),intent(in):: m0,m1

  jo=iand (m0,LOW_BITS)          ! get the lower half of m0
  jo=ishft(jo,U16_NBITS)         ! shift it up to the higher half
  jo=ior  (jo,iand(m1,LOW_BITS)) ! combine with the lower half of m1

  !! -- Or in its more compact form.
  !jo=ior(ishft(iand(m0,LOW_BITS),U16_NBITS),iand(m1,LOW_BITS))
end function joint_

pure subroutine split_(ji,m0,m1)
        ! Decompose the higher half and the lower half of an unsigned
        ! integer into the lower halfs of two unsigned integers.
  implicit none
  integer(kind=KIND_FSUM),intent(in ):: ji
  integer(kind=KIND_FSUM),intent(out):: m0,m1

  m0=iand (ji, HGH_BITS)        ! get the higher half of ji
  m0=ishft(m0,-U16_NBITS)       ! shift it down to the lower half
  m1=iand (ji, LOW_BITS)        ! get the lower half of ji

  !! -- Or in their more compact forms.
  !m0=ishft(iand(ji,HGH_BITS),-U16_NBITS)
  !m1=      iand(ji,LOW_BITS)
end subroutine split_

pure function modsum_i32r0(v,addto) result(ms)
  implicit none; integer(kind=KIND_FSUM):: ms
  integer(kind=KI32),intent(in):: v
  integer(kind=KIND_FSUM),optional,intent(in):: addto
  ms=modsum_(transfer(v,mold=flchsum_mold),addto=addto)
end function modsum_i32r0
pure function modsum_i32r1(v,addto) result(ms)
  implicit none; integer(kind=KIND_FSUM):: ms
  integer(kind=KI32),dimension(:),intent(in):: v
  integer(kind=KIND_FSUM),optional,intent(in):: addto
  ms=modsum_(transfer(v,mold=flchsum_mold),addto=addto)
end function modsum_i32r1
pure function modsum_i32r2(v,addto) result(ms)
  implicit none; integer(kind=KIND_FSUM):: ms
  integer(kind=KI32),dimension(:,:),intent(in):: v
  integer(kind=KIND_FSUM),optional,intent(in):: addto
  ms=modsum_(transfer(v,mold=flchsum_mold),addto=addto)
end function modsum_i32r2

pure function modsum_i64r0(v,addto) result(ms)
  implicit none; integer(kind=KIND_FSUM):: ms
  integer(kind=KI64),intent(in):: v
  integer(kind=KIND_FSUM),optional,intent(in):: addto
  ms=modsum_(transfer(v,mold=flchsum_mold),addto=addto)
end function modsum_i64r0
pure function modsum_i64r1(v,addto) result(ms)
  implicit none; integer(kind=KIND_FSUM):: ms
  integer(kind=KI64),dimension(:),intent(in):: v
  integer(kind=KIND_FSUM),optional,intent(in):: addto
  ms=modsum_(transfer(v,mold=flchsum_mold),addto=addto)
end function modsum_i64r1
pure function modsum_i64r2(v,addto) result(ms)
  implicit none; integer(kind=KIND_FSUM):: ms
  integer(kind=KI64),dimension(:,:),intent(in):: v
  integer(kind=KIND_FSUM),optional,intent(in):: addto
  ms=modsum_(transfer(v,mold=flchsum_mold),addto=addto)
end function modsum_i64r2

pure function modsum_r32r0(v,addto) result(ms)
  implicit none; integer(kind=KIND_FSUM):: ms
  real(kind=KR32),intent(in):: v
  integer(kind=KIND_FSUM),optional,intent(in):: addto
  ms=modsum_(transfer(v,mold=flchsum_mold),addto=addto)
end function modsum_r32r0
pure function modsum_r32r1(v,addto) result(ms)
  implicit none; integer(kind=KIND_FSUM):: ms
  real(kind=KR32),dimension(:),intent(in):: v
  integer(kind=KIND_FSUM),optional,intent(in):: addto
  ms=modsum_(transfer(v,mold=flchsum_mold),addto=addto)
end function modsum_r32r1
pure function modsum_r32r2(v,addto) result(ms)
  implicit none; integer(kind=KIND_FSUM):: ms
  real(kind=KR32),dimension(:,:),intent(in):: v
  integer(kind=KIND_FSUM),optional,intent(in):: addto
  ms=modsum_(transfer(v,mold=flchsum_mold),addto=addto)
end function modsum_r32r2

pure function modsum_r64r0(v,addto) result(ms)
  implicit none; integer(kind=KIND_FSUM):: ms
  real(kind=KR64),intent(in):: v
  integer(kind=KIND_FSUM),optional,intent(in):: addto
  ms=modsum_(transfer(v,mold=flchsum_mold),addto=addto)
end function modsum_r64r0
pure function modsum_r64r1(v,addto) result(ms)
  implicit none; integer(kind=KIND_FSUM):: ms
  real(kind=KR64),dimension(:),intent(in):: v
  integer(kind=KIND_FSUM),optional,intent(in):: addto
  ms=modsum_(transfer(v,mold=flchsum_mold),addto=addto)
end function modsum_r64r1
pure function modsum_r64r2(v,addto) result(ms)
  implicit none; integer(kind=KIND_FSUM):: ms
  real(kind=KR64),dimension(:,:),intent(in):: v
  integer(kind=KIND_FSUM),optional,intent(in):: addto
  ms=modsum_(transfer(v,mold=flchsum_mold),addto=addto)
end function modsum_r64r2

pure function modsum_r128r0(v,addto) result(ms)
  implicit none; integer(kind=KIND_FSUM):: ms
  real(kind=KR128),intent(in):: v
  integer(kind=KIND_FSUM),optional,intent(in):: addto
  ms=modsum_(transfer(v,mold=flchsum_mold),addto=addto)
end function modsum_r128r0
pure function modsum_r128r1(v,addto) result(ms)
  implicit none; integer(kind=KIND_FSUM):: ms
  real(kind=KR128),dimension(:),intent(in):: v
  integer(kind=KIND_FSUM),optional,intent(in):: addto
  ms=modsum_(transfer(v,mold=flchsum_mold),addto=addto)
end function modsum_r128r1
pure function modsum_r128r2(v,addto) result(ms)
  implicit none; integer(kind=KIND_FSUM):: ms
  real(kind=KR128),dimension(:,:),intent(in):: v
  integer(kind=KIND_FSUM),optional,intent(in):: addto
  ms=modsum_(transfer(v,mold=flchsum_mold),addto=addto)
end function modsum_r128r2

pure function modsum_(iv,addto) result(ms)

        ! compute Fletcher''s checksum of iv(:), which is to be used for
        ! data of all possible TKRs transparently, in the form of
        !
        !   use m_FletcherChecksum, only: flchsum
        !   use m_FletcherChecksum, only: flchsum_mold
        !   [...]
        !   ms=flchsum(transfer(w,mold=flchsum_mold))
        !   ms=flchsum(transfer(v,mold=flchsum_mold),addto=ms)

  implicit none
  integer(kind=KIND_FSUM):: ms
  integer(kind=KIND_MOLD),dimension(:),intent(in):: iv
  integer(kind=KIND_FSUM),optional,intent(in):: addto ! expected to be a result of a
                                                ! previous modsum() call.
  integer(kind=KIND_FSUM):: m0,m1,mv,i
  !integer(kind=KI64):: addto_

  m0=0
  m1=0
  if(present(addto)) call split_(addto,m0,m1)

  !if(present(addto)) then
  !  addto_=unsign8_(addto)
  !  m1=mod(addto_,U16_MAX)       ! chop leading bits
  !  m0=    addto_/U16_MAX        ! keep leading bits
  !  m0=mod(   m0 ,U16_MAX)       ! chop other leading bits, just in case
  !endif

  do i=1,size(iv)
    mv=unsign_(iv(i))
    !mv=iv(i)                    ! mv may have a signed value
    !if(mv<0) mv=mv+U16_MAX       ! map it back to an unsigned value.
    m0=mod(m0+mv,U16_MOD)        ! this is a simple checksum
    m1=mod(m1+m0,U16_MOD)        ! this is a Fletchers''s checksum
  enddo
  !ms=ior(m0*U16_MAX,m1)          ! shift m0 up, then wrap it and m1
  ms=joint_(m0,m1)

!  ms=ior(m0*U16_MAX,0_KIND_FSUM)      ! shifted m1 only
!  ms=m0                        ! m0 only
!  ms=m1                        ! m1 only
!  ms=size(iv)                  ! size only
end function modsum_

subroutine reduce_rank0_(msmesg,root,comm)
! Compute a checksum of checksums from all PEs.
!       call flchsum_reduce(ms,root,comm)
  use mpeu_mpif, only: MPI_IKIND
  use mpeu_mpif, only: mpi_type
  use mpeu_util, only: die
  implicit none

  integer(kind=KIND_FSUM),intent(inout):: msmesg
  integer(kind=MPI_IKIND),intent(in):: root
  integer(kind=MPI_IKIND),intent(in):: comm

  character(len=*),parameter:: myname_=myname//'::reduce_rank0_'
  integer(kind=KIND_FSUM),allocatable,dimension(:):: msgath
  integer(kind=MPI_IKIND):: ier
  integer(kind=MPI_IKIND):: myid,nPEs
  integer:: iPE

  call mpi_comm_size(comm,nPEs,ier)
        if(ier/=0) call die(myname_,'mpi_comm_size(), ierror =',ier)
  call mpi_comm_rank(comm,myid,ier)
        if(ier/=0) call die(myname_,'mpi_comm_rank(), ierror =',ier)

  if(myid/=root) nPEs=0
  allocate(msgath(0:nPEs-1))

  call mpi_gather(msmesg,1_MPI_IKIND,mpi_type(msmesg), &
                  msgath,1_MPI_IKIND,mpi_type(msgath), root,comm,ier)
        if(ier/=0) call die(myname_,'mpi_gather(), ierror =',ier)

  msmesg=0
  if(myid==root) msmesg=flchsum(msgath)

  deallocate(msgath)

end subroutine reduce_rank0_

subroutine reduce_rank1_(msmesg,root,comm)
! Compute a checksum of checksums from all PEs.
!       call flchsum_reduce(msmesg,root,comm)
  use mpeu_mpif, only: MPI_IKIND
  use mpeu_mpif, only: mpi_type
  use mpeu_util, only: die,assert_
  implicit none

  integer(kind=KIND_FSUM),dimension(:),intent(inout):: msmesg 
  integer(kind=MPI_IKIND),intent(in):: root
  integer(kind=MPI_IKIND),intent(in):: comm

  character(len=*),parameter:: myname_=myname//'::reduce_rank1_'
  integer(kind=KIND_FSUM),allocatable,dimension(:,:):: msgath
  integer(kind=MPI_IKIND):: ier,lrec
  integer(kind=MPI_IKIND):: myid,nPEs
  integer:: irec,iPE

  call mpi_comm_size(comm,nPEs,ier)
        if(ier/=0) call die(myname_,'mpi_comm_size(), ierror =',ier)
  call mpi_comm_rank(comm,myid,ier)
        if(ier/=0) call die(myname_,'mpi_comm_rank(), ierror =',ier)

  if(myid/=root) nPEs=0
  lrec=size(msmesg)
  allocate(msgath(1:lrec,0:nPEs-1))

  call mpi_gather(msmesg,lrec,mpi_type(msmesg), &
                  msgath,lrec,mpi_type(msgath), root,comm,ier)
        if(ier/=0) call die(myname_,'mpi_gather(), ierror =',ier)

  msmesg(:)=0
  if(myid==root) then
    do irec=1,lrec
      msmesg(irec)=flchsum(msgath(irec,0:nPEs-1))
    enddo
  endif

  deallocate(msgath)

end subroutine reduce_rank1_

subroutine reduce_rank2_(msmesg,root,comm)
! Compute a checksum of checksums from all PEs.
!       call flchsum_reduce(msmesg,root,comm)
!       if(ierror/=0) error()
  use mpeu_mpif, only: MPI_IKIND
  use mpeu_mpif, only: mpi_type
  use mpeu_util, only: die,assert_
  implicit none

  integer(kind=KIND_FSUM),dimension(:,:),intent(inout):: msmesg 
  integer(kind=MPI_IKIND),intent(in):: root
  integer(kind=MPI_IKIND),intent(in):: comm

  character(len=*),parameter:: myname_=myname//'::reduce_rank2_'
  integer(kind=KIND_FSUM),allocatable,dimension(:,:,:):: msgath
  integer(kind=MPI_IKIND):: ier,lrec1,lrec2
  integer(kind=MPI_IKIND):: myid,nPEs
  integer:: irec1,irec2,iPE

  call mpi_comm_size(comm,nPEs,ier)
        if(ier/=0) call die(myname_,'mpi_comm_size(), ierror =',ier)
  call mpi_comm_rank(comm,myid,ier)
        if(ier/=0) call die(myname_,'mpi_comm_rank(), ierror =',ier)

  if(myid/=root) nPEs=0
  lrec1=size(msmesg,1)
  lrec2=size(msmesg,2)
  allocate(msgath(1:lrec1,1:lrec2,0:nPEs-1))

  call mpi_gather(msmesg,lrec1*lrec2,mpi_type(msmesg), &
                  msgath,lrec1*lrec2,mpi_type(msgath), root,comm,ier)
        if(ier/=0) call die(myname_,'mpi_gather(), ierror =',ier)

  msmesg(:,:)=0
  if(myid==root) then
    do irec2=1,lrec2
    do irec1=1,lrec1
      msmesg(irec1,irec2)=flchsum(msgath(irec1,irec2,0:nPEs-1))
    enddo
    enddo
  endif

  deallocate(msgath)

end subroutine reduce_rank2_

pure function dump_(iv,addto) result(hex)
        !   use m_FletcherChecksum, only: flchsum_dump
        !   use m_FletcherChecksum, only: flchsum_mold
        !   [...]
        !   write(6,'(2i4,x,a)') size(v,1),size(v,2),modsum_dump(transfer(v,mold=flchsum_mold))
  implicit none
  character(len=8):: hex
  integer(kind=KIND_MOLD),dimension(:),intent(in):: iv
  integer(kind=KIND_FSUM),optional,intent(in):: addto ! expected to be a result of a
  write(hex,'(z8.8)') flchsum(iv,addto=addto)
end function dump_

end module m_FletcherChecksum
