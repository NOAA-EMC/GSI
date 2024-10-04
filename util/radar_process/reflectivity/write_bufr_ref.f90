  subroutine write_bufr_nsslref(maxlvl,nlon,nlat,numref,ref3d_column,idate)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    write_bufr_nsslref
!   prgmmr: hu           org: essl/gsd                date: 2008-12-01
!   
! abstract: write NSSL mosaic reflectivity in RR grid into bufr
!   
! program history log:
!   2008-12-01  kleist
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  linux 
!
!$$$
    use kinds, only: r_kind,i_kind
    implicit none

    real(r_kind), parameter :: zero = 0.0_r_kind
    real(r_kind), parameter :: one = 1.0_r_kind
    REAL(r_kind) :: ref3d_column(maxlvl+2,nlon*nlat)   ! 3D reflectivity in column
    real(r_kind) :: hdr(5),obs(1,35)
    character(80):: hdrstr='SID XOB YOB DHR TYP'
    character(80):: obsstr='HREF'

    REAL(i_kind),PARAMETER ::  MXBF = 160000_i_kind
    INTEGER(i_kind) :: ibfmsg = MXBF/4_i_kind

    character(8) subset,sid
    integer(i_kind) :: ludx,lendian_in,idate

    INTEGER(i_kind)  ::  maxlvl,nlon,nlat
    INTEGER(i_kind)  ::  numlvl,numref
    INTEGER(i_kind)  ::  i,n,k,iret


!mhu    idate=2008120100
    subset='ADPUPA'
    sid='NSSLREF'
    ludx=22
    lendian_in=10

!
!  covert BUFR value of missing (-64) and no echo (-63) from cloud analysis
!  value of missing (-999.0) and no echo (-99.0)
!
    DO i=1,numref
    DO k=1,maxlvl
      if(ref3d_column(k+2,i) < -63.0 .and. ref3d_column(k+2,i) > -100.0 ) then
        ref3d_column(k+2,i)=-63.0_r_kind
      elseif(ref3d_column(k+2,i) < -100.0) then
        ref3d_column(k+2,i)=-64.0_r_kind
      endif
    ENDDO
    ENDDO

    open(ludx,file='prepobs_prep.bufrtable',action='read')
    open(lendian_in,file='NSSLRefInGSI.bufr',action='write',form='unformatted')

    call datelen(10)
    call openbf(lendian_in,'OUT',ludx)
    do n=1,numref
      hdr(1)=transfer(sid,hdr(1))
      hdr(2)=ref3d_column(1,n)/10.0_r_kind
      hdr(3)=ref3d_column(2,n)/10.0_r_kind
      hdr(4)=0
      hdr(5)=500

      do k=1,maxlvl
        obs(1,k)=ref3d_column(2+k,n)
      enddo
      call openmb(lendian_in,subset,idate)
      call ufbint(lendian_in,hdr,5,   1,iret,hdrstr)
      call ufbint(lendian_in,obs,1,maxlvl,iret,obsstr)
      call writsb(lendian_in,ibfmsg,iret)
!      write(6,*) 'write_bufr_nsslref,1st: write BUFR message ibfmsg(1:',iret,') to local system'
    enddo
    call closbf(lendian_in)
    write(6,*) 'write_bufr_nsslref, DONE: write columns:',numref

end subroutine write_bufr_nsslref
