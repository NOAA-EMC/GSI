program prepbufr_encode_surface
!
!  write a surface observation into prepbufr file
!
 implicit none

 integer, parameter :: mxmn=35, mxlv=1
 character(80):: hdstr='SID XOB YOB DHR TYP ELV SAID T29'
 character(80):: obstr='POB QOB TOB ZOB UOB VOB PWO CAT PRSS'
 character(80):: qcstr='PQM QQM TQM ZQM WQM NUL PWQ     '
 character(80):: oestr='POE QOE TOE NUL WOE NUL PWE     '
 real(8) :: hdr(mxmn),obs(mxmn,mxlv),qcf(mxmn,mxlv),oer(mxmn,mxlv)

 character(8) :: subset
 integer      :: unit_out=10,unit_table=20,idate,iret

 character(8) :: c_sid
 real(8)      :: rstation_id
 equivalence(rstation_id,c_sid)
!
! write observation into prepbufr file
!
 open(unit_table,file='prepbufr.table',action='read')
 open(unit_out,file='prepbufr',action='write',form='unformatted')
 call datelen(10)
 call openbf(unit_out,'OUT',unit_table)

   idate=2010050700 ! cycle time: YYYYMMDDHH
   subset='ADPSFC'  ! surface land (SYNOPTIC, METAR) reports
   call openmb(unit_out,subset,idate)

! set headers
      hdr=10.0e10
      c_sid='KTKI'; hdr(1)=rstation_id
      hdr(2)=263.4; hdr(3)=33.2; hdr(4)=-0.1; hdr(6)=179.0

! set obs, qcf, oer for  wind
      hdr(5)=287          ! report type
      obs=10.0e10;qcf=10.0e10;oer=10.0e10
      obs(1,1)=985.2; obs(5,1)=-2.8; obs(6,1)=-7.7; obs(8,1)=6.0
      qcf(1,1)=2.0  ; qcf(5,1)=2.0
      oer(5,1)=1.6
! encode  wind obs
      call ufbint(unit_out,hdr,mxmn,1   ,iret,hdstr)
      call ufbint(unit_out,obs,mxmn,mxlv,iret,obstr)
      call ufbint(unit_out,oer,mxmn,mxlv,iret,oestr)
      call ufbint(unit_out,qcf,mxmn,mxlv,iret,qcstr)
      call writsb(unit_out)

! set obs, qcf, oer for  temperature and moisture
      hdr(5)=187          ! report type
      obs=10.0e10;qcf=10.0e10;oer=10.0e10
      obs(1,1)=985.2;obs(2,1)=12968.0;obs(3,1)=31.3;obs(4,1)=179.0;obs(8,1)=0.0
      qcf(1,1)=2.0  ;qcf(2,1)=2.0    ;qcf(3,1)=2.0 ;qcf(4,1)=2.0
      oer(1,1)=0.5  ;oer(2,1)=0.6    ;oer(3,1)=2.3 
! encode temperature and moisture
      call ufbint(unit_out,hdr,mxmn,1   ,iret,hdstr)
      call ufbint(unit_out,obs,mxmn,mxlv,iret,obstr)
      call ufbint(unit_out,oer,mxmn,mxlv,iret,oestr)
      call ufbint(unit_out,qcf,mxmn,mxlv,iret,qcstr)
      call writsb(unit_out)

   call closmg(unit_out)
 call closbf(unit_out)

end program
