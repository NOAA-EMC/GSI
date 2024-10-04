program prepbufr_append_upperair
!
!  append a upper air observation into prepbufr file
!
 implicit none

 integer, parameter :: mxmn=35, mxlv=200
 character(80):: hdstr='SID XOB YOB DHR TYP ELV SAID T29'
 character(80):: obstr='POB QOB TOB ZOB UOB VOB PWO CAT PRSS'
 character(80):: qcstr='PQM QQM TQM ZQM WQM NUL PWQ     '
 character(80):: oestr='POE QOE TOE NUL WOE NUL PWE     '
 real(8) :: hdr(mxmn),obs(mxmn,mxlv),qcf(mxmn,mxlv),oer(mxmn,mxlv)

 character(8) :: subset
 integer      :: unit_out=10,unit_table=20,idate,iret,nlvl

 character(8) :: c_sid
 real(8)      :: rstation_id
 equivalence(rstation_id,c_sid)

! get bufr table from existing bufr file
 open(unit_table,file='prepbufr.table')
 open(unit_out,file='prepbufr',status='old',form='unformatted')
 call openbf(unit_out,'IN',unit_out)
 call dxdump(unit_out,unit_table)
 call closbf(unit_out)
!
! write observation into prepbufr file
!
 open(unit_out,file='prepbufr',status='old',form='unformatted')
 call datelen(10)
 call openbf(unit_out,'APN',unit_table)

   idate=2010050700 ! cycle time: YYYYMMDDHH
   subset='ADPUPA'  ! upper-air (raob, drops) reports
   call openmb(unit_out,subset,idate)

! set headers
      hdr=10.0e10
      c_sid='71823'; hdr(1)=rstation_id
      hdr(2)=286.3; hdr(3)=53.8; hdr(4)=0.0; hdr(6)=307.0

! set obs, qcf, oer for  wind
      hdr(5)=220          ! report type: sounding
      obs=10.0e10;qcf=10.0e10;oer=10.0e10
      obs(1,1)=500.0; obs(5,1)=-2.0;obs(6,1)=0.7 ;obs(8,1)=1.0
      qcf(1,1)=2.0  ; qcf(5,1)=2.0
                      oer(5,1)=2.5
      obs(1,2)=432.5; obs(4,2)=6401.;obs(5,2)=-7.1;obs(6,2)=-1.2;obs(8,2)=4.0
      qcf(1,2)=2.0  ; qcf(4,2)=2.0  ;qcf(5,2)= 2.0
                                     oer(5,2)=2.6
      nlvl=2
! encode  wind obs
      call ufbint(unit_out,hdr,mxmn,1   ,iret,hdstr)
      call ufbint(unit_out,obs,mxmn,nlvl,iret,obstr)
      call ufbint(unit_out,oer,mxmn,nlvl,iret,oestr)
      call ufbint(unit_out,qcf,mxmn,nlvl,iret,qcstr)
      call writsb(unit_out)

! set obs, qcf, oer for  temperature and moisture
      hdr(5)=120          ! report type: sounding
      obs=10.0e10;qcf=10.0e10;oer=10.0e10
      obs(1,1)=825.0;obs(2,1)=3672.0;obs(3,1)=0.8 ;               obs(8,1)=2.0
      qcf(1,1)=2.0  ;qcf(2,1)=2.0   ;qcf(3,1)=2.0 ;
                     oer(2,1)=1.2   ;oer(3,1)=1.3
      obs(1,2)=700.0;obs(2,2)=1157.0;obs(3,2)=-7.3;obs(4,2)=2841.;obs(8,2)=1.0
      qcf(1,2)=2.0  ;qcf(2,2)=2.0   ;qcf(3,2)=2.0 ;qcf(4,2)=2.0
                     oer(2,2)=1.4   ;oer(3,2)=1.0
      obs(1,3)=623.0;obs(2,3)=254.0 ;obs(3,3)=-12.9;             ;obs(8,3)=2.0
      qcf(1,3)=2.0  ;qcf(2,3)=2.0   ;qcf(3,3)=2.0 
                     oer(2,3)=1.5   ;oer(3,3)=1.0
      nlvl=3
! encode temperature and moisture
      call ufbint(unit_out,hdr,mxmn,1   ,iret,hdstr)
      call ufbint(unit_out,obs,mxmn,nlvl,iret,obstr)
      call ufbint(unit_out,oer,mxmn,nlvl,iret,oestr)
      call ufbint(unit_out,qcf,mxmn,nlvl,iret,qcstr)
      call writsb(unit_out)

   call closmg(unit_out)
 call closbf(unit_out)

end program
