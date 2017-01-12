program prepbufr_encode_upperair
!
!  write a upper air observation into prepbufr file
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
!
! write observation into prepbufr file
!
 open(unit_table,file='prepbufr.table',action='read')
 open(unit_out,file='prepbufr',action='write',form='unformatted')
 call datelen(10)
 call openbf(unit_out,'OUT',unit_table)

   idate=2010050700 ! cycle time: YYYYMMDDHH
   subset='ADPUPA'  ! upper-air (raob, drops) reports
   call openmb(unit_out,subset,idate)

! set headers
      hdr=10.0e10
      c_sid='72293'; hdr(1)=rstation_id
      hdr(2)=242.9; hdr(3)=32.9; hdr(4)=0.0; hdr(6)=134.0

! set obs, qcf, oer for  wind
      hdr(5)=220          ! report type: sounding 
      obs=10.0e10;qcf=10.0e10;oer=10.0e10
      obs(1,1)=998.0; obs(5,1)=4.6 ;obs(6,1)=2.2 ;obs(8,1)=3.0
      qcf(1,1)=2.0  ; qcf(5,1)=2.0
                      oer(5,1)=2.3
      obs(1,2)=850.0; obs(5,2)=2.0 ;obs(6,2)=-1.7;obs(8,2)=1.0
      qcf(1,2)=2.0  ; qcf(5,2)=2.0
                      oer(5,2)=2.6
      obs(1,3)=700.0; obs(5,3)=12.1;obs(6,3)=-4.4;obs(8,3)=1.0
      qcf(1,3)=2.0  ; qcf(5,3)=2.0
                      oer(5,3)=2.5
      nlvl=3
! encode  wind obs
      call ufbint(unit_out,hdr,mxmn,1   ,iret,hdstr)
      call ufbint(unit_out,obs,mxmn,nlvl,iret,obstr)
      call ufbint(unit_out,oer,mxmn,nlvl,iret,oestr)
      call ufbint(unit_out,qcf,mxmn,nlvl,iret,qcstr)
      call writsb(unit_out)

! set obs, qcf, oer for  temperature and moisture
      hdr(5)=120          ! report type: sounding
      obs=10.0e10;qcf=10.0e10;oer=10.0e10
      obs(1,1)=998.0;obs(2,1)=8112.0;obs(3,1)=22.3;obs(4,1)=134.0;obs(8,1)=0.0
      qcf(1,1)=2.0  ;qcf(2,1)=2.0   ;qcf(3,1)=2.0 ;qcf(4,1)=2.0
      oer(1,1)=0.7  ;oer(2,1)=0.7   ;oer(3,1)=1.4 
      obs(1,2)=925.0;obs(2,2)=6312.0;obs(3,2)=14.1;obs(4,2)=779.0;obs(8,2)=1.0
      qcf(1,2)=2.0  ;qcf(2,2)=2.0   ;qcf(3,2)=2.0 ;qcf(4,2)=2.0
                     oer(2,2)=0.9   ;oer(3,2)=1.5 
      obs(1,3)=850.0;obs(2,3)=2161.0;obs(3,3)=14.8;obs(4,3)=1493.;obs(8,3)=1.0
      qcf(1,3)=2.0  ;qcf(2,3)=2.0   ;qcf(3,3)=2.0 ;qcf(4,3)=2.0
                     oer(2,3)=1.1   ;oer(3,3)=1.4 
      obs(1,4)=700.0;obs(2,4)=2131.0;obs(3,4)=9.2 ;obs(4,4)=3118.;obs(8,4)=1.0
      qcf(1,4)=2.0  ;qcf(2,4)=2.0   ;qcf(3,4)=2.0 ;qcf(4,4)=2.0
                     oer(2,4)=1.4   ;oer(3,4)=1.0 
      nlvl=4
! encode temperature and moisture
      call ufbint(unit_out,hdr,mxmn,1   ,iret,hdstr)
      call ufbint(unit_out,obs,mxmn,nlvl,iret,obstr)
      call ufbint(unit_out,oer,mxmn,nlvl,iret,oestr)
      call ufbint(unit_out,qcf,mxmn,nlvl,iret,qcstr)
      call writsb(unit_out)

   call closmg(unit_out)
 call closbf(unit_out)

end program
