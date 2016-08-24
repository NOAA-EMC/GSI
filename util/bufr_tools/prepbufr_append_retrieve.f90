program prepbufr_append_retrieve
!
!  append a retrieved data into prepbufr file
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
   subset='SATWND'  ! upper-air (raob, drops) reports
   call openmb(unit_out,subset,idate)

! set headers
      hdr=10.0e10
      c_sid='A114424Z'; hdr(1)=rstation_id
      hdr(2)=199.6; hdr(3)=13.9; hdr(4)=-1.0; hdr(6)=934.0; hdr(7)=255.0

! set obs, qcf, oer for  wind
      hdr(5)=251          ! report type: NESDIS VISIBLE CLOUD DRIFT
                          !             (ALL LEVELS) (GOES) - u, v
      obs=10.0e10;qcf=10.0e10;oer=10.0e10
      obs(1,1)=906.0;obs(4,1)=934.0;obs(5,1)=-11.4;obs(6,1)=-3.3;obs(8,1)=6.0
      qcf(1,1)=2.0  ;qcf(4,1)=2.0  ;qcf(5,1)=1.0
                                    oer(5,1)=3.8
      nlvl=1
! encode  wind obs
      call ufbint(unit_out,hdr,mxmn,1   ,iret,hdstr)
      call ufbint(unit_out,obs,mxmn,nlvl,iret,obstr)
      call ufbint(unit_out,oer,mxmn,nlvl,iret,oestr)
      call ufbint(unit_out,qcf,mxmn,nlvl,iret,qcstr)
      call writsb(unit_out)

   call closmg(unit_out)
 call closbf(unit_out)

end program
