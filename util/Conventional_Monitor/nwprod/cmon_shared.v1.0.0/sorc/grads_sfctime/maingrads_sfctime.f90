!-----------------------------------------------------------------------------
!  maingrads_sfctime
!
!    This program reads the conventional data and converts it into a GrADS
!    data file.  The data is a profile type having multiple levels.
!
!-----------------------------------------------------------------------------
program maingrads_sfctime

   use generic_list
   use data

   implicit none


   interface

      subroutine read_conv2grads(ctype,stype,itype,nreal,nobs,&
                                 isubtype,subtype,list)
         use generic_list
         character(3)           :: ctype
         character(10)          :: stype
         integer                :: itype
         integer                :: nreal
         integer                :: nobs
         integer                :: isubtype
         character(2)           :: subtype
         type(list_node_t),pointer   :: list
      end subroutine read_conv2grads


      subroutine grads_sfctime(fileo,ifileo,nobs,nreal,&
                    nlev,plev,iscater,igrads,isubtype,subtype,list)

         use generic_list
         character(ifileo)       :: fileo
         integer                 :: ifileo,nobs,nreal,nlev 
         real(4),dimension(nlev) :: plev
         integer                 :: iscater,igrdas,isubtype
         character(2)            :: subtype
         type(list_node_t),pointer   :: list
         
      end subroutine grads_sfctime

   end interface


   type(list_node_t), pointer   :: list => null()
   type(list_node_t), pointer   :: next => null()
   type(data_ptr)               :: ptr

   real(4),dimension(11) :: ptime11 
   real(4),dimension(7) :: ptime7
   character(10) :: fileo,stype,timecard 
   character(3) :: intype
   character(2) :: subtype
   integer nreal,iscater,igrads,isubtype 
   integer nobs,lstype
   integer n_time7,n_time11,itype

   namelist /input/intype,stype,itype,nreal,iscater,igrads,timecard,isubtype,subtype

   data n_time11 / 11 /
   data n_time7 / 7 /
   data ptime11 / -2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5 /
   data ptime7 / -3.0,-2.0,-1.0,0.0,1.0,2.0,3.0 /

   read(5,input)
   write(6,*)' User input below'
   write(6,input)

   lstype=len_trim(stype) 

   call read_conv2grads(intype,stype,itype,nreal,nobs,isubtype,subtype,list)

   if( nobs > 0 ) then

      if( trim(timecard) == 'time11') then
!         print *, 'time11'
         call grads_sfctime(stype,lstype,nobs,nreal,n_time11,&
                            ptime11,iscater,igrads,isubtype,subtype, list) 
      else if( trim(timecard) == 'time7') then 
!         print *, 'time7'
         call grads_sfctime(stype,lstype,nobs,nreal,n_time7,&
                            ptime7,iscater,igrads,isubtype,subtype,list) 
      endif
   
   else
      print *, 'NOBS <= 0, NO OUTPUT GENERATED'
   end if

   call list_free( list )

   stop
end
