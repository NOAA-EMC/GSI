!-----------------------------------------------------------------------------
!  maingrads_lev
!
!    This program reads the conventional data and converts it into a GrADS
!    data file.  
!
!    The data is from a vertical profile having multiple levels.  The
!    hint value is the vertical level thickness, for example, level 925 
!    includes the range of 925-hint to 925+hint.
!-----------------------------------------------------------------------------

   use generic_list
   use data
 
   implicit none

   interface

      subroutine read_conv2grads(ctype,stype,itype,nreal,nobs,isubtype,subtype,list)
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


      subroutine grads_lev(fileo,ifileo,nobs,nreal,nlev,plev,iscater,igrads, &
                           levcard,hint,isubtype,subtype,list)
         use generic_list

         integer ifileo
         character(ifileo)              :: fileo
         integer                        :: nobs,nreal,nlev,igrads,isubtype
         real(4),dimension(nlev)        :: plev
         character(10)                  :: levcard
         real*4                         :: hint
         character(2) subtype
         type(list_node_t), pointer     :: list
      end subroutine grads_lev

   end interface

   integer itype
   real(4),dimension(3) :: plowlev 
   real(4),dimension(6) :: pacft 
   real(4),dimension(4) :: pupair
   real(4),dimension(10) :: palllev
   character(10) :: levcard,fileo,stype 
   character(3) :: intype
   character(2) :: subtype
   integer nreal,iscater,igrads 
   integer n_alllev,n_acft,n_lowlev,n_upair,nobs,lstype,intv,isubtype
   real hint

   type(list_node_t), pointer   :: list => null()
   type(list_node_t), pointer   :: next => null()
   type(data_ptr)               :: ptr

   namelist /input/intype,stype,itype,nreal,iscater,igrads,levcard,intv,subtype,isubtype
   data pacft /700.,600.,500.,400.,300.,100./
   data palllev /950.,850.,700.,600.,500.,400.,300.,250.,200.,100./
   data plowlev /950.,850.,700./
   data pupair /300.,250.,200.,100./
   data n_alllev / 10 /
   data n_acft / 6 /
   data n_lowlev / 3 /
   data n_upair / 4 /



   read(5,input)
   write(6,*)' User input:'
   write(6,input)

   lstype=len_trim(stype) 
   hint=real(intv)

   call read_conv2grads( intype,stype,itype,nreal,nobs,isubtype,subtype,list )

   print *, 'AFTER read_conv2grads, nreal =', nreal

   if( nobs > 0 ) then
      if(trim(levcard) == 'alllev' ) then
         call grads_lev(stype,lstype,nobs,nreal,n_alllev,palllev,iscater, &
                        igrads,levcard,hint,isubtype,subtype,list)
      else if (trim(levcard) == 'acft' ) then
         call grads_lev(stype,lstype,nobs,nreal,n_acft,pacft,iscater,igrads,&
                        levcard,hint,isubtype,subtype,list)
      else if(trim(levcard) == 'lowlev' ) then
         call grads_lev(stype,lstype,nobs,nreal,n_lowlev,plowlev,iscater,&
                        igrads,levcard,hint,isubtype,subtype,list)
      else if(trim(levcard) == 'upair' ) then
         call grads_lev(stype,lstype,nobs,nreal,n_upair,pupair,iscater,&
                        igrads,levcard,hint,isubtype,subtype,list)
      end if
   else
      print *, 'NOBS <= 0, NO OUTPUT GENERATED' 
   end if

   call list_free( list ) 

   stop
end
