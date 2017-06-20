!-----------------------------------------------------------------------------
!  maingrads_sfc
!
!    This program reads the conventional data and converts it into GrADS
!    data files.  The data is a profile type having multiple levels.
!
!-----------------------------------------------------------------------------
program maingrads_sfc

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


      subroutine grads_sfc(fileo,ifileo,nobs,nreal,iscater,igrads,&
                           isubtype, subtype, list)
         use generic_list

         integer ifileo
         character(ifileo)              :: fileo
         integer                        :: nobs,nreal,iscater,igrads,isubtype
         character(2)                   :: subtype
         type(list_node_t), pointer     :: list
      end subroutine grads_sfc

   end interface

   real(4),dimension(21) :: pmand 
   character(10) :: fileo,stype 
   character(3) :: intype
   character(2) :: subtype
   integer nreal,nreal_m2,iscater,igrads,isubtype 
   integer n_alllev,n_acft,n_lowlev,n_upair,nobs,lstype
   integer n_mand,itype

   type(list_node_t), pointer   :: list => null()
   type(list_node_t), pointer   :: next => null()
   type(data_ptr)               :: ptr

   namelist /input/intype,stype,itype,nreal,iscater,igrads,subtype,isubtype

   data n_mand / 21 /
   data pmand /1000.,925.,850.,700.,500.,400.,300.,250.,200.,150.,100.,&
               70.,50.,30.,20.,10.,7.,5.,3.,2.,1./

   read(5,input)
   write(6,*)' User input below'
   write(6,input)

   lstype=len_trim(stype) 

   call read_conv2grads( intype,stype,itype,nreal,nobs,isubtype,subtype,list )

 
   if( nobs > 0 ) then
      call grads_sfc(stype,lstype,nobs,nreal,iscater,igrads,isubtype,subtype,list) 
   else
      print *, 'NOBS <= 0, NO OUTPUT GENERATED'
   end if

   call list_free( list )

   stop
end
