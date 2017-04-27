!--------------------------------------------------------------------------
!  maingrads_mandlev
!
!  This program reads the conventional data and converts it into a GrADS
!  data file. 
!--------------------------------------------------------------------------

program maingrads_mandlev

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


      subroutine grads_mandlev(fileo,ifileo,nobs,nreal,nlev,plev,iscater,igrads,&
                isubtype,subtype,list)

         use generic_list

         integer                :: ifileo
         character(ifileo)      :: fileo
         integer                        :: nobs,nreal,nlev
         integer                        :: iscater,igrads,isubtype
         real(4),dimension(nlev)        :: plev
         character(2)                   :: subtype
         type(list_node_t), pointer     :: list
      end subroutine grads_mandlev

   end interface


   real(4),dimension(13) :: pmand 
   character(10) :: fileo,stype 
   character(3) :: intype
   character(2) :: subtype
   integer nreal,nreal_m2,iscater,igrads,isubtype,itype
   integer n_alllev,n_acft,n_lowlev,n_upair,nobs,lstype

   type(list_node_t), pointer   :: list => null()
   type(list_node_t), pointer   :: next => null()
   type(data_ptr)               :: ptr


   namelist /input/intype,stype,itype,nreal,iscater,igrads,subtype,isubtype
   integer n_mand

   data n_mand / 13 /
   data pmand /1000.,925.,850.,700.,500.,400.,300.,250.,200.,150.,100.,&
              70.,50./


   read(5,input)
   write(6,*)' User input :'
   write(6,input)

   print *, 'intype   = ', intype
   print *, 'stype    = ', stype
   print *, 'itype    = ', itype
   print *, 'nreal    = ', nreal
   print *, 'iscater  = ', iscater
   print *, 'igrads   = ', igrads 
   print *, 'subtype  = ', subtype
   print *, 'isubtype = ', isubtype

   lstype=len_trim(stype) 

   call read_conv2grads( intype,stype,itype,nreal,nobs,isubtype,subtype,list )


   if( nobs > 0 ) then 
      call grads_mandlev(stype,lstype,nobs,nreal,n_mand,pmand,iscater,igrads,&
                isubtype,subtype,list) 
   else
      print *, 'NOBS <= 0, NO OUTPUT GENERATED'
   end if

   call list_free( list )

   stop
end
