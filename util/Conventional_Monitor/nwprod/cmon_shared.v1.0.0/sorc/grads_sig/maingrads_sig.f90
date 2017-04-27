!-----------------------------------------------------------------------------
!  maingrads_sig
!
!    This program reads the conventional data and converts it into a GrADS
!    data file.  The data is from a vertical profile having multiple levels.
!
!-----------------------------------------------------------------------------
program maingrads_sig


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


      subroutine grads_sig(fileo,ifileo,nobs,nreal,nlev,plev,iscater,igrads,isubtype, &
                           subtype,list)
         use generic_list

         integer ifileo
         character(ifileo)              :: fileo
         integer                        :: nobs,nreal,nlev,igrads,isubtype
         real(4),dimension(nlev)        :: plev
         character(2) subtype
         type(list_node_t), pointer     :: list
      end subroutine grads_sig

   end interface


   real(4),dimension(46) :: psig 
   character(10) :: fileo,stype,time
   character(3) :: intype
   character(2) :: subtype
   integer nreal,nreal_m2,iscater,igrads,isubtype 
   integer n_alllev,n_acft,n_lowlev,n_upair,nobs,lstype
   integer itype,n_sig,ii

   type(list_node_t), pointer   :: list => null() 
   type(list_node_t), pointer   :: next => null() 
   type(data_ptr)               :: ptr


   namelist /input/intype,stype,itype,nreal,iscater,igrads,subtype,isubtype

   data n_sig / 46 /
!----------------------------------------------------------------
!  the psig levels are used to match the level in the pgb files
   data psig /997.,992.,985.,978.,970.,960.,950.,938.,&
             925.,911.,895.,877.,850.,837.,814.,789.,762.,&
             733.,700.,671.,638.,600.,570.,534.,500.,463.,&
             428.,400.,361.,329.,300.,271.,250.,219.,200.,& 
             175.,156.,138.,122.,100.,95.,83.,73.,64.,55.,48. /


   call date_and_time(TIME=time)
   print *, 'time = ', time

   read(5,input)

   write(6,*)' User input: '
   write(6,input)

   lstype=len_trim(stype) 

   call read_conv2grads(intype,stype,itype,nreal,nobs,isubtype,subtype,list)


   if( nobs > 0 ) then
      call grads_sig(stype,lstype,nobs,nreal,n_sig,psig,iscater,igrads, &
                     isubtype,subtype,list) 
   else
      print *, 'NOBS <= 0, NO OUTPUT GENERATED'
   end if


   call list_free( list )

   call date_and_time( TIME=time )
   print *, 'time = ', time

   stop

end program maingrads_sig
