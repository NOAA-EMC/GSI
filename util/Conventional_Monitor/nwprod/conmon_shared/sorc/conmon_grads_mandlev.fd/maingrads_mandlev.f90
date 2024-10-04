!--------------------------------------------------------------------------
!  maingrads_mandlev
!
!  This program reads the conventional data and converts it into a GrADS
!  data file. 
!--------------------------------------------------------------------------

program maingrads_mandlev

   use generic_list
   use data
   use conmon_read_diag

   implicit none

   interface

      subroutine grads_mandlev(fileo,ifileo,nobs,nreal,nlev,plev,iscater,igrads,&
                isubtype,subtype,list,run)

         use generic_list

         integer                :: ifileo
         character(ifileo)      :: fileo
         integer                        :: nobs,nreal,nlev
         integer                        :: iscater,igrads,isubtype
         real(4),dimension(nlev)        :: plev
         character(3)                   :: subtype
         type(list_node_t), pointer     :: list
         character(3)                   :: run
      end subroutine grads_mandlev

   end interface


   real(4),dimension(13) :: pmand 
   character(10) :: fileo,stype 
   character(3) :: intype
   character(3) :: subtype
   integer nreal,nreal_m2,iscater,igrads,isubtype,itype
   integer n_alllev,n_acft,n_lowlev,n_upair,nobs,lstype

   type(list_node_t), pointer   :: list => null()
   type(list_node_t), pointer   :: next => null()
   type(data_ptr)               :: ptr

   !--- namelist with defaults
   logical               :: netcdf              = .false.
   character(100)        :: input_file          = "conv_diag"
   character(3)          :: run                 = "ges"
   namelist /input/input_file,intype,stype,itype,nreal,iscater,igrads,subtype,isubtype,netcdf,run

   integer n_mand
   data n_mand / 13 /
   data pmand /1000.,925.,850.,700.,500.,400.,300.,250.,200.,150.,100.,&
              70.,50./


   read(5,input)
   write(6,*)' User input :'
   write(6,input)

   print *, 'input_file = ', input_file
   print *, 'intype   = ', intype
   print *, 'stype    = ', stype
   print *, 'itype    = ', itype
   print *, 'nreal    = ', nreal
   print *, 'iscater  = ', iscater
   print *, 'igrads   = ', igrads 
   print *, 'subtype  = ', subtype
   print *, 'isubtype = ', isubtype
   print *, 'netcdf   = ', netcdf  

   lstype=len_trim(stype) 

   write(6,*)'netcdf       =', netcdf
   call set_netcdf_read( netcdf )

   call conmon_read_diag_file( input_file,intype,itype,nreal,nobs,isubtype,list )


   if( nobs > 0 ) then 
      call grads_mandlev(stype,lstype,nobs,nreal,n_mand,pmand,iscater,igrads,&
                isubtype,subtype,list,run) 
   else
      print *, 'NOBS <= 0, NO OUTPUT GENERATED'
   end if

   call list_free( list )

   stop
end
