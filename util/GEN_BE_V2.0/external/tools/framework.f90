

module framework

   use configure
   use timer


   contains

   
   subroutine mpas_framework_init(dminfo, domain)

      implicit none

      type (dm_info), pointer :: dminfo
      type (domain_type), pointer :: domain

      allocate(dminfo)
      call dmpar_init(dminfo)

      call read_namelist(dminfo)

      call allocate_domain(domain, dminfo)

   end subroutine mpas_framework_init

   
   subroutine mpas_framework_finalize(dminfo, domain)
  
      implicit none

      type (dm_info), pointer :: dminfo
      type (domain_type), pointer :: domain

      call deallocate_domain(domain)

      call dmpar_finalize(dminfo)

   end subroutine mpas_framework_finalize

end module mpas_framework
