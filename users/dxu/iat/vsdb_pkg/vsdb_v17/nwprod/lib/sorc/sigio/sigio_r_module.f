!-------------------------------------------------------------------------------
module sigio_r_module
!$$$  Module Documentation Block
!
! Module:    sigio_r_module  API for global spectral sigma file random I/O
!   Prgmmr: Iredell          Org: W/NX23     Date: 1999-01-18
!
! Abstract: This module provides an Application Program Interface extension
!   for performing I/O on the sigma restart file of the global spectral model.
!   Functions include opening, reading, writing, and closing as well as
!   allocating and deallocating data buffers used in the transfers.
!   The I/O performed here is random.
!   The transfers are limited to header records, data records,
!   surface data records, or specific levels of upper air data records.
!   See the documentation for sigio_module for sequential I/O.
!   
! Program History Log:
!   1999-01-18  Mark Iredell
!
! Modules Used:
!   sigio_module     API for global spectral sigma file I/O
!
! Public Variables:
!
! Public Defined Types:
!   sigio_dats       Sigma file surface data fields
!     hs                Real(sigio_realkind)(:) pointer to spectral
!                       coefficients of surface height in m
!     ps                Real(sigio_realkind)(:) pointer to spectral
!                       coefficients of log of surface pressure over 1 kPa
!
!   sigio_datm       Sigma file multilevel data fields
!     k1                Integer(sigio_intkind) first level number
!     k2                Integer(sigio_intkind) last level number
!     t                 Real(sigio_realkind)(:,:) pointer to spectral
!                       coefficients of virtual temperature by level in K
!     d                 Real(sigio_realkind)(:,:) pointer to spectral
!                       coefficients of divergence by level in 1/second
!     z                 Real(sigio_realkind)(:,:) pointer to spectral
!                       coefficients of vorticity by level in 1/second
!     q                 Real(sigio_realkind)(:,:,:) pointer to spectral
!                       coefficients of tracers by tracer number and level
!                       in specific densities
!
!   sigio_dati       Sigma file single data field
!     i                 Integer(sigio_intkind) record index
!     f                 Real(sigio_realkind)(:) pointer to field
!                       
!   sigio_dbts       Sigma file longreal surface data fields
!     hs                Real(sigio_dblekind)(:) pointer to spectral
!                       coefficients of surface height in m
!     ps                Real(sigio_dblekind)(:) pointer to spectral
!                       coefficients of log of surface pressure over 1 kPa
!
!   sigio_dbtm       Sigma file longreal multilevel data fields
!     k1                Integer(sigio_intkind) first level number
!     k2                Integer(sigio_intkind) last level number
!     t                 Real(sigio_dblekind)(:,:) pointer to spectral
!                       coefficients of virtual temperature by level in K
!     d                 Real(sigio_dblekind)(:,:) pointer to spectral
!                       coefficients of divergence by level in 1/second
!     z                 Real(sigio_dblekind)(:,:) pointer to spectral
!                       coefficients of vorticity by level in 1/second
!     q                 Real(sigio_dblekind)(:,:,:) pointer to spectral
!                       coefficients of tracers by tracer number and level
!                       in specific densities
!
!   sigio_dbti       Sigma file longreal single data field
!     i                 Integer(sigio_intkind) record index
!     f                 Real(sigio_dblekind)(:) pointer to field
!                       
! Public Subprograms:
!   sigio_rropen     Open sigma file for random reading
!     lu                Integer(sigio_intkind) input logical unit
!     cfname            Character(*) input filename
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_rwopen     Open sigma file for random writing
!     lu                Integer(sigio_intkind) input logical unit
!     cfname            Character(*) input filename
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_rxopen     Open sigma file for random reading and writing
!     lu                Integer(sigio_intkind) input logical unit
!     cfname            Character(*) input filename
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_rclose     Close sigma file for random I/O
!     lu                Integer(sigio_intkind) input logical unit
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_rrhead     Read header information with random I/O
!     lu                Integer(sigio_intkind) input logical unit
!     head              Type(sigio_head) output header information
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_rwhead     Write header information with random I/O
!     lu                Integer(sigio_intkind) input logical unit
!     head              Type(sigio_head) input header information
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_aldats     Allocate surface data fields
!     head              Type(sigio_head) input header information
!     dats              Type(sigio_dats) output surface data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_axdats     Deallocate surface data fields
!     dats              Type(sigio_dats) output surface data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_aldatm     Allocate multilevel data fields
!     head              Type(sigio_head) input header information
!     k1                Integer(sigio_intkind) input first level number
!     k2                Integer(sigio_intkind) input last level number
!     datm              Type(sigio_datm) output multilevel data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_axdatm     Deallocate multilevel data fields
!     datm              Type(sigio_datm) output multilevel data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_aldati     Allocate single data fields
!     head              Type(sigio_head) input header information
!     i                 Integer(sigio_intkind) input record index
!     dati              Type(sigio_dati) output single data field
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_axdati     Deallocate single data fields
!     dati              Type(sigio_dati) output single data field
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_rrdata     Read data fields with random I/O
!     lu                Integer(sigio_intkind) input logical unit
!     head              Type(sigio_head) input header information
!     data              Type(sigio_data) output data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_rwdata     Write data fields with random I/O
!     lu                Integer(sigio_intkind) input logical unit
!     head              Type(sigio_head) input header information
!     data              Type(sigio_data) input data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_rrohdc     Open, read header & data and close with random I/O
!     lu                Integer(sigio_intkind) input logical unit
!     cfname            Character(*) input filename
!     head              Type(sigio_head) output header information
!     data              Type(sigio_data) or type(sigio_dbta) output data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_rwohdc     Open, write header & data and close with random I/O
!     lu                Integer(sigio_intkind) input logical unit
!     cfname            Character(*) input filename
!     head              Type(sigio_head) input header information
!     data              Type(sigio_data) or type(sigio_dbta) input data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_rrdats     Read surface data fields with random I/O
!     lu                Integer(sigio_intkind) input logical unit
!     head              Type(sigio_head) input header information
!     dats              Type(sigio_dats) output surface data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_rwdats     Write surface data fields with random I/O
!     lu                Integer(sigio_intkind) input logical unit
!     head              Type(sigio_head) input header information
!     dats              Type(sigio_dats) input surface data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_rrdatm     Read multilevel data fields with random I/O
!     lu                Integer(sigio_intkind) input logical unit
!     head              Type(sigio_head) input header information
!     datm              Type(sigio_datm) output multilevel data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_rwdatm     Write multilevel data fields with random I/O
!     lu                Integer(sigio_intkind) input logical unit
!     head              Type(sigio_head) input header information
!     datm              Type(sigio_datm) input multilevel data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_rrdati     Read single data field with random I/O
!     lu                Integer(sigio_intkind) input logical unit
!     head              Type(sigio_head) input header information
!     dati              Type(sigio_dati) output single data field
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_rwdati     Write single data field with random I/O
!     lu                Integer(sigio_intkind) input logical unit
!     head              Type(sigio_head) input header information
!     dati              Type(sigio_dati) input single data field
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_aldbts     Allocate longreal surface data fields
!     head              Type(sigio_head) input header information
!     dbts              Type(sigio_dbts) output longreal surface data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_axdbts     Deallocate longreal surface data fields
!     dbts              Type(sigio_dbts) output longreal surface data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_aldbtm     Allocate longreal multilevel data fields
!     head              Type(sigio_head) input header information
!     k                 Integer(sigio_intkind) input level number
!     dbtm              Type(sigio_dbtm) output longreal multilevel data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_axdbtm     Deallocate longreal multilevel data fields
!     dbtm              Type(sigio_dbtm) output longreal multilevel data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_aldbti     Allocate longreal single data fields
!     head              Type(sigio_head) input header information
!     i                 Integer(sigio_intkind) input record index
!     dbti              Type(sigio_dbti) output longreal single data field
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_axdbti     Deallocate longreal single data fields
!     dbti              Type(sigio_dbti) output longreal single data field
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_rrdbta     Read longreal data fields with random I/O
!     lu                Integer(sigio_intkind) input logical unit
!     head              Type(sigio_head) input header information
!     dbta              Type(sigio_dbta) output longreal data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_rwdbta     Write longreal data fields with random I/O
!     lu                Integer(sigio_intkind) input logical unit
!     head              Type(sigio_head) input header information
!     dbta              Type(sigio_dbta) input longreal data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_rrdbts     Read longreal surface data fields with random I/O
!     lu                Integer(sigio_intkind) input logical unit
!     head              Type(sigio_head) input header information
!     dbts              Type(sigio_dbts) output longreal surface data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_rwdbts     Write longreal surface data fields with random I/O
!     lu                Integer(sigio_intkind) input logical unit
!     head              Type(sigio_head) input header information
!     dbts              Type(sigio_dbts) input longreal surface data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_rrdbtm     Read longreal multilevel data fields with random I/O
!     lu                Integer(sigio_intkind) input logical unit
!     head              Type(sigio_head) input header information
!     dbtm              Type(sigio_dbtm) output longreal multilevel data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_rwdbtm     Write longreal multilevel data fields with random I/O
!     lu                Integer(sigio_intkind) input logical unit
!     head              Type(sigio_head) input header information
!     dbtm              Type(sigio_dbtm) input longreal multilevel data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_rrdbti     Read longreal single data field with random I/O
!     lu                Integer(sigio_intkind) input logical unit
!     head              Type(sigio_head) input header information
!     dbti              Type(sigio_dbti) output longreal single data field
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_rwdbti     Write longreal single data field with random I/O
!     lu                Integer(sigio_intkind) input logical unit
!     head              Type(sigio_head) input header information
!     dbti              Type(sigio_dbti) input longreal single data field
!     iret              Integer(sigio_intkind) output return code
!
! Subprograms called:
!   baopenr           Byte-addressable open for reading
!   baopenw           Byte-addressable open for writing
!   baclose           Byte-addressable close
!   bafrindexl        Byte-addressable Fortran record index
!   bafrreadl         Byte-addressable Fortran record read
!   bafrwritel        Byte-addressable Fortran record write
!
! Remarks:
!   (1) The sigma file format follows:
!         ON85 label (32 bytes)
!         Header information record containing
!           real forecast hour, initial date, sigma interfaces, sigma levels,
!           padding to allow for 100 levels, and finally 44 identifier words
!           containing JCAP, LEVS, NTRAC, etc. (250 4-byte words)
!         Orography (NC 4-byte words, where NC=(JCAP+1)*(JCAP+2))
!         Log surface pressure (NC 4-byte words)
!         Temperature (LEVS records of NC 4-byte words)
!         Divergence & Vorticity interleaved (2*LEVS records of NC 4-byte words)
!         Tracers (LEVS*NTRAC records of NC 4-byte words)
!
!   (2) Possible return codes:
!          0   Successful call
!         -1   Open or close I/O error
!         -2   Header record I/O error (possible EOF)
!         -3   Allocation or deallocation error
!         -4   Data record I/O error
!         -5   Insufficient data dimensions allocated
!
! Examples:
!   (1) Write out orography and surface pressure only from processor 0:
!
!     subroutine write_surface_fields(me,head,len,orog,lnps)
!     use sigio_r_module
!     integer,intent(in):: me
!     type(sigio_head),intent(in):: head
!     integer,intent(in):: len
!     real(sigio_dblekind),intent(in),target:: orog(len),lnps(len)
!     type(sigio_dbts) dbts
!     integer iret
!     if(me.eq.0) then
!       dbts%hs=>orog
!       dbts%ps=>lnps
!       call sigio_rwdbts(51,head,dbts,iret)
!     endif
!     end subroutine
! 
! Attributes:
!   Language: Fortran 90
!
!$$$
  use sigio_module
  implicit none
  private
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Public Variables
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Public Types
  type,public:: sigio_dats
    real(sigio_realkind),pointer:: hs(:),ps(:)
  end type
  type,public:: sigio_datm
    integer(sigio_intkind):: k1,k2
    real(sigio_realkind),pointer:: t(:,:),d(:,:),z(:,:)
    real(sigio_realkind),pointer:: q(:,:,:)
  end type
  type,public:: sigio_dati
    integer(sigio_intkind):: i
    real(sigio_realkind),pointer:: f(:)
  end type
  type,public:: sigio_dbts
    real(sigio_dblekind),pointer:: hs(:),ps(:)
  end type
  type,public:: sigio_dbtm
    integer(sigio_intkind):: k1,k2
    real(sigio_dblekind),pointer:: t(:,:),d(:,:),z(:,:)
    real(sigio_dblekind),pointer:: q(:,:,:)
  end type
  type,public:: sigio_dbti
    integer(sigio_intkind):: i
    real(sigio_dblekind),pointer:: f(:)
  end type
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Private Variables
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Private Types
  type sigio_head2
    sequence
    real(sigio_realkind):: fhour
    integer(sigio_intkind):: idate(4)
    real(sigio_realkind):: sisl(2*100+1)
    real(sigio_realkind):: ext(44)
  end type
  type sigio_head1a
    sequence
    character(8):: clab8
    integer(sigio_intkind):: ivs,nhead,ndata,reserved(3)
  end type
  type sigio_head3a
    sequence
    real(sigio_realkind) fhour
    integer(sigio_intkind):: idate(4)
    integer(sigio_intkind):: jcap,levs,&
      itrun,iorder,irealf,igen,latf,lonf,&
      latb,lonb,latr,lonr,ntrac,nvcoord,&
      icen2,iens(2),idpp,idsl,idvc,idvm,&
      idvt,idrun,idusr
    real(sigio_realkind) pdryini
    integer(sigio_intkind):: ncldt,ixgr
    integer(sigio_intkind):: reserved(18)
  end type
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Public Subprograms
  public sigio_rropen,sigio_rwopen,sigio_rxopen,sigio_rclose
  public sigio_rrhead,sigio_rwhead
  public sigio_aldats,sigio_axdats
  public sigio_aldatm,sigio_axdatm
  public sigio_aldati,sigio_axdati
  public sigio_rrdata,sigio_rwdata
  public sigio_rrohdc,sigio_rwohdc
  public sigio_rrdats,sigio_rwdats
  public sigio_rrdatm,sigio_rwdatm
  public sigio_rrdati,sigio_rwdati
  public sigio_aldbts,sigio_axdbts
  public sigio_aldbtm,sigio_axdbtm
  public sigio_aldbti,sigio_axdbti
  public sigio_rrdbta,sigio_rwdbta
  public sigio_rrdbts,sigio_rwdbts
  public sigio_rrdbtm,sigio_rwdbtm
  public sigio_rrdbti,sigio_rwdbti
  interface sigio_rrohdc
    module procedure sigio_rrohdca,sigio_rrohdcb
  end interface
  interface sigio_rwohdc
    module procedure sigio_rwohdca,sigio_rwohdcb
  end interface
contains
!-------------------------------------------------------------------------------
  subroutine sigio_rropen(lu,cfname,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    integer(sigio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call baopenr(lu,cfname,iret)
    if(iret.ne.0) iret=-1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_rwopen(lu,cfname,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    integer(sigio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call baopenw(lu,cfname,iret)
    if(iret.ne.0) iret=-1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_rxopen(lu,cfname,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    integer(sigio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call baopen(lu,cfname,iret)
    if(iret.ne.0) iret=-1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_rclose(lu,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    integer(sigio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call baclose(lu,iret)
    if(iret.ne.0) iret=-1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_rrhead(lu,head,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    type(sigio_head),intent(inout):: head
    integer(sigio_intkind),intent(out):: iret
    type(sigio_head2):: head2
    type(sigio_head1a):: head1a
    type(sigio_head3a):: head3a
    integer(8):: iskip,iread,nread
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-2
    iskip=0
    iread=sigio_lhead1
    call bafrreadl(lu,iskip,iread,nread,head1a)
    if(nread.lt.iread) return
!LLF+PM--
    call byteswap(head1a%ivs,sigio_intkind,6)
!LLF+PM==
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head1a%clab8.eq.'GFS SIG ') then  ! modern sigma file
      head%ivs=head1a%ivs
      call bafrindexl(lu,iskip+nread,nread,iskip)
      iread=200
      call bafrreadl(lu,iskip,iread,nread,head3a)
      if(nread.lt.iread) return
!LLF+PM--
      call byteswap(head3a%fhour,sigio_realkind,1)
      call byteswap(head3a%idate,sigio_intkind,28)
      call byteswap(head3a%pdryini,sigio_realkind,1)
      call byteswap(head3a%ncldt,sigio_intkind,20)
!LLF+PM==
      head%fhour=head3a%fhour
      head%idate=head3a%idate
      head%jcap=head3a%jcap
      head%levs=head3a%levs
      head%itrun=head3a%itrun
      head%iorder=head3a%iorder
      head%irealf=head3a%irealf
      head%igen=head3a%igen
      head%latf=head3a%latf
      head%lonf=head3a%lonf
      head%latb=head3a%latb
      head%lonb=head3a%lonb
      head%latr=head3a%latr
      head%lonr=head3a%lonr
      head%ntrac=head3a%ntrac
      head%nvcoord=head3a%nvcoord
      head%icen2=head3a%icen2
      head%iens=head3a%iens
      head%idpp=head3a%idpp
      head%idsl=head3a%idsl
      head%idvc=head3a%idvc
      head%idvm=head3a%idvm
      head%idvt=head3a%idvt
      head%idrun=head3a%idrun
      head%idusr=head3a%idusr
      head%pdryini=head3a%pdryini
      head%ncldt=head3a%ncldt
      head%ixgr=head3a%ixgr
      call sigio_alhead(head,iret)
      iskip=iskip+nread
      iread=4*size(head%vcoord)
      call bafrreadl(lu,iskip,iread,nread,head%vcoord)
      if(nread.lt.iread) return
!LLF+PM--
      call byteswap(head%vcoord,sigio_realkind,iread/4)
!LLF+PM==
      iskip=iskip+nread
      iread=size(head%cfvars)
      call bafrreadl(lu,iskip,iread,nread,head%cfvars)
      if(nread.lt.iread) return
!
      if (mod(head%idvm/10,10) == 3) then
        iskip=iskip+nread
        iread=4*size(head%cpi)
        call bafrreadl(lu,iskip,iread,nread,head%cpi)
        if(nread.lt.iread) return
        call byteswap(head%cpi,sigio_realkind,iread/4)
        iskip=iskip+nread
        iread=4*size(head%ri)
        call bafrreadl(lu,iskip,iread,nread,head%ri)
        if(nread.lt.iread) return
        call byteswap(head%ri,sigio_realkind,iread/4)
      endif
      head%clabsig=' '
      head%si=sigio_realfill
      head%sl=sigio_realfill
      head%ak=sigio_realfill
      head%bk=sigio_realfill
      head%pdryini=sigio_realfill
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    else
      iskip=0
      iread=sigio_lhead1
      call bafrreadl(lu,iskip,iread,nread,head%clabsig)
      if(nread.lt.iread) return
      iskip=iskip+nread
      iread=1000
      call bafrreadl(lu,iskip,iread,nread,head2)
      if(nread.lt.iread) return
!LLF+PM--
      call byteswap(head2%fhour,sigio_realkind,1)
      call byteswap(head2%idate,sigio_intkind,4)
      call byteswap(head2%sisl,sigio_realkind,245)
!LLF+PM==
      iret=0
      head%fhour=head2%fhour
      head%idate=head2%idate
      head%jcap=head2%ext(1)
      head%levs=head2%ext(2)
      head%itrun=head2%ext(3)
      head%iorder=head2%ext(4)
      head%irealf=head2%ext(5)
      head%igen=head2%ext(6)
      head%lonf=head2%ext(7)
      head%latf=head2%ext(8)
      head%lonb=head2%ext(9)
      head%latb=head2%ext(10)
      head%lonr=head2%ext(11)
      head%latr=head2%ext(12)
      head%ntrac=max(head2%ext(13),1.)
      head%icen2=head2%ext(14)
      head%iens=head2%ext(15:16)
      head%idpp=head2%ext(17)
      head%idsl=head2%ext(18)
      head%idvc=head2%ext(19)
      head%idvm=head2%ext(20)
      head%idvt=head2%ext(21)
      head%idrun=head2%ext(22)
      head%idusr=head2%ext(23)
      head%pdryini=head2%ext(24)
      head%ncldt=head2%ext(25)
      head%si=sigio_realfill
      head%sl=sigio_realfill
      head%ak=sigio_realfill
      head%bk=sigio_realfill
      if(head%idvc.eq.2) then
        head%ak(1:head%levs+1)=head2%sisl(1:head%levs+1)
        head%bk(1:head%levs+1)=head2%sisl(head%levs+2:2*head%levs+2)
      else
        head%si(1:head%levs+1)=head2%sisl(1:head%levs+1)
        head%sl(1:head%levs)=head2%sisl(head%levs+2:2*head%levs+1)
      endif
      head%ivs=198410
      if(head%idvc.eq.2) then
        head%nvcoord=2
        call sigio_alhead(head,iret)
        head%vcoord(1:head%levs+1,1)=head%ak(1:head%levs+1)
        head%vcoord(1:head%levs+1,2)=head%bk(1:head%levs+1)
      elseif(head%idvc.eq.0.or.head%idvc.eq.1) then
        head%nvcoord=1
        call sigio_alhead(head,iret)
        head%vcoord(1:head%levs+1,1)=head%si(1:head%levs+1)
      endif
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_adhead(head)
    iret=0
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_rwhead(lu,head,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    type(sigio_head),intent(inout):: head
    integer(sigio_intkind),intent(out):: iret
    type(sigio_head2):: head2
    type(sigio_head1a):: head1a
    integer,allocatable:: head2a(:)
    type(sigio_head3a):: head3a
    integer(8):: iskip,iwrite,nwrite
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-2
    call sigio_adhead(head)
    if(head%ivs.ge.200509) then
      head1a%clab8='GFS SIG '
      head1a%ivs=head%ivs
      head1a%nhead=head%nhead
      head1a%ndata=head%ndata
      head1a%reserved=0
      iskip=0
      iwrite=head%lhead(1)
!LLF+PM--
      call byteswap(head1a%ivs,sigio_intkind,6)
!LLF+PM==
      call bafrwritel(lu,iskip,iwrite,nwrite,head1a)
      if(nwrite.lt.iwrite) return
!LLF+PM--
      call byteswap(head1a%ivs,sigio_intkind,6)
!LLF+PM==
      allocate(head2a(head%nhead+head%ndata))
      head2a(:head%nhead)=head%lhead
      head2a(head%nhead+1:)=head%ldata
      iskip=iskip+nwrite
      iwrite=head%lhead(2)
!LLF+PM--
      call byteswap(head2a,sigio_intkind,iwrite/sigio_intkind)
!LLF+PM==
      call bafrwritel(lu,iskip,iwrite,nwrite,head2a)
      deallocate(head2a)
      if(nwrite.lt.iwrite) return
      head3a%fhour=head%fhour
      head3a%idate=head%idate
      head3a%jcap=head%jcap
      head3a%levs=head%levs
      head3a%itrun=head%itrun
      head3a%iorder=head%iorder
      head3a%irealf=head%irealf
      head3a%igen=head%igen
      head3a%latf=head%latf
      head3a%lonf=head%lonf
      head3a%latb=head%latb
      head3a%lonb=head%lonb
      head3a%latr=head%latr
      head3a%lonr=head%lonr
      head3a%ntrac=head%ntrac
      head3a%nvcoord=head%nvcoord
      head3a%icen2=head%icen2
      head3a%iens=head%iens
      head3a%idpp=head%idpp
      head3a%idsl=head%idsl
      head3a%idvc=head%idvc
      head3a%idvm=head%idvm
      head3a%idvt=head%idvt
      head3a%idrun=head%idrun
      head3a%idusr=head%idusr
      head3a%pdryini=head%pdryini
      head3a%ncldt=head%ncldt
      head3a%ixgr=head%ixgr
      head3a%reserved=0
      iskip=iskip+nwrite
      iwrite=head%lhead(3)
!LLF+PM--
      call byteswap(head3a%fhour,sigio_realkind,1)
      call byteswap(head3a%idate,sigio_intkind,28)
      call byteswap(head3a%pdryini,sigio_realkind,1)
      call byteswap(head3a%ncldt,sigio_intkind,20)
!LLF+PM==
      call bafrwritel(lu,iskip,iwrite,nwrite,head3a)
      if(nwrite.lt.iwrite) return
!LLF+PM--
      call byteswap(head3a%fhour,sigio_realkind,1)
      call byteswap(head3a%idate,sigio_intkind,28)
      call byteswap(head3a%pdryini,sigio_realkind,1)
      call byteswap(head3a%ncldt,sigio_intkind,20)
!LLF+PM==
      iskip=iskip+nwrite
      iwrite=head%lhead(4)
!LLF+PM--
      call byteswap(head%vcoord,sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
      call bafrwritel(lu,iskip,iwrite,nwrite,head%vcoord)
      if(nwrite.lt.iwrite) return
!LLF+PM--
      call byteswap(head%vcoord,sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
      iskip=iskip+nwrite
      iwrite=head%lhead(5)
      call bafrwritel(lu,iskip,iwrite,nwrite,head%cfvars)
      if(nwrite.lt.iwrite) return
!
      if (mod(head%idvm/10,10) == 3) then
        iskip=iskip+nwrite
        iwrite=head%lhead(7)
        call byteswap(head%cpi,sigio_realkind,iwrite/sigio_realkind)
        call bafrwritel(lu,iskip,iwrite,nwrite,head%cpi)
        if(nwrite.lt.iwrite) return
        iskip=iskip+nwrite
        iwrite=head%lhead(7)
        call byteswap(head%ri,sigio_realkind,iwrite/sigio_realkind)
        call bafrwritel(lu,iskip,iwrite,nwrite,head%ri)
        if(nwrite.lt.iwrite) return
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    else
      iskip=0
      iwrite=sigio_lhead1
      call bafrwritel(lu,iskip,iwrite,nwrite,head%clabsig)
      if(nwrite.lt.iwrite) return
      head2%fhour=head%fhour 
      head2%idate=head%idate 
      head2%sisl=0
      if(head%idvc.eq.2) then
        if(head%nvcoord.eq.2.and.head%vcoord(1,2).eq.1.) then
          head2%sisl(1:head%levs+1)=head%vcoord(1:head%levs+1,1)
          head2%sisl(head%levs+2:2*head%levs+2)=head%vcoord(1:head%levs+1,2)
        else
          head2%sisl(1:head%levs+1)=head%ak(1:head%levs+1)
          head2%sisl(head%levs+2:2*head%levs+2)=head%bk(1:head%levs+1)
        endif
      elseif(head%idvc.eq.0.or.head%idvc.eq.1) then
        if(head%nvcoord.eq.1.and.head%vcoord(1,1).eq.1.) then
          head2%sisl(1:head%levs+1)=head%vcoord(1:head%levs+1,1)
          call sigio_modpr(1,1,head%levs,head%nvcoord,head%idvc,head%idsl,&
                           head%vcoord,iret,ps=(/1./),&
                           pm=head2%sisl(head%levs+2:2*head%levs+1))
        else
          head2%sisl(1:head%levs+1)=head%si(1:head%levs+1)
          head2%sisl(head%levs+2:2*head%levs+1)=head%sl(1:head%levs)
        endif
      endif
      head2%ext(1)=head%jcap
      head2%ext(2)=head%levs
      head2%ext(3)=head%itrun
      head2%ext(4)=head%iorder
      head2%ext(5)=head%irealf
      head2%ext(6)=head%igen
      head2%ext(7)=head%lonf
      head2%ext(8)=head%latf
      head2%ext(9)=head%lonb
      head2%ext(10)=head%latb
      head2%ext(11)=head%lonr
      head2%ext(12)=head%latr
      head2%ext(13)=head%ntrac
      head2%ext(14)=head%icen2
      head2%ext(15:16)=head%iens
      head2%ext(17)=head%idpp
      head2%ext(18)=head%idsl
      head2%ext(19)=head%idvc
      head2%ext(20)=head%idvm
      head2%ext(21)=head%idvt
      head2%ext(22)=head%idrun
      head2%ext(23)=head%idusr
      head2%ext(24)=head%pdryini
      head2%ext(25)=head%ncldt
      head2%ext(26:44)=0
      iskip=iskip+nwrite
      iwrite=1000
!LLF+PM--
      call byteswap(head2%fhour,sigio_realkind,1)
      call byteswap(head2%idate,sigio_intkind,4)
      call byteswap(head2%sisl,sigio_realkind,245)
!LLF+PM==
      call bafrwritel(lu,iskip,iwrite,nwrite,head2)
      if(nwrite.lt.iwrite) return
!LLF+PM--
      call byteswap(head2%fhour,sigio_realkind,1)
      call byteswap(head2%idate,sigio_intkind,4)
      call byteswap(head2%sisl,sigio_realkind,245)
!LLF+PM==
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=0
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_aldats(head,dats,iret)
    implicit none
    type(sigio_head),intent(in):: head
    type(sigio_dats),intent(inout):: dats
    integer(sigio_intkind),intent(out):: iret
    integer nc,dim1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!   call sigio_axdats(dats,iret)
    if(associated(dats%hs)) call sigio_axdats(dats,iret)
    nc=(head%jcap+1)*(head%jcap+2)
    dim1=nc
    allocate(dats%hs(dim1),dats%ps(dim1),stat=iret)
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_axdats(dats,iret)
    implicit none
    type(sigio_dats),intent(inout):: dats
    integer(sigio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    deallocate(dats%hs,dats%ps,stat=iret)
    nullify(dats%hs,dats%ps)
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_aldatm(head,k1,k2,datm,iret)
    implicit none
    type(sigio_head),intent(in):: head
    integer(sigio_intkind),intent(in):: k1,k2
    type(sigio_datm),intent(inout):: datm
    integer(sigio_intkind),intent(out):: iret
    integer nc,dim1,dim3q
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!   call sigio_axdatm(datm,iret)
    if(associated(datm%t)) call sigio_axdatm(datm,iret)
    iret=-3
    if(k1.lt.1.or.k2.gt.head%levs) return
    nc=(head%jcap+1)*(head%jcap+2)
    dim1=nc
    dim3q=head%ntrac
    datm%k1=k1
    datm%k2=k2
    allocate(datm%t(dim1,k1:k2),datm%d(dim1,k1:k2),datm%z(dim1,k1:k2),&
             datm%q(dim1,k1:k2,dim3q),stat=iret)
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_axdatm(datm,iret)
    implicit none
    type(sigio_datm),intent(inout):: datm
    integer(sigio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    datm%k1=0
    datm%k2=0
    deallocate(datm%t,datm%d,datm%z,datm%q,stat=iret)
    nullify(datm%t,datm%d,datm%z,datm%q)
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_aldati(head,i,dati,iret)
    implicit none
    type(sigio_head),intent(in):: head
    integer(sigio_intkind),intent(in):: i
    type(sigio_dati),intent(inout):: dati
    integer(sigio_intkind),intent(out):: iret
    integer dim1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!   call sigio_axdati(dati,iret)
    if(associated(dati%f)) call sigio_axdati(dati,iret)
    iret=-3
    if(i.lt.1.or.i.gt.head%ndata) return
    dim1=head%ldata(i)/(4*head%irealf)
    dati%i=i
    allocate(dati%f(dim1),stat=iret)
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_axdati(dati,iret)
    implicit none
    type(sigio_dati),intent(inout):: dati
    integer(sigio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dati%i=0
    deallocate(dati%f,stat=iret)
    nullify(dati%f)
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_rrdata(lu,head,data,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    type(sigio_head),intent(in):: head
    type(sigio_data),intent(inout):: data
    integer(sigio_intkind),intent(out):: iret
    integer:: i,k,n
    integer:: nc,mdim1,mdim2,mdim3q
    integer(8):: iskip,iread,nread
    type(sigio_dbta):: dbta
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    mdim1=min(size(data%hs,1),size(data%ps,1),&
              size(data%t,1),size(data%d,1),size(data%z,1),&
              size(data%q,1))
    mdim2=min(size(data%t,2),size(data%d,2),size(data%z,2),&
              size(data%q,2))
    mdim3q=size(data%q,3)
    nc=(head%jcap+1)*(head%jcap+2)
    iret=-5
    if(mdim1.lt.nc.or.&
       mdim2.lt.head%levs.or.&
       mdim3q.lt.head%ntrac) return
    iret=-4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%irealf.ne.2) then
      iskip=0
      do i=1,head%nhead
        call bafrindexl(0,iskip,int(head%lhead(i),8),iskip)
      enddo
      i=1
      iread=head%ldata(i)
      call bafrreadl(lu,iskip,iread,nread,data%hs)
      if(nread.lt.iread) return
!LLF+PM--
      call byteswap(data%hs,sigio_realkind,iread/sigio_realkind)
!LLF+PM==
      i=i+1
      iskip=iskip+nread
      iread=head%ldata(i)
      call bafrreadl(lu,iskip,iread,nread,data%ps)
      if(nread.lt.iread) return
!LLF+PM--
      call byteswap(data%ps,sigio_realkind,iread/sigio_realkind)
!LLF+PM==
      do k=1,head%levs
        i=i+1
        iskip=iskip+nread
        iread=head%ldata(i)
        call bafrreadl(lu,iskip,iread,nread,data%t(1,k))
        if(nread.lt.iread) return
!LLF+PM--
        call byteswap(data%t(1,k),sigio_realkind,iread/sigio_realkind)
!LLF+PM==
      enddo
      do k=1,head%levs
        i=i+1
        iskip=iskip+nread
        iread=head%ldata(i)
        call bafrreadl(lu,iskip,iread,nread,data%d(1,k))
        if(nread.lt.iread) return
!LLF+PM--
        call byteswap(data%d(1,k),sigio_realkind,iread/sigio_realkind)
!LLF+PM==
        i=i+1
        iskip=iskip+nread
        iread=head%ldata(i)
        call bafrreadl(lu,iskip,iread,nread,data%z(1,k))
        if(nread.lt.iread) return
!LLF+PM--
        call byteswap(data%z(1,k),sigio_realkind,iread/sigio_realkind)
!LLF+PM==
      enddo
      do n=1,head%ntrac
        do k=1,head%levs
          i=i+1
          iskip=iskip+nread
          iread=head%ldata(i)
          call bafrreadl(lu,iskip,iread,nread,data%q(1,k,n))
          if(nread.lt.iread) return
!LLF+PM--
        call byteswap(data%q(1,k,n),sigio_realkind,iread/sigio_realkind)
!LLF+PM==
        enddo
      enddo
      do n=1,head%nxgr
        i=i+1
        iskip=iskip+nread
        iread=head%ldata(i)
        call bafrreadl(lu,iskip,iread,nread,data%xgr(1,1,n))
        if(nread.lt.iread) return
!LLF+PM--
        call byteswap(data%xgr(1,1,n),sigio_realkind,iread/sigio_realkind)
!LLF+PM==
      enddo
      if(head%nxss.gt.0) then
        i=i+1
        iskip=iskip+nread
        iread=head%ldata(i)
        call bafrreadl(lu,iskip,iread,nread,data%xss)
        if(nread.lt.iread) return
!LLF+PM--
        call byteswap(data%xss,sigio_realkind,iread/sigio_realkind)
!LLF+PM==
      endif
    else
      call sigio_aldbta(head,dbta,iret)
      if(iret.ne.0) return
      call sigio_rrdbta(lu,head,dbta,iret)
      if(iret.ne.0) return
      data%hs(:nc)=dbta%hs(:nc)
      data%ps(:nc)=dbta%ps(:nc)
      data%t(:nc,:head%levs)=dbta%t(:nc,:head%levs)
      data%d(:nc,:head%levs)=dbta%d(:nc,:head%levs)
      data%z(:nc,:head%levs)=dbta%z(:nc,:head%levs)
      data%q(:nc,:head%levs,:head%ntrac)=dbta%q(:nc,:head%levs,:head%ntrac)
      data%xgr(:head%lonb,:head%latb,:head%nxgr)=&
       dbta%xgr(:head%lonb,:head%latb,:head%nxgr)
      data%xss(:head%nxss)=dbta%xss(:head%nxss)
      call sigio_axdbta(dbta,iret)
    endif
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_rwdata(lu,head,data,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    type(sigio_head),intent(in):: head
    type(sigio_data),intent(in):: data
    integer(sigio_intkind),intent(out):: iret
    integer:: i,k,n
    integer:: nc,mdim1,mdim2,mdim3q
    integer(8):: iskip,iwrite,nwrite
    type(sigio_dbta):: dbta
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    mdim1=min(size(data%hs,1),size(data%ps,1),&
              size(data%t,1),size(data%d,1),size(data%z,1),&
              size(data%q,1))
    mdim2=min(size(data%t,2),size(data%d,2),size(data%z,2),&
              size(data%q,2))
    mdim3q=size(data%q,3)
    nc=(head%jcap+1)*(head%jcap+2)
    iret=-5
    if(mdim1.lt.nc.or.&
       mdim2.lt.head%levs.or.&
       mdim3q.lt.head%ntrac) return
    iret=-4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%irealf.ne.2) then
      iskip=0
      do i=1,head%nhead
        call bafrindexl(0,iskip,int(head%lhead(i),8),iskip)
      enddo
      i=1
      iwrite=head%ldata(i)
!LLF+PM--
      call byteswap(data%hs,sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
      call bafrwritel(lu,iskip,iwrite,nwrite,data%hs)
      if(nwrite.lt.iwrite) return
!LLF+PM--
      call byteswap(data%hs,sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
      i=i+1
      iskip=iskip+nwrite
      iwrite=head%ldata(i)
!LLF+PM--
      call byteswap(data%ps,sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
      call bafrwritel(lu,iskip,iwrite,nwrite,data%ps)
      if(nwrite.lt.iwrite) return
!LLF+PM--
      call byteswap(data%ps,sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
      do k=1,head%levs
        i=i+1
        iskip=iskip+nwrite
        iwrite=head%ldata(i)
!LLF+PM--
        call byteswap(data%t(1,k),sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
        call bafrwritel(lu,iskip,iwrite,nwrite,data%t(1,k))
        if(nwrite.lt.iwrite) return
!LLF+PM--
        call byteswap(data%t(1,k),sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
      enddo
      do k=1,head%levs
        i=i+1
        iskip=iskip+nwrite
        iwrite=head%ldata(i)
!LLF+PM--
        call byteswap(data%d(1,k),sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
        call bafrwritel(lu,iskip,iwrite,nwrite,data%d(1,k))
        if(nwrite.lt.iwrite) return
!LLF+PM--
        call byteswap(data%d(1,k),sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
        i=i+1
        iskip=iskip+nwrite
        iwrite=head%ldata(i)
!LLF+PM--
        call byteswap(data%z(1,k),sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
        call bafrwritel(lu,iskip,iwrite,nwrite,data%z(1,k))
        if(nwrite.lt.iwrite) return
!LLF+PM--
        call byteswap(data%z(1,k),sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
      enddo
      do n=1,head%ntrac
        do k=1,head%levs
          i=i+1
          iskip=iskip+nwrite
          iwrite=head%ldata(i)
!LLF+PM--
          call byteswap(data%q(1,k,n),sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
          call bafrwritel(lu,iskip,iwrite,nwrite,data%q(1,k,n))
          if(nwrite.lt.iwrite) return
!LLF+PM--
          call byteswap(data%q(1,k,n),sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
        enddo
      enddo
      do n=1,head%nxgr
        i=i+1
        iskip=iskip+nwrite
        iwrite=head%ldata(i)
!LLF+PM--
        call byteswap(data%xgr(1,1,n),sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
        call bafrwritel(lu,iskip,iwrite,nwrite,data%xgr(1,1,n))
        if(nwrite.lt.iwrite) return
!LLF+PM--
        call byteswap(data%xgr(1,1,n),sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
      enddo
      if(head%nxss.gt.0) then
        i=i+1
        iskip=iskip+nwrite
        iwrite=head%ldata(i)
!LLF+PM--
        call byteswap(data%xss,sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
        call bafrwritel(lu,iskip,iwrite,nwrite,data%xss)
        if(nwrite.lt.iwrite) return
!LLF+PM--
        call byteswap(data%xss,sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
      endif
    else
      call sigio_aldbta(head,dbta,iret)
      if(iret.ne.0) return
      dbta%hs(:nc)=data%hs(:nc)
      dbta%ps(:nc)=data%ps(:nc)
      dbta%t(:nc,:head%levs)=data%t(:nc,:head%levs)
      dbta%d(:nc,:head%levs)=data%d(:nc,:head%levs)
      dbta%z(:nc,:head%levs)=data%z(:nc,:head%levs)
      dbta%q(:nc,:head%levs,:head%ntrac)=data%q(:nc,:head%levs,:head%ntrac)
      dbta%xgr(:head%lonb,:head%latb,:head%nxgr)=&
       data%xgr(:head%lonb,:head%latb,:head%nxgr)
      dbta%xss(:head%nxss)=data%xss(:head%nxss)
      call sigio_rwdbta(lu,head,dbta,iret)
      if(iret.ne.0) return
      call sigio_axdbta(dbta,iret)
    endif
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_rrohdca(lu,cfname,head,data,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    type(sigio_head),intent(inout):: head
    type(sigio_data),intent(inout):: data
    integer(sigio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_rropen(lu,cfname,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_rrhead(lu,head,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_aldata(head,data,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_rrdata(lu,head,data,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_rclose(lu,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_rwohdca(lu,cfname,head,data,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    type(sigio_head),intent(inout):: head
    type(sigio_data),intent(in):: data
    integer(sigio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_rwopen(lu,cfname,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_rwhead(lu,head,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_rwdata(lu,head,data,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_rclose(lu,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_rrdats(lu,head,dats,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    type(sigio_head),intent(in):: head
    type(sigio_dats),intent(inout):: dats
    integer(sigio_intkind),intent(out):: iret
    integer:: i
    integer:: nc,mdim1
    integer(8):: iskip,iread,nread
    type(sigio_dbts):: dbts
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    mdim1=min(size(dats%hs,1),size(dats%ps,1))
    nc=(head%jcap+1)*(head%jcap+2)
    iret=-5
    if(mdim1.lt.nc) return
    iret=-4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%irealf.ne.2) then
      iskip=0
      do i=1,head%nhead
        call bafrindexl(0,iskip,int(head%lhead(i),8),iskip)
      enddo
      i=1
      iread=head%ldata(i)
      call bafrreadl(lu,iskip,iread,nread,dats%hs)
      if(nread.lt.iread) return
!LLF+PM--
      call byteswap(dats%hs,sigio_realkind,iread/sigio_realkind)
!LLF+PM==
      i=i+1
      iskip=iskip+nread
      iread=head%ldata(i)
      call bafrreadl(lu,iskip,iread,nread,dats%ps)
      if(nread.lt.iread) return
!LLF+PM--
      call byteswap(dats%ps,sigio_realkind,iread/sigio_realkind)
!LLF+PM==
    else
      call sigio_aldbts(head,dbts,iret)
      if(iret.ne.0) return
      call sigio_rrdbts(lu,head,dbts,iret)
      if(iret.ne.0) return
      dats%hs(:nc)=dbts%hs(:nc)
      dats%ps(:nc)=dbts%ps(:nc)
      call sigio_axdbts(dbts,iret)
    endif
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_rwdats(lu,head,dats,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    type(sigio_head),intent(in):: head
    type(sigio_dats),intent(in):: dats
    integer(sigio_intkind),intent(out):: iret
    integer:: i
    integer:: nc,mdim1
    integer(8):: iskip,iwrite,nwrite
    type(sigio_dbts):: dbts
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    mdim1=min(size(dats%hs,1),size(dats%ps,1))
    nc=(head%jcap+1)*(head%jcap+2)
    iret=-5
    if(mdim1.lt.nc) return
    iret=-4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%irealf.ne.2) then
      iskip=0
      do i=1,head%nhead
        call bafrindexl(0,iskip,int(head%lhead(i),8),iskip)
      enddo
      i=1
      iwrite=head%ldata(i)
!LLF+PM--
      call byteswap(dats%hs,sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
      call bafrwritel(lu,iskip,iwrite,nwrite,dats%hs)
      if(nwrite.lt.iwrite) return
!LLF+PM--
      call byteswap(dats%hs,sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
      i=i+1
      iskip=iskip+nwrite
      iwrite=head%ldata(i)
!LLF+PM--
      call byteswap(dats%ps,sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
      call bafrwritel(lu,iskip,iwrite,nwrite,dats%ps)
      if(nwrite.lt.iwrite) return
!LLF+PM--
      call byteswap(dats%ps,sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
    else
      call sigio_aldbts(head,dbts,iret)
      if(iret.ne.0) return
      dbts%hs(:nc)=dats%hs(:nc)
      dbts%ps(:nc)=dats%ps(:nc)
      call sigio_rwdbts(lu,head,dbts,iret)
      if(iret.ne.0) return
      call sigio_axdbts(dbts,iret)
    endif
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_rrdatm(lu,head,datm,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    type(sigio_head),intent(in):: head
    type(sigio_datm),intent(inout):: datm
    integer(sigio_intkind),intent(out):: iret
    integer:: i,k,n
    integer:: nc,k1,k2,mdim1,ldim2,udim2,mdim3q
    integer(8):: iskip,iread,nread
    type(sigio_dbtm):: dbtm
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    k1=datm%k1
    k2=datm%k2
    mdim1=min(size(datm%t,1),size(datm%d,1),size(datm%z,1),&
              size(datm%q,1))
    ldim2=max(lbound(datm%t,2),lbound(datm%d,2),lbound(datm%z,2),&
              lbound(datm%q,2))
    udim2=min(ubound(datm%t,2),ubound(datm%d,2),ubound(datm%z,2),&
              ubound(datm%q,2))
    mdim3q=size(datm%q,3)
    nc=(head%jcap+1)*(head%jcap+2)
    iret=-5
    if(k1.lt.1.or.k2.gt.head%levs.or.&
       mdim1.lt.nc.or.&
       ldim2.gt.k1.or.udim2.lt.k2.or.&
       mdim3q.lt.head%ntrac) return
    iret=-4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%irealf.ne.2) then
      iskip=0
      do i=1,head%nhead
        call bafrindexl(0,iskip,int(head%lhead(i),8),iskip)
      enddo
      i=1
      call bafrindexl(0,iskip,int(head%ldata(i),8),iskip)
      i=i+1
      call bafrindexl(0,iskip,int(head%ldata(i),8),iskip)
      do k=1,head%levs
        if(k.lt.k1.or.k.gt.k2) then
          i=i+1
          call bafrindexl(0,iskip,int(head%ldata(i),8),iskip)
        else
          i=i+1
          iread=head%ldata(i)
          call bafrreadl(lu,iskip,iread,nread,datm%t(1,k))
          if(nread.lt.iread) return
!LLF+PM--
          call byteswap(datm%t(1,k),sigio_realkind,iread/sigio_realkind)
!LLF+PM==
          iskip=iskip+nread
        endif
      enddo
      do k=1,head%levs
        if(k.lt.k1.or.k.gt.k2) then
          i=i+1
          call bafrindexl(0,iskip,int(head%ldata(i),8),iskip)
          i=i+1
          call bafrindexl(0,iskip,int(head%ldata(i),8),iskip)
        else
          iread=head%ldata(i)
          call bafrreadl(lu,iskip,iread,nread,datm%d(1,k))
          if(nread.lt.iread) return
!LLF+PM--
          call byteswap(datm%d(1,k),sigio_realkind,iread/sigio_realkind)
!LLF+PM==
          iskip=iskip+nread
          iread=head%ldata(i)
          call bafrreadl(lu,iskip,iread,nread,datm%z(1,k))
          if(nread.lt.iread) return
!LLF+PM--
          call byteswap(datm%z(1,k),sigio_realkind,iread/sigio_realkind)
!LLF+PM==
          iskip=iskip+nread
        endif
      enddo
      do n=1,head%ntrac
        do k=1,head%levs
          if(k.lt.k1.or.k.gt.k2) then
            i=i+1
            call bafrindexl(0,iskip,int(head%ldata(i),8),iskip)
          else
            i=i+1
            iread=head%ldata(i)
            call bafrreadl(lu,iskip,iread,nread,datm%q(1,k,n))
            if(nread.lt.iread) return
!LLF+PM--
            call byteswap(datm%q(1,k,n),sigio_realkind,iread/sigio_realkind)
!LLF+PM==
            iskip=iskip+nread
          endif
        enddo
      enddo
    else
      call sigio_aldbtm(head,k1,k2,dbtm,iret)
      if(iret.ne.0) return
      call sigio_rrdbtm(lu,head,dbtm,iret)
      if(iret.ne.0) return
      datm%t(:nc,k1:k2)=dbtm%t(:nc,k1:k2)
      datm%d(:nc,k1:k2)=dbtm%d(:nc,k1:k2)
      datm%z(:nc,k1:k2)=dbtm%z(:nc,k1:k2)
      datm%q(:nc,k1:k2,:head%ntrac)=dbtm%q(:nc,k1:k2,:head%ntrac)
      call sigio_axdbtm(dbtm,iret)
    endif
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_rwdatm(lu,head,datm,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    type(sigio_head),intent(in):: head
    type(sigio_datm),intent(in):: datm
    integer(sigio_intkind),intent(out):: iret
    integer:: i,k,n
    integer:: nc,k1,k2,mdim1,ldim2,udim2,mdim3q
    integer(8):: iskip,iwrite,nwrite
    type(sigio_dbtm):: dbtm
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    k1=datm%k1
    k2=datm%k2
    mdim1=min(size(datm%t,1),size(datm%d,1),size(datm%z,1),&
              size(datm%q,1))
    ldim2=max(lbound(datm%t,2),lbound(datm%d,2),lbound(datm%z,2),&
              lbound(datm%q,2))
    udim2=min(ubound(datm%t,2),ubound(datm%d,2),ubound(datm%z,2),&
              ubound(datm%q,2))
    mdim3q=size(datm%q,3)
    nc=(head%jcap+1)*(head%jcap+2)
    iret=-5
    if(k1.lt.1.or.k2.gt.head%levs.or.&
       mdim1.lt.nc.or.&
       ldim2.gt.k1.or.udim2.lt.k2.or.&
       mdim3q.lt.head%ntrac) return
    iret=-4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%irealf.ne.2) then
      iskip=0
      do i=1,head%nhead
        call bafrindexl(0,iskip,int(head%lhead(i),8),iskip)
      enddo
      i=1
      call bafrindexl(0,iskip,int(head%ldata(i),8),iskip)
      i=i+1
      call bafrindexl(0,iskip,int(head%ldata(i),8),iskip)
      do k=1,head%levs
        if(k.lt.k1.or.k.gt.k2) then
          i=i+1
          call bafrindexl(0,iskip,int(head%ldata(i),8),iskip)
        else
          i=i+1
          iwrite=head%ldata(i)
!LLF+PM--
          call byteswap(datm%t(1,k),sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
          call bafrwritel(lu,iskip,iwrite,nwrite,datm%t(1,k))
          if(nwrite.lt.iwrite) return
!LLF+PM--
          call byteswap(datm%t(1,k),sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
          iskip=iskip+nwrite
        endif
      enddo
      do k=1,head%levs
        if(k.lt.k1.or.k.gt.k2) then
          i=i+1
          call bafrindexl(0,iskip,int(head%ldata(i),8),iskip)
          i=i+1
          call bafrindexl(0,iskip,int(head%ldata(i),8),iskip)
        else
          iwrite=head%ldata(i)
!LLF+PM--
          call byteswap(datm%d(1,k),sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
          call bafrwritel(lu,iskip,iwrite,nwrite,datm%d(1,k))
          if(nwrite.lt.iwrite) return
!LLF+PM--
          call byteswap(datm%d(1,k),sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
          iskip=iskip+nwrite
          iwrite=head%ldata(i)
!LLF+PM--
          call byteswap(datm%z(1,k),sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
          call bafrwritel(lu,iskip,iwrite,nwrite,datm%z(1,k))
          if(nwrite.lt.iwrite) return
!LLF+PM--
          call byteswap(datm%z(1,k),sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
          iskip=iskip+nwrite
        endif
      enddo
      do n=1,head%ntrac
        do k=1,head%levs
          if(k.lt.k1.or.k.gt.k2) then
            i=i+1
            call bafrindexl(0,iskip,int(head%ldata(i),8),iskip)
          else
            i=i+1
            iwrite=head%ldata(i)
!LLF+PM--
            call byteswap(datm%q(1,k,n),sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
            call bafrwritel(lu,iskip,iwrite,nwrite,datm%q(1,k,n))
            if(nwrite.lt.iwrite) return
!LLF+PM--
            call byteswap(datm%q(1,k,n),sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
            iskip=iskip+nwrite
          endif
        enddo
      enddo
    else
      call sigio_aldbtm(head,k1,k2,dbtm,iret)
      if(iret.ne.0) return
      dbtm%t(:nc,k1:k2)=datm%t(:nc,k1:k2)
      dbtm%d(:nc,k1:k2)=datm%d(:nc,k1:k2)
      dbtm%z(:nc,k1:k2)=datm%z(:nc,k1:k2)
      dbtm%q(:nc,k1:k2,:head%ntrac)=datm%q(:nc,k1:k2,:head%ntrac)
      call sigio_rwdbtm(lu,head,dbtm,iret)
      if(iret.ne.0) return
      call sigio_axdbtm(dbtm,iret)
    endif
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_rrdati(lu,head,dati,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    type(sigio_head),intent(in):: head
    type(sigio_dati),intent(inout):: dati
    integer(sigio_intkind),intent(out):: iret
    integer:: i,k,n
    integer:: mdim1
    integer:: mlen
    integer(8):: iskip,iread,nread
    type(sigio_dbti):: dbti
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    i=dati%i
    mdim1=size(dati%f,1)
    iret=-5
    if(i.lt.1.or.i.gt.head%ndata) return
    mlen=head%ldata(i)/(4*head%irealf)
    if(mdim1.lt.mlen) return
    iret=-4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%irealf.ne.2) then
      iskip=0
      do i=1,head%nhead
        call bafrindexl(0,iskip,int(head%lhead(i),8),iskip)
      enddo
      do i=1,dati%i-1
        call bafrindexl(0,iskip,int(head%ldata(i),8),iskip)
      enddo
      i=dati%i
      iread=head%ldata(i)
      call bafrreadl(lu,iskip,iread,nread,dati%f)
      if(nread.lt.iread) return
!LLF+PM--
      call byteswap(dati%f,sigio_realkind,iread/sigio_realkind)
!LLF+PM==
    else
      i=dati%i
      call sigio_aldbti(head,i,dbti,iret)
      if(iret.ne.0) return
      call sigio_rrdbti(lu,head,dbti,iret)
      if(iret.ne.0) return
      dati%f(:mlen)=dbti%f(:mlen)
      call sigio_axdbti(dbti,iret)
    endif
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_rwdati(lu,head,dati,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    type(sigio_head),intent(in):: head
    type(sigio_dati),intent(in):: dati
    integer(sigio_intkind),intent(out):: iret
    integer:: i,k,n
    integer:: mdim1
    integer:: mlen
    integer(8):: iskip,iwrite,nwrite
    type(sigio_dbti):: dbti
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    i=dati%i
    mdim1=size(dati%f,1)
    iret=-5
    if(i.lt.1.or.i.gt.head%ndata) return
    mlen=head%ldata(i)/(4*head%irealf)
    if(mdim1.lt.mlen) return
    iret=-4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%irealf.ne.2) then
      iskip=0
      do i=1,head%nhead
        call bafrindexl(0,iskip,int(head%lhead(i),8),iskip)
      enddo
      do i=1,dati%i-1
        call bafrindexl(0,iskip,int(head%ldata(i),8),iskip)
      enddo
      i=dati%i
      iwrite=head%ldata(i)
!LLF+PM--
      call byteswap(dati%f,sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
      call bafrwritel(lu,iskip,iwrite,nwrite,dati%f)
      if(nwrite.lt.iwrite) return
!LLF+PM--
      call byteswap(dati%f,sigio_realkind,iwrite/sigio_realkind)
!LLF+PM==
      iret=0
    else
      i=dati%i
      call sigio_aldbti(head,i,dbti,iret)
      if(iret.ne.0) return
      dbti%f(:mlen)=dati%f(:mlen)
      call sigio_rwdbti(lu,head,dbti,iret)
      if(iret.ne.0) return
      call sigio_axdbti(dbti,iret)
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_aldbts(head,dbts,iret)
    implicit none
    type(sigio_head),intent(in):: head
    type(sigio_dbts),intent(inout):: dbts
    integer(sigio_intkind),intent(out):: iret
    integer nc,dim1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!   call sigio_axdbts(dbts,iret)
    if(associated(dbts%hs)) call sigio_axdbts(dbts,iret)
    nc=(head%jcap+1)*(head%jcap+2)
    dim1=nc
    allocate(dbts%hs(dim1),dbts%ps(dim1),stat=iret)
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_axdbts(dbts,iret)
    implicit none
    type(sigio_dbts),intent(inout):: dbts
    integer(sigio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    deallocate(dbts%hs,dbts%ps,stat=iret)
    nullify(dbts%hs,dbts%ps)
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_aldbtm(head,k1,k2,dbtm,iret)
    implicit none
    type(sigio_head),intent(in):: head
    integer(sigio_intkind),intent(in):: k1,k2
    type(sigio_dbtm),intent(inout):: dbtm
    integer(sigio_intkind),intent(out):: iret
    integer nc,dim1,dim3q
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!   call sigio_axdbtm(dbtm,iret)
    if(associated(dbtm%t)) call sigio_axdbtm(dbtm,iret)
    iret=-3
    if(k1.lt.1.or.k2.gt.head%levs) return
    nc=(head%jcap+1)*(head%jcap+2)
    dim1=nc
    dim3q=head%ntrac
    dbtm%k1=k1
    dbtm%k2=k2
    allocate(dbtm%t(dim1,k1:k2),dbtm%d(dim1,k1:k2),dbtm%z(dim1,k1:k2),&
             dbtm%q(dim1,k1:k2,dim3q),stat=iret)
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_axdbtm(dbtm,iret)
    implicit none
    type(sigio_dbtm),intent(inout):: dbtm
    integer(sigio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dbtm%k1=0
    dbtm%k2=0
    deallocate(dbtm%t,dbtm%d,dbtm%z,dbtm%q,stat=iret)
    nullify(dbtm%t,dbtm%d,dbtm%z,dbtm%q)
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_aldbti(head,i,dbti,iret)
    implicit none
    type(sigio_head),intent(in):: head
    integer(sigio_intkind),intent(in):: i
    type(sigio_dbti),intent(inout):: dbti
    integer(sigio_intkind),intent(out):: iret
    integer dim1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!   call sigio_axdbti(dbti,iret)
    if(associated(dbti%f)) call sigio_axdbti(dbti,iret)
    iret=-3
    if(i.lt.1.or.i.gt.head%ndata) return
    dim1=head%ldata(i)/(4*head%irealf)
    dbti%i=i
    allocate(dbti%f(dim1),stat=iret)
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_axdbti(dbti,iret)
    implicit none
    type(sigio_dbti),intent(inout):: dbti
    integer(sigio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dbti%i=0
    deallocate(dbti%f,stat=iret)
    nullify(dbti%f)
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_rrdbta(lu,head,dbta,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    type(sigio_head),intent(in):: head
    type(sigio_dbta),intent(inout):: dbta
    integer(sigio_intkind),intent(out):: iret
    integer:: i,k,n
    integer:: nc,mdim1,mdim2,mdim3q
    integer(8):: iskip,iread,nread
    type(sigio_data):: data
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    mdim1=min(size(dbta%hs,1),size(dbta%ps,1),&
              size(dbta%t,1),size(dbta%d,1),size(dbta%z,1),&
              size(dbta%q,1))
    mdim2=min(size(dbta%t,2),size(dbta%d,2),size(dbta%z,2),&
              size(dbta%q,2))
    mdim3q=size(dbta%q,3)
    nc=(head%jcap+1)*(head%jcap+2)
    iret=-5
    if(mdim1.lt.nc.or.&
       mdim2.lt.head%levs.or.&
       mdim3q.lt.head%ntrac) return
    iret=-4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%irealf.eq.2) then
      iskip=0
      do i=1,head%nhead
        call bafrindexl(0,iskip,int(head%lhead(i),8),iskip)
      enddo
      i=1
      iread=head%ldata(i)
      call bafrreadl(lu,iskip,iread,nread,dbta%hs)
      if(nread.lt.iread) return
!LLF+PM--
      call byteswap(dbta%hs,sigio_dblekind,iread/sigio_dblekind)
!LLF+PM==
      i=i+1
      iskip=iskip+nread
      iread=head%ldata(i)
      call bafrreadl(lu,iskip,iread,nread,dbta%ps)
      if(nread.lt.iread) return
!LLF+PM--
      call byteswap(dbta%ps,sigio_dblekind,iread/sigio_dblekind)
!LLF+PM==
      do k=1,head%levs
        i=i+1
        iskip=iskip+nread
        iread=head%ldata(i)
        call bafrreadl(lu,iskip,iread,nread,dbta%t(1,k))
        if(nread.lt.iread) return
!LLF+PM--
        call byteswap(dbta%t(1,k),sigio_dblekind,iread/sigio_dblekind)
!LLF+PM==
      enddo
      do k=1,head%levs
        i=i+1
        iskip=iskip+nread
        iread=head%ldata(i)
        call bafrreadl(lu,iskip,iread,nread,dbta%d(1,k))
        if(nread.lt.iread) return
!LLF+PM--
        call byteswap(dbta%d(1,k),sigio_dblekind,iread/sigio_dblekind)
!LLF+PM==
        i=i+1
        iskip=iskip+nread
        iread=head%ldata(i)
        call bafrreadl(lu,iskip,iread,nread,dbta%z(1,k))
        if(nread.lt.iread) return
!LLF+PM--
        call byteswap(dbta%z(1,k),sigio_dblekind,iread/sigio_dblekind)
!LLF+PM==
      enddo
      do n=1,head%ntrac
        do k=1,head%levs
          i=i+1
          iskip=iskip+nread
          iread=head%ldata(i)
          call bafrreadl(lu,iskip,iread,nread,dbta%q(1,k,n))
          if(nread.lt.iread) return
!LLF+PM--
          call byteswap(dbta%q(1,k,n),sigio_dblekind,iread/sigio_dblekind)
!LLF+PM==
        enddo
      enddo
      do n=1,head%nxgr
        i=i+1
        iskip=iskip+nread
        iread=head%ldata(i)
        call bafrreadl(lu,iskip,iread,nread,dbta%xgr(1,1,n))
        if(nread.lt.iread) return
!LLF+PM--
        call byteswap(dbta%xgr(1,1,n),sigio_dblekind,iread/sigio_dblekind)
!LLF+PM==
      enddo
      if(head%nxss.gt.0) then
        i=i+1
        iskip=iskip+nread
        iread=head%ldata(i)
        call bafrreadl(lu,iskip,iread,nread,dbta%xss)
        if(nread.lt.iread) return
!LLF+PM--
        call byteswap(dbta%xss,sigio_dblekind,iread/sigio_dblekind)
!LLF+PM==
      endif
    else
      call sigio_aldata(head,data,iret)
      if(iret.ne.0) return
      call sigio_rrdata(lu,head,data,iret)
      if(iret.ne.0) return
      dbta%hs(:nc)=data%hs(:nc)
      dbta%ps(:nc)=data%ps(:nc)
      dbta%t(:nc,:head%levs)=data%t(:nc,:head%levs)
      dbta%d(:nc,:head%levs)=data%d(:nc,:head%levs)
      dbta%z(:nc,:head%levs)=data%z(:nc,:head%levs)
      dbta%q(:nc,:head%levs,:head%ntrac)=data%q(:nc,:head%levs,:head%ntrac)
      dbta%xgr(:head%lonb,:head%latb,:head%nxgr)=&
       data%xgr(:head%lonb,:head%latb,:head%nxgr)
      dbta%xss(:head%nxss)=data%xss(:head%nxss)
      call sigio_axdata(data,iret)
    endif
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_rwdbta(lu,head,dbta,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    type(sigio_head),intent(in):: head
    type(sigio_dbta),intent(in):: dbta
    integer(sigio_intkind),intent(out):: iret
    integer:: i,k,n
    integer:: nc,mdim1,mdim2,mdim3q
    integer(8):: iskip,iwrite,nwrite
    type(sigio_data):: data
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    mdim1=min(size(dbta%hs,1),size(dbta%ps,1),&
              size(dbta%t,1),size(dbta%d,1),size(dbta%z,1),&
              size(dbta%q,1))
    mdim2=min(size(dbta%t,2),size(dbta%d,2),size(dbta%z,2),&
              size(dbta%q,2))
    mdim3q=size(dbta%q,3)
    nc=(head%jcap+1)*(head%jcap+2)
    iret=-5
    if(mdim1.lt.nc.or.&
       mdim2.lt.head%levs.or.&
       mdim3q.lt.head%ntrac) return
    iret=-4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%irealf.eq.2) then
      iskip=0
      do i=1,head%nhead
        call bafrindexl(0,iskip,int(head%lhead(i),8),iskip)
      enddo
      i=1
      iwrite=head%ldata(i)
!LLF+PM--
      call byteswap(dbta%hs,sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
      call bafrwritel(lu,iskip,iwrite,nwrite,dbta%hs)
      if(nwrite.lt.iwrite) return
!LLF+PM--
      call byteswap(dbta%hs,sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
      i=i+1
      iskip=iskip+nwrite
      iwrite=head%ldata(i)
!LLF+PM--
      call byteswap(dbta%ps,sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
      call bafrwritel(lu,iskip,iwrite,nwrite,dbta%ps)
      if(nwrite.lt.iwrite) return
!LLF+PM--
      call byteswap(dbta%ps,sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
      do k=1,head%levs
        i=i+1
        iskip=iskip+nwrite
        iwrite=head%ldata(i)
!LLF+PM--
        call byteswap(dbta%t(1,k),sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
        call bafrwritel(lu,iskip,iwrite,nwrite,dbta%t(1,k))
        if(nwrite.lt.iwrite) return
!LLF+PM--
        call byteswap(dbta%t(1,k),sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
      enddo
      do k=1,head%levs
        i=i+1
        iskip=iskip+nwrite
        iwrite=head%ldata(i)
!LLF+PM--
        call byteswap(dbta%d(1,k),sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
        call bafrwritel(lu,iskip,iwrite,nwrite,dbta%d(1,k))
        if(nwrite.lt.iwrite) return
!LLF+PM--
        call byteswap(dbta%d(1,k),sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
        i=i+1
        iskip=iskip+nwrite
        iwrite=head%ldata(i)
!LLF+PM--
        call byteswap(dbta%z(1,k),sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
        call bafrwritel(lu,iskip,iwrite,nwrite,dbta%z(1,k))
        if(nwrite.lt.iwrite) return
!LLF+PM--
        call byteswap(dbta%z(1,k),sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
      enddo
      do n=1,head%ntrac
        do k=1,head%levs
          i=i+1
          iskip=iskip+nwrite
          iwrite=head%ldata(i)
!LLF+PM--
          call byteswap(dbta%q(1,k,n),sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
          call bafrwritel(lu,iskip,iwrite,nwrite,dbta%q(1,k,n))
          if(nwrite.lt.iwrite) return
!LLF+PM--
          call byteswap(dbta%q(1,k,n),sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
        enddo
      enddo
      do n=1,head%nxgr
        i=i+1
        iskip=iskip+nwrite
        iwrite=head%ldata(i)
!LLF+PM--
        call byteswap(dbta%xgr(1,1,n),sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
        call bafrwritel(lu,iskip,iwrite,nwrite,dbta%xgr(1,1,n))
        if(nwrite.lt.iwrite) return
!LLF+PM--
        call byteswap(dbta%xgr(1,1,n),sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
      enddo
      if(head%nxss.gt.0) then
        i=i+1
        iskip=iskip+nwrite
        iwrite=head%ldata(i)
!LLF+PM--
        call byteswap(dbta%xss,sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
        call bafrwritel(lu,iskip,iwrite,nwrite,dbta%xss)
        if(nwrite.lt.iwrite) return
!LLF+PM--
        call byteswap(dbta%xss,sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
      endif
    else
      call sigio_aldata(head,data,iret)
      if(iret.ne.0) return
      data%hs(:nc)=dbta%hs(:nc)
      data%ps(:nc)=dbta%ps(:nc)
      data%t(:nc,:head%levs)=dbta%t(:nc,:head%levs)
      data%d(:nc,:head%levs)=dbta%d(:nc,:head%levs)
      data%z(:nc,:head%levs)=dbta%z(:nc,:head%levs)
      data%q(:nc,:head%levs,:head%ntrac)=dbta%q(:nc,:head%levs,:head%ntrac)
      data%xgr(:head%lonb,:head%latb,:head%nxgr)=&
       dbta%xgr(:head%lonb,:head%latb,:head%nxgr)
      data%xss(:head%nxss)=dbta%xss(:head%nxss)
      call sigio_rwdata(lu,head,data,iret)
      if(iret.ne.0) return
      call sigio_axdata(data,iret)
    endif
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_rrohdcb(lu,cfname,head,dbta,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    type(sigio_head),intent(inout):: head
    type(sigio_dbta),intent(inout):: dbta
    integer(sigio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_rropen(lu,cfname,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_rrhead(lu,head,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_aldbta(head,dbta,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_rrdbta(lu,head,dbta,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_rclose(lu,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_rwohdcb(lu,cfname,head,dbta,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    type(sigio_head),intent(inout):: head
    type(sigio_dbta),intent(in):: dbta
    integer(sigio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_rwopen(lu,cfname,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_rwhead(lu,head,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_rwdbta(lu,head,dbta,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_rclose(lu,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_rrdbts(lu,head,dbts,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    type(sigio_head),intent(in):: head
    type(sigio_dbts),intent(inout):: dbts
    integer(sigio_intkind),intent(out):: iret
    integer:: i
    integer:: nc,mdim1
    integer(8):: iskip,iread,nread
    type(sigio_dats):: dats
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    mdim1=min(size(dbts%hs,1),size(dbts%ps,1))
    nc=(head%jcap+1)*(head%jcap+2)
    iret=-5
    if(mdim1.lt.nc) return
    iret=-4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%irealf.eq.2) then
      iskip=0
      do i=1,head%nhead
        call bafrindexl(0,iskip,int(head%lhead(i),8),iskip)
      enddo
      i=1
      iread=head%ldata(i)
      call bafrreadl(lu,iskip,iread,nread,dbts%hs)
      if(nread.lt.iread) return
!LLF+PM--
      call byteswap(dbts%hs,sigio_dblekind,iread/sigio_dblekind)
!LLF+PM==
      i=i+1
      iskip=iskip+nread
      iread=head%ldata(i)
      call bafrreadl(lu,iskip,iread,nread,dbts%ps)
      if(nread.lt.iread) return
!LLF+PM--
      call byteswap(dbts%ps,sigio_dblekind,iread/sigio_dblekind)
!LLF+PM==
    else
      call sigio_aldats(head,dats,iret)
      if(iret.ne.0) return
      call sigio_rrdats(lu,head,dats,iret)
      if(iret.ne.0) return
      dbts%hs(:nc)=dats%hs(:nc)
      dbts%ps(:nc)=dats%ps(:nc)
      call sigio_axdats(dats,iret)
    endif
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_rwdbts(lu,head,dbts,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    type(sigio_head),intent(in):: head
    type(sigio_dbts),intent(in):: dbts
    integer(sigio_intkind),intent(out):: iret
    integer:: i
    integer:: nc,mdim1
    integer(8):: iskip,iwrite,nwrite
    type(sigio_dats):: dats
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    mdim1=min(size(dbts%hs,1),size(dbts%ps,1))
    nc=(head%jcap+1)*(head%jcap+2)
    iret=-5
    if(mdim1.lt.nc) return
    iret=-4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%irealf.eq.2) then
      iskip=0
      do i=1,head%nhead
        call bafrindexl(0,iskip,int(head%lhead(i),8),iskip)
      enddo
      i=1
      iwrite=head%ldata(i)
!LLF+PM--
      call byteswap(dbts%hs,sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
      call bafrwritel(lu,iskip,iwrite,nwrite,dbts%hs)
      if(nwrite.lt.iwrite) return
!LLF+PM--
      call byteswap(dbts%hs,sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
      i=i+1
      iskip=iskip+nwrite
      iwrite=head%ldata(i)
!LLF+PM--
      call byteswap(dbts%ps,sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
      call bafrwritel(lu,iskip,iwrite,nwrite,dbts%ps)
      if(nwrite.lt.iwrite) return
!LLF+PM--
      call byteswap(dbts%ps,sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
    else
      call sigio_aldats(head,dats,iret)
      if(iret.ne.0) return
      dats%hs(:nc)=dbts%hs(:nc)
      dats%ps(:nc)=dbts%ps(:nc)
      call sigio_rwdats(lu,head,dats,iret)
      if(iret.ne.0) return
      call sigio_axdats(dats,iret)
    endif
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_rrdbtm(lu,head,dbtm,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    type(sigio_head),intent(in):: head
    type(sigio_dbtm),intent(inout):: dbtm
    integer(sigio_intkind),intent(out):: iret
    integer:: i,k,n
    integer:: nc,k1,k2,mdim1,ldim2,udim2,mdim3q
    integer(8):: iskip,iread,nread
    type(sigio_datm):: datm
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    k1=dbtm%k1
    k2=dbtm%k2
    mdim1=min(size(dbtm%t,1),size(dbtm%d,1),size(dbtm%z,1),&
              size(dbtm%q,1))
    ldim2=max(lbound(dbtm%t,2),lbound(dbtm%d,2),lbound(dbtm%z,2),&
              lbound(dbtm%q,2))
    udim2=min(ubound(dbtm%t,2),ubound(dbtm%d,2),ubound(dbtm%z,2),&
              ubound(dbtm%q,2))
    mdim3q=size(dbtm%q,3)
    nc=(head%jcap+1)*(head%jcap+2)
    iret=-5
    if(k1.lt.1.or.k2.gt.head%levs.or.&
       mdim1.lt.nc.or.&
       ldim2.gt.k1.or.udim2.lt.k2.or.&
       mdim3q.lt.head%ntrac) return
    iret=-4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%irealf.eq.2) then
      iskip=0
      do i=1,head%nhead
        call bafrindexl(0,iskip,int(head%lhead(i),8),iskip)
      enddo
      i=1
      call bafrindexl(0,iskip,int(head%ldata(i),8),iskip)
      i=i+1
      call bafrindexl(0,iskip,int(head%ldata(i),8),iskip)
      do k=1,head%levs
        if(k.lt.k1.or.k.gt.k2) then
          i=i+1
          call bafrindexl(0,iskip,int(head%ldata(i),8),iskip)
        else
          i=i+1
          iread=head%ldata(i)
          call bafrreadl(lu,iskip,iread,nread,dbtm%t(1,k))
          if(nread.lt.iread) return
!LLF+PM--
          call byteswap(dbtm%t(1,k),sigio_dblekind,iread/sigio_dblekind)
!LLF+PM==
          iskip=iskip+nread
        endif
      enddo
      do k=1,head%levs
        if(k.lt.k1.or.k.gt.k2) then
          i=i+1
          call bafrindexl(0,iskip,int(head%ldata(i),8),iskip)
          i=i+1
          call bafrindexl(0,iskip,int(head%ldata(i),8),iskip)
        else
          iread=head%ldata(i)
          call bafrreadl(lu,iskip,iread,nread,dbtm%d(1,k))
          if(nread.lt.iread) return
!LLF+PM--
          call byteswap(dbtm%d(1,k),sigio_dblekind,iread/sigio_dblekind)
!LLF+PM==
          iskip=iskip+nread
          iread=head%ldata(i)
          call bafrreadl(lu,iskip,iread,nread,dbtm%z(1,k))
          if(nread.lt.iread) return
!LLF+PM--
          call byteswap(dbtm%z(1,k),sigio_dblekind,iread/sigio_dblekind)
!LLF+PM==
          iskip=iskip+nread
        endif
      enddo
      do n=1,head%ntrac
        do k=1,head%levs
          if(k.lt.k1.or.k.gt.k2) then
            i=i+1
            call bafrindexl(0,iskip,int(head%ldata(i),8),iskip)
          else
            i=i+1
            iread=head%ldata(i)
            call bafrreadl(lu,iskip,iread,nread,dbtm%q(1,k,n))
            if(nread.lt.iread) return
!LLF+PM--
            call byteswap(dbtm%q(1,k,n),sigio_dblekind,iread/sigio_dblekind)
!LLF+PM==
            iskip=iskip+nread
          endif
        enddo
      enddo
    else
      call sigio_aldatm(head,k1,k2,datm,iret)
      if(iret.ne.0) return
      call sigio_rrdatm(lu,head,datm,iret)
      if(iret.ne.0) return
      dbtm%t(:nc,k1:k2)=datm%t(:nc,k1:k2)
      dbtm%d(:nc,k1:k2)=datm%d(:nc,k1:k2)
      dbtm%z(:nc,k1:k2)=datm%z(:nc,k1:k2)
      dbtm%q(:nc,k1:k2,:head%ntrac)=datm%q(:nc,k1:k2,:head%ntrac)
      call sigio_axdatm(datm,iret)
    endif
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_rwdbtm(lu,head,dbtm,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    type(sigio_head),intent(in):: head
    type(sigio_dbtm),intent(in):: dbtm
    integer(sigio_intkind),intent(out):: iret
    integer:: i,k,n
    integer:: nc,k1,k2,mdim1,ldim2,udim2,mdim3q
    integer(8):: iskip,iwrite,nwrite
    type(sigio_datm):: datm
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    k1=dbtm%k1
    k2=dbtm%k2
    mdim1=min(size(dbtm%t,1),size(dbtm%d,1),size(dbtm%z,1),&
              size(dbtm%q,1))
    ldim2=max(lbound(dbtm%t,2),lbound(dbtm%d,2),lbound(dbtm%z,2),&
              lbound(dbtm%q,2))
    udim2=min(ubound(dbtm%t,2),ubound(dbtm%d,2),ubound(dbtm%z,2),&
              ubound(dbtm%q,2))
    mdim3q=size(dbtm%q,3)
    nc=(head%jcap+1)*(head%jcap+2)
    iret=-5
    if(k1.lt.1.or.k2.gt.head%levs.or.&
       mdim1.lt.nc.or.&
       ldim2.gt.k1.or.udim2.lt.k2.or.&
       mdim3q.lt.head%ntrac) return
    iret=-4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%irealf.eq.2) then
      iskip=0
      do i=1,head%nhead
        call bafrindexl(0,iskip,int(head%lhead(i),8),iskip)
      enddo
      i=1
      call bafrindexl(0,iskip,int(head%ldata(i),8),iskip)
      i=i+1
      call bafrindexl(0,iskip,int(head%ldata(i),8),iskip)
      do k=1,head%levs
        if(k.lt.k1.or.k.gt.k2) then
          i=i+1
          call bafrindexl(0,iskip,int(head%ldata(i),8),iskip)
        else
          i=i+1
          iwrite=head%ldata(i)
!LLF+PM--
          call byteswap(dbtm%t(1,k),sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
          call bafrwritel(lu,iskip,iwrite,nwrite,dbtm%t(1,k))
          if(nwrite.lt.iwrite) return
!LLF+PM--
          call byteswap(dbtm%t(1,k),sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
          iskip=iskip+nwrite
        endif
      enddo
      do k=1,head%levs
        if(k.lt.k1.or.k.gt.k2) then
          i=i+1
          call bafrindexl(0,iskip,int(head%ldata(i),8),iskip)
          i=i+1
          call bafrindexl(0,iskip,int(head%ldata(i),8),iskip)
        else
          iwrite=head%ldata(i)
!LLF+PM--
          call byteswap(dbtm%d(1,k),sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
          call bafrwritel(lu,iskip,iwrite,nwrite,dbtm%d(1,k))
          if(nwrite.lt.iwrite) return
!LLF+PM--
          call byteswap(dbtm%d(1,k),sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
          iskip=iskip+nwrite
          iwrite=head%ldata(i)
!LLF+PM--
          call byteswap(dbtm%z(1,k),sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
          call bafrwritel(lu,iskip,iwrite,nwrite,dbtm%z(1,k))
          if(nwrite.lt.iwrite) return
!LLF+PM--
          call byteswap(dbtm%z(1,k),sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
          iskip=iskip+nwrite
        endif
      enddo
      do n=1,head%ntrac
        do k=1,head%levs
          if(k.lt.k1.or.k.gt.k2) then
            i=i+1
            call bafrindexl(0,iskip,int(head%ldata(i),8),iskip)
          else
            i=i+1
            iwrite=head%ldata(i)
!LLF+PM--
            call byteswap(dbtm%q(1,k,n),sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
            call bafrwritel(lu,iskip,iwrite,nwrite,dbtm%q(1,k,n))
            if(nwrite.lt.iwrite) return
!LLF+PM--
            call byteswap(dbtm%q(1,k,n),sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
            iskip=iskip+nwrite
          endif
        enddo
      enddo
    else
      call sigio_aldatm(head,k1,k2,datm,iret)
      if(iret.ne.0) return
      datm%t(:nc,k1:k2)=dbtm%t(:nc,k1:k2)
      datm%d(:nc,k1:k2)=dbtm%d(:nc,k1:k2)
      datm%z(:nc,k1:k2)=dbtm%z(:nc,k1:k2)
      datm%q(:nc,k1:k2,:head%ntrac)=dbtm%q(:nc,k1:k2,:head%ntrac)
      call sigio_rwdatm(lu,head,datm,iret)
      if(iret.ne.0) return
      call sigio_axdatm(datm,iret)
    endif
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_rrdbti(lu,head,dbti,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    type(sigio_head),intent(in):: head
    type(sigio_dbti),intent(inout):: dbti
    integer(sigio_intkind),intent(out):: iret
    integer:: i,k,n
    integer:: mdim1
    integer:: mlen
    integer(8):: iskip,iread,nread
    type(sigio_dati):: dati
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    i=dbti%i
    mdim1=size(dbti%f,1)
    iret=-5
    if(i.lt.1.or.i.gt.head%ndata) return
    mlen=head%ldata(i)/(4*head%irealf)
    if(mdim1.lt.mlen) return
    iret=-4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%irealf.eq.2) then
      iskip=0
      do i=1,head%nhead
        call bafrindexl(0,iskip,int(head%lhead(i),8),iskip)
      enddo
      do i=1,dbti%i-1
        call bafrindexl(0,iskip,int(head%ldata(i),8),iskip)
      enddo
      i=dbti%i
      iread=head%ldata(i)
      call bafrreadl(lu,iskip,iread,nread,dbti%f)
      if(nread.lt.iread) return
!LLF+PM--
      call byteswap(dbti%f,sigio_dblekind,iread/sigio_dblekind)
!LLF+PM==
    else
      i=dbti%i
      call sigio_aldati(head,i,dati,iret)
      if(iret.ne.0) return
      call sigio_rrdati(lu,head,dati,iret)
      if(iret.ne.0) return
      dbti%f(:mlen)=dati%f(:mlen)
      call sigio_axdati(dati,iret)
    endif
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_rwdbti(lu,head,dbti,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    type(sigio_head),intent(in):: head
    type(sigio_dbti),intent(in):: dbti
    integer(sigio_intkind),intent(out):: iret
    integer:: i,k,n
    integer:: mdim1
    integer:: mlen
    integer(8):: iskip,iwrite,nwrite
    type(sigio_dati):: dati
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    i=dbti%i
    mdim1=size(dbti%f,1)
    iret=-5
    if(i.lt.1.or.i.gt.head%ndata) return
    mlen=head%ldata(i)/(4*head%irealf)
    if(mdim1.lt.mlen) return
    iret=-4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%irealf.eq.2) then
      iskip=0
      do i=1,head%nhead
        call bafrindexl(0,iskip,int(head%lhead(i),8),iskip)
      enddo
      do i=1,dbti%i-1
        call bafrindexl(0,iskip,int(head%ldata(i),8),iskip)
      enddo
      i=dbti%i
      iwrite=head%ldata(i)
!LLF+PM--
      call byteswap(dbti%f,sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
      call bafrwritel(lu,iskip,iwrite,nwrite,dbti%f)
      if(nwrite.lt.iwrite) return
!LLF+PM--
      call byteswap(dbti%f,sigio_dblekind,iwrite/sigio_dblekind)
!LLF+PM==
    else
      i=dbti%i
      call sigio_aldati(head,i,dati,iret)
      if(iret.ne.0) return
      dati%f(:mlen)=dbti%f(:mlen)
      call sigio_rwdati(lu,head,dati,iret)
      if(iret.ne.0) return
      call sigio_axdati(dati,iret)
    endif
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
end module
