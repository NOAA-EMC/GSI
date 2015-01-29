  ! ----- 
  ! Variable definitions and inquiry
  ! ----- 
  function nf90_def_var_Scalar(ncid, name, xtype, varid)
    integer,               intent( in) :: ncid
    character (len = *),   intent( in) :: name
    integer,               intent( in) :: xtype
    integer,               intent(out) :: varid
    integer                            :: nf90_def_var_Scalar
    
    ! Dummy - shouldn't get used
    integer, dimension(1) :: dimids
    
    nf90_def_var_Scalar = nf_def_var(ncid, name, xtype, 0, dimids, varid)
  end function nf90_def_var_Scalar
  ! ----- 
  function nf90_def_var_oneDim(ncid, name, xtype, dimids, varid)
    integer,               intent( in) :: ncid
    character (len = *),   intent( in) :: name
    integer,               intent( in) :: xtype
    integer,               intent( in) :: dimids
    integer,               intent(out) :: varid
    integer                            :: nf90_def_var_oneDim
    
    nf90_def_var_oneDim = nf_def_var(ncid, name, xtype, 1, dimids, varid)
  end function nf90_def_var_oneDim
  ! ----- 
  function nf90_def_var_ManyDims(ncid, name, xtype, dimids, varid)
    integer,               intent( in) :: ncid
    character (len = *),   intent( in) :: name
    integer,               intent( in) :: xtype
    integer, dimension(:), intent( in) :: dimids
    integer,               intent(out) :: varid
    integer                            :: nf90_def_var_ManyDims
    
    nf90_def_var_ManyDims = nf_def_var(ncid, name, xtype, size(dimids), dimids, varid)
  end function nf90_def_var_ManyDims
  ! ----- 
  function nf90_inq_varid(ncid, name, varid)
    integer,             intent( in) :: ncid
    character (len = *), intent( in) :: name
    integer,             intent(out) :: varid
    integer                          :: nf90_inq_varid
    
    nf90_inq_varid = nf_inq_varid(ncid, name, varid)
  end function nf90_inq_varid
  ! ----- 
  function nf90_Inquire_Variable(ncid, varid, name, xtype, ndims, dimids, nAtts)
    integer,                         intent( in) :: ncid, varid
    character (len = *),   optional, intent(out) :: name
    integer,               optional, intent(out) :: xtype, ndims 
    integer, dimension(:), optional, intent(out) :: dimids
    integer,               optional, intent(out) :: nAtts
    integer                                      :: nf90_Inquire_Variable
    
    ! Local variables
    character (len = nf90_max_name)       :: varName
    integer                               :: externalType, numDimensions
    integer, dimension(nf90_max_var_dims) :: dimensionIDs
    integer                               :: numAttributes
    
    nf90_Inquire_Variable = nf_inq_var(ncid, varid, varName, externalType, &
                                       numDimensions, dimensionIDs, numAttributes)
    if(present(name))   name                   = trim(varName)
    if(present(xtype))  xtype                  = externalType
    if(present(ndims))  ndims                  = numDimensions
    if(present(dimids)) dimids(:numDimensions) = dimensionIDs
    if(present(nAtts))  nAtts                  = numAttributes
  end function nf90_Inquire_Variable
  ! ----- 
  function nf90_rename_var(ncid, varid, newname)
    integer,             intent( in) :: ncid, varid
    character (len = *), intent( in) :: newname
    integer                          :: nf90_rename_var
    
    nf90_rename_var = nf_rename_var(ncid, varid, newname)
  end function nf90_rename_var
  ! ----- 
