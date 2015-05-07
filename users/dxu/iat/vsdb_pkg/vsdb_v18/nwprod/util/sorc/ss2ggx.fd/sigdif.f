program sigdif
  use sigio_module
  implicit none
  integer narg,iargc
  integer(sigio_intkind),parameter:: lusig1=11,lusig2=12,lusig3=51
  integer(sigio_intkind):: irets
  character*255 cfsig1,cfsig2,cfsig3
  type(sigio_head):: sighead1,sighead2
  type(sigio_data):: sigdata1,sigdata2
  integer ncfsig1,ncfsig2,ncfsig3
  narg=iargc()
  if(narg.ne.3) then
     if(narg.ne.0) call errmsg('sigdif: incorrect number of arguments')
     call eusage
     call errexit(1)
  endif
  call getarg(1,cfsig1)
  ncfsig1=len_trim(cfsig1)
  call sigio_srohdc(lusig1,cfsig1(1:ncfsig1),sighead1,sigdata1,irets)
  if(irets.ne.0) then
     call errmsg('sigdif: error reading file '//cfsig1(1:ncfsig1))
     call errexit(2)
  endif
  call getarg(2,cfsig2)
  ncfsig2=len_trim(cfsig2)
  call sigio_srohdc(lusig2,cfsig2(1:ncfsig2),sighead2,sigdata2,irets)
  if(irets.ne.0) then
     call errmsg('sigdif: error reading file '//cfsig2(1:ncfsig2))
     call errexit(2)
  endif
  if(sighead1%jcap.ne.sighead2%jcap) then
     call errmsg('sigdif: spectral truncations differ')
     call errexit(3)
  endif
  if(sighead1%levs.ne.sighead2%levs) then
     print *,sighead1%levs
     print *,sighead2%levs
     call errmsg('sigdif: vertical levels differ')
     call errexit(4)
  endif
  if(any(sighead1%sl(:sighead1%levs).ne.sighead2%sl(:sighead2%levs))) then
     print *,sighead1%sl(:sighead1%levs)
     print *,sighead2%sl(:sighead2%levs)
     call errmsg('sigdif: vertical levels differ')
     call errexit(4)
  endif
  if(sighead1%ntrac.ne.sighead2%ntrac) then
     call errmsg('sigdif: numbers of tracers differ')
     call errexit(5)
  endif
  sighead1%idpp=1
  sigdata1%hs=sigdata1%hs-sigdata2%hs
  sigdata1%ps=sigdata1%ps-sigdata2%ps
  sigdata1%t=sigdata1%t-sigdata2%t
  sigdata1%d=sigdata1%d-sigdata2%d
  sigdata1%z=sigdata1%z-sigdata2%z
  sigdata1%q=sigdata1%q-sigdata2%q
  call getarg(3,cfsig3)
  ncfsig3=len_trim(cfsig3)
  call sigio_swohdc(lusig3,cfsig3(1:ncfsig3),sighead1,sigdata1,irets)
  if(irets.ne.0) then
     call errmsg('sigdif: error writing file '//cfsig3(1:ncfsig3))
     call errexit(6)
  endif
end program
subroutine eusage
  implicit none
  call errmsg('usage: sigdif sigfile1 sigfile2 sigfiled')
end subroutine
