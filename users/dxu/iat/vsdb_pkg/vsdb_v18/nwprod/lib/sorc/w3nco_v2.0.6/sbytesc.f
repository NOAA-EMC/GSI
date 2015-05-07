      SUBROUTINE SBYTESC(OUT,IN,ISKIP,NBYTE,NSKIP,N)
C          Store bytes - pack bits:  Put arbitrary size values into a
C          packed bit string, taking the low order bits from each value
C          in the unpacked array.
C            IOUT  = packed array output
C            IN    = unpacked array input
C            ISKIP = initial number of bits to skip
C            NBYTE = number of bits to pack
C            NSKIP = additional number of bits to skip on each iteration
C            N     = number of iterations
C v1.1
C
      character*1 out(*)
      integer in(N), bitcnt, ones(8), tbit
      save ones
      data ones/    1,  3,  7, 15, 31, 63,127,255/

c     number bits from zero to ...
c     nbit is the last bit of the field to be filled

      nbit = iskip + nbyte - 1
      do i = 1, n
         itmp = in(i)
         bitcnt = nbyte
         index=nbit/8+1
         ibit=mod(nbit,8)
         nbit = nbit + nbyte + nskip

c        make byte aligned
         if (ibit.ne.7) then
             tbit = min(bitcnt,ibit+1)
             imask = ishft(ones(tbit),7-ibit)
             itmp2 = iand(ishft(itmp,7-ibit),imask)
             itmp3 = iand(mova2i(out(index)), 255-imask)
             out(index) = char(ior(itmp2,itmp3))
             bitcnt = bitcnt - tbit
             itmp = ishft(itmp, -tbit)
             index = index - 1
         endif

c        now byte aligned

c        do by bytes
         do while (bitcnt.ge.8)
             out(index) = char(iand(itmp,255))
             itmp = ishft(itmp,-8)
             bitcnt = bitcnt - 8
             index = index - 1
         enddo

c        do last byte

         if (bitcnt.gt.0) then
             itmp2 = iand(itmp,ones(bitcnt))
             itmp3 = iand(mova2i(out(index)), 255-ones(bitcnt))
             out(index) = char(ior(itmp2,itmp3))
         endif
      enddo

      return
      end
