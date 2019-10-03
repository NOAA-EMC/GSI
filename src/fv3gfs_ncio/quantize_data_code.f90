   integer, intent(in) :: nbits
   real(4), intent(out) :: compress_err
   real(4) dataMin, dataMax, scale_fact, offset
   dataMax = maxval(dataIn); dataMin = minval(dataIn)
   ! convert data to 32 bit integers in range 0 to 2**nbits-1, then cast
   ! cast back to 32 bit floats (data is then quantized in steps
   ! proportional to 2**nbits so last 32-nbits in floating
   ! point representation should be zero for efficient zlib compression).
   scale_fact = (dataMax - dataMin) / (2**nbits-1); offset = dataMin
   dataOut = scale_fact*(nint((dataIn - offset) / scale_fact)) + offset
   compress_err = maxval(abs(dataIn-dataOut))
