c     parameter(idim=144,jdim=73)
      parameter(idim=360,jdim=181)
      dimension x(idim,jdim)
c     open(unit=10,file='grid.data',form='unformatted')
      open(unit=10,file='grid1deg.data',form='unformatted')
      do 10 j=1,jdim
      do 10 i=1,idim
      x(i,j)=1e20
10    continue
      write(10)x
      stop
      end
