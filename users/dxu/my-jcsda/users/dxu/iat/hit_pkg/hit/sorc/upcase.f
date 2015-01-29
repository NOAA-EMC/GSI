      subroutine upcase (string,nchar)

c  this routine converts all lower case characters (ascii 97 - 122)
c  in the variable string to upper case characters (ascii 65 - 90).

      character string*(*)

c  loop thru each character in the string


c     call prctrc('upcase',.true.)

      do 100 i=1,nchar

c  if it is lower case, subtract 32 from it to make it upper case.

      ich = ichar(string(i:i))
      if ((ich .gt. 96) .and. (ich .lt. 123)) string(i:i) = 
     &         char(ichar(string(i:i))-32)
  100 continue

c     call prctrc('upcase',.false.)

      return
      end
