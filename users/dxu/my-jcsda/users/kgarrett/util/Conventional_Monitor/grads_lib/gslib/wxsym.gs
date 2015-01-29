*  Draws all wx symbols
*
*  Note:  A file must be open to issue a set command.
*         (This will be fixed in a future version).
*         open a file before running this script, or 
*         change the file name below to a data descriptor
*         file that is on your system. 

function main (args) 

  'query file'
  if (sublin(result,1) = "No Files Open") 
    'open /local/lib/grads/masklow.ctl'
  endif
  wx = 1
  x = 1
  y = 7.2
  'set string 1 c 6'
  'set strsiz 0.25'
  while (wx<42) 
    'draw wxsym 'wx' 'x' 'y' 0.7 -1 6'
    'draw string 'x' '%(y-0.55)%' 'wx
    wx = wx + 1
    x = x + 1
    if (x > 10) 
      x = 1
      y = y - 1.5
    endif
  endwhile 
