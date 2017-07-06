*  Plots a string using current attributes
*  at "clicked on" position.  String can be provided
*  as an argument or will be prompted for 
*
function string (args)
  if (args='') 
    say 'Enter string:'
    pull args
  endif
  say 'Click where you want the string'
  'query bpos'
  x = subwrd(result,3)
  y = subwrd(result,4)
  'draw string 'x' 'y' 'args
