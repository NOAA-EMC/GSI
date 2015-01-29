say 'FRAME script running. Enter "exit" to terminate script.'
while (1)
  prompt 'ga> '
  pull cmd
  wrd = subwrd(cmd,1)
  if (wrd='exit' | wrd='quit') 
    break
  endif
  if (wrd='d' | wrd='display') 
    'clear norset'
    cmd
    res = result
    pull dummy
    'clear'
    prompt res
  else
    cmd
    prompt result
  endif
endwhile
say 'FRAME script done'
