
*  accept user input until flush, then issue all commands
*  and wait for prompt.

i = 0
say "Enter commands, then flush: "
while (1)
  i = i + 1
  say "Next Command:"
  pull cmd.i
  if (cmd.i = 'flush')
    break
  endif
endwhile
if (i=1)
  return
endif
'clear'
j = 1
while (j<i)
  cmd.j
  res.j = result
  j = j + 1
endwhile
pull dummy
'clear'
j = 1
while (j<i)
  say res.j
  j = j + 1
endwhile

