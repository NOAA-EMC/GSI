function fprintf(args)
 
* Command line argumets
* ---------------------
  expr = subwrd(args,1)
  file = subwrd(args,2)
  format = subwrd(args,3)
  numl = subwrd(args,4)
  numb = subwrd(args,5)
  u = subwrd(args,6)
 
  if ( file='' )
    say ''
    say 'NAME'
    say '     fprintf - print GrADS variables to a text file'
    say ''
    say 'SYNOPSIS'
    say '     fprintf  expr  txtFile  [format numl numb [u]]'
    say ''
    say 'DESCRIPTION'
    say '     Evaluates the contents of the GrADS expression *expr* writing'
    say '     its values to a formatted text file *txtFile*. On output, the'
    say '     number of values and the undef values are returned; a negative'
    say '     number of values signals an error condition.'
    say ''
    say '     Unlike the output of *set gxout print*, the resulting ASCII'
    say '     file has only the data values and *no header*.'
    say ''
    say 'OPTIONS'
    say '     The optional parameters are the same as the ones required by'
    say '     the GrADS command *set prnopts*, namely'
    say '     format   a C language template for formatting ASCII output.'
    say '              Default is %g.'
    say '     numl     number of values to print per record. Default is 8.'
    say '     numb     number of blanks to insert between values. Default is 1.'
    say '     u        print "Undef" instead of the numerical value for'
    say '              missing data.'
    say 'BUGS'
    say '     The GrADS expression cannot have spaces in it.'
    say ''    
    say 'COPYRIGHT'
    say '     This script has been placed in the public domain'
    say ''
    return
  endif
 
* Set the display environment and produce buffered ASCII output
* -------------------------------------------------------------
  'set gxout print'
  if ( format != '' )
    'set prnopts ' format ' ' numl ' ' numb ' ' u ' '
  endif
  'display ' expr
   if ( rc!=0 ); return -1; endif
 
*  Get rid of header line: Printing Grid -- 3358 Values -- Undef = 1e+20
*  but record number of values and undef values for later reference
*  ---------------------------------------------------------------------
   buffer = result
   i = 1; line = sublin(buffer,i)
   n = subwrd(line,4)
   undef = subwrd(line,9)
 
* Now write the data values to text file: first line...
* -----------------------------------------------------
  i = 2; line = sublin(buffer,i)
  if ( write_(file,line) > 0 ); return -2; endif
 
* Append subsequent lines
* -----------------------
  i = i + 1; line = sublin(buffer,i)
  while ( line != '' )
    if ( write_(file,line,append) != 0 ); return -3; endif 
    i = i + 1; line = sublin(buffer,i)
  endwhile
  if ( close(file) != 0 ); return -4; endif
 
* All done
* --------
  say 'wrote ' n ' values to file "' file '"'
  return n ' ' undef ' ' 
 
function write_(file,line)
   rc = write(file,line)
   return subwrd(rc,1)
