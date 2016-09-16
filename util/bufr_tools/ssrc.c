/* SSRC - byte-swap sequential record counts in Fortran unformatted files.
 * Do NOT swap the data in the records.
 * This program assumes that Fortran sequential unformatted files have the
 * form  <count> <data> <count>
 * where <count> is a 4-byte integer equal to the number of bytes in <data>.
 * This program assumes that "int" is a 32-bit type.  Lahey and similar
 * PC Fortrans usually limit record sizes to 2 GB, so a 32-bit int suffices
 * to handle all cases.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ------------------------------------------------------------------ */
/* SWAPINT - byte-swap the input value.
 */
int swapint(int val)
{
    int loc = val;
    unsigned char *c = (unsigned char *) &loc;
    unsigned char temp;

    temp = c[3];
    c[3] = c[0];
    c[0] = temp;
    temp = c[2];
    c[2] = c[1];
    c[1] = temp;
    return(loc);
}
/* ------------------------------------------------------------------ */
/* RWCOUNT - read, swap, and write the record byte count.
 * Return value is:
 *  >= 0 : the record size in bytes
 *  -1: EOF on input
 *  -2: input error
 *  -3: negative header count value
 *  -4: error writing to outbuf
 */
int rwcount(FILE *inbuf, FILE *outbuf)
{
    int count;
    size_t nrds = fread(&count, sizeof(count), 1, inbuf);

    if(nrds != 1)   /* oops; perhaps EOF */
       return(feof(inbuf) ? -1 : -2);
    count = swapint(count);
    if(count < 0)
       return(-3);
    return(fwrite(&count, sizeof(count), 1, outbuf) == 1 ? count : -4);
}
/* ------------------------------------------------------------------ */
/* PROCESS - process an input file.
 * Return value is 0 if no errors occurred.
 * Otherwise print a descriptive diagnostic and return a nonzero value.
 */
int process(const char *infn, const char *outfn)
{
    FILE *inbuf, *outbuf;
    size_t recno = 1;
    int count;
    
    if(strcmp(infn, "-") == 0)  {
       inbuf = stdin;
       infn = "standard input";
    } else if((inbuf = fopen(infn, "rb")) == NULL) {
       perror(infn);
       return(-1);
    }
    if(outfn == NULL)  {
       outbuf = stdout;
       outfn = "standard output";
    } else if((outbuf = fopen(outfn, "wb")) == NULL) {
       perror(outfn);
       return(-1);
    }
    while((count = rwcount(inbuf, outbuf)) >= 0)  {
       unsigned char *buf = malloc(count);
       if(buf == NULL)  {
          fprintf(stderr, "%s: memory allocation failed on record %lu\n",
             infn, (unsigned long) recno);
          return(1);
       }
       if(fread(buf, 1, count, inbuf) != count)  {
          fprintf(stderr, "%s: unexpected short record %lu\n", 
             infn, (unsigned long) recno);
          return(1);
       }
       if(fwrite(buf, 1, count, outbuf) != count)  {
          fprintf(stderr, "%s: error writing record %lu\n",
             outfn, (unsigned long) recno);
          return(1);
       }
       if(rwcount(inbuf, outbuf) != count)  {
          fprintf(stderr, "%s: record markers don't match, record %lu\n",
             infn, (unsigned long) recno);
          return(1);
       }
       free(buf);
       recno++;
    }
    switch(count)  {
    case -3:
       fprintf(stderr, "%s: negative record size at record %lu\n",
          infn, (unsigned long) recno);
       return(1);
    case -2:
       fprintf(stderr, "%s: error reading record length, record %lu\n",
          infn, (unsigned long) recno);
       return(1);
    case -1:
       return(0);   /* normal return: EOF on input */
    default:
       fprintf(stderr, "%s: errro reading record %lu\n",
          infn, (unsigned long) recno);
       return(1);
   }
}
/* ------------------------------------------------------------------ */
int main(int argc, char **argv)
{
   if(argc == 1)
      return(process("-", NULL));
   else
      return(process(argv[1], NULL));
}
