/***********************************************************************
 *  Program Name      : get_conv_index.c
 *  Type              : Subroutine
 *  Function          : Program calculates ice particle type (global)
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called :
 *  Called by         : calprod.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   06/25/2002      v2.0
 ***********************************************************************/
#include "MSWATH_INCLUDE.h"
#include "ESWATH.h"

void get_conv_index(short int iscan)
{
       float        b89, b150, b182, b180, b176;
       float        dta, dta1, dta2, dta3;
       short int    clr;
       short int    ifov;

       for (ifov = 0; ifov < NUMSPOT_M; ifov++)
       {
           b89 = at[iscan][ifov][0];
           b150 = at150[iscan][ifov];
           b182 = at[iscan][ifov][2];
           b180 = at[iscan][ifov][3];
           b176 = at176[iscan][ifov];
       
          if(b89 != MISSING && b150 != MISSING && b182 != MISSING &&
              b180 != MISSING &&  b176 != MISSING)
          {
              dta=b89 - b150;
              dta1=b182 - b176;
              dta2=b180 - b176;
              dta3=b182 - b180;
              clr=0;
              if(dta > 0 && dta2 > 0 && dta2 > dta1 && dta2 > dta3) clr=1;
              if(dta > 0 && dta2 > 0 && dta1 > 0 && dta3 > 0 &&
                 dta1 > dta2 && dta1 > dta3 && dta2 > dta3) clr=2;
              if(dta > 0 && dta2 > 0 && dta1 > 0 && dta3 > 0 &&
                 dta1 > dta2 && dta1 > dta3 && dta2 <= dta3) clr=3;
              ctype[iscan][ifov]=clr;

          }
       }
}
