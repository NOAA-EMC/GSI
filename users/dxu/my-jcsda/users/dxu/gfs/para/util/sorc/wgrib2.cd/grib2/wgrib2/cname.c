#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "wgrib2.h"
#include "grb2.h"



extern struct gribtab_s gribtab[];

static struct gribtab_s *search_gribtab(struct gribtab_s *gribtab, unsigned char **sec);

#ifdef USE_TIGGE
extern int tigge;
extern struct gribtab_s tigge_gribtab[];
#endif
/*
 * get the name information
 *
 * if inv_out, name, desc, unit == NULL, not used
 *
 * v1.0 Wesley Ebisuzaki 2006
 * v1.1 Wesley Ebisuzaki 4/2007 netcdf support
 * v1.2 Wesley Ebisuzaki 4/2007 multiple table support
 */


int getName(unsigned char **sec, int mode, char *inv_out, char *name, char *desc, char *unit) {

    int discipline, center, mastertab, localtab, parmcat, parmnum;
    struct gribtab_s *p;

    p = NULL;

#ifdef USE_TIGGE
    if (tigge) p = search_gribtab(tigge_gribtab, sec);		/* tigge is default table */
#endif
    if (p == NULL) p = search_gribtab(gribtab, sec);
#ifdef USE_TIGGE
    /* if undefined and a tigge file */
    if (p == NULL && !tigge && (code_table_1_3(sec) == 4 || code_table_1_3(sec) == 5)) p = search_gribtab(tigge_gribtab, sec);
#endif

    if (p) {
        if (name) strcpy(name, p->name);
	if (desc) strcpy(desc, p->desc);
	if (unit) strcpy(unit, p->unit);

	if (inv_out) {
	    sprintf(inv_out, "%s", p->name);
	    inv_out += strlen(inv_out);
            if (mode) sprintf(inv_out," %s [%s]", p->desc, p->unit);
        }
   }
   else {
        discipline = GB2_Discipline(sec);
        center = GB2_Center(sec);
        mastertab = GB2_MasterTable(sec);
        localtab = GB2_LocalTable(sec);
        parmcat = GB2_ParmCat(sec);
        parmnum = GB2_ParmNum(sec);

        if (name) sprintf(name,"var%d_%d_%d",discipline,parmcat,parmnum);
	if (desc) strcpy(desc,"desc");
	if (unit) strcpy(unit,"unit");

	if (inv_out) {
            if (parmnum < 192) {
                sprintf(inv_out,"var discipline=%d master_table=%d parmcat=%d parm=%d", 
                  discipline, mastertab, parmcat, parmnum);
            }
            else {
	        sprintf(inv_out,"var discipline=%d center=%d local_table=%d parmcat=%d parm=%d",
                  discipline, center, localtab, parmcat, parmnum);
            }
	}
   }
   return 0;
}

/*
 * search the grib table
 */

static struct gribtab_s *search_gribtab(struct gribtab_s *p, unsigned char **sec){

    int discipline, center, mastertab, localtab, parmcat, parmnum;

    discipline = GB2_Discipline(sec);
    center = GB2_Center(sec);
    mastertab = GB2_MasterTable(sec);
    localtab = GB2_LocalTable(sec);
    parmcat = GB2_ParmCat(sec);
    parmnum = GB2_ParmNum(sec);

    // if (mastertab == 0) mastertab = 1;
    if (mastertab >= 0 && mastertab <= 5) mastertab = 1;

    for (; p->disc >= 0; p++) {
        if (parmnum < 192) {
            if (discipline == p->disc && (mastertab == p->mtab || p->mtab == -1) &&
                parmcat == p->pcat && parmnum == p->pnum) {
                return p;
            }
        }
        else {
            if (discipline == p->disc && center == p->cntr &&
                localtab == p->ltab &&
                parmcat == p->pcat && parmnum == p->pnum) {
                return p;
                break;
            }
        }
   }
   return NULL;
}

