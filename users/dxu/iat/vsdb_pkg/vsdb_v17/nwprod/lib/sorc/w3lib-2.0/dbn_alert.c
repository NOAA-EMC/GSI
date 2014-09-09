/*
 * dbn_alert.c
 *
 * Distributed Brokered Networking (DBNet) alert hook 
 *
 * author: Luis J. Cano
 * date:   2/20/96
 *
 *
 * $Date: 1996/09/16 18:51:08 $
 * $Id: dbn_alert.c,v 2.5 1996/09/16 18:51:08 dbnet-bl Exp $
 * $Log: dbn_alert.c,v $
 * Revision 2.5  1996/09/16 18:51:08  dbnet-bl
 * Added local (file scope) variables to hold the exec parameters, using
 * the vars passed in from a fortran program would cause a Bad Address error
 * on the cray.  This is apparently a problem with doing an exec in c when
 * called by fortran.... Larry
 *
 * Revision 2.4  1996/09/11 12:39:48  dbnet-bl
 * Changed exec call to a fork, exec, waitpid to allow fortran callers
 * to use this module in the blocked mode..
 *
 * Revision 2.3  1996/08/21 13:53:21  dbnet-bl
 * Added -D option for the dbn_alert function to be all caps.
 * This option will work in conjuntion with the UNDERSCORE option.
 * The option name is CAPS.  This is needed for the fortran bindin
 * on the cray.  Larry
 *
 * Revision 2.2  1996/08/14 17:18:06  dbnet-bl
 * execl would not execute a script on the cray (only binary executables).
 * Changed execl to execlp. Changed the second param when using execlp to the
 * file name DBNALERTFILE vice the entire path. Changed the NULL to (char *)0,
 * per posix. Louie.
 *
 * Revision 2.1  1996/08/07 15:05:05  dbnet-bl
 * Bug with the execlp, doesn't work in a qsub on a cray. Larry found and fixed it
 * by changing the execlp to execl. Added a NOT_BLOCKED macro that can be set when
 * compiled (-DNOT_BLOCKED), which will do the alert work in a granchild
 * process, or if not set, defaults the execlp in the current process. Going back
 * to the execlp on the cray, the bug is not with execlp, but the way it was being
 * used. When in a qsup, it doesn't like when the execlp is used with a complete
 * path. Works when not in a qsub, and has been working on the workstations. Louie
 *
 * Revision 2.0  1996/08/07 14:29:49  dbnet-bl
 * initial DBNet 2.0 module checkin
 *
 *
 */


#define _POSIX_SOURCE 1

#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>


/* add new define for CAPS.  Make it work with UNDERSCORE */
#ifdef UNDERSCORE

#ifdef CAPS
#define dbn_alert DBN_ALERT_
#else
#define dbn_alert dbn_alert_
#endif

#else

#ifdef CAPS
#define dbn_alert DBN_ALERT
#else
#define dbn_alert dbn_alert
#endif

#endif

#define DBNROOT_ENV "DBNROOT"
#define DBNALERTPL "/bin/dbn_alert.pl"
#define DBNALERTFILE "dbn_alert.pl"


static char *ntype = NULL;
static char *nsubtype = NULL;
static char *njob = NULL;
static char *npath = NULL;
static char *dbn_alertpl=NULL;

void free_mem(void) ;

/*
 * main function
 */
void dbn_alert(char *type,     int *typelen, 
	       char *subtype,  int *subtypelen,
               char *job,      int *joblen,
	       char *path,     int *pathlen,
	       int  *iret                          ) {

	const char myname[]="\ndbn_alert:";

	pid_t cpid, gpid;
	static char *dbn_root=NULL;

/*
 * ensure null terminated strings
 */
	type[*typelen]=0x0;
	subtype[*subtypelen]=0x0;
        job[*joblen]=0x0;
	path[*pathlen]=0x0;
	*iret=1;			/* assume an error */

/*
 * validate DBNet root env variable
 */
	if (dbn_root==NULL) {
		dbn_root=getenv(DBNROOT_ENV);
		if (dbn_root==NULL) {
			(void)fprintf(stderr, "\n%s environment variable %s not set!\n", myname, DBNROOT_ENV);
			return;
		}
	}

/*
 * determine path to dbn_alert.pl
 */

	if (dbn_alertpl==NULL) {
		dbn_alertpl=malloc(strlen(dbn_root)+strlen(DBNALERTPL)+1);	/* include null */
		if (dbn_alertpl==NULL) {
			(void)fprintf(stderr, "%s malloc(%u) for the dnb_alert.pl path failed", 
				myname, strlen(dbn_root)+strlen(DBNALERTPL)+1);
			return;
		}
		(void)strcpy(dbn_alertpl, dbn_root);
		(void)strcat(dbn_alertpl, DBNALERTPL);
	}

        if ( (ntype=malloc((size_t)typelen + 1)) == NULL) {	/* include null */
	   (void)fprintf(stderr, "%s malloc(%u) for the ntype failed", 
		myname, typelen + 1);
                free_mem();
		return;
	}
	(void)strcpy(ntype, type);

        if ( (nsubtype=malloc((size_t)subtypelen + 1)) == NULL) {	/* include null */
	   (void)fprintf(stderr, "%s malloc(%u) for the nsubtype failed", 
		myname, subtypelen + 1);
                free_mem();
		return;
	}
	(void)strcpy(nsubtype, subtype);
	
        if ( (njob=malloc((size_t)joblen + 1)) == NULL) {	/* include null */
	   (void)fprintf(stderr, "%s malloc(%u) for the njob failed", 
		myname, joblen + 1);
                free_mem();
		return;
	}
	(void)strcpy(njob, job);
        
 	if ( (npath=malloc((size_t)pathlen + 1)) == NULL) {	/* include null */
	   (void)fprintf(stderr, "%s malloc(%u) for the npath failed", 
		myname, pathlen + 1);
                free_mem();
		return;
	}
	(void)strcpy(npath, path);

#if NOT_BLOCKED

/*
 * do the alert work in a grandchild process
 */
	if ((cpid=fork()) < 0) {
		perror("dbn_alert: fork");
		(void)fprintf(stderr, "%s fork failed", myname);
                free_mem();
		return;
	}
	if (cpid==0) {	/* child processing */
		if ((gpid=fork()) < 0) {
			perror("dbn_alert: child: fork");
			(void)fprintf(stderr, "%s child: fork failed", myname);
			exit(EXIT_FAILURE);
		}
		if (gpid==0) {  /* grandchild process */
			if (execlp(dbn_alertpl, DBNALERTFILE, ntype, nsubtype, njob, npath, (char *)0)<0) {
				perror("dbn_alert: grandchild: execlp");
				(void)fprintf(stderr, "%s grandchild: execlp(%s, %s, %s, %s,%s, %s) failed\n",
					myname, dbn_alertpl, DBNALERTFILE, ntype, nsubtype, njob,npath);  
			}
			exit(EXIT_FAILURE);
		}
		exit(EXIT_SUCCESS);
	}
	while (wait((int *)NULL) != cpid) 
		;

#else

/*
 * do the alert work in current proccess 
 */
/* change the exec to a system call for the blocked version, this will allow the
   caller to get the return code.
	if (execlp(dbn_alertpl, DBNALERTFILE, type, subtype, job, path, (char *)0)<0) {
		perror("dbn_alert: execlp");
		(void)fprintf(stderr, "%s execlp(%s, %s, %s, %s, %s, %s) failed\n",
		myname, dbn_alertpl, DBNALERTFILE, type, subtype, job, path);
		return;
	} 
*/
	if ((cpid=fork()) < 0) {
		perror("dbn_alert: fork");
		(void)fprintf(stderr, "%s fork failed", myname);
                free_mem();
		return;
	}
	if (cpid==0) {	/* child processing */

	  if (execlp(dbn_alertpl, DBNALERTFILE, ntype, nsubtype, njob, npath,'\0')<0) {
		perror("dbn_alert: execle");
		(void)fprintf(stderr, "%s execlp(%s, %s, %s, %s, %s, %s) failed\n",
		myname, dbn_alertpl, DBNALERTFILE, ntype, nsubtype, njob, npath);
                *iret = 2;
		return;
	  } 
	} else {
          waitpid(cpid,iret,0);
          free_mem();
          return;
        }

#endif
        free_mem();
	*iret=0;		/* indicate success */
	return;
}

void free_mem(void) {

  if (ntype != NULL) 
    free(ntype);
  if (njob != NULL) 
    free(njob);
  if (nsubtype != NULL) 
    free(nsubtype);
  if (npath != NULL) 
    free(npath);
  if (dbn_alertpl != NULL) 
    free(dbn_alertpl);
}

