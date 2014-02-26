/***********************************************************************
 *  Program Name      : getfname.c
 *  Type              : Main program 
 *  Function          : Program gets lists of matched AMSU-A and MHS 
 *			swaths for the final MHS granule processing
 *  Input Files       : fileA.length, fileM.length, tmpA.list, tmpM.list 
 *  Output Files      : swath_fname_A.list, swath_fname_M.list, 
 *			fileAll.length 
 *  Subroutine Called : None
 *  Called by         : None 
 *
 ***********************************************************************/
#include "stdio.h"
#include "string.h"
#include "stdlib.h"

#define  MAX   200

int main()
{
  FILE		*fp_A, *fp_M, *fp;
  int		numA, numM, i, j, index = 0;
  int		dayAbegin[MAX], dayMbegin[MAX];
  int		dayAend[MAX], dayMend[MAX];
  int		beginAhr[MAX], endAhr[MAX], beginAmin[MAX], endAmin[MAX], beginAsec[MAX], endAsec[MAX];
  int		beginMhr[MAX], endMhr[MAX], beginMmin[MAX], endMmin[MAX], beginMsec[MAX], endMsec[MAX];
  long		beginA[MAX], endA[MAX], beginM[MAX], endM[MAX];
  char          *nameA[MAX], *nameM[MAX], nametmp[80];
  char		dumchar[80];

/* Read in the numbers of swath files from AMSU-A and MHS  */

  if((fp_A = fopen("fileA.length", "r")) != NULL)
  {
     fscanf(fp_A, "%d", &numA);
  }
  else
  { 
     printf("Fail to open fileA.length\n");
     exit(1);
  }
  fclose(fp_A);

  if((fp_M = fopen("fileM.length", "r")) != NULL)
     fscanf(fp_M, "%d", &numM);
  else
  { 
     printf("Fail to open fileM.length\n");
     exit(1);
  }
  fclose(fp_M);
 
  printf("Initial swath numbers A & M: %d %d \n", numA, numM);


/* Retrieve info. about swaths such as sizes, beginning and ending times */

  fp_A = fopen("tmpA.list", "r");
  fp_M = fopen("tmpM.list", "r");

  for(i = 0; i < numA; i++)
  {
    fscanf(fp_A, "%15s%3d%2s%2d%2d%2s%2d%2d",dumchar,&dayAbegin[i],dumchar, &beginAhr[i],&beginAmin[i],dumchar,&endAhr[i],&endAmin[i]);
    fgets(dumchar, 80, fp_A); 
    beginA[i] = beginAhr[i] * 60. + beginAmin[i] ; 
    endA[i] = endAhr[i] * 60. + endAmin[i] ;
  }

  for(i = 0; i < numM; i++)
  {
    fscanf(fp_M, "%15s%3d%2s%2d%2d%2s%2d%2d",dumchar,&dayMbegin[i],dumchar, &beginMhr[i],&beginMmin[i],dumchar,&endMhr[i],&endMmin[i]);
    fgets(dumchar, 80, fp_M); 
    beginM[i] = beginMhr[i] * 60. + beginMmin[i];
    endM[i] = endMhr[i] * 60. + endMmin[i];
  }

  fclose(fp_A);
  fclose(fp_M);

  fp_A = fopen("tmpA.list", "r");
  fp_M = fopen("tmpM.list", "r");

  for(i = 0; i < numA; i++)
  {
    fgets(nametmp, 43, fp_A);
    fgets(dumchar, 80, fp_A);
    nameA[i] = (char *) malloc(strlen(nametmp)+1);
    strcpy(nameA[i], nametmp); 
  }

  for(i = 0; i < numM; i++)
  {
    fgets(nametmp, 43, fp_M);
    fgets(dumchar, 80, fp_M);
    nameM[i] = (char *) malloc(strlen(nametmp)+1);
    strcpy(nameM[i], nametmp); 
  }
  
  fclose(fp_A);
  fclose(fp_M);

  fp_A = fopen("swath_fname_A.list", "w");
  fp_M = fopen("swath_fname_M.list", "w");

/* Determine which orbits are extra and remove them */
  for(i = 0; i < numA; i++)
  {
     for(j = 0; j < numM; j++)
     {

       if(abs(beginA[i] - beginM[j]) < 2 && abs(endA[i]-endM[j]) < 20)
       {

         {
           fprintf(fp_A, "%s\n", nameA[i]);
           fprintf(fp_M, "%s\n", nameM[j]);
           index += 1; 
         }
       }
     }
  }

  fclose(fp_A);
  fclose(fp_M);

  printf("  Final swath numbers A & M: %d %d \n", index, index);

  fp = fopen("fileAll.length", "w");
  fprintf(fp, "%d \n", index);
  fclose(fp);
  return 0;
} /* end of getfname.c */
