/**************************************************************************
 *  Program Name      : wrt_navig.c
 *  Type              : Subroutine
 *  Function          : Program writes navigation parameters to 
 *                      HDF-EOS swath file
 *  Input Files       : None
 *  Output Files      : NPR.ABOP.NK.Dyyddd.Shhmm.Ehhmm.Bnnnnnnn.NS
 *                      (yy: year, ddd: julian day, Shhmm: starting hour
 *                       and minute, Ehhmm: ending hour and minute,
 *                       nnnnnnn: orbit ID)
 *  Subroutine Called : None
 *  Called by         : sw_wrt.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/26/2000      v2.0
 **************************************************************************/
#include "AMB2HDF_INCLUDE.h"
#include "ESWATH.h"

/*************************************************************/
long int wrt_navig(long int sw_id)
{
      long int   status, result=0;

      short int  epoch_year; 
      short int  epoch_day;
      long  int  epoch_time; 

      double     semimajor_axis;
      double     eccentricity;
      double     inclination;
      double     argument_of_perigee;
      double     right_ascension;
      double     mean_anomaly;


      epoch_year = hblock3.orbit_vector_epoch_year;
      epoch_day  = hblock3.orbit_vector_day_of_year;
      epoch_time = hblock3.orbit_vector_utc_time_of_day;
      printf("Epoch year day time %d %d %ld \n",epoch_year, 
                             epoch_day, epoch_time); 

      semimajor_axis = hblock3.semimajor_axis;
      eccentricity = hblock3.eccentricity;
      inclination =hblock3.inclination;
      argument_of_perigee =hblock3.argument_of_perigee;
      right_ascension = hblock3.right_ascension;
      mean_anomaly = hblock3.mean_anomaly;
      
      printf("%f %f %f %f %f %f \n", semimajor_axis, eccentricity, 
            inclination, argument_of_perigee, right_ascension, mean_anomaly); 

      status = SWwriteattr(sw_id, "Epoch_year", DFNT_INT16, 1, &epoch_year);
      printf ("wrt_navig/writeattr epoch_year %ld\n", status);
      result = result + status;

      status = SWwriteattr(sw_id, "Epoch_day", DFNT_INT16, 1, &epoch_day);
      printf ("wrt_navig/writeattr epoch_day %ld\n", status);
      result = result + status;

      status = SWwriteattr(sw_id, "Epoch_time", DFNT_INT32, 1, &epoch_time);
      printf ("wrt_navig/writeattr epoch_time %ld\n", status);
      result = result + status;

      status = SWwriteattr(sw_id, "semimajor_axis", DFNT_FLOAT64, 1, 
                                                    &semimajor_axis);
      printf("wrt_navig/writeattr semimajor_axis %ld\n",status);
      result = result + status;

      status = SWwriteattr(sw_id, "eccentricity", DFNT_FLOAT64,1,&eccentricity);
      printf("wrt_navig/writeattr eccentricity %ld\n",status);
      result = result + status;

      status = SWwriteattr(sw_id, "inclination", DFNT_FLOAT64,1,&inclination);
      printf("wrt_navig/writeattr inclination  %ld\n",status);
      result = result + status;

      status = SWwriteattr(sw_id, "argument_of_perigee", DFNT_FLOAT64, 1, 
                                                        &argument_of_perigee);
      printf("wrt_navig/writeattr argument_of_perigee%ld\n",status);
      result = result + status;

      status = SWwriteattr(sw_id, "right_ascension", DFNT_FLOAT64, 1, 
                                                     &right_ascension);
      printf("wrt_navig/writeattr right_ascension %ld\n",status);
      result = result + status;

      status = SWwriteattr(sw_id, "mean_anomaly", DFNT_FLOAT64,1,&mean_anomaly);
      printf("wrt_navig/writeattr mean_anomaly %ld\n",status);
      result = result + status;

      return(result);

} /* end of wrt_navig.c */
