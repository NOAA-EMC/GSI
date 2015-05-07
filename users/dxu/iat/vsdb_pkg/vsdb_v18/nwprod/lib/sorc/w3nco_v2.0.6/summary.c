/***************************************************************

This code will make a system call to return various
useful parameters.  When subroutine summary is called, a list
of system resource statistics is printed to stdout.

Users need to place a call to start() at the beginning of the
section of code to be "measured" and a call to summary() at the end.

Use as follows:

call start()
   do stuff
call summary()

Jim Tuccillo August 1999
***************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/utsname.h>
#ifdef _AIX
#include <sys/proc.h>   
#endif
#ifdef __linux__
#include <errno.h>
#include <sys/resource.h>
#endif

/* #include "trace_mpif.h" */

static FILE *fp = NULL;
int numtask, mypid;
int procid_0;
int profile, msglen;
int trace_flag;
double tcpu, twall, tbytes, f_bytes;
double tot_wall, final_wall, start_wall;
double cpu_comm, wall_comm;
#ifdef _AIX
extern double rtc ();
#endif
struct time_data {
     double s_cpu;
     double s_wall;
     double f_cpu;
     double f_wall;
     double c_cpu;
     double c_wall;
     double c_bytes;
     int    c_calls;
     int    c_buckets[32];
     float  c_sum[32];
     double b_cpu[32];
     double b_wall[32];
};

struct time_data MPI_Allgather_data;
struct time_data MPI_Allgatherv_data;
struct time_data MPI_Allreduce_data;
struct time_data MPI_Alltoall_data;
struct time_data MPI_Alltoallv_data;
struct time_data MPI_Barrier_data;
struct time_data MPI_Bcast_data;
struct time_data MPI_Gather_data;
struct time_data MPI_Gatherv_data;
struct time_data MPI_Op_create_data;
struct time_data MPI_Op_free_data;
struct time_data MPI_Reduce_scatter_data;
struct time_data MPI_Reduce_data;
struct time_data MPI_Scan_data;
struct time_data MPI_Scatter_data;
struct time_data MPI_Scatterv_data;
struct time_data MPI_Attr_delete_data;
struct time_data MPI_Attr_get_data;
struct time_data MPI_Attr_put_data;
struct time_data MPI_Comm_compare_data;
struct time_data MPI_Comm_create_data;
struct time_data MPI_Comm_dup_data;
struct time_data MPI_Comm_free_data;
struct time_data MPI_Comm_group_data;
struct time_data MPI_Comm_rank_data;
struct time_data MPI_Comm_remote_group_data;
struct time_data MPI_Comm_remote_size_data;
struct time_data MPI_Comm_size_data;
struct time_data MPI_Comm_split_data;
struct time_data MPI_Comm_test_inter_data;
struct time_data MPI_Group_compare_data;
struct time_data MPI_Group_difference_data;
struct time_data MPI_Group_excl_data;
struct time_data MPI_Group_free_data;
struct time_data MPI_Group_incl_data;
struct time_data MPI_Group_intersection_data;
struct time_data MPI_Group_rank_data;
struct time_data MPI_Group_range_excl_data;
struct time_data MPI_Group_range_incl_data;
struct time_data MPI_Group_size_data;
struct time_data MPI_Group_translate_ranks_data;
struct time_data MPI_Group_union_data;
struct time_data MPI_Intercomm_create_data;
struct time_data MPI_Intercomm_merge_data;
struct time_data MPI_Keyval_create_data;
struct time_data MPI_Keyval_free_data;
struct time_data MPI_Abort_data;
struct time_data MPI_Error_class_data;
struct time_data MPI_Errhandler_create_data;
struct time_data MPI_Errhandler_free_data;
struct time_data MPI_Errhandler_get_data;
struct time_data MPI_Error_string_data;
struct time_data MPI_Errhandler_set_data;
struct time_data MPI_Get_processor_name_data;
struct time_data MPI_Initialized_data;
struct time_data MPI_Wtick_data;
struct time_data MPI_Wtime_data;
struct time_data MPI_Address_data;
struct time_data MPI_Bsend_data;
struct time_data MPI_Bsend_init_data;
struct time_data MPI_Buffer_attach_data;
struct time_data MPI_Buffer_detach_data;
struct time_data MPI_Cancel_data;
struct time_data MPI_Request_free_data;
struct time_data MPI_Recv_init_data;
struct time_data MPI_Send_init_data;
struct time_data MPI_Get_elements_data;
struct time_data MPI_Get_count_data;
struct time_data MPI_Ibsend_data;
struct time_data MPI_Iprobe_data;
struct time_data MPI_Irecv_data;
struct time_data MPI_Irsend_data;
struct time_data MPI_Isend_data;
struct time_data MPI_Issend_data;
struct time_data MPI_Pack_data;
struct time_data MPI_Pack_size_data;
struct time_data MPI_Probe_data;
struct time_data MPI_Recv_data;
struct time_data MPI_Rsend_data;
struct time_data MPI_Rsend_init_data;
struct time_data MPI_Send_data;
struct time_data MPI_Sendrecv_data;
struct time_data MPI_Sendrecv_replace_data;
struct time_data MPI_Ssend_data;
struct time_data MPI_Ssend_init_data;
struct time_data MPI_Start_data;
struct time_data MPI_Startall_data;
struct time_data MPI_Test_data;
struct time_data MPI_Testall_data;
struct time_data MPI_Testany_data;
struct time_data MPI_Test_cancelled_data;
struct time_data MPI_Testsome_data;
struct time_data MPI_Type_commit_data;
struct time_data MPI_Type_contiguous_data;
struct time_data MPI_Type_extent_data;
struct time_data MPI_Type_free_data;
struct time_data MPI_Type_hindexed_data;
struct time_data MPI_Type_hvector_data;
struct time_data MPI_Type_indexed_data;
struct time_data MPI_Type_lb_data;
struct time_data MPI_Type_size_data;
struct time_data MPI_Type_struct_data;
struct time_data MPI_Type_ub_data;
struct time_data MPI_Type_vector_data;
struct time_data MPI_Unpack_data;
struct time_data MPI_Wait_data;
struct time_data MPI_Waitall_data;
struct time_data MPI_Waitany_data;
struct time_data MPI_Waitsome_data;
struct time_data MPI_Cart_coords_data;
struct time_data MPI_Cart_create_data;
struct time_data MPI_Cart_get_data;
struct time_data MPI_Cart_map_data;
struct time_data MPI_Cart_rank_data;
struct time_data MPI_Cart_shift_data;
struct time_data MPI_Cart_sub_data;
struct time_data MPI_Cartdim_get_data;
struct time_data MPI_Dims_create_data;
struct time_data MPI_Graph_create_data;
struct time_data MPI_Graph_get_data;
struct time_data MPI_Graph_map_data;
struct time_data MPI_Graph_neighbors_data;
struct time_data MPI_Graph_neighbors_count_data;
struct time_data MPI_Graphdims_get_data;
struct time_data MPI_Topo_test_data;


int bucket (lng)
   int    lng;
{
   int    i, j;
   if (lng <= 0) {return(0);}
   for (i=1, j=--lng; j>0; ++i) {
      j = j>>1;
   }
   return (i);
}



void elapse (timer)
     double *timer;

{

/*

      typedef struct { unsigned long tv_sec;
               long tv_nsec; } timestruc;

      timestruc TimePointer;
      int ret;

      ret = gettimer (TIMEOFDAY, &TimePointer);
      if (ret != 0) {
            printf ("getttimer FAILED!!!\n");
            printf ("ret = %d\n", ret);
            return;
      }


      *timer = ((double) TimePointer.tv_sec) + (((double) TimePointer.tv_nsec) * ((double) 0.000000001));
      return;

*/
#ifdef _AIX
      *timer = rtc();
#endif
#ifdef __linux__
  struct timeval st;
  if (gettimeofday (&st, NULL) == -1) {
    fprintf (stderr,
             "elapse: gettimeofday: %s.\n",
             strerror (errno));
    *timer = 0.;
  }
  *timer = ((double) st.tv_sec) + 1.e-6 * ((double) st.tv_usec);
#endif

}


void cputim (usr, sys)
     double *usr;
     double *sys;

{

      double real;
      typedef struct { int tms_utime;
                       int tms_stime;
                       int tms_cutime;
                       int tms_cstime; } tms;

      tms Time_buffer;
      int ret;

      ret = times (&Time_buffer);

      real = ((double) ret) * 0.01;

      *usr = ((double) Time_buffer.tms_utime) * 0.01;
      *sys = ((double) Time_buffer.tms_stime) * 0.01;
      return;

}


void start_timer (time)
   struct time_data *time;

{
   double user, sys;
   double wall;

   cputim (&user, &sys);
   elapse (&wall);
   time->s_cpu = user + sys;
   time->s_wall = wall;

   return;
}

void end_timer (time)
   struct time_data *time;

{
   double user, sys;
   double wall;

   cputim (&user, &sys);
   elapse (&wall);
   time->f_cpu = user + sys;
   time->f_wall = wall;
   time->c_cpu += time->f_cpu - time->s_cpu;
   time->c_wall += time->f_wall - time->s_wall;

   return;
}




void resource ()

{

      double usr, sys;
      long data[14];
#ifdef _AIX
      typedef struct {
          int             tv_sec;         /* seconds */
          int             tv_usec;        /* microseconds */
      } timeval;
#endif
      double user, system;
      int ret;

      struct rusage RU;
      ret = getrusage (0, &RU);

      if (ret != 0) {
            printf ("getrusage FAILED!!!\n");
            printf ("ret = %d\n", ret);
            return;
      }


      user = ((double) RU.ru_utime.tv_sec) + (((double) RU.ru_utime.tv_usec) * ((double) 0.000001));
      system = ((double) RU.ru_stime.tv_sec) + (((double) RU.ru_stime.tv_usec) * ((double) 0.000001));


     printf("*****************RESOURCE STATISTICS*******************************\n");
     printf("The total amount of wall time                        = %f\n", tot_wall);
     printf("The total amount of time in user mode                = %f\n", user);
     printf("The total amount of time in sys mode                 = %f\n", system);
#ifdef _AIX
     printf("The maximum resident set size (KB)                   = %d\n", RU.ru_maxrss);
     printf("Average shared memory use in text segment (KB*sec)   = %d\n", RU.ru_ixrss);
     printf("Average unshared memory use in data segment (KB*sec) = %d\n", RU.ru_idrss);
     printf("Average unshared memory use in stack segment(KB*sec) = %d\n", RU.ru_isrss);
     printf("Number of page faults without I/O activity           = %d\n", RU.ru_minflt);
     printf("Number of page faults with I/O activity              = %d\n", RU.ru_majflt);
     printf("Number of times process was swapped out              = %d\n", RU.ru_nswap);
     printf("Number of times filesystem performed INPUT           = %d\n", RU.ru_inblock);
     printf("Number of times filesystem performed OUTPUT          = %d\n", RU.ru_oublock);
     printf("Number of IPC messages sent                          = %d\n", RU.ru_msgsnd);
     printf("Number of IPC messages received                      = %d\n", RU.ru_msgrcv);
     printf("Number of Signals delivered                          = %d\n", RU.ru_nsignals);
     printf("Number of Voluntary Context Switches                 = %d\n", RU.ru_nvcsw);
     printf("Number of InVoluntary Context Switches               = %d\n", RU.ru_nivcsw);
#endif
#ifdef __linux__
     printf ("The maximum resident set size (KB)                   = %ld\n", RU.ru_maxrss);
     printf ("Number of page faults without I/O activity           = %ld\n", RU.ru_minflt);
     printf ("Number of page faults with I/O activity              = %ld\n", RU.ru_majflt);
     printf ("Number of times filesystem performed INPUT           = %ld\n", RU.ru_inblock);
     printf ("Number of times filesystem performed OUTPUT          = %ld\n", RU.ru_oublock);
     printf ("Number of Voluntary Context Switches                 = %ld\n", RU.ru_nvcsw);
     printf ("Number of InVoluntary Context Switches               = %ld\n", RU.ru_nivcsw);
#endif
     printf("*****************END OF RESOURCE STATISTICS*************************\n\n");


      usr = user;
      sys = system;
      data[0] = RU.ru_maxrss;
      data[1] = RU.ru_ixrss;
      data[2] = RU.ru_idrss;
      data[3] = RU.ru_isrss;
      data[4] = RU.ru_minflt;
      data[5] = RU.ru_majflt;
      data[6] = RU.ru_nswap;
      data[7] = RU.ru_inblock;
      data[8] = RU.ru_oublock;
      data[9] = RU.ru_msgsnd;
      data[10] = RU.ru_msgrcv;
      data[11] = RU.ru_nsignals;
      data[12] = RU.ru_nvcsw;
      data[13] = RU.ru_nivcsw;

      return;

}





void print_timing (string, time)
   char             *string;
   struct time_data *time;

{


   if (time->c_calls > 0) {
     fprintf (fp, "Information for %s: AVG. Length = %13.2f, CALLS = %d, WALL = %13.3f, CPU = %13.3f \n",
     string, (double) (time->c_bytes) / (double) time->c_calls, time->c_calls,
                       time->c_wall, time->c_cpu);
   }

   if (time->c_wall > 0.001 ) {
     fprintf (fp, "                %s: Total BYTES = %g, BW = %8.3f MBYTES/WALL SEC., BW = %8.3f MBYTES/CPU SEC.\n",
     string, time->c_bytes,
             ((double) time->c_bytes * 0.000001)/time->c_wall,
             ((double) time->c_bytes * 0.000001)/time->c_cpu);
   }

   twall  += time->c_wall;
   tcpu   += time->c_cpu;
   tbytes += time->c_bytes * 0.000001;

   /* Print the distribution of the message lengths */
   if (time->c_calls > 0) {
     int i, j1, j2;

     j1 = 0; j2 = 0;
     fprintf (fp, "       AVG. Length    # of Calls    MB/WALL Sec.  MB/CPU Sec.   WALL Secs.     CPU Secs.   \n");
     if (time->c_buckets[0] >0) {
      fprintf (fp, " %13.2f %13d    %13.3f %13.3f %13.4f %13.4f \n",
                   time->c_sum[0]/(float)time->c_buckets[0], time->c_buckets[0],
                   ((double) time->c_sum[0] * 0.000001)/time->b_wall[0],
                   ((double) time->c_sum[0] * 0.000001)/time->b_cpu[0],
                   time->b_wall[0], time->b_cpu[0]);
       }
     time->c_buckets[3] = time->c_buckets[1] + time->c_buckets[2] + time->c_buckets[3];
     j1 = 1; j2 = 4;
     for (i =3; i < 31; ++i) {
      if (time->c_buckets[i] > 0) {
      fprintf (fp, " %13.2f %13d    %13.3f %13.3f %13.4f %13.4f \n",
                   time->c_sum[i]/(float)time->c_buckets[i], time->c_buckets[i],
                   ((double) time->c_sum[i] * 0.000001)/time->b_wall[i],
                   ((double) time->c_sum[i] * 0.000001)/time->b_cpu[i],
                   time->b_wall[i], time->b_cpu[i]);
      }
       j1 = j2 +1;
       j2 = j2 + j2;
     }

   fprintf (fp, "\n");

   }

}

#ifdef _AIX
void summary( returnVal  )
int * returnVal;
#endif
#ifdef __linux__
void summary_ (int *returnVal)
#endif
{

  double temp, temp1;
  char trace_file[255], processor[8];

/*
    MPI_Finalize - prototyping replacement for MPI_Finalize
*/

   elapse(&final_wall);
   tot_wall = final_wall - start_wall;


    resource();

   if (fp) fclose (fp);

  return;
}

#ifdef _AIX
void start()
#endif
#ifdef __linux__
void start_ ()
#endif
{
  int stateid;
  int  Argc;
  char **Argv;

  char *answer;
 

  trace_flag=1;

  profile = 0;
  elapse (&start_wall);

  return;
}

