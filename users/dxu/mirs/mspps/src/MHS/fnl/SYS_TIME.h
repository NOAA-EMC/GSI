/***********************************************************************        
 *  Header file name :  SYS_TIME.h     
 *  Programer        :  Bing-Zhang (SMSRC)
 *  Date             :  2/8/95              
 *  Function         :  This header file contants the system date 
 *                      and time.
 ***********************************************************************/ 

/* parameters for system time (processing date/time) */

typedef struct
{
  short int     sys_year, sys_mon,
                sys_mday,  sys_hour,
                sys_min,  sys_sec,
                sys_jday;
}Sys_Time;


