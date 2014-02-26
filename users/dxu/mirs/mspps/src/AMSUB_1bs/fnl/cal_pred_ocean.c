/***********************************************************************
 *  Program Name      : cal_pred_ocean.c
 *  Type              : Subroutine
 *  Function          : Program calculates cloud base brightness
 *                      temperature over ocean 
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : emissivity.c 
 *  Called by         : icewp.c
 *
 *  05/09/2007        : algorithm enhancement (ccr3135)
 ***********************************************************************/ 
#include "BSWATH_INCLUDE.h"
#include "ESWATH.h"

/******************************************************/ 
void emissivity(float s0, float s1, float s2, float s3, float s4, float s5[2]); 

void cal_pred_ocean(short int iscan, short int ifov, short int ii)
{
    short int	ilat, ilon, hr, min;

    float 	tu0_89, tul_89, tuv_89;
    float 	tu0_150, tul_150, tuv_150;
    float	ts, tpwg, wind_u, wind_v, wind; 
    float	trans_89, trans_150;
    float	kl_89, kl_150;
    float	blza, aclw, atpw;
    float	mu, em89hv[2], em150hv[2], theta, sintheta, sintheta2, costheta2;
    float	em89, em150; 
    float       interpt;

    float	kv_89 = 0.0115839;
    float	kv_150 = 0.029519;

    float	al_89 = 1.03486;
    float	bl_89 = -0.0097151;
    float	cl_89 = -0.000065914;

    float	a0_89 = 0.108333;
    float	b0_89 = -0.000221042;

    float	al_150 = 1.7129;
    float	bl_150 = 0.0051329;
    float	cl_150 = -0.00022475;

    float	a0_150 = 0.030698;
    float	b0_150 = -0.000071433;

    blza = MISSING;
    aclw = MISSING;
    atpw = MISSING;
    theta = MISSING;
    mu = MISSING;
    sintheta = MISSING;
    sintheta2 = MISSING;
    costheta2 = MISSING;
    ts = MISSING;
    tpwg = MISSING;
    wind_u = MISSING;
    wind_v = MISSING;
    wind = MISSING;

    tu0_89 = MISSING;
    tu0_150 = MISSING;
    tul_89 = MISSING; 
    tul_150 = MISSING; 
    tuv_89 = MISSING;
    tuv_150 = MISSING; 

    trans_89 = MISSING;
    trans_150 = MISSING; 

    em89 = MISSING;
    em150 = MISSING;

    pred89 = MISSING;
    pred150 = MISSING;
    
/*---------------------------------------*
 * Define parameters 
 *---------------------------------------*/
    ilat = 90.0 - lat[iscan][ifov] + 0.5;
    ilon = 180.0 + lon[iscan][ifov] + 0.5;
    if(ilat < 0) 
      ilat = 0; 
    if(ilat > NUMROW_GDAS - 1)
      ilat = NUMROW_GDAS - 1;
    if(ilon < 0) 
      ilon = 0; 
    if(ilon > NUMCOL_GDAS - 1)
      ilon = NUMCOL_GDAS - 1;

    hr = hour[iscan];
    min = minute[iscan];

    if(avnhr[ii] == 24 && hr < 3)
      interpt = (hr + min/60.)/3.;
    else
      interpt = (hr - avnhr[ii] + min/60.)/3.;

    ts = ts_avn[ii][ilat][ilon] + (ts_avn[ii+1][ilat][ilon]-ts_avn[ii][ilat][ilon])*interpt;
    tpwg = tpw_avn[ii][ilat][ilon] + (tpw_avn[ii+1][ilat][ilon]-tpw_avn[ii][ilat][ilon])*interpt;
    wind_u = windu_avn[ii][ilat][ilon] + (windu_avn[ii+1][ilat][ilon]-windu_avn[ii][ilat][ilon])*interpt;
    wind_v = windv_avn[ii][ilat][ilon] + (windv_avn[ii+1][ilat][ilon]-windv_avn[ii][ilat][ilon])*interpt;
    wind = sqrt(wind_u*wind_u + wind_v*wind_v);
 
    blza = lza[iscan][ifov]; 
    theta = blza * PI / 180.;

    if(clw_AB[iscan][ifov] >= 0)
       aclw = clw_AB[iscan][ifov]*1./CLW_SCAL;
    if(tpw_AB[iscan][ifov] >= 0)
       atpw = tpw_AB[iscan][ifov]*1./TPW_SCAL;

/*---------------------------------------*
 * Calculate cloud base BT 
 *---------------------------------------*/
    kl_89 = al_89 + bl_89 * T_CLOUD_LAYER + cl_89 * pow(T_CLOUD_LAYER, 2); 
    kl_150 = al_150 + bl_150 * T_CLOUD_LAYER + cl_150 * pow(T_CLOUD_LAYER, 2); 

    if(ts != MISSING)
    {
      tu0_89 = a0_89 + b0_89 * ts;
      tu0_150 = a0_150 + b0_150 * ts;
    }

    if(aclw < 0.0)
      aclw = 0.;
    tul_89 = kl_89 * aclw;
    tul_150 = kl_150 * aclw;

    if(atpw < 0.)
      atpw = tpwg;
    tuv_89 = kv_89 * atpw;
    tuv_150 = kv_150 * atpw;

    if(ts != MISSING && wind != MISSING && blza != MISSING)
    {
      sintheta = sin(theta)* REARTH/(REARTH + RSAT);
      sintheta2 = pow(sintheta, 2);
      costheta2 = 1.0 - sintheta2;

      emissivity(wind, theta, ts, SALINITY, 89.e9, em89hv);
      em89 = costheta2*em89hv[1] + sintheta2*em89hv[0];
      emissivity(wind, theta, ts, SALINITY, 150.e9, em150hv);
      em150 = costheta2*em150hv[1] + sintheta2*em150hv[0];
    }
   
    mu = cos(theta);

    if(tu0_89 != MISSING && tuv_89 != MISSING && tul_89 != MISSING && mu != MISSING)
      trans_89 = exp(-(tu0_89 + tuv_89 + tul_89)/mu);

    if(trans_89 != MISSING && ts != MISSING && em89 != MISSING)
      pred89 = ts * (1 - (1 - em89) * pow(trans_89, 2));


    if(tu0_150 != MISSING && tuv_150 != MISSING && tul_150 != MISSING && mu != MISSING)
      trans_150 = exp(-(tu0_150 + tuv_150 + tul_150)/mu);

    if(trans_150 != MISSING && ts != MISSING && em150 != MISSING)
      pred150 = ts * (1 - (1 - em150) * pow(trans_150, 2));

} /* end of cal_pred_ocean.c */

/***********************************************************************
 *  Program Name       : emissivity.c
 *  Type               : Subroutine
 *  Function           : Program calculates emissivity
 *  Input Files        : None
 *  Output Files       : None
 *  Subroutines called : epsilon.c 
 *  Called by          : cal_pred_ocean.c
 ***********************************************************************/

void epsilon(float s0, float s1, float s2, double *s3, double *s4);

void emissivity(float wind, float angle, float t, float s, float f, float em_vector[2])

{

/*
;     Variables: angle = incident angle in radians (input)
;         t     = temperature (K)
;         s     = sea water salinity (per thousand)
;         f     = frequency (Hz)
;         wind  = wind speed (m/s)
;
;     Internal Variables
;
;         foam  = foam fraction
;         g,tr  = empirical functions for wind induced
;                 changes in reflection coefficient
;
;     Output
;         Emissivity vector (eh, ev)
;
;     Literature Sources:
;
;     (1) Calm water emissvity
;         Klein and Swift (KS) emissivity for calm water (1977) IEEE Trans.
;         Antennas Propag., 25, 104-111
;     (2) Roughtness correction part for emissivity
;         Francies et al. 1983
;         Satellite microwave remote sensing
;         Observations was made by Hollinger (1971), HO71
;     (3) Foam emissivity is based on
;         Stogryn (1972). JGR, 77, 641-653, ST72
;------------------------------------------------------------------- */
  
  
  float  pi, g, tr, ref, rh, rv, rclear, rfoam;
  
  float foam,  degre,  degre2,  degre3,  degre10, xx, yy, rr, dd; 
  double  tmp;

  struct complx {
        double x;
        double y;
  }; 
  struct complx eps = {0.0, 0.0};

  struct complx aid1 = {0.0, 0.0};

  struct complx aid2 = {0.0, 0.0};

  struct complx aid3 = {0.0, 0.0};
  
  struct complx aid = {0.0, 0.0};
 
  pi = 3.1415926;

  em_vector[0]= 1.0;

  em_vector[1]= 1.0;


  epsilon(t, s, f, &eps.x, &eps.y); 

  /* eps is the complex dielectric constant of saltwater */

  degre = angle/pi * 180.0;
  
  xx = eps.x - pow(sin(angle),2);

  yy = eps.y; 
  
  dd = atan2(yy, xx)/2.0; 
  
  rr = pow( (pow(xx,2) + pow(yy,2)), 0.25);
  
  aid1.x = rr*cos(dd);
  
  aid1.y = rr*sin(dd);

  /* Fractional amount of foam coverage */

  foam = 7.751e-6 * pow(wind, .231);

  if (foam < 0.0) foam = 0.0;

  if (foam > 1.0) foam = 1.0;

  /* Compute the emissivity for horizontal polarization */

  aid2.x = cos(angle) - aid1.x;
  aid2.y = - aid1.y;

  aid3.x = cos(angle) + aid1.x;
  aid3.y = aid1.y;

  degre2 = pow(degre, 2);

  degre3 = pow(degre, 3);
 
  degre10 = pow(degre, 10);


  /* Empirical functions for wind induced reflection change */

  
  g = 1.0 - 1.748e-3 * degre - 7.336e-5 * degre2 + 1.044e-7 *degre3;

  tr = wind * (1.15e-1 + 3.8e-5 * degre2 ) * sqrt(f*1.0e-9);

  aid.x = (aid2.x * aid3.x + aid2.y * aid3.y)/(pow(aid3.x, 2) + pow(aid3.y,2));
  
  aid.y = (aid2.y * aid3.x - aid2.x * aid3.y)/(pow(aid3.x, 2) + pow(aid3.y,2));

  tmp = sqrt(aid.x * aid.x + aid.y * aid.y); 
  ref = pow( tmp,2);

  rclear = ref - tr / t;

 /* Reflection coeff. of foam covered sea surface */

  rfoam = 1.0 - (208.0 + 1.29e-9 * f)/ t*g;

 /* Linear interpolation between foam free and foam covered reflection coeff.*/

  rh = ( 1.0 - foam) * rclear + foam * rfoam;

  if (rh > 1.0) rh = 1.0;
  
  if (rh < 0.0) rh = 0.0;

  em_vector[0] = 1.0 - rh;

  /* Compute the emissivity for vertical polarization */

  aid2.x = eps.x * cos(angle) - aid1.x;

  aid2.y = eps.y * cos(angle) - aid1.y;

  aid3.x = eps.x * cos(angle) + aid1.x;

  aid3.y = eps.y * cos(angle) + aid1.y;

  /* Empirical functions for wind induced reflection changes */

  g  = 1.0 - 9.946e-4 * degre + 3.218e-5 * degre2 - 
       1.187e-6 * degre3 + 7.e-20*degre10;
  
  tr = wind*(1.17e-1-2.09e-3*exp(7.32e-2*degre)) *sqrt(f*1.0e-9);


  aid.x = (aid2.x * aid3.x + aid2.y * aid3.y)/(pow(aid3.x, 2) + pow(aid3.y,2));
  
  aid.y = (aid2.y * aid3.x - aid2.x * aid3.y)/(pow(aid3.x, 2) + pow(aid3.y,2));


  tmp = sqrt(aid.x * aid.x + aid.y * aid.y); 
  ref = pow( tmp,2);

  rclear = ref - tr / t;

 /*  Reflection coeff. of foam covered sea surface */

  rfoam = 1.0 - (208.0 + 1.29e-9 * f)/ t*g;

 /* Linear interpolation between foam free and foam covered reflection coeff.*/

  rv = ( 1.0 - foam) * rclear + foam * rfoam;

  if (rv > 1.0) rv = 1.0;

  if (rv < 0.0) rv = 0.0;
  
  em_vector[1] = 1.0 - rv;

} /* end of emissivity */

/***********************************************************************
 *  Program Name       : epsilon.c
 *  Type               : Subroutine
 *  Function           : Program calculates the dialectric constant
 *			 of saltwater 
 *  Input Files        : None
 *  Output Files       : None
 *  Subroutines called : None 
 *  Called by          : emissivity.c
 ***********************************************************************/
void epsilon(float t1, float s, float f,  double *i, double *j)
{

/*

; Reference:
;
; Microwave remote sensing by Ulaby et al (1984)  pp 2022

; T1    Water Skin temperature (K)
; S     Salinity  Parts per thousand
; F     Frequency (Hz)

 */

  float eo, esw, eswo, eswi, a, tswo, tsw, b, sswo, d, fi, ssw;
  float t, t2, t3, s2, s3, pi, epsp, epspp;	  

  pi = 3.1415926;

  t    = t1 - 273.0;

  t2 = t*t;

  t3 = pow(t, 3);
  
  s2 = s*s;

  s3 = pow(s, 3);
  
  eswi = 4.9;

  eo   = 8.854e-12;

  eswo = 87.134 - 1.949e-1 * t - 1.276e-2 * t2 + 2.491e-4 * t3;

  a = 1.0 + 1.613e-5 * t*s - 3.656e-3 * s + 3.210e-5 * s2 - 4.232e-7 * s3;
  
  esw = eswo * a;

  tswo = 1.1109e-10 - 3.824e-12 * t + 6.938e-14 * t2 - 5.096e-16 * t3;

  b = 1.0 + 2.282e-5 * t * s - 7.638e-4 * s - 7.760e-6 * s2 + 1.105e-8 * s3;
  
  tsw = tswo*b;

  epsp = eswi + (esw - eswi)/ (1.0 + pow( f * tsw, 2) );

  sswo = s*(0.18252-1.4619e-3*s+2.093e-5*s2-1.282e-7*s3);
  
  d  = 25.0 - t;
  
  fi = d * (2.033e-2 + 1.266e-4 * d + 2.464e-6 * d * d -   
       s * (1.849e-5 - 2.551e-7 * d + 2.551e-8 * d * d)); 
  
  ssw = sswo * exp(-fi);
  
  epspp = tsw * f * (esw - eswi) / (1.0 + pow (tsw * f, 2) );
  
  epspp = epspp + ssw/(2.0 * pi * eo * f);

  *i =   epsp;
  
  *j = - epspp;

} /* end of epsilon.c */


