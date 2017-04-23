#include <stdio.h>
#include <stdint.h>
#include <math.h>

typedef struct {
  double pr;
  double dn;
} prdn;

/* https://en.wikipedia.org/wiki/Barometric_formula */
prdn calc_pressure(double alt)
{
  prdn prdn2;

  /* constants */
  double M = 0.0289644;
  double R = 8.3144598;
  double g = 9.80665;
  
  double hb = 0.0;
  double tb = 0.0;
  double lb = 0.0;
  double pb = 0.0;
  double rho = 0.0;
  
  /* set values for different altitude regions */
  if (alt < 11000.0) {
    hb = 0.0;
    tb = 288.15;
    lb = -0.0065;
    pb = 101325.0;
    rho = 1.225;
  } else if (alt <= 20000.0) {
    hb = 11000.0;
    tb = 216.65;
    lb = 0.0;
    pb = 22632.1;
    rho = 0.36391;
  } else if (alt <= 32000.0) {
    hb = 20000.0;
    tb = 216.65;
    lb = 0.0003048;
    pb = 5474.89;
    rho = 0.08803;
  } else if (alt <= 47000.0) {
    hb = 32000.0;
    tb = 228.65;
    lb = 0.00085344;
    pb = 868.02;
    rho = 0.01322;
  } else if (alt <= 51000.0) {
    hb = 47000.0;
    tb = 270.65;
    lb = 0.0;
    pb = 110.91;
    rho = 0.00143;
  } else if (alt <= 71000.0) {
    hb = 51000.0;
    tb = 270.65;
    lb = -0.00085344;
    pb = 66.94;
    rho = 0.00086;
  } else if (alt <= 86000.0) {
    hb = 71000.0;
    tb = 214.65;
    lb = 0.0006096;
    pb = 3.96;
    rho = 0.000064;
  }
  
  /* if temp lapse rate is 0.0 use the first formula, otherwise second */
  if (lb == 0.0) {
    prdn2.pr = pb * exp( (-g * M * (alt-hb))/(R*tb));
  } else {
    prdn2.pr = pb * pow( ( tb / (tb + lb * (alt- hb) ) ), ((g*M)/(R*lb)));
  }
  
  if (lb == 0.0) {
    prdn2.dn = rho * exp( (-g * M * (alt-hb))/(R*tb));
  } else {
    prdn2.dn = rho * pow( ( tb / (tb + lb * (alt- hb) ) ), (1.0+((g*M)/(R*lb))));
  }
  
  
  return prdn2;
}


/* https://en.wikipedia.org/wiki/Barometric_formula */
double calc_temp(double alt)
{
  // Standard atmospheric pressure
  // Below 51 km: Practical Meteorology by Roland Stull, pg 12
  // Above 51 km: http://www.braeunig.us/space/atmmodel.htm
  double geopot_height = alt/1000.0;

  if (geopot_height <= 11.0)          // Troposphere
    return 288.15f - (6.5 * geopot_height);
  else if (geopot_height <= 20.0)     // Stratosphere starts
    return 216.65f;
  else if (geopot_height <= 32.0)
    return 196.65f + geopot_height;
  else if (geopot_height <= 47.0)
    return 228.65f + 2.8 * (geopot_height - 32.0);
  else if (geopot_height <= 51.0)     // Mesosphere starts
    return 270.65f;
  else if (geopot_height <= 71.0)
    return 270.65f - 2.8 * (geopot_height - 51.0);
  else if (geopot_height <= 84.85)
    return 214.65f - 2.0 * (geopot_height - 71.0);
  // Thermosphere has high kinetic temperature (500 C to 2000 C) but temperature
  // as measured by a thermometer would be very low because of almost vacuum.


}

double gas_dens(double molmass, double pressure, double temperature)
{
  /* universal gas constant */
  double r = 8.3144598; 
  
  double gd = ((molmass*pressure)/(r*temperature));
  return gd;
}

double boyancy(double p_air, double p_gas, double gaccel, double volume)
{
  double fb = ((p_air-p_gas)*gaccel*volume);
  return fb;
}

int main(void)
{

  /* printing */
  int print_deci = 1000;
  int print_count = -1;

  /* constants */
  double g = 9.80665;
  double mm_h2 = 2.01588E-3;

  /* initial state variables for the balloon */
  double b_mass = 4.0;
  double b_alti = 0.0;
  double b_cd = 0.47;
  double f_lift_kg = 2.62;
  double b_diam = 2.0;
  double p_cd = 1.0;
  double p_cross_area = 0.22;
  double b_burst_dia = 5.0;
  double land_ele = 300.0;
  double f_boyant = 0.0;
  
  /* calculated state variables for the balloon */
  double b_velo = 0.0;
  double b_acel = 0.0;
  double b_volu = 0.0;
  double b_cross_area = 0.0;
  double b_pres = 0.0;
  double b_radius = 0.0;
  double f_net = 0.0;
  double f_lift = 0.0;
  double f_drag  = 0.0;
  double a_temp = 0.0;
  prdn prdn1;  
  /* time increment in seconds*/
  double t_inc = 0.01;
  double t = 0.0;
  double adabatic_const = 0.0;
  double adabatic_temp = 0.0;
  double h2_den = 0.0;
  
  /* Simulation results */
  double max_alti = 0.0;
  
  /* calculate some inital things*/
  b_alti = land_ele;
  double f_g = b_mass * g;
  f_lift = f_lift_kg * g;
  /* volume of sphere V = 4/3 * pi *r^3 */ 
  b_volu = (4.0/3.0) * M_PI * pow((b_diam/2), 3);
  prdn1 = calc_pressure(b_alti);
  b_pres = prdn1.pr;
  a_temp = calc_temp(b_alti);
  adabatic_const = (b_pres*b_volu)/a_temp;
  
  
  /* print flight parameters */
  printf("\n");
  printf("Total Mass: %.3fkg\n", b_mass);
  printf("Total Lift: %.3f N (%.3f kg)\n", f_lift, f_lift_kg);
  printf("Diameter: %.3f m\n", b_diam);
  printf("\n");
  
  // printf("seconds, fnet, fdrag, altitude, velocity, acceleration, pressure, radius, volume\n");
  while(b_radius < b_burst_dia) {
  
    /* calculate pressure at altitude */
    prdn prdn_new = calc_pressure(b_alti);
  
    /* calculate new voume of balloon */
    b_volu = (b_pres*b_volu)/prdn_new.pr;
    b_pres = prdn_new.pr;
    
    /* calculate radius from volume */
    b_radius = cbrt((3.0*b_volu)/(4.0*M_PI));
  
    /* cross sectional area of sphere: A = pi*r^2 */
    b_cross_area = M_PI * pow(b_radius, 2);
    
    a_temp = calc_temp(b_alti);
    adabatic_temp = (b_pres*b_volu)/adabatic_const;
    
    /* calculate h2 density */
    h2_den = gas_dens(mm_h2, b_pres, a_temp);
    
    /* calculate boyant force */
    f_boyant = boyancy(prdn_new.dn, h2_den, g, b_volu);
    
    /* drag force - ascending balloon*/
    /* https://en.wikipedia.org/wiki/Drag_equation */
    f_drag = (0.5)*prdn_new.dn*pow(b_velo,2)*b_cd*b_cross_area;
    
    /* net forces */
    f_net = f_boyant - (f_g + f_drag);
    

    
    /* Kenimatic Equations */
    /* https://en.wikipedia.org/wiki/Equations_of_motion */
    b_acel = f_net/b_mass;
    b_velo = b_velo + b_acel*t_inc;
    b_alti = b_alti + b_velo*t_inc + (0.5 * b_acel * pow(t_inc,2));
    
    print_count++;
    if (print_count == print_deci){
      //printf("%.1f, %f, %f, %f, %f, %f, %f\n", t, b_alti, b_pres, a_temp, h2_den, prdn_new.dn, f_boyant);
      printf("%.1f, %f, %f, %f, %f, %f, %f, %f\n", t, b_alti, b_velo, b_acel, b_pres, b_radius, f_net, f_boyant);
      print_count = 0;
    }
    
    if (b_alti > max_alti) max_alti = b_alti;

    t += t_inc;    
   }   
  
  double t_ascent = t;
  double ahours = t_ascent/3600.0;
  double amin = (ahours - floor(ahours))*60;
  double asec = (amin - floor(amin))*60;
  
  /* calculate decent phase of flight */
  print_count = 0;
  
  while(b_alti > land_ele) {
    /* calculate pressure at altitude */
    prdn prdn_new = calc_pressure(b_alti);
    
    /* drag force - parachute*/
    /* https://en.wikipedia.org/wiki/Drag_equation */
    f_drag = (0.5)*prdn_new.dn*pow(b_velo,2)*p_cd*p_cross_area;
    
    /* net forces */
    f_net = f_drag - f_g;
    
    a_temp = calc_temp(b_alti);
    
    /* Kenimatic Equations */
    /* https://en.wikipedia.org/wiki/Equations_of_motion */
    b_acel = f_net/b_mass;
    b_velo = b_velo + b_acel*t_inc;
    b_alti = b_alti + b_velo*t_inc + (0.5 * b_acel * pow(t_inc,2));
    
    print_count++;
    if (print_count == print_deci){
      //printf("%.1f, %f, %f, %f, %f, %f, %f, %f\n", t , f_net, f_drag, b_alti, b_velo, b_acel, b_pres, a_temp);
      print_count = 0;
    }
    
    t += t_inc;   
    
  }
  
  double t_decent = t - t_ascent;
  double dhours = t_decent/3600.0;
  double dmin = (dhours - floor(dhours))*60;
  double dsec = (dmin - floor(dmin))*60;
  
  double t_total = t;
  double hours = t_total/3600.0;
  double min = (hours - floor(hours))*60;
  double sec = (min - floor(min))*60;
  
  printf("Maximum Altitude: %.1f meters.\n", max_alti);
  printf("Ascent duration %.0f hours, %.0f min, %.2f sec.\n", floor(ahours), floor(amin), asec);
  printf("Decent duration %.0f hours, %.0f min, %.2f sec.\n", floor(dhours), floor(dmin), dsec);
  printf("Flight duration %.0f hours, %.0f min, %.2f sec.\n", floor(hours), floor(min), sec);
    
  printf("\n");
  return 0;
}
