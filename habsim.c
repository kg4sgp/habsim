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

int main(void)
{

  /* constants */
  double air_den = 1.225;
  double g = 9.80665;

  /* initial state variables for the balloon */
  double b_mass = 1.0;
  double b_alti = 0.0;
  double b_cd = 0.47;
  double f_lift_kg = 2.62;
  double b_diam = 2.0;
  double p_cd = 1.0;
  double p_cross_area = 0.22;
  double b_burst_dia = 5.0;
  double land_ele = 300.0;
  
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
  prdn prdn1;  
  /* time increment in seconds*/
  double t_inc = 0.01;
  double t = 0.0;
  
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
  air_den = prdn1.dn;
  
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
    
    /* use altitude to calculate cross sectional area of balloon */
    b_radius = cbrt((3.0*b_volu)/(4.0*M_PI));
  
    /* cross sectional area of sphere: A = pi*r^2 */
    b_cross_area = M_PI * pow(b_radius, 2);
    
    /* drag force - ascending balloon*/
    /* https://en.wikipedia.org/wiki/Drag_equation */
    f_drag = (0.5)*prdn_new.dn*pow(b_velo,2)*b_cd*b_cross_area;
    
    /* net forces */
    f_net = f_lift - (f_g + f_drag);
    
    /* Kenimatic Equations */
    /* https://en.wikipedia.org/wiki/Equations_of_motion */
    b_acel = f_net/b_mass;
    b_velo = b_velo + b_acel*t_inc;
    b_alti = b_alti + b_velo*t_inc + (0.5 * b_acel * pow(t_inc,2));
    
    // printf("%f, %f, %f, %f, %f, %f, %f, %f, %f\n", t , f_net, f_drag, b_alti, b_velo, b_acel, b_pres, b_radius, b_volu);
    
    if (b_alti > max_alti) max_alti = b_alti;

    t += t_inc;    
   }   
  
  double t_ascent = t;
  double ahours = t_ascent/3600.0;
  double amin = (ahours - floor(ahours))*60;
  double asec = (amin - floor(amin))*60;
  
  /* calculate descent phase of flight */
  
  while(b_alti > land_ele) {
    /* calculate pressure at altitude */
    prdn prdn_new = calc_pressure(b_alti);
    
    /* drag force - parachute*/
    /* https://en.wikipedia.org/wiki/Drag_equation */
    f_drag = (0.5)*prdn_new.dn*pow(b_velo,2)*p_cd*p_cross_area;
    
    /* net forces */
    f_net = f_drag - f_g;
    
    /* Kenimatic Equations */
    /* https://en.wikipedia.org/wiki/Equations_of_motion */
    b_acel = f_net/b_mass;
    b_velo = b_velo + b_acel*t_inc;
    b_alti = b_alti + b_velo*t_inc + (0.5 * b_acel * pow(t_inc,2));
    
    // printf("%f, %f, %f, %f, %f, %f, %f\n", t , f_net, f_drag, b_alti, b_velo, b_acel, b_pres);

    t += t_inc;   
    
  }
  
  double t_descent = t - t_ascent;
  double dhours = t_descent/3600.0;
  double dmin = (dhours - floor(dhours))*60;
  double dsec = (dmin - floor(dmin))*60;
  
  double t_total = t;
  double hours = t_total/3600.0;
  double min = (hours - floor(hours))*60;
  double sec = (min - floor(min))*60;
  
  printf("Maximum Altitude: %.1f meters.\n", max_alti);
  printf("Ascent duration %.0f hours, %.0f min, %.2f sec.\n", floor(ahours), floor(amin), asec);
  printf("Descent duration %.0f hours, %.0f min, %.2f sec.\n", floor(dhours), floor(dmin), dsec);
  printf("Flight duration %.0f hours, %.0f min, %.2f sec.\n", floor(hours), floor(min), sec);
    
  printf("\n");
  return 0;
}
