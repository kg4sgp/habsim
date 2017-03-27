#include <stdio.h>
#include <stdint.h>
#include <math.h>

int main(void)
{

  /* constants */
  double air_den = 1.225;
  double g = 9.806;

  /* initial state variables for the balloon */
  double b_mass = 1.0;
  double b_alti = 0.0;
  double b_cd = 0.47;
  double f_lift_kg = 3.0;
  double b_diam = 2.0;
  
  /* calculated state variables for the balloon */
  double b_velo = 0.0;
  double b_acel = 0.0;
  double b_volu = 0.0;
  double b_cross_area = 0.0;
  double f_net = 0.0;
  double f_lift = 0.0;
  double f_drag  = 0.0;
  
  /* time increment in seconds*/
  double t_inc = 0.1;
  double t = 0;
  
  /* force of gravity */
  double f_g = b_mass * g;
  f_lift = f_lift_kg * g;
  
  /* print flight parameters */
  printf("\n");
  printf("Total Mass: %.3fkg\n", b_mass);
  printf("Total Lift: %.3f N (%.3f kg)\n", f_lift, f_lift_kg);
  printf("Diameter: %.3f m\n", b_diam);
  printf("\n");
  
  
  while(b_alti < 30000) {
  
    /* use altitude to calculate cross sectional area of balloon */
     
  
    /* cross sectional area of sphere: A = pi*r^2 */
    b_cross_area = M_PI * pow((b_diam/2), 2);
    /* volume of sphere V = 4/3 * pi *r^3 */ 
    b_volu = (4.0/3.0) * M_PI * pow((b_diam/2), 3);
    
    /* drag force */
    /* https://en.wikipedia.org/wiki/Drag_equation */
    f_drag = (0.5)*air_den*pow(b_velo,2)*b_cd*b_cross_area;
    
    /* net forces */
    f_net = f_lift - (f_g + f_drag);
    
    /* Kenimatic Equations */
    /* https://en.wikipedia.org/wiki/Equations_of_motion */
    b_acel = f_net/b_mass;
    b_velo = b_velo + b_acel*t_inc;
    b_alti = b_alti + b_velo*t_inc + (0.5 * b_acel * pow(t_inc,2));
    
    printf("%f fnet: %f, alt: %f, vel: %f, acel: %f\n", t , f_net, b_alti, b_velo, b_acel);

    t += t_inc;    
   }    
  
  double hours = t/3600.0;
  double min = (hours - floor(hours))*60;
  double sec = (min - floor(min))*60;
  
  printf("Flight time %.0f hours, %.0f min, %f sec.\n", floor(hours), floor(min), sec);
    
  printf("\n");
  return 0;
}
