#include "cglib.h"
#include "cglib-other.h"

void select_color_function(struct theme_data* td)
{
  switch(td->theme_type)
  {
    case 0:
      td->color_function = &get_gradient;
      break;
    /*
    IMPLEMENT HSL TO RGB FUNCTION

    case 1:
      td->color_function = &get_random_hue;
      break;
    */
    case 2:
      td->color_function = &get_random_col;
      break;
    case 3:
      td->color_function = &get_dark_gradient;
      break;
    case 4:
      td->color_function = &get_light_gradient;
    default:
      td->color_function = &get_random_col;
      break;
  }
  return;
}

void get_gradient(struct theme_data* td)
{
  int r_delta = td->stop.r - td->start.r;
  r_delta *= (r_delta < 0) ? -1 : 1;
  int r_dir = (td->start.r > td->stop.r) ? -1 : 1;
  int r_delta_index = r_delta * td->percentage;
  
  int g_delta = td->stop.g - td->start.g;
  g_delta *= (g_delta < 0) ? -1 : 1;
  int g_dir = (td->start.g > td->stop.g) ? -1 : 1;
  int g_delta_index = g_delta * td->percentage;

  int b_delta = td->stop.b - td->start.b;
  b_delta *= (b_delta < 0) ? -1 : 1;
  int b_dir = (td->start.b > td->stop.b) ? -1 : 1;
  int b_delta_index = b_delta * td->percentage;

  td->out.r = td->start.r + r_delta_index * r_dir;
  td->out.g = td->start.g + g_delta_index * g_dir;
  td->out.b = td->start.b + b_delta_index * b_dir;

  return;
}

void get_random_hue(struct theme_data* td)
{
  /* IMPLEMENT HSL -> RGB ALGORITHM*/

  //get rand hue
  //convert hue, sat, and light to rgb
  return;
}

void get_random_col(struct theme_data* td)
{
  td->out.r = rand();
  td->out.g = rand();
  td->out.b = rand();
  return;
}

void get_dark_gradient(struct theme_data* td)
{
  get_gradient(td);
  rgb_darken(&td->out, td->percentage * td->lightness_mod_percentage);
  return;
}

void get_light_gradient(struct theme_data* td)
{
  get_gradient(td);
  rgb_darken(&td->out, td->percentage * td->lightness_mod_percentage);
  return;
}

void rgb_darken(struct rgb* values, float percentage)
{
  percentage = 1.0 - percentage;
  values->r *= percentage;
  values->g *= percentage;
  values->b *= percentage;
  return;
}

void rgb_lighten(struct rgb* values, float percentage)
{
  values->r += ((255 - values->r)) * percentage;
  values->g += ((255 - values->g)) * percentage;
  values->b += ((255 - values->b)) * percentage;
  return;
}