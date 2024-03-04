#include "cglib.h"
#include "cglib-other.h"

static void print_slice(FILE* file, struct pie_data* pd, float* origin, float* sum_counter, int i, float radius, float sum, uint8_t overlap)
{
  float percentage_overlap = ((i + 1 < pd->n_slices) && overlap) ? pd->slices[i + 1].percentage / 2 : 0;
  percentage_overlap = (percentage_overlap == 0 && sum == 1.0 && overlap) ? pd->slices[0].percentage / 2 : percentage_overlap;
  float radians[2];
  radians[0] = 2 * (*sum_counter)*PI;
  radians[1] = 2 * ((pd->slices[i].percentage + percentage_overlap) + (*sum_counter))*PI;
  *sum_counter   += pd->slices[i].percentage;
  uint8_t large_arc_flag = (pd->slices[i].percentage + percentage_overlap > 0.5) ? 1 : 0;

  float start[2], stop[2];
  start[0] = cos(radians[0]) * radius + origin[0];
  start[1] = sin(radians[0]) * radius + origin[1];
  stop[0] =  cos(radians[1]) * radius + origin[0];
  stop[1] =  sin(radians[1]) * radius + origin[1];

  if (pd->theme)
  {
    pd->theme->percentage = (i + 1) / (pd->n_slices*1.0);
    pd->theme->color_function(pd->theme);

    fprintf(file, svg_slice,
      start[0],
      start[1],
      radius,
      radius,
      large_arc_flag,
      stop[0],
      stop[1],
      origin[0],
      origin[1],
      pd->theme->out.r,
      pd->theme->out.g,
      pd->theme->out.b);
  }
  else
  {
    fprintf(file, svg_slice,
      start[0],
      start[1],
      radius,
      radius,
      large_arc_flag,
      stop[0],
      stop[1],
      origin[0],
      origin[1],
      pd->slices[i].color.r,
      pd->slices[i].color.g,
      pd->slices[i].color.b);
  }
  return;
}

void pie(struct pie_data* pd)
{
  FILE* file;
  if(pd->general->d_file) file = pd->general->file;
  else file = fopen(pd->general->file_name, "wb");

//  uint8_t large_arc_flag;
//  float percentage_overlap, 
    float radius;
    float origin[2]; 
//        radians[3], 
//        start[2], 
//        stop[2],
    float origin_radius;

  origin_radius = 0.0;
  
  print_top_header(file, pd->general);
  radius = (pd->general->viewport_y <= pd->general->viewport_x) ? (pd->general->viewport_y - pd->general->margin) / 2 : (pd->general->viewport_x - pd->general->margin) / 2;
//  origin[0] = pd->general->viewport_x / 2.0 + pd->general->margin / 2.0;
  origin[0] = radius + pd->general->margin / 2.0;
  origin[1] = pd->general->viewport_y / 2.0 + pd->general->margin / 2.0;

  fprintf(file, svg_circle, origin[0], origin[1], origin_radius);

  float sum = 0.0;

  if (pd->theme)
    select_color_function(pd->theme);

  loop(pd->n_slices)
  {
    sum += pd->slices[i].percentage;
  }

  sum = roundf(sum * 100) / 100;

  print_font_size_group(file, pd->general);
  valid_pie_data(pd, sum)
  {
    float sum_counter;

    fprintf(file, svg_limiter_box);
    loop(pd->n_slices)
    {
      print_slice(file, pd, origin, &sum_counter, i, radius, sum, 1);
    }
    if(sum == 1.0)
    {
      sum_counter = 0.0;
      print_slice(file, pd, origin, &sum_counter, 0, radius, sum, 0);
    }
    print_slice_pointers(file, pd);
    fprintf(file, svg_group_stop);
  }
  fprintf(file, svg_group_stop);
  fprintf(file, svg_top_header_stop);
  if(!pd->general->d_file) fclose(file);
}
