#include "cglib.h"
#include "cglib-other.h"

static void print_section(FILE* file, struct pie_data* pd, float* origin, float* sum_counter, int i, float radius, float sum, uint8_t overlap)
{
  float radius_divisior = 1.5;
  float percentage_overlap = ((i + 1 < pd->n_slices) && overlap) ? pd->slices[i + 1].percentage / 2 : 0;
  percentage_overlap = (percentage_overlap == 0 && sum == 1.0 && overlap) ? pd->slices[0].percentage / 2 : percentage_overlap;
  uint8_t large_arc_flag  = (pd->slices[i].percentage + percentage_overlap > 0.5) ? 1 : 0;
  float radians_1 = 2 * (*sum_counter) * PI;
  float radians_2 = 2 * ((pd->slices[i].percentage + percentage_overlap) + (*sum_counter)) * PI;
  *sum_counter   += pd->slices[i].percentage;
  float x_0 = cos(radians_1) * radius + origin[0];
  float y_0 = sin(radians_1) * radius + origin[1];
  float x_1 = cos(radians_2) * radius + origin[0];
  float y_1 = sin(radians_2) * radius + origin[1];
  float x_2 = cos(radians_2) * (radius / radius_divisior) + origin[0];
  float y_2 = sin(radians_2) * (radius / radius_divisior) + origin[1];
  float x_3 = cos(radians_1) * (radius / radius_divisior) + origin[0];
  float y_3 = sin(radians_1) * (radius / radius_divisior) + origin[1];
  pd->theme->percentage = (i + 1) / (pd->n_slices * 1.0);
  pd->theme->color_function(pd->theme);
  fprintf(file, svg_doughnut_section,
    x_0,
    y_0,
    radius,
    radius,
    large_arc_flag,
    x_1,
    y_1,
    x_2,
    y_2,
    radius / radius_divisior,
    radius / radius_divisior,
    large_arc_flag,
    x_3,
    y_3,
    pd->theme->out.r,
    pd->theme->out.g,
    pd->theme->out.b);
  return;
}

void doughnut(struct pie_data* pd)
{
  FILE* file;
  if(pd->general->d_file) file = pd->general->file;
  else file = fopen(pd->general->file_name, "wb");
  
  print_top_header(file, pd->general);
  print_font_size_group(file, pd->general);
  float radius = (pd->general->viewport_y <= pd->general->viewport_x) ? (pd->general->viewport_y - pd->general->margin) / 2 : (pd->general->viewport_x - pd->general->margin) / 2;
  float origin[2];
  origin[0] = pd->general->viewport_x / 2.0 + pd->general->margin / 2.0;
  origin[1] = pd->general->viewport_y / 2.0 + pd->general->margin / 2.0;
  float sum = 0.0;
  loop(pd->n_slices)
  {
    sum += pd->slices[i].percentage;
  }
//  uint8_t large_arc_flag;
  float sum_counter;
  char* ret;
//  float current_pointer_len;
//  uint8_t calculated_rgb[3];
  valid_pie_data(pd, roundf(sum * 100) / 100)
  {
    sum_counter = 0.0;
    fprintf(file, svg_limiter_box);
    select_color_function(pd->theme);
    loop(pd->n_slices)
    {
      print_section(file, pd, origin, &sum_counter, i, radius, sum, 1);
    }
    if(sum == 1.0)
    {
      sum_counter = 0.0;
      print_section(file, pd, origin, &sum_counter, 0, radius, sum, 0);
    }
    print_slice_pointers(file, pd);
    fprintf(file, svg_group_stop);
    //ARBRITARY VALUES HERE, MUST BE ELIMINATED LATER
    ret = stringify("text-anchor=\"middle\" font-size=\"%d\"", pd->d_h1_font_size);
    fprintf(file, svg_custom_group, ret);
    free(ret);
    fprintf(file, svg_text,
      origin[0],
      origin[1],
      pd->doughnut_header);
    ret = stringify("dominant-baseline=\"hanging\" font-size=\"%d\"", pd->d_h2_font_size);
    fprintf(file, svg_custom_group, ret);
    free(ret);
    fprintf(file, svg_text,
      origin[0],
      origin[1] + pd->axel_data->axel_number_offset,
      pd->doughnut_sub_header);
    fprintf(file, svg_group_stop);
    fprintf(file, svg_group_stop);
  }
  else
  {
    printf("error slices do not add up to 100%%, but %9.6f%%\n", sum);
  }
  fprintf(file, svg_group_stop);
  fprintf(file, svg_top_header_stop);
  if(!pd->general->d_file) fclose(file);
}
