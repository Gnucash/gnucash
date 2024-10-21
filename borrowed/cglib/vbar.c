#include "cglib.h"
#include "cglib-other.h"

void vbar(struct bar_data* bd)
{
  FILE* file;
  if(bd->general->d_file) file = bd->general->file;
  else file = fopen(bd->general->file_name, "wb");

  fprintf(file, 
    svg_top_header_start, 
    bd->general->viewport_x + bd->general->margin, 
    bd->general->viewport_y + bd->general->margin, 
    bd->general->stroke_width,
    bd->general->margin / 2.0,
    bd->general->margin / 2.0,
    bd->general->viewport_x, 
    bd->general->viewport_y);
  float y_axel_x_offset = (bd->axel_data->h[0] < 0) ? 
                          bd->general->viewport_y * (bd->axel_data->h[0] / (bd->axel_data->h[0] - bd->axel_data->h[1])) : 0;
  float x_axel_y_offset = (bd->axel_data->w[0] < 0) ?
                          bd->general->viewport_x * (bd->axel_data->w[0] / (bd->axel_data->w[0] - bd->axel_data->w[1])) : 0;
  draw_axis_horizontals(file, x_axel_y_offset, y_axel_x_offset, bd->general, bd->axel_data);
  float bar_width = (bd->general->viewport_x - bd->spacing * (bd->n_bars - 1))
                    / bd->n_bars;
  float x_bar_jump = bar_width + bd->spacing;
  float height;
  float height_offset;
  float height_offset_2;
  float middle;
//  uint8_t label_offset_multiplier = 1.0;
  char* ret;
  ret = stringify("text-anchor = \"middle\" font-size=\"%d\"", bd->general->font_size);
  fprintf(file, svg_custom_group, ret);
  free(ret);
  fprintf(file, svg_limiter_box);
  select_color_function(bd->theme);
  loop(bd->n_bars)
  {
    height = (bd->bars[i].value / (bd->axel_data->h[1] - bd->axel_data->h[0])) * bd->general->viewport_y;
    height_offset = (height > 0) ? height : 0;
    height_offset_2 = (height > 0) ? height : height - 20.0;
    height *= (height > 0) ? 1 : -1;
    bd->theme->percentage = (i + 1) / (bd->n_bars* 1.0);
    
    bd->theme->color_function(bd->theme);
    
    fprintf(file, svg_box,
      bd->theme->out.r,
      bd->theme->out.g,
      bd->theme->out.b,
      bd->general->margin / 2.0 + i * x_bar_jump + x_axel_y_offset,
      bd->general->margin / 2.0 + bd->general->viewport_y - y_axel_x_offset - height_offset,
      bar_width,
      height);
    ret = stringify("%9.1f",  bd->bars[i].value);
    middle = (bd->general->margin / 2.0 + i * x_bar_jump + x_axel_y_offset + bar_width / 2);
    fprintf(file, svg_text, 
      middle, 
      bd->general->margin / 2.0 + bd->general->viewport_y - y_axel_x_offset - height_offset_2 - bd->axel_data->axel_number_offset, 
      ret);
    free(ret);
  }
  fprintf(file, svg_group_stop);
//  uint8_t s = 1;
  int8_t m = 1;
  struct group_data* grp_dat = init_group_dat(file, "\0", "dominant-baseline=\"hanging\"");
  select_color_function(bd->theme);
  loop(bd->n_bars)
  {
    height = (bd->bars[i].value / (bd->axel_data->h[1] - bd->axel_data->h[0])) * bd->general->viewport_y;

//printf("height, %f\n", height);

    grp_dat->cmp_1 = make_cmp_1(grp_dat, height <= 0);
    grp_dat->cmp_2 = make_cmp_2(grp_dat, height > 0);
    m = (print_group(grp_dat) == 1) ? -1 : 1; 
    middle = (bd->general->margin / 2.0 + i * x_bar_jump + x_axel_y_offset + bar_width / 2);

//printf("middle, %f\n", middle);
//printf("bd->general->margin, %f\n", bd->general->margin  / 2.0);
//printf("bd->general->viewport_y, %f\n", bd->general->viewport_y);
//printf("y_axel_x_offset, %f\n", y_axel_x_offset);
//printf("bd->axel_data->axel_number_offset, %f\n", bd->axel_data->axel_number_offset);
//printf("m, %d\n", m);
//printf("y %f\n", bd->general->margin / 2.0 + bd->general->viewport_y - y_axel_x_offset + bd->axel_data->axel_number_offset * m);


    fprintf(file, svg_text,
      middle,
      bd->general->margin / 2.0 + bd->general->viewport_y - y_axel_x_offset + bd->axel_data->axel_number_offset * m,
      bd->bars[i].name);
  }
  destroy_group_dat(grp_dat);
  fprintf(file, svg_group_stop);
  fprintf(file, svg_group_stop);
  fprintf(file, svg_top_header_stop);
  if(!bd->general->d_file) fclose(file);
  return;
}
