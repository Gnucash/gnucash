#include "cglib.h"
#include "cglib-other.h"

void graph(struct graph_data* gd)
{
  FILE* file;
  if(gd->general->d_file) file = gd->general->file;
  else file = fopen(gd->general->file_name, "wb");

  fprintf(file, 
    svg_top_header_start, 
    gd->general->viewport_x + gd->general->margin, 
    gd->general->viewport_y + gd->general->margin, 
    gd->general->stroke_width,
    gd->general->margin / 2.0,
    gd->general->margin / 2.0,
    gd->general->viewport_x, 
    gd->general->viewport_y);

  char* format;

//  float yOffset = gd->general->viewport_y;

  float y_axel_x_offset = (gd->axel_data->h[0] < 0) ? 
                          gd->general->viewport_y * (gd->axel_data->h[0] / (gd->axel_data->h[0] - gd->axel_data->h[1])) : 0;

  float x_axel_y_offset = (gd->axel_data->w[0] < 0) ?
                          gd->general->viewport_x * (gd->axel_data->w[0] / (gd->axel_data->w[0] - gd->axel_data->w[1])) : 0;

  float x;
  float y;

  draw_axis_horizontals(file, x_axel_y_offset, y_axel_x_offset, gd->general, gd->axel_data);

  fprintf(file, svg_limiter_box);

  for(int i = 0; i < gd->n_lines; i++)
  {
    if(gd->lines[i].graph_type == 0)
    {
      format = malloc(strlen(svg_p_line_point)+1);
      strcpy(format, svg_p_line_point);
      fprintf(file, svg_p_line_start, 
        gd->lines[i].stroke_rgb[0], 
        gd->lines[i].stroke_rgb[1], 
        gd->lines[i].stroke_rgb[2]);
    }
    else
    {
      format = malloc(strlen(svg_g_point)+1);
      strcpy(format, svg_g_point);
      fprintf(file, svg_points_group_start,
        gd->lines[i].stroke_rgb[0], 
        gd->lines[i].stroke_rgb[1], 
        gd->lines[i].stroke_rgb[2],
        gd->lines[i].stroke_rgb[0],     
        gd->lines[i].stroke_rgb[1], 
        gd->lines[i].stroke_rgb[2]);
    }
    for(int k = 0; k < gd->lines[i].n_points; k++)
    {
      x = gd->general->margin / 2.0 
        + gd->lines[i].points[k].x
        * ((gd->general->viewport_x) / (gd->axel_data->w[1] - gd->axel_data->w[0]))
        + x_axel_y_offset;

      y = gd->general->margin / 2.0
        - gd->lines[i].points[k].y
        * ((gd->general->viewport_y) / (gd->axel_data->h[1] - gd->axel_data->h[0]))
        + gd->general->viewport_y 
        - y_axel_x_offset;

      fprintf(file, format, x, y);
    }
    (gd->lines[i].graph_type == 0) ? fprintf(file, svg_p_line_stop) : fprintf(file, svg_points_group_stop);
    free(format);
  }
  fprintf(file, svg_group_stop);
  fprintf(file, svg_top_header_stop);
  if(!gd->general->d_file) fclose(file);
  return;
}
