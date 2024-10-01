#include "cglib.h"
#include "cglib-other.h"


void print_legend(FILE* file, struct pie_data* pd, float x, float y)
{
  char* ret;
  ret = stringify("text-anchor = \"start\" font-size=\"%d\"", pd->general->font_size);
  fprintf(file, svg_custom_group, ret);
  free(ret);

  for(int i = 0; i < pd->n_slices; i++)
  {
    fprintf(file, svg_box,
      pd->slices[i].color.r,
      pd->slices[i].color.g,
      pd->slices[i].color.b,
      x,
      y + (i * (pd->general->font_size + 5)),

      (double)30,
      (double)pd->general->font_size);

    fprintf(file, svg_text, 
      x + 40,
      y + (i * (pd->general->font_size + 5)) + pd->general->font_size,

      pd->slices[i].name);
  }
  fprintf(file, svg_group_stop);
}

void print_slice_pointers(FILE* file, struct pie_data* pd)
{
  float pointer_len = 120.0;
  char* ret;

  float sum_counter, 
    radians,
//    x_start,
//    x_stop,
//    y_start,
//    y_stop,
    x_pointer_start,
    y_pointer_start;

  float current_pointer_len;

  float radius = (pd->general->viewport_y <= pd->general->viewport_x) ? (pd->general->viewport_y - pd->general->margin) / 2 : (pd->general->viewport_x - pd->general->margin) / 2;

  float origin[2];
  origin[0] = pd->general->viewport_x / 2.0 + pd->general->margin / 2.0;
  origin[1] = pd->general->viewport_y / 2.0 + pd->general->margin / 2.0;

  sum_counter = 0.0;

//  uint8_t s = 1;

  ret = stringify("stroke=\"#%02x%02x%02x\"", 
    pd->axel_data->axel_rgb[0],
    pd->axel_data->axel_rgb[1],
    pd->axel_data->axel_rgb[2]);
  fprintf(file, svg_custom_group, ret);
  free(ret);

  for(int i = 0; i < pd->n_slices; i++)
  {
    radians = 2*(pd->slices[i].percentage/2+sum_counter)*PI;
    sum_counter += pd->slices[i].percentage;
    x_pointer_start = cos(radians)*radius+origin[0];
    y_pointer_start = sin(radians)*radius+origin[1];
    current_pointer_len = (origin[0] < x_pointer_start) ? pointer_len : -pointer_len;

    //pointer lol
    fprintf(file, svg_line, 
      x_pointer_start, 
      y_pointer_start,
      x_pointer_start + current_pointer_len,
      y_pointer_start);
  }
  fprintf(file, svg_group_stop);

  sum_counter = 0.0;

  struct group_data* grp_dat = init_group_dat(file, "text-anchor=\"end\"", "\0");  
  for(int i = 0; i < pd->n_slices; i++)
  {
    radians = 2   * (pd->slices[i].percentage / 2 + sum_counter) * PI;
    sum_counter     += pd->slices[i].percentage;
    x_pointer_start = cos(radians) * radius + origin[0];
    y_pointer_start = sin(radians) * radius + origin[1];
    current_pointer_len = (origin[0] < x_pointer_start) ? pointer_len : -pointer_len;

    grp_dat->cmp_1 = make_cmp_1(grp_dat, origin[0] < x_pointer_start);
    grp_dat->cmp_2 = make_cmp_2(grp_dat, origin[0] > x_pointer_start);
    print_group(grp_dat);

    fprintf(file, svg_text,
      x_pointer_start + current_pointer_len,
      y_pointer_start - pd->axel_data->axel_number_offset,
      pd->slices[i].name);
  }
  fprintf(file, svg_group_stop);
  destroy_group_dat(grp_dat);

  sum_counter = 0.0;

//  s = 1;

  ret = stringify("fill=\"#%02x%02x%02x\" dominant-baseline=\"hanging\"", 
    pd->axel_data->axel_rgb[0],
    pd->axel_data->axel_rgb[1],
    pd->axel_data->axel_rgb[2]);
  fprintf(file, svg_custom_group, ret);
  free(ret);
  
  grp_dat = init_group_dat(file, "text-anchor=\"end\"", "\0"); 
  for(int i = 0; i < pd->n_slices; i++)
  {
    radians = 2   * (pd->slices[i].percentage / 2 + sum_counter) * PI;
    sum_counter     += pd->slices[i].percentage;
    x_pointer_start = cos(radians) * radius + origin[0];
    y_pointer_start = sin(radians) * radius + origin[1];
    current_pointer_len = (origin[0] < x_pointer_start) ? pointer_len : -pointer_len;

    ret = stringify("%9.1f%%", pd->slices[i].percentage * 100);
    
    grp_dat->cmp_1 = make_cmp_1(grp_dat, origin[0] < x_pointer_start);
    grp_dat->cmp_2 = make_cmp_2(grp_dat, origin[0] > x_pointer_start);
    print_group(grp_dat);

    fprintf(file, svg_text,   
      x_pointer_start + current_pointer_len, 
      y_pointer_start + pd->axel_data->axel_number_offset, ret);

    free(ret);
  }
  destroy_group_dat(grp_dat);
  fprintf(file, svg_group_stop);
  fprintf(file, svg_group_stop);
}

void print_font_size_group(FILE* file, struct general_data* gd)
{
  char* ret = stringify("font-size=\"%d\"", gd->font_size);
  fprintf(file, svg_custom_group, ret);
  free(ret);
  return;
}

void draw_axis_horizontals(FILE* file, float x_axel_y_offset, float y_axel_x_offset, struct general_data* gd, struct axel_data* ad)
{
  char* ret;
  char* ret_2;

  ret = stringify("stroke = \"#%02x%02x%02x\"", 
    ad->axel_lines_rgb[0],
    ad->axel_lines_rgb[1],
    ad->axel_lines_rgb[2]);

  fprintf(file, svg_custom_group, ret);
  free(ret);

  float add = gd->viewport_x / ad->n_measure_points;
  float counter = 0;

  if(ad->vertical_lines)
  {
    //draw vertical lines with corresponding numbers
    
    for(int i = 0; i < ad->n_measure_points + 1; i++)
    {
      fprintf(file, svg_line, 
        gd->margin / 2.0 + counter, 
        gd->margin / 2.0, 
        gd->margin / 2.0 + counter,
        gd->margin / 2.0 + gd->viewport_y);

      counter += add;
    }
  }

  if(ad->horizontal_lines)
  {
    //draw horizontal lines with corresponding numbers
    add = gd->viewport_y / ad->n_measure_points;
    counter = 0;
    for(int i = 0; i < ad->n_measure_points + 1; i++)
    {
      fprintf(file, svg_line, 
        gd->margin / 2.0, 
        gd->margin / 2.0 + gd->viewport_y - counter, 
        gd->margin / 2.0 + gd->viewport_x, 
        gd->margin / 2.0 + gd->viewport_y - counter);

      counter += add;
    }
  }

  fprintf(file, svg_group_stop);

  float a_add = (ad->w[1] - ad->w[0]) / ad->n_measure_points;
  float a_counter = ad->w[0];
  counter =  0;
  add = gd->viewport_x / ad->n_measure_points;
  
  if(ad->numbered_x)
  {
    ret = stringify("text-anchor = \"end\" dominant-baseline=\"hanging\" font-size=\"%d\"", gd->font_size);

    fprintf(file, svg_custom_group, ret);
    free(ret);
    for(int i = 0; i < ad->n_measure_points + 1; i++)
    {
      ret = stringify("%9.2f", a_counter);

      ret_2 = stringify("transform=\"rotate(%9.6f, %9.6f, %9.6f)\"", 
      ad->x_axel_text_angle,
      gd->margin / 2.0 + counter,
      gd->margin / 2.0 + gd->viewport_y - y_axel_x_offset);
      
      fprintf(file, svg_custom_text, 
        gd->margin / 2.0 + counter,
        gd->margin / 2.0 + gd->viewport_y - y_axel_x_offset + ad->axel_number_offset,
        ret_2,
        ret);

      counter += add;
      a_counter += a_add;
      
      free(ret);
      free(ret_2);
    }
    fprintf(file, svg_group_stop);
  }

  counter = 0;

  a_add = (ad->h[1] - ad->h[0]) / ad->n_measure_points;
  a_counter = ad->h[0];
  add = gd->viewport_y / ad->n_measure_points;

  if(ad->numbered_y)
  {
    ret = stringify("text-anchor = \"end\" font-size=\"%d\"", gd->font_size);

    fprintf(file, svg_custom_group, ret);
    free(ret);
    for(int i = 0; i < ad->n_measure_points + 1; i++)
    {
      ret = stringify("%9.2f", a_counter);

      fprintf(file, svg_text, 
        gd->margin / 2.0 + x_axel_y_offset + ad->axel_number_offset * -1,
        gd->margin / 2.0 + gd->viewport_y - counter,
        ret);

      counter += add;
      a_counter += a_add;
      
      free(ret);
    }
    fprintf(file, svg_group_stop);
  }

  ret = stringify("stroke = \"#%02x%02x%02x\"", 
    ad->axel_rgb[0],
    ad->axel_rgb[1],
    ad->axel_rgb[2]);

  fprintf(file, svg_custom_group, ret);

  free(ret);

  //draw the y axel
  fprintf(file, svg_line, 
    gd->margin / 2.0 + x_axel_y_offset, 
    gd->margin / 2.0, 
    gd->margin / 2.0 + x_axel_y_offset,
    gd->margin / 2.0 + gd->viewport_y);

  //draw the x axel
  fprintf(file, svg_line, 
    gd->margin / 2.0, 
    gd->margin / 2.0 + gd->viewport_y - y_axel_x_offset, 
    gd->margin / 2.0 + gd->viewport_x, 
    gd->margin / 2.0 + gd->viewport_y - y_axel_x_offset);

  fprintf(file, svg_group_stop);
  return;
}

void print_top_header(FILE* file, struct general_data* gd)
{
  fprintf(file, 
    svg_top_header_start, 
    gd->viewport_x + gd->margin, 
    gd->viewport_y + gd->margin, 
    gd->stroke_width,
    gd->margin / 2.0,
    gd->margin / 2.0,
    gd->viewport_x, 
    gd->viewport_y);
  return;
}

char* stringify(const char* format, ...)
{
  char* str;
  int   len_str;

  va_list args;
  va_start(args, format);
  len_str = vsnprintf(NULL, 0, format, args);
  va_end(args);
  str = malloc(sizeof(char) * (len_str + 1));
  va_start(args, format);
  vsnprintf(str, len_str + 1, format, args);
  va_end(args);
  str[len_str] = '\0';
  return str;
}

int print_group(struct group_data* grp_dat)
{
  if(grp_dat->cmp_1)
  {
    (grp_dat->previous != -1) ? fprintf(grp_dat->file, svg_group_stop) : 0;
    fprintf(grp_dat->file, svg_custom_group, grp_dat->cmp_1_out);
    grp_dat->previous = 1;
  }
  else if(grp_dat->cmp_2)
  {
    (grp_dat->previous != -1) ? fprintf(grp_dat->file, svg_group_stop) : 0;
    fprintf(grp_dat->file, svg_custom_group, grp_dat->cmp_2_out);
    grp_dat->previous = 2;
  }
  return grp_dat->previous;
}

struct group_data* init_group_dat(FILE* file, const char* cmp_1_out, const char* cmp_2_out)
{
  struct group_data* grp_dat;
  grp_dat = malloc(sizeof(struct group_data));
  grp_dat->file = file;
  grp_dat->previous = -1;
  grp_dat->cmp_1_out = malloc(strlen(cmp_1_out)+1);
  strcpy(grp_dat->cmp_1_out, cmp_1_out);
  grp_dat->cmp_2_out = malloc(strlen(cmp_2_out)+1);
  strcpy(grp_dat->cmp_2_out, cmp_2_out);
  return grp_dat;
}

void destroy_group_dat(struct group_data* grp_dat)
{
  free(grp_dat->cmp_1_out);
  free(grp_dat->cmp_2_out);
  free(grp_dat);
  return;
}

int32_t cut_(int32_t range[2], int32_t x)
{
  if(x < range[0])
  {
    int8_t flip = (range[0] < 0) ? -1 : 1;
    return cut_(range, x + range[0] * flip);
  }
  else if(x > range[1])
  {
    return cut_(range, range[0] - (x - range[1] - 1));
  }
  else
  {
    return x;
  }
}
