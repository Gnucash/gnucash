#include <config.h>
#include <glib/gi18n.h>

#include <stdio.h>

#include "my-test-of-cglib.h"
#include "cglib.h"


typedef struct general_data mygeneral_data;

void
my_test_of_cglib_charts(void)
{
g_print("%s called\n",__FUNCTION__);
  srand(time(NULL));
//  struct general_data* general = malloc(sizeof(struct general_data));

  mygeneral_data* general = g_new0 (mygeneral_data, 1);

  general->stroke_width = 1;
  general->margin = 100;
  general->viewport_x = 640;
  general->viewport_y = 360;
  general->font_size = 12;
  general->d_file = 0;

  struct axel_data* axel = malloc(sizeof(struct axel_data));
  axel->axel_rgb[0] = 100;
  axel->axel_rgb[1] = 100;
  axel->axel_rgb[2] = 100;

  axel->axel_lines_rgb[0] = 220;
  axel->axel_lines_rgb[1] = 220;
  axel->axel_lines_rgb[2] = 220;

  axel->horizontal_lines = 1;
  axel->vertical_lines = 1;
  axel->numbered_x = 1;
  axel->numbered_y = 1;

  axel->x_axel_text_angle = -30.0;

  axel->axel_number_offset = 5.0;
  
  axel->n_measure_points = 5;

  axel->w[0] = -25;
  axel->w[1] = 25;
  axel->h[0] = -200;
  axel->h[1] = 200;


  struct theme_data* theme = malloc(sizeof(struct theme_data));
  theme->stop.r = 65;
  theme->stop.g = 130;
  theme->stop.b = 234;

  theme->start.r = 234;
  theme->start.g = 99;
  theme->start.b = 175;

  theme->theme_type = 0;
  
  theme->lightness_mod_percentage = 0.6;


  struct graph_data* gp = malloc(sizeof(struct graph_data));
  gp->general = general;
  gp->axel_data = axel;
  general->file_name = malloc(sizeof(char) * 30);
  strcpy(general->file_name, "/tmp/graph.svg\0");

  gp->n_lines = 2;
  gp->lines = malloc(sizeof(struct line) * 2);

  gp->lines[0].n_points = 80;
  gp->lines[1].n_points = 80;

  gp->lines[0].graph_type = 0;
  gp->lines[1].graph_type = 1;

  gp->lines[0].stroke_rgb[0] = 255;
  gp->lines[0].stroke_rgb[1] = 0;
  gp->lines[0].stroke_rgb[2] = 0;
  
  gp->lines[1].stroke_rgb[0] = 0;
  gp->lines[1].stroke_rgb[1] = 0;
  gp->lines[1].stroke_rgb[2] = 255;

  gp->lines[0].points = malloc(sizeof(struct point) * gp->lines[0].n_points);
  gp->lines[1].points = malloc(sizeof(struct point) * gp->lines[1].n_points);

  float count = -20;
  for(int i = -40; i < 40; i++)
  {
    count += 0.5;
    gp->lines[0].points[i + 40].x = count;
    gp->lines[0].points[i + 40].y = count * count;
  }
  count = -20;
  for(int i = -40; i < 40; i++)
  {
    count += 0.5;
    gp->lines[1].points[i + 40].x = count;
    gp->lines[1].points[i + 40].y = -count * count * count;
  }
  graph(gp);


  general->d_file = 1;
  FILE* direct = fopen("/tmp/graph-direct.svg", "w");
  
  general->file = direct;
  graph(gp);
  general->d_file = 0;

  fclose(direct);

  free(gp->lines[0].points);
  free(gp->lines[1].points);
  free(gp->lines);
  free(gp);




  struct pie_data* pd = malloc(sizeof(struct pie_data));
  free(general->file_name);
  general->file_name = malloc(sizeof(char) * 30);
  strcpy(general->file_name, "/tmp/pie.svg\0");
  general->margin = 40.0;
  pd->general = general;
  pd->axel_data = axel;
  pd->theme = theme;
  pd->n_slices = 6;
  pd->slices = malloc(sizeof(struct pie_slice) * pd->n_slices);

  pd->d_h1_font_size = 50;
  pd->d_h2_font_size = 20;

  for(int i = 0; i < pd->n_slices; i++)
  {
    pd->slices[i].percentage = 1.0 / pd->n_slices;
    pd->slices[i].name = malloc(6);
    strcpy(pd->slices[i].name, "Label\0");
  }

  pie(pd);
  free(general->file_name);
  general->file_name = malloc(sizeof(char) * 30);
  strcpy(general->file_name, "/tmp/doughnut.svg\0");
  pd->doughnut_header = malloc(sizeof(char) * 15);
  strcpy(pd->doughnut_header, "Label\0");
  pd->doughnut_sub_header = malloc(sizeof(char) * 30);
  strcpy(pd->doughnut_sub_header, "Label\0");
  doughnut(pd);

  free(pd->doughnut_header);
  free(pd->doughnut_sub_header);
  for(int i = 0; i < pd->n_slices; i++)
  {
    free(pd->slices[i].name);
  }
  free(pd->slices);
  free(pd);


  struct bar_data* bd = malloc(sizeof(struct bar_data));
  free(general->file_name);
  general->file_name = malloc(sizeof(char) * 30);
  strcpy(general->file_name, "/tmp/vbar.svg\0");
  bd->general = general;
  general->margin = 100;
  bd->theme = theme;
  bd->axel_data = axel;
  bd->n_bars =  15;
  bd->spacing = 5;
  axel->numbered_x = 0;
  axel->vertical_lines = 0;
  bd->bars = malloc(sizeof(struct bar) * bd->n_bars);

  axel->w[0] = 0;
  axel->w[1] = 50;
  axel->h[0] = -100;
  axel->h[1] = 100;

  for(int i = 0; i < bd->n_bars; i++)
  {
    bd->bars[i].value = 50.0 - i * 6;
    bd->bars[i].name = malloc(6);
    strcpy(bd->bars[i].name, "Label\0");
  }

  vbar(bd);

  axel->w[0] = -100;
  axel->w[1] = 100;
  axel->h[0] = 0;
  axel->h[1] = 100;

  axel->numbered_y = 0;
  axel->numbered_x = 1;
  axel->vertical_lines = 1;
  axel->horizontal_lines = 0;

  strcpy(general->file_name, "/tmp/hbar.svg\0");
  hbar(bd);

  free(general->file_name);
  free(general);
  free(axel);
  free(theme);
  

  for(int i = 0; i < bd->n_bars; i++)
  {
    free(bd->bars[i].name);
  }
  free(bd->bars);
  free(bd);
}
