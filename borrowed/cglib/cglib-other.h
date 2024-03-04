#include <stdio.h>
#include <stdint.h>

#ifndef C_GRAPH_LIB_OTHER
  #define C_GRAPH_LIB_OTHER

  #define make_cmp_1(grp_dat, cmp) (grp_dat->previous == -1 || grp_dat->previous == 2) && cmp
  #define make_cmp_2(grp_dat, cmp) (grp_dat->previous == -1 || grp_dat->previous == 1) && cmp
  #define valid_pie_data(pd, sum) if((pd->n_slices > 1 || (pd->n_slices == 1 && pd->slices[0].percentage < 1.0)) && sum <= 1.0)
  #define loop(x) for(int i = 0; i < x; i++)
  #define cut(range, x) (range[0] < range[1]) ? cut_(range, x) : 0 * printf("error\n")
  
  #define svg_top_header_start "<?xml version=\"1.0\" encoding=\"UTF-8\" ?><!--THIS DOCUMENT WAS GENERATED WITH CGLIB CREATED BY FULLNITROUS RIGHTS RESERVED TO VOIDZEHN HTTPS://MDPP.XYZ HTTPS://VOIDZEHN.COM HTTPS://CGLIB.XYZ HTTPS://FULLNITROUS.COM--><svg xmlns=\"http://www.w3.org/2000/svg\" style =\"font-family: sans-serif;\" version=\"1.1\" viewBox=\"0 0 %9.6f %9.6f\" stroke-width=\"%d\"><defs><clipPath id=\"cliparea\"><rect x=\"%9.6f\" y=\"%9.6f\" width=\"%9.6f\" height=\"%9.6f\"/></clipPath></defs>"
  #define svg_limiter_box "<g clip-path=\"url(#cliparea)\">"
  #define svg_custom_mask_box "<g mask=\"url(#%s)\">"
  #define svg_mask_path "<defs><mask id=\"%s\">%s</mask></defs>"
  #define svg_top_header_stop "</svg>"
  #define svg_points_group_start "<g font-size=\"10\" text-anchor=\"start\" stroke-width=\"1\" stroke =\"#%02x%02x%02x\" fill=\"#%02x%02x%02x\">"
  #define svg_points_group_stop "</g>"
  #define svg_point "<circle cx=\"%9.6f\" cy=\"%9.6f\" r=\"2\"/>"
  #define svg_g_point "<circle cx=\"%9.6f\" cy=\"%9.6f\" r=\"1\"/>"  
  #define svg_circle "<circle cx=\"%9.6f\" cy=\"%9.6f\" r=\"%9.6f\"/>"
  #define svg_p_line_start "<polyline fill=\"none\" stroke=\"#%02x%02x%02x\" points=\""
  #define svg_p_line_stop "\"/>"
  #define svg_line "<line x1=\"%9.6f\" y1=\"%9.6f\" x2=\"%9.6f\" y2=\"%9.6f\"/>"
  #define svg_slice "<path d=\"M %9.6f %9.6f A %9.6f %9.6f 0 %d 1 %9.6f %9.6f L %9.6f %9.6f \" fill= \"#%02x%02x%02x\"></path>"
  #define svg_doughnut_section "<path d=\"M %9.6f %9.6f A %9.6f %9.6f 0 %d 1 %9.6f %9.6f L %9.6f %9.6f A %9.6f %9.6f 0 %d 0 %9.6f %9.6f\" fill=\"#%02x%02x%02x\"></path>"
  #define svg_arc "<path d=\"M %9.6f %9.6f A %9.6f %9.6f 0 %d 1 %9.6f %9.6f\" fill \"none\" stroke \"#%02x%02x%02x\"></path>"
  #define svg_box "<rect fill=\"#%02x%02x%02x\" x=\"%9.6f\" y=\"%9.6f\" width=\"%9.6f\" height=\"%9.6f\"/>"
  #define svg_text "<text x=\"%9.6f\" y=\"%9.6f\">%s</text>"
  #define svg_custom_text "<text x=\"%9.6f\" y=\"%9.6f\" %s >%s</text>"
  #define svg_bd_text "<text x=\"%9.6f\" y=\"%9.6f\">%s : %9.6f</text>"
  #define svg_p_line_point "%9.6f, %9.6f "
  #define svg_custom_group "<g %s>"
  #define svg_group_stop "</g>"

  struct group_data
  {
    FILE*    file;
    uint8_t  cmp_1;
    uint8_t  cmp_2;
    int8_t   previous;
    char*    cmp_1_out;
    char*    cmp_2_out; 
  };

  void draw_axis_horizontals(FILE* file, float x_axel_y_offset, float y_axel_x_offset, struct general_data* gd, struct axel_data* ad);
  void get_gradient(struct theme_data* td);
  void print_top_header(FILE* file, struct general_data* gd);
  void print_slice_pointers(FILE* file, struct pie_data* pd);
  void print_legend(FILE* file, struct pie_data* pd, float x, float y);
  void print_font_size_group(FILE* file, struct general_data* gd);
  void destroy_group_dat(struct group_data* grp_dat);
  
  char* stringify(const char* format, ...);
  
  int print_group(struct group_data* grp_dat);
  
  struct group_data* init_group_dat(FILE* file, const char* cmp_1_out, const char* cmp_2_out);

  void select_color_function(struct theme_data* td);
  void get_dark_gradient(struct theme_data* td);
  void get_light_gradient(struct theme_data* td);
  void get_random_hue(struct theme_data* td);
  void get_random_col(struct theme_data* td);

  int32_t cut_(int32_t range[2], int32_t x);

  void rgb_darken(struct rgb* values, float percentage);
  void rgb_lighten(struct rgb* values, float percentage);

#endif
