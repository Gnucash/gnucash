#include <litehtml.h>
using namespace litehtml;

class Bitmap
{
public:
	int width  = 0;
	int height = 0;
	std::vector<web_color> data;

	Bitmap() {}
	Bitmap(int width, int height, web_color color = web_color::white) : width(width), height(height)
	{
		data.resize(width * height, color);
	}
	Bitmap(string filename)
	{
		load(filename);
	}

	bool operator==(const Bitmap& bmp) const { return width == bmp.width && height == bmp.height && data == bmp.data; }
	bool operator!=(const Bitmap& bmp) const { return !(*this == bmp); }

	web_color get_pixel(int x, int y) const;
	void set_pixel(int x, int y, web_color color);
	void draw_line(int x0, int y0, int x1, int y1, web_color color);
	void draw_rect(int x, int y, int width, int height, web_color color);
	void fill_rect(position rect, web_color color);
	void draw_bitmap(int x, int y, const Bitmap& bmp);
	void replace_color(web_color original, web_color replacement);

	position find_picture(web_color bgcolor = web_color::white);
	void resize(int new_width, int new_height);
	void load(string filename);
	void save(string filename);
};