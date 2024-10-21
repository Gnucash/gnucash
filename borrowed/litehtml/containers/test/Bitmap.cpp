#include "Bitmap.h"
#include "lodepng.h"
using namespace std;

web_color Bitmap::get_pixel(int x, int y) const
{
	if (x < 0 || x >= width || y < 0 || y >= height)
		return web_color::black;
	else
		return data[x + y * width];
}

void Bitmap::set_pixel(int x, int y, web_color color)
{
	if (x < 0 || x >= width || y < 0 || y >= height) return;
	if (color.alpha == 0) return;
	data[x + y * width] = color;
}

// endpoint is not drawn, like in GDI
void Bitmap::draw_line(int x0, int y0, int x1, int y1, web_color color)
{
	if (x0 != x1 && y0 != y1) return; // only horz and vert lines supported

	if (x0 == x1) // vert line
	{
		if (y0 > y1) swap(y0, y1);
		for (int y = y0; y < y1; y++)
			set_pixel(x0, y, color);
	}
	else if (y0 == y1) // horz line
	{
		if (x0 > x1) swap(x0, x1);
		for (int x = x0; x < x1; x++)
			set_pixel(x, y0, color);
	}
}

void Bitmap::draw_rect(int x, int y, int width, int height, web_color color)
{
	draw_line(x, y,              x + width, y,              color); // top
	draw_line(x, y + height - 1, x + width, y + height - 1, color); // bottom
	draw_line(x,             y, x,             y + height, color); // left
	draw_line(x + width - 1, y, x + width - 1, y + height, color); // right
}

void Bitmap::fill_rect(position rect, web_color color)
{
	for (int y = rect.top(); y < rect.bottom(); y++)
		for (int x = rect.left(); x < rect.right(); x++)
			set_pixel(x, y, color);
}

void Bitmap::draw_bitmap(int x0, int y0, const Bitmap& bmp)
{
	for (int y = 0; y < bmp.height; y++)
		for (int x = 0; x < bmp.width; x++)
			set_pixel(x0 + x, y0 + y, bmp.get_pixel(x, y));
}

void Bitmap::replace_color(web_color original, web_color replacement)
{
	for (auto& pixel : data)
	{
		if (pixel == original)
			pixel = replacement;
	}
}

// find minimal rectangle containing pixels different from bgcolor
position Bitmap::find_picture(web_color bgcolor)
{
	auto horz_line_empty = [&](int y) {
		for (int x = 0; x < width; x++)
			if (data[x + y * width] != bgcolor) return false;
		return true;
	};
	auto vert_line_empty = [&](int x) {
		for (int y = 0; y < height; y++)
			if (data[x + y * width] != bgcolor) return false;
		return true;
	};

	position pos;
	int y;
	for (y = 0; y < height && horz_line_empty(y); y++);
	if (y == height) return pos; // no picture
	pos.y = y;
	for (y = height - 1; y >= 0 && horz_line_empty(y); y--);
	pos.height = y + 1 - pos.y;

	int x;
	for (x = 0; x < width && vert_line_empty(x); x++);
	pos.x = x;
	for (x = width - 1; x >= 0 && vert_line_empty(x); x--);
	pos.width = x + 1 - pos.x;

	return pos;
}

void Bitmap::resize(int new_width, int new_height)
{
	vector<web_color> new_data(new_width * new_height, web_color::white);
	for (int y = 0; y < min(new_height, height); y++)
		for (int x = 0; x < min(new_width, width); x++)
			new_data[x + y * new_width] = data[x + y * width];
	
	width = new_width;
	height = new_height;
	data = new_data;
}

void Bitmap::load(string filename)
{
	vector<byte> image;
	unsigned w, h;
	lodepng::decode(image, w, h, filename);
	if (w * h == 0) return;
	
	width = w;
	height = h;
	data.resize(w * h);
	memcpy(data.data(), image.data(), w * h * 4);
}

void Bitmap::save(string filename)
{
	lodepng::encode(filename, (byte*)data.data(), width, height);
}
