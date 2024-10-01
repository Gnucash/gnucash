#include "test_container.h"
#include "Font.h"
string readfile(string filename);

// note: font is selected only by size, name and style are not used
uint_ptr test_container::create_font(const char* /*faceName*/, int size, int /*weight*/, font_style /*italic*/, unsigned int /*decoration*/, font_metrics* fm)
{
	Font* font = new Font(size);

	if (fm)
	{
		fm->ascent   = font->ascent;
		fm->descent  = font->descent;
		fm->height   = font->height;
		fm->x_height = font->x_height;
	}
	return (uint_ptr)font;
}

int test_container::text_width(const char* text, uint_ptr hFont)
{
	Font* font = (Font*)hFont;
	return (int)strlen(text) * font->width;
}

void test_container::draw_text(uint_ptr hdc, const char* text, uint_ptr hFont, web_color color, const position& pos)
{
	auto bmp = (Bitmap*)hdc;
	Font* font = (Font*)hFont;

	int x = pos.x;
	for (auto p = text; *p; p++)
	{
		Bitmap glyph = font->get_glyph(*p, color);
		bmp->draw_bitmap(x, pos.y, glyph);
		x += glyph.width;
	}
}

int test_container::pt_to_px(int pt) const { return pt * 96 / 72; }
int test_container::get_default_font_size() const { return 16; }
const char* test_container::get_default_font_name() const { return ""; }

void test_container::draw_background(uint_ptr hdc, const std::vector<background_paint>& bg)
{
	auto bmp = (Bitmap*)hdc;
	bmp->fill_rect(bg.back().border_box, bg.back().color);
}

void test_container::draw_borders(uint_ptr hdc, const borders& borders, const position& pos, bool /*root*/)
{
	auto bmp = (Bitmap*)hdc;

	// left border
	for (int x = 0; x < borders.left.width; x++)
		bmp->draw_line(
			pos.left() + x, pos.top(), 
			pos.left() + x, pos.bottom(), borders.left.color);

	// right border
	for (int x = 0; x < borders.right.width; x++)
		bmp->draw_line(
			pos.right() - x - 1, pos.top(),
			pos.right() - x - 1, pos.bottom(), borders.right.color);

	// top border
	for (int y = 0; y < borders.top.width; y++)
		bmp->draw_line(
			pos.left(),  pos.top() + y,
			pos.right(), pos.top() + y, borders.top.color);

	// bottom border
	for (int y = 0; y < borders.bottom.width; y++)
		bmp->draw_line(
			pos.left(),  pos.bottom() - y - 1,
			pos.right(), pos.bottom() - y - 1, borders.bottom.color);
}

void test_container::draw_list_marker(uint_ptr hdc, const list_marker& marker)
{
	auto bmp = (Bitmap*)hdc;
	bmp->fill_rect(marker.pos, marker.color);
}

void test_container::import_css(string& text, const string& url, string& baseurl)
{
	baseurl = basedir + "/" + url;
	text = readfile(baseurl);
}

void test_container::get_client_rect(position& client) const
{
	client = position(0, 0, width, height);
}
