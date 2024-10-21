#include "Bitmap.h"

class Font
{
public:
	int width    = 0;
	int height   = 0;
	int ascent   = 0;
	int descent  = 0;
	int x_height = 0;
	Bitmap glyphs[128];
	
	static string font_dir;
	static struct size_name { int size; string name; } installed_fonts[];
	
	Font(int size);
	Bitmap get_glyph(int ch, web_color color);
	void load(string filename);
};