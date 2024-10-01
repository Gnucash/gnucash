#include <windows.h>
#include <gdiplus.h>
#include "gdiplus_container.h"
#pragma comment(lib, "gdiplus.lib")
using namespace Gdiplus;
using namespace litehtml;

gdiplus_container::gdiplus_container()
{
	GdiplusStartupInput gdiplusStartupInput;
	GdiplusStartup(&m_gdiplusToken, &gdiplusStartupInput, NULL);
}

gdiplus_container::~gdiplus_container()
{
	clear_images();
	GdiplusShutdown(m_gdiplusToken);
}

static Color gdiplus_color(web_color color)
{
	return Color(color.alpha, color.red, color.green, color.blue);
}

void gdiplus_container::draw_ellipse(HDC hdc, int x, int y, int width, int height, web_color color, int line_width)
{
	Graphics graphics(hdc);

	graphics.SetCompositingQuality(CompositingQualityHighQuality);
	graphics.SetSmoothingMode(SmoothingModeAntiAlias);

	Pen pen(gdiplus_color(color));
	graphics.DrawEllipse(&pen, x, y, width, height);
}

void gdiplus_container::fill_ellipse(HDC hdc, int x, int y, int width, int height, web_color color)
{
	Graphics graphics(hdc);

	graphics.SetCompositingQuality(CompositingQualityHighQuality);
	graphics.SetSmoothingMode(SmoothingModeAntiAlias);

	SolidBrush brush(gdiplus_color(color));
	graphics.FillEllipse(&brush, x, y, width, height);
}

void gdiplus_container::fill_rect(HDC hdc, int x, int y, int width, int height, web_color color)
{
	Graphics graphics(hdc);

	SolidBrush brush(gdiplus_color(color));
	graphics.FillRectangle(&brush, x, y, width, height);
}

void gdiplus_container::get_img_size(uint_ptr img, size& sz)
{
	Bitmap* bmp = (Bitmap*)img;
	if (bmp)
	{
		sz.width  = bmp->GetWidth();
		sz.height = bmp->GetHeight();
	}
}

void gdiplus_container::free_image(uint_ptr img)
{
	Bitmap* bmp = (Bitmap*)img;
	delete bmp;
}

void gdiplus_container::draw_img_bg(HDC hdc, uint_ptr img, const background_paint& bg)
{
	Bitmap* bgbmp = (Bitmap*)img;

	Graphics graphics(hdc);
	graphics.SetInterpolationMode(InterpolationModeNearestNeighbor);
	graphics.SetPixelOffsetMode(PixelOffsetModeHalf);

	Region reg(Rect(bg.border_box.left(), bg.border_box.top(), bg.border_box.width, bg.border_box.height));
	graphics.SetClip(&reg);

	Bitmap* scaled_img = nullptr;
	if (bg.image_size.width != bgbmp->GetWidth() || bg.image_size.height != bgbmp->GetHeight())
	{
		scaled_img = new Bitmap(bg.image_size.width, bg.image_size.height);
		Graphics gr(scaled_img);
		gr.SetPixelOffsetMode(PixelOffsetModeHighQuality);
		gr.DrawImage(bgbmp, 0, 0, bg.image_size.width, bg.image_size.height);
		bgbmp = scaled_img;
	}

	switch (bg.repeat)
	{
	case background_repeat_no_repeat:
		{
			graphics.DrawImage(bgbmp, bg.position_x, bg.position_y, bgbmp->GetWidth(), bgbmp->GetHeight());
		}
		break;
	case background_repeat_repeat_x:
		{
			CachedBitmap bmp(bgbmp, &graphics);
			int x = bg.position_x;
			while(x > bg.clip_box.left()) x -= bgbmp->GetWidth();
			for(; x < bg.clip_box.right(); x += bgbmp->GetWidth())
			{
				graphics.DrawCachedBitmap(&bmp, x, bg.position_y);
			}
		}
		break;
	case background_repeat_repeat_y:
		{
			CachedBitmap bmp(bgbmp, &graphics);
			int y = bg.position_y;
			while(y > bg.clip_box.top()) y -= bgbmp->GetHeight();
			for(; y < bg.clip_box.bottom(); y += bgbmp->GetHeight())
			{
				graphics.DrawCachedBitmap(&bmp, bg.position_x, y);
			}
		}
		break;
	case background_repeat_repeat:
		{
			CachedBitmap bmp(bgbmp, &graphics);
			int x = bg.position_x;
			while(x > bg.clip_box.left()) x -= bgbmp->GetWidth();
			int y0 = bg.position_y;
			while(y0 > bg.clip_box.top()) y0 -= bgbmp->GetHeight();

			for(; x < bg.clip_box.right(); x += bgbmp->GetWidth())
			{
				for(int y = y0; y < bg.clip_box.bottom(); y += bgbmp->GetHeight())
				{
					graphics.DrawCachedBitmap(&bmp, x, y);
				}
			}
		}
		break;
	}

	delete scaled_img;
}

// length of dash and space for "dashed" style, in multiples of pen width
const float dash = 3;
const float space = 2;

static void draw_horz_border(Graphics& graphics, const border& border, int y, int left, int right)
{
	if (border.style != border_style_double || border.width < 3)
	{
		if (border.width == 1) right--; // 1px-wide lines are longer by one pixel in GDI+ (the endpoint is also drawn)
		Pen pen(gdiplus_color(border.color), (float)border.width);
		if (border.style == border_style_dotted)
		{
			float dashValues[2] = { 1, 1 };
			pen.SetDashPattern(dashValues, 2);
		}
		else if (border.style == border_style_dashed)
		{
			float dashValues[2] = { dash, space };
			pen.SetDashPattern(dashValues, 2);
		}
		graphics.DrawLine(&pen,
			Point(left,  y + border.width / 2),
			Point(right, y + border.width / 2));
	}
	else
	{
		int single_line_width = (int)round(border.width / 3.);
		if (single_line_width == 1) right--;
		Pen pen(gdiplus_color(border.color), (float)single_line_width);
		graphics.DrawLine(&pen,
			Point(left,  y + single_line_width / 2),
			Point(right, y + single_line_width / 2));
		graphics.DrawLine(&pen,
			Point(left,  y + border.width - 1 - single_line_width / 2),
			Point(right, y + border.width - 1 - single_line_width / 2));
	}
}

static void draw_vert_border(Graphics& graphics, const border& border, int x, int top, int bottom)
{
	if (border.style != border_style_double || border.width < 3)
	{
		if (border.width == 1) bottom--;
		Pen pen(gdiplus_color(border.color), (float)border.width);
		if (border.style == border_style_dotted)
		{
			float dashValues[2] = { 1, 1 };
			pen.SetDashPattern(dashValues, 2);
		}
		else if (border.style == border_style_dashed)
		{
			float dashValues[2] = { dash, space };
			pen.SetDashPattern(dashValues, 2);
		}
		graphics.DrawLine(&pen,
			Point(x + border.width / 2, top),
			Point(x + border.width / 2, bottom));
	}
	else
	{
		int single_line_width = (int)round(border.width / 3.);
		if (single_line_width == 1) bottom--;
		Pen pen(gdiplus_color(border.color), (float)single_line_width);
		graphics.DrawLine(&pen,
			Point(x + single_line_width / 2, top),
			Point(x + single_line_width / 2, bottom));
		graphics.DrawLine(&pen,
			Point(x + border.width - 1 - single_line_width / 2, top),
			Point(x + border.width - 1 - single_line_width / 2, bottom));
	}
}

void gdiplus_container::draw_borders(uint_ptr hdc, const borders& borders, const position& draw_pos, bool root)
{
	apply_clip((HDC) hdc);
	Graphics graphics((HDC)hdc);

	if (borders.left.width != 0)
	{
		draw_vert_border(graphics, borders.left, draw_pos.left(), draw_pos.top(), draw_pos.bottom());
	}
	if (borders.right.width != 0)
	{
		draw_vert_border(graphics, borders.right, draw_pos.right() - borders.right.width, draw_pos.top(), draw_pos.bottom());
	}
	if (borders.top.width != 0)
	{
		draw_horz_border(graphics, borders.top, draw_pos.top(), draw_pos.left(), draw_pos.right());
	}
	if (borders.bottom.width != 0)
	{
		draw_horz_border(graphics, borders.bottom, draw_pos.bottom() - borders.bottom.width, draw_pos.left(), draw_pos.right());
	}

	release_clip((HDC) hdc);
}
