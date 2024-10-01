#include "cairo_container.h"
#define _USE_MATH_DEFINES
#include <math.h>
#include "cairo_font.h"
#include <strsafe.h>

cairo_container::cairo_container(void)
{
	m_temp_surface	= cairo_image_surface_create(CAIRO_FORMAT_ARGB32, 2, 2);
	m_temp_cr		= cairo_create(m_temp_surface);
	m_font_link		= NULL;
	CoCreateInstance(CLSID_CMultiLanguage, NULL, CLSCTX_ALL, IID_IMLangFontLink2, (void**) &m_font_link);
	InitializeCriticalSection(&m_img_sync);
}

cairo_container::~cairo_container(void)
{
	clear_images();
	if(m_font_link)
	{
		m_font_link->Release();
	}
	cairo_surface_destroy(m_temp_surface);
	cairo_destroy(m_temp_cr);
	DeleteCriticalSection(&m_img_sync);
}

litehtml::uint_ptr cairo_container::create_font( const char* faceName, int size, int weight, litehtml::font_style italic, unsigned int decoration, litehtml::font_metrics* fm )
{
	std::wstring fnt_name = L"sans-serif";

	litehtml::string_vector fonts;
	litehtml::split_string(faceName, fonts, ",");
	if(!fonts.empty())
	{
		litehtml::trim(fonts[0]);
		wchar_t* f = cairo_font::utf8_to_wchar(fonts[0].c_str());
		fnt_name = f;
		delete f;
		if (fnt_name.front() == L'"' || fnt_name.front() == L'\'')
		{
			fnt_name.erase(0, 1);
		}
		if (fnt_name.back() == L'"' || fnt_name.back() == L'\'')
		{
			fnt_name.erase(fnt_name.length() - 1, 1);
		}
	}

	cairo_font* fnt = new cairo_font(	m_font_link,
										fnt_name.c_str(), 
										size, 
										weight, 
										(italic == litehtml::font_style_italic) ? TRUE : FALSE,
										(decoration & litehtml::font_decoration_linethrough) ? TRUE : FALSE,
										(decoration & litehtml::font_decoration_underline) ? TRUE : FALSE);

	cairo_save(m_temp_cr);
	fnt->load_metrics(m_temp_cr);

	if(fm)
	{
		fm->ascent		= fnt->metrics().ascent;
		fm->descent		= fnt->metrics().descent;
		fm->height		= fnt->metrics().height;
		fm->x_height	= fnt->metrics().x_height;
		if(italic == litehtml::font_style_italic || decoration)
		{
			fm->draw_spaces = true;
		} else
		{
			fm->draw_spaces = false;
		}
	}

	cairo_restore(m_temp_cr);

	return (litehtml::uint_ptr) fnt;
}

void cairo_container::delete_font( litehtml::uint_ptr hFont )
{
	cairo_font* fnt = (cairo_font*) hFont;
	if(fnt)
	{
		delete fnt;
	}
}

int cairo_container::text_width( const char* text, litehtml::uint_ptr hFont )
{
	cairo_font* fnt = (cairo_font*) hFont;
	
	cairo_save(m_temp_cr);
	int ret = fnt->text_width(m_temp_cr, text);
	cairo_restore(m_temp_cr);
	return ret;
}

void cairo_container::draw_text( litehtml::uint_ptr hdc, const char* text, litehtml::uint_ptr hFont, litehtml::web_color color, const litehtml::position& pos )
{
	if(hFont)
	{
		cairo_font* fnt = (cairo_font*) hFont;
		cairo_t* cr		= (cairo_t*) hdc;
		cairo_save(cr);

		apply_clip(cr);

		int x = pos.left();
		int y = pos.bottom() - fnt->metrics().descent;

		set_color(cr, color);
		fnt->show_text(cr, x, y, text);

		cairo_restore(cr);
	}
}

int cairo_container::pt_to_px( int pt ) const
{
	HDC dc = GetDC(NULL);
	int ret = MulDiv(pt, GetDeviceCaps(dc, LOGPIXELSY), 72);
	ReleaseDC(NULL, dc);
	return ret;
}

int cairo_container::get_default_font_size() const
{
	return 16;
}

void cairo_container::draw_list_marker( litehtml::uint_ptr hdc, const litehtml::list_marker& marker )
{
	if(!marker.image.empty())
	{
		std::wstring url;
		make_url_utf8(marker.image.c_str(), marker.baseurl, url);

		lock_images_cache();
		images_map::iterator img_i = m_images.find(url.c_str());
		if(img_i != m_images.end())
		{
			if(img_i->second)
			{
				draw_txdib((cairo_t*)hdc, img_i->second.get(), marker.pos.x, marker.pos.y, marker.pos.width, marker.pos.height);
				unlock_images_cache();
				return;
			}
		}
		unlock_images_cache();
	}

	switch(marker.marker_type)
	{
	case litehtml::list_style_type_circle:
		{
			draw_ellipse((cairo_t*) hdc, marker.pos.x, marker.pos.y, marker.pos.width, marker.pos.height, marker.color, 0.5);
		}
		break;
	case litehtml::list_style_type_disc:
		{
			fill_ellipse((cairo_t*) hdc, marker.pos.x, marker.pos.y, marker.pos.width, marker.pos.height, marker.color);
		}
		break;
	case litehtml::list_style_type_square:
		if(hdc)
		{
			cairo_t* cr = (cairo_t*) hdc;
			cairo_save(cr);

			cairo_new_path(cr);
			cairo_rectangle(cr, marker.pos.x, marker.pos.y, marker.pos.width, marker.pos.height);

			set_color(cr, marker.color);
			cairo_fill(cr);
			cairo_restore(cr);
		}
		break;
	}
}

void cairo_container::load_image( const char* src, const char* baseurl, bool redraw_on_ready )
{
	std::wstring url;
	make_url_utf8(src, baseurl, url);
	lock_images_cache();
	if(m_images.find(url.c_str()) == m_images.end())
	{
		unlock_images_cache();
		image_ptr img = get_image(url.c_str(), redraw_on_ready);
		lock_images_cache();
		m_images[url] = img;
		unlock_images_cache();
	} else
	{
		unlock_images_cache();
	}

}

void cairo_container::get_image_size( const char* src, const char* baseurl, litehtml::size& sz )
{
	std::wstring url;
	make_url_utf8(src, baseurl, url);

	sz.width	= 0;
	sz.height	= 0;

	lock_images_cache();
	images_map::iterator img = m_images.find(url.c_str());
	if(img != m_images.end())
	{
		if(img->second)
		{
			sz.width	= img->second->getWidth();
			sz.height	= img->second->getHeight();
		}
	}
	unlock_images_cache();
}

void cairo_container::draw_image( litehtml::uint_ptr hdc, const char* src, const char* baseurl, const litehtml::position& pos )
{
	cairo_t* cr = (cairo_t*) hdc;
	cairo_save(cr);
	apply_clip(cr);

	std::wstring url;
	make_url_utf8(src, baseurl, url);
	lock_images_cache();
	images_map::iterator img = m_images.find(url.c_str());
	if(img != m_images.end())
	{
		if(img->second)
		{
			draw_txdib(cr, img->second.get(), pos.x, pos.y, pos.width, pos.height);
		}
	}
	unlock_images_cache();
	cairo_restore(cr);
}

void cairo_container::draw_background( litehtml::uint_ptr hdc, const std::vector<litehtml::background_paint>& bgvec )
{
	cairo_t* cr = (cairo_t*) hdc;
	cairo_save(cr);
	apply_clip(cr);

	const auto& bg = bgvec.back();

	rounded_rectangle(cr, bg.border_box, bg.border_radius);
	cairo_clip(cr);

	cairo_rectangle(cr, bg.clip_box.x, bg.clip_box.y, bg.clip_box.width, bg.clip_box.height);
	cairo_clip(cr);

	if(bg.color.alpha)
	{
		set_color(cr, bg.color);
		cairo_paint(cr);
	}

	for (int i = (int)bgvec.size() - 1; i >= 0; i--)
	{
		const auto& bg = bgvec[i];

		cairo_rectangle(cr, bg.clip_box.x, bg.clip_box.y, bg.clip_box.width, bg.clip_box.height);
		cairo_clip(cr);

		std::wstring url;
		make_url_utf8(bg.image.c_str(), bg.baseurl.c_str(), url);

		lock_images_cache();
		auto img_i = m_images.find(url);
		if (img_i != m_images.end() && img_i->second)
		{
			image_ptr bgbmp = img_i->second;

			image_ptr new_img;
			if (bg.image_size.width != bgbmp->getWidth() || bg.image_size.height != bgbmp->getHeight())
			{
				new_img = image_ptr(new CTxDIB);
				bgbmp->resample(bg.image_size.width, bg.image_size.height, new_img.get());
				bgbmp = new_img;
			}


			cairo_surface_t* img = cairo_image_surface_create_for_data((unsigned char*)bgbmp->getBits(), CAIRO_FORMAT_ARGB32, bgbmp->getWidth(), bgbmp->getHeight(), bgbmp->getWidth() * 4);
			cairo_pattern_t* pattern = cairo_pattern_create_for_surface(img);
			cairo_matrix_t flib_m;
			cairo_matrix_init(&flib_m, 1, 0, 0, -1, 0, 0);
			cairo_matrix_translate(&flib_m, -bg.position_x, -bg.position_y);
			cairo_pattern_set_extend(pattern, CAIRO_EXTEND_REPEAT);
			cairo_pattern_set_matrix(pattern, &flib_m);

			switch (bg.repeat)
			{
			case litehtml::background_repeat_no_repeat:
				draw_txdib(cr, bgbmp.get(), bg.position_x, bg.position_y, bgbmp->getWidth(), bgbmp->getHeight());
				break;

			case litehtml::background_repeat_repeat_x:
				cairo_set_source(cr, pattern);
				cairo_rectangle(cr, bg.clip_box.left(), bg.position_y, bg.clip_box.width, bgbmp->getHeight());
				cairo_fill(cr);
				break;

			case litehtml::background_repeat_repeat_y:
				cairo_set_source(cr, pattern);
				cairo_rectangle(cr, bg.position_x, bg.clip_box.top(), bgbmp->getWidth(), bg.clip_box.height);
				cairo_fill(cr);
				break;

			case litehtml::background_repeat_repeat:
				cairo_set_source(cr, pattern);
				cairo_rectangle(cr, bg.clip_box.left(), bg.clip_box.top(), bg.clip_box.width, bg.clip_box.height);
				cairo_fill(cr);
				break;
			}

			cairo_pattern_destroy(pattern);
			cairo_surface_destroy(img);
		}
		unlock_images_cache();
	}
	
	cairo_restore(cr);
}

bool cairo_container::add_path_arc(cairo_t* cr, double x, double y, double rx, double ry, double a1, double a2, bool neg)
{
	if(rx > 0 && ry > 0)
	{
		cairo_save(cr);

		cairo_translate(cr, x, y);
		cairo_scale(cr, 1, ry / rx);
		cairo_translate(cr, -x, -y);

		if(neg)
		{
			cairo_arc_negative(cr, x, y, rx, a1, a2);
		} else
		{
			cairo_arc(cr, x, y, rx, a1, a2);
		}

		cairo_restore(cr);
		return true;
	}
	return false;
}

void cairo_container::draw_borders( litehtml::uint_ptr hdc, const litehtml::borders& borders, const litehtml::position& draw_pos, bool root )
{
	cairo_t* cr = (cairo_t*) hdc;
	cairo_save(cr);
	apply_clip(cr);

	cairo_new_path(cr);

	int bdr_top		= 0;
	int bdr_bottom	= 0;
	int bdr_left	= 0;
	int bdr_right	= 0;

	if(borders.top.width != 0 && borders.top.style > litehtml::border_style_hidden)
	{
		bdr_top = (int) borders.top.width;
	}
	if(borders.bottom.width != 0 && borders.bottom.style > litehtml::border_style_hidden)
	{
		bdr_bottom = (int) borders.bottom.width;
	}
	if(borders.left.width != 0 && borders.left.style > litehtml::border_style_hidden)
	{
		bdr_left = (int) borders.left.width;
	}
	if(borders.right.width != 0 && borders.right.style > litehtml::border_style_hidden)
	{
		bdr_right = (int) borders.right.width;
	}

	// draw right border
	if (bdr_right)
	{
		set_color(cr, borders.right.color);

		double r_top	= (double) borders.radius.top_right_x;
		double r_bottom	= (double) borders.radius.bottom_right_x;

		if(r_top)
		{
			double end_angle	= 2.0 * M_PI;
			double start_angle	= end_angle - M_PI / 2.0  / ((double) bdr_top / (double) bdr_right + 0.5);

			if (!add_path_arc(cr,
					draw_pos.right() - r_top,
					draw_pos.top() + r_top,
					r_top - bdr_right,
					r_top - bdr_right + (bdr_right - bdr_top),
					end_angle,
					start_angle, true))
			{
				cairo_move_to(cr, draw_pos.right() - bdr_right, draw_pos.top() + bdr_top);
			}

			if (!add_path_arc(cr,
					draw_pos.right() - r_top,
					draw_pos.top() + r_top,
					r_top,
					r_top,
					start_angle,
					end_angle, false))
			{
				cairo_line_to(cr, draw_pos.right(), draw_pos.top());
			}
		} else
		{
			cairo_move_to(cr, draw_pos.right() - bdr_right, draw_pos.top() + bdr_top);
			cairo_line_to(cr, draw_pos.right(), draw_pos.top());
		}

		if(r_bottom)
		{
			cairo_line_to(cr, draw_pos.right(),	draw_pos.bottom() - r_bottom);

			double start_angle	= 0;
			double end_angle	= start_angle + M_PI / 2.0  / ((double) bdr_bottom / (double) bdr_right + 0.5);

			if (!add_path_arc(cr,
				draw_pos.right() - r_bottom,
				draw_pos.bottom() - r_bottom,
				r_bottom,
				r_bottom,
				start_angle,
				end_angle, false))
			{
				cairo_line_to(cr, draw_pos.right(), draw_pos.bottom());
			}

			if (!add_path_arc(cr,
				draw_pos.right() - r_bottom,
				draw_pos.bottom() - r_bottom,
				r_bottom - bdr_right,
				r_bottom - bdr_right + (bdr_right - bdr_bottom),
				end_angle,
				start_angle, true))
			{
				cairo_line_to(cr, draw_pos.right() - bdr_right, draw_pos.bottom() - bdr_bottom);
			}
		} else
		{
			cairo_line_to(cr, draw_pos.right(),	draw_pos.bottom());
			cairo_line_to(cr, draw_pos.right() - bdr_right,	draw_pos.bottom() - bdr_bottom);
		}

		cairo_fill(cr);
	}

	// draw bottom border
	if(bdr_bottom)
	{
		set_color(cr, borders.bottom.color);

		double r_left	= borders.radius.bottom_left_x;
		double r_right	= borders.radius.bottom_right_x;

		if(r_left)
		{
			double start_angle	= M_PI / 2.0;
			double end_angle	= start_angle + M_PI / 2.0  / ((double) bdr_left / (double) bdr_bottom + 0.5);

			if (!add_path_arc(cr,
				draw_pos.left() + r_left,
				draw_pos.bottom() - r_left,
				r_left - bdr_bottom + (bdr_bottom - bdr_left),
				r_left - bdr_bottom,
				start_angle,
				end_angle, false))
			{
				cairo_move_to(cr, draw_pos.left() + bdr_left, draw_pos.bottom() - bdr_bottom);
			}

			if (!add_path_arc(cr,
				draw_pos.left() + r_left,
				draw_pos.bottom() - r_left,
				r_left,
				r_left,
				end_angle,
				start_angle, true))
			{
				cairo_line_to(cr, draw_pos.left(), draw_pos.bottom());
			}
		} else
		{
			cairo_move_to(cr, draw_pos.left(), draw_pos.bottom());
			cairo_line_to(cr, draw_pos.left() + bdr_left, draw_pos.bottom() - bdr_bottom);
		}

		if(r_right)
		{
			cairo_line_to(cr, draw_pos.right() - r_right,	draw_pos.bottom());

			double end_angle	= M_PI / 2.0;
			double start_angle	= end_angle - M_PI / 2.0  / ((double) bdr_right / (double) bdr_bottom + 0.5);

			if (!add_path_arc(cr,
				draw_pos.right() - r_right,
				draw_pos.bottom() - r_right,
				r_right,
				r_right,
				end_angle,
				start_angle, true))
			{
				cairo_line_to(cr, draw_pos.right(), draw_pos.bottom());
			}

			if (!add_path_arc(cr,
				draw_pos.right() - r_right,
				draw_pos.bottom() - r_right,
				r_right - bdr_bottom + (bdr_bottom - bdr_right),
				r_right - bdr_bottom,
				start_angle,
				end_angle, false))
			{
				cairo_line_to(cr, draw_pos.right() - bdr_right, draw_pos.bottom() - bdr_bottom);
			}
		} else
		{
			cairo_line_to(cr, draw_pos.right() - bdr_right,	draw_pos.bottom() - bdr_bottom);
			cairo_line_to(cr, draw_pos.right(),	draw_pos.bottom());
		}

		cairo_fill(cr);
	}

	// draw top border
	if(bdr_top)
	{
		set_color(cr, borders.top.color);

		double r_left	= borders.radius.top_left_x;
		double r_right	= borders.radius.top_right_x;

		if(r_left)
		{
			double end_angle	= M_PI * 3.0 / 2.0;
			double start_angle	= end_angle - M_PI / 2.0  / ((double) bdr_left / (double) bdr_top + 0.5);

			if (!add_path_arc(cr,
				draw_pos.left() + r_left,
				draw_pos.top() + r_left,
				r_left,
				r_left,
				end_angle,
				start_angle, true))
			{
				cairo_move_to(cr, draw_pos.left(), draw_pos.top());
			}

			if (!add_path_arc(cr,
				draw_pos.left() + r_left,
				draw_pos.top() + r_left,
				r_left - bdr_top + (bdr_top - bdr_left),
				r_left - bdr_top,
				start_angle,
				end_angle, false))
			{
				cairo_line_to(cr, draw_pos.left() + bdr_left, draw_pos.top() + bdr_top);
			}
		} else
		{
			cairo_move_to(cr, draw_pos.left(), draw_pos.top());
			cairo_line_to(cr, draw_pos.left() + bdr_left, draw_pos.top() + bdr_top);
		}

		if(r_right)
		{
			cairo_line_to(cr, draw_pos.right() - r_right,	draw_pos.top() + bdr_top);

			double start_angle	= M_PI * 3.0 / 2.0;
			double end_angle	= start_angle + M_PI / 2.0  / ((double) bdr_right / (double) bdr_top + 0.5);

			if (!add_path_arc(cr,
				draw_pos.right() - r_right,
				draw_pos.top() + r_right,
				r_right - bdr_top + (bdr_top - bdr_right),
				r_right - bdr_top,
				start_angle,
				end_angle, false))
			{
				cairo_line_to(cr, draw_pos.right() - bdr_right, draw_pos.top() + bdr_top);
			}

			if (!add_path_arc(cr,
				draw_pos.right() - r_right,
				draw_pos.top() + r_right,
				r_right,
				r_right,
				end_angle,
				start_angle, true))
			{
				cairo_line_to(cr, draw_pos.right(), draw_pos.top());
			}
		} else
		{
			cairo_line_to(cr, draw_pos.right() - bdr_right,	draw_pos.top() + bdr_top);
			cairo_line_to(cr, draw_pos.right(),	draw_pos.top());
		}

		cairo_fill(cr);
	}

	// draw left border
	if (bdr_left)
	{
		set_color(cr, borders.left.color);

		double r_top	= borders.radius.top_left_x;
		double r_bottom	= borders.radius.bottom_left_x;

		if(r_top)
		{
			double start_angle	= M_PI;
			double end_angle	= start_angle + M_PI / 2.0  / ((double) bdr_top / (double) bdr_left + 0.5);

			if (!add_path_arc(cr,
				draw_pos.left() + r_top,
				draw_pos.top() + r_top,
				r_top - bdr_left,
				r_top - bdr_left + (bdr_left - bdr_top),
				start_angle,
				end_angle, false))
			{
				cairo_move_to(cr, draw_pos.left() + bdr_left, draw_pos.top() + bdr_top);
			}

			if (!add_path_arc(cr,
				draw_pos.left() + r_top,
				draw_pos.top() + r_top,
				r_top,
				r_top,
				end_angle,
				start_angle, true))
			{
				cairo_line_to(cr, draw_pos.left(), draw_pos.top());
			}
		} else
		{
			cairo_move_to(cr, draw_pos.left() + bdr_left, draw_pos.top() + bdr_top);
			cairo_line_to(cr, draw_pos.left(), draw_pos.top());
		}

		if(r_bottom)
		{
			cairo_line_to(cr, draw_pos.left(),	draw_pos.bottom() - r_bottom);

			double end_angle	= M_PI;
			double start_angle	= end_angle - M_PI / 2.0  / ((double) bdr_bottom / (double) bdr_left + 0.5);

			if (!add_path_arc(cr,
				draw_pos.left() + r_bottom,
				draw_pos.bottom() - r_bottom,
				r_bottom,
				r_bottom,
				end_angle,
				start_angle, true))
			{
				cairo_line_to(cr, draw_pos.left(), draw_pos.bottom());
			}

			if (!add_path_arc(cr,
				draw_pos.left() + r_bottom,
				draw_pos.bottom() - r_bottom,
				r_bottom - bdr_left,
				r_bottom - bdr_left + (bdr_left - bdr_bottom),
				start_angle,
				end_angle, false))
			{
				cairo_line_to(cr, draw_pos.left() + bdr_left, draw_pos.bottom() - bdr_bottom);
			}
		} else
		{
			cairo_line_to(cr, draw_pos.left(),	draw_pos.bottom());
			cairo_line_to(cr, draw_pos.left() + bdr_left,	draw_pos.bottom() - bdr_bottom);
		}

		cairo_fill(cr);
	}
	cairo_restore(cr);
}

void cairo_container::set_clip(const litehtml::position& pos, const litehtml::border_radiuses& bdr_radius)
{
	m_clips.emplace_back(pos, bdr_radius);
}

void cairo_container::del_clip()
{
	if(!m_clips.empty())
	{
		m_clips.pop_back();
	}
}

void cairo_container::apply_clip( cairo_t* cr )
{
	for(const auto& clip_box : m_clips)
	{
		rounded_rectangle(cr, clip_box.box, clip_box.radius);
		cairo_clip(cr);
	}
}

void cairo_container::draw_ellipse( cairo_t* cr, int x, int y, int width, int height, const litehtml::web_color& color, double line_width )
{
	if(!cr) return;
	cairo_save(cr);

	apply_clip(cr);

	cairo_new_path(cr);

	cairo_translate (cr, x + width / 2.0, y + height / 2.0);
	cairo_scale (cr, width / 2.0, height / 2.0);
	cairo_arc (cr, 0, 0, 1, 0, 2 * M_PI);

	set_color(cr, color);
	cairo_set_line_width(cr, line_width);
	cairo_stroke(cr);

	cairo_restore(cr);
}

void cairo_container::fill_ellipse( cairo_t* cr, int x, int y, int width, int height, const litehtml::web_color& color )
{
	if(!cr) return;
	cairo_save(cr);

	apply_clip(cr);

	cairo_new_path(cr);

	cairo_translate (cr, x + width / 2.0, y + height / 2.0);
	cairo_scale (cr, width / 2.0, height / 2.0);
	cairo_arc (cr, 0, 0, 1, 0, 2 * M_PI);

	set_color(cr, color);
	cairo_fill(cr);

	cairo_restore(cr);
}

void cairo_container::clear_images()
{
	lock_images_cache();
	m_images.clear();
	unlock_images_cache();
}

const char* cairo_container::get_default_font_name() const
{
	return "Times New Roman";
}

void cairo_container::draw_txdib( cairo_t* cr, CTxDIB* bmp, int x, int y, int cx, int cy )
{
	cairo_save(cr);

	cairo_matrix_t flib_m;
	cairo_matrix_init(&flib_m, 1, 0, 0, -1, 0, 0);

	cairo_surface_t* img = NULL;

	CTxDIB rbmp;

	if(cx != bmp->getWidth() || cy != bmp->getHeight())
	{
		bmp->resample(cx, cy, &rbmp);
		img = cairo_image_surface_create_for_data((unsigned char*) rbmp.getBits(), CAIRO_FORMAT_ARGB32, rbmp.getWidth(), rbmp.getHeight(), rbmp.getWidth() * 4);
		cairo_matrix_translate(&flib_m, 0, -rbmp.getHeight());
		cairo_matrix_translate(&flib_m, x, -y);
	} else
	{
		img = cairo_image_surface_create_for_data((unsigned char*) bmp->getBits(), CAIRO_FORMAT_ARGB32, bmp->getWidth(), bmp->getHeight(), bmp->getWidth() * 4);
		cairo_matrix_translate(&flib_m, 0, -bmp->getHeight());
		cairo_matrix_translate(&flib_m, x, -y);
	}

	cairo_transform(cr, &flib_m);
	cairo_set_source_surface(cr, img, 0, 0);
	cairo_paint(cr);

	cairo_restore(cr);
	cairo_surface_destroy(img);
}

void cairo_container::rounded_rectangle(cairo_t* cr, const litehtml::position& pos, const litehtml::border_radiuses& radius)
{
	cairo_new_path(cr);
	if(radius.top_left_x)
	{
		cairo_arc(cr, pos.left() + radius.top_left_x, pos.top() + radius.top_left_x, radius.top_left_x, M_PI, M_PI * 3.0 / 2.0);
	} else
	{
		cairo_move_to(cr, pos.left(), pos.top());
	}

	cairo_line_to(cr, pos.right() - radius.top_right_x, pos.top());

	if(radius.top_right_x)
	{
		cairo_arc(cr, pos.right() - radius.top_right_x, pos.top() + radius.top_right_x, radius.top_right_x, M_PI * 3.0 / 2.0, 2.0 * M_PI);
	}

	cairo_line_to(cr, pos.right(), pos.bottom() - radius.bottom_right_x);

	if(radius.bottom_right_x)
	{
		cairo_arc(cr, pos.right() - radius.bottom_right_x, pos.bottom() - radius.bottom_right_x, radius.bottom_right_x, 0, M_PI / 2.0);
	}

	cairo_line_to(cr, pos.left() - radius.bottom_left_x, pos.bottom());

	if(radius.bottom_left_x)
	{
		cairo_arc(cr, pos.left() + radius.bottom_left_x, pos.bottom() - radius.bottom_left_x, radius.bottom_left_x, M_PI / 2.0, M_PI);
	}
}

void cairo_container::remove_image( std::wstring& url )
{
	lock_images_cache();
	images_map::iterator i = m_images.find(url);
	if(i != m_images.end())
	{
		m_images.erase(i);
	}
	unlock_images_cache();
}

void cairo_container::add_image(std::wstring& url, image_ptr& img)
{
	lock_images_cache();
	images_map::iterator i = m_images.find(url);
	if(i != m_images.end())
	{
		if(img)
		{
			i->second = img;
		} else
		{
			m_images.erase(i);
		}
	}
	unlock_images_cache();
}

void cairo_container::lock_images_cache()
{
	EnterCriticalSection(&m_img_sync);
}

void cairo_container::unlock_images_cache()
{
	LeaveCriticalSection(&m_img_sync);
}

std::shared_ptr<litehtml::element> cairo_container::create_element(const char* tag_name, const litehtml::string_map& attributes, const std::shared_ptr<litehtml::document>& doc)
{
	return 0;
}

void cairo_container::get_media_features(litehtml::media_features& media)  const
{
	litehtml::position client;
	get_client_rect(client);
	HDC hdc = GetDC(NULL);

	media.type			= litehtml::media_type_screen;
	media.width			= client.width;
	media.height		= client.height;
	media.color			= 8;
	media.monochrome	= 0;
	media.color_index	= 256;
	media.resolution	= GetDeviceCaps(hdc, LOGPIXELSX);
	media.device_width	= GetDeviceCaps(hdc, HORZRES);
	media.device_height	= GetDeviceCaps(hdc, VERTRES);

	ReleaseDC(NULL, hdc);
}

void cairo_container::get_language(litehtml::string& language, litehtml::string & culture) const
{
	language = "en";
	culture = "";
}

void cairo_container::make_url_utf8( const char* url, const char* basepath, std::wstring& out )
{
	wchar_t* urlW = cairo_font::utf8_to_wchar(url);
	wchar_t* basepathW = cairo_font::utf8_to_wchar(basepath);
	make_url(urlW, basepathW, out);

	if(urlW) delete urlW;
	if(basepathW) delete basepathW;
}

void cairo_container::transform_text( litehtml::string& text, litehtml::text_transform tt )
{
	if(text.empty()) return;

	LPWSTR txt = cairo_font::utf8_to_wchar(text.c_str());
	switch(tt)
	{
	case litehtml::text_transform_capitalize:
		CharUpperBuff(txt, 1);
		break;
	case litehtml::text_transform_uppercase:
		CharUpperBuff(txt, lstrlen(txt));
		break;
	case litehtml::text_transform_lowercase:
		CharLowerBuff(txt, lstrlen(txt));
		break;
	}
	LPSTR txtA = cairo_font::wchar_to_utf8(txt);
	text = txtA;
	delete txtA;
	delete txt;
}

void cairo_container::link(const std::shared_ptr<litehtml::document>& doc, const litehtml::element::ptr& el)
{
}

litehtml::string cairo_container::resolve_color(const litehtml::string& color) const
{
	struct custom_color 
	{
		const char*	name;
		int					color_index;
	};

	static custom_color colors[] = {
		{ "ActiveBorder",          COLOR_ACTIVEBORDER},
		{ "ActiveCaption",         COLOR_ACTIVECAPTION},
		{ "AppWorkspace",          COLOR_APPWORKSPACE },
		{ "Background",            COLOR_BACKGROUND },
		{ "ButtonFace",            COLOR_BTNFACE },
		{ "ButtonHighlight",       COLOR_BTNHIGHLIGHT },
		{ "ButtonShadow",          COLOR_BTNSHADOW },
		{ "ButtonText",            COLOR_BTNTEXT },
		{ "CaptionText",           COLOR_CAPTIONTEXT },
        { "GrayText",              COLOR_GRAYTEXT },
		{ "Highlight",             COLOR_HIGHLIGHT },
		{ "HighlightText",         COLOR_HIGHLIGHTTEXT },
		{ "InactiveBorder",        COLOR_INACTIVEBORDER },
		{ "InactiveCaption",       COLOR_INACTIVECAPTION },
		{ "InactiveCaptionText",   COLOR_INACTIVECAPTIONTEXT },
		{ "InfoBackground",        COLOR_INFOBK },
		{ "InfoText",              COLOR_INFOTEXT },
		{ "Menu",                  COLOR_MENU },
		{ "MenuText",              COLOR_MENUTEXT },
		{ "Scrollbar",             COLOR_SCROLLBAR },
		{ "ThreeDDarkShadow",      COLOR_3DDKSHADOW },
		{ "ThreeDFace",            COLOR_3DFACE },
		{ "ThreeDHighlight",       COLOR_3DHILIGHT },
		{ "ThreeDLightShadow",     COLOR_3DLIGHT },
		{ "ThreeDShadow",          COLOR_3DSHADOW },
		{ "Window",                COLOR_WINDOW },
		{ "WindowFrame",           COLOR_WINDOWFRAME },
		{ "WindowText",            COLOR_WINDOWTEXT }
	};

    for (auto& clr : colors)
    {
		if (!litehtml::t_strcasecmp(clr.name, color.c_str()))
        {
            char  str_clr[20];
            DWORD rgb_color =  GetSysColor(clr.color_index);
            StringCchPrintfA(str_clr, 20, "#%02X%02X%02X", GetRValue(rgb_color), GetGValue(rgb_color), GetBValue(rgb_color));
            return std::move(litehtml::string(str_clr));
        }
    }
    return std::move(litehtml::string());
}
