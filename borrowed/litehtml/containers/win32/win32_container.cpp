#include "win32_container.h"

win32_container::win32_container()
{
	m_hClipRgn = NULL;
	m_tmp_hdc = GetDC(NULL);
	InitializeCriticalSection(&m_img_sync);

	EnumFonts(m_tmp_hdc, NULL, EnumFontsProc, (LPARAM)this);
	m_installed_fonts.insert(L"monospace");
	m_installed_fonts.insert(L"serif");
	m_installed_fonts.insert(L"sans-serif");
	m_installed_fonts.insert(L"fantasy");
	m_installed_fonts.insert(L"cursive");
}

win32_container::~win32_container()
{
	DeleteCriticalSection(&m_img_sync);
	if(m_hClipRgn)
	{
		DeleteObject(m_hClipRgn);
	}
	ReleaseDC(NULL, m_tmp_hdc);
}

int CALLBACK win32_container::EnumFontsProc(const LOGFONT* lplf, const TEXTMETRIC* lptm, DWORD dwType, LPARAM lpData)
{
	win32_container* container = (win32_container*)lpData;
	container->m_installed_fonts.insert(lplf->lfFaceName);
	return 1;
}

static LPCWSTR get_exact_font_name(LPCWSTR facename)
{
	if      (!lstrcmpi(facename, L"monospace"))		return L"Courier New";
	else if (!lstrcmpi(facename, L"serif"))			return L"Times New Roman";
	else if (!lstrcmpi(facename, L"sans-serif"))	return L"Arial";
	else if (!lstrcmpi(facename, L"fantasy"))		return L"Impact";
	else if (!lstrcmpi(facename, L"cursive"))		return L"Comic Sans MS";
	else											return facename;
}

static void trim_quotes(litehtml::string& str)
{
	if (str.front() == '"' || str.front() == '\'')
		str.erase(0, 1);

	if (str.back() == '"' || str.back() == '\'')
		str.erase(str.length() - 1, 1);
}

litehtml::uint_ptr win32_container::create_font( const char* font_list, int size, int weight, litehtml::font_style italic, unsigned int decoration, litehtml::font_metrics* fm )
{
	std::wstring font_name;
	litehtml::string_vector fonts;
	litehtml::split_string(font_list, fonts, ",");
	bool found = false;
	for (auto& name : fonts)
	{
		litehtml::trim(name);
		trim_quotes(name);
		std::wstring wname = (const wchar_t*)litehtml_to_wchar(name.c_str());
		if (m_installed_fonts.count(wname))
		{
			font_name = wname;
			found = true;
			break;
		}
	}
	if (!found) font_name = litehtml_to_wchar(get_default_font_name());
	font_name = get_exact_font_name(font_name.c_str());

	LOGFONT lf = {};
	wcscpy_s(lf.lfFaceName, LF_FACESIZE, font_name.c_str());

	lf.lfHeight			= -size;
	lf.lfWeight			= weight;
	lf.lfItalic			= (italic == litehtml::font_style_italic) ? TRUE : FALSE;
	lf.lfCharSet		= DEFAULT_CHARSET;
	lf.lfOutPrecision	= OUT_DEFAULT_PRECIS;
	lf.lfClipPrecision	= CLIP_DEFAULT_PRECIS;
	lf.lfQuality		= DEFAULT_QUALITY;
	lf.lfStrikeOut		= (decoration & litehtml::font_decoration_linethrough) ? TRUE : FALSE;
	lf.lfUnderline		= (decoration & litehtml::font_decoration_underline) ? TRUE : FALSE;
	HFONT hFont = CreateFontIndirect(&lf);

	if (fm)
	{
		SelectObject(m_tmp_hdc, hFont);
		TEXTMETRIC tm = {};
		GetTextMetrics(m_tmp_hdc, &tm);
		fm->ascent = tm.tmAscent;
		fm->descent = tm.tmDescent;
		fm->height = tm.tmHeight;
		fm->x_height = tm.tmHeight / 2;   // this is an estimate; call GetGlyphOutline to get the real value
		fm->draw_spaces = italic || decoration;
	}

	return (uint_ptr) hFont;
}

void win32_container::delete_font( uint_ptr hFont )
{
	DeleteObject((HFONT) hFont);
}

const char* win32_container::get_default_font_name() const
{
	return "Times New Roman";
}

int win32_container::get_default_font_size() const
{
	return 16;
}

int win32_container::text_width( const char* text, uint_ptr hFont )
{
	SIZE size = {};
	SelectObject(m_tmp_hdc, (HFONT)hFont);
	std::wstring wtext = (const wchar_t*)litehtml_to_wchar(text);
	GetTextExtentPoint32(m_tmp_hdc, wtext.c_str(), (int)wtext.size(), &size);
	return size.cx;
}

void win32_container::draw_text( uint_ptr hdc, const char* text, uint_ptr hFont, litehtml::web_color color, const litehtml::position& pos )
{
	apply_clip((HDC) hdc);

	HFONT oldFont = (HFONT) SelectObject((HDC) hdc, (HFONT) hFont);

	SetBkMode((HDC) hdc, TRANSPARENT);

	SetTextColor((HDC) hdc, RGB(color.red, color.green, color.blue));

	RECT rcText = { pos.left(), pos.top(), pos.right(), pos.bottom() };
	DrawText((HDC) hdc, litehtml_to_wchar(text), -1, &rcText, DT_SINGLELINE | DT_NOPREFIX | DT_BOTTOM | DT_NOCLIP);

	SelectObject((HDC) hdc, oldFont);

	release_clip((HDC) hdc);
}

int win32_container::pt_to_px( int pt ) const
{
	return MulDiv(pt, GetDeviceCaps(m_tmp_hdc, LOGPIXELSY), 72);
}

void win32_container::draw_list_marker(uint_ptr hdc, const litehtml::list_marker& marker)
{
	apply_clip((HDC)hdc);

	int top_margin = marker.pos.height / 3;
	if (top_margin < 4)
		top_margin = 0;

	int draw_x = marker.pos.x;
	int draw_y = marker.pos.y + top_margin;
	int draw_width = marker.pos.height - top_margin * 2;
	int draw_height = marker.pos.height - top_margin * 2;

	switch (marker.marker_type)
	{
	case litehtml::list_style_type_circle:
		{
			draw_ellipse((HDC)hdc, draw_x, draw_y, draw_width, draw_height, marker.color, 1);
		}
		break;
	case litehtml::list_style_type_disc:
		{
			fill_ellipse((HDC)hdc, draw_x, draw_y, draw_width, draw_height, marker.color);
		}
		break;
	case litehtml::list_style_type_square:
		{
			fill_rect((HDC)hdc, draw_x, draw_y, draw_width, draw_height, marker.color);
		}
		break;
	}
	release_clip((HDC)hdc);
}

void win32_container::make_url_utf8(const char* url, const char* basepath, std::wstring& out)
{
	make_url(litehtml::utf8_to_wchar(url), litehtml::utf8_to_wchar(basepath), out);
}

void win32_container::load_image( const char* src, const char* baseurl, bool redraw_on_ready )
{
	std::wstring url;
	make_url_utf8(src, baseurl, url);
	
	lock_images_cache();
	if (m_images.count(url) == 0)
	{
		unlock_images_cache();
		uint_ptr img = get_image(url.c_str(), redraw_on_ready);
		add_image(url.c_str(), img);
	}
	else
	{
		unlock_images_cache();
	}
}

void win32_container::add_image(LPCWSTR url, uint_ptr img)
{
	lock_images_cache();
	m_images[url] = img;
	unlock_images_cache();
}

void win32_container::get_image_size( const char* src, const char* baseurl, litehtml::size& sz )
{
	std::wstring url;
	make_url_utf8(src, baseurl, url);

	sz.width  = 0;
	sz.height = 0;

	lock_images_cache();
	images_map::iterator img = m_images.find(url);
	if(img != m_images.end() && img->second)
	{
		get_img_size(img->second, sz);
	}
	unlock_images_cache();
}

void win32_container::clear_images()
{
	lock_images_cache();
	for(auto& img : m_images)
	{
		if(img.second)
		{
			free_image(img.second);
		}
	}
	m_images.clear();
	unlock_images_cache();
}

void win32_container::lock_images_cache()
{
	EnterCriticalSection(&m_img_sync);
}

void win32_container::unlock_images_cache()
{
	LeaveCriticalSection(&m_img_sync);
}

void win32_container::draw_background( uint_ptr _hdc, const std::vector<litehtml::background_paint>& bg )
{
	HDC hdc = (HDC)_hdc;
	apply_clip(hdc);

	auto border_box = bg.back().border_box;
	auto color = bg.back().color;
	fill_rect(hdc, border_box.x, border_box.y, border_box.width, border_box.height, color);

	for (int i = (int)bg.size() - 1; i >= 0; i--)
	{
		std::wstring url;
		make_url_utf8(bg[i].image.c_str(), bg[i].baseurl.c_str(), url);

		lock_images_cache();
		images_map::iterator img = m_images.find(url);
		if (img != m_images.end() && img->second)
		{
			draw_img_bg(hdc, img->second, bg[i]);
		}
		unlock_images_cache();
	}

	release_clip(hdc);
}

void win32_container::set_clip( const litehtml::position& pos, const litehtml::border_radiuses& bdr_radius )
{
	m_clips.push_back(pos);
}

void win32_container::del_clip()
{
	if(!m_clips.empty())
	{
		m_clips.pop_back();
	}
}

void win32_container::apply_clip(HDC hdc)
{
	if(m_hClipRgn)
	{
		DeleteObject(m_hClipRgn);
		m_hClipRgn = NULL;
	}

	if(!m_clips.empty())
	{
		POINT ptView = {0, 0};
		GetWindowOrgEx(hdc, &ptView);

		litehtml::position clip_pos = m_clips.back();
		m_hClipRgn = CreateRectRgn(clip_pos.left() - ptView.x, clip_pos.top() - ptView.y, clip_pos.right() - ptView.x, clip_pos.bottom() - ptView.y);
		SelectClipRgn(hdc, m_hClipRgn);
	}
}

void win32_container::release_clip(HDC hdc)
{
	SelectClipRgn(hdc, NULL);

	if(m_hClipRgn)
	{
		DeleteObject(m_hClipRgn);
		m_hClipRgn = NULL;
	}
}

litehtml::element::ptr win32_container::create_element(const char* tag_name, const litehtml::string_map& attributes, const litehtml::document::ptr& doc)
{
	return 0;
}

void win32_container::get_media_features(litehtml::media_features& media)  const
{
	litehtml::position client;
	get_client_rect(client);

	media.type = litehtml::media_type_screen;
	media.width = client.width;
	media.height = client.height;
	media.color = 8;
	media.monochrome = 0;
	media.color_index = 256;
	media.resolution = GetDeviceCaps(m_tmp_hdc, LOGPIXELSX);
	media.device_width = GetDeviceCaps(m_tmp_hdc, HORZRES);
	media.device_height = GetDeviceCaps(m_tmp_hdc, VERTRES);
}

void win32_container::get_language(litehtml::string& language, litehtml::string& culture) const
{
	language = "en";
	culture = "";
}

void win32_container::transform_text(litehtml::string& text, litehtml::text_transform tt)
{
	if (text.empty()) return;

	LPWSTR txt = _wcsdup(litehtml_to_wchar(text.c_str()));
	switch (tt)
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
	text = litehtml_from_wchar(txt);
	free(txt);
}

void win32_container::link(const litehtml::document::ptr& doc, const litehtml::element::ptr& el)
{
}

litehtml::string win32_container::resolve_color(const litehtml::string& color) const
{
	struct custom_color
	{
		const char*	name;
		int					color_index;
	};

	static custom_color colors[] = {
		{ "ActiveBorder",          COLOR_ACTIVEBORDER },
		{ "ActiveCaption",         COLOR_ACTIVECAPTION },
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
		if (!litehtml::t_strcasecmp(color.c_str(), clr.name))
		{
			char  str_clr[20];
			DWORD rgb_color = GetSysColor(clr.color_index);
			t_snprintf(str_clr, 20, "#%02X%02X%02X", GetRValue(rgb_color), GetGValue(rgb_color), GetBValue(rgb_color));
			return std::move(litehtml::string(str_clr));
		}
	}
	return std::move(litehtml::string());
}
