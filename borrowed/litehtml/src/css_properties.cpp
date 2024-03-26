#include "html.h"
#include "css_properties.h"
#include <cmath>

#define offset(member) ((uint_ptr)&this->member - (uint_ptr)this)

void litehtml::css_properties::compute(const element* el, const document::ptr& doc)
{
	compute_font(el, doc);
	int font_size = get_font_size();
	m_color = el->get_color_property(_color_, true, web_color::black, offset(m_color));

	m_el_position	 = (element_position) el->get_enum_property( _position_,		false,	element_position_static, offset(m_el_position));
	m_display		 = (style_display)	  el->get_enum_property( _display_,			false,	display_inline,			 offset(m_display));
	m_visibility	 = (visibility)		  el->get_enum_property( _visibility_,		true,	visibility_visible,		 offset(m_visibility));
	m_float			 = (element_float)	  el->get_enum_property( _float_,			false,	float_none,				 offset(m_float));
	m_clear			 = (element_clear)	  el->get_enum_property( _clear_,			false,	clear_none,				 offset(m_clear));
	m_box_sizing	 = (box_sizing)		  el->get_enum_property( _box_sizing_,		false,	box_sizing_content_box,	 offset(m_box_sizing));
	m_overflow		 = (overflow)		  el->get_enum_property( _overflow_,		false,	overflow_visible,		 offset(m_overflow));
	m_text_align	 = (text_align)		  el->get_enum_property( _text_align_,		true,	text_align_left,		 offset(m_text_align));
	m_vertical_align = (vertical_align)	  el->get_enum_property( _vertical_align_,	false,	va_baseline,			 offset(m_vertical_align));
	m_text_transform = (text_transform)	  el->get_enum_property( _text_transform_,	true,	text_transform_none,	 offset(m_text_transform));
	m_white_space	 = (white_space)	  el->get_enum_property( _white_space_,		true,	white_space_normal,		 offset(m_white_space));
	m_caption_side	 = (caption_side)	  el->get_enum_property( _caption_side_,	true,	caption_side_top,		 offset(m_caption_side));

	// https://www.w3.org/TR/CSS22/visuren.html#dis-pos-flo
	if (m_display == display_none)
	{
		// 1. If 'display' has the value 'none', then 'position' and 'float' do not apply. In this case, the element
		//    generates no box.
		m_float = float_none;
	} else
	{
		// 2. Otherwise, if 'position' has the value 'absolute' or 'fixed', the box is absolutely positioned,
		//    the computed value of 'float' is 'none', and display is set according to the table below.
		//    The position of the box will be determined by the 'top', 'right', 'bottom' and 'left' properties
		//    and the box's containing block.
		if (m_el_position == element_position_absolute || m_el_position == element_position_fixed)
		{
			m_float = float_none;

			if (m_display == display_inline_table)
			{
				m_display = display_table;
			} else if (m_display == display_inline ||
					   m_display == display_table_row_group ||
					   m_display == display_table_column ||
					   m_display == display_table_column_group ||
					   m_display == display_table_header_group ||
					   m_display == display_table_footer_group ||
					   m_display == display_table_row ||
					   m_display == display_table_cell ||
					   m_display == display_table_caption ||
					   m_display == display_inline_block)
			{
				m_display = display_block;
			}
		} else if (m_float != float_none)
		{
			// 3. Otherwise, if 'float' has a value other than 'none', the box is floated and 'display' is set
			//    according to the table below.
			if (m_display == display_inline_table)
			{
				m_display = display_table;
			} else if (m_display == display_inline ||
					   m_display == display_table_row_group ||
					   m_display == display_table_column ||
					   m_display == display_table_column_group ||
					   m_display == display_table_header_group ||
					   m_display == display_table_footer_group ||
					   m_display == display_table_row ||
					   m_display == display_table_cell ||
					   m_display == display_table_caption ||
					   m_display == display_inline_block)
			{
				m_display = display_block;
			}
		} else if(el->is_root())
		{
			// 4. Otherwise, if the element is the root element, 'display' is set according to the table below,
			//    except that it is undefined in CSS 2.2 whether a specified value of 'list-item' becomes a
			//    computed value of 'block' or 'list-item'.
			if (m_display == display_inline_table)
			{
				m_display = display_table;
			} else if (m_display == display_inline ||
				m_display == display_table_row_group ||
				m_display == display_table_column ||
				m_display == display_table_column_group ||
				m_display == display_table_header_group ||
				m_display == display_table_footer_group ||
				m_display == display_table_row ||
				m_display == display_table_cell ||
				m_display == display_table_caption ||
				m_display == display_inline_block ||
				m_display == display_list_item)
			{
				m_display = display_block;
			}
		}
	}
	// 5. Otherwise, the remaining 'display' property values apply as specified.

	const css_length _auto = css_length::predef_value(0);
	const css_length none = _auto, normal = _auto;

	m_css_width      = el->get_length_property(_width_,      false, _auto, offset(m_css_width));
	m_css_height     = el->get_length_property(_height_,     false, _auto, offset(m_css_height));

	m_css_min_width  = el->get_length_property(_min_width_,  false, _auto, offset(m_css_min_width));
	m_css_min_height = el->get_length_property(_min_height_, false, _auto, offset(m_css_min_height));

	m_css_max_width  = el->get_length_property(_max_width_,  false, none, offset(m_css_max_width));
	m_css_max_height = el->get_length_property(_max_height_, false, none, offset(m_css_max_height));

	doc->cvt_units(m_css_width, font_size);
	doc->cvt_units(m_css_height, font_size);

	doc->cvt_units(m_css_min_width, font_size);
	doc->cvt_units(m_css_min_height, font_size);

	doc->cvt_units(m_css_max_width, font_size);
	doc->cvt_units(m_css_max_height, font_size);

	m_css_margins.left   = el->get_length_property(_margin_left_,   false, 0, offset(m_css_margins.left));
	m_css_margins.right  = el->get_length_property(_margin_right_,  false, 0, offset(m_css_margins.right));
	m_css_margins.top    = el->get_length_property(_margin_top_,    false, 0, offset(m_css_margins.top));
	m_css_margins.bottom = el->get_length_property(_margin_bottom_, false, 0, offset(m_css_margins.bottom));

	doc->cvt_units(m_css_margins.left,	 font_size);
	doc->cvt_units(m_css_margins.right,	 font_size);
	doc->cvt_units(m_css_margins.top,	 font_size);
	doc->cvt_units(m_css_margins.bottom, font_size);

	m_css_padding.left   = el->get_length_property(_padding_left_,   false, 0, offset(m_css_padding.left));
	m_css_padding.right  = el->get_length_property(_padding_right_,  false, 0, offset(m_css_padding.right));
	m_css_padding.top    = el->get_length_property(_padding_top_,    false, 0, offset(m_css_padding.top));
	m_css_padding.bottom = el->get_length_property(_padding_bottom_, false, 0, offset(m_css_padding.bottom));

	doc->cvt_units(m_css_padding.left,	 font_size);
	doc->cvt_units(m_css_padding.right,	 font_size);
	doc->cvt_units(m_css_padding.top,	 font_size);
	doc->cvt_units(m_css_padding.bottom, font_size);

	m_css_borders.left.color   = el->get_color_property(_border_left_color_,   false, m_color, offset(m_css_borders.left.color));
	m_css_borders.right.color  = el->get_color_property(_border_right_color_,  false, m_color, offset(m_css_borders.right.color));
	m_css_borders.top.color    = el->get_color_property(_border_top_color_,    false, m_color, offset(m_css_borders.top.color));
	m_css_borders.bottom.color = el->get_color_property(_border_bottom_color_, false, m_color, offset(m_css_borders.bottom.color));

	m_css_borders.left.style   = (border_style) el->get_enum_property(_border_left_style_,   false, border_style_none, offset(m_css_borders.left.style));
	m_css_borders.right.style  = (border_style) el->get_enum_property(_border_right_style_,  false, border_style_none, offset(m_css_borders.right.style));
	m_css_borders.top.style    = (border_style) el->get_enum_property(_border_top_style_,    false, border_style_none, offset(m_css_borders.top.style));
	m_css_borders.bottom.style = (border_style) el->get_enum_property(_border_bottom_style_, false, border_style_none, offset(m_css_borders.bottom.style));

	m_css_borders.left.width   = el->get_length_property(_border_left_width_,   false, border_width_medium_value, offset(m_css_borders.left.width));
	m_css_borders.right.width  = el->get_length_property(_border_right_width_,  false, border_width_medium_value, offset(m_css_borders.right.width));
	m_css_borders.top.width    = el->get_length_property(_border_top_width_,    false, border_width_medium_value, offset(m_css_borders.top.width));
	m_css_borders.bottom.width = el->get_length_property(_border_bottom_width_, false, border_width_medium_value, offset(m_css_borders.bottom.width));

	if (m_css_borders.left.style == border_style_none || m_css_borders.left.style == border_style_hidden)
		m_css_borders.left.width = 0;
	if (m_css_borders.right.style == border_style_none || m_css_borders.right.style == border_style_hidden)
		m_css_borders.right.width = 0;
	if (m_css_borders.top.style == border_style_none || m_css_borders.top.style == border_style_hidden)
		m_css_borders.top.width = 0;
	if (m_css_borders.bottom.style == border_style_none || m_css_borders.bottom.style == border_style_hidden)
		m_css_borders.bottom.width = 0;

	doc->cvt_units(m_css_borders.left.width,	font_size);
	doc->cvt_units(m_css_borders.right.width,	font_size);
	doc->cvt_units(m_css_borders.top.width,		font_size);
	doc->cvt_units(m_css_borders.bottom.width,	font_size);

	m_css_borders.radius.top_left_x = el->get_length_property(_border_top_left_radius_x_, false, 0, offset(m_css_borders.radius.top_left_x));
	m_css_borders.radius.top_left_y = el->get_length_property(_border_top_left_radius_y_, false, 0, offset(m_css_borders.radius.top_left_y));

	m_css_borders.radius.top_right_x = el->get_length_property(_border_top_right_radius_x_, false, 0, offset(m_css_borders.radius.top_right_x));
	m_css_borders.radius.top_right_y = el->get_length_property(_border_top_right_radius_y_, false, 0, offset(m_css_borders.radius.top_right_y));

	m_css_borders.radius.bottom_left_x = el->get_length_property(_border_bottom_left_radius_x_, false, 0, offset(m_css_borders.radius.bottom_left_x));
	m_css_borders.radius.bottom_left_y = el->get_length_property(_border_bottom_left_radius_y_, false, 0, offset(m_css_borders.radius.bottom_left_y));

	m_css_borders.radius.bottom_right_x = el->get_length_property(_border_bottom_right_radius_x_, false, 0, offset(m_css_borders.radius.bottom_right_x));
	m_css_borders.radius.bottom_right_y = el->get_length_property(_border_bottom_right_radius_y_, false, 0, offset(m_css_borders.radius.bottom_right_y));

	doc->cvt_units( m_css_borders.radius.top_left_x,			font_size);
	doc->cvt_units( m_css_borders.radius.top_left_y,			font_size);
	doc->cvt_units( m_css_borders.radius.top_right_x,			font_size);
	doc->cvt_units( m_css_borders.radius.top_right_y,			font_size);
	doc->cvt_units( m_css_borders.radius.bottom_left_x,			font_size);
	doc->cvt_units( m_css_borders.radius.bottom_left_y,			font_size);
	doc->cvt_units( m_css_borders.radius.bottom_right_x,		font_size);
	doc->cvt_units( m_css_borders.radius.bottom_right_y,		font_size);

	m_border_collapse = (border_collapse) el->get_enum_property(_border_collapse_, true, border_collapse_separate, offset(m_border_collapse));

	m_css_border_spacing_x = el->get_length_property(__litehtml_border_spacing_x_, true, 0, offset(m_css_border_spacing_x));
	m_css_border_spacing_y = el->get_length_property(__litehtml_border_spacing_y_, true, 0, offset(m_css_border_spacing_y));

	doc->cvt_units(m_css_border_spacing_x, font_size);
	doc->cvt_units(m_css_border_spacing_y, font_size);

	m_css_offsets.left	 = el->get_length_property(_left_,	false, _auto, offset(m_css_offsets.left));
	m_css_offsets.right  = el->get_length_property(_right_, false, _auto, offset(m_css_offsets.right));
	m_css_offsets.top	 = el->get_length_property(_top_,	false, _auto, offset(m_css_offsets.top));
	m_css_offsets.bottom = el->get_length_property(_bottom_,false, _auto, offset(m_css_offsets.bottom));

	doc->cvt_units(m_css_offsets.left,   font_size);
	doc->cvt_units(m_css_offsets.right,  font_size);
	doc->cvt_units(m_css_offsets.top,    font_size);
	doc->cvt_units(m_css_offsets.bottom, font_size);

	m_z_index = el->get_length_property(_z_index_, false, _auto, offset(m_z_index));
	m_content = el->get_string_property(_content_, false, "", offset(m_content));
	m_cursor = el->get_string_property(_cursor_, true, "auto", offset(m_cursor));

	m_css_text_indent = el->get_length_property(_text_indent_, true, 0, offset(m_css_text_indent));
	doc->cvt_units(m_css_text_indent, font_size);

	m_css_line_height = el->get_length_property(_line_height_, true, normal, offset(m_css_line_height));
	if(m_css_line_height.is_predefined())
	{
		m_line_height = m_font_metrics.height;
	} else if(m_css_line_height.units() == css_units_none)
	{
		m_line_height = (int) std::nearbyint(m_css_line_height.val() * font_size);
	} else
	{
		m_line_height = doc->to_pixels(m_css_line_height, font_size, font_size);
		m_css_line_height = (float) m_line_height;
	}

	m_list_style_type     = (list_style_type)     el->get_enum_property(_list_style_type_,     true, list_style_type_disc,        offset(m_list_style_type));
	m_list_style_position = (list_style_position) el->get_enum_property(_list_style_position_, true, list_style_position_outside, offset(m_list_style_position));

	m_list_style_image = el->get_string_property(_list_style_image_, true, "", offset(m_list_style_image));
	if (!m_list_style_image.empty())
	{
		m_list_style_image_baseurl = el->get_string_property(_list_style_image_baseurl_, true, "", offset(m_list_style_image_baseurl));
		doc->container()->load_image(m_list_style_image.c_str(), m_list_style_image_baseurl.c_str(), true);
	}

	m_order = el->get_int_property(_order_, false, 0, offset(m_order));

	compute_background(el, doc);
	compute_flex(el, doc);
}

static const int font_size_table[8][7] =
{
		{ 9,    9,     9,     9,    11,    14,    18},
		{ 9,    9,     9,    10,    12,    15,    20},
		{ 9,    9,     9,    11,    13,    17,    22},
		{ 9,    9,    10,    12,    14,    18,    24},
		{ 9,    9,    10,    13,    16,    20,    26},
		{ 9,    9,    11,    14,    17,    21,    28},
		{ 9,   10,    12,    15,    17,    23,    30},
		{ 9,   10,    13,    16,    18,    24,    32}
};

void litehtml::css_properties::compute_font(const element* el, const document::ptr& doc)
{
	// initialize font size
	css_length sz = el->get_length_property(_font_size_, true, css_length::predef_value(font_size_medium), offset(m_font_size));

	int parent_sz = 0;
	int doc_font_size = doc->container()->get_default_font_size();
	element::ptr el_parent = el->parent();
	if (el_parent)
	{
		parent_sz = el_parent->css().get_font_size();
	} else
	{
		parent_sz = doc_font_size;
	}
	
	int font_size = parent_sz;

	if(sz.is_predefined())
	{
		int idx_in_table = doc_font_size - 9;
		if(idx_in_table >= 0 && idx_in_table <= 7)
		{
			if(sz.predef() >= font_size_xx_small && sz.predef() <= font_size_xx_large)
			{
				font_size = font_size_table[idx_in_table][sz.predef()];
			} else if(sz.predef() == font_size_smaller)
			{
				font_size = (int) (parent_sz / 1.2);
			}  else if(sz.predef() == font_size_larger)
			{
				font_size = (int) (parent_sz * 1.2);
			} else
			{
				font_size = parent_sz;
			}
		} else
		{
			switch(sz.predef())
			{
				case font_size_xx_small:
					font_size = doc_font_size * 3 / 5;
					break;
				case font_size_x_small:
					font_size = doc_font_size * 3 / 4;
					break;
				case font_size_small:
					font_size = doc_font_size * 8 / 9;
					break;
				case font_size_large:
					font_size = doc_font_size * 6 / 5;
					break;
				case font_size_x_large:
					font_size = doc_font_size * 3 / 2;
					break;
				case font_size_xx_large:
					font_size = doc_font_size * 2;
					break;
				case font_size_smaller:
					font_size = (int) (parent_sz / 1.2);
					break;
				case font_size_larger:
					font_size = (int) (parent_sz * 1.2);
					break;
				default:
					font_size = parent_sz;
					break;
			}
		}
	} else
	{
		if(sz.units() == css_units_percentage)
		{
			font_size = sz.calc_percent(parent_sz);
		} else
		{
			font_size = doc->to_pixels(sz, parent_sz);
		}
	}
	
	m_font_size = (float)font_size;

	// initialize font
	m_font_family		=               el->get_string_property(_font_family_,		true, doc->container()->get_default_font_name(),	offset(m_font_family));
	m_font_weight		= (font_weight) el->get_enum_property(  _font_weight_,		true, font_weight_normal,							offset(m_font_weight));
	m_font_style		= (font_style)  el->get_enum_property(  _font_style_,		true, font_style_normal,							offset(m_font_style));
	m_text_decoration	=               el->get_string_property(_text_decoration_,	true, "none",										offset(m_text_decoration));

	m_font = doc->get_font(
		m_font_family.c_str(), 
		font_size, 
		index_value(m_font_weight, font_weight_strings).c_str(), 
		index_value(m_font_style, font_style_strings).c_str(),
		m_text_decoration.c_str(), 
		&m_font_metrics);
}

void litehtml::css_properties::compute_background(const element* el, const document::ptr& doc)
{
	int font_size = get_font_size();

	m_bg.m_color		= el->get_color_property(_background_color_, false, web_color::transparent, offset(m_bg.m_color));

	const css_size auto_auto(css_length::predef_value(background_size_auto), css_length::predef_value(background_size_auto));
	m_bg.m_position_x	= el->get_length_vector_property(_background_position_x_, false, { css_length(0, css_units_percentage) }, offset(m_bg.m_position_x));
	m_bg.m_position_y	= el->get_length_vector_property(_background_position_y_, false, { css_length(0, css_units_percentage) }, offset(m_bg.m_position_y));
	m_bg.m_size			= el->get_size_vector_property  (_background_size_,       false, { auto_auto }, offset(m_bg.m_size));

	for (auto& x : m_bg.m_position_x) doc->cvt_units(x, font_size);
	for (auto& y : m_bg.m_position_y) doc->cvt_units(y, font_size);
	for (auto& size : m_bg.m_size)
	{
		doc->cvt_units(size.width,  font_size);
		doc->cvt_units(size.height, font_size);
	}

	m_bg.m_attachment = el->get_int_vector_property(_background_attachment_, false, { background_attachment_scroll }, offset(m_bg.m_attachment));
	m_bg.m_repeat     = el->get_int_vector_property(_background_repeat_,     false, { background_repeat_repeat },     offset(m_bg.m_repeat));
	m_bg.m_clip       = el->get_int_vector_property(_background_clip_,       false, { background_box_border },        offset(m_bg.m_clip));
	m_bg.m_origin     = el->get_int_vector_property(_background_origin_,     false, { background_box_padding },       offset(m_bg.m_origin));

	m_bg.m_image   = el->get_string_vector_property(_background_image_,  false, {""}, offset(m_bg.m_image));
	m_bg.m_baseurl = el->get_string_property(_background_image_baseurl_, false, "",   offset(m_bg.m_baseurl));

	for (const auto& image : m_bg.m_image)
	{
		if (!image.empty())
		{
			doc->container()->load_image(image.c_str(), m_bg.m_baseurl.c_str(), true);
		}
	}
}

void litehtml::css_properties::compute_flex(const element* el, const document::ptr& doc)
{
	if (m_display == display_flex || m_display == display_inline_flex)
	{
		m_flex_direction = (flex_direction) el->get_enum_property(_flex_direction_, false, flex_direction_row, offset(m_flex_direction));
		m_flex_wrap = (flex_wrap) el->get_enum_property(_flex_wrap_, false, flex_wrap_nowrap, offset(m_flex_wrap));

		m_flex_justify_content = (flex_justify_content) el->get_enum_property(_justify_content_, false, flex_justify_content_flex_start, offset(m_flex_justify_content));
		m_flex_align_items = (flex_align_items) el->get_enum_property(_align_items_, false, flex_align_items_flex_normal, offset(m_flex_align_items));
		m_flex_align_content = (flex_align_content) el->get_enum_property(_align_content_, false, flex_align_content_stretch, offset(m_flex_align_content));
	}
	m_flex_align_self = (flex_align_items) el->get_enum_property(_align_self_, false, flex_align_items_auto, offset(m_flex_align_self));
	auto parent = el->parent();
	if (parent && (parent->css().m_display == display_flex || parent->css().m_display == display_inline_flex))
	{
		m_flex_grow = el->get_number_property(_flex_grow_, false, 0, offset(m_flex_grow));
		m_flex_shrink = el->get_number_property(_flex_shrink_, false, 1, offset(m_flex_shrink));
		m_flex_basis = el->get_length_property(_flex_basis_, false, css_length::predef_value(flex_basis_auto), offset(m_flex_basis));
		if(!m_flex_basis.is_predefined() && m_flex_basis.units() == css_units_none && m_flex_basis.val() != 0)
		{
			// flex-basis property must contain units
			m_flex_basis.predef(flex_basis_auto);
		}
		doc->cvt_units(m_flex_basis, get_font_size());
		if(m_display == display_inline || m_display == display_inline_block)
		{
			m_display = display_block;
		} else if(m_display == display_inline_table)
		{
			m_display = display_table;
		} else if(m_display == display_inline_flex)
		{
			m_display = display_flex;
		}
	}
}

std::vector<std::tuple<litehtml::string, litehtml::string>> litehtml::css_properties::dump_get_attrs()
{
	std::vector<std::tuple<string, string>> ret;

	ret.emplace_back(std::make_tuple("display", index_value(m_display, style_display_strings)));
	ret.emplace_back(std::make_tuple("el_position", index_value(m_el_position, element_position_strings)));
	ret.emplace_back(std::make_tuple("text_align", index_value(m_text_align, text_align_strings)));
	ret.emplace_back(std::make_tuple("font_size", m_font_size.to_string()));
	ret.emplace_back(std::make_tuple("overflow", index_value(m_overflow, overflow_strings)));
	ret.emplace_back(std::make_tuple("white_space", index_value(m_white_space, white_space_strings)));
	ret.emplace_back(std::make_tuple("visibility", index_value(m_visibility, visibility_strings)));
	ret.emplace_back(std::make_tuple("box_sizing", index_value(m_box_sizing, box_sizing_strings)));
	ret.emplace_back(std::make_tuple("z_index", m_z_index.to_string()));
	ret.emplace_back(std::make_tuple("vertical_align", index_value(m_vertical_align, vertical_align_strings)));
	ret.emplace_back(std::make_tuple("float", index_value(m_float, element_float_strings)));
	ret.emplace_back(std::make_tuple("clear", index_value(m_clear, element_clear_strings)));
	ret.emplace_back(std::make_tuple("margins", m_css_margins.to_string()));
	ret.emplace_back(std::make_tuple("padding", m_css_padding.to_string()));
	ret.emplace_back(std::make_tuple("borders", m_css_borders.to_string()));
	ret.emplace_back(std::make_tuple("width", m_css_width.to_string()));
	ret.emplace_back(std::make_tuple("height", m_css_height.to_string()));
	ret.emplace_back(std::make_tuple("min_width", m_css_min_width.to_string()));
	ret.emplace_back(std::make_tuple("min_height", m_css_min_width.to_string()));
	ret.emplace_back(std::make_tuple("max_width", m_css_max_width.to_string()));
	ret.emplace_back(std::make_tuple("max_height", m_css_max_width.to_string()));
	ret.emplace_back(std::make_tuple("offsets", m_css_offsets.to_string()));
	ret.emplace_back(std::make_tuple("text_indent", m_css_text_indent.to_string()));
	ret.emplace_back(std::make_tuple("line_height", std::to_string(m_line_height)));
	ret.emplace_back(std::make_tuple("list_style_type", index_value(m_list_style_type, list_style_type_strings)));
	ret.emplace_back(std::make_tuple("list_style_position", index_value(m_list_style_position, list_style_position_strings)));
	ret.emplace_back(std::make_tuple("border_spacing_x", m_css_border_spacing_x.to_string()));
	ret.emplace_back(std::make_tuple("border_spacing_y", m_css_border_spacing_y.to_string()));

	return ret;
}
