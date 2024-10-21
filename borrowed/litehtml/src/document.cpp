#include "html.h"
#include "document.h"
#include "stylesheet.h"
#include "html_tag.h"
#include "el_text.h"
#include "el_para.h"
#include "el_space.h"
#include "el_body.h"
#include "el_image.h"
#include "el_table.h"
#include "el_td.h"
#include "el_link.h"
#include "el_title.h"
#include "el_style.h"
#include "el_script.h"
#include "el_comment.h"
#include "el_cdata.h"
#include "el_base.h"
#include "el_anchor.h"
#include "el_break.h"
#include "el_div.h"
#include "el_font.h"
#include "el_tr.h"
#include <cmath>
#include <cstdio>
#include <algorithm>
#include "gumbo.h"
#include "utf8_strings.h"
#include "render_item.h"
#include "render_table.h"
#include "render_block.h"

litehtml::document::document(document_container* objContainer)
{
	m_container	= objContainer;
}

litehtml::document::~document()
{
	m_over_element = nullptr;
	if(m_container)
	{
		for(auto& font : m_fonts)
		{
			m_container->delete_font(font.second.font);
		}
	}
}

litehtml::document::ptr litehtml::document::createFromString( const char* str, document_container* objPainter, const char* master_styles, const char* user_styles )
{
	// parse document into GumboOutput
	GumboOutput* output = gumbo_parse(str);

	// Create litehtml::document
	document::ptr doc = std::make_shared<document>(objPainter);

	// Create litehtml::elements.
	elements_list root_elements;
	doc->create_node(output->root, root_elements, true);
	if (!root_elements.empty())
	{
		doc->m_root = root_elements.back();
	}
	// Destroy GumboOutput
	gumbo_destroy_output(&kGumboDefaultOptions, output);

	if (master_styles && *master_styles)
	{
		doc->m_master_css.parse_stylesheet(master_styles, nullptr, doc, nullptr);
		doc->m_master_css.sort_selectors();
	}
	if (user_styles && *user_styles)
	{
		doc->m_user_css.parse_stylesheet(user_styles, nullptr, doc, nullptr);
		doc->m_user_css.sort_selectors();
	}

	// Let's process created elements tree
	if (doc->m_root)
	{
		doc->container()->get_media_features(doc->m_media);

		doc->m_root->set_pseudo_class(_root_, true);

		// apply master CSS
		doc->m_root->apply_stylesheet(doc->m_master_css);

		// parse elements attributes
		doc->m_root->parse_attributes();

		// parse style sheets linked in document
		media_query_list::ptr media;
		for (const auto& css : doc->m_css)
		{
			if (!css.media.empty())
			{
				media = media_query_list::create_from_string(css.media, doc);
			}
			else
			{
				media = nullptr;
			}
			doc->m_styles.parse_stylesheet(css.text.c_str(), css.baseurl.c_str(), doc, media);
		}
		// Sort css selectors using CSS rules.
		doc->m_styles.sort_selectors();

		// get current media features
		if (!doc->m_media_lists.empty())
		{
			doc->update_media_lists(doc->m_media);
		}

		// Apply parsed styles.
		doc->m_root->apply_stylesheet(doc->m_styles);

		// Apply user styles if any
		doc->m_root->apply_stylesheet(doc->m_user_css);

		// Initialize m_css
		doc->m_root->compute_styles();

		// Create rendering tree
		doc->m_root_render = doc->m_root->create_render_item(nullptr);

		// Now the m_tabular_elements is filled with tabular elements.
		// We have to check the tabular elements for missing table elements 
		// and create the anonymous boxes in visual table layout
		doc->fix_tables_layout();

		// Finally initialize elements
		// init() return pointer to the render_init element because it can change its type
		doc->m_root_render = doc->m_root_render->init();
	}

	return doc;
}

litehtml::uint_ptr litehtml::document::add_font( const char* name, int size, const char* weight, const char* style, const char* decoration, font_metrics* fm )
{
	uint_ptr ret = 0;

	if(!name)
	{
		name = m_container->get_default_font_name();
	}

	char strSize[20];
	t_itoa(size, strSize, 20, 10);

	string key = name;
	key += ":";
	key += strSize;
	key += ":";
	key += weight;
	key += ":";
	key += style;
	key += ":";
	key += decoration;

	if(m_fonts.find(key) == m_fonts.end())
	{
		font_style fs = (font_style) value_index(style, font_style_strings, font_style_normal);
		int	fw = value_index(weight, font_weight_strings, -1);
		if(fw >= 0)
		{
			switch(fw)
			{
			case litehtml::font_weight_bold:
				fw = 700;
				break;
			case litehtml::font_weight_bolder:
				fw = 600;
				break;
			case litehtml::font_weight_lighter:
				fw = 300;
				break;
			case litehtml::font_weight_normal:
				fw = 400;
				break;
			case litehtml::font_weight_100:
				fw = 100;
				break;
			case litehtml::font_weight_200:
				fw = 200;
				break;
			case litehtml::font_weight_300:
				fw = 300;
				break;
			case litehtml::font_weight_400:
				fw = 400;
				break;
			case litehtml::font_weight_500:
				fw = 500;
				break;
			case litehtml::font_weight_600:
				fw = 600;
				break;
			case litehtml::font_weight_700:
				fw = 700;
				break;
			case litehtml::font_weight_800:
				fw = 800;
				break;
			case litehtml::font_weight_900:
				fw = 900;
				break;
			}
		} else
		{
			fw = atoi(weight);
			if(fw < 100)
			{
				fw = 400;
			}
		}

		unsigned int decor = 0;

		if(decoration)
		{
			std::vector<string> tokens;
			split_string(decoration, tokens, " ");
			for(auto & token : tokens)
			{
				if(!t_strcasecmp(token.c_str(), "underline"))
				{
					decor |= font_decoration_underline;
				} else if(!t_strcasecmp(token.c_str(), "line-through"))
				{
					decor |= font_decoration_linethrough;
				} else if(!t_strcasecmp(token.c_str(), "overline"))
				{
					decor |= font_decoration_overline;
				}
			}
		}

		font_item fi= {0, {}};

		fi.font = m_container->create_font(name, size, fw, fs, decor, &fi.metrics);
		m_fonts[key] = fi;
		ret = fi.font;
		if(fm)
		{
			*fm = fi.metrics;
		}
	}
	return ret;
}

litehtml::uint_ptr litehtml::document::get_font( const char* name, int size, const char* weight, const char* style, const char* decoration, font_metrics* fm )
{
	if(!size)
	{
		return 0;
	}
	if(!name)
	{
		name = m_container->get_default_font_name();
	}

	char strSize[20];
	t_itoa(size, strSize, 20, 10);

	string key = name;
	key += ":";
	key += strSize;
	key += ":";
	key += weight;
	key += ":";
	key += style;
	key += ":";
	key += decoration;

	auto el = m_fonts.find(key);

	if(el != m_fonts.end())
	{
		if(fm)
		{
			*fm = el->second.metrics;
		}
		return el->second.font;
	}
	return add_font(name, size, weight, style, decoration, fm);
}

int litehtml::document::render( int max_width, render_type rt )
{
	int ret = 0;
	if(m_root)
	{
		position client_rc;
		m_container->get_client_rect(client_rc);
		containing_block_context cb_context;
		cb_context.width = max_width;
		cb_context.width.type = containing_block_context::cbc_value_type_absolute;
		cb_context.height = client_rc.height;
		cb_context.height.type = containing_block_context::cbc_value_type_absolute;

		if(rt == render_fixed_only)
		{
			m_fixed_boxes.clear();
			m_root_render->render_positioned(rt);
		} else
		{
			ret = m_root_render->render(0, 0, cb_context, nullptr);
			if(m_root_render->fetch_positioned())
			{
				m_fixed_boxes.clear();
				m_root_render->render_positioned(rt);
			}
			m_size.width	= 0;
			m_size.height	= 0;
			m_content_size.width = 0;
			m_content_size.height = 0;
			m_root_render->calc_document_size(m_size, m_content_size);
		}
	}
	return ret;
}

void litehtml::document::draw( uint_ptr hdc, int x, int y, const position* clip )
{
	if(m_root && m_root_render)
	{
		m_root->draw(hdc, x, y, clip, m_root_render);
		m_root_render->draw_stacking_context(hdc, x, y, clip, true);
	}
}

int litehtml::document::to_pixels( const char* str, int fontSize, bool* is_percent/*= 0*/ ) const
{
	if(!str)	return 0;
	
	css_length val;
	val.fromString(str);
	if(is_percent && val.units() == css_units_percentage && !val.is_predefined())
	{
		*is_percent = true;
	}
	return to_pixels(val, fontSize);
}

int litehtml::document::to_pixels( const css_length& val, int fontSize, int size ) const
{
	if(val.is_predefined())
	{
		return 0;
	}
	int ret;
	switch(val.units())
	{
	case css_units_percentage:
		ret = val.calc_percent(size);
		break;
	case css_units_em:
		ret = round_f(val.val() * (float) fontSize);
		break;
	case css_units_pt:
		ret = m_container->pt_to_px((int) val.val());
		break;
	case css_units_in:
		ret = m_container->pt_to_px((int) (val.val() * 72));
		break;
	case css_units_cm:
		ret = m_container->pt_to_px((int) (val.val() * 0.3937 * 72));
		break;
	case css_units_mm:
		ret = m_container->pt_to_px((int) (val.val() * 0.3937 * 72) / 10);
		break;
	case css_units_vw:
		ret = (int)((double)m_media.width * (double)val.val() / 100.0);
		break;
	case css_units_vh:
		ret = (int)((double)m_media.height * (double)val.val() / 100.0);
		break;
	case css_units_vmin:
		ret = (int)((double)std::min(m_media.height, m_media.width) * (double)val.val() / 100.0);
		break;
	case css_units_vmax:
		ret = (int)((double)std::max(m_media.height, m_media.width) * (double)val.val() / 100.0);
		break;
	case css_units_rem:
		ret = (int) ((double) m_root->css().get_font_size() * (double) val.val());
		break;
	default:
		ret = (int) val.val();
		break;
	}
	return ret;
}

void litehtml::document::cvt_units( css_length& val, int fontSize, int /*size*/ ) const
{
	if(val.is_predefined())
	{
		return;
	}
	int ret;
	switch(val.units())
	{
		case css_units_em:
			ret = round_f(val.val() * (float) fontSize);
			val.set_value((float) ret, css_units_px);
			break;
		case css_units_pt:
			ret = m_container->pt_to_px((int) val.val());
			val.set_value((float) ret, css_units_px);
			break;
		case css_units_in:
			ret = m_container->pt_to_px((int) (val.val() * 72));
			val.set_value((float) ret, css_units_px);
			break;
		case css_units_cm:
			ret = m_container->pt_to_px((int) (val.val() * 0.3937 * 72));
			val.set_value((float) ret, css_units_px);
			break;
		case css_units_mm:
			ret = m_container->pt_to_px((int) (val.val() * 0.3937 * 72) / 10);
			val.set_value((float) ret, css_units_px);
			break;
		default:
			break;
	}
}

int litehtml::document::width() const
{
	return m_size.width;
}

int litehtml::document::height() const
{
	return m_size.height;
}

int litehtml::document::content_width() const
{
	return m_content_size.width;
}

int litehtml::document::content_height() const
{
	return m_content_size.height;
}


void litehtml::document::add_stylesheet( const char* str, const char* baseurl, const char* media )
{
	if(str && str[0])
	{
		m_css.push_back(css_text(str, baseurl, media));
	}
}

bool litehtml::document::on_mouse_over( int x, int y, int client_x, int client_y, position::vector& redraw_boxes )
{
	if(!m_root || !m_root_render)
	{
		return false;
	}

	element::ptr over_el = m_root_render->get_element_by_point(x, y, client_x, client_y);

	bool state_was_changed = false;

	if(over_el != m_over_element)
	{
		if(m_over_element)
		{
			if(m_over_element->on_mouse_leave())
			{
				state_was_changed = true;
			}
		}
		m_over_element = over_el;
	}

	string cursor;

	if(m_over_element)
	{
		if(m_over_element->on_mouse_over())
		{
			state_was_changed = true;
		}
		cursor = m_over_element->css().get_cursor();
	}
	
	m_container->set_cursor(cursor.c_str());
	
	if(state_was_changed)
	{
		return m_root->find_styles_changes(redraw_boxes);
	}
	return false;
}

bool litehtml::document::on_mouse_leave( position::vector& redraw_boxes )
{
	if(!m_root || !m_root_render)
	{
		return false;
	}
	if(m_over_element)
	{
		if(m_over_element->on_mouse_leave())
		{
			return m_root->find_styles_changes(redraw_boxes);
		}
	}
	return false;
}

bool litehtml::document::on_lbutton_down( int x, int y, int client_x, int client_y, position::vector& redraw_boxes )
{
	if(!m_root || !m_root_render)
	{
		return false;
	}

	element::ptr over_el = m_root_render->get_element_by_point(x, y, client_x, client_y);

	bool state_was_changed = false;

	if(over_el != m_over_element)
	{
		if(m_over_element)
		{
			if(m_over_element->on_mouse_leave())
			{
				state_was_changed = true;
			}
		}
		m_over_element = over_el;
		if(m_over_element)
		{
			if(m_over_element->on_mouse_over())
			{
				state_was_changed = true;
			}
		}
	}

	string cursor;

	if(m_over_element)
	{
		if(m_over_element->on_lbutton_down())
		{
			state_was_changed = true;
		}
		cursor = m_over_element->css().get_cursor();
	}

	m_container->set_cursor(cursor.c_str());

	if(state_was_changed)
	{
		return m_root->find_styles_changes(redraw_boxes);
	}

	return false;
}

bool litehtml::document::on_lbutton_up( int /*x*/, int /*y*/, int /*client_x*/, int /*client_y*/, position::vector& redraw_boxes )
{
	if(!m_root || !m_root_render)
	{
		return false;
	}
	if(m_over_element)
	{
		if(m_over_element->on_lbutton_up())
		{
			return m_root->find_styles_changes(redraw_boxes);
		}
	}
	return false;
}

litehtml::element::ptr litehtml::document::create_element(const char* tag_name, const string_map& attributes)
{
	element::ptr newTag;
	document::ptr this_doc = shared_from_this();
	if(m_container)
	{
		newTag = m_container->create_element(tag_name, attributes, this_doc);
	}
	if(!newTag)
	{
		if(!strcmp(tag_name, "br"))
		{
			newTag = std::make_shared<litehtml::el_break>(this_doc);
		} else if(!strcmp(tag_name, "p"))
		{
			newTag = std::make_shared<litehtml::el_para>(this_doc);
		} else if(!strcmp(tag_name, "img"))
		{
			newTag = std::make_shared<litehtml::el_image>(this_doc);
		} else if(!strcmp(tag_name, "table"))
		{
			newTag = std::make_shared<litehtml::el_table>(this_doc);
		} else if(!strcmp(tag_name, "td") || !strcmp(tag_name, "th"))
		{
			newTag = std::make_shared<litehtml::el_td>(this_doc);
		} else if(!strcmp(tag_name, "link"))
		{
			newTag = std::make_shared<litehtml::el_link>(this_doc);
		} else if(!strcmp(tag_name, "title"))
		{
			newTag = std::make_shared<litehtml::el_title>(this_doc);
		} else if(!strcmp(tag_name, "a"))
		{
			newTag = std::make_shared<litehtml::el_anchor>(this_doc);
		} else if(!strcmp(tag_name, "tr"))
		{
			newTag = std::make_shared<litehtml::el_tr>(this_doc);
		} else if(!strcmp(tag_name, "style"))
		{
			newTag = std::make_shared<litehtml::el_style>(this_doc);
		} else if(!strcmp(tag_name, "base"))
		{
			newTag = std::make_shared<litehtml::el_base>(this_doc);
		} else if(!strcmp(tag_name, "body"))
		{
			newTag = std::make_shared<litehtml::el_body>(this_doc);
		} else if(!strcmp(tag_name, "div"))
		{
			newTag = std::make_shared<litehtml::el_div>(this_doc);
		} else if(!strcmp(tag_name, "script"))
		{
			newTag = std::make_shared<litehtml::el_script>(this_doc);
		} else if(!strcmp(tag_name, "font"))
		{
			newTag = std::make_shared<litehtml::el_font>(this_doc);
		} else
		{
			newTag = std::make_shared<litehtml::html_tag>(this_doc);
		}
	}

	if(newTag)
	{
		newTag->set_tagName(tag_name);
		for (const auto & attribute : attributes)
		{
			newTag->set_attr(attribute.first.c_str(), attribute.second.c_str());
		}
	}

	return newTag;
}

void litehtml::document::get_fixed_boxes( position::vector& fixed_boxes )
{
	fixed_boxes = m_fixed_boxes;
}

void litehtml::document::add_fixed_box( const position& pos )
{
	m_fixed_boxes.push_back(pos);
}

bool litehtml::document::media_changed()
{
	container()->get_media_features(m_media);
	if (update_media_lists(m_media))
	{
		m_root->refresh_styles();
		m_root->compute_styles();
		return true;
	}
	return false;
}

bool litehtml::document::lang_changed()
{
	if(!m_media_lists.empty())
	{
		string culture;
		container()->get_language(m_lang, culture);
		if(!culture.empty())
		{
			m_culture = m_lang + '-' + culture;
		}
		else
		{
			m_culture.clear();
		}
		m_root->refresh_styles();
		m_root->compute_styles();
		return true;
	}
	return false;
}

bool litehtml::document::update_media_lists(const media_features& features)
{
	bool update_styles = false;
	for(auto & m_media_list : m_media_lists)
	{
		if(m_media_list->apply_media_features(features))
		{
			update_styles = true;
		}
	}
	return update_styles;
}

void litehtml::document::add_media_list( const media_query_list::ptr& list )
{
	if(list)
	{
		if(std::find(m_media_lists.begin(), m_media_lists.end(), list) == m_media_lists.end())
		{
			m_media_lists.push_back(list);
		}
	}
}

void litehtml::document::create_node(void* gnode, elements_list& elements, bool parseTextNode)
{
	auto* node = (GumboNode*)gnode;
	switch (node->type)
	{
	case GUMBO_NODE_ELEMENT:
		{
			string_map attrs;
			GumboAttribute* attr;
			for (unsigned int i = 0; i < node->v.element.attributes.length; i++)
			{
				attr = (GumboAttribute*)node->v.element.attributes.data[i];
				attrs[attr->name] = attr->value;
			}


			element::ptr ret;
			const char* tag = gumbo_normalized_tagname(node->v.element.tag);
			if (tag[0])
			{
				ret = create_element(tag, attrs);
			}
			else
			{
				if (node->v.element.original_tag.data && node->v.element.original_tag.length)
				{
					std::string strA;
					gumbo_tag_from_original_text(&node->v.element.original_tag);
					strA.append(node->v.element.original_tag.data, node->v.element.original_tag.length);
					ret = create_element(strA.c_str(), attrs);
				}
			}
			if (!strcmp(tag, "script"))
			{
				parseTextNode = false;
			}
			if (ret)
			{
				elements_list child;
				for (unsigned int i = 0; i < node->v.element.children.length; i++)
				{
					child.clear();
					create_node(static_cast<GumboNode*> (node->v.element.children.data[i]), child, parseTextNode);
					std::for_each(child.begin(), child.end(), 
						[&ret](element::ptr& el)
						{
							ret->appendChild(el);
						}
					);
				}
				elements.push_back(ret);
			}
		}
		break;
	case GUMBO_NODE_TEXT:
		{
			if (!parseTextNode)
			{
				elements.push_back(std::make_shared<el_text>(node->v.text.text, shared_from_this()));
			}
			else
			{
				m_container->split_text(node->v.text.text,
					[this, &elements](const char* text) { elements.push_back(std::make_shared<el_text>(text, shared_from_this())); },
					[this, &elements](const char* text) { elements.push_back(std::make_shared<el_space>(text, shared_from_this())); });
			}
		}
		break;
	case GUMBO_NODE_CDATA:
		{
			element::ptr ret = std::make_shared<el_cdata>(shared_from_this());
			ret->set_data(node->v.text.text);
			elements.push_back(ret);
		}
		break;
	case GUMBO_NODE_COMMENT:
		{
			element::ptr ret = std::make_shared<el_comment>(shared_from_this());
			ret->set_data(node->v.text.text);
			elements.push_back(ret);
		}
		break;
	case GUMBO_NODE_WHITESPACE:
		{
			string str = node->v.text.text;
			for (size_t i = 0; i < str.length(); i++)
			{
				elements.push_back(std::make_shared<el_space>(str.substr(i, 1).c_str(), shared_from_this()));
			}
		}
		break;
	default:
		break;
	}
}

void litehtml::document::fix_tables_layout()
{
	for (const auto& el_ptr : m_tabular_elements)
	{
		switch (el_ptr->src_el()->css().get_display())
		{
		case display_inline_table:
		case display_table:
			fix_table_children(el_ptr, display_table_row_group, "table-row-group");
			break;
		case display_table_footer_group:
		case display_table_row_group:
		case display_table_header_group:
			{
				auto parent = el_ptr->parent();
				if (parent)
				{
					if (parent->src_el()->css().get_display() != display_inline_table)
						fix_table_parent(el_ptr, display_table, "table");
				}
				fix_table_children(el_ptr, display_table_row, "table-row");
			}
			break;
		case display_table_row:
			fix_table_parent(el_ptr, display_table_row_group, "table-row-group");
			fix_table_children(el_ptr, display_table_cell, "table-cell");
			break;
		case display_table_cell:
			fix_table_parent(el_ptr, display_table_row, "table-row");
			break;
		// TODO: make table layout fix for table-caption, table-column etc. elements
		case display_table_caption:
		case display_table_column:
		case display_table_column_group:
		default:
			break;
		}
	}
}

void litehtml::document::fix_table_children(const std::shared_ptr<render_item>& el_ptr, style_display disp, const char* disp_str)
{
	std::list<std::shared_ptr<render_item>> tmp;
	auto first_iter = el_ptr->children().begin();
	auto cur_iter = el_ptr->children().begin();

	auto flush_elements = [&]()
	{
		element::ptr annon_tag = std::make_shared<html_tag>(el_ptr->src_el(), string("display:") + disp_str);
		std::shared_ptr<render_item> annon_ri;
		if(annon_tag->css().get_display() == display_table_cell)
		{
			annon_tag->set_tagName("table_cell");
			annon_ri = std::make_shared<render_item_block>(annon_tag);
		} else if(annon_tag->css().get_display() == display_table_row)
		{
			annon_ri = std::make_shared<render_item_table_row>(annon_tag);
		} else
		{
			annon_ri = std::make_shared<render_item_table_part>(annon_tag);
		}
		for(const auto& el : tmp)
		{
			annon_ri->add_child(el);
		}
		// add annon item as tabular for future processing
		add_tabular(annon_ri);
		annon_ri->parent(el_ptr);
		first_iter = el_ptr->children().insert(first_iter, annon_ri);
		cur_iter = std::next(first_iter);
		while (cur_iter != el_ptr->children().end() && (*cur_iter)->parent() != el_ptr)
		{
			cur_iter = el_ptr->children().erase(cur_iter);
		}
		first_iter = cur_iter;
		tmp.clear();
	};

	while (cur_iter != el_ptr->children().end())
	{
		if ((*cur_iter)->src_el()->css().get_display() != disp)
		{
			if (!(*cur_iter)->src_el()->is_table_skip() || ((*cur_iter)->src_el()->is_table_skip() && !tmp.empty()))
			{
				if (disp != display_table_row_group || (*cur_iter)->src_el()->css().get_display() != display_table_caption)
				{
					if (tmp.empty())
					{
						first_iter = cur_iter;
					}
					tmp.push_back((*cur_iter));
				}
			}
			cur_iter++;
		}
		else if (!tmp.empty())
		{
			flush_elements();
		}
		else
		{
			cur_iter++;
		}
	}
	if (!tmp.empty())
	{
		flush_elements();
	}
}

void litehtml::document::fix_table_parent(const std::shared_ptr<render_item>& el_ptr, style_display disp, const char* disp_str)
{
	auto parent = el_ptr->parent();

	if (parent->src_el()->css().get_display() != disp)
	{
		auto this_element = std::find_if(parent->children().begin(), parent->children().end(),
			[&](const std::shared_ptr<render_item>& el)
			{
				if (el == el_ptr)
				{
					return true;
				}
				return false;
			}
		);
		if (this_element != parent->children().end())
		{
			style_display el_disp = el_ptr->src_el()->css().get_display();
			auto first = this_element;
			auto last = this_element;
			auto cur = this_element;

			// find first element with same display
			while (true)
			{
				if (cur == parent->children().begin()) break;
				cur--;
				if ((*cur)->src_el()->is_table_skip() || (*cur)->src_el()->css().get_display() == el_disp)
				{
					first = cur;
				}
				else
				{
					break;
				}
			}

			// find last element with same display
			cur = this_element;
			while (true)
			{
				cur++;
				if (cur == parent->children().end()) break;

				if ((*cur)->src_el()->is_table_skip() || (*cur)->src_el()->css().get_display() == el_disp)
				{
					last = cur;
				}
				else
				{
					break;
				}
			}

			// extract elements with the same display and wrap them with anonymous object
			element::ptr annon_tag = std::make_shared<html_tag>(parent->src_el(), string("display:") + disp_str);
			std::shared_ptr<render_item> annon_ri;
			if(annon_tag->css().get_display() == display_table || annon_tag->css().get_display() == display_inline_table)
			{
				annon_ri = std::make_shared<render_item_table>(annon_tag);
			} else if(annon_tag->css().get_display() == display_table_row)
			{
				annon_ri = std::make_shared<render_item_table_row>(annon_tag);
			} else
			{
				annon_ri = std::make_shared<render_item_table_part>(annon_tag);
			}
			std::for_each(first, std::next(last, 1),
				[&annon_ri](std::shared_ptr<render_item>& el)
				{
					annon_ri->add_child(el);
				}
			);
			first = parent->children().erase(first, std::next(last));
			parent->children().insert(first, annon_ri);
			add_tabular(annon_ri);
			annon_ri->parent(parent);
		}
	}
}

void litehtml::document::append_children_from_string(element& parent, const char* str)
{
	// parent must belong to this document
	if (parent.get_document().get() != this)
	{
		return;
	}

	// parse document into GumboOutput
	GumboOutput* output = gumbo_parse(str);

	// Create litehtml::elements.
	elements_list child_elements;
	create_node(output->root, child_elements, true);

	// Destroy GumboOutput
	gumbo_destroy_output(&kGumboDefaultOptions, output);

	// Let's process created elements tree
	for (const auto& child : child_elements)
	{
		// Add the child element to parent
		parent.appendChild(child);

		// apply master CSS
		child->apply_stylesheet(m_master_css);

		// parse elements attributes
		child->parse_attributes();

		// Apply parsed styles.
		child->apply_stylesheet(m_styles);

		// Apply user styles if any
		child->apply_stylesheet(m_user_css);

		// Initialize m_css
		child->compute_styles();

		// Now the m_tabular_elements is filled with tabular elements.
		// We have to check the tabular elements for missing table elements 
		// and create the anonymous boxes in visual table layout
		fix_tables_layout();

		// Finally initialize elements
		//child->init();
	}
}

void litehtml::document::dump(dumper& cout)
{
	if(m_root_render)
	{
		m_root_render->dump(cout);
	}
}
