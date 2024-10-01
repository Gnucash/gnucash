#include "html.h"
#include "html_tag.h"
#include "document.h"
#include "iterators.h"
#include "stylesheet.h"
#include "table.h"
#include <algorithm>
#include <locale>
#include "el_before_after.h"
#include "num_cvt.h"
#include "line_box.h"
#include <stack>
#include "render_item.h"

litehtml::html_tag::html_tag(const std::shared_ptr<document>& doc) : element(doc)
{
	m_tag = empty_id;
	m_id = empty_id;
}

litehtml::html_tag::html_tag(const element::ptr& parent, const string& style) : element(parent->get_document()),
	m_tag(empty_id),
	m_id(empty_id)
{
	litehtml::style st;
	st.add(style);
	add_style(st);
	this->parent(parent);
	compute_styles();
}

bool litehtml::html_tag::appendChild(const element::ptr &el)
{
	if(el)
	{
		el->parent(shared_from_this());
		m_children.push_back(el);
		return true;
	}
	return false;
}

bool litehtml::html_tag::removeChild(const element::ptr &el)
{
	if(el && el->parent() == shared_from_this())
	{
		el->parent(nullptr);
		m_children.erase(std::remove(m_children.begin(), m_children.end(), el), m_children.end());
		return true;
	}
	return false;
}

void litehtml::html_tag::clearRecursive()
{
	for(auto& el : m_children)
	{
		el->clearRecursive();
		el->parent(nullptr);
	}
	m_children.clear();
}

litehtml::string_id litehtml::html_tag::id() const
{
	return m_id;
}

litehtml::string_id litehtml::html_tag::tag() const
{
	return m_tag;
}

const char* litehtml::html_tag::get_tagName() const
{
	return _s(m_tag).c_str();
}

void litehtml::html_tag::set_tagName( const char* _tag )
{
	string tag = _tag;
	lcase(tag);
	m_tag = _id(tag);
}

void litehtml::html_tag::set_attr( const char* _name, const char* _val )
{
	if(_name && _val)
	{
		string name = _name;
		lcase(name);
		m_attrs[name] = _val;

		if( name == "class" )
		{
			string val = _val;
			// class names are matched case-insensitively in quirks mode
			// we match them case-insensitively in all modes (same for id)
			lcase(val);
			m_str_classes.resize( 0 );
			split_string( val, m_str_classes, " " );
			m_classes.clear();
			for (auto& cls : m_str_classes) m_classes.push_back(_id(cls));
		}
		else if (name == "id")
		{
			string val = _val;
			lcase(val);
			m_id = _id(val);
		}
	}
}

const char* litehtml::html_tag::get_attr( const char* name, const char* def ) const
{
	auto attr = m_attrs.find(name);
	if(attr != m_attrs.end())
	{
		return attr->second.c_str();
	}
	return def;
}

litehtml::elements_list litehtml::html_tag::select_all(const string& selector )
{
	css_selector sel;
	sel.parse(selector);
	
	return select_all(sel);
}

litehtml::elements_list litehtml::html_tag::select_all(const css_selector& selector )
{
	litehtml::elements_list res;
	select_all(selector, res);
	return res;
}

void litehtml::html_tag::select_all(const css_selector& selector, elements_list& res)
{
	if(select(selector))
	{
		res.push_back(shared_from_this());
	}
	
	for(auto& el : m_children)
	{
		el->select_all(selector, res);
	}
}


litehtml::element::ptr litehtml::html_tag::select_one( const string& selector )
{
	css_selector sel;
	sel.parse(selector);

	return select_one(sel);
}

litehtml::element::ptr litehtml::html_tag::select_one( const css_selector& selector )
{
	if(select(selector))
	{
		return shared_from_this();
	}

	for(auto& el : m_children)
	{
		element::ptr res = el->select_one(selector);
		if(res)
		{
			return res;
		}
	}
	return nullptr;
}

void litehtml::html_tag::apply_stylesheet( const litehtml::css& stylesheet )
{
	if(is_root())
	{
		int i = 0;
		i++;
	}
	for(const auto& sel : stylesheet.selectors())
	{
		// optimization
		{
			const auto& r = sel->m_right;
			if (r.m_tag != star_id && r.m_tag != m_tag)
				continue;

			if (!r.m_attrs.empty())
			{
				const auto& attr = r.m_attrs[0];
				if (attr.type == select_class &&
					std::find(m_classes.begin(), m_classes.end(), attr.name) == m_classes.end())
					continue;
			}
		}

		int apply = select(*sel, false);

		if(apply != select_no_match)
		{
			used_selector::ptr us = std::unique_ptr<used_selector>(new used_selector(sel, false));

			if(sel->is_media_valid())
			{
				auto apply_before_after = [&]()
					{
						const auto& content_property = sel->m_style->get_property(_content_);
						bool content_none = content_property.m_type == prop_type_string && content_property.m_string == "none";
						bool create = !content_none && (sel->m_right.m_attrs.size() > 1 || sel->m_right.m_tag != star_id);

						element::ptr el;
						if(apply & select_match_with_after)
						{
							el = get_element_after(*sel->m_style, create);
						} else if(apply & select_match_with_before)
						{
							el = get_element_before(*sel->m_style, create);
						} else
						{
							return;
						}
						if(el)
						{
							if(!content_none)
							{
								el->add_style(*sel->m_style);
							} else
							{
								el->parent()->removeChild(el);
							}
						} else
						{
							if(!content_none)
							{
								add_style(*sel->m_style);
							}
						}
						us->m_used = true;
					};


				if(apply & select_match_pseudo_class)
				{
					if(select(*sel, true))
					{
						if((apply & (select_match_with_after | select_match_with_before)))
						{
							apply_before_after();
						} else
						{
							add_style(*sel->m_style);
							us->m_used = true;
						}
					}
				} else if((apply & (select_match_with_after | select_match_with_before)))
				{
					apply_before_after();
				} else
				{
					add_style(*sel->m_style);
					us->m_used = true;
				}
			}
			m_used_styles.push_back(std::move(us));
		}
	}

	for(auto& el : m_children)
	{
		if(el->css().get_display() != display_inline_text)
		{
			el->apply_stylesheet(stylesheet);
		}
	}
}

void litehtml::html_tag::get_content_size( size& sz, int max_width )
{
	sz.height	= 0;
	if(m_css.get_display() == display_block)
	{
		sz.width	= max_width;
	} else
	{
		sz.width	= 0;
	}
}

void litehtml::html_tag::draw(uint_ptr hdc, int x, int y, const position *clip, const std::shared_ptr<render_item> &ri)
{
	position pos = ri->pos();
	pos.x	+= x;
	pos.y	+= y;

	draw_background(hdc, x, y, clip, ri);

	if(m_css.get_display() == display_list_item && m_css.get_list_style_type() != list_style_type_none)
	{
		if(m_css.get_overflow() > overflow_visible)
		{
			position border_box = pos;
			border_box += ri->get_paddings();
			border_box += ri->get_borders();

			border_radiuses bdr_radius = m_css.get_borders().radius.calc_percents(border_box.width, border_box.height);

			bdr_radius -= ri->get_borders();
			bdr_radius -= ri->get_paddings();

			get_document()->container()->set_clip(pos, bdr_radius);
		}

		draw_list_marker(hdc, pos);

		if(m_css.get_overflow() > overflow_visible)
		{
			get_document()->container()->del_clip();
		}
	}
}

litehtml::string litehtml::html_tag::get_custom_property(string_id name, const string& default_value) const
{
	const property_value& value = m_style.get_property(name);

	if (value.m_type == prop_type_string)
	{
		return value.m_string;
	}
	else if (auto _parent = parent())
	{
		return _parent->get_custom_property(name, default_value);
	}
	return default_value;
}

template<class Type, litehtml::property_type property_value_type, Type litehtml::property_value::* property_value_member>
const Type& litehtml::html_tag::get_property_impl(string_id name, bool inherited, const Type& default_value, uint_ptr css_properties_member_offset) const
{
	const property_value& value = m_style.get_property(name);

	if (value.m_type == property_value_type)
	{
		return value.*property_value_member;
	}
	else if (inherited || value.m_type == prop_type_inherit)
	{
		if (auto _parent = parent())
		{
			return *(Type*)((byte*)&_parent->css() + css_properties_member_offset);
		}
		return default_value;
	}
	// value must be invalid here
	//assert(value.m_type == prop_type_invalid);
	return default_value;
}

int litehtml::html_tag::get_enum_property(string_id name, bool inherited, int default_value, uint_ptr css_properties_member_offset) const
{
	return get_property_impl<int, prop_type_enum_item, &property_value::m_enum_item>(name, inherited, default_value, css_properties_member_offset);
}

int litehtml::html_tag::get_int_property(string_id name, bool inherited, int default_value, uint_ptr css_properties_member_offset) const
{
	return get_property_impl<int, prop_type_enum_item, &property_value::m_enum_item>(name, inherited, default_value, css_properties_member_offset);
}

litehtml::css_length litehtml::html_tag::get_length_property(string_id name, bool inherited, css_length default_value, uint_ptr css_properties_member_offset) const
{
	return get_property_impl<css_length, prop_type_length, &property_value::m_length>(name, inherited, default_value, css_properties_member_offset);
}

litehtml::web_color litehtml::html_tag::get_color_property(string_id name, bool inherited, web_color default_value, uint_ptr css_properties_member_offset) const
{
	return get_property_impl<web_color, prop_type_color, &property_value::m_color>(name, inherited, default_value, css_properties_member_offset);
}

litehtml::string litehtml::html_tag::get_string_property(string_id name, bool inherited, const string& default_value, uint_ptr css_properties_member_offset) const
{
	return get_property_impl<string, prop_type_string, &property_value::m_string>(name, inherited, default_value, css_properties_member_offset);
}

float litehtml::html_tag::get_number_property(string_id name, bool inherited, float default_value, uint_ptr css_properties_member_offset) const
{
	return get_property_impl<float, prop_type_number, &property_value::m_number>(name, inherited, default_value, css_properties_member_offset);
}

litehtml::string_vector litehtml::html_tag::get_string_vector_property(string_id name, bool inherited, const string_vector& default_value, uint_ptr css_properties_member_offset) const
{
	return get_property_impl<string_vector, prop_type_string_vector, &property_value::m_string_vector>(name, inherited, default_value, css_properties_member_offset);
}

litehtml::int_vector litehtml::html_tag::get_int_vector_property(string_id name, bool inherited, const int_vector& default_value, uint_ptr css_properties_member_offset) const
{
	return get_property_impl<int_vector, prop_type_enum_item_vector, &property_value::m_enum_item_vector>(name, inherited, default_value, css_properties_member_offset);
}

litehtml::length_vector litehtml::html_tag::get_length_vector_property(string_id name, bool inherited, const length_vector& default_value, uint_ptr css_properties_member_offset) const
{
	return get_property_impl<length_vector, prop_type_length_vector, &property_value::m_length_vector>(name, inherited, default_value, css_properties_member_offset);
}

litehtml::size_vector litehtml::html_tag::get_size_vector_property(string_id name, bool inherited, const size_vector& default_value, uint_ptr css_properties_member_offset) const
{
	return get_property_impl<size_vector, prop_type_size_vector, &property_value::m_size_vector>(name, inherited, default_value, css_properties_member_offset);
}

void litehtml::html_tag::compute_styles(bool recursive)
{
	const char* style = get_attr("style");
	document::ptr doc = get_document();

	if (style)
	{
		m_style.add(style, "", doc->container());
	}

	m_style.subst_vars(this);

	m_css.compute(this, doc);

	if (recursive)
	{
		for (const auto& el : m_children)
		{
			el->compute_styles();
		}
	}
}

bool litehtml::html_tag::is_white_space() const
{
	return false;
}

int litehtml::html_tag::select(const string& selector)
{
	css_selector sel;
	sel.parse(selector);
	return select(sel, true);
}

int litehtml::html_tag::select(const css_selector& selector, bool apply_pseudo)
{
	int right_res = select(selector.m_right, apply_pseudo);
	if(right_res == select_no_match)
	{
		return select_no_match;
	}
	element::ptr el_parent = parent();
	if(selector.m_left)
	{
		if (!el_parent)
		{
			return select_no_match;
		}
		switch(selector.m_combinator)
		{
		case combinator_descendant:
			{
				bool is_pseudo = false;
				element::ptr res = find_ancestor(*selector.m_left, apply_pseudo, &is_pseudo);
				if(!res)
				{
					return select_no_match;
				} else
				{
					if(is_pseudo)
					{
						right_res |= select_match_pseudo_class;
					}
				}
			}
			break;
		case combinator_child:
			{
				int res = el_parent->select(*selector.m_left, apply_pseudo);
				if(res == select_no_match)
				{
					return select_no_match;
				} else
				{
					if(right_res != select_match_pseudo_class)
					{
						right_res |= res;
					}
				}
			}
			break;
		case combinator_adjacent_sibling:
			{
				bool is_pseudo = false;
				element::ptr res = el_parent->find_adjacent_sibling(shared_from_this(), *selector.m_left, apply_pseudo, &is_pseudo);
				if(!res)
				{
					return select_no_match;
				} else
				{
					if(is_pseudo)
					{
						right_res |= select_match_pseudo_class;
					}
				}
			}
			break;
		case combinator_general_sibling:
			{
				bool is_pseudo = false;
				element::ptr res =  el_parent->find_sibling(shared_from_this(), *selector.m_left, apply_pseudo, &is_pseudo);
				if(!res)
				{
					return select_no_match;
				} else
				{
					if(is_pseudo)
					{
						right_res |= select_match_pseudo_class;
					}
				}
			}
			break;
		default:
			right_res = select_no_match;
		}
	}
	return right_res;
}

int litehtml::html_tag::select(const css_element_selector& selector, bool apply_pseudo)
{
	if(selector.m_tag != star_id && selector.m_tag != m_tag)
	{
		return select_no_match;
	}

	int res = select_match;

	for(const auto& attr : selector.m_attrs)
	{
		switch(attr.type)
		{
		case select_class:
			if (std::find(m_classes.begin(), m_classes.end(), attr.name) == m_classes.end())
			{
				return select_no_match;
			}
			break;
		case select_id:
			if (attr.name != m_id)
			{
				return select_no_match;
			}
			break;
		case select_pseudo_element:
			if(attr.name == _after_)
			{
				if(selector.m_attrs.size() == 1 && selector.m_tag == star_id && m_tag != __tag_after_)
				{
					return select_no_match;
				}
				res |= select_match_with_after;
			} else if(attr.name == _before_)
			{
				if(selector.m_attrs.size() == 1 && selector.m_tag == star_id && m_tag != __tag_before_)
				{
					return select_no_match;
				}
				res |= select_match_with_before;
			} else
			{
				return select_no_match;
			}
			break;
		case select_pseudo_class:
			if(apply_pseudo)
			{
				if (select_pseudoclass(attr) == select_no_match)
				{
					return select_no_match;
				}
			} else
			{
				res |= select_match_pseudo_class;
			}
			break;
		default:
			if (select_attribute(attr) == select_no_match)
			{
				return select_no_match;
			}
		}
	}
	return res;
}

int litehtml::html_tag::select_pseudoclass(const css_attribute_selector& sel)
{
	element::ptr el_parent = parent();

	switch (sel.name)
	{
	case _only_child_:
		if (!el_parent || !el_parent->is_only_child(shared_from_this(), false))
		{
			return select_no_match;
		}
		break;
	case _only_of_type_:
		if (!el_parent || !el_parent->is_only_child(shared_from_this(), true))
		{
			return select_no_match;
		}
		break;
	case _first_child_:
		if (!el_parent || !el_parent->is_nth_child(shared_from_this(), 0, 1, false))
		{
			return select_no_match;
		}
		break;
	case _first_of_type_:
		if (!el_parent || !el_parent->is_nth_child(shared_from_this(), 0, 1, true))
		{
			return select_no_match;
		}
		break;
	case _last_child_:
		if (!el_parent || !el_parent->is_nth_last_child(shared_from_this(), 0, 1, false))
		{
			return select_no_match;
		}
		break;
	case _last_of_type_:
		if (!el_parent || !el_parent->is_nth_last_child(shared_from_this(), 0, 1, true))
		{
			return select_no_match;
		}
		break;
	case _nth_child_:
	case _nth_of_type_:
	case _nth_last_child_:
	case _nth_last_of_type_:
	{
		if (!el_parent) return select_no_match;

		int num = sel.a;
		int off = sel.b;
		if (!num && !off) return select_no_match;

		switch (sel.name)
		{
		case _nth_child_:
			if (!el_parent->is_nth_child(shared_from_this(), num, off, false))
			{
				return select_no_match;
			}
			break;
		case _nth_of_type_:
			if (!el_parent->is_nth_child(shared_from_this(), num, off, true))
			{
				return select_no_match;
			}
			break;
		case _nth_last_child_:
			if (!el_parent->is_nth_last_child(shared_from_this(), num, off, false))
			{
				return select_no_match;
			}
			break;
		case _nth_last_of_type_:
			if (!el_parent->is_nth_last_child(shared_from_this(), num, off, true))
			{
				return select_no_match;
			}
			break;
		default:
			break;
		}

	}
	break;
	case _not_:
		if (select(*sel.sel, true))
		{
			return select_no_match;
		}
		break;
	case _lang_:
		if (!get_document()->match_lang(sel.val))
		{
			return select_no_match;
		}
		break;
	default:
		if (std::find(m_pseudo_classes.begin(), m_pseudo_classes.end(), sel.name) == m_pseudo_classes.end())
		{
			return select_no_match;
		}
		break;
	}
	return select_match;
}

int litehtml::html_tag::select_attribute(const css_attribute_selector& sel)
{
	const char* attr_value = get_attr(_s(sel.name).c_str());

	switch (sel.type)
	{
	case select_exists:
		if (!attr_value)
		{
			return select_no_match;
		}
		break;
	case select_equal:
		if (!attr_value || strcmp(attr_value, sel.val.c_str()))
		{
			return select_no_match;
		}
		break;
	case select_contain_str:
		if (!attr_value || !strstr(attr_value, sel.val.c_str()))
		{
			return select_no_match;
		}
		break;
	case select_start_str:
		if (!attr_value || strncmp(attr_value, sel.val.c_str(), sel.val.length()))
		{
			return select_no_match;
		}
		break;
	case select_end_str:
		if (!attr_value)
		{
			return select_no_match;
		}
		else if (strncmp(attr_value, sel.val.c_str(), sel.val.length()))
		{
			const char* s = attr_value + strlen(attr_value) - sel.val.length() - 1;
			if (s < attr_value)
			{
				return select_no_match;
			}
			if (sel.val != s)
			{
				return select_no_match;
			}
		}
		break;
	default:
		break;
	}
	return select_match;
}

litehtml::element::ptr litehtml::html_tag::find_ancestor(const css_selector& selector, bool apply_pseudo, bool* is_pseudo)
{
	element::ptr el_parent = parent();
	if (!el_parent)
	{
		return nullptr;
	}
	int res = el_parent->select(selector, apply_pseudo);
	if(res != select_no_match)
	{
		if(is_pseudo)
		{
			if(res & select_match_pseudo_class)
			{
				*is_pseudo = true;
			} else
			{
				*is_pseudo = false;
			}
		}
		return el_parent;
	}
	return el_parent->find_ancestor(selector, apply_pseudo, is_pseudo);
}

void litehtml::html_tag::parse_attributes()
{
	for(auto& el : m_children)
	{
		el->parse_attributes();
	}
}

void litehtml::html_tag::get_text( string& text )
{
	for (auto& el : m_children)
	{
		el->get_text(text);
	}
}

bool litehtml::html_tag::is_body()  const
{
	return false;
}

void litehtml::html_tag::set_data( const char* /*data*/ )
{

}

bool litehtml::html_tag::on_mouse_over()
{
	bool ret = false;

	element::ptr el = shared_from_this();
	while(el)
	{
		if(el->set_pseudo_class(_hover_, true))
		{
			ret = true;
		}
		el = el->parent();
	}

	return ret;
}

bool litehtml::html_tag::on_mouse_leave()
{
	bool ret = false;

	element::ptr el = shared_from_this();
	while(el)
	{
		if(el->set_pseudo_class(_hover_, false))
		{
			ret = true;
		}
		if(el->set_pseudo_class(_active_, false))
		{
			ret = true;
		}
		el = el->parent();
	}

	return ret;
}

bool litehtml::html_tag::on_lbutton_down()
{
	bool ret = false;

	element::ptr el = shared_from_this();
	while (el)
	{
		if (el->set_pseudo_class(_active_, true))
		{
			ret = true;
		}
		el = el->parent();
	}

	return ret;
}

bool litehtml::html_tag::on_lbutton_up()
{
	bool ret = false;

	element::ptr el = shared_from_this();
	while (el)
	{
		if (el->set_pseudo_class(_active_, false))
		{
			ret = true;
		}
		el = el->parent();
	}

	on_click();

	return ret;
}

void litehtml::html_tag::on_click()
{
	if (!is_root())
	{
		element::ptr el_parent = parent();
		if (el_parent)
		{
			el_parent->on_click();
		}
	}
}

bool litehtml::html_tag::is_break() const
{
	return false;
}

void litehtml::html_tag::draw_background(uint_ptr hdc, int x, int y, const position *clip,
										 const std::shared_ptr<render_item> &ri)
{
	position pos = ri->pos();
	pos.x	+= x;
	pos.y	+= y;

	position el_pos = pos;
	el_pos += ri->get_paddings();
	el_pos += ri->get_margins();

	if(m_css.get_display() != display_inline && m_css.get_display() != display_table_row)
	{
		if(el_pos.does_intersect(clip) || is_root())
		{
			auto v_offset = ri->get_draw_vertical_offset();
			pos.y += v_offset;
			pos.height -= v_offset;

			const background* bg = get_background();
			if(bg)
			{
				std::vector<background_paint> bg_paint;
				init_background_paint(pos, bg_paint, bg, ri);
				if(is_root())
				{
					for(auto& b : bg_paint)
					{
						b.clip_box = *clip;
						b.border_box = *clip;
					}
				}

				get_document()->container()->draw_background(hdc, bg_paint);
			}
			position border_box = pos;
			border_box += ri->get_paddings();
			border_box += ri->get_borders();

			borders bdr = m_css.get_borders();
			if(bdr.is_visible())
			{
				bdr.radius = m_css.get_borders().radius.calc_percents(border_box.width, border_box.height);
				get_document()->container()->draw_borders(hdc, bdr, border_box, is_root());
			}
		}
	} else
	{
		const background* bg = get_background();

		position::vector boxes;
		ri->get_inline_boxes(boxes);

		std::vector<background_paint> bg_paint;
		position content_box;

		for(auto box = boxes.begin(); box != boxes.end(); box++)
		{
			box->x	+= x;
			box->y	+= y;

			if(box->does_intersect(clip))
			{
				content_box = *box;
				content_box -= ri->get_borders();
				content_box -= ri->get_paddings();

				if(bg)
				{
					init_background_paint(content_box, bg_paint, bg, ri);
				}

				css_borders bdr;

				// set left borders radius for the first box
				if(box == boxes.begin())
				{
					bdr.radius.bottom_left_x	= m_css.get_borders().radius.bottom_left_x;
					bdr.radius.bottom_left_y	= m_css.get_borders().radius.bottom_left_y;
					bdr.radius.top_left_x		= m_css.get_borders().radius.top_left_x;
					bdr.radius.top_left_y		= m_css.get_borders().radius.top_left_y;
				}

				// set right borders radius for the last box
				if(box == boxes.end() - 1)
				{
					bdr.radius.bottom_right_x	= m_css.get_borders().radius.bottom_right_x;
					bdr.radius.bottom_right_y	= m_css.get_borders().radius.bottom_right_y;
					bdr.radius.top_right_x		= m_css.get_borders().radius.top_right_x;
					bdr.radius.top_right_y		= m_css.get_borders().radius.top_right_y;
				}

				
				bdr.top		= m_css.get_borders().top;
				bdr.bottom	= m_css.get_borders().bottom;
				if(box == boxes.begin())
				{
					bdr.left	= m_css.get_borders().left;
				}
				if(box == boxes.end() - 1)
				{
					bdr.right	= m_css.get_borders().right;
				}

				if(bg)
				{
					for (auto& bgp : bg_paint)
					{
						bgp.border_radius = bdr.radius.calc_percents(bgp.border_box.width, bgp.border_box.width);
					}
					get_document()->container()->draw_background(hdc, bg_paint);
				}
				if(bdr.is_visible())
				{
					borders b = bdr;
					b.radius = bdr.radius.calc_percents(box->width, box->height);
					get_document()->container()->draw_borders(hdc, b, *box, false);
				}
			}
		}
	}
}

bool litehtml::html_tag::set_pseudo_class( string_id cls, bool add )
{
	bool ret = false;
	if(add)
	{
		if(std::find(m_pseudo_classes.begin(), m_pseudo_classes.end(), cls) == m_pseudo_classes.end())
		{
			m_pseudo_classes.push_back(cls);
			ret = true;
		}
	} else
	{
		auto pi = std::find(m_pseudo_classes.begin(), m_pseudo_classes.end(), cls);
		if(pi != m_pseudo_classes.end())
		{
			m_pseudo_classes.erase(pi);
			ret = true;
		}
	}
	return ret;
}

bool litehtml::html_tag::set_class( const char* pclass, bool add )
{
	string_vector classes;
	bool changed = false;

	split_string( pclass, classes, " " );

	if(add)
	{
		for( auto & _class : classes )
		{
			if(std::find(m_str_classes.begin(), m_str_classes.end(), _class) == m_str_classes.end())
			{
				m_str_classes.push_back( std::move( _class ) );
				changed = true;
			}
		}
	} else
	{
		for( const auto & _class : classes )
		{
			auto end = std::remove(m_str_classes.begin(), m_str_classes.end(), _class);

			if(end != m_str_classes.end())
			{
				m_str_classes.erase(end, m_str_classes.end());
				changed = true;
			}
		}
	}

	if( changed )
	{
		string class_string;
		join_string(class_string, m_str_classes, " ");
		set_attr("class", class_string.c_str());

		return true;
	}
	else
	{
		return false;
	}

}

bool litehtml::html_tag::is_replaced() const
{
	return false;
}

void litehtml::html_tag::init_background_paint(position pos, std::vector<background_paint>& bg_paint, const background* bg, const std::shared_ptr<render_item>& ri)
{
	bg_paint = { background_paint() };
	if (!bg) return;

	int bg_count = std::max((int)bg->m_image.size(), 1);
	bg_paint.resize(bg_count);

	for (int i = 0; i < bg_count; i++)
	{
		init_one_background_paint(i, pos, bg_paint[i], bg, ri);
	}

	bg_paint.back().color = bg->m_color;
}

void litehtml::html_tag::init_one_background_paint(int i, position pos, background_paint& bg_paint, const background* bg, const std::shared_ptr<render_item>& ri)
{
	bg_paint.image		= i < (int) bg->m_image.size() ? bg->m_image[i] : "";
	bg_paint.baseurl	= bg->m_baseurl;
	bg_paint.attachment = i < (int) bg->m_attachment.size() ? (background_attachment)bg->m_attachment[i] : background_attachment_scroll;
	bg_paint.repeat		= i < (int) bg->m_repeat.size() ? (background_repeat)bg->m_repeat[i] : background_repeat_repeat;
	int clip			= i < (int) bg->m_clip.size() ? bg->m_clip[i] : background_box_border;
	int origin			= i < (int) bg->m_origin.size() ? bg->m_origin[i] : background_box_padding;
	const css_size auto_auto(css_length::predef_value(background_size_auto), css_length::predef_value(background_size_auto));
	css_size size		= i < (int) bg->m_size.size() ? bg->m_size[i] : auto_auto;
	css_length position_x = i < (int) bg->m_position_x.size() ? bg->m_position_x[i] : css_length(0, css_units_percentage);
	css_length position_y = i < (int) bg->m_position_y.size() ? bg->m_position_y[i] : css_length(0, css_units_percentage);

	position content_box	= pos;
	position padding_box	= pos;
	padding_box += ri->get_paddings();
	position border_box		= padding_box;
	border_box += ri->get_borders();

	switch(clip)
	{
	case background_box_padding:
		bg_paint.clip_box = padding_box;
		break;
	case background_box_content:
		bg_paint.clip_box = content_box;
		break;
	default:
		bg_paint.clip_box = border_box;
		break;
	}

	switch(origin)
	{
	case background_box_border:
		bg_paint.origin_box = border_box;
		break;
	case background_box_content:
		bg_paint.origin_box = content_box;
		break;
	default:
		bg_paint.origin_box = padding_box;
		break;
	}

	if(!bg_paint.image.empty())
	{
		get_document()->container()->get_image_size(bg_paint.image.c_str(), bg_paint.baseurl.c_str(), bg_paint.image_size);
		if(bg_paint.image_size.width && bg_paint.image_size.height)
		{
			litehtml::size img_new_sz = bg_paint.image_size;
			double img_ar_width		= (double) bg_paint.image_size.width / (double) bg_paint.image_size.height;
			double img_ar_height	= (double) bg_paint.image_size.height / (double) bg_paint.image_size.width;


			if(size.width.is_predefined())
			{
				switch(size.width.predef())
				{
				case background_size_contain:
					if( (int) ((double) bg_paint.origin_box.width * img_ar_height) <= bg_paint.origin_box.height )
					{
						img_new_sz.width = bg_paint.origin_box.width;
						img_new_sz.height	= (int) ((double) bg_paint.origin_box.width * img_ar_height);
					} else
					{
						img_new_sz.height = bg_paint.origin_box.height;
						img_new_sz.width	= (int) ((double) bg_paint.origin_box.height * img_ar_width);
					}
					break;
				case background_size_cover:
					if( (int) ((double) bg_paint.origin_box.width * img_ar_height) >= bg_paint.origin_box.height )
					{
						img_new_sz.width = bg_paint.origin_box.width;
						img_new_sz.height	= (int) ((double) bg_paint.origin_box.width * img_ar_height);
					} else
					{
						img_new_sz.height = bg_paint.origin_box.height;
						img_new_sz.width	= (int) ((double) bg_paint.origin_box.height * img_ar_width);
					}
					break;
				case background_size_auto:
					if(!size.height.is_predefined())
					{
						img_new_sz.height	= size.height.calc_percent(bg_paint.origin_box.height);
						img_new_sz.width	= (int) ((double) img_new_sz.height * img_ar_width);
					}
					break;
				}
			} else
			{
				img_new_sz.width = size.width.calc_percent(bg_paint.origin_box.width);
				if(size.height.is_predefined())
				{
					img_new_sz.height = (int) ((double) img_new_sz.width * img_ar_height);
				} else
				{
					img_new_sz.height = size.height.calc_percent(bg_paint.origin_box.height);
				}
			}

			bg_paint.image_size = img_new_sz;
			bg_paint.position_x = bg_paint.origin_box.x + (int) position_x.calc_percent(bg_paint.origin_box.width - bg_paint.image_size.width);
			bg_paint.position_y = bg_paint.origin_box.y + (int) position_y.calc_percent(bg_paint.origin_box.height - bg_paint.image_size.height);
		}
	}
	bg_paint.border_radius	= m_css.get_borders().radius.calc_percents(border_box.width, border_box.height);
	bg_paint.border_box		= border_box;
	bg_paint.is_root		= is_root();
}

void litehtml::html_tag::draw_list_marker( uint_ptr hdc, const position& pos )
{
	list_marker lm;

	size img_size;
	if (css().get_list_style_image() != "")
	{
		lm.image   = css().get_list_style_image();
		lm.baseurl = css().get_list_style_image_baseurl().c_str();
		get_document()->container()->get_image_size(lm.image.c_str(), lm.baseurl, img_size);
	} else
	{
		lm.baseurl = nullptr;
	}

	int ln_height	= css().get_line_height();
	int sz_font		= css().get_font_size();
	lm.pos.x		= pos.x;
	lm.pos.width    = sz_font - sz_font * 2 / 3;
	lm.color        = css().get_color();
	lm.marker_type  = css().get_list_style_type();
	lm.font         = css().get_font();

	if (css().get_list_style_type() >= list_style_type_armenian)
	{
		lm.pos.y = pos.y;
		lm.pos.height = pos.height;
		lm.index = atoi(get_attr("list_index", "0"));
	}
	else
	{
		lm.pos.height = sz_font - sz_font * 2 / 3;
		lm.pos.y = pos.y + ln_height / 2 - lm.pos.height / 2;
		lm.index = -1;
	}

	if(img_size.width && img_size.height)
	{
		if(lm.pos.y + img_size.height > pos.y + pos.height)
		{
			lm.pos.y = pos.y + pos.height - img_size.height;
		}
		if(img_size.width > lm.pos.width)
		{
			lm.pos.x -= img_size.width - lm.pos.width;
		}

		lm.pos.width	= img_size.width;
		lm.pos.height	= img_size.height;
	}

	if (m_css.get_list_style_position() == list_style_position_outside)
	{
		if (m_css.get_list_style_type() >= list_style_type_armenian)
		{
			if(lm.font)
			{
				auto tw_space = get_document()->container()->text_width(" ", lm.font);
				lm.pos.x = pos.x - tw_space * 2;
				lm.pos.width = tw_space;
			} else
			{
				lm.pos.width = 0;
			}
		}
		else
		{
			lm.pos.x -= sz_font;
		}
	}

	if (m_css.get_list_style_type() >= list_style_type_armenian)
	{
		auto marker_text = get_list_marker_text(lm.index);
		lm.pos.height = ln_height;
		if (marker_text.empty())
		{
			get_document()->container()->draw_list_marker(hdc, lm);
		}
		else
		{
			if(lm.font)
			{
				marker_text += ".";
				auto tw = get_document()->container()->text_width(marker_text.c_str(), lm.font);
				auto text_pos = lm.pos;
				text_pos.move_to(text_pos.right() - tw, text_pos.y);
				text_pos.width = tw;
				get_document()->container()->draw_text(hdc, marker_text.c_str(), lm.font, lm.color, text_pos);
			}
		}
	}
	else
	{
		get_document()->container()->draw_list_marker(hdc, lm);
	}
}

litehtml::string litehtml::html_tag::get_list_marker_text(int index)
{
	switch (m_css.get_list_style_type())
	{
	case litehtml::list_style_type_decimal:
		return std::to_string(index);
	case litehtml::list_style_type_decimal_leading_zero:
		{
			auto txt = std::to_string(index);
			if (txt.length() == 1)
			{
				txt = "0" + txt;
			}
			return txt;
		}
	case litehtml::list_style_type_lower_latin:
	case litehtml::list_style_type_lower_alpha:
		return num_cvt::to_latin_lower(index);
	case litehtml::list_style_type_lower_greek:
		return num_cvt::to_greek_lower(index);
	case litehtml::list_style_type_upper_alpha:
	case litehtml::list_style_type_upper_latin:
		return num_cvt::to_latin_upper(index);
	case litehtml::list_style_type_lower_roman:
		return num_cvt::to_roman_lower(index);
	case litehtml::list_style_type_upper_roman:
		return num_cvt::to_roman_upper(index);
	default:
		return "";
//	case litehtml::list_style_type_armenian:
//	case litehtml::list_style_type_georgian:
//	case litehtml::list_style_type_hebrew:
//	case litehtml::list_style_type_hiragana:
//	case litehtml::list_style_type_hiragana_iroha:
//	case litehtml::list_style_type_katakana:
//	case litehtml::list_style_type_katakana_iroha:
//  case litehtml::list_style_type_none:
//  case litehtml::list_style_type_circle:
//  case litehtml::list_style_type_disc:
//  case litehtml::list_style_type_square:
//  case litehtml::list_style_type_cjk_ideographic:
//      break;
	}
}

bool litehtml::html_tag::is_nth_child(const element::ptr& el, int num, int off, bool of_type) const
{
	int idx = 1;
	for(const auto& child : m_children)
	{
		if(child->css().get_display() != display_inline_text)
		{
			if( (!of_type) || (of_type && el->tag() == child->tag()) )
			{
				if(el == child)
				{
					if(num != 0)
					{
						if((idx - off) >= 0 && (idx - off) % num == 0)
						{
							return true;
						}

					} else if(idx == off)
					{
						return true;
					}
					return false;
				}
				idx++;
			}
			if(el == child) break;
		}
	}
	return false;
}

bool litehtml::html_tag::is_nth_last_child(const element::ptr& el, int num, int off, bool of_type) const
{
	int idx = 1;
	for(auto child = m_children.rbegin(); child != m_children.rend(); child++)
	{
		if((*child)->css().get_display() != display_inline_text)
		{
			if( !of_type || (of_type && el->tag() == (*child)->tag()) )
			{
				if(el == (*child))
				{
					if(num != 0)
					{
						if((idx - off) >= 0 && (idx - off) % num == 0)
						{
							return true;
						}

					} else if(idx == off)
					{
						return true;
					}
					return false;
				}
				idx++;
			}
			if(el == (*child)) break;
		}
	}
	return false;
}

litehtml::element::ptr litehtml::html_tag::find_adjacent_sibling( const element::ptr& el, const css_selector& selector, bool apply_pseudo /*= true*/, bool* is_pseudo /*= 0*/ )
{
	element::ptr ret;
	for(auto& e : m_children)
	{
		if(e->css().get_display() != display_inline_text)
		{
			if(e == el)
			{
				if(ret)
				{
					int res = ret->select(selector, apply_pseudo);
					if(res != select_no_match)
					{
						if(is_pseudo)
						{
							if(res & select_match_pseudo_class)
							{
								*is_pseudo = true;
							} else
							{
								*is_pseudo = false;
							}
						}
						return ret;
					}
				}
				return nullptr;
			} else
			{
				ret = e;
			}
		}
	}
	return nullptr;
}

litehtml::element::ptr litehtml::html_tag::find_sibling(const element::ptr& el, const css_selector& selector, bool apply_pseudo /*= true*/, bool* is_pseudo /*= 0*/)
{
	element::ptr ret = nullptr;
	for(auto& e : m_children)
	{
		if(e->css().get_display() != display_inline_text)
		{
			if(e == el)
			{
				return ret;
			} else if(!ret)
			{
				int res = e->select(selector, apply_pseudo);
				if(res != select_no_match)
				{
					if(is_pseudo)
					{
						if(res & select_match_pseudo_class)
						{
							*is_pseudo = true;
						} else
						{
							*is_pseudo = false;
						}
					}
					ret = e;
				}
			}
		}
	}
	return nullptr;
}

bool litehtml::html_tag::is_only_child(const element::ptr& el, bool of_type) const
{
	int child_count = 0;
	for(const auto& child : m_children)
	{
		if(child->css().get_display() != display_inline_text)
		{
			if( !of_type || (of_type && el->tag() == child->tag()) )
			{
				child_count++;
			}
			if(child_count > 1) break;
		}
	}
	if(child_count > 1)
	{
		return false;
	}
	return true;
}

litehtml::element::ptr litehtml::html_tag::get_element_before(const style& style, bool create)
{
	if(!m_children.empty())
	{
		if( m_children.front()->tag() == __tag_before_ )
		{
			return m_children.front();
		}
	}
	if(create)
	{
		return add_pseudo_before(style);
	}
	return nullptr;
}

litehtml::element::ptr litehtml::html_tag::get_element_after(const style& style, bool create)
{
	if(!m_children.empty())
	{
		if( m_children.back()->tag() == __tag_after_ )
		{
			return m_children.back();
		}
	}
	if(create)
	{
		return add_pseudo_after(style);
	}
	return nullptr;
}


void litehtml::html_tag::handle_counter_properties()
{
	const auto& reset_property = m_style.get_property(string_id::_counter_reset_);
	if (reset_property.m_type == prop_type_string_vector) {
		auto reset_function = [&](const string_id&name_id, const int value) {
			reset_counter(name_id, value);
		};
		parse_counter_tokens(reset_property.m_string_vector, 0, reset_function);
		return;
	}

	const auto& inc_property = m_style.get_property(string_id::_counter_increment_);
	if (inc_property.m_type == prop_type_string_vector) {
		auto inc_function = [&](const string_id&name_id, const int value) {
			increment_counter(name_id, value);
		};
		parse_counter_tokens(inc_property.m_string_vector, 1, inc_function);
		return;
	}
}


void litehtml::html_tag::add_style(const style& style)
{
	m_style.combine(style);
	handle_counter_properties();
}

void litehtml::html_tag::refresh_styles()
{
	for (auto& el : m_children)
	{
		if(el->css().get_display() != display_inline_text)
		{
			el->refresh_styles();
		}
	}

	m_style.clear();

	for (auto& usel : m_used_styles)
	{
		usel->m_used = false;

		if(usel->m_selector->is_media_valid())
		{
			int apply = select(*usel->m_selector, false);

			if(apply != select_no_match)
			{
				if(apply & select_match_pseudo_class)
				{
					if(select(*usel->m_selector, true))
					{
						if(apply & select_match_with_after)
						{
							element::ptr el = get_element_after(*usel->m_selector->m_style, false);
							if(el)
							{
								el->add_style(*usel->m_selector->m_style);
							}
						} else if(apply & select_match_with_before)
						{
							element::ptr el = get_element_before(*usel->m_selector->m_style, false);
							if(el)
							{
								el->add_style(*usel->m_selector->m_style);
							}
						}
						else
						{
							add_style(*usel->m_selector->m_style);
							usel->m_used = true;
						}
					}
				} else if(apply & select_match_with_after)
				{
					element::ptr el = get_element_after(*usel->m_selector->m_style, false);
					if(el)
					{
						el->add_style(*usel->m_selector->m_style);
					}
				} else if(apply & select_match_with_before)
				{
					element::ptr el = get_element_before(*usel->m_selector->m_style, false);
					if(el)
					{
						el->add_style(*usel->m_selector->m_style);
					}
				} else
				{
					add_style(*usel->m_selector->m_style);
					usel->m_used = true;
				}
			}
		}
	}
}

const litehtml::background* litehtml::html_tag::get_background(bool own_only)
{
	if(own_only)
	{
		// return own background with check for empty one
		if(m_css.get_bg().is_empty())
		{
			return nullptr;
		}
		return &m_css.get_bg();
	}

	if(m_css.get_bg().is_empty())
	{
		// if this is root element (<html>) try to get background from body
		if (is_root())
		{
			for (const auto& el : m_children)
			{
				if( el->is_body() )
				{
					// return own body background
					return el->get_background(true);
				}
			}
		}
		return nullptr;
	}
	
	if(is_body())
	{
		element::ptr el_parent = parent();
		if (el_parent)
		{
			if (!el_parent->get_background(true))
			{
				// parent of body will draw background for body
				return nullptr;
			}
		}
	}

	return &m_css.get_bg();
}

litehtml::string litehtml::html_tag::dump_get_name()
{
	if(m_tag == empty_id)
	{
		return "anon [html_tag]";
	}
	return _s(m_tag) + " [html_tag]";
}
