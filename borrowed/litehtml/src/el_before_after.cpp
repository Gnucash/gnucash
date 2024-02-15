#include "html.h"
#include "el_before_after.h"
#include "el_text.h"
#include "el_space.h"
#include "el_image.h"
#include "utf8_strings.h"

litehtml::el_before_after_base::el_before_after_base(const std::shared_ptr<document>& doc, bool before) : html_tag(doc)
{
	m_tag = before ? __tag_before_ : __tag_after_;
}

void litehtml::el_before_after_base::add_style(const style& style)
{
	html_tag::add_style(style);

	auto children = m_children;
	m_children.clear();

	const auto& content_property = style.get_property(_content_);
	if(content_property.m_type == prop_type_string && !content_property.m_string.empty())
	{
		int idx = value_index(content_property.m_string, content_property_string);
		if(idx < 0)
		{
			string fnc;
			string::size_type i = 0;
			while(i < content_property.m_string.length() && i != string::npos)
			{
				if(content_property.m_string.at(i) == '"' || content_property.m_string.at(i) == '\'')
				{
                    auto chr = content_property.m_string.at(i);
					fnc.clear();
					i++;
					string::size_type pos = content_property.m_string.find(chr, i);
					string txt;
					if(pos == string::npos)
					{
						txt = content_property.m_string.substr(i);
						i = string::npos;
					} else
					{
						txt = content_property.m_string.substr(i, pos - i);
						i = pos + 1;
					}
					add_text(txt);
				} else if(content_property.m_string.at(i) == '(')
				{
					i++;
					litehtml::trim(fnc);
					litehtml::lcase(fnc);
					string::size_type pos = content_property.m_string.find(')', i);
					string params;
					if(pos == string::npos)
					{
						params = content_property.m_string.substr(i);
						i = string::npos;
					} else
					{
						params = content_property.m_string.substr(i, pos - i);
						i = pos + 1;
					}
					add_function(fnc, params);
					fnc.clear();
				} else
				{
					fnc += content_property.m_string.at(i);
					i++;
				}
			}
		}
	}

	if(m_children.empty())
	{
		m_children = children;
	}
}

void litehtml::el_before_after_base::add_text( const string& txt )
{
	string word;
	string esc;

	for(auto chr : txt)
	{
		if(chr == '\\' ||
			(!esc.empty() && esc.length() < 5 &&
					(
						(chr >= '0' && chr <= '9') ||
						(chr >= 'A' && chr <= 'Z') ||
						(chr >= 'a' && chr <= 'z')
					)
			))
		{
			if(!esc.empty() && chr == '\\')
			{
				word += convert_escape(esc.c_str() + 1);
				esc.clear();
			}
			esc += chr;
		} else
		{
			if(!esc.empty())
			{
				word += convert_escape(esc.c_str() + 1);
				esc.clear();
			}
			if(isspace(chr))
			{
				if(!word.empty())
				{
					element::ptr el = std::make_shared<el_text>(word.c_str(), get_document());
					appendChild(el);
					word.clear();
				}
				word += chr;
				element::ptr el = std::make_shared<el_space>(word.c_str(), get_document());
				appendChild(el);
				word.clear();
			} else
			{
				word += chr;
			}
		}
	}

	if(!esc.empty())
	{
		word += convert_escape(esc.c_str() + 1);
	}
	if(!word.empty())
	{
		element::ptr el = std::make_shared<el_text>(word.c_str(), get_document());
		appendChild(el);
		word.clear();
	}
}

void litehtml::el_before_after_base::add_function( const string& fnc, const string& params )
{
	int idx = value_index(fnc, "attr;counter;counters;url");
	switch(idx)
	{
	// attr
	case 0:
		{
			string p_name = params;
			trim(p_name);
			lcase(p_name);
			element::ptr el_parent = parent();
			if (el_parent)
			{
				const char* attr_value = el_parent->get_attr(p_name.c_str());
				if (attr_value)
				{
					add_text(attr_value);
				}
			}
		}
		break;
	// counter
	case 1:
		add_text(get_counter_value(params));
		break;
	// counters
	case 2:
		{
			string_vector tokens;
			split_string(params, tokens, ",");
			add_text(get_counters_value(tokens));
		}
		break;
	// url
	case 3:
		{
			string p_url = params;
			trim(p_url);
			if(!p_url.empty())
			{
				if(p_url.at(0) == '\'' || p_url.at(0) == '\"')
				{
					p_url.erase(0, 1);
				}
			}
			if(!p_url.empty())
			{
				if(p_url.at(p_url.length() - 1) == '\'' || p_url.at(p_url.length() - 1) == '\"')
				{
					p_url.erase(p_url.length() - 1, 1);
				}
			}
			if(!p_url.empty())
			{
				element::ptr el = std::make_shared<el_image>(get_document());
				el->set_attr("src", p_url.c_str());
				el->set_attr("style", "display:inline-block");
				el->set_tagName("img");
				appendChild(el);
				el->parse_attributes();
			}
		}
		break;
	}
}

litehtml::string litehtml::el_before_after_base::convert_escape( const char* txt )
{
    char* str_end;
	wchar_t u_str[2];
    u_str[0] = (wchar_t) strtol(txt, &str_end, 16);
    u_str[1] = 0;
	return litehtml::string(litehtml_from_wchar(u_str));
}
