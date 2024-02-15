#include "html.h"
#include "el_table.h"
#include "document.h"
#include "iterators.h"


litehtml::el_table::el_table(const std::shared_ptr<document>& doc) : html_tag(doc)
{
}


bool litehtml::el_table::appendChild(const element::ptr& el)
{
	if(!el)	return false;
	if( el->tag() == _tbody_ ||
		el->tag() == _thead_ ||
		el->tag() == _tfoot_ ||
		el->tag() == _caption_)
	{
		return html_tag::appendChild(el);
	}
	return false;
}

void litehtml::el_table::parse_attributes()
{
	const char* str = get_attr("width");
	if(str)
	{
		m_style.add_property(_width_, str);
	}

	str = get_attr("cellspacing");
	if(str)
	{
		string val = str;
		val += " ";
		val += str;
		m_style.add_property(_border_spacing_, val);
	}
	
	str = get_attr("border");
	if(str)
	{
		m_style.add_property(_border_width_, str);
	}

	str = get_attr("bgcolor");
	if (str)
	{
		m_style.add_property(_background_color_, str, "", false, get_document()->container());
	}

	html_tag::parse_attributes();
}
