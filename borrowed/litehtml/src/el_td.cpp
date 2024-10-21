#include "html.h"
#include "el_td.h"


litehtml::el_td::el_td(const std::shared_ptr<document>& doc) : html_tag(doc)
{

}

void litehtml::el_td::parse_attributes()
{
	const char* str = get_attr("width");
	if(str)
	{
		m_style.add_property(_width_, str);
	}
	str = get_attr("background");
	if(str)
	{
		string url = "url('";
		url += str;
		url += "')";
		m_style.add_property(_background_image_, url);
	}
	str = get_attr("align");
	if(str)
	{
		m_style.add_property(_text_align_, str);
	}

	str = get_attr("bgcolor");
	if (str)
	{
		m_style.add_property(_background_color_, str, "", false, get_document()->container());
	}

	str = get_attr("valign");
	if(str)
	{
		m_style.add_property(_vertical_align_, str);
	}
	html_tag::parse_attributes();
}
