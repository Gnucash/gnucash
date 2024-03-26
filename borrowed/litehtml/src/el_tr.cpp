#include "html.h"
#include "el_tr.h"


litehtml::el_tr::el_tr(const std::shared_ptr<document>& doc) : html_tag(doc)
{

}

void litehtml::el_tr::parse_attributes()
{
	const char* str = get_attr("align");
	if(str)
	{
		m_style.add_property(_text_align_, str);
	}
	str = get_attr("valign");
	if(str)
	{
		m_style.add_property(_vertical_align_, str);
	}
	str = get_attr("bgcolor");
	if (str)
	{
		m_style.add_property(_background_color_, str, "", false, get_document()->container());
	}
	html_tag::parse_attributes();
}
