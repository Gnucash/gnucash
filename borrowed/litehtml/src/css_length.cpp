#include "html.h"
#include "css_length.h"

void litehtml::css_length::fromString( const string& str, const string& predefs, int defValue )
{
	// TODO: Make support for calc
	if(str.substr(0, 4) == "calc")
	{
		m_is_predefined = true;
		m_predef		= defValue;
		return;
	}

	int predef = value_index(str, predefs, -1);
	if(predef >= 0)
	{
		m_is_predefined = true;
		m_predef		= predef;
	} else
	{
		m_is_predefined = false;

		string num;
		string un;
		bool is_unit = false;
		for(char chr : str)
		{
			if(!is_unit)
			{
				if(t_isdigit(chr) || chr == '.' || chr == '+' || chr == '-')
				{
					num += chr;
				} else
				{
					is_unit = true;
				}
			}
			if(is_unit)
			{
				un += chr;
			}
		}
		if(!num.empty())
		{
			m_value = t_strtof(num);
			m_units	= (css_units) value_index(un, css_units_strings, css_units_none);
		} else
		{
			// not a number so it is predefined
			m_is_predefined = true;
			m_predef = defValue;
		}
	}
}

litehtml::css_length litehtml::css_length::from_string(const string& str, const string& predefs, int defValue)
{
	css_length len;
	len.fromString(str, predefs, defValue);
	return len;
}

litehtml::string litehtml::css_length::to_string() const
{
    if(m_is_predefined)
    {
        return "def(" + std::to_string(m_predef) + ")";
    }
    return std::to_string(m_value) + "{" + index_value(m_units, css_units_strings) + "}";
}

litehtml::css_length litehtml::css_length::predef_value(int val)
{
	css_length len;
	len.predef(val);
	return len;
}
