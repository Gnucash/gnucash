#ifndef LH_STYLE_H
#define LH_STYLE_H

namespace litehtml
{
	enum property_type
	{
		prop_type_invalid, // indicates "not found" condition in style::get_property
		prop_type_inherit, // "inherit" was specified as the value of this property

		prop_type_enum_item,
		prop_type_enum_item_vector,
		prop_type_length,
		prop_type_length_vector,
		prop_type_number,
		prop_type_color,
		prop_type_string,
		prop_type_string_vector,
		prop_type_size_vector,

		prop_type_var, // also string, but needs further parsing because of var()
	};

	class property_value
	{
	public:
		property_type	m_type;
		bool			m_important;

		union {
			int 			m_enum_item;
			int_vector		m_enum_item_vector;
			css_length		m_length;
			length_vector	m_length_vector;
			float			m_number;
			web_color		m_color;
			string			m_string;
			string_vector	m_string_vector;
			size_vector		m_size_vector;
		};

		property_value()
			: m_type(prop_type_invalid)
		{
		}
		property_value(bool important, property_type type)
			: m_type(type), m_important(important)
		{
		}
		property_value(const string& str, bool important, property_type type = prop_type_string)
			: m_type(type), m_important(important), m_string(str)
		{
		}
		property_value(const string_vector& vec, bool important)
			: m_type(prop_type_string_vector), m_important(important), m_string_vector(vec)
		{
		}
		property_value(const css_length& length, bool important)
			: m_type(prop_type_length), m_important(important), m_length(length)
		{
		}
		property_value(const length_vector& vec, bool important)
			: m_type(prop_type_length_vector), m_important(important), m_length_vector(vec)
		{
		}
		property_value(float number, bool important)
			: m_type(prop_type_number), m_important(important), m_number(number)
		{
		}
		property_value(int enum_item, bool important)
			: m_type(prop_type_enum_item), m_important(important), m_enum_item(enum_item)
		{
		}
		property_value(const int_vector& vec, bool important)
			: m_type(prop_type_enum_item_vector), m_important(important), m_enum_item_vector(vec)
		{
		}
		property_value(web_color color, bool important)
			: m_type(prop_type_color), m_important(important), m_color(color)
		{
		}
		property_value(const size_vector& vec, bool important)
			: m_type(prop_type_size_vector), m_important(important), m_size_vector(vec)
		{
		}
		~property_value()
		{
			switch (m_type)
			{
			case prop_type_string:
			case prop_type_var:
				m_string.~string();
				break;
			case prop_type_string_vector:
				m_string_vector.~string_vector();
				break;
			case prop_type_length:
				m_length.~css_length();
				break;
			case prop_type_length_vector:
				m_length_vector.~length_vector();
				break;
			case prop_type_enum_item_vector:
				m_enum_item_vector.~int_vector();
				break;
			case prop_type_color:
				m_color.~web_color();
				break;
			case prop_type_size_vector:
				m_size_vector.~size_vector();
				break;
			default:
				break;
			}
		}
		property_value& operator=(const property_value& val)
		{
			this->~property_value();

			switch (val.m_type)
			{
			case prop_type_invalid:
				new(this) property_value();
				break;
			case prop_type_inherit:
				new(this) property_value(val.m_important, val.m_type);
				break;
			case prop_type_string:
			case prop_type_var:
				new(this) property_value(val.m_string, val.m_important, val.m_type);
				break;
			case prop_type_string_vector:
				new(this) property_value(val.m_string_vector, val.m_important);
				break;
			case prop_type_enum_item:
				new(this) property_value(val.m_enum_item, val.m_important);
				break;
			case prop_type_enum_item_vector:
				new(this) property_value(val.m_enum_item_vector, val.m_important);
				break;
			case prop_type_length:
				new(this) property_value(val.m_length, val.m_important);
				break;
			case prop_type_length_vector:
				new(this) property_value(val.m_length_vector, val.m_important);
				break;
			case prop_type_number:
				new(this) property_value(val.m_number, val.m_important);
				break;
			case prop_type_color:
				new(this) property_value(val.m_color, val.m_important);
				break;
			case prop_type_size_vector:
				new(this) property_value(val.m_size_vector, val.m_important);
				break;
			}

			return *this;
		}
	};

	typedef std::map<string_id, property_value>	props_map;

	class style
	{
	public:
		typedef std::shared_ptr<style>		ptr;
		typedef std::vector<style::ptr>		vector;
	private:
		props_map							m_properties;
		static std::map<string_id, string>	m_valid_values;
	public:
		void add(const string& txt, const string& baseurl = "", document_container* container = nullptr)
		{
			parse(txt, baseurl, container);
		}

		void add_property(string_id name, const string& val, const string& baseurl = "", bool important = false, document_container* container = nullptr);

		const property_value& get_property(string_id name) const;

		void combine(const style& src);
		void clear()
		{
			m_properties.clear();
		}

		void subst_vars(const element* el);

	private:
		void parse_property(const string& txt, const string& baseurl, document_container* container);
		void parse(const string& txt, const string& baseurl, document_container* container);
		void parse_background(const string& val, const string& baseurl, bool important, document_container* container);
		bool parse_one_background(const string& val, document_container* container, background& bg);
		void parse_background_image(const string& val, const string& baseurl, bool important);
		// parse comma-separated list of keywords
		void parse_keyword_comma_list(string_id name, const string& val, bool important);
		void parse_background_position(const string& val, bool important);
		bool parse_one_background_position(const string& val, css_length& x, css_length& y);
		void parse_background_size(const string& val, bool important);
		bool parse_one_background_size(const string& val, css_size& size);
		void parse_font(const string& val, bool important);
		void parse_flex(const string& val, bool important);
		void parse_align_self(string_id name, const string& val, bool important);
		static css_length parse_border_width(const string& str);
		static void parse_two_lengths(const string& str, css_length len[2]);
		static int parse_four_lengths(const string& str, css_length len[4]);
		static void subst_vars_(string& str, const element* el);

		void add_parsed_property(string_id name, const property_value& propval);
		void remove_property(string_id name, bool important);
	};
}

#endif  // LH_STYLE_H
