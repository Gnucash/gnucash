#ifndef LH_HTML_TAG_H
#define LH_HTML_TAG_H

#include "element.h"
#include "style.h"
#include "background.h"
#include "css_margins.h"
#include "borders.h"
#include "css_selector.h"
#include "stylesheet.h"
#include "line_box.h"
#include "table.h"

namespace litehtml
{

	class html_tag : public element
	{
		friend class elements_iterator;
		friend class el_table;
		friend class table_grid;
		friend class line_box;
	public:
		typedef std::shared_ptr<html_tag>	ptr;
	protected:
		string_id				m_tag;
		string_id				m_id;
		string_vector			m_str_classes;
		std::vector<string_id>	m_classes;
		litehtml::style			m_style;
		string_map				m_attrs;
		std::vector<string_id>	m_pseudo_classes;

		void			select_all(const css_selector& selector, elements_list& res) override;

	public:
		explicit html_tag(const std::shared_ptr<document>& doc);
		// constructor for anonymous wrapper boxes
		explicit html_tag(const element::ptr& parent, const string& style = "display: block");

		bool				appendChild(const element::ptr &el) override;
		bool				removeChild(const element::ptr &el) override;
		void				clearRecursive() override;
		string_id			tag() const override;
		string_id			id() const override;
		const char*			get_tagName() const override;
		void				set_tagName(const char* tag) override;
		void				set_data(const char* data) override;

		void				set_attr(const char* name, const char* val) override;
		const char*			get_attr(const char* name, const char* def = nullptr) const override;
		void				apply_stylesheet(const litehtml::css& stylesheet) override;
		void				refresh_styles() override;

		bool				is_white_space() const override;
		bool				is_body() const override;
		bool				is_break() const override;

		bool				on_mouse_over() override;
		bool				on_mouse_leave() override;
		bool				on_lbutton_down() override;
		bool				on_lbutton_up() override;
		void				on_click() override;
		bool				set_pseudo_class(string_id cls, bool add) override;
		bool				set_class(const char* pclass, bool add) override;
		bool				is_replaced() const override;
		void				compute_styles(bool recursive = true) override;
		void				draw(uint_ptr hdc, int x, int y, const position *clip, const std::shared_ptr<render_item> &ri) override;
		void				draw_background(uint_ptr hdc, int x, int y, const position *clip,
									const std::shared_ptr<render_item> &ri) override;

		template<class Type, property_type property_value_type, Type property_value::* property_value_member>
		const Type&			get_property_impl  (string_id name, bool inherited, const Type&   default_value, uint_ptr css_properties_member_offset) const;
		int					get_enum_property  (string_id name, bool inherited, int           default_value, uint_ptr css_properties_member_offset) const override;
		int					get_int_property   (string_id name, bool inherited, int           default_value, uint_ptr css_properties_member_offset) const override;
		css_length			get_length_property(string_id name, bool inherited, css_length    default_value, uint_ptr css_properties_member_offset) const override;
		web_color			get_color_property (string_id name, bool inherited, web_color     default_value, uint_ptr css_properties_member_offset) const override;
		string				get_string_property(string_id name, bool inherited, const string& default_value, uint_ptr css_properties_member_offset) const override;
		float				get_number_property(string_id name, bool inherited, float         default_value, uint_ptr css_properties_member_offset) const override;
		string_vector		get_string_vector_property(string_id name, bool inherited, const string_vector& default_value, uint_ptr css_properties_member_offset) const override;
		int_vector			get_int_vector_property   (string_id name, bool inherited, const int_vector&    default_value, uint_ptr css_properties_member_offset) const override;
		length_vector		get_length_vector_property(string_id name, bool inherited, const length_vector& default_value, uint_ptr css_properties_member_offset) const override;
		size_vector			get_size_vector_property  (string_id name, bool inherited, const size_vector&   default_value, uint_ptr css_properties_member_offset) const override;
		string				get_custom_property(string_id name, const string& default_value) const override;

		elements_list&	children();

		int					select(const string& selector) override;
		int					select(const css_selector& selector, bool apply_pseudo = true) override;
		int					select(const css_element_selector& selector, bool apply_pseudo = true) override;
		int					select_pseudoclass(const css_attribute_selector& sel);
		int					select_attribute(const css_attribute_selector& sel);

		elements_list		select_all(const string& selector) override;
		elements_list		select_all(const css_selector& selector) override;

		element::ptr		select_one(const string& selector) override;
		element::ptr		select_one(const css_selector& selector) override;

		element::ptr		find_ancestor(const css_selector& selector, bool apply_pseudo = true, bool* is_pseudo = nullptr) override;
		element::ptr		find_adjacent_sibling(const element::ptr& el, const css_selector& selector, bool apply_pseudo = true, bool* is_pseudo = nullptr) override;
		element::ptr		find_sibling(const element::ptr& el, const css_selector& selector, bool apply_pseudo = true, bool* is_pseudo = nullptr) override;
		void				get_text(string& text) override;
		void				parse_attributes() override;

		void				get_content_size(size& sz, int max_width) override;
		void				add_style(const style& style) override;

		bool				is_nth_child(const element::ptr& el, int num, int off, bool of_type) const override;
		bool				is_nth_last_child(const element::ptr& el, int num, int off, bool of_type) const override;
		bool				is_only_child(const element::ptr& el, bool of_type) const override;
		const background*	get_background(bool own_only = false) override;

		string				dump_get_name() override;

	protected:
		void				init_background_paint(position pos, std::vector<background_paint>& bg_paint, const background* bg, const std::shared_ptr<render_item>& ri);
		void				init_one_background_paint(int i, position pos, background_paint& bg_paint, const background* bg, const std::shared_ptr<render_item>& ri);
		void				draw_list_marker( uint_ptr hdc, const position &pos );
		string				get_list_marker_text(int index);
		element::ptr		get_element_before(const style& style, bool create);
		element::ptr		get_element_after(const style& style, bool create);

	private:
		void				handle_counter_properties();

	};

	/************************************************************************/
	/*                        Inline Functions                              */
	/************************************************************************/

	inline elements_list& litehtml::html_tag::children()
	{
		return m_children;
	}
}

#endif  // LH_HTML_TAG_H
