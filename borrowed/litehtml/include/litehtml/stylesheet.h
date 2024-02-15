#ifndef LH_STYLESHEET_H
#define LH_STYLESHEET_H

#include "style.h"
#include "css_selector.h"

namespace litehtml
{
	class document_container;

	class css
	{
		css_selector::vector	m_selectors;
	public:
		css() = default;
		~css() = default;

		const css_selector::vector& selectors() const
		{
			return m_selectors;
		}

		void clear()
		{
			m_selectors.clear();
		}

		void	parse_stylesheet(const char* str, const char* baseurl, const std::shared_ptr<document>& doc, const media_query_list::ptr& media);
		void	sort_selectors();
		static void	parse_css_url(const string& str, string& url);

	private:
		void	parse_atrule(const string& text, const char* baseurl, const std::shared_ptr<document>& doc, const media_query_list::ptr& media);
		void	add_selector(const css_selector::ptr& selector);
		bool	parse_selectors(const string& txt, const style::ptr& styles, const media_query_list::ptr& media);

	};

	inline void litehtml::css::add_selector( const css_selector::ptr& selector )
	{
		selector->m_order = (int) m_selectors.size();
		m_selectors.push_back(selector);
	}

}

#endif  // LH_STYLESHEET_H
