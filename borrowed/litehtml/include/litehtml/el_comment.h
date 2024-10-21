#ifndef LH_EL_COMMENT_H
#define LH_EL_COMMENT_H

#include "html_tag.h"

namespace litehtml
{
	class el_comment : public element
	{
		string	m_text;
	public:
		explicit el_comment(const std::shared_ptr<document>& doc);

		bool is_comment() const override;
		void get_text(string& text) override;
		void set_data(const char* data) override;
        std::shared_ptr<render_item> create_render_item(const std::shared_ptr<render_item>& /*parent_ri*/) override
        {
            // Comments are not rendered
            return nullptr;
        }
	};
}

#endif  // LH_EL_COMMENT_H
