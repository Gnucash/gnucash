#ifndef LH_EL_IMAGE_H
#define LH_EL_IMAGE_H

#include "html_tag.h"

namespace litehtml
{

	class el_image : public html_tag
	{
		string	m_src;
	public:
		el_image(const document::ptr& doc);

		bool	is_replaced() const override;
		void	parse_attributes() override;
		void	compute_styles(bool recursive = true) override;
		void	draw(uint_ptr hdc, int x, int y, const position *clip, const std::shared_ptr<render_item> &ri) override;
		void	get_content_size(size& sz, int max_width) override;
		string	dump_get_name() override;

		std::shared_ptr<render_item> create_render_item(const std::shared_ptr<render_item>& parent_ri) override;

	private:
//		int calc_max_height(int image_height);
	};
}

#endif  // LH_EL_IMAGE_H
