#ifndef LITEHTML_RENDER_IMAGE_H
#define LITEHTML_RENDER_IMAGE_H

#include "render_item.h"

namespace litehtml
{
	class render_item_image : public render_item
	{
	protected:
		int calc_max_height(int image_height, int containing_block_height);
		int _render(int x, int y, const containing_block_context &containing_block_size, formatting_context* fmt_ctx, bool second_pass) override;

	public:
		explicit render_item_image(std::shared_ptr<element>  src_el) : render_item(std::move(src_el))
		{}

		std::shared_ptr<render_item> clone() override
		{
			return std::make_shared<render_item_image>(src_el());
		}
	};
}

#endif //LITEHTML_RENDER_IMAGE_H
