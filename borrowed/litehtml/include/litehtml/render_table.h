#ifndef LITEHTML_RENDER_TABLE_H
#define LITEHTML_RENDER_TABLE_H

#include "render_item.h"

namespace litehtml
{
	class render_item_table : public render_item
	{
	protected:
		// data for table rendering
		std::unique_ptr<table_grid>	m_grid;
		int						    m_border_spacing_x;
		int						    m_border_spacing_y;

		int _render(int x, int y, const containing_block_context &containing_block_size, formatting_context* fmt_ctx, bool second_pass) override;

	public:
		explicit render_item_table(std::shared_ptr<element>  src_el);

		std::shared_ptr<render_item> clone() override
		{
			return std::make_shared<render_item_table>(src_el());
		}
		void draw_children(uint_ptr hdc, int x, int y, const position* clip, draw_flag flag, int zindex) override;
		int get_draw_vertical_offset() override;
		std::shared_ptr<render_item> init() override;
	};

	class render_item_table_part : public render_item
	{
	public:
		explicit render_item_table_part(std::shared_ptr<element>  src_el) : render_item(std::move(src_el))
		{}

		std::shared_ptr<render_item> clone() override
		{
			return std::make_shared<render_item_table_part>(src_el());
		}
	};

	class render_item_table_row : public render_item
	{
	public:
		explicit render_item_table_row(std::shared_ptr<element>  src_el) : render_item(std::move(src_el))
		{}

		std::shared_ptr<render_item> clone() override
		{
			return std::make_shared<render_item_table_row>(src_el());
		}
		void get_inline_boxes( position::vector& boxes ) const override;
	};
}

#endif //LITEHTML_RENDER_TABLE_H
