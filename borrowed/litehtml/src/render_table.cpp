#include "html.h"
#include "render_table.h"
#include "document.h"
#include "iterators.h"


litehtml::render_item_table::render_item_table(std::shared_ptr<element> _src_el) :
        render_item(std::move(_src_el)),
        m_border_spacing_x(0),
        m_border_spacing_y(0)
{
}

int litehtml::render_item_table::_render(int x, int y, const containing_block_context &containing_block_size, formatting_context* fmt_ctx, bool /*second_pass*/)
{
    if (!m_grid) return 0;

	containing_block_context self_size = calculate_containing_block_context(containing_block_size);

    // Calculate table spacing
    int table_width_spacing = 0;
    if (src_el()->css().get_border_collapse() == border_collapse_separate)
    {
        table_width_spacing = m_border_spacing_x * (m_grid->cols_count() + 1);
    }
    else
    {
        table_width_spacing = 0;

        if (m_grid->cols_count())
        {
            table_width_spacing -= std::min(border_left(), m_grid->column(0).border_left);
            table_width_spacing -= std::min(border_right(), m_grid->column(m_grid->cols_count() - 1).border_right);
        }

        for (int col = 1; col < m_grid->cols_count(); col++)
        {
            table_width_spacing -= std::min(m_grid->column(col).border_left, m_grid->column(col - 1).border_right);
        }
    }


    // Calculate the minimum content width (MCW) of each cell: the formatted content may span any number of lines but may not overflow the cell box.
    // If the specified 'width' (W) of the cell is greater than MCW, W is the minimum cell width. A value of 'auto' means that MCW is the minimum
    // cell width.
    //
    // Also, calculate the "maximum" cell width of each cell: formatting the content without breaking lines other than where explicit line breaks occur.

    if (m_grid->cols_count() == 1 && self_size.width.type != containing_block_context::cbc_value_type_auto)
    {
        for (int row = 0; row < m_grid->rows_count(); row++)
        {
            table_cell* cell = m_grid->cell(0, row);
            if (cell && cell->el)
            {
                cell->min_width = cell->max_width = cell->el->render(0, 0, self_size.new_width(self_size.render_width - table_width_spacing), fmt_ctx);
                cell->el->pos().width = cell->min_width - cell->el->content_offset_left() -
						cell->el->content_offset_right();
            }
        }
    }
    else
    {
        for (int row = 0; row < m_grid->rows_count(); row++)
        {
            for (int col = 0; col < m_grid->cols_count(); col++)
            {
                table_cell* cell = m_grid->cell(col, row);
                if (cell && cell->el)
                {
                    if (!m_grid->column(col).css_width.is_predefined() && m_grid->column(col).css_width.units() != css_units_percentage)
                    {
                        int css_w = m_grid->column(col).css_width.calc_percent(self_size.width);
                        int el_w = cell->el->render(0, 0, self_size.new_width(css_w),fmt_ctx);
                        cell->min_width = cell->max_width = std::max(css_w, el_w);
                        cell->el->pos().width = cell->min_width - cell->el->content_offset_left() -
								cell->el->content_offset_right();
                    }
                    else
                    {
                        // calculate minimum content width
                        cell->min_width = cell->el->render(0, 0, self_size.new_width(cell->el->content_offset_width()), fmt_ctx);
                        // calculate maximum content width
                        cell->max_width = cell->el->render(0, 0, self_size.new_width(self_size.render_width - table_width_spacing), fmt_ctx);
                    }
                }
            }
        }
    }

    // For each column, determine a maximum and minimum column width from the cells that span only that column.
    // The minimum is that required by the cell with the largest minimum cell width (or the column 'width', whichever is larger).
    // The maximum is that required by the cell with the largest maximum cell width (or the column 'width', whichever is larger).

    for (int col = 0; col < m_grid->cols_count(); col++)
    {
        m_grid->column(col).max_width = 0;
        m_grid->column(col).min_width = 0;
        for (int row = 0; row < m_grid->rows_count(); row++)
        {
            if (m_grid->cell(col, row)->colspan <= 1)
            {
                m_grid->column(col).max_width = std::max(m_grid->column(col).max_width, m_grid->cell(col, row)->max_width);
                m_grid->column(col).min_width = std::max(m_grid->column(col).min_width, m_grid->cell(col, row)->min_width);
            }
        }
    }

    // For each cell that spans more than one column, increase the minimum widths of the columns it spans so that together,
    // they are at least as wide as the cell. Do the same for the maximum widths.
    // If possible, widen all spanned columns by approximately the same amount.

    for (int col = 0; col < m_grid->cols_count(); col++)
    {
        for (int row = 0; row < m_grid->rows_count(); row++)
        {
            if (m_grid->cell(col, row)->colspan > 1)
            {
                int max_total_width = m_grid->column(col).max_width;
                int min_total_width = m_grid->column(col).min_width;
                for (int col2 = col + 1; col2 < col + m_grid->cell(col, row)->colspan; col2++)
                {
                    max_total_width += m_grid->column(col2).max_width;
                    min_total_width += m_grid->column(col2).min_width;
                }
                if (min_total_width < m_grid->cell(col, row)->min_width)
                {
                    m_grid->distribute_min_width(m_grid->cell(col, row)->min_width - min_total_width, col, col + m_grid->cell(col, row)->colspan - 1);
                }
                if (max_total_width < m_grid->cell(col, row)->max_width)
                {
                    m_grid->distribute_max_width(m_grid->cell(col, row)->max_width - max_total_width, col, col + m_grid->cell(col, row)->colspan - 1);
                }
            }
        }
    }

    // If the 'table' or 'inline-table' element's 'width' property has a computed value (W) other than 'auto', the used width is the
    // greater of W, CAPMIN, and the minimum width required by all the columns plus cell spacing or borders (MIN).
    // If the used width is greater than MIN, the extra width should be distributed over the columns.
    //
    // If the 'table' or 'inline-table' element has 'width: auto', the used width is the greater of the table's containing block width,
    // CAPMIN, and MIN. However, if either CAPMIN or the maximum width required by the columns plus cell spacing or borders (MAX) is
    // less than that of the containing block, use max(MAX, CAPMIN).


    int table_width = 0;
    int min_table_width = 0;
    int max_table_width = 0;

    if (self_size.width.type == containing_block_context::cbc_value_type_absolute)
    {
        table_width = m_grid->calc_table_width(self_size.render_width - table_width_spacing, false, min_table_width, max_table_width);
    }
    else
    {
        table_width = m_grid->calc_table_width(self_size.render_width - table_width_spacing, self_size.width.type == containing_block_context::cbc_value_type_auto, min_table_width, max_table_width);
    }

    min_table_width += table_width_spacing;
    max_table_width += table_width_spacing;
    table_width += table_width_spacing;
    m_grid->calc_horizontal_positions(m_borders, src_el()->css().get_border_collapse(), m_border_spacing_x);

    bool row_span_found = false;

    // render cells with computed width
    for (int row = 0; row < m_grid->rows_count(); row++)
    {
        m_grid->row(row).height = 0;
        for (int col = 0; col < m_grid->cols_count(); col++)
        {
            table_cell* cell = m_grid->cell(col, row);
            if (cell->el)
            {
                int span_col = col + cell->colspan - 1;
                if (span_col >= m_grid->cols_count())
                {
                    span_col = m_grid->cols_count() - 1;
                }
                int cell_width = m_grid->column(span_col).right - m_grid->column(col).left;

                //if (cell->el->pos().width != cell_width - cell->el->content_offset_left() -
				//									 cell->el->content_offset_right())
                {
                    cell->el->render(m_grid->column(col).left, 0, self_size.new_width(cell_width), fmt_ctx, true);
                    cell->el->pos().width = cell_width - cell->el->content_offset_left() -
							cell->el->content_offset_right();
                }
                /*else
                {
                    cell->el->pos().x = m_grid->column(col).left + cell->el->content_offset_left();
                }*/

                if (cell->rowspan <= 1)
                {
                    m_grid->row(row).height = std::max(m_grid->row(row).height, cell->el->height());
                }
                else
                {
                    row_span_found = true;
                }

            }
        }
    }

    if (row_span_found)
    {
        for (int col = 0; col < m_grid->cols_count(); col++)
        {
            for (int row = 0; row < m_grid->rows_count(); row++)
            {
                table_cell* cell = m_grid->cell(col, row);
                if (cell->el)
                {
                    int span_row = row + cell->rowspan - 1;
                    if (span_row >= m_grid->rows_count())
                    {
                        span_row = m_grid->rows_count() - 1;
                    }
                    if (span_row != row)
                    {
                        int h = 0;
                        for (int i = row; i <= span_row; i++)
                        {
                            h += m_grid->row(i).height;
                        }
                        if (h < cell->el->height())
                        {
                            m_grid->row(span_row).height += cell->el->height() - h;
                        }
                    }
                }
            }
        }
    }

    // Calculate vertical table spacing
    int table_height_spacing = 0;
    if (src_el()->css().get_border_collapse() == border_collapse_separate)
    {
        table_height_spacing = m_border_spacing_y * (m_grid->rows_count() + 1);
    }
    else
    {
        table_height_spacing = 0;

        if (m_grid->rows_count())
        {
            table_height_spacing -= std::min(border_top(), m_grid->row(0).border_top);
            table_height_spacing -= std::min(border_bottom(), m_grid->row(m_grid->rows_count() - 1).border_bottom);
        }

        for (int row = 1; row < m_grid->rows_count(); row++)
        {
            table_height_spacing -= std::min(m_grid->row(row).border_top, m_grid->row(row - 1).border_bottom);
        }
    }


    // calculate block height
    int block_height = 0;
	if(self_size.height.type != containing_block_context::cbc_value_type_auto && self_size.height > 0)
	{
		block_height = self_size.height - (m_padding.height() + m_borders.height());
	}

    // calculate minimum height from m_css.get_min_height()
    int min_height = 0;
    if (!src_el()->css().get_min_height().is_predefined() && src_el()->css().get_min_height().units() == css_units_percentage)
    {
		min_height = src_el()->css().get_min_height().calc_percent(containing_block_size.height);
    }
    else
    {
        min_height = (int)src_el()->css().get_min_height().val();
    }

    int minimum_table_height = std::max(block_height, min_height);

    m_grid->calc_rows_height(minimum_table_height - table_height_spacing, m_border_spacing_y);
    m_grid->calc_vertical_positions(m_borders, src_el()->css().get_border_collapse(), m_border_spacing_y);

    int table_height = 0;

    // place cells vertically
    for (int col = 0; col < m_grid->cols_count(); col++)
    {
        for (int row = 0; row < m_grid->rows_count(); row++)
        {
            table_cell* cell = m_grid->cell(col, row);
            if (cell->el)
            {
                int span_row = row + cell->rowspan - 1;
                if (span_row >= m_grid->rows_count())
                {
                    span_row = m_grid->rows_count() - 1;
                }
                cell->el->pos().y = m_grid->row(row).top + cell->el->content_offset_top();
                cell->el->pos().height = m_grid->row(span_row).bottom - m_grid->row(row).top -
						cell->el->content_offset_top() -
										 cell->el->content_offset_bottom();
                table_height = std::max(table_height, m_grid->row(span_row).bottom);
                cell->el->apply_vertical_align();
            }
        }
    }

    if (src_el()->css().get_border_collapse() == border_collapse_collapse)
    {
        if (m_grid->rows_count())
        {
            table_height -= std::min(border_bottom(), m_grid->row(m_grid->rows_count() - 1).border_bottom);
        }
    }
    else
    {
        table_height += m_border_spacing_y;
    }

    // Render table captions
    // Table border doesn't round the caption, so we have to start caption in the border position
    int top_captions = -border_top();

    for (auto& caption : m_grid->captions())
    {
		if(caption->css().get_caption_side() == caption_side_top)
		{
			caption->render(-border_left(), top_captions, self_size.new_width(table_width + border_left() + border_right()), fmt_ctx);
			top_captions += caption->height();
		}
    }

    if (top_captions)
    {
        // Add border height to get the top of cells
        top_captions += border_top();

        // Save caption height for draw_background
		m_grid->top_captions_height(top_captions);

        // Move table cells to the bottom side
        for (int row = 0; row < m_grid->rows_count(); row++)
        {
            m_grid->row(row).el_row->pos().y += top_captions;
            for (int col = 0; col < m_grid->cols_count(); col++)
            {
                table_cell* cell = m_grid->cell(col, row);
                if (cell->el)
                {
                    cell->el->pos().y += top_captions;
                }
            }
        }
    }

	int bottom_captions = 0;

	for (auto& caption : m_grid->captions())
	{
		if(caption->css().get_caption_side() == caption_side_bottom)
		{
			caption->render(-border_left(), table_height + top_captions + bottom_captions, self_size.new_width(table_width + border_left() + border_right()), fmt_ctx);
			bottom_captions += caption->height();
		}
	}

	m_pos.move_to(x + content_offset_left(), y + content_offset_top());
	m_pos.width = table_width;
	m_pos.height = table_height + top_captions + bottom_captions;

	if(self_size.width.type != containing_block_context::cbc_value_type_absolute)
	{
		return std::min(table_width, max_table_width) + content_offset_width();
	}
	return table_width + content_offset_width();
}

std::shared_ptr<litehtml::render_item> litehtml::render_item_table::init()
{
    // Initialize Grid
    m_grid = std::unique_ptr<table_grid>(new table_grid());

    go_inside_table 		table_selector;
    table_rows_selector		row_selector;
    table_cells_selector	cell_selector;

    elements_iterator row_iter(false, &table_selector, &row_selector);

    row_iter.process(shared_from_this(), [&](std::shared_ptr<render_item>& el, iterator_item_type /*item_type*/)
        {
            m_grid->begin_row(el);


            elements_iterator cell_iter(true, &table_selector, &cell_selector);
            cell_iter.process(el, [&](std::shared_ptr<render_item>& el, iterator_item_type item_type)
                {
					if(item_type != iterator_item_type_end_parent)
					{
						el = el->init();
						m_grid->add_cell(el);
					}
                });
        });

    for (auto& el : m_children)
    {
        if (el->src_el()->css().get_display() == display_table_caption)
        {
            el = el->init();
            m_grid->captions().push_back(el);
        }
    }

    m_grid->finish();

	if(src_el()->css().get_border_collapse() == border_collapse_separate)
	{
		int font_size = src_el()->css().get_font_size();
		document::ptr doc = src_el()->get_document();
		m_border_spacing_x = doc->to_pixels(src_el()->css().get_border_spacing_x(), font_size);
		m_border_spacing_y = doc->to_pixels(src_el()->css().get_border_spacing_y(), font_size);
	} else
	{
		m_border_spacing_x	= 0;
		m_border_spacing_y	= 0;
	}

    src_el()->add_render(shared_from_this());

    return shared_from_this();
}

void litehtml::render_item_table::draw_children(uint_ptr hdc, int x, int y, const position* clip, draw_flag flag, int zindex)
{
    if (!m_grid) return;

    position pos = m_pos;
    pos.x += x;
    pos.y += y;
    for (auto& caption : m_grid->captions())
    {
        if (flag == draw_block)
        {
            caption->src_el()->draw(hdc, pos.x, pos.y, clip, caption);
        }
        caption->draw_children(hdc, pos.x, pos.y, clip, flag, zindex);
    }
    for (int row = 0; row < m_grid->rows_count(); row++)
    {
        if (flag == draw_block)
        {
            m_grid->row(row).el_row->src_el()->draw_background(hdc, pos.x, pos.y, clip, m_grid->row(row).el_row);
        }
        for (int col = 0; col < m_grid->cols_count(); col++)
        {
            table_cell* cell = m_grid->cell(col, row);
            if (cell->el)
            {
                if (flag == draw_block)
                {
                    cell->el->src_el()->draw(hdc, pos.x, pos.y, clip, cell->el);
                }
                cell->el->draw_children(hdc, pos.x, pos.y, clip, flag, zindex);
            }
        }
    }
}

int litehtml::render_item_table::get_draw_vertical_offset()
{
    if(m_grid)
    {
        return m_grid->top_captions_height();
    }
    return 0;
}

void litehtml::render_item_table_row::get_inline_boxes( position::vector& boxes ) const
{
	position pos;
	for(auto& el : m_children)
	{
		if(el->src_el()->css().get_display() == display_table_cell)
		{
			pos.x		= el->left() + el->margin_left();
			pos.y		= el->top() - m_padding.top - m_borders.top;

			pos.width	= el->right() - pos.x - el->margin_right() - el->margin_left();
			pos.height	= el->height() + m_padding.top + m_padding.bottom + m_borders.top + m_borders.bottom;

			boxes.push_back(pos);
		}
	}
}
