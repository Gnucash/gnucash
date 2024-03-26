#include "html.h"
#include "line_box.h"
#include "element.h"
#include "render_item.h"
#include <algorithm>

//////////////////////////////////////////////////////////////////////////////////////////

void litehtml::line_box_item::place_to(int x, int y)
{
	m_element->pos().x = x + m_element->content_offset_left();
	m_element->pos().y = y + m_element->content_offset_top();
}

litehtml::position& litehtml::line_box_item::pos()
{
	return m_element->pos();
}


int litehtml::line_box_item::width() const
{
	return m_element->width();
}

int litehtml::line_box_item::top() const
{
	return m_element->top();
}

int litehtml::line_box_item::bottom() const
{
	return m_element->bottom();
}

int litehtml::line_box_item::right() const
{
	return m_element->right();
}

int litehtml::line_box_item::left() const
{
	return m_element->left();
}

//////////////////////////////////////////////////////////////////////////////////////////

litehtml::lbi_start::lbi_start(const std::shared_ptr<render_item>& element) : line_box_item(element)
{
	m_pos.height = m_element->src_el()->css().get_font_metrics().height;
	m_pos.width = m_element->content_offset_left();
}

void litehtml::lbi_start::place_to(int x, int y)
{
	m_pos.x = x + m_element->content_offset_left();
	m_pos.y = y;
}

int litehtml::lbi_start::width() const
{
	return m_pos.width;
}

int litehtml::lbi_start::top() const
{
	return m_pos.y;
}

int litehtml::lbi_start::bottom() const
{
	return m_pos.y + m_pos.height;
}

int litehtml::lbi_start::right() const
{
	return m_pos.x;
}

int litehtml::lbi_start::left() const
{
	return m_pos.x - m_element->content_offset_left();
}

//////////////////////////////////////////////////////////////////////////////////////////

litehtml::lbi_end::lbi_end(const std::shared_ptr<render_item>& element) : lbi_start(element)
{
	m_pos.height = m_element->src_el()->css().get_font_metrics().height;
	m_pos.width = m_element->content_offset_right();
}

void litehtml::lbi_end::place_to(int x, int y)
{
	m_pos.x = x;
	m_pos.y = y;
}

int litehtml::lbi_end::right() const
{
	return m_pos.x + m_pos.width;
}

int litehtml::lbi_end::left() const
{
	return m_pos.x;
}

//////////////////////////////////////////////////////////////////////////////////////////

litehtml::lbi_continue::lbi_continue(const std::shared_ptr<render_item>& element) : lbi_start(element)
{
	m_pos.height = m_element->src_el()->css().get_font_metrics().height;
	m_pos.width = 0;
}

void litehtml::lbi_continue::place_to(int x, int y)
{
	m_pos.x = x;
	m_pos.y = y;
}

int litehtml::lbi_continue::right() const
{
	return m_pos.x;
}

int litehtml::lbi_continue::left() const
{
	return m_pos.x;
}

int litehtml::lbi_continue::width() const
{
	return 0;
}

//////////////////////////////////////////////////////////////////////////////////////////

void litehtml::line_box::add_item(std::unique_ptr<line_box_item> item)
{
    item->get_el()->skip(false);
    bool add	= true;
	switch (item->get_type())
	{
		case line_box_item::type_text_part:
			if(item->get_el()->src_el()->is_white_space())
			{
				add = !is_empty() && !have_last_space();
			}
			break;
		case line_box_item::type_inline_start:
		case line_box_item::type_inline_end:
		case line_box_item::type_inline_continue:
			add = true;
			break;
	}
	if(add)
	{
		item->place_to(m_left + m_width, m_top);
		m_width += item->width();
		m_height = std::max(m_height, item->get_el()->height());
		m_items.emplace_back(std::move(item));
	} else
	{
		item->get_el()->skip(true);
	}
}

int litehtml::line_box::calc_va_baseline(const va_context& current, vertical_align va, const font_metrics& new_font, int top, int bottom)
{
	switch(va)
	{
		case va_super:
			return current.baseline - current.fm.height / 3;
		case va_sub:
			return current.baseline + current.fm.height / 3;
		case va_middle:
			return current.baseline - current.fm.x_height / 2;
		case va_text_top:
			return current.baseline - (current.fm.height - current.fm.base_line()) +
										new_font.height - new_font.base_line();
		case va_text_bottom:
			return current.baseline + current.fm.base_line() - new_font.base_line();
		case va_top:
			return top + new_font.height - new_font.base_line();
		case va_bottom:
			return bottom - new_font.height + new_font.base_line();
		default:
			return current.baseline;
	}
}

std::list< std::unique_ptr<litehtml::line_box_item> > litehtml::line_box::finish(bool last_box, const containing_block_context &containing_block_size)
{
	std::list< std::unique_ptr<line_box_item> > ret_items;

	if(!last_box)
	{
		while(!m_items.empty())
		{
			if (m_items.back()->get_type() == line_box_item::type_text_part)
			{
				// remove trailing spaces
				if (m_items.back()->get_el()->src_el()->is_break() ||
					m_items.back()->get_el()->src_el()->is_white_space())
				{
					m_width -= m_items.back()->width();
					m_items.back()->get_el()->skip(true);
					m_items.pop_back();
				} else
				{
					break;
				}
			} else if (m_items.back()->get_type() == line_box_item::type_inline_start)
			{
				// remove trailing empty inline_start markers
				// these markers will be added at the beginning of the next line box
				m_width -= m_items.back()->width();
				ret_items.emplace_back(std::move(m_items.back()));
				m_items.pop_back();
			} else
			{
				break;
			}
		}
	} else
	{
		// remove trailing spaces
		auto iter = m_items.rbegin();
		while(iter != m_items.rend())
		{
			if ((*iter)->get_type() == line_box_item::type_text_part)
			{
				if((*iter)->get_el()->src_el()->is_white_space())
				{
					(*iter)->get_el()->skip(true);
					m_width -= (*iter)->width();
					// Space can be between text and inline_end marker
					// We have to shift all items on the right side
					if(iter != m_items.rbegin())
					{
						auto r_iter = iter;
						r_iter--;
						while (true)
						{
							(*r_iter)->pos().x -= (*iter)->width();
							if (r_iter == m_items.rbegin())
							{
								break;
							}
							r_iter--;
						}
					}
					// erase white space element
					iter = decltype(iter) (m_items.erase( std::next(iter).base() ));
				} else
				{
					break;
				}
			} else
			{
				iter++;
			}
		}
	}

    if( is_empty() || (!is_empty() && last_box && is_break_only()) )
    {
        m_height = m_default_line_height;
		m_baseline = m_font_metrics.base_line();
        return ret_items;
    }

    int spc_x = 0;

    int add_x = 0;
    switch(m_text_align)
    {
        case text_align_right:
            if(m_width < (m_right - m_left))
            {
                add_x = (m_right - m_left) - m_width;
            }
            break;
        case text_align_center:
            if(m_width < (m_right - m_left))
            {
                add_x = ((m_right - m_left) - m_width) / 2;
            }
            break;
        case text_align_justify:
            if (m_width < (m_right - m_left))
            {
                add_x = 0;
                spc_x = (m_right - m_left) - m_width;
                if (spc_x > m_width/4)
                    spc_x = 0;
            }
            break;
        default:
            add_x = 0;
    }

    int counter = 0;
    float offj  = float(spc_x) / std::max(1.f, float(m_items.size())-1.f);
    float cixx  = 0.0f;

    int line_top	= 0;
    int line_bottom	= 0;

	va_context current_context;
	std::list<va_context> contexts;

	current_context.baseline = 0;
	current_context.fm = m_font_metrics;

	m_min_width = 0;

    for (const auto& lbi : m_items)
    {
		m_min_width += lbi->get_rendered_min_width();
		{ // start text_align_justify
			if (spc_x && counter)
			{
				cixx += offj;
				if ((counter + 1) == int(m_items.size()))
					cixx += 0.99f;
				lbi->pos().x += int(cixx);
			}
			counter++;
			if ((m_text_align == text_align_right || spc_x) && counter == int(m_items.size()))
			{
				// Forcible justify the last element to the right side for text align right and justify;
				lbi->pos().x = m_right - lbi->pos().width;
			} else if (add_x)
			{
				lbi->pos().x += add_x;
			}
		} // end text_align_justify

		if (lbi->get_type() == line_box_item::type_inline_start || lbi->get_type() == line_box_item::type_inline_continue)
		{
			contexts.push_back(current_context);
			current_context.baseline = calc_va_baseline(current_context,
														lbi->get_el()->css().get_vertical_align(),
														lbi->get_el()->css().get_font_metrics(),
														line_top, line_bottom);
			current_context.fm = lbi->get_el()->css().get_font_metrics();
		}

		// Align elements vertically by baseline.
        if(lbi->get_el()->src_el()->css().get_display() == display_inline_text || lbi->get_el()->src_el()->css().get_display() == display_inline)
        {
			// inline elements and text are aligned by baseline only
			// at this point the baseline for text is properly aligned already
			lbi->pos().y = current_context.baseline - lbi->get_el()->css().get_font_metrics().height + lbi->get_el()->css().get_font_metrics().base_line();
        } else
        {
            switch(lbi->get_el()->css().get_vertical_align())
            {
				case va_sub:
                case va_super:
					{
						int bl = calc_va_baseline(current_context, lbi->get_el()->css().get_vertical_align(), current_context.fm, line_top, line_bottom);
						lbi->pos().y = bl - lbi->get_el()->get_last_baseline() +
								lbi->get_el()->content_offset_top();
					}
					break;
				case va_bottom:
					lbi->pos().y = line_bottom - lbi->get_el()->height() + lbi->get_el()->content_offset_top();
					break;
				case va_top:
					lbi->pos().y = line_top + lbi->get_el()->content_offset_top();
					break;
                case va_baseline:
					lbi->pos().y = current_context.baseline - lbi->get_el()->get_last_baseline() +
							lbi->get_el()->content_offset_top();
                    break;
                case va_text_top:
					lbi->pos().y = current_context.baseline - current_context.fm.height + current_context.fm.base_line() +
							lbi->get_el()->content_offset_top();
                    break;
				case va_text_bottom:
					lbi->pos().y = current_context.baseline + current_context.fm.base_line() - lbi->get_el()->height() +
							lbi->get_el()->content_offset_top();
					break;
                case va_middle:
					lbi->pos().y = current_context.baseline - current_context.fm.x_height / 2 - lbi->get_el()->height() / 2 +
							lbi->get_el()->content_offset_top();
                    break;
            }
        }

		if (lbi->get_type() == line_box_item::type_inline_end)
		{
			if(!contexts.empty())
			{
				current_context = contexts.back();
				contexts.pop_back();
			}
		}

		// calculate line height
		line_top = std::min(line_top, lbi->top());
		line_bottom = std::max(line_bottom, lbi->bottom());

		if(lbi->get_el()->src_el()->css().get_display() == display_inline_text)
		{
			m_line_height = std::max(m_line_height, lbi->get_el()->css().get_line_height());
		}
    }

	m_height = line_bottom - line_top;
	int top_shift = line_top;
	if(m_height < m_line_height)
	{
		top_shift -= (m_line_height - m_height) / 2;
		m_height = m_line_height;
	}
	m_baseline = line_bottom;

	struct inline_item_box
	{
		std::shared_ptr<render_item> element;
		position box;

		inline_item_box() = default;
		explicit inline_item_box(const std::shared_ptr<render_item>& el) : element(el) {}
	};

	std::list<inline_item_box> inlines;

	contexts.clear();

	current_context.baseline = 0;
	current_context.fm = m_font_metrics;
	bool va_top_bottom = false;

    for (const auto& lbi : m_items)
    {
		// Calculate baseline. Now we calculate baseline for vertical alignment top and bottom
		if (lbi->get_type() == line_box_item::type_inline_start || lbi->get_type() == line_box_item::type_inline_continue)
		{
			contexts.push_back(current_context);
			va_top_bottom = lbi->get_el()->css().get_vertical_align() == va_bottom || lbi->get_el()->css().get_vertical_align() == va_top;
			current_context.baseline = calc_va_baseline(current_context,
														lbi->get_el()->css().get_vertical_align(),
														lbi->get_el()->css().get_font_metrics(),
														top_shift, top_shift + m_height);
			current_context.fm = lbi->get_el()->css().get_font_metrics();
		}

		// Align inlines and text by baseline if current vertical alignment is top or bottom
		if(va_top_bottom)
		{
			if (lbi->get_el()->src_el()->css().get_display() == display_inline_text ||
				lbi->get_el()->src_el()->css().get_display() == display_inline)
			{
				// inline elements and text are aligned by baseline only
				// at this point the baseline for text is properly aligned already
				lbi->pos().y = current_context.baseline - lbi->get_el()->css().get_font_metrics().height +
							   lbi->get_el()->css().get_font_metrics().base_line();
			}
		}

		// Pop the prev context
		if (lbi->get_type() == line_box_item::type_inline_end)
		{
			if(!contexts.empty())
			{
				current_context = contexts.back();
				contexts.pop_back();
			}
		}

		// move element to the correct position
		lbi->pos().y += m_top - top_shift;

		// Perform vertical align top and bottom for inline boxes
        if(lbi->get_el()->css().get_display() != display_inline_text && lbi->get_el()->css().get_display() != display_inline)
        {
            if(lbi->get_el()->css().get_vertical_align() == va_top)
			{
				lbi->pos().y = m_top + lbi->get_el()->content_offset_top();
			} else if(lbi->get_el()->css().get_vertical_align() == va_bottom)
			{
				lbi->pos().y = m_top + m_height - lbi->get_el()->height() + lbi->get_el()->content_offset_top();
			}
        }
        lbi->get_el()->apply_relative_shift(containing_block_size);

		// Calculate and push inline box into the render item element
		if(lbi->get_type() == line_box_item::type_inline_start || lbi->get_type() == line_box_item::type_inline_continue)
		{
			if(lbi->get_type() == line_box_item::type_inline_start)
			{
				lbi->get_el()->clear_inline_boxes();
			}
			inlines.emplace_back(lbi->get_el());
			inlines.back().box.x = lbi->left();
			inlines.back().box.y = lbi->top() - lbi->get_el()->content_offset_top();
			inlines.back().box.height = lbi->bottom() - lbi->top() + lbi->get_el()->content_offset_height();
		} else if(lbi->get_type() == line_box_item::type_inline_end)
		{
			if(!inlines.empty())
			{
				inlines.back().box.width = lbi->right() - inlines.back().box.x;
				inlines.back().element->add_inline_box(inlines.back().box);
				inlines.pop_back();
			}
		}
    }

	for(auto iter = inlines.rbegin(); iter != inlines.rend(); ++iter)
	{
		iter->box.width =  m_items.back()->right() - iter->box.x;
		iter->element->add_inline_box(iter->box);

		ret_items.emplace_front(std::unique_ptr<line_box_item>(new lbi_continue(iter->element)));
	}

	return ret_items;
}

std::shared_ptr<litehtml::render_item> litehtml::line_box::get_first_text_part() const
{
	for(const auto & item : m_items)
	{
		if(item->get_type() == line_box_item::type_text_part)
		{
			return item->get_el();
		}
	}
	return nullptr;
}


std::shared_ptr<litehtml::render_item> litehtml::line_box::get_last_text_part() const
{
	for(auto iter = m_items.rbegin(); iter != m_items.rend(); iter++)
	{
		if((*iter)->get_type() == line_box_item::type_text_part)
		{
			return (*iter)->get_el();
		}
	}
	return nullptr;
}


bool litehtml::line_box::can_hold(const std::unique_ptr<line_box_item>& item, white_space ws) const
{
    if(!item->get_el()->src_el()->is_inline()) return false;

	if(item->get_type() == line_box_item::type_text_part)
	{
		// force new line on floats clearing
		if (item->get_el()->src_el()->is_break() && item->get_el()->src_el()->css().get_clear() != clear_none)
		{
			return false;
		}

		auto last_el = get_last_text_part();

		// the first word is always can be hold
		if(!last_el)
		{
			return true;
		}

		// force new line if the last placed element was line break
		// Skip If there are the only break item - this is float clearing
		if (last_el && last_el->src_el()->is_break() && m_items.size() > 1)
		{
			return false;
		}

		// line break should stay in current line box
		if (item->get_el()->src_el()->is_break())
		{
			return true;
		}

		if (ws == white_space_nowrap || ws == white_space_pre ||
			(ws == white_space_pre_wrap && item->get_el()->src_el()->is_space()))
		{
			return true;
		}

		if (m_left + m_width + item->width() > m_right)
		{
			return false;
		}
	}

    return true;
}

bool litehtml::line_box::have_last_space()  const
{
	auto last_el = get_last_text_part();
	if(last_el)
	{
		return last_el->src_el()->is_white_space() || last_el->src_el()->is_break();
	}
	return false;
}

bool litehtml::line_box::is_empty() const
{
    if(m_items.empty()) return true;
	if(m_items.size() == 1 &&
		m_items.front()->get_el()->src_el()->is_break() &&
		m_items.front()->get_el()->src_el()->css().get_clear() != clear_none)
	{
		return true;
	}
    for (const auto& el : m_items)
    {
		if(el->get_type() == line_box_item::type_text_part)
		{
			if (!el->get_el()->skip() || el->get_el()->src_el()->is_break())
			{
				return false;
			}
		}
    }
    return true;
}

int litehtml::line_box::baseline() const
{
    return m_baseline;
}

int litehtml::line_box::top_margin() const
{
    return 0;
}

int litehtml::line_box::bottom_margin() const
{
    return 0;
}

void litehtml::line_box::y_shift( int shift )
{
	m_top += shift;
    for (auto& el : m_items)
    {
        el->pos().y += shift;
    }
}

bool litehtml::line_box::is_break_only() const
{
    if(m_items.empty()) return false;

	bool break_found = false;

	for (auto iter = m_items.rbegin(); iter != m_items.rend(); iter++)
	{
		if((*iter)->get_type() == line_box_item::type_text_part)
		{
			if((*iter)->get_el()->src_el()->is_break())
			{
				break_found = true;
			} else if(!(*iter)->get_el()->skip())
			{
				return false;
			}
		}
	}
	return break_found;
}

std::list< std::unique_ptr<litehtml::line_box_item> > litehtml::line_box::new_width( int left, int right)
{
	std::list< std::unique_ptr<line_box_item> > ret_items;
    int add = left - m_left;
    if(add)
    {
		m_left	= left;
		m_right	= right;
        m_width = 0;
        auto remove_begin = m_items.end();
		auto i = m_items.begin();
		i++;
		while (i != m_items.end())
        {
            if(!(*i)->get_el()->skip())
            {
                if(m_left + m_width + (*i)->width() > m_right)
                {
                    remove_begin = i;
                    break;
                } else
                {
					(*i)->pos().x += add;
                    m_width += (*i)->get_el()->width();
                }
            }
			i++;
        }
        if(remove_begin != m_items.end())
        {
			while(remove_begin != m_items.end())
			{
				ret_items.emplace_back(std::move(*remove_begin));
			}
            m_items.erase(remove_begin, m_items.end());
        }
    }
	return ret_items;
}

