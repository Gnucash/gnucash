#ifndef LH_LINE_BOX_H
#define LH_LINE_BOX_H

#include <vector>
#include <memory>
#include "os_types.h"
#include "types.h"

namespace litehtml
{
    class render_item;

    struct line_context
    {
        int calculatedTop;
        int top;
        int left;
        int right;

        int width() const
        {
            return right - left;
        }
        void fix_top()
        {
            calculatedTop = top;
        }
		line_context() : calculatedTop(0), top(0), left(0), right(0) {}
    };

	class line_box_item
	{
	public:
		enum element_type
		{
			type_text_part,
			type_inline_start,
			type_inline_continue,
			type_inline_end
		};
	protected:
		std::shared_ptr<render_item> m_element;
		int m_rendered_min_width;
	public:
		explicit line_box_item(const std::shared_ptr<render_item>& element) : m_element(element), m_rendered_min_width(0) {}
		line_box_item() : m_element(), m_rendered_min_width(0) {};
		line_box_item(const line_box_item& el) = default;
		line_box_item(line_box_item&&) = default;

		int height() const { return right() - left(); }
		const std::shared_ptr<render_item>& get_el() const { return m_element; }
		virtual position& pos();
		virtual void place_to(int x, int y);
		virtual int width() const;
		virtual int top() const;
		virtual int bottom() const;
		virtual int right() const;
		virtual int left() const;
		virtual element_type get_type() const	{ return type_text_part; }
		virtual int get_rendered_min_width() const	{ return m_rendered_min_width; }
		virtual void set_rendered_min_width(int min_width)	{ m_rendered_min_width = min_width; }
	};

	class lbi_start : public line_box_item
	{
	protected:
		position m_pos;
	public:
		explicit lbi_start(const std::shared_ptr<render_item>& element);

		void place_to(int x, int y) override;
		int width() const override;
		position& pos() override { return m_pos; }
		int top() const override;
		int bottom() const override;
		int right() const override;
		int left() const override;
		element_type get_type() const override	{ return type_inline_start; }
		int get_rendered_min_width() const override { return width(); }
	};

	class lbi_end : public lbi_start
	{
	public:
		explicit lbi_end(const std::shared_ptr<render_item>& element);

		void place_to(int x, int y) override;
		int right() const override;
		int left() const override;
		element_type get_type() const override	{ return type_inline_end; }
	};

	class lbi_continue : public lbi_start
	{
	public:
		explicit lbi_continue(const std::shared_ptr<render_item>& element);

		void place_to(int x, int y) override;
		int right() const override;
		int left() const override;
		int width() const override;
		element_type get_type() const override	{ return type_inline_continue; }
	};

	class line_box
    {
		struct va_context
		{
			int 			baseline;
			font_metrics 	fm;

			va_context() : baseline(0) {}
		};

        int		                m_top;
        int		                m_left;
        int		                m_right;
        int						m_height;
        int						m_width;
		int						m_line_height;
		int						m_default_line_height;
        font_metrics			m_font_metrics;
        int						m_baseline;
        text_align				m_text_align;
		int 					m_min_width;
		std::list< std::unique_ptr<line_box_item> > m_items;
    public:
        line_box(int top, int left, int right, int line_height, const font_metrics& fm, text_align align) :
				m_top(top),
				m_left(left),
				m_right(right),
				m_height(0),
				m_width(0),
				m_line_height(0),
				m_default_line_height(line_height),
				m_font_metrics(fm),
				m_baseline(0),
				m_text_align(align),
				m_min_width(0)
		{
        }

        int		bottom() const	{ return m_top + height();	}
        int		top() const		{ return m_top;				}
        int		right() const	{ return m_left + width();	}
        int		left() const	{ return m_left;			}
        int		height() const  { return m_height;				}
        int	 	width() const	{ return m_width;				}
		int	 	line_right() const	{ return m_right;			}
		int	 	min_width() const	{ return m_min_width;		}

        void				add_item(std::unique_ptr<line_box_item> item);
        bool				can_hold(const std::unique_ptr<line_box_item>& item, white_space ws) const;
        bool				is_empty() const;
        int					baseline() const;
        int					top_margin() const;
        int					bottom_margin() const;
        void				y_shift(int shift);
		std::list< std::unique_ptr<line_box_item> >	finish(bool last_box, const containing_block_context &containing_block_size);
		std::list< std::unique_ptr<line_box_item> > new_width(int left, int right);
		std::shared_ptr<render_item> 		get_last_text_part() const;
		std::shared_ptr<render_item> 		get_first_text_part() const;
		std::list< std::unique_ptr<line_box_item> >& 	items() { return m_items; }
	private:
        bool				have_last_space() const;
        bool				is_break_only() const;
		static int			calc_va_baseline(const va_context& current, vertical_align va, const font_metrics& new_font, int top, int bottom);
    };
}

#endif //LH_LINE_BOX_H
