/********************************************************************\
 * gnc-tree-container.hpp
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef GNC_LIST_MODEL_CONTAINER_HPP
#define GNC_LIST_MODEL_CONTAINER_HPP

#include <string>
#include <optional>
#include <algorithm>
#include <memory>

class GncListModelData
{
public:
    GncListModelData (GtkTreeModel* model, const GtkTreeIter& iter) : m_model{model}, m_iter{iter} {};

    template <typename T>
    T get_column (int column)
    {
        gpointer rv;
        gtk_tree_model_get(m_model, &m_iter, column, &rv, -1);
        return static_cast<T>(rv);
    }

    GtkTreeIter& get_iter () { return m_iter; };

    int get_column_int (int column)
    {
        int rv;
        gtk_tree_model_get(m_model, &m_iter, column, &rv, -1);
        return rv;
    }

    std::string get_column_string (int column)
    {
        auto str = get_column<char*>(column);
        std::string rv{str};
        g_free (str);
        return rv;
    }

    void set_columns (int unused, ...)
    {
        va_list var_args;
        va_start (var_args, unused);
        gtk_list_store_set_valist (GTK_LIST_STORE(m_model), &get_iter(), var_args);
        va_end (var_args);
    }

    template <typename T>
    void set_column (int column, T data) { gtk_list_store_set(GTK_LIST_STORE(m_model), &get_iter(), column, data, -1); }

    // overloads the template when data is a std::string
    void set_column (int column, const std::string& str) { set_column (column, str.c_str()); }

    bool operator==(const GncListModelData& other) const
    {
        return (m_model == other.m_model) &&
            (m_iter.stamp == other.m_iter.stamp) &&
            (m_iter.user_data == other.m_iter.user_data) &&
            (m_iter.user_data2 == other.m_iter.user_data2) &&
            (m_iter.user_data3 == other.m_iter.user_data3);
    }

private:
    GtkTreeModel* m_model;
    GtkTreeIter m_iter;
};

// Custom container class
template <typename ModelType = GncListModelData>
class GncListModelContainer
{
public:

    // Custom iterator class
    class GncListModelIter
    {
    public:
        /* Set iterator traits queried by STL algorithms. These are
           required for std::find_if etc to iterate through the
           container. */
        using iterator_category = std::forward_iterator_tag;
        using value_type = ModelType;
        using difference_type = std::ptrdiff_t;
        using pointer = ModelType*;
        using reference = ModelType&;

        GncListModelIter(GtkTreeModel* model, std::optional<GtkTreeIter> iter) : m_model(model), m_iter(iter) {}

        GncListModelIter(GtkTreeModel* model) : m_model (model)
        {
            GtkTreeIter iter;
            m_iter = gtk_tree_model_get_iter_first(m_model, &iter) ? std::make_optional(iter) : std::nullopt;
        }

        GncListModelIter& operator++()
        {
            if (!m_iter.has_value())
                throw "no value, cannot increment";
            if (!gtk_tree_model_iter_next(m_model, &m_iter.value()))
                m_iter = std::nullopt;
            return *this;
        }

        ModelType operator*() const
        {
            if (!m_iter.has_value())
                throw "no value, cannot dereference";
            return ModelType (m_model, *m_iter);
        }

        std::unique_ptr<ModelType> operator->()
        {
            if (!m_iter.has_value())
                throw "no value, cannot dereference";
            return std::make_unique<ModelType> (m_model, *m_iter);
        }

        bool has_value() const { return m_iter.has_value(); };

        bool operator==(const GncListModelIter& other) const
        {
            if (m_model != other.m_model)
                return false;
            if (!m_iter.has_value() && !other.m_iter.has_value())
                return true;
            if (!m_iter.has_value() || !other.m_iter.has_value())
                return false;
            return (ModelType (m_model, *m_iter) == ModelType (m_model, *other.m_iter));
        }

        bool operator!=(const GncListModelIter& other) const { return !(*this == other); }

    private:
        GtkTreeModel* m_model;
        std::optional<GtkTreeIter> m_iter;
    };

    GncListModelContainer(GtkTreeModel* model) : m_model(model)
    {
        g_return_if_fail (GTK_IS_TREE_MODEL (m_model));
        g_object_ref (m_model);
    }

    ~GncListModelContainer () { g_object_unref (m_model); }

    GncListModelIter begin() const { return GncListModelIter(m_model); };

    GncListModelIter end() const { return GncListModelIter(m_model, std::nullopt); };

    GncListModelIter append()
    {
        GtkTreeIter iter;
        gtk_list_store_append (GTK_LIST_STORE(m_model), &iter);
        return GncListModelIter(m_model, iter);
    };

    size_t size() const { return std::distance (begin(), end()); }

    bool empty() const { return begin() == end(); };

    void clear() { gtk_list_store_clear (GTK_LIST_STORE (m_model)); };

private:
    GtkTreeModel* m_model;
};

#endif
