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

#ifndef GNC_TREE_CONTAINER_HPP
#define GNC_TREE_CONTAINER_HPP

#include <string>
#include <optional>

class GncTreeData
{
public:
    GncTreeData (GtkTreeModel* model, GtkTreeIter iter) : m_model{model}, m_iter{iter} {};

    template <typename T>
    T get (int column)
    {
        gpointer rv;
        gtk_tree_model_get(m_model, &m_iter, column, &rv, -1);
        return static_cast<T>(rv);
    }

    GtkTreeIter& get_iter () { return m_iter; };

    int get_int (int column)
    {
        int rv;
        gtk_tree_model_get(m_model, &m_iter, column, &rv, -1);
        return rv;
    }

    std::string get_string (int column)
    {
        auto str = get<char*>(column);
        std::string rv{str};
        g_free (str);
        return rv;
    }

    bool operator==(const GncTreeData& other) const
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

// Custom iterator class
template <typename ModelType>
class GncTreeIter
{
public:
    GncTreeIter(GtkTreeModel* model, std::optional<GtkTreeIter> iter) : m_model(model), m_iter(iter) {}

    GncTreeIter(GtkTreeModel* model) : m_model (model)
    {
        GtkTreeIter iter;
        gtk_tree_model_get_iter_first(m_model, &iter);
        m_iter = iter;
    }

    GncTreeIter& operator++()
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

    bool has_value() { return m_iter.has_value(); };

    bool operator==(const GncTreeIter& other) const
    {
        if (!m_iter.has_value() && !other.m_iter.has_value())
            return true;
        if (!m_iter.has_value() || !other.m_iter.has_value())
            return false;
        return (ModelType (m_model, *m_iter) == ModelType (m_model, *other.m_iter));
    }

    bool operator!=(const GncTreeIter& other) const { return !(*this == other); }

private:
    GtkTreeModel* m_model;
    std::optional<GtkTreeIter> m_iter;
};

// Custom container class
template <typename ModelType = GncTreeData>
class GncTreeContainer
{
public:
    using TreeModelIterator = GncTreeIter<ModelType>;

    GncTreeContainer(GtkTreeModel* model) : m_model(model)
    {
        g_return_if_fail (GTK_IS_TREE_MODEL (m_model));
        g_object_ref (m_model);
    }

    ~GncTreeContainer () { g_object_unref (m_model); }

    TreeModelIterator begin() { return TreeModelIterator(m_model); };

    TreeModelIterator end() { return TreeModelIterator(m_model, std::nullopt); };

    bool empty() { return begin() == end(); };

private:
    GtkTreeModel* m_model;
};

#endif
