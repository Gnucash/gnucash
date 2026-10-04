#include <memory>
#include <functional>
#include "gnc-plugin.h"

class GncFilterInvoicesDialog {
    class FilterDialog;

    std::unique_ptr<FilterDialog> filter; 
    GncPluginPage                 *plugin_page = nullptr;
    GncOwnerType                  owner_type;

    using Callback = std::function<void()>;

public:
    GncFilterInvoicesDialog (GncPluginPage &page, GncOwnerType &owner,
                            Callback apply_cb);

    ~GncFilterInvoicesDialog ();

    Callback apply_filter = [](){
        g_warn_if_fail (true);
    };

    void create_dialog ();

    QofQuery *make_filter ();

    void
    free_filter (QofQuery *query)
    {
        qof_query_destroy (query);
    }
};
