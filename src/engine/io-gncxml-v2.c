#include "io-gncxml-v2.h"


struct _load_counter_struct
{
    int accounts_total;
    int accounts_loaded;

    int commodities_total;
    int commodities_loaded;

    int transactions_total;
    int transactions_loaded;

    int prices_total;
    int prices_loaded;
};

typedef struct _load_counter_struct load_counter;

static load_counter*
make_counter()
{
    load_counter *ret;

    ret = g_new0(load_counter, 1);

    return ret;
}

gboolean
gnc_book_load_from_xml_file_v2(GNCBook *book)
{
    sixtp_gdv2 gd;

    gd.book = book;
    gd.data = (gpointer)make_counter();
    
    /* gd.addAccountFunc = add_account_local; */
    
    
    return FALSE;
}


gboolean
gnc_book_write_to_xml_file_v2(GNCBook *book, const char *filename)
{
    return FALSE;
}


gboolean gnc_is_xml_data_file_v2(const gchar *name)
{
    return FALSE;
}

