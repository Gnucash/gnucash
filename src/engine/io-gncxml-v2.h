/*
 * io-gncxml.h -- api for new XML-based file format
 *
 * Initial code by James LewisMoss
 *
 * Copyright (c) 2001 Gnumatic Incorporated
 *
 */
#ifndef __IO_GNCXML_V2_H__
#define __IO_GNCXML_V2_H__

#include <glib.h>

#include "gnc-book.h"
#include "Account.h"
#include "Transaction.h"
#include "gnc-commodity.h"
#include "gnc-pricedb.h"

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

struct sixtp_global_data_v2_struct
{
    GNCBook *book;
    gpointer data;

    int value;
    char *tag;

    load_counter counter;
    
    gboolean (*addAccountFunc)(struct sixtp_global_data_v2_struct *data,
                               Account *act);
    gboolean (*addCommodityFunc)(struct sixtp_global_data_v2_struct *data,
                                 gnc_commodity *com);
    gboolean (*addTransactionFunc)(struct sixtp_global_data_v2_struct *data,
                                   Transaction *act);
    gboolean (*addPriceDBFunc)(struct sixtp_global_data_v2_struct *data,
                               GNCPriceDB *prc);
};

typedef struct sixtp_global_data_v2_struct sixtp_gdv2;

/* read in an account group from a file */
gboolean gnc_book_load_from_xml_file_v2(GNCBook *book);

/* write all account info to a file */
gboolean gnc_book_write_to_xml_file_v2(GNCBook *book, const char *filename);

/* The is_gncxml_file() routine checks to see if the first few 
 * chars of the file look like gnc-xml data.
 */
gboolean gnc_is_xml_data_file_v2(const gchar *name);



#endif /* __IO_GNCXML_V2_H__ */
