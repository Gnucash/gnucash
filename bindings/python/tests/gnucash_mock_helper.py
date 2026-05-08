import unittest.mock
import sys
import os
import types
from functools import wraps

def setup_gnucash_mocks():
    # Root gnucash module
    gnucash = types.ModuleType('gnucash')
    sys.modules['gnucash'] = gnucash
    gnucash.__path__ = []

    # C extension mocks
    class MockCoreC(types.ModuleType):
        def __init__(self, name):
            super().__init__(name)
            # Essential constants
            self.SESSION_NORMAL_OPEN = 0
            self.SESSION_NEW_STORE = 1
            self.SESSION_NEW_OVERWRITE = 2
            self.SESSION_READ_ONLY = 3
            self.SESSION_BREAK_LOCK = 4
            self.GNC_DENOM_AUTO = 0
            self.GNC_HOW_RND_ROUND = 6
            self.ERR_BACKEND_NO_ERR = 0
            self.GETTEXT_PACKAGE = "gnucash"
            self.GNC_HOW_DENOM_FIXED = 1
            self.GNC_HOW_RND_NEVER = 0

            # Manually add essential names that might be missed or are prefix-discovered
            # We need to add enough names for ALL classes in gnucash_core.py
            manual_prefix_names = [
                # GncPriceDB
                'gnc_pricedb_lookup_latest', 'gnc_pricedb_lookup_nearest_in_time64',
                'gnc_pricedb_lookup_nearest_before_t64', 'gnc_pricedb_convert_balance_latest_price',
                'gnc_pricedb_convert_balance_nearest_price_t64', 'gnc_pricedb_get_prices',
                # Session
                'qof_session_new', 'qof_session_get_book', 'qof_session_load', 'qof_session_save',
                'qof_session_begin', 'qof_session_end', 'qof_session_get_error', 'qof_session_pop_error',
                # GncNumeric
                'gnc_numeric_create', 'gnc_numeric_zero', 'gnc_numeric_same', 'gnc_numeric_add',
                'gnc_numeric_sub', 'gnc_numeric_mul', 'gnc_numeric_div', 'gnc_numeric_neg',
                'gnc_numeric_abs', 'gnc_numeric_add_fixed', 'gnc_numeric_sub_fixed',
                'gnc_numeric_convert', 'gnc_numeric_reduce', 'gnc_numeric_invert',
                # GncCommodity & Table & Namespace
                'gnc_commodity_new', 'gnc_commodity_clone',
                'gnc_commodity_table_get_table', 'gnc_pricedb_get_db',
                'gnc_commodity_table_lookup', 'gnc_commodity_table_lookup_unique', 'gnc_commodity_table_find_full',
                'gnc_commodity_table_insert', 'gnc_commodity_table_add_namespace', 'gnc_commodity_table_find_namespace',
                'gnc_commodity_table_get_namespaces_list', 'gnc_commodity_table_get_commodities', 'gnc_commodity_table_get_quotable_commodities',
                'gnc_commodity_namespace_get_commodity_list',
                # GncLot
                'gnc_lot_new', 'gnc_lot_get_account', 'gnc_lot_get_book', 'gnc_lot_get_earliest_split', 'gnc_lot_get_latest_split', 'gnc_lot_get_balance', 'gnc_lot_lookup', 'gnc_lot_make_default',
                # Transaction
                'xaccTransGetSplit', 'xaccTransFindSplitByAccount', 'xaccTransClone', 'xaccTransReverse', 'xaccTransGetReversedBy', 'xaccTransGetImbalanceValue', 'xaccTransGetAccountValue', 'xaccTransGetAccountAmount', 'xaccTransGetAccountConvRate', 'xaccTransGetAccountBalance', 'xaccTransGetCurrency', 'gncTransGetGUID', 'xaccTransGetSplitList', 'xaccTransGetImbalance',
                # Split
                'xaccSplitGetBook', 'xaccSplitGetAccount', 'xaccSplitGetParent', 'xaccSplitLookup', 'xaccSplitGetOtherSplit', 'xaccSplitGetAmount', 'xaccSplitGetValue', 'xaccSplitGetSharePrice', 'xaccSplitConvertAmount', 'xaccSplitGetBaseValue', 'xaccSplitGetBalance', 'xaccSplitGetClearedBalance', 'xaccSplitGetReconciledBalance', 'xaccSplitVoidFormerAmount', 'xaccSplitVoidFormerValue', 'gncSplitGetGUID', 'xaccSplitSetAccount', 'xaccSplitSetParent',
                # Account
                'xaccAccountget_book', 'xaccAccountLookup', 'xaccAccountget_parent', 'xaccAccountget_root', 'xaccAccountnth_child', 'xaccAccountlookup_by_code', 'xaccAccountlookup_by_name', 'xaccAccountlookup_by_full_name', 'xaccAccountFindTransByDesc', 'xaccAccountFindSplitByDesc', 'xaccAccountGetBalance', 'xaccAccountGetClearedBalance', 'xaccAccountGetReconciledBalance', 'xaccAccountGetPresentBalance', 'xaccAccountGetProjectedMinimumBalance', 'xaccAccountGetBalanceAsOfDate', 'xaccAccountConvertBalanceToCurrency', 'xaccAccountConvertBalanceToCurrencyAsOfDate', 'xaccAccountGetBalanceInCurrency', 'xaccAccountGetClearedBalanceInCurrency', 'xaccAccountGetReconciledBalanceInCurrency', 'xaccAccountGetPresentBalanceInCurrency', 'xaccAccountGetProjectedMinimumBalanceInCurrency', 'xaccAccountGetBalanceAsOfDateInCurrency', 'xaccAccountGetBalanceChangeForPeriod', 'xaccAccountGetCommodity', 'gncAccountGetGUID', 'xaccAccountGetSplitList', 'xaccAccountget_children', 'xaccAccountget_children_sorted', 'xaccAccountget_descendants', 'xaccAccountget_descendants_sorted', 'xaccAccountGetName', 'xaccAccountSetName',
                # Book
                'qof_book_new', 'gnc_book_get_root_account', 'gnc_book_set_root_account', 'qof_book_increment_and_format_counter',
                # GUID
                'guid_new_return', 'guid_copy', 'guid_to_string',
                # Query
                'qof_query_create', 'qof_query_run', 'qof_query_destroy',
                # Imported names
                'gncInvoiceLookup', 'gncInvoiceGetInvoiceFromTxn', 'gncInvoiceGetInvoiceFromLot',
                'gncEntryLookup', 'gncCustomerLookup', 'gncVendorLookup', 'gncJobLookup',
                'gncEmployeeLookup', 'gncTaxTableLookup', 'gncTaxTableLookupByName',
                'gnc_search_invoice_on_id', 'gnc_search_customer_on_id', 'gnc_search_bill_on_id',
                'gnc_search_vendor_on_id', 'gncInvoiceNextID', 'gncCustomerNextID',
                'gncVendorNextID', 'gncTaxTableGetTables', 'double_to_gnc_numeric', 'gnc_numeric_from_string',
                'gnc_numeric_to_string', 'gnc_numeric_check'
            ]
            for name in manual_prefix_names:
                setattr(self, name, unittest.mock.MagicMock())

            self.qof_session_get_error.return_value = 0

        def __getattr__(self, name):
            return unittest.mock.MagicMock()

    gnucash_core_c = MockCoreC('gnucash.gnucash_core_c')
    sys.modules['gnucash.gnucash_core_c'] = gnucash_core_c

    sys.modules['gnucash._sw_core_utils'] = unittest.mock.MagicMock()
    sys.modules['gnucash._sw_app_utils'] = unittest.mock.MagicMock()

    # Mock deprecation properly
    deprecation = types.ModuleType('gnucash.deprecation')
    def dummy_decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            return func(*args, **kwargs)
        return wrapper
    def deprecated_with_msg(msg):
        return dummy_decorator

    deprecation.deprecated_args_session_init = dummy_decorator
    deprecation.deprecated_args_session_begin = dummy_decorator
    deprecation.deprecated_args_session = unittest.mock.MagicMock()
    deprecation.deprecated = deprecated_with_msg
    sys.modules['gnucash.deprecation'] = deprecation

    sys.modules['gnucash.app_utils'] = unittest.mock.MagicMock()

    # For gnucash.function_class and gnucash.gnucash_core, we want to use the real files
    bindings_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
    if bindings_path not in sys.path:
        sys.path.insert(0, bindings_path)

    import function_class
    sys.modules['gnucash.function_class'] = function_class

    return gnucash_core_c
