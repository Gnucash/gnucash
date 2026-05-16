"""Tests for GncPrice / GncPriceDB / GncLot return-type wrapping.

All tests create data programmatically in in-memory sessions so they run
without any external data files or XML backend.
"""

import warnings
from datetime import datetime
from unittest import TestCase, main

from gnucash import (
    Account,
    Book,
    GncCommodity,
    GncNumeric,
    GncPrice,
    Session,
    Split,
    Transaction,
)
from gnucash.gnucash_core import GncCommodityNamespace, GncLot, GncPriceDB


# ---------------------------------------------------------------------------
# Helper: set up an in-memory book with a commodity and prices
# ---------------------------------------------------------------------------
class PriceSession(TestCase):
    """Base class that creates a session with a custom commodity and prices."""

    def setUp(self):
        self.ses = Session()
        self.book = self.ses.get_book()
        self.table = self.book.get_table()
        self.usd = self.table.lookup("CURRENCY", "USD")

        # Create a custom commodity
        self.test_comm = GncCommodity(
            self.book, "Test Stock", "NASDAQ", "TSTK", "TSTK", 10000
        )
        self.table.insert(self.test_comm)

        # Add prices to the price DB
        self.pricedb = self.book.get_price_db()

        self.price1 = GncPrice(self.book)
        self.price1.set_commodity(self.test_comm)
        self.price1.set_currency(self.usd)
        self.price1.set_time64(datetime(2025, 1, 15))
        self.price1.set_value(GncNumeric(4200, 100))  # 42.00
        self.price1.set_typestr("last")
        self.pricedb.add_price(self.price1)

        self.price2 = GncPrice(self.book)
        self.price2.set_commodity(self.test_comm)
        self.price2.set_currency(self.usd)
        self.price2.set_time64(datetime(2025, 6, 15))
        self.price2.set_value(GncNumeric(4500, 100))  # 45.00
        self.price2.set_typestr("last")
        self.pricedb.add_price(self.price2)

    def tearDown(self):
        self.ses.end()


# ---------------------------------------------------------------------------
# Test: GncPrice and GncPriceDB wrapping
# ---------------------------------------------------------------------------
class TestGncPriceWrapping(PriceSession):
    """Verify that GncPrice / GncPriceDB methods return properly wrapped
    Python objects instead of raw SwigPyObjects."""

    # -- GncPriceDB single-price lookups --

    def test_lookup_latest_returns_gnc_price(self):
        price = self.pricedb.lookup_latest(self.test_comm, self.usd)
        self.assertIsNotNone(price, "No price found for TSTK/USD")
        self.assertIsInstance(price, GncPrice)

    def test_nth_price_returns_gnc_price(self):
        price = self.pricedb.nth_price(self.test_comm, 0)
        self.assertIsNotNone(price, "nth_price(TSTK, 0) returned None")
        self.assertIsInstance(price, GncPrice)

    # -- GncPrice attribute methods --

    def test_get_commodity_returns_gnc_commodity(self):
        price = self.pricedb.lookup_latest(self.test_comm, self.usd)
        self.assertIsNotNone(price)
        comm = price.get_commodity()
        self.assertIsInstance(comm, GncCommodity)

    def test_get_currency_returns_gnc_commodity(self):
        price = self.pricedb.lookup_latest(self.test_comm, self.usd)
        self.assertIsNotNone(price)
        curr = price.get_currency()
        self.assertIsInstance(curr, GncCommodity)

    def test_get_value_returns_gnc_numeric(self):
        price = self.pricedb.lookup_latest(self.test_comm, self.usd)
        self.assertIsNotNone(price)
        val = price.get_value()
        self.assertIsInstance(val, GncNumeric)

    def test_clone_returns_gnc_price(self):
        price = self.pricedb.lookup_latest(self.test_comm, self.usd)
        self.assertIsNotNone(price)
        cloned = price.clone(self.book)
        self.assertIsInstance(cloned, GncPrice)

    # -- GncPriceDB list methods --

    def test_lookup_latest_any_currency_returns_list_of_gnc_price(self):
        prices = self.pricedb.lookup_latest_any_currency(self.test_comm)
        self.assertIsInstance(prices, list)
        self.assertGreater(len(prices), 0,
                           "Expected at least one price for TSTK")
        for p in prices:
            self.assertIsInstance(p, GncPrice)

    def test_get_prices_returns_list_of_gnc_price(self):
        prices = self.pricedb.get_prices(self.test_comm, self.usd)
        self.assertIsInstance(prices, list)
        self.assertGreater(len(prices), 0)
        for p in prices:
            self.assertIsInstance(p, GncPrice)

    def test_lookup_nearest_in_time_any_currency(self):
        date = datetime(2025, 1, 20)
        prices = self.pricedb.lookup_nearest_in_time_any_currency_t64(
            self.test_comm, date)
        self.assertIsInstance(prices, list)
        for p in prices:
            self.assertIsInstance(p, GncPrice)

    def test_lookup_nearest_before_any_currency(self):
        date = datetime(2025, 7, 1)
        prices = self.pricedb.lookup_nearest_before_any_currency_t64(
            self.test_comm, date)
        self.assertIsInstance(prices, list)
        for p in prices:
            self.assertIsInstance(p, GncPrice)


# ---------------------------------------------------------------------------
# Test: GncLot.get_split_list
# ---------------------------------------------------------------------------
class TestGncLotSplitList(TestCase):
    """Verify that GncLot.get_split_list() returns wrapped Split objects.

    Creates a buy+sell pair on an account then scrubs lots, matching the
    pattern in test_account.py's test_assignlots.
    """

    def setUp(self):
        self.ses = Session()
        self.book = self.ses.get_book()
        table = self.book.get_table()
        currency = table.lookup("CURRENCY", "USD")

        stock = GncCommodity(self.book, "Lot Test", "COMMODITY", "LTX", "LTX", 100000)
        table.insert(stock)

        self.stock_acct = Account(self.book)
        self.stock_acct.SetCommodity(stock)
        root = self.book.get_root_account()
        root.append_child(self.stock_acct)

        cash_acct = Account(self.book)
        cash_acct.SetCommodity(currency)
        root.append_child(cash_acct)

        tx = Transaction(self.book)
        tx.BeginEdit()
        tx.SetCurrency(currency)
        tx.SetDateEnteredSecs(datetime.now())
        tx.SetDatePostedSecs(datetime.now())

        # Buy 1.3 shares
        s1 = Split(self.book)
        s1.SetParent(tx)
        s1.SetAccount(self.stock_acct)
        s1.SetAmount(GncNumeric(13, 10))
        s1.SetValue(GncNumeric(100, 1))

        s2 = Split(self.book)
        s2.SetParent(tx)
        s2.SetAccount(cash_acct)
        s2.SetAmount(GncNumeric(-100, 1))
        s2.SetValue(GncNumeric(-100, 1))

        # Sell 1.3 shares
        s3 = Split(self.book)
        s3.SetParent(tx)
        s3.SetAccount(self.stock_acct)
        s3.SetAmount(GncNumeric(-13, 10))
        s3.SetValue(GncNumeric(-100, 1))

        s4 = Split(self.book)
        s4.SetParent(tx)
        s4.SetAccount(cash_acct)
        s4.SetAmount(GncNumeric(100, 1))
        s4.SetValue(GncNumeric(100, 1))

        tx.CommitEdit()
        self.stock_acct.ScrubLots()

    def tearDown(self):
        self.ses.end()

    def test_lot_exists(self):
        lots = self.stock_acct.GetLotList()
        self.assertIsInstance(lots, list)
        self.assertGreater(len(lots), 0, "ScrubLots should have created a lot")
        for lot in lots:
            self.assertIsInstance(lot, GncLot)

    def test_get_split_list_returns_splits(self):
        lots = self.stock_acct.GetLotList()
        self.assertGreater(len(lots), 0)
        splits = lots[0].get_split_list()
        self.assertIsInstance(splits, list)
        self.assertGreater(len(splits), 0, "Lot has no splits")
        for s in splits:
            self.assertIsInstance(s, Split)


# ---------------------------------------------------------------------------
# Test: Split.GetNoclosingBalance and Split.GetCapGains return GncNumeric
# ---------------------------------------------------------------------------
class TestSplitGncNumericReturns(TestCase):
    """Verify that Split methods returning gnc_numeric by value are wrapped."""

    def setUp(self):
        self.ses = Session()
        self.book = self.ses.get_book()
        table = self.book.get_table()
        currency = table.lookup("CURRENCY", "USD")

        root = self.book.get_root_account()
        acct = Account(self.book)
        acct.SetCommodity(currency)
        root.append_child(acct)

        other = Account(self.book)
        other.SetCommodity(currency)
        root.append_child(other)

        tx = Transaction(self.book)
        tx.BeginEdit()
        tx.SetCurrency(currency)
        tx.SetDateEnteredSecs(datetime.now())
        tx.SetDatePostedSecs(datetime.now())

        self.split = Split(self.book)
        self.split.SetParent(tx)
        self.split.SetAccount(acct)
        self.split.SetAmount(GncNumeric(100, 1))
        self.split.SetValue(GncNumeric(100, 1))

        s2 = Split(self.book)
        s2.SetParent(tx)
        s2.SetAccount(other)
        s2.SetAmount(GncNumeric(-100, 1))
        s2.SetValue(GncNumeric(-100, 1))

        tx.CommitEdit()

    def tearDown(self):
        self.ses.end()

    def test_get_noclosing_balance_returns_gnc_numeric(self):
        val = self.split.GetNoclosingBalance()
        self.assertIsInstance(val, GncNumeric)

    def test_get_cap_gains_returns_gnc_numeric(self):
        val = self.split.GetCapGains()
        self.assertIsInstance(val, GncNumeric)


# ---------------------------------------------------------------------------
# Test: Account.get_currency_or_parent
# ---------------------------------------------------------------------------
class TestAccountCurrencyOrParent(TestCase):
    """Verify Account.get_currency_or_parent() returns GncCommodity."""

    def setUp(self):
        self.ses = Session()
        self.book = self.ses.get_book()
        table = self.book.get_table()
        self.usd = table.lookup("CURRENCY", "USD")

        root = self.book.get_root_account()
        self.acct = Account(self.book)
        self.acct.SetCommodity(self.usd)
        root.append_child(self.acct)

    def tearDown(self):
        self.ses.end()

    def test_get_currency_or_parent_returns_commodity(self):
        result = self.acct.get_currency_or_parent()
        self.assertIsNotNone(result)
        self.assertIsInstance(result, GncCommodity)


# ---------------------------------------------------------------------------
# Test: GncCommodity.obtain_twin wrapping
# ---------------------------------------------------------------------------
class TestCommodityObtainTwin(TestCase):
    """Verify GncCommodity.obtain_twin(book) returns GncCommodity."""

    def test_obtain_twin_same_book(self):
        ses = Session()
        book = ses.get_book()
        table = book.get_table()
        usd = table.lookup("CURRENCY", "USD")
        self.assertIsNotNone(usd)
        twin = usd.obtain_twin(book)
        self.assertIsInstance(twin, GncCommodity)
        ses.end()


# ---------------------------------------------------------------------------
# Test: GncCommodity.get_namespace_ds wrapping
# ---------------------------------------------------------------------------
class TestCommodityNamespaceDS(TestCase):
    """Verify GncCommodity.get_namespace_ds() returns
    GncCommodityNamespace."""

    def test_get_namespace_ds(self):
        ses = Session()
        book = ses.get_book()
        table = book.get_table()
        usd = table.lookup("CURRENCY", "USD")
        self.assertIsNotNone(usd)
        ns = usd.get_namespace_ds()
        self.assertIsInstance(ns, GncCommodityNamespace)
        ses.end()


# ---------------------------------------------------------------------------
# Test: SWIG typemap compatibility (wrapper → instance unwrap)
# ---------------------------------------------------------------------------
class TestSwigTypemapCompat(PriceSession):
    """Verify that passing a ClassFromFunctions wrapper to a C function
    still works (via the SWIG typemap) and emits a DeprecationWarning."""

    def test_wrapper_triggers_deprecation_warning(self):
        """Passing a GncPrice wrapper to gnucash_core_c should emit
        DeprecationWarning and still return a valid result."""
        from gnucash import gnucash_core_c as gc

        price = self.pricedb.lookup_latest(self.test_comm, self.usd)
        if price is None:
            self.skipTest("No price data")

        self.assertIsInstance(price, GncPrice)

        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            # Pass the wrapper object directly — typemap should unwrap it
            comm_instance = gc.gnc_price_get_commodity(price)
            dep_warnings = [x for x in w
                            if issubclass(x.category, DeprecationWarning)]
            self.assertGreater(len(dep_warnings), 0,
                               "Expected DeprecationWarning from typemap")

    def test_raw_instance_no_warning(self):
        """Passing price.instance directly should NOT emit a warning."""
        from gnucash import gnucash_core_c as gc

        price = self.pricedb.lookup_latest(self.test_comm, self.usd)
        if price is None:
            self.skipTest("No price data")

        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            comm_instance = gc.gnc_price_get_commodity(price.instance)
            dep_warnings = [x for x in w
                            if issubclass(x.category, DeprecationWarning)]
            self.assertEqual(len(dep_warnings), 0,
                             "Unexpected DeprecationWarning for .instance")


# ---------------------------------------------------------------------------
# Test: GncPriceDB.get_*_price returns GncNumeric
# ---------------------------------------------------------------------------
class TestGetPriceReturnsGncNumeric(PriceSession):
    """Verify that get_latest_price, get_nearest_price, and
    get_nearest_before_price return GncNumeric instead of raw
    _gnc_numeric."""

    def test_get_latest_price_returns_gnc_numeric(self):
        val = self.pricedb.get_latest_price(self.test_comm, self.usd)
        self.assertIsInstance(val, GncNumeric)
        self.assertNotEqual(float(val), 0.0,
                            "Expected a non-zero price for TSTK/USD")

    def test_get_nearest_price_returns_gnc_numeric(self):
        date = datetime(2025, 1, 20)
        val = self.pricedb.get_nearest_price(self.test_comm, self.usd, date)
        self.assertIsInstance(val, GncNumeric)

    def test_get_nearest_before_price_returns_gnc_numeric(self):
        date = datetime(2025, 7, 1)
        val = self.pricedb.get_nearest_before_price(
            self.test_comm, self.usd, date)
        self.assertIsInstance(val, GncNumeric)

    def test_get_latest_price_arithmetic(self):
        """Verify the returned GncNumeric supports arithmetic."""
        val = self.pricedb.get_latest_price(self.test_comm, self.usd)
        doubled = val + val
        self.assertIsInstance(doubled, GncNumeric)
        self.assertAlmostEqual(float(doubled), float(val) * 2, places=6)


# ---------------------------------------------------------------------------
# Test: ClassFromFunctions double-wrap protection
# ---------------------------------------------------------------------------
class TestDoubleWrapProtection(TestCase):
    """Verify that passing a wrapper object as instance= to a wrapper
    class constructor unwraps it instead of creating a broken object."""

    def test_gnc_numeric_double_wrap(self):
        original = GncNumeric(7, 3)
        double = GncNumeric(instance=original)
        self.assertEqual(double.num(), 7)
        self.assertEqual(double.denom(), 3)

    def test_gnc_numeric_double_wrap_arithmetic(self):
        original = GncNumeric(1, 4)
        double = GncNumeric(instance=original)
        result = double + GncNumeric(3, 4)
        self.assertAlmostEqual(float(result), 1.0, places=6)

    def test_gnc_commodity_double_wrap(self):
        ses = Session()
        book = ses.get_book()
        table = book.get_table()
        usd = table.lookup("CURRENCY", "USD")
        double = GncCommodity(instance=usd)
        self.assertIsInstance(double, GncCommodity)
        self.assertEqual(double.get_mnemonic(), "USD")
        ses.end()

    def test_raw_instance_still_works(self):
        """Passing a raw SWIG proxy as instance= must still work."""
        from gnucash import gnucash_core_c as gc
        raw = gc.gnc_numeric_create(5, 2)
        val = GncNumeric(instance=raw)
        self.assertEqual(val.num(), 5)
        self.assertEqual(val.denom(), 2)


if __name__ == '__main__':
    main()
