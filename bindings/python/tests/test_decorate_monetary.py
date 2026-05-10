import unittest
from unittest.mock import MagicMock
import sys

from unittest_support import *

from gnucash.gnucash_core import decorate_monetary_list_returning_function, GncCommodity, GncNumeric
import gnucash.gnucash_core_c as gnucash_core_c

class TestDecorateMonetaryList(unittest.TestCase):
    def test_decorator_multiple_items(self):
        item1 = MagicMock()
        item1.commodity = gnucash_core_c.gnc_commodity_new(None, "comm1", "namespace1", "code1", "frac1", 1)
        item1.value = gnucash_core_c.gnc_numeric_create(100, 100)

        item2 = MagicMock()
        item2.commodity = gnucash_core_c.gnc_commodity_new(None, "comm2", "namespace2", "code2", "frac2", 1)
        item2.value = gnucash_core_c.gnc_numeric_create(200, 100)

        def mock_orig_function(self, *args):
            return [item1, item2]

        decorated = decorate_monetary_list_returning_function(mock_orig_function)

        mock_self = MagicMock()
        result = decorated(mock_self, "arg1")

        self.assertEqual(len(result), 2)

        # Verify first item
        self.assertIsInstance(result[0][0], GncCommodity)
        self.assertIsInstance(result[0][1], GncNumeric)

        # Verify second item
        self.assertIsInstance(result[1][0], GncCommodity)
        self.assertIsInstance(result[1][1], GncNumeric)

    def test_decorator_empty_list(self):
        def mock_orig_function(self, *args):
            return []

        decorated = decorate_monetary_list_returning_function(mock_orig_function)
        result = decorated(MagicMock())
        self.assertEqual(result, [])

    def test_decorator_arguments_passed(self):
        mock_orig = MagicMock(return_value=[])
        decorated = decorate_monetary_list_returning_function(mock_orig)

        mock_self = MagicMock()
        decorated(mock_self, "pos_arg1", "pos_arg2")

        mock_orig.assert_called_once_with(mock_self, "pos_arg1", "pos_arg2")

    def test_decorator_none_commodity(self):
        item = MagicMock()
        item.commodity = None
        item.value = gnucash_core_c.gnc_numeric_create(100, 100)

        def mock_orig_function(self, *args):
            return [item]

        decorated = decorate_monetary_list_returning_function(mock_orig_function)

        # Expect TypeError as this is the actual behavior of the current codebase.
        with self.assertRaises(TypeError):
            decorated(MagicMock())

if __name__ == '__main__':
    unittest.main()
