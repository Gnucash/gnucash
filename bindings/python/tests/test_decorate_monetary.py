import unittest
from unittest.mock import MagicMock

# Import the decorator from the target module
from gnucash.gnucash_core import decorate_monetary_list_returning_function

# Define mock classes that mimic the behavior of the real ones for testing.
# These will be used to wrap the raw values returned by the original function.
class MockGnuCashCoreClass(object):
    def __init__(self, **kargs):
        self.instance = kargs.get('instance')

class MockGncCommodity(MockGnuCashCoreClass): pass
class MockGncNumeric(MockGnuCashCoreClass): pass

class TestDecorateMonetaryList(unittest.TestCase):
    def setUp(self):
        # We need to ensure that GncCommodity and GncNumeric are available
        # in the namespace of the function we are testing.
        # In a real environment, they are globally available in gnucash_core.py.
        # For this test, we'll patch them into the module where the decorator resides.
        import gnucash.gnucash_core as core
        self.orig_commodity = getattr(core, 'GncCommodity', None)
        self.orig_numeric = getattr(core, 'GncNumeric', None)
        core.GncCommodity = MockGncCommodity
        core.GncNumeric = MockGncNumeric

    def tearDown(self):
        import gnucash.gnucash_core as core
        if self.orig_commodity:
            core.GncCommodity = self.orig_commodity
        if self.orig_numeric:
            core.GncNumeric = self.orig_numeric

    def test_decorator_multiple_items(self):
        """Test decorator with multiple items in the list"""
        # Mock items with commodity and value attributes
        item1 = MagicMock()
        item1.commodity = "comm1"
        item1.value = "val1"

        item2 = MagicMock()
        item2.commodity = "comm2"
        item2.value = "val2"

        def mock_orig_function(self, *args):
            return [item1, item2]

        decorated = decorate_monetary_list_returning_function(mock_orig_function)

        mock_self = MagicMock()
        result = decorated(mock_self, "arg1")

        self.assertEqual(len(result), 2)

        # Verify first item
        self.assertIsInstance(result[0][0], MockGncCommodity)
        self.assertEqual(result[0][0].instance, "comm1")
        self.assertIsInstance(result[0][1], MockGncNumeric)
        self.assertEqual(result[0][1].instance, "val1")

        # Verify second item
        self.assertIsInstance(result[1][0], MockGncCommodity)
        self.assertEqual(result[1][0].instance, "comm2")
        self.assertIsInstance(result[1][1], MockGncNumeric)
        self.assertEqual(result[1][1].instance, "val2")

    def test_decorator_empty_list(self):
        """Test decorator with an empty list"""
        def mock_orig_function(self, *args):
            return []

        decorated = decorate_monetary_list_returning_function(mock_orig_function)
        result = decorated(MagicMock())
        self.assertEqual(result, [])

    def test_decorator_arguments_passed(self):
        """Test that arguments are correctly passed to the original function"""
        mock_orig = MagicMock(return_value=[])
        decorated = decorate_monetary_list_returning_function(mock_orig)

        mock_self = MagicMock()
        decorated(mock_self, "pos_arg1", "pos_arg2")

        mock_orig.assert_called_once_with(mock_self, "pos_arg1", "pos_arg2")

if __name__ == '__main__':
    unittest.main()
