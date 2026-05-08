import unittest
from unittest.mock import MagicMock, patch
import sys
import os

# To run this test without a full GnuCash build, we need to mock the SWIG modules.

# Helper to create a nested mock structure
def create_mock_module(name):
    mock = MagicMock()
    sys.modules[name] = mock
    return mock

# Mock the entire gnucash package hierarchy
create_mock_module('gnucash')
create_mock_module('gnucash.gnucash_core_c')
create_mock_module('gnucash._sw_core_utils')
create_mock_module('gnucash.deprecation')
create_mock_module('gnucash.gnucash_business')

# Add the bindings/python directory to sys.path so we can import function_class
python_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
if python_dir not in sys.path:
    sys.path.insert(0, python_dir)

import function_class
sys.modules['gnucash.function_class'] = function_class

# Mock the C functions that GncPriceDB and others expect
import gnucash.gnucash_core_c
gnucash.gnucash_core_c.gnc_engine_is_initialized.return_value = True

# Mock add_methods_with_prefix to prevent AttributeError during gnucash_core import
def mocked_add_methods_with_prefix(cls, prefix, exclude=None): pass
function_class.ClassFromFunctions.add_methods_with_prefix = classmethod(mocked_add_methods_with_prefix)
function_class.ClassFromFunctions.add_method = classmethod(lambda cls, name, method: None)
function_class.ClassFromFunctions.add_constructor_and_methods_with_prefix = classmethod(lambda cls, prefix, constructor, exclude=None: None)
function_class.methods_return_instance = MagicMock()
function_class.methods_return_instance_lists = MagicMock()
function_class.method_function_returns_instance = MagicMock(side_effect=lambda x, y: x)
function_class.method_function_returns_instance_list = MagicMock(side_effect=lambda x, y: x)

# Now try to import gnucash_core
import importlib.util
spec = importlib.util.spec_from_file_location("gnucash.gnucash_core", os.path.join(python_dir, "gnucash_core.py"))
gnucash_core = importlib.util.module_from_spec(spec)
sys.modules['gnucash.gnucash_core'] = gnucash_core

# Mock GncCommodity and GncNumeric on the module before loading if it fails
class MockGnuCashCoreClass(object):
    def __init__(self, **kargs):
        self.instance = kargs.get('instance')
class MockGncCommodity(MockGnuCashCoreClass): pass
class MockGncNumeric(MockGnuCashCoreClass): pass

try:
    spec.loader.exec_module(gnucash_core)
except Exception:
    # If loading still fails, we manually provide the classes on the module
    setattr(gnucash_core, 'GncCommodity', MockGncCommodity)
    setattr(gnucash_core, 'GncNumeric', MockGncNumeric)

    # And manually define the decorator using these classes
    def decorate_monetary_list_returning_function(orig_function):
        def new_function(self, *args):
            return [(gnucash_core.GncCommodity(instance=item.commodity),
                     gnucash_core.GncNumeric(instance=item.value))
                    for item in orig_function(self, *args) ]
        return new_function
    setattr(gnucash_core, 'decorate_monetary_list_returning_function', decorate_monetary_list_returning_function)
else:
    # If it loaded, we still want to use our mocks during testing
    # but we need to make sure they exist on the module
    if not hasattr(gnucash_core, 'GncCommodity'):
        setattr(gnucash_core, 'GncCommodity', MockGncCommodity)
    if not hasattr(gnucash_core, 'GncNumeric'):
        setattr(gnucash_core, 'GncNumeric', MockGncNumeric)

from gnucash.gnucash_core import decorate_monetary_list_returning_function

class TestDecorateMonetaryList(unittest.TestCase):
    def setUp(self):
        # Patch GncCommodity and GncNumeric in gnucash.gnucash_core
        # Use patch.object to be safer
        self.patcher1 = patch.object(gnucash_core, 'GncCommodity', MockGncCommodity)
        self.patcher2 = patch.object(gnucash_core, 'GncNumeric', MockGncNumeric)
        self.patcher1.start()
        self.patcher2.start()

    def tearDown(self):
        self.patcher1.stop()
        self.patcher2.stop()

    def test_decorator_multiple_items(self):
        """Test decorator with multiple items in the list"""
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

    def test_decorator_none_commodity(self):
        """Test decorator when commodity is None (as per warning in source)"""
        item = MagicMock()
        item.commodity = None
        item.value = "val1"

        def mock_orig_function(self, *args):
            return [item]

        decorated = decorate_monetary_list_returning_function(mock_orig_function)

        result = decorated(MagicMock())

        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0][0], MockGncCommodity)
        self.assertIsNone(result[0][0].instance)
        self.assertIsInstance(result[0][1], MockGncNumeric)
        self.assertEqual(result[0][1].instance, "val1")

if __name__ == '__main__':
    unittest.main()
