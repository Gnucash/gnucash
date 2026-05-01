import unittest
from unittest.mock import MagicMock, patch
import sys
import os
import types

# Ensure we can import from the parent directory (bindings/python)
script_dir = os.path.dirname(os.path.abspath(__file__))
parent_dir = os.path.abspath(os.path.join(script_dir, '..'))
sys.path.append(parent_dir)

# 1. Mock the gnucash package and its C extensions
mock_gnucash_core_c = MagicMock()
mock_sw_core_utils = MagicMock()

gnucash_pkg = types.ModuleType('gnucash')
sys.modules['gnucash'] = gnucash_pkg
gnucash_pkg.gnucash_core_c = mock_gnucash_core_c
sys.modules['gnucash.gnucash_core_c'] = mock_gnucash_core_c
gnucash_pkg._sw_core_utils = mock_sw_core_utils
sys.modules['gnucash._sw_core_utils'] = mock_sw_core_utils

# populate __dict__ for extract_attributes_with_prefix if needed
mock_gnucash_core_c.__dict__.update({
    'ERR_BACKEND_NO_ERR': 0,
})

# 2. Mock function_class before importing gnucash_core
import function_class

class MockMeta(type):
    def __getattr__(cls, name):
        return MagicMock()

class SimpleClassFromFunctions(metaclass=MockMeta):
    def __init__(self, *args, **kwargs):
        self.__instance = kwargs.get('instance')
    def get_instance(self): return self.__instance
    instance = property(get_instance)
    @classmethod
    def add_method(cls, func_name, meth_name):
        setattr(cls, meth_name, MagicMock())
    @classmethod
    def add_methods_with_prefix(cls, prefix, exclude=[]): pass
    @classmethod
    def add_constructor_and_methods_with_prefix(cls, prefix, constructor, exclude=[]): pass
    @classmethod
    def decorate_functions(cls, decorator, *args): pass
    @classmethod
    def decorate_method(cls, decorator, method_name, *args, **kargs): pass

function_class.ClassFromFunctions = SimpleClassFromFunctions
function_class.methods_return_instance = MagicMock()
function_class.methods_return_instance_lists = MagicMock()
function_class.method_function_returns_instance = MagicMock(side_effect=lambda f, c: f)
function_class.method_function_returns_instance_list = MagicMock(side_effect=lambda f, c: f)
function_class.extract_attributes_with_prefix = MagicMock(return_value=[])

sys.modules['gnucash.function_class'] = function_class

# 3. Mock deprecation
import deprecation
sys.modules['gnucash.deprecation'] = deprecation

# 4. Import gnucash_core
import gnucash_core
gnucash_core.ERR_BACKEND_NO_ERR = 0

from gnucash_core import Session

class TestSessionGenerateErrors(unittest.TestCase):
    def setUp(self):
        # Create a Session instance with a mocked instance data
        self.mock_instance = MagicMock()
        self.session = Session(instance=self.mock_instance)

        # Mock get_error and pop_error on the session instance
        self.session.get_error = MagicMock()
        self.session.pop_error = MagicMock()

        # Access ERR_BACKEND_NO_ERR from the gnucash_core module
        self.ERR_NO_ERR = gnucash_core.ERR_BACKEND_NO_ERR

    def test_generate_errors_no_errors(self):
        """Test generate_errors when there are no errors."""
        self.session.get_error.return_value = self.ERR_NO_ERR

        errors_gen = self.session.generate_errors()
        errors = list(errors_gen)

        self.assertEqual(len(errors), 0)
        self.session.get_error.assert_called_once()
        self.session.pop_error.assert_not_called()

    def test_generate_errors_single_error(self):
        """Test generate_errors when there is a single error."""
        some_error = 123
        self.session.get_error.side_effect = [some_error, self.ERR_NO_ERR]
        self.session.pop_error.return_value = some_error

        errors = list(self.session.generate_errors())

        self.assertEqual(errors, [some_error])
        self.assertEqual(self.session.get_error.call_count, 2)
        self.session.pop_error.assert_called_once()

    def test_generate_errors_multiple_errors(self):
        """Test generate_errors when there are multiple errors."""
        error1 = 123
        error2 = 456
        self.session.get_error.side_effect = [error1, error2, self.ERR_NO_ERR]
        self.session.pop_error.side_effect = [error1, error2]

        errors = list(self.session.generate_errors())

        self.assertEqual(errors, [error1, error2])
        self.assertEqual(self.session.get_error.call_count, 3)
        self.assertEqual(self.session.pop_error.call_count, 2)

if __name__ == '__main__':
    unittest.main()
