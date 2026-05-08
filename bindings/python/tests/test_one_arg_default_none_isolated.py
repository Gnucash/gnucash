# test cases for one_arg_default_none decorator logic
#
# This test is isolated and does not import gnucash_core,
# but instead tests the logic by referencing the implementation.
#
# @author Jules

import unittest
import sys
import os

# Ensure the bindings/python directory is in PYTHONPATH for function_class
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))
from function_class import default_arguments_decorator

# Replicate the implementation of one_arg_default_none
def one_arg_default_none(function):
    return default_arguments_decorator(function, None, None)

class TestOneArgDefaultNoneIsolated(unittest.TestCase):
    def test_one_arg_default_none_decorator_logic(self):
        # A mock function that we want to decorate
        def mock_function(instance, arg1=None, arg2=None):
            """Original docstring"""
            return instance, arg1, arg2

        # Apply the decorator
        decorated = one_arg_default_none(mock_function)

        # one_arg_default_none uses default_arguments_decorator(function, None, None)
        # which means it sets the first TWO positional arguments (after self) to None if not provided.

        # When calling decorated(inst), it should be mock_function(inst, None, None)
        inst = "mock_instance"
        self.assertEqual(decorated(inst), (inst, None, None))

        # When calling decorated(inst, "value"), it should be mock_function(inst, "value", None)
        self.assertEqual(decorated(inst, "value"), (inst, "value", None))

        # When calling decorated(inst, "value1", "value2"), it should be mock_function(inst, "value1", "value2")
        self.assertEqual(decorated(inst, "value1", "value2"), (inst, "value1", "value2"))

        # Verify docstring modification (behavior of default_arguments_decorator)
        self.assertIsNotNone(decorated.__doc__)
        self.assertIn("positional argument defaults", decorated.__doc__)
        self.assertIn("None", decorated.__doc__)

if __name__ == "__main__":
    unittest.main()
