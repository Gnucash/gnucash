import unittest
import sys
import os

# Ensure we use mock bindings
from gnucash_mock_helper import setup_gnucash_mocks
mock_gnucash_core_c = setup_gnucash_mocks()

from gnucash_core import one_arg_default_none, Session

class TestOneArgDefaultNone(unittest.TestCase):
    def test_one_arg_default_none_logic(self):
        # A mock function that we want to decorate.
        # Since one_arg_default_none is used on methods like Session.load and Session.save,
        # which accept self plus another argument (percentage_func), it has one explicit parameter after self
        # but one_arg_default_none provides *two* Nones.
        # This means default_arguments_decorator is providing defaults for up to two positional args after self.
        def mock_function(instance, arg1=None, arg2=None):
            """Original docstring"""
            return instance, arg1, arg2

        # Apply the decorator
        decorated = one_arg_default_none(mock_function)

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

    def test_session_load_save_integration(self):
        session = Session()

        # Test Session.load() without arguments
        # It should call qof_session_load(instance, None) due to the decorator
        session.load()
        mock_gnucash_core_c.qof_session_load.assert_called_with(session.instance, None)

        # Test Session.load(percentage_func)
        percent_func = lambda x: None
        session.load(percent_func)
        mock_gnucash_core_c.qof_session_load.assert_called_with(session.instance, percent_func)

        # Test Session.save() without arguments
        # It should call qof_session_save(instance, None) due to the decorator
        session.save()
        mock_gnucash_core_c.qof_session_save.assert_called_with(session.instance, None)

        # Test Session.save(percentage_func)
        percent_func = lambda x: None
        session.save(percent_func)
        mock_gnucash_core_c.qof_session_save.assert_called_with(session.instance, percent_func)

if __name__ == "__main__":
    unittest.main()
