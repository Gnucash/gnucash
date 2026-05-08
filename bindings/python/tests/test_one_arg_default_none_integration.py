# test cases for one_arg_default_none decorator integration in gnucash_core.py
#
# This test uses gnucash_mock_helper to import gnucash_core and verify
# that the decorator is correctly applied and functioning on Session methods.
#
# @author Jules

import unittest
import sys
import os

# Setup mocks before importing anything from gnucash
# Add the current directory to sys.path so we can find gnucash_mock_helper
sys.path.insert(0, os.path.dirname(__file__))
from gnucash_mock_helper import setup_gnucash_mocks
mock_gnucash_core_c = setup_gnucash_mocks()

import gnucash_core

class TestOneArgDefaultNoneIntegration(unittest.TestCase):
    def test_session_load_behavior(self):
        session = gnucash_core.Session()

        # Test Session.load() without arguments
        # It should call qof_session_load(instance, None) due to the decorator
        session.load()
        mock_gnucash_core_c.qof_session_load.assert_called_with(session.instance, None)

        # Test Session.load(percentage_func)
        percent_func = lambda x: None
        session.load(percent_func)
        mock_gnucash_core_c.qof_session_load.assert_called_with(session.instance, percent_func)

    def test_session_save_behavior(self):
        session = gnucash_core.Session()

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
