import unittest
from unittest.mock import MagicMock, patch
import sys
import os

# Store original sys.modules to restore after test
original_modules = sys.modules.copy()
original_path = sys.path.copy()

# Ensure bindings/python is in sys.path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

# Mocking gnucash and _sw_app_utils because they might not be available in the test environment
# without a full build of the project.
mock_gnucash = MagicMock()
mock_sw_app_utils = MagicMock()

# We need to mock 'gnucash' before 'app_utils' is imported
sys.modules['gnucash'] = mock_gnucash
sys.modules['gnucash._sw_app_utils'] = mock_sw_app_utils

# Now we can import app_utils
import app_utils

class TestAppUtils(unittest.TestCase):
    @classmethod
    def tearDownClass(cls):
        # Restore sys.modules and sys.path
        sys.modules.clear()
        sys.modules.update(original_modules)
        sys.path[:] = original_path

    def setUp(self):
        app_utils._sw_app_utils.gnc_get_current_session.reset_mock()
        mock_gnucash.Session.reset_mock()

    def test_gnc_get_current_session_exists(self):
        # Setup mock instance
        mock_instance = MagicMock()

        app_utils._sw_app_utils.gnc_get_current_session.return_value = mock_instance

        # Mock Session class inside gnucash mock
        mock_session_class = MagicMock()
        mock_gnucash.Session = mock_session_class

        # Call the function
        result = app_utils.gnc_get_current_session()

        # Verify
        app_utils._sw_app_utils.gnc_get_current_session.assert_called_once()
        mock_session_class.assert_called_once_with(instance=mock_instance)
        self.assertEqual(result, mock_session_class.return_value)

    def test_gnc_get_current_session_not_exists(self):
        # Setup mock to return None (simulating no current session)
        app_utils._sw_app_utils.gnc_get_current_session.return_value = None

        # Call the function
        result = app_utils.gnc_get_current_session()

        # Verify
        app_utils._sw_app_utils.gnc_get_current_session.assert_called_once()
        self.assertIsNone(result)

if __name__ == '__main__':
    unittest.main()
