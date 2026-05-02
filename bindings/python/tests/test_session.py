# test cases for Session wrapper object
#
# test for get_book may belong in test_book but it makes sense here
# to see if get_current_session works
# test for app_utils on the other hand could go to a subfolder of
# /libgnucash/app-utils
#
# @date 2020-04-03
# @author Christoph Holtermann <mail@c-holtermann.net>

from unittest import TestCase, main, mock

from gnucash import (
        Session,
        SessionOpenMode
)

from gnucash.gnucash_core import GnuCashBackendException
from unittest.mock import patch

class TestSession(TestCase):
    def test_create_empty_session(self):
        self.ses = Session()

    def test_session_deprecated_arguments(self):
        """use deprecated arguments ignore_lock, is_new, force_new"""
        self.ses = Session(ignore_lock=False, is_new=True, force_new=False)

    def test_session_mode(self):
        """use mode argument"""
        self.ses = Session(mode=SessionOpenMode.SESSION_NORMAL_OPEN)

    def test_session_with_new_file(self):
        """create Session with new xml file"""
        from tempfile import TemporaryDirectory
        from urllib.parse import urlunparse
        with TemporaryDirectory() as tempdir:
            uri = urlunparse(("xml", tempdir, "tempfile", "", "", ""))
            with Session(uri, SessionOpenMode.SESSION_NEW_STORE) as ses:
                pass

            # try to open nonexistent file without NEW mode - should raise Exception
            uri = urlunparse(("xml", tempdir, "tempfile2", "", "", ""))
            with Session() as ses:
                with self.assertRaises(GnuCashBackendException):
                    ses.begin(uri, mode=SessionOpenMode.SESSION_NORMAL_OPEN)

            # try to open nonexistent file without NEW mode - should raise Exception
            # use deprecated arg is_new
            uri = urlunparse(("xml", tempdir, "tempfile2", "", "", ""))
            with Session() as ses:
                with self.assertRaises(GnuCashBackendException):
                    ses.begin(uri, is_new=False)

            uri = urlunparse(("xml", tempdir, "tempfile3", "", "", ""))
            with Session() as ses:
                ses.begin(uri, mode=SessionOpenMode.SESSION_NEW_STORE)

            # test using deprecated args
            uri = urlunparse(("xml", tempdir, "tempfile4", "", "", ""))
            with Session() as ses:
                ses.begin(uri, is_new=True)


    def test_app_utils_get_current_session(self):
        from gnucash import _sw_app_utils
        self.ses_instance = _sw_app_utils.gnc_get_current_session()
        self.ses = Session(instance = self.ses_instance)
        self.assertIsInstance(obj = self.ses, cls = Session)

    def test_get_book_from_current_session(self):
        from gnucash import _sw_app_utils
        from gnucash import Book
        self.ses_instance = _sw_app_utils.gnc_get_current_session()
        self.ses = Session(instance = self.ses_instance)
        self.book = self.ses.get_book()
        self.assertIsInstance(obj = self.book, cls = Book)

    @patch('gnucash.Session.get_error')
    def test_generate_errors_empty(self, mock_get_error):
        from gnucash.gnucash_core import ERR_BACKEND_NO_ERR
        mock_get_error.return_value = ERR_BACKEND_NO_ERR
        ses = Session()
        errors = list(ses.generate_errors())
        self.assertEqual(errors, [])
        mock_get_error.assert_called_once()

    @patch('gnucash.Session.get_error')
    @patch('gnucash.Session.pop_error')
    def test_generate_errors_multiple(self, mock_pop_error, mock_get_error):
        from gnucash.gnucash_core import ERR_BACKEND_NO_ERR
        # Assume 1 and 2 are some backend errors.
        mock_get_error.side_effect = [1, 2, ERR_BACKEND_NO_ERR]
        mock_pop_error.side_effect = [1, 2]
        ses = Session()
        errors = list(ses.generate_errors())
        self.assertEqual(errors, [1, 2])
        self.assertEqual(mock_get_error.call_count, 3)
        self.assertEqual(mock_pop_error.call_count, 2)

    @patch('gnucash.Session.pop_all_errors')
    def test_raise_backend_errors_empty(self, mock_pop_all_errors):
        """Test that raise_backend_errors does nothing when there are no errors."""
        mock_pop_all_errors.return_value = ()
        ses = Session()
        # This should not raise an exception
        ses.raise_backend_errors()
        mock_pop_all_errors.assert_called_once()

    @patch('gnucash.Session.pop_all_errors')
    def test_raise_backend_errors_with_errors(self, mock_pop_all_errors):
        """Test that raise_backend_errors raises GnuCashBackendException when there are errors."""
        from gnucash.gnucash_core import backend_error_dict
        # Get a valid error key from backend_error_dict if it is not empty, otherwise default to a mock value
        # We need a valid key because the function uses backend_error_dict[errors[0]]
        if backend_error_dict:
            error_code = next(iter(backend_error_dict.keys()))
        else:
            # Fallback if dictionary is somehow empty or mocked
            error_code = 1
            backend_error_dict[error_code] = 'ERR_MOCK_ERROR'

        mock_pop_all_errors.return_value = (error_code,)
        ses = Session()

        with self.assertRaises(GnuCashBackendException) as context:
            ses.raise_backend_errors("test_function")

        mock_pop_all_errors.assert_called_once()
        self.assertIn("call to test_function resulted in the following errors", str(context.exception))
        self.assertEqual(context.exception.errors, (error_code,))


if __name__ == '__main__':
    main()
