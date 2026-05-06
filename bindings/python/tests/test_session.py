# test cases for Session wrapper object
#
# test for get_book may belong in test_book but it makes sense here
# to see if get_current_session works
# test for app_utils on the other hand could go to a subfolder of
# /libgnucash/app-utils
#
# @date 2020-04-03
# @author Christoph Holtermann <mail@c-holtermann.net>

from unittest import TestCase, main

from gnucash import (
        Session,
        SessionOpenMode
)

from gnucash.gnucash_core import GnuCashBackendException

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

if __name__ == '__main__':
    main()
