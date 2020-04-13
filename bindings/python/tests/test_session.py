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

from gnucash import Session

class TestSession(TestCase):
    def test_create_empty_session(self):
        self.ses = Session()

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
