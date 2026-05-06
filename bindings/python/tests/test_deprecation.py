import unittest
import warnings
from gnucash.deprecation import deprecated

class TestDeprecation(unittest.TestCase):
    def test_deprecated_decorator(self):
        @deprecated("Use waldo_pepper() instead.")
        def my_deprecated_func():
            return "old behavior"

        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")

            result = my_deprecated_func()

            self.assertEqual(result, "old behavior")
            self.assertEqual(len(w), 1)
            self.assertTrue(issubclass(w[-1].category, DeprecationWarning))
            self.assertIn("my_deprecated_func", str(w[-1].message))
            self.assertIn("Use waldo_pepper() instead.", str(w[-1].message))

    def test_deprecated_returns_wrapped_function_result(self):
        @deprecated("Use something else")
        def my_func(a, b):
            """My Docstring"""
            return a + b

        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            res = my_func(2, 3)
            self.assertEqual(res, 5)
            self.assertEqual(my_func.__name__, "my_func")
            self.assertEqual(my_func.__doc__, "My Docstring")

    def test_deprecated_args_session_deprecation(self):
        from gnucash.deprecation import deprecated_args_session
        from gnucash.gnucash_core import SessionOpenMode

        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")

            # test 1: no deprecated args, no mode. Should return mode=None
            mode = deprecated_args_session()
            self.assertIsNone(mode)

            # test 2: mode provided, no deprecated args. Should return mode
            mode = deprecated_args_session(mode=SessionOpenMode.SESSION_READ_ONLY)
            self.assertEqual(mode, SessionOpenMode.SESSION_READ_ONLY)

            # test 3: mode from ignore_lock_or_mode arg
            mode = deprecated_args_session(ignore_lock_or_mode=SessionOpenMode.SESSION_NORMAL_OPEN)
            self.assertEqual(mode, SessionOpenMode.SESSION_NORMAL_OPEN)

        self.assertEqual(len(w), 0)

        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")

            # test 4: using deprecated arg is_new
            mode = deprecated_args_session(is_new=True)
            self.assertEqual(mode, SessionOpenMode.SESSION_NEW_STORE)

            # test 5: using deprecated arg force_new
            mode = deprecated_args_session(force_new=True, is_new=True)
            self.assertEqual(mode, SessionOpenMode.SESSION_NEW_OVERWRITE)

            # test 6: using deprecated arg ignore_lock
            mode = deprecated_args_session(ignore_lock=True)
            self.assertEqual(mode, SessionOpenMode.SESSION_READ_ONLY)

            # test 7: using both mode and deprecated args
            mode = deprecated_args_session(is_new=True, mode=SessionOpenMode.SESSION_NORMAL_OPEN)
            self.assertEqual(mode, SessionOpenMode.SESSION_NORMAL_OPEN)

        self.assertEqual(len(w), 4)
        for warning in w:
            self.assertTrue(issubclass(warning.category, DeprecationWarning))
            self.assertIn("Use of ignore_lock, is_new or force_new arguments is deprecated", str(warning.message))

    def test_deprecated_args_session_init(self):
        from gnucash.deprecation import deprecated_args_session_init

        class DummySession:
            def __init__(self, book_uri=None, mode=None, instance=None):
                self.book_uri = book_uri
                self.mode = mode
                self.instance = instance

        OriginalInit = DummySession.__init__
        DummySession.__init__ = deprecated_args_session_init(DummySession.__init__)

        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            ses = DummySession(book_uri="dummy_uri", is_new=True)
            from gnucash.gnucash_core import SessionOpenMode
            self.assertEqual(ses.mode, SessionOpenMode.SESSION_NEW_STORE)
            self.assertEqual(len(w), 1)

    def test_deprecated_args_session_begin(self):
        from gnucash.deprecation import deprecated_args_session_begin

        class DummySession:
            def begin(self, new_uri=None, mode=None):
                self.new_uri = new_uri
                self.mode = mode

        OriginalBegin = DummySession.begin
        DummySession.begin = deprecated_args_session_begin(DummySession.begin)

        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            ses = DummySession()
            ses.begin(new_uri="dummy_uri", force_new=True, is_new=True)
            from gnucash.gnucash_core import SessionOpenMode
            self.assertEqual(ses.mode, SessionOpenMode.SESSION_NEW_OVERWRITE)
            self.assertEqual(len(w), 1)

if __name__ == '__main__':
    unittest.main()
