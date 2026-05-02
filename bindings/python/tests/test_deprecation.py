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

if __name__ == '__main__':
    unittest.main()
