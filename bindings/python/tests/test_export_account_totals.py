import sys
import os
from unittest import TestCase, main
from unittest.mock import MagicMock

# Mock the gnucash module before importing the script that uses it
mock_gnucash = MagicMock()
sys.modules["gnucash"] = mock_gnucash

# Add the example_scripts directory to sys.path to find the script
script_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'example_scripts'))
if script_dir not in sys.path:
    sys.path.append(script_dir)

# Now we can import the function to be tested
from export_account_totals import get_all_sub_accounts, to_string_with_decimal_point_placed

class TestExportAccountTotals(TestCase):
    def create_mock_account(self, name, children=None):
        account = MagicMock()
        account.GetName.return_value = name
        account.get_children_sorted.return_value = children if children else []
        return account

    def test_no_children(self):
        """Test with an account that has no children."""
        root = self.create_mock_account("Root")

        results = list(get_all_sub_accounts(root))

        self.assertEqual(len(results), 0)

    def test_flat_children(self):
        """Test with an account that has multiple direct children."""
        child1 = self.create_mock_account("Child1")
        child2 = self.create_mock_account("Child2")
        root = self.create_mock_account("Root", [child1, child2])

        results = list(get_all_sub_accounts(root))

        self.assertEqual(len(results), 2)
        self.assertEqual(results[0], (child1, "Child1"))
        self.assertEqual(results[1], (child2, "Child2"))

    def test_nested_children(self):
        """Test with nested children to verify recursion and name joining."""
        grandchild = self.create_mock_account("Grandchild")
        child = self.create_mock_account("Child", [grandchild])
        root = self.create_mock_account("Root", [child])

        results = list(get_all_sub_accounts(root))

        self.assertEqual(len(results), 2)
        self.assertEqual(results[0], (child, "Child"))
        self.assertEqual(results[1], (grandchild, "Child::Grandchild"))

    def test_complex_hierarchy(self):
        """Test a more complex hierarchy."""
        # Root
        #  +- A
        #  |  +- A1
        #  |  +- A2
        #  |     +- A2a
        #  +- B

        a2a = self.create_mock_account("A2a")
        a1 = self.create_mock_account("A1")
        a2 = self.create_mock_account("A2", [a2a])
        a = self.create_mock_account("A", [a1, a2])
        b = self.create_mock_account("B")
        root = self.create_mock_account("Root", [a, b])

        results = list(get_all_sub_accounts(root))

        expected = [
            (a, "A"),
            (a1, "A::A1"),
            (a2, "A::A2"),
            (a2a, "A::A2::A2a"),
            (b, "B")
        ]

        self.assertEqual(len(results), len(expected))
        for i in range(len(expected)):
            self.assertEqual(results[i], expected[i])

    def test_to_string_with_decimal_point_placed(self):
        """Test the decimal point placement logic."""

        class MockNumeric:
            def __init__(self, num, denom):
                self._num = num
                self._denom = denom

            def num(self):
                return self._num

            def denom(self):
                return self._denom

            def to_decimal(self, arg):
                return True

            def __copy__(self):
                return MockNumeric(self._num, self._denom)

        def create_mock_numeric(num, denom):
            return MockNumeric(num, denom)

        # Test cases: (numerator, denominator, expected_output)
        test_cases = [
            (100, 1, "100"),
            (100, 100, "1.00"),
            (1, 100, "0.01"),
            (10, 100, "0.10"),
            (1001, 1000, "1.001"),
            (0, 100, "0.00"),
            (-1, 100, "-0.01"),
            (-100, 100, "-1.00"),
            (-101, 100, "-1.01"),
        ]

        for num, denom, expected in test_cases:
            with self.subTest(num=num, denom=denom):
                gnc_num = create_mock_numeric(num, denom)
                self.assertEqual(to_string_with_decimal_point_placed(gnc_num), expected)

if __name__ == "__main__":
    main()
