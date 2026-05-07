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
from change_tax_code import mark_account_with_code_as_tax_related

class TestChangeTaxCode(TestCase):
    def setUp(self):
        # Create a mock account
        self.root_account = MagicMock()
        self.root_account.GetCode.return_value = "ROOT"
        self.root_account.get_children.return_value = []

    def test_root_match(self):
        """Test that the function returns True and sets tax related when root matches."""
        target_code = "1234"
        self.root_account.GetCode.return_value = target_code

        result = mark_account_with_code_as_tax_related(self.root_account, target_code)

        self.assertTrue(result)
        self.root_account.SetTaxRelated.assert_called_once_with(True)

    def test_child_match(self):
        """Test that the function finds a match in a direct child."""
        target_code = "1234"
        child = MagicMock()
        child.GetCode.return_value = target_code
        child.get_children.return_value = []
        self.root_account.get_children.return_value = [child]

        result = mark_account_with_code_as_tax_related(self.root_account, target_code)

        self.assertTrue(result)
        child.SetTaxRelated.assert_called_once_with(True)
        # Root should NOT be marked since it didn't match
        self.root_account.SetTaxRelated.assert_not_called()

    def test_grandchild_match(self):
        """Test that the function finds a match in a nested grandchild."""
        target_code = "1234"
        child = MagicMock()
        child.GetCode.return_value = "CHILD"

        grandchild = MagicMock()
        grandchild.GetCode.return_value = target_code
        grandchild.get_children.return_value = []

        child.get_children.return_value = [grandchild]
        self.root_account.get_children.return_value = [child]

        result = mark_account_with_code_as_tax_related(self.root_account, target_code)

        self.assertTrue(result)
        grandchild.SetTaxRelated.assert_called_once_with(True)
        child.SetTaxRelated.assert_not_called()
        self.root_account.SetTaxRelated.assert_not_called()

    def test_no_match(self):
        """Test that the function returns False when no account matches the code."""
        child = MagicMock()
        child.GetCode.return_value = "CHILD"
        child.get_children.return_value = []
        self.root_account.get_children.return_value = [child]

        result = mark_account_with_code_as_tax_related(self.root_account, "NOMATCH")

        self.assertFalse(result)
        child.SetTaxRelated.assert_not_called()
        self.root_account.SetTaxRelated.assert_not_called()

    def test_branching_match(self):
        """Test that the function continues searching if the first branch doesn't match."""
        target_code = "1234"
        child1 = MagicMock()
        child1.GetCode.return_value = "CHILD1"
        child1.get_children.return_value = []

        child2 = MagicMock()
        child2.GetCode.return_value = target_code
        child2.get_children.return_value = []

        self.root_account.get_children.return_value = [child1, child2]

        result = mark_account_with_code_as_tax_related(self.root_account, target_code)

        self.assertTrue(result)
        child2.SetTaxRelated.assert_called_once_with(True)
        child1.SetTaxRelated.assert_not_called()
        self.root_account.SetTaxRelated.assert_not_called()

if __name__ == "__main__":
    main()
