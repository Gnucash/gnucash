import sys
import os
import types
from unittest import TestCase, main
from unittest.mock import MagicMock, patch
from datetime import datetime

# Setup mock modules properly
def create_mock_module(name):
    mock = types.ModuleType(name)
    sys.modules[name] = mock
    return mock

mock_gnucash = create_mock_module("gnucash")
mock_gnucash_business = create_mock_module("gnucash.gnucash_business")
mock_gnucash.gnucash_business = mock_gnucash_business
create_mock_module("str_methods")
create_mock_module("gncinvoicefkt")
mock_ipython = create_mock_module("IPython")
mock_ipython.version_info = (0, 0, 0)
create_mock_module("IPython.frontend")
create_mock_module("IPython.frontend.terminal")
mock_ipapp = create_mock_module("IPython.frontend.terminal.ipapp")
mock_ipapp.TerminalIPythonApp = MagicMock()

# Add some required constants and classes to mock_gnucash_business
mock_gnucash_business.Customer = MagicMock()
mock_gnucash_business.Employee = MagicMock()
mock_gnucash_business.Vendor = MagicMock()
mock_gnucash_business.Job = MagicMock()
mock_gnucash_business.Address = MagicMock()
mock_gnucash_business.Invoice = MagicMock()
mock_gnucash_business.TaxTable = MagicMock()
mock_gnucash_business.TaxTableEntry = MagicMock()
mock_gnucash_business.GNC_AMT_TYPE_PERCENT = 1
mock_gnucash_business.GNC_DISC_PRETAX = 1

mock_gnucash.SessionOpenMode = MagicMock()

class Entry:
    def __init__(self, instance=None):
        if instance:
            self.GetDescription = instance.GetDescription
            self.GetInvPrice = instance.GetInvPrice
            self.GetQuantity = instance.GetQuantity
    def GetDescription(self): return ""
    def GetInvPrice(self): return MagicMock()
    def GetQuantity(self): return MagicMock()

mock_gnucash_business.Entry = Entry

# Add the example_scripts directory to sys.path
script_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'example_scripts'))
if script_dir not in sys.path:
    sys.path.append(script_dir)

from latex_invoices import invoice_to_lco

class TestLatexInvoices(TestCase):
    def setUp(self):
        self.invoice = MagicMock()
        self.owner = MagicMock()
        self.addr = MagicMock()

        self.invoice.GetOwner.return_value = self.owner
        self.owner.GetAddr.return_value = self.addr

        self.owner.GetName.return_value = "Owner Name"
        self.addr.GetName.return_value = "Addr Name"
        self.addr.GetAddr1.return_value = "Addr 1"
        self.addr.GetAddr2.return_value = "Addr 2"
        self.addr.GetAddr3.return_value = "Addr 3"
        self.addr.GetAddr4.return_value = "Addr 4"

        self.invoice.GetID.return_value = "INV-123"

        self.date_posted = datetime(2023, 1, 1)
        self.date_due = datetime(2023, 1, 31)
        self.invoice.GetDatePosted.return_value = self.date_posted
        self.invoice.GetDateDue.return_value = self.date_due

        self.entry_instance = MagicMock()
        self.entry_instance.GetDescription.return_value = "Item Description"

        price = MagicMock()
        price.to_double.return_value = 100.0
        self.entry_instance.GetInvPrice.return_value = price

        quantity = MagicMock()
        quantity.num.return_value = 2
        quantity.denom.return_value = 1
        self.entry_instance.GetQuantity.return_value = quantity

        self.invoice.GetEntries.return_value = [self.entry_instance]

        # Mock locale.currency and locale.setlocale globally for tests
        self.locale_patcher = patch('locale.currency', return_value="100,00")
        self.setlocale_patcher = patch('locale.setlocale')
        self.mock_currency = self.locale_patcher.start()
        self.mock_setlocale = self.setlocale_patcher.start()

    def tearDown(self):
        self.locale_patcher.stop()
        self.setlocale_patcher.stop()

    def test_invoice_to_lco_basic(self):
        """Test the function's output with a standard invoice."""
        result = invoice_to_lco(self.invoice)
        self.assertIsInstance(result, str)
        self.assertIn("\\ProvidesFile{data.lco}", result)
        self.assertIn("INV-123", result)
        self.assertIn("01.01.2023", result)
        self.assertIn("31.01.2023", result)
        self.assertIn("Item Description", result)
        self.assertIn("Owner Name", result)
        self.assertIn("Addr Name", result)
        self.assertIn("Addr 1", result)
        self.assertIn("2", result) # Quantity
        self.assertIn("100,00", result) # Price

    def test_invoice_to_lco_no_entries(self):
        """Test behavior with no invoice entries."""
        self.invoice.GetEntries.return_value = []
        result = invoice_to_lco(self.invoice)
        self.assertIsInstance(result, str)
        self.assertIn("\\ProvidesFile{data.lco}", result)
        self.assertIn("INV-123", result)
        self.assertNotIn("\\Artikel", result)

    def test_invoice_to_lco_minimal_address(self):
        """Test behavior with missing address fields."""
        self.owner.GetName.return_value = ""
        self.addr.GetName.return_value = ""
        self.addr.GetAddr1.return_value = "Only Addr 1"
        self.addr.GetAddr2.return_value = ""
        self.addr.GetAddr3.return_value = ""
        self.addr.GetAddr4.return_value = ""

        result = invoice_to_lco(self.invoice)
        self.assertIsInstance(result, str)
        self.assertIn("Only Addr 1", result)
        self.assertNotIn("Owner Name", result)
        self.assertIn("{Only Addr 1}", result)

if __name__ == "__main__":
    main()
