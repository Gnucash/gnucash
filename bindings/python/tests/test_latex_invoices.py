# -*- coding: utf-8 -*-
##@file
# @brief Tests for latex_invoices.py
# @author Christoph Holtermann (c.holtermann (at) gmx.de)
# @date Jan 2024
#
# Copyright (C) 2024 Christoph Holtermann <c.holtermann@gmx.de>
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of
# the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, contact:
# Free Software Foundation           Voice:  +1-617-542-5942
# 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
# Boston, MA  02110-1301,  USA       gnu@gnu.org

import sys
import os
import io
import unittest
from unittest.mock import MagicMock, patch, mock_open

# Mocking modules that are not available in the test environment or are part of GnuCash bindings
mock_gnucash = MagicMock()
mock_gnucash.SessionOpenMode.SESSION_READ_ONLY = 1
mock_gnucash.SessionOpenMode.SESSION_NORMAL_OPEN = 2
sys.modules["gnucash"] = mock_gnucash
sys.modules["gnucash.gnucash_business"] = MagicMock()
sys.modules["str_methods"] = MagicMock()

# Mock gncinvoicefkt and ensure it has get_all_invoices
mock_gncinvoicefkt = MagicMock()
# We need to make sure get_all_invoices is available for "from gncinvoicefkt import *"
# However, import * doesn't work well with MagicMock.
# We'll manually inject it into latex_invoices if needed, or better,
# mock the functions before importing.
sys.modules["gncinvoicefkt"] = mock_gncinvoicefkt

# Mock IPython and its version info to avoid the TypeError
mock_ipython = MagicMock()
mock_ipython.version_info = (7, 0, 0)
sys.modules["IPython"] = mock_ipython
sys.modules["IPython.terminal"] = MagicMock()
sys.modules["IPython.terminal.ipapp"] = MagicMock()

# Add example_scripts to path so we can import latex_invoices
script_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'example_scripts'))
if script_dir not in sys.path:
    sys.path.append(script_dir)

# Now we can import the module to test
import latex_invoices

# If from gncinvoicefkt import * didn't work as expected with MagicMock
if not hasattr(latex_invoices, 'get_all_invoices'):
    latex_invoices.get_all_invoices = MagicMock()

class TestLatexInvoices(unittest.TestCase):

    def setUp(self):
        self.mock_invoice = MagicMock()
        self.mock_invoice.GetOwner.return_value.GetName.return_value = "Owner Name"

        mock_addr = MagicMock()
        mock_addr.GetName.return_value = "Addr Name"
        mock_addr.GetAddr1.return_value = "Addr 1"
        mock_addr.GetAddr2.return_value = "Addr 2"
        mock_addr.GetAddr3.return_value = "Addr 3"
        mock_addr.GetAddr4.return_value = "Addr 4"
        self.mock_invoice.GetOwner.return_value.GetAddr.return_value = mock_addr

        self.mock_invoice.GetID.return_value = "INV-123"

        self.mock_date = MagicMock()
        self.mock_date.strftime.return_value = "01.01.2023"
        self.mock_invoice.GetDatePosted.return_value = self.mock_date
        self.mock_invoice.GetDateDue.return_value = self.mock_date

        self.mock_entry = MagicMock()
        self.mock_entry.GetDescription.return_value = "Test Entry"
        self.mock_entry.GetInvPrice.return_value.to_double.return_value = 10.0
        self.mock_entry.GetQuantity.return_value.num.return_value = 1
        self.mock_entry.GetQuantity.return_value.denom.return_value = 1

        self.mock_invoice.GetEntries.return_value = [self.mock_entry]
        self.mock_invoice.__str__.return_value = "INV-123"

    def test_invoice_to_lco(self):
        # We need to mock Entry because of the type check: if type(ent) != Entry:
        # And we need to mock locale.currency
        with patch('latex_invoices.Entry') as mock_entry_class, \
             patch('locale.setlocale'), \
             patch('locale.currency', return_value="10,00 EUR"):

            # Make sure Entry(instance=ent) returns the mock entry
            mock_entry_class.return_value = self.mock_entry

            result = latex_invoices.invoice_to_lco(self.mock_invoice)

            self.assertIn("\\ProvidesFile{data.lco}", result)
            self.assertIn("INV-123", result)
            self.assertIn("Owner Name", result)
            self.assertIn("Addr 1", result)
            self.assertIn("Test Entry", result)
            self.assertIn("10,00", result)
            self.assertIn("01.01.2023", result)

    def test_main_help(self):
        with patch('sys.stdout', new_callable=io.StringIO) as mock_stdout:
            # Running main with --help should return 0 and print Usage
            result = latex_invoices.main(['latex_invoices.py', '--help'])
            self.assertEqual(result, 0)
            self.assertIn("Usage:", mock_stdout.getvalue())

        with patch('sys.stdout', new_callable=io.StringIO) as mock_stdout:
            # Running main with -h should also return 0 and print Usage
            result = latex_invoices.main(['latex_invoices.py', '-h'])
            self.assertEqual(result, 0)
            self.assertIn("Usage:", mock_stdout.getvalue())

    def test_main_invalid_args(self):
        with patch('sys.stderr', new_callable=io.StringIO) as mock_stderr, \
             patch('sys.stdout', new_callable=io.StringIO):
            # No input given should result in error
            result = latex_invoices.main(['latex_invoices.py'])
            self.assertEqual(result, 2)
            self.assertIn("Error: No input given !", mock_stderr.getvalue())

    @patch('latex_invoices.get_all_invoices')
    def test_main_list_invoices(self, mock_get_all_invoices):
        mock_get_all_invoices.return_value = [self.mock_invoice]

        with patch('sys.stdout', new_callable=io.StringIO) as mock_stdout:
            # -l flag to list invoices
            latex_invoices.main(['latex_invoices.py', '-l', 'file:///dummy.gnucash'])
            self.assertIn("listing all invoices", mock_stdout.getvalue())
            self.assertIn("0)", mock_stdout.getvalue())
            self.assertIn("INV-123", mock_stdout.getvalue())

    def test_main_session_failure(self):
        # Mock Session to raise an exception
        with patch('latex_invoices.gnucash.Session', side_effect=Exception("Failed to open")):
            with patch('sys.stdout', new_callable=io.StringIO) as mock_stdout:
                result = latex_invoices.main(['latex_invoices.py', 'file:///dummy.gnucash'])
                self.assertEqual(result, 2)
                self.assertIn("Problem opening input.", mock_stdout.getvalue())

    @patch('latex_invoices.get_all_invoices')
    @patch('builtins.open', new_callable=mock_open)
    def test_main_generate_latex(self, mock_file_open, mock_get_all_invoices):
        mock_get_all_invoices.return_value = [self.mock_invoice]

        # -n 0 to generate LaTeX for the first invoice
        with patch('latex_invoices.invoice_to_lco', return_value="MOCK LCO CONTENT"), \
             patch('sys.stdout', new_callable=io.StringIO):
            latex_invoices.main(['latex_invoices.py', '-n', '0', 'file:///dummy.gnucash'])

        mock_file_open.assert_called_with("data.lco", "w")
        mock_file_open().write.assert_called_with("MOCK LCO CONTENT")

    @patch('latex_invoices.get_all_invoices')
    @patch('builtins.open', new_callable=mock_open)
    def test_main_custom_output(self, mock_file_open, mock_get_all_invoices):
        mock_get_all_invoices.return_value = [self.mock_invoice]

        # -o custom.lco to use a custom output file
        with patch('latex_invoices.invoice_to_lco', return_value="MOCK LCO CONTENT"), \
             patch('sys.stdout', new_callable=io.StringIO):
            latex_invoices.main(['latex_invoices.py', '-n', '0', '-o', 'custom.lco', 'file:///dummy.gnucash'])

        mock_file_open.assert_called_with("custom.lco", "w")

    @patch('latex_invoices.gnucash.Session')
    @patch('latex_invoices.get_all_invoices')
    def test_main_ignore_lock(self, mock_get_all_invoices, mock_session):
        mock_get_all_invoices.return_value = []
        with patch('sys.stdout', new_callable=io.StringIO):
            latex_invoices.main(['latex_invoices.py', '-f', 'file:///dummy.gnucash'])

        # SessionOpenMode.SESSION_READ_ONLY is mocked as 1
        mock_session.assert_called_once_with('file:///dummy.gnucash', 1)

    @patch('latex_invoices.TerminalIPythonApp')
    @patch('latex_invoices.get_all_invoices')
    def test_main_ipshell(self, mock_get_all_invoices, mock_ipapp):
        mock_get_all_invoices.return_value = []
        with patch('sys.stdout', new_callable=io.StringIO):
            latex_invoices.main(['latex_invoices.py', '-i', 'file:///dummy.gnucash'])

        mock_ipapp.instance().initialize.assert_called_once()
        mock_ipapp.instance().start.assert_called_once()

if __name__ == "__main__":
    unittest.main()
