 ####################################################################
 # runtest.py -- Gnucash testcases                                  #
 # Copyright (C) 2007 Ahmed Sayed Hassan, <ahmadsayed83@yahoo.com>  #
 #                                                                  #
 # This program is free software; you can redistribute it and/or    #
 # modify it under the terms of the GNU General Public License as   #
 # published by the Free Software Foundation; either version 2 of   #
 # the License, or (at your option) any later version.              #
 #                                                                  #
 # This program is distributed in the hope that it will be useful,  #
 # but WITHOUT ANY WARRANTY; without even the implied warranty of   #
 # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    #
 # GNU General Public License for more details.                     #
 #                                                                  #
 # You should have received a copy of the GNU General Public License#
 # along with this program; if not, contact:                        #
 #                                                                  #
 # Free Software Foundation           Voice:  +1-617-542-5942       #
 # 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       #
 # Boston, MA  02110-1301,  USA       gnu@gnu.org                   #
 #                                                                  #
 ####################################################################

__author__ = "Ahmed Sayed <ahmadsayed83@yahoo.com>"

import unittest
from dogtail.utils import run
from os import system
from GnuCash import *
import GnuCash
import types
import sys
from validator import *

def appstate (func):
    """ Default application state when start run the application when done close it """
    def wrapper(* args, **kwargs):
        run('gnucash')
        sleep (10)
        gnuCash = GnuCashApp()
        gnuCash.dismiss_all_dialogs()

        func(*args, **kwargs)
        gnuCash.close_without_saving()
    return wrapper


class SimpleTest(unittest.TestCase):
    """ Test the Basic Gnucash functionality """
    def setUp(self):
        cleanup_all()

    def tearDown(self):
        ungraceful_close()


    def test_start_gnucash_first_time(self):
        """ Clean all gnucash data in gconf and start it then close it without saving """
        run('gnucash')
        sleep (10)
        gnuCash = GnuCashApp()
        gnuCash.dismiss_all_dialogs()
        gnuCash.close_without_saving()

    def test_start_gnucash_tip_of_day(self):
        """  Start gnucash check if Tip of days appear properly. """
        #cleanup_all()
        run('gnucash')
        sleep (10)
        gnuCash = GnuCashApp()
        # TODO : move Tip of Day Dialog to wrapper
        tip_of_day_dlg = gnuCash.findChild(predicate.GenericPredicate(roleName='dialog', name='GnuCash Tip Of The Day'))
        if tip_of_day_dlg == None:
            raise Exception()
        gnuCash.dismiss_all_dialogs()

    def test_disable_tip_of_day(self):
        """ test Disable tip of day open gnucash """
        run('gnucash')
        sleep(10)
        gnuCash = GnuCashApp()
        # TODO : move Tip of Day Dialog to wrapper
        tip_of_day_dlg = \
        gnuCash.findChild\
        (predicate.GenericPredicate(roleName='dialog', \
        name='GnuCash Tip Of The Day'))
        show_at_startup_cb = \
        tip_of_day_dlg.findChild\
        (predicate.GenericPredicate(roleName='check box', \
        name='Show tips at startup'))
        show_at_startup_cb.click()
        gnuCash.dismiss_all_dialogs()
        run('gnucash')
        sleep(10)
        gnuCash = GnuCashApp()
        tip_of_day_dlg = \
        gnuCash.findChild(predicate.GenericPredicate(roleName='dialog', \
        name='GnuCash Tip Of The Day'), \
        requireResult=False)
        if tip_of_day_dlg != None:
            raise Exception()


class DialogTest(unittest.TestCase):
    """ 
    Test that dialog loaded as expected 
    Also this test show if the wrappers are defined properly
    """
    def setUp(self):
        cleanup_all()
        run('gnucash')
        sleep (10)
        gnuCash = GnuCashApp()
        gnuCash.dismiss_all_dialogs()
        gnuCash.add_new_account_page()

    def tearDown(self):
        gnuCash = GnuCashApp()
        gnuCash.close_without_saving()

    def test_invoke_FindCustomer(self):
       findcustomer = FindCustomer()
       findcustomer.invoke()
       findcustomer.dismiss()

    def test_invoke_FindInvoice(self):
       findinvoice = FindInvoice()
       findinvoice.invoke()
       findinvoice.dismiss()

    def test_invoke_FindJob(self):
       findjob = FindJob()
       findjob.invoke()
       findjob.dismiss()

    def test_invoke_FindTransaction(self):
       findtransaction = FindTransaction()
       findtransaction.invoke()
       findtransaction.dismiss()

    def test_invoke_FindVendor(self):
       findvendor = FindVendor()
       findvendor.invoke()
       findvendor.dismiss()

    def test_invoke_FindVendorJob(self):
       findvendorjob = FindVendorJob()
       findvendorjob.invoke()
       findvendorjob.dismiss()

    def test_invoke_NewAccount(self):
       newaccount = NewAccount()
       newaccount.invoke()
       newaccount.dismiss()

    def test_invoke_NewCustomer(self):
       newcustomer = NewCustomer()
       newcustomer.invoke()
       newcustomer.dismiss()

    def test_invoke_NewInvoice(self):
       newinvoice = NewInvoice()
       newinvoice.invoke()
       newinvoice.dismiss()

    def test_invoke_NewJob(self):
       newjob = NewJob()
       newjob.invoke()
       newjob.dismiss()

    def test_invoke_NewVendor(self):
       newvendor = NewVendor()
       newvendor.invoke()
       newvendor.dismiss()

    def test_invoke_NewVendorJob(self):
       newvendorjob = NewVendorJob()
       newvendorjob.invoke()
       newvendorjob.dismiss()

    def test_invoke_Prefernces(self):
       preferences = Preferences()
       preferences.invoke()
       preferences.dismiss()

    def test_invoke_ProcessPayment(self):
       processpayment = ProcessPayment()
       processpayment.invoke()
       processpayment.dismiss()

    def test_invoke_ResetWarnings(self):
       resetwarnings = ResetWarnings()
       resetwarnings.invoke()
       resetwarnings.dismiss()

    def test_invoke_SetupAccountingPeriods(self):
        """ TODO: Setup Accounting Periods start wizard the roleName is frame not dialog """
        setupaccountingperiods = SetupAccountingPeriods()
        setupaccountingperiods.invoke()
        setupaccountingperiods.dismiss()

    def test_invoke_StyleSheets(self):
       stylesheets = StyleSheets()
       stylesheets.invoke()
       stylesheets.dismiss()

    def test_invoke_TaxOption(self):
       taxoption = TaxOption()
       taxoption.invoke()

class TestDialogs(unittest.TestCase):
    """ Test Each dialog functionality """
    def setUp(self):
        cleanup_all()
        run('gnucash')
        sleep(20)
        gnuCash = GnuCashApp()
        gnuCash.dismiss_all_dialogs()

    def tearDown(self):
        gnuCash = GnuCashApp()
        gnuCash.close_without_saving()

    def test_add_remove_style_sheets(self):
        """ Test Adding new Style Sheet """
        gnucash = GnuCashApp()
        style_sheet = StyleSheets()
        style_sheet.invoke()
        style_sheet.new_btn.click()
        new_style_dlg = gnucash.findChild(predicate.GenericPredicate(roleName='dialog', name='New Style Sheet'))
        new_style = NewStyleSheet(new_style_dlg)
        new_style.style_name = "test"
        new_style.template = "Fancy"
        new_style.accept()
        sleep(5)
        html_style_dlg_list = gnucash.findChildren(predicate.GenericPredicate(roleName='dialog'))
        html_style_dlg = None
        for dialog in html_style_dlg_list:
            result = re.match('HTML Style Sheet Properties: .', dialog.name)
            if result != None:
                print dialog.name
                html_style_dlg = HTMLStyleSheet(dialog)
        html_style_dlg.defaults_btn.click()
        html_style_dlg.accept()
        # validate the the whole table 
        validate_node(style_sheet.style_sheets_table, 'test_style_sheets')
        test_cell = style_sheet.style_sheets_table.findChild\
        (predicate.GenericPredicate(roleName='table cell', name='test'))
        test_cell.doAction('activate')
        style_sheet.delete_btn.click()
        # after removing the node confirm that the state resotred to the previous State before Adding
        self.assertEquals(validate_node(style_sheet.style_sheets_table, 'test_style_sheets_2'), EXIT_SUCCESS)
        style_sheet.dismiss()


class Business(unittest.TestCase):
    """ Test  Business related dialogs Add/Find object """
    def setUp(self):
        cleanup_all()
        run('gnucash')
        sleep(20)
        gnuCash = GnuCashApp()
        gnuCash.dismiss_all_dialogs()

    def tearDown(self):
        gnuCash = GnuCashApp()
        gnuCash.close_without_saving()

    def add_new_customers(self):
        """ normal function used to add the list of customers to be processed
            list = [
                [ 
                    [data in the customer tab 1],
                    [data in the shipping address tab 1]
                ],
                [
                    [data in the customer tab 2],
                    [data in the shipping address tab 2]
                ],
            ]
            TODO: anyone else feel it is relatively complex.
        """
        customer_info_list = [
        [
        ['ABC Corp', 'Bob McBob', '123 First Ave.', \
        'Somecity, SS 12345', '515-234-5678','515-235-5679', \
        'abc@abc.com','Bob McBobs, Sales Dept.'],
        ['Shipping Contact1', '123 First Ave.', \
        'Somecity, SS 12345', '', '', '515-234-5678','515-235-5679', \
        'abc@abc.com']
        ],
        [
        ['ABC2 Corp', 'Bob2 McBob2', '123 First Ave.', \
        'Somecity, SS 12345', '515-234-5678','515-235-5679', \
        'abc@abc.com','Bob McBobs, Sales Dept.'],
        ['Shipping Contact2', '123 First Ave.', \
        'Somecity, SS 12345', '', '', '515-234-5678','515-235-5679', \
        'abc@abc.com']
        ],
        [
        ['ABC3 Corp', 'Bob3 McBob3', '123 First Ave.', \
        'Somecity, SS 12345', '515-234-5678','515-235-5679', \
        'abc@abc.com','Bob McBobs, Sales Dept.'],
        ['Shipping Contact3', '123 First Ave.', \
        'Somecity, SS 12345', '', '', '515-234-5678','515-235-5679', \
        'abc@abc.com']
        ],
        [
        ['ABC4 Corp', 'Bob4 McBob4', '123 First Ave.', \
        'Somecity, SS 12345', '515-234-5678','515-235-5679', \
        'abc@abc.com','Bob McBobs, Sales Dept.'],
        ['Shipping Contact4', '123 First Ave.', \
        'Somecity, SS 12345', '', '', '515-234-5678','515-235-5679', \
        'abc@abc.com']
        ],
        ]
        for customer_info in customer_info_list:
            new_customer = NewCustomer()
            new_customer.invoke()
            new_customer.add_new_customer(*customer_info[0])
            new_customer.add_new_shipping_address(*customer_info[1])
            new_customer.accept()

    def add_new_vendors(self):
        """ normal function used to add the list of vendors to be processed
            list = [
                [ 
                    [data in the vendor tab 1],
                ],
                [
                    [data in the vendor tab 2],
                ],
            ]
        """
        vendor_info_list = [
        [
        ['ABC Corp', 'Bob McBob', '123 First Ave.', \
        'Somecity, SS 12345', '515-234-5678','515-235-5679', \
        'abc@abc.com','Bob McBobs, Sales Dept.'],
        ],
        [
        ['ABC2 Corp', 'Bob2 McBob2', '123 First Ave.', \
        'Somecity, SS 12345', '515-234-5678','515-235-5679', \
        'abc@abc.com','Bob McBobs, Sales Dept.'],
        ],
        [
        ['ABC3 Corp', 'Bob3 McBob3', '123 First Ave.', \
        'Somecity, SS 12345', '515-234-5678','515-235-5679', \
        'abc@abc.com','Bob McBobs, Sales Dept.'],
        ],
        [
        ['ABC4 Corp', 'Bob4 McBob4', '123 First Ave.', \
        'Somecity, SS 12345', '515-234-5678','515-235-5679', \
        'abc@abc.com','Bob McBobs, Sales Dept.'],
        ],
        ]
        for vendor_info in vendor_info_list:
            new_vendor = NewVendor()
            new_vendor.invoke()
            new_vendor.add_new_vendor(*vendor_info[0])
            new_vendor.accept()

    def test_add_new_customer(self):
        """ Test adding new cusotmers """
        self.add_new_customers()
        # Validation by using Find and dump the result table
        find_customer = FindCustomer()
        find_customer.invoke()
        find_customer.text_fields_list[0].text = "."
        find_customer.search_by_list[0].combovalue = 'Company Name'
        find_customer.search_by_list[1].combovalue = 'matches regex'
        find_customer.find()
        self.assertEquals(validate_node(find_customer.result_table, 'test_add_new_customer'), EXIT_SUCCESS)

    def test_find_customer(self):
        """ Test Find Customers 
        search_criteria = [
                [
                    [ search_by, search_type, search_term, is case insestive ]
                    [ search criteria 2]
                ],
                [
                    [serch critieria 1 for second testcase]
                ],
            ] """
        self.add_new_customers()
        search_criteria_list = [
                [
                    ['Company Name', 'matches regex', '.', False], # Get all results
                ],
                [
                    ['Company Name', 'matches regex', '[a-z][0-9]', False],
                ],
                [
                    ['Company Name', 'matches regex', '[a-z][3-9]', False],
                ],
                [
                    ['Company Name', 'matches regex', '.', False],
                    ['Customer ID', 'contains', '1', False],
                ],
                [
                    ['Billing Contact', 'matches regex', 'Bob2', False],
                ],
                [
                    ['Shipping Contact', 'matches regex', '.', False],
                ],
                [
                    ['Shipping Contact', 'matches regex', '[a-z][2-9]', False],
                    ['Customer ID', 'contains', '2', False],
                ]


            ]
        count = 0
        for test_no, search_criteria in enumerate(search_criteria_list):
            find_customer = FindCustomer()
            find_customer.invoke()
            for i,criteria in enumerate(search_criteria):
                find_customer.search_by_list[0].combovalue = criteria[0]
                find_customer.search_by_list[1].combovalue = criteria[1]
                find_customer.text_fields_list[0].text = criteria[2]
                if criteria[3]:
                    find_customer.is_case_insensitive_list[0].click()
                if i < (len(search_criteria)-1):
                    find_customer.add_criteria()
            find_customer.find()
            try:
                self.assertEquals(\
                validate_node(find_customer.result_table, \
                'test_find_new_customer_%d' % test_no), \
                EXIT_SUCCESS)
            except:
                count = count + 1
            find_customer.dismiss()
        self.assertEquals(count, 0)

    def test_add_new_vendor(self):
        """ Test Add new vendors """
        self.add_new_vendors()
        # Validation by using Find and dump the result table
        find_vendor= FindVendor()
        find_vendor.invoke()
        find_vendor.text_fields_list[0].text = "."
        find_vendor.search_by_list[0].combovalue = 'Company Name'
        find_vendor.search_by_list[1].combovalue = 'matches regex'
        find_vendor.find()
        self.assertEquals(validate_node(find_vendor.result_table, 'test_add_new_vendor'), EXIT_SUCCESS)

    def test_find_vendor(self):
        """ Test Find Customer 
        search_criteria = [
                [
                    [ search_by, search_type, search_term, is case insestive ]
                    [ search criteria 2]
                ],
                [
                    [serch critieria 1 for second testcase]
                ],
            ] """
        self.add_new_vendors()
        search_criteria_list = [
                [
                    ['Company Name', 'matches regex', '.', False], # Get all results
                ],
                [
                    ['Company Name', 'matches regex', '[a-z][0-9]', False],
                ],
                [
                    ['Company Name', 'matches regex', '[a-z][3-9]', False],
                ],
                [
                    ['Company Name', 'matches regex', '.', False],
                    ['Vendor ID', 'contains', '1', False],
                ],
                [
                    ['Billing Contact', 'matches regex', 'Bob2', False],
                ],

            ]
        count = 0
        for test_no, search_criteria in enumerate(search_criteria_list):
            find_vendor = FindVendor()
            find_vendor.invoke()
            for i,criteria in enumerate(search_criteria):
                find_vendor.search_by_list[0].combovalue = criteria[0]
                find_vendor.search_by_list[1].combovalue = criteria[1]
                find_vendor.text_fields_list[0].text = criteria[2]
                if criteria[3]:
                    find_vendor.is_case_insensitive_list[0].click()
                if i < (len(search_criteria)-1):
                    find_vendor.add_criteria()
            find_vendor.find()
            try:
                self.assertEquals(\
                validate_node(find_vendor.result_table, \
                'test_find_vendor_%d' % test_no), \
                EXIT_SUCCESS)
            except:
                count = count + 1
            find_vendor.dismiss()
        self.assertEquals(count, 0)

    def test_add_new_invoice(self):
        """ Test Add new Invoice """
        self.add_new_customers()
        # add new invoice
        new_invoice = NewInvoice()
        new_invoice.invoke()
        new_invoice.billing_id = "ABC Purchase Order # 12988"
        new_invoice.notes = "Your Personal notes goes here\nNotes do not appear on printed invoices"
        new_invoice.customer = 'ABC Corp'
        new_invoice.accept()

        # Validation by using Find and dump the result table
        find_invoice = FindInvoice()
        find_invoice.invoke()
        find_invoice.text_fields_list[0].text = "."
        find_invoice.search_by_list[0].combovalue = 'Invoice ID'
        find_invoice.search_by_list[1].combovalue = 'matches regex'
        find_invoice.find()
        self.assertEquals(validate_node(find_invoice.result_table, 'test_add_new_invoice'), EXIT_SUCCESS)

    def test_add_new_bill(self):
        """ Test Add new Bills """
        self.add_new_vendors()

        new_bill = NewBill()
        new_bill.invoke()
        new_bill.vendor = 'ABC2 Corp'
        new_bill.notes = 'Additional notes about the bill go here'
        new_bill.accept()

        # Validation by using Find and dump the result table
        find_bill = FindBill()
        find_bill.invoke()
        find_bill.text_fields_list[0].text = "."
        find_bill.search_by_list[0].combovalue = 'Bill ID'
        find_bill.search_by_list[1].combovalue = 'matches regex'
        find_bill.find()
        self.assertEquals(validate_node(find_bill.result_table, 'test_add_new_bill'), EXIT_SUCCESS)

class TestWizard(unittest.TestCase):
    """ Will be a scenarios will walk throw different pathes of the wizads """
    def setUp(self):
        cleanup_all()
        run('gnucash')
        sleep(20)
        gnuCash = GnuCashApp()
        gnuCash.dismiss_all_dialogs()

    def tearDown(self):
        gnuCash = GnuCashApp()
        gnuCash.close_without_saving()

    def new_account_wizard(self, category_list):
        """ FIXME: This method  does not work properly, if category list contains multiple items"""
        gnucash = GnuCashApp()
        gnucash.menu('File').menu('New').menuItem('New File').click()

        focus.application('gnucash')
        click('Continue Without Saving', roleName='push button')

        duride_frame = gnucash.findChild(\
        predicate.GenericPredicate(roleName='frame', \
        name='New Account Hierarchy Setup'))
        new_account_setup = NewAccountSetup(duride_frame)
        new_account_setup.clickForward()
        new_account_setup.clickForward()
        new_account_setup.set_account_category(category_list)
        new_account_setup.clickForward()
        new_account_setup.clickForward()
        new_account_setup.clickApply()

    def test_default_wizard(self):
        """ Test Only the default wizards """
        gnucash = GnuCashApp()
        gnucash.menu('File').menu('New').menuItem('New File').click()

        focus.application('gnucash')
        click('Continue Without Saving', roleName='push button')

        duride_frame = gnucash.findChild(\
        predicate.GenericPredicate(roleName='frame', \
        name='New Account Hierarchy Setup'))
        new_account_setup = NewAccountSetup(duride_frame)
        new_account_setup.clickForward()
        new_account_setup.clickForward()
        new_account_setup.clickForward()
        new_account_setup.clickForward()
        new_account_setup.clickApply()

        # Validation
        account_tab = gnucash.tab('Accounts')
        self.assertEquals(validate_node(account_tab, 'test_default_wizard'), EXIT_SUCCESS)

    def test_new_account_wizard_select_all(self):
        """ Test the new account wizard and select all categorize """
        gnucash = GnuCashApp()
        gnucash.menu('File').menu('New').menuItem('New File').click()

        focus.application('gnucash')
        click('Continue Without Saving', roleName='push button')

        duride_frame = gnucash.findChild(\
        predicate.GenericPredicate(roleName='frame', \
        name='New Account Hierarchy Setup'))
        new_account_setup = NewAccountSetup(duride_frame)
        new_account_setup.clickForward()
        new_account_setup.clickForward()
        new_account_setup.currentPage().button('Select All').click()
        new_account_setup.clickForward()
        new_account_setup.clickForward()
        new_account_setup.clickApply()

        # Validation
        account_tab = gnucash.tab('Accounts')
        self.assertEquals(validate_node(account_tab, 'test_new_account_wizard_select_all'), EXIT_SUCCESS)

    def test_new_account_wizard_a_simple_checkbook(self):
        gnucash = GnuCashApp()
        """ Test Creating A Simple Checkbook """ 
        self.new_account_wizard(['A Simple Checkbook'])
        #validation
        account_tab = gnucash.tab('Accounts')
        self.assertEquals(validate_node(account_tab, 'test_new_account_wizard_a_simple_checkbook'), EXIT_SUCCESS)

    def test_new_account_wizard_business_accounts(self):
        gnucash = GnuCashApp()
        """ Test Creating Business Accounts """ 
        self.new_account_wizard(['Business Accounts'])
        #validation
        account_tab = gnucash.tab('Accounts')
        self.assertEquals(validate_node(account_tab, 'test_new_account_wizard_business_accounts'), EXIT_SUCCESS)

    def test_new_account_wizard_car_loan(self):
        gnucash = GnuCashApp()
        """ Test Creating Car Loan """ 
        self.new_account_wizard(['Car Loan'])
        #validation
        account_tab = gnucash.tab('Accounts')
        self.assertEquals(validate_node(account_tab, 'test_new_account_wizard_car_loan'), EXIT_SUCCESS)

    def test_new_account_wizard_cd_and_money_market(self):
        gnucash = GnuCashApp()
        """ Test Creating CD and Money Market """ 
        self.new_account_wizard(['CD and Money Market'])
        #validation
        account_tab = gnucash.tab('Accounts')
        self.assertEquals(validate_node(account_tab, 'test_new_account_wizard_cd_and_money_market'), EXIT_SUCCESS)

    def test_new_account_wizard_childcare_expenses(self):
        gnucash = GnuCashApp()
        """ Test Creating Childcare Expenses """ 
        self.new_account_wizard(['Childcare Expenses'])
        #validation
        account_tab = gnucash.tab('Accounts')
        self.assertEquals(validate_node(account_tab, 'test_new_account_wizard_childcare_expenses'), EXIT_SUCCESS)

    def test_new_account_wizard_common_accounts(self):
        gnucash = GnuCashApp()
        """ Test Creating Common Accounts """ 
        self.new_account_wizard(['Common Accounts'])
        #validation
        account_tab = gnucash.tab('Accounts')
        self.assertEquals(validate_node(account_tab, 'test_new_account_wizard_common_accounts'), EXIT_SUCCESS)

    def test_new_account_wizard_education_loan(self):
        gnucash = GnuCashApp()
        """ Test Creating Education Loan """ 
        self.new_account_wizard(['Education Loan'])
        #validation
        account_tab = gnucash.tab('Accounts')
        self.assertEquals(validate_node(account_tab, 'test_new_account_wizard_education_loan'), EXIT_SUCCESS)

    def test_new_account_wizard_fixed_assets(self):
        gnucash = GnuCashApp()
        """ Test Creating Fixed Assets """ 
        self.new_account_wizard(['Fixed Assets'])
        #validation
        account_tab = gnucash.tab('Accounts')
        self.assertEquals(validate_node(account_tab, 'test_new_account_wizard_fixed_assets'), EXIT_SUCCESS)

    def test_new_account_wizard_home_mortgage_loan(self):
        gnucash = GnuCashApp()
        """ Test Creating Home Mortgage Loan """ 
        self.new_account_wizard(['Home Mortgage Loan'])
        #validation
        account_tab = gnucash.tab('Accounts')
        self.assertEquals(validate_node(account_tab, 'test_new_account_wizard_home_mortgage_loan'), EXIT_SUCCESS)

    def test_new_account_wizard_homeowner_expenses(self):
        gnucash = GnuCashApp()
        """ Test Creating Homeowner Expenses """ 
        self.new_account_wizard(['Homeowner Expenses'])
        #validation
        account_tab = gnucash.tab('Accounts')
        self.assertEquals(validate_node(account_tab, 'test_new_account_wizard_homeowner_expenses'), EXIT_SUCCESS)

    def test_new_account_wizard_investment_accounts(self):
        gnucash = GnuCashApp()
        """ Test Creating Investment Accounts """ 
        self.new_account_wizard(['Investment Accounts'])
        #validation
        account_tab = gnucash.tab('Accounts')
        self.assertEquals(validate_node(account_tab, 'test_new_account_wizard_investment_accounts'), EXIT_SUCCESS)

    def test_new_account_wizard_other_loans(self):
        gnucash = GnuCashApp()
        """ Test Creating Other Loans """ 
        self.new_account_wizard(['Other Loans'])
        #validation
        account_tab = gnucash.tab('Accounts')
        self.assertEquals(validate_node(account_tab, 'test_new_account_wizard_other_loans'), EXIT_SUCCESS)

    def test_new_account_wizard_renter_expenses(self):
        gnucash = GnuCashApp()
        """ Test Creating Renter Expenses """ 
        self.new_account_wizard(['Renter Expenses'])
        #validation
        account_tab = gnucash.tab('Accounts')
        self.assertEquals(validate_node(account_tab, 'test_new_account_wizard_renter_expenses'), EXIT_SUCCESS)

    def test_new_account_wizard_retirement_accounts(self):
        gnucash = GnuCashApp()
        """ Test Creating Retirement Accounts """ 
        self.new_account_wizard(['Retirement Accounts'])
        #validation
        account_tab = gnucash.tab('Accounts')
        self.assertEquals(validate_node(account_tab, 'test_new_account_wizard_retirement_accounts'), EXIT_SUCCESS)

    def test_new_account_wizard_spouse_income(self):
        gnucash = GnuCashApp()
        """ Test Creating Spouse Income """ 
        self.new_account_wizard(['Spouse Income'])
        #validation
        account_tab = gnucash.tab('Accounts')
        self.assertEquals(validate_node(account_tab, 'test_new_account_wizard_spouse_income'), EXIT_SUCCESS)

    def test_new_account_wizard_spouse_retirement_accounts(self):
        gnucash = GnuCashApp()
        """ Test Creating Spouse Retirement Accounts """ 
        self.new_account_wizard(['Spouse Retirement Accounts'])
        #validation
        account_tab = gnucash.tab('Accounts')
        self.assertEquals(validate_node(account_tab, 'test_new_account_wizard_spouse_retirement_accounts'), EXIT_SUCCESS)

class TestPreferences(unittest.TestCase):
    """  Will be a set of scenarios that will detect the affet of changing preferences """
    pass

class TestReports(unittest.TestCase):
    """ 
    To Test the reporting I'll follow the following
        1 - Open a presaved datafile
        2 - Creat the report
        3 - Export the report to HTML file
        4 - Sed the actual file and reference file to remove the current date.
        5 - apply diff
    """
    def setUp(self):
        """ a setup  for the test case in this type of test just run gnucash and go dismiss first dialog """
        cleanup_all()
        run('gnucash')
        sleep (20)
        gnuCash = GnuCashApp()
        gnuCash.dismiss_all_dialogs()
        gnuCash.open_data_file('mytest2')

    def tearDown(self):
        """ just close gnucash without Saving """
        gnuCash = GnuCashApp()
        try:
            gnuCash.close_without_saving()
        except:
            pass

    def validate_report(self, testcase_name):
        """ Helper method to aid in validating the report """
        return validate_files(testcase_name, filter_command="\'/TABLE/,/\/TABLE/p\'")

    def test_account_summary(self):
        gnucash = GnuCashApp()
        gnucash.create_account_summary()
        export_report = ExportReport()
        export_report.invoke()
        export_report.export_report(gnucash.cwd_path + '/act/test_account_summary_act')
        sleep(5)
        self.assertEquals(self.validate_report('test_account_summary'), \
        EXIT_SUCCESS)

    def test_transaction_report(self):
        gnucash = GnuCashApp()
        gnucash.create_transaction_report()
        export_report = ExportReport()
        export_report.invoke()
        export_report.export_report(gnucash.cwd_path + '/act/test_transaction_report_act')
        sleep(5)
        self.assertEquals(self.validate_report('test_transaction_report'), \
        EXIT_SUCCESS)

class ScenarioTest(unittest.TestCase):
    """ Test a compelete Scenario """

    def transaction(self, account):
        """ Helper method to open the account register tab """
        config.childrenLimit = 500
        gnucash = GnuCashApp()
        accounts = gnucash.tab('Accounts')
        account = accounts.findChild(predicate.GenericPredicate(roleName='table cell', name=account))
        account.doAction('activate')

    def setUp(self):
        """ a setup  for the test case in this type of test just run gnucash and go dismiss first dialog """
        cleanup_all()
        run('gnucash')
        sleep (20)
        gnuCash = GnuCashApp()
        gnuCash.dismiss_all_dialogs()

    def tearDown(self):
        """ just close gnucash without Saving """
        gnuCash = GnuCashApp()
        gnuCash.close_without_saving()

    def test_new_account_dialog(self):
        """ Test Add new accounts """
        gnucash = GnuCashApp()
        gnucash.add_new_account_page()
        account_list = [
                ['Asset', 'Asset'],
                ['Liability','Liability'],
                ['Equity', 'Equity'],
                ['Income', 'Income'],
                ['Expense', 'Expense'],
                ['Checking', 'Bank', 'Asset'],
                ['Savings', 'Bank', 'Asset'],
                ['Visa', 'Credit Card', 'Liability'],
                ['Salary', 'Income', 'Income'],
                ['Phone', 'Expense', 'Expense'],
                ['Electricity', 'Expense', 'Expense'],
                ['Rent', 'Expense', 'Expense'],
                ['Groceries', 'Expense', 'Expense'],
                ['Taxes', 'Expense', 'Expense'],
                ['Opening Balance', 'Equity', 'Equity']
            ]
        for account in account_list:
            gnucash.add_account(*account)
        # Validation
        account_tab = gnucash.tab('Accounts')
        self.assertEquals(validate_node(account_tab, 'test_new_account_dialog'), EXIT_SUCCESS)

    def test_perform_transaction(self):
        """ Call the previos test case and then perform some transaction 
            TODO: validatior not yet but it will be on the affect of the transaction by checking the account page not the register page
        """
        self.test_new_account_dialog()
        gnucash = GnuCashApp()
        self.transaction("Checking")
        reg = gnucash.tab("Checking").findChild(predicate.GenericPredicate(roleName='layered pane'))
        register = Register(reg)
        register.row = 0
        register.date = "07/15/2007"
        register.transfer = "Equity:Opening Balance"
        register.deposite = "1000"
        register.end_trans()

        self.transaction("Savings")
        reg = gnucash.tab("Savings").findChild(predicate.GenericPredicate(roleName='layered pane'))
        register = Register(reg)
        register.row = 0
        register.date = "07/15/2007"
        register.transfer = "Equity:Opening Balance"
        register.deposite = "1000"
        register.end_trans()

        self.transaction("Visa")
        reg = gnucash.tab("Visa").findChild(predicate.GenericPredicate(roleName='layered pane'))
        register = Register(reg)
        register.row = 0
        register.date = "07/15/2007"
        register.transfer = "Equity:Opening Balance"
        register.withdrawal = "500"
        register.end_trans()
        # Validation
        account_tab = gnucash.tab('Accounts')
        self.assertEquals(validate_node(account_tab, 'test_perform_transaction'), EXIT_SUCCESS)

    def test_perform_reconcilation(self):
        """ Test Reconcilation """
        gnucash = GnuCashApp()
        gnucash.open_data_file('mytest2')
        reconcile = gnucash.reconcile_account('Asset')
        reconcile.include_subaccount = True
        reconcile.accept()
        reconcileFrame = ReconcileFrame()
        self.assertEquals(\
        validate_node(reconcileFrame.funds_in, \
        'test_before_perform_reconcilation_funds_in'), EXIT_SUCCESS)
        self.assertEquals(\
        validate_node(reconcileFrame.funds_out, \
        'test_before_perform_reconcilation_funds_out'), EXIT_SUCCESS)
        reconcileFrame.select_all_funds_out()
        reconcileFrame.select_all_funds_in()
        reconcileFrame.finish()

        # Validation Done by opening the Reconcile frame and check Funds In and Funds Out Table are empty
        reconcile = gnucash.reconcile_account('Asset')
        reconcile.accept()
        reconcileFrame = ReconcileFrame()
        self.assertEquals(\
        validate_node(reconcileFrame.funds_in, \
        'test_after_perform_reconcilation_funds_in'), EXIT_SUCCESS)
        self.assertEquals(\
        validate_node(reconcileFrame.funds_out, \
        'test_after_perform_reconcilation_funds_out'), EXIT_SUCCESS)

    def test_accounts_receivable(self):
        """ Test accounts receivable Jobs not added yet"""
        gnucash = GnuCashApp()

        # Account setup
        gnucash.add_new_account_page()
        account_list = [
                ['Asset', 'Asset'],
                ['Income', 'Income'],
                ['Checking', 'Bank', 'Asset'],
                ['Accounts Receivable', 'A/Receivable', 'Asset'],
                ['Sales', 'Income', 'Income'],
            ]
        for account in account_list:
            gnucash.add_account(*account)

        # Company Registration
        book_options = BookOptions()
        book_options.invoke()
        book_options.company_name = 'ABC Corp'
        book_options.company_id = '12-1234455'
        book_options.accept()

        # add new customer 
        new_customer = NewCustomer()
        new_customer.invoke()
        new_customer.customer.company_name_txt.text = 'ABC Inc'
        new_customer.customer.name_txt.text = 'Bob McBob'
        new_customer.customer.address_1_txt.text = '123 First Ave.'
        new_customer.customer.address_2_txt.text = 'Somecity, SS 12345'
        new_customer.customer.phone_txt.text = '515-234-5678'
        new_customer.customer.fax_txt.text = '515-235-5679'
        new_customer.customer.email_txt.text = 'abc@abc.com'
        new_customer.customer.notes_txt.text = 'Bob McBobs, Sales Dept.'
        new_customer.accept()

        # add new invoice
        new_invoice = NewInvoice()
        new_invoice.invoke()
        new_invoice.billing_id = "ABC Purchase Order # 12988"
        new_invoice.notes = "Your Personal notes goes here\nNotes do not appear on printed invoices"
        new_invoice.customer = 'ABC Inc'
        new_invoice.accept()

        # Edit the Invoice
        my_tab =  gnucash.tab('Edit Invoice - 000001')

        edit_invoice = EditInvoice(my_tab)
        edit_invoice.invoice_register.date = '07/15/2007'
        edit_invoice.invoice_register.description = 'Nails'
        edit_invoice.invoice_register.action = 'Material'
        edit_invoice.invoice_register.income_account = 'Income:Sales'
        edit_invoice.invoice_register.quantity = '1,000.00'
        edit_invoice.invoice_register.unit_price = '0.10'


        edit_invoice.invoice_register.row = 1
        edit_invoice.invoice_register.date = '07/15/2007'
        edit_invoice.invoice_register.description = 'Hammer'
        edit_invoice.invoice_register.action = 'Material'
        edit_invoice.invoice_register.income_account = 'Income:Sales'
        edit_invoice.invoice_register.quantity = '1.00'
        edit_invoice.invoice_register.unit_price = '500.00'
        edit_invoice.invoice_register.discount = '5.00'

        gnucash.menu('Actions').menuItem('Enter').click()

        # Post to Asset:Accounts Receivable 
        gnucash.button('Post').click()
        question = Question()
        question.post_to_account = 'Asset:Accounts Receivable'
        question.accept()

        # close the edit invoice tab as a cleanup
        edit_invoice.invoice_register.end_trans()

        # Validation
        account_tab = gnucash.tab('Accounts')
        self.assertEquals(validate_node(account_tab, \
        'test_accounts_receivable'), EXIT_SUCCESS)

    def test_accounts_payable(self):

        gnucash = GnuCashApp()

        # Account setup
        gnucash.add_new_account_page()
        account_list = [
                ['Asset', 'Asset'],
                ['Liabilities', 'Liability'],
                ['Expenses', 'Expense'],
                ['Bank', 'Bank', 'Asset'],
                ['Accounts Payable', 'A/Payable', 'Liabilities'],
                ['AP Expenses', 'Expense', 'Expenses'],
            ]
        for account in account_list:
            gnucash.add_account(*account)

        # Company Registration
        book_options = BookOptions()
        book_options.invoke()
        book_options.company_name = 'BCA Corp'
        book_options.company_id = '12-1234455'
        book_options.accept()

        # add Three vendors
        new_vendor = NewVendor()
        new_vendor.invoke()
        new_vendor.vendor.company_name_txt.text = 'BCA Inc'
        new_vendor.vendor.name_txt.text = 'Joe Recievable'
        new_vendor.vendor.address_1_txt.text = '321 Second St.'
        new_vendor.vendor.address_2_txt.text = 'Someothercity, SC 54321'
        new_vendor.vendor.phone_txt.text = '322-555-2345'
        new_vendor.vendor.fax_txt.text = '322-555-2346'
        new_vendor.vendor.email_txt.text = 'bca@bca.com'
        new_vendor.vendor.notes_txt.text = 'Track any other comments here'
        new_vendor.accept()

        new_vendor = NewVendor()
        new_vendor.invoke()
        new_vendor.vendor.company_name_txt.text = 'Paper Supply Corporation'
        new_vendor.vendor.name_txt.text = 'Fred Smith'
        new_vendor.vendor.address_1_txt.text = '321 Second St.'
        new_vendor.vendor.address_2_txt.text = 'Someothercity, SC 54321'
        new_vendor.vendor.phone_txt.text = '322-555-2345'
        new_vendor.vendor.fax_txt.text = '322-555-2346'
        new_vendor.vendor.email_txt.text = 'bca@bca.com'
        new_vendor.vendor.notes_txt.text = 'Track any other comments here'
        new_vendor.accept()

        new_vendor = NewVendor()
        new_vendor.invoke()
        new_vendor.vendor.company_name_txt.text = 'XYZ Computer Sales'
        new_vendor.vendor.name_txt.text = 'Frank Reciever'
        new_vendor.vendor.address_1_txt.text = '321 Second St.'
        new_vendor.vendor.address_2_txt.text = 'Someothercity, SC 54321'
        new_vendor.vendor.phone_txt.text = '322-555-2345'
        new_vendor.vendor.fax_txt.text = '322-555-2346'
        new_vendor.vendor.email_txt.text = 'bca@bca.com'
        new_vendor.vendor.notes_txt.text = 'Track any other comments here'
        new_vendor.accept()

        # add new Bill
        new_bill = NewBill()
        new_bill.invoke()
        new_bill.vendor = 'Paper Supply Corporation'
        new_bill.notes = 'Additional notes about the bill go here'
        new_bill.accept()

        # Edit the Invoice

        my_tab =  gnucash.tab('Edit Bill - 000001')
        edit_invoice = EditInvoice(my_tab)
        edit_invoice.invoice_register.date = '07/15/2007'
        edit_invoice.invoice_register.description = 'LetterHead'
        edit_invoice.invoice_register.action = 'Material'
        edit_invoice.invoice_register.income_account = 'Expenses:AP Expenses'
        edit_invoice.invoice_register.quantity = '5.00'
        edit_invoice.invoice_register.unit_price = '15'
        gnucash.menu('Actions').menuItem('Enter').click()

        edit_invoice.invoice_register.row = 1
        edit_invoice.invoice_register.date = '07/15/2007'
        edit_invoice.invoice_register.description = 'Envelops'
        edit_invoice.invoice_register.action = 'Material'
        edit_invoice.invoice_register.income_account = 'Expenses:AP Expenses'
        edit_invoice.invoice_register.quantity = '500'
        edit_invoice.invoice_register.unit_price = '0.05'
        gnucash.menu('Actions').menuItem('Enter').click()
        # Post to Asset:Accounts Receivable 
        gnucash.button('Post').click()
        question = Question()
        question.post_to_account = 'Liabilities:Accounts Payable'
        question.accept()

        # close the edit invoice tab as a cleanup
        edit_invoice.invoice_register.end_trans()

        # Validation
        account_tab = gnucash.tab('Accounts')
        self.assertEquals(\
        validate_node(account_tab, \
        'test_accounts_payable'), EXIT_SUCCESS)

if __name__ == "__main__":
    config.childrenLimit = 1500
    os.system("rm act/*")
    os.system("rm projects_under_test/*")
    unittest.main()
