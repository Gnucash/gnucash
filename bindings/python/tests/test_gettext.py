# test gettext
#
# @date 2020-04-08
# @author Christoph Holtermann <mail@c-holtermann.net>

from unittest import TestCase, main
import gettext
import gnucash
from gnucash import _sw_core_utils

class MyException(Exception):
    pass

class TestGettext(TestCase):
    def test_import_gettext(self):
        import gettext

    def test_get_localedir(self):
        _localedir = _sw_core_utils.gnc_path_get_localedir()

    def test_translation(self):
        raise MyException({'_(""): ':_(""), '_sw_core_utils.gnc_path_get_localedir(): ':_sw_core_utils.gnc_path_get_localedir()})
        self.assertTrue("Project-Id-Version: GnuCash" in _(""))

if __name__ == '__main__':
    main()
