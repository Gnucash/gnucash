# app_utils.py -- High level python wrapper for app-utils
#
# @author Christoph Holtermann <mail@c-holtermann.net>
## @file
#  @brief High level python wrapper for app-utils
#  @ingroup python_bindings 

from gnucash import _sw_app_utils

def gnc_get_current_session():
    from gnucash import Session
    return Session(instance=_sw_app_utils.gnc_get_current_session())

# further functions in _sw_app_utils
# _gnc_get_current_book is availabe through Session.get_book()
# _gnc_get_current_root_account is available through Session.get_root_account()
