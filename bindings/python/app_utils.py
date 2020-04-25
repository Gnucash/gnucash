# app_utils.py -- High level python wrapper for app-utils
#
# @author Christoph Holtermann <mail@c-holtermann.net>
## @file
#  @brief High level python wrapper for app-utils
#  @ingroup python_bindings 
#
# further functions in _sw_app_utils that have not been included:
# _gnc_get_current_book is available through Session.get_book()
# _gnc_get_current_root_account is available through Book.get_root_account()


from gnucash import _sw_app_utils

def gnc_get_current_session():
    from gnucash import Session
    return Session(instance=_sw_app_utils.gnc_get_current_session())
