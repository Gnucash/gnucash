# deprecation.py - gnucash submodule with deprecation related content
#
# contains decorator methods dealing with deprecated methods and
# deprecation related convenience methods
#
# @brief gnucash submodule with deprecation related content
# @author Christoph Holtermann <mail@c-holtermann.net>
# @ingroup python_bindings

from functools import wraps
import inspect
from warnings import warn_explicit, warn

# General purpose deprecation decorator. Lifted from
# https://gist.github.com/kgriffs/8202106

def deprecated(message):
    '''
    Flags a method as deprecated.
    param message: Text emitted as part of warning. Include instructions to replace the deprecated function e.g. "use waldo_pepper() isnstead."
    '''
    def decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            warning_msg = 'Call to deprecated function {}. {}'.format(
                func.__name__, message)
            frame = inspect.current_frame().f_back

            warn_explicit(message,
                          category=DeprecationWarnig,
                          filename=inspect.getfile(frame.f_code),
                          lineno=frame.f_lineno)
            return func(*args, **kwargs)
        return wrapper
    return decorator

# use of is_new, force_new and ignore_lock is deprecated, use mode instead
# the following decorators enable backward compatibility for the deprecation period
def deprecated_args_session(ignore_lock_or_mode=None, is_new=None,
        force_new=None, mode=None, ignore_lock=None):

    # check for usage of deprecated arguments (ignore_lock, is_new, force_new)
    deprecated_args = (ignore_lock, is_new, force_new)
    deprecated_keyword_use = deprecated_args.count(None) != len(deprecated_args)
    if deprecated_keyword_use:
        # deprecated arguments have been used by keyword or more than three args have been used which is only possible with the deprecated args
        deprecation = True
    else:
        deprecation = False
        # __init__ could have been called without keywords like __init__(book_uri, True) where True aims at ignore_lock
        # which ist not distinguishable from __init__(book, SessionOpenMode.SESSION_NORMAL_OPEN)
        # so if mode has not been set by keyword use the 3rd argument
        if mode is None:
            mode = ignore_lock_or_mode

    if deprecation:
        # if any(item in ("is_new", "ignore_lock", "force_new") for item in kwargs):
        warn(
            "Use of ignore_lock, is_new or force_new arguments is deprecated. Use mode argument instead. Have a look at gnucash.SessionOpenMode.",
            category=DeprecationWarning,
            stacklevel=3
        )

        # if not provided calculate mode from deprecated args
        if mode is None:
            from gnucash.gnucash_core import SessionOpenMode
            ignore_lock = False if ignore_lock is None else ignore_lock
            is_new = False if is_new is None else is_new
            force_new = False if force_new is None else force_new
            mode = SessionOpenMode((ignore_lock << 2) + (is_new << 1) + force_new)

    return mode

def deprecated_args_session_init(original_function):
    """decorator for Session.__init__() to provide backward compatibility for deprecated use of ignore_lock, is_new and force_new"""
    @wraps(original_function)
    def new_function(self, book_uri=None, ignore_lock_or_mode=None, is_new=None,
                 force_new=None, instance=None, mode=None, ignore_lock=None):

        mode = deprecated_args_session(ignore_lock_or_mode, is_new, force_new, mode, ignore_lock)
        return(original_function(self, book_uri=book_uri, mode=mode, instance=instance))
    return new_function

def deprecated_args_session_begin(original_function):
    """decorator for Session.begin() to provide backward compatibility for deprecated use of ignore_lock, is_new and force_new"""
    @wraps(original_function)
    def new_function(self, new_uri=None, ignore_lock_or_mode=None, is_new=None,
                 force_new=None, mode=None, ignore_lock=None):
        mode = deprecated_args_session(ignore_lock_or_mode, is_new, force_new, mode, ignore_lock)
        return(original_function(self, new_uri=new_uri, mode=mode))
    return new_function
