%module gnucash
%include perl5_swig_annotations.i
%{
#include <Account.h>
#include <DateUtils.h>
#include <GNCId.h>
#include <Group.h>
#include <Query.h>
#include <Queue.h>
#include <Scrub.h>
#include <TransLog.h>
#include <Transaction.h>
#include <date.h>
#include <gnc-book.h>
%}
%include Account.h
%include DateUtils.h
%include GNCId.h
%include Group.h
%include Query.h
%include Queue.h
%include Scrub.h
%include TransLog.h
%include Transaction.h
%include date.h
%include "gnc-book.h"
