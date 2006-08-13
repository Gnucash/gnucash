%module sw_business_gnome
%{
/* Includes the header in the wrapper code */
#include <config.h>
#include <gtk/gtk.h>
#include <business-urls.h>
#include <dialog-billterms.h>
#include <dialog-customer.h>
#include <dialog-employee.h>
#include <dialog-invoice.h>
#include <dialog-job.h>
#include <dialog-order.h>
#include <dialog-payment.h>
#include <dialog-vendor.h>
%}

%import "business-core.i"

/* Parse the header file to generate wrappers */
%include <business-urls.h>
%include <dialog-billterms.h>
%include <dialog-customer.h>
%include <dialog-employee.h>
%include <dialog-invoice.h>
%include <dialog-job.h>
%include <dialog-order.h>
%include <dialog-payment.h>
%include <dialog-vendor.h>
