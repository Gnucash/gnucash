#ifndef __EGG_INTL_H__
#define __EGG_INTL_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <libgnome/gnome-i18n.h>

#define P_(String) dgettext(GETTEXT_PACKAGE "-properties",String)

#endif /* __EGG_INTL_H__ */
