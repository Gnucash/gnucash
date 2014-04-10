noinst_LTLIBRARIES = libgoffice-app.la

noinst_HEADERS = \
	file-priv.h \
	io-context-priv.h

libgoffice_app_la_SOURCES = \
	go-doc.c 			\
	go-doc-control.c 		\
	\
	go-plugin.c 			\
	go-plugin-loader.c 		\
	go-plugin-loader-module.c 	\
	go-plugin-service.c 		\
	\
	file.c				\
	error-info.c			\
	io-context.c			\
	go-cmd-context.c

libgoffice_app_ladir = $(goffice_include_dir)/app
libgoffice_app_la_HEADERS = 	\
	goffice-app.h			\
	\
	go-doc.h 			\
	go-doc-impl.h 			\
	go-doc-control.h 		\
	go-doc-control-impl.h 		\
	go-plugin.h 			\
	go-plugin-loader.h 		\
	go-plugin-loader-module.h 	\
	module-plugin-defs.h 		\
	go-plugin-service.h 		\
	go-plugin-service-impl.h	\
	file.h				\
	error-info.h			\
	io-context.h			\
	io-context-priv.h		\
	go-cmd-context.h		\
	go-cmd-context-impl.h

# This list should be empty when we reach a stable phase.
UNUSED = \
	go-app.h \
	go-error-stack.h \
	go-object.c \
	go-object.h \
	go-service.c \
	go-service.h \
	go-service-impl.h

EXTRA_DIST = $(UNUSED)

include $(top_srcdir)/lib/goffice-0.0.4/goffice.mk
