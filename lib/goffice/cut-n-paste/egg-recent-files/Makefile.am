INCLUDES = \
	$(GNUMERIC_CFLAGS) -I$(top_srcdir)/src -DUSE_STABLE_LIBGNOMEUI		\
	 		\
	 	\
	-DEGG_COMPILATION

noinst_LIBRARIES = libeggrecent.a

vfsdir = $(libdir)/gnome-vfs-2.0/modules

vfsconfdir = $(sysconfdir)/gnome-vfs-2.0/modules

libeggrecent_a_LIBADD = $(EGG_RECENT_LIBS)

libeggrecent_a_SOURCES =		\
	egg-recent.h			\
	egg-recent-item.h		\
	egg-recent-item.c		\
	egg-recent-model.c		\
	egg-recent-model.h		\
	egg-recent-view.c		\
	egg-recent-view.h		\
	egg-recent-view-gtk.c		\
	egg-recent-view-gtk.h		\
		\
		\
	egg-recent-util.c		\
	egg-recent-util.h





