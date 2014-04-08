### Makefile

CFLAGS := -Wall -O2 -g -I../../engine -I ../../../

GTK_LIBS := -lgtk -lgdk -lglib
LOADLIBES := -lplot -L/usr/X11R6/lib ${GTK_LIBS} -lXaw -lXt -lXext -lX11 -lm

plot-test: ../../libengine.a plot-test.o
