/* -*-c-*- */

/* Treat time_t as a floating point number at the guile level.  On
   many of the common platforms it's just an integer, but there are
   supposedly platforms (acording to the GNU libc info pages) where it
   may be a real, and a real is always safe here. */

typedef double time_t;

/*
  %typemap(guile, out) time_t {
  $target = gh_double2scm((double) (* $source));
}

%typemap(guile, in) time_t {
  $target = gh_scm2double($source);
}

*/
