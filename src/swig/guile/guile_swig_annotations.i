/* -*-c-*- */

/* This tells SWIG to pass SCM values straight through.  I'm not sure
   how kosher this is, so we may have to watch it in future SWIG
   releases, but it works for now.  It's designed to allow you to have
   something like this on the C side:

    void foo(SCM f) {
      gh_call0(f);
    }
 
  And call it from Guile like this:
 
    (foo (lambda (x) (display "OK") (newline)))
*/

%typemap(guile,out) SCM {
    $target = $source;
}

%typemap(guile,in) SCM* {
    $target = &$source;
}


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
