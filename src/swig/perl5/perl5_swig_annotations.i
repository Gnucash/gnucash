/* Treat time_t as a floating point number at the guile level.  On
   many of the common platforms it's just an integer, but there are
   supposedly platforms (acording to the GNU libc info pages) where it
   may be a real, and a real is always safe here. */

/*
typedef double time_t;
*/

/* Convert the return values from the function to perl values */
/* specifically, create a new perl scalar and store the int value there */
%typemap(perl5, out) time_t {

  $target = newSViv ((IV) *($source));
  /* 
   * An alternate way of writing this code would have been ...
   *    $target = sv_newmortal ();
   *    sv_setiv ($target, (IV) $source);
   */
  argvi ++;
  printf ("Info: converted return val of time_t secs to %d \n", (int) SvIV($target));
}

%typemap(perl5, in) time_t *  {
  /* Convert function arguments from perl to the C represntation */
  /* in particular, convert perl scalar into integer, then cast to time_t */
  /* this is kinda whacked, I don't know why swig wants to turn time_t's into
   * ptrs ... thus the icky alloca
   */
  $target = alloca (sizeof (time_t));
  *($target) = (time_t) SvIV($source);
  printf ("Info: time_t input arg is %ld \n", * ($target));
}

/*
%typemap(perl5,in) char * {
             ... Turn a perl array into a char ** ...
     }
*/


