/* 
 * FILE:
 * perl5_swig_annotations.i
 *
 * FUNCTION:
 * clean up various aspects of the gnucash engine interface with 
 * respect to the SWIG-generated perl bindings. This includes:
 * -- handling time_t values as ints
 * -- converting C ** arrays to perl arrays
 *
 * HISTORY:
 * Created by Linas Vepstas January 1999
 * Copyright (c) 1999 Linas Vepstas 
 */

#ifdef DOESNT_WORK_DONT_KNOW_WHY
%apply int {time_t }
#endif /* DOESNT_WORK_DONT_KNOW_WHY */

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
  // printf ("Info: converted return time_t secs to %d \n", (int) SvIV($target));
}

%typemap(perl5, in) time_t *  {
  /* Convert function arguments from perl to the C represntation */
  /* in particular, convert perl scalar into integer, then cast to time_t */
  /* this is kinda whacked, I don't know why swig wants to turn time_t's into
   * ptrs ... thus the icky alloca
   */
  $target = alloca (sizeof (time_t));
  *($target) = (time_t) SvIV($source);
  // printf ("Info: time_t input arg is %ld \n", * ($target));
}

// Creates a new Perl array and places a Split ** into it
%typemap(perl5,out) Split ** {
    AV *myav;
    SV **svs;
    int i = 0,len = 0;

    /* Figure out how many elements we have */
    while ($source[len]) len++;
    svs = (SV **) malloc(len*sizeof(SV *));
    printf ("Info: measured Split array length of len=%d\n", len);

    /* fill in array of scalar values */
    for (i = 0; i < len ; i++) {
        svs[i] = sv_newmortal();
        sv_setref_pv (svs[i], "SplitPtr", $source[i]);
    };

    /* convert array of scalars into perl array */
    myav =  av_make(len,svs);
    free(svs);
    $target = newRV((SV*)myav);
    sv_2mortal($target);
    argvi ++;
}

