/* 
 * FILE:
 * perl5_typemaps.i
 *
 * FUNCTION:
 * Clean up various aspects of the gnucash engine interface with 
 * respect to the SWIG-generated perl bindings. This includes:
 * -- handling time_t values as ints
 * -- converting C ** arrays to perl arrays
 *
 * HISTORY:
 * Created by Linas Vepstas January 1999
 * Copyright (c) 1999,2001 Linas Vepstas  <linas@linas.org>
 */

/* --------------------------------------------------------- */
/* first, some basic typemaps */

/* According to the docs, this kind of apply should work ... but it seems
 * to work only for pointer types, not for scalars, and I can't tell why ... */
#ifdef DOESNT_WORK_DONT_KNOW_WHY
%apply int BOTH {time_t x};
#endif /* DOESNT_WORK_DONT_KNOW_WHY */

/* Convert the return values from the function to perl values */
/* Specifically, create a new perl scalar and store the int value there */
/* We can handle multiple types at the same time here */
%typemap(perl5, out) gboolean,time_t,GNCBackendError {

  $target = newSViv ((IV) *($source));
  /* 
   * An alternate way of writing this code would have been ...
   *    $target = sv_newmortal ();
   *    sv_setiv ($target, (IV) $source);
   */
  argvi ++;
  // printf ("Info: converted return %d \n", (int) SvIV($target)); 
}

/* Another problem is that $type expands to a poiner, so we have to 
 * specify each type by hand ... */
%typemap(perl5, in) time_t *(time_t temp) {
  /* Convert function arguments from perl to the C representation */
  /* in particular, convert perl scalar into integer, then cast to type */
  temp = (time_t) SvIV($source);
  $target = &temp;
  // printf ("Info: time_t input arg is %ld \n", * ($target));
}

%typemap(perl5, in) gboolean *(gboolean temp) {
  /* Convert function arguments from perl to the C representation */
  /* in particular, convert perl scalar into integer, then cast to type */
  temp = (gboolean) SvIV($source);
  $target = &temp;
  // printf ("Info: gboolean input arg is %ld \n", * ($target));
}

/* --------------------------------------------------------- */

#define EXPERIMENTAL_PARRAY_SUPPORT
#ifdef EXPERIMENTAL_PARRAY_SUPPORT

// argh.  I've tried everything, including rtfm, and I still
// can't make this work ....
// everything seems be OK here, just the way the documentation says it
// should be (see perlguts, perlap man pages),  but the actual perl 
// test program gets an array that's the wrong length (and can't 
// yank blessed scalars out of it), no matter what I try here.
//
// Curiously, the 'perlxstut' man page says example 7 deals with
// arrays, but the example hasn't been written yet ... Hmmm.
// That would be exaclty the example we can't make work here ... hmmmm.

// Creates a new perl array and puts 
%typemap(perl5,out) SplitList * {
    AV *myav;
    int i = 0,len = 0;
    GList *node;

    /* Figure out how many elements we have */
    for (node=$source; node; node=node->next) { len++; }

printf ("eh duude typemap got a split list !!! length=%d \n", len);

    myav = newAV();
    for (node=$source; node; node=node->next) 
    { 
        SV *sv = sv_newmortal();
        sv_setref_pv (sv, "SplitPtr", node);
printf ("duude sv is object ? %d\n", sv_isobject(sv));
printf ("duude sv is blessed to class splitptr? %d\n", sv_isa(sv, "SplitPtr"));
        av_push (myav, sv);
    };
printf (" the av len is %d\n", av_len(myav));
    $target = newRV((SV*)myav);

    // sv_2mortal($target);

    argvi ++;
}
#endif

/* --------------------------------------------------------- */

#ifdef EXPERIMENTAL_ARRAY_SUPPORT

// Creates a new perl array and puts 
%typemap(perl5,out) SplitList * {
    AV *myav;
    SV **svs;
    int i = 0,len = 0;
    GList *node;

    /* Figure out how many elements we have */
    for (node=$source; node; node=node->next) { len++; }

printf ("eh duude typemap got a split list !!! length=%d \n", len);

    svs = (SV **) malloc(len*sizeof(SV *));
    for (i=0, node=$source; node; node=node->next, i++) 
    { 
        svs[i] = sv_newmortal();
        sv_setref_pv (svs[i], "SplitPtr", node);
        // sv_setref_pvn (sv, "SplitPtr", node, sizeof (Split));
        // printf ("typemap sv is object ? %d\n", sv_isobject(svs[i]));
        // printf ("typemap sv is blessed to class splitptr? %d\n",
        //        sv_isa(svs[i], "SplitPtr"));
    };
    myav = av_make(len,svs);
printf (" the av len is %d\n", av_len(myav));
    free(svs);
    // $target = newRV((SV*)myav);
    // $target = myav;
    $target = newSVsv((SV*)myav);
    sv_2mortal($target);

    argvi ++;
}
#endif

/* --------------------------------------------------------- */

#ifdef DOESNT_WORK_DONT_KNOW_WHY
// well, this sort of works ... it does create an array,
// but it doesn't seem to be an array of SplitPtr,
// which is what we want if we are to have 
//    foreach $split (@splits) {...}


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
    // $target = myav;
    $target = newRV((SV*)myav);
    sv_2mortal($target);
    argvi ++;
}

#endif /* DOESNT_WORK_DONT_KNOW_WHY */
