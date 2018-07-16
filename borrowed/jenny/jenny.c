/*
-------------------------------------------------------------------------------
By Bob Jenkins, March 2003.  Public domain.

jenny.c -- jennyrate tests from m dimensions of features that cover all
  n-tuples of features, n <= m, with each feature chosen from a different 
  dimension.  For example, given 10 dimensions (m=10) with 2 to 8 features
  apiece, cover all feature triplets (n=3).  A lower bound on the number
  of tests required is the product of the sizes of the largest n dimensions.
  Already-written tests can be piped in to be reused.

Arguments
  Arguments without a leading '-' : an integer in 2..52.  Represents a
       dimension.  Dimensions are implicitly numbered 1..65535, in the 
       order they appear.  Features in dimensions are always implicitly
       given 1-character names, which are in order a..z, A..Z .  It's a
       good idea to pass the output of jenny through a postprocessor that
       expands these names into something intelligible.

  -o : old.  -ofoo.txt reads existing tests from the file foo.txt, includes
       those tests in the output, and adds whatever other tests are needed to
       complete coverage.  An error is reported if the input tests are of the
       wrong shape or contain disallowed feature interactions.  If you have
       added new dimensions since those tests were written, be sure to include
       a do-nothing feature in each new dimension, then pad the existing tests
       with do-nothing out to the correct number of dimensions.

  -h : help.  Print out instructions for using jenny.

  -n : an integer.  Cover all n-tuples of features, one from each dimension.
       Default is 2 (pairs).  3 (triplets) may be reasonable.  4 (quadruplets)
       is definitely overkill.  n > 4 is highly discouraged.

  -s : seed.  An integer.  Seed the random number generator.

  -w : without this combination of features.  A feature is given by a dimension
       number followed by a one-character feature name.  A single -w can
       disallow multiple features in a dimension.  For example, -w1a2cd4ac
       disallows the combinations (1a,2c,4a),(1a,2c,4c),(1a,2d,4a),(1a,2d,4c)
       where 1a represents the first dimension's first feature, 2c is the 
       second dimension's third feature, and 4a is the fourth dimension's
       first feature.

Example: 10 dimensions of 2, 3, 8, 3, 2, 2, 5, 3, 2, 2 features apiece,
with some restrictions, asking for all triplets of features to be covered.
This will produce at least 8*5*3=120 tests.  Splitting the 8 features in the
third dimension into three dimensions each of length 2 would reduce the
number of testcases required to at least 5*3*3=45.

  jenny -n3 2 3 8 -w1a2bc3b -w1b3a 3 -w1a4b 2 2 5 3 2 2 -w9a10b -w3a4b -s3
-------------------------------------------------------------------------------
*/

#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

/*
-------------------------------------------------------------------------------
Implementation:

Internally, there can be 64K dimensions with 64K features apiece.  Externally,
the number of features per dimensions is limited to just 52, and are implicitly
named a..z, A..Z.  Other printable characters, like |, caused trouble in the
shell when I tried to give them during a without.

The program first finds tests for all features, then adds tests to cover
all pairs of features, then all triples of features, and so forth up to
the tuples size the user asked for.
-------------------------------------------------------------------------------
*/

/*
-------------------------------------------------------------------------------
Structures
-------------------------------------------------------------------------------
*/

typedef  unsigned char      ub1;
typedef           char      sb1;
typedef  unsigned short     ub2;
typedef  signed   short     sb2;
typedef  unsigned long      ub4;
typedef  signed   long      sb4;
typedef  unsigned long long ub8;
typedef  signed   long long sb8;
#define TRUE  1
#define FALSE 0
#define UB4MAXVAL 0xffffffff
#define UB2MAXVAL 0xffff

/*
-------------------------------------------------------------------------------
Random number stuff
-------------------------------------------------------------------------------
*/

#define FLEARAND_SIZE 256
typedef  struct flearandctx {
  ub4 b,c,d,z;                                             /* special memory */
  ub4 m[FLEARAND_SIZE];                         /* big random pool of memory */
  ub4 r[FLEARAND_SIZE];                                           /* results */
  ub4 q;                     /* counter, which result of r was last reported */
} flearandctx;

/* Pseudorandom numbers, courtesy of FLEA */
void flearand_batch( flearandctx *x) {
  ub4 a, b=x->b, c=x->c+(++x->z), d=x->d, i, *m=x->m, *r=x->r;
  for (i=0; i<FLEARAND_SIZE; ++i) {
    a = m[b % FLEARAND_SIZE];
    m[b % FLEARAND_SIZE] = d;
    d = (c<<19) + (c>>13) + b;
    c = b ^ m[i];
    b = a + d;
    r[i] = c;
  }
  x->b=b; x->c=c; x->d=d;
}

ub4 flearand( flearandctx *x) {
  if (!x->q--) {
    x->q = FLEARAND_SIZE-1;
    flearand_batch(x);
  }
  return x->r[x->q];
}

void flearand_init( flearandctx *x, ub4 seed) {
  ub4    i;

  x->b = x->c = x->d = x->z = seed;
  for (i = 0; i<FLEARAND_SIZE; ++i) {
    x->m[i] = seed;
  }
  for (i=0; i<10; ++i) {
    flearand_batch(x);
  }
  x->q = 0;
}




/*
------------------------------------------------------------------------------
Other helper routines
------------------------------------------------------------------------------
*/

#define TUPLE_ARRAY  5040       /* tuple array size, multiple of 1,2,3,4,5,6 */

/* An arbitrary feature, prefix fe */
typedef  struct feature {
  ub2    d;                                                /* Dimension name */
  ub2    f;                                                  /* Feature name */
} feature;

/* a tuple array, prefix tu */
typedef  struct tu_arr {
  struct tu_arr  *next;                                  /* next tuple array */
  ub2             len;                               /* length of this array */
  feature         fe[TUPLE_ARRAY];                        /* array of tuples */
} tu_arr;

/* an iterator over a tuple array, prefix tu */
typedef  struct tu_iter {
  struct tu_arr **tu;                                 /* current tuple array */
  ub2             offset;                         /* offset of current tuple */
  ub2             n;                         /* number of features per tuple */
  ub4            *count;                         /* number of tuples in list */
  feature        *fe;                                       /* current tuple */
} tu_iter;

/* names of features, for output */
static const char feature_name[] =
"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";



/*
------------------------------------------------------------------------------
Stuff specific to jenny
------------------------------------------------------------------------------
*/

#define MAX_FEATURES 52                  /* can't name more than 52 features */
#define MAX_TESTS    65534         /* arbitrary limit on number of testcases */
#define MAX_N        32       /* never can do complete coverage of 33-tuples */
#define MAX_WITHOUT  (MAX_FEATURES*MAX_N)       /* max features in a without */
#define MAX_DIMENSIONS (((ub2)~0)-1) /* More than 64K dimensions needs a ub4 */

/* A "test", which is a combination of features.  Prefix t. */
typedef  struct test {
  ub2         *f;                                   /* features in this test */
} test;

/* representation of a restriction, prefix w */
typedef  struct without {
  ub2             len;                            /* length of feature array */
  struct feature *fe;                                       /* feature array */
} without;

/* without chain */
typedef  struct wchain {
  without       *w;
  struct wchain *next;
} wchain;


/* Return a count of how many withouts are disobeyed. */
/* Also set a pointer to a randomly chosen violated without */
int count_withouts(
test     *t,                                                /* test to check */
wchain   *wc)                                                /* restrictions */
{
  ub4      count;                             /* count of disobeyed withouts */

  for (count = 0; wc; wc = wc->next) {
    without *w = wc->w;
    ub4      i = 0;
    int      match = TRUE;                   /* match the entire restriction */
    while (i<w->len) {
      int dimension_match = FALSE;                /* match in this dimension */
      do {
	if (t->f[w->fe[i].d] == w->fe[i].f) {
	  dimension_match = TRUE;
	}
	++i;
      } while (i<w->len && (w->fe[i].d == w->fe[i-1].d));
      if (!dimension_match) {
	match = FALSE;
	break;
      }
    }
    if (match) {
      ++count;
    }
  }
  return count;
}


/* token definitions */
typedef enum token_type {
  TOKEN_ERROR = 0,                                /* TOKEN_ERROR has to be 0 */
  TOKEN_END,
  TOKEN_NUMBER,
  TOKEN_FEATURE,
  TOKEN_SPACE
} token_type;


/* whole current state, prefix s */
typedef  struct state {
  ub1       n_final;           /* The n in the user's n-tuples, default is 2 */
  ub2       ndim;                                    /* number of dimensions */
  ub2       ntests;                              /* number of testcases in t */
  ub1     **n;  /* n[d][f] is current n-tuple size for dimension d feature f */
  test    **t;                             /* all the tests generated so far */
  ub2      *dim;                     /* number of features in each dimension */
  wchain  **wc;                   /* s->wc[d] lists withouts for dimension d */
  wchain   *wc2;                      /* a list of all the original withouts */
  wchain   *wc3;                             /* additional, deduced withouts */
  tu_arr ***tu;  /* tu[d][f] lists untested tuples for dimension d feature f */
  tu_arr ***one;
       /* one[testcase][d] lists tuples with d covered only by this testcase */
  ub4     **onec;          /* onec[testcase][d] is count of one[testcase][d] */
  ub4     **used;
   /* used[testcase][d] = pass# if this pass has already explored test[t][d] */
  ub4     **tc;   /* tc[d][f] is # untested tulpes for dimension d feature f */
  test     *tuple_tester;   /* an all -1 test used to test individual tuples */
  ub2      *dimord;                   /* order in which to choose dimensions */
  ub2      *featord;                    /* order in which to choose features */
  flearandctx   r;                                  /* random number context */
} state;


void my_free( char *x)
{
  free(x);
}

/* zero out a list of tuples */
void truncate_tuple( tu_arr **tu, ub4 *count)
{
  while (*tu) {
    tu_arr *tu2 = *tu;
    *tu = ((*tu)->next);
    my_free((char *)tu2);
  }
  *count = 0;
}

/* delete the i-th test */
void delete_test( state *s, ub4 i)
{
  test *t = s->t[i];

  s->t[i] = s->t[--s->ntests];
  my_free((char *)t->f);
  my_free((char *)t);
  if (s->one[s->ntests]) {
    ub2   d;
    for (d=0; d<s->ndim; ++d) {
      truncate_tuple(&s->one[s->ntests][d], &s->onec[s->ntests][d]);
    }
    my_free((char *)s->one[s->ntests]);
    my_free((char *)s->onec[s->ntests]);
    s->one[s->ntests]  = (tu_arr **)0;
    s->onec[s->ntests] = (ub4 *)0;
  }
}

void cleanup(state *s)
{
  if (s->tu) {
    ub2 d,f;
    for (d=0; d<s->ndim; ++d) {
      if (s->tu[d]) {
	for (f=0; f<s->dim[d]; ++f) {
	  truncate_tuple(&s->tu[d][f], &s->tc[d][f]);
	}
	my_free((char *)s->tu[d]);
      }
    }
    my_free((char *)s->tu);
  }

  /* free n, the tuple lengths */
  if (s->n) {
    ub2 d;
    for (d=0; d<s->ndim; ++d) {
      if (s->n[d]) {
	my_free((char *)s->n[d]);
      }
    }
    my_free((char *)s->n);
  }

  /* free tc, count of uncovered tuples */
  if (s->tc) {
    ub2 d;
    for (d=0; d<s->ndim; ++d) {
      if (s->tc[d]) {
	my_free((char *)s->tc[d]);
      }
    }
    my_free((char *)s->tc);
  }

  /* free the secondary chains of restrictions */
  if (s->wc) {
    ub2 i;
    for (i=0; i<s->ndim; ++i) {
      while (s->wc[i]) {
	wchain  *wc = s->wc[i];
	s->wc[i] = s->wc[i]->next;
	my_free((char *)wc);
      }
    }
    my_free((char *)s->wc);
  }

  /* free all the actual restrictions */
  while (s->wc2) {
    wchain  *wc = s->wc2;
    without *w = wc->w;
    s->wc2 = s->wc2->next;
    if (w->fe) my_free((char *)w->fe);
    my_free((char *)w);
    my_free((char *)wc);
  }

  while (s->wc3) {
    wchain  *wc = s->wc3;
    without *w = wc->w;
    s->wc3 = s->wc3->next;
    if (w->fe) my_free((char *)w->fe);
    my_free((char *)w);
    my_free((char *)wc);
  }

  if (s->t) {
    while (s->ntests) {
      delete_test(s, 0);
    }
    my_free((char *)s->t);
  }

  if (s->one) {
    my_free((char *)s->one);
  }

  if (s->onec) {
    my_free((char *)s->onec);
  }

  if (s->tuple_tester) {
    if (s->tuple_tester->f) {
      my_free((char *)s->tuple_tester->f);
    }
    my_free((char *)s->tuple_tester);
  }

  if (s->dimord) {
    my_free((char *)s->dimord);
  }

  if (s->featord) {
    my_free((char *)s->featord);
  }

  /* free the array of dimension lengths */
  if (s->dim) my_free((char *)s->dim);
}


char *my_alloc( state *s, size_t len)
{
  char *rsl;
  if (!(rsl = (char *)malloc(len+sizeof(size_t)))) {
    printf("jenny: could not allocate space\n");
    cleanup(s);
    exit(0);  
  }
  memset(rsl, 0x00, len);
  return rsl;
}

/* insert a tuple into a tuple array */
int insert_tuple( state *s, tu_iter *ctx, feature *tuple)
{
  ub4      i;
  feature *fe;
  ub1      n = ctx->n;
  ub4      lim = TUPLE_ARRAY / n;

  while (*ctx->tu && (*ctx->tu)->len == lim) {
    ctx->tu = &((*ctx->tu)->next);
  }
  if (!*ctx->tu) {
    if (!((*ctx->tu) = (tu_arr *)my_alloc(s, sizeof(tu_arr)))) {
      return FALSE;
    }
    (*ctx->tu)->len = 0;
    (*ctx->tu)->next = (tu_arr *)0;
  }
  fe = &(*ctx->tu)->fe[(*ctx->tu)->len*n];
  for (i=0; i<n; ++i) {
    fe[i].d = tuple[i].d;
    fe[i].f = tuple[i].f;
  }
  ++(*ctx->tu)->len;
  ++*ctx->count;
  return TRUE;
}

/* print out a single tuple */
void show_tuple( feature *fe, ub2 len)
{
  ub4 i;
  for (i=0; i<len; ++i) {
    printf(" %d%c", fe[i].d+1, feature_name[fe[i].f]);
  }
  printf(" \n");
}

/* delete a tuple from a tuple array */
feature *delete_tuple( tu_iter *ctx)
{
  feature *fe;
  ub4      i;
  feature *tuple = ctx->fe;
  ub1      n = ctx->n;

  --(*ctx->tu)->len;
  --*ctx->count;
  fe = &(*ctx->tu)->fe[(*ctx->tu)->len * n];
  for (i=0; i<n; ++i) {
    tuple[i].d = fe[i].d;
    tuple[i].f = fe[i].f;
  }
  if (!(*ctx->tu)->len) {
    tu_arr *tu2 = *ctx->tu;
    *ctx->tu = ((*ctx->tu)->next);
    my_free((char *)tu2);
  }

  if (!*ctx->tu) {                                   /* freed the last block */
    ctx->offset = 0;
    ctx->fe = (feature *)0;
    return ctx->fe;
  } else if (tuple == fe) {
    ctx->offset = 0;
    ctx->fe = &(*ctx->tu)->fe[0];          /* freed this block, move to next */
    return ctx->fe;
  } else {
    return tuple;                 /* moved a new tuple into the old location */
  }
}

/* start a tuple iterator */
feature *start_tuple( tu_iter *ctx, tu_arr **tu, ub4 n, ub4 *count)
{
  ctx->tu = tu;
  ctx->offset = 0;
  ctx->n = n;
  ctx->count = count;

  if (*tu) {
    ctx->fe = (*tu)->fe;
  } else {
    ctx->fe = (feature *)0;
  }
  return ctx->fe;
}

/* get the next tuple from a tuple iterator (0 if no more tuples) */
static feature *next_tuple( tu_iter *ctx)
{
  if (++ctx->offset < (*ctx->tu)->len) {
    ctx->fe += ctx->n;
  } else {
    ctx->tu = &(*ctx->tu)->next;
    ctx->offset = 0;
    if (*ctx->tu && (*ctx->tu)->len) {
      ctx->fe = (*ctx->tu)->fe;
    } else {
      ctx->fe = (feature *)0;
    }
  }
  return ctx->fe;
}


/* test if this test covers this tuple */
static int test_tuple( ub2 *test, feature *tuple, ub2 n)
{
  sb4 i;
  for (i=0; i<n; ++i) {
    if (tuple[i].f != test[tuple[i].d]) {
      return FALSE;
    }
  }
  return TRUE;
}

/* test if first tuple (t1, n1) is a subset of second tuple (t2, n2) */
int subset_tuple( feature *t1, ub1 n1, feature *t2, ub1 n2)
{
  sb4 i, j;
  if (n2 < n1)
    return FALSE;
  for (i=0, j=0; i<n1; ++i) {
    while (t1[i].d > t2[j].d) {
      if (++j == n2)
	return FALSE;
    }
    if (t1[i].d != t2[j].d || t1[i].f != t2[j].f) {
      return FALSE;
    }
  }
  return TRUE;
}

void initialize( state *s)
{
  /* make all freeable pointers start out zero */
  s->dim          = (ub2 *)0;
  s->wc           = (wchain **)0;
  s->wc2          = (wchain *)0;
  s->wc3          = (wchain *)0;
  s->tu           = (tu_arr ***)0;
  s->one          = (tu_arr ***)0;
  s->onec         = (ub4 **)0;
  s->n            = (ub1 **)0;
  s->tc           = (ub4 **)0;
  s->t            = (test **)0;
  s->tuple_tester = (test *)0;
  s->dimord       = (ub2 *)0;
  s->featord      = (ub2 *)0;

  /* fill in default values */
  s->ndim = (ub2)0;
  s->n_final = 2;     /* guarantees that all pairs of dimensions are covered */
  s->ntests = 0;
  flearand_init(&s->r, 0);             /* initialize random number generator */
}


/* add one test to the list of tests */
int add_test( state *s, test *t)
{
  ub4 i;
  if (s->ntests == MAX_TESTS) {
    return FALSE;
  }
  s->one[s->ntests] = (tu_arr **)my_alloc(s, sizeof(tu_arr *)*s->ndim);
  s->onec[s->ntests] = (ub4 *)my_alloc(s, sizeof(ub4)*s->ndim);
  for (i=0; i<s->ndim; ++i) {
    s->one[s->ntests][i] = (tu_arr *)0;
    s->onec[s->ntests][i] = 0;
  }
  s->t[s->ntests++] = t;
  return TRUE;
}

/*
 * parse a token
 * Start at *curr in string *inp of length inl
 * Adjust *curr to be after the just-parsed token
 * Place the token value in *rsl
 * Return the token type
 */
token_type parse_token(char *inp, ub4 inl, ub4 *curr, ub4 *rsl)
{
  char mychar;
  ub4  i;

  if (*curr == inl)
    return TOKEN_END;
  mychar = inp[*curr];

  if (mychar == '\0') {
    return TOKEN_END;
  } else if (mychar == ' ' || mychar == '\t' || mychar == '\n') {
    /*--------------------------------------------------------- parse spaces */
    for (i=*curr+1; i < inl; ++i) {
      mychar = inp[i];
      if (!(mychar == ' ' || mychar == '\t' || mychar == '\n'))
	break;
    }
    *curr = i;
    return TOKEN_SPACE;
  } else if (mychar >= '0' && mychar <= '9') {
    /*------------------------------------------------------- parse a number */
    ub4 i, number = 0;
    for (i=*curr; i < inl && inp[i] >= '0' && inp[i] <= '9';  ++i) {
      number = number*10 + (inp[i] - '0');
    }
    *curr = i;
    *rsl = number;
    return TOKEN_NUMBER;
  } else if ((mychar >= 'a' && mychar <= 'z') ||
	     (mychar >= 'A' && mychar <= 'Z')) {
    /*------------------------------------------------- parse a feature name */
    ub4 i;
    for (i=0; i<MAX_FEATURES; ++i)
      if (feature_name[i] == mychar)
	break;
    if (i == MAX_FEATURES) {
      printf("jenny: the name '%c' is not used for any feature\n",
	     mychar);
	return TOKEN_ERROR;
    }
    *rsl = i;
    ++*curr;
    return TOKEN_FEATURE;
  } else {
    return TOKEN_ERROR;
  }
}

#define BUFSIZE (MAX_DIMENSIONS*7+2)
/* load old tests before generating new ones */
int load( state *s, char *testfile)
{
  char  buf[BUFSIZE];            /* buffer holding a line read from the file */
  FILE *f;
  
  if (testfile[0] == '\0') {
    f = stdin;
  } else {
    f = fopen(testfile, "r");
  }

  if (!f) {
    printf("jenny: file %s could not be opened\n", testfile);
    return FALSE;
  }

  while (fgets(buf, BUFSIZE, f) && (buf[0] != '.')) {
    ub4   curr = 0;                               /* current offset into buf */
    ub4   value;                                              /* token value */
    token_type token;                                          /* token type */
    ub4   i;
    test *t;

    t = (test *)my_alloc( s, sizeof(test));
    t->f = (ub2 *)my_alloc( s, sizeof(ub2)*s->ndim);
    if (!add_test(s, t)) {
      goto failure;
    }

    for (i=0; i<s->ndim; ++i) {
      if (parse_token(buf, UB4MAXVAL, &curr, &value) != TOKEN_SPACE) {
	printf("jenny: -o, non-space found where space expected\n");
	goto failure;
      }
      if (parse_token(buf, UB4MAXVAL, &curr, &value) != TOKEN_NUMBER) {
	printf("jenny: -o, non-number found where number expected\n");
	goto failure;
      }
      if (value-1 != i) {
	printf("jenny: -o, number %d found out-of-place\n", value);
	goto failure;
      }
      if (parse_token(buf, UB4MAXVAL, &curr, &value) != TOKEN_FEATURE) {
	printf("jenny: -o, non-feature found where feature expected\n");
	goto failure;
      }
      if (value >= s->dim[i]) {
	printf("jenny: -o, feature %c does not exist in dimension %d\n", 
	       feature_name[value], i+1);
	goto failure;
      }
      t->f[i] = value;
    }
    if (parse_token(buf, UB4MAXVAL, &curr, &value) != TOKEN_SPACE) {
      printf("jenny: -o, non-space found where trailing space expected\n");
      goto failure;
    }
    if (parse_token(buf, UB4MAXVAL, &curr, &value) != TOKEN_END) {
      printf("jenny: -o, testcase not properly terminated\n");
      goto failure;
    }

    /* make sure the testcase obeys all the withouts */
    if (count_withouts(t, s->wc2)) {
      printf("jenny: -o, old testcase contains some without\n");
      goto failure;
    }
  }

  (void)fclose(f);
  return TRUE;

 failure:
  while (fgets(buf, BUFSIZE, f) && (buf[0] != '.'))
    ;                                            /* finish reading the input */
  (void)fclose(f);                                         /* close the file */
  return FALSE;
}

static const sb1 *jenny_doc[] = {
  "jenny:\n",
  "  Given a set of feature dimensions and withouts, produce tests\n",
  "  covering all n-tuples of features where all features come from\n",
  "  different dimensions.  For example (=, <, >, <=, >=, !=) is a\n",
  "  dimension with 6 features.  The type of the left-hand argument is\n",
  "  another dimension.  Dimensions are numbered 1..65535, in the order\n",
  "  they are listed.  Features are implicitly named a..z, A..Z.\n",
  "   3 Dimensions are given by the number of features in that dimension.\n",
  "  -h prints out these instructions.\n",
  "  -n specifies the n in n-tuple.  The default is 2 (meaning pairs).\n",
  "  -w gives withouts.  -w1b4ab says that combining the second feature\n",
  "     of the first dimension with the first or second feature of the\n",
  "     fourth dimension is disallowed.\n",
  "  -ofoo.txt reads old jenny testcases from file foo.txt and extends them.",
  "\n\n",
  "  The output is a testcase per line, one feature per dimension per\n",
  "  testcase, followed by the list of all allowed tuples that jenny could\n",
  "  not reach.\n",
  "\n",
  "  Example: jenny -n3 3 2 2 -w2b3b 5 3 -w1c3b4ace5ac 8 2 2 3 2\n",
  "  This gives ten dimensions, asks that for any three dimensions all\n",
  "  combinations of features (one feature per dimension) be covered,\n",
  "  plus it asks that certain combinations of features\n",
  "  (like (1c,3b,4c,5c)) not be covered.\n",
  "\n"
};


/* parse -n, the tuple size */
int parse_n( state *s, char *myarg)
{
  ub4 curr = 0;
  ub4 temp = UB4MAXVAL;
  token_type token;
  ub4 dummy;

  if ((token=parse_token(myarg, UB4MAXVAL, &curr, &temp)) != TOKEN_NUMBER) {
    printf("jenny: -n should give an integer in 1..32, for example, -n2.\n");
    return FALSE;
  }
  if ((token=parse_token(myarg, UB4MAXVAL, &curr, &dummy)) != TOKEN_END) {
    printf("jenny: -n should be followed by just an integer\n");
    return FALSE;
  }
  
  if ((temp < 1) || (temp > 32)) {
    printf("jenny: -n says all n-tuples should be covered.\n");
    return FALSE;
  }
  if (temp > s->ndim) {
    printf("jenny: -n, %ld-tuples are impossible with only %d dimensions\n",
	   temp, s->ndim);
    return FALSE;
  }
  s->n_final = (ub2)temp;
  return TRUE;
}



/* parse -w, a without */
int parse_w( state *s, sb1 *myarg)
{
  without   *w;
  wchain    *wc;
  feature    fe[MAX_WITHOUT];
  ub1        used[MAX_DIMENSIONS];
  ub4        dimension_number;
  ub4        curr = 0;
  ub4        fe_len, value;
  ub4        i, j, k;
  size_t     len = strlen(myarg);
  token_type t = parse_token(myarg, len, &curr, &value);
  
  for (i=0; i<s->ndim; ++i)
    used[i] = FALSE;
  if (t != TOKEN_NUMBER) {
    printf("jenny: -w is <number><features><number><features>...\n");
    printf("jenny: -w must start with an integer (1 to #dimensions)\n");
    return FALSE;
  }
  fe_len=0;
  
 number:
  dimension_number = --value;
  if (dimension_number >= s->ndim) {
    printf("jenny: -w, dimension %ld does not exist, ", dimension_number+1);
    printf("you gave only %d dimensions\n", s->ndim);
    return FALSE;
  }
  if (used[dimension_number]) {
    printf("jenny: -w, dimension %d was given twice in a single without\n",
	   dimension_number+1);
    return FALSE;
  }
  used[dimension_number] = TRUE;
  
  
  switch (parse_token(myarg, len, &curr, &value)) {
  case TOKEN_FEATURE: goto feature;
  case TOKEN_END:
    printf("jenny: -w, withouts must follow numbers with features\n");
    return FALSE;
  default:
    printf("jenny: -w, unexpected without syntax\n");
    printf("jenny: proper withouts look like -w2a1bc99a\n");
    return FALSE;
  }
  
 feature:
  if (value >= s->dim[dimension_number]) {
    printf("jenny: -w, there is no feature '%c' in dimension %d\n",
	   feature_name[value], dimension_number+1);
    return FALSE;
  }
  fe[fe_len].d = dimension_number;
  fe[fe_len].f = value;
  if (++fe_len >= MAX_WITHOUT) {
    printf("jenny: -w, at most %d features in a single without\n",
	   MAX_WITHOUT);
    return FALSE;
  }

  
  switch (parse_token(myarg, len, &curr, &value)) {
  case TOKEN_FEATURE: goto feature;
  case TOKEN_NUMBER: goto number;
  case TOKEN_END: goto end;
  default:
    printf("jenny: -w, unexpected without syntax\n");
    printf("jenny: proper withouts look like -w2a1bc99a\n");
    return FALSE;
  }
  
 end:

  /* sort the dimensions and features in this restriction */
  for (i=0; i<fe_len; ++i) {
    for (j=i+1; j<fe_len; ++j) {
      if ((fe[i].d > fe[j].d) ||
	  ((fe[i].d == fe[j].d) && (fe[i].f > fe[j].f))) {
	ub2 fe_temp;
	fe_temp = fe[i].d;
	fe[i].d = fe[j].d;
	fe[j].d = fe_temp;
	fe_temp = fe[i].f;
	fe[i].f = fe[j].f;
	fe[j].f = fe_temp;
      }
    }
  }

  /* allocate a without */
  w = (without *)my_alloc( s, sizeof(without));
  wc = (wchain *)my_alloc( s, sizeof(wchain));
  wc->next = s->wc2;
  wc->w = w;
  w->len = fe_len;
  w->fe = (feature *)my_alloc( s, sizeof(feature)*fe_len);
  for (i=0; i<fe_len; ++i) {
    w->fe[i].d = fe[i].d;
    w->fe[i].f = fe[i].f;
  }
  s->wc2 = wc;

  return TRUE;
}

/* parse -s, a seed for the random number generator */
int parse_s( state *s, sb1 *myarg)
{
  ub4 seed = 0;
  ub4 dummy = 0;
  ub4 curr = 0;
  if (parse_token( myarg, UB4MAXVAL, &curr, &seed) != TOKEN_NUMBER) {
    printf("jenny: -s must be followed by a positive integer\n");
    return FALSE;
  }
  if (parse_token( myarg, UB4MAXVAL, &curr, &dummy) != TOKEN_END) {
    printf("jenny: -s should give just an integer, example -s123\n");
    return FALSE;
  }
  flearand_init(&s->r, seed);          /* initialize random number generator */
  return TRUE;
}

void preliminary( state *s)
{
  wchain  *wc;
  ub4      d;

  s->tuple_tester = (test *)my_alloc( s, sizeof(test));
  s->tuple_tester->f = (ub2 *)my_alloc( s, sizeof(ub2)*s->ndim);
  s->dimord  = (ub2 *)my_alloc( s, sizeof(ub2)*s->ndim);
  s->wc = (wchain **)my_alloc( s, sizeof(wchain *)*s->ndim);
  s->tu = (tu_arr ***)my_alloc( s, sizeof(tu_arr **)*s->ndim);
  s->one = (tu_arr ***)my_alloc( s, sizeof(tu_arr **)*MAX_TESTS);
  s->n  = (ub1 **)my_alloc( s, sizeof(ub1 *)*s->ndim);
  s->onec = (ub4 **)my_alloc( s, sizeof(ub4 *)*MAX_TESTS);
  s->tc = (ub4 **)my_alloc( s, sizeof(ub4 *)*s->ndim);
  s->t  = (test **)my_alloc( s, sizeof(test *)*MAX_TESTS);

  /* initialize to safe values before doing further allocations */
  for (d=0; d<s->ndim; ++d) {
    s->tuple_tester->f[d] = (ub2)~0;
    s->dimord[d] = (ub2)d;
    s->wc[d] = (wchain *)0;
    s->tu[d] = (tu_arr **)0;
    s->n[d] = (ub1 *)0;
    s->tc[d] = (ub4 *)0;
  }

  s->featord = (ub2 *)my_alloc( s, sizeof(ub2)*MAX_FEATURES);

  /* allocate roots for feature-specific lists of uncovered tuples */
  for (d=0; d<s->ndim; ++d) {
    ub2 f;
    s->tu[d] = (tu_arr **)my_alloc( s, sizeof(tu_arr *)*s->dim[d]);
    s->n[d]  = (ub1 *)my_alloc(s, sizeof(ub1)*s->dim[d]);
    s->tc[d] = (ub4 *)my_alloc(s, sizeof(ub4)*s->dim[d]);
    for (f=0; f<s->dim[d]; ++f) {
      s->tu[d][f] = (tu_arr *)0;
      s->n[d][f]  = 0;
      s->tc[d][f] = 0;
    }
  }

  /* make dimension-specific lists of withouts */
  for (wc=s->wc2; wc; wc=wc->next) {
    without *w = wc->w;
    int      old = -1;
    int      i;
    for (i=0; i<w->len; ++i) {
      if (w->fe[i].d != old) {
	wchain *wcx = (wchain *)my_alloc( s, sizeof(wchain));
	wcx->w = w;
	wcx->next = s->wc[w->fe[i].d];
	s->wc[w->fe[i].d] = wcx;
	old = w->fe[i].d;
      }
    }
  }
}

/* parse the inputs */
int parse( int argc, char *argv[], state *s)
{
  int   i, j;
  ub4   temp;
  char *testfile = (char *)0;

  /* internal check: we have MAX_FEATURES names for features */
  if (strlen(feature_name) != MAX_FEATURES) {
    printf("feature_name length is wrong, %d\n", strlen(feature_name));
    return FALSE;
  }

  /* How many dimensions are there?  Set ndim, allocate space for dim.  */
  for (temp=0, i=1; i<argc; ++i) {
    if (argv[i][0] >= '0' && argv[i][0] <= '9') {
      ++temp;
    }
  }
  if (temp > MAX_DIMENSIONS) {
    printf("jenny: maximum number of dimensions is %ld.  %ld is too many.\n",
	   MAX_DIMENSIONS, temp);
    return FALSE;
  }
  s->ndim = (ub2)temp;
  s->dim = (ub2 *)my_alloc( s, sizeof(ub2)*(s->ndim));

  /* Read the lengths of all the dimensions */
  for (i=1, j=0; i<argc; ++i) {
    if (argv[i][0] >= '0' && argv[i][0] <= '9') {        /* dimension length */
      sb1 *myarg = argv[i];
      ub4  dummy;
      ub4  curr = 0;

      (void)parse_token(myarg, UB4MAXVAL, &curr, &temp);
      if (parse_token(myarg, UB4MAXVAL, &curr, &dummy) != TOKEN_END) {
	printf("jenny: something was trailing a dimension number\n");
	return FALSE;
      }
      if (temp > MAX_FEATURES) {
	printf("jenny: dimensions must be smaller than %d.  %ld is too big.\n",
	       MAX_FEATURES, temp);
	return FALSE;
      }
      if (temp < 2) {
	printf("jenny: a dimension must have at least 2 features, not %d\n",
	       temp);
	return FALSE;
      }
      s->dim[j++] = (ub2)temp;
    } else if (argv[i][1] == 'h') {
      int i;
      for (i=0; i<(sizeof(jenny_doc)/sizeof(ub1 *)); ++i) {
	printf(jenny_doc[i]);
      }
      return FALSE;
    }
  }

  /* Read the rest of the arguments */
  for (i=1; i<argc; ++i) if (argv[i][0] == '-') {        /* dimension length */
    switch(argv[i][1]) {
    case '\0':
      printf("jenny: '-' by itself isn't a proper argument.\n");
      return FALSE;
    case 'o':                               /* -o, file containing old tests */
      testfile = &argv[i][2];
      break;
    case 'n':                       /* -n, get the n of "cover all n-tuples" */
      if (!parse_n( s, &argv[i][2])) return FALSE;
      break;
    case 'w':                                /* -w, "without", a restriction */
      if (!parse_w( s, &argv[i][2])) return FALSE;
      break;
    case 's':                           /* -s, "random", change the behavior */
      if (!parse_s( s, &argv[i][2])) return FALSE;
      break;
    default:
      printf("jenny: legal arguments are numbers, -n, -s, -w, -h, not -%c\n",
	     argv[i][1]);
      return FALSE;
    }
  }                                            /* for (each argument) if '-' */

  if (s->n_final > s->ndim) {
    printf("jenny: %ld-tuples are impossible with only %d dimensions\n",
	   s->n_final, s->ndim);
    return FALSE;
  }

  preliminary(s);            /* allocate structures, do preliminary analysis */

  /* read in any old tests so we can build from that base */
  if (testfile) {
    if (!load( s, testfile)) {
      return FALSE;
    }
  }

  return TRUE;
}

/* print out a single test */
void report( test *t, ub2 len)
{
  ub4 i;
  for (i=0; i<len; ++i) {
    printf(" %d%c", i+1, feature_name[t->f[i]]);
  }
  printf(" \n");
}

/* print out all the tests */
void report_all( state *s)
{
  ub4   i;
  for (i=0; i<s->ntests; ++i) {
    report(s->t[i], s->ndim);
  }
}


void start_builder( state *s, feature *tuple, ub1 n)
{
  ub1 i;
  for (i=0; i<n; ++i) {
    tuple[i].d = i;
    tuple[i].f = 0;
  }
}

int next_builder( state *s, feature *tuple, ub1 n)
{
  sb4 i = n;

  while (i-- &&
	 tuple[i].d == s->ndim-n+i &&
	 tuple[i].f == s->dim[tuple[i].d]-1)
    ;
  if (i == -1)
    return FALSE;
  else if (tuple[i].f < s->dim[tuple[i].d]-1) {
    ++tuple[i].f;                                       /* increment feature */
    for (i; i<n-1; ++i) {            /* reset all less significant positions */
      tuple[i+1].d = tuple[i].d+1;
      tuple[i+1].f = 0;
    }
  }
  else {
    ++tuple[i].d;      /* increment least significant non-maxed-out position */
    tuple[i].f = (ub2)0;                                /* reset its feature */
    for (i; i<n-1; ++i) {            /* reset all less significant positions */
      tuple[i+1].d = tuple[i].d+1;
      tuple[i+1].f = 0;
    }
  }
  return TRUE;
}


void build_tuples( state *s, ub2 d, ub2 f)
{
  feature  offset[MAX_N];                                      /* n-1-tuples */
  feature  tuple[MAX_N];                      /* n-tuples that include (d,f) */
  sb4      i, j, n = s->n[d][f];
  ub8      count = 0;
  test    *t;
  tu_iter  ctx;

  if (s->tc[d][f] > 0 || s->n[d][f] == s->n_final) {
    return;                               /* no need to generate more tuples */
  }

  n = ++s->n[d][f];                        /* move up to a bigger tuple size */

  /* get ready to insert tuples into the tuple list for (d,f) */
  start_tuple(&ctx, &s->tu[d][f], n, &s->tc[d][f]);
  for (i=0; i<n; ++i) {
    tuple[i].d = 0;
    tuple[i].f = 0;
  }

  /* Offset iterates through all n-1-tuples.  Inject (d,f) into each. */
  start_builder( s, offset, n-1);
  for (;;) {
    for (i=0; (i<n-1) && (offset[i].d < d); ++i) {
      tuple[i].d = offset[i].d;
      tuple[i].f = offset[i].f;
    }
    /* can't inject (d,f) into a tuple that already has d */
    if ((i<n-1) && (offset[i].d == d))
      goto make_next_tuple;
    tuple[i].d = d;
    tuple[i].f = f;
    for (++i; i<n; ++i) {
      tuple[i].d = offset[i-1].d;
      tuple[i].f = offset[i-1].f;
    }

    for (i=0; i<n; ++i) {
      s->tuple_tester->f[tuple[i].d] = tuple[i].f;
    }
    if (count_withouts(s->tuple_tester, s->wc2) ||
	count_withouts(s->tuple_tester, s->wc3))
      goto make_next_tuple;

    /* is this tuple covered by the existing tests? */
    for (j=0; j<s->ntests; ++j) {
      test *t = s->t[j];
      for (i=0; i<n; ++i) {
	if (t->f[tuple[i].d] != tuple[i].f) {
	  break;
	}
      }
      if (i == n) {
	goto make_next_tuple;
      }
    }

    /* add it to the list of uncovered tuples */
    if (!insert_tuple(s, &ctx, tuple)) {
      printf("jenny: could not insert tuple\n");
      return;
    }
    ++count;

    /* next tuple */
  make_next_tuple:
    for (i=0; i<n; ++i) {
      s->tuple_tester->f[tuple[i].d] = (ub2)~0;
    }
    if (!next_builder(s, offset, n-1))
      break;
  }
}

/*
 * Tweak the test, other than entries in tuple, so that all withouts are
 * obeyed.
 *
 * Algorithm: loop through all the dimensions touched by any without, changing
 * features in those dimensions so that the total number of withouts disobeyed
 * decreases or stays the same.  Succeed if all withouts are obeyed.  Give up
 * after MAX_NO_PROGRESS consecutive loops that fail to decrease the number of
 * withouts disobeyed.
 *
 * Would it be better to find some disobeyed without, and change one of its
 * dimensions at random?  No.  Consider
 *   jenny 2 2 2 -w1a2a -w1b3a -w1b3a -w2b3a -w2b3a
 * and tentative testcase
 *   1a 2a 3a
 * Progress is impossible unless you change a dimension that is not currently
 * in any disobeyed without.
 */
#define MAX_NO_PROGRESS 2
int obey_withouts(
state *s,                                                    /* global state */
test  *t,                                                /* test being built */
ub1   *mut)              /* mut[i] = 1 if I am allowed to adjust dimension i */
{
  ub4      i;
  without *w;                               /* one of the disobeyed withouts */
  ub4      count;                        /* number of withouts currently hit */
  ub2      ndim;                                         /* size of dimord[] */
  ub2      temp;

  /* how many withouts are currently disobeyed? */
  if (!count_withouts(t, s->wc2))
    return TRUE;

  /* fill dimord[] with all dimensions that can and should be tweaked */
  for (ndim=0, i=0; i<s->ndim; ++i) {
    if (mut[i] && s->wc[i]) {
      s->dimord[ndim++] = i;
    }
  }

  /* hillclimbing, with sidestepping, minimize number of withouts hit */
  for (i=0; i<MAX_NO_PROGRESS; ++i) {
    ub4     j;
    ub2     best[MAX_FEATURES];                      /* best features so far */
    ub1     ok = TRUE;
    
    for (j=ndim; j>0; --j) {
      ub2 fcount = 0;                    /* count of filled elements of best */
      ub2 mydim;                                    /* the current dimension */
      ub2 k;

      /* walk the dimensions in a random order, no replacement */
      mydim = flearand(&s->r) % j;
      temp = s->dimord[mydim];
      s->dimord[mydim] = s->dimord[j-1];
      s->dimord[j-1] = temp;
      mydim = s->dimord[j-1];

      /* see how many withouts this dimension is disobeying */
      count = count_withouts(t, s->wc[mydim]);

      /* test every feature of this dimension, trying to make progress */
      for (k=0; k<s->dim[mydim]; ++k) {
	ub2 newcount;
	t->f[mydim] = k;
	newcount = count_withouts(t, s->wc[mydim]);
	if (newcount <= count) {
	  if (newcount < count) {
	    i = 0;                                      /* partial progress! */
	    fcount = 0;
	    count = newcount;
	  }
	  best[fcount++] = k;
	}
      }

      /* choose one of the best features for this dimension at random */
      if (fcount == 0) {
	printf("jenny: internal error a\n");
      } else if (fcount == 1) {
	t->f[mydim] = best[0];
      } else {
	temp = (flearand(&s->r) % fcount);
	t->f[mydim] = best[temp];
      }

      if (count > 0)
	ok = FALSE;
    }
    if (ok) {                                       /* no withouts disobeyed */
      return TRUE;
    }
  }
  return FALSE;               /* failure, could not satisfy all the withouts */
}

ub4 count_tuples( state *s, test *t, int d, int f)
{
  ub4      count = 0;
  ub1      n = s->n[d][f];
  tu_iter  ctx;
  feature *this = start_tuple(&ctx, &s->tu[d][f], n, &s->tc[d][f]);
  while (this) {
    count += test_tuple(t->f, this, n);
    this = next_tuple(&ctx);
  }
  return count;
}

ub4 maximize_coverage( 
state *s,                                                    /* global state */
test  *t,            /* testcase being built, already obeys all restrictions */
ub1   *mut,                        /* mut[i] = 1 if I can adjust dimension i */
ub1    n)                            /* size of smallest tuple left to cover */
{
  ub1 progress;
  ub2 ndim;
  ub2 i;
  ub4 total;

  /* build a list of all the dimensions that we can modify */
  for (ndim=0, i=0; i<s->ndim; ++i) {
    if (mut[i]) {
      s->dimord[ndim++] = i;
    }
  }

  /* repeatedly loop through all dimensions, maximizing tuple coverage */
  do {
    progress = FALSE;             /* assume no improvement in tuple coverage */
    total    = 1; /* one, for the one fixed tuple we are guaranteed to cover */

    /* scramble the array of dimensions; */
    for (i=ndim; i>1; --i) {
      ub2 j = flearand(&s->r) % i;
      ub2 temp = s->dimord[i-1];
      s->dimord[i-1] = s->dimord[j];
      s->dimord[j] = temp;
    }

    /* for every dimension that we can adjust */
    for (i=0; i<ndim; ++i) {
      ub2 best[MAX_FEATURES];     /* list of features with the best coverage */
      ub2 count = 0;                                       /* size of best[] */
      ub2 d = s->dimord[i];
      ub1 best_n = s->n[d][t->f[d]];
      ub4 coverage = count_tuples(s, t, d, t->f[d]);
      ub4 f;

      /* for every feature in mydim, see if using it would improve coverage */
      for (f=0; f<s->dim[d]; ++f) {
	t->f[d] = f;                            /* switch to the new feature */
	if (!count_withouts(t, s->wc[d])) {         /* need to obey withouts */
	  ub4 new_coverage = count_tuples(s, t, d, f);
	  if (s->n[d][f] < best_n) {
	    best_n = s->n[d][f];
	    progress = TRUE;
	    coverage = new_coverage;
	    count = 0;
	    best[count++] = f;
	  } else if (s->n[d][f] == best_n && new_coverage >= coverage) {
	    if (new_coverage > coverage) {
	      progress = TRUE;
	      coverage = new_coverage;
	      count = 0;
	    }
	    best[count++] = f;
	  }
	}
      }

      /* 
       * Change this dimension to the best features seen.
       * Worst case, everyone was worse than the old value, so best[0] will
       * be the old value.  Coverage will be the same and still no withouts
       * will be hit.
       */
      if (count == 0) {
	printf("jenny: internal error b\n");
      } else if (count == 1) {
	t->f[d] = best[0];
      } else {
	t->f[d] = best[flearand(&s->r) % count];
      }
      if (s->n[d][t->f[d]] == n)
	total += coverage;
    }

  } while (progress);
  return total;
}


/*
 * Generate one test that obeys all the restrictions and covers at 
 * least one tuple.  Do not add it to the list of tests yet.  Return FALSE
 * if we couldn't satisfy the withouts while covering this tuple.
 */
#define MAX_ITERS 10
ub4 generate_test( state *s, test *t, feature *tuple, ub1 n)
{
  int  iter, i;
  ub1  mut[MAX_DIMENSIONS];        /* mut[i] = 1 if I can adjust dimension i */
  ub4  coverage = 0;

  /* mut[i] = 1 if I can modify dimension i */
  for (i=0; i<s->ndim; ++i) mut[i] = 1;
  for (i=0; i<n; ++i) mut[tuple[i].d] = 0;
  
  for (iter=0; iter<MAX_ITERS; ++iter) {
    /* Produce a totally random testcase */
    for (i=0; i<s->ndim; ++i) {
      t->f[i] = flearand(&s->r) % (s->dim[i]);
    }
    
    /* Plug in the chosen new tuple */
    for (i=0; i<n; ++i){
      t->f[tuple[i].d] = tuple[i].f;
    }

    /* If we can get all the withouts obeyed, break, success */
    if (!s->wc2 || obey_withouts(s, t, mut)) {
      if (count_withouts(t, s->wc2)) {
	printf("internal error without %d\n", s->wc2);
      }
      break;
    }
  }

  /* quit if we were unable to satisfy the withouts */
  if (iter == MAX_ITERS) {
    goto done;
  }

  /*
   * We now have a test that covers the new tuple and satisfies withouts.
   * Do hillclimbing to cover as many new tuples as possible.
   */
  coverage = maximize_coverage(s, t, mut, n);

 done:
  return coverage;
}

#define GROUP_SIZE 5
void cover_tuples( state *s)
{
  test *curr_test;
  curr_test = (test *)my_alloc( s, sizeof(test));
  curr_test->f = (ub2 *)my_alloc( s, sizeof(ub2)*s->ndim);

  while (TRUE) {
    ub4         i;
    ub2         d;
    test       *best_test;
    sb4         best_count = -1;
    ub1         tuple_n = MAX_N;
    ub4         tuple_count = 0;
    ub1         covered = FALSE;
    feature    *tuple = (feature *)0;
    ub4         tuple_f, tuple_d;

    /* extend lists of tuples and choose one tuple to cover */
    for (d=0; d<s->ndim; ++d) {
      ub2 f;
      for (f=0; f<s->dim[d]; ++f) {
	build_tuples(s, d, f);
	if (s->n[d][f] < tuple_n) {
	  tuple_n = s->n[d][f];
	  tuple_count = s->tc[d][f];
	  tuple = s->tu[d][f]->fe;
	  tuple_f = f;
	  tuple_d = d;
	} else if (s->n[d][f] == tuple_n && s->tc[d][f] > tuple_count) {
	  tuple_count = s->tc[d][f];
	  tuple = s->tu[d][f]->fe;
	  tuple_f = f;
	  tuple_d = d;
	}
      }
    }

    if (tuple_count == 0) {
      if (tuple_n == s->n_final)
	break;                             /* no more tuples to cover, done! */
      else
	continue;
    }


    best_test = (test *)my_alloc( s, sizeof(test));
    best_test->f = (ub2 *)my_alloc( s, sizeof(ub2)*s->ndim);

    /* find a good test */
    for (i=0; i<GROUP_SIZE; ++i) {
      tu_iter  ctx;
      sb4      this_count;

      /* generate a test that covers the first tuple */
      if (!(this_count = generate_test(s, curr_test, tuple, tuple_n))) {
	continue;
      }
      covered = TRUE;

      /* see how many tuples are covered altogether */
      if (this_count > best_count) {
	test *temp = best_test;
	best_test = curr_test;
	curr_test = temp;

	best_count = this_count;
      }
    }

    if (!covered) {
      wchain  *wc = (wchain *)my_alloc( s, sizeof(wchain));
      without *w = (without *)my_alloc( s, sizeof(without));
      feature  extra[MAX_N];

      /* make a copy of tuple, because we'll be deleting it */
      for (i=0; i<tuple_n; ++i) {
	extra[i].d = tuple[i].d;
	extra[i].f = tuple[i].f;
      }

      printf("Could not cover tuple ");
      show_tuple(tuple, tuple_n);

      /* add this tuple to the list of restrictions */
      wc->w = w;
      wc->next = s->wc3;
      s->wc3 = wc;
      w->fe = (feature *)0;
      w->len = tuple_n;
      w->fe = (feature *)my_alloc( s, sizeof(feature)*tuple_n);
      for (i=0; i<tuple_n; ++i) {
	w->fe[i].d = extra[i].d;
	w->fe[i].f = extra[i].f;
      }
      
      for (d=0; d<s->ndim; ++d) {
	ub2      f;
	tu_iter  ctx;
	for (f=0; f<s->dim[d]; ++f) {
	  ub1      n = s->n[d][f];
	  feature *this = start_tuple(&ctx, &s->tu[d][f], n, &s->tc[d][f]);
	
	  /* remove all the tuples covered by it */
	  while (this) {
	    if (subset_tuple(extra, tuple_n, this, n)) {
	      this = delete_tuple(&ctx);
	    } else {
	      this = next_tuple(&ctx);
	    }
	  }
	}
      }
      my_free((char *)best_test->f);
      my_free((char *)best_test);
    } else {
      ub2      d;
      for (d=0; d<s->ndim; ++d) {
	tu_iter  ctx;
	ub2      f = best_test->f[d];
	ub1      n = s->n[d][f];
	feature *this = start_tuple(&ctx, &s->tu[d][f], n, &s->tc[d][f]);
	
	/* remove all the tuples covered by it */
	while (this) {
	  if (test_tuple(best_test->f, this, n)) {
	    this = delete_tuple(&ctx);
	  } else {
	    this = next_tuple(&ctx);
	  }
	}
      }

      /* add it to the list of tests */
      if (!add_test(s, best_test)) {
	printf("jenny: exceeded maximum number of tests\n");
	my_free((char *)curr_test->f);
	my_free((char *)curr_test);
	my_free((char *)best_test->f);
	my_free((char *)best_test);
	cleanup(s);
	exit(0);
      }
    }
  }

  my_free((char *)curr_test->f);
  my_free((char *)curr_test);
}

void prepare_reduce( state *s) 
{
  feature tuple[MAX_N];
  ub1     n = s->n_final;
  ub4     t, d;

  for (t=0; t<s->ntests; ++t) {
    for (d=0; d<s->ndim; ++d) {
      s->onec[t][d] = 0;
    }
  }

  /* Iterate through all the tuples */
  start_builder( s, tuple, n);

  for (;;) {
    sb4 i;
    ub2 thistest;

    for (i=0; i<n; ++i) {
      s->tuple_tester->f[tuple[i].d] = tuple[i].f;
    }
    if (count_withouts(s->tuple_tester, s->wc2) ||
	count_withouts(s->tuple_tester, s->wc3))
      goto make_next_tuple;

    for (i=0; i<s->ntests; ++i) {
      ub1 j;
      for (j=0; j<n; ++j)
	if (s->t[i]->f[tuple[j].d] != tuple[j].f)
	  break;
      if (j == n)
	break;                              /* this test contains this tuple */
    }

    /* no tests cover this tuple */
    if (i==s->ntests) {
      printf("error: some tuple not covered at all\n");
    } else {
      thistest = i;
      for (++i; i<s->ntests; ++i) {
	ub1 j;
	for (j=0; j<n; ++j)
	  if (s->t[i]->f[tuple[j].d] != tuple[j].f)
	    break;
	if (j == n)
	  break;                            /* this test contains this tuple */
      }
      if (i == s->ntests) {
	ub1 j;
	for (j=0; j<n; ++j) {
	  tu_iter ctx;
	  (void)start_tuple(&ctx, &s->one[thistest][tuple[j].d], n, 
			    &s->onec[thistest][tuple[j].d]);
	  (void)insert_tuple(s, &ctx, tuple);
	}
      }
    }

  make_next_tuple:
    for (i=0; i<n; ++i) {
      s->tuple_tester->f[tuple[i].d] = (ub2)~0;
    }
    if (!next_builder( s, tuple, n))
      break;
  }
}

/* find a test to try to eliminate */
int which_test( state *s)
{
  ub4 t;
  ub4 mincount = ~0;
  ub4 mint = 0;                  /* test with the fewest once-covered tuples */
  for (t=0; t<s->ntests; ++t) {
    ub4 i, j=0;
    for (i=0; i<s->ndim; ++i) {
      j += s->onec[t][i];
    }
    if (j <= mincount) {
      mincount = j;
      mint = t;
    }
  }
  return mint;
}

void reduce_tests( state *s) 
{
  ub4 t;
  prepare_reduce( s);
  t = which_test( s);
}

/* Confirm that every tuple is covered by either a testcase or a without */
int confirm( state *s)
{
  feature  offset[MAX_N];
  sb4      i, j, n = s->n_final;

  /* Make a list of allowed but uncovered tuples */
  for (i=0; i<n; ++i) {
    offset[i].d = i;
    offset[i].f = 0;
  }

  for (;;) {
    for (i=0; i<n; ++i) {
      s->tuple_tester->f[offset[i].d] = offset[i].f;
    }
    if (count_withouts(s->tuple_tester, s->wc2) ||
	count_withouts(s->tuple_tester, s->wc3))
      goto make_next_tuple;

    /* is this tuple covered by the existing tests? */
    for (j=0; j<s->ntests; ++j) {
      test *t = s->t[j];
      for (i=0; i<n; ++i) {
	if (t->f[offset[i].d] != offset[i].f) {
	  break;
	}
      }
      if (i == n) {
	goto make_next_tuple;
      }
    }

    printf("problem with %d%c\n", offset[0].d+1, feature_name[offset[0].f]);
    return FALSE;                       /* found a tuple that is not covered */

  make_next_tuple:
    for (i=0; i<n; ++i) {
      s->tuple_tester->f[offset[i].d] = (ub2)~0;
    }
    i=n;
    while (i-- &&
	   offset[i].d == s->ndim-n+i &&
	   offset[i].f == s->dim[offset[i].d]-1)
      ;
    if (i == -1)
      break;                                                         /* done */
    else if (offset[i].f < s->dim[offset[i].d]-1) {
      ++offset[i].f;                                    /* increment feature */
      for (i; i<n-1; ++i) {          /* reset all less significant positions */
	offset[i+1].d = offset[i].d+1;
	offset[i+1].f = 0;
      }
    }
    else {
      ++offset[i].d;   /* increment least significant non-maxed-out position */
      offset[i].f = (ub2)0;                             /* reset its feature */
      for (i; i<n-1; ++i) {          /* reset all less significant positions */
	offset[i+1].d = offset[i].d+1;
	offset[i+1].f = 0;
      }
    }
  }

  return TRUE;              /* all tuples are covered by a test or a without */
}


void driver( int argc, char *argv[])
{
  state s;                                                 /* internal state */

  initialize(&s);

  if (parse(argc, argv, &s)) {               /* read the user's instructions */
    cover_tuples(&s);     /* generate testcases until all tuples are covered */
    /* reduce_tests(&s); */         /* try to reduce the number of testcases */
    if (confirm(&s))       /* doublecheck that all tuples really are covered */
      report_all(&s);                                  /* report the results */
    else
      printf("jenny: internal error, some tuples not covered\n");
  }
  cleanup(&s);                                      /* deallocate everything */
}

int main( int argc, char *argv[])
{
  driver(argc, argv);
  return 0;
}

