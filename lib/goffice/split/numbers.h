#ifndef GNUMERIC_NUMBERS_H
#define GNUMERIC_NUMBERS_H

#include <math.h>
#ifdef HAVE_IEEEFP_H
#include <ieeefp.h>
#endif
#ifdef HAVE_IEEE754_H
#include <ieee754.h>
#endif

#ifdef WITH_LONG_DOUBLE

#ifdef HAVE_SUNMATH_H
#include <sunmath.h>
#endif

typedef long double gnm_float;
#ifdef HAVE_STRTOLD
#ifdef MUST_PROTOTYPE_STRTOLD
long double strtold (const char *, char **);
#endif
#define strtognum strtold
#else
#define NEED_FAKE_STRTOGNUM
/* Defined in gutils.c  */
gnm_float strtognum (const char *str, char **end);
#endif

#ifdef HAVE_MODFL
#define modfgnum modfl
#else
#define NEED_FAKE_MODFGNUM
/* Defined in gutils.c  */
gnm_float modfgnum (gnm_float x, gnm_float *iptr);
#endif

#ifdef HAVE_LDEXPL
#define ldexpgnum ldexpl
#else
#define NEED_FAKE_LDEXPGNUM
/* Defined in gutils.c  */
gnm_float ldexpgnum (gnm_float x, int exp);
#endif

#ifdef HAVE_FREXPL
#define frexpgnum frexpl
#else
#define NEED_FAKE_FREXPGNUM
/* Defined in gutils.c  */
gnm_float frexpgnum (gnm_float x, int *exp);
#endif

#ifdef HAVE_ERF
#define erfgnum erfl
#else
#define NEED_FAKE_ERFGNUM
/* Defined in gutils.c  */
gnm_float erfgnum (gnm_float x);
#endif

#ifdef HAVE_ERFC
#define erfcgnum erfcl
#else
#define NEED_FAKE_ERFCGNUM
/* Defined in gutils.c  */
gnm_float erfcgnum (gnm_float x);
#endif

#ifdef HAVE_YNL
#define yngnum ynl
#else
#define NEED_FAKE_YNGNUM
/* Defined in gutils.c  */
gnm_float yngnum (int n, gnm_float x);
#endif

#define acosgnum acosl
#define acoshgnum acoshl
#define asingnum asinl
#define asinhgnum asinhl
#define atan2gnum atan2l
#define atangnum atanl
#define atanhgnum atanhl
#define ceilgnum ceill
#define cosgnum cosl
#define coshgnum coshl
#define expgnum expl
#define expm1gnum expm1l
#define finitegnum finitel
#define floorgnum floorl
#define fmodgnum fmodl
#define gnumabs fabsl
#define hypotgnum hypotl
#define isnangnum isnanl
#define lgammagnum lgammal
#define lgamma_rgnum lgammal_r
#define log10gnum log10l
#define log1pgnum log1pl
#define loggnum logl
#define powgnum powl
#define singnum sinl
#define sinhgnum sinhl
#define sqrtgnum sqrtl
#define tangnum tanl
#define tanhgnum tanhl

#define GNUM_FORMAT_e "Le"
#define GNUM_FORMAT_E "LE"
#define GNUM_FORMAT_f "Lf"
#define GNUM_FORMAT_g "Lg"
#define GNUM_DIG LDBL_DIG
#define GNUM_MANT_DIG LDBL_MANT_DIG
#define GNUM_MIN_EXP LDBL_MIN_EXP
#define GNUM_MAX_EXP LDBL_MAX_EXP
#define GNUM_MIN LDBL_MIN
#define GNUM_MAX LDBL_MAX
#define GNUM_EPSILON LDBL_EPSILON
#define GNM_const(_c) _c ## L

#else /* !WITH_LONG_DOUBLE */

typedef double gnm_float;

#define acosgnum acos
#define acoshgnum acosh
#define asingnum asin
#define asinhgnum asinh
#define atan2gnum atan2
#define atangnum atan
#define atanhgnum atanh
#define ceilgnum ceil
#define cosgnum cos
#define coshgnum cosh
#define erfcgnum erfc
#define erfgnum erf
#define expgnum exp
#define expm1gnum expm1
#define floorgnum floor
#define fmodgnum fmod
#define frexpgnum frexp
#define gnumabs fabs
#define hypotgnum hypot
#define isnangnum isnan
#define ldexpgnum ldexp
#define lgammagnum lgamma
#define lgamma_rgnum lgamma_r
#define log10gnum log10
#define log1pgnum log1p
#define loggnum log
#define modfgnum modf
#define powgnum pow
#define singnum sin
#define sinhgnum sinh
#define sqrtgnum sqrt
#define strtognum strtod
#define tangnum tan
#define tanhgnum tanh
#define yngnum yn

/* What a circus!  */
#ifdef HAVE_FINITE
#define finitegnum finite
#elif defined(HAVE_ISFINITE)
#define finitegnum isfinite
#elif defined(FINITE)
#define finitegnum FINITE
#error "I don't know an equivalent of finite for your system; you lose"
#endif

#ifndef HAVE_LGAMMA_R
#define NEED_FAKE_LGAMMA_R
/* Defined in gutils.c  */
gnm_float lgamma_rgnum (gnm_float x, int *signp);
#endif

#ifndef HAVE_EXPM1
#define NEED_FAKE_EXPM1
/* Defined in gutils.c  */
gnm_float expm1 (gnm_float x);
#endif

#ifndef HAVE_ASINH
#define NEED_FAKE_ASINH
/* Defined in gutils.c  */
gnm_float asinh (gnm_float x);
#endif

#ifndef HAVE_ACOSH
#define NEED_FAKE_ACOSH
/* Defined in gutils.c  */
gnm_float acosh (gnm_float x);
#endif

#ifndef HAVE_ATANH
#define NEED_FAKE_ATANH
/* Defined in gutils.c  */
gnm_float atanh (gnm_float x);
#endif

#define GNUM_FORMAT_e "e"
#define GNUM_FORMAT_E "E"
#define GNUM_FORMAT_f "f"
#define GNUM_FORMAT_g "g"
#define GNUM_DIG DBL_DIG
#define GNUM_MANT_DIG DBL_MANT_DIG
#define GNUM_MIN_EXP DBL_MIN_EXP
#define GNUM_MAX_EXP DBL_MAX_EXP
#define GNUM_MIN DBL_MIN
#define GNUM_MAX DBL_MAX
#define GNUM_EPSILON DBL_EPSILON
#define GNM_const(_c) _c

#endif

#endif /* GNUMERIC_NUMBERS_H */
