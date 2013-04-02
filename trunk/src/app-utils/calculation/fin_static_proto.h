/***************************************************************************
 *              -------------------
 *    create   : Tue Jul 11 20:21:20 2000
 *    copyright: (C) 2000 by Terry D. Boldt
 *    email    : tboldt@attglobal.net
 *              -------------------
 ***************************************************************************/
/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
/***************************************************************************
 *  Static Function Prototypes
 *  Tue Jul 11 20:21:20 2000
 *
 ***************************************************************************/

#if defined( FIN_STATICS )
/*==================================================*/
/* fin.c
 */
/* Line Number: 614 */
static double            rnd(
    double x,
    unsigned places);
/* Line Number: 628 */
static double            dabs(
    double x);
/* Line Number: 634 */
static double            _A(
    double eint,
    unsigned per);
/* Line Number: 1181 */
static double            _B(
    double eint,
    unsigned beg);
/* Line Number: 1394 */
static double            nom_int(
    double eint,
    unsigned CF,
    unsigned PF,
    unsigned disc);
/* Line Number: 781 */
static double            eff_int(
    double nint,
    unsigned CF,
    unsigned PF,
    unsigned disc);
/* Line Number: 798 */
static double            fi(
    unsigned per,
    double eint,
    double pv,
    double pmt,
    double fv,
    unsigned bep);
/* Line Number: 1449 */
static double            fip(
    unsigned per,
    double eint,
    double pv,
    double pmt,
    double fv,
    unsigned bep);
#endif /* FIN_STATICS */

#if defined( EXPRESSION_PARSER_STATICS )
/*==================================================*/
/* expression_parser.c
 */
/* Line Number: 485 */
static var_store_ptr     pop(
    parser_env_ptr pe);
/* Line Number: 321 */
static var_store_ptr     push(
    var_store_ptr push_value,
    parser_env_ptr pe);
/* Line Number: 519 */
static var_store_ptr     get_named_var(
    parser_env_ptr pe);
/* Line Number: 366 */
static var_store_ptr     get_unnamed_var(
    parser_env_ptr pe);
/* Line Number: 579 */
static void              free_var(
    var_store_ptr value,
    parser_env_ptr pe);
/* Line Number: 596 */
static void              next_token(
    parser_env_ptr pe);
/* Line Number: 426 */
static void              assignment_op(
    parser_env_ptr pe);
/* Line Number: 695 */
static void              add_sub_op(
    parser_env_ptr pe);
/* Line Number: 464 */
static void              multiply_divide_op(
    parser_env_ptr pe);
/* Line Number: 488 */
static void              primary_exp(
    parser_env_ptr pe);
#endif /* EXPRESSION_PARSER_STATICS */

#if defined( NUMERIC_OPS_STATICS )
/*==================================================*/
/* numeric_ops.c
 */
#endif /* NUMERIC_OPS_STATICS */

#if defined( AMORT_OPT_STATICS )
/*==================================================*/
/* amort_opt.c
 */
#endif /* AMORT_OPT_STATICS */

#if defined( AMORT_PRT_STATICS )
/*==================================================*/
/* amort_prt.c
 */
#endif /* AMORT_PRT_STATICS */

