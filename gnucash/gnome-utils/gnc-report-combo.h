/********************************************************************\
 * gnc-report-combo.h -- report select widget for GnuCash           *
 *                                                                  *
 * Copyright (C) 2022 Bob Fewell                                    *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef GNC_REPORT_COMBO_H
#define GNC_REPORT_COMBO_H

#define GNC_TYPE_REPORT_COMBO            (gnc_report_combo_get_type())
#define GNC_REPORT_COMBO(o)              (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_REPORT_COMBO, GncReportCombo))
#define GNC_REPORT_COMBO_CLASS(k)        (G_TYPE_CHECK_CLASS_CAST ((k), GNC_TYPE_REPORT_COMBO, GncReportComboClass))
#define GNC_IS_REPORT_COMBO(o)           (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_REPORT_COMBO))
#define GNC_IS_REPORT_COMBO_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((obj), GNC_TYPE_REPORT_COMBO))
#define GNC_REPORT_COMBO_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_REPORT_COMBO, GncReportComboClass))

typedef struct
{
    GtkBox box;

} GncReportCombo;

typedef struct
{
    GtkBoxClass parent_class;
    void (*changed) (GncReportCombo *grc);

} GncReportComboClass;

/** Return the GType for the GncReportCombo widget.
 *
 *  @return A GType value.
 */
GType gnc_report_combo_get_type (void) G_GNUC_CONST;

/** Create a new GncReportCombo widget which can be used to provide
 *  a list of reports and select one.
 *
 *  @param guid_scm_function The SCM function to get guids.
 *
 *  @return A GncReportCombo widget.
 */
GtkWidget *gnc_report_combo_new (const char* guid_scm_function);

/** Refresh the report combo model from existing SCM function.
 *
 *  @param grc The report combo widget.
 */
void gnc_report_combo_refresh (GncReportCombo *grc);

/** Set the active report in the GncReportCombo widget.
 *
 *  @param grc The report combo widget.
 *
 *  @param active_report_guid A string representing the report guid
 *
 *  @param active_report_name A string representing the report name
 */
void gnc_report_combo_set_active (GncReportCombo *grc,
                                  const char* active_report_guid,
                                  const char* active_report_name);

/** Get the active report guid string.
 *
 *  @param grc The report combo widget.
 *
 *  @return The string guid of the selected report or NULL if none active
 */
const gchar * gnc_report_combo_get_active_guid (GncReportCombo *grc);

/** Get the active report name string.
 *
 *  @param grc The report combo widget.
 *
 *  @return The string name of the selected report or NULL if none active
 */
const gchar * gnc_report_combo_get_active_name (GncReportCombo *grc);

/** Get the warning image widget.
 *
 *  @param grc The report combo widget.
 *
 *  @return A pointer to the warning image widget
 */
GtkWidget * gnc_report_combo_get_warning_image (GncReportCombo *grc);

#endif /* __GNC_REPORT_COMBO_H__ */



