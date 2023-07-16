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

#ifdef __cplusplus
extern "C"
{
#endif

#define GNC_TYPE_REPORT_COMBO            (gnc_report_combo_get_type())
G_DECLARE_FINAL_TYPE (GncReportCombo, gnc_report_combo, GNC, REPORT_COMBO, GtkBox)

typedef struct
{
    char *report_guid;
    char *report_name;
} ReportListEntry;

/** Create a new GncReportCombo widget which can be used to provide
 *  a list of reports and select one.
 *
 *  @param report_list The list of report guids to populate the model
 *
 *  @return A GncReportCombo widget.
 */
GtkWidget *gnc_report_combo_new (GSList *report_list);

/** Refresh the report combo model.
 *
 *  @param grc The report combo widget.
 *
 *  @param report_list The report list to update the combo with.
 */
void gnc_report_combo_refresh (GncReportCombo *grc, GSList *report_list);

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
gchar * gnc_report_combo_get_active_guid (GncReportCombo *grc);

/** Get the active report name string.
 *
 *  @param grc The report combo widget.
 *
 *  @return The string name of the selected report or NULL if none active
 */
gchar * gnc_report_combo_get_active_name (GncReportCombo *grc);

/** Set the active report to the guid string
 *
 *  @param grc The report combo widget.
 *
 *  @param guid_name The concatination of the guid/name of the Invoice Report
 */
void gnc_report_combo_set_active_guid_name (GncReportCombo *grc,
                                            const gchar *guid_name);

/** Get the active report name string.
 *
 *  @param grc The report combo widget.
 *
 *  @return The concatinated string of report guid and name of the selected
 *          report or NULL if none active
 */
gchar * gnc_report_combo_get_active_guid_name (GncReportCombo *grc);

/** Is the warning displayed for active entry.
 *
 *  @param grc The report combo widget.
 *
 *  @return TRUE is warning is displayed, else FALSE
 */
gboolean gnc_report_combo_is_warning_visible_for_active (GncReportCombo *grc);

#ifdef __cplusplus
}
#endif

#endif /* __GNC_REPORT_COMBO_H__ */
