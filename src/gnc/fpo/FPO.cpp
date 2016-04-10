/********************************************************************\
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

#include "FPO.hpp"
#include "gnc/dashboard.hpp"
#include "gnc/mainwindow.hpp"

#include <QLabel>

namespace gnc
{

FPO::FPO(QWidget *parent, QHBoxLayout *FPOLayout) :
    QWidget(parent)
{
    /* Left viewlet */
    leftViewlet = new ViewletView(parent, FPOLayout);
    leftViewlet->leftVSet(parent, FPOLayout);

    /* Right viewlet */
    rightViewlet = new ViewletView(parent, FPOLayout);
    rightViewlet->rightVSet(parent, FPOLayout);

    /* Default viewlet */
    defaultViewlet = new ViewletView(parent, FPOLayout);
    defaultViewlet->defaultVSet(parent, FPOLayout);
}

} // END namespace gnc
