/* hbci-interaction.c
   Copyright 2002 by Christian Stimming <stimming@tuhh.de> */

/***************************************************************************
 *                                                                         *
 *   This library is free software; you can redistribute it and/or         *
 *   modify it under the terms of the GNU Lesser General Public            *
 *   License as published by the Free Software Foundation; either          *
 *   version 2.1 of the License, or (at your option) any later version.    *
 *                                                                         *
 *   This library is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   Lesser General Public License for more details.                       *
 *                                                                         *
 *   You should have received a copy of the GNU Lesser General Public      *
 *   License along with this library; if not, write to the Free Software   *
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston,                 *
 *   MA  02111-1307  USA                                                   *
 *                                                                         *
 ***************************************************************************/

#include <stdio.h>
#include <string.h>
#include "hbci-interaction.h"
#include <openhbci/interactorcb.h>
#include <openhbci/progressmonitorcb.h>


#define LOCALSIZE 1024

static int msgInputPin(const HBCI_User *user,
		char *pinbuf,
		int bufsize,
		int minsize,
		int newPin)
{
    const HBCI_Bank * b;
    char localbuf[LOCALSIZE];

    printf("Pineingabe fuer ");
    if (user) {
        b=HBCI_User_bank(user);
        if (b) {
	    printf("BLZ %s", HBCI_Bank_bankCode(b));
        }
        else
            printf("<unbekannte Bank>");
	printf(", Benutzerkennung %s", HBCI_User_userId(user));
    } // if user is valid
    else
        printf("<unbekannter User>");
    printf("\n");

    // FIXME: only show asterisks, dont use gets !
    gets(localbuf);
    strncpy(pinbuf, localbuf, bufsize);
    memset(localbuf, 0, LOCALSIZE);

    // done
    return 1;
}


static int msgInsertCardOrAbort(const HBCI_User *user)
{
    const HBCI_Bank * b;

    printf("Bitte Karte einlegen fuer ");
    if (user) {
        b=HBCI_User_bank(user);
        if (b) {
	    printf("BLZ %s", HBCI_Bank_bankCode(b));
        }
        else
            printf("<unbekannte Bank>");
    } // if user is valid
    else
        printf("<unbekannter User>");
    printf("\n");
    printf("<Bitte druecken Sie ENTER>\n");
    getchar();

    return 1;
}


static int msgInsertCorrectCardOrAbort(const HBCI_User *user)
{
    const HBCI_Bank *b;

    printf("Bitte korrekte Karte einlegen fuer ");
    if (user) {
        b=HBCI_User_bank(user);
        if (b) {
	    printf("BLZ %s", HBCI_Bank_bankCode(b));
        }
        else
            printf("<unbekannt Bank>");
    } // if user is valid
    else
	printf("<unbekannt Bank>");
    printf("\n");
    printf("<Bitte druecken Sie ENTER>\n");
    getchar();

    return 1;
}


static void msgStateResponse(const char *msg)
{
    fprintf(stdout,"my-msgStateResponse: %s\n",msg);
}

static int keepAlive()
{
    //fprintf(stdout, "my-keepAlive: returning 1\n");
    return 1;
}


HBCI_Interactor *
gnc_hbci_new_interactor()
{
    HBCI_InteractorCB *inter;
    inter = HBCI_InteractorCB_new();

    HBCI_InteractorCB_setMsgInputPin(inter, &msgInputPin);
    HBCI_InteractorCB_setMsgInsertCardOrAbort(inter, &msgInsertCardOrAbort);
    HBCI_InteractorCB_setMsgInsertCorrectCardOrAbort(
	inter,
	&msgInsertCorrectCardOrAbort);
    HBCI_InteractorCB_setMsgStateResponse(inter,
					 &msgStateResponse);
    HBCI_InteractorCB_setKeepalive(inter,
				  &keepAlive) ;

    return HBCI_InteractorCB_Interactor(inter);
}



/* ---------------------------------------------------------- */


static void jobStarted(JobProgressType type, int actions)
{
    const char *msg;
    switch(type){
    case JOB_OPENINGDIALOG:
	msg = "Eröffne Dialog";
	break;
    case JOB_CLOSINGDIALOG:
	msg = "Schließe Dialog";
	break;
	/** Opening network connection. */
    case    JOB_OPENINGNETWORK:
	msg = "Beginne Netzwerkverbindung";
	break;
	/** Closing network connection. */
    case    JOB_CLOSINGNETWORK:
	msg = "Schließe Netzwerkverbindung";
	break;
	/** Get balance */
    case    JOB_GET_BALANCE:
	msg = "Job: Saldo abholen";
	break;
	/** Get transaction statement */
    case    JOB_GET_TRANS:
	msg = "Job: Umsätze abholen";
	break;
	/** Transfer money */
    case    JOB_NEW_TRANSFER:
	msg = "Job: Neue Überweisung";
	break;
	/** Debit note */
    case    JOB_DEBIT_NOTE:
	msg = "";
	break;
	/** Get standing orders */
    case    JOB_GET_STO:
	msg = "";
	break;
	/** Create a new standing order */
    case    JOB_NEW_STO:
	msg = "";
	break;
	/** Delete a standing order */
    case    JOB_DELETE_STO:
	msg = "";
	break;
	/** Get account list */
    case    JOB_GET_ACCOUNTS:
	msg = "Job: Kontenliste abholen";
	break;
	/** Get SystemId */
    case    JOB_GET_SYSTEMID:
	msg = "Job: System-Kennung abgleichen";
	break;
	/** Get keys */
    case    JOB_GET_KEYS:
	msg = "Job: Schlüssel holen";
	break;
	/** Send keys */
    case    JOB_SEND_KEYS:
	msg = "Job: Schlüssel senden";
	break;
    }
    
    fprintf(stdout,"Jobstart (w/ %d actions): %s\n",actions, msg);
}
HBCI_ProgressMonitor *
gnc_hbci_new_pmonitor()
{
    HBCI_ProgressMonitorCB *pmon;
    pmon = HBCI_ProgressMonitorCB_new();

    HBCI_ProgressMonitorCB_setJobStarted(pmon, &jobStarted);
    
    return HBCI_ProgressMonitorCB_ProgressMonitor(pmon);
}
