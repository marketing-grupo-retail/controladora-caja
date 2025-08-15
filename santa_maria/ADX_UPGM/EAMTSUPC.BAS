\/* TIME STAMP BLOCK **********************************************
\** END OF TIME STAMP BLOCK **************************************/
\/****************************************************************/
\/*                                                              */
\/*      MODULE NAME: EAMTSUPC                                   */
\/*                                                              */
\/*      DESCRIPTIVE NAME: TERMINAL SALES USER EXITS             */
\/*                                                              */
\/*                                                               */
\/*      COPYRIGHT:                                               */
\/*      5696-536 THIS MODULE IS "RESTRICTED MATERIALS OF IBM"    */
\/*      (C) COPYRIGHT IBM CORP 1986, 1993 ALL RIGHTS RESERVED    */
\/*      LICENSED MATERIALS - PROPERTY OF IBM REFER TO COPYRIGHT  */
\/*      INSTRUCTIONS FORM NUMBER G120-2083                       */
\/*                                                               */
\/*      STATUS: RELEASE 2 LEVEL 0                                */
\/*                                                              */
\/*      PERSON RESPONSIBLE =                                    */
\/*                                                              */
\/*      FUNCTION: EAMTSUPC                                      */
\/*                                                              */
\/*                THIS MODULE CONTAINS THE EXTERNAL FUNCTIONS   */
\/*                DEFINING THE USER EXITS FOR TERMINAL SALES.   */
\/*                                                              */
\/*      NOTES:                                                  */
\/*                                                              */
\/*         DEPENDENCIES: NONE                                   */
\/*                                                              */
\/*         RESTRICTIONS: NONE                                   */
\/*                                                              */
\/*         REGISTER CONVENTIONS:                                */
\/*                                                              */
\/*            RESTRICTED REGISTERS = N/A                        */
\/*                                                              */
\/*            REGISTERS USED = N/A                              */
\/*                                                              */
\/*      MODULE TYPE: CBASIC                                     */
\/*                                                              */
\/*         PROCESSOR: CBASIC COMPILER                           */
\/*                                                              */
\/*         MODULE SIZE: 200 LOC                                 */
\/*                                                              */
\/*         ATTRIBUTES: REENTRANT                                */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC01                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED PRIOR TO FIRST ENTRY IN A  */
\/*                  TRANSACTION.                                */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: AT START OF CHECKOUT TRANSACTION FROM       */
\/*                  MAINLINE.                                   */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC02                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AFTER A CUSTOMER CHECKOUT  */
\/*                  TRANSACTION COMPLETES.                      */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: FROM TSETEC01 AT EOT.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC03                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED WHEN A TIMER OCCURS BETWEEN*/
\/*                  TRANSACTIONS.                               */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TS11EC01.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC04                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED BEFORE PERFORMING ANY      */
\/*                  NON-SALES TRANSACTION.                      */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSCSEC09 (NON-SALES INIT)       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC05                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AFTER ENDING ANY NON-SALES */
\/*                  TRANSACTION.                                */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSCSEC10 (NON-SALES TERMINATION)*/
\/*                                                              */
\/*      ENTRY POINT: TSUPEC06                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED BEFORE PROCESSING ANY      */
\/*                  INVALID PROCEDURE NUMBER.                   */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TS11EC01.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC07                                   */
\/*                                                              */
\/*         PURPOSE: CALLED AFTER OPENING ALL FILES.             */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED AT END OF TERMINAL INITIALIZATION    */
\/*                  IN TSINEC01.                                */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC08                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AFTER THE READ OF AN ITEM  */
\/*                  RECORD.                                     */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSIEEC01.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC09                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AFTER THE READ OF AN       */
\/*                  OPERATOR AUTHORIZATION RECORD.              */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSSOEC01.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC10                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AFTER THE READ OF A        */
\/*                  TERMINAL STATUS RECORD.                     */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSSFEC01.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC11                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AFTER THE READ OF AN       */
\/*                  OPERATOR STATUS RECORD.                     */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSSFEC01.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC12                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AFTER THE READ OF A TENDER */
\/*                  LISTING RECORD.                             */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSTLEC01.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC13                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AFTER THE READ OF A TENDER */
\/*                  VERIFICATION RECORD.                        */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSCSEC01.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC14                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AFTER THE READ OF THE      */
\/*                  KEYBOARD/SCANNER/WAND.                      */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TS11EC01.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC15                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AFTER THE READ OF THE SCALE*/
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSIEEC01                        */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC16                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AFTER THE READ OF THE      */
\/*                  EXTERNAL TENDER VERIFICATION PIPE.          */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FORM TSCSEC01.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC17                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED BEFORE WRITING EOT DATA TO */
\/*                  THE HARD TOTALS.                            */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSTPEC02.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC18                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED BEFORE WRITING A           */
\/*                  TRANSACTION SUMMARY LOG RECORD.             */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSTPEC02,TSTPEC10,TSTPEC06      */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC19                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED BEFORE IMMEDIATE OR        */
\/*                  DELAYED UPDATE TO ITEM RECORD               */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSPCEC01.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC20                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED BEFORE WRITING A LINE TO   */
\/*                  THE CUSTOMER RECEIPT.                       */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSHIECPR.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC21                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED BEFORE WRITING A LINE TO   */
\/*                  THE SUMMARY JOURNAL.                        */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSHIECPR.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC22                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED BEFORE WRITING A LINE TO   */
\/*                  THE DOCUMENT INSERT STATION.                */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSHIECPR.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC23                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED BEFORE WRITING A LINE TO   */
\/*                  THE DISPLAY.                                */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSHIECDS.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC24                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED BEFORE OPENING THE CASH    */
\/*                  DRAWER.                                     */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSCSEC06.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC25                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED BEFORE SOUNDING THE TONE.  */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSHIECET.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC26                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED BEFORE WRITING A MESSAGE TO*/
\/*                  THE EXTERNAL TENDER VERIFICATION PIPE.      */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSTPEC02.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC27                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AT PROCESSING OF AN IO     */
\/*                  ERROR.                                      */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TS11EW01.                       */
\/*                                                              */
\/* AIO05390                                                     */
\/*      ENTRY POINT: TSUPEA27                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AT PROCESSING OF AN        */
\/*                  ASYNCHRONOUS IO ERROR.                      */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TS11EW01.                       */
\/* EIO05390                                                     */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC28                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AT SIGNON VALIDATION.      */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSSOEC01.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC29                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AT THE TAX CALCULATION.    */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSTXEC01.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC30                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AT THE BALANCE DUE         */
\/*                  CALCULATION.                                */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSBDEC01.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC31                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AT DATA ENTRY PROCESSING.  */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TS11EC01.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC32                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AT TENDER VALIDATION.      */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSCSEC01.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC33                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AT RECOVERY OF TOTALS      */
\/*                  PROCESSING.                                 */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM ???.                            */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC34                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AT STANDALONE ENTRY.       */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TS11EC01.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC35                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AT STANDALONE EXIT.        */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSSFEC01.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC36                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AT MANAGER'S OVERRIDE      */
\/*                  PROCESSING.                                 */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TS11EC01.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC37                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AT ERROR GUIDANCE          */
\/*                  PROCESSING.                                 */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TS11EC01.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC38                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AT DEPARTMENT KEY          */
\/*                  PROCESSING.                                 */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSIEEC01.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC39                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AT ITEM ENTRY LIMIT        */
\/*                  CHECKING                                    */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSIEEC01.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC40                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AT TENDER LIMIT CHECKING.  */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSCSEC02.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC42                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AT COUPON VALIDATION.      */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSIEEC01.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC43                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AT PRICE COMPUTATION.      */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSIEEC01.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC44                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AT DISCOUNT COMPUTATION.   */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSBDEC02.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC45                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AT CASH DRAWER LIMIT       */
\/*                  CHECKING.                                   */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSTPEC01.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC46                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AT TRADING STAMP           */
\/*                  CALCULATION.                                */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSETEC01.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC47                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AT EFTS SERIAL             */
\/*                  INITIALIZATION                              */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSRFECML.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC48                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AT EFTS SERIAL             */
\/*                  TERMINATION                                 */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSRFECML.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC49                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED BEFORE EFTS SERIAL WRITE   */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSDLECML.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC50                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AFTER EFTS SERIAL READ     */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSDLECML.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC51                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED BEFORE IO PROCESSOR UNLOCK */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TS11EC01.                       */
\/*                                                              */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC52                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AFTER READ OF MSR IR75105  */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TS11EC01.                       */
\/*                                                              */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC53                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AT TRANSACTION    IR80607  */
\/*                  RECOVERY BEFORE PROCESSING A                */
\/*                  RECOVERED STRING                            */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TS??????.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC54                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AFTER "WAIT" timedout      */
\/*                  on the IOPROC.  IR82110                     */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TS11EC01.                       */
\/*                                                              */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC55                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED FOR BARCODES      IR80166  */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TS??????.                       */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC56                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED BEFORE A WRITE             */
\/*                  TO THE SEQUENTIAL FILE DURING               */
\/*                  THE SUSPENSION PROCESS             IR90981  */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM SUSPEND.RETRIEVE                */
\/*                              (EAMTSSRC.B86)                  */
\/*AIR39146                                                      */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC58                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED PRIOR TO AUTO              */
\/*                  POWER DOWN OF TERMINAL.                     */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM MAIN LINE                       */
\/*                              (EAMTS11C.B86)                  */
\/*                                                              */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC59                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AFTER WRITING              */
\/*                  TRANSACTION LOG DATA.                       */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSTPEC02,TSTPEC10,TSTPEC06      */
\/*                                                              */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC60                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AFTER WRITING              */
\/*                  TO THE CUSTOMER RECEIPT                     */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSHIECPR.                       */
\/*                                                              */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC61                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AFTER WRITING              */
\/*                  TO THE SUMMARY JOURNAL                      */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSHIECPR.                       */
\/*                                                              */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC62                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AFTER WRITING              */
\/*                  TO THE DOCUMENT INSERT STATION              */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSHIECPR.                       */
\/*                                                              */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC63                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AFTER WRITING              */
\/*                  TO THE DISPLAY                              */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSHIECPR.                       */
\/*                                                              */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC64                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AFTER OPENING              */
\/*                  THE CASH DRAWER                             */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSHIECPR.                       */
\/*                                                              */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC65                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AFTER WRITING A            */
\/*                  MESSAGE TO THE EXTERNAL VERIFICATION        */
\/*                  PIPE                                        */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM TSHIECPR.                       */
\/*                                                              */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC66                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AFTER MANAGER              */
\/*                  OVERRIDE PROCESSING/VERIFICATION IS         */
\/*                  COMPLETE                                    */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM ????????? focus                 */
\/*                                                              */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC67                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AFTER PRICE                */
\/*                  COMPUTATION IS COMPLETE                     */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM ????????? focus                 */
\/*                                                              */
\/*                                                              */
\/*EIR39146                                                      */
\/*                                                              */
\/*      ENTRY POINT: TSUPEC57                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AFTER THE READ             */
\/*                  FROM THE SEQUENTIAL FILE DURING             */
\/*                  THE RETRIEVAL PROCESS              IR90981  */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM SUSPEND.RETRIEVE                */
\/*                              (EAMTSSRC.B86)                  */
\/*                                                              */
\/*      EXIT-NORMAL: RETURN TO CALLER                           */
\/*                                                              */
\/*      EXIT-ERROR: NONE                                        */
\/*                                                              */
\/*         RETURN CODE:                                         */
\/*                                                              */
\/*            REASON CODE:                                      */
\/*            MESSAGE ID:                                       */
\/*                                                              */
\/*         ABEND CODES:                                         */
\/*                                                              */
\/*          ERROR MESSAGES:                                     */
\/*                                                              */
\/*      EXTERNAL REFERENCES:                                    */
\/*                                                              */
\/*         ROUTINES:                                            */
\/*             TSPREC01 : PRINT ROUTINE                         */
\/*             TSDSEC01 : DISPLAY ROUTINE                       */
\/*             TSCSEC03 : STACK ERROR GUIDANCE                  */
\/*             TSCSEC06 : OPEN CASH DRAWER                      */
\/*             TSCSEC08 : DISPLAY GUIDANCE AND WAIT FOR CLEAR   */
\/*             TSROEC01 : READ TERMINAL OPTIONS                 */
\/*             TSTPEC01 : ADD DATA ENTRY STRING TO SUMMARY LOG  */
\/*             TSTPEC06 : WRITE EXCEPTION LOG ENTRY             */
\/*             TSHIEC01 : OPEN THE CASH DRAWER                  */
\/*             TSHIEC02 : GET STATUS OF CASH DRAWER             */
\/*             TSHIEC04 : GET STATUS OF DOCUMENT INSERT STATION */
\/*             TSHIEC05 : READ THE SCALE                        */
\/*             TSHIECET : SOUND THE TONE                        */
\/*                                                              */
\/*         DATA AREAS:                                          */
\/*                                                              */
\/*           MODIFIED:                                          */
\/*                                                              */
\/*           REFERENCED:                                        */
\/*                                                              */
\/*         CONTROL BLOCKS: NONE                                 */
\/*                                                              */
\/*         MACROS: NONE                                         */
\/*                                                              */
\/*         TABLES: NONE                                         */
\/*                                                              */
\/*      CHANGE ACTIVITY: LEVEL 0                                */
\/*                                                              */
\/* IR68987 6/20/86 R. MEARES ADDED INCLUDE FOR SYSTEM SERVICES  */
\/*                                                              */
\/* IR70123 6/20/86 JSJ G53 ADDED ADDITIONAL DOCUMENTATION       */
\/*                                                              */
\/* IR73623 5/5/87 JSJ G53 ADDED SUPPORT FOR FLAG TO BE          */
\/*         REFERENCED IN USER EXIT 18.  IF THE FLAG IS ON       */
\/*         USER CAN ASSUME THE EXIT WAS CALLED IMMEDIATELY      */
\/*         BEFORE A WRITE TO THE TRANSACTION SUMMARY LOG OF     */
\/*         TRANSACTION DATA.  IF THE FLAG IS OFF, THE USER      */
\/*         CAN ASSUME THAT THE EXIT WAS CALLED BEFORE THE       */
\/*         TERMINAL WRITES EXCEPTION LOG INFORMATION TO THE     */
\/*         SUMMARY LOG.                                         */
\/*                                                              */
\/* IR75105 8/11/87 CFB G53 ADDED USER EXIT 52, AFTER READ OF MSR*/
\/*                                                              */
\/* IR76369 2/3/88 CFB F97 ADDED DOC ON DISPLAY LINETYPE 13      */
\/*                        AND PRINT LINETYPE 30 USED FOR        */
\/*                        FEATURES.                             */
\/*                                                              */
\/* IR80062 9/30/88 SMY 891 ADDED INPUT/OUTPUT PARAMETERS TO     */
\/*                         USER EXIT 26.                        */
\/*                                                              */
\/* IR80607 11/7/88 CJH IBM ADDED USER EXIT 53, AT TRANS RECOVERY*/
\/*                                                              */
\/* IR80572 11/8/8 CJH IBM   ADDED GLOBAL VARIABLE TS.TLOG.CLEAR */
\/*                          FOR USER EXIT 2.                    */
\/*                                                              */
\/* IR80446 11/29/88 SMY IBM UPDATED VARIABLES AVAILABLE FOR     */
\/*                          REFERENCE FOR USER EXIT 14.         */
\/*                                                              */
\/* IR80166  3/07/89 NCW DCL USER EXIT ADDED FOR BARCODES        */
\/*                                                              */
\/* IR80239  4/14/89 MCM IBM ADDED GLOBAL VARIABLES FOR DUAL     */
\/*                          DISPLAY ENHANCEMENT (TSUPEC23).     */
\/*                                                              */
\/* IR81279 12/23/88 CMJ ADDED GLOBAL VAR. FOR UPDATE TO UE 16.  */
\/*                                                              */
\/*       IR82110     New user exit (TSUPEC54) added. This exit  */
\/*                   was added to allow user to perform general */
\/*                   timeout processing. The exit is called from*/
\/*                   the main loop after the "WAIT" timesout on */
\/*                   the IOPROC.                                */
\/*                   The combination of user exits 14,51 and 54 */
\/*                   should now allow maximum flexability in    */
\/*                   this area.                                 */
\/*                   HWN IBM/G53  1 MAR 89                      */
\/*                                                              */
\/* IR87595 12/12/89 MCM IBM ADDED PARAMETERS FOR TSUPEC55       */
\/*                         (USER EXIT USED  FOR BARCODES)       */
\/*                                                              */
\/* IR89353 05/01/90 MCM IBM ADDED VARIABLES AVAILABLE TO        */
\/*                          TSUPEC27 (AT I/O ERROR PROCESSING)  */
\/*                                                              */
\/* IR92088 05/01/90 JJG IBM CHANGED REMARK ABOUT VARIABLE       */
\/*                          AVAILABLE FOR UPDATE FROM           */
\/*                          BYPASS.REPRINT TO BYPASS.REPRINT%   */
\/*                                                              */
\/* IR90981 02/06/91 TNM IBM ADDED USER EXIT 56 AND 57 FOR       */
\/*                          SUSPEND/RETRIEVE                    */
\/*                                                              */
\/* IR99587 01/05/93 BAH MGVA Added comments for TS.LINEDATA = 2 */
\/*                           when printing the Total Number of  */
\/*                           Items Sold on the CR.              */
\/*                           Added comments for TS.LINETYPE 6   */
\/*                           to include Total Number of Items   */
\/*                           Sold print line.                   */
\/*                                                              */
\/* IR39146  User Exits added: 58 - 67                           */
\/*          See below for desciptions of the user exits.        */
\/*          RSH MGVA 12Nov98                                    */
\/*                                                              */
\/* IR39696  Added comments for changes to the TS.LINETYPE and   */
\/*          TS.LINEDATA values for print lines specific to the  */
\/*          Multiple Currency Feature.                          */
\/*          RSH MGVA 25Feb99                                    */
\/*                                                              */
\/* IR40389  Add GLOBAL declarations for customers requiring     */
\/*          EN45501 conformance.                                */
\/*          RSH MGVA 26Apr99                                    */
\/*                                                              */
\/* IO05390  Design change.  This is the second part of a        */
\/*          two-step fix begun with IR51337.  That fix was      */
\/*          implemented to prevent customer-supplied user exit  */
\/*          code from running during asynchronous errors.       */
\/*          With this second fix, capability for                */
\/*          customer-supplied user exit code for asynchronous   */
\/*          errors is restored, but with a differently-named,   */
\/*          new user exit routine, TSUPEA27.  Accompanying      */
\/*          this new user exit stub is a set of instructions    */
\/*          that must be carefully followed in order to         */
\/*          prevent the problems that necessitated IR51337.     */
\/*          GGK IBM 13Oct2006                                   */
\/*                                                              */
\/* END-OF-SPECIFICATIONS ****************************************/

\ REM!!**************** INCLUDE FILES  ********************************
\ REM!!
\ REM!! %INCLUDE EAMTSWKG.J86        ! working storage
\ REM!! %INCLUDE EAMETWKG.J86        ! EFTS working storage
\ REM!! %INCLUDE EAMITEMR.J86        ! item record file
\ REM!! %INCLUDE EAMOSTAT.J86        ! operator status file
\ REM!! %INCLUDE EAMTERMS.J86        ! terminal status file
\ REM!! %INCLUDE EAMTRANS.J86        ! transaction summary log
\ REM!! %INCLUDE EAMTAXTB.J86        ! tax tables
\ REM!! %INCLUDE EAMASMRT.J86        ! assembler subroutines
\ REM!! %INCLUDE EAMADXRT.J86        ! system services functions  IR68987
\ REM!! %INCLUDE EAMSOPTS.J86        ! store options
\ REM!! %INCLUDE EAMTOPTS.J86        ! terminal options
\ REM!! %INCLUDE EAMTSVER.J86        ! totals save file
\ REM!! %INCLUDE EAMTSUVA.J86        ! user exit global variables
\ REM!! %INCLUDE EAMXXCPY.J86        ! copyright statement
\ REM!! %INCLUDE EAMTSUSU.J86        ! user exit common subroutines
\ REM!! %INCLUDE EAMTSU01.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU02.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU03.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU04.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU05.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU06.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU07.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU08.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU09.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU10.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU11.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU12.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU13.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU14.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU15.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU16.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU17.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU18.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU19.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU20.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU21.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU22.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU23.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU24.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU25.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU26.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU27.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU28.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU29.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU30.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU31.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU32.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU33.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU34.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU35.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU36.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU37.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU38.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU39.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU40.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU41.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU42.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU43.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU44.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU45.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU46.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU47.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU48.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU49 J86        ! user exit code
\ REM!! %INCLUDE EAMTSU50.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU51.J86        ! user exit code
\ REM!! %INCLUDE EAMTSU52.J86        ! user exit code  IR75105
\ REM!! %INCLUDE EAMTSU53.J86        ! USER EXIT CODE  IR80607
\ REM!! %INCLUDE EAMTSU54.J86        ! USER EXIT CODE  IR82110
\ REM!! %INCLUDE EAMTSU55.J86        ! USER EXIT CODE  IR80166
\ REM!! %INCLUDE EAMTSU56.J86        ! USER EXIT CODE  IR90981
\ REM!! %INCLUDE EAMTSU57.J86        ! USER EXIT CODE  IR90981
!AIR39146
\ REM!! %INCLUDE EAMTSU58.J86        ! USER EXIT CODE
\ REM!! %INCLUDE EAMTSU59.J86        ! USER EXIT CODE
\ REM!! %INCLUDE EAMTSU60.J86        ! USER EXIT CODE
\ REM!! %INCLUDE EAMTSU61.J86        ! USER EXIT CODE
\ REM!! %INCLUDE EAMTSU62.J86        ! USER EXIT CODE
\ REM!! %INCLUDE EAMTSU63.J86        ! USER EXIT CODE
\ REM!! %INCLUDE EAMTSU64.J86        ! USER EXIT CODE
\ REM!! %INCLUDE EAMTSU65.J86        ! USER EXIT CODE
\ REM!! %INCLUDE EAMTSU66.J86        ! USER EXIT CODE
\ REM!! %INCLUDE EAMTSU67.J86        ! USER EXIT CODE
!EIR39146
\ REM!!
\ REM!!****************************************************************

%ENVIRON T

\ REM!!**************** INCLUDE FILES  ********************************

%INCLUDE EAMTSWKG.J86        ! working storage
%INCLUDE EAMETWKG.J86        ! EFTS working storage
%INCLUDE EAMITEMR.J86        ! item record file
%INCLUDE EAMOSTAT.J86        ! operator status file
%INCLUDE EAMTERMS.J86        ! terminal status file
%INCLUDE EAMTRANS.J86        ! transaction summary log
%INCLUDE EAMTAXTB.J86        ! tax tables
%INCLUDE EAMSOPTS.J86        ! store options
%INCLUDE EAMTOPTS.J86        ! terminal options
%INCLUDE EAMTSVER.J86        ! totals save file
!AIR40389 - Added for EN45501 Support
INTEGER*4 GLOBAL             \
          EN45501.ADDR(2),   \ Return addresses for calls
          EN45501.WAIT       ! Wait time for display
!EIR40389

%INCLUDE EAMTSUVA.J86        ! user exit global variables
%INCLUDE EAMXXCPY.J86        ! copyright statement
!
%INCLUDE EAMASMRT.J86        ! assembler subroutines
%INCLUDE EAMADXRT.J86        ! system services routines          IR68987

\ REM!!****************************************************************
\ REM!!* TERMINAL SALES USER EXITS - AVAILABLE I/O SESSION NUMBERS    *
\ REM!!****************************************************************
\ REM!!*                                                              *
\ REM!!* The following sessions numbers are available for use :       *
\ REM!!*                                                              *
\ REM!!* 6, 7, 10, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 23,        *
\ REM!!* 26, 28, 29, 38,                                              *
\ REM!!* 43, 45, 47, 48, 49, 50, 52, 53, 54, 55, 56, 57, 58,          *
\ REM!!* 59, 60                                                       *
\ REM!!****************************************************************

\ REM!!**************** EXTERNAL  ROUTINES ****************************

\ REM!!****************************************************************
\ REM!!* SUB TSPREC01 print routine                                   *
\ REM!!****************************************************************
\ REM!!*
\ REM!!* Purpose : To print data on one or more of the terminal
\ REM!!* -------   printers.
\ REM!!*
\ REM!!
\ REM!!    Print processing
\ REM!!
\ REM!!      This module is called whenever anything is to be printed.
\ REM!!      Depending on the type of print required, functions such
\ REM!!      as amount formatting are  performed here.
\ REM!!
\ REM!!      TS.LINETYPE controls the type of print required. This
\ REM!!      variable must be set. The value it is set to determines
\ REM!!      which other variables need specifying. The linetype
\ REM!!      specific documentation which follows lists all these
\ REM!!      other variables. These other variables MUST be set for
\ REM!!      the print to function correctly.
\ REM!!
\ REM!!      Values of this parameter have the following meaning:
\ REM!!
\ REM!!        0:  invalid
\ REM!!        1:  item etc
\ REM!!        2:  tender/loan pickup amount, summary report
\ REM!!        3:  discount
\ REM!!        4:  tax/change/check fee/loan pickup summary total
\ REM!!        5:  item pricing/loan pickup qty
\ REM!!        6:  store data print/special sign on/total of items sold !IR99587
\ REM!!        7:  balance due
\ REM!!        8:  ttaf
\ REM!!        9:  tender print/frank
\ REM!!       10:  tn data entry
\ REM!!       11:  non-sale/no-sale header
\ REM!!       12:  special sign off/non-sales trailer
\ REM!!       13:  standalone entry/exit
\ REM!!       14:  account no
\ REM!!       15:  item limit,etc error
\ REM!!       16:  coupons tendered/tax paid at eot
\ REM!!       17:  stamps issued
\ REM!!       18:  personalised start/trailer
\ REM!!       19:  help msgs
\ REM!!       20:  tender listing batch prompt
\ REM!!       21:  tender listing report
\ REM!!       22:  void tn trailer
\ REM!!       23:  signed off msg
\ REM!!       24:  # of prices changed (batch)
\ REM!!       25:  training exit lines
\ REM!!       26:  terminal transfer data
\ REM!!       27:  monitored terminal address
\ REM!!       28:  price verify additional data
\ REM!!       29:  print buffer as passed (for terminal monitor)
\ REM!!       30:  print feature descriptor  IR76369
\ REM!!       31:  tailored for suspend/retrieve IR90981
\ REM!!     > 32:  invalid
\ REM!!
\ REM!!    Input conditions and assumptions
\ REM!!
\ REM!!    - types of print line will only be specified for functions
\ REM!!      that are permited/used in the system (e.g. a system not
\ REM!!      using trading stamps will not attempt to print type 17)
\ REM!!
\ REM!!      any print options set in TS.PRT.PARM where:
\ REM!!                4000H  - print on CR
\ REM!!                2000H  - print on SJ
\ REM!!                1000H  - print on DI
\ REM!!                0100H  - number of linefeeds (eg here = 1)
\ REM!!      Note that any combination of the CR, SJ or DI may be
\ REM!!      used.
\ REM!!      Non optional parameters as specified below need NOT be
\ REM!!      included in this parameter.
\ REM!!
\ REM!!      The following variables are reset on exit from this routine
\ REM!!      and need to be saved if this routine is called from a
\ REM!!      user exit:
\ REM!!          TS.LINETYPE        2 byte integer
\ REM!!          TS.LINEDATA        2 byte integer
\ REM!!          TS.LINEDATA2       2 byte integer
\ REM!!          TS.LINEDATA3       2 byte integer
\ REM!!          TS.PRT.PARM        2 byte integer
\ REM!!          TS.PRT.OPT         2 byte integer
\ REM!!          TS.PRT.SJDI        2 byte integer
\ REM!!          TS.SAVPRT.OPT      2 byte integer
\ REM!!          TS.XXMOD           2 byte integer
\ REM!!          TS.YYMOD           2 byte integer
\ REM!!          TS.ZMOD            2 byte integer
\ REM!!
\ REM!!****************************************************************
\ REM!!
\ REM!!p1: ITEM ENTRY PRINT LINE
\ REM!!
\ REM!!     q @ p.pp pm 0 1234567890   **added**   )
\ REM!!     q/p.pp pm 2 1234567890                 )  price verify
\ REM!!     q @ p.pp pm 3 1234567890               )  line 2
\ REM!!     q @ .pp pm 4 1234567890                )
\ REM!!     123456789012 pm 5 1234567890           )
\ REM!!
\ REM!!                                 q/price
\ REM!!            additional value
\ REM!!    q@price
\ REM!!    xx yymm *item description*    ppp.pp-z
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = 1
\ REM!!    CR = Y
\ REM!!    SJ = OPT
\ REM!!    DI = OPT
\ REM!!
\ REM!!    quantity entered                    TS.LINEDATA = 1
\ REM!!    price verify/change not split       TS.LINEDATA = 2
\ REM!!    price verify/change split package   TS.LINEDATA = 3
\ REM!!
\ REM!!    entry exceeds exception log limit   TS.LOG.LIMIT = Y
\ REM!!
\ REM!!    quantity            - SL.IE.QTYORWGT
\ REM!!    price               - TS.XPRICE
\ REM!!    sale price          - SL.IE.SALEPRIC
\ REM!!    sale price          - SL.IE.SALEPRIC2 if TS.LINEDATA2 = 1
\ REM!!    xx modifier         - TS.XXMOD
\ REM!!    z modifier          - TS.ZMOD
\ REM!!    overide descriptor  - TS.YYMOD
\ REM!!    overide indicator   - TS.MMMOD
\ REM!!    item descriptor     - IR.ITEMNAME$
\ REM!!    other descriptor    - IR.ITEMNAME$
\ REM!!    price change qty    - TS.PC.QTY$
\ REM!!    price change price  - TS.PC.PRICE1
\ REM!!    if PM 2             - TS.PC.PRICE2
\ REM!!****************************************************************
\ REM!!
\ REM!!p2: TENDERS
\ REM!!
\ REM!!
\ REM!!    xx yymm tender description    ppp.pp-
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = OPT
\ REM!!    CR = Y
\ REM!!    SJ = OPT
\ REM!!    DI = N
\ REM!!
\ REM!!    xx modifier         - TS.XXMOD
\ REM!!    overide descriptor  - TS.YYMOD
\ REM!!    overide indicator   - TS.MMMOD
\ REM!!    tender type         - SL.TE.TENDTYPE
\ REM!!    tender variety      - SL.TE.TENDVAR
\ REM!!    tender amount       - SL.TE.AMTTENDE
\ REM!!
\ REM!!    Foreign Currency Tender Line (MCF) - TS.LINEDATA = 2  IR39696
\ REM!!
\ REM!!****************************************************************
\ REM!!
\ REM!!p3: DISCOUNTS
\ REM!!
\ REM!!    cl
\ REM!!    rr yymm discount group ***    ppp.pp-
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = OPT
\ REM!!    CR = Y
\ REM!!    SJ = OPT
\ REM!!    DI = OPT
\ REM!!
\ REM!!    overide descriptor  - TS.YYMOD
\ REM!!    overide indicator   - TS.MMMOD
\ REM!!    discount rate keyed - TS.DSCRT.ENTERED = Y
\ REM!!    keyed discount rate - SL.DI.DISRATE
\ REM!!    void of discount    - TS.XXMOD  = 8
\ REM!!    discount group      - TS.DISC.INDEX
\ REM!!    discount amount     - TS.DISC.SAVE(0,0)
\ REM!!
\ REM!!****************************************************************
\ REM!!
\ REM!!p4:  TAX EXEMP/CHANGE/CHECK FEE/LOAN PICKUP SUMMARY TOTAL
\ REM!!
\ REM!!            fs chg   p.pp  chg    ppp.pp-     1
\ REM!!            change                ppp.pp-     2
\ REM!!            tax exemption         ppp.pp-     3
\ REM!!            tender fee            ppp.pp-     4
\ REM!!       **** total loan            ppp.pp-     5
\ REM!!    xx yymm
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = OPT
\ REM!!    CR = Y
\ REM!!    SJ = OPT
\ REM!!    DI = OPT
\ REM!!
\ REM!!    change due - fs/no fs               TS.LINEDATA = 1
\ REM!!    discountable amount                 TS.LINEDATA = 2
\ REM!!    tax exemption                       TS.LINEDATA = 3
\ REM!!    tender fee                          TS.LINEDATA = 4
\ REM!!    loan/pickup total                   TS.LINEDATA = 5
\ REM!!    xx modifier         - TS.XXMOD
\ REM!!    overide descriptor  - TS.YYMOD
\ REM!!    overide indicator   - TS.MMMOD
\ REM!!    fs amount           - TS.TENDERED(3)
\ REM!!    fs change amount    - TS.BALDUE(1)
\ REM!!    change amount       - TS.BALDUE(0)
\ REM!!    discountable amount - TS.TOTALS(0,0,1)
\ REM!!    tax exempt amount   - SL.DI.AMTTAXEX
\ REM!!    tender fee amount   - SL.TE.AMTTNFEE
\ REM!!    loan/pickup descr   - TS.PICKLOAN
\ REM!!    loan/pickup amount  - TS.PLC.TOTAL
\ REM!!
\ REM!!****************************************************************
\ REM!!
\ REM!!p5: ITEM PRICING(split/weight/qty on sep line)/LOAN PICKUP QTY
\ REM!!
\ REM!!     nn @ pp.pp
\ REM!!     nn @ nn/pp.pp
\ REM!!     nn.nnlb @ nnlb/pp.pp
\ REM!!     nn.nnlb @ pp.pp/lb
\ REM!!     nn.nnlb @ pp.pp/lb    manual
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = 1
\ REM!!    CR = Y
\ REM!!    SJ = OPT
\ REM!!    DI = N
\ REM!!
\ REM!!    quantity @ price                    TS.LINEDATA = 1
\ REM!!    quantity @ qty/price                TS.LINEDATA = 2
\ REM!!    weight entry                        TS.LINEDATA = 3
\ REM!!    loan/pickup                         TS.LINEDATA = 4
\ REM!!    weight manual                       SL.IE.INDICAT1
\ REM!!    quantity            - SL.IE.QTYORWGT
\ REM!!    sale quantity       - SL.IE.SALEQUAN
\ REM!!    weight              - SL.IE.QTYORWGT
\ REM!!    price               - SL.IE.SALEPRIC
\ REM!!    if TS.LINEDATA2 = 1 - SL.IE.SALEPRIC2
\ REM!!    loan/pickup qty     - TS.PLC.QTY
\ REM!!    loan/pickup amt     - SL.TE.AMTTENDE
\ REM!!
\ REM!!****************************************************************
\ REM!!
\ REM!!p6: SPECIAL SIGN ON/STORE DATA PRINT LINE/TOTAL # OF ITEMS SOLD  !IR99587
\ REM!!
\ REM!!          sa
\ REM!!    mm/dd/yy hh:mm ssss tt xxxx yyyyyyyyyy
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = OPT
\ REM!!    CR = OPT
\ REM!!    SJ = Y
\ REM!!    DI = N
\ REM!!
\ REM!!    special sign on                     TS.LINEDATA = 0
\ REM!!    store data print line               TS.LINEDATA = 1
\ REM!!    total # of items sold               TS.LINEDATA = 2       !IR99587
\ REM!!    store number        - TS.STORE$
\ REM!!    terminal number     - TS.TERMINAL$
\ REM!!    transaction number  - SL.HD.TRANSNUM
\ REM!!    operator number     - SL.HD.OPERATOR$
\ REM!!
\ REM!!****************************************************************
\ REM!!
\ REM!!p7: BALANCE DUE/BALANCE DUE FOOD STAMPS
\ REM!!
\ REM!!           fs bal due              pp.pp
\ REM!!      **** tax      pp.pp  bal     pp.pp
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = 1
\ REM!!    CR = Y
\ REM!!    SJ = Y
\ REM!!    DI = N
\ REM!!
\ REM!!    balance due                         TS.LINEDATA = 1
\ REM!!    fs balance due                      TS.LINEDATA = 2
\ REM!!    Alternate Base Currency due         TS.LINEDATA = 3  IR39696
\ REM!!    Other Currency Balance due          TS.LINEDATA = 4  IR39696
\ REM!!    fs balance due amt  - TS.BALDUE(1)
\ REM!!    balance due amount  - TS.BALDUE(0)
\ REM!!    tax in system       - TX.RATE(1) NE 0
\ REM!!    tax amount          - TS.TAXES(0,0)
\ REM!!
\ REM!!****************************************************************
\ REM!!
\ REM!!p8: TTAF AT SIGN ON/AT EOT
\ REM!!
\ REM!!                             p,ppp,ppp.pp
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = OPT
\ REM!!    CR = OPT
\ REM!!    SJ = OPT
\ REM!!    DI = N
\ REM!!
\ REM!!    TTAF amount         - TS.TTAF
\ REM!!
\ REM!!****************************************************************
\ REM!!
\ REM!!p9: TENDER PRINTING/TENDER FRANKING
\ REM!!
\ REM!!    up to 6 user specified print lines
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = OPT (as specified)
\ REM!!    CR = N (except EFT)
\ REM!!    SJ = N
\ REM!!    DI = Y (except EFT)
\ REM!!
\ REM!!    franking format #   - TS.LINEDATA
\ REM!!    franking lines      - TO.FRANK.TXT$(N,N)
\ REM!!    line feed value     - TO.FRANK.LF(N,N)
\ REM!!    variables:
\ REM!!    account number      - TS.IO.DATA$(9)
\ REM!!    tender amount       - SL.TE.AMTTENDE
\ REM!!    bal due amt         - TS.BALDUE(0)
\ REM!!    change  amt         - TS.BALDUE(0)
\ REM!!    tender verify stat  - SL.TE.STATUS
\ REM!!    operator number     - SL.HD.OPERATOR$
\ REM!!    terminal number     - TS.TERMINAL$
\ REM!!    store number        - TS.STORE$
\ REM!!    transaction number  - SL.HD.TRANSNUM
\ REM!!    mgrs overide indic  - TS.MMMOD
\ REM!!
\ REM!!****************************************************************
\ REM!!
\ REM!!p10: TRANSACTION DATA ENTRY
\ REM!!
\ REM!!     ** data entry **                         0
\ REM!!    *****field 1***** 12345678901234567890    1
\ REM!!    *****field 2***** 12345678901234567890    2
\ REM!!    *****field 3***** 12345678901234567890    3
\ REM!!    *****field 4***** 12345678901234567890    4
\ REM!!    *****field 5***** 12345678901234567890    5
\ REM!!    *****field 6***** 12345678901234567890    6
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = OPT
\ REM!!    CR = OPT
\ REM!!    SJ = OPT
\ REM!!    DI = N
\ REM!!
\ REM!!    line # as above     - TS.LINEDATA
\ REM!!    field value         - TS.TEMP1$ (preformatted)
\ REM!!
\ REM!!****************************************************************
\ REM!!
\ REM!!p11: NON-SALE/NO-SALE HEADER
\ REM!!
\ REM!!     procedure type                    nn
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = 1
\ REM!!    CR = OPT
\ REM!!    SJ = OPT
\ REM!!    DI = N
\ REM!!
\ REM!!    procedure number    - TS.LINEDATA
\ REM!!    variable data:
\ REM!!    pickup/loan #       - TS.PLC.NUM
\ REM!!    tender type         - SL.TE.TENDTYPE
\ REM!!    till contents       - TS.TILL.TOTAL
\ REM!!
\ REM!!****************************************************************
\ REM!!
\ REM!!p12: NON-SALE TRAILER/SPECIAL SIGN OFF
\ REM!!
\ REM!!    **terminal secured**   mm/dd/yy hh:mm
\ REM!!    operation complete     mm/dd/yy hh:mm
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = OPT
\ REM!!    CR = OPT
\ REM!!    SJ = Y
\ REM!!    DI = N
\ REM!!
\ REM!!    special sign off                    TS.LINEDATA = 1
\ REM!!    complete trailer                    TS.LINEDATA = 2
\ REM!!    incomplete trailer                  TS.LINEDATA = 3
\ REM!!
\ REM!!****************************************************************
\ REM!!
\ REM!!p13: STANDALONE ENTRY/EXIT
\ REM!!
\ REM!!    ** standalone entered **
\ REM!!    ** standalone exited **  n,nnn,nnn.nn
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = OPT
\ REM!!    CR = OPT
\ REM!!    SJ = OPT
\ REM!!    DI = N
\ REM!!
\ REM!!    standalone entered                  TS.LINEDATA = 1
\ REM!!    standalone exited                   TS.LINEDATA = 2
\ REM!!    TTAF amount         - TE.TR.GROSSPOS-TE.TR.GROSSNEG
\ REM!!
\ REM!!****************************************************************
\ REM!!
\ REM!!p14: ACCOUNT NUMBER
\ REM!!
\ REM!!    account number012345678901234567890123
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = 1
\ REM!!    CR = N
\ REM!!    SJ = OPT
\ REM!!    DI = N
\ REM!!
\ REM!!    account no          - TS.IO.DATA$(9)
\ REM!!
\ REM!!****************************************************************
\ REM!!
\ REM!!p15: ITEM LIMIT, DISCOUNT, TENDER, CHANGE, FEE ERROR
\ REM!!
\ REM!!    Bnnn *item description*   123456789012
\ REM!!    Bnnn discount group ***           rr
\ REM!!    Bnnn discount group ***        pp.pp
\ REM!!    Bnnn tender description        pp.pp
\ REM!!    Bnnn tax exemption             pp.pp  )
\ REM!!    Bnnn change due                pp.pp  )  or any other descriptor
\ REM!!    Bnnn fee error                 pp.pp  )
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = 1
\ REM!!    CR = N
\ REM!!    SJ = OPT
\ REM!!    DI = N
\ REM!!
\ REM!!    error msg no        - TS.LINEDATA
\ REM!!
\ REM!!    amount field required               TS.LINEDATA3 = 1
\ REM!!    quantity/integer field required     TS.LINEDATA3 = 2
\ REM!!    weight field required               TS.LINEDATA3 = 3
\ REM!!
\ REM!!    item descriptor required            TS.LINEDATA2 = 9999
\ REM!!    procedure header required           TS.LINEDATA2 = 1000
\ REM!!    tender fee descr required           TS.LINEDATA2 = 2000
\ REM!!    other descriptor # (from EAMSDESC)  = TS.LINEDATA2
\ REM!!
\ REM!!    item number         - SL.IT.ITEMCODE$
\ REM!!    item descr          - IR.ITEMNAME$
\ REM!!    amount/qty/rate/wt  - TS.LIMAMT
\ REM!!    tender type         - SL.TE.TENDTYPE
\ REM!!    procedure number    - TS.PROCEDURE
\ REM!!****************************************************************
\ REM!!
\ REM!!p16: COUPONS TENDERED/TAX PAID (AT EOT)
\ REM!!
\ REM!!            tax paid        ppp.pp-
\ REM!!            coupons tendered  p.pp
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = 1
\ REM!!    CR = OPT
\ REM!!    SJ = N
\ REM!!    DI = N
\ REM!!
\ REM!!    coupons tendered                    TS.LINEDATA = 1
\ REM!!    tax paid                            TS.LINEDATA = 2
\ REM!!
\ REM!!    coupons amount      - TS.COUP.AMT
\ REM!!    tax paid amount     - TS.TAXES(0,0)
\ REM!!
\ REM!!****************************************************************
\ REM!!
\ REM!!p17: STAMPS ISSUED (AT EOT)
\ REM!!
\ REM!!            stamps            nnn
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = 1
\ REM!!    CR = OPT
\ REM!!    SJ = N
\ REM!!    DI = N
\ REM!!
\ REM!!    stamps amount       - SL.STAMPS
\ REM!!
\ REM!!****************************************************************
\ REM!!
\ REM!!p18: PERSONALIZED HEADER/TRAILER
\ REM!!
\ REM!!     **one or two lines of personalised**
\ REM!!     **header or trailer**
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = set as we go along
\ REM!!    CR = Y if non-null text
\ REM!!    SJ = N
\ REM!!    DI = N
\ REM!!
\ REM!!    header and trailer                  TS.LINEDATA = 0
\ REM!!    header only                         TS.LINEDATA = 1
\ REM!!    header1             - TO.HEADERLINE1$
\ REM!!    header2             - TO.HEADERLINE2$
\ REM!!    trailer1            - TO.TRAILERLINE1$
\ REM!!    trailer2            - TO.TRAILERLINE2$
\ REM!!
\ REM!!****************************************************************
\ REM!!
\ REM!!p19: HELP MESSAGES BETWEEN/INSIDE TRANSACTIONS
\ REM!!
\ REM!!
\ REM!!      ** N lines help msg
\ REM!!      ** some lines only if authorised
\ REM!!      ** + N user defined lines
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = set as we go along
\ REM!!    CR = Y
\ REM!!    SJ = N
\ REM!!    DI = N
\ REM!!
\ REM!!    between transactions                TS.LINEDATA = 0
\ REM!!    inside transaction                  TS.LINEDATA = procedure #
\ REM!!    operator auth       - TS.OPER.IND0
\ REM!!    operator auth       - TS.OPER.IND1
\ REM!!
\ REM!!****************************************************************
\ REM!!
\ REM!!p20: TENDER LISTING BATCH PROMPT
\ REM!!
\ REM!!    last batch starts      mm/dd/yy hh:mm
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = x (just above tear bar)
\ REM!!    CR = Y
\ REM!!    SJ = N
\ REM!!    DI = N
\ REM!!
\ REM!!    batch date/time     - TS.TL.DATE$
\ REM!!
\ REM!!****************************************************************
\ REM!!
\ REM!!p21: TENDER LISTING REPORT
\ REM!!
\ REM!!            TENDER DESCRIPTION
\ REM!!    NO. FEE   CUSTOMER             AMOUNT
\ REM!!    1    .00  12345678901          PPP.PP
\ REM!!    2    .25  12345678901          PPP.PP
\ REM!!            VARIETY 1 SUBTOTAL     PPP.PP
\ REM!!    3    .00  12345678901          PPP.PP
\ REM!!                .
\ REM!!                .
\ REM!!            VARIETY 3 SUBTOTAL     PPP.PP
\ REM!!              TOTAL               PPPP.PP
\ REM!!
\ REM!!            TENDER DESCRIPTION
\ REM!!     NO TENDERS LISTED FOR THIS TERMINAL
\ REM!!
\ REM!!            TENDER DESCRIPTION
\ REM!!    NO.       CUSTOMER             AMOUNT
\ REM!!    1         12345678901          PPP.PP
\ REM!!    2         ** STANDALONE **     PPP.PP
\ REM!!              TOTAL               PPPP.PP
\ REM!!    REMOVAL                MM/DD/YY HH:MM
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = OPT
\ REM!!    CR = Y
\ REM!!    SJ = N
\ REM!!    DI = N
\ REM!!
\ REM!!    tender description                  TS.LINEDATA = 1
\ REM!!    no entries line                     TS.LINEDATA = 2
\ REM!!    tender type header                  TS.LINEDATA = 3
\ REM!!    customer info line                  TS.LINEDATA = 4
\ REM!!    variety subtotal                    TS.LINEDATA = 5
\ REM!!    tender total                        TS.LINEDATA = 6
\ REM!!    removal line                        TS.LINEDATA = 7
\ REM!!    tender type         - SL.TE.TENDTYPE
\ REM!!    tender fee          - TS.TL.FEE$
\ REM!!    amount              - TS.TL.TDR
\ REM!!    line number         - TS.TL.LINENO
\ REM!!    cust number         - TS.TL.CUST$
\ REM!!    subtotal            - TS.TL.SUBTOT
\ REM!!    variety type        - TS.TL.VARNO
\ REM!!    total               - TS.TL.TOTAL
\ REM!!    batch date/time     - TS.TL.DATE$
\ REM!!
\ REM!!****************************************************************
\ REM!!
\ REM!!p22: VOID TRANSACTION TRAILER
\ REM!!
\ REM!!       **** *void transaction*     pp.pp
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = x (just above tear bar)
\ REM!!    CR = Y
\ REM!!    SJ = Y
\ REM!!    DI = OPT
\ REM!!
\ REM!!    transaction amount  - TS.BALDUE(0)
\ REM!!
\ REM!!****************************************************************
\ REM!!
\ REM!!p23: SIGNED OFF MESSAGE
\ REM!!
\ REM!!       signed off
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = x (just above tear bar)
\ REM!!    CR = Y
\ REM!!    SJ = Y
\ REM!!    DI = N
\ REM!!
\ REM!!****************************************************************
\ REM!!
\ REM!!p24: # OF PRICES CHANGED (BATCH)
\ REM!!
\ REM!!       no prices changed
\ REM!!       nn prices changed - batch yymmddss
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = 1
\ REM!!    CR = Y
\ REM!!    SJ = Y
\ REM!!    DI = N
\ REM!!
\ REM!!    no prices changed                   TS.LINEDATA = 1
\ REM!!    delayed price change                TS.LINEDATA = 2
\ REM!!    imed price change                   TS.LINEDATA = 3
\ REM!!    # of prices changed - TS.PC.CHANGENO
\ REM!!    batch number        - TS.PC.BATCHNO$
\ REM!!
\ REM!!****************************************************************
\ REM!!
\ REM!!p25: TRAINING EXIT LINES
\ REM!!
\ REM!!    ** operator training exit
\ REM!!    ** training total      tt / nn,nnn.nn
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = 1
\ REM!!    CR = Y
\ REM!!    SJ = Y
\ REM!!    DI = N
\ REM!!
\ REM!!    ttaf                - TS.TR.TTAF
\ REM!!    training tran #     - TS.TR.TRANNUM
\ REM!!
\ REM!!****************************************************************
\ REM!!
\ REM!!p26: TERMINAL TRANSFER DATA (FROM AND TO)
\ REM!!
\ REM!!     term  nn oper 1234567890 signed off
\ REM!!     trans. nnnn transferred to 1234567890
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = x (just above tear bar)
\ REM!!    CR = Y
\ REM!!    SJ = Y
\ REM!!    DI = N
\ REM!!
\ REM!!    transaction transferred             TS.LINEDATA = 1
\ REM!!    terminal no         - TE.TR.TERMINAL$
\ REM!!    from operator       - TE.TR.OPERATOR$
\ REM!!    to operator         - TS.OPER$
\ REM!!    transaction #       - TE.TR.TRANSNUM$
\ REM!!
\ REM!!****************************************************************
\ REM!!
\ REM!!p27: MONITORED TERMINAL ADDRESS
\ REM!!
\ REM!!     monitoring terminal  nn
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = x (just above tear bar)
\ REM!!    CR = Y
\ REM!!    SJ = Y
\ REM!!    DI = N
\ REM!!
\ REM!!    monitoring term #   - TS.TM.TERM$
\ REM!!
\ REM!!****************************************************************
\ REM!!
\ REM!!p28: PRICE VERIFY/CHANGE (ADDITIONAL DATA)
\ REM!!
\ REM!!        *ITEM DESCRIPTION*   12345678901
\ REM!!      1 ITEM TYPE              0
\ REM!!      2 PRICING METHOD         0
\ REM!!      3 DEPARTMENT           100
\ REM!!      4 COUPON FAMILY 1      123   * CREDIT ACCOUNT
\ REM!!      5 COUPON FAMILY 2      123   * DEBIT ACCOUNT
\ REM!!      6 MIX & MATCH GROUP     12
\ REM!!      7 LINKED ITEM         1234
\ REM!!      8 USER DATA 1         0000
\ REM!!      9 USER DATA 2         0000
\ REM!!
\ REM!!     10 WEIGHT ITEM  N  11 PRICE REQ    N
\ REM!!     12 QTY KEYED    Y  13 QTY REQUIRED N
\ REM!!     14 FOR SALE     Y  15 SUNDAY SALE  Y
\ REM!!     16 LOG ITEM     N  17 COUNT ITEM   Y
\ REM!!     18 TAX A        Y  19 TAX B        N
\ REM!!     20 TAX C        N  21 TAX D        N
\ REM!!     22 FOODSTAMP    Y  23 TRADE STAMP  N
\ REM!!     24 DISCOUNTABLE N* 25 MULT COUPONS N
\ REM!!     26 SHELF LABEL  Y  27 RESERVED 1   N
\ REM!!     28 RESERVED 2   N  29 RESERVED 3   N
\ REM!!     30 USER FLAG 1  N  31 USER FLAG 2  N
\ REM!!     32 USER FLAG 3  N  33 USER FLAG 4  N
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = OPT
\ REM!!    CR = Y
\ REM!!    SJ = N
\ REM!!    DI = N
\ REM!!
\ REM!!    print all lines     - TS.LINEDATA = 0
\ REM!!    print 1 line        - TS.LINEDATA = line number
\ REM!!
\ REM!!    item descriptor     - IR.ITEMNAME$
\ REM!!    item code           - TS.PC.ITEMO$
\ REM!!    data fields         - TS.PC.FLD$(N,0)
\ REM!!    flag fields         - TS.PC.FLG$(N,0)
\ REM!!
\ REM!!****************************************************************
\ REM!!
\ REM!!p29: PRINT BUFFER AS PASSED (TERMINAL MONITOR)
\ REM!!
\ REM!!    * PRINT BUFFER AS PASSED             *
\ REM!!    12345678901234567890123456789012345678
\ REM!!
\ REM!!    set print parameters
\ REM!!    LF = (as passed)
\ REM!!    CR = (as passed)
\ REM!!    SJ = (as passed)
\ REM!!    DI = (as passed)
\ REM!!
\ REM!!    print line          - TS.SAVPRT$
\ REM!!    print options       - TS.SAVPRT.OPT
\ REM!!
\AIR39696  With Multiple Currency installed:
\ REM!!    SA Multiple Currency Blank Line - TS.LINEDATA = 1
\ REM!!    Exchange Rate Line              - TS.LINEDATA = 2
\EIR39696
\ REM!!****************************************************************
\ REM!!****************************************************************
\ REM!!IR76369
\ REM!!p30: PRINT FEATURE DESCRIPTOR
\ REM!!
\ REM!!     USED FOR IBM SUPPORTED FEATURES
\ REM!!****************************************************************
 SUB TSPREC01 EXTERNAL       ! print routine
 END SUB

\ REM!!****************************************************************
\ REM!!* SUB TSDSEC01 display routine                                 *
\ REM!!****************************************************************
\ REM!!*
\ REM!!* Purpose : To display data on the terminal display
\ REM!!* -------
\ REM!!*
\ REM!!
\ REM!!    Display Processing
\ REM!!
\ REM!!      This module is called whenever anything is to be displayed.
\ REM!!      Depending on the type of display required, functions such
\ REM!!      as amount formatting are performed.
\ REM!!
\ REM!!      TS.LINETYPE controls the type of display required. This
\ REM!!      variable must be set. The value it is set to determines
\ REM!!      which other variables need specifying. The linetype
\ REM!!      specific documentation which follows lists all these
\ REM!!      other variables. These other variables MUST be set for
\ REM!!      the display to function correctly.
\ REM!!
\ REM!!      Values of this parameter have the following meaning:
\ REM!!
\ REM!!        0:  invalid
\ REM!!        1:  item entry
\ REM!!        2:  tender  (chg/bal, f.s. chg, fee, loan/pkp)
\ REM!!        3:  balance due
\ REM!!        4:  f.s. balance due (total loan/pkp)
\ REM!!        5:  void transaction
\ REM!!        6:  sales prompt
\ REM!!        7:  non-sale prompt
\ REM!!        8:  error guidance (with item code,etc)
\ REM!!        9:  post error guidance prompt
\ REM!!       10:  overrideable error guidance
\ REM!!       11:  item verification (price change/verify)
\ REM!!       12:  display buffers as passed (e.g.terminal monitor)
\ REM!!       13:  display feature descriptor IR76369
\ REM!!     > 13:  invalid
\ REM!!
\ REM!!      Assumptions
\ REM!!
\ REM!!      - types of display will only be specified for functions
\ REM!!        that are permitted/used in the system (e.g. a system not
\ REM!!        using food stamps will not attempt to display type 4)
\ REM!!
\ REM!!      The following variables are reset on exit from this routine
\ REM!!      and need to be saved if this routine is called from a
\ REM!!      user exit:
\ REM!!          TS.LINETYPE        2 byte integer
\ REM!!          TS.LINEDATA        2 byte integer
\ REM!!          TS.LINEDATA2       2 byte integer
\ REM!!          TS.LINEDATA3       2 byte integer
\ REM!!          TS.DS.NOSAVE       2 byte integer
\ REM!!          TS.XXMOD           2 byte integer
\ REM!!          TS.YYMOD           2 byte integer
\ REM!!
\ REM!!****************************************************************
\ REM!!
\ REM!!d1: ITEM ENTRY DISPLAY
\ REM!!
\ REM!!     *item description*
\ REM!!     cc xxxx      pp.pp-
\ REM!!    12345678901234567890
\ REM!!
\ REM!!
\ REM!!    item descriptor  - IR.ITEMNAME$
\ REM!!    item price       - TS.XPRICE
\ REM!!    cash drawer limit exceeded - TS.CDLIM1.EXCEEDED = Y
\ REM!!    repeat item      - repeat key in SL.IE.INDICAT1
\ REM!!                       repeat number in SL.IE.QTYORWGT
\ REM!!****************************************************************
\ REM!!
\ REM!!d2: TENDER DISPLAY
\ REM!!
\ REM!!    fee .pp csh   pp.pp
\ REM!!    fs p.pp chg   pp.pp
\ REM!!    12345678901234567890
\ REM!!
\ REM!!    tender description
\ REM!!    pickup        pp.pp
\ REM!!   (pi  void      pp.pp )
\ REM!!    12345678901234567890
\ REM!!
\ REM!!    total pickup
\ REM!!                  pp.pp
\ REM!!    12345678901234567890
\ REM!!
\ REM!!
\ REM!!    tender desc.  pp.pp
\ REM!!    change        pp.pp
\ REM!!    12345678901234567890
\ REM!!
\ REM!!    normal tender                             - TS.LINEDATA = 1
\ REM!!    manual tendering, tender cashing/display  - TS.LINEDATA = 2
\ REM!!    loan/pickup/count summary                 - TS.LINEDATA = 3
\ REM!!    loan/pickup/count detail line             - TS.LINEDATA = 4
\ REM!!    tender type            - SL.TE.TENDTYPE
\ REM!!    tender amount          - SL.TE.AMTTENDE
\ REM!!    loan/pickup total      - TS.PLC.TOTAL
\ REM!!    loan/pickup tender     - SL.TE.AMTTENDE
\ REM!!    pickup/loan descriptor - TS.PICKLOAN
\ REM!!    fee amt                - SL.TE.AMTTNFEE
\ REM!!    fs amt                 - TS.TENDERED(3)
\ REM!!    balance amount         - TS.BALDUE(0)
\ REM!!    fs balance amount      - TS.BALDUE(1)
\ REM!!****************************************************************
\ REM!!
\ REM!!d3: BALANCE DUE DISPLAY
\ REM!!
\ REM!!    tax due       pp.pp
\ REM!!    total         pp.pp
\ REM!!    12345678901234567890
\ REM!!
\ REM!!    tax amount             - TS.TAXES(0,0)
\ REM!!    balance amount         - TS.BALDUE(0)
\ REM!!****************************************************************
\ REM!!
\ REM!!d4: FOOD STAMP BALANCE DUE DISPLAY
\ REM!!
\ REM!!    fs bal due
\ REM!!                  pp.pp
\ REM!!    12345678901234567890
\ REM!!
\ REM!!    fs balance amount      - TS.BALDUE(1)
\ REM!!****************************************************************
\ REM!!
\ REM!!d5: VOID TRANSACTION DISPLAY
\ REM!!
\ REM!!     *void transaction*
\ REM!!                  pp.pp
\ REM!!    12345678901234567890
\ REM!!
\ REM!!    balance amount         - TS.BALDUE(0)
\ REM!!****************************************************************
\ REM!!
\ REM!!d6: SALES PROMPT
\ REM!!
\ REM!!     prompt top line
\ REM!!     ** standalone **
\ REM!!    12345678901234567890
\ REM!!
\ REM!!     prompt top line
\ REM!!       ** training  **
\ REM!!    12345678901234567890
\ REM!!
\ REM!!       ** thank you **
\ REM!!       stamps      nnn
\ REM!!    12345678901234567890
\ REM!!
\ REM!!       stamps      nnn
\ REM!!       ** standalone **
\ REM!!    12345678901234567890
\ REM!!
\ REM!!     prompt top line
\ REM!!     prompt bottom line
\ REM!!    12345678901234567890
\ REM!!
\ REM!!    prompt number from descr table (TS.SDESC$) - TS.LINEDATA
\ REM!!    "STANDALONE" goes with prompt  - TS.LINEDATA2 = 1
\ REM!!    "TRAINING"   goes with prompt  - TS.LINEDATA2 = 2
\ REM!!    "STAMPS nn"  goes with prompt  - TS.LINEDATA2 = 3
\ REM!!    stamps given           - TO.PRTCRTSTMP = Y
\ REM!!    stamps amount          - SL.STAMPS
\ REM!!****************************************************************
\ REM!!
\ REM!!d7: NON-SALE PROMPT  (= SALES PROMPT)
\ REM!!
\ REM!!     prompt top line
\ REM!!     ** standalone **
\ REM!!    12345678901234567890
\ REM!!
\ REM!!     prompt top line
\ REM!!       ** training  **
\ REM!!    12345678901234567890
\ REM!!
\ REM!!     prompt top line
\ REM!!     prompt bottom line
\ REM!!    12345678901234567890
\ REM!!
\ REM!!    prompt number from file  (EAMSDESC) - TS.LINEDATA
\ REM!!    "STANDALONE" goes with prompt  - TS.LINEDATA2 = 1
\ REM!!    "TRAINING"   goes with prompt  - TS.LINEDATA2 = 2
\ REM!!****************************************************************
\ REM!!
\ REM!!d8: ERROR GUIDANCE  (WITH ITEM CODE OR OTHER VARIABLE DATA)
\ REM!!
\ REM!!    NB. USE FOR ALL ERROR DISPLAYS EXCEPT 2ND DISPLAY ON OVERIDE
\ REM!!        (which is d10)                    ***
\ REM!!
\ REM!!    Bnnn  error msg
\ REM!!    *sa*    1234567890
\ REM!!    12345678901234567890
\ REM!!
\ REM!!    Bnnn  error msg
\ REM!!     *item description*
\ REM!!    12345678901234567890
\ REM!!
\ REM!!    Bnnn  check quantity
\ REM!!                     qq
\ REM!!    12345678901234567890
\ REM!!
\ REM!!    Bnnn  check weight
\ REM!!                  ww.ww
\ REM!!    12345678901234567890
\ REM!!
\ REM!!    Bnnn  check price
\ REM!!                  pp.pp
\ REM!!    12345678901234567890
\ REM!!
\ REM!!    Bnnn  xx
\ REM!!    discount limit check
\ REM!!    12345678901234567890
\ REM!!
\ REM!!
\ REM!!    error message number from file (EAMSDESC) - TS.LINEDATA
\ REM!!    "*SA*"   goes with prompt  - TS.LINEDATA2 = 1
\ REM!!    "VOID"   goes with prompt  - TS.LINEDATA2 = 2
\ REM!!    variable data required:
\ REM!!                                        - TS.LINEDATA3 =
\ REM!!    variable data as required:
\ REM!!    item code              - SL.IT.ITEMCODE$      1
\ REM!!    item description       - IR.ITEMNAME$         2
\ REM!!    quantity               - TS.TEMP1I4           3
\ REM!!    weight                 - SL.IE.QTYORWGT       4
\ REM!!    price                  - TS.TEMP1I4           5
\ REM!!    other variables:
\ REM!!    msg 40, session no     - ERRF%                6
\ REM!!    discount group         - SL.DI.DISGROUP       8
\ REM!!    msg 51, account no     - TS.ACNUM$            9
\ REM!!    terminal secured                             12
\ REM!!    address                - OS.TERMINAL$        13
\ REM!!    mon term addr          - TE.ST.MONITOR$      14
\ REM!!
\ REM!!    xx modifier            - TS.XXMOD
\ REM!!****************************************************************
\ REM!!
\ REM!!d9: POST ERROR GUIDANCE PROMPT
\ REM!!
\ REM!!     prompt top line
\ REM!!     prompt bottom line
\ REM!!    12345678901234567890
\ REM!!
\ REM!!    saved display top line - TS.SAVDISP1$
\ REM!!    saved display bot line - TS.SAVDISP2$
\ REM!!****************************************************************
\ REM!!
\ REM!!d10: OVERRIDEABLE ERROR GUIDANCE
\ REM!!
\ REM!!    Bnnn xx    vvv.vv oo
\ REM!!     prompt bottom line
\ REM!!    12345678901234567890
\ REM!!
\ REM!!    Bnnn quantity  vv oo
\ REM!!     *variable data*
\ REM!!    12345678901234567890
\ REM!!
\ REM!!    error message number from file (EAMSDESC) - TS.LINEDATA
\ REM!!
\ REM!!    amount field required         - TS.LINEDATA3 = 1
\ REM!!    quantity field required       - TS.LINEDATA3 = 2
\ REM!!    weight field required         - TS.LINEDATA3 = 3
\ REM!!    pending overide               - TS.LINEDATA3 = 4
\ REM!!
\ REM!!    item descriptor required      - TS.LINEDATA2 = 9999
\ REM!!    procedure header required     - TS.LINEDATA2 = 1000
\ REM!!    tender fee required           - TS.LINEDATA2 = 2000
\ REM!!    discount descriptor req       - TS.LINEDATA2 = 3000
\ REM!!    other descriptor # (from EAMSDESC)  = TS.LINEDATA2
\ REM!!
\ REM!!    variable data as required:
\ REM!!    amount/qty/wt/entries  - TS.LIMAMT
\ REM!!    xx descriptor          - TS.XXMOD
\ REM!!    overide type           - TS.YYMOD
\ REM!!    item descriptor        - IR.ITEMNAME$
\ REM!!    procedure number       - TS.PROCEDURE
\ REM!!    tender type            - SL.TE.TENDTYPE
\ REM!!    discount group index   - TS.DISC.INDEX
\ REM!!****************************************************************
\ REM!!
\ REM!!d11: ITEM VERIFICATION DISPLAY
\ REM!!
\ REM!!     *item description*
\ REM!!        *vf*     pp.pp
\ REM!!    12345678901234567890
\ REM!!
\ REM!!     *item description*
\ REM!!     np *vf*    n/p.ppxx
\ REM!!    12345678901234567890
\ REM!!
\ REM!!     *item description*
\ REM!!    add *vf* 1234567890
\ REM!!    12345678901234567890
\ REM!!
\ REM!!     *item description*
\ REM!!     np void    n/p.ppxx
\ REM!!    12345678901234567890
\ REM!!
\ REM!!    verify item/new price         - TS.LINEDATA = 0
\ REM!!    adding item                   - TS.LINEDATA = 2
\ REM!!
\ REM!!    item description       - IR.ITEMNAME$
\ REM!!    price                  - TS.PC.PRICE1
\ REM!!    price (PM 2)           - TS.PC.PRICE2
\ REM!!    sale qty               - TS.PC.QTY$
\ REM!!    item code              - TS.PC.ITEMO$
\ REM!!    mm group               - IR.MPGROUP$
\ REM!!    weight item            - IR.INDICAT0
\ REM!!****************************************************************
\ REM!!
\ REM!!d12: DISPLAY BUFFERS AS PASSED (TERMINAL MONITOR)
\ REM!!
\ REM!!     top line
\ REM!!     bottom line
\ REM!!    12345678901234567890
\ REM!!
\ REM!!    saved display top line - TS.TEMP1$
\ REM!!    saved display bot line - TS.TEMP2$
\ REM!!****************************************************************
\ REM!!IR76369
\ REM!!d13: DISPLAY FEATURE DESCRIPTOR
\ REM!!
\ REM!!     reserved for IBM supported features
\ REM!!****************************************************************
 SUB TSDSEC01 EXTERNAL       ! display routine
 END SUB

\ REM!!****************************************************************
\ REM!! SUB TSCSEC03 stack error guidance
\ REM!!****************************************************************
\ REM!!*
\ REM!!* Purpose :  this routine is called to stack an error guidance
\ REM!!* -------    which will be displayed and processed by
\ REM!!*            mainline when next the function returns to it.
\ REM!!*
\ REM!!* Calling interface :
\ REM!!* -----------------
\ REM!!* Parameters : none
\ REM!!*
\ REM!!* Global Variables :
\ REM!!*
\ REM!!*    TS.STACKERR(0) =  4   displays guidance requiring CLEAR
\ REM!!*
\ REM!!*    TS.PURGE.ERR     can be set on if all errors currently
\ REM!!*                     stacked are to be purged. It will stack
\ REM!!*                     the guidance you pass.
\ REM!!*
\ REM!!*
\ REM!!*   The routine is called with the paramters set for the
\ REM!!*   display as they would be for a call to TSDSEC01.
\ REM!!*
\ REM!!****************************************************************

 SUB TSCSEC03 EXTERNAL   !stack error guidance
 END SUB

\ REM!!****************************************************************
\ REM!!* SUB TSCSEC06 open cash drawer                                *
\ REM!!****************************************************************
\ REM!!*                                                              *
\ REM!!* Purpose : To open the cash drawer                            *
\ REM!!* -------                                                      *
\ REM!!*                                                              *
\ REM!!* Calling interface:                                           *
\ REM!!* -----------------                                            *
\ REM!!*    Parameters       : none                                   *
\ REM!!*                                                              *
\ REM!!*    Global variables : none                                   *
\ REM!!*                                                              *
\ REM!!****************************************************************

 SUB TSCSEC06 EXTERNAL       ! open cash drawer
 END SUB

\ REM!!****************************************************************
\ REM!!* SUB TSCSEC08 display guidance and wait for clear             *
\ REM!!****************************************************************
\ REM!!*                                                              *
\ REM!!* Purpose : To display an error message/guidance and wait for  *
\ REM!!* -------   the clear key to be hit.                           *
\ REM!!*                                                              *
\ REM!!* Calling interface:                                           *
\ REM!!* -----------------                                            *
\ REM!!*    Parameters       : none                                   *
\ REM!!*                                                              *
\ REM!!*    Global variables : as for TSDSEC01 - display routine      *
\ REM!!*                                                              *
\ REM!!****************************************************************

 SUB TSCSEC08 EXTERNAL       ! display guidance and wait for clear
 END SUB

\ REM!!****************************************************************
\ REM!!* SUB TSROEC01 read terminal options                           *
\ REM!!****************************************************************
\ REM!!*                                                              *
\ REM!!* Purpose : To read the latest terminal options from the       *
\ REM!!* -------   store controller                                   *
\ REM!!*                                                              *
\ REM!!* Calling interface:                                           *
\ REM!!* -----------------                                            *
\ REM!!*    Parameters       : none                                   *
\ REM!!*                                                              *
\ REM!!*    Global variables : none                                   *
\ REM!!*                                                              *
\ REM!!****************************************************************

 SUB TSROEC01 EXTERNAL       ! read terminal options
 END SUB

\ REM!!****************************************************************
\ REM!!* SUB TSTPEC01 add data entry string to summary log            *
\ REM!!****************************************************************
\ REM!!*                                                              *
\ REM!!* Purpose : To add a data entry string to the transaction      *
\ REM!!* -------   summary log for processing by checkout support     *
\ REM!!*                                                              *
\ REM!!* Calling interface:                                           *
\ REM!!* -----------------                                            *
\ REM!!*    Parameters       : none                                   *
\ REM!!*                                                              *
\ REM!!*    Global variables : TS.TEMP1I1 = 99                        *
\ REM!!*                       TS.USERDATA$ set to data to be placed  *
\ REM!!*                       in data entry string.                  *
\ REM!!*                                                              *
\ REM!!****************************************************************

 SUB TSTPEC01 EXTERNAL       ! add data entry string to summary log
 END SUB

\ REM!!****************************************************************
\ REM!!* SUB TSTPEC06 write entry to exception log                    *
\ REM!!****************************************************************
\ REM!!*                                                              *
\ REM!!* Purpose : To write an 'exception' entry to the transaction   *
\ REM!!* -------   summary log which checkout support will then place *
\ REM!!*           in the exception log                               *
\ REM!!*                                                              *
\ REM!!* Calling interface:                                           *
\ REM!!* -----------------                                            *
\ REM!!*    Parameters       : none                                   *
\ REM!!*                                                              *
\ REM!!*    Global variables : SL.LOGDATA$ set up for the fields to   *
\ REM!!*                       be placed in the exception log.        *
\ REM!!*                       Note that the first five fields in     *
\ REM!!*                       each exception log are common and do   *
\ REM!!*                       not need to be set up here.            *
\ REM!!*                       The format of this field should be as  *
\ REM!!*                       given in data base section of the      *
\ REM!!*                       functional specification.              *
\ REM!!*                                                              *
\ REM!!*                       SL.LOGCODE set to the log code for     *
\ REM!!*                       the exception entry to be written      *
\ REM!!*                                                              *
\ REM!!****************************************************************

 SUB TSTPEC06 EXTERNAL       ! write exception log entry
 END SUB

\ REM!!****************************************************************
\ REM!!* SUB TSHIEC01 open cash drawer                                *
\ REM!!****************************************************************
\ REM!!*                                                              *
\ REM!!* Purpose : To open the cash drawer                            *
\ REM!!*                                                              *
\ REM!!* Calling interface:                                           *
\ REM!!* -----------------                                            *
\ REM!!*    Parameters       : none                                   *
\ REM!!*                                                              *
\ REM!!*    Global variables : none                                   *
\ REM!!*                                                              *
\ REM!!****************************************************************

 SUB TSHIEC01 EXTERNAL       ! open cash drawer
 END SUB

\ REM!!****************************************************************
\ REM!!* SUB TSHIEC02 get status of cash drawer                       *
\ REM!!****************************************************************
\ REM!!*                                                              *
\ REM!!* Purpose : To get status of cash drawer                       *
\ REM!!*                                                              *
\ REM!!* Calling interface:                                           *
\ REM!!* -----------------                                            *
\ REM!!*    Parameters       : none                                   *
\ REM!!*                                                              *
\ REM!!*    Global variables : TS.DEV.FLAG gives status on return     *
\ REM!!*                       -1 = drawer is open                    *
\ REM!!*                        0 = drawer is closed                  *
\ REM!!*                                                              *
\ REM!!****************************************************************

 SUB TSHIEC02 EXTERNAL       ! get status of cash drawer
 END SUB

\ REM!!****************************************************************
\ REM!!* SUB TSHIEC04 get status of document insert station           *
\ REM!!****************************************************************
\ REM!!*                                                              *
\ REM!!* Purpose : To get status of document insert station           *
\ REM!!*                                                              *
\ REM!!* Calling interface:                                           *
\ REM!!* -----------------                                            *
\ REM!!*    Parameters       : none                                   *
\ REM!!*                                                              *
\ REM!!*    Global variables : TS.DEV.FLAG gives status on return     *
\ REM!!*                       -1 = document is inserted              *
\ REM!!*                        0 = no document present               *
\ REM!!*                                                              *
\ REM!!****************************************************************

 SUB TSHIEC04 EXTERNAL       ! get status of document insert station
 END SUB

\ REM!!****************************************************************
\ REM!!* SUB TSHIEC05 read weight on scale                            *
\ REM!!****************************************************************
\ REM!!*                                                              *
\ REM!!* Purpose : To read weight from scale                          *
\ REM!!*                                                              *
\ REM!!* Calling interface:                                           *
\ REM!!* -----------------                                            *
\ REM!!*    Parameters       : none                                   *
\ REM!!*                                                              *
\ REM!!*    Global variables : TS.TEMP1I4 gives weight on return      *
\ REM!!*                                                              *
\ REM!!****************************************************************

 SUB TSHIEC05 EXTERNAL       ! read weight from scale
 END SUB

\ REM!!****************************************************************
\ REM!!* SUB TSHIECET sound tone                                      *
\ REM!!****************************************************************
\ REM!!*                                                              *
\ REM!!* Purpose : To sound the tone                                  *
\ REM!!*                                                              *
\ REM!!* Calling interface:                                           *
\ REM!!* -----------------                                            *
\ REM!!*    Parameters       : none                                   *
\ REM!!*                                                              *
\ REM!!*    Global variables : none                                   *
\ REM!!*                                                              *
\ REM!!****************************************************************

 SUB TSHIECET EXTERNAL       ! sound the tone
 END SUB

\ REM!!**********  END OF EXTERNAL ROUTINES  **************************

!AIR71561 - move include statement from above for compile
%INCLUDE EAMTSUSU.J86        ! user exit common subroutines
!EIR71561

\ REM!!****************************************************************
\ REM!!  TSUPEC01         : CALLED PRIOR TO FIRST ENTRY IN A
\ REM!!  --------           TRANSACTION
\ REM!!  TYPE             : TIME SEQUENCE EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : NONE
\ REM!!****************************************************************

%INCLUDE EAMTSU01.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC02         : CALLED AFTER A CUSTOMER CHECKOUT
\ REM!!  --------           TRANSACTION COMPLETES
\ REM!!  TYPE             : TIME SEQUENCE EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TS.TLOG.CLEAR - IS SET TO 0 BEFORE CALL   IR80572
\ REM!!                     TO THIS EXIT.  IF STILL 0 ON RETURN FROM  IR80572
\ REM!!                     EXIT, TLOG ARRAY IS REDIMENSIONED.  SET   IR80572
\ REM!!                     TO -1 TO BYPASS REDIMENSIONING.           IR80572
\ REM!!                                                               IR80572
\ REM!!  COMMENTS         : IF FLAG IS TURNED ON, IT IS THE USER'S    IR80572
\ REM!!                     RESPONSIBLITY TO ADD CODE TO REDIMENSION  IR80572
\ REM!!                     THIS ARRAY BEFORE THE START OF THE NEXT   IR80572
\ REM!!                     TRANSACTION. OTHERWISE THERE IS A CHANCE  IR80572
\ REM!!                     THAT MEMORY WILL BE EXCEDED.              IR80572
\ REM!!****************************************************************

%INCLUDE EAMTSU02.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC03         : CALLED WHEN A TIMEOUT OCCURS BETWEEN
\ REM!!  --------           TRANSACTIONS
\ REM!!  TYPE             : TIME SEQUENCE EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : NONE
\ REM!!****************************************************************

%INCLUDE EAMTSU03.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC04         : CALLED BEFORE PERFORMING ANY NON-SALES
\ REM!!  --------           TRANSACTION
\ REM!!  TYPE             : TIME SEQUENCE EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : NONE
\ REM!!****************************************************************

%INCLUDE EAMTSU04.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC05         : CALLED AFTER ENDING ANY NON-SALES
\ REM!!  --------           TRANSACTION
\ REM!!  TYPE             : TIME SEQUENCE EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : NONE
\ REM!!****************************************************************

%INCLUDE EAMTSU05.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC06         : CALLED BEFORE PROCESSING ANY INVALID
\ REM!!  --------           PROCEDURE NUMBER
\ REM!!                     THIS EXIT MAY BE USED TO DEFINE USER
\ REM!!                     PROCEDURES
\ REM!!  TYPE             : TIME SEQUENCE EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : TS.PROCEDURE - PROCEDURE NUMBER
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TS.GUIDANCE - SET TO ZERO IF NOT AN ERROR
\ REM!!                     TS.USER.RETURN -  SET TO NON-ZERO IF A USER
\ REM!!                     PROCEDURE IS DEFINED AND THE 'INVALID
\ REM!!                     PROCEDURE' PROMPT IS NOT TO BE DISPLAYED
\ REM!!****************************************************************

%INCLUDE EAMTSU06.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC07         : CALLED AFTER OPENING ALL FILES
\ REM!!  --------
\ REM!!  TYPE             : I/O EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : NONE
\ REM!!
\ REM!!  COMMENTS         : THIS EXIT SHOULD BE USED TO OPEN ANY
\ REM!!                     REQUIRED USER FILES. AVAILABLE SESSION
\ REM!!                     NUMBERS ARE LISTED AT THE BEGINNING OF
\ REM!!                     THIS MODULE.
\ REM!!****************************************************************

%INCLUDE EAMTSU07.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC08         : CALLED AFTER THE READ OF AN ITEM RECORD
\ REM!!  --------
\ REM!!  TYPE             : I/O EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : IR.ITEMCODE$
\ REM!!                     IR.INDICAT0
\ REM!!                     IR.INDICAT1
\ REM!!                     IR.INDICAT2
\ REM!!                     IR.NUMBERS
\ REM!!                     IR.MPGROUP
\ REM!!                     IR.SALEQUAN
\ REM!!                     IR.SALEPRIC
\ REM!!                     IR.LINKEDTO
\ REM!!                     IR.ITEMNAME
\ REM!!                     IR.INDICAT3
\ REM!!                     IR.USEREXIT
\ REM!!
\ REM!!  COMMENTS         : GLOBAL VARIABLES LISTED AS REFERENCE ONLY
\ REM!!                     SHOULD NOT BE CHANGED.
\ REM!!
\ REM!!                     NOTE THAT TO CAUSE THE ITEM ENTRY TO BE
\ REM!!                     REJECTED, THE DEPARTMENT NUMBER MAY BE
\ REM!!                     SET TO ZERO ("CHECK ITEM DATA") OR THE
\ REM!!                     NOT FOR SALE FLAG MAY BE SET ("ITEM NOT
\ REM!!                     FOR SALE") OR AN ERROR MAY BE PUT ONTO
\ REM!!                     THE DISPLAY STACK (CAUSING THE ITEM TO
\ REM!!                     BE REJECTED WITH THE GIVEN ERROR MESSAGE).
\ REM!!
\ REM!!****************************************************************

%INCLUDE EAMTSU08.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC09         : CALLED AFTER THE READ OF AN OPERATOR
\ REM!!  --------           AUTHORIZATION RECORD
\ REM!!  TYPE             : I/O EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : TS.OPER$
\ REM!!                     TS.INTRX
\ REM!!                     TS.PROCEDURE
\ REM!!                     TS.STANDALONE
\ REM!!                     TS.SPEC.SIGN.OFF
\ REM!!                     TS.SIGNED.ON
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TS.OPER.IND0
\ REM!!                     TS.OPER.IND1
\ REM!!
\ REM!!  COMMENTS         : GLOBAL VARIABLES LISTED AS REFERENCE ONLY
\ REM!!                     SHOULD NOT BE CHANGED.
\ REM!!****************************************************************

%INCLUDE EAMTSU09.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC10         : CALLED AFTER THE READ OF A TERMINAL STATUS
\ REM!!  --------           RECORD
\ REM!!  TYPE             : I/O EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : TE.TR.TERMINAL$
\ REM!!                     TE.TR.OPERATOR$
\ REM!!                     TE.TR.TRANSNUM$
\ REM!!                     TE.TR.NUMLOANS
\ REM!!                     TE.TR.AMTLOANS
\ REM!!                     TE.TR.NUMPKUPS
\ REM!!                     TE.TR.AMTPKUPS
\ REM!!                     TE.TR.GROSSPOS
\ REM!!                     TE.TR.GROSSNEG
\ REM!!                     TE.TR.AMTMISCS
\ REM!!                     TE.TR.NUMTRANS
\ REM!!                     TE.TR.AMT(1)      FOR EACH TENDER TYPE - 8
\ REM!!                     TS.INTRX
\ REM!!                     TS.PROCEDURE
\ REM!!                     TS.STANDALONE
\ REM!!                     TS.SPEC.SIGN.OFF
\ REM!!                     TS.SIGNED.ON
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TE.TR.USERHT$
\ REM!!                     TE.TR.TRANTYPE$
\ REM!!                     TE.TR.STATUS
\ REM!!                     TE.TR.USEREXIT
\ REM!!
\ REM!!  COMMENTS         : GLOBAL VARIABLES LISTED AS REFERENCE ONLY
\ REM!!                     SHOULD NOT BE CHANGED.
\ REM!!****************************************************************

%INCLUDE EAMTSU10.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC11         : CALLED AFTER THE READ OF AN OPERATOR
\ REM!!  --------           STATUS RECORD
\ REM!!  TYPE             : I/O EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : OS.OP.TERMINAL$
\ REM!!                     OS.OP.OPERATOR$
\ REM!!                     OS.OP.NUMLOANS
\ REM!!                     OS.OP.AMTLOANS
\ REM!!                     OS.OP.NUMPKUPS
\ REM!!                     OS.OP.AMTPKUPS
\ REM!!                     OS.OP.AMT(1)      FOR EACH TENDER TYPE - 8
\ REM!!                     TS.INTRX
\ REM!!                     TS.PROCEDURE
\ REM!!                     TS.STANDALONE
\ REM!!                     TS.SPEC.SIGN.OFF
\ REM!!                     TS.SIGNED.ON
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : OS.OP.STATUS
\ REM!!
\ REM!!  COMMENTS         : GLOBAL VARIABLES LISTED AS REFERENCE ONLY
\ REM!!                     SHOULD NOT BE CHANGED.
\ REM!!****************************************************************

%INCLUDE EAMTSU11.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC12         : CALLED AFTER THE READ OF A TENDER LISTING
\ REM!!  --------           RECORD
\ REM!!  TYPE             : I/O EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TS.TL.TYPE$     *
\ REM!!                     TS.TL.TERM$     *
\ REM!!                     TS.TL.TDR.TYPE$ *
\ REM!!                     TS.TL.OPER$     *
\ REM!!                     TS.TL.CUST$
\ REM!!                     TS.TL.TDR$      *
\ REM!!                     TS.TL.FEE$
\ REM!!                     TS.TL.USER$     *
\ REM!!                     TS.TL.DATE$
\ REM!!
\ REM!!  COMMENTS         : GLOBAL VARIABLES LISTED AS REFERENCE ONLY
\ REM!!                     SHOULD NOT BE CHANGED.
\ REM!!                     VARIABLES MARKED WITH AN ASTERISK (*) ARE
\ REM!!                     CURRENTLY LOCAL DUE TO SYMBOL TABLE
\ REM!!                     LIMITATIONS.
\ REM!!****************************************************************

%INCLUDE EAMTSU12.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC13         : CALLED AFTER THE READ OF A TENDER
\ REM!!  --------           VERIFICATION RECORD
\ REM!!  TYPE             : I/O EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TS.USEREXIT - USER EXIT FIELD READ FROM
\ REM!!                     FILE
\ REM!!
\ REM!!  COMMENTS         : GLOBAL VARIABLES LISTED AS REFERENCE ONLY
\ REM!!                     SHOULD NOT BE CHANGED.
\ REM!!****************************************************************

%INCLUDE EAMTSU13.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC14         : CALLED AFTER THE READ OF THE
\ REM!!  --------           KEYBOARD/SCANNER/WAND
\ REM!!  TYPE             : I/O EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!! !IR80446 removed TS.IO.COUNT from the list since its not updated
\ REM!!          before this exit is called.
\ REM!!  REFERENCE ONLY   : TS.INTRX
\ REM!!                     TS.IO.STATE                 !IR80446
\ REM!!                     TS.OVRFLAG                  !IR80446
\ REM!!                     TS.PROCEDURE
\ REM!!                     TS.STANDALONE
\ REM!!                     TS.SPEC.SIGN.OFF
\ REM!!                     TS.SIGNED.ON
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TS.IO.HDR$ - this variable contains the
\ REM!!                     header string returned by the i/o processor.
\ REM!!                     Its meaning and use are documented in the
\ REM!!                     IBM 4680 BASIC language reference.
\ REM!!
\ REM!!                     The following two arrays contain ten
\ REM!!                     elements each and correspond to the ten
\ REM!!                     possible positions into which data may be
\ REM!!                     placed by the i/o processor.
\ REM!!                     TS.IO.KEYS(1) - function code (if key
\ REM!!                     pressed) for each of ten positions.
\ REM!!                     TS.IO.DATA$(1) - corresponding data
\ REM!!                     associated with that key.
\ REM!!
\ REM!!  COMMENTS         : GLOBAL VARIABLES LISTED AS REFERENCE ONLY
\ REM!!                     SHOULD NOT BE CHANGED.
\ REM!!
\ REM!! The following list contains the keys which are recognizable by the
\ REM!! supermarket terminal application.  A user must select from this set the
\ REM!! keys which he requires.  Only 37 function keys are available on the
\ REM!! keyboard along with the 10 numeric keys and the "Asterisk" key.  Some
\ REM!! keys such as "Enter" and "Clear" often use two function key positions.
\ REM!!
\ REM!! The following keys are required of all users of the application.
\ REM!!
\ REM!! Key Name:            Decimal Function Code:
\ REM!!
\ REM!! 10 Numerics          48-57 (SYSTEM DEFINED)
\ REM!! Sign-on/off          61
\ REM!! Void (-)             70
\ REM!! Clear                73
\ REM!! Asterisk             78
\ REM!! Override             79
\ REM!! Enter                80
\ REM!! Total                81
\ REM!! Cash                 91
\ REM!! No-sale              100
\ REM!!
\ REM!! The following keys are optional based on the users need of the function
\ REM!! provided by the keys.
\ REM!!
\ REM!! Key Name:            Decimal Function Code:
\ REM!!
\ REM!! Discount             62
\ REM!! Data Entry           63
\ REM!! Tare                 71
\ REM!! Weight               72
\ REM!! Price                74
\ REM!! Quantity             75
\ REM!! F.S./No F.S.         76
\ REM!! Tax/No Tax           77
\ REM!! Verify Tender        90
\ REM!! Check                92
\ REM!! Foodstamp            93
\ REM!! Misc. Tender         94-96
\ REM!! Item Lookup Keys     200-255
\ REM!!
\ REM!! The following keys are optional based on the users method of
\ REM!! implementation of the function.  Each of the functions provided
\ REM!! by these keys can be obtained without these keys.
\ REM!!
\ REM!! Key Name:            Decimal Function Code:
\ REM!!
\ REM!! Deposit              64
\ REM!! Manufacturer Coupon  65
\ REM!! Store Coupon         66
\ REM!! Refund               67
\ REM!! Procedures           101-119
\ REM!!                      where 101 = procedure 1 = Tender Cashing
\ REM!!                            101 = procedure 2 = Tender Exchange
\ REM!!                                          .
\ REM!!                                          .
\ REM!!                                          .
\ REM!!
\ REM!! The terminal key sequences are implemented through a state table which
\ REM!! defines the allowed key sequences to the I/O processor which handles
\ REM!! data entry at the terminal.  This table along with validity checks in
\ REM!! the code determines what key sequences are valid at any point in time.
\ REM!! provided in the default standard keying table EAMS@000 and the default
\ REM!! alternate keying table EAMS@999.
\ REM!! The function codes and data are passed to the
\ REM!! applications in one of 10 positions.  Capitalized function key
\ REM!! names imply that data is allowed or required with the function key.
\ REM!!
\ REM!! STATE:      POSITION:
\ REM!! #: NAME:     0    1    2    3    4    5    6    7    8    9    10
\ REM!!  1 CLEAR    clr   -    -    -    -    -    -    -    -    -    -
\ REM!!  2 SIGNON    -    -    -   "*"   -  &S/ON  -    -    -    -    -
\ REM!!  3 SIGNON-2 &ent  -  OVRIDE -    -   S/ON  -    -    -    -    -
\ REM!!  4 SO-PSWD   -    -    -    -    -    -    -    -    -    -  ENTER
\ REM!!  5 SPEC-SO  &ent  -    -    - &nosal S/ON  -    -    -    -    -
\ REM!!  6 OVERRIDE ent CLEAR  -    -    -    -    -    -    -    -  OVRIDE
\ REM!!  7 ACCT NO   -  CLEAR  -   "*"   -    -    -    -    -    -    -
\ REM!!  8 ACCT/OVR  -  CLEAR  -   "*"   -    -    -    -    -    -  ENTER
\ REM!!                                                             OVERRIDE
\ REM!!  9 ENT/CLR   -    -  enter  -    -    -    -    -    -    -    -
\ REM!! 10 MAIN     clr void ENTER "*" dpst DEPT  WGT fs/no coup TARE DATA
\ REM!!                     tx/no tx  NOSAL PRICE QTY TNDRS refnd VFY
\ REM!!                     procedure       DISC total      ovride
\ REM!!                                     S/ON
\ REM!! 11 ENT/DAT   -   CLR   -    -    -    -    -    -    -    -  ENTER
\ REM!! 12 STANDALN  -    -  enter "*"   -    -    -    -    -    -    -
\ REM!! 13 ENTER    enter -    -    -    -    -    -    -    -    -    -
\ REM!! 20 TC/TE    &ent void proc "*" nosal S/ON totl TNDR refnd VFY DATA
\ REM!! 21 CL/CP/TC &ent void proc "*" nosal S/ON totl TNDR  -    -   DATA
\ REM!!                                           QTY  COUP
\ REM!! 22 T/LIST    -    -  enter  -  nosal S/ON totl tendr -    -    -
\ REM!!                      proc
\ REM!! 23 TT/TMON   -    -  ENTER  -  nosal S/ON totl  -    -    -    -
\ REM!!                      proc
\ REM!! 24 PR/CHG    -    -  ENT  OVR  nosal S/ON totl  -    -    -    -
\ REM!!                      proc "*"  PRICE
\ REM!!                                 dept.
\ REM!!
\ REM!! & denotes those function codes which are defined only for
\ REM!! alternate keying.
\ REM!!
\ REM!! Notes:
\ REM!!
\ REM!! 1) The CLEAR key can be entered in any state to clear the previously
\ REM!! entered numerics unless the clear key is specifically defined in the
\ REM!! state to behave differently.  This global definition of the clear key
\ REM!! goes in position 0 which is never passed back to the application.
\ REM!!
\ REM!! 2) The VOID key can be entered in any state and is passed to the
\ REM!! application in position 1 with no data allowed.
\ REM!!
\ REM!! 3) The clear state is defined to allow the application to get control
\ REM!! when the clear key is hit.
\ REM!!
\ REM!! 4) Multiple states are required for sign-on to avoid displaying a
\ REM!! password as keyed.
\ REM!!
\ REM!! 5) When the enter key is found in position 10, the application normally
\ REM!! moves the data to the place where it would nrmally occur and pairs it
\ REM!! with the appropriate function code.  This allows prompted data to look
\ REM!! like it was keyed in the preceeding key sequence.
\ REM!!
\ REM!! 6) States SPEC-SO,SO-PSWD, OVERRIDE, ACCTNO, and ACCT/OVR are used only
\ REM!! when prompting for a password,override number, account number, or clear
\ REM!! key.  ENT/DAT is used when prompting for other data such as price,
\ REM!! weight, quantity, or fees.
\ REM!!
\ REM!! 7) The ENT/CLR state is used only with the external verification prompt
\ REM!! to retry or bypass external verification after a timeout.
\ REM!!
\ REM!! 8) The ENTER state is used only in conjunction with the OVERRIDE state
\ REM!! and is required to handle scanned input.
\ REM!!
\ REM!! 9) The MAIN state handles all input which occurs in or between checkout
\ REM!! transactions except for input which is specially prompted for on the
\ REM!! display.
\ REM!!
\ REM!! 10) All non-sale procedures except for operator training use single
\ REM!! self-contained states. TC/TE is for Tender Cashing and Tender Exchange.
\ REM!! CL/CP/TC is for Cashier Loan, Cashier Pickup, and Tender Count.  T/LIST
\ REM!! is for Tender Listing.  TT/TMON is for Terminal Transfer and Terminal
\ REM!! Monitor.  PR/CHG is for Price verify/Change.
\ REM!!
\ REM!! 11) The "*" key requires data in every state except for STANDALN and
\ REM!! CL/CP/TC where data is not allowed.
\ REM!!
\ REM!!   INPUT DATA:            INPUT POSITION:   ASSOCIATED FUNCTION KEY:
\ REM!!
\ REM!! Item Code                      2           ENTER
\ REM!! New Password                   2           OVERRIDE
\ REM!! Terminal Number                2           ENTER
\ REM!! Operator #                     3           "*"
\ REM!! Expiration Date                3           "*"
\ REM!! Deal Quantity/Weight           3           "*"
\ REM!! Tender variety                 3           "*"
\ REM!! Discount rate                  3           "*"
\ REM!! Transaction data               4           NOSALE
\ REM!! Password                       5           SIGNON
\ REM!! Procedure #                    5           SIGNON
\ REM!! Price                          5           PRICE or LOOKUPKEY
\ REM!! Discount Group                 5           DISCOUNT
\ REM!! Weight                         6           WEIGHT
\ REM!! Quantity                       6           QUANTITY
\ REM!! Tender Amounts                 7           TENDERS or COUPONS
\ REM!! Customer Account #             9           TENDER VERIFY
\ REM!! Tare Value                     9           TARE
\ REM!! Data Entry                    10           DATA ENTRY
\ REM!! Override #                    10           OVERRIDE
\ REM!!
\ REM!! The following keys are sometimes defined with no data allowed and the
\ REM!! function codes appear in the following positions:
\ REM!!
\ REM!!   MODIFIER KEY           INPUT POSITION:
\ REM!!
\ REM!! VOID                           1
\ REM!! TAX/NO TAX                     2
\ REM!! "*"                            3
\ REM!! NOSALE                         4
\ REM!! DEPOSIT                        4
\ REM!! LOOKUPKEY                      5
\ REM!! TOTAL                          6
\ REM!! FS/NO FS                       7
\ REM!! TENDERS                        7
\ REM!! REFUND                         8
\ REM!! MFR COUPON                     8
\ REM!! STR COUPON                     8
\ REM!! OVERRIDE                       8
\ REM!!
\ REM!!****************************************************************

%INCLUDE EAMTSU14.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC15         : CALLED AFTER THE READ OF THE SCALE
\ REM!!  --------
\ REM!!  TYPE             : I/O EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TS.TEMP1I4 - WEIGHT FROM SCALES
\ REM!!
\ REM!!  COMMENTS         : GLOBAL VARIABLES LISTED AS REFERENCE ONLY
\ REM!!                     SHOULD NOT BE CHANGED.
\ REM!!****************************************************************

%INCLUDE EAMTSU15.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC16         : CALLED AFTER THE READ OF THE EXTERNAL
\ REM!!  --------           TENDER VERIFICATION PIPE
\ REM!!  TYPE             : I/O EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : ET.OUT.MSG$, ET.OUT.MSGLEN        !IR81279
\ REM!!****************************************************************

%INCLUDE EAMTSU16.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC17         : CALLED BEFORE WRITING EOT DATA TO THE HARD
\ REM!!  --------           TOTALS
\ REM!!  TYPE             : I/O EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TE.TR.USERHT$ - USER HARD TOTALS FIELD
\ REM!!
\ REM!!  COMMENTS         : BEFORE WRITING TO HARD TOTALS THE FIELD
\ REM!!                     TE.TR.USERHT$ WILL BE RIGHT EXTENDED OR
\ REM!!                     TRUNCATED AS REQUIRED TO MAKE IT 10 BYTES
\ REM!!                     LONG.
\ REM!!****************************************************************

%INCLUDE EAMTSU17.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC18         : CALLED BEFORE WRITING A TRANSACTION
\ REM!!  --------           SUMMARY RECORD OR EXCEPTION LOG INFORMATION
\ REM!!                     FROM THE TERMINAL TO THE SUMMARY LOG
\ REM!!  TYPE             : I/O EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  !AIR73623 - ADDED SUPPORT FOR THE FOLLOWING FLAG SO THAT
\ REM!!              USER IS ABLE TO DETERMINE WHICH EXIT CALL IS
\ REM!!              ACTIVE
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : TS.TRANS.LOG.WRITE
\ REM!!                     IF THE VALUE OF THIS FLAG IS -1, THEN
\ REM!!                     THE EXIT WAS CALLED BEFORE A WRITE MATRIX
\ REM!!                     OF TRANSACTION DATA TO THE SUMMARY LOG.
\ REM!!                     IF THE VALUE OF THE FLAG IS 0, THEN THE
\ REM!!                     EXIT WAS CALLED BEFORE A WRITE OF
\ REM!!                     EXCEPTION LOG DATA TO THE SUMMARY LOG.
\ REM!! !EIR73623
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : NONE
\ REM!!****************************************************************

%INCLUDE EAMTSU18.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC19         : CALLED BEFORE IMMEDIATE OR DELAYED UPDATE
\ REM!!  --------           TO ITEM RECORD
\ REM!!  TYPE             : I/O EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : IR.ITEMCODE$
\ REM!!                     IR.INDICAT0
\ REM!!                     IR.INDICAT1
\ REM!!                     IR.INDICAT2
\ REM!!                     IR.NUMBERS
\ REM!!                     IR.MPGROUP
\ REM!!                     IR.SALEQUAN
\ REM!!                     IR.SALEPRIC
\ REM!!                     IR.LINKEDTO
\ REM!!                     IR.ITEMNAME
\ REM!!                     IR.INDICAT3
\ REM!!                     IR.USEREXIT
\ REM!!
\ REM!!  COMMENTS         : GLOBAL VARIABLES LISTED AS REFERENCE ONLY
\ REM!!                     SHOULD NOT BE CHANGED.
\ REM!!****************************************************************

%INCLUDE EAMTSU19.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC20         : CALLED BEFORE WRITING A LINE TO THE
\ REM!!  --------           CUSTOMER RECEIPT
\ REM!!  TYPE             : I/O EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TS.PRTBUF$ - 38-CHAR BUFFER TO BE PRINTED
\ REM!!
\ REM!!  COMMENTS         : THE BUFFER PASSED MAY BE CHANGED AS REQUIRED
\ REM!!                     BEFORE RETURNING TO PRINT IT.
\ REM!!                     NOTE:  IN ADDITION TO THE NORMAL VARIABLES
\ REM!!                     SAVED AND RESTORE WHEN PRINTING FROM A USER
\ REM!!                     EXIT, THE FOLLOWING MUST BE SAVED AND
\ REM!!                     RESTORED:   TS.PRINTPRM,  TS.FORMCR$
\ REM!!****************************************************************

%INCLUDE EAMTSU20.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC21         : CALLED BEFORE WRITING A LINE TO THE SUMMARY
\ REM!!  --------           JOURNAL
\ REM!!  TYPE             : I/O EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TS.SJDATA$ - FIRST 20 CHARS OF 38-CHAR
\ REM!!                              BUFFER TO BE PRINTED
\ REM!!
\ REM!!  COMMENTS         : THE BUFFER PASSED MAY BE CHANGED AS REQD
\ REM!!                     TO CHANGE THE FIRST 20 CHARACTERS OF THE
\ REM!!                     PRINT LINE.
\ REM!!                     NOTE:  IN ADDITION TO THE NORMAL VARIABLES
\ REM!!                     SAVED AND RESTORE WHEN PRINTING FROM A USER
\ REM!!                     EXIT, THE FOLLOWING MUST BE SAVED AND
\ REM!!                     RESTORED:   TS.PRINTPRM,  TS.FORMCR$
\ REM!!****************************************************************

%INCLUDE EAMTSU21.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC22         : CALLED BEFORE WRITING A LINE TO THE
\ REM!!  --------           DOCUMENT INSERT STATION
\ REM!!  TYPE             : I/O EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TS.PRTBUF$ - 38-CHAR BUFFER TO BE PRINTED
\ REM!!
\ REM!!  COMMENTS         : THE BUFFER PASSED MAY BE CHANGED AS REQUIRED
\ REM!!                     BEFORE RETURNING TO PRINT IT.
\ REM!!                     NOTE:  IN ADDITION TO THE NORMAL VARIABLES
\ REM!!                     SAVED AND RESTORE WHEN PRINTING FROM A USER
\ REM!!                     EXIT, THE FOLLOWING MUST BE SAVED AND
\ REM!!                     RESTORED:   TS.PRINTPRM,  TS.FORMCR$
\ REM!!****************************************************************

%INCLUDE EAMTSU22.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC23         : CALLED BEFORE WRITING A LINE TO THE
\ REM!!  --------           DISPLAY
\ REM!!  TYPE             : I/O EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : TS.IN.BACKUP   -1 IF IN LOOP BACKUP-IR75271
\ REM!!                                     0 IF NOT IN LOOP BACKUP
\ REM!!                     TS.SV.LINETYPE  1 - 13 DISPLAY LINE TYPE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TS.DISP1$, TS.DISP2$ - TOP AND BOTTOM
\ REM!!                     LINES OF DISPLAY.
\ REM!!                     TS2.DISP1$, TS2.DISP2$ - TOP AND BOTTOM
\ REM!!                     LINES OF SECOND DISPLAY.  IR80239
\ REM!!                     LED.REQD - WHICH LEDs ON THE SHOPPER DISPLAY
\ REM!!                     ARE TO BE LIT UP.         IR80239
\ REM!!
\ REM!!  COMMENTS         :THE DISPLAY BUFFERS MAY BE CHANGED AS
\ REM!!                    REQUIRED BEFORE RETURN TO DO THE DISPLAY.
\ REM!!                    TS.IN.BACKUP IS SET/RESET WHENEVER BACKUP
\ REM!!                    SITUATION CHANGES.  WHEN TS.IN.BACKUP IS
\ REM!!                    CHANGED A RE-DISPLAY OF CURRENT DISPLAY IS
\ REM!!                    FORCED.  THE FLAG CAN BE CHECKED AT THIS
\ REM!!                    EXIT AND DISPLAY MODIFIED BASED ON LINETYPE T
\ REM!!                    INDICATE TO OPERATOR LOOP BACKUP STATUS.
\ REM!!****************************************************************

%INCLUDE EAMTSU23.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC24         : CALLED BEFORE OPENING THE CASH DRAWER
\ REM!!  --------
\ REM!!  TYPE             : I/O EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TS.USER.RETURN
\ REM!!
\ REM!!  COMMENTS         : UPON ENTRY TS.USER.RETURN IS ZERO. SET IT
\ REM!!                     TO A NON-ZERO VALUE IF IT IS DESIRED TO
\ REM!!                     INHIBIT THE OPEN OF THE CASH DRAWER.
\ REM!!****************************************************************

%INCLUDE EAMTSU24.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC25         : CALLED BEFORE SOUNDING THE TONE
\ REM!!  --------
\ REM!!  TYPE             : I/O EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TS.USER.RETURN
\ REM!!
\ REM!!  COMMENTS         : UPON ENTRY TS.USER.RETURN IS ZERO. SET IT
\ REM!!                     TO A NON-ZERO VALUE IF IT IS DESIRED TO
\ REM!!                     INHIBIT THE SUBSEQUENT SOUNDING OF THE
\ REM!!                     TONE.
\ REM!!****************************************************************

%INCLUDE EAMTSU25.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC26         : CALLED BEFORE WRITING A MESSAGE TO THE
\ REM!!  --------           EXTERNAL TENDER VERIFICATION PIPE
\ REM!!  TYPE             : I/O EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : ET.IN.MSG$, ET.IN.MSGLEN     !IR80062
\ REM!!
\ REM!!  OUTPUT PARMS.    : ET.IN.MSG$, ET.IN.MSGLEN     !IR80062
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : NONE
\ REM!!****************************************************************

%INCLUDE EAMTSU26.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC27         : CALLED AT PROCESSING OF AN I/O ERROR
\ REM!!  --------
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : TE.TR.TERMINAL$
\ REM!!                     TE.TR.OPERATOR$
\ REM!!                     TE.TR.TRANSNUM$
\ REM!!                     TS.INTRX
\ REM!!                     TS.PROCEDURE
\ REM!!                     TS.STANDALONE
\ REM!!                     TS.SPEC.SIGN.OFF
\ REM!!                     TS.SIGNED.ON
\ REM!!                     ERRN
\ REM!!                     ERR
\ REM!!                     ERRF%
\ REM!!                     TS.ER.RETURN
\ REM!!                     ASYNC.ERRN
\ REM!!                     DVICE
\ REM!!                     TER
\ REM!!                     ERR27.ROUTINE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : BYPASS.REPRINT% IR92088 ADDED "%" TO COMMENT
\ REM!!
\ REM!!  COMMENTS         : GLOBAL VARIABLES LISTED AS REFERENCE ONLY
\ REM!!                     SHOULD NOT BE CHANGED.
\ REM!!****************************************************************

%INCLUDE EAMTSU27.J86

!AIO05390
\ REM!!****************************************************************
\ REM!!  TSUPEA27         : CALLED AT PROCESSING OF AN ASYNC I/O ERROR
\ REM!!  --------
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : TE.TR.TERMINAL$
\ REM!!                     TE.TR.OPERATOR$
\ REM!!                     TE.TR.TRANSNUM$
\ REM!!                     TS.INTRX
\ REM!!                     TS.PROCEDURE
\ REM!!                     TS.STANDALONE
\ REM!!                     TS.SPEC.SIGN.OFF
\ REM!!                     TS.SIGNED.ON
\ REM!!                     ERRN
\ REM!!                     ERR
\ REM!!                     ERRF%
\ REM!!                     TS.ER.RETURN
\ REM!!                     ASYNC.ERRN
\ REM!!                     DVICE
\ REM!!                     TER
\ REM!!                     ERR27.ROUTINE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : BYPASS.REPRINT% IR92088 ADDED "%" TO COMMENT
\ REM!!
\ REM!!  COMMENTS         : GLOBAL VARIABLES LISTED AS REFERENCE ONLY
\ REM!!                     SHOULD NOT BE CHANGED.
\ REM!!
\ REM!!          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
\ REM!!          !!  WARNING - BECAUSE THIS ROUTINE RUNS ASYNCHRONOUSLY,  !!
\ REM!!          !!  ALTERING OF ANY GLOBAL VARIABLE HERE CAN CAUSE THE   !!
\ REM!!          !!  CURRENTLY-RUNNING FUNCTION TO BEHAVE ERRATICALLY OR  !!
\ REM!!          !!  ABEND.  REFER TO THE NOTES SECTION OF THE ON ASYNC   !!
\ REM!!          !!  ERROR CALL STATEMENT IN THE BASIC LANGUAGE REFERENCE !!
\ REM!!          !!  MANUAL FOR ADDITIONAL INFORMATION.                   !!
\ REM!!          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
\ REM!!
\ REM!!****************************************************************

%INCLUDE EAMTSA27.J86   !!!  See WARNING above  !!!
!EIO05390

\ REM!!****************************************************************
\ REM!!  TSUPEC28         : CALLED AT SIGNON VALIDATION
\ REM!!  --------
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : TS.OPER$
\ REM!!                     TS.INTRX
\ REM!!                     TS.PROCEDURE
\ REM!!                     TS.STANDALONE
\ REM!!                     TS.SPEC.SIGN.OFF
\ REM!!                     TS.SIGNED.ON
\ REM!!                     TS.IO.KEYS(1)
\ REM!!                     TS.IO.DATA$(1)
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : NONE
\ REM!!
\ REM!!  COMMENTS         : GLOBAL VARIABLES LISTED AS REFERENCE ONLY
\ REM!!                     SHOULD NOT BE CHANGED.
\ REM!!****************************************************************

%INCLUDE EAMTSU28.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC29         : CALLED AT THE TAX CALCULATION
\ REM!!  --------
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : TS.TOTALS(X, Y, Z) - 3D ARRAY OF
\ REM!!                     TAXABLE AMOUNTS
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TS.TAXES(X,Y) - 1D ARRAY OF TAXES
\ REM!!
\ REM!!  COMMENTS         : GLOBAL VARIABLES LISTED AS REFERENCE ONLY
\ REM!!                     SHOULD NOT BE CHANGED.
\ REM!!****************************************************************

%INCLUDE EAMTSU29.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC30         : CALLED AT THE BALANCE DUE CALCULATION
\ REM!!  --------
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : TS.TOTALS(X, Y, Z) - 3D ARRAY OF
\ REM!!                     TAXABLE AMOUNTS
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TS.BALDUE(X) - REGULAR AND F/S BALANCE
\ REM!!                     DUE
\ REM!!
\ REM!!  COMMENTS         : GLOBAL VARIABLES LISTED AS REFERENCE ONLY
\ REM!!                     SHOULD NOT BE CHANGED.
\ REM!!****************************************************************

%INCLUDE EAMTSU30.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC31         : CALLED AT DATA ENTRY PROCESSING
\ REM!!  --------
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : TS.OPER$
\ REM!!                     TS.INTRX
\ REM!!                     TS.PROCEDURE
\ REM!!                     TS.STANDALONE
\ REM!!                     TS.SPEC.SIGN.OFF
\ REM!!                     TS.SIGNED.ON
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TS.IO.KEYS(1)
\ REM!!                     TS.IO.DATA$(1)
\ REM!!
\ REM!!  COMMENTS         : GLOBAL VARIABLES LISTED AS REFERENCE ONLY
\ REM!!                     SHOULD NOT BE CHANGED.
\ REM!!****************************************************************

%INCLUDE EAMTSU31.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC32         : CALLED AT TENDER VALIDATION
\ REM!!  --------
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : RISK - RISK CODE FROM VALIDATION
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TS.USER.RETURN - REQUIRED VALUE FOR
\ REM!!                     RISK CODE
\ REM!!
\ REM!!  COMMENTS         : UPON ENTRY TS.USER.RETURN IS ZERO. IF IT
\ REM!!                     IS REQUIRED TO CHANGE THE CALCULATED RISK
\ REM!!                     CODE THEN SET TS.USER.RETURN TO THAT VALUE;
\ REM!!                     ANY NON-ZERO VALUE WILL BE PICKED UP BY THE
\ REM!!                     CALLING APPLICATION.
\ REM!!****************************************************************

%INCLUDE EAMTSU32.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC33         : CALLED AT RECOVERY OF TOTALS PROCESSING
\ REM!!  --------
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : NONE
\ REM!!
\ REM!!  COMMENTS         : GLOBAL VARIABLES LISTED AS REFERENCE ONLY
\ REM!!                     SHOULD NOT BE CHANGED.
\ REM!!****************************************************************

%INCLUDE EAMTSU33.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC34         : CALLED AT STANDALONE ENTRY
\ REM!!  --------
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : NONE
\ REM!!
\ REM!!  COMMENTS         : GLOBAL VARIABLES LISTED AS REFERENCE ONLY
\ REM!!                     SHOULD NOT BE CHANGED.
\ REM!!****************************************************************

%INCLUDE EAMTSU34.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC35         : CALLED AT STANDALONE EXIT
\ REM!!  --------
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : NONE
\ REM!!
\ REM!!  COMMENTS         : GLOBAL VARIABLES LISTED AS REFERENCE ONLY
\ REM!!                     SHOULD NOT BE CHANGED.
\ REM!!****************************************************************

%INCLUDE EAMTSU35.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC36         : CALLED AT MANAGER'S OVERRIDE PROCESSING
\ REM!!  --------
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TS.TEMP1I4 - OVERRIDE NUMBER (INTEGER)
\ REM!!
\ REM!!  COMMENTS         : THE OVERRIDE NUMBER MAY BE MANIPULATED AS
\ REM!!                     DESIRED (E.G. ENCRYPTED) BEFORE VALIDATION
\ REM!!                     TAKES PLACE UPON RETURN FROM THIS ROUTINE.
\ REM!!
\ REM!!    TO ENSURE THAT AN OVERRIDE IS ACCEPTED, THE OVERRIDE
\ REM!!    NUMBER MAY BE SET TO ONE OF THE VALUES IN THE TABLE.
\ REM!!
\ REM!!****************************************************************

%INCLUDE EAMTSU36.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC37         : CALLED AT ERROR GUIDANCE PROCESSING
\ REM!!  --------
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TS.DISP1$ AND TS.DISP2$
\ REM!!                     TS.USER.RETURN
\ REM!!
\ REM!!
\ REM!!  COMMENTS         : TS.USER.RETURN IS SET TO 0 ON ENTRY.
\ REM!!                     THE DISPLAY LINES CAN BE REMOVED
\ REM!!                     ALTOGETHER BY SETTING TS.USER.RETURN
\ REM!!                     TO NON-ZERO
\ REM!!****************************************************************

%INCLUDE EAMTSU37.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC38         : CALLED AT DEPARTMENT KEY PROCESSING
\ REM!!  --------
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : IR.ITEMCODE$
\ REM!!                     IR.INDICAT0
\ REM!!                     IR.INDICAT1
\ REM!!                     IR.INDICAT2
\ REM!!                     IR.NUMBERS
\ REM!!                     IR.MPGROUP
\ REM!!                     IR.SALEQUAN
\ REM!!                     IR.SALEPRIC
\ REM!!                     IR.LINKEDTO
\ REM!!                     IR.ITEMNAME
\ REM!!                     IR.INDICAT3
\ REM!!                     IR.USEREXIT
\ REM!!
\ REM!!  COMMENTS         : GLOBAL VARIABLES LISTED AS REFERENCE ONLY
\ REM!!                     SHOULD NOT BE CHANGED.
\ REM!!****************************************************************

%INCLUDE EAMTSU38.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC39         : CALLED AT ITEM ENTRY LIMIT CHECKING
\ REM!!  --------
\ REM!!                     CALLED WHENEVER AN ERROR GUIDANCE IS TO
\ REM!!                     BE STACKED OR MAY BE STACKED DEPENDING
\ REM!!                     ON THE LIMITS SET.
\ REM!!
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : SL.IT.INDICAT3A - ITEM TYPE
\ REM!!                     TS.XXMOD - INDICATES LIMIT TYPE
\ REM!!                                (IF APPLICABLE)
\ REM!!                        1  = "DP" DEPOSIT
\ REM!!                        2  = "RF" REFUND
\ REM!!                        3  = "DR" DEPOSIT RETURN
\ REM!!                        4  = "MR" MISC. RECEIPT
\ REM!!                        5  = "MP" MISC. PAYOUT
\ REM!!                        6  = "MC" MANF. COUPON
\ REM!!                        7  = "SC" STORE COUPON
\ REM!!                        8  = "CL" CANCEL
\ REM!!                        9  = "DC" DEPOSIT CANCEL
\ REM!!                        10 = "WT" WEIGHT
\ REM!!                        11 = "VF" VERIFIED ITEM
\ REM!!                        12 = "LO" LOANS
\ REM!!                        13 = "PI" PICKUPS
\ REM!!                        14 = "TC" TENDER COUNT
\ REM!!                        15 = "PR" PRICE OVERRIDE
\ REM!!                        16 = "SV" STORE COUPON VALIDATION
\ REM!!                        17 = "TR" TRANSACTION LIMIT
\ REM!!                        21 = "DI" DISCOUNT
\ REM!!                        22 = "RT" DISCOUNT RATE
\ REM!!                        23 = "TX" TAX EXEMPTION
\ REM!!                        24 = "VT" VOID TRANSACTION
\ REM!!                        26 = "PE" PENDING OVERRIDE
\ REM!!                        27 = "CV" CANCEL VALIDATION
\ REM!!                        29 = "MV" MANF COUPON VALIDATION
\ REM!!                        30 = "EX" TENDER EXCHANGE
\ REM!!                        31 = "CM" COUPON MULTIPLICATION
\ REM!!                        32 = "TL" TILL EXCHANGE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : ALL PARAMETERS TO DISPLAY GUIDANCE
\ REM!!
\ REM!!      (SEE DOCUMENTATION FOR DETAILS)
\ REM!!
\ REM!!  COMMENTS         : GLOBAL VARIABLES LISTED AS REFERENCE ONLY
\ REM!!                     SHOULD NOT BE CHANGED.
\ REM!!
\ REM!!       THIS USER EXIT ALLOWS THE USER TO ALTER ANY DISPLAY
\ REM!!       GUIDANCE.  FOR EXAMPLE THE USER MAY ALTER THE
\ REM!!       PARAMETERS FOR AN INVALID VOID GUIDANCE TO REQUIRE
\ REM!!       AN OPERATORS OVERRIDE ONLY.  THE TYPE OF ERROR MAY
\ REM!!       BE DETERMINED BY EXAMINING TS.LINEDATA (THE ERROR
\ REM!!       MESSAGE NUMBER BXXX) AND TS.MO.REASON (THE MANAGER'S
\ REM!!       OVERRIDE REASON AS EXCEPTION LOGGED).  IN ORDER TO
\ REM!!       CAUSE AN ERROR MESSAGE NOT TO BE DISPLAYED AT ALL,
\ REM!!       SET AN AMOUNT AND LIMIT SUCH THAT THE AMOUNT IS LESS
\ REM!!       THAN THE LIMIT (SET TS.STACKERR(1) AND TS.STACKERR(2)
\ REM!!       BOTH TO ZERO) AND SET TS.STACKERR(0) TO SAY LIMITS
\ REM!!       IN EFFECT (EG. 1).
\ REM!!       NOTE THAT 4 IS TREATED AS 104 BECAUSE LIMITS ARE
\ REM!!       ALWAYS OVERRIDABLE.
\ REM!!
\ REM!!****************************************************************

%INCLUDE EAMTSU39.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC40         : CALLED AT TENDER LIMIT CHECKING
\ REM!!  --------
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : SL.IT.INDICAT3A - ITEM TYPE
\ REM!!                     TS.XXMOD - INDICATES LIMIT TYPE
\ REM!!                                (IF APPLICABLE)
\ REM!!                        25 = "FS" FOODSTAMP CHANGE
\ REM!!                        28 = "TN" TENDER AMOUNT
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : ALL PARAMETERS TO DISPLAY GUIDANCE
\ REM!!
\ REM!!      (SEE DOCUMENTATION FOR DETAILS)
\ REM!!
\ REM!!  COMMENTS         : GLOBAL VARIABLES LISTED AS REFERENCE ONLY
\ REM!!                     SHOULD NOT BE CHANGED.
\ REM!!
\ REM!!       THIS USER EXIT ALLOWS THE USER TO ALTER A DISPLAY
\ REM!!       GUIDANCE FOR A TENDER LIMIT CHECK.
\ REM!!       TS.LINEDATA = 63 ON ENTRY TO THIS EXIT.
\ REM!!       IN ORDER TO STOP DISPLAY OF THE GUIDANCE ALTOGETHER,
\ REM!!       SET AN AMOUNT AND LIMIT SUCH THAT THE AMOUNT IS LESS
\ REM!!       THAN THE LIMIT (SET TS.STACKERR(1) AND TS.STACKERR(2)
\ REM!!       BOTH TO ZERO) AND SET TS.STACKERR(0) TO SAY LIMITS
\ REM!!       IN EFFECT (EG. 1).
\ REM!!
\ REM!!****************************************************************

%INCLUDE EAMTSU40.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC41         : CALLED BEFORE IO PROCESSOR UNLOCK
\ REM!!  --------
\ REM!!  THIS USER EXIT IS REMOVED (OBSOLETE CANCEL VALIDATION EXIT).
\ REM!!
\ REM!!
\ REM!!****************************************************************

\ REM!!****************************************************************
\ REM!!  TSUPEC42         : CALLED AT COUPON VALIDATION
\ REM!!  --------
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   :
\ REM!!    SL.IT.INDICAT3A - COUPON TYPE
\ REM!!              6 - MANUFACTURER COUPON
\ REM!!              7 - STORE COUPON
\ REM!!
\ REM!!    SL.IT.XPRICE - COUPON VALUE
\ REM!!    SL.IT.DEPARTME - DEPARTMENT
\ REM!!    SL.IT.FAMILYNU - FAMILY NUMBER
\ REM!!
\ REM!!    COUPON.QUANTITIES (A,B,C)
\ REM!!    COUPON.AMOUNTS (A,B)
\ REM!!
\ REM!!    WHERE A - LEVEL OF MATCHING
\ REM!!              0 - ITEM/COUPON HAS SAME FAMILY CODE
\ REM!!              1 - ITEM/COUPON HAS SAME FAMILY GROUP
\ REM!!              2 - ITEM/COUPON HAS SAME SUMMARY GROUP
\ REM!!              3 - ITEM/COUPON MATCHES ON MRF#/DEPT ONLY
\ REM!!
\ REM!!          B - TYPE OF TOTAL
\ REM!!              0 - ALL
\ REM!!              1 - TOTAL OF TAX A ITEMS/COUPONS ONLY
\ REM!!              2 - TOTAL OF TAX B ITEMS/COUPONS ONLY
\ REM!!              3 - TOTAL OF TAX C ITEMS/COUPONS ONLY
\ REM!!              4 - TOTAL OF TAX D ITEMS/COUPONS ONLY
\ REM!!              5 - TOTAL OF NON-FS ITEMS/COUPONS ONLY
\ REM!!              6 - TOTAL OF NON-DISC. ITEMS/COUPONS ONLY
\ REM!!
\ REM!!          C - QUANTITIES BY TYPE
\ REM!!              0 - NUMBER OF ITEMS
\ REM!!              1 - NUMBER OF MANUFACTURER COUPONS
\ REM!!              2 - NUMBER OF STORE COUPONS
\ REM!!
\ REM!!    NOTE: THE ABOVE TOTALS DO NOT INCLUDE THE CURRENT
\ REM!!    COUPON ENTRY BEING PROCESSED.
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE :
\ REM!!
\ REM!!    SL.IT.XPRICE - COUPON VALUE
\ REM!!    SL.IT.INDICAT1 - FLAGS AS DOCUMENTED
\ REM!!    SL.IT.INDICAT2 - FLAGS AS DOCUMENTED
\ REM!!
\ REM!!  COMMENTS :
\ REM!!       THE GENERAL PURPOSE OF THIS ROUTINE IS TO CONTROL
\ REM!!       THE VALUE OF A COUPON.  IT IS CALLED AS SOON AS
\ REM!!       THE VALUE OF A COUPON HAS BEEN DETERMINED BUT
\ REM!!       BEFORE THE VALUE IS USED.  THE USER IS ABLE TO ALTER
\ REM!!       THE VALUE OF THE COUPON, OR THE TAXABILITY AND
\ REM!!       VARIOUS OTHER DATA AT THIS POINT.
\ REM!!
\ REM!!       MODIFICATIONS CONCERNING THE ACCEPTANCE OR REJECTION
\ REM!!       OF A COUPON SHOULD BE PERFORMED AT TSUPEC39.
\ REM!!
\ REM!!****************************************************************

%INCLUDE EAMTSU42.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC43         : CALLED AT PRICE COMPUTATION
\ REM!!  --------
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : SL.IE.SALEPRIC - PRICE ON FILE
\ REM!!                     SL.IE.SALEPRIC2- SECOND PRICE ON FILE
\ REM!!                     SL.IE.QTYORWGT - QUANTITY OR WEIGHT
\ REM!!                     SL.IE.SALEPRIC2- SECOND PRICE ON FILE
\ REM!!                     SL.IE.SALEQUAN - SALE QUANTITY
\ REM!!                     SL.IE.DEALQUAN - TOTAL DEAL QUANTITY
\ REM!!                     SL.IE.METHOD   - PRICING METHOD
\ REM!!
\ REM!!    NOTE: THE TOTAL DEAL QUANTITY IS THE TOTAL NUMBER
\ REM!!    OF ITEMS IN THE DEAL IN THIS TRANSACTION AND INCLUDES
\ REM!!    THE CURRENT ENTRY.  THE NORMAL DEAL RULES APPLY, IE.
\ REM!!    SAME MIX AND MATCH NUMBER OR SAME ITEM CODE IF A ZERO
\ REM!!    MIX AND MATCH NUMBER ETC.
\ REM!!
\ REM!!    A VOID MAY BE DETECTED BY EXAMINING THE KEYBOARD BUFFER.
\ REM!!
\ REM!!    A PRICING METHOD GREATER THAN 5 IS IS NOT REJECTED
\ REM!!    AND THUS MAY BE USED AS ANOTHER PRICING METHOD PROCESSED
\ REM!!    IN THIS USER EXIT.  SL.IT.XPRICE WILL NORMALLY CONTAIN
\ REM!!    THE EXTENDED PRICE ON ENTRY TO THIS ROUTINE.  IF THE
\ REM!!    PRICING METHOD WAS GREATER THAN 5 THEN THE PRICE WILL
\ REM!!    BE SET TO ZERO ON ENTRY.
\ REM!!
\ REM!!    NOTE THAT THE PRICE FIELD IS SUPPLIED SPLIT INTO TWO
\ REM!!    FIELDS, THE FIRST FIVE DIGITS AND THE SECOND FIVE DIGITS.
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : SL.IT.XPRICE - EXTENDED PRICE
\ REM!!
\ REM!!  COMMENTS         : GLOBAL VARIABLES LISTED AS REFERENCE ONLY
\ REM!!                     SHOULD NOT BE CHANGED.
\ REM!!****************************************************************

%INCLUDE EAMTSU43.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC44         : CALLED AT DISCOUNT COMPUTATION
\ REM!!  --------
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : TS.TOTALS - 3D ARRAY OF TRANSACTION TOTALS
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TS.DISC.SAVE - 2D ARRAY OF DISCOUNT AMOUNTS
\ REM!!
\ REM!!  COMMENTS         : GLOBAL VARIABLES LISTED AS REFERENCE ONLY
\ REM!!                     SHOULD NOT BE CHANGED.
\ REM!!****************************************************************

%INCLUDE EAMTSU44.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC45         : CALLED AT CASH DRAWER LIMIT CHECKING
\ REM!!  --------
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : TE.TR.AMT(1) - CASH AMOUNT
\ REM!!                     TO.CDLIMIT1  - FIRST CASH DRAWER LIMIT
\ REM!!                     TO.CDLIMIT2  - SECOND CASH DRAWER LIMIT
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TS.CDLIM1.EXCEEDED - FLAG INDICATING
\ REM!!                     FIRST CASH DRAWER LIMIT EXCEEDED
\ REM!!                     TS.CDLIM2.EXCEEDED - FLAG INDICATING
\ REM!!                     SECOND CASH DRAWER LIMIT EXCEEDED
\ REM!!
\ REM!!  COMMENTS         : GLOBAL VARIABLES LISTED AS REFERENCE ONLY
\ REM!!                     SHOULD NOT BE CHANGED.
\ REM!!****************************************************************

%INCLUDE EAMTSU45.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC46         : CALLED AT TRADING STAMP CALCULATION
\ REM!!  --------
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : STAMPS - AMOUNT OF STAMPS CALC'ED
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : TS.STAMPABLE(1) - DISCOUNTABLE TRADING
\ REM!!                     STAMPABLE AMOUNT AND GRAND TOTAL
\ REM!!                     TRADING STAMPABLE AMOUNT.
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TS.USER.RETURN - INCREMENT TO STAMP AMOUNT
\ REM!!
\ REM!!  COMMENTS         : UPON ENTRY TS.USER.RETURN IS ZERO. THE
\ REM!!                     VALUE OF TS.USER.RETURN WILL BE ADDED TO
\ REM!!                     THE AMOUNT OF STAMPS CALCULATED.
\ REM!!****************************************************************

%INCLUDE EAMTSU46.J86

\ REM!!*****************************************************************
\ REM!!  TSUPEC47         : CALLED AT REFT INITIALIZATION
\ REM!!  --------
\ REM!!
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : SEE COMMENTS BELOW
\ REM!!                     STAMPABLE AMOUNT AND GRAND TOTAL
\ REM!!                     TRADING STAMPABLE AMOUNT.
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : SEE COMMENTS BELOW
\ REM!!
\ REM!!  COMMENTS         : TSUPEC47 IS CALLED AT REFT INTIIALIZATION
\ REM!!
\ REM!!  COMMENTS         : PERTINENT TO TSUPEC47-50
\ REM!!  THESE ROUTINES ARE CALLED AT MSR/PINPAD INITIALIZATION,
\ REM!!  TERMINATION, READS, WRITES. THE EFTS TERMINAL APPLICATION
\ REM!!  EXPECTS MESSAGES IN VISA II FORMAT. THESE ROUTINES ARE
\ REM!!  NOT CALLED FOR LINK-LEVEL RESPONSES (ACK,NAK,EOT).
\ REM!!  NOTE THAT IT IS NOT NECESSARY TO RECALCULATE A LRC
\ REM!!  CHARACTER FOR CHANGED OUTBOUND MESSAGES AS THIS WILL BE
\ REM!!  PERFORMED BY THE CALLING APPLICATION.
\ REM!!  A LIST IS PROVIDED INDICATING THE NAMES OF VARIABLES RELEVANT
\ REM!!  TO EACH OF THE USER EXITS.
\ REM!!
\ REM!!  DIRECTIONS:
\ REM!!
\ REM!!    ALL VARIABLES MENTIONED BELOW MUST BE EXTERNALLY DECLARED
\ REM!!  BEFORE USE.
\ REM!!    IF IT IS DESIRED TO CHANGE A MESSAGE, SET UP THE NEW MESSAGE
\ REM!!  IN STRING TS.ET.USER$. ALL MESSAGE CONTROL CHARACTERS SUCH AS
\ REM!!  STX AND ETX MUST BE ALSO SET UP THERE. THE ORIGINAL MESSAGE
\ REM!!  WILL BE FOUND IN
\ REM!!    DL.IN.MSG$, WITH LENGTH DL.IN.MSGLEN      (FOR TSUPEC50)
\ REM!!    DL.OUT.MSG$, LEFTMOST DL.OUT.MSGLEN BYTES (FOR TSUPEC49)
\ REM!!  (NOTE THAT DL.OUT.MSG$ IS A FIXED-LENGTH 256-BYTE BUFFER)
\ REM!!  NONE OF THE ABOVEMENTIONED VARIABLES MAY BE CHANGED; WITH
\ REM!!  THE EXCEPTION THAT IN ORDER TO DISCARD AN OUTBOUND MESSAGE,
\ REM!!  SET DL.OUT.MSGLEN TO 0.
\ REM!!
\ REM!!  OTHER VARIABLES WHOSE VALUE ONLY MAY BE USED:
\ REM!!    DL.TYPE$      (ST)  EXTRACTED MESSAGE TYPE OF THE FORM "XX."
\ REM!!    DL.IN.MSGCNT  (I2)  INBOUND SEQUENCE NUMBER
\ REM!!    DL.IN.MSGCNT  (I2)  OUTBOUND SEQUENCE NUMBER
\ REM!!
\ REM!!    DL.DEVSTAT    (I4)  RESULT OF LAST GETLONG FROM SERIAL LINK
\ REM!!
\ REM!!  VARIABLES WHOSE VALUE MAY BE CHANGED TO INFLUENCE PROCESSING:
\ REM!!    DL.OUT.MSGLEN (I2)  SET TO 0 TO INHIBIT MESSAGE SEND
\ REM!!    DL.RESET.SEQ  (I2)  FLAG: SET TO RESET SEQUENCE NUMBERS
\ REM!!
\ REM!!  SOME VARIABLES ARE DEFINED BELOW FOR USE WITHIN THIS MODULE.
\ REM!!  OTHERS MAY BE DEFINED AS NECESSARY.
\ REM!!
\ REM!!*****************************************************************

%INCLUDE EAMTSU47.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC48         : CALLED AT REFT TERMINATION
\ REM!!  --------
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : SEE TSUPEC47 ABOVE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : SEE TSUPEC47 ABOVE
\ REM!!
\ REM!!  COMMENTS         : SEE TSUPEC47 ABOVE
\ REM!!****************************************************************

%INCLUDE EAMTSU48.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC49         : CALLED BEFORE REFT SERIAL WRITE
\ REM!!  --------
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : SEE TSUPEC47 ABOVE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : SEE TSUPEC47 ABOVE
\ REM!!
\ REM!!  COMMENTS         : SEE TSUPEC47 ABOVE. USE THIS EXIT TO
\ REM!!    MANIPULATE THE MESSAGE TO BE WRITTEN TO THE MSR/PINPAD.
\ REM!!    LRC CHECKING IS PERFORMED AFTER RETURN FROM THIS
\ REM!!    ROUTINE. SEQUENCE NUMBER INSERTION HAS ALREADY BEEN
\ REM!!    PERFORMED.
\ REM!!****************************************************************

%INCLUDE EAMTSU49.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC50         : CALLED AFTER REFT SERIAL READ
\ REM!!  --------
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : SEE TSUPEC47 ABOVE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : SEE TSUPEC47 ABOVE
\ REM!!
\ REM!!  COMMENTS         : SEE TSUPEC47 ABOVE. USE THIS EXIT TO
\ REM!!    MANIPULATE THE MESSAGE READ FROM THE MSR/PINPAD.
\ REM!!    ANY MESSAGE READ FROM THE DEVICE
\ REM!!    SHOULD BE MADE TO CONFORM TO THE VISA II STANDARDS OF THE
\ REM!!    TERMINAL APPLICATION. (ANY MESSAGE WHICH DOES NOT
\ REM!!    CONFORM THUS WILL PRODUCE UNPREDICTABLE RESULTS).
\ REM!!    NOTE THAT BY THE TIME THIS ROUTINE IS CALLED LRC
\ REM!!    VALIDATION HAS ALREADY BEEN PERFORMED, ALTHOUGH
\ REM!!    SEQUENCE NUMBER VALIDATION HAS NOT.
\ REM!!****************************************************************

%INCLUDE EAMTSU50.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC51         : CALLED BEFORE IO PROCESSOR UNLOCK
\ REM!!  --------
\ REM!!
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : CALLPARM - 0 = MAJOR UNLOCK
\ REM!!                                1 = ERROR PROCESSING UNLOCK
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TS.IO.STATE     - STATE IF CALLPARM = 0
\ REM!!                     TS.IO.NEXTSTATE - STATE IF CALLPARM = 1
\ REM!!                     CHANGING THESE VARIBALES WILL UNLOCK TO THE STATE
\ REM!!                     SPECIFIED. ENSURE THAT IF CALLPARM = 1 THEN
\ REM!!                     TS.IO.STATE MUST REMAIN UNCHANGED, AND IF
\ REM!!                     CALLPARM = 0 THEN
\ REM!!                     TS.IO.NEXTSTATE MUST REMAIN UNCHANGED.
\ REM!!
\ REM!!  COMMENTS         : IF CALLPARM = 1 THEN THE USER EXIT WILL BE CALLED
\ REM!!                     AT LEAST EVERY 50 MSECS; THUS CODE MUST BE AS
\ REM!!                     EFFICIENT AS POSSSIBLE.
\ REM!!
\ REM!!****************************************************************

%INCLUDE EAMTSU51.J86
!AIR75105
\ REM!!****************************************************************
\ REM!!  TSUPEC52         : CALLED AFTER READ OF MSR
\ REM!!  --------
\ REM!!
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : TS.TEMP1$ - DATA AS READ FROM MSR
\ REM!!                                 THIS STRING IS PROVIDED FOR USER
\ REM!!                                 EXIT, IT IS NOT USED FROM THIS
\ REM!!                                 VARIABLE BY THE APPLICATION.
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TS.IO.DATA$(9) ACCOUNT NUMBER EXTRACTED FROM
\ REM!!                                 DATA READ AS IT WILL BE USED BY
\ REM!!                                 THE APPLICATION.  THIS DATA IS
\ REM!!                                 AN UNPACKED ASCII STRING.
\ REM!!
\ REM!!  COMMENTS         : THE SEPARATOR CHARACTER USED BY THE         ALLED
\ REM!!                     APPLICATION TO EXTRACT THE ACCOUNT NUMBER   S
\ REM!!                     FROM THE DATA READ FROM THE MSR IS A X'0D'.
\ REM!!                     DATA UP TO THE SEPARATOR IS TAKEN AS
\ REM!!                     THE ACCOUNT NUMBER.  IF NO SEPARATOR IS
\ REM!!                     FOUND THEN THE ACCOUNT NUMBER IS ASSUMED
\ REM!!                     TO BE THE FIRST 16 BYTES.
\ REM!!
\ REM!!****************************************************************

%INCLUDE EAMTSU52.J86
!EIR75105

!AIR80607
\ REM!!****************************************************************
\ REM!!  TSUPEC53         : CALLED AT TRANSACTION RECOVERY BEFORE EACH
\ REM!!  --------           STRING IS PROCESSED.
\ REM!!
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : SL.STR.ENTRY$ - CONTAINS THE CURRENT STRING.
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TS.TEMP1I2 - STRING TYPE TO BE PROCESSED.
\ REM!!                     IF SET TO 0, ANY PROCESSING FOR THIS STRING
\ REM!!                     IS BYPASSED.
\ REM!!
\ REM!!  COMMENTS         : EXIT PROVIDED FOR HANDLING UNIQUE STRING    ALLED
\ REM!!                     DATA DURING TRANSACTION RECOVERY.           S
\ REM!!
\ REM!!****************************************************************

%INCLUDE EAMTSU53.J86
!EIR80607


!AIR82110 - New User Exit
\ REM!!****************************************************************
\ REM!!  TSUPEC54         : Called After Timeout from "WAIT" on the IOPROC
\ REM!!  --------
\ REM!!
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!  UPDATE           :
\ REM!!
\ REM!!  COMMENTS         : This user exit was provided to allow the user
\ REM!!                     to perform general timeout processing after
\ REM!!                     the the "WAIT" has timedout on the IOPROC.
\ REM!!
\ REM!!                     THIS USER EXIT WILL BE CALLED AT LEAST EVERY
\ REM!!                     SECOND;THUS CODE MUST BE AS EFFICIENT AS
\ REM!!                     POSSSIBLE.
\ REM!!                     AVOID USING THIS EXIT UNLESS YOU HAVE A NEED
\ REM!!                     TO PERFORM PROCESSING EVERY SECOND.
\ REM!!
\ REM!!****************************************************************
%INCLUDE EAMTSU54.J86
!EIR82110 - New User Exit

!AIR80166
\ REM!!****************************************************************
\ REM!!  TSUPEC55         : CALLED AT BARCODE PROCESSING
\ REM!!  --------
\ REM!!
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : ITEMCODE$ - The original item code entered.
\ REM!!                     ITEMVALID - Flag indicating if the item code
\ REM!!                                 is valid.
\ REM!!                     ITEMKEY$  - Corresponding item record key
\ REM!!                                 produced.
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : NONE
\ REM!!
\ REM!!  COMMENTS         : EXIT PROVIDED FOR HANDLING BARCODES
\ REM!!
\ REM!!****************************************************************

%INCLUDE EAMTSU55.J86
!EIR80166

!AIR90981
\ REM!!****************************************************************
\ REM!!  TSUPEC56         : CALLED AT SUSPENSION TIME
\ REM!!  --------
\ REM!!
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  REFERENCE ONLY   : These are the variables that the suspension
\ REM!!                     process saved in the header record:
\ REM!!                     TS.BALDUE(0)
\ REM!!                     SL.HD.TRANSNUM+1
\ REM!!                     SL.HD.DATETIME$
\ REM!!                     SL.HD.TRANSTYPE
\ REM!!                     SL.END
\ REM!!                     SL.HD.OPERATOR$
\ REM!!                     TS.RINGTIME
\ REM!!                     TS.TENDERTI
\ REM!!                     TS.SPECIAL
\ REM!!                     TS.INACTIVE
\ REM!!                     TS.PENDCOUNT
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : NONE
\ REM!!
\ REM!!  COMMENTS         : EXIT PROVIDED FOR THE SUSPENSION OF
\ REM!!                     TRANSACTION. TRANSACTION HEADER COULD BE
\ REM!!                     REBUILT AT THIS TIME TO STORE DIFFERENT
\ REM!!                     DATA. IF SO, THERE MUST BE A RESTORE HEADER
\ REM!!                     ROUTINE AT RETRIEVAL TIME TO OBTAIN THE DATA
\ REM!!                     SAVED. IN ADDITIONS, TRANSACTION DATA STRING
\ REM!!                     SUCH AS THE ITEM VOID STRINGS CAN BE
\ REM!!                     EXTRACTED OR ADDED BEFORE WRITING TO THE
\ REM!!                     SEQUENTIAL FILE.
\ REM!!
\ REM!!****************************************************************

%INCLUDE EAMTSU56.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC57         : CALLED AT RETRIEVAL TIME
\ REM!!  --------
\ REM!!
\ REM!!  TYPE             : FUNCTIONAL PROCESSING EXIT
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  REFERENCE ONLY   : These are the variables that the retrieval
\ REM!!                     process restores from the header record:
\ REM!!                     TS.RINGTIME
\ REM!!                     TS.TENDERTI
\ REM!!                     TS.SPECIAL
\ REM!!                     TS.INACTIVE
\ REM!!                     TS.PENDCOUNT
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TS.RETV.STR$ - a global string that contains
\ REM!!                     the header record of the retrieving trx.
\ REM!!
\ REM!!  COMMENTS         : EXIT PROVIDED FOR THE RETRIEVAL OF
\ REM!!                     TRANSACTION. IF THE TRANSACTION HEADER
\ REM!!                     HAS BEEEN REBUILT BY USEREXIT TSUPEC56,
\ REM!!                     THIS IS THE TIME TO RESTORE THE HEADER.
\ REM!!                     SINCE TRANSACTION DATA HAVE BEEN SUCCESSFULL
\ REM!!                     READ FROM THE SEQUENTIAL FILE, ANY DATA
\ REM!!                     STRINGS CAN BE ADDED OR EXTRACTED BEFORE
\ REM!!                     RECOVERING THE SUSPENDED TRANSACTION.
\ REM!!
\ REM!!****************************************************************

%INCLUDE EAMTSU57.J86
!EIR90981
!AIR39146
\ REM!!****************************************************************
\ REM!!  TSUPEC58         : Before Auto Power Down of Terminal
\ REM!!  --------
\ REM!!
\ REM!!  TYPE             : Time Sequence Exit
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  REFERENCE ONLY   : NONE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : TS.TEMP1$ - Contains the date and time of the
\ REM!!                                 scheduled power on in the format
\ REM!!                                 "0DDhhmm", where DD is the day,
\ REM!!                                 hh is the hour, and mm is the
\ REM!!                                 minute.  If the power-on time for
\ REM!!                                 today has already passed, then the
\ REM!!                                 value of DD will be one more than
\ REM!!                                 today's date, even on the last
\ REM!!                                 day of the month.  If the power-on
\ REM!!                                 time has not yet passed, then DD
\ REM!!                                 will contain the value of today's date.
\ REM!!                                 The AutoPower off may be avoied by
\ REM!!                                 setting TS.TEMP1$ to NULL ("").
\ REM!!
\ REM!!  COMMENTS         : Exit provided to allow the user to do processing
\ REM!!                     prior to powering down the terminal.
\ REM!!
\ REM!!****************************************************************

%INCLUDE EAMTSU58.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC59         : After writing to the Transaction Summary Log
\ REM!!  --------
\ REM!!
\ REM!!  TYPE             : I/O Exit
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  REFERENCE ONLY   : TS.TRANS.LOG.WRITE
\ REM!!                     If the value of this flag is -1, then
\ REM!!                     the exit was called after a write of
\ REM!!                     transaction data to the summary log.
\ REM!!                     If the value of the flag is 0, then the
\ REM!!                     exit was called after a write of
\ REM!!                     exception log data to the summary log.
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : NONE
\ REM!!
\ REM!!  COMMENTS         : No parameters are passed and no global
\ REM!!                     variables are available for modification.
\ REM!!
\ REM!!****************************************************************

%INCLUDE EAMTSU59.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC60         : After writing a line to the customer receipt
\ REM!!  --------
\ REM!!
\ REM!!  TYPE             : I/O Exit
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  REFERENCE ONLY   : TS.PRTBUF$,
\ REM!!                     EP.WIDE.PRTBUF$
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : NONE
\ REM!!
\ REM!!  COMMENTS         : When this exit is called, the print data is
\ REM!!                     contained in TS.PRTBUF$ or EP.WIDE.PRTBUF$
\ REM!!                     (if using a 4610 printer and your receipt
\ REM!!                     options are set to more than 38 bytes of
\ REM!!                     data) AFTER format characters have been
\ REM!!                     added to the data.  NOTE: This is the data
\ REM!!                     as it was sent to the printer, and may be
\ REM!!                     longer in length than the data that was
\ REM!!                     passed through user exit 20.  Control
\ REM!!                     characters may be added to the front or the
\ REM!!                     back of the data since the execution of
\ REM!!                     TSUPEC20.  If you are using a 4610 thermal
\ REM!!                     printer and have selected a receipt width
\ REM!!                     larger than 38 bytes, then TS.PRTBUF$
\ REM!!                     should contain the print data without
\ REM!!                     print control characters.  This may not
\ REM!!                     be true in all cases.  If Electronic
\ REM!!                     Marketing is NOT installed, a GLOBAL variable
\ REM!!                     delcaration will need to be added to be added
\ REM!!                     to EAMTSUVA.J86 in order to access the GLOBAL
\ REM!!                     variable EP.WIDE.PRTBUF$.
\ REM!!
\ REM!!****************************************************************

%INCLUDE EAMTSU60.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC61         : After writing a line to the summary journal
\ REM!!  --------
\ REM!!
\ REM!!  TYPE             : I/O Exit
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  REFERENCE ONLY   : TS.PRTBUF$
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : NONE
\ REM!!
\ REM!!  COMMENTS         : TS.PRTBUF$ contains the data that was sent
\ REM!!                     to the printer and may contain print control
\ REM!!                     characters that have been added to the
\ REM!!                     front or back of the string AFTER TSUPEC21
\ REM!!                     has executed.
\ REM!!
\ REM!!                     If a 4610 printer is installed, the data will
\ REM!!                     not actually be printed, but the user still
\ REM!!                     gets control in this exit.
\ REM!!
\ REM!!****************************************************************

%INCLUDE EAMTSU61.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC62         : After writing a line on the document insert
\ REM!!  --------           station
\ REM!!
\ REM!!  TYPE             : I/O Exit
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  REFERENCE ONLY   : TS.PRTBUF$
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : NONE
\ REM!!
\ REM!!  COMMENTS         : TS.PRTBUF$ contains the data that was sent
\ REM!!                     to the printer and may contain print control
\ REM!!                     characters that have been added to the
\ REM!!                     front or back of the string AFTER TSUPEC22
\ REM!!                     has executed.
\ REM!!
\ REM!!****************************************************************

%INCLUDE EAMTSU62.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC63         : After writing a line to the display
\ REM!!  --------
\ REM!!
\ REM!!  TYPE             : I/O Exit
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  REFERENCE ONLY   : TS.IN.BACKUP   -1 IF IN LOOP BACKUP
\ REM!!                                     0 IF NOT IN LOOP BACKUP
\ REM!!                     TS.SV.LINETYPE  1 - 13 DISPLAY LINE TYPE
\ REM!!                     TS.DISP1$       Top Line of Display 1
\ REM!!                     TS.DISP2$       Bottom Line of Display 2
\ REM!!                     TS2.DISP1$      Top Line of Display 2
\ REM!!                     TS2.DISP2$      Bottom Line of Display 2
\ REM!!                     LED.REQD        LEDs on the Shopper Display
\ REM!!                                     which were lit up
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : NONE
\ REM!!
\ REM!!  COMMENTS         : Exit provided to allow the user to do
\ REM!!                     condition processing AFTER a message is
\ REM!!                     displayed.
\ REM!!
\ REM!!****************************************************************

%INCLUDE EAMTSU63.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC64         : After opening the cash drawer
\ REM!!  --------
\ REM!!
\ REM!!  TYPE             : I/O Exit
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  REFERENCE ONLY   : TS.USER.RETURN
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : NONE
\ REM!!
\ REM!!  COMMENTS         : This exit is called immediately after the
\ REM!!                     write to the cash drawer to open it.  This
\ REM!!                     exit is called whether or not the cash
\ REM!!                     drawer has actually been opened.  Variable
\ REM!!                     TS.USER.RETURN contains the value set in
\ REM!!                     TSUPEC24.  If TS.USER.RETURN is non-zero,
\ REM!!                     opening of the drawer was bypassed due to
\ REM!!                     TSUPEC24.
\ REM!!
\ REM!!****************************************************************

%INCLUDE EAMTSU64.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC65         : After writing a message to the external
\ REM!!  --------           verification pipe
\ REM!!
\ REM!!  TYPE             : I/O Exit
\ REM!!
\ REM!!  INPUT PARMS.     : CALLPARM - This variable will contain the
\ REM!!                                return code of the PRSTWRT function.
\ REM!!                                A zero value indicates a successful
\ REM!!                                PRS pipe write.
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  REFERENCE ONLY   : MS.DEST$       - PRS Destination
\ REM!!                     TS.ET.XMITBUF$ - Data sent in request
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : NONE
\ REM!!
\ REM!!  COMMENTS         : Exit provided in order for the user to
\ REM!!                     perform additional processing based upon
\ REM!!                     the success or failure of the write performed
\ REM!!                     to the external verification pipe.
\ REM!!
\ REM!!****************************************************************

%INCLUDE EAMTSU65.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC66         : After manager override verification is complete
\ REM!!  --------
\ REM!!
\ REM!!  TYPE             : Functional Processing Exit
\ REM!!
\ REM!!  INPUT PARMS.     : CALLPARM - (-1) if MGR override validation is
\ REM!!                                     successful
\ REM!!                                ( 0) if MGR override validation is
\ REM!!                                     not successful
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  REFERENCE ONLY   : TS.GUIDANCE,
\ REM!!                     TS.TEMP1I4,
\ REM!!                     TS.MMMOD,
\ REM!!                     SL.OVERRIDE$
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : NONE
\ REM!!
\ REM!!  COMMENTS         : When manager's override validation is not
\ REM!!                     successful, TS.GUIDANCE may be inspected to
\ REM!!                     determine if "B003 Check Key Sequence" is to
\ REM!!                     be displayed (1003) or if "B059 Check
\ REM!!                     Override Number" (1059) is to be displayed.
\ REM!!                     The Exit is called immediately prior to
\ REM!!                     posting the message and getting the CLEAR
\ REM!!                     key from the operator.
\ REM!!
\ REM!!                     On a successful entry, SL.OVERRIDE$ contains
\ REM!!                     the packed override number that was entered.
\ REM!!                     TS.MMMOD contains the slot in the
\ REM!!                     TO.OVRIDNUM array for the successful entry.
\ REM!!                     TS.TEMP1I4 contains the matching override
\ REM!!                     value, which may be different than what is
\ REM!!                     contained in SL.OVERRIDE$ if the value has
\ REM!!                     been changed during user exit 36 processing.
\ REM!!
\ REM!!****************************************************************

%INCLUDE EAMTSU66.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC67         : After price computation is complete
\ REM!!  --------
\ REM!!
\ REM!!  TYPE             : Functional Processing Exit
\ REM!!
\ REM!!  INPUT PARMS.     : NONE
\ REM!!
\ REM!!  OUTPUT PARMS.    : NONE
\ REM!!
\ REM!!  REFERENCE ONLY   : SL.IE.SALEPRIC - PRICE ON FILE
\ REM!!                     SL.IE.SALEPRIC2- SECOND PRICE ON FILE
\ REM!!                     SL.IE.QTYORWGT - QUANTITY OR WEIGHT
\ REM!!                     SL.IE.SALEQUAN - SALE QUANTITY
\ REM!!                     SL.IE.DEALQUAN - TOTAL DEAL QUANTITY
\ REM!!                     SL.IE.METHOD   - PRICING METHOD
\ REM!!                     SL.IT.XPRICE - EXTENDED PRICE
\ REM!!                     TS.XPRICE - EXTENDED PRICE
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : None
\ REM!!
\ REM!!  COMMENTS         : This exit is called after the variable
\ REM!!                     TS.XPRICE has been set up.  The call of
\ REM!!                     this exit ONLY occurs once the item entry
\ REM!!                     has been accepted, but prior to the print
\ REM!!                     and display of the item.  This exit will
\ REM!!                     be called while processing items during
\ REM!!                     transaction recovery.
\ REM!!
\ REM!!****************************************************************

%INCLUDE EAMTSU67.J86

\ REM!!****************************************************************
\ REM!!  TSUPEC68         : Before Updating the Summary Log Array
\ REM!!  --------
\ REM!!  This exit is called before the application is about to add
\ REM!!  an entry to the summary log array.  This exit might be used
\ REM!!  to alter summary log array elements as they are placed in the
\ REM!!  array, or to add new entries to the array based upon the
\ REM!!  entry just received.
\ REM!!
\ REM!!
\ REM!!  TYPE             : Functional Processing Exit
\ REM!!
\ REM!!  INPUT PARMS.     : ENTRY$, ELEMENT
\ REM!!
\ REM!!  OUTPUT PARMS.    : ENTRY$, ELEMENT
\ REM!!
\ REM!!  REFERENCE ONLY   : None.
\ REM!!
\ REM!!  GLOBALS FOR
\ REM!!  UPDATE           : SL.END
\ REM!!
\ REM!!  COMMENTS         : This exit gives the user control
\ REM!!                     immediately before the string ENTRY$
\ REM!!                     is written to the summary log array.
\ REM!!                     Variable ELEMENT is the position within the
\ REM!!                     array that the string ENTRY$ is to be
\ REM!!                     written.  The header string for a transaction
\ REM!!                     (ELEMENT 0) may be written to the summary log
\ REM!!                     array multiple times for a single order.  The
\ REM!!                     user may alter the data in ENTRY$ prior to
\ REM!!                     its inclusion in the summary log array.  The
\ REM!!                     user may also use this exit to add strings to
\ REM!!                     the transaction summary log array.  If you
\ REM!!                     are using Big Memory model, additional strings
\ REM!!                     may be written to the summary log array simply
\ REM!!                     by creating the string and inserting it into
\ REM!!                     SL.STR$ at the appropriate location.  If this
\ REM!!                     method is used, then the user must also update
\ REM!!                     global variable SL.END appropriately.  For
\ REM!!                     users of either Big Memory Model or Medium
\ REM!!                     Memory model, the user may call WRITE.SL.STR
\ REM!!                     to add an entry to the array (all Medium Memory
\ REM!!                     Model users should use this method).  In order
\ REM!!                     to do so, file EAMTSXHC.J86 needs to be
\ REM!!                     included in EAMTSUPC.BAS, if it is not already
\ REM!!                     included.  The user should beware that calling
\ REM!!                     WRITE.SL.STR from this user exit will cause
\ REM!!                     re-entry into this user exit.
\ REM!!
\ REM!!****************************************************************

%INCLUDE EAMTSU68.J86

!EIR39146
