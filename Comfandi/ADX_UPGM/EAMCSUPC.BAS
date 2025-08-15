\/* TIME STAMP BLOCK **********************************************
\** END OF TIME STAMP BLOCK **************************************/
\/****************************************************************/
\/*                                                              */
\/*      MODULE NAME: EAMCSUPC                                   */
\/*                                                              */
\/*      DESCRIPTIVE NAME: CHECKOUT SUPPORT USER EXITS           */
\/*                                                              */
\/*                                                              */
\/*      COPYRIGHT:                                              */
\/*      5696-536 THIS MODULE IS "RESTRICTED MATERIALS OF IBM"   */
\/*      (C) COPYRIGHT IBM CORP 1986, 1993 ALL RIGHTS RESERVED   */
\/*      LICENSED MATERIALS - PROPERTY OF IBM REFER TO COPYRIGHT */
\/*      INSTRUCTIONS FORM NUMBER G120-2083                      */
\/*                                                              */
\/*      STATUS: RELEASE 2 LEVEL 0                               */
\/*                                                              */
\/*      STATUS: RELEASE 1 LEVEL 0                               */
\/*                                                              */
\/*      PERSON RESPONSIBLE =                                    */
\/*                                                              */
\/*      FUNCTION: EAMCSUPC                                      */
\/*                                                              */
\/*                THIS MODULE CONTAINS THE EXTERNAL FUNCTIONS   */
\/*                DEFINING THE USER EXITS FOR CHECKOUT SUPPORT. */
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
\/*      ENTRY POINT: CSUPEC01                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AFTER READ OF SUMMARY LOG  */
\/*                  SALES RECORD.                               */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: WHENEVER SUMMARY LOG SALES RECORD IS READ.  */
\/*                                                              */
\/*      ENTRY POINT: CSUPEC02                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT CALLED AFTER READ OF SUMMARY LOG  */
\/*                  NON-SALES RECORD.                           */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: WHENEVER SUMMARY LOG NON-SALES RECORD IS    */
\/*                  READ.                                       */
\/*                                                              */
\/*      ENTRY POINT: CSUPEC03                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT FOR ANY ITEM IN A SALES           */
\/*                  TRANSACTION.                                */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: BEFORE PROCESSING AN ITEM IN A SALES        */
\/*                  TRANSACTION.                                */
\/*                                                              */
\/*      ENTRY POINT: CSUPEC04                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT FOR WRITING TO                    */
\/*                  ACCOUNTING FILE.                            */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: BEFORE WRITING ANY RECORD TO THE            */
\/*                  ACCOUNTING FILE.                            */
\/*                                                              */
\/*      ENTRY POINT: CSUPEC05                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT FOR WRITING TO                    */
\/*                  OPERATOR PERFORMANCE FILE.                  */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: BEFORE WRITING ANY RECORD TO THE            */
\/*                  OPERATOR PERFORMANCE FILE.                  */
\/*                                                              */
\/*      ENTRY POINT: CSUPEC06                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT FOR WRITING TO                    */
\/*                  DEPARTMENT TOTALS FILE.                     */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: BEFORE WRITING ANY RECORD TO THE            */
\/*                  DEPARTMENT TOTALS FILE.                     */
\/*                                                              */
\/*      ENTRY POINT: CSUPEC07                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT FOR WRITING TO                    */
\/*                  TENDER LISTING FILE.                        */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: BEFORE WRITING ANY RECORD TO THE            */
\/*                  TENDER LISTING FILE.                        */
\/*                                                              */
\/*      ENTRY POINT: CSUPEC08                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT FOR WRITING TO                    */
\/*                  MISCELLANEOUS TRANSACTIONS FILE.            */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: BEFORE WRITING ANY RECORD TO THE            */
\/*                  MISCELLANEOUS TRANSACTIONS FILE.            */
\/*                                                              */
\/*      ENTRY POINT: CSUPEC09                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT FOR WRITING TO                    */
\/*                  ITEM MOVEMENT FILE.                         */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: BEFORE WRITING ANY RECORD TO THE            */
\/*                  ITEM MOVEMENT FILE.                         */
\/*                                                              */
\/*      ENTRY POINT: CSUPEC10                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT FOR WRITING TO                    */
\/*                  TERMINAL PRODUCTIVITY FILE.                 */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: BEFORE WRITING ANY RECORD TO THE            */
\/*                  TERMINAL PRODUCTIVITY FILE.                 */
\/*                                                              */
\/*      ENTRY POINT: CSUPEC11                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT FOR WRITING TO                    */
\/*                  EXCEPTION LOG.                              */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: BEFORE WRITING ANY RECORD TO THE            */
\/*                  EXCEPTION LOG.                              */
\/*                                                              */
\/*      ENTRY POINT: CSUPEC12                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT FOR OPENING ALL TOTALS FILES.     */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: AFTER OPENING TOTALS FILES.                 */
\/*                                                              */
\/*      ENTRY POINT: CSUPEC13                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT FOR TOTALS UPDATE DISK ERROR.     */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: BEFORE PROCESSING TOTALS UPDATE DISK ERROR. */
\/*                                                              */
\/*      ENTRY POINT: CSUPEC14                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT FOR TRANSACTION DATA ENTRY        */
\/*                  STRING.                                     */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: BEFORE PROCESSING TRANSACTION DATA ENTRY    */
\/*                  STRING.                                     */
\/*                                                              */
\/*      ENTRY POINT: CSUPEC15                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT FOR ITEM OR TENDER STRING.        */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: BEFORE PROCESSING ITEM OR TENDER STRING.    */
\/*                                                              */
\/*      ENTRY POINT: CSUPEC16                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT FOR STORE CLOSING RECORD.         */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: AFTER PROCESSING STORE CLOSING RECORD.      */
\/*                                                              */
\/*      ENTRY POINT: CSUPEC17                                   */
\/*                                                              */
\/*         PURPOSE: USER EXIT FOR TIMER PROCESSING.             */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: AFTER CHECKOUT SUPPORT TIMER PROCESSING.    */
\/*                                                              */
\/*  AIR45806                                                    */
\/*      ENTRY POINT: CSUPEC18                                   */
\/*                                                              */
\/*         PURPOSE: AFTER READ OF TRANSACTION SUMMARY LOG.      */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: AFTER READING TRANSACTION SUMMARY LOG.      */
\/*                                                              */
\/*  EIR45806                                                    */
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
\/*             NAME: CSMLEC02 : MODULE TRACING                  */
\/*             NAME: CSVFEC02 : BREAK STRING INTO FIELDS        */
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
\/*      CHANGE DATE: 06/05/85                                   */
\/*                                                              */
\/*   IR45806 - Add User Exit 18 - After reading Transaction     */
\/*             Summary Log.                                     */
\/*             CMJ MGVA 14May2001                               */
\/*                                                              */
\/* END-OF-SPECIFICATIONS ****************************************/

\ REM!!**************** INCLUDE FILES *********************************
\ REM!!
\ REM!! %INCLUDE EAMCSXPT.J86    exception log
\ REM!! %INCLUDE EAMCSGLO.J86    checkout support global variables
\ REM!! %INCLUDE EAMTLIST.J86    tender listing file
\ REM!! %INCLUDE EAMTPROD.J86    terminal productivity file
\ REM!! %INCLUDE EAMMTRNC.J86    miscellaneous transactions file
\ REM!! %INCLUDE EAMCUSTA.J86    customer account status file   IR68987
\ REM!! %INCLUDE EAMTRANS.J86    transaction summary log        IR68987
\ REM!! %INCLUDE EAMSOPTS.J86    store options                  IR68987
\ REM!! %INCLUDE EAMADXRT.J86    system services interface      IR68987
\ REM!! %INCLUDE EAMIWORK.J86    item movement work file        IR77860
\ REM!! %INCLUDE EAMCSUVA.J86    user exit global variables
\ REM!! %INCLUDE EAMCSUSU.J86    user exit common subroutines
\ REM!! %INCLUDE EAMCSU01.J86    user exit 1  code
\ REM!! %INCLUDE EAMCSU02.J86    user exit 2  code
\ REM!! %INCLUDE EAMCSU03.J86    user exit 3  code
\ REM!! %INCLUDE EAMCSU04.J86    user exit 4  code
\ REM!! %INCLUDE EAMCSU05.J86    user exit 5  code
\ REM!! %INCLUDE EAMCSU06.J86    user exit 6  code
\ REM!! %INCLUDE EAMCSU07.J86    user exit 7  code
\ REM!! %INCLUDE EAMCSU08.J86    user exit 8  code
\ REM!! %INCLUDE EAMCSU09.J86    user exit 9  code
\ REM!! %INCLUDE EAMCSU10.J86    user exit 10 code
\ REM!! %INCLUDE EAMCSU11.J86    user exit 11 code
\ REM!! %INCLUDE EAMCSU12.J86    user exit 12 code
\ REM!! %INCLUDE EAMCSU13.J86    user exit 13 code
\ REM!! %INCLUDE EAMCSU14.J86    user exit 14 code
\ REM!! %INCLUDE EAMCSU15.J86    user exit 15 code
\ REM!! %INCLUDE EAMCSU16.J86    user exit 16 code
\ REM!! %INCLUDE EAMCSU17.J86    user exit 17 code
\ REM!! %INCLUDE EAMCSU18.J86    user exit 18 code  IR45806
\ REM!!
\ REM!!****************************************************************

 %INCLUDE EAMCSXPT.J86
 %INCLUDE EAMCSGLO.J86
 %INCLUDE EAMTLIST.J86
 %INCLUDE EAMTPROD.J86
 %INCLUDE EAMMTRNC.J86
 %INCLUDE EAMCUSTA.J86            ! customer account status file  IR68987
 %INCLUDE EAMTRANS.J86            ! transaction summary log       IR68987
 %INCLUDE EAMSOPTS.J86            ! store options                 IR68987
 %INCLUDE EAMCSUVA.J86            ! user exit global variables
 %INCLUDE EAMIWORK.J86            ! item movement work file       IR77860
 %INCLUDE EAMCSCF1.J86            ! checkout support cntl file    IR92597
!
 %INCLUDE EAMXXCPY.J86            ! Copyright Statement
 %INCLUDE EAMASMCT.J86            ! assembler routines
 %INCLUDE EAMADXRT.J86            ! system services interface     IR68987

\ REM!!**************** EXTERNAL  ROUTINES ****************************

\ REM!!****************************************************************
\ REM!!* CHECKOUT SUPPORT USER EXITS - AVAILABLE I/O SESSION NUMBERS  *
\ REM!!****************************************************************
\ REM!!*                                                              *
\ REM!!* The following I/O session numbers are available for use :    *
\ REM!!*                                                              *
\ REM!!* IR74084 - corrected list of available session numbers        *
\ REM!!* IR78667 - DELETE SESSION # 25 FROM LIST                      *
\ REM!!* IR95920 - DELETE SESSIONS # 56 and 60 FROM LIST              *
\ REM!!* IR98151 - DELETE SESSIONS # 59 FROM LIST                     *
\ REM!!*                                                              *
\ REM!!*            4,  5,  9, 10, 11, 16, 20, 23, 24,                *
\ REM!!*           26, 54, 55, 57, 58, 62, 63                         *   *
\ REM!!*                                                              *
\ REM!!****************************************************************

\ REM!!****************************************************************
\ REM!!* SUB CSVFEC02 split string into fields                        *
\ REM!!****************************************************************
\ REM!!*                                                              *
\ REM!!* Purpose : To split a data entry string in the summary log        *
\ REM!!* -------   into its constituent fields.                       *
\ REM!!*                                                              *
\ REM!!* Calling interface:                                           *
\ REM!!* -----------------                                            *
\ REM!!*    Parameters       : position of string in string CS.INPUT$ *
\ REM!!*                       e.g. if the fifth string in transaction*
\ REM!!*                       then call is CALL CSVFEC02(5)          *
\ REM!!*                                                              *
\ REM!!* On return, the array CS.FLD.LEN is set up to the length of   *
\ REM!!* each field in the data entry string.                         *
\ REM!!*                                                              *
\ REM!!* !IR78084 correct description of CS.FLD.POS                   *
\ REM!!*                                                              *
\ REM!!* On return, the array CS.FLD.POS is set up to the position of *
\ REM!!* the colon preceding each field in the data entry string.     *
\ REM!!*                                                              *
\ REM!!****************************************************************

 FUNCTION CSVFEC02( P1 ) EXTERNAL
     INTEGER*2 P1
 END FUNCTION

 FUNCTION CSMLEC02( P1 ) EXTERNAL
     INTEGER*2 P1
 END FUNCTION

!AIR71561 - move include file from above
 %INCLUDE EAMCSUSU.J86            ! user exit common subroutines
!EIR71561
\ REM!!****************************************************************
\ REM!!  CSUPEC01        : User exit called after read of summary log
\ REM!!  --------          sales record
\ REM!!  Type            : I/O exit
\ REM!!
\ REM!!  Input parms.    : none
\ REM!!
\ REM!!  Return parm.    : None
\ REM!!
\ REM!!  Globals for
\ REM!!  reference only  : CS.INPUT$         Transaction string
\ REM!!
\ REM!!                    This string contains the transaction as it
\ REM!!                    appears in the summary log.
\ REM!!
\ REM!!  Globals for
\ REM!!  update          : none
\ REM!!
\ REM!!  Comments        : Global variables listed as refernce only
\ REM!!                    must not be changed.
\ REM!!****************************************************************

%INCLUDE EAMCSU01.J86

\ REM!!****************************************************************
\ REM!!  CSUPEC02        : User exit called after read of summary log
\ REM!!  --------          non-sales record
\ REM!!  Type            : I/O exit
\ REM!!
\ REM!!  Input parms.    : none
\ REM!!
\ REM!!  Return parm.    : None
\ REM!!
\ REM!!  Globals for
\ REM!!  reference only  : CS.INPUT$         Transaction string
\ REM!!
\ REM!!                    This string contains the transaction as it
\ REM!!                    appears in the summary log.
\ REM!!
\ REM!!  Globals for
\ REM!!  update          : none
\ REM!!
\ REM!!  Comments        : Global variables listed as refernce only
\ REM!!                    must not be changed.
\ REM!!****************************************************************

%INCLUDE EAMCSU02.J86

\ REM!!****************************************************************
\ REM!!  CSUPEC03        : User exit called before processing any item
\ REM!!  --------          in a sales transaction
\ REM!!  Type            : Time sequence exit
\ REM!!
\ REM!!  Input parms.    : none
\ REM!!
\ REM!!  Return parm.    : None
\ REM!!
\ REM!!  Globals for
\ REM!!  reference only  : CS.INPUT$         Transaction string
\ REM!!                    This string contains the transaction as it
\ REM!!                    appears in the summary log.
\ REM!!
\ REM!!                    CS.STR   index to the item string about
\ REM!!                             to be processed
\ REM!!
\ REM!!  Globals for
\ REM!!  update          : none
\ REM!!
\ REM!!  Comments        : This user exit is called before any item
\ REM!!                    is processed.
\ REM!!                    Global variables listed as refernce only
\ REM!!                    must not be changed.
\ REM!!****************************************************************

%INCLUDE EAMCSU03.J86

\ REM!!****************************************************************
\ REM!!  CSUPEC04        : User exit called before writing accounting
\ REM!!  --------          record
\ REM!!  Type            : I/O exit
\ REM!!
\ REM!!  Input parms.    : ACCTREC$ - accounting record about
\ REM!!                               to be written
\ REM!!
\ REM!!  Return parm.    : Accounting record after update to user exit
\ REM!!                    fields
\ REM!!
\ REM!!  Comments        : The record to be written is passed as one
\ REM!!                    string as if read using READ LINE.
\ REM!!                    The user exit should check the key fields
\ REM!!                    to see if the record passed is the store
\ REM!!                    record or an operator or terminal record.
\ REM!!                    Only the user exit fields in the record
\ REM!!                    may be changed. All other fields may be
\ REM!!                    inspected but should be left unaltered.
\ REM!!                    The user exit fields may be set using the
\ REM!!                    provided assembler subroutine PUTN4.
\ REM!!****************************************************************

%INCLUDE EAMCSU04.J86

\ REM!!****************************************************************
\ REM!!  CSUPEC05        : User exit called before writing operator
\ REM!!  --------          performance record
\ REM!!  Type            : I/O exit
\ REM!!
\ REM!!  Input parms.    : PERFREC$ - performnace record about
\ REM!!                               to be written
\ REM!!
\ REM!!  Return parm.    : Performance record after update to user exit
\ REM!!                    fields
\ REM!!
\ REM!!  Comments        : The record to be written is passed as one
\ REM!!                    string.
\ REM!!                    The user exit should check the key fields
\ REM!!                    to see if the record passed is the store
\ REM!!                    record or an operator record.
\ REM!!                    Only the user exit fields in the record
\ REM!!                    may be changed. All other fields may be
\ REM!!                    inspected but should be left unaltered.
\ REM!!                    The user exit fields may be set using the
\ REM!!                    provided assembler subroutine PUTN4.
\ REM!!****************************************************************

%INCLUDE EAMCSU05.J86

\ REM!!****************************************************************
\ REM!!  CSUPEC06        : User exit called before writing department
\ REM!!  --------          totals record
\ REM!!  Type            : I/O exit
\ REM!!
\ REM!!  Input parms.    : P1 - gives the offset within CS.ST.STR$ to
\ REM!!                    record just updated.
\ REM!!                                  8 = store record
\ REM!!                    any other value = department record
\ REM!!
\ REM!!
\ REM!!  Return parm.    : None - updates are to global variables
\ REM!!
\ REM!!  Globals for
\ REM!!  reference only  :     CS.DT.STR$        Complete record
\ REM!!
\ REM!!                        Refer to the supermarket data base
\ REM!!                        to find the offsets to various fields
\ REM!!
\ REM!!  Globals for
\ REM!!  update          : 1 - Store record
\ REM!!                        CS.DT.STR$        Complete record
\ REM!!                        User exit fields are at offset P1 + 64
\ REM!!                        and consist of 5 4-byte integer fields.
\ REM!!
\ REM!!                    2 - Department record
\ REM!!                        CS.DT.STR$        Complete record
\ REM!!                        User exit field is at offset P1 + 38
\ REM!!                        and is a 4-byte integer field.
\ REM!!
\ REM!!  Comments        : Global variables listed as refernce only
\ REM!!                    must not be changed.
\ REM!!                    The user exit fields may be set using the
\ REM!!                    provided assembler subroutine PUTN4.
\ REM!!****************************************************************

%INCLUDE EAMCSU06.J86

\ REM!!****************************************************************
\ REM!!  CSUPEC07        : User exit called before writing tender
\ REM!!  --------          listing record
\ REM!!  Type            : I/O exit
\ REM!!
\ REM!!  Input parms.    : P1 - indicates type of record about to be
\ REM!!                    written.
\ REM!!                    1 - tender record
\ REM!!                    2 - start/end of period record
\ REM!!                    3 - group delimiter record
\ REM!!
\ REM!!  Return parm.    : None - updates are to global variables
\ REM!!
\ REM!!  Globals for
\ REM!!  reference only  : 1 - Tender record
\ REM!!                        WORK1$            Record type
\ REM!!                        SL.HD.TERMINAL$   Terminal number
\ REM!!                        TL.TR.TENDTYPE$   Tender type
\ REM!!                        SL.HD.OPERATOR$   Operator number
\ REM!!                        TL.TR.CUSTOMER$   Customer a/c number
\ REM!!                        TL.TR.AMTTENDER$  Tender amount
\ REM!!                        TL.TR.AMTTNFEE$   Fee amount
\ REM!!
\ REM!!                    2 - Start/end of period record
\ REM!!                        WORK3$            Record type
\ REM!!                        WORK4$            Date/time of start/end
\ REM!!
\ REM!!                    3 - Group delimiter record
\ REM!!                        None
\ REM!!
\ REM!!  Globals for
\ REM!!  update          : 1 - Tender record
\ REM!!                        TL.TR.USEREXIT$   User exit field
\ REM!!
\ REM!!                    2 - Start/end of period
\ REM!!                        None
\ REM!!
\ REM!!                    3 - Group delimiter record
\ REM!!                        TL.GD.USEREXIT$   User exit field
\ REM!!
\ REM!!  Comments        : Global variables listed as refernce only
\ REM!!                    must not be changed.
\ REM!!                    The code below is for example only.
\ REM!!****************************************************************

%INCLUDE EAMCSU07.J86

\ REM!!****************************************************************
\ REM!!  CSUPEC08        : User exit called before writing miscellaneous
\ REM!!  --------          transactions record
\ REM!!  Type            : I/O exit
\ REM!!
\ REM!!  Input parms.    : P1 - indicates type of record about to be
\ REM!!                    written.
\ REM!!                    1 - miscellaneous account record
\ REM!!                    2 - store record
\ REM!!
\ REM!!  Return parm.    : None - updates are to global variables
\ REM!!
\ REM!!  Globals for
\ REM!!  reference only  : 1 - Miscellaneous account record
\ REM!!                        MT.AC.ACCOUNT$    Account number
\ REM!!                        MT.AC.RESTART$    System restart field
\ REM!!                        MT.AC.AMOUNT$     Amount for account
\ REM!!                        MT.AC.NAME$       Account descriptor
\ REM!!
\ REM!!                    2 - Store record
\ REM!!                        MT.ST.ACCOUNT$    Account number
\ REM!!                        MT.ST.DATETIME$   Period start/end time
\ REM!!                        MT.ST.RESERVED$   Reserved
\ REM!!
\ REM!!  Globals for
\ REM!!  update          : 1 - Miscellaneous account record
\ REM!!                        MT.AC.USEREXIT$   User exit field
\ REM!!
\ REM!!                    2 - Store record
\ REM!!                        MT.ST.USEREXIT$   Account number
\ REM!!
\ REM!!  Comments        : Global variables listed as refernce only
\ REM!!                    must not be changed.
\ REM!!                    The two user exit fields are fixed length
\ REM!!                    8 bytes long
\ REM!!                    The code below is for example only.
\ REM!!****************************************************************

%INCLUDE EAMCSU08.J86

\ REM!!****************************************************************
\ REM!!  CSUPEC09        : User exit called before writing item movement
\ REM!!  --------          record
\ REM!!  Type            : I/O exit
\ REM!!
\ REM!!  Input parms.    : P1 - indicates type of record about to be
\ REM!!                    written.
\ REM!!                    1 - item movement record
\ REM!!                    2 - store record
\ REM!!
\ REM!!  Return parm.    : None - updates are to global variables
\ REM!!
\ REM!!  Globals for
\ REM!!  reference only  : 1 - Item movement record
\ REM!!                        CS.IM.REC$
\ REM!!                        offset 0  length 6 Item number
\ REM!!                        offset 18 length 4 System restart field
\ REM!!
\ REM!!                    2 - Store record
\ REM!!                        CS.IM.ITEMCODE$    Item code
\ REM!!                        CS.IM.DATETIME$    Period start/end time
\ REM!!                        CS.IM.RESERVED$    Reserved
\ REM!!
\ REM!!  Globals for
\ REM!!  update          : 1 - Item movement record
\ REM!!                        CS.IM.REC$
\ REM!!                        offset 6  length 4 item count
\ REM!!                        offset 10 length 4 accumulated amount
\ REM!!                        offset 14 length 4 accumulated weight
\ REM!!
\ REM!!                    2 - Store record
\ REM!!                        None
\ REM!!
\ REM!!  Comments        : Global variables listed as refernce only
\ REM!!                    must not be changed.
\ REM!!****************************************************************

%INCLUDE EAMCSU09.J86

\ REM!!****************************************************************
\ REM!!  CSUPEC10        : User exit called before writing terminal
\ REM!!  --------          productivity record
\ REM!!  Type            : I/O exit
\ REM!!
\ REM!!  Input parms.    : P1 - indicates type of record about to be
\ REM!!                    written.
\ REM!!                    1 - productivity record
\ REM!!                    2 - start/end of period record
\ REM!!
\ REM!!  Return parm.    : None - updates are to global variables
\ REM!!
\ REM!!  Globals for
\ REM!!  reference only  : 1 - Productivity record
\ REM!!                        CS.TR.TERMINAL$   Terminal number
\ REM!!                        WORK3$            Date and time written
\ REM!!                        CS.TR.NETSALES    Net sales amount
\ REM!!                        CS.TR.NUMITEMS    Net item sales
\ REM!!                        CS.TR.NUMCOUPS    Store/manf coupon count
\ REM!!                        CS.TR.NUMTRANS    Number transactions
\ REM!!                        CS.TR.RINGTIME    Ring time
\ REM!!                        CS.TR.TENDERTI    Tender time
\ REM!!                        CS.TR.SPECIAL     Special sign-off time
\ REM!!                        CS.TR.INACTIVE    Inactive time
\ REM!!                        CS.TR.NONSALES    Non-sales time
\ REM!!
\ REM!!                    2 - Start/end of period record
\ REM!!                        TP.EP.DATETIME$   start/end date & time
\ REM!!
\ REM!!  Globals for
\ REM!!  update          : 1 - Productivity record
\ REM!!                        TP.PR.USEREXIT$   User exit field
\ REM!!
\ REM!!                    2 - Store record
\ REM!!                        None
\ REM!!
\ REM!!  Comments        : Global variables listed as refernce only
\ REM!!                    must not be changed.
\ REM!!                    The user exit field is variable length.
\ REM!!                    The code below is for example only.
\ REM!!****************************************************************

%INCLUDE EAMCSU10.J86

\ REM!!****************************************************************
\ REM!!  CSUPEC11        : User exit called before writing exception
\ REM!!  --------          log record
\ REM!!  Type            : I/O exit
\ REM!!
\ REM!!  Input parms.    : None
\ REM!!
\ REM!!  Return parm.    : None - updates are to global variables
\ REM!!
\ REM!!  Globals for
\ REM!!  reference only  : Common to all logs:
\ REM!!                        XL.TERMINAL$      terminal number
\ REM!!                        XL.TRANSNUM$      transaction number
\ REM!!                        XL.DATETIME$      date/time logged
\ REM!!                        XL.CODE$          type of exception log
\ REM!!                        XL.OPERATOR$      operator number
\ REM!!                        XL.OVERRIDE$      override number
\ REM!!                        XL.REASON$        override reason
\ REM!!
\ REM!!                    Only for log code = 1:
\ REM!!                        XL.INDICAT0$      exception reason
\ REM!!                        SL.IT.INDICAT3A   type of sale
\ REM!!                        SL.IT.XPRICE      item price
\ REM!!                        SL.IE.QTYORWGT    quantity or weight
\ REM!!                        XL.IT.FILEDEAL$   overridden quantity
\ REM!!                        XL.IT.FILEPRIC$   overridden price
\ REM!!
\ REM!!                    Only for log code = 2:
\ REM!!                        none
\ REM!!
\ REM!!                    Only for log code = 3:
\ REM!!                        WORK3I4           tender type
\ REM!!                        WORK4I4           tender variety
\ REM!!                        WORK1$            account number
\ REM!!                        WORK2I4           tender amount
\ REM!!                        CS.AMTBALAN       balance at tender time
\ REM!!
\ REM!!                    Only for log code = 4:
\ REM!!                        XL.VO.AMTVOID$    void amount
\ REM!!
\ REM!!                    Only for log code = 5:
\ REM!!                        XL.INDICAT0$      from tender type
\ REM!!                        XL.INDICAT1$      to tender type
\ REM!!                        XL.TE.AMTTENDE$   tender amount
\ REM!!
\ REM!!                    Only for log code = 6:
\ REM!!                        XL.INDICAT0$      type of no-sale
\ REM!!
\ REM!!                    Only for log code = 7:
\ REM!!                        SL.HD.NUMSTRIN    no. of transactions
\ REM!!                        SL.HD.GROSSPOS    gross positive total
\ REM!!                        SL.HD.GROSSNEG    gross negative total
\ REM!!
\ REM!!                    Only for log code = 8:
\ REM!!                        XL.IT.ITEMCODE$   item code rejected
\ REM!!
\ REM!!                    Only for log code = 9:
\ REM!!                        none
\ REM!!
\ REM!!                    Only for log code = 10:
\ REM!!                        WORK1$            oldata/newdata fields
\ REM!!
\ REM!!                    Only for log code = 12:
\ REM!!                        XL.INDICAT0$      type of non-sale
\ REM!!                        XL.NN.TRANSDATA$
\ REM!!
\ REM!!                    Only for log code = 13:
\ REM!!                        XL.INDICAT0$      accountability
\ REM!!                        POS.TENDER(1) -   positive and negative
\ REM!!                        NEG.TENDER(1)     tender amounts for
\ REM!!                                          eight tender types
\ REM!!
\ REM!!                    Only for log code = 14:
\ REM!!                        as for 13
\ REM!!
\ REM!!                    Only for log code = 15:
\ REM!!                        none
\ REM!!
\ REM!!                    Only for log code = 16:
\ REM!!                        XL.INDICAT0$      missing found
\ REM!!                                          or duplicate
\ REM!!
\ REM!!                    Only for log code = 18:
\ REM!!                        XL.INDICAT0$      accountability
\ REM!!                        POS.TENDER(1) -   positive and negative
\ REM!!                        NEG.TENDER(1)     tender amounts for
\ REM!!                                          eight tender types
\ REM!!
\ REM!!                    Only for log code = 19:
\ REM!!                        none
\ REM!!
\ REM!!
\ REM!!  Globals for
\ REM!!  update          : Exception log by checkout support
\ REM!!                        XL.USEREXIT$      User exit field
\ REM!!
\ REM!!  Comments        : Global variables listed as refernce only
\ REM!!                    must not be changed.
\ REM!!                    This user exit is not called for log entries
\ REM!!                    added to the summary log by the terminal.
\ REM!!                    The user exit field is variable length.
\ REM!!                    The code below is for example only.
\ REM!!****************************************************************

%INCLUDE EAMCSU11.J86

\ REM!!****************************************************************
\ REM!!  CSUPEC12        : User exit called after opening all totals
\ REM!!  --------          files
\ REM!!  Type            : I/O exit
\ REM!!
\ REM!!  Input parms.    : none
\ REM!!
\ REM!!  Return parm.    : None
\ REM!!
\ REM!!  Globals for
\ REM!!  reference only  : none
\ REM!!
\ REM!!  Globals for
\ REM!!  update          : none
\ REM!!
\ REM!!  Comments        : This user exit will only be called
\ REM!!                    following checkout support initialisation.
\ REM!!                    No parameters are passed and no global
\ REM!!                    variables are available for reference
\ REM!!                    or modification.
\ REM!!****************************************************************

%INCLUDE EAMCSU12.J86

\ REM!!****************************************************************
\ REM!!  CSUPEC13        : User exit called before processing totals
\ REM!!  --------          update disk error
\ REM!!  Type            : Functional processing exit
\ REM!!
\ REM!!  Input parms.    : None
\ REM!!
\ REM!!  Return parm.    : None
\ REM!!
\ REM!!  Globals for
\ REM!!  reference only  : none
\ REM!!
\ REM!!  Globals for
\ REM!!  update          : none
\ REM!!
\ REM!!  Comments        : This user exit is called whenever a disk
\ REM!!                    error occurs on a file which checkout
\ REM!!                    support processes.
\ REM!!                    The following BASIC functions give the
\ REM!!                    details of the error.
\ REM!!                      ERR    error code
\ REM!!                      ERRN   additional error information
\ REM!!                      ERRF%  i/o session number of file in error
\ REM!!****************************************************************

%INCLUDE EAMCSU13.J86

\ REM!!****************************************************************
\ REM!!  CSUPEC14        : User exit called before processing
\ REM!!  --------          transaction data entry string
\ REM!!  Type            : Time sequence exit
\ REM!!
\ REM!!  Input parms.    : None
\ REM!!
\ REM!!  Return parm.    : String to be included in exception log
\ REM!!                    entry (if any)
\ REM!!
\ REM!!  Globals for
\ REM!!  reference only  : CS.INPUT$         Transaction string
\ REM!!                    This string contains the transaction as it
\ REM!!                    appears in the summary log.
\ REM!!
\ REM!!                    CS.STR   index to the data entry string
\ REM!!
\ REM!!  Globals for
\ REM!!  update          : none
\ REM!!
\ REM!!  Comments        : This user exit is called whenever a data
\ REM!!                    entry string is encountered in the summary
\ REM!!                    log.
\ REM!!                    Global variables listed as refernce only
\ REM!!                    must not be changed.
\ REM!!                    The code below is for example only.
\ REM!!****************************************************************

%INCLUDE EAMCSU14.J86

\ REM!!****************************************************************
\ REM!!  CSUPEC15        : User exit called before processing item
\ REM!!  --------          or tender entry string
\ REM!!  Type            : Time sequence exit
\ REM!!
\ REM!!  Input parms.    : P1 indicates item or tender string
\ REM!!                    1 = item string
\ REM!!                    2 = tender string
\ REM!!
\ REM!!  Return parm.    : None
\ REM!!
\ REM!!  Globals for
\ REM!!  reference only  : CS.INPUT$         Transaction string
\ REM!!                    This string contains the transaction as it
\ REM!!                    appears in the summary log.
\ REM!!
\ REM!!                    CS.STR   index to the item or tender string
\ REM!!                             to be processed
\ REM!!
\ REM!!  Globals for
\ REM!!  update          : none
\ REM!!
\ REM!!  Comments        : This user exit is called before any item
\ REM!!                    or tender string is processed.
\ REM!!                    Global variables listed as refernce only
\ REM!!                    must not be changed.
\ REM!!****************************************************************

%INCLUDE EAMCSU15.J86

\ REM!!****************************************************************
\ REM!!  CSUPEC16        : User exit called after processing store
\ REM!!  --------          closing record
\ REM!!  Type            : Time sequence exit
\ REM!!
\ REM!!  Input parms.    : none
\ REM!!
\ REM!!  Return parm.    : None
\ REM!!
\ REM!!  Globals for
\ REM!!  reference only  : CS.INPUT$ close string just processed
\ REM!!
\ REM!!  Globals for
\ REM!!  update          : none
\ REM!!
\ REM!!  Comments        : This user exit will only be called
\ REM!!                    following checkout support processing
\ REM!!                    of a close record.
\ REM!!                    No parameters are passed and global
\ REM!!                    variables listed are available for reference
\ REM!!                    only.
\ REM!!****************************************************************

%INCLUDE EAMCSU16.J86

\ REM!!****************************************************************
\ REM!!  CSUPEC17        : User exit called after timer processing
\ REM!!  --------
\ REM!!  Type            : Time sequence exit
\ REM!!
\ REM!!  Input parms.    : none
\ REM!!
\ REM!!  Return parm.    : None
\ REM!!
\ REM!!  Globals for
\ REM!!  reference only  : none
\ REM!!
\ REM!!  Globals for
\ REM!!  update          : none
\ REM!!
\ REM!!  Comments        : This user exit will only be called
\ REM!!                    following checkout support timer
\ REM!!                    processing i.e. every hour.
\ REM!!                    No parameters are passed and no global
\ REM!!                    variables are available for reference
\ REM!!                    or modification.
\ REM!!****************************************************************

%INCLUDE EAMCSU17.J86

\ REM!!****************************************************************
\ REM!!  CSUPEC18        : User exit called after reading Summary Log
\ REM!!  --------
\ REM!!  Type            : Functional processing exit
\ REM!!
\ REM!!  Input parms.    : none
\ REM!!
\ REM!!  Return parm.    : None
\ REM!!
\ REM!!  Globals for
\ REM!!  reference only  : CS.INPUT$
\ REM!!
\ REM!!  Globals for
\ REM!!  update          : CS.INPUT$
\ REM!!
\ REM!!  Comments        : This user exit is called  from
\ REM!!                    CSMLEC04 after reading a transaction
\ REM!!                    from the Transaction Summary Log
\ REM!!                    into CS.INPUT$.
\ REM!!****************************************************************

%INCLUDE EAMCSU18.J86

