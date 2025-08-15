\/* TIME STAMP BLOCK **********************************************
\** END OF TIME STAMP BLOCK **************************************/
\/****************************************************************/
\/*                                                              */
\/*      MODULE NAME: EAMEJUPC                                   */
\/*                                                              */
\/*      DESCRIPTIVE NAME: JOURNAL USER EXITS                    */
\/*                                                              */
\/*      COPYRIGHT:                                              */
\/*      5696-536 THIS MODULE IS "RESTRICTED MATERIALS OF IBM"   */
\/*      (C) COPYRIGHT IBM CORP 1986, 1993 ALL RIGHTS RESERVED   */
\/*      LICENSED MATERIALS - PROPERTY OF IBM REFER TO COPYRIGHT */
\/*      INSTRUCTIONS FORM NUMBER G120-2083                      */
\/*                                                              */
\/*      STATUS: RELEASE 2 LEVEL 0                               */
\/*                                                              */
\/*      PERSON RESPONSIBLE =                                    */
\/*                                                              */
\/*      FUNCTION: EAMEJUPC                                      */
\/*                                                              */
\/*                THIS MODULE CONTAINS THE EXTERNAL FUNCTIONS   */
\/*                DEFINING THE USER EXITS FOR JOURNAL REPORTING.*/
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
\/*         MODULE SIZE: ??? LOC                                 */
\/*                                                              */
\/*         ATTRIBUTES: REENTRANT                                */
\/*                                                              */
\/*                                                              */
\/*                                                              */
\/*                                                              */
\/* END-OF-SPECIFICATIONS ****************************************/

\ REM!!**************** INCLUDE FILES  ********************************
\ REM!!
\ REM!! %INCLUDE EAMTSWKG.J86        ! working storage
\ REM!! %INCLUDE EAMETWKG.J86        ! EFTS working storage
\ REM!! %INCLUDE EAMITEMR.J86        ! item record file
\ REM!! %INCLUDE EAMTRANS.J86        ! transaction summary log
\ REM!! %INCLUDE EAMTAXTB.J86        ! tax tables
\ REM!! %INCLUDE EAMASMRT.J86        ! assembler subroutines
\ REM!! %INCLUDE EAMADXRT.J86        ! system services functions
\ REM!! %INCLUDE EAMSOPTS.J86        ! store options
\ REM!! %INCLUDE EAMTOPTS.J86        ! terminal options
\ REM!! %INCLUDE EAMEJUVA.J86        ! user exit global variables
\ REM!! %INCLUDE EAMXXCPY.J86        ! copyright statement
\ REM!! %INCLUDE EAMEJUSU.J86        ! user exit common subroutines
\ REM!! %INCLUDE EAMEJUxx.J86        ! user exit code
\ REM!!
\ REM!!****************************************************************

\ REM!!**************** INCLUDE FILES  ********************************

%INCLUDE EAMTSWKG.J86        ! working storage
%INCLUDE EAMETWKG.J86        ! EFTS working storage
%INCLUDE EAMITEMR.J86        ! item record file
%INCLUDE EAMTRANS.J86        ! transaction summary log
%INCLUDE EAMTAXTB.J86        ! tax tables
%INCLUDE EAMSOPTS.J86        ! store options
%INCLUDE EAMTOPTS.J86        ! terminal options
%INCLUDE EAMTSVER.J86        ! totals save file
%INCLUDE EAMEJUVA.J86        ! user exit global variables
%INCLUDE EAMXXCPY.J86        ! copyright statement
!
%INCLUDE EAMASMRT.J86        ! assembler subroutines
%INCLUDE EAMADXRT.J86        ! system services routines

\ REM!!****************************************************************
\ REM!!* JOURNAL USER EXITS - AVAILABLE I/O SESSION NUMBERS
\ REM!!****************************************************************
\ REM!!*                                                              *
\ REM!!* The following sessions numbers are available for use :       *
\ REM!!*                                                              *
\ REM!!* 6, 7, 10, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 38,        *
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
\ REM!!****************************************************************
\ REM!!****************************************************************
\ REM!!IR76369
\ REM!!p30: PRINT FEATURE DESCRIPTOR
\ REM!!
\ REM!!     USED FOR IBM SUPPORTED FEATURES
\ REM!!****************************************************************
 SUB TSPREC01 EXTERNAL       ! print routine
 END SUB

\ REM!!**********  END OF EXTERNAL ROUTINES  **************************

%INCLUDE EAMEJUSU.J86        ! user exit common subroutines


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

%INCLUDE EAMEJU07.J86

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

%INCLUDE EAMEJU20.J86

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

%INCLUDE EAMEJU21.J86

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
\ REM!!  COMMENTS         : EXIT PROVIDED FOR HANDLING UNIQUE STRING
\ REM!!                     DATA DURING TRANSACTION RECOVERY.
\ REM!!
\ REM!!****************************************************************

%INCLUDE EAMEJU53.J86

