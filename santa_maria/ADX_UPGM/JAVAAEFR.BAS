!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
!RGW search on "<READ THIS!" for sections requiring review/rework
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
!
! Module Name:      javaaefr.bas (jaaefrv4.bas)
!
!
! Descriptive Name: Application specific code for the AEF java gui application
!                   For SI V4.
!
! Copyright:
!            (C) COPYRIGHT IBM CORP 2003 ALL RIGHTS RESERVED
!            LICENSED MATERIALS - PROPERTY OF IBM REFER TO COPYRIGHT
!            INSTRUCTIONS FORM NUMBER G120-2083
!
! Status: Release 1 Level 0
!
! Person Responsible =
!
! Function:
!           This module contains the public routines specific to the
!           Supermarket Application to support the Java GUI Customer
!           Display.
!
!
! Change Activity:
!
!
! D3458    Implement SAVP2001 APARs IR51702 (Force unload of scale prior
!          to weight entries if scale not at zero at s/on or special s/off
!          exit), IR51722 (Scale blanks when weight item entered), and
!          IR51405 (Avoid tlog string corruption).
!          RSH IBM 02Jul03
!
! D3872    Terminal Monitor catching substates that should be ignored.
!          TAL IBM 10Sep03
!
! D4559    SA WIC support.
!          TAL IBM 18Aug03
!
! IRkhg    Restructured the way that the java gui interacts with SA.
!          KHG IBM 09Oct03
!
! D6540    Added context-sensitive print lines.
!          GGK IBM 28Oct2004
!
! D6706    Remove scale polling for CSS sessions.
!          CJN IBM 18Nov2004
!
! D2012    SI GUI: Terminal gets B000 Application End in training mode
!          TAL IBM 10Feb05
!
! D7350    SA WIC EBT substates conflicting with GUI SA/ACE Substates
!
! IO05031  Implemented the ability to do a manager override when the tax/notax,
!          food stamp/no foodstamp, manufacturer/store coupon and refund
!          keys are pressed before a department key is pressed.
!          DLB IBM 28Aug06
!
! IO05908  SU 0000007E can occur if multiple EFT tenders are franked.
!          GGK IBM 15Jan07
!
! D2424    Implemented SCS requirements already implemented by ACE.
!          NOTE: SA base APAR IO07467 is pre-requisite.
!           17178 balanceSatisfied added to tender event
!           17667 Till Exchange Event
!                   (prereq EAMTSNSC.BDC, JAVAGUIC.BDC and JAVAGUIV.J86)
!           17914 Number of columns to print in cashReceipt line
!           18127 receiptComplete attribute added to cashReceipt line
!                   (prerequisite change in EAMTSPRC.BDC)
!           18197 Loyalty ID in the Store Options Event
!           18219 Department number added to items and coupons
!           18316 Total Items category must be Trailer
!           18325 Add Date & Time to Suspend Transaction and Void Transaction
!
!           tender-change.  added 'change' to 'tender' event for POSBC
!
!          Already there:
!           18159 Foodstamp balance in the Transaction Totals Event
!           18210 Linked Coupon IDs in the Coupon Event
!           18326 Loyalty savings in the Transaction Totals Event
!           18425 New Substate Changed Event with B112, B113 and B874
!
!          Not implemented now:
!           17994 Department Taxability and Foodstampability
!           18193 Electronic Payment Events/Waiting For Signature
!          GGK IBM 05Sep2007
!
! D2430    Customers with extremely small discounts receive an SS 6E error
!          because the code was expecting at least 3 digits.  SI APAR IO07988.
!          CRM/GGK IBM 03Dec2007
!
! D2474    XML generated for the department number is invalid.  When composing
!          the number tag in the departmentDefinition construct, it uses the
!          index of the department number.  It should use the actual
!          department number defined in the item record that corresponds to
!          the department key.
!          GGK IBM 12Mar2008
!
! D2479    XML generated for the QTYORWGT field is invalid.  The QTYORWGT
!          field as placed into the tlog 02H extension string is not always
!          valid.  If an item with a linked coupon is sold using the quantity
!          key but the linked coupon is issued only once due to validation,
!          then the 0x20 bit of its INDICAT1 field is OFF - this signifies
!          that the QTYORWGT field is not valid.  In cases such as those, use
!          the SALEQUAN field instead.
!          GGK IBM 21Apr2008
!
! D2506    Add new function for the LocaleOptions event, which must follow
!          the loading options event.  When the currency format is standard
!          USA (x,xxx.xx), then the LocaleOptions event is not sent.
!          GGK IBM 09May2008
!
! IO08970  Implement POSBC support for SI V3.
!          CJN IBM 06Jun2008
!
! D2523    If more than 327 items are sold in an order, the item number goes
!          negative.
!          GGK IBM 11Jun2008
!
! D17772   POSBC receipt for a suspended transaction doesn't include enough
!          information, because NonSales lines are discarded by POSBC.
!          GGK IBM 01Jul2008
!
! D2534    End transaction events may be sent after the transaction number
!          has already been incremented.  Therefore we will save the trans
!          number from the transaction start event, and use that for the
!          transaction end event.
!          CJN IBM 10Jul2008
!
! D17710   Always send the totalSavings field, even if it is zero.
!          CJN IBM 29Jul2008
!
! D18204   Weight Entry Line Not Printed In Red When Item Voided.
!          CJN IBM 30Jul2008
!
! D18279   Quantity required items should include a quantity field of "1".
!          CJN IBM 04Aug2008
!
! IO11189  While reading the scale, the use of global variables to temporarily
!          save the contents of variables like TS.ERRPTR can result in the
!          trashing of those variables due to the reuse of them for other
!          purposes during the processing of an ON ERROR condition.
!          An SU, elsewhere, is one symptom.  Instead, use variables that are
!          dedicated for the save-restore purpose.
!          GGK IBM 24Aug2009
!
! IO11370  Fix completion IO11189.  Further protection is needed.  If a
!          javaAEFEvent call is in progress, the contents of many global
!          variables can be trashed at any point by a recursive call that
!          is initiated by a printer ON ASYNC ERROR call.  This can lead to
!          resumption, in particular, at a point where a string operation is
!          about to occur for a string that is no longer in existence.
!          To protect from errors of this type, bracket the entire context
!          of all javaAEFEvent calls with a DISABLE-ENABLE.
!
!          This change has a very slight performance improvement also.
!          Prior to this fix, each string update operation would interrupt
!          the OS Kernal twice.  With this fix, the added DISABLE-ENABLE
!          pair prevents all of those Kernal calls.  Although those OS
!          calls are relatively fast, my measurements show that about
!          2,800 OS calls are eliminated per item sold.
!          GGK IBM 29Sep2009
!
! D21508   Individual items that involve more than two printed lines do not
!          generate complete XML code.  Only two of the lines are included.
!          Likewise, Tenders that involve more than one line (excluding
!          franking) have only one of the lines included.  Up to 5 lines
!          are handled now, by using an array rather than differently-named
!          variables for each of the lines.  If more than 5 exist, the
!          last line is always included and the last excess intermediate
!          lines are ignored.  This design change is not required with Base
!          SA, but will be required if User code or an SA enhancement causes
!          that condition to occur.  The limits of 5 can be altered by
!          changing GLOBAL variables in a user exit.
!
!          Also, add debugging code permanently, and turn it off by default.
!          GGK IBM 21Dec2009
!
! 2625     For serviceability, add the SI version number of this file as a
!          GLOBAL variable (JAVAAEFR.VERSION), which is initialized to 4.
!          GGK IBM 05Nov2009
!
! IO11817  AE IO08970.  Customers who write more than about 190 lines to
!          the receipt header will get a SL 00000066 error because we cannot
!          buffer more than about 190 - 240 print lines of XML within a single
!          64 KB string.  This fix actually limits the string size to 32 KB
!          rather than 64 KB so that we do not need to add a BIGLEN call
!          in the Terminal.  When the string is almost 32 KB, send it
!          to SI and continue to buffer from that point on.  This may
!          cause an incomplete transaction header for failover transactions
!          that use a huge header.
!
!          Also fixed a Base issue involving Training.  The XML string
!          is not sent due to a wrong 'exit' from a GOSUB.  It should
!          have returned rather than exiting.  That bug was made worse
!          by IO11370 because the wrong 'exit' prevents an ENABLE from
!          running, which eventually hangs the application.
!
!          Also globalized some variables to make them easier to find in
!          a dump.  To minimize conflicts with identical globals used for
!          other purposes, the names were also changed.  To reduce change-
!          flag clutter, each of these name-change globalizations is not
!          individually change-flagged.  They are:
!
!            data$                -> SI.data$
!
!          Also added global string SI.javaaefr.trace$ for serviceability.
!          An entry of the trace is:
!            |e=ee t=tt d=dd[nn=cc
!          where
!            ee is the event number,
!            tt is TS.LINETYPE (omitted if it is zero)
!            dd is TS.LINEDATA (omitted if it is zero)
!            nn is the length of the generated XML
!            cc is the first 10 characters of the generated XML
!          The other characters are exactly as shown
!
!          GGK IBM 05Jan2010
!
! IO12312  GUI receipt lines that were created with customer
!          extensions(exit 60) are not displaying on GUI receipt when they
!          are attached to the first item in the order.
!          TLD IBM 05May2010
!
! D22768   The counter for the tender type printline events needs to be set to zero
!          after the printlines are processed. The variable is NUM.TENDER.HDR.LINES.
!          Problem was introduced by defect 21508.
!          TLD IBM 19May2010
!
! IO12917  Customer created xml events do not have ability to wait for
!          start of transaction and may get purged before needed. Create API
!          for customer events to get sent through javaAEFEvent() and provide
!          ability for buffering of events to wait for start of transaction.
!          TLD IBM 11Aug2010
!
! D25022   Using Terminal Transfer, the tender amounts are not captured as
!          tender headers.  Problem was introduced by defect 21508.
!          GGK IBM 23Aug2010
!
! D29485   In certain cases substate 1006 (manager override) was getting sent
!          to AEF twice, back to back. The substate was initiated from
!          TSDSEC01, routine "D6:" when LINEDATA= 1. The 1006 was also
!          getting sent from main. The java hooks will no longer send a substate
!          1006 case when "d6:" originates a jGUISubstate = 1032->11032.
!          TLD IBM 07Jul2011
!
! D2759    Add 2 new attributes to XML string for the departmentDefinition event.
!          rahumes IBM 23Sep2011
!
! IO16905  User Entry unable to modify receipt header to work properly in CHEC
!          that do work OK on SA POS. Add IF filters in label.receiptEvent.
!          rahumes TGCS 17Aug2012
!
!
! IO18536  Possible SI GUI hang when no scale, performing transactions, 
!          and printer runs out of paper.
!          Add flag to stop reading scale when scale is offline.
!          rahumes TGCS 20Feb2013
!
! IO19200  Print lines greater than 38 characters are truncated on the 
!          Electronic receipt.
!          Code was changed to handle these longer lines. COREQ SA APAR IO19207
!          CRM TGCS 10JUL2013
!
! IO19200  Add SUB label.receipt.setup.java() to fix linker failure 
!          "OBJECT FILE ERROR 2" with " RECORD: LEDATA" 
!          in the GOSUB label.receiptEvent:.
!          rahumes TGCS 13Aug2013
!
! IO19427  A non-weighed item with PM 2 sets the wrong value
!          for the "itemEvent" message "quantity" field.
!          Defect 2479 needed to take SL.IE.INDICAT1= 0 
!          into account. 
!          rahumes TGCS 18Sep2013
!
! IO21722  To allow user to override javaGuiClass$ in a user exit
!          only set variable when it is null.
!          CRM TGCS 10Jul2014
!
! IO21731  Add into label.receiptEvent: a call to new routine retrieveFormatSub:
!          in order to handle retrieves properly per POSBC. ACE was the model.
!          rahumes TGCS 17Jul2014 
!
! IO22151  Add retrieveProcessing attribute in transactionUpdateMsg on a retrieve.
!          Rename work$ to work1$. 
!          rahumes TGCS 23Sep2014 
!
!
! END-OF-SPECIFICATIONS
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
%ENVIRON T

%INCLUDE JAVAGUIV.J86                     ! javagui variables
!%INCLUDE JAVAGUIS.J86                     ! Global Variables added for SureVision IRkhg

%INCLUDE EAMTSWKG.J86                     ! supermarket appl wking storage
%INCLUDE EAMTRANS.J86                     ! global defines for tlog vars
%INCLUDE EAMITEMR.J86                     ! global defines for item record vars
%INCLUDE EAMTERMS.J86
%INCLUDE EAMOPERA.J86                     ! operator file include
%INCLUDE EAMTOPTS.J86                     ! terminal options
%INCLUDE EAMSOPTS.J86                     ! store options
%INCLUDE EAMP4VAJ.J86                     ! 4610 variable
%INCLUDE USERCADF.J86                     ! EM Activity file vars

string    printLine.rawData$

INTEGER*1 GLOBAL JAVAAEFR.VERSION         ! set to SI version (4) D2625

INTEGER*2 GLOBAL JAVA.TENDER.HDR.MAX      ! Allow customer to set D21508
INTEGER*2 GLOBAL JAVA.ITEM.PRT.MAX        ! Allow customer to set D21508

STRING    GLOBAL                          \
          ITEM.99.UNPACKED$,              \ needed for itemJustAdded
\A3458 AIR51405
          java.amt$,                      \ formatted dollar amount
          ts2.temp1$,                     \
          ts.weightsep$,                  \
          java.wgt$,                      \ formatted weight
\E3458 EIR51405
          EMSS.PMSG$,                     \ PERSONALIZED TRAILER DISPLAY
\AD21508 Use a pair of Tender arrays instead of two strings
\         JAVA.TENDER.HDR.LINE$,          \ TENDER header print line
\         JAVA.TENDER.HDR.LINE.RAW$,      \ TENDER header print line raw
          JAVA.TENDER.HDR.LINE$(1),       \ TENDER header print lines
          JAVA.TENDER.HDR.LINE.RAW$(1),   \ TENDER header print lines raw
\ED21508
          JAVA.TENDER.PRT.LINE$(1),       \ TENDER print line
          JAVA.TENDER.PRT.LINE.RAW$(1),   \ TENDER print line raw
          TS.OPER.NAME$,                  \ operator name
          CA.EXPDATE$,                    \
          MC.ID$,                         \ Foreign currency ISO identifier
          EO.STORE.NAME$,                 \
          EO.ADDR1$,                      \
          EO.ADDR2$,                      \
          JAVA.USEREVENT.DATA$            ! IO12917 User event info to pass to AEF

!AIO19200
INTEGER*2 GLOBAL                 \
   EP.ETO.DEFAULT.CR.WIDTH       ! Default cash receipt character line length.
STRING GLOBAL                    \                                                            
   EP.WIDE.PRTBUF$               ! Wide Print buffer (used when EP.ETO.DEFAULT.CR.WIDTH > 38 )
STRING sav.prtbuf$               ! Work variable                                                           
!EIO19200

string global Nuo.Xml.Cate$,Nuo.Xml.Clave$,Nuo.Xml.Valor$

INTEGER   GLOBAL                          \ D2424
          OP.VA.VALUE                     \ VALIDATION VALUE D2424/18197

!AD2759
INTEGER*1 GLOBAL \
        IRLOOK.INDICAT1(1)
!ED2759


INTEGER*1 GLOBAL                          \
          AMOUNT.DECIMAL.PLACES,          \ D2506  0 (default) implies 2
          javaRecoveryStartTrx,           \ need start trx event?
          ts.ds.amount.overflow,          \ amount overflow occurred  3458 IR51405
          WIC.ITEM,                       \ Flag if current item is wic item
          WIC.MODE,                       \ Flag to indicate wic trans
          WIC.ENABLE,                     \ Flag to indicate feature enabled
          WIC.ERROR,                      \ Wic error indicator
          EAN99,                          \ needed for itemJustAdded
          TS.RECOVERY,                    \ in recovery flag
          TS.SUSP.IN.PROGRESS,            \ suspension in progress
          TS.RETV.IN.PROGRESS,            \ retrieval in progress
          TS.RETV.BEGIN,                  \ on to begin retrieve in progress, off to end it AIO22151
          JAVA.TENDER.PRT.INDEX,          \ Index used to point into the JAVA.TENDER.PRT.LINE$ arrays
          TO.EBT,                         \ electronic benefits used?
          receiptCompleted,               \ end of transaction flag D2424/18127
          JAVA.PRICE.VERIFY,              \ Set during a priceVerifyMsg              IO08970
          JAVA.TENDER.HDR.SIGNATURE,      \ Indicates a tender signature header line IO08970
          JAVA.TENDER.PRT.SIGNATURE(1),   \ Indicates a tender signature line        IO08970
          EMSS.ALTERED.UPC,               \
          JAVA.USEREVENT.FLAG             ! IO12917 User Flag for controlling AEF logic



!Begin @tal10032003
INTEGER*2 GLOBAL MC.ENABLED,              \ MC Feature Flag IR37838
                 MAXLUPS,                 \ maximum number of lookups defined
                 aefRequiredAge%

STRING  GLOBAL \
        IRLOOK.DEPARTME$(1),  \           !dept numbers D2474
        IRLOOK.ITEMNAME$(1)   \           !dept names
!End @tal10032003


!Begin @tal08182003
INTEGER*1 GLOBAL      \
          JAVA.DEBUG.UNITNUM,                  \ for debugging only D21508
          WIC.CHECK.DATE.FORMAT                ! 0=no date checking
                                               ! 1=date is needed in form mmddyy
                                               ! 2=date is needed in form ddmmyy
INTEGER*2 GLOBAL      \
          WIC.DISC.GROUP, \ which if any disc group to use for tax exempt
          WIC.TDR1,   \   ! valid tender/variety is valid for wic tender
          WIC.TDR2        ! alternate tender/variety valid for wic tender

STRING    GLOBAL                   \
          WIC.OVR.NONWIC.ITEM$,    \ blank = no override allowed
                                   \ m = manager override o = operator override
                                   \ used for selling non wic item to wic cust
                                   \
          WIC.OVR.NONWIC.TDR$      ! blank = no override allowed
                                   ! m = manager override o = operator override
                                   ! used for tendering with non wic tender
!End @tal08182003

INTEGER*2 GLOBAL                          \
          SCANNED.PRICE,                  \ needed for itemJustAdded
          ts.money.symbol,                \ disp formatting at present 3458 IR51405
          UK.SCALES.OPTION,               \ set if to.scale.type > 1
          UK.SCANSCALE.OPTION,            \ set if to.scale.type = 4
          UK.ALLSCALES.OPTION,            \ set if to.scale.type = 1
          JAVA.SLOT                       ! Foreign currency slot

INTEGER*4 GLOBAL                          \
          EMSS.TR.PCCPNAMT,               \ transaction pc coupon amount
          EMSS.PD.POINTS,                 \ EM pending redeemed points
          IR.PRICE1,                      \ price field from item record
          X%,                             \
          FC.BALDUE                       ! Foreign balance due

!!***** CODE FOR UPC E STORE COUPONS ***
 INTEGER GLOBAL \
         EMSS.XLAT.LMT,          \ Size of translation array
         EMSS.XLAT.PTR           ! Pointer into translation array
 INTEGER*4 GLOBAL \              ! (Changed to I4 to handle EAN 99)
         EMSS.XLAT2(1)           ! Array of input UPC mfr numbers
 INTEGER*4 GLOBAL \
         EMSS.XLAT4(1)           ! Array of xlated UPC numbers

STRING    GLOBAL                          \
          EMSS.ZERO$,                     \
          OP.LINK.AP$

INTEGER   GLOBAL                          \
          OP.MSR.KEY                      ! Customer key for EM.

INTEGER*1                                 \
          ITEM.VOIDED,                    \ flag if item is voided
          ITEM.RETURNED,                  \ flag if item is returned
          ITEM.DEPOSIT,                   \ flag if item is Deposit
          WT.ITEM,                        \ flag if item is a weight item
          EXT.STR,                        \ flag if extension string
          WIC.IN.PROGRESS                 !

STRING                                    \
          javaDisplay,                    \
          javaLastScale,                  \ last scale message
          javaLastStatus,                 \ last status string
\AD21508 Use two arrays instead of four strings
\         JAVA.ITEM.HDR.LINE$,            \ item header print line
\         JAVA.ITEM.HDR.LINE.RAW$,        \ item header print line raw
\         JAVA.ITEM.PRT.LINE$,            \ item print line
\         JAVA.ITEM.PRT.LINE.RAW$,        \ item print line raw
\ED21508
          JAVA.DISCOUNT.HDR.LINE$,        \ item header print line
          JAVA.DISCOUNT.HDR.LINE.RAW$,    \ item header print line raw
          JAVA.DISCOUNT.PRT.LINE$,        \ item print line
          JAVA.DISCOUNT.PRT.LINE.RAW$,    \ item print line raw
          RESTRICTED.TIME.DATA$,          \ data to hold the restrictive times
          true,                           \
          false

!AD21508 Use two GLOBAL arrays instead of four local strings
STRING    GLOBAL                          \
          JAVA.ITEM.PRT.LINE$(1),         \ item header or print line
          JAVA.ITEM.PRT.LINE.RAW$(1)      ! item header or print line raw
!ED21508 Use two arrays instead of four strings

integer*4                                 \
          javaLastSubstate                ! @tal05272003 - save last substate

!A3458 AIR51702 Require a scale zero before any weighed items can be sold
integer*1 global                          \
          warn.scale.nonzero              ! zero has not occurred yet
!E3458 EIR51702

  integer*2 \
    NUM.PRT.LINES,           \ saved number of print lines         D21508
    NUM.TENDER.HDR.LINES,    \ saved number of tender header lines D21508
    PRT.LINE.COUNTER,        \ loop counter for print/tender lines D21508
    arraySize%, \
    ioSess%, \
    jvmErrorCount%, \
    aefPipeNum,     \
\   IR.ITEMTYPE,    \ IO08970
    JAVA.MANF.WORK, \ Work variable for manufacturer number during price verify IO08970
    SAVE.TRANSNUM,  \ Transaction number from last started transaction D2534
    EF.ENABLE1                            ! eft personality flags

  string        \
    HEADER.STR$,\ Buffer for header print lines sent before transaction start IO08970
    XMLFile$,   \
    errorCode$, \
\   data$,      \      IO11817 made this global
    tranTypeList$, \
    eventString$,  \
    aefGuiClass$,  \
    aefGuiMethod$

!AIO11817 made or added these variables GLOBAL for serviceability
  string GLOBAL            \
    SI.javaaefr.trace$,    \ new trace added here
    SI.data$               ! previously was a local stack variable 'data$'

  INTEGER*2 GLOBAL         \
    SI.javaaefr.trace.max  ! max length of trace string
!EIO11817

  integer*4      \
    errorNum%

  integer*1 \
    HEADER.LINE,        \ Indicates a header print line IO08970
    NONSALES.LINE,      \ Flags a printline as nonsales type IO08970
    exemptTransDisc,    \
    nonExemptTransDisc, \
    isVoided,           \
    transDiscAllowed,   \
    itemVoidAllowed,    \
    transStarted        ! flags to allow printline data to be sent when IO12312
                        ! transaction started                           IO12312

  string \
    aefXML$(1)                  ! XML string from file


!AIRJGUI
INTEGER*1 GLOBAL JAVA.REDIRECT
!EIRJGUI

!AIO18536
INTEGER*2 GLOBAL  SCALE.NOT.AVAILABLE            ! 0=available ( default ); -1=scale not available; 2=retry availability
!EIO18536

!******************************************************************************
!Base Electronic Marketing variable declarations

STRING GLOBAL                             \
       EMSS.POINTS$                       ! points for point only items

INTEGER*1 GLOBAL                          \
          EMSS.EM.INSTALLED,              \ flag if EM is installed
                                          \ (added through apar IR50206)
          OP.NO.EM                        ! flag is set if EM not enabled

!******************************************************************************


STRING GLOBAL                             \
       MANUFACTURER.NUMBER$

!AIO11189 Unique save-restore variables must be used around javaReadScale
!         to prevent destruction by ON ERROR processing.
  STRING       scale.javaWork1            ! used in conjunction with
  INTEGER*2    scale.java1i2,             \ calls to
               scale.java2i2              ! javaReadScale
!EIO11189

%INCLUDE EAMXXCPY.J86                     ! COPYRIGHT

!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
! Functions
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴

%INCLUDE JAVAGUIC.J86                     ! java externals
%INCLUDE JAVAGUIU.J86                     ! User exit externals
%INCLUDE EAMASMCT.J86                     ! assembler functions

!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
! Function JavaHealth
!
!   return data:   -1 If the java pipe is detected indicating java health.
!
!
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
FUNCTION javaHealth external

  INTEGER*2                                              \
            javaHealth
END FUNCTION

!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
!
! sub-routine javaReadScale
!
! Modelled after TSHIEC05
!
! Read the scale & store the weight in ts.temp1i4
!
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
SUB javaReadScale EXTERNAL
END SUB

Sub TraceRh(dato$)
string dato$
on error goto orroroso
open "R::C:TRCAEF" as 103 append
  write #103;date$,"|",time$,"|",dato$
close 103
exit sub
orroroso:
write #34;"orroso errf%",errf%," err",err,chr$(13)

end sub



!*******************************************************************************
!
! function ?StripMod3$
!
! parms: print buffer to strip control characters from
! Return Value: string w/o control characters
!
!*******************************************************************************
function STRIP.MOD3$(printString) external
  string        STRIP.MOD3$             ! returned string
  string        printString             ! Passed string to convert

  integer*2     length,  \              ! length of string
                escPos                  ! Escape Position
  string        escChar


end function                            ! END ?StripMod3$

!AIO08970
SUB CHECK.FOR.WIC.ITEM EXTERNAL     ! Wic processing to test if item is a wic item
END SUB
!EIO08970

!Begin @tal10062003
!*******************************************************************************
! Function to return XML string for specified index.
!   Use a function instead of just accessing array element in case
!   the specified index is larger than the current array size or
!   if the input file was not there for some reason.
!*******************************************************************************
function ?aefXML$(index%) public recursive
  string        ?aefXML$
  integer*2     index%

  on error goto handle.error
  ?aefXML$ = aefXML$(index%)

end.of.function:
  exit function
handle.error:
  resume end.of.function

end function
!End @tal10062003

!*******************************************************************************
function ?StripLeading$(char$,data$) recursive
  string        ?StripLeading$
  string        char$, \
                data$

  integer*2     ii,jj, aefm%

  jj =  len(data$)
  aefm% = 0
  for ii = 1 to jj
    if mid$(data$,ii,1) = char$ then \
      aefm% = ii else ii = jj
  next ii
  ?StripLeading$ = mid$(data$,aefm%+1,jj)
end function

!*******************************************************************************
function ?StripTrailing$(char$,data$) recursive
  string        ?StripTrailing$
  string        char$, \
                data$

  integer*2     ii,jj, aefm%

  jj =  len(data$)
  aefm% = jj+1
  for ii = jj to 1 step -1
    if mid$(data$,ii,1) = char$ then \
      aefm% = ii else ii = 1
  next ii
  if aefm% = 1 then ?StripTrailing$ = "" else \
    ?StripTrailing$ = left$(data$,aefm%-1)
end function

!*******************************************************************************
!*******************************************************************************
function ?RemoveCharacter$(char$,line$) recursive
  string        ?RemoveCharacter$, \
                char$, \
                line$

  integer*2     aefm%, aefs%, aefl%
  integer*1     loop

  loop = (-01)
  aefs% = 1
  while loop
    aefm% = match(char$, line$, aefs%)
    if aefm% = 0 then \
    begin
      loop = (00H) \
    endif \
    else begin
      aefl% = len(line$)
      if aefm% = 1 then \
        line$ = mid$(line$,2,aefl%) \
      else if aefm% = aefl% then \
        line$ = mid$(line$,1,aefl%-1) \
      else \
        line$ = mid$(line$,1,aefm%-1) + mid$(line$,aefm%+1,aefl%)
    endif

  wend
  ?RemoveCharacter$ = line$                  ! return string

fend                                         ! END ?RemoveCharacter$

!*******************************************************************************
function ?NoSurroundSpace$(indata$) recursive
  string        ?NoSurroundSpace$
  string        indata$

  ?NoSurroundSpace$ = ?StripTrailing$(" ",?StripLeading$(" ",indata$))

end function

!*******************************************************************************
function ?InQuote$(indata$) recursive
  string      ?InQuote$
  string      indata$

  ?InQuote$ = """" + indata$ + """"

end function

!A6540
!********************** HEXI1$ ************************************
! This is a function to convert an integer to its hexadecimal form*
! HEX.IN  contains the integer to convert to hex.                 *
! HEXI$ contains the hexadecimal representation of the string     *
!******************************************************************
 FUNCTION HEXI1$(HX) RECURSIVE
   STRING    HEXI1$,                             \ returned string
             ERRFX$                              ! work variable

   INTEGER*2 S1, S2                              ! work variables

   INTEGER*1 HX                                  ! input

   ERRFX$ = ""                                   ! set initial state

   FOR S1 = 1 TO 2                               ! loop thru input
     S2 = HX AND 00FH                            ! get one nibble
     IF (HX AND 00FH) <= 9 THEN                  \
       ERRFX$ = CHR$(48 + S2) + ERRFX$           \ 0..9: '0' + S2
     ELSE                                        \
       ERRFX$ = CHR$(55 + S2) + ERRFX$           ! A..F: 'A' + S2 - 10
     HX = HX / 16                                ! shift to next nibble
   NEXT S1                                       ! next nibble
   HEXI1$ = ERRFX$                               ! move to return string

 END FUNCTION                                    ! hexi1$
!E6540

\/******************************************************************/
\/* FUNCTION: ?FormatForXML$                                       */
\/*                                                                */
\/*  DESCRIPTION: Expands any characters in the range of 0 to 31   */
\/*               ascii.  These characters are not valid XML       */
\/*               characters.  Each character in that range will   */
\/*               be expanded to [\xnn], where nn is the hex       */
\/*               representation of that character.  (Prior to     */
\/*               6540, representation was ascii.)                 */
\/*               Also performs the following conversions:         */
\/*                  & to &amp;                                    */
\/*                  ' to &apos;                                   */
\/*                  > to &gt;                                     */
\/*                  < to &lt;                                     */
\/*                  " to &quot;                                   */
\/*                                                                */
\/******************************************************************/
FUNCTION ?FormatForXML$ (IN$) public recursive
  STRING        ?FormatForXML$
  STRING        IN$

  INTEGER*2     MATCHPOS, \
                AEFI%

  FOR AEFI% = 0 TO 31                              ! for each of the characters

    MATCHPOS = MATCH(CHR$(AEFI%), IN$, 1)
    WHILE (MATCHPOS > 0)
!A6540
\     IN$ = LEFT$(IN$, MATCHPOS-1) + "[\" + STR$(AEFI%) + "]" + \
      IN$ = LEFT$(IN$, MATCHPOS-1) + "[\x" + HEXI1$(AEFI%) + "]" + \
\E6540
            RIGHT$(IN$, LEN(IN$) - MATCHPOS)
      MATCHPOS = MATCH(CHR$(AEFI%), IN$, 1)
    WEND
  NEXT AEFI%                                       ! for each of the characters

  !
  ! Expand the & characters
  !
  MATCHPOS = MATCH("&", IN$, 1)
  WHILE (MATCHPOS > 0)
    IN$ = LEFT$(IN$, MATCHPOS-1) + "&amp;" + RIGHT$(IN$, LEN(IN$) - MATCHPOS)
    MATCHPOS = MATCH("&", IN$, MATCHPOS + 4)
  WEND

  !
  ! Expand the 'characters
  !
  MATCHPOS = MATCH("'", IN$, 1)
  WHILE (MATCHPOS > 0)
    IN$ = LEFT$(IN$, MATCHPOS-1) + "&apos;" + RIGHT$(IN$, LEN(IN$) - MATCHPOS)
    MATCHPOS = MATCH("'", IN$, MATCHPOS + 4)
  WEND

  !
  ! Expand the > characters
  !
  MATCHPOS = MATCH(">", IN$, 1)
  WHILE (MATCHPOS > 0)
    IN$ = LEFT$(IN$, MATCHPOS-1) + "&gt;" + RIGHT$(IN$, LEN(IN$) - MATCHPOS)
    MATCHPOS = MATCH(">", IN$, MATCHPOS + 4)
  WEND

  !
  ! Expand the < characters
  !
  MATCHPOS = MATCH("<", IN$, 1)
  WHILE (MATCHPOS > 0)
    IN$ = LEFT$(IN$, MATCHPOS-1) + "&lt;" + RIGHT$(IN$, LEN(IN$) - MATCHPOS)
    MATCHPOS = MATCH("<", IN$, MATCHPOS + 4)
  WEND

  !
  ! Expand the " characters
  !
  MATCHPOS = MATCH("""", IN$, 1)
  WHILE (MATCHPOS > 0)
    IN$ = LEFT$(IN$, MATCHPOS-1) + "&quot;" + RIGHT$(IN$, LEN(IN$) - MATCHPOS)
    MATCHPOS = MATCH("""", IN$, MATCHPOS + 4)
  WEND

  ?FormatForXML$ = IN$

END FUNCTION

\/******************************************************************/
\/* FUNCTION: ?StripControlChar$                                   */
\/*                                                                */
\/*  DESCRIPTION: Strips any non-printable characters from the     */
\/*               string.                                          */
\/*                                                                */
\/******************************************************************/
FUNCTION ?StripControlChar$ (IN$) public recursive
  STRING ?StripControlChar$
  STRING IN$

  INTEGER*2 MATCHPOS, \
            AEFI%

  FOR AEFI% = 0 TO 31                              ! for each of the characters
    MATCHPOS = MATCH(CHR$(AEFI%), IN$, 1)
    WHILE (MATCHPOS > 0)
      IN$ = LEFT$(IN$, MATCHPOS-1) + RIGHT$(IN$, LEN(IN$) - MATCHPOS)
      MATCHPOS = MATCH(CHR$(AEFI%), IN$, 1)
    WEND
  NEXT AEFI%

  ?StripControlChar$ = IN$

END FUNCTION


!*******************************************************************************
!*******************************************************************************
sub RemoveConditionalTags(inString$) recursive
  string        inString$

  string        w$
  integer*2     aefs%, aefe%

  aefs% = match("[",inString$,1)                   ! look for start delimiter [
  while aefs%
    aefe% = match("]",inString$,aefs%+1)              ! look for end delimiter ]
    if aefe% then \
    begin
      if aefs% > 1 then \                          ! remove extra lead space if any
      begin
        if mid$(inString$,aefs%-1,1) = " " then \
        begin
          aefs% = aefs%-1
        endif
      endif
      w$ = "~"+inString$+"~"                    ! add filler
      w$ = left$(w$,aefs%)+mid$(w$,aefe%+2,len(w$))
      inString$ = mid$(w$,2,len(w$)-2)          ! update string
      aefs% = match("[",inString$,aefs%)              ! look for next start
    endif \
    else begin                                  ! no ] - done
      aefs% = 0
    endif
  wend

end sub

!*******************************************************************************
! Conditional Tag: [(tag%)text=&#]
! Output is text=&#
!*******************************************************************************
sub AllowConditionalTag(tag%, inString$) recursive
  string        inString$
  integer*2     tag%

  integer*1     keepLooking
  integer*2     aefs%, aefe%
  string        w$, s$

  !Begin @tal10032003 - updated to match EAL version.
  keepLooking = (-1)
  s$ = "[("+str$(tag%)+")"                      ! will look for [(tag%)
  while keepLooking
    aefs% = match(s$,inString$,1)                  ! find start delimiter
    if aefs% then \                                ! if found
    begin
      aefe% = match(")",inString$,aefs%+1)            ! look for end of conditional tag#
      w$ = "~"+inString$+"~"                    ! add filler
      w$ = left$(w$,aefs%)+mid$(w$,aefe%+2,len(w$))
      inString$ = mid$(w$,2,len(w$)-2)          ! update string
      aefs% = match("]",inString$,aefs%)              ! find end delimiter
      if aefs% then \
      begin
        w$ = "~"+inString$+"~"                  ! add filler
        w$ = left$(w$,aefs%)+mid$(w$,aefs%+2,len(w$))
        inString$ = mid$(w$,2,len(w$)-2)        ! update string
      endif
    endif \
    else keepLooking = (00)
  wend

end sub

!*******************************************************************************
!*******************************************************************************
sub SubstituteMarker(marker$, inString$, with$) recursive
  string        marker$, inString$, with$

  integer*2     aefm%, aefl%, aefw%
  string        w$

  aefm% = match(marker$, inString$,1)
  if aefm% then \
  begin
    w$ = "[" + inString$ + "]"          ! avoid mid$ problem
    aefw% = len(w$)
    aefl% = len(marker$)
    w$ = left$(w$,aefm%) + with$ + \
         mid$(w$,aefm%+aefl%+1,len(w$))
    inString$ = mid$(w$,2,len(w$)-2)    ! drop extraneous characters
  endif
end sub

!*******************************************************************************
!*******************************************************************************
function ?FindMarker$(inString$) recursive
  string        ?FindMarker$
  string        inString$

  integer*2     aefm%, aefl%, aefi%
  string        w$

  w$ = ""
  aefm% = match("&#",inString$,1)          ! marker = &##... where #=number
  if aefm% then \
  begin
    aefl% = len(inString$)
    for aefi% = aefm%+1 to aefl%
      w$ = mid$(inString$,aefi%,1)
      if w$ >= "0" and w$ <= "9" then \
      begin
        if aefi% = aefl% then \               ! should not happen, but...
        begin
          w$ = mid$(inString$,aefm%,aefl%)    ! get marker
        endif
      endif \
      else begin
        w$ = mid$(inString$,aefm%,aefi%-aefm%)   ! get marker
        aefi% = aefl%                         ! set to exit
      endif
    next aefi%
  endif
  if len(w$) = 0 or len(w$) > 5 then \  ! impose limit 0-9999
    w$ = ""

  ?FindMarker$ = w$

end function


!A6540
\/******************************************************************/
\/* FUNCTION: ContextSensitivePrintLines                           */
\/*                                                                */
\/*  DESCRIPTION: Substitutes the lineType, lineCategory and font  */
\/*               attributes.                                      */
\/*                                                                */
\/*  lineType is usually set based upon TS.LINETYPE, except:       */
\/*    if TS.LINETYPE is 1 and TS.LINEDATA is 0 or 1, and          */
\/*       TS.XXMOD is 2 through 8,                                 */
\/*         then select member 100 + TS.XXMOD                      */
\/*    if TS.LINETYPE is 10 and TS.LINEDATA is 2,                  */
\/*      then select member 120 + TS.LINEDATA                      */
\/*    if TS.LINETYPE is 18 and TS.LINEDATA is 0,                  */
\/*      then select member 130 + TS.LINEDATA                      */
\/*    if TS.LINETYPE is 29 and TS.LINEDATA is -3 through +1,      */
\/*      then select member 143 + TS.LINEDATA                      */
\/*                                                                */
\/*  lineCategory is usually set based upon TS.LINEDATA, except    */
\/*  for most cases similar to the exceptions for lineType, above. */
\/*                                                                */
\/*  SA TS.LINETYPE definitions, from EAMTSPRC.LCV:                */
\/*  1: ITEM ENTRY PRINT LINE                                      */
\/*  2: TENDERS                                                    */
\/*  3: DISCOUNTS                                                  */
\/*  4: TAX EXEMP/CHANGE/CHECK FEE/LOAN PICKUP SUMMARY TOTAL       */
\/*  5: ITEM PRICING(SPLIT/WEIGHT/QTY ON SEP LINE)/LOAN PICKUP QTY */
\/*  6: SPECIAL SIGN ON/STORE DATA PRINT LINE/TOTAL # OF ITEMS SOLD*/
\/*  7: BALANCE DUE/BALANCE DUE FOOD STAMPS                        */
\/*  8: TTAF AT SIGN ON/AT EOT                                     */
\/*  9: TENDER PRINTING/TENDER FRANKING                            */
\/*  10: TRANSACTION DATA ENTRY                                    */
\/*  11: NON-SALE/NO-SALE HEADER                                   */
\/*  12: NON-SALE TRAILER/SPECIAL SIGN OFF                         */
\/*  13: STANDALONE ENTRY/EXIT                                     */
\/*  14: ACCOUNT NUMBER                                            */
\/*  15: ITEM LIMIT, DISCOUNT, TENDER, CHANGE, FEE ERROR           */
\/*  16: COUPONS TENDERED/TAX PAID (AT EOT)                        */
\/*  17: STAMPS ISSUED (AT EOT)                                    */
\/*  18: PERSONALISED HEADER/TRAILER                               */
\/*  19: HELP MESSAGES BETWEEN/INSIDE TRANSACTIONS                 */
\/*  20: TENDER LISTING BATCH PROMPT                               */
\/*  21: TENDER LISTING REPORT                                     */
\/*  22: VOID TRANSACTION TRAILER                                  */
\/*  23: SIGNED OFF MESSAGE                                        */
\/*  24: # OF PRICES CHANGED (BATCH)                               */
\/*  25: TRAINING EXIT LINES                                       */
\/*  26: TERMINAL TRANSFER DATA (FROM AND TO)                      */
\/*  27: MONITORED TERMINAL ADDRESS                                */
\/*  28: PRICE VERIFY/CHANGE (ADDITIONAL DATA)                     */
\/*  29: PRINT BUFFER AS PASSED (TERMINAL MONITOR)                 */
\/*  30: PRINT FEATURE DESCRIPTOR                                  */
\/*  31: PRINT OPTION TAILORED FOR SUSPEND/RETRIEVE                */
\/*                                                                */
\/******************************************************************/
sub ContextSensitivePrintLines(eventString$) recursive
  string        eventString$
  string        marker$,         \
                with$,           \
                LineTypeString$, \
                LineCatString$,  \
                ESCpattern$,     \
                font$,           \ Font name IO08970
                aefs1$

  integer*2    \aeflt%,          \ IO08970 Remove and make global
               \aeflc%,          \ IO08970 Remove and make global
                aefi1%, aefi2%,  \ scratch
                aefRawPos%,      \ position in rawData line
                aeffont%,        \ the font to use
                aefctp%,         \ the # of columns to print D2424/17914
                I2A%,            \ Work variable for line feed calculation IO08970
                I2B%,            \ Work variable for line feed calculation IO08970
                aefESCfound%     ! was the ESC sequence found?

  LineTypeString$ = ?aefXML$(145) ! LineType substitutions
  LineCatString$ = ?aefXML$(146)  ! LineCategory substitutions
  font$ = ?aefXML$(148)           ! font substitutions IO08970
  NONSALES.LINE = 0               ! IO08970

!AIO08970 Establish the lineType value based upon TS.LINETYPE, unless
!         JAVA.LINETYPE is non-zero which overrides the TS.LINETYPE value.
  IF (JAVA.LINETYPE <> 0) THEN BEGIN
    aeflt% = JAVA.LINETYPE            ! 0 - 31
    aeflc% = JAVA.LINETYPE            ! 0 - ?
  ENDIF ELSE BEGIN
    ! Establish the lineType value based upon TS.LINETYPE
    IF TS.LINETYPE <> 0 THEN BEGIN
!EIO08970
      aeflt% = TS.LINETYPE            ! 0 - 31
      aeflc% = TS.LINETYPE            ! 0 - ?
!AIO08970
    ENDIF
  ENDIF
  IF (JAVA.LINECATEGORY <> 0) THEN BEGIN
    aeflc% = JAVA.LINECATEGORY
  ENDIF
!EIO08970

!AIO08970 Check for linetype override
! if TS.LINETYPE = 1 then begin ! ITEM ENTRY PRINT LINE
  if (TS.LINETYPE = 1) AND            \ ITEM ENTRY PRINT LINE
     (JAVA.LINETYPE = 0) then begin   !
!EIO08970
    if (TS.LINEDATA >= 0) and (TS.LINEDATA <= 1) then begin
      if (TS.XXMOD >= 1) and (TS.XXMOD <= 8) then begin
        aeflt% = 100 + TS.XXMOD   ! 101 - 108
        if (IR.INDICAT1A AND 20H) then begin
          aeflt% = 109            ! 109 points only item
        endif
      endif
    endif

  endif

!AD18204 Check for a voided item pricing line
  IF (TS.LINETYPE = 5) AND                        \ ITEM PRICING LINE
     (JAVA.LINETYPE = 0) AND                      \ AND NO JAVA OVERRIDE
     ((SL.IT.INDICAT2 AND 0080H) <> 0) THEN BEGIN ! THIS IS A VOID
     aeflt% = 108                                 ! ItemCancel type
  ENDIF
!ED18204

!AIO08970 Check for linetype override
! if TS.LINETYPE = 4 then begin   ! TAX EXEMP/CHANGE/CHECK FEE/LOAN PICKUP SUMMARY TOTAL
  if (TS.LINETYPE = 4) AND          \ TAX EXEMPT/CHANGE/CHECK FEE/LOAN
     (JAVA.LINETYPE = 0) then begin ! PICKUP SlUMMARY TOTAL
!EIO08970
    if (TS.LINEDATA >= 1) and (TS.LINEDATA <= 5) then begin
      aeflt% = 210 + TS.LINEDATA  ! 211 - 215
      aeflc% = 210 + TS.LINEDATA  ! 211 - 215
    endif
  endif

  if aeflt% = 6 then begin
    if (TS.LINEDATA = 2) then begin
      aeflt% = 110 + TS.LINEDATA  ! 112
!AD2424 Defect 18316 - the line category for TotalItemCount must be 'Trailer'
!     aeflc% = 120 + TS.LINEDATA  ! 122 - 122
!ED2424
    endif
  endif

!AIO08970 Check for linetype override
! if TS.LINETYPE = 10 then begin  ! TRANSACTION DATA ENTRY
  if (TS.LINETYPE = 10) AND       \ TRANSACTION DATA ENTRY
     (JAVA.LINETYPE = 0) then begin
!EIO08970
    if (TS.LINEDATA = 2) then begin
      aeflt% = 120 + TS.LINEDATA  ! 122 - 122
      aeflc% = 120 + TS.LINEDATA  ! 122 - 122
!AIO08970
!AIO12312 Buffer print line only if not in transaction AND start transaction not sent
      if (TS.INTRX = 0) AND       \
         (transStarted = 0) then begin
!EIO12312
        HEADER.LINE = 1
      endif
!EIO08970
    endif
  endif

!AIO08970 Check for linetype override
! if TS.LINETYPE = 18 then begin    ! PERSONALISED HEADER/TRAILER
  if (TS.LINETYPE = 18) AND         \ PERSONALIZED HEADER/TRAILER
     (JAVA.LINETYPE = 0) then begin    !
!EIO08970
    if (TS.LINEDATA = 0) then begin    ! Trailer
      aeflt% = 130
      aeflc% = 130
!AIO08970
!    endif
     endif else begin
       HEADER.LINE = 1
     endif
!EIO08970
  endif

!AIO08970 Check for linetype override
! if TS.LINETYPE = 29 then begin  ! PRINT BUFFER AS PASSED (TERMINAL MONITOR)
  if (TS.LINETYPE = 29) AND       \ PRINT BUFFER AS PASSED
     (JAVA.LINETYPE = 0) then begin ! (TERMINAL MONITOR)
!EIO08970
    if (TS.LINEDATA >= -3) and    \
       (TS.LINEDATA <= 1) then begin
      aeflt% = 143 + TS.LINEDATA  ! 140 - 144
      aeflc% = 143 + TS.LINEDATA  ! 140 - 144
    endif
!AIO08970
!AIO12312 Buffer print line only if not in transaction AND start transaction not sent
    if (TS.INTRX = 0) AND          \
       (transStarted = 0) then begin
!EIO12312
      HEADER.LINE = 1
    endif
!EIO08970
  endif

!AIO08970
  IF JAVA.PRICE.VERIFY = -1 THEN BEGIN    ! Price Verify
    aeflt% = 145 ! NonSales
    aeflc% = 145 ! NonSales
  ENDIF
!EIO08970

!AD17772 If a Suspend Transaction is in progress, convert NonSales lines
  IF (TS.SUSP.IN.PROGRESS) OR         \ suspension in progress
     (TS.RETV.IN.PROGRESS) THEN BEGIN ! retrieve in progress
    IF aeflt% = 31 THEN BEGIN         ! PRINT OPTION TAILORED FOR SUSPEND/RETRIEVE
      IF (TS.SUSP.IN.PROGRESS) THEN BEGIN ! Assign suspend lineType and lineCategory  AIO21731    
        aeflt% = 7                      ! Convert to TransactionTotal
        aeflc% = 1                      ! Convert to LineItem
!AIO21731    
      ENDIF ELSE BEGIN                  ! Assign retrieve lineType and lineCategory
        aeflt% = 220                    ! Convert to RequestHeader
        aeflc% = 221                    ! Convert to Header
      ENDIF      
!EIO21731    
    ENDIF
  ENDIF
!ED17772

  CALL AllowConditionalTag(aeflt%,LineTypeString$) ! lineType="?????????"
  CALL AllowConditionalTag(aeflc%,LineCatString$) ! lineCategory="?????????"

  ! discard all of the conditions that are not specifically kept by the code above
  CALL RemoveConditionalTags(LineTypeString$)
  CALL RemoveConditionalTags(LineCatString$)

  ! a special case is a null name or "*".  These are assigned "NonSales"
  if (LineTypeString$="*") or     \ the default case
     (LineTypeString$="") then    \ not found
    LineTypeString$="NonSales"
  if (lineCatString$="*") or      \ the default case
     (lineCatString$="") then     \ not found
    lineCatString$="NonSales"

!AIO08970
  if (LineTypeString$="NonSales") then begin
    NONSALES.LINE = -1
  endif
!EIO08970

  marker$ = ?FindMarker$(eventString$)      ! Find the marker for the lineType attribute
  IF LEN(marker$) > 0 THEN BEGIN
     with$ = ?InQuote$(LineTypeString$)     ! put something here for test
     CALL SubstituteMarker(marker$, eventString$, with$)  ! Insert the rawData
  ENDIF

  marker$ = ?FindMarker$(eventString$)      ! Find the marker for the lineCategory attribute
  IF LEN(marker$) > 0 THEN BEGIN
     with$ = ?InQuote$(LineCatString$)     ! put something here for test
     CALL SubstituteMarker(marker$, eventString$, with$)  ! Insert the rawData
  ENDIF

  ! The font attribute is determined by looking for an ESC string at the
  ! start of the rawData.  If found, parse string-147 (IPGM:EAMAEFXF.DAT)
  ! for all recognized ESC sequences, and determine for each sequence if
  ! it is in rawData at the byte position currently being examined.  If any
  ! recognized ESC sequence is found there, save the font number from string-
  ! 147.  If another ESC sequence immediately follows in rawData, repeat the
  ! parse/search/save process from that point.
  ! Font numbers supposed to end up as 0..3 for regular, and 4..7 for bold.
  marker$ = ?FindMarker$(eventString$)      ! Find the marker for the font attribute
  aefctp% = 44                 ! the # of columns to print D2424/17914
  IF LEN(marker$) > 0 THEN BEGIN
     aeffont% = 0              ! the font to use (default 0)
     aefRawPos% = 2            ! start at beginning of rawdata, after double quote
     aefESCfound% = -1         ! go through 'while' at least once
     while (len(printLine.rawData$) > aefRawPos%) and \ line is not null and contains an
        (mid$(printLine.rawData$,aefRawPos%,6) = "[\x1B]") and \ ESC
        (aefESCfound% <> 0)    ! until no ESC sequences are recognized
       ! raw data for this print line contains an ESC sequence.
       ! compare against established ESC sequences to determine the font number
       aefs1$ = ?aefXML$(147)  ! get the ESC matching strings
       aefi1% = 1              ! start at first byte of string-147
       aefESCfound% = 0        ! ESC not found yet in this rawData pass
       while aefi1% < len(aefs1$) ! until all string-147 sequences are checked
         aefi2% = MATCH(",",aefs1$,aefi1%) ! end of the current ESC sequence
         if aefi2% < (aefi1%+1) then \ no comma found, or consecutive commas
           aefi1% = 9999       \ break out of while loop (bad data format)
         else begin            ! compare against this string-147 ESC sequence
           ESCpattern$ = MID$(aefs1$,aefi1%,aefi2%-aefi1%) ! expected ESC seq
           if mid$(printLine.rawData$,aefRawPos%,aefi2%-aefi1%) = ESCpattern$ \
           then begin          ! found an ESC sequence, add in its value
             aeffont% = aeffont% or VAL(MID$(aefs1$,aefi2%+1,1)) ! font value
             aefRawPos% = aefRawPos% + aefi2%-aefi1% ! skip to next possible ESC
             aefi2% = 9999     ! break out of inner while loop
             aefESCfound% = -1 ! cause the outer 'while' to repeat
           endif
           aefi1% = aefi2% + 3 ! skip over the comma,fontnumber,comma
         endif
       wend                    ! inner while loop
     wend                      ! outer while loop


!AIO08970
!    with$ = ?InQuote$(str$(aeffont%)) ! the font to use
     CALL AllowConditionalTag(aeffont% + 1,font$)
     ! discard all of the conditions that are not specifically kept by the code above
     CALL RemoveConditionalTags(font$)
     with$ = ?InQuote$(font$)
!EIO08970
     CALL SubstituteMarker(marker$, eventString$, with$)  ! Insert the font

!AIO08970 centered
     marker$ = ?FindMarker$(eventString$)      ! Find the marker for centered
     IF LEN(marker$) > 0 THEN BEGIN
       with$ = "false"
       IF (MATCH("[\x1B]a[\x01]", eventString$, 1) > 0) THEN BEGIN
         with$ = "true"
       ENDIF
       with$ = ?InQuote$(with$)
       CALL SubstituteMarker(marker$, eventString$, with$)  ! Insert into XML
     ENDIF
!EIO08970

!AD2424/17914 Columns to print
     aeffont% = aeffont% AND 3 ! clear the 'bold' bit 04H
     IF aeffont% = 1 THEN BEGIN
       aefctp% = 30            ! 30 columns with font 1
     ENDIF ELSE                \
     IF (aeffont% = 2) OR (aeffont% = 3) THEN BEGIN
       aefctp% = 19            ! 19 columns with font 2 or 3
     ENDIF                     ! else print 38 columns
!ED2424/17914
  ENDIF

!AD2424/17914 Columns to print
  marker$ = ?FindMarker$(eventString$)      ! Find the marker for columnsToPrint
  IF LEN(marker$) > 0 THEN BEGIN
     with$ = ?InQuote$(STR$(aefctp%))       ! columns to print
     CALL SubstituteMarker(marker$, eventString$, with$)  ! Insert into XML
     !write #34;"evenString$",eventString$,chr$(10)
     !write #34;"marker$",marker$,chr$(10)
  ENDIF
!ED2424/17914

!AIO08970 Process linefeeds
  marker$ = ?FindMarker$(eventString$)      ! Find the marker for linefeeds
  IF LEN(marker$) > 0 THEN BEGIN
    with$ = ?InQuote$("")
    GOSUB lineFeedsSub
    CALL SubstituteMarker(marker$, eventString$, with$)  ! Insert into XML
  ENDIF

  EXIT SUB

lineFeedsSub:
  ! Set up the linefeeds.  Even if this is a 4610, variable TS.FORMCR$ is still
  ! set up for linefeeds
  I2A% = 1                                       ! assume 1 linefeed
  I2B% = MATCHB("A", TS.FORMCR$, 1)              ! Locate char
  IF (I2B% > 0) THEN BEGIN                       ! Char found
    I2A% = VAL(RIGHT$(TS.FORMCR$,LEN(TS.FORMCR$) - I2B%))
    IF (I2A% < 1) THEN BEGIN                     ! if invalid data
      I2A% = 1                                   ! adjust to one
    ENDIF                                        ! if invalid data
  ENDIF                                          ! Char found

  with$ = ?InQuote$(STR$(I2A%))
  RETURN
!EIO08970
end sub
!E6540

!*******************************************************************************
!*******************************************************************************
function ?GetXMLEvent$(inString$) recursive
  string        ?GetXMLEvent$
  string        inString$

  integer*2     aefm%, aefs%, aefl%, aefi%

  if left$(inString$,1) = "<" then \            ! valid XML string?
  begin
    aefl% = len(inString$)
    for aefi% = 2 to aefl%                            ! skip leading spaces
      aefs% = aefi%
      if mid$(inString$,aefi%,1) <> " " then \     ! if non space
      begin
        aefi% = aefl%                                 ! set to exit
      endif
    next aefi%
    aefm% = match(" ",inString$,aefs%)                ! Look for next space.
    if aefm% = 0 then \                            ! If not space terminated, look for
       aefm% = match(">",inString$,aefs%)             ! closing bracket.
    if aefm% then \
    begin
      ?GetXMLEvent$ = mid$(inString$,aefs%,aefm%-aefs%)  ! get event tag
    endif
  endif

end function


!*******************************************************************************
!Function to return true or false on the flag passed in
!*******************************************************************************
FUNCTION ?TrueOrFalse$(flag)
  string        ?TrueOrFalse$
  integer*4     flag

  IF (flag) THEN \
  BEGIN
    ?TrueOrFalse$ = """true"""
  ENDIF \
  ELSE BEGIN
    ?TrueOrFalse$ = """false"""
  ENDIF

END FUNCTION

! AIO05031
FUNCTION ?TrueOrmanagerOverride$(flag)
  string        ?TrueOrmanagerOverride$
  integer*4     flag

  IF (flag) THEN \
  BEGIN
    ?TrueOrmanagerOverride$ = """true"""
  ENDIF \
  ELSE BEGIN
    ?TrueOrmanagerOverride$ = """managerOverride"""
  ENDIF

END FUNCTION
! EIO05031

!*******************************************************************************
sub ForceCloseIO(io%) recursive
  integer*2     io%

  on error goto handle.error
  if io% <> 0 then \
    close io%                                   ! test close
end.of.function:
  exit sub
handle.error:
  resume end.of.function
end sub                                         ! END sf.ForceCloseIO

!*****************************************************************************!
!*****************************************************************************!
function AvailSession%(aName$) recursive
  integer*2     AvailSession%
  string        aName$

  integer*2     aefs%, sess%

  on error goto HandleError
  for aefs% = 1 to 99
    errorCode$ = ""
    open aName$ as aefs% NOWRITE NODEL
    if errorCode$ = "" then \           ! no error (or file not found)
    begin
      sess% = aefs%                        ! save avail session#
      aefs% = 99                           ! set to exit
    endif
  next aefs%
  call ForceCloseIO(sess%)
  AvailSession% = sess%                 ! return
EndOfFunction:

  exit function
HandleError:
  errorCode$ = ERR
  errorNum% = ERRN
  if errorCode$ = "OE" and \            ! file not found
     errorNum% = 80204010h then \
  begin
    errorCode$ = ""                     ! indicate session avail
  endif
  resume

end function

!*****************************************************************************!
!*****************************************************************************!
function OpenThisFile(fileName$, sess%) recursive
  integer*1     OpenThisFile
  string        fileName$               ! Filename of Edit Check Pattern
  integer*2     sess%                   ! IO Session ID

  integer*1     openOK

  call ForceCloseIO(sess%)
  openOK = (-1)                         ! default open ok
  on error goto handle.error

  open fileName$ as sess% \             ! open as SEQUENTIAL
         NOWRITE NODEL
  OpenThisFile = openOK                 ! return

  exit function

  handle.error:

  openOK = (00) : resume                ! error return = failure

end function                            ! END OpenThisFile

!*****************************************************************************!
!*****************************************************************************!
sub LoadXMLElement(index$,XML$) recursive
  string        index$, XML$

  integer*2     index%

  on error goto HandleError
  index% = val(index$)
  if index% = 0 then \                  ! element 0 = size of array
  begin
    arraySize% = val(XML$)
    dim aefXML$(arraySize%)
  endif \
  else begin                            ! others
    if index% <= arraySize% then \
    begin
      ReWriteElement:
      aefXML$(index%) = XML$            ! load element
    endif
  endif

EndOfSub:
  exit sub

HandleError:
  errorCode$ = ERR
  errorNum% = ERRN
  if errorCode$ = "AE" then \           ! array not dim
  begin
    arraySize% = 100                    ! dim w/ default size
    dim aefXML$(arraySize%)
    resume ReWriteElement
  endif
  resume EndOfSub

end sub

!*****************************************************************************!
!*****************************************************************************!
sub LoadXMLArray(buff$) recursive
  string        buff$

  string        index$, XML$, w$
  integer*2     aefm%

  if match(left$(buff$,1),";\!=,",1) = 0 and \   ! skip comments
     len(buff$) <> 0 then \
  begin
    aefm% = match(",",buff$,1)
    if aefm% > 1 then \                            ! if found ","
    begin                                       ! get array index
      w$ = ucase$(left$(buff$,aefm%-1))
      w$ = ?RemoveCharacter$(" ",w$)
      index$ = w$                               ! return option tag
      w$ = mid$(buff$,aefm%+1,len(buff$))          ! get XML string
      XML$ = ?StripTrailing$(" ",w$)   
      
      !call tracerh("index$ "+index$+ " Xml$ "+xml$)
      call LoadXMLElement(index$,XML$)  
      
    endif
  endif

end sub                                 ! END GetXMLString

!*****************************************************************************!
!*****************************************************************************!
sub ReadXMLFile recursive

  integer*2     haveFile

  XMLFile$ = "R::$AMAEFXF"                      ! try local copy
  ioSess% = AvailSession%(XMLFile$)             ! find avail session#
  if ioSess% then \
  begin
    haveFile  = OpenThisFile(XMLFile$,ioSess%)  ! test open
    if not haveFile then \                      ! if not found
    begin
      XMLFile$ = "R::ADX_IPGM:EAMAEFXF.DAT"     ! try another
      haveFile  = OpenThisFile(XMLFile$,ioSess%)! test open
    endif

    if haveFile then \
    begin
      if end #ioSess% then handleEOF
      while 1
        read #ioSess%; LINE SI.data$       
        !call tracerh("lei "+SI.data$)
        call LoadXMLArray(SI.data$)
      wend
    endif else \
    begin
       arraySize% = 100                    ! do this to prevent AE error
       dim aefXML$(arraySize%)
    endif
  endif \
  else begin
    arraySize% = 100                    ! do this to prevent AE error
    dim aefXML$(arraySize%)
  endif
handleEOF:
  call ForceCloseIO(ioSess%)
end sub

!*******************************************************************************
function ?SetTransactionType$(procedure) recursive
  integer*2     procedure
  string        ?SetTransactionType$

  string        tranType$
  integer*2     aefm%, f%

  if (procedure = -1) then begin
     procedure = 0
  endif
  if (WIC.IN.PROGRESS = -1) then begin
     procedure = 14
  endif
  tranType$ = "<"+right$("00"+str$(procedure),2)+">"
  aefm% = match(tranType$,tranTypeList$,1)
  if aefm% then \                                  ! if in list
  begin
    f% = match(";",tranTypeList$,aefm%+4)
    if (f%-aefm%) > 4 then \                       ! get valid modifier
    begin
      ?SetTransactionType$ = mid$(tranTypeList$,aefm%+4,f%-aefm%-4)
    endif
  endif

end function


!*******************************************************************************
function ?TransDiscAllowed$(allowed) recursive
  integer*1     allowed
  string        ?TransDiscAllowed$

  string element$, \
         marker$,  \
         with$


  transDiscAllowed = allowed
  eventString$ = ""
  eventString$ = ?aefXML$(48)                    ! transactionUpdate
  element$ = ?GetXMLEvent$(eventString$)
  IF LEN(element$) THEN \
  BEGIN
    CALL AllowConditionalTag(1,eventString$)    ! transactionDiscountAllowed="true|false"
    CALL AllowConditionalTag(2,eventString$)    ! itemVoidAllowed="true|false"
    CALL RemoveConditionalTags(eventString$)
    marker$ = ?FindMarker$(eventString$)        ! marker = &##... where #=number
    with$ = ?TrueOrFalse$(transDiscAllowed)
    CALL SubstituteMarker(marker$, eventString$, with$)
    marker$ = ?FindMarker$(eventString$)
    with$ = ?TrueOrFalse$(itemVoidAllowed)
    CALL SubstituteMarker(marker$, eventString$, with$)
    ?TransDiscAllowed$ = eventString$
  ENDIF

end function


!*******************************************************************************
function ?ItemVoidAllowed$(allowed) recursive
  integer*1     allowed
  string        ?ItemVoidAllowed$

  string element$, \
         marker$,  \
         with$


  itemVoidAllowed = allowed
  eventString$ = ""
  eventString$ = ?aefXML$(48)                    ! transactionUpdate
  element$ = ?GetXMLEvent$(eventString$)
  IF LEN(element$) THEN \
  BEGIN
    call AllowConditionalTag(1,eventString$)    ! transactionDiscountAllowed="true|false"
    call AllowConditionalTag(2,eventString$)    ! itemVoidAllowed="true|false"
    CALL RemoveConditionalTags(eventString$)
    marker$ = ?FindMarker$(eventString$)        ! marker = &##... where #=number
    with$ = ?TrueOrFalse$(transDiscAllowed)
    CALL SubstituteMarker(marker$, eventString$, with$)
    marker$ = ?FindMarker$(eventString$)        ! marker = &##... where #=number
    with$ = ?TrueOrFalse$(itemVoidAllowed)
    CALL SubstituteMarker(marker$, eventString$, with$)
    ?ItemVoidAllowed$ = eventString$
  ENDIF

end function

!*******************************************************************************
!   function ?AEF.INIT
!
!   parameters:          none
!   return data:         none
!
!*******************************************************************************
function ?AEF.INIT public recursive

  aefGuiClass$ = \
    "com.ibm.retail.AEF.data.ApplicationDataConnectorImpl"      ! set the class
  aefGuiMethod$ = "processEvent"        ! set the method
  aefPipeNum = 99                       ! set default
  jvmErrorCount% = 0                    ! start JVM heartbeat error count
  aefUnitOfWorkFlag = (00)
!  call ?AEF.WAIT.FOR.JVM                ! wait for the JVM before calling JAVA

  arraySize% = 100                      ! default size of XML array

  JAVAAEFR.VERSION = 4                  ! SI V4 D2625

  call ReadXMLFile
  tranTypeList$ = ?aefXML$(1)+aefXML$(2)

end function                                  ! END AEF.INIT


!! Function copied from EM module eamtsfbz
!!***** CODE FOR UPC E STORE COUPONS ***
!!*****************************************************************
!!** CONVERT ALTERED UPCS BACK TO INPUT UPCS FOR LOG OR DISPLAY  **
!!*****************************************************************
FUNCTION JAVA.RESTORE.UPC(IN$)                                   !
         STRING JAVA.RESTORE.UPC,IN$                             !
                                                                 !
     IF MID$(IN$,2,1) = "=" THEN BEGIN       ! TRANSLATED UPC
       J$ = MID$(IN$,3,4) + MID$(IN$,8,5)    ! GET XLATE CODE
       TS.TEMP4I4 = VAL(J$)                  !  SAVED FOR SEARCH
       X% = 0 : K% = 0                       ! DEFAULT DATA
       WHILE X% = 0 AND K% < EMSS.XLAT.PTR   ! LOOP TILL DONE
         K% = K% + 1                         ! BUMP TO NEXT ENTRY
         IF EMSS.XLAT4(K%) = TS.TEMP4I4 THEN \ FOUND MATCH
           X% = EMSS.XLAT2(K%)               ! SAVE XLAT DATA
       WEND                                  ! CONTINUE LOOP
       IF X% = 0 THEN X% = EMSS.XLAT2(1)     ! DEFAULT TO FIRST
       J$ = RIGHT$(EMSS.ZERO$ + STR$(X%),7)  ! UPC = YYYYYYYXXXXX
     ENDIF ELSE BEGIN                        ! NOT XLATED UPC
       J$ = LEFT$(EMSS.ZERO$,7)              ! UPC = 0C00000XXXXX
       CALL SUBSTR(J$,1,IN$,1,1)             !
!AIR37474
       IF ASC(OP.LINK.AP$) > 10H AND         \ EAN APPEND CC00000XXXXX
          MID$(IN$,2,1) > "0" THEN BEGIN     ! NON-ZERO UPC CATEGORY
        IF MID$(UNPACK$(OP.LINK.AP$),2,6)    \ LINK APPEND MATCHES
           = MID$(J$,2,6) THEN               \ ON ALL BUT FIRST DIGIT
          J$ = LEFT$(UNPACK$(OP.LINK.AP$),7) ! USE LINK APPEND
       ENDIF                                 ! EAN APPEND CC00000XXXXX
!EIR37474
     ENDIF                                   ! NOT XLATED UPC
     JAVA.RESTORE.UPC = J$ + RIGHT$(IN$,5)   ! OUTPUT UPC
END FUNCTION                                 !


!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
!
! function strip.prt4610$
!
! This function strips 4610 control characters from the print line.
!
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
FUNCTION STRIP.PRT4610$(IN$) EXTERNAL            !
  STRING                                         \
         IN$,                                    \
         STRIP.PRT4610$                          !
END FUNCTION                                     !

!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
!
! function strip.mod3$
!
! This function strips MOD3 control characters from the print line.
!
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
!FUNCTION STRIP.MOD3$(IN$) EXTERNAL               !
!  STRING                                         \
!         IN$,                                    \
!         STRIP.MOD3$                             !
!END FUNCTION                                     !


!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
! tax calculation
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
SUB TSTXEC01 EXTERNAL                            ! Tax calculation
END SUB                                          ! tstxec01

!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
! Application tracing routine
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
FUNCTION EAMTRACE(TRXX) EXTERNAL                 !
  INTEGER*1 EAMTRACE                             !
  INTEGER*2 TRXX                                 !
END FUNCTION                                     !

!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
! Terminal error logging
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
FUNCTION ADX.ERROR.LOG EXTERNAL                  ! Error log routine
END FUNCTION                                     ! adx.error.log


!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
! Local routines
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴

%INCLUDE EAME2OFT.J86                            ! VALUEPACK OPTIONS ROUTINE


!A3458 AIR51405

\ REM  ****************************************************************
\ REM
\ REM   FUNCTION JAVA.FORMAT.AMOUNT
\ REM
\ REM     CALL FORMAT.DOLLARS TO CONVERT AMOUNT FIELD TO DOLLAR STRING
\ REM     MOVE DISPLAY LINE TO OUTPUT PARAMETER
\ REM
\ REM   END FUNCTION
\ REM
\ REM  ****************************************************************
  FUNCTION JAVA.FORMAT.AMOUNT ( AMT1 ) EXTERNAL        !
  INTEGER*1 JAVA.FORMAT.AMOUNT                         !
  INTEGER*4 AMT1                                       !


  END FUNCTION


!****************** JAVA.FC.FORMAT.AMOUNT *************************
! This function adjusts the number of decimal places, the cents   *
! separator, and the dollars separator based on what is defined   *
! in the Exchange Rate File "DECIMAL" field.                      *
!******************************************************************

FUNCTION JAVA.FC.FORMAT.AMOUNT ( AMT, FMT.LN.SLOT ) EXTERNAL

  INTEGER*1                                      \
            JAVA.FC.FORMAT.AMOUNT                ! Return Code

  INTEGER*2                                      \
            FMT.LN.SLOT                          ! Foreign Currency Slot Number

  INTEGER*4                                      \
            AMT                                  ! Amount to Format

END FUNCTION


\ REM  ****************************************************************
\ REM
\ REM   FUNCTION JAVA.FORMAT.WEIGHT
\ REM
\ REM     CONVERT WEIGHT FIELD TO STRING
\ REM     ADD DECIMAL POINT IN POSITION SPECIFIED IN OPTIONS
\ REM
\ REM   END FUNCTION
\ REM
\ REM  ****************************************************************
  FUNCTION JAVA.FORMAT.WEIGHT ( WGT ) EXTERNAL    !
  INTEGER*1 JAVA.FORMAT.WEIGHT                    !
  INTEGER*4 WGT                                   !

  END FUNCTION
!E3458 EIR51405





!******************************************************************************
!function to test if passed value string is all zeroes.
!use real variable so a decimal can be included in string
!******************************************************************************
FUNCTION ZERO.VALUE(VALUE$)

  STRING                                         \
         VALUE$,                                 \ value in string value
         ZV.ERR$                                 ! used for debugging

  REAL                                           \
       VALUE                                     ! save value in real var

  INTEGER*1                                      \
            ZERO.VALUE                           ! function return

  ZERO.VALUE = 0                                 ! assume not zero value

  ON ERROR GOTO ZERO.VALUE.ERR                   ! in case of VAL error

  VALUE = VAL(VALUE$)                            ! get real value of argument

  IF (VALUE = 0.0) THEN BEGIN                    ! if zero value
    ZERO.VALUE = -1                              ! value is zero
  ENDIF                                          ! if zero value

EXIT.ZERO.VALUE:
  EXIT FUNCTION

ZERO.VALUE.ERR:

  ZV.ERR$ = ERR

  RESUME EXIT.ZERO.VALUE                         ! exit out of function

END FUNCTION                                     ! end ZERO.VALUE
!******************************************************************************


!******************************************************************************
!Function to return the "item qualifier" of an item.
!The "item qualifier" is defined to be how the item was entered in the
!transaction, i.e., scanned, keyed, linked, etc.
FUNCTION ITEM.QUALIFIER$(INDICAT3B)

   STRING                                        \
          ITEM.QUALIFIER$                        ! function return

   INTEGER*1                                     \
             INDICAT3B                           ! SL.IT.INDICAT3B

   IF (INDICAT3B < 0) OR                         \
      (INDICAT3B > 4) THEN BEGIN                 ! if invalid value
     ITEM.QUALIFIER$ = ""
     EXIT FUNCTION
   ENDIF                                         ! if invalid value

   ON (INDICAT3B + 1) GOSUB                      \
     SCANNED.ITEM.CODE,                          \
     KEYED.ITEM.CODE,                            \
     ITEM.LOOKUP.KEYED,                          \
     ITEM.CODE.LINKED,                           \
     ITEM.CODE.WANDED

   EXIT FUNCTION

SCANNED.ITEM.CODE:
   ITEM.QUALIFIER$ = "ScannedItemCode"
   RETURN

KEYED.ITEM.CODE:
   ITEM.QUALIFIER$ = "KeyedItemCode"
   RETURN

ITEM.LOOKUP.KEYED:
   ITEM.QUALIFIER$ = "ItemLookupKeyed"
   RETURN

ITEM.CODE.LINKED:
   ITEM.QUALIFIER$ = "LinkedItemCode"
   RETURN

ITEM.CODE.WANDED:
   ITEM.QUALIFIER$ = "WandedItemCode"
   RETURN

END FUNCTION                                     ! end ITEM.QUALIFIER$
!******************************************************************************


!******************************************************************************
!Function to return if item is repeatable or not.
!This code was originally down in the "label.itemEvent" routine, moved it
!up here to be more readable.
!They had to use their own flag to show the item is repeatable since the base flag
!has not been set when the item event is called
!******************************************************************************
FUNCTION javaItemRepeatable

  INTEGER*1                                      \
            javaItemRepeatable                   ! function return

  javaItemRepeatable = 0                         ! initial setting

  !The event is called in TSIEEC03, so the TS.ENTRY.REPEATABLE flag has not been set
  !   yet.  Duplicate the code from TSIEEC01 to set this flag correctly (but use
  !   a dummy flag so as not to interfere)

  !The event is called in TSIEEC03, so the TS.ENTRY.REPEATABLE flag has not been set
  !   yet.  Duplicate the code from TSIEEC01 to set this flag correctly (but use
  !   a dummy flag so as not to interfere)
  \ REM!!********************  B B D L  *********************************
  \ REM!!
  \ REM!!  IF item is not linked to THEN
  \ REM!!  IF item is not a coupon multiplier THEN
  \ REM!!  : set entry repeatable
  \ REM!!  ENDIF
  \ REM!!
  \ REM!!*************************************************************

  IF SL.IT.INDICAT3B <> 3 THEN                   \
  IF (SL.IT.INDICAT2 AND 0400H) = 0 THEN BEGIN
    javaItemRepeatable = -1
  ENDIF

  \ REM!!*************************************************************
  \ REM!!
  \ REM!!   set off repeat flag if item is unusual
  \ REM!!
  \ REM!!********************  B B D L  *********************************
  \ REM!!
  \ REM!!  IF a weight item THEN
  \ REM!!  : set entry as not repeatable
  \ REM!!  ENDIF
  \ REM!!
  \ REM!!  IF not a sale THEN
  \ REM!!  IF not a deposit THEN
  \ REM!!  IF not a miscellaneous receipt THEN
  \ REM!!  : set entry as not repeatable (negative entry)
  \ REM!!  ENDIF
  \ REM!!
  \ REM!!  IF quantity or weight key hit THEN not repeatable
  \ REM!!
  \ REM!!  IF any override entered THEN not repeatable
  \ REM!!
  \ REM!!*************************************************************

  ! A WEIGHT ITEM IS NOT ALLOWED TO BE REPEATED

  IF (SL.IE.INDICAT1 AND (08H+40H)) THEN BEGIN
    javaItemRepeatable = 0
  ENDIF


  IF TS.REPEAT.COUNT+1 > TO.MAXREPEAT THEN BEGIN
    javaItemRepeatable = 0
  ENDIF

!AENH01  void previous transaction entries not repeatable
  IF VPLI.TRAN THEN BEGIN
    javaItemRepeatable = 0
  ENDIF
!EENH01

  ! IF NOT SALE, DEPOSIT OR MISC. RECEIPT THEN NO REPEAT ALLOWED

  IF SL.IT.INDICAT3A <> 0 THEN                   \
  IF SL.IT.INDICAT3A <> 1 THEN                   \
  IF SL.IT.INDICAT3A <> 4 THEN BEGIN
    javaItemRepeatable = 0
  ENDIF

  IF (SCANNED.PRICE)      OR                     \
     (TS.IO.KEYS(6) <> 0) OR                     \
     (TS.IO.KEYS(0) = 79) THEN javaItemRepeatable = 0

END FUNCTION                                     ! end javaItemRepeatable
!******************************************************************************

!AIO08970
!******************************************************************************
!Function to return if price verify item is repeatable or not.
!******************************************************************************
FUNCTION javaPVItemRepeatable

  INTEGER*1                                      \
            javaPVItemRepeatable                 ! function return

  javaPVItemRepeatable = -1                      ! initial setting

  \ REM!!*************************************************************
  \ REM!!
  \ REM!!   set off repeat flag if item is unusual
  \ REM!!
  \ REM!!********************  B B D L  *********************************
  \ REM!!
  \ REM!!  IF a weight item THEN
  \ REM!!  : set entry as not repeatable
  \ REM!!  ENDIF
  \ REM!!
  \ REM!!  IF not a sale THEN
  \ REM!!  IF not a deposit THEN
  \ REM!!  IF not a miscellaneous receipt THEN
  \ REM!!  : set entry as not repeatable (negative entry)
  \ REM!!  ENDIF
  \ REM!!*************************************************************

  !  TS.PC.IT: 0=normal, 1=deposit, 2=refund, 3=deposit return,
  !            4=misc sale, 5=misc refund, 6=mfr coupon, 7=store coupon
  IF (IR.INDICAT0 AND 40H) OR        \ WEIGHT ITEM
     (TS.PC.IT = 2) OR               \ REFUND ITEM
     (TS.PC.IT = 3) OR               \ DEPOSIT RETURN
     (TS.PC.IT >= 5) THEN BEGIN      \ MISC REFUND OR MFR/STR COUPON
    javaPVItemRepeatable = 0         ! not repeatable
  ENDIF

END FUNCTION                                     ! end javaItemRepeatable
!******************************************************************************
!EIO08970


!******************************************************************************
!Routine to set up the item restrictive data.
!Unable to set up from the java initialization routine since it's called
!before the store options are read.
!******************************************************************************
SUB SETUP.RESTRICTED.DATA

  STRING                                         \
         element$,                               \ main element data
         marker$,                                \
         with$,                                  \
         START.TIME$,                            \ start time of restriction
         END.TIME$                               \ end time of restriction

  INTEGER*4 marker%, aefl%

  INTEGER*2 aefi%

  RESTRICTED.TIME.DATA$ = ""                     ! clear string

  javaSOrestOptsRead = -1                        ! flag restrictive options read

  FOR aefi% = 1 TO 7                                ! for each day of the week

    E$ =  RIGHT$("0" + STR$(SO.RESTEND(aefi%)),2) + \ chk to make sure end time is valid
          RIGHT$("0" + STR$(SO.RESTEND.MIN(aefi%)),2)

    IF (E$ > "0000") THEN BEGIN                  ! if end time is valid

!67,<restrict reversed=&1 startDay=[(1)"sun"][(2)"mon"][(3)"tue"][(4)"wed"][(5)"thu"][(6)"fri"][(7)"sat"] endDay=[(8)"sun"][(9)"mon"][(10)"tue"][(11)"wed"][(12)"thu"][(13)"fri"][(14)"sat"] startTime=&2 endTime=&3/>

       eventString$ = ?aefXML$(67)                   ! <restrict>
       element$ = ?GetXMLEvent$(eventString$)
       IF LEN(element$) THEN \
       BEGIN
         CALL AllowConditionalTag(aefi%, eventString$)     ! startDay
         CALL AllowConditionalTag(aefi% + 7, eventString$) ! endDay
         CALL RemoveConditionalTags(eventString$)
         GOSUB FORMAT.TIME$
         marker$ = ?FindMarker$(eventString$)       ! marker = &##... where #=number
         aefl% = LEN(marker$)
         WHILE aefl% <> 0                              ! loop until no more markers
           marker% = val(mid$(marker$,2,aefl%))        ! marker value
           with$ = ?InQuote$("")
           ON marker% GOSUB revSub,                 \
                            startSub, endSub
           CALL SubstituteMarker(marker$, eventString$, with$)
           marker$ = ?FindMarker$(eventString$)     ! next marker &##...
           aefl% = LEN(marker$)
         WEND
         RESTRICTED.TIME.DATA$ = RESTRICTED.TIME.DATA$ + eventString$
       ENDIF
    ENDIF                                        ! if end time is valid

  NEXT aefi%                                        ! for each day of the week

  A$ = ""                                        ! cleanup work variables
  B$ = ""
  C$ = ""
  D$ = ""
  E$ = ""
  START.TIME$ = ""
  END.TIME$ = ""
  with$ = ""
  marker$ = ""
  element$ = ""
  eventString$ = ""

  EXIT SUB                                       ! we're done


revSub:
  with$ = ?TrueOrFalse$(SO.REST.REVERSE)
  RETURN

startSub:
  with$ = ?InQuote$(?FormatForXML$(START.TIME$))
  RETURN

endSub:
  with$ = ?InQuote$(?FormatForXML$(END.TIME$))
  RETURN



FORMAT.TIME$:

  K% = SO.RESTSTART(aefi%)                       ! get starting hours
  IF (K% > 11) THEN BEGIN                        ! must be PM
    K% = K% - 12                                 ! adjust to 12 hour time
    D$ = "PM"
  ENDIF ELSE BEGIN                               ! else AM
    D$ = "AM"
  ENDIF                                          ! must be PM

  IF (K% = 0) THEN BEGIN                         ! either midnight or noon
    K% = 12                                      ! fix it
  ENDIF                                          ! either midnight or noon

  B$ = RIGHT$("00" + STR$(K%),2)                 ! get formatted hours
  C$ = RIGHT$("00" + STR$(SO.RESTSTART.MIN(aefi%)),2)    ! get minutes

  START.TIME$ = B$ + ":" + C$ + D$

  K% = SO.RESTEND(aefi%)                            ! get ending hours
  IF (K% > 11) THEN BEGIN                        ! must be PM
    K% = K% - 12                                 ! adjust to 12 hour time
    D$ = "PM"
  ENDIF ELSE BEGIN                               ! else AM
    D$ = "AM"
  ENDIF                                          ! must be PM

  IF (K% = 0) THEN BEGIN                         ! either midnight or noon
    K% = 12                                      ! fix it
  ENDIF                                          ! either midnight or noon

  B$ = RIGHT$("00" + STR$(K%),2)                 ! get formatted hours
  C$ = RIGHT$("00" + STR$(SO.RESTEND.MIN(aefi%)),2) ! get minutes

  END.TIME$ = B$ + ":" + C$ + D$

  RETURN

END SUB                                          ! end SETUP.RESTRICTED.DATA
!******************************************************************************


!******************************************************************************
!Routine to handle a point only item.
!******************************************************************************
SUB POINT.ONLY.ITEM(DATA$,PT.TYPE$,PT.ITEMNAME$,POINTS$,REDEEMED.PTS)

  STRING                                         \
         PTS$,                                   \ points in string value
         PT.TYPE$,                               \ point type
         PT.ITEMNAME$,                           \ point item name from item record
         POINTS$,                                \ points passed in string value
         DATA$,                                  \ return data value
         ELEMENT$,                               \
         marker$,                                \
         with$

  INTEGER*1                                      \
            VOIDED.PTS,                          \ flag if points voided
            REDEEMED.PTS                         ! flag if points redeemed

  INTEGER*4 marker%, aefl%

  PTS$ = ?RemoveCharacter$(" ",POINTS$)          ! get points from EM var

  IF ((SL.IT.INDICAT2 AND 0080H) <> 0) THEN BEGIN ! if this is a void
    VOIDED.PTS = -1                              ! set void flag
  ENDIF ELSE BEGIN                               ! else not a void
    VOIDED.PTS = 0                               ! turn off void flag
  ENDIF                                          ! if this is a void

  !NOTE:  It was decided to send redeemed points in as a negative amount even though
  !base EM considers redeemed point just another bucket of positive points.
  !We also want to move any negative marker from the end of the EM variable and move
  !to the beginning for the XML string.
  !If redeemed points then we just invert whatever sign was there.
  IF (RIGHT$(PTS$,1) = "-") THEN BEGIN           ! if negative sign on string
    PTS$ = LEFT$(PTS$,LEN(PTS$) - 1)             ! strip it off
    IF (REDEEMED.PTS = 0) THEN BEGIN             ! if bonus point item
      IF (ZERO.VALUE(PTS$) = 0) THEN BEGIN       ! don't add sign to zero
        PTS$ = "-" + PTS$                        ! add back in neg sign at beginning
      ENDIF                                      ! don't add sign to zero
    ENDIF                                        ! if bonus point item
  ENDIF ELSE BEGIN                               ! else no negative sign
    IF REDEEMED.PTS THEN BEGIN                   ! if redeemed points
      IF (ZERO.VALUE(PTS$) = 0) THEN BEGIN       ! don't add sign to zero
        PTS$ = "-" + PTS$                        ! add back in neg sign at beginning
      ENDIF                                      ! don't add sign to zero
    ENDIF                                        ! if redeemed points
  ENDIF                                          ! if negative sign on string



!50,<points id=&1 idQualifier=&2 type=&3 description=&4 points=&5 redeemed=&6 isVoided=&7>
!51,</points>
!A6540
!   !52,<printLine rawData=&1 filteredData=&2/>
!52,<printLine rawData=&1 filteredData=&2 lineType=&3 lineCategory=&4 font=&5 centered="false" columnsToPrint=&6/> D2424/17914
!E6540

    eventString$ = ?aefXML$(50)                   ! <points>
    element$ = ?GetXMLEvent$(eventString$)
    IF LEN(element$) THEN \
    BEGIN
      CALL RemoveConditionalTags(eventString$)
      marker$ = ?FindMarker$(eventString$)        ! marker = &##... where #=number
      aefl% = LEN(marker$)
      WHILE aefl% <> 0                               ! loop until no more markers
        marker% = val(mid$(marker$,2,aefl%))         ! marker value
        with$ = ?InQuote$("")
        ON marker% GOSUB itemIdSub, idQualSub,         \
                         ptsTypeSub, ptsDescSub,      \
                         ptsSub, redeemedSub,         \
\AIO08970
\                        ptsVoidSub
                         ptsVoidSub, ptsQtySub
!EIO08970
        CALL SubstituteMarker(marker$, eventString$, with$)
        marker$ = ?FindMarker$(eventString$)      ! next marker &##...
        aefl% = LEN(marker$)
      WEND
      data$ = data$ + eventString$
    ENDIF

    FOR PRT.LINE.COUNTER = 1 TO NUM.PRT.LINES        ! Handle all lines D21508
      eventString$ = ?aefXML$(52)                    ! <printLine>
      element$ = ?GetXMLEvent$(eventString$)
!AD21508 Handle each saved line
\     IF (LEN(element$)>0 AND JAVA.ITEM.HDR.LINE.RAW$ <> "" ) THEN \
      IF (LEN(element$)>0) THEN \
\ED21508
      BEGIN
         call RemoveConditionalTags(eventString$)
         marker$ = ?FindMarker$(eventString$)           ! rawData attribute
         IF (LEN(marker$) > 0) THEN \
         BEGIN
!AD21508 Handle each saved line
\           with$ = ?InQuote$(JAVA.ITEM.HDR.LINE.RAW$)
            with$ = ?InQuote$(JAVA.ITEM.PRT.LINE.RAW$(PRT.LINE.COUNTER))
!ED21508
            printLine.rawData$ = with$                 ! 6540
            CALL SubstituteMarker(marker$, eventString$, with$)
        ENDIF
        marker$ = ?FindMarker$(eventString$)           ! filteredData attribute
        IF (LEN(marker$) > 0) THEN \
        BEGIN
!AD21508 Handle each saved line
\           with$ = ?InQuote$(JAVA.ITEM.HDR.LINE$)
            with$ = ?InQuote$(JAVA.ITEM.PRT.LINE$(PRT.LINE.COUNTER))
!ED21508
            CALL SubstituteMarker(marker$, eventString$, with$)
        ENDIF
        CALL ContextSensitivePrintLines(eventString$)  ! 6540
        data$ = data$ + eventString$

      ENDIF
    NEXT                                               ! D21508
!AD21508 This block was handled with the loop above.  Delete it.
\   eventString$ = ?aefXML$(52)                        ! <printLine>
\   element$ = ?GetXMLEvent$(eventString$)
\   IF (LEN(element$)>0 AND JAVA.ITEM.PRT.LINE.RAW$ <> "" ) THEN \
\   BEGIN
\      call RemoveConditionalTags(eventString$)
\      marker$ = ?FindMarker$(eventString$)           ! rawData attribute
\      IF (LEN(marker$) > 0) THEN \
\      BEGIN
\          with$ = ?InQuote$(JAVA.ITEM.PRT.LINE.RAW$)
\          printLine.rawData$ = with$                 ! 6540
\          CALL SubstituteMarker(marker$, eventString$, with$)
\      ENDIF
\      marker$ = ?FindMarker$(eventString$)           ! filteredData attribute
\      IF (LEN(marker$) > 0) THEN \
\      BEGIN
\          with$ = ?InQuote$(JAVA.ITEM.PRT.LINE$)
\          CALL SubstituteMarker(marker$, eventString$, with$)
\      ENDIF
\      CALL ContextSensitivePrintLines(eventString$)  ! 6540
\      data$ = data$ + eventString$
\
\   ENDIF
!ED21508
    eventString$ = ?aefXML$(51)                        ! </points>
    element$ = ?GetXMLEvent$(eventString$)
    IF LEN(element$)>0 THEN \
    BEGIN
       call RemoveConditionalTags(eventString$)
       data$ = data$ + eventString$
    ENDIF

!AIO08970
!AD21508 Zero a counter instead of nulling the strings
\   JAVA.ITEM.HDR.LINE$ = ""
\   JAVA.ITEM.HDR.LINE.RAW$ = ""
\   JAVA.ITEM.PRT.LINE$ = ""
\   JAVA.ITEM.PRT.LINE.RAW$ = ""
    NUM.PRT.LINES=0                   ! no saved lines now
!ED21508
!EIO08970

    GOTO ptsEnd

itemIdSub:
  !Now build the data string
  !First the item code (since SL.IT.ITEMCODE is set differently for EAN99
  !   AFTER the call to TSIEEC03, use the unpacked value in this case.
  IF (EAN99) THEN BEGIN
     with$ = ?InQuote$(?FormatForXML$(ITEM.99.UNPACKED$))
  ENDIF ELSE BEGIN
     with$ = ?InQuote$(?FormatForXML$(SL.IT.ITEMCODE$))
  ENDIF
  RETURN

idQualSub:
   with$ = ?InQuote$(?FormatForXML$(ITEM.QUALIFIER$(SL.IT.INDICAT3B)))
   RETURN


ptsTypeSub:
   with$ = ?InQuote$(?FormatForXML$(PT.TYPE$))
   RETURN

ptsDescSub:
   with$ = ?InQuote$(?FormatForXML$(?RemoveCharacter$(" ", PT.ITEMNAME$)))
   RETURN

ptsSub:
   with$ = ?InQuote$(PTS$)
   RETURN

redeemedSub:
   with$ = ?TrueOrFalse$(VOIDED.PTS)
   RETURN

ptsVoidSub:
   with$ = ?TrueOrFalse$(VOIDED.PTS)
   RETURN

!AIO08970
ptsQtySub:
  IF EXT.STR THEN BEGIN                        ! if extension string
    IF SL.IE.INDICAT1 AND 07EH THEN BEGIN      ! a weight or quantity is valid
      with$ = ?InQuote$(STR$(SL.IE.QTYORWGT))  ! QTYORWGT field is valid
    ENDIF ELSE BEGIN
      with$ = ?InQuote$(STR$(SL.IE.SALEQUAN))  ! format sale quantity
    ENDIF
  ENDIF ELSE BEGIN                             ! else no extension string
    with$ = ?InQuote$("1")                     ! only a quantity of 1
  ENDIF
  RETURN
!EIO08970

ptsEnd:

END SUB                                          ! end POINT.ONLY.ITEM
!******************************************************************************






!******************************************************************************
!Function returns true if a valueless coupon.
!EM uses valueless coupons for cross promotion validation, it was decided to
!not send the item data on these type of coupons.
!******************************************************************************
FUNCTION VALUELESS.COUPON

  INTEGER*1                                      \
            VALUELESS.COUPON                     ! function return

  VALUELESS.COUPON = 0                           ! assume not valueless cpn

  IF (SL.IT.INDICAT3A <> 6) AND                  \ MFR coupon
     (SL.IT.INDICAT3A <> 7) THEN BEGIN           ! STR coupon
                                                 ! if not a coupon
    EXIT FUNCTION                                ! get out
  ENDIF                                          ! if not a coupon

  IF (TS.XPRICE <> 0) THEN BEGIN                 ! if coupon has value
    EXIT FUNCTION                                ! get out
  ENDIF                                          ! if coupon has value

  IF (IR.PRICE1 <> 1) THEN BEGIN                 ! if not valueless coupon
    EXIT FUNCTION                                ! get out
  ENDIF                                          ! if not valueless coupon

  VALUELESS.COUPON = -1                          ! must be a valueless coupon

END FUNCTION                                     ! end VALUELESS.COUPON
!******************************************************************************


!******************************************************************************
!Function returns the a price in the correct format.
!******************************************************************************
FUNCTION PRICE.FORMATTED$(PRICE%)

  STRING                                         \
         AEF.TEMP$,                              \
         PRICE.FORMATTED$                        ! function return

  INTEGER*4                                      \
            PRICE%                               ! work variable

  CALL JAVA.FORMAT.AMOUNT(PRICE%)                ! format amount

  JAVA.AMT$ = ?RemoveCharacter$(" ",JAVA.AMT$)   ! strip off blanks

  IF (RIGHT$(JAVA.AMT$,1) = "-") THEN BEGIN      ! if negative sign on string
    JAVA.AMT$ = LEFT$(JAVA.AMT$,LEN(JAVA.AMT$) - 1)   ! strip it off
    JAVA.AMT$ = "-" + JAVA.AMT$                  ! move to front of string
    JAVA.AMT$ = ?RemoveCharacter$(" ",JAVA.AMT$) ! strip blanks again
  ENDIF                                          ! if negative sign on string

  PRICE.FORMATTED$ = JAVA.AMT$                   ! return formatted price
END FUNCTION                                     ! end PRICE.FORMATTED$
!******************************************************************************


!******************************************************************************
!Function returns the extended price in the correct format.
!******************************************************************************
FUNCTION EXTENDED.PRICE.FORMATTED$

  STRING                                         \
         EXTENDED.PRICE.FORMATTED$               ! function return

  INTEGER*4                                      \
            XPRICE                               ! work variable

  XPRICE = SL.IT.XPRICE                          ! get the extended price

  IF ((SL.IT.INDICAT2 AND 0004H ) <> 0) THEN BEGIN    ! if negative price due to deal
    XPRICE = -XPRICE                             ! negate price
  ENDIF                                          ! if negative price due to deal

  IF ((SL.IT.INDICAT2 AND 0080H) <> 0) THEN BEGIN     ! item voided
    XPRICE = -XPRICE                             ! negate price
  ENDIF                                          ! item voided

  IF (SL.IT.INDICAT3A = 5) OR                    \ miscellaneous refund item
     (SL.IT.INDICAT3A = 6) OR                    \ manufacturer coupon
     (SL.IT.INDICAT3A = 7) THEN BEGIN            ! store coupon
                                                 ! if negative priced item type
    XPRICE = -XPRICE
  ENDIF                                          ! if negative priced item type

  EXTENDED.PRICE.FORMATTED$ = PRICE.FORMATTED$(XPRICE)

END FUNCTION                                     ! end EXTENDED.PRICE.FORMATTED$
!******************************************************************************


!******************************************************************************
!Function returns the pricing method formatted.
!******************************************************************************
FUNCTION GET.PRICING.METHOD$

  STRING                                         \
         GET.PRICING.METHOD$                     ! function return

  ! if invalid pricing method
  IF (SL.IE.METHOD < 0) OR                       \
     (SL.IE.METHOD > 5) THEN BEGIN               !
    GET.PRICING.METHOD$ = """invalid"""          ! invalid data
    EXIT FUNCTION                                ! get out
  ENDIF                                          ! if invalid pricing method

  ON (SL.IE.METHOD + 1) GOSUB                    \
    PM0,                                         \
    PM1,                                         \
    PM2,                                         \
    PM3,                                         \
    PM4,                                         \
    PM5

  EXIT FUNCTION

PM0:
  IF (SL.IE.SALEQUAN > 1) THEN BEGIN             ! if a deal quantity is set
    GET.PRICING.METHOD$ = """dealQuantity"""
  ENDIF ELSE BEGIN                               ! else a deal quantity is not set
    GET.PRICING.METHOD$ = """unit"""
  ENDIF                                          ! if a deal quantity is set
  RETURN

PM1:
  GET.PRICING.METHOD$ = """basePlus1"""
  RETURN

PM2:
  GET.PRICING.METHOD$ = """groupThreshold"""
  RETURN

PM3:
  GET.PRICING.METHOD$ = """reducedDeal"""
  RETURN

PM4:
  GET.PRICING.METHOD$ = """increasedDeal"""
  RETURN

PM5:
  GET.PRICING.METHOD$ = """alias"""
  RETURN

END FUNCTION                                     ! end GET.PRICING.METHOD$
!******************************************************************************


!AIO08970
!******************************************************************************
! Returns the formatted pricing method, but does not depend on summary log data.
!******************************************************************************
FUNCTION GET.PV.PRICING.METHOD$

  STRING                                         \
         GET.PV.PRICING.METHOD$                  ! function return

  ! if invalid pricing method
  IF (TS.PC.PM < 0) OR                           \
     (TS.PC.PM > 5) THEN BEGIN                   !
    GET.PV.PRICING.METHOD$ = """invalid"""       ! invalid data
    EXIT FUNCTION                                ! get out
  ENDIF                                          ! if invalid pricing method

  ON (TS.PC.PM + 1) GOSUB                        \
    PM0,                                         \
    PM1,                                         \
    PM2,                                         \
    PM3,                                         \
    PM4,                                         \
    PM5

  EXIT FUNCTION

PM0:
  IF (VAL(TS.PC.QTY$) > 1) THEN BEGIN            ! if a deal quantity is set
    GET.PV.PRICING.METHOD$ = """dealQuantity"""
  ENDIF ELSE BEGIN                               ! else a deal quantity is not set
    GET.PV.PRICING.METHOD$ = """unit"""
  ENDIF                                          ! if a deal quantity is set
  RETURN

PM1:
  GET.PV.PRICING.METHOD$ = """basePlus1"""
  RETURN

PM2:
  GET.PV.PRICING.METHOD$ = """groupThreshold"""
  RETURN

PM3:
  GET.PV.PRICING.METHOD$ = """reducedDeal"""
  RETURN

PM4:
  GET.PV.PRICING.METHOD$ = """increasedDeal"""
  RETURN

PM5:
  GET.PV.PRICING.METHOD$ = """alias"""
  RETURN

END FUNCTION                                     ! end GET.PV.PRICING.METHOD$
!******************************************************************************
!EIO08970


!Begin @tal10012003
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
! Options inside the options event.
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
!<store name=&1 number=&2 phone=&3/>
FUNCTION ?Options.store$
   STRING ?Options.store$
   STRING xml$, element$, marker$, with$
   INTEGER*2   aefl%, marker%

   xml$ = ?aefXML$(92)                  ! store
   IF LEN(EO.STORE.NAME$) > 0 THEN BEGIN
      call AllowConditionalTag(1,xml$)    ! SA has some of the optional data
   ENDIF
   IF LEN(EO.ADDR1$) > 0 THEN BEGIN
      call AllowConditionalTag(3,xml$)    ! SA has some of the optional data
   ENDIF

   element$ = ?GetXMLEvent$(xml$)
   IF (LEN(element$) > 0) THEN BEGIN
      CALL RemoveConditionalTags(xml$)
      marker$ = ?FindMarker$(xml$)                 ! marker = &##... where #=number
      aefl% = LEN(marker$)
      WHILE aefl% <> 0                                ! loop until no more markers
        marker% = val(mid$(marker$,2,aefl%))          ! marker value
        with$ = ?InQuote$("")
        ON marker% GOSUB \
           nameSub1,numberSub2,divisionSub3,address1Sub4, \
           address2Sub5,citySub6,stateSub7,zipSub8, \
           phone1Sub9,phone2Sub10
        CALL SubstituteMarker(marker$, xml$, with$)
        marker$ = ?FindMarker$(xml$)               ! next marker &##...
        aefl% = LEN(marker$)
      WEND
   ENDIF ELSE BEGIN
      !Invalid XML, missing option name
      xml$ = ""
   ENDIF
   ?Options.store$ = xml$  !return the result
   xml$ = ""               !Free memory
   element$ = ""           !Free memory
   marker$ = ""            !Free memory
   with$ = ""              !Free memory
   EXIT FUNCTION

nameSub1:
   with$ = ?InQuote$(?FormatForXML$(?StripTrailing$(" ",EO.STORE.NAME$)))
   return
numberSub2:
   with$ = ?InQuote$(?FormatForXML$(?StripTrailing$(" ",TS.STORE$)))
   return
address1Sub4:
   with$ = ?InQuote$(?FormatForXML$(?StripTrailing$(" ",EO.ADDR1$)))
   return
address2Sub5:
   with$ = ?InQuote$(?FormatForXML$(?StripTrailing$(" ",EO.ADDR2$)))
   return
!The rest of these elements are not available in SA
divisionSub3:
citySub6:
stateSub7:
zipSub8:
phone1Sub9:
phone2Sub10:
   return

END FUNCTION

!<!-- Price override reason codes (0 to N allowed)) -->
FUNCTION ?Options.priceOverrideReason$
   STRING ?Options.priceOverrideReason$
   !Not available in SA
   ?Options.priceOverrideReason$ = ""
END FUNCTION

!<!-- Refund reason codes (0 to N allowed) -->
FUNCTION ?Options.refundReason$
   STRING ?Options.refundReason$
   !Not available in SA
   ?Options.refundReason$ = ""
END FUNCTION

!<!-- Tare codes (0 to N allowed) -->
FUNCTION ?Options.tareCode$
   STRING ?Options.tareCode$
   STRING xml$, element$, marker$, with$

   integer*2 icode
   integer*2 aefl%, marker%
   string    allTaresXML$

   !SA doesn't really have descriptions, so they won't be included.

   !Max of 10 tares in SA
   FOR icode = 0 TO 9
      xml$ = ?aefXML$(97)                   ! tare
      element$ = ?GetXMLEvent$(xml$)
      IF (LEN(element$) > 0) THEN BEGIN
         CALL RemoveConditionalTags(xml$)
         marker$ = ?FindMarker$(xml$)                 ! marker = &##... where #=number
         aefl% = LEN(marker$)
         WHILE aefl% <> 0                                ! loop until no more markers
           marker% = val(mid$(marker$,2,aefl%))          ! marker value
           with$ = ?InQuote$("")
           ON marker% GOSUB \
              descriptionSub1,codeSub2
           CALL SubstituteMarker(marker$, xml$, with$)
           marker$ = ?FindMarker$(xml$)               ! next marker &##...
           aefl% = LEN(marker$)
         WEND
         allTaresXML$ = allTaresXML$ + xml$
      ENDIF ELSE BEGIN
         !Invalid XML, missing option name
         xml$ = ""
      ENDIF
   NEXT icode

   ?Options.tareCode$ = allTaresXML$   !return the result
   allTaresXML$ = ""                !Free memory
   xml$ = ""                        !Free memory
   element$ = ""                    !Free memory
   marker$ = ""                     !Free memory
   with$ = ""                       !Free memory
   EXIT FUNCTION

descriptionSub1:
   with$ = ?InQuote$("")
   return
codeSub2:
   with$ = ?InQuote$(STR$(icode))
   return
END FUNCTION

!<!-- Transaction discount reason codes (0 to N allowed) -->
! Only include those discount groups which have a non-zero discount percent.
FUNCTION ?Options.transactionDiscountReason$
   STRING ?Options.transactionDiscountReason$
   STRING xml$, element$, marker$, with$

   integer*2 aefl%, marker%
   integer*2 iindex, icode
   string    allDiscountXML$, pct$

   iindex = 0
   icode = TO.DISC.CODE(iindex)                  !discount code (ID)
   WHILE (icode <> 0)                            !while we have valid codes

      if (TO.DISC.RATE(iindex)> 0) THEN BEGIN    ! Only include the discounts with
                                                 ! an actual discount (non-zero discount rate).
         xml$ = ""
         for aefl% = 98 to 101
           xml$ = xml$ + ?aefXML$(aefl%)            ! transactionDiscount 98-101
         next aefl%

         call AllowConditionalTag(1,xml$)        ! SA has taxExemption but not taxPlanExempts

         element$ = ?GetXMLEvent$(xml$)
         IF (LEN(element$) > 0) THEN BEGIN
            CALL RemoveConditionalTags(xml$)
            marker$ = ?FindMarker$(xml$)                 ! marker = &##... where #=number
            aefl% = LEN(marker$)
            WHILE aefl% <> 0                                ! loop until no more markers
              marker% = val(mid$(marker$,2,aefl%))          ! marker value
              with$ = ?InQuote$("")
              on marker% gosub \
                 descriptionSub1,codeSub2,rateSub3,taxExemptionSub4, \
                 taxPlan1ExemptSub5,taxPlan2ExemptSub6,taxPlan3ExemptSub7, \
                 taxPlan4ExemptSub8,taxPlan5ExemptSub9,taxPlan6ExemptSub10, \
                 taxPlan7ExemptSub11,taxPlan8ExemptSub12
              CALL SubstituteMarker(marker$, xml$, with$)
              marker$ = ?FindMarker$(xml$)               ! next marker &##...
              aefl% = LEN(marker$)
            WEND
            allDiscountXML$ = allDiscountXML$ + xml$
         ENDIF ELSE BEGIN
            !Invalid XML, missing option name
            xml$ = ""
         ENDIF

      ENDIF

      iindex = iindex + 1                         !next index
      icode = TO.DISC.CODE(iindex)                !next code
   WEND

   ?Options.transactionDiscountReason$ = allDiscountXML$   !return the result

   allDiscountXML$ = ""             !Free memory
   xml$ = ""                        !Free memory
   element$ = ""                    !Free memory
   marker$ = ""                     !Free memory
   with$ = ""                       !Free memory
   EXIT FUNCTION

descriptionSub1:
   with$ = ?InQuote$(?FormatForXML$(?NoSurroundSpace$(TO.DISC.DESC$(iindex))))
   RETURN

codeSub2:
   with$ = ?InQuote$(STR$(icode))
   RETURN

rateSub3:
!AIO07988/D2430 Discount can be less than 1%.  Add leading zeros.
\  pct$ = STR$(TO.DISC.RATE(iindex))
   pct$ = RIGHT$("0000" + STR$(TO.DISC.RATE(iindex)),5)
!EIO07988/D2430
   pct$ = LEFT$(pct$, LEN(pct$) - 2) + "." + RIGHT$(pct$, 2)
   with$ = ?InQuote$(pct$)
   RETURN

taxExemptionSub4:
   with$ = ?TrueOrFalse$(TO.DISC.EXEMPT(iindex))
   RETURN

!The rest of these elements are not available in SA. Should never get here
!   as these are part of optional group 2.
taxPlan1ExemptSub5:
taxPlan2ExemptSub6:
taxPlan3ExemptSub7:
taxPlan4ExemptSub8:
taxPlan5ExemptSub9:
taxPlan6ExemptSub10:
taxPlan7ExemptSub11:
taxPlan8ExemptSub12:
   RETURN

END FUNCTION

!<!-- Item discount reason codes (0 to N allowed) -->
FUNCTION ?Options.itemDiscountReason$
   STRING ?Options.itemDiscountReason$
   !Not available in SA
   ?Options.itemDiscountReason$ = ""
END FUNCTION

!<!-Tender definitions (0 to N allowed) -->
FUNCTION ?Options.tenderDefinition$
   STRING ?Options.tenderDefinition$
   STRING xml$, element$, marker$, with$

   string         allTendersXML$
   integer*2      aefl%, marker%, tdrLoop


   FOR tdrLoop = 1 TO TO.NUMTNDR

      xml$ = ?aefXML$(103) + ?aefXML$(104)        ! tender def'n (103-104)

      element$ = ?GetXMLEvent$(xml$)
      if len(element$) then \
      begin
        call RemoveConditionalTags(xml$)
        marker$ = ?FindMarker$(xml$)        ! marker = &##... where #=number
        aefl% = len(marker$)
        while aefl% <> 0                               ! loop until no more markers
          marker% = val(mid$(marker$,2,aefl%))         ! marker value
          with$ = ?InQuote$("")
          on marker% gosub \
            descriptionSub1,typeSub2,varietySub3,keySub4, \
            foreignTenderSub5,allowedSub6,canBeLoanedSub7, \
            canBePickedUpSub8,canBeCountedSub9,canBeRefundedSub10, \
            canBeVerifiedSub11,shortDescriptionSub12,tenderExchangeRankSub13
          call SubstituteMarker(marker$, xml$, with$)
          marker$ = ?FindMarker$(xml$)      ! next marker &##...
          aefl% = len(marker$)
        wend
        allTendersXML$ = allTendersXML$ + xml$
      endif

   NEXT tdrLoop

   ?Options.tenderDefinition$ = allTendersXML$   !return the result

   allTendersXML$ = ""              !Free memory
   xml$ = ""                        !Free memory
   element$ = ""                    !Free memory
   marker$ = ""                     !Free memory
   with$ = ""                       !Free memory
   exit function

descriptionSub1:
   with$ = ?InQuote$(?FormatForXML$(?StripTrailing$(" ",TO.TENDVDESC$(tdrLoop))))
   return
typeSub2:
   with$ = ?InQuote$(STR$(TO.TENDOPTS(tdrLoop,0)))
   return
varietySub3:
   with$ = ?InQuote$(STR$(TO.TENDOPTS(tdrLoop,1)))
   return
keySub4:
   return
foreignTenderSub5:
   with$ = ?TrueOrFalse$(0)      !default to not a foreign tender.  Not sure where else to get this info from.
   return
allowedSub6:                     ! tender accepted.  If it is in the list, it is allowed.
canBeLoanedSub7:                 ! always true in SA
canBePickedUpSub8:               ! always true in SA
canBeCountedSub9:                ! always true in SA
   with$ = ?TrueOrFalse$(-1)
   return
canBeRefundedSub10:              ! If change limit is > 0 it is refundable.
   with$ = ?TrueOrFalse$(TO.TENDLIMITS(tdrLoop, 1) > 0)
   return
canBeVerifiedSub11:
   with$ = ?TrueOrFalse$(TO.TENDOPTS(tdrLoop, 7) > 0)  ! can be verified
   return
shortDescriptionSub12:
   return
tenderExchangeRankSub13:
   with$ = ?InQuote$(STR$(TO.RANKING(TO.TENDOPTS(tdrLoop,0))))
   return
END FUNCTION

!<!-VAT Tax Codes (0 to N allowed) -->
FUNCTION ?Options.vatTaxCode$
   STRING ?Options.vatTaxCode$
   !Not available in SA
   ?Options.vatTaxCode$ = ""
END FUNCTION

!<storeOptions>
FUNCTION ?Options.storeOptions$
   STRING ?Options.storeOptions$

   STRING      xml$, element$, marker$, with$, cfCode$
   INTEGER*2   aefl%, marker%

!107,<storeOptions [(1)alphaStateInputAllowed=&1 alphaDriversLicenseInputAllowed=&2] [(2)priceVerifyInsideTransactionAllowed=&3
!108, overrideRequiresManagerKey=&4 maximumTransactionSize=&5 transactionWarningSize=&6 foodstampsAllowed=&7
!109, onlyFoodstampsAllowedAfterFoodstampTotal=&8 maximumSuspendedTransactions=&9] [(3)customerFunctionCode=&10]
!110, [(4)suspendTransactionAllowed=&11 WICTenderOnlyInWICTransaction=&12 weightInputDecimalPlaces=&13
!111, foreignTenderAllowed=&14 WICEBTEnabled=&15 CuStOmErLoYaLtYiD=&16]>

   xml$ = ""
   cfCode$ = ""
   for aefl% = 107 to 111
     xml$ = xml$ + ?aefXML$(aefl%)         ! storeOptions 107-111
   next aefl%
   xml$ = xml$ + ?aefXML$(115)          ! </storeOptions>

   call AllowConditionalTag(2,xml$)     ! SA has most of the group 2 optionals
   call AllowConditionalTag(4,xml$)     ! SA has most of the group 4 optionals
   IF EMSS.EM.INSTALLED THEN BEGIN      ! if EM installed
     IF (OP.NO.EM = 0) THEN BEGIN       ! if EM is active
        cfCode$ = STR$(OP.MSR.KEY)      ! customer key on keyboard
        IF (LEN(cfCode$) > 0) THEN \
        BEGIN
           call AllowConditionalTag(3,xml$) ! customerFunctionCode attribute
        ENDIF
     ENDIF
   ENDIF

   element$ = ?GetXMLEvent$(xml$)
   IF (LEN(element$) > 0) THEN BEGIN
      CALL RemoveConditionalTags(xml$)
      marker$ = ?FindMarker$(xml$)                 ! marker = &##... where #=number
      aefl% = LEN(marker$)
      WHILE aefl% <> 0                                ! loop until no more markers
        marker% = val(mid$(marker$,2,aefl%))          ! marker value
        with$ = ?InQuote$("")

        on marker% gosub \
           alphaStateInputAllowedSub1,alphaDriversLicenseInputAllowedSub2, \
           priceVerifyInsideTransAllowedSub3,overrideRequiresManagerKeySub4, \
           maximumTransactionSizeSub5,transactionWarningSizeSub6, \
           foodstampsAllowedSub7,onlyFSAllowedAfterFSTotalSub8, \
           maximumSuspendedTransactionsSub9,customerFunctionCodeSub10, \
           suspendTransactionAllowedSub11,WICTenderOnlyInWICTransactionSub12, \
           weightInputDecimalPlacesSub13,foreignTenderAllowedSub14,  \
           WICEBTEnabled15,    \@tal11042004
           customerLoyaltyID16 !D2424/18197

        CALL SubstituteMarker(marker$, xml$, with$)
        marker$ = ?FindMarker$(xml$)               ! next marker &##...
        aefl% = LEN(marker$)
      WEND
   ENDIF ELSE BEGIN
      !Invalid XML, missing option name
      xml$ = ""
   ENDIF

   ?Options.storeOptions$ = xml$  !return the result

   xml$ = ""               !Free memory
   element$ = ""           !Free memory
   marker$ = ""            !Free memory
   with$ = ""              !Free memory
   EXIT FUNCTION

!Alpha not applicable in SA, should not even get to these as they should be removed
alphaStateInputAllowedSub1:
alphaDriversLicenseInputAllowedSub2:
   return
priceVerifyInsideTransAllowedSub3:
   with$ = ?TrueOrFalse$(TO.PRICEVERIFY)
   return
overrideRequiresManagerKeySub4:
   with$ = ?TrueOrFalse$(TO.MGRKEY)
   return
maximumTransactionSizeSub5:
   with$ = ?InQuote$(STR$(GC.SL.SIZE))
   return
transactionWarningSizeSub6:
   with$ = ?InQuote$(STR$(GC.SL.WARN))
   return
foodstampsAllowedSub7:
   with$ = ?TrueOrFalse$(TO.FOODSTAMPS)
   return
onlyFSAllowedAfterFSTotalSub8:      !always true for SA
   with$ = ?TrueOrFalse$(-1)
   return
maximumSuspendedTransactionsSub9:
   with$ = ?InQuote$(STR$(SO.SR.TYPE))
   return
customerFunctionCodeSub10:
   with$ = ""
   IF (LEN(cfCode$) > 0) THEN \
   BEGIN
      with$ = ?InQuote$(cfCode$)
   ENDIF
   return
suspendTransactionAllowedSub11:
   with$ = ?TrueOrFalse$(SO.SR.TYPE > 0)
   return
WICTenderOnlyInWICTransactionSub12:    !WIC Tender Only in WIC xact (no option in SA)
   with$ = ?TrueOrFalse$(0)
   return
weightInputDecimalPlacesSub13:
   with$ = ?InQuote$(STR$(TO.WGT.DECPOS))
   return
foreignTenderAllowedSub14:
   with$ = ?TrueOrFalse$(MC.ENABLED)
   return
!Begin @tal11042004
WICEBTEnabled15:
   with$ = ?TrueOrFalse$(TO.WE.ENABLED)
   return
!End @tal11042004
!AD2424/18197
customerLoyaltyID16:
   with$ = ?InQuote$(STR$(OP.VA.VALUE))
   return
!ED2424/18197

END FUNCTION

!<terminalOptions>
FUNCTION ?Options.terminalOptions$
   STRING ?Options.terminalOptions$

   !Not available in SA
   ?Options.terminalOptions$ = ""
END FUNCTION

!<!-- Department definitions (0 to N allowed) -->
FUNCTION ?Options.departmentDefinition$
   STRING ?Options.departmentDefinition$
   STRING      xml$, element$, marker$, with$
   INTEGER*2   aefl%, marker%

   integer*2 icode,fcode
   string    allDeptsXML$, deptName$

   fcode=199
   FOR icode = 1 TO MAXLUPS
      !use descriptor from lookup array.
      !only send data for departments that have a valid lookup
      deptName$ = ?RemoveCharacter$(CHR$(0),IRLOOK.ITEMNAME$(icode))
      IF (deptName$ <> "") THEN BEGIN

         xml$ = ?aefXML$(122) + ?aefXML$(123)        ! dept def'n (122-123)

         call AllowConditionalTag(1,xml$)    ! SA has most of the group 1 optionals

         element$ = ?GetXMLEvent$(xml$)
         if len(element$) then \
         begin
           call RemoveConditionalTags(xml$)
           marker$ = ?FindMarker$(xml$)        ! marker = &##... where #=number
           aefl% = len(marker$)
           while aefl% <> 0                               ! loop until no more markers
             marker% = val(mid$(marker$,2,aefl%))         ! marker value
             with$ = ?InQuote$("")
             on marker% gosub \
               descriptionSub1,numberSub2,keySub3, \
               toggleTaxAllowedSub4, toggleFoodstampAllowedSub5, \
               refundAllowedSub6, storeCouponAllowedSub7, manufacturerCouponAllowedSub8, \
\!AD2759
\!             priceAllowedSub9
               priceAllowedSub9, \
               taxableSub10, foodstampableSub11
\!ED2759
             call SubstituteMarker(marker$, xml$, with$)
             marker$ = ?FindMarker$(xml$)      ! next marker &##...
             aefl% = len(marker$)
           wend
           allDeptsXML$ = allDeptsXML$ + xml$
         endif
      ENDIF
   NEXT icode

   ?Options.departmentDefinition$ = allDeptsXML$   !return the result

   allDeptsXML$ = ""                !Free memory
   xml$ = ""                        !Free memory
   element$ = ""                    !Free memory
   marker$ = ""                     !Free memory
   with$ = ""                       !Free memory

   EXIT FUNCTION

descriptionSub1:
   with$ = ?InQuote$(?FormatForXML$(?NoSurroundSpace$(deptName$)))
   RETURN
numberSub2:
!A D2474 Use the department NUMBER, not its index
\  with$ = ?InQuote$(STR$(icode))
   with$ = ?InQuote$(STR$(VAL(UNPACK$(IRLOOK.DEPARTME$(icode)))))
!E D2474
   RETURN
keySub3:
   with$ = ?InQuote$(STR$(fcode+icode))
   RETURN
toggleTaxAllowedSub4:
! AIO05031
!   with$ = ?TrueOrFalse$(TO.LUKEYS(icode,0))
   with$ = ?TrueOrmanagerOverride$(TO.LUKEYS(icode,0))
! EIO05031
   RETURN
toggleFoodstampAllowedSub5:
! AIO05031
!   with$ = ?TrueOrFalse$(TO.LUKEYS(icode,1))
   with$ = ?TrueOrmanagerOverride$(TO.LUKEYS(icode,1))
! EIO05031
   RETURN
refundAllowedSub6:
! AIO05031
!   with$ = ?TrueOrFalse$(TO.LUKEYS(icode,2))
   with$ = ?TrueOrmanagerOverride$(TO.LUKEYS(icode,2))
! EIO05031
   RETURN
storeCouponAllowedSub7:
! AIO05031
!   with$ = ?TrueOrFalse$(TO.LUKEYS(icode,4))
   with$ = ?TrueOrmanagerOverride$(TO.LUKEYS(icode,4))
!EIO05031
   RETURN
manufacturerCouponAllowedSub8:
! AIO05031
!   with$ = ?TrueOrFalse$(TO.LUKEYS(icode,3))
   with$ = ?TrueOrmanagerOverride$(TO.LUKEYS(icode,3))
! EIO05031
   RETURN

priceAllowedSub9:
   RETURN

!AD2759
taxableSub10:
   !  This is the item code Tax Plan A,B,C,D value for this dept at initialization.
   !  Ref EAMTSINC.BDC.
   with$ = ?TrueOrFalse$( (IRLOOK.INDICAT1(icode) AND 00F0H) )
   RETURN

foodstampableSub11:
   !  This is the item code Food Stamp Item value for this dept at initialization.
   !  Ref EAMTSINC.BDC.
   with$ = ?TrueOrFalse$( (IRLOOK.INDICAT1(icode) AND 0008H) )
   RETURN
!ED2759



END FUNCTION

!<!-Alternate Tax codes (0 to N allowed) -->
FUNCTION ?Options.alternateTaxCode$
   STRING ?Options.alternateTaxCode$

   !Not available in SA
   ?Options.alternateTaxCode$ = ""
END FUNCTION

!<!- Manual Tax codes (0 to N allowed) -->
FUNCTION ?Options.manualTaxCode$
   STRING ?Options.manualTaxCode$

   !Not available in SA
   ?Options.manualTaxCode$ = ""
END FUNCTION


!<!- No Tax codes (0 to N allowed) -->
FUNCTION ?Options.noTaxCode$
   STRING ?Options.noTaxCode$
   STRING xml$, element$, marker$, with$

   integer*2 aefl%, marker%
   integer*2 iindex, icode
   string    allDiscountXML$

   iindex = 0
   icode = TO.DISC.CODE(iindex)                  !discount code (ID)
   WHILE (icode <> 0)                            !while we have valid codes

      IF (TO.DISC.RATE(iindex) = 0 AND           \ Only include the discounts with
          TO.DISC.EXEMPT(iindex)) THEN           \ a zero disc. rate and a tax exemption.
      BEGIN
         xml$ = ?aefXML$(41)                     ! <noTaxCode>

         element$ = ?GetXMLEvent$(xml$)
         IF (LEN(element$) > 0) THEN BEGIN
            CALL RemoveConditionalTags(xml$)
            marker$ = ?FindMarker$(xml$)                 ! marker = &##... where #=number
            aefl% = LEN(marker$)
            WHILE aefl% <> 0                                ! loop until no more markers
              marker% = val(mid$(marker$,2,aefl%))          ! marker value
              with$ = ?InQuote$("")
              on marker% gosub \
                 ntDescSub1,ntCodeSub2
              CALL SubstituteMarker(marker$, xml$, with$)
              marker$ = ?FindMarker$(xml$)               ! next marker &##...
              aefl% = LEN(marker$)
            WEND
            allDiscountXML$ = allDiscountXML$ + xml$
         ENDIF ELSE BEGIN
            !Invalid XML, missing option name
            xml$ = ""
         ENDIF

      ENDIF

      iindex = iindex + 1                         !next index
      icode = TO.DISC.CODE(iindex)                !next code
   WEND

   ?Options.noTaxCode$ = allDiscountXML$   !return the result

   allDiscountXML$ = ""             !Free memory
   xml$ = ""                        !Free memory
   element$ = ""                    !Free memory
   marker$ = ""                     !Free memory
   with$ = ""                       !Free memory
   EXIT FUNCTION

ntDescSub1:
   with$ = ?InQuote$(?FormatForXML$(?NoSurroundSpace$(TO.DISC.DESC$(iindex))))
   RETURN

ntCodeSub2:
   with$ = ?InQuote$(STR$(icode))
   RETURN

END FUNCTION




SUB itemEventElementType(indicat3a, elementType$)
   STRING elementType$
   INTEGER*1 indicat3a

!AIO08970
!  IF (indicat3a > 9) THEN BEGIN
!    elementType$ = ""
!    EXIT SUB
!  ENDIF
   elementType$ = ""
!EIO08970

   ON (indicat3a + 1) GOSUB                      \
      label.normal,                              \ 1
      label.deposit,                             \ 2
      label.refund,                              \ 3
      label.depositreturn,                       \ 4
      label.miscreceipt,                         \ 5
      label.miscpayout,                          \ 6
      label.manfcoupon,                          \ 7
      label.storecoupon,                         \ 8
      label.itemvoid,                            \ 9
      label.depvoid                              ! 10
  EXIT SUB

  ! 0 = NORMAL SALE
  ! 1 = DEPOSIT
  ! 2 = REFUND
  ! 3 = DEPOSIT RETURN
  ! 4 = MISC TRANS RECEIPT
  ! 5 = MISC TRANS PAYOUT
  ! 6 = MANF COUPON
  ! 7 = STORE COUPON
  ! 8 = ITEM SALE CANCEL
  ! 9 = DEPOSIT CANCEL


label.normal:
! elementType$ = "itemEvent"                ! IO08970
  elementType$ = "NormalSaleItem"           ! IO08970
  RETURN

label.deposit:
! elementType$ = "itemEvent"                ! IO08970
  elementType$ = "DepositItem"              ! IO08970
  RETURN

label.refund:
! elementType$ = "itemEvent"                ! IO08970
  elementType$ = "RefundItem"               ! IO08970
  RETURN

label.depositreturn:
! elementType$ = "itemEvent"                ! IO08970
  elementType$ = "DepositReturnItem"        ! IO08970
  RETURN

label.miscreceipt:
! elementType$ = "itemEvent"                ! IO08970
  elementType$ = "MiscPayinItem"            ! IO08970
  RETURN

label.miscpayout:
! elementType$ = "miscPayout"               ! IO08970
  elementType$ = "MiscPayoutItem"           ! IO08970
  RETURN

label.manfcoupon:
! elementType$ = "coupon"                   ! IO08970
  elementType$ = "ManufacturerCouponItem"   ! IO08970
  RETURN

label.storecoupon:
! elementType$ = "coupon"                   ! IO08970
  elementType$ = "StoreCouponItem"          ! IO08970
  RETURN

label.itemvoid:
! elementType$ = "itemEvent"                ! IO08970
  elementType$ = "VoidItem"                 ! IO08970
  RETURN

label.depvoid:
! elementType$ = "itemEvent"                ! IO08970
  elementType$ = "VoidDeposit"              ! IO08970
  RETURN

END SUB




!AIO19200
!**************************************************************
!
! SUB label.receipt.setup.java
!
! parms: printLine.temp$ current printline being processed.
!
! comments: This SUB was created to provide future space for code changes
! within the GOSUB label.receiptEvent: so that linker LEDATA errors will not occur.
! The code within was ORIGINALLY copied intact from label.receiptEvent:
! into this SUB.           
! IT IS DESIGNED TO ONLY BE CALLED FROM label.receiptEvent: !!!
!           
!**************************************************************
SUB label.receipt.setup.java ( printLine.temp$ ) RECURSIVE
    STRING                                       \
             printLine.temp$                     !


    JAVA.TENDER.HDR.SIGNATURE = 0                  ! IO08970

    IF (TS.LINETYPE = 2) OR                        \
       (TS.LINETYPE = 9) THEN BEGIN                ! Tender print lines
      IF (TS.LINETYPE = 2) THEN BEGIN              ! header line
!AD21508 There can be more than one tender header line
\       JAVA.TENDER.HDR.LINE$ = printLine.temp$
\       JAVA.TENDER.HDR.LINE.RAW$ = ?FormatForXML$(TS.PRTBUF$)
        ! do not save headers if in a loan or pickup (or other non-sales)
!AD25022 Also capture Tender Headers for Terminal Transfer
\       IF (TS.PROCEDURE <= 0) THEN BEGIN          ! only if in a sales transaction
        IF (TS.PROCEDURE <= 0) OR                  \ only if in a sales transaction
           (TS.PROCEDURE  = 8) THEN BEGIN          ! or Terminal Transfer
!ED25022
          IF NUM.TENDER.HDR.LINES < JAVA.TENDER.HDR.MAX THEN BEGIN ! no not blow the array
            NUM.TENDER.HDR.LINES = NUM.TENDER.HDR.LINES + 1 ! one more now
          ENDIF
          JAVA.TENDER.HDR.LINE$(NUM.TENDER.HDR.LINES) = printLine.temp$
          JAVA.TENDER.HDR.LINE.RAW$(NUM.TENDER.HDR.LINES) = ?FormatForXML$(TS.PRTBUF$)
        ENDIF
!ED21508
        JAVA.TENDER.PRT.INDEX = 0
!AIO08970 If a tender signature line was processed, set the
!         JAVA.TENDER.HDR.SIGNATURE flag to indicate that
!         the next <printLine> event is a signature line.
        IF (JAVA.SIGNATURE.LINE <> 0 ) THEN BEGIN
          JAVA.TENDER.HDR.SIGNATURE = -1
          JAVA.SIGNATURE.LINE = 0
        ENDIF
!EIO08970
!AIO05908 Allocate these arrays to the max number of franked lines per tender
!         times the number of EFT tenders that can be accepted.
!       DIM JAVA.TENDER.PRT.LINE$(24)              !D2012 - Franking formats are 24 lines max
!       DIM JAVA.TENDER.PRT.LINE.RAW$(24)          !D2012 - Franking formats are 24 lines max
        DIM JAVA.TENDER.PRT.LINE$(120)             !D2012 - Franking formats are 24 lines max
        DIM JAVA.TENDER.PRT.LINE.RAW$(120)         !D2012 - Franking formats are 24 lines max
        DIM JAVA.TENDER.PRT.SIGNATURE(120)         !IO08970
!EIO05908
      ENDIF ELSE BEGIN
        JAVA.TENDER.PRT.LINE$(JAVA.TENDER.PRT.INDEX) = printLine.temp$
        JAVA.TENDER.PRT.LINE.RAW$(JAVA.TENDER.PRT.INDEX) = ?FormatForXML$(TS.PRTBUF$)
!AIO08970  If a tender signature line was processed, set the
!          JAVA.TENDER.PRT.SIGNATURE array element to indicate
!          that the next <printLine> event is a signature line.
        IF (JAVA.SIGNATURE.LINE <> 0 ) THEN BEGIN
          JAVA.TENDER.PRT.SIGNATURE(JAVA.TENDER.PRT.INDEX) = -1
          JAVA.SIGNATURE.LINE = 0
        ENDIF
!EIO08970

        JAVA.TENDER.PRT.INDEX = JAVA.TENDER.PRT.INDEX + 1
      ENDIF
    ENDIF                                          ! tender print

    IF (TS.LINETYPE = 4) OR                        \
       (TS.LINETYPE = 3) THEN BEGIN                ! discount print
      IF (TS.LINETYPE = 4) THEN BEGIN
        JAVA.DISCOUNT.HDR.LINE$ = printLine.temp$
        JAVA.DISCOUNT.HDR.LINE.RAW$ = ?FormatForXML$(TS.PRTBUF$)
      ENDIF ELSE BEGIN
        JAVA.DISCOUNT.PRT.LINE$ = printLine.temp$
        JAVA.DISCOUNT.PRT.LINE.RAW$ = ?FormatForXML$(TS.PRTBUF$)
      ENDIF                                        ! header line or blank
    ENDIF                                          ! discount print

END SUB
!EIO19200

!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
! sub javaAEFEvent
!
! parms: event  The enumerated number of the event to be fired.
!
! comments: event may be set to -1 to avoid firing the event.
!           Note that even if the event is not excluded, it may
!           not be fired.  This will occur when the javaGui interface
!           detects that the java application is not active.
!
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
SUB javaAEFEvent(event) PUBLIC RECURSIVE

  INTEGER*1                                      \
\           lineFeeds,                           \ number of print line feeds
            REDEEMED.PTS                         ! flag if coupon is a redeemed
                                                 ! points coupon

  STRING                                         \
         DEBUG.TOPSTRING$,                       \ built at beginning of event
         PT.TYPE$,                               \ type of points
         JAVA.TARGETCALL$,                       \ used to hold the value of all
                                                 \ targeted coupons.
         JAVA.TARGETC$                           ! used to hold the value of a
                                                 ! targeted coupon.
  INTEGER*2                                      \
             baseLength,                         \ @tal10012003 - length variable
             event, saveEvent,                   \
             crRetrieveFormat, I2C%,             \ Cash Receipt Retrieve Format, work var.  AIO21731
             I2A%,I2B%                           ! work variables
  STRING                                         \
             token$, javawork1, marker$,         \ made 'data$' global IO11817
             work1$,                             \ work var AIO21731
             with$                               !
  INTEGER*4                                      \
            fsOver,                              \ Excess foodstamp tender
            JAVA.COUPONINDEX,                    \
            JAVA.MAXCOUPONS,                     \
            fsChange,                            \ Foodstamp coin change due (.01-.99)
            marker%,                             \
            fsBalance,                           \
            I4A%,                                \ D2523
            aefl%

  !A6540
  !SL.IT.ITEMCODE is set after the call to TSIEEC03 for EAN99, so save
  STRING                                         \
             javaItemCode$                       !
  STRING                                         \
             element$                            !
  STRING                                         \
             printLine.temp$                     !
  !E6540
  STRING     itemType$                           ! IO08970

!AIO11370 Disable interrupts so that this function cannot be
!         interrupted part way through, and called recursively
!         due to an async error from the printer.
  aefl% = DISABLE
!EIO11370
  SI.data$ = ""       ! GLOBAL, we now need to initialize IO11817
  saveEvent = event

!AIO11817 Trace important variables in a global string
  SI.javaaefr.trace$ = SI.javaaefr.trace$ +     \
    "|e=" + STR$(event)                         ! a new event starts here
  IF (TS.LINETYPE <> 0) THEN BEGIN              ! only if a value
    SI.javaaefr.trace$ = SI.javaaefr.trace$ +   \ add it to trace
      " t=" + STR$(TS.LINETYPE)                 !
  ENDIF
  IF (TS.LINEDATA <> 0) THEN BEGIN              ! only if a value
    SI.javaaefr.trace$ = SI.javaaefr.trace$ +   \ add it to trace
      " d=" + STR$(TS.LINEDATA)                 !
  ENDIF

  IF SI.javaaefr.trace.max = 0 THEN BEGIN
    SI.javaaefr.trace.max = 2000                ! max string size
  ENDIF
  SI.javaaefr.trace$ = RIGHT$(                  \ truncate to
      SI.javaaefr.trace$,SI.javaaefr.trace.max) ! a reasonable size
!EIO11817

!AD21508 This block is for debugging only
\  IF (JAVA.DEBUG.UNITNUM = 0) THEN BEGIN
\    JAVA.DEBUG.UNITNUM=92                      ! uncomment for debugging
\    CREATE "R::C:/SILOGS/DTRC.GGK" AS JAVA.DEBUG.UNITNUM ! uncomment for debugging
\  ENDIF
\
\  IF (JAVA.DEBUG.UNITNUM <> 0) THEN BEGIN      \
\    DEBUG.TOPSTRING$ = TIME$ +       \         \
\                ". AEF terminalSubStateMsg." + \
\                " event="+STR$(EVENT) +        \
\                " LT="+STR$(TS.LINETYPE) +     \
\                " LD="+STR$(TS.LINEDATA) +     \
\                " PR="+STR$(TS.PROCEDURE) +    \  D25022
\                " jGuiSubState="+str$(jGuiSubState)
\  ENDIF
!ED21508

  !set a flag if not sending the unit of work message so we'll know when
  !some event has occurred when the system has finished current processing
  IF (event <> aefUnitOfWorkMsg) AND             \ not unit of work message
     (event <> updateStatusMsg) AND              \
     (event <> updateScaleMsg)  THEN BEGIN       ! not updating status message
    aefUnitOfWorkFlag = -1                       ! flag we've done some work
  ENDIF

!call TraceRh("Evento en javaaefr" + str$(event))
  ON event GOSUB                                 \
     label.receiptEvent,                         \ 1 update receipt
     label.noop,                                 \ 2 no op old updateTotalsMsg is na
     label.updateScaleMsg,                       \ 3 Scale Window Needs Update
     label.addAccountTenderMsg,                  \ 4
     label.addDiscountEntryMsg,                  \ 5
     label.addItemEntryMsg,                      \ 6
     label.addTenderEntryMsg,                    \ 7 Tender Taken
     label.changeGivenMsg,                       \ 8
     label.endAccountingTransactionMsg,          \ 9
     label.endSalesTransactionMsg,               \ 10
     label.endTransactionEvent,                  \ 11 End Trx
     label.signoffEvent,                         \ 12
     label.signonEvent,                          \ 13
     label.optionsLoadedMsg,                     \ 14
     label.priceVerifyMsg,                       \ 15
     label.procedureCompletedMsg,                \ 16
     label.procedureStartedMsg,                  \ 17
     label.startAccountingTransactionMsg,        \ 18
     label.startSalesTransactionMsg,             \ 19
     label.startTransactionEvent,                \ 20 Start Trx
     label.reportOutputMsg,                      \ 21
     label.scrollMsg,                            \ 22
     label.displayVelocityCodesMsg,              \ 23
     label.hideVelocityCodesMsg,                 \ 24
     label.redrawReceiptWindowMsg,               \ 25
     label.updateStatusMsg,                      \ 26 Update of status Bar
     label.customerEvent,                        \ 27 Customer Card Scanned
     label.noop,                                 \ 28 Java "keys" used is na
     label.add2x20Msg,                           \ 29
     label.terminalSubStateMsg,                  \ 30
     label.optionsLoadingStartedMsg,             \ 31
     label.optionsLoadingEndedMsg,               \ 32
     label.optionsDataMsg,                       \ 33
     label.itemEvent,                            \ 34
     label.voidTransactionMsg,                   \ 35
     label.foodStampTotalMsg,                    \ 36
     label.foreignTotalMsg,                      \ 37
     label.terminalConnectionChangeMsg,          \ 38
     label.otrMsg,                               \ 39
     label.discountJustAddedMsg,                 \ 40
     label.operatorSpecialSignedOnMsg,           \ 41
     label.transactionTotalsEvent,               \ 42
     label.aefEMredemptionCpnMsg,                \ 43
     label.aefUnitOfWorkMsg,                     \ 44
     label.operatorSpecialSignedOffMsg,          \ 45
     label.procedureStartedMsg2,                 \ 46
     label.operatorDisplay,                      \ 47
     label.customerDisplay,                      \ 48
     label.noSaleStartMsg,                       \ 49
     label.noSaleEndMsg,                         \ 50
     label.tillExchangeOccurred,                 \ 51 D2424/17667
     label.transactionUpdate,                    \ 52 IO08970
     label.tenderDeclined,                       \ 53 IO08970
     label.aefUserEvent,                         \ 54 IO12917
     label.aefUserNuoEvent,                      \ 55  
     label.jGuiEndAppEvents                      !


   CALL javaGuiAppTrace(javaTraceUser$)        ! trace the user
   CALL javaAEFUserExit(event, SI.data$)       ! Call the user exit
   CALL javaGuiAppTrace(javaTraceApp$)         ! Trace back

   IF (event >= 0 AND LEN(SI.data$) > 0) THEN BEGIN                              ! abort NOT requested
      IF javaGuiClass$ = "" THEN                                    \ IO21722 only if not pre-set
        javaGuiClass$ = "com.ibm.retail.AEF.data.ApplicationDataConnectorImpl"  ! set the class
      javaGuiMethod$ = "processEvent"                                         ! set the method
      CALL transmitToJVM(event, SI.data$, SI.data$, 2)
   ENDIF

!AD21508 This block is for debugging only
\  IF (JAVA.DEBUG.UNITNUM <> 0) THEN BEGIN
\    IF (SI.DATA$ <> "") AND                   \ something there
\       (LEFT$(SI.DATA$,12) <> "<scale scale") \ not a scale read command
\    THEN BEGIN                                ! trace this event results
\      WRITE #JAVA.DEBUG.UNITNUM; ""               ! uncomment for debugging
\      WRITE #JAVA.DEBUG.UNITNUM; DEBUG.TOPSTRING$ ! uncomment for debugging
\      WRITE #JAVA.DEBUG.UNITNUM;                  \ uncomment for debugging
\            " xml={" + SI.data$ + "}"
\    ENDIF
\  ENDIF
!ED21508
!AIO11817 Trace part of the generated XML
  SI.javaaefr.trace$ = SI.javaaefr.trace$ +    \ add to trace
    "[" +                                      \ delimiter
    STR$(LEN(SI.data$)) + "=" +                \ length of XML
    LEFT$(SI.data$, 10)                        ! first part of XML
!EIO11817

!AIO11370 Allow interrupts to occur again.
!         Printer errors, if any, will be reported NOW.
   aefl% = ENABLE
!EIO11370

EXIT SUB

!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
label.receiptEvent:                              ! Receipt Message

!10,<cashReceipt linefeeds=&1 paperCut=&2 receiptCompleted=&3>
!11,</cashReceipt>
!AIO19200
  IF EP.ETO.DEFAULT.CR.WIDTH  > 38 THEN BEGIN     ! Default receipt line length.
    sav.prtbuf$ = TS.PRTBUF$                      ! Save copy
    TS.PRTBUF$ = EP.WIDE.PRTBUF$                  ! Use Wide instead of regular
  ENDIF 
!EIO19200

!AIO21731    
  ! Handle unique retrieve format.
  crRetrieveFormat = 0    ! Retreive format type: 0=no retrieve, 1=msg header, 2=printLine.
  IF ( TS.LINETYPE = 31 ) AND ( TS.RETV.IN.PROGRESS = -1 ) THEN BEGIN  
    crRetrieveFormat = 1
  ENDIF
!EIO21731    
        
    eventString$ = ?aefXML$(10)                   ! <cashReceipt>
    element$ = ?GetXMLEvent$(eventString$)
    IF LEN(element$) THEN \
    BEGIN
      CALL RemoveConditionalTags(eventString$)
      marker$ = ?FindMarker$(eventString$)        ! marker = &##... where #=number
      aefl% = LEN(marker$)
      WHILE aefl% <> 0                               ! loop until no more markers
        marker% = val(mid$(marker$,2,aefl%))         ! marker value
        with$ = ?InQuote$("")
        ON marker% GOSUB lineFeedsSub,                \ 1
                         paperCutSub,                 \ 2
                         receiptCompletedSub          ! 3 D2424/18127
        CALL SubstituteMarker(marker$, eventString$, with$)
        marker$ = ?FindMarker$(eventString$)      ! next marker &##...
        aefl% = LEN(marker$)
      WEND
      SI.data$ = SI.data$ + eventString$

!AIO21731    
      IF ( crRetrieveFormat = 1 ) THEN BEGIN 
        GOSUB retrieveFormatSub      
      ENDIF
!EIO21731    

      eventString$ = ?aefXML$(52)                 ! <printLine>
      element$ = ?GetXMLEvent$(eventString$)
      IF LEN(element$) THEN \
      BEGIN
!AIO08970
        IF (JAVA.SIGNATURE.LINE <> 0) THEN BEGIN   ! if this is a blank signature line
          CALL AllowConditionalTag(1,eventString$) ! include signatureLine="true" attribute.
        ENDIF
!EIO08970

        CALL RemoveConditionalTags(eventString$)
        marker$ = ?FindMarker$(eventString$)      ! Find the marker for the rawData attribute
        IF LEN(marker$) > 0 THEN BEGIN
           with$ = ?InQuote$(?FormatForXML$(TS.PRTBUF$))
           printLine.rawData$ = with$                 ! 6540
           CALL SubstituteMarker(marker$, eventString$, with$)  ! Insert the rawData
        ENDIF

        marker$ = ?FindMarker$(eventString$)      ! Find the marker for the filteredData attribute
        IF LEN(marker$) > 0 THEN BEGIN
           with$ = ?InQuote$(?FormatForXML$(TS.PRTBUF$))
           IF (prt4610.enable) THEN BEGIN                 ! 4610 enabled
             ! strip the control chars
             printLine.temp$ = ?FormatForXML$(?StripControlChar$(STRIP.PRT4610$(TS.PRTBUF$)))
             with$ = ?InQuote$(printline.temp$)
           ENDIF ELSE BEGIN                               ! not 4610
             IF (mod3.enable) THEN BEGIN                  ! Model 3 printer
               ! Remove control chars
               printLine.temp$ = ?FormatForXML$(?StripControlChar$(STRIP.MOD3$(TS.PRTBUF$)))
               with$ = ?InQuote$(printLine.temp$)
             ENDIF ELSE BEGIN                             ! model 2 printer
               ! pass the data "as is"
               printLine.temp$ = ?FormatForXML$(?StripControlChar$(TS.PRTBUF$))
               with$ = ?InQuote$(printLine.temp$)
             ENDIF                                        ! model 3 printer?
           ENDIF                                          ! 4610 enabled?
           CALL SubstituteMarker(marker$, eventString$, with$)  ! Insert the filteredData
        ENDIF
        CALL ContextSensitivePrintLines(eventString$)  ! 6540

!AIO21731    
        IF ( crRetrieveFormat = 2 ) THEN BEGIN
          GOSUB retrieveFormatSub
        ENDIF ELSE BEGIN 
!EIO21731    
          SI.data$ = SI.data$ + eventString$
        ENDIF                                              !AIO21731
      ENDIF
      SI.data$ = SI.data$ + ?aefXML$(11)

    ENDIF

!AIO08970 SA sends the header lines before the transaction actually starts.
! We'll save the header lines until the transaction starts, then send them.
!AIO16905
    !IF (HEADER.LINE <> 0) THEN BEGIN   ! Original
    IF (HEADER.LINE <> 0)  OR \
       ( (TS.PROCEDURE = 0 OR TS.PROCEDURE = -1) AND  \
         (transStarted = 0) AND TS.LINETYPE <> 6 AND TS.LINETYPE <> 8 AND TS.LINETYPE <> 11 ) THEN BEGIN
!EIO16905

      HEADER.STR$ = HEADER.STR$ + SI.data$
      SI.data$ = ""
!AIO11817 We will abend if we try to save more than 64 KB in a string.
!         To prevent the need to add BIGLEN to the INP file, we stop
!         at 32 KB.  When almost at 32 KB, put out the saved data and
!         then continue to buffer again.
!         32 KB is approximately 90 - 120 print lines.
      IF (LEN(HEADER.STR$) > 32000) THEN BEGIN ! 767 bytes to spare
        SI.data$ = HEADER.STR$                 ! put it out now
        HEADER.STR$ = ""                       ! nothing buffered now
      ENDIF
!EIO11817
      HEADER.LINE = 0
    ENDIF ELSE BEGIN
!AD17710 Always send the totalSavings field, even if it is zero.
!     HEADER.STR$ = ""
      IF (NONSALES.LINE = 0) THEN BEGIN
         ! Clear out the buffered header print lines only
         ! if we receive a line that is not a header line
         ! and not a nonsales line.
         HEADER.STR$ = ""
      ENDIF
!ED17710
    ENDIF
!EIO08970
    IF LEN(SI.data$) = 0 THEN BEGIN
       event = -1
    ENDIF

    IF (TS.LINETYPE = 1) OR                        \
       (TS.LINETYPE = 5) THEN BEGIN                ! item sale print
!AD21508 Always save each linetype 1 and 5 string in the print array
\     IF (TS.LINETYPE = 5) OR                      \
\        (RIGHT$(printLine.temp$,4) = " ") THEN BEGIN  ! header line or blank
\       JAVA.ITEM.HDR.LINE$ = printLine.temp$
\       JAVA.ITEM.HDR.LINE.RAW$ = ?FormatForXML$(TS.PRTBUF$)
\     ENDIF ELSE BEGIN
\       JAVA.ITEM.PRT.LINE$ = printLine.temp$
\       JAVA.ITEM.PRT.LINE.RAW$ = ?FormatForXML$(TS.PRTBUF$)
        IF NUM.PRT.LINES < JAVA.ITEM.PRT.MAX THEN BEGIN ! do not blow the array
          NUM.PRT.LINES = NUM.PRT.LINES + 1        ! one more line is saved
        ENDIF
        JAVA.ITEM.PRT.LINE$(NUM.PRT.LINES) = printLine.temp$
        JAVA.ITEM.PRT.LINE.RAW$(NUM.PRT.LINES) = ?FormatForXML$(TS.PRTBUF$)
\     ENDIF                                        ! header line or blank
!ED21508
    ENDIF ELSE BEGIN                               ! else not item line print
!AD21508 Zero a counter instead of nulling the strings
\     JAVA.ITEM.HDR.LINE$ = ""                     ! clear print variables
\     JAVA.ITEM.HDR.LINE.RAW$ = ""
\     JAVA.ITEM.PRT.LINE$ = ""
\     JAVA.ITEM.PRT.LINE.RAW$ = ""
      NUM.PRT.LINES=0                              ! no saved lines now
!ED21508
    ENDIF                                          ! item sale print

!AIO19200                                                                       
    CALL label.receipt.setup.java(printLine.temp$)
!eIO19200                                                                       

    printLine.temp$ = ""                           ! cleanup

    marker$ = ""
    eventString$ = ""
    with$ = ""
!AIO19200                                                                       
    IF EP.ETO.DEFAULT.CR.WIDTH  > 38 THEN BEGIN    ! receipt line length
      TS.PRTBUF$ = sav.prtbuf$                     ! restore saved prtbuf
      sav.prtbuf$ = ""                             ! reset to nullname
    ENDIF
!EIO19200
  RETURN                                           ! return to caller


!AIO21731    
! Called only by "label.receiptEvent:" when a retrieve is in process.
! Replaces the original cashReceipt event data created when retrieving a transaction.
! Which part of the XML message is editted is based upon the value of crRetrieveFormat.
retrieveFormatSub:

  ! Edit msg [<cashReceipt ... > section.
  IF ( crRetrieveFormat = 1 ) THEN BEGIN

    ! Append "GUIIgnore" to cashReceipt.  Perform first so the deletes below have a " " character to search.
    ! ACE GUIIgnore = false to indicate scrolling receipt cash receipt lines for SI GUI.
    ! Also the default value in the ISDefaultACEAEF::overrideGUIIgnoreFlag() is false for base, so set it false.
    eventString$ = MID$(eventString$, 1, LEN(eventString$) - 1  )                     ! Remove end ">" character.
    eventString$ = eventString$ + " GUIIgnore="+ CHR$(34) + "false" + CHR$(34) + ">"  ! Add GUIIgnore.
    
    ! Delete  "paperCut=" 
    I2A% = MATCH( "paperCut=", eventString$, 1) - 1   ! Get start of "paperCut=".         
    I2B% = MATCH( " ", eventString$, (I2A%+1) )       ! Get end of it.
    I2C% = LEN( eventString$ ) - I2B% + 1             ! Len of new tail of eventString.        
    work1$ = MID$( eventString$, 1, I2A% )            ! Save from 1 to start of papercut.
    with$ = MID$( eventString$, (I2B%+1), I2C%)       ! Save from end of papercut to end of eventString$.
    eventString$ = work1$ + with$                     ! Rebuild eventString$

    ! Delete  "receiptCompleted="
    I2A% = MATCH( "receiptCompleted=", eventString$, 1) - 1 ! Get start of "receiptCompleted=".          
    I2B% = MATCH( " ", eventString$, (I2A%+1) )       ! Get end of it.
    I2C% = LEN( eventString$ ) - I2B% + 1             ! Len of new tail of eventString.  
    work1$ = MID$( eventString$, 1, I2A% )            ! Save from 1 to start of receiptCompleted.
    with$ = MID$( eventString$, (I2B%+1), I2C%)       ! Save from end of receiptCompleted to end of eventString$.
    eventString$ = work1$ + with$                     ! Rebuild eventString$.

    SI.data$ = eventString$                           ! Overwrite original header.
      
    crRetrieveFormat = 2       ! Setup for next call.
    
  ! Edit msg <printLine ...  /></cashReceipt>] section.
  ! NOTE: An attribute inside printLine does not have to be in a specific order.
  ! However, it should only occur once.  If it occurs twice the last one wins.
  ENDIF ELSE IF ( crRetrieveFormat = 2 ) THEN BEGIN

    ! Msg printLine's header section.
    
    ! Get to the end of the eventString$ setup in crRetrieveFormat = 2 above.
    I2A% = MATCH ( ">",eventString$, 1)
    
    eventString$ = MID$(eventString$, 1, I2A%-2 )     ! Remove the original end tag "/>".

    ! Remove the chunk from between original rawData and lineType.
    I2A% = MATCH( "rawData", eventString$, 1) - 1     ! Get start of "rawData"
    I2B% = MATCH( "lineType", eventString$, 1)        ! Get start of "lineType".  
    I2C% = LEN( eventString$ ) - I2B% + 1             ! Len of new tail of eventString.      
    work1$ = MID$( eventString$, 1, I2A% )            ! Save from 1 to start of rawData.
    with$ = MID$( eventString$, I2B%, I2C% )          ! Save from lineType to end of eventString$.
    eventString$ = work1$ + with$                     ! Rebuild eventString$.
    
    ! Determine signatureLine.
    work1$ = "false"
    IF (JAVA.SIGNATURE.LINE <> 0 ) THEN BEGIN
      work1$ = "true"
    ENDIF

    ! Append printLine
    eventString$ = eventString$ + " signatureLine=" + CHR$(34) + work1$ + CHR$(34)
    SI.data$ = SI.data$ + eventString$                ! Append printLine.

    ! Msg filteredData section.    
    eventString$ =  " filteredData=" + CHR$(34) + printLine.temp$ + CHR$(34)
    SI.data$ = SI.data$ + eventString$                ! Append filteredData.
  
    ! Msg rawdData section.    
    eventString$ =  " rawData=" + CHR$(34) + "[\x1B][\x61][\x01][\x1B][\x21][\x00]" + printLine.temp$ + "[\x0A]" + CHR$(34) + " />"
    SI.data$ = SI.data$ + eventString$                ! Append rawData.


    ! Cleanup.  
    work1$ = ""
    with$ = ""
    crRetrieveFormat = 0                              ! All done.
    
  ENDIF
  
  RETURN
!EIO21731    


lineFeedsSub:
   ! Set up the linefeeds.  Even if this is a 4610, variable TS.FORMCR$ is still
   ! set up for linefeeds
   I2A% = 1                                       ! assume 1 linefeed
   I2B% = MATCHB("A", TS.FORMCR$, 1)              ! Locate char
   IF (I2B% > 0) THEN BEGIN                       ! Char found
     I2A% = VAL(RIGHT$(TS.FORMCR$,LEN(TS.FORMCR$) - I2B%))
     IF (I2A% < 1) THEN BEGIN                     ! if invalid data
       I2A% = 1                                   ! adjust to one
     ENDIF                                        ! if invalid data
   ENDIF                                          ! Char found

   with$ = ?InQuote$(STR$(I2A%))
   RETURN

paperCutSub:
   ! Set up if a paper cut is with this line
   I2A% = 0                                       ! assume not a paper cut
   IF GUIL.ENABLE THEN BEGIN                      ! if guillotine configured
     IF TO.GUILLOTINE THEN BEGIN                  ! if guillotine option on
       IF PRT4610.ENABLE THEN BEGIN               ! if 4610 printer
         ! if linecut characters found
         IF MATCH(CHR$(0CH) + CHR$(0AH), TS.PRTBUF$,1) THEN BEGIN
           I2A% = -1                              ! flag it
         ENDIF                                    ! if linecut characters found
       ENDIF ELSE BEGIN                           ! else not 4610 printer
         ! if linecut characters found
         IF MATCH(CHR$(27) + CHR$(80), TS.PRTBUF$,1) THEN BEGIN
           I2A% = -1                              ! flag it
         ENDIF                                    ! if linecut characters found
       ENDIF                                      ! if 4610 printer
     ENDIF                                        ! if guillotine option on
   ENDIF                                          ! if guillotine configured

   with$ = ?TrueOrFalse$(I2A%)  ! papercut?

   RETURN

!AD2424/18127
receiptCompletedSub:
   ! Set up end of receipt.  This can be true only when receiptCompleted
   ! is TRUE (set in EAMTSPRC).

   with$ = ?TrueOrFalse$(receiptCompleted)        ! end of transaction

   RETURN
!ED2424


label.noop:                                      ! Events not applicable to aef

  ! Equivalent event does not exist yet
  event=-1
  RETURN                                         ! return to caller

label.transactionTotalsEvent:

    CALL tstxec01                                ! Calculate tax
!72,<transactionTotals total=&1 tax=&2 subTotal=&3 balanceDue=&4
!73, foodstampTotal=&5 foodstampBalance=&6 foodstampChange=&7 changeDue=&8
!74, totalItems=&9 [(1)totalSavings=&10] totalCouponCount=&11
!75, totalCouponAmount=&12 tenderExchanged=&13 />
    eventString$ = ?aefXML$(72) + \              ! transactionTotals
                   ?aefXML$(73) + \
                   ?aefXML$(74) + \
                   ?aefXML$(75)
    element$ = ?GetXMLEvent$(eventString$)
    IF LEN(element$) THEN \
    BEGIN
!AD18204 Weight Entry Line Not Printed In Red When Item Voided
\     IF (EMSS.TR.PCCPNAMT <> 0) THEN BEGIN       ! if a loyalty savings
!ED18204
      CALL AllowConditionalTag(1,eventString$) ! include totalSavings attribute.
!AD18204
\     ENDIF
!ED18204
      IF(MC.ID$ <> "") THEN BEGIN
         CALL AllowConditionalTag(2,eventString$) ! include foreignBalanceDue attribute
         CALL AllowConditionalTag(3,eventString$) ! include foreignCurrency attribute
      ENDIF
      CALL RemoveConditionalTags(eventString$)
      marker$ = ?FindMarker$(eventString$)        ! marker = &##... where #=number
      aefl% = LEN(marker$)
      WHILE aefl% <> 0                               ! loop until no more markers
        marker% = val(mid$(marker$,2,aefl%))         ! marker value
        with$ = ?InQuote$("")
        ON marker% GOSUB tranTotalSub, tranTaxSub,    \
                         tranSubTotalSub, balDueSub,  \
                         fsTotalSub, fsBalanceSub,    \
                         fsChangeSub, changeDueSub,   \
                         itemsSub, savingsSub,        \
                         numCouponsSub, couponAmtSub, \
                         forBalSub, forCurrSub,       \
                         tndrExchgSub
        CALL SubstituteMarker(marker$, eventString$, with$)
        marker$ = ?FindMarker$(eventString$)      ! next marker &##...
        aefl% = LEN(marker$)
      WEND
      SI.data$ = SI.data$ + eventString$
    ENDIF ELSE \
    BEGIN
       event = -1
    ENDIF

    marker$ = ""
    eventString$ = ""
    with$ = ""
  RETURN                                         ! return to caller




tranTotalSub:
   ! transaction total (includes tax)
   ! format the trans total incl taxes
   CALL java.format.amount(ts.taxes(0,0) + ts.totals(0,0,0))
   javaWork1 = ?RemoveCharacter$(" ",JAVA.AMT$)
   with$ = ?InQuote$(javaWork1)
   RETURN

tranTaxSub:
   ! transaction tax
   CALL java.format.amount(ts.taxes(0,0))
   javaWork1 = ?RemoveCharacter$(" ",JAVA.AMT$)
   with$ = ?InQuote$(javaWork1)
   RETURN

tranSubTotalSub:
   ! transaction subtotal
   CALL java.format.amount(ts.totals(0,0,0))
   javaWork1 = ?RemoveCharacter$(" ",JAVA.AMT$)
   with$ = ?InQuote$(javaWork1)
   RETURN

balDueSub:
   ! Amount (balance) due
   fsOver = 0                                     ! reset excess foodstamp
   fsChange = 0                                   ! reset fstamp change (.01 to .99)
   IF (TS.FS.TENDERED) THEN BEGIN                 ! food stamps tendered
 !   calculate any over payment and change due( < 1.00) in foodstamps
     fsOver = TS.TENDERED(3) - TS.TOTALS(0,1,0)   ! over tendered if positive

     IF ((fsover < 0) OR                          \ if not excessive foodstamp tender
         (TO.FOODSTAMPS = 0)) THEN BEGIN          ! or foodstamps not in use
       fsover = 0                                 ! then reset fs tender overage
     ENDIF                                        ! not excessive fs tdr/fs not in use

     ! if overtendered will equal coin chg
     fschange = MOD(fsover, 100)
     IF (TO.EBT) THEN BEGIN                       ! if coin change ok then
      fschange = 0                                ! reset coin change
     ENDIF                                        ! ebt active
   ENDIF                                          ! foodstamps tendered

   IF ((TS.FS.TENDERED) AND                       \ fs tndred and
       (TS.TRX.STATUS = -2))  THEN BEGIN          ! stat = fs tender
     CALL java.format.amount(TS.BALDUE(0))        !
   ENDIF ELSE BEGIN                               ! actual total
     CALL java.format.amount(TS.TAXES(0,0)  +     \ current grand
                        TS.TOTALS(0,0,0) -        \
                        (TS.TENDERED(0)  -        \
                         fsover          +        \
                         fschange))               ! total

   ENDIF                                          ! end other tndr

   javaWork1 = ?RemoveCharacter$(" ",JAVA.AMT$)
   with$ = ?InQuote$(javaWork1)
   RETURN

fsTotalSub:
   ! FS total
   CALL java.format.amount(TS.TOTALS(0,1,0))           ! format the FS totals
   with$ = ?InQuote$(?RemoveCharacter$(" ",JAVA.AMT$))
   RETURN

fsBalanceSub:
   fsChange = 0                                   ! reset fstamp change (.01 to .99)
   fsBalance = TS.TOTALS(0,1,0) - TS.TENDERED(3)  ! over tendered if positive
   IF (fsBalance < 0) THEN BEGIN
      fsBalance = 0
   ENDIF
   CALL JAVA.FORMAT.AMOUNT(fsBalance)
   ! save the string result but strip blanks
   javaWork1 = ?RemoveCharacter$(" ",JAVA.AMT$)
   IF (RIGHT$(javaWork1,1) = "-") THEN BEGIN      ! if a negative value
     javaWork1 = "-" + LEFT$(javaWork1,LEN(javaWork1) - 1)  ! move negative sign to front
   ENDIF                                          ! if a negative value
   with$ = ?InQuote$(javaWork1)
   RETURN

fsChangeSub:
   fschange = 0                                   ! reset fstamp change (.01 to .99)
   IF (TS.FS.TENDERED) THEN BEGIN                 ! food stamps tendered
 !   calculate any over payment and change due( < 1.00) in foodstamps
     fschange = TS.TENDERED(3) - TS.TOTALS(0,1,0) !

     IF ((fschange < 0) OR                        \ if not excessive foodstamp tender
         (TO.FOODSTAMPS = 0)) THEN BEGIN          ! or foodstamps not in use
       fschange = 0                               ! then reset fs tender overage
     ENDIF                                        ! not excessive fs tdr/fs not in use

     IF (TO.EBT AND                               \
         (TO.TENDOPTS(TS.TDR.INDEX, 7) > 2)) THEN \
     BEGIN                                        ! if EBT foodstamps, then
      fschange = 0                                ! no foodstamp change.
     ENDIF
   ENDIF
   CALL java.format.amount(fschange)
   with$ = ?InQuote$(?RemoveCharacter$(" ",JAVA.AMT$))
   RETURN

changeDueSub:
   ! handle the change due field if end of transaction
   CALL JAVA.FORMAT.AMOUNT(0)
   javaWork1 = ?RemoveCharacter$(" ", JAVA.AMT$)
   with$ = ?InQuote$(javaWork1)
   IF (TS.BALDUE(0) <= 0) AND                     \ if end of transaction
      (TS.TENDERED(0) <> 0) THEN BEGIN
     CALL JAVA.FORMAT.AMOUNT(TS.BALDUE(0))        ! get the change due
     ! save the string result but strip blanks
     javaWork1 = ?RemoveCharacter$(" ",JAVA.AMT$)
     IF (RIGHT$(javaWork1,1) = "-") THEN BEGIN    ! if a negative value
       javaWork1 = LEFT$(javaWork1,LEN(javaWork1) - 1)  ! strip negative sign at end
     ENDIF                                        ! if a negative value
     with$ = ?InQuote$(javaWork1)
   ENDIF                                          ! if end of transaction
   RETURN

itemsSub:
   ! total items
     IF (TS.CENTSEP$ = " ") THEN BEGIN            ! if no cents,
       CALL java.format.amount(ITEM.COUNT)        ! format the count as an amount
       JAVA.AMT$ = LEFT$(JAVA.AMT$,               \ remove trailing space
                        LEN(JAVA.AMT$) - 1)       !
     ENDIF ELSE BEGIN                             ! if cents
!AD2523 ITEM.COUNT is I*2, and the expression will overflow if over 327 items.
\      CALL java.format.amount(ITEM.COUNT * 100)  ! format the count * 100 as amount
       I4A% = ITEM.COUNT                          ! copy the count to I*4 variable
       CALL java.format.amount(I4A% * 100)        ! format the count * 100 as amount
!ED2523
       JAVA.AMT$ = LEFT$(JAVA.AMT$,               \ and truncate the ".00 "
                        LEN(JAVA.AMT$) - 4)       !
       IF (JAVA.AMT$ = " ") THEN BEGIN            ! No count
         JAVA.AMT$ = "0"                          ! set to zero
       ENDIF                                      ! No count
     ENDIF                                        ! cents in system?
     ! save the string result but strip blanks
     javaWork1 = ?RemoveCharacter$(" ",JAVA.AMT$)
     IF (RIGHT$(javaWork1,1) = "-") THEN BEGIN    ! if a negative value
       javaWork1 = "-" + javaWork1                ! add negative sign to beginning
       javaWork1 = LEFT$(javaWork1,LEN(javaWork1) - 1)  ! strip negative sign at end
     ENDIF                                        ! if a negative value
     with$ = ?InQuote$(javawork1)
   RETURN

savingsSub:
   ! loyalty savings
!AD18204 Weight Entry Line Not Printed In Red When Item Voided
\  with$ = ?InQuote$("")
\  IF (EMSS.TR.PCCPNAMT <> 0) THEN BEGIN          ! if a loyalty savings
   with$ = ?InQuote$("0")
!ED18204
   CALL java.format.amount(EMSS.TR.PCCPNAMT)    ! format the loyalty savings
   with$ = ?InQuote$(?RemoveCharacter$(" ",JAVA.AMT$))
!AD18204
\  ENDIF                                          ! if a loyalty savings
!ED18204
   RETURN

numCouponsSub:
   ! coupon quantity
   with$ = ?InQuote$(STR$(TS.COUP.QTY))
   RETURN

couponAmtSub:
   ! coupon amount
   CALL java.format.amount(TS.COUP.AMT)           ! format the coupon amount
   with$ = ?InQuote$(?RemoveCharacter$(" ",JAVA.AMT$))
   RETURN

forBalSub:
   ! foreignBalanceDue
   CALL JAVA.FC.FORMAT.AMOUNT(FC.BALDUE, JAVA.SLOT)
   with$ = ?InQuote$(?RemoveCharacter$(" ", JAVA.AMT$))
   RETURN

forCurrSub:
   ! foreignCurrency
   with$ = ?InQuote$(MC.ID$)
   RETURN

tndrExchgSub:
   !XXX Tender Exchanged -- is this correct for all tender types?
   !XXX
   CALL java.format.amount(TS.TENDERED(0))
   with$ = ?InQuote$(?RemoveCharacter$(" ",JAVA.AMT$))
   RETURN


!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
label.updateScaleMsg:                             ! Scale Message

!144,<scale scaleWeightValue=&1 scaleWeightUnit=&2 scaleWeightLabels=&3 />
    eventString$ = ?aefXML$(144)                  ! <scale>
    element$ = ?GetXMLEvent$(eventString$)
    IF LEN(element$) THEN \
    BEGIN
      CALL RemoveConditionalTags(eventString$)
      marker$ = ?FindMarker$(eventString$)        ! marker = &##... where #=number
      aefl% = LEN(marker$)
      WHILE aefl% <> 0                               ! loop until no more markers
        marker% = val(mid$(marker$,2,aefl%))         ! marker value
        with$ = ?InQuote$("")
        ON marker% GOSUB scaleValSub,                 \ 1
                         scaleUnitSub,                \ 2
                         scaleLabelSub                ! 3
        CALL SubstituteMarker(marker$, eventString$, with$)
        marker$ = ?FindMarker$(eventString$)      ! next marker &##...
        aefl% = LEN(marker$)
      WEND
      SI.data$ = SI.data$ + eventString$
    ENDIF

    IF LEN(SI.data$) = 0 THEN \
    BEGIN
       event = -1
    ENDIF

    if (eventString$ = javaLastScale) then begin  ! same as last time
      event = -1                                  ! do not send it
    endif                                         ! same as last time

    javaLastScale = eventString$                  ! save for comparison


    marker$ = ""
    eventString$ = ""
    with$ = ""
  RETURN                                          ! return to caller




scaleValSub:

!AIO18536
   if ( scale.not.available <> 0 )  then begin     !  No scale ( -1 ) or no scale and retrying ( 2 ).

!  if (ts.errf = 39) and                     \ error on scale
!      ((ts.ts11werr$ = "FU") or             \ device not open or
!       (ts.ts11werr$ = "DO"))               ! offline
!EIO18536
       if (ts.in.ipl) then begin
          with$ = ?InQuote$(?FormatForXML$(javaScaleOtherErr$))
       endif else begin
          with$ = ?InQuote$(?FormatForXML$(javaNoScale$))
       endif
   endif else \                              ! error reading scale
   begin
   !A3458 For IR51722 / IR51405
   !      IR51722 Formats -2 as zero weight
   !      IR51405 Implements java.format.weight
     if (ts.temp1i4 >=0) or                  \ valid weight
        (ts.temp1i4 = -2) then begin         ! zero, but manual weight prohibited
   ! If -2, the scale was read as zero.  Display zero.
       if (ts.temp1i4 = -2) then begin       ! zero, but manual weight prohibited
         call java.format.weight(0)          ! format the weight as zero
       endif else begin                      ! else a valid weight
         call java.format.weight(ts.temp1i4) ! format the weight
       endif                                 !
       javaWork2$ = java.wgt$                ! get the string
     endif else begin                        ! not valid read
       if (javaScaleErrn =                   \
                       80A30563h) then begin ! scale in motion
         javaWork2$ = javaScaleMotionErr$    ! Set scale in motion error
       endif else begin                      ! other error from scale
         javaWork2$ = javaScaleOtherErr$     ! set other error text
       endif                                 ! scale error
     endif                                   ! not valid read
   !E3458 For IR51722 / IR51405

   !A3458 AIR51702 If weight is now zero, reset the scale non-zero warning flag
     if (ts.temp1i4 = 0) then begin          ! scale is currently zero
       warn.scale.nonzero = 0                ! reset the warning now
     endif
   !E3458 EIR51702
     with$ = ?InQuote$(javaWork2$)
   endif
   RETURN

scaleUnitSub:
   if (ts.errf = 39) and                   \ error on scale
      ((ts.ts11werr$ = "FU") or            \ device not open or
       (ts.ts11werr$ = "DO")) then begin   ! offline
     with$ = ?InQuote$("")
   endif else \                            ! error reading scale
   begin
     with$ = ?InQuote$(?FormatForXML$(?NoSurroundSpace$(ts.sdesc$(102))))
   endif
   RETURN

scaleLabelSub:
   if (ts.errf = 39) and                   \ error on scale
      ((ts.ts11werr$ = "FU") or            \ device not open or
       (ts.ts11werr$ = "DO")) then begin   ! offline
     with$ = ?InQuote$("")
   endif else \                            ! error reading scale
   begin
      with$ = ?InQuote$(?FormatForXML$(javaScaleLabelData$))
   endif
   RETURN





!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
label.addAccountTenderMsg:
label.addDiscountEntryMsg:
label.addItemEntryMsg:
  event=-1
  RETURN

label.addTenderEntryMsg:
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
!AEF
! READ THIS!
!   (*) Currency will not be sent by SA
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴

  IF (itemVoidAllowed) THEN BEGIN
    SI.data$ = SI.data$ + ?ItemVoidAllowed$(0)
  ENDIF

!AIO08970 Updated the <tender> event definition in eamaefxf.dat.
!12,<tender tenderType=&1 tenderVariety=&2 isVoided=&3 description=&4 amount=&5
!           balanceSatisfied=&12 change=&13 [(7)foodStampChange=&14]
!           [(1)currency=&6] [(2)fee=&7] [(3)accountNumber=&8]
!           [(4)referenceNumber=&9] [(5)expirationDate=&10] [(6)cardID=&11]
!           [(8)responseCode=&15] [(9)responseCodeDescriptor=&16]
!           [(10)maskedAccountNumber=&17] [(11)approvalCode=&18]
!           [(12)sequenceNumber=&19] signatureNeeded=&20 regEPrinted=&21>
!13,</tender>
!18,<legal text=&1/>
!EIO08970
    eventString$ = ?aefXML$(12)                   ! <tender>
    element$ = ?GetXMLEvent$(eventString$)
    IF LEN(element$) THEN \
    BEGIN
      IF (SL.TE.AMTTNFEE <> 0) THEN BEGIN         ! if a tender fee
         CALL AllowConditionalTag(2,eventString$) ! include fee attribute.
      ENDIF
      IF TS.ACNUM$ <> "" THEN BEGIN               ! ACCOUNT NUMBER not null
         CALL AllowConditionalTag(3,eventString$) ! include accountNumber attribute.
      ENDIF
      IF TS.EXPIRY$ <> "" AND \
         TS.EXPIRY$ <> "0000" THEN BEGIN          ! EXPIRATION DATE not null
          CALL AllowConditionalTag(5,eventString$)! include expirationDate attribute.
      ENDIF
      IF TS.BALDUE(1) < 0 THEN BEGIN              ! if Food Stamp change
         CALL AllowConditionalTag(7,eventString$) ! foodStampChange attribute.
      ENDIF
!AIO08970 Populate the new attributes in the <tender> event based on
!         the JAVA.xxx variables that must be set by the payment system.
      IF (JAVA.TENDER.CURRENCY$ <> "" ) THEN BEGIN
         CALL AllowConditionalTag(1,eventString$) ! currency attribute.
      ENDIF
      IF (JAVA.TENDER.REFNUM$ <> "" ) THEN BEGIN
         CALL AllowConditionalTag(4,eventString$) ! referenceNumber attribute.
      ENDIF
      IF (JAVA.TENDER.CARDID$ <> "" ) THEN BEGIN
         CALL AllowConditionalTag(6,eventString$) ! cardID attribute.
      ENDIF
      IF (JAVA.TENDER.RESPONSE.CODE$ <> "" ) THEN BEGIN
         CALL AllowConditionalTag(8,eventString$) ! responseCode attribute.
      ENDIF
      IF (JAVA.TENDER.RESPONSE.CODE.DESC$ <> "" ) THEN BEGIN
         CALL AllowConditionalTag(9,eventString$) ! responseCodeDescriptor attribute.
      ENDIF
      IF (JAVA.TENDER.MASKED.ACCOUNT.NUMBER$ <> "" ) THEN BEGIN
         CALL AllowConditionalTag(10,eventString$) ! maskedAccountNumber attribute.
      ENDIF
      IF (JAVA.TENDER.APPROVAL.CODE$ <> "" ) THEN BEGIN
         CALL AllowConditionalTag(11,eventString$) ! approvalCode attribute.
      ENDIF
      IF (JAVA.TENDER.SEQUENCE.NUMBER$ <> "" ) THEN BEGIN
         CALL AllowConditionalTag(12,eventString$) ! sequenceNumber attribute.
      ENDIF
!EIO08970

      CALL RemoveConditionalTags(eventString$)
      marker$ = ?FindMarker$(eventString$)        ! marker = &##... where #=number
      aefl% = LEN(marker$)
      WHILE aefl% <> 0                               ! loop until no more markers
        marker% = val(mid$(marker$,2,aefl%))         ! marker value
        with$ = ?InQuote$("")
        ON marker% GOSUB tendTypeSub,                 \ 1
                         tendVarietySub,              \ 2
                         tendVoidedSub,               \ 3
                         tendDescSub,                 \ 4
                         tendAmtSub,                  \ 5
                         tendCurrencySub,             \ 6
                         tendFeeSub,                  \ 7
                         tendAcctNumSub,              \ 8
                         tendRefNumSub,               \ 9
                         tendExpDateSub,              \ 10
                         tendCardIDSub,               \ 11
                         tendBalanceSatisfiedSub,     \ 12 D2424/17178
                         tendChangeSub,               \ 13 D2424/change
                         tendFoodStampChangeSub,      \ 14 D2424/foodstampchange IO08970
                         tendResponseCodeSub,         \ 15 IO08970
                         tendResponseCodeDescSub,     \ 16 IO08970
                         tendMaskedAcctNumSub,        \ 17 IO08970
                         tendApprovalCodeSub,         \ 18 IO08970
                         tendSequenceNumberSub,       \ 19 IO08970
                         tendSigNeededSub,            \ 20 IO08970
                         tendRegEPrintedSub           ! 21 IO08970
        CALL SubstituteMarker(marker$, eventString$, with$)
        marker$ = ?FindMarker$(eventString$)      ! next marker &##...
        aefl% = LEN(marker$)
      WEND
      SI.data$ = eventString$

!AIO08970 Populate the <legal> text element for the print line
!         if the payment system has set the JAVA.TENDER.LEGAL.TEXT$
      IF (JAVA.TENDER.LEGAL.TEXT$ <> "") THEN BEGIN
        eventString$ = ?aefXML$(18)                       ! <legal>
        element$ = ?GetXMLEvent$(eventString$)
        IF (LEN(element$)>0) THEN \
        BEGIN
          call RemoveConditionalTags(eventString$)
          marker$ = ?FindMarker$(eventString$)           ! text attribute
          IF (LEN(marker$) > 0) THEN \
          BEGIN
            with$ = ?InQuote$(?FormatForXML$(JAVA.TENDER.LEGAL.TEXT$))
            CALL SubstituteMarker(marker$, eventString$, with$)
          ENDIF
          SI.data$ = SI.data$ + eventString$
        ENDIF
      ENDIF
!EIO08970

      !  tender header print line
      FOR PRT.LINE.COUNTER = 1 TO NUM.TENDER.HDR.LINES  ! handle all lines D21508
         eventString$ = ?aefXML$(52)                    ! <printLine>
         element$ = ?GetXMLEvent$(eventString$)
!AD21508 Process this tender header line
\        IF (LEN(element$)>0 AND JAVA.TENDER.HDR.LINE.RAW$ <> "" ) THEN \
         IF (LEN(element$)>0) THEN \
\ED21508
         BEGIN
!AIO08970 Include the signatureLine attribute in the <printLine> event if
!         the JAVA.TENDER.HDR.SIGNATURE variable has been set.
            IF (JAVA.TENDER.HDR.SIGNATURE <> 0) THEN BEGIN ! if this is a blank signature line
              CALL AllowConditionalTag(1,eventString$)     ! include signatureLine="true" attribute.
            ENDIF
!EIO08970
            call RemoveConditionalTags(eventString$)
            marker$ = ?FindMarker$(eventString$)           ! rawData attribute
            IF (LEN(marker$) > 0) THEN \
            BEGIN
!AD21508 There can be more than one tender header line
\               with$ = ?InQuote$(JAVA.TENDER.HDR.LINE.RAW$)
                with$ = ?InQuote$(JAVA.TENDER.HDR.LINE.RAW$(PRT.LINE.COUNTER))
!ED21508
                printLine.rawData$ = with$                 ! 6540
                CALL SubstituteMarker(marker$, eventString$, with$)
            ENDIF
            marker$ = ?FindMarker$(eventString$)           ! filteredData attribute
            IF (LEN(marker$) > 0) THEN \
            BEGIN
!AD21508 There can be more than one tender header line
\               with$ = ?InQuote$(JAVA.TENDER.HDR.LINE$)
                with$ = ?InQuote$(JAVA.TENDER.HDR.LINE$(PRT.LINE.COUNTER))
!ED21508
                CALL SubstituteMarker(marker$, eventString$, with$)
            ENDIF
            CALL ContextSensitivePrintLines(eventString$)  ! 6540
            SI.data$ = SI.data$ + eventString$
         ENDIF
      NEXT                                   ! end of this tender header D21508

      IF NUM.TENDER.HDR.LINES > 0 THEN BEGIN ! if there were any tender hdrs D21508

         JAVA.TENDER.PRT.INDEX = 0
         eventString$ = ?aefXML$(52)                    ! <printLine>
         WHILE JAVA.TENDER.PRT.LINE.RAW$(JAVA.TENDER.PRT.INDEX) <> "" \
               AND LEN(eventString$) > 0
!AIO08970 Include the signatureLine attribute in the <printLine> event if
!         the JAVA.TENDER.PRT.SIGNATURE array element has been set.
            IF (JAVA.TENDER.PRT.SIGNATURE(JAVA.TENDER.PRT.INDEX) <> 0) \
            THEN BEGIN                          ! this is a blank signature line
              CALL AllowConditionalTag(1,eventString$) ! include signatureLine="true" attribute.
            ENDIF
!EIO08970

            CALL RemoveConditionalTags(eventString$)
            marker$ = ?FindMarker$(eventString$)           ! rawData attribute
            IF (LEN(marker$) > 0) THEN \
            BEGIN
                with$ = ?InQuote$(JAVA.TENDER.PRT.LINE.RAW$(JAVA.TENDER.PRT.INDEX))
                printLine.rawData$ = with$                 ! 6540
                CALL SubstituteMarker(marker$, eventString$, with$)
            ENDIF
            marker$ = ?FindMarker$(eventString$)           ! filteredData attribute
            IF (LEN(marker$) > 0) THEN \
            BEGIN
                with$ = ?InQuote$(JAVA.TENDER.PRT.LINE$(JAVA.TENDER.PRT.INDEX))
                CALL SubstituteMarker(marker$, eventString$, with$)
            ENDIF
            JAVA.TENDER.PRT.INDEX = JAVA.TENDER.PRT.INDEX + 1
            CALL ContextSensitivePrintLines(eventString$)  ! 6540
            SI.data$ = SI.data$ + eventString$
            eventString$ = ?aefXML$(52)
         WEND
      ENDIF
!AD21508 Zero a counter instead of nulling the strings
\     JAVA.TENDER.HDR.LINE$ = ""
\     JAVA.TENDER.HDR.LINE.RAW$ = ""
      NUM.TENDER.HDR.LINES = 0
!ED21508
      JAVA.TENDER.PRT.INDEX = 0
      SI.data$ = SI.data$ + ?aefXML$(13)                ! </tender>
    ENDIF


    IF LEN(SI.data$) = 0 THEN BEGIN
       event = -1
    ENDIF

    marker$ = ""
    eventString$ = ""
    with$ = ""
  RETURN                                         ! return to caller




tendTypeSub:
   with$ = ?InQuote$(STR$(SL.TE.TENDTYPE))
   RETURN
tendVarietySub:
   with$ = ?InQuote$(STR$(SL.TE.TENDVAR))
   RETURN
tendVoidedSub:
   IF TS.IO.KEYS(1) = 70 THEN BEGIN              ! Tender VOIDED
      with$ = ?TrueOrFalse$(-1)
   ENDIF ELSE BEGIN
      with$ = ?TrueOrFalse$(0)
   ENDIF
   RETURN
tendDescSub:
   with$ = ?InQuote$(?FormatForXML$(?NoSurroundSpace$(TO.TENDVDESC$(TS.TENDIX(SL.TE.TENDTYPE,SL.TE.TENDVAR)))))
   RETURN
tendAmtSub:
   with$ = ?InQuote$(PRICE.FORMATTED$(SL.TE.AMTTENDE))
   RETURN
tendCurrencySub:
   with$ = ?InQuote$(?FormatForXML$(?NoSurroundSpace$(JAVA.TENDER.CURRENCY$))) ! IO08970
   JAVA.TENDER.CURRENCY$ = ""                                                  ! IO08970
   RETURN
tendFeeSub:
   with$ = ?InQuote$(PRICE.FORMATTED$(SL.TE.AMTTNFEE))
   RETURN
tendAcctNumSub:
   with$ = ?InQuote$(?FormatForXML$(TS.ACNUM$))
   RETURN
tendRefNumSub:
   with$ = ?InQuote$(?FormatForXML$(?NoSurroundSpace$(JAVA.TENDER.REFNUM$)))   ! IO08970
   JAVA.TENDER.REFNUM$ = ""                                                    ! IO08970
   RETURN
tendExpDateSub:
   with$ = ?InQuote$(?FormatForXML$(TS.EXPIRY$))
   RETURN
tendCardIDSub:
   with$ = ?InQuote$(?FormatForXML$(?NoSurroundSpace$(JAVA.TENDER.CARDID$)))   ! IO08970
   JAVA.TENDER.CARDID$ = ""                                                    ! IO08970
   RETURN
!AD2424/17178.  also added change to tender event for POSBC
tendBalanceSatisfiedSub:
   with$ = ?TrueOrFalse$(TS.BALDUE(0) <= 0)
   RETURN
tendChangeSub:
!AIO08970 Calculate the change due.
!  with$ = ?InQuote$(PRICE.FORMATTED$(-TS.BALDUE(0)))
   ! handle the change due field if end of transaction
   CALL JAVA.FORMAT.AMOUNT(0)
   javaWork1 = ?RemoveCharacter$(" ", JAVA.AMT$)
   with$ = ?InQuote$(javaWork1)
   IF (TS.BALDUE(0) <= 0) THEN BEGIN              \ if end of transaction
     CALL JAVA.FORMAT.AMOUNT(TS.BALDUE(0))        ! get the change due
     ! save the string result but strip blanks
     javaWork1 = ?RemoveCharacter$(" ",JAVA.AMT$)
     IF (RIGHT$(javaWork1,1) = "-") THEN BEGIN    ! if a negative value
       javaWork1 = LEFT$(javaWork1,LEN(javaWork1) - 1)  ! strip negative sign at end
     ENDIF                                        ! if a negative value
     with$ = ?InQuote$(javaWork1)
   ENDIF                                          ! if end of transaction
!EIO08970
   RETURN
tendFoodStampChangeSub:
   with$ = ?InQuote$(PRICE.FORMATTED$(-TS.BALDUE(1)))
   RETURN
!ED2424

!AIO08970 Add new subroutines for populating the new attributes of the
!         <tender> event.
tendResponseCodeSub:
   with$ = ?InQuote$( \
      ?FormatForXML$(?NoSurroundSpace$(JAVA.TENDER.RESPONSE.CODE$)))
   JAVA.TENDER.RESPONSE.CODE$ = ""
   RETURN
tendResponseCodeDescSub:
   with$ = ?InQuote$( \
      ?FormatForXML$(?NoSurroundSpace$(JAVA.TENDER.RESPONSE.CODE.DESC$)))
   JAVA.TENDER.RESPONSE.CODE.DESC$ = ""
   RETURN
tendMaskedAcctNumSub:
   with$ = ?InQuote$( \
      ?FormatForXML$(?NoSurroundSpace$(JAVA.TENDER.MASKED.ACCOUNT.NUMBER$)))
   JAVA.TENDER.MASKED.ACCOUNT.NUMBER$ = ""
   RETURN
tendApprovalCodeSub:
   with$ = ?InQuote$( \
      ?FormatForXML$(?NoSurroundSpace$(JAVA.TENDER.APPROVAL.CODE$)))
   JAVA.TENDER.APPROVAL.CODE$ = ""
   RETURN
tendSequenceNumberSub:
   with$ = ?InQuote$( \
      ?FormatForXML$(?NoSurroundSpace$(JAVA.TENDER.SEQUENCE.NUMBER$)))
   JAVA.TENDER.SEQUENCE.NUMBER$ = ""
   RETURN
tendSigNeededSub:
   with$ = ?TrueOrFalse$(JAVA.TENDER.SIGNATURE.NEEDED)
   JAVA.TENDER.SIGNATURE.NEEDED = 0
   RETURN
tendRegEPrintedSub:
   with$ = ?TrueOrFalse$(JAVA.TENDER.REGE.PRINTED)
   JAVA.TENDER.REGE.PRINTED = 0
   RETURN
!EIO08970

  RETURN

label.tenderDeclined:
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
!AEF
! READ THIS!
!   (*) Currency will not be sent by SA
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴

!AIO08970 <tenderDeclined> event is now supported.  This code generates
!         and populates the <tenderDeclined> XML event.
!155,<tenderDeclined tenderType=&1 tenderVariety=&2 isVoided=&3 description=&4
!    amount=&5 balanceSatisfied=&12 change=&13 [(7)foodStampChange=&14]
!    [(1)currency=&6] [(2)fee=&7] [(3)accountNumber=&8] [(4)referenceNumber=&9]
!    [(5)expirationDate=&10] [(6)cardID=&11] [(8)responseCode=&15]
!    [(9)responseCodeDescriptor=&16] [(10)maskedAccountNumber=&17]
!    [(11)approvalCode=&18] [(12)sequenceNumber=&19] signatureNeeded=&20
!    regEPrinted=&21>
!156,</tender>
    eventString$ = ?aefXML$(155)                   ! <tenderDeclined>
    element$ = ?GetXMLEvent$(eventString$)
    IF LEN(element$) THEN \
    BEGIN
      IF (SL.TE.AMTTNFEE <> 0) THEN BEGIN         ! if a tender fee
        CALL AllowConditionalTag(2,eventString$)  ! include fee attribute.
      ENDIF
      IF TS.ACNUM$ <> "" THEN BEGIN               ! ACCOUNT NUMBER not null
        CALL AllowConditionalTag(3,eventString$)  ! include accountNumber attribute.
      ENDIF
      IF (TS.EXPIRY$ <> "") AND \
         (TS.EXPIRY$ <> "0000") THEN BEGIN        ! EXPIRATION DATE not null
        CALL AllowConditionalTag(5,eventString$)  ! include expirationDate attribute.
      ENDIF
      IF TS.BALDUE(1) < 0 THEN BEGIN              ! if Food Stamp change
        CALL AllowConditionalTag(7,eventString$)  ! foodStampChange attribute.
      ENDIF
      IF (JAVA.TENDER.CURRENCY$ <> "" ) THEN BEGIN
        CALL AllowConditionalTag(1,eventString$)  ! currency attribute.
      ENDIF
      IF (JAVA.TENDER.REFNUM$ <> "" ) THEN BEGIN
        CALL AllowConditionalTag(4,eventString$)  ! referenceNumber attribute.
      ENDIF
      IF (JAVA.TENDER.CARDID$ <> "" ) THEN BEGIN
        CALL AllowConditionalTag(6,eventString$)  ! cardID attribute.
      ENDIF
      IF (JAVA.TENDER.RESPONSE.CODE$ <> "" ) THEN BEGIN
        CALL AllowConditionalTag(8,eventString$)  ! responseCode attribute.
      ENDIF
      IF (JAVA.TENDER.RESPONSE.CODE.DESC$ <> "" ) THEN BEGIN
        CALL AllowConditionalTag(9,eventString$)  ! responseCodeDescriptor attribute.
      ENDIF
      IF (JAVA.TENDER.MASKED.ACCOUNT.NUMBER$ <> "" ) THEN BEGIN
        CALL AllowConditionalTag(10,eventString$) ! maskedAccountNumber attribute.
      ENDIF
      IF (JAVA.TENDER.APPROVAL.CODE$ <> "" ) THEN BEGIN
        CALL AllowConditionalTag(11,eventString$) ! approvalCode attribute.
      ENDIF
      IF (JAVA.TENDER.SEQUENCE.NUMBER$ <> "" ) THEN BEGIN
        CALL AllowConditionalTag(12,eventString$) ! sequenceNumber attribute.
      ENDIF

      CALL RemoveConditionalTags(eventString$)
      marker$ = ?FindMarker$(eventString$)           ! marker = &##... where #=number
      aefl% = LEN(marker$)
      WHILE aefl% <> 0                               ! loop until no more markers
        marker% = val(mid$(marker$,2,aefl%))         ! marker value
        with$ = ?InQuote$("")

        ON marker% GOSUB tendTypeSub,                 \ 1  tenderType
                         tendVarietySub,              \ 2  tenderVariety
                         tendVoidedSub,               \ 3  isVoided
                         tendDescSub,                 \ 4  description
                         tendAmtSub,                  \ 5  amount
                         tendCurrencySub,             \ 6  currency
                         tendFeeSub,                  \ 7  fee
                         tendAcctNumSub,              \ 8  accountNumber
                         tendRefNumSub,               \ 9  referenceNumber
                         tendExpDateSub,              \ 10 expirationDate
                         tendCardIDSub,               \ 11 cardID
                         tendBalanceSatisfiedSub,     \ 12 balanceSatisfied
                         tendChangeSub,               \ 13 change
                         tendFoodStampChangeSub,      \ 14 foodStampChange
                         tendResponseCodeSub,         \ 15 responseCode
                         tendResponseCodeDescSub,     \ 16 responseCodeDescriptor
                         tendMaskedAcctNumSub,        \ 17 maskedAccountNumber
                         tendApprovalCodeSub,         \ 18 approvalCode
                         tendSequenceNumberSub,       \ 19 sequenceNumber
                         tendSigNeededSub,            \ 20 signatureNeeded
                         tendRegEPrintedSub           ! 21 regEPrinted
        CALL SubstituteMarker(marker$, eventString$, with$)
        marker$ = ?FindMarker$(eventString$)      ! next marker &##...
        aefl% = LEN(marker$)
      WEND
      SI.data$ = eventString$

      ! Each tender will have one or more print lines.  The first line is
      ! contained in JAVA.TENDER.HDR.LINE.RAW$.  If the tender has more than
      ! one print line, the second and following lines are contained in
      ! JAVA.TENDER.PRT.LINE.RAW$ array.  The following code constructs the
      ! <printLine> element(s) for the tender.  There will be one <printLine>
      ! element for each print line for the tender.

      !  tender header print line
!AD21508 If there were any tender header lines
\     IF (JAVA.TENDER.HDR.LINE.RAW$ <> "") THEN BEGIN
      FOR PRT.LINE.COUNTER = 1 TO NUM.TENDER.HDR.LINES  ! handle all lines
!ED21508
        eventString$ = ?aefXML$(52)                    ! <printLine>
        element$ = ?GetXMLEvent$(eventString$)
!AD21508 Process this tender header line
\       IF (LEN(element$)>0 AND JAVA.TENDER.HDR.LINE.RAW$ <> "" ) THEN \
        IF (LEN(element$)>0) THEN \
\ED21508
        BEGIN
          IF (JAVA.TENDER.HDR.SIGNATURE <> 0) THEN BEGIN ! if this is a blank signature line
            CALL AllowConditionalTag(1,eventString$)     ! include signatureLine="true" attribute.
          ENDIF
          call RemoveConditionalTags(eventString$)
          marker$ = ?FindMarker$(eventString$)         ! rawData attribute
          IF (LEN(marker$) > 0) THEN \
          BEGIN
!AD21508 Process this tender header line
\           with$ = ?InQuote$(JAVA.TENDER.HDR.LINE.RAW$)
            with$ = ?InQuote$(JAVA.TENDER.HDR.LINE.RAW$(PRT.LINE.COUNTER))
!ED21508
            printLine.rawData$ = with$
            CALL SubstituteMarker(marker$, eventString$, with$)
          ENDIF
          marker$ = ?FindMarker$(eventString$)         ! filteredData attribute
          IF (LEN(marker$) > 0) THEN \
          BEGIN
!AD21508 Process this tender header line
\           with$ = ?InQuote$(JAVA.TENDER.HDR.LINE$)
            with$ = ?InQuote$(JAVA.TENDER.HDR.LINE$(PRT.LINE.COUNTER))
!ED21508
            CALL SubstituteMarker(marker$, eventString$, with$)
          ENDIF
          CALL ContextSensitivePrintLines(eventString$)
          SI.data$ = SI.data$ + eventString$
        ENDIF
      NEXT                                   ! end of this tender header D21508

      IF NUM.TENDER.HDR.LINES > 0 THEN BEGIN ! if there were any tender hdrs D21508
        JAVA.TENDER.PRT.INDEX = 0
        eventString$ = ?aefXML$(52)                  ! <printLine>
        ! Process the second and following print lines (if any) for the tender.
        WHILE JAVA.TENDER.PRT.LINE.RAW$(JAVA.TENDER.PRT.INDEX) <> "" \
              AND LEN(eventString$) > 0
          IF (JAVA.TENDER.PRT.SIGNATURE(JAVA.TENDER.PRT.INDEX) <> 0) \ if this is a blank signature line
          THEN BEGIN
            CALL AllowConditionalTag(1,eventString$) ! include signatureLine="true" attribute.
          ENDIF

          CALL RemoveConditionalTags(eventString$)
          marker$ = ?FindMarker$(eventString$)       ! rawData attribute
          IF (LEN(marker$) > 0) THEN \
          BEGIN
            with$ = ?InQuote$( \
                    JAVA.TENDER.PRT.LINE.RAW$(JAVA.TENDER.PRT.INDEX))
            printLine.rawData$ = with$
            CALL SubstituteMarker(marker$, eventString$, with$)
          ENDIF
          marker$ = ?FindMarker$(eventString$)       ! filteredData attribute
          IF (LEN(marker$) > 0) THEN \
          BEGIN
            with$ = ?InQuote$(JAVA.TENDER.PRT.LINE$(JAVA.TENDER.PRT.INDEX))
            CALL SubstituteMarker(marker$, eventString$, with$)
          ENDIF
          JAVA.TENDER.PRT.INDEX = JAVA.TENDER.PRT.INDEX + 1
          CALL ContextSensitivePrintLines(eventString$)
          SI.data$ = SI.data$ + eventString$
          eventString$ = ?aefXML$(52)
        WEND
!AD21508 Zero a counter instead of nulling the strings
\       JAVA.TENDER.HDR.LINE$ = ""
\       JAVA.TENDER.HDR.LINE.RAW$ = ""
        NUM.TENDER.HDR.LINES = 0
!ED21508
        JAVA.TENDER.PRT.INDEX = 0
      ENDIF
      SI.data$ = SI.data$ + ?aefXML$(156)            ! </tenderDeclined>
    ENDIF

    ! If SI.data$ is empty, there is no XML event which should be sent
    ! to Store Integrator.  In this case, set the event flag to -1
    ! which will halt the event from being sent to Store Integrator.
    IF LEN(SI.data$) = 0 THEN BEGIN
       event = -1
    ENDIF

    marker$ = ""
    eventString$ = ""
    with$ = ""
  RETURN                                         ! return to caller
!EIO08970

label.changeGivenMsg:
label.endAccountingTransactionMsg:
label.endSalesTransactionMsg:
  event=-1
  RETURN

!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
label.endTransactionEvent:

!48,<transactionUpdate [(1)transactionDiscountAllowed=&1] [(2)itemVoidAllowed=&2]/>
!14,<transactionVoid id=&1 type=&2 category=&3 date=&4 time=&5/>
!15,<transactionEnd id=&1 type=&2 category=&3/>
!16,<transactionSuspended id=&1 type=&2 category=&3 date=&4 time=&5/>

  IF (TS.PROCEDURE = 0 OR TS.PROCEDURE = -1) THEN \
  BEGIN
     ! Regular sales trans ended, send transactionDiscountAllowed="false"
     !                                 itemVoidAllowed="false"
     IF (transDiscAllowed OR itemVoidAllowed) THEN BEGIN

        eventString$ = ""
        with$ = ?TrueOrFalse$(0)                      ! substitution value = "false"
        eventString$ = ?aefXML$(48)                    ! transactionUpdate
        element$ = ?GetXMLEvent$(eventString$)
        IF LEN(element$) THEN \
        BEGIN
          call AllowConditionalTag(1,eventString$) ! transactionDiscountAllowed="false"
          call AllowConditionalTag(2,eventString$) ! itemVoidAllowed="false"
          CALL RemoveConditionalTags(eventString$)
          marker$ = ?FindMarker$(eventString$)
          CALL SubstituteMarker(marker$, eventString$, with$)
          transDiscAllowed = 0
          marker$ = ?FindMarker$(eventString$)
          CALL SubstituteMarker(marker$, eventString$, with$)
          itemVoidAllowed = 0
        ENDIF

        SI.data$ = SI.data$ + eventString$

     ENDIF
  ENDIF

!AD2424/18325.  Add time and date to transaction void or suspend.
  TS.TEMP2$ = UNPACK$(LEFT$(SL.HD.DATETIME$, 3))
  TS.TEMP1$ = UNPACK$(RIGHT$(SL.HD.DATETIME$, 2))
  TS.TEMP1$ = LEFT$(TS.TEMP1$,2) + TS.TIMESEP$ + MID$(TS.TEMP1$,3,2)
!ED2424

  eventString$ = ""
  IF ((TS.IO.KEYS(1) = 70) AND                   \
     ( TS.IO.KEYS(6) = 81)) THEN BEGIN           ! trans void
     eventString$ = ?aefXML$(14)
!AIO12312  Reset transStarted flag
     transStarted = 0
!EIO12312
  ENDIF ELSE BEGIN
     IF TS.SUSP.IN.PROGRESS THEN BEGIN           ! transaction suspended
       eventString$ = ?aefXML$(16)
!AIO12312  Reset transStarted flag
       transStarted = 0
!EIO12312
     ENDIF ELSE BEGIN
       eventString$ = ?aefXML$(15)                ! transaction ended normally
!AIO12312  Reset transStarted flag
       transStarted = 0
!EIO12312
     ENDIF
  ENDIF
  element$ = ?GetXMLEvent$(eventString$)
  IF LEN(element$) THEN \
  BEGIN
    CALL RemoveConditionalTags(eventString$)
    marker$ = ?FindMarker$(eventString$)        ! marker = &##... where #=number
    aefl% = LEN(marker$)
    WHILE aefl% <> 0                               ! loop until no more markers
      marker% = val(mid$(marker$,2,aefl%))         ! marker value
      with$ = ?InQuote$("")
!AD2424/18325.  Add time and date to transaction void or suspend.
!     ON marker% GOSUB idSub, typeSub, catSub
!AIO08970 Call a unique subroutine for populating the "id" attribute.
!     ON marker% GOSUB idSub, typeSub, catSub, dateSub, timeSub
      ON marker% GOSUB endIdSub, typeSub, catSub, dateSub, timeSub
!EIO08970
!ED2424
      CALL SubstituteMarker(marker$, eventString$, with$)
      marker$ = ?FindMarker$(eventString$)      ! next marker &##...
      aefl% = LEN(marker$)
    WEND
    SI.data$ = SI.data$ + eventString$

  ENDIF

  WIC.IN.PROGRESS = 0

  IF LEN(SI.data$) = 0 THEN BEGIN
     event = -1
  ENDIF

  eventString$ = ""
  element$ = ""
  marker$ = ""
  with$ = ""
  RETURN

!AIO08970 New subroutine for populating the "id" attribute.
endIdSub:
!AD2534 Use the transaction number of the last started transaction
!  with$ = ?InQuote$(STR$(SL.HD.TRANSNUM))
  with$ = ?InQuote$(STR$(SAVE.TRANSNUM))
!ED2534
  RETURN
!EIO08970



!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
label.signoffEvent:

!17,<signOff operatorID=&1/>

  eventString$ = ?aefXML$(17)                    ! <signOff>
  element$ = ?GetXMLEvent$(eventString$)
  IF LEN(element$) THEN \
  BEGIN
    CALL RemoveConditionalTags(eventString$)
    marker$ = ?FindMarker$(eventString$)        ! marker = &##... where #=number
    IF LEN(marker$) > 0 THEN BEGIN
       GOSUB opIdSub
       CALL SubstituteMarker(marker$, eventString$, with$)
    ENDIF
    SI.data$ = SI.data$ + eventString$
  ENDIF

  ! if in training mode, then set we're leaving training on the sign off
  IF JAVA.TS.TRAINING THEN BEGIN
    SI.data$ = SI.data$ + ?aefXML$(44)                       \
    !    <dataEvent dataCategory="WORKSTATION_STATUS" TRAINING_MODE="false" />"
    JAVA.TS.TRAINING = 0
  ENDIF

  RETURN

!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
label.signonEvent:
  !Build the event in XML format

!19,<signOn operatorID=&1 operatorName=&2>
!20, <authorization salesAllowed=&3 tenderCashingAllowed=&4 tenderExchangeAllowed=&5
!21, loanAllowed=&6 pickupAllowed=&7 tenderListingAllowed=&8
!22, priceVerifyAllowed=&9 operatorTrainingAllowed=&10 terminalTransferAllowed=&11
!23, terminalMonitorAllowed=&12 tenderCountAllowed=&13 returnItemAllowed=&14
!24, wicTransactionAllowed=&15 deptTotalsReportAllowed=&16
!25, userNonsales1Allowed=&17 userNonsales2Allowed=&18 nosaleOpenCashDrawerAllowed=&19
!26, nosaleTenderRemovalAllowed=&20 nosaleTillExchangeAllowed=&21
!27, nosaleTenderVerifyAllowed=&22 nosaleTillReportAllowed=&23
!28, returnsAllowed=&24 discountsAllowed=&25 miscItemPayoutAllowed=&26
!29, immediateItemChangeAllowed=&27 delayedItemChangeAllowed=&28
!30, moreThanPriceChangesAllowed=&29 nosalePriceVerifyAllowed=&30
!31, managersProceduresAllowed=&31 reprintTenderReceiptAllowed=&32
!32, userFunction1Allowed=&33 frontEndCashierAllowed=&34/>
!40, </signOn>
  eventString$ = ?aefXML$(19) +                  \ <signOn>
                 ?aefXML$(20) +                  \    <authorization/>
                 ?aefXML$(21) +                  \
                 ?aefXML$(22) +                  \
                 ?aefXML$(23) +                  \
                 ?aefXML$(24) +                  \
                 ?aefXML$(25) +                  \
                 ?aefXML$(26) +                  \
                 ?aefXML$(27) +                  \
                 ?aefXML$(28) +                  \
                 ?aefXML$(29) +                  \
                 ?aefXML$(30) +                  \
                 ?aefXML$(31) +                  \
                 ?aefXML$(32) +                  \
                 ?aefXML$(40)                    ! </signOn>
  element$ = ?GetXMLEvent$(eventString$)
  IF LEN(element$) THEN \
  BEGIN
    CALL RemoveConditionalTags(eventString$)
    marker$ = ?FindMarker$(eventString$)        ! marker = &##... where #=number
    aefl% = LEN(marker$)
    WHILE aefl% <> 0                               ! loop until no more markers
      marker% = val(mid$(marker$,2,aefl%))         ! marker value
      with$ = ?InQuote$("")
      ON marker% GOSUB opIdSub, opNameSub,         \
                       a1Sub, a2Sub, a3Sub,        \
                       a4Sub, a5Sub, a6Sub,        \
                       a7Sub, a8Sub, a9Sub,        \
                       a10Sub, a11Sub, a12Sub,        \
                       a13Sub, a14Sub, a15Sub,        \
                       a16Sub, a17Sub, a18Sub,        \
                       a19Sub, a20Sub, a21Sub,        \
                       a22Sub, a23Sub, a24Sub,        \
                       a25Sub, a26Sub, a27Sub,        \
                       a28Sub, a29Sub, a30Sub,        \
                       a31Sub, a32Sub
      CALL SubstituteMarker(marker$, eventString$, with$)
      marker$ = ?FindMarker$(eventString$)      ! next marker &##...
      aefl% = LEN(marker$)
    WEND
    SI.data$ = SI.data$ + eventString$
  ENDIF

  IF LEN(SI.data$) = 0 THEN BEGIN
     event = -1
  ENDIF

  eventString$ = ""
  marker$ = ""
  with$ = ""
  RETURN


opIdSub:
   with$ = ?InQuote$(?FormatForXML$(?StripLeading$("0",UNPACK$(TS.OPER$))))
   RETURN

opNameSub:
   with$ = ?InQuote$(?FormatForXML$(?RemoveCharacter$(" ",TS.OPER.NAME$)))
   RETURN

a1Sub:
   ! sales transactions allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND0 AND 8000H))
   RETURN

a2Sub:
   !              X'4000' - Tender cashing allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND0 AND 4000H))
   RETURN

a3Sub:
   !              X'2000' - Tender  Exchange allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND0 AND 2000H))
   RETURN

a4Sub:
   !              X'1000' - Loan transaction allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND0 AND 1000H))
   RETURN

a5Sub:
   !              X'0800' - Pickup transaction allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND0 AND 0800H))
   RETURN

a6Sub:
   !              X'0400' - Tender Listing allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND0 AND 0400H))
   RETURN

a7Sub:
   !              X'0200' - Price verification allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND0 AND 0200H))
   RETURN

a8Sub:
   !              X'0100' - Operator training allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND0 AND 0100H))
   RETURN

a9Sub:
   !              X'0080' - Terminal transfer allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND0 AND 0080H))
   RETURN

a10Sub:
   !              X'0040' - Terminal monitor allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND0 AND 0040H))
   RETURN

a11Sub:
   !              X'0020' - Tender count allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND0 AND 0020H))
   RETURN

a12Sub:
   !              X'0010' - Return item allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND0 AND 0010H))
   RETURN

a13Sub:
   !              X'0008' - WIC Transaction allowed
   with$ = ?TrueOrFalse$(WIC.ENABLE)      !@tal08182003
   RETURN

a14Sub:
   !              X'0004' - Department Totals Report
   with$ = ?TrueOrFalse$((TS.OPER.IND0 AND 0004H))
   RETURN

a15Sub:
   !              X'0002' - User Non-sales 1 allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND0 AND 0002H))
   RETURN

a16Sub:
   !              X'0001' - User Non-sales 2 allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND0 AND 0001H))
   RETURN

a17Sub:
   !              X'8000' - No-sale open cash drawer allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND1 AND 8000H))
   RETURN

a18Sub:
   !              X'4000' - No-sale tender removal allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND1 AND 4000H))
   RETURN

a19Sub:
   !              X'2000' - No-sale till exchange allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND1 AND 2000H))
   RETURN

a20Sub:
   !              X'1000' - No-sale tender verify allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND1 AND 1000H))
   RETURN

a21Sub:
   !              X'0800' - No-sale till report allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND1 AND 0800H))
   RETURN

a22Sub:
   !              X'0400' - Refunds allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND1 AND 0400H))
   RETURN

a23Sub:
   !              X'0200' - Discounts allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND1 AND 0200H))
   RETURN

a24Sub:
   !              X'0100' - Misc. Item payouts allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND1 AND 0100H))
   RETURN

a25Sub:
   !              X'0080' - Immediate I.R. changes allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND1 AND 0080H))
   RETURN

a26Sub:
   !              X'0040' - Delayed I.R. Changes allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND1 AND 0040H))
   RETURN

a27Sub:
   !              X'0020' - More than price changes allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND1 AND 0020H))
   RETURN

a28Sub:
   !              X'0010' - No-sale price verify allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND1 AND 0010H))
   RETURN

a29Sub:
   !              X'0008' - Manager's Procedures allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND1 AND 0008H))
   RETURN

a30Sub:
   !              X'0004' - Reprint Tender Receipt allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND1 AND 0004H))
   RETURN

a31Sub:
   !              X'0002' - User Function  1 allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND1 AND 0002H))
   RETURN

a32Sub:
   !              X'0001' - Front End Cashier allowed
   with$ = ?TrueOrFalse$((TS.OPER.IND1 AND 0001H))
   RETURN


!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
label.optionsLoadedMsg:
!AIO08970  Price Verify Message is now supported.  Options Loaded Message
!          remains unsupported.
  event=-1
  RETURN

!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
! This event is raised after an no-sales price verify has been performed.
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
!EIO08970
label.priceVerifyMsg:
!AIO08970 Generate and populate an <item> or <coupon> event for a
!         price verify operation.
  JAVA.PRICE.VERIFY = -1
  CALL CHECK.FOR.WIC.ITEM

  ITEM.VOIDED = 0
  ITEM.RETURNED = 0

  IF (TS.PC.IT = 1) THEN BEGIN                   ! if item deposit
    ITEM.DEPOSIT = -1
  ENDIF ELSE BEGIN                               ! else item not deposit
    ITEM.DEPOSIT = 0
  ENDIF                                          ! if item deposit

  IF ((IR.INDICAT0 AND 0040H) <> 0) THEN BEGIN   ! if weight item
    WT.ITEM = -1                                 ! flag it
  ENDIF ELSE BEGIN                               ! else not weight item
    WT.ITEM = 0
  ENDIF                                          ! if weight item

!;55-70=item/coupon data
!55,<[(14)coupon][(15)itemEvent] eventType=[(27)"itemSale"][(28)"priceVerify"] itemType=&30 [(16) couponType="store"][(17) couponType="manufacturer"]
!    [(18)manufacturerNumber=&24] itemID=&1 [(30)itemIDQualifier=&2] description=&3 [(31)isVoided=&4] departmentNumber=&29
!56, [(25)isReturn=&5] [(26)isRefunded=&28] isDeposit=&6 itemRepeatAllowed=&7 pricingMethod=&8
!57, [(19)extendedPrice=&9] [(20)value=&25] [(21)itemTaxable=&10] [(22)reducesTaxDue=&26] [(1)quantity=&11]
!58, [(2)dealQuantity=&12 dealPrice=&13]
!59, [(3)unitPrice=&14]
!60, [(4)groupID=&15]
!61, [(5)dealQuantity=&12]
!62, [(6)dealPrice=&13]
!63, [(7)dealPrice=&13 unitPrice=&14]
!64, [(8)reducedPrice=&16 unitPrice=&14] [(29)enteredPriceUsed=&17]
!65, [(9)weight=&18] [(23)foodstampEligible=&19] [(24)reducesFoodstampBalanceDue=&27] [(10)requiredAge=&20] [(11)wicEligible=&21]
!66, [(12)linkedItemID=&22] [(13)linkedItemIDQualifier=&23]>
!67,<restrict reversed=&1 startDay=[(1)"sun"][(2)"mon"][(3)"tue"][(4)"wed"][(5)"thu"][(6)"fri"][(7)"sat"] endDay=[(8)"sun"][(9)"mon"][(10)"tue"][(11)"wed"][(12)"thu"][(13)"fri"][(14)"sat"] startTime=&2 endTime=&3/>
!69,</coupon>
!70,</itemEvent>

  eventString$ = ""
  for aefl% = 55 to 66
    eventString$ = eventString$ + ?aefXML$(aefl%)  ! <itemEvent>
  next aefl%
  element$ = ?GetXMLEvent$(eventString$)
  IF LEN(element$) THEN \
  BEGIN

    CALL AllowConditionalTag(28, eventString$)  ! eventType="priceVerify"

    IF (TS.PC.IT NE 6) AND                         \
       (TS.PC.IT NE 7) THEN BEGIN                  ! If not a coupon
      CALL AllowConditionalTag(15, eventString$)   ! <itemEvent>
      CALL AllowConditionalTag(23, eventString$)   ! foodstampEligible
    ENDIF ELSE BEGIN                               ! This is a coupon
      CALL AllowConditionalTag(14, eventString$)   ! <coupon>
      CALL AllowConditionalTag(22, eventString$)   ! reducesTaxBalanceDue
      CALL AllowConditionalTag(24, eventString$)   ! reducesFoodstampBalanceDue

      IF (TS.PC.IT = 6) THEN BEGIN                 ! If manufacturer's coupon
        CALL AllowConditionalTag(17, eventString$) ! couponType="manufacturer"
        IF LEN(MANUFACTURER.NUMBER$) > 0 THEN BEGIN
          CALL AllowConditionalTag(18, eventString$) ! manufacturerNumber
        ENDIF
      ENDIF ELSE BEGIN                             ! Store coupon
        CALL AllowConditionalTag(16, eventString$) ! couponType="store"
      ENDIF                                        ! Value of Mfr or Store Coupon
    ENDIF

    !Now set up pricing method unique data
    IF (TS.PC.PM = 0) THEN BEGIN                   ! if pricing method 0
      IF (VAL(TS.PC.QTY$) > 1) THEN BEGIN          ! if a deal quantity is set
        CALL AllowConditionalTag(2,eventString$)   ! dealQuantity dealPrice
      ENDIF ELSE BEGIN                             ! else a deal quantity is not set
        CALL AllowConditionalTag(3,eventString$)   ! unitPrice
      ENDIF                                        ! if a deal quantity is set

    ENDIF ELSE BEGIN                               ! else pricing method 1 - 4
      IF (VAL(IR.MPGROUP$) > 0) THEN BEGIN         ! if a multipricing group
        CALL AllowConditionalTag(4,eventString$)   ! groupID
      ENDIF                                        ! if a multipricing group
      CALL AllowConditionalTag(5,eventString$)     ! dealQuantity
      IF (TS.PC.PM = 1) THEN BEGIN                 ! if pricing method 1
         CALL AllowConditionalTag(6,eventString$)  ! dealPrice
      ENDIF ELSE BEGIN                             ! else not pricing method 1
        IF (TS.PC.PM = 2) THEN BEGIN               ! if pricing method 2
          CALL AllowConditionalTag(7,eventString$) ! dealPrice unitPrice
        ENDIF ELSE BEGIN                           ! else not pricing method 2
          IF (TS.PC.PM = 3) OR                     \
             (TS.PC.PM = 4) THEN BEGIN             ! if pricing method 3 or 4
            CALL AllowConditionalTag(8,eventString$)   ! reducedPrice unitPrice
          ENDIF                                    ! if pricing method 3 or 4
        ENDIF                                      ! if pricing method 2
      ENDIF                                        ! if pricing method 1
    ENDIF                                          ! if pricing method 0

    IF (aefRequiredAge% NE 0) THEN BEGIN           ! if age restricted item
      CALL AllowConditionalTag(10, eventString$)
    ENDIF

    IF WIC.ENABLE THEN BEGIN                     ! if wic feature enabled
      CALL AllowConditionalTag(11, eventString$) ! wicEnabled
    ENDIF

    IF IR.LINKEDTO$ NE (CHR$(0) + CHR$(0)) THEN BEGIN   ! item linked
      CALL AllowConditionalTag(12, eventString$)
    ENDIF

    CALL RemoveConditionalTags(eventString$)
    marker$ = ?FindMarker$(eventString$)        ! marker = &##... where #=number
    aefl% = LEN(marker$)
    WHILE aefl% <> 0                               ! loop until no more markers
      marker% = val(mid$(marker$,2,aefl%))         ! marker value
      with$ = ?InQuote$("")

      ON marker% GOSUB pv.itemIDSub, pv.itemQualSub,    \ 1-2   itemID, itemIDQualifier
               pv.itemDescSub, pv.itemVoidSub,          \ 3-4   description, isVoided
               pv.itemReturnSub, pv.itemDepSub,         \ 5-6   isReturn, isDeposit
               pv.itemRepSub, pv.itemMethodSub,         \ 7-8   itemRepeatAllowed, pricingMethod
               pv.itemExtSub, pv.itemTaxableSub,        \ 9-10  extendedPrice, itemTaxable
               pv.itemQtySub, pv.itemDealQtySub,        \ 11-12 quantity, dealQuantity
               pv.itemDealPriceSub, pv.itemUnitSub,     \ 13-14 dealPrice, unitPrice
               pv.itemGrpSub, pv.itemReducedSub,        \ 15-16 groupID, reducedPrice
               pv.itemEnteredSub, pv.itemWeightSub,     \ 17-18 enteredPriceUsed, weight
               pv.itemFSSub, pv.itemAgeSub,             \ 19-20 foodStampEligible, requiredAge
               pv.itemWICSub, pv.itemLinkSub,           \ 21-22 wicEligible, linkedItemID
               pv.itemLinkQualSub,                      \ 23    linkedItemIDQualifier
               pv.manNumberSub, pv.valueSub,            \ 24-25 manufacturerNumber, value
               pv.cpnReducesTaxSub, pv.reducesFSBalSub, \ 26-27 reducesTaxDue, reducesFoodstampBalanceDue
               pv.cpnRefundedSub,                       \ 28    isRefunded
               pv.itemDepartmentSub                     ! 29    departmentNumber

      CALL SubstituteMarker(marker$, eventString$, with$)
      marker$ = ?FindMarker$(eventString$)      ! next marker &##...
      aefl% = LEN(marker$)
    WEND
    SI.data$ = SI.data$ + eventString$

    ! If restricted item, append the <restrict> elements.
    IF (IR.INDICAT0 AND 02H) THEN BEGIN            ! if time restricted item
      IF (javaSOrestOptsRead = 0) THEN BEGIN       ! if restrictive optns not read yet
        CALL SETUP.RESTRICTED.DATA                 ! set up the string
      ENDIF                                        ! if restrictive options not read yet
      SI.data$ = SI.data$ + RESTRICTED.TIME.DATA$  ! restricted times
    ENDIF                                          ! if time restricted item

    ! Generate the <printLine> element for the price verify item.
!AD21508 JAVA.ITEM.HDR and JAVA.ITEM.PRT lines are now combined
!        into a single array of up to 5 elements.
!        The next 3 lines of comments are the old behavior.
\   ! The first print line is contained in JAVA.ITEM.HDR.LINE.RAW$.
\   ! If there is a second print line for the item, it will be
\   ! contained in JAVA.ITEM.PRT.LINE.RAW$.
    FOR PRT.LINE.COUNTER = 1 TO NUM.PRT.LINES        ! handle all lines
!ED21508
      eventString$ = ?aefXML$(52)                    ! <printLine>
      element$ = ?GetXMLEvent$(eventString$)

!AD21508 Handle each saved print line
\     IF (LEN(element$)>0) AND (JAVA.ITEM.HDR.LINE.RAW$ <> "") THEN \
      IF (LEN(element$)>0) THEN \
\ED21508
      BEGIN
        call RemoveConditionalTags(eventString$)
        marker$ = ?FindMarker$(eventString$)         ! rawData attribute
        IF (LEN(marker$) > 0) THEN \
        BEGIN
!AD21508 Handle each saved print line
\         with$ = ?InQuote$(JAVA.ITEM.HDR.LINE.RAW$)
          with$ = ?InQuote$(JAVA.ITEM.PRT.LINE.RAW$(PRT.LINE.COUNTER))
!ED21508
          printLine.rawData$ = with$
          CALL SubstituteMarker(marker$, eventString$, with$)
        ENDIF
        marker$ = ?FindMarker$(eventString$)         ! filteredData attribute
        IF (LEN(marker$) > 0) THEN \
        BEGIN
!AD21508 Handle each saved print line
\         with$ = ?InQuote$(JAVA.ITEM.HDR.LINE$)
          with$ = ?InQuote$(JAVA.ITEM.PRT.LINE$(PRT.LINE.COUNTER))
!ED21508
          CALL SubstituteMarker(marker$, eventString$, with$)
        ENDIF
        CALL ContextSensitivePrintLines(eventString$)
        SI.data$ = SI.data$ + eventString$
      ENDIF
    NEXT                                           ! D21508

!AD21508 This block was handled with the loop above.  Delete it.
\   ! If there is a second print line for the price verify item,
\   ! generate another <printLine> element.
\   eventString$ = ?aefXML$(52)                    ! <printLine>
\   element$ = ?GetXMLEvent$(eventString$)
\   IF (LEN(element$)>0 AND JAVA.ITEM.PRT.LINE.RAW$ <> "" ) THEN \
\   BEGIN
\     call RemoveConditionalTags(eventString$)
\     marker$ = ?FindMarker$(eventString$)         ! rawData attribute
\     IF (LEN(marker$) > 0) THEN \
\     BEGIN
\       with$ = ?InQuote$(JAVA.ITEM.PRT.LINE.RAW$)
\       printLine.rawData$ = with$
\       CALL SubstituteMarker(marker$, eventString$, with$)
\     ENDIF
\     marker$ = ?FindMarker$(eventString$)         ! filteredData attribute
\     IF (LEN(marker$) > 0) THEN \
\     BEGIN
\       with$ = ?InQuote$(JAVA.ITEM.PRT.LINE$)
\       CALL SubstituteMarker(marker$, eventString$, with$)
\     ENDIF
\     CALL ContextSensitivePrintLines(eventString$)
\     SI.data$ = SI.data$ + eventString$
\   ENDIF
!ED21508

    IF (TS.PC.IT = 6 OR TS.PC.IT = 7) THEN BEGIN   ! If a coupon
      eventString$ = ?aefXML$(69)                  ! </coupon>
    ENDIF ELSE BEGIN                               ! else an item
      eventString$ = ?aefXML$(70)                  ! </itemEvent>
    ENDIF
    element$ = ?GetXMLEvent$(eventString$)
    IF LEN(element$)>0 THEN \
    BEGIN
      call RemoveConditionalTags(eventString$)
      SI.data$ = SI.data$ + eventString$
    ENDIF
  ENDIF

  ! If SI.data$ is empty, there is no XML which should be sent
  ! to Store Integrator.  Therefore, set the event flag to -1
  ! which will bypass sending the event to Store Integrator.
  IF (LEN(SI.data$) = 0) THEN \
  BEGIN
    event = -1
  ENDIF

  marker$ = ""
  with$ = ""
  eventString$ = ""
  JAVA.PRICE.VERIFY = 0
  RETURN

pv.itemIDSub:
  with$ = ?InQuote$(?FormatForXML$(UNPACK$(IR.ITEMCODE$)))
  RETURN
pv.itemQualSub:
pv.itemVoidSub:
pv.itemReturnSub:
pv.itemExtSub:
pv.itemQtySub:
pv.itemEnteredSub:
pv.itemWeightSub:
pv.itemLinkQualSub:
pv.valueSub:
pv.cpnRefundedSub:
  with$ = ?InQuote$("")
  RETURN
pv.itemDescSub:
  ! description
  with$ = ?InQuote$(?FormatForXML$(?NoSurroundSpace$(IR.ITEMNAME$)))
  RETURN
pv.itemDepSub:
  ! is item a deposit?
  with$ = ?TrueOrFalse$(ITEM.DEPOSIT)
  RETURN
pv.itemRepSub:
  ! is item repeatable?
  with$ = ?TrueOrFalse$(javaPVItemRepeatable)
  RETURN
pv.itemMethodSub:
  ! return the pricing method
  with$ = GET.PV.PRICING.METHOD$
  RETURN
pv.itemTaxableSub:
  ! Taxable?
  IF ( ( IR.INDICAT1 AND 00F0H) <> 0 ) THEN BEGIN
    with$ = ?TrueOrFalse$(-1)
  ENDIF ELSE BEGIN
    with$ = ?TrueOrFalse$(0)
  ENDIF
  RETURN
pv.itemDealQtySub:
  with$ = ?InQuote$(TS.PC.QTY$)
  RETURN
pv.itemDealPriceSub:
  with$ = ?InQuote$(PRICE.FORMATTED$(0))
  IF ( TS.PC.PM < 2) AND    \ If pricing method is unit or base plus 1 and
     ( VAL(TS.PC.QTY$) >1 ) \ price qty is greater than 1
  THEN BEGIN
    with$ = ?InQuote$(PRICE.FORMATTED$(TS.PC.PRICE1))   ! use deal price
  ENDIF ELSE IF ( TS.PC.PM = 2 ) THEN BEGIN  ! If group threshold pricing
    with$ = ?InQuote$(PRICE.FORMATTED$(TS.PC.PRICE1))   ! use deal price
  ENDIF
  RETURN
pv.itemUnitSub:
  with$ = ?InQuote$(PRICE.FORMATTED$(0))
  IF ( TS.PC.PM < 2) AND    \ If pricing method is unit or base plus 1 and
     ( VAL(TS.PC.QTY$) <2 ) \ price qty is zero or 1
  THEN BEGIN
    with$ = ?InQuote$(PRICE.FORMATTED$(TS.PC.PRICE1)) ! Use deal price
  ENDIF ELSE IF ( TS.PC.PM = 2 ) THEN BEGIN           ! If group threshold
                                                      ! pricing
    with$ = ?InQuote$(PRICE.FORMATTED$(TS.PC.PRICE2)) ! use single unit price
  ENDIF ELSE IF (TS.PC.PM > 2 ) THEN BEGIN            ! If reduced/increased
                                                      ! deal pricing
    with$ = ?InQuote$(PRICE.FORMATTED$(TS.PC.PRICE1)) ! use deal price
  ENDIF
  RETURN
pv.itemGrpSub:
  with$ = ?InQuote$(IR.MPGROUP$)
  RETURN
pv.itemReducedSub:
  if ( TS.PC.PM > 2 ) THEN BEGIN   ! If group threshold, reduced/increased deal
    with$ = ?InQuote$(PRICE.FORMATTED$(TS.PC.PRICE2)) ! use single unit price
  ENDIF ELSE BEGIN
    with$ = ?InQuote$(PRICE.FORMATTED$(0))
  ENDIF
  RETURN
pv.itemFSSub:
  ! Eligible for Food Stamps
  with$ = ?TrueOrFalse$(IR.INDICAT1 AND 0008H)
  RETURN
pv.itemAgeSub:
  with$ = ?InQuote$(STR$(aefRequiredAge%))
  RETURN
pv.itemWICSub:
  with$ = ?TrueOrFalse$(WIC.ITEM)
  RETURN
pv.itemLinkSub:
  with$ = ?InQuote$(?FormatForXML$(?StripLeading$("0",unpack$(IR.LINKEDTO$))))
  RETURN
pv.manNumberSub:
  ! Initially assumes 11 digit UPC.  Other types of UPCs are checked below.
  ! EM may add some characters to the beginning of the item code.
  ! The following code checks for these characters, and restores the item
  ! code so that the manufacturers number can be extracted.
  MANUFACTURER.NUMBER$ = MID$(IR.ITEMCODE$, 3, 5)
  IF LEFT$(IR.ITEMCODE$,1) = "=" THEN BEGIN         ! CHECK FOR EM FLAG
    JAVA.MANF.WORK = 2                              !
    IF LEFT$(IR.ITEMCODE$,2) = "==" THEN BEGIN      ! CHECK FOR EM FLAG
      JAVA.MANF.WORK = 3                            !
    ENDIF                                           !
  ENDIF ELSE BEGIN                                  !
    JAVA.MANF.WORK = 1                              !
  ENDIF                                             !
  IF JAVA.MANF.WORK > 1 THEN BEGIN
    JAVA.MANF.WORK = VAL(MID$(IR.ITEMCODE$,JAVA.MANF.WORK,2))
  ENDIF ELSE BEGIN
    JAVA.MANF.WORK = VAL(MID$(IR.ITEMCODE$,JAVA.MANF.WORK,3))
  ENDIF
  IF LEFT$(IR.ITEMCODE$,2) <> "==" THEN BEGIN
    IF ((JAVA.MANF.WORK = 30) OR                     \
        (JAVA.MANF.WORK = 38)) THEN BEGIN            !
      MANUFACTURER.NUMBER$ = "0" +                   \
        MID$(IR.ITEMCODE$, 3, 4)
    ENDIF
  ENDIF
  with$ = ?InQuote$(MANUFACTURER.NUMBER$)
  RETURN
pv.cpnReducesTaxSub:
  with$ = ?TrueOrFalse$(IR.INDICAT1 AND 00F0H)
  RETURN
pv.reducesFSBalSub:
   with$ = ?TrueOrFalse$(IR.INDICAT1 AND 0008H)
  RETURN
pv.itemDepartmentSub:
  with$ = ?InQuote$( RIGHT$( "000"+UNPACK$(IR.DEPARTME$), 3) )
  RETURN
!EIO08970

label.startAccountingTransactionMsg:
label.startSalesTransactionMsg:
  event=-1
  RETURN

!The non-sales procedure started/completed both pass the procedure ID in the event
label.procedureCompletedMsg:
  event = -1
  RETURN


label.procedureStartedMsg:

  IF (TS.PROCEDURE = 7) THEN BEGIN
     JAVA.TS.TRAINING = -1
     eventString$ = ""
     eventString$ = ?aefXML$(43)                  ! dataEvent TRAINING_MODE=true
     element$ = ?GetXMLEvent$(eventString$)
     IF LEN(element$) THEN \
     BEGIN
       SI.data$ = eventString$
     ENDIF ELSE \
     BEGIN
        event = -1
     ENDIF
!AIO11817 This is a base problem that was made worse by IO11370
!    EXIT SUB
     RETURN               ! return to javaAEFevent
!EIO11817
  ENDIF ELSE BEGIN
     event = -1
  ENDIF

  RETURN

label.procedureStartedMsg2:
  event = -1
  RETURN



label.operatorDisplay:
!78,<dataEvent dataCategory="POS_DEVICE" OPERATOR_DISPLAY=&1/>

  eventString$ = ?aefXML$(78)                    ! <dataEvent dataCategory="POS_DEVICE" OPERATOR_DISPLAY=&1/>
  element$ = ?GetXMLEvent$(eventString$)
  IF LEN(element$) THEN \
  BEGIN
    CALL RemoveConditionalTags(eventString$)
    marker$ = ?FindMarker$(eventString$)        ! marker = &##... where #=number
    IF LEN(marker$) > 0 THEN BEGIN
       with$ = ?InQuote$(javaOperatorDisplay$)
       CALL SubstituteMarker(marker$, eventString$, with$)
    ENDIF
    SI.data$ = SI.data$ + eventString$
  ENDIF

  IF LEN(SI.data$) = 0 THEN BEGIN
     event = -1
  ENDIF

  eventString$ = ""
  marker$ = ""
  with$ = ""
  RETURN

label.customerDisplay:
!79,<dataEvent dataCategory="POS_DEVICE" CUSTOMER_DISPLAY=&1/>

  eventString$ = ?aefXML$(79)                    ! <dataEvent dataCategory="POS_DEVICE" CUSTOMER_DISPLAY=&1/>
  element$ = ?GetXMLEvent$(eventString$)
  IF LEN(element$) THEN \
  BEGIN
    CALL RemoveConditionalTags(eventString$)
    marker$ = ?FindMarker$(eventString$)        ! marker = &##... where #=number
    IF LEN(marker$) > 0 THEN BEGIN
       with$ = ?InQuote$(javaCustomerDisplay$)
       CALL SubstituteMarker(marker$, eventString$, with$)
    ENDIF
    SI.data$ = SI.data$ + eventString$
  ENDIF

  IF LEN(SI.data$) = 0 THEN BEGIN
     event = -1
  ENDIF

  eventString$ = ""
  marker$ = ""
  with$ = ""
  RETURN

label.noSaleStartMsg:

    TS.TEMP2$ = UNPACK$(LEFT$(SL.HD.DATETIME$, 3))
    TS.TEMP1$ = UNPACK$(RIGHT$(SL.HD.DATETIME$, 2))
    TS.TEMP1$ = LEFT$(TS.TEMP1$,2) + TS.TIMESEP$ + MID$(TS.TEMP1$,3,2)

    eventString$ = ?aefXML$(45)                    ! transactionStart
    element$ = ?GetXMLEvent$(eventString$)
    IF LEN(element$) THEN \
    BEGIN
      CALL RemoveConditionalTags(eventString$)
      marker$ = ?FindMarker$(eventString$)        ! marker = &##... where #=number
      aefl% = LEN(marker$)
      WHILE aefl% <> 0                               ! loop until no more markers
        marker% = val(mid$(marker$,2,aefl%))         ! marker value
        with$ = ?InQuote$("")
        ON marker% GOSUB idSub, dateSub, \
                   timeSub, typeSub, \
                   modSub, catSub
        CALL SubstituteMarker(marker$, eventString$, with$)
        marker$ = ?FindMarker$(eventString$)      ! next marker &##...
        aefl% = LEN(marker$)
      WEND
      SI.data$ = eventString$
    ENDIF ELSE \
    BEGIN
       event = -1
    ENDIF

  RETURN


label.noSaleEndMsg:
    eventString$ = ?aefXML$(15)                    ! transactionEnd
    element$ = ?GetXMLEvent$(eventString$)
    IF LEN(element$) THEN \
    BEGIN
      CALL RemoveConditionalTags(eventString$)
      marker$ = ?FindMarker$(eventString$)        ! marker = &##... where #=number
      aefl% = LEN(marker$)
      WHILE aefl% <> 0                               ! loop until no more markers
        marker% = val(mid$(marker$,2,aefl%))         ! marker value
        with$ = ?InQuote$("")
        ON marker% GOSUB idSub, typeSub, catSub
        CALL SubstituteMarker(marker$, eventString$, with$)
        marker$ = ?FindMarker$(eventString$)      ! next marker &##...
        aefl% = LEN(marker$)
      WEND
      SI.data$ = eventString$
    ENDIF ELSE \
    BEGIN
       event = -1
    ENDIF
    RETURN

  RETURN

!E6540

!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
label.startTransactionEvent:


  IF (TS.PROCEDURE = 7) THEN BEGIN              ! training mode.
     eventString$ = ?aefXML$(43)                ! send the data event.
     element$ = ?GetXMLEvent$(eventString$)
     IF LEN(element$) THEN BEGIN
       SI.data$ = SI.data$ + eventString$
     ENDIF
  ENDIF ELSE  \
  BEGIN

!AD22768  reset tender line counter at start of transaction
     NUM.TENDER.HDR.LINES=0                       ! no saved tender print lines now
!ED22768
     IF (WIC.MODE = -1) THEN BEGIN
        WIC.IN.PROGRESS = -1
     ENDIF

     TS.TEMP2$ = UNPACK$(LEFT$(SL.HD.DATETIME$, 3))
     TS.TEMP1$ = UNPACK$(RIGHT$(SL.HD.DATETIME$, 2))
     TS.TEMP1$ = LEFT$(TS.TEMP1$,2) + TS.TIMESEP$ + MID$(TS.TEMP1$,3,2)

     eventString$ = ?aefXML$(45)                   ! transactionStart
     element$ = ?GetXMLEvent$(eventString$)
     IF LEN(element$) THEN \
     BEGIN
       CALL RemoveConditionalTags(eventString$)
       marker$ = ?FindMarker$(eventString$)        ! marker = &##... where #=number
       aefl% = LEN(marker$)
       WHILE aefl% <> 0                               ! loop until no more markers
         marker% = val(mid$(marker$,2,aefl%))         ! marker value
         with$ = ?InQuote$("")
         ON marker% GOSUB idSub, dateSub, \
                    timeSub, typeSub, \
                    modSub, catSub
         CALL SubstituteMarker(marker$, eventString$, with$)
         marker$ = ?FindMarker$(eventString$)      ! next marker &##...
         aefl% = LEN(marker$)
       WEND
       SI.data$ = SI.data$ + eventString$

     ENDIF
!AIO12312  Enable transStarted flag
     transStarted = 1
!EIO12312
  ENDIF

  IF (TS.PROCEDURE = 0 OR TS.PROCEDURE = -1) THEN \
  BEGIN
     exemptTransDisc = 0
     nonExemptTransDisc = 0
     ! Regular sales trans started, send transactionDiscountAllowed="true"
     SI.data$ = SI.data$ + ?TransDiscAllowed$(-1)
!AIO08970 If processing a header string, buffer it until the
!         transaction starts.
     IF (LEN(HEADER.STR$) > 0) THEN BEGIN
       SI.data$ = SI.data$ + HEADER.STR$
       HEADER.STR$ = ""
     ENDIF
!EIO08970
  ENDIF

  IF (LEN(SI.data$) = 0) THEN \
  BEGIN
     event = -1
  ENDIF


  marker$ = ""
  with$ = ""
  RETURN

idSub:
  with$ = ?InQuote$(STR$(SL.HD.TRANSNUM + 1))
!AD2534 Save the transaction number of the last started transaction
  SAVE.TRANSNUM = SL.HD.TRANSNUM + 1
!ED2534
  RETURN
dateSub:
  with$ = ?InQuote$(?FormatForXML$(TS.TEMP2$))
  RETURN
timeSub:
  with$ = ?InQuote$(?FormatForXML$(TS.TEMP1$))
  RETURN
typeSub:
  with$ = ?InQuote$(?FormatForXML$(?SetTransactionType$(TS.PROCEDURE)))
  RETURN
modSub:
  RETURN
catSub:
  IF (TS.PROCEDURE = 0 OR TS.PROCEDURE = -1 ) THEN BEGIN
     with$ = ?InQuote$("sales")
  ENDIF ELSE BEGIN
     with$ = ?InQuote$("nonsales")
  ENDIF
  RETURN



!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
label.reportOutputMsg:
label.scrollMsg:
label.displayVelocityCodesMsg:
label.hideVelocityCodesMsg:
label.redrawReceiptWindowMsg:

  event=-1
  RETURN

!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
label.updateStatusMsg:


  IF tssEnabled = 0 THEN BEGIN                   ! IR6706
     ! save error stack
!AIO11189 Unique save-restore variables must be used around javaReadScale
!         to prevent destruction by ON ERROR processing.
\    javaWork1 = ts.errdisp$
\    java1i2 = ts.errptr
\    java2i2 = ts.notone
     scale.javaWork1 = ts.errdisp$
     scale.java1i2 = ts.errptr
     scale.java2i2 = ts.notone
!EIO11189

     CALL javaReadScale                             ! read the scale

     ! restore error stack
!AIO11189 Unique save-restore variables must be used around javaReadScale
!         to prevent destruction by ON ERROR processing.
\    ts.errdisp$ = javaWork1
\    ts.errptr = java1i2
\    ts.notone = java2i2
     ts.errdisp$ = scale.javaWork1
     ts.errptr = scale.java1i2
     ts.notone = scale.java2i2
!EIO11189
     event = updateScaleMsg                         ! IRkhg

     CALL javaAEFEvent(event)                       ! fire the event khg
  ENDIF                                          ! IR6706

!  event = -1
  RETURN                                         ! update status message

!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
label.customerEvent:

!126,<customer id=&1 [(1)loyaltyIDType="primary"][(2)loyaltyIDType=&2] [(3)expirationDate=&3] name=&4>
!127,<targetedCoupon couponCode=&1/>
!128,<pointsTotal [(1)type=&1][(2)type="primary"] [(3)description="Primary Points Total"][(4)description=&2] points=&3/>
!129,<pointsBalance [(1)type=&1][(2)type="primary"] [(3)description="Primary Points Balance"] [(4)description=&2] points=&3/>
!130,<message>&1</message>
!131,</customer>
  eventString$ = ?aefXML$(126)                  ! <customer>
  element$ = ?GetXMLEvent$(eventString$)
  IF LEN(element$) THEN \
  BEGIN
    CALL AllowConditionalTag(1, eventString$)   ! loyaltyIDType="primary"
    IF (CA.EXPDATE$ <> "" AND CA.EXPDATE$ <> "0000") THEN BEGIN
       CALL AllowConditionalTag(3, eventString$)! expirationDate=xxxxxx
    ENDIF

    CALL RemoveConditionalTags(eventString$)
    marker$ = ?FindMarker$(eventString$)        ! marker = &##... where #=number
    aefl% = LEN(marker$)
    WHILE aefl% <> 0                               ! loop until no more markers
      marker% = val(mid$(marker$,2,aefl%))         ! marker value
      with$ = ?InQuote$("")
      ON marker% GOSUB custIdSub, loyaltyIdTypeSub, \
                 loyaltyExpSub, custNameSub
      CALL SubstituteMarker(marker$, eventString$, with$)
      marker$ = ?FindMarker$(eventString$)      ! next marker &##...
      aefl% = LEN(marker$)
    WEND
    SI.data$ = SI.data$ + eventString$

  ENDIF
  JAVA.TARGETCALL$ = UNPACK$(CA.TARGETC$)
  JAVA.MAXCOUPONS = LEN(JAVA.TARGETCALL$) / 4    ! Get the max num of targeted cpns
  FOR JAVA.COUPONINDEX = 1 TO JAVA.MAXCOUPONS
    JAVA.TARGETC$ = MID$(JAVA.TARGETCALL$,(JAVA.COUPONINDEX * 4) - 3,4)
    IF VAL(JAVA.TARGETC$) > 0 THEN BEGIN
       eventString$ = ?aefXML$(127)                  ! <targetedCoupon>
       element$ = ?GetXMLEvent$(eventString$)
       IF LEN(element$) THEN \
       BEGIN
         CALL RemoveConditionalTags(eventString$)
         marker$ = ?FindMarker$(eventString$)        ! marker = &##... where #=number
         IF LEN(marker$) > 0 THEN BEGIN
           with$ = ?InQuote$(?FormatForXML$(JAVA.TARGETC$))
           CALL SubstituteMarker(marker$, eventString$, with$)

         ENDIF
         SI.data$ = SI.data$ + eventString$

       ENDIF
    ENDIF
  NEXT


  eventString$ = ?aefXML$(128)                  ! <pointsTotal>
  element$ = ?GetXMLEvent$(eventString$)
  IF LEN(element$) THEN \
  BEGIN
    CALL AllowConditionalTag(2, eventString$)   ! type="primary"
    CALL AllowConditionalTag(3, eventString$)   ! description="Primary Points Total"
    CALL RemoveConditionalTags(eventString$)
    marker$ = ?FindMarker$(eventString$)        ! marker = &##... where #=number
    IF LEN(marker$) > 0 THEN BEGIN
       with$ = ?InQuote$(STR$(CA.POINTS))
       CALL SubstituteMarker(marker$, eventString$, with$)
    ENDIF
    SI.data$ = SI.data$ + eventString$
  ENDIF


  eventString$ = ?aefXML$(129)                  ! <pointsBalance>
  element$ = ?GetXMLEvent$(eventString$)
  IF LEN(element$) THEN \
  BEGIN
    CALL AllowConditionalTag(2, eventString$)   ! type="primary"
    CALL AllowConditionalTag(3, eventString$)   ! description="Primary Points Balance"
    CALL RemoveConditionalTags(eventString$)
    marker$ = ?FindMarker$(eventString$)        ! marker = &##... where #=number
    IF LEN(marker$) > 0 THEN BEGIN
       with$ = ?InQuote$(STR$(CA.POINTS - CA.REDEEMED))
       CALL SubstituteMarker(marker$, eventString$, with$)
    ENDIF
    SI.data$ = SI.data$ + eventString$
  ENDIF


  SI.data$ = SI.data$ + ?aefXML$(131)

  IF (LEN(SI.data$) = 0) THEN \
  BEGIN
     event = -1
  ENDIF

  marker$ = ""
  with$ = ""
  eventString$ = ""
  element$ = ""
  RETURN                                         ! return


custIdSub:
  with$ = ?InQuote$(?FormatForXML$(?StripLeading$("0",unpack$(ca.custnum$))))
  RETURN
loyaltyIdTypeSub:
  ! Base EM only supports "primary" type.
  RETURN
loyaltyExpSub:
  with$ = ?InQuote$(?FormatForXML$(CA.EXPDATE$))
  RETURN
custNameSub:
  with$ = ?InQuote$(?FormatForXML$(?NoSurroUndSpace$(CA.CUSTOMER$)))
  RETURN

!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
! AIRxxxxx - additional SureVision events
label.add2x20Msg:
  event=-1
  RETURN

!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
! This event is raised when a substate change occurs
label.terminalSubStateMsg:

   IF (jGuiSubState = 0) THEN BEGIN
      ! 0 is not a valid substate.
      event = -1
      RETURN
   ENDIF

   !D7350 Remap the SA WIC EBT substates to avoid conflict with other ACE/SA GUI substates
   IF ((jGuiSubState > 11210)  AND                \
       (jGuiSubState < 11230)) THEN BEGIN    !  Move WIC EBT messages(11211 - 11229) to (14211-14229) range
             jGuiSubState=jGuiSubState+3000  ! set the substate
   ENDIF
   !D7350

   ! Some prompts map to a single surevision substate. Remap those
   ! prompts here.
   IF (((jGuiSubState < 1001)  OR                \
        (jGuiSubState > 1013)) AND               \
        (jGuiSubState < 5000)) THEN BEGIN
      jGuiSubState=jGuiSubState+10000            ! set the substate
   ENDIF

   !@tal09092003 - if doing terminal monitor, then skip this sub state
   !  Otherwise, this terminal will react to the event as if it was it's own.
   !  Need to leave the monitor screen up until the function is ended.
   !  Unless this is the substate to prompt for the terminal to be monitored,
   !  (11203) or the substate to go into terminal monitor,
   !  in which case we DO want to handle it.
   IF (TS.TM.STATUS AND 0040H)  THEN BEGIN
      ! We are monitoring, so skip all substates but 11203 and 11227
      IF ((jGuiSubState = 11203) OR (jGuiSubState = 11227)) THEN BEGIN
      ENDIF ELSE BEGIN
         event = -1     !skip this event
         return
      ENDIF
   ENDIF

   !@tal05272003 - if this is a repeat signoff substate (11036) and
   !  it is the same as the last substate, then lose it.
   IF ((jGuiSubState=11036) AND (jGuiSubState = javaLastSubstate)) THEN BEGIN
      event = -1     !skip this event
      return
   ENDIF

   !@cjn01192004 - Comment out this code because it stops the 11034 from
   !               ever being sent.
   !@tal06262003 - if this is an expecting quantity (11034) and the
   !  last state is B029 QUANTITY NEEDED err (10359) then skip it.
   !  SA does the 11034 as part of the conditioning for the error stack
   !  and the real 11034 will come after the error is cleared.
   !IF ((jGuiSubState=11034) AND (javaLastSubstate = 10359)) THEN BEGIN
   !   javaLastSubstate = 11034
   !   event = -1     !skip this event
   !   return
   !ENDIF

   !@tal06122003 - SA appears to modify ENTER ITEM prompt with TRAINING and
   !  displays this following a nonsales procedure.
   !
   !  CAUTION: If this msg is displayed
   !  at other times during training, then it is possible that SureVision
   !  will assume the start of a new transaction/procedure when in fact it
   !  may not be.
   IF (jGuiSubState=11041 AND TS.TRAINING) THEN BEGIN
      jGuiSubState=expectingProcedureEvent                       !EXPECTING_PROCEDURE
   ENDIF

   IF ((jGuiSubState=11038) OR                   \ select proc or enter new item
                  (jGuiSubState=11044)) THEN BEGIN
      !@tal08192003 - if in wic transaction, then do NOT send 1008.  Instead,
      !  let substate flow.
      !IF NOT(WIC.MODE) THEN BEGIN @cjn01152004 this code stopped the 1008 from being sent
      !                                         when signing on from secure mode after doing
      !                                         the 14 signon, then timing out into secure mode.
      !                                         I was unable to find the inappropriate 1008 in wic mode.
                                                                 !Thank you Come Again map to
         jGuiSubState=expectingProcedureEvent   !EXPECTING_PROCEDURE
      !ENDIF                       @cjn01152004
   ENDIF ELSE \
   IF (jGuiSubState=11032) THEN BEGIN            !if Enter Override or CLEAR
      jGuiSubState=expectingManagerOverrideEvent !Expecting Managers Override
!AD29485 Do not send override substate now, will be sent again by eamts11c.main
      event = -1                                !skip this event
      return
!ED29485
   ENDIF ELSE \
   IF (jGuiSubState=11070) THEN BEGIN            !FS BAL DUE
      jGuiSubState=1003                          !maps to EXPECTING_FOODSTAMP_TENDER
   ENDIF ELSE \
   IF (jGuiSubState=11197) THEN BEGIN            !KEY LOAN AMOUNTS
      jGuiSubState=expectingLoanAmount           !maps to EXPECTING_LOAN_AMOUNT
   ENDIF ELSE \
   IF (jGuiSubState=11198) THEN BEGIN            !KEY PICKUP AMOUNTS
      jGuiSubState=expectingPickupAmount         !maps to EXPECTING_PICKUP_AMOUNT
   ENDIF ELSE \
   IF (jGuiSubState=11204) THEN BEGIN            !KEY TENDER AMOUNTS
      jGuiSubState=expectingTenderCountAmount    !maps to EXPECTING_TENDERCOUNT
   ENDIF

!AIO08970 If a pinpad is present during the "ENTER ACCOUNT NUMBER" prompt,
!         then change the substate from 11057 to 5010.
!         JAVA.PINPAD.ACTIVE is set by the payment system.
   IF (jGuiSubState=11057 AND JAVA.PINPAD.ACTIVE) THEN BEGIN
     jGuiSubState=waitingForAccountNumAtPINPAD
   ENDIF
!EIO08970

   IF (TS.IO.HDR$ <> "") THEN BEGIN
      java1i2= ASC(MID$(TS.IO.HDR$,13,1))
      !If this was a result of the total key being pressed then,
      !send a substate event also.
      !If this was a result of the total key being pressed then, send a substate event also.
      !@tal06272003 - and make sure the last state was not a 1003 because in this
      !  case the total key really cancels the fs tender and we want the 1002.
      if ((jGuiSubState=1002) AND (java1i2 = 81) AND (javaLastSubstate <> 1003)) then begin
        jGuiSubState=totalPressed                ! TOTAL_PRESSED
      ENDIF
   ENDIF


!143,<dataEvent dataCategory="POS_DEVICE" subState=&1/>
  eventString$ = ?aefXML$(143)                    ! <dataEvent dataCategory="POS_DEVICE" subState=&1/>
  element$ = ?GetXMLEvent$(eventString$)
  IF LEN(element$) THEN \
  BEGIN
    CALL RemoveConditionalTags(eventString$)
    marker$ = ?FindMarker$(eventString$)        ! marker = &##... where #=number
    IF LEN(marker$) > 0 THEN BEGIN
       with$ = ?InQuote$(STR$(jGuiSubState))
       CALL SubstituteMarker(marker$, eventString$, with$)
    ENDIF
    SI.data$ = SI.data$ + eventString$
  ENDIF

  IF LEN(SI.data$) = 0 THEN BEGIN
     event = -1
  ENDIF

  !@tal05272003 - save this substate as the last
  javaLastSubstate = jGuiSubState
  eventString$ = ""
  marker$ = ""
  with$ = ""
  RETURN                                         ! return

label.optionsLoadingStartedMsg:
  SI.data$ = SI.data$ + ?aefXML$(46)
  ! <dataEvent dataCategory="WORKSTATION_STATUS" OPTIONS_LOADING_IN_PROGRESS="true" />
!AD2506 add the decimal point, separator and number of digits if the format
!       is not x,xxx.xx  (USA standard)
!       OS configuration declares the number of decimal places to be zero
!         if the cent separator is blank (0x20).
!       Otherwise ("," or "."), the number of decimal places is 2.
!       If the user wants decimal places to be something other than 0 or 2,
!         then they must set AMOUNT.DECIMAL.PLACES in a user exit.
! 151,<LocaleOptions decimalSeparator=&1 groupingSeparator=&2 amountDecimalPlaces=&3/>
!AD21508 Use an array instead of (previously) two strings
  IF JAVA.ITEM.PRT.MAX = 0 THEN BEGIN         ! if not set by a user exit
    JAVA.ITEM.PRT.MAX = 5                     ! use the default
  ENDIF

  IF JAVA.TENDER.HDR.MAX = 0 THEN BEGIN       ! if not set by a user exit
    JAVA.TENDER.HDR.MAX = 5                   ! use the default
  ENDIF

  DIM JAVA.ITEM.PRT.LINE$(JAVA.ITEM.PRT.MAX)     ! item print line
  DIM JAVA.ITEM.PRT.LINE.RAW$(JAVA.ITEM.PRT.MAX) ! item print line raw
  NUM.PRT.LINES = 0                           ! no saved item lines now

  DIM JAVA.TENDER.HDR.LINE$(JAVA.TENDER.HDR.MAX)     ! TENDER header print lines
  DIM JAVA.TENDER.HDR.LINE.RAW$(JAVA.TENDER.HDR.MAX) ! TENDER header print lines raw
  NUM.TENDER.HDR.LINES = 0                    ! no saved tender header lines
!ED21508
  IF (TS.CENTSEP$ <> " ") AND                 \ if 2 decimal places
     (AMOUNT.DECIMAL.PLACES = 0) THEN BEGIN   ! not initialized yet
    AMOUNT.DECIMAL.PLACES = 2                 !   use 2 decimal places
  ENDIF
  eventString$ = ?aefXML$(151)                ! <LocaleOptions>
  element$ = ?GetXMLEvent$(eventString$)
  IF (LEN(element$) > 0) AND                  \
     ((TS.DOLLARSEP$ + TS.CENTSEP$ + STR$(AMOUNT.DECIMAL.PLACES)) \ USA = ",.2"
          <> ",.2") THEN BEGIN                ! if format is not x,xxx.xx
    CALL RemoveConditionalTags(eventString$)
    marker$ = ?FindMarker$(eventString$)      ! marker = &##... where #=number
    aefl% = LEN(marker$)
    WHILE aefl% <> 0                          ! loop until no more markers
      marker% = val(mid$(marker$,2,aefl%))    ! marker value
      with$ = ?InQuote$("")
      ON marker% GOSUB LocaleDecimalSeparatorSub, LocaleGroupingSeparatorSub, \
                       LocaleDecimalPlacesSub
      CALL SubstituteMarker(marker$, eventString$, with$)
      marker$ = ?FindMarker$(eventString$)    ! next marker &##...
      aefl% = LEN(marker$)
    WEND
    SI.data$ = SI.data$ + eventString$
  ENDIF
  RETURN                                      ! return
LocaleDecimalSeparatorSub:
  with$ = ?InQuote$(TS.CENTSEP$)              ! "."
  RETURN
LocaleGroupingSeparatorSub:
  with$ = ?InQuote$(TS.DOLLARSEP$)            ! ","
  RETURN
LocaleDecimalPlacesSub:
  IF (AMOUNT.DECIMAL.PLACES = -1) THEN BEGIN  ! use NO decimal places
    with$ = ?InQuote$("0")                    !   (initialized by a user exit)
  ENDIF ELSE BEGIN                            ! use 2 (or customer-defined)
    with$ = ?InQuote$(STR$(AMOUNT.DECIMAL.PLACES))
  ENDIF
  RETURN
!ED2506


label.optionsLoadingEndedMsg:
  SI.data$ = SI.data$ + ?aefXML$(47)
  ! <dataEvent dataCategory="WORKSTATION_STATUS" OPTIONS_LOADING_IN_PROGRESS="false" />
  RETURN                                         ! return

label.voidTransactionMsg:
label.foreignTotalMsg:
label.otrMsg:
  event=-1
  RETURN

label.operatorSpecialSignedOnMsg:
!71, <unlock operatorID=&1/>

  eventString$ = ?aefXML$(71)                   ! <unlock>
  element$ = ?GetXMLEvent$(eventString$)
  IF LEN(element$) THEN \
  BEGIN
    CALL RemoveConditionalTags(eventString$)
    marker$ = ?FindMarker$(eventString$)        ! marker = &##... where #=number
    IF LEN(marker$) > 0 THEN BEGIN
       GOSUB opIdSub
       CALL SubstituteMarker(marker$, eventString$, with$)
    ENDIF
    SI.data$ = SI.data$ + eventString$
  ENDIF
  RETURN


label.operatorSpecialSignedOffMsg:

!80, <lock operatorID=&1/>

  eventString$ = ?aefXML$(80)                   ! <lock>
  element$ = ?GetXMLEvent$(eventString$)
  IF LEN(element$) THEN \
  BEGIN
    CALL RemoveConditionalTags(eventString$)
    marker$ = ?FindMarker$(eventString$)        ! marker = &##... where #=number
    IF LEN(marker$) > 0 THEN BEGIN
       GOSUB opIdSub
       CALL SubstituteMarker(marker$, eventString$, with$)
    ENDIF
    SI.data$ = SI.data$ + eventString$
  ENDIF
  RETURN

!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
! This event is raised when the terminal either goes into or comes out of
!   standalone mode.
label.terminalConnectionChangeMsg:
   event=-1
   RETURN

!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
! This event is raised when FS-Total is keyed.
! The data needs to be the current foodstamp total.
label.foodStampTotalMsg:
   GOSUB label.transactionTotalsEvent
   RETURN

!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
! This event is raised when balance due is calculated and a discount was calculated.
! The data is disc group id'amount'percent'flags
! where flags (Y/N) are:
!   voided?
!   applies to whole tran?
!   keyed?
!   tax exempt?
!   customer activity?
!   refreshed?
label.discountJustAddedMsg:
  isVoided = 0
  IF (TS.IO.KEYS(1)=70) THEN BEGIN
     isVoided = -1
  ENDIF
  IF (TO.DISC.EXEMPT(TS.DISC.INDEX)) THEN BEGIN
     IF (isVoided) THEN BEGIN
        exemptTransDisc = 0
     ENDIF ELSE \
     BEGIN
        exemptTransDisc = -1
     ENDIF
  ENDIF ELSE \
  BEGIN
     IF (isVoided) THEN BEGIN
        nonExemptTransDisc = 0
     ENDIF ELSE \
     BEGIN
        nonExemptTransDisc = -1
     ENDIF
  ENDIF

!140,<transactionDiscount description=&1 isVoided=&2 discountMethod="percent" rate=&4
!141, amount=&5 discountReason=&6 reducesTaxBalanceDue=&7>
!142,</transactionDiscount>

  ! If the discount percentage is non-zero, send the <transactionDiscount> event.
  IF ( TO.DISC.RATE(TS.DISC.INDEX) NE 0) THEN BEGIN
    eventString$ = ?aefXML$(140) + ?aefXML$(141)     ! <transactionDiscount>
    element$ = ?GetXMLEvent$(eventString$)
    IF LEN(element$) THEN \
    BEGIN
      CALL RemoveConditionalTags(eventString$)
      marker$ = ?FindMarker$(eventString$)        ! marker = &##... where #=number
      aefl% = LEN(marker$)
      WHILE aefl% <> 0                               ! loop until no more markers
        marker% = val(mid$(marker$,2,aefl%))         ! marker value
        with$ = ?InQuote$("")
        ON marker% GOSUB discDescSub, discVoidSub, \
                         discMethodSub, discRateSub, \
                         discAmountSub, discReasonSub, \
                         reducesTaxSub
        CALL SubstituteMarker(marker$, eventString$, with$)
        marker$ = ?FindMarker$(eventString$)      ! next marker &##...
        aefl% = LEN(marker$)
      WEND
      SI.data$ = SI.data$ + eventString$
    ENDIF
    eventString$ = ?aefXML$(52)                    ! <printLine>
    element$ = ?GetXMLEvent$(eventString$)
    IF (LEN(element$)>0 AND JAVA.DISCOUNT.HDR.LINE.RAW$ <> "" ) THEN \
    BEGIN
       call RemoveConditionalTags(eventString$)
       marker$ = ?FindMarker$(eventString$)           ! rawData attribute
       IF (LEN(marker$) > 0) THEN \
       BEGIN
           with$ = ?InQuote$(JAVA.DISCOUNT.HDR.LINE.RAW$)
           printLine.rawData$ = with$                 ! 6540
           CALL SubstituteMarker(marker$, eventString$, with$)
       ENDIF
       marker$ = ?FindMarker$(eventString$)           ! filteredData attribute
       IF (LEN(marker$) > 0) THEN \
       BEGIN
           with$ = ?InQuote$(JAVA.DISCOUNT.HDR.LINE$)
           CALL SubstituteMarker(marker$, eventString$, with$)
       ENDIF
       CALL ContextSensitivePrintLines(eventString$)  ! 6540
       SI.data$ = SI.data$ + eventString$
    ENDIF
    eventString$ = ?aefXML$(52)                        ! <printLine>
    element$ = ?GetXMLEvent$(eventString$)
    IF (LEN(element$)>0 AND JAVA.DISCOUNT.PRT.LINE.RAW$ <> "" ) THEN \
    BEGIN
       call RemoveConditionalTags(eventString$)
       marker$ = ?FindMarker$(eventString$)           ! rawData attribute
       IF (LEN(marker$) > 0) THEN \
       BEGIN
           with$ = ?InQuote$(JAVA.DISCOUNT.PRT.LINE.RAW$)
           printLine.rawData$ = with$                 ! 6540
           CALL SubstituteMarker(marker$, eventString$, with$)
       ENDIF
       marker$ = ?FindMarker$(eventString$)           ! filteredData attribute
       IF (LEN(marker$) > 0) THEN \
       BEGIN
           with$ = ?InQuote$(JAVA.DISCOUNT.PRT.LINE$)
           CALL SubstituteMarker(marker$, eventString$, with$)
       ENDIF
       CALL ContextSensitivePrintLines(eventString$)  ! 6540
       SI.data$ = SI.data$ + eventString$

    ENDIF
    SI.data$ = SI.data$ + ?aefXML$(142)
  ENDIF



  JAVA.DISCOUNT.HDR.LINE$ = ""
  JAVA.DISCOUNT.HDR.LINE.RAW$ = ""
  JAVA.DISCOUNT.PRT.LINE$ = ""
  JAVA.DISCOUNT.PRT.LINE.RAW$ = ""

  IF (TO.DISC.EXEMPT(TS.DISC.INDEX)) THEN BEGIN
     ! Transaction tax exemption.
     eventString$ = ?aefXML$(49)                      ! <transactionTaxChange>
     element$ = ?GetXMLEvent$(eventString$)
     IF LEN(element$) THEN \
     BEGIN
       CALL AllowConditionalTag(1,eventString$)       ! taxType="taxExempt"
       CALL RemoveConditionalTags(eventString$)
       marker$ = ?FindMarker$(eventString$)           ! marker = &##... where #=number
       IF (LEN(marker$) > 0) THEN BEGIN
          with$ = ?InQuote$(STR$(TO.DISC.CODE(TS.DISC.INDEX)))
          CALL SubstituteMarker(marker$, eventString$, with$)
       ENDIF
       marker$ = ?FindMarker$(eventString$)
       IF (LEN(marker$) > 0) THEN BEGIN               ! isVoided="true|false"
          with$ = ?TrueOrFalse$(isVoided)
          CALL SubstituteMarker(marker$, eventString$, with$)
       ENDIF
       SI.data$ = SI.data$ + eventString$
     ENDIF
  ENDIF


  IF (NOT isVoided AND exemptTransDisc AND nonExemptTransDisc) THEN BEGIN
     ! Transaction Discount NOT allowed.
     SI.data$ = SI.data$ + ?TransDiscAllowed$(0)
  ENDIF ELSE \
  IF (isVoided) THEN BEGIN
     ! Transaction Discount allowed.
     SI.data$ = SI.data$ + ?TransDiscAllowed$(-1)
  ENDIF
  RETURN



discDescSub:
   with$ = ?InQuote$(?NoSurroundSpace$(?FormatForXML$(TO.DISC.DESC$(TS.DISC.INDEX))))
   RETURN
discVoidSub:
   IF (TS.IO.KEYS(1)=70) THEN BEGIN
     with$ = ?TrueOrFalse$(-1)
   ENDIF ELSE BEGIN
     with$ = ?TrueOrFalse$(0)
   ENDIF
   RETURN
discMethodSub:
   RETURN
discRateSub:
   with$ = ?InQuote$(PRICE.FORMATTED$(SL.DI.DISRATE))
   RETURN
discAmountSub:
   with$ = ?InQuote$(PRICE.FORMATTED$(TS.DISC.SAVE(0,0)))
   RETURN
discReasonSub:
   with$ = ?InQuote$(STR$(TO.DISC.CODE(TS.DISC.INDEX)))
   RETURN
reducesTaxSub:
   IF (TO.DISC.EXEMPT(TS.DISC.INDEX)) THEN BEGIN
      with$ = ?TrueOrFalse$(0)
   ENDIF ELSE BEGIN
      with$ = ?TrueOrFalse$(-1)
   ENDIF
   RETURN




!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴

!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
! This event is raised after an item has been added to the sales transaction.
! The data is: item code'item type'item attributes
label.itemEvent:

  !SI.data$ = ""                                 ! clear data variable
  ! Check to see if the item is an EM based points only item
  IF EMSS.EM.INSTALLED THEN BEGIN                ! if EM installed
    IF (OP.NO.EM = 0) THEN BEGIN                 ! if EM is active

      IF ((IR.INDICAT1A AND 20H) = 20H) THEN BEGIN    ! if a points item
        IF (SL.IT.INDICAT3B = 8) OR              \ if redemption
           (SL.IT.INDICAT3B = 9) THEN BEGIN      ! or bonus points
          IF (SL.IT.INDICAT3B = 8) THEN BEGIN    ! if redemption
            REDEEMED.PTS = -1                    ! set redemption flag
          ENDIF ELSE BEGIN                       ! else not redemption
            REDEEMED.PTS = 0                     ! turn off redemption flag
          ENDIF                                  ! if redemption

          ! Check to see if this is either a bonus point item/coupon or if no pending
          ! points on the redemption.  If pending points then it's possible this redemption
          ! coupon will not be processed now and we need to wait until the linking item
          ! has been validated.
          IF (REDEEMED.PTS = 0) OR               \ if bonus points
             (EMSS.PD.POINTS = 0) THEN BEGIN     ! or no pending redemption points
             ! ok to process point item now

            IF (JAVA.EM.PT.USERTYPE$ = "") THEN BEGIN ! if user hasn't designated type
              PT.TYPE$ = "primary"               ! base only has primary points
            ENDIF ELSE BEGIN                     ! else user designated type
              PT.TYPE$ = JAVA.EM.PT.USERTYPE$    ! use the user's type
            ENDIF                                ! if user hasn't designated type

            CALL POINT.ONLY.ITEM(SI.data$,       \ set up data for point only item
                                 PT.TYPE$,       \
                                 IR.ITEMNAME$,   \
                                 EMSS.POINTS$,   \
                                 REDEEMED.PTS)   !

          ENDIF ELSE BEGIN                       ! else don't process now, back out
            event = -1                           ! flag no event yet
          ENDIF                                  ! ok to process point item now
          RETURN                                 ! get out
        ENDIF                                    ! if redemption or bonus points
      ENDIF                                      ! if a points item

      IF VALUELESS.COUPON THEN BEGIN             ! if a valueless coupon
!AD21508 Zero a counter instead of nulling the strings
\       JAVA.ITEM.HDR.LINE$ = ""                 ! clear print variables
\       JAVA.ITEM.PRT.LINE$ = ""
        NUM.PRT.LINES=0                          ! no saved lines now
!ED21508
        event = -1                               ! no event
        RETURN                                   ! get out
      ENDIF                                      ! if a valueless coupon

    ENDIF                                        ! if EM is active
  ENDIF                                          ! if EM installed





  IF ((SL.IT.INDICAT2 AND 0080H) <> 0) THEN BEGIN  ! if item voided
    ITEM.VOIDED = -1
  ENDIF ELSE BEGIN                               ! else item not voided
    ITEM.VOIDED = 0
  ENDIF                                          ! if item voided

  IF (SL.IT.INDICAT3A = 2 OR SL.IT.INDICAT3A = 3) THEN BEGIN  ! if item returned
       ITEM.RETURNED = -1
  ENDIF ELSE BEGIN                               ! else item not returned
       ITEM.RETURNED = 0
  ENDIF                                          ! if item returned

  IF (SL.IT.INDICAT3A = 1 OR SL.IT.INDICAT3A = 3) THEN BEGIN  ! if item deposit
         ITEM.DEPOSIT = -1
  ENDIF ELSE BEGIN                               ! else item not deposit
         ITEM.DEPOSIT = 0
  ENDIF                                          ! if item deposit

  IF ((SL.IT.INDICAT1 AND 4000H) <> 0) THEN BEGIN  ! if weight item
    WT.ITEM = -1                                 ! flag it
  ENDIF ELSE BEGIN                               ! else not weight item
    WT.ITEM = 0
  ENDIF                                          ! if weight item

  IF ((SL.IT.INDICAT2 AND 2H) <> 0) THEN BEGIN   ! If extension
    EXT.STR = -1
  ENDIF ELSE BEGIN                               ! else no extension
    EXT.STR = 0
  ENDIF                                          ! if extension

  ! The element type is based on SL.IT.INDICAT3A
  ! 0 = NORMAL SALE
  ! 1 = DEPOSIT
  ! 2 = REFUND
  ! 3 = DEPOSIT RETURN
  ! 4 = MISC TRANS RECEIPT
  ! 5 = MISC TRANS PAYOUT
  ! 6 = MANF COUPON
  ! 7 = STORE COUPON
  ! 8 = ITEM SALE CANCEL
  ! 9 = DEPOSIT CANCEL

  ! Can remove this function!
  !CALL itemEventElementType(SL.IT.INDICAT3A, element$)
!AIO08970 We need to include an itemType attribute, so calculate the value.
  CALL itemEventElementType(SL.IT.INDICAT3A, itemType$)
!EIO08970

!;55-70=item/coupon data
!AIO08970
!55,<[(14)coupon][(15)itemEvent] eventType=[(27)"itemSale"][(28)"priceVerify"]
!   itemType=&30 [(16) couponType="store"][(17) couponType="manufacturer"]
!   [(18)manufacturerNumber=&24] itemID=&1 [(30)itemIDQualifier=&2]
!   description=&3 [(31)isVoided=&4] departmentNumber=&29
!EIO08970
!56, [(25)isReturn=&5] [(26)isRefunded=&27] isDeposit=&6 itemRepeatAllowed=&7 pricingMethod=&8
!57, [(19)extendedPrice=&9] [(20)value=&25] [(21)itemTaxable=&10] [(22)reducesTaxDue=&26] [(1)quantity=&11]
!58, [(2)dealQuantity=&12 dealPrice=&13]
!59, [(3)unitPrice=&14]
!60, [(4)groupID=&15]
!61, [(5)dealQuantity=&12]
!62, [(6)dealPrice=&13]
!63, [(7)dealPrice=&13 unitPrice=&14]
!64, [(8)reducedPrice=&16 unitPrice=&14] [(29)enteredPriceUsed=&17] ! IO008970
!65, [(9)weight=&18] [(23)foodstampEligible=&19] [(24)reducesFoodstampBalanceDue=&28] [(10)requiredAge=&20] [(11)wicEligible=&21]
!66, [(12)linkedItemID=&22] [(13)linkedItemIDQualifier=&23]>
!67,<restrict reversed=&1 day=[(1)"sun"][(2)"mon"][(3)"tue"][(4)"wed"][(5)"thu"][(6)"fri"][(7)"sat"] start=&2 end=&3/>
!69,</coupon>
!70,</itemEvent>


  eventString$ = ""
  for aefl% = 55 to 66
    eventString$ = eventString$ + ?aefXML$(aefl%)  ! <itemEvent>
  next aefl%
  element$ = ?GetXMLEvent$(eventString$)
  IF LEN(element$) THEN \
  BEGIN

    CALL AllowConditionalTag(27, eventString$)  ! eventType="itemSale" IO08970
    CALL AllowConditionalTag(29, eventString$)  ! enteredPriceUsed     IO08970
    CALL AllowConditionalTag(30, eventString$)  ! itemIDQualifier      IO08970
    CALL AllowConditionalTag(31, eventString$)  ! isVoided             IO08970

    IF (SL.IT.INDICAT3A NE 6 AND SL.IT.INDICAT3A NE 7) THEN BEGIN
       ! Not a coupon
       CALL AllowConditionalTag(15, eventString$)  ! <itemEvent>
       CALL AllowConditionalTag(19, eventString$)  ! extendedPrice
       CALL AllowConditionalTag(21, eventString$)  ! itemTaxable
       CALL AllowConditionalTag(23, eventString$)  ! foodstampEligible
       CALL AllowConditionalTag(25, eventString$)  ! isReturn
    ENDIF ELSE BEGIN
       ! This is a coupon
       CALL AllowConditionalTag(14, eventString$)  ! <coupon>
       CALL AllowConditionalTag(22, eventString$)  ! reducesTaxBalanceDue
       CALL AllowConditionalTag(24, eventString$)  ! reducesFoodstampBalanceDue
       CALL AllowConditionalTag(26, eventString$)  ! isRefunded

       IF (SL.IT.INDICAT3A = 6) THEN BEGIN
          ! Manufacturers coupon
          CALL AllowConditionalTag(17, eventString$) ! couponType="manufacturer"
          IF LEN(MANUFACTURER.NUMBER$) > 0 THEN BEGIN
             CALL AllowConditionalTag(18, eventString$) ! manufacturerNumber
          ENDIF
       ENDIF ELSE BEGIN
          CALL AllowConditionalTag(16, eventString$) ! couponType="store"
       ENDIF
       CALL AllowConditionalTag(20, eventString$)  ! value
    ENDIF

    IF WT.ITEM THEN BEGIN                          ! if weight item
      CALL AllowConditionalTag(9, eventString$)    ! weight
!AD18279  Always include the quantity attribute.
\   ENDIF ELSE BEGIN                               ! else not weight item
\     CALL AllowConditionalTag(1, eventString$)
!ED18279
    ENDIF
!AD18279  Always include the quantity attribute.
    CALL AllowConditionalTag(1, eventString$)
!ED18279


    !Now set up pricing method unique data
    IF (SL.IE.METHOD = 0) THEN BEGIN               ! if pricing method 0

      IF (SL.IE.SALEQUAN > 1) THEN BEGIN           ! if a deal quantity is set
        CALL AllowConditionalTag(2,eventString$)   ! dealQuantity dealPrice
      ENDIF ELSE BEGIN                             ! else a deal quantity is not set
        CALL AllowConditionalTag(3,eventString$)   ! unitPrice
      ENDIF                                        ! if a deal quantity is set

    ENDIF ELSE BEGIN                               ! else not pricing method 0

      IF (SL.IE.MPGROUP > 0) THEN BEGIN            ! if a multipricing group
        CALL AllowConditionalTag(4,eventString$)   ! groupID
      ENDIF                                        ! if a multipricing group

      CALL AllowConditionalTag(5,eventString$)     ! dealQuantity

      IF (SL.IE.METHOD = 1) THEN BEGIN             ! if pricing method 1
         CALL AllowConditionalTag(6,eventString$)  ! dealPrice
      ENDIF ELSE BEGIN                             ! else not pricing method 1
        IF (SL.IE.METHOD = 2) THEN BEGIN           ! if pricing method 2
          CALL AllowConditionalTag(7,eventString$) ! dealPrice unitPrice
        ENDIF ELSE BEGIN                           ! else not pricing method 2
          IF (SL.IE.METHOD = 3) OR                 \
             (SL.IE.METHOD = 4) THEN BEGIN         ! if pricing method 3 or 4
            CALL AllowConditionalTag(8,eventString$)   ! reducedPrice unitPrice
          ENDIF                                    ! if pricing method 3 or 4
        ENDIF                                      ! if pricing method 2
      ENDIF                                        ! if pricing method 1
    ENDIF                                          ! if pricing method 0

    IF (aefRequiredAge% NE 0) THEN BEGIN             ! if age restricted item
      CALL AllowConditionalTag(10, eventString$)
    ENDIF


    IF WIC.ENABLE THEN BEGIN                     ! if wic feature enabled
      CALL AllowConditionalTag(11, eventString$) ! wicEnabled
    ENDIF

    IF IR.LINKEDTO$ NE (CHR$(0) + CHR$(0)) THEN BEGIN   ! item linked
      CALL AllowConditionalTag(12, eventString$)
    ENDIF

    CALL RemoveConditionalTags(eventString$)
    marker$ = ?FindMarker$(eventString$)        ! marker = &##... where #=number
    aefl% = LEN(marker$)
    WHILE aefl% <> 0                               ! loop until no more markers
      marker% = val(mid$(marker$,2,aefl%))         ! marker value
      with$ = ?InQuote$("")
      ON marker% GOSUB itemIDSub, itemQualSub,      \ 1-2
                 itemDescSub, itemVoidSub,          \ 3-4
                 itemReturnSub, itemDepSub,         \ 5-6
                 itemRepSub, itemMethodSub,         \ 7-8
                 itemExtSub, itemTaxableSub,        \ 9-10
                 itemQtySub, itemDealQtySub,        \ 11-12
                 itemDealPriceSub, itemUnitSub,     \ 13-14
                 itemGrpSub, itemReducedSub,        \ 15-16
                 itemEnteredSub, itemWeightSub,     \ 17-18
                 itemFSSub, itemAgeSub,             \ 19-20
                 itemWICSub, itemLinkSub,           \ 21-22
                 itemLinkQualSub,                   \ 23
                 manNumberSub, valueSub,            \ 24-25
                 cpnReducesTaxSub, reducesFSBalSub, \ 26-27
                 cpnRefundedSub,                    \ 28
\AIO08970 Populate new itemType attribute.
\                itemDepartmentSub                  ! 29  D2424/18219
                 itemDepartmentSub,                 \ 29
                 itemTypeSub                        ! 30
!EIO08970
      CALL SubstituteMarker(marker$, eventString$, with$)
      marker$ = ?FindMarker$(eventString$)      ! next marker &##...
      aefl% = LEN(marker$)
    WEND
    SI.data$ = SI.data$ + eventString$


    ! If restricted item, append the <restrict> elements.
    IF (IR.INDICAT0 AND 02H) THEN BEGIN            ! if time restricted item
      IF (javaSOrestOptsRead = 0) THEN BEGIN       ! if restrictive optns not read yet
        CALL SETUP.RESTRICTED.DATA                 ! set up the string
      ENDIF                                        ! if restrictive options not read yet
      SI.data$ = SI.data$ + RESTRICTED.TIME.DATA$  ! restricted times
    ENDIF                                          ! if time restricted item


    FOR PRT.LINE.COUNTER = 1 TO NUM.PRT.LINES        ! handle all lines D21508
      eventString$ = ?aefXML$(52)                    ! <printLine>
      element$ = ?GetXMLEvent$(eventString$)
!AD21508 Handle each saved print line
\     IF (LEN(element$)>0 AND JAVA.ITEM.HDR.LINE.RAW$ <> "" ) THEN \
      IF (LEN(element$)>0) THEN \
\ED21508
      BEGIN
         call RemoveConditionalTags(eventString$)
         marker$ = ?FindMarker$(eventString$)           ! rawData attribute
         IF (LEN(marker$) > 0) THEN \
         BEGIN
!AD21508 Handle each saved print line
\            with$ = ?InQuote$(JAVA.ITEM.HDR.LINE.RAW$)
             with$ = ?InQuote$(JAVA.ITEM.PRT.LINE.RAW$(PRT.LINE.COUNTER))
!ED21508
             printLine.rawData$ = with$                 ! 6540
             CALL SubstituteMarker(marker$, eventString$, with$)
         ENDIF
         marker$ = ?FindMarker$(eventString$)           ! filteredData attribute
         IF (LEN(marker$) > 0) THEN \
         BEGIN
!AD21508 Handle each saved print line
\            with$ = ?InQuote$(JAVA.ITEM.HDR.LINE$)
             with$ = ?InQuote$(JAVA.ITEM.PRT.LINE$(PRT.LINE.COUNTER))
!ED21508
             CALL SubstituteMarker(marker$, eventString$, with$)
         ENDIF
         CALL ContextSensitivePrintLines(eventString$)  ! 6540
         SI.data$ = SI.data$ + eventString$
      ENDIF
    NEXT                                                ! D21508

!AD21508 This block was handled with the loop above.  Delete it.
\   eventString$ = ?aefXML$(52)                        ! <printLine>
\   element$ = ?GetXMLEvent$(eventString$)
\   IF (LEN(element$)>0 AND JAVA.ITEM.PRT.LINE.RAW$ <> "" ) THEN \
\   BEGIN
\      call RemoveConditionalTags(eventString$)
\      marker$ = ?FindMarker$(eventString$)           ! rawData attribute
\      IF (LEN(marker$) > 0) THEN \
\      BEGIN
\          with$ = ?InQuote$(JAVA.ITEM.PRT.LINE.RAW$)
\          printLine.rawData$ = with$                 ! 6540
\          CALL SubstituteMarker(marker$, eventString$, with$)
\      ENDIF
\      marker$ = ?FindMarker$(eventString$)           ! filteredData attribute
\      IF (LEN(marker$) > 0) THEN \
\      BEGIN
\          with$ = ?InQuote$(JAVA.ITEM.PRT.LINE$)
\          CALL SubstituteMarker(marker$, eventString$, with$)
\      ENDIF
\      CALL ContextSensitivePrintLines(eventString$)  ! 6540
\      SI.data$ = SI.data$ + eventString$
\   ENDIF
!ED21508

    IF (SL.IT.INDICAT3A = 6 OR SL.IT.INDICAT3A = 7) THEN BEGIN
       ! coupon
       eventString$ = ?aefXML$(69)                    ! </coupon>
    ENDIF ELSE BEGIN
       eventString$ = ?aefXML$(70)                    ! </itemEvent>
    ENDIF
    element$ = ?GetXMLEvent$(eventString$)
    IF LEN(element$)>0 THEN \
    BEGIN
       call RemoveConditionalTags(eventString$)
       SI.data$ = SI.data$ + eventString$
    ENDIF
  ENDIF

  SI.data$ = SI.data$ + ?ItemVoidAllowed$(NOT ITEM.VOIDED)

  IF (LEN(SI.data$) = 0) THEN \
  BEGIN
     event = -1
  ENDIF

  marker$ = ""
  with$ = ""
  eventString$ = ""
!AIO08970 Clear the variables holding the print line data,
!         since the event has already been generated.
!AD21508 Zero a counter instead of nulling the strings
\ JAVA.ITEM.HDR.LINE$ = ""
\ JAVA.ITEM.HDR.LINE.RAW$ = ""
\ JAVA.ITEM.PRT.LINE$ = ""
\ JAVA.ITEM.PRT.LINE.RAW$ = ""
  NUM.PRT.LINES=0                          ! no saved lines now
!ED21508
!EIO08970
  RETURN

itemIDSub:
  !First the item code (since SL.IT.ITEMCODE is set differently for EAN99
  !   AFTER the call to TSIEEC03, use the unpacked value in this case.
  IF (EAN99) THEN BEGIN
     with$ = ?InQuote$(?FormatForXML$(ITEM.99.UNPACKED$))
  ENDIF ELSE BEGIN
     IF (EMSS.ALTERED.UPC AND LEFT$(SL.IT.ITEMCODE$,1) = "=") THEN BEGIN
        ! EM altered the UPC, we will have to restore it.
        !!*******************************************************************
        !!** REFORMAT ITEM CODE BY TAKING OUT ADDED MFR NUMBER AND MARKER  **
        !!*******************************************************************
        IF MID$(SL.IT.ITEMCODE$,2,1) = "0" THEN BEGIN        ! NO CATEGORY NUMBER
          IF MID$(SL.IT.ITEMCODE$,8,1) = "0" THEN BEGIN      ! 4 DIGIT VELOCITY CODE
            with$ = ?InQuote$(?FormatForXML$(?StripLeading$("0",RIGHT$(SL.IT.ITEMCODE$,4))))
          ENDIF ELSE BEGIN
            with$ = ?InQuote$(?FormatForXML$(?StripLeading$("0",RIGHT$(SL.IT.ITEMCODE$,5))))
          ENDIF
        ENDIF ELSE BEGIN                                     ! CATEGORY NUMBER
          with$ = ?InQuote$(?FormatForXML$(?StripLeading$("0",JAVA.RESTORE.UPC(SL.IT.ITEMCODE$))))    ! RESTORE UPC
        ENDIF
     ENDIF ELSE BEGIN
        ! Item code has not been altered.
        with$ = ?InQuote$(?FormatForXML$(SL.IT.ITEMCODE$))
     ENDIF
  ENDIF

  RETURN
itemQualSub:
  ! set up item qualifier
  with$ = ?InQuote$(?FormatForXML$(ITEM.QUALIFIER$(SL.IT.INDICAT3B)))
  RETURN
itemDescSub:
  ! description
  with$ = ?InQuote$(?FormatForXML$(?NoSurroundSpace$(IR.ITEMNAME$)))
  RETURN
itemVoidSub:
  ! is item voided?
  with$ = ?TrueOrFalse$(ITEM.VOIDED)
  RETURN
itemReturnSub:
  ! is item a return?
  with$ = ?TrueOrFalse$(ITEM.RETURNED)
  RETURN
itemDepSub:
  ! is item a deposit?
  with$ = ?TrueOrFalse$(ITEM.DEPOSIT)
  RETURN
itemRepSub:
  ! is item repeatable?
  with$ = ?TrueOrFalse$(javaItemRepeatable)
  RETURN
itemMethodSub:
  ! return the pricing method
  with$ = GET.PRICING.METHOD$
  RETURN
itemExtSub:
    ! extended price
    with$ = ?InQuote$(EXTENDED.PRICE.FORMATTED$)
  RETURN
itemTaxableSub:
    ! Taxable?
    with$ =  + ?TrueOrFalse$(SL.IT.INDICAT1 AND 00F0H)
  RETURN
itemQtySub:
!AD18279  Always include the quantity attribute (except for a weight item)
\ IF EXT.STR THEN BEGIN                        ! if extension string
  IF EXT.STR AND NOT WT.ITEM THEN BEGIN        ! if extension string and not weight
!ED18279
!AD2479 Use SL.IE.QTYORWGT or SL.IE.SALEQUAN as the quantity, based upon flags
\   with$ = ?InQuote$(STR$(SL.IE.QTYORWGT))    ! format quantity
!AIO19427 
!   IF SL.IE.INDICAT1 AND 07EH THEN BEGIN      ! a weight or quantity is valid
    IF ( (SL.IE.INDICAT1 AND 07EH) OR   \      ! a weight or quantity is valid
         (SL.IE.INDICAT1 = 0) )         THEN BEGIN      
!EIO19427 
      with$ = ?InQuote$(STR$(SL.IE.QTYORWGT))  ! QTYORWGT field is valid
    ENDIF ELSE BEGIN
      with$ = ?InQuote$(STR$(SL.IE.SALEQUAN))  ! format sale quantity
    ENDIF
!ED2479
  ENDIF ELSE BEGIN                             ! else no extension string
    with$ = ?InQuote$("1")                     ! only a quantity of 1
  ENDIF                                        ! if extension string
  RETURN
itemDealQtySub:
  with$ = ?InQuote$(STR$(SL.IE.SALEQUAN))
  RETURN
itemDealPriceSub:
  with$ = ?InQuote$(PRICE.FORMATTED$(SL.IE.SALEPRIC))
  RETURN
itemUnitSub:
  with$ = ?InQuote$(PRICE.FORMATTED$(SL.IE.SALEPRIC))
  RETURN
itemGrpSub:
  with$ = ?InQuote$(STR$(SL.IE.MPGROUP))
  RETURN
itemReducedSub:
  with$ = ?InQuote$(PRICE.FORMATTED$(SL.IE.SALEPRIC2))
  RETURN
itemEnteredSub:
  IF ((SL.IT.INDICAT1 AND 2000H) <> 0) OR           \
      ((SL.IT.INDICAT1 AND 1000H) <> 0) THEN BEGIN
     with$ = ?TrueOrFalse$(-1)
  ENDIF ELSE BEGIN
     with$ = ?TrueOrFalse$(0)
  ENDIF
  RETURN
itemWeightSub:
  with$ =  ?InQuote$(STR$(SL.IE.QTYORWGT))
  RETURN
itemFSSub:
  ! Eligible for Food Stamps
  with$ = ?TrueOrFalse$(SL.IT.INDICAT1 AND 0008H)
  RETURN
itemAgeSub:
  with$ = ?InQuote$(STR$(aefRequiredAge%))
  RETURN
itemWICSub:
  with$ = ?TrueOrFalse$(WIC.ITEM)
  RETURN
itemLinkSub:
  with$ = ?InQuote$(?FormatForXML$(?StripLeading$("0",unpack$(IR.LINKEDTO$))))
  RETURN
itemLinkQualSub:
  with$ = ?InQuote$("")
  RETURN
manNumberSub:
  with$ = ?InQuote$(MANUFACTURER.NUMBER$)
  RETURN
valueSub:
  ! extended price
  with$ = ?InQuote$(EXTENDED.PRICE.FORMATTED$)
  RETURN
cpnReducesTaxSub:
  with$ = ?TrueOrFalse$(SL.IT.INDICAT1 AND 00F0H)
  RETURN
reducesFSBalSub:
   with$ = ?TrueOrFalse$(SL.IT.INDICAT1 AND 0008H)
  RETURN
cpnRefundedSub:
  with$ = ?TrueOrFalse$(ITEM.RETURNED)
  RETURN
!AD2424/18219 - Include dept number with items and coupons
itemDepartmentSub:
  with$ = ?InQuote$( RIGHT$( "000"+UNPACK$(IR.DEPARTME$), 3) )
  RETURN
!ED2424
!AIO08970 Add a new subroutine for populating the itemType attribute
itemTypeSub:
  with$ = ?InQuote$(itemType$)
  RETURN
!EIO08970




!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
! The optionsDataMsg event uses javaOptionsDataMsg to hold one option string.
! @tal10012003
! This event will get called once and builds the entire options string.
label.optionsDataMsg:
   eventString$ = ?aefXML$(90)                     ! <options>
   baseLength = LEN(eventString$)

   !<store name=&1 number=&2 phone=&3/>
   eventString$ = eventString$ + ?Options.store$
   !<!-- Price override reason codes (0 to N allowed)) -->
   eventString$ = eventString$ + ?Options.priceOverrideReason$
   !<!-- Refund reason codes (0 to N allowed) -->
   eventString$ = eventString$ + ?Options.refundReason$
   !<!-- Tare codes (0 to N allowed) -->
   eventString$ = eventString$ + ?Options.tareCode$
   !<!-- Transaction discount reason codes (0 to N allowed) -->
   eventString$ = eventString$ + ?Options.transactionDiscountReason$
   IF (LEN(eventString$) > baseLength) THEN BEGIN
      SI.data$ = SI.data$ + eventString$ + ?aefXML$(91)  ! </options>
      ! Send part of the options info.
      CALL javaGuiAppTrace(javaTraceUser$)         ! trace the user
      CALL javaAEFUserExit(event, SI.data$)        ! Call the user exit
      CALL javaGuiAppTrace(javaTraceApp$)          ! Trace back

      IF (LEN(SI.data$) > 0) THEN BEGIN                                          ! abort NOT requested
         IF javaGuiClass$ = "" THEN                                 \ IO21722 only if not pre-set
           javaGuiClass$ = "com.ibm.retail.AEF.data.ApplicationDataConnectorImpl"  ! set the class
         javaGuiMethod$ = "processEvent"                                         ! set the method
         CALL transmitToJVM(event, SI.data$, SI.data$, 2)
      ENDIF
      SI.data$ = ""
      eventString$ = ?aefXML$(90)
   ENDIF

   !<!-- Item discount reason codes (0 to N allowed) -->
   eventString$ = eventString$ + ?Options.itemDiscountReason$
   !<!-Tender definitions (0 to N allowed) -->
   eventString$ = eventString$ + ?Options.tenderDefinition$
   IF (LEN(eventString$) > baseLength) THEN BEGIN
      SI.data$ = SI.data$ + eventString$ + ?aefXML$(91)  ! </options>
      ! Send part of the options info.
      CALL javaGuiAppTrace(javaTraceUser$)         ! trace the user
      CALL javaAEFUserExit(event, SI.data$)        ! Call the user exit
      CALL javaGuiAppTrace(javaTraceApp$)          ! Trace back

      IF (LEN(SI.data$) > 0) THEN BEGIN                                             ! abort NOT requested
         IF javaGuiClass$ = "" THEN                                 \ IO21722 only if not pre-set
           javaGuiClass$ = "com.ibm.retail.AEF.data.ApplicationDataConnectorImpl"  ! set the class
         javaGuiMethod$ = "processEvent"                                         ! set the method
         CALL transmitToJVM(event, SI.data$, SI.data$, 2)
      ENDIF
      SI.data$ = ""
      eventString$ = ?aefXML$(90)
   ENDIF
   !<!-VAT Tax Codes (0 to N allowed) -->
   eventString$ = eventString$ + ?Options.vatTaxCode$
   !<storeOptions>
   eventString$ = eventString$ + ?Options.storeOptions$
   !<terminalOptions>
   eventString$ = eventString$ + ?Options.terminalOptions$
   !<!-- Department definitions (0 to N allowed) -->
   eventString$ = eventString$ + ?Options.departmentDefinition$
   !<!-Alternate Tax codes (0 to N allowed) -->
   eventString$ = eventString$ + ?Options.alternateTaxCode$
   !<!- Manual Tax codes (0 to N allowed) -->
   eventString$ = eventString$ + ?Options.manualTaxCode$
   !<!- No Tax codes (0 to N allowed) -->
   eventString$ = eventString$ + ?Options.noTaxCode$

   !If some options were added, then set event up to go, else we'll skip it
   IF (LEN(eventString$) > baseLength) THEN BEGIN
      SI.data$ = SI.data$ + eventString$ + ?aefXML$(91)              ! </options>
   ENDIF

  RETURN
! E6540


label.aefEMredemptionCpnMsg:

  SI.data$ = ""

  ! if no data set up correctly, then back out
  IF (JAVA.EM.RD.POINTS$ = "") THEN BEGIN        ! if no redemption points
    IF (JAVA.EM.RD.ITEMNAME$ = "") THEN BEGIN    ! if item name not set up
      IF (JAVA.EM.RD.PRINTLINE$ = "")            \ if print line not set up
      THEN BEGIN
        event = -1                               ! reset event flag
!AD21508 Zero a counter instead of nulling the strings
\       JAVA.ITEM.HDR.LINE$ = ""
\       JAVA.ITEM.PRT.LINE$ = ""
        NUM.PRT.LINES=0                          ! no saved lines now
!ED21508
        RETURN                                   ! get out
      ENDIF                                      ! if print line not set up
    ENDIF                                        ! if item name not set up
  ENDIF                                          ! if no redemption points

  REDEEMED.PTS = -1                              ! this is a redemption

  IF (JAVA.EM.PT.USERTYPE$ = "") THEN BEGIN      ! if user hasn't designated type
    PT.TYPE$ = "primary"                         ! base only has primary points
  ENDIF ELSE BEGIN                               ! else user designated type
    PT.TYPE$ = JAVA.EM.PT.USERTYPE$              ! use the user's type
  ENDIF                                          ! if user hasn't designated type

!AD21508 Set up the print line here
\ JAVA.ITEM.HDR.LINE$ = ""                       ! set up print lines
\ JAVA.ITEM.PRT.LINE$ = JAVA.EM.RD.PRINTLINE$
  IF NUM.PRT.LINES < JAVA.ITEM.PRT.MAX THEN BEGIN ! do not blow the array
    NUM.PRT.LINES = NUM.PRT.LINES + 1             ! one more line is saved
  ENDIF
  JAVA.ITEM.PRT.LINE$(NUM.PRT.LINES) = JAVA.EM.RD.PRINTLINE$
!ED21508

  CALL POINT.ONLY.ITEM(SI.data$,                 \ set up data for point only item
                       PT.TYPE$,                 \
                       JAVA.EM.RD.ITEMNAME$,     \
                       JAVA.EM.RD.POINTS$,       \
                       REDEEMED.PTS)

  ! clear variables
  JAVA.EM.RD.PRINTLINE$ = ""
  JAVA.EM.RD.ITEMNAME$ = ""
  JAVA.EM.RD.POINTS$ = ""
!AD21508 Zero a counter instead of nulling the strings
\ JAVA.ITEM.HDR.LINE$ = ""
\ JAVA.ITEM.PRT.LINE$ = ""
  NUM.PRT.LINES=0                                ! no saved lines now
!ED21508

  RETURN

label.aefUnitOfWorkMsg:

  IF (aefUnitOfWorkFlag = 0) THEN BEGIN          ! if no work has occurred
     event = -1                                  ! reset event flag
     RETURN                                      ! return without doing anything
  ENDIF                                          ! if no work has occurred


!76,<unitOfWork dataCategory="WORKSTATION_STATUS" unitOfWork="true"/>
!77,<unitOfWork dataCategory="WORKSTATION_STATUS" unitOfWork="false"/>
  eventString$ = ?aefXML$(76) + ?aefXML$(77)
  element$ = ?GetXMLEvent$(eventString$)
  IF LEN(element$) THEN \
  BEGIN
     SI.data$ = SI.data$ + eventString$
  ENDIF


  eventString$ = ""
  element$ = ""
  aefUnitOfWorkFlag = 0                          ! reset unit of work flag

  RETURN


label.jGuiEndAppEvents:

RETURN

!AD2424/17667 Add a Till Exchange Event
label.tillExchangeOccurred:
  eventString$ = ?aefXML$(150)           ! 150,<tillExchangeOccurred/>
  SI.data$ = SI.data$ + eventString$
RETURN
!ED2424/17667

!AIO08970 Add support for the <transactionUpdate> event.
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
label.transactionUpdate:

!48,<transactionUpdate [(1)transactionDiscountAllowed=&1]
!   [(2)itemVoidAllowed=&2] [(3)paymentSentForHostProcessing="true"]/>

  eventString$ = ""
  eventString$ = ?aefXML$(48)                  ! transactionUpdate
  element$ = ?GetXMLEvent$(eventString$)
  IF LEN(element$) THEN \
  BEGIN
    CALL AllowConditionalTag(1,eventString$)   ! transactionDiscountAllowed="true|false"
    CALL AllowConditionalTag(2,eventString$)   ! itemVoidAllowed="true|false"
    ! JAVA.PAYMENT.SENT.TO.HOST is set by the payment system.
    IF (JAVA.PAYMENT.SENT.TO.HOST <> 0) THEN BEGIN
      CALL AllowConditionalTag(3,eventString$) ! paymentSentForHostProcessing="true"
    ENDIF
    CALL RemoveConditionalTags(eventString$)
    marker$ = ?FindMarker$(eventString$)       ! marker = &##... where #=number
    with$ = ?TrueOrFalse$(transDiscAllowed)
    CALL SubstituteMarker(marker$, eventString$, with$)
    marker$ = ?FindMarker$(eventString$)
    with$ = ?TrueOrFalse$(itemVoidAllowed)
    CALL SubstituteMarker(marker$, eventString$, with$)
  
!AIO22151  
    ! Append if a suspend/retrieve is in progress and beginning or ending.  
    IF (TS.RETV.IN.PROGRESS) THEN BEGIN
      ! Define which is happening.
      IF (TS.RETV.BEGIN) THEN BEGIN
         with$ = "begin"
      ENDIF ELSE BEGIN
         with$ = "end"
      ENDIF
          
      ! Get the index where transactionUpdate event ends.
      I2A% = MATCH ( "/>", eventString$, 1)
      work1$ = RIGHT$(eventString$, 2)
      
      ! Append attribute.
      eventString$ = LEFT$(eventString$, I2A% - 1) + \
                   " retrieveProcessing=" + CHR$(34) + with$ + CHR$(34) + work1$
    ENDIF  
!EIO22151  


    SI.data$ = SI.data$ + eventString$
  ENDIF


  RETURN
!EIO08970

!AIO12917 Add support for the user events via label aefUserEvent.
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
label.aefUserEvent:

! user events are not looked up in xmlevents.
! The events are created in the user code and passed as is
  eventString$ = JAVA.USEREVENT.DATA$
  IF LEN(eventString$) THEN \
  BEGIN
    SI.data$ = SI.data$ + eventString$
!      If USEREVENT.FLAG set to 1 then  save the user event along with header lines
!      until the transaction starts, then send them in the order received.
    IF (JAVA.USEREVENT.FLAG = 1) THEN BEGIN
      HEADER.STR$ = HEADER.STR$ + SI.data$
      SI.data$ = ""
!         We will abend if we try to save more than 64 KB in a string.
!         To prevent the need to add BIGLEN to the INP file, we stop
!         at 32 KB.  When almost at 32 KB, put out the saved data and
!         then continue to buffer again.
!         32 KB is approximately 90 - 120 print lines.
      IF (LEN(HEADER.STR$) > 32000) THEN BEGIN ! 767 bytes to spare
        SI.data$ = HEADER.STR$                 ! put it out now
        HEADER.STR$ = ""                       ! nothing buffered now
      ENDIF
    ENDIF
  ENDIF


  RETURN
!EIO12917

label.aefUserNuoEvent:
!call TraceRh("Evento 55 en javaaefr")
  eventString$ = ?aefXML$(200)         ! Nuo Interface
      !call TraceRh("eventString$" + eventString$)
      !!!call TraceRh("Nuo.Xml.Cate$" + Nuo.Xml.Cate$)    
      !!call TraceRh("Nuo.Xml.Clave$" + Nuo.Xml.Clave$)
      !call TraceRh("Nuo.Xml.Valor$" + Nuo.Xml.Valor$)
CALL SubstituteMarker("&1", eventString$,Nuo.Xml.Cate$)  
CALL SubstituteMarker("&2", eventString$,Nuo.Xml.Clave$)  
CALL SubstituteMarker("&3", eventString$,Nuo.Xml.Valor$) 
!call TraceRh("eventString$" + eventString$)
 
SI.data$ = eventString$

!call TraceRh("SI.data$ en javaaefr "+SI.data$)  
RETURN


END SUB                                  !

!AIRkhg
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
!   function javaAefInit
!
!   parameters:          initialize for Java AEF Gui processing
!   return data:         none
!   data areas modified: enumerated values of events
!                        java class & method variables
!   references:          java gui initialization user exit
!
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
FUNCTION javaAefInit PUBLIC

      IF (callAefHooks) THEN BEGIN
         aefEnabled = -1
      ENDIF
      aefUnitOfWorkFlag = -1

      javaPipeNum = 91

      ! do the descriptor file now
      TS.ER.RETURN = -1                            ! prepare for error

      OPEN "R::ADX_IPGM:EAM2DESC.DAT" RECL 49 AS 64             \ Open descriptor file
                                     NOWRITE NODEL !

      IF (TS.ER.RETURN <> 0) THEN BEGIN            ! error on open
         ! now, read descriptors
         READ #64, 2321; javaNoScale$              ! "No Scale"
         READ #64, 2322; javaScaleLabelData$       ! scale label data
         READ #64, 2323; javaScaleMotionErr$       ! scale in motion error
         READ #64, 2324; javaScaleOtherErr$        ! scale other error

         CLOSE 64                                  ! close descriptors file
      ENDIF                                        ! error on open





      CALL ?AEF.INIT


END FUNCTION



!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
! handle SureVision options
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
SUB handleOptionsAEF PUBLIC

   !Begin @tal10102003
!   call javaGuiAppTrace(javaTraceGui)           ! Trace application specific
!   CALL javaAEFEvent(optionsLoadingStartedMsg)  ! Options loading started
!   call javaGuiAppTrace(javaTraceApp$)           ! Trace application specific

   call javaGuiAppTrace(javaTraceGui$)           ! Trace application specific
   CALL javaAEFEvent(optionsDataMsg)             ! Options data
   call javaGuiAppTrace(javaTraceApp$)           ! Trace application specific

!   call javaGuiAppTrace(javaTraceGui)           ! Trace application specific
!   CALL javaAEFEvent(optionsLoadingEndedMsg)    ! Options loading started
!   call javaGuiAppTrace(javaTraceApp$)           ! Trace application specific
   !End @tal10102003

END SUB


!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
! Function aefBlockForJava
!
!   This function blocks until the pipe created in the JVM is opened successfully.
!   return data:   -1 If the java pipe is detected indicating java health.
!                   0 If the no check for the java pipe was required.
!
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
FUNCTION aefBlockForJava PUBLIC RECURSIVE            ! aefBlockForJava

  INTEGER*2                                          \
            aefBlockForJava

   aefBlockForJava = 0

   IF (blockForJava = -1) THEN BEGIN
     WHILE(javaHealth = 0)                           ! wait for the JVM before
        WAIT;5000                                    ! proceeding
     WEND
     aefBlockForJava = -1                            ! We successfully waited for
                                                     ! the JVM pipe.
   ENDIF

END FUNCTION                                         ! aefBlockForJava



