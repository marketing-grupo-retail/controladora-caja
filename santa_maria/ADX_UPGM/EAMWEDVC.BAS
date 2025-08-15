\/* TIME STAMP BLOCK-************************************************
\** END OF TIME STAMP BLOCK ****************************************/
\/******************************************************************/
\/*                                                                */
\/*      MODULE NAME: EAMWEDVC                                     */
\/*                                                                */
\/*      DESCRIPTIVE NAME: WIC SmartCard Support for Enhanced EFT  */
\/*                                                                */
\/*      COPYRIGHT:                                                */
\/*      5696-536 THIS MODULE IS "RESTRICTED MATERIALS OF IBM"     */
\/*      (C) COPYRIGHT IBM CORP 1986, 2004 ALL RIGHTS RESERVED     */
\/*      LICENSED MATERIALS - PROPERTY OF IBM REFER TO COPYRIGHT   */
\/*      INSTRUCTIONS FORM NUMBER G120-2083                        */
\/*                                                                */
\/*      STATUS: RELEASE 2 LEVEL 0                                 */
\/*                                                                */
\/*      PERSON RESPONSIBLE = LOCHNER, TERRY                       */
\/*                                                                */
\/*      FUNCTION: EAMWEDVC                                        */
\/*                                                                */
\/*                THIS MODULE CONTROLS THE FLOW OF EFTS MESSAGES  */
\/*                WITHIN THE 4683. MESSAGES FLOW BETWEEN THE      */
\/*                HOST, THE PINPAD/MSR AND THE SALES APPLICATION. */
\/*                                                                */
\/*      NOTES:                                                    */
\/*                                                                */
\/*         DEPENDENCIES:                                          */
\/*                THE ROUTINES IN THIS MODULE MUST BE CALLED AT   */
\/*                APPROPRIATE TIMES FROM THE TERMINAL SALES       */
\/*                MAINLINE.                                       */
\/*                                                                */
\/*         RESTRICTIONS: NONE                                     */
\/*                                                                */
\/*         REGISTER CONVENTIONS:                                  */
\/*                                                                */
\/*            RESTRICTED REGISTERS = N/A                          */
\/*                                                                */
\/*            REGISTERS USED = N/A                                */
\/*                                                                */
\/*      MODULE TYPE: CBASIC                                       */
\/*                                                                */
\/*         PROCESSOR: CBASIC COMPILER                             */
\/*         MODULE SIZE: 200 LOC                                   */
\/*                                                                */
\/*         ATTRIBUTES: REENTRANT                                  */
\/*                                                                */
\/*      ENTRY POINT:                                              */
\/*                                                                */
\/*         PURPOSE:                                               */
\/*                                                                */
\/*                                                                */
\/*                ***** RETURN CONTROL *****                      */
\/*                                                                */
\/*         LINKAGE: THIS MODULE IS CALLED FROM TERMINAL MAINLINE. */
\/*                                                                */
\/*                                                                */
\/*      INPUT:                                                    */
\/*                                                                */
\/*         SYMBOLIC NAME:                                         */
\/*                                                                */
\/*         DESCRIPTION:                                           */
\/*                                                                */
\/*                                                                */
\/*      OUTPUT:  NONE                                             */
\/*                                                                */
\/*      EXIT-NORMAL: RETURN TO CALLER                             */
\/*                                                                */
\/*         RETURN CODE:                                           */
\/*                                                                */
\/*      EXIT-ERROR: RETURN TO CALLER                              */
\/*                                                                */
\/*         RETURN CODE:                                           */
\/*                                                                */
\/*            REASON CODE:                                        */
\/*            MESSAGE ID:                                         */
\/*                                                                */
\/*                                                                */
\/*                                                                */
\/*                                                                */
\/*      EXTERNAL REFERENCES:                                      */
\/*                                                                */
\/*         ROUTINES:                                              */
\/*           NAME: TSMSECRC - MESSAGE SEGMENTER, RECEIVE ROUTINE  */
\/*           NAME: TSMSECXM - MESSAGE SEGMENTER, TRANSMIT ROUTINE */
\/*           NAME: TSMSECIN - MESSAGE SEGMENTER, INITIALIZATION   */
\/*           NAME: TSDLECML - DATA LINK CONTROL MAINLINE          */
\/*           NAME: TSUPEC47 - USER EXIT, REFT INITIALIZATION      */
\/*           NAME: TSUPEC48 - USER EXIT, REFT TERMINATION         */
\/*           NAME: SUBSTR   - ASSEMBLER ROUTINE                   */
\/*           NAME: GETN2    - ASSEMBLER ROUTINE                   */
\/*           NAME: PUTN2    - ASSEMBLER ROUTINE                   */
\/*                                                                */
\/*         DATA AREAS:                                            */
\/*                                                                */
\/*           MODIFIED:   NONE                                     */
\/*                                                                */
\/*           REFERENCED: NONE                                     */
\/*                                                                */
\/*         CONTROL BLOCKS: NONE                                   */
\/*                                                                */
\/*         MACROS: NONE                                           */
\/*                                                                */
\/*         TABLES: NONE                                           */
\/*                                                                */
\/*      CHANGE ACTIVITY: LEVEL 0                                  */
\/*                                                                */
\/*                                                                */
\/*                                                                */
\/*                                                                */
\/*      CHANGE DATE:                                              */
\/*                                                                */
\/* END-OF-SPECIFICATIONS ******************************************/

\ REM!!****************  OVERVIEW OF PROGRAM  *************************
\ REM!! MODULE NAME:
\ REM!! FUNCTION:
\ REM!!
\ REM!!
\ REM!!*****************************************************************

\ REM!!*********************  B B D L  ********************************

%ENVIRON T              ! terminal program

\ REM!!*******************  INCLUDE FILES  ****************************
\ REM!!
\ REM!! %INCLUDE EAMSOPTS.J86   STORE OPTIONS
\ REM!! %INCLUDE EAMTERMS.J86   TMNL STATUS FILE
\ REM!! %INCLUDE EAMTSWKG.J86   TMNL WORKING STORAGE
\ REM!! %INCLUDE EAMETWKG.J86   EFTS WORKING STORAGE
\ REM!! %INCLUDE EAMASMRT.J86   ASSEMBLER ROUTINES
\ REM!!
\ REM!!*****************************************************************

%INCLUDE EAMSOPTS.J86
%INCLUDE EAMTOPTS.J86
%INCLUDE EAMTERMS.J86
%INCLUDE EAMTSWKG.J86
%INCLUDE EAMETWKG.J86

\ REM!!*******************  LOCAL VARIABLES  **************************

   INTEGER*2   WIC.MAX.BUFFER    !Max buffer size

\ REM!!*******************  GLOBAL VARIABLES  **************************

   INTEGER*1 GLOBAL                    \
             PP.80.REMOVE.CARD.SENT    ! flag a remove card message was sent 

   !Current "state" of WIC EBT - @tal01112005
   !(i.e. which command is in process)
   ! 1 = _00 Initialize
   ! 2 = _10 Get PAN
   ! 3 = _20 Read balance
   ! 4 = _30 Debit balance
   ! 5 = _40 Block card
   ! 6 = _50 End Transaction (display msg)
   ! 7 = _60 Authenticate
   ! 8 = _70 Deactivate
   ! 9 = _80 Remove Card
   INTEGER*2 GLOBAL WIC.EBT.STATE   

\ *********************** more include files ***********************

%INCLUDE EAMWEDLC.J86   !Data Link Control for WIC device


!*******************  Initialize (_00) **************************
!Inputs:
!- Max buffer size
!- Processing mode
!  0 = Normal
!  1 = Training
!  2 = Certification
!  3 = Certification Training
!- State codes, concatenation of 2-character state codes for each state
!  supported (e.g. TXARLA, for Texas, Arkansas, Louisiana)
!
!Outputs:
!- 4-digit Error code
!- State codes activated (e.g. TXARLA)
!- Version number for each state code
!  "FF" means not found
!  e.g.  "01FF01" implies TX=01, AR=not found, LA=01
!
!Implementation notes:
!- Message to pinpad includes current date/time
!- Number of valid states in message to pinpad is determined by taking the 
!  length of the string of state codes divided by 2.
!
!Description:
!- Format a message: _00yyyymmddhhmmssmmmppnnaabb..cc
!  yyyymmddhhmmss is current date/time
!  mmm is the maximum buffer size, with leading zeroes (MaxBufSize)
!  pp is the processing mode, with leading zero (ProcMode)
!  nn is the number of 2-character state codes to follow
!  aabb..cc are the valid state codes (States$)
!- Send the message and wait for a response
!- If the response string is NULL or it does not begin with "_01", there was a
!  problem communicating with the pinpad.  Return ErrCode$= "FFFF" for the 
!  application to handle appropriately.
!- Parse the return string: _01EEEEnnaabbcc...rarbrc...
!  EEEE is the error code (ErrCode$)
!  nn is the number of 2-character state codes to follow
!  aabbcc are the valid state codes (States$)
!  rarbrc are the version numbers for the respective state codes (StatesVer$)

SUB WICInitialize(MaxBufSize, ProcMode, States$, ErrCode$, StatesVer$) PUBLIC
   INTEGER*2   MaxBufSize
   INTEGER*1   ProcMode
   STRING      States$, ErrCode$, StatesVer$
   STRING      Req$, Reply$
   INTEGER*1   NSSent, NSRcvd, RC

   WIC.EBT.STATE = 1    !@tal01112005
   
   !Save the max buffer size being sent to the device in a local variable
   !  so we have it to check for on the redeem benefits request (_30).
   WIC.MAX.BUFFER = MaxBufSize

   !Calculate number of state codes.
   NSSent = LEN(States$) / 2

   !Format the request message
   Req$ =   "_00" +                                \ Message type
            "20" + DATE$ +                         \ Current date as yyyymmdd
            TIME$ +                                \ Current time as hhmmss
            RIGHT$("000" + STR$(MaxBufSize), 3) +  \ Max buff size with lead 0s
            RIGHT$("00" + STR$(ProcMode), 2) +     \ Proc mode with leading 0s
            RIGHT$("00" + STR$(NSSent), 2) +       \ Nbr of states with lead 0s
            States$                                ! List of states

   Reply$ = "_01"    !@tal09142004
   
   !Send the request and wait 30 seconds for a reply.
   CALL WICSendAndWait(Req$, 30, Reply$, RC)

   !Assume there is a problem.
   ErrCode$ = "FFFF"
   IF (RC = 0) THEN BEGIN
      !A response was received
      !It should begin with _01.  If not, return error code FFFF
      IF (LEFT$(Reply$, 3) = "_01") THEN BEGIN
         !Good reply. Parse the reply into returnable parts.
         ErrCode$    = MID$(Reply$, 4, 4)
         NSRcvd      = VAL(MID$(Reply$, 8, 2))
         States$     = MID$(Reply$, 10, 2 * NSRcvd)
         StatesVer$  = MID$(Reply$, 10 + (2 * NSRcvd), 2 * NSRcvd)
      ENDIF
   ENDIF ELSE IF (RC < 0) THEN BEGIN
      ErrCode$ = "RTRY"
   ENDIF ELSE IF (RC = 1) THEN BEGIN
      ErrCode$ = "100000"     !Timeout err   
   ENDIF ELSE IF (RC = 2) THEN BEGIN
      ErrCode$ = "200000"     !Cashier cancel
   ENDIF

   !Cleanup.
   Req$ = ""
   Reply$ = ""
   
   WIC.EBT.STATE = 0    !@tal01112005

END SUB

!*******************  Get PAN (_10) **************************
!Inputs:
!- Processing mode (see Initialize for values)
!
!Outputs:
!- 4-digit Error code
!- State code for which the card is active (e.g. TX)
!- PAN (Personal Account Number)
!- Issuer (Benefits Issuing Entity, to be logged in Claims File)
!
!Implementation notes:
!- Message to pinpad includes current date/time
!
!Description:
!- Format a message: _10yyyymmddhhmmsspp
!  yyyymmddhhmmss is current date/time
!  pp is the processing mode with leading zero (ProcMode)
!- Send the message and wait for a response
!- If the response string is NULL or it does not begin with "_11", there was a
!  problem communicating with the pinpad.  Return ErrCode$= "FFFF" for the 
!  application to handle appropriately.
!- Parse the return string: _11EEEESSllp...pa...a
!  EEEE is the error code (ErrCode$)
!  SS is the state for which the card is valid (State$)
!  ll is the length of the PAN
!  p...p is the PAN (PAN$)

SUB WICGetPAN(ProcMode, ErrCode$, State$, PAN$) PUBLIC
   STRING      ErrCode$, State$, PAN$
   INTEGER*1   ProcMode
   STRING      Req$, Reply$
   INTEGER*1   PANLength, RC

   WIC.EBT.STATE = 2    !@tal01112005
   
   !Format the request message
   Req$ =   "_10" +                                \ Message type
            "20" + DATE$ +                         \ Current date as yyyymmdd
            TIME$ +                                \ Current time as hhmmss
            RIGHT$("00" + STR$(ProcMode), 2)       ! Proc mode with leading 0s

   Reply$ = "_11"    !@tal09142004
               
   !Send the request and wait indefinitely (well, just an hour) for a reply.
   CALL WICSendAndWait(Req$, 3600, Reply$, RC)

   !Assume there is a problem.
   ErrCode$ = "FFFF"
   IF (RC = 0) THEN BEGIN
      !A response was received
      !It should begin with _11.  If not, return error code FFFF
      IF (LEFT$(Reply$, 3) = "_11") THEN BEGIN
         !Good reply. Parse the reply into returnable parts.
         ErrCode$    = MID$(Reply$, 4, 4)
         State$      = MID$(Reply$, 8, 2)
         PANLength   = VAL(MID$(Reply$, 10, 2))
         PAN$        = MID$(Reply$, 12, PANLength)
      ENDIF
   ENDIF ELSE IF (RC = 1) THEN BEGIN
      ErrCode$ = "100000"     !Timeout err   
   ENDIF ELSE IF (RC = 2) THEN BEGIN
      ErrCode$ = "200000"     !Cashier cancel
   ENDIF ELSE IF (RC = -5) THEN BEGIN
      ErrCode$ = "500000"     !Card in reader device
   ENDIF

   !Cleanup.
   Req$ = ""
   Reply$ = ""

   WIC.EBT.STATE = 0    !@tal01112005
   
END SUB

!*******************  Read Balance (_20) **************************
!Inputs:
!- State code
!
!Outputs:
!- 4-digit Error code
!- State code for which the card is active (e.g. TX)
!- Last date to spend (to print on the Current Balance receipt)
!- First date to spend (to be logged in the Claims File)
!- Issuer (Benefits Issuing Entity)
!- WIC Items (a string of 10-byte WIC items concatenated)
!
!Implementation notes:
!- Message to pinpad includes current date/time
!
!Description:
!- Format a message: _20SSyyyymmddhhmmss
!  SS is the state for which the card is valid (State$)
!  yyyymmddhhmmss is current date/time
!- Send the message and wait for a response
!- If the response string is NULL or it does not begin with "_21", there was a
!  problem communicating with the pinpad.  Return ErrCode$= "FFFF" for the 
!  application to handle appropriately.
!- Parse the return string: _21EEEESScyyyymmddyyyymmddb...bssmmnniiii...ii
!  EEEE is the error code (ErrCode$)
!  SS is the state for which the card is valid (State$)
!  c is 0 if this is the last message of items, 1 if more messages to follow
!  yyyymmdd is the last date to spend (LastDate$)
!  yyyymmdd is the first date to spend (FirstDate$)
!  b...b is the 15-character Benefits Issuing Entity (Issuer$)
!  ss is sequential block number
!  mm is the total number of items in the balance
!  nn is the number of items in the current block
!  iiii...ii is a concatenated string of 10-byte WIC items (WICItems$)
!- If more records are expected, read again until a response is received.  
!  Parse it as described above and append the additional items to WICItems$.
!  Continue until the last record is received.

SUB WICReadBalance(State$, ErrCode$, LastDate$, FirstDate$, Issuer$, WICItems$) PUBLIC
   STRING      State$, ErrCode$, LastDate$, FirstDate$, Issuer$, WICItems$
   STRING      Req$, Reply$
   INTEGER*1   RC, Continue, Done
   INTEGER*2   SS, MM, NN

   WIC.EBT.STATE = 3    !@tal01112005

   !Format the request message
   Req$ =   "_20" +                                \ Message type
            State$ +                               \ State code
            "20" + DATE$ +                         \ Current date as yyyymmdd
            TIME$                                  ! Current time as hhmmss

   !Use the LastDate, FirstDate and Issuer from the first response only.
   LastDate$ = ""
   FirstDate$ = ""
   Issuer$ = ""

   !Initialize list of items to empty string so we can start appending right away
   WICItems$ = ""

   Reply$ = "_21"    !@tal09142004

   !Send the request and wait 30 seconds for a reply.
   CALL WICSendAndWait(Req$, 30, Reply$, RC)

   Done = 0 !Not done yet
   WHILE NOT(Done)
      Done = -1   !Assume done unless we get something to continue with   
      IF (RC = 0) THEN BEGIN
         !A response was received
         !It should begin with _21.  If not, return error code FFFF
         IF (LEFT$(Reply$, 3) = "_21") THEN BEGIN
            !Good reply. Parse the reply into returnable parts.
            ErrCode$    = MID$(Reply$, 4, 4)
            State$      = MID$(Reply$, 8, 2)
            Continue    = VAL(MID$(Reply$, 10, 1))
            IF (LastDate$ = "") THEN BEGIN
               LastDate$ = MID$(Reply$, 11, 8)
            ENDIF
            IF (FirstDate$ = "") THEN BEGIN
               FirstDate$ = MID$(Reply$, 19, 8)
            ENDIF
            IF (Issuer$ = "") THEN BEGIN
               Issuer$ = MID$(Reply$, 27, 15)
            ENDIF
            !Can't think of a use for the sequence number and total number of 
            !  items but parse into variable anyway in case we want to log it 
            !  someday.
            SS = VAL(MID$(Reply$, 42, 2))
            !Same for total number of items
            MM = VAL(MID$(Reply$, 44, 2))
            NN = VAL(MID$(Reply$, 46, 2))
            !Items are 10-chars each, so grab rightmost chars based on NN
            WICItems$ = WICItems$ + RIGHT$(Reply$, NN * 10)

            !Now look to see if we need to receive more.
            IF (Continue = 1) THEN BEGIN
               !Need to receive next msg and spin around again.
               Done = 0    
               !Wait up to 10 secs for the next msg
               CALL WICReceive(10, Reply$)
               IF (Reply$ = "") THEN BEGIN
                  !No data rcvd, so set up ret code to force error.
                  RC = 1
               ENDIF
            ENDIF
         ENDIF ELSE BEGIN
            !Bad reply.
            ErrCode$ = "FFFF"
         ENDIF
      ENDIF ELSE IF (RC = 1) THEN BEGIN
         ErrCode$ = "100000"     !Timeout err   
      ENDIF ELSE IF (RC = 2) THEN BEGIN
         ErrCode$ = "200000"     !Cashier cancel
      ENDIF ELSE IF (RC = -5) THEN BEGIN
         ErrCode$ = "500000"     !Card in reader device
      ENDIF ELSE BEGIN
         !Something else happened.
         ErrCode$ = "FFFF"
      ENDIF

   WEND

   !Cleanup.
   Req$ = ""
   Reply$ = ""

   WIC.EBT.STATE = 0    !@tal01112005

END SUB

!*******************  Decrement Redeemed Benefits (_30) **********************
!Inputs:
!- State code for which the card is valid (e.g. TX)
!- WIC Items to be debited from balance (a string of 10-byte WIC items 
!  concatenated)
!
!Outputs:
!- 4-digit Error code
!- State code for which the card is valid (e.g. TX)
!- Transaction signature (to be logged in the Claims File)
!
!Description:
!- Format a message: _30SScssmmnni...i
!  SS is the state for which the card is valid (State$)
!  c is 0 for the last message of items, 1 is more messages to follow
!  ss is sequential block number
!  mm is the total number of items in the balance
!  nn is the number of items in the current block
!  i...i is a concatenated string of 10-byte WIC items (WICItems$)
!- Send the message.
!- If more records need to be sent because the record size would exceed the 
!  maximum buffer size, format it as described above for the additional items.
!  Continue until the last record is sent.
!- Wait for a response.
!- If the response string is NULL or it does not begin with "_31", there was a
!  problem communicating with the pinpad.  Return ErrCode$= "FFFF" for the 
!  application to handle appropriately.
!- Parse the return string: _31EEEESSllt...t
!  EEEE is the error code (ErrCode$)
!  SS is the state for which the card is valid (State$)
!  ll is the length of the transaction signature
!  t...t is the transaction signature (Signature$)

SUB WICDebitBalance(State$, WICItems$, ErrCode$, Signature$) PUBLIC
   STRING      State$, WICItems$, ErrCode$, Signature$
   STRING      Req$, Reply$
   INTEGER*2   SS, MM, MaxNN, ItemPtr, ItemsLeft, Continue, NN, TranLength
   STRING      Items$
   INTEGER*1   RC

   WIC.EBT.STATE = 4    !@tal01112005
   
   !Total items is length of WICItems$ / 10
   MM = LEN(WICItems$) / 10

   !Max number of items that can go in each msg is (WIC.MAX.BUFFER - header/10)
   MaxNN = (WIC.MAX.BUFFER - 12) / 10

   !Start at the 1st item
   ItemPtr = 1

   !Initialize current block # to 1
   SS = 1

   !Used to tell if we had a problem, so initialize.
   Req$ = ""
   Reply$ = ""

   WHILE ItemPtr <= MM
      !Remaining items is (MM - ItemPtr) + 1
      ItemsLeft = (MM - ItemPtr) + 1

      IF (ItemsLeft > MaxNN) THEN BEGIN
         Continue = 1      !We are going to have a continuation msg
         NN = MaxNN        !Max items will be in this msg
      ENDIF ELSE BEGIN
         Continue = 0      !We are NOT going to have a continuation msg
         NN = ItemsLeft    !Only Items Left will be in this msg
      ENDIF
      !Pull out the items starting at (ItemPtr-1)*10+1 for NN*10 chars
      Items$ = MID$(WICItems$, ((ItemPtr - 1) * 10) + 1, NN * 10)

      !Format the request message
      Req$ =   "_30" +                                \ Message type
               State$ +                               \ State code
               STR$(Continue) +                       \ Continue
               RIGHT$("00" + STR$(SS), 2) +           \ Block # with leading 0s
               RIGHT$("00" + STR$(MM), 2) +           \ Total # of items
               RIGHT$("00" + STR$(NN), 2) +           \ items in this block
               Items$                                 ! WIC items

      !Begin @tal10062004
      !If we are sending a continuation message, then send it and spin around
      !  to format the next one.  If this is the last one, then don't send it 
      !  yet, so that the last message can be done with the SendAndWait which
      !  properly deals with a timeout and/or operator clear key.
      IF (Continue = 1) THEN BEGIN
         !Send the request.
         CALL WICSend(Req$, RC)
      ENDIF ELSE BEGIN
         !Ok to leave.
         RC = 0
      ENDIF
      
      !If sent then increment to next item, else set up to return to retry
      IF (RC = 0) THEN BEGIN
         ItemPtr = ItemPtr + NN
         SS = SS + 1
      ENDIF ELSE BEGIN
         Req$ = ""         !So we don't wait for receive msg
         ItemPtr = MM + 1  !Set up to leave while loop
      ENDIF
      !End @tal10062004

   WEND

   !Wait up to 3600 secs for the reply if we sent something
   IF (Req$ <> "") THEN BEGIN
      !Begin @tal10052004 - Need to wait for the right type of response (_31)
      Reply$ = "_31"    !@tal09142004

      !Send the request and wait 3600 seconds for a reply.
      CALL WICSendAndWait(Req$, 3600, Reply$, RC)
      !End @tal10052004
   ENDIF

   !Assume there is a problem.
   ErrCode$ = "FFFF"
   IF (RC = 0) THEN BEGIN
      IF (Reply$ <> "") THEN BEGIN
         !A response was received
         !It should begin with _31.  If not, return error code FFFF
         IF (LEFT$(Reply$, 3) = "_31") THEN BEGIN
            !Good reply. Parse the reply into returnable parts.
            ErrCode$    = MID$(Reply$, 4, 4)
            State$      = MID$(Reply$, 8, 2)
            TranLength  = VAL(MID$(Reply$, 10, 2))
            Signature$  = RIGHT$(Reply$, TranLength)
         ENDIF
      ENDIF
   ENDIF ELSE IF (RC = 1) THEN BEGIN
      ErrCode$ = "100000"     !Timeout err   
   ENDIF ELSE IF (RC = 2) THEN BEGIN
      ErrCode$ = "200000"     !Cashier cancel
   ENDIF ELSE IF (RC = -5) THEN BEGIN
      ErrCode$ = "500000"     !Card in reader device
   ENDIF

   !Cleanup.
   Req$ = ""
   Reply$ = ""
   Items$ = ""

   WIC.EBT.STATE = 0    !@tal01112005

END SUB

!*******************  Block Card (_40) **************************
!Inputs:
!- State code for which the card is valid (e.g. TX)
!- Date/Time of card block from hot card list (YYYYMMDDHHMMSS)
!- Reason Code from hot card list
!
!Outputs:
!- 4-digit Error code
!- State code for which the card is valid (e.g. TX)
!
!Description:
!- Format a message: _40SSyyyymmddhhmmssrrrr
!  SS is the state for which the card is valid (State$)
!  yyyymmddhhmmss is the date/time of the card block from the hot card list 
!   file (BlockDateTime$)
!  rrrr is the Reason Code from hot card list (Reason$)
!- Send the message and wait for a response
!- If the response string is NULL or it does not begin with "_41", there was a
!  problem communicating with the pinpad.  Return ErrCode$= "FFFF" for the 
!  application to handle appropriately.
!- Parse the return string: _41EEEESS
!  EEEE is the error code (ErrCode$)
!  SS is the state for which the card is valid (State$)

SUB WICBlockCard(State$, BlockDateTime$, Reason$, ErrCode$) PUBLIC
   STRING      State$, BlockDateTime$, Reason$, ErrCode$
   STRING      Req$, Reply$
   INTEGER*1   RC

   WIC.EBT.STATE = 5    !@tal01112005
   
   !Format the request message
   Req$ =   "_40" +                    \ Message type
            State$ +                   \ State code
            BlockDateTime$ +           \ Block date/time as yyyymmddhhmmss
            Reason$                    ! Reason Code

   Reply$ = "_41"    !@tal09142004

   !Send the request and wait 30 seconds for a reply.
   CALL WICSendAndWait(Req$, 30, Reply$, RC)

   !Assume there is a problem.
   ErrCode$ = "FFFF"
   IF (RC = 0) THEN BEGIN
      !A response was received
      !It should begin with _41.  If not, return error code FFFF
      IF (LEFT$(Reply$, 3) = "_41") THEN BEGIN
         !Good reply. Parse the reply into returnable parts.
         ErrCode$    = MID$(Reply$, 4, 4)
         State$      = MID$(Reply$, 8, 2)
      ENDIF
   ENDIF ELSE IF (RC = 1) THEN BEGIN
      ErrCode$ = "100000"     !Timeout err   
   ENDIF ELSE IF (RC = 2) THEN BEGIN
      ErrCode$ = "200000"     !Cashier cancel
   ENDIF ELSE IF (RC = -5) THEN BEGIN
      ErrCode$ = "500000"     !Card in reader device
   ENDIF

   !Cleanup.
   Req$ = ""
   Reply$ = ""

   WIC.EBT.STATE = 0    !@tal01112005
   
END SUB

!*******************  End Transaction (_50) **************************
!Note: This command does not appear to be used by ACE.
!
!Inputs:
!- State code for which the card is valid (e.g. TX)
!- 2-digit completion code that may be used to display text at the ppad.
!
!Outputs:
!- 4-digit Error code
!- State code for which the card is valid (e.g. TX)
!
!Implementation notes:
!- Message to pinpad includes current date/time
!
!Description:
!- Format a message: _50SSyyyymmddhhmmsszz
!  SS is the state for which the card is valid (State$)
!  yyyymmddhhmmss is the current date/time
!  zz is a 2-digit completion code that may be used to display text at the ppad.
!- Send the message and wait for a response
!- If the response string is NULL or it does not begin with "_51", there was a
!  problem communicating with the pinpad.  Return ErrCode$= "FFFF" for the 
!  application to handle appropriately.
!- Parse the return string: _51EEEESS
!  EEEE is the error code (ErrCode$)
!  SS is the state for which the card is valid (State$)

SUB WICEndTransaction(State$, CompCode$, ErrCode$) PUBLIC
   STRING      State$, CompCode$, ErrCode$
   STRING      Req$, Reply$
   INTEGER*1   RC

   WIC.EBT.STATE = 6    !@tal01112005

   !Format the request message
   Req$ =   "_50" +                    \ Message type
            State$ +                   \ State code
            "20" + DATE$ +             \ Current date as yyyymmdd
            TIME$ +                    \ Current time as hhmmss
            CompCode$                  ! 2-digit completion code

   Reply$ = "_51"    !@tal09142004

   !Send the request and wait 30 seconds for a reply.
   CALL WICSendAndWait(Req$, 30, Reply$, RC)

   !Assume there is a problem.
   ErrCode$ = "FFFF"
   IF (RC = 0) THEN BEGIN
      !A response was received
      !It should begin with _51.  If not, return error code FFFF
      IF (LEFT$(Reply$, 3) = "_51") THEN BEGIN
         !Good reply. Parse the reply into returnable parts.
         ErrCode$    = MID$(Reply$, 4, 4)
         State$      = MID$(Reply$, 8, 2)
      ENDIF
   ENDIF ELSE IF (RC = 1) THEN BEGIN
      ErrCode$ = "100000"     !Timeout err   
   ENDIF ELSE IF (RC = 2) THEN BEGIN
      ErrCode$ = "200000"     !Cashier cancel
   ENDIF ELSE IF (RC = -5) THEN BEGIN
      ErrCode$ = "500000"     !Card in reader device
   ENDIF

   !Cleanup.
   Req$ = ""
   Reply$ = ""
   
   WIC.EBT.STATE = 0    !@tal01112005

END SUB

!*******************  Authenticate User (_60) **************************
!Inputs:
!- State code for which the card is valid (e.g. TX)
!- Number of PIN retries allowed
!
!Outputs:
!- 4-digit Error code
!- Authentication result code
!  "00" = PIN entry succseeful
!  "FF" = PIN entry unsuccessful but card not locked
!  "LL" = PIN locked during PIN entry
!  "PP" = PIN locked prior to current attempts
!
!Note: The caller must keep track of the return codes from each attempt in 
! order to differentiate between a card that locked during this set of PIN 
! entries and one that was already locked before the recent attempt was made.
!
!Description:
!- Format a message: _60SSaa
!  SS is the state for which the card is valid (State$)
!  aa is the number of PIN attempts allowed with a leading 0 (MaxRetries)
!- Send the message and wait for a response
!- If the response string is NULL or it does not begin with "_61", there was a
!  problem communicating with the pinpad.  Return ErrCode$= "FFFF" for the 
!  application to handle appropriately.
!- Parse the return string: _61EEEErr
!  EEEE is the error code (ErrCode$)
!  rr is the result of the authentication (AuthRC$)

SUB WICAuthenticate(State$, MaxRetries, ErrCode$, AuthRC$) PUBLIC
   STRING      State$, ErrCode$, AuthRC$
   INTEGER*1   MaxRetries
   STRING      Req$, Reply$
   INTEGER*1   RC

   WIC.EBT.STATE = 7    !@tal01112005

   !Format the request message
   Req$ =   "_60" +                                \ Message type
            State$ +                               \ State code
            RIGHT$("00" + STR$(MaxRetries), 2)     ! Max retries of PIN

   Reply$ = "_61"    !@tal09142004

   !Send the request and wait 3600 seconds for a reply.
   CALL WICSendAndWait(Req$, 3600, Reply$, RC)

   !Assume there is a problem.
   ErrCode$ = "FFFF"
   IF (RC = 0) THEN BEGIN
      !A response was received
      !It should begin with _61.  If not, return error code FFFF
      IF (LEFT$(Reply$, 3) = "_61") THEN BEGIN
         !Good reply. Parse the reply into returnable parts.
         ErrCode$    = MID$(Reply$, 4, 4)
         AuthRC$     = MID$(Reply$, 8, 2)
      ENDIF
   ENDIF ELSE IF (RC = 1) THEN BEGIN
      ErrCode$ = "100000"     !Timeout err   
   ENDIF ELSE IF (RC = 2) THEN BEGIN
      ErrCode$ = "200000"     !Cashier cancel
   ENDIF ELSE IF (RC = -5) THEN BEGIN
      ErrCode$ = "500000"     !Card in reader device
   ENDIF

   !Cleanup.
   Req$ = ""
   Reply$ = ""

   WIC.EBT.STATE = 0    !@tal01112005

END SUB

!*******************  Deactivate (_70) **************************
!Note: This command does not appear to be used by ACE.
!
!Inputs:
!- None
!
!Outputs:
!- 4-digit Error code
!
!Description:
!- Format a message: _70
!- Send the message and wait for a response
!- If the response string is NULL or it does not begin with "_71", there was a
!  problem communicating with the pinpad.  Return ErrCode$= "FFFF" for the 
!  application to handle appropriately.
!- Parse the return string: _71EEEE
!  EEEE is the error code (ErrCode$)

SUB WICDeactivate(ErrCode$) PUBLIC
   STRING      ErrCode$
   STRING      Req$, Reply$
   INTEGER*1   RC

   WIC.EBT.STATE = 8    !@tal01112005

   !Format the request message
   Req$ =   "_70"                                  ! Message type

   Reply$ = "_71"    !@tal09142004
               
   !Send the request and wait 30 seconds for a reply.
   CALL WICSendAndWait(Req$, 30, Reply$, RC)

   !Assume there is a problem.
   ErrCode$ = "FFFF"
   IF (RC = 0) THEN BEGIN
      !A response was received
      !It should begin with _71.  If not, return error code FFFF
      IF (LEFT$(Reply$, 3) = "_71") THEN BEGIN
         !Good reply. Parse the reply into returnable parts.
         ErrCode$    = MID$(Reply$, 4, 4)
      ENDIF
   ENDIF ELSE IF (RC = 1) THEN BEGIN
      ErrCode$ = "100000"     !Timeout err   
   ENDIF ELSE IF (RC = 2) THEN BEGIN
      ErrCode$ = "200000"     !Cashier cancel
   ENDIF ELSE IF (RC = -5) THEN BEGIN
      ErrCode$ = "500000"     !Card in reader device
   ENDIF

   !Cleanup.
   Req$ = ""
   Reply$ = ""

   WIC.EBT.STATE = 0    !@tal01112005

END SUB

!*******************  Remove Card (_80) **************************
!Inputs:
!- None
!
!Outputs:
!- 4-digit Error code
!
!Description:
!- Format a message: _80
!- Send the message and wait for a response
!- It doesn't matter what the response is.

SUB WICRemoveCard(ErrCode$) PUBLIC
   STRING      ErrCode$
   STRING      Req$, Reply$
   INTEGER*1   RC

   WIC.EBT.STATE = 9    !@tal01112005

   !Format the request message
   Req$ =   "_80"                                  ! Message type

   Reply$ = "_81"    !@tal09142004

   !Send the request and wait 30 seconds for a reply.
   CALL WICSendAndWait(Req$, 30, Reply$, RC)
   IF (RC = 0) THEN BEGIN
      ErrCode$ = "0000"
      PP.80.REMOVE.CARD.SENT = -1                  ! flag remove card msg sent
   ENDIF ELSE IF (RC = 1) THEN BEGIN
      ErrCode$ = "100000"     !Timeout err   
   ENDIF ELSE IF (RC = 2) THEN BEGIN
      ErrCode$ = "200000"     !Cashier cancel
   ENDIF ELSE BEGIN
      ErrCode$ = "FFFF"
   ENDIF
   
   !Cleanup.
   Req$ = ""
   Reply$ = ""

   WIC.EBT.STATE = 0          !@tal01112005

END SUB

