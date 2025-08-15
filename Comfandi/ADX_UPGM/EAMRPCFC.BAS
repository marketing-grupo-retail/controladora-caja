\/* TIME STAMP BLOCK ************************************************
\** END OF TIME STAMP BLOCK ****************************************/
\/******************************************************************/
\/*                                                                */
\/*      MODULE NAME: EAMRPCFC                                     */
\/*                                                                */
\/*      DESCRIPTIVE NAME: REPORT COMMON FUNCTIONS/SUBROUTINES     */
\/*                                                                */
\/*                                                                */
\/*      COPYRIGHT:                                                */
\/*      5696-536 THIS MODULE IS "RESTRICTED MATERIALS OF IBM"     */
\/*      (C) COPYRIGHT IBM CORP 1986, 1993 ALL RIGHTS RESERVED     */
\/*      LICENSED MATERIALS - PROPERTY OF IBM REFER TO COPYRIGHT   */
\/*      INSTRUCTIONS FORM NUMBER G120-2083                        */
\/*                                                                */
\/*      STATUS: RELEASE 2 LEVEL 0                                 */
\/*                                                                */
\/*                                                                */
\/*      PERSON RESPONSIBLE = VAUGHAN, D. ROBIN                    */
\/*                                                                */
\/*      FUNCTION: EAMRPCFC                                        */
\/*                                                                */
\/*                ROUTINES USED BY REPORT PROCEDURES              */
\/*                                                                */
\/*      NOTES:                                                    */
\/*                                                                */
\/*         DEPENDENCIES: NONE                                     */
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
\/*         MODULE SIZE: XXXX...                                   */
\/*                Estimated size in bytes.                        */
\/*                                                                */
\/*         ATTRIBUTES: REENTRANT                                  */
\/*                                                                */
\/*      ENTRY POINT: RPCFEC01                                     */
\/*                                                                */
\/*         PURPOSE: OUTPUT REPORT LINE ENTRY POINT                */
\/*                                                                */
\/*                ***** RETURN CONTROL *****                      */
\/*                                                                */
\/*         LINKAGE: CALLED FROM MODULES PRINTING/DISPLAYING       */
\/*                  REPORT DATA.                                  */
\/*                                                                */
\/*      INPUT:                                                    */
\/*                                                                */
\/*                                                                */
\/*      OUTPUT:                                                   */
\/*                                                                */
\/*         SYMBOLIC NAME: SCREEN                                  */
\/*                                                                */
\/*         DESCRIPTION: OUTPUT VIA DISPLAY MANAGER                */
\/*                                                                */
\/*                REPORT LINE                                     */
\/*                                                                */
\/*                ***** PRIME OUTPUT *****                        */
\/*                                                                */
\/*         SYMBOLIC NAME: PRINTER                                 */
\/*                                                                */
\/*         DESCRIPTION: OUTPUT TO PRINTER                         */
\/*                                                                */
\/*                REPORT LINE                                     */
\/*                                                                */
\/*                ***** PRIME OUTPUT *****                        */
\/*                                                                */
\/*         SYMBOLIC NAME: EAMRPT01                                */
\/*                                                                */
\/*         DESCRIPTION: SAVED REPORTS FILE                        */
\/*                                                                */
\/*                REPORT LINE                                     */
\/*                                                                */
\/*                ***** FILE OUTPUT *****                         */
\/*                                                                */
\/*         SYMBOLIC NAME: EAMRPT02                                */
\/*                                                                */
\/*         DESCRIPTION: SAVED REPORTS FILE                        */
\/*                                                                */
\/*                ***** FILE OUTPUT *****                         */
\/*                                                                */
\/*         SYMBOLIC NAME: EAMRPT03                                */
\/*                                                                */
\/*         DESCRIPTION: SAVED REPORTS FILE                        */
\/*                                                                */
\/*                ***** FILE OUTPUT *****                         */
\/*                                                                */
\/*         SYMBOLIC NAME: EAMRPT04                                */
\/*                                                                */
\/*         DESCRIPTION: SAVED REPORTS FILE                        */
\/*                                                                */
\/*                ***** FILE OUTPUT *****                         */
\/*                                                                */
\/*         SYMBOLIC NAME: EAMRPT05                                */
\/*                                                                */
\/*         DESCRIPTION: SAVED REPORTS FILE                        */
\/*                                                                */
\/*                ***** FILE OUTPUT *****                         */
\/*                                                                */
\/*         SYMBOLIC NAME: EAMRPT06                                */
\/*                                                                */
\/*         DESCRIPTION: SAVED REPORTS FILE                        */
\/*                                                                */
\/*                ***** FILE OUTPUT *****                         */
\/*                                                                */
\/*         SYMBOLIC NAME: EAMRPT07                                */
\/*                                                                */
\/*         DESCRIPTION: SAVED REPORTS FILE                        */
\/*                                                                */
\/*                ***** FILE OUTPUT *****                         */
\/*                                                                */
\/*         SYMBOLIC NAME: EAMRPT08                                */
\/*                                                                */
\/*         DESCRIPTION: SAVED REPORTS FILE                        */
\/*                                                                */
\/*                ***** FILE OUTPUT *****                         */
\/*                                                                */
\/*         SYMBOLIC NAME: EAMRPT09                                */
\/*                                                                */
\/*         DESCRIPTION: SAVED REPORTS FILE                        */
\/*                                                                */
\/*                ***** FILE OUTPUT *****                         */
\/*                                                                */
\/*         SYMBOLIC NAME: EAMRPT10                                */
\/*                                                                */
\/*         DESCRIPTION: SAVED REPORTS FILE                        */
\/*                                                                */
\/*                ***** FILE OUTPUT *****                         */
\/*                                                                */
\/*         SYMBOLIC NAME: EAMRPT11                                */
\/*                                                                */
\/*         DESCRIPTION: SAVED REPORTS FILE                        */
\/*                                                                */
\/*                ***** FILE OUTPUT *****                         */
\/*                                                                */
\/*                                                                */
\/*      ENTRY POINT: RPCFEC02                                     */
\/*                                                                */
\/*         PURPOSE: RIGHT JUSTIFY STRING TO SPECIFIED LENGTH      */
\/*                                                                */
\/*                ***** RETURN CONTROL *****                      */
\/*                                                                */
\/*         LINKAGE: CALLED FROM REPORT MODULES                    */
\/*                                                                */
\/*      INPUT:                                                    */
\/*                                                                */
\/*      OUTPUT:                                                   */
\/*                                                                */
\/*      ENTRY POINT: RPCFEC03                                     */
\/*                                                                */
\/*         PURPOSE: LEFT JUSTIFY STRING TO SPECIFIED LENGTH       */
\/*                                                                */
\/*                ***** RETURN CONTROL *****                      */
\/*                                                                */
\/*         LINKAGE: CALLED FROM REPORT MODULES                    */
\/*                                                                */
\/*      INPUT:                                                    */
\/*                                                                */
\/*      OUTPUT:                                                   */
\/*                                                                */
\/*      ENTRY POINT: RPCFEC04                                     */
\/*                                                                */
\/*         PURPOSE: CONCATONATES STRINGS TO SPECIFIED LENGTH      */
\/*                                                                */
\/*                ***** RETURN CONTROL *****                      */
\/*                                                                */
\/*         LINKAGE: CALLED FROM REPORT MODULES                    */
\/*                                                                */
\/*      INPUT:                                                    */
\/*                                                                */
\/*      OUTPUT:                                                   */
\/*                                                                */
\/*                                                                */
\/*      ENTRY POINT: RPCFEC05                                     */
\/*                                                                */
\/*         PURPOSE: CONVERTS KEY DATA TO OPERATOR/TERMINAL ID     */
\/*                                                                */
\/*                ***** RETURN CONTROL *****                      */
\/*                                                                */
\/*         LINKAGE: CALLED FROM REPORT MODULES                    */
\/*                                                                */
\/*      INPUT:                                                    */
\/*                                                                */
\/*      OUTPUT:                                                   */
\/*                                                                */
\/*                                                                */
\/*      ENTRY POINT: RPCFEC10                                     */
\/*                                                                */
\/*         PURPOSE: ROUTINE TO OUTPUT FIRST FOUR LINES OF         */
\/*                  REPORT HEADER TO DESTINATION                  */
\/*                                                                */
\/*                ***** RETURN CONTROL *****                      */
\/*                                                                */
\/*         LINKAGE: CALLED FROM REPORT MODULES                    */
\/*                                                                */
\/*      INPUT:                                                    */
\/*                                                                */
\/*         SYMBOLIC NAME: EAMRDESC                                */
\/*                                                                */
\/*         DESCRIPTION: REPORT DESCRIPTORS FILE.                  */
\/*                                                                */
\/*                ***** FILE INPUT *****                          */
\/*                                                                */
\/*                                                                */
\/*      OUTPUT:                                                   */
\/*                                                                */
\/*      EXIT-NORMAL: RETURN TO CALLER                             */
\/*                                                                */
\/*         RETURN CODE:                                           */
\/*                                                                */
\/*      EXIT-ERROR: CALLING MODULES ERROR ROUTINE                 */
\/*                                                                */
\/*         RETURN CODE: RP11WERR                                  */
\/*                                                                */
\/*            REASON CODE:                                        */
\/*            MESSAGE ID:                                         */
\/*                                                                */
\/*         ABEND CODES:                                           */
\/*                                                                */
\/*         ERROR MESSAGES:                                        */
\/*                                                                */
\/*      EXTERNAL REFERENCES:                                      */
\/*                                                                */
\/*         ROUTINES:                                              */
\/*                                                                */
\/*           NAME: MMCFEC05 - CHECKS DISPLAY MANAGER RETURN CODES */
\/*           NAME: MMCFEC08 - STRIPS LEADING AND TRAILING BLANKS  */
\/*           NAME: MMCFEC11 - GETS SYSTEM DATE AND TIME           */
\/*           NAME: MMCFEC14 - FORMATS DATE AND TIME               */
\/*                                                                */
\/*         DATA AREAS:                                            */
\/*                                                                */
\/*           MODIFIED:                                            */
\/*                                                                */
\/*             PRINTER : OUTPUT TO PRINTER FROM RPCFEC01          */
\/*             SCREEN  : OUTPUT VIA DISPLAY MANAGER               */
\/*             EAMRPT01: REPORT FILE                              */
\/*             EAMRPT02: REPORT FILE                              */
\/*             EAMRPT03: REPORT FILE                              */
\/*             EAMRPT04: REPORT FILE                              */
\/*             EAMRPT05: REPORT FILE                              */
\/*             EAMRPT06: REPORT FILE                              */
\/*             EAMRPT07: REPORT FILE                              */
\/*             EAMRPT08: REPORT FILE                              */
\/*             EAMRPT09: REPORT FILE                              */
\/*             EAMRPT10: REPORT FILE                              */
\/*             EAMRPT11: REPORT FILE                              */
\/*                                                                */
\/*           REFERENCED:                                          */
\/*                                                                */
\/*             EAMRDESC: REPORT DESCRIPTORS FILE                  */
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
\/* END-OF-SPECIFICATIONS ******************************************/

\ REM!!***************** INCLUDE FILE *****************************!!
\ REM!! %INCLUDE EAMCFINC.J86   global variables                   !!
\ REM!! %INCLUDE EAMATTRD.J86   video attributes                   !!
\ REM!! %INCLUDE EAMRPINC.J86   global variables (rpt com funcs)   !!
\ REM!! %INCLUDE EAMUSED.J86    Chain parameters - pass to ....    !!
\ REM!! %INCLUDE DMEXTR.J86     Display manager functions          !!
\ REM!! %INCLUDE EAMXXCPY.J86   include the copyright statement    !!
\ REM!! %INCLUDE EAMRPU01.J86   include the reports user exit      !!
\ REM!! %INCLUDE EAMRPUVA.J86   include the reports user exit ! IR68987
\ REM!! %INCLUDE EAMRPUSU.J86   include the reports user exit ! IR68987
\ REM!!************************************************************!!

\ REM!!***************** RPCFEC01 *********************************!!
\ REM!! This subroutine outputs a passed string to the specified
\ REM!! destination.
\ REM!!
\ REM!! SUB RPCFEC01(LINE$)    PUBLIC       (WRITE.LINE)
\ REM!!    IF destination is screen THEN
\ REM!!      position cursor at next field
\ REM!!      display print.string at field
\ REM!!    ELSE
\ REM!!      build format string for write to session number
\ REM!!      write print.string to session number
\ REM!!    ENDIF
\ REM!!    bump line number
\ REM!! END SUB
\ REM!!************************************************************!!

\ REM!!******************* RPCFEC02 *******************************!!
\ REM!! This function pads out a string to a fixed length (the
\ REM!! string will be "right justified").
\ REM!!************************************************************!!

\ REM!!****************** RPCFEC03 ********************************!!
\ REM!! This function pads out a string to a fixed length (the
\ REM!! string will be "left justified").
\ REM!!************************************************************!!

\ REM!!****************** RPCFEC04 ********************************!!
\ REM!! This function builds a string of fixed length from the two
\ REM!! strings passed. "string1.....string2".
\ REM!! If the strings are too long the right one will overlay the
\ REM!! left.
\ REM!!************************************************************!!

\ REM!!*************** RPCFEC05 ***********************************!!
\ REM!! This function formats an account key to return a string
\ REM!! containing the operator/terminal number with fixed length
\ REM!! of 10 characters. If a leading asterisk is needed this
\ REM!! will be inserted also.
\ REM!!************************************************************!!

\ REM!!****************** RPCFEC10 ********************************!!
\ REM!! This subroutine builds and outputs the first four lines
\ REM!! of the header to the selected destination.
\ REM!!
\ REM!!  SUB RPCFEC10
\ REM!!
\ REM!!     IF first time called for this report THEN
\ REM!!        CALL SET.UP.DESC to get header descriptors
\ REM!!        build first line using FIRST.LINE$
\ REM!!        build second line using SECOND.LINE$
\ REM!!        set the page number as would be needed
\ REM!!     ENDIF
\ REM!!
\ REM!!     IF not to screen THEN
\ REM!!        use RPCFEC01 to send eject char to session no.
\ REM!!        bump page number
\ REM!!     ENDIF
\ REM!!
\ REM!!     Set up page number details using PAGE.STR$
\ REM!!
\ REM!!     reset line number/field number
\ REM!!     insert page details in first line
\ REM!!     CALL RPCFEC01 to output first header line
\ REM!!     CALL RPCFEC01 to skip a line
\ REM!!     CALL RPCFEC01 to output second header line
\ REM!!     CALL RPCFEC01 to skip a line
\ REM!!
\ REM!!  END SUB
\ REM!!************************************************************!!
\
\ REM!!***************** SET UP DESC ******************************!!
\ REM!! SUB SET.UP.DESC
\ REM!!
\ REM!!   This subroutine sets up the descriptors needed for the
\ REM!!   report header lines. It also initialises some of the
\ REM!!   variables used in the header routine.
\ REM!! END SUB
\ REM!!************************************************************!!
\
\ REM!!**************** FIRST LINE$ *******************************!!
\ REM!! This function builds the first line of the header.
\ REM!!
\ REM!! FUNCTION FIRST.LINE$
\ REM!!   add the store string to the title
\ REM!!   try and centre the title allowing for the page string
\ REM!!
\ REM!!   build the first line from the title, store details,
\ REM!!             and the page details
\ REM!!
\ REM!! END FUNCTION
\ REM!!************************************************************!!
\
\ REM!!*************** SECOND LINE$ *******************************!!
\ REM!! This function builds the second line of the header.
\ REM!!
\ REM!! FUNCTION SECOND.LINE$
\ REM!!    build the reported at date and time string
\ REM!!    IF no period details to be shown THEN
\ REM!!       set period details to null
\ REM!!    ELSE
\ REM!!       build period details from desc and period datetime
\ REM!!
\ REM!!    concatonate the two string to form the second line
\ REM!! END FUNCTION
\ REM!!************************************************************!!
\
\ REM!!***************** PAGE.STR$ ********************************!!
\ REM!! This function builds the "Page xxx of xxx" string.
\ REM!!
\ REM!! FUNCTION PAGE.STR$
\ REM!!
\ REM!!    IF maximum number of pages is 1 THEN
\ REM!!       set returned string to a null
\ REM!!    ELSE
\ REM!!       IF there is no maximum page THEN
\ REM!!          build returned string from the page number only
\ REM!!       ELSE
\ REM!!          build returned string as page xxx of xxx
\ REM!!
\ REM!! END FUNCTION
\ REM!!************************************************************!!
\
\ REM!!************************************************************!!
\ REM!!*** END OF BBDL ************************ START OF CODE *****!!
\ REM!!************************************************************!!

      %INCLUDE EAMCFINC.J86   ! global variables
      %INCLUDE EAMATTRD.J86   ! video attributes
      %INCLUDE EAMRPINC.J86   ! report common function variables
      %INCLUDE EAMUSED.J86    !

%INCLUDE EAMRPUVA.J86                ! REPORTS USER EXIT       IR68987

  STRING    GLOBAL     \ destination of the previous report    IR73607
      HEAD.STR1$,      \ header 1
      HEAD.STR2$       ! header 2

  INTEGER*2 GLOBAL     \ destination of the previous report    IR73607
      LAST.DESTIN      !                                       IR73607

  INTEGER*2            \
                       \
      LEN.PAGE.STR,    \
      FEC10.READ,      \ descriptors read flag
      TITLE.DESC.NO,   \ title descriptor number
      WORK,            \ work integers
      WORK2            !

  STRING               \
      WORK$,           \ work strings
      WORK2$,          \
                       \
      FMT$,            \string to hold write form                        (ENH01)
                       \
      PAGE$,           \ descriptors
      OF$,             \
      STORE$,          \
      PERIOD$(1),      \
      REPORTED.AT$,    \
      SKIP$            ! carriage return + line feed

\ REM!!************************************************************!!
\ REM!!                  External Routines
\ REM!!************************************************************!!

  %INCLUDE EAMXXCPY.J86  ! include the copyright statement

  %INCLUDE DMEXTR.J86    ! Display Manager

  SUB MMCFEC05(RET.VAL,DM.CMD)  EXTERNAL  ! checks display manager return
      STRING DM.CMD                       ! codes
      INTEGER*2 RET.VAL                   !
  END SUB

  FUNCTION MMCFEC08$(BLANK$)    EXTERNAL  ! strips leading and trailing
      STRING MMCFEC08$, BLANK$            ! blanks from a passed string
  END FUNCTION                            !

  FUNCTION MMCFEC11$            EXTERNAL  ! returns a formatted system
      STRING MMCFEC11$                    ! date and time
  END FUNCTION                            !

  FUNCTION MMCFEC14$(DATETIME$) EXTERNAL  ! formats the passed date and time
      STRING MMCFEC14$,DATETIME$          !
  END FUNCTION                            !

%INCLUDE EAMRPUSU.J86                ! REPORTS USER EXIT       IR68987

\ REM!!***************** RPCFEC01 *********************************!!
\ REM!! This subroutine outputs a passed string to the specified
\ REM!! destination.
\ REM!!
\ REM!! SUB RPCFEC01(LINE$)    PUBLIC       (WRITE.LINE)
\ REM!!    IF destination is screen THEN
\ REM!!      position cursor at next field
\ REM!!      display print.string at field
\ REM!!    ELSE
\ REM!!      build format string for write to session number
\ REM!!      write print.string to session number
\ REM!!    ENDIF
\ REM!!    bump line number
\ REM!! END SUB
\ REM!!************************************************************!!

  SUB RPCFEC01(LINE$)   PUBLIC       ! (WRITE.LINE)

  STRING  LINE$,FORMAT$

%INCLUDE EAMRPU01.J86                ! REPORTS USER EXIT       RM060386

     IF DESTINATION = DISPLAY THEN          \ if to screen
        CALL MMCFEC05(POSF(LINE.NO),"CF3")   :\ position cursor
        CALL MMCFEC05(PUTF(LINE$),"CF7")      \ display string
     ELSE                                   \ if to file/printer
        FORMAT$ = "C" + STR$(LEN(LINE$)) + ",C2":\
        WRITE FORM FORMAT$;#1;LINE$,SKIP$   ! write string
\    ENDIF                                  !
     LINE.NO = LINE.NO + 1                  ! bump line number

  END SUB

\ REM!!******************* RPCFEC02 *******************************!!
\ REM!! This function pads out a string to a fixed length (the
\ REM!! string will be "right justified").
\ REM!!************************************************************!!

  FUNCTION RPCFEC02$(IN.STR$,LENGTH)  PUBLIC   ! (R.JUST$)

  STRING RPCFEC02$,IN.STR$
  INTEGER LENGTH

       RPCFEC02$ = STRING$(LENGTH-LEN(IN.STR$)," ") + IN.STR$

  END FUNCTION

\ REM!!****************** RPCFEC03 ********************************!!
\ REM!! This function pads out a string to a fixed length (the
\ REM!! string will be "left justified").
\ REM!!************************************************************!!

  FUNCTION RPCFEC03$(IN.STR$,LENGTH) PUBLIC  ! (L.JUST$)

  STRING RPCFEC03$,IN.STR$
  INTEGER LENGTH

       RPCFEC03$ = IN.STR$ + STRING$(LENGTH-LEN(IN.STR$)," ")

  END FUNCTION

\ REM!!****************** RPCFEC04 ********************************!!
\ REM!! This function builds a string of fixed length from the two
\ REM!! strings passed. "string1.....string2".
\ REM!! If the strings are too long the right one will overlay the
\ REM!! left.
\ REM!!************************************************************!!

  FUNCTION RPCFEC04$(L.STR$,R.STR$,LENGTH) PUBLIC  ! (CONCAT$)

  STRING RPCFEC04$,L.STR$,R.STR$
  INTEGER LENGTH

       WORK  = LEN(L.STR$)               !
       WORK2 = LEN(R.STR$)               !
       IF (WORK+WORK2 <= LENGTH) THEN    \
          RPCFEC04$ = L.STR$ + STRING$(LENGTH-(WORK+WORK2)," ")\
                      + R.STR$           \
       ELSE                              \
          RPCFEC04$ = LEFT$(L.STR$,LENGTH-WORK2) + R.STR$
\      ENDIF                             !

  END FUNCTION

\ REM!!*************** RPCFEC05 ***********************************!!
\ REM!! This function formats an account key to return a string
\ REM!! containing the operator/terminal number with fixed length
\ REM!! of 10 characters. If a leading asterisk is needed this
\ REM!! will be inserted also.
\ REM!!************************************************************!!

  FUNCTION RPCFEC05$(ID.STR$) PUBLIC   ! (ID$)

  STRING RPCFEC05$,ID.STR$

      WORK$ = STR$(VAL(UNPACK$(RIGHT$(ID.STR$,5))))
      OP.OR.TERM = VAL(LEFT$(ID.STR$,1)) - 1
      IF (OP.OR.TERM = 2 AND ASTERISK.NEEDED) THEN\
         WORK$ = "*" + WORK$
\     ENDIF
      RPCFEC05$ = WORK$

  END FUNCTION

\ REM!!************************************************************!!
\ REM!!****************** RPCFEC10 ********************************!!
\ REM!!************************************************************!!

\ REM!!***************** SET UP DESC ******************************!!
\ REM!! This subroutine sets up the descriptors needed for the
\ REM!! report header lines. It also initialises some of the
\ REM!! variables used in the header routine.
\ REM!!************************************************************!!

  SUB SET.UP.DESC

  STRING  STORE.NO$

     IF NOT FEC10.READ THEN            \ if need descriptors
        DIM PERIOD$(2)                :\
        READ #6,1;PERIOD$(1)          :\
        READ #6,2;PERIOD$(2)          :\
        READ #6,3;REPORTED.AT$        :\
        READ #6,4;PAGE$               :\
        PAGE$ = MMCFEC08$(PAGE$)      :\
        READ #6,5;OF$                 :\
        OF$ = " " + MMCFEC08$(OF$)    :\
        READ #6,6;STORE$              :\
        LEN.PAGE.STR = 8 + LEN(PAGE$) + LEN(OF$):\
        STORE.NO$ = STR$(VAL(LEFT$(MM.STATUS$,4))):\
        STORE$ = STORE$ + STORE.NO$   :\
        SKIP$ = CHR$(13) + CHR$(10)   :\
        FEC10.READ = ON1
\    ENDIF
     IF TITLE.DESC.NO <> 0 THEN     \
        READ #6,TITLE.DESC.NO;RP.TITLE$

  END SUB

\ REM!!**************** FIRST LINE$ *******************************!!
\ REM!! This function builds the first line of the header.
\ REM!!
\ REM!! FUNCTION FIRST.LINE$
\ REM!!   add the store string to the title
\ REM!!   try and centre the title allowing for the page string
\ REM!!
\ REM!!   build the first line from the title, store details,
\ REM!!             and the page details
\ REM!!
\ REM!! END FUNCTION
\ REM!!************************************************************!!

  FUNCTION FIRST.LINE$

  STRING FIRST.LINE$
  !AIR89063
  STRING SHORT$,   \
         LONG$
  !EIR89063

!AIR89063
     WORK$ = MMCFEC08$(RP.TITLE$) + STORE$
!AIR94226 Delete IR92025,IR93639,IR93710and correct IR89063
!    IF (TITLE.DESC.NO <> 600) AND           \ IR92025 If not Dept Anal.
!       (TITLE.DESC.NO <> 601) AND           \ IR92025 Dept. Anal.
!       (TITLE.DESC.NO <> 430) AND           \ IR93639
!       (TITLE.DESC.NO <> 410) AND           \ IR93710 Item movement
!       (TITLE.DESC.NO <> 421) AND           \ IR93710 Item movement
!       (TITLE.DESC.NO <> 422) AND           \ IR93710 Item movement
!       (TITLE.DESC.NO <> 423) AND           \ IR93710 Item movement
!       (PERIOD < 5) THEN BEGIN              ! IR93710 for user reports
!       IF TITLE.DESC.NO <> 9 THEN BEGIN
!EIR94226
     IF ((TITLE.DESC.NO = 0) OR      \ IR94226 IR95955 If long totals accumulate
        (TITLE.DESC.NO =500)) AND    \ IR94226 IR95955 for this report, and
        (PERIOD < 5) THEN BEGIN      ! IR94226 not user reports
           READ #6,91;SHORT$
           READ #6,92;LONG$
           IF PERIOD > 2 THEN BEGIN
              WORK$ = MMCFEC08$(LONG$ + " " + RP.TITLE$) + STORE$
           ENDIF ELSE BEGIN
              WORK$ = MMCFEC08$(SHORT$ + " " + RP.TITLE$) + STORE$
           ENDIF
!       ENDIF! IR94226
     ENDIF                                   ! IR92025 IR94226
!EIR89063

     WORK = (80 - (2*LEN.PAGE.STR) - LEN(WORK$))

     IF WORK < 0 THEN                       \
        WORK = 11                           \
     ELSE                                   \
        WORK = (WORK/2) + LEN.PAGE.STR - 2  !

     IF DESTINATION = DISPLAY THEN          \
        WORK = WORK - 10                    !

     FIRST.LINE$ = STRING$(WORK," ") + WORK$

  FEND

\ REM!!*************** SECOND LINE$ *******************************!!
\ REM!! This function builds the second line of the header.
\ REM!!
\ REM!! FUNCTION SECOND.LINE$
\ REM!!    build the reported at date and time string
\ REM!!    IF no period details to be shown THEN
\ REM!!       set period details to null
\ REM!!    ELSE
\ REM!!       build period details from desc and period datetime
\ REM!!
\ REM!!    concatonate the two string to form the second line
\ REM!! END FUNCTION
\ REM!!************************************************************!!

  FUNCTION SECOND.LINE$

  STRING SECOND.LINE$

   WORK$ = REPORTED.AT$ + MMCFEC11$
!AIR94226 Delete IR93639
!  IF (TITLE.DESC.NO <> 430) THEN BEGIN          ! IR93639 not sys. stat.
!EIR94226
     IF PERIOD > 2 THEN BEGIN  ! if no period details
!AIR89063
        WORK2$ = NULL$         !
!AIR94226
!       IF TITLE.DESC.NO <> 9 THEN BEGIN
        IF ((TITLE.DESC.NO = 0) OR       \ IR95955 If long totals accumulated
           (TITLE.DESC.NO =500) OR      \ for this report,
           (TITLE.DESC.NO = 600) OR     \ need to include Dept Analysis
           (TITLE.DESC.NO = 601)) AND    \ IR95955 Report for printing 2nd line
           (PERIOD < 5) THEN BEGIN      \ not for user reports
\EIR94226
         WORK2$ = PERIOD$(PERIOD-2) + MMCFEC14$(UNPACK$(RP.DATETIME$))
        ENDIF
!EIR89063
     ENDIF ELSE BEGIN          !
        WORK2$ = PERIOD$(PERIOD) + MMCFEC14$(UNPACK$(RP.DATETIME$))
     ENDIF
!AIR94226  Delete IR93639
!  ENDIF ELSE BEGIN                             ! IR93639
!    WORK2$ = NULL$                             ! IR93639
!    WORK$  = NULL$                             ! IR93639
!  ENDIF                                        ! IR93639
!EIR94226

   SECOND.LINE$ = RPCFEC04$(WORK2$,WORK$,78)

  END FUNCTION

\ REM!!***************** PAGE.STR$ ********************************!!
\ REM!! This function builds the "Page xxx of xxx" string.
\ REM!!
\ REM!! FUNCTION PAGE.STR$
\ REM!!
\ REM!!    IF maximum number of pages is 1 THEN
\ REM!!       set returned string to a null
\ REM!!    ELSE
\ REM!!       IF there is no maximum page THEN
\ REM!!          build returned string from the page number only
\ REM!!       ELSE
\ REM!!          build returned string as page xxx of xxx
\ REM!!
\ REM!! END FUNCTION
\ REM!!************************************************************!!

  FUNCTION PAGE.STR$

  STRING PAGE.STR$

     WORK$ = PAGE$ + RPCFEC02$(STR$(PAGE),3)
     IF MAX.PAGE = 1 THEN        \
        PAGE.STR$ = NULL$        \
     ELSE                        \
        IF NO.MAX.PAGE THEN      \
           PAGE.STR$ = WORK$     \
        ELSE                     \
           PAGE.STR$ = WORK$ + OF$ +\
                           RPCFEC02$(STR$(MAX.PAGE),3)
\       ENDIF
\    ENDIF

  END FUNCTION

\ REM!!****************** RPCFEC10 ********************************!!
\ REM!! This subroutine builds and outputs the first four lines
\ REM!! of the header to the selected destination.
\ REM!!
\ REM!!  SUB RPCFEC10
\ REM!!
\ REM!!     IF first time called for this report THEN
\ REM!!        CALL SET.UP.DESC to get header descriptors
\ REM!!        build first line using FIRST.LINE$
\ REM!!        build second line using SECOND.LINE$
\ REM!!        set the page number as would be needed
\ REM!!     ENDIF
\ REM!!************************************************************!!

  SUB RPCFEC10(TITLE.NO)   PUBLIC

  INTEGER*2  TITLE.NO     !

        IF LAST.DESTIN <> DESTINATION THEN BEGIN
           LAST.DESTIN = DESTINATION
           FIRST.HEADER = ON1
        ENDIF

        IF FIRST.HEADER THEN           \ if first header
           TITLE.DESC.NO = TITLE.NO   :\
           CALL SET.UP.DESC           :\ get descriptors
           HEAD.STR1$ = FIRST.LINE$   :\
           HEAD.STR2$ = SECOND.LINE$  :\
           FIRST.HEADER = OFF0        :\
           IF DESTINATION = DISPLAY THEN\ if to screen
              PAGE = 1                 \
           ELSE                        \
              PAGE = 0                 \
\          ENDIF
\       ENDIF

\ REM!!************************************************************!!
\ REM!!     IF not to screen THEN
\ REM!!        use RPCFEC01 to send eject char to session no.
\ REM!!        IF COMPRESSED.PRINT variable in on also                   (ENH01)
\ REM!!           set compress print mode.                               (ENH01)
\ REM!!                           Note: User must turn off compress      (ENH01)
\ REM!!                                 print before closing file.       (ENH01)
\ REM!!        bump page number
\ REM!!     ENDIF
\ REM!!
\ REM!!     Set up page number details using PAGE.STR$
\ REM!!
\ REM!!     reset line number/field number
\ REM!!     insert page details in first line
\ REM!!     CALL RPCFEC01 to output first header line
\ REM!!     CALL RPCFEC01 to skip a line
\ REM!!     IF more than the first lines needed THEN
\ REM!!        CALL RPCFEC01 to output second header line
\ REM!!        CALL RPCFEC01 to skip a line
\ REM!!     ENDIF
\ REM!!
\ REM!!  END SUB
\ REM!!************************************************************!!

        IF DESTINATION <> DISPLAY AND           \ if not to screen       (ENH01)
          NOT COMPRESSED.PRINT THEN BEGIN       ! and no compressed print(ENH01)
           WRITE FORM "C1";#1;CHR$(12)          ! write eject char
           PAGE = PAGE + 1                      ! bump page number
        ENDIF                                                           !(ENH01)
        IF DESTINATION <> DISPLAY AND           \ if not to screen       (ENH01)
          COMPRESSED.PRINT THEN BEGIN           ! and compressed print   (ENH01)
            FMT$ = "C"+ STR$(LEN(COMPRESSED.PRINT.ON$) + 1)             !(ENH01)
            WRITE FORM FMT$;#1;CHR$(12),        \ write eject &          (ENH01)
             COMPRESSED.PRINT.ON$               !  compress print string (ENH01)
           PAGE = PAGE + 1                      ! bump page number       (ENH01)
        ENDIF                                                           !(ENH01)

        WORK = 78
        IF DESTINATION = DISPLAY THEN WORK = 68

        WORK2$ = RPCFEC04$(HEAD.STR1$,PAGE.STR$,WORK)
                                       ! add page details

        LINE.NO = 1                    ! reset position
        CALL RPCFEC01(WORK2$)          ! first line of header
        CALL RPCFEC01(NULL$)           ! skip a line
!AIR89063
        IF PERIOD < 5 THEN BEGIN       ! IR94226
!EIR89063
           CALL RPCFEC01(HEAD.STR2$)   ! second line
           CALL RPCFEC01(NULL$)        ! skip a line
        ENDIF

   END SUB

\ REM!!************************************************************!!
\ REM!!************ End of Report Common Functions ****************!!
\ REM!!************************************************************!!
