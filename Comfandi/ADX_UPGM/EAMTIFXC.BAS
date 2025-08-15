\/* TIME STAMP BLOCK ************************************************
\** END OF TIME STAMP BLOCK ****************************************/
\/******************************************************************/
\/*                                                                */
\/*      MODULE NAME: EAMTIFXC.B86                                 */
\/*                                                                */
\/*      DESCRIPTIVE NAME: EXTRACT INDIVIDUAL CHECK DATA           */
\/*                                                                */
\/*      COPYRIGHT: 5696-546 THIS MODULE IS "RESTRICTED            */
\/*                 MATERIALS OF IBM" (C) COPYRIGHT IBM CORP       */
\/*                 1986,1993 ALL RIGHTS RESERVED LICENSED         */
\/*                 MATERIALS - PROPERTY OF IBM                    */
\/*                                                                */
\/*                 REFER TO COPYRIGHT INSTRUCTIONS FORM NUMBER    */
\/*                 G120-2083                                      */
\/*                                                                */
\/*      THIS PROGRAM IS INVOKED FROM THE COMMAND LINE. IT CAN BE  */
\/*      STARTED WITH OR WITHOUT COMMAND LINE PARAMETERS. IF NO    */
\/*      COMMAND LINE PARAMETERS ARE PROVIDED, THE USER WILL       */
\/*      BE PROMPTED FOR THE REQUIRED INFORMATION.                 */
\/*                                                                */
\/*      THE COMMAND LINE PARAMETERS ARE:                          */
\/*                                                                */
\/* EAMTIFXL filename type_search search_parameter offset(optional)*/
\/*                                                                */
\/*              FILENAME - IMAGE INPUT FILE NAME                  */
\/*                                                                */
\/*           TYPE_SEARCH - TAG OR IMAGE                           */
\/*                    TAG - (DEFAULT) PROVIDES DATE, TIME AND     */
\/*                          TERMINAL ID FOR EACH IMAGE ON FILE    */
\/*                          THAT MEET THE SEARCH PARAMETER        */
\/*                          CRITERIA.                             */
\/*                  IMAGE - PROVIDES AN IMAGE FILE FOR EACH IMAGE */
\/*                          ON FILE THAT MEET THE SEARCH CRITERIA.*/
\/*                                                                */
\/*      SEARCH_PARAMETER - DATE OR ALL                            */
\/*                   DATE - THE DATE OF INTEREST, SPECIFIED AS    */
\/*                          DATEyymmdd                            */
\/*                    ALL - (DEFAULT IF TAG SEARCH) ALL TAG OR    */
\/*                          IMAGE DATA IN THE INPUT FILE.         */
\/*                                                                */
\/*                OFFSET - (OPTIONAL) OFFSET, IN THE USER STRING  */
\/*                         IN THE IMAGE HEADER STRING, TO THE     */
\/*                         DESIRED OUTPUT FILE NAME FOR THAT      */
\/*                         IMAGE. THE FILE NAME SHOULD CONFORM TO */
\/*                         THE 8.3 NAMING CONVENTION. THE OFFSET  */
\/*                         SHOULD BE STATED RELATIVE TO THE VALUE */
\/*                         OF 1. ENTERED AS: OFFSET=xxx,          */
\/*                         xxx MAY BE VARIABLE IN LENGTH.         */
\/*                         IF NO OFFSET IS PROVIDED, A UNIQUE     */
\/*                         NAME WILL BE CREATED FOR EACH IMAGE    */
\/*                         FILE REQUESTED. THE NAME WILL BE BASED */
\/*                         ON THE TERMINAL ID WHERE THE IMAGE WAS */
\/*                         CAPTURED AND THE DATE AND TIME OF THE  */
\/*                         PROCESSING OF THE CHECK.               */
\/*                         THE FILE WILL BE CREATED IN THE        */
\/*                         CURRENT ACTIVE DIRECTORY.              */
\/*                                                                */
\/*    PROGRAM IS NOT NLS ENABLED AS THIS FUNCTION IS ONLY         */
\/*    RELEVANT IN THE US.  It would be a small effort to          */
\/*    externalize the prompts if it became necessary.             */
\/*                                                                */
\/*    IR52752  Added tender amount fld to the search result       */
\/*             listing for image searchs by date.                 */
\/*             KHG IBM 11/11/03                                   */
\/*                                                                */
\/* END-OF-SPECIFICATIONS ******************************************/

 rem  ******************* main line start ***************************
 rem
 rem  open a converted file (converted by EAMTIFCL.286). Read header.
 rem  create a new file from data after removing control characters.
 rem
 rem  ***************************************************************
 rem   on error goto tiferror
 rem  ***************************************************************
 rem
 rem   **************************************************************
 rem   *********** tiferror - controller error entry point **********
 rem   **************************************************************
 rem
 rem
 rem
 rem   ****** no return, this is main module ************************
 rem
 rem  ********************* main line end ***************************
 rem  ***************************************************************

 STRING  GLOBAL          \
      PROMPTS$(1),       \ array of prompt strings
      COMMTAIL$,         \ command tail
      STORE.NUM$,        \ store number
      INPUT.FILE$,       \ input file name
      OUTPUT.FILE$,      \ output file name
      FORMAT.STRING$,    \ format string for readforms
      READ.STRING$,      \ string version of the file
      USER.TAG$,         \ user string
      TIF.ERRN$,         \ string version of TIF.ERRN
      TIF.WERR$,         \ error code for "ERR" function
      HEADER$,           \ header string
      SEARCH.MATCH$,     \ tag data for a search match
      IMG.DATE$,         \ date of image from header yymmdd
      IMG.TIME$,         \ time of image from header hhmmss
      TERM.NUM$,         \ terminal number from header
      SIZE.OF.BLOCKS$,   \ size of blocks from WM
      NUM.OF.BLOCKS$,    \ number of blocks for image
      IMG.SIZE$,         \ size of check image
      REMAINING.SIZE$,   \ size of data in last block
      MICR.LEN$,         \ length of micr string
      IMG.TNDR.AMT$,     \ amt for the check tendered
      TIF.WRITE$(1)      ! array for writing out TIF via strings

 INTEGER GLOBAL          \
      VALID.INPUT.PARMS, \ valid input parameters
      TAG.SEARCH,        \ flag if a TAG search
      TAG.BY.DATE,       \ flag if a TAG search by date
      TAG.BY.TERM,       \ flag if a TAG search by terminal number
      IMAGE.SEARCH,      \ flag if an IMAGE search
      IMAGE.ALL,         \ output all IMAGE files
      TAG.ALL,           \ output all TAG data
      END.OF.FILE.FOUND  ! end of EAMIMGO file found flag

 INTEGER*2 GLOBAL        \
      WORK2I2,           \ temp variable
      OUTPUT.FILE.CNT,   \ output file count
      FILENAME.OFFSET,   \ offset, in user.tag$, to file name
      READ.STRING.LEN,   \ LEN of string from file
      TIF.ESESSION       ! Session number of error returned from "ERRF%" function

 INTEGER*4 GLOBAL        \
      TIF.ERRN           ! Return code from "ERRN" function.


!********************************************************************
!        EXTERNAL DEFINITIONS FOR ADX SERVICES
!********************************************************************

 FUNCTION ADXERROR (TERM,MSGGRP,MSGNUM,SEVERITY,EVENT,UNIQUE$) EXTERNAL
    INTEGER*2   TERM,MSGNUM
    INTEGER*1   SEVERITY,MSGGRP,EVENT
    STRING      UNIQUE$
 END FUNCTION


!*********************************************************************
!
! ASSEMBLER SUBROUTINE DEFINITIONS
!
! These definitions should be copied by any routine which wishes to
! use the performance-enhanced assembler routines.
!
!*********************************************************************

!*********************************************************************
!
! SUBSTR
!
! This routine performs a merge of two strings. String P3$, offset P4
! is merged into string P1$ offset P2 for a length of P5 bytes
!
! CAUTION: P1$ must be long enough to totally contain P3$
!
!*********************************************************************

 FUNCTION SUBSTR(P1$,P2,P3$,P4,P5) EXTERNAL
   STRING P1$,P3$
   INTEGER*2 P2,P4,P5
 END FUNCTION


 rem  ****************************************************************
 rem  DIGIT.TO.CHARACTER - converts the passed in two digit string to
 rem      a single character representation.  Digits 0 to 9 remain the
 rem      same, while anything above gets an alphabetic value from
 rem      A to N (10 to 23 respectively).
 rem  ****************************************************************
  FUNCTION DIGIT.TO.CHARACTER$(IN$) PUBLIC

    STRING IN$,                                  \ input digit as a string
           DIGIT.TO.CHARACTER$                   ! converted value

    INTEGER IN.VALUE                             ! input as an integer

    IN.VALUE = VAL(IN$)                          ! convert to integer
    DIGIT.TO.CHARACTER$ = STR$(IN.VALUE)         ! initial for values 0 to 9

    ON IN.VALUE GOTO DONE,DONE,DONE,DONE,DONE,DONE,DONE,DONE,DONE, \
            A,B,C,D,E,F,G,H,I,J,K,L,M,N

    A:
       DIGIT.TO.CHARACTER$ = "A"
       GOTO DONE
    B:
       DIGIT.TO.CHARACTER$ = "B"
       GOTO DONE
    C:
       DIGIT.TO.CHARACTER$ = "C"
       GOTO DONE
    D:
       DIGIT.TO.CHARACTER$ = "D"
       GOTO DONE
    E:
       DIGIT.TO.CHARACTER$ = "E"
       GOTO DONE
    F:
       DIGIT.TO.CHARACTER$ = "F"
       GOTO DONE
    G:
       DIGIT.TO.CHARACTER$ = "G"
       GOTO DONE
    H:
       DIGIT.TO.CHARACTER$ = "H"
       GOTO DONE
    I:
       DIGIT.TO.CHARACTER$ = "I"
       GOTO DONE
    J:
       DIGIT.TO.CHARACTER$ = "J"
       GOTO DONE
    K:
       DIGIT.TO.CHARACTER$ = "K"
       GOTO DONE
    L:
       DIGIT.TO.CHARACTER$ = "L"
       GOTO DONE
    M:
       DIGIT.TO.CHARACTER$ = "M"
       GOTO DONE
    N:
       DIGIT.TO.CHARACTER$ = "N"
       GOTO DONE

    DONE:
  FEND


 rem  ****************************************************************
 rem  VALIDATE.INWORD - this function takes in the string keyword from
 rem      the input parameter processing and validates whether it is
 rem      a supported function or not.  TAG and IMAGE are supported.
 rem      TAG will be the default if no input is received.
 rem  ****************************************************************
  FUNCTION VALIDATE.INWORD(IN$) PUBLIC

    STRING  IN$
    INTEGER VALIDATE.INWORD

    VALIDATE.INWORD = -1                         ! will always be valid

    IF (IN$ = "TAG") THEN BEGIN
       TAG.SEARCH = -1                           ! set tag flag
    ENDIF ELSE IF (IN$ = "IMAGE") THEN BEGIN
       IMAGE.SEARCH = -1                         ! set image flag
    ENDIF ELSE BEGIN
       TAG.SEARCH = -1                           ! default to tag
    ENDIF

  FEND


 rem  ****************************************************************
 rem  CONVERT.TO.HEX$ - This routine converts the passed in ERRN value
 rem      returns a formated string in HEX format for output display.
 rem  ****************************************************************
  FUNCTION CONVERT.TO.HEX$(IN) PUBLIC

    STRING  CONVERT.TO.HEX$, Z$, Y$
    INTEGER*4 IN, S%, SX%, THE.SUM%

    Y$ = ""
    FOR S% = 28 TO 0 STEP -4
       SX% = SHIFT(IN, S%)
       THE.SUM% = SX% AND 0000FH
       IF THE.SUM% > 9 THEN        \
          THE.SUM% = THE.SUM% + 55 \
       ELSE                        \
          THE.SUM% = THE.SUM% + 48
       Z$ = CHR$(THE.SUM%)
       Y$ = Y$ + Z$
    NEXT S%
    CONVERT.TO.HEX$ = Y$

  FEND


 rem  ****************************************************************
 rem  Set up prompts - this is intended for US use only, so these
 rem                   prompts are not being put to a file for
 rem                   translation.
 rem  ****************************************************************
  SUB SET.UP.PROMPTS PUBLIC

     DIM PROMPTS$(16)

     PROMPTS$(1) = "Input IMAGE(to create image file) or TAG(for list) TAG is default: "
     PROMPTS$(2) = "Input search parameter (DATEyymmdd) or ALL, All is default"
     PROMPTS$(3) = "Input the name for the output file (eight characters only, no extension): "
     PROMPTS$(4) = "Invalid input parameters.  Please try again."
     PROMPTS$(5) = "IMAGE keyword requires parameters."
     PROMPTS$(6) = "Date   Time  Terminal   Amount  -  Tag Results    "
     PROMPTS$(7) = "---------------------------------------------"
     PROMPTS$(8) = "Enter name of input file:  Default = EAMIMGH : "
     PROMPTS$(9) = "ERROR on Input File: "
     PROMPTS$(10) = "ERROR on Output File: "
     PROMPTS$(11) = "Date   Time  Terminal  Amount   -> Image file created  "
     PROMPTS$(12) = "Do you have an output file name in the user data (Y/N)? :"
     PROMPTS$(13) = "What is the file name offset?"
     PROMPTS$(14) = "Enter date search as: DATEyymmdd  You entered:"
     PROMPTS$(15) = "Enter terminal search as: TERMtttt  You entered:"
     PROMPTS$(16) = "Invalid data with OFFSET="

  END SUB


 rem  ****************************************************************
 rem  Gather parameters from command line, or prompt if none entered.
 rem  ****************************************************************
  SUB PROCESS.INPUT.PARMS PUBLIC

     INTEGER*2  MATCH.POS                        ! match positions
     INTEGER*2  TAILLEN                          ! length of commtail$
     STRING     INWORD$                          ! key word call
     STRING     INSEARCH$                        ! search parameter
     STRING     YES$                             ! Y/N result
     STRING     WORK.STR$                        ! work string

     TAG.SEARCH = 0                              ! initialize everything
     IMAGE.SEARCH = 0
     IMAGE.ALL = 0
     TAG.ALL = 0
     TAG.BY.DATE = 0
     TAG.BY.TERM = 0
     FILENAME.OFFSET = 0
     SEARCH.MATCH$ = ""
     YES$ = ""
     VALID.INPUT.PARMS = -1

     COMMTAIL$ = COMMAND$                        ! get command parameters
     IF COMMAND$ = "" THEN BEGIN                 ! if no parameters

        ! if no parameters, prompt for input
        PRINT PROMPTS$(8)                        ! get input file name
        INPUT INPUT.FILE$
        INPUT.FILE$ = UCASE$(INPUT.FILE$)
        IF INPUT.FILE$ = "" THEN BEGIN           ! if no data
           INPUT.FILE$ = "EAMIMGH"               ! set default
        ENDIF
        PRINT PROMPTS$(1)                        ! get type of search
        INPUT INWORD$
        VALID.INPUT.PARMS = VALIDATE.INWORD(UCASE$(INWORD$))  ! validate keyword

        IF VALID.INPUT.PARMS THEN BEGIN          ! if proper keyword, prompt for more.

           PRINT PROMPTS$(2)                     ! get search parameters
           INPUT INSEARCH$
           INSEARCH$ = UCASE$(INSEARCH$)

           ! If a tag search and nothing entered or ALL entered, set flag.
           IF TAG.SEARCH THEN BEGIN
              IF ((INSEARCH$ = "") OR (INSEARCH$ = "ALL")) THEN BEGIN
                 TAG.ALL = -1                    ! set flag
              ENDIF ELSE BEGIN                   ! date entered

                 ! If a tag search and keyword DATE, set up rest of string for search.
                 IF (LEFT$(INSEARCH$, 4) = "DATE") THEN BEGIN
                    IF LEN(INSEARCH$) < 10 THEN BEGIN  ! check for complete date
                       PRINT PROMPTS$(14)        ! tell them that more info is needed
                       VALID.INPUT.PARMS = 0     ! start over
                       EXIT SUB                  ! get out
                    ENDIF ELSE BEGIN             ! keyword was found
                       TAG.BY.DATE = -1          ! set flag
                       INSEARCH$ = RIGHT$(INSEARCH$, 6) ! get date
                    ENDIF                        ! endif good data
                 ENDIF ELSE BEGIN                ! invalid keyword
                    PRINT PROMPTS$(14)           ! tell them that more info is needed
                    VALID.INPUT.PARMS = 0        ! start over
                    EXIT SUB                     ! get out
                 ENDIF
              ENDIF                              ! endif not all
           ENDIF                                 ! endif tag search

           ! If an image search with date, also prompt for an output file name.
           IF IMAGE.SEARCH THEN BEGIN
              IF (LEFT$(INSEARCH$, 4) = "DATE") THEN BEGIN ! date search
                 IF LEN(INSEARCH$) < 10 THEN BEGIN ! is it the correct length
                    WORK.STR$ = PROMPTS$(14) + "  " + INSEARCH$
                    PRINT WORK.STR$              ! tell them what is wrong
                    VALID.INPUT.PARMS = 0        ! start over
                    EXIT SUB                     ! get out
                 ENDIF ELSE BEGIN                ! endif len <
                    TAG.BY.DATE = -1             ! set flag
                    INSEARCH$ = RIGHT$(INSEARCH$, 6) ! get date yymmdd
                    PRINT PROMPTS$(12)           ! is the file name in user.tag$
                    INPUT YES$
                    YES$ = UCASE$(YES$)
                    IF YES$ = "Y" THEN BEGIN     ! if it is
                       PRINT PROMPTS$(13)        ! where does it start
                       INPUT WORK.STR$
                       FILENAME.OFFSET = VAL(WORK.STR$)
                    ENDIF                        ! endif filename offset
                 ENDIF                           ! endif good data
              ENDIF ELSE BEGIN                   ! all images requested
                 ! hidden option for all images output.
                 IF (INSEARCH$ = "ALL") THEN BEGIN
                    IMAGE.ALL = -1               ! set flag
                    PRINT PROMPTS$(12)           ! is the file name in user.tag$
                    INPUT YES$
                    YES$ = UCASE$(YES$)
                    IF YES$ = "Y" THEN BEGIN     ! if it is
                       PRINT PROMPTS$(13)        ! where does it start
                       INPUT WORK.STR$
                       FILENAME.OFFSET = VAL(WORK.STR$)
                    ENDIF                        ! endif filename offset
                 ENDIF ELSE BEGIN                ! else invalid keyword
                    WORK.STR$ = PROMPTS$(14) + "  " + INSEARCH$
                    PRINT WORK.STR$              ! tell them what is wrong
                    VALID.INPUT.PARMS = 0        ! start over
                    EXIT SUB                     ! get out
                 ENDIF
              ENDIF
           ENDIF                                 ! endif image file request
           SEARCH.MATCH$ = INSEARCH$
        ENDIF
     ENDIF ELSE BEGIN

       ! parameter tail has something.  Need to see if it is what we expect.
       TAILLEN = LEN(COMMTAIL$)
       MATCH.POS = MATCH(" ", COMMTAIL$, 1)      ! find the first blank
       IF MATCH.POS = 0 THEN BEGIN               ! if just one or no parameters
          VALID.INPUT.PARMS = 0                  ! not valid
          EXIT SUB                               ! get out
       ENDIF

       ! if more than one parameter, pull off the first which is the input file name.
       INPUT.FILE$ = LEFT$(COMMTAIL$, MATCH.POS - 1)
       COMMTAIL$ = RIGHT$(COMMTAIL$, TAILLEN - MATCH.POS)
       TAILLEN = LEN(COMMTAIL$)

       MATCH.POS = MATCH(" ", COMMTAIL$, 1)      ! find next blank
       IF MATCH.POS = 0 THEN BEGIN               ! if no blank found,
          INWORD$ = COMMTAIL$                    ! just a key word alone entered
          VALID.INPUT.PARMS = VALIDATE.INWORD(INWORD$) ! see if a valid keyword

          ! if keyword is valid, note error that parameters are required for IMAGE search.
          IF (VALID.INPUT.PARMS AND IMAGE.SEARCH) THEN BEGIN
             PRINT PROMPTS$(5)                   ! tell them that more info is needed
             VALID.INPUT.PARMS = 0               ! start over
             EXIT SUB                            ! get out
          ENDIF

          ! if keyword valid and TAG search, no parms is equivalent to all search.
          IF (VALID.INPUT.PARMS AND TAG.SEARCH) THEN BEGIN
             TAG.ALL = -1
             EXIT SUB
          ENDIF
       ENDIF

       ! keyword has parameters so continue.
       INWORD$ = LEFT$(COMMTAIL$, MATCH.POS - 1)    ! pull off keyword
       VALID.INPUT.PARMS = VALIDATE.INWORD(INWORD$) ! see if a valid keyword

       COMMTAIL$ = RIGHT$(COMMTAIL$, TAILLEN - MATCH.POS)
       TAILLEN = LEN(COMMTAIL$)

       ! next parameter will be search criteria.
       MATCH.POS = MATCH(" ", COMMTAIL$, 1)
       ! if another blank found, also inputting a file name for IMAGE.
       IF MATCH.POS <> 0 THEN BEGIN
          INSEARCH$ = LEFT$(COMMTAIL$, MATCH.POS - 1)
       ENDIF ELSE BEGIN
          INSEARCH$ = COMMTAIL$
       ENDIF

       ! if image search, check for ALL and/or output file name.
       IF IMAGE.SEARCH THEN BEGIN
          IF INSEARCH$ = "ALL" THEN BEGIN        ! get all images
             IMAGE.ALL = -1                      ! set the flag
          ENDIF ELSE BEGIN
             IF (LEFT$(INSEARCH$, 4) = "DATE") THEN BEGIN ! date search
                IF LEN(INSEARCH$) < 10 THEN BEGIN ! is it the correct length
                   WORK.STR$ = PROMPTS$(14) + "  " + INSEARCH$
                   PRINT WORK.STR$              ! tell them what is wrong
                   VALID.INPUT.PARMS = 0        ! start over
                   EXIT SUB                     ! get out
                ENDIF ELSE BEGIN                ! endif len <
                   TAG.BY.DATE = -1             ! set flag
                   INSEARCH$ = RIGHT$(INSEARCH$, 6) ! get date yymmdd
                ENDIF
             ENDIF
          ENDIF
          COMMTAIL$ = RIGHT$(COMMTAIL$, TAILLEN - MATCH.POS)
          TAILLEN = LEN(COMMTAIL$)
          FILENAME.OFFSET = 0                   ! no offset provided
          ! look for file name offset
          MATCH.POS = MATCH("OFFSET=", COMMTAIL$, 1)
          IF MATCH.POS <> 0 THEN BEGIN
             IF (TAILLEN < 8) OR (TAILLEN > 413) THEN BEGIN
                PRINT PROMPTS$(16)
                VALID.INPUT.PARMS = 0           ! start over
                EXIT SUB                        ! get out
             ENDIF ELSE BEGIN
                FILENAME.OFFSET = VAL(RIGHT$(COMMTAIL$, TAILLEN - 7))
             ENDIF
          ENDIF
       ENDIF

       ! if tag search, check for ALL.
       IF TAG.SEARCH THEN BEGIN
          IF (INSEARCH$ = "ALL") THEN BEGIN
             TAG.ALL = -1
          ENDIF ELSE BEGIN
          ! If a tag search and keyword DATE, set up rest of string for search.
             IF (LEFT$(INSEARCH$, 4) = "DATE") THEN BEGIN
                IF LEN(INSEARCH$) < 10 THEN BEGIN
                   WORK.STR$ = PROMPTS$(14) + "  " + INSEARCH$
                   PRINT WORK.STR$                 ! tell them that more info is needed
                   VALID.INPUT.PARMS = 0           ! start over
                   EXIT SUB                        ! get out
                ENDIF ELSE BEGIN                   ! endif len <
                   TAG.BY.DATE = -1
                   INSEARCH$ = RIGHT$(INSEARCH$, 6)
                ENDIF                              ! endif date tag
             ENDIF
          ENDIF                                    ! endif not ALL
       ENDIF                                       ! endif tag search
       SEARCH.MATCH$ = INSEARCH$
     ENDIF


  END SUB


 rem  ****************************************************************
 rem  Open the TIF spooling file written to by EAMTIFCL.286.
 rem  ****************************************************************
  SUB OPEN.INPUT.FILE PUBLIC

    OPEN INPUT.FILE$ AS 65 BUFFSIZE 16000 NOWRITE NODEL

  END SUB

 rem  ****************************************************************
 rem  Close the file created from EAMTIFCL.286.
 rem  ****************************************************************
  SUB CLOSE.INPUT.FILE PUBLIC

    CLOSE 65

  END SUB


 rem  ****************************************************************
 rem  Read header portion and set up info.
 rem  ****************************************************************
  SUB PULL.HEADER.RECORD PUBLIC

    FORMAT.STRING$ = "C508"

    READ FORM FORMAT.STRING$; #65; HEADER$

    ! if we have not read past the end of file, continue.
    IF NOT END.OF.FILE.FOUND THEN BEGIN
       READ.STRING.LEN = LEN(HEADER$)
       IMG.DATE$ = MID$(HEADER$, 1, 6)        ! image date yymmdd
       IMG.TIME$ = MID$(HEADER$, 7, 6)        ! transaction time hhmmss
       TERM.NUM$ = MID$(HEADER$, 13, 4)       ! terminal id
       STORE.NUM$ = MID$(HEADER$, 17, 4)      ! store number
       SIZE.OF.BLOCKS$ = MID$(HEADER$, 21, 3) ! size of each data block (not used)
       IMG.SIZE$ = MID$(HEADER$, 24, 5)       ! size of the image in bytes
       NUM.OF.BLOCKS$ = MID$(HEADER$, 29, 2)  ! num of data blocks (not used)
       REMAINING.SIZE$ = MID$(HEADER$, 31, 3) ! size of last data block (not used)
       IMG.TNDR.AMT$ = MID$(HEADER$, 34, 8)   ! amt for the check tendered
       MICR.LEN$ = MID$(HEADER$, 42, 2)       ! length of micr string
       USER.TAG$ = RIGHT$(HEADER$, 415)       ! user data
    ENDIF

  END SUB


 rem  ****************************************************************
 rem  Create new output file (one for image without control data).
 rem  ****************************************************************
  SUB CREATE.NEW.FILE PUBLIC

    ! create a new sequential file to output the image data as it
    ! is rebuilt (without the control characters from the matrix writes).
    CREATE OUTPUT.FILE$ AS 66 BUFFSIZE 16000

  END SUB


 rem  ****************************************************************
 rem  Close the output file created by program.
 rem  ****************************************************************
  SUB CLOSE.OUTPUT.FILE PUBLIC

    CLOSE 66

  END SUB



 rem  ****************************************************************
 rem  Not looking for images so skip over them.
 rem  ****************************************************************
  SUB SKIP.OVER.IMAGE.BLOCK PUBLIC

    STRING IN.STRING$

    ! skip over the image data for this picture.
    FORMAT.STRING$ = "C" + IMG.SIZE$
    READ FORM FORMAT.STRING$; #65; IN.STRING$

  END SUB


 rem  ****************************************************************
 rem  Parse the old data to new data - without control characters.
 rem  ****************************************************************
  SUB PARSE.OLD.TO.NEW PUBLIC

    STRING    OUT.STRING$,                       \ giant output string
              IN.STRING$                         ! input string read

    CALL CREATE.NEW.FILE                         ! create new TIF file

    ! read in next record for this image.
    FORMAT.STRING$ = "C" + IMG.SIZE$
    READ FORM FORMAT.STRING$; #65; IN.STRING$

    ! write form the output string to the sequential file.
    WRITE FORM FORMAT.STRING$; #66; IN.STRING$

    CALL CLOSE.OUTPUT.FILE                       ! close the output file


  END SUB


 rem  ****************************************************************
 rem  Process all images into files.
 rem  ****************************************************************
  SUB PROCESS.IMAGE.ALL PUBLIC

    STRING CONVERTED.TIME$,                      \ time converted to three chars
           CONVERTED.DATE$                       ! date converted to three chars


    PRINT PROMPTS$(11)                           ! put up output header
    PRINT PROMPTS$(7)

    END.OF.FILE.FOUND = 0                        ! set flag to FALSE
    CALL OPEN.INPUT.FILE                         ! open the input file
    CALL PULL.HEADER.RECORD                      ! pull header info

    IF FILENAME.OFFSET = 0 THEN BEGIN            ! no file name in user.tag$
       ! establish output file name
       ! need to convert month to one character, hour to one character
       ! and store in new string values.
       CONVERTED.DATE$ = DIGIT.TO.CHARACTER$(MID$(IMG.DATE$,3,2)) + \
                                             MID$(IMG.DATE$,5,2)
       CONVERTED.TIME$ = LEFT$(IMG.TIME$,4)      ! hhmm
       OUTPUT.FILE$ = TERM.NUM$ + CONVERTED.TIME$ + "." + CONVERTED.DATE$
       WHILE (NOT END.OF.FILE.FOUND)
          CALL PARSE.OLD.TO.NEW                  ! create an image file
          PRINT IMG.DATE$; " "; IMG.TIME$; " "; TERM.NUM$; "    "; \
                IMG.TNDR.AMT$; "     "; OUTPUT.FILE$
          CALL PULL.HEADER.RECORD                ! pull header info
          ! establish output file name
          ! need to convert month to one character, hour to one character
          ! and store in new string values.
          CONVERTED.DATE$ = DIGIT.TO.CHARACTER$(MID$(IMG.DATE$,3,2)) + \
                                                MID$(IMG.DATE$,5,2)
          CONVERTED.TIME$ = LEFT$(IMG.TIME$,4)   ! hhmm
          OUTPUT.FILE$ = TERM.NUM$ + CONVERTED.TIME$ + "." + CONVERTED.DATE$
       WEND                                      ! END.OF.FILE.FOUND
    ENDIF ELSE BEGIN                             ! file name is in user.tag$
       OUTPUT.FILE$ = MID$(USER.TAG$, FILENAME.OFFSET, 12)
       WHILE (NOT END.OF.FILE.FOUND)
          CALL PARSE.OLD.TO.NEW                  ! create an image file
          PRINT IMG.DATE$; " "; IMG.TIME$; " "; TERM.NUM$; "    "; \
                IMG.TNDR.AMT$; "     "; OUTPUT.FILE$
          CALL PULL.HEADER.RECORD                ! pull header info
          OUTPUT.FILE$ = MID$(USER.TAG$, FILENAME.OFFSET, 12)
       WEND                                      ! END.OF.FILE.FOUND
    ENDIF                                        ! file name is in user.tag$
    CALL CLOSE.INPUT.FILE                        ! close the input file

  END SUB


 rem  ****************************************************************
 rem  Process certain images into files.
 rem  ****************************************************************
  SUB PROCESS.IMAGE.SEARCH PUBLIC

    STRING  OUTPUT.FILE.NAME$

    STRING CONVERTED.TIME$,                      \ time converted to three chars
           CONVERTED.DATE$                       ! date converted to three chars

    INTEGER LEN.SEARCH.MATCH


    PRINT PROMPTS$(11)                           ! put up output header
    PRINT PROMPTS$(7)

    END.OF.FILE.FOUND = 0                        ! set flag to FALSE
    LEN.SEARCH.MATCH = LEN(SEARCH.MATCH$)

    CALL OPEN.INPUT.FILE                         ! open the input file
    CALL PULL.HEADER.RECORD                      ! pull header info

    IF FILENAME.OFFSET = 0 THEN BEGIN            ! no file name in user.tag$
       ! establish output file name
       ! need to convert month to one character, hour to one character
       ! and store in new string values.
       CONVERTED.DATE$ = DIGIT.TO.CHARACTER$(MID$(IMG.DATE$,3,2)) + \
                                             MID$(IMG.DATE$,5,2)
       CONVERTED.TIME$ = LEFT$(IMG.TIME$,4)      ! hhmm
       OUTPUT.FILE$ = TERM.NUM$ + CONVERTED.TIME$ + "." + CONVERTED.DATE$

       WHILE (NOT END.OF.FILE.FOUND)
          ! if search parameter entered matches header info, then generate image.
          IF SEARCH.MATCH$ = LEFT$(HEADER$, LEN.SEARCH.MATCH) THEN BEGIN
             CALL PARSE.OLD.TO.NEW               ! create an image file
!AIR52752
!             PRINT IMG.DATE$; " "; IMG.TIME$; " "; TERM.NUM$; "        "; OUTPUT.FILE$
             PRINT IMG.DATE$; " "; IMG.TIME$; " "; TERM.NUM$; "    "; \
                IMG.TNDR.AMT$; "     "; OUTPUT.FILE$
!EIR52752
          ENDIF ELSE BEGIN
             ! otherwise skip over image portion
             CALL SKIP.OVER.IMAGE.BLOCK
          ENDIF
          CALL PULL.HEADER.RECORD                ! pull header info
          CONVERTED.DATE$ = DIGIT.TO.CHARACTER$(MID$(IMG.DATE$,3,2)) + \
                                                MID$(IMG.DATE$,5,2)
          CONVERTED.TIME$ = LEFT$(IMG.TIME$,4)   ! hhmm
          OUTPUT.FILE$ = TERM.NUM$ + CONVERTED.TIME$ + "." + CONVERTED.DATE$
       WEND                                      ! END.OF.FILE.FOUND
    ENDIF ELSE BEGIN                             ! file name in user.tag$
       OUTPUT.FILE$ = MID$(USER.TAG$, FILENAME.OFFSET, 12)
       WHILE (NOT END.OF.FILE.FOUND)
          CALL PARSE.OLD.TO.NEW                  ! create an image file
!AIR52752
!          PRINT IMG.DATE$; " "; IMG.TIME$; " "; TERM.NUM$; "        "; OUTPUT.FILE$
          PRINT IMG.DATE$; " "; IMG.TIME$; " "; TERM.NUM$; "    "; \
                IMG.TNDR.AMT$; "     "; OUTPUT.FILE$
!EIR52752
          CALL PULL.HEADER.RECORD                ! pull header info
          OUTPUT.FILE$ = MID$(USER.TAG$, FILENAME.OFFSET, 12)
       WEND                                      ! END.OF.FILE.FOUND
    ENDIF                                        ! file name in user.tag$

    CALL CLOSE.INPUT.FILE                        ! close the input file

  END SUB


 rem  ****************************************************************
 rem  Process all tag data - report to STDOUT.
 rem  ****************************************************************
  SUB PROCESS.TAG.ALL PUBLIC

    STRING IN.STRING$

    PRINT PROMPTS$(6)                            ! output display header
    PRINT PROMPTS$(7)

    END.OF.FILE.FOUND = 0                        ! set flag to FALSE
    CALL OPEN.INPUT.FILE                         ! open the input file
    CALL PULL.HEADER.RECORD                      ! pull header info

    WHILE (NOT END.OF.FILE.FOUND)

       ! print the data from this header record
       PRINT IMG.DATE$; " "; IMG.TIME$; " "; TERM.NUM$; "      ";IMG.TNDR.AMT$
       ! skip over the image block
       CALL SKIP.OVER.IMAGE.BLOCK
       ! ready to pull the next header
       CALL PULL.HEADER.RECORD                   ! pull header info

    WEND                                         ! END.OF.FILE.FOUND
    CALL CLOSE.INPUT.FILE                        ! close the input file

  END SUB


 rem  ****************************************************************
 rem  Process tag search data - report to STDOUT.
 rem  ****************************************************************
  SUB PROCESS.TAG.SEARCH PUBLIC

    STRING IN.STRING$
    INTEGER LEN.SEARCH.MATCH

    PRINT PROMPTS$(6)                            ! output display header
    PRINT PROMPTS$(7)

    END.OF.FILE.FOUND = 0                        ! set flag to FALSE
    LEN.SEARCH.MATCH = LEN(SEARCH.MATCH$)

    CALL OPEN.INPUT.FILE                         ! open the input file
    CALL PULL.HEADER.RECORD                      ! pull header info

    WHILE (NOT END.OF.FILE.FOUND)

       ! print the tag data IF it meets the search criteria specified.
       IF TAG.BY.DATE THEN BEGIN
          IF SEARCH.MATCH$ = LEFT$(HEADER$, 6) THEN           \
             PRINT IMG.DATE$; " "; IMG.TIME$; " "; TERM.NUM$; \
                   "      ";IMG.TNDR.AMT$
       ENDIF ELSE IF TAG.BY.TERM THEN BEGIN
          IF SEARCH.MATCH$ = MID$(HEADER$, 13, 4) THEN        \
             PRINT IMG.DATE$; " "; IMG.TIME$; " "; TERM.NUM$; \
                   "      ";IMG.TNDR.AMT$
       ENDIF ELSE IF SEARCH.MATCH$ = LEFT$(HEADER$, LEN.SEARCH.MATCH) THEN \
          PRINT IMG.DATE$; " "; IMG.TIME$; " "; TERM.NUM$; "      ";IMG.TNDR.AMT$

       ! skip over the image block
       CALL SKIP.OVER.IMAGE.BLOCK

       ! ready to pull the next header
       CALL PULL.HEADER.RECORD                   ! pull header info

    WEND                                         ! END.OF.FILE.FOUND
    CALL CLOSE.INPUT.FILE                        ! close the input file

  END SUB



  !REM------------------------------------------------------------------
  !REM                                                                 |
  !REM                  I\    /I    /\    I  I\  I                     |
  !REM                  I \  / I   /--\   I  I \ I                     |
  !REM                  I  \/  I  /    \  I  I  \I                     |
  !REM                                                                 |
  !REM------------------------------------------------------------------

  ON ERROR GOTO TIFERROR                         !

  CALL SET.UP.PROMPTS                            ! set up prompts
  CALL PROCESS.INPUT.PARMS                       ! get program parameters

  IF NOT VALID.INPUT.PARMS THEN BEGIN            ! if not valid inputs,
     PRINT PROMPTS$(4)                           !   print error
     GOTO END.PROGRAM                            !   exit program
  ENDIF

  IF IMAGE.ALL THEN BEGIN
     CALL PROCESS.IMAGE.ALL                      ! process all images
     GOTO END.PROGRAM
  ENDIF

  IF TAG.ALL THEN BEGIN
     CALL PROCESS.TAG.ALL                        ! report all tags
     GOTO END.PROGRAM
  ENDIF

  IF TAG.SEARCH THEN BEGIN
     CALL PROCESS.TAG.SEARCH                     ! search for certain tags
     GOTO END.PROGRAM
  ENDIF

  IF IMAGE.SEARCH THEN BEGIN
     CALL PROCESS.IMAGE.SEARCH                   ! search for certain images
     GOTO END.PROGRAM
  ENDIF

  GOTO END.PROGRAM


REM---------------------------------------------------------------------------
REM_______________MAIN ON ERROR ROUTINE FOR PROGRAM      _____________________

TIFERROR:                                  !
  TIF.WERR$ = ERR                          ! read in error codes
  TIF.ESESSION = ERRF%                     !
  TIF.ERRN = ERRN                          ! error code

  ! if end of file error on input spool file, close input file and exit program.
  IF (TIF.ESESSION = 65) AND (TIF.WERR$ = "EF") THEN BEGIN
     END.OF.FILE.FOUND = -1
     RESUME
  ENDIF

  TIF.ERRN$ = CONVERT.TO.HEX$(TIF.ERRN)

  ! if any other error on input file, note error.
  IF (TIF.ESESSION = 65) THEN BEGIN
     PRINT PROMPTS$(9); INPUT.FILE$; " "; TIF.WERR$; " "; TIF.ERRN$

     WORK2I2 = ADXERROR( 0,               \ 0 for controller
                  ASC("B"),               \ supermarket
                         0,               \ no. of  msg displayed
                         3,               \ severity level
                        50,               \ 50 = image conversion support
                        "FX"+RIGHT$("00"+STR$(ERRF%),2)+ERR+PACK$(TIF.ERRN$))
  ENDIF

  ! if any other error on output file, note error.
  IF (TIF.ESESSION = 66) THEN BEGIN
     PRINT PROMPTS$(10); OUTPUT.FILE$; "   "; TIF.WERR$; " "; TIF.ERRN$
     WORK2I2 = ADXERROR( 0,               \ 0 for controller
                  ASC("B"),               \ supermarket
                         0,               \ no. of  msg displayed
                         3,               \ severity level
                        50,               \ 50 = image conversion support
                        "FX"+RIGHT$("00"+STR$(ERRF%),2)+ERR+PACK$(TIF.ERRN$))
  ENDIF

END.PROGRAM:
  END
