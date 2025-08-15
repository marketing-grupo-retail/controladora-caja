\/*TIME STAMP BLOCK ************************************************            
\** END OF TIME STAMP BLOCK ****************************************/           
\/******************************************************************/           
\/*                                                                */           
\/*      MODULE NAME: EAMRPU4C                                     */           
\/*                                                                */           
\/*      DESCRIPTIVE NAME: USER REPORT 4                           */           
\/*                                                                */           
\/*                                                                */           
\/*                                                               */            
\/*      COPYRIGHT:                                               */            
\/*      5696-536 THIS MODULE IS "RESTRICTED MATERIALS OF IBM"    */            
\/*      (C) COPYRIGHT IBM CORP 1986, 1993 ALL RIGHTS RESERVED    */            
\/*      LICENSED MATERIALS - PROPERTY OF IBM REFER TO COPYRIGHT  */            
\/*      INSTRUCTIONS FORM NUMBER G120-2083                       */            
\/*                                                               */            
\/*      STATUS: RELEASE 2 LEVEL 0                                */            
\/*                                                               */            
\/*      PERSON RESPONSIBLE =                                      */           
\/*                                                                */           
\/*      FUNCTION: EAMRPU4C                                        */           
\/*                                                                */           
\/*                THIS MODULE IS CALLED FROM THE USER REPORTS     */           
\/*                MENU. IT TAKES THE FORM OF A BASIC SHELL WHICH  */           
\/*                ENABLES A USER TO WRITE AND RUN THEIR OWN       */           
\/*                SPECIFIC REPORT                                 */           
\/*                                                                */           
\/*      NOTES:                                                    */           
\/*                                                                */           
\/*         DEPENDENCIES:                                          */           
\/*                                                                */           
\/*         RESTRICTIONS:                                          */           
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
\/*      ENTRY POINT: RPU4EC01                                     */           
\/*                                                                */           
\/*         PURPOSE: USER REPORT                                   */           
\/*                                                                */           
\/*                THIS MODULE IS ENTERED AFTER USER               */           
\/*                REPORT 4 HAS BEEN SELECTED FROM                 */           
\/*                THE USER REPORTS MENU.                          */           
\/*                                                                */           
\/*                ***** RETURN CONTROL *****                      */           
\/*                                                                */           
\/*         LINKAGE: CHAINED TO FROM EAMRPU4L                      */           
\/*                                                                */           
\/*                                                                */           
\/*      INPUT:                                                    */           
\/*                                                                */           
\/*         SYMBOLIC NAME: MMCHAINP                                */           
\/*                                                                */           
\/*         DESCRIPTION: CHAINING PARAMETERS                       */           
\/*                                                                */           
\/*                                                                */           
\/*                ***** PRIME INPUT *****                         */           
\/*                                                                */           
\/*                                                                */           
\/*      OUTPUT:                                                   */           
\/*                                                                */           
\/*                                                                */           
\/*         SYMBOLIC NAME: MMCHAINP                                */           
\/*                                                                */           
\/*         DESCRIPTION: CHAINING PARAMETERS                       */           
\/*                                                                */           
\/*                                                                */           
\/*                ***** PRIME OUTPUT *****                        */           
\/*                                                                */           
\/*                                                                */           
\/*                                                                */           
\/*      EXIT-NORMAL:                                              */           
\/*                                                                */           
\/*         RETURN CODE:  E_SUCCESS                                */           
\/*                                                                */           
\/*      EXIT-ERROR: RPUREW01                                      */           
\/*                                                                */           
\/*         RETURN CODE:                                           */           
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
\/*                  NAME: RPUREC01 - USER REPORTS MENU            */           
\/*                                                                */           
\/*         DATA AREAS:                                            */           
\/*                                                                */           
\/*           MODIFIED:                                            */           
\/*                                                                */           
\/*            MMCHAINP: CHAINING PARAMETERS                       */           
\/*                                                                */           
\/*           REFERENCED:                                          */           
\/*                                                                */           
\/*         CONTROL BLOCKS: NONE                                   */           
\/*                                                                */           
\/*         MACROS: NONE                                           */           
\/*                                                                */           
\/*         TABLES: NONE                                           */           
\/*                                                                */           
\/*      CHANGE ACTIVITY: LEVEL 0                                  */           
\/*                                                                */           
\/*      CHANGE DATE: SEPTEMBER 1, 1987                            */           
\/*                                                                */           
\/*      IR75403 - INSURE PRINTER CLOSED IN ERROR ROUTINE          */           
\/*                BEFORE REOPENING.                               */           
\/*                IBM CJH 9/1/87                                  */           
\/*                                                                */           
\/***************** END OF SPECIFICATIONS **************************/           
                                                                                
\******************************************************************!!           
\REM                       START OF BBDL                           !!           
\******************************************************************!!           
                                                                                
\REM!!*************************************************************!!           
\REM!!             INCLUDES USED BY THIS MODULE                    !!           
\REM!!*************************************************************!!           
\REM!!                                                             !!           
\REM!!     EAMUSED.J86       !define variables passed in chain     !!           
\REM!!     EAMUSE.J86        !initialize chain variables           !!           
\REM!!     EAMCHAIN.J86      !chaining routine                     !!           
\REM!!*************************************************************!!           
                                                                                
\REM!!*************************************************************!!           
\REM!!                  MAIN PROGRAM START                         !!           
\REM!!*************************************************************!!           
\REM!!                                                             !!           
\REM!!                                                             !!           
\REM!!     %INCLUDE EAMUSED.J86                                    !!           
\REM!!     %INCLUDE EAMUSE.J86                                     !!           
\REM!!                                                             !!           
\REM!!     begin writing user report after include statements.     !!           
\REM!!     to return to user report menu call chainmod             !!           
\REM!!                                                             !!           
\REM!!*************************************************************!!           
                                                                                
\REM!!*************************************************************!!           
\REM!!                 CHAIN.MODULE                                !!           
\REM!!*************************************************************!!           
\REM!!  SUB CHAINMOD                                               !!           
\REM!!                                                             !!           
\REM!!     CHAINTO$ = "EAMRPURL.286"                               !!           
\REM!!     MM.CHAINFROM$ = "EAMRPU4L.286"                          !!           
\REM!!     %INCLUDE EAMCHAIN.J86                                   !!           
\REM!!                                                             !!           
\REM!!  END SUB                                                    !!           
\REM!!                                                             !!           
\REM!!*************************************************************!!           
                                                                                
\REM!!*************************************************************!!           
\REM!!                 START OF CODE                               !!           
\REM!!*************************************************************!!           
                                                                                
\******************************************************************!!           
\REM!!              VARIABLE DECLARATIONS                          !!           
\******************************************************************!!           
                                                                                
%INCLUDE EAMUSED.J86  ! Define variables passed on chain                        
                                                                                
INTEGER*2 FKEY.CHECK(1)   ! used to check the f keys                            
                                                                                
                                                                                
STRING                    \                                                     
          CHAINTO$,       \ name of module to be chained to on exit             
          RETURN.CODE$    \ runtime return code for on error                    
                          \                                                     
                                                                                
\REM!!*************************************************************!!           
\REM!!                     CHAIN MODULE                            !!           
\REM!!*************************************************************!!           
\REM!! This subroutine CHAINS back to the user reports menu when                
\REM!! this report is completed.                                                
                                                                                
   SUB CHAINMOD                                                                 
                                                                                
   INTEGER            \                                                         
       CLSDIS,        \                                                         
       BLANK.OK       !                                                         
                                                                                
       CHAINTO$ = MM.CHAINFROM$                                                 
       MM.CHAINFROM$ = "EAMRPU4L.286"                                           
       %INCLUDE EAMCHAIN.J86                                                    
                                                                                
    END SUB                                                                     
                                                                                
\******************************************************************!!           
                                                                                
\REM!!*************************************************************!!           
\REM!!               MAIN PROGRAM START                            !!           
\REM!!*************************************************************!!           
                                                                                
                                                                                
RPU4EC01:                                                                       
                                                                                
  ON ERROR GOTO RPU4EW01                                                        
                                                                                
  %INCLUDE EAMUSE.J86                                                           
                                                                                
                           !-------------------------------------------!        
  MM.SCRN.INPUT$(1) = "0"  ! THIS INDICATES THAT THE REPORT HAS NOT    !        
                           ! BEEN IMPLEMENTED. WHEN YOU IMPLEMENT THE  !        
                           ! REPORT SET MM.SCRN.INPUT$(1) = "1"        !        
                           !-------------------------------------------!        
                                                                                
\REM!!*************************************************************!!           
\REM!!        START THE MAIN CODE UNDER THIS HEADER                !!           
\REM!!*************************************************************!!           
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
\REM!!*************************************************************!!           
\REM!!        FINISH THE MAIN CODE BEFORE THIS HEADER              !!           
\REM!!*************************************************************!!           
                                                                                
       CALL CHAINMOD   ! this will exit from the user report                    
                       ! and return control to the user reports menu            
                                                                                
\REM!!*************************************************************!!           
\REM!!            RUN TIME ERROR HANDLING                          !!           
\REM!!*************************************************************!!           
                                                                                
RPU4EW01:                                                                       
                                                                                
   CONSOLE          ! CLOSE PRINTER IF OPEN                    IR75403          
   LPRINTER:PRINT "RETURN CODE = ";ERR;"SESSION NO = ";ERRF% : CONSOLE          
                                                                                
\REM!!*************************************************************!!           
\REM!!     PLACE ANY CODE FOR HANDLING ERRORS UNDER THIS HEADER    !!           
\REM!!*************************************************************!!           
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
END                                                                             
