\/*TIME STAMP BLOCK ************************************************            
\** END OF TIME STAMP BLOCK ****************************************/           
\/******************************************************************/           
\/*                                                                */           
\/*      MODULE NAME: EAMAPAUC                                     */           
\/*                                                                */           
\/*      DESCRIPTIVE NAME: PERSONALIZATION USER PROCEDURE          */           
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
\/*      STATUS: RELEASE 1 LEVEL 0                                 */           
\/*                                                                */           
\/*      PERSON RESPONSIBLE =                                      */           
\/*                                                                */           
\/*      FUNCTION: EAMAPAUC                                        */           
\/*                                                                */           
\/*                THIS MODULE IS CALLED FROM THE PERSONALIZATION  */           
\/*                MENU. IT TAKES THE FORM OF A BASIC SHELL        */           
\/*                WHICH ENABLES THE USER TO WRITE AND RUN THEIR   */           
\/*                OWN SPECIFIC PROCEDURE.                         */           
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
\/*      ENTRY POINT: APAUEC01                                     */           
\/*                                                                */           
\/*         PURPOSE: USER PROCEDURE                                */           
\/*                                                                */           
\/*                THIS MODULE IS ENTERED AFTER USER               */           
\/*                PROCEDURE   HAS BEEN SELECTED FROM              */           
\/*                THE PERSONALIZATION MENU.                       */           
\/*                                                                */           
\/*                ***** RETURN CONTROL *****                      */           
\/*                                                                */           
\/*         LINKAGE: CHAINED TO FROM EAMAPM1L                      */           
\/*                                                                */           
\/*      INPUT:                                                    */           
\/*                                                                */           
\/*         SYMBOLIC NAME: MMCHAINP                                */           
\/*                                                                */           
\/*         DESCRIPTION: CHAINING PARAMETERS                       */           
\/*                                                                */           
\/*                ***** PRIME INPUT *****                         */           
\/*                                                                */           
\/*      OUTPUT:                                                   */           
\/*                                                                */           
\/*         SYMBOLIC NAME: MMCHAINP                                */           
\/*                                                                */           
\/*         DESCRIPTION: CHAINING PARAMETERS                       */           
\/*                                                                */           
\/*                ***** PRIME OUTPUT *****                        */           
\/*                                                                */           
\/*                                                                */           
\/*      EXIT-NORMAL:                                              */           
\/*                                                                */           
\/*         RETURN CODE:  E_SUCCESS                                */           
\/*                                                                */           
\/*      EXIT-ERROR: APAUEW01                                      */           
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
\/*                                                                */           
\/*                                                                */           
\/*                                                                */           
\/***************** END OF SPECIFICATIONS **************************/           
                                                                                
\******************************************************************!!           
\REM                       START OF BBDL                           !!           
\******************************************************************!!           
                                                                                
\REM!!*************************************************************!!           
\REM!!             INCLUDES USED BY THIS MODULE                    !!           
\REM!!*************************************************************!!           
\REM!!                                                             !!           
\REM!!     EAMUSED.J86      ! define variables passed in chain     !!           
\REM!!     EAMUSE.J86       ! initialize chain variables           !!           
\REM!!     EAMCHAIN.J86     ! chaining routine                     !!           
\REM!!     EAMXXCPY.J86     ! copyright statement for object       !!           
\REM!!*************************************************************!!           
                                                                                
\REM!!*************************************************************!!           
\REM!!                  MAIN PROGRAM START                         !!           
\REM!!*************************************************************!!           
\REM!!                                                             !!           
\rem!!     begin writing user procedure after include statements.  !!           
\rem!!     to return to personalization menu call chainmod         !!           
\REM!!                                                             !!           
\REM!!*************************************************************!!           
                                                                                
\REM!!*************************************************************!!           
\REM!!                 CHAIN.MODULE                                !!           
\REM!!*************************************************************!!           
\REM!!  SUB CHAINMOD                                               !!           
\REM!!                                                             !!           
\REM!!     chain back to personalization menu                      !!           
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
                                                                                
                                                                                
STRING                    \                                                     
          CHAINTO$,       \ name of module to be chained to on exit             
          RETURN.CODE$    ! runtime return code for on error                    
INTEGER*2 FKEY.CHECK(1)   ! used by EAMUSED                                     
                                                                                
\REM!!*************************************************************!!           
\REM!!                     CHAIN MODULE                            !!           
\REM!!*************************************************************!!           
\REM!! This subroutine CHAINS back to the user procedures menu when             
\REM!! this procedure is completed.                                             
                                                                                
   SUB CHAINMOD                                                                 
                                      !-----------------------------!           
   INTEGER       \                    !                             !           
       CLSDIS,   \                    !                             !           
       BLANK.OK  !                    ! THIS IS THE INTERFACE BACK  !           
                                      ! TO THE PERSONALIZATION MENU !           
       CHAINTO$ = "EAMAPM1L.286"      ! DO NOT CHANGE THIS CODE.    !           
       MM.CHAINFROM$ = "EAMAPAUL.286" !                             !           
       %INCLUDE EAMCHAIN.J86          !                             !           
                                      !-----------------------------!           
    END SUB                                                                     
                                                                                
\******************************************************************!!           
                                                                                
\REM!!*************************************************************!!           
\REM!!               MAIN PROGRAM START                            !!           
\REM!!*************************************************************!!           
                                                                                
                                                                                
APAUEC01:                                                                       
                                                                                
  ON ERROR GOTO APAUEW01                                                        
                                                                                
  %INCLUDE EAMUSE.J86                                                           
                                                                                
                                                                                
!AIR74440                  !-------------------------------------------!        
  MM.SCRN.INPUT$(1) = "0"  ! THIS INDICATES THAT THE PROCEDURE HAS NOT !        
                           ! BEEN IMPLEMENTED. WHEN YOU IMPLEMENT THE  !        
                           ! PROCEDURE SET MM.SCRN.INPUT$(1) = "1"     !        
!EIR74440                  !-------------------------------------------!        
                                                                                
                                                                                
\REM!!*************************************************************!!           
\REM!!        START THE MAIN CODE UNDER THIS HEADER                !!           
\REM!!*************************************************************!!           
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
\REM!!*************************************************************!!           
\REM!!        FINISH THE MAIN CODE BEFORE THIS HEADER              !!           
\REM!!*************************************************************!!           
                                                                                
       CALL CHAINMOD   ! this will exit from the user procedure                 
                       ! and return to the personalization menu                 
                                                                                
\REM!!*************************************************************!!           
\REM!!            RUN TIME ERROR HANDLING                          !!           
\REM!!*************************************************************!!           
                                                                                
APAUEW01:                                                                       
         LPRINTER :PRINT "EAMAPAUL.286 RUN TIME ERROR" : CONSOLE                
         LPRINTER :PRINT "SESSION NUMBER => ";ERRF%    : CONSOLE                
         LPRINTER :PRINT "ERROR CODE     => ";ERR      : CONSOLE                
         LPRINTER :PRINT "ERRN           => ";ERRN     : CONSOLE                
                                                                                
\REM!!*************************************************************!!           
\REM!!     PLACE ANY CODE FOR HANDLING ERRORS UNDER THIS HEADER    !!           
\REM!!*************************************************************!!           
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
END                                                                             
