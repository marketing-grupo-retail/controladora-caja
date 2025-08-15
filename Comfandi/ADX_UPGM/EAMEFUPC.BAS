\/* TIME STAMP BLOCK **********************************************             
\** END OF TIME STAMP BLOCK **************************************/             
\/*                                                              */             
\/*      COPYRIGHT:                                              */             
\/*      5696-536 THIS MODULE IS "RESTRICTED MATERIALS OF IBM"   */             
\/*      (C) COPYRIGHT IBM CORP 1986, 1993 ALL RIGHTS RESERVED   */             
\/*      LICENSED MATERIALS - PROPERTY OF IBM REFER TO COPYRIGHT */             
\/*      INSTRUCTIONS FORM NUMBER G120-2083                      */             
\/*                                                              */             
\/*      STATUS: RELEASE 2 LEVEL 0                               */             
\/*                                                              */             
\/*                                                              */             
\/*      PERSON RESPONSIBLE = PHIL MAY                           */             
\/*                                                              */             
\/*      FUNCTION: EAMEFUPC                                      */             
\/*                                                              */             
\/*                THIS MODULE CONTAINS THE EXTERNAL FUNCTIONS   */             
\/*                DEFINING THE USER EXITS FOR STEAMBOAT EFT.    */             
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
\/*         MODULE SIZE: USER EXITS                              */             
\/*                                                              */             
\/*         ATTRIBUTES: REENTRANT                                */             
\/*                                                              */             
\/*      ENTRY POINT: EFUPEC01                                   */             
\/*                                                              */             
\/*         PURPOSE: USER EXIT CALLED AFTER INITIALIZATION       */             
\/*                                                              */             
\/*                ***** RETURN CONTROL *****                    */             
\/*                                                              */             
\/*         LINKAGE: WHEN ALL INITIALIZATION IS COMPLETE THIS    */             
\/*                  ROUTINE ALLOWS THE USER TO ADD FUNCTION.    */             
\/*                                                              */             
\/*      ENTRY POINT: EFUPEC02                                   */             
\/*                                                              */             
\/*         PURPOSE: USER EXIT BEFORE TERMINATION                */             
\/*                                                              */             
\/*                ***** RETURN CONTROL *****                    */             
\/*                                                              */             
\/*         LINKAGE: CALLED BEFORE EFT SHUTS DOWN                */             
\/*                                                              */             
\/*      ENTRY POINT: EFUPEC03                                   */             
\/*                                                              */             
\/*         PURPOSE: USER EXIT WHEN OUTBOUND MESSAGE RECEIVED    */             
\/*                                                              */             
\/*                ***** RETURN CONTROL *****                    */             
\/*                                                              */             
\/*         LINKAGE: CALLED AS SOON AS EFTS DETECTS AN OUTBOUND  */             
\/*                  MESSAGE TO BE PROCESSED                     */             
\/*                                                              */             
\/*      ENTRY POINT: EFUPEC04                                   */             
\/*                                                              */             
\/*         PURPOSE: USER EXIT WHEN INBOUND MESSAGE RECEIVED     */             
\/*                                                              */             
\/*                ***** RETURN CONTROL *****                    */             
\/*                                                              */             
\/*         LINKAGE: CALLED AS SOON AS EFTS DETECTS AN INBOUND   */             
\/*                  MESSAGE TO BE PROCESSED                     */             
\/*                                                              */             
\/*      ENTRY POINT: EFUPEC05                                   */             
\/*                                                              */             
\/*         PURPOSE: USER EXIT CALLED AFTER AN ERROR HAS BEEN    */             
\/*                  DETECTED                                    */             
\/*                                                              */             
\/*                ***** RETURN CONTROL *****                    */             
\/*                                                              */             
\/*         LINKAGE: CALLED WHEN EFTS HAS DETERMINED THAT A      */             
\/*                  ERROR IS NOT RECOVERABLE                    */             
\/*                                                              */             
\/*      ENTRY POINT: EFUPEC06                                   */             
\/*                                                              */             
\/*         PURPOSE: USER EXIT AFTER MESSAGE PARSING             */             
\/*                                                              */             
\/*                ***** RETURN CONTROL *****                    */             
\/*                                                              */             
\/*         LINKAGE: CALLED WHEN ALL INDIVIDUAL FIELDS HAVE BEEN */             
\/*                  BROKEN OUT OF IN/OUTBOUND MESSAGES          */             
\/*                                                              */             
\/*      ENTRY POINT: EFUPEC07                                   */             
\/*                                                              */             
\/*         PURPOSE: USER EXIT AFTER READ OF SAVE FILE           */             
\/*                                                              */             
\/*                ***** RETURN CONTROL *****                    */             
\/*                                                              */             
\/*         LINKAGE: CALLED AFTER SAVE FILE HAS BEEN READ        */             
\/*                                                              */             
\/*      ENTRY POINT: EFUPEC08                                   */             
\/*                                                              */             
\/*         PURPOSE: USER EXIT BEFORE WRITE OF SAVE FILE         */             
\/*                                                              */             
\/*                ***** RETURN CONTROL *****                    */             
\/*                                                              */             
\/*         LINKAGE: CALLED BEFORE SAVE FILE IS WRITTEN          */             
\/*                                                              */             
\/*      ENTRY POINT: EFUPEC09                                   */             
\/*                                                              */             
\/*         PURPOSE: USER EXIT CALLED DURING ERROR HANDLING      */             
\/*                                                              */             
\/*                ***** RETURN CONTROL *****                    */             
\/*                                                              */             
\/*         LINKAGE: CALLED WHEN THE ERROR IS NOT TO BE HANDLED  */             
\/*                  BY EFTS TO ALLOW THE USER TO HANDLE IT      */             
\/*                                                              */             
\/*      ENTRY POINT: EFUPEC10                                   */             
\/*                                                              */             
\/*         PURPOSE: USER EXIT CALLED AFTER AN OUTBOUND MESSAGE  */             
\/*                  HAS BEEN PARSED                             */             
\/*                                                              */             
\/*                ***** RETURN CONTROL *****                    */             
\/*                                                              */             
\/*         LINKAGE: CALLED IF SELECTED BY USER DURING PARSING   */             
\/*                                                              */             
\/*      ENTRY POINT: EFUPEC11                                   */             
\/*                                                              */             
\/*         PURPOSE: USER EXIT CALLED AFTER OUTBOUND MESSAGE     */             
\/*                  PARSED                                      */             
\/*                                                              */             
\/*                ***** RETURN CONTROL *****                    */             
\/*                                                              */             
\/*         LINKAGE: CALLED IF SELECTED BY USER DURING PARSING   */             
\/*                                                              */             
\/*      ENTRY POINT: EFUPEC12                                   */             
\/*                                                              */             
\/*         PURPOSE: USER EXIT DURING HOST REVERSAL              */             
\/*                                                              */             
\/*                ***** RETURN CONTROL *****                    */             
\/*                                                              */             
\/*         LINKAGE: CALLED WHEN HOST REVERSAL IS DETECTED       */             
\/*                                                              */             
\/*      ENTRY POINT: EFUPEC13                                   */             
\/*                                                              */             
\/*         PURPOSE: USER EXIT DURING TERMINAL REVERSAL          */             
\/*                                                              */             
\/*                ***** RETURN CONTROL *****                    */             
\/*                                                              */             
\/*         LINKAGE: CALLED DURING TERMINAL REVERSAL PROCESSING  */             
\/*                                                              */             
\/*      ENTRY POINT: EFUPEC14                                   */             
\/*                                                              */             
\/*         PURPOSE: USER EXIT DURING MESSAGE RECOVERY           */             
\/*                                                              */             
\/*                ***** RETURN CONTROL *****                    */             
\/*                                                              */             
\/*         LINKAGE: CALLED WHEN A HOST REQUEST IS BEING         */             
\/*                  RECOVERED                                   */             
\/*                                                              */             
\/*      ENTRY POINT: EFUPEC15                                   */             
\/*                                                              */             
\/*         PURPOSE: USER EXIT WHEN PROCESSING RESPONSE          */             
\/*                                                              */             
\/*                ***** RETURN CONTROL *****                    */             
\/*                                                              */             
\/*         LINKAGE: CALLED JUST BEFORE UPDATING SAVE FILE WITH  */             
\/*                  TOTALS AFTER A RESPONSE HAS BEEN RECEIVED   */             
\/*                                                              */             
\/*      ENTRY POINT: EFUPEC16                                   */             
\/*                                                              */             
\/*         PURPOSE: USER EXIT AT AUTHORIZATION TIMEOUT          */             
\/*                                                              */             
\/*                ***** RETURN CONTROL *****                    */             
\/*                                                              */             
\/*         LINKAGE: CALLED WHEN THE TERMINAL HAS DETECTED A     */             
\/*                  TIMEOUT FOR THE AUTHORIZATION REQUEST       */             
\/*                                                              */             
\/*      ENTRY POINT: EFUPEC17                                   */             
\/*                                                              */             
\/*         PURPOSE: USER EXIT AFTER READ OF TERMINAL STATUS     */             
\/*                                                              */             
\/*                ***** RETURN CONTROL *****                    */             
\/*                                                              */             
\/*         LINKAGE: CALLED AFTER READING EFT STATUS FILE        */             
\/*                                                              */             
\/*      ENTRY POINT: EFUPEC18                                   */             
\/*                                                              */             
\/*         PURPOSE: USER EXIT BEFORE WRITING TERMINAL STATUS    */             
\/*                                                              */             
\/*                ***** RETURN CONTROL *****                    */             
\/*                                                              */             
\/*         LINKAGE: CALLED BEFORE EFT STATUS FILE IS UPDATED    */             
\/*                                                              */             
\/*      ENTRY POINT: EFUPEC21                                   */             
\/*                                                              */             
\/*         PURPOSE: USER EXIT CALLED AT SNA INITIALIZATION      */             
\/*                                                              */             
\/*                ***** RETURN CONTROL *****                    */             
\/*                                                              */             
\/*         LINKAGE: CALLED WHENEVER SESSION IS OPENED           */             
\/*                                                              */             
\/*      ENTRY POINT: EFUPEC22                                   */             
\/*                                                              */             
\/*         PURPOSE: USER EXIT CALLED AT SNA TERMINATION         */             
\/*                                                              */             
\/*                ***** RETURN CONTROL *****                    */             
\/*                                                              */             
\/*         LINKAGE: CALLED WHEN SESSION IS CLOSED               */             
\/*                                                              */             
\/*      ENTRY POINT: EFUPEC23                                   */             
\/*                                                              */             
\/*         PURPOSE: USER EXIT AFTER SNA MESSAGE RECEIVED        */             
\/*                                                              */             
\/*                ***** RETURN CONTROL *****                    */             
\/*                                                              */             
\/*         LINKAGE: CALLED WHEN ANY SNA MESSAGE RECEIVED        */             
\/*                                                              */             
\/*      ENTRY POINT: EFUPEC24                                   */             
\/*                                                              */             
\/*         PURPOSE: USER EXIT CALLED BEFORE SNA SEND            */             
\/*                                                              */             
\/*                ***** RETURN CONTROL *****                    */             
\/*                                                              */             
\/*         LINKAGE: CALLED BEFORE ANY SNA MESSAGE IS SENT       */             
\/*                                                              */             
\/*      ENTRY POINT: EFUPEC25                                   */             
\/*                                                              */             
\/*         PURPOSE: USER EXIT WHEN SNA ERROR DETECTED           */             
\/*                                                              */             
\/*                ***** RETURN CONTROL *****                    */             
\/*                                                              */             
\/*         LINKAGE: CALLED AT ANY SNA ERROR                     */             
\/*                                                              */             
\/*      ENTRY POINT: EFUPEC31                                   */             
\/*                                                              */             
\/*         PURPOSE: USER EXIT CALLED AT DATA ENTRY PROCESSING.  */             
\/*                                                              */             
\/*                ***** RETURN CONTROL *****                    */             
\/*                                                              */             
\/*         LINKAGE: CALLED FROM TS11EC01.                       */             
\/*                                                              */             
\/*      ENTRY POINT: EFUPEC32                                   */             
\/*                                                              */             
\/*         PURPOSE: USER EXIT CALLED AT TENDER VALIDATION.      */             
\/*                                                              */             
\/*                ***** RETURN CONTROL *****                    */             
\/*                                                              */             
\/*         LINKAGE: CALLED FROM TSCSEC01.                       */             
\/*                                                              */             
\/*      ENTRY POINT: EFUPEC33                                   */             
\/*                                                              */             
\/*         PURPOSE: USER EXIT CALLED AT RECOVERY OF TOTALS      */             
\/*                  PROCESSING.                                 */             
\/*                                                              */             
\/*                ***** RETURN CONTROL *****                    */             
\/*                                                              */             
\/*         LINKAGE: CALLED FROM ???.                            */             
\/*                                                              */             
\/*      ENTRY POINT: EFUPEC34                                   */             
\/*                                                              */             
\/*         PURPOSE: USER EXIT CALLED AT STANDALONE ENTRY.       */             
\/*                                                              */             
\/*                ***** RETURN CONTROL *****                    */             
\/*                                                              */             
\/*         LINKAGE: CALLED FROM TS11EC01.                       */             
\/*                                                              */             
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
\/*         ROUTINES:   NONE                                     */             
\/*                                                              */             
\/*         DATA AREAS:                                          */             
\/*                                                              */             
\/*           MODIFIED:  NONE                                    */             
\/*                                                              */             
\/*           REFERENCED: NONE                                   */             
\/*                                                              */             
\/*         CONTROL BLOCKS: NONE                                 */             
\/*                                                              */             
\/*         MACROS: NONE                                         */             
\/*                                                              */             
\/*         TABLES: NONE                                         */             
\/*                                                              */             
\/*      CHANGE ACTIVITY: LEVEL 0                                */             
\/*                                                              */             
\/*                                                              */             
\/* END-OF-SPECIFICATIONS ****************************************/             
                                                                                
!AIR68986                                                                       
                                                                                
\ REM!!**************** INCLUDE FILES  ********************************         
                                                                                
%INCLUDE EAMEFWKG.J86        ! working storage                                  
%INCLUDE EAMSOPTS.J86        ! store options                                    
%INCLUDE EAMEFUVA.J86        ! user exit global variables                       
                                                                                
!AIR80517 - Commented next include, moved it below to allow user exits          
!           to access SMA Assembler routines                                    
                                                                                
!%INCLUDE EAMEFUSU.J86        ! user exit common subroutines                    
%INCLUDE EAMASMCT.J86        ! assembler routines                               
%INCLUDE EAMADXRT.J86        ! system services interface     IR68987            
%INCLUDE EAMEFUSU.J86        ! user exit common subroutines                     
                                                                                
!EIR80517                                                                       
                                                                                
\ REM!!****************************************************************         
\ REM!!* STEAMBOAT EFTS USER EXITS - AVAILABLE I/O SESSION NUMBERS    *         
\ REM!!****************************************************************         
\ REM!!*                                                              *         
\ REM!!* The steamboat EFT application uses session numbers as        *         
\ REM!!* follows:                                                     *         
\ REM!!*                                                              *         
\ REM!!*  4 - SNA link                                                *         
\ REM!!*  5 - SNA session                                             *         
\ REM!!*  6 - EFT save file                                           *         
\ REM!!*  7 - EFT status file                                         *         
\ REM!!* 42 - Terminal communications pipe                            *         
\ REM!!*                                                              *         
\ REM!!****************************************************************         
                                                                                
\ REM!!****************************************************************         
\ REM!!  EFUPEC01         : CALLED AFTER EFTS INITIALIZATION                    
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
\ REM!!                                                                         
\ REM!! This exit is provided in order that any user initialization can         
\ REM!! be performed. At this point store options have been read,               
\ REM!! EAMEXTVF and EAMEXTSV have been opened and the terminal and SNA         
\ REM!! communications have been initialized.                                   
\ REM!!                                                                         
\ REM!! The routine will be called whenever EFTS performs initialization,       
\ REM!! including restarting after critical errors.                             
\ REM!!                                                                         
\ REM!!****************************************************************         
                                                                                
%INCLUDE EAMEFU01.J86                                                           
                                                                                
\ REM!!****************************************************************         
\ REM!!  EFUPEC02         : CALLED WHEN EFTS HAS EXITED THE MAIN                
\ REM!!                     PROCESSING WITH AN UNRECOVERABLE ERROR              
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
\ REM!!                                                                         
\ REM!! This routine is called before EFTS performs any termination             
\ REM!! processing. EFTS will only reach this point if the error routine        
\ REM!! has indicated that closedown is required. The user routine              
\ REM!! should close any sessions that were open and perform any                
\ REM!! cleanup. Note that EFTS may attempt to recover with a RESUME            
\ REM!! LABEL andbthis will reexecute the user initialization routine           
\ REM!! EFUPEC01.                                                               
\ REM!!                                                                         
\ REM!!****************************************************************         
                                                                                
%INCLUDE EAMEFU02.J86                                                           
                                                                                
\ REM!!****************************************************************         
\ REM!!  EFUPEC03         : AFTER EFTS HAS RECEIVED AN OUTBOUND MESSAGE         
\ REM!!                                                                         
\ REM!!  INPUT PARMS.     : NONE                                                
\ REM!!                                                                         
\ REM!!  OUTPUT PARMS.    : NONE                                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  REFERENCE ONLY   : NONE                                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  UPDATE           :                                                     
\ REM!!                                                                         
\ REM!!  EF.OUT.MSG$      - Outbound message                                    
\ REM!!  EF.OUT.MSGLEN    - Outbound message length                             
\ REM!!  EF.OUT.PROCESS   - Indicates whether the message is to be              
\ REM!!                     processed. Set to 0 if message should be            
\ REM!!                     discarded.                                          
\ REM!!****************************************************************         
\ REM!!                                                                         
\ REM!! This routine is called as soon as an outbound message has been          
\ REM!! received. It is called before the message has been parsed. It           
\ REM!! is here that any host protocol translation should be performed,         
\ REM!! since the parsing routine assumes VISA II format. The message           
\ REM!! content and length can be changed to match VISA II, or the              
\ REM!! message can be discarded.                                               
\ REM!!                                                                         
\ REM!!****************************************************************         
                                                                                
%INCLUDE EAMEFU03.J86                                                           
                                                                                
\ REM!!****************************************************************         
\ REM!!  EFUPEC04         : CALLED BEFORE PROCESSING AN INBOUND MESSAGE         
\ REM!!                                                                         
\ REM!!  INPUT PARMS.     : NONE                                                
\ REM!!                                                                         
\ REM!!  OUTPUT PARMS.    : NONE                                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  REFERENCE ONLY   :                                                     
\ REM!!                                                                         
\ REM!!  EF.IN.MSG$       - Inbound message                                     
\ REM!!  EF.IN.MSGLEN     - Inbound message length                              
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  UPDATE           : NONE                                                
\ REM!!****************************************************************         
\ REM!!                                                                         
\ REM!! This routine is called when a message is received from the              
\ REM!! terminal. The message is about to be passed to the parsing              
\ REM!! routine.                                                                
\ REM!!                                                                         
\ REM!!****************************************************************         
                                                                                
%INCLUDE EAMEFU04.J86                                                           
                                                                                
\ REM!!****************************************************************         
\ REM!!  EFUPEC05         : CALLED AFTER EFTS FAILURE PROCESSING                
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
\ REM!!                                                                         
\ REM!! At this point EFTS has just finished sending OFFLINE messages           
\ REM!! to all the terminals. EFT has therefore been disabled until             
\ REM!! EFTS restarts.                                                          
\ REM!!                                                                         
\ REM!!****************************************************************         
                                                                                
%INCLUDE EAMEFU05.J86                                                           
                                                                                
\ REM!!****************************************************************         
\ REM!!  EFUPEC06         : CALLED AFTER MESSAGE PARSING                        
\ REM!!                                                                         
\ REM!!  INPUT PARMS.     : NONE                                                
\ REM!!                                                                         
\ REM!!  OUTPUT PARMS.    : NONE                                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  REFERENCE ONLY   : NONE                                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  UPDATE           :                                                     
\ REM!!                                                                         
\ REM!!  All EF.VS. variables (See EAMEFWKG)                                    
\ REM!!  EF.IN.TYPE       - number of message processing routine                
\ REM!!  EF.OUT.TYPE      - number of message processing routine                
\ REM!!****************************************************************         
\ REM!!                                                                         
\ REM!! At this point the inbound/outbound message has been broken down.        
\ REM!! The EF.VS. variables have been extracted from the message for           
\ REM!! processing as required.                                                 
\ REM!!                                                                         
\ REM!! If the message was not recognized by the parsing routine then           
\ REM!! the variable EF.IN/OUT.TYPE will be set to 9. This should be            
\ REM!! set to 10 if the users own processing routine is to be invoked.         
\ REM!!                                                                         
\ REM!!****************************************************************         
                                                                                
%INCLUDE EAMEFU06.J86                                                           
                                                                                
\ REM!!****************************************************************         
\ REM!!  EFUPEC07         : CALLED AFTER READ OF SAVE FILE                      
\ REM!!                                                                         
\ REM!!  INPUT PARMS.     : NONE                                                
\ REM!!                                                                         
\ REM!!  OUTPUT PARMS.    : NONE                                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  REFERENCE ONLY   : NONE                                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  UPDATE           :                                                     
\ REM!!                                                                         
\ REM!!  SV.CNTOK         - count  of accepted transactions                     
\ REM!!  SV.CNTEX         - count  of rejected transactions                     
\ REM!!  SV.AMTOK         - amount of accepted transactions                     
\ REM!!  SV.AMTEX         - amount of rejected transactions                     
\ REM!!  SV.USER$         - user area in save file                              
\ REM!!****************************************************************         
\ REM!!                                                                         
\ REM!! The save file has been read, and a null record created if one           
\ REM!! did not already exist. The audit totals for the device are in           
\ REM!! SV.CNTOK(6), SV.CNTEX(6), SV.AMTOK(6), SV.AMTEX(6). Currently           
\ REM!! the first three elements of these arrays are used and correspond        
\ REM!! to the left hand three account buttons on the MSR/PIN numbering         
\ REM!! from the top.                                                           
\ REM!!                                                                         
\ REM!! The transaction status of the last EFT transaction is saved in          
\ REM!! this file, along with a copy of the last authorization request.         
\ REM!! This data is required for recovery and should not be modified.          
\ REM!!                                                                         
\ REM!!****************************************************************         
                                                                                
%INCLUDE EAMEFU07.J86                                                           
                                                                                
\ REM!!****************************************************************         
\ REM!!  EFUPEC08         : CALLED BEFORE WRITE OF SAVE FILE                    
\ REM!!                                                                         
\ REM!!  INPUT PARMS.     : NONE                                                
\ REM!!                                                                         
\ REM!!  OUTPUT PARMS.    : NONE                                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  REFERENCE ONLY   : NONE                                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  UPDATE           :                                                     
\ REM!!                                                                         
\ REM!!  SV.CNTOK         - COUNT OF ACCEPTED TRANSACTIONS        !AIR68987     
\ REM!!  SV.CNTEX         - count  of rejected transactions                     
\ REM!!  SV.AMTOK         - amount of accepted transactions                     
\ REM!!  SV.AMTEX         - amount of rejected transactions                     
\ REM!!  SV.USER$         - user area in save file                              
\ REM!!****************************************************************         
\ REM!!                                                                         
\ REM!! The save file is about to be written                                    
\ REM!!                                                                         
\ REM!!****************************************************************         
                                                                                
%INCLUDE EAMEFU08.J86                                                           
                                                                                
\ REM!!****************************************************************         
\ REM!!  EFUPEC09         : CALLED DURING ERROR PROCESSING                      
\ REM!!                                                                         
\ REM!!  INPUT PARMS.     : NONE                                                
\ REM!!                                                                         
\ REM!!  OUTPUT PARMS.    : NONE                                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  REFERENCE ONLY   :                                                     
\ REM!!                                                                         
\ REM!!  EF.CODE$         - error code                                          
\ REM!!  EF.ERRN          - 4 byte error code                                   
\ REM!!  EF.ERRF          - error session number                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  UPDATE           :                                                     
\ REM!!                                                                         
\ REM!!  ERROR.TYPE       - indicates what error action should be taken         
\ REM!!                                                                         
\ REM!!****************************************************************         
\ REM!!                                                                         
\????????????? EF.CODE$,EF.ERRN,EF.ERRF,ERROR.TYPE need externalizing           
\ REM!!                                                                         
\ REM!! The user exit is called when EFTS has determined that the error         
\ REM!! cannot be handled. If the user exit is able to handle the error         
\ REM!! then the variable ERROR.TYPE should be set to 0   or 7      at          
\ REM!! exit in order to cause the appropriate RESUME. 8        will            
\ REM!! cause EFTS to perform a RESUME LABEL to the top of the routine.         
\ REM!! 9      will cause termination.                                          
\ REM!!                                                                         
\ REM!!****************************************************************         
                                                                                
%INCLUDE EAMEFU09.J86                                                           
                                                                                
\ REM!!****************************************************************         
\ REM!!  EFUPEC10         : CALLED AFTER AN OUTBOUND MESSAGE HAS BEEN           
\ REM!!                     SELECTED BY THE USER PARSING EXIT                   
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
\ REM!!                                                                         
\ REM!! The message under consideration was not recognized by SEFT, but         
\ REM!! the user exit in the parsing routine recognized it and routed it        
\ REM!! here. Do with it what you will. The message will be discarded           
\ REM!! when you are finished.                                                  
\ REM!!                                                                         
\ REM!!****************************************************************         
                                                                                
%INCLUDE EAMEFU10.J86                                                           
                                                                                
\ REM!!****************************************************************         
\ REM!!  EFUPEC11         : CALLED AFTER AN INBOUND MESSAGE HAS BEEN            
\ REM!!                     SELECTED BY THE USER PARSING EXIT                   
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
\ REM!!                                                                         
\ REM!! The message under consideration was not recognized by SEFT, but         
\ REM!! the user exit in the parsing routine recognized it and routed it        
\ REM!! here. Do with it what you will. The message will be discarded           
\ REM!! when you are finished.                                                  
\ REM!!                                                                         
\ REM!!****************************************************************         
                                                                                
%INCLUDE EAMEFU11.J86                                                           
                                                                                
\ REM!!****************************************************************         
\ REM!!  EFUPEC12         : CALLED WHEN PROCESSING A HOST REVERSAL              
\ REM!!                                                                         
\ REM!!  INPUT PARMS.     : NONE                                                
\ REM!!                                                                         
\ REM!!  OUTPUT PARMS.    : NONE                                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  REFERENCE ONLY   : NONE                                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  UPDATE           :                                                     
\ REM!!                                                                         
\ REM!!  EF.IN.MSG$       - reversal message                                    
\ REM!!  EF.IN.MSGLEN     - reversal message length                             
\ REM!!                                                                         
\ REM!!****************************************************************         
\ REM!!                                                                         
\ REM!! The reversal message is about to be sent. The void indicator has        
\ REM!! been set in the message. If you wish to put the reversal message        
\ REM!! in a different form then do it here. Note that a reversal               
\ REM!! response must be received by the system so do not attempt               
\ REM!! anything which may lose the reversal.                                   
\ REM!!                                                                         
\ REM!!****************************************************************         
                                                                                
%INCLUDE EAMEFU12.J86                                                           
                                                                                
\ REM!!****************************************************************         
\ REM!!  EFUPEC13         : CALLED BEFORE SENDING TERMINAL REVERSAL             
\ REM!!                                                                         
\ REM!!  INPUT PARMS.     : NONE                                                
\ REM!!                                                                         
\ REM!!  OUTPUT PARMS.    : NONE                                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  REFERENCE ONLY   :                                                     
\ REM!!                                                                         
\ REM!!  EF.OUT.MSG$      - reversal message                                    
\ REM!!  EF.OUT.MSGLEN    - reversal message length                             
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  UPDATE           : NONE                                                
\ REM!!****************************************************************         
\ REM!!                                                                         
\ REM!! Invoked just before the terminal reversal is sent.                      
\ REM!!                                                                         
\ REM!!****************************************************************         
                                                                                
%INCLUDE EAMEFU13.J86                                                           
                                                                                
\ REM!!****************************************************************         
\ REM!!  EFUPEC14         : CALLED DURING MESSAGE RECOVERY                      
\ REM!!                                                                         
\ REM!!  INPUT PARMS.     : NONE                                                
\ REM!!                                                                         
\ REM!!  OUTPUT PARMS.    : NONE                                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  REFERENCE ONLY   :                                                     
\ REM!!                                                                         
\ REM!!  SV.STATUS        - status file transaction status                      
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  UPDATE           :                                                     
\ REM!!                                                                         
\ REM!!  EF.IN.MSG$       - recovered message                                   
\ REM!!  EF.IN.MSGLEN     - recovered message length                            
\ REM!!****************************************************************         
\ REM!!                                                                         
\ REM!! Message recovery is invoked when the EFTS host link comes online.       
\ REM!! It involves scanning the save file and replaying all saved              
\ REM!! transactions. This exit is called before each recovered                 
\ REM!! message is sent. Setting the message length to zero will cause          
\ REM!! the message to be discarded. Be aware that the system relies on         
\ REM!! the sending of these messages to maintain auditability.                 
\ REM!!                                                                         
\ REM!!****************************************************************         
                                                                                
%INCLUDE EAMEFU14.J86                                                           
                                                                                
\ REM!!****************************************************************         
\ REM!!  EFUPEC15         : TOTALS UPDATE WHEN RESPONSE RECEIVED                
\ REM!!                                                                         
\ REM!!  INPUT PARMS.     : NONE                                                
\ REM!!                                                                         
\ REM!!  OUTPUT PARMS.    : NONE                                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  REFERENCE ONLY   : NONE                                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  UPDATE           : SV. audit variables                                 
\ REM!!****************************************************************         
\ REM!!                                                                         
\ REM!! This exit is called whenever the SV. audit variables are updated,       
\ REM!! either for authorization response, VOID response or REVERSAL            
\ REM!! response.                                                               
\ REM!!                                                                         
\ REM!!****************************************************************         
                                                                                
%INCLUDE EAMEFU15.J86                                                           
                                                                                
\ REM!!****************************************************************         
\ REM!!  EFUPEC16         : CALLED AT AUTHORIZATION TIMEOUT                     
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
\ REM!!                                                                         
\ REM!! This routine is called when an authorization timeout message is         
\ REM!! sent from the terminal. The terminal will not reverse the msg           
\ REM!! until another timeout period has elapsed. This exit could be            
\ REM!! used to log an offline transaction and send an authorization            
\ REM!! response to the terminal.                                               
\ REM!!                                                                         
\ REM!! NOTE:- The host will still have an outstanding authorization            
\ REM!! request. The routine should call SEFTHREV to ensure that the            
\ REM!! pending online transaction will be reversed at some stage.              
\ REM!!                                                                         
\ REM!!****************************************************************         
                                                                                
%INCLUDE EAMEFU16.J86                                                           
                                                                                
\ REM!!****************************************************************         
\ REM!!  EFUPEC17         : CALLED AFTER EFT STATUS FILE HAS BEEN READ          
\ REM!!                                                                         
\ REM!!  INPUT PARMS.     : NONE                                                
\ REM!!                                                                         
\ REM!!  OUTPUT PARMS.    : NONE                                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  REFERENCE ONLY   : NONE                                                
\ REM!!                                                                         
\ REM!!  ST.PROGLOAD$     - program load id                                     
\ REM!!  ST.PARMLOAD$     - parameter load id                                   
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  UPDATE           : NONE                                                
\ REM!!****************************************************************         
\ REM!!                                                                         
\ REM!! The EFT status file is an internal file used to monitor the             
\ REM!! status of the MSR/PIN device and  the software and parameter            
\ REM!! loads held in each device.                                              
\ REM!!                                                                         
\ REM!!****************************************************************         
                                                                                
%INCLUDE EAMEFU17.J86                                                           
                                                                                
\ REM!!****************************************************************         
\ REM!!  EFUPEC18         : CALLED BEFORE WRITING EFT STATUS FILE               
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
\ REM!!                                                                         
\ REM!! See EFUPEC17                                                            
\ REM!!                                                                         
\ REM!!****************************************************************         
                                                                                
%INCLUDE EAMEFU18.J86                                                           
%INCLUDE EAMEFU19.J86                                                           
                                                                                
\ REM!!****************************************************************         
\ REM!!  EFUPEC21         : CALLED AT SNA INITIALIZATION                        
\ REM!!                                                                         
\ REM!!  INPUT PARMS.     : NONE                                                
\ REM!!                                                                         
\ REM!!  OUTPUT PARMS.    : NONE                                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  REFERENCE ONLY   :                                                     
\ REM!!                                                                         
\ REM!!  SN.LNK$          - SNA link name                                       
\ REM!!  SN.SES$          - SNA session name                                    
\ REM!!  EF.RC            - Return code from open of link                       
\ REM!!  EF.COMMAND$      - Command passed by CDOS (Link name, ses name)        
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  UPDATE           : NONE                                                
\ REM!!****************************************************************         
\ REM!!                                                                         
\???????EXTERNALIZE SN.LNK$ AND SN.SES$ FOR EXIT???????                         
\ REM!!                                                                         
\ REM!! This routine is called immediately after the SNA initialization         
\ REM!! has completed. Any user host session initialization could be            
\ REM!! inserted here.                                                          
\ REM!!                                                                         
\ REM!!****************************************************************         
                                                                                
%INCLUDE EAMEFU21.J86                                                           
                                                                                
\ REM!!****************************************************************         
\ REM!!  EFUPEC22         : CALLED AT SNA TERMINATION                           
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
\ REM!!                                                                         
\ REM!! Any user host session termination code could be inserted here.          
\ REM!!                                                                         
\ REM!!****************************************************************         
                                                                                
%INCLUDE EAMEFU22.J86                                                           
                                                                                
\ REM!!****************************************************************         
\ REM!!  EFUPEC23         : CALLED AFTER READING FROM THE HOST LINE             
\ REM!!                                                                         
\ REM!!  INPUT PARMS.     : NONE                                                
\ REM!!                                                                         
\ REM!!  OUTPUT PARMS.    : NONE                                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  REFERENCE ONLY   : NONE                                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  UPDATE           :                                                     
\ REM!!                                                                         
\ REM!!  EF.OUT.MSG$      - Outbound message                                    
\ REM!!  EF.OUT.MSGLEN    - Outbound message length                             
\ REM!!  EF.OUT.PROCESS   - Outbound message process indicator                  
\ REM!!****************************************************************         
\ REM!!                                                                         
\ REM!! Called after all SNA specific processing is complete before             
\ REM!! returning to EFTS for processing of the message. Set                    
\ REM!! EF.OUT.PROCESS to 0 to discard the message.                             
\ REM!!                                                                         
\ REM!!****************************************************************         
                                                                                
%INCLUDE EAMEFU23.J86                                                           
                                                                                
\ REM!!****************************************************************         
\ REM!!  EFUPEC24         : CALLED BEFORE SENDING A MESSAGE                     
\ REM!!                                                                         
\ REM!!  INPUT PARMS.     : NONE                                                
\ REM!!                                                                         
\ REM!!  OUTPUT PARMS.    : NONE                                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  REFERENCE ONLY   : NONE                                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  UPDATE           :                                                     
\ REM!!                                                                         
\ REM!!  EF.IN.MSG$       - Inbound message                                     
\ REM!!  EF.IN.MSGLEN     - Inbound message length                              
\ REM!!****************************************************************         
\ REM!!                                                                         
\ REM!! Called whenever EFTS requests an SNA message to be sent.                
\ REM!!                                                                         
\ REM!!****************************************************************         
                                                                                
%INCLUDE EAMEFU24.J86                                                           
                                                                                
\ REM!!****************************************************************         
\ REM!!  EFUPEC25         : CALLED WHENEVER SNA INTERFACE DETECTS ERROR         
\ REM!!                                                                         
\ REM!!  INPUT PARMS.     : NONE                                                
\ REM!!                                                                         
\ REM!!  OUTPUT PARMS.    : NONE                                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  REFERENCE ONLY   : NONE                                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  UPDATE           : EF.ONLINE                                           
\ REM!!****************************************************************         
\ REM!!                                                                         
\ REM!! EF.ONLINE has just been set to 0   to indicate that SNA failure         
\ REM!! has occurred. EFTS will handle termination processing.                  
\ REM!!                                                                         
\ REM!!****************************************************************         
                                                                                
%INCLUDE EAMEFU25.J86                                                           
                                                                                
\ REM!!****************************************************************         
\ REM!!  EFUPEC31         : CALLED AT MESSAGE SEGMENT INITIALIZATION            
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
\ REM!!                                                                         
\ REM!! At this point the pipe with session number 45     and ID                
\ REM!! "E"      has been created and the terminal services pipe                
\ REM!! has been initialized.                                                   
\ REM!!                                                                         
\ REM!!****************************************************************         
                                                                                
%INCLUDE EAMEFU31.J86                                                           
                                                                                
\ REM!!****************************************************************         
\ REM!!  EFUPEC32         : CALLED AT PIPE TERMINATION                          
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
\ REM!!                                                                         
\ REM!! Called immmediately before pipe session number 45     is closed.        
\ REM!!                                                                         
\ REM!!****************************************************************         
                                                                                
%INCLUDE EAMEFU32.J86                                                           
                                                                                
\ REM!!****************************************************************         
\ REM!!  EFUPEC33         : CALLED AFTER RECEIVE OF MESSAGE FROM TERM           
\ REM!!                                                                         
\ REM!!  INPUT PARMS.     : NONE                                                
\ REM!!                                                                         
\ REM!!  OUTPUT PARMS.    : NONE                                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  REFERENCE ONLY   : NONE                                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  UPDATE           :                                                     
\ REM!!                                                                         
\ REM!!  EF.IN.PROCESS    - Message process indicator                           
\ REM!!  EF.IN.MSG$       - Message                                             
\ REM!!  EF.IN.MSGLEN     - Message length                                      
\ REM!!****************************************************************         
\ REM!!                                                                         
\ REM!! When a message has been received from a terminal this routine           
\ REM!! is called to allow processing before the message is passed to           
\ REM!! SEFT. The message can be discarded by setting EF.IN.PROCESS             
\ REM!! to 0                                                                    
\ REM!!                                                                         
\ REM!!****************************************************************         
                                                                                
%INCLUDE EAMEFU33.J86                                                           
                                                                                
\ REM!!****************************************************************         
\ REM!!  EFUPEC34         : CALLED BEFORE SENDING A MESSAGE TO TERMINAL         
\ REM!!                                                                         
\ REM!!  INPUT PARMS.     : NONE                                                
\ REM!!                                                                         
\ REM!!  OUTPUT PARMS.    : NONE                                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  REFERENCE ONLY   : NONE                                                
\ REM!!                                                                         
\ REM!!  GLOBALS FOR                                                            
\ REM!!  UPDATE           :                                                     
\ REM!!                                                                         
\ REM!!  EF.OUT.MSG$      - Message                                             
\ REM!!  EF.OUT.MSGLEN    - Message length                                      
\ REM!!****************************************************************         
\ REM!!                                                                         
\ REM!! The message is about to be segmented and sent to the terminal.          
\ REM!! It can be modified, or it can be discarded by setting                   
\ REM!! EF.OUT.MSGLEN to 0                                                      
\ REM!!                                                                         
\ REM!!****************************************************************         
                                                                                
%INCLUDE EAMEFU34.J86                                                           
!EIR68986                                                                       
                                                                                
