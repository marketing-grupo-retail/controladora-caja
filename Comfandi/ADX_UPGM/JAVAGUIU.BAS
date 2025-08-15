!ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
!
! Module Name:      javaguiu.bas
!
!
! Descriptive Name: CBASIC user exit shell for Java GUI function
!
!
! Copyright:
!            IBM Supermarket Application Value Pack 2001 for 4690 OS
!            5639-M34 THIS MODULE IS "RESTRICTED MATERIALS OF IBM"
!            (C) COPYRIGHT IBM CORP 2000 ALL RIGHTS RESERVED
!            LICENSED MATERIALS - PROPERTY OF IBM REFER TO COPYRIGHT
!            INSTRUCTIONS FORM NUMBER G120-2083
!
! Status: Release 1 Level 0
!
! Person Responsible = Robert S. Hoblit, IBM
!
! Function:         Java GUI User exit shell
!
! Change Activity:
!
! IR44477  Add missing copywright include.
!          RSH IBM 14Dec00
!
! IR52982  Modified java GUI hooks for use with VP, SureVision and AEF.
!          KHG IBM 30Jan2004
!
! END-OF-SPECIFICATIONS
!ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ

%ENVIRON T

!ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
! Variable declarations
!ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
%INCLUDE JAVAGUIV.J86                            ! Global Variable include

%INCLUDE JGUIUVA.J86                             ! User variables

%INCLUDE EAMXXCPY.J86                            ! Copywright Include IR44477

!ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
! External function prototypes
!ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
%INCLUDE JAVAGUIC.J86                            ! Java Gui Externals

%INCLUDE JGUIUSU.J86                             ! user subroutines

!ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
! Public routines
!ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ

!ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
! function javaGuiInitUserExit
!
! Parms: startEvent - lowest value for user exit events)
!
!ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
FUNCTION javaGuiInitUserExit(startEvent) PUBLIC  ! javaGuiInitUserExit
  INTEGER*2 startEvent                           ! user start event number

  %INCLUDE JGUIU01.J86                           ! Allow user to add events

END FUNCTION                                     ! javaGuiInitUserExit

!ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
! subroutine javaGuiUserExit
!
! Parms: eventNum  - number of the event (application or user)
!        eventData - data associated with the event
!
!ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
SUB javaGuiUserExit(eventNum, eventData) PUBLIC RECURSIVE ! javaGuiUserExit
  INTEGER*2 eventnum                             ! event number
  STRING    eventData                            ! return data

  %INCLUDE JGUIU02.J86                           ! allow user to process

END SUB                                          ! javaGuiUserExit

!ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
! sub javaGuiUserException
!
! parms: event       The event number fired
!        data$       The data buffer passed
!        execption$  The java exception data buffer returned from the
!                    CBASIC InvokeMethod call (null indicates success).
!
! Comments:  This subroutine is provided so that the user can
!            have an opportunity to become aware of, and possibly handle
!            exceptions excountered from the invokeMethod call.
!            This routine should be used with extreme caution as the
!            call mechanism is recursive -> generating multiple errors
!            without ever returning will result in a stack overflow.
!            So, the advice is to NOT use this routine to call the javaEvent
!            function!
!
!
SUB javaGuiUserException(eventNum,         \
                         eventData,        \
                         eventException)   \
                         PUBLIC RECURSIVE  ! User exception handler
  INTEGER*2                                \
             eventNum                      ! event number
  STRING                                   \
             eventData,                    \ data originally passed
             eventException                ! exception

  %INCLUDE JGUIU03.J86                     ! allow user to process

END SUB                                    ! User exception handler

!ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
! sub javaGuiUserJavaKey
!
! parms: keyStem     The key stem to interrogate
!        validFlag   The valid flag
!
! Comments:  On entry, the validFlag is set to -1 if the passed
!            key stem value matches a system defined java key.
!            When the validFlag is set to zero, the key is not
!            a valid system defined java key.
!
!            The user may use this user exit to facilitate supporting
!            additional java keys.
!
!
SUB javaGuiUserJavaKey(keyStem,           \
                       validFlag) PUBLIC  ! User java key validation

  INTEGER*2                               \
             keyStem,                     \ passed key stem
             validFlag                    ! validitiy flag

  %INCLUDE JGUIU04.J86                    ! allow user to process

END SUB                                   ! User java key validation

!AIR52982
!ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
! subroutine javaAEFUserExit
!
! Parms: eventNum  - number of the event (application or user)
!        eventData - data associated with the event
!
!ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
SUB javaAEFUserExit(eventNum, eventData$) PUBLIC RECURSIVE ! javaGuiUserExit
  INTEGER*2 eventnum                             ! event number
  STRING    eventData$                           ! return data

  %INCLUDE JGUIU05.J86                           ! allow user to process

END SUB                                          ! javaGuiUserExit
!EIR52982
