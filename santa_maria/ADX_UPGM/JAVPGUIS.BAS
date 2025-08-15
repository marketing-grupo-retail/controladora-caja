!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
!
! JAVAGUIS.BAS
!
!
! Descriptive Name: Application specific code for the java gui application
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
! Person Responsible = Kenneth H. Geiger, IBM
!
! Function:
!                   This module contains the public routine stubs specific to the
!                   Supermarket Application to support the Java GUI Customer
!                   Display.
!
! Change Activity:
!
!
! END-OF-SPECIFICATIONS
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
%ENVIRON T


%INCLUDE JAVAGUIU.J86

!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
!
!   function javaVPEvent
!
!   parameters:    event number (based upon enumerated list)
!   return data:   -1 to indicate that java interface should not be called
!   references:    java gui event processing user exit
!                  cbasic -> java routines
!
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
SUB javaVPEvent(eventNum) PUBLIC RECURSIVE       
  INTEGER*2                                      \
            eventNum                             ! The Event #

    eventNum = -1

END SUB



!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
!   function javaVPInit                                      khg
!
!   parameters:          initialize for Java Value Pack Gui processing
!   return data:         none
!   data areas modified: enumerated values of events
!                        java class & method variables
!   references:          java gui initialization user exit
!
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
FUNCTION javaVPInit PUBLIC
END FUNCTION


!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
! sub javaVPException
!
! parms: event       The event number fired
!        data$       The data buffer passed
!        execption$  The java exception data buffer returned from the
!                    CBASIC InvokeMethod call (null indicates success).
!
! Comments:  This subroutine is provided so that the application developers
!            have an opportunity to become aware of, and possibly handle
!            exceptions excountered from the invokeMethod call.
!            This routine should be used with extreme caution as the
!            call mechanism is recursive -> generating multiple errors
!            without ever returning will result in a stack overflow.
!            So, the advice is to NOT use this routine to call the javaEvent
!            function!
!
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
SUB javaVPException(event, data$, exception$) PUBLIC RECURSIVE
                                                 ! Appl exception handler
  INTEGER*2                                      \
             event                               ! event number
  STRING                                         \
             data$,                              \ data originally passed
             exception$                          ! exception


END SUB                                          ! Appl Event
