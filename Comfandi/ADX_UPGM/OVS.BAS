! The following example uses the ASSIGNKEY statement 
! to assign a STRING value to several function keys. 
! See the FUNCTIONKEY statement in this chapter for 
! more information. 
ASSIGNKEY 1,"HELP" 
ASSIGNKEY 3,"QUIT" 
ASSIGNKEY 5,"THIS" 
ASSIGNKEY 7,"THAT" 
ASSIGNKEY 10,"TEST" 
FUNCTIONKEY ON 
PRINT "press any key to quit" 
WHILE NOT CONSTAT% 
  WAIT ; 200 ! suspend execution to service lower priority tasks 
WEND 
FUNCTIONKEY OFF 
}END