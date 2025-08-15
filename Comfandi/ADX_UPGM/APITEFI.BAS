!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : APITEFI.BAS                      *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo Transferencia Electronica *
!                de Fondos Ecuador                *
!**************************************************
! Observaciones:
! Version Certificada para DataFast y Unibanco
! Julio  31 2007

%ENVIRON T
!
! Supermarket application global data
!
%INCLUDE EAMTSWKG.J86
%INCLUDE EAMTOPTS.J86
%INCLUDE EAMTRANS.J86
%INCLUDE C:\APPL\APIVARI.J86

String    Global Bin.Autoriza(2),Base.Bin.Autoriza(2),  Auto.Dife(1)    ! Control de autorizadores
String    Global Gr.Tef.CodDif$
Integer*4 Global Gr.Tef.TotBin%, Gr.Tef.BTotBin%

!
! end of data definitions
!------------------------------------------------------------------------
!
!  Standard supermarket routines
!
Sub TSPREC01 EXTERNAL
End Sub
!
Sub TSDSEC01 EXTERNAL
End Sub
!
Sub TSTPEC01 EXTERNAL
End Sub
!
Sub TSCSEC08 EXTERNAL
End Sub
!
Sub TSCSECRK EXTERNAL
End Sub
!
Sub TSTDEC01 EXTERNAL
End Sub

Sub TSHIECET External								    !  Tono de alerta
End Sub

Sub TSCSEC03 EXTERNAL          ! stack display guidance
End Sub

!
FUNCTION FORMAT.AMOUNT(EP.INT4) EXTERNAL 
    Integer*1 FORMAT.AMOUNT
    Integer*4 EP.INT4 
END FUNCTION        
!---------------------------------------------------------------------------
!  System routines
Sub ADXSERVE (RET,FUNC,PARM1,PARM2) EXTERNAL
  Integer*4 RET
  Integer*2 FUNC,PARM1
  String PARM2
End Sub
!---------------------------------------------------------------------------
!  User defined routines
!
Sub EP.SEND.TO.THREADER EXTERNAL
End Sub
!
!
Sub EP.PARSE.THREADER.RESPONSE EXTERNAL
End Sub


Function U.CORTACR External					! Corte de papel
End Function 													!

Function Asic.Datos$(Umsg1$,Umsg2$) External     														! Captura De Datos
String Asic.Datos$, Umsg1$, Umsg2$
End Function 

Function  VISORES4690(U.D.VISOR%, U.D.LINEA1$, U.D.LINEA2$,U.D.TIEMPO%,U.D.POSICION$) External ! Msg Display
String U.d.Linea1$, U.d.Linea2$, U.d.Posicion$								      ! Variables de
Integer*2 U.d.visor%, U.d.tiempo%, u.t.visor%, ud.pos%							! trabajo
String U.D.Linea1$, U.D.Linea2$										!
End Function 

Sub INT4.TO.HEXA(ERRVALUE%,ERRVALUE$) EXTERNAL
  Integer*4 ERRVALUE%
  String ERRVALUE$
End Sub
!
!   *** ASYNCRONOUS ERROR HANDLING  *********
!
Sub ASYNC.ERR(RFLAG,OVER) EXTERNAL
  Integer*2 RFLAG
  String OVER
End Sub
!
!
Sub ERRORTRAP  EXTERNAL
End Sub
!
Sub EP.GET.CURRENT.TIME EXTERNAL
End Sub
!
Sub EP.GETUNPK EXTERNAL
End Sub                                                ! siguiente campo 
!
Sub EP.SAVE.PRINT EXTERNAL
End Sub
!
Sub EP.RESTORE.PRINT  EXTERNAL 
End Sub
!
Sub EP.LINE.PRINT(LINEA$,ESTACION%) EXTERNAL 
    String LINEA$
    Integer*2 ESTACION%
End Sub
!
Sub EP.SAVE.DISPLAY EXTERNAL
End Sub
!
Sub EP.RESTORE.DISPLAY EXTERNAL
End Sub
!
Sub EP.DISPLAY.A.MESSAGE(UE.DISP.MESSAGE$) EXTERNAL 
  String UE.DISP.MESSAGE$
End Sub
!
Sub EP.DISPLAY.AN.ERROR(UE.DISP.MESSAGE$) EXTERNAL
  String UE.DISP.MESSAGE$, UE.WORK$
End Sub
!
Sub EP.SAVE.KEYS EXTERNAL
End Sub
!
Sub EP.RESTORE.KEYS EXTERNAL
End Sub

!--------------------------------------------------------------------------
!   Java API routines
Sub JavaCall.Initialize.Request(ClassName,MethodName,TheRequest) EXTERNAL
  String ClassName
  String MethodName
  String TheRequest
End Sub

Sub JavaCall.AddParameter.String(TheRequest,TheParameter) EXTERNAL
  String TheRequest
  String TheParameter
End Sub

Sub JavaCall.InvokeMethod.RETURNString(TheRequest,RETURNValue, Exception) EXTERNAL
  String TheRequest
  String RETURNValue
  String Exception
End Sub
!--------------------------------------------------------------------------
!   Application manager routines
!---------------------------------------------------------
!


Sub EP.TRX.INVALIDA Public 

     TS.USER.RETURN = 99
     TS.LINEDATA = 51                 ! "TARJETA INVALIDA"
     TS.STACKERR(0) = 0               ! manager's override required
     TS.STACKERR(3) = 0               ! item descriptor
     TS.STACKERR(6) = 0               ! put "M" on last display
     TS.STACKERR(7) = -1              ! indicate no printing required
     TS.MO.REASON = 0                 ! invalid key with department
     Call TSCSEC03       
     EP.EFT.TRX%     = 0
     EP.EFT.DETAIL%  = 0
     EP.EFT.SMA%     = 0

End Sub 

Sub EP.ADD.DATA.ENTRY(UE.DATA1,UE.DATA2,UE.DATA3,UE.DATA4,UE.DATA5,UE.DATA6) PUBLIC
  String UE.DATA1,UE.DATA2,UE.DATA3,UE.DATA4,UE.DATA5,UE.DATA6
  Call EP.SAVE.KEYS                        ! SAVE KEYED DATA
  Dim TS.IO.KEYS(10)                       ! CLEAR KEYED DATA
  Dim TS.IO.DATA$(10)                      ! CLEAR KEYED DATA
  TS.IO.KEYS(10) = 63                      ! Data entry key
  TS.IO.KEYS(4)  = 100                     ! No-sale key
  TS.IO.DATA$(2) = UE.DATA1                ! Campo 1
  TS.IO.DATA$(3) = UE.DATA2		           ! Campo 2
  TS.IO.DATA$(4) = UE.DATA3	               ! Campo 3
  TS.IO.DATA$(5) = UE.DATA4		           ! Campo 4
  TS.IO.DATA$(6) = UE.DATA5	               ! Campo 5
  TS.IO.DATA$(7) = UE.DATA6		           ! Campo 6
  TS.TEMP1I1 = 11                          ! Data entry
  Call TSTPEC01                            ! Process data entry
  Call EP.RESTORE.KEYS
End Sub
!
Sub EP.ADD.DATA.DENTRY99(UE.DATA1,UE.DATA2) PUBLIC
  String UE.DATA1,UE.DATA2
!
  TS.USERDATA$ = PACK$(UE.DATA1) + ":" + UE.DATA2
  TS.TEMP1I1 = 99                  
  Call TSTPEC01
!
End Sub
!
Sub EP.GET.KBDATA(UE.REQ.MESSAGE$, UE.INI.RANGE$, UE.END.RANGE$, \
    UE.KEYB.DATA$) PUBLIC
  String UE.REQ.MESSAGE$, UE.KEYB.DATA$, UE.INI.RANGE$, UE.END.RANGE$,\
         UE.WORK$, UE.SAVDISP$ 
  UE.SAVDISP$ = TS.SAVDISP1$ + TS.SAVDISP2$
  UE.WORK$ = UE.REQ.MESSAGE$ + String$(40," ")
  TS.SAVDISP1$ = LEFT$(UE.WORK$,20) ! Split msg For display
  TS.SAVDISP2$ = MID$(UE.WORK$,21,20)
  TS.IO.MOTORKEY = 0                    ! Set For no input yet
  TS.IO.STATE = 11                      ! Enter/Data state
  While TS.IO.MOTORKEY = 0              ! While still no input
    TS.LINETYPE = 9                     ! Redisplay prompt
    Call TSDSEC01                       ! Prompt For Date
    UNLOCKDEV 32, TS.IO.STATE, PRIORITY ! Keyed expiration date
    WAIT  32; 100                       ! Wait For input
    While NOT EVENT%                    ! While no input
      WAIT  32; 100                     ! Wait For input
    Wend
    TS11.OVRFLAG = 33                   ! Mark For Exit 14
    Call TSCSECRK                       ! Parse input / Exit 14
    If TS.IO.MOTORKEY = 73 Then \       ! Clear key
    Begin 
       If LEN(TS.IO.DATA$(1)) > 0 OR    \ Data Entered
          TS.TEMP1I2 > 1 Then           \ More than 1 key
         TS.IO.MOTORKEY = 0             ! Try again
    EndIf \
    Else \
    Begin                               ! Not clear key
      If TS.IO.KEYS(10) = 80 Then \     ! Keyed data
      Begin
        If LEN(TS.IO.DATA$(10)) < LEN(UE.INI.RANGE$) OR \
           LEN(TS.IO.DATA$(10)) > LEN(UE.END.RANGE$) Then \
        Begin 
          TS.LINETYPE = 8
          TS.LINEDATA = 6
          Call TSCSEC08
          TS.IO.MOTORKEY = 0
        EndIf \
        Else \
        Begin 
          UE.KEYB.DATA$ = TS.IO.DATA$(10) ! 
        EndIf
      EndIf                                ! Keyed data
    EndIf                                  ! Not clear key
  Wend                                     ! While still no input
  UE.WORK$ = ""
  TS11.OVRFLAG = 0                         ! Clear Exit marker
  TS.SAVDISP1$ = LEFT$(UE.SAVDISP$,20)     ! Restore prior display
  TS.SAVDISP2$ = MID$(UE.SAVDISP$,21,20)
  UE.SAVDISP$ = ""
  TS.IO.NEXTSTATE = 10                     ! Restore state = MAIN
  TS.IO.STATE = 10                         ! Restore state = MAIN
  UNLOCKDEV 32, TS.IO.STATE                ! Restore state = MAIN
End Sub
!
!  Print vouchers
!
Sub EP.STORE.EFTLINE(UE.PRINT.LINE$) PUBLIC
  String UE.PRINT.LINE$
!
  EP.POINTER2% = EP.POINTER2% + 1
  If LEN(UE.PRINT.LINE$) <= 38 Then \
    EP.EFT.LINE$(EP.POINTER2%) = UE.PRINT.LINE$ \
  Else \
    EP.EFT.LINE$(EP.POINTER2%) = LEFT$(UE.PRINT.LINE$,38)
!
End Sub
!
!
Sub EP.STORE.VOUCHER(UE.PRINT.LINE$,UE.PRT.CUT$) PUBLIC 
  String UE.PRINT.LINE$, UE.PRT.CUT$
  EP.POINTER1% = EP.POINTER1% + 1
  If LEN(UE.PRINT.LINE$) <= 38 Then \
    EP.VOUCHER$(EP.POINTER1%) = UE.PRINT.LINE$ \
  Else \
    EP.VOUCHER$(EP.POINTER1%) = LEFT$(UE.PRINT.LINE$,38)
  EP.CUT.VOUCHER$(EP.POINTER1%) = UE.PRT.CUT$ 
End Sub
!
!
Sub EPAY.VOUCHER PUBLIC
  Integer*2 EP.COUNT%
  
!  TO.USEREXIT(20) = 0  
!  TO.USEREXIT(60) = 0  
  
  If EP.REPRINT% Then Begin
    TO.TRAILERLINE1$ = LEFT$(EP.GUIDE.MESSAGE$(20),38)
    EP.REPRINT% = 0     
  EndIf \
  Else  \

  If EP.INTERCHG% Then \
  Begin
    TO.TRAILERLINE1$ = LEFT$(EP.GUIDE.MESSAGE$(21),38)
    EP.INTERCHG% = 0     
  EndIf \
  Else  \
  Begin 
    TO.TRAILERLINE1$ = LEFT$(EP.GUIDE.MESSAGE$(19),38)
  EndIf

  TO.TRAILERLINE2$ = String$(38,"_")
  If EP.POINTER1% >= 2 Then \
  Begin
    Call EP.SAVE.PRINT
    For EP.COUNT% = 2 TO EP.POINTER1%
      If EP.CUT.VOUCHER$(EP.COUNT%) = "1" Then \
      Begin 

        !Call EP.LINE.PRINT(EP.SAVE.GLINE$,4100H) 
        Write #34 ; EP.SAVE.GLINE$ + Chr$(10)
        TS.LINETYPE = 18 
        TS.LINEDATA = 0
        Call TSPREC01
      EndIf  \
      Else \
      	Write #34 ; EP.VOUCHER$(EP.COUNT%) + chr$(10)
        !Call EP.LINE.PRINT(EP.VOUCHER$(EP.COUNT%),4100H) 
    Next EP.COUNT% 
  EndIf

  !Call EP.LINE.PRINT(EP.SAVE.GLINE$,4100H) 
  Write #34 ; EP.SAVE.GLINE$ + Chr$(10)
  
  Write #34 ; " " + Chr$(10)
  Write #34 ; " " + Chr$(10)
  Write #34 ; " " + Chr$(10)
  Write #34 ; " " + Chr$(10)
  Write #34 ; " " + Chr$(10)
  Call U.CORTACR
  
  TO.HEADERLINE1$ = EP.SAV.HD1$
  TO.HEADERLINE2$ = EP.SAV.HD2$
  TS.LINETYPE = 18 
  TS.LINEDATA = 0
  Call TSPREC01
!  TO.USEREXIT(20) = -1
!  TO.USEREXIT(60) = -1
  Call EP.RESTORE.PRINT
  TO.TRAILERLINE1$ = EP.SAV.TR1$
  TO.TRAILERLINE2$ = EP.SAV.TR2$

End Sub

Sub EPAY.RESET.VOUCHER PUBLIC
  Dim EP.VOUCHER$(300)
  Dim EP.CUT.VOUCHER$(300)
  EP.POINTER1%  = 0
  EP.LINEA.IMP% = 1
  EP.POINTER2%  = 0
  EP.REPRINT% = 0
  EP.AUTORIZA% = -1
  EP.TEF.CTRLDEV% = 0
  
End Sub
!
!
Sub PRINT.EFT.HEADER PUBLIC
  Integer*2 EP.COUNT%
!
  If EP.POINTER1% >= 2 Then \
  Begin
!    TO.USEREXIT(20) = 0
!    TO.USEREXIT(60) = 0
    Call EP.SAVE.PRINT
    TS.LINETYPE = 29
    TS.SAVPRT$ = String$(38," ")
    TS.SAVPRT.OPT = 4100H
    Call TSPREC01
!    TO.HEADERLINE1$  = EP.ESC$+CHR$(58)+LEFT$(EP.EFT.LINE$(1),30)
    TO.HEADERLINE1$  = LEFT$(EP.EFT.LINE$(1),30)
    TO.HEADERLINE2$  = String$(38," ")
    TS.LINETYPE = 18 
    TS.LINEDATA = 99
    Call TSPREC01
    Call EP.RESTORE.PRINT
!    TO.USEREXIT(20) = -1
!    TO.USEREXIT(60) = -1
  EndIf
End Sub
!
Sub PRINT.EFT.HEADER.CUT PUBLIC
  Integer*2 EP.COUNT%
!  TO.USEREXIT(20) = 0
!  TO.USEREXIT(60) = 0
  Call EP.SAVE.PRINT
!  TO.HEADERLINE1$  = EP.ESC$+CHR$(58)+LEFT$(EP.EFT.LINE$(1),30)
  TO.HEADERLINE1$  = LEFT$(EP.EFT.LINE$(1),30)
  TO.HEADERLINE2$  = String$(38," ")
  TS.LINETYPE = 18 
  TS.LINEDATA = 0
  Call TSPREC01
  Call EP.RESTORE.PRINT
!  TO.USEREXIT(20) = -1
!  TO.USEREXIT(60) = -1
End Sub
!
!
Sub PRINT.IVA.DETAIL(UE.TAX.EPAY$) PUBLIC
  Integer*4 UE.INT4%
  Integer*1 UE.INT1%
  String    UE.TAX.EPAY$
! 
  If EP.FUNCTION$ = "11" OR \
     EP.FUNCTION$ = "12" OR \
     EP.FUNCTION$ = "14" OR \
     EP.FUNCTION$ = "19" Then \
  Begin
    UE.INT4% = VAL(UE.TAX.EPAY$)
    UE.INT1% = FORMAT.AMOUNT(UE.INT4%)
!   Call EP.LINE.PRINT(LEFT$(EP.GUIDE.MESSAGE$(08), 15) + \
!     RIGHT$(String$(12," ") + TS.TEMP1$, 12), 6200H)
    Write #34 ; LEFT$(EP.GUIDE.MESSAGE$(08), 15) + \
                RIGHT$(String$(12," ") + TS.TEMP1$,12) + Chr$(10)
  EndIf
End Sub

Sub PRINT.EFT.DETAIL(UE.VALOR.EPAY$, UE.TAX.EPAY$) PUBLIC
  Integer*4 UE.INT4%
  Integer*2 UE.COUNT%
  Integer*1 UE.INT1%
  String    UE.VALOR.EPAY$, UE.TAX.EPAY$
  
  If TS.PROCEDURE < 1 Then \
  Begin 
    For UE.COUNT% = 1 TO EP.POINTER2%
      !Call EP.LINE.PRINT(EP.EFT.LINE$(UE.COUNT%),6100H) 
      Write #34 ; EP.EFT.LINE$(UE.COUNT%) + chr$(10)
    Next UE.COUNT% 
    !Call EP.LINE.PRINT(String$(38," "),6100H)
    Write #34 ; String$(38," ") + chr$(10)
    EP.POINTER2%   = 0
    EP.EFT.DETAIL% = 0
  EndIf \
  Else \
  Begin 
    EP.M% = 0 
    While EP.LINEA.IMP% <= EP.POINTER2% AND \
          EP.M% = 0 
      !Call EP.LINE.PRINT(EP.EFT.LINE$(EP.LINEA.IMP%),6100H)
      Write #34 ; EP.EFT.LINE$(EP.LINEA.IMP%) + chr$(10)
      EP.LINEA.IMP% = EP.LINEA.IMP% + 1 
      EP.M% = MATCH(EP.EFT.LINE$(1), EP.EFT.LINE$(EP.LINEA.IMP%),1)
    Wend
    !Call EP.LINE.PRINT(String$(38," "),6100H) 
    Write #34 ; String$(38," ") + chr$(10)
    If EP.LINEA.IMP% > EP.POINTER2% Then EP.EFT.DETAIL% = 0
  EndIf
End Sub
!
!
Sub INTERCHG.VOUCHER PUBLIC
!
  EP.INTERCHG%  = -1
!
  Call PRINT.EFT.HEADER.CUT
  Call EPAY.VOUCHER
!
End Sub
!
!
Sub INTERCHG.VOUCHER.CAB PUBLIC
!
  EP.INTERCHG%  = -1
!
  Call EPAY.VOUCHER
!
End Sub
!

Sub Carga.Bin.Autorizador External
End Sub 

Sub Base.Bin.Autorizador  External
End Sub

Sub Carga.Lista.Diferido External
End Sub 

Function Retorno.Autorizador(Xbin$) External
Integer*2 Xh%, Retorno.Autorizador
String Xret$, Xbin$                                         ! Variables de trabajo
End Function 

Function Autorizador.Base(Xbin$) External
Integer*2 Xh%, Autorizador.Base
String Xret$, Xbin$                                         ! Variables de trabajo
End Function 

Sub EPAY.INITIALIZATION PUBLIC
  String UE.VAL1$, UE.VAL2$, EP.LAST$
  Integer*2 UE.VAL2%, EP.I%, EP.PARM%, UE.Q%
!  
  Call EP.DISPLAY.A.MESSAGE("CARGANDO PARAMETROS TEF ESPERE POR FAVOR")  
  EP.SAV.HD1$ = TO.HEADERLINE1$
  EP.SAV.HD2$ = TO.HEADERLINE2$
  EP.SAV.TR1$ = TO.TRAILERLINE1$
  EP.SAV.TR2$ = TO.TRAILERLINE2$
  EP.STX$ = CHR$(2)
  EP.ETX$ = CHR$(3)
  EP.EOT$ = CHR$(4)
  EP.ENQ$ = CHR$(5)
  EP.ACK$ = CHR$(6)
  EP.LF$  = CHR$(10)
  EP.CR$  = CHR$(13)
  EP.NACK$= CHR$(21)
  EP.ETB$ = CHR$(23)
  EP.ESC$ = CHR$(27)
  EP.SIZE.ISO%   = 120
  EP.SIZE.APPL%  = 50
  EP.SIZE.COMM%  = 50
  EP.SIZE.GUIDE% = 50
  EP.SIZE.TV%    = 20
  EP.MAX.TRX%    = 20
  EP.STORE$      = RIGHT$(String$(4,"0") + TS.STORE$  ,4)
!
! nombre Logico: APIPARM = ADX_IDT1:APIPARMR.DAT
!
!---------------------------------------------------------

  EP.LINEA% = 0
  EP.AUTORIZA% = -1
  EP.EFT.ACTIVO% = 0 
  EP.TEF.CTRLDEV% = 0
  
  Dim EP.VOUCHER$(300)
  Dim EP.CUT.VOUCHER$(300)
  Dim EP.EFT.LINE$(100)
  Dim EP.TRX.APPL$(EP.MAX.TRX%)
  Dim EP.TRX.FUNCTION$(EP.MAX.TRX%)
  Dim EP.TRX.TRANSNUM$(EP.MAX.TRX%)
  Dim EP.TRX.RESP%(EP.MAX.TRX%)
  Dim EP.TRX.APPROV%(EP.MAX.TRX%)
  Dim EP.TRX.VOUCHER%(EP.MAX.TRX%)
  Dim EP.TRX.TOHOST1%(EP.MAX.TRX%) 
  Dim EP.TRX.TOHOST2%(EP.MAX.TRX%)
  Dim EP.TRX.ANUL%(EP.MAX.TRX%)
  Dim EP.TRX.REVERSO%(EP.MAX.TRX%)
  Dim EP.TRX.PEND%(EP.MAX.TRX%)   
  Dim EP.TRX.FECHA$(EP.MAX.TRX%)   
  Dim EP.PROMO.ID$(10)
  Dim EP.PROMO.STATUS$(10)
  Dim EP.PROMO.POINTS$(10) 
  Dim EP.PROMO.REDEEM$(10)
  Dim EP.PROMO.AMTPUR$(10)
  Dim EP.PROMO.QTYPUR$(10)     
!
  TS.ER.RETURN = -1
  TS.ERRN = 0
  TS.TS11WERR$ = ""
!
  Open "MSR:" AS 41
!
  If NOT EP.PARM.LOADED% Then \
  Begin 
    Dim EP.ISO.CODE$(EP.SIZE.ISO%)	            ! Descripcion de los mensajes iso 8583
    Dim EP.ISO.RESPONSE$(EP.SIZE.ISO%)          !
    Dim EP.APPL.CODE$(EP.SIZE.APPL%)	          ! Descripcion de los mensajes APPL MNGR
    Dim EP.APPL.RESPONSE$(EP.SIZE.APPL%)        !
    Dim EP.COMM.CODE$(EP.SIZE.COMM%)	          ! Descripcion de los mensajes COMUNICACIONES
    Dim EP.COMM.RESPONSE$(EP.SIZE.COMM%)        !
    Dim EP.GUIDE.CODE$(EP.SIZE.GUIDE%)	        ! Descripcion de los mensajes COMUNICACIONES
    Dim EP.GUIDE.MESSAGE$(EP.SIZE.GUIDE%)       !
    Dim EP.TV.CODE$(EP.SIZE.TV%)	              ! Descripcion de los grupo de tipo variedad especial
    Dim EP.TV.GROUP$(EP.SIZE.TV%)               !
    TS.ER.RETURN = -1
    Open "R::APIPARM" AS EP.IOPARM% NOWRITE NODEL
    If NOT TS.ER.RETURN  Then Begin
      Call EP.DISPLAY.AN.ERROR("ERROR APERTURA APIPARM")
      EP.DEVICE.STATE% = 99
      GOTO NOMORE.MESSAGES
    EndIf
    If END # EP.IOPARM% Then  CLOSE.MESSAGES
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.EFT.ACTIVO% = VAL(UE.VAL2$)
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.POS.PTOS$ = UE.VAL2$
    EP.POS% = MATCH(String$(4,"9")+":",EP.POS.PTOS$,1)
    If EP.POS% = 0 Then  \
      EP.POS% = MATCH(RIGHT$(String$(4,"0")+TS.TERMINAL$,4)+":",EP.POS.PTOS$,1)
    If EP.POS% <> 0 Then  \
      EP.PTOS.ACTIVO% = -1 \
    Else \
      EP.PTOS.ACTIVO% = 0
!
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.POS.RED$ = UE.VAL2$
    EP.POS% = MATCH(String$(4,"9")+":",EP.POS.RED$,1)
    If EP.POS% = 0 Then  \
      EP.POS% = MATCH(RIGHT$(String$(4,"0")+TS.TERMINAL$,4)+":",EP.POS.RED$,1)
    If EP.POS% <> 0 Then  \
      EP.RED.ACTIVO% = -1 \
    Else \
      EP.RED.ACTIVO% = 0
!
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.EFT.CLAVE$ = UE.VAL2$
    EP.LEN.CLAVE% = LEN(EP.EFT.CLAVE$)
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.CHAIN$ = RIGHT$(String$(4,"0") + UE.VAL2$,4)
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.TV.CHEQ$ = RIGHT$(String$(2,"0") + UE.VAL2$,2)
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.TV.TJDB$ = RIGHT$(String$(2,"0") + UE.VAL2$,2)
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.TV.TJCR$ = RIGHT$(String$(2,"0") + UE.VAL2$,2)
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.IVADEV$ = UE.VAL2$
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.CB.PORC% = VAL(UE.VAL2$)
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.CB.MAXIMP% = VAL(UE.VAL2$)
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.TEF.VOU% = VAL(UE.VAL2$)
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.DET.FIN% = VAL(UE.VAL2$)
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.POS.SALDO$ = UE.VAL2$
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.POS.CASHB$ = UE.VAL2$
!
    EP.POS% = MATCH(String$(4,"9")+":",EP.POS.CASHB$,1)
    If EP.POS% = 0 Then \
    Begin 
      EP.POS% = MATCH(RIGHT$(String$(4,"0")+TS.TERMINAL$,4)+":",EP.POS.CASHB$,1)
      If EP.POS% = 0 Then \
      Begin
        EP.CB.MAXIMP% = 0
        EP.CB.PORC%   = 0
      EndIf
    EndIf
!   
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.SPIPE% = VAL(UE.VAL2$)
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.RPIPE% = VAL(UE.VAL2$)
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.EIAP% = VAL(UE.VAL2$)
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.NO.TRACE% = VAL(UE.VAL2$)
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.COM% = VAL(UE.VAL2$)
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.SPEED% = VAL(UE.VAL2$)
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.PARITY$ = UE.VAL2$
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.DATA.BITS% = VAL(UE.VAL2$)
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.STOP.BITS% = VAL(UE.VAL2$)
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.MAX.TRIES% = VAL(UE.VAL2$)
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.CHARS.WAIT% = VAL(UE.VAL2$)
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.SHORT.WAIT% = VAL(UE.VAL2$)
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.LONG.WAIT% = VAL(UE.VAL2$)
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.MESSAGE.WAIT% = VAL(UE.VAL2$)
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.MESSAGE.TIMEOUT% = VAL(UE.VAL2$)
    Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.APPROVAL.TIMEOUT% = VAL(UE.VAL2$)
    EP.PARM.LOADED%  = -1
    EP.PARM% = 0

!-- Cambio datos parametros Apiparm se define rango 6000 para manejo diferidos
!--     


    While EP.PARM% <= 7000
      Read # EP.IOPARM% ; UE.VAL1$, UE.VAL2$
      If UE.VAL2$ = "1000" OR \
         UE.VAL2$ = "2000" OR \
         UE.VAL2$ = "3000" OR \
         UE.VAL2$ = "4000" OR \
         UE.VAL2$ = "5000" OR \
         UE.VAL2$ = "6000" OR \
         UE.VAL2$ = "7000" Then Begin
         	
        EP.PARM% = VAL(UE.VAL2$)
        If EP.PARM% = 2000 Then Begin 
          EP.ISO.CODE$(EP.SIZE.ISO%) = EP.ISO.CODE$(EP.I% - 1)
          EP.ISO.RESPONSE$(EP.SIZE.ISO%) = EP.ISO.RESPONSE$(EP.I% - 1)
        EndIf
        If EP.PARM% = 3000 Then Begin 
          EP.APPL.CODE$(EP.SIZE.APPL%) = EP.APPL.CODE$(EP.I% - 1)
          EP.APPL.RESPONSE$(EP.SIZE.APPL%) = EP.APPL.RESPONSE$(EP.I% - 1)
        EndIf
        If EP.PARM% = 4000 Then Begin 
          EP.COMM.CODE$(EP.SIZE.COMM%) = EP.COMM.CODE$(EP.I% - 1)
          EP.COMM.RESPONSE$(EP.SIZE.COMM%) = EP.COMM.RESPONSE$(EP.I% - 1)
        EndIf
        If EP.PARM% = 5000 Then Begin 
          EP.GUIDE.CODE$(EP.SIZE.GUIDE%) = EP.GUIDE.CODE$(EP.I% - 1)
          EP.GUIDE.MESSAGE$(EP.SIZE.GUIDE%) = EP.GUIDE.MESSAGE$(EP.I% - 1)
        EndIf
        If EP.PARM% = 7000 Then Begin 
          EP.TV.CODE$(EP.SIZE.TV%) = EP.TV.CODE$(EP.I% - 1)
          EP.TV.GROUP$(EP.SIZE.TV%) = EP.TV.GROUP$(EP.I% - 1)
          EP.PARM% = 8000
        EndIf
        EP.I% = 1

      EndIf Else Begin
      	
        If EP.PARM% = 6000 Then Begin     						! Datos diferidos
            EP.DIF.DATAFAST$ = UE.VAL2$     					! Si diferido activo DATAFAST
            Read # EP.IOPARM% ; UE.VAL1$, UE.VAL2$
            EP.DIF.CODDATA$  = UE.VAL2$     					! Codigo Reportar diferido
            Read # EP.IOPARM% ; UE.VAL1$, UE.VAL2$
            EP.DIF.DATAMSG$  = UE.VAL1$               ! MENSAJE CAPTURA
            EP.DIF.DATAMES$  = "," + UE.VAL2$+","     ! Lista de meses permitidos
            Read # EP.IOPARM% ; UE.VAL1$, UE.VAL2$
            EP.DIF.UNIBANCO$ = UE.VAL2$     					! Si diferido activo UNIBANCO
            Read # EP.IOPARM% ; UE.VAL1$, UE.VAL2$
            EP.DIF.CODUNIB$  = UE.VAL2$     					! Codigo Reportar diferido
            Read # EP.IOPARM% ; UE.VAL1$, UE.VAL2$
            EP.DIF.UNIBMSG$  = UE.VAL1$               ! MENSAJE CAPTURA
            EP.DIF.UNIBMES$  = "," + UE.VAL2$+","     ! Lista de meses permitidos            
        EndIf
        
        If EP.PARM% = 1000 Then Begin
          EP.ISO.RESPONSE$(EP.I%) = UE.VAL1$
          EP.ISO.CODE$(EP.I%) = UE.VAL2$
        EndIf
        
        If EP.PARM% = 2000 Then Begin
          EP.APPL.RESPONSE$(EP.I%) = UE.VAL1$
          EP.APPL.CODE$(EP.I%) = UE.VAL2$
        EndIf
        
        If EP.PARM% = 3000 Then Begin
          EP.COMM.RESPONSE$(EP.I%) = UE.VAL1$
          EP.COMM.CODE$(EP.I%) = UE.VAL2$
        EndIf
        
        If EP.PARM% = 4000 Then Begin
          EP.GUIDE.MESSAGE$(EP.I%) = UE.VAL1$
          EP.GUIDE.CODE$(EP.I%) = UE.VAL2$
        EndIf
        
        If EP.PARM% = 5000 Then Begin
          UE.Q% = MATCH(" ",UE.VAL1$,1)
          If UE.Q% >= 2 Then \
            UE.VAL1$ = LEFT$(UE.VAL1$, UE.Q%-1)
            EP.TV.GROUP$(EP.I%) = UE.VAL1$
            EP.TV.CODE$(EP.I%)  = UE.VAL2$
        EndIf
        
        EP.I% = EP.I% + 1
      EndIf        !
    Wend
    CLOSE.MESSAGES:    
      Close EP.IOPARM%
      Call Carga.Bin.Autorizador               ! Carga Autorizador para diferidos
      Call Base.Bin.Autorizador                ! Carga Autorizador Base Bines
      Call Carga.Lista.Diferido                ! Lista Diferidos para cajero
      Call EP.LINE.PRINT("MODULO TEF Vie 20/10/2017 11:21 am",4100H)
      Call EP.LINE.PRINT("MODULO TEF Vie 20/10/2017 11:21 am",2100H)
 EndIf
  
NOMORE.MESSAGES:
!
  If EP.EFT.ACTIVO% Then \
  Begin
    EP.METHOD$      = "runOther"
    EP.CLASS.GRAL$  = "com.appl.ApplKernel"
    EP.REQUEST$     ="C$"
    EP.RUNMETH$     ="appl.ApplManager.setNumterminal"
    EP.EXCEPTION$=""
    EP.TERMINAL$=RIGHT$(String$(6,"0")+TS.TERMINAL$,6)+ \
               RIGHT$(String$(4,"0")+EP.CHAIN$,4) + \
               RIGHT$(String$(4,"0")+TS.STORE$,4)

    Call JavaCall.Initialize.Request(EP.CLASS.GRAL$,EP.METHOD$,EP.REQUEST$)
    Call JavaCall.AddParameter.String(EP.REQUEST$,EP.TERMINAL$)
    Call JavaCall.AddParameter.String(EP.REQUEST$,EP.RUNMETH$)
    EP.EXCEPTION$=""
    Call JavaCall.InvokeMethod.RETURNString(EP.REQUEST$,EP.RETURNVALUE$,EP.EXCEPTION$)


    If LEN(EP.EXCEPTION$) > 0 Then \
    Begin
      Call EP.DISPLAY.AN.ERROR("Err en:"+EP.METHOD$)
      Exit Sub
    EndIf

    Call EP.DISPLAY.A.MESSAGE(EP.RETURNVALUE$)
!---------------------------------------------------------
!  Call EP.DISPLAY.A.MESSAGE("userht="+ UNPACK$(TE.TR.USERHT$) + "|")
!---------------------------------------------------------
!
    If VAL(LEFT$(UNPACK$(TE.TR.USERHT$),6)) <> 0 Then \
    Begin 
      EP.EPAY.TRANSNUM$ = LEFT$(UNPACK$(TE.TR.USERHT$),6)
    EndIf \
    Else \
    Begin 
      TS.ER.RETURN = -1
      Open "R::EFTTRX" KEYED RECL 158 AS EP.IOPARM% NOWRITE NODEL
      If NOT TS.ER.RETURN  Then Begin   ! si no abre
        TE.TR.USERHT$ = PACK$(String$(6,"0"))+ MID$(TE.TR.USERHT$,4,7)  
        EP.EPAY.TRANSNUM$ = LEFT$(UNPACK$(TE.TR.USERHT$),6)
  !--------------------------------------------------------- 
  !     Call EP.DISPLAY.AN.ERROR("no abre EFTTRX="+ UNPACK$(TE.TR.USERHT$) + ":")
  !---------------------------------------------------------

      EndIf \
      Else \
      Begin 
        EP.KEY$ = RIGHT$(String$(4,"0")+TS.TERMINAL$,4) +   \
           String$(12,"9")
        TS.ER.RETURN = -1 
        Read Form "C4 C12 5C20 C40 C2"; #EP.IOPARM%         \
          KEY EP.KEY$ ;                                     \
          EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,  \
          EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$,         \
          EP.A$, EP.A$
        If NOT TS.ER.RETURN Then \  ! si no existe
        Begin 
          TE.TR.USERHT$ = PACK$(String$(6,"0"))+ MID$(TE.TR.USERHT$,4,7)  ! 
          EP.EPAY.TRANSNUM$ = String$(6,"0")
!---------------------------------------------------------
       Call EP.DISPLAY.A.MESSAGE("NO HAY CONSECUTIVO="+ UNPACK$(TE.TR.USERHT$) + ":")
!---------------------------------------------------------
        EndIf \
        Else \
        Begin 
          EP.EPAY.TRANSNUM$ = LEFT$(EP.DE2.DATA$,6)
          TE.TR.USERHT$ = PACK$(EP.EPAY.TRANSNUM$)+ MID$(TE.TR.USERHT$,4,7)  
!---------------------------------------------------------
      Call EP.DISPLAY.A.MESSAGE("si lee="+ UNPACK$(TE.TR.USERHT$) + ":")
!---------------------------------------------------------
        EndIf
        CLOSE EP.IOPARM%
      EndIf
    EndIf
!
  EndIf  
!

End Sub
!
!
!
!
Sub TRANSLATE.COMM.CODE(UE.CODE$,UE.CODE.DESC$) PUBLIC
  String UE.CODE$,UE.CODE.DESC$
  Integer*2 EP.I%
  Integer*1 EP.FOUND%
!
  EP.FOUND% = 0
  EP.I% = 1
  While NOT EP.FOUND% AND EP.I% < EP.SIZE.COMM%
    If UE.CODE$ = EP.COMM.CODE$(EP.I%) Then \
    Begin
      EP.FOUND% = -1
      UE.CODE.DESC$ = EP.COMM.RESPONSE$(EP.I%)
    EndIf
    EP.I% = EP.I% + 1
  Wend
  If NOT EP.FOUND% Then \
    UE.CODE.DESC$ = EP.COMM.RESPONSE$(EP.SIZE.COMM%)
End Sub
!
!
Sub TRANSLATE.APPL.CODE(UE.CODE$,UE.CODE.DESC$) PUBLIC
  String UE.CODE$,UE.CODE.DESC$
  Integer*2 EP.I%
  Integer*1 EP.FOUND%
  EP.FOUND% = 0
  EP.I% = 1
  While NOT EP.FOUND% AND EP.I% <= EP.SIZE.APPL%
    If UE.CODE$ = EP.APPL.CODE$(EP.I%) Then \
    Begin
      EP.FOUND% = -1
      UE.CODE.DESC$ = EP.APPL.RESPONSE$(EP.I%)
    EndIf
    EP.I% = EP.I% + 1
  Wend
  If NOT EP.FOUND% Then \
    UE.CODE.DESC$ = EP.APPL.RESPONSE$(EP.SIZE.APPL%)
End Sub
!
!
Sub TRANSLATE.ISO.CODE(UE.CODE$,UE.CODE.DESC$) PUBLIC
  String UE.CODE$,UE.CODE.DESC$
  Integer*2 EP.I%
  Integer*1 EP.FOUND%
  EP.FOUND% = 0
  EP.I% = 1
  While NOT EP.FOUND% AND EP.I% <= EP.SIZE.ISO%
    If UE.CODE$ = EP.ISO.CODE$(EP.I%) Then \
    Begin
      EP.FOUND% = -1
      UE.CODE.DESC$ = EP.ISO.RESPONSE$(EP.I%)
    EndIf
    EP.I% = EP.I% + 1
  Wend
  If NOT EP.FOUND% Then \
    UE.CODE.DESC$ = EP.ISO.RESPONSE$(EP.SIZE.ISO%)
End Sub
!
!
Sub TRANSLATE.GUIDE.CODE(UE.CODE$,UE.CODE.DESC$) PUBLIC
  String UE.CODE$,UE.CODE.DESC$
  Integer*2 EP.I%
  Integer*1 EP.FOUND%
  EP.FOUND% = 0
  EP.I% = 1
  While NOT EP.FOUND% AND EP.I% <= EP.SIZE.GUIDE%
    If UE.CODE$ = EP.GUIDE.CODE$(EP.I%) Then \
    Begin
      EP.FOUND% = -1
      UE.CODE.DESC$ = EP.GUIDE.MESSAGE$(EP.I%)
    EndIf
    EP.I% = EP.I% + 1
  Wend
  If NOT EP.FOUND% Then \
    UE.CODE.DESC$ = EP.GUIDE.MESSAGE$(EP.SIZE.GUIDE%)
End Sub
!
Sub TRANSLATE.TV.CODE(UE.GROUP.DESC$,UE.CODE$) PUBLIC
  String UE.CODE$,UE.GROUP.DESC$
  Integer*2 EP.I%
  Integer*1 EP.FOUND%
  EP.FOUND% = 0
  EP.I% = 1                              
  UE.CODE$ = ""
  While NOT EP.FOUND% AND EP.I% <= EP.SIZE.TV%
    EP.POS% = MATCH(UE.GROUP.DESC$,EP.TV.GROUP$(EP.I%),1)
    If EP.POS% <> 0 Then \
    Begin
      EP.FOUND% = -1
      UE.CODE$ = EP.TV.CODE$(EP.I%)
    EndIf
    EP.I% = EP.I% + 1
  Wend
  If NOT EP.FOUND% Then \
    UE.CODE$ = "00"
End Sub
!
!
Sub CAL.TAX.TOTALS(EP.TAX.VAL%) PUBLIC
  Integer*4 EP.TAX.VAL%
  Integer*2 IFOR%
!  
  EP.TAX.VAL% = 0
  For IFOR% = 1 TO 8

    EP.TAX.VAL%= EP.TAX.VAL% + ROUND(FLOAT(UE.IVAC.TOTALS(IFOR%))/\
              (1.+FLOAT(UE.IVAC.TABLAIVA(IFOR%))/100.)*FLOAT(UE.IVAC.TABLAIVA(IFOR%))/100.,0,0)

  Next IFOR%
  
End Sub
!
!
Sub CAL.TAX.BASE(TAX.BASE%) PUBLIC
  Integer*4 TAX.BASE%
  Integer*2 IFOR%, POS%
  TAX.BASE% = 0
  For IFOR% = 1 TO 8
    POS% = MATCH(STR$(UE.IVAC.TABLAIVA(IFOR%))+"%",EP.IVADEV$,1)
    If POS% = 1 OR MID$(EP.IVADEV$,POS%-1,1) = "%" Then \
    Begin  
      TAX.BASE%= TAX.BASE% + ROUND(FLOAT(UE.IVAC.TOTALS(IFOR%))/\
              (1.+FLOAT(UE.IVAC.TABLAIVA(IFOR%))/100.),0,0)
      Call EP.DISPLAY.AN.ERROR("IVA "+STR$(UE.IVAC.TABLAIVA(IFOR%))+\
          "="+STR$(UE.IVAC.TOTALS(IFOR%)))
    EndIf 
  Next IFOR%
End Sub
!

FUNCTION CAL.TAX.BASE2%(TAX.BASE%) PUBLIC
  Integer*4 CAL.TAX.BASE2%,TAX.BASE%
  Integer*2 IFOR%, POS%
  TAX.BASE% = 0
  For IFOR% = 1 TO 8
    POS% = MATCH(STR$(UE.IVAC.TABLAIVA(IFOR%))+"%",EP.IVADEV$,1)
    If POS% = 1 OR MID$(EP.IVADEV$,POS%-1,1) = "%" Then \
    Begin  
      TAX.BASE%= TAX.BASE% + ROUND(FLOAT(UE.IVAC.TOTALS(IFOR%))/\
              (1.+FLOAT(UE.IVAC.TABLAIVA(IFOR%))/100.),0,0)
      Call EP.DISPLAY.AN.ERROR("IVA "+STR$(UE.IVAC.TABLAIVA(IFOR%))+\
          "="+STR$(UE.IVAC.TOTALS(IFOR%)))
    EndIf 
  Next IFOR%
  CAL.TAX.BASE2% = TAX.BASE%
END FUNCTION
!
Sub EP.BUSCA.LIMITE.TV(UE.TIPO.VAR$)
  Integer*2 UE.I%
  Integer*1 UE.FOUND%
  String UE.TIPO.VAR$

  UE.I% = 1
  UE.FOUND% = 0 
  While UE.I% <= TO.NUMTNDR AND NOT UE.FOUND%
    If TO.TENDOPTS(UE.I%,0) = VAL(LEFT$(UE.TIPO.VAR$,1)) AND  \
       TO.TENDOPTS(UE.I%,1) = VAL(RIGHT$(UE.TIPO.VAR$,1)) Then \
    Begin 
      EP.MAX.TV% = TO.TENDLIMITS(UE.I%,0)
      EP.CHG.TV% = TO.TENDLIMITS(UE.I%,1)
      
            If TO.TENDOPTS(UE.I%,7) <> 2 Then \
      Begin 
        EP.VERIF.TV.SAV% = TO.TENDOPTS(UE.I%,7)
        TO.TENDOPTS(UE.I%,7) = 2
      EndIf \
      Else  \
      Begin
        EP.VERIF.TV.SAV% = 99
      EndIf
      UE.FOUND%  = -1
      EP.TV.POS% = UE.I%
    EndIf \
    Else  \
    UE.I% = UE.I% + 1
  Wend    


End Sub

!
!  Read MSG data
!

Sub EP.GET.TRX.TYPE(UE.DISP.MESSAGE$,UE.MSR.DATA$,UE.KEYB.DATA$) PUBLIC
   String UE.DISP.MESSAGE$, UE.MSR.DATA$, UE.KEYB.DATA$, UE.SAVDISP$
   INTEGER*4 X.GETLONG
!
   UE.MSR.DATA$ = ""
   UE.SAVDISP$ = TS.SAVDISP1$ + TS.SAVDISP2$
   TS.SAVDISP1$ = LEFT$(UE.DISP.MESSAGE$,20) 
   TS.SAVDISP2$ = MID$(UE.DISP.MESSAGE$,21,20)
   TS.IO.MOTORKEY = 0                      ! Set For no input yet
   TS.IO.STATE = 7                         ! Acct/Date state
   While TS.IO.MOTORKEY = 0                ! While still no Input
     UNLOCKDEV 32, TS.IO.STATE, PRIORITY   ! Keyed as account number
     If EVENT% NE 41 Then \
     Begin                                ! Not MSR already
       TS.LINETYPE = 9                    ! Redisplay prompt
       Call TSDSEC01                      ! Prompt For Customer ID
     EndIf                                ! Not MSR in Kiosk mode
       TS.ER.RETURN = -1                  ! RETURN from error
     UNLOCKDEV  41                        ! Unlock the MSR
     TS.ER.RETURN = -1                    ! RETURN from error
     WAIT  32, 41; 10                     ! Wait For input
     While NOT EVENT%                     ! While no input
       TS.ER.RETURN = -1                  ! RETURN from error
       WAIT  32, 41; 100                  ! Wait For input
     Wend
     TS.ER.RETURN = 0
! 
     If EVENT% = 32 Then \                ! Process keyboard entry
     Begin                                ! Input from I/O processor
       TS11.OVRFLAG = 34                  ! Mark For Exit 14
       Call TSCSECRK                      ! Parse input / Exit 14
       If TS.IO.MOTORKEY = 73 Then \      ! Clear key
       Begin  
         If LEN(TS.IO.DATA$(1)) > 0 OR    \ Data Entered with clear key
            TS.TEMP1I2 > 1 Then \         ! More than 1 key
         Begin        
           TS.IO.MOTORKEY = 0             ! Try again
         EndIf                            ! Clear after data
       EndIf \ 
       Else \                             ! Not clear key
       Begin  
         If TS.IO.KEYS(10) = 80 Then \    ! Keyed data
         Begin 
           If LEN(TS.IO.DATA$(10)) > 0 Then  \ any option taken
           Begin 
             If (VAL(TS.IO.DATA$(10)) < 1 OR    \ Not valid option 
                VAL(TS.IO.DATA$(10)) > 2 ) Then \
             Begin                            !
               TS.LINETYPE = 8                ! Operator Guidance
               TS.LINEDATA = 6                ! Data out of range
               Call TSCSEC08                  ! Guidance with Clear
               TS.IO.MOTORKEY = 0             ! Try again
             EndIf \                          ! Good option or no option
             Else \                           ! Valid option
             Begin  
               UE.KEYB.DATA$ = TS.IO.DATA$(10) ! Move option keyed
               UE.MSR.DATA$  = ""             ! Not data read
             EndIf                            ! Valid option
           EndIf \
           Else \
           Begin
             TS.LINETYPE = 8                ! Operator Guidance
             TS.LINEDATA = 6                ! Data out of range
             Call TSCSEC08                  ! Guidance with Clear
             TS.IO.MOTORKEY = 0             ! Try again
           EndIf
         EndIf                              ! Keyed data
       EndIf                                ! Not clear key
     EndIf \ 
     Else \                                 ! Not I/O processor
     Begin   
!      
       If EVENT% = 41 Then \                ! Input from MSR
       Begin
         UE.KEYB.DATA$ = ""
         UE.MSR.DATA$  = ""
         TS.ER.RETURN = -1                  ! RETURN from error
         Read #  41 ; LINE UE.MSR.DATA$     ! Read MSR
         If LEN(UE.MSR.DATA$) < 6 OR        \ Insufficient data
            TS.ER.RETURN = 0 Then \
         Begin     ! Bad read
           TS.IO.MOTORKEY = 73              ! Clear key
           UE.MSR.DATA$ = ""
         EndIf Else Begin                              ! Data read
           TS.IO.DEVICE = 1                 ! Force a keyed account
           TS.IO.MOTORKEY = 80              ! Show input ready
         EndIf                              ! Data read
         TS.ER.RETURN = 0
       EndIf                                ! Input from MSR
     EndIf                                  ! Not I/O processor
   Wend                                     ! While still no input
! 
   
   TS11.OVRFLAG = 0                         ! Clear Exit marker
   TS.SAVDISP1$ = LEFT$(UE.SAVDISP$,20)     ! Restore prior display
   TS.SAVDISP2$ = MID$(UE.SAVDISP$,21,20)
   UE.SAVDISP$ = ""
   TS.IO.NEXTSTATE = 10                     ! Restore state = MAIN
   TS.IO.STATE = 10                         ! Restore state = MAIN
   UNLOCKDEV 32, TS.IO.STATE                ! Restore state = MAIN
      
!
End Sub
!-----------------------------------------------------------
!  	Electronic funds transfer API routine
!-----------------------------------------------------------
!                  EFT Common Application Routines
!
Sub EP.SEND.DBCR.TO.AMJ2 PUBLIC

    EP.METHOD$      = "runOther"
    EP.CLASS.GRAL$  = "com.appl.ApplKernel"
    EP.REQUEST$     ="C$"
    EP.RUNMETH$     ="appl.ApplManager.tarjeta"
    EP.EXCEPTION$   =""
    EP.RETURNVALUE$ = ""
    EP.AMJ.MESSAGE$ = ""
!
    Call JavaCall.Initialize.Request(EP.CLASS.GRAL$,EP.METHOD$,EP.REQUEST$)
    Call JavaCall.AddParameter.String(EP.REQUEST$,EP.MESSAGE$)
    Call JavaCall.AddParameter.String(EP.REQUEST$,EP.RUNMETH$)
    EP.EXCEPTION$=""
    Call JavaCall.InvokeMethod.RETURNString(EP.REQUEST$,EP.RETURNVALUE$,\
        EP.EXCEPTION$)
!
    If LEN(EP.EXCEPTION$) > 0 Then \
    Begin
      Call EP.LINE.PRINT("Error en el envio",4100H)
      Call EP.LINE.PRINT(LEFT$(EP.EXCEPTION$,38),4100H)
      EP.ERRNCODE% = 1
      EP.GOOD.END% = 0
    EndIf \
    Else \
    Begin
      EP.ERRNCODE% = 0
      EP.GOOD.END% = -1
      EP.AMJ.MESSAGE$ = EP.RETURNVALUE$
    EndIf
End Sub 
!
!
Sub EP.PARSE.DBCR.RESPONSE2 PUBLIC
!
  EP.H.AUTH.NUMBER$ = ""
  EP.H.APPROV.CODE$ = ""
  EP.H.APPROV.DESC$ = ""
  EP.H.CARD.NUMBER$ = ""
  EP.COMERCIO$    = ""
  EP.EPAY.TERMINAL$= ""
  EP.CUOTAS.QTY$  = ""
  EP.FECHA.POSTEO$= ""
  EP.FECHA.PROC$  = ""
  EP.COD.PROC$    = ""
  EP.RRN$         = ""
  EP.H.TIPO.VAR$    = ""
  EP.FRANQUICIA$  = ""
  EP.BANCO$       = ""
  EP.PRODUCTO$    = ""
  EP.H.USER.DATA$   = ""
  EP.APPL.STATUS$ = ""
  EP.FECHA.VENC$  = ""
  EP.CARD.BIN$    = ""
!
    If EP.M.LEN% >= 10 Then  \
      EP.H.AUTH.NUMBER$ = MID$(EP.AMJ.MESSAGE$,10,6)
    If EP.M.LEN% >= 16 Then  \
      EP.H.APPROV.CODE$ = MID$(EP.AMJ.MESSAGE$,16,2)
    If EP.M.LEN% >= 18 Then  \
      EP.H.APPROV.DESC$ = MID$(EP.AMJ.MESSAGE$,18,20)
    If EP.M.LEN% >= 38 Then  \
      EP.H.CARD.NUMBER$ = MID$(EP.AMJ.MESSAGE$,38,4)
    If EP.M.LEN% >= 42 Then  \
      EP.COMERCIO$ = MID$(EP.AMJ.MESSAGE$,42,10)
    If EP.M.LEN% >= 52 Then  \
      EP.EPAY.TERMINAL$ = MID$(EP.AMJ.MESSAGE$,52,8)
    If EP.M.LEN% >= 60 Then  \
      EP.CUOTAS.QTY$ = MID$(EP.AMJ.MESSAGE$,60,2)
    If EP.M.LEN% >= 62 Then  \
      EP.FECHA.POSTEO$ = MID$(EP.AMJ.MESSAGE$,62,4)
    If EP.M.LEN% >= 66 Then  \
      EP.FECHA.PROC$ = MID$(EP.AMJ.MESSAGE$,66,12)
    If EP.M.LEN% >= 78 Then  \
      EP.COD.PROC$ = MID$(EP.AMJ.MESSAGE$,78,6)
    If EP.M.LEN% >= 84 Then  \
      EP.RRN$ = MID$(EP.AMJ.MESSAGE$,84,12)
    If EP.M.LEN% >= 96 Then  \
      EP.H.TIPO.VAR$ = MID$(EP.AMJ.MESSAGE$,96,2)
!    EP.TIPO.VAR$ = STR$(EP.TV.TARJ%)
    If EP.M.LEN% >= 98 Then  \
      EP.FRANQUICIA$ = MID$(EP.AMJ.MESSAGE$,98,10)
    If EP.M.LEN% >= 108 Then  \
      EP.BANCO$ = MID$(EP.AMJ.MESSAGE$,108,10)
    If EP.M.LEN% >= 118 Then \
      EP.PRODUCTO$ = MID$(EP.AMJ.MESSAGE$,118,10)
    EP.H.USER.DATA$ = EP.FRANQUICIA$ + EP.BANCO$ + EP.PRODUCTO$
    If EP.M.LEN% >= 128 Then \
      EP.FECHA.VENC$ = MID$(EP.AMJ.MESSAGE$,128,4)
    If EP.M.LEN% >= 132 Then \
      EP.CARD.BIN$ = MID$(EP.AMJ.MESSAGE$,132,6)    
    If EP.M.LEN% >= 138 Then \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,138,2)
!---------------------------------------------------------
  If NOT EP.NO.TRACE% Then \
  Begin
    TS.LINETYPE = 29
    TS.SAVPRT$ = "Parse="+" DBCR"+\
              " len= "+STR$(EP.M.LEN%)+\
             " app="+EP.H.APPROV.CODE$+ \
             " tv= "+EP.H.TIPO.VAR$
    TS.SAVPRT.OPT = 4100H
    Call TSPREC01
  EndIf
!---------------------------------------------------------
End Sub
!
Sub EP.SEND.MSG.TO.AMJ PUBLIC
!
!   Call Java appl manager routine
!
    EP.METHOD$      = "runOther"
    EP.CLASS.GRAL$  = "com.appl.ApplKernel"
    EP.REQUEST$     ="C$"
    EP.RUNMETH$     ="appl.ApplManager.kernel"
    EP.EXCEPTION$=""
    EP.RETURNVALUE$ = ""
    EP.AMJ.MESSAGE$ = ""
!
    Call JavaCall.Initialize.Request(EP.CLASS.GRAL$,EP.METHOD$,EP.REQUEST$)
    Call JavaCall.AddParameter.String(EP.REQUEST$,EP.MESSAGE$)
    Call JavaCall.AddParameter.String(EP.REQUEST$,EP.RUNMETH$)
    EP.EXCEPTION$=""
    Call JavaCall.InvokeMethod.RETURNString(EP.REQUEST$,EP.RETURNVALUE$,\
        EP.EXCEPTION$)
!
    If LEN(EP.EXCEPTION$) > 0 Then \
    Begin
      Call EP.LINE.PRINT("Error en el envio",4100H)
      Call EP.LINE.PRINT(LEFT$(EP.EXCEPTION$,38),4100H)
      EP.ERRNCODE% = 1
      EP.GOOD.END% = 0
    EndIf \
    Else \
    Begin
      EP.ERRNCODE% = 0
      EP.GOOD.END% = -1
      EP.AMJ.MESSAGE$ = EP.RETURNVALUE$
    EndIf
End Sub
!
!
Sub EP.SEND.CHEQUE.TO.AMJ2 PUBLIC
    EP.METHOD$      = "runOther"
    EP.CLASS.GRAL$  = "com.appl.ApplKernel"
    EP.REQUEST$     ="C$"
    EP.RUNMETH$     ="appl.ApplManager.Cheque"
    EP.EXCEPTION$=""
    EP.RETURNVALUE$ = ""
    EP.AMJ.MESSAGE$ = ""
!
    Call JavaCall.Initialize.Request(EP.CLASS.GRAL$,EP.METHOD$,EP.REQUEST$)
    Call JavaCall.AddParameter.String(EP.REQUEST$,EP.MESSAGE$)
    Call JavaCall.AddParameter.String(EP.REQUEST$,EP.RUNMETH$)
    EP.EXCEPTION$=""
    Call JavaCall.InvokeMethod.RETURNString(EP.REQUEST$,EP.RETURNVALUE$,\
        EP.EXCEPTION$)
!
    If LEN(EP.EXCEPTION$) > 0 Then \
    Begin
      Call EP.LINE.PRINT("Error en el envio",4100H)
      Call EP.LINE.PRINT(LEFT$(EP.EXCEPTION$,38),4100H)
      EP.ERRNCODE% = 1
      EP.GOOD.END% = 0
    EndIf \
    Else \
    Begin
      EP.ERRNCODE% = 0
      EP.GOOD.END% = -1
      EP.AMJ.MESSAGE$ = EP.RETURNVALUE$
    EndIf
End Sub 
!
!
Sub EP.PARSE.CHEQUE.RESPONSE2 PUBLIC
!
  EP.H.AUTH.NUMBER$ = ""
  EP.H.APPROV.CODE$ = ""
  EP.H.APPROV.DESC$ = ""
  EP.COMERCIO$    = ""
  EP.EPAY.TERMINAL$= ""
  EP.FECHA.POSTEO$= ""
  EP.FECHA.PROC$  = ""
  EP.COD.PROC$    = ""
  EP.RRN$         = ""
  EP.H.TIPO.VAR$    = ""
  EP.FRANQUICIA$  = ""
  EP.BANCO$       = ""
  EP.PRODUCTO$    = ""
  EP.H.USER.DATA$   = ""
  EP.APPL.STATUS$  = ""
!
    If EP.M.LEN% >= 10 Then  \
      EP.H.AUTH.NUMBER$ = MID$(EP.AMJ.MESSAGE$,10,6)
    If EP.M.LEN% >= 16 Then  \
      EP.H.APPROV.CODE$ = MID$(EP.AMJ.MESSAGE$,16,2)
    If EP.M.LEN% >= 18 Then  \
      EP.H.APPROV.DESC$ = MID$(EP.AMJ.MESSAGE$,18,20)
    If EP.M.LEN% >= 38 Then  \
      EP.COMERCIO$ = MID$(EP.AMJ.MESSAGE$,38,10)
    If EP.M.LEN% >= 48 Then  \
      EP.EPAY.TERMINAL$ = MID$(EP.AMJ.MESSAGE$,48,8)
    If EP.M.LEN% >= 56 Then  \
      EP.FECHA.POSTEO$ = MID$(EP.AMJ.MESSAGE$,56,4)
    If EP.M.LEN% >= 60 Then  \
      EP.FECHA.PROC$ = MID$(EP.AMJ.MESSAGE$,60,12)
    If EP.M.LEN% >= 72 Then  \
      EP.COD.PROC$ = MID$(EP.AMJ.MESSAGE$,72,6)
    If EP.M.LEN% >= 78 Then  \
      EP.RRN$ = MID$(EP.AMJ.MESSAGE$,78,12)
    If EP.M.LEN% >= 90 Then  \
      EP.H.TIPO.VAR$ = MID$(EP.AMJ.MESSAGE$,90,2)
!    EP.TIPO.VAR$ = STR$(EP.TV.TARJ%)
    If EP.M.LEN% >= 92 Then  \
      EP.FRANQUICIA$ = MID$(EP.AMJ.MESSAGE$,92,10)
    If EP.M.LEN% >= 102 Then  \
      EP.BANCO$ = MID$(EP.AMJ.MESSAGE$,102,10)
    If EP.M.LEN% >= 112 Then \
      EP.PRODUCTO$ = MID$(EP.AMJ.MESSAGE$,112,10)
    EP.H.USER.DATA$ = EP.FRANQUICIA$  + EP.BANCO$ + EP.PRODUCTO$
    If EP.M.LEN% >= 122 Then \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,122,2)
End Sub
!
Sub EP.PARSE.MSRTEST.REQUEST PUBLIC
!
    EP.APPL.STATUS$  = ""
!
    If EP.M.LEN% >= 10 Then \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,10,2)
!      
End Sub
!
Sub EP.PARSE.DISPLAY.REQUEST PUBLIC
!
    EP.DISP.MESSAGE$ = ""
    EP.DISP.PARAM$   = ""
    EP.DISP.OPER$    = ""
    EP.DISP.CUST$    = ""
    EP.APPL.STATUS$  = ""
!
    If EP.M.LEN% >= 10 Then \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,10,2)
    If EP.M.LEN% >= 12 Then \
      EP.DISP.MESSAGE$ = MID$(EP.AMJ.MESSAGE$,12,40)
    If EP.M.LEN% >= 52 Then \
      EP.DISP.PARAM$ = MID$(EP.AMJ.MESSAGE$,52,1)
    If EP.M.LEN% >= 53 Then \
      EP.DISP.OPER$ = MID$(EP.AMJ.MESSAGE$,53,1)
    If EP.M.LEN% >= 54 Then \
      EP.DISP.CUST$ = MID$(EP.AMJ.MESSAGE$,54,1)
!      
End Sub
!
Sub EP.PARSE.DISPLAY.REQUEST2 PUBLIC
!
    EP.DISP.MESSAGE$ = ""
    EP.DISP.PARAM$   = ""
    EP.DISP.OPER$    = ""
    EP.APPL.STATUS$   = ""
!
    If EP.M.LEN% >= 9 Then \
      EP.DISP.MESSAGE$ = MID$(EP.AMJ.MESSAGE$,9,40)
    If EP.M.LEN% >= 49 Then \
      EP.DISP.PARAM$ = MID$(EP.AMJ.MESSAGE$,49,1)
    If EP.M.LEN% >= 50 Then \
      EP.DISP.OPER$ = MID$(EP.AMJ.MESSAGE$,50,1)
    If EP.M.LEN% >= 51 Then \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,51,2)
End Sub
!
Sub EP.PARSE.PRT.HEADER2 PUBLIC
!
    EP.PRT.MESSAGE$ = ""
    EP.PRT.PARAM$   = ""
    EP.PRT.CUST$    = ""
    EP.PRT.CUT$     = ""
    EP.APPL.STATUS$  = ""
!
    If EP.M.LEN% >= 9 Then \
      EP.PRT.MESSAGE$ = MID$(EP.AMJ.MESSAGE$,9,38)
    If EP.M.LEN% >= 47 Then \
      EP.PRT.PARAM$ = MID$(EP.AMJ.MESSAGE$,47,1)
    If EP.M.LEN% >= 48 Then \
      EP.PRT.CUST$ = MID$(EP.AMJ.MESSAGE$,48,1)
    If EP.M.LEN% >= 49 Then \
      EP.PRT.CUT$ = MID$(EP.AMJ.MESSAGE$,49,1)
    If EP.M.LEN% >= 50 Then \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,50,2)
End Sub
!
Sub EP.PARSE.PRT.LINE2 PUBLIC
!
    EP.PRT.MESSAGE$ = ""
    EP.PRT.PARAM$   = ""
    EP.APPL.STATUS$  = ""
    EP.PRT.CUST$  = ""
    EP.PRT.VOUC$  = ""
!
    If EP.M.LEN% >= 9 Then \
      EP.PRT.MESSAGE$ = MID$(EP.AMJ.MESSAGE$,9,38)
    If EP.M.LEN% >= 47 Then \
      EP.PRT.PARAM$ = MID$(EP.AMJ.MESSAGE$,47,1)
    If EP.M.LEN% >= 48 Then \
      EP.PRT.CUST$ = MID$(EP.AMJ.MESSAGE$,48,1)
    If EP.M.LEN% >= 49 Then \
      EP.PRT.VOUC$ = MID$(EP.AMJ.MESSAGE$,49,1)
    If EP.M.LEN% >= 50 Then \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,50,2)
End Sub
!
Sub EP.PARSE.PRT.CLOSE2 PUBLIC
!
    EP.PRT.MESSAGE$ = ""
    EP.PRT.PARAM$   = ""
    EP.PRT.CUT$     = ""
    EP.APPL.STATUS$  = ""
!
    If EP.M.LEN% >= 9 Then \
      EP.PRT.MESSAGE$ = MID$(EP.AMJ.MESSAGE$,9,38)
    If EP.M.LEN% >= 47 Then \
      EP.PRT.PARAM$ = MID$(EP.AMJ.MESSAGE$,47,1)
    If EP.M.LEN% >= 48 Then \
      EP.PRT.CUT$ = MID$(EP.AMJ.MESSAGE$,48,1)
    If EP.M.LEN% >= 49 Then \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,49,2)
End Sub
!
!
Sub EP.PARSE.DATA.REQUEST   PUBLIC
!
    EP.DISP.MESSAGE$  = ""
    EP.INI.RANGE$     = ""
    EP.END.RANGE$     = ""
    EP.APPL.STATUS$   = ""
!
    If EP.M.LEN% >= 10 Then \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,10,2)
    If EP.M.LEN% >= 12 Then \
      EP.DISP.MESSAGE$ = MID$(EP.AMJ.MESSAGE$,12,40)
    If EP.M.LEN% >= 52 Then \
      EP.INI.RANGE$ = MID$(EP.AMJ.MESSAGE$,52,20)
    If EP.M.LEN% >= 72 Then \
      EP.END.RANGE$ = MID$(EP.AMJ.MESSAGE$,72,20)
!      
End Sub
!
!
Sub EP.PARSE.DATA.REQUEST2 PUBLIC
!
    EP.DISP.MESSAGE$ = ""
    EP.INI.RANGE$     = ""
    EP.END.RANGE$     = ""
    EP.APPL.STATUS$   = ""
!
    If EP.M.LEN% >= 9 Then \
      EP.DISP.MESSAGE$ = MID$(EP.AMJ.MESSAGE$,9,40)
    If EP.M.LEN% >= 49 Then \
      EP.INI.RANGE$ = MID$(EP.AMJ.MESSAGE$,49,10)
      EP.INI.RANGE$ = STR$(VAL(EP.INI.RANGE$))
    If EP.M.LEN% >= 59 Then \
      EP.END.RANGE$ = MID$(EP.AMJ.MESSAGE$,59,10)
      EP.END.RANGE$ = STR$(VAL(EP.END.RANGE$))
    If EP.M.LEN% >= 69 Then \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,69,2)
End Sub
!
Sub EP.PARSE.DENTRY.REQUEST2 PUBLIC
!
    EP.DE1.DATA$ = ""
    EP.DE2.DATA$ = ""
    EP.DE3.DATA$ = ""
    EP.DE4.DATA$ = ""
    EP.DE5.DATA$ = ""
    EP.DE6.DATA$ = ""
    EP.APPL.STATUS$ = ""
!
    If EP.M.LEN% >= 9 Then \
      EP.DE1.DATA$ = MID$(EP.AMJ.MESSAGE$,9,12)
    If EP.M.LEN% >= 21 Then \
      EP.DE2.DATA$ = MID$(EP.AMJ.MESSAGE$,21,20)
    If EP.M.LEN% >= 41 Then \
      EP.DE3.DATA$ = MID$(EP.AMJ.MESSAGE$,41,20)
    If EP.M.LEN% >= 61 Then \
      EP.DE4.DATA$ = MID$(EP.AMJ.MESSAGE$,61,20)
    If EP.M.LEN% >= 81 Then \
      EP.DE5.DATA$ = MID$(EP.AMJ.MESSAGE$,81,20)
    If EP.M.LEN% >= 101 Then \
      EP.DE6.DATA$ = MID$(EP.AMJ.MESSAGE$,101,20)
    If EP.M.LEN% >= 121 Then \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,121,2)
End Sub
!
Sub EP.PARSE.DENTRY99.REQUEST2 PUBLIC
!
    EP.DE1.DATA$   = ""
    EP.DE2.DATA$   = ""
    EP.APPL.STATUS$ = ""
!
    If EP.M.LEN% >= 9 Then \
      EP.DE1.DATA$ = MID$(EP.AMJ.MESSAGE$,9,12)
    If EP.M.LEN% >= 21 Then \
      EP.DE2.DATA$ = MID$(EP.AMJ.MESSAGE$,21,140)
    If EP.M.LEN% >= 161 Then \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,161,2)

End Sub

Sub EP.REVERSE.TRX EXTERNAL
End Sub

!
Sub EPAY.REVERSE.TRX PUBLIC
!

  If EP.TRX.PEND$ <> "" AND \
    (EP.REV.FUNCTION$ = "15" OR \
     EP.REV.FUNCTION$ = "13" ) Then \
  Begin 
    EP.REV.APPL$ = "01"
    EP.MESSAGE$ = Right$(String$(6,"0") + TS.TERMINAL$,6) + EP.TRX.PEND$ + EP.FECHA.PEND$ + \
                  Right$(String$(4,"0") + EP.CHAIN$,4) + \
                  Right$(String$(4,"0") + TS.STORE$ ,4)
    EP.MSGLEN$  = Right$(String$(3,"0") + Str$(LEN(EP.MESSAGE$)),3)
    EP.MESSAGE$ = EP.REV.APPL$ + EP.REV.FUNCTION$ + EP.MSGLEN$ + EP.MESSAGE$
    GoSub SEND.REVERSO.TO.AMJ
    EP.M.LEN% = LEN(EP.AMJ.MESSAGE$)
    If EP.M.LEN% >= 8 Then \
    Begin
      EP.REV.STATUS$ = MID$(EP.AMJ.MESSAGE$,8,1)
    EndIf \
    Else \
    Begin
      EP.REV.STATUS$ = "0"
    EndIf
    If EP.REV.STATUS$ = "0" OR   \
       EP.REV.STATUS$ = "1" Then \
    Begin
      Exit Sub
    EndIf
!       
    If EP.REV.STATUS$ = "9" OR   \
       EP.REV.STATUS$ = "A" Then \
    Begin
      GOSub PARSE.REVERSO.RESPONSE
      EP.REVTRX.STATUS$ = MID$(EP.DE6.DATA$,17,1)
      EP.REV.APROB$ = MID$(EP.DE2.DATA$,19,2) 
      Call EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$, EP.DE4.DATA$, \
           EP.DE5.DATA$, EP.DE6.DATA$)
      If EP.REV.APROB$ = "C4" Then \     ! no hay trx disponible en host para reversar
      Begin 
        EP.REV.COUNT% = EP.REV.COUNT% + 1
        If EP.REV.COUNT% >= 3 Then \
        Begin 
          EP.TOHOST.FOUND% = -1
          EP.TRX.PEND$ = "" 
          EP.REV.FUNCTION$ = ""
          EP.FECHA.PEND$ = ""
          EP.REV.COUNT% = 0
          EP.REV.APROB$ = ""
        EndIf  
      EndIf \
      Else \
      If EP.REVTRX.STATUS$ = "0" Then  \
      Begin
        EP.TOHOST.FOUND% = -1
        EP.TRX.PEND$ = "" 
        EP.REV.FUNCTION$ = ""
        EP.FECHA.PEND$ = ""
        EP.REV.COUNT% = 0
        EP.REV.APROB$ = ""
      EndIf  
    EndIf   
  EndIf
  
  Exit Sub
  
SEND.REVERSO.TO.AMJ:
    
    LOCATE #30;1,1,OFF																		   					! Posiciona cursor
    
    !Write FORM "C40";#30;EP.GUIDE.MESSAGE$(40)                        ! Envia mensake 
    
    EP.METHOD$      = "runOther"
    EP.CLASS.GRAL$  = "com.appl.ApplKernel"
    EP.REQUEST$     ="C$"
    EP.RUNMETH$     ="appl.ApplManager.solicitudReverso"
    EP.EXCEPTION$   = ""
    EP.RETURNVALUE$ = ""
    EP.AMJ.MESSAGE$ = ""
!
    Write FORM "2C20";#30;"Generando Anulacion", "Movimiento... "

    Call JavaCall.Initialize.Request(EP.CLASS.GRAL$,EP.METHOD$,EP.REQUEST$)
    Call JavaCall.AddParameter.String(EP.REQUEST$,EP.MESSAGE$)
    Call JavaCall.AddParameter.String(EP.REQUEST$,EP.RUNMETH$)
    EP.EXCEPTION$=""
    Call JavaCall.InvokeMethod.RETURNString(EP.REQUEST$,EP.RETURNVALUE$,\
        EP.EXCEPTION$)

    If LEN(EP.EXCEPTION$) > 0 Then \
    Begin
      Call EP.LINE.PRINT("Error en reverso",4100H)
      Call EP.LINE.PRINT(LEFT$(EP.EXCEPTION$,38),4100H)
      EP.ERRNCODE% = 1
    EndIf \
    Else \
    Begin
      !Call EP.LINE.PRINT("Rta Rev "+EP.RETURNVALUE$,4100H)
      EP.ERRNCODE% = 0
      EP.AMJ.MESSAGE$ = EP.RETURNVALUE$
    EndIf
Return 

PARSE.REVERSO.RESPONSE:
!
    EP.DE1.DATA$ = ""
    EP.DE2.DATA$ = ""
    EP.DE3.DATA$ = ""
    EP.DE4.DATA$ = ""
    EP.DE5.DATA$ = ""
    EP.DE6.DATA$ = ""
!
    If EP.M.LEN% >= 9 Then \
      EP.DE1.DATA$ = MID$(EP.AMJ.MESSAGE$,9,12)
    If EP.M.LEN% >= 21 Then \
      EP.DE2.DATA$ = MID$(EP.AMJ.MESSAGE$,21,20)
    If EP.M.LEN% >= 41 Then \
      EP.DE3.DATA$ = MID$(EP.AMJ.MESSAGE$,41,20)
    If EP.M.LEN% >= 61 Then \
      EP.DE4.DATA$ = MID$(EP.AMJ.MESSAGE$,61,20)
    If EP.M.LEN% >= 81 Then \
      EP.DE5.DATA$ = MID$(EP.AMJ.MESSAGE$,81,20)
    If EP.M.LEN% >= 101 Then \
      EP.DE6.DATA$ = MID$(EP.AMJ.MESSAGE$,101,20)
!      
RETURN
!
End Sub

!
!
Sub EPAY.REVERSE.ANULTTL PUBLIC
!
  EP.TRX.PEND$     = EP.EPAY.TRANSNUM$
  EP.FECHA.PEND$   = EP.ECR.DATETIME$
  EP.REV.FUNCTION$ = "13"
  Call EPAY.REVERSE.TRX
!
End Sub
!
!
Sub EP.MSR.TEST(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
  EP.USER.DATA$) PUBLIC
!
  
! tpv application interface data
!
  String EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
    EP.USER.DATA$
  EP.AMJ.STATUS$  = "2"
  EP.TRX.STATUS$  = "0"

  Call EP.SAVE.KEYS
  EP.KEYB.DATA$ = ""
  EP.MSR.DATA$  = ""
  Call EP.GET.TRX.TYPE(EP.GUIDE.MESSAGE$(32),EP.MSR.DATA$,EP.KEYB.DATA$)
!
  If TS.IO.MOTORKEY = 73 Then \
  Begin
    EP.AMJ.STATUS$ = "2"
    EP.TRX.STATUS$ = "K"
    Call EP.RESTORE.KEYS
    Exit Sub
  EndIf
!
  Call EP.RESTORE.KEYS
!
  If EP.KEYB.DATA$ = "1" Then \
    EP.FUNCTION$ = "40"       \
  Else \
  If EP.KEYB.DATA$ = "2" Then \
    EP.FUNCTION$ = "41"       \
  Else \
  If EP.MSR.DATA$ <> ""  Then \
    EP.FUNCTION$ = "42"       \
  Else \   
  Begin
    EP.AMJ.STATUS$ = "2"
    EP.TRX.STATUS$ = "J"
    Call EP.RESTORE.KEYS
    Exit Sub
  EndIf 
  EP.ECR.FUNCTION$ = EP.FUNCTION$
  EP.ECR.DATETIME$ = DATE$ + TIME$
  EP.STATE%         = 1
  EP.EXCEPTION$     = ""
  EP.KEYB.DATA$     = ""
  EP.USER.DATA$     = ""
!
  If EP.FUNCTION$ = "42" Then \
    EP.USER.DATA$ = EP.MSR.DATA$
  EP.MSR.DATA$      = ""
!
  EP.MESSAGE$ = RIGHT$(String$(6,"0")+ TS.TERMINAL$,6) + "000000" + EP.ECR.DATETIME$ + \
      "00" + EP.CHAIN$ + RIGHT$(String$(4,"0") + TS.STORE$,4) + EP.USER.DATA$  
  EP.MSGLEN$ = RIGHT$(String$(3,"0") + STR$(LEN(EP.MESSAGE$)),3)
  EP.MESSAGE$ = EP.APPL$ + EP.FUNCTION$ + EP.MSGLEN$ + EP.MESSAGE$
  Call EP.SEND.MSG.TO.AMJ
  While (EP.STATE% < 4) AND (EP.EXCEPTION$ = "")
    EP.M.LEN% = LEN(EP.AMJ.MESSAGE$)
    If EP.M.LEN% < 8 Then \
    Begin
      EP.TRX.STATUS$ = "0"
      EP.AMJ.STATUS$ = "1"
      Exit Sub
    EndIf \
    Else \
    Begin
      EP.AMJ.STATUS$ = MID$(EP.AMJ.MESSAGE$,8,1)
    EndIf 
    If EP.AMJ.STATUS$ = "0" OR   \
       EP.AMJ.STATUS$ = "1" Then \
    Begin
      Exit Sub
    EndIf
!     
    If EP.AMJ.STATUS$ = "2" Then \        ! Transaccion con respuesta de host concluida
    Begin
      If EP.M.LEN% < 9 Then \
      Begin
        EP.TRX.STATUS$ = "1" 
        Exit Sub
      EndIf
      EP.TRX.STATUS$ = MID$(EP.AMJ.MESSAGE$,9,1)
      Call EP.PARSE.MSRTEST.REQUEST
      EP.STATE% = 4
      If EP.TRX.STATUS$ <> "0" Then \
      Begin
        EP.STATE% = 4
        Exit Sub        
      EndIf
    EndIf \
    Else  \                          !  Otros estados de transaccion 
    Begin
      If EP.AMJ.STATUS$ = "3" Then \
      Begin
        Call EP.PARSE.DISPLAY.REQUEST
        Call EP.DISPLAY.A.MESSAGE(EP.DISP.MESSAGE$)
      EndIf   
    EndIf
    If EP.AMJ.STATUS$ = "7" Then \
    Begin 
      Call EP.PARSE.DATA.REQUEST
      Call EP.SAVE.KEYS
      EP.KEYB.DATA$ = ""
      Call EP.GET.KBDATA(EP.DISP.MESSAGE$, EP.INI.RANGE$, EP.END.RANGE$, \
        EP.KEYB.DATA$)
      Call EP.RESTORE.KEYS
    EndIf
    If  (EP.STATE% < 4) AND (EP.EXCEPTION$ = "") Then \   ! Si se continua el ciclo
    Begin    
      EP.MESSAGE$ = RIGHT$(String$(6,"0")+ TS.TERMINAL$,6) + "000000" + EP.ECR.DATETIME$ + \
        EP.APPL.STATUS$
      If EP.AMJ.STATUS$ = "7" Then \
        EP.MESSAGE$ = EP.MESSAGE$ + EP.KEYB.DATA$
      EP.MSGLEN$  = RIGHT$(String$(3,"0") + STR$(LEN(EP.MESSAGE$)),3)
      EP.MESSAGE$ = EP.APPL$ + EP.FUNCTION$ + EP.MSGLEN$ + EP.MESSAGE$
      Call  EP.SEND.MSG.TO.AMJ
    EndIf
  Wend
!
End Sub
!
!
Sub SELECT.VOUCHER(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
  EP.ECR.TRANSNUM$, EP.APPROV.CODE$, EP.APPROV.DESC$, EP.USER.DATA$) PUBLIC
!
  String EP.ERRFX$
!
! tpv application interface data
!
  String EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
    EP.ECR.TRANSNUM$, EP.APPROV.CODE$, EP.APPROV.DESC$, EP.USER.DATA$
!
! DATA INITIALIZATION
!
  EP.FECHA.INIC$  = DATE$ + TIME$
  EP.AMJ.STATUS$  = "2"
  EP.TRX.STATUS$  = "0"
  EP.COMERCIO$    = ""
  EP.EPAY.TERMINAL$=""
  EP.CUOTAS.QTY$  = ""
  EP.FECHA.POSTEO$= ""
  EP.FECHA.PROC$  = ""
  EP.COD.PROC$    = ""
  EP.CENTRAL.CHEQUE$ = ""
  EP.TIPO.ID$     = ""
  EP.ID.GIRADOR$  = ""
  EP.TEL.GIRADOR$ = ""
  EP.COD.BANCO$   = ""
  EP.CTA.CTE$     = ""
  EP.NRO.CHEQUE$  = ""
  EP.RRN$         = ""
  EP.FRANQUICIA$  = ""
  EP.BANCO$       = ""
  EP.PRODUCTO$    = ""
  EP.USER.DATA$   = ""
  EP.GOOD.END%    = 0
  EP.STATE%       = 1
  EP.EXCEPTION$   = ""
  EP.MSR.DATA$    = ""
  EP.KEYB.DATA$   = ""
  EP.REPRINT% = -1

  Call EP.SAVE.KEYS
  EP.KEYB.DATA$ = "1"
 ! Call EP.GET.KBDATA(EP.GUIDE.MESSAGE$(22),"1" , "3", \
 ! EP.KEYB.DATA$)
!
  If TS.IO.MOTORKEY = 73 OR       \
     VAL(EP.KEYB.DATA$) <> 1 Then \
  Begin
    EP.AMJ.STATUS$ = "2"
    EP.TRX.STATUS$ = "K"
    Call EP.RESTORE.KEYS
    Exit Sub
  EndIf
!
!
  If EP.KEYB.DATA$ = "3" Then \
  Begin
    EP.TRX.STATUS$ = "0"
    EP.AMJ.STATUS$ = "2"
    EP.REPRINT%  = -1
    EP.APPROV.CODE$ = "00"
!    Call PRINT.EFT.HEADER.CUT
!    Call EPAY.VOUCHER
    Exit Sub
  EndIf
!
  If EP.KEYB.DATA$ = "1" Then \
    EP.FUNCTION$ = "19" \
  Else \
  If EP.KEYB.DATA$ = "2" Then \
    EP.FUNCTION$ = "20" 

  EP.ECR.FUNCTION$ = EP.FUNCTION$  
!
  Call EP.RESTORE.KEYS
!
  EP.KEYB.DATA$ = ""
  Call EP.GET.KBDATA(EP.GUIDE.MESSAGE$(23),"1" , "999999", \
  EP.KEYB.DATA$)
  If TS.IO.MOTORKEY = 73 Then \
  Begin
    EP.AMJ.STATUS$   = "2"
    EP.TRX.STATUS$   = "K"
    Call EP.RESTORE.KEYS
    Exit Sub
  EndIf
  EP.ECR.TRANSNUM$ = RIGHT$(String$(6,"0") + EP.KEYB.DATA$ ,6)
!
  EP.DE1102.FOUND% = 0
  EP.DE1103.FOUND% = 0
  EP.DE1203.FOUND% = 0
  EP.APPROV.FOUND% = 0
  EP.EFT.FOUND% = 0
  TS.ER.RETURN = -1
  Open "R::EFTTRX" KEYED RECL 158 AS EP.IOPARM% NOWRITE NODEL
  If NOT TS.ER.RETURN  Then \   ! si no abre
  Begin 
    EP.AMJ.STATUS$ = "2"
    EP.TRX.STATUS$ = "V"
    Exit Sub
  EndIf
  If EP.FUNCTION$ = "19" Then \
  Begin 
    EP.KEY$ = RIGHT$(String$(4,"0")+TS.TERMINAL$,4) +   \
      "0111"+ EP.ECR.TRANSNUM$ + "02"
    EP.SAVE.KEY$ = EP.KEY$
    TS.ER.RETURN = -1 
    Read Form "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
      KEY EP.KEY$ ;                                         \
      EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,      \
      EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$, EP.A$, EP.A$
    If NOT TS.ER.RETURN Then \  ! si no existe
    Begin 
      EP.KEY$ = RIGHT$(String$(4,"0")+TS.TERMINAL$,4) +   \
        "0112"+ EP.ECR.TRANSNUM$ + "02"
      EP.SAVE.KEY$ = EP.KEY$
      TS.ER.RETURN = -1 
      Read Form "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
        KEY EP.KEY$ ;                                         \
        EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,      \
        EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$, EP.A$, EP.A$
      If NOT TS.ER.RETURN Then \  ! si no existe
      Begin  
        EP.KEY$ = RIGHT$(String$(4,"0")+TS.TERMINAL$,4) +      \
          "0114"+ EP.ECR.TRANSNUM$ + "02"
        EP.SAVE.KEY$ = EP.KEY$
        TS.ER.RETURN = -1 
        Read Form "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
          KEY EP.KEY$ ;                                         \
          EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,      \
          EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$, EP.A$, EP.A$
        If NOT TS.ER.RETURN Then \  ! si no existe
        Begin  
          EP.AMJ.STATUS$ = "2"
          EP.TRX.STATUS$ = "O"
          CLOSE EP.IOPARM%
          Exit Sub
        EndIf
      EndIf
    EndIf 
  EndIf \
  Else \
  Begin
    EP.KEY$ = RIGHT$(String$(4,"0")+TS.TERMINAL$,4) + \
       "0121"+ EP.ECR.TRANSNUM$ + "02" 
    TS.ER.RETURN = -1 
    Read Form "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
      KEY EP.KEY$ ;                                         \
      EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,      \
      EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$, EP.A$, EP.A$
    If NOT TS.ER.RETURN Then \  ! si no existe
    Begin 
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "O"
      CLOSE EP.IOPARM%
      Exit Sub
    EndIf 
  EndIf

  If EP.FUNCTION$ = "19" Then \    ! Es transaccion de tarjeta
  Begin 
    EP.DE1102.FOUND% = -1
    EP.DE.ISOCOD$ = MID$(EP.DE2.DATA$,19,2)
    EP.DE.POSTEO$ = LEFT$(EP.DE3.DATA$,4)
    EP.DE.CODPRO$ = MID$(EP.DE3.DATA$,5,6)
    EP.DE.CARD$   = MID$(EP.DE3.DATA$,11,4)
    EP.DE.AUTH$   = MID$(EP.DE3.DATA$,15,6)
    EP.DE.AMOUNT$ = LEFT$(EP.DE4.DATA$,10)
    EP.DE.IVA$    = MID$(EP.DE4.DATA$,11,10)
    EP.DE.PROPINA$ = "00" + MID$(EP.DE5.DATA$,13,8)
    EP.DE.RRN$    = LEFT$(EP.DE6.DATA$,12)
    EP.DE.CUOTAS$ = MID$(EP.DE6.DATA$,13,2)  
    EP.DE.TIPOVAR$ = MID$(EP.DE6.DATA$,15,2)
    If EP.DE.ISOCOD$ = "00" OR  \
       EP.DE.ISOCOD$ = "08" OR  \
       EP.DE.ISOCOD$ = "11" OR  \
       EP.DE.ISOCOD$ = "76" OR  \
       EP.DE.ISOCOD$ = "77" OR  \
       EP.DE.ISOCOD$ = "78" OR  \
       EP.DE.ISOCOD$ = "79" OR  \
       EP.DE.ISOCOD$ = "80" OR  \
       EP.DE.ISOCOD$ = "81" Then  \
    Begin
      EP.APPROV.FOUND% = -1
    EndIf \
    Else \
    Begin
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "T" 
      CLOSE EP.IOPARM%
      Exit Sub
    EndIf
  EndIf \                               ! fin de transaccion de tarjeta
  Else \
  Begin                                 ! Es transaccion de cheque
    EP.DE.ISOCOD$     = MID$(EP.DE2.DATA$,19,2)
    EP.DE.CENTRAL.CHEQUE$ = MID$(EP.DE3.DATA$,11,1)
    EP.DE.COD.BANCO$  = MID$(EP.DE3.DATA$,13,2) + String$(8," ")
    EP.DE.AUTH$       = MID$(EP.DE3.DATA$,15,6)
    EP.DE.AMOUNT$     = LEFT$(EP.DE4.DATA$,10)
    EP.DE.NRO.CHEQUE$ = MID$(EP.DE4.DATA$,11,9)
    EP.DE.CTA.CTE$    = MID$(EP.DE5.DATA$,8,13)
    EP.DE.RRN$        = LEFT$(EP.DE6.DATA$,12)
    If EP.DE.ISOCOD$  = "00" Then \
    Begin
      EP.APPROV.FOUND% = -1
    EndIf \
    Else \
    Begin
      EP.FUNCTION$     = ""
      EP.ECR.FUNCTION$ = ""
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "T" 
      CLOSE EP.IOPARM%
      Exit Sub
    EndIf
  EndIf                                ! fin de transaccion de cheque

  If EP.FUNCTION$ = "19" Then \
  Begin 
    EP.KEY$ = LEFT$(EP.SAVE.KEY$,14) + "03" 
  EndIf \
  Else \
  Begin
    EP.KEY$ = RIGHT$(String$(4,"0")+TS.TERMINAL$,4) + \
      "0121" + EP.ECR.TRANSNUM$ + "03" 
  EndIf
  TS.ER.RETURN = -1 
  Read Form "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
    KEY EP.KEY$ ;                                         \
    EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,      \
    EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$, EP.A$, EP.A$
  If NOT TS.ER.RETURN Then \  ! si no existe
  Begin 
    EP.AMJ.STATUS$ = "2"
    EP.TRX.STATUS$ = "U" 
    CLOSE EP.IOPARM%
    Exit Sub
  EndIf
!
  EP.DE.APPL$       = MID$(EP.DE1.DATA$,EP.LEN.CLAVE% + 1,2)
  EP.DE.TRANSNUM$   = MID$(EP.DE1.DATA$,EP.LEN.CLAVE% + 5,6) 
  EP.DE.IVA.BASE$   = LEFT$(EP.DE2.DATA$,10)
  EP.DE.CASHBACK$   = MID$(EP.DE2.DATA$,11,10)
  EP.DE.FECHA.PROC$ = LEFT$(EP.DE3.DATA$,12)
  EP.DE.CARD.BIN$   = MID$(EP.DE5.DATA$,13,6)
  EP.DE.FECHA.VENC$ = MID$(EP.DE6.DATA$,13,4) 
  EP.VOUCHER.FOUND% = -1
  EP.CAJERO$        = RIGHT$(String$(10,"0") + UNPACK$(TS.OPER$),10)
!       
  EP.KEY$ = RIGHT$(String$(4,"0") + TS.TERMINAL$,4) +    \
     "0113"+ EP.ECR.TRANSNUM$ + "04"

  TS.ER.RETURN = -1 
  Read Form "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
    KEY EP.KEY$ ;                                         \
    EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,      \
    EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$, EP.A$, EP.A$
!
  If TS.ER.RETURN Then \           ! si existe
  Begin 
    EP.AMJ.STATUS$ = "2"
    EP.TRX.STATUS$ = "U"
    CLOSE EP.IOPARM%
    Exit Sub
  EndIf
!
!       
  EP.KEY$ = RIGHT$(String$(4,"0") + TS.TERMINAL$,4) +    \
     "0113"+ EP.ECR.TRANSNUM$ + "05"

  TS.ER.RETURN = -1 
  Read Form "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
    KEY EP.KEY$ ;                                         \
    EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,      \
    EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$, EP.A$, EP.A$
!
  If TS.ER.RETURN Then \           ! si existe
  Begin 
    EP.AMJ.STATUS$ = "2"
    EP.TRX.STATUS$ = "U"
    CLOSE EP.IOPARM%
    Exit Sub
  EndIf
!
!       
  EP.KEY$ = RIGHT$(String$(4,"0") + TS.TERMINAL$,4) +    \
     "0115"+ EP.ECR.TRANSNUM$ + "04"

  TS.ER.RETURN = -1 
  Read Form "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
    KEY EP.KEY$ ;                                         \
    EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,      \
    EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$, EP.A$, EP.A$
! Call ep.line.print("KEY="+EP.KEY$+" ER="+STR$(TS.ER.RETURN),4100H)  
!
  If TS.ER.RETURN Then \           ! si existe
  Begin 
    EP.AMJ.STATUS$ = "2"
    EP.TRX.STATUS$ = "U"
    CLOSE EP.IOPARM%
    Exit Sub
  EndIf
!       
  EP.KEY$ = RIGHT$(String$(4,"0") + TS.TERMINAL$,4) +    \
     "0115"+ EP.ECR.TRANSNUM$ + "05"

  TS.ER.RETURN = -1 
  Read Form "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
    KEY EP.KEY$ ;                                         \
    EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,      \
    EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$, EP.A$, EP.A$
!
  If TS.ER.RETURN Then \           ! si existe
  Begin 
    EP.AMJ.STATUS$ = "2"
    EP.TRX.STATUS$ = "U"
    CLOSE EP.IOPARM%
    Exit Sub
  EndIf
!
  EP.KEY$ = RIGHT$(String$(4,"0") + TS.TERMINAL$,4) +    \
     "0110"+ EP.ECR.TRANSNUM$ + "03"

  TS.ER.RETURN = -1 
  Read Form "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
    KEY EP.KEY$ ;                                         \
    EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,      \
    EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$, EP.A$, EP.A$
!
  If TS.ER.RETURN Then \           ! si existe
  Begin 
    EP.AMJ.STATUS$ = "2"
    EP.TRX.STATUS$ = "L"
    CLOSE EP.IOPARM%
    Exit Sub
  EndIf
!
  CLOSE EP.IOPARM%
End Sub
!    
Sub EP.REPRINT.VOUCHER(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
        EP.ECR.TRANSNUM$, EP.APPROV.CODE$, EP.APPROV.DESC$, EP.USER.DATA$) EXTERNAL  
! tpv application interface data
!
  String EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
    EP.ECR.TRANSNUM$, EP.APPROV.CODE$, EP.APPROV.DESC$, EP.USER.DATA$
!
End Sub
!
Sub REPRINT.VOUCHER(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
        EP.ECR.TRANSNUM$, EP.APPROV.CODE$, EP.APPROV.DESC$, EP.USER.DATA$) Public   
! tpv application interface data
!
  String EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
    EP.ECR.TRANSNUM$, EP.APPROV.CODE$, EP.APPROV.DESC$, EP.USER.DATA$
!
!
  EP.REPRINT% = -1
  EP.STATE% = 0
  EP.EXCEPTION$ = ""
  EP.CAJERO$      = RIGHT$(String$(10,"0") + UNPACK$(TS.OPER$),10)
  If EP.APPROV.FOUND% AND EP.VOUCHER.FOUND% Then \
  Begin 
    If EP.ECR.FUNCTION$ = "19" Then \
    Begin 
      EP.MESSAGE$ = RIGHT$(String$(6,"0")+ TS.TERMINAL$,6)+ EP.ECR.TRANSNUM$ + \
        EP.DE.AUTH$ + EP.DE.CARD.BIN$ + EP.DE.CARD$ + EP.DE.CUOTAS$ +          \
        EP.DE.FECHA.PROC$ + EP.DE.CODPRO$ + EP.DE.RRN$ + EP.DE.FECHA.VENC$ +   \
        EP.DE.AMOUNT$ + EP.DE.IVA$ + EP.DE.IVA.BASE$ + EP.CAJERO$ +            \
        EP.DE.PROPINA$ + EP.DE.CASHBACK$ + EP.CHAIN$ +                         \
        RIGHT$(String$(4,"0") + TS.STORE$,4) + "00"
      EP.MSGLEN$ = RIGHT$(String$(3,"0") + STR$(LEN(EP.MESSAGE$)),3)
      EP.MESSAGE$ = EP.APPL$ + EP.ECR.FUNCTION$ + EP.MSGLEN$ + EP.MESSAGE$
      Call EP.SEND.DBCR.TO.AMJ2
    EndIf \
    Else \
    Begin 
      EP.MESSAGE$ = RIGHT$(String$(6,"0")+ TS.TERMINAL$,6)+ EP.ECR.TRANSNUM$ +    \
      EP.DE.AUTH$ + EP.DE.CENTRAL.CHEQUE$ + EP.DE.COD.BANCO$ + EP.DE.CTA.CTE$ + EP.DE.NRO.CHEQUE$ + \
      EP.DE.FECHA.PROC$ + EP.DE.RRN$ + EP.DE.AMOUNT$ +                           \
      EP.CHAIN$ + RIGHT$(String$(4,"0") + TS.STORE$,4) + "00"
      EP.MSGLEN$ = RIGHT$(String$(3,"0") + STR$(LEN(EP.MESSAGE$)),3)
      EP.MESSAGE$ = EP.APPL$ + EP.ECR.FUNCTION$ + EP.MSGLEN$ + EP.MESSAGE$
      Call EP.SEND.CHEQUE.TO.AMJ2
    EndIf
    While (EP.STATE% < 4) AND (EP.EXCEPTION$ = "")
      EP.M.LEN% = LEN(EP.AMJ.MESSAGE$)
      If EP.M.LEN% < 8 Then \
      Begin
        EP.TRX.STATUS$ = "0"
        EP.AMJ.STATUS$ = "1"
        Exit Sub
      EndIf \
      Else \
      Begin
        EP.AMJ.STATUS$ = MID$(EP.AMJ.MESSAGE$,8,1)
      EndIf 
      If EP.AMJ.STATUS$ = "0" OR   \
         EP.AMJ.STATUS$ = "1" Then \
      Begin
        Exit Sub
      EndIf
 !     
      If EP.AMJ.STATUS$ = "2" Then \        ! Transaccion con respuesta de host concluida
      Begin
        If EP.M.LEN% < 9 Then \
        Begin
          EP.TRX.STATUS$ = "1" 
          Exit Sub
        EndIf
        EP.TRX.STATUS$ = MID$(EP.AMJ.MESSAGE$,9,1)
        EP.RESP.FOUND% = -1
        If EP.FUNCTION$ = "19" Then \
          Call EP.PARSE.DBCR.RESPONSE2 \
        Else \
          Call EP.PARSE.CHEQUE.RESPONSE2
        EP.APPROV.CODE$ = EP.H.APPROV.CODE$
        EP.APPROV.DESC$ = EP.H.APPROV.DESC$
        If EP.TRX.STATUS$ <> "0" Then \
        Begin
          EP.STATE% = 4
          Exit Sub        
        EndIf
        If EP.APPROV.CODE$ = "00" OR  \
           EP.APPROV.CODE$ = "08" OR  \
           EP.APPROV.CODE$ = "11" OR  \
           EP.APPROV.CODE$ = "76" OR  \
           EP.APPROV.CODE$ = "77" OR  \
           EP.APPROV.CODE$ = "78" OR  \
           EP.APPROV.CODE$ = "79" OR  \
           EP.APPROV.CODE$ = "80" OR  \
           EP.APPROV.CODE$ = "81" Then  \
        Begin 
          EP.STATE% = 2
          Call EP.DISPLAY.A.MESSAGE(EP.APPROV.DESC$)
          EP.APPROV.FOUND% = -1
        EndIf \
        Else  \
        Begin
          EP.STATE% = 4
          Exit Sub        
        EndIf
      EndIf \
      Else  \                          !  Otros estados de transaccion 
      Begin
        If EP.AMJ.STATUS$ = "4" Then \
        Begin
          Call EP.PARSE.PRT.HEADER2
          Call EP.STORE.VOUCHER(EP.PRT.MESSAGE$,EP.PRT.CUT$)
          EP.PRT.CUT$ = "0"
          Call EP.STORE.EFTLINE(EP.PRT.MESSAGE$)
        EndIf   
        If EP.AMJ.STATUS$ = "5" Then \
        Begin
          Call EP.PARSE.PRT.LINE2
          If EP.PRT.VOUC$ = "1" Then \
            Call EP.STORE.VOUCHER(EP.PRT.MESSAGE$,EP.PRT.CUT$)
          If EP.PRT.CUST$ = "1" Then \
            Call EP.STORE.EFTLINE(EP.PRT.MESSAGE$)
        EndIf   
        If EP.AMJ.STATUS$ = "6" Then \       !  Transacccion finalizada satisfactoriamente
        Begin
          Call EP.PARSE.PRT.CLOSE2
          Call EP.STORE.VOUCHER(EP.PRT.MESSAGE$,EP.PRT.CUT$)
          EP.AMJ.STATUS$ = "2"               !  Sale como transaccion con respuesta del host
          EP.STATE% = 4                      !   Fin de ciclo  
        EndIf   
      EndIf
      If  (EP.STATE% < 4) AND (EP.EXCEPTION$ = "") Then \   ! Si se continua el ciclo
      Begin    
        EP.MESSAGE$ = EP.APPL.STATUS$
        EP.MSGLEN$  = RIGHT$(String$(3,"0") + STR$(LEN(EP.MESSAGE$)),3)
        EP.MESSAGE$ = EP.APPL$ + EP.FUNCTION$ + EP.MSGLEN$ + EP.MESSAGE$
        If EP.FUNCTION$ = "19" Then \
          Call EP.SEND.DBCR.TO.AMJ2 \
        Else \
          Call  EP.SEND.CHEQUE.TO.AMJ2
      EndIf
    Wend
  EndIf 
End Sub



Function CONVERT.TOHEX$(THE.SUM%)
String CONVERT.TOHEX$
Integer*4 THE.SUM%
IF THE.SUM% > 9 THEN \
   THE.SUM%=THE.SUM%+55 \
ELSE \
   THE.SUM%=THE.SUM%+48
CONVERT.TOHEX$=CHR$(THE.SUM%)
End Function

Function DATOS.LEIDOS.MSR(XDATA$)
Integer*4 XJ%, THE.SUM% 
String XA$, DISPL.DATA$, XDATA$, DATOS.LEIDOS.MSR

For XJ% = 1 to LEN(XDATA$)
  XA$ = MID$(XDATA$,XJ%,1)
  THE.SUM% = ASC(XA$)
  XA$ = CONVERT.TOHEX$(THE.SUM%)
  DISPL.DATA$ = DISPL.DATA$ + XA$
Next XJ%
DATOS.LEIDOS.MSR = DISPL.DATA$
End Function 


!
Sub ACCOUNT.BALANCE(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
  EP.ECR.TRANSNUM$, EP.APPROV.CODE$, EP.APPROV.DESC$, EP.USER.DATA$) PUBLIC
!
  String EP.ERRFX$, X.Msg$
!
! tpv application interface data
!
  String EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$,             \
    EP.TAX.EPAY$, EP.VALOR.EFT$, EP.ECR.TRANSNUM$, EP.APPROV.CODE$,\
    EP.PROPINA$, EP.USER.DATA$, EP.AUTH.NUMBER$, EP.APPROV.DESC$, EP.RRN$,       \
    EP.CARD.NUMBER$, EP.TIPO.VAR$, EP.ECR.NBR$
!
! Transmision control data
!
  If EP.TRX.PEND$ <> "" Then \
  Begin  
    Call EPAY.REVERSE.TRX
    If EP.TRX.PEND$ <> "" Then \
    Begin
      EP.AMJ.STATUS$ = EP.REV.STATUS$
      EP.TRX.STATUS$ = EP.REVTRX.STATUS$
      Exit Sub      
    EndIf
  EndIf
 
  EP.CHAIN$         = "0000"
  EP.CAJERO$ = RIGHT$(String$(10,"0") + UNPACK$(TS.OPER$),10)
  EP.FECHA.INIC$  = DATE$ + TIME$
  EP.IVA.BASE$    = String$(10,"0")
  EP.DONACION$    = String$(10,"0")
  EP.CASHBACK$    = String$(10,"0")
  EP.VALOR.EPAY$  = String$(10,"0")
  EP.TAX.EPAY$    = String$(10,"0")
  EP.PROP.EPAY$   = String$(10,"0")
  EP.CAJERO$      = RIGHT$(String$(10,"0") + UNPACK$(TS.OPER$),10)
  EP.AMJ.STATUS$  = "0"
  EP.TRX.STATUS$  = "0"
  EP.COMERCIO$    = ""
  EP.EPAY.TERMINAL$=""
  EP.CUOTAS.QTY$  = ""
  EP.FECHA.POSTEO$= ""
  EP.FECHA.PROC$  = ""
  EP.COD.PROC$    = ""
  EP.CENTRAL.CHEQUE$ = ""
  EP.TIPO.ID$     = ""
  EP.ID.GIRADOR$  = ""
  EP.TEL.GIRADOR$ = ""
  EP.COD.BANCO$   = ""
  EP.CTA.CTE$     = ""
  EP.NRO.CHEQUE$  = ""
  EP.RRN$         = ""
  EP.FRANQUICIA$  = ""
  EP.BANCO$       = ""
  EP.PRODUCTO$    = ""
  EP.USER.DATA$   = ""
  EP.GOOD.END%    = 0
  EP.STATE%       = 1
  EP.EXCEPTION$   = ""
  EP.MSR.DATA$    = ""
  EP.KEYB.DATA$   = ""
  EP.ANUL.TRANSNUM$ = String$(6,"0")
  EP.ANUL.AUTH$     = String$(6,"0")
  EP.ANUL.CODPRO$   = String$(6,"0")
  EP.ANUL.RRN$      = String$(12,"0")
  EP.ANUL.CUOTAS$   = String$(2,"0")
  EP.ANUL.POSTEO$   = String$(4,"0")
  EP.ANUL.CARD$     = String$(4,"0")
  
!
  Call EP.SAVE.KEYS
  EP.MSR.DATA$ = ""
  EP.KEYB.DATA$ = ""
  Call EP.GET.TRX.TYPE(EP.GUIDE.MESSAGE$(24), EP.MSR.DATA$, EP.KEYB.DATA$)

  !EP.MSR.DATA$ = ASIC.DATOS$(Left$(EP.GUIDE.MESSAGE$(24),20),Right$(EP.GUIDE.MESSAGE$(24),20))  

!
  If TS.IO.MOTORKEY = 73 Then \
  Begin
    EP.AMJ.STATUS$ = "2"
    EP.TRX.STATUS$ = "K"
    Call EP.RESTORE.KEYS
    Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
    TS.IO.MOTORKEY = 73 
    Exit Sub
  EndIf
  
  X.Msg$ = EP.MSR.DATA$
  Call EP.RESTORE.KEYS
  If EP.ECR.FUNCTION$ = "50" Then Begin
    If EP.KEYB.DATA$ = "1"   OR \
       EP.MSR.DATA$ <> ""  Then Begin 
      EP.FUNCTION$   = "50" 
    EndIf Else Begin
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "K"
      Call EP.RESTORE.KEYS
      Exit Sub
    EndIf  
  EndIf
!  
!   
  Call EP.DISPLAY.A.MESSAGE(EP.GUIDE.MESSAGE$(27))
  EP.SAVE.TRANSNUM$ = EP.EPAY.TRANSNUM$ 
  EP.EPAY.TRANSNUM$ = RIGHT$(String$(6,"0")+STR$(VAL(EP.EPAY.TRANSNUM$) + 1),6)
  TE.TR.USERHT$     = PACK$(EP.EPAY.TRANSNUM$)+ MID$(TE.TR.USERHT$,4,7)  
  EP.ECR.TRANSNUM$  = EP.EPAY.TRANSNUM$  !  RETURNs transaction number used
  EP.ECR.DATETIME$  = DATE$ + TIME$   
  
!
 If EP.FUNCTION$ = "50" Then \        ! Transaccion tarjeta
  Begin

    EP.ECR.NBR$  = RIGHT$(STRING$(6,"0")+TS.TERMINAL$,6) + \
                   RIGHT$(STRING$(4,"0")+TS.STORE$,4) ! numero de terminal y tienda  
    EP.USER.DATA$ = EP.MSR.DATA$
    EP.ANUL.CODPRO$ = "000000" 
    EP.MESSAGE$ = Left$(EP.ECR.NBR$,6) + EP.EPAY.TRANSNUM$ + EP.ECR.DATETIME$ + \
      EP.VALOR.EPAY$ + EP.TAX.EPAY$   + EP.IVA.BASE$ + EP.CAJERO$ + EP.PROP.EPAY$ + EP.CASHBACK$ +\                              \
      EP.ANUL.AUTH$ + EP.ANUL.CODPRO$ + EP.ANUL.CUOTAS$ + EP.ANUL.TRANSNUM$ +   \
      EP.ANUL.POSTEO$ + EP.ANUL.CARD$ + EP.CHAIN$ + MID$(EP.ECR.NBR$,7,4) +     \
      X.Msg$
      
    EP.MSGLEN$ = Right$(String$(3,"0") + STR$(LEN(EP.MESSAGE$)),3)
    EP.MESSAGE$ = EP.APPL$ + EP.FUNCTION$ + EP.MSGLEN$ + EP.MESSAGE$
    EP.METHOD$      = "runOther"
    EP.CLASS.GRAL$  = "com.appl.ApplKernel"
    EP.REQUEST$     ="C$"
    EP.RUNMETH$     ="appl.ApplManager.tarjeta"
    EP.EXCEPTION$   =""
    EP.RETURNVALUE$ = ""
    EP.AMJ.MESSAGE$ = ""
    Call JavaCall.Initialize.Request(EP.CLASS.GRAL$,EP.METHOD$,EP.REQUEST$)
    Call JavaCall.AddParameter.String(EP.REQUEST$,EP.MESSAGE$)
    Call JavaCall.AddParameter.String(EP.REQUEST$,EP.RUNMETH$)
    EP.EXCEPTION$ = ""
    Call JavaCall.InvokeMethod.RETURNString(EP.REQUEST$,EP.RETURNVALUE$,\
        EP.EXCEPTION$)
    If LEN(EP.EXCEPTION$) > 0 Then Begin
      Call EP.LINE.PRINT("Error en el envio",4100H)
      Call EP.LINE.PRINT(LEFT$(EP.EXCEPTION$,38),4100H)
      EP.ERRNCODE% = 1
      EP.GOOD.END% = 0
    EndIf Else Begin
      EP.ERRNCODE% = 0
      EP.GOOD.END% = -1
      EP.AMJ.MESSAGE$ = EP.RETURNVALUE$
    EndIf    
    
    If Mid$(EP.AMJ.MESSAGE$,8,2) = "20" Then Begin 											! Si operacion al Was OK
     If Mid$(EP.AMJ.MESSAGE$,16,2) = "00" Then Begin 									  ! Si requerimiento Exitoso
       EP.AMJ.MESSAGE$ = Mid$(EP.AMJ.MESSAGE$,18,20)                    ! Toma el valor del saldo
       Call Format.Amount(Val(EP.AMJ.MESSAGE$))
       Call TSHIECET
       Call Visores4690(2,"Saldo Disponible    ","Us$ "+TS.TEMP1$,0,"L")
       TS.TEMP1$ = ASIC.DATOS$("Saldo Disponible    ","Us$ "+TS.TEMP1$)    
       EP.AMJ.MESSAGE$ = ""
     EndIf Else Begin 																								  ! Error Reportado
       EP.AMJ.MESSAGE$ = Mid$(EP.AMJ.MESSAGE$,18,20)                    ! Toma el mensaje de Error
       Call EP.DISPLAY.AN.ERROR(EP.AMJ.MESSAGE$)												! Presenta mensaje
       EP.AMJ.MESSAGE$ = ""																							! Inicializa variable 
     EndIf 
    EndIf Else Begin 
       Call EP.DISPLAY.AN.ERROR(EP.GUIDE.MESSAGE$(28))
    EndIf 
    Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
    TS.IO.MOTORKEY = 73 
  EndIf
  
End Sub
!
!
!-----------------------------------------------------------
!  	recover routines
!
Sub EPAY.START.RECOVER.EFT PUBLIC 
!
!---------------------------------------------------------
  If NOT EP.NO.TRACE% Then \
  Begin
    TS.LINETYPE = 29
    TS.SAVPRT$ = "start recover="+ STR$(EP.MAX.TRX%)
    TS.SAVPRT.OPT = 4100H
    Call TSPREC01
  EndIf
!---------------------------------------------------------
  EP.LINEA% = 0
  Dim EP.VOUCHER$(300)
  Dim EP.CUT.VOUCHER$(300)
  Dim EP.EFT.LINE$(100)
  Dim EP.TRX.APPL$(EP.MAX.TRX%)
  Dim EP.TRX.FUNCTION$(EP.MAX.TRX%)
  Dim EP.TRX.TRANSNUM$(EP.MAX.TRX%)
  Dim EP.TRX.RESP%(EP.MAX.TRX%)
  Dim EP.TRX.APPROV%(EP.MAX.TRX%)
  Dim EP.TRX.VOUCHER%(EP.MAX.TRX%)
  Dim EP.TRX.TOHOST1%(EP.MAX.TRX%) 
  Dim EP.TRX.TOHOST2%(EP.MAX.TRX%)
  Dim EP.TRX.ANUL%(EP.MAX.TRX%)   
  Dim EP.TRX.REVERSO%(EP.MAX.TRX%)
  Dim EP.TRX.PEND%(EP.MAX.TRX%)
  Dim EP.TRX.FECHA$(EP.MAX.TRX%)
  Dim EP.PROMO.ID$(10)
  Dim EP.PROMO.STATUS$(10)
  Dim EP.PROMO.POINTS$(10) 
  Dim EP.PROMO.REDEEM$(10)
  Dim EP.PROMO.AMTPUR$(10)
  Dim EP.PROMO.QTYPUR$(10)     
  EP.CAJERO$ = RIGHT$(String$(10,"0") + UNPACK$(TS.OPER$),10)
  EP.TEF.CTRLDEV% = 0
!
End Sub
!
!
Sub EPAY.RECOVER.EFT PUBLIC 
!
  If TS.TEMP1I2 = 11 Then \
  Begin 
    EP.J% = 3
    EP.B$ = SL.STR.ENTRY$ + ":"
    Call EP.GETUNPK
    If LEN(EP.A$) > EP.LEN.CLAVE% Then  \   ! Valid record
    Begin
      If LEFT$(EP.A$,EP.LEN.CLAVE%) = EP.EFT.CLAVE$ Then \ ! EFT record
      Begin
        EP.DE.APPL$     = MID$(EP.A$,EP.LEN.CLAVE% + 1,2)
        EP.DE.FUNCTION$ = MID$(EP.A$,EP.LEN.CLAVE% + 3,2)
        EP.DE.TRANSNUM$ = MID$(EP.A$,EP.LEN.CLAVE% + 5,6)
        EP.DE.TIPO$     = MID$(EP.A$,EP.LEN.CLAVE% + 11,2)
        If EP.DE.TIPO$ = "01" Then    \
        Begin
          EP.LINEA% = EP.LINEA% + 1
          EP.REV.APPL$ = EP.DE.APPL$ 
          EP.TRX.APPL$(EP.LINEA%) = EP.DE.APPL$
          EP.TRX.TRANSNUM$(EP.LINEA%) = EP.DE.TRANSNUM$
          EP.EPAY.TRANSNUM$ = EP.DE.TRANSNUM$ 
          EP.TRX.FUNCTION$(EP.LINEA%) = EP.DE.FUNCTION$
          Call EP.GETUNPK
          EP.DE2.DATA$ = EP.A$
          Call EP.GETUNPK
          EP.DE3.DATA$ = EP.A$
          Call EP.GETUNPK
          EP.DE4.DATA$ = EP.A$
          Call EP.GETUNPK
          EP.DE5.DATA$ = EP.A$
          Call EP.GETUNPK
          EP.DE6.DATA$ = EP.A$
          EP.DE.ALLCARD$ = EP.DE2.DATA$
          EP.DE.INVOICE.NBR$ =  RIGHT$(String$(6,"0") + MID$(EP.DE6.DATA$,11,6),6)
          EP.TRX.FECHA$(EP.LINEA%) = LEFT$(EP.DE5.DATA$,12)
          EP.APPROV.FOUND% = 0
          EP.VOUCHER.FOUND% = 0
        EndIf     ! end del tipo 01                        
!                      
        If EP.DE.TIPO$ = "02" Then    \
        Begin
          EP.TRX.RESP%(EP.LINEA%) = -1
          Call EP.GETUNPK
          EP.DE2.DATA$ = EP.A$
          EP.DE.ISOCOD$ = MID$(EP.A$,19,2)
          If EP.DE.ISOCOD$ = "00" OR   \
             EP.DE.ISOCOD$ = "08" OR   \
             EP.DE.ISOCOD$ = "11" OR   \
             EP.DE.ISOCOD$ = "76" OR   \
             EP.DE.ISOCOD$ = "77" OR   \
             EP.DE.ISOCOD$ = "78" OR   \
             EP.DE.ISOCOD$ = "79" OR   \
             EP.DE.ISOCOD$ = "80" OR   \
             EP.DE.ISOCOD$ = "81" Then \
          Begin
            EP.TRX.APPROV%(EP.LINEA%) = -1
            EP.APPROV.FOUND% = -1
          EndIf 
          Call EP.GETUNPK
          EP.DE3.DATA$ = EP.A$
          Call EP.GETUNPK
          EP.DE4.DATA$ = EP.A$
          Call EP.GETUNPK
          EP.DE5.DATA$ = EP.A$
          Call EP.GETUNPK
          EP.DE6.DATA$ = EP.A$
          If EP.DE.APPL$ = "01" Then  \
          Begin 
            If EP.DE.FUNCTION$ = "11"  OR \
               EP.DE.FUNCTION$ = "12"  OR \
               EP.DE.FUNCTION$ = "14"  Then \    ! Es transaccion de tarjeta
            Begin
              EP.DE1102.FOUND% = -1
              EP.DE.ISOCOD$ = MID$(EP.DE2.DATA$,19,2)
              EP.DE.POSTEO$ = LEFT$(EP.DE3.DATA$,4)
              EP.DE.CODPRO$ = MID$(EP.DE3.DATA$,5,6)
              EP.DE.CARD$   = MID$(EP.DE3.DATA$,11,4)
              EP.DE.AUTH$   = MID$(EP.DE3.DATA$,15,6)
              EP.DE.AMOUNT$ = LEFT$(EP.DE4.DATA$,10)
              EP.DE.IVA$    = MID$(EP.DE4.DATA$,11,10)
              EP.DE.PROPINA$ = "00" + MID$(EP.DE5.DATA$,13,8)
              EP.DE.RRN$    = LEFT$(EP.DE6.DATA$,12)
              EP.DE.CUOTAS$ = MID$(EP.DE6.DATA$,13,2)
              EP.DE.TIPOVAR$ = MID$(EP.DE6.DATA$,15,2) 
            EndIf \
            Else \
            Begin   
              EP.DE.ISOCOD$     = MID$(EP.DE2.DATA$,19,2)
              EP.DE.CENTRAL.CHEQUE$ = MID$(EP.DE3.DATA$,11,1)
              EP.DE.COD.BANCO$  = MID$(EP.DE3.DATA$,13,2) + String$(8," ")
              EP.DE.AUTH$       = MID$(EP.DE3.DATA$,15,6)
              EP.DE.AMOUNT$     = LEFT$(EP.DE4.DATA$,10)
              EP.DE.NRO.CHEQUE$ = MID$(EP.DE4.DATA$,11,9)
              EP.DE.CTA.CTE$    = MID$(EP.DE5.DATA$,8,13)
              EP.DE.RRN$        = LEFT$(EP.DE6.DATA$,12)
              EP.DE.CUOTAS$     = MID$(EP.DE6.DATA$,13,2)              
              EP.DE.TIPOVAR$    = MID$(EP.DE6.DATA$,15,2)
            EndIf
          EndIf \
          Else \
          Begin
            EP.DE1102.FOUND% = -1
            EP.DE.ISOCOD$ = MID$(EP.DE2.DATA$,19,2)
            EP.DE.POSTEO$ = LEFT$(EP.DE3.DATA$,4)
            EP.DE.CODPRO$ = MID$(EP.DE3.DATA$,5,6)
            EP.DE.CARD$   = MID$(EP.DE3.DATA$,11,4)
            EP.DE.AUTH$   = MID$(EP.DE3.DATA$,15,6)
            EP.DE.AMOUNT$ = LEFT$(EP.DE4.DATA$,10)
            EP.DE.PUNTOS$    = MID$(EP.DE4.DATA$,11,10)
            EP.DE.RRN$    = LEFT$(EP.DE6.DATA$,12)
          EndIf  
        EndIf  ! end del tipo 02
!                                              
        If EP.DE.TIPO$ = "03" Then \
        Begin
          EP.TRX.VOUCHER%(EP.LINEA%) = -1
          EP.VOUCHER.FOUND% = -1
          Call EP.GETUNPK
          EP.DE2.DATA$ = EP.A$
          Call EP.GETUNPK
          EP.DE3.DATA$ = EP.A$
          Call EP.GETUNPK
          EP.DE4.DATA$ = EP.A$
          Call EP.GETUNPK
          EP.DE5.DATA$ = EP.A$
          Call EP.GETUNPK
          EP.DE6.DATA$ = EP.A$
          If EP.DE.APPL$ = "01" Then \
          Begin 
            EP.DE.IVA.BASE$   = LEFT$(EP.DE2.DATA$,10)
            EP.DE.CASHBACK$   = MID$(EP.DE2.DATA$,11,10)
            EP.DE.FECHA.PROC$ = LEFT$(EP.DE3.DATA$,12)
            EP.DE.ANULTRANSNUM$ = LEFT$(EP.DE4.DATA$,6) 
            EP.DE.CARD.BIN$   = MID$(EP.DE5.DATA$,13,6)
            EP.DE.FECHA.VENC$ = MID$(EP.DE6.DATA$,13,4)
          EndIf \
          Else \
          Begin
            EP.DE.FECHA.PROC$ = LEFT$(EP.DE3.DATA$,12)
            EP.DE.ANULTRANSNUM$ = LEFT$(EP.DE4.DATA$,6) 
            EP.DE.CARD.BIN$   = MID$(EP.DE5.DATA$,13,6)
            EP.DE.FECHA.VENC$ = MID$(EP.DE6.DATA$,13,4)
          EndIf
          If (EP.DE.APPL$ = "01" AND EP.DE.FUNCTION$ = "12")  OR \   !  anula en la misma transaccion
             (EP.DE.APPL$ <> "01" AND \
             (EP.DE.FUNCTION$ = "04"  OR EP.DE.FUNCTION$ = "05"))  Then \ !  anula en la misma transaccion
          Begin
            EP.EFT.FOUND% = 0
            EP.POS% = 1
            While NOT EP.EFT.FOUND%  AND EP.POS% <= EP.MAX.TRX% 
              If EP.TRX.TRANSNUM$(EP.POS%) = EP.DE.ANULTRANSNUM$ Then \
              Begin
                EP.EFT.FOUND% = -1
              EndIf \
              Else \
              Begin
                EP.POS% = EP.POS% + 1
              EndIf
            Wend
            If EP.EFT.FOUND% Then \
            Begin
              EP.TRX.ANUL%(EP.POS%) = -1 
            EndIf
          EndIf   ! end anula en la misma trx
        EndIf    ! end del tipo 03
!    AQUI VA EL Call A REPRINT             
        If EP.APPROV.FOUND% AND EP.VOUCHER.FOUND% AND \
             NOT TS.RETV.IN.PROGRESS Then \
        Begin  
          EP.FUNCTION$ = "00"
          If EP.DE.APPL$ = "01" Then \
          Begin 
            If EP.DE.FUNCTION$ = "11"  OR \
               EP.DE.FUNCTION$ = "12"  OR \
               EP.DE.FUNCTION$ = "14"  Then \    ! Es transaccion de tarjeta
              EP.FUNCTION$ = "19" \
            Else \
              EP.FUNCTION$ = "20"
            EP.TAX.EPAY$       = EP.DE.IVA$
  !            EP.TAX.EFT$        = EP.DE.IVA$
            EP.IVA.BASE$       = EP.DE.IVA.BASE$
            EP.PROP.EPAY$      = EP.DE.PROPINA$
            EP.CASHBACK$       = EP.DE.CASHBACK$
          EndIf \
          Else  \
          Begin 
            If EP.DE.FUNCTION$ = "02"  Then \
              EP.FUNCTION$ = "10" \
            Else \
            If EP.DE.FUNCTION$ = "03"  Then \
              EP.FUNCTION$ = "11" \
            Else \
            If EP.DE.FUNCTION$ = "04"  Then \
              EP.FUNCTION$ = "12" \
            Else \
            If EP.DE.FUNCTION$ = "05"  Then \    ! Es transaccion de tarjeta
              EP.FUNCTION$ = "13" 
          EndIf
          EP.CAJERO$         = RIGHT$(String$(10,"0") + UNPACK$(TS.OPER$),10)
          EP.EFT.TRX%        = -1
          EP.EFT.DETAIL%     = -1
          EP.EFT.SMA%        = -1
          EP.DE.TRX.STATUS$  = ""
          EP.DE.AMJ.STATUS$  = ""
          EP.DE.APPROV.CODE$ = ""
          EP.DE.APPROV.DESC$ = ""
          EP.DE.USER.DATA$   = ""  
          If EP.DE.APPL$ = "01" Then \
           Call REPRINT.VOUCHER(EP.DE.APPL$, EP.FUNCTION$, EP.DE.TRX.STATUS$, EP.DE.AMJ.STATUS$, \
               EP.DE.TRANSNUM$, EP.DE.APPROV.CODE$, EP.DE.APPROV.DESC$, EP.DE.USER.DATA$)  \
          Else \
           Call EP.REPRINT.VOUCHER(EP.DE.APPL$, EP.FUNCTION$, EP.DE.TRX.STATUS$, EP.DE.AMJ.STATUS$, \
               EP.DE.TRANSNUM$, EP.DE.APPROV.CODE$, EP.DE.APPROV.DESC$, EP.DE.USER.DATA$)  
                
        EndIf   ! end del reprint
!                          
        If EP.DE.TIPO$ = "04"  OR \       ! respuesta TO de appl manager
           EP.DE.TIPO$ = "05" Then \      ! reversos
        Begin
          EP.EFT.FOUND% = 0
          EP.POS% = 1
          While NOT EP.EFT.FOUND%  AND EP.POS% <= EP.MAX.TRX% 
            If EP.TRX.TRANSNUM$(EP.POS%) = "" Then \
            Begin
              EP.LINEA% = EP.POS%
              EP.TRX.TRANSNUM$(EP.POS%) = EP.DE.TRANSNUM$
              EP.EFT.FOUND% = -1
            EndIf \
            Else \
            If EP.TRX.TRANSNUM$(EP.POS%) = EP.DE.TRANSNUM$ Then \
            Begin
              EP.EFT.FOUND% = -1
            EndIf \
            Else  \
            Begin
              EP.POS% = EP.POS% + 1
            EndIf
          Wend
          If EP.EFT.FOUND% Then \    ! reverso found
          Begin 
            Call EP.GETUNPK
            Call EP.GETUNPK
            Call EP.GETUNPK
            Call EP.GETUNPK
            Call EP.GETUNPK
            If MID$(EP.A$,17,1) = "0" Then \       !  finaliza normalmente
            Begin    
              EP.TRX.REVERSO%(EP.POS%) = 0
              If EP.DE.TIPO$ = "04" Then \
              Begin 
                EP.TRX.TOHOST1%(EP.POS%) = -1
              EndIf \
              Else \
              Begin
                EP.TRX.TOHOST2%(EP.POS%) = -1
              EndIf
            EndIf \
            Else  \
            Begin
              EP.TRX.REVERSO%(EP.POS%) = -1
            EndIf
          EndIf                               ! end reverso found                                                 
        EndIf                                 ! end tipo 04 o 05         
      EndIf                                   ! end EFT record    
    EndIf                                     ! end valid record
  EndIf
  If TS.TEMP1I2 = 05  OR \
     TS.TEMP1I2 = 06  Then \
  Begin 
    If EP.EFT.SMA% Then \
      EP.EFT.SMA% = 0
  EndIf
End Sub
!
!
Sub EPAY.END.RECOVER.EFT PUBLIC

  Dim EP.TRX.PEND%(EP.MAX.TRX%) 
  For EP.M% = 1 TO EP.LINEA% 
    If EP.TRX.TRANSNUM$(EP.M%) <> "" AND  \    ! si hubo transaccion 
       NOT EP.TRX.TOHOST1%(EP.M%)    AND  \    ! si no hay reverso appl manager
       NOT EP.TRX.TOHOST2%(EP.M%)    Then \    ! si no hay reverso host
    Begin
      If EP.TRX.RESP%(EP.M%) Then  \           ! si obtuvo respuesta
      Begin
        If EP.TRX.APPROV%(EP.M%) Then \        ! si fue aprobada
        Begin 
          If NOT EP.TRX.VOUCHER%(EP.M%) Then \ ! si no obtuvo voucher
          Begin
            EP.TRX.PEND%(EP.M%) = -1           ! se marca como pendiente
            EP.TRX.PEND$ = EP.TRX.TRANSNUM$(EP.M%)
            EP.FECHA.PEND$ = EP.TRX.FECHA$(EP.M%)
            If EP.DE.APPL$ = "01"  Then \
              EP.REV.FUNCTION$ = "15" \
            Else \
            Begin
              If EP.TRX.FUNCTION$(EP.M%) = "02"  Then  \        ! deteccion de necesidad de reversos 
                EP.REV.FUNCTION$ = "06"     \        ! 
              Else \
              If EP.TRX.FUNCTION$(EP.M%) = "03"  Then  \        ! 
                EP.REV.FUNCTION$ = "07"     \        ! 
              Else \  
              If EP.TRX.FUNCTION$(EP.M%) = "04" Then   \        ! 
                EP.REV.FUNCTION$ = "08"     \        ! 
              Else \
              If EP.TRX.FUNCTION$(EP.M%) = "05" Then   \        ! 
                EP.REV.FUNCTION$ = "09"              ! 
            EndIf 
          EndIf  
        EndIf  
      EndIf \
      Else \                                   ! si no obtuvo respuesta                                  
      Begin 
        EP.TRX.PEND%(EP.M%) = -1
        EP.TRX.PEND$ = EP.TRX.TRANSNUM$(EP.M%)
        EP.FECHA.PEND$ = EP.TRX.FECHA$(EP.M%)
        If EP.DE.APPL$ = "01"  Then \
          EP.REV.FUNCTION$ = "13" \
        Else \
        Begin
          If EP.TRX.FUNCTION$(EP.M%) = "02"  Then  \        ! deteccion de necesidad de reversos 
            EP.REV.FUNCTION$ = "06"     \        ! 
          Else \
          If EP.TRX.FUNCTION$(EP.M%) = "03"  Then  \        ! 
            EP.REV.FUNCTION$ = "07"     \        ! 
          Else \  
          If EP.TRX.FUNCTION$(EP.M%) = "04" Then   \        ! 
            EP.REV.FUNCTION$ = "08"     \        ! 
          Else \
          If EP.TRX.FUNCTION$(EP.M%) = "05" Then   \        ! 
            EP.REV.FUNCTION$ = "09"              ! 
        EndIf 
      EndIf
    EndIf
  Next EP.M% 
  !
  If EP.TRX.TRANSNUM$(EP.LINEA%) <> "" AND  \    ! si ULTIMA TRANSACCION 
     NOT EP.TRX.TOHOST1%(EP.LINEA%)  AND  \    ! si no hay reverso appl manager
     NOT EP.TRX.TOHOST2%(EP.LINEA%)  Then \    ! si no hay reverso host
  Begin
    If EP.DE.APPL$ = "01" Then \
    Begin
      If EP.TRX.RESP%(EP.LINEA%)    AND \          ! si obtuvo respuesta
         EP.TRX.APPROV%(EP.LINEA%)  AND \          ! si fue aprobada
         EP.TRX.VOUCHER%(EP.LINEA%) AND \          ! si obtuvo voucher
         EP.EFT.SMA% Then \                        ! no ha entrado a Supermarket Application
      Begin
        Call EP.SAVE.KEYS
        Dim TS.IO.KEYS(10)                         ! Clear input
        If EP.DE.FUNCTION$ = "12" OR \
           EP.DE.FUNCTION$ = "14" Then  \
          TS.IO.KEYS(1) = 70
        TS.IO.KEYS(3)  = 78                        ! Asterisk key
        TS.IO.DATA$(3) = RIGHT$(EP.DE.TIPOVAR$,1)  ! Tender variety
        TS.IO.KEYS(7)  = VAL(LEFT$(EP.DE.TIPOVAR$,1)) + 90 ! Tender key
        TS.IO.DATA$(7) = EP.DE.AMOUNT$             ! Tender amount
        TS.IO.DATA$(9) = EP.DE.CARD$ + EP.DE.AUTH$ + \
            EP.TRX.TRANSNUM$(EP.LINEA%)
        TS.IO.KEYS(9)  = 90
  !      If SL.TE.TENDTYPE = 3 Then \ Food stamps so
  !        TS.TRX.STATUS = -1 ! set F.S. bal due
        Call TSTDEC01
        Call EP.RESTORE.KEYS
        TS.TEMP1$ = "TRX CONTABILIZADA!!!"
        TS.TEMP2$ = String$(20," ") 
        TS.LINETYPE = 12
        Call TSCSEC08
      EndIf
      If EP.TRX.RESP%(EP.LINEA%)    AND \          ! si obtuvo respuesta
         EP.TRX.APPROV%(EP.LINEA%)  AND \          ! si fue aprobada
         EP.TRX.VOUCHER%(EP.LINEA%) AND \          ! si obtuvo voucher
         EP.EFT.SMA% Then \                        ! no ha entrado a Supermarket Application
      Begin 
        EP.POINTER2%  = EP.LAST.POINT2% - 1
        EP.EFT.FOUND% = 0
        EP.F% = 0
        EP.M% = EP.POINTER1% 
        While NOT EP.EFT.FOUND% AND EP.M% > 0
          EP.POS% = MATCH("Firma:", EP.VOUCHER$(EP.M%),1)
          If EP.POS% <> 0 Then \
            EP.F% = EP.M%
          EP.VOUCHER$(EP.M%) = "anul" + MID$(EP.VOUCHER$(EP.M%),5,\
            LEN(EP.VOUCHER$(EP.M%)) - 4) 
          EP.M% = EP.M% - 1
          EP.POS% = MATCH(LEFT$(EP.GUIDE.MESSAGE$(18),20),EP.VOUCHER$(EP.M%),1) 
          If EP.POS% <> 0 Then \
            EP.EFT.FOUND% = -1
        Wend
        If EP.F% <> 0 Then \
        Begin
          EP.VOUCHER$(EP.F% - 1) = "ESTA TRANSACCION NO SE REALIZO CORREC-"
          EP.VOUCHER$(EP.F%)     = "TAMENTE POR TAL RAZÓN ESTE VOUCHER NO "
          EP.VOUCHER$(EP.F% + 1) = "ES VALIDO COMO SOPORTE DE UNA TRANSAC-"
          EP.VOUCHER$(EP.F% + 2) = "CIÓN FINANCIERA, POR FAVOR REALICE    "
          EP.VOUCHER$(EP.F% + 3) = "NUEVAMENTE UNA TRANSACCIÓN. ESTA TRAN-"
          EP.VOUCHER$(EP.F% + 4) = "SACCION NO SERA CARGADA AL TARJETAHA- "
          EP.VOUCHER$(EP.F% + 5) = "BIENTE, POR FAVOR CONSERVE ESTE RECIBO"
          EP.VOUCHER$(EP.F% + 6) = "PARA CUALQUIER RECLAMACIÓN            "
        EndIf
        EP.TRX.PEND%(EP.LINEA%) = -1
        EP.TRX.PEND$ = EP.TRX.TRANSNUM$(EP.LINEA%)
        EP.FECHA.PEND$ = EP.TRX.FECHA$(EP.LINEA%)
        EP.REV.FUNCTION$ = "15" 
      EndIf
    EndIf \   ! end aplicacion tef
    Else \    ! applicaciones diferentes a tef
    Begin
      If EP.TRX.RESP%(EP.LINEA%)    AND \          ! si obtuvo respuesta
         EP.TRX.APPROV%(EP.LINEA%)  AND \          ! si fue aprobada
         EP.TRX.VOUCHER%(EP.LINEA%) Then \         ! si obtuvo voucher
      Begin
        If EP.DE.FUNCTION$ = "02" Then \
        Begin 
          EP.I4% = VAL(EP.DE.AMOUNT$)
          EP.I1% = FORMAT.AMOUNT(EP.I4%)
          Call EP.LINE.PRINT("Acumular puntos:Vlr compra="+ \
            RIGHT$(String$(10," ") + TS.TEMP1$, 10), 6100H)
        EndIf \
        Else  \
        If EP.DE.FUNCTION$ = "03" Then \
        Begin 
          EP.I4% = VAL(EP.DE.PUNTOS$)
          EP.I1% = FORMAT.AMOUNT(EP.I4%)
          Call EP.LINE.PRINT("Redencion punts:Ptos redim="+ \
            RIGHT$(String$(10," ") + TS.TEMP1$, 10), 6100H)
        EndIf \
        Else  \
        If EP.DE.FUNCTION$ = "04" Then \
        Begin 
          EP.I4% = VAL(EP.DE.AMOUNT$) * -1
          EP.I1% = FORMAT.AMOUNT(EP.I4%)
          Call EP.LINE.PRINT("Anulacion punts:Vlr compra="+ \
            RIGHT$(String$(10," ") + TS.TEMP1$, 10), 6100H)
        EndIf \
        Else  \
        If EP.DE.FUNCTION$ = "05" Then \
        Begin 
          EP.I4% = VAL(EP.DE.PUNTOS$) * -1
          EP.I1% = FORMAT.AMOUNT(EP.I4%)
          Call EP.LINE.PRINT("Anula redencion:Ptos redim="+ \
            RIGHT$(String$(10," ") + TS.TEMP1$, 10), 6100H)
        EndIf       
        Call EP.LINE.PRINT("Voucher="+EP.DE.TRANSNUM$ + "  Auth="+EP.DE.AUTH$,6200H)

      EndIf
    EndIf
  EndIf  
End Sub
!
!
!-----------------------------------------------------------
!  	Load parameters API routine
!
Sub LOAD.PARM.API(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
  EP.ECR.NBR$, EP.ECR.TRANSNUM$, EP.USER.DATA$) PUBLIC
!
!
! tpv application interface data
!
  String EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
    EP.ECR.NBR$, EP.ECR.TRANSNUM$, EP.USER.DATA$
!
!  ON ERROR GOTO ERRORTRAP                     !! set "ON ERROR"
!  ON ASYNC ERROR Call ASYNC.ERR                !! set "ON ASYNC ERROR"

  EP.EPAY.TRANSNUM$ = RIGHT$(String$(6,"0")+STR$(VAL(EP.EPAY.TRANSNUM$) + 1),6)
  TE.TR.USERHT$ = PACK$(EP.EPAY.TRANSNUM$)+ MID$(TE.TR.USERHT$,4,7)  
  EP.ECR.TRANSNUM$ = EP.EPAY.TRANSNUM$
  EP.TRX.STATUS$ = "0"
  EP.GOOD.END% = 0
  EP.AMJ.STATUS$ = "0"
  Call EP.GET.CURRENT.TIME
  EP.INIT.TRX.TIME% = EP.CURRENT.TIME%
  EP.ECR.DATETIME$  = DATE$ + TIME$
  EP.MESSAGE$ = LEFT$(EP.ECR.NBR$,6) + EP.ECR.TRANSNUM$ + EP.ECR.DATETIME$ + \
                EP.CHAIN$ + MID$(EP.ECR.NBR$,7,4)
  EP.MSGLEN$ = RIGHT$(String$(3,"0")+STR$(LEN(EP.MESSAGE$)),3)
  EP.MESSAGE$ = EP.APPL$+EP.ECR.FUNCTION$+EP.MSGLEN$+EP.MESSAGE$
  GOSub SEND.LOADRQ.TO.AMJ
  If EP.GOOD.END% Then \
    GOSub PARSE.LOADRQ.RESPONSE

  Exit Sub
!
!
SEND.LOADRQ.TO.AMJ:
!
!   Call Java appl manager routine
!
!
  EP.METHOD$      = "runOther"
  EP.CLASS.GRAL$  = "com.appl.ApplKernel"
  EP.REQUEST$     ="C$"
  EP.RUNMETH$     ="appl.ApplManager.carga"
  EP.EXCEPTION$="" 
!
  Call JavaCall.Initialize.Request(EP.CLASS.GRAL$,EP.METHOD$,EP.REQUEST$)
  Call JavaCall.AddParameter.String(EP.REQUEST$,EP.MESSAGE$)
  Call JavaCall.AddParameter.String(EP.REQUEST$,EP.RUNMETH$)
  EP.EXCEPTION$=""
  Call JavaCall.InvokeMethod.RETURNString(EP.REQUEST$,EP.RETURNVALUE$,EP.EXCEPTION$)
!
  If LEN(EP.EXCEPTION$) > 0 Then \
  Begin
    TS.LINETYPE = 29
    TS.SAVPRT$ = "**err="+left$(EP.EXCEPTION$,32)
    TS.SAVPRT.OPT = 4100H
    Call TSPREC01
    EP.ERRNCODE% = 1
    EP.GOOD.END% = 0
  EndIf \
  Else \
  Begin
      EP.ERRNCODE% = 0
      EP.GOOD.END% = -1
      EP.AMJ.MESSAGE$ = EP.RETURNVALUE$
  EndIf
Return
!
PARSE.LOADRQ.RESPONSE:
    EP.AMJ.STATUS$ = ""
    EP.TRX.STATUS$ = ""
!
    EP.M.LEN% = LEN(EP.AMJ.MESSAGE$)
    If EP.M.LEN% >= 9 Then \
    Begin
      EP.AMJ.STATUS$ = MID$(EP.AMJ.MESSAGE$,8,1)
      EP.TRX.STATUS$ = MID$(EP.AMJ.MESSAGE$,9,1)
    EndIf \
    Else \
    Begin
      EP.AMJ.STATUS$ = "0"
      EP.TRX.STATUS$ = "3"                 ! load parm response error
    EndIf
Return
!
!
End Sub

!--- Dialogos adicionados para el manejo de diferidos
!--- Desarrollado por OVS 17 Jul 2007 

Function DIALOGOS.ADICIONALES(XTIPO$,X.PAGO$)
String DIALOGOS.ADICIONALES, X.PAGO$, \
       Xdat1$, XTIPO$
Dialogo1:
    Xdat1$ = Asic.Datos$(Left$(EP.GUIDE.MESSAGE$(4),20),Right$(EP.GUIDE.MESSAGE$(4),20))
    If Xdat1$ = "E" Then Begin 
       Dialogos.Adicionales = "00"
       Exit Function 
    EndIf 
    If Match(Xdat1$,"12",1) <= 0 Then GoTo Dialogo1
    Dialogos.Adicionales = Xdat1$
 
End Function 


Function Captura.Diferidos.Tef(Xaut%) Public
String Xdlg1$, Xdlg2$, Xkey$, DM.DUMMY$,DM.NAME$,DM.MESES$, EP.TMP.VAR$, Xmes$
Integer*1 Captura.Diferidos.Tef, Xaut%, Xstat%
  TS.ER.RETURN = -1
  Open "R::ADX_IDT1:TEFPARAM.DAT" KEYED RECL 42 AS EP.IOPARM%
  If TS.ER.RETURN <> -1 Then BEGIN 
  	 Call EP.DISPLAY.A.MESSAGE("ERROR PARAMETROS DIFERIDOS")
  	 WAIT ; 1800
  	 Captura.Diferidos.Tef = 0
  	 Exit Function 
  ENDIF 
  Gr.Tef.CodDif$ = "00"           ! Init variable
  Xdlg1$ = Asic.Datos$("Codigo Diferido...?",Left$(Auto.Dife(Xaut%),20))
  If Xdlg1$ = "E" Then Begin 
     Captura.Diferidos.Tef = 0
     Close EP.IOPARM%
     Exit Function 
  EndIf 
  Xkey$ = Pack$( Right$("00"+Str$(Xaut%),2) + Right$("00"+Xdlg1$,2) )
  TS.ER.RETURN = -1
  READ FORM "C2 2C20";#EP.IOPARM% KEY XKEY$;                   \! Lee Reg Cabecera 
      DM.DUMMY$,DM.NAME$,DM.MESES$
  Xstat% = TS.ER.RETURN
  Close EP.IOPARM%

  If Xstat% <> -1 Then BEGIN 
  	 Call EP.DISPLAY.AN.ERROR("CODIGO DIFERIDO NO  EXISTE")
     Captura.Diferidos.Tef = 0
     Exit Function   	 
  ENDIF Else BEGIN 
     Xkey$ = Asic.Datos$(DM.NAME$,"Correcto? 1.Si 0.No")
     If Xkey$ <> "1" Then Begin 
       Captura.Diferidos.Tef = 0
       Exit Function 
     EndIf 
     Gr.Tef.CodDif$ = Right$("00"+Xdlg1$,2)                     ! Codigo diferido capturado
  ENDIF 

  I.ENTRA.CUOTAS:
 	  Xdlg2$ = Asic.Datos$("MESES A DIFERIR",DM.MESES$)
    If Xdlg2$ = "E" Then Begin 
       Captura.Diferidos.Tef = 0
       Exit Function 
    EndIf
    Xmes$ = "," + DM.MESES$
    EP.TMP.VAR$ = "," + Xdlg2$ + ","
    If MATCH(EP.TMP.VAR$,Xmes$,1) <= 0 Then GoSub I.ENTRA.CUOTAS
    EP.CUOTAS.QTY$  = RIGHT$(String$(2,"0")+ Str$(Val(Xdlg2$)),2)
    EP.TIPO.DIFERI$ = RIGHT$(String$(2,"0")+ Str$(Val(Xdlg1$)),2)
    Captura.Diferidos.Tef = -1
  
End Function 

Sub INIT.EFT.API(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
  EP.ECR.NBR$, EP.ECR.TRANSNUM$, EP.VALOR.EFT$, EP.TAX.EFT$, EP.PROPINA$, \
  EP.APPROV.CODE$, EP.APPROV.DESC$, EP.CARD.NUMBER$, EP.AUTH.NUMBER$, \
  EP.TIPO.VAR$, EP.USER.DATA$) PUBLIC

!
! WebSphere Message handling data
!
  String               \
    EP.APPL$,          \
    EP.ECR.FUNCTION$,  \
    EP.TRX.STATUS$,    \
    EP.AMJ.STATUS$,    \
    EP.ECR.NBR$,       \
    EP.ECR.TRANSNUM$,  \
    EP.VALOR.EFT$,     \
    EP.TAX.EFT$,       \
    EP.PROPINA$,       \
    EP.APPROV.CODE$,   \
    EP.APPROV.DESC$,   \
    EP.CARD.NUMBER$,   \
    EP.CARD.NUMBER2$,   \
    EP.AUTH.NUMBER$,   \
    EP.TIPO.VAR$,      \
    EP.TMP.VAR$,       \
    EP.KEYB.CREDIT.DATA$, \
    EP.DIF.LISTAMES$,  \
    EP.USER.DATA$, XB$

Integer*2 XJ%, Xautor%
!
!
!  ON ERROR GOTO ERRORTRAP           !! set "ON ERROR"
!  ON ASYNC ERROR Call ASYNC.ERR      !! set "ON ASYNC ERROR"
!  

  If EP.TRX.PEND$ <> "" Then Begin  
  
    If EP.REV.APPL$ = "01" Then \ 
      Call EPAY.REVERSE.TRX \
    Else \  
      Call EP.REVERSE.TRX 
   
    If EP.TRX.PEND$ <> "" Then Begin
      EP.AMJ.STATUS$ = EP.REV.STATUS$
      EP.TRX.STATUS$ = EP.REVTRX.STATUS$
      Exit Sub      
    EndIf
  EndIf
!
  EP.AMJ.STATUS$  = "2"
  EP.TRX.STATUS$  = "0"
  EP.APPL.STATUS$ = "00"
  EP.VALOR.EPAY$  = EP.VALOR.EFT$
  EP.TAX.EPAY$    = EP.TAX.EFT$
  EP.PROP.EPAY$   = EP.PROPINA$
  EP.IVA.BASE$    = RIGHT$(String$(10,"0") + Mid$(EP.USER.DATA$,1,10),10)
  EP.CASHBACK$    = RIGHT$(String$(10,"0") + MID$(EP.USER.DATA$,11,10),10)
  EP.CAJERO$      = RIGHT$(String$(10,"0") + UNPACK$(TS.OPER$),10)
  EP.DONACION$    = String$(10,"0")
  EP.FILLER4$     = String$(10,"0")
  EP.FECHA.INIC$  = DATE$ + TIME$
  EP.FUNCTION$    = "00"
  EP.AUTH.NUMBER$ = ""
  EP.APPROV.CODE$ = ""
  EP.APPROV.DESC$ = ""
  EP.CARD.NUMBER$ = ""
  EP.CARD.NUMBER2$ = ""
  EP.COMERCIO$    = ""
  EP.EPAY.TERMINAL$=""
  EP.CUOTAS.QTY$  = ""
  EP.FECHA.POSTEO$= ""
  EP.FECHA.PROC$  = ""
  EP.COD.PROC$    = ""
  EP.CENTRAL.CHEQUE$ = ""
  EP.TIPO.ID$     = ""
  EP.ID.GIRADOR$  = ""
  EP.TEL.GIRADOR$ = ""
  EP.COD.BANCO$   = ""
  EP.CTA.CTE$     = ""
  EP.NRO.CHEQUE$  = ""
  EP.RRN$         = ""
  EP.TIPO.VAR$    = ""
  EP.FRANQUICIA$  = ""
  EP.BANCO$       = ""
  EP.PRODUCTO$    = ""
  EP.USER.DATA$   = ""
  EP.GOOD.END%    = 0
  EP.STATE%       = 1
  EP.EXCEPTION$   = ""
  EP.MSR.DATA$    = ""
  EP.KEYB.DATA$   = ""
  EP.ANUL.TRANSNUM$ = String$(6,"0")
  EP.ANUL.AUTH$     = String$(6,"0")
  EP.ANUL.CODPRO$   = String$(6,"0")
  EP.ANUL.RRN$      = String$(12,"0")
  EP.ANUL.CUOTAS$   = String$(2,"0")
  EP.ANUL.POSTEO$   = String$(4,"0")
  EP.ANUL.CARD$     = String$(4,"0")
  Gr.Tef.CodDif$    = "00"             ! Init codigo de diferido
  
!
!

  Call EP.SAVE.KEYS

  Call EP.GET.TRX.TYPE(EP.GUIDE.MESSAGE$(1),EP.MSR.DATA$,EP.KEYB.DATA$)
!
  If TS.IO.MOTORKEY = 73 Then \
  Begin
    EP.AMJ.STATUS$ = "2"
    EP.TRX.STATUS$ = "K"
    Call EP.RESTORE.KEYS
    Exit Sub
  EndIf
!
!  Call EP.RESTORE.KEYS
Repeat2:  

    EP.KEYB.CREDIT.DATA$ = ""
    EP.TIPO.DIFERI$ = "00" 
 
   
!--- Validar Dialogos si son para Datafax o Cuota Facil 
!--- Dependiendo del BIN
  XB$ = ""
  FOR XJ% = 1 TO LEN(EP.MSR.DATA$)
	   XB$ = XB$ + Str$(Val(UNPACK$(MID$(EP.MSR.DATA$,XJ%,1))))
  Next XJ%
  XB$ = Right$(XB$,188)                              ! Toma ultima lectura de la banda magnetica
  XB$ = Mid$(XB$,3,6)                                ! Extrae el bin de la tarjeta

  If Val(XB$) <= 0 Then BEGIN 
  	 Call EP.DISPLAY.AN.ERROR("ERROR LECTURA TARJETA")
     EP.AMJ.STATUS$ = "2"
     EP.TRX.STATUS$ = "K"
     Call EP.RESTORE.KEYS
     Exit Sub
  EndIf 

!--- Para pruebas de bines  
!    XB$ = Asic.Datos$("BIN PRUEBA","6 DIGITOS")
!    If XB$ = "E" Then Begin 
!      EP.AMJ.STATUS$ = "2"
!      EP.TRX.STATUS$ = "K"
!      Call EP.RESTORE.KEYS
!      Exit Sub
!    EndIf
    
!--- Fin de pruebas    
      
  Xautor% = Retorno.Autorizador(Xb$)
  If Xautor% = 100 Then Begin
!  	 Call VISORES4690(1,"BIN NO PARAMETRIZADO","AUTORIZADOR DEFAULT",1800,"L")
 
  	 Xautor% = Autorizador.Base(Xb$)  ! Retorna autorizador Base del bin
 
  Endif 
  
!  TS.TEMP1$  = " INVENTO POCHO."
!  If Xautor% = 0 Then TS.TEMP1$ = " DATAFAST"
!  If Xautor% = 1 Then TS.TEMP1$ = " UNIBANCO"
!  If Xautor% = 2 Then TS.TEMP1$ = " MEDIANET"
!  Call VISORES4690(1,"UTILIZA AUTORIZADOR",STR$(Xautor%)+TS.TEMP1$,1800,"L")
  
  If Val(EP.TIPO.TARJETA$) = 177 Then GoTo DIALOGO.CUOTAFACIL \ 
  Else GoTo DIALOGO.DATAFAS
  
DIALOGO.CUOTAFACIL:
  
  Call EP.GET.KBDATA(EP.GUIDE.MESSAGE$(25),"1" , "2", \
                     EP.KEYB.CREDIT.DATA$)
  If TS.IO.MOTORKEY = 73 Then Begin
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "K"
      Call EP.RESTORE.KEYS
      Exit Sub
  EndIf
  If Val(EP.KEYB.CREDIT.DATA$) = 1 Then Begin 								! TIPO CORRIENTE
     EP.TIPO.CUENTA$ = "1"                                    ! Tipo Corriente 
     EP.CUOTAS.QTY$  = "00"
     GoTo CONTINUA.PROCESOTEF
  EndIf Else Begin 																						! TIPO DIFERIDO 
     EP.TIPO.DIFERI$ = "00" 

   If Val(EP.DIF.UNIBANCO$) = 1 Then BEGIN									! Si Diferidos Activos
      TS.TEMP1I4 = Captura.Diferidos.Tef(Xautor%)           ! Captura tipo de diferido
      If TS.TEMP1I4 = -1 Then BEGIN                         ! Si diferido valido
      	 EP.TIPO.CUENTA$ = "2"                              !
      	 GoTo CONTINUA.PROCESOTEF                           ! Continua proceso pago
      EndIf Else Begin    	                                ! Si diferido no valido
        Call EP.DISPLAY.AN.ERROR("PROCESO CANCELADO")
        EP.AMJ.STATUS$ = "2"
        EP.TRX.STATUS$ = "K"
        Call EP.RESTORE.KEYS
        Exit Sub        
      EndIf 
   EndIf

  EndIf 
    
DIALOGO.DATAFAS:

    Call EP.GET.KBDATA(EP.GUIDE.MESSAGE$(2),"1" , "3", \
    EP.KEYB.CREDIT.DATA$)
    
    If TS.IO.MOTORKEY = 73 Then \
    Begin
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "K"
      Call EP.RESTORE.KEYS
      Exit Sub
    EndIf
    If Val(EP.KEYB.CREDIT.DATA$) > 3 Or Val(EP.KEYB.CREDIT.DATA$) < 1 Then GoSub Repeat2
    
 If EP.KEYB.CREDIT.DATA$ = "2" Then Begin  \! Captura de diferidos 
     EP.TIPO.DIFERI$ = "00"
   If Val(EP.DIF.DATAFAST$) = 1 Then BEGIN									! Si Diferidos Activos
      TS.TEMP1I4 = Captura.Diferidos.Tef(Xautor%)                 ! Captura tipo de diferido
      If TS.TEMP1I4 = -1 Then BEGIN                         ! Si diferido valido
      	 EP.TIPO.CUENTA$ = "2"                              !
      	 GoTo CONTINUA.PROCESOTEF                           ! Continua proceso pago
      EndIf Else Begin    	                                ! Si diferido no valido
        Call EP.DISPLAY.AN.ERROR("PROCESO CANCELADO")
        EP.AMJ.STATUS$ = "2"
        EP.TRX.STATUS$ = "K"
        Call EP.RESTORE.KEYS
        Exit Sub        
      EndIf 
   EndIf
		
	 ENTRA.CUOTAS:
	  EP.KEYB.CREDIT.DATA$ = ""
 	  EP.KEYB.CREDIT.DATA$ = Asic.Datos$(Left$(EP.GUIDE.MESSAGE$(3),20),Right$(EP.GUIDE.MESSAGE$(3),20))
    If EP.KEYB.CREDIT.DATA$ = "E" Then Begin   ! Presiono tecla borrar
       EP.KEYB.CREDIT.DATA$ = ""
       EP.AMJ.STATUS$ = "2"
       EP.TRX.STATUS$ = "K"
       Call EP.RESTORE.KEYS
       Exit Sub
    EndIf 
    
    EP.TMP.VAR$ = "," + EP.KEYB.CREDIT.DATA$ + ","
    If MATCH(EP.TMP.VAR$,EP.DIF.LISTAMES$,1) <=0 Then GoSub ENTRA.CUOTAS
    
    EP.CUOTAS.QTY$ = RIGHT$(String$(2,"0")+ Str$(Val(EP.KEYB.CREDIT.DATA$)),2)
    
	EndIf Else Begin 
	   EP.CUOTAS.QTY$ = "00"
	   If EP.KEYB.CREDIT.DATA$ = "3" Then Begin                            ! Captura tarjetas debito 
	      EP.TIPO.CUENTA$ = DIALOGOS.ADICIONALES("1",EP.KEYB.CREDIT.DATA$) ! Si es cuenta de ahorros o corriente
    	  If EP.TIPO.CUENTA$ = "00" Then Begin 														 ! No capturo dato adicional
           EP.AMJ.STATUS$ = "2"
           EP.TRX.STATUS$ = "K"
           Call EP.RESTORE.KEYS
           Exit Sub
	      EndIf 
	      
	      If Val(EP.TIPO.CUENTA$) = 1 Then EP.TIPO.CUENTA$ = "3" Else \    ! Si es cuenta de ahorros 
	                                       EP.TIPO.CUENTA$ = "4"           ! Si es cuenta corriente 
	   EndIf Else EP.TIPO.CUENTA$ = "1"                                    ! Tipo credito corriente 
	EndIf 

CONTINUA.PROCESOTEF:
  Call EP.RESTORE.KEYS
    
!
  If EP.ECR.FUNCTION$ = "09" Then \
  Begin
    If EP.KEYB.DATA$ = "1"   OR \
       EP.MSR.DATA$ <> ""  Then \
      EP.FUNCTION$ = "11" \
    Else \
      EP.FUNCTION$ = "21"
  EndIf 

  If EP.ECR.FUNCTION$ = "10" Then \  
  Begin
    If EP.KEYB.DATA$ = "1"  OR \
       EP.MSR.DATA$ <> "" Then \
    Begin 
      If TS.PROCEDURE < 1 Then \
      Begin 
        EP.FUNCTION$ = "12" 
      EndIf \
      Else \
      Begin
        EP.FUNCTION$ = "14" 
      EndIf
    EndIf \
    Else  \
    Begin     
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "R"
      Exit Sub
    EndIf
  EndIf
  If EP.KEYB.DATA$ = "1"  OR   \
     EP.MSR.DATA$ <> "" Then   \
     	Begin
    EP.TIPO.VAR$ = EP.TV.TJDB$ 
    If EP.KEYB.CREDIT.DATA$ = "3" Then EP.TIPO.VAR$ = EP.TV.TJCR$
   EndIf Else \
  If EP.KEYB.DATA$ = "2" Then \
    EP.TIPO.VAR$ = EP.TV.CHEQ$ \
  Else \
    EP.TIPO.VAR$ = EP.TV.TJDB$  
!	 
  Call EP.BUSCA.LIMITE.TV(EP.TIPO.VAR$)
!  Call EP.DISPLAY.AN.ERROR("TV="+EP.TIPO.VAR$+ " MAX="+STR$(EP.MAX.TV%))

  If EP.FUNCTION$ = "12" Then \
  Begin 
    If VAL(EP.ECR.TRANSNUM$) = 0 Then \     ! No viene trx para anular
    Begin
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "N"
      Exit Sub
    EndIf    
!
    EP.DE1102.FOUND% = 0
    EP.APPROV.FOUND% = 0
    EP.DE1103.FOUND% = 0
    EP.DE1203.FOUND% = 0
    For EP.M% = 1 TO SL.END 
      If SL.STR$(EP.M%) <> "" Then \
      Begin 
        If ASC(SL.STR$(EP.M%)) = 11H Then \
        Begin
          EP.B$ = SL.STR$(EP.M%) + ":"
          EP.J% = 3 
          Call EP.GETUNPK
          If LEFT$(EP.A$,EP.LEN.CLAVE%) = EP.EFT.CLAVE$ Then \  
          Begin
            If MID$(EP.A$,EP.LEN.CLAVE% + 1,4) = "0111"            AND \  
               MID$(EP.A$,EP.LEN.CLAVE% +5,6)  = EP.ECR.TRANSNUM$  AND \
               MID$(EP.A$,EP.LEN.CLAVE% +11,2) = "02"             Then \
            Begin
              EP.DE1102.FOUND% = -1
              Call EP.GETUNPK   
              EP.DE.ISOCOD$ = MID$(EP.A$,19,2)
              Call EP.GETUNPK
              EP.DE.POSTEO$ = LEFT$(EP.A$,4)
              EP.DE.CODPRO$ = MID$(EP.A$,5,6)
              EP.DE.CARD$ = MID$(EP.A$,11,4)
              EP.DE.AUTH$ = MID$(EP.A$,15,6)
              Call EP.GETUNPK
              EP.DE.AMOUNT$ = LEFT$(EP.A$,10)
              EP.DE.IVA$ = MID$(EP.A$,11,10)
              Call EP.GETUNPK
              EP.DE.PROPINA$ = "00" + MID$(EP.A$,13,8)
              Call EP.GETUNPK
              EP.DE.RRN$ =  LEFT$(EP.A$,12)
              EP.DE.CUOTAS$ = MID$(EP.A$,13,2)  
              EP.DE.TIPOVAR$ = MID$(EP.A$,15,2)  
              If EP.DE.ISOCOD$ = "00" OR  \
                 EP.DE.ISOCOD$ = "08" OR  \
                 EP.DE.ISOCOD$ = "11" OR  \
                 EP.DE.ISOCOD$ = "76" OR  \
                 EP.DE.ISOCOD$ = "77" OR  \
                 EP.DE.ISOCOD$ = "78" OR  \
                 EP.DE.ISOCOD$ = "79" OR  \
                 EP.DE.ISOCOD$ = "80" OR  \
                 EP.DE.ISOCOD$ = "81" Then  \
              Begin
                EP.APPROV.FOUND% = -1
              EndIf
            EndIf
!
            If MID$(EP.A$,EP.LEN.CLAVE% + 1,4) = "0111"           AND \
               MID$(EP.A$,EP.LEN.CLAVE% + 5,6) = EP.ECR.TRANSNUM$ AND \
               MID$(EP.A$,EP.LEN.CLAVE% +11,2) = "03"            Then \
            Begin
              EP.DE1103.FOUND% = -1
              Call EP.GETUNPK
              EP.DE.IVA.BASE$ = LEFT$(EP.A$,10)
              EP.DE.CASHBACK$ = MID$(EP.A$,11,10)
            EndIf 
!
            If MID$(EP.A$,EP.LEN.CLAVE% + 1,4) = "0112"           AND \
               MID$(EP.A$,EP.LEN.CLAVE% +11,2) = "03"            Then \
            Begin
              Call EP.GETUNPK
              EP.DE.IVA.BASE$ = LEFT$(EP.A$,10)
              EP.DE.CASHBACK$ = MID$(EP.A$,11,10)
              Call EP.GETUNPK
              Call EP.GETUNPK
              If LEN(EP.A$) > 0 Then \
              Begin
                If LEFT$(EP.A$,6) = EP.ECR.TRANSNUM$ Then \
                Begin
                  EP.DE1203.FOUND% = -1 
                EndIf
              EndIf
            EndIf
          EndIf
        EndIf
      EndIf
    Next EP.M%
    If NOT EP.DE1102.FOUND% Then \           ! no existe respuesta de transaccion
    Begin
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "O"
      Exit Sub
    EndIf
    If NOT EP.APPROV.FOUND% Then \
    Begin
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "T"
      Exit Sub
    EndIf    
    If NOT EP.DE1103.FOUND% Then \         ! no se termino transaccion voucher
    Begin
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "U"
      Exit Sub
    EndIf
    If EP.DE1203.FOUND% Then \         ! Ya existe anulacion anterior
    Begin
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "L"
      Exit Sub
    EndIf    
    If VAL(EP.VALOR.EPAY$) <> VAL(EP.DE.AMOUNT$) Then \ ! No coincide valor de transaccion
    Begin
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "M"
      Exit Sub
    EndIf   
    EP.ANUL.TRANSNUM$ = EP.ECR.TRANSNUM$
    EP.ANUL.POSTEO$   = EP.DE.POSTEO$
    EP.ANUL.CODPRO$   = EP.DE.CODPRO$
    EP.ANUL.CARD$     = EP.DE.CARD$
    EP.ANUL.AUTH$     = EP.DE.AUTH$
    EP.ANUL.RRN$      = EP.DE.RRN$
    EP.ANUL.CUOTAS$   = EP.DE.CUOTAS$ 
    EP.TAX.EPAY$      = EP.DE.IVA$
    EP.IVA.BASE$      = EP.DE.IVA.BASE$
    !EP.TAX.BASE$     = EP.IVA.BASE$
    EP.PROP.EPAY$     = EP.DE.PROPINA$
    EP.CASHBACK$      = EP.DE.CASHBACK$
    EP.CAJERO$        = RIGHT$(String$(10,"0") + UNPACK$(TS.OPER$),10)
    EP.TIPO.VAR$      = EP.DE.TIPOVAR$
  EndIf                                  ! fin de transaccion de anulacion dentro de misma transaccion 
!
  If EP.FUNCTION$ = "14" Then \          ! anulacion de transaccion de compra anterior 
  Begin
    If LEN(EP.ECR.TRANSNUM$) > 0 Then \
    Begin 
      If VAL(EP.ECR.TRANSNUM$) = 0 Then \     ! No viene trx para anular
      Begin
        EP.AMJ.STATUS$ = "2"
        EP.TRX.STATUS$ = "N"
        Exit Sub
      EndIf    
    EndIf
    EP.DE1102.FOUND% = 0
    EP.DE1103.FOUND% = 0
    EP.DE1203.FOUND% = 0
    EP.APPROV.FOUND% = 0
    EP.EFT.FOUND% = 0
    TS.ER.RETURN = -1
    Open "R::EFTTRX" KEYED RECL 158 AS EP.IOPARM% NOWRITE NODEL
    If NOT TS.ER.RETURN  Then \   ! si no abre
    Begin 
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "V"
      Exit Sub
    EndIf
    EP.KEY$ = RIGHT$(String$(4,"0")+TS.TERMINAL$,4) + \
       "0111"+ EP.ECR.TRANSNUM$ + "02"
    TS.ER.RETURN = -1 
    Read Form "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
      KEY EP.KEY$ ;                                         \
      EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,      \
      EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$, EP.A$, EP.A$
    If NOT TS.ER.RETURN Then \  ! si no existe
    Begin 
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "O"
      CLOSE EP.IOPARM%
      Exit Sub
    EndIf \
    Else \
    Begin
      EP.DE1102.FOUND% = -1
      EP.DE.ISOCOD$ = MID$(EP.DE2.DATA$,19,2)
      EP.DE.POSTEO$ = LEFT$(EP.DE3.DATA$,4)
      EP.DE.CODPRO$ = MID$(EP.DE3.DATA$,5,6)
      EP.DE.CARD$   = MID$(EP.DE3.DATA$,11,4)
      EP.DE.AUTH$   = MID$(EP.DE3.DATA$,15,6)
      EP.DE.AMOUNT$ = LEFT$(EP.DE4.DATA$,10)
      EP.DE.IVA$    = MID$(EP.DE4.DATA$,11,10)
      EP.DE.PROPINA$ = "00" + MID$(EP.DE5.DATA$,13,8)
      EP.DE.RRN$    = LEFT$(EP.DE6.DATA$,12)
      EP.DE.CUOTAS$ = MID$(EP.DE6.DATA$,13,2)  
      EP.DE.TIPOVAR$ = MID$(EP.DE6.DATA$,15,2)  
      If EP.DE.ISOCOD$ = "00" OR  \
         EP.DE.ISOCOD$ = "08" OR  \
         EP.DE.ISOCOD$ = "11" OR  \
         EP.DE.ISOCOD$ = "76" OR  \
         EP.DE.ISOCOD$ = "77" OR  \
         EP.DE.ISOCOD$ = "78" OR  \
         EP.DE.ISOCOD$ = "79" OR  \
         EP.DE.ISOCOD$ = "80" OR  \
         EP.DE.ISOCOD$ = "81" Then  \
      Begin
        EP.APPROV.FOUND% = -1
      EndIf \
      Else \
      Begin
        EP.AMJ.STATUS$ = "2"
        EP.TRX.STATUS$ = "T" 
        CLOSE EP.IOPARM%
        Exit Sub
      EndIf
    EndIf
    EP.KEY$ = RIGHT$(String$(4,"0")+TS.TERMINAL$,4) + \
       "0111"+ EP.ECR.TRANSNUM$ + "03"
    TS.ER.RETURN = -1 
    Read Form "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
      KEY EP.KEY$ ;                                         \
      EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,      \
      EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$, EP.A$, EP.A$
    If NOT TS.ER.RETURN Then \  ! si no existe
    Begin 
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "U" 
      CLOSE EP.IOPARM%
      Exit Sub
    EndIf 
    EP.DE.IVA.BASE$ = LEFT$(EP.DE2.DATA$,10)
    EP.DE.CASHBACK$ = MID$(EP.DE2.DATA$,11,10)
!       
    EP.KEY$ = RIGHT$(String$(4,"0")+TS.TERMINAL$,4) + \
       "0110"+ EP.ECR.TRANSNUM$ + "03"

    TS.ER.RETURN = -1 
    Read Form "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
      KEY EP.KEY$ ;                                         \
      EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,      \
      EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$, EP.A$, EP.A$
!
    If TS.ER.RETURN Then \           ! si existe
    Begin 
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "L"
      CLOSE EP.IOPARM%
      Exit Sub
    EndIf
    CLOSE EP.IOPARM%
!    
    If VAL(EP.VALOR.EPAY$) <> VAL(EP.DE.AMOUNT$) Then \ ! No coincide valor de transaccion
    Begin
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "M"
      Exit Sub
    EndIf   
    EP.ANUL.TRANSNUM$ = EP.ECR.TRANSNUM$
    EP.ANUL.POSTEO$   = EP.DE.POSTEO$
    EP.ANUL.CODPRO$   = EP.DE.CODPRO$
    EP.ANUL.CARD$     = EP.DE.CARD$
    EP.ANUL.AUTH$     = EP.DE.AUTH$
    EP.ANUL.RRN$      = EP.DE.RRN$
    EP.ANUL.CUOTAS$   = EP.DE.CUOTAS$ 
    EP.TAX.EPAY$      = EP.DE.IVA$
    EP.TAX.EFT$       = EP.DE.IVA$
    EP.IVA.BASE$      = EP.DE.IVA.BASE$
    !EP.TAX.BASE$      = EP.IVA.BASE$    
    EP.PROP.EPAY$     = EP.DE.PROPINA$
    EP.CASHBACK$      = EP.DE.CASHBACK$
    EP.CAJERO$        = RIGHT$(String$(10,"0") + UNPACK$(TS.OPER$),10)
    EP.TIPO.VAR$      = EP.DE.TIPOVAR$
!    
  EndIf                   ! fin de transaccion de anulacion de transaccion anterior 
!     
  If EP.FUNCTION$ = "11"  OR  \
     EP.FUNCTION$ = "21" Then  \   ! trasaccion de compra normal
  Begin
    EP.ANUL.TRANSNUM$ = String$(6,"0")
    EP.ANUL.POSTEO$   = String$(4,"0")
    EP.ANUL.CODPRO$   = String$(6,"0")
    EP.ANUL.CARD$     = String$(4,"0")
    EP.ANUL.AUTH$     = String$(6,"0")
    EP.ANUL.CUOTAS$   = String$(2,"0")
    EP.ANUL.RRN$      = String$(12,"0") 
    
    EP.MAX.TV% = 9999999

!    Call EP.DISPLAY.AN.ERROR("MX=" + STR$(EP.MAX.TV%) + \
!                  " V=" + EP.VALOR.EPAY$)

                  
    If VAL(EP.VALOR.EPAY$) > EP.MAX.TV% Then \
    Begin
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "P"
      Exit Sub
    EndIf   
!

    If TS.PROCEDURE < 1 Then \
    Begin 
    
      EP.CB.IMP% = VAL(EP.CASHBACK$)
      EP.CB.MAXIMP% = 9999999
      EP.CHG.TV% = 9999999
 !                      Call EP.DISPLAY.AN.ERROR("IMP="+ STR$(EP.CHG.TV%) + " MAX " + STR$(EP.CB.MAXIMP%) + "PORC " + STR$(FLOAT(EP.CB.PORC%)))

      If EP.CB.IMP% > EP.CHG.TV%     OR \
         EP.CB.IMP% > EP.CB.MAXIMP%  OR \
         FLOAT(EP.CB.IMP%) / FLOAT(ABS(VAL(EP.VALOR.EPAY$))) * 100.0  > \
         FLOAT(EP.CB.PORC%)        Then \
      Begin
 
        EP.AMJ.STATUS$ = "2"
        EP.TRX.STATUS$ = "Q"
        Exit Sub
      EndIf
    EndIf  
  EndIf  

End Sub
!
!  Aqui se debe partir en rutina 32

Sub END.EFT.API(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
  EP.ECR.NBR$, EP.ECR.TRANSNUM$, EP.VALOR.EFT$, EP.TAX.EFT$, EP.PROPINA$, \
  EP.APPROV.CODE$, EP.APPROV.DESC$, EP.CARD.NUMBER$, EP.AUTH.NUMBER$, \
  EP.TIPO.VAR$, EP.USER.DATA$) PUBLIC

!
! WebSphere Message handling data
!
  String               \
    EP.APPL$,          \
    EP.ECR.FUNCTION$,  \
    EP.TRX.STATUS$,    \
    EP.AMJ.STATUS$,    \
    EP.ECR.NBR$,       \
    EP.ECR.TRANSNUM$,  \
    EP.14.TRANSNUM$,  \
    EP.VALOR.EFT$,    \
    EP.TAX.EFT$,       \
    EP.PROPINA$,       \
    EP.APPROV.CODE$,   \
    EP.APPROV.DESC$,   \
    EP.CARD.NUMBER$,   \
    EP.CARD.NUMBER2$,   \
    EP.LOTE$,   \
    EP.AUTH.NUMBER$,   \
    EP.TIPO.VAR$,      \
    EP.USER.DATA$
! 

ep.14.transnum$ = ep.ecr.transnum$
  EP.IVA.BASE$    = RIGHT$(String$(10,"0") + LEFT$(EP.USER.DATA$,10),10)
  EP.CASHBACK$    = RIGHT$(String$(10,"0") + MID$(EP.USER.DATA$,11,10),10)
! 
  EP.SAVE.TRANSNUM$ = EP.EPAY.TRANSNUM$ 
  EP.EPAY.TRANSNUM$ = RIGHT$(String$(6,"0")+STR$(VAL(EP.EPAY.TRANSNUM$) + 1),6)
  TE.TR.USERHT$     = PACK$(EP.EPAY.TRANSNUM$)+ MID$(TE.TR.USERHT$,4,7)  
  EP.ECR.TRANSNUM$  = EP.EPAY.TRANSNUM$  !  RETURNs transaction number used
  EP.ECR.DATETIME$  = DATE$ + TIME$ 
!
  If EP.FUNCTION$ = "11"  OR  \
     EP.FUNCTION$ = "12"  OR  \
     EP.FUNCTION$ = "14" Then \        ! Transaccion tarjeta
  Begin
 !-- Comentariar siguiente linea
  !Call EP.DISPLAY.AN.ERROR("EP.CUOTAS.QTY$ "+EP.CUOTAS.QTY$+ ":"+EP.ANUL.CUOTAS$)
 
 		EP.ANUL.CUOTAS$ = EP.CUOTAS.QTY$
    EP.USER.DATA$ = EP.MSR.DATA$
!--- Adicionado 26 julio 2007 OVS    
 
    EP.ANUL.CODPRO$ = Right$("000000"+(EP.TIPO.CUENTA$ + EP.TIPO.DIFERI$),6)    ! Arma Tipo de cuenta y tipo diferido
    !-- Comentariar siguiente linea
    !Call EP.DISPLAY.AN.ERROR("TIPO CUENTA "+EP.ANUL.CODPRO$)
 
    
    EP.MESSAGE$ = LEFT$(EP.ECR.NBR$,6) + EP.EPAY.TRANSNUM$ + EP.ECR.DATETIME$ + \
      EP.VALOR.EPAY$ + EP.TAX.EPAY$ + EP.IVA.BASE$ + EP.CAJERO$ + EP.PROP.EPAY$ + EP.CASHBACK$ +\                              \
      EP.ANUL.AUTH$ + EP.ANUL.CODPRO$ + EP.ANUL.CUOTAS$ + EP.ANUL.TRANSNUM$ +   \
      EP.ANUL.POSTEO$ + EP.ANUL.CARD$ + EP.CHAIN$ + MID$(EP.ECR.NBR$,7,4) +     \
      EP.USER.DATA$ 
      
    EP.MSGLEN$ = RIGHT$(String$(3,"0") + STR$(LEN(EP.MESSAGE$)),3)
    EP.MESSAGE$ = EP.APPL$ + EP.FUNCTION$ + EP.MSGLEN$ + EP.MESSAGE$
    GoSub SEND.DBCR.TO.AMJ
  EndIf
!
  If EP.FUNCTION$ = "21" Then \   ! Transaccion cheque 
  Begin
    EP.KEYB.DATA$ = ""
    Call EP.GET.KBDATA(EP.GUIDE.MESSAGE$(9),"1" , "2", \
    EP.KEYB.DATA$)
    If TS.IO.MOTORKEY = 73 Then \
    Begin
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "K"
      Call EP.RESTORE.KEYS
      Exit Sub
    EndIf
    EP.CENTRAL.CHEQUE$ = EP.KEYB.DATA$
!
    EP.KEYB.DATA$ = ""
    Call EP.GET.KBDATA(EP.GUIDE.MESSAGE$(10),"1" , "3", \
    EP.KEYB.DATA$)
    If TS.IO.MOTORKEY = 73 Then \
    Begin
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "K"
      Call EP.RESTORE.KEYS
      Exit Sub
    EndIf
    EP.TIPO.ID$ = EP.KEYB.DATA$
!
    EP.KEYB.DATA$ = ""
    Call EP.GET.KBDATA(EP.GUIDE.MESSAGE$(11),"1000" , String$(11,"9"), \
    EP.KEYB.DATA$)
    If TS.IO.MOTORKEY = 73 Then \
    Begin
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "K"
      Call EP.RESTORE.KEYS
      Exit Sub
    EndIf
    EP.ID.GIRADOR$ = RIGHT$(String$(11,"0") + EP.KEYB.DATA$, 11)
!
    EP.KEYB.DATA$ = ""
    Call EP.GET.KBDATA(EP.GUIDE.MESSAGE$(12),"1000000" , String$(7,"9"), \
    EP.KEYB.DATA$)
    If TS.IO.MOTORKEY = 73 Then \
    Begin
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "K"
      Call EP.RESTORE.KEYS
      Exit Sub
    EndIf
    EP.TEL.GIRADOR$ = RIGHT$(String$(7,"0") + EP.KEYB.DATA$, 7)
!
    EP.KEYB.DATA$ = ""
    Call EP.GET.KBDATA(EP.GUIDE.MESSAGE$(13),"1" , "99", \
    EP.KEYB.DATA$)
    If TS.IO.MOTORKEY = 73 Then \
    Begin
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "K"
      Call EP.RESTORE.KEYS
      Exit Sub
    EndIf
    EP.COD.BANCO$ = RIGHT$(String$(2,"0") + EP.KEYB.DATA$, 2)
!
    EP.KEYB.DATA$ = ""
    Call EP.GET.KBDATA(EP.GUIDE.MESSAGE$(14),"1000" , String$(13,"9"), \
    EP.KEYB.DATA$)
    If TS.IO.MOTORKEY = 73 Then \
    Begin
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "K"
      Call EP.RESTORE.KEYS
      Exit Sub
    EndIf
    EP.CTA.CTE$ = RIGHT$(String$(13,"0") + EP.KEYB.DATA$, 13)
!
    EP.KEYB.DATA$ = ""
    Call EP.GET.KBDATA(EP.GUIDE.MESSAGE$(15),"100" , String$(9,"9"), \
    EP.KEYB.DATA$)
    If TS.IO.MOTORKEY = 73 Then \
    Begin
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "K"
      Call EP.RESTORE.KEYS
      Exit Sub
    EndIf
    EP.NRO.CHEQUE$ = RIGHT$(String$(9,"0") + EP.KEYB.DATA$, 9)
!
    Call EP.RESTORE.KEYS
    EP.MESSAGE$ = LEFT$(EP.ECR.NBR$,6) + EP.EPAY.TRANSNUM$ + EP.ECR.DATETIME$ + \
      EP.VALOR.EPAY$ + EP.CENTRAL.CHEQUE$ + EP.TIPO.ID$ + EP.ID.GIRADOR$ + \
      EP.TEL.GIRADOR$ + EP.COD.BANCO$ + EP.CTA.CTE$ + EP.NRO.CHEQUE$ + \
      EP.CHAIN$ + MID$(EP.ECR.NBR$,7,4) + EP.USER.DATA$
      EP.MSGLEN$ = RIGHT$(String$(3,"0") + STR$(LEN(EP.MESSAGE$)),3)
      EP.MESSAGE$ = EP.APPL$ + EP.FUNCTION$ + EP.MSGLEN$ + EP.MESSAGE$
    GoSub SEND.CHEQUE.TO.AMJ
  EndIf
!
  EP.INICIO.FOUND%  = 0
  EP.RESP.FOUND%    = 0
  EP.APPROV.FOUND%  = 0
  EP.VOUCHER.FOUND% = 0
  EP.TOHOST.FOUND%  = 0
  EP.APPROV.CODE$   = "99"
!  

  While (EP.STATE% < 4) AND (EP.EXCEPTION$ = "")
    EP.M.LEN% = LEN(EP.AMJ.MESSAGE$)
    If EP.M.LEN% < 8 Then \
    Begin
      EP.TRX.STATUS$ = "0"
      EP.AMJ.STATUS$ = "1"
      Exit Sub
    EndIf \
    Else \
    Begin
      EP.AMJ.STATUS$ = MID$(EP.AMJ.MESSAGE$,8,1)
    EndIf 
    If EP.AMJ.STATUS$ = "0" OR   \
       EP.AMJ.STATUS$ = "1" Then \
    Begin
      Exit Sub
    EndIf
    If EP.AMJ.STATUS$ = "2" Then \        ! Transaccion con respuesta de host concluida
    Begin
      If EP.M.LEN% < 9 Then \
      Begin
        EP.TRX.STATUS$ = "1" 
        Exit Sub
      EndIf
      EP.TRX.STATUS$ = MID$(EP.AMJ.MESSAGE$,9,1)
      EP.RESP.FOUND% = -1
!
      If EP.FUNCTION$ = "21" OR  \
         EP.FUNCTION$ = "22" OR  \
         EP.FUNCTION$ = "23" Then \
      Begin
        GOSub PARSE.CHEQUE.RESPONSE
        If EP.TRX.STATUS$ <> "0" Then \
        Begin
          EP.STATE% = 4
          Exit Sub        
        EndIf
        Call EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.APPL$ + EP.FUNCTION$ + EP.EPAY.TRANSNUM$ + "02" , \ DE-1
             EP.COMERCIO$ + EP.EPAY.TERMINAL$ + EP.APPROV.CODE$,                                    \ DE-2 
             EP.FECHA.POSTEO$ + EP.COD.PROC$ + EP.CENTRAL.CHEQUE$ + EP.TIPO.ID$ + EP.COD.BANCO$ + EP.AUTH.NUMBER$ , \ DE-3
             EP.VALOR.EPAY$ + EP.NRO.CHEQUE$ + "0",                                                 \ DE-4
             EP.TEL.GIRADOR$ + EP.CTA.CTE$,                                                         \ DE-5
             EP.RRN$ + "00" + EP.TIPO.VAR$ + EP.TRX.STATUS$ + "000" )                               ! DE-6
        !-- Almacena 2User Data 99
         TS.USERDATA$= EP.EFT.CLAVE$ + EP.APPL$ + EP.FUNCTION$ + EP.EPAY.TRANSNUM$ + "02" + ":" + \
                       EP.FRANQUICIA$ + ":" + EP.BANCO$ + ":" + EP.PRODUCTO$ + ":" + EP.VALOR.EPAY$ + ":" + \
                       EP.TAX.EPAY$ + ":" + Gr.Tef.CodDif$
         TS.TEMP1I1 = 99                  
         Call TSTPEC01

      EndIf \
      Else \
      Begin 
        GOSub PARSE.DBCR.RESPONSE
        If EP.TRX.STATUS$ <> "0" Then \
        Begin
          EP.STATE% = 4
          Exit Sub        
        EndIf
        EP.USER.DATA$ = EP.CARD.BIN$
!
        Call EP.BUSCA.LIMITE.TV(EP.TIPO.VAR$)
        If VAL(EP.VALOR.EPAY$) > EP.MAX.TV% Then \
        Begin
 !         Call EP.DISPLAY.AN.ERROR(EP.GUIDE.MESSAGE$(38))
          EP.MAX.TV.SAV%    = TO.TENDLIMITS(EP.TV.POS%,0)
          EP.CHG.TV.SAV%    = TO.TENDLIMITS(EP.TV.POS%,1)
          TO.TENDLIMITS(EP.TV.POS%,0) = 9999999
          TO.TENDLIMITS(EP.TV.POS%,1) = 9999999
          EP.TV.CHG% = -1
        EndIf   
!      
        If TS.PROCEDURE < 1 AND \
          (VAL(EP.VALOR.EPAY$) - TS.BALDUE(0)) > EP.CHG.TV% Then \
        Begin
!          Call EP.DISPLAY.AN.ERROR(EP.GUIDE.MESSAGE$(38))
          EP.MAX.TV.SAV%    = TO.TENDLIMITS(EP.TV.POS%,0)
          EP.CHG.TV.SAV%    = TO.TENDLIMITS(EP.TV.POS%,1)
          TO.TENDLIMITS(EP.TV.POS%,0) = 9999999
          TO.TENDLIMITS(EP.TV.POS%,1) = 9999999
          EP.TV.CHG% = -1
        EndIf
!        
        If EP.TV.POS% <> TS.TDR.INDEX Then \
        Begin
          TS.NUMTNDRS(EP.TV.POS%) = TS.NUMTNDRS(TS.TDR.INDEX)
          TS.TENDVAMT(EP.TV.POS%) = TS.TENDVAMT(TS.TDR.INDEX)
          TS.NUMTNDRS(TS.TDR.INDEX) = 0
          TS.TENDVAMT(TS.TDR.INDEX) = 0
          TS.TDR.INDEX    = EP.TV.POS%
        EndIf
!
       If EP.FUNCTION$ = "14" Or EP.FUNCTION$ = "12" Then \
       	Begin
        Call EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.APPL$ + EP.FUNCTION$ + EP.14.TRANSNUM$ + "02" , \ DE-1
             EP.COMERCIO$ + EP.EPAY.TERMINAL$ + EP.APPROV.CODE$,                                    \ DE-2 
             EP.FECHA.POSTEO$ + EP.COD.PROC$ + EP.CARD.NUMBER$ + EP.AUTH.NUMBER$,                   \ DE-3
             EP.VALOR.EPAY$ + EP.TAX.EPAY$,                                                         \ DE-4
             EP.FECHA.PROC$ + RIGHT$(EP.PROP.EPAY$,8),                                              \ DE-5
             EP.RRN$ + EP.CUOTAS.QTY$ + EP.TIPO.VAR$ + EP.TRX.STATUS$ +"000")                       ! DE-6

      	EndIf Else \
      	Begin
        Call EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.APPL$ + EP.FUNCTION$ + EP.EPAY.TRANSNUM$ + "02" , \ DE-1
             EP.COMERCIO$ + EP.EPAY.TERMINAL$ + EP.APPROV.CODE$,                                    \ DE-2 
             EP.FECHA.POSTEO$ + EP.COD.PROC$ + EP.CARD.NUMBER$ + EP.AUTH.NUMBER$,                   \ DE-3
             EP.VALOR.EPAY$ + EP.TAX.EPAY$,                                                         \ DE-4
             EP.FECHA.PROC$ + RIGHT$(EP.PROP.EPAY$,8),                                              \ DE-5
             EP.RRN$ + EP.CUOTAS.QTY$ + EP.TIPO.VAR$ + EP.TRX.STATUS$ +"000")                       ! DE-6

      	EndIf
      
!-- Ajuste User Data 99        
       
        TS.USERDATA$ = EP.EFT.CLAVE$ + EP.APPL$ + EP.FUNCTION$ + EP.EPAY.TRANSNUM$ + "02" + ":" +   \
        EP.FRANQUICIA$ + ":" + EP.CARD.NUMBER2$ + EP.LOTE$ + ":" + EP.PRODUCTO$ + ":" + EP.VALOR.EPAY$ + ":" + \
        EP.TAX.EPAY$ +  ":" + Gr.Tef.CodDif$
        TS.TEMP1I1 = 99                  
        Call TSTPEC01

!--- Adiciona grabacion UD para TEF - TRX ONLINE Grupo Retail - 17 Sept 2012 --- OVS
       If Val(EP.AUTH.NUMBER$) <> 0 Then Begin           ! Con autorizacion proceso
        TS.USERDATA$ = PACK$("20120917")       + ":" + \ ! Codigo del UD
                       Pack$(EP.CARD.NUMBER2$) + ":" + \ ! Numero de la tarjeta
                       PACK$(EP.VALOR.EPAY$)   + ":" + \ ! Valor del Pago
                       Pack$(EP.CUOTAS.QTY$)   + ":" + \ ! Numero de Cuotas
                       Pack$(EP.AUTH.NUMBER$)  + ":" + \ ! Numero de autorizacion
                       EP.FRANQUICIA$          + ":" + \ ! Nombre Franquicia
                       Pack$(EP.LOTE$)         + ":" + \ ! Numero de Lote
                       Pack$(EP.TAX.EPAY$)     + ":" + \ ! Impuesto reportado
                       Pack$(Gr.Tef.CodDif$)   + ":" + \ ! Codigo del diferido
                       Pack$("00")                       ! Cierre registro
        TS.TEMP1I1 = 99                  
        Call TSTPEC01
       EndIf
!---        
      EndIf    
      If EP.APPROV.CODE$ = "00" OR  \
         EP.APPROV.CODE$ = "08" OR  \
         EP.APPROV.CODE$ = "11" OR  \
         EP.APPROV.CODE$ = "76" OR  \
         EP.APPROV.CODE$ = "77" OR  \
         EP.APPROV.CODE$ = "78" OR  \
         EP.APPROV.CODE$ = "79" OR  \
         EP.APPROV.CODE$ = "80" OR  \
         EP.APPROV.CODE$ = "81" Then  \
      Begin 
        EP.STATE% = 2
        Call EP.DISPLAY.A.MESSAGE(EP.APPROV.DESC$)
        EP.APPROV.FOUND% = -1
      EndIf \
      Else  \
      Begin
        EP.STATE% = 4
        Exit Sub        
      EndIf
    EndIf \
    Else  \                          !  Otros estados de transaccion 
    Begin
      If EP.AMJ.STATUS$ = "3" Then \
      Begin
        GOSub PARSE.DISPLAY.REQUEST
        Call EP.DISPLAY.A.MESSAGE(EP.DISP.MESSAGE$)
      EndIf   
      If EP.AMJ.STATUS$ = "4" Then \
      Begin
        GOSub PARSE.PRT.HEADER
        Call EP.STORE.VOUCHER(EP.PRT.MESSAGE$,EP.PRT.CUT$)
        EP.PRT.CUT$ = "0"
        Call EP.STORE.EFTLINE(EP.PRT.MESSAGE$)
      EndIf   
      If EP.AMJ.STATUS$ = "5" Then \
      Begin
        GOSub PARSE.PRT.LINE
        If EP.PRT.VOUC$ = "1" Then \
          Call EP.STORE.VOUCHER(EP.PRT.MESSAGE$,EP.PRT.CUT$)
        If EP.PRT.CUST$ = "1" Then \
          Call EP.STORE.EFTLINE(EP.PRT.MESSAGE$)
      EndIf   
      If EP.AMJ.STATUS$ = "6" Then \       !  Transacccion finalizada satisfactoriamente
      Begin
        GOSub PARSE.PRT.CLOSE
        Call EP.STORE.VOUCHER(EP.PRT.MESSAGE$,EP.PRT.CUT$)
        EP.AMJ.STATUS$ = "2"               !  Sale como transaccion con respuesta del host
        EP.STATE% = 4                      !   Fin de ciclo  
        Call EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.APPL$ + EP.FUNCTION$  + EP.EPAY.TRANSNUM$ + "03", \ DE-1
             EP.IVA.BASE$ + EP.CASHBACK$,             \ DE-2
             EP.FECHA.PROC$ + RIGHT$(EP.DONACION$,8), \ DE-3
             EP.ANUL.TRANSNUM$,                       \ DE-4
             DATE$ + TIME$ + EP.CARD.BIN$,            \ DE-5
             EP.ANUL.RRN$ + EP.FECHA.VENC$)           ! DE-6 
        EP.VOUCHER.FOUND% = -1
      EndIf   
      If EP.AMJ.STATUS$ = "7" Then \
      Begin 
        GOSub PARSE.DATA.REQUEST
        Call EP.SAVE.KEYS 
        EP.KEYB.DATA$ = ""
        Call EP.GET.KBDATA(EP.DISP.MESSAGE$, EP.INI.RANGE$, EP.END.RANGE$, \
          EP.KEYB.DATA$)
        Call EP.RESTORE.KEYS
      EndIf
      If EP.AMJ.STATUS$ = "8" Then \
      Begin
        GOSub PARSE.DENTRY.REQUEST
        EP.INICIO.FOUND% = -1
        Call EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$, EP.DE4.DATA$, \
             EP.DE5.DATA$, EP.DE6.DATA$)
      EndIf   
      If EP.AMJ.STATUS$ = "9" Then \
      Begin
        GOSub PARSE.DENTRY.REQUEST
        EP.TRX.STATUS$ = MID$(EP.DE6.DATA$,17,1)
        Call EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$, EP.DE4.DATA$, \
             EP.DE5.DATA$, EP.DE6.DATA$)
        EP.STATE% = 4                       !   Fin de ciclo  
        If EP.TRX.STATUS$ = "0" Then  \
        Begin
          EP.TOHOST.FOUND% = -1
        EndIf  
      EndIf   
      If EP.AMJ.STATUS$ = "A" Then \
      Begin
        GOSub PARSE.DENTRY.REQUEST
        EP.TRX.STATUS$ = MID$(EP.DE6.DATA$,17,1)
        Call EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$, EP.DE4.DATA$, \
             EP.DE5.DATA$, EP.DE6.DATA$)
        EP.STATE% = 4                       !   Fin de ciclo  
        If EP.TRX.STATUS$ = "0" Then  \
        Begin
          EP.TOHOST.FOUND% = -1
        EndIf  
      EndIf   
    EndIf
    If  (EP.STATE% < 4) AND (EP.EXCEPTION$ = "") Then \   ! Si se continua el ciclo
    Begin    
      EP.MESSAGE$ = EP.APPL.STATUS$
      If EP.AMJ.STATUS$ = "7" Then \
        EP.MESSAGE$ = EP.MESSAGE$ + EP.KEYB.DATA$
      EP.MSGLEN$  = RIGHT$(String$(3,"0") + STR$(LEN(EP.MESSAGE$)),3)
      EP.MESSAGE$ = EP.APPL$ + EP.FUNCTION$ + EP.MSGLEN$ + EP.MESSAGE$
      If EP.FUNCTION$ = "21" OR  \
         EP.FUNCTION$ = "22" OR  \
         EP.FUNCTION$ = "23" Then \
      Begin
        GOSub SEND.CHEQUE.TO.AMJ
      EndIf \
      Else \
      Begin 
        GOSub SEND.DBCR.TO.AMJ
      EndIf
    EndIf
!
  Wend
!
  EP.TRX.PEND$     = ""
  EP.REV.FUNCTION$ = ""
  EP.FECHA.PEND$   = ""
  EP.REV.COUNT%    = 0
  EP.REV.APROB$    = ""
!
  If EP.FUNCTION$ = "11"  OR  \        ! unicamente para 
     EP.FUNCTION$ = "12"  OR  \
     EP.FUNCTION$ = "14" Then \        ! Transaccion tarjeta
  Begin
    If EP.INICIO.FOUND%      AND   \
       NOT EP.TOHOST.FOUND% Then   \
    Begin
      If EP.RESP.FOUND% Then       \
      Begin
        If EP.APPROV.FOUND% Then   \
        Begin
          If NOT EP.VOUCHER.FOUND% Then \
          Begin
            EP.TRX.PEND$ = EP.EPAY.TRANSNUM$ 
            EP.FECHA.PEND$ = EP.ECR.DATETIME$
          EndIf
        EndIf 
      EndIf \
      Else  \
      Begin
        EP.TRX.PEND$ = EP.EPAY.TRANSNUM$
        EP.FECHA.PEND$ = EP.ECR.DATETIME$
      EndIf
    EndIf
!
    If EP.TRX.PEND$ <> "" Then \
    Begin 
      If EP.APPROV.FOUND% Then \
      Begin 
        EP.REV.FUNCTION$ = "15"
      EndIf \
      Else \
      Begin
        EP.REV.FUNCTION$ = "13" 
      EndIf 
      Call EPAY.REVERSE.TRX
    EndIf
  EndIf 
!
!
  Exit Sub
!
!
SEND.DBCR.TO.AMJ:
!
!   Call Java appl manager routine
!----------------------------------------

    EP.METHOD$      = "runOther"
    EP.CLASS.GRAL$  = "com.appl.ApplKernel"
    EP.REQUEST$     ="C$"
    EP.RUNMETH$     ="appl.ApplManager.tarjeta"
    EP.EXCEPTION$=""
    EP.RETURNVALUE$ = ""
    EP.AMJ.MESSAGE$ = ""
!
 
!--- Variables Adicionadas para manejo de reverso por falla en proceso de autorizacion
!--- del teclado, OVS 9 de Julio de 2008  
  
  EP.GR.TRANSNUM$ = EP.EPAY.TRANSNUM$ 
  EP.GR.DATETIME$ = EP.ECR.DATETIME$
  
!--- Fin de lineas adicionadas  
 
    
    Call JavaCall.Initialize.Request(EP.CLASS.GRAL$,EP.METHOD$,EP.REQUEST$)
    Call JavaCall.AddParameter.String(EP.REQUEST$,EP.MESSAGE$)
    Call JavaCall.AddParameter.String(EP.REQUEST$,EP.RUNMETH$)
    EP.EXCEPTION$=""
    Call JavaCall.InvokeMethod.RETURNString(EP.REQUEST$,EP.RETURNVALUE$,\
        EP.EXCEPTION$)
!
    
    If LEN(EP.EXCEPTION$) > 0 Then \
    Begin
      Call EP.LINE.PRINT("Error en el envio",4100H)
      Call EP.LINE.PRINT(LEFT$(EP.EXCEPTION$,38),4100H)
      EP.ERRNCODE% = 1
      EP.GOOD.END% = 0
    EndIf \
    Else \
    Begin
      EP.ERRNCODE% = 0
      EP.GOOD.END% = -1
      EP.AMJ.MESSAGE$ = EP.RETURNVALUE$
    EndIf
Return
!
PARSE.DBCR.RESPONSE:
!
  EP.TRX.STATUS$  = ""
  EP.AUTH.NUMBER$ = ""
  EP.APPROV.CODE$ = ""
  EP.APPROV.DESC$ = ""
  EP.CARD.NUMBER$ = ""
  EP.CARD.NUMBER2$ = ""
  EP.COMERCIO$    = ""
  EP.EPAY.TERMINAL$= ""
  EP.CUOTAS.QTY$  = ""
  EP.FECHA.POSTEO$= ""
  EP.FECHA.PROC$  = ""
  EP.COD.PROC$    = ""
  EP.RRN$         = ""
  EP.TIPO.VAR$    = ""
  EP.FRANQUICIA$  = ""
  EP.BANCO$       = ""
  EP.PRODUCTO$    = ""
  EP.USER.DATA$   = ""
  EP.APPL.STATUS$ = ""
  EP.FECHA.VENC$  = ""
  EP.CARD.BIN$    = ""
!

!EP.CARD.NUMBER2$ = MID$(EP.AMJ.MESSAGE$,38,20)
EP.CARD.NUMBER2$ = MID$(EP.AMJ.MESSAGE$,38,26)
EP.LOTE$ =right$(EP.CARD.NUMBER2$,6)
EP.CARD.NUMBER2$ = LEFT$(EP.CARD.NUMBER2$,20)

!EP.AMJ.MESSAGE$ = MID$(EP.AMJ.MESSAGE$,1,37)  +  MID$(EP.AMJ.MESSAGE$,54,Len(EP.AMJ.MESSAGE$))
EP.AMJ.MESSAGE$ = MID$(EP.AMJ.MESSAGE$,1,37)  + MID$(EP.CARD.NUMBER2$,17,4)+ MID$(EP.AMJ.MESSAGE$,64,Len(EP.AMJ.MESSAGE$))

    If EP.M.LEN% >= 9 Then   \ 
      EP.TRX.STATUS$ = MID$(EP.AMJ.MESSAGE$,9,1)
    If EP.M.LEN% >= 10 Then  \
      EP.AUTH.NUMBER$ = MID$(EP.AMJ.MESSAGE$,10,6)
    If EP.M.LEN% >= 16 Then  \
      EP.APPROV.CODE$ = MID$(EP.AMJ.MESSAGE$,16,2)
    If EP.M.LEN% >= 18 Then  \
      EP.APPROV.DESC$ = MID$(EP.AMJ.MESSAGE$,18,20)
    If EP.M.LEN% >= 38 Then  \
      EP.CARD.NUMBER$ = MID$(EP.AMJ.MESSAGE$,38,4) ! "38,4"
!     EP.CARD.NUMBER$ = LEFT$(EP.CARD.NUMBER2$,4)
    If EP.M.LEN% >= 42 Then  \
      EP.COMERCIO$ = MID$(EP.AMJ.MESSAGE$,42,10)			! "42,10"
     If EP.M.LEN% >= 52 Then  \
      EP.EPAY.TERMINAL$ = MID$(EP.AMJ.MESSAGE$,52,8)
    If EP.M.LEN% >= 60 Then  \
      EP.CUOTAS.QTY$ = MID$(EP.AMJ.MESSAGE$,60,2)
    If EP.M.LEN% >= 62 Then  \
      EP.FECHA.POSTEO$ = MID$(EP.AMJ.MESSAGE$,62,4)
    If EP.M.LEN% >= 66 Then  \
      EP.FECHA.PROC$ = MID$(EP.AMJ.MESSAGE$,66,12)
    If EP.M.LEN% >= 78 Then  \
      EP.COD.PROC$ = MID$(EP.AMJ.MESSAGE$,78,6)
    If EP.M.LEN% >= 84 Then  \
      EP.RRN$ = MID$(EP.AMJ.MESSAGE$,84,12)
    If EP.M.LEN% >= 96 Then  \
      EP.TIPO.VAR$ = MID$(EP.AMJ.MESSAGE$,96,2)
!    EP.TIPO.VAR$ = STR$(EP.TV.TARJ%)
    If EP.M.LEN% >= 98 Then  \
      EP.FRANQUICIA$ = MID$(EP.AMJ.MESSAGE$,98,10)
    If EP.M.LEN% >= 108 Then  \
      EP.BANCO$ = MID$(EP.AMJ.MESSAGE$,108,10)
    If EP.M.LEN% >= 118 Then \
      EP.PRODUCTO$ = MID$(EP.AMJ.MESSAGE$,118,10)
    EP.USER.DATA$ = EP.FRANQUICIA$ + EP.BANCO$ + EP.PRODUCTO$
    If EP.M.LEN% >= 128 Then \
      EP.FECHA.VENC$ = MID$(EP.AMJ.MESSAGE$,128,4)
    If EP.M.LEN% >= 132 Then \
      EP.CARD.BIN$ = MID$(EP.AMJ.MESSAGE$,132,6)    
    If EP.M.LEN% >= 138 Then \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,138,2)
Return
!
SEND.CHEQUE.TO.AMJ:
!
!   Call Java appl manager routine
!--------------------------------------------------------------------
    EP.METHOD$      = "runOther"
    EP.CLASS.GRAL$  = "com.appl.ApplKernel"
    EP.REQUEST$     ="C$"
    EP.RUNMETH$     ="appl.ApplManager.Cheque"
    EP.EXCEPTION$=""
    EP.RETURNVALUE$ = ""
    EP.AMJ.MESSAGE$ = ""
!
    Call JavaCall.Initialize.Request(EP.CLASS.GRAL$,EP.METHOD$,EP.REQUEST$)
    Call JavaCall.AddParameter.String(EP.REQUEST$,EP.MESSAGE$)
    Call JavaCall.AddParameter.String(EP.REQUEST$,EP.RUNMETH$)
    EP.EXCEPTION$=""
    Call JavaCall.InvokeMethod.RETURNString(EP.REQUEST$,EP.RETURNVALUE$,\
        EP.EXCEPTION$)
!
    If LEN(EP.EXCEPTION$) > 0 Then \
    Begin
      Call EP.LINE.PRINT("Error en el envio",4100H)
      Call EP.LINE.PRINT(LEFT$(EP.EXCEPTION$,38),4100H)
      EP.ERRNCODE% = 1
      EP.GOOD.END% = 0
    EndIf \
    Else \
    Begin
      EP.ERRNCODE% = 0
      EP.GOOD.END% = -1
      EP.AMJ.MESSAGE$ = EP.RETURNVALUE$
    EndIf
Return 
!
PARSE.CHEQUE.RESPONSE:
!
  EP.TRX.STATUS$  = ""
  EP.AUTH.NUMBER$ = ""
  EP.APPROV.CODE$ = ""
  EP.APPROV.DESC$ = ""
  EP.COMERCIO$    = ""
  EP.EPAY.TERMINAL$= ""
  EP.FECHA.POSTEO$= ""
  EP.FECHA.PROC$  = ""
  EP.COD.PROC$    = ""
  EP.RRN$         = ""
  EP.TIPO.VAR$    = ""
  EP.FRANQUICIA$  = ""                        
  EP.BANCO$       = ""
  EP.PRODUCTO$    = ""
  EP.USER.DATA$   = ""
  EP.APPL.STATUS$ = ""
!
    If EP.M.LEN% >= 9 Then   \ 
      EP.TRX.STATUS$ = MID$(EP.AMJ.MESSAGE$,9,1)
    If EP.M.LEN% >= 10 Then  \
      EP.AUTH.NUMBER$ = MID$(EP.AMJ.MESSAGE$,10,6)
    If EP.M.LEN% >= 16 Then  \
      EP.APPROV.CODE$ = MID$(EP.AMJ.MESSAGE$,16,2)
    If EP.M.LEN% >= 18 Then  \
      EP.APPROV.DESC$ = MID$(EP.AMJ.MESSAGE$,18,20)
    If EP.M.LEN% >= 38 Then  \
      EP.COMERCIO$ = MID$(EP.AMJ.MESSAGE$,38,10)
    If EP.M.LEN% >= 48 Then  \
      EP.EPAY.TERMINAL$ = MID$(EP.AMJ.MESSAGE$,48,8)
    If EP.M.LEN% >= 56 Then  \
      EP.FECHA.POSTEO$ = MID$(EP.AMJ.MESSAGE$,56,4)
    If EP.M.LEN% >= 60 Then  \
      EP.FECHA.PROC$ = MID$(EP.AMJ.MESSAGE$,60,12)
    If EP.M.LEN% >= 72 Then  \
      EP.COD.PROC$ = MID$(EP.AMJ.MESSAGE$,72,6)
    If EP.M.LEN% >= 78 Then  \
      EP.RRN$ = MID$(EP.AMJ.MESSAGE$,78,12)
    If EP.M.LEN% >= 90 Then  \
      EP.TIPO.VAR$ = MID$(EP.AMJ.MESSAGE$,90,2)
!    EP.TIPO.VAR$ = STR$(EP.TV.TARJ%)
    If EP.M.LEN% >= 92 Then  \
      EP.FRANQUICIA$ = MID$(EP.AMJ.MESSAGE$,92,10)
    If EP.M.LEN% >= 102 Then  \
      EP.BANCO$ = MID$(EP.AMJ.MESSAGE$,102,10)
    If EP.M.LEN% >= 112 Then \
      EP.PRODUCTO$ = MID$(EP.AMJ.MESSAGE$,112,10)
    EP.USER.DATA$ = EP.FRANQUICIA$  + EP.BANCO$ + EP.PRODUCTO$
    If EP.M.LEN% >= 122 Then \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,122,2)
Return
!

!
PARSE.DISPLAY.REQUEST:
!
    EP.DISP.MESSAGE$ = ""
    EP.DISP.PARAM$   = ""
    EP.DISP.OPER$    = ""
    EP.APPL.STATUS$   = ""
!
    If EP.M.LEN% >= 9 Then \
      EP.DISP.MESSAGE$ = MID$(EP.AMJ.MESSAGE$,9,40)
    If EP.M.LEN% >= 49 Then \
      EP.DISP.PARAM$ = MID$(EP.AMJ.MESSAGE$,49,1)
    If EP.M.LEN% >= 50 Then \
      EP.DISP.OPER$ = MID$(EP.AMJ.MESSAGE$,50,1)
    If EP.M.LEN% >= 51 Then \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,51,2)
Return
!
PARSE.PRT.HEADER:
!
    EP.PRT.MESSAGE$ = ""
    EP.PRT.PARAM$   = ""
    EP.PRT.CUST$    = ""
    EP.PRT.CUT$     = ""
    EP.APPL.STATUS$  = ""
!
    If EP.M.LEN% >= 9 Then \
      EP.PRT.MESSAGE$ = MID$(EP.AMJ.MESSAGE$,9,38)
    If EP.M.LEN% >= 47 Then \
      EP.PRT.PARAM$ = MID$(EP.AMJ.MESSAGE$,47,1)
    If EP.M.LEN% >= 48 Then \
      EP.PRT.CUST$ = MID$(EP.AMJ.MESSAGE$,48,1)
    If EP.M.LEN% >= 49 Then \
      EP.PRT.CUT$ = MID$(EP.AMJ.MESSAGE$,49,1)
    If EP.M.LEN% >= 50 Then \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,50,2)
Return
!
PARSE.PRT.LINE:
!
    EP.PRT.MESSAGE$ = ""
    EP.PRT.PARAM$   = ""
    EP.APPL.STATUS$  = ""
    EP.PRT.CUST$  = ""
    EP.PRT.VOUC$  = ""
!
    If EP.M.LEN% >= 9 Then \
      EP.PRT.MESSAGE$ = MID$(EP.AMJ.MESSAGE$,9,38)
    If EP.M.LEN% >= 47 Then \
      EP.PRT.PARAM$ = MID$(EP.AMJ.MESSAGE$,47,1)
    If EP.M.LEN% >= 48 Then \
      EP.PRT.CUST$ = MID$(EP.AMJ.MESSAGE$,48,1)
    If EP.M.LEN% >= 49 Then \
      EP.PRT.VOUC$ = MID$(EP.AMJ.MESSAGE$,49,1)
    If EP.M.LEN% >= 50 Then \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,50,2)
Return
!
PARSE.PRT.CLOSE:
!
    EP.PRT.MESSAGE$ = ""
    EP.PRT.PARAM$   = ""
    EP.PRT.CUT$     = ""
    EP.APPL.STATUS$  = ""
!
    If EP.M.LEN% >= 9 Then \
      EP.PRT.MESSAGE$ = MID$(EP.AMJ.MESSAGE$,9,38)
    If EP.M.LEN% >= 47 Then \
      EP.PRT.PARAM$ = MID$(EP.AMJ.MESSAGE$,47,1)
    If EP.M.LEN% >= 48 Then \
      EP.PRT.CUT$ = MID$(EP.AMJ.MESSAGE$,48,1)
    If EP.M.LEN% >= 49 Then \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,49,2)
Return
!
PARSE.DATA.REQUEST:
!
    EP.DISP.MESSAGE$ = ""
    EP.INI.RANGE$     = ""
    EP.END.RANGE$     = ""
    EP.APPL.STATUS$   = ""
!
    If EP.M.LEN% >= 9 Then \
      EP.DISP.MESSAGE$ = MID$(EP.AMJ.MESSAGE$,9,40)
    If EP.M.LEN% >= 49 Then \
      EP.INI.RANGE$ = MID$(EP.AMJ.MESSAGE$,49,10)
    If EP.M.LEN% >= 59 Then \
      EP.END.RANGE$ = MID$(EP.AMJ.MESSAGE$,59,10)
    If EP.M.LEN% >= 69 Then \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,69,2)
Return
!
PARSE.DENTRY.REQUEST:
!
    EP.DE1.DATA$ = ""
    EP.DE2.DATA$ = ""
    EP.DE3.DATA$ = ""
    EP.DE4.DATA$ = ""
    EP.DE5.DATA$ = ""
    EP.DE6.DATA$ = ""
    EP.APPL.STATUS$ = ""
!
    If EP.M.LEN% >= 9 Then \
      EP.DE1.DATA$ = MID$(EP.AMJ.MESSAGE$,9,12)
    If EP.M.LEN% >= 21 Then \
      EP.DE2.DATA$ = MID$(EP.AMJ.MESSAGE$,21,20)
    If EP.M.LEN% >= 41 Then \
      EP.DE3.DATA$ = MID$(EP.AMJ.MESSAGE$,41,20)
    If EP.M.LEN% >= 61 Then \
      EP.DE4.DATA$ = MID$(EP.AMJ.MESSAGE$,61,20)
    If EP.M.LEN% >= 81 Then \
      EP.DE5.DATA$ = MID$(EP.AMJ.MESSAGE$,81,20)
    If EP.M.LEN% >= 101 Then \
      EP.DE6.DATA$ = MID$(EP.AMJ.MESSAGE$,101,20)
    If EP.M.LEN% >= 121 Then \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,121,2)
Return

End Sub
!
END

