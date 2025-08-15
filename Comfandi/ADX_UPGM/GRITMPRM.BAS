!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRITMPRM.BAS                     *
!Autor         : Oscar Valencia                   *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Consulta Articulos en promocion  *
!**************************************************

%ENVIRON C

String    Global ERRFX$, MSG$, IR.USERDATA$
Integer*1 Global TS.ERROR, J%
%INCLUDE EAMITEMR.J86

Function TRADUCE.ERROR                                       !
Integer*4 HX%, S%, SX%, SUM%
String    Z$
    HX% = ERRN                                               ! Traduccion de los
    ERRFX$=""                                                ! codigos de error
    FOR S% = 28 TO 0 STEP -4                                 ! del sistema operativo
        SX% = SHIfT(HX%,S%)                                  !
        SUM% = SX% AND 000FH                                 !
        If SUM% > 9 Then SUM% = SUM% + 55                   \!
        Else SUM% = SUM% + 48                                !
        Z$ = CHR$(SUM%)                                      !
        ERRFX$ = ERRFX$ + Z$                                 !
    NEXT S%                                                  !
End Function

Sub Busqueda.Producto

TS.ERROR = -1
%Include EAMIRRD1.J86                                       ! Busqueda Item
If TS.ERROR <> -1 Then Begin																! Si Error de Busqueda
	 IR.ITEMNAME$ = "PRODUCTO NO EXISTE"                      ! Msg al Usuario
	 IR.SALEPRIC$ = Pack$("00")                               !
EndIf Else Begin																						!
   If UnPack$(IR.INDICAT2$) = "05" Then Begin               ! Metodo de precio 5 
   	  IR.ITEMCODE$ = IR.SALEQUAN$ + IR.SALEPRIC$            ! Busca articulo ligado
   	  %Include EAMIRRD1.J86                                 ! Busqueda del Item
   	  If TS.ERROR = 0 Then Begin                            ! Item No Existe
	       IR.ITEMNAME$ = "PRODUCTO NO EXISTE"                ! Msg al Usuario
	       IR.SALEPRIC$ = Pack$("00")                         !
   	  EndIf 
   EndIf
EndIf

   
End Sub 

Sub Mensaje(Xmsg$)
String XMSG$
  Locate 23, 1 : Print String$(77," ")
  Locate 23, 1 : Print XMSG$
  Wait ; 1200
End Sub 

Sub Consulta.Promociones
Integer*4 REC.NO, I%, Blk.Num, MAX.R.SEC, X, R.S, R, Aplicada%
String    H$, PZERO$, REG$, C01$, C02$, C03$, C04$, C05$, C06$,  KEY$, A$ 
Integer*2 Key.Len, Rec.Len, Lpanta%, Mes1%, Mes2%
String    X1$, X2$, X3$, X4$
   REC.NO = 1					             														 ! Initialize Counter
   I% = 1						               														 ! Initialize Counter

   For J% = 6 To 22
     Print String$(77," ")
   Next J% 
   Locate 6, 1
   Print " "
   Print "Promocion   F. Inicial     F. Final    Tipo    ValorDscto "
   TS.ERROR = -1
   Open "MKTP:PRMADMON.DAT" KEYED RECL  9 AS 31 														! Apertura Control de promociones
   If TS.ERROR <> -1 Then Begin																							! Error en la apertura
	    Msg$ = "Error Apertura Maestro Administrador Promociones"
	    Call Mensaje(Msg$)
	    Exit Sub 
   EndIf
   TS.ERROR = -1
   Open "MKPPRITM" DIRECT RECL 512 AS 33
   If TS.ERROR <> -1 Then Begin																							! Error en la apertura
	    Msg$ = "Error Apertura Maestro Promociones Producto"
	    Call Mensaje(Msg$)
	    Exit Sub 
   EndIf

Read Form "T43 I4 I2 T55 I2 C456"; #33,REC.NO;                          \! Read Firts Record
BLK.NUM, REC.LEN, KEY.LEN, H$		                                      ! 
PZERO$ = PACK$(STRING$(2*KEY.LEN,"0"))                                   ! Armed Key Control
Max.R.SEC = 508/REC.LEN                                                  ! Length record
For REC.NO = 2 TO BLK.NUM                                    ! Cicle to read all blocks  
  REG$=""
  Read Form "T5 C508"; #33, REC.NO;  H$                       ! H$ contains block 
  X = 1 : R.S = 0 : KEY$ = Mid$(H$,X,KEY.LEN)                 ! Extract First key
  While KEY$  NE  PZERO$                                      ! Inside sector loop 
    R.S = R.S + 1                                             ! Records On This Sector 
    R = R + 1                                                 !
    C01$= Unpack$(MID$(H$,X+0,8))                             ! Item promocionado 
    C01$ = Left$(C01$,12)                                     ! PLU DEL PRODUCTO
    C02$ = Right$(C01$,4)                                     ! Cliente Objetivo
    C03$ = MID$(H$,X+8,1)                                     ! Indicador de promocion
    C04$ = Unpack$(MID$(H$,X+9,4))                            ! Dscto aplicar
    C05$ = Unpack$(MID$(H$,X+13,2))                           ! Link a entregar
    C05$ = Unpack$(MID$(H$,X+15,2))                           ! Promocode
    C05$ = Unpack$(MID$(H$,X+17,12))                          ! Data Variable
    C06$ = Unpack$(MID$(H$,X+29,02))                          ! Numero de promocion
    If C03$ = "1" Then A$ = "Dvl"
    If C03$ = "2" Then A$ = "Ptg"
    If C03$ = "3" Then A$ = "Pfp"
    If C03$ = "4" Then A$ = "Lnk"
    If Val(C01$) = Val(UnPack$(IR.ITEMCODE$)) Then Begin
    	 TS.ERROR = -1
       Read Form "C2 2C3 C1";#31 KEY Pack$(C06$); X1$, X2$, X3$, X4$
       If TS.ERROR =  -1 Then BEGIN 
       	  X2$ = Unpack$(X2$)
       	  X3$ = Unpack$(X3$)
       	  X2$ = "20"+Left$(X2$,2)+"/"+MID$(X2$,3,2)+"/"+Right$(X2$,2)
       	  X3$ = "20"+Left$(X3$,2)+"/"+MID$(X3$,3,2)+"/"+Right$(X3$,2)
       EndIf Else BEGIN
          X2$ = "ERROR FECHA "
          X3$ = "ERROR FECHA "
       EndIf
       Print "     " + C06$ + "   " + X2$ + "   " + X3$ + "    " + A$ + "       " + C04$ 
    EndIf
    X = X + REC.LEN                                          ! Index to next key
    KEY$ = Mid$(H$,X,KEY.LEN)                                ! Pick up next key
    If R.S = MAX.R.SEC Then KEY$ = PZERO$                    ! If EOF() record or file
  Wend
Next REC.NO
   
   Close 33 : Close 31
   
End Sub 

On Error GoTo ErrFail
Clears
TS.ERROR = -1
Open "EAMITEMR" KEYED RECL  77 AS 4   						 					! Apertura Maestro de articulos
If TS.ERROR <> -1 Then Begin																! Error en la apertura
	 Msg$ = "Error Apertura Maestro de Articulos"
	 Call Mensaje(Msg$)
	 Stop 
EndIf

While (1)
   Locate 1, 1 : Print "Consulta Promocion Articulos                                 Grupo Retail Ltda"
   Locate 2, 1 : Print String$(77," ")
   Locate 23,1 : Print "Ingrese Codigo del Articulo a consultar, Presione 0 y Enter para Terminar"
   locate 2, 1 : Input "Item Consultar  :";IR.ITEMCODE$
   IR.ITEMCODE$ = Pack$(Right$("000000000000"+IR.ITEMCODE$,12))
   If IR.ITEMCODE$ = Pack$("000000000000") Then GoTo Final.Consulta
   Call Busqueda.Producto
   Print "------------------------------------------------------------------------------"
   Msg$ = Str$(Val(UnPack$(IR.SALEPRIC$)))
   Msg$ = Right$("            "+Msg$,12)
   Locate 4, 1
   Print "Item Consultado :" + UnPack$(IR.ITEMCODE$) + "   " + IR.ITEMNAME$ 
   Print "Precio Producto :"+Msg$
   If TS.Error = -1 Then Begin
      Call Consulta.Promociones
   EndIf Else Begin
   	  For J% = 6 To 22
   	    Print String$(77," ")
   	  Next J% 
   EndIf
Wend 
final.consulta:   
 Close 4
 Locate 24,1
 Stop 

ErrFail:
     Call TRADUCE.ERROR
     TS.ERROR = 0
     If ERR = "IH" Then Resume 
     If ERR = "KF" Then Resume 
     If ERR = "OE" OR ERR = "FU" Then Resume 
     If ERR = "EF" Then Resume 
     MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
     Call Mensaje(MSG$)
		 Stop
