!************************************************** 
!Empresa       : Grupo  Retail Ltda               *
!Programa      : INCUSACT.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Revision integridad archivos     *
!                EAMFBCUS Y EAMFBACT DE EMS       *
!**************************************************
! Modificaciones:


%ENVIRON C						   																		 ! Ambiente de controlador

String    Global UE.MSG$, ERRFX$, Finr$, XFecha$

String    Global APELLIDOS, NOMBRE, DIR1, DIR2, CIUDAD, FCUMP, TEL1, TEL2, SEXO, CEDULA, CUPON

String    Global \          
          CUSTNUM$,     RSTAT$,     LNAME$,     FNAME$,  \!
          ALINE1$,      ALINE2$,    CITY$,      STATE$,  \!
          ZIP$,         ZIPEXIT$,   HPHONA$,    HPHONE$, \!
          BPHONA$,      BPHONE$,    DRIVER$,     HSTORE$,\!
          CUSTDEMO$,    RESERVED$,  SSNBREXT$,  SSNBR$,  \!
          GENDER$,      INCOME$,    BDATE$,     EDATE$,  \!
          FSIZE$,       CHAGE1$,    CHAGE2$,    CHAGE3$, \!
          CHAGE4$,      CHAGE5$,    CHAGE6$,    CHAGE7$, \!
          TNBRA$,       TNBRR$,     TPADJP$,    TPADJN$, \!
          LADNBR$,      LRDNBR$,    LADATE$,    LRDATE$, \!
          CDATE$,       LRCDATE$,   CUSAUT$,    ALIAS$,  \!
          REFCUS$,      LASTRC$,    STATEEXT$,  ZIPASC$, \! Segun formato
          ZIPEXTASC$,   DIVEREXT$,  USERDATA$,  CUSTOMER$
          
String    Global  \
					TOTAL.POINTS, TOTAL.TRANS, REDEEMED.PTS, \
					AUTO.COUPONS, LAST.DATE, LAST.POINTS, STATUS.LEVEL, \
					MESSAGE, OPTIONS, DISCOUNT, MULTIPLIER, TARGET.COUPS, \
					ALT.CUSTNUM, PERIOD.POINTS, PERIOD.TRANS, PERIOD.REDEEM, \
					LAST.REDEEN, SALES.TOTAL, USER.DATA, LAST.TNUM, FECHA, PERIOD

Integer*1  Global U.Error%, U.Ind%
Integer*4  Global U.QtyOk%, U.QtyNok%, U.Qty%, Tot.Act%, Total.Kill%

Sub MSG.LOG
    Locate 23,1 : Print String$(78," ") 
    Locate 23,1 : Print "Msg: "+UE.MSG$
End Sub 

Function TRADUCE.ERROR                                       !
Integer*4 HX%, S%, SX%, SUM%
String    Z$
    HX% = ERRN                                               ! Traduccion de los
    ERRFX$=""                                                ! codigos de error
    FOR S% = 28 TO 0 STEP -4                                 ! del sistema operativo
        SX% = SHIFT(HX%,S%)                                  !
        SUM% = SX% AND 000FH                                 !
        IF SUM% > 9 THEN SUM% = SUM% + 55                   \!
        ELSE SUM% = SUM% + 48                                !
        Z$ = CHR$(SUM%)                                      !
        ERRFX$ = ERRFX$ + Z$                                 !
    NEXT S%                                                  !
End Function

Sub Fin.Proceso
    Locate 15,29 : Print "**** ESTADISTICAS ****"
    Locate 16, 1 : Print "Numero de Registros Sincronizados : ",Str$(Tot.Act%)
    Locate 17, 1 : Print "Numero de Registros Eliminados    : ",Str$(Total.Kill%)

    UE.MSG$ = "Fin de Proceso Terminado"
    Call Msg.Log
    Stop
End Sub 

Sub Valida.FBCUS(CLTE$)
String CLTE$
      
      CLTE$    = Str$(Val(CLTE$))
      CUSTNUM$ = Pack$(Right$("000000000000000000"+CLTE$,18))
      U.ERROR% = -1 
      Read Form "C9 C1 C17 C13 2C25 C20 C2 C3 2C2 C4 C2 C4 C20 2C3 C1 C2 C5 2C1 2C3 8C1 2C2 2C4 6C3 2C1 C9 C3 C1 C5 C4 C5 C16" \
		   ;#4 KEY CUSTNUM$; \
          CUSTNUM$,     RSTAT$,     LNAME$,     FNAME$,  \!
          ALINE1$,      ALINE2$,    CITY$,      STATE$,  \!
          ZIP$,         ZIPEXIT$,   HPHONA$,    HPHONE$, \!
          BPHONA$,      BPHONE$,    DRIVER$,     HSTORE$,\!
          CUSTDEMO$,    RESERVED$,  SSNBREXT$,  SSNBR$,  \!
          GENDER$,      INCOME$,    BDATE$,     EDATE$,  \!
          FSIZE$,       CHAGE1$,    CHAGE2$,    CHAGE3$, \!
          CHAGE4$,      CHAGE5$,    CHAGE6$,    CHAGE7$, \!
          TNBRA$,       TNBRR$,     TPADJP$,    TPADJN$, \!
          LADNBR$,      LRDNBR$,    LADATE$,    LRDATE$, \!
          CDATE$,       LRCDATE$,   CUSAUT$,    ALIAS$,  \!
          REFCUS$,      LASTRC$,    STATEEXT$,  ZIPASC$, \! Segun formato
          ZIPEXTASC$,   DIVEREXT$,  USERDATA$


      If U.Error% <> -1 Then Begin 
          U.Error% = -1
          CLTE$    = Str$(Val(CLTE$))
          CUSTNUM$ = Pack$(Right$("000000000000000000"+CLTE$,18))
          FSIZE$    = PACK$("00")                                                        
          CHAGE1$   = PACK$("00")                                                        
          CHAGE2$   = PACK$("00")                                                        
          CHAGE3$   = PACK$("00")                                                        
			    CHAGE4$   = PACK$("00")                                                        
			    CHAGE5$   = PACK$("00")                                                        
			    CHAGE6$   = PACK$("00")                                                        
			    CHAGE7$   = PACK$("00")                                                        
			    TNBRA$    = PACK$("0000")                                                      
			    TNBRR$    = PACK$("0000")                                                      
			    TPADJP$   = PACK$("00000000")                                                  
			    TPADJN$   = PACK$("00000000")                                                  
			    LADNBR$   = PACK$("000000")                                                    
			    LRDNBR$   = PACK$("000000")                                                    
			    LADATE$   = PACK$("000000")                                                    
			    LRDATE$   = PACK$("000000")                                                    
			    CDATE$    = PACK$(DATE$)                                                       
			    LRCDATE$  = PACK$("000000")                                                    
			    CUSAUT$   = " "                                                                
			    ALIAS$    = " "                                                                
			    REFCUS$   = PACK$("000000000000000000")                                        
			    LASTRC$   = PACK$("000000")                                                    
			    STATEEXT$ = " "                                                                
			    ZIPASC$   = "00000"                                                            
			    ZIPEXTASC$= "0000"                                                             
			    DIVEREXT$ = "     "                                                            
			    USERDATA$ = PACK$("00000000000000000000000000000000")                          
          ZIPEXIT$  = PACK$("0000")                                                      
          ZIP$      = Pack$("000000")                                                    
          HPHONA$   = Pack$("0000")                                                      
                                                                                         
          RSTAT$ = " "                                                                   
          LNAME$ = " "                                                                   
          FNAME$ = "SIN DEFINIR"                                                         
          ALINE1$ = " "                                                                  
          ALINE2$ = " "                                                                  
          CITY$   = "CALI"                                                               
          STATE$  = "  "                                                                 
          HPHONE$ = " "                                                                  
          BPHONE$ = " "                                                                  
          GENDER$ = "M"                                                                  
          BDATE$  = Pack$(DATE$)                                                         
                                                                                         
          Write Form "C9 C1 C17 C13 2C25 C20 C2 C3 2C2 C4 C2 C4 C20 2C3 C1 C2 C5 2C1 2C3 8C1 2C2 2C4 6C3 2C1 C9 C3 C1 C5 C4 C5 C16";#4; \!
          CUSTNUM$,     RSTAT$,     LNAME$,     FNAME$,  \!
          ALINE1$,      ALINE2$,    CITY$,      STATE$,  \!
          ZIP$,         ZIPEXIT$,   HPHONA$,    HPHONE$, \!
          BPHONA$,      BPHONE$,    DRIVER$,    HSTORE$, \!
          CUSTDEMO$,    RESERVED$,  SSNBREXT$,  SSNBR$,  \!
          GENDER$,      INCOME$,    BDATE$,     EDATE$,  \!
          FSIZE$,       CHAGE1$,    CHAGE2$,    CHAGE3$, \!
          CHAGE4$,      CHAGE5$,    CHAGE6$,    CHAGE7$, \!
          TNBRA$,       TNBRR$,     TPADJP$,    TPADJN$, \!
          LADNBR$,      LRDNBR$,    LADATE$,    LRDATE$, \!
          CDATE$,       LRCDATE$,   CUSAUT$,    ALIAS$,  \!
          REFCUS$,      LASTRC$,    STATEEXT$,  ZIPASC$, \! Segun formato
          ZIPEXTASC$,   DIVEREXT$,  USERDATA$
          
          If U.Error% = -1 Then Begin
            U.QtyOk% = U.QtyOk% + 1 
            Locate 11,27 : Print Str$(U.QtyOk%)          
          EndIf Else Begin
            U.QtyNok% = U.QtyNok% + 1
            Locate 12,27 : Print Str$(U.QtyNok%)
            Write #2; Unpack$(CUSTNUM$),LNAME$+FNAME$,Str$(U.Qty%),UE.MSG$,FINR$
          EndIf    
      EndIf   
End Sub 

Sub Lista.Act(Clte$,fecha$)
String Clte$, fecha$ 

If Val(Fecha$) <= Val(Xfecha$) Then Begin 
   Write Form "C18 C1 C6 C2";#11;Clte$,",",fecha$,finr$
   U.QtyOk% = U.QtyOk% + 1 
   Locate 11,27 : Print Str$(U.QtyOk%)          
EndIf Else Begin 
   U.QtyNok% = U.QtyNok% + 1
   Locate 12,27 : Print Str$(U.QtyNok%)
EndIf    

End Sub 

Sub RECORRER.ACT
Integer*4 REC.NO, I%, Blk.Num, MAX.R.SEC, X, R.S, R, Ncli%
String H$, PZERO$, REG$, C01$, C02$, C03$, C04$, KEY$
Integer*2 Key.Len, Rec.Len
Ncli% = 0 
Open "EAMFBCUS" KEYED RECL 254  AS 4							    	     ! Apertura Eamfbcus
Open "EAMFBACT" DIRECT RECL 512 AS 33   										 ! Apertura control de promociones

REC.NO = 1					                                         ! Initialize Counter
I% = 1						                                           ! Initialize Counter
Read Form "T43 I4 I2 T55 I2 C456"; #33,REC.NO;              \! Read Firts Record
     BLK.NUM, REC.LEN, KEY.LEN, H$		                       ! 
PZERO$ = PACK$(STRING$(2*KEY.LEN,"0"))                       ! Armed Key Control
MAX.R.SEC = 508/REC.LEN                                      ! Length record
U.Error% = -1                                                ! Control de errores
For REC.NO = 2 TO BLK.NUM                                    ! Cicle to read all blocks  
  REG$=""
  Read Form "T5 C508"; #33, REC.NO;  H$                      ! H$ contains block 
  X = 1 : R.S = 0 : KEY$ = Mid$(H$,X,KEY.LEN)                ! Extract First key
  While KEY$  NE  PZERO$                                     ! Inside sector loop 
    R.S = R.S + 1                                            ! Records On This Sector 
    R = R + 1                                                !
    C01$= Unpack$(MID$(H$,X+0,9))                            ! Codigo del cliente
    C02$= Unpack$(MID$(H$,X+9,4))                            ! 
    C02$= Unpack$(MID$(H$,X+13,2))                           ! 
    C02$= Unpack$(MID$(H$,X+15,4))                           ! 
    C02$= Unpack$(MID$(H$,X+19,3))                           !     
    C02$= Unpack$(MID$(H$,X+22,3))                           ! Fecha de Ultimo Movimiento

    If Val(C01$) <> 0 Then Begin 														 ! Cliente Valido
       Ncli% = Ncli% + 1 
       Locate 10,35 : Print Ncli%
       If U.Ind% = 1 Then \
          Call VALIDA.FBCUS(C01$) Else \
          Call Lista.Act(C01$,C02$)
    EndIf 
    X = X + REC.LEN                                          ! Index to next key
    KEY$ = Mid$(H$,X,KEY.LEN)                                ! Pick up next key
    If R.S = MAX.R.SEC Then KEY$ = PZERO$                    ! If EOF() record or file
  Wend
Next REC.NO
Close 33
Close 4 
End Sub 


Sub Pantalla.Principal
   CLEARS
   Locate 2, 4: Print CHR$(218)+STRING$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "**** REVISION INTEGRIDAD Y DEPURACION E.M.S   V.10   ****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Octubre    2  2007  G.R.***"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+STRING$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
   Locate 8, 1: Print "Actualizando Archivo   : EAMFBCUS.DAT"
   Locate 9, 1: Print "Actualizando Archivo   : EAMFBACT.DAT"
   Locate 10,1: Print "Registros Procesados   : "
   Locate 11,1: Print "Registros Actualizados : " 
   Locate 12,1: Print "Registros No Procesados: " 
   Locate 13,1: Print "Ejecutando Proceso     : " 
   Locate 23,1: Print "Msg : "
   FINR$ = CHR$(13) + CHR$(10)   ! Marca de fin de registro
End Sub

Sub Eliminar.Clientes
String Clte$, Fecha$
  Open "ADX_UDT1:CLIENTES.TXT" As 11
  Open "EAMFBCUS" KEYED RECL 254 AS 3        ! Abre Archivo Clientes
  Open "EAMFBACT" KEYED RECL 101 AS 4		     ! ABRE ARCH. ACTIVIDAD CLIENTE
  U.QtyOk% = 0 
  Locate 11,27 : Print String$(40," ")          
  If End #11 Then FinProceso
  While (1)
     Read #11;Clte$, Fecha$
     U.QtyOk% = U.QtyOk% + 1 
     Clte$ = Pack$(Right$("000000000000000000"+CLTE$,18))
     Locate 11,27 : Print Str$(U.QtyOk%)          
     Delrec 3; Clte$
     Delrec 4; Clte$
  Wend 
  
  FinProceso:
    Total.Kill% = U.QtyOk%
    Close 11
End Sub

Sub Eliminar.Movimiento
String Fecha$, Mesadd$, Mes$, Ano$, Dia$
Integer*4 NewMes%
   U.Ind%  = 2
   MesAdd$ = "6"
   Mes$    = Mid$(date$,3,2)
   Ano$    = Mid$(Date$,1,2)
   Dia$    = Mid$(Date$,5,2)
   NewMes% = Val(Mes$) + Val(MesAdd$)
   If Newmes% > 12 Then Begin 
      Newmes% = Newmes% - 12
   EndIf 
   If Val(Mes$) <= 6 Then Begin 
      Ano$ = Str$( Val(Ano$) - 1)
   EndIf 
   
   Xfecha$ = Right$("00"+Ano$,2) + Right$("00"+Str$(NewMes%),2) + Dia$
   Locate 13,27: Print "Eliminando Movimientos Anteriores al 20"+XFecha$
   Call Recorrer.Act
   Close 11
   Locate 11,1: Print "Eliminando Clientes    : " 
   Call Eliminar.Clientes
   
End Sub 


!---
!------- Bloque Principal
!---

On Error GoTo Errores.Aplicativo																					! Control de proceso de errores
U.Ind% = 1
Call Pantalla.Principal																									  ! Pantalla Principal
Create "ADX_UDT1:ERRINTEG.TXT"  AS 2                                      ! Errores proceso de carga
Locate 13,27: Print "Verificando Integridad EAMFBACT"
Call RECORRER.ACT																													! Verificacion del ACT
Locate 10,27: Print String$(40," ")
Locate 11,27: Print String$(40," ")
Locate 12,27: Print String$(40," ")
Create "ADX_UDT1:CLIENTES.TXT"  AS 11 Buffsize 2048                       ! Errores proceso de carga
Tot.Act% = U.QtyOk%
U.QtyOk% = 0 :  U.QtyNok% = 0 : U.Qty% = 0 
Locate 11,1: Print "Clientes a Eliminar    : " 
Call Eliminar.Movimiento
Call Fin.Proceso


Errores.Aplicativo:
  U.Error% = 0
  CALL TRADUCE.ERROR
  UE.MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
  
  If ERR = "IH" Then Resume 
  
  If ERRF% = 1 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO ADX_UDT1:CLIENTES.TXT NO EXISTE... "
     Call Msg.Log
     Stop
  EndIf 
  If ERRF% = 4 AND ERR = "KF" THEN Begin
     Resume 
  EndIf 
  
  If ERRF% = 4 AND ERR = "EF" THEN Begin
     Resume 
  EndIf 

  If ERRF% = 4 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO MAESTRO DE CLIENTES NO EXISTE... "
     Call Msg.Log
     Stop        
  EndIf 
  If ERRF% = 1 AND ERR = "EF" THEN Begin
     Call Fin.Proceso												   														! Fin de ejecucion del programa     
  EndIf 
  CALL MSG.LOG
  STOP

