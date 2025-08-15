!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : AJUSTE.BAS                       *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Agosto 21 de 2.002               *
!Observaciones : Carga promociones especiales para*
!                clientes y productos modulo de   *
!                mercadeo                         *
!**************************************************

%ENVIRON C					! Ambiente Controlador

String    GLOBAL ERRFX$, MSG$, DATA$, ARCHIVO$
INTEGER*1 GLOBAL BAN.PRG%, POS%,TS.ERROR, Terror%, Topen%, I%

%INCLUDE BASROUT.J86

Function TRADUCE.ERROR                                       !
INTEGER*4 HX%, S%, SX%, SUM%
String Z$
    HX% = ERRN                                               ! Traduccion de los
    ERRFX$=""                                                ! codigos de error
    For S% = 28 TO 0 STEP -4                                 ! del sistema operativo
        SX% = SHIfT(HX%,S%)                                  !
        SUM% = SX% AND 000FH                                 !
        If SUM% > 9 Then SUM% = SUM% + 55                   \!
        ELSE SUM% = SUM% + 48                                !
        Z$ = CHR$(SUM%)                                      !
        ERRFX$ = ERRFX$ + Z$                                 !
    Next S%                                                  !
End Function

Sub PANTALLA
   CLEARS
   Locate 2, 4: Print CHR$(218)+String$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "**** ACT. PROMOCIONES MODULO MERCADEO     Ver. 1.00 ****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revisi¢n Software Septiembre 12 2010 G.R. ***"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+String$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
   Locate 23,1: Print "Msg:"
End Sub

Sub BARRA
   POS% = POS% + 1
   If POS% = 1 Then Locate  12,45: Print "|"
   If POS% = 2 Then Locate  12,45: Print "/"
   If POS% = 3 Then Locate  12,45: Print "-"
   If POS% = 4 Then Locate  12,45: Print "\"
   If POS% = 4 Then POS% = 0
End Sub
!--- Fin barra de estado

Sub PROCESA
 String UE.LOG$, UE.COD$, M1$, M2$, M3$, M4$, M5$, M6$, UE.CUPON$, KEY$, M7$, M8$, M9$
 String C1$, C2$, C3$, C4$, C5$, C9$
 String X$, PROMOS$(1), X.A$
 Integer*1 IND.GRABA%, II%, X%, Y%
 TOpen% = 0
 Again.Procesa:
 Terror% = -1
 Open ARCHIVO$ As 1
 If Terror% = 0 Then Begin 
 	  Wait ; 1800
 	  TOpen% = TOpen% + 1 
 	  If TOpen% > 5 Then Stop 
 	  GoTo Again.Procesa
 Endif 
 
 If End #1 Then FIN.CARGA
 Locate 12,15: Print "Procesando Informacion ..."
 POS% = 0
 !--- Estructura archivo de carga
 !--- Tipo promocion, Codigo Itm/Clte, Msg, Vlr Comparacion, Lista Cliente, codigo promocion
 !30 = Presencia de un producto 
 !31 = Presencia de un cliente
 !32 = No presencia de una sección 
 !33 = Compra mínima 
 !34 = Por no presencia de un PLU 
 !35 = Presencia de un departamento
 !36 = Presencia de una forma de pago
 
 While (1)
   Call BARRA
   Read #1;C1$, C2$, C3$, C4$, C5$, C9$
   C1$ = Right$("00"+C1$,2)                    ! Tipo de promocion
   C2$ = Right$("0000000000000000"+C2$,16)     ! Plu - Cliente Promocion
   C3$ = Right$("0000"+C3$,4)                  ! Mensaje Promocion
   M1$ = Pack$(C1$ + C2$)                      ! Arma llave del registro
   Print "ALMACENA "+UNPACK$(M1$)
   If C1$ <> "31" Then Begin 									 ! Si diferente a lista de clientes
             
   EndIf Else Begin
    	 	 M2$ = Pack$("0000")     ! Empaqueta Mensaje
         M3$ = PACK$("00000000000000000000")      ! Relleno
         C2$ = "1400140114021403140414051406140714081409141014111412141314141415141614171418141914201421"
         C4$ = Pack$(Left$(C2$+String$(90,"0"),90))   ! Cliente Objetivo

      Write Form "C9 C2 C10 C45";#33;         \! Almacena Informacion
             M1$,			                        \! Codigo del articulo / Cliente
             M2$,                             \! Mensaje Publicitario
             M3$,                             \! Valor Compra Minima
             C4$                               ! Lista de cliente a quien va dirigida la promocion
   EndIf 
   M2$ = PACK$("0000")
   If UNPACK$(M2$) <> "0000" Then \
      Call Osshell("C:\GRMODUL\MSGCARGA.286 "+"C:\PROMODAT\MSGD"+UNPACK$(M2$)+".PRM") ! Carga mensaje
      
   !EndIf 

 Wend
 FIN.CARGA:
    Close 1 
    Close 33 
End Sub

Sub TERMINAR
 Locate 23,1: Print String$(78," ")     	        ! si no existe lo crea. 
 Locate 23,1: Print "Msg: Fin Ejecucion Programa ..."	!
 Stop							! Fin del programa
End Sub
!--- Fin de terminar


!------
!--- Bloque Principal
!------

ON ERROR GOTO ERRORES
!ARCHIVO$ = COMMAND$

ARCHIVO$ = "/PROMODAT/PRMC1400.PRM"
CALL PANTALLA					! Pantalla principal
OPEN "MKTP:MKPDEFPR.DAT" KEYED RECL 66 AS 33 ! Apertura logos
CALL PROCESA					! Procesa Carga
CALL TERMINAR

!--- Definicion de las rutinas de error
ERRORES:
         If ERRF% = 4 AND                                 \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") Then Begin          ! el archivo de control
            Locate 23,1: Print String$(78," ")             ! si no existe lo crea.
            Locate 23,1: Print "Msg: Error, Maestro de Promociones no Existe ..."
            STOP                                           ! retorna ejecucion
         EndIf                                             ! del programa
         If ERRF% = 4 AND ERR = "EF" Then Begin           \! Si encuentra fin de 
            BAN.PRG% = 0				   ! ejecucion normal del
            RESUME
         EndIf
         If ERRF% = 33 AND ERR = "EF" Then Begin           \! Si encuentra fin de 
            BAN.PRG% = 0				   ! ejecucion normal del
            RESUME
         EndIf
         If ERRF% = 1 AND                                 \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") Then Begin          ! el archivo de control
            Terror% = 0 
            Locate 23,1: Print String$(78," ")             ! si no existe lo crea.
            Locate 23,1: Print "Msg: Archivo de Carga No Existe ..."
            Resume                                         ! retorna ejecucion
         EndIf                                             ! del programa
         CALL TRADUCE.ERROR
         MSG$ = "Msg: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
         Locate 23,1:Print MSG$                            ! Presenta Error pantalla
