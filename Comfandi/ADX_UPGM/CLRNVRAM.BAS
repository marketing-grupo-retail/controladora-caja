!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : CLRNVRAM.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Julio 14 del 2.004               *
!Observaciones : Inicializacion de la NVRAM       *
!                de la terminal para numeracion   *
!                fiscal                           *
!**************************************************

%ENVIRON T		                 ! Ambiente de terminal

Sub Reset.HARD.TOTALS
String FECHA.TRX$, Gr.Fis.HTNtaVta$, Gr.Fis.HTFacVta$, Gr.Fis.HTNtaCred$, \
       Gr.Fis.FecTrx$, Gr.Fis.Almacen2$, Gr.Fis.Relleno$, FIS.F16$
    OPEN "ANDISPLAY:" AS 30                            ! open the alpha display
    OPEN "CR:"     AS 34															 ! Open Printer Customer
    OPEN "TOTRET:" AS 45															 ! Open Hard Totals
    FECHA.TRX$ = "20" + DATE$ + TIME$                  ! Agrega Fecha y Hora
    FECHA.TRX$ = Pack$(FECHA.TRX$)                     ! Empaqueta Fecha
    Write Form "C40";#30;"INICIALIZANDO HARDTOTALS"
    WAIT ; 1200
    Gr.Fis.HTNtaVta$  = Pack$("00000000")
    Gr.Fis.HTFacVta$  = Pack$("00000000")
    Gr.Fis.HTNtaCred$ = Pack$("00000000")
    Gr.Fis.Almacen2$  = Pack$("0000")
    Gr.Fis.Relleno$   = String$(40,"0") 
    FIS.F16$ = "3C4 C6  C2 C40"
    Write Form FIS.F16$;#45,16315;                   \!
             Gr.Fis.HTNtaVta$ ,                      \! Nota de Venta
             Gr.Fis.HTFacVta$ ,                      \! Factura de Venta
             Gr.Fis.HTNtaCred$,                      \! Nota de Credito
             Gr.Fis.FecTrx$   ,                      \! Fecha de Transaccion
             Gr.Fis.Almacen2$ ,                      \! Almacen de la numeracion
             Gr.Fis.Relleno$                          !
             
    Write Form "C38";#34;"HARDTOTALS INICIALIZADOS"
    Close 45
    Close 34
End Sub  

On ERROR GOTO ERROR.LABEL:       
Call Reset.HARD.TOTALS
Exit.POINT:
 Stop 

Error.LABEL:
  If (ERR = "DF") Then Begin   											  ! Hard Totals already open - nee bother.
     Resume
  EndIf Else Begin
  	If ERRF% = 45 Then \
  	 Write Form "C38";#34;"ERROR APERTURA HARDTOTALS"
     Resume Exit.POINT:                               ! Let the base handle nasty errors.
  EndIf
