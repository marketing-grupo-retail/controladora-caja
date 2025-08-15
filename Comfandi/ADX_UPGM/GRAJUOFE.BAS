!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRAJUOFE.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Ajuste descuentos Ofertas por    *
!                Variacion Precios                *
!**************************************************

%ENVIRON C						   																										! Ambiente de controlador

Integer*1 Global Terror%
String    Global Param2$, Dato.So$, FileOut$
Integer*4 Global Tot.Aju%, Tot.del%

Sub ADXSERVE(RET,FUNC,PARM1,PARM2) External                  								! Msg background
   Integer*4 RET
   Integer*2 FUNC,PARM1
   String    PARM2
End Sub

Sub Write.Data
String Rbuffer$, X.Lec$, X.Costo$, X.Recibo$, X.Vlr$, X.Signo$
String X.Pago$, X.Caja$, X.Trx$, X.Tmp$, X.Forma$, Finr$
Integer*4 X.Len%, X.J%, X.Comp%
      FINR$ = Chr$(13) + Chr$(10)																			! fin de registro
      Rbuffer$ = Param2$																							! 
      X.Len% = Len(Rbuffer$)						  					  								! Toma longitud del registro
      X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
      Write form X.Lec$; #51 ; Rbuffer$, Finr$           							! Graba registro
End Sub 

Sub MENSAJE
Integer*4 PP
 If DATO.SO$ = "1" Then Call ADXSERVE(PP,26,1,Param2$) \
  Else Print Param2$
  Call Write.Data
End Sub 

Sub VERIFICA.OFERTA																													! Validacion de Ofertas
Integer*4 Ofe%, Vta.Of%, Lig1%, DES.99%, VTA.TO%, LIG2%, Des.New%
Integer*1 IND1OF%																														!
String    INDIS$,VALOF$,LIG99$,NOMOFE$,CUSER$, GR.DATA$, KEY.OF$, PREFOF$, \!
          KEYIND$, FIS.KIT$,RELE1$,LIGO$,DESOFE$,RELE2$, GR.DAT2$,         \!
          PLUOF$, LIGA$, RELE3$, IND.012$, KEY.MER$
  Param2$ = String$(40,"-")
  Call Mensaje          

  Param2$ = "Verificacion Modulo Ofertas - Grupo Retail 25-Sep-2014"
  Call Mensaje          
  Param2$ = "Inicia Proceso "+Date$+" Hora: "+Time$
  Call Mensaje          
  For OFE% = 1 TO 999                                    										! Para todas las Ofertas
      VTA.TO% = 0
      DES.99% = 0
      PREFOF$="999999"+Right$("0000"+STR$(OFE%),4)       										! Prefijo ofertas
      KEY.OF$ = Right$("0000" + STR$(OFE%),4)            										! Arma llave Ofertas
      KEY.OF$ = Pack$("99999999" + KEY.OF$)              										! con prefijo
      Terror% = -1																													! Control de errores
      Read Form "C8 I1 C7 C6 C2 C18 C4 C31";                               \! Lectura Oferta desde
         #4 Key KEY.OF$; KEYIND$, IND1OF%,                                 \! EAMITEMR
         INDIS$,VALOF$,LIG99$,NOMOFE$,CUSER$, GR.DATA$                      !
      If Terror% = -1 Then Begin																						! Oferta Localizada
	       VTA.OF% = Val(UnPack$(VALOF$))                  										! Toma Valor Oferta
       For LIG1% = 1 To 99                             											! Explosiona la Oferta
           LIGA$=RIGHT$("00"+Str$(LIG1%),2)            											! Arma llave para
           FIS.KIT$ = PACK$(PREFOF$ + LIGA$)           											! lectura de Regs MP5
           Terror% = -1															 												! Control de errores
           READ FORM "C16 C6 C2 C18 C4 C31";          										 \! Lectura de Reg Liga
                #4 KEY FIS.KIT$;                      										 \! EAMITEMR
                RELE1$,VALOF$,LIGO$,DESOFE$,RELE2$,                        \!
                GR.DAT2$																										!
           IF Terror% = -1 THEN BEGIN                   										! Lectura Ok Liga/Desc
              IF LIG1% <> 99 THEN BEGIN                											! Si no es Descto
                 PLUOF$ = UNPACK$(VALOF$)              											! Toma Cod Alias
                 KEY.OF$ = VALOF$                      											! y arma Llave Lec
                 Terror% = -1
                 READ FORM "C8 C9 C5 C2 C18 C4 C31";  									   \! Lee los Reg ALIAS
                      #4 KEY KEY.OF$;                 										 \! verdaderos de Oferta
                      RELE1$,RELE3$,                                       \!
                      VALOF$,LIGA$,DESOFE$,RELE2$,                         \!
                      GR.DAT2$																							!
                 If Terror% = -1 Then \                                     
                    VTA.TO% = VTA.TO% + Val(UNPACK$(VALOF$))  		  				! Acum Prec/Vta Real
              EndIf Else BEGIN                                              ! Item de Dscto
                  DES.99% = Val(UNPACK$(VALOF$))       											! Toma Vr Liga/Descto
              EndIf
           EndIf Else Begin                                                 ! Si NO hay mas Ligas
              If LIG1% <> 99 Then LIG1% = 98                                ! y NO es la Ult Liga
           EndIf                                                            ! Bypass hasta 98
       Next LIG1%                                                           ! siguiente Liga

       If (VTA.TO% - DES.99%) <> VTA.OF% Then Begin													! Si diferencia 
       	  PARAM2$ = " VLR TOT "+STR$(VTA.TO%) + \
       	            " VLR OFE "+STR$(VTA.OF%)
       	  Call MENSAJE
       	  Des.New% = (VTA.TO% - Vta.Of%)																		! Nuevo Descuento
       	  If Des.New% >= 0 Then Begin																				! Nuevo dscto calculado
             PARAM2$ = " Act. Oferta: " + PREFOF$   		+				    			 \! Mensaje BG de Lec
                       " Dscto Old  : " + Str$(Des.99%) +                  \! Dscto anterior
                       " Dscto New  : " + Str$(Des.New%)                    ! Nuevo Dscto
             Call MENSAJE
             FIS.KIT$ = PACK$(PREFOF$ + "99")           										! lectura de Regs MP5
             Terror% = -1															 											! Control de errores
             READ FORM "C6 C2 I1 C7 C6 C2 C18 C4 C31"; #4 KEY FIS.KIT$ ;   \! Busca Item Dscto
               KEY.MER$,IND.012$, IND1OF%,INDIS$,VALOF$,LIG99$,            \!
               NOMOFE$,CUSER$, GR.DATA$                                     ! 
             VALOF$ = Pack$(Right$("000000000000"+Str$(Des.New%),12))       ! Ajusta dato
             WRITE FORM "C6 C2 I1 C7 C6 C2 C18 C4 C31";#4;                 \! Actualiza vlr dscto
                FIS.KIT$, IND.012$,IND1OF%,INDIS$,VALOF$,LIG99$,           \!
                NOMOFE$,CUSER$, GR.DATA$                                    ! 
       	  	 Tot.Aju% = Tot.Aju% + 1																				! Total Ofertas Ajustadas
       	  EndIf
       	  If Des.New% < 0 Then Begin																				! Diferencia negativa
             KEY.OF$ = Right$("0000" + STR$(OFE%),4)      									! Arma llave Ofertas
             KEY.OF$ = Pack$("99999999" + KEY.OF$)        									! con prefijo
             DELREC 4 ; KEY.OF$                                             ! Retira Oferta
             Lig1% = 0																											! Init Contenido
             For LIG1% = 1 To 99                             								! Explosiona la Oferta
               LIGA$=RIGHT$("00"+Str$(LIG1%),2)            								  ! Arma llave para
               FIS.KIT$ = PACK$(PREFOF$ + LIGA$)           									! lectura de Regs MP5
               DELREC 4 ; KEY.OF$                                           ! Retira Items contenido oferta
             Next Lig1%  
             PARAM2$ = " Elimina : " + Right$(PREFOF$,4) +					       \! Mensaje BG de Lec
                       " Vlr Ofe : " + Str$(Vta.Of%)     +                 \! Vlr de la oferta
                       " Vlr New : " + Str$(Vta.To%)                        ! Venta Mercadeo
             Call MENSAJE
             Tot.del% = Tot.del% + 1 																				! Total Ofertas Eliminadas
       	  EndIf
       	  
       EndIf
      EndIf
  Next Ofe%
End Sub 




On Error GoTo Fail.Process
DATO.SO$ = COMMAND$                                             						! Dato S.O
If DATO.SO$ = "BACKGRND" Then DATO.SO$ = "1" Else DATO.SO$ = "0"						! Tipo de ejecucion
FileOut$ = "ADX_UDT1:AO"+DATE$+".TXT"																				! Auditoria Ajuste Ofertas
Open "EAMITEMR" KEYED RECL 77 AS 4 BUFF 10																	! Apertura Eamitemr
Open FileOut$ As 51 Append																									! Adiciona Auditoria proceso
Call VERIFICA.OFERTA																												! Verificacion Ofertas
PARAM2$ = "RESUMEN Ajustadas : " + Str$(tot.aju%)     +		                 \! Total de Ofertas modificadas
          " Eliminadas: " + Str$(tot.del%)                                  ! y eliminadas

Call MENSAJE
Param2$ = "Termina Proceso "+Date$+" Hora: "+Time$
Call Mensaje          
Param2$ = String$(40,"-")
Call Mensaje          
Close 51																																		! Cierre archivo auditoria
Close 4																																			! Cierre archivo Items
Stop 																																				! Para ejecucion programa

Fail.Process:
 
 If ERR = "EF" Then Terror% = 0 : Resume
 If ERRF% = 51 AND (ERR = "OE" OR ERR = "FU") Then Begin										! Manejo Auditoria
    Create FileOut$ As 51
    Resume
EndIf 

 Param2$ = "ERROR APPL "+ERR
 Call Mensaje
 Stop
  
