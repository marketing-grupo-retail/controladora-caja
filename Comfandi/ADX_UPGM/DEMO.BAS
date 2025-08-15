!--- Calculo de ofertas

Integer*1 Global Terror%
String    Global Param2$

Sub MENSAJE
    Print Param2$
End Sub 

Sub VERIFICA.OFERTA																													! Validacion de Ofertas
Integer*4 Ofe%, Vta.Of%, Lig1%, DES.99%, VTA.TO%, LIG2%, Des.New%
Integer*1 IND1OF%																														!
String    INDIS$,VALOF$,LIG99$,NOMOFE$,CUSER$, GR.DATA$, KEY.OF$, PREFOF$, \!
          KEYIND$, FIS.KIT$,RELE1$,LIGO$,DESOFE$,RELE2$, GR.DAT2$,         \!
          PLUOF$, LIGA$, RELE3$, IND.012$, KEY.MER$
  For OFE% = 1 TO 999                                    										! Para todas las Ofertas
!      PARAM2$ = " Leyendo Oferta: " + STR$(OFE%)         										! Mensaje BG de Lec
!      Call MENSAJE
      PREFOF$="999999"+Right$("0000"+STR$(OFE%),4)       										! Prefijo ofertas
      KEY.OF$ = Right$("0000" + STR$(OFE%),4)            										! Arma llave Ofertas
      KEY.OF$ = Pack$("99999999" + KEY.OF$)              										! con prefijo
      Terror% = -1																													! Control de errores
      Read Form "C8 I1 C7 C6 C2 C18 C4 C31";                               \! Lectura Oferta desde
         #4 Key KEY.OF$; KEYIND$, IND1OF%,                                 \! EAMITEMR
         INDIS$,VALOF$,LIG99$,NOMOFE$,CUSER$, GR.DATA$                      !
      If Terror% = -1 Then Begin																						! Oferta Localizada
      	 Print "Procesando Oferta "+Str$(Ofe%)															!
	       VTA.OF% = Val(UnPack$(VALOF$))                  										! Toma Valor Oferta
         Print "Valor Oferta "+Str$(VTA.OF%)

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
                 READ FORM "C8 C9 C5 C2 C18 C4 C31";  									   \! Lee los Reg ALIAS
                      #4 KEY KEY.OF$;                 										 \! verdaderos de Oferta
                      RELE1$,RELE3$,                                       \!
                      VALOF$,LIGA$,DESOFE$,RELE2$,                         \!
                      GR.DAT2$																							!
                 VTA.TO% = VTA.TO% + Val(UNPACK$(VALOF$))  		  						! Acum Prec/Vta Real
                 PARAM2$=" Lee Of.: "            +                         \! Despliega Inf de
                         STR$(OFE%)+"   Link: "  +                         \! cada Plu ALIAS
                         STR$(LIG1%)+"  PLU: "   +                         \! y su Valor
                         STR$(VAL(PLUOF$))+" Vr: " +                       \!
                         STR$(VAL(UNPACK$(VALOF$)))                         !
                  Call MENSAJE
              EndIf Else BEGIN                                              ! Item de Dscto
                 PARAM2$=" Dsc Of.: "            +                         \! Despliega Inf de
                         STR$(OFE%)+"   Link: "  +                         \! cada Plu ALIAS
                         STR$(LIG1%)+"  PLU: "   +                         \! y su Valor
                         STR$(VAL(PLUOF$))+" Vr: " +                       \!
                         STR$(VAL(UNPACK$(VALOF$)))                         !
                  Call MENSAJE              	
                  DES.99% = Val(UNPACK$(VALOF$))       											! Toma Vr Liga/Descto
              EndIf
           EndIf Else Begin                                                 ! Si NO hay mas Ligas
              If LIG1% <> 99 Then LIG1% = 98                                ! y NO es la Ult Liga
           EndIf                                                            ! Bypass hasta 98
       Next LIG1%                                                           ! siguiente Liga

       PARAM2$ = "TOTAL CALCULADO "+STR$(VTA.TO%)
       Call MENSAJE
      	 
       If (VTA.TO% - DES.99%) <> VTA.OF% Then Begin													! Si diferencia 
       	  Param2$ = "Oferta con Cambios"
       	  Call Mensaje
       	  Param2$ = "Precio Definido "+str$(Vta.Of%)
       	  Call Mensaje
       	  Param2$ = "Descuento Inicial "+str$(Des.99%)
       	  Call Mensaje
       	  Param2$ = "Diferencia para Ajuste "+str$(VTA.TO% - Vta.Of%)
       	  Call Mensaje
       	  
       	  Des.New% = (VTA.TO% - Vta.Of%)																		! Nuevo Descuento
       	  If Des.New% >= 0 Then Begin																				! Nuevo dscto calculado
             FIS.KIT$ = PACK$(PREFOF$ + "99")           										! lectura de Regs MP5
             Param2$ = "Actualiza Item "+Unpack$(Fis.Kit$)
             Call mensaje
             Terror% = -1															 											! Control de errores
             READ FORM "C6 C2 I1 C7 C6 C2 C18 C4 C31"; #4 KEY FIS.KIT$ ;   \! Busca Item Dscto
               KEY.MER$,IND.012$, IND1OF%,INDIS$,VALOF$,LIG99$,            \!
               NOMOFE$,CUSER$, GR.DATA$                                     ! 
             VALOF$ = Pack$(Right$("000000000000"+Str$(Des.New%),12))       ! Ajusta dato
             WRITE FORM "C6 C2 I1 C7 C6 C2 C18 C4 C31";#4;                 \! Actualiza vlr dscto
                FIS.KIT$, IND.012$,IND1OF%,INDIS$,VALOF$,LIG99$,           \!
                NOMOFE$,CUSER$, GR.DATA$                                    ! 
       	  	 
       	  EndIf
       	  If Des.New% < 0 Then Begin																				! Diferencia negativa
       	  	 !--- Elimina la oferta
       	  EndIf
       	  
       EndIf Else Begin																											! Oferta Sin cambios
       	  Param2$ = "Oferta sin Cambios"
       	  Call Mensaje
       EndIf
      	 
      	 
      EndIf
  Next Ofe%
End Sub 

On Error GoTo PAILA
Open "EAMITEMR" KEYED RECL 77 AS 4 BUFF 10
Call VERIFICA.OFERTA
Close 4
Stop 

PAILA:
 
 If ERR = "EF" Then Terror% = 0 : Resume
 
 PRINT "ERROR APPL "+ERR
 Stop
  
