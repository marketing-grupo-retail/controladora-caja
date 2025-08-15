!************************************************** 
!Empresa       : Grupo Retail S.A                 *
!Programa      : INMOVTLP.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Interface Recaudos TelePalmira   *
!**************************************************

%ENVIRON C						   												! Ambiente de controlador

%INCLUDE CFMEDPAG.VAR

 Function GETN4(P1$,P2) EXTERNAL
     INTEGER*4 GETN4
     STRING P1$
     INTEGER*2 P2
 End Function

Sub LIQUIVA.MED External
End Sub 

Function Graba.Planilla(Xpos%,F1$, F2$, F3$, I1%, I2%, I3%, I4%, F4$) Public 
String F1$, F2$, F3$, F4$
Integer*4 I1%, I2%, I3%, I4%, Xpos%
  Write Form "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
             Xpos%;F1$, F2$, F3$, I1%, I2%, I3%, I4%, F4$
End Function 

Sub String.FISCAL Public 															 !
Integer*4 Xpos%
String Pre99$
  PRE99$ = "TR+"                                       !
  Xpos% = MATCH(PRE99$,INAREA$,1)                      !
  If Xpos% > 0 Then BEGIN                              ! Ident TransVta
     VTA%=0 : IV1%=0 : IV2%=0 : IV3%=0 : IV4%=0        !
              IV5%=0 : IV6%=0 : IV7%=0 : IV8%=0        !
     PRETR$ = MID$(INAREA$,Xpos%+3,4)                  !
     NUFIS$ = Str$(Val(UNPACK$(MID$(INAREA$,Xpos%+7,8)))) ! Retira ceros de la izquierda
     NUFIS$ = RIGHT$("           "+NUFIS$,11)          !
  EndIf                                                !
End Sub

SUB ACU.IVAICO                                   ! 
  EOF$ = "  "                                    ! Inicia Flag de Error
  LLAVE$ = RIGHT$("000000000000" + COPLU$,12)    ! Arma llave
  LLAVE$ = PACK$(LLAVE$)                         ! y la empaqueta
  TFI$   = RIGHT$("00"+STR$(IVAD%(DEPI%,0)),2)   ! Convierte a Str long 2 
  TFI%   = MATCH(TARIF$,TFI$,1)                  ! Busca Posic Tarifa IVA
  TFI%   = TFI% / 3 + 1                          ! Calcula Ocorrencia
  ICON%  = 0                                     ! ImpoConsumo Default
  READ FORM "C9 C1 C2 C4 C1 C5 C20 2I2 C31";#22 \! Lee EAMITEMR
       KEY LLAVE$;REGI$,IND2$,SEC$,FAGR$,       \! para localizar
           CONT$,ALI$,LINO$,ICON%,FECR%,GR.DATA$ ! Impuesto al Consumo
  ICON% = GETN4(GR.DATA$,16)                     ! Valor del impoconsumo
  IF EOF$ = "  " THEN BEGIN                      ! Si hay error Lectura
     IF RIGHT$(UNPACK$(IND2$),1)="5" THEN BEGIN  ! Si em Metodo/Precio 5
        SECCI$ = SEC$                            ! toma depto y
        LLAVE$ = PACK$("00")+ALI$                ! extrae Llave ALIAS
        READ FORM "C9 C1 C2 C4 C1 C5 C20 2I2 C31";  \! Lee nuevemente 
             #22 KEY LLAVE$;REGI$,IND2$,        \! EAMITEMR
                 SEC$,FAGR$,CONT$,ALI$,         \!
                 LINO$,ICON%,FECR%,GR.DATA$      ! para comparar Deptos
     ENDIF                                       !
  ENDIF                                          !
  ICON% = GETN4(GR.DATA$,16)                     ! Valor del impoconsumo
  IF CAN% = 0 THEN CAN% = 1                      !
  ICO%  = ICON% * CAN% * SGN%                    ! Calcula Imp Consumo
  IV.TF%(DEPI%,TFI%,0) = IV.TF%(DEPI%,TFI%,0)+  \!
                         BB.PRECIO               ! Acumula Vta/PLU
  IV.TF%(DEPI%,TFI%,1) = IV.TF%(DEPI%,TFI%,1)+  \!
                         ICO%                    ! Acumula ImpCon/PLU
  IV.TF%(DEPI%,TFI%,2) = IV.TF%(DEPI%,TFI%,2)+  \!
                         CAN%                    ! Acumula Cantidad
End SUB                                          !

SUB GRABA.OTROIVA External                                             !
    CALL LIQUIVA.MED                                          !
    SG$ = "+"                                                 !
    REGMED9$ = LEFT$(REGMED$,74)                              !
    EXEIVA$ = RIGHT$("000000000" + STR$(ABS(IVATRA%(0))),9)   !
    PLAIVA$ = RIGHT$("000000000" + STR$(ABS(IVATRA%(1))),9)   !
    PLBIVA$ = RIGHT$("000000000" + STR$(ABS(IVATRA%(2))),9)   !
    PLCIVA$ = RIGHT$("000000000" + STR$(ABS(IVATRA%(3))),9)   !
    PLDIVA$ = RIGHT$("000000000" + STR$(ABS(IVATRA%(4))),9)   !
    GRTIVA$ = RIGHT$("000000000" + STR$(ABS(IVATRA%(5))),9)   ! 
    ICOIVA$ = RIGHT$("000000000" + STR$(ABS(IVATRA%(6))),9)   ! 
    FOR FI% = 0 TO 6                                          !
        TOTTRAN% = IVATRA%(FI%) + TOTTRAN%                    !
        IF TOTTRAN% < 0 THEN SG$ = "-"                        !
    NEXT FI%                                                  !
    TOTTRAN$ = RIGHT$("000000000" + STR$(ABS(TOTTRAN%)),9)    ! 
    IF TOTTRAN%=0 AND MID$(REGMED9$,21,1)="-" Then SG$ = "-"  !
    DESCUE$ = STR$(SA.AMTDISCO*-1)                               !
    DESCUE$ =RIGHT$(STRING$(6,"0")+DESCUE$,6)                 !
    Call String.Fiscal
    !FISCO$ = "TR"+"+"+LEFT$(PRETR$,2)+ "00"+"-"+RIGHT$(NUFIS$,7)
    
    NUMFIS$ = RIGHT$("           "+NUFIS$,11)
    FISCO$ = LEFT$(PRETR$,4) + NUMFIS$

    !FISCO$ = RIGHT$(STRING$(15,"0")+FISCO$,15)                ! Oct. 21 de 2002
    !FISCO$ = PRENUF$                                          !
    
    If Gr.Ind.Rem% = -1 Then Begin 
    	 FISCO$ = String$(15,"X")
    EndIf 
    REGMED9$=REGMED9$+EXEIVA$+SG$+"16"+PLAIVA$     +         \!
             SG$+"20"+PLBIVA$+SG$+"35"+PLCIVA$     +         \!
             SG$+"10"+PLDIVA$+SG$+GRTIVA$+SG$      +         \!
             ICOIVA$+SG$+TOTTRAN$+SG$                         !
    WRITE FORM "C162 C15 C6 C2";#FIL9,POSI%;                 \! 
               REGMED9$,FISCO$,DESCUE$,FR$                    !
    POSI% = POSI% + 1                                         !
    DESCUE$ = ""
    SA.AMTDISCO = 0
    DIM IVATRA%(6)                                            !
    TOTTRAN% = 0                                              !
    CREDI% = 0 
END SUB                                                       !
