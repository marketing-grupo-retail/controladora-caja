!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : INSTAPRO.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Creacion Estructuras Promociones *
!**************************************************
! Modificaciones:

Integer*4 Global XRET%

%Include BASROUT.J86	      ! Rutinas generales

Sub Adxserve(RET,FUNC,PARM1,PARM2)  External
Integer*4 RET
Integer*2 FUNC,PARM1
String PARM2
End Sub

Function ADXSTART(NOMBRE,PARAMETROS,MENSAJE) External 
 Integer*2 ADXSTART
 String NOMBRE,PARAMETROS,MENSAJE
End Function


On Error GoTo Fail.Proc
Call ADXSERVE (XRET%, 26, 0,"IPL Controller In 5 Min."+Left$(time$,2)+":"+mid$(time$,3,2))							! Mensaje al Background
wait ; 8000
!XRET% = ADXSTART("ADX_SPGM:ADXCS20L.286","Y 14","")    ! Cambio fecha contable 

Call OSSHELL("CC::ADXCS20L Y 14 ")

Call ADXSERVE (XRET%, 26, 0,"Listo el IPL Controller")			! Mensaje al Background
Stop      
     
Fail.Proc:
Call ADXSERVE (XRET%, 26, 0,"Failed IPL "+ERR)							! Mensaje al Background
Stop
