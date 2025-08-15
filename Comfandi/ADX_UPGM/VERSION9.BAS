!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : INSTAPRO.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Creacion Estructuras Promociones *
!**************************************************
! Modificaciones:

Clears
Print "Creando Estructuras Modulo Promociones - Ver. 9.0 20/May/2011"
Print " "

On Error GoTo Falla.Aplicacion
!-- Creacion Promociones x Presencia Mensaje
     Print "Creando definicion de promociones (MKPDEFPR)"
     CREATE POSFILE "MKTP:MKPDEFPR.DAT" KEYED 9,,,150000 RECL 31   \! si no existe lo crea.
                           AS 4 compound perupdate 		            ! 
     Close 4 

!-- Creacion Promociones x No Presencia Articulos
     Print "Creando Promociones por no presencia (MKPPNPRD)"
     CREATE POSFILE "MKTP:MKPPNPRD.DAT" KEYED 12,,,150000 RECL 16   \! si no existe lo crea.
                           AS 4 compound perupdate 		            ! 
     Close 4 

Print " "
Print "Fin procedimiento de instalacion"
Stop

Falla.Aplicacion:
Print "ERROR DE EJECUCION "+ERR
Stop
