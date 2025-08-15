!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : INSTAPRO.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Creacion Estructuras Promociones *
!**************************************************
! Modificaciones:

On Error GoTo Paila

Clears
Print "ANULACION PROMOCION FORMA PAGO - Ver. 1.0 07/Oct/2016"
Print " "

!-- Creacion Promocion a formas de pago
     Print "Ajustando promocion formas de pago (TF:PFPAGO)"
     CREATE POSFILE "TF:PFPAGO" KEYED 1,,,500 RECL 16   \! si no existe lo crea.
                           AS 4 compound perupdate 		            ! 
     Close 4 

     Print "Ajustando control articulos ligados a formas de pago (TF:AFPAGO)"
     CREATE POSFILE "TF:AFPAGO" KEYED 7,,,45000 RECL 7   \! si no existe lo crea.
                           AS 4 compound perupdate 		            ! 
     Close 4 

Print " "
Print "Procedimiento Finalizado"
     
Stop      

Paila:

Print "Error de Aplicacion "+ERR
Stop 
