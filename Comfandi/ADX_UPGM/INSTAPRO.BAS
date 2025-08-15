!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : INSTAPRO.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Creacion Estructuras Promociones *
!**************************************************
! Modificaciones:

Clears
Print "Creando Estructuras Modulo Promociones - Ver. 9.0 12/Ago/2011"
Print " "
!-- Creacion Administracion de Promociones
     Print "Creando Administrador de promociones (PRMADMON)"
     CREATE POSFILE "MKTP:PRMADMON.DAT" KEYED 2,,,150000 RECL 9   \! si no existe lo crea.
                           AS 4 compound perupdate 		            ! 
     Close 4 

!-- Creacion Promociones x Presencia Mensaje
     Print "Creando definicion de promociones (MKPDEFPR)"
     CREATE POSFILE "MKTP:MKPDEFPR.DAT" KEYED 9,,,150000 RECL 66 \! si no existe lo crea.
                           AS 4 compound perupdate 		            ! 
     Close 4 

!-- Creacion Mensajes
     Print "Creando Mensajes Promocionales (MKPMSGPR)"
     CREATE POSFILE "MKTP:MKPMSGPR.DAT" KEYED 2,,,1500 RECL 279   \! si no existe lo crea.
                           AS 4 compound perupdate 		            ! 
     Close 4 

!-- Creacion Promociones x No Presencia Articulos
     Print "Creando Promociones por no presencia (MKPPNPRD)"
     CREATE POSFILE "MKTP:MKPPNPRD.DAT" KEYED 12,,,150000 RECL 16   \! si no existe lo crea.
                           AS 4 compound perupdate 		            ! 
     Close 4 
     
!-- Creacion Promociones x Presencia ITEMS Beneficio
     Print "Creando promociones por presencia de ITEMS (MKPPRITM)"
     CREATE POSFILE "MKTP:MKPPRITM.DAT" KEYED 8,,,500000 RECL 31   \! si no existe lo crea.
                           AS 4 compound perupdate 		            ! 
     Close 4 


!-- Creacion Secciones
     Print "Creando Descuento por Seccion (SECCION)"
     CREATE POSFILE "MKTP:SECCION.DAT" KEYED 12,,,150000 RECL 84   \! si no existe lo crea.
                           AS 4 compound perupdate 		            ! 
     Close 4 

!-- Creacion Rifas
     Print "Creando control articulos en rifas (TF:A0004)"
     CREATE POSFILE "TF:A0004" KEYED 8,,,150000 RECL 8   \! si no existe lo crea.
                           AS 4 compound perupdate 		            ! 
     Close 4 

!-- Creacion Rifas
     Print "Creando control rifas (TF:P0004)"
     CREATE POSFILE "TF:P0004" KEYED 4,,,150000 RECL 40   \! si no existe lo crea.
                           AS 4 compound perupdate 		            ! 
     Close 4 


!-- Creacion Rifas
     Print "Creando control articulos en rifas (TF:LR004)"
     CREATE POSFILE "TF:LR004" KEYED 1,,,100 RECL 301   \! si no existe lo crea.
                           AS 4 compound perupdate 		            ! 
     Close 4 

!-- Creacion Promocion a formas de pago
     Print "Creando control promocion formas de pago (TF:PFPAGO)"
     CREATE POSFILE "TF:PFPAGO" KEYED 1,,,500 RECL 16   \! si no existe lo crea.
                           AS 4 compound perupdate 		            ! 
     Close 4 

     Print "Creando control articulos ligados a formas de pago (TF:AFPAGO)"
     CREATE POSFILE "TF:AFPAGO" KEYED 7,,,45000 RECL 7   \! si no existe lo crea.
                           AS 4 compound perupdate 		            ! 
     Close 4 

Print " "
Print "Procedimiento Estructura Promociones Finalizado"
     
Stop      
     