       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTA-PLA.
       AUTHOR. CHICOTE-MARIO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PHILIPS.
       OBJECT-COMPUTER. PHILIPS.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL PLATOS ASSIGN TO DISK "PLATOS.DAT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS NUMPLATO
           ALTERNATE RECORD KEY IS CODIGO WITH DUPLICATES.
           SELECT OPTIONAL MESAS ASSIGN TO DISK NOMES
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.
           SELECT OPTIONAL FACTURA ASSIGN TO DISK "FACTURAS.DAT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS NFAC
           ALTERNATE RECORD KEY IS FFECHA WITH DUPLICATES
           ALTERNATE RECORD KEY IS FGIF WITH DUPLICATES
           ALTERNATE RECORD KEY IS FPAGADA WITH DUPLICATES.
           SELECT LISTADO ASSIGN TO PRINT "PRINTER"
           FILE STATUS IS ERRORIMP.
       DATA DIVISION.
       FILE SECTION.
       FD PLATOS LABEL RECORD STANDARD.
       01 REG-PLA.
           02 NUMPLATO PIC 99.
           02 CODIGO PIC XX.
           02 NOM PIC X(26).
           02 PVP PIC 9(4).
       FD MESAS LABEL RECORD STANDARD.
       01 REG-ME.
           02 M-NUMPLATO PIC 99.
           02 M-CODIGO PIC XX.
           02 M-NOM PIC X(26).
           02 M-PVP PIC 9(4).
           02 M-CANTIDAD PIC 99.
           02 M-TOTAL PIC 9(6).
       FD FACTURA LABEL RECORD STANDARD.
       01 REG-FAC.
           02 NFAC   PIC 9(8).
           02 FGIF   PIC 9(9).
           02 FFECHA.
              03 FDIA PIC 9(2).
              03 FMES PIC 9(2).
              03 FANO PIC 9(4).
           02 FPASTA  PIC 9(8).
           02 FPAGADA PIC X.
           02 FMESA   PIC 99.
       FD LISTADO LABEL RECORD OMITTED.
       01 FACTLIN PIC X(132).
       WORKING-STORAGE SECTION.
       77 ERRORIMP PIC XX.
       77 RESERVA PIC 99.
       77 NOMES PIC X(10).
       77 VENT PIC 9(16).
       77 VENTANA PIC 9(14).
       77 TECLAF PIC 99 COMP.
       77 TECLA PIC X.
       77 PUNTERO PIC 99.
       77 ESTADO PIC 9.
       77 POSICION PIC 99.
       77 FIN PIC X.
       77 NADA PIC X.
       77 MODIFIC PIC X.
       77 NUMID PIC 99.
       77 PIVOTE PIC 99.
       77 SUMTOTAL PIC 9(7).
       77 IVA PIC 9(6).
       77 TOTIVA PIC 9(7).
       77 NUMFACT PIC 9(8).
       01 FechaSys.
           02 AnoSys PIC 99.
           02 MesSys PIC 99.
           02 DiaSys PIC 99.
       01 LIN.
           02 SITIO PIC 99.
           02 REPETICION PIC 99.
       01 CONTADORES.
           02 I PIC 99.
           02 A PIC 99.
           02 J PIC 99.
           02 CONTREG PIC 9(2).
           02 LINEA PIC 99.
           02 INICIO PIC 99.
           02 TEND PIC 99.
       01 PANTALLA.
            02 LP PIC X(80) OCCURS 25.

      **********************  LINEAS DE MENSAJES  *****************


       01 DATOSMEN.
           02 FILLER PIC X(78) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "          ¨Desea continuar? S/N ".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE " Esc=Salir   F1=Consulta   F2=Ca".
           02 FILLER PIC X(32) VALUE "mbio de mesa   F3=Factura   F4=B".
           02 FILLER PIC X(14) VALUE "orrado     ".
           02 FILLER PIC X(32) VALUE "Si desea continuar introduciento".
           02 FILLER PIC X(32) VALUE " platos presion  AV-PAG, sino pr".
           02 FILLER PIC X(14) VALUE "esione RE-PAG ".
           02 FILLER PIC X(32) VALUE "     Este plato no existe      ".
           02 FILLER PIC X(32) VALUE "                               ".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "Este plato ya lo ha introducido".
           02 FILLER PIC X(32) VALUE " por favor, introduzca otro.   ".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "Situese en el plato deseado den".
           02 FILLER PIC X(32) VALUE "tro de la lista y presione ESC ".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "Elija el numero de mesa        ".
           02 FILLER PIC X(32) VALUE ALL " ".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "La factura se esta generando, es".
           02 FILLER PIC X(32) VALUE "pere un momento                 ".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "El plato donde esta situado el ".
           02 FILLER PIC X(32) VALUE "cursor se ha dado de baja      ".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "¨Desea abandonar la ejecuci¢n ".
           02 FILLER PIC X(32) VALUE "del programa? SI ¢ NO         ".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "Error de disco o la factura ya".
           02 FILLER PIC X(32) VALUE " existe".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "Grabaci¢n satisfactoria".
           02 FILLER PIC X(32) VALUE ALL " ".
           02 FILLER PIC X(14) VALUE ALL " ".
       01 MENSAJES REDEFINES DATOSMEN.
           02 LM PIC X(78) OCCURS 13.


      ************************   LINEAS DE LA FACTURA   ***************


       01 LIN1.
           02 FILLER PIC 9(4) COMP-1 VALUE 15.
           02 PIC X(39) VALUE "ÕÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÑÍÍÍÍÍÑ".
           02 PIC X(9) VALUE "ÍÍÍÍÍÍÍÍ¸".
       01 LIN2.
           02 FILLER PIC 9(4) COMP-1 VALUE 15.
           02 PIC X(39) VALUE "³PLATO                     ³UNID³ PVP ³".
           02 PIC X(9) VALUE " TOTAL  ³".
       01 LIN3.
           02 FILLER PIC 9(4) COMP-1 VALUE 15.
           02 PIC X(39) VALUE "ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍØÍÍÍÍØÍÍÍÍÍØ".
           02 PIC X(9) VALUE "ÍÍÍÍÍÍÍÍµ".
       01 LINDET.
           02 FILLER PIC 9(4) COMP-1 VALUE 15.
           02 PIC X VALUE "³".
           02 L-NOM PIC X(26).
           02 PIC X(2) VALUE "³ ".
           02 L-CANTIDAD PIC Z9.
           02 PIC X(2) VALUE " ³".
           02 L-PVP PIC Z.ZZ9.
           02 PIC X(2) VALUE "³ ".
           02 L-TOTAL PIC ZZZ.ZZ9.
           02 PIC X(2) VALUE "³".
       01 LIN4.
           02 FILLER PIC 9(4) COMP-1 VALUE 15.
           02 PIC X(39) VALUE "ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÏÍÍÍÍÍÏ".
           02 PIC X(9) VALUE "ÍÍÍÍÍÍÍÍ¾".
       01 LIN5.
           02 FILLER PIC 9(4) COMP-1 VALUE 15.
           02 PIC X(38) VALUE SPACES.
           02 L-SUMTOTAL PIC Z.ZZZ.ZZ9.
       01 LIN6.
           02 FILLER PIC 9(4) COMP-1 VALUE 15.
           02 PIC X(39) VALUE "                             + 6% IVA: ".
           02 PIC X VALUE " ".
           02 L-IVA PIC ZZZ.ZZ9.
       01 LIN7.
           02 FILLER PIC 9(4) COMP-1 VALUE 15.
           02 PIC X(39) VALUE SPACES.
           02 PIC X(9) VALUE "ÄÄÄÄÄÄÄÄÄ".
       01 LIN8.
           02 FILLER PIC 9(4) COMP-1 VALUE 15.
           02 PIC X(38) VALUE "                                TOTAL:".
           02 L-TOTIVA PIC Z.ZZZ.ZZ9.
       01 LIN9.
           02 FILLER PIC 9(4) COMP-1 VALUE 15.
           02 PIC X(36) VALUE "               GRACIAS POR SU VISITA".

      ***********************  FICHEROS MESA  *********************

       01 FICHERO.
           02 FILLER PIC X(10) VALUE "01MESA.DAT".
           02 FILLER PIC X(10) VALUE "02MESA.DAT".
           02 FILLER PIC X(10) VALUE "03MESA.DAT".
           02 FILLER PIC X(10) VALUE "04MESA.DAT".
           02 FILLER PIC X(10) VALUE "05MESA.DAT".
           02 FILLER PIC X(10) VALUE "06MESA.DAT".
           02 FILLER PIC X(10) VALUE "07MESA.DAT".
           02 FILLER PIC X(10) VALUE "08MESA.DAT".
           02 FILLER PIC X(10) VALUE "09MESA.DAT".
           02 FILLER PIC X(10) VALUE "10MESA.DAT".
           02 FILLER PIC X(10) VALUE "11MESA.DAT".
           02 FILLER PIC X(10) VALUE "12MESA.DAT".
           02 FILLER PIC X(10) VALUE "13MESA.DAT".
           02 FILLER PIC X(10) VALUE "14MESA.DAT".
           02 FILLER PIC X(10) VALUE "15MESA.DAT".
           02 FILLER PIC X(10) VALUE "16MESA.DAT".
       01 TABLAS REDEFINES FICHERO.
           02 LF PIC X(10) OCCURS 16.

      ************************ PRESENTACION DE PANTALLA ***************

       01 TITULO.
           02 FILLER PIC X(27) VALUE "ÕÍCDÍÍÑÍNUM. PLATOÍÑÍÍÍÍÍÍÍ".
           02 FILLER PIC X(27) VALUE "NOMBRE DEL PLATOÍÍÍÍÍÍÑÍCAN".
           02 FILLER PIC X(26) VALUE "TIDADÍÑÍÍPVPÍÍÍÑÍÍTOTALÍÍ¸".
       01 DATOSPAN.
            02 LD OCCURS 18.
               03 FILLER PIC X VALUE " ".
               03 T-CODIGO PIC XX.
               03 FILLER PIC X(8) VALUE "  ³     ".
               03 T-NUMPLATO PIC 99.
               03 FILLER PIC X(7) VALUE "     ³ ".
               03 T-NOM PIC X(26).
               03 FILLER PIC X(7) VALUE "  ³    ".
               03 T-CANTIDAD PIC 99.
               03 FILLER PIC X(6) VALUE "    ³ ".
               03 T-PVP PIC 9(4).
               03 FILLER PIC X(6) VALUE "   ³  ".
               03 T-TOT PIC 9(6).
               03 FILLER PIC X VALUE " ".
       01 PCVENTANA.
           02 FILLER PIC 99 VALUE 2.
           02 FILLER PIC 99 VALUE 13.
           02 FILLER PIC 99 VALUE 20.
           02 FILLER PIC 99 VALUE 55.
           02 FILLER PIC 99 VALUE 62.
           02 FILLER PIC 99 VALUE 73.
       01 TABLA REDEFINES PCVENTANA.
           02 CONTP PIC 99 OCCURS 6.
       01 DATOSVENTANA.
           02 FILLER PIC 99 VALUE 6.
           02 FILLER PIC 99 VALUE 19.
           02 FILLER PIC 99 VALUE 49.
           02 FILLER PIC 99 VALUE 60.
           02 FILLER PIC 99 VALUE 69.
       01 POSILINEAS REDEFINES DATOSVENTANA.
           02 PSLINEAS PIC 99 OCCURS 5.
       PROCEDURE DIVISION.
       DECLARATIVES.
       UNO SECTION.
           USE AFTER ERROR PROCEDURE ON LISTADO.
       ERROR-IMPRESORA.
           IF ERRORIMP NOT = "00"
              MOVE 2 TO I
              CALL "MENSAJE.COB" USING I
           END-IF.
       END DECLARATIVES.
       DOS SECTION.
       MAIN.
           PERFORM PRESENTA
           PERFORM ELECCION
           MOVE 0 TO TECLAF
           OPEN I-O PLATOS
           MOVE "S" TO MODIFIC
           PERFORM ALTAS UNTIL TECLAF = 27
           CLOSE PLATOS
           DISPLAY " " ERASE
           EXIT PROGRAM.

      ** Presentaci¢n en pantalla de la ventana **

       PRESENTA.
           MOVE 0300218004000011 TO VENT
           CALL "VENTBUF.COB" USING VENT PANTALLA
           MOVE PSLINEAS(1) TO SITIO
           CALL "LINVBUF.COB" USING VENT LIN PANTALLA
           MOVE PSLINEAS(2) TO SITIO
           CALL "LINVBUF.COB" USING VENT LIN PANTALLA
           MOVE PSLINEAS(3) TO SITIO
           CALL "LINVBUF.COB" USING VENT LIN PANTALLA
           MOVE PSLINEAS(4) TO SITIO
           CALL "LINVBUF.COB" USING VENT LIN PANTALLA
           MOVE PSLINEAS(5) TO SITIO
           CALL "LINVBUF.COB" USING VENT LIN PANTALLA
           MOVE 2201258004000011 TO VENT
           CALL "VENTBUF.COB" USING VENT PANTALLA
           MOVE TITULO TO LP(3)
           PERFORM MOSTRAR.
       MOSTRAR.
           PERFORM SACAR VARYING I FROM 2 BY 1 UNTIL I > 25.
       SACAR.
           DISPLAY LP(I) LINE I POSITION 1
           CONTROL "FCOLOR=WHITE, BCOLOR=BLUE".

      ** Controla el cambio de mesa,abriendo el fich. correspondiente **

       ELECCION.
           DISPLAY "MESA" LINE 22 POSITION 5
           MOVE 8 TO I
           PERFORM MENSAJE
           PERFORM WITH TEST AFTER UNTIL RESERVA > 0 AND < 17
               ACCEPT RESERVA UPDATE NO BEEP LINE 22 POSITION 15
               END-ACCEPT
           END-PERFORM
           MOVE LF(RESERVA) TO NOMES
           OPEN I-O MESAS
           CLOSE MESAS
           MOVE 0 TO INICIO
           MOVE 18 TO TEND
           PERFORM LEER.
       ALTAS.
           MOVE CONTP(1) TO POSICION
           MOVE "S" TO MODIFIC
           MOVE 0 TO PIVOTE
           MOVE "N" TO FIN
           DISPLAY LM(3) LINE 24 POSITION 2 REVERSE
           IF PUNTERO = 18
                MOVE 4 TO I
                PERFORM MENSAJE
                PERFORM ACEPTAR UNTIL TECLAF = 67 OR 68
                PERFORM TECLASFUNCION
                MOVE 1 TO I
                PERFORM MENSAJE
           END-IF.
           IF ESTADO = 0
                IF T-NUMPLATO(PUNTERO) = " " OR 0
                     MOVE "N" TO MODIFIC
                     ELSE MOVE T-NUMPLATO(PUNTERO) TO PIVOTE
                END-IF
                ACCEPT T-NUMPLATO(PUNTERO) LINE LINEA
                  POSITION CONTP(2) NO BEEP TAB UPDATE REVERSE
                  ON EXCEPTION TECLAF PERFORM TECLASFUNCION
                END-ACCEPT
           END-IF.
           IF ESTADO = 0
              PERFORM LOCALIZAR
           END-IF.
           IF ESTADO = 0
              ACCEPT T-CANTIDAD(PUNTERO) LINE LINEA
                 POSITION CONTP(4) NO BEEP TAB UPDATE REVERSE
                 ON EXCEPTION TECLAF PERFORM TECLASFUNCION
              END-ACCEPT
              DISPLAY T-NUMPLATO(PUNTERO) LINE LINEA POSITION CONTP(2)
              DISPLAY T-CANTIDAD(PUNTERO) LINE LINEA POSITION CONTP(4)
           COMPUTE T-TOT(PUNTERO) = T-CANTIDAD(PUNTERO) * T-PVP(PUNTERO)
              DISPLAY T-TOT(PUNTERO) LINE LINEA POSITION CONTP(6)
              IF MODIFIC = "S"
                 PERFORM MODIFICACIONES
                 ELSE ADD 1 TO CONTREG
              END-IF
              ADD 1 TO LINEA
              ADD 1 TO PUNTERO
           END-IF.

      * cuando hago una modificaci¢n lo primero que hago es una
      * lectura y si el fich. est  vacio dar  error. Por eso
      * grabo el primero y evito ese error.

           IF CONTREG = 2
               PERFORM APERTURA
           END-IF.
           MOVE 0 TO ESTADO.
       LEER.
           OPEN I-O MESAS
           MOVE 4 TO LINEA
           MOVE 2 TO POSICION
           CALL "CUROFF.EXE"
           PERFORM BLANCOS VARYING PUNTERO FROM 1 BY 1
              UNTIL PUNTERO > 17
           CALL "CURON.EXE"
           MOVE "N" TO FIN
           READ MESAS AT END MOVE "S" TO FIN
           END-READ
           MOVE 1 TO PUNTERO
           MOVE 4 TO LINEA
           MOVE 1 TO I
           MOVE 1 TO CONTREG
           PERFORM BUCLE-LEER UNTIL FIN = "S"
           MOVE 4 TO LINEA
           MOVE 1 TO PUNTERO
           CLOSE MESAS.

      ***** Cada vez que pasamos de pantalla o cambiamos de mesa *****
      ** limpiamos la pantalla para visualizar o aceptar mas regs.  **

       BLANCOS.
           MOVE ALL " " TO T-NUMPLATO(PUNTERO)
           MOVE ALL " " TO T-CODIGO(PUNTERO)
           MOVE ALL " " TO T-NOM(PUNTERO)
           MOVE ALL " " TO T-PVP(PUNTERO)
           MOVE ALL " " TO T-TOT(PUNTERO)
           MOVE ALL " " TO T-CANTIDAD(PUNTERO)
           DISPLAY LD(PUNTERO) LINE LINEA POSITION POSICION
           ADD 1 TO LINEA.

      ****** Carga en la tabla y visualiza los registros de mesas *****
      ***** si existen m s de 18 regs controla cuales ha de cargar ****
      *****       incrementando o decrementando INICIO Y TEND      ****

       BUCLE-LEER.
           IF I > INICIO AND I < TEND  AND M-NUMPLATO NOT = 0
               MOVE M-NUMPLATO TO T-NUMPLATO(PUNTERO)
               MOVE M-CODIGO TO T-CODIGO(PUNTERO)
               MOVE M-NOM TO T-NOM(PUNTERO)
               MOVE M-PVP TO T-PVP(PUNTERO)
               MOVE M-TOTAL TO T-TOT(PUNTERO)
               MOVE M-CANTIDAD TO T-CANTIDAD(PUNTERO)
               DISPLAY LD(PUNTERO) LINE LINEA POSITION POSICION
               ADD 1 TO LINEA
               ADD 1 TO PUNTERO
           END-IF.
           READ MESAS AT END MOVE "S" TO FIN
           END-READ.
           IF FIN NOT = "S" AND M-NUMPLATO NOT = 0
                ADD 1 TO CONTREG
                ADD 1 TO I
           END-IF.

      ***** Busqueda en el fichero platos.dat del reg. introducido *****
      ***** su paso a la tabla y su posterior visualizacion *****

       LOCALIZAR.
           IF PIVOTE NOT = T-NUMPLATO(PUNTERO)
             OPEN I-O MESAS
             PERFORM COMPROBAR UNTIL T-NUMPLATO(PUNTERO) = M-NUMPLATO
                 OR FIN = "S"
             CLOSE MESAS
             MOVE 1 TO I
             PERFORM Comprobar-tabla UNTIL I NOT < 18
             IF FIN = "N"
                MOVE 1 TO ESTADO
                MOVE 6 TO I
                MOVE 0 TO T-NUMPLATO(PUNTERO)
                PERFORM 1MENSAJE
                  ELSE
                    MOVE T-NUMPLATO(PUNTERO) TO NUMPLATO
                    READ PLATOS INVALID KEY
                      MOVE 5 TO I
                      PERFORM 1MENSAJE
                      MOVE 0 TO T-NUMPLATO(PUNTERO)
                      MOVE 1 TO ESTADO
                        NOT INVALID KEY
                          MOVE NUMPLATO TO T-NUMPLATO(PUNTERO)
                          MOVE CODIGO TO T-CODIGO(PUNTERO)
                          MOVE NOM TO T-NOM(PUNTERO)
                          MOVE PVP TO T-PVP(PUNTERO)
                          DISPLAY LD(PUNTERO) LINE LINEA
                            POSITION POSICION
                    END-READ
             END-IF
           END-IF.

      ****************** control de las teclas de funcion *************

       TECLASFUNCION.
      ** Ayuda y consulta de los platos
           IF TECLAF = 1
               CLOSE PLATOS
               MOVE 7 TO I
               PERFORM  1MENSAJE
               CALL "CONSULT.COB" USING T-NUMPLATO(PUNTERO)
               OPEN I-O PLATOS
               PERFORM LOCALIZAR
           END-IF.

      ** Cambio de mesa
           IF TECLAF = 2
               MOVE 1 TO ESTADO
               PERFORM APERTURA
               PERFORM ELECCION
           END-IF.

      ** Calculo, impresion y grabaci¢n de la factura
           IF TECLAF = 3
               MOVE 1 TO ESTADO
               MOVE 0 TO SUMTOTAL TOTIVA IVA
               MOVE 9 TO I
               PERFORM MENSAJE
               OPEN OUTPUT LISTADO
               MOVE "00" TO ERRORIMP
               WRITE FACTLIN FROM SPACES
               if errorimp = "00"
                  PERFORM APERTURA
                  PERFORM ImprimirFactura
                  PERFORM GrabarFactura
                  CLOSE MESAS
                  OPEN OUTPUT MESAS
                  CLOSE MESAS
                  PERFORM ELECCION
               end-if
               CLOSE LISTADO
           END-IF.

      ** Baja de un plato del fichero mesas
           IF TECLAF = 4
               MOVE 1 TO ESTADO
               MOVE "N" TO FIN
               MOVE 10 TO I
               PERFORM 1MENSAJE
               PERFORM APERTURA
               OPEN I-O MESAS
               READ MESAS
               PERFORM COMPROBAR UNTIL T-NUMPLATO(PUNTERO) = M-NUMPLATO
               MOVE 0 TO M-NUMPLATO
               REWRITE REG-ME
               END-REWRITE
               CLOSE MESAS
               MOVE 0 TO T-NUMPLATO(PUNTERO)
           END-IF.

      ** Opci¢n de salida mediante el programa "mensaje.cob"
           IF TECLAF = 27
               MOVE 1 TO ESTADO
               MOVE 1 TO NUMID
               CALL "MENSAJE.COB" USING NUMID
               MOVE  11 TO I
               PERFORM MENSAJE
               IF NUMID NOT = 1
                 MOVE 0 TO TECLAF
                 ELSE
                    PERFORM APERTURA
               END-IF
           END-IF.

      ** Tecla para moverte por pantalla hacia arriba
           IF TECLAF = 52
              DISPLAY T-NUMPLATO(PUNTERO) LINE LINEA POSITION CONTP(2)
              DISPLAY T-CANTIDAD(PUNTERO) LINE LINEA POSITION CONTP(4)
              COMPUTE LINEA = LINEA - 1
              COMPUTE PUNTERO = PUNTERO - 1
              IF LINEA = 3
                 MOVE 4 TO LINEA
                 MOVE 1 TO PUNTERO
              END-IF
              MOVE 1 TO ESTADO
           END-IF.

      ** Tecla para moverte por pantalla hacia abajo
           IF TECLAF = 53
              DISPLAY T-NUMPLATO(PUNTERO) LINE LINEA POSITION CONTP(2)
              DISPLAY T-CANTIDAD(PUNTERO) LINE LINEA POSITION CONTP(4)
              COMPUTE LINEA = LINEA + 1
              COMPUTE PUNTERO = PUNTERO + 1
              IF T-NUMPLATO(PUNTERO) = " "
                 COMPUTE LINEA = LINEA - 1
                 COMPUTE PUNTERO = PUNTERO - 1
              END-IF
              MOVE 1 TO ESTADO
           END-IF.

      ** Tecla para volver una p gina hacia atr s
           IF TECLAF = 67
              PERFORM APERTURA
              IF TEND > 20
                 COMPUTE INICIO = INICIO - 17
                 COMPUTE TEND = TEND - 17
              END-IF
              PERFORM LEER
              MOVE 1 TO ESTADO
              MOVE 4 TO LINEA
              MOVE 1 TO PUNTERO
           END-IF.

      ** Tecla para avanzar una p gina hacia delante
           IF TECLAF = 68
               IF PUNTERO = 18 AND CONTREG = 18 OR 35 OR 52 OR 69
                   PERFORM APERTURA
                   MOVE 4 TO LINEA
                   MOVE 2 TO POSICION
                   ADD 17 TO INICIO
                   ADD 17 TO TEND
                   PERFORM BLANCOS VARYING PUNTERO FROM 1 BY 1
                     UNTIL PUNTERO > 17
                   MOVE 4 TO LINEA
                   MOVE 1 TO PUNTERO
                   ELSE
                     PERFORM APERTURA
                     ADD 17 TO INICIO
                     ADD 17 TO TEND
                     PERFORM LEER
                     MOVE 1 TO PUNTERO
                     MOVE 4 TO LINEA
              END-IF
              MOVE 1 TO ESTADO
           END-IF.
       ACEPTAR.
           ACCEPT TECLAF LINE 24 POSITION 78 OFF NO BEEP
                ON EXCEPTION TECLAF CONTINUE
           END-ACCEPT.

      ****************  modificaciones del fichero mesas  *************

       MODIFICACIONES.
           MOVE 1 TO ESTADO
           MOVE "N" TO FIN
           OPEN I-O MESAS
           READ MESAS
           PERFORM COMPROBAR UNTIL PIVOTE = M-NUMPLATO OR FIN = "S"
           IF FIN = "N"
               MOVE T-CODIGO(PUNTERO) TO M-CODIGO
               MOVE T-NUMPLATO(PUNTERO) TO M-NUMPLATO
               MOVE T-NOM(PUNTERO) TO M-NOM
               MOVE T-PVP(PUNTERO) TO M-PVP
               MOVE T-CANTIDAD(PUNTERO) TO M-CANTIDAD
               MOVE T-TOT(PUNTERO) TO M-TOTAL
               REWRITE REG-ME
           END-IF.
           CLOSE MESAS.
       COMPROBAR.
           READ MESAS AT END MOVE "S" TO FIN
           END-READ.
       Comprobar-tabla.
           IF T-NUMPLATO(I) = T-NUMPLATO(PUNTERO) AND I NOT = PUNTERO
                 MOVE "N" TO FIN
           END-IF.
           ADD 1 TO I.

      ***************** grabaci¢n del fichero mesas  ****************

       APERTURA.
           PERFORM GRABAR VARYING A FROM 1 BY 1 UNTIL A > 17
              OR T-NUMPLATO(A) = " ".
       GRABAR.
           OPEN I-O MESAS
           MOVE "N" TO FIN
           PERFORM COMPROBAR UNTIL M-NUMPLATO = T-NUMPLATO(A)
              OR FIN = "S"
           CLOSE MESAS
           IF T-NUMPLATO(A) NOT = 0 AND FIN = "S"
              OPEN EXTEND MESAS
              MOVE T-CODIGO(A) TO M-CODIGO
              MOVE T-NUMPLATO(A) TO M-NUMPLATO
              MOVE T-NOM(A) TO M-NOM
              MOVE T-PVP(A) TO M-PVP
              MOVE T-CANTIDAD(A) TO M-CANTIDAD
              MOVE T-TOT(A) TO M-TOTAL
              WRITE REG-ME
              END-WRITE
              CLOSE MESAS
           END-IF.

      **********  c lculo , impresi¢n, y grabaci¢n de la factura ******

       FACTURA.
           MOVE M-NOM TO L-NOM
           MOVE M-CANTIDAD TO L-CANTIDAD
           MOVE M-PVP TO L-PVP
           MOVE M-TOTAL TO L-TOTAL
           WRITE FACTLIN FROM LINDET BEFORE ADVANCING 1 LINE
           END-WRITE
           ADD M-TOTAL TO SUMTOTAL
           READ MESAS AT END MOVE "S" TO FIN
           END-READ.
       ImprimirFactura.
           MOVE "N" TO FIN
           OPEN I-O MESAS
           WRITE FACTLIN FROM LIN1 BEFORE ADVANCING 1 LINE
           WRITE FACTLIN FROM LIN2 BEFORE ADVANCING 1 LINE
           WRITE FACTLIN FROM LIN3 BEFORE ADVANCING 1 LINE
           READ MESAS AT END MOVE "S" TO FIN
           END-READ
           PERFORM FACTURA UNTIL FIN = "S"
           WRITE FACTLIN FROM LIN4 BEFORE ADVANCING 1 LINE
           MOVE SUMTOTAL TO L-SUMTOTAL
           WRITE FACTLIN FROM LIN5 BEFORE ADVANCING 1 LINE
           COMPUTE IVA = SUMTOTAL * 0,06
           MOVE IVA TO L-IVA
           WRITE FACTLIN FROM LIN6 BEFORE ADVANCING 1 LINE
           WRITE FACTLIN FROM LIN7 BEFORE ADVANCING 1 LINE
           COMPUTE TOTIVA = IVA + SUMTOTAL
           MOVE TOTIVA TO L-TOTIVA
           WRITE FACTLIN FROM LIN8 BEFORE ADVANCING 3 LINE
           WRITE FACTLIN FROM LIN9 BEFORE ADVANCING 1 LINE
           END-WRITE.
       GrabarFactura.
           OPEN I-O FACTURA
           CALL "SALVA.EXE"
           PERFORM Buscar-ultimo VARYING NUMFACT FROM 1 BY 1 UNTIL
              NUMFACT > 998
           MOVE 10211662000001 TO VENTANA
           CALL "VENTANA.COB" USING VENTANA
           MOVE RESERVA TO FMESA
           MOVE TOTIVA TO FPASTA
           ACCEPT FechaSys FROM DATE END-ACCEPT
           DISPLAY "FACTURA: " LINE 10 POSITION 38 REVERSE
           DISPLAY NFAC LINE 10 POSITION 47
           DISPLAY "N§ MESA" LINE 12 POSITION 24 REVERSE
           DISPLAY FMESA LINE 13 POSITION 26
           DISPLAY "IMPORTE" LINE 12 POSITION 38 REVERSE
           DISPLAY L-TOTIVA LINE 13 POSITION 36
           DISPLAY "FECHA" LINE 12 POSITION 56 REVERSE
           DISPLAY Diasys LINE 13 POSITION 54
           DISPLAY "/" LINE 13 POSITION 56 REVERSE
           DISPLAY Messys LINE 13 POSITION 57
           DISPLAY "/" LINE 13 POSITION 59 REVERSE
           DISPLAY Anosys LINE 13 POSITION 60
           DISPLAY "PAGADA: " LINE 15 POSITION 32 REVERSE
           DISPLAY "GIF: " LINE 15 POSITION 45 REVERSE
           PERFORM WITH TEST AFTER UNTIL FPAGADA = "S" OR "s" OR "N"
           OR "n"
               ACCEPT FPAGADA LINE 15 POSITION 40 NO BEEP END-ACCEPT
           END-PERFORM
           IF FPAGADA =  "S"  OR "s"
               MOVE " " TO  FPAGADA
             ELSE
               MOVE "*" TO  FPAGADA
           END-IF
           MOVE DIASYS TO FDIA
           MOVE MESSYS TO FMES
           MOVE ANOSYS TO FANO
           MOVE TOTIVA TO FPASTA
           ACCEPT FGIF LINE 15 POSITION 50 TAB NO BEEP
           WRITE REG-FAC INVALID KEY MOVE 12 TO I
                                     PERFORM MENSAJE
                         NOT INVALID KEY MOVE 13 TO I
                                     PERFORM MENSAJE
           END-WRITE.
           CALL "RECUP.EXE"
           CLOSE FACTURA.
       Buscar-ultimo.
           MOVE NUMFACT TO NFAC
           READ FACTURA INVALID KEY
                MOVE 999 TO NUMFACT
                 NOT INVALID KEY
                   CONTINUE
           END-READ.
       MENSAJE.
           DISPLAY LM(I) LINE 24 POSITION 2 REVERSE.
       1MENSAJE.
           DISPLAY LM(I) LINE 24 POSITION 2 REVERSE
           ACCEPT NADA  NO BEEP
           DISPLAY LM(1) LINE 24 POSITION 2 REVERSE.