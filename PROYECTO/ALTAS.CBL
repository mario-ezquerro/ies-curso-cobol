       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTA-PLA.
       AUTHOR. CHICOTE-MARIO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PHILIPS.
       OBJECT-COMPUTER. PHILIPS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL PLATOS ASSIGN TO DISK "PLATOS.DAT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS NUMPLATO
           ALTERNATE RECORD KEY IS CODIGO WITH DUPLICATES.
       DATA DIVISION.
       FILE SECTION.
       FD PLATOS LABEL RECORD STANDARD.
       01 REG-PLA.
           02 NUMPLATO PIC 99.
           02 CODIGO PIC XX.
           02 NOM PIC X(26).
           02 PVP PIC 9(4).
       WORKING-STORAGE SECTION.
       77 CONTINUAR PIC X.
       77 VENT PIC 9(16).
       77 VENTANA PIC 9(14).
       77 MAXLINEAS PIC 99 VALUE 21.
       77 TECLA PIC X.
       77 TECLAF PIC 99 COMP.
       77 SALTO PIC 99 VALUE 40.
       77 CONTREG PIC S9(2).
       77 MAS PIC X VALUE "S".
       01 FLAGS.
           02 BIEN PIC X.
           02 REG-MODIF PIC X VALUE "N".
           02 ESTADO PIC 9.
           02 NUMID PIC 99.
       01 LIN.
           02 SITIO PIC 99.
           02 REPETICION PIC 99.
       01 CONTADORES.
           02 I PIC 99.
           02 J PIC 99.
           02 K PIC 99.
           02 CONTL PIC 99.
           02 CONTP PIC 99 OCCURS 4.
           02 CONTR PIC 99 VALUE 0.
           02 PUNTERO PIC 99.
           02 LADO PIC X VALUE "I".
           02 VEZ PIC 99 VALUE 1.
           02 LINEA PIC 99.
           02 POSICION PIC 99.
       01 PANTALLA.
            02 LP PIC X(80) OCCURS 25.
       01 DATOSPAN.
            02 LD OCCURS 37.
               03 FILLER PIC X VALUE " ".
               03 PNUMPLATO PIC 99.
               03 FILLER PIC X VALUE "³".
               03 PCODIGO PIC XX.
               03 FILLER PIC X VALUE " ".
               03 PNOM PIC X(26).
               03 FILLER PIC X VALUE "³".
               03 PPVP PIC 9(4).
       01 DATOSMEN.
           02 FILLER PIC X(78) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "              ¨DESEA CONTINUAR? ".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "  ESC=SALIR    USA LAS FLECHAS  ".
           02 FILLER PIC X(32) VALUE "  F3=PLATO NUEVO F4=BORRAR PLATO".
           02 FILLER PIC X(14) VALUE "F5:IMPRIMIR ".
           02 FILLER PIC X(32) VALUE "       ERROR DE DISCO ".
           02 FILLER PIC X(32) VALUE " O EL PLATO YA EXISTE".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "     ESTE PLATO NO EXISTE      ".
           02 FILLER PIC X(32) VALUE "                               ".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "     CODIGO DE PLATO EXISTENTE ".
           02 FILLER PIC X(32) VALUE " DESEA MODIFICARLO? (S/N)   ".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "    DESEA GUARDAR LOS CAMBIOS E".
           02 FILLER PIC X(32) VALUE "FECTUADOS EN EL REGISTRO? (S/N) ".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "  GRABACION SUPER SATISFACTORIA".
           02 FILLER PIC X(32) VALUE ALL " ".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE " ATENCION DEME TODOS LOS CAMPOS ".
           02 FILLER PIC X(32) VALUE "PARA CANCELAR PULSE ESC         ".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE " EN ESTE MOMENTO A CANCELADO ".
           02 FILLER PIC X(32) VALUE "NO SE PRODICIRA NINGUNA ALTA  ".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE " SEGURO QUE QUIER BORRAR ESTE ".
           02 FILLER PIC X(32) VALUE " DELICIOSO PLATO?  (S/N)       ".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "                               ".
           02 FILLER PIC X(32) VALUE " EL FICHERO ESTA LLENO         ".
           02 FILLER PIC X(14) VALUE ALL " ".
       01 MENSAJES REDEFINES DATOSMEN.
           02 LM PIC X(78) OCCURS 12.
       01 TITULO.
           02 FILLER PIC X(19) VALUE "ÕÍCDÑÍÍÍÍNOMBRE DEL".
           02 FILLER PIC X(20) VALUE " PLATOÍÍÍÍÍÍÍÍÍÑÍPVP".
           02 FILLER PIC X(20) VALUE "ÑÑÍCDÑÍÍÍNOMBRE DEL ".
           02 FILLER PIC X(21) VALUE "PLATOÍÍÍÍÍÍÍÍÍÍÑÍPVP¸".
       01 DATOSVENTANA.
           02 FILLER PIC 99 VALUE 04.
           02 FILLER PIC 99 VALUE 34.
           02 FILLER PIC 99 VALUE 39.
           02 FILLER PIC 99 VALUE 40.
           02 FILLER PIC 99 VALUE 44.
           02 FILLER PIC 99 VALUE 74.
       01 POSILINEAS REDEFINES DATOSVENTANA.
           02 PSLINEAS PIC 99 OCCURS 6.
       PROCEDURE DIVISION.
       MAIN.
           PERFORM PRESENTA
           OPEN I-O PLATOS.
           MOVE "S" TO CONTINUAR
           MOVE "N" TO BIEN
           MOVE "I" TO LADO
           PERFORM INICI
           MOVE 1 TO CONTREG
           PERFORM LEER
           MOVE 0 TO TECLAF
           PERFORM ALTAS UNTIL TECLAF = 27.
           CLOSE PLATOS
           DISPLAY " " CONTROL "FCOLOR=BLAK, BCOLOR=BLAK"
           DISPLAY " " ERASE
           EXIT PROGRAM.


      * ************************ PROCESO DE ALTAS *********************
       BuscarUltimo.
           MOVE I TO NUMPLATO
           READ PLATOS INVALID KEY
                       MOVE I TO PNUMPLATO(37)
                       MOVE 99 TO I
                       NOT INVALID KEY
                       CONTINUE
           END-READ.
       ALTAS.
           DISPLAY LM(3) LINE 24 POSITION 2 REVERSE
           DISPLAY PNUMPLATO(PUNTERO)LINE CONTL
           POSITION CONTP(1) REVERSE
           IF ESTADO = 0
                ACCEPT PCODIGO(PUNTERO) LINE CONTL
                  POSITION CONTP(2) NO BEEP TAB UPDATE REVERSE
                  ON EXCEPTION TECLAF PERFORM TECLASFUNCION
                END-ACCEPT
           END-IF.
           IF ESTADO = 0
                ACCEPT PNOM(PUNTERO) LINE CONTL
                  POSITION CONTP(3) NO BEEP TAB UPDATE REVERSE
                  ON EXCEPTION TECLAF PERFORM TECLASFUNCION
                END-ACCEPT
           END-IF.
           IF ESTADO = 0
               ACCEPT PPVP(PUNTERO) LINE CONTL
                  POSITION CONTP(4) NO BEEP TAB UPDATE REVERSE
                  ON EXCEPTION TECLAF PERFORM TECLASFUNCION
               END-ACCEPT
               MOVE "S" TO REG-MODIF
               PERFORM GRABAR
               MOVE "N" TO REG-MODIF
           END-IF.
           PERFORM INTRO
           MOVE 0 TO ESTADO
           IF TECLAF = 27
               MOVE 1 TO NUMID
               CALL "MENSAJE.COB" USING NUMID
               IF NUMID NOT = 1 MOVE 0 TO TECLAF END-IF
           END-IF.
       AcceptarAltas.
              MOVE 0 TO ESTADO
              DISPLAY LM(9) LINE 24 POSITION 2 REVERSE
              DISPLAY LM(1) LINE 23 POSITION 2 REVERSE
              PERFORM BuscarUltimo VARYING I FROM 1 BY 1 UNTIL I = 00
              IF ESTADO = 0
                IF PNUMPLATO(37) = 00
                      MOVE 12 TO I
                      PERFORM MENSAJE
                      MOVE 1 TO ESTADO
                END-IF
                DISPLAY PNUMPLATO(37) LINE 13 POSITION 24
              END-IF
              IF ESTADO = 0
                  ACCEPT PCODIGO(37) LINE 13
                  POSITION 27 NO BEEP UPDATE
                  ON EXCEPTION TECLAF PERFORM TECLALTAS
                END-ACCEPT
              END-IF
              IF ESTADO = 0
                MOVE ALL " " TO PNOM(37)
                DISPLAY PNOM(37) LINE 13 POSITION 30
                ACCEPT PNOM(37) LINE 13
                   POSITION 30 NO BEEP TAB
                   ON EXCEPTION TECLAF PERFORM TECLALTAS
                END-ACCEPT
              END-IF
              IF ESTADO = 0
                ACCEPT PPVP(37) LINE 13
                   POSITION 57  NO BEEP TAB UPDATE
                   ON EXCEPTION TECLAF PERFORM TECLALTAS
                END-ACCEPT
                MOVE PNUMPLATO(37) TO NUMPLATO
                MOVE PCODIGO(37)   TO CODIGO
                MOVE PNOM(37)      TO NOM
                MOVE PPVP(37)      TO PVP
                MOVE "N" TO REG-MODIF
                PERFORM GRABAR
                DISPLAY LM(1) LINE 23 POSITION 2 REVERSE
              END-IF.
       BORRAR.
           PERFORM BucleBorrar VARYING I FROM 1 BY 1 UNTIL I > 36.
           MOVE 2 TO POSICION
           CALL "CUROFF.EXE"
           PERFORM BucleSacar  VARYING I FROM 3 BY 1 UNTIL I > 20.
           ADD SALTO TO POSICION
           PERFORM BucleSacar  VARYING I FROM 3 BY 1 UNTIL I > 20
           CALL "CURON.EXE".
       BucleBorrar.
           MOVE ALL " "  TO PNUMPLATO(I)
           MOVE ALL " "  TO PCODIGO(I)
           MOVE ALL " "  TO PNOM (I)
           MOVE ALL " "  TO PPVP(I).
       BucleSacar.
           DISPLAY LD(I) LINE I POSITION POSICION.
       GRABAR.
           IF REG-MODIF = "S"
             PERFORM BUSCAR
             MOVE PNUMPLATO(PUNTERO) TO NUMPLATO
             MOVE PCODIGO(PUNTERO)   TO CODIGO
             MOVE PNOM(PUNTERO)      TO NOM
             MOVE PPVP(PUNTERO)      TO PVP
             MOVE "N" TO REG-MODIF
             MOVE 7 TO I
             PERFORM MENSAJE
             IF TECLA = "S" OR "s"
                 REWRITE REG-PLA INVALID KEY MOVE 4 TO I
                                          PERFORM MENSAJE
                          NOT INVALID KEY MOVE 8 TO I
                                          PERFORM MENSAJE
                 END-REWRITE
               ELSE
                 MOVE PNUMPLATO(PUNTERO) TO NUMPLATO
                 READ PLATOS INVALID KEY
                             MOVE ALL " " TO PCODIGO(PUNTERO)
                             MOVE ALL " " TO PNOM(PUNTERO)
                             MOVE ALL " " TO PPVP(PUNTERO)
                             MOVE 5 TO I
                             PERFORM MENSAJE
                       NOT INVALID KEY
                             MOVE CODIGO TO PCODIGO(PUNTERO)
                             MOVE NOM    TO PNOM(PUNTERO)
                             MOVE PVP    TO PPVP(PUNTERO)
                  END-READ
              END-IF
              COMPUTE I = CONTP(1) - 1
              DISPLAY LD(PUNTERO) LINE CONTL POSITION I REVERSE
            ELSE
              WRITE REG-PLA INVALID KEY MOVE 4 TO I
                                          PERFORM MENSAJE
                          NOT INVALID KEY MOVE 8 TO I
                                          PERFORM MENSAJE
              END-WRITE
           END-IF.
       MOSTRAR.
           PERFORM SACAR VARYING I FROM 2 BY 1 UNTIL I > 25.
       SACAR.
           DISPLAY LP(I) LINE I POSITION 1
           CONTROL "FCOLOR=WHITE, BCOLOR=BLUE".


      * ************** PROCESO DE MODIFICACIONES Y BAJAS **************
       LEER.
           MOVE 1 TO I
           MOVE 3 TO LINEA
           MOVE 2 TO POSICION
           PERFORM BUCLE-LEER UNTIL I = 37.
       BUCLE-LEER.
           MOVE CONTREG TO NUMPLATO
           IF CONTREG NOT = 99
               READ PLATOS
                   INVALID KEY
                      ADD 1 TO CONTREG
                   NOT INVALID KEY
                      MOVE NUMPLATO TO PNUMPLATO(I)
                      MOVE CODIGO TO PCODIGO(I)
                      MOVE NOM TO PNOM(I)
                      MOVE PVP TO PPVP(I)
                      DISPLAY LD(I) LINE LINEA POSITION POSICION
                      ADD 1 TO I
                      ADD 1 TO CONTREG
                      ADD 1 TO LINEA
                      IF LINEA = MAXLINEAS
                          MOVE 3 TO LINEA
                          ADD SALTO TO POSICION
                      END-IF
                END-READ
             ELSE
                MOVE 37 TO I
           END-IF.
       MENSAJE.
           DISPLAY LM(I) LINE 24 POSITION 2 REVERSE
           ACCEPT TECLA  OFF
           DISPLAY LM(1) LINE 24 POSITION 2 REVERSE
           DISPLAY "Ô" LINE 25 POSITION 1.

      * ******************** PROCESO DE BUSQUEDA ********************
       BUSCAR.
           MOVE PNUMPLATO(PUNTERO) TO NUMPLATO
           READ PLATOS INVALID KEY
                             MOVE ALL " " TO PCODIGO(PUNTERO)
                             MOVE ALL " " TO PNOM(PUNTERO)
                             MOVE ALL " " TO PPVP(PUNTERO)
                             COMPUTE I = CONTP(1) - 1
                             DISPLAY LD(PUNTERO)
                             LINE CONTL POSITION I
                             MOVE 5 TO I
                             PERFORM MENSAJE
                       NOT INVALID KEY
                             CONTINUE
           END-READ.

      ******************* CONTROL DE LA POSICION EN LA TABLA **********
       INTRO.
           IF TECLAF = 13
             COMPUTE I = CONTP(1) - 1
             DISPLAY LD(PUNTERO) LINE CONTL POSITION I
             ADD 1 TO CONTL
             ADD 1 TO PUNTERO
             PERFORM CONTROLES
             COMPUTE I = CONTP(1) - 1
             DISPLAY LD(PUNTERO) LINE CONTL POSITION I REVERSE
           END-IF.
       TECLASFUNCION.
           MOVE 1 TO ESTADO
           COMPUTE I = CONTP(1) - 1
           DISPLAY LD(PUNTERO) LINE CONTL POSITION I
           IF TECLAF = 4
                MOVE PNUMPLATO(PUNTERO) TO NUMPLATO
                MOVE 11 TO I
                PERFORM MENSAJE
                IF TECLA =  "S" OR "s"  AND PNUMPLATO(PUNTERO) NOT = 0
                   DELETE PLATOS
                   MOVE ALL " " TO PCODIGO(PUNTERO)
                   MOVE ALL " " TO PNOM(PUNTERO)
                   MOVE ALL " " TO PPVP(PUNTERO)
                   MOVE ALL "0" TO PNUMPLATO(PUNTERO)
                   COMPUTE I = CONTP(1) - 1
                   DISPLAY LD(PUNTERO) LINE CONTL POSITION I
                END-IF
           END-IF
           IF TECLAF = 5
                CLOSE PLATOS
                CALL "LISPLA.COB"
                OPEN I-O PLATOS
           END-IF
           IF TECLAF = 52
                 COMPUTE CONTL = CONTL - 1
                 COMPUTE PUNTERO = PUNTERO - 1
           END-IF.
           IF TECLAF = 53
                 ADD 1 TO CONTL
                 ADD 1 TO PUNTERO
                 PERFORM CONTROLES
           END-IF.
           IF TECLAF = 67 AND  PNUMPLATO(1) NOT = 1
              COMPUTE CONTREG = PNUMPLATO(1) - 36
              IF CONTREG < 1 MOVE 1 TO CONTREG END-IF
              PERFORM BORRAR
              PERFORM LEER
              MOVE "I" TO LADO
              PERFORM INICI
           END-IF.
           IF TECLAF = 68
              MOVE "M" TO BIEN
              MOVE 36 TO I
              PERFORM UltimoPantalla UNTIL BIEN = "B"
              MOVE I TO CONTREG
              PERFORM BORRAR
              PERFORM LEER
              MOVE "I" TO LADO
              PERFORM INICI
           END-IF
           IF TECLAF = 3
              MOVE 11211463000041 TO VENTANA
              CALL "VENTANA.COB" USING VENTANA
              DISPLAY "PLATOS-NUEVOS" LINE 10 POSITION 35 REVERSE
              DISPLAY "ÄÄÄÄÄÄÄÄÄÄÄÄÄ" LINE 11 POSITION 35 REVERSE
              DISPLAY "N§ CD NOMBRE DEL PLATO           PVP"
              LINE 12 POSITION 24 REVERSE
              PERFORM AcceptarAltas UNTIL TECLAF  = 27
              CALL "RECUP.EXE"
              MOVE 0 TO TECLAF
              MOVE PNUMPLATO(1) TO CONTREG
              PERFORM LEER
           END-IF
           IF TECLAF = 50 OR 51
               IF LADO = "D"
                 COMPUTE PUNTERO = PUNTERO - 18
                 MOVE "I" TO LADO
                 MOVE 3  TO CONTP(1)
                 MOVE 6  TO CONTP(2)
                 MOVE 9  TO CONTP(3)
                 MOVE 36 TO CONTP(4)
               ELSE
                 MOVE "D" TO LADO
                 ADD 18 TO PUNTERO
                 ADD SALTO TO CONTP(1)
                 ADD SALTO TO CONTP(2)
                 ADD SALTO TO CONTP(3)
                 ADD SALTO TO CONTP(4)
              END-IF
           END-IF
           PERFORM CONTROLES
           COMPUTE I = CONTP(1) - 1
           DISPLAY LD(PUNTERO) LINE CONTL POSITION I REVERSE.
       TECLALTAS.
           IF TECLAF = 27
            MOVE 1 TO ESTADO
            MOVE 10  TO I
            PERFORM MENSAJE
           END-IF.
       CONTROLES.
           IF CONTL = 2
                  IF LADO = "I"
                      MOVE "D" TO LADO
                      PERFORM INICI
                      MOVE 20 TO CONTL
                      MOVE 36 TO PUNTERO
                    ELSE
                      MOVE "I" TO LADO
                      PERFORM INICI
                      MOVE 20 TO CONTL
                      MOVE 18 TO PUNTERO
                   END-IF
            END-IF.
           IF CONTL = MAXLINEAS
              IF LADO = "I"
                   MOVE "D" TO LADO
                   PERFORM INICI
                ELSE
                   MOVE "I" TO LADO
                   PERFORM INICI
              END-IF
           END-IF.
           PERFORM SITUAR.
       SITUAR.
           MOVE "M" TO BIEN
           PERFORM SituarActivo UNTIL BIEN = "B".
       SituarActivo.
           IF PNUMPLATO(PUNTERO) =  ALL " "
                COMPUTE PUNTERO = PUNTERO - 1
                COMPUTE CONTL = CONTL - 1
                IF CONTL = 2
                  IF LADO = "I"
                      ADD 1 TO PUNTERO
                      ADD 1 TO CONTL
                      MOVE "B" TO BIEN
                    ELSE
                      MOVE "I" TO LADO
                      PERFORM INICI
                      MOVE 20 TO CONTL
                      MOVE 18 TO PUNTERO
                   END-IF
                END-IF
             ELSE
               MOVE "B" TO BIEN
           END-IF.
       INICI.
           IF LADO = "I"
                 MOVE 3  TO CONTL
                 MOVE 1  TO PUNTERO
                 MOVE 3  TO CONTP(1)
                 MOVE 6  TO CONTP(2)
                 MOVE 9  TO CONTP(3)
                 MOVE 36 TO CONTP(4)
             ELSE
                 MOVE 3    TO CONTL
                 MOVE 19   TO PUNTERO
                 ADD SALTO TO CONTP(1)
                 ADD SALTO TO CONTP(2)
                 ADD SALTO TO CONTP(3)
                 ADD SALTO TO CONTP(4)
           END-IF.
       UltimoPantalla.
           IF PNUMPLATO(I) = ALL " "
               SUBTRACT 1 FROM I
              ELSE
               MOVE "B" TO BIEN
           END-IF.
       PRESENTA.
           MOVE  0200218004000011 TO VENT
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
           MOVE PSLINEAS(6) TO SITIO
           CALL "LINVBUF.COB" USING VENT LIN PANTALLA
           MOVE  2200258004000011 TO VENT
           CALL "VENTBUF.COB" USING VENT PANTALLA
           MOVE TITULO TO LP(2)
           PERFORM MOSTRAR.