       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJER3.
       AUTHOR. CHICOTE-MARIO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PHILIPS.
       OBJECT-COMPUTER. PHILIPS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL LIBROS ASSIGN TO DISK "LIBROS.DAT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS NREG
           ALTERNATE RECORD KEY IS APEL1 WITH DUPLICATES
           ALTERNATE RECORD KEY IS APEL2 WITH DUPLICATES
           ALTERNATE RECORD KEY IS NOM WITH DUPLICATES
           ALTERNATE RECORD KEY IS TITULO WITH DUPLICATES
           ALTERNATE RECORD KEY IS EDIT WITH DUPLICATES
           ALTERNATE RECORD KEY IS ANEDIT WITH DUPLICATES.
       DATA DIVISION.
       FILE SECTION.
       FD LIBROS LABEL RECORD STANDARD.
       01 REG-LIB.
           02 NREG PIC X(6).
           02 AUTOR.
              03 APEL1 PIC X(10).
              03 APEL2 PIC X(10).
              03 NOM PIC X(10).
           02 TITULO PIC X(40).
           02 EDIT PIC X(15).
           02 ANEDIT PIC 9(4).
           02 NUMPAG PIC 9(4).
           02 PVP PIC 9(5).
           02 EXIST PIC 99.
       WORKING-STORAGE SECTION.
       77 ANTNOM PIC X(10).
       77 ANTNREG PIC X(6).
       77 ANTAPEL1 PIC X(10).
       77 ANTAPEL2 PIC X(10).
       77 ANTTITULO PIC X(40).
       77 ANTEDIT PIC X(15).
       77 ANTANEDIT PIC 9(4).
       77 BLANCOS PIC X(76) VALUE " ".
       77 NADA PIC X.
       77 CONTINUAR PIC X.
       77 FIN PIC X.
       77 CONT PIC 9.
       77 BORRA PIC X.
       77 EXISTIR PIC X.
       01 VENT.
           02 CUADRO PIC 9(8).
           02 HELP PIC 9(8).
       LINKAGE SECTION.
       77 MODO PIC 9.
       PROCEDURE DIVISION USING MODO.
       MAIN.
           MOVE "S" TO CONTINUAR
           OPEN I-O LIBROS
           MOVE 0 TO CONT
           DISPLAY " " ERASE
           MOVE 01010379 TO CUADRO
           MOVE 10000000 TO HELP
           CALL "VENTANA.COB" USING VENT
           MOVE 04012279 TO CUADRO
           CALL "VENTANA.COB" USING VENT
           MOVE 23012579 TO CUADRO
           MOVE 12000000 TO HELP
           CALL "VENTANA.COB" USING VENT
           DISPLAY "CONTROL EXISTENCIA LIBROS" LINE 2 POSITION 3
           PERFORM BAJA UNTIL CONTINUAR = "N" OR "n".
           CLOSE LIBROS
           DISPLAY " " ERASE
           EXIT PROGRAM.
       BAJA.
           DISPLAY "ISBN:      " LINE 11 POSITION   37
           DISPLAY "AUTOR" LINE 5 POSITION 5
           DISPLAY "APELLIDO 1§:             APELLIDO 2§:           "
              LINE 7 POSITION 5
           DISPLAY "NOMBRE:               " LINE 7 POSITION 57
           DISPLAY "TITULO:                                         "
              LINE 9 POSITION 5
           DISPLAY "EDITORIAL:                  " LINE 11 POSITION 5
           DISPLAY "A¥O DE EDICION:      " LINE 13 POSITION 5
           DISPLAY "NUM. PAGINAS:        " LINE 15 POSITION 5
           DISPLAY "PRECIO:           " LINE 17 POSITION 5
           DISPLAY "EXISTENCIAS:      " LINE 19 POSITION 5
           PERFORM ACCEPTATOTAL.
       BORRADO.
           DELETE LIBROS
           INVALID KEY DISPLAY "ERROR AL BORRAR" LINE 24 POSITION 7
           NOT INVALID KEY DISPLAY "BORRADO SATISFACTORIO"
           LINE 24 POSITION 7 REVERSE
           ACCEPT NADA OFF.
       ACCEPTATOTAL.
           EVALUATE MODO
               WHEN 1
                  ACCEPT TITULO LINE 9 POSITION 13 TAB
                  MOVE TITULO TO ANTTITULO
                  START LIBROS KEY IS = TITULO
                     INVALID KEY PERFORM ERROR1
                     NOT INVALID KEY
                            MOVE "N" TO FIN
                            READ LIBROS NEXT AT END MOVE "S" TO
                               CONTINUAR END-READ
                             PERFORM LEER1 WITH TEST AFTER UNTIL
                                CONTINUAR = "N" OR FIN = "S"
                                OR TITULO NOT = ANTTITULO
                  END-START
                WHEN 3
                  ACCEPT EDIT LINE 11 POSITION 16 TAB
                  MOVE EDIT TO ANTEDIT
                  START LIBROS KEY IS = EDIT
                     INVALID KEY PERFORM ERROR1
                     NOT INVALID KEY
                            MOVE "N" TO FIN
                            READ LIBROS NEXT AT END MOVE "S" TO
                               CONTINUAR END-READ
                             PERFORM LEER1 WITH TEST AFTER UNTIL
                                CONTINUAR = "N" OR FIN = "S"
                                OR EDIT NOT = ANTEDIT
                  END-START
                WHEN 4
                  ACCEPT ANEDIT LINE 13 POSITION 21 TAB
                  MOVE ANEDIT TO ANTANEDIT
                  START LIBROS KEY IS = ANEDIT
                     INVALID KEY PERFORM ERROR1
                     NOT INVALID KEY
                            MOVE "N" TO FIN
                            READ LIBROS NEXT AT END MOVE "S" TO
                               CONTINUAR END-READ
                             PERFORM LEER1 WITH TEST AFTER UNTIL
                                CONTINUAR = "N" OR FIN = "S"
                                OR ANEDIT NOT = ANTANEDIT
                  END-START
                WHEN 5
                  ACCEPT NREG LINE 11 POSITION 44 TAB
                  MOVE NREG TO ANTNREG
                  START LIBROS KEY IS = NREG
                     INVALID KEY PERFORM ERROR1
                     NOT INVALID KEY
                            MOVE "N" TO FIN
                            READ LIBROS NEXT AT END MOVE "S" TO
                               CONTINUAR END-READ
                             PERFORM LEER1 WITH TEST AFTER UNTIL
                                CONTINUAR = "N" OR FIN = "S"
                                OR NREG NOT = ANTNREG
                  END-START
                 WHEN 6
                   ACCEPT NOM LINE 7 POSITION 65 TAB
                   MOVE NOM TO ANTNOM
                   START LIBROS KEY IS = NOM
                      INVALID KEY PERFORM ERROR1
                      NOT INVALID KEY
                             MOVE "N" TO FIN
                             READ LIBROS NEXT AT END MOVE "S" TO
                               CONTINUAR END-READ
                             PERFORM LEER1 WITH TEST AFTER UNTIL
                                CONTINUAR = "N" OR FIN = "S"
                                OR NOM NOT = ANTNOM
                   END-START
                 WHEN 7
                   ACCEPT APEL1 LINE 7 POSITION 18 TAB
                   MOVE APEL1 TO ANTAPEL1
                   START LIBROS KEY IS = APEL1
                      INVALID KEY PERFORM ERROR1
                      NOT INVALID KEY
                             MOVE "N" TO FIN
                             READ LIBROS NEXT AT END MOVE "S" TO
                               CONTINUAR END-READ
                             PERFORM LEER1 WITH TEST AFTER UNTIL
                                CONTINUAR = "N" OR FIN = "S"
                                OR APEL1 NOT = ANTAPEL1
                   END-START
                 WHEN 8
                   ACCEPT APEL2 LINE 7 POSITION 43 TAB
                   MOVE APEL2 TO ANTAPEL2
                   START LIBROS KEY IS = APEL2
                      INVALID KEY PERFORM ERROR1
                      NOT INVALID KEY
                             MOVE "N" TO FIN
                             READ LIBROS NEXT AT END MOVE "S" TO
                               CONTINUAR END-READ
                             PERFORM LEER1 WITH TEST AFTER UNTIL
                                CONTINUAR = "N" OR FIN = "S"
                                OR APEL2 NOT = ANTAPEL2
                   END-START
           END-EVALUATE.
       LEER1.
           PERFORM MOSTRAR
           READ LIBROS NEXT AT END MOVE "S" TO FIN.
       MOSTRAR.
           DISPLAY BLANCOS LINE 24 POSITION 3 REVERSE
           DISPLAY "DATOS PARA BORRAR" LINE 24 POSITION 9 REVERSE
           DISPLAY APEL1 LINE 7 POSITION 18
           DISPLAY APEL2 LINE 7 POSITION 43
           DISPLAY NOM LINE 7 POSITION 65
           DISPLAY TITULO LINE 9 POSITION 13
           DISPLAY EDIT LINE 11 POSITION 16
           DISPLAY NREG LINE 11 POSITION 44
           DISPLAY ANEDIT LINE 13 POSITION 21
           DISPLAY NUMPAG LINE 15 POSITION 19
           DISPLAY PVP LINE 17 POSITION 14
           DISPLAY EXIST LINE 19 POSITION 18
           DISPLAY BLANCOS LINE 24 POSITION 3 REVERSE
           DISPLAY "¨BORRAR REGISTRO?" LINE 24 POSITION 7 REVERSE
           MOVE "N" TO BORRA
           ACCEPT BORRA LINE 24 POSITION 27 REVERSE UPDATE
           IF BORRA = "S"
                   PERFORM BORRADO.
           DISPLAY BLANCOS LINE 24 POSITION 3 REVERSE
           DISPLAY "DESEA CONTINUAR S/N?" LINE 24 POSITION 40
                 REVERSE
           ACCEPT CONTINUAR LINE 24 POSITION 61 REVERSE UPDATE.
       ERROR1.
           DISPLAY BLANCOS LINE 24 POSITION 3 REVERSE
           DISPLAY " NO HAY DATOS"
           LINE 24 POSITION 40 REVERSE
           ACCEPT NADA LINE 24 POSITION 61 REVERSE UPDATE
           DISPLAY BLANCOS LINE 24 POSITION 3 REVERSE
           DISPLAY "DESEA CONTINUAR S/N?" LINE 24 POSITION 40
                 REVERSE
           ACCEPT CONTINUAR LINE 24 POSITION 61 REVERSE UPDATE.
