       IDENTIFICATION DIVISION.
       PROGRAM-ID. MACAPP.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-400Mb.
       OBJECT-COMPUTER. IBM-400Mb.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 CONT PIC 99.
       77 COLOR1 PIC X(10) VALUE "BCOLOR=RED".
       01 VariablesTeclado.
           02 TECLA PIC X.
           02 TECLAFUNCION PIC 99 COMP.
       01 CamposHanler.
           02 BOTON PIC 99.
       01 POSICIONADORES.
           02 J        PIC 99.
           02 I        PIC 99.
           02 LINEA    PIC 99.
           02 POSICION PIC 99.
       01 VENTANA.
           02 ESQUINAS.
                 03 TOPL   PIC 99.
                 03 TOPP   PIC 99.
                 03 DOWNL  PIC 99.
                 03 DOWNP  PIC 99.
           02 HELPCUADRO.
                 03 ANCHO  PIC 99.
                 03 ALTO   PIC 99.
                 03 TIPO   PIC 9.
                 03 SOMBRA PIC 9.
       01 DatosVentanas.
           02 FILLER PIC 9(14) VALUE 11211463000041.
           02 FILLER PIC 9(14) VALUE 09231753000041.
           02 FILLER PIC 9(14) VALUE 00000000000000.
           02 FILLER PIC 9(14) VALUE 00000000000000.
           02 FILLER PIC 9(14) VALUE 00000000000000.
       01 VV REDEFINES DatosVentanas.
           02 EsVent PIC 9(14) OCCURS 5.
       01 DATOSMEN.
           02 FILLER PIC X(78) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE " Quiere salirse de la aplicacion".
           02 FILLER PIC X(32) VALUE "  (S/N)                         ".
           02 FILLER PIC X(14) VALUE ALL " ".
       01 MENSAJES REDEFINES DATOSMEN.
           02 LM PIC X(78) OCCURS 2.
       LINKAGE SECTION.
       77 NumID PIC 99.
       PROCEDURE DIVISION USING NumID.
       MAIN.
           MOVE ALL " " TO VariablesTeclado
           EVALUATE NumID
              WHEN 1
                MOVE EsVent(1) TO VENTANA
                CALL "VENTANA.COB" USING VENTANA
                CALL "CUROFF.EXE"
                MOVE 2 TO I
                PERFORM MENSAJE
                DISPLAY "¨SEGURO QUE QIERE SALIR?" LINE 11 POSITION 30
                REVERSE
                MOVE 1 TO BOTON
                PERFORM SALIDA UNTIL TECLA = "S" OR "s" OR "N" OR "n"
                  OR TECLAFUNCION = 13
                IF TECLAFUNCION = 13 AND BOTON = 1
                    MOVE 1 TO NumID
                  ELSE
                    MOVE 0 TO NumID
                END-IF
                IF TECLA = "S" OR "s"
                    MOVE 1 TO NumID
                END-IF
              WHEN 2
                MOVE EsVent(2) TO VENTANA
                CALL "VENTANA.COB" USING VENTANA
                CALL "CUROFF.EXE"
                COMPUTE J = TOPP + 2
                DISPLAY " La impresora no contesta. " LINE 9  POSITION J
                REVERSE
                DISPLAY "Asegurate de que este conec" LINE 10 POSITION J
                REVERSE
                DISPLAY "tada, con papel, la luz de " LINE 11 POSITION J
                REVERSE
                DISPLAY "SELEC encendida. Si tiene  " LINE 12 POSITION J
                REVERSE
                DISPLAY "papel, ojo que no este atas" LINE 13 POSITION J
                REVERSE
                DISPLAY "cado.  ( y de paso mira si " LINE 14 POSITION J
                REVERSE
                DISPLAY "tienes la bragueta subida)." LINE 15 POSITION J
                REVERSE
                DISPLAY "     ERROR-MUY-GORDO       " LINE 17 POSITION J
                BLINK
                ACCEPT TECLA LINE 25 POSITION 0 NO BEEP ON EXCEPTION
                  TECLAFUNCION CONTINUE END-ACCEPT
           END-EVALUATE
           DISPLAY LM(1) LINE 24 POSITION 2 REVERSE
           CALL "RECUP.EXE"
           CALL "CURON.EXE"
           EXIT PROGRAM.
      ******************************************************************
      *      Zona de llamadas primarias                                *
      ******************************************************************
       SALIDA.
           IF BOTON = 1
                 DISPLAY "ÚÄÄÄÄÄÄÄÄ¿" LINE 12 POSITION 30
                 DISPLAY "³   SI   ³" LINE 13 POSITION 30
                 DISPLAY "ÀÄÄÄÄÄÄÄÄÙ" LINE 14 POSITION 30
                 DISPLAY "ÚÄÄÄÄÄÄÄÄ¿" LINE 12 POSITION 44 REVERSE
                 DISPLAY "³   NO   ³" LINE 13 POSITION 44 REVERSE
                 DISPLAY "ÀÄÄÄÄÄÄÄÄÙ" LINE 14 POSITION 44 REVERSE
               ELSE
                 DISPLAY "ÚÄÄÄÄÄÄÄÄ¿" LINE 12 POSITION 30 REVERSE
                 DISPLAY "³   SI   ³" LINE 13 POSITION 30 REVERSE
                 DISPLAY "ÀÄÄÄÄÄÄÄÄÙ" LINE 14 POSITION 30 REVERSE
                 DISPLAY "ÚÄÄÄÄÄÄÄÄ¿" LINE 12 POSITION 44
                 DISPLAY "³   NO   ³" LINE 13 POSITION 44
                 DISPLAY "ÀÄÄÄÄÄÄÄÄÙ" LINE 14 POSITION 44
           END-IF
           PERFORM HandlerTeclas
           IF BOTON = 0 MOVE 2 TO BOTON END-IF
           IF BOTON = 3 MOVE 1 TO BOTON END-IF.
       MENSAJE.
           DISPLAY LM(I) LINE 24 POSITION 2 REVERSE.
      ******************************************************************
      *      Zona de llamadas secundarias                              *
      ******************************************************************
       HandlerTeclas.
           CALL "CUROFF.EXE"
           ACCEPT TECLA LINE 25 POSITION 0 NO BEEP ON EXCEPTION
            TECLAFUNCION PERFORM ControlBoton
           END-ACCEPT
           CALL "CURON.EXE".
       ControlBoton.
           IF TECLAFUNCION = 50 SUBTRACT 1 FROM BOTON END-IF
           IF TECLAFUNCION = 51 ADD 1 TO BOTON END-IF.
