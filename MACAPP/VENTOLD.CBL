       IDENTIFICATION DIVISION.
       PROGRAM-ID. MACAPP.
      *PROGRAMA QUE CALCULA LOS SUELDOS NETOS DE LOS CURRANTES
      *DIEZ QUE SE INTRODUCEN POR TECLAS
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-400Mb.
       OBJECT-COMPUTER. IBM-400Mb.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ASCYV.
           02 FILLER PIC X(5) VALUE "ÉÚÖÕ±".
           02 FILLER PIC X(5) VALUE "ÍÄÄÍ±".
           02 FILLER PIC X(5) VALUE "»¿·¸±".
           02 FILLER PIC X(5) VALUE "º³º³±".
           02 FILLER PIC X(5) VALUE "ÈÀÓÔ±".
           02 FILLER PIC X(5) VALUE "¼Ù½¾±".
       01 DVENT REDEFINES ASCYV.
           02 BLOQUESVENT OCCURS 6.
              03 DIBUJOV PIC X OCCURS 5.
       77 NADA PIC X.
       77 CONT PIC 99.
       77 SITIO  PIC 99.
       77 POSICION PIC 99.
       77 J PIC 99.
       77 I PIC 99.
       77 LINEA PIC 99.
       LINKAGE SECTION.
       01 VENTANA.
           02 ESQUINAS.
                 03 TOPL   PIC 99.
                 03 TOPP   PIC 99.
                 03 DOWNL  PIC 99.
                 03 DOWNP  PIC 99.
           02 HELPCUADRO.
                 03 TIPO.
                     04 BIT1 PIC 9.
                     04 BIT2 PIC 9.
                 03 ANCHO  PIC 99.
                 03 ALTO   PIC 99.
                 03 SOMBRA PIC 99.
       PROCEDURE DIVISION USING VENTANA.
       MAIN.
           IF BIT2 = 0
              MOVE 1 TO BIT2.
           IF TOPL = 0
              MOVE 1 TO TOPL.
           IF TOPP = 0
              MOVE 1 TO TOPP.
           PERFORM VENTANA.
           EXIT PROGRAM.
      *Este perform dibuja en la pantalla una ventana de la cual
      * le indicamos la esquina superior derecha y una de dos, o
      * la esquin inferior izquierda o el ancho y alto de esa ventana
      * que queremos, tambiem podemos indicar si queremos una sombra
      *NOTA: Al dibujar la ventana desaparece todo lo que este debajo
      *      de ella.
       VENTANA.
           IF ANCHO NOT = 0
                COMPUTE DOWNL = TOPL + ALTO
                COMPUTE DOWNP = TOPP + ANCHO
                MOVE 0 TO ALTO
                MOVE 0 TO ANCHO
           END-IF
           IF SOMBRA = 1 AND (DOWNP NOT = 80 AND DOWNL NOT = 25)
                COMPUTE I = TOPL + 1
                COMPUTE J = DOWNP + 1
                PERFORM LSOMBRAV VARYING LINEA FROM I BY 1 UNTIL
                  LINEA > (DOWNL + 1)
                COMPUTE J = TOPP + 1
                COMPUTE I = DOWNL + 1
                PERFORM LSOMBRAH VARYING POSICION FROM J BY 1 UNTIL
                  POSICION = (DOWNP + 1)
                MOVE 0 TO SOMBRA.
           EVALUATE BIT1
                 WHEN 0 THRU 1
                    DISPLAY DIBUJOV(1 , BIT2) LINE TOPL POSITION TOPP
                 WHEN 2 THRU 3
                    DISPLAY DIBUJOV(1 , BIT2) LINE TOPL POSITION TOPP
                       REVERSE
                 WHEN 4 THRU 5
                    DISPLAY DIBUJOV(1 , BIT2) LINE TOPL POSITION TOPP
                      BLINK
                 WHEN 6 THRU 7
                    DISPLAY DIBUJOV(1 , BIT2) LINE TOPL POSITION TOPP
                      BLINK REVERSE
                 WHEN OTHER PERFORM ERR00
           END-EVALUATE
           MOVE TOPL TO LINEA.
           COMPUTE I = TOPP + 1.
           PERFORM LineaHorizontal VARYING POSICION FROM I BY 1
             UNTIL POSICION = DOWNP.
           EVALUATE BIT1
                 WHEN 0 THRU 1
                    DISPLAY DIBUJOV(3 , BIT2) LINE TOPL POSITION DOWNP
                 WHEN 2 THRU 3
                    DISPLAY DIBUJOV(3, BIT2) LINE TOPL POSITION DOWNP
                      REVERSE
                 WHEN 4 THRU 5
                    DISPLAY DIBUJOV(3 , BIT2) LINE TOPL POSITION DOWNP
                       BLINK
                 WHEN 6 THRU 7
                    DISPLAY DIBUJOV(3 , BIT2) LINE TOPL POSITION DOWNP
                       BLINK REVERSE
                 WHEN OTHER PERFORM ERR00
           END-EVALUATE
           COMPUTE I = TOPL + 1
           PERFORM LINESDOS VARYING LINEA FROM I BY 1 UNTIL
             LINEA = DOWNL.
           EVALUATE BIT1
                 WHEN 0 THRU 1
                    DISPLAY DIBUJOV(5 , BIT2) LINE DOWNL POSITION TOPP
                 WHEN 2 THRU 3
                    DISPLAY DIBUJOV(5 , BIT2) LINE DOWNL POSITION TOPP
                       REVERSE
                 WHEN 4 THRU 5
                    DISPLAY DIBUJOV(5 , BIT2) LINE DOWNL POSITION TOPP
                       BLINK
                 WHEN 6 THRU 7
                    DISPLAY DIBUJOV(5 , BIT2) LINE DOWNL POSITION TOPP
                       BLINK REVERSE
                 WHEN OTHER PERFORM ERR00
           END-EVALUATE
           MOVE DOWNL TO LINEA.
           COMPUTE I = TOPP + 1.
           PERFORM LineaHorizontal VARYING POSICION FROM I BY 1
             UNTIL POSICION = DOWNP.
           EVALUATE BIT1
                 WHEN 0 THRU 1
                    DISPLAY DIBUJOV(6 , BIT2) LINE DOWNL POSITION DOWNP
                 WHEN 2 THRU 3
                    DISPLAY DIBUJOV(6 , BIT2) LINE DOWNL POSITION DOWNP
                      REVERSE
                 WHEN 4 THRU 5
                    DISPLAY DIBUJOV(6 , BIT2) LINE DOWNL POSITION DOWNP
                      BLINK
                 WHEN 6 THRU 7
                    DISPLAY DIBUJOV(6 , BIT2) LINE DOWNL POSITION DOWNP
                      BLINK REVERSE
                 WHEN OTHER PERFORM ERR00
           END-EVALUATE.
       LineaHorizontal.
           EVALUATE BIT1
                 WHEN 0 THRU 1
                    DISPLAY DIBUJOV(2 , BIT2) LINE LINEA POSITION
                      POSICION
                 WHEN 2 THRU 3
                    DISPLAY DIBUJOV(2 , BIT2) LINE LINEA POSITION
                      POSICION REVERSE
                 WHEN 4 THRU 5
                    DISPLAY DIBUJOV(2 , BIT2) LINE LINEA POSITION
                     POSICION BLINK
                 WHEN 6 THRU 7
                    DISPLAY DIBUJOV(2 , BIT2) LINE LINEA POSITION
                     POSICION BLINK REVERSE
                 WHEN OTHER PERFORM ERR00
           END-EVALUATE.
       LINESDOS.
           EVALUATE BIT1
                 WHEN 0 THRU 1
                    DISPLAY DIBUJOV(4 , BIT2) LINE LINEA POSITION TOPP
                 WHEN 2 THRU 3
                    DISPLAY DIBUJOV(4 , BIT2) LINE LINEA POSITION TOPP
                      REVERSE
                 WHEN 4 THRU 5
                    DISPLAY DIBUJOV(4 , BIT2) LINE LINEA POSITION TOPP
                       BLINK
                 WHEN 6 THRU 7
                    DISPLAY DIBUJOV(4 , BIT2) LINE LINEA POSITION TOPP
                       BLINK REVERSE
                 WHEN OTHER PERFORM ERR00
           END-EVALUATE
           COMPUTE J = TOPP + 1.
           PERFORM BLANCOS VARYING POSICION FROM J BY 1 UNTIL
              POSICION = DOWNP.
           EVALUATE BIT1
                 WHEN 0 THRU 1
                    DISPLAY DIBUJOV(4 , BIT2) LINE LINEA POSITION DOWNP
                 WHEN 2 THRU 3
                    DISPLAY DIBUJOV(4 , BIT2) LINE LINEA POSITION DOWNP
                      REVERSE
                 WHEN 4 THRU 5
                    DISPLAY DIBUJOV(4 , BIT2) LINE LINEA POSITION DOWNP
                      BLINK
                 WHEN 6 THRU 7
                    DISPLAY DIBUJOV(4 , BIT2) LINE LINEA POSITION DOWNP
                      BLINK REVERSE
                 WHEN OTHER PERFORM ERR00
           END-EVALUATE.
       BLANCOS.
           EVALUATE BIT1
                 WHEN 0
                    DISPLAY " " LINE LINEA POSITION POSICION
                 WHEN 2
                    DISPLAY " " LINE LINEA POSITION POSICION
                      REVERSE
                 WHEN 4
                    DISPLAY " " LINE LINEA POSITION POSICION BLINK
                 WHEN 6
                    DISPLAY " " LINE LINEA POSITION POSICION
                    BLINK REVERSE
           END-EVALUATE.
       LSOMBRAV.
           EVALUATE BIT1
                 WHEN 0 THRU 1
                    DISPLAY DIBUJOV(1 , 5)  LINE LINEA POSITION J
                 WHEN 2 THRU 3
                    DISPLAY DIBUJOV(1 , 5)  LINE LINEA POSITION J
                      REVERSE
                 WHEN 4 THRU 5
                    DISPLAY DIBUJOV(1 , 5)  LINE LINEA POSITION J BLINK
                 WHEN 6 THRU 7
                    DISPLAY DIBUJOV(1 , 5)  LINE LINEA POSITION J
                    BLINK REVERSE
                 WHEN OTHER PERFORM ERR00
           END-EVALUATE.
       LSOMBRAH.
           EVALUATE BIT1
                 WHEN 0 THRU 1
                    DISPLAY DIBUJOV(1 , 5)  LINE I POSITION POSICION
                 WHEN 2 THRU 3
                    DISPLAY DIBUJOV(1 , 5)  LINE I POSITION POSICION
                      REVERSE
                 WHEN 4 THRU 5
                    DISPLAY DIBUJOV(1 , 5)  LINE I POSITION POSICION
                      BLINK
                 WHEN 6 THRU 7
                    DISPLAY DIBUJOV(1 , 5)  LINE I POSITION POSICION
                      BLINK REVERSE
                 WHEN OTHER PERFORM ERR00
           END-EVALUATE.
       ERR00.
           DISPLAY "ERROR EN EL BIT1" LINE 24 POSITION 3.