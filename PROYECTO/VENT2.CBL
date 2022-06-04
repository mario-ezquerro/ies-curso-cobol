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
       01 POSICIONADORES.
           02 SITIO  PIC 99.
           02 POSICION PIC 99.
           02 J PIC 99.
           02 I PIC 99.
           02 LINEA PIC 99.
       01 BARRA.
          02 ELEMBARRA PIC X OCCURS 80.
       01 TablaColores.
           02 ColorA PIC X(27) VALUE "FCOLOR=WHITE, BCOLOR=BLUE".
           02 ColorN PIC X(27) VALUE "FCOLOR=WHITE, BCOLOR=BLAK".
           02 ColorV PIC X(27) VALUE "FCOLOR=WHITE, BCOLOR=BLUE".
       01 COLORES REDEFINES TablaColores.
           02 COL PIC X(27) OCCURS 3.
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
                 03 COLOR  PIC 9.
                 03 SOMBRA PIC 9.
       PROCEDURE DIVISION USING VENTANA.
       MAIN.
           IF COLOR = 0 MOVE 1 TO COLOR END-IF
           IF BIT2 = 0  MOVE 1 TO BIT2 END-IF
           IF TOPL = 0  MOVE 1 TO TOPL END-IF
           IF TOPP = 0  MOVE 1 TO TOPP END-IF
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
                COMPUTE DOWNP = TOPP + ANCHO - 1
              ELSE
                COMPUTE ANCHO = DOWNP - TOPP + 1
                COMPUTE ALTO  = DOWNL - TOPL
           END-IF
           IF BIT1 = 0
            PERFORM GUARRADA
            MOVE DIBUJOV(1 , BIT2) TO ELEMBARRA (1)
            MOVE DIBUJOV(3 , BIT2) TO ELEMBARRA (ANCHO)
            DISPLAY BARRA LINE TOPL POSITION TOPP SIZE ANCHO
               CONTROL COL(COLOR)
            MOVE ALL " " TO BARRA
            MOVE DIBUJOV(4 , BIT2) TO ELEMBARRA (1)
            MOVE DIBUJOV(4 , BIT2) TO ELEMBARRA (ANCHO)
            IF SOMBRA = 1
               ADD 1 TO ANCHO
               MOVE DIBUJOV(1 , 5) TO ELEMBARRA (ANCHO)
            END-IF
            COMPUTE I = TOPL + 1
            PERFORM LINHORZ VARYING LINEA FROM I BY 1
               UNTIL LINEA = DOWNL
            PERFORM GUARRADA
            IF SOMBRA = 1
                COMPUTE ANCHO = ANCHO - 1
            END-IF
            MOVE DIBUJOV(5 , BIT2) TO ELEMBARRA (1)
            MOVE DIBUJOV(6 , BIT2) TO ELEMBARRA (ANCHO)
            DISPLAY BARRA LINE DOWNL POSITION TOPP SIZE ANCHO
               CONTROL COL(COLOR)
            IF SOMBRA = 1
              COMPUTE LINEA = DOWNL + 1
              COMPUTE POSICION = TOPP + 1
              COMPUTE J = DOWNP + 1
              DISPLAY DIBUJOV(1 , 5) LINE DOWNL POSITION J
              MOVE ALL "±" TO BARRA
              DISPLAY  BARRA LINE LINEA POSITION POSICION
              SIZE ANCHO
            END-IF
            MOVE 0 TO ANCHO
            MOVE 0 TO ALTO
           ELSE
            PERFORM GUARRADA
            MOVE DIBUJOV(1 , BIT2) TO ELEMBARRA (1)
            MOVE DIBUJOV(3 , BIT2) TO ELEMBARRA (ANCHO)
            DISPLAY BARRA LINE TOPL POSITION TOPP SIZE ANCHO REVERSE
              CONTROL COL(COLOR)
            MOVE ALL " " TO BARRA
            MOVE DIBUJOV(4 , BIT2) TO ELEMBARRA (1)
            MOVE DIBUJOV(4 , BIT2) TO ELEMBARRA (ANCHO)
            IF SOMBRA = 1
               ADD 1 TO ANCHO
               MOVE DIBUJOV(1 , 5) TO ELEMBARRA (ANCHO)
            END-IF
            COMPUTE I = TOPL + 1
            PERFORM LINHORZR VARYING LINEA FROM I BY 1
               UNTIL LINEA = DOWNL
            PERFORM GUARRADA
            IF SOMBRA = 1
                COMPUTE ANCHO = ANCHO - 1
            END-IF
            MOVE DIBUJOV(5 , BIT2) TO ELEMBARRA (1)
            MOVE DIBUJOV(6 , BIT2) TO ELEMBARRA (ANCHO)
            DISPLAY BARRA LINE DOWNL POSITION TOPP SIZE ANCHO REVERSE
              CONTROL COL(COLOR)
            IF SOMBRA = 1
              COMPUTE LINEA = DOWNL + 1
              COMPUTE POSICION = TOPP + 1
              COMPUTE J = DOWNP + 1
              DISPLAY DIBUJOV(1 , 5) LINE DOWNL POSITION J
              MOVE ALL "±" TO BARRA
              DISPLAY  BARRA LINE LINEA POSITION POSICION REVERSE
              SIZE ANCHO
            END-IF
            MOVE 0 TO ANCHO
            MOVE 0 TO ALTO
           END-IF.
       LINHORZR.
           DISPLAY BARRA LINE LINEA POSITION TOPP SIZE ANCHO
           CONTROL COL(COLOR) REVERSE.
       LINHORZ.
           DISPLAY BARRA LINE LINEA POSITION TOPP SIZE ANCHO
           CONTROL COL(COLOR).
       GUARRADA.
           EVALUATE BIT2
              WHEN 1
                MOVE ALL "Í" TO BARRA
              WHEN 2
                MOVE ALL "Ä" TO BARRA
              WHEN 3
                MOVE ALL "Ä" TO BARRA
              WHEN 4
                MOVE ALL "Í" TO BARRA
              WHEN 5
                MOVE ALL "±" TO BARRA
              WHEN OTHER
                MOVE ALL "Í" TO BARRA
           END-EVALUATE.