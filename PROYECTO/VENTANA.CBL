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
       77 CONT PIC 99.
       77 COLOR1 PIC X(10) VALUE "BCOLOR=RED".
       01 POSICIONADORES.
           02 J        PIC 99.
           02 I        PIC 99.
           02 LINEA    PIC 99.
           02 POSICION PIC 99.
           02 HTOP     PIC 99.
           02 HDOWN    PIC 99.
       01 BARRA.
          02 ELEMBARRA PIC X OCCURS 80.
       LINKAGE SECTION.
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
       PROCEDURE DIVISION USING VENTANA.
       MAIN.
           IF TIPO = 0 MOVE 1 TO TIPO END-IF
           IF TOPL = 0 MOVE 1 TO TOPL END-IF
           IF TOPP = 0 MOVE 1 TO TOPP END-IF
           PERFORM VENTANA
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
           COMPUTE I = (TOPL + DOWNL) / 2
           MOVE I TO HDOWN
           MOVE I TO HTOP
           CALL "CUROFF.EXE"
           CALL "SALVA.EXE"
           MOVE ALL " " TO BARRA
           MOVE DIBUJOV(4 , TIPO) TO ELEMBARRA (1)
           MOVE DIBUJOV(4 , TIPO) TO ELEMBARRA (ANCHO)
           PERFORM DIBUJAR UNTIL TOPL > HTOP AND DOWNL < HDOWN
           PERFORM CARGARLINEA
           MOVE DIBUJOV(1 , TIPO) TO ELEMBARRA (1)
           MOVE DIBUJOV(3 , TIPO) TO ELEMBARRA (ANCHO)
           DISPLAY BARRA LINE HTOP POSITION TOPP SIZE ANCHO REVERSE
           MOVE DIBUJOV(5 , TIPO) TO ELEMBARRA (1)
           MOVE DIBUJOV(6 , TIPO) TO ELEMBARRA (ANCHO)
           DISPLAY BARRA LINE HDOWN POSITION TOPP SIZE ANCHO REVERSE
           IF SOMBRA = 1
             COMPUTE I = (TOPL + DOWNL) / 2
             MOVE I TO HDOWN
             MOVE I TO HTOP
             COMPUTE POSICION = 1 + DOWNP
             PERFORM SacarSombra UNTIL TOPL > HTOP AND DOWNL < HDOWN
             DISPLAY " " LINE HDOWN POSITION POSICION CONTROL COLOR1
             MOVE ALL " " TO BARRA
             COMPUTE POSICION = TOPP + 1
             COMPUTE LINEA    = HDOWN + 1
             DISPLAY  BARRA LINE LINEA POSITION POSICION CONTROL COLOR1
             SIZE ANCHO
           END-IF
           MOVE 0 TO ANCHO
           MOVE 0 TO ALTO.
           CALL "CURON.EXE".
       SacarSombra.
           DISPLAY " " LINE HTOP  POSITION POSICION CONTROL COLOR1
           DISPLAY " " LINE HDOWN POSITION POSICION CONTROL COLOR1
           COMPUTE HTOP = HTOP - 1
           ADD 1 TO HDOWN.
       DIBUJAR.
           DISPLAY BARRA LINE HTOP POSITION TOPP SIZE ANCHO REVERSE
           DISPLAY BARRA LINE HDOWN POSITION TOPP SIZE ANCHO REVERSE
           COMPUTE HTOP = HTOP - 1
           ADD 1 TO HDOWN.
       CARGARLINEA.
           EVALUATE TIPO
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