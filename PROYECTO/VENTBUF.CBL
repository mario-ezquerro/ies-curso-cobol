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
           02 FILLER PIC X(5) VALUE "…⁄÷’±".
           02 FILLER PIC X(5) VALUE "ÕƒƒÕ±".
           02 FILLER PIC X(5) VALUE "ªø∑∏±".
           02 FILLER PIC X(5) VALUE "∫≥∫≥±".
           02 FILLER PIC X(5) VALUE "»¿”‘±".
           02 FILLER PIC X(5) VALUE "ºŸΩæ±".
       01 DVENT REDEFINES ASCYV.
           02 BLOQUESVENT OCCURS 6.
              03 DIBUJOV PIC X OCCURS 5.
       01 POSICIONADORES.
           02 SITIO  PIC 99.
           02 POSICION PIC 99.
           02 J PIC 99.
           02 I PIC 99.
           02 LINEA PIC 99.
       LINKAGE SECTION.
       01 VENTANA.
           02 ESQUINAS.
                 03 TOPL   PIC 99.
                 03 TOPP   PIC 99.
                 03 DOWNL  PIC 99.
                 03 DOWNP  PIC 99.
           02 HELPCUADRO.
                 03 BIT1 PIC 9.
                 03 TIPO PIC 9.
                 03 ANCHO  PIC 99.
                 03 ALTO   PIC 99.
                 03 SOMBRA PIC 99.
       01 BUFFER.
           02 LB OCCURS 25.
             03 ELEMB PIC X OCCURS 80.
       PROCEDURE DIVISION USING VENTANA , BUFFER.
       MAIN.
           IF TIPO = 0 MOVE 1 TO TIPO END-IF
           IF TOPL = 0 MOVE 1 TO TOPL END-IF
           IF TOPP = 0 MOVE 1 TO TOPP END-IF
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
                MOVE 0 TO ANCHO
                MOVE 0 TO ALTO
           END-IF
            PERFORM LINH VARYING POSICION FROM TOPP BY 1
                UNTIL POSICION = DOWNP.
            PERFORM LINV VARYING LINEA FROM TOPL BY 1
               UNTIL LINEA = DOWNL.
            MOVE DIBUJOV(1 , TIPO) TO ELEMB (TOPL , TOPP).
            MOVE DIBUJOV(3 , TIPO) TO ELEMB (TOPL , DOWNP).
            MOVE DIBUJOV(5 , TIPO) TO ELEMB (DOWNL , TOPP).
            MOVE DIBUJOV(6 , TIPO) TO ELEMB (DOWNL , DOWNP).
       LINV.
           MOVE DIBUJOV(4 , TIPO) TO ELEMB(LINEA , TOPP).
           MOVE DIBUJOV(4 , TIPO) TO ELEMB(LINEA , DOWNP).
       LINH.
           MOVE DIBUJOV(2 , TIPO) TO ELEMB(TOPL , POSICION).
           MOVE DIBUJOV(2 , TIPO) TO ELEMB(DOWNL , POSICION).