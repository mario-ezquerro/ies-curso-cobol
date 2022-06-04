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
           02 FILLER PIC X(5) VALUE "ÕƒƒÕ±".
       01 DVENT REDEFINES ASCYV.
           02 DIBUJOV PIC X OCCURS 5.
       01 ASCYL.
           02 FILLER PIC X(4) VALUE "Ã√«∆".
           02 FILLER PIC X(4) VALUE "π¥∂µ".
       01 DLINEA REDEFINES ASCYL.
           02 BLOQUESLIN OCCURS 2.
             03 DIBUJOL PIC X OCCURS 4.
       77 NADA PIC X.
       77 CONT PIC 99.
       77 POSICION PIC 99.
       77 J PIC 99.
       77 I PIC 99.
       77 LINEA PIC 99.
       LINKAGE SECTION.
       01 LIN.
           02 SITIO PIC 99.
           02 REPETICION PIC 99.
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
       PROCEDURE DIVISION USING VENTANA LIN.
       MAIN.
           IF ANCHO NOT = 0
               COMPUTE DOWNP = TOPP + ANCHO
               MOVE 0 TO ANCHO.
           COMPUTE SITIO = SITIO + TOPL
           IF ALTO = 0
              MOVE 2 TO ALTO.
           IF REPETICION = 0
             MOVE 1 TO REPETICION.
           PERFORM BARRAH VARYING CONT FROM 0 BY 1 UNTIL CONT =
              REPETICION.
           EXIT PROGRAM.

      *Esta rutina muestra en pantalla una linea en la fila SITIO
      *con el ancho de la ultima ventana dibujada y si no con el ancho
      *de la variable TOPP Y DOWNP sino tambiem podemos solamente dar el
      *ancho ANCHO de la ventana y la primera posicion TOPP.
       BARRAH.
           EVALUATE BIT1
                 WHEN 0 THRU 1
                   DISPLAY DIBUJOL(1, BIT2) LINE SITIO POSITION TOPP
                 WHEN 2 THRU 3
                   DISPLAY DIBUJOL(1, BIT2) LINE SITIO POSITION TOPP
                       REVERSE
                 WHEN 4 THRU 5
                   DISPLAY DIBUJOL(1, BIT2) LINE SITIO POSITION TOPP
                      BLINK
                 WHEN 6 THRU 7
                   DISPLAY DIBUJOL(1, BIT2) LINE SITIO POSITION  TOPP
                      BLINK REVERSE
           END-EVALUATE
           COMPUTE I = 1 + TOPP.
           PERFORM RALLA VARYING POSICION FROM I BY 1
           UNTIL POSICION = DOWNP.
           EVALUATE BIT1
                 WHEN 0 THRU 1
                   DISPLAY DIBUJOL(2, BIT2) LINE SITIO POSITION DOWNP
                 WHEN 2 THRU 3
                   DISPLAY DIBUJOL(2, BIT2) LINE SITIO POSITION DOWNP
                       REVERSE
                 WHEN 4 THRU 5
                   DISPLAY DIBUJOL(2, BIT2) LINE SITIO POSITION DOWNP
                      BLINK
                 WHEN 6 THRU 7
                   DISPLAY DIBUJOL(2, BIT2) LINE SITIO POSITION  DOWNP
                      BLINK REVERSE
           END-EVALUATE
           COMPUTE SITIO = SITIO + ALTO.
       RALLA.
           EVALUATE BIT1
                 WHEN 0 THRU 1
                    DISPLAY DIBUJOV(BIT2) LINE SITIO POSITION POSICION
                 WHEN 2 THRU 3
                    DISPLAY DIBUJOV(BIT2) LINE SITIO POSITION POSICION
                       REVERSE
                 WHEN 4 THRU 5
                    DISPLAY DIBUJOV(BIT2) LINE SITIO POSITION POSICION
                      BLINK
                 WHEN 6 THRU 7
                    DISPLAY DIBUJOV(BIT2) LINE SITIO POSITION POSICION
                      BLINK REVERSE
           END-EVALUATE.

