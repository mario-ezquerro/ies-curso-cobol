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
       01 ASCYL.
           02 FILLER PIC X(5) VALUE "ËÂÒÑ±".
           02 FILLER PIC X(5) VALUE "ÊÁÐÏ±".
           02 FILLER PIC X(5) VALUE "º³º³±".
           02 FILLER PIC X(5) VALUE "ÍÄÄÍ±".
           02 FILLER PIC X(5) VALUE "ÎÅ×Ø±".
       01 DLINEAS REDEFINES ASCYL.
           02 BLOQUESLINEAS OCCURS 5.
              03 DIBUJOL PIC X OCCURS 5.
       01 POSICIONADORES.
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
                 03 BIT1   PIC 9.
                 03 TIPO   PIC 9.
                 03 ANCHO  PIC 99.
                 03 ALTO   PIC 99.
                 03 CRUCE  PIC 9.
                 03 SOMBRA PIC 9.
       01 LIN.
           02 SITIO PIC 99.
           02 REPETICION PIC 99.
       01 BUFFER.
           02 LB OCCURS 25.
             03 ELEMB PIC X OCCURS 80.
       PROCEDURE DIVISION USING VENTANA LIN BUFFER.
       MAIN.
           IF ANCHO = 0 MOVE 1 TO ANCHO END-IF.
           COMPUTE SITIO = SITIO + TOPP
           PERFORM LINV VARYING LINEA FROM TOPL BY 1
               UNTIL LINEA = DOWNL.
           MOVE DIBUJOL(1 , TIPO) TO ELEMB (TOPL , SITIO).
           MOVE DIBUJOL(2 , TIPO) TO ELEMB (DOWNL , SITIO).
           COMPUTE SITIO = SITIO + ANCHO.
           COMPUTE REPETICION = REPETICION - 1.
           EXIT PROGRAM.
       LINV.
           IF CRUCE NOT = 0
                IF ELEMB(LINEA , SITIO) = DIBUJOL(4 , TIPO)
                   MOVE DIBUJOL(5 , TIPO) TO ELEMB(LINEA , SITIO)
                  ELSE
                   MOVE DIBUJOL(3 , TIPO) TO ELEMB(LINEA , SITIO)
                END-IF
              ELSE
                MOVE DIBUJOL(3 , TIPO) TO ELEMB(LINEA , SITIO)
           END-IF.
