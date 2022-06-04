       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTA-PLA.
       AUTHOR. CHICOTE-MARIO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PHILIPS.
       OBJECT-COMPUTER. PHILIPS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TeclasSistema.
           02 TECLA PIC X.
           02 TECLAF PIC 99 COMP.
       01 LIN.
           02 SITIO PIC 99.
           02 REPETICION PIC 99.
       01 CONTADORES.
           02 I PIC 99.
           02 J PIC 99.
           02 K PIC 99.
           02 LINEA PIC 99.
           02 POSICION PIC 99.
           02 VALOR    PIC 9999.
       01 Registros.
          02 VENTANA.
               03 TOPL    PIC 99.
               03 TOPP    PIC 99.
               03 DOWNL   PIC 99.
               03 DOWNP   PIC 99.
               03 ANCHO   PIC 99.
               03 ALTO    PIC 99.
               03 TIPO    PIC 9.
               03 SOMBRA  PIC 9.
       01 Colores.
           02 ColorA PIC X(27) VALUE "FCOLOR=WHITE, BCOLOR=BLUE".
           02 Color1 PIC X(27) VALUE "FCOLOR=WHITE, BCOLOR=BLUE".
           02 ColorN PIC X(27) VALUE "FCOLOR=WHITE, BCOLOR=BLAK".
           02 ColorV PIC X(27) VALUE "FCOLOR=WHITE, BCOLOR=BLUE".
       PROCEDURE DIVISION.
       MAIN.
           MOVE 08210000390421 TO VENTANA
           CALL "VENTANA.COB" USING VENTANA
           COMPUTE LINEA = TOPL
           COMPUTE POSICION = TOPP + 2
           DISPLAY "     PROGRAMA REALIZADO POR:        "
           LINE LINEA POSITION POSICION REVERSE CONTROL  COLOR1
           ADD 1 TO LINEA
           DISPLAY "                                    "
           LINE LINEA POSITION POSICION REVERSE CONTROL  COLOR1
           ADD 1 TO LINEA
           DISPLAY "          CHICOTE & MARIO           "
           LINE LINEA POSITION POSICION REVERSE CONTROL  COLOR1
           ADD 1 TO LINEA
           DISPLAY "  PARA  MAS INFORMACION  BUSQUENOS  "
           LINE LINEA POSITION POSICION REVERSE CONTROL  COLOR1
           ADD 1 TO LINEA
           DISPLAY "      EN LAS PAGINAS AMARILLAS      "
           LINE LINEA POSITION POSICION REVERSE CONTROL  COLOR1
           ACCEPT TECLA LINE 25 POSITION 1 NO BEEP ON
           EXCEPTION TECLAF CONTINUE END-ACCEPT
           DISPLAY "À" LINE 25 POSITION 1
           CONTROL COLORA
           CALL "RECUP.EXE"
           EXIT PROGRAM.