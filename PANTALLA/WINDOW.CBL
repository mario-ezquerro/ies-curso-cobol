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
           02 FILLER PIC X(5) VALUE "���ձ".
           02 FILLER PIC X(5) VALUE "���ͱ".
           02 FILLER PIC X(5) VALUE "�����".
           02 FILLER PIC X(5) VALUE "�����".
           02 FILLER PIC X(5) VALUE "���Ա".
           02 FILLER PIC X(5) VALUE "�ٽ��".
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
       01 CORRIDA.
           02 ELEMCORRIDA PIC X OCCURS 80.
       01 PANTALLA.
           02 LP OCCURS 25.
              03 PIXEL PIC X OCCURS 80.
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
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY " " ERASE
           MOVE "       PAAATAAATAL�SIHDFA NCKCVN" TO LP(7)
           MOVE "       ASRIYUIASCIWETNF AWV4TNIOPENWVTNT" TO LP(10)
           MOVE "       RUEHTWWWWWWWWWWWW" TO LP(12)
           PERFORM MOSTRAR.
           MOVE 02121725 TO ESQUINAS
           MOVE 01000000 TO HELPCUADRO
           PERFORM VENTANA
           PERFORM SACAR
           IF BIT2 = 0
              MOVE 1 TO BIT2.
           IF TOPL = 0
              MOVE 1 TO TOPL.
           IF TOPP = 0
              MOVE 1 TO TOPP.
           ACCEPT NADA OFF.
           STOP RUN.
       SACAR.
           PERFORM LPLINEA VARYING LINEA FROM TOPL BY 1
              UNTIL LINEA > DOWNL.
       LPLINEA.
           DISPLAY LP(LINEA) LINE LINEA POSITION 1 SIZE DOWNP.
       MOSTRAR.
           PERFORM MODULOD VARYING I FROM 1 BY 1 UNTIL I > 25.
       MODULOD.
           DISPLAY LP(I) LINE I POSITION  1.
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
            MOVE DIBUJOV(5 , BIT2) TO ELEMCORRIDA (1)
            MOVE DIBUJOV(6 , BIT2) TO ELEMCORRIDA (ANCHO)
            DISPLAY BARRA LINE TOPL POSITION TOPP SIZE ANCHO
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
            IF SOMBRA = 1
              COMPUTE LINEA = DOWNL + 1
              COMPUTE POSICION = TOPP + 1
              COMPUTE J = DOWNP + 1
              DISPLAY DIBUJOV(1 , 5) LINE DOWNL POSITION J
              MOVE ALL "�" TO BARRA
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
            IF SOMBRA = 1
              COMPUTE LINEA = DOWNL + 1
              COMPUTE POSICION = TOPP + 1
              COMPUTE J = DOWNP + 1
              DISPLAY DIBUJOV(1 , 5) LINE DOWNL POSITION J
              MOVE ALL "�" TO BARRA
              DISPLAY  BARRA LINE LINEA POSITION POSICION REVERSE
              SIZE ANCHO
            END-IF
            MOVE 0 TO ANCHO
            MOVE 0 TO ALTO
           END-IF.
       LINHORZR.
           DISPLAY BARRA LINE LINEA POSITION TOPP SIZE ANCHO REVERSE.
       LINHORZ.
           DISPLAY CORRIDA LINE LINEA POSITION TOPP SIZE ANCHO
           DISPLAY BARRA LINE LINEA POSITION TOPP SIZE ANCHO.
       GUARRADA.
           EVALUATE BIT2
              WHEN 1
                MOVE ALL "�" TO BARRA
                MOVE ALL "�" TO CORRIDA
              WHEN 2
                MOVE ALL "�" TO BARRA
              WHEN 3
                MOVE ALL "�" TO BARRA
              WHEN 4
                MOVE ALL "�" TO BARRA
              WHEN 5
                MOVE ALL "�" TO BARRA
              WHEN OTHER
                MOVE ALL "�" TO BARRA
           END-EVALUATE.