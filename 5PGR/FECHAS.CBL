       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENU.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. INVES-20Mb.
       OBJECT-COMPUTER. INVES-20Mb.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 CIERTO PIC 9 VALUE 1.
       77 COCIENTE PIC 9.
       77 RESTO PIC 9(8).
       77 CORRECTO PIC 9.
       77 FALSO PIC 9 VALUE 0.
       77 NADA PIC X.
       77 LINEA PIC 99.
       77 POSICION PIC 99.
       LINKAGE SECTION.
       01 VENT.
           02 CUADRO.
              03 TOPL PIC 99.
              03 TOPP PIC 99.
              03 DOWNL PIC 99.
              03 DOWNP PIC 99.
           02 HELP PIC 9(8).
       01 FECHA.
           02 DIA PIC 9(2).
           02 MES PIC 9(2).
           02 ANO PIC 9(4).
       PROCEDURE DIVISION USING FECHA VENT.
       INICIO.
           MOVE FALSO TO CORRECTO
           PERFORM PROG UNTIL CORRECTO = CIERTO
           EXIT PROGRAM.
       PROG.
           COMPUTE DOWNL = TOPL + 2
           COMPUTE DOWNP = TOPP + 16
           MOVE 02000000 TO HELP
           CALL "VENTANA.COB" USING CUADRO HELP
           MOVE TOPL TO LINEA
           COMPUTE POSICION = TOPP + 1
           DISPLAY "FECHA:" LINE LINEA POSITION POSICION
           COMPUTE LINEA = TOPL + 1
           ADD 5 TO POSICION
           DISPLAY "/" LINE LINEA POSITION POSICION
           ADD 3 TO POSICION
           DISPLAY "/" LINE LINEA POSITION POSICION
           MOVE TOPP TO POSICION
           ADD 4 TO POSICION
           ACCEPT DIA LINE LINEA POSITION POSICION  SIZE 2 UPDATE.
           ADD 3 TO POSICION
           ACCEPT MES LINE LINEA POSITION POSICION SIZE 2 UPDATE.
           ADD 3 TO POSICION
           ACCEPT ANO LINE LINEA POSITION POSICION SIZE 4 UPDATE.
           IF DIA > 0 AND DIA NOT > 31 AND ANO > 1900
                EVALUATE MES
                    WHEN 1 PERFORM MESES31
                    WHEN 3 PERFORM MESES31
                    WHEN 5 PERFORM MESES31
                    WHEN 7 THRU 8 PERFORM MESES31
                    WHEN 10 PERFORM MESES31
                    WHEN 12 PERFORM MESES31
                    WHEN 4 PERFORM MESES30
                    WHEN 6 PERFORM MESES30
                    WHEN 9 PERFORM MESES30
                    WHEN 11 PERFORM MESES30
                    WHEN 2 PERFORM MESFEB
                    WHEN OTHER MOVE FALSO TO CORRECTO
                END-EVALUATE
               ELSE MOVE FALSO TO CORRECTO.
       MESES31.
           IF DIA NOT > 31
                MOVE CIERTO TO CORRECTO
               ELSE MOVE FALSO TO CORRECTO.
       MESES30.
           IF DIA < 31
                MOVE CIERTO TO CORRECTO
               ELSE MOVE FALSO TO CORRECTO.
       MESFEB.
           DIVIDE ANO BY 4 GIVING COCIENTE REMAINDER RESTO
           IF RESTO = 0
                IF DIA NOT > 29
                    MOVE CIERTO TO CORRECTO
                   ELSE MOVE FALSO TO CORRECTO
                END-IF
              ELSE
                IF DIA < 29
                    MOVE CIERTO TO CORRECTO
                   ELSE MOVE FALSO TO CORRECTO
                END-IF.









