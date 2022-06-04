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
       01 FECHA.
           02 DIA PIC 9(2).
           02 MES PIC 9(2).
           02 ANO PIC 9(4).
       PROCEDURE DIVISION.
       INICIO.
           MOVE FALSO TO CORRECTO
           PERFORM PROG UNTIL CORRECTO = CIERTO
           STOP RUN.
       PROG.
           DISPLAY " " ERASE.
           DISPLAY "INTRODUCIR FECHA:" LINE 4 POSITION 10 HIGH
                    REVERSE BLINK.
           ACCEPT DIA LINE 4 POSITION 27 SIZE 2 UPDATE.
           DISPLAY "/" LINE 4 POSITION 29.
           ACCEPT MES LINE 4 POSITION 30 SIZE 2 UPDATE.
           DISPLAY "/" LINE 4 POSITION 32.
           ACCEPT ANO LINE 4 POSITION 33 SIZE 4 UPDATE.
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
           IF CORRECTO = CIERTO
                DISPLAY " " ERASE
                DISPLAY "­­­­BIEEEN!!!! LA FECHA ES CORRECTA"
                        LINE 12 POSITION 24
               ELSE DISPLAY "­LA FECHA ES INCORRECTA!" LINE 12 POSITION
                        30
               ACCEPT NADA OFF.
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









