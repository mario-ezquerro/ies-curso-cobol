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
       77 BLANCOS PIC X(76) VALUE ALL " ".
       LINKAGE SECTION.
       01 FECHA.
           02 DIA PIC 99.
           02 MES PIC 99.
       PROCEDURE DIVISION USING FECHA.
       INICIO.
           MOVE FALSO TO CORRECTO
           PERFORM PROG UNTIL CORRECTO = CIERTO
           EXIT PROGRAM.
       PROG.
           DISPLAY BLANCOS LINE 24 POSITION 3 REVERSE
           ACCEPT DIA LINE 10 POSITION 21 SIZE 2 UPDATE.
           ACCEPT MES LINE 10 POSITION 24 SIZE 2 UPDATE.
           IF DIA > 0 AND DIA NOT > 31
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
               ELSE MOVE FALSO TO CORRECTO
           END-IF
           IF CORRECTO = FALSO
               DISPLAY "FECHA ERRONEA. PULSE UNA TECLA "
                LINE 24 POSITION 10 REVERSE
               DISPLAY "Y DEME OTRA FECHA" LINE 24 POSITION 41 REVERSE
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
           DIVIDE 92 BY 4 GIVING COCIENTE REMAINDER RESTO
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