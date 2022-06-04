       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJER1.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM400.
       OBJECT-COMPUTER. IBM400.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT COMPRAS ASSIGN TO DISK "COMPRAS.DAT".
       DATA DIVISION.
       FILE SECTION.
       FD COMPRAS LABEL RECORD STANDARD.
       01 REG-COMP.
           02 CODPROV PIC X(4).
           02 FFECHA.
              03 DIA PIC 99.
              03 MES PIC 99.
           02 FTOTALC PIC 9(5).
       WORKING-STORAGE SECTION.
       77 PROVE PIC X(4).
       77 NADA PIC X.
       77 VENT PIC 9(16).
       77 TECLA PIC 99 COMP.
       77 FIN PIC X.
       77 I PIC 99.
       77 VAR PIC 99.
       77 TOTAL PIC 9(10).
       77 EXIS PIC 9.
       01 TABLA.
           02 TMESES PIC 9(8) OCCURS 12.
       01 DATOS.
           02 FILLER PIC X(20) VALUE "ENERO.....:         ".
           02 FILLER PIC X(20) VALUE "FEBRERO...:         ".
           02 FILLER PIC X(20) VALUE "MARZO.....:         ".
           02 FILLER PIC X(20) VALUE "ABRIL.....:         ".
           02 FILLER PIC X(20) VALUE "MAYO......:         ".
           02 FILLER PIC X(20) VALUE "JUNIO.....:         ".
           02 FILLER PIC X(20) VALUE "JULIO.....:         ".
           02 FILLER PIC X(20) VALUE "AGOSTO....:         ".
           02 FILLER PIC X(20) VALUE "SEPTIEMBRE:         ".
           02 FILLER PIC X(20) VALUE "OCTUBRE...:         ".
           02 FILLER PIC X(20) VALUE "NOVIEMBRE.:         ".
           02 FILLER PIC X(20) VALUE "DICIEMBRE.:         ".
       01 TAB-MES REDEFINES DATOS.
             02 NMES PIC  X(20) OCCURS 12.
       77 LCDMESES PIC ZZ.ZZZ.ZZ9.
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY " " ERASE
           MOVE 0000248011000000 TO VENT
           CALL "VENTANA.COB" USING VENT
           PERFORM CREAC UNTIL TECLA = 27
           STOP RUN.
       CREAC.
           DISPLAY "NUM. PROVEEDOR:       " LINE 2 POSITION 50
           MOVE 5 TO VAR
           PERFORM DMESES VARYING I FROM 1 BY 1 UNTIL I > 12.
           DISPLAY "TOTAL.....:             " LINE 19 POSITION 5
           ACCEPT PROVE LINE 2 POSITION 73 TAB ON EXCEPTION TECLA
              CONTINUE
           END-ACCEPT.
           IF TECLA NOT = 27
               PERFORM MOSTRAR
           END-IF.
       MOSTRAR.
           PERFORM LIMPIAR VARYING I FROM 1 BY 1 UNTIL I > 12.
           MOVE 0 TO TOTAL
           MOVE "N" TO FIN
           OPEN INPUT COMPRAS.
           READ COMPRAS AT END MOVE "S" TO FIN
           END-READ.
           MOVE 0 TO EXIS
           PERFORM LECTURA UNTIL FIN = "S"
           CLOSE COMPRAS
           MOVE 5 TO VAR
           IF EXIS NOT = 0
              PERFORM VISUAL VARYING I FROM 1 BY 1 UNTIL I > 12
              DISPLAY TOTAL LINE 19 POSITION 17
              DISPLAY "TECLE ALGO PARA CONTINUAR" LINE 23
                  POSITION 5
              ELSE DISPLAY "EL PROVEEDOR NO EXISTE" LINE 23 POSITION 5
           END-IF.
           ACCEPT NADA OFF LINE 23 POSITION 45
           DISPLAY "                              " LINE 23 POSITION 5.
       LIMPIAR.
           MOVE  0 TO TMESES(I).
       LECTURA.
           IF PROVE = CODPROV
                 COMPUTE TMESES(MES) = TMESES(MES) + FTOTALC
                 COMPUTE TOTAL = TOTAL + FTOTALC
                 ADD 1 TO EXIS
           END-IF.
           READ COMPRAS AT END MOVE "S" TO FIN
           END-READ.
       VISUAL.
           MOVE TMESES(I) TO LCDMESES
           DISPLAY LCDMESES LINE VAR POSITION 17
           COMPUTE VAR = VAR + 1.
       DMESES.
           DISPLAY  NMES(I) LINE VAR POSITION 5.
           ADD 1 TO VAR.




