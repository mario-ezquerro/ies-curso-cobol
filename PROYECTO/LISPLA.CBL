       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTA-PLA.
       AUTHOR. CHICOTE-MARIO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PHILIPS.
       OBJECT-COMPUTER. PHILIPS.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PLATOS ASSIGN TO DISK "PLATOS.DAT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY IS NUMPLATO
           ALTERNATE RECORD KEY IS CODIGO WITH DUPLICATES.
           SELECT LISTADO ASSIGN TO PRINT "PRINTER"
           FILE STATUS IS ERRORIMP.
       DATA DIVISION.
       FILE SECTION.
       FD PLATOS LABEL RECORD STANDARD.
       01 REG-PLA.
           02 NUMPLATO PIC 99.
           02 CODIGO PIC XX.
           02 NOM PIC X(26).
           02 PVP PIC 9(4).
       FD LISTADO LABEL RECORD OMITTED.
       01 LIN PIC X(80).
       WORKING-STORAGE SECTION.
       77 CONTLIN PIC 99.
       77 WCOD PIC X(2).
       77 VENTANA PIC 9(14).
       77 FIN PIC X VALUE "N".
       77 SW PIC 9 VALUE 1.
       77 VALIDO PIC X VALUE "N".
       77 TECLA PIC X.
       77 TECLAF PIC 99 COMP.
       77 ERRORIMP PIC XX.
       77 COL1 PIC X(26) VALUE "FCOLOR=WHITE, BCOLOR=BLACK".
       77 COL2 PIC X(26) VALUE "FCOLOR=BLACK, BCOLOR=WHITE".
       01 LIN1.
           02 PIC X(36) VALUE "ีอออออออออัออออออออัออออออออออออออออ".
           02 PIC X(23) VALUE "อออออออออออออัอออออออธ".
       01 LIN2.
           02 PIC X(36) VALUE "ณ N.PLATO ณ CODIGO ณ           PLATO".
           02 PIC X(23) VALUE "             ณ   PVP ณ".
       01 LIN3.
           02 PIC X(36) VALUE "ฦอออออออออุออออออออุออออออออออออออออ".
           02 PIC X(23) VALUE "อออออออออออออุอออออออต".
       01 LINDET.
           02 PIC X(4) VALUE "ณ   ".
           02 L-NUM PIC Z9.
           02 PIC X(8) VALUE "    ณ   ".
           02 L-COD PIC X(2).
           02 PIC X(5) VALUE "   ณ ".
           02 L-NOM PIC X(26).
           02 PIC X(4) VALUE "  ณ ".
           02 L-PVP PIC Z.ZZ9.
           02 PIC X(2) VALUE " ณ".
       01 LIN4.
           02 PIC X(36) VALUE "ิอออออออออฯออออออออฯออออออออออออออออ".
           02 PIC X(23) VALUE "อออออออออออออฯอออออออพ".
       77 I PIC 99.
       PROCEDURE DIVISION.
       DECLARATIVES.
       UNO SECTION.
           USE AFTER ERROR PROCEDURE ON LISTADO.
       ERROR-IMPRESORA.
           IF ERRORIMP NOT = "00"
              MOVE 2 TO I
              CALL "MENSAJE.COB" USING I
           END-IF.
       END DECLARATIVES.
       DOS SECTION.
       MAIN.
           OPEN INPUT PLATOS
           OPEN OUTPUT LISTADO
           MOVE "00" TO ERRORIMP
           WRITE LIN FROM SPACES
           if errorimp = "00"
               PERFORM PRESENTA
           end-if
           CLOSE PLATOS LISTADO
           CALL "RECUP.EXE"
           EXIT PROGRAM.
       PRESENTA.
           MOVE 10211662000001 TO VENTANA
           CALL "VENTANA.COB" USING VENTANA
           DISPLAY "ฺฤฤฤฤฤฤฤฤฤฤฟ" LINE 12 POSITION 26 CONTROL COL1
           DISPLAY "ณPOR CODIGOณ" LINE 13 POSITION 26 CONTROL COL1
           DISPLAY "ภฤฤฤฤฤฤฤฤฤฤู" LINE 14 POSITION 26 CONTROL COL1
           DISPLAY "ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ" LINE 12 POSITION 42 CONTROL COL2
           DISPLAY "ณCARTA COMPLETAณ" LINE 13 POSITION 42 CONTROL COL2
           DISPLAY "ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤู" LINE 14 POSITION 42 CONTROL COL2
           CALL "CUROFF.EXE"
           PERFORM ACEPTAR UNTIL TECLAF = 13
           CALL "CURON.EXE"
           IF SW = 1
               DISPLAY "CODIGO:" LINE 16 POSITION 27 CONTROL COL2
               PERFORM ENTRACOD UNTIL VALIDO = "S"
               PERFORM LECTURA UNTIL FIN = "S"
                ELSE MOVE 0 TO NUMPLATO
                    START PLATOS KEY IS > NUMPLATO
                      INVALID KEY CONTINUE
                         NOT INVALID KEY
                           MOVE 1 TO CONTLIN
                           PERFORM LECTURA UNTIL FIN = "S"
                    END-START
           END-IF.
       LECTURA.
           IF SW = 2
                READ PLATOS NEXT AT END MOVE "S" TO FIN
                END-READ
                MOVE NUMPLATO TO L-NUM
                MOVE CODIGO TO L-COD
                MOVE NOM TO L-NOM
                MOVE PVP TO L-PVP
           END-IF.
           IF SW = 1
              READ PLATOS NEXT AT END MOVE "S" TO FIN
                END-READ
              IF WCOD = CODIGO
                MOVE NUMPLATO TO L-NUM
                MOVE CODIGO TO L-COD
                MOVE NOM TO L-NOM
                MOVE PVP TO L-PVP
                  ELSE MOVE "S" TO FIN
              END-IF
           END-IF.
           IF CONTLIN = 1 AND FIN = "N"
              WRITE LIN FROM LIN1 BEFORE 1
              WRITE LIN FROM LIN2 BEFORE 1
              WRITE LIN FROM LIN3 BEFORE 1
           END-IF.
           IF FIN = "N"
              WRITE LIN FROM LINDET BEFORE 1
              ADD 1 TO CONTLIN
           END-IF.
           IF CONTLIN = 50 OR FIN = "S"
              WRITE LIN FROM LIN4 BEFORE PAGE
              MOVE 1 TO CONTLIN
           END-IF.
       TECLASFUNCION.
           IF TECLAF = 50
             MOVE 1 TO SW
             DISPLAY "ฺฤฤฤฤฤฤฤฤฤฤฟ" LINE 12 POSITION 26 CONTROL COL1
             DISPLAY "ณPOR CODIGOณ" LINE 13 POSITION 26 CONTROL COL1
             DISPLAY "ภฤฤฤฤฤฤฤฤฤฤู" LINE 14 POSITION 26 CONTROL COL1
             DISPLAY "ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ" LINE 12 POSITION 42 CONTROL COL2
             DISPLAY "ณCARTA COMPLETAณ" LINE 13 POSITION 42 CONTROL COL2
             DISPLAY "ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤู" LINE 14 POSITION 42 CONTROL COL2
           END-IF.
           IF TECLAF = 51
             MOVE 2 TO SW
             DISPLAY "ฺฤฤฤฤฤฤฤฤฤฤฟ" LINE 12 POSITION 26 CONTROL COL2
             DISPLAY "ณPOR CODIGOณ" LINE 13 POSITION 26 CONTROL COL2
             DISPLAY "ภฤฤฤฤฤฤฤฤฤฤู" LINE 14 POSITION 26 CONTROL COL2
             DISPLAY "ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ" LINE 12 POSITION 42 CONTROL COL1
             DISPLAY "ณCARTA COMPLETAณ" LINE 13 POSITION 42 CONTROL COL1
             DISPLAY "ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤู" LINE 14 POSITION 42 CONTROL COL1
           END-IF.
       ACEPTAR.
           ACCEPT TECLA LINE 24 POSITION 80 NO BEEP OFF
              ON EXCEPTION TECLAF PERFORM TECLASFUNCION
           END-ACCEPT.
       ENTRACOD.
           ACCEPT WCOD NO BEEP LINE 16 POSITION 34
           MOVE WCOD TO CODIGO
           START PLATOS KEY IS = CODIGO
              INVALID KEY
               DISPLAY "Introduzca otro codigo"
                   LINE 16 POSITION 38 CONTROL COL2 BLINK
                 MOVE "N" TO VALIDO
               NOT INVALID KEY
                 MOVE 1 TO CONTLIN
                 MOVE "S" TO VALIDO
           END-START.



