

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLIENTES.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM400.
       OBJECT-COMPUTER. IBM400.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MAESTRO ASSIGN TO DISK "MAESTRO.DAT".
           SELECT MOVI ASSIGN TO DISK "MOVI.DAT".
           SELECT ACTUAL ASSIGN TO DISK "ACTUAL.DAT".
       DATA DIVISION.
       FILE SECTION.
       FD MAESTRO LABEL RECORD STANDARD.
       01 REGMAE.
           02 NUMAE PIC X(10).
           02 SALDO PIC 9(9).
       FD MOVI LABEL RECORD STANDARD.
       01 REGMOV.
           02 NUMOV PIC X(10).
           02 IMPORTE PIC 9(9).
           02 OPERAC PIC X.
       FD ACTUAL LABEL RECORD STANDARD.
       01 REGACT.
           02 NUMACT PIC X(10).
           02 SALDOACT PIC 9(11).
       WORKING-STORAGE SECTION.
       01 VENT.
           02 CUADRADO PIC 9(8).
           02 HELP PIC 9(8).
       01 LIN.
           02 SITIO PIC 99.
           02 REPETICION PIC 99.
       PROCEDURE DIVISION.
       MAIN.
           PERFORM INI-CLIENT.
           PERFORM TRATACUENTA UNTIL NUMAE = HIGH-VALUES AND
                 NUMOV = HIGH-VALUES.
           PERFORM FIN-CLIENT.
           STOP RUN.
       TRATACUENTA.
           IF NUMAE > NUMOV
                 PERFORM TRAT-ALT
               ELSE
                 PERFORM MODIFICACIONES
           END-IF.
       MODIFICACIONES.
           IF NUMAE < NUMOV
                PERFORM ANTIGUO
               ELSE
                PERFORM OPERAR
           END-IF.
           READ MAESTRO AT END MOVE HIGH-VALUES TO NUMAE.
       INI-CLIENT.
           OPEN INPUT MAESTRO MOVI
           OPEN OUTPUT ACTUAL
           READ MAESTRO AT END MOVE HIGH-VALUES TO NUMAE
           READ MOVI AT END MOVE HIGH-VALUES TO NUMOV
           END-READ.
       TRAT-ALT.
           IF OPERAC = 'I'
                MOVE NUMOV TO NUMACT
                MOVE IMPORTE TO SALDOACT
                WRITE REGACT
               ELSE
                DISPLAY "ERROR"
           END-IF.
           READ MOVI AT END MOVE HIGH-VALUES TO NUMOV.
       ANTIGUO.
           WRITE REGACT FROM REGMAE.
       OPERAR.
           IF OPERAC = 'I'
                COMPUTE SALDO = SALDO + IMPORTE
               ELSE COMPUTE SALDO = SALDO - IMPORTE
           END-IF.
           MOVE NUMAE TO NUMACT
           MOVE SALDO TO SALDOACT
           WRITE REGACT
           READ MOVI AT END MOVE HIGH-VALUES TO NUMOV.
       FIN-CLIENT.
           CLOSE MAESTRO MOVI ACTUAL.



