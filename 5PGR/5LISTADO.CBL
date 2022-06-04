       IDENTIFICATION DIVISION.
       PROGRAM-ID. AGENDA.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PHILIPS.
       OBJECT-COMPUTER. PHILIPS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL ALUMNO ASSIGN TO RANDOM "ALUMNOS.DAT"
           ORGANIZATION IS RELATIVE
           ACCESS MODE IS DYNAMIC
           RELATIVE KEY IS NUM-AL.
           SELECT LISTADO ASSIGN TO PRINT "PRINTER".
       DATA DIVISION.
       FILE SECTION.
       FD ALUMNO LABEL RECORD STANDARD.
       01 REG-ALUM.
           02 NOM-AL PIC X(10).
           02 APELLIDO-AL PIC X(15).
           02 SEXO-AL PIC X.
               88 VARON VALUE "V".
               88 HEMBRA VALUE "H".
           02 DATOS-CURSO.
               03 CURSO PIC 9.
               03 GRUPO PIC X.
           02 NOTAS PIC 99 OCCURS 5.
       FD LISTADO LABEL RECORD OMITTED.
       01 LINEA PIC X(80).
       WORKING-STORAGE SECTION.
       77 NUM-AL PIC 9(3).
       77 SEXO PIC X.
       77 CONTLIN PIC 99 VALUE 0.
       77 CONTPAG PIC 99 VALUE 1.
       01 VENT.
           02 CUADRO PIC 9(8).
           02 HELP PIC 9(8).
       01 FECHA.
           02 DIA PIC 9(2).
           02 MES PIC 9(2).
           02 ANO PIC 9(4).
       01 LIN1.
           02 FILLER PIC X(7) VALUE SPACES.
           02 FILLER PIC X(15) VALUE "LISTADO ALUMNOS".
           02 FILLER PIC X(18) VALUE SPACES.
           02 FILLER PIC X(6) VALUE "SEXO :".
           02 LSEXO PIC X(6).
       01 LIN2.
           02 FILLER PIC X(7) VALUE SPACES.
           02 FILLER PIC X(15) VALUE "อออออออออออออออ".
       01 LIN3.
           02 FILLER PIC X(7) VALUE SPACES.
           02 FILLER PIC X(7) VALUE "FECHA :".
           02 LDIA PIC 99.
           02 FILLER PIC X VALUE "/".
           02 LMES PIC 99.
           02 FILLER PIC X VALUE "/".
           02 LANO PIC Z9.
           02 FILLER PIC X(18) VALUE SPACES.
           02 PAGINA PIC X(8) VALUE "PAGINA :".
           02 LPAG PIC Z9.
       01  LIN4.
           02 FILLER PIC X(7) VALUE SPACES.
           02 FILLER PIC X(6) VALUE "NUMERO".
           02 FILLER PIC X(5) VALUE SPACES.
           02 FILLER PIC X(6) VALUE "NOMBRE".
           02 FILLER PIC X(10) VALUE SPACES.
           02 FILLER PIC X(9) VALUE "APELLIDOS".
           02 FILLER PIC X(15) VALUE SPACES.
           02 FILLER PIC X(5) VALUE "CURSO".
           02 FILLER PIC X(3) VALUE SPACES.
           02 FILLER PIC X(5) VALUE "GRUPO".
       01 LIN5.
           02 FILLER PIC X(7) VALUE SPACES.
           02 FILLER PIC X(32) VALUE "ออออออออออออออออออออออออออออออออ".
           02 FILLER PIC X(32) VALUE "ออออออออออออออออออออออออออออออออ".
       01 LIN-DET.
           02 FILLER PIC X(10) VALUE SPACES.
           02 L-NUMAL PIC ZZ9.
           02 FILLER PIC X(5) VALUE SPACES.
           02 L-NOMAL PIC X(10).
           02 FILLER PIC X(6) VALUE SPACES.
           02 L-APEL PIC X(15).
           02 FILLER PIC X(13) VALUE SPACES.
           02 L-CURSO PIC 9.
           02 FILLER PIC X(7) VALUE SPACES.
           02 L-GRUPO PIC X.
       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT ALUMNO.
           OPEN OUTPUT LISTADO.
           PERFORM DATOS.
           PERFORM CABECERAS.
           PERFORM LEER VARYING NUM-AL FROM 1 BY 1 UNTIL NUM-AL = 550.           PERFORM
           CLOSE ALUMNO.
           EXIT PROGRAM.
       DATOS.
           MOVE 08021577 TO CUADRO
           MOVE 00000001 TO HELP
           CALL "VENTANA.COB" USING CUADRO , HELP
           DISPLAY "LISTADO ALUMNOS" LINE 11 POSITION 6
           DISPLAY "SEXO:" LINE 11 POSITION 40
           PERFORM 1VALIDA WITH TEST AFTER UNTIL SEXO = 'V' OR 'H'
           MOVE 10580000 TO CUADRO
           MOVE 0 TO HELP
           CALL "FECHAS.COB" USING FECHA VENT.
       1VALIDA.
           ACCEPT SEXO LINE 11 POSITION 47.
       CABECERAS.
           IF SEXO = 'V' MOVE "VARON" TO LSEXO
              ELSE MOVE "HEMBRA" TO LSEXO.
           MOVE DIA TO LDIA
           MOVE MES TO LMES
           MOVE ANO TO LANO
           MOVE CONTPAG TO LPAG
           WRITE LINEA FROM LIN1 BEFORE ADVANCING 1
           WRITE LINEA FROM LIN2 BEFORE ADVANCING 2
           WRITE LINEA FROM LIN3 BEFORE ADVANCING 2
           WRITE LINEA FROM LIN4 BEFORE ADVANCING 1
           WRITE LINEA FROM LIN5 BEFORE ADVANCING 2.
       LEER.
           READ ALUMNO INVALID KEY CONTINUE
             NOT INVALID KEY PERFORM IMPRIMIR.
       IMPRIMIR.
           IF SEXO-AL = SEXO
             MOVE NUM-AL TO L-NUMAL
             MOVE NOM-AL TO L-NOMAL
             MOVE APELLIDO-AL TO L-APEL
             MOVE CURSO TO L-CURSO
             MOVE GRUPO TO L-GRUPO
             WRITE LINEA FROM LIN-DET BEFORE ADVANCING 1
             ADD 1 TO CONTLIN
           END-IF
           IF CONTLIN = 4
             MOVE 0 TO CONTLIN
             ADD 1 TO CONTPAG
             MOVE SPACES TO LINEA
             WRITE LINEA BEFORE PAGE
             PERFORM CABECERAS.


