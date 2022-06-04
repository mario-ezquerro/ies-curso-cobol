       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTA-PLA.
       AUTHOR. CHICOTE-MARIO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PHILIPS.
       OBJECT-COMPUTER. PHILIPS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL PLATOS ASSIGN TO DISK NOMBRE
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS NUMPLATO
           ALTERNATE RECORD KEY IS CODIGO WITH DUPLICATES.
           SELECT OPTIONAL FACTURA ASSIGN TO DISK NOMBRE
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS NFAC
           ALTERNATE RECORD KEY IS FFECHA WITH DUPLICATES
           ALTERNATE RECORD KEY IS FGIF WITH DUPLICATES
           ALTERNATE RECORD KEY IS FPAGADA WITH DUPLICATES.
           SELECT OPTIONAL CLIENTES ASSIGN TO DISK NOMBRE
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS CGIF
           ALTERNATE RECORD KEY IS CNOMBRE WITH DUPLICATES
           ALTERNATE RECORD KEY IS CCIUDAD WITH DUPLICATES.
       DATA DIVISION.
       FILE SECTION.
       FD PLATOS LABEL RECORD STANDARD.
       01 REG-PLA.
           02 NUMPLATO PIC 99.
           02 CODIGO PIC XX.
           02 NOM PIC X(26).
           02 PVP PIC 9(4).
       FD FACTURA LABEL RECORD STANDARD.
       01 REG-FAC.
           02 NFAC   PIC 9(8).
           02 FGIF   PIC 9(8).
           02 FFECHA.
              03 FDIA PIC 99.
              03 FMES PIC 99.
              03 FANO PIC 9999.
           02 FPASTA  PIC 9(8).
           02 FPAGADA PIC X.
           02 FMESA   PIC 99.
       FD CLIENTES LABEL RECORD STANDARD.
       01 REG-CLI.
           02 CGIF       PIC X(9).
           02 CNOMBRE    PIC X(29).
           02 CDIRECCION PIC X(40).
           02 CNUMERO    PIC X(3).
           02 CCIUDAD    PIC X(15).
           02 CCP        PIC X(6).
           02 CPROVIN    PIC X(15).
           02 CTELEFONO  PIC X(9).
       WORKING-STORAGE SECTION.
       01 BUFFERS.
           02 BUFFER-CLI PIC X(126) OCCURS 100.
           02 BUFFER-FAC PIC X(35)  OCCURS 100.
           02 BUFFER-PLA PIC X(34)  OCCURS 100.
       01 DatosFicheros.
           02 FILLER PIC X(15) VALUE "PLATOS.DAT".
           02 FILLER PIC X(15) VALUE "A:\PLATO.DAT".
           02 FILLER PIC X(15) VALUE "FACTURAS.DAT".
           02 FILLER PIC X(15) VALUE "A:\FACTURAS.DAT".
           02 FILLER PIC X(15) VALUE "CLIENTES.DAT".
           02 FILLER PIC X(15) VALUE "A:\CLIENTES.DAT".
       01 TablaFicheros REDEFINES DatosFicheros.
           02 FICHERO PIC X(15) OCCURS 6.
       77 NOMBRE PIC X(15).
       01 VarConstantes.
           02 MasterPos PIC 99 VALUE 18.
           02 MaxPorciento PIC 9(9) VALUE 99.
           02 VENTANA1  PIC 9(14) VALUE 12251562000021.
           02 VENTANA2  PIC 9(16) VALUE 2200258004000000.
           02 VENTANA3  PIC 9(14) VALUE 08171465000021.
           02 VENTANA4  PIC 9(14) VALUE 08211665000021.
           02 VENTANA5  PIC 9(14) VALUE 11251542000021.
           02 VENTANA6  PIC 9(14) VALUE 12211360000021.
       01 TeclasSistema.
           02 TECLA PIC X.
           02 TECLAF PIC 99 COMP.
       01 FLAGS.
           02 BIEN   PIC X.
           02 ESTADO PIC 9.
           02 NUMID  PIC 99.
           02 ACCION PIC 9.
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
           02 PTRO PIC 999.
           02 CUAL PIC 9.
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
           02 CUANTO PIC X(80).
           02 POR.
               03 NFICHERO PIC X(15).
               03 PORCIENTO PIC 999.
               03 FILLER PIC X VALUE "%".
       01 DATOSMEN.
           02 FILLER PIC X(78) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "   No tiene ningun cliente en e".
           02 FILLER PIC X(32) VALUE "sta ciudad FORASTERO           ".
           02 FILLER PIC X(14) VALUE " ".
           02 FILLER PIC X(32) VALUE "   ERROR intento de Overfloat  ".
           02 FILLER PIC X(32) VALUE "                               ".
           02 FILLER PIC X(14) VALUE " ".
           02 FILLER PIC X(32) VALUE " No permito que el campo nombre".
           02 FILLER PIC X(32) VALUE "este sin datos no te pases de".
           02 FILLER PIC X(14) VALUE "listo".
           02 FILLER PIC X(32) VALUE "   ERROR intento de Overfloat  ".
           02 FILLER PIC X(32) VALUE "                               ".
           02 FILLER PIC X(14) VALUE " ".
       01 MENSAJES REDEFINES DATOSMEN.
           02 LM PIC X(78) OCCURS 5.
       01 BLANCOS.
           02 FILLER PIC X(41) VALUE ALL " ".
       01 Colores.
           02 ColorA PIC X(27) VALUE "FCOLOR=WHITE, BCOLOR=BLUE".
           02 ColorN PIC X(27) VALUE "FCOLOR=WHITE, BCOLOR=BLAK".
           02 ColorV PIC X(27) VALUE "FCOLOR=WHITE, BCOLOR=BLUE".
       PROCEDURE DIVISION.
       MAIN.
           PERFORM INIC
           perform esto until valor  = maxporciento
      *     MOVE "B" TO BIEN
      *     MOVE FICHERO(1) TO NOMBRE
      *     OPEN INPUT PLATOS
      *     MOVE FICHERO(2) TO NOMBRE
      *     OPEN OUTPUT PLATOS
      *     PERFORM  BAKUPPALTOS UNTIL BIEN = "N"
      *     MOVE FICHERO(1) TO NOMBRE
      *     CLOSE PLATOS
      *     MOVE FICHERO(2) TO NOMBRE
      *     CLOSE PLATOS
           MOVE 3 TO ACCION.
           PERFORM SVENTANA.
           EXIT PROGRAM.
       esto.
           ADD 1 TO VALOR
           MOVE 2 TO ACCION
           PERFORM SVENTANA.

       INIC.
           MOVE 1 TO VALOR
           MOVE 1 TO ACCION
           PERFORM SVENTANA
           MOVE 2 TO ACCION
           PERFORM SVENTANA.
       BAKUPPALTOS.
           MOVE FICHERO(1) TO NFICHERO
           PERFORM VARYING I FROM 1 BY 1 UNTIL 1 = 100
           AND BIEN = "B"
              MOVE ALL " " TO BUFFER-PLA(I)
           END-PERFORM
           MOVE FICHERO(1) TO NOMBRE
           PERFORM CARGARPLATO VARYING I FROM 1 BY 1 UNTIL 1 = 100
           AND BIEN = "B"
           MOVE FICHERO(2) TO NOMBRE
           PERFORM ADISCOPLATO VARYING I FROM 1 BY 1 UNTIL 1 = 100
           ADD 10 TO VALOR
           MOVE 2 TO ACCION
           PERFORM SVENTANA.
       CARGARPLATO.
           READ PLATOS NEXT AT END MOVE "N" TO BIEN END-READ
           MOVE REG-PLA TO BUFFER-PLA(I).
       ADISCOPLATO.
           IF BUFFER-PLA(I) NOT = ALL " "
             MOVE BUFFER-PLA(I) TO REG-PLA
             WRITE REG-PLA END-WRITE
           END-IF.
      *****************************************************************
      *                                                               *
      *****************************************************************
       SVENTANA.
           CALL "CUROFF.EXE"
           EVALUATE ACCION
              WHEN 1
                  MOVE VENTANA1 TO VENTANA
                  CALL "VENTANA.COB" USING VENTANA
                  COMPUTE LINEA = TOPL + 1
                  COMPUTE POSICION = 2 + TOPP
                  DISPLAY "FICHERO:" LINE LINEA POSITION POSICION
                   REVERSE
                  COMPUTE LINEA = TOPL + 2
                  COMPUTE POSICION = 2 + TOPP
                  COMPUTE ANCHO =  DOWNP - 2 - POSICION
                  MOVE SPACES TO CUANTO
                  DISPLAY CUANTO LINE LINEA POSITION POSICION
                          SIZE ANCHO
              WHEN 2
                  PERFORM AJUSTAR
              WHEN 3
                  CALL "RECUP.EXE"
           END-EVALUATE
           CALL "CURON.EXE".
       AJUSTAR.
           MOVE VENTANA1 TO VENTANA
           COMPUTE LINEA = TOPL + 1
           COMPUTE POSICION =  DOWNP - 21
           COMPUTE PORCIENTO = (VALOR * 100) / MaxPorciento
           DISPLAY POR LINE LINEA POSITION POSICION REVERSE
           COMPUTE LINEA = TOPL + 2
           COMPUTE POSICION = 2 + TOPP
           COMPUTE ANCHO =  DOWNP - 2 - POSICION
           COMPUTE ANCHO = (ANCHO * PORCIENTO) / 100
           IF ANCHO = 0 MOVE 1 TO ANCHO END-IF
           MOVE SPACES TO CUANTO
           DISPLAY CUANTO LINE LINEA POSITION POSICION SIZE ANCHO
           CONTROL COLORA.

