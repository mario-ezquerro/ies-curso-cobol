       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONSULTAS-PLA.
       AUTHOR. CHICOTE-MARIO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PHILIPS.
       OBJECT-COMPUTER. PHILIPS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL PLATOS ASSIGN TO DISK "PLATOS.DAT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY IS NUMPLATO
           ALTERNATE RECORD KEY IS CODIGO WITH DUPLICATES.
       DATA DIVISION.
       FILE SECTION.
       FD PLATOS LABEL RECORD STANDARD.
       01 REG-PLA.
           02 NUMPLATO PIC 99.
           02 CODIGO PIC XX.
           02 NOM PIC X(26).
           02 PVP PIC 9(4).
       WORKING-STORAGE SECTION.
       01 ASCYV.
           02 FILLER PIC X(5) VALUE "ÉÚÖÕ±".
           02 FILLER PIC X(5) VALUE "ÍÄÄÍ±".
           02 FILLER PIC X(5) VALUE "»¿·¸±".
           02 FILLER PIC X(5) VALUE "º³º³±".
           02 FILLER PIC X(5) VALUE "ÈÀÓÔ±".
           02 FILLER PIC X(5) VALUE "¼Ù½¾±".
       01 DVENT REDEFINES ASCYV.
           02 BLOQUESVENT OCCURS 6.
              03 DIBUJOV PIC X OCCURS 5.
       01 Colores.
          02 COLOR1 PIC X(10) VALUE "BCOLOR=RED".
       01 BARRA.
          02 ELEMBARRA PIC X OCCURS 80.
       01 VENTANA.
           02 ESQUINAS.
                 03 TOPL   PIC 99.
                 03 TOPP   PIC 99.
                 03 DOWNL  PIC 99.
                 03 DOWNP  PIC 99.
           02 HELPCUADRO.
                 03 ANCHO  PIC 99.
                 03 ALTO   PIC 99.
                 03 TIPO   PIC 9.
                 03 SOMBRA PIC 9.
       01 VarFijas.
           02 MAXLINEAS PIC 99 VALUE 22.
           02 MasterPos PIC 99 VALUE 18.
           02 VENTANA1  PIC 9(14) VALUE 02182459000020.
       01 Posiciones.
           02 DONDE PIC 99 OCCURS 3.
       01 AcceptTeclado.
           02 VALOR PIC XX OCCURS 2.
           02 Tecla PIC X.
           02 TeclaFunc PIC 99 COMP.
       01 FLAGS.
           02 Fin PIC X.
           02 BIEN PIC X.
           02 Estado PIC 9.
       01 LIN.
           02 SITIO PIC 99.
           02 REPETICION PIC 99.
       01 CONTADORES.
           02 I PIC 99.
           02 J PIC 99.
           02 K PIC 99.
           02 CONT PIC 99.
           02 PUNTERO PIC 99.
           02 LINEA PIC 99.
           02 POSICION PIC 99.
           02 CONTREG PIC S9(2).
           02 HTOP     PIC 99.
           02 HDOWN    PIC 99.
           02 CONTL PIC 99 OCCURS 2.
           02 PTRO  PIC 99 OCCURS 2.
       01 DATOSPAN.
            02 LD OCCURS 99.
               03 FILLER PIC X VALUE "³".
               03 FILLER PIC X VALUE " ".
               03 PNUMPLATO PIC 99.
               03 FILLER PIC X VALUE "³".
               03 PCODIGO PIC XX.
               03 FILLER PIC X VALUE " ".
               03 PNOM PIC X(26).
               03 FILLER PIC X VALUE "³".
               03 PPVP PIC 9(4).
               03 FILLER PIC X VALUE "³".
       77 CodigoBis PIC XX.
       01 DATOSMEN.
           02 FILLER PIC X(40) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "FLECHAS ESC INTRO PAG- AR,AB    ".
           02 FILLER PIC X(8)  VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "DAME EL NUMERO Y TOPO DE PLATO ".
           02 FILLER PIC X(8)  VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "CODIGO IMPROCEDENTE ".
           02 FILLER PIC X(8)  VALUE ALL " ".
       01 MENSAJES REDEFINES DATOSMEN.
           02 LM PIC X(40) OCCURS 4.
       01 TITULO.
           02 FILLER PIC X(19) VALUE "ÚNUMÂCDÄÄNOMBRE DEL".
           02 FILLER PIC X(21) VALUE " PLATOÄÄÄÄÄÄÄÄÄÂÄPVP¿".
       01 BarraUltima.
           02 FILLER PIC X(20) VALUE "ÀÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           02 FILLER PIC X(20) VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÙ".
       LINKAGE SECTION.
       77 NUMID PIC 99.
       PROCEDURE DIVISION USING NUMID.
       MAIN.
           PERFORM INICIO.
           PERFORM Busqueda UNTIL TeclaFunc = 27.
           CLOSE PLATOS
           CALL "RECUP.EXE"
           EXIT PROGRAM.
       INICIO.
           OPEN I-O PLATOS
           MOVE MasterPos to DONDE(1)
           COMPUTE DONDE(2) = 1 + DONDE(1)
           COMPUTE DONDE(3) = 1 + DONDE(2)
           MOVE 0 TO TeclaFunc
           CALL "SALVA.EXE"
           MOVE VENTANA1 TO VENTANA
           PERFORM VENTANA
           DISPLAY TITULO LINE 3 POSITION DONDE(2)
           MOVE 0 TO ESTADO
           MOVE 4 TO CONTL(1)
           MOVE 1 TO PUNTERO
           PERFORM AjustarTabla.
       Busqueda.
           MOVE 2 TO I
           CALL "CUROFF.EXE"
           PERFORM MENSAJE
           DISPLAY LM(3) LINE 23 POSITION DONDE(2) REVERSE
           CALL "CURON.EXE"
           COMPUTE I = DONDE(2) + 36
           MOVE VALOR(1) TO VALOR(2)
           ACCEPT VALOR(1) LINE 23 POSITION I UPDATE NO BEEP TAB
            ON EXCEPTION TeclaFunc PERFORM HandlerTeclas
           END-ACCEPT.
           IF ESTADO = 0   AND VALOR(1) NOT = VALOR(2)
                MOVE VALOR(1) TO CODIGO
                PERFORM Localizar
              ELSE
                MOVE 0 TO ESTADO
           END-IF.
       HandlerTeclas.
           DISPLAY LD(PUNTERO) LINE CONT POSITION DONDE(2)
           MOVE 1 TO ESTADO
           IF TeclaFunc = 27
                MOVE PNUMPLATO(PUNTERO) TO NUMID
           END-IF
           IF TeclaFunc = 53
                ADD 1 TO PUNTERO
                ADD 1 TO CONT
           END-IF
           IF TeclaFunc = 52
                SUBTRACT 1 FROM PUNTERO
                SUBTRACT 1 FROM CONT
           END-IF
           IF PUNTERO = PTRO(2)
               MOVE PTRO(1) TO PUNTERO
               MOVE CONTL(1) TO CONT
           END-IF
           IF PUNTERO < PTRO(1)
               MOVE PTRO(2) TO PUNTERO
               SUBTRACT 1 FROM PUNTERO
               MOVE CONTL(2) TO CONT
               SUBTRACT 1 FROM CONT
           END-IF
           DISPLAY LD(PUNTERO) LINE CONT POSITION DONDE(2) REVERSE.
       Localizar.
           PERFORM BorrarTabla
           MOVE CODIGO TO CodigoBis
           START PLATOS KEY IS = CODIGO
               INVALID KEY
                   MOVE 4 TO I
                   PERFORM MENSAJE
                   DISPLAY BarraUltima LINE CONTL(2) POSITION DONDE(2)
                   DISPLAY LD(PUNTERO) LINE CONT POSITION DONDE(2)
                   REVERSE
                   CALL "CUROFF"
                   ACCEPT TECLA OFF
                   CALL "CURON"
                   MOVE 1 TO PUNTERO
                   PERFORM AjustarTabla
               NOT INVALID KEY
                   MOVE "N" TO FIN
                   MOVE 1 TO I
                   PERFORM CargarTabla UNTIL FIN = "S"
                   MOVE 1 TO PUNTERO
                   PERFORM AjustarTabla
           END-START.
       CargarTabla.
           READ PLATOS NEXT AT END MOVE  "S" TO FIN
           END-READ
           IF CODIGO = CodigoBis AND FIN = "N" AND I < 100
                MOVE NUMPLATO  TO PNUMPLATO(I)
                MOVE CODIGO    TO PCODIGO(I)
                MOVE NOM       TO PNOM (I)
                MOVE PVP       TO PPVP(I)
                ADD 1 TO I
              ELSE
                MOVE "S" TO FIN
           END-IF.
       BorrarTabla.
           PERFORM BucleBorrar VARYING I FROM 1 BY 1 UNTIL I = 99
           PERFORM BorrarPantalla VARYING I FROM CONTL(1) BY 1
            UNTIL I > CONTL(2).
       BucleBorrar.
           MOVE ALL " "  TO PNUMPLATO(I)
           MOVE ALL " "  TO PCODIGO(I)
           MOVE ALL " "  TO PNOM (I)
           MOVE ALL " "  TO PPVP(I).
       BorrarPantalla.
           DISPLAY LM(1) LINE I POSITION DONDE(2) REVERSE.
      ******************************************************************
      * Algoritmos que ajusta la tabla y pantalla en lecturas e inicios
      ******************************************************************
       AjustarTabla.
           MOVE PUNTERO TO PTRO(1)
           MOVE CONTL(1) TO CONT
           MOVE "N" TO BIEN
           PERFORM SacarPantalla UNTIL BIEN = "S"
           MOVE CONT TO CONTL(2)
           MOVE PUNTERO TO PTRO(2)
           DISPLAY BarraUltima LINE CONTL(2) POSITION DONDE(2)
           MOVE PTRO(1) TO PUNTERO
           MOVE CONTL(1) TO CONT
           DISPLAY LD(PUNTERO) LINE CONT POSITION DONDE(2) REVERSE.
       SacarPantalla.
           IF PNUMPLATO(PUNTERO) NOT = ALL " "
                DISPLAY LD(PUNTERO) LINE CONT POSITION DONDE(2)
                ADD 1 TO CONT
                ADD 1 TO PUNTERO
                IF CONT = MaxLineas MOVE "S" TO BIEN END-IF
             ELSE
               MOVE "S" TO BIEN
               IF PUNTERO = 1
                 ADD 1 TO CONT
                 ADD 1 TO PUNTERO
               END-IF
           END-IF.
      ******************************************************************
      *      algoritmos de sub-sug rutinas + ventana...                *
      ******************************************************************
       MENSAJE.
           DISPLAY LM(I) LINE 24 POSITION DONDE(2) REVERSE.

       Ventana.
           IF TIPO = 0 MOVE 1 TO TIPO END-IF
           IF TOPL = 0 MOVE 1 TO TOPL END-IF
           IF TOPP = 0 MOVE 1 TO TOPP END-IF
           PERFORM bucleVentana.
       bucleVentana.
           IF ANCHO NOT = 0
                COMPUTE DOWNL = TOPL + ALTO
                COMPUTE DOWNP = TOPP + ANCHO - 1
              ELSE
                COMPUTE ANCHO = DOWNP - TOPP + 1
                COMPUTE ALTO  = DOWNL - TOPL
           END-IF
           COMPUTE I = (TOPL + DOWNL) / 2
           MOVE I TO HDOWN
           MOVE I TO HTOP
           CALL "CUROFF.EXE"
           MOVE ALL " " TO BARRA
           MOVE DIBUJOV(4 , TIPO) TO ELEMBARRA (1)
           MOVE DIBUJOV(4 , TIPO) TO ELEMBARRA (ANCHO)
           PERFORM DIBUJAR UNTIL TOPL > HTOP AND DOWNL < HDOWN
           PERFORM CARGARLINEA
           MOVE DIBUJOV(1 , TIPO) TO ELEMBARRA (1)
           MOVE DIBUJOV(3 , TIPO) TO ELEMBARRA (ANCHO)
           DISPLAY BARRA LINE HTOP POSITION TOPP SIZE ANCHO REVERSE
           MOVE DIBUJOV(5 , TIPO) TO ELEMBARRA (1)
           MOVE DIBUJOV(6 , TIPO) TO ELEMBARRA (ANCHO)
           DISPLAY BARRA LINE HDOWN POSITION TOPP SIZE ANCHO REVERSE
           IF SOMBRA = 1
             COMPUTE I = (TOPL + DOWNL) / 2
             MOVE I TO HDOWN
             MOVE I TO HTOP
             COMPUTE POSICION = 1 + DOWNP
             PERFORM SacarSombra UNTIL TOPL > HTOP AND DOWNL < HDOWN
             DISPLAY " " LINE HDOWN POSITION POSICION CONTROL COLOR1
             MOVE ALL " " TO BARRA
             COMPUTE POSICION = TOPP + 1
             COMPUTE LINEA    = HDOWN + 1
             DISPLAY  BARRA LINE LINEA POSITION POSICION CONTROL COLOR1
             SIZE ANCHO
           END-IF
           MOVE 0 TO ANCHO
           MOVE 0 TO ALTO.
           CALL "CURON.EXE".
       SacarSombra.
           DISPLAY " " LINE HTOP  POSITION POSICION CONTROL COLOR1
           DISPLAY " " LINE HDOWN POSITION POSICION CONTROL COLOR1
           COMPUTE HTOP = HTOP - 1
           ADD 1 TO HDOWN.
       DIBUJAR.
           DISPLAY BARRA LINE HTOP POSITION TOPP SIZE ANCHO REVERSE
           DISPLAY BARRA LINE HDOWN POSITION TOPP SIZE ANCHO REVERSE
           COMPUTE HTOP = HTOP - 1
           ADD 1 TO HDOWN.
       CARGARLINEA.
           EVALUATE TIPO
              WHEN 1
                MOVE ALL "Í" TO BARRA
              WHEN 2
                MOVE ALL "Ä" TO BARRA
              WHEN 3
                MOVE ALL "Ä" TO BARRA
              WHEN 4
                MOVE ALL "Í" TO BARRA
              WHEN 5
                MOVE ALL "±" TO BARRA
              WHEN OTHER
                MOVE ALL "Í" TO BARRA
           END-EVALUATE.
