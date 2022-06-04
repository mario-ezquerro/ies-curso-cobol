       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTA-PLA.
       AUTHOR. CHICOTE-MARIO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PHILIPS.
       OBJECT-COMPUTER. PHILIPS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL RESERVA ASSIGN TO DISK "RESERVA.DAT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS PAGINA
           ALTERNATE RECORD KEY IS FECHA WITH DUPLICATES.
       DATA DIVISION.
       FILE SECTION.
       FD RESERVA LABEL RECORD STANDARD.
       01 REG-RES.
           02 NOMBRE PIC X(30).
           02 DIRECCION PIC X(25).
           02 LOCALIDAD PIC X(15).
           02 TELEFONO PIC X(10).
           02 PROVINCIA PIC X(10).
           02 COMENSALES PIC 99.
           02 NIF PIC X(9).
           02 PAGINA PIC 9(5).
           02 FECHA.
               03 ANO PIC 99.
               03 MES PIC 99.
               03 DIA PIC 99.
           02 ANOTACIONES.
               03 NOTAS PIC X(63) OCCURS 8.
       WORKING-STORAGE SECTION.
       77 VENT PIC 9(16).
       77 I PIC 99.
       77 J PIC 99.
       77 TECLA PIC X.
       77 TECLAF PIC 99 COMP.
       77 LINEA PIC 99.
       77 PAG PIC 9(5) VALUE 1.
       77 COLOR PIC X(25) VALUE "FCOLOR=WHITE, BCOLOR=BLUE".
       77 FIN PIC X.
       77 VEZ PIC 9(3) VALUE 1.
       77 MODIFIC PIC X VALUE "N".
       77 EXIS PIC X.
       77 NUMID PIC 99.
       77 ESTADO PIC 9.
       77 ANULAR PIC X VALUE "S".
       77 BORRADO PIC X VALUE "N".
       77 NOTAC PIC X VALUE "N".
      * Campos sobre los que voy a aceptar.
       77 T-NOMBRE PIC X(30).
       77 T-DIRECCION PIC X(25).
       77 T-LOCALIDAD PIC X(15).
       77 T-TELEFONO PIC X(10).
       77 T-PROVINCIA PIC X(10).
       77 T-COMENSALES PIC 99.
       77 T-NIF PIC X(9).
       01 T-ANOTACIONES.
           02 T-NOTAS PIC X(63) OCCURS 8.
       01 WFECHA.
           02 WANO PIC 99.
           02 WMES PIC 99.
           02 WDIA PIC 99.
      ****
       01 FEC-DIA.
           02 FANO PIC 99.
           02 FMES PIC 99.
           02 FDIA PIC 99.
       01 LIN.
           02 SITIO PIC 99.
           02 REPETICION PIC 99.
       01 PANTALLA.
           02 LP PIC X(80) OCCURS 25.
       01 DATOSPAN.
           02 LD OCCURS 37.
               03 FILLER PIC X VALUE " ".
               03 PNUMPLATO PIC 99.
               03 FILLER PIC X VALUE "³".
               03 PCODIGO PIC XX.
               03 FILLER PIC X VALUE " ".
               03 PNOM PIC X(26).
               03 FILLER PIC X VALUE "³".
               03 PPVP PIC 9(4).
       01 DATOSMEN.
           02 FILLER PIC X(80) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "ÚÄÄÄÄÄ¿ÄÄÄÄÄ¿ÄÄÄÄÄ¿ÄÄÄÄÄ¿ÄÄÄÄÄ¿Ä".
           02 FILLER PIC X(31) VALUE "ÄÄÄÄÄ¿ÄÄÄÄÄ¿ÄÄÄÄÄ¿ÄÄÄÄÄÄÄ¿ÄÄÄÄÄ".
           02 FILLER PIC X(17) VALUE "¿ÄÄÄÄÄ¿ÄÄÄÄÄÄÄÄÄ¿".
           02 FILLER PIC X(32) VALUE "³ENERO³FEBRE³MARZO³ABRIL³MAYO ³J".
           02 FILLER PIC X(31) VALUE "UNIO ³JULIO³AGOST³SEPTBRE³OCTUB".
           02 FILLER PIC X(17) VALUE "³NOVBR³DICIEMBRE³".
           02 FILLER PIC X(32) VALUE "³     ÀÄÄÄÄÄÀÄÄÄÄÄÀÄÄÄÄÄÀÄÄÄÄÄÀÄ".
           02 FILLER PIC X(31) VALUE "ÄÄÄÄÄÀÄÄÄÄÄÀÄÄÄÄÄÀÄÄÄÄÄÄÄÀÄÄÄÄÄ".
           02 FILLER PIC X(17) VALUE "ÀÄÄÄÄÄÀÄÄÄÄÄÄÄÄÄ³".
           02 FILLER PIC X(32) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄ¿ÄÄÄÄÄ¿ÄÄÄÄÄ¿ÄÄÄÄÄ¿Ä".
           02 FILLER PIC X(31) VALUE "ÄÄÄÄÄ¿ÄÄÄÄÄ¿ÄÄÄÄÄ¿ÄÄÄÄÄÄÄ¿ÄÄÄÄÄ".
           02 FILLER PIC X(17) VALUE "¿ÄÄÄÄÄ¿ÄÄÄÄÄÄÄÄÄ¿".
           02 FILLER PIC X(32) VALUE "³    FEBRERO³MARZO³ABRIL³MAYO ³J".
           02 FILLER PIC X(31) VALUE "UNIO ³JULIO³AGOST³SEPTBRE³OCTUB".
           02 FILLER PIC X(17) VALUE "³NOVBR³DICIEMBRE³".
           02 FILLER PIC X(32) VALUE "³           ÀÄÄÄÄÄÀÄÄÄÄÄÀÄÄÄÄÄÀÄ".
           02 FILLER PIC X(31) VALUE "ÄÄÄÄÄÀÄÄÄÄÄÀÄÄÄÄÄÀÄÄÄÄÄÄÄÀÄÄÄÄÄ".
           02 FILLER PIC X(17) VALUE "ÀÄÄÄÄÄÀÄÄÄÄÄÄÄÄÄ³".
           02 FILLER PIC X(32) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ÄÄÄÄÄ¿ÄÄÄÄÄ¿Ä".
           02 FILLER PIC X(31) VALUE "ÄÄÄÄÄ¿ÄÄÄÄÄ¿ÄÄÄÄÄ¿ÄÄÄÄÄÄÄ¿ÄÄÄÄÄ".
           02 FILLER PIC X(17) VALUE "¿ÄÄÄÄÄ¿ÄÄÄÄÄÄÄÄÄ¿".
           02 FILLER PIC X(32) VALUE "³            MARZO³ABRIL³MAYO ³J".
           02 FILLER PIC X(31) VALUE "UNIO ³JULIO³AGOST³SEPTBRE³OCTUB".
           02 FILLER PIC X(17) VALUE "³NOVBR³DICIEMBRE³".
           02 FILLER PIC X(32) VALUE "³                 ÀÄÄÄÄÄÀÄÄÄÄÄÀÄ".
           02 FILLER PIC X(31) VALUE "ÄÄÄÄÄÀÄÄÄÄÄÀÄÄÄÄÄÀÄÄÄÄÄÄÄÀÄÄÄÄÄ".
           02 FILLER PIC X(17) VALUE "ÀÄÄÄÄÄÀÄÄÄÄÄÄÄÄÄ³".
           02 FILLER PIC X(32) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ÄÄÄÄÄ¿Ä".
           02 FILLER PIC X(31) VALUE "ÄÄÄÄÄ¿ÄÄÄÄÄ¿ÄÄÄÄÄ¿ÄÄÄÄÄÄÄ¿ÄÄÄÄÄ".
           02 FILLER PIC X(17) VALUE "¿ÄÄÄÄÄ¿ÄÄÄÄÄÄÄÄÄ¿".
           02 FILLER PIC X(32) VALUE "³                  ABRIL³MAYO ³J".
           02 FILLER PIC X(31) VALUE "UNIO ³JULIO³AGOST³SEPTBRE³OCTUB".
           02 FILLER PIC X(17) VALUE "³NOVBR³DICIEMBRE³".
           02 FILLER PIC X(32) VALUE "³                       ÀÄÄÄÄÄÀÄ".
           02 FILLER PIC X(31) VALUE "ÄÄÄÄÄÀÄÄÄÄÄÀÄÄÄÄÄÀÄÄÄÄÄÄÄÀÄÄÄÄÄ".
           02 FILLER PIC X(17) VALUE "ÀÄÄÄÄÄÀÄÄÄÄÄÄÄÄÄ³".
           02 FILLER PIC X(32) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿Ä".
           02 FILLER PIC X(31) VALUE "ÄÄÄÄÄ¿ÄÄÄÄÄ¿ÄÄÄÄÄ¿ÄÄÄÄÄÄÄ¿ÄÄÄÄÄ".
           02 FILLER PIC X(17) VALUE "¿ÄÄÄÄÄ¿ÄÄÄÄÄÄÄÄÄ¿".
           02 FILLER PIC X(32) VALUE "³                        MAYO ³J".
           02 FILLER PIC X(31) VALUE "UNIO ³JULIO³AGOST³SEPTBRE³OCTUB".
           02 FILLER PIC X(17) VALUE "³NOVBR³DICIEMBRE³".
           02 FILLER PIC X(32) VALUE "³                             ÀÄ".
           02 FILLER PIC X(31) VALUE "ÄÄÄÄÄÀÄÄÄÄÄÀÄÄÄÄÄÀÄÄÄÄÄÄÄÀÄÄÄÄÄ".
           02 FILLER PIC X(17) VALUE "ÀÄÄÄÄÄÀÄÄÄÄÄÄÄÄÄ³".
           02 FILLER PIC X(32) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           02 FILLER PIC X(31) VALUE "ÄÄÄÄÄ¿ÄÄÄÄÄ¿ÄÄÄÄÄ¿ÄÄÄÄÄÄÄ¿ÄÄÄÄÄ".
           02 FILLER PIC X(17) VALUE "¿ÄÄÄÄÄ¿ÄÄÄÄÄÄÄÄÄ¿".
           02 FILLER PIC X(32) VALUE "³                              J".
           02 FILLER PIC X(31) VALUE "UNIO ³JULIO³AGOST³SEPTBRE³OCTUB".
           02 FILLER PIC X(17) VALUE "³NOVBR³DICIEMBRE³".
           02 FILLER PIC X(32) VALUE "³                               ".
           02 FILLER PIC X(31) VALUE "     ÀÄÄÄÄÄÀÄÄÄÄÄÀÄÄÄÄÄÄÄÀÄÄÄÄÄ".
           02 FILLER PIC X(17) VALUE "ÀÄÄÄÄÄÀÄÄÄÄÄÄÄÄÄ³".
           02 FILLER PIC X(32) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           02 FILLER PIC X(31) VALUE "ÄÄÄÄÄÄÄÄÄÄÄ¿ÄÄÄÄÄ¿ÄÄÄÄÄÄÄ¿ÄÄÄÄÄ".
           02 FILLER PIC X(17) VALUE "¿ÄÄÄÄÄ¿ÄÄÄÄÄÄÄÄÄ¿".
           02 FILLER PIC X(32) VALUE "³                               ".
           02 FILLER PIC X(31) VALUE "      JULIO³AGOST³SEPTBRE³OCTUB".
           02 FILLER PIC X(17) VALUE "³NOVBR³DICIEMBRE³".
           02 FILLER PIC X(32) VALUE "³                               ".
           02 FILLER PIC X(31) VALUE "           ÀÄÄÄÄÄÀÄÄÄÄÄÄÄÀÄÄÄÄÄ".
           02 FILLER PIC X(17) VALUE "ÀÄÄÄÄÄÀÄÄÄÄÄÄÄÄÄ³".
           02 FILLER PIC X(32) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           02 FILLER PIC X(31) VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ÄÄÄÄÄÄÄ¿ÄÄÄÄÄ".
           02 FILLER PIC X(17) VALUE "¿ÄÄÄÄÄ¿ÄÄÄÄÄÄÄÄÄ¿".
           02 FILLER PIC X(32) VALUE "³                               ".
           02 FILLER PIC X(31) VALUE "           AGOSTO³SEPTBRE³OCTUB".
           02 FILLER PIC X(17) VALUE "³NOVBR³DICIEMBRE³".
           02 FILLER PIC X(32) VALUE "³                               ".
           02 FILLER PIC X(31) VALUE "                 ÀÄÄÄÄÄÄÄÀÄÄÄÄÄ".
           02 FILLER PIC X(17) VALUE "ÀÄÄÄÄÄÀÄÄÄÄÄÄÄÄÄ³".
           02 FILLER PIC X(32) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           02 FILLER PIC X(31) VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ÄÄÄÄÄ".
           02 FILLER PIC X(17) VALUE "¿ÄÄÄÄÄ¿ÄÄÄÄÄÄÄÄÄ¿".
           02 FILLER PIC X(32) VALUE "³                               ".
           02 FILLER PIC X(31) VALUE "               SEPTIEMBRE³OCTUB".
           02 FILLER PIC X(17) VALUE "³NOVBR³DICIEMBRE³".
           02 FILLER PIC X(32) VALUE "³                               ".
           02 FILLER PIC X(31) VALUE "                         ÀÄÄÄÄÄ".
           02 FILLER PIC X(17) VALUE "ÀÄÄÄÄÄÀÄÄÄÄÄÄÄÄÄ³".
           02 FILLER PIC X(32) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           02 FILLER PIC X(31) VALUE ALL "Ä".
           02 FILLER PIC X(17) VALUE "¿ÄÄÄÄÄ¿ÄÄÄÄÄÄÄÄÄ¿".
           02 FILLER PIC X(32) VALUE "³                               ".
           02 FILLER PIC X(31) VALUE "                        OCTUBRE".
           02 FILLER PIC X(17) VALUE "³NOVBR³DICIEMBRE³".
           02 FILLER PIC X(32) VALUE "³                               ".
           02 FILLER PIC X(31) VALUE ALL " ".
           02 FILLER PIC X(17) VALUE "ÀÄÄÄÄÄÀÄÄÄÄÄÄÄÄÄ³".
           02 FILLER PIC X(32) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           02 FILLER PIC X(31) VALUE ALL "Ä".
           02 FILLER PIC X(17) VALUE "ÄÄÄÄÄÄ¿ÄÄÄÄÄÄÄÄÄ¿".
           02 FILLER PIC X(32) VALUE "³                               ".
           02 FILLER PIC X(31) VALUE "                            NOV".
           02 FILLER PIC X(17) VALUE "IEMBRE³DICIEMBRE³".
           02 FILLER PIC X(32) VALUE "³                               ".
           02 FILLER PIC X(31) VALUE ALL " ".
           02 FILLER PIC X(17) VALUE "      ÀÄÄÄÄÄÄÄÄÄ³".
           02 FILLER PIC X(32) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           02 FILLER PIC X(31) VALUE ALL "Ä".
           02 FILLER PIC X(17) VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
           02 FILLER PIC X(32) VALUE "³                               ".
           02 FILLER PIC X(31) VALUE ALL " ".
           02 FILLER PIC X(17) VALUE "      DICIEMBRE ³".
           02 FILLER PIC X(32) VALUE "³                               ".
           02 FILLER PIC X(31) VALUE ALL " ".
           02 FILLER PIC X(17) VALUE "                ³".
           02 FILLER PIC X(32) VALUE "³ NOMBRE:                       ".
           02 FILLER PIC X(32) VALUE "                 NIF:           ".
           02 FILLER PIC X(16) VALUE "               ³".
           02 FILLER PIC X(32) VALUE "³ DIRECCION:                    ".
           02 FILLER PIC X(32) VALUE "                 TELEFONO:      ".
           02 FILLER PIC X(16) VALUE "               ³".
           02 FILLER PIC X(32) VALUE "³ LOCALIDAD:                    ".
           02 FILLER PIC X(32) VALUE "                 PROVINCIA:     ".
           02 FILLER PIC X(16) VALUE "               ³".
           02 FILLER PIC X(32) VALUE "³ NUM. DE COMENSALES:           ".
           02 FILLER PIC X(32) VALUE "                                ".
           02 FILLER PIC X(16) VALUE "               ³".
           02 FILLER PIC X(32) VALUE "³ ANOTACIONES:                  ".
           02 FILLER PIC X(32) VALUE "                                ".
           02 FILLER PIC X(16) VALUE "               ³".
           02 FILLER PIC X(32) VALUE "³         ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           02 FILLER PIC X(32) VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           02 FILLER PIC X(16) VALUE "ÄÄÄÄÄÄÄÄÄ      ³".
           02 FILLER PIC X(32) VALUE "³ F2:Buscar fecha  F3:Anular res".
           02 FILLER PIC X(32) VALUE "erva F5:A¤adir reserva en esa fe".
           02 FILLER PIC X(16) VALUE "cha RE/AV PAG  ³".
           02 FILLER PIC X(32) VALUE "³ No existen m s hojas. Presione".
           02 FILLER PIC X(32) VALUE " RE-PAG ¢ F2 para cambiar de fec".
           02 FILLER PIC X(16) VALUE "cha            ³".
           02 FILLER PIC X(32) VALUE "³ ¨Est  seguro de que quiere anu".
           02 FILLER PIC X(32) VALUE "lar esta reserva? S/N           ".
           02 FILLER PIC X(16) VALUE "               ³".
           02 FILLER PIC X(32) VALUE "³ Est  buscando la fecha m s pr¢".
           02 FILLER PIC X(32) VALUE "xima, espere un momento.        ".
           02 FILLER PIC X(16) VALUE "               ³".
       01 LINEAMENSAJE REDEFINES DATOSMEN.
           02 LM PIC X(80) OCCURS 50.
       01 DATOSVENTANA.
           02 FILLER PIC 99 VALUE 04.
           02 FILLER PIC 99 VALUE 34.
           02 FILLER PIC 99 VALUE 39.
           02 FILLER PIC 99 VALUE 40.
           02 FILLER PIC 99 VALUE 44.
           02 FILLER PIC 99 VALUE 74.
       01 POSILINEAS REDEFINES DATOSVENTANA.
           02 PSLINEAS PIC 99 OCCURS 6.
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY " " ERASE
           OPEN I-O RESERVA
           ACCEPT WFECHA FROM DATE
           MOVE WFECHA TO FEC-DIA
           MOVE LOW-VALUE TO FECHA
           START RESERVA KEY IS > FECHA
             INVALID KEY CONTINUE
                 NOT INVALID KEY
                  PERFORM BORRADO UNTIL FECHA NOT < FEC-DIA
           END-START
           MOVE FEC-DIA TO WFECHA
           PERFORM RECFECHA
           PERFORM ALTAS UNTIL TECLAF = 27
           CLOSE RESERVA
           EXIT PROGRAM.

      ***   Borrado de las paginas anteriores a la fecha actual  ***

       BORRADO.
           READ RESERVA NEXT AT END MOVE "S" TO FIN
           END-READ.
           IF FECHA < FEC-DIA
                 DELETE RESERVA
           END-IF.

      ***  Evaluo el mes y seg£n eso muestro una cabecera u otra ***

       RECFECHA.
           EVALUATE WMES
               WHEN 1 MOVE 2 TO J
               WHEN 2 MOVE 5 TO J
               WHEN 3 MOVE 8 TO J
               WHEN 4 MOVE 11 TO J
               WHEN 5 MOVE 14 TO J
               WHEN 6 MOVE 17 TO J
               WHEN 7 MOVE 20 TO J
               WHEN 8 MOVE 23 TO J
               WHEN 9 MOVE 26 TO J
               WHEN 10 MOVE 29 TO J
               WHEN 11 MOVE 32 TO J
               WHEN 12 MOVE 35 TO J
           END-EVALUATE.
           PERFORM CABECERA
           IF VEZ = 1
              PERFORM PRESENTA
              ADD 1 TO VEZ
           END-IF.
           PERFORM LECTURA
           IF EXIS = "N"
              PERFORM BuscarUltimo VARYING PAG FROM 1 BY 1 UNTIL PAG = 0
           END-IF.
       CABECERA.
           MOVE  0200258004000011 TO VENT
           CALL "VENTBUF.COB" USING VENT PANTALLA
           MOVE LM(J) TO LP(2)
           ADD 1 TO J
           MOVE LM(J) TO LP(3)
           ADD 1 TO J
           MOVE LM(J) TO LP(4)
           CALL "CUROFF.EXE"
           PERFORM SACAR VARYING I FROM 2 BY 1 UNTIL I > 4
           CALL "CURON.EXE".
       SACAR.
           DISPLAY LP(I) LINE I POSITION 1 CONTROL COLOR.

      ***  Me muestra el resto de pantalla que no sea la cabecera  ***

       PRESENTA.
           MOVE LM(38) TO LP(7)
           MOVE LM(39) TO LP(8)
           MOVE LM(40) TO LP(9)
           MOVE LM(41) TO LP(10)
           MOVE LM(42) TO LP(12)
           MOVE LM(43) TO LP(14) LP(15) LP(16) LP(17) LP(18)
              LP(19) LP(20) LP(21)
           MOVE LM(44) TO LP(23)
           PERFORM SACAR VARYING I FROM 5 BY 1 UNTIL I > 25
           DISPLAY WDIA LINE 5 POSITION 5 CONTROL COLOR
           DISPLAY "/" LINE 5 POSITION 7 CONTROL COLOR
           DISPLAY WMES LINE 5 POSITION 8 CONTROL COLOR
           DISPLAY "/" LINE 5 POSITION 10 CONTROL COLOR
           DISPLAY WANO LINE 5 POSITION 11 CONTROL COLOR
           MOVE WFECHA TO FECHA.

      ***  Compruebo si existe la fecha introducida   ***
      ***        y actuaremos en consecuencia...      ***
       LECTURA.
           START RESERVA KEY IS = FECHA
             INVALID KEY MOVE "N" TO EXIS
                         MOVE "N" TO MODIFIC
                         PERFORM BLANCOS
            NOT INVALID KEY
               PERFORM VISUAL
           END-START.

      ***  Si existe, leemos el registro y lo visualizamos  ***
       VISUAL.
           READ RESERVA NEXT AT END MOVE "S" TO FIN
           END-READ
           MOVE FECHA TO WFECHA
           MOVE NOMBRE TO T-NOMBRE
           MOVE NIF TO T-NIF
           MOVE DIRECCION TO T-DIRECCION
           MOVE LOCALIDAD TO T-LOCALIDAD
           MOVE PROVINCIA TO T-PROVINCIA
           MOVE TELEFONO TO T-TELEFONO
           MOVE COMENSALES TO T-COMENSALES
           CALL "CUROFF.EXE"
           DISPLAY WDIA LINE 5 POSITION 5 CONTROL COLOR
           DISPLAY WMES LINE 5 POSITION 8 CONTROL COLOR
           DISPLAY WANO LINE 5 POSITION 11 CONTROL COLOR
           DISPLAY T-NOMBRE LINE 7 POSITION 11 CONTROL COLOR
           DISPLAY T-NIF LINE 7 POSITION 55 CONTROL COLOR
           DISPLAY T-DIRECCION LINE 8 POSITION 14 CONTROL COLOR
           DISPLAY T-TELEFONO LINE 8 POSITION 60 CONTROL COLOR
           DISPLAY T-LOCALIDAD LINE 9 POSITION 14 CONTROL COLOR
           DISPLAY T-PROVINCIA LINE 9 POSITION 61 CONTROL COLOR
           DISPLAY T-COMENSALES LINE 10 POSITION 24 CONTROL COLOR
           MOVE 14 TO LINEA
           MOVE 1 TO I
           PERFORM UNTIL I > 8
              MOVE NOTAS(I) TO T-NOTAS(I)
              DISPLAY T-NOTAS(I) LINE LINEA POSITION 11 CONTROL COLOR
              ADD 1 TO I
              ADD 1 TO LINEA
           END-PERFORM.
           CALL "CURON.EXE"
           MOVE "S" TO MODIFIC
           MOVE "S" TO EXIS.

      *** Si no existe muevo blancos a todos los campos para que me ***
      *** limpie la pantalla y para que el UPDATE este a blancos    ***
       BLANCOS.
           MOVE ALL " " TO T-NOMBRE
           MOVE ALL " " TO T-NIF
           MOVE ALL " " TO T-DIRECCION
           MOVE ALL " " TO T-LOCALIDAD
           MOVE ALL " " TO T-PROVINCIA
           MOVE ALL " " TO T-TELEFONO
           MOVE ALL " " TO T-COMENSALES
           CALL "CUROFF.EXE"
           DISPLAY T-NOMBRE LINE 7 POSITION 11 CONTROL COLOR
           DISPLAY T-NIF LINE 7 POSITION 55 CONTROL COLOR
           DISPLAY T-DIRECCION LINE 8 POSITION 14 CONTROL COLOR
           DISPLAY T-TELEFONO LINE 8 POSITION 60 CONTROL COLOR
           DISPLAY T-LOCALIDAD LINE 9 POSITION 14 CONTROL COLOR
           DISPLAY T-PROVINCIA LINE 9 POSITION 61 CONTROL COLOR
           DISPLAY T-COMENSALES LINE 10 POSITION 24 CONTROL COLOR.
           MOVE 14 TO LINEA
           MOVE 1 TO I
           PERFORM UNTIL I > 8
             MOVE ALL " " TO T-NOTAS(I)
             MOVE LM(43) TO LP(14) LP(15) LP(16) LP(17) LP(18)
              LP(19) LP(20) LP(21) LP(22)
             DISPLAY LP(LINEA) LINE LINEA POSITION 1 CONTROL COLOR
             ADD 1 TO I
             ADD 1 TO LINEA
           END-PERFORM.
           CALL "CURON.EXE".

      ***  Asigno la clave principal al registro, ***
      *** leo hasta que encuentro la £ltima clave ***
       BuscarUltimo.
           MOVE PAG TO PAGINA
           READ RESERVA INVALID KEY
                  MOVE PAG TO PAGINA
                  MOVE 99999 TO PAG
                  NOT INVALID KEY CONTINUE
           END-READ.

      ***  Acepto todos los campos del registro y compruebo si ***
      ***  se llama a las teclas de funcion.  Las notas las    ***
      ***  acepto mediante un bucle ya que est n en una tabla  ***
       ALTAS.
           MOVE 0 TO ESTADO
           IF ESTADO = 0
             ACCEPT T-NOMBRE LINE 7 POSITION 11 CONTROL COLOR TAB UPDATE
               NO BEEP ON EXCEPTION TECLAF PERFORM TECLASFUNCION
             END-ACCEPT
           END-IF.
           IF ESTADO = 0
             ACCEPT T-NIF LINE 7 POSITION 55 CONTROL COLOR TAB UPDATE
               NO BEEP ON EXCEPTION TECLAF PERFORM TECLASFUNCION
             END-ACCEPT
           END-IF.
           IF ESTADO = 0
             ACCEPT T-DIRECCION LINE 8 POSITION 14 CONTROL COLOR TAB
               NO BEEP UPDATE ON EXCEPTION TECLAF PERFORM TECLASFUNCION
             END-ACCEPT
           END-IF.
           IF ESTADO = 0
              ACCEPT T-TELEFONO LINE 8 POSITION 60 CONTROL COLOR TAB
               UPDATE NO BEEP ON EXCEPTION TECLAF PERFORM TECLASFUNCION
             END-ACCEPT
           END-IF.
           IF ESTADO = 0
             ACCEPT T-LOCALIDAD LINE 9 POSITION 14 CONTROL COLOR TAB
                NO BEEP UPDATE ON EXCEPTION TECLAF PERFORM TECLASFUNCION
             END-ACCEPT
           END-IF.
           IF ESTADO = 0
             ACCEPT T-PROVINCIA LINE 9 POSITION 61 CONTROL COLOR TAB
                NO BEEP UPDATE ON EXCEPTION TECLAF PERFORM TECLASFUNCION
             END-ACCEPT
           END-IF.
           IF ESTADO = 0
             ACCEPT T-COMENSALES LINE 10 POSITION 24 CONTROL COLOR TAB
                NO BEEP UPDATE ON EXCEPTION TECLAF PERFORM TECLASFUNCION
             END-ACCEPT
           END-IF.
           MOVE 1 TO I
           MOVE 14 TO LINEA
           IF ESTADO = 0
             PERFORM ANOTAR UNTIL I > 8
             MOVE "N" TO NOTAC
           END-IF.
           MOVE 0 TO ESTADO.
       ANOTAR.
           MOVE "S" TO NOTAC
           IF ESTADO = 0
              ACCEPT NOTAS(I) LINE LINEA POSITION 11 CONTROL COLOR
               NO BEEP UPDATE ON EXCEPTION TECLAF PERFORM TECLASFUNCION
              END-ACCEPT
           END-IF.
           IF ESTADO = 0
              ADD 1 TO I
              ADD 1 TO LINEA
           END-IF.

      ***    Muevo los campos aceptados a los campos del registro    ***
      *** Anteriormente evaluo si existe o no el registro; si existe ***
      ***           llamo a MODIFICAR sino llamo a GRABAR            ***
       MOVER.
           MOVE WFECHA TO FECHA
           MOVE T-NOMBRE TO NOMBRE
           MOVE T-NIF TO NIF
           MOVE T-DIRECCION TO DIRECCION
           MOVE T-LOCALIDAD TO LOCALIDAD
           MOVE T-PROVINCIA TO PROVINCIA
           MOVE T-TELEFONO TO TELEFONO
           MOVE T-COMENSALES TO COMENSALES
           PERFORM UNTIL I > 8
              MOVE T-NOTAS(I) TO NOTAS(I)
              ADD 1 TO I
           END-PERFORM.
       GRABAR.
           PERFORM MOVER
           WRITE REG-RES
           END-WRITE.
           MOVE "S" TO MODIFIC
           MOVE "S" TO EXIS
           DISPLAY "YA HE GRABADO Y HE MOVIDO S TO EXIS".
       MODIFICAR.
           PERFORM MOVER
           REWRITE REG-RES
           END-REWRITE.

      *** Realizo las operaciones correspondientes  ***
      ***   asignadas a cada tecla de funcion       ***
       TECLASFUNCION.

      * F2 cambia de fecha, por ello antes tenemos que grabar.
      * Para validar la fecha llamamos a FECHAS.COB.
           IF TECLAF = 2
             IF MODIFIC = "S"
              PERFORM MODIFICAR
               ELSE
                 IF BORRADO NOT = "S"
                   PERFORM GRABAR
                 END-IF
             END-IF
             CALL "FECHAS.COB" USING WFECHA
             MOVE "N" TO FIN
             MOVE 1 TO ESTADO
             MOVE WFECHA TO FECHA
             PERFORM RECFECHA
           END-IF.

      * F3 Borra la p gina. Muevo "s" to borrado porque al cambiar de
      * fecha con F2 me vuelve a grabar y as¡ lo evito.
           IF TECLAF = 3
              DISPLAY LM(46) LINE 23 POSITION 1
              PERFORM WITH TEST AFTER UNTIL
                    ANULAR = "S" OR "s" OR "N" OR "n"
                  ACCEPT ANULAR LINE 23 POSITION 60 TAB
                      ON EXCEPTION TECLAF CONTINUE
                  END-ACCEPT
              END-PERFORM
              DISPLAY LM(44) LINE 23 POSITION 1
              IF ANULAR = "S" OR "s"
                  DELETE RESERVA
              END-IF
              MOVE 1 TO ESTADO
              MOVE "S" TO BORRADO
              MOVE "N" TO EXIS
              MOVE "N" TO MODIFIC
           END-IF.

      * Me permite a¤adir mas reservas en una misma fecha. De la otra
      * forma me modificaria la reserva ya existente de esa fecha.
           IF TECLAF = 5
             PERFORM BLANCOS
             PERFORM BuscarUltimo VARYING PAG FROM 1 BY 1 UNTIL PAG = 0
             MOVE 1 TO ESTADO
             MOVE "N" TO FIN
             MOVE "N" TO EXIS
             MOVE "N" TO MODIFIC
           END-IF.

      * En los accept de las notas, permito al usuario que se mueve por
      * las 5 lineas de anotaciones.
           IF TECLAF = 52
             IF NOTAC = "S" AND I > 1
                SUBTRACT 1 FROM LINEA
                SUBTRACT 1 FROM I
             END-IF
           END-IF.
           IF TECLAF = 53
             IF NOTAC = "S" AND I < 8
                ADD 1 TO LINEA
                ADD 1 TO I
             END-IF
           END-IF.

      * El usuario puede cambiar de fecha mediante F2 o bien p g arriba
      * o p g. abajo. Por si acaso teclea AV/RE PAG en medio de las
      * altas, le obligo a grabar.
           IF TECLAF = 67
               IF FIN = "S"
                   DISPLAY LM(44) LINE 23 POSITION 1  CONTROL COLOR
               END-IF
               IF MODIFIC = "S"
                 PERFORM MODIFICAR
                   ELSE
                    PERFORM GRABAR
               END-IF
               MOVE "N" TO EXIS
               DISPLAY LM(47) LINE 23 POSITION 1 CONTROL COLOR
               PERFORM DECREMENTO UNTIL EXIS = "S" OR FECHA = FEC-DIA
               DISPLAY LM(44) LINE 23 POSITION 1  CONTROL COLOR
               MOVE 1 TO ESTADO
               MOVE "N" TO FIN
           END-IF.
           IF TECLAF = 68
               IF EXIS = "S"
                 IF FIN = "S"
                  DISPLAY LM(45) LINE 23 POSITION 1 CONTROL COLOR
                   ELSE
                    IF MODIFIC = "S"
                      PERFORM MODIFICAR
                         ELSE
                          PERFORM GRABAR
                    END-IF
                    PERFORM VISUAL
                 END-IF
               END-IF
               MOVE 1 TO ESTADO
           END-IF.

      * Activo la tecla de escape.
           IF TECLAF = 27
             MOVE 1 TO ESTADO
             MOVE 1 TO NUMID
             CALL "MENSAJE.COB" USING NUMID
             IF NUMID NOT = 1
                 MOVE 0 TO TECLAF
                 ELSE
                   IF MODIFIC = "S"
                     PERFORM MODIFICAR
                       ELSE
                        PERFORM GRABAR
                   END-IF
                   MOVE 1 TO ESTADO
              END-IF
           END-IF.

      * Para activar RE-PAG voy decrementando la fecha hasta que
      * encuentra una.
       DECREMENTO.
           SUBTRACT 1 FROM WDIA
           IF DIA = 0
              SUBTRACT 1 FROM WMES
              MOVE 31 TO WDIA
              IF MES = 1
                 SUBTRACT 1 FROM WANO
                 MOVE 12 TO WMES
              END-IF
           END-IF.
           MOVE WFECHA TO FECHA
           START RESERVA KEY IS = FECHA
                  INVALID KEY CONTINUE
                   NOT INVALID KEY PERFORM VISUAL
           END-START.

