       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTA-PLA.
       AUTHOR. CHICOTE-MARIO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PHILIPS.
       OBJECT-COMPUTER. PHILIPS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL FACTURA ASSIGN TO DISK "FACTURAS.DAT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS NFAC
           ALTERNATE RECORD KEY IS FFECHA WITH DUPLICATES
           ALTERNATE RECORD KEY IS FGIF WITH DUPLICATES
           ALTERNATE RECORD KEY IS FPAGADA WITH DUPLICATES.
           SELECT OPTIONAL CLIENTES ASSIGN TO DISK "CLIENTES.DAT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS CGIF
           ALTERNATE RECORD KEY IS CNOMBRE WITH DUPLICATES
           ALTERNATE RECORD KEY IS CCIUDAD WITH DUPLICATES.
           SELECT LISTADO ASSIGN TO PRINT "PRINTER"
           FILE STATUS IS ERRORIMP.
       DATA DIVISION.
       FILE SECTION.
       FD FACTURA LABEL RECORD STANDARD.
       01 REG-FAC.
           02 NFAC   PIC 9(8).
           02 FGIF   PIC 9(9).
           02 FFECHA.
              03 FDIA PIC 9(2).
              03 FMES PIC 9(2).
              03 FANO PIC 9(4).
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
       FD LISTADO LABEL RECORD STANDARD.
       01 LINEAIMP PIC X(80).
       WORKING-STORAGE SECTION.
       01 FechaSys.
           02 AnoSys PIC 99.
           02 MesSys PIC 99.
           02 DiaSys PIC 99.
       01 VarConstantes.
           02 MAXLINEAS PIC 99 VALUE 20.
           02 MasterPos PIC 99 VALUE 22.
           02 MaxImp    PIC 99 VALUE 41.
           02 VENTANA1  PIC 9(16) VALUE 0222215512000000.
           02 VENTANA2  PIC 9(16) VALUE 2200258004000010.
           02 VENTANA3  PIC 9(14) VALUE 08171465000021.
           02 VENTANA4  PIC 9(14) VALUE 08211665000021.
       01 TeclasSistema.
           02 TECLA PIC X.
           02 TECLAF PIC 99 COMP.
       01 FLAGS.
           02 BIEN PIC X.
           02 ESTADO PIC 9.
           02 NUMID PIC 99.
       01 LIN.
           02 SITIO PIC 99.
           02 REPETICION PIC 99.
       01 CONTADORES.
           02 I PIC 99.
           02 J PIC 99.
           02 K PIC 99.
           02 LINEA PIC 99.
           02 POSICION PIC 99.
           02 DONDE PIC 99 OCCURS 2.
           02 CONTL PIC 99 OCCURS 2.
           02 PTRO  PIC 99 OCCURS 2.
           02 PUNTERO PIC 99.
           02 CONT PIC 99.
           02 PtroPila PIC 99.
       01 Registros.
          02 VENT    PIC 9(16).
          02 VENTANA PIC 9(14).
          02 PILA OCCURS 1 TO 50 DEPENDING ON MAXLINEAS.
             03 FAC    PIC 9(8).
             03 PAGADA PIC X.
             03 FECHA OCCURS 2.
                04 DIA PIC 99.
                04 MES PIC 99.
                04 ANO PIC 9999.
       01 DATOSPAN.
            02 LD OCCURS 30.
               03 FILLER PIC X VALUE "³".
               03 TNFAC PIC 9(8).
               03 FILLER PIC X VALUE "³".
               03 TFECHA.
                   04 TDIA PIC 9(2).
                   04 FILLER PIC X VALUE "/".
                   04 TMES PIC 9(2).
                   04 FILLER PIC X VALUE "/".
                   04 TANO PIC 9(4).
               03 FILLER PIC X VALUE "³".
               03 TPASTA PIC 9(8).
               03 FILLER PIC X VALUE "³".
               03 TPAGADA PIC X.
               03 FILLER PIC X VALUE "³".
       01 DATOSMEN.
           02 FILLER PIC X(78) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "   DEME EL NUMERO DE FACTURA A ".
           02 FILLER PIC X(32) VALUE "PARTIR DE LA QUE QUIERE BUSCA".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE " T = TODAS  N = SOLO NO PAGADAS ".
           02 FILLER PIC X(32) VALUE " P = SOLO LAS PAGADAS   O ESC".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "  DEME EL PRIMER Y EL ULTIMO DIA".
           02 FILLER PIC X(32) VALUE ALL " ".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "   FECHA   ERRONEA DEMA OTRA FEC".
           02 FILLER PIC X(32) VALUE "HA".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "   SALIR:ESC F2:BUSQUEDAS F3:BUS".
           02 FILLER PIC X(32) VALUE "CAR-EMPRESA F4:FACTURA-PAGADA   ".
           02 FILLER PIC X(14) VALUE " F5:IMPRIMIR".
           02 FILLER PIC X(32) VALUE "  ESC:FIN   USE LAS FLECHAS UP  ".
           02 FILLER PIC X(32) VALUE "DOWN   F4:FACTURA-PAGADA ".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "  GRABACION SUPER SATISFACTORIA".
           02 FILLER PIC X(32) VALUE ALL " ".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE " ATENCION DEME TODOS LOS CAMPOS ".
           02 FILLER PIC X(32) VALUE "PARA CANCELAR PULSE ESC         ".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE " EN ESTE MOMENTO A CANCELADO ".
           02 FILLER PIC X(32) VALUE "NO SE PRODICIRA NINGUNA ALTA  ".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE " NO TENGO A ESTE  CLIENTE FICHAD".
           02 FILLER PIC X(32) VALUE "O (DEBERIA DARTE VERGUENZA) ¨TE ".
           02 FILLER PIC X(14) VALUE "PAGO? INUTIL".
           02 FILLER PIC X(32) VALUE "ERROR DE  LOCALIZACION (LLAME AL".
           02 FILLER PIC X(32) VALUE " INUTIL DE SU TECNICO) ".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE " NO TIENE NINGUNA FACTURA EN EL ".
           02 FILLER PIC X(32) VALUE " DIA INDICADO  (¨A LO MEJOR ERA ".
           02 FILLER PIC X(14) VALUE " FIESTA?)".
           02 FILLER PIC X(32) VALUE " NO TIENE NINGUNA FACTURA CON ES".
           02 FILLER PIC X(32) VALUE "TE NUMERO  (¨SEGURO QUE NO ES DI".
           02 FILLER PIC X(14) VALUE "NERO NEGRO)".
       01 MENSAJES REDEFINES DATOSMEN.
           02 LM PIC X(78) OCCURS 14.
       01 TITULO.
           02 FILLER PIC X(32) VALUE "ÚÄN FACTÄÂÄÄÄDIAÄÄÄÄÂÄÄÄPVPÄÄÂP¿".
       01 BarraUltima.
           02 FILLER PIC X(32) VALUE "ÀÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÙ".
       01 BLANCOS.
           02 FILLER PIC X(32) VALUE ALL " ".
       01 Colores.
           02 ColorA PIC X(27) VALUE "FCOLOR=WHITE, BCOLOR=BLUE".
           02 ColorN PIC X(27) VALUE "FCOLOR=WHITE, BCOLOR=BLAK".
           02 ColorV PIC X(27) VALUE "FCOLOR=WHITE, BCOLOR=BLUE".
       01 VARIMPRESORA.
           02 ERRORIMP PIC XX.
           02 TOTALES  PIC 9(9).
           02 CONTPAG  PIC 999.
       01 DatosCab.
           02 PIC X(38) VALUE "ÚÄN FactÄÂÄÄFechaÄÄÄÂPastaÄÄÄÂÄNIFÄÄÄÄ".
           02 PIC X(25) VALUE "ÄÂÄNombreÄÄÄÄÄÄÄÄÄÄÄÄÄÂP¿".
           02 PIC X(38) VALUE "ÀÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄ".
           02 PIC X(25) VALUE "ÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÙ".
       01 CABEZARASIMP REDEFINES DatosCab.
           02 CAB PIC X(63) OCCURS 2.
       01 LINDETALLE.
           02 PIC X VALUE "³".
           02 PNFAC PIC X(8).
           02 PIC X VALUE "³".
           02 PDIA PIC ZZ.
           02 PIC X VALUE "/".
           02 PMES PIC ZZ.
           02 PIC X VALUE "/".
           02 PANO PIC ZZZZ.
           02 PIC X VALUE "³".
           02 PPASTA PIC ZZZZZZZZ.
           02 PIC X VALUE "³".
           02 PNIF PIC X(9).
           02 PIC X VALUE "³".
           02 PNOMBRE PIC X(20).
           02 PIC X VALUE "³".
           02 PPAGADA PIC X.
           02 PIC X VALUE "³".
       01 TT.
           02 PIC X(20) VALUE "              SUMA: ".
           02 PTOTALES PIC ZZZZZZZZZ.
           02 PIC X(20) VALUE SPACES.
           02 PIC X(8) VALUE "PAGINA:".
           02 PPAGINA PIC ZZZ.
           
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
           PERFORM INIC
           PERFORM CONSULTAS UNTIL TECLAF = 27.
           CLOSE FACTURA
           CLOSE CLIENTES
           EXIT PROGRAM.
       INIC.
           OPEN I-O FACTURA
           OPEN I-O CLIENTES
           MOVE VENTANA1 TO VENT
           CALL "VENT2.COB" USING VENT
           MOVE VENTANA2 TO VENT
           CALL "VENT2.COB" USING VENT
           MOVE MasterPos TO DONDE(1)
           COMPUTE DONDE(2) = 1 + DONDE(1)
           DISPLAY TITULO LINE 3 POSITION DONDE(2)
           MOVE 0 TO TECLAF
           MOVE 4 TO CONTL(1)
           MOVE 1 TO PUNTERO
           MOVE 1 TO PtroPila
           MOVE 0 TO FAC(PtroPila)
           MOVE 00000000 TO FECHA(PtroPila , 1)
           MOVE 99999999 TO FECHA(PtroPila , 2)
           MOVE "T" TO PAGADA(PtroPila)
           PERFORM leer.
       CONSULTAS.
           MOVE 0 TO ESTADO
           CALL "CUROFF"
           MOVE 6 TO I
           PERFORM MENSAJE
           ACCEPT TECLA  LINE 25 POSITION 1 NO BEEP
           ON EXCEPTION TECLAF PERFORM TECLASFUNCION
           END-ACCEPT
           DISPLAY "Ô" LINE 25 POSITION 1 CONTROL COLORA
           IF TECLAF = 27
               MOVE 1 TO NUMID
               CALL "MENSAJE.COB" USING NUMID
               IF NUMID NOT = 1 MOVE 0 TO TECLAF END-IF
           END-IF.
       TECLASFUNCION.
           DISPLAY LD(PUNTERO) LINE CONT POSITION DONDE(2)
           MOVE 1 TO ESTADO
           IF TECLAF = 2
              PERFORM ModosBusqueda
              IF TECLAF  = 27
                  MOVE 0 TO TECLAF
                 ELSE
                  MOVE 1 TO PtroPila
                  PERFORM BUSCAR
              END-IF
           END-IF
           IF TECLAF = 3  AND TNFAC(PUNTERO)  NOT = ALL  " "
              PERFORM INIBUSQUEDA
              PERFORM BuscarEmpresa  UNTIL  TECLAF = 27
              CALL "CURON"
              CALL "RECUP"
              IF TECLAF  = 27
                  MOVE 0 TO TECLAF
              END-IF
           END-IF
           IF TECLAF = 4 AND  TNFAC(PUNTERO)  NOT  = ALL "  "
              PERFORM  MARCAR
           END-IF
           IF TECLAF = 5 AND  TNFAC(PUNTERO)  NOT  = ALL "  "
           AND FAC(1) NOT = ALL " "
              MOVE "00" TO ERRORIMP
              OPEN OUTPUT LISTADO
              MOVE "N"  TO BIEN
              WRITE LINEAIMP FROM SPACES
              if errorimp = "00"
                PERFORM IMPRIMIR 
              end-if
              CLOSE LISTADO
           END-IF
           IF TECLAF = 52
              SUBTRACT 1 FROM PUNTERO
              SUBTRACT 1 FROM CONT
           END-IF.
           IF TECLAF = 53
              ADD 1 TO PUNTERO
              ADD 1 TO CONT
           END-IF.
           IF TECLAF = 67 AND PtroPila NOT = 1
               SUBTRACT 1 FROM PtroPila
               PERFORM BUSCAR
           END-IF.
           IF TECLAF = 68  AND PtroPila NOT = MAXLINEAS
               COMPUTE J =  PTRO(2) - 1
               IF  PTRO(1) NOT = J
                 MOVE PAGADA(PtroPila) TO  TPAGADA(1)
                 ADD 1 TO PtroPila
                 MOVE TDIA(1)  TO DIA(PtroPila , 1)
                 MOVE TMES(1)  TO MES(PtroPila,  1)
                 MOVE TANO(1)  TO ANO(PtroPila,  1)
                 MOVE TNFAC(J) TO FAC(PtroPila)
                 MOVE TDIA(J)  TO DIA(PtroPila , 2)
                 MOVE TMES(J)  TO MES(PtroPila , 2)
                 MOVE TANO(J)  TO ANO(PtroPila , 2)
                 MOVE TPAGADA(1) TO PAGADA(PtroPila)
                 PERFORM BUSCAR
               END-IF
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
      *****************************************************************
      *                                                               *
      *****************************************************************
       BorrarTabla.
           PERFORM BucleBorrar VARYING I FROM 1 BY 1
             UNTIL  I = MaxLineas
           PERFORM BorrarPantalla VARYING I FROM CONTL(1) BY 1
             UNTIL I > CONTL(2).
       BucleBorrar.
           MOVE ALL " " TO TNFAC(I)
           MOVE ALL " " TO TDIA(I)
           MOVE ALL " " TO TMES(I)
           MOVE ALL " " TO TANO(I)
           MOVE ALL " " TO TPASTA(I)
           MOVE ALL " " TO TPAGADA(I).
       BorrarPantalla.
           DISPLAY BLANCOS LINE I POSITION DONDE(2) REVERSE.
      *****************************************************************
      *                                                               *
      *****************************************************************
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
           IF TNFAC(PUNTERO) NOT = ALL " "
                DISPLAY LD(PUNTERO) LINE CONT POSITION DONDE(2)
                ADD 1 TO CONT
                ADD 1 TO PUNTERO
                IF CONT = MaxLineas MOVE "S" TO BIEN END-IF
             ELSE
               MOVE "S" TO BIEN
               IF PUNTERO = 1
                DISPLAY LD(PUNTERO) LINE CONT POSITION DONDE(2)
                ADD 1 TO CONT
                ADD 1 TO PUNTERO
               END-IF
           END-IF.
      *****************************************************************
      *                                                               *
      *****************************************************************
       INIBUSQUEDA.
           MOVE VENTANA3 TO VENTANA
           CALL "VENTANA.COB" USING VENTANA
           CALL "CUROFF.EXE"
           DISPLAY "¿DATOS DETALLADOS DE LA FACTURAÚ" LINE 7 POSITION 25
                REVERSE
           DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" LINE 8 POSITION 25
                REVERSE
           DISPLAY "EMPRESA:"    LINE 9 POSITION  23 REVERSE
           DISPLAY "GIF:"        LINE 10 POSITION 23 REVERSE
           DISPLAY "MESA:"       LINE 11 POSITION 23 REVERSE
           DISPLAY "DIA:"        LINE 11 POSITION 46 REVERSE
           DISPLAY "N-FACTURA:"  LINE 12 POSITION 23 REVERSE
           DISPLAY "TOTAL:"      LINE 12 POSITION 46 REVERSE.
       BuscarEmpresa.
           MOVE TNFAC(PUNTERO) TO NFAC
           READ  FACTURA
                INVALID KEY
                   MOVE  12 TO I
                   PERFORM MENSAJE
                   PERFORM ALTO
                NOT INVALID KEY
                   DISPLAY FMESA   LINE 11 POSITION 28
                   DISPLAY TFECHA(PUNTERO)  LINE 11 POSITION 50
                   DISPLAY NFAC    LINE 12 POSITION 33
                   DISPLAY FPASTA  LINE 12 POSITION 52
                   DISPLAY FGIF    LINE 10 POSITION 27
                   IF FPAGADA = "*"
                      DISPLAY " FACTURA SIN PAGAR" LINE 14 POSITION 32
                          BLINK
                    ELSE
                      DISPLAY "  FACTURA PAGADA  " LINE 14 POSITION 32
                         REVERSE
                   END-IF
           END-READ
           IF FGIF NOT =  ALL " "
                MOVE FGIF  TO CGIF
                READ CLIENTES
                  INVALID KEY
                     DISPLAY "FACTURA SIN NIF              "
                     LINE 9  POSITION 32  REVERSE
                     MOVE  11 TO I
                     PERFORM ALTO
                     PERFORM MENSAJE
                  NOT INVALID KEY
                    DISPLAY CNOMBRE LINE 9  POSITION 32
               END-READ
             ELSE
                DISPLAY "FACTURA SIN NIF                   "
                LINE 9  POSITION 32  REVERSE
           END-IF
           MOVE 7 TO I
           PERFORM MENSAJE
           ACCEPT TECLA  LINE 25 POSITION 1 NO BEEP
              ON EXCEPTION TECLAF PERFORM HANDLERFUNC
           END-ACCEPT
           DISPLAY "Ô" LINE 25 POSITION 1 CONTROL COLORA.
       HANDLERFUNC.
           IF TECLAF = 4 AND  TNFAC(PUNTERO)  NOT  = ALL "  "
              PERFORM  MARCAR
           END-IF
           IF TECLAF = 52
              SUBTRACT 1 FROM PUNTERO
              SUBTRACT 1 FROM CONT
           END-IF.
           IF TECLAF = 53
              ADD 1 TO PUNTERO
              ADD 1 TO CONT
           END-IF.
           IF PUNTERO = PTRO(2)
               MOVE PTRO(1) TO PUNTERO
               MOVE CONTL(1) TO CONT
           END-IF
           IF PUNTERO < PTRO(1)
               MOVE PTRO(2) TO PUNTERO
               SUBTRACT 1 FROM PUNTERO
               MOVE CONTL(2) TO CONT
               SUBTRACT 1 FROM CONT
           END-IF.
      *****************************************************************
      *                                                               *
      *****************************************************************
       ModosBusqueda.
           MOVE 10211562000041 TO VENTANA
           CALL "VENTANA.COB" USING VENTANA
           DISPLAY "BUSQUEDA RAPIDA POR CAMPOS"
             LINE 10 POSITION 28 REVERSE
           DISPLAY "N DE FACTURA:" LINE 12 POSITION 23 REVERSE
           DISPLAY "PAGADAS(T/P/N):" LINE 12 POSITION 45 REVERSE
           DISPLAY " DESDE:  /  /    HASTA:  /  /  "
             LINE 14 POSITION 23 REVERSE
           ACCEPT FECHASYS FROM DATE END-ACCEPT
           MOVE 2 TO I
           PERFORM MENSAJE
           ACCEPT FAC(1) LINE 12 POSITION 36 NO BEEP
             UPDATE TAB ON EXCEPTION TECLAF CONTINUE
           END-ACCEPT
           MOVE 3 TO I
           PERFORM MENSAJE
           MOVE "T" TO PAGADA(PtroPila)
           PERFORM WITH TEST AFTER UNTIL PAGADA(1) = "P" OR "N" OR "T"
             OR "p" OR "n" OR "t" OR TECLAF = 27
              ACCEPT PAGADA(1) LINE 12 POSITION 60 NO BEEP
                UPDATE  ON EXCEPTION TECLAF CONTINUE
              END-ACCEPT
           END-PERFORM
           MOVE 1 TO K
           MOVE 4 TO I
           PERFORM MENSAJE
           PERFORM VerDias WITH TEST AFTER UNTIL BIEN = "B"
             OR TECLAF  = 27
           MOVE 2 TO K
           MOVE 4 TO I
           PERFORM MENSAJE
           PERFORM VerDias WITH TEST AFTER UNTIL BIEN = "B"
             OR TECLAF  = 27
           CALL "RECUP".
       VerDias.
           IF K = 1
              MOVE 30 TO J
            ELSE
              MOVE 46 TO J
           END-IF
           MOVE DIASYS TO DIA(1 , K)
           MOVE MESSYS TO MES(1 , K)
           MOVE ANOSYS TO ANO(1 , K)
           IF TECLAF NOT = 27
             ACCEPT DIA(1 , K) LINE 14 POSITION J UPDATE NO BEEP TAB
                 ON EXCEPTION TECLAF CONTINUE
             END-ACCEPT
           END-IF
           ADD 3 TO J
           IF TECLAF NOT = 27
             ACCEPT MES(1 , K) LINE 14 POSITION J UPDATE NO BEEP TAB
                ON EXCEPTION TECLAF CONTINUE
             END-ACCEPT
           END-IF
           ADD 3 TO J
           IF TECLAF NOT = 27
             ACCEPT ANO(1 , K) LINE 14 POSITION J UPDATE NO BEEP TAB
                   ON EXCEPTION TECLAF CONTINUE
             END-ACCEPT
           END-IF
           MOVE "N" TO BIEN
           IF MES(1 , K) = 1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12
              IF DIA(1 , K) > 0 AND < 32
                   MOVE "B" TO BIEN
                 ELSE
                   MOVE "N" TO BIEN
              END-IF
            ELSE
              IF MES(1 , K) = 2 OR 4 OR 6 OR 9 OR 11
                 IF DIA(1 , K) > 0 AND < 31
                   MOVE "B" TO BIEN
                 ELSE
                   MOVE "N" TO BIEN
                 END-IF
              END-IF
           END-IF
           IF BIEN = "N"
                 MOVE 5 TO I
                 PERFORM MENSAJE
               ELSE
                 MOVE 1 TO I
                 PERFORM MENSAJE
           END-IF.
       MENSAJE.
           DISPLAY LM(I) LINE 24 POSITION 2 REVERSE.
       ALTO.
           ACCEPT TECLA  LINE 25 POSITION 1 NO BEEP
              ON EXCEPTION TECLAF CONTINUE
           END-ACCEPT
           DISPLAY "Ô" LINE 25 POSITION 1 CONTROL COLORA.
      *****************************************************************
      *                                                               *
      *****************************************************************
       BUSCAR.
           IF FAC(PtroPila) =  0
                 MOVE FECHA(PtroPila , 1) TO FFECHA
                 START FACTURA KEY IS NOT < FFECHA
                    INVALID KEY
                       MOVE 13 TO I
                       PERFORM MENSAJE
                       PERFORM ALTO
                     NOT INVALID KEY
                       PERFORM BorrarTabla
                       MOVE 1 TO PUNTERO
                       MOVE "B" TO BIEN
                       MOVE 0 TO NFAC
                       PERFORM CARGAR UNTIL BIEN = "N"
                       MOVE 1 TO PUNTERO
                       PERFORM AjustarTabla
                 END-START
             ELSE
                 MOVE FAC(PtroPila) TO NFAC
                 START FACTURA KEY IS NOT < NFAC
                    INVALID KEY
                       MOVE 14 TO I
                       PERFORM MENSAJE
                       PERFORM ALTO
                     NOT INVALID KEY
                       PERFORM LEER
                 END-START
           END-IF.
      *****************************************************************
      *                                                               *
      *****************************************************************
       LEER.
           PERFORM BorrarTabla
           MOVE 1 TO PUNTERO
           MOVE "B" TO BIEN
           MOVE FAC(PtroPila) TO NFAC
           PERFORM CARGAR UNTIL BIEN = "N"
           MOVE 1 TO PUNTERO
           PERFORM AjustarTabla.
       CARGAR.
           READ FACTURA NEXT AT END MOVE "N" TO BIEN
           END-READ.
            IF BIEN = "B" AND PUNTERO NOT = MAXLINEAS AND NFAC
            NOT = 0 AND FFECHA NOT < FECHA(PtroPila , 1)
            AND FFECHA  NOT > FECHA(PtroPila , 2)
              IF (PAGADA(PtroPila) = "T" OR "t")
                 PERFORM MOVER
                 ADD 1 TO PUNTERO
              END-IF
              IF (PAGADA(PtroPila) = "N" OR "n")
                 AND FPAGADA = "*"
                     PERFORM MOVER
                     ADD 1 TO PUNTERO
              END-IF
              IF (PAGADA(PtroPila) = "P" OR "p")
               AND FPAGADA = " "
                   PERFORM MOVER
                   ADD 1 TO PUNTERO
              END-IF
            END-IF.
       MOVER.
           MOVE NFAC     TO TNFAC(Puntero)
           MOVE FDIA     TO TDIA(Puntero)
           MOVE FMES     TO TMES(Puntero)
           MOVE FANO     TO TANO(Puntero)
           MOVE FPASTA   TO TPASTA(Puntero)
           MOVE FPAGADA  TO TPAGADA(Puntero).

       MARCAR.
           MOVE TNFAC(PUNTERO) TO NFAC
           READ  FACTURA
                INVALID KEY
                   MOVE  12 TO I
                   PERFORM MENSAJE
                   PERFORM ALTO
                NOT INVALID KEY
                   IF FPAGADA = "*"
                      MOVE  " " TO FPAGADA TPAGADA(PUNTERO)
                    ELSE
                      MOVE "*"  TO FPAGADA TPAGADA(PUNTERO)
                   END-IF
           END-READ
           REWRITE REG-FAC END-REWRITE.
      *****************************************************************
      *                                                               *
      *****************************************************************
       IMPRIMIR.
           MOVE 0 TO  TOTALES
           MOVE 1 TO LINEA
           MOVE 1 TO CONTPAG
           MOVE FAC(1) TO NFAC
           MOVE "B" TO BIEN
           START FACTURA KEY IS NOT < NFAC
                    INVALID KEY
                       MOVE 14 TO I
                       PERFORM MENSAJE
                       PERFORM ALTO
                       MOVE "N" TO BIEN
                     NOT INVALID KEY
                       CONTINUE
           END-START
           MOVE 1 TO I
           PERFORM IMPCAB
           PERFORM LEERIMP UNTIL BIEN = "N".
           IF LINEA NOT = 1
              MOVE 2 TO I
              PERFORM IMPCAB
              MOVE TOTALES TO PTOTALES
              MOVE CONTPAG TO PPAGINA
              WRITE LINEAIMP FROM TT
              MOVE SPACES TO LINEAIMP
              WRITE LINEAIMP BEFORE PAGE
           END-IF.
       IMPCAB.
           WRITE LINEAIMP FROM CAB(I) BEFORE  ADVANCING  1 LINE.
       LEERIMP.
           READ FACTURA NEXT AT END MOVE "N" TO BIEN
           END-READ.
            IF BIEN = "B" AND PUNTERO NOT = MAXLINEAS AND NFAC
            NOT = 0 AND FFECHA NOT < FECHA(1 , 1)
            AND FFECHA  NOT > FECHA(1 , 2)
              IF (PAGADA(1) = "T" OR "t")
                 PERFORM MARTILLEAR
              END-IF
              IF (PAGADA(1) = "N" OR "n")
                 AND FPAGADA = "*"
                     PERFORM MARTILLEAR
              END-IF
              IF (PAGADA(1) = "P" OR "p")
               AND FPAGADA = " "
                   PERFORM MARTILLEAR
              END-IF
            END-IF.
       MARTILLEAR.
           MOVE NFAC     TO PNFAC
           MOVE FDIA     TO PDIA
           MOVE FMES     TO PMES
           MOVE FANO     TO PANO
           MOVE FPASTA   TO PPASTA
           MOVE FPAGADA  TO PPAGADA
           MOVE FGIF     TO PNIF
           IF FGIF NOT =  ALL " "
                MOVE FGIF  TO CGIF
                READ CLIENTES
                  INVALID KEY
                    MOVE "CLIENTE NO FICNADO" TO PNOMBRE
                  NOT INVALID KEY
                    MOVE CNOMBRE TO PNOMBRE
               END-READ
             ELSE
               MOVE ALL " " TO PNIF
           END-IF
           COMPUTE TOTALES = TOTALES + FPASTA
           WRITE LINEAIMP FROM LINDETALLE BEFORE  ADVANCING  1 LINE
           ADD 1 TO LINEA
           IF LINEA = MaxIMP AND BIEN = "B"
              MOVE 2 TO I
              PERFORM IMPCAB
              MOVE TOTALES TO PTOTALES
              MOVE CONTPAG TO PPAGINA
              WRITE LINEAIMP FROM TT
              MOVE SPACES TO LINEAIMP
              WRITE LINEAIMP BEFORE PAGE
              MOVE 1 TO I
              PERFORM IMPCAB
              MOVE 1 TO LINEA
              ADD 1 TO CONTPAG
           END-IF.

