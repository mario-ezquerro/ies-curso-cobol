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
           ACCESS MODE IS RANDOM
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
       FD LISTADO LABEL RECORD STANDARD.
       01 LINEAIMP PIC X(80).
       WORKING-STORAGE SECTION.
       01 FechaSys.
           02 AnoSys PIC 99.
           02 MesSys PIC 99.
           02 DiaSys PIC 99.
       01 CamposFecha.
           02 FECHA OCCURS 2.
              03 DIA PIC 99.
              03 MES PIC 99.
              03 ANO PIC 99.
       01 VarConstantes.
           02 MAXLINEAS PIC 99 VALUE 20.
           02 MaxPila   PIC 99 VALUE 20.
           02 MasterPos PIC 99 VALUE 18.
           02 MaxIMP    PIC 99 VALUE 11.
           02 VENTANA1  PIC 9(16) VALUE 0218216012000000.
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
           02 PASO   PIC X.
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
           02 MODO PIC 99.
           02 CUAL PIC 99.
           02 PAGINA PIC 999.
           02 ERRORIMP PIC XX.
       01 Registros.
          02 AUX     PIC X(9).
          02 VENT    PIC 9(16).
          02 VENTANA.
               03 TOPL    PIC 99.
               03 TOPP   PIC 99.
               03 DOWNL   PIC 99.
               03 DOWNP   PIC 99.
               03 ANCHO   PIC 99.
               03 ALTO    PIC 99.
               03 TIPO    PIC 9.
               03 SOMBRA  PIC 9.
          02 FAC    PIC 9(8).
          02 PAGADA PIC X.
          02 PILA OCCURS 1 TO 50 DEPENDING ON MaxPila.
            03 NOMBRE PIC X(29).
            03 GIF    PIC X(9).
            03 CIUDAD PIC X(15).
       01 DATOSPAN.
            02 LD OCCURS 30.
               03 FILLER  PIC X VALUE "³".
               03 TGIF    PIC  X(9).
               03 FILLER  PIC X VALUE "³".
               03 TNOMBRE PIC X(29).
               03 FILLER  PIC X VALUE "³".
       01 DATOSMEN.
           02 FILLER PIC X(78) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "   DEME EL NUMERO DE FACTURA A ".
           02 FILLER PIC X(32) VALUE "PARTIR DE LA QUE QUIERE BUSCA".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE " SALIR:ESC F2:BUSQUEDAS F3:ALTAS".
           02 FILLER PIC X(32) VALUE " F4:DETALLES PAGAV PAGRE F5:IMPR".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE " ESTE CLIENTE YA ESTA FICHADO   ".
           02 FILLER PIC X(32) VALUE ALL " ".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "   VAMOS A METER OTRO CLIENTE".
           02 FILLER PIC X(32) VALUE ALL " ".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE " ATENCION ERROR DE ESCRITURA".
           02 FILLER PIC X(32) VALUE " INTENTELO OTRA VEZ (HABER SI HA".
           02 FILLER PIC X(14) VALUE "Y SUERTE) ".
           02 FILLER PIC X(32) VALUE " CLIENTE EN EL DISCO            ".
           02 FILLER PIC X(32) VALUE ALL " ".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE "  GRABACION SUPER SATISFACTORIA".
           02 FILLER PIC X(32) VALUE ALL " ".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE " ATENCION DEME TODOS LOS CAMPOS ".
           02 FILLER PIC X(32) VALUE "PARA CANCELAR PULSE ESC         ".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE " BUSQUEDA POR EL GIF DE LA EMP".
           02 FILLER PIC X(32) VALUE "RESA O DEL PARTICULAR O ESC".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE " BUSQUEDA POR NOMBRES O ESC".
           02 FILLER PIC X(32) VALUE ALL " ".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE " LISTADO POR CIUDADES, PUEBLOS.".
           02 FILLER PIC X(32) VALUE ".....  ESC".
           02 FILLER PIC X(14) VALUE ALL " ".
           02 FILLER PIC X(32) VALUE " EL GIF INTRODUCIDO NO EXIXTE  ".
           02 FILLER PIC X(32) VALUE "...".
           02 FILLER PIC X(14) VALUE " ".
           02 FILLER PIC X(32) VALUE " EL NOMBRE INTRODUCIDO NOE EXIS".
           02 FILLER PIC X(32) VALUE "TE.. ".
           02 FILLER PIC X(14) VALUE " ".
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
           02 LM PIC X(78) OCCURS 18.
       01 DATOSMENU1.
          02 FILLER PIC 9(4) VALUE 0308.
          02 FILLER PIC X(08) VALUE "10NIF".
          02 FILLER PIC X(08) VALUE "11NOMBRE".
          02 FILLER PIC X(08) VALUE "12CIUDAD".
       01 HANDLEMENU REDEFINES DATOSMENU1.
          02 VMENUS OCCURS 1.
              03 NUMEROITENS PIC 99.
              03 ANCHOMENU PIC 99.
              03 VVMENUS OCCURS 3.
                  04 ASMENU PIC 99.
                  04 NAMEITEN PIC X(6).
       01 TITULO.
           02 FILLER PIC X(32) VALUE "ÚÄÄÄNIFÄÄÄÂÄNOMBREÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           02 FILLER PIC X(9)  VALUE "ÄÄÄÄÄÄÄÄ¿".
       01 BarraUltima.
           02 FILLER PIC X(32) VALUE "ÀÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           02 FILLER PIC X(9)  VALUE "ÄÄÄÄÄÄÄÄÙ".
       01 BLANCOS.
           02 FILLER PIC X(41) VALUE ALL " ".
       01 Colores.
           02 ColorA PIC X(27) VALUE "FCOLOR=WHITE, BCOLOR=BLUE".
           02 ColorN PIC X(27) VALUE "FCOLOR=WHITE, BCOLOR=BLAK".
           02 ColorV PIC X(27) VALUE "FCOLOR=WHITE, BCOLOR=BLUE".
       01 LINDET1.
          02 FILLER PIC X VALUE "³".
          02 PGIF PIC X(9).
          02 FILLER PIC X VALUE "³".
          02 PNOMBRE PIC X(29).
          02 FILLER PIC X VALUE "³".
          02 PTELEFONO PIC ZZZZZZZZZ.
          02 FILLER PIC X VALUE "³".
          02 PCP PIC ZZZZZZ.
          02 FILLER PIC X VALUE "³".
       01 LINDET2.
          02 FILLER PIC X VALUE "³".
          02 PDIRECCION PIC X(40).
          02 FILLER PIC X VALUE "³".
          02 PNUMERO PIC ZZZZZ.
          02 FILLER PIC X VALUE "³".
          02 PCIUDAD PIC X(15).
          02 FILLER PIC X VALUE "³".
          02 PPROVIN PIC X(15).
          02 FILLER PIC X VALUE "³".
       01 DatosCAB.
          02 FILLER PIC X(33) VALUE "ÚÄÄÄNIFÄÄÄÂÄNOMBREÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
          02 FILLER PIC X(33) VALUE "ÄÄÄÄÄÄÄÂTELEFONOÄÂCPÄÄÄÄ¿        ".
          02 FILLER PIC X(14) VALUE ALL " ".
          02 FILLER PIC X(33) VALUE "ÃÄÄÄDIREECCIONÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
          02 FILLER PIC X(33) VALUE "ÄÄÄÄÄÄÄÁÂNÄÄÄÄÂÄCIUDADÄÄÁÄÄÄÄÄÂÄP".
          02 FILLER PIC X(14) VALUE "ROVINCIAÄÄÄÄÄ¿".
          02 FILLER PIC X(33) VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
          02 FILLER PIC X(33) VALUE "ÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄ".
          02 FILLER PIC X(14) VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
       01 TCA REDEFINES DatosCAB.
          02 CAB PIC X(80) OCCURS 3.
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
           MOVE 1 TO MODO
           MOVE 0 TO TECLAF
           MOVE 4 TO CONTL(1)
           MOVE " " TO CNOMBRE
           PERFORM LLENAR
           MOVE CGIF    TO GIF(1)
           MOVE CNOMBRE TO NOMBRE(1)
           MOVE CCIUDAD TO CIUDAD(1).
       LLENAR.
           MOVE 1 TO CUAL
           MOVE "N" TO BIEN
           MOVE 1 TO PUNTERO
           MOVE "1" TO CGIF
           PERFORM CARGARGIF UNTIL BIEN = "B".
           MOVE 1 TO PUNTERO
           PERFORM AjustarTabla.
       CONSULTAS.
           MOVE 0 TO ESTADO
           CALL "CUROFF"
           MOVE 3 TO I
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
              PERFORM BuscarEmpresa
           END-IF
           IF TECLAF = 3
              PERFORM AltasEmpresa
              START CLIENTES KEY IS NOT < CNOMBRE
              END-START
              PERFORM BorrarTabla
              PERFORM LLENAR
           END-IF
           IF TECLAF = 4 AND TGIF(PUNTERO) NOT = ALL " "
              PERFORM DetalleEmpresa
              START CLIENTES KEY IS NOT < CNOMBRE
              END-START
              PERFORM LLENAR
           END-IF
           IF TECLAF = 5  AND  GIF(1) NOT = ALL  " "
              MOVE "00" TO ERRORIMP
              OPEN OUTPUT LISTADO
              WRITE LINEAIMP FROM SPACES
              IF  ERRORIMP = "00"
                PERFORM IMPRIMIR
              END-IF
              CLOSE  LISTADO
              START CLIENTES KEY IS NOT < CNOMBRE
              END-START
              PERFORM LLENAR
           END-IF
           IF TECLAF = 52
              SUBTRACT 1 FROM PUNTERO
              SUBTRACT 1 FROM CONT
           END-IF.
           IF TECLAF = 53
              ADD 1 TO PUNTERO
              ADD 1 TO CONT
           END-IF.
           IF TECLAF = 67 AND CUAL NOT = 1  AND CUAL NOT = 0
               COMPUTE K = CUAL + 1
               PERFORM WITH TEST AFTER
               UNTIL PILA(CUAL) NOT = PILA(K)  OR CUAL = 1
                  SUBTRACT 1 FROM K
                  SUBTRACT 1 FROM CUAL
               END-PERFORM
               MOVE GIF(CUAL) TO AUX
               EVALUATE MODO
                 WHEN 1
                    MOVE GIF(CUAL) TO CGIF
                 WHEN 2
                    MOVE NOMBRE(CUAL) TO CNOMBRE
                 WHEN 3
                    MOVE CIUDAD(CUAL) TO CCIUDAD
               END-EVALUATE
               PERFORM PAG
           END-IF.
           IF TECLAF = 68 AND CUAL NOT = MaxPila  AND CUAL NOT = 0
               MOVE PTRO(1)    TO K
               MOVE TGIF(K)    TO CGIF
               READ CLIENTES
                  INVALID KEY
                     MOVE 16 TO I
                     PERFORM MENSAJE
                     PERFORM ALTO
                  NOT INVALID KEY
                     MOVE CGIF    TO GIF(CUAL)
                     MOVE CNOMBRE TO NOMBRE(CUAL)
                     MOVE CCIUDAD TO CIUDAD(CUAL)
               END-READ
               ADD 1 TO CUAL
               MOVE CGIF    TO GIF(CUAL)
               MOVE CNOMBRE TO NOMBRE(CUAL)
               MOVE CCIUDAD TO CIUDAD(CUAL)
               COMPUTE k = PTRO(2) - 1
               MOVE TGIF(K)  TO CGIF
               MOVE TGIF(K)  TO AUX
               READ CLIENTES
                   INVALID KEY
                     MOVE 16 TO I
                     PERFORM MENSAJE
                     PERFORM ALTO
                   NOT INVALID KEY
                    PERFORM PAG
               END-READ
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
       PAG.
           PERFORM BorrarTabla
           EVALUATE MODO
              WHEN 1
                READ CLIENTES
                    INVALID KEY
                      MOVE "N" TO BIEN
                      MOVE 13 TO I
                      PERFORM MENSAJE
                      PERFORM ALTO
                    NOT INVALID KEY CONTINUE
                END-READ
                MOVE "N" TO BIEN
                MOVE 1 TO PUNTERO
                PERFORM CARGARGIF UNTIL BIEN = "B"
              WHEN 2
                  START CLIENTES KEY IS = CNOMBRE
                    INVALID KEY
                      MOVE "N" TO BIEN
                      MOVE 14 TO I
                      PERFORM MENSAJE
                      PERFORM ALTO
                    NOT INVALID KEY CONTINUE
                  END-START
                  MOVE "N" TO BIEN
                  MOVE 1 TO PUNTERO
                  MOVE "N" TO PASO
                  PERFORM CARGARNOMBRE UNTIL BIEN = "B"
              WHEN 3
                  START CLIENTES KEY IS = CCIUDAD
                    INVALID KEY
                      MOVE "N" TO BIEN
                      MOVE 15 TO I
                      PERFORM MENSAJE
                      PERFORM ALTO
                    NOT INVALID KEY CONTINUE
                  END-START
                  MOVE "N" TO BIEN
                  MOVE 1 TO PUNTERO
                  MOVE "N" TO PASO
                  PERFORM CARGARCIUDAD UNTIL BIEN = "B"
           END-EVALUATE
           MOVE 1 TO PUNTERO
           PERFORM AjustarTabla.
      *****************************************************************
      *                                                               *
      *****************************************************************
       BorrarTabla.
           PERFORM BucleBorrar VARYING I FROM 1 BY 1
             UNTIL  I = MaxLineas
           PERFORM BorrarPantalla VARYING I FROM CONTL(1) BY 1
             UNTIL I > CONTL(2).
       BucleBorrar.
           MOVE ALL " " TO TGIF(I)
           MOVE ALL " " TO TNOMBRE(I).
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
           IF TGIF(PUNTERO) NOT = ALL " "
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
      *****************************************************************
      *                                                               *
      *****************************************************************
       BuscarEmpresa.
           MOVE VENTANA5 TO VENTANA
           CALL "VENTANA.COB" USING VENTANA
           CALL "CUROFF"
           MOVE 1 TO MODO
           COMPUTE POSICION = TOPP + 3
           MOVE TOPL  TO LINEA
           DISPLAY "BUSQUEDA POR:" LINE LINEA POSITION POSICION REVERSE
           PERFORM PreOpci UNTIL TECLAF = 13 OR 27
           CALL "CURON"
           CALL "RECUP"
           IF TECLAF = 13
             PERFORM BorrarTabla
             PERFORM VARYING I FROM 1 BY 1 UNTIL I > MaxPila
               MOVE ALL " " TO PILA(I)
             END-PERFORM
             MOVE 1 TO CUAL
             EVALUATE MODO
               WHEN 1
                   PERFORM BUSGIF
               WHEN 2
                   PERFORM BUSNOM
               WHEN 3
                   PERFORM BUSCIU
             END-EVALUATE
             MOVE 1 TO PUNTERO
             PERFORM AjustarTabla
           END-IF
           IF TECLAF = 27
              MOVE 0 TO TECLAF
              MOVE 0 TO CUAL
           END-IF.
       BUSGIF.
           MOVE VENTANA6 TO VENTANA
           CALL "VENTANA.COB" USING VENTANA
           MOVE TOPL TO LINEA
           COMPUTE POSICION = TOPP + 1
           DISPLAY "GIF:" LINE LINEA POSITION POSICION REVERSE
           COMPUTE POSICION = POSICION + 6
           MOVE "N" TO BIEN
           PERFORM UNTIL BIEN = "S" OR TECLAF = 27
             ACCEPT GIF(CUAL) LINE LINEA POSITION POSICION
               NO BEEP TAB UPDATE ON EXCEPTION TECLAF CONTINUE
             END-ACCEPT
             IF TECLAF  = 13
                MOVE GIF(CUAL) TO CGIF
                READ CLIENTES
                    INVALID KEY
                      MOVE "N" TO BIEN
                      MOVE 13 TO I
                      PERFORM MENSAJE
                      PERFORM ALTO
                    NOT INVALID KEY
                      MOVE "S" TO BIEN
                END-READ
             END-IF
           END-PERFORM
           CALL "RECUP"
           IF TECLAF NOT = 27
              MOVE "N" TO BIEN
              MOVE 1 TO PUNTERO
              PERFORM CARGARGIF UNTIL BIEN = "B"
             ELSE
              MOVE 0 TO CUAL
           END-IF.
       BUSNOM.
           MOVE VENTANA6 TO VENTANA
           CALL "VENTANA.COB" USING VENTANA
           MOVE TOPL TO LINEA
           COMPUTE POSICION = TOPP + 1
           DISPLAY "NOMBRE:" LINE LINEA POSITION POSICION REVERSE
           COMPUTE POSICION = POSICION + 9
           MOVE "N" TO BIEN
           PERFORM UNTIL BIEN = "S" OR TECLAF = 27
             ACCEPT NOMBRE(CUAL) LINE LINEA POSITION POSICION
               NO BEEP TAB UPDATE ON EXCEPTION TECLAF CONTINUE
             END-ACCEPT
             IF TECLAF  = 13
                MOVE NOMBRE(CUAL) TO CNOMBRE
                START CLIENTES KEY IS NOT < CNOMBRE
                    INVALID KEY
                      MOVE "N" TO BIEN
                      MOVE 14 TO I
                      PERFORM MENSAJE
                      PERFORM ALTO
                    NOT INVALID KEY
                      MOVE "S" TO BIEN
                END-START
             END-IF
            END-PERFORM
            CALL "RECUP"
            IF TECLAF NOT = 27
              MOVE "N" TO BIEN
              MOVE 1 TO PUNTERO
              MOVE 1 TO K
              MOVE "S" TO PASO
              PERFORM CARGARNOMBRE UNTIL BIEN = "B"
             ELSE
              MOVE 0 TO CUAL
            END-IF.
       BUSCIU.
           MOVE VENTANA6 TO VENTANA
           CALL "VENTANA.COB" USING VENTANA
           MOVE TOPL TO LINEA
           COMPUTE POSICION = TOPP + 1
           DISPLAY "CIUDAD:" LINE LINEA POSITION POSICION REVERSE
           COMPUTE POSICION = POSICION + 9
           MOVE "N" TO BIEN
           PERFORM UNTIL BIEN = "S" OR TECLAF = 27
             ACCEPT CIUDAD(CUAL) LINE LINEA POSITION POSICION
               NO BEEP TAB UPDATE ON EXCEPTION TECLAF CONTINUE
             END-ACCEPT
             IF TECLAF  = 13
                MOVE CIUDAD(CUAL) TO CCIUDAD
                START CLIENTES KEY IS = CCIUDAD
                    INVALID KEY
                      MOVE "N" TO BIEN
                      MOVE 15 TO I
                      PERFORM MENSAJE
                      PERFORM ALTO
                    NOT INVALID KEY
                      MOVE "S" TO BIEN
                END-START
             END-IF
            END-PERFORM
            CALL "RECUP"
            IF TECLAF NOT = 27
              MOVE "N" TO BIEN
              MOVE 1 TO PUNTERO
              MOVE "S" TO PASO
              PERFORM CARGARCIUDAD UNTIL BIEN = "B"
             ELSE
              MOVE 0 TO CUAL
            END-IF.
       CARGARGIF.
           IF PUNTERO = MaxLineas
               MOVE "B" TO BIEN
            ELSE
              MOVE CGIF    TO TGIF(PUNTERO)
              MOVE CNOMBRE TO TNOMBRE(PUNTERO)
              ADD 1 TO PUNTERO
           END-IF
           READ CLIENTES NEXT AT END MOVE "B" TO BIEN
           END-READ.
       CARGARNOMBRE.
           READ CLIENTES NEXT AT END MOVE "B" TO BIEN
           END-READ
           IF BIEN NOT = "B" AND PUNTERO NOT = MaxLineas
                IF AUX NOT = CGIF  AND PASO = "N"
                   CONTINUE
                  ELSE
                   MOVE "S" TO PASO
                   MOVE CGIF    TO TGIF(PUNTERO)
                   MOVE CNOMBRE TO TNOMBRE(PUNTERO)
                   ADD 1 TO PUNTERO
                END-IF
               ELSE
                 MOVE "B" TO BIEN
           END-IF.
       CARGARCIUDAD.
           READ CLIENTES NEXT AT END MOVE "B" TO BIEN
           END-READ
           IF BIEN NOT = "B" AND PUNTERO NOT = MaxLineas
               IF CCIUDAD = CIUDAD(CUAL)
                 IF AUX NOT = CGIF  AND PASO = "N"
                    CONTINUE
                   ELSE
                    MOVE "S" TO PASO
                    MOVE CGIF    TO TGIF(PUNTERO)
                    MOVE CNOMBRE TO TNOMBRE(PUNTERO)
                    ADD 1 TO PUNTERO
                 END-IF
               ELSE
                 MOVE "B" TO BIEN
           END-IF.
       PreOpci.
           COMPUTE LINEA = TOPL + 1
           PERFORM SACAROPC VARYING J FROM 1 BY 1
            UNTIL J > NUMEROITENS(1)
           ACCEPT TECLA OFF NO BEEP ON EXCEPTION TECLAF CONTINUE
           END-ACCEPT
           IF TECLAF = 53 ADD 1 TO MODO END-IF
           IF TECLAF = 52 SUBTRACT 1 FROM MODO END-IF
           IF MODO = 0 MOVE NUMEROITENS(1) TO MODO END-IF
           IF MODO > NUMEROITENS(1) MOVE 1 TO MODO END-IF.
       SacarOpc.
           IF J = MODO
               DISPLAY NAMEITEN(1, J) LINE LINEA POSITION POSICION
               MOVE ASMENU(1, J) TO I
               PERFORM MENSAJE
             ELSE
               DISPLAY NAMEITEN(1, J) LINE LINEA POSITION POSICION
                REVERSE
           END-IF
           ADD 1 TO LINEA.
      *****************************************************************
      *                                                               *
      *****************************************************************
       DetalleEmpresa.
           PERFORM VentCompEmpresa
           CALL "CUROFF"
           MOVE TGIF(PUNTERO) TO CGIF
           READ CLIENTES
              INVALID KEY
                MOVE 9 TO I
                PERFORM MENSAJE
                PERFORM  ALTO
              NOT INVALID KEY
                DISPLAY CGIF       LINE 10 POSITION 24 CONTROL COLORA
                DISPLAY CNOMBRE    LINE 10 POSITION 35 CONTROL COLORA
                DISPLAY CDIRECCION LINE 12 POSITION 24 CONTROL COLORA
                DISPLAY CCIUDAD    LINE 14 POSITION 35 CONTROL COLORA
                DISPLAY CTELEFONO  LINE 14 POSITION 51 CONTROL COLORA
                DISPLAY CCP        LINE 16 POSITION 24 CONTROL COLORA
                DISPLAY CPROVIN    LINE 16 POSITION 35 CONTROL COLORA
                DISPLAY CNUMERO    LINE 14 POSITION 24 CONTROL COLORA
           END-READ
           CALL "CURON"
           MOVE "N" TO BIEN
           MOVE 0 TO ESTADO
           DELETE  CLIENTES
           PERFORM ACCPETSCAMPOS
           WRITE REG-CLI
                     INVALID KEY
                        MOVE 6 TO I
                        PERFORM MENSAJE
                        PERFORM ALTO
                     NOT INVALID KEY
                        MOVE 7 TO I
                        PERFORM MENSAJE
           END-WRITE
           CALL "RECUP.EXE"
           MOVE 0 TO  TECLAF.
      *****************************************************************
      *                                                               *
      *****************************************************************
       AltasEmpresa.
           PERFORM VentCompEmpresa
           PERFORM BucleEmpresa UNTIL TECLAF = 27
           MOVE 0 TO TECLAF
           CALL "RECUP.EXE".
       BucleEmpresa.
           MOVE " " TO REG-CLI
           CALL "CUROFF"
           DISPLAY CNOMBRE    LINE 10 POSITION 35
           DISPLAY CDIRECCION LINE 12 POSITION 24
           DISPLAY CCIUDAD    LINE 14 POSITION 35
           DISPLAY CTELEFONO  LINE 14 POSITION 51
           DISPLAY CCP        LINE 16 POSITION 24
           DISPLAY CPROVIN    LINE 16 POSITION 35
           DISPLAY CNUMERO    LINE 14 POSITION 24
           CALL "CURON"
           MOVE "N" TO BIEN
           MOVE 0 TO ESTADO
           PERFORM ACCPETSCAMPOS
           IF TECLAF NOT = 27
              WRITE REG-CLI
                     INVALID KEY
                        MOVE 6 TO I
                        PERFORM MENSAJE
                        PERFORM ALTO
                     NOT INVALID KEY
                        MOVE 7 TO I
                        PERFORM MENSAJE
              END-WRITE
           END-IF
           MOVE 8 TO I
           PERFORM MENSAJE.
      *****************************************************************
      *                                                               *
      *****************************************************************
       VentCompEmpresa.
           MOVE VENTANA4 TO VENTANA
           CALL "VENTANA" USING VENTANA
           DISPLAY  "    CLIENTES   " LINE 07 POSITION 34 REVERSE
           DISPLAY  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" LINE 08 POSITION 34 REVERSE
           DISPLAY " NIF:      NOMBRE:   " LINE 09 POSITION 24
             REVERSE.
           DISPLAY " DIRECCION:          " LINE 11 POSITION 24
             REVERSE.
           DISPLAY " NUMERO:   CIUDAD:         TELEFONO:"
             LINE 13 POSITION 24 REVERSE.
           DISPLAY " CP:       PROVINCIA:" LINE 15 POSITION 24
             REVERSE.
       ACCPETSCAMPOS.
           PERFORM UNTIL BIEN = "B" OR TECLAF = 27
              MOVE 5 TO I
              PERFORM MENSAJE
              ACCEPT CGIF LINE 10 POSITION 24 NO BEEP TAB UPDATE
                ON EXCEPTION TECLAF PERFORM SENAL
              END-ACCEPT
              IF ESTADO NOT = 1 AND CGIF NOT = ALL " "
                 READ CLIENTES
                    INVALID KEY
                       MOVE "B" TO BIEN
                    NOT INVALID KEY
                       MOVE "N" TO BIEN
                       MOVE 4 TO I
                       PERFORM MENSAJE
                       PERFORM ALTO
                 END-READ
              END-IF
           END-PERFORM
           IF TECLAF NOT = 27
            PERFORM WITH TEST AFTER UNTIL CNOMBRE NOT = ALL " "
            OR TECLAF = 27
              MOVE 17  TO I
              PERFORM MENSAJE
              ACCEPT CNOMBRE LINE 10 POSITION 35 NO BEEP TAB UPDATE
                ON EXCEPTION TECLAF PERFORM SENAL
              END-ACCEPT
            END-PERFORM
           END-IF
           IF TECLAF NOT = 27
              MOVE 5 TO I
              PERFORM MENSAJE
              ACCEPT CDIRECCION LINE 12 POSITION 24 NO BEEP TAB UPDATE
                ON EXCEPTION TECLAF PERFORM SENAL
              END-ACCEPT
           END-IF
           IF TECLAF NOT = 27
              ACCEPT CNUMERO LINE 14 POSITION 24 NO BEEP TAB UPDATE
                ON EXCEPTION TECLAF PERFORM SENAL
              END-ACCEPT
           END-IF
           IF TECLAF NOT = 27
             PERFORM WITH TEST AFTER UNTIL CCIUDAD NOT = ALL " "
              OR TECLAF = 27
             MOVE 17  TO I
             PERFORM MENSAJE
                ACCEPT CCIUDAD LINE 14 POSITION 35 NO BEEP TAB UPDATE
                  ON EXCEPTION TECLAF PERFORM SENAL
                END-ACCEPT
             END-PERFORM
           END-IF
           IF TECLAF NOT = 27
              MOVE 5 TO I
              PERFORM MENSAJE
              ACCEPT CTELEFONO LINE 14 POSITION 51 NO BEEP TAB UPDATE
                ON EXCEPTION TECLAF PERFORM SENAL
              END-ACCEPT
           END-IF
           IF TECLAF NOT = 27
              ACCEPT CCP LINE 16 POSITION 24 NO BEEP TAB UPDATE
                ON EXCEPTION TECLAF PERFORM SENAL
              END-ACCEPT
           END-IF
           IF TECLAF NOT = 27
              ACCEPT CPROVIN LINE 16 POSITION 35 NO BEEP TAB UPDATE
                ON EXCEPTION TECLAF PERFORM SENAL
              END-ACCEPT
           END-IF.
       MENSAJE.
           DISPLAY LM(I) LINE 24 POSITION 2 REVERSE.
       ALTO.
           ACCEPT TECLA  LINE 25 POSITION 1 NO BEEP
              ON EXCEPTION TECLAF CONTINUE
           END-ACCEPT
           DISPLAY "Ô" LINE 25 POSITION 1 CONTROL COLORA.
       SENAL.
           MOVE 1 TO ESTADO.
      *****************************************************************
      *                                                               *
      *****************************************************************
       IMPRIMIR.
           MOVE 1 TO LINEA
           MOVE 1 TO PAGINA
           MOVE "B" TO BIEN
           MOVE GIF(1)    TO CGIF
           MOVE NOMBRE(1) TO CNOMBRE
           MOVE CIUDAD(1) TO CCIUDAD
           PERFORM LEERIMP UNTIL BIEN = "N".
       LEERIMP.
           EVALUATE MODO
              WHEN 1
                READ CLIENTES
                    INVALID KEY
                      MOVE "N" TO BIEN
                      MOVE 13 TO I
                      PERFORM MENSAJE
                      PERFORM ALTO
                    NOT INVALID KEY CONTINUE
                END-READ
                MOVE "N" TO BIEN
                MOVE 1 TO PUNTERO
                PERFORM CARGARPGIF UNTIL BIEN = "B"
              WHEN 2
                  START CLIENTES KEY IS = CNOMBRE
                    INVALID KEY
                      MOVE "N" TO BIEN
                      MOVE 14 TO I
                      PERFORM MENSAJE
                      PERFORM ALTO
                    NOT INVALID KEY CONTINUE
                  END-START
                  MOVE "N" TO BIEN
                  MOVE 1 TO PUNTERO
                  MOVE "N" TO PASO
                  PERFORM CARGARPNOMBRE UNTIL BIEN = "B"
              WHEN 3
                  START CLIENTES KEY IS = CCIUDAD
                    INVALID KEY
                      MOVE "N" TO BIEN
                      MOVE 15 TO I
                      PERFORM MENSAJE
                      PERFORM ALTO
                    NOT INVALID KEY CONTINUE
                  END-START
                  MOVE "N" TO BIEN
                  MOVE 1 TO PUNTERO
                  MOVE "N" TO PASO
                  PERFORM CARGARPCIUDAD UNTIL BIEN = "B"
           END-EVALUATE.
       MARTILLEAR.
           MOVE CGIF       TO PGIF
           MOVE CNOMBRE    TO PNOMBRE
           MOVE CDIRECCION TO PDIRECCION
           MOVE CCIUDAD    TO PCIUDAD
           MOVE CTELEFONO  TO PTELEFONO
           MOVE CCP        TO PCP
           MOVE CPROVIN    TO PPROVIN
           MOVE CNUMERO    TO PNUMERO
           PERFORM IMPDATOS VARYING I FROM 1 BY 1 UNTIL I > 3.
           ADD 1 TO LINEA
           IF LINEA = MaxIMP
              MOVE SPACES TO LINEAIMP
              WRITE LINEAIMP BEFORE PAGE
              MOVE 1 TO LINEA
              ADD 1 TO PAGINA
           END-IF.
       IMPDATOS.
           EVALUATE I
             WHEN 1
              WRITE LINEAIMP FROM CAB(I) BEFORE ADVANCING 1 LINE
              WRITE LINEAIMP FROM LINDET1 BEFORE ADVANCING 1 LINE
             WHEN 2
              WRITE LINEAIMP FROM CAB(I) BEFORE ADVANCING 1 LINE
              WRITE LINEAIMP FROM LINDET2 BEFORE ADVANCING 1 LINE
             WHEN 3
              WRITE LINEAIMP FROM CAB(I) BEFORE ADVANCING 1 LINE
           END-EVALUATE.
       CARGARPGIF.
           IF PUNTERO = MaxLineas
               MOVE "B" TO BIEN
            ELSE
               IF BIEN NOT = "N"
                 PERFORM MARTILLEAR
               END-IF
           END-IF
           READ CLIENTES NEXT AT END MOVE "B" TO BIEN
           END-READ.
       CARGARPNOMBRE.
           READ CLIENTES NEXT AT END MOVE "B" TO BIEN
           END-READ
           IF BIEN NOT = "B" AND PUNTERO NOT = MaxLineas
                 PERFORM MARTILLEAR
               ELSE
                 MOVE "B" TO BIEN
           END-IF.
       CARGARPCIUDAD.
           READ CLIENTES NEXT AT END MOVE "B" TO BIEN
           END-READ
           IF BIEN NOT = "B" AND PUNTERO NOT = MaxLineas
               IF CCIUDAD = CIUDAD(CUAL)
                  PERFORM MARTILLEAR
               ELSE
                 MOVE "B" TO BIEN
           END-IF.