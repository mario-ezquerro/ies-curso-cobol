       IDENTIFICATION DIVISION.
       PROGRAM-ID. MACAPP.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-400Mb.
       OBJECT-COMPUTER. IBM-400Mb.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ASCYL.
           02 FILLER PIC X(4) VALUE "ÌÃÇÆ".
           02 FILLER PIC X(4) VALUE "¹´¶µ".
       01 DLINEA REDEFINES ASCYL.
           02 BLOQUESLIN OCCURS 2.
             03 DIBUJOL PIC X OCCURS 4.
       01 ASCYV.
           02 FILLER PIC X(6) VALUE "ÉÚÖÕ± ".
           02 FILLER PIC X(6) VALUE "ÍÄÄÍ± ".
           02 FILLER PIC X(6) VALUE "»¿·¸± ".
           02 FILLER PIC X(6) VALUE "º³º³± ".
           02 FILLER PIC X(6) VALUE "ÈÀÓÔ± ".
           02 FILLER PIC X(6) VALUE "¼Ù½¾± ".
       01 DVENT REDEFINES ASCYV.
           02 BLOQUESVENT OCCURS 6.
              03 DIBUJOV PIC X OCCURS 6.
       77 X PIC 99.
       77 Y PIC 99.
       77 NADA PIC X.
       77 CONT PIC 99.
       77 SITIO  PIC 99.
       77 POSICION PIC 99.
       77 J PIC 99.
       77 I PIC 99.
       77 LINEA PIC 99.
       01 VariblesImput.
           02 TECLA PIC X.
           02 TECLAFUNC PIC 99 COMP.
       01 BARRA.
           02 FILLER PIC X(68) VALUE " ".
           02 LCDFECHA.
               03 DIA PIC Z9.
               03 FILLER PIC X VALUE "-".
               03 MES PIC Z9.
               03 FILLER PIC X VALUE "-".
               03 ANO PIC Z9.
           02 FILLER PIC X(4) VALUE " ".
       01 FECHASYS.
           02 ANOSYS PIC 99.
           02 MESSYS PIC 99.
           02 DIASYS PIC 99.
       01 BARRAV.
           02  ELEMBARRA PIC X OCCURS 80.
       01 VENT.
           02 ESQUINAS.
                 03 TOPL   PIC 99.
                 03 TOPP   PIC 99.
                 03 DOWNL  PIC 99.
                 03 DOWNP  PIC 99.
                 03 ANCHO  PIC 99.
                 03 ALTO   PIC 99.
           02 HELP.
                 03 BIT2 PIC 9.
                 03 SOMBRA PIC 9.
       01 EVENT.
           02 AUX         PIC 99.
           02 WHERE       PIC 99.
           02 WHAT        PIC 99.
             88 InstSubMe  VALUE 1.
             88 SubMenu    VALUE 2.
           02 PtroSubMenu PIC 9.
           02 MENUID      PIC 99.
           02 ANTMENUID   PIC 99.
           02 WHICH       PIC 99.
           02 ANTWHICH    PIC 99.
           02 PASADOS OCCURS 4.
              03 ANT      PIC 99.
       77 MODO PIC 9 VALUE 1.
       77 MODOWHICH PIC 99 VALUE 1.
       77 MODOID PIC 99 VALUE 2.
       01 DATOSBARRAMENU.
           02 FILLER PIC 99 VALUE 02.
           02 FILLER PIC X(16) VALUE "0203OPCIONES".
           02 FILLER PIC X(16) VALUE "0214CONSULTA POR".
           02 FILLER PIC X(16) VALUE "0000SUBMENU".
       01 BARRAMENU REDEFINES DATOSBARRAMENU.
           02 MAXNUMEROIDS PIC 99.
           02 HANDLEBARRA OCCURS 3.
              03 LINEAMENU PIC 99.
              03 POSICIONMENU PIC 99.
              03 NAMEMENU PIC X(12).
       01 DATOSMENU1.
          02 FILLER PIC 9(4) VALUE 0710.
          02 FILLER PIC X(17) VALUE "A0ALTAS".
          02 FILLER PIC X(17) VALUE "A0MODIFICAR".
          02 FILLER PIC X(17) VALUE "A0BAJAS".
          02 FILLER PIC X(17) VALUE "A0LISTADO".
          02 FILLER PIC X(17) VALUE "A0IMPRESORA".
          02 FILLER PIC X(17) VALUE "L".
          02 FILLER PIC X(17) VALUE "A0FIN ".
      ********************************************
          02 FILLER PIC 9(4) VALUE 0512.
          02 FILLER PIC X(17) VALUE "M0TITULO".
          02 FILLER PIC X(17) VALUE "S3AUTOR".
          02 FILLER PIC X(17) VALUE "A0EDITORIAL".
          02 FILLER PIC X(17) VALUE "A0A¥O EDICION".
          02 FILLER PIC X(17) VALUE "A0ISBN ".
          02 FILLER PIC X(17) VALUE ALL " ".
          02 FILLER PIC X(17) VALUE ALL " ".
      ********************************************
          02 FILLER PIC 9(4) VALUE 0311.
          02 FILLER PIC X(17) VALUE "A0NOMBRE".
          02 FILLER PIC X(17) VALUE "A0APELLIDO 1".
          02 FILLER PIC X(17) VALUE "A0APELLIDO 2".
          02 FILLER PIC X(17) VALUE ALL " ".
          02 FILLER PIC X(17) VALUE ALL " ".
          02 FILLER PIC X(17) VALUE ALL " ".
          02 FILLER PIC X(17) VALUE ALL " ".
      *********************************************
       01 HANDLEMENU REDEFINES DATOSMENU1.
          02 VMENUS OCCURS 3.
              03 NUMEROITENS PIC 99.
              03 ANCHOMENU PIC 99.
              03 VVMENUS OCCURS 7.
                  04 ACT PIC X.
                    88 ACTIVO   VALUE  "A".
                    88 MARCADO  VALUE  "M".
                    88 LIN    VALUE  "L".
                    88 INACTIVO VALUE  "I".
                  04 NEXTMENU PIC 9.
                  04 NAMEITEN PIC X(15).

       PROCEDURE DIVISION.
       MAIN.
           DISPLAY " " ERASE
           MOVE 1 TO MENUID
           MOVE 1 TO WHICH
           MOVE 0 TO WHAT
           ACCEPT FECHASYS FROM DATE
           MOVE DIASYS TO DIA
           MOVE MESSYS TO MES
           MOVE ANOSYS TO ANO
           PERFORM INSTALMENU
           MOVE MENUID TO ANTMENUID
           PERFORM BUCLEMENU WITH TEST AFTER UNTIL TECLAFUNC = 27.
           DISPLAY " " ERASE
           STOP RUN.
       BUCLEMENU.
           ACCEPT TECLA NO BEEP OFF ON EXCEPTION TECLAFUNC
           PERFORM HANDLETECLAS.
           IF ANTMENUID = MENUID OR WHAT = 1 OR 2
                PERFORM INSTALMENU
             ELSE
                  PERFORM BORRARMENU
                  MOVE 1 TO WHICH
                  PERFORM INSTALMENU
                  MOVE MENUID TO ANTMENUID
           END-IF
           IF TECLAFUNC = 13
             EVALUATE MENUID
               WHEN 1
                 EVALUATE WHICH
                   WHEN 1
                        CALL "ALTAS.COB"
                        PERFORM AJUSTAMENU
                   WHEN 2
                        CALL "MODIFIC.COB" USING MODO
                        PERFORM AJUSTAMENU
                   WHEN 3
                        CALL "BAJAS.COB" USING MODO
                        PERFORM AJUSTAMENU
                   WHEN 4
                        CALL "LISTADO.COB" USING MODO
                        PERFORM AJUSTAMENU
                   WHEN 5
                        CALL "IMPRIMIR.COB"
                        PERFORM AJUSTAMENU
                   WHEN 7
                        MOVE 27 TO TECLAFUNC
                 END-EVALUATE
               WHEN 2
                 EVALUATE WHICH
                   WHEN 1
                      MOVE 1 TO MODO
                      PERFORM ACTMODOS
                   WHEN 2
                      PERFORM MOSTRARSUBMENU
                      PERFORM INSTALMENU
                   WHEN 3
                      MOVE 3 TO MODO
                      PERFORM ACTMODOS
                   WHEN 4
                      MOVE 4 TO MODO
                      PERFORM ACTMODOS
                   WHEN 5
                      MOVE 5 TO MODO
                      PERFORM ACTMODOS
                 END-EVALUATE
               WHEN 3
                 EVALUATE WHICH
                   WHEN 1
                      MOVE 6 TO MODO
                      PERFORM ACTMODOS
                   WHEN 2
                      MOVE 7 TO MODO
                      PERFORM ACTMODOS
                   WHEN 3
                      MOVE 8 TO MODO
                      PERFORM ACTMODOS
             END-EVALUATE
           END-IF.
       ACTMODOS.
           MOVE "A" TO ACT(MODOID , MODOWHICH)
           MOVE "M" TO ACT(MENUID , WHICH)
           MOVE MENUID TO MODOID
           MOVE WHICH TO MODOWHICH
           PERFORM INSTALMENU.
       HANDLETECLAS.
           MOVE WHICH TO ANTWHICH
           IF TECLAFUNC = 3 SUBTRACT 1 FROM WHICH END-IF
           IF TECLAFUNC = 4 ADD 1 TO WHICH END-IF
           IF TECLAFUNC = 1 AND WHAT = 2
              PERFORM BORRARMENU
              MOVE ANT(1) TO MENUID
              MOVE ANT(2) TO ANTMENUID
              MOVE ANT(4) TO ANTWHICH
              MOVE 0 TO WHAT
              MOVE ANT(3) TO WHICH
              PERFORM DMENU
              ADD 1 TO MENUID
           END-IF
           IF TECLAFUNC = 2 AND WHAT = 2
              PERFORM BORRARMENU
              MOVE ANT(1) TO MENUID
              MOVE ANT(2) TO ANTMENUID
              MOVE ANT(4) TO ANTWHICH
              MOVE 0 TO WHAT
              COMPUTE WHICH = ANT(3) + 1
           END-IF
           IF WHICH = 0
               MOVE NUMEROITENS(MENUID) TO WHICH
               COMPUTE ANTWHICH = WHICH + 1
           END-IF
           IF WHICH = (NUMEROITENS(MENUID) + 1) MOVE 1 TO WHICH END-IF
           IF WHAT = 0
               IF TECLAFUNC = 2 ADD 1 TO MENUID END-IF
               IF TECLAFUNC = 1 SUBTRACT 1 FROM MENUID END-IF
               IF MENUID = (MAXNUMEROIDS + 1 ) MOVE 1 TO MENUID END-IF
               IF MENUID = 0 MOVE MAXNUMEROIDS TO MENUID END-IF
           END-IF.

      *****************************************************
      *Pseudo codigo de las barras horizontales en el menu*
      *****************************************************
       BARRAH.
           COMPUTE SITIO = SITIO + TOPL
           MOVE ALL "Ä" TO BARRAV
           MOVE DIBUJOL(1 , BIT2) TO ELEMBARRA(1)
           MOVE DIBUJOL(2 , BIT2) TO ELEMBARRA(ANCHO)
           DISPLAY BARRAV LINE SITIO POSITION TOPP REVERSE SIZE ANCHO.
      *****************************************************************
      *****************************************************************
       AJUSTAMENU.
           DISPLAY BARRA LINE 1 POSITION 1 REVERSE
           PERFORM MENUS VARYING CONT FROM 1 BY 1 UNTIL CONT >
               MAXNUMEROIDS
           PERFORM DMENU
           PERFORM HANDLEITENSMENU.
       INSTALMENU.
           IF WHAT = 1
                PERFORM SUBMENU
                PERFORM DMENU
                PERFORM CARGARMENU VARYING CONT FROM 1 BY 1 UNTIL
                CONT > NUMEROITENS(MENUID)
             ELSE
                IF MENUID NOT = ANTMENUID
                  DISPLAY BARRA LINE 1 POSITION 1 REVERSE
                  PERFORM MENUS VARYING CONT FROM 1 BY 1 UNTIL CONT >
                  MAXNUMEROIDS
                  PERFORM DMENU
                 END-IF
           END-IF
           PERFORM HANDLEITENSMENU.
       MENUS.
           IF CONT = MENUID
                 DISPLAY NAMEMENU(CONT) LINE 1 POSITION
                 POSICIONMENU(CONT)
               ELSE
                 DISPLAY NAMEMENU(CONT) LINE 1 POSITION
                 POSICIONMENU(CONT) REVERSE
            END-IF.

       DMENU.
           COMPUTE TOPP = POSICIONMENU(MENUID) - 1
           COMPUTE Y = POSICIONMENU(MENUID) + 2
           MOVE LINEAMENU(MENUID) TO TOPL
           MOVE TOPL TO X
           COMPUTE DOWNP = POSICIONMENU(MENUID) +
              ANCHOMENU(MENUID) + 4
           COMPUTE DOWNL = NUMEROITENS(MENUID) + LINEAMENU(MENUID) + 1
           MOVE 21 TO HELP
           PERFORM VENTANA.
       HANDLEITENSMENU.
           MOVE TOPL TO X
           PERFORM CARGARMENU VARYING CONT FROM 1 BY 1 UNTIL
            CONT > NUMEROITENS(MENUID).
           IF WHAT = 1
                PERFORM SUBMENU
                PERFORM DMENU
                PERFORM CARGARMENU VARYING CONT FROM 1 BY 1 UNTIL
                CONT > NUMEROITENS(MENUID)
           END-IF.
       CARGARMENU.
           COMPUTE X = X + 1
           IF CONT NOT = WHICH
             EVALUATE ACT(MENUID , CONT)
               WHEN "S"
                   DISPLAY NAMEITEN(MENUID , CONT) LINE X POSITION Y
                   SIZE ANCHOMENU(MENUID) REVERSE
                   COMPUTE Y = Y + ANCHOMENU(MENUID) + 1
                   DISPLAY "¯" LINE X POSITION Y REVERSE
                   COMPUTE Y = Y - ANCHOMENU(MENUID) - 1
               WHEN "A"
                  COMPUTE Y = Y - 2
                  DISPLAY " " LINE X POSITION Y REVERSE
                  ADD 2 TO Y
                  DISPLAY NAMEITEN(MENUID , CONT) LINE X POSITION Y
                  SIZE ANCHOMENU(MENUID) REVERSE
               WHEN "I"
                  DISPLAY NAMEITEN(MENUID , CONT) LINE X POSITION Y
                   SIZE ANCHOMENU(MENUID) REVERSE CONTROL "BCOLOR=RED"
                   DISPLAY " " CONTROL "BCOLOR=BLACK"
               WHEN "M"
                   COMPUTE Y = Y - 2
                   DISPLAY "û" LINE X POSITION Y REVERSE
                   ADD 2 TO Y
                   DISPLAY NAMEITEN(MENUID , CONT) LINE X POSITION Y
                   SIZE ANCHOMENU(MENUID) REVERSE
               WHEN "L"
                   MOVE CONT TO SITIO
                   PERFORM BARRAH
             END-EVALUATE
            ELSE
             PERFORM INDESEADOS UNTIL
              ACT(MENUID , WHICH) NOT = "L" AND "I"
             EVALUATE ACT(MENUID , CONT)
                WHEN "S"
                   DISPLAY NAMEITEN(MENUID , CONT) LINE X POSITION Y
                   SIZE ANCHOMENU(MENUID)
                   COMPUTE Y = Y + ANCHOMENU(MENUID) + 1
                   DISPLAY "¯" LINE X POSITION Y
                   COMPUTE Y = Y - ANCHOMENU(MENUID) - 1
                WHEN "M"
                  COMPUTE Y = Y - 2
                  DISPLAY "û" LINE X POSITION Y
                  ADD 2 TO Y
                  DISPLAY NAMEITEN(MENUID , CONT) LINE X POSITION Y
                  SIZE ANCHOMENU(MENUID)
                WHEN "A"
                  COMPUTE Y = Y - 2
                  DISPLAY " " LINE X POSITION Y REVERSE
                  ADD 2 TO Y
                  DISPLAY NAMEITEN(MENUID , CONT) LINE X POSITION Y
                  SIZE ANCHOMENU(MENUID)
             END-EVALUATE
           END-IF.
       INDESEADOS.
           IF WHICH NOT < ANTWHICH
                  ADD 1 TO WHICH
                  ADD 1 TO X
                  IF WHICH = (NUMEROITENS(MENUID) + 1)
                    MOVE 1 TO WHICH
                    MOVE 0 TO ANTWHICH
                    COMPUTE X = TOPL + 1
                  END-IF
                ELSE
                  COMPUTE WHICH = WHICH - 1
                  COMPUTE X = X - 1
                  IF WHICH = 0 OR NUMEROITENS(MENUID)
                    MOVE NUMEROITENS(MENUID) TO WHICH
                    COMPUTE ANTWHICH = WHICH + 1
                    COMPUTE X = TOPL + NUMEROITENS(MENUID)
                  END-IF
           END-IF
           MOVE WHICH TO CONT.
       BORRARMENU.
           COMPUTE TOPP = POSICIONMENU(ANTMENUID) - 1
           COMPUTE Y = POSICIONMENU(ANTMENUID) + 1
           MOVE LINEAMENU(MENUID) TO TOPL
           MOVE TOPL TO X
           COMPUTE DOWNP = POSICIONMENU(ANTMENUID) +
              ANCHOMENU(ANTMENUID) + 5
           COMPUTE DOWNL = NUMEROITENS(ANTMENUID) + LINEAMENU(MENUID)
            + 2
           MOVE 6 TO BIT2
           PERFORM VENTANA.
       SUBMENU.
           MOVE MENUID TO ANT(1)
           MOVE ANTMENUID TO ANT(2)
           MOVE WHICH TO ANT(3)
           MOVE ANTWHICH TO ANT(4)
           MOVE PtroSubMenu TO MENUID
           MOVE PtroSubMenu TO ANTMENUID
           MOVE ANT(1) TO AUX
           COMPUTE LINEAMENU(MENUID) = LINEAMENU(AUX) + WHICH
           COMPUTE POSICIONMENU(MENUID) = POSICIONMENU(AUX) +
           ANCHOMENU(AUX) + 4
           MOVE 1 TO WHICH
           MOVE 2 TO WHAT.
       MOSTRARSUBMENU.
           COMPUTE Y = Y - ANCHOMENU(MENUID) - 1
           MOVE NEXTMENU(MENUID , WHICH) TO PtroSubMenu
           MOVE 1  TO WHAT.
      *****************************************************************
       VENTANA.
           IF BIT2 = 6
              ADD 1 TO ANCHO
              MOVE " " TO BARRAV
              PERFORM BorraVentana VARYING LINEA FROM TOPL BY 1
              UNTIL LINEA > DOWNL
            ELSE
             COMPUTE ANCHO = DOWNP - TOPP + 1
             COMPUTE ALTO  = DOWNL - TOPL
            PERFORM GUARRADA
            MOVE DIBUJOV(1 , BIT2) TO ELEMBARRA (1)
            MOVE DIBUJOV(3 , BIT2) TO ELEMBARRA (ANCHO)
            DISPLAY BARRAV LINE TOPL POSITION TOPP SIZE ANCHO REVERSE
            MOVE ALL " " TO BARRAV
            MOVE DIBUJOV(4 , BIT2) TO ELEMBARRA (1)
            MOVE DIBUJOV(4 , BIT2) TO ELEMBARRA (ANCHO)
            IF SOMBRA = 1
               ADD 1 TO ANCHO
               MOVE DIBUJOV(1 , 5) TO ELEMBARRA (ANCHO)
            END-IF
            COMPUTE I = TOPL + 1
            PERFORM LINHORZR VARYING LINEA FROM I BY 1
               UNTIL LINEA = DOWNL
            PERFORM GUARRADA
            IF SOMBRA = 1
                COMPUTE ANCHO = ANCHO - 1
            END-IF
            MOVE DIBUJOV(5 , BIT2) TO ELEMBARRA (1)
            MOVE DIBUJOV(6 , BIT2) TO ELEMBARRA (ANCHO)
            DISPLAY BARRAV LINE DOWNL POSITION TOPP SIZE ANCHO REVERSE
            IF SOMBRA = 1
              COMPUTE LINEA = DOWNL + 1
              COMPUTE POSICION = TOPP + 1
              COMPUTE J = DOWNP + 1
              DISPLAY DIBUJOV(1 , 5) LINE DOWNL POSITION J REVERSE
              MOVE ALL "±" TO BARRAV
              DISPLAY  BARRAV LINE LINEA POSITION POSICION REVERSE
              SIZE ANCHO
            END-IF
           END-IF.
       LINHORZR.
           DISPLAY BARRAV LINE LINEA POSITION TOPP SIZE ANCHO REVERSE.
       GUARRADA.
           EVALUATE BIT2
              WHEN 1
                MOVE ALL "Í" TO BARRAV
              WHEN 2
                MOVE ALL "Ä" TO BARRAV
              WHEN 3
                MOVE ALL "Ä" TO BARRAV
              WHEN 4
                MOVE ALL "Í" TO BARRAV
              WHEN 5
                MOVE ALL "±" TO BARRAV
              WHEN OTHER
                MOVE ALL "Í" TO BARRAV
           END-EVALUATE.
       BorraVentana.
           DISPLAY BARRAV  LINE LINEA POSITION TOPP SIZE ANCHO.