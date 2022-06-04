       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUELDO.
      *PROGRAMA QUE CALCULA LOS SUELDOS NETOS DE LOS CURRANTES
      *DIEZ QUE SE INTRODUCEN POR TECLAS
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-400Mb.
       OBJECT-COMPUTER. IBM-400Mb.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Variables de control.
       77 NADA PIC X.
       77 CONT PIC 99.
      *Constante de lineas fijas.
       77 EDobleDechS PIC X VALUE "É".
       77 EDobleIzquS PIC X VALUE "»".
       77 EDobleDechI PIC X VALUE "È".
       77 EDobleIzquI PIC X VALUE "¼".
       77 LDobleHorz PIC X VALUE "Í".
       77 LSimpleHorz PIC X VALUE "Ä".
       77 LDobleVert PIC X VALUE "º".
      *Variables de la pantalla 1.
       77 LINEAH PIC X(78).
       77 LINEAV PIC X(23).
       PROCEDURE DIVISION.
       MAIN.
           PERFORM PANTALLA1
           PERFORM PANTALLA2
           ACCEPT NADA
           STOP RUN.
       PANTALLA1.
      *Este saca una linea doble a lo largo de la pantalla
      *ocupando todo el borde mas exterior de la misma
           DISPLAY EdobleDechS LINE 1 POSITION 1
           MOVE ALL  "Í" TO LINEAH
           DISPLAY LINEAH LINE 1 POSITION 2
           DISPLAY EDobleIzquS LINE 1 POSITION 80
           PERFORM PINTAR VARYING CONT FROM 2 BY 1 UNTIL CONT = 25
           DISPLAY LINEAH LINE 25 POSITION 2
           DISPLAY EDobleDechI LINE 25 POSITION 1
           DISPLAY EDobleIzquI LINE 25 POSITION 80.
       PINTAR.
           DISPLAY LDobleVert LINE CONT POSITION 1
           DISPLAY LDobleVert LINE CONT POSITION 80.
       PANTALLA2.
