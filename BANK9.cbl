       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK9.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS KEYBOARD-STATUS.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TARJETAS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS TNUM
           FILE STATUS IS FST.

           SELECT INTENTOS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS INUM
           FILE STATUS IS FSI.


       DATA DIVISION.
       FILE SECTION.
       FD TARJETAS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "../data/tarjetas.ubd".
       01 TAJETAREG.
           02 TNUM      PIC 9(16).
           02 TPIN      PIC  9(4).

       FD INTENTOS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "../data/intentos.ubd".
       01 INTENTOSREG.
           02 INUM      PIC 9(16).
           02 IINTENTOS PIC 9(1).


       WORKING-STORAGE SECTION.
       77 FST                       PIC   X(2).
       77 FSI                       PIC   X(2).

       78 BLACK                   VALUE      0.
       78 BLUE                    VALUE      1.
       78 GREEN                   VALUE      2.
       78 CYAN                    VALUE      3.
       78 RED                     VALUE      4.
       78 MAGENTA                 VALUE      5.
       78 YELLOW                  VALUE      6.
       78 WHITE                   VALUE      7.

       01 CAMPOS-FECHA.
           05 FECHA.
               10 ANO               PIC   9(4).
               10 MES               PIC   9(2).
               10 DIA               PIC   9(2).
           05 HORA.
               10 HORAS             PIC   9(2).
               10 MINUTOS           PIC   9(2).
               10 SEGUNDOS          PIC   9(2).
               10 MILISEGUNDOS      PIC   9(2).
           05 DIF-GMT               PIC  S9(4).

       01 KEYBOARD-STATUS           PIC 9(4).
           88 ENTER-PRESSED          VALUE 0.
           88 PGUP-PRESSED        VALUE 2001.
           88 PGDN-PRESSED        VALUE 2002.
           88 UP-ARROW-PRESSED    VALUE 2003.
           88 DOWN-ARROW-PRESSED  VALUE 2004.
           88 ESC-PRESSED         VALUE 2005.

       77 PIN-INTRODUCIDO          PIC  9(4).
       77 NEW-PIN-INTR             PIC  9(4).
       77 CONF-PIN-INTR            PIC  9(4).

       77 PRESSED-KEY              PIC  9(1).

       LINKAGE SECTION.
       77 TNUM-L                   PIC  9(16).

       SCREEN SECTION.
       01 BLANK-SCREEN.
           05 FILLER LINE 1 BLANK SCREEN BACKGROUND-COLOR BLACK.

       01 DATA-ACCEPT.
           05 PIN-ACCEPT BLANK ZERO SECURE LINE 11 COL 55
               PIC 9(4) USING PIN-INTRODUCIDO.

       01 NEW-PIN-ACCEPT.
           05 NEW-PIN BLANK ZERO LINE 12 COL 55
               PIC 9(4) USING NEW-PIN-INTR.

           05 CONF-PIN BLANK ZERO LINE 13 COL 55
               PIC 9(4) USING CONF-PIN-INTR.

       PROCEDURE DIVISION USING TNUM-L.

      * Imprime la cabecera junto con la fecha actual
       IMPRIMIR-CABECERA.

           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.

           DISPLAY BLANK-SCREEN.
           DISPLAY "Cajero Automatico UnizarBank" LINE 2 COL 26
               WITH FOREGROUND-COLOR IS BLUE
               WITH BACKGROUND-COLOR IS WHITE.

           MOVE FUNCTION CURRENT-DATE TO CAMPOS-FECHA.

           DISPLAY DIA LINE 4 COL 32.
           DISPLAY "-" LINE 4 COL 34.
           DISPLAY MES LINE 4 COL 35.
           DISPLAY "-" LINE 4 COL 37.
           DISPLAY ANO LINE 4 COL 38.
           DISPLAY HORAS LINE 4 COL 44.
           DISPLAY ":" LINE 4 COL 46.
           DISPLAY MINUTOS LINE 4 COL 47.

      * Reimprime la cabecera y pide al usuario el pin el programa queda
      * a la espera del usuario en el ACCEPT (declarado arriba en SCREEN
      * SECTION)
       P2.
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           INITIALIZE PIN-INTRODUCIDO.
           INITIALIZE NEW-PIN-INTR.
           INITIALIZE CONF-PIN-INTR.
           DISPLAY "Cambio de clave personal" LINE 09 COL 15.
           DISPLAY "Introduzca el pin actual:" LINE 11 COL 15.
           DISPLAY "Enter - Confirmar" LINE 24 COL 02.
           DISPLAY "ESC - Cancelar" LINE 24 COL 65.
           ACCEPT DATA-ACCEPT ON EXCEPTION
               IF ESC-PRESSED
                   EXIT PROGRAM
               ELSE IF ENTER-PRESSED
                       GO TO P3
               ELSE
                   GO TO P2.

      * Chequea si el pin introducido es el mismo que el que ha insertado
      * el usuario al entrar en la aplicacion.
      * Si no coinciden muestra un mensaje de error
       P3.
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "Enter - Reintentar" LINE 24 COL 01.
           DISPLAY "ESC - Cancelar" LINE 24 COL 65.

           CLOSE TARJETAS.
           OPEN I-O TARJETAS.
           IF FST NOT = 00
               GO TO PSYS-ERR.

           MOVE TNUM-L TO TNUM.
           READ TARJETAS.

           CLOSE INTENTOS.
           OPEN I-O INTENTOS.
           IF FSI NOT = 00
               GO TO PSYS-ERR.

           MOVE TNUM TO INUM.
           READ INTENTOS INVALID KEY GO TO PSYS-ERR.

           IF IINTENTOS <= 0
               GO TO BLOQUEO-TARJETA
           END-IF.

           IF PIN-INTRODUCIDO <> TPIN
               GO TO RESTAR-INTENTO
           ELSE
               GO TO P4
           END-IF.

           ACCEPT PRESSED-KEY WITH NO ECHO LINE 24 COL 79
               IF ESC-PRESSED
                   EXIT PROGRAM
               ELSE
                   GO TO P2.

           GO TO P4.

       RESTAR-INTENTO.
           IF IINTENTOS > 0
               SUBTRACT 1 FROM IINTENTOS
           END-IF.

           REWRITE INTENTOSREG INVALID KEY GO TO PSYS-ERR.

           IF IINTENTOS <= 0
              GO TO BLOQUEO-TARJETA
           END-IF.

           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "El codigo PIN es incorrecto" LINE 9 COL 26
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.
           DISPLAY "Le quedan " LINE 11 COL 30
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.
           DISPLAY IINTENTOS LINE 11 COL 40
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.
           DISPLAY " intentos" LINE 11 COL 42
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.

           DISPLAY "Enter - Reintentar" LINE 24 COL 1.
           DISPLAY "ESC - Cancelar" LINE 24 COL 65.

           ACCEPT PRESSED-KEY WITH NO ECHO LINE 24 COL 79
               IF ESC-PRESSED
                   EXIT PROGRAM
               ELSE
                   GO TO P2.

      * Pide al usuario el nuevo pin dos veces y queda a la espera en
      * NEW-PIN-ACCEPT del pin.
       P4.
           PERFORM REINICIAR-INTENTOS THRU REINICIAR-INTENTOS
           CLOSE INTENTOS.
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           INITIALIZE NEW-PIN.
           INITIALIZE CONF-PIN.

           DISPLAY "Cambio de clave personal" LINE 09 COL 15.
           DISPLAY "Introduzca nueva clave:" LINE 12 COL 15.
           DISPLAY "Repita la nueva clave:"
               LINE 13 COL 15.

           DISPLAY "Enter - Confirmar" LINE 24 COL 02.
           DISPLAY "ESC - Cancelar" LINE 24 COL 65.

           ACCEPT NEW-PIN-ACCEPT ON EXCEPTION
               IF ESC-PRESSED
                   EXIT PROGRAM
               ELSE IF ENTER-PRESSED
                   GO TO P5
               ELSE
                   GO TO P2.

      * Chequea que los dos pines introducidos coincidan.
      * Si coinciden persiste el nuevo pin en el fichero tarjetas.ubd
      * Si no hace un go to CONF-ERR para mostrar un mensaje de error
       P5.
           IF NEW-PIN <> CONF-PIN
               GO TO CONF-ERR
           ELSE
               MOVE NEW-PIN TO TPIN
               REWRITE TAJETAREG INVALID KEY GO TO PSYS-ERR
           END-IF.

           CLOSE TARJETAS.
           GO TO P5-PIN-CHANGED.


      * Muestra un mensaje de error cuando los nuevos pines del usuario
      * no coinciden entre ellos
       CONF-ERR.
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           INITIALIZE NEW-PIN-INTR.
           INITIALIZE CONF-PIN-INTR.
           DISPLAY "Los pins proporcionados no coinciden" LINE 9 COL 20
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.

           DISPLAY "Enter - Reintentar" LINE 24 COL 01.
           DISPLAY "ESC - Cancelar" LINE 24 COL 65.

           ACCEPT PRESSED-KEY WITH NO ECHO LINE 24 COL 79
               IF ESC-PRESSED
                   EXIT PROGRAM
               ELSE
                   GO TO P4.

      * Muestra un mensaje de exito cuando al usuario se le ha podido
      * cambiar el pin
       P5-PIN-CHANGED.
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "La clave se ha cambiado correctamente" LINE 9 COL 22
               WITH FOREGROUND-COLOR IS BLACK
                    BACKGROUND-COLOR IS GREEN.

           DISPLAY "Enter - Confirmar" LINE 24 COL 33.
           GO TO EXIT-ENTER.


       PSYS-ERR.
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "Ha ocurrido un error interno" LINE 9 COL 25
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.
           DISPLAY "Vuelva mas tarde" LINE 11 COL 32
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.
           DISPLAY "Enter - Confirmar" LINE 24 COL 33.

       EXIT-ENTER.
           ACCEPT PRESSED-KEY WITH NO ECHO LINE 24 COL 79
           IF ENTER-PRESSED
               EXIT PROGRAM
           ELSE
               GO TO EXIT-ENTER.

       REINICIAR-INTENTOS.
           MOVE 3 TO IINTENTOS.
           REWRITE INTENTOSREG INVALID KEY GO TO PSYS-ERR.

       BLOQUEO-TARJETA.
           CLOSE INTENTOS.
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY " Se ha sobrepasado el numero de intentos"
               LINE 9 COL 20
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.
           DISPLAY "Por su seguridad se ha bloqueado la tarjeta"
               LINE 11 COL 18
                   WITH FOREGROUND-COLOR IS WHITE
                   BACKGROUND-COLOR IS RED.
           DISPLAY "Acuda a una sucursal" LINE 12 COL 30
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.
           DISPLAY "Enter - Aceptar" LINE 24 COL 33 .

       PINT-ERR-ENTER.
           ACCEPT PRESSED-KEY WITH NO ECHO LINE 24 COL 79 ON EXCEPTION
           IF ENTER-PRESSED
              EXIT PROGRAM
           ELSE
               GO TO PINT-ERR-ENTER.
