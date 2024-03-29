       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK1.

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

           SELECT F-MOVIMIENTOS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS MOV-NUM
           FILE STATUS IS FSM.

           SELECT OPTIONAL F-TRANSFERENCIAS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS TR-NUM
           FILE STATUS IS FSTR.


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
       FD F-MOVIMIENTOS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "../data/movimientos.ubd".
       01 MOVIMIENTO-REG.
           02 MOV-NUM              PIC  9(35).
           02 MOV-TARJETA          PIC  9(16).
           02 MOV-ANO              PIC   9(4).
           02 MOV-MES              PIC   9(2).
           02 MOV-DIA              PIC   9(2).
           02 MOV-HOR              PIC   9(2).
           02 MOV-MIN              PIC   9(2).
           02 MOV-SEG              PIC   9(2).
           02 MOV-IMPORTE-ENT      PIC  S9(7).
           02 MOV-IMPORTE-DEC      PIC   9(2).
           02 MOV-CONCEPTO         PIC  X(35).
           02 MOV-SALDOPOS-ENT     PIC  S9(9).
           02 MOV-SALDOPOS-DEC     PIC   9(2).
       FD F-TRANSFERENCIAS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "../data/transferencias.ubd".
       01 TRANSFE-REG.
           02 TR-NUM               PIC  9(35).
           02 TR-ORIGEN            PIC  9(16).
           02 TR-DESTINO           PIC  9(16).
           02 TR-IMPORTE-ENT       PIC  S9(7).
           02 TR-IMPORTE-DEC       PIC   9(2).
      *    TR-CONCEPTO = {Transferencia programada, Transferencia periodica}
           02 TR-CONCEPTO          PIC   X(35).
           02 TR-ANO               PIC   9(4).
           02 TR-MES               PIC   9(2).
           02 TR-DIA               PIC   9(2).

       WORKING-STORAGE SECTION.

       77 FST                      PIC  X(2).
       77 FSI                      PIC  X(2).
       77 FSM                      PIC  X(2).
       77 FSTR                     PIC  X(2).

       78 BLACK   VALUE 0.
       78 BLUE    VALUE 1.
       78 GREEN   VALUE 2.
       78 CYAN    VALUE 3.
       78 RED     VALUE 4.
       78 MAGENTA VALUE 5.
       78 YELLOW  VALUE 6.
       78 WHITE   VALUE 7.

       01 CAMPOS-FECHA.
           05 FECHA.
               10 ANO              PIC  9(4).
               10 MES              PIC  9(2).
               10 DIA              PIC  9(2).
           05 HORA.
               10 HORAS            PIC  9(2).
               10 MINUTOS          PIC  9(2).
               10 SEGUNDOS         PIC  9(2).
               10 MILISEGUNDOS     PIC  9(2).
           05 DIF-GMT              PIC S9(4).

       01 KEYBOARD-STATUS           PIC 9(4).
           88 ENTER-PRESSED          VALUE 0.
           88 PGUP-PRESSED        VALUE 2001.
           88 PGDN-PRESSED        VALUE 2002.
           88 UP-ARROW-PRESSED    VALUE 2003.
           88 DOWN-ARROW-PRESSED  VALUE 2004.
           88 ESC-PRESSED         VALUE 2005.

       77 PRESSED-KEY              PIC  9(4).
       77 PIN-INTRODUCIDO          PIC  9(4).
       77 CHOICE                   PIC  9(1).
       77 FECHA-ACTUAL             PIC  9(8).
       77 FECHA-ULT-COMP-TR        PIC  9(8) VALUE 0.
       77 SALDO-SUFICIENTE         PIC  9(1).

       77 LAST-USER-ORD-MOV-NUM    PIC  9(35).
       77 LAST-MOV-NUM             PIC  9(35).

       77 CENT-SALDO-ORD-USER      PIC  S9(9).
       77 CENT-IMPOR-USER          PIC  S9(9).

       SCREEN SECTION.
       01 BLANK-SCREEN.
           05 FILLER LINE 1 BLANK SCREEN BACKGROUND-COLOR BLACK.


       01 DATA-ACCEPT.
           05 TARJETA-ACCEPT BLANK ZERO AUTO LINE 08 COL 50
               PIC 9(16) USING TNUM.
           05 PIN-ACCEPT BLANK ZERO SECURE LINE 09 COL 50
               PIC 9(4) USING PIN-INTRODUCIDO.



       PROCEDURE DIVISION.
       IMPRIMIR-CABECERA.

           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'
           SET ENVIRONMENT 'COB_SCREEN_ESC'        TO 'Y'

           DISPLAY BLANK-SCREEN.

           DISPLAY "Cajero Automatico UnizarBank" LINE 2 COL 26
               WITH FOREGROUND-COLOR IS BLUE
               WITH BACKGROUND-COLOR IS WHITE.

           MOVE FUNCTION CURRENT-DATE TO CAMPOS-FECHA.

           DISPLAY DIA LINE 4 COL 32 .
           DISPLAY "-" LINE 4 COL 34 .
           DISPLAY MES LINE 4 COL 35 .
           DISPLAY "-" LINE 4 COL 37 "-".
           DISPLAY ANO LINE 4 COL 38 .
           DISPLAY HORAS LINE 4 COL 44 .
           DISPLAY ":" LINE 4 COL 46 .
           DISPLAY MINUTOS LINE 4 COL 47 .


       P1.
           DISPLAY "Bienvenido a UnizarBank" LINE 8 COL 28.
           DISPLAY "Por favor, introduzca la tarjeta para operar"
               LINE 10 COL 18.

           DISPLAY "Enter - Aceptar" LINE 24 COL 33.

       P1-ENTER.
           ACCEPT CHOICE WITH NO ECHO LINE 24 COL 79 ON EXCEPTION
           IF ENTER-PRESSED
               GO TO P2
           ELSE
               GO TO P1-ENTER.


       P2.
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "ESC - Salir" LINE 24 COL 33.
           INITIALIZE TNUM.
           INITIALIZE PIN-INTRODUCIDO.
           INITIALIZE TPIN.
           DISPLAY "Numero de tarjeta:" LINE 8 COL 15.
           DISPLAY "Inserte el pin de tarjeta:" LINE 9 COL 15.
           ACCEPT DATA-ACCEPT ON EXCEPTION
               IF ESC-PRESSED
                   GO TO IMPRIMIR-CABECERA
               ELSE
                   GO TO P2.

           CLOSE TARJETAS .
           OPEN I-O TARJETAS.
           IF FST NOT = 00
               GO TO PSYS-ERR.

           READ TARJETAS INVALID KEY GO TO PSYS-ERR.

           CLOSE INTENTOS.
           OPEN I-O INTENTOS.
           IF FSI NOT = 00
               GO TO PSYS-ERR.
           MOVE TNUM TO INUM.

           READ INTENTOS INVALID KEY GO TO PSYS-ERR.

           IF IINTENTOS = 0
               GO TO PINT-ERR.

           IF PIN-INTRODUCIDO NOT = TPIN
               GO TO PPIN-ERR.

           PERFORM REINICIAR-INTENTOS THRU REINICIAR-INTENTOS.

       PMENU.
           CLOSE TARJETAS.
           CLOSE INTENTOS.

           OPEN I-O INTENTOS.
           IF FSI NOT = 00
               GO TO PSYS-ERR.
           MOVE TNUM TO INUM.

           READ INTENTOS INVALID KEY GO TO PSYS-ERR.

           IF IINTENTOS <= 0
               CLOSE INTENTOS
               PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA
               GO TO P1.

           CLOSE INTENTOS.

           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "1 - Consultar saldo" LINE 8 COL 15.
           DISPLAY "2 - Consultar movimientos" LINE 9 COL 15.
           DISPLAY "3 - Retirar efectivo" LINE 10 COL 15.
           DISPLAY "4 - Ingresar efectivo" LINE 11 COL 15.
           DISPLAY "5 - Ordenar transferencia" LINE 12 COL 15.
           DISPLAY "6 - Listado de transferencias" LINE 13 COL 15.
           DISPLAY "7 - Comprar entradas de espectaculos" LINE 14 COL 15.
           DISPLAY "8 - Cambiar clave" LINE 16 COL 15.
           DISPLAY "ESC - Salir" LINE 24 COL 34.

       PMENUA1.
           ACCEPT CHOICE LINE 24 COL 79 ON EXCEPTION
               IF ESC-PRESSED
                   GO TO IMPRIMIR-CABECERA
               ELSE
                   GO TO PMENUA1.

      *    Consultar saldo
           IF CHOICE = 1
               CALL "BANK2" USING TNUM
               GO TO PMENU.
      *    Consultar movimientos
           IF CHOICE = 2
               CALL "BANK3" USING TNUM
               GO TO PMENU.
      *    Retirar efectivo
           IF CHOICE = 3
               CALL "BANK4" USING TNUM
               GO TO PMENU.
      *    Ingresar efectivo
           IF CHOICE = 4
               CALL "BANK5" USING TNUM
               GO TO PMENU.
      *    Ordenar transferencias
           IF CHOICE = 5
               CALL "BANK6" USING TNUM
               GO TO PMENU.
      *    Listado de transferencias
           IF CHOICE = 6
               CALL "BANK7" USING TNUM
               GO TO PMENU.
      *    Comprar entradas de espectaculos
           IF CHOICE = 7
               CALL "BANK8" USING TNUM
               GO TO PMENU.
      *    Cambiar clave
           IF CHOICE = 8
               CALL "BANK9" USING TNUM
               GO TO PMENU.

           GO TO PMENU.


       PSYS-ERR.
           CLOSE F-MOVIMIENTOS.
           CLOSE F-TRANSFERENCIAS.
           CLOSE TARJETAS.
           CLOSE INTENTOS.

           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "Ha ocurrido un error interno" LINE 9 COL 25
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.
           DISPLAY "Vuelva mas tarde" LINE 11 COL 32
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.
           DISPLAY "Enter - Aceptar" LINE 24 COL 33.
           GO TO PINT-ERR-ENTER.


       PINT-ERR.

           CLOSE TARJETAS.
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
           ACCEPT CHOICE WITH NO ECHO LINE 24 COL 79 ON EXCEPTION
           IF ENTER-PRESSED
               GO TO IMPRIMIR-CABECERA
           ELSE
               GO TO PINT-ERR-ENTER.


       PPIN-ERR.
           IF IINTENTOS > 0
               SUBTRACT 1 FROM IINTENTOS.
           REWRITE INTENTOSREG INVALID KEY GO TO PSYS-ERR.

           CLOSE TARJETAS.
           CLOSE INTENTOS.

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

           DISPLAY "Enter - Aceptar" LINE 24 COL 1.
           DISPLAY "ESC - Cancelar" LINE 24 COL 65.

       PPIN-ERR-ENTER.
           ACCEPT CHOICE WITH NO ECHO LINE 24 COL 79 ON EXCEPTION
           DISPLAY " " LINE 0  COL 0.

           IF ENTER-PRESSED
               GO TO IMPRIMIR-CABECERA
           ELSE
               IF ESC-PRESSED
                   GO TO IMPRIMIR-CABECERA
               ELSE
                   GO TO PPIN-ERR-ENTER.

       REINICIAR-INTENTOS.
           MOVE 3 TO IINTENTOS.
           REWRITE INTENTOSREG INVALID KEY GO TO PSYS-ERR.
