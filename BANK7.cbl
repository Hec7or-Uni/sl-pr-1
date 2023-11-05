       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK7.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS KEYBOARD-STATUS.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL F-MOVIMIENTOS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS MOV-NUM
           FILE STATUS IS FSM.

           SELECT OPTIONAL F-TRANSFERENCIAS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS TR-NUM
           FILE STATUS IS FSTR.

           SELECT OPTIONAL F-MOV-TRANSFE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS MOV-TRANSFE-NUM
           FILE STATUS IS FSMTR.


       DATA DIVISION.
       FILE SECTION.
       FD F-MOVIMIENTOS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "../data/movimientos.ubd".
       01 MOVIMIENTO-REG.
           02 MOV-NUM               PIC  9(35).
           02 MOV-TARJETA           PIC  9(16).
           02 MOV-ANO               PIC   9(4).
           02 MOV-MES               PIC   9(2).
           02 MOV-DIA               PIC   9(2).
           02 MOV-HOR               PIC   9(2).
           02 MOV-MIN               PIC   9(2).
           02 MOV-SEG               PIC   9(2).
           02 MOV-IMPORTE-ENT       PIC  S9(7).
           02 MOV-IMPORTE-DEC       PIC   9(2).
           02 MOV-CONCEPTO          PIC  X(35).
           02 MOV-SALDOPOS-ENT      PIC  S9(9).
           02 MOV-SALDOPOS-DEC      PIC   9(2).

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
           02 TR-ANO-ULT-EJEC      PIC   9(4).
           02 TR-MES-ULT-EJEC      PIC   9(2).
           02 TR-DIA-ULT-EJEC      PIC   9(2).
           02 TR-CONCEPTO          PIC   X(35).
           02 TR-ANO               PIC   9(4).
           02 TR-MES               PIC   9(2).
           02 TR-DIA               PIC   9(2).

       FD F-MOV-TRANSFE
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "../data/movtransfe.ubd".
       01 MOV-TRANSFE-REG.
           02 MOV-TRANSFE-NUM               PIC  9(35).
           02 MOV-TRANSFE-TARJETA           PIC  9(16).
           02 MOV-TRANSFE-ANO               PIC   9(4).
           02 MOV-TRANSFE-MES               PIC   9(2).
           02 MOV-TRANSFE-DIA               PIC   9(2).
           02 MOV-TRANSFE-HOR               PIC   9(2).
           02 MOV-TRANSFE-MIN               PIC   9(2).
           02 MOV-TRANSFE-SEG               PIC   9(2).
           02 MOV-TRANSFE-IMPORTE-ENT       PIC  S9(7).
           02 MOV-TRANSFE-IMPORTE-DEC       PIC   9(2).
           02 MOV-TRANSFE-CONCEPTO          PIC  X(35).
           02 MOV-TRANSFE-SALDOPOS-ENT      PIC  S9(9).
           02 MOV-TRANSFE-SALDOPOS-DEC      PIC   9(2).

       WORKING-STORAGE SECTION.
       77 FSM                       PIC   X(2).
       77 FSTR                      PIC   X(2).
       77 FSMTR                     PIC   X(2).

       78 BLACK                     VALUE    0.
       78 BLUE                      VALUE    1.
       78 GREEN                     VALUE    2.
       78 CYAN                      VALUE    3.
       78 RED                       VALUE    4.
       78 MAGENTA                   VALUE    5.
       78 YELLOW                    VALUE    6.
       78 WHITE                     VALUE    7.

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

       01 KEYBOARD-STATUS           PIC   9(4).
           88 ENTER-PRESSED         VALUE    0.
           88 PGUP-PRESSED          VALUE 2001.
           88 PGDN-PRESSED          VALUE 2002.
           88 UP-ARROW-PRESSED      VALUE 2003.
           88 DOWN-ARROW-PRESSED    VALUE 2004.
           88 ESC-PRESSED           VALUE 2005.
       77 PRESSED-KEY               PIC   9(4).

       77 DIA1-USUARIO              PIC   9(2).
       77 MES1-USUARIO              PIC   9(2).
       77 ANO1-USUARIO              PIC   9(4).
       77 DIA2-USUARIO              PIC   9(2).
       77 MES2-USUARIO              PIC   9(2).
       77 ANO2-USUARIO              PIC   9(4).

       77 EURENT1-USUARIO           PIC  S9(7).
       77 EURDEC1-USUARIO           PIC   9(2).
       77 EURENT2-USUARIO           PIC  S9(7).
       77 EURDEC2-USUARIO           PIC   9(2).

       77 FECHA-MIN                 PIC   9(8).
       77 FECHA-MOV                 PIC   9(8).
       77 FECHA-MAX                 PIC   9(8).
       77 CENT-MIN                  PIC  S9(9).
       77 CENT-MOV                  PIC  S9(9).
       77 CENT-MAX                  PIC  S9(9).

       77 MOV-EN-PANTALLA           PIC   9(2).
       77 LINEA-MOV-ACTUAL          PIC   9(2).
       77 MOV-VALIDO                PIC   9(1).
       77 MODULO-LIN-ACTUAL         PIC   9(1).

       01 TABLA.
           05 REGISTROS-EN-PANTALLA PIC  9(35) OCCURS 15 TIMES.

       77 CONTADOR                  PIC   9(2).
       77 ITERACIONES               PIC   9(2).
       77 COPIA-MOV                 PIC   9(35).

       77 TR-MOV-NUM                PIC   9(8) VALUE 0.

       LINKAGE SECTION.
       77 TNUM                      PIC  9(16).


       SCREEN SECTION.
       01 BLANK-SCREEN.
           05 FILLER LINE 1 BLANK SCREEN BACKGROUND-COLOR BLACK.

       01 FILTRO-MOVIMIENTOS.
           05 DIA-MIN BLANK ZERO AUTO UNDERLINE
               LINE 13 COL 37 PIC 9(2) USING DIA1-USUARIO.
           05 MES-MIN BLANK ZERO AUTO UNDERLINE
               LINE 13 COL 40 PIC 9(2) USING MES1-USUARIO.
           05 ANO-MIN BLANK ZERO AUTO UNDERLINE
               LINE 13 COL 43 PIC 9(4) USING ANO1-USUARIO.
           05 DIA-MAX BLANK ZERO BEEP AUTO UNDERLINE
               LINE 13 COL 50 PIC 9(2) USING DIA2-USUARIO.
           05 MES-MAX BLANK ZERO AUTO UNDERLINE
               LINE 13 COL 53 PIC 9(2) USING MES2-USUARIO.
           05 ANO-MAX BLANK ZERO UNDERLINE
               LINE 13 COL 56 PIC 9(4) USING ANO2-USUARIO.

       01 FILA-MOVIMIENTO-PAR.

           05 MOV-DIA-PAR LINE LINEA-MOV-ACTUAL COL 02
               FOREGROUND-COLOR YELLOW PIC 99 FROM MOV-TRANSFE-DIA.
           05 SEPARADOR-PAR-1 LINE LINEA-MOV-ACTUAL COL 04
               FOREGROUND-COLOR YELLOW PIC A FROM "-".
           05 MOV-MES-PAR LINE LINEA-MOV-ACTUAL COL 05
               FOREGROUND-COLOR YELLOW PIC 99 FROM MOV-TRANSFE-MES.
           05 SEPARADOR-PAR-2 LINE LINEA-MOV-ACTUAL COL 07
               FOREGROUND-COLOR YELLOW PIC A FROM "-".
           05 MOV-ANO-PAR LINE LINEA-MOV-ACTUAL COL 08
               FOREGROUND-COLOR YELLOW PIC 9(4) FROM MOV-TRANSFE-ANO.
           05 MOV-HOR-PAR LINE LINEA-MOV-ACTUAL COL 13
               FOREGROUND-COLOR YELLOW PIC 99 FROM MOV-TRANSFE-HOR.
           05 SEPARADOR-PAR-3 LINE LINEA-MOV-ACTUAL COL 15
               FOREGROUND-COLOR YELLOW PIC A FROM ":".
           05 MOV-MIN-PAR LINE LINEA-MOV-ACTUAL COL 16
               FOREGROUND-COLOR YELLOW PIC 99 FROM MOV-TRANSFE-MIN.
           05 SEPARADOR-PAR-4 LINE LINEA-MOV-ACTUAL COL 18
               FOREGROUND-COLOR YELLOW PIC A FROM "|".
           05 MOV-CONCEPTO-PAR LINE LINEA-MOV-ACTUAL COL 19
               FOREGROUND-COLOR YELLOW PIC X(35)
               FROM MOV-TRANSFE-CONCEPTO.
           05 SEPARADOR-5-PAR LINE LINEA-MOV-ACTUAL COL 54
               FOREGROUND-COLOR YELLOW PIC A FROM "|".
           05 MOV-IMPORTE-ENT-PAR SIGN IS LEADING SEPARATE
               LINE LINEA-MOV-ACTUAL COL 55
               FOREGROUND-COLOR YELLOW PIC S9(7)
               FROM MOV-TRANSFE-IMPORTE-ENT.
           05 SEPARADOR-6-PAR LINE LINEA-MOV-ACTUAL COL 63
               FOREGROUND-COLOR YELLOW PIC A FROM ",".
           05 MOV-IMPORTE-DEC-PAR LINE LINEA-MOV-ACTUAL COL 64
               FOREGROUND-COLOR YELLOW PIC 99
               FROM MOV-TRANSFE-IMPORTE-DEC.
           05 SEPARADOR-7-PAR LINE LINEA-MOV-ACTUAL COL 66
               FOREGROUND-COLOR YELLOW PIC A FROM "|".
           05 MOV-SALDOPOS-ENT-PAR SIGN IS LEADING SEPARATE
               LINE LINEA-MOV-ACTUAL COL 67
               FOREGROUND-COLOR YELLOW PIC S9(9)
               FROM MOV-TRANSFE-SALDOPOS-ENT.
           05 SEPARADOR-8-PAR LINE LINEA-MOV-ACTUAL COL 77
               FOREGROUND-COLOR YELLOW PIC A FROM ",".
           05 MOV-SALDOPOS-DEC-PAR LINE LINEA-MOV-ACTUAL COL 78
               FOREGROUND-COLOR YELLOW PIC 99
               FROM MOV-TRANSFE-SALDOPOS-DEC.

       01 FILA-MOVIMIENTO-IMPAR.
           05 MOV-DIA-IMPAR LINE LINEA-MOV-ACTUAL COL 02
               PIC 99 FROM MOV-TRANSFE-DIA.
           05 SEPARADOR-IMPAR-1 LINE LINEA-MOV-ACTUAL COL 04
               PIC A FROM "-".
           05 MOV-MES-IMPAR LINE LINEA-MOV-ACTUAL COL 05
               PIC 99 FROM MOV-TRANSFE-MES.
           05 SEPARADOR-IMPAR-2 LINE LINEA-MOV-ACTUAL COL 07
               PIC A FROM "-".
           05 MOV-ANO-IMPAR LINE LINEA-MOV-ACTUAL COL 08
               PIC 9(4) FROM MOV-TRANSFE-ANO.
           05 MOV-HOR-IMPAR LINE LINEA-MOV-ACTUAL COL 13
               PIC 99 FROM MOV-TRANSFE-HOR.
           05 SEPARADOR-IMPAR-3 LINE LINEA-MOV-ACTUAL COL 15
               PIC A FROM ":".
           05 MOV-MIN-IMPAR LINE LINEA-MOV-ACTUAL COL 16
               PIC 99 FROM MOV-TRANSFE-MIN.
           05 SEPARADOR-IMPAR-4 LINE LINEA-MOV-ACTUAL COL 18
               PIC A FROM "|".
           05 MOV-CONCEPTO-IMPAR LINE LINEA-MOV-ACTUAL COL 19
               PIC X(35) FROM MOV-TRANSFE-CONCEPTO.
           05 SEPARADOR-5-IMPAR LINE LINEA-MOV-ACTUAL COL 54
               PIC A FROM "|".
           05 MOV-IMPORTE-ENT-IMPAR
               SIGN IS LEADING SEPARATE
               LINE LINEA-MOV-ACTUAL COL 55
               PIC S9(7) FROM MOV-TRANSFE-IMPORTE-ENT.
           05 SEPARADOR-6-IMPAR LINE LINEA-MOV-ACTUAL COL 63
               PIC A FROM ",".
           05 MOV-IMPORTE-DEC-IMPAR LINE LINEA-MOV-ACTUAL COL 64
               PIC 99 FROM MOV-TRANSFE-IMPORTE-DEC.
           05 SEPARADOR-7-IMPAR LINE LINEA-MOV-ACTUAL COL 66
               PIC A FROM "|".
           05 MOV-SALDOPOS-ENT-IMPAR
               SIGN IS LEADING SEPARATE
               LINE LINEA-MOV-ACTUAL COL 67
               PIC S9(9) FROM MOV-TRANSFE-SALDOPOS-ENT.
           05 SEPARADOR-8-IMPAR LINE LINEA-MOV-ACTUAL COL 77
               PIC A FROM ",".
           05 MOV-SALDOPOS-DEC-IMPAR LINE LINEA-MOV-ACTUAL COL 78
               PIC 99 FROM MOV-TRANSFE-SALDOPOS-DEC.


       PROCEDURE DIVISION USING TNUM.
       IMPRIMIR-CABECERA.

           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'
           SET ENVIRONMENT 'COB_SCREEN_ESC'        TO 'Y'

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

       PCONSULTA-MOV.

           INITIALIZE DIA1-USUARIO.
           INITIALIZE MES1-USUARIO.
           INITIALIZE ANO1-USUARIO.
           INITIALIZE DIA2-USUARIO.
           INITIALIZE MES2-USUARIO.
           INITIALIZE ANO2-USUARIO.

           INITIALIZE EURENT1-USUARIO.
           INITIALIZE EURDEC1-USUARIO.
           INITIALIZE EURENT2-USUARIO.
           INITIALIZE EURDEC2-USUARIO.

           DISPLAY "Se  mostraran los ultimos movimientos,"
               LINE 8 COL 8.
           DISPLAY "de mas a menos recientes." LINE 8 COL 47.

           DISPLAY "Alternativamente, indique un intervalo"
               LINE 10 COL 8.
           DISPLAY "de fechas." LINE 10 COL 47.

           DISPLAY "Entre las fechas   /  /     y   /  /    "
               LINE 13 COL 20.

           DISPLAY "Enter - Aceptar" LINE 24 COL 02.
           DISPLAY "ESC - Cancelar" LINE 24 COL 65.

           ACCEPT FILTRO-MOVIMIENTOS ON EXCEPTION
               IF ESC-PRESSED
                   EXIT PROGRAM
               ELSE
                   GO TO PCONSULTA-MOV.

           IF DIA2-USUARIO = 0
               IF MES2-USUARIO = 0
                   IF ANO2-USUARIO = 0
                       MOVE 99   TO DIA2-USUARIO
                       MOVE 99   TO MES2-USUARIO
                       MOVE 9999 TO ANO2-USUARIO.

           IF EURENT2-USUARIO = 0
               IF EURDEC2-USUARIO = 0
                   IF EURENT1-USUARIO = 0
                       IF EURDEC1-USUARIO = 0
                           MOVE 9999999  TO EURENT2-USUARIO
                           MOVE 99       TO EURDEC2-USUARIO
                           MOVE -9999999  TO EURENT1-USUARIO
                           MOVE 99        TO EURDEC1-USUARIO.

       JUNTAR-FICHEROS.
           OPEN I-O F-MOVIMIENTOS.
               IF FSM <> 00
                  GO TO PSYS-ERR.

           OPEN I-O F-TRANSFERENCIAS.
               IF FSTR <> 00
                   GO TO PSYS-ERR.

           OPEN I-O F-MOV-TRANSFE.
               IF FSMTR = 05
                   GO TO TRY-AGAIN
               ELSE IF FSMTR <> 00
                   GO TO PSYS-ERR
               ELSE
                   GO TO LEER-MOVIMIENTOS
               END-IF.

       TRY-AGAIN.
           CLOSE F-MOV-TRANSFE.
           OPEN I-O F-MOV-TRANSFE.
               IF FSMTR <> 00
                   GO TO TRY-AGAIN.

       LEER-MOVIMIENTOS.
           READ F-MOV-TRANSFE NEXT RECORD
              AT END GO TO CERRAR-MOVIMIENTOS.

              DELETE F-MOV-TRANSFE RECORD END-DELETE.

           GO TO LEER-MOVIMIENTOS.

       CERRAR-MOVIMIENTOS.
           CLOSE F-MOV-TRANSFE.

           OPEN I-O F-MOV-TRANSFE.
               IF FSMTR <> 00
                   GO TO PSYS-ERR.

           MOVE 0 TO TR-MOV-NUM.

       PRINT-MOVIMIENTOS.

           READ F-MOVIMIENTOS NEXT RECORD
              AT END GO TO LEER-TRANSFERENCIAS.

              IF MOV-CONCEPTO <> "Ingreso"
                 AND MOV-CONCEPTO <> "Retirada" THEN
                     DISPLAY "TRANSFE" LINE 1 COL 1
                     ADD 1 TO TR-MOV-NUM
                     MOVE TR-MOV-NUM TO MOV-TRANSFE-NUM
      *              MOVE MOV-NUM TO MOV-TRANSFE-NUM
                     MOVE MOV-TARJETA TO MOV-TRANSFE-TARJETA
                     MOVE MOV-ANO TO MOV-TRANSFE-ANO
                     MOVE MOV-MES TO MOV-TRANSFE-MES
                     MOVE MOV-DIA TO MOV-TRANSFE-DIA
                     MOVE MOV-HOR TO MOV-TRANSFE-HOR
                     MOVE MOV-MIN TO MOV-TRANSFE-MIN
                     MOVE MOV-SEG TO MOV-TRANSFE-SEG
                     MOVE MOV-IMPORTE-ENT TO MOV-TRANSFE-IMPORTE-ENT
                     MOVE MOV-IMPORTE-DEC TO MOV-TRANSFE-IMPORTE-DEC
                     MOVE MOV-CONCEPTO TO MOV-TRANSFE-CONCEPTO
                     MOVE MOV-SALDOPOS-ENT TO MOV-TRANSFE-SALDOPOS-ENT
                     MOVE MOV-SALDOPOS-DEC TO MOV-TRANSFE-SALDOPOS-DEC

                     WRITE MOV-TRANSFE-REG
                END-IF.

              GO TO PRINT-MOVIMIENTOS.

       LEER-TRANSFERENCIAS.
           CLOSE F-MOVIMIENTOS.
           READ F-TRANSFERENCIAS NEXT RECORD AT END GO TO FIN-JUNTAR.

              IF TR-CONCEPTO = "Transferencia programada"
                 ADD 1 TO TR-MOV-NUM
                 MOVE TR-MOV-NUM TO MOV-TRANSFE-NUM
                 MOVE TR-ORIGEN TO MOV-TRANSFE-TARJETA
                 MOVE TR-ANO TO MOV-TRANSFE-ANO
                 MOVE TR-MES TO MOV-TRANSFE-MES
                 MOVE TR-DIA TO MOV-TRANSFE-DIA
                 MOVE 0 TO MOV-TRANSFE-HOR
                 MOVE 0 TO MOV-TRANSFE-MIN
                 MOVE 0 TO MOV-TRANSFE-SEG
                 MOVE TR-IMPORTE-ENT TO MOV-TRANSFE-IMPORTE-ENT
                 MOVE TR-IMPORTE-DEC TO MOV-TRANSFE-IMPORTE-DEC
                 MOVE TR-CONCEPTO TO MOV-TRANSFE-CONCEPTO
                 MOVE 0 TO MOV-TRANSFE-SALDOPOS-ENT
                 MOVE 0 TO MOV-TRANSFE-SALDOPOS-DEC

                 WRITE MOV-TRANSFE-REG

                 GO TO LEER-TRANSFERENCIAS
              END-IF.

      *       20/01/2020       14/02/2020
      *       TRANSFE PERIO PARA LOS DIAS 12

              MOVE MES1-USUARIO TO TR-MES.
              MOVE ANO1-USUARIO TO TR-ANO.

              IF DIA1-USUARIO > TR-DIA
                  ADD 1 TO TR-MES
                  IF TR-MES = 13
                     MOVE 1 TO TR-MES
                     ADD 1 TO TR-ANO
                  END-IF
              END-IF.

              COMPUTE FECHA-MAX = (ANO2-USUARIO * 10000)
                   + (MES2-USUARIO * 100)
                   + DIA2-USUARIO.

       BUCLE-SUMAR-MES.
              COMPUTE FECHA-MIN = (TR-ANO * 10000)
                              + (TR-MES * 100)
                              + TR-DIA.

              IF FECHA-MAX < FECHA-MIN THEN
                  GO TO LEER-TRANSFERENCIAS
              END-IF.

              ADD 1 TO MOV-NUM.
              MOVE MOV-NUM TO MOV-TRANSFE-NUM.
              MOVE TR-ORIGEN TO MOV-TRANSFE-TARJETA.
              MOVE TR-ANO TO MOV-TRANSFE-ANO.
              MOVE TR-MES TO MOV-TRANSFE-MES.
              MOVE TR-DIA TO MOV-TRANSFE-DIA.
              MOVE 0 TO MOV-TRANSFE-HOR.
              MOVE 0 TO MOV-TRANSFE-MIN.
              MOVE 0 TO MOV-TRANSFE-SEG.
              MOVE TR-IMPORTE-ENT TO MOV-TRANSFE-IMPORTE-ENT.
              MOVE TR-IMPORTE-DEC TO MOV-TRANSFE-IMPORTE-DEC.
              MOVE TR-CONCEPTO TO MOV-TRANSFE-CONCEPTO.
              MOVE 0 TO MOV-TRANSFE-SALDOPOS-ENT.
              MOVE 0 TO MOV-TRANSFE-SALDOPOS-DEC.

              WRITE MOV-TRANSFE-REG.

              ADD 1 TO TR-MES
              IF TR-MES = 13
                     MOVE 1 TO TR-MES
                     ADD 1 TO TR-ANO
              END-IF.

              GO TO BUCLE-SUMAR-MES.

       FIN-JUNTAR.
           CLOSE F-MOV-TRANSFE.
           CLOSE F-TRANSFERENCIAS.

           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.

      *-----------------------------------------------------------------
           OPEN INPUT F-MOV-TRANSFE.
               IF FSMTR <> 00
                   GO TO PSYS-ERR.

       POSICIONAR-FINAL.
           READ F-MOV-TRANSFE NEXT RECORD AT END GO PLECTURA-MOV.
               GO TO POSICIONAR-FINAL.

       PLECTURA-MOV.
           DISPLAY "FECHA" LINE 7 COL 8.
           DISPLAY "|" LINE 7 COL 18.
           DISPLAY "CONCEPTO" LINE 7 COL 35.
           DISPLAY "|" LINE 7 COL 54.
           DISPLAY "IMPORTE" LINE 7 COL 57.
           DISPLAY "|" LINE 7 COL 66.
           DISPLAY "SALDO" LINE 7 COL 71.

           DISPLAY "Re. pag - Esp. anteriores" LINE 24 COL 2.
           DISPLAY "ESC - Salir" LINE 24 COL 33.
           DISPLAY "Av. pag - Esp. posteriores" LINE 24 COL 54.

           MOVE 0 TO MOV-EN-PANTALLA.
           MOVE 7 TO LINEA-MOV-ACTUAL.


       LEER-PRIMEROS.
           READ F-MOV-TRANSFE PREVIOUS RECORD AT END GO WAIT-ORDER.
               MOVE 1 TO MOV-VALIDO.

               PERFORM FILTRADO THRU FILTRADO.

               IF MOV-VALIDO = 1
                   ADD 1 TO LINEA-MOV-ACTUAL
                   ADD 1 TO MOV-EN-PANTALLA
                   MOVE MOV-TRANSFE-NUM TO
                       REGISTROS-EN-PANTALLA(MOV-EN-PANTALLA)
                   MOVE 0 TO MOV-VALIDO
                   PERFORM MOSTRAR-MOVIMIENTO THRU MOSTRAR-MOVIMIENTO.

               IF MOV-EN-PANTALLA = 15
                   GO TO WAIT-ORDER.

               GO TO LEER-PRIMEROS.

       WAIT-ORDER.

           ACCEPT PRESSED-KEY WITH NO ECHO LINE 24 COL 79 ON EXCEPTION

              IF ESC-PRESSED THEN
                  CLOSE F-MOV-TRANSFE
                  EXIT PROGRAM
              END-IF

              IF PGDN-PRESSED THEN
                  GO TO FLECHA-ABAJO
              END-IF

              IF PGUP-PRESSED THEN
                  GO TO FLECHA-ARRIBA
              END-IF

           END-ACCEPT.

           GO TO WAIT-ORDER.

       FLECHA-ABAJO.
           MOVE REGISTROS-EN-PANTALLA(MOV-EN-PANTALLA)
               TO MOV-TRANSFE-NUM.
           READ F-MOV-TRANSFE INVALID KEY GO WAIT-ORDER.
           GO TO LEER-VIEJO.

       FLECHA-ARRIBA.
           MOVE REGISTROS-EN-PANTALLA(1) TO MOV-TRANSFE-NUM.
           READ F-MOV-TRANSFE INVALID KEY GO WAIT-ORDER.
           GO TO LEER-NUEVO.

       LEER-VIEJO.
           READ F-MOV-TRANSFE PREVIOUS RECORD
               AT END GO WAIT-ORDER.

               MOVE 1 TO MOV-VALIDO.
               PERFORM FILTRADO THRU FILTRADO.

               IF MOV-VALIDO = 1
                   MOVE 2 TO MOV-VALIDO
                   GO TO CONTROL-PANTALLA
               ELSE
                   GO TO LEER-VIEJO.

       LEER-NUEVO.
           READ F-MOV-TRANSFE NEXT RECORD
               AT END GO WAIT-ORDER.

               MOVE 1 TO MOV-VALIDO.
               PERFORM FILTRADO THRU FILTRADO.

               IF MOV-VALIDO = 1
                   MOVE 3 TO MOV-VALIDO
                   GO TO CONTROL-PANTALLA
               ELSE
                   GO TO LEER-NUEVO.

       CONTROL-PANTALLA.
           IF MOV-VALIDO = 2 THEN
               MOVE 0 TO MOV-VALIDO
               PERFORM REORDENAR-1 THRU REORDENAR-1
               GO TO WAIT-ORDER
           ELSE
               IF MOV-VALIDO = 3 THEN
                   MOVE 0 TO MOV-VALIDO
                   PERFORM REORDENAR-2 THRU REORDENAR-2
                   GO TO WAIT-ORDER
               ELSE
                   GO TO WAIT-ORDER
               END-IF
           END-IF.

       REORDENAR-1.
           MOVE 2 TO CONTADOR.
           MOVE MOV-EN-PANTALLA TO ITERACIONES.
           SUBTRACT 1 FROM ITERACIONES.

           PERFORM ITERACIONES TIMES
               MOVE REGISTROS-EN-PANTALLA(CONTADOR) TO COPIA-MOV
               SUBTRACT 1 FROM CONTADOR
               MOVE COPIA-MOV TO REGISTROS-EN-PANTALLA(CONTADOR)
               ADD 2 TO CONTADOR
           END-PERFORM.

           MOVE MOV-TRANSFE-NUM
           TO REGISTROS-EN-PANTALLA(MOV-EN-PANTALLA).
           PERFORM MOSTRAR-TABLA THRU MOSTRAR-TABLA.

           GO TO WAIT-ORDER.

       REORDENAR-2.
           MOVE MOV-EN-PANTALLA TO CONTADOR.
           SUBTRACT 1 FROM CONTADOR.
           MOVE MOV-EN-PANTALLA TO ITERACIONES.
           SUBTRACT 1 FROM ITERACIONES.


           PERFORM ITERACIONES TIMES
               MOVE REGISTROS-EN-PANTALLA(CONTADOR) TO COPIA-MOV
               ADD 1 TO CONTADOR
               MOVE COPIA-MOV TO REGISTROS-EN-PANTALLA(CONTADOR)
               SUBTRACT 2 FROM CONTADOR
           END-PERFORM.

           MOVE MOV-TRANSFE-NUM TO REGISTROS-EN-PANTALLA(1).

           PERFORM MOSTRAR-TABLA THRU MOSTRAR-TABLA.

           GO TO WAIT-ORDER.

       MOSTRAR-TABLA.
           MOVE 8 TO LINEA-MOV-ACTUAL.
           MOVE 1 TO CONTADOR.

           PERFORM MOV-EN-PANTALLA TIMES
               MOVE REGISTROS-EN-PANTALLA(CONTADOR) TO MOV-TRANSFE-NUM
               PERFORM READ-MOVIMIENTO THRU READ-MOVIMIENTO
               PERFORM MOSTRAR-MOVIMIENTO THRU MOSTRAR-MOVIMIENTO
               ADD 1 TO LINEA-MOV-ACTUAL
               ADD 1 TO CONTADOR
           END-PERFORM.

       READ-MOVIMIENTO.
           READ F-MOV-TRANSFE INVALID KEY GO TO PSYS-ERR.

       PSYS-ERR.
           CLOSE F-MOV-TRANSFE.

           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "Ha ocurrido un error interno" LINE 9 COL 25
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.
           DISPLAY "Vuelva mas tarde" LINE 11 COL 32
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.
           DISPLAY "Enter - Aceptar" LINE 24 COL 33.

       EXIT-ENTER.
           ACCEPT PRESSED-KEY LINE 24 COL 79
           IF ENTER-PRESSED
               EXIT PROGRAM
           ELSE
               GO TO EXIT-ENTER.


       FILTRADO.

           IF TNUM NOT = MOV-TRANSFE-TARJETA
              MOVE 0 TO MOV-VALIDO.

           COMPUTE FECHA-MIN = (ANO1-USUARIO * 10000)
                               + (MES1-USUARIO * 100)
                               + DIA1-USUARIO.

           COMPUTE FECHA-MOV = (MOV-TRANSFE-ANO * 10000)
                               + (MOV-TRANSFE-MES * 100)
                               + MOV-TRANSFE-DIA.

           COMPUTE FECHA-MAX = (ANO2-USUARIO * 10000)
                               + (MES2-USUARIO * 100)
                               + DIA2-USUARIO.

           IF MOV-TRANSFE-CONCEPTO <> "Transferencia periodica"
                  IF FECHA-MIN > FECHA-MOV
                      MOVE 0 TO MOV-VALIDO
                  END-IF
                  IF FECHA-MAX < FECHA-MOV
                      MOVE 0 TO MOV-VALIDO
                  END-IF
           END-IF.


       MOSTRAR-MOVIMIENTO.

           MOVE FUNCTION MOD(LINEA-MOV-ACTUAL, 2)
               TO MODULO-LIN-ACTUAL.

           IF MODULO-LIN-ACTUAL = 0
               DISPLAY FILA-MOVIMIENTO-PAR
           ELSE
               DISPLAY FILA-MOVIMIENTO-IMPAR.
