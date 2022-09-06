       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCCACT2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.


           SELECT ENTRADA ASSIGN TO "ENTRADATXT.TXT"
           FILE STATUS IS FS-ENTRADA.

           SELECT SALIDA ASSIGN TO "SALIDA-CCCACT.TXT"
           FILE STATUS IS FS-SALIDA.

           SELECT ORDENADO ASSIGN TO "SORT".


       I-O-CONTROL.

       DATA DIVISION.
       FILE SECTION.
       FD ENTRADA
          RECORDING MODE IS F
          RECORD CONTAINS 52 CHARACTERS.

       01 REG-CCCACT.
           05 CCCACT-CLIENTE                 PIC X(10).
           05 CCCACT-SUCURSAL                PIC 9(06).
           05 CCCACT-CODIGO                  PIC X(03).
           05 CCCACT-FECHA.
               10 CCCACT-ANIO                PIC 9(04).
               10 CCCACT-MES                 PIC 9(02).
               10 CCCACT-DIA                 PIC 9(02).
           05 CCCACT-HORA.
               10 CCCACT-HH                  PIC 9(02).
               10 CCCACT-MM                  PIC 9(02).
               10 CCCACT-SS                  PIC 9(02).
           05 CCCACT-IMPORTE                 PIC S9(12)V9(04).
           05 CCCACT-TIPO                    PIC X(03).



       FD SALIDA
          RECORDING MODE F.
       01 REG-SALIDA                         PIC X(184).


       SD ORDENADO.

          01 REG-ORDENADO.
           05 CLIENTE                 PIC X(10).
           05 SUCURSAL                PIC 9(06).
           05 CODIGO                  PIC X(03).
           05 FECHA.
               10 AA                  PIC 9(04).
               10 MM                  PIC 9(02).
               10 DD                  PIC 9(02).
           05 CCCACT-HORA.
               10 HORA                PIC 9(02).
               10 MIN                 PIC 9(02).
               10 SEG                 PIC 9(02).
           05 IMPORTE                 PIC S9(12)V9(04).
           05 TIPO                    PIC X(03).

      *-----------------------------------------------------------------

       WORKING-STORAGE SECTION.

      *---------------------------------------------
      * VARIABLES PARA CONTROL DE LOS FILE STATUS.
      *---------------------------------------------

       01 WS-FILE-STATUS.
           05 FS-ENTRADA                    PIC X(02).
             88 ENTRADA-OK                      VALUE "00".
             88 EOF-ENTRADA                     VALUE "10".
           05 FS-SALIDA                     PIC X(02).
             88 LISTADO-OK                      VALUE "00".

      *---------------------------------------------
      * VARIABLES PARA AUXILIARES DATOS DE ENTRADA.
      *---------------------------------------------

       01 AUX-CCCACT.
           05 AUX-CCCACT-CLIENTE            PIC X(10).
           05 AUX-CCCACT-SUCURSAL           PIC 9(06).
           05 AUX-CCCACT-CODIGO             PIC X(03).
           05 AUX-CCCACT-FECHA.
               10 AUX-CCCACT-ANIO           PIC 9(04).
               10 AUX-CCCACT-MES            PIC 9(02).
               10 AUX-CCCACT-DIA            PIC 9(02).
           05 AUX-CCCACT-HORA.
               10 AUX-CCCACT-HH             PIC 9(02).
               10 AUX-CCCACT-MM             PIC 9(02).
               10 AUX-CCCACT-SS             PIC 9(02).
           05 AUX-CCCACT-IMPORTE            PIC S9(12)V9(04).
           05 AUX-CCCACT-TIPO               PIC X(03).
      *-------- 52

       01 WS-FECHA-PROCESO-SISTEMA.
           05 WS-PROCESO-ANIO               PIC 9(04).
           05 WS-PROCESO-MES                PIC 9(02).
           05 WS-PROCESO-DIA                PIC 9(02).

      *--------------------------------------------------
      * ARMADO DE CABECERAS, TITULOS Y COLUMNAS DE DATOS.
      *--------------------------------------------------

       01 WS-CABECERA-PRINCIPAL.
           05 FILLER                       PIC X(45) VALUE SPACES.
           05 WS-TITULO                    PIC X(41)
                VALUE "LISTADO DE CUENTAS CORRIENTES ACTUALIZADO".
           05 FILLER                       PIC X(45) VALUE SPACES.


       01 WS-SUBTITULOS.
           05 FILLER                       PIC X(16) VALUE SPACES.
           05 WS-NOMBRE-BANCO              PIC X(14)
                   VALUE "BANCO: GALICIA".
           05 FILLER                       PIC X(90) VALUE SPACES.
           05 WS-FECHA-SISTEMA.
               10 WS-DIA                   PIC 9(02) VALUE ZEROES.
               10 FILLER                   PIC X(01) VALUE "/".
               10 WS-MES                   PIC 9(02) VALUE ZEROES.
               10 FILLER                   PIC X(01) VALUE "/".
               10 WS-ANIO                  PIC 9(04) VALUE ZEROES.


       01 WS-COLUMNAS.
           05 FILLER                       PIC X(02).
           05 COLUMNA-CLIENTE              PIC X(11)
                 VALUE "COD-CLIENTE".
           05 FILLER                       PIC X(05).
           05 COLUMNA-SUCURSAL             PIC X(08)
                 VALUE "SUCURSAL".
           05 FILLER                       PIC X(10).
           05 COLUMNA-CODIGO               PIC X(06)
                 VALUE "CODIGO".
           05 FILLER                       PIC X(10).
           05 COLUMNA-FECHA                PIC X(12)
                 VALUE "FECHA Y HORA".
           05 FILLER                       PIC X(15).
           05 COLUMNA-IMPORTE              PIC X(07)
                 VALUE "IMPORTE".
           05 FILLER                       PIC X(10).
           05 COLUMNA-TIPO                 PIC X(18)
                 VALUE "TIPO DE MOVIMIENTO".
           05 FILLER                       PIC X(14).


       01 WS-DATOS-CCCACT.
           05 FILLER                       PIC X(03).
           05 WS-DATO-CLIENTE              PIC 9(10).
           05 FILLER                       PIC X(08).
           05 WS-DATO-SUCURSAL             PIC 9(06).
           05 FILLER                       PIC X(18).
           05 WS-DATO-CODIGO               PIC X(03).
           05 FILLER                       PIC X(10).
           05 WS-DATO-FECHA.
               10 DATO-DIA                 PIC 9(02).
               10 FILLER                   PIC X(01) VALUE "/".
               10 DATO-MES                 PIC 9(02).
               10 FILLER                   PIC X(01) VALUE "/".
               10 DATO-ANIO                PIC 9(04).
           05 FILLER                       PIC X(02).
           05 WS-DATO-HORA.
               10 DATO-HH                  PIC 9(02).
               10 FILLER                   PIC X(01) VALUE ":".
               10 DATO-MM                  PIC 9(02).
               10 FILLER                   PIC X(01) VALUE ":".
               10 DATO-SS                  PIC 9(02).
           05 FILLER                       PIC X(02).
           05 WS-DATO-IMPORTE              PIC $ZZZ.ZZZ.ZZZ.Z99,9999.
           05 FILLER                       PIC X(15).
           05 WS-DATO-TIPO                 PIC X(03).
           05 FILLER                       PIC X(70) VALUE SPACES.
      *------ 130
      *--------------------------><--------><---------------------------

       01 LINEA-TITULO-PRINCIPAL           PIC X(133) VALUE SPACES.

      *--------------------------><--------><---------------------------

       01 LINEA-GUIONADA                   PIC X(133) VALUE ALL "-".

      *--------------------------><--------><---------------------------
      *  DECLARACION DE VARIABLES PARA CORTE DE CONTROL
      *--------------------------><--------><---------------------------
       01 CLIENTE-ACTUAL                   PIC 9(10).
       01 CLIENTE-ANTERIOR                 PIC 9(10).


       PROCEDURE DIVISION.

       00000-ORDEN-SORT.

           INITIALIZE AUX-CCCACT

           ACCEPT WS-FECHA-PROCESO-SISTEMA FROM DATE YYYYMMDD.
           MOVE WS-PROCESO-ANIO            TO WS-ANIO
           MOVE WS-PROCESO-MES             TO WS-MES
           MOVE WS-PROCESO-DIA             TO WS-DIA

           SORT ORDENADO
      * USO EL CODIGO DEL CLIENTE PARA ORDENAR
           ON ASCENDING KEY CLIENTE


           INPUT PROCEDURE 00010-CARGAR

           OUTPUT PROCEDURE 00020-SALIDA

           IF SORT-RETURN NOT = ZERO
               DISPLAY "SORT-RETURN : " SORT-RETURN
           END-IF.

               CLOSE ENTRADA SALIDA.

           STOP RUN.

      *--------------------------><--------><---------------------------

       00010-CARGAR.

           OPEN INPUT ENTRADA
                OUTPUT SALIDA

           PERFORM 00030-LECTURA-SORT.

           PERFORM UNTIL EOF-ENTRADA

           MOVE REG-CCCACT TO REG-ORDENADO

           RELEASE REG-ORDENADO

           DISPLAY CLIENTE

           PERFORM 00030-LECTURA-SORT

           END-PERFORM.

           DISPLAY "--------------------------------------------------"

           .

      *--------------------------><--------><---------------------------

       00020-SALIDA.

           MOVE WS-CABECERA-PRINCIPAL TO LINEA-TITULO-PRINCIPAL
           WRITE REG-SALIDA FROM LINEA-TITULO-PRINCIPAL
           AFTER ADVANCING 2 LINES

           MOVE WS-SUBTITULOS TO LINEA-TITULO-PRINCIPAL
           WRITE REG-SALIDA FROM LINEA-TITULO-PRINCIPAL
           AFTER ADVANCING 2 LINES

           MOVE WS-COLUMNAS TO LINEA-TITULO-PRINCIPAL
           WRITE REG-SALIDA FROM LINEA-TITULO-PRINCIPAL
           AFTER ADVANCING 2 LINES

           SET ENTRADA-OK TO TRUE

           RETURN ORDENADO
           AT END SET EOF-ENTRADA TO TRUE
           END-RETURN

           PERFORM UNTIL EOF-ENTRADA

                 DISPLAY CLIENTE

                 RETURN ORDENADO
                 AT END SET EOF-ENTRADA TO TRUE

                 END-RETURN

           MOVE REG-ORDENADO         TO AUX-CCCACT

           MOVE AUX-CCCACT-CLIENTE   TO WS-DATO-CLIENTE
           MOVE AUX-CCCACT-SUCURSAL  TO WS-DATO-SUCURSAL
           MOVE AUX-CCCACT-CODIGO    TO WS-DATO-CODIGO

           MOVE AUX-CCCACT-DIA       TO DATO-DIA
           MOVE AUX-CCCACT-MES       TO DATO-MES
           MOVE AUX-CCCACT-ANIO      TO DATO-ANIO

           MOVE AUX-CCCACT-HH        TO DATO-HH
           MOVE AUX-CCCACT-MM        TO DATO-MM
           MOVE AUX-CCCACT-SS        TO DATO-SS

           MOVE AUX-CCCACT-IMPORTE   TO WS-DATO-IMPORTE
           MOVE AUX-CCCACT-TIPO      TO WS-DATO-TIPO

           MOVE WS-DATOS-CCCACT      TO LINEA-TITULO-PRINCIPAL

           WRITE REG-SALIDA FROM LINEA-TITULO-PRINCIPAL
           AFTER ADVANCING 2 LINES

           END-PERFORM.
      *--------------------------><--------><---------------------------

      * FUNCION PARA LECTURA DEL FICHERO QUE SORT-EARE

       00030-LECTURA-SORT.

           READ ENTRADA.

      *     DISPLAY "ENTRADA " FS-ENTRADA.

       END PROGRAM CCCACT2.
