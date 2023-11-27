      *----------------------------------------------------------------*
      * Author: GABRIELA RODRIGUEZ
      * Date: 07/08/2023
      * Purpose: ARCHIVOS SECUENCIALES
      * Tectonics: cobc
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.

       PROGRAM-ID. CL09EJ01.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ENTRADA
           ASSIGN TO '../SUELDOS.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-ENTRADA.

       DATA DIVISION.

       FILE SECTION.

       FD ENTRADA.
       01 ENT-ARCHIVOS.
          05 ENT-ID-EMPLEADO                PIC 9(5).
          05 ENT-NOMBRE                     PIC X(15).
          05 ENT-APELLIDO                   PIC X(15).
          05 ENT-DIAS-TRABAJADO             PIC 9(2).
          05 ENT-SUELDO                     PIC 9(8)V9(2).

       WORKING-STORAGE SECTION.

       01 FS-STATUS.
          05 FS-ENTRADA                      PIC X(2).
             88 FS-ENTRADA-OK                VALUE '00'.
             88 FS-ENTRADA-NFD               VALUE '35'.
             88 FS-ENTRADA-EOF               VALUE '10'.

       01 WS-ENTRADA-EOF                     PIC X(1).
          88 WS-ENTRADA-EOF-YES              VALUE 'Y'.
          88 WS-ENTRADA-EOF-NO               VALUE 'N'.

       01 WS-ENTRADA-CANT-REG                PIC 9(5) VALUE 0.
       01 WS-ENTRADA-IMP-TOTAL               PIC 9(8)V9(2) VALUE 0.
       01 WS-ENTRADA-IMP-FORMATO             PIC $ZZ.ZZZ.ZZ9,99.
       01 WS-ENTRADA-CANT-REG-FORMAT         PIC Z9.



           01 WS-FILA.
                  05 FILLER           PIC X(03) VALUE SPACES.
                  05 WS-FILA-EMPLEADO PIC X(06).
                   05 FILLER          PIC X(03) VALUE SPACES.
                  05 WS-FILA-NOMBRE   PIC X(15).
                  05 FILLER           PIC X(03) VALUE SPACES.
                  05 WS-FILA-APELLIDO PIC X(15).
                  05 FILLER           PIC X(03) VALUE SPACES.
                  05 WS-FILA-DIAS-TRA PIC X(15).
                  05 FILLER           PIC X(03) VALUE SPACES.
                  05 WS-FILA-SUELDO   PIC $ZZ.ZZZ.ZZ9,99.
                  05 FILLER           PIC X(03) VALUE SPACES.



       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

           PERFORM 1000-ABRIR-ARCHIVO
              THRU 1000-ABRIR-ARCHIVO-EXIT.

           IF FS-ENTRADA-OK



              PERFORM 2000-PROCESAR-ARCHIVO
                 THRU 2000-PROCESAR-ARCHIVO-EXIT
                UNTIL WS-ENTRADA-EOF-YES

              PERFORM 3000-CERRAR-ARCHIVO
                 THRU 3000-CERRAR-ARCHIVO-EXIT

             PERFORM 4000-TOTALES-ARCHIVO
                THRU 4000-TOTALES-ARCHIVO-EXIT

           END-IF.

           STOP RUN.
      *----------------------------------------------------------------*
       1000-ABRIR-ARCHIVO.

           OPEN INPUT ENTRADA.

           EVALUATE FS-ENTRADA
               WHEN '00'
                    PERFORM 1500-LEER-ARCHIVO
                       THRU 1500-LEER-ARCHIVO-EXIT
               WHEN '35'
                    SET WS-ENTRADA-EOF-YES       TO TRUE
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE ENTRADA'
                    DISPLAY 'FILE STATUS: ' FS-ENTRADA
               WHEN OTHER
                    SET WS-ENTRADA-EOF-YES       TO TRUE
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE ENTRADA'
                    DISPLAY 'FILE STATUS: ' FS-ENTRADA
           END-EVALUATE.

       1000-ABRIR-ARCHIVO-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1500-LEER-ARCHIVO.

           READ ENTRADA
             AT END
                SET WS-ENTRADA-EOF-YES           TO TRUE
             NOT AT END
                SET WS-ENTRADA-EOF-NO            TO TRUE
                ADD 1                            TO WS-ENTRADA-CANT-REG
           END-READ.
           EVALUATE FS-ENTRADA
               WHEN '00'
                    CONTINUE
               WHEN '10'
                    CONTINUE
               WHEN OTHER
                    SET WS-ENTRADA-EOF-YES       TO TRUE
                    DISPLAY 'ERROR AL leer EL ARCHIVO DE ENTRADA'
                    DISPLAY 'FILE STATUS: ' FS-ENTRADA
           END-EVALUATE.





       1500-LEER-ARCHIVO-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2000-PROCESAR-ARCHIVO.



      *     ADD 1        TO WS-ENTRADA-CANT-REG

            COMPUTE WS-ENTRADA-IMP-TOTAL = WS-ENTRADA-IMP-TOTAL +
             ENT-SUELDO.

           MOVE WS-ENTRADA-IMP-TOTAL         TO WS-ENTRADA-IMP-FORMATO.



           MOVE ENT-ID-EMPLEADO         TO WS-FILA-EMPLEADO.
           MOVE ENT-NOMBRE              TO WS-FILA-APELLIDO.
           MOVE ENT-APELLIDO            TO WS-FILA-APELLIDO.
           MOVE ENT-DIAS-TRABAJADO      TO WS-FILA-DIAS-TRA.
           MOVE ENT-SUELDO              TO WS-FILA-SUELDO.


           DISPLAY WS-FILA.




      * Aca hacer display de los campos del registro.
      * Atentos que el Importe para mostrar, debemos usar el formateado
      * WS-ENTRADA-IMP-FORMATO

           PERFORM 1500-LEER-ARCHIVO
             THRU 1500-LEER-ARCHIVO-EXIT.

       2000-PROCESAR-ARCHIVO-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3000-CERRAR-ARCHIVO.

           CLOSE ENTRADA.

           IF NOT FS-ENTRADA-OK
              DISPLAY 'ERROR EN CLOSE DE ENTRADA: ' FS-ENTRADA
           END-IF.

       3000-CERRAR-ARCHIVO-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       4000-TOTALES-ARCHIVO.


           MOVE WS-ENTRADA-CANT-REG TO WS-ENTRADA-CANT-REG-FORMAT .

           DISPLAY "**********************Existen "
           WS-ENTRADA-CANT-REG-FORMAT
           " registros ingresados******************************".
      *Aca mostrar la cantidad de registros y el total de sueldos

           DISPLAY "+-+-+-+-+-+-+-+- La totalidad de los sueldos es de:"
            WS-ENTRADA-IMP-FORMATO "+-+-+-+-+-+-+-+-+-".



       4000-TOTALES-ARCHIVO-EXIT.
           EXIT.
      *----------------------------------------------------------------*

       END PROGRAM CL09EJ01.
