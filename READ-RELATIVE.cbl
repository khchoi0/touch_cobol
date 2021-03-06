       IDENTIFICATION DIVISION. 
       PROGRAM-ID. READ-RELATIVE.
       AUTHOR. KA HOU, CHOI.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT 
              SUPPLIER-FILE ASSIGN TO "RELSUPP.DAT"
              ORGANIZATION IS RELATIVE
              ACCESS MODE IS DYNAMIC
              RELATIVE KEY IS SUPPLIER-KEY
              FILE STATUS IS SUPPLIER-STATUS.

       DATA DIVISION. 
       FILE SECTION. 
       FD SUPPLIER-FILE.
       01  SUPPLIER-REC.
           88 END-OF-FILE VALUE HIGH-VALUES.
           02 SUPPLIER-CODE PIC 99.
           02 SUPPLIER-NAME PIC X(20).
           02 SUPPLIER-ADDR PIC X(50).

       WORKING-STORAGE SECTION. 
       01  SUPPLIER-STATUS PIC X(2).
           88 RECORD-FOUND VALUE "00".
       01  SUPPLIER-KEY PIC 99.
       01  PRINT-SUPPLIER-REC.
           02 PRINT-SUPPLIER-CODE PIC BB99.
           02 PRINT-SUPPLIER-NAME PIC BBX(20).
           02 PRINT-SUPPLIER-ADDR PIC BBX(50).

       01  READ-TYPE PIC 9.
           88 DIRECT-READ VALUE 1.
           88 SEQUENTIAL-READ VALUE 2.

       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT SUPPLIER-FILE.
           DISPLAY "READ TYPE: DIRECT = 1, SEQ = 2 --> " 
              WITH NO ADVANCING
           ACCEPT READ-TYPE.

           IF DIRECT-READ THEN 
              DISPLAY "ENTER SUPPLIER CODE KEY (2 DIGITS) --> " 
                 WITH NO ADVANCING
              ACCEPT SUPPLIER-KEY
              READ SUPPLIER-FILE 
                 INVALID KEY DISPLAY "SUPP STATUS :-", SUPPLIER-STATUS 
              END-READ
              PERFORM DISPLAY-RECORD
           END-IF.

           IF SEQUENTIAL-READ THEN 
              READ SUPPLIER-FILE NEXT RECORD 
                 AT END SET END-OF-FILE TO TRUE
              END-READ
              PERFORM UNTIL END-OF-FILE
                 PERFORM DISPLAY-RECORD
                 READ SUPPLIER-FILE NEXT RECORD 
                    AT END SET END-OF-FILE TO TRUE 
                 END-READ
              END-PERFORM
           END-IF       

           CLOSE SUPPLIER-FILE.
           STOP RUN.

       DISPLAY-RECORD.
           IF RECORD-FOUND
              MOVE SUPPLIER-CODE TO PRINT-SUPPLIER-CODE
              MOVE SUPPLIER-NAME TO PRINT-SUPPLIER-NAME
              MOVE SUPPLIER-ADDR TO PRINT-SUPPLIER-ADDR
              DISPLAY PRINT-SUPPLIER-REC 
           END-IF.

       END PROGRAM READ-RELATIVE.
       