       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATMS.
       AUTHOR. KA HOU, CHOI.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT
              MASTER-FILE ASSIGN TO "master.txt"
              ORGANIZATION   IS LINE SEQUENTIAL
              ACCESS MODE    IS SEQUENTIAL
              FILE STATUS    IS MASTER-FILE-STATUS.

           SELECT
              T71-ONE-FILE ASSIGN TO "trans711.txt"
              ORGANIZATION   IS LINE SEQUENTIAL
              ACCESS MODE    IS SEQUENTIAL
              FILE STATUS    IS T71-ONE-FILE-STATUS.

           SELECT
              T71-THREE-FILE ASSIGN TO "trans713.txt"
              ORGANIZATION   IS LINE SEQUENTIAL
              ACCESS MODE    IS SEQUENTIAL
              FILE STATUS    IS T71-THREE-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       01  MASTER-RECORD.
           02 MSTR-ACCT-HOLDER-NAME         PIC X(20).
           02 MSTR-ACCT-INFO.
              03 MSTR-ACCT-NUMBER           PIC 9(16).
              03 MSTR-ACCT-PASSWORD         PIC 9(6).
           02 MSTR-ACCT-BALANCE             PIC S9(13)V9(2)
                                            SIGN LEADING SEPARATE.

       FD  T71-ONE-FILE.
       01  T71-ONE-RECORD.
           02 ONE-ACCT-NUMBER               PIC 9(16).
           02 ONE-OPERATION                 PIC A.
           02 ONE-AMOUNT                    PIC 9(5)V9(2).
           02 ONE-TIMESTAMP                 PIC 9(5).

       FD  T71-THREE-FILE.
       01  T71-THREE-RECORD.
           02 THREE-ACCT-NUMBER             PIC 9(16).
           02 THREE-OPERATION               PIC A.
           02 THREE-AMOUNT                  PIC 9(5)V9(2).
           02 THREE-TIMESTAMP               PIC 9(5).

       WORKING-STORAGE SECTION.
       01  MASTER-FILE-STATUS               PIC 99.
           88 MASTER-FILE-EOF-REACHED       VALUE 10.
           88 MASTER-FILE-NOT-FOUND         VALUE 35.
           88 MASTER-FILE-ALREADY-OPEN      VALUE 41.
       01  T71-ONE-FILE-STATUS              PIC 99.
           88 T71-ONE-FILE-ALREADY-OPEN     VALUE 41.
       01  T71-THREE-FILE-STATUS            PIC 99.
           88 T71-THREE-FILE-ALREADY-OPEN   VALUE 41.

       01  USER-ATM-CHOICE                  PIC 999.
           88 ATM-71-ONE                    VALUE 711 1.
           88 ATM-71-THREE                  VALUE 713 2.

       01  VALIDATE-ACCT-INFO-FOR           PIC 9.
           88 VALIDATING-USER               VALUE 1.
           88 VALIDATING-TARGET             VALUE 2.

       01  USER-ACCT-RECORD.
           02 USER-ACCT-INFO.
              03 USER-ACCT-NUMBER           PIC 9(16).
              03 USER-ACCT-PASSWORD         PIC 9(6).
           02 USER-ACCT-BALANCE             PIC S9(14)V9(2).

       01  USER-SERVICE-CHOICE              PIC A.
           88 DEPOSIT                       VALUE "D".
           88 WITHDRAWAL                    VALUE "W".
           88 TRANSFER                      VALUE "T".

       01  TARGET-ACCT-NUNBER               PIC 9(16).

       01  TRANSAC-AMOUNT                   PIC 9(5)V9(2).

       01  TRANSAC-BUFFER.
           02 TRANSAC-BUF-ACCT-NUMBER       PIC 9(16).
           02 TRANSAC-BUF-OPERATION         PIC A.
           02 TRANSAC-BUF-AMOUNT            PIC 9(5)V9(2).
           02 TRANSAC-BUF-TIMESTAMP         PIC 9(5) VALUE 0.

       01  TRANSFER-STATUS                  PIC 9 VALUE 0.
           88 INITIAL-TRANSFER-STATUS       VALUE 0.
           88 DONE-WITHDRAW                 VALUE 1.
           88 DONE-DEPOSIT                  VALUE 2.

       01  IF-CONTINUE-CHOICE               PIC X.
           88 CONTINUE-YES                  VALUE "Y".
           88 CONTINUE-NO                   VALUE "N".

       PROCEDURE DIVISION.
       CHECK-MASTER-FILE-EXISTS.
           OPEN INPUT MASTER-FILE.
           IF MASTER-FILE-NOT-FOUND
           THEN
              DISPLAY "[ABORT]: MASTER FILE NOT FOUND"
              STOP RUN
           END-IF.

           GO TO ATM-INITIALIZE.

       ATM-INITIALIZE.
           DISPLAY SPACES
           DISPLAY "##############################################"
           DISPLAY "##         Gringotts Wizarding Bank         ##"
           DISPLAY "##                 Welcome                  ##"
           DISPLAY "##############################################"
           GO TO CHOOSE-ATM.

       CHOOSE-ATM.
           DISPLAY "=> PLEASE CHOOSE THE ATM"
           DISPLAY "=> PRESS 1 FOR ATM 711"
           DISPLAY "=> PRESS 2 FOR ATM 713"
           ACCEPT USER-ATM-CHOICE.

           IF ATM-71-ONE OR ATM-71-THREE
           THEN
              GO TO INPUT-ACCT-INFO
           END-IF.

           DISPLAY "=> INVALID INPUT"
           GO TO CHOOSE-ATM.

       INPUT-ACCT-INFO.
           DISPLAY "=> ACCOUNT"
           ACCEPT USER-ACCT-NUMBER.

           DISPLAY "=> PASSWORD"
           ACCEPT USER-ACCT-PASSWORD.

           SET VALIDATING-USER TO TRUE.
           GO TO VALIDATE-ACCT-INFO.

       CHOOSE-SERVICE.
           DISPLAY "=> PLEASE CHOOSE YOUR SERVICE"
           DISPLAY "=> PRESS D FOR DEPOSIT"
           DISPLAY "=> PRESS W FOR WITHDRAWAL"
           DISPLAY "=> PRESS T FOR TRANSFER"
           ACCEPT USER-SERVICE-CHOICE.

           IF DEPOSIT OR WITHDRAWAL
           THEN
              GO TO INPUT-AMOUNT
           END-IF.

           IF TRANSFER
           THEN
              GO TO INPUT-TARGET-INFO
           END-IF.

           DISPLAY "=> INVALID INPUT"
           GO TO CHOOSE-SERVICE.

       INPUT-AMOUNT.
           DISPLAY "=> AMOUNT"
           ACCEPT TRANSAC-AMOUNT.

           DISPLAY
              "==> OPERATION: "
              USER-ACCT-BALANCE    SPACE
              USER-SERVICE-CHOICE  SPACE
              TRANSAC-AMOUNT.

           IF TRANSAC-AMOUNT < 0
           THEN
              DISPLAY "=> INVALID INPUT"
              GO TO INPUT-AMOUNT
           END-IF.

           IF NOT DEPOSIT AND TRANSAC-AMOUNT > USER-ACCT-BALANCE
           THEN
              DISPLAY "=> INSUFFICIENT BALANCE"
              GO TO INPUT-AMOUNT
           END-IF.

           GO TO GENERATE-TRANSAC-RECORD.

       INPUT-TARGET-INFO.
           DISPLAY "=> TARGET ACCOUT"
           ACCEPT TARGET-ACCT-NUNBER.

           IF TARGET-ACCT-NUNBER = USER-ACCT-NUMBER
           THEN
              DISPLAY "=> YOU CANNOT TRANSFER TO YOURSELF"
              GO TO INPUT-TARGET-INFO
           END-IF.

           SET VALIDATING-TARGET TO TRUE.
           GO TO VALIDATE-ACCT-INFO.

       GENERATE-TRANSAC-RECORD.
           IF DEPOSIT OR WITHDRAWAL
           THEN
              MOVE USER-ACCT-NUMBER      TO TRANSAC-BUF-ACCT-NUMBER
              MOVE USER-SERVICE-CHOICE   TO TRANSAC-BUF-OPERATION
              MOVE TRANSAC-AMOUNT        TO TRANSAC-BUF-AMOUNT
              GO TO WRITE-ATM-RECORD
           END-IF.

           IF TRANSFER
           THEN
              IF NOT DONE-WITHDRAW
              THEN
                 MOVE USER-ACCT-NUMBER   TO TRANSAC-BUF-ACCT-NUMBER
                 MOVE "W"                TO TRANSAC-BUF-OPERATION
                 MOVE TRANSAC-AMOUNT     TO TRANSAC-BUF-AMOUNT
                 SET DONE-WITHDRAW TO TRUE
                 GO TO WRITE-ATM-RECORD
              END-IF
              IF DONE-WITHDRAW
              THEN
                 MOVE TARGET-ACCT-NUNBER TO TRANSAC-BUF-ACCT-NUMBER
                 MOVE "D"                TO TRANSAC-BUF-OPERATION
                 MOVE TRANSAC-AMOUNT     TO TRANSAC-BUF-AMOUNT
                 SET DONE-DEPOSIT TO TRUE
                 GO TO WRITE-ATM-RECORD
              END-IF
           END-IF.

       WRITE-ATM-RECORD.
           IF
              NOT T71-ONE-FILE-ALREADY-OPEN OR
              NOT T71-THREE-FILE-ALREADY-OPEN
           THEN
              OPEN OUTPUT T71-ONE-FILE, T71-THREE-FILE
           END-IF.

           IF ATM-71-ONE
           THEN
              SET ATM-71-ONE TO TRUE
              WRITE T71-ONE-RECORD FROM TRANSAC-BUFFER
              DISPLAY "==> [ATM-711]: " TRANSAC-BUFFER
              ADD 1 TO TRANSAC-BUF-TIMESTAMP
           END-IF.

           IF ATM-71-THREE
           THEN
              SET ATM-71-THREE TO TRUE
              WRITE T71-THREE-RECORD FROM TRANSAC-BUFFER
              DISPLAY "==> [ATM-713]: " TRANSAC-BUFFER
              ADD 1 TO TRANSAC-BUF-TIMESTAMP
           END-IF.

           IF TRANSFER AND NOT DONE-DEPOSIT
           THEN
              GO TO GENERATE-TRANSAC-RECORD
           END-IF.

           SET INITIAL-TRANSFER-STATUS TO TRUE.
           GO TO CHOOSE-IF-CONTINUE.

       CHOOSE-IF-CONTINUE.
           DISPLAY "=> CONTINUE?"
           DISPLAY "=> N FOR NO"
           DISPLAY "=> Y FOR YES"
           ACCEPT IF-CONTINUE-CHOICE.

           IF CONTINUE-YES
           THEN
              GO TO ATM-INITIALIZE
           END-IF.

           IF NOT CONTINUE-NO
           THEN
              DISPLAY "=> INVALID INPUT"
              GO TO CHOOSE-IF-CONTINUE
           END-IF.

           CLOSE T71-ONE-FILE, T71-THREE-FILE.
           STOP RUN.

       VALIDATE-ACCT-INFO.
           IF NOT MASTER-FILE-ALREADY-OPEN
           THEN
              OPEN INPUT MASTER-FILE
           END-IF.

           READ MASTER-FILE
              AT END SET MASTER-FILE-EOF-REACHED TO TRUE
           END-READ.

           IF MASTER-FILE-EOF-REACHED
           THEN
              CLOSE MASTER-FILE
              IF VALIDATING-USER
              THEN
                 DISPLAY "=> INCORRECT ACCOUNT/PASSWORD"
                 GO TO INPUT-ACCT-INFO
              END-IF
              IF VALIDATING-TARGET
              THEN
                 DISPLAY "=> TARGET ACCOUNT DOES NOT EXIST"
                 GO TO INPUT-TARGET-INFO
              END-IF
           END-IF.

           IF VALIDATING-USER AND
              MSTR-ACCT-INFO = USER-ACCT-INFO
           THEN
              IF MSTR-ACCT-BALANCE IS NEGATIVE
              THEN
                 DISPLAY "=> NEGATIVE REMAINS TRANSACTION ABORT"
                 CLOSE MASTER-FILE
                 GO TO ATM-INITIALIZE
              END-IF
              MOVE MSTR-ACCT-BALANCE TO USER-ACCT-BALANCE
              CLOSE MASTER-FILE
              GO TO CHOOSE-SERVICE
           END-IF.

           IF VALIDATING-TARGET AND
              MSTR-ACCT-NUMBER = TARGET-ACCT-NUNBER
           THEN
              CLOSE MASTER-FILE
              GO TO INPUT-AMOUNT
           END-IF.

           GO TO VALIDATE-ACCT-INFO.

       END PROGRAM ATMS.
