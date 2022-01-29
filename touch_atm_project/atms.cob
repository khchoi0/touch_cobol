       IDENTIFICATION DIVISION. 
       PROGRAM-ID. ATMS.
       AUTHOR. KA HOU, CHOI.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT 
              MASTER-FILE ASSIGN TO "master.txt"
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS IS MASTER-FILE-STATUS.

           SELECT 
              T71-ONE-FILE ASSIGN TO "trans711.txt"
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS IS T71-ONE-FILE-STATUS.

           SELECT 
              T71-THREE-FILE ASSIGN TO "trans713.txt"
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS IS T71-THREE-FILE-STATUS.

       DATA DIVISION. 
       FILE SECTION. 
       FD  MASTER-FILE.
       01  MASTER-RECORD.
           02 MSTR-ACCT-HOLDER-NAME   PIC A(20).
           02 MSTR-ACCT-INFO.
              03 MSTR-ACCT-NUMBER     PIC 9(16).
              03 MSTR-ACCT-PASSWORD   PIC 9(6).
           02 MSTR-ACCT-SIGN          PIC X.
              88 ACCT-POSITIVE        VALUE "+".
              88 ACCT-NEGATIVE        VALUE "-".
           02 MSTR-ACCT-BALANCE       PIC 9(13)V9(2).

       FD  T71-ONE-FILE.
       01  T71-ONE-RECORD.
           02 ONE-ACCT-NUMBER         PIC 9(16).
           02 ONE-OPERATION           PIC A.
           02 ONE-AMOUNT              PIC 9(5)V9(2).
           02 ONE-TIMESTAMP           PIC 9(5).

       FD  T71-THREE-FILE.
       01  T71-THREE-RECORD.
           02 THREE-ACCT-NUMBER       PIC 9(16).
           02 THREE-OPERATION         PIC A.
           02 THREE-AMOUNT            PIC 9(5)V9(2).
           02 THREE-TIMESTAMP         PIC 9(5).

       WORKING-STORAGE SECTION. 
       01  MASTER-FILE-STATUS               PIC 99.
           88 MASTER-FILE-ALREADY-OPEN      VALUE 41.
       01  T71-ONE-FILE-STATUS              PIC 99.
           88 T71-ONE-FILE-ALREADY-OPEN     VALUE 41.
       01  T71-THREE-FILE-STATUS            PIC 99.
           88 T71-THREE-FILE-ALREADY-OPEN   VALUE 41.

       01  MASTER-FILE-EOF-STATUS                PIC 9.
           88 EOF-REACHED                   VALUE 1.

       01  USER-ATM-CHOICE                  PIC 9.
           88 ATM-71-ONE                    VALUE 1.
           88 ATM-71-THREE                  VALUE 2.

       01  VALIDATE-ACCT-INFO-FOR           PIC 9.
           88 VALIDATING-USER-INFO          VALUE 1.
           88 VALIDATING-TARGET-INFO        VALUE 2.

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

       01  TRANSACTION-AMOUNT               PIC 9(5)V9(2).

       01  TRANSACTION-RECORD.
           02 T-REC-ACCT-NUMBER             PIC 9(16).
           02 T-REC-OPERATION               PIC A.
           02 T-REC-AMOUNT                  PIC 9(5)V9(2).
           02 T-REC-TIMESTAMP               PIC 9(5) VALUE 0.

       01  TRANSFER-RECORD-STATUS           PIC 9.
           88 WITHDRAWED                    VALUE 1.
           88 DEPOSITED                     VALUE 2.

       01  IF-CONTINUE-CHOICE               PIC A.
           88 CONTINUE-YES                  VALUE "Y".
           88 CONTINUE-NO                   VALUE "N".

       PROCEDURE DIVISION.
       MAIN.
           IF NOT T71-ONE-FILE-ALREADY-OPEN 
           THEN 
              OPEN OUTPUT T71-ONE-FILE 
           END-IF.
           
           IF NOT T71-THREE-FILE-ALREADY-OPEN 
           THEN 
              OPEN OUTPUT T71-THREE-FILE 
           END-IF.

           GO TO ATM-INITIALIZE.

           STOP RUN.

       ATM-INITIALIZE.
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

           IF ATM-71-ONE 
           THEN 
              GO TO INPUT-ACCT-INFO 
           END-IF.

           IF ATM-71-THREE 
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
           
           MOVE 1 TO VALIDATE-ACCT-INFO-FOR.
           GO TO VALIDATE-ACCT-INFO.

       CHOOSE-SERVICE.
           DISPLAY "PLEASE CHOOSE YOUR SERVICE"
           DISPLAY "PRESS D FOR DEPOSIT"
           DISPLAY "PRESS W FOR WITHDRAWAL"
           DISPLAY "PRESS T FOR TRANSFER"
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
           ACCEPT TRANSACTION-AMOUNT.

           DISPLAY USER-ACCT-BALANCE SPACE TRANSACTION-AMOUNT

           IF TRANSACTION-AMOUNT < 0
           THEN 
              DISPLAY "=> INVALID INPUT"
              GO TO INPUT-AMOUNT
           END-IF

           IF DEPOSIT 
           THEN 
              GO TO GENERATE-TRANSACTION-RECORD
           END-IF.

           IF WITHDRAWAL OR TRANSFER 
           THEN 
              IF TRANSACTION-AMOUNT > USER-ACCT-BALANCE 
              THEN 
                 DISPLAY "INSUFFICIENT BALANCE"
                 GO TO INPUT-AMOUNT 
              END-IF 
              GO TO GENERATE-TRANSACTION-RECORD
           END-IF.

       INPUT-TARGET-INFO.
           DISPLAY "=> TARGET ACCOUT"
           ACCEPT TARGET-ACCT-NUNBER.

           IF TARGET-ACCT-NUNBER = USER-ACCT-NUMBER 
           THEN 
              DISPLAY "YOU CANNOT TRANSFER TO YOURSELF"
              GO TO INPUT-TARGET-INFO 
           END-IF .

           MOVE 2 TO VALIDATE-ACCT-INFO-FOR.
           GO TO VALIDATE-ACCT-INFO.

       GENERATE-TRANSACTION-RECORD.
           IF DEPOSIT OR WITHDRAWAL 
           THEN 
              MOVE USER-ACCT-NUMBER TO T-REC-ACCT-NUMBER
              MOVE USER-SERVICE-CHOICE TO T-REC-OPERATION
              MOVE TRANSACTION-AMOUNT TO T-REC-AMOUNT
              DISPLAY USER-ATM-CHOICE ": " TRANSACTION-RECORD
              GO TO WRITE-ATM-RECORD
           END-IF.
           
           IF TRANSFER
           THEN 
              IF NOT WITHDRAWED 
              THEN 
                 MOVE USER-ACCT-NUMBER TO T-REC-ACCT-NUMBER
                 MOVE "W" TO T-REC-OPERATION
                 MOVE TRANSACTION-AMOUNT TO T-REC-AMOUNT
                 DISPLAY USER-ATM-CHOICE ": " TRANSACTION-RECORD 
                 MOVE 1 TO TRANSFER-RECORD-STATUS
                 GO TO WRITE-ATM-RECORD
              END-IF

              MOVE TARGET-ACCT-NUNBER TO T-REC-ACCT-NUMBER
              MOVE "D" TO T-REC-OPERATION
              MOVE TRANSACTION-AMOUNT TO T-REC-AMOUNT
              DISPLAY USER-ATM-CHOICE ": " TRANSACTION-RECORD 
              MOVE 2 TO TRANSFER-RECORD-STATUS
              GO TO WRITE-ATM-RECORD
           END-IF.

       WRITE-ATM-RECORD.
           IF ATM-71-ONE
           THEN 
              WRITE T71-ONE-RECORD FROM TRANSACTION-RECORD
              ADD 1 TO T-REC-TIMESTAMP
           END-IF.

           IF ATM-71-THREE
           THEN
              WRITE T71-THREE-RECORD FROM TRANSACTION-RECORD
              ADD 1 TO T-REC-TIMESTAMP
           END-IF.

           IF TRANSFER AND NOT DEPOSITED
           THEN 
              GO TO GENERATE-TRANSACTION-RECORD
           END-IF.
           
           MOVE 0 TO TRANSFER-RECORD-STATUS
           GO TO CHOOSE-IF-CONTINUE.           

       CHOOSE-IF-CONTINUE.
           DISPLAY "=> CONTINUE?"
           DISPLAY "=>  N FOR NO"
           DISPLAY "=>  Y FOR YES"
           ACCEPT IF-CONTINUE-CHOICE.

           IF CONTINUE-YES
           THEN 
              GO TO MAIN
           END-IF.

           IF NOT CONTINUE-NO 
           THEN 
              DISPLAY "=> INVALID INPUT"
              GO TO CHOOSE-IF-CONTINUE 
           END-IF. 

           CLOSE T71-ONE-FILE, T71-THREE-FILE.
           STOP RUN.

       VALIDATE-ACCT-INFO.
           IF NOT MASTER-FILE-ALREADY-OPEN THEN 
              OPEN INPUT MASTER-FILE
           END-IF.

           READ MASTER-FILE
              AT END SET EOF-REACHED TO TRUE
           END-READ.

           IF EOF-REACHED 
           THEN 
              CLOSE MASTER-FILE 
              MOVE 0 TO MASTER-FILE-EOF-STATUS 
              IF VALIDATING-USER-INFO 
              THEN
                 DISPLAY "INCORRECT ACCOUNT/PASSWORD"
                 GO TO INPUT-ACCT-INFO
              END-IF
              IF VALIDATING-TARGET-INFO 
              THEN
                 DISPLAY "TARGET ACCOUNT DOES NOT EXIST"
                 GO TO INPUT-TARGET-INFO
              END-IF
           END-IF.
           
           IF VALIDATING-USER-INFO AND 
              MSTR-ACCT-INFO = USER-ACCT-INFO
           THEN 
              DISPLAY MASTER-RECORD 
              IF ACCT-NEGATIVE 
              THEN 
                 DISPLAY "NEGATIVE REMAINS TRANSACTION ABORT"
                 CLOSE MASTER-FILE 
                 GO TO MAIN
              END-IF 
              IF ACCT-POSITIVE
              THEN
                 MOVE MSTR-ACCT-BALANCE TO USER-ACCT-BALANCE
              CLOSE MASTER-FILE
              GO TO CHOOSE-SERVICE
           END-IF.

           IF VALIDATING-TARGET-INFO AND 
              MSTR-ACCT-NUMBER = TARGET-ACCT-NUNBER 
           THEN 
              CLOSE MASTER-FILE
              GO TO INPUT-AMOUNT
           END-IF.

           GO TO VALIDATE-ACCT-INFO.    

       END PROGRAM ATMS.
