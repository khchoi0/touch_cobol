      *
      * CSCI3180 Principles of Programming Languages
      *
      * --- Declaration ---
      *
      * I declare that the assignment here submitted is original except
      * for source material explicitly acknowledged. I also acknowledge
      * that I am aware of University policy and regulations on honesty
      * in academic work, and of the disciplinary guidelines and
      * procedures applicable to breaches of such policy and
      * regulations, as contained in the website
      * https//www.cuhk.edu.hk/policy/academichonesty/
      *
      * Assignment 1
      * Name        : CHOI, Ka Hou
      * Student ID  : 1155135747
      * Email Addr  : 1155135747@link.cuhk.edu.hk
      *

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

      * ERROR HANDLING
       01  MASTER-FILE-STATUS               PIC 99.
           88 MASTER-FILE-EOF-REACHED       VALUE 10.
           88 MASTER-FILE-NOT-FOUND         VALUE 35.
           88 MASTER-FILE-ALREADY-OPEN      VALUE 41.
       01  T71-ONE-FILE-STATUS              PIC 99.
           88 T71-ONE-FILE-ALREADY-OPEN     VALUE 41.
       01  T71-THREE-FILE-STATUS            PIC 99.
           88 T71-THREE-FILE-ALREADY-OPEN   VALUE 41.

       01  USER-ATM-CHOICE                  PIC 999.
           88 ATM-71-ONE                    VALUE 1.
           88 ATM-71-THREE                  VALUE 2.

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

       01  TARGET-ACCT-NUMBER               PIC 9(16).

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

      * VALID INPUT
           IF ATM-71-ONE OR ATM-71-THREE
           THEN
              GO TO INPUT-ACCT-INFO
           END-IF.

      * INVALID INPUT
           DISPLAY "=> INVALID INPUT"
           GO TO CHOOSE-ATM.

       INPUT-ACCT-INFO.
           DISPLAY "=> ACCOUNT"
           ACCEPT USER-ACCT-NUMBER.

           DISPLAY "=> PASSWORD"
           ACCEPT USER-ACCT-PASSWORD.

      * VALIDATE USER ACCT INFO
           SET VALIDATING-USER TO TRUE.
           GO TO VALIDATE-ACCT-INFO.

       CHOOSE-SERVICE.
           DISPLAY "=> PLEASE CHOOSE YOUR SERVICE"
           DISPLAY "=> PRESS D FOR DEPOSIT"
           DISPLAY "=> PRESS W FOR WITHDRAWAL"
           DISPLAY "=> PRESS T FOR TRANSFER"
           ACCEPT USER-SERVICE-CHOICE.

      * VALID INPUT: D / W
           IF DEPOSIT OR WITHDRAWAL
           THEN
              GO TO INPUT-AMOUNT
           END-IF.

      * VALID INPUT: T
           IF TRANSFER
           THEN
              GO TO INPUT-TARGET-INFO
           END-IF.

      * INVALID INPUT
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

      * INVALID INPUT
           IF TRANSAC-AMOUNT < 0
           THEN
              DISPLAY "=> INVALID INPUT"
              GO TO INPUT-AMOUNT
           END-IF.

      * IF TRANSFERRING OR WITHDRAWING, THEN CHECK USER ACCOUNT BALANCE
           IF NOT DEPOSIT AND TRANSAC-AMOUNT > USER-ACCT-BALANCE
           THEN
              DISPLAY "=> INSUFFICIENT BALANCE"
              GO TO INPUT-AMOUNT
           END-IF.

      * VALID INPUT
           GO TO GENERATE-TRANSAC-RECORD.

       INPUT-TARGET-INFO.
           DISPLAY "=> TARGET ACCOUNT"
           ACCEPT TARGET-ACCT-NUMBER.

           IF TARGET-ACCT-NUMBER = USER-ACCT-NUMBER
           THEN
              DISPLAY "=> YOU CANNOT TRANSFER TO YOURSELF"
              GO TO INPUT-TARGET-INFO
           END-IF.

      * VALIDATE TARGET ACCT INFO
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
                 MOVE TARGET-ACCT-NUMBER TO TRANSAC-BUF-ACCT-NUMBER
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

      * USER CHOSE ATM 711
           IF ATM-71-ONE
           THEN
              WRITE T71-ONE-RECORD FROM TRANSAC-BUFFER
              DISPLAY "==> [ATM-711]: " TRANSAC-BUFFER
              ADD 1 TO TRANSAC-BUF-TIMESTAMP
           END-IF.

      * USER CHOSE ATM713
           IF ATM-71-THREE
           THEN
              WRITE T71-THREE-RECORD FROM TRANSAC-BUFFER
              DISPLAY "==> [ATM-713]: " TRANSAC-BUFFER
              ADD 1 TO TRANSAC-BUF-TIMESTAMP
           END-IF.

      * TRANSFERRING: AFTER WITHDRAWAL, THEN GENERATE DEPOSIT RECORD
           IF TRANSFER AND NOT DONE-DEPOSIT
           THEN
              GO TO GENERATE-TRANSAC-RECORD
           END-IF.

      * FINISHED TRANSACTION RECORD WRITE
           SET INITIAL-TRANSFER-STATUS TO TRUE.
           GO TO CHOOSE-IF-CONTINUE.

       CHOOSE-IF-CONTINUE.
           DISPLAY "=> CONTINUE?"
           DISPLAY "=> N FOR NO"
           DISPLAY "=> Y FOR YES"
           ACCEPT IF-CONTINUE-CHOICE.

      * VALID INPUT: Y
           IF CONTINUE-YES
           THEN
              GO TO ATM-INITIALIZE
           END-IF.

      * VALID INPUT: N
           IF CONTINUE-NO
           THEN
              CLOSE T71-ONE-FILE, T71-THREE-FILE
              STOP RUN
           END-IF.

      * INVALID INPUT
           DISPLAY "=> INVALID INPUT"
           GO TO CHOOSE-IF-CONTINUE.

       VALIDATE-ACCT-INFO.
           IF NOT MASTER-FILE-ALREADY-OPEN
           THEN
              OPEN INPUT MASTER-FILE
           END-IF.

           READ MASTER-FILE
              AT END SET MASTER-FILE-EOF-REACHED TO TRUE
           END-READ.

      * RECORD NOT FOUND
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

      * USER RECORD FOUND
           IF VALIDATING-USER AND
              MSTR-ACCT-INFO = USER-ACCT-INFO
           THEN

      * USER BALANCE IS NEGATIVE
              IF MSTR-ACCT-BALANCE IS NEGATIVE
              THEN
                 DISPLAY "=> NEGATIVE REMAINS TRANSACTION ABORT"
                 CLOSE MASTER-FILE
                 GO TO ATM-INITIALIZE
              END-IF

      * USER BALANCE IS POSITIVE
              MOVE MSTR-ACCT-BALANCE TO USER-ACCT-BALANCE
              CLOSE MASTER-FILE
              GO TO CHOOSE-SERVICE
           END-IF.

      * TARGET RECORD FOUND
           IF VALIDATING-TARGET AND
              MSTR-ACCT-NUMBER = TARGET-ACCT-NUMBER
           THEN
              CLOSE MASTER-FILE
              GO TO INPUT-AMOUNT
           END-IF.

      * CONTINUE SEARCHING LOOP
           GO TO VALIDATE-ACCT-INFO.

       END PROGRAM ATMS.
