       IDENTIFICATION DIVISION.
       PROGRAM-ID. CGBPR2.
       AUTHOR. GARRETT BURNS.

      *TO RUN: COBC -XO PROGRAM2.EXE --STD=MF PR2CGB.CBL
      ***********************************************
      *
      * INPUT:
      *   THE INPUT RECORD LAYOUT CONTAINS THE FOLLOWING DATA FOR EACH RECORD:
      *       1. WAREHOUSE ID
      *       2. EMPLOYEE ID
      *       3. EMPLOYEE POSITION
      *       4. EMPLOYEE LAST NAME
      *       5. EMPLOYEE FIRST NAME
      *       6. HIRE DATE
      *       7. STARTING SALARY
      *       8. DATE OF LAST PAY INCREASE
      *       9. CURRENT SALARY
      *       10. UNION DUES
      *       11. INSURANCE
      *
      * *********************************************
      *
      * OUTPUT:
      *   THE OUTPUT REPORT CONTAINS THE FOLLOWING INFORMATION:
      * ********
      *   DETAIL LINE:   
      *       1. WAREHOUSE ID
      *       2. EMPLOYEE ID
      *       3. EMPLOYEE POSITION
      *       4. EMPLOYEE LAST NAME
      *       5. INCREASED CURRENT SALARY
      *       6. INCREASED UNION DUES
      *       7. INCREASED INSURANCE
      * ********
      *   FINAL TOTALS:
      *       1. PROVIDE A DETAILED EMPLOYEE SALARY REPORT ON DRAKEA, LTD.
      *       2. SEPARATE EMPLOYEES INTO THEIR INDIVIDUAL WAREHOUSES, BASED ON WAREHOUSE ID
      *       2. FIND AND IMPLEMENT INCREASED CURRENT SALARY FOR ALL EMPLOYEES
      *       3. FIND AND IMPLEMENT INCREASED UNION DUES FOR ALL EMPLOYEES
      *       3. FIND AND IMPLEMENT INCREASED INSURANCE FOR ALL EMPLOYEES
      * ********
      *   CALCULATIONS:
      *       INCREASE ALL EMPLOYEE'S CURRENT SALARY BY 5% (1.05)
      *       INCREASE UNION DUES BY 3% (1.03)
      *       INCREASE INSURANCE BY 5% (1.05)
      
      *       INCREASED CURRENT SALARY =
      *           CURRENT SALARY OF EMPLOYEE * 1.05
      *       TOTAL INCREASED CURRENT SALARY FOR ALL EMPLOYEES =
      *           THE SUM OF ALL INCREASED CURRENT SALARIES + TOTAL INCREASED CURRENT SALARIES
      *
      *       INCREASED UNION DUES =
      *           CURRENT UNION DUES * 1.03
      *       TOTAL INCREASED UNION DUES =
      *           THE SUM OF ALL INCREASED UNION DUES + TOTAL INCREASED UNION DUES
      *       
      *       INCREASED INSURANCE =
      *           CURRENT INSURANCE * 1.05
      *       TOTAL INCREASED INSURANCE FOR ALL EMPLOYEES =
      *           THE SUM OF ALL INCREASED INSURANCE + TOTAL INCREASED INSURANCE
      *
      ***********************************************
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   LAPTOP-U5VKK9JE.
       OBJECT-COMPUTER.   LAPTOP-U5VKK9JE.
      
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-RECORDS
               ASSIGN TO 'PR2FA19-1.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-REPORT
               ASSIGN TO PRINTER 'EMPLOYEEFILE'.
       
       DATA DIVISION.
       FILE SECTION.
       
       FD  INPUT-RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       
       01  INFORMATION.
           05 I-WAREHOUSE-ID       PIC X(4).
           05 I-EMPLOYEE-ID        PIC X(5).
           05 I-EMPLOYEE-POSITION  PIC X(2).
           05 I-EMPLOYEE-LASTNAME  PIC X(10).
           05 I-EMPLOYEE-FIRSTNAME PIC X(10).
           05 FILLER               PIC X(3).
           05 I-HIRE-DATE          PIC 9(8).
           05 I-STARTING-SALARY    PIC 999999V99.
           05 FILLER               PIC X(4).
           05 I-LAST-PAY-INCREASE  PIC 9(8).
           05 I-CURRENT-SALARY     PIC 999999V99.
           05 FILLER               PIC X(5).
           05 I-UNION-DUES         PIC 9(3).
           05 I-INSURANCE          PIC 999V99.
       
       FD  OUTPUT-REPORT
           RECORD CONTAINS 70 CHARACTERS.
       
       01  RECORD-REPORT           PIC X(79).
       
       
       WORKING-STORAGE SECTION.
       
       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG            PIC X           VALUE ' '.
           05  FIRST-RECORD        PIC X(3)        VALUE 'YES'.
           05  WS-HOLDING          PIC X(11).
           
       01  WS-DATE.
           05  WS-YEAR             PIC 99.
           05  WS-MONTH            PIC 99.
           05  WS-DAY              PIC 99.
       
       01  TOTAL-FIELDS.
           05  TF-CURRENT-SALARY   PIC S9(7)V9(2)  VALUE +0.
           05  TF-INCREASED-CURR   PIC S9(7)V9(2)  VALUE +0.
           05  TF-TOTAL-CURR       PIC S9(7)V9(2)  VALUE +0.
           
           05  TF-UNION-DUES       PIC S9(6)V9(2)  VALUE +0.
           05  TF-INCREASED-UNION  PIC S9(6)V9(2)  VALUE +0.
           05  TF-TOTAL-UNION      PIC S9(6)V9(2)  VALUE +0.
           
           05  TF-INSURANCE        PIC S9(6)V9(2)  VALUE +0.
           05  TF-INCREASED-INSUR  PIC S9(6)V9(2)  VALUE +0.
           05  TF-TOTAL-INSUR      PIC S9(6)V9(2)  VALUE +0.
           
       01  REPORT-FIELDS.
           05  PROPER-SPACING      PIC S9          VALUE +1.
       
      ****************    OUTPUT AREA    ****************
       
       01  HEADING-ONE.
           05  H1-MONTH            PIC 99.
           05                      PIC X           VALUE '/'.
           05  H1-DAY              PIC 99.
           05                      PIC X           VALUE '/'.
           05  H1-YEAR             PIC 99.
           05                      PIC X(5).
           05                      PIC X(3)        VALUE 'CGB'.
           05                      PIC X(18)       VALUE SPACES.
           05                      PIC X(11)       VALUE 'DRAKEA, LTD'.
           05                      PIC X(24)       VALUE SPACES.
           05                      PIC X(8)        VALUE 'PAGE 01'.

       01  HEADING-TWO.
           05                      PIC X(35)       VALUE SPACES.
           05                      PIC X(7)        VALUE 'SALARY '.
           05                      PIC X(6)        VALUE 'REPORT'.
           05                      PIC X(31)       VALUE SPACES.

       01  WAREHOUSE-LINE.
           05                      PIC X(12)       VALUE 'WAREHOUSE:  '.
           05  WL-WAREHOUSE-ID     PIC X(11).

       01  HEADING-THREE.
           05                      PIC X(2).
           05                      PIC X(8)        VALUE 'EMPLOYEE'.
           05                      PIC X(4).
           05                      PIC X(8)        VALUE 'EMPLOYEE'.
           05                      PIC X(5).
           05                      PIC X(8)        VALUE 'EMPLOYEE'.
           05                      PIC X(4).
           05                      PIC X(9)        VALUE 'INCREASED'.
           05                      PIC X(5).
           05                      PIC X(9)        VALUE 'INCREASED'.
           05                      PIC X(4).
           05                      PIC X(9)        VALUE 'INCREASED'.
           05                      PIC X(4).

       01  HEADING-FOUR.
           05                      PIC X(5).
           05                      PIC X(2)        VALUE 'ID'.
           05                      PIC X(7).
           05                      PIC X(8)        VALUE 'POSITION'.
           05                      PIC X(4).
           05                      PIC X(9)        VALUE 'LAST NAME'.
           05                      PIC X(5).
           05                      PIC X(7)        VALUE 'CURRENT'.
           05                      PIC X(6).
           05                      PIC X(10)       VALUE 'UNION DUES'.
           05                      PIC X(3).
           05                      PIC X(9)        VALUE 'INSURANCE'.
           05                      PIC X(4).

       01  DETAIL-LINE.
           05                      PIC X(3)        VALUE SPACES.
           05  DL-EMPLOYEE-ID      PIC X(5).
           05                      PIC X(5)        VALUE SPACES.
           05  DL-EMPLOYEE-POS     PIC X(10).
           05                      PIC X(3)        VALUE SPACES.
           05  DL-EMPLOYEE-LAST    PIC X(10).
           05                      PIC X(3)        VALUE SPACES.
           05  DL-INCREASED-CURR   PIC $ZZZ,ZZZ.99.
           05                      PIC X(3)        VALUE SPACES.
           05  DL-INCREASED-UNION  PIC $ZZ,ZZZ.99.
           05                      PIC X(3)        VALUE SPACES.
           05  DL-INCREASED-INSUR  PIC $ZZ,ZZZ.99.
           05                      PIC X(3)        VALUE SPACES.

       01  TOTAL-LINE.
           05                      PIC X(7).
           05  TL-WAREHOUSE-ID     PIC X(11).
           05                      PIC X(19)        
                                         VALUE ' WAREHOUSE TOTAL:  '.
           05  TL-INCREASED-CURR   PIC $Z,ZZZ,ZZZ.99.
           05                      PIC X(4).
           05  TL-INCREASED-UNION  PIC $ZZ,ZZZ.99.
           05                      PIC X(3).
           05  TL-INCREASED-INSUR  PIC $ZZ,ZZZ.99.

      
       PROCEDURE DIVISION.
      *                                CGB
       10-CONTROL-MODULE.
       
           PERFORM 20-HOUSEKEEPING-ROUTINE
           PERFORM 30-READ-FILE
           PERFORM 700-EOF-ROUTINE
           
           .
           
      *** CANNOT CALL THE HEADER ROUTINE BEFORE WE PROCESS THE DATA ***
       20-HOUSEKEEPING-ROUTINE.
       
           OPEN INPUT INPUT-RECORDS
               OUTPUT OUTPUT-REPORT
           ACCEPT WS-DATE FROM DATE
               
               MOVE WS-MONTH TO H1-MONTH
               MOVE WS-DAY TO H1-DAY
               MOVE WS-YEAR TO H1-YEAR
           PERFORM 40-HEADER-ROUTINE
           
           .
       
       30-READ-FILE.
           PERFORM UNTIL EOF-FLAG = 'N'
               READ INPUT-RECORDS
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END
                       PERFORM 100-PROCESS-EMPLOYEE-RECORD
               END-READ
           END-PERFORM
           
           .
           
       40-HEADER-ROUTINE.
       
           WRITE RECORD-REPORT FROM HEADING-ONE
               AFTER ADVANCING PROPER-SPACING
               
           
           MOVE 2 TO PROPER-SPACING
           MOVE HEADING-TWO TO RECORD-REPORT
           WRITE RECORD-REPORT FROM HEADING-TWO
               AFTER ADVANCING PROPER-SPACING
               
           .
           
       100-PROCESS-EMPLOYEE-RECORD.
       
      * NESTED IF-STATEMENT FOR FINDING WAREHOUSE ID AND WRITING OUT WAREHOUSE
           IF I-WAREHOUSE-ID = 'AL10' THEN
               MOVE 'ALABAMA' TO WL-WAREHOUSE-ID,
                                 TL-WAREHOUSE-ID
           ELSE
               IF I-WAREHOUSE-ID = 'GA11' THEN
                   MOVE 'GEORGIA' TO WL-WAREHOUSE-ID,
                                     TL-WAREHOUSE-ID
               ELSE
                   IF I-WAREHOUSE-ID = 'MS12' THEN
                       MOVE 'MISSISSIPPI' TO WL-WAREHOUSE-ID, 
                                             TL-WAREHOUSE-ID
                   END-IF
               END-IF
           END-IF
      
      
           IF FIRST-RECORD = 'YES'
               MOVE WL-WAREHOUSE-ID TO WS-HOLDING
               MOVE 'NO' TO FIRST-RECORD
               PERFORM 400-PRINT-HEADER
           ELSE
               IF WL-WAREHOUSE-ID NOT = WS-HOLDING
                   PERFORM 300-BREAK
                   PERFORM 400-PRINT-HEADER
               END-IF
           END-IF
           
      * NESTED IF-STATEMENT FOR FINDING EMPLOYEE POSITION AND WRITING OUT JOB
           IF I-EMPLOYEE-POSITION = 'WM' THEN
               MOVE 'MANAGER' TO DL-EMPLOYEE-POS
           ELSE
               IF I-EMPLOYEE-POSITION = 'DS' THEN
                   MOVE 'SUPERVISOR' TO DL-EMPLOYEE-POS
               ELSE
                   IF I-EMPLOYEE-POSITION = 'OW' THEN
                       MOVE 'OFFICE' TO DL-EMPLOYEE-POS
                   ELSE
                       IF I-EMPLOYEE-POSITION = 'WW' THEN
                           MOVE 'WAREHOUSE' TO DL-EMPLOYEE-POS
                       ELSE
                           IF I-EMPLOYEE-POSITION = 'WS' THEN
                               MOVE 'SECURITY' TO DL-EMPLOYEE-POS
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
            
            
           MOVE I-EMPLOYEE-ID TO DL-EMPLOYEE-ID
           MOVE I-EMPLOYEE-LASTNAME TO DL-EMPLOYEE-LAST
           MOVE I-CURRENT-SALARY TO TF-CURRENT-SALARY
           MOVE I-UNION-DUES TO TF-UNION-DUES
           MOVE I-INSURANCE TO TF-INSURANCE
           
      * INCREASED CURRENT SALARY
           COMPUTE TF-INCREASED-CURR = TF-CURRENT-SALARY * 1.05
      * TOTAL CURRENT SALARY
           COMPUTE TF-TOTAL-CURR = TF-INCREASED-CURR
                   + TF-TOTAL-CURR
           MOVE TF-INCREASED-CURR TO DL-INCREASED-CURR
       
       
      * INCREASED UNION DUES
           COMPUTE TF-INCREASED-UNION = TF-UNION-DUES * 1.03
      * TOTAL UNION DUES
           COMPUTE TF-TOTAL-UNION = TF-INCREASED-UNION + TF-TOTAL-UNION
           MOVE TF-INCREASED-UNION TO DL-INCREASED-UNION
           
           
      * INCREASED INSURANCE
           COMPUTE TF-INCREASED-INSUR = TF-INSURANCE * 1.05
      * TOTAL INSURANCE
           COMPUTE TF-TOTAL-INSUR = TF-INCREASED-INSUR + TF-TOTAL-INSUR
           MOVE TF-INCREASED-INSUR TO DL-INCREASED-INSUR
           
           MOVE DETAIL-LINE TO RECORD-REPORT
           PERFORM 200-WRITE-A-LINE
           MOVE 1 TO PROPER-SPACING
           
           .
       
       200-WRITE-A-LINE.
           WRITE RECORD-REPORT
               AFTER ADVANCING PROPER-SPACING
               
           .
           
       300-BREAK.
       
      * MOVES WORKING CLASS TO TOTAL LINE
           MOVE WS-HOLDING TO TL-WAREHOUSE-ID
      
      * MOVES NEW CLASS INTO HOLDING FOR NEXT ROTATION
           MOVE WL-WAREHOUSE-ID TO WS-HOLDING
           
      * MOVES THE WORKING CLASS TOTALS TO OUTPUT ON TOTAL LINE
           MOVE TF-TOTAL-CURR TO TL-INCREASED-CURR
           MOVE TF-TOTAL-UNION TO TL-INCREASED-UNION
           MOVE TF-TOTAL-INSUR TO TL-INCREASED-INSUR
           
           MOVE 3 TO PROPER-SPACING
           MOVE TOTAL-LINE TO RECORD-REPORT
           
      * THEN, RESETS TOTALS FOR THE NEXT WAREHOUSE
           MOVE ZEROS TO TF-TOTAL-CURR
           MOVE ZEROS TO TF-TOTAL-UNION
           MOVE ZEROS TO TF-TOTAL-INSUR
           
           PERFORM 200-WRITE-A-LINE
           
           .
           
       400-PRINT-HEADER.
           
           MOVE WL-WAREHOUSE-ID TO WL-WAREHOUSE-ID
           MOVE 2 TO PROPER-SPACING
           
           WRITE RECORD-REPORT FROM WAREHOUSE-LINE
               AFTER ADVANCING 3 LINES

           WRITE RECORD-REPORT FROM HEADING-THREE
               AFTER ADVANCING 3 LINES
           
           WRITE RECORD-REPORT FROM HEADING-FOUR
               AFTER ADVANCING 1 LINE
               
           .
           
       500-EOJ-ROUTINE.
       
           PERFORM 300-BREAK
           
           .
           
       700-EOF-ROUTINE.    
       
           PERFORM 500-EOJ-ROUTINE
           
           CLOSE INPUT-RECORDS
               OUTPUT-REPORT
           STOP RUN
           
           .
