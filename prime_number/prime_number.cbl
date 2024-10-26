      ******************************************************************
      * Author: Rayane TOKO
      * Date: October 26, 2024
      * Purpose: Check if an input number is prime or not.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PrimeNumber.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  user_input  PIC S9(3) VALUE ZERO.
       01  divider     PIC 9(3) VALUE ZERO.
       01  divider_max PIC 9(3) VALUE ZERO.
       01  result      PIC 9(3) VALUE ZERO.
       01  rest        PIC 9(3) VALUE ZERO.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM GET_INPUT THRU DISPLAY_PROCESS_RESULT.
           STOP RUN.

       GET_INPUT.
           DISPLAY 'Please enter a number: min = 0 and max = 999'
           ACCEPT user_input.

       MAIN_PROCESS.
           MOVE 2 TO divider.
           MOVE 1 TO rest.
           COMPUTE divider_max ROUNDED = user_input / 2.
           PERFORM WITH TEST AFTER UNTIL divider >= divider_max OR
             rest = 0
               DIVIDE user_input BY divider GIVING result REMAINDER rest
               ADD 1 TO divider
           END-PERFORM.

       DISPLAY_PROCESS_RESULT.
           IF user_input NOT = 2 AND (rest = 0 OR user_input <= 1)
               DISPLAY 'It''s not a prime number'
           ELSE
               DISPLAY 'It''s a prime number'
           END-IF.
       END PROGRAM PrimeNumber.
