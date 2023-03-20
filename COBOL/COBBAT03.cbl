      *-----------------------                                          
       IDENTIFICATION DIVISION.                                         
      *-----------------------                                          
       PROGRAM-ID.  COBBAT03.                                           
      *--------------------                                             
       ENVIRONMENT DIVISION.                                            
      *--------------------                                             
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
           SELECT INP-INPUT1  ASSIGN TO INDD1
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS FS-INPUT1.
      *                          
           SELECT INP-INPUT2  ASSIGN TO INDD2
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS FS-INPUT2.
      *                                                    
           SELECT OUT-OUTPUT  ASSIGN TO OUTDD
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS FS-OUTPUT.                          
      *-------------                                                    
       DATA DIVISION.                                                   
      *-------------                                                    
       FILE SECTION.                                                    
       FD  INP-INPUT1  RECORDING MODE F.                                
       01  INPUT-REC1         PIC X(100).                               
      *                                                                 
       FD  INP-INPUT2  RECORDING MODE F.                                
       01  INPUT-REC2         PIC X(100).                               
      *                                                                 
       FD  OUT-OUTPUT RECORDING MODE F.                                 
       01  OUTPUT-REC         PIC X(100).                               
      *                                                                 
       WORKING-STORAGE SECTION.                                         
       01 FLAGS.                                                        
         05 KEY-VALUE1           PIC X(10) VALUE SPACE.
         05 KEY-VALUE2           PIC X(10) VALUE SPACE.                        
      *
       01 FS-FILE-STATUS.
          05 FS-INPUT1.
             10 FS-INPUT1-OK     PIC X(02) VALUE '00'.
             10 FS-INPUT1-EOF    PIC X(02) VALUE '10'.    
          05 FS-INPUT2.
             10 FS-INPUT2-OK     PIC X(02) VALUE '00'.
             10 FS-INPUT2-EOF    PIC X(02) VALUE '10'.  
          05 FS-OUTPUT.
             10 FS-OUTPUT-OK     PIC X(02) VALUE '00'.
             10 FS-OUTPUT-EOF    PIC X(02) VALUE '10'. 
      *
       01 WS-FS-ERROR.
          05 WS-ERROR            PIC X(30) VALUE SPACE.
          05 WS-FILE-STATUS      PIC X(02) VALUE SPACE.                            
      *                       
      *------------------                                               
      *-C O P Y B O O K S -                                             
      *------------------  
        COPY COBCPY01.
        COPY COBCPY02.
        COPY COBCPY03.                                             
      *------------------                                               
       PROCEDURE DIVISION.                                              
      *------------------                                               
      *                                                                 
           DISPLAY 'PROGRAM COBBAT03 - START'.
      *
           PERFORM OPEN-FILES
              THRU OPEN-FILES-EX.
      *
           PERFORM READ-INP-INPUT1
              THRU READ-INP-INPUT1-EX.     
      *
           PERFORM READ-INP-INPUT2
              THRU READ-INP-INPUT2-EX.
      *
           PERFORM PARA-PROCESS
              THRU PARA-PROCESS-EX.
      *
           PERFORM CLOSE-STOP
              THRU CLOSE-STOP-EX. 
      *
           GOBACK.                           
      *                                                                 
       OPEN-FILES.                                                      
           OPEN INPUT  INP-INPUT1.                                      
           OPEN INPUT  INP-INPUT2.                                      
           OPEN OUTPUT OUT-OUTPUT. 
      *
       OPEN-FILES-EX.
           EXIT.                                     
      *                                                                 
       PARA-PROCESS.                                        
      *
           EVALUATE TRUE
               WHEN KEY-VALUE1 > KEY-VALUE2
                  PERFORM READ-INP-INPUT2
                     THRU READ-INP-INPUT2-EX
               WHEN KEY-VALUE1 < KEY-VALUE2
                  PERFORM READ-INP-INPUT1
                     THRU READ-INP-INPUT1-EX
               WHEN KEY-VALUE1 = KEY-VALUE2
                  PERFORM WRITE-OUTPUT
                     THRU WRITE-OUTPUT-EX
      *
                  PERFORM READ-INP-INPUT1
                     THRU READ-INP-INPUT1-EX
      *
                  PERFORM READ-INP-INPUT1
                     THRU READ-INP-INPUT1-EX
           END-EVALUATE.
      *
       PARA-PROCESS-EX.
           EXIT.
      *                                                                 
       CLOSE-STOP.                                                      
      *                                                                 
           DISPLAY 'PROGRAM COBBAT03 - STOP'.                           
      *                                                                 
           CLOSE INP-INPUT1. 
           CLOSE INP-INPUT2.                                            
           CLOSE OUT-OUTPUT.                                            
      *
       CLOSE-STOP-EX.
           EXIT.
      *                                                                 
       READ-INP-INPUT1.                                                  
           INITIALIZE INPUT-REC1
                      KEY-VALUE1                                      
      *                                                                 
           READ INP-INPUT1      INTO COBCPY01.                          
      *
           EVALUATE TRUE
               WHEN FS-INPUT1 = '00'
                  MOVE EMP-NUM             TO KEY-VALUE1
               WHEN FS-INPUT1 = '10'
                  MOVE '9999999999'        TO KEY-VALUE1
               WHEN OTHER
                  MOVE 'ERROR IN INPUT FILE1'
                                           TO WS-ERROR
                  MOVE FS-INPUT1           TO WS-FILE-STATUS
           END-EVALUATE.
      *
       READ-INP-INPUT1-EX.
           EXIT.
      *
       READ-INP-INPUT2.                                                  
           INITIALIZE INPUT-REC2
                      KEY-VALUE2                                      
      *                                                                 
           READ INP-INPUT2      INTO COBCPY02.                          
      *
           EVALUATE TRUE
               WHEN FS-INPUT2-OK
                  MOVE EMP-NUMB            TO KEY-VALUE2
               WHEN FS-INPUT2-EOF
                  MOVE '9999999999'        TO KEY-VALUE2
               WHEN OTHER
                  MOVE 'ERROR IN INPUT FILE2'
                                           TO WS-ERROR
                  MOVE FS-INPUT2           TO WS-FILE-STATUS
           END-EVALUATE.
      *
       READ-INP-INPUT2-EX.
           EXIT.
      *                                                                 
       WRITE-OUTPUT.                                                  
           MOVE SPACES       TO  OUTPUT-REC                             
           MOVE EMP-NUM      TO  OUT-NUM
           MOVE EMP-FIRST-NM TO  OUT-FIRST-NM
           MOVE EMP-LAST-NM  TO  OUT-LAST-NM
           MOVE EMP-PHONE    TO  OUT-PHONE
           MOVE EMP-ADDRESS  TO  OUT-ADDRESS
      *                             
           WRITE OUTPUT-REC.
      *
           IF FS-OUTPUT-OK
              CONTINUE
           ELSE
              MOVE 'ERROR IN WRITING OUTPUT'
                                           TO WS-ERROR
              MOVE FS-OUTPUT               TO WS-FILE-STATUS
           END-IF.
      *
       WRITE-OUTPUT-EX.
           EXIT.                                          
      *                                                                 
