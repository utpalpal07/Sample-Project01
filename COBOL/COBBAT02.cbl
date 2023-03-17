      *-----------------------                                          
       IDENTIFICATION DIVISION.                                         
      *-----------------------                                          
       PROGRAM-ID.  COBBAT02.                                           
      *--------------------                                             
       ENVIRONMENT DIVISION.                                            
      *--------------------                                             
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
           SELECT INP-INPUT  ASSIGN TO INDD.                            
           SELECT OUT-OUTPUT ASSIGN TO OUTDD.                           
      *-------------                                                    
       DATA DIVISION.                                                   
      *-------------                                                    
       FILE SECTION.                                                    
       FD  INP-INPUT  RECORDING MODE F.                                 
       01  INPUT-REC.                                                   
           05  INP-NAME       PIC X(10).                                
           05  FILLER         PIC X(70).                                
      *                                                                 
       FD  OUT-OUTPUT RECORDING MODE F.                                 
       01  OUTPUT-REC.                                                  
           05  OUT-NAME       PIC X(10).                                
           05  FILLER         PIC X(01) VALUE '-'.                      
           05  CURR-DATE      PIC X(10).                                
           05  FILLER         PIC X(59) VALUE SPACES.                   
      *                                                                 
       WORKING-STORAGE SECTION.                                         
       01 FLAGS.                                                        
         05 LASTREC           PIC X VALUE SPACE.                        
         05 WS-CURR-DATE      PIC X(10) VALUE SPACES.                   
      *                                                                 
           EXEC SQL  INCLUDE SQLCA   END-EXEC.                          
      *------------------                                               
       PROCEDURE DIVISION.                                              
      *------------------                                               
      *                                                                 
           DISPLAY 'PROGRAM COBBAT01 - START'.                          
      *                                                                 
       OPEN-FILES.                                                      
           OPEN INPUT  INP-INPUT.                                       
           OPEN OUTPUT OUT-OUTPUT.                                      
      *                                                                 
       READ-WRITE-UNTIL-LASTREC.                                        
           PERFORM READ-INP-INPUT                                       
           PERFORM UNTIL LASTREC = 'Y'                                  
              PERFORM RETRIEVE-DATE                                     
              PERFORM WRITE-COMBINED                                    
              PERFORM READ-INP-INPUT                                    
           END-PERFORM.                                                 
      *                                                                 
       CLOSE-STOP.                                                      
      *                                                                 
           DISPLAY 'PROGRAM COBBAT01 - STOP'.                           
      *                                                                 
           CLOSE INP-INPUT.                                             
           CLOSE OUT-OUTPUT.                                            
           GOBACK.                                                      
      *                                                                 
       READ-INP-INPUT.                                                  
           INITIALIZE INPUT-REC.                                        
      *                                                                 
           READ INP-INPUT                                               
           AT END MOVE 'Y' TO LASTREC                                   
           END-READ.                                                    
      *                                                                 
       RETRIEVE-DATE.                                                   
           EXEC SQL                                                     
              SELECT  CURRENT DATE                                      
                INTO :WS-CURR-DATE                                      
                FROM  SYSIBM.SYSDUMMY                                   
           END-EXEC.                                                    
      *                                                                 
           DISPLAY 'SQLCODE CURRENT DATE: ' SQLCODE                     
      *                                                                 
           EVALUATE SQLCODE                                             
              WHEN 0                                                    
                 MOVE WS-CURR-DATE  TO CURR-DATE                        
              WHEN OTHER                                                
                 DISPLAY 'ERROR CURRENT DATE'                           
           END-EVALUATE.                                                
      *                                                                 
       WRITE-COMBINED.                                                  
           MOVE SPACES       TO  OUTPUT-REC                             
           MOVE INP-NAME     TO  OUT-NAME                               
      *    MOVE '14-11-2022' TO  CURR-DATE                              
           WRITE OUTPUT-REC.                                            
      *                                                                         
