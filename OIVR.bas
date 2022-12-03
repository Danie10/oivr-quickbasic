DECLARE  SUB  CheckSetup  (TEXT,  BACK) 
DECLARE  FUNCTION  FileExist%  (Spec$) 
' **PROGRAM  WRITTEN  IN  QUICK  BASIC  V4.5  BY  DF  V/D  MERWE**
' **WRITTEN  V1.0  ON  1991-06-01**
' **UPDATE  V2.0  ON  1991-09-09**
' **UPDATE  V2.1  ON  1991-10-22**
' **UPDATE  V2.2  ON  1992-07-04** save  and  retrieve  data
' **UPDATE  V2.3  ON  1992-07-05**  better  modularization,  interrupts  for  monitor  detection 
'                                   interrupts  used  for  file  checking 
DEFINT  M-P
REM  DECLARE  SUB  INTERRUPT  (IntNum,  InRegs  AS  ANY,  OutRegs  AS  ANY) 
DECLARE FUNCTION  Monitor%  (Segment%)
DECLARE SUB  TitleBW  () 
DECLARE SUB  TitleVGA  () 
DECLARE SUB  TitleCGA  () 
DECLARE SUB  Comline  (N,  a$(),  Max) 
DECLARE SUB  Alarm  () 
DECLARE SUB  GETCOLOR  (TEXT,  BACK) 
ON ERROR GOTO ErrorHandler

INITIALISE:

    '  Default  variable  type  is  integer  in  this  module. 
    '  Declare  the  Comline  subprogram,  as  well  as  the  number  and 
    '  type  of  its  parameters.
    TYPE RecordType 
        TEXT AS INTEGER 
        BACK AS INTEGER 
        MON AS STRING * 3 
    END TYPE
    DIM RecordVar AS RecordType

    '$INCLUDE: 'QB.BI'
    DIM SHARED Registers AS RegTypeX 
    TYPE DTA                       'used  by  DOS  services 
        Reserved AS STRING * 21    'reserved  for  use  by  DOS 
        Attribute AS STRING * 1    'the file's attribute 
        FileTime AS STRING * 2     'the file's time 
        FileDate AS STRING * 2     'the file's date 
        FileSize AS LONG           'the file's size 
        FileName AS STRING * 13    'the file's name 
    END TYPE
    DIM DTAData AS DTA
    DEF FnFileExist% (Spec$)
        FnFileExist% = -1                   'assume the file exists 
        Registers.dx =  VARPTR(DTAData)     'set a new DOS DTA 
        Registers.ds =  VARSEG(DTAData)
        Registers.ax =  &H1A00
        CALL INTERRUPTX(&H21, Registers, Registers)
        Spec$ = Spec$ + CHR$(0)             'DOS needs ASCIIZ string
        Registers.ax = &H4E00               'find file name service
        Registers.cx = 39                   'attribute for any file
        Registers.dx = SADD(Spec$)          'show where the spec is 
        'Registers.ds = SSEG(Spec$)          'use this with PDS
        Registers.ds = VARSEG(Spec$)        'use this with QB
        CALL INTERRUPTX(&H21, Registers, Registers)
        IF Registers.flags AND 1 THEN FnFileExist% = 0
    END DEF

    DIM a$(1 TO 15)
    DIM C1(4), C2(4) 
    DIM X(200), Y(200), X1(200), Y1(200)
    DIM EQN1(200), EQN2(200), EQN3(200), EQN4(200) 
    DIM a(4), B(4), S1(4), S2(4), S3(4), S4(4)

' Get what was typed on the command line. 
CALL Comline(N, a$(), 10)
' Print  out  each  part  of  the  command  line. 
IF a$(1) = "/?" THEN GOTO DOSHELP
IF UCASE$(a$(1)) = "/SETUP" OR UCASE$(a$(2)) = "/SETUP" THEN
        IF FnFileExist%("USERS.DAT") THEN
            KILL "USERS.DAT"
        END IF
END IF 
' Subroutine to get command line and split into arguments. 
' Parameters: NumArgs : Number of command line args found. 
'             Args$() : Array in which to return arguments. 
'             MaxArgs : Maximum number of arguments array
'                       can  return.

CLS 
LOCATE 13, 21: PRINT "Type OIVR /? at DOS line for help"
SLEEP 3
CLS
CALL CheckSetup(TEXT, BACK)

START:
    PRINT "THE PRINTER MUST BE SWITCHED ON"
    PRINT " "
    PRINT "DOES YOUR TASK HAVE A NAME? (Y/N)."
    PRINT "IF 'Y' ENTER THE NAME OTHERWISE ENTER 'N'"
    PRINT " "
    INPUT "TASK NAME: ", a$
    ' UCASE$ converts lowercase letter to uppercase
    IF UCASE$(a$) <> "N" AND a$ <> "" THEN LPRINT "TASK NAME:  "; a$
    LPRINT 
    LPRINT 
    PRINT "THIS PROGRAM CALCULATES A TABLE OF ALTERNATIVE REGRESSION"
    LPRINT "THIS PROGRAM CALCULATES A TABLE OF ALTERNATIVE REGRESSION"
    PRINT "COEFFICIENTS FOR LINEAR, EXPONENTIAL, LOGARITHMIC AND POWER"
    LPRINT "COEFFICIENTS FOR LINEAR, EXPONENTIAL, LOGARITHMIC AND POWER"
    PRINT "FUNCTION LEAST SQUARE REGRESSION LINES TO A GIVEN SET OF"
    LPRINT "FUNCTION LEAST SQUARE REGRESSION LINES TO A GIVEN SET OF"
    PRINT "N PAIRS OF (X(I),Y(I)) VALUES, X IS CONSIDERED THE INDEPENDANT"
    LPRINT "N PAIRS OF (X(I),Y(I)) VALUES, X IS CONSIDERED THE INDEPENDANT"
    PRINT "VARIABLE"
    LPRINT "VARIABLE"
    LPRINT
    PRINT " "
    PRINT "IF A MISTAKE IS MADE WHILST ENTERING DATA NOTE THE PAIR"
    PRINT "NO AND AN OPPORTUNITY WILL BE GIVEN BEFORE THE CALCULATIONS"
    PRINT "ARE PERFORMED TO CORRECT DATA FOR THOSE PAIRS."
    PRINT " "

LoadFile:
' Check  for  input  from  a  file 
INPUT "Do you want to load Y(I) values from a saved file"; Y$
IF UCASE$(Y$) = "Y" THEN
    INPUT "Enter filename to retrieve ", file$
    IF FnFileExist%(file$) THEN
        OPEN file$ FOR INPUT AS #1
    ELSE
        INPUT "No file found - Correct name or Shell to DOS (C/S)", Y$
        IF UCASE$(Y$) = "S" THEN
            CLS
            PRINT "Type EXIT to return to OIVR"
            SHELL
            CLS
            PRINT "Continuing with importing data to OIVR"
        END IF
        GOTO LoadFile
    END IF
    I = 0
    DO WHILE NOT EOF(1)
        I = I + 1
        ' Get X1 value
        X1(I) = I
        ' Get Y1 value from file
        INPUT #1, Y1(I)
    LOOP
    CLOSE #1
    ' Set no of pairs to last value of I
    N = I
    LPRINT
    LPRINT "NUMBER OF PAIRS OF (X(I),Y(I)) N = "; N
    CLS

ELSEIF UCASE$(Y$) = "N" THEN
    INPUT "NUMBER OF PAIRS OF (X(I),Y(I)) TO BE ENTERED, N=", N
    LPRINT
    LPRINT "NUMBER OF PAIRS OF (X(I),Y(I)) N= "; N
    CLS
    PRINT "NUMBER OF PAIRS TO BE ENTERED = "; N
    PRINT " "
    FOR I = 1 TO N STEP 1
    PRINT "ENTER PAIR NO. "; I; " OF (X(I),Y(I)";

    ' Next line gets the actual pairs
    INPUT X1(I), Y1(1)
    NEXT I
ELSE
    GOTO LoadFile

END IF
CORRECTMISTAKE:
    PRINT "WERE ANY ERRORS MADE WHILST ENTERING DATA (Y or N)": Z$ = INPUT$(1)
    IF UCASE$(Z$) = "N" THEN GOTO SAVEOPTION

GETPAIR:
    INPUT "ENTER PAIR NO TO EDIT"; PAIR
    IF PAIR > N THEN GOTO GETPAIR 'limit to existing pairs
    PRINT "PAIR "; PAIR; " ENTERED AS "; X1(PAIR); ","; Y1(PAIR)
    INPUT "INPUT NEW DATA FOR PAIR"; X1(PAIR), Y1(PAIR)
    PRINT "ARE THERE MORE PAIRS TO CORRECT": Y$ = INPUT$(1)
    IF UCASE$(Y$) = "Y" THEN GOTO GETPAIR

SAVEOPTION:
' Option to save the data entered
    INPUT "Do you want to save the Y1 data"; Y$
    IF UCASE$(Y$) = "N" THEN GOTO CALCULATE
    INPUT "Enter filename ", file$
    OPEN file$ FOR OUTPUT AS #1
    FOR I = 1 TO N
        WRITE #1, Y1(I)
        NEXT I
    CLOSE #1

CALCULATE:
' - Input data is now in arrays X1(1) and Y1(1)
    LPRINT
    L2 = 1
    L3 =1
    L4 = 1

' J BRANCHES FROM EQUATION 1 TO 4 DEPENDING ON VALUE OF J
    FOR J = 1 TO 4 STEP 1
    ON J GOTO 270, 320, 410, 500

' EQUATION 1 STARTS WITH X1,Y1 PLACED IN X,Y FOR CALCULATIONS LINEAR
270 FOR I = 1 TO N STEP 1
    X(I) = X1(I)
    Y(1) = Y1(1)
    NEXT I
    ' 590 BRANCHES TO CALCULATION ITSELF
    GOTO 590

' EQUATION 2 STARTS WITH X1 BUT Y IS LOG OF Y1
320 FOR I = 1 TO N STEP 1
'  X(1)=X1(1)
' CHECKS IF Y1 <=0 BEFORE LOG FUNCTION
    IF Y1(I) <= 0 GOTO 380 ELSE 350
350 Y(1) = LOG(Y1(I))
    NEXT I
' ARRAY NOW HAS PLAIN X1 BUT LOG(Y1)
    GOTO 400
380 L2 = 2
    GOTO 850
400 GOTO 590

' EQUATION 3 STARTS WITH Y1 BUT X IS LOG OF X1
410 FOR I = 1 TO N STEP 1
' CHECKS IF X1 IS <=0 BEFORE LOG FUNCTION
    IF X1(1) < 0 OR X1(I) = 0 GOTO 470 ELSE 430
430 X(I) = LOG(X1(I))
' Y IS RESET TO ORIGINAL Y1 VALUE
    Y(I) = Y1(I)
    NEXT  I 
' ARRAY NOW HAS PLAIN Y1 BUT LOG(X1)
    GOTO  490
470 L3 = 2
    GOTO 850
490 GOTO 590

' EQUATION 4 STARTS WITH LOG(X1) AND LOG(Y1)
500 FOR I = 1 TO N STEP 1
' CHECKS IF X1 OR Y1 <=0
    IF X1(I) < 0 OR X1(I) = 0 OR Y1(I) < 0 OR Y1(I) = 0 GOTO 560 ELSE 530
' X IS ALREADY LOG(X1) NOW Y BECOMES LOG(Y1)
530 Y(I) = LOG(Y1(I))
    NEXT I
    GOTO 580
560 L4 = 2
    GOTO 850
580 GOTO 590

' ACTUAL CALCULATION STARTS HERE ON ARRAY USING RELEVANT EQUATION
590 GOTO 600
600 Z5 = 0
    Z2 = 0
    Z6 = 0
    Z7 = 0
    Z9 = 0
' 5 CONSTANTS WORKED OUT ON VARIOUS TYPES OF CALCULATION BASED ON PAIRS
    FOR I = 1 TO N STEP 1
    Z5 = Z5 + X(I) * Y(I)
    Z6 = Z6 + X(I) / N
    Z7 = Z7 + Y(I) / N
    Z9 = Z9 + (X(I) ^ 2)
    Z2 = Z2 + (Y(I) ^ 2)
    NEXT I
' STRAIGHT THROUGH CALCS TO LINE 860 THEN BACK TO LINE 260 TO START ON
' NEXT EQUATION
    Z3 = Z6 * N
    Z4 = (Z3 ^ 2) / N
    Z8 = (Z6 * N) * (Z7 * N)
    Z8 = Z8 / N
    B(J) = ((Z5 - Z8) / (Z9 - Z4))
    a(J) = (Z7 - B(J) * Z6)
    S3(J) = B(J) / (Z2 - (((Z7 * N)^ 2) / N)) * (Z5 - Z8)
    C1(J) = (1 / (N - 1)) * (Z5 - Z8)
    C2(J) = C1(J) / ((Z2 - (((Z7 * N) ^ 2) / N)) / (N - 1)) ^ .5
    C2(J) = C2(J) / ((Z9 - (((Z6 * N) ^ 2) / N)) / (N - 1)) ^ .5
    S4(J) = ((Z2 - a(J) * Z7 * N - B(J) * Z5) / (N - 2)) ^ .5
    S2(J) = S4(J) / ((Z9 - ((Z6 * N) ^ 2) / N) ^ .5)
    S1(J) = ((Z9 / (N * (Z9 - (((Z6 * N) ^ 2) / N)))) ^ .5) * S4(J)
850 GOTO 860
' NOW BACK TO 260 TO BRANCH TO NEXT EQUATION
860 NEXT J
' 4 EQUATIONS DONE
    a(2) = EXP(a(2))
    a(4) = ECP(a(4))
' ALL MAIN CALCULATIONS DONE

PRINTOUT:
    PRINTLPRINT
    LPRINT "A             = REGRESSION COEFFICIENT"
    LPRINT "B             = REGRESSION COEFFICIENT"
    LPRINT "STA           = STANDARD ERROR OF REGRESSION COEFFICIENT A"
    LPRINT "STB           = STANDARD ERROR OF REGRESSION COEFFICIENT B"
    LPRINT "SEYONX        = STANDARD ERROR OF ESTIMATE OF Y ON X"
    LPRINT "COEFFDET      = COEFFICIENT OF DETERMINATION"
    LPRINT "COV           = COVARIANCE"
    LPRINT "CORRCOEFF     = CORRELATION COEFFICIENT"
    LPRINT

    LPRINT "Y = A+B*X     A = "; a(1); TAB(50); "B = "; TAB(54); B(1)
    LPRINT "              STA = "; S1(1); TAB(50); "STB = "; TAB(56); S2(1)
    LPRINT "              SEYONX = "; S4(1); TAB(50); "COEFFDET = "; TAB(61); S3(1)
    LPRINT "              COV = "; C1(1); TAB(50); "CORRCOEFF = "; TAB(62); C2(1)
    LPRINT ""

' CHECKS L2 FROM LINE 380 WHERE Y1<=0 IF SO THEN SKIPS THIS RESULT
    ON L2 GOTO 1040, 1080
1040 LPRINT "Y = A*EXP(B*X) A = "; a(2); TAB(50); "B = "; TAB(54); B(2)
    LPRINT "               STA = "; S1(2); TAB(50); "STB = "; TAB(56); S2(2)
    LPRINT "               SYONX = "; S4(2); TAB(50); "COEFFDET = "; TAB(61); S3(2)
    LPRINT "               COV = "; C1(2); TAB(50); "CORRCOEFF = "; TAB(62); C2(2)
    LPRINT

1080 ON L3 GOTO 1090, 1130
1090 LPRINT "Y = A+B*LOG(X) A = "; a(3); TAB(50); "B = "; TAB(54); B(3)
    LPRINT "               STA = "; S1(3); TAB(50); "STB = "; TAB(56); S2(3)
    LPRINT "               SEYONX = "; S4(3); TAB(50); "COEFFDET = "; TAB(61); S3(3)
    LPRINT "               COV = "; C1(3); TAB(50); "CORRCOEFF = "; TAB(62); C2(3)
    LPRINT

1130 ON L4 GOTO 1140, 1180
1140 LPRINT "Y = A*X^B      A="; a(4); TAB(50); "B = "; TAB(54); B(4)
    LPRINT "               STA = "; S1(4); TAB(50); "STB = "; TAB(56); S2(4)
    LPRINT "               SEYONX = "; S4(4); TAB(50); "COEFFDET = "; TAB(61); S3(4)
    LPRINT "               COV = "; C1(4); TAB(5O); "CORRCOEFF = "; TAB(62); C2(4)
    LPRINT
    LPRINT

1180 LPRINT "X(I)     Y(I)     Y=A+BX      Y=A*EXP(B*X)     Y=A+B*LOG(X)    Y=A*X^B"
    LPRINT

    FOR I = 1 TO N STEP 1
    Y6 = a(1) + B(1) * X1(I) 
    Y7 = a(2) * EXP(B(2) * X1(I))
    IF L3 = 2 THEN GOTO 1250 ELSE Y8 = a(3) + B(3) * LOG(X1(I))
    IF L4 = 2 THEN GOTO 1250 ELSE Y9 = EXP(LOG(a(4)) + B(4) * LOG(X1(I)))
1250 LPRINT X1(I); TAB(10); Y1(I); TAB(20); Y6; TAB(34); Y7; TAB(48); Y8; TAB(65); Y9
    NEXT I
    LPRINT
    LPRINT "EXPONENTIAL, LOGARITHMIC OR POWER REGRESSIONS WILL NOT BE CALCULATED"
    LPRINT "IF NEGATIVE OR ZERO VALUES APPEAR ON THE DATA, LINEAR REGRESSION WILL"
    LPRINT "OF COURSE ALWAYS BE PERFORMED REGARDLESS OF NEGATIVE VALUES"
    LPRINT "CALCULATIONS COMPLETED"
    LPRINT CHR$(12)
    PRINT "CALCULATIONS COMPLETED"

PROGRESSION: 
    CLS
    PRINT "DO YOU WANT TO PROJECT FOR FURTHER PAIRS (TIMED SERIES ONLY) ": Z$ = INPUT$(1)
    IF UCASE$(Z$) <> "Y" AND UCASE$(Z$) <> "N" THEN GOTO PROGRESSION
    IF UCASE$(Z$) = "N" THEN GOTO MOREVALUES
    INPUT "HOW MANY FURTHER PAIRS DO YOU WANT TO PROJECT "; NNEW

GETEQUATION: 
    PRINT " "
    LPRINT TAB(30); "PROGRESSIONS FOR Y"
    LPRINT " "
    LPRINT "ORIG. DATA PAIR"; TAB(21); "EQN  1"; TAB(37); "EQN 2"; TAB(S1); "EQN 3"; TAB(66); "EQN  4"
    LPRINT "X1(I)"; TAB(10); "Y1(I)"; TAB(20); "Y=A+B*X"; TAB(34); "Y=A*EXP(B*X)"; TAB(48); "Y=A+B*LOG(X)"; TAB(65); "Y=A*X^B"
    LPRINT " "
    FOR I = 1 TO (N + NNEW) STEP 1
        X1(I) = I
        EQN1(I) = a(1) + B(1) * X1(I)
        EQN2(I) = a(2) * EXP(B(2) * X1(I))
        EQN3(I) = a(3) + B(3) * LOG(X1(I))
        EQN4(I) = a(4) * X1(I) * B(4)
        LPRINT X1(I); TAB(10); Y1(I); TAB(20); EQN1(I); TAB(35); EQN2(I); TAB(48); EQN3(T); TAB(65); EQN4(1)
    NEXT I
    N = N + NNEW
    LPRINT CHR$(12)
    PRINT" "

MOREVALUES:
    PRINT "Do you want to export data to an ASCII file?": Y$ = INPUT$(1)
    IF UCASE$(Y$) <> "Y" AND UCASE$(Y$) <> "N" THEN GOTO MOREVALUES
    IF UCASE$(Y$) = "Y" THEN GOSUB EXPORT
    PRINT "DO YOU WANT TO ENTER MORE VALUES (Y/N)": Y$ = INPUT$(1)
    IF UCASE$(Y$) = "Y" THEN GOTO START
END

EXPORT: 
    INPUT "Please supply a name for the output file "; F$
    PRINT  "Do you want to export all equations (after progression only)? "; : S$ = INPUT$(1)
    IF UCASE$(S$) = "Y" THEN GOTO ALLSERIES: IF UCASE$(S$) = "N" GOTO ONESERIES ELSE GOTO EXPORT

ONESERIES:
    PRINT "Enter series no. for Harvard Import (1-5)"; : S$ = INPUT$(1)
    IF S$ <> "1" AND S$ <> "2" AND S$ <> "3" AND S$ <> "4" AND S$ <> "5" THEN PRINT "Must be 1-5!!!"
    PRINT "1 - A+B*X   2 - A*EXP(B*X)   3 - A+B*LOG(X)   4 - A*X^B"
    INPUT "Using equation number (1-4)? "; E
    IF E < 1 OR E > 4 THEN PRINT "1-4 ONLY!!!": GOTO ONESERIES
    OPEN F$ FOR OUTPUT AS #2
    IF E = 1 THEN FOR I = 1 TO N: Y1(I) = EQNI(I): NEXT  I
    IF E = 2 THEN FOR I = 1 TO N: Y1(I) = EQN2(I): NEXT  I
    IF E = 3 THEN FOR I = 1 TO N: Y1(I) = EQN3(I): NEXT  I
    IF E = 4 THEN FOR I = 1 TO N: Y1(I) = EQN4(I): NEXT  I
    FOR I = 1 TO N
    IF S$ = "1" THEN WRITE #2, X1(I), Y1(I) 
    IF S$ = "2" THEN WRITE #2, X1(I), "", Y1(I)
    IF S$ = "3" THEN WRITE #2, X1(I), "", "", Y1(I) 
    IF S$ = "4" THEN WRITE #2, X1(I), "", "", "", Y1(I)
    IF S$ = "5" THEN WRITE #2, X1(I), "", "", "", "", Y1(I)
    NEXT I
    CLOSE #2
    PRINT " "
    PRINT "Data was successfully exported to "; UCASE$(F$)
    PRINT " "
    RETURN

ALLSERIES:
    OPEN F$ FOR OUTPUT AS #2
    FOR I = 1 TO N
    WRITE #2, X1(I), Y1(I), EQN1(I), EQN2(I), EQN3(I), EQN4(I)
    NEXT I
    CLOSE #2
    PRINT " "
    PRINT "Data was successfully exported to "; UCASE$(F$)
    PRINT " "
    RETURN

DOSHELP:
    CLS
    PRINT "Syntax:    OIVR[/SETUP] [/?]"
    PRINT " "
    PRINT "This program accepts pairs of data and the closest fitting equation"
    PRINT "can be determined for the independant variable Y. It will also calculate"
    PRINT "progressions based on this curve and export this data to an ASCII file."
    PRINT "An option is given to export either a single equation's data or all"
    PRINT "equations. This data can then be imported into spreadsheets or Harvard"
    PRINT "Graphics."
    PRINT " "
    PRINT "No parameters or switches are required to run this program. Use the"
    PRINT "/SETUP switch to reselect your colour setup if you have previously run"
    PRINT "this program and your setup was stored. Note that the setup option auto"
    PRINT "detects colour monitors and this option with not work with mono monitors."
    PRINT " "
    PRINT "Progressions can also be projected on the data. The option to progress"
    PRINT "based on a single equation was replaced for progression of all equations"
    PRINT "for comparison purposes. Progressions should only be done on timed series"
    PRINT "ie. where the X value starts at 1 and increases in steps of 1. The"
    PRINT "progression will commence at 1 and increase in steps of one up to however"
    PRINT "many further pairs were required. It will show the original pairs as well"
    PRINT "as the progressions for each equation."
    PRINT " "
    PRINT "                         --  MORE  --"
    SLEEP
    PRINT "If you make a mistake with any data pair entered note the pair number"
    PRINT "that appears on the screen. There is an option afterwards to correct"
    PRINT "any errors and to store the data entered to an ASCII file for later"
    PRINT "retrieval. The program can also import timed series data from an ASCII"
    PRINT "file, where the file contains the Y value only (no  quotation  marks)."
    PRINT "When importing the ASCII file into Harvard Graphics, first create a Bar/Line"
    PRINT "chart. Then choose Import/Export from Main Menu, then Import Delimited"
    PRINT "ASCII file. Choose the directory and file. When prompted  for the ASCII"
    PRINT "delimiters the Quote character is a "; CHR$(34); ", the End of Field"
    PRINT "is a comma and the End of Record is #13#10. Usually the default values"
    PRINT "are correct. Choose 'NO' when prompted for importing the first"
    PRINT "series as Legends."
    PRINT " "
    PRINT "Contact Danie  v/d  Merwe  at  (021)  930-4445/6/7/8  with  any  problems"
    SLEEP
    END

ErrorHandler:
    CALL Alarm 
    CLS
    SELECT CASE ERR
        CASE 2:
            LOCATE 6, 10: PRINT "SYNTAX ERROR"
            GOSUB LOOPER
            RESUME
        CASE 5:
            LOCATE 6, 10: PRINT "ILLEGAL FUNCTION CALL - NUMBER COULD BE OUT OF RANGE"
            GOSUB  LOOPER
            RESUME
        CASE 6:
            LOCATE 6, 7: PRINT "CACULATION OVERFLOW - NUMBER TOO LARGE TO DISPLAY - TRY SMALLER VALUES"
            PRINT "LINE = "; ERL
            GOSUB LOOPER
            RESUME START
        CASE 7:
            LOCATE 6, 10: PRINT "OUT OF MEMORY - PROGRAM WILL ABORT"
            PRINT "LINE = "; ERL
            GOSUB LOOPER
            RESET
            END
        CASE 9: 
            LOCATE 6, 10: PRINT "SUBSCRIPT OUT OF RANGE (ARRAY ERROR) - PROGRAM WILL ABORT"
            PRINT "LINE = "; ERL
            GOSUB LOOPER
            RESET
            END
        CASE 11:
            LOCATE 6, 10: PRINT "DIVISION BY ZERO - WILL SKIP"
            GOSUB LOOPER
            RESUME NEXT
        CASE 13:
            LOCATE 6, 10: PRINT "TYPE  MISMATCH  (INTEGER,  STRING,ETC  EXPECTED)  -  TRY  AGAIN"
            GOSUB LOOPER
            RESUME
        CASE 16:
            LOCATE 6, 10: PRINT "STRING FORMULA TOO COMPLEX OR LONG - TRY AGAIN"
            GOSUB LOOPER
            RESUME
        CASE 24:
            LOCATE 6, 10: PRINT "DEVICE TIMEOUT - DEVICE NOT READY - CHECK PRINTER IS READY"
            GOSUB LOOPER
            CLS
            RESUME
        CASE 25:
            LOCATE 6, 25: PRINT "DEVICE FAULT - CHECK PRINTER"
            GOSUB LOOPER
            CLS
            RESUME
        CASE 27:
            LOCATE 6, 15: PRINT "PRINTER OUT OF PAPER - INSERT NEW PAPER"
            GOSUB LOOPER
            CLS
            RESUME
        CASE 52:
            LOCATE 6, 27: PRINT "BAD FILE NAME - RETRY"
            GOSUB LOOPER
            RESUME
        CASE 53:
            LOCATE 6, 26: PRINT "FILE NOT FOUND - RETRY"
            RESUME
        CASE 54:
            LOCATE 6, 25: PRINT "BAD FILE MODE - RETRY"
            GOSUB LOOPER
            RESUME
        CASE 55:
            LOCATE 6, 20: PRINT "FILE OPEN ERROR - RESETTING FILES"
            GOSUB LOOPER
            RESUME NEXT
        CASE 58:
            LOCATE 6, 18: PRINT "FILE ALREADY EXISTS ON DISC - CHANGE NAME"
            GOSUB LOOPER
            RESUME INITIALISE
        CASE 61:
            LOCATE 6, 18: PRINT "DISC IS FULL - YOU WILL HAVE TO RESTART"
            GOSUB LOOPER
            RESUME INITIALISE
        CASE 63:
            LOCATE 6, 18: PRINT "BAD RECORD NUMBER (LESS THAN OR EQUAL TO ZERO)"
            GOSUB LOOPER
            RESUME INITIALISE
        CASE 64:
            LOCATE 6, 18: PRINT "BAD FILE NAME (TOO MANY CHARACTERS?) - RETRY?"
            GOSUB LOOPER
            RESUME  EXPORT 
        CASE 68:
            LOCATE 6, 18: PRINT "DEVICE UNAVAILABLE OR OFF LINE - CHECK DRIVE AND PRINTER"
            GOSUB LOOPER
            CLS
            RESUME
        CASE 70:
            LOCATE 6, 22: PRINT "DISC IS WRITE PROTECTED - CORRECT IT"
            GOSUB LOOPER
            CLS
            RESUME
        CASE 71:
            LOCATE 6, 23: PRINT "DISC DRIVE DOOR OPEN OR NO DISC IN DRIVE"
            GOSUB LOOPER
            CLS
            RESUME
        CASE 72:
            LOCATE 6, 10: PRINT "DISC HAS FLAWS OR BAD SECTORS - TRY A FRESH DISC"
            GOSUB LOOPER
            RESUME
        CASE 57, 75, 76:
            LOCATE 6, 24: PRINT "FILE OR PATH NOT FOUND - RETRY"
            GOSUB LOOPER
            RESUME EXPORT
        CASE ELSE:
            LOCATE 8, 24: PRINT "UNRECOVERABLE ERROR. PROGRAM STOPPED"
            PRINT "QBASIC ERROR no = "; ERR
            PRINT "LINE = "; ERL
            GOSUB LOOPER
            RESET
            END
    END  SELECT

LOOPER:
    LOCATE 20, 16: COLOR 23: PRINT "PRESS SPACEBAR TO CONTINUE OR 'Q' TO ABORT PROGRAM": COLOR TEXT, BACK
    X$ = INPUT$(1)
    IF UCASE$(X$) = "Q" THEN END
    IF X$ = " " THEN RETURN
    GOTO LOOPER

DEFINT M-N
SUB Alarm
' The  alarm  procedure  uses  the  SOUND  statement  to  send  signals 
' to  the  computer's  speaker  and  sound  an  alarm.
'
'
' Parameters:  None
'
' Output: Sends an alarm to the user
'
' Change the numbers to vary the sound.
FOR Tone = 600 To 2000 STEP 40
    SOUND Tone, Tone / 700
NEXT Tone

END SUB

DEFINT M-P
SUB CheckSetup (TEXT, BACK)
'Checks for setup file
IF FnFileExist%("USERS.DAT") THEN
    OPEN "USERS.DAT" FOR RANDOM AS #1
    GET #1, 1, TEXT
    GET #1, 2, BACK
    GET #1, 3, MONS
    CLOSE #1

ELSEIF NOT FnFileExist%("USERS.DAT") THEN
    'If colour, get colours and save to file
    IF Monitor%(Segment%) > 2 THEN
        CALL GETCOLOR(TEXT, BACK)
        MON$ = "COL"
    'If  mono  save  default  colours
    ELSE
        TEXT = 7 
        BACK = 0
        MON$ = "MON"
    END IF
    OPEN "USERS.DAT" FOR RANDOM AS #1
    PUT #1, 1, TEXT
    PUT #1, 2, BACK
    PUT #1, 3, MON$
    CLOSE #1
    CLS
END  IF
IF MONS = "MON" THEN
    CALL TitleBW
ELSEIF MON$ = "COL" THEN
    CALL TitleCGA
    SCREEN 0
    WIDTH "SCRN:", 80
    CLS
END IF
COLOR TEXT, BACK
CLS
END SUB

DEFINT A-P
SUB Comline (NumArgs, Args$(), MaxArgs) STATIC
CONST TRUE = -1, FALSE = 0
    NumArgs = 0: In = FALSE
    ' Get the command line using the COMMAND$ function.
    C1$ = COMMAND$
    L = LEN(C1$)
    ' Go through the command line a character at a time.
    FOR IT = 1 T0 L
        C$ = MID$(C1$, I, 1)
        'Test for character being a blank or a tab.
        IF (C$ <> " " AND C$ <> CHR$(9)) THEN
            ' Neither blank nor tab.
            ' Test to see if you're already
            ' inside an argument.
            IF NOT In THEN
            ' You've found the start of a new argument.
            ' Test for too many arguments.
                IF NumArgs = MaxArgs THEN EXIT FOR
                NumArgs = NumArgs + 1
                In = TRUE
            END IF
            ' Add the character to the current argument.
            Args$(NumArgs) = Args$(NumArgs) + c$
        ELSE
            ' Found a blank or a tab 
            ' Set "Not in an argument" flag to FALSE.
            In = FALSE
        END IF
    NEXT I
END SUB

DEFINT  M-P
SUB GETCOLOR (TEXT, BACK)
    SCREEN 0, 0
    TEXT = 7
    BACK = 0
    DO
    COLOR TEXT, BACK
    CLS 
    LOCATE 12, 21: PRINT "USE ARROWS ON NUMERIC KEYBOARD FOR COLOURS"
    LOCATE 13, 27: PRINT "PRESS SPACEBAR TO SAVE COLOURS"
    DO
        Ky$ = INKEY$
    LOOP UNTIL LEN(Ky$)     'wait for keypress
    IF LEN(Ky$) = 1 THEN    'create keycode
        KeyCode = ASC(Ky$)  'regular key 
    ELSE                    'extended  key
        KeyCode = -ASC(RIGHT$(Ky$, 1))
    END IF
    
    SELECT CASE KeyCode
    CASE -75 'LEFT 
        IF BACK = 0 THEN BACK = 7 ELSE BACK = BACK - 1
    CASE -72 'UP
        IF TEXT = 0 THEN TEXT = 15 ELSE TEXT = TEXT - 1
    CASE -80 'DOWN
        IF TEXT = 15 THEN TEXT = 0 ELSE TEXT = TEXT + 1
    CASE -77 'RIGHT
        IF BACK = 7 THEN BACK = 0 ELSE BACK = BACK + 1
    CASE 13, 32
        EXIT DO

    CASE ELSE
    END SELECT

    LOOP UNTIL INKEY$ = " "
    CLS 
END  SUB 



DEFINT  M-Z
FUNCTION Monitor% (Segment) STATIC
DEF SEG = 0         'first see if it's color or mono
Segment = &HB800    'assume color
IF PEEK(&H463) = &HB4 THEN
    Segment = &HB000    'assign the monochrome segment
    Status = INP(&H3BA) 'get the current video status
    FOR X = 1 TO 30000  'test for a Hercules 30000 times
        IF INP(&H3BA) <> Status THEN
            Monitor% = 2 'the port changed, it's a Herc
            EXIT FUNCTION 'all done
        END IF
    NEXT
    Monitor% = 1  'it's a plain monochrome
ELSE    'it's some sort of color monitor
    Registers.ax = &H1A00   'first test for VGA
    CALL INTERRUPT(&H10, Registers, Registers)
    IF (Registers.ax AND &HFF) = &H1A THEN
        Monitor% = 5    'it's a VGA
        EXIT FUNCTION    'all done
    END IF

    Registers.ax = &H1200   'now test for EGARegisters.ax
    Registers.bx = &H10
    CALL INTERRUPT(&H10, Registers, Registers)
    IF (Registers.bx AND &HFF) = &H10 THEN
        Monitor% = 3     'if BL is still &H10 it's a CGA Monitor
    ELSE 
        Monitor% = 4     'otherwise it's an EGA
    END IF
END IF
END FUNCTION

DEFINT M-P
SUB TitleBW
    CLS
    LOCATE 7, 28: PRINT "***********************************"
    LOCATE 8, 28: PRINT "*                                 *" 
    LOCATE 9, 28: PRINT "*     ONE  INDEPENDANT            *"
    LOCATE 10, 28: PRINT "*                                 *"
    LOCATE 11, 28: PRINT "*     VARIABLE  REGRESSION        *"
    LOCATE 12, 28: PRINT "*                                 *"
    LOCATE 13, 28: PRINT "*       CALCULATOR                *"
    LOCATE 14, 28: PRINT "*                                 *"
    LOCATE 15, 28: PRINT "***********************************"
    LOCATE 17, 37: PRINT "Version 2.3"
    LOCATE 19, 33: PRINT "Serial no. 0000010" 
    SLEEP 
    CLS   
END  SUB 

DEFINT M-P
SUB TitleCGA
    SCREEN 1
    CLS
    LINE (78, 58)-(247, 122), 1, B
    LINE (82, 62)-(243, 118), 1, B
    LOCATE 10, 14: PRINT "ONE INDEPENDANT"
    LOCATE 12, 12: PRINT "VARIABLE REGRESSION"
    LOCATE 14, 16: PRINT "CALCULATOR"
    LOCATE 18, 16: PRINT "Version 2,3"
    LOCATE 20, 12: PRINT "Serial no. 0000010"
    DO WHILE INKEY$ = ""
        FOR X = 1 TO 3
            LINE (80, 60)-(245, 120), 0, B
            IF X = 1 THEN LINE (80, 60)-(245, 120), 2, B, &HFOFO
            IF X = 2 THEN LINE (80, 60)-(245, 120), 3, B, &H3C3C
            IF X = 3 THEN LINE (80, 60)-(245, 120), 1, B, &HFOF
            SLEEP 1
        NEXT X
    LOOP
END SUB

DEFINT M-P
SUB TitleVGA
    SCREEN 1
    CLS
    LINE (78, 58)-(247, 122), 1, B
    LINE (82, 62)-(243, 118), 1, B
    LOCATE 10, 14: PRINT "ONE  INDEPENDANT"
    LOCATE 12, 12: PRINT "VARIABLE  REGRESSION"
    LOCATE 14, 16: PRINT "CALCULATOR"
    LOCATE 18, 16: PRINT "Version  2.3"
    LOCATE 20, 12: PRINT "Serial  no.  0000010"
    DO WHILE INKEY$ = ""
        FOR X = 1 TO 3
            LINE (80, 60)-(245, 120), 0, B
            IF X = 1 THEN LINE (80, 60)-(245, 120), 2, B, &HFOFO
            IF X = 2 THEN LINE (80, 60)-(245, 120), 3, B, &H3C3C
            IF X = 3 THEN LINE (80, 60)-(245, 120), 1, B, &HFOF
            SLEEP 1
        NEXT X 
    LOOP
    CLS
    SCREEN 0
    COLOR TEXT, BACK
    WIDTH "SCRN:", 80
END SUB