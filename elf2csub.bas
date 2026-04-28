' CMM2 Tool to convert ELF binaries into CSUBs
' (c) 2026 Matthias Grimm

OPTION EXPLICIT
OPTION BASE 0
OPTION CONSOLE SCREEN
OPTION DEFAULT NONE

CONST VER.VER$ = "1"
CONST VER.REV$ = "0"
CONST VER.DATE$ = "1.5.2026"

#INCLUDE "inc/config.inc"
#INCLUDE "inc/error.inc"
#INCLUDE "inc/fileio.inc"

CONST MODE_MERGE=0
CONST MODE_JOIN=1

DIM as.input$
DIM as.output$
DIM as.mode% = MODE_MERGE

PRINT
PRINT "ELF Binary to CSUB converter"
PRINT "Version V" + VER.VER$ + "." + VER.REV$ + " (" + VER.DATE$ + ") by Matthias Grimm"

DIM n%=1, c%, o%
DIM arg$,path$
DO WHILE as.input$=""
    arg$ = FIELD$(MM.CMDLINE$,n%," ")
    IF arg$="" THEN EXIT DO
    IF LEFT$(arg$,1) = "-" THEN
	FOR c%=2 TO LEN(arg$)
	    SELECT CASE MID$(arg$,c%,1)
	    CASE "j"
		as.mode% = MODE_JOIN
	    CASE "o"
		IF c% <> LEN(arg$) THEN printError("Output file missing!") : EXIT DO
		inc n%
		as.output$ = FIELD$(MM.CMDLINE$,n%," ")
	    CASE ELSE
		printError("Unknown switch '-"+MID$(arg$,c%,1)+"'!") : EXIT DO
	    END SELECT
	NEXT
    ELSE
	as.input$ = arg$
    ENDIF
    inc n%
LOOP

IF as.input$="" THEN
    PRINT "Usage: "+CHR$(22)+"elf2csub.bas"+CHR$(22)+", "+CHR$(22)+"[-jo <output file>] <input file>"+CHR$(22)
    PRINT "       -j = each C-function will be saved as separate CSUB file."
    PRINT "            (Default is to save all C-functions as one CSUB file)"
    PRINT "       -o = output file or <input>.csub if missing"
    PRINT "       If -o is omited, the output files will get the function name as filename"
    PRINT "       <input> is mandatory. if the extension is missing '.bin' will be attached."

ELSE
    IF getExt$(as.input$)="" THEN as.input$ = as.input$+".elf"
    path$=getPath$(as.input$)

    readELFopen(as.input$)
    IF elf.funcCount% >0 THEN
	IF as.mode% = MODE_MERGE THEN
	    elf.loadAll()
	    IF as.output$="" then as.output$=path$+elf.funcName$(elf.mainIdx%)+".csub"
	    writeCSUB(as.output$, elf.funcName$(elf.mainIdx%))
	    PRINT "Out: " + as.output$
	ELSE
	    FOR n%=0 to elf.funcCount%-1
		elf.loadFunc(n%)
		as.output$=path$+elf.funcName$(n%)+".csub"
		writeCSUB(as.output$, elf.funcName$(n%))
		PRINT "Out: " + as.output$
	    NEXT
	ENDIF
    ENDIF
    readELFClose()
ENDIF

