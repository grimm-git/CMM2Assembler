' CMM2 Tool to convert CSUB into binaries
' It can extract CSUBs from basic files
' (c) 2026 Matthias Grimm

OPTION EXPLICIT
OPTION BASE 0
OPTION CONSOLE SCREEN
OPTION DEFAULT NONE

CONST VER.VER$ = "0"
CONST VER.REV$ = "1"
CONST VER.DATE$ = "18.4.2026"

CONST FLG_PRESERVE_NAME = 1
CONST FLG_PRESERVE_SIG = 2

#INCLUDE "inc/config.inc"
#INCLUDE "inc/error.inc"
#INCLUDE "inc/fileio.inc"

DIM as.input$  = ""
DIM as.output$ = ""
DIM as.flags%  = 0

PRINT
PRINT "CSUB to Binary converter"
PRINT "Version V" + VER.VER$ + "." + VER.REV$ + " (" + VER.DATE$ + ") by Matthias Grimm"

DIM n%=1, c%
DIM arg$, ext$, path$
DO WHILE as.input$=""
    arg$ = FIELD$(MM.CMDLINE$,n%," ")
    IF arg$="" THEN EXIT DO
    IF LEFT$(arg$,1) = "-" THEN
	FOR c%=2 TO LEN(arg$)
	    SELECT CASE MID$(arg$,c%,1)
	    CASE "p"
		as.flags%=FLG_PRESERVE_NAME
	    CASE "P"
		as.flags%=FLG_PRESERVE_SIG
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
    PRINT "Usage: "+CHR$(34)+"csub2bin.bas"+CHR$(34)+", "+CHR$(34)+"[-pP | -o <output file>] <input file>"+CHR$(34)
    PRINT "       -p = preserve the CSUB name as file name"
    PRINT "       -P = preserve the complete signature as file name: NAME_TYPE_TYPE_..bin"
    PRINT "       -o = output file or <input>.bin if missing, overwritten by -p and -P"
    PRINT "        <input> is mandatory. If the extension is missing ".bas" will be attached."
ELSE
    IF getExt$(as.input$)="" THEN as.input$ = as.input$+".bas"
    IF as.output$="" THEN as.output$ = stripExt$(as.input$) + ".bin"

    readCSUB(as.input$)
    printCSUBSignature()

    path$=getPath$(as.input$)
    ext$=getExt$(as.output$)
    SELECT CASE as.flags%
    CASE FLG_PRESERVE_NAME
	as.output$=path$+csub.name$+"."+ext$
    CASE FLG_PRESERVE_SIG
	as.output$=path$+csub.name$
	FOR n%=0 to 10
	    if csub.prms$(n%)="" THEN EXIT FOR
	    as.output$=as.output$+"_"+csub.prms$(n%)
	NEXT
	as.output$=as.output$+"."+ext$
    END SELECT

    writeBinary(as.output$)
    PRINT "Out: " + as.output$
    IF as.flags%=0 THEN
        printWarning("The CSUB signature can't be preserved in binary format (see option -p or -P)",5)
    ENDIF
ENDIF

