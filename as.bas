' CMM2 Thumb/Thumb-2 assembler
' (c) 2026 Matthias Grimm

OPTION EXPLICIT
OPTION BASE 0
OPTION CONSOLE SCREEN
OPTION DEFAULT NONE

CONST VER.VER$ = "1"
CONST VER.REV$ = "0"
CONST VER.DATE$ = "1.5.2026"

CONST FLG_LABELS  = 1 << 0
CONST FLG_SYMBOLS = 1 << 1
CONST FLG_CSUB    = 1 << 2   ' 0=bin | 1=csub

#INCLUDE "inc/config.inc"
#INCLUDE "inc/error.inc"
#INCLUDE "inc/mnemonics.inc"
#INCLUDE "inc/fileio.inc"
#INCLUDE "inc/lexer.inc"
#INCLUDE "inc/symbols.inc"
#INCLUDE "inc/literals.inc"
#INCLUDE "inc/directives.inc"
#INCLUDE "inc/tokenizer.inc"
#INCLUDE "inc/parser.inc"
#INCLUDE "inc/encoder.inc"

DIM as.input$
DIM as.output$
DIM as.incdir$
DIM as.flags% = FLG_CSUB
DIM as.lineNum%

' --- Include file machinery ---
CONST MAX_INC_DEPTH = 8     ' max nesting of .include
CONST MAX_INC_FILES = 64    ' max distinct files in one assembly run

DIM inc.sp%                          ' top of include stack: 0=outermost, -1=drained
DIM inc.line%(MAX_INC_DEPTH - 1)     ' saved parent line numbers
DIM inc.file$(MAX_INC_DEPTH - 1) LENGTH 128
DIM inc.seen$(MAX_INC_FILES - 1) LENGTH 128   ' UCASE$ of every file ever opened
DIM inc.seenCount%

PRINT
PRINT "CMM2 Thumb/Thumb-2 Assembler"
PRINT "Version V" + VER.VER$ + "." + VER.REV$ + " (" + VER.DATE$ + ") by Matthias Grimm"

DIM n% = 1, c%
DIM arg$, tmp$
DO WHILE as.input$=""
    arg$ = FIELD$(MM.CMDLINE$,n%," ")
    IF arg$="" THEN EXIT DO
    IF LEFT$(arg$,1) = "-" THEN
	FOR c%=2 TO LEN(arg$)
	    SELECT CASE MID$(arg$,c%,1)
	    CASE "f"
		inc n%
		tmp$ = LCASE$(FIELD$(MM.CMDLINE$,n%," "))
		IF c% <> LEN(arg$) OR tmp$="" THEN printError("Output format missing!") : EXIT DO
		IF tmp$="csub" THEN
		    as.flags% = as.flags% OR FLG_CSUB
		ELSE IF tmp$="bin" THEN
		    as.flags% = as.flags% AND NOT FLG_CSUB
		ELSE
		    printError("Unknow output format '" + tmp$ + "'") : EXIT DO
		ENDIF
	    CASE "l"
		as.flags% = as.flags% OR FLG_LABELS
	    CASE "s"
		as.flags% = as.flags% OR FLG_SYMBOLS
	    CASE "o"
		IF c% <> LEN(arg$) THEN printError("Output file missing!") : EXIT DO
		inc n%
		as.output$ = FIELD$(MM.CMDLINE$,n%," ")
	    CASE "I"
		IF c% <> LEN(arg$) THEN printError("Include directory missing!") : EXIT DO
		inc n%
		as.incdir$ = FIELD$(MM.CMDLINE$,n%," ")
	    CASE ELSE
		printError("Unknown assembler switch '-"+MID$(arg$,c%,1)+"'!") : EXIT DO
	    END SELECT
	NEXT
    ELSE
	as.input$ = arg$
    ENDIF
    inc n%
LOOP

IF as.input$="" THEN
    PRINT "Usage: "+CHR$(34)+"as.bas"+CHR$(34)+", "+CHR$(34)+"[-lsf <csub|bin>] [-o <output file>] [-I <include dir>] <input file>"+CHR$(34)
    PRINT "        -f = output format, default csub"
    PRINT "        -l = print table of labels"
    PRINT "        -s = print table of symbols"
    PRINT "        -I = prefix for all files from the .include directive"
    PRINT "        -o = output file or <input>.bin if missing"
    PRINT "        <input> is mandatory. if the extension is missing '.s' will be attached."

ELSE
    IF getExt$(as.input$)="" THEN as.input$ = as.input$+".s"
    IF as.output$="" THEN as.output$=stripExt$(as.input$) + CHOICE((as.flags% AND FLG_CSUB)=FLG_CSUB, ".csub",".bin")
    IF as.incdir$<>"" THEN
	IF MID$(as.incdir$,LEN(as.incdir$),1) <> "/" THEN as.incdir$ = as.incdir$ + "/"
    ENDIF

    initMnemonics()
    PRINT "-> " + STR$(MAX_MNE) + " mnemonics loaded."

    as.assemble(as.input$)
    IF as.flags% AND FLG_SYMBOLS = FLG_SYMBOLS THEN dumpSymbols()
    IF as.flags% AND FLG_LABELS  = FLG_LABELS THEN dumpLabels()
ENDIF

' Assemble one source file.
' @param srcFile$  assembler source file
SUB as.assemble(srcFile$)
    LOCAL srcLine$, fh%, totalLines%

    PRINT "-> Assembling : " + srcFile$ + " to " + as.output$

    as.resetState()

    ' --- Open top-level source file as the bottom of the include stack ---
    ON ERROR SKIP
    OPEN srcFile$ FOR INPUT AS #1
    IF MM.ERRNO > 0 THEN printError("File not found: " + srcFile$,3) : EXIT SUB

    inc.sp%             = 0
    inc.file$(0)        = srcFile$
    inc.seen$(0)        = UCASE$(srcFile$)
    inc.seenCount%      = 1
    as.lineNum%         = 0

    ' --- Main assembly loop: drains entire include stack ---
    DO WHILE inc.sp% >= 0
	fh% = inc.sp% + 1
	IF EOF(fh%) THEN
	    CLOSE #fh%
	    INC inc.sp%, -1
	    IF inc.sp% >= 0 THEN
		as.lineNum% = inc.line%(inc.sp%)
		err.line%   = as.lineNum%
	    END IF
	ELSE
	    LINE INPUT #fh%, srcLine$
	    INC as.lineNum%
	    INC totalLines%
	    err.line% = as.lineNum%

	    ' Strip trailing CR (Windows CRLF files)
	    IF LEN(srcLine$) > 0 THEN
		IF RIGHT$(srcLine$, 1) = CHR$(13) THEN
		    srcLine$ = LEFT$(srcLine$, LEN(srcLine$) - 1)
		END IF
	    END IF

	    tokenizeLine(srcLine$)
	    IF tok.count% > 0 THEN
		IF tok.type%(0) = TT_DIR THEN
		    processDir(tok.str$(0), srcLine$)
		ELSE
		    IF validateLine%() = 0 THEN
			encode()
			IF needsFlushLit%() THEN flushLit(0)
		    END IF
		END IF
	    END IF
	END IF
    LOOP

    ' --- Post-processing ---
    IF lit.count% > 0 THEN flushLit(1)
    checkLbls()

    ' --- Report ---
    PRINT "   " + STR$(totalLines%) + " lines,  " + STR$(io.ptr% * 2) + " bytes assembled."
    IF err.count% > 0 THEN
	PRINT "   " + STR$(err.count%) + " error" + CHOICE(err.count%=1,"","s") + "  -  No output written."
    ELSE
	PRINT "   Output file '" + as.output$ + "' written"
	IF (as.flags% AND FLG_CSUB)=FLG_CSUB THEN
	    writeCsub(as.output$)
	ELSE
	   writeBinary(as.output$)
	ENDIF
    END IF
    PRINT ""
END SUB

' Reset all assembler states for a fresh assembly run.
SUB as.resetState()
    LOCAL rsi%

    tok.count%  = 0
    sym.count%  = 0
    lbl.count%  = 0
    lit.count%  = 0
    io.ptr%    = 0
    io.base%   = 0
    it.active%  = 0
    it.cond%    = 0
    it.count%   = 0
    it.pos%     = 0
    it.mask%    = 0
    err.count%  = 0
    err.line%   = 0
    dir.pendByte% = -1
    inc.sp%        = -1
    inc.seenCount% = 0
    as.lineNum%    = 0

    FOR rsi% = 0 TO MAX_BUFFER - 1
	enc.patchType%(rsi%) = 0
	enc.patchAux%(rsi%)  = 0
    NEXT rsi%
END SUB

' Process a .include "name" directive: insert another source file at the
' current position. Filenames must be bare names (no path components).
' The file is searched in the current directory first, then in as.incdir$.
' A file already opened earlier in this assembly run is silently skipped
' with a warning (case-insensitive comparison on the resolved path).
SUB as.includeFile(name$)
    LOCAL resolved$ LENGTH 128
    LOCAL key$      LENGTH 128
    LOCAL i%, fh%

    ' Reject path components — only basenames are allowed
    IF INSTR(name$, "/")  > 0 OR INSTR(name$, "\")  > 0 OR INSTR(name$, ":")  > 0 THEN
	showError ".include: path components not allowed: " + name$
	EXIT SUB
    END IF

    ' Resolve: try CWD first, then as.incdir$
    resolved$ = ""
    ON ERROR SKIP
    OPEN name$ FOR INPUT AS #9
    IF MM.ERRNO = 0 THEN
	resolved$ = name$
	CLOSE #9
    ELSEIF as.incdir$ <> "" THEN
	ON ERROR SKIP
	OPEN as.incdir$ + name$ FOR INPUT AS #9
	IF MM.ERRNO = 0 THEN
	    resolved$ = as.incdir$ + name$
	    CLOSE #9
	END IF
    END IF

    IF resolved$ = "" THEN
	showError ".include: file not found: " + name$
	EXIT SUB
    END IF

    ' Case-insensitive duplicate check
    key$ = UCASE$(resolved$)
    FOR i% = 0 TO inc.seenCount% - 1
	IF inc.seen$(i%) = key$ THEN
	    showWarn ".include: already included, skipped: " + name$
	    EXIT SUB
	END IF
    NEXT i%

    ' Depth check
    IF inc.sp% + 1 >= MAX_INC_DEPTH THEN
	showError ".include: nesting too deep at: " + name$
	EXIT SUB
    END IF

    IF inc.seenCount% >= MAX_INC_FILES THEN
	showError ".include: too many distinct files (max " + STR$(MAX_INC_FILES) + ")"
	EXIT SUB
    END IF

    ' Push: save parent line number, advance stack, open child
    inc.line%(inc.sp%) = as.lineNum%
    INC inc.sp%
    inc.file$(inc.sp%) = resolved$
    fh% = inc.sp% + 1
    OPEN resolved$ FOR INPUT AS #fh%
    as.lineNum% = 0
    err.line%   = 0

    inc.seen$(inc.seenCount%) = key$
    INC inc.seenCount%
END SUB

