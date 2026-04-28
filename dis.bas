' CMM2 Thumb/Thumb-2 Disassembler
' (c) 2026 Matthias Grimm

OPTION EXPLICIT
OPTION BASE 0
OPTION CONSOLE SCREEN
OPTION DEFAULT NONE

CONST VER.VER$ = "0"
CONST VER.REV$ = "1"
CONST VER.DATE$ = "20.4.2026"

#INCLUDE "inc/config.inc"
#INCLUDE "inc/error.inc"
#INCLUDE "inc/fileio.inc"
#INCLUDE "inc/disassembler.inc"

CONST MODE_DUMP = 0
CONST MODE_CODE = 1

CONST COLW_ADDR  = 8
CONST COLW_HWORD = 5
CONST COLW_MNE   = 10
CONST COLW_ARGS  = 32

DIM as.input$  = ""
DIM as.output$ = ""
DIM as.mode%  = MODE_DUMP

PRINT
PRINT "CMM2 Thumb/Thumb-2 Disassembler"
PRINT "Version V" + VER.VER$ + "." + VER.REV$ + " (" + VER.DATE$ + ") by Matthias Grimm"

DIM n% = 1, c%
DIM arg$
DO WHILE as.input$ = ""
    arg$ = FIELD$(MM.CMDLINE$, n%, " ")
    IF arg$ = "" THEN EXIT DO
    IF LEFT$(arg$, 1) = "-" THEN
        FOR c% = 2 TO LEN(arg$)
            SELECT CASE MID$(arg$, c%, 1)
	    CASE "c"
		as.mode%=MODE_CODE
	    CASE "d"
		as.mode%=MODE_DUMP
            CASE "o"
                IF c% <> LEN(arg$) THEN printError("Output file missing!") : EXIT DO
                INC n%
                as.output$ = FIELD$(MM.CMDLINE$, n%, " ")
            CASE ELSE
                printError("Unknown switch '-" + MID$(arg$, c%, 1) + "'!") : EXIT DO
            END SELECT
        NEXT
    ELSE
        as.input$ = arg$
    END IF
    INC n%
LOOP

IF as.input$ = "" THEN
    PRINT "Usage: " + CHR$(34) + "dis.bas" + CHR$(34) + ", " + CHR$(34) + "[-[c|d]o <output file>] <input file>" + CHR$(34)
    PRINT "       -c = output assembler code that as.bas can understand"
    PRINT "       -d = dump memory and disassemble it"
    PRINT "       -o = write disassembly to file; if omitted output goes to screen"
    PRINT "       <input file> is mandatory. file extension defines file type. Following types are supported:"
    PRINT "           .bin  = binary as created by as.bas"
    PRINT "           .csub = CSUB module"
    PRINT "           .bas  = CSUB module embedded in a Basic file"
    PRINT "           .elf  = compiled code in ELF file format, eg. created by gcc"

ELSE
    IF getExt$(as.input$) = "" THEN as.input$ = as.input$ + ".bin"
    DIM ext$ = UCASE$(getExt$(as.input$))
    SELECT CASE ext$
    CASE "ELF"
	readELFopen(as.input$)
	IF elf.funcCount% >0 THEN
	    elf.loadAll()
	    readELFClose()
	ENDIF
    CASE "CSUB", "BAS"
        readCSUB(as.input$)
    CASE ELSE
        readBinary(as.input$)
    END SELECT

    IF io.ptr% > 0 THEN
	IF as.output$ <> "" THEN
	    ON ERROR SKIP
	    OPEN as.output$ FOR OUTPUT AS #2
	    IF MM.ERRNO > 0 THEN printError("Can't create output file: '"+as.output$+"'") : END
	    dis.fh% = 2  'needed at he moment

	    PRINT #2,"; CMM2 Thumb/Thumb-2 Disassembler"
	    PRINT #2,"; Base: 0x" + HEX$(io.base%,8) + " - (" + STR$(io.ptr% * 2) + " bytes)"
	    PRINT #2,""
	ELSE
	    PRINT ""
	ENDIF

        dis.scan()
        disassemble()

	IF as.output$ <> "" THEN CLOSE #2
    END IF
END IF

SUB dis.out(addr%,hw0%,hw1%,mnm$)
    LOCAL tmp$,sp%,result$

    IF as.mode%=MODE_DUMP THEN
        tmp$=CHOICE(addr%=-1,"    ",RIGHT$("    "+HEX$(addr%),4))
	sp%=CHOICE(COLW_ADDR>4,COLW_ADDR-4,0)
	result$=" " + tmp$ + SPACE$(sp%)
    
        tmp$=CHOICE(hw0%=-1,"",HEX$(hw0% AND &HFFFF,4))
	sp%=CHOICE(LEN(tmp$)<COLW_HWORD,COLW_HWORD-LEN(tmp$),0)
	result$=result$+SPACE$(sp%)+tmp$
    
        tmp$=CHOICE(hw1%=-1,"",HEX$(hw1% AND &HFFFF,4))
	sp%=CHOICE(LEN(tmp$)<COLW_HWORD,COLW_HWORD-LEN(tmp$),0)
	result$=result$+SPACE$(sp%)+tmp$+"  "

	result$=result$+mnm$
    ELSE
	result$=SPACE$(COLW_ADDR)
	result$=result$+mnm$
    ENDIF
    dis.outln(result$)
END SUB

FUNCTION dis.fmtMne$(text$)
    LOCAL sp%

    sp%=CHOICE(LEN(text$)<COLW_MNE,COLW_MNE-LEN(text$),0)
    dis.fmtMne$=text$+SPACE$(sp%)
END FUNCTION

SUB dis.outln(text$)
    IF as.output$ = "" THEN
	PRINT text$
    ELSE
	PRINT #2, text$
    ENDIF
END SUB

SUB disassemble()
    LOCAL i% = 0, addr%, hw0%, hw1%, s$, idx%, poolWord%
    LOCAL itCond%(3), itN% = 0, itIdx% = 0
    LOCAL itCc%, itMask%, itInv%
    LOCAL cc0b%, b%, k%
    
    IF io.base% <> 0 THEN
        dis.out -1,-1,-1,dis.fmtMne$(".base") + "0x" + HEX$(io.base%,8)
	dis.outln
    END IF

    IF as.mode% = MODE_CODE THEN
        dis.out -1,-1,-1,dis.fmtMne$(".syntax") + "unified"
        dis.out -1,-1,-1,dis.fmtMne$(".cpu") + "cortex-m7"
        dis.out -1,-1,-1,dis.fmtMne$(".fpu") + "softvfp"
        dis.out -1,-1,-1,".thumb"
	dis.outln
    ENDIF

    ' Emit .equ directives for external (out-of-range) branch/ADR targets
    IF dis.extCount% > 0 THEN
        FOR k% = 0 TO dis.extCount% - 1
            dis.out -1,-1,-1, dis.fmtMne$(".equ") + dis.extName$(dis.extAddr%(k%)) + ", " + dis.h32$(dis.extAddr%(k%))
        NEXT
        dis.outln
    END IF

    DO WHILE i% < io.ptr%
        addr% = io.base% + i% * 2

        idx% = dis.labelIdx%(addr%)
        IF idx% >= 0 THEN
	    dis.outln
	    dis.outln "label"+STR$(idx%)+":"
        END IF

        ' Literal pool word
        IF dis.isPool%(addr%) THEN
            IF i% + 1 < io.ptr% THEN
		hw0%=io.buffer%(i%) AND &HFFFF
		hw1%=io.buffer%(i% + 1) AND &HFFFF
		dis.out addr%,hw0%,hw1%,dis.fmtMne$(".word")+"0x"+HEX$(hw1%<<16 OR hw0%,8)
                INC i%, 2
            ELSE
		hw0%=io.buffer%(i%) AND &HFFFF
		dis.out addr%,hw0%,.1,dis.fmtMne$(".hword")+"0x"+HEX$(hw0%,4)
                INC i%
            END IF
            CONTINUE DO
        END IF

        hw0% = io.buffer%(i%) AND &HFFFF
        IF dis.isThumb32%(hw0%) AND i% + 1 < io.ptr% THEN
	    hw1% = io.buffer%(i% + 1) AND &HFFFF
	    s$ = dis.decode$(hw0%, hw1%, addr%)
	ELSE
	    hw1% = -1
            s$ = dis.decode$(hw0%, 0, addr%)
	ENDIF

        ' Apply pending IT-block condition to this instruction
        IF itN% > 0 THEN
            s$ = dis.applyIT$(s$, dis.cc$(itCond%(itIdx%)))
            INC itIdx%
            INC itN%,-1
        END IF

        ' Detect IT instruction: set up conditions for the N follow-on instructions
        ' (N = 3 - position of lowest set bit in mask; bits above the guard are T/E markers)
        IF (hw0% AND &HFF00) = &HBF00 AND (hw0% AND &HF) <> 0 THEN
            itCc%   = (hw0% >> 4) AND &HF
            itMask% = hw0% AND &HF
            itInv%  = itCc% XOR 1
            cc0b%   = itCc% AND 1
            itIdx%  = 0
            IF (itMask% AND 1) <> 0 THEN
                itN% = 4
            ELSEIF (itMask% AND 2) <> 0 THEN
                itN% = 3
            ELSEIF (itMask% AND 4) <> 0 THEN
                itN% = 2
            ELSE
                itN% = 1
            END IF
            itCond%(0) = itCc%
            IF itN% >= 2 THEN
                IF ((itMask% >> 3) AND 1) = cc0b% THEN itCond%(1) = itCc% ELSE itCond%(1) = itInv%
            END IF
            IF itN% >= 3 THEN
                IF ((itMask% >> 2) AND 1) = cc0b% THEN itCond%(2) = itCc% ELSE itCond%(2) = itInv%
            END IF
            IF itN% >= 4 THEN
                IF ((itMask% >> 1) AND 1) = cc0b% THEN itCond%(3) = itCc% ELSE itCond%(3) = itInv%
            END IF
        END IF

	dis.out addr%,hw0%,hw1%,s$
        INC i%, dis.sz%
    LOOP
END SUB

