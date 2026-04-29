' ============================================================
' fileio.inc  -  File I/O functions including read/write handler
'                for supported formats
'
' File handle conventions:
'   #1 = sequential reader  (readBinary, readCSUB)
'   #2 = sequential writer  (writeBinary, writeCSUB)
'   #3 = random-access ELF  (readELFOpen .. readELFClose)
'
' Writers:
'   writeBinary(filename$)             - raw little-endian halfword stream
'   writeCSUB   (filename$, csubName$) - MMBasic CSUB wrapper
'
' Readers (populate io.buffer%(), io.ptr%, io.entry%):
'   readBinary(filename$)  - raw .bin → halfword buffer
'   readCSUB  (filename$)  - parse MMBasic CSUB wrapper, skip entry word
'   readELFMerge(filename$)- ARM ELF .text+rodata → buffer  [merge mode]
'
' ELF two-phase API (merge or join mode):
'   readELFOpen(filename$) - parse ELF, build function table, leave #3 open
'   elf.loadAll()          - merge: full .text (+dinit pad +.rodata) → buffer
'   elf.loadFunc(n%)       - join:  function n only             → buffer
'   readELFClose()         - close #3
'   elf.funcCount%         - number of functions found
'   elf.funcName$(n%)      - function name  (0-based)
'   elf.funcSize%(n%)      - function size in bytes
'
' io.entry% is written by ELF/assembler before writeCSUB:
'   merge → word-offset of main/first function
'   join  → 0  (function is at start of its own buffer)
'   assemble → word-offset of first defined label  (set by caller)
'
' The readers share the same io.buffer%() pivot, so any input
' format can be paired with any output format.
' ============================================================

CONST MAX_BUFFER   = 16384   ' output buffer in halfwords (= 32 KB)

DIM io.buffer%(MAX_BUFFER)
DIM io.ptr%    = 0    ' next free halfword slot
DIM io.base%   = 0    ' base address (set by .base directive, default 0)
DIM io.entry%  = 0    ' CSUB entry word (word-offset of entry point / 4)

DIM csub.name$
DIM csub.prms$(10)

' --- ELF reader state (populated by readELFOpen) ---
CONST MAX_ELF_FUNCS = 64
DIM elf.funcCount%    = 0
DIM elf.funcName$(MAX_ELF_FUNCS) LENGTH MAX_NAME_LEN
DIM elf.funcAddr%(MAX_ELF_FUNCS)   ' virtual address of each function (with Thumb bit)
DIM elf.funcSize%(MAX_ELF_FUNCS)   ' byte size of each function
DIM elf.textBase%     = 0    ' .text virtual base address
DIM elf.textFileOfs%  = 0    ' .text file offset
DIM elf.textSize%     = 0    ' .text byte size
DIM elf.rodataFileOfs% = 0   ' .rodata file offset (0 if absent)
DIM elf.rodataSize%   = 0    ' .rodata byte size   (0 if absent)
DIM elf.dinitSize%    = 0    ' .dinit padding bytes needed between .text and .rodata
DIM elf.mainIdx%      = -1   ' index of 'main' or first function in sorted table
DIM elf.error%        = 0    ' =1 if elfReadOpen() failed

' Write io.buffer%(0..io.ptr%-1) as a raw binary file.
' Each halfword is written little-endian (low byte first).
' @param   filename$   name of file to write
SUB writeBinary(filename$)
    LOCAL i%, hw%
    
    OPEN filename$ FOR OUTPUT AS #2
    FOR i% = 0 TO io.ptr% - 1
	hw% = io.buffer%(i%) AND &HFFFF
	PRINT #2, CHR$(hw% AND &HFF) + CHR$((hw% >> 8) AND &HFF);
    NEXT
    CLOSE #2
END SUB

' Write io.buffer%() as an MMBasic CSUB source file.
' csubName$ is the identifier after "CSUB" and "End CSUB".
'
' Format:
'   CSUB <name>
'       <entry_offset_word>
'       <up-to-8 hex words per line, tab-indented>
'   End CSUB
'
' Each 32-bit word = io.buffer%(2*i) OR (io.buffer%(2*i+1) << 16).
' entry_offset_word = io.entry% (set by caller before invoking writeCSUB).
' Pad to 4-byte boundary with zeros before calculating words.
' @param   filename$   Name of the CSUB file
' @param   csubName$   Name of the CSUB
SUB writeCSUB(filename$, csubName$)
    LOCAL i%, nHW%, nWords%, wordVal%, linePos%, entryWord%
    LOCAL hex8$ LENGTH 8

    ' Pad output to 4-byte (2 halfword) boundary
    IF (io.ptr% AND 1) <> 0 THEN
	io.buffer%(io.ptr%) = 0
	INC io.ptr%
    END IF

    nHW%    = io.ptr%
    nWords% = nHW% / 2

    ' Entry word set by caller via io.entry% (assembler: first label / 4;
    ' ELF merge: main offset / 4; ELF join / binary: 0).
    entryWord% = io.entry%

    OPEN filename$ FOR OUTPUT AS #2
    PRINT #2, "CSUB " + csubName$
    PRINT #2, CHR$(9) + RIGHT$("00000000" + HEX$(entryWord% AND &HFFFFFFFF), 8)

    linePos% = 0
    FOR i% = 0 TO nWords% - 1
	wordVal% = (io.buffer%(i% * 2) AND &HFFFF) OR ((io.buffer%(i% * 2 + 1) AND &HFFFF) << 16)
	hex8$    = RIGHT$("00000000" + HEX$(wordVal% AND &HFFFFFFFF), 8)
	IF linePos% = 0 THEN PRINT #2, CHR$(9);
	PRINT #2, hex8$;
	linePos% = linePos% + 1
	IF linePos% = 8 OR i% = nWords% - 1 THEN
	    PRINT #2, ""
	    linePos% = 0
	ELSE
	    PRINT #2, " ";
	END IF
    NEXT

    PRINT #2, "End CSUB"
    CLOSE #2
END SUB

' Load a raw little-endian binary file into io.buffer%().
' Two bytes per halfword; an odd trailing byte is padded with 0.
' @param   filename$   File name to read
SUB readBinary(filename$)
    LOCAL hw%
    LOCAL c1$ LENGTH 1
    LOCAL c2$ LENGTH 1

    ON ERROR SKIP
    OPEN filename$ FOR INPUT AS #1
    IF MM.ERRNO>0 THEN printError "File not found: " + filename$ : EXIT SUB
    
    DO
	c1$ = INPUT$(1, #1)
	IF LEN(c1$) = 0 THEN EXIT DO
	c2$ = INPUT$(1, #1)
	IF LEN(c2$) = 0 THEN
	    hw% = ASC(c1$)
	ELSE
	    hw% = ASC(c1$) OR (ASC(c2$) << 8)
	END IF
	IF io.ptr% >= MAX_BUFFER THEN
	    printError "Input exceeds output buffer"
	    EXIT DO
	END IF
	io.buffer%(io.ptr%) = hw% AND &HFFFF
	INC io.ptr%
    LOOP
    CLOSE #1
    PRINT "Read " + filename$ + "  (" + STR$(io.ptr% * 2) + " bytes)"
END SUB

' Reads an MMBasic CSUB file into io.buffer%().
' Accepts the exact format produced by writeCSUB():
'   CSUB <name>
'       <entry_offset_word>           (skipped — metadata only)
'       <hex_word> <hex_word> ...     (up to 8 per line)
'   End CSUB
' Each 32-bit hex word is split into two little-endian halfwords.
' @param   filename$   BAS file name to extract CSUB from
SUB readCSUB(filename$)
    LOCAL inBody%=0, entrySeen%=0, eof%=0
    LOCAL buf$         LENGTH 255
    LOCAL trimmed$     LENGTH 255
    LOCAL p%, i%

    ON ERROR SKIP
    OPEN filename$ FOR INPUT AS #1
    IF MM.ERRNO>0 THEN printError "File not found: " + filename$ : EXIT SUB
    
    DO
	io.readLine(buf$, eof%)
	IF eof% AND LEN(buf$) = 0 THEN EXIT DO
	trimmed$ = trimStr$(buf$)

	IF inBody% = 0 THEN
	    IF LEN(trimmed$) >= 5 AND UCASE$(LEFT$(trimmed$, 5)) = "CSUB " THEN
		p% = INSTR(6,trimmed$," ")
		IF p%>0 THEN
		    csub.name$=MID$(trimmed$,6,p%-6)
		    trimmed$=MID$(trimmed$,p%+1)
		    FOR i%=0 to 10
			csub.prms$(i%)=FIELD$(trimmed$,i%+1,",")
		    NEXT
		ELSE
		    csub.name$=MID$(trimmed$,6)  ' No parameters given
		ENDIF
		inBody% = 1
	    ELSEIF UCASE$(trimmed$) = "CSUB" THEN
		printWarning "CSUB header without name in " + filename$
		csub.name$ = ""
		inBody% = 1
	    END IF
	ELSE
	    IF UCASE$(trimmed$) = "END CSUB" THEN EXIT DO
	    IF LEN(trimmed$) = 0 THEN CONTINUE DO
	    IF entrySeen% = 0 THEN
		entrySeen% = 1          ' first body line = entry offset; ignore
	    ELSE
		io.parseCsubHexLine(trimmed$)
	    END IF
	END IF
	IF eof% THEN EXIT DO
    LOOP
    CLOSE #1

    IF inBody% = 0 THEN
	printError "CSUB header not found in " + filename$
	EXIT SUB
    END IF
    PRINT "Read " + filename$ + "  (" + STR$(io.ptr% * 2) + " bytes)"
END SUB

SUB printCSUBSignature()
    LOCAL i%

    IF csub.name$ = "" THEN
	PRINT "     CSUB Signature: None found"
    ELSE
        PRINT "     CSUB Signature: " + csub.name$ + "(";
	FOR i%=0 to 10
	    IF csub.prms$(i%) <> "" THEN
		IF i%>0 THEN PRINT ",";
		PRINT csub.prms$(i%);
	   ENDIF
	NEXT
	PRINT ")"
    ENDIF
END SUB

' Read one line from #1 into line$ (max 255 chars).  Any characters
' past 255 on the same line are read and discarded, so input files
' with long pre-CSUB BASIC lines do not blow the string-length limit.
' eof% is set to 1 when EOF is reached *before or on* the terminator.
' @param   dst$
' @param   eof%   Flag, indicating that EOF has been reached
SUB io.readLine(dst$, eof%)
    LOCAL n%
    LOCAL c$ LENGTH 1

    dst$ = ""
    n%   = 0
    DO
	IF EOF(1) THEN eof% = 1 : EXIT SUB
	c$ = INPUT$(1, #1)
	IF LEN(c$) = 0 THEN eof% = 1 : EXIT SUB
	IF c$ = CHR$(10) THEN EXIT SUB
	IF c$ <> CHR$(13) THEN
	    IF n% < 255 THEN
		dst$ = dst$ + c$
		n% = n% + 1
	    END IF
	END IF
    LOOP
END SUB

' Helper: split a whitespace-separated list of 8-hex-digit words into
' two little-endian halfwords each, appended to io.buffer%().
' @param   s$
SUB io.parseCsubHexLine(s$)
    LOCAL p%, startP%, n%, wordVal%
    LOCAL c$   LENGTH 1
    LOCAL tok$ LENGTH 16

    n% = LEN(s$)
    p% = 1
    DO WHILE p% <= n%
	c$ = MID$(s$, p%, 1)
	IF c$ = " " OR c$ = CHR$(9) THEN
	    p% = p% + 1
	ELSE
	    startP% = p%
	    DO WHILE p% <= n%
		c$ = MID$(s$, p%, 1)
		IF c$ = " " OR c$ = CHR$(9) THEN EXIT DO
		p% = p% + 1
	    LOOP
	    tok$ = MID$(s$, startP%, p% - startP%)
	    wordVal% = VAL("&H" + tok$)
	    IF io.ptr% + 1 >= MAX_BUFFER THEN
		printError "Input exceeds output buffer"
		EXIT SUB
	    END IF
	    io.buffer%(io.ptr%)     = wordVal% AND &HFFFF
	    io.buffer%(io.ptr% + 1) = (wordVal% >> 16) AND &HFFFF
	    INC io.ptr%,2
	END IF
    LOOP
END SUB

' Return the extension (without the leading dot), or "" if the
' filename has no extension. Stops at path separators so a
' dotted directory name does not fool the search.
' @param name$ = file name to return extension from
FUNCTION getExt$(name$)
  LOCAL i%
  LOCAL c$ LENGTH 1

  FOR i% = LEN(name$) TO 1 STEP -1
    c$ = MID$(name$, i%, 1)
    IF c$ = "/" OR c$ = "\" THEN getExt$ = "" : EXIT FUNCTION
    IF c$ = "." THEN getExt$ = MID$(name$, i% + 1) : EXIT FUNCTION
  NEXT i%
  getExt$ = ""
END FUNCTION

' Remove file extension (last dot and everything after it).
' @param name$ = file name to remove extension from
FUNCTION stripExt$(name$)
    LOCAL i%

    FOR i% = LEN(name$) TO 1 STEP -1
	IF MID$(name$, i%, 1) = "." THEN
          stripExt$ = LEFT$(name$, i% - 1)
          EXIT FUNCTION
	END IF
    NEXT
    stripExt$ = name$
END FUNCTION

' Remove file name, returning directory path only.
' @param  path$  file path
FUNCTION getPath$(path$)
    LOCAL i%

    FOR i% = LEN(path$) TO 1 STEP -1
	IF MID$(path$, i%, 1) = "/" OR MID$(path$, i%, 1) = "\" THEN
	    getPath$ = LEFT$(path$, i%)
	    EXIT FUNCTION
	END IF
    NEXT
    getPath$ = ""
END FUNCTION

' Remove directory path, returning filename only.
' @param  path$  file path
FUNCTION stripPath$(path$)
    LOCAL i%

    FOR i% = LEN(path$) TO 1 STEP -1
	IF MID$(path$, i%, 1) = "/" OR MID$(path$, i%, 1) = "\" THEN
	    stripPath$ = MID$(path$, i% + 1)
	    EXIT FUNCTION
	END IF
    NEXT
    stripPath$ = path$
END FUNCTION


' Trim leading and trailing spaces (TRIM$ is not available in MMBasic).
' @param   s$   String to clean
FUNCTION trimStr$(s$)
    LOCAL i%, j%

    i% = 1
    DO WHILE i% <= LEN(s$) AND MID$(s$, i%, 1) = " "
	INC i%
    LOOP
    IF i% > LEN(s$) THEN trimStr$ = "" : EXIT FUNCTION
    j% = LEN(s$)
    DO WHILE j% >= i% AND MID$(s$, j%, 1) = " "
	j% = j% - 1
    LOOP
    trimStr$ = MID$(s$, i%, j% - i% + 1)
END FUNCTION

' ============================================================
' ELF reader  (ARM 32-bit little-endian ELF, Thumb code)
'
' Private helpers: elf.rdN, elf.b1%, elf.b2%, elf.b4%, elf.rdStr$
' Public API:
'   readELFOpen(filename$)   - parse ELF, build function table, keep #3 open
'   readELFClose()           - close #3
'   elf.loadAll()            - merge: .text [+dinit pad] [+.rodata] → io.buffer%()
'   elf.loadFunc(n%)         - join:  one function → io.buffer%()
'   readELFMerge(filename$)  - convenience: open + loadAll + close
' ============================================================

' Read n% bytes from file #3 at 0-based file offset ofs% into buf$.
' @param  ofs%   0-based file byte offset
' @param  n%     number of bytes to read
' @param  buf$   output buffer (passed by reference)
SUB elf.rdN(ofs%, n%, buf$)
    SEEK #3, ofs% + 1
    buf$ = INPUT$(n%, #3)
END SUB

' Parse unsigned byte from string buf$ at 0-based offset p%.
FUNCTION elf.b1%(buf$, p%)
    elf.b1% = ASC(MID$(buf$, p% + 1, 1))
END FUNCTION

' Parse 2-byte little-endian value from string buf$ at 0-based offset p%.
FUNCTION elf.b2%(buf$, p%)
    elf.b2% = ASC(MID$(buf$, p% + 1, 1)) OR (ASC(MID$(buf$, p% + 2, 1)) << 8)
END FUNCTION

' Parse 4-byte little-endian value from string buf$ at 0-based offset p%.
FUNCTION elf.b4%(buf$, p%)
    LOCAL b0%, b1%, b2%, b3%
    b0% = ASC(MID$(buf$, p% + 1, 1))
    b1% = ASC(MID$(buf$, p% + 2, 1))
    b2% = ASC(MID$(buf$, p% + 3, 1))
    b3% = ASC(MID$(buf$, p% + 4, 1))
    elf.b4% = b0% OR (b1% << 8) OR (b2% << 16) OR (b3% << 24)
END FUNCTION

' Read null-terminated string from file #3 at 0-based offset ofs%.
' Reads at most MAX_NAME_LEN characters; longer names are truncated.
' @param  ofs%   0-based file byte offset of first character
FUNCTION elf.rdStr$(ofs%)
    LOCAL c$  LENGTH 1
    LOCAL s$  LENGTH MAX_NAME_LEN
    LOCAL n%  = 0
    SEEK #3, ofs% + 1
    DO
        c$ = INPUT$(1, #3)
        IF LEN(c$) = 0 OR ASC(c$) = 0 THEN EXIT DO
        IF n% < MAX_NAME_LEN THEN s$ = s$ + c$ : n% = n% + 1
    LOOP
    elf.rdStr$ = s$
END FUNCTION

' Parse the ELF file and build the function table.  File #3 is left open
' for subsequent elf.loadAll() or elf.loadFunc() calls.
'
' Populates: elf.funcCount%, elf.funcName$(), elf.funcAddr%(),
'            elf.funcSize%(), elf.mainIdx%,
'            elf.textBase%, elf.textFileOfs%, elf.textSize%,
'            elf.rodataFileOfs%, elf.rodataSize%, elf.dinitSize%
'
' ELF section header layout (40 bytes, 32-bit ELF):
'   +0  sh_name (4)   +4  sh_type (4)   +8  sh_flags (4)
'   +12 sh_addr (4)   +16 sh_offset (4) +20 sh_size (4)  ...
'
' Symbol table entry layout (16 bytes, 32-bit ELF):
'   +0  st_name (4)   +4  st_value (4)  +8  st_size (4)
'   +12 st_info (1)   +13 st_other (1)  +14 st_shndx (2)
'   st_info = 0x12 → global function (STB_GLOBAL | STT_FUNC)
'
' @param  filename$  path to the ARM ELF file
SUB readELFOpen(filename$)
    LOCAL head$    LENGTH 52
    LOCAL sh$     LENGTH 40
    LOCAL sym16$  LENGTH 16
    LOCAL sname$  LENGTH MAX_NAME_LEN
    LOCAL fname$  LENGTH MAX_NAME_LEN
    LOCAL tName$  LENGTH MAX_NAME_LEN
    LOCAL shBase%, shESize%, shCount%, shStrIdx%
    LOCAL strTabBase%
    LOCAL j%, i%
    LOCAL nameOfs%, vaddr%, sinfo%
    LOCAL txtSect%, symSect%, strSect%, rodSect%, dinitSect%
    LOCAL rodBase%, dinitBase%, dinitSize%
    LOCAL symBase%, symTabSize%, symCount%, symStrBase%
    LOCAL textEnd%, tAddr%

    elf.funcCount%     = 0
    elf.textBase%      = 0
    elf.textFileOfs%   = 0
    elf.textSize%      = 0
    elf.rodataFileOfs% = 0
    elf.rodataSize%    = 0
    elf.dinitSize%     = 0
    elf.mainIdx%       = -1
    elf.error%         = 0

    ON ERROR SKIP
    OPEN filename$ FOR RANDOM AS #3
    IF MM.ERRNO > 0 THEN printError("ELF file not found: " + filename$) : elf.error%=1 : EXIT SUB

    ' --- ELF header (52 bytes) ---
    ' e_shoff   at byte 32  (4 bytes): section header table file offset
    ' e_shentsize at byte 46 (2 bytes): size of one section header entry
    ' e_shnum   at byte 48  (2 bytes): number of section headers
    ' e_shstrndx at byte 50 (2 bytes): index of section-name string table
    elf.rdN(0, 52, head$)
    shBase%   = elf.b4%(head$, 32)
    shESize%  = elf.b2%(head$, 46)
    shCount%  = elf.b2%(head$, 48)
    shStrIdx% = elf.b2%(head$, 50)

    ' Section-name string table: its sh_offset is at +16 in its own header
    elf.rdN(shBase% + shStrIdx% * shESize%, 40, sh$)
    strTabBase% = elf.b4%(sh$, 16)

    ' --- Locate required sections ---
    txtSect%   = -1
    symSect%   = -1
    strSect%   = -1
    rodSect%   = -1
    dinitSect% = -1
    FOR j% = 0 TO shCount% - 1
        elf.rdN(shBase% + j% * shESize%, 40, sh$)
        nameOfs% = elf.b4%(sh$, 0)
        sname$   = elf.rdStr$(strTabBase% + nameOfs%)
        SELECT CASE sname$
        CASE ".text"   : IF txtSect%   < 0 THEN txtSect%   = j%
        CASE ".symtab" : IF symSect%   < 0 THEN symSect%   = j%
        CASE ".strtab" : IF strSect%   < 0 THEN strSect%   = j%
        CASE ".rodata" : IF rodSect%   < 0 THEN rodSect%   = j%
        CASE ".dinit"  : IF dinitSect% < 0 THEN dinitSect% = j%
        END SELECT
    NEXT j%

    IF txtSect% < 0 THEN
        printError("ELF: .text section not found in " + filename$)
        CLOSE #3
	elf.error%=1
        EXIT SUB
    END IF
    IF symSect% < 0 OR strSect% < 0 THEN
        printError("ELF: symbol/string table not found in " + filename$)
        CLOSE #3
	elf.error%=1
        EXIT SUB
    END IF

    ' --- Read .text header ---
    elf.rdN(shBase% + txtSect% * shESize%, 40, sh$)
    elf.textBase%    = elf.b4%(sh$, 12)
    elf.textFileOfs% = elf.b4%(sh$, 16)
    elf.textSize%    = elf.b4%(sh$, 20)

    ' --- Read .rodata header (optional) ---
    rodBase% = 0
    IF rodSect% >= 0 THEN
        elf.rdN(shBase% + rodSect% * shESize%, 40, sh$)
        rodBase%           = elf.b4%(sh$, 12)
        elf.rodataFileOfs% = elf.b4%(sh$, 16)
        elf.rodataSize%    = elf.b4%(sh$, 20)
    END IF

    ' --- Check if .dinit sits between .text and .rodata (needs zero padding) ---
    IF dinitSect% >= 0 AND elf.rodataSize% > 0 THEN
        elf.rdN(shBase% + dinitSect% * shESize%, 40, sh$)
        dinitBase% = elf.b4%(sh$, 12)
        dinitSize% = elf.b4%(sh$, 20)
        IF dinitBase% > elf.textBase% AND dinitBase% < rodBase% THEN
            elf.dinitSize% = dinitSize%
        END IF
    END IF

    ' --- Scan symbol table for global functions (st_info = 0x12) ---
    ' Skip names starting with '_' (compiler-generated helpers)
    elf.rdN(shBase% + symSect% * shESize%, 40, sh$)
    symBase%    = elf.b4%(sh$, 16)
    symTabSize% = elf.b4%(sh$, 20)
    symCount%   = symTabSize% / 16

    elf.rdN(shBase% + strSect% * shESize%, 40, sh$)
    symStrBase% = elf.b4%(sh$, 16)

    FOR i% = 0 TO symCount% - 1
        elf.rdN(symBase% + i% * 16, 16, sym16$)
        sinfo% = elf.b1%(sym16$, 12)
        IF sinfo% = &H12 THEN
            nameOfs% = elf.b4%(sym16$, 0)
            vaddr%   = elf.b4%(sym16$, 4)
            fname$   = elf.rdStr$(symStrBase% + nameOfs%)
            IF LEFT$(fname$, 1) <> "_" AND elf.funcCount% < MAX_ELF_FUNCS THEN
                elf.funcAddr%(elf.funcCount%) = vaddr%
                elf.funcName$(elf.funcCount%) = fname$
                elf.funcCount% = elf.funcCount% + 1
            END IF
        END IF
    NEXT i%

    ' --- Sort functions by address (insertion sort) ---
    FOR i% = 1 TO elf.funcCount% - 1
        tAddr% = elf.funcAddr%(i%)
        tName$ = elf.funcName$(i%)
        j% = i% - 1
        DO WHILE j% >= 0 AND elf.funcAddr%(j%) > tAddr%
            elf.funcAddr%(j% + 1) = elf.funcAddr%(j%)
            elf.funcName$(j% + 1) = elf.funcName$(j%)
            j% = j% - 1
        LOOP
        elf.funcAddr%(j% + 1) = tAddr%
        elf.funcName$(j% + 1) = tName$
    NEXT i%

    ' --- Compute function sizes from sorted address gaps ---
    ' ARM Thumb: symbol addresses have bit 0 set; mask it off for arithmetic.
    textEnd% = elf.textBase% + elf.textSize%
    FOR i% = 0 TO elf.funcCount% - 1
        IF i% < elf.funcCount% - 1 THEN
            elf.funcSize%(i%) = (elf.funcAddr%(i% + 1) AND &HFFFFFFFE) - (elf.funcAddr%(i%) AND &HFFFFFFFE)
        ELSE
            elf.funcSize%(i%) = textEnd% - (elf.funcAddr%(i%) AND &HFFFFFFFE)
        END IF
        IF elf.funcName$(i%) = "main" THEN elf.mainIdx% = i%
    NEXT i%
    IF elf.mainIdx% < 0 AND elf.funcCount% > 0 THEN elf.mainIdx% = 0

    PRINT "-> ELF " + filename$ + ": " + STR$(elf.funcCount%) + " functions, " + STR$(elf.textSize%) + " bytes .text"
    IF elf.rodataSize% > 0 THEN PRINT "         .rodata " + STR$(elf.rodataSize%) + " bytes"
    IF elf.dinitSize% > 0 THEN PRINT "         .dinit pad " + STR$(elf.dinitSize%) + " bytes"
END SUB

' Close the ELF file opened by readELFOpen.
SUB readELFClose()
    IF elf.error%=0 THEN CLOSE #3
END SUB

' Merge mode: load .text (+ optional .dinit zero padding + .rodata)
' into io.buffer%() as halfwords.
' Sets io.entry% = word-offset of main/first function from .text start.
' Sets io.base% = .text virtual base address.
' File #3 must be open (call readELFOpen first).
SUB elf.loadAll()
    LOCAL i%, hw%
    LOCAL c1$ LENGTH 1, c2$ LENGTH 1

    io.ptr%   = 0
    io.base%  = elf.textBase%
    io.entry% = 0
    IF elf.mainIdx% >= 0 THEN
        io.entry% = ((elf.funcAddr%(elf.mainIdx%) AND &HFFFFFFFE) - elf.textBase%) / 4
    END IF

    ' --- .text section ---
    SEEK #3, elf.textFileOfs% + 1
    i% = 0
    DO WHILE i% < elf.textSize%
        c1$ = INPUT$(1, #3)
        IF i% + 1 < elf.textSize% THEN
            c2$ = INPUT$(1, #3)
        ELSE
            c2$ = CHR$(0)
        END IF
        IF io.ptr% >= MAX_BUFFER THEN printError("ELF .text exceeds output buffer") : EXIT SUB
        io.buffer%(io.ptr%) = ASC(c1$) OR (ASC(c2$) << 8)
        INC io.ptr%
        INC i%,2
    LOOP

    ' --- .dinit zero padding (bridges address gap to .rodata) ---
    i% = 0
    DO WHILE i% < elf.dinitSize%
        IF io.ptr% >= MAX_BUFFER THEN printError("ELF .dinit pad exceeds output buffer") : EXIT SUB
        io.buffer%(io.ptr%) = 0
        INC io.ptr%
        INC i%,2
    LOOP

    ' --- .rodata section ---
    IF elf.rodataSize% > 0 THEN
        SEEK #3, elf.rodataFileOfs% + 1
        i% = 0
        DO WHILE i% < elf.rodataSize%
            c1$ = INPUT$(1, #3)
            IF i% + 1 < elf.rodataSize% THEN
                c2$ = INPUT$(1, #3)
            ELSE
                c2$ = CHR$(0)
            END IF
            IF io.ptr% >= MAX_BUFFER THEN printError("ELF .rodata exceeds output buffer") : EXIT SUB
            io.buffer%(io.ptr%) = ASC(c1$) OR (ASC(c2$) << 8)
            INC io.ptr%
            INC i%,2
        LOOP
    END IF

    PRINT "   entry word: " + RIGHT$("00000000" + HEX$(io.entry% AND &HFFFFFFFF), 8) + "  " + STR$(io.ptr% * 2) + " bytes loaded"
END SUB

' Join mode: load one function's code into io.buffer%().
' Sets io.entry% = 0 (function is at offset 0 of its own buffer).
' Sets io.base%  = 0.
' File #3 must be open (call readELFOpen first).
' @param  n%  0-based function index (0 .. elf.funcCount%-1)
SUB elf.loadFunc(n%)
    LOCAL fileOfs%, byteSize%, i%
    LOCAL c1$ LENGTH 1, c2$ LENGTH 1

    IF n% < 0 OR n% >= elf.funcCount% THEN
        printError("ELF: function index out of range: " + STR$(n%))
        EXIT SUB
    END IF

    io.ptr%   = 0
    io.base%  = 0
    io.entry% = 0

    fileOfs%  = elf.textFileOfs% + ((elf.funcAddr%(n%) AND &HFFFFFFFE) - elf.textBase%)
    byteSize% = elf.funcSize%(n%)

    SEEK #3, fileOfs% + 1
    i% = 0
    DO WHILE i% < byteSize%
        c1$ = INPUT$(1, #3)
        IF i% + 1 < byteSize% THEN
            c2$ = INPUT$(1, #3)
        ELSE
            c2$ = CHR$(0)
        END IF
        IF io.ptr% >= MAX_BUFFER THEN printError("ELF function exceeds output buffer") : EXIT SUB
        io.buffer%(io.ptr%) = ASC(c1$) OR (ASC(c2$) << 8)
        INC io.ptr%
        INC i%,2
    LOOP
END SUB

