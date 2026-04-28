' directives.inc  -  Assembler directive handlers
'
' Entry point: processDir(dir$, line$)
'   dir$  = directive keyword in lower case (e.g. "equ", "word")
'   line$ = full source line (for error messages)
' The lexer (lex.line$, lex.pos%) is already positioned just
' after the directive keyword when processDir is called.


' Byte packing: dir.pendByte% holds a pending unpaired byte
' (-1 = none).  directives.flushByte() pads it to a halfword.
' Reset dir.pendByte% = -1 in resetState() between files.
DIM dir.pendByte% = -1

' Flush any pending unpaired byte as a zero-padded halfword.
SUB directives.flushByte()
    IF dir.pendByte% >= 0 THEN
	emit16(dir.pendByte% AND &HFF)
	dir.pendByte% = -1
    END IF
END SUB

' Emit one byte; pairs are packed into a single halfword (little-endian).
SUB directives.emitByte(b%)
    IF dir.pendByte% >= 0 THEN
	emit16((dir.pendByte% AND &HFF) OR ((b% AND &HFF) << 8))
	dir.pendByte% = -1
    ELSE
	dir.pendByte% = b% AND &HFF
    END IF
END SUB

' Skip optional whitespace and one comma.
SUB directives.skipComma()
    LOCAL dummy$ LENGTH 1

    lexer.skipWS()
    IF lexer.peek$() = "," THEN dummy$ = lexer.get$()
END SUB

' Read a double-quoted string from the current lex position and emit its
' bytes.  Uses direct lex.line$/lex.pos% access so that "//" inside a
' string is NOT treated as a comment.  Appends one NUL byte when nul%=1.
SUB directives.emitString(nul%)
    LOCAL c$ LENGTH 1

    lexer.skipWS()
    IF MID$(lex.line$, lex.pos%, 1) <> CHR$(34) THEN
	showError "Expected string literal"
	EXIT SUB
    END IF
    lex.pos% = lex.pos% + 1   ' consume opening "
    DO
	IF lex.pos% > LEN(lex.line$) THEN showError "Unterminated string" : EXIT DO
	c$ = MID$(lex.line$, lex.pos%, 1)
	lex.pos% = lex.pos% + 1
	IF c$ = CHR$(34) THEN EXIT DO   ' closing "
	IF c$ = "\" THEN
	    IF lex.pos% > LEN(lex.line$) THEN showError "Truncated escape" : EXIT DO
	    c$ = MID$(lex.line$, lex.pos%, 1)
	    lex.pos% = lex.pos% + 1
	    SELECT CASE c$
		CASE "n"      : directives.emitByte(10)
		CASE "r"      : directives.emitByte(13)
		CASE "t"      : directives.emitByte(9)
		CASE "0"      : directives.emitByte(0)
		CASE "\"      : directives.emitByte(92)
		CASE CHR$(34) : directives.emitByte(34)
		CASE ELSE     : directives.emitByte(ASC(c$))
	    END SELECT
	ELSE
	    directives.emitByte(ASC(c$))
        END IF
    LOOP
    IF nul% THEN directives.emitByte(0)
END SUB

' Dispatch an assembler directive.
SUB processDir(dir$, line$)
    LOCAL val%, ok%, i%, n%
    LOCAL boundary%, byteAddr%, newByteAddr%, padHalfwords%
    LOCAL name$  LENGTH MAX_NAME_LEN
    LOCAL ref$   LENGTH MAX_NAME_LEN
    LOCAL dummy$ LENGTH 1
    LOCAL incName$ LENGTH 128
    LOCAL ch$ LENGTH 1

    SELECT CASE dir$

    ' ---- Data emission ----

    CASE "word", "4byte", "long"
      directives.flushByte()
      DO
        lexer.skipWS()
        IF lexer.atEnd%() THEN EXIT DO
        ok% = 0
        val% = lexer.readNum%(ok%)
        IF ok% = 0 THEN
          ' Not a number — try a symbol reference (e.g. a .equ name)
          ref$ = lexer.readWord$()
          IF LEN(ref$) = 0 THEN showError ".word: bad value in: " + line$ : EXIT DO
          val% = lookupSym%(ref$)
        END IF
        emit16(val% AND &HFFFF)                 ' low halfword first (little-endian)
        emit16((val% >> 16) AND &HFFFF)          ' high halfword
        directives.skipComma()
      LOOP

    CASE "hword", "short", "2byte"
      directives.flushByte()
      DO
        lexer.skipWS()
        IF lexer.atEnd%() THEN EXIT DO
        ok% = 0
        val% = lexer.readNum%(ok%)
        IF ok% = 0 THEN showError ".hword: bad value in: " + line$ : EXIT DO
        emit16(val% AND &HFFFF)
        directives.skipComma()
      LOOP

    CASE "byte"
      DO
        lexer.skipWS()
        IF lexer.atEnd%() THEN EXIT DO
        ok% = 0
        val% = lexer.readNum%(ok%)
        IF ok% = 0 THEN showError ".byte: bad value in: " + line$ : EXIT DO
        directives.emitByte(val% AND &HFF)
        directives.skipComma()
      LOOP
      directives.flushByte()

    CASE "space", "skip", "zero"
      directives.flushByte()
      lexer.skipWS()
      ok% = 0
      n% = lexer.readNum%(ok%)
      IF ok% = 0 THEN showError ".space: bad size in: " + line$ : EXIT SUB
      ' optional fill byte — only zero fill is supported
      FOR i% = 1 TO (n% >> 1)
        emit16(0)
      NEXT i%
      IF (n% AND 1) THEN emit16(0)   ' odd byte count: pad last halfword

    CASE "ascii"
      directives.emitString(0)
      directives.flushByte()

    CASE "asciz", "string"
      directives.emitString(1)
      directives.flushByte()

    ' ---- Symbol definition ----

    CASE "equ", "set"
      lexer.skipWS()
      name$ = lexer.readWord$()
      IF LEN(name$) = 0 THEN showError ".equ: missing name in: " + line$ : EXIT SUB
      directives.skipComma()
      lexer.skipWS()
      ok% = 0
      val% = lexer.readNum%(ok%)
      IF ok% = 0 THEN
        ' value is a symbol reference
        ref$ = lexer.readWord$()
        IF LEN(ref$) = 0 THEN showError ".equ: missing value in: " + line$ : EXIT SUB
        val% = lookupSym%(ref$)
      END IF
      defineSym(name$, val%)

    ' ---- Alignment ----

    CASE "align", "p2align"
      ' .align n  →  align to 2^n byte boundary; pad with NOP (code section).
      directives.flushByte()
      lexer.skipWS()
      ok% = 0
      n% = lexer.readNum%(ok%)
      IF ok% = 0 THEN n% = 0
      IF n% = 0 THEN EXIT SUB
      boundary%    = 1 << n%
      byteAddr%    = io.base% + io.ptr% * 2
      newByteAddr% = (byteAddr% + boundary% - 1) AND (-boundary%)
      padHalfwords% = (newByteAddr% - byteAddr%) >> 1
      FOR i% = 1 TO padHalfwords%
        emit16(&HBF00)
      NEXT i%

    CASE "balign"
      ' .balign n  →  align to n bytes; pad with NOP (code section).
      directives.flushByte()
      lexer.skipWS()
      ok% = 0
      n% = lexer.readNum%(ok%)
      IF ok% = 0 OR n% <= 1 THEN EXIT SUB
      boundary%    = n%
      byteAddr%    = io.base% + io.ptr% * 2
      newByteAddr% = (byteAddr% + boundary% - 1) AND (-boundary%)
      padHalfwords% = (newByteAddr% - byteAddr%) >> 1
      FOR i% = 1 TO padHalfwords%
        emit16(&HBF00)
      NEXT i%

    ' ---- Base address ----

    CASE "base"
      lexer.skipWS()
      ok% = 0
      val% = lexer.readNum%(ok%)
      IF ok% = 0 THEN showError ".base: bad address in: " + line$ : EXIT SUB
      io.base% = val%

    ' ---- Source inclusion ----

    CASE "include"
      lexer.skipWS()
      IF MID$(lex.line$, lex.pos%, 1) <> CHR$(34) THEN
        showError ".include: expected quoted filename in: " + line$
        EXIT SUB
      END IF
      lex.pos% = lex.pos% + 1   ' consume opening "
      incName$ = ""
      DO
        IF lex.pos% > LEN(lex.line$) THEN
          showError ".include: unterminated string in: " + line$
          EXIT SUB
        END IF
        ch$ = MID$(lex.line$, lex.pos%, 1)
        lex.pos% = lex.pos% + 1
        IF ch$ = CHR$(34) THEN EXIT DO
        incName$ = incName$ + ch$
      LOOP
      IF LEN(incName$) = 0 THEN
        showError ".include: empty filename in: " + line$
        EXIT SUB
      END IF
      as.includeFile(incName$)

    ' ---- Informational — no output ----

    CASE "syntax", "cpu", "fpu", "arch", "thumb", "arm"
    CASE "text", "data", "bss", "section", "pushsection", "popsection"
    CASE "global", "globl", "local", "weak", "hidden", "protected"
    CASE "type", "size", "file", "ident"
    CASE "thumb_func", "thumb_set", "code", "code16", "code32"
    CASE "ltorg"   ' literal pool flush hint — handled by literals.inc in Phase 5
    CASE "end"

    CASE ELSE
      showWarn "Unknown directive ignored: ." + dir$

  END SELECT
END SUB

