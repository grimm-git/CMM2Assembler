' ============================================================
' lexer.inc  -  Character-level source line scanner
' Phase 1.
' ============================================================

CONST MAX_SRC_LEN  = 128     ' max source line length

DIM lex.line$ LENGTH MAX_SRC_LEN
DIM lex.pos%  = 1

' Initialise scanner with a new source line.
' Trailing whitespace (spaces, tabs, CR) is stripped before storing — it
' never carries meaning in asm and would otherwise overflow lex.line$
' on padded source lines.  If the trimmed line still exceeds MAX_SRC_LEN,
' a proper error is raised instead of crashing the string assignment.
SUB lexer.init(src$)
  LOCAL n%, c$ LENGTH 1
  n% = LEN(src$)
  DO WHILE n% > 0
    c$ = MID$(src$, n%, 1)
    IF c$ <> " " AND c$ <> CHR$(9) AND c$ <> CHR$(13) THEN EXIT DO
    INC n%, -1
  LOOP
  IF n% > MAX_SRC_LEN THEN
    showError("source line exceeds " + STR$(MAX_SRC_LEN) + " chars")
    lex.line$ = LEFT$(src$, MAX_SRC_LEN)
  ELSE
    lex.line$ = LEFT$(src$, n%)
  END IF
  lex.pos% = 1
END SUB

' Advance past spaces and tabs.
SUB lexer.skipWS()
  LOCAL c$ LENGTH 1
  DO WHILE lex.pos% <= LEN(lex.line$)
    c$ = MID$(lex.line$, lex.pos%, 1)
    IF c$ = " " OR c$ = CHR$(9) THEN
      lex.pos% = lex.pos% + 1
    ELSE
      EXIT DO
    END IF
  LOOP
END SUB

' Return 1 if nothing meaningful remains (end of string or comment).
FUNCTION lexer.atEnd%()
  LOCAL c$ LENGTH 1
  IF lex.pos% > LEN(lex.line$) THEN lexer.atEnd% = 1 : EXIT FUNCTION
  c$ = MID$(lex.line$, lex.pos%, 1)
  IF c$ = ";" OR c$ = "@" THEN lexer.atEnd% = 1 : EXIT FUNCTION
  IF c$ = "/" AND lex.pos% + 1 <= LEN(lex.line$) THEN
    IF MID$(lex.line$, lex.pos% + 1, 1) = "/" THEN
      lexer.atEnd% = 1 : EXIT FUNCTION
    END IF
  END IF
  lexer.atEnd% = 0
END FUNCTION

' Return current character without advancing, or "" at end/comment.
FUNCTION lexer.peek$()
  IF lexer.atEnd%() THEN lexer.peek$ = "" : EXIT FUNCTION
  lexer.peek$ = MID$(lex.line$, lex.pos%, 1)
END FUNCTION

' Return current character and advance, or "" at end/comment.
FUNCTION lexer.get$()
  IF lexer.atEnd%() THEN lexer.get$ = "" : EXIT FUNCTION
  lexer.get$ = MID$(lex.line$, lex.pos%, 1)
  lex.pos% = lex.pos% + 1
END FUNCTION

' Read an alphanumeric+underscore word; return it in original case.
' Caller is responsible for UCASE$() before keyword/mnemonic comparisons.
' Does NOT consume dots — caller handles "." prefixes and ".N"/".W" suffixes.
FUNCTION lexer.readWord$()
  LOCAL result$ LENGTH MAX_NAME_LEN
  LOCAL c$      LENGTH 1
  result$ = ""
  DO WHILE lex.pos% <= LEN(lex.line$)
    c$ = MID$(lex.line$, lex.pos%, 1)
    IF (c$ >= "A" AND c$ <= "Z") OR (c$ >= "a" AND c$ <= "z") OR (c$ >= "0" AND c$ <= "9") OR c$ = "_" THEN
      result$ = result$ + c$
      lex.pos% = lex.pos% + 1
    ELSE
      EXIT DO
    END IF
  LOOP
  lexer.readWord$ = result$
END FUNCTION

' Read an integer literal at the current position.
' Handles: #-prefix, unary minus, 0x.../&H... hex, decimal.
' Sets ok% = 1 on success, 0 on failure (position left at first unrecognised char).
FUNCTION lexer.readNum%(ok%)
  LOCAL result%, neg%, startPos%
  LOCAL c$   LENGTH 1
  LOCAL hex$ LENGTH 16
  ok%      = 0
  neg%     = 0
  startPos% = lex.pos%

  IF lex.pos% <= LEN(lex.line$) THEN
    IF MID$(lex.line$, lex.pos%, 1) = "#" THEN lex.pos% = lex.pos% + 1
  END IF

  IF lex.pos% <= LEN(lex.line$) THEN
    IF MID$(lex.line$, lex.pos%, 1) = "-" THEN
      neg% = 1
      lex.pos% = lex.pos% + 1
    ELSEIF MID$(lex.line$, lex.pos%, 1) = "+" THEN
      lex.pos% = lex.pos% + 1
    END IF
  END IF

  IF lex.pos% > LEN(lex.line$) THEN lex.pos% = startPos% : EXIT FUNCTION

  c$ = MID$(lex.line$, lex.pos%, 1)

  ' &H hex
  IF c$ = "&" AND lex.pos% + 1 <= LEN(lex.line$) THEN
    IF UCASE$(MID$(lex.line$, lex.pos% + 1, 1)) = "H" THEN
      lex.pos% = lex.pos% + 2
      hex$ = ""
      DO WHILE lex.pos% <= LEN(lex.line$)
        c$ = UCASE$(MID$(lex.line$, lex.pos%, 1))
        IF (c$ >= "0" AND c$ <= "9") OR (c$ >= "A" AND c$ <= "F") THEN
          hex$ = hex$ + c$
          lex.pos% = lex.pos% + 1
        ELSE
          EXIT DO
        END IF
      LOOP
      IF LEN(hex$) = 0 THEN lex.pos% = startPos% : EXIT FUNCTION
      result% = VAL("&H" + hex$)
      IF neg% THEN result% = -result%
      lexer.readNum% = result% AND &HFFFFFFFF
      ok% = 1
      EXIT FUNCTION
    END IF
  END IF

  ' 0x hex
  IF c$ = "0" AND lex.pos% + 1 <= LEN(lex.line$) THEN
    IF UCASE$(MID$(lex.line$, lex.pos% + 1, 1)) = "X" THEN
      lex.pos% = lex.pos% + 2
      hex$ = ""
      DO WHILE lex.pos% <= LEN(lex.line$)
        c$ = UCASE$(MID$(lex.line$, lex.pos%, 1))
        IF (c$ >= "0" AND c$ <= "9") OR (c$ >= "A" AND c$ <= "F") THEN
          hex$ = hex$ + c$
          lex.pos% = lex.pos% + 1
        ELSE
          EXIT DO
        END IF
      LOOP
      IF LEN(hex$) = 0 THEN lex.pos% = startPos% : EXIT FUNCTION
      result% = VAL("&H" + hex$)
      IF neg% THEN result% = -result%
      lexer.readNum% = result% AND &HFFFFFFFF
      ok% = 1
      EXIT FUNCTION
    END IF
  END IF

  ' Decimal
  IF c$ >= "0" AND c$ <= "9" THEN
    result% = 0
    DO WHILE lex.pos% <= LEN(lex.line$)
      c$ = MID$(lex.line$, lex.pos%, 1)
      IF c$ >= "0" AND c$ <= "9" THEN
        result% = result% * 10 + VAL(c$)
        lex.pos% = lex.pos% + 1
      ELSE
        EXIT DO
      END IF
    LOOP
    IF neg% THEN result% = -result%
    lexer.readNum% = result% AND &HFFFFFFFF
    ok% = 1
    EXIT FUNCTION
  END IF

  ' No numeric token found
  lex.pos% = startPos%
END FUNCTION
