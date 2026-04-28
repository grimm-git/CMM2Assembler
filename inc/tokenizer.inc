' ============================================================
' tokenizer.inc  -  Pass 1: tokenize one source line
' Phase 1.
'
' Entry point: tokenizeLine(src$)
' Fills tok.type%(), tok.val%(), tok.str$(), tok.count%.
' Labels are forwarded to defineLbl(); directives emit a single
' TT_DIR token and return (directives.inc handles the rest).
' ============================================================

CONST MAX_TOKENS   = 16      ' max tokens per source line

DIM tok.type%(MAX_TOKENS)
DIM tok.val%(MAX_TOKENS)
DIM tok.str$(MAX_TOKENS) LENGTH MAX_NAME_LEN
DIM tok.count% = 0

' Map a register name (already upper-cased) to 0..15, or -1 if not a register.
FUNCTION tokenizer.parseRegName%(name$)
  LOCAL i%, n%
  LOCAL c$     LENGTH 1
  LOCAL uName$ LENGTH MAX_NAME_LEN
  uName$ = UCASE$(name$)
  IF uName$ = "SP" THEN tokenizer.parseRegName% = 13 : EXIT FUNCTION
  IF uName$ = "LR" THEN tokenizer.parseRegName% = 14 : EXIT FUNCTION
  IF uName$ = "PC" THEN tokenizer.parseRegName% = 15 : EXIT FUNCTION
  IF LEFT$(uName$, 1) <> "R" THEN tokenizer.parseRegName% = -1 : EXIT FUNCTION
  IF LEN(uName$) < 2 OR LEN(uName$) > 3 THEN tokenizer.parseRegName% = -1 : EXIT FUNCTION
  FOR i% = 2 TO LEN(uName$)
    c$ = MID$(uName$, i%, 1)
    IF c$ < "0" OR c$ > "9" THEN tokenizer.parseRegName% = -1 : EXIT FUNCTION
  NEXT i%
  n% = VAL(MID$(uName$, 2))
  IF n% < 0 OR n% > 15 THEN tokenizer.parseRegName% = -1 : EXIT FUNCTION
  tokenizer.parseRegName% = n%
END FUNCTION

' Read a register-list literal {Rx,Ry,...} or {Rx-Ry,...} from the current
' lex position (which must be pointing at '{').  Returns bitmask of registers.
FUNCTION tokenizer.parseRegList%()
  LOCAL bitmask%, r1%, r2%, i%
  LOCAL c$    LENGTH 1
  LOCAL word$ LENGTH MAX_NAME_LEN
  bitmask% = 0
  lex.pos% = lex.pos% + 1   ' consume '{'
  DO
    lexer.skipWS()
    IF lexer.atEnd%() THEN showError "Unclosed register list" : EXIT DO
    c$ = lexer.peek$()
    IF c$ = "}" THEN lex.pos% = lex.pos% + 1 : EXIT DO
    IF c$ = "," THEN lex.pos% = lex.pos% + 1 : CONTINUE DO
    word$ = lexer.readWord$()
    r1% = tokenizer.parseRegName%(word$)
    IF r1% < 0 THEN showError "Expected register in list: " + word$ : EXIT DO
    lexer.skipWS()
    IF lexer.peek$() = "-" THEN
      lex.pos% = lex.pos% + 1
      lexer.skipWS()
      word$ = lexer.readWord$()
      r2% = tokenizer.parseRegName%(word$)
      IF r2% < 0 OR r2% < r1% THEN showError "Bad register range in list" : EXIT DO
      FOR i% = r1% TO r2%
        bitmask% = bitmask% OR (1 << i%)
      NEXT i%
    ELSE
      bitmask% = bitmask% OR (1 << r1%)
    END IF
  LOOP
  tokenizer.parseRegList% = bitmask%
END FUNCTION

' Append one token to the tok.* arrays.
SUB tokenizer.emitTok(tt%, tv%, ts$)
  IF tok.count% >= MAX_TOKENS THEN showError "Too many tokens" : EXIT SUB
  tok.type%(tok.count%) = tt%
  tok.val%(tok.count%)  = tv%
  tok.str$(tok.count%)  = ts$
  tok.count% = tok.count% + 1
END SUB

' Parse all operands from the current lex position until end of line.
SUB tokenizer.parseOperands()
  LOCAL reg%, ok%, val%, sh%
  LOCAL c$    LENGTH 1
  LOCAL word$ LENGTH MAX_NAME_LEN
  DO
    lexer.skipWS()
    IF lexer.atEnd%() THEN EXIT DO
    c$ = lexer.peek$()

    SELECT CASE c$

      CASE ","
        c$ = lexer.get$()
        tokenizer.emitTok(TT_COMMA, 0, "")

      CASE "["
        c$ = lexer.get$()
        tokenizer.emitTok(TT_LBRA, 0, "")

      CASE "]"
        c$ = lexer.get$()
        tokenizer.emitTok(TT_RBRA, 0, "")

      CASE "!"
        c$ = lexer.get$()
        tokenizer.emitTok(TT_BANG, 0, "")

      CASE "{"
        tokenizer.emitTok(TT_RLIST, tokenizer.parseRegList%(), "")

      CASE "="
        ' LDR =value  or  LDR =symbol  or  LDR =#symbol
        c$ = lexer.get$()   ' consume '='
        lexer.skipWS()
        c$ = lexer.peek$()
        IF c$ = "#" OR c$ = "&" OR (c$ >= "0" AND c$ <= "9") OR c$ = "-" OR c$ = "+" THEN
          ok% = 0
          val% = lexer.readNum%(ok%)
          IF ok% THEN
            tokenizer.emitTok(TT_LIT, val%, "")
          ELSE
            ' lexer.readNum% reset position; skip leading # if present, then read symbol name
            IF lexer.peek$() = "#" THEN c$ = lexer.get$()
            word$ = lexer.readWord$()
            IF LEN(word$) = 0 THEN showError "Expected value after =" : EXIT DO
            tokenizer.emitTok(TT_LIT, lookupSym%(word$), word$)
          END IF
        ELSE
          word$ = lexer.readWord$()
          IF LEN(word$) = 0 THEN showError "Expected value after =" : EXIT DO
          tokenizer.emitTok(TT_LIT, lookupSym%(word$), word$)
        END IF

      CASE "#"
        ok% = 0
        val% = lexer.readNum%(ok%)
        IF ok% THEN
          tokenizer.emitTok(TT_IMM, val%, "")
        ELSE
          ' lexer.readNum% reset position on failure; skip the '#' and read symbol
          IF lexer.peek$() = "#" THEN c$ = lexer.get$()
          word$ = lexer.readWord$()
          IF LEN(word$) = 0 THEN showError "Bad immediate value" : EXIT DO
          tokenizer.emitTok(TT_IMM, lookupSym%(word$), word$)
        END IF

      CASE ELSE
        word$ = lexer.readWord$()
        IF LEN(word$) = 0 THEN
          showError "Unexpected character: " + c$
          lex.pos% = lex.pos% + 1   ' skip to avoid infinite loop
          EXIT DO
        END IF
        reg% = tokenizer.parseRegName%(word$)
        IF reg% >= 0 THEN
          IF reg% <= 7 THEN
            tokenizer.emitTok(TT_REGLO, reg%, word$)
          ELSE
            tokenizer.emitTok(TT_REGHI, reg%, word$)
          END IF
        ELSE
          sh% = lookupShift%(UCASE$(word$))
          IF sh% <> SHIFT_NONE THEN
            tokenizer.emitTok(TT_SHIFT, sh%, word$)
          ELSE
            tokenizer.emitTok(TT_ADDR, 0, word$)
          END IF
        END IF

    END SELECT
  LOOP
END SUB

' Tokenize one source line into tok.type%(), tok.val%(), tok.str$(), tok.count%.
SUB tokenizeLine(src$)
  ' Numeric locals
  LOCAL mlen%, idx%, fl%, cc%, sz%, savedPos%
  ' String locals — MMBasic LOCAL strings must have explicit LENGTH.
  ' NOTE: "rem$" cannot be used as a variable — MMBasic treats "REM" as the
  '       comment keyword, so any line starting with rem$ is a no-op comment.
  '       Use "sfx$" (suffix) instead.
  LOCAL word$    LENGTH MAX_NAME_LEN  ' word read from source (mnemonic or label)
  LOCAL uWord$   LENGTH MAX_NAME_LEN  ' upper-case version of word$
  LOCAL sfx$     LENGTH 8             ' suffix after mnemonic match (e.g. "SGE", "TTEEQ")
  LOCAL maskStr$ LENGTH 4    ' IT T/E mask chars (e.g. "TTE")
  LOCAL c$       LENGTH 1    ' single-character scratch
  LOCAL eff$     LENGTH 2    ' CPS effect suffix
  LOCAL base$    LENGTH 8    ' mnemonic base for 1-char flag check

  tok.count% = 0
  lexer.init(src$)
  lexer.skipWS()
  IF lexer.atEnd%() THEN EXIT SUB

  ' --- Label detection: word immediately followed by ':' ---
  ' word$ kept in original case so the label name is stored as written.
  savedPos% = lex.pos%
  word$ = lexer.readWord$()
  IF LEN(word$) > 0 AND lexer.peek$() = ":" THEN
    lex.pos% = lex.pos% + 1   ' consume ':'
    defineLbl(word$, io.base% + io.ptr% * 2)
    lexer.skipWS()
    IF lexer.atEnd%() THEN EXIT SUB
  ELSE
    lex.pos% = savedPos%      ' not a label — rewind
  END IF

  ' --- Directive: starts with '.' ---
  IF lexer.peek$() = "." THEN
    lex.pos% = lex.pos% + 1   ' consume '.'
    word$ = lexer.readWord$()
    IF LEN(word$) = 0 THEN showError "Empty directive name" : EXIT SUB
    tokenizer.emitTok(TT_DIR, 0, LCASE$(word$))
    EXIT SUB   ' directives.inc processes the rest of the line
  END IF

  ' --- Mnemonic ---
  ' word$ = original case from source; uWord$ = uppercase for all lookups.
  word$ = lexer.readWord$()
  IF LEN(word$) = 0 THEN EXIT SUB
  uWord$ = UCASE$(word$)

  ' Longest-match: try 7 chars down to 1
  idx% = 0
  FOR mlen% = 7 TO 1 STEP -1
    IF mlen% <= LEN(uWord$) THEN
      idx% = lookupMne%(LEFT$(uWord$, mlen%))
      IF idx% > 0 THEN EXIT FOR
    END IF
  NEXT mlen%

  IF idx% = 0 THEN
    showError "Unknown mnemonic: " + word$
    EXIT SUB
  END IF

  ' Disambiguate BL vs B<cond>: longest-match prefers "BL" over "B", so
  ' BLT/BLE/BLS would be tokenised as BL+suffix. If the suffix starts with
  ' a real condition code (LS/LT/LE), the user meant B<cond>, so back off.
  IF LEFT$(uWord$, mlen%) = "BL" AND LEN(uWord$) >= 3 THEN
    IF lookupCond%(MID$(uWord$, 2, 2)) <> COND_NONE THEN
      idx%  = lookupMne%("B")
      mlen% = 1
    END IF
  END IF

  tokenizer.emitTok(TT_MNE, idx%, LEFT$(uWord$, mlen%))

  ' --- Mnemonic suffix: [S] [CC] [.N|.W] (all comparisons on uWord$) ---
  sfx$ = MID$(uWord$, mlen% + 1)

  ' S-flag: no condition starts with S, so first char S is unambiguously the flag
  fl% = FLAG_NONE
  IF LEN(sfx$) > 0 AND LEFT$(sfx$, 1) = "S" THEN
    fl% = FLAG_S
    sfx$ = MID$(sfx$, 2)
  END IF

  ' IT-block T/E mask chars: present only for mnemonic "IT"
  ' Strip any leading T/E chars so the condition code check below succeeds.
  ' The full IT variant (ITT, ITTE, …) is reconstructed in the MNE token str.
  IF LEFT$(uWord$, mlen%) = "IT" THEN
    maskStr$ = ""
    DO WHILE LEN(sfx$) > 0
      c$ = LEFT$(sfx$, 1)
      IF c$ = "T" OR c$ = "E" THEN
        maskStr$ = maskStr$ + c$
        sfx$ = MID$(sfx$, 2)
      ELSE
        EXIT DO
      END IF
    LOOP
    ' Update the MNE token str to reflect the full variant, e.g. "ITT"
    tok.str$(0) = "IT" + maskStr$
    ' IT condition code is the NEXT word in the source line (standalone operand,
    ' not part of the mnemonic suffix).  Parse it here so it becomes TT_COND.
    lexer.skipWS()
    word$ = lexer.readWord$()
    cc% = lookupCond%(UCASE$(word$))
    IF cc% = COND_NONE THEN showError "IT: expected condition code" : EXIT SUB
    tokenizer.emitTok(TT_COND, cc%, "")
    EXIT SUB   ' no further operands for IT instructions
  END IF

  ' CPS effect suffix: CPSIE / CPSID -- emit "IE"/"ID" as TT_ADDR for the encoder.
  IF LEFT$(uWord$, mlen%) = "CPS" AND LEN(sfx$) >= 2 THEN
    eff$ = LEFT$(sfx$, 2)
    IF eff$ = "IE" OR eff$ = "ID" THEN
      tokenizer.emitTok(TT_ADDR, 0, eff$)
      sfx$ = MID$(sfx$, 3)
    END IF
  END IF

  ' Condition code: 2-char (for non-IT mnemonics)
  cc% = COND_NONE
  IF LEN(sfx$) >= 2 THEN
    cc% = lookupCond%(LEFT$(sfx$, 2))
    IF cc% <> COND_NONE THEN sfx$ = MID$(sfx$, 3)
  END IF

  ' Size qualifier: .N or .W — may be in sfx$ or still ahead in the stream
  sz% = SIZE_AUTO
  IF LEN(sfx$) = 0 AND lexer.peek$() = "." THEN
    IF lex.pos% + 1 <= LEN(lex.line$) THEN
      c$ = UCASE$(MID$(lex.line$, lex.pos% + 1, 1))
      IF c$ = "N" THEN sz% = SIZE_N : lex.pos% = lex.pos% + 2
      IF c$ = "W" THEN sz% = SIZE_W : lex.pos% = lex.pos% + 2
    END IF
  ELSEIF LEN(sfx$) >= 2 AND LEFT$(sfx$, 1) = "." THEN
    c$ = UCASE$(MID$(sfx$, 2, 1))
    IF c$ = "N" THEN sz% = SIZE_N : sfx$ = MID$(sfx$, 3)
    IF c$ = "W" THEN sz% = SIZE_W : sfx$ = MID$(sfx$, 3)
  END IF

  ' Mnemonic-specific 1-char flag suffix:
  '   R rounding flag on SMMUL / SMMLA / SMMLS
  '   X swap flag on dual multiplies SMUAD / SMUSD / SMLAD / SMLSD / SMLALD / SMLSLD
  IF LEN(sfx$) = 1 THEN
    base$ = LEFT$(uWord$, mlen%)
    IF sfx$ = "R" AND (base$ = "SMMUL" OR base$ = "SMMLA" OR base$ = "SMMLS") THEN
      fl% = FLAG_R
      sfx$ = ""
    ELSEIF sfx$ = "X" AND (base$ = "SMUAD" OR base$ = "SMUSD" OR base$ = "SMLAD" OR base$ = "SMLSD" OR base$ = "SMLALD" OR base$ = "SMLSLD") THEN
      fl% = FLAG_X
      sfx$ = ""
    END IF
  END IF

  IF LEN(sfx$) > 0 THEN
    showError "Unknown mnemonic suffix: " + word$ + " (leftover: " + sfx$ + ")"
    EXIT SUB
  END IF

  IF fl% <> FLAG_NONE THEN tokenizer.emitTok(TT_FLAG, fl%, "")
  IF cc% <> COND_NONE THEN tokenizer.emitTok(TT_COND, cc%, "")
  IF sz% <> SIZE_AUTO THEN tokenizer.emitTok(TT_SIZE, sz%, "")

  tokenizer.parseOperands()
END SUB
