' ============================================================
' symbols.inc  -  Symbol table (.equ) and label table
' Phase 2.
'
' Symbol table: forward references not allowed; .equ must precede use.
' Label table:  forward references allowed via a backpatch chain.
'   Chain storage: lbl.chain%(i) = head halfword-index in io.buffer%;
'   each chain slot io.buffer%(p) holds the next index, -1 terminates.
'   On defineLbl the chain is walked and backpatchLbl() is called per slot.
' ============================================================

CONST MAX_SYMBOLS  = 256     ' max .equ symbol definitions
CONST MAX_LABELS   = 512     ' max label entries (incl. forward refs)

DIM sym.name$(MAX_SYMBOLS) LENGTH MAX_NAME_LEN
DIM sym.val%(MAX_SYMBOLS)
DIM sym.count% = 0

DIM lbl.name$(MAX_LABELS) LENGTH MAX_NAME_LEN
DIM lbl.addr%(MAX_LABELS)    ' resolved address  OR  head of backpatch chain
DIM lbl.def%(MAX_LABELS)     ' 0 = forward ref pending,  1 = resolved
DIM lbl.chain%(MAX_LABELS)   ' next unresolved halfword-index in chain (-1 = end)
DIM lbl.count% = 0

' Define a symbol from a .equ directive.
' Reports an error and does nothing if the name is already defined.
SUB defineSym(name$, val%)
  LOCAL i%
  IF LEN(name$) = 0 THEN showError "Empty symbol name" : EXIT SUB
  FOR i% = 1 TO sym.count%
    IF sym.name$(i%) = name$ THEN
      showError "Symbol already defined: " + name$
      EXIT SUB
    END IF
  NEXT i%
  IF sym.count% >= MAX_SYMBOLS THEN
    showError "Symbol table full (max " + STR$(MAX_SYMBOLS) + ")"
    EXIT SUB
  END IF
  sym.count% = sym.count% + 1
  sym.name$(sym.count%) = name$
  sym.val%(sym.count%)  = val%
END SUB

' Return 1 if symbol is defined, 0 otherwise (no error).
FUNCTION symExists%(name$)
  LOCAL i%
  FOR i% = 1 TO sym.count%
    IF sym.name$(i%) = name$ THEN symExists% = 1 : EXIT FUNCTION
  NEXT i%
  symExists% = 0
END FUNCTION

' Return the value of a symbol (case-sensitive lookup).
' Reports an error and returns 0 if the symbol is not found.
FUNCTION lookupSym%(name$)
  LOCAL i%
  FOR i% = 1 TO sym.count%
    IF sym.name$(i%) = name$ THEN
      lookupSym% = sym.val%(i%)
      EXIT FUNCTION
    END IF
  NEXT i%
  showError "Undefined symbol: " + name$
  lookupSym% = 0
END FUNCTION

' ============================================================
' Label table  (with forward-reference backpatch)
' ============================================================

' Define a label at byte address addr%.
' - If the label was previously forward-referenced: resolves it and
'   walks the backpatch chain, calling backpatchLbl() for each slot.
' - If the label was already defined: reports a duplicate error.
' - Otherwise: registers a new resolved label.
SUB defineLbl(name$, addr%)
  LOCAL i%, p%, nxt%
  IF LEN(name$) = 0 THEN showError "Empty label name" : EXIT SUB
  FOR i% = 1 TO lbl.count%
    IF lbl.name$(i%) = name$ THEN
      IF lbl.def%(i%) = 1 THEN
        showError "Duplicate label: " + name$
        EXIT SUB
      END IF
      ' Resolve forward reference and walk backpatch chain
      lbl.addr%(i%) = addr%
      lbl.def%(i%)  = 1
      p% = lbl.chain%(i%)
      DO WHILE p% >= 0
        nxt% = io.buffer%(p%)   ' next chain link stored in the placeholder slot
        backpatchLbl(p%, addr%)
        p% = nxt%
      LOOP
      lbl.chain%(i%) = -1
      EXIT SUB
    END IF
  NEXT i%
  ' New label, directly resolved
  IF lbl.count% >= MAX_LABELS THEN
    showError "Label table full (max " + STR$(MAX_LABELS) + ")"
    EXIT SUB
  END IF
  lbl.count% = lbl.count% + 1
  lbl.name$(lbl.count%) = name$
  lbl.addr%(lbl.count%) = addr%
  lbl.def%(lbl.count%)  = 1
  lbl.chain%(lbl.count%) = -1
END SUB

' Reference a label.
' Returns the resolved byte address if the label is already defined.
' Returns -1 if the label is not yet defined (forward reference);
' in that case curOff% is threaded into the backpatch chain so that
' defineLbl() can patch it later.
FUNCTION refLbl%(name$, curOff%)
  LOCAL i%
  FOR i% = 1 TO lbl.count%
    IF lbl.name$(i%) = name$ THEN
      IF lbl.def%(i%) = 1 THEN
        refLbl% = lbl.addr%(i%)
        EXIT FUNCTION
      END IF
      ' Existing forward-ref entry: prepend curOff% to chain
      io.buffer%(curOff%)  = lbl.chain%(i%)  ' point to old head (or -1)
      lbl.chain%(i%)     = curOff%         ' new head
      refLbl% = -1
      EXIT FUNCTION
    END IF
  NEXT i%
  ' Label not seen before: create a forward-ref entry
  IF lbl.count% >= MAX_LABELS THEN
    showError "Label table full (max " + STR$(MAX_LABELS) + ")"
    refLbl% = -1
    EXIT FUNCTION
  END IF
  lbl.count% = lbl.count% + 1
  lbl.name$(lbl.count%)  = name$
  lbl.addr%(lbl.count%)  = 0
  lbl.def%(lbl.count%)   = 0
  lbl.chain%(lbl.count%) = curOff%   ' first (and only) chain entry so far
  io.buffer%(curOff%)       = -1        ' chain terminator
  refLbl% = -1
END FUNCTION

' After the final source line: report every label that was referenced
' but never defined (i.e. still has lbl.def% = 0).
SUB checkLbls()
  LOCAL i%
  FOR i% = 1 TO lbl.count%
    IF lbl.def%(i%) = 0 THEN
      showError "Undefined label: " + lbl.name$(i%)
    END IF
  NEXT i%
END SUB

' ============================================================
' Phase 2 debug helpers
' ============================================================

' Print the complete symbol table to the console.
' Columns: name (TAB 37) decimal value (TAB 50) hex value
SUB dumpSymbols()
  LOCAL i%
  PRINT "-> Symbol table (" + STR$(sym.count%) + " " + choice(sym.count%=1,"entry","entries") + ")"
  PRINT "   Name"; TAB(37); "Decimal"; TAB(50); "Hex"
  PRINT "   " + STRING$(32, "-"); TAB(37); STRING$(12, "-"); TAB(50); STRING$(10, "-")
  FOR i% = 1 TO sym.count%
    PRINT "   "; sym.name$(i%); TAB(37); STR$(sym.val%(i%)); TAB(50); "0x" + RIGHT$("00000000" + HEX$(sym.val%(i%) AND &HFFFFFFFF), 8)
  NEXT i%
  PRINT
END SUB

' Print the complete label table to the console.
' Columns: name (TAB 37) state (TAB 46) address or chain
SUB dumpLabels()
  LOCAL i%
  PRINT "-> Label table (" + STR$(lbl.count%) + " " + choice(lbl.count%=1,"entry","entries") + ")"
  PRINT "   Name"; TAB(37); "State"; TAB(50); "Address"
  PRINT "   " + STRING$(32, "-"); TAB(37); STRING$(12, "-"); TAB(50); STRING$(10, "-")
  FOR i% = 1 TO lbl.count%
    IF lbl.def%(i%) = 1 THEN
      PRINT "   "; lbl.name$(i%); TAB(37); "resolved"; TAB(50); "0x" + RIGHT$("00000000" + HEX$(lbl.addr%(i%) AND &HFFFFFFFF), 8)
    ELSE
      PRINT "   "; lbl.name$(i%); TAB(37); "UNRESOLVED"; TAB(50); "chain=" + STR$(lbl.chain%(i%))
    END IF
  NEXT i%
  PRINT
END SUB
