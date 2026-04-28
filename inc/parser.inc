' ============================================================
' parser.inc  -  Pass 2: validate token sequence and maintain
'               IT-block state machine.
'
' Entry point: validateLine%()
'   Called after tokenizeLine() for every non-directive line.
'   Returns 0 on success, 1 if an error was reported (encode() is
'   skipped whenever this returns 1).
'
' IT-block state variables (defined in config.inc):
'   it.active%  1 while inside an IT block
'   it.cond%    COND_xx of the IT instruction (1-indexed)
'   it.count%   total instruction slots (1..4)
'   it.pos%     current slot, 0-indexed (0 = first instr after IT)
'   it.mask%    4-bit mask from the IT encoding
'
' Slot/mask relationship (derived from encoder.doIt):
'   Slot 0 always uses firstcond (T); mask has no T/E for it.
'   Slots 1..count-1: T/E encoded in mask bit (4 - it.pos%):
'     maskBit = J  -> T (firstcond)
'     maskBit != J -> E (inverted firstcond)
'   where J = (it.cond% - 1) AND 1
' ============================================================

DIM it.active% = 0   ' 1 while inside an IT block
DIM it.cond%   = 0   ' condition of the IT instruction (COND_xx)
DIM it.count%  = 0   ' total instructions covered by IT (1..4)
DIM it.pos%    = 0   ' current position within IT block (1..4)
DIM it.mask%   = 0   ' T/E mask bits (decoded from IT mnemonic)

FUNCTION validateLine%()
  LOCAL i%, cc%, maskBit%, J%, expectedCond%, armCond%
  LOCAL mne$ LENGTH 8

  validateLine% = 0   ' default: OK

  ' --- Extract condition code from token stream ---
  ' Tokens after the mnemonic (index 0) arrive in order:
  '   [TT_FLAG?] [TT_COND?] [TT_SIZE?] [operands...]
  ' Stop scanning as soon as we see something other than FLAG/COND/SIZE.
  cc% = COND_NONE
  FOR i% = 1 TO tok.count% - 1
    IF tok.type%(i%) = TT_COND THEN
      cc% = tok.val%(i%)
      EXIT FOR
    END IF
    IF tok.type%(i%) <> TT_FLAG AND tok.type%(i%) <> TT_SIZE THEN EXIT FOR
  NEXT i%

  mne$ = tok.str$(0)

  ' --- IT-block state machine ---

  IF it.active% THEN

    ' IT inside an active IT block is architecturally undefined.
    IF LEFT$(mne$, 2) = "IT" THEN
      showError "IT instruction not allowed inside IT block"
      it.active% = 0
      validateLine% = 1
      EXIT FUNCTION
    END IF

    ' Determine the expected condition for the current slot.
    IF it.pos% = 0 THEN
      ' Slot 0 is always T (firstcond); the mask carries no T/E for it.
      expectedCond% = it.cond%
    ELSE
      ' Slots 1..count-1: read T/E from mask bit (4 - it.pos%).
      maskBit%  = (it.mask% >> (4 - it.pos%)) AND 1
      J%        = (it.cond% - 1) AND 1
      IF maskBit% = J% THEN
        expectedCond% = it.cond%                        ' T slot
      ELSE
        armCond%      = (it.cond% - 1) XOR 1            ' flip LSB = inverted cond
        expectedCond% = armCond% + 1                    ' back to 1-indexed COND_xx
      END IF
    END IF

    ' Validate the condition on this instruction.
    IF cc% = COND_NONE THEN
      showError "Instruction inside IT block must carry a condition code"
      validateLine% = 1
    ELSEIF cc% <> expectedCond% THEN
      showError "Wrong condition for IT slot " + STR$(it.pos%) + " (expected " + STR$(expectedCond%) + " got " + STR$(cc%) + ")"
      validateLine% = 1
    END IF

    ' Advance the IT block counter regardless of error, to keep state coherent.
    it.pos% = it.pos% + 1
    IF it.pos% >= it.count% THEN
      it.active% = 0
    END IF

  ELSEIF cc% <> COND_NONE THEN

    ' Conditional instruction outside an IT block.
    ' In Thumb-2, only B<cond> and BL<cond> are legal here; every other
    ' conditionally-encoded instruction must be guarded by an IT block.
    ' IT itself carries its condition as an operand, not a suffix, so
    ' it never reaches this branch (the tokenizer emits TT_COND but the
    ' mnemonic match exits before the operand loop).
    IF mne$ <> "B" AND mne$ <> "BL" AND LEFT$(mne$, 2) <> "IT" THEN
      showError "Conditional suffix on '" + mne$ + "' outside IT block"
      validateLine% = 1
    END IF

  END IF

END FUNCTION
