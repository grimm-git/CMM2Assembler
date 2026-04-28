' ============================================================
' literals.inc  -  Literal pool management (Phase 5)
'
' LDR Rt,=value pseudo-instruction:
'   1. encoder.doLdr emits a 16-bit LDR Rt,[PC,#0] placeholder and
'      calls addLit(val%, refOff%) where refOff% is the halfword index
'      of the placeholder.
'   2. needsFlushLit%() returns 1 when the oldest pending literal is
'      within 32 halfwords of exceeding its reach (1020 bytes = 510 hw).
'   3. flushLit(force%) is called:
'        force%=0: only if needsFlushLit% triggered (at natural code breaks)
'        force%=1: unconditionally at end-of-file
'      It emits a B <past_pool> if the preceding word was not already an
'      unconditional branch, then emits the literal values and patches
'      every referencing LDR with the correct PC-relative offset.
' ============================================================

CONST MAX_LITERALS = 128     ' max entries in one literal pool

DIM lit.val%(MAX_LITERALS)      ' 32-bit value to be stored
DIM lit.refOff%(MAX_LITERALS)   ' halfword offset of the LDR that references it
DIM lit.count% = 0

' Add a 32-bit literal value to the current pool.
' refOff% = halfword index of the placeholder LDR instruction.
SUB addLit(val%, refOff%)
  IF lit.count% >= MAX_LITERALS THEN
    showError "Literal pool full (max " + STR$(MAX_LITERALS) + ")"
    EXIT SUB
  END IF
  lit.count% = lit.count% + 1
  lit.val%(lit.count%)    = val% AND &HFFFFFFFF
  lit.refOff%(lit.count%) = refOff%
END SUB

' Return 1 if any pending literal is close to exceeding its PC-relative reach.
' 16-bit LDR Rt,[PC,#imm8] reach: imm8*4 <= 1020 bytes = 510 halfwords from the
' PC of the LDR instruction.  PC = refOff*2 + 4 (word-aligned to refOff*2 + 4,
' but for the check we use worst-case: the literal must land within 1020 bytes).
' We flush proactively when the gap would exceed 480 halfwords (leave 30 slack).
FUNCTION needsFlushLit%()
  LOCAL oldest%, gap%
  IF lit.count% = 0 THEN needsFlushLit% = 0 : EXIT FUNCTION
  oldest% = lit.refOff%(1)   ' earliest LDR halfword index
  gap%    = io.ptr% - oldest%
  IF gap% >= 480 THEN needsFlushLit% = 1 ELSE needsFlushLit% = 0
END FUNCTION

' Flush the current literal pool.
' force%=1: unconditional; force%=0: only if needed (needsFlushLit%).
SUB flushLit(force%)
  LOCAL i%, litBase%, litByteBase%, pcAddr%, offset%, imm8%, brOff%
  LOCAL prevHw%, needBranch%
  LOCAL ldrOff%, litOff%, pcBase%, litAddr%
  LOCAL brTarget%, brOffset%, brImm%
  LOCAL placeholder%

  IF lit.count% = 0 THEN EXIT SUB
  IF NOT force% AND NOT needsFlushLit%() THEN EXIT SUB

  ' Check whether the instruction immediately before us is an unconditional branch.
  ' If not, emit a B <past_pool> to skip over the literal data.
  needBranch% = 1
  IF io.ptr% > 0 THEN
    prevHw% = io.buffer%(io.ptr% - 1)
    ' Unconditional B T2: bits[15:11] = 11100
    IF (prevHw% AND &HF800) = &HE000 THEN needBranch% = 0
    ' BX: 0100 0111 0xxx x000
    IF (prevHw% AND &HFF87) = &H4700 THEN needBranch% = 0
    ' POP {…,PC}: BC xx with bit 8 set
    IF (prevHw% AND &HFF00) = &HBD00 THEN needBranch% = 0
  END IF

  IF needBranch% THEN
    brOff% = io.ptr%
    emit16(&HE000)   ' B placeholder
  END IF

  ' Align to 4-byte boundary if needed (literals must be word-aligned for LDR PC-rel)
  IF (io.ptr% AND 1) <> 0 THEN emit16(0)

  ' Emit literal values (each 32-bit value = 2 halfwords, little-endian)
  litBase% = io.ptr%
  litByteBase% = io.base% + litBase% * 2
  FOR i% = 1 TO lit.count%
    emit16(lit.val%(i%) AND &HFFFF)
    emit16((lit.val%(i%) >> 16) AND &HFFFF)
  NEXT i%

  ' Patch the branch past the pool
  IF needBranch% THEN
    brTarget%  = io.base% + io.ptr% * 2
    brOffset%  = (brTarget% - (io.base% + brOff% * 2 + 4)) / 2
    IF brOffset% < -1024 OR brOffset% > 1023 THEN
      showError "Literal pool branch out of range"
    ELSE
      io.buffer%(brOff%) = &HE000 OR (brOffset% AND &H7FF)
    END IF
  END IF

  ' Patch each referencing LDR instruction
  FOR i% = 1 TO lit.count%
    ldrOff%  = lit.refOff%(i%)
    litOff%  = litBase% + (i% - 1) * 2   ' halfword index of this literal's lo-halfword

    ' PC for the LDR = word-aligned (ldrOff%*2 + 4), i.e. (ldrOff%*2+4) rounded DOWN to 4
    pcBase%  = io.base% + ldrOff% * 2 + 4
    pcBase%  = pcBase% AND &HFFFFFFFC   ' word-align downward

    litAddr% = io.base% + litOff% * 2

    offset%  = litAddr% - pcBase%
    placeholder% = io.buffer%(ldrOff%)
    IF (placeholder% AND &HF800) = &HF800 THEN
      ' 32-bit T2 literal: imm12 byte offset in hw1 low 12 bits, U=1 already set
      IF offset% < 0 OR offset% > 4095 THEN
        showError "Literal pool offset out of T3 range: " + STR$(offset%)
      ELSE
        io.buffer%(ldrOff% + 1) = (io.buffer%(ldrOff% + 1) AND &HF000) OR (offset% AND &HFFF)
      END IF
    ELSE
      ' 16-bit T1 LDR Rt,[PC,#imm8*4]
      IF offset% < 0 OR offset% > 1020 OR (offset% AND 3) <> 0 THEN
        showError "Literal pool offset out of T1 range or misaligned: " + STR$(offset%)
      ELSE
        imm8% = offset% / 4
        io.buffer%(ldrOff%) = (io.buffer%(ldrOff%) AND &HFF00) OR imm8%
      END IF
    END IF
  NEXT i%

  lit.count% = 0
END SUB
