' ============================================================
' encoder.inc - emit binary for one instruction
'               16-bit and 32-bit Thumb-2
'
' Token stream convention:
'   tok.str$(0) = mnemonic (upper-case, without S/cond/size)
'   tok.type%(1..) = TT_FLAG?, TT_COND?, TT_SIZE?, then operands
'   Operand index op% is passed to each handler.
' ============================================================

' Forward-branch patch type table
' enc.patchType%(slot) records what kind of branch placeholder was
' emitted at that halfword slot, so backpatchLbl() can write the
' correct encoding (the chain-link overwrites the placeholder bits).
'   0  = not a patched slot
'   1  = B T2  (16-bit unconditional)
'   2  = B T4  (32-bit unconditional)
'   3  = BL T1 (32-bit)
'   4  = CBZ/CBNZ T1 (16-bit). enc.patchAux% holds (isNz<<3)|Rn.
'  20..33 = Bcond T1 (16-bit), (value-20) = ARM cond 0..13
'  40..53 = B T3 / B.W (32-bit conditional), (value-40) = ARM cond 0..13
'
' enc.patchAux%(slot) carries per-slot data that backpatchLbl needs but
' the chain pointer in io.buffer%() has already overwritten. Currently
' used by patch type 4 (CBZ/CBNZ) to store the op bit and Rn.
DIM enc.patchType%(MAX_BUFFER)
DIM enc.patchAux%(MAX_BUFFER)

' --- Low-level emitters ---

SUB emit16(hw%)
  IF io.ptr% >= MAX_BUFFER THEN showError "Output buffer full" : EXIT SUB
  io.buffer%(io.ptr%) = hw% AND &HFFFF
  INC io.ptr%
END SUB

SUB emit32(hi%, lo%)
  IF io.ptr% + 1 >= MAX_BUFFER THEN showError "Output buffer full" : EXIT SUB
  io.buffer%(io.ptr%)     = hi% AND &HFFFF
  io.buffer%(io.ptr% + 1) = lo% AND &HFFFF
  INC io.ptr%,2
END SUB

' --- Helpers ---

' Rotate-left 32-bit (n may be 0..31)
FUNCTION encoder.rol32%(v%, n%)
  LOCAL r%
  r% = n% MOD 32
  IF r% = 0 THEN encoder.rol32% = v% AND &HFFFFFFFF : EXIT FUNCTION
  encoder.rol32% = (((v% AND &HFFFFFFFF) << r%) OR (((v% AND &HFFFFFFFF) >> (32 - r%)) AND &HFFFFFFFF)) AND &HFFFFFFFF
END FUNCTION

' Encode a modified 12-bit immediate (Thumb-2 data-processing).
' Returns imm12 (0..4095) on success; sets ok%=1.  ok%=0 on failure.
FUNCTION encoder.modImm12%(val%, ok%)
  LOCAL n%, r%, rotated%, v%
  v% = val% AND &HFFFFFFFF
  ok% = 1

  ' Pattern 00,00: zero-extended byte 0x000000XY
  IF (v% AND &HFFFFFF00) = 0 THEN encoder.modImm12% = v% AND &HFF : EXIT FUNCTION

  ' Pattern 00,01: 0x00XY00XY
  n% = v% AND &HFF
  IF v% = ((n%) OR (n% << 16)) AND n% <> 0 THEN
    encoder.modImm12% = &H100 OR n% : EXIT FUNCTION
  END IF

  ' Pattern 00,10: 0xXY00XY00
  n% = (v% >> 8) AND &HFF
  IF v% = (((n% << 8) OR (n% << 24)) AND &HFFFFFFFF) AND n% <> 0 THEN
    encoder.modImm12% = &H200 OR n% : EXIT FUNCTION
  END IF

  ' Pattern 00,11: 0xXYXYXYXY
  n% = v% AND &HFF
  IF v% = ((n%) OR (n% << 8) OR (n% << 16) OR (n% << 24)) AND n% <> 0 THEN
    encoder.modImm12% = &H300 OR n% : EXIT FUNCTION
  END IF

  ' Rotation encoding: find r such that ROL32(v,r) is 0x80..0xFF
  FOR r% = 1 TO 31
    rotated% = encoder.rol32%(v%, r%)
    IF rotated% >= &H80 AND rotated% <= &HFF THEN
      ' imm12[11:7] = ror_amount = r%, imm12[6:0] = data7 = rotated% AND &H7F
      encoder.modImm12% = ((r% AND &H1F) << 7) OR (rotated% AND &H7F)
      EXIT FUNCTION
    END IF
  NEXT r%

  ok% = 0
END FUNCTION

' Encode a 32-bit modified-immediate data-processing instruction.
' op1_5 = 5-bit op1 field (see ARM DPI table)
' S% = 1 if S-flag set
' Rn, Rd = register numbers (Rn=15 for MOV/MVN)
' val% = 32-bit immediate to encode
SUB encoder.emit32ModImm(op1_5%, S%, Rn%, Rd%, val%)
  LOCAL imm12%, ok%, i%, im3%, im8%
  LOCAL lo32%

  ok% = 0
  imm12% = encoder.modImm12%(val%, ok%)
  IF ok% = 0 THEN showError "Immediate not encodable as Thumb-2 modified immediate" : EXIT SUB
  ' imm12 = {i[11], imm3[10:8], imm8[7:0]}
  i%   = (imm12% >> 11) AND 1
  im3% = (imm12% >> 8)  AND 7
  im8% = imm12% AND &HFF
  lo32% = (im3% << 12) OR ((Rd% AND &HF) << 8) OR im8%
  emit32(&HF000 OR (i% << 10) OR (op1_5% << 5) OR (S% << 4) OR (Rn% AND &HF), lo32%)
END SUB

' Encode a 32-bit register data-processing instruction (shifted register).
' op1_5 = 5-bit op1 field; S% = 1 if S set; Rn, Rd, Rm = registers
' sh% = SHIFT_xx constant; shimm% = shift amount (0 = no shift)
SUB encoder.emit32RegDp(op1_5%, S%, Rn%, Rd%, Rm%, sh%, shimm%)
  LOCAL type2%, imm3%, imm2%
  LOCAL lo32dp%

  ' type = sh%-1 for LSL/LSR/ASR/ROR (SHIFT_LSL=1..SHIFT_RRX=5)
  ' RRX is encoded as ROR #0
  IF sh% = SHIFT_RRX THEN
    type2% = 3
    shimm% = 0
  ELSEIF sh% = SHIFT_NONE THEN
    type2% = 0
    shimm% = 0
  ELSE
    type2% = sh% - 1
  END IF
  imm3% = (shimm% >> 2) AND 7
  imm2% = shimm% AND 3
  lo32dp% = (imm3% << 12) OR ((Rd% AND &HF) << 8) OR (imm2% << 6) OR (type2% << 4) OR (Rm% AND &HF)
  emit32(&HEA00 OR (op1_5% << 5) OR (S% << 4) OR (Rn% AND &HF), lo32dp%)
END SUB

' Emit a 32-bit ADD/SUB with 12-bit plain immediate (ADDW/SUBW T4).
' addSub%: 0=ADD, 1=SUB; S%: 0/1; Rn,Rd,imm12 (0..4095 straight)
SUB encoder.emit32AddImm12(addSub%, S%, Rn%, Rd%, imm12%)
  LOCAL i%, im3%, im8%
  LOCAL op1_5%
  LOCAL lo32ai%
  LOCAL op4%

  IF addSub% = 0 THEN op1_5% = &H00 ELSE op1_5% = &H0D
  ' T4 encoding: first HW = F200 | S | Rn (for ADD) / F600 | S | Rn (for SUB)
  ' Actually ADD T4: 11110 i 10 0000 S Rn / 0 imm3 Rd imm8
  ' SUB T4: 11110 i 10 1010 S Rn / 0 imm3 Rd imm8
  i%   = (imm12% >> 11) AND 1
  im3% = (imm12% >> 8)  AND 7
  im8% = imm12% AND &HFF
  IF addSub% = 0 THEN op4% = 0 ELSE op4% = &HA
  lo32ai% = (im3% << 12) OR ((Rd% AND &HF) << 8) OR im8%
  emit32(&HF200 OR (i% << 10) OR (op4% << 4) OR (S% << 4) OR (Rn% AND &HF), lo32ai%)
END SUB

' Backpatch: write the branch offset into the previously emitted slot at off%.
' addr% = resolved byte address of target label.
' Type determined from enc.patchType%(off%) (set at emit time).
SUB backpatchLbl(off%, addr%)
  LOCAL ptype%, pcAddr%, offset%, imm%, imm11%, S%, J1%, J2%, imm10%
  LOCAL aux%, isNz%, rnCb%, i5Cb%, imm5Cb%
  LOCAL pliFlag5%, pcAlign5%, delta5%, mag5%
  LOCAL aux6%, rd6%, isWide6%, pcAlign6%, delta6%
  LOCAL i4w%, im3w%, im8w%
  LOCAL nw%, i4nw%, im3nw%, im8nw%
  LOCAL armCond%
  LOCAL armCond2%, J1c%, J2c%, imm6%, imm11c%

  ptype%  = enc.patchType%(off%)
  pcAddr% = io.base% + off% * 2 + 4   ' Thumb PC = instruction addr + 4

  IF ptype% = 0 THEN showError "Backpatch: no type info at slot " + STR$(off%) : EXIT SUB

  SELECT CASE ptype%
    CASE 1
      ' B T2: 16-bit unconditional
      offset% = addr% - pcAddr%
      IF (offset% MOD 2) <> 0 THEN showError "Branch target misaligned" : EXIT SUB
      imm% = offset% / 2
      IF imm% < -1024 OR imm% > 1023 THEN showError "B: label too far for 16-bit" : EXIT SUB
      io.buffer%(off%) = &HE000 OR (imm% AND &H7FF)

    CASE 2
      ' B T4: 32-bit unconditional
      offset% = (addr% - pcAddr%) / 2
      S%     = (offset% >> 23) AND 1
      J1%    = 1 XOR S% XOR ((offset% >> 21) AND 1)
      J2%    = 1 XOR S% XOR ((offset% >> 20) AND 1)
      imm10% = (offset% >> 11) AND &H3FF
      imm11% = offset% AND &H7FF
      io.buffer%(off%)     = &HF000 OR (S% << 10) OR imm10%
      io.buffer%(off% + 1) = &H9000 OR (J1% << 13) OR (J2% << 11) OR imm11%

    CASE 3
      ' BL T1: 32-bit
      offset% = (addr% - pcAddr%) / 2
      S%     = (offset% >> 23) AND 1
      J1%    = 1 XOR S% XOR ((offset% >> 21) AND 1)
      J2%    = 1 XOR S% XOR ((offset% >> 20) AND 1)
      imm10% = (offset% >> 11) AND &H3FF
      imm11% = offset% AND &H7FF
      io.buffer%(off%)     = &HF000 OR (S% << 10) OR imm10%
      io.buffer%(off% + 1) = &HD000 OR (J1% << 13) OR (J2% << 11) OR imm11%

    CASE 4
      ' CBZ / CBNZ T1: 1011 op0 i1 imm5 Rn  (op = isNz, only forward branches)
      aux%    = enc.patchAux%(off%)
      isNz%   = (aux% >> 3) AND 1
      rnCb%   = aux% AND 7
      offset% = addr% - pcAddr%
      IF offset% < 0 OR offset% > 126 OR (offset% AND 1) <> 0 THEN
        showError "CBZ/CBNZ: label out of range (0..126, even)" : EXIT SUB
      END IF
      imm5Cb% = (offset% >> 1) AND &H1F
      i5Cb%   = (offset% >> 6) AND 1
      io.buffer%(off%) = &HB100 OR (isNz% << 11) OR (i5Cb% << 9) OR (imm5Cb% << 3) OR rnCb%

    CASE 5
      ' PLD/PLI literal: hw0 = 1111 100x U001 1111, hw1 = 1111 imm12
      ' patchAux = pli flag (0=PLD, 1=PLI). Range: ±4095, target relative to Align(PC,4).
      pliFlag5% = enc.patchAux%(off%) AND 1
      pcAlign5% = pcAddr% AND &HFFFFFFFC
      delta5%   = addr% - pcAlign5%
      IF delta5% >= 0 THEN
        IF delta5% > 4095 THEN showError "PLD/PLI: label out of range (delta=" + STR$(delta5%) + ")" : EXIT SUB
        io.buffer%(off%)     = &HF89F OR (pliFlag5% << 8)
        io.buffer%(off% + 1) = &HF000 OR (delta5% AND &HFFF)
      ELSE
        mag5% = -delta5%
        IF mag5% > 4095 THEN showError "PLD/PLI: label out of range (delta=" + STR$(delta5%) + ")" : EXIT SUB
        io.buffer%(off%)     = &HF81F OR (pliFlag5% << 8)
        io.buffer%(off% + 1) = &HF000 OR (mag5% AND &HFFF)
      END IF

    CASE 6
      ' ADR forward-ref: patchAux bit4=isWide(.W), bits[3:0]=Rd
      aux6%     = enc.patchAux%(off%)
      rd6%      = aux6% AND &HF
      isWide6%  = (aux6% >> 4) AND 1
      pcAlign6% = pcAddr% AND &HFFFFFFFC
      delta6%   = addr% - pcAlign6%
      IF isWide6% = 0 THEN
        IF rd6% > 7 OR delta6% < 0 OR delta6% > 1020 OR (delta6% AND 3) <> 0 THEN
	    IF (delta6% AND 3) <> 0 THEN
		showError "ADR: distance to target not 4-aligned (delta=" + STR$(delta6%) + ")"
	    ELSE
              showError "ADR: target out of range for narrow form (use ADR.W)"
	    ENDIF
          EXIT SUB
        END IF
        io.buffer%(off%) = &HA000 OR (rd6% << 8) OR (delta6% / 4)
      ELSE
        IF delta6% >= 0 AND delta6% <= 4095 THEN
          i4w%  = (delta6% >> 11) AND 1
          im3w% = (delta6% >> 8) AND 7
          im8w% = delta6% AND &HFF
          io.buffer%(off%)     = &HF20F OR (i4w% << 10)
          io.buffer%(off% + 1) = (im3w% << 12) OR ((rd6% AND &HF) << 8) OR im8w%
        ELSEIF delta6% < 0 AND delta6% >= -4095 THEN
          nw%    = -delta6%
          i4nw%  = (nw% >> 11) AND 1
          im3nw% = (nw% >> 8) AND 7
          im8nw% = nw% AND &HFF
          io.buffer%(off%)     = &HF2AF OR (i4nw% << 10)
          io.buffer%(off% + 1) = (im3nw% << 12) OR ((rd6% AND &HF) << 8) OR im8nw%
        ELSE
          showError "ADR: target out of range (delta=" + STR$(delta6%) + ")"
        END IF
      END IF

    CASE IS >= 20
      IF ptype% < 40 THEN
        ' Bcond T1: 16-bit conditional,  ARM cond = ptype% - 20
        armCond% = ptype% - 20
        offset% = addr% - pcAddr%
        IF (offset% MOD 2) <> 0 THEN showError "Branch target misaligned" : EXIT SUB
        imm% = offset% / 2
        IF imm% < -128 OR imm% > 127 THEN showError "B<cond>: label too far for 16-bit" : EXIT SUB
        io.buffer%(off%) = &HD000 OR (armCond% << 8) OR (imm% AND &HFF)
      ELSE
        ' B T3: 32-bit conditional, ARM cond = ptype% - 40
        armCond2% = ptype% - 40
        offset% = (addr% - pcAddr%) / 2
        S%     = (offset% >> 23) AND 1
        J1c%   = (offset% >> 19) AND 1
        J2c%   = (offset% >> 18) AND 1
        imm6%  = (offset% >> 12) AND &H3F
        imm11c% = offset% AND &H7FF
        io.buffer%(off%)     = &HF000 OR (S% << 10) OR (armCond2% << 6) OR imm6%
        io.buffer%(off% + 1) = &H8000 OR (J1c% << 13) OR (J2c% << 11) OR imm11c%
      END IF

    CASE ELSE
      showError "Backpatch: unknown type " + STR$(ptype%) + " at slot " + STR$(off%)
  END SELECT
END SUB

' Resolve a branch target from a TT_ADDR token.
' Returns the target byte address, or emits a placeholder and returns -1.
' off% = current output halfword index (where placeholder will be written).
FUNCTION encoder.resolveLabel%(name$, off%)
  LOCAL addr%
  addr% = refLbl%(name$, off%)
  encoder.resolveLabel% = addr%
END FUNCTION

' --- Main dispatch ---

SUB encode()
  LOCAL fl%, cc%, sz%, op%
  LOCAL mne$ LENGTH 8
  mne$ = tok.str$(0)
  fl%  = FLAG_NONE
  cc%  = COND_NONE
  sz%  = SIZE_AUTO
  op%  = 1

  DO WHILE op% < tok.count%
    SELECT CASE tok.type%(op%)
      CASE TT_FLAG : fl% = tok.val%(op%) : INC op%
      CASE TT_COND : cc% = tok.val%(op%) : INC op%
      CASE TT_SIZE : sz% = tok.val%(op%) : INC op%
      CASE ELSE    : EXIT DO
    END SELECT
  LOOP

  SELECT CASE mne$
    CASE "MOV"            : encoder.doMov(op%, fl%, cc%, sz%)
    CASE "MOVW"           : encoder.doMovw(op%, fl%, cc%, sz%)
    CASE "MOVT"           : encoder.doMovt(op%, fl%, cc%, sz%)
    CASE "MVN"            : encoder.doMvn(op%, fl%, cc%, sz%)
    CASE "ADD", "ADDW"    : encoder.doAdd(op%, fl%, cc%, sz%)
    CASE "SUB", "SUBW"    : encoder.doSub(op%, fl%, cc%, sz%)
    CASE "SUBS"           : encoder.doSub(op%, FLAG_S, cc%, sz%)
    CASE "ADC"            : encoder.doAdcSbc(op%, fl%, cc%, sz%, 0)
    CASE "SBC"            : encoder.doAdcSbc(op%, fl%, cc%, sz%, 1)
    CASE "RSB"            : encoder.doRsb(op%, fl%, cc%, sz%)
    CASE "AND"            : encoder.doBitwise(op%, fl%, cc%, sz%, 0)
    CASE "ORR"            : encoder.doBitwise(op%, fl%, cc%, sz%, 1)
    CASE "EOR"            : encoder.doBitwise(op%, fl%, cc%, sz%, 2)
    CASE "BIC"            : encoder.doBitwise(op%, fl%, cc%, sz%, 3)
    CASE "ORN"            : encoder.doBitwise(op%, fl%, cc%, sz%, 4)
    CASE "MUL"            : encoder.doMul(op%, fl%, cc%, sz%)
    CASE "LSL"            : encoder.doShift(op%, fl%, cc%, sz%, SHIFT_LSL)
    CASE "LSR"            : encoder.doShift(op%, fl%, cc%, sz%, SHIFT_LSR)
    CASE "ASR"            : encoder.doShift(op%, fl%, cc%, sz%, SHIFT_ASR)
    CASE "ROR"            : encoder.doShift(op%, fl%, cc%, sz%, SHIFT_ROR)
    CASE "RRX"            : encoder.doRrx(op%, fl%, cc%, sz%)
    CASE "CMP"            : encoder.doCmp(op%, fl%, cc%, sz%)
    CASE "CMN"            : encoder.doCmn(op%, fl%, cc%, sz%)
    CASE "TST"            : encoder.doTst(op%, fl%, cc%, sz%)
    CASE "TEQ"            : encoder.doTeq(op%, fl%, cc%, sz%)
    CASE "LDR"            : encoder.doLdr(op%, fl%, cc%, sz%)
    CASE "LDRB"           : encoder.doLdrx(op%, fl%, cc%, sz%, 1, 0)
    CASE "LDRH"           : encoder.doLdrx(op%, fl%, cc%, sz%, 2, 0)
    CASE "LDRSB"          : encoder.doLdrx(op%, fl%, cc%, sz%, 1, 1)
    CASE "LDRSH"          : encoder.doLdrx(op%, fl%, cc%, sz%, 2, 1)
    CASE "STR"            : encoder.doStr(op%, fl%, cc%, sz%)
    CASE "STRB"           : encoder.doStrx(op%, fl%, cc%, sz%, 1)
    CASE "STRH"           : encoder.doStrx(op%, fl%, cc%, sz%, 2)
    CASE "LDM", "LDMIA", "LDMFD" : encoder.doLdm(op%, fl%, cc%, sz%)
    CASE "STM", "STMIA", "STMEA" : encoder.doStm(op%, fl%, cc%, sz%)
    CASE "LDMDB", "LDMEA" : encoder.doLdmdb(op%, fl%, cc%, sz%)
    CASE "STMDB", "STMFD" : encoder.doStmdb(op%, fl%, cc%, sz%)
    CASE "PUSH"           : encoder.doPush(op%, fl%, cc%, sz%)
    CASE "POP"            : encoder.doPop(op%, fl%, cc%, sz%)
    CASE "ADR"            : encoder.doAdr(op%, fl%, cc%, sz%)
    CASE "B"              : encoder.doBranch(op%, fl%, cc%, sz%)
    CASE "BL"             : encoder.doBl(op%, fl%, cc%, sz%)
    CASE "BX"             : encoder.doBx(op%, fl%, cc%, sz%)
    CASE "BLX"            : encoder.doBlx(op%, fl%, cc%, sz%)
    CASE "CBZ"            : encoder.doCbz(op%, fl%, cc%, sz%, 0)
    CASE "CBNZ"           : encoder.doCbz(op%, fl%, cc%, sz%, 1)
    CASE "IT"             : encoder.doIt(op%, fl%, cc%, sz%)
    CASE "NOP"            : encoder.doHint(sz%, &HBF00, 0)
    CASE "YIELD"          : encoder.doHint(sz%, &HBF10, 1)
    CASE "WFE"            : encoder.doHint(sz%, &HBF20, 2)
    CASE "WFI"            : encoder.doHint(sz%, &HBF30, 3)
    CASE "SEV"            : encoder.doHint(sz%, &HBF40, 4)
    CASE "BKPT"           : encoder.doBkpt(op%, fl%, cc%, sz%)
    CASE "SVC"            : encoder.doSvc(op%, fl%, cc%, sz%)
    CASE "SXTB"           : encoder.doSxtb(op%, fl%, cc%, sz%)
    CASE "SXTH"           : encoder.doSxth(op%, fl%, cc%, sz%)
    CASE "UXTB"           : encoder.doUxtb(op%, fl%, cc%, sz%)
    CASE "UXTH"           : encoder.doUxth(op%, fl%, cc%, sz%)
    CASE "SXTB16"         : encoder.doExtend32(op%, &HFA2F, 0)
    CASE "UXTB16"         : encoder.doExtend32(op%, &HFA3F, 0)
    CASE "SXTAB"          : encoder.doExtend32(op%, &HFA40, 1)
    CASE "SXTAH"          : encoder.doExtend32(op%, &HFA00, 1)
    CASE "SXTAB16"        : encoder.doExtend32(op%, &HFA20, 1)
    CASE "UXTAB"          : encoder.doExtend32(op%, &HFA50, 1)
    CASE "UXTAH"          : encoder.doExtend32(op%, &HFA10, 1)
    CASE "UXTAB16"        : encoder.doExtend32(op%, &HFA30, 1)
    CASE "PKHBT"          : encoder.doPkh(op%, 0)
    CASE "PKHTB"          : encoder.doPkh(op%, 1)
    CASE "DBG"            : encoder.doDbg(op%)
    CASE "LDREX"          : encoder.doLdrex(op%, fl%, cc%, sz%, 1)
    CASE "STREX"          : encoder.doLdrex(op%, fl%, cc%, sz%, 0)
    CASE "LDREXB"         : encoder.doLdrexBH(op%, fl%, cc%, sz%, 1, 0)
    CASE "LDREXH"         : encoder.doLdrexBH(op%, fl%, cc%, sz%, 1, 1)
    CASE "STREXB"         : encoder.doLdrexBH(op%, fl%, cc%, sz%, 0, 0)
    CASE "STREXH"         : encoder.doLdrexBH(op%, fl%, cc%, sz%, 0, 1)
    CASE "CLREX"          : emit32(&HF3BF, &H8F2F)
    CASE "LDRD"           : encoder.doLdrd(op%, fl%, cc%, sz%, 1)
    CASE "STRD"           : encoder.doLdrd(op%, fl%, cc%, sz%, 0)
    CASE "LDRT"           : encoder.doLdrxt(op%, fl%, cc%, sz%, &HF850)
    CASE "LDRBT"          : encoder.doLdrxt(op%, fl%, cc%, sz%, &HF810)
    CASE "LDRSBT"         : encoder.doLdrxt(op%, fl%, cc%, sz%, &HF910)
    CASE "LDRHT"          : encoder.doLdrxt(op%, fl%, cc%, sz%, &HF830)
    CASE "LDRSHT"         : encoder.doLdrxt(op%, fl%, cc%, sz%, &HF930)
    CASE "STRT"           : encoder.doLdrxt(op%, fl%, cc%, sz%, &HF840)
    CASE "STRBT"          : encoder.doLdrxt(op%, fl%, cc%, sz%, &HF800)
    CASE "STRHT"          : encoder.doLdrxt(op%, fl%, cc%, sz%, &HF820)
    CASE "REV"            : encoder.doRev0(op%, fl%, cc%, sz%)
    CASE "REV16"          : encoder.doRev1(op%, fl%, cc%, sz%)
    CASE "REVSH"          : encoder.doRev3(op%, fl%, cc%, sz%)
    CASE "SETEND"         : encoder.doSetend(op%, fl%, cc%, sz%)
    CASE "CPS"            : encoder.doCps(op%, fl%, cc%, sz%)
    CASE "CLZ"            : encoder.doClz(op%, fl%, cc%, sz%)
    CASE "SDIV"           : encoder.doDiv(op%, fl%, cc%, sz%, 0)
    CASE "UDIV"           : encoder.doDiv(op%, fl%, cc%, sz%, 1)
    CASE "MLA"            : encoder.doMla(op%, fl%, cc%, sz%)
    CASE "MLS"            : encoder.doMls(op%, fl%, cc%, sz%)
    CASE "SMULL"          : encoder.doMull(op%, fl%, cc%, sz%, 0)
    CASE "UMULL"          : encoder.doMull(op%, fl%, cc%, sz%, 1)
    CASE "SMLAL"          : encoder.doMull(op%, fl%, cc%, sz%, 2)
    CASE "UMLAL"          : encoder.doMull(op%, fl%, cc%, sz%, 3)
    CASE "SBFX"           : encoder.doBitfield(op%, fl%, cc%, sz%, 0)
    CASE "UBFX"           : encoder.doBitfield(op%, fl%, cc%, sz%, 1)
    CASE "BFI"            : encoder.doBfi(op%, fl%, cc%, sz%)
    CASE "BFC"            : encoder.doBfc(op%, fl%, cc%, sz%)
    CASE "RBIT"           : encoder.doRbit(op%, fl%, cc%, sz%)
    CASE "TBB"            : encoder.doTb(op%, fl%, cc%, sz%, 0)
    CASE "TBH"            : encoder.doTb(op%, fl%, cc%, sz%, 1)
    CASE "SEL"            : encoder.doSel(op%, fl%, cc%, sz%)
    ' Parallel add/sub  -- op1 selects operation, op2 selects signedness/modifier
    CASE "SADD16"         : encoder.doParAddSub(op%, 1, 0)
    CASE "SASX"           : encoder.doParAddSub(op%, 2, 0)
    CASE "SSAX"           : encoder.doParAddSub(op%, 6, 0)
    CASE "SSUB16"         : encoder.doParAddSub(op%, 5, 0)
    CASE "SADD8"          : encoder.doParAddSub(op%, 0, 0)
    CASE "SSUB8"          : encoder.doParAddSub(op%, 4, 0)
    CASE "QADD16"         : encoder.doParAddSub(op%, 1, 1)
    CASE "QASX"           : encoder.doParAddSub(op%, 2, 1)
    CASE "QSAX"           : encoder.doParAddSub(op%, 6, 1)
    CASE "QSUB16"         : encoder.doParAddSub(op%, 5, 1)
    CASE "QADD8"          : encoder.doParAddSub(op%, 0, 1)
    CASE "QSUB8"          : encoder.doParAddSub(op%, 4, 1)
    CASE "SHADD16"        : encoder.doParAddSub(op%, 1, 2)
    CASE "SHASX"          : encoder.doParAddSub(op%, 2, 2)
    CASE "SHSAX"          : encoder.doParAddSub(op%, 6, 2)
    CASE "SHSUB16"        : encoder.doParAddSub(op%, 5, 2)
    CASE "SHADD8"         : encoder.doParAddSub(op%, 0, 2)
    CASE "SHSUB8"         : encoder.doParAddSub(op%, 4, 2)
    CASE "UADD16"         : encoder.doParAddSub(op%, 1, 4)
    CASE "UASX"           : encoder.doParAddSub(op%, 2, 4)
    CASE "USAX"           : encoder.doParAddSub(op%, 6, 4)
    CASE "USUB16"         : encoder.doParAddSub(op%, 5, 4)
    CASE "UADD8"          : encoder.doParAddSub(op%, 0, 4)
    CASE "USUB8"          : encoder.doParAddSub(op%, 4, 4)
    CASE "UQADD16"        : encoder.doParAddSub(op%, 1, 5)
    CASE "UQASX"          : encoder.doParAddSub(op%, 2, 5)
    CASE "UQSAX"          : encoder.doParAddSub(op%, 6, 5)
    CASE "UQSUB16"        : encoder.doParAddSub(op%, 5, 5)
    CASE "UQADD8"         : encoder.doParAddSub(op%, 0, 5)
    CASE "UQSUB8"         : encoder.doParAddSub(op%, 4, 5)
    CASE "UHADD16"        : encoder.doParAddSub(op%, 1, 6)
    CASE "UHASX"          : encoder.doParAddSub(op%, 2, 6)
    CASE "UHSAX"          : encoder.doParAddSub(op%, 6, 6)
    CASE "UHSUB16"        : encoder.doParAddSub(op%, 5, 6)
    CASE "UHADD8"         : encoder.doParAddSub(op%, 0, 6)
    CASE "UHSUB8"         : encoder.doParAddSub(op%, 4, 6)
    CASE "DMB"            : encoder.doBarrier(op%, fl%, cc%, sz%, &HF3BF, &H8F50)
    CASE "DSB"            : encoder.doBarrier(op%, fl%, cc%, sz%, &HF3BF, &H8F40)
    CASE "ISB"            : encoder.doBarrier(op%, fl%, cc%, sz%, &HF3BF, &H8F60)
    CASE "MRS"            : encoder.doMrs(op%, fl%, cc%, sz%)
    CASE "MSR"            : encoder.doMsr(op%, fl%, cc%, sz%)
    CASE "PLD"            : encoder.doPldPli(op%, fl%, cc%, sz%, 0)
    CASE "PLI"            : encoder.doPldPli(op%, fl%, cc%, sz%, 1)
    CASE "QADD"           : encoder.doSatArith(op%, 8)
    CASE "QDADD"          : encoder.doSatArith(op%, 9)
    CASE "QSUB"           : encoder.doSatArith(op%, 10)
    CASE "QDSUB"          : encoder.doSatArith(op%, 11)
    CASE "SMULBB"         : encoder.doMul3or4(op%, &HFB10, &H00, 0)
    CASE "SMULBT"         : encoder.doMul3or4(op%, &HFB10, &H10, 0)
    CASE "SMULTB"         : encoder.doMul3or4(op%, &HFB10, &H20, 0)
    CASE "SMULTT"         : encoder.doMul3or4(op%, &HFB10, &H30, 0)
    CASE "SMLABB"         : encoder.doMul3or4(op%, &HFB10, &H00, 1)
    CASE "SMLABT"         : encoder.doMul3or4(op%, &HFB10, &H10, 1)
    CASE "SMLATB"         : encoder.doMul3or4(op%, &HFB10, &H20, 1)
    CASE "SMLATT"         : encoder.doMul3or4(op%, &HFB10, &H30, 1)
    CASE "SMULWB"         : encoder.doMul3or4(op%, &HFB30, &H00, 0)
    CASE "SMULWT"         : encoder.doMul3or4(op%, &HFB30, &H10, 0)
    CASE "SMLAWB"         : encoder.doMul3or4(op%, &HFB30, &H00, 1)
    CASE "SMLAWT"         : encoder.doMul3or4(op%, &HFB30, &H10, 1)
    CASE "USAD8"          : encoder.doMul3or4(op%, &HFB70, &H00, 0)
    CASE "USADA8"         : encoder.doMul3or4(op%, &HFB70, &H00, 1)
    CASE "SMMUL"          : encoder.doMul3or4(op%, &HFB50, choice(fl%=FLAG_R, &H10, &H00), 0)
    CASE "SMMLA"          : encoder.doMul3or4(op%, &HFB50, choice(fl%=FLAG_R, &H10, &H00), 1)
    CASE "SMMLS"          : encoder.doMul3or4(op%, &HFB60, choice(fl%=FLAG_R, &H10, &H00), 1)
    CASE "SMUAD"          : encoder.doMul3or4(op%, &HFB20, choice(fl%=FLAG_X, &H10, &H00), 0)
    CASE "SMUSD"          : encoder.doMul3or4(op%, &HFB40, choice(fl%=FLAG_X, &H10, &H00), 0)
    CASE "SMLAD"          : encoder.doMul3or4(op%, &HFB20, choice(fl%=FLAG_X, &H10, &H00), 1)
    CASE "SMLSD"          : encoder.doMul3or4(op%, &HFB40, choice(fl%=FLAG_X, &H10, &H00), 1)
    CASE "SMLALBB"        : encoder.doMull4op(op%, &HFBC0, &H80)
    CASE "SMLALBT"        : encoder.doMull4op(op%, &HFBC0, &H90)
    CASE "SMLALTB"        : encoder.doMull4op(op%, &HFBC0, &HA0)
    CASE "SMLALTT"        : encoder.doMull4op(op%, &HFBC0, &HB0)
    CASE "SMLALD"         : encoder.doMull4op(op%, &HFBC0, choice(fl%=FLAG_X, &HD0, &HC0))
    CASE "SMLSLD"         : encoder.doMull4op(op%, &HFBD0, choice(fl%=FLAG_X, &HD0, &HC0))
    CASE "UMAAL"          : encoder.doMull4op(op%, &HFBE0, &H60)
    CASE "SSAT"           : encoder.doSat(op%, &HF300, 1)
    CASE "USAT"           : encoder.doSat(op%, &HF380, 0)
    CASE "SSAT16"         : encoder.doSat16(op%, &HF320, 1)
    CASE "USAT16"         : encoder.doSat16(op%, &HF3A0, 0)
    CASE ELSE
      IF LEFT$(mne$, 2) = "IT" THEN
        encoder.doIt(op%, fl%, cc%, sz%)
      ELSE
        showError "Unimplemented mnemonic: " + mne$
      END IF
  END SELECT
END SUB

' ============================================================
' Helper: read a reg/imm/shift/addr sequence.
' After calling, the following LOCAL vars hold results:
'   i%        = index PAST last consumed token
'   rd%,rn%,rm%  = register numbers (-1 if not present)
'   imm%      = immediate value (meaningful only if has_imm%=1)
'   sh_type%  = SHIFT_xx (SHIFT_NONE=0 if no shift)
'   sh_imm%   = shift amount (0 for RRX)
'   has_imm%, has_rm%, has_shift%  = 0/1 flags
' This is done inline in each handler to avoid MMBasic SUB limitations.
' ============================================================

' ============================================================
' MOV  -  move immediate or register
' ============================================================
SUB encoder.doMov(op%, fl%, cc%, sz%)
    LOCAL i%, rd%, rm%, imm%, ok%, S%
    LOCAL sh%, shimm%
    LOCAL type2%
    LOCAL imm3%, imm2%
    LOCAL sh2%, shimm2%

  S% = (fl% = FLAG_S)
  i% = op%
  IF tok.type%(i%) <> TT_REGLO AND tok.type%(i%) <> TT_REGHI THEN
    showError "MOV: expected destination register" : EXIT SUB
  END IF
  rd% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF i% >= tok.count% THEN showError "MOV: missing source" : EXIT SUB

  IF tok.type%(i%) = TT_IMM THEN
    imm% = tok.val%(i%) AND &HFFFFFFFF : INC i%
    ' MOVS Rd, #imm8  T1 16-bit: 001 00 Rd imm8 (Rd must be R0-R7, imm 0-255, always sets S)
    IF S% AND rd% <= 7 AND imm% <= 255 AND sz% <> SIZE_W THEN
      emit16(&H2000 OR (rd% << 8) OR imm%)
    ELSE
      ' MOV.W T3: ORR-family encoding with Rn=1111
      ok% = 0
      IF sz% = SIZE_N THEN showError "MOV: value too large for 16-bit" : EXIT SUB
      encoder.emit32ModImm(&H02, S%, &HF, rd%, imm%)
    END IF

  ELSEIF (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) AND sz% <> SIZE_W THEN
    rm% = tok.val%(i%) : INC i%
    ' Check for shift after register
    IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
    IF i% < tok.count% AND tok.type%(i%) = TT_SHIFT THEN
      ' MOV.W Rd, Rm, <shift> #n  (32-bit)
      sh% = tok.val%(i%) : INC i%
      shimm% = 0
      IF i% < tok.count% AND tok.type%(i%) = TT_IMM THEN shimm% = tok.val%(i%) : INC i%
      IF sh% = SHIFT_RRX THEN
        ' RRX T1: 1110 1010 0100 1111 | 0000 Rd 0011 Rm
        emit32(&HEA4F, ((rd% AND &HF) << 8) OR &H30 OR (rm% AND &HF))
      ELSE
        ' LSL/LSR/ASR/ROR T3 (shift-by-imm)
        type2% = sh% - 1
        imm3% = (shimm% >> 2) AND 7 : imm2% = shimm% AND 3
        emit32(&HEA4F OR (S% << 4), (imm3% << 12) OR ((rd% AND &HF) << 8) OR (imm2% << 6) OR (type2% << 4) OR (rm% AND &HF))
      END IF
      EXIT SUB
    END IF
    ' No shift: plain MOV Rd, Rm
    IF NOT S% THEN
      ' MOV T1 hi-reg: 0100 0100 DN Rm Rdn
      emit16(&H4600 OR ((rd% >> 3) << 7) OR ((rm% AND &HF) << 3) OR (rd% AND 7))
    ELSE
      ' MOVS T2 16-bit: need lo regs; MOVS Rd,Rm is LSL #0
      IF rd% <= 7 AND rm% <= 7 THEN
        emit16((rm% << 3) OR rd%)   ' LSL #0 T1 encoding = 000 00 00000 Rm Rd
      ELSE
        encoder.emit32RegDp(&H02, 1, &HF, rd%, rm%, SHIFT_NONE, 0)
      END IF
    END IF
  ELSE
    ' 32-bit MOV reg with possible shift
    IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN
      showError "MOV: bad operand" : EXIT SUB
    END IF
    rm% = tok.val%(i%) : INC i%
    sh2% = SHIFT_NONE : shimm2% = 0
    IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
    IF i% < tok.count% AND tok.type%(i%) = TT_SHIFT THEN
      sh2% = tok.val%(i%) : INC i%
      IF i% < tok.count% AND tok.type%(i%) = TT_IMM THEN shimm2% = tok.val%(i%) : INC i%
    END IF
    encoder.emit32RegDp(&H02, S%, &HF, rd%, rm%, sh2%, shimm2%)
  END IF
END SUB

' MOVW: 16-bit zero-extended immediate  T3
SUB encoder.doMovw(op%, fl%, cc%, sz%)
    LOCAL i%, rd%, imm%
    LOCAL i4%, imm4%, im3%, im8%

    i% = op%
    IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "MOVW: expected register" : EXIT SUB
    rd% = tok.val%(i%) : INC i%
    IF tok.type%(i%) = TT_COMMA THEN INC i%
    IF tok.type%(i%) <> TT_IMM THEN showError "MOVW: expected immediate" : EXIT SUB
    imm% = tok.val%(i%) AND &HFFFF
    ' MOVW T3: 11110 i 10 0100 imm4 | 0 imm3 Rd imm8
    i4%   = (imm% >> 11) AND 1
    imm4% = (imm% >> 12) AND &HF
    im3%  = (imm% >> 8)  AND 7
    im8%  = imm% AND &HFF
    emit32(&HF240 OR (i4% << 10) OR imm4%, (im3% << 12) OR ((rd% AND &HF) << 8) OR im8%)
END SUB

' MOVT: top halfword  T1
SUB encoder.doMovt(op%, fl%, cc%, sz%)
  LOCAL i%, rd%, imm%
  LOCAL i4%, imm4%, im3%, im8%
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "MOVT: expected register" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF tok.type%(i%) <> TT_IMM THEN showError "MOVT: expected immediate" : EXIT SUB
  imm% = tok.val%(i%) AND &HFFFF
  i4%   = (imm% >> 11) AND 1
  imm4% = (imm% >> 12) AND &HF
  im3%  = (imm% >> 8)  AND 7
  im8%  = imm% AND &HFF
  emit32(&HF2C0 OR (i4% << 10) OR imm4%, (im3% << 12) OR ((rd% AND &HF) << 8) OR im8%)
END SUB

' MVN  T1 16-bit / T2 32-bit
SUB encoder.doMvn(op%, fl%, cc%, sz%)
  LOCAL i%, rd%, rm%, imm%, S%, sh%, shimm%
  S% = (fl% = FLAG_S)
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "MVN: expected register" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF tok.type%(i%) = TT_IMM THEN
    imm% = tok.val%(i%)
    encoder.emit32ModImm(&H03, S%, &HF, rd%, imm%)   ' MVN = NOT-ORR with Rn=1111, op1=00011
  ELSEIF tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI THEN
    rm% = tok.val%(i%) : INC i%
    sh% = SHIFT_NONE : shimm% = 0
    IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
    IF i% < tok.count% AND tok.type%(i%) = TT_SHIFT THEN
      sh% = tok.val%(i%) : INC i%
      IF i% < tok.count% AND tok.type%(i%) = TT_IMM THEN shimm% = tok.val%(i%) : INC i%
    END IF
    ' MVN T1 16-bit: 0100 0011 11 Rm Rd (rd,rm must be R0-R7, sets S, no shift)
    IF S% AND rd% <= 7 AND rm% <= 7 AND sh% = SHIFT_NONE AND sz% <> SIZE_W THEN
      emit16(&H43C0 OR (rm% << 3) OR rd%)
    ELSE
      encoder.emit32RegDp(&H03, S%, &HF, rd%, rm%, sh%, shimm%)
    END IF
  ELSE
    showError "MVN: bad operand"
  END IF
END SUB

' ============================================================
' ADD / ADDS
' ============================================================
SUB encoder.doAdd(op%, fl%, cc%, sz%)
  LOCAL i%, rd%, rn%, rm%, imm%, S%, has_imm%, has_rm%, sh%, shimm%, ok%, negVal%, forceW%
  LOCAL imm12t%
  LOCAL i4a%, im3a%, im8a%, adrimm%
  S% = (fl% = FLAG_S)
  forceW% = (tok.str$(0) = "ADDW")
  i% = op%

  ' Rd
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "ADD: expected Rd" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%

  ' After Rd: Rn, Rm, Imm, or end-of-operands
  IF i% >= tok.count% THEN showError "ADD: missing operand" : EXIT SUB

  ' Check for 2-operand form: ADD Rd, <src>
  IF tok.type%(i%) = TT_IMM THEN
    imm% = tok.val%(i%) AND &HFFFFFFFF : INC i%
    ' Negative immediate: rewrite as SUB
    IF imm% > &H7FFFFFFF THEN
      ' Treat as negative → rewrite ADDS Rd,#-n as SUBS Rd,#n
      negVal% = (-(imm%)) AND &HFFFFFFFF   ' This is ugly in MMBasic; let's just call doSub logic
      ' Reconstruct token buffer trick: just call doSub directly with modified imm
      ' Actually easier to encode the 32-bit SUBS
      ok% = 0
      IF sz% = SIZE_N THEN showError "ADD/SUB: negative imm not encodable as 16-bit" : EXIT SUB
      encoder.emit32ModImm(&H0D, S%, rd%, rd%, ((-imm%) AND &HFFFFFFFF))
      EXIT SUB
    END IF
    ' ADD SP, #imm*4 T2 (16-bit): SP=R13, imm must be multiple of 4, 0..508, no S
    IF rd% = 13 AND NOT S% AND (imm% AND 3) = 0 AND imm% <= 508 AND sz% <> SIZE_W THEN
      emit16(&HB000 OR ((imm% / 4) AND &H7F))
      EXIT SUB
    END IF
    ' ADDS Rdn, #imm8 T2 16-bit: Rd=Rn, R0-R7, imm 0-255, sets S
    ' Also used for conditional ADD inside an IT block (S implied by 16-bit encoding).
    IF (S% OR (cc% <> COND_NONE)) AND rd% <= 7 AND imm% <= 255 AND sz% <> SIZE_W THEN
      emit16(&H3000 OR (rd% << 8) OR imm%)
      EXIT SUB
    END IF
    ' 32-bit: try modified immediate first, then plain imm12
    IF sz% = SIZE_N THEN showError "ADD: imm too large for 16-bit" : EXIT SUB
    ok% = 0
    imm12t% = encoder.modImm12%(imm%, ok%)
    IF ok% AND NOT forceW% THEN
      ' ADD.W / ADDS.W T3 with modified immediate  (op1=0x08=ADD)
      encoder.emit32ModImm(&H08, S%, rd%, rd%, imm%)
    ELSEIF imm% <= 4095 THEN
      ' ADDW T4 plain imm12
      encoder.emit32AddImm12(0, S%, rd%, rd%, imm%)
    ELSE
      showError "ADD: immediate " + STR$(imm%) + " not encodable"
    END IF
    EXIT SUB
  END IF

  ' Second operand is a register: could be Rn (3-operand) or Rm (2-operand)
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "ADD: expected register" : EXIT SUB
  rn% = tok.val%(i%) : INC i%

  IF i% >= tok.count% OR tok.type%(i%) <> TT_COMMA THEN
    ' 2-operand: ADD Rd, Rn  →  treat as ADD Rd, Rd, Rn (hi-reg T2 or 32-bit)
    rm% = rn%
    ' ADD hi-reg T2: any regs, no S and not inside IT block, Rdn and Rm are the regs
    IF NOT S% AND (cc% = COND_NONE) AND sz% <> SIZE_W THEN
      emit16(&H4400 OR ((rd% >> 3) << 7) OR ((rm% AND &HF) << 3) OR (rd% AND 7))
    ELSE
      ' ADDS / conditional ADD: use 3-reg T1 if all low regs
      IF (S% OR (cc% <> COND_NONE)) AND rd% <= 7 AND rm% <= 7 AND sz% <> SIZE_W THEN
        emit16(&H1800 OR (rm% << 6) OR (rd% << 3) OR rd%)
      ELSE
        encoder.emit32RegDp(&H08, S%, rd%, rd%, rm%, SHIFT_NONE, 0)
      END IF
    END IF
    EXIT SUB
  END IF

  ' 3-operand: ADD Rd, Rn, <op2>
  INC i%  ' skip comma

  ' Rn=SP special cases
  IF rn% = 13 THEN   ' SP
    IF tok.type%(i%) = TT_IMM THEN
      imm% = tok.val%(i%)
      ' ADD SP, SP, #imm7*4 T2 16-bit: Rd=Rn=SP, imm=multiple of 4, 0..508, no S
      IF NOT S% AND rd% = 13 AND (imm% AND 3) = 0 AND imm% <= 508 AND sz% <> SIZE_W THEN
        emit16(&HB000 OR ((imm% / 4) AND &H7F))
        EXIT SUB
      END IF
      ' ADD Rd, SP, #imm8*4 T1 16-bit: Rd=R0-R7, imm=multiple of 4, 0..1020, no S
      IF NOT S% AND rd% <= 7 AND (imm% AND 3) = 0 AND imm% <= 1020 AND sz% <> SIZE_W THEN
        emit16(&HA800 OR (rd% << 8) OR (imm% / 4))
        EXIT SUB
      END IF
      ' 32-bit
      IF sz% = SIZE_N THEN showError "ADD SP,Rd,#n: imm too large for 16-bit" : EXIT SUB
      ok% = 0 : LOCAL imm12s%
      imm12s% = encoder.modImm12%(imm%, ok%)
      IF ok% AND NOT forceW% THEN
        encoder.emit32ModImm(&H08, S%, 13, rd%, imm%)
      ELSEIF imm% <= 4095 THEN
        encoder.emit32AddImm12(0, S%, 13, rd%, imm%)
      ELSE
        showError "ADD SP,Rd,#n: immediate not encodable"
      END IF
      EXIT SUB
    ELSEIF tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI THEN
      rm% = tok.val%(i%) : INC i%
      ' Read optional shift tokens
      sh% = SHIFT_NONE : shimm% = 0
      IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
      IF i% < tok.count% AND tok.type%(i%) = TT_SHIFT THEN
        sh% = tok.val%(i%) : INC i%
        IF i% < tok.count% AND tok.type%(i%) = TT_IMM THEN shimm% = tok.val%(i%) : INC i%
      END IF
      ' ADD Rd, SP, Rm
      ' If Rd=Rm (and no S, no shift), use ADD T2 16-bit: ADD Rdn, SP (hi-reg form 0x4400)
      IF NOT S% AND rd% = rm% AND sh% = SHIFT_NONE AND sz% <> SIZE_W THEN
        emit16(&H4400 OR ((rd% >> 3) << 7) OR ((13 AND &HF) << 3) OR (rd% AND 7))
        EXIT SUB
      END IF
      ' If Rd=SP and Rm is anything, also a hi-reg form (handled above via 2-op path)
      IF sz% = SIZE_N THEN showError "ADD Rd,SP,Rm: 16-bit not available" : EXIT SUB
      encoder.emit32RegDp(&H08, S%, 13, rd%, rm%, sh%, shimm%)
      EXIT SUB
    END IF
  END IF

  ' Rn=PC: ADR
  IF rn% = 15 THEN   ' PC
    IF tok.type%(i%) = TT_IMM THEN
      imm% = tok.val%(i%)
      IF NOT S% AND rd% <= 7 AND (imm% AND 3) = 0 AND imm% >= 0 AND imm% <= 1020 AND sz% <> SIZE_W THEN
        emit16(&HA000 OR (rd% << 8) OR (imm% / 4))
        EXIT SUB
      END IF
      ' ADR T3 32-bit
      IF sz% = SIZE_N THEN showError "ADR: offset too large for 16-bit" : EXIT SUB
      adrimm% = imm% AND &HFFF
      i4a%  = (adrimm% >> 11) AND 1
      im3a% = (adrimm% >> 8)  AND 7
      im8a% = adrimm% AND &HFF
      emit32(&HF20F OR (i4a% << 10), (im3a% << 12) OR ((rd% AND &HF) << 8) OR im8a%)
      EXIT SUB
    END IF
  END IF

  ' General 3-operand: Rd, Rn, Imm or Rd, Rn, Rm [,shift #n]
  IF tok.type%(i%) = TT_IMM THEN
    imm% = tok.val%(i%) AND &HFFFFFFFF : INC i%
    ' ADDS Rd, Rn, #imm3 T1 16-bit: all R0-R7, imm 0-7, always sets S
    IF S% AND rd% <= 7 AND rn% <= 7 AND imm% <= 7 AND sz% <> SIZE_W THEN
      emit16(&H1C00 OR (imm% << 6) OR (rn% << 3) OR rd%)
      EXIT SUB
    END IF
    ' ADDS Rdn, #imm8 T2 16-bit: Rd=Rn, R0-R7, imm 0-255
    IF S% AND rd% = rn% AND rd% <= 7 AND imm% <= 255 AND sz% <> SIZE_W THEN
      emit16(&H3000 OR (rd% << 8) OR imm%)
      EXIT SUB
    END IF
    ' 32-bit
    IF sz% = SIZE_N THEN showError "ADD: imm too large for 16-bit" : EXIT SUB
    ok% = 0 : LOCAL imm12g%
    imm12g% = encoder.modImm12%(imm%, ok%)
    IF ok% AND NOT forceW% THEN
      encoder.emit32ModImm(&H08, S%, rn%, rd%, imm%)
    ELSEIF imm% <= 4095 THEN
      encoder.emit32AddImm12(0, S%, rn%, rd%, imm%)
    ELSE
      showError "ADD: immediate " + STR$(imm%) + " not encodable"
    END IF
    EXIT SUB
  END IF

  ' Rd, Rn, Rm [, shift #n]
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "ADD: expected Rm" : EXIT SUB
  rm% = tok.val%(i%) : INC i%
  sh% = SHIFT_NONE : shimm% = 0
  IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  IF i% < tok.count% AND tok.type%(i%) = TT_SHIFT THEN
    sh% = tok.val%(i%) : INC i%
    IF i% < tok.count% AND tok.type%(i%) = TT_IMM THEN shimm% = tok.val%(i%) : INC i%
  END IF

  ' ADDS Rd,Rn,Rm T1 16-bit: all R0-R7, no shift, sets S
  IF S% AND rd% <= 7 AND rn% <= 7 AND rm% <= 7 AND sh% = SHIFT_NONE AND sz% <> SIZE_W THEN
    emit16(&H1800 OR (rm% << 6) OR (rn% << 3) OR rd%)
    EXIT SUB
  END IF
  ' 32-bit
  IF sz% = SIZE_N THEN showError "ADD: hi-reg or shift requires 32-bit" : EXIT SUB
  encoder.emit32RegDp(&H08, S%, rn%, rd%, rm%, sh%, shimm%)
END SUB

' ============================================================
' SUB / SUBS
' ============================================================
SUB encoder.doSub(op%, fl%, cc%, sz%)
  LOCAL i%, rd%, rn%, rm%, imm%, S%, sh%, shimm%, ok%, forceW%
  S% = (fl% = FLAG_S)
  forceW% = (tok.str$(0) = "SUBW")
  i% = op%

  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "SUB: expected Rd" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF i% >= tok.count% THEN showError "SUB: missing operand" : EXIT SUB

  ' 2-operand: SUB Rd, #imm or SUB Rd, Rm
  IF tok.type%(i%) = TT_IMM THEN
    imm% = tok.val%(i%) AND &HFFFFFFFF : INC i%
    ' SUB SP, #imm*4 T1 16-bit
    IF rd% = 13 AND NOT S% AND (imm% AND 3) = 0 AND imm% <= 508 AND sz% <> SIZE_W THEN
      emit16(&HB080 OR ((imm% / 4) AND &H7F))
      EXIT SUB
    END IF
    ' SUBS Rdn, #imm8 T2 16-bit
    IF S% AND rd% <= 7 AND imm% <= 255 AND sz% <> SIZE_W THEN
      emit16(&H3800 OR (rd% << 8) OR imm%)
      EXIT SUB
    END IF
    ' 32-bit
    IF sz% = SIZE_N THEN showError "SUB: imm too large for 16-bit" : EXIT SUB
    ok% = 0 : LOCAL imm12a%
    imm12a% = encoder.modImm12%(imm%, ok%)
    IF ok% AND NOT forceW% THEN
      encoder.emit32ModImm(&H0D, S%, rd%, rd%, imm%)
    ELSEIF imm% <= 4095 THEN
      encoder.emit32AddImm12(1, S%, rd%, rd%, imm%)
    ELSE
      showError "SUB: immediate " + STR$(imm%) + " not encodable"
    END IF
    EXIT SUB
  END IF

  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "SUB: expected register" : EXIT SUB
  rn% = tok.val%(i%) : INC i%

  IF i% >= tok.count% OR tok.type%(i%) <> TT_COMMA THEN
    ' 2-operand SUB Rd, Rm → treat as SUB Rd, Rd, Rm (32-bit only)
    encoder.emit32RegDp(&H0D, S%, rd%, rd%, rn%, SHIFT_NONE, 0)
    EXIT SUB
  END IF

  INC i%  ' skip comma

  IF tok.type%(i%) = TT_IMM THEN
    imm% = tok.val%(i%) AND &HFFFFFFFF : INC i%
    ' SUB SP, SP, #imm7*4 T1 16-bit: Rd=Rn=SP, imm=multiple of 4, 0..508, no S
    IF NOT S% AND rd% = 13 AND rn% = 13 AND (imm% AND 3) = 0 AND imm% <= 508 AND sz% <> SIZE_W THEN
      emit16(&HB080 OR ((imm% / 4) AND &H7F))
      EXIT SUB
    END IF
    ' SUBS Rdn, #imm8 T2 (preferred when Rd=Rn, covers imm<=255)
    IF S% AND rd% = rn% AND rd% <= 7 AND imm% <= 255 AND sz% <> SIZE_W THEN
      emit16(&H3800 OR (rd% << 8) OR imm%)
      EXIT SUB
    END IF
    ' SUBS Rd, Rn, #imm3 T1 16-bit (3-operand, Rd≠Rn or imm>7 not applicable)
    IF S% AND rd% <= 7 AND rn% <= 7 AND imm% <= 7 AND sz% <> SIZE_W THEN
      emit16(&H1E00 OR (imm% << 6) OR (rn% << 3) OR rd%)
      EXIT SUB
    END IF
    IF sz% = SIZE_N THEN showError "SUB: imm too large for 16-bit" : EXIT SUB
    ok% = 0 : LOCAL imm12b%
    imm12b% = encoder.modImm12%(imm%, ok%)
    IF ok% AND NOT forceW% THEN
      encoder.emit32ModImm(&H0D, S%, rn%, rd%, imm%)
    ELSEIF imm% <= 4095 THEN
      encoder.emit32AddImm12(1, S%, rn%, rd%, imm%)
    ELSE
      showError "SUB: immediate " + STR$(imm%) + " not encodable"
    END IF
    EXIT SUB
  END IF

  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "SUB: expected Rm" : EXIT SUB
  rm% = tok.val%(i%) : INC i%
  sh% = SHIFT_NONE : shimm% = 0
  IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  IF i% < tok.count% AND tok.type%(i%) = TT_SHIFT THEN
    sh% = tok.val%(i%) : INC i%
    IF i% < tok.count% AND tok.type%(i%) = TT_IMM THEN shimm% = tok.val%(i%) : INC i%
  END IF

  ' SUBS Rd,Rn,Rm T1 16-bit: all R0-R7, no shift
  IF S% AND rd% <= 7 AND rn% <= 7 AND rm% <= 7 AND sh% = SHIFT_NONE AND sz% <> SIZE_W THEN
    emit16(&H1A00 OR (rm% << 6) OR (rn% << 3) OR rd%)
    EXIT SUB
  END IF
  IF sz% = SIZE_N THEN showError "SUB: hi-reg/shift requires 32-bit" : EXIT SUB
  encoder.emit32RegDp(&H0D, S%, rn%, rd%, rm%, sh%, shimm%)
END SUB

' ============================================================
' ADC / SBC
' ============================================================
SUB encoder.doAdcSbc(op%, fl%, cc%, sz%, isSbc%)
  LOCAL i%, rd%, rn%, rm%, imm%, S%, sh%, shimm%, op1_5%, dp16%
  S% = (fl% = FLAG_S)
  i% = op%
  IF isSbc% THEN op1_5% = &H0B ELSE op1_5% = &H0A
  IF isSbc% THEN dp16% = &H4180 ELSE dp16% = &H4140

  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "ADC/SBC: expected Rd" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%

  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "ADC/SBC: expected Rn" : EXIT SUB
  rn% = tok.val%(i%) : INC i%

  IF i% >= tok.count% OR tok.type%(i%) <> TT_COMMA THEN
    ' 2-operand: ADC Rdn, Rm  (Rd=Rn)
    rm% = rn%
    IF S% AND rd% <= 7 AND rm% <= 7 AND sz% <> SIZE_W THEN
      emit16(dp16% OR (rm% << 3) OR rd%)
    ELSE
      encoder.emit32RegDp(op1_5%, S%, rd%, rd%, rm%, SHIFT_NONE, 0)
    END IF
    EXIT SUB
  END IF

  INC i%  ' skip comma
  IF tok.type%(i%) = TT_IMM THEN
    imm% = tok.val%(i%)
    IF sz% = SIZE_N THEN showError "ADC/SBC: imm requires 32-bit" : EXIT SUB
    encoder.emit32ModImm(op1_5%, S%, rn%, rd%, imm%)
    EXIT SUB
  END IF

  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "ADC/SBC: expected Rm" : EXIT SUB
  rm% = tok.val%(i%) : INC i%
  sh% = SHIFT_NONE : shimm% = 0
  IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  IF i% < tok.count% AND tok.type%(i%) = TT_SHIFT THEN
    sh% = tok.val%(i%) : INC i%
    IF i% < tok.count% AND tok.type%(i%) = TT_IMM THEN shimm% = tok.val%(i%) : INC i%
  END IF
  IF S% AND rd% = rn% AND rd% <= 7 AND rm% <= 7 AND sh% = SHIFT_NONE AND sz% <> SIZE_W THEN
    emit16(dp16% OR (rm% << 3) OR rd%)
  ELSE
    encoder.emit32RegDp(op1_5%, S%, rn%, rd%, rm%, sh%, shimm%)
  END IF
END SUB

' ============================================================
' RSB / RSBS
' ============================================================
SUB encoder.doRsb(op%, fl%, cc%, sz%)
  LOCAL i%, rd%, rn%, rm%, imm%, S%, sh%, shimm%
  S% = (fl% = FLAG_S)
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "RSB: expected Rd" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "RSB: expected Rn" : EXIT SUB
  rn% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF tok.type%(i%) = TT_IMM THEN
    imm% = tok.val%(i%)
    ' RSB T1 16-bit: 0100 0010 01 Rn Rd (negs — only #0 immediate)
    IF S% AND imm% = 0 AND rd% <= 7 AND rn% <= 7 AND sz% <> SIZE_W THEN
      emit16(&H4240 OR (rn% << 3) OR rd%)
    ELSE
      encoder.emit32ModImm(&H0E, S%, rn%, rd%, imm%)
    END IF
  ELSEIF tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI THEN
    rm% = tok.val%(i%) : INC i%
    sh% = SHIFT_NONE : shimm% = 0
    IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
    IF i% < tok.count% AND tok.type%(i%) = TT_SHIFT THEN
      sh% = tok.val%(i%) : INC i%
      IF i% < tok.count% AND tok.type%(i%) = TT_IMM THEN shimm% = tok.val%(i%) : INC i%
    END IF
    encoder.emit32RegDp(&H0E, S%, rn%, rd%, rm%, sh%, shimm%)
  ELSE
    showError "RSB: bad operand"
  END IF
END SUB

' ============================================================
' AND/ORR/EOR/BIC/ORN  — bitwise operations
' subOp%: 0=AND, 1=ORR, 2=EOR, 3=BIC, 4=ORN
' ============================================================
SUB encoder.doBitwise(op%, fl%, cc%, sz%, subOp%)
  LOCAL i%, rd%, rn%, rm%, imm%, S%, sh%, shimm%
  LOCAL dp16%, op1_5%
  S% = (fl% = FLAG_S)
  i% = op%
  ' 16-bit data-processing opcodes:  AND=0000, EOR=0001, ORR=1100, BIC=1110
  ' 32-bit op1_5: AND=00000, BIC=00001, ORR=00010, ORN=00011, EOR=00100
  SELECT CASE subOp%
    CASE 0 : dp16% = &H4000 : op1_5% = &H00  ' AND
    CASE 1 : dp16% = &H4300 : op1_5% = &H02  ' ORR
    CASE 2 : dp16% = &H4040 : op1_5% = &H04  ' EOR
    CASE 3 : dp16% = &H4380 : op1_5% = &H01  ' BIC
    CASE 4 : dp16% = -1     : op1_5% = &H03  ' ORN (no 16-bit form)
  END SELECT

  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "Bitwise: expected Rd" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%

  ' After Rd: could be Rn then comma then op2, or direct imm (2-operand: Rdn, #imm)
  IF tok.type%(i%) = TT_IMM THEN
    imm% = tok.val%(i%)
    IF sz% = SIZE_N THEN showError "Bitwise: imm requires 32-bit" : EXIT SUB
    encoder.emit32ModImm(op1_5%, S%, rd%, rd%, imm%)
    EXIT SUB
  END IF

  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "Bitwise: expected register" : EXIT SUB
  rn% = tok.val%(i%) : INC i%

  IF i% >= tok.count% OR tok.type%(i%) <> TT_COMMA THEN
    ' 2-operand: Rd, Rn  →  Rd = Rd op Rn
    rm% = rn%
    IF S% AND dp16% >= 0 AND rd% <= 7 AND rm% <= 7 AND sz% <> SIZE_W THEN
      emit16(dp16% OR (rm% << 3) OR rd%)
    ELSE
      encoder.emit32RegDp(op1_5%, S%, rd%, rd%, rm%, SHIFT_NONE, 0)
    END IF
    EXIT SUB
  END IF

  INC i%  ' skip comma

  IF tok.type%(i%) = TT_IMM THEN
    imm% = tok.val%(i%)
    ' ORN with Rn=1111 = MVN, but we handle that separately
    ' For all: 32-bit modified immediate
    IF sz% = SIZE_N THEN showError "Bitwise: imm requires 32-bit" : EXIT SUB
    encoder.emit32ModImm(op1_5%, S%, rn%, rd%, imm%)
    EXIT SUB
  END IF

  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "Bitwise: expected Rm" : EXIT SUB
  rm% = tok.val%(i%) : INC i%
  sh% = SHIFT_NONE : shimm% = 0
  IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  IF i% < tok.count% AND tok.type%(i%) = TT_SHIFT THEN
    sh% = tok.val%(i%) : INC i%
    IF i% < tok.count% AND tok.type%(i%) = TT_IMM THEN shimm% = tok.val%(i%) : INC i%
  END IF
  ' 16-bit: 2-reg, rd=rn, lo regs, no shift, S set
  IF S% AND dp16% >= 0 AND rd% = rn% AND rd% <= 7 AND rm% <= 7 AND sh% = SHIFT_NONE AND sz% <> SIZE_W THEN
    emit16(dp16% OR (rm% << 3) OR rd%)
  ELSE
    encoder.emit32RegDp(op1_5%, S%, rn%, rd%, rm%, sh%, shimm%)
  END IF
END SUB

' ============================================================
' MUL
' ============================================================
SUB encoder.doMul(op%, fl%, cc%, sz%)
  LOCAL i%, rd%, rn%, rm%, S%
  S% = (fl% = FLAG_S)
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "MUL: expected Rd" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "MUL: expected Rn" : EXIT SUB
  rn% = tok.val%(i%) : INC i%
  IF i% >= tok.count% OR tok.type%(i%) <> TT_COMMA THEN
    rm% = rn%  ' 2-operand: MUL Rdn, Rm
  ELSE
    INC i%
    IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "MUL: expected Rm" : EXIT SUB
    rm% = tok.val%(i%)
  END IF
  ' MUL T1 16-bit: 0100 0011 01 Rn Rdm (Rdm=Rd=Rm, lo regs)
  IF S% AND rd% = rm% AND rd% <= 7 AND rn% <= 7 AND sz% <> SIZE_W THEN
    emit16(&H4340 OR (rn% << 3) OR rd%)
  ELSE
    ' MUL T2 32-bit: 1111 1011 0000 Rn | 1111 Rd 0000 Rm
    emit32(&HFB00 OR (rn% AND &HF), (&HF << 12) OR ((rd% AND &HF) << 8) OR (rm% AND &HF))
  END IF
END SUB

' ============================================================
' LSL / LSR / ASR / ROR
' shType%: SHIFT_LSL=1, SHIFT_LSR=2, SHIFT_ASR=3, SHIFT_ROR=4
' ============================================================
SUB encoder.doShift(op%, fl%, cc%, sz%, shType%)
  LOCAL i%, rd%, rm%, rs%, imm%, S%, dp16op%
  LOCAL opc5i%, type2si%, im3si%, im2si%
  LOCAL op1r%
  LOCAL opc5%, type2s%, im3s%, im2s%
  LOCAL op1rs%
  S% = (fl% = FLAG_S)
  i% = op%
  ' 16-bit data-processing shift ops: LSL=0010, LSR=0011, ASR=0100, ROR=0111
  SELECT CASE shType%
    CASE SHIFT_LSL : dp16op% = &H4080  ' LSLS T2 (reg)
    CASE SHIFT_LSR : dp16op% = &H40C0  ' LSRS T2
    CASE SHIFT_ASR : dp16op% = &H4100  ' ASRS T2
    CASE SHIFT_ROR : dp16op% = &H41C0  ' RORS T2
    CASE ELSE      : dp16op% = -1
  END SELECT

  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "Shift: expected Rd" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%

  ' 2-operand shift-by-immediate: LSLS Rdn, #n  (no second register)
  IF tok.type%(i%) = TT_IMM THEN
    imm% = tok.val%(i%) : INC i%
    rm% = rd%
    ' Fall through to 3-operand immediate handling below via a goto-like jump...
    ' Actually inline it here:
    SELECT CASE shType%
      CASE SHIFT_LSL : opc5i% = 0
      CASE SHIFT_LSR : opc5i% = 1
      CASE SHIFT_ASR : opc5i% = 2
      CASE ELSE      : opc5i% = -1
    END SELECT
    IF S% AND opc5i% >= 0 AND rd% <= 7 AND rm% <= 7 AND imm% >= 0 AND imm% <= 31 AND sz% <> SIZE_W THEN
      emit16((opc5i% << 11) OR (imm% << 6) OR (rm% << 3) OR rd%)
    ELSE
      IF shType% = SHIFT_ROR THEN type2si% = 3 ELSE type2si% = shType% - 1
      im3si% = (imm% >> 2) AND 7 : im2si% = imm% AND 3
      emit32(&HEA4F OR (S% << 4), (im3si% << 12) OR ((rd% AND &HF) << 8) OR (im2si% << 6) OR (type2si% << 4) OR (rm% AND &HF))
    END IF
    EXIT SUB
  END IF

  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "Shift: expected Rm/Rdn" : EXIT SUB
  rm% = tok.val%(i%) : INC i%

  IF i% >= tok.count% OR tok.type%(i%) <> TT_COMMA THEN
    ' 2-operand shift-by-register: LSLS Rd, Rm  (Rd is also dest, same as Rd+=lsl Rm)
    rs% = rm%   ' shift amount register
    rm% = rd%   ' source = dest (Rdn form)
    IF S% AND dp16op% >= 0 AND rd% <= 7 AND rs% <= 7 AND sz% <> SIZE_W THEN
      emit16(dp16op% OR (rs% << 3) OR rd%)
    ELSE
      ' T2 32-bit shift-by-register
      SELECT CASE shType%
        CASE SHIFT_LSL : op1r% = 0
        CASE SHIFT_LSR : op1r% = 1
        CASE SHIFT_ASR : op1r% = 2
        CASE SHIFT_ROR : op1r% = 3
      END SELECT
      emit32(&HFA00 OR (op1r% << 5) OR (S% << 4) OR (rm% AND &HF), (&HF << 12) OR ((rd% AND &HF) << 8) OR (rs% AND &HF))
    END IF
    EXIT SUB
  END IF

  ' 3-operand: Rd, Rm, #imm  or  Rd, Rm, Rs
  ' Check if rd=rm (2-operand source) — the common case after parsing shows 3 tokens
  INC i%  ' skip comma
  IF tok.type%(i%) = TT_IMM THEN
    imm% = tok.val%(i%)
    ' Shift-by-immediate
    ' LSLS/LSRS/ASRS T1 16-bit: 000 opc imm5 Rm Rd
    SELECT CASE shType%
      CASE SHIFT_LSL : opc5% = 0
      CASE SHIFT_LSR : opc5% = 1
      CASE SHIFT_ASR : opc5% = 2
      CASE ELSE      : opc5% = -1
    END SELECT
    IF S% AND opc5% >= 0 AND rd% <= 7 AND rm% <= 7 AND imm% >= 0 AND imm% <= 31 AND sz% <> SIZE_W THEN
      emit16((opc5% << 11) OR (imm% << 6) OR (rm% << 3) OR rd%)
    ELSE
      ' 32-bit shift-by-imm (MOV encoding with shift)
      IF shType% = SHIFT_ROR THEN type2s% = 3 ELSE type2s% = shType% - 1
      im3s% = (imm% >> 2) AND 7 : im2s% = imm% AND 3
      emit32(&HEA4F OR (S% << 4), (im3s% << 12) OR ((rd% AND &HF) << 8) OR (im2s% << 6) OR (type2s% << 4) OR (rm% AND &HF))
    END IF
  ELSEIF tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI THEN
    ' Shift by register: LSLS Rd, Rm, Rs
    rs% = tok.val%(i%)
    IF S% AND dp16op% >= 0 AND rd% = rm% AND rd% <= 7 AND rs% <= 7 AND sz% <> SIZE_W THEN
      emit16(dp16op% OR (rs% << 3) OR rd%)
    ELSE
      SELECT CASE shType%
        CASE SHIFT_LSL : op1rs% = 0
        CASE SHIFT_LSR : op1rs% = 1
        CASE SHIFT_ASR : op1rs% = 2
        CASE SHIFT_ROR : op1rs% = 3
      END SELECT
      emit32(&HFA00 OR (op1rs% << 5) OR (S% << 4) OR (rm% AND &HF), (&HF << 12) OR ((rd% AND &HF) << 8) OR (rs% AND &HF))
    END IF
  ELSE
    showError "Shift: expected immediate or register"
  END IF
END SUB

' RRX
SUB encoder.doRrx(op%, fl%, cc%, sz%)
  LOCAL i%, rd%, rm%, S%
  S% = (fl% = FLAG_S)
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "RRX: expected Rd" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN rm% = rd% ELSE rm% = tok.val%(i%)
  ' RRX T1: 1110 1010 0100 1111 | 0000 Rd 0011 Rm
  emit32(&HEA4F OR (S% << 4), ((rd% AND &HF) << 8) OR &H30 OR (rm% AND &HF))
END SUB

' ============================================================
' CMP / CMN / TST / TEQ
' ============================================================
SUB encoder.doCmp(op%, fl%, cc%, sz%)
  LOCAL i%, rn%, rm%, imm%, sh%, shimm%
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "CMP: expected Rn" : EXIT SUB
  rn% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF tok.type%(i%) = TT_IMM THEN
    imm% = tok.val%(i%)
    IF rn% <= 7 AND imm% <= 255 AND sz% <> SIZE_W THEN
      emit16(&H2800 OR (rn% << 8) OR imm%)
    ELSE
      encoder.emit32ModImm(&H0D, 1, rn%, &HF, imm%)
    END IF
  ELSEIF tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI THEN
    rm% = tok.val%(i%) : INC i%
    sh% = SHIFT_NONE : shimm% = 0
    IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
    IF i% < tok.count% AND tok.type%(i%) = TT_SHIFT THEN
      sh% = tok.val%(i%) : INC i%
      IF i% < tok.count% AND tok.type%(i%) = TT_IMM THEN shimm% = tok.val%(i%) : INC i%
    END IF
    IF rn% <= 7 AND rm% <= 7 AND sh% = SHIFT_NONE AND sz% <> SIZE_W THEN
      emit16(&H4280 OR (rm% << 3) OR rn%)  ' CMP T1
    ELSEIF sh% = SHIFT_NONE AND sz% <> SIZE_W THEN
      emit16(&H4500 OR ((rn% >> 3) << 7) OR ((rm% AND &HF) << 3) OR (rn% AND 7))  ' CMP T2 hi-reg
    ELSE
      encoder.emit32RegDp(&H0D, 1, rn%, &HF, rm%, sh%, shimm%)
    END IF
  ELSE
    showError "CMP: bad operand"
  END IF
END SUB

SUB encoder.doCmn(op%, fl%, cc%, sz%)
  LOCAL i%, rn%, rm%, imm%, sh%, shimm%
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "CMN: expected Rn" : EXIT SUB
  rn% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF tok.type%(i%) = TT_IMM THEN
    imm% = tok.val%(i%)
    encoder.emit32ModImm(&H08, 1, rn%, &HF, imm%)
  ELSEIF tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI THEN
    rm% = tok.val%(i%) : INC i%
    sh% = SHIFT_NONE : shimm% = 0
    IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
    IF i% < tok.count% AND tok.type%(i%) = TT_SHIFT THEN
      sh% = tok.val%(i%) : INC i%
      IF i% < tok.count% AND tok.type%(i%) = TT_IMM THEN shimm% = tok.val%(i%) : INC i%
    END IF
    IF rn% <= 7 AND rm% <= 7 AND sh% = SHIFT_NONE AND sz% <> SIZE_W THEN
      emit16(&H42C0 OR (rm% << 3) OR rn%)
    ELSE
      encoder.emit32RegDp(&H08, 1, rn%, &HF, rm%, sh%, shimm%)
    END IF
  ELSE
    showError "CMN: bad operand"
  END IF
END SUB

SUB encoder.doTst(op%, fl%, cc%, sz%)
  LOCAL i%, rn%, rm%, imm%, sh%, shimm%
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "TST: expected Rn" : EXIT SUB
  rn% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF tok.type%(i%) = TT_IMM THEN
    imm% = tok.val%(i%)
    encoder.emit32ModImm(&H00, 1, rn%, &HF, imm%)
  ELSEIF tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI THEN
    rm% = tok.val%(i%) : INC i%
    sh% = SHIFT_NONE : shimm% = 0
    IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
    IF i% < tok.count% AND tok.type%(i%) = TT_SHIFT THEN
      sh% = tok.val%(i%) : INC i%
      IF i% < tok.count% AND tok.type%(i%) = TT_IMM THEN shimm% = tok.val%(i%) : INC i%
    END IF
    IF rn% <= 7 AND rm% <= 7 AND sh% = SHIFT_NONE AND sz% <> SIZE_W THEN
      emit16(&H4200 OR (rm% << 3) OR rn%)
    ELSE
      encoder.emit32RegDp(&H00, 1, rn%, &HF, rm%, sh%, shimm%)
    END IF
  ELSE
    showError "TST: bad operand"
  END IF
END SUB

SUB encoder.doTeq(op%, fl%, cc%, sz%)
  LOCAL i%, rn%, rm%, imm%, sh%, shimm%
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "TEQ: expected Rn" : EXIT SUB
  rn% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF tok.type%(i%) = TT_IMM THEN
    imm% = tok.val%(i%)
    encoder.emit32ModImm(&H04, 1, rn%, &HF, imm%)
  ELSEIF tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI THEN
    rm% = tok.val%(i%) : INC i%
    sh% = SHIFT_NONE : shimm% = 0
    IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
    IF i% < tok.count% AND tok.type%(i%) = TT_SHIFT THEN
      sh% = tok.val%(i%) : INC i%
      IF i% < tok.count% AND tok.type%(i%) = TT_IMM THEN shimm% = tok.val%(i%) : INC i%
    END IF
    encoder.emit32RegDp(&H04, 1, rn%, &HF, rm%, sh%, shimm%)
  ELSE
    showError "TEQ: bad operand"
  END IF
END SUB

' ============================================================
' LDR  — full dispatcher (literal, imm offset, reg offset)
' ============================================================
' Emit a T4 (8-bit imm) load/store: LDR/STR/LDRB/STRB/LDRH/STRH/LDRSB/LDRSH.
' opc32hi_t3 is the T3 12-bit hw0 base (e.g. &HF8D0 for LDR); the T4 form
' is the same with bit 7 cleared. P,W per ARM ARM addressing mode:
'   offset (negative imm only): P=1, W=0
'   pre-index `[Rn,#imm]!`:      P=1, W=1
'   post-index `[Rn],#imm`:      P=0, W=1
SUB encoder.emitLdstT4(opc32hi_t3%, rn%, rt%, imm%, P%, W%)
  LOCAL u%, mag%
  IF imm% < 0 OR imm% > &H7FFFFFFF THEN
    u% = 0
    mag% = (-imm%) AND &HFFFFFFFF
  ELSE
    u% = 1
    mag% = imm%
  END IF
  IF mag% > 255 THEN showError "LDR/STR: T4 imm out of range (-255..255)" : EXIT SUB
  emit32((opc32hi_t3% AND &HFF7F) OR (rn% AND &HF), ((rt% AND &HF) << 12) OR &H800 OR (P% << 10) OR (u% << 9) OR (W% << 8) OR (mag% AND &HFF))
END SUB

SUB encoder.doLdr(op%, fl%, cc%, sz%)
  LOCAL i%, rt%, rn%, rm%, imm%, sh%, shimm%, addr%, litOff%
  LOCAL litVal%, ok32%, imm4l%, i16l%, imm3l%, imm8l%
  LOCAL dummy12%
  LOCAL preBang%
  LOCAL pcU%, pcMag%
  LOCAL type2l%
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "LDR: expected Rt" : EXIT SUB
  rt% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%

  ' LDR Rt,=value  →  MOV.W / MOVW when possible, else literal pool
  IF tok.type%(i%) = TT_LIT THEN
    litVal% = tok.val%(i%)
    ' Try MOV.W with modified immediate (covers 0..255 and many other values)
    ok32% = 0
    dummy12% = encoder.modImm12%(litVal%, ok32%)
    IF ok32% THEN
      encoder.emit32ModImm(&H02, 0, &HF, rt%, litVal%)
      EXIT SUB
    END IF
    ' Try MOVW for 16-bit values (0..65535)
    IF (litVal% AND &HFFFF0000) = 0 THEN
      i16l%  = litVal% AND &HFFFF
      imm4l% = (i16l% >> 12) AND &HF
      i16l%  = (litVal% >> 11) AND 1
      imm3l% = (litVal% >> 8)  AND 7
      imm8l% = litVal% AND &HFF
      emit32(&HF240 OR (i16l% << 10) OR imm4l%, (imm3l% << 12) OR ((rt% AND &HF) << 8) OR imm8l%)
      EXIT SUB
    END IF
    ' Literal pool: 32-bit T3 placeholder for .W or high regs, else 16-bit T1.
    litOff% = io.ptr%
    IF sz% = SIZE_W OR rt% > 7 THEN
      emit32(&HF8DF, (rt% AND &HF) << 12)
    ELSE
      emit16(&H4800 OR ((rt% AND 7) << 8))
    END IF
    addLit(litVal%, litOff%)
    EXIT SUB
  END IF

  ' LDR Rt,[Rn,...]
  IF tok.type%(i%) <> TT_LBRA THEN showError "LDR: expected [ or =" : EXIT SUB
  INC i%

  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "LDR: expected base register" : EXIT SUB
  rn% = tok.val%(i%) : INC i%

  IF tok.type%(i%) = TT_RBRA THEN
    ' LDR Rt,[Rn]  →  imm=0, but check for post-index `,#imm`
    INC i%
    IF i% + 1 < tok.count% AND tok.type%(i%) = TT_COMMA AND tok.type%(i% + 1) = TT_IMM THEN
      INC i%
      imm% = tok.val%(i%) : INC i%
      encoder.emitLdstT4(&HF8D0, rn%, rt%, imm%, 0, 1)
      EXIT SUB
    END IF
    imm% = 0
    sh% = SHIFT_NONE : shimm% = 0
    IF rn% = 15 THEN   ' PC-relative, no displacement
      IF rt% <= 7 AND sz% <> SIZE_W THEN
        emit16(&H4800 OR (rt% << 8))   ' placeholder offset=0; literal pool handles this
      ELSE
        emit32(&HF8DF, ((rt% AND &HF) << 12))
      END IF
      EXIT SUB
    END IF
    ' LDR Rt,[Rn,#0]
    IF rt% <= 7 AND rn% <= 7 AND sz% <> SIZE_W THEN
      emit16(&H6800 OR (0 << 6) OR (rn% << 3) OR rt%)  ' imm5=0
    ELSE
      emit32(&HF8D0 OR (rn% AND &HF), ((rt% AND &HF) << 12))
    END IF
    EXIT SUB
  END IF

  IF tok.type%(i%) = TT_COMMA THEN INC i%

  IF tok.type%(i%) = TT_IMM OR tok.type%(i%) = TT_ADDR THEN
    IF tok.type%(i%) = TT_ADDR THEN imm% = lookupSym%(tok.str$(i%)) ELSE imm% = tok.val%(i%)
    INC i%
    preBang% = 0
    IF tok.type%(i%) = TT_RBRA THEN
      INC i%
      IF i% < tok.count% AND tok.type%(i%) = TT_BANG THEN preBang% = 1 : INC i%
    END IF
    ' Pre-index or negative offset → T4 8-bit imm
    IF preBang% THEN
      encoder.emitLdstT4(&HF8D0, rn%, rt%, imm%, 1, 1)
      EXIT SUB
    END IF
    IF imm% > &H7FFFFFFF THEN
      encoder.emitLdstT4(&HF8D0, rn%, rt%, imm%, 1, 0)
      EXIT SUB
    END IF

    IF rn% = 15 THEN   ' LDR Rt,[PC,#imm]  T1 16-bit / T2 32-bit literal
      IF rt% <= 7 AND (imm% AND 3) = 0 AND imm% >= 0 AND imm% <= 1020 AND sz% <> SIZE_W THEN
        emit16(&H4800 OR (rt% << 8) OR (imm% / 4))
      ELSE
        IF imm% < 0 OR imm% > &H7FFFFFFF THEN
          pcU% = 0
          pcMag% = (-imm%) AND &HFFFFFFFF
        ELSE
          pcU% = 1
          pcMag% = imm%
        END IF
        IF pcMag% > 4095 THEN showError "LDR PC-rel: imm out of range (-4095..4095)" : EXIT SUB
        emit32(&HF85F OR (pcU% << 7), ((rt% AND &HF) << 12) OR (pcMag% AND &HFFF))
      END IF
      EXIT SUB
    END IF
    IF rn% = 13 THEN   ' LDR Rt,[SP,#imm] T2 16-bit
      IF rt% <= 7 AND (imm% AND 3) = 0 AND imm% >= 0 AND imm% <= 1020 AND sz% <> SIZE_W THEN
        emit16(&H9800 OR (rt% << 8) OR (imm% / 4))
        EXIT SUB
      END IF
    END IF
    IF rt% <= 7 AND rn% <= 7 AND (imm% AND 3) = 0 AND imm% >= 0 AND imm% <= 124 AND sz% <> SIZE_W THEN
      emit16(&H6800 OR ((imm% / 4) << 6) OR (rn% << 3) OR rt%)
    ELSE
      emit32(&HF8D0 OR (rn% AND &HF), ((rt% AND &HF) << 12) OR (imm% AND &HFFF))
    END IF
    EXIT SUB
  END IF

  IF tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI THEN
    rm% = tok.val%(i%) : INC i%
    sh% = SHIFT_NONE : shimm% = 0
    IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
    IF i% < tok.count% AND tok.type%(i%) = TT_SHIFT THEN
      sh% = tok.val%(i%) : INC i%
      IF i% < tok.count% AND tok.type%(i%) = TT_IMM THEN shimm% = tok.val%(i%) : INC i%
    END IF
    IF tok.type%(i%) = TT_RBRA THEN INC i%
    IF rt% <= 7 AND rn% <= 7 AND rm% <= 7 AND sh% = SHIFT_NONE AND sz% <> SIZE_W THEN
      emit16(&H5800 OR (rm% << 6) OR (rn% << 3) OR rt%)
    ELSE
      IF sh% = SHIFT_NONE THEN type2l% = 0 : shimm% = 0 ELSE type2l% = sh% - 1
      emit32(&HF850 OR (rn% AND &HF), ((rt% AND &HF) << 12) OR (shimm% << 4) OR (type2l% << 4) OR (rm% AND &HF))
    END IF
    EXIT SUB
  END IF
  showError "LDR: bad addressing mode"
END SUB

' ============================================================
' LDRB / LDRH / LDRSB / LDRSH  (size%: 1=byte, 2=half; signed%: 0/1)
' ============================================================
SUB encoder.doLdrx(op%, fl%, cc%, sz%, size%, signed%)
  LOCAL i%, rt%, rn%, rm%, imm%, sh%, shimm%
  LOCAL opc16%, opc32hi%
  LOCAL ldrxPreBang%
  i% = op%
  ' 16-bit register-offset opcodes: 0101 opc Rm Rn Rt
  ' STRB=010, LDRB=110, STRH=001, LDRH=101, LDRSB=011, LDRSH=111
  ' 16-bit imm-offset:  LDRB: 01111 imm5 Rn Rt; LDRH: 10001 imm5 Rn Rt
  SELECT CASE size% * 2 + signed%
    CASE 2 : opc16% = &H5C00 : opc32hi% = &HF890  ' LDRB  T1
    CASE 3 : opc16% = &H5600 : opc32hi% = &HF990  ' LDRSB T1
    CASE 4 : opc16% = &H5A00 : opc32hi% = &HF8B0  ' LDRH  T1
    CASE 5 : opc16% = &H5E00 : opc32hi% = &HF9B0  ' LDRSH T1
  END SELECT

  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "LDRx: expected Rt" : EXIT SUB
  rt% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF tok.type%(i%) <> TT_LBRA THEN showError "LDRx: expected [" : EXIT SUB
  INC i%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "LDRx: expected Rn" : EXIT SUB
  rn% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%

  IF tok.type%(i%) = TT_RBRA OR i% >= tok.count% THEN
    IF tok.type%(i%) = TT_RBRA THEN INC i%
    ' Post-index `[Rn],#imm`
    IF i% + 1 < tok.count% AND tok.type%(i%) = TT_COMMA AND tok.type%(i% + 1) = TT_IMM THEN
      INC i%
      imm% = tok.val%(i%) : INC i%
      encoder.emitLdstT4(opc32hi%, rn%, rt%, imm%, 0, 1)
      EXIT SUB
    END IF
    imm% = 0
    IF size% = 1 AND rt% <= 7 AND rn% <= 7 AND sz% <> SIZE_W THEN
      emit16(&H7800 OR (0 << 6) OR (rn% << 3) OR rt%)  ' LDRB imm5=0
    ELSEIF size% = 2 AND rt% <= 7 AND rn% <= 7 AND sz% <> SIZE_W THEN
      emit16(&H8800 OR (0 << 6) OR (rn% << 3) OR rt%)  ' LDRH imm5=0
    ELSE
      emit32(opc32hi% OR (rn% AND &HF), ((rt% AND &HF) << 12))
    END IF
    EXIT SUB
  END IF

  IF tok.type%(i%) = TT_IMM THEN
    imm% = tok.val%(i%) : INC i%
    ldrxPreBang% = 0
    IF tok.type%(i%) = TT_RBRA THEN
      INC i%
      IF i% < tok.count% AND tok.type%(i%) = TT_BANG THEN ldrxPreBang% = 1 : INC i%
    END IF
    IF ldrxPreBang% THEN
      encoder.emitLdstT4(opc32hi%, rn%, rt%, imm%, 1, 1)
      EXIT SUB
    END IF
    IF imm% > &H7FFFFFFF THEN
      encoder.emitLdstT4(opc32hi%, rn%, rt%, imm%, 1, 0)
      EXIT SUB
    END IF
    IF size% = 1 AND rt% <= 7 AND rn% <= 7 AND imm% >= 0 AND imm% <= 31 AND sz% <> SIZE_W THEN
      emit16(&H7800 OR (imm% << 6) OR (rn% << 3) OR rt%)
    ELSEIF size% = 2 AND rt% <= 7 AND rn% <= 7 AND (imm% AND 1) = 0 AND imm% >= 0 AND imm% <= 62 AND sz% <> SIZE_W THEN
      emit16(&H8800 OR ((imm% / 2) << 6) OR (rn% << 3) OR rt%)
    ELSE
      emit32(opc32hi% OR (rn% AND &HF), ((rt% AND &HF) << 12) OR (imm% AND &HFFF))
    END IF
    EXIT SUB
  END IF

  IF tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI THEN
    rm% = tok.val%(i%) : INC i%
    sh% = SHIFT_NONE : shimm% = 0
    IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
    IF i% < tok.count% AND tok.type%(i%) = TT_SHIFT THEN
      sh% = tok.val%(i%) : INC i%
      IF i% < tok.count% AND tok.type%(i%) = TT_IMM THEN shimm% = tok.val%(i%) : INC i%
    END IF
    IF tok.type%(i%) = TT_RBRA THEN INC i%
    IF rt% <= 7 AND rn% <= 7 AND rm% <= 7 AND sh% = SHIFT_NONE AND sz% <> SIZE_W THEN
      emit16(opc16% OR (rm% << 6) OR (rn% << 3) OR rt%)
    ELSE
      emit32((opc32hi% AND &HFF7F) OR (rn% AND &HF), ((rt% AND &HF) << 12) OR (shimm% << 4) OR (rm% AND &HF))
    END IF
    EXIT SUB
  END IF
  showError "LDRx: bad addressing mode"
END SUB

' ============================================================
' STR
' ============================================================
SUB encoder.doStr(op%, fl%, cc%, sz%)
  LOCAL i%, rt%, rn%, rm%, imm%, sh%, shimm%
  LOCAL strPreBang%
  LOCAL type2t%
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "STR: expected Rt" : EXIT SUB
  rt% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF tok.type%(i%) <> TT_LBRA THEN showError "STR: expected [" : EXIT SUB
  INC i%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "STR: expected Rn" : EXIT SUB
  rn% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%

  IF tok.type%(i%) = TT_RBRA OR i% >= tok.count% THEN
    IF tok.type%(i%) = TT_RBRA THEN INC i%
    ' Post-index `[Rn],#imm`
    IF i% + 1 < tok.count% AND tok.type%(i%) = TT_COMMA AND tok.type%(i% + 1) = TT_IMM THEN
      INC i%
      imm% = tok.val%(i%) : INC i%
      encoder.emitLdstT4(&HF8C0, rn%, rt%, imm%, 0, 1)
      EXIT SUB
    END IF
    IF rn% = 13 AND rt% <= 7 AND sz% <> SIZE_W THEN
      emit16(&H9000 OR (rt% << 8))   ' STR Rt,[SP,#0]
    ELSEIF rt% <= 7 AND rn% <= 7 AND sz% <> SIZE_W THEN
      emit16(&H6000 OR (rn% << 3) OR rt%)
    ELSE
      emit32(&HF8C0 OR (rn% AND &HF), ((rt% AND &HF) << 12))
    END IF
    EXIT SUB
  END IF

  IF tok.type%(i%) = TT_IMM OR tok.type%(i%) = TT_ADDR THEN
    IF tok.type%(i%) = TT_ADDR THEN imm% = lookupSym%(tok.str$(i%)) ELSE imm% = tok.val%(i%)
    INC i%
    strPreBang% = 0
    IF tok.type%(i%) = TT_RBRA THEN
      INC i%
      IF i% < tok.count% AND tok.type%(i%) = TT_BANG THEN strPreBang% = 1 : INC i%
    END IF
    IF strPreBang% THEN
      encoder.emitLdstT4(&HF8C0, rn%, rt%, imm%, 1, 1)
      EXIT SUB
    END IF
    IF imm% > &H7FFFFFFF THEN
      encoder.emitLdstT4(&HF8C0, rn%, rt%, imm%, 1, 0)
      EXIT SUB
    END IF
    IF rn% = 13 AND rt% <= 7 AND (imm% AND 3) = 0 AND imm% >= 0 AND imm% <= 1020 AND sz% <> SIZE_W THEN
      emit16(&H9000 OR (rt% << 8) OR (imm% / 4))
    ELSEIF rt% <= 7 AND rn% <= 7 AND (imm% AND 3) = 0 AND imm% >= 0 AND imm% <= 124 AND sz% <> SIZE_W THEN
      emit16(&H6000 OR ((imm% / 4) << 6) OR (rn% << 3) OR rt%)
    ELSE
      emit32(&HF8C0 OR (rn% AND &HF), ((rt% AND &HF) << 12) OR (imm% AND &HFFF))
    END IF
    EXIT SUB
  END IF

  IF tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI THEN
    rm% = tok.val%(i%) : INC i%
    sh% = SHIFT_NONE : shimm% = 0
    IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
    IF i% < tok.count% AND tok.type%(i%) = TT_SHIFT THEN
      sh% = tok.val%(i%) : INC i%
      IF i% < tok.count% AND tok.type%(i%) = TT_IMM THEN shimm% = tok.val%(i%) : INC i%
    END IF
    IF tok.type%(i%) = TT_RBRA THEN INC i%
    IF rt% <= 7 AND rn% <= 7 AND rm% <= 7 AND sh% = SHIFT_NONE AND sz% <> SIZE_W THEN
      emit16(&H5000 OR (rm% << 6) OR (rn% << 3) OR rt%)
    ELSE
      IF sh% = SHIFT_NONE THEN type2t% = 0 : shimm% = 0 ELSE type2t% = sh% - 1
      emit32(&HF840 OR (rn% AND &HF), ((rt% AND &HF) << 12) OR (shimm% << 4) OR (type2t% << 4) OR (rm% AND &HF))
    END IF
    EXIT SUB
  END IF
  showError "STR: bad addressing mode"
END SUB

' STRB / STRH  (size%: 1=byte, 2=half)
SUB encoder.doStrx(op%, fl%, cc%, sz%, size%)
  LOCAL i%, rt%, rn%, rm%, imm%, sh%, shimm%
  LOCAL opc16%, opc32hi%
  LOCAL strxPreBang%
  IF size% = 1 THEN opc16% = &H5400 ELSE opc16% = &H5200
  IF size% = 1 THEN opc32hi% = &HF880 ELSE opc32hi% = &HF8A0
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "STRx: expected Rt" : EXIT SUB
  rt% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF tok.type%(i%) <> TT_LBRA THEN showError "STRx: expected [" : EXIT SUB
  INC i%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "STRx: expected Rn" : EXIT SUB
  rn% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%

  IF tok.type%(i%) = TT_RBRA OR i% >= tok.count% THEN
    IF tok.type%(i%) = TT_RBRA THEN INC i%
    ' Post-index `[Rn],#imm`
    IF i% + 1 < tok.count% AND tok.type%(i%) = TT_COMMA AND tok.type%(i% + 1) = TT_IMM THEN
      INC i%
      imm% = tok.val%(i%) : INC i%
      encoder.emitLdstT4(opc32hi%, rn%, rt%, imm%, 0, 1)
      EXIT SUB
    END IF
    imm% = 0
    IF size% = 1 AND rt% <= 7 AND rn% <= 7 AND sz% <> SIZE_W THEN
      emit16(&H7000 OR (rn% << 3) OR rt%)
    ELSEIF size% = 2 AND rt% <= 7 AND rn% <= 7 AND sz% <> SIZE_W THEN
      emit16(&H8000 OR (rn% << 3) OR rt%)
    ELSE
      emit32(opc32hi% OR (rn% AND &HF), ((rt% AND &HF) << 12))
    END IF
    EXIT SUB
  END IF

  IF tok.type%(i%) = TT_IMM THEN
    imm% = tok.val%(i%) : INC i%
    strxPreBang% = 0
    IF tok.type%(i%) = TT_RBRA THEN
      INC i%
      IF i% < tok.count% AND tok.type%(i%) = TT_BANG THEN strxPreBang% = 1 : INC i%
    END IF
    IF strxPreBang% THEN
      encoder.emitLdstT4(opc32hi%, rn%, rt%, imm%, 1, 1)
      EXIT SUB
    END IF
    IF imm% > &H7FFFFFFF THEN
      encoder.emitLdstT4(opc32hi%, rn%, rt%, imm%, 1, 0)
      EXIT SUB
    END IF
    IF size% = 1 AND rt% <= 7 AND rn% <= 7 AND imm% >= 0 AND imm% <= 31 AND sz% <> SIZE_W THEN
      emit16(&H7000 OR (imm% << 6) OR (rn% << 3) OR rt%)
    ELSEIF size% = 2 AND rt% <= 7 AND rn% <= 7 AND (imm% AND 1) = 0 AND imm% >= 0 AND imm% <= 62 AND sz% <> SIZE_W THEN
      emit16(&H8000 OR ((imm% / 2) << 6) OR (rn% << 3) OR rt%)
    ELSE
      emit32(opc32hi% OR (rn% AND &HF), ((rt% AND &HF) << 12) OR (imm% AND &HFFF))
    END IF
    EXIT SUB
  END IF

  IF tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI THEN
    rm% = tok.val%(i%) : INC i%
    sh% = SHIFT_NONE : shimm% = 0
    IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
    IF i% < tok.count% AND tok.type%(i%) = TT_SHIFT THEN
      sh% = tok.val%(i%) : INC i%
      IF i% < tok.count% AND tok.type%(i%) = TT_IMM THEN shimm% = tok.val%(i%) : INC i%
    END IF
    IF tok.type%(i%) = TT_RBRA THEN INC i%
    IF rt% <= 7 AND rn% <= 7 AND rm% <= 7 AND sh% = SHIFT_NONE AND sz% <> SIZE_W THEN
      emit16(opc16% OR (rm% << 6) OR (rn% << 3) OR rt%)
    ELSE
      emit32((opc32hi% AND &HFF7F) OR (rn% AND &HF), ((rt% AND &HF) << 12) OR (shimm% << 4) OR (rm% AND &HF))
    END IF
    EXIT SUB
  END IF
  showError "STRx: bad addressing mode"
END SUB

' ============================================================
' LDMIA / STMIA / LDMDB / STMDB
' ============================================================
SUB encoder.doLdm(op%, fl%, cc%, sz%)
  ' LDMIA Rn{!}, {reglist}.  T1 16-bit if Rn<=7 and reglist R0-R7 and no .W.
  ' T2 32-bit hw0 base = &HE890 (W=0) or &HE8B0 (W=1, "!" writeback).
  LOCAL i%, rn%, bitmask%, w%
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "LDMIA: expected Rn" : EXIT SUB
  rn% = tok.val%(i%) : INC i%
  w% = 0
  IF tok.type%(i%) = TT_BANG THEN w% = 1 : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF tok.type%(i%) <> TT_RLIST THEN showError "LDMIA: expected register list" : EXIT SUB
  bitmask% = tok.val%(i%)
  IF rn% <= 7 AND (bitmask% AND &HFF00) = 0 AND sz% <> SIZE_W THEN
    emit16(&HC800 OR (rn% << 8) OR (bitmask% AND &HFF))
  ELSE
    emit32((&HE890 OR (w% << 5)) OR (rn% AND &HF), bitmask% AND &HFFFF)
  END IF
END SUB

SUB encoder.doStm(op%, fl%, cc%, sz%)
  ' STMIA: T2 hw0 base = &HE880 (W=0) or &HE8A0 (W=1).
  LOCAL i%, rn%, bitmask%, w%
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "STMIA: expected Rn" : EXIT SUB
  rn% = tok.val%(i%) : INC i%
  w% = 0
  IF tok.type%(i%) = TT_BANG THEN w% = 1 : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF tok.type%(i%) <> TT_RLIST THEN showError "STMIA: expected register list" : EXIT SUB
  bitmask% = tok.val%(i%)
  IF rn% <= 7 AND (bitmask% AND &HFF00) = 0 AND sz% <> SIZE_W THEN
    emit16(&HC000 OR (rn% << 8) OR (bitmask% AND &HFF))
  ELSE
    emit32((&HE880 OR (w% << 5)) OR (rn% AND &HF), bitmask% AND &HFFFF)
  END IF
END SUB

SUB encoder.doLdmdb(op%, fl%, cc%, sz%)
  ' LDMDB: hw0 base = &HE910 (W=0) or &HE930 (W=1).
  LOCAL i%, rn%, bitmask%, w%
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "LDMDB: expected Rn" : EXIT SUB
  rn% = tok.val%(i%) : INC i%
  w% = 0
  IF tok.type%(i%) = TT_BANG THEN w% = 1 : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF tok.type%(i%) <> TT_RLIST THEN showError "LDMDB: expected register list" : EXIT SUB
  bitmask% = tok.val%(i%)
  emit32((&HE910 OR (w% << 5)) OR (rn% AND &HF), bitmask% AND &HDFFF)  ' bit 13 not allowed
END SUB

SUB encoder.doStmdb(op%, fl%, cc%, sz%)
  ' STMDB: hw0 base = &HE900 (W=0) or &HE920 (W=1).
  LOCAL i%, rn%, bitmask%, w%
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "STMDB: expected Rn" : EXIT SUB
  rn% = tok.val%(i%) : INC i%
  w% = 0
  IF tok.type%(i%) = TT_BANG THEN w% = 1 : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF tok.type%(i%) <> TT_RLIST THEN showError "STMDB: expected register list" : EXIT SUB
  bitmask% = tok.val%(i%)
  emit32((&HE900 OR (w% << 5)) OR (rn% AND &HF), bitmask% AND &HDFFF)
END SUB

' ============================================================
' PUSH / POP
' ============================================================
' Population count over the 16 register-list bits (R0-R12, LR=14, PC=15).
FUNCTION encoder.regListPop%(bitmask%)
  LOCAL n%, b%
  n% = 0
  FOR b% = 0 TO 15
    IF (bitmask% AND (1 << b%)) <> 0 THEN INC n%
  NEXT b%
  encoder.regListPop% = n%
END FUNCTION

' Index of the (single) set bit in bitmask.  Caller must ensure exactly one bit is set.
FUNCTION encoder.regListSingle%(bitmask%)
  LOCAL b%
  FOR b% = 0 TO 15
    IF (bitmask% AND (1 << b%)) <> 0 THEN encoder.regListSingle% = b% : EXIT FUNCTION
  NEXT b%
  encoder.regListSingle% = -1
END FUNCTION

SUB encoder.doPush(op%, fl%, cc%, sz%)
  LOCAL i%, bitmask%
  LOCAL has_lr%
  i% = op%
  IF tok.type%(i%) <> TT_RLIST THEN showError "PUSH: expected register list" : EXIT SUB
  bitmask% = tok.val%(i%)
  ' 16-bit T1: R0-R7 + optional LR (bit14), no other hi regs
  has_lr% = (bitmask% AND &H4000) <> 0
  IF (bitmask% AND &HBF00) = 0 AND sz% <> SIZE_W THEN
    ' Only R0-R7 + LR
    emit16(&HB400 OR (has_lr% << 8) OR (bitmask% AND &HFF))
  ELSEIF encoder.regListPop%(bitmask%) = 1 THEN
    ' Single-register T3:  STR Rt,[SP,#-4]!
    encoder.emitLdstT4(&HF8C0, 13, encoder.regListSingle%(bitmask%), -4, 1, 1)
  ELSE
    ' 32-bit STMDB SP!
    emit32(&HE92D, bitmask% AND &HDFFF)
  END IF
END SUB

SUB encoder.doPop(op%, fl%, cc%, sz%)
  LOCAL i%, bitmask%
  LOCAL has_pc%
  i% = op%
  IF tok.type%(i%) <> TT_RLIST THEN showError "POP: expected register list" : EXIT SUB
  bitmask% = tok.val%(i%)
  ' 16-bit T1: R0-R7 + optional PC (bit15), no other hi regs
  has_pc% = (bitmask% AND &H8000) <> 0
  IF (bitmask% AND &H7F00) = 0 AND sz% <> SIZE_W THEN
    emit16(&HBC00 OR (has_pc% << 8) OR (bitmask% AND &HFF))
  ELSEIF encoder.regListPop%(bitmask%) = 1 THEN
    ' Single-register T3:  LDR Rt,[SP],#4
    encoder.emitLdstT4(&HF8D0, 13, encoder.regListSingle%(bitmask%), 4, 0, 1)
  ELSE
    emit32(&HE8BD, bitmask% AND &HFFFF)
  END IF
END SUB

' ============================================================
' ADR
' ============================================================
SUB encoder.doAdr(op%, fl%, cc%, sz%)
  LOCAL i%, rd%, imm%, addr%, off%
  LOCAL lbl$ LENGTH MAX_NAME_LEN
  LOCAL i4d%, im3d%, im8d%
  LOCAL pcAlignS%, deltaS%
  LOCAL nd%, i4n%, im3n%, im8n%
  LOCAL tgt%
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "ADR: expected Rd" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF tok.type%(i%) = TT_IMM THEN
    imm% = tok.val%(i%)
    IF rd% <= 7 AND (imm% AND 3) = 0 AND imm% >= 0 AND imm% <= 1020 AND sz% <> SIZE_W THEN
      emit16(&HA000 OR (rd% << 8) OR (imm% / 4))
    ELSE
      i4d% = (imm% >> 11) AND 1 : im3d% = (imm% >> 8) AND 7 : im8d% = imm% AND &HFF
      emit32(&HF20F OR (i4d% << 10), (im3d% << 12) OR ((rd% AND &HF) << 8) OR im8d%)
    END IF
  ELSEIF tok.type%(i%) = TT_ADDR THEN
    lbl$ = tok.str$(i%)
    off% = io.ptr%
    ' .equ symbols must be defined before use; resolve immediately.
    IF symExists%(lbl$) THEN
      addr% = lookupSym%(lbl$)
      pcAlignS% = (io.base% + off% * 2 + 4) AND &HFFFFFFFC
      deltaS%   = addr% - pcAlignS%
      IF rd% <= 7 AND deltaS% >= 0 AND deltaS% <= 1020 AND (deltaS% AND 3) = 0 AND sz% <> SIZE_W THEN
        emit16(&HA000 OR (rd% << 8) OR (deltaS% / 4))
      ELSEIF deltaS% >= 0 AND deltaS% <= 4095 THEN
        i4d% = (deltaS% >> 11) AND 1 : im3d% = (deltaS% >> 8) AND 7 : im8d% = deltaS% AND &HFF
        emit32(&HF20F OR (i4d% << 10), (im3d% << 12) OR ((rd% AND &HF) << 8) OR im8d%)
      ELSEIF deltaS% < 0 AND deltaS% >= -4095 THEN
        nd% = -deltaS%
        i4n% = (nd% >> 11) AND 1 : im3n% = (nd% >> 8) AND 7 : im8n% = nd% AND &HFF
        emit32(&HF2AF OR (i4n% << 10), (im3n% << 12) OR ((rd% AND &HF) << 8) OR im8n%)
      ELSE
        showError "ADR: target out of range (delta=" + STR$(deltaS%) + ")"
      END IF
      EXIT SUB
    END IF
    ' Label form (may be forward reference): emit placeholder, set patch type,
    ' then call refLbl. Resolved → backpatch immediately; unresolved → chained.
    IF sz% = SIZE_W THEN
      emit32(&HF20F, ((rd% AND &HF) << 8))
      enc.patchType%(off%) = 6
      enc.patchAux%(off%)  = (rd% AND &HF) OR &H10
    ELSE
      emit16(&HA000 OR ((rd% AND 7) << 8))
      enc.patchType%(off%) = 6
      enc.patchAux%(off%)  = (rd% AND &HF)
    END IF
    tgt% = encoder.resolveLabel%(lbl$, off%)
    IF tgt% >= 0 THEN backpatchLbl(off%, tgt%)
  ELSE
    showError "ADR: expected immediate or label"
  END IF
END SUB

' ============================================================
' B  —  branch
' ============================================================
SUB encoder.doBranch(op%, fl%, cc%, sz%)
  LOCAL i%, offset%, imm%, pcAddr%, off%
  LOCAL lbl$ LENGTH MAX_NAME_LEN
  LOCAL tgt%
  i% = op%
  IF tok.type%(i%) <> TT_ADDR THEN showError "B: expected label" : EXIT SUB
  lbl$ = tok.str$(i%)
  off% = io.ptr%
  pcAddr% = io.base% + off% * 2 + 4

  IF cc% = COND_NONE THEN
    ' Unconditional B
    IF sz% = SIZE_W THEN
      emit32(&HF000, &H9000)   ' B T4 placeholder
      enc.patchType%(off%) = 2
      tgt% = encoder.resolveLabel%(lbl$, off%)
      IF tgt% >= 0 THEN backpatchLbl(off%, tgt%)
    ELSE
      emit16(&HE000)   ' B T2 placeholder
      enc.patchType%(off%) = 1
      tgt% = encoder.resolveLabel%(lbl$, off%)
      IF tgt% >= 0 THEN
        offset% = (tgt% - pcAddr%) / 2
        IF offset% < -1024 OR offset% > 1023 THEN
          showError "B: label too far for 16-bit (use B.W)"
        ELSE
          io.buffer%(off%) = &HE000 OR (offset% AND &H7FF)
        END IF
      END IF
    END IF
  ELSE
    ' Conditional B
    IF sz% = SIZE_W THEN
      emit32(&HF000, &H8000)   ' B T3 placeholder
      enc.patchType%(off%) = 40 + (cc% - 1)
      tgt% = encoder.resolveLabel%(lbl$, off%)
      IF tgt% >= 0 THEN backpatchLbl(off%, tgt%)
    ELSE
      emit16(&HD000 OR ((cc% - 1) << 8))   ' B T1 placeholder
      enc.patchType%(off%) = 20 + (cc% - 1)
      tgt% = encoder.resolveLabel%(lbl$, off%)
      IF tgt% >= 0 THEN
        offset% = (tgt% - pcAddr%) / 2
        IF offset% < -128 OR offset% > 127 THEN
          showError "B<cond>: label too far for 16-bit (use .W)"
        ELSE
          io.buffer%(off%) = &HD000 OR ((cc% - 1) << 8) OR (offset% AND &HFF)
        END IF
      END IF
    END IF
  END IF
END SUB

' Helper: backpatch a 32-bit conditional branch T3
SUB encoder.backpatchCond32(off%, addr%, cc%)
  LOCAL pcAddr%, offset%, S%, J1%, J2%, imm6%, imm11%
  pcAddr% = io.base% + off% * 2 + 4
  offset% = (addr% - pcAddr%) / 2
  S%    = (offset% >> 23) AND 1
  J1%   = (offset% >> 19) AND 1
  J2%   = (offset% >> 18) AND 1
  imm6% = (offset% >> 12) AND &H3F
  imm11% = (offset% >> 1)  AND &H7FF
  io.buffer%(off%)     = &HF000 OR (S% << 10) OR ((cc% - 1) << 6) OR (imm6% AND &H3F)
  io.buffer%(off% + 1) = &H8000 OR (J1% << 13) OR (J2% << 11) OR imm11%
END SUB

' ============================================================
' BL  —  always 32-bit
' ============================================================
SUB encoder.doBl(op%, fl%, cc%, sz%)
  LOCAL i%, off%, tgt%, offset%, S%, J1%, J2%, imm10%, imm11%
  LOCAL lbl$ LENGTH MAX_NAME_LEN
  LOCAL pcAddr%
  i% = op%
  IF tok.type%(i%) <> TT_ADDR THEN showError "BL: expected label" : EXIT SUB
  lbl$ = tok.str$(i%)
  off% = io.ptr%
  emit32(&HF000, &HD000)   ' placeholder BL
  enc.patchType%(off%) = 3
  tgt% = encoder.resolveLabel%(lbl$, off%)
  IF tgt% >= 0 THEN
    pcAddr% = io.base% + off% * 2 + 4
    offset% = (tgt% - pcAddr%) / 2
    S%     = (offset% >> 23) AND 1
    J1%    = 1 XOR S% XOR ((offset% >> 21) AND 1)
    J2%    = 1 XOR S% XOR ((offset% >> 20) AND 1)
    imm10% = (offset% >> 11) AND &H3FF
    imm11% = offset% AND &H7FF
    io.buffer%(off%)     = &HF000 OR (S% << 10) OR imm10%
    io.buffer%(off% + 1) = &HD000 OR (J1% << 13) OR (J2% << 11) OR imm11%
  END IF
END SUB

' ============================================================
' BX / BLX  register
' ============================================================
SUB encoder.doBx(op%, fl%, cc%, sz%)
  LOCAL i%, rm%
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "BX: expected register" : EXIT SUB
  rm% = tok.val%(i%)
  emit16(&H4700 OR ((rm% AND &HF) << 3))
END SUB

SUB encoder.doBlx(op%, fl%, cc%, sz%)
  LOCAL i%, rm%
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "BLX: expected register" : EXIT SUB
  rm% = tok.val%(i%)
  emit16(&H4780 OR ((rm% AND &HF) << 3))
END SUB

' ============================================================
' CBZ / CBNZ
' ============================================================
SUB encoder.doCbz(op%, fl%, cc%, sz%, isNz%)
  LOCAL i%, rn%, off%, tgt%, pcAddr%, offset%, i5%, imm5%
  LOCAL lbl$ LENGTH MAX_NAME_LEN
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO) THEN showError "CBZ: expected R0-R7" : EXIT SUB
  rn% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF tok.type%(i%) <> TT_ADDR THEN showError "CBZ: expected label" : EXIT SUB
  lbl$ = tok.str$(i%)
  off% = io.ptr%
  emit16(&HB100 OR (isNz% << 11) OR rn%)   ' placeholder
  tgt% = encoder.resolveLabel%(lbl$, off%)
  IF tgt% >= 0 THEN
    pcAddr% = io.base% + off% * 2 + 4
    offset% = tgt% - pcAddr%
    IF offset% < 0 OR offset% > 126 OR (offset% AND 1) <> 0 THEN
      showError "CBZ/CBNZ: label out of range (0..126, even)"
    ELSE
      imm5% = (offset% >> 1) AND &H1F
      i5%   = (offset% >> 6) AND 1
      io.buffer%(off%) = &HB100 OR (isNz% << 11) OR (i5% << 9) OR (imm5% << 3) OR rn%
    END IF
  ELSE
    ' Forward reference — defineLbl will call backpatchLbl with patch type 4.
    ' io.buffer%(off%) has been clobbered by the chain pointer; carry op+Rn aside.
    enc.patchType%(off%) = 4
    enc.patchAux%(off%)  = (isNz% << 3) OR rn%
  END IF
END SUB

' ============================================================
' IT  — IT block instruction
' ============================================================
SUB encoder.doIt(op%, fl%, cc%, sz%)
  LOCAL firstCond%, mask%, numSlots%, J%
  LOCAL mneStr$    LENGTH 8
  LOCAL maskChars$ LENGTH 4
  LOCAL c$         LENGTH 1
  ' cc% holds the condition code consumed by encode() from the token stream.
  IF cc% = COND_NONE THEN showError "IT: expected condition code" : EXIT SUB
  firstCond% = cc% - 1   ' COND_xx is 1-indexed; ARM encoding is 0-indexed

  ' The full IT variant is in tok.str$(0): "IT", "ITT", "ITTE", etc.
  mneStr$ = tok.str$(0)   ' already includes T/E chars

  ' Decode the mask from the IT variant string (chars after "IT")
  ' J = LSB of firstCond (ARM 4-bit encoding).
  ' Each T/E slot bit = J if T, (1-J) if E.
  ' Guard bit (lowest 1 in mask) marks the last slot position.
  '   0 slots (IT):   guard at bit3 → mask = 1000
  '   1 slot  (ITx):  T/E at bit3, guard at bit2 → mask = {b3}100
  '   2 slots (ITxx): T/E at bit3, bit2, guard at bit1 → mask = {b3}{b2}10
  '   3 slots (ITxxx): T/E at bit3..1, guard at bit0 → mask = {b3}{b2}{b1}1
  J% = firstCond% AND 1
  mask% = 0
  maskChars$ = MID$(mneStr$, 3)   ' everything after "IT"
  numSlots% = LEN(maskChars$)     ' 0=IT, 1=ITT/ITE, 2=ITTT/etc, 3=ITTTT

  IF numSlots% = 0 THEN
    mask% = &H8
  ELSEIF numSlots% = 1 THEN
    IF MID$(maskChars$, 1, 1) = "T" THEN mask% = (J% << 3) OR 4 ELSE mask% = ((1 - J%) << 3) OR 4
  ELSEIF numSlots% = 2 THEN
    IF MID$(maskChars$, 1, 1) = "T" THEN mask% = (J% << 3) ELSE mask% = ((1 - J%) << 3)
    IF MID$(maskChars$, 2, 1) = "T" THEN mask% = mask% OR (J% << 2) OR 2 ELSE mask% = mask% OR ((1 - J%) << 2) OR 2
  ELSEIF numSlots% = 3 THEN
    IF MID$(maskChars$, 1, 1) = "T" THEN mask% = (J% << 3) ELSE mask% = ((1 - J%) << 3)
    IF MID$(maskChars$, 2, 1) = "T" THEN mask% = mask% OR (J% << 2) ELSE mask% = mask% OR ((1 - J%) << 2)
    IF MID$(maskChars$, 3, 1) = "T" THEN mask% = mask% OR (J% << 1) OR 1 ELSE mask% = mask% OR ((1 - J%) << 1) OR 1
  END IF

  ' Set IT block state for subsequent instruction validation
  it.active% = 1
  it.cond%   = cc%   ' store as COND_xx (cc% is already COND_xx)
  it.count%  = numSlots% + 1
  it.pos%    = 0
  it.mask%   = mask%

  emit16(&HBF00 OR (firstCond% << 4) OR mask%)
END SUB

' ============================================================
' BKPT / SVC
' ============================================================
SUB encoder.doBkpt(op%, fl%, cc%, sz%)
  LOCAL i%, imm%
  i% = op%
  IF tok.type%(i%) = TT_IMM THEN imm% = tok.val%(i%) AND &HFF ELSE imm% = 0
  emit16(&HBE00 OR imm%)
END SUB

SUB encoder.doSvc(op%, fl%, cc%, sz%)
  LOCAL i%, imm%
  i% = op%
  IF tok.type%(i%) = TT_IMM THEN imm% = tok.val%(i%) AND &HFF ELSE imm% = 0
  emit16(&HDF00 OR imm%)
END SUB

' ============================================================
' SXTB / SXTH / UXTB / UXTH  T1 16-bit
' subOp%: SXTH=0, SXTB=1, UXTH=2, UXTB=3
' ============================================================
' Core implementation:

SUB encoder.doExtendOp(op%, fl%, cc%, sz%, subOp%)
  LOCAL i%, rd%, rm%, rot%, rotOk%
  LOCAL op32h%
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "Extend: expected Rd" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "Extend: expected Rm" : EXIT SUB
  rm% = tok.val%(i%) : INC i%
  rotOk% = 1 : rot% = encoder.parseExtRot%(i%, rotOk%)
  IF rotOk% = 0 THEN showError "Extend: bad rotation (use ROR #0/#8/#16/#24)" : EXIT SUB
  IF rd% <= 7 AND rm% <= 7 AND rot% = 0 AND sz% <> SIZE_W THEN
    emit16(&HB200 OR (subOp% << 6) OR (rm% << 3) OR rd%)
  ELSE
    ' 32-bit T2: SXTH=FA0F, SXTB=FA4F, UXTH=FA1F, UXTB=FA5F (Rn=1111)
    SELECT CASE subOp%
      CASE 0 : op32h% = &HFA0F  ' SXTH
      CASE 1 : op32h% = &HFA4F  ' SXTB
      CASE 2 : op32h% = &HFA1F  ' UXTH
      CASE 3 : op32h% = &HFA5F  ' UXTB
    END SELECT
    emit32(op32h%, (&HF << 12) OR ((rd% AND &HF) << 8) OR &H80 OR (rot% << 4) OR (rm% AND &HF))
  END IF
END SUB

' ============================================================
' Trailing ", ROR #n" parser for extend instructions.
' n must be 0, 8, 16, or 24.  Returns rot field 0..3.
' Sets ok%=0 on syntax error (caller reports).
' ============================================================
FUNCTION encoder.parseExtRot%(i%, ok%)
  LOCAL n%
  ok% = 1
  encoder.parseExtRot% = 0
  IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  IF i% >= tok.count% THEN EXIT FUNCTION
  IF tok.type%(i%) <> TT_SHIFT THEN EXIT FUNCTION
  IF tok.val%(i%) <> SHIFT_ROR THEN ok% = 0 : EXIT FUNCTION
  INC i%
  IF i% >= tok.count% OR tok.type%(i%) <> TT_IMM THEN ok% = 0 : EXIT FUNCTION
  n% = tok.val%(i%) : INC i%
  SELECT CASE n%
    CASE 0  : encoder.parseExtRot% = 0
    CASE 8  : encoder.parseExtRot% = 1
    CASE 16 : encoder.parseExtRot% = 2
    CASE 24 : encoder.parseExtRot% = 3
    CASE ELSE : ok% = 0
  END SELECT
END FUNCTION

' ============================================================
' 32-bit extend variants: SXTB16/UXTB16 (hasRn%=0) and the
' SXTAB/SXTAH/SXTAB16/UXTAB/UXTAH/UXTAB16 family (hasRn%=1).
' opHi% is the first halfword base (Rn nibble cleared).
' ============================================================
SUB encoder.doExtend32(op%, opHi%, hasRn%)
  LOCAL i%, rd%, rn%, rm%, rot%, rotOk%
  i% = op%
  IF i% >= tok.count% OR (tok.type%(i%) <> TT_REGLO AND tok.type%(i%) <> TT_REGHI) THEN showError "Extend: expected Rd" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  rn% = 15
  IF hasRn% THEN
    IF i% >= tok.count% OR (tok.type%(i%) <> TT_REGLO AND tok.type%(i%) <> TT_REGHI) THEN showError "Extend: expected Rn" : EXIT SUB
    rn% = tok.val%(i%) : INC i%
    IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  END IF
  IF i% >= tok.count% OR (tok.type%(i%) <> TT_REGLO AND tok.type%(i%) <> TT_REGHI) THEN showError "Extend: expected Rm" : EXIT SUB
  rm% = tok.val%(i%) : INC i%
  rotOk% = 1 : rot% = encoder.parseExtRot%(i%, rotOk%)
  IF rotOk% = 0 THEN showError "Extend: bad rotation (use ROR #0/#8/#16/#24)" : EXIT SUB
  emit32(opHi% OR (rn% AND &HF), (&HF << 12) OR ((rd% AND &HF) << 8) OR &H80 OR (rot% << 4) OR (rm% AND &HF))
END SUB

' ============================================================
' PKHBT (tb=0, LSL shift) / PKHTB (tb=1, ASR shift)
' ============================================================
SUB encoder.doPkh(op%, isTb%)
  LOCAL i%, rd%, rn%, rm%, sh%, shimm%, imm3%, imm2%
  i% = op%
  IF i% >= tok.count% OR (tok.type%(i%) <> TT_REGLO AND tok.type%(i%) <> TT_REGHI) THEN showError "PKH: expected Rd" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  IF i% >= tok.count% OR (tok.type%(i%) <> TT_REGLO AND tok.type%(i%) <> TT_REGHI) THEN showError "PKH: expected Rn" : EXIT SUB
  rn% = tok.val%(i%) : INC i%
  IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  IF i% >= tok.count% OR (tok.type%(i%) <> TT_REGLO AND tok.type%(i%) <> TT_REGHI) THEN showError "PKH: expected Rm" : EXIT SUB
  rm% = tok.val%(i%) : INC i%
  shimm% = 0
  IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  IF i% < tok.count% AND tok.type%(i%) = TT_SHIFT THEN
    sh% = tok.val%(i%) : INC i%
    IF isTb% AND sh% <> SHIFT_ASR THEN showError "PKHTB: shift must be ASR" : EXIT SUB
    IF NOT isTb% AND sh% <> SHIFT_LSL THEN showError "PKHBT: shift must be LSL" : EXIT SUB
    IF i% < tok.count% AND tok.type%(i%) = TT_IMM THEN shimm% = tok.val%(i%) AND &H1F
  END IF
  imm3% = (shimm% >> 2) AND 7
  imm2% = shimm% AND 3
  emit32(&HEAC0 OR (rn% AND &HF), (imm3% << 12) OR ((rd% AND &HF) << 8) OR (imm2% << 6) OR (isTb% << 5) OR (rm% AND &HF))
END SUB

' ============================================================
' Parallel add/sub (signed/unsigned, normal/saturating/halving):
' SADD/SSUB/SASX/SSAX/UADD/USUB/UASX/USAX/QADD/QSUB/QASX/QSAX/
' UQADD/UQSUB/UQASX/UQSAX/SHADD/SHSUB/SHASX/SHSAX/UHADD/UHSUB/UHASX/UHSAX
' Encoding T1: 1111 1010 1 op1 Rn 1111 Rd 0 op2 Rm
' op1 selects ADD16/ASX/SAX/SUB16/ADD8/SUB8.
' op2 selects S/Q/SH/U/UQ/UH (signedness + modifier).
' ============================================================
SUB encoder.doParAddSub(op%, op1%, op2%)
  LOCAL i%, rd%, rn%, rm%
  i% = op%
  IF i% >= tok.count% OR (tok.type%(i%) <> TT_REGLO AND tok.type%(i%) <> TT_REGHI) THEN showError "ParAdd/Sub: expected Rd" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  IF i% >= tok.count% OR (tok.type%(i%) <> TT_REGLO AND tok.type%(i%) <> TT_REGHI) THEN showError "ParAdd/Sub: expected Rn" : EXIT SUB
  rn% = tok.val%(i%) : INC i%
  IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  IF i% >= tok.count% OR (tok.type%(i%) <> TT_REGLO AND tok.type%(i%) <> TT_REGHI) THEN showError "ParAdd/Sub: expected Rm" : EXIT SUB
  rm% = tok.val%(i%)
  emit32(&HFA80 OR ((op1% AND 7) << 4) OR (rn% AND &HF), &HF000 OR ((rd% AND &HF) << 8) OR ((op2% AND 7) << 4) OR (rm% AND &HF))
END SUB

' ============================================================
' LDREX / STREX  (word-size, with optional #imm offset, range 0..1020 mult of 4)
'   LDREX  Rt, [Rn{, #imm}]      T1: E850|Rn / Rt 1111 imm8       (imm = offset/4)
'   STREX  Rd, Rt, [Rn{, #imm}]  T1: E840|Rn / Rt Rd imm8
' ============================================================
SUB encoder.doLdrex(op%, fl%, cc%, sz%, isLoad%)
  LOCAL i%, rd%, rt%, rn%, imm%, off%
  i% = op%
  IF i% >= tok.count% OR (tok.type%(i%) <> TT_REGLO AND tok.type%(i%) <> TT_REGHI) THEN showError "LDREX/STREX: expected register" : EXIT SUB
  rt% = tok.val%(i%) : INC i%
  IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  rd% = 15
  IF NOT isLoad% THEN
    rd% = rt%
    IF i% >= tok.count% OR (tok.type%(i%) <> TT_REGLO AND tok.type%(i%) <> TT_REGHI) THEN showError "STREX: expected Rt" : EXIT SUB
    rt% = tok.val%(i%) : INC i%
    IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  END IF
  IF i% >= tok.count% OR tok.type%(i%) <> TT_LBRA THEN showError "LDREX/STREX: expected [" : EXIT SUB
  INC i%
  IF i% >= tok.count% OR (tok.type%(i%) <> TT_REGLO AND tok.type%(i%) <> TT_REGHI) THEN showError "LDREX/STREX: expected Rn" : EXIT SUB
  rn% = tok.val%(i%) : INC i%
  imm% = 0
  IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  IF i% < tok.count% AND tok.type%(i%) = TT_IMM THEN
    imm% = tok.val%(i%) : INC i%
    IF imm% < 0 OR imm% > 1020 OR (imm% AND 3) <> 0 THEN showError "LDREX/STREX: imm must be 0..1020 mult of 4" : EXIT SUB
  END IF
  IF i% < tok.count% AND tok.type%(i%) = TT_RBRA THEN INC i%
  off% = (imm% >> 2) AND &HFF
  IF isLoad% THEN
    emit32(&HE850 OR (rn% AND &HF), ((rt% AND &HF) << 12) OR &HF00 OR off%)
  ELSE
    emit32(&HE840 OR (rn% AND &HF), ((rt% AND &HF) << 12) OR ((rd% AND &HF) << 8) OR off%)
  END IF
END SUB

' ============================================================
' LDREXB / LDREXH / STREXB / STREXH  -- byte/halfword exclusive, [Rn] only
'   LDREXB Rt, [Rn]              T1: E8D0|Rn / Rt 1111 0100 1111
'   LDREXH Rt, [Rn]              T1: E8D0|Rn / Rt 1111 0101 1111
'   STREXB Rd, Rt, [Rn]          T1: E8C0|Rn / Rt 1111 0100 Rd
'   STREXH Rd, Rt, [Rn]          T1: E8C0|Rn / Rt 1111 0101 Rd
' ============================================================
SUB encoder.doLdrexBH(op%, fl%, cc%, sz%, isLoad%, isH%)
  LOCAL i%, rd%, rt%, rn%, op2%
  i% = op%
  IF i% >= tok.count% OR (tok.type%(i%) <> TT_REGLO AND tok.type%(i%) <> TT_REGHI) THEN showError "LDREX/STREX B/H: expected register" : EXIT SUB
  rt% = tok.val%(i%) : INC i%
  IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  rd% = 15
  IF NOT isLoad% THEN
    rd% = rt%
    IF i% >= tok.count% OR (tok.type%(i%) <> TT_REGLO AND tok.type%(i%) <> TT_REGHI) THEN showError "STREX B/H: expected Rt" : EXIT SUB
    rt% = tok.val%(i%) : INC i%
    IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  END IF
  IF i% >= tok.count% OR tok.type%(i%) <> TT_LBRA THEN showError "LDREX/STREX B/H: expected [" : EXIT SUB
  INC i%
  IF i% >= tok.count% OR (tok.type%(i%) <> TT_REGLO AND tok.type%(i%) <> TT_REGHI) THEN showError "LDREX/STREX B/H: expected Rn" : EXIT SUB
  rn% = tok.val%(i%) : INC i%
  IF i% < tok.count% AND tok.type%(i%) = TT_RBRA THEN INC i%
  IF isH% THEN op2% = 5 ELSE op2% = 4
  IF isLoad% THEN
    emit32(&HE8D0 OR (rn% AND &HF), ((rt% AND &HF) << 12) OR &HF0F OR (op2% << 4))
  ELSE
    emit32(&HE8C0 OR (rn% AND &HF), ((rt% AND &HF) << 12) OR &HF00 OR (op2% << 4) OR (rd% AND &HF))
  END IF
END SUB

' ============================================================
' LDRD / STRD  -- Rt, Rt2, [Rn{, #±imm}] / [Rn, #±imm]! / [Rn], #±imm
'   T1: 1110 100 P U 1 W L Rn  Rt Rt2 imm8     (offset = imm8 * 4)
' ============================================================
SUB encoder.doLdrd(op%, fl%, cc%, sz%, isLoad%)
  LOCAL i%, rt%, rt2%, rn%, imm%, p%, u%, w%, l%, off%, postIndex%, hadBang%
  i% = op%
  IF i% >= tok.count% OR (tok.type%(i%) <> TT_REGLO AND tok.type%(i%) <> TT_REGHI) THEN showError "LDRD/STRD: expected Rt" : EXIT SUB
  rt% = tok.val%(i%) : INC i%
  IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  IF i% >= tok.count% OR (tok.type%(i%) <> TT_REGLO AND tok.type%(i%) <> TT_REGHI) THEN showError "LDRD/STRD: expected Rt2" : EXIT SUB
  rt2% = tok.val%(i%) : INC i%
  IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  IF i% >= tok.count% OR tok.type%(i%) <> TT_LBRA THEN showError "LDRD/STRD: expected [" : EXIT SUB
  INC i%
  IF i% >= tok.count% OR (tok.type%(i%) <> TT_REGLO AND tok.type%(i%) <> TT_REGHI) THEN showError "LDRD/STRD: expected Rn" : EXIT SUB
  rn% = tok.val%(i%) : INC i%
  imm% = 0 : postIndex% = 0
  IF i% < tok.count% AND tok.type%(i%) = TT_RBRA THEN
    INC i%
    ' Possible post-index: ], #imm
    IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN
      INC i%
      IF i% < tok.count% AND tok.type%(i%) = TT_IMM THEN
        imm% = tok.val%(i%)
        INC i%
        postIndex% = 1
      END IF
    END IF
  ELSE
    IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
    IF i% < tok.count% AND tok.type%(i%) = TT_IMM THEN
      imm% = tok.val%(i%)
      INC i%
    END IF
    IF i% < tok.count% AND tok.type%(i%) = TT_RBRA THEN INC i%
  END IF
  hadBang% = 0
  IF i% < tok.count% AND tok.type%(i%) = TT_BANG THEN
    hadBang% = 1
    INC i%
  END IF
  IF postIndex% THEN
    p% = 0 : w% = 1
  ELSEIF hadBang% THEN
    p% = 1 : w% = 1
  ELSE
    p% = 1 : w% = 0
  END IF
  IF imm% > &H7FFFFFFF THEN
    u% = 0
    off% = (-imm%) AND &HFFFFFFFF
  ELSE
    u% = 1
    off% = imm%
  END IF
  IF (off% AND 3) <> 0 OR off% > 1020 THEN showError "LDRD/STRD: imm must be -1020..1020 mult of 4" : EXIT SUB
  off% = off% >> 2
  IF isLoad% THEN l% = 1 ELSE l% = 0
  emit32(&HE800 OR (p% << 8) OR (u% << 7) OR &H40 OR (w% << 5) OR (l% << 4) OR (rn% AND &HF), ((rt% AND &HF) << 12) OR ((rt2% AND &HF) << 8) OR (off% AND &HFF))
END SUB

' ============================================================
' Unprivileged loads/stores: LDRT / LDRBT / LDRSBT / LDRHT / LDRSHT /
'                           STRT / STRBT / STRHT
'   T1: <baseHi>|Rn / Rt 1110 imm8         (imm8 = 0..255, P=1,U=1,W=1)
' ============================================================
SUB encoder.doLdrxt(op%, fl%, cc%, sz%, baseHi%)
  LOCAL i%, rt%, rn%, imm%
  i% = op%
  IF i% >= tok.count% OR (tok.type%(i%) <> TT_REGLO AND tok.type%(i%) <> TT_REGHI) THEN showError "LDR/STR-T: expected Rt" : EXIT SUB
  rt% = tok.val%(i%) : INC i%
  IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  IF i% >= tok.count% OR tok.type%(i%) <> TT_LBRA THEN showError "LDR/STR-T: expected [" : EXIT SUB
  INC i%
  IF i% >= tok.count% OR (tok.type%(i%) <> TT_REGLO AND tok.type%(i%) <> TT_REGHI) THEN showError "LDR/STR-T: expected Rn" : EXIT SUB
  rn% = tok.val%(i%) : INC i%
  imm% = 0
  IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  IF i% < tok.count% AND tok.type%(i%) = TT_IMM THEN
    imm% = tok.val%(i%) : INC i%
    IF imm% < 0 OR imm% > 255 THEN showError "LDR/STR-T: imm8 must be 0..255" : EXIT SUB
  END IF
  IF i% < tok.count% AND tok.type%(i%) = TT_RBRA THEN INC i%
  emit32(baseHi% OR (rn% AND &HF), ((rt% AND &HF) << 12) OR &HE00 OR (imm% AND &HFF))
END SUB

' ============================================================
' Hint instructions: NOP/YIELD/WFE/WFI/SEV. Honor .W (T2 32-bit `f3af 80xx`).
' ============================================================
SUB encoder.doHint(sz%, narrow16%, hintId%)
  IF sz% = SIZE_W THEN
    emit32(&HF3AF, &H8000 OR (hintId% AND &HFF))
  ELSE
    emit16(narrow16%)
  END IF
END SUB

' ============================================================
' DBG #imm4
' ============================================================
SUB encoder.doDbg(op%)
  LOCAL i%, imm%
  i% = op%
  IF i% >= tok.count% OR tok.type%(i%) <> TT_IMM THEN showError "DBG: expected #imm4" : EXIT SUB
  imm% = tok.val%(i%) AND &HF
  emit32(&HF3AF, &H80F0 OR imm%)
END SUB

' ============================================================
' REV / REV16 / REVSH  — core:
' ============================================================
SUB encoder.doRevOp(op%, fl%, cc%, sz%, subOp%)
  LOCAL i%, rd%, rm%
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "REV: expected Rd" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "REV: expected Rm" : EXIT SUB
  rm% = tok.val%(i%)
  IF rd% <= 7 AND rm% <= 7 AND sz% <> SIZE_W THEN
    emit16(&HBA00 OR (subOp% << 6) OR (rm% << 3) OR rd%)
  ELSE
    ' 32-bit: REV T2, REV16 T2, REVSH T2
    ' REV   T2: FA90 Rm, F Rd 80 Rm
    ' REV16 T2: FA90 Rm, F Rd 90 Rm
    ' REVSH T2: FA90 Rm, F Rd B0 Rm
    SELECT CASE subOp%
      CASE 0 : emit32(&HFA90 OR (rm% AND &HF), (&HF << 12) OR ((rd% AND &HF) << 8) OR &H80 OR (rm% AND &HF))
      CASE 1 : emit32(&HFA90 OR (rm% AND &HF), (&HF << 12) OR ((rd% AND &HF) << 8) OR &H90 OR (rm% AND &HF))
      CASE 3 : emit32(&HFA90 OR (rm% AND &HF), (&HF << 12) OR ((rd% AND &HF) << 8) OR &HB0 OR (rm% AND &HF))
    END SELECT
  END IF
END SUB

' ============================================================
' SETEND
' ============================================================
SUB encoder.doSetend(op%, fl%, cc%, sz%)
  LOCAL i%
  LOCAL endian$ LENGTH 2
  i% = op%
  IF tok.type%(i%) <> TT_ADDR THEN showError "SETEND: expected BE or LE" : EXIT SUB
  endian$ = UCASE$(tok.str$(i%))
  IF endian$ = "BE" THEN
    emit16(&HB658)
  ELSEIF endian$ = "LE" THEN
    emit16(&HB650)
  ELSE
    showError "SETEND: expected BE or LE, got " + endian$
  END IF
END SUB

' ============================================================
' CPS  — change processor state
' ============================================================
SUB encoder.doCps(op%, fl%, cc%, sz%)
  LOCAL i%, im%, aif%, k%
  LOCAL modeStr$ LENGTH 8
  LOCAL flagStr$ LENGTH 8
  LOCAL ch$ LENGTH 1
  i% = op%
  IF i% >= tok.count% OR tok.type%(i%) <> TT_ADDR THEN showError "CPS: expected IE or ID" : EXIT SUB
  modeStr$ = UCASE$(tok.str$(i%)) : INC i%
  IF modeStr$ = "IE" THEN
    im% = 0
  ELSEIF modeStr$ = "ID" THEN
    im% = 1
  ELSE
    showError "CPS: expected IE or ID, got " + modeStr$ : EXIT SUB
  END IF
  ' iflag operand: combination of A / I / F (e.g. "I", "F", "IF", "AIF")
  IF i% >= tok.count% OR tok.type%(i%) <> TT_ADDR THEN showError "CPS: expected iflag (A/I/F)" : EXIT SUB
  flagStr$ = UCASE$(tok.str$(i%))
  aif% = 0
  FOR k% = 1 TO LEN(flagStr$)
    ch$ = MID$(flagStr$, k%, 1)
    SELECT CASE ch$
      CASE "A" : aif% = aif% OR 4
      CASE "I" : aif% = aif% OR 2
      CASE "F" : aif% = aif% OR 1
      CASE ELSE : showError "CPS: bad iflag char " + ch$ : EXIT SUB
    END SELECT
  NEXT k%
  ' T1 16-bit: 1011 0110 011 im 0 aif
  emit16(&HB660 OR (im% << 4) OR (aif% AND &H7))
END SUB

' ============================================================
' CLZ / RBIT
' ============================================================
SUB encoder.doClz(op%, fl%, cc%, sz%)
  LOCAL i%, rd%, rm%
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "CLZ: expected Rd" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "CLZ: expected Rm" : EXIT SUB
  rm% = tok.val%(i%)
  ' CLZ T1: FAB0 Rm | F Rd 80 Rm
  emit32(&HFAB0 OR (rm% AND &HF), (&HF << 12) OR ((rd% AND &HF) << 8) OR &H80 OR (rm% AND &HF))
END SUB

SUB encoder.doRbit(op%, fl%, cc%, sz%)
  LOCAL i%, rd%, rm%
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "RBIT: expected Rd" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "RBIT: expected Rm" : EXIT SUB
  rm% = tok.val%(i%)
  ' RBIT T1: FA90 Rm | F Rd A0 Rm
  emit32(&HFA90 OR (rm% AND &HF), (&HF << 12) OR ((rd% AND &HF) << 8) OR &HA0 OR (rm% AND &HF))
END SUB

' ============================================================
' SDIV / UDIV
' ============================================================
SUB encoder.doDiv(op%, fl%, cc%, sz%, isUdiv%)
  LOCAL i%, rd%, rn%, rm%
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "DIV: expected Rd" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "DIV: expected Rn" : EXIT SUB
  rn% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "DIV: expected Rm" : EXIT SUB
  rm% = tok.val%(i%)
  ' SDIV T1: FB90 Rn | F Rd F0 Rm
  ' UDIV T1: FBB0 Rn | F Rd F0 Rm
  IF isUdiv% THEN
    emit32(&HFBB0 OR (rn% AND &HF), (&HF << 12) OR ((rd% AND &HF) << 8) OR &HF0 OR (rm% AND &HF))
  ELSE
    emit32(&HFB90 OR (rn% AND &HF), (&HF << 12) OR ((rd% AND &HF) << 8) OR &HF0 OR (rm% AND &HF))
  END IF
END SUB

' ============================================================
' MLA / MLS
' ============================================================
SUB encoder.doMla(op%, fl%, cc%, sz%)
  LOCAL i%, rd%, rn%, rm%, ra%
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "MLA: expected Rd" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  rn% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  rm% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  ra% = tok.val%(i%)
  emit32(&HFB00 OR (rn% AND &HF), ((ra% AND &HF) << 12) OR ((rd% AND &HF) << 8) OR (rm% AND &HF))
END SUB

SUB encoder.doMls(op%, fl%, cc%, sz%)
  LOCAL i%, rd%, rn%, rm%, ra%
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "MLS: expected Rd" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  rn% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  rm% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  ra% = tok.val%(i%)
  emit32(&HFB00 OR (rn% AND &HF), ((ra% AND &HF) << 12) OR ((rd% AND &HF) << 8) OR &H10 OR (rm% AND &HF))
END SUB

' ============================================================
' SMULL / UMULL / SMLAL / UMLAL
' subOp%: 0=SMULL, 1=UMULL, 2=SMLAL, 3=UMLAL
' ============================================================
SUB encoder.doMull(op%, fl%, cc%, sz%, subOp%)
  LOCAL i%, rdlo%, rdhi%, rn%, rm%
  i% = op%
  rdlo% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  rdhi% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  rn%   = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  rm%   = tok.val%(i%)
  SELECT CASE subOp%
    CASE 0 : emit32(&HFB80 OR (rn% AND &HF), ((rdlo% AND &HF) << 12) OR ((rdhi% AND &HF) << 8) OR (rm% AND &HF))  ' SMULL
    CASE 1 : emit32(&HFBA0 OR (rn% AND &HF), ((rdlo% AND &HF) << 12) OR ((rdhi% AND &HF) << 8) OR (rm% AND &HF))  ' UMULL
    CASE 2 : emit32(&HFBC0 OR (rn% AND &HF), ((rdlo% AND &HF) << 12) OR ((rdhi% AND &HF) << 8) OR (rm% AND &HF))  ' SMLAL
    CASE 3 : emit32(&HFBE0 OR (rn% AND &HF), ((rdlo% AND &HF) << 12) OR ((rdhi% AND &HF) << 8) OR (rm% AND &HF))  ' UMLAL
  END SELECT
END SUB

' ============================================================
' SBFX / UBFX
' ============================================================
SUB encoder.doBitfield(op%, fl%, cc%, sz%, isUbfx%)
  LOCAL i%, rd%, rn%, lsb%, width%
  LOCAL widthm1%, imm3%, imm2%
  i% = op%
  IF i% >= tok.count% OR (tok.type%(i%) <> TT_REGLO AND tok.type%(i%) <> TT_REGHI) THEN showError "BFX: expected Rd" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  IF i% >= tok.count% OR (tok.type%(i%) <> TT_REGLO AND tok.type%(i%) <> TT_REGHI) THEN showError "BFX: expected Rn" : EXIT SUB
  rn% = tok.val%(i%) : INC i%
  IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  IF i% >= tok.count% OR tok.type%(i%) <> TT_IMM THEN showError "BFX: expected #lsb" : EXIT SUB
  lsb% = tok.val%(i%) : INC i%
  IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  IF i% >= tok.count% OR tok.type%(i%) <> TT_IMM THEN showError "BFX: expected #width" : EXIT SUB
  width% = tok.val%(i%)
  widthm1% = width% - 1
  imm3% = (lsb% >> 2) AND 7 : imm2% = lsb% AND 3
  IF isUbfx% THEN
    emit32(&HF3C0 OR (rn% AND &HF), (imm3% << 12) OR ((rd% AND &HF) << 8) OR (imm2% << 6) OR (widthm1% AND &H1F))
  ELSE
    emit32(&HF340 OR (rn% AND &HF), (imm3% << 12) OR ((rd% AND &HF) << 8) OR (imm2% << 6) OR (widthm1% AND &H1F))
  END IF
END SUB

' ============================================================
' BFI / BFC
' ============================================================
SUB encoder.doBfi(op%, fl%, cc%, sz%)
  LOCAL i%, rd%, rn%, lsb%, width%, msb%, imm3%, imm2%
  i% = op%
  IF i% >= tok.count% OR (tok.type%(i%) <> TT_REGLO AND tok.type%(i%) <> TT_REGHI) THEN showError "BFI: expected Rd" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  IF i% >= tok.count% OR (tok.type%(i%) <> TT_REGLO AND tok.type%(i%) <> TT_REGHI) THEN showError "BFI: expected Rn" : EXIT SUB
  rn% = tok.val%(i%) : INC i%
  IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  IF i% >= tok.count% OR tok.type%(i%) <> TT_IMM THEN showError "BFI: expected #lsb" : EXIT SUB
  lsb% = tok.val%(i%) : INC i%
  IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  IF i% >= tok.count% OR tok.type%(i%) <> TT_IMM THEN showError "BFI: expected #width" : EXIT SUB
  width% = tok.val%(i%)
  msb% = lsb% + width% - 1
  imm3% = (lsb% >> 2) AND 7 : imm2% = lsb% AND 3
  emit32(&HF360 OR (rn% AND &HF), (imm3% << 12) OR ((rd% AND &HF) << 8) OR (imm2% << 6) OR (msb% AND &H1F))
END SUB

SUB encoder.doBfc(op%, fl%, cc%, sz%)
  LOCAL i%, rd%, lsb%, width%, msb%, imm3%, imm2%
  i% = op%
  IF i% >= tok.count% OR (tok.type%(i%) <> TT_REGLO AND tok.type%(i%) <> TT_REGHI) THEN showError "BFC: expected Rd" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  IF i% >= tok.count% OR tok.type%(i%) <> TT_IMM THEN showError "BFC: expected #lsb" : EXIT SUB
  lsb% = tok.val%(i%) : INC i%
  IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  IF i% >= tok.count% OR tok.type%(i%) <> TT_IMM THEN showError "BFC: expected #width" : EXIT SUB
  width% = tok.val%(i%)
  msb% = lsb% + width% - 1
  imm3% = (lsb% >> 2) AND 7 : imm2% = lsb% AND 3
  emit32(&HF36F, (imm3% << 12) OR ((rd% AND &HF) << 8) OR (imm2% << 6) OR (msb% AND &H1F))
END SUB

' ============================================================
' TBB / TBH
' ============================================================
SUB encoder.doTb(op%, fl%, cc%, sz%, isH%)
  LOCAL i%, rn%, rm%
  i% = op%
  IF tok.type%(i%) = TT_LBRA THEN INC i%
  rn% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  rm% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_RBRA THEN INC i%
  emit32(&HE8D0 OR (rn% AND &HF), &HF000 OR (isH% << 4) OR (rm% AND &HF))
END SUB

' ============================================================
' SEL
' ============================================================
SUB encoder.doSel(op%, fl%, cc%, sz%)
  LOCAL i%, rd%, rn%, rm%
  i% = op%
  rd% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  rn% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  rm% = tok.val%(i%)
  emit32(&HFAA0 OR (rn% AND &HF), (&HF << 12) OR ((rd% AND &HF) << 8) OR &H80 OR (rm% AND &HF))
END SUB

' ============================================================
' Memory barriers  (DMB / DSB / ISB)
' ============================================================
SUB encoder.doBarrier(op%, fl%, cc%, sz%, hi%, lo%)
  LOCAL i%, opt%
  i% = op%
  opt% = &HF   ' default option = SY
  IF i% < tok.count% AND tok.type%(i%) = TT_ADDR THEN
    SELECT CASE UCASE$(tok.str$(i%))
      CASE "SY"   : opt% = &HF
      CASE "ST"   : opt% = &HE
      CASE "LD"   : opt% = &HD
      CASE "ISH"  : opt% = &HB
      CASE "ISHST": opt% = &HA
      CASE "NSH"  : opt% = &H7
      CASE "NSHST": opt% = &H6
      CASE "OSH"  : opt% = &H3
      CASE "OSHST": opt% = &H2
    END SELECT
  END IF
  emit32(hi%, lo% OR opt%)
END SUB

' ============================================================
' MRS / MSR
' ============================================================
' Resolve an ARMv7-M special-register name to its SYSm value (0..&H14)
' and set mask% (default 2) when the name carries an APSR mask suffix
' such as _nzcvq / _g / _nzcvqg (or the legacy _f/_c/_x/_s aliases).
' Returns -1 on unknown name.
FUNCTION encoder.parseSysReg%(name$, mask%)
  LOCAL n$ LENGTH 16
  LOCAL u%, base$, suf$
  n$ = UCASE$(name$)
  mask% = 2
  SELECT CASE n$
    CASE "APSR"        : encoder.parseSysReg% = &H00 : EXIT FUNCTION
    CASE "IAPSR"       : encoder.parseSysReg% = &H01 : EXIT FUNCTION
    CASE "EAPSR"       : encoder.parseSysReg% = &H02 : EXIT FUNCTION
    CASE "PSR", "XPSR" : encoder.parseSysReg% = &H03 : EXIT FUNCTION
    CASE "IPSR"        : encoder.parseSysReg% = &H05 : EXIT FUNCTION
    CASE "EPSR"        : encoder.parseSysReg% = &H06 : EXIT FUNCTION
    CASE "IEPSR"       : encoder.parseSysReg% = &H07 : EXIT FUNCTION
    CASE "MSP"         : encoder.parseSysReg% = &H08 : EXIT FUNCTION
    CASE "PSP"         : encoder.parseSysReg% = &H09 : EXIT FUNCTION
    CASE "PRIMASK"     : encoder.parseSysReg% = &H10 : EXIT FUNCTION
    CASE "BASEPRI"     : encoder.parseSysReg% = &H11 : EXIT FUNCTION
    CASE "BASEPRI_MAX" : encoder.parseSysReg% = &H12 : EXIT FUNCTION
    CASE "FAULTMASK"   : encoder.parseSysReg% = &H13 : EXIT FUNCTION
    CASE "CONTROL"     : encoder.parseSysReg% = &H14 : EXIT FUNCTION
  END SELECT
  u% = INSTR(n$, "_")
  IF u% > 0 THEN
    base$ = LEFT$(n$, u% - 1)
    suf$  = MID$(n$, u% + 1)
    IF base$ = "APSR" OR base$ = "CPSR" THEN
      SELECT CASE suf$
        CASE "NZCVQ", "F", "S", "X"     : mask% = 2
        CASE "G", "C"                   : mask% = 1
        CASE "NZCVQG", "GNZCVQ", "FS", "FG", "FC", "FX" : mask% = 3
      END SELECT
      encoder.parseSysReg% = &H00
      EXIT FUNCTION
    END IF
  END IF
  encoder.parseSysReg% = -1
END FUNCTION

SUB encoder.doMrs(op%, fl%, cc%, sz%)
  LOCAL i%, rd%, sysm%, mask%
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "MRS: expected Rd" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF i% >= tok.count% OR tok.type%(i%) <> TT_ADDR THEN showError "MRS: expected special register" : EXIT SUB
  sysm% = encoder.parseSysReg%(tok.str$(i%), mask%)
  IF sysm% < 0 THEN showError "MRS: unknown special register " + tok.str$(i%) : EXIT SUB
  ' MRS clears mask field (bits 11:10) per gas behaviour.
  emit32(&HF3EF, &H8000 OR ((rd% AND &HF) << 8) OR (sysm% AND &HFF))
END SUB

SUB encoder.doMsr(op%, fl%, cc%, sz%)
  LOCAL i%, rn%, sysm%, mask%
  i% = op%
  IF i% >= tok.count% OR tok.type%(i%) <> TT_ADDR THEN showError "MSR: expected special register" : EXIT SUB
  sysm% = encoder.parseSysReg%(tok.str$(i%), mask%)
  IF sysm% < 0 THEN showError "MSR: unknown special register " + tok.str$(i%) : EXIT SUB
  INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "MSR: expected Rn" : EXIT SUB
  rn% = tok.val%(i%)
  emit32(&HF380 OR (rn% AND &HF), &H8000 OR ((mask% AND 3) << 10) OR (sysm% AND &HFF))
END SUB

' ============================================================
' PLD / PLI  (pli% selects bit 8 of hw0: PLD=0xF8xx, PLI=0xF9xx)
'   PLD/PLI [Rn, #imm12]            (T1 immediate, U=1)
'   PLD/PLI [Rn, #-imm8]            (T2 immediate, U=0)
'   PLD/PLI [Rn]                    ([Rn,#0] T1)
'   PLD/PLI [Rn, Rm {,LSL #imm2}]   (T1 register)
'   PLD/PLI label                   (literal / PC-relative)
' ============================================================
SUB encoder.doPldPli(op%, fl%, cc%, sz%, pli%)
  LOCAL i%, rn%, rm%, imm%, sh%, shimm%, magn%, off%, tgt%
  LOCAL lbl$ LENGTH MAX_NAME_LEN
  i% = op%

  ' Literal/label form (also covers forward references via backpatch type 5)
  IF tok.type%(i%) = TT_ADDR THEN
    lbl$ = tok.str$(i%)
    off% = io.ptr%
    emit32(&HF89F OR (pli% << 8), &HF000)
    enc.patchType%(off%) = 5
    enc.patchAux%(off%)  = pli%
    tgt% = encoder.resolveLabel%(lbl$, off%)
    IF tgt% >= 0 THEN backpatchLbl(off%, tgt%)
    EXIT SUB
  END IF

  IF tok.type%(i%) <> TT_LBRA THEN showError "PLD/PLI: expected [ or label" : EXIT SUB
  INC i%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "PLD/PLI: expected Rn" : EXIT SUB
  rn% = tok.val%(i%) : INC i%

  ' [Rn]
  IF tok.type%(i%) = TT_RBRA THEN
    INC i%
    IF rn% = 15 THEN
      emit32(&HF89F OR (pli% << 8), &HF000)
    ELSE
      emit32((&HF890 OR (pli% << 8)) OR (rn% AND &HF), &HF000)
    END IF
    EXIT SUB
  END IF

  IF tok.type%(i%) = TT_COMMA THEN INC i%

  ' [Rn, #imm]
  IF tok.type%(i%) = TT_IMM THEN
    imm% = tok.val%(i%) : INC i%
    IF tok.type%(i%) = TT_RBRA THEN INC i%
    IF imm% > &H7FFFFFFF THEN
      magn% = (-imm%) AND &HFFFFFFFF
      IF magn% > 255 THEN showError "PLD/PLI: negative imm out of range (-255..0)" : EXIT SUB
      emit32((&HF810 OR (pli% << 8)) OR (rn% AND &HF), &HFC00 OR (magn% AND &HFF))
    ELSE
      IF imm% > 4095 THEN showError "PLD/PLI: imm out of range (0..4095)" : EXIT SUB
      emit32((&HF890 OR (pli% << 8)) OR (rn% AND &HF), &HF000 OR (imm% AND &HFFF))
    END IF
    EXIT SUB
  END IF

  ' [Rn, Rm {,LSL #imm2}]
  IF tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI THEN
    rm% = tok.val%(i%) : INC i%
    sh% = SHIFT_NONE : shimm% = 0
    IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
    IF i% < tok.count% AND tok.type%(i%) = TT_SHIFT THEN
      sh% = tok.val%(i%) : INC i%
      IF i% < tok.count% AND tok.type%(i%) = TT_IMM THEN shimm% = tok.val%(i%) : INC i%
    END IF
    IF tok.type%(i%) = TT_RBRA THEN INC i%
    IF sh% <> SHIFT_NONE AND sh% <> SHIFT_LSL THEN showError "PLD/PLI: only LSL allowed" : EXIT SUB
    IF shimm% < 0 OR shimm% > 3 THEN showError "PLD/PLI: shift imm out of range (0..3)" : EXIT SUB
    emit32((&HF810 OR (pli% << 8)) OR (rn% AND &HF), &HF000 OR ((shimm% AND 3) << 4) OR (rm% AND &HF))
    EXIT SUB
  END IF

  showError "PLD/PLI: bad addressing mode"
END SUB

' ============================================================
' Shared 3-/4-operand 32-bit multiply encoder.
'   hw0 = hw0base | Rn          hw1 = (Ra<<12) | (Rd<<8) | hw1mid | Rm
'   hasRa%=0 → Ra=&HF (no accumulator, "SMUL" form)
'   hasRa%=1 → 4th operand parsed as Ra ("SMLA" form)
' Used by groups I/J/K/L/N (SMULxx, SMLAxx, SMUAD, SMMLA, USAD8, ...).
' ============================================================
SUB encoder.doMul3or4(op%, hw0base%, hw1mid%, hasRa%)
  LOCAL i%, rd%, rn%, rm%, ra%
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "Mul: expected Rd" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "Mul: expected Rn" : EXIT SUB
  rn% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "Mul: expected Rm" : EXIT SUB
  rm% = tok.val%(i%) : INC i%
  IF hasRa% THEN
    IF tok.type%(i%) = TT_COMMA THEN INC i%
    IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "Mul: expected Ra" : EXIT SUB
    ra% = tok.val%(i%)
  ELSE
    ra% = &HF
  END IF
  emit32(hw0base% OR (rn% AND &HF), ((ra% AND &HF) << 12) OR ((rd% AND &HF) << 8) OR (hw1mid% AND &HF0) OR (rm% AND &HF))
END SUB

' ============================================================
' Shared 4-register long-multiply encoder.
'   Operand order: RdLo, RdHi, Rn, Rm
'   hw0 = hw0base | Rn         hw1 = (RdLo<<12) | (RdHi<<8) | hw1mid | Rm
' Used by group M (SMLALxx, SMLALD/X, SMLSLD/X, UMAAL).
' ============================================================
SUB encoder.doMull4op(op%, hw0base%, hw1mid%)
  LOCAL i%, rdlo%, rdhi%, rn%, rm%
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "MullDsp: expected RdLo" : EXIT SUB
  rdlo% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "MullDsp: expected RdHi" : EXIT SUB
  rdhi% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "MullDsp: expected Rn" : EXIT SUB
  rn% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "MullDsp: expected Rm" : EXIT SUB
  rm% = tok.val%(i%)
  emit32(hw0base% OR (rn% AND &HF), ((rdlo% AND &HF) << 12) OR ((rdhi% AND &HF) << 8) OR (hw1mid% AND &HF0) OR (rm% AND &HF))
END SUB

' ============================================================
' SSAT / USAT  (group O)
'   Syntax: Rd, #sat, Rn[, LSL/ASR #imm5]
'   hw0base: SSAT=&HF300, USAT=&HF380; sh bit: bit5 of hw0 (0=LSL, 1=ASR)
'   minus1%=1 → encode sat-1 (signed); minus1%=0 → encode sat directly (unsigned)
'   hw1 = imm3 Rd imm2 0 sat_imm5  (imm5 split: imm3=imm5[4:2], imm2=imm5[1:0])
' ============================================================
SUB encoder.doSat(op%, hw0base%, minus1%)
  LOCAL i%, rd%, sat%, rn%, sh%, shimm%, shBit%, sat5%, imm3%, imm2%
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "SAT: expected Rd" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF tok.type%(i%) <> TT_IMM THEN showError "SAT: expected #sat" : EXIT SUB
  sat% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "SAT: expected Rn" : EXIT SUB
  rn% = tok.val%(i%) : INC i%
  sh% = SHIFT_NONE : shimm% = 0
  IF i% < tok.count% AND tok.type%(i%) = TT_COMMA THEN INC i%
  IF i% < tok.count% AND tok.type%(i%) = TT_SHIFT THEN
    sh% = tok.val%(i%) : INC i%
    IF i% < tok.count% AND tok.type%(i%) = TT_IMM THEN shimm% = tok.val%(i%)
  END IF
  IF sh% <> SHIFT_NONE AND sh% <> SHIFT_LSL AND sh% <> SHIFT_ASR THEN showError "SAT: only LSL/ASR allowed" : EXIT SUB
  IF shimm% < 0 OR shimm% > 31 THEN showError "SAT: shift out of range (0..31)" : EXIT SUB
  IF minus1% THEN
    IF sat% < 1 OR sat% > 32 THEN showError "SSAT: imm out of range (1..32)" : EXIT SUB
    sat5% = (sat% - 1) AND &H1F
  ELSE
    IF sat% < 0 OR sat% > 31 THEN showError "USAT: imm out of range (0..31)" : EXIT SUB
    sat5% = sat% AND &H1F
  END IF
  shBit% = choice(sh% = SHIFT_ASR, 1, 0)
  imm3%  = (shimm% >> 2) AND 7
  imm2%  = shimm% AND 3
  emit32(hw0base% OR (shBit% << 5) OR (rn% AND &HF), (imm3% << 12) OR ((rd% AND &HF) << 8) OR (imm2% << 6) OR sat5%)
END SUB

' ============================================================
' SSAT16 / USAT16 (group O)
'   Syntax: Rd, #sat, Rn  (no shift)
'   hw0base: SSAT16=&HF320, USAT16=&HF3A0
'   hw1 = 0000 Rd 0000 sat_imm4 ; SSAT16: encode sat-1, USAT16: encode sat
' ============================================================
SUB encoder.doSat16(op%, hw0base%, minus1%)
  LOCAL i%, rd%, sat%, rn%, sat4%
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "SAT16: expected Rd" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF tok.type%(i%) <> TT_IMM THEN showError "SAT16: expected #sat" : EXIT SUB
  sat% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "SAT16: expected Rn" : EXIT SUB
  rn% = tok.val%(i%)
  IF minus1% THEN
    IF sat% < 1 OR sat% > 16 THEN showError "SSAT16: imm out of range (1..16)" : EXIT SUB
    sat4% = (sat% - 1) AND &HF
  ELSE
    IF sat% < 0 OR sat% > 15 THEN showError "USAT16: imm out of range (0..15)" : EXIT SUB
    sat4% = sat% AND &HF
  END IF
  emit32(hw0base% OR (rn% AND &HF), ((rd% AND &HF) << 8) OR sat4%)
END SUB

' ============================================================
' QADD / QSUB / QDADD / QDSUB  (saturating arith)
'   Syntax: Qxxx Rd, Rm, Rn
'   hw0 = 1111 1010 1000 Rn       (FA80 + Rn)
'   hw1 = 1111 Rd op4 Rm          (op4: QADD=8, QDADD=9, QSUB=10, QDSUB=11)
' ============================================================
SUB encoder.doSatArith(op%, op4%)
  LOCAL i%, rd%, rm%, rn%
  i% = op%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "Q{D}ADD/SUB: expected Rd" : EXIT SUB
  rd% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "Q{D}ADD/SUB: expected Rm" : EXIT SUB
  rm% = tok.val%(i%) : INC i%
  IF tok.type%(i%) = TT_COMMA THEN INC i%
  IF NOT (tok.type%(i%) = TT_REGLO OR tok.type%(i%) = TT_REGHI) THEN showError "Q{D}ADD/SUB: expected Rn" : EXIT SUB
  rn% = tok.val%(i%)
  emit32(&HFA80 OR (rn% AND &HF), &HF000 OR ((rd% AND &HF) << 8) OR ((op4% AND &HF) << 4) OR (rm% AND &HF))
END SUB

' ============================================================
' 4-arg wrappers for extend and rev (MMBasic has no overloading)
' ============================================================
SUB encoder.doSxth(op%, fl%, cc%, sz%)  : encoder.doExtendOp(op%, fl%, cc%, sz%, 0) : END SUB
SUB encoder.doSxtb(op%, fl%, cc%, sz%)  : encoder.doExtendOp(op%, fl%, cc%, sz%, 1) : END SUB
SUB encoder.doUxth(op%, fl%, cc%, sz%)  : encoder.doExtendOp(op%, fl%, cc%, sz%, 2) : END SUB
SUB encoder.doUxtb(op%, fl%, cc%, sz%)  : encoder.doExtendOp(op%, fl%, cc%, sz%, 3) : END SUB
SUB encoder.doRev0(op%, fl%, cc%, sz%)  : encoder.doRevOp(op%, fl%, cc%, sz%, 0)    : END SUB
SUB encoder.doRev1(op%, fl%, cc%, sz%)  : encoder.doRevOp(op%, fl%, cc%, sz%, 1)    : END SUB
SUB encoder.doRev3(op%, fl%, cc%, sz%)  : encoder.doRevOp(op%, fl%, cc%, sz%, 3)    : END SUB
