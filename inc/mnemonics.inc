' ============================================================
' mnemonics.inc  -  Mnemonic and keyword lookup tables
'
' Call initMnemonics() once at startup to load the DATA table.
' All lookup functions expect upper-case input.
' ============================================================

CONST MAX_MNE = 197     ' number of known mnemonics (indices 1..197)

DIM mne.name$(MAX_MNE) LENGTH 8

' Load the 195 mnemonic names from the DATA section in assembler.bas.
SUB initMnemonics()
  LOCAL i%
  RESTORE mne_data
  FOR i% = 1 TO MAX_MNE
    READ mne.name$(i%)
  NEXT i%
END SUB

' Return mnemonic index 1..MAX_MNE, or 0 if not found.
' Caller must pass an upper-case string.
FUNCTION lookupMne%(s$)
  LOCAL i%
  FOR i% = 1 TO MAX_MNE
    IF mne.name$(i%) = s$ THEN
      lookupMne% = i%
      EXIT FUNCTION
    END IF
  NEXT i%
  lookupMne% = 0
END FUNCTION

' Return COND_xx constant for a 2-character condition string, or COND_NONE.
' Caller must pass an upper-case string.
FUNCTION lookupCond%(s$)
  SELECT CASE s$
    CASE "EQ" : lookupCond% = COND_EQ
    CASE "NE" : lookupCond% = COND_NE
    CASE "CS" : lookupCond% = COND_CS
    CASE "CC" : lookupCond% = COND_CC
    CASE "MI" : lookupCond% = COND_MI
    CASE "PL" : lookupCond% = COND_PL
    CASE "VS" : lookupCond% = COND_VS
    CASE "VC" : lookupCond% = COND_VC
    CASE "HI" : lookupCond% = COND_HI
    CASE "LS" : lookupCond% = COND_LS
    CASE "GE" : lookupCond% = COND_GE
    CASE "LT" : lookupCond% = COND_LT
    CASE "GT" : lookupCond% = COND_GT
    CASE "LE" : lookupCond% = COND_LE
    CASE ELSE  : lookupCond% = COND_NONE
  END SELECT
END FUNCTION

' Return FLAG_xx constant, or FLAG_NONE.
' Caller must pass an upper-case string.
FUNCTION lookupFlag%(s$)
  SELECT CASE s$
    CASE "S"  : lookupFlag% = FLAG_S
    CASE "X"  : lookupFlag% = FLAG_X
    CASE "R"  : lookupFlag% = FLAG_R
    CASE "DB" : lookupFlag% = FLAG_DB
    CASE "IA" : lookupFlag% = FLAG_IA
    CASE ELSE  : lookupFlag% = FLAG_NONE
  END SELECT
END FUNCTION

' Return SHIFT_xx constant, or SHIFT_NONE.
' Caller must pass an upper-case string.
FUNCTION lookupShift%(s$)
  SELECT CASE s$
    CASE "LSL" : lookupShift% = SHIFT_LSL
    CASE "LSR" : lookupShift% = SHIFT_LSR
    CASE "ASR" : lookupShift% = SHIFT_ASR
    CASE "ROR" : lookupShift% = SHIFT_ROR
    CASE "RRX" : lookupShift% = SHIFT_RRX
    CASE ELSE   : lookupShift% = SHIFT_NONE
  END SELECT
END FUNCTION

' ============================================================
' Mnemonic data table  -  195 entries, indices 1..195
' Read by initMnemonics() via RESTORE mne_data / READ
' ============================================================
mne_data:
' 7-character mnemonics (12)
DATA "SHADD16","SHSUB16","SMLALBB","SMLALBT","SMLALTB","SMLALTT","SXTAB16"
DATA "UHADD16","UHSUB16","UQADD16","UQSUB16","UXTAB16"
' 6-character mnemonics (40)
DATA "LDREXB","LDREXD","LDREXH","LDRSBT","LDRSHT","QADD16","QSUB16","SADD16"
DATA "SETEND","SHADD8","SHSUB8","SMLABB","SMLABT","SMLALD","SMLATB","SMLATT"
DATA "SMLAWB","SMLAWT","SMLSLD","SMULBB","SMULBT","SMULTB","SMULTT","SMULWB"
DATA "SMULWT","SSAT16","SSUB16","STREXB","STREXD","STREXH","SXTB16","UADD16"
DATA "UHADD8","UHSUB8","UQADD8","UQSUB8","USADA8","USAT16","USUB16","UXTB16"
' 5-character mnemonics (49)
DATA "CLREX","LDMDB","LDMIA","LDRBT","LDREX","LDRHT","LDRSB","LDRSH"
DATA "PKHBT","PKHTB","QADD8","QDADD","QDSUB","QSUB8","REV16","REVSH"
DATA "SADD8","SHASX","SHSAX","SMLAD","SMLAL","SMLSD","SMMLA","SMMLS"
DATA "SMMUL","SMUAD","SMULL","SMUSD","SSUB8","STMDB","STMIA","STRBT"
DATA "STREX","STRHT","SXTAB","SXTAH","UADD8","UHASX","UHSAX","UMAAL"
DATA "UMLAL","UMULL","UQASX","UQSAX","USAD8","USUB8","UXTAB","UXTAH"
DATA "YIELD"
' 4-character mnemonics (35)
DATA "ADDW","BKPT","CBNZ","LDRB","LDRD","LDRH","LDRT","MOVT","MOVW"
DATA "PUSH","QADD","QASX","QSAX","QSUB","RBIT","SASX","SBFX","SDIV"
DATA "SSAT","SSAX","STRB","STRD","STRH","STRT","SUBS","SUBW","SXTB"
DATA "SXTH","UASX","UBFX","UDIV","USAT","USAX","UXTB","UXTH"
' 3-character mnemonics (55)
DATA "ADC","ADD","ADR","AND","ASR","BFC","BFI","BIC","BLX","BXJ"
DATA "CBZ","CLZ","CMN","CMP","CPS","DBG","DMB","DSB","EOR","ISB"
DATA "LDR","LSL","LSR","MLA","MLS","MOV","MRS","MSR","MUL","MVN"
DATA "NOP","ORN","ORR","PLD","PLI","POP","REV","RFE","ROR","RRX"
DATA "RSB","SBC","SEL","SEV","SMC","SRS","STR","SUB","SVC","TBB"
DATA "TBH","TEQ","TST","WFE","WFI","LDM","STM"
' 2- and 1-character mnemonics (4)
DATA "BL","BX","IT","B"
