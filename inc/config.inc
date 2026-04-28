' ============================================================
' config.inc  -  Global constants and shared variable declarations
' Must be the FIRST file included by assembler.bas
' ============================================================

' --- Array size limits ---
CONST MAX_NAME_LEN = 31      ' max identifier / label name length

' --- Token type codes ---
CONST TT_NONE   = 0
CONST TT_MNE    = 1    ' mnemonic
CONST TT_FLAG   = 2    ' S / X / R / DB / IA
CONST TT_COND   = 3    ' condition code
CONST TT_SIZE   = 4    ' .N or .W
CONST TT_REGLO  = 5    ' register R0-R7
CONST TT_REGHI  = 6    ' register R8-R15
CONST TT_IMM    = 7    ' immediate value (#n)
CONST TT_ADDR   = 8    ' label / address reference
CONST TT_LIT    = 9    ' literal pool index  (LDR Rn,=val)
CONST TT_LBRA   = 10   ' [
CONST TT_RBRA   = 11   ' ]
CONST TT_LBRACE = 12   ' {
CONST TT_RBRACE = 13   ' }
CONST TT_RLIST  = 14   ' resolved register-list bitmask
CONST TT_COMMA  = 15   ' ,
CONST TT_SHIFT  = 16   ' shift operator (LSL/LSR/ASR/ROR/RRX)
CONST TT_DIR    = 17   ' assembler directive (.equ/.space/etc.)
CONST TT_BANG   = 18   ' '!' write-back modifier (post-bracket)

' --- Flag codes (optional suffix on mnemonic) ---
CONST FLAG_NONE = 0
CONST FLAG_S    = 1    ' S  – update condition flags
CONST FLAG_X    = 2    ' X
CONST FLAG_R    = 3    ' R
CONST FLAG_DB   = 4    ' DB – decrement before
CONST FLAG_IA   = 5    ' IA – increment after

' --- Condition codes ---
CONST COND_NONE = 0
CONST COND_EQ   = 1    ' Z=1
CONST COND_NE   = 2    ' Z=0
CONST COND_CS   = 3    ' C=1
CONST COND_CC   = 4    ' C=0
CONST COND_MI   = 5    ' N=1
CONST COND_PL   = 6    ' N=0
CONST COND_VS   = 7    ' V=1
CONST COND_VC   = 8    ' V=0
CONST COND_HI   = 9    ' C=1 AND Z=0
CONST COND_LS   = 10   ' C=0 OR  Z=1
CONST COND_GE   = 11   ' N=V
CONST COND_LT   = 12   ' N<>V
CONST COND_GT   = 13   ' Z=0 AND N=V
CONST COND_LE   = 14   ' Z=1 OR  N<>V

' --- Size qualifier codes ---
CONST SIZE_AUTO = 0    ' omitted  – assembler chooses (prefers 16-bit)
CONST SIZE_N    = 1    ' .N       – narrow, prefer 16-bit
CONST SIZE_W    = 2    ' .W       – wide,   force  32-bit

' --- Shift operator codes ---
CONST SHIFT_NONE = 0
CONST SHIFT_LSL  = 1
CONST SHIFT_LSR  = 2
CONST SHIFT_ASR  = 3
CONST SHIFT_ROR  = 4
CONST SHIFT_RRX  = 5

