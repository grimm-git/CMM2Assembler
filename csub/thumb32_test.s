@ ============================================================
@ test_thumb32.s  -  Coverage of 32-bit Thumb-2 mnemonics for
@ the Cortex-M7. Every mnemonic appears at least once with a
@ valid argument setting that the assembler must encode as
@ a 32-bit instruction.  Where a @ mnemonic also has a 16-bit
@ form, the .W suffix forces the @ wide (32-bit) encoding.
@
@ Built with:
@   arm-none-eabi-as -mcpu=cortex-m7 -mthumb \
@                    -o thumb32_test.o thumb32_test.s
@ ============================================================

        .syntax unified
        .thumb
        .cpu    cortex-m7
        .fpu    softvfp
        .align

        .equ    IMM12,    0xABC
        .equ    BIG32,    0x12345678

start:

@ =====================================================================
@ Branches and hints (T3/T4 encodings)
@ =====================================================================

        B.W     fwd                     @ T4 unconditional
        BEQ.W   fwd                     @ T3 conditional
        BNE.W   fwd
        BCS.W   fwd
        BCC.W   fwd
        BMI.W   fwd
        BPL.W   fwd
        BVS.W   fwd
        BVC.W   fwd
        BHI.W   fwd
        BLS.W   fwd
        BGE.W   fwd
        BLT.W   fwd
        BGT.W   fwd
        BLE.W   fwd
        BL      fwd                     @ T1 (always 32-bit)

        NOP.W
        YIELD.W
        WFE.W
        WFI.W
        SEV.W
        DBG     #0xF                    @ debug hint

@ =====================================================================
@ Data-processing, modified immediate (T1/T3)
@ =====================================================================

        ADC     R0, R1, #0x55
        ADD.W   R0, R1, #0x100
        ADD     R0, R1, #0x10000        @ T4 plain imm12 (encoded wide)
        ADDW    R0, R1, #IMM12          @ T4 explicit
        AND     R0, R1, #0xFF00FF00
        BIC     R0, R1, #0x0F0F0F0F
        CMN.W   R0, #0x80
        CMP.W   R0, #0x1200             @ modified-imm: 0x12 ror
        EOR     R0, R1, #0xAA
        MOV.W   R0, #0xFF00FF00
        MOVW    R0, #0xBEEF             @ T3 16-bit imm
        MOVT    R0, #0xDEAD
        MVN     R0, #0x0F
        ORN     R0, R1, #0x33
        ORR     R0, R1, #0x77
        RSB     R0, R1, #5              @ T2 (32-bit)
        SBC     R0, R1, #1
        SUB.W   R0, R1, #0x80
        SUBW    R0, R1, #IMM12          @ T4 plain imm12
        TEQ     R0, #0x10
        TST.W   R0, #0xFF

@ =====================================================================
@ Data-processing, register (T2 with optional shifted operand)
@ =====================================================================

        ADC.W   R0, R1, R2
        ADC.W   R0, R1, R2, LSL #3
        ADD.W   R0, R1, R2, ASR #4
        AND.W   R0, R1, R2, LSR #2
        BIC.W   R0, R1, R2, ROR #1
        CMN.W   R0, R1, LSL #2
        CMP.W   R0, R1, RRX
        EOR.W   R0, R1, R2
        MOV.W   R0, R1                  @ T3
        MVN.W   R0, R1, ASR #5
        ORN     R0, R1, R2
        ORR.W   R0, R1, R2, LSL #8
        PKHBT   R0, R1, R2, LSL #4
        PKHTB   R0, R1, R2, ASR #16
        RSB.W   R0, R1, R2, LSR #1
        SBC.W   R0, R1, R2
        SUB.W   R0, R1, R2, LSL #2
        TEQ     R0, R1, ASR #2
        TST.W   R0, R1, ROR #3

@ =====================================================================
@ Plain shifts (register, T2 32-bit form)
@ =====================================================================

        ASR.W   R0, R1, #5
        ASR.W   R0, R1, R2
        LSL.W   R0, R1, #5
        LSL.W   R0, R1, R2
        LSR.W   R0, R1, #5
        LSR.W   R0, R1, R2
        ROR.W   R0, R1, #5
        ROR.W   R0, R1, R2
        RRX     R0, R1

@ =====================================================================
@ Sign / zero extend with rotation and add variants
@ =====================================================================

        SXTB.W  R0, R1
        SXTB.W  R0, R1, ROR #8
        SXTH.W  R0, R1, ROR #16
        UXTB.W  R0, R1, ROR #24
        UXTH.W  R0, R1
        SXTB16  R0, R1, ROR #8
        UXTB16  R0, R1
        SXTAB   R0, R1, R2
        SXTAB16 R0, R1, R2, ROR #8
        SXTAH   R0, R1, R2, ROR #16
        UXTAB   R0, R1, R2
        UXTAB16 R0, R1, R2
        UXTAH   R0, R1, R2, ROR #24

@ =====================================================================
@ Bit-field, count, reverse
@ =====================================================================

        BFC     R0, #4, #8
        BFI     R0, R1, #4, #8
        SBFX    R0, R1, #4, #8
        UBFX    R0, R1, #4, #8
        CLZ     R0, R1
        RBIT    R0, R1
        REV.W   R0, R1
        REV16.W R0, R1
        REVSH.W R0, R1

@ =====================================================================
@ Multiply (32-bit)
@ =====================================================================

        MUL.W   R0, R1, R2
        MLA     R0, R1, R2, R3
        MLS     R0, R1, R2, R3

@ DSP signed/unsigned 16-bit multiplies (BB/BT/TB/TT)
        SMULBB  R0, R1, R2
        SMULBT  R0, R1, R2
        SMULTB  R0, R1, R2
        SMULTT  R0, R1, R2
        SMLABB  R0, R1, R2, R3
        SMLABT  R0, R1, R2, R3
        SMLATB  R0, R1, R2, R3
        SMLATT  R0, R1, R2, R3

@ DSP wide-by-half multiplies
        SMULWB  R0, R1, R2
        SMULWT  R0, R1, R2
        SMLAWB  R0, R1, R2, R3
        SMLAWT  R0, R1, R2, R3

@ Dual 16x16 multiplies
        SMUAD   R0, R1, R2
        SMUADX  R0, R1, R2
        SMUSD   R0, R1, R2
        SMUSDX  R0, R1, R2
        SMLAD   R0, R1, R2, R3
        SMLADX  R0, R1, R2, R3
        SMLSD   R0, R1, R2, R3
        SMLSDX  R0, R1, R2, R3

@ Most-significant-word multiplies
        SMMUL   R0, R1, R2
        SMMULR  R0, R1, R2
        SMMLA   R0, R1, R2, R3
        SMMLAR  R0, R1, R2, R3
        SMMLS   R0, R1, R2, R3
        SMMLSR  R0, R1, R2, R3

@ Sum of absolute differences
        USAD8   R0, R1, R2
        USADA8  R0, R1, R2, R3

@ =====================================================================
@ Long multiply / multiply-accumulate (RdLo, RdHi, Rn, Rm)
@ =====================================================================

        SMULL   R0, R1, R2, R3
        UMULL   R0, R1, R2, R3
        SMLAL   R0, R1, R2, R3
        UMLAL   R0, R1, R2, R3
        UMAAL   R0, R1, R2, R3

        SMLALBB R0, R1, R2, R3
        SMLALBT R0, R1, R2, R3
        SMLALTB R0, R1, R2, R3
        SMLALTT R0, R1, R2, R3
        SMLALD  R0, R1, R2, R3
        SMLALDX R0, R1, R2, R3
        SMLSLD  R0, R1, R2, R3
        SMLSLDX R0, R1, R2, R3

@ =====================================================================
@ Divide
@ =====================================================================

        SDIV    R0, R1, R2
        UDIV    R0, R1, R2

@ =====================================================================
@ Saturating arithmetic
@ =====================================================================

        SSAT    R0, #16, R1
        SSAT    R0, #12, R1, LSL #4
        SSAT    R0, #12, R1, ASR #4
        USAT    R0, #16, R1
        USAT    R0, #12, R1, LSL #4
        SSAT16  R0, #12, R1
        USAT16  R0, #12, R1
        QADD    R0, R1, R2
        QSUB    R0, R1, R2
        QDADD   R0, R1, R2
        QDSUB   R0, R1, R2

@ =====================================================================
@ Parallel add/subtract (signed, unsigned, saturating, halving)
@ =====================================================================

        SADD16  R0, R1, R2
        SADD8   R0, R1, R2
        SSUB16  R0, R1, R2
        SSUB8   R0, R1, R2
        SASX    R0, R1, R2
        SSAX    R0, R1, R2

        UADD16  R0, R1, R2
        UADD8   R0, R1, R2
        USUB16  R0, R1, R2
        USUB8   R0, R1, R2
        UASX    R0, R1, R2
        USAX    R0, R1, R2

        QADD16  R0, R1, R2
        QADD8   R0, R1, R2
        QSUB16  R0, R1, R2
        QSUB8   R0, R1, R2
        QASX    R0, R1, R2
        QSAX    R0, R1, R2

        UQADD16 R0, R1, R2
        UQADD8  R0, R1, R2
        UQSUB16 R0, R1, R2
        UQSUB8  R0, R1, R2
        UQASX   R0, R1, R2
        UQSAX   R0, R1, R2

        SHADD16 R0, R1, R2
        SHADD8  R0, R1, R2
        SHSUB16 R0, R1, R2
        SHSUB8  R0, R1, R2
        SHASX   R0, R1, R2
        SHSAX   R0, R1, R2

        UHADD16 R0, R1, R2
        UHADD8  R0, R1, R2
        UHSUB16 R0, R1, R2
        UHSUB8  R0, R1, R2
        UHASX   R0, R1, R2
        UHSAX   R0, R1, R2

        SEL     R0, R1, R2

@ =====================================================================
@ Address generation
@ =====================================================================

        ADR.W   R0, fwd                 @ wide ADR (T2/T3)

@ =====================================================================
@ Load / store, immediate (T3 imm12 and T4 imm8 +/-, pre/post-index)
@ =====================================================================

        LDR.W   R0, [R1, #0x100]
        LDR.W   R0, [R1, #-20]
        LDR.W   R0, [R1, #4]!
        LDR.W   R0, [R1], #4
        LDR.W   R0, [PC, #16]           @ literal T2
        LDR.W   R0, =BIG32              @ literal pool

        LDRB.W  R0, [R1, #0x100]
        LDRB.W  R0, [R1, #-1]
        LDRB.W  R0, [R1, #1]!
        LDRB.W  R0, [R1], #1
        LDRSB   R0, [R1, #0x100]
        LDRSB   R0, [R1, #-1]
        LDRSB   R0, [R1, #1]!
        LDRSB   R0, [R1], #1

        LDRH.W  R0, [R1, #0x100]
        LDRH.W  R0, [R1, #-2]
        LDRH.W  R0, [R1, #2]!
        LDRH.W  R0, [R1], #2
        LDRSH   R0, [R1, #0x100]
        LDRSH   R0, [R1, #-2]
        LDRSH   R0, [R1, #2]!
        LDRSH   R0, [R1], #2

        STR.W   R0, [R1, #0x100]
        STR.W   R0, [R1, #-20]
        STR.W   R0, [R1, #4]!
        STR.W   R0, [R1], #4
        STRB.W  R0, [R1, #0x100]
        STRB.W  R0, [R1, #-1]
        STRB.W  R0, [R1, #1]!
        STRB.W  R0, [R1], #1
        STRH.W  R0, [R1, #0x100]
        STRH.W  R0, [R1, #-2]
        STRH.W  R0, [R1, #2]!
        STRH.W  R0, [R1], #2

@ Unprivileged variants
        LDRT    R0, [R1, #4]
        LDRBT   R0, [R1, #4]
        LDRSBT  R0, [R1, #4]
        LDRHT   R0, [R1, #4]
        LDRSHT  R0, [R1, #4]
        STRT    R0, [R1, #4]
        STRBT   R0, [R1, #4]
        STRHT   R0, [R1, #4]

@ Register offset (T2: shifted Rm, imm2)
        LDR.W   R0, [R1, R2, LSL #2]
        LDRB.W  R0, [R1, R2, LSL #1]
        LDRSB   R0, [R1, R2, LSL #3]
        LDRH.W  R0, [R1, R2, LSL #2]
        LDRSH   R0, [R1, R2, LSL #1]
        STR.W   R0, [R1, R2, LSL #2]
        STRB.W  R0, [R1, R2, LSL #1]
        STRH.W  R0, [R1, R2, LSL #2]

@ Doubleword load / store
        LDRD    R0, R1, [R2, #16]
        LDRD    R0, R1, [R2, #16]!
        LDRD    R0, R1, [R2], #16
        LDRD    R0, R1, [PC, #8]
        STRD    R0, R1, [R2, #16]
        STRD    R0, R1, [R2, #16]!
        STRD    R0, R1, [R2], #16

@ Exclusive access
        LDREX   R0, [R1]
        LDREXB  R0, [R1]
        LDREXH  R0, [R1]
        STREX   R0, R1, [R2]
        STREXB  R0, R1, [R2]
        STREXH  R0, R1, [R2]
        CLREX

@ Preload hints
        PLD     [R0, #16]
        PLD     [R0, R1, LSL #2]
        PLD     fwd
        PLI     [R0, #16]
        PLI     [R0, R1, LSL #2]
        PLI     fwd

@ =====================================================================
@ Load / store multiple (T2 forms)
@ =====================================================================

        LDM.W   R0, {R1, R2, R3}
        LDMIA.W R0!, {R1-R5}
        LDMDB   R0, {R1, R2, R3}
        LDMDB   R0!, {R1-R5}
        STM.W   R0, {R1, R2, R3}
        STMIA.W R0!, {R1-R5}
        STMDB   R0, {R1, R2, R3}
        STMDB   R0!, {R1-R5}

        PUSH.W  {R4}                    @ T1 single-reg wide
        POP.W   {R4}
        PUSH    {R0-R7, R8, LR}         @ wide multi-reg
        POP     {R0-R7, R8, PC}

@ =====================================================================
@ Table branch
@ =====================================================================

        TBB     [PC, R0]
        TBH     [PC, R0, LSL #1]
        TBB     [R1, R0]
        TBH     [R1, R0, LSL #1]

@ =====================================================================
@ System / supervisor
@ =====================================================================

        MRS     R0, APSR
        MRS     R0, IPSR
        MRS     R0, PSR
        MRS     R0, MSP
        MRS     R0, PSP
        MRS     R0, PRIMASK
        MRS     R0, FAULTMASK
        MRS     R0, BASEPRI
        MRS     R0, CONTROL
        MSR     APSR_nzcvq, R0
        MSR     PRIMASK, R0
        MSR     FAULTMASK, R0
        MSR     BASEPRI, R0
        MSR     BASEPRI_MAX, R0
        MSR     CONTROL, R0

        CPSIE   I
        CPSID   F
        CPSIE   IF

        DMB     SY
        DMB     ISH
        DSB     SY
        DSB     ISH
        ISB     SY

@ =====================================================================
@ Branch target
@ =====================================================================

        .align  2
fwd:
        NOP.W
        BX      LR

        .end
