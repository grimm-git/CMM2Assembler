@ ============================================================
@ test_thumb32_test2.s  -  Randomized variant of test_thumb32.s
@ Same 32-bit Thumb-2 mnemonics, instructions shuffled, with
@ R0 -> R3 and R1 -> R4 throughout.  Register-list contents
@ inside { ... } are left untouched.
@ ============================================================

        .syntax unified
        .thumb
        .cpu    cortex-m7
        .fpu    softvfp
        .align

        .equ    IMM12,    0xABC
        .equ    BIG32,    0x12345678

start:

        STRD    R3, R4, [R2, #16]
        DMB     ISH
        UQADD16 R3, R4, R2
        WFI.W
        SMLABB  R3, R4, R2, R3
        ADD     R3, R4, #0x10000        @ T4 plain imm12 (encoded wide)
        UQSAX   R3, R4, R2
        SADD16  R3, R4, R2
        SHASX   R3, R4, R2
        MSR     CONTROL, R3
        LDREX   R3, [R4]
        SMLAWT  R3, R4, R2, R3
        STRB.W  R3, [R4], #1
        LDRD    R3, R4, [R2, #16]!
        STRH.W  R3, [R4, R2, LSL #2]
        UXTAB16 R3, R4, R2
        SXTH.W  R3, R4, ROR #16
        BEQ.W   fwd                     @ T3 conditional
        PUSH.W  {R4}                    @ T1 single-reg wide
        LDR.W   R3, [R4, #4]!
        STRB.W  R3, [R4, R2, LSL #1]
        LDRD    R3, R4, [R2, #16]
        UHSUB16 R3, R4, R2
        LDRB.W  R3, [R4, R2, LSL #1]
        PKHBT   R3, R4, R2, LSL #4
        BMI.W   fwd
        LDR.W   R3, [R4, #0x100]
        LSL.W   R3, R4, R2
        USAD8   R3, R4, R2
        LDRT    R3, [R4, #4]
        SMLALTT R3, R4, R2, R3
        SMULTT  R3, R4, R2
        SMLALTB R3, R4, R2, R3
        RSB     R3, R4, #5              @ T2 (32-bit)
        SMULBB  R3, R4, R2
        RBIT    R3, R4
        SBC     R3, R4, #1
        STRBT   R3, [R4, #4]
        MRS     R3, APSR
        LDRHT   R3, [R4, #4]
        CMN.W   R3, R4, LSL #2
        SHSUB8  R3, R4, R2
        LDMIA.W R3!, {R1-R5}
        REV16.W R3, R4
        MRS     R3, BASEPRI
        UQSUB16 R3, R4, R2
        BPL.W   fwd
        LDR.W   R3, [R4, #-20]
        AND     R3, R4, #0xFF00FF00
        SBFX    R3, R4, #4, #8
        TST.W   R3, R4, ROR #3
        SMLADX  R3, R4, R2, R3
        LDRBT   R3, [R4, #4]
        UHADD8  R3, R4, R2
        SMMLS   R3, R4, R2, R3
        TBH     [R4, R3, LSL #1]
        MVN     R3, #0x0F
        MRS     R3, FAULTMASK
        SXTAB   R3, R4, R2
        SADD8   R3, R4, R2
        STRD    R3, R4, [R2, #16]!
        POP     {R0-R7, R8, PC}
        MOVW    R3, #0xBEEF             @ T3 16-bit imm
        LDRSBT  R3, [R4, #4]
        TST.W   R3, #0xFF
        BCC.W   fwd
        QASX    R3, R4, R2
        LDRH.W  R3, [R4, #2]!
        DMB     SY
        BHI.W   fwd
        UHSAX   R3, R4, R2
        USUB8   R3, R4, R2
        QDADD   R3, R4, R2
        SMMLSR  R3, R4, R2, R3
        USADA8  R3, R4, R2, R3
        MRS     R3, PSP
        SMULWT  R3, R4, R2
        SSAT16  R3, #12, R4
        MOV.W   R3, #0xFF00FF00
        MSR     FAULTMASK, R3
        ISB     SY
        LSR.W   R3, R4, #5
        LDRD    R3, R4, [PC, #8]
        STMDB   R3!, {R1-R5}
        LDRSB   R3, [R4], #1
        BFI     R3, R4, #4, #8
        MOV.W   R3, R4                  @ T3
        DSB     SY
        LDRH.W  R3, [R4, R2, LSL #2]
        CLZ     R3, R4
        LDRSH   R3, [R4, #-2]
        SMLALDX R3, R4, R2, R3
        ADR.W   R3, fwd                 @ wide ADR (T2/T3)
        UXTB16  R3, R4
        UDIV    R3, R4, R2
        MOVT    R3, #0xDEAD
        DBG     #0xF                    @ debug hint
        RRX     R3, R4
        TEQ     R3, R4, ASR #2
        BFC     R3, #4, #8
        UHASX   R3, R4, R2
        UADD8   R3, R4, R2
        UADD16  R3, R4, R2
        QADD    R3, R4, R2
        TBB     [R4, R3]
        LDRSB   R3, [R4, #1]!
        SMULWB  R3, R4, R2
        SMULL   R3, R4, R2, R3
        STRT    R3, [R4, #4]
        PUSH    {R0-R7, R8, LR}         @ wide multi-reg
        BLS.W   fwd
        SXTB.W  R3, R4, ROR #8
        SSUB8   R3, R4, R2
        SMLSDX  R3, R4, R2, R3
        BVS.W   fwd
        EOR     R3, R4, #0xAA
        PLD     [R3, #16]
        SMLATT  R3, R4, R2, R3
        SMLAWB  R3, R4, R2, R3
        MRS     R3, MSP
        AND.W   R3, R4, R2, LSR #2
        LDRSH   R3, [R4], #2
        LDRSB   R3, [R4, #-1]
        POP.W   {R4}
        STR.W   R3, [R4, #-20]
        LDREXB  R3, [R4]
        SMLALD  R3, R4, R2, R3
        SSAT    R3, #16, R4
        STR.W   R3, [R4, #0x100]
        MLS     R3, R4, R2, R3
        STRB.W  R3, [R4, #-1]
        EOR.W   R3, R4, R2
        SASX    R3, R4, R2
        SXTB16  R3, R4, ROR #8
        BIC     R3, R4, #0x0F0F0F0F
        SMLALBB R3, R4, R2, R3
        ADD.W   R3, R4, R2, ASR #4
        UMAAL   R3, R4, R2, R3
        LDRH.W  R3, [R4, #0x100]
        B.W     fwd                     @ T4 unconditional
        LDR.W   R3, [R4, R2, LSL #2]
        USAX    R3, R4, R2
        MSR     APSR_nzcvq, R3
        PLD     [R3, R4, LSL #2]
        LDRSH   R3, [R4, #0x100]
        UBFX    R3, R4, #4, #8
        PLI     [R3, R4, LSL #2]
        LDRH.W  R3, [R4], #2
        CMP.W   R3, #0x1200             @ modified-imm: 0x12 ror
        BNE.W   fwd
        PLD     fwd
        LSL.W   R3, R4, #5
        LDMDB   R3, {R1, R2, R3}
        MRS     R3, CONTROL
        STREXH  R3, R4, [R2]
        LDRSH   R3, [R4, #2]!
        SMULTB  R3, R4, R2
        LDR.W   R3, [PC, #16]           @ literal T2
        SSUB16  R3, R4, R2
        SMUAD   R3, R4, R2
        PLI     [R3, #16]
        CLREX
        SUB.W   R3, R4, #0x80
        CMN.W   R3, #0x80
        DSB     ISH
        ADD.W   R3, R4, #0x100
        SMLAL   R3, R4, R2, R3
        SMLALBT R3, R4, R2, R3
        STREX   R3, R4, [R2]
        LDRH.W  R3, [R4, #-2]
        LDREXH  R3, [R4]
        STMDB   R3, {R1, R2, R3}
        STRH.W  R3, [R4, #-2]
        SMUADX  R3, R4, R2
        QDSUB   R3, R4, R2
        STRD    R3, R4, [R2], #16
        SDIV    R3, R4, R2
        USAT    R3, #12, R4, LSL #4
        PLI     fwd
        ORN     R3, R4, #0x33
        LSR.W   R3, R4, R2
        ORR     R3, R4, #0x77
        SMMULR  R3, R4, R2
        QSUB16  R3, R4, R2
        CPSIE   I
        UMLAL   R3, R4, R2, R3
        QSAX    R3, R4, R2
        LDR.W   R3, [R4], #4
        LDRB.W  R3, [R4, #-1]
        STM.W   R3, {R1, R2, R3}
        USAT16  R3, #12, R4
        ORR.W   R3, R4, R2, LSL #8
        MSR     BASEPRI, R3
        LDRSH   R3, [R4, R2, LSL #1]
        SMLATB  R3, R4, R2, R3
        SXTAH   R3, R4, R2, ROR #16
        BVC.W   fwd
        LDRD    R3, R4, [R2], #16
        BLE.W   fwd
        LDRSB   R3, [R4, #0x100]
        LDRB.W  R3, [R4, #1]!
        UXTAH   R3, R4, R2, ROR #24
        SHADD8  R3, R4, R2
        PKHTB   R3, R4, R2, ASR #16
        TBB     [PC, R3]
        LDMDB   R3!, {R1-R5}
        QADD8   R3, R4, R2
        ROR.W   R3, R4, R2
        LDRSB   R3, [R4, R2, LSL #3]
        SMMLA   R3, R4, R2, R3
        STREXB  R3, R4, [R2]
        ASR.W   R3, R4, #5
        SEL     R3, R4, R2
        SMLSLDX R3, R4, R2, R3
        ADC.W   R3, R4, R2
        MRS     R3, PSR
        SSAX    R3, R4, R2
        WFE.W
        QSUB8   R3, R4, R2
        TBH     [PC, R3, LSL #1]
        SHSAX   R3, R4, R2
        ROR.W   R3, R4, #5
        UQSUB8  R3, R4, R2
        ORN     R3, R4, R2
        REV.W   R3, R4
        MLA     R3, R4, R2, R3
        TEQ     R3, #0x10
        MUL.W   R3, R4, R2
        STR.W   R3, [R4], #4
        QADD16  R3, R4, R2
        SMMUL   R3, R4, R2
        CPSIE   IF
        SMULBT  R3, R4, R2
        ADDW    R3, R4, #IMM12          @ T4 explicit
        STRH.W  R3, [R4], #2
        SUB.W   R3, R4, R2, LSL #2
        STRH.W  R3, [R4, #0x100]
        SEV.W
        UXTB.W  R3, R4, ROR #24
        LDRB.W  R3, [R4], #1
        SBC.W   R3, R4, R2
        UQADD8  R3, R4, R2
        BGE.W   fwd
        YIELD.W
        UHADD16 R3, R4, R2
        STMIA.W R3!, {R1-R5}
        QSUB    R3, R4, R2
        REVSH.W R3, R4
        STRH.W  R3, [R4, #2]!
        LDRSHT  R3, [R4, #4]
        USUB16  R3, R4, R2
        UASX    R3, R4, R2
        STR.W   R3, [R4, #4]!
        UXTH.W  R3, R4
        SSAT    R3, #12, R4, ASR #4
        SUBW    R3, R4, #IMM12          @ T4 plain imm12
        LDR.W   R3, =BIG32              @ literal pool
        ASR.W   R3, R4, R2
        STR.W   R3, [R4, R2, LSL #2]
        ADC     R3, R4, #0x55
        SMLSLD  R3, R4, R2, R3
        SHSUB16 R3, R4, R2
        UHSUB8  R3, R4, R2
        CMP.W   R3, R4, RRX
        LDRB.W  R3, [R4, #0x100]
        MSR     PRIMASK, R3
        CPSID   F
        UQASX   R3, R4, R2
        SMUSD   R3, R4, R2
        SXTAB16 R3, R4, R2, ROR #8
        MRS     R3, IPSR
        SHADD16 R3, R4, R2
        MSR     BASEPRI_MAX, R3
        UXTAB   R3, R4, R2
        BCS.W   fwd
        USAT    R3, #16, R4
        STRHT   R3, [R4, #4]
        SMLAD   R3, R4, R2, R3
        STRB.W  R3, [R4, #0x100]
        SMLABT  R3, R4, R2, R3
        BGT.W   fwd
        LDM.W   R3, {R1, R2, R3}
        SMMLAR  R3, R4, R2, R3
        SMUSDX  R3, R4, R2
        BIC.W   R3, R4, R2, ROR #1
        BL      fwd                     @ T1 (always 32-bit)
        NOP.W
        STRB.W  R3, [R4, #1]!
        ADC.W   R3, R4, R2, LSL #3
        MRS     R3, PRIMASK
        MVN.W   R3, R4, ASR #5
        SXTB.W  R3, R4
        SMLSD   R3, R4, R2, R3
        UMULL   R3, R4, R2, R3
        SSAT    R3, #12, R4, LSL #4
        BLT.W   fwd
        RSB.W   R3, R4, R2, LSR #1

        .align  2
fwd:
        NOP.W
        BX      LR

        .end
