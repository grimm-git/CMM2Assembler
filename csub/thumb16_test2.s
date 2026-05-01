@ ============================================================
@ test_thumb16_test2.s  -  Randomized variant of test_thumb16.s
@ Same 16-bit Thumb mnemonics, instructions shuffled, with
@ R0 -> R3 and R1 -> R4 throughout.  R3-R3 (PUSH/POP) collapsed
@ to single register R3.
@ ============================================================

.syntax unified
.thumb

.equ    CONST8,   0x42
.equ    CONST_W,  0x12345678            @ 32-bit so LDR =CONST_W stays in pool

start:
        ADR     R4, lit_pool            @ positive offset, should be T1
        B       next
.align 2
lit_pool:
        .word   0xDEADBEEF
        .word   CONST_W
next:
        ADR.W   R4, lit_pool            @ negative offset, must be T2

        SEV
        LSLS    R3, R4
        LDR     R3, [R4, R2]            @ register
        ANDS    R3, R4
        ADD     R3, PC, #8              @ Rd, PC, #imm8 (T1 ADR alias)
        SXTH    R3, R4
        SVC     #0x10
        UXTB    R3, R4
        LDRH    R3, [R4, R2]
        LDR     R3, =CONST_W            @ literal pool (16-bit T1)
        CMP     R8, R9                  @ high-register T2
        NOP
        POP     {R3, PC}
        SUBS    R3, R4, #5              @ imm3
        STRH    R3, [R4, R2]
        SBCS    R3, R4
        STR     R3, [SP, #16]
        STRH    R3, [R4, #6]
        WFE
        LSRS    R3, R4, #3
        LSRS    R3, R4
        LDRB    R3, [R4, #5]
        STRB    R3, [R4, R2]
        ADDS    R3, R4, R2              @ register
        PUSH    {R3-R7, LR}
        REVSH   R3, R4
        LDMIA   R3!, {R4, R2, R3}
        STRB    R3, [R4, #5]
        MOV     R8, R9                  @ high-register T1
        SXTB    R3, R4
        MVNS    R3, R4
        EORS    R3, R4
        SUB     SP, SP, #16             @ SP-relative imm7*4
        MOVS    R3, R4                  @ low-low (LSL #0 alias, T2)
        STR     R3, [R4, R2]
        MULS    R3, R4, R3              @ T1: Rdm = Rn * Rdm
        REV16   R3, R4
        ADD     SP, SP, #16             @ SP-relative imm7*4
        LDRH    R3, [R4, #6]            @ imm5*2
        REV     R3, R4
        ASRS    R3, R4, #5              @ immediate shift
        STR     R3, [R4, #20]
        MOVS    R3, #200                @ imm8
        RORS    R3, R4
        ASRS    R3, R4                  @ register shift
        ADCS    R3, R4                  @ Rd = Rd + Rm + C
        YIELD
        LDR     R3, [PC, #16]           @ literal
        BKPT    #0xAB
        UXTH    R3, R4
        ORRS    R3, R4
        TST     R3, R4
        LDRSB   R3, [R4, R2]
        LDRB    R3, [R4, R2]
        LDRSH   R3, [R4, R2]
        CMN     R3, R4
        SETEND  BE
        ADDS    R3, R4, #5              @ imm3
        LDR     R3, [SP, #16]           @ SP-relative
        SUBS    R3, #200                @ imm8 to Rdn
        IT      EQ
        MOVEQ   R3, R4                  @ single-instruction IT body
        WFI
        ADD     R3, SP, #20             @ Rd, SP, #imm8
        SETEND  LE
        ADD     R8, R9                  @ high-register form (no S)
        STMIA   R3!, {R4, R2, R3}
        BICS    R3, R4
        CMP     R3, #100                @ imm8
        LSLS    R3, R4, #3
        RSBS    R3, R4, #0              @ NEG alias, T1
        SUBS    R3, R4, R2              @ register
        LDR     R3, [R4, #20]           @ imm5*4
        ADDS    R3, #200                @ imm8 to Rdn
        CMP     R3, R4                  @ low-low
@ ---------- Branches (kept at end so fwd: is reachable) ----------

        B       fwd                     @ B T2 unconditional
        BEQ     fwd                     @ B<cond> T1
        BNE     fwd
        BCS     fwd
        BCC     fwd
        BMI     fwd
        BPL     fwd
        BVS     fwd
        BVC     fwd
        BHI     fwd
        BLS     fwd
        BGE     fwd
        BLT     fwd
        BGT     fwd
        BLE     fwd
        CBZ     R3, fwd
        CBNZ    R3, fwd
        BX      LR                      @ register branch
        BLX     R3                      @ register branch with link (T1)

fwd:
        NOP

        .end
