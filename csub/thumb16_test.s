@ ============================================================
@ test_thumb16.s  -  Coverage of every 16-bit Thumb mnemonic
@ Each mnemonic appears at least once with a valid argument
@ setting that the assembler should encode as a 16-bit (T1)
@ instruction. Forward labels are defined at the end so that
@ all branches resolve.
@
@ Built with:
@   arm-none-eabi-as -mcpu=cortex-m7 -mthumb \
@                    -o thumb16_test.o thumb16_test.s
@ ============================================================

.syntax unified
.thumb

.equ    CONST8,   0x42
.equ    CONST_W,  0x1234

start:

@ ---------- Data processing (low-register T1) ----------

        ADC     R0, R1                  @ Rd = Rd + Rm + C
        ADR     R4, lit_pool            @ positive offset, should be T1
        ADD     R0, R1, #5              @ imm3
        ADD     R0, R1, R2              @ register
        ADD     R0, #200                @ imm8 to Rdn
        ADD     R8, R9                  @ high-register form (no S)
	B	next
.align 2
lit_pool:
        .word   0xDEADBEEF
        .word   CONST_W

next:	ADD     R0, SP, #20             @ Rd, SP, #imm8
        ADD     SP, SP, #16             @ SP-relative imm7*4
        ADD     R3, PC, #8              @ Rd, PC, #imm8 (T1 ADR alias)
        ADR.W   R4, lit_pool            @ negative offset, must be T2
        AND     R0, R1
        ASR     R0, R1, #5              @ immediate shift
        ASR     R0, R1                  @ register shift
        BIC     R0, R1
        CMN     R0, R1
        CMP     R0, #100                @ imm8
        CMP     R0, R1                  @ low-low
        CMP     R8, R9                  @ high-register T2
        EOR     R0, R1
        LSL     R0, R1, #3
        LSL     R0, R1
        LSR     R0, R1, #3
        LSR     R0, R1
        MOVS    R0, #200                @ imm8
        MOVS    R0, R1                  @ low-low (LSL #0 alias, T2)
        MOV     R8, R9                  @ high-register T1
        MUL     R0, R1, R0              @ T1: Rdm = Rn * Rdm
        MVN     R0, R1
        ORR     R0, R1
        ROR     R0, R1
        RSBS    R0, R1, #0              @ NEG alias, T1
        SBC     R0, R1
        SUB     R0, R1, #5              @ imm3
        SUB     R0, R1, R2              @ register
        SUB     R0, #200                @ imm8 to Rdn
        SUB     SP, SP, #16             @ SP-relative imm7*4
        TST     R0, R1

@ ---------- Load / store (immediate, register, PC/SP) ----------

        LDR     R0, [R1, #20]           @ imm5*4
        LDR     R0, [R1, R2]            @ register
	LDR     R0, [PC, #16]           @ literal
        LDR     R0, [SP, #16]           @ SP-relative
        LDR     R0, =CONST_W            @ literal pool (16-bit T1)
        LDRB    R0, [R1, #5]
        LDRB    R0, [R1, R2]
        LDRH    R0, [R1, #6]            @ imm5*2
        LDRH    R0, [R1, R2]
        LDRSB   R0, [R1, R2]
        LDRSH   R0, [R1, R2]
        STR     R0, [R1, #20]
        STR     R0, [R1, R2]
        STR     R0, [SP, #16]
        STRB    R0, [R1, #5]
        STRB    R0, [R1, R2]
        STRH    R0, [R1, #6]
        STRH    R0, [R1, R2]

@ ---------- Load / store multiple, push / pop ----------

        LDMIA   R0!, {R1, R2, R3}
        STMIA   R0!, {R1, R2, R3}
        PUSH    {R0-R3, LR}
        POP     {R0-R3, PC}

@ ---------- Sign / zero extend (T1) ----------

        SXTB    R0, R1
        SXTH    R0, R1
        UXTB    R0, R1
        UXTH    R0, R1

@ ---------- Reverse bytes (T1) ----------

        REV     R0, R1
        REV16   R0, R1
        REVSH   R0, R1

@ ---------- Hint instructions (T1) ----------

        NOP
        SEV
        WFE
        WFI
        YIELD

@ ---------- System / special (T1) ----------

        BKPT    #0xAB
        SVC     #0x10
        SETEND  BE
        SETEND  LE

@ ---------- IT block (T1) ----------

        IT      EQ
        MOVEQ   R0, R1                  @ single-instruction IT body

@ ---------- Branches ----------

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
        CBZ     R0, fwd
        CBNZ    R0, fwd
        BX      LR                      @ register branch
        BLX     R3                      @ register branch with link (T1)

fwd:
        NOP

        .end
