' ============================================================
' disassembler.inc  -  ARM Thumb / Thumb-2 disassembler
'
' Entry points:
'   dis.scan()          - pass 1: find branch targets + literal pool addresses
'   dis.run(outFile$)   - pass 2: decode and emit; outFile$="" → screen
'
' Uses io.buffer%(), io.ptr%, io.base% as input (populated by any reader).
' Output file handle: #2 (same convention as writers).
' ============================================================

CONST MAX_DIS_LABELS = 512
CONST MAX_DIS_POOL   = 256
CONST MAX_DIS_EXT    = 128

DIM dis.lblCount%  = 0
DIM dis.lblAddr%(MAX_DIS_LABELS)

DIM dis.poolCount% = 0
DIM dis.poolAddr%(MAX_DIS_POOL)

DIM dis.extCount%  = 0
DIM dis.extAddr%(MAX_DIS_EXT)

DIM dis.sz% = 1     ' halfwords consumed by last dis.decode$ call (1 or 2)

' ============================================================
' Private helpers
' ============================================================

' Format 32-bit value: decimal if 0..9, else 0xN..N (no leading zeros)
FUNCTION dis.h32$(v%)
    LOCAL u% = v% AND &HFFFFFFFF
    IF u% >= 0 AND u% <= 9 THEN dis.h32$ = STR$(u%) ELSE dis.h32$ = "0x" + HEX$(u%)
END FUNCTION

' Format 16-bit value: decimal if 0..9, else 0xN..N (no leading zeros)
FUNCTION dis.h16$(v%)
    LOCAL u% = v% AND &HFFFF
    IF u% <= 9 THEN dis.h16$ = STR$(u%) ELSE dis.h16$ = "0x" + HEX$(u%)
END FUNCTION

' Sign-extend v% from b% bits.
FUNCTION dis.sext%(v%, b%)
    LOCAL m% = 1 << (b% - 1)
    IF v% AND m% THEN dis.sext% = v% - (m% << 1) ELSE dis.sext% = v%
END FUNCTION

' Register name: R0-R12, SP, LR, PC
FUNCTION dis.rn$(r%)
    SELECT CASE r% AND 15
    CASE 13 : dis.rn$ = "SP"
    CASE 14 : dis.rn$ = "LR"
    CASE 15 : dis.rn$ = "PC"
    CASE ELSE: dis.rn$ = "R" + STR$(r% AND 15)
    END SELECT
END FUNCTION

' Condition code name (0-13; 14=AL is omitted in output)
FUNCTION dis.cc$(c%)
    SELECT CASE c% AND 15
    CASE  0 : dis.cc$ = "EQ"
    CASE  1 : dis.cc$ = "NE"
    CASE  2 : dis.cc$ = "CS"
    CASE  3 : dis.cc$ = "CC"
    CASE  4 : dis.cc$ = "MI"
    CASE  5 : dis.cc$ = "PL"
    CASE  6 : dis.cc$ = "VS"
    CASE  7 : dis.cc$ = "VC"
    CASE  8 : dis.cc$ = "HI"
    CASE  9 : dis.cc$ = "LS"
    CASE 10 : dis.cc$ = "GE"
    CASE 11 : dis.cc$ = "LT"
    CASE 12 : dis.cc$ = "GT"
    CASE 13 : dis.cc$ = "LE"
    CASE ELSE: dis.cc$ = "??"
    END SELECT
END FUNCTION

' Register list bitmask → "{R0-R3,R7,LR}" string
' Runs of 3+ consecutive low registers (R0..R12) collapse to "Rs-Re".
' SP/LR/PC are always listed individually.
FUNCTION dis.rlist$(m%)
    LOCAL i%, j%, k%, s$, first% = 1
    s$ = "{"
    i% = 0
    DO WHILE i% <= 12
        IF m% AND (1 << i%) THEN
            j% = i%
            DO WHILE j% < 12
                IF (m% AND (1 << (j%+1))) = 0 THEN EXIT DO
                INC j%
            LOOP
            IF first% = 0 THEN s$ = s$ + ","
            first% = 0
            IF j% - i% >= 2 THEN
                s$ = s$ + dis.rn$(i%) + "-" + dis.rn$(j%)
            ELSE
                FOR k% = i% TO j%
                    IF k% > i% THEN s$ = s$ + ","
                    s$ = s$ + dis.rn$(k%)
                NEXT
            ENDIF
            i% = j% + 1
        ELSE
            INC i%
        ENDIF
    LOOP
    FOR i% = 13 TO 15
        IF m% AND (1 << i%) THEN
            IF first% = 0 THEN s$ = s$ + ","
            s$ = s$ + dis.rn$(i%)
            first% = 0
        ENDIF
    NEXT
    dis.rlist$ = s$ + "}"
END FUNCTION

' 1 if hw% is the first halfword of a 32-bit Thumb-2 instruction
' (bits[15:13]=111 AND bits[12:11]<>00)
FUNCTION dis.isThumb32%(hw%)
    IF (hw% AND &HE000) <> &HE000 THEN
	dis.isThumb32% = 0
    ELSE
        dis.isThumb32% = ((hw% AND &H1800) <> 0)
    ENDIF
END FUNCTION

' ============================================================
' Label and literal-pool tables
' ============================================================

SUB dis.addLabel(addr%)
    LOCAL i%
    FOR i% = 0 TO dis.lblCount% - 1
        IF dis.lblAddr%(i%) = addr% THEN EXIT SUB
    NEXT
    IF dis.lblCount% >= MAX_DIS_LABELS THEN EXIT SUB
    dis.lblAddr%(dis.lblCount%) = addr%
    INC dis.lblCount%
END SUB

SUB dis.addPool(addr%)
    LOCAL i%
    FOR i% = 0 TO dis.poolCount% - 1
        IF dis.poolAddr%(i%) = addr% THEN EXIT SUB
    NEXT
    IF dis.poolCount% >= MAX_DIS_POOL THEN EXIT SUB
    dis.poolAddr%(dis.poolCount%) = addr%
    INC dis.poolCount%
END SUB

' Return sorted index of addr% in label table, or -1
FUNCTION dis.labelIdx%(addr%)
    LOCAL i%
    dis.labelIdx% = -1
    FOR i% = 0 TO dis.lblCount% - 1
        IF dis.lblAddr%(i%) = addr% THEN dis.labelIdx% = i% : EXIT FUNCTION
    NEXT
END FUNCTION

' Add an external (out-of-range) branch/ADR target address to the ext table.
SUB dis.addExt(addr%)
    LOCAL i%
    FOR i% = 0 TO dis.extCount% - 1
        IF dis.extAddr%(i%) = addr% THEN EXIT SUB
    NEXT
    IF dis.extCount% >= MAX_DIS_EXT THEN EXIT SUB
    dis.extAddr%(dis.extCount%) = addr%
    INC dis.extCount%
END SUB

' Format "EXT_XXXXXXXX" label name for an external address.
FUNCTION dis.extName$(addr%)
    dis.extName$ = "EXT_" + RIGHT$("00000000" + HEX$(addr%), 8)
END FUNCTION

' Return 1 if addr% is in the ext table.
FUNCTION dis.isExt%(addr%)
    LOCAL i%
    dis.isExt% = 0
    FOR i% = 0 TO dis.extCount% - 1
        IF dis.extAddr%(i%) = addr% THEN dis.isExt% = 1 : EXIT FUNCTION
    NEXT
END FUNCTION

' Splice an IT-block condition suffix into a decoded instruction string.
' Inserts cc$ into the mnemonic before any "." size suffix, keeping the
' 8-char mnemonic field width so operands stay aligned.
FUNCTION dis.applyIT$(s$, cc$)
    LOCAL sp% = INSTR(s$, " ")
    LOCAL mne$, rest$, dot%, j%
    IF sp% = 0 THEN dis.applyIT$ = s$ + cc$ : EXIT FUNCTION
    mne$ = LEFT$(s$, sp% - 1)
    j%   = sp%
    DO WHILE j% <= LEN(s$)
        IF MID$(s$, j%, 1) <> " " THEN EXIT DO
        INC j%
    LOOP
    IF j% <= LEN(s$) THEN rest$ = MID$(s$, j%) ELSE rest$ = ""
    dot% = INSTR(mne$, ".")
    IF dot% > 0 THEN
        ' 32-bit form (.W): both S and non-S encodings are valid inside IT — keep as-is.
        mne$ = LEFT$(mne$, dot% - 1) + cc$ + MID$(mne$, dot%)
    ELSE
        ' 16-bit T1 always-flag-setting form: unified syntax inside IT drops the S.
        IF LEN(mne$) > 1 AND RIGHT$(mne$, 1) = "S" THEN mne$ = LEFT$(mne$, LEN(mne$) - 1)
        mne$ = mne$ + cc$
    END IF
    IF rest$ = "" THEN dis.applyIT$ = mne$ ELSE dis.applyIT$ = dis.fmtMne$(mne$) + rest$
END FUNCTION

' Return "L<idx>" if addr% is a known label, "EXT_xxxxxxxx" if external, else hex.
FUNCTION dis.lblName$(addr%)
    LOCAL idx% = dis.labelIdx%(addr%)
    IF idx% >= 0 THEN dis.lblName$ = "label" + STR$(idx%) : EXIT FUNCTION
    IF dis.isExt%(addr%) THEN dis.lblName$ = dis.extName$(addr%) : EXIT FUNCTION
    dis.lblName$ = dis.h32$(addr%)
END FUNCTION

' 1 if addr% is a known literal pool address
FUNCTION dis.isPool%(addr%)
    LOCAL i%
    FOR i% = 0 TO dis.poolCount% - 1
        IF dis.poolAddr%(i%) = addr% THEN dis.isPool% = 1 : EXIT FUNCTION
    NEXT
    dis.isPool% = 0
END FUNCTION

SUB dis.sortLabels()
    LOCAL i%, j%, t%
    FOR i% = 1 TO dis.lblCount% - 1
        t% = dis.lblAddr%(i%)
        j% = i% - 1
        DO WHILE j% >= 0
            IF dis.lblAddr%(j%) <= t% THEN EXIT DO
            dis.lblAddr%(j% + 1) = dis.lblAddr%(j%)
            INC j%, -1
        LOOP
        dis.lblAddr%(j% + 1) = t%
    NEXT
END SUB

SUB dis.sortPool()
    LOCAL i%, j%, t%
    FOR i% = 1 TO dis.poolCount% - 1
        t% = dis.poolAddr%(i%)
        j% = i% - 1
        DO WHILE j% >= 0
            IF dis.poolAddr%(j%) <= t% THEN EXIT DO
            dis.poolAddr%(j% + 1) = dis.poolAddr%(j%)
            INC j%, -1
        LOOP
        dis.poolAddr%(j% + 1) = t%
    NEXT
END SUB

' ============================================================
' 32-bit branch target helpers
' ============================================================

' BL / B.W (T1/T4): S:I1:I2:imm10:imm11:0  (25-bit signed offset)
FUNCTION dis.bltgt32%(hw0%, hw1%, addr%)
    LOCAL s%, j1%, j2%, i1%, i2%, imm10%, imm11%, raw%
    s%     = (hw0% >> 10) AND 1
    imm10% = hw0% AND &H3FF
    j1%    = (hw1% >> 13) AND 1
    j2%    = (hw1% >> 11) AND 1
    imm11% = hw1% AND &H7FF
    i1%    = 1 XOR (j1% XOR s%)
    i2%    = 1 XOR (j2% XOR s%)
    raw%   = (imm11% << 1) OR (imm10% << 12) OR (i2% << 22) OR (i1% << 23) OR (s% << 24)
    IF s% THEN INC raw%, -(1 << 25)
    dis.bltgt32% = addr% + 4 + raw%
END FUNCTION

' Bcc.W (T3): S:J2:J1:imm6:imm11:0  (21-bit signed offset)
FUNCTION dis.bcctgt32%(hw0%, hw1%, addr%)
    LOCAL s%, j1%, j2%, imm6%, imm11%, raw%
    s%     = (hw0% >> 10) AND 1
    imm6%  = hw0% AND &H3F
    j1%    = (hw1% >> 13) AND 1
    j2%    = (hw1% >> 11) AND 1
    imm11% = hw1% AND &H7FF
    raw%   = (imm11% << 1) OR (imm6% << 12) OR (j1% << 18) OR (j2% << 19) OR (s% << 20)
    IF s% THEN INC raw%, -(1 << 21)
    dis.bcctgt32% = addr% + 4 + raw%
END FUNCTION

' ============================================================
' Pass 1 – scan for branch targets and literal pool addresses
' ============================================================
SUB dis.scan()
    LOCAL i% = 0, addr%, hw0%, hw1%, target%, op5%, imm5%, ib%, imm%
    LOCAL endAddr%
    LOCAL ib2%, im3%, im8%, imm12%

    dis.lblCount%  = 0
    dis.poolCount% = 0
    dis.extCount%  = 0
    endAddr%       = io.base% + io.ptr% * 2

    DO WHILE i% < io.ptr%
        addr% = io.base% + i% * 2
        hw0%  = io.buffer%(i%) AND &HFFFF

        IF dis.isThumb32%(hw0%) THEN
            IF i% + 1 < io.ptr% THEN
                hw1% = io.buffer%(i% + 1) AND &HFFFF
                ' BL / B.W : hw0 bits[15:11]=11110, hw1 bit[14]=1 (BL) or bit[14]=0 (B.W)
                IF (hw0% AND &HF800) = &HF000 AND (hw1% AND &HD000) = &HD000 THEN
                    target% = dis.bltgt32%(hw0%, hw1%, addr%)
                    IF target% >= io.base% AND target% < endAddr% THEN dis.addLabel(target%) ELSE dis.addExt(target%)
                END IF
                IF (hw0% AND &HF800) = &HF000 AND (hw1% AND &HD000) = &H9000 THEN
                    target% = dis.bltgt32%(hw0%, hw1%, addr%)
                    IF target% >= io.base% AND target% < endAddr% THEN dis.addLabel(target%) ELSE dis.addExt(target%)
                END IF
                ' Bcc.W (T3): hw0 bits[15:11]=11110, hw1 bits[15:14]=10, bit12=0
                ' Cond field (hw0[9:6]) must be < 14: 14/15 are misc-control/hints, not Bcc.
                IF (hw0% AND &HF800) = &HF000 AND (hw1% AND &HD000) = &H8000 AND ((hw0% >> 6) AND &HF) < 14 THEN
                    target% = dis.bcctgt32%(hw0%, hw1%, addr%)
                    IF target% >= io.base% AND target% < endAddr% THEN dis.addLabel(target%) ELSE dis.addExt(target%)
                END IF
                ' ADR.W: hw0 pattern (ADDW/SUBW with Rn=1111), 12-bit imm in hw1/hw0
                IF (hw0% AND &HFBFF) = &HF20F OR (hw0% AND &HFBFF) = &HF2AF THEN
                    ib2%  = (hw0% >> 10) AND 1
                    im3%  = (hw1% >> 12) AND 7
                    im8%  = hw1% AND &HFF
                    imm12%= (ib2% << 11) OR (im3% << 8) OR im8%
                    IF (hw0% AND &HFBFF) = &HF20F THEN
                        target% = ((addr% + 4) AND &HFFFFFFFC) + imm12%
                    ELSE
                        target% = ((addr% + 4) AND &HFFFFFFFC) - imm12%
                    END IF
                    IF target% >= io.base% AND target% < endAddr% THEN dis.addLabel(target%) ELSE dis.addExt(target%)
                END IF
            END IF
            INC i%, 2
        ELSE
            op5% = (hw0% >> 11) AND &H1F
            SELECT CASE op5%
            CASE &H1A, &H1B  ' 11010/11011 – Bcc (bits[11:8]=cond, <> 14/15)
                IF ((hw0% >> 8) AND &HF) < 14 THEN
                    target% = addr% + 4 + dis.sext%(hw0% AND &HFF, 8) * 2
                    IF target% >= io.base% AND target% < endAddr% THEN dis.addLabel(target%) ELSE dis.addExt(target%)
                END IF
            CASE &H1C  ' 11100 – B unconditional
                target% = addr% + 4 + dis.sext%(hw0% AND &H7FF, 11) * 2
                IF target% >= io.base% AND target% < endAddr% THEN dis.addLabel(target%) ELSE dis.addExt(target%)
            CASE &H14  ' 10100 – ADR (16-bit T1): 1010 0 Rd imm8
                target% = ((addr% + 4) AND &HFFFFFFFC) + (hw0% AND &HFF) * 4
                IF target% >= io.base% AND target% < endAddr% THEN dis.addLabel(target%) ELSE dis.addExt(target%)
            CASE &H09  ' 01001 – LDR Rd,[PC,#imm8*4]
                target% = ((addr% + 4) AND &HFFFFFFFC) + (hw0% AND &HFF) * 4
                dis.addPool(target%)
            END SELECT
            ' CBZ / CBNZ: bits[15:12]=1011, bit[10]=0, bit[8]=1
            IF (hw0% AND &HFB00) = &HB100 OR (hw0% AND &HFB00) = &HB900 THEN
                ib%    = (hw0% >> 9) AND 1
                imm5%  = (hw0% >> 3) AND &H1F
                imm%   = (ib% << 6) OR (imm5% << 1)
                target% = addr% + 4 + imm%
                IF target% >= io.base% AND target% < endAddr% THEN dis.addLabel(target%) ELSE dis.addExt(target%)
            END IF
            INC i%
        END IF
    LOOP

    dis.sortLabels()
    dis.sortPool()
END SUB

' ============================================================
' Instruction decoders  (16-bit Thumb)
' ============================================================

' Dispatch: decode one instruction, set dis.sz% (1 or 2)
FUNCTION dis.decode$(hw0%, hw1%, addr%)
    LOCAL top5% = (hw0% >> 11) AND &H1F

    dis.sz% = 1
    IF dis.isThumb32%(hw0%) THEN
        dis.sz% = 2
        dis.decode$ = dis.dec32$(hw0%, hw1%, addr%)
        EXIT FUNCTION
    END IF

    SELECT CASE top5%
    CASE &H00 : dis.decode$ = dis.decShift$(hw0%, 0)          ' LSL #imm
    CASE &H01 : dis.decode$ = dis.decShift$(hw0%, 1)          ' LSR #imm
    CASE &H02 : dis.decode$ = dis.decShift$(hw0%, 2)          ' ASR #imm
    CASE &H03 : dis.decode$ = dis.decAddSub$(hw0%)            ' ADD/SUB reg/imm3
    CASE &H04 : dis.decode$ = dis.decMovImm$(hw0%, 0)         ' MOV Rn,#imm8
    CASE &H05 : dis.decode$ = dis.decMovImm$(hw0%, 1)         ' CMP Rn,#imm8
    CASE &H06 : dis.decode$ = dis.decMovImm$(hw0%, 2)         ' ADD Rn,#imm8
    CASE &H07 : dis.decode$ = dis.decMovImm$(hw0%, 3)         ' SUB Rn,#imm8
    CASE &H08 : dis.decode$ = dis.decDP$(hw0%)                ' Data-proc / special
    CASE &H09 : dis.decode$ = dis.decLDRlit$(hw0%, addr%)     ' LDR [PC,#imm]
    CASE &H0A, &H0B : dis.decode$ = dis.decLSReg$(hw0%)      ' Load/store register
    CASE &H0C : dis.decode$ = dis.decLSimm$(hw0%, 0, 0)       ' STR  [Rn,#imm5*4]
    CASE &H0D : dis.decode$ = dis.decLSimm$(hw0%, 1, 0)       ' LDR  [Rn,#imm5*4]
    CASE &H0E : dis.decode$ = dis.decLSimm$(hw0%, 0, 1)       ' STRB [Rn,#imm5]
    CASE &H0F : dis.decode$ = dis.decLSimm$(hw0%, 1, 1)       ' LDRB [Rn,#imm5]
    CASE &H10 : dis.decode$ = dis.decLShw$(hw0%, 0)           ' STRH [Rn,#imm5*2]
    CASE &H11 : dis.decode$ = dis.decLShw$(hw0%, 1)           ' LDRH [Rn,#imm5*2]
    CASE &H12 : dis.decode$ = dis.decLSSP$(hw0%, 0)           ' STR [SP,#imm8*4]
    CASE &H13 : dis.decode$ = dis.decLSSP$(hw0%, 1)           ' LDR [SP,#imm8*4]
    CASE &H14 : dis.decode$ = dis.decADR$(hw0%, addr%)        ' ADR Rd,PC,#imm8*4
    CASE &H15 : dis.decode$ = dis.decAddSPi$(hw0%)            ' ADD Rd,SP,#imm8*4
    CASE &H16, &H17 : dis.decode$ = dis.decMisc$(hw0%, addr%) ' PUSH/POP/IT/CBZ/…
    CASE &H18 : dis.decode$ = dis.decLDMSTM$(hw0%, 0)         ' STM
    CASE &H19 : dis.decode$ = dis.decLDMSTM$(hw0%, 1)         ' LDM
    CASE &H1A, &H1B : dis.decode$ = dis.decBcc$(hw0%, addr%)  ' Bcc / SVC
    CASE &H1C : dis.decode$ = dis.decB$(hw0%, addr%)           ' B unconditional
    CASE ELSE  : dis.decode$ = ".hword  " + dis.h16$(hw0%)
    END SELECT
END FUNCTION

' LSL/LSR/ASR #imm  (top5=0/1/2)
' LSL #0 is canonically MOV Rd,Rm
FUNCTION dis.decShift$(hw%, op%)
    LOCAL rd%  = hw% AND 7
    LOCAL rm%  = (hw% >> 3) AND 7
    LOCAL imm% = (hw% >> 6) AND &H1F
    LOCAL mne$ LENGTH 4
    SELECT CASE op%
    CASE 0 : mne$ = "LSLS"
    CASE 1 : mne$ = "LSRS"
    CASE 2 : mne$ = "ASRS"
    END SELECT
    IF op% = 0 AND imm% = 0 THEN
        dis.decShift$ = dis.fmtMne$("MOVS") + dis.rn$(rd%) + "," + dis.rn$(rm%)
    ELSE
        dis.decShift$ = dis.fmtMne$(mne$) + dis.rn$(rd%) + "," + dis.rn$(rm%) + ",#" + STR$(imm%)
    END IF
END FUNCTION

' ADD/SUB register or #imm3  (top5=3) — T1 always sets flags
FUNCTION dis.decAddSub$(hw%)
    LOCAL rd%  = hw% AND 7
    LOCAL rn%  = (hw% >> 3) AND 7
    LOCAL op3% = (hw% >> 6) AND 7
    LOCAL sub% = (hw% >> 9) AND 1
    LOCAL imm% = (hw% >> 10) AND 1
    LOCAL mne$ LENGTH 4
    IF sub% THEN mne$ = "SUBS" ELSE mne$ = "ADDS"
    IF imm% THEN
        dis.decAddSub$ = dis.fmtMne$(mne$) + dis.rn$(rd%) + "," + dis.rn$(rn%) + ",#" + STR$(op3%)
    ELSE
        dis.decAddSub$ = dis.fmtMne$(mne$) + dis.rn$(rd%) + "," + dis.rn$(rn%) + "," + dis.rn$(op3%)
    END IF
END FUNCTION

' MOV/CMP/ADD/SUB Rn,#imm8  (top5=4..7)
FUNCTION dis.decMovImm$(hw%, op%)
    LOCAL rn%  = (hw% >> 8) AND 7
    LOCAL imm% = hw% AND &HFF
    LOCAL mne$ LENGTH 4
    SELECT CASE op%
    CASE 0 : mne$ = "MOVS"
    CASE 1 : mne$ = "CMP"
    CASE 2 : mne$ = "ADDS"
    CASE 3 : mne$ = "SUBS"
    END SELECT
    dis.decMovImm$ = dis.fmtMne$(mne$) + dis.rn$(rn%) + ",#" + STR$(imm%)
END FUNCTION

' Data-processing and special instructions  (top5=8)
FUNCTION dis.decDP$(hw%)
    LOCAL top6% = (hw% >> 10) AND &H3F
    LOCAL rd%   = hw% AND 7
    LOCAL rm%   = (hw% >> 3) AND 7
    LOCAL op4%  = (hw% >> 6) AND &HF
    LOCAL op2%  = (hw% >> 8) AND 3
    LOCAL rdn%  = (hw% AND 7) OR ((hw% >> 4) AND 8)
    LOCAL rm2%  = (hw% >> 3) AND &HF
    LOCAL mne$  LENGTH 8

    IF top6% = &H10 THEN
        ' Standard data processing (bits[15:10]=010000)
        SELECT CASE op4%
        CASE  0 : mne$ = "ANDS"
        CASE  1 : mne$ = "EORS"
        CASE  2 : mne$ = "LSLS"
        CASE  3 : mne$ = "LSRS"
        CASE  4 : mne$ = "ASRS"
        CASE  5 : mne$ = "ADCS"
        CASE  6 : mne$ = "SBCS"
        CASE  7 : mne$ = "RORS"
        CASE  8 : mne$ = "TST"
        CASE  9 : mne$ = "RSBS"
        CASE 10 : mne$ = "CMP"
        CASE 11 : mne$ = "CMN"
        CASE 12 : mne$ = "ORRS"
        CASE 13 : mne$ = "MULS"
        CASE 14 : mne$ = "BICS"
        CASE 15 : mne$ = "MVNS"
        END SELECT
        SELECT CASE op4%
        CASE 8, 10, 11  ' TST, CMP, CMN – no dest
            dis.decDP$ = dis.fmtMne$(mne$) + dis.rn$(rd%) + "," + dis.rn$(rm%)
        CASE 9          ' RSBS Rd,Rm,#0
            dis.decDP$ = dis.fmtMne$("RSBS") + dis.rn$(rd%) + "," + dis.rn$(rm%) + ",#0"
        CASE 13         ' MULS Rd,Rm,Rd
            dis.decDP$ = dis.fmtMne$("MULS") + dis.rn$(rd%) + "," + dis.rn$(rm%) + "," + dis.rn$(rd%)
        CASE 15         ' MVNS Rd,Rm
            dis.decDP$ = dis.fmtMne$("MVNS") + dis.rn$(rd%) + "," + dis.rn$(rm%)
        CASE ELSE
            dis.decDP$ = dis.fmtMne$(mne$) + dis.rn$(rd%) + "," + dis.rn$(rm%)
        END SELECT
    ELSE
        ' Special data / branch exchange (bits[15:10]=010001)
        SELECT CASE op2%
        CASE 0 : dis.decDP$ = dis.fmtMne$("ADD") + dis.rn$(rdn%) + "," + dis.rn$(rm2%)
        CASE 1 : dis.decDP$ = dis.fmtMne$("CMP") + dis.rn$(rdn%) + "," + dis.rn$(rm2%)
        CASE 2 : dis.decDP$ = dis.fmtMne$("MOV") + dis.rn$(rdn%) + "," + dis.rn$(rm2%)
        CASE 3
            IF (hw% >> 7) AND 1 THEN
                dis.decDP$ = dis.fmtMne$("BLX") + dis.rn$(rm2%)
            ELSE
                dis.decDP$ = dis.fmtMne$("BX") + dis.rn$(rm2%)
            END IF
        END SELECT
    END IF
END FUNCTION

' LDR Rd,[PC,#imm8*4]  (top5=9)
FUNCTION dis.decLDRlit$(hw%, addr%)
    LOCAL rd%   = (hw% >> 8) AND 7
    LOCAL imm%  = (hw% AND &HFF) * 4
    LOCAL pool% = ((addr% + 4) AND &HFFFFFFFC) + imm%
    dis.decLDRlit$ = dis.fmtMne$("LDR") + dis.rn$(rd%) + ",[PC,#" + STR$(imm%) + "]  ; " + dis.h32$(pool%)
END FUNCTION

' Load/store register offset  (top5=&H0A/&H0B → bits[15:11]=0101x)
FUNCTION dis.decLSReg$(hw%)
    LOCAL rd%  = hw% AND 7
    LOCAL rn%  = (hw% >> 3) AND 7
    LOCAL rm%  = (hw% >> 6) AND 7
    LOCAL op3% = (hw% >> 9) AND 7
    LOCAL mne$ LENGTH 8
    SELECT CASE op3%
    CASE 0 : mne$ = "STR"
    CASE 1 : mne$ = "STRH"
    CASE 2 : mne$ = "STRB"
    CASE 3 : mne$ = "LDRSB"
    CASE 4 : mne$ = "LDR"
    CASE 5 : mne$ = "LDRH"
    CASE 6 : mne$ = "LDRB"
    CASE 7 : mne$ = "LDRSH"
    END SELECT
    dis.decLSReg$ = dis.fmtMne$(mne$) + dis.rn$(rd%) + ",[" + dis.rn$(rn%) + "," + dis.rn$(rm%) + "]"
END FUNCTION

' LDR/STR/LDRB/STRB with imm5 offset  (top5=&H0C..&H0F)
' ldflag%: 0=store, 1=load   byteflag%: 0=word, 1=byte
FUNCTION dis.decLSimm$(hw%, ldflag%, byteflag%)
    LOCAL rd%   = hw% AND 7
    LOCAL rn%   = (hw% >> 3) AND 7
    LOCAL imm5% = (hw% >> 6) AND &H1F
    LOCAL off%
    LOCAL mne$  LENGTH 8
    IF ldflag% THEN
        IF byteflag% THEN mne$ = "LDRB" ELSE mne$ = "LDR"
    ELSE
        IF byteflag% THEN mne$ = "STRB" ELSE mne$ = "STR"
    END IF
    IF byteflag% THEN off% = imm5% ELSE off% = imm5% * 4
    IF off% = 0 THEN
        dis.decLSimm$ = dis.fmtMne$(mne$) + dis.rn$(rd%) + ",[" + dis.rn$(rn%) + "]"
    ELSE
        dis.decLSimm$ = dis.fmtMne$(mne$) + dis.rn$(rd%) + ",[" + dis.rn$(rn%) + ",#" + STR$(off%) + "]"
    END IF
END FUNCTION

' STRH/LDRH with imm5*2 offset  (top5=&H10/&H11)
FUNCTION dis.decLShw$(hw%, ldflag%)
    LOCAL rd%  = hw% AND 7
    LOCAL rn%  = (hw% >> 3) AND 7
    LOCAL off% = ((hw% >> 6) AND &H1F) * 2
    IF ldflag% THEN
        IF off% = 0 THEN
            dis.decLShw$ = dis.fmtMne$("LDRH") + dis.rn$(rd%) + ",[" + dis.rn$(rn%) + "]"
        ELSE
            dis.decLShw$ = dis.fmtMne$("LDRH") + dis.rn$(rd%) + ",[" + dis.rn$(rn%) + ",#" + STR$(off%) + "]"
        END IF
    ELSE
        IF off% = 0 THEN
            dis.decLShw$ = dis.fmtMne$("STRH") + dis.rn$(rd%) + ",[" + dis.rn$(rn%) + "]"
        ELSE
            dis.decLShw$ = dis.fmtMne$("STRH") + dis.rn$(rd%) + ",[" + dis.rn$(rn%) + ",#" + STR$(off%) + "]"
        END IF
    END IF
END FUNCTION

' LDR/STR SP-relative  (top5=&H12/&H13)
FUNCTION dis.decLSSP$(hw%, ldflag%)
    LOCAL rd%  = (hw% >> 8) AND 7
    LOCAL off% = (hw% AND &HFF) * 4
    IF ldflag% THEN
        dis.decLSSP$ = dis.fmtMne$("LDR") + dis.rn$(rd%) + ",[SP,#" + STR$(off%) + "]"
    ELSE
        dis.decLSSP$ = dis.fmtMne$("STR") + dis.rn$(rd%) + ",[SP,#" + STR$(off%) + "]"
    END IF
END FUNCTION

' ADR Rd,PC,#imm8*4  (top5=&H14)
FUNCTION dis.decADR$(hw%, addr%)
    LOCAL rd%   = (hw% >> 8) AND 7
    LOCAL imm%  = (hw% AND &HFF) * 4
    LOCAL tgt%  = ((addr% + 4) AND &HFFFFFFFC) + imm%
    dis.decADR$ = dis.fmtMne$("ADR") + dis.rn$(rd%) + "," + dis.lblName$(tgt%)
END FUNCTION

' ADD Rd,SP,#imm8*4  (top5=&H15)
FUNCTION dis.decAddSPi$(hw%)
    LOCAL rd%  = (hw% >> 8) AND 7
    LOCAL imm% = (hw% AND &HFF) * 4
    dis.decAddSPi$ = dis.fmtMne$("ADD") + dis.rn$(rd%) + ",SP,#" + STR$(imm%)
END FUNCTION

' Miscellaneous 16-bit  (top5=&H16/&H17, bits[15:12]=1011)
FUNCTION dis.decMisc$(hw%, addr%)
    LOCAL rd%   = hw% AND 7
    LOCAL ib%   = (hw% >> 9) AND 1
    LOCAL imm5% = (hw% >> 3) AND &H1F
    LOCAL mne$ LENGTH 8
    LOCAL imm%
    LOCAL tgt%
    LOCAL mask%
    LOCAL cc4%
    LOCAL itm%
    LOCAL its$ LENGTH 8
    LOCAL rm%
    LOCAL nLet%, cc0%
    LOCAL aif$ LENGTH 4, fl%

    ' ADD/SUB SP,SP,#imm7*4  : bits[15:8] = 10110000
    IF (hw% AND &HFF00) = &HB000 THEN
	mne$ = CHOICE((hw% >> 7) AND 1,"SUB","ADD")
        dis.decMisc$ = dis.fmtMne$(mne$) + "SP,SP,#" + STR$((hw% AND &H7F) * 4)
        EXIT FUNCTION
    END IF

    ' SETEND : 1011 0110 0101 0 E 0 0 0  (hw & FFF7 = B650)
    IF (hw% AND &HFFF7) = &HB650 THEN
        dis.decMisc$ = dis.fmtMne$("SETEND") + CHOICE((hw% AND 8) <> 0,"BE","LE")
        EXIT FUNCTION
    END IF

    ' PUSH {regs}  : bits[15:9]=1011010
    IF (hw% AND &HFE00) = &HB400 THEN
        mask% = (hw% AND &HFF) OR (((hw% >> 8) AND 1) << 14)
        dis.decMisc$ = dis.fmtMne$("PUSH") + dis.rlist$(mask%)
        EXIT FUNCTION
    END IF

    ' POP {regs}  : bits[15:9]=1011110
    IF (hw% AND &HFE00) = &HBC00 THEN
        mask% = (hw% AND &HFF) OR (((hw% >> 8) AND 1) << 15)
        dis.decMisc$ = dis.fmtMne$("POP") + dis.rlist$(mask%)
        EXIT FUNCTION
    END IF

    ' BKPT #imm8  : bits[15:8]=10111110
    IF (hw% AND &HFF00) = &HBE00 THEN
        dis.decMisc$ = dis.fmtMne$("BKPT") + "#" + STR$(hw% AND &HFF)
        EXIT FUNCTION
    END IF

    ' NOP/YIELD/WFE/WFI/SEV  : bits[15:8]=10111111, bits[3:0]=0000
    ' Must be checked before IT (IT requires bits[3:0]<>0)
    IF (hw% AND &HFF0F) = &HBF00 THEN
        SELECT CASE (hw% >> 4) AND &HF
        CASE 0 : dis.decMisc$ = "NOP"
        CASE 1 : dis.decMisc$ = "YIELD"
        CASE 2 : dis.decMisc$ = "WFE"
        CASE 3 : dis.decMisc$ = "WFI"
        CASE 4 : dis.decMisc$ = "SEV"
        CASE ELSE: dis.decMisc$ = dis.fmtMne$(".hword") + dis.h16$(hw%)
        END SELECT
        EXIT FUNCTION
    END IF

    ' IT{xyz}  : bits[15:8]=10111111, bits[7:4]=cond, bits[3:0]=mask (<>0)
    ' Mask guard bit = lowest set bit; #T/E letters = 3 - guard position.
    IF (hw% AND &HFF00) = &HBF00 THEN
        cc4%  = (hw% >> 4) AND &HF
        itm%  = hw% AND &HF
        cc0%  = cc4% AND 1
        IF (itm% AND 1) <> 0 THEN
            nLet% = 3
        ELSEIF (itm% AND 2) <> 0 THEN
            nLet% = 2
        ELSEIF (itm% AND 4) <> 0 THEN
            nLet% = 1
        ELSE
            nLet% = 0
        END IF
        its$ = "IT"
        IF nLet% >= 1 THEN IF ((itm% >> 3) AND 1) = cc0% THEN its$ = its$ + "T" ELSE its$ = its$ + "E"
        IF nLet% >= 2 THEN IF ((itm% >> 2) AND 1) = cc0% THEN its$ = its$ + "T" ELSE its$ = its$ + "E"
        IF nLet% >= 3 THEN IF ((itm% >> 1) AND 1) = cc0% THEN its$ = its$ + "T" ELSE its$ = its$ + "E"
        dis.decMisc$ = dis.fmtMne$(its$) + dis.cc$(cc4%)
        EXIT FUNCTION
    END IF

    ' CBZ  : bits[15:12]=1011, bit[11]=0, bit[10]=0, bit[8]=1
    IF (hw% AND &HFB00) = &HB100 THEN
        ib%   = (hw% >> 9) AND 1
        imm%  = (ib% << 6) OR (imm5% << 1)
        tgt%  = addr% + 4 + imm%
        dis.decMisc$ = dis.fmtMne$("CBZ") + dis.rn$(rd%) + "," + dis.lblName$(tgt%)
        EXIT FUNCTION
    END IF

    ' CBNZ : bits[15:12]=1011, bit[11]=1, bit[10]=0, bit[8]=1
    IF (hw% AND &HFB00) = &HB900 THEN
        ib%   = (hw% >> 9) AND 1
        imm%  = (ib% << 6) OR (imm5% << 1)
        tgt%  = addr% + 4 + imm%
        dis.decMisc$ = dis.fmtMne$("CBNZ") + dis.rn$(rd%) + "," + dis.lblName$(tgt%)
        EXIT FUNCTION
    END IF

    ' SXTH/SXTB/UXTH/UXTB : bits[15:8]=10110010, bits[7:6]=op selector
    IF (hw% AND &HFF00) = &HB200 THEN
        rm% = (hw% >> 3) AND 7
        SELECT CASE (hw% >> 6) AND 3
        CASE 0 : dis.decMisc$ = dis.fmtMne$("SXTH") + dis.rn$(rd%) + "," + dis.rn$(rm%)
        CASE 1 : dis.decMisc$ = dis.fmtMne$("SXTB") + dis.rn$(rd%) + "," + dis.rn$(rm%)
        CASE 2 : dis.decMisc$ = dis.fmtMne$("UXTH") + dis.rn$(rd%) + "," + dis.rn$(rm%)
        CASE 3 : dis.decMisc$ = dis.fmtMne$("UXTB") + dis.rn$(rd%) + "," + dis.rn$(rm%)
        END SELECT
        EXIT FUNCTION
    END IF

    ' REV/REV16/REVSH : bits[15:8]=10111010, bits[7:6]=op selector
    IF (hw% AND &HFF00) = &HBA00 THEN
        rm% = (hw% >> 3) AND 7
        SELECT CASE (hw% >> 6) AND 3
        CASE 0 : dis.decMisc$ = dis.fmtMne$("REV") + dis.rn$(rd%) + "," + dis.rn$(rm%)
        CASE 1 : dis.decMisc$ = dis.fmtMne$("REV16") + dis.rn$(rd%) + "," + dis.rn$(rm%)
        CASE 3 : dis.decMisc$ = dis.fmtMne$("REVSH") + dis.rn$(rd%) + "," + dis.rn$(rm%)
        CASE ELSE: dis.decMisc$ = dis.fmtMne$(".hword") + dis.h16$(hw%)
        END SELECT
        EXIT FUNCTION
    END IF

    ' CPSIE/CPSID : bits[15:5]=10110110011, bit4=imod (0=enable, 1=disable),
    ' bits[3:0]=AIF flags
    IF (hw% AND &HFFE0) = &HB660 THEN
        aif$ = ""
        fl% = hw% AND 7
        IF (fl% AND 4) <> 0 THEN aif$ = aif$ + "A"
        IF (fl% AND 2) <> 0 THEN aif$ = aif$ + "I"
        IF (fl% AND 1) <> 0 THEN aif$ = aif$ + "F"
        IF (hw% AND &H10) <> 0 THEN
            dis.decMisc$ = dis.fmtMne$("CPSID") + aif$
        ELSE
            dis.decMisc$ = dis.fmtMne$("CPSIE") + aif$
        END IF
        EXIT FUNCTION
    END IF

    dis.decMisc$ = dis.fmtMne$(".hword") + dis.h16$(hw%)
END FUNCTION

' STM / LDM  (top5=&H18/&H19)
FUNCTION dis.decLDMSTM$(hw%, ldflag%)
    LOCAL rn%   = (hw% >> 8) AND 7
    LOCAL mask% = hw% AND &HFF
    IF ldflag% THEN
        ' LDM: writeback only if Rn NOT in list
        IF mask% AND (1 << rn%) THEN
            dis.decLDMSTM$ = dis.fmtMne$("LDM") + dis.rn$(rn%) + "," + dis.rlist$(mask%)
        ELSE
            dis.decLDMSTM$ = dis.fmtMne$("LDM") + dis.rn$(rn%) + "!," + dis.rlist$(mask%)
        END IF
    ELSE
        dis.decLDMSTM$ = dis.fmtMne$("STM") + dis.rn$(rn%) + "!," + dis.rlist$(mask%)
    END IF
END FUNCTION

' Bcc + SVC  (top5=&H1A/&H1B, bits[15:12]=1101)
FUNCTION dis.decBcc$(hw%, addr%)
    LOCAL cc4%  = (hw% >> 8) AND &HF
    LOCAL tgt%  = addr% + 4 + dis.sext%(hw% AND &HFF, 8) * 2
    SELECT CASE cc4%
    CASE 14 : dis.decBcc$ = dis.fmtMne$(".hword") + dis.h16$(hw%)   ' UDF
    CASE 15 : dis.decBcc$ = dis.fmtMne$("SVC") + "#" + STR$(hw% AND &HFF)
    CASE ELSE
        dis.decBcc$ = dis.fmtMne$("B" + dis.cc$(cc4%)) + dis.lblName$(tgt%)
    END SELECT
END FUNCTION

' B unconditional  (top5=&H1C)
FUNCTION dis.decB$(hw%, addr%)
    LOCAL tgt% = addr% + 4 + dis.sext%(hw% AND &H7FF, 11) * 2
    dis.decB$ = dis.fmtMne$("B") + dis.lblName$(tgt%)
END FUNCTION

' ============================================================
' 32-bit Thumb-2 helpers
' ============================================================

' ThumbExpandImm: decode ARM modified 12-bit immediate to 32-bit value.
' i%, imm3%, imm8% come from hw0[10], hw1[14:12], hw1[7:0].
FUNCTION dis.modImm%(i%, imm3%, imm8%)
    LOCAL imm12% = (i% << 11) OR (imm3% << 8) OR imm8%
    LOCAL top4%  = (imm12% >> 8) AND &HF
    LOCAL unrot%, sh%
    SELECT CASE top4%
    CASE 0 : dis.modImm% = imm8%
    CASE 1 : dis.modImm% = imm8% OR (imm8% << 16)
    CASE 2 : dis.modImm% = (imm8% << 8) OR (imm8% << 24)
    CASE 3 : dis.modImm% = imm8% OR (imm8% << 8) OR (imm8% << 16) OR (imm8% << 24)
    CASE ELSE
        unrot% = &H80 OR (imm8% AND &H7F)
        sh%    = (imm12% >> 7) AND &H1F
        dis.modImm% = ((unrot% >> sh%) OR (unrot% << (32 - sh%))) AND &HFFFFFFFF
    END SELECT
END FUNCTION

' Decode 32-bit data-processing (modified immediate).
' Precondition: (hw0 AND &HFA00)=&HF000, hw1[15]=0
FUNCTION dis.decDP32imm$(hw0%, hw1%)
    LOCAL op4%  = (hw0% >> 5) AND &HF
    LOCAL s%    = (hw0% >> 4) AND 1
    LOCAL rn%   = hw0% AND &HF
    LOCAL i%    = (hw0% >> 10) AND 1
    LOCAL imm3% = (hw1% >> 12) AND 7
    LOCAL rd%   = (hw1% >> 8) AND &HF
    LOCAL imm8% = hw1% AND &HFF
    LOCAL imm$  = "#" + dis.h32$(dis.modImm%(i%, imm3%, imm8%))
    LOCAL mne$  LENGTH 10

    SELECT CASE op4%
    CASE 0
        IF s% AND rd% = 15 THEN dis.decDP32imm$ = dis.fmtMne$("TST.W") + dis.rn$(rn%) + "," + imm$ : EXIT FUNCTION
        mne$ = "AND"
    CASE 1 : mne$ = "BIC"
    CASE 2
        IF rn% = 15 THEN
            IF s% THEN mne$ = "MOVS" ELSE mne$ = "MOV"
            dis.decDP32imm$ = dis.fmtMne$(mne$ + ".W") + dis.rn$(rd%) + "," + imm$ : EXIT FUNCTION
        END IF
        mne$ = "ORR"
    CASE 3
        IF rn% = 15 THEN
            IF s% THEN mne$ = "MVNS" ELSE mne$ = "MVN"
            dis.decDP32imm$ = dis.fmtMne$(mne$ + ".W") + dis.rn$(rd%) + "," + imm$ : EXIT FUNCTION
        END IF
        mne$ = "ORN"
    CASE 4
        IF s% AND rd% = 15 THEN dis.decDP32imm$ = dis.fmtMne$("TEQ.W") + dis.rn$(rn%) + "," + imm$ : EXIT FUNCTION
        mne$ = "EOR"
    CASE 8
        IF s% AND rd% = 15 THEN dis.decDP32imm$ = dis.fmtMne$("CMN.W") + dis.rn$(rn%) + "," + imm$ : EXIT FUNCTION
        mne$ = "ADD"
    CASE 10 : mne$ = "ADC"
    CASE 11 : mne$ = "SBC"
    CASE 13
        IF s% AND rd% = 15 THEN dis.decDP32imm$ = dis.fmtMne$("CMP.W") + dis.rn$(rn%) + "," + imm$ : EXIT FUNCTION
        mne$ = "SUB"
    CASE 14 : mne$ = "RSB"
    CASE ELSE
        dis.decDP32imm$ = dis.fmtMne$(".hword") + dis.h16$(hw0%) + "," + dis.h16$(hw1%) : EXIT FUNCTION
    END SELECT

    IF s% THEN mne$ = mne$ + "S"
    dis.decDP32imm$ = dis.fmtMne$(mne$ + ".W") + dis.rn$(rd%) + "," + dis.rn$(rn%) + "," + imm$
END FUNCTION

' Format a shift suffix (appended to a register operand).
' Returns "" for type=0,imm=0; ",RRX" for type=3,imm=0; else ",<op> #n".
FUNCTION dis.shSfx$(type2%, shimm%)
    LOCAL nm$ LENGTH 3, n%
    IF type2% = 3 AND shimm% = 0 THEN dis.shSfx$ = ",RRX" : EXIT FUNCTION
    IF type2% = 0 AND shimm% = 0 THEN dis.shSfx$ = "" : EXIT FUNCTION
    SELECT CASE type2%
    CASE 0 : nm$ = "LSL" : n% = shimm%
    CASE 1 : nm$ = "LSR" : IF shimm% = 0 THEN n% = 32 ELSE n% = shimm%
    CASE 2 : nm$ = "ASR" : IF shimm% = 0 THEN n% = 32 ELSE n% = shimm%
    CASE 3 : nm$ = "ROR" : n% = shimm%
    END SELECT
    dis.shSfx$ = "," + nm$ + " #" + STR$(n%)
END FUNCTION

' Decode 32-bit data-processing (shifted register).
' Precondition: (hw0 AND &HFE00)=&HEA00, hw1[15]=0
FUNCTION dis.decDP32reg$(hw0%, hw1%)
    LOCAL op4%   = (hw0% >> 5) AND &HF
    LOCAL s%     = (hw0% >> 4) AND 1
    LOCAL rn%    = hw0% AND &HF
    LOCAL rd%    = (hw1% >> 8) AND &HF
    LOCAL rm%    = hw1% AND &HF
    LOCAL type2% = (hw1% >> 4) AND 3
    LOCAL shimm% = (((hw1% >> 12) AND 7) << 2) OR ((hw1% >> 6) AND 3)
    LOCAL sfx$   = dis.shSfx$(type2%, shimm%)
    LOCAL mne$ LENGTH 10
    LOCAL snm$ LENGTH 6
    LOCAL n2%

    SELECT CASE op4%
    CASE 0
        IF s% AND rd% = 15 THEN dis.decDP32reg$ = dis.fmtMne$("TST.W") + dis.rn$(rn%) + "," + dis.rn$(rm%) + sfx$ : EXIT FUNCTION
        mne$ = "AND"
    CASE 1 : mne$ = "BIC"
    CASE 2
        IF rn% = 15 THEN
            IF type2% = 0 AND shimm% = 0 THEN
                IF s% THEN snm$ = "MOVS" ELSE snm$ = "MOV"
                dis.decDP32reg$ = dis.fmtMne$(snm$ + ".W") + dis.rn$(rd%) + "," + dis.rn$(rm%) : EXIT FUNCTION
            END IF
            IF type2% = 3 AND shimm% = 0 THEN
                IF s% THEN snm$ = "RRXS" ELSE snm$ = "RRX"
                dis.decDP32reg$ = dis.fmtMne$(snm$) + dis.rn$(rd%) + "," + dis.rn$(rm%) : EXIT FUNCTION
            END IF
            SELECT CASE type2%
            CASE 0 : snm$ = "LSL" : n2% = shimm%
            CASE 1 : snm$ = "LSR" : IF shimm% = 0 THEN n2% = 32 ELSE n2% = shimm%
            CASE 2 : snm$ = "ASR" : IF shimm% = 0 THEN n2% = 32 ELSE n2% = shimm%
            CASE 3 : snm$ = "ROR" : n2% = shimm%
            END SELECT
            IF s% THEN snm$ = snm$ + "S"
            dis.decDP32reg$ = dis.fmtMne$(snm$ + ".W") + dis.rn$(rd%) + "," + dis.rn$(rm%) + ",#" + STR$(n2%) : EXIT FUNCTION
        END IF
        mne$ = "ORR"
    CASE 3
        IF rn% = 15 THEN
            IF s% THEN mne$ = "MVNS" ELSE mne$ = "MVN"
            dis.decDP32reg$ = dis.fmtMne$(mne$ + ".W") + dis.rn$(rd%) + "," + dis.rn$(rm%) + sfx$ : EXIT FUNCTION
        END IF
        mne$ = "ORN"
    CASE 4
        IF s% AND rd% = 15 THEN dis.decDP32reg$ = dis.fmtMne$("TEQ.W") + dis.rn$(rn%) + "," + dis.rn$(rm%) + sfx$ : EXIT FUNCTION
        mne$ = "EOR"
    CASE 8
        IF s% AND rd% = 15 THEN dis.decDP32reg$ = dis.fmtMne$("CMN.W") + dis.rn$(rn%) + "," + dis.rn$(rm%) + sfx$ : EXIT FUNCTION
        mne$ = "ADD"
    CASE 10 : mne$ = "ADC"
    CASE 11 : mne$ = "SBC"
    CASE 13
        IF s% AND rd% = 15 THEN dis.decDP32reg$ = dis.fmtMne$("CMP.W") + dis.rn$(rn%) + "," + dis.rn$(rm%) + sfx$ : EXIT FUNCTION
        mne$ = "SUB"
    CASE 14 : mne$ = "RSB"
    CASE ELSE
        dis.decDP32reg$ = dis.fmtMne$(".hword") + dis.h16$(hw0%) + "," + dis.h16$(hw1%) : EXIT FUNCTION
    END SELECT

    IF s% THEN mne$ = mne$ + "S"
    dis.decDP32reg$ = dis.fmtMne$(mne$ + ".W") + dis.rn$(rd%) + "," + dis.rn$(rn%) + "," + dis.rn$(rm%) + sfx$
END FUNCTION

' Decode 32-bit data-processing (register): shift-by-reg, extend, misc (REV/CLZ/RBIT).
' Precondition: (hw0 AND &HFF00)=&HFA00, hw1[15:12]=1111
FUNCTION dis.decDP32misc$(hw0%, hw1%)
    LOCAL rn%      = hw0% AND &HF
    LOCAL rd%      = (hw1% >> 8) AND &HF
    LOCAL rm%      = hw1% AND &HF
    LOCAL op2%     = (hw1% >> 4) AND &HF
    LOCAL rot%     = (hw1% >> 4) AND 3
    LOCAL hw0mid%  = (hw0% >> 4) AND &H1F
    LOCAL hw0hi%   = hw0% AND &HFFF0
    LOCAL op2sel%  = (hw1% >> 4) AND 3
    LOCAL mne$ LENGTH 8
    LOCAL body$
    LOCAL sel%

    ' Shift-by-register: hw0[8]=0, op=hw0[6:5] (LSL/LSR/ASR/ROR), hw1[7:4]=0000
    IF hw0mid% < 8 AND op2% = 0 THEN
        sel% = (hw0% >> 5) AND 3
        SELECT CASE sel%
        CASE 0 : mne$ = "LSL"
        CASE 1 : mne$ = "LSR"
        CASE 2 : mne$ = "ASR"
        CASE 3 : mne$ = "ROR"
        END SELECT
        IF (hw0% AND &H10) <> 0 THEN mne$ = mne$ + "S"
        dis.decDP32misc$ = dis.fmtMne$(mne$ + ".W") + dis.rn$(rd%) + "," + dis.rn$(rn%) + "," + dis.rn$(rm%)
        EXIT FUNCTION
    END IF

    ' Extend: hw0[8]=0, hw1[7:6]=10 (rot in hw1[5:4])
    IF hw0mid% < 8 AND (hw1% AND &HC0) = &H80 THEN
        sel% = (hw0% >> 4) AND 7
        SELECT CASE sel%
        CASE 0 : IF rn% = 15 THEN mne$ = "SXTH" ELSE mne$ = "SXTAH"
        CASE 1 : IF rn% = 15 THEN mne$ = "UXTH" ELSE mne$ = "UXTAH"
        CASE 4 : IF rn% = 15 THEN mne$ = "SXTB" ELSE mne$ = "SXTAB"
        CASE 5 : IF rn% = 15 THEN mne$ = "UXTB" ELSE mne$ = "UXTAB"
        CASE ELSE
            dis.decDP32misc$ = dis.fmtMne$(".hword") + dis.h16$(hw0%) + "," + dis.h16$(hw1%) : EXIT FUNCTION
        END SELECT
        IF rn% = 15 THEN
            body$ = dis.rn$(rd%) + "," + dis.rn$(rm%)
        ELSE
            body$ = dis.rn$(rd%) + "," + dis.rn$(rn%) + "," + dis.rn$(rm%)
        END IF
        IF rot% <> 0 THEN body$ = body$ + ",ROR #" + STR$(rot% * 8)
        IF rn% = 15 THEN dis.decDP32misc$ = dis.fmtMne$(mne$ + ".W") + body$ ELSE dis.decDP32misc$ = dis.fmtMne$(mne$) + body$
        EXIT FUNCTION
    END IF

    ' Misc: REV/REV16/RBIT/REVSH (hw0 & FFF0 = FA90) and CLZ (hw0 & FFF0 = FAB0)
    IF (hw1% AND &HC0) = &H80 THEN
        IF hw0hi% = &HFA90 THEN
            SELECT CASE op2sel%
            CASE 0 : mne$ = "REV.W"
            CASE 1 : mne$ = "REV16.W"
            CASE 2 : mne$ = "RBIT"
            CASE 3 : mne$ = "REVSH.W"
            END SELECT
            dis.decDP32misc$ = dis.fmtMne$(mne$) + dis.rn$(rd%) + "," + dis.rn$(rm%) : EXIT FUNCTION
        ELSEIF hw0hi% = &HFAB0 AND op2sel% = 0 THEN
            dis.decDP32misc$ = dis.fmtMne$("CLZ") + dis.rn$(rd%) + "," + dis.rn$(rm%) : EXIT FUNCTION
        END IF
    END IF

    dis.decDP32misc$ = dis.fmtMne$(".hword") + dis.h16$(hw0%) + "," + dis.h16$(hw1%)
END FUNCTION

' Decode 32-bit multiply / divide.
' Precondition: (hw0 AND &HFF00)=&HFB00
FUNCTION dis.decMul32$(hw0%, hw1%)
    LOCAL rn%  = hw0% AND &HF
    LOCAL op1% = (hw0% >> 4) AND &HF
    LOCAL ra%  = (hw1% >> 12) AND &HF
    LOCAL rd%  = (hw1% >> 8) AND &HF
    LOCAL op2% = (hw1% >> 4) AND &HF
    LOCAL rm%  = hw1% AND &HF
    LOCAL mne$ LENGTH 6

    LOCAL bt$ LENGTH 2
    SELECT CASE op1%
    CASE 0  ' MUL / MLA / MLS
        IF op2% = 0 THEN
            IF ra% = 15 THEN
                dis.decMul32$ = dis.fmtMne$("MUL.W") + dis.rn$(rd%) + "," + dis.rn$(rn%) + "," + dis.rn$(rm%)
            ELSE
                dis.decMul32$ = dis.fmtMne$("MLA") + dis.rn$(rd%) + "," + dis.rn$(rn%) + "," + dis.rn$(rm%) + "," + dis.rn$(ra%)
            END IF
            EXIT FUNCTION
        ELSEIF op2% = 1 THEN
            dis.decMul32$ = dis.fmtMne$("MLS") + dis.rn$(rd%) + "," + dis.rn$(rn%) + "," + dis.rn$(rm%) + "," + dis.rn$(ra%)
            EXIT FUNCTION
        END IF
    CASE 1  ' SMULBB/BT/TB/TT (ra=15)  /  SMLABB/BT/TB/TT (ra<>15)
        IF op2% < 4 THEN
            IF (op2% AND 2) <> 0 THEN bt$ = "T" ELSE bt$ = "B"
            IF (op2% AND 1) <> 0 THEN bt$ = bt$ + "T" ELSE bt$ = bt$ + "B"
            IF ra% = 15 THEN
                dis.decMul32$ = dis.fmtMne$("SMUL" + bt$) + dis.rn$(rd%) + "," + dis.rn$(rn%) + "," + dis.rn$(rm%)
            ELSE
                dis.decMul32$ = dis.fmtMne$("SMLA" + bt$) + dis.rn$(rd%) + "," + dis.rn$(rn%) + "," + dis.rn$(rm%) + "," + dis.rn$(ra%)
            END IF
            EXIT FUNCTION
        END IF
    CASE 2  ' SMUAD / SMUADX (ra=15)  /  SMLAD / SMLADX (ra<>15)
        IF op2% < 2 THEN
            IF op2% = 1 THEN bt$ = "X" ELSE bt$ = ""
            IF ra% = 15 THEN
                dis.decMul32$ = dis.fmtMne$("SMUAD" + bt$) + dis.rn$(rd%) + "," + dis.rn$(rn%) + "," + dis.rn$(rm%)
            ELSE
                dis.decMul32$ = dis.fmtMne$("SMLAD" + bt$) + dis.rn$(rd%) + "," + dis.rn$(rn%) + "," + dis.rn$(rm%) + "," + dis.rn$(ra%)
            END IF
            EXIT FUNCTION
        END IF
    CASE 3  ' SMULWB/WT (ra=15)  /  SMLAWB/WT (ra<>15)
        IF op2% < 2 THEN
            IF op2% = 1 THEN bt$ = "T" ELSE bt$ = "B"
            IF ra% = 15 THEN
                dis.decMul32$ = dis.fmtMne$("SMULW" + bt$) + dis.rn$(rd%) + "," + dis.rn$(rn%) + "," + dis.rn$(rm%)
            ELSE
                dis.decMul32$ = dis.fmtMne$("SMLAW" + bt$) + dis.rn$(rd%) + "," + dis.rn$(rn%) + "," + dis.rn$(rm%) + "," + dis.rn$(ra%)
            END IF
            EXIT FUNCTION
        END IF
    CASE 4  ' SMUSD / SMUSDX  /  SMLSD / SMLSDX
        IF op2% < 2 THEN
            IF op2% = 1 THEN bt$ = "X" ELSE bt$ = ""
            IF ra% = 15 THEN
                dis.decMul32$ = dis.fmtMne$("SMUSD" + bt$) + dis.rn$(rd%) + "," + dis.rn$(rn%) + "," + dis.rn$(rm%)
            ELSE
                dis.decMul32$ = dis.fmtMne$("SMLSD" + bt$) + dis.rn$(rd%) + "," + dis.rn$(rn%) + "," + dis.rn$(rm%) + "," + dis.rn$(ra%)
            END IF
            EXIT FUNCTION
        END IF
    CASE 5  ' SMMUL / SMMULR (ra=15)  /  SMMLA / SMMLAR (ra<>15)
        IF op2% < 2 THEN
            IF op2% = 1 THEN bt$ = "R" ELSE bt$ = ""
            IF ra% = 15 THEN
                dis.decMul32$ = dis.fmtMne$("SMMUL" + bt$) + dis.rn$(rd%) + "," + dis.rn$(rn%) + "," + dis.rn$(rm%)
            ELSE
                dis.decMul32$ = dis.fmtMne$("SMMLA" + bt$) + dis.rn$(rd%) + "," + dis.rn$(rn%) + "," + dis.rn$(rm%) + "," + dis.rn$(ra%)
            END IF
            EXIT FUNCTION
        END IF
    CASE 6  ' SMMLS / SMMLSR (always with Ra)
        IF op2% < 2 THEN
            IF op2% = 1 THEN bt$ = "R" ELSE bt$ = ""
            dis.decMul32$ = dis.fmtMne$("SMMLS" + bt$) + dis.rn$(rd%) + "," + dis.rn$(rn%) + "," + dis.rn$(rm%) + "," + dis.rn$(ra%)
            EXIT FUNCTION
        END IF
    CASE 7  ' USAD8 (ra=15)  /  USADA8 (ra<>15)
        IF op2% = 0 THEN
            IF ra% = 15 THEN
                dis.decMul32$ = dis.fmtMne$("USAD8") + dis.rn$(rd%) + "," + dis.rn$(rn%) + "," + dis.rn$(rm%)
            ELSE
                dis.decMul32$ = dis.fmtMne$("USADA8") + dis.rn$(rd%) + "," + dis.rn$(rn%) + "," + dis.rn$(rm%) + "," + dis.rn$(ra%)
            END IF
            EXIT FUNCTION
        END IF
    CASE 8  ' SMULL
        IF op2% = 0 THEN mne$ = "SMULL"
    CASE 9  ' SDIV
        IF op2% = 15 THEN mne$ = "SDIV"
    CASE 10 ' UMULL
        IF op2% = 0 THEN mne$ = "UMULL"
    CASE 11 ' UDIV
        IF op2% = 15 THEN mne$ = "UDIV"
    CASE 12 ' SMLAL / SMLALBB family / SMLALD / SMLALDX
        IF op2% = 0 THEN
            mne$ = "SMLAL"
        ELSEIF (op2% AND &HC) = 8 THEN
            ' SMLALBB/BT/TB/TT: op2=10xy
            IF (op2% AND 2) <> 0 THEN bt$ = "T" ELSE bt$ = "B"
            IF (op2% AND 1) <> 0 THEN bt$ = bt$ + "T" ELSE bt$ = bt$ + "B"
            dis.decMul32$ = dis.fmtMne$("SMLAL" + bt$) + dis.rn$(ra%) + "," + dis.rn$(rd%) + "," + dis.rn$(rn%) + "," + dis.rn$(rm%)
            EXIT FUNCTION
        ELSEIF (op2% AND &HE) = &HC THEN
            ' SMLALD / SMLALDX: op2=110x
            IF op2% = 13 THEN bt$ = "X" ELSE bt$ = ""
            dis.decMul32$ = dis.fmtMne$("SMLALD" + bt$) + dis.rn$(ra%) + "," + dis.rn$(rd%) + "," + dis.rn$(rn%) + "," + dis.rn$(rm%)
            EXIT FUNCTION
        END IF
    CASE 13 ' SMLSLD / SMLSLDX
        IF (op2% AND &HE) = &HC THEN
            IF op2% = 13 THEN bt$ = "X" ELSE bt$ = ""
            dis.decMul32$ = dis.fmtMne$("SMLSLD" + bt$) + dis.rn$(ra%) + "," + dis.rn$(rd%) + "," + dis.rn$(rn%) + "," + dis.rn$(rm%)
            EXIT FUNCTION
        END IF
    CASE 14 ' UMLAL / UMAAL
        IF op2% = 0 THEN
            mne$ = "UMLAL"
        ELSEIF op2% = 6 THEN
            dis.decMul32$ = dis.fmtMne$("UMAAL") + dis.rn$(ra%) + "," + dis.rn$(rd%) + "," + dis.rn$(rn%) + "," + dis.rn$(rm%)
            EXIT FUNCTION
        END IF
    END SELECT

    IF mne$ = "" THEN
        dis.decMul32$ = dis.fmtMne$(".hword") + dis.h16$(hw0%) + "," + dis.h16$(hw1%)
        EXIT FUNCTION
    END IF

    IF mne$ = "SDIV" OR mne$ = "UDIV" THEN
        dis.decMul32$ = dis.fmtMne$(mne$) + dis.rn$(rd%) + "," + dis.rn$(rn%) + "," + dis.rn$(rm%)
    ELSE
        ' Long multiply: RdLo=ra, RdHi=rd
        dis.decMul32$ = dis.fmtMne$(mne$) + dis.rn$(ra%) + "," + dis.rn$(rd%) + "," + dis.rn$(rn%) + "," + dis.rn$(rm%)
    END IF
END FUNCTION

' Decode 32-bit load/store byte/halfword/word (imm12, imm8 P/U/W, register, literal).
' Precondition: (hw0 AND &HFE00)=&HF800
FUNCTION dis.decLdSt32$(hw0%, hw1%)
    LOCAL s%    = (hw0% >> 8) AND 1
    LOCAL u12%  = (hw0% >> 7) AND 1
    LOCAL sz%   = (hw0% >> 5) AND 3
    LOCAL l%    = (hw0% >> 4) AND 1
    LOCAL rn%   = hw0% AND &HF
    LOCAL rt%   = (hw1% >> 12) AND &HF
    LOCAL mne$ LENGTH 8
    LOCAL body$
    LOCAL sign$ LENGTH 1
    LOCAL imm%, p%, u%, w%, rm%, imm2%

    ' Reserved: signed-word, signed-store, sz=11
    IF sz% = 3 THEN dis.decLdSt32$ = dis.fmtMne$(".hword") + dis.h16$(hw0%) + "," + dis.h16$(hw1%) : EXIT FUNCTION
    IF s% = 1 AND (sz% = 2 OR l% = 0) THEN dis.decLdSt32$ = dis.fmtMne$(".hword") + dis.h16$(hw0%) + "," + dis.h16$(hw1%) : EXIT FUNCTION

    IF l% THEN
        IF s% THEN
            IF sz% = 0 THEN mne$ = "LDRSB.W" ELSE mne$ = "LDRSH.W"
        ELSE
            SELECT CASE sz%
            CASE 0 : mne$ = "LDRB.W"
            CASE 1 : mne$ = "LDRH.W"
            CASE 2 : mne$ = "LDR.W"
            END SELECT
        END IF
    ELSE
        SELECT CASE sz%
        CASE 0 : mne$ = "STRB.W"
        CASE 1 : mne$ = "STRH.W"
        CASE 2 : mne$ = "STR.W"
        END SELECT
    END IF

    ' PC-relative literal (Rn=15) — always imm12, U from bit[7]
    IF rn% = 15 THEN
        imm% = hw1% AND &HFFF
        IF u12% THEN body$ = "[PC,#" + STR$(imm%) + "]" ELSE body$ = "[PC,#-" + STR$(imm%) + "]"
        dis.decLdSt32$ = dis.fmtMne$(mne$) + dis.rn$(rt%) + "," + body$ : EXIT FUNCTION
    END IF

    ' imm12 positive form
    IF u12% THEN
        imm% = hw1% AND &HFFF
        IF imm% = 0 THEN
            body$ = "[" + dis.rn$(rn%) + "]"
        ELSE
            body$ = "[" + dis.rn$(rn%) + ",#" + STR$(imm%) + "]"
        END IF
        dis.decLdSt32$ = dis.fmtMne$(mne$) + dis.rn$(rt%) + "," + body$ : EXIT FUNCTION
    END IF

    ' imm8 P/U/W form: hw1[11]=1
    IF (hw1% AND &H800) <> 0 THEN
        p% = (hw1% >> 10) AND 1
        u% = (hw1% >> 9) AND 1
        w% = (hw1% >> 8) AND 1
        imm% = hw1% AND &HFF
        IF u% THEN sign$ = "" ELSE sign$ = "-"
        IF p% = 1 AND w% = 0 THEN
            body$ = "[" + dis.rn$(rn%) + ",#" + sign$ + STR$(imm%) + "]"
        ELSEIF p% = 1 AND w% = 1 THEN
            body$ = "[" + dis.rn$(rn%) + ",#" + sign$ + STR$(imm%) + "]!"
        ELSEIF p% = 0 AND w% = 1 THEN
            body$ = "[" + dis.rn$(rn%) + "],#" + sign$ + STR$(imm%)
        ELSE
            dis.decLdSt32$ = dis.fmtMne$(".hword") + dis.h16$(hw0%) + "," + dis.h16$(hw1%) : EXIT FUNCTION
        END IF
        dis.decLdSt32$ = dis.fmtMne$(mne$) + dis.rn$(rt%) + "," + body$ : EXIT FUNCTION
    END IF

    ' Register form: hw1[11:6]=000000, shift imm2 in hw1[5:4]
    IF (hw1% AND &HFC0) = 0 THEN
        rm%   = hw1% AND &HF
        imm2% = (hw1% >> 4) AND 3
        IF imm2% = 0 THEN
            body$ = "[" + dis.rn$(rn%) + "," + dis.rn$(rm%) + "]"
        ELSE
            body$ = "[" + dis.rn$(rn%) + "," + dis.rn$(rm%) + ",LSL #" + STR$(imm2%) + "]"
        END IF
        dis.decLdSt32$ = dis.fmtMne$(mne$) + dis.rn$(rt%) + "," + body$ : EXIT FUNCTION
    END IF

    dis.decLdSt32$ = dis.fmtMne$(".hword") + dis.h16$(hw0%) + "," + dis.h16$(hw1%)
END FUNCTION

' Format special-register name for MRS/MSR SYSm field (ARMv7-M).
FUNCTION dis.sysm$(sysm%)
    SELECT CASE sysm%
    CASE 0   : dis.sysm$ = "APSR"
    CASE 1   : dis.sysm$ = "IAPSR"
    CASE 2   : dis.sysm$ = "EAPSR"
    CASE 3   : dis.sysm$ = "xPSR"
    CASE 5   : dis.sysm$ = "IPSR"
    CASE 6   : dis.sysm$ = "EPSR"
    CASE 7   : dis.sysm$ = "IEPSR"
    CASE 8   : dis.sysm$ = "MSP"
    CASE 9   : dis.sysm$ = "PSP"
    CASE 16  : dis.sysm$ = "PRIMASK"
    CASE 17  : dis.sysm$ = "BASEPRI"
    CASE 18  : dis.sysm$ = "BASEPRI_MAX"
    CASE 19  : dis.sysm$ = "FAULTMASK"
    CASE 20  : dis.sysm$ = "CONTROL"
    CASE ELSE: dis.sysm$ = "#" + STR$(sysm%)
    END SELECT
END FUNCTION

' Barrier option name for DMB/DSB/ISB.
FUNCTION dis.bopt$(opt%)
    SELECT CASE opt%
    CASE 15 : dis.bopt$ = "SY"
    CASE 14 : dis.bopt$ = "ST"
    CASE 13 : dis.bopt$ = "LD"
    CASE 11 : dis.bopt$ = "ISH"
    CASE 10 : dis.bopt$ = "ISHST"
    CASE 9  : dis.bopt$ = "ISHLD"
    CASE 7  : dis.bopt$ = "NSH"
    CASE 6  : dis.bopt$ = "NSHST"
    CASE 5  : dis.bopt$ = "NSHLD"
    CASE 3  : dis.bopt$ = "OSH"
    CASE 2  : dis.bopt$ = "OSHST"
    CASE 1  : dis.bopt$ = "OSHLD"
    CASE ELSE: dis.bopt$ = "#" + STR$(opt%)
    END SELECT
END FUNCTION

' Decode LDM.W/STM.W/LDMDB/STMDB/LDRD/STRD/TBB/TBH.
' Precondition: (hw0 AND &HFE00)=&HE800
FUNCTION dis.decLdm32$(hw0%, hw1%)
    LOCAL rn%   = hw0% AND &HF
    LOCAL w%    = (hw0% >> 5) AND 1
    LOCAL l%    = (hw0% >> 4) AND 1
    LOCAL mask%
    LOCAL rt%, rt2%, imm%, p%, u%
    LOCAL mne$ LENGTH 8
    LOCAL bang$ LENGTH 1
    LOCAL sign$ LENGTH 1
    LOCAL body$
    LOCAL strdRd%, stbRd%

    ' TBB/TBH: hw0 = 0xE8Dx, hw1[15:5]=11110000000, bit4=H
    IF (hw0% AND &HFFF0) = &HE8D0 AND (hw1% AND &HFFE0) = &HF000 THEN
        IF (hw1% AND &H10) <> 0 THEN
            dis.decLdm32$ = dis.fmtMne$("TBH") + "[" + dis.rn$(rn%) + "," + dis.rn$(hw1% AND &HF) + ",LSL #1]"
        ELSE
            dis.decLdm32$ = dis.fmtMne$("TBB") + "[" + dis.rn$(rn%) + "," + dis.rn$(hw1% AND &HF) + "]"
        END IF
        EXIT FUNCTION
    END IF

    ' LDM.W/STM.W (IA): (hw0 & 0xFFC0) = 0xE880
    IF (hw0% AND &HFFC0) = &HE880 THEN
        ' Preserve existing POP.W/PUSH.W aliases (POP.W = LDM.W SP!)
        IF hw0% = &HE8BD THEN
            dis.decLdm32$ = dis.fmtMne$("POP.W") + dis.rlist$(hw1% AND &HDFFF) : EXIT FUNCTION
        END IF
        IF l% THEN mne$ = "LDM.W" ELSE mne$ = "STM.W"
        IF w% THEN bang$ = "!" ELSE bang$ = ""
        mask% = hw1% AND &HDFFF
        dis.decLdm32$ = dis.fmtMne$(mne$) + dis.rn$(rn%) + bang$ + "," + dis.rlist$(mask%)
        EXIT FUNCTION
    END IF

    ' LDMDB/STMDB: (hw0 & 0xFFC0) = 0xE900
    IF (hw0% AND &HFFC0) = &HE900 THEN
        IF hw0% = &HE92D THEN
            dis.decLdm32$ = dis.fmtMne$("PUSH.W") + dis.rlist$(hw1% AND &HDFFF) : EXIT FUNCTION
        END IF
        IF l% THEN mne$ = "LDMDB" ELSE mne$ = "STMDB"
        IF w% THEN bang$ = "!" ELSE bang$ = ""
        mask% = hw1% AND &HDFFF
        dis.decLdm32$ = dis.fmtMne$(mne$) + dis.rn$(rn%) + bang$ + "," + dis.rlist$(mask%)
        EXIT FUNCTION
    END IF

    ' LDRD/STRD: hw0[6]=1, with P=1 or W=1 (i.e. bit[8] or bit[5] set)
    IF (hw0% AND &H40) <> 0 AND ((hw0% AND &H100) <> 0 OR (hw0% AND &H20) <> 0) THEN
        p%    = (hw0% >> 8) AND 1
        u%    = (hw0% >> 7) AND 1
        rt%   = (hw1% >> 12) AND &HF
        rt2%  = (hw1% >> 8) AND &HF
        imm%  = (hw1% AND &HFF) * 4
        IF l% THEN mne$ = "LDRD" ELSE mne$ = "STRD"
        IF u% THEN sign$ = "" ELSE sign$ = "-"
        IF p% = 1 AND w% = 0 THEN
            body$ = "[" + dis.rn$(rn%) + ",#" + sign$ + STR$(imm%) + "]"
        ELSEIF p% = 1 AND w% = 1 THEN
            body$ = "[" + dis.rn$(rn%) + ",#" + sign$ + STR$(imm%) + "]!"
        ELSE
            body$ = "[" + dis.rn$(rn%) + "],#" + sign$ + STR$(imm%)
        END IF
        dis.decLdm32$ = dis.fmtMne$(mne$) + dis.rn$(rt%) + "," + dis.rn$(rt2%) + "," + body$
        EXIT FUNCTION
    END IF

    ' LDREX  : hw0 = E850|Rn,  hw1 = Rt 1111 imm8 (imm8*4 byte offset)
    IF (hw0% AND &HFFF0) = &HE850 AND (hw1% AND &H0F00) = &H0F00 THEN
        rt%   = (hw1% >> 12) AND &HF
        imm%  = (hw1% AND &HFF) * 4
        IF imm% > 0 THEN
            dis.decLdm32$ = dis.fmtMne$("LDREX") + dis.rn$(rt%) + ",[" + dis.rn$(rn%) + ",#" + STR$(imm%) + "]"
        ELSE
            dis.decLdm32$ = dis.fmtMne$("LDREX") + dis.rn$(rt%) + ",[" + dis.rn$(rn%) + "]"
        END IF
        EXIT FUNCTION
    END IF

    ' STREX  : hw0 = E840|Rn,  hw1 = Rt Rd imm8
    IF (hw0% AND &HFFF0) = &HE840 THEN
        rt%     = (hw1% >> 12) AND &HF
        strdRd% = (hw1% >> 8)  AND &HF
        imm%    = (hw1% AND &HFF) * 4
        IF imm% > 0 THEN
            dis.decLdm32$ = dis.fmtMne$("STREX") + dis.rn$(strdRd%) + "," + dis.rn$(rt%) + ",[" + dis.rn$(rn%) + ",#" + STR$(imm%) + "]"
        ELSE
            dis.decLdm32$ = dis.fmtMne$("STREX") + dis.rn$(strdRd%) + "," + dis.rn$(rt%) + ",[" + dis.rn$(rn%) + "]"
        END IF
        EXIT FUNCTION
    END IF

    ' LDREXB / LDREXH  : hw0 = E8D0|Rn,  hw1 = Rt 1111 010H 1111
    IF (hw0% AND &HFFF0) = &HE8D0 AND (hw1% AND &H0FEF) = &H0F4F THEN
        rt% = (hw1% >> 12) AND &HF
        IF (hw1% AND &H10) <> 0 THEN
            dis.decLdm32$ = dis.fmtMne$("LDREXH") + dis.rn$(rt%) + ",[" + dis.rn$(rn%) + "]"
        ELSE
            dis.decLdm32$ = dis.fmtMne$("LDREXB") + dis.rn$(rt%) + ",[" + dis.rn$(rn%) + "]"
        END IF
        EXIT FUNCTION
    END IF

    ' STREXB / STREXH  : hw0 = E8C0|Rn,  hw1 = Rt 1111 010H Rd
    IF (hw0% AND &HFFF0) = &HE8C0 AND (hw1% AND &H0FE0) = &H0F40 THEN
        rt%    = (hw1% >> 12) AND &HF
        stbRd% = hw1% AND &HF
        IF (hw1% AND &H10) <> 0 THEN
            dis.decLdm32$ = dis.fmtMne$("STREXH") + dis.rn$(stbRd%) + "," + dis.rn$(rt%) + ",[" + dis.rn$(rn%) + "]"
        ELSE
            dis.decLdm32$ = dis.fmtMne$("STREXB") + dis.rn$(stbRd%) + "," + dis.rn$(rt%) + ",[" + dis.rn$(rn%) + "]"
        END IF
        EXIT FUNCTION
    END IF

    dis.decLdm32$ = dis.fmtMne$(".hword") + dis.h16$(hw0%) + "," + dis.h16$(hw1%)
END FUNCTION

' ============================================================
' 32-bit Thumb-2 decoder
' ============================================================
FUNCTION dis.dec32$(hw0%, hw1%, addr%)
    LOCAL tgt%
    LOCAL cc4%
    LOCAL rd%
    LOCAL imm4%, imm3%, imm8%, ib%, imm16%
    LOCAL rt%, rn2%, off%
    LOCAL mask%
    LOCAL satIs16%, satImm3%, satImm2%, satRd%, satRn%, satShN%, satSatI%
    LOCAL satSh5%, satIsU%
    LOCAL satMne$ LENGTH 6
    LOCAL extOp%, extRn%, extRd%, extRm%, extRot%
    LOCAL extMne$ LENGTH 8
    LOCAL pkRn%, pkRd%, pkRm%, pkImm3%, pkImm2%, pkTb%, pkShN%
    LOCAL siKind%, siPre%, siRn%, siRd%, siRm%
    LOCAL qMne$ LENGTH 8
    LOCAL miscMne$ LENGTH 8
    LOCAL siBase$ LENGTH 4, siPfx$ LENGTH 2, siSfx$ LENGTH 2

    ' BL (T1): hw0=11110 S imm10, hw1=11 J1 1 J2 imm11  (hw1 bit12=1)
    IF (hw0% AND &HF800) = &HF000 AND (hw1% AND &HD000) = &HD000 THEN
        tgt% = dis.bltgt32%(hw0%, hw1%, addr%)
        dis.dec32$ = dis.fmtMne$("BL") + dis.lblName$(tgt%)
        EXIT FUNCTION
    END IF

    ' B.W (T4): hw0=11110 S imm10, hw1=10 J1 0 J2 imm11  (hw1 bits[15:14]=10, bit12=0)
    IF (hw0% AND &HF800) = &HF000 AND (hw1% AND &HD000) = &H9000 THEN
        tgt% = dis.bltgt32%(hw0%, hw1%, addr%)
        dis.dec32$ = dis.fmtMne$("B.W") + dis.lblName$(tgt%)
        EXIT FUNCTION
    END IF

    ' Bcc.W (T3): hw0=11110 S cond imm6, hw1=10 J1 0 J2 imm11  (hw1 bit12=0)
    ' cond=1110/1111 is reserved → falls through to MRS/MSR/DMB/etc. below.
    IF (hw0% AND &HF800) = &HF000 AND (hw1% AND &HD000) = &H8000 AND ((hw0% >> 6) AND &HF) < &HE THEN
        cc4% = (hw0% >> 6) AND &HF
        tgt% = dis.bcctgt32%(hw0%, hw1%, addr%)
        dis.dec32$ = dis.fmtMne$("B" + dis.cc$(cc4%) + ".W") + dis.lblName$(tgt%)
        EXIT FUNCTION
    END IF

    ' MOVW (T3): hw0=11110 i 100100 imm4, hw1=0 imm3 Rd imm8
    IF (hw0% AND &HFBF0) = &HF240 AND (hw1% AND &H8000) = 0 THEN
        rd%    = (hw1% >> 8) AND &HF
        imm4%  = hw0% AND &HF
        imm3%  = (hw1% >> 12) AND 7
        imm8%  = hw1% AND &HFF
        ib%    = (hw0% >> 10) AND 1
        imm16% = (imm4% << 12) OR (ib% << 11) OR (imm3% << 8) OR imm8%
        dis.dec32$ = dis.fmtMne$("MOVW") + dis.rn$(rd%) + ",#" + STR$(imm16%)
        EXIT FUNCTION
    END IF

    ' MOVT (T1): hw0=11110 i 101100 imm4, hw1=0 imm3 Rd imm8
    IF (hw0% AND &HFBF0) = &HF2C0 AND (hw1% AND &H8000) = 0 THEN
        rd%    = (hw1% >> 8) AND &HF
        imm4%  = hw0% AND &HF
        imm3%  = (hw1% >> 12) AND 7
        imm8%  = hw1% AND &HFF
        ib%    = (hw0% >> 10) AND 1
        imm16% = (imm4% << 12) OR (ib% << 11) OR (imm3% << 8) OR imm8%
        dis.dec32$ = dis.fmtMne$("MOVT") + dis.rn$(rd%) + ",#" + STR$(imm16%)
        EXIT FUNCTION
    END IF

    ' 32-bit load/store byte/halfword/word: hw0[15:9]=1111100
    IF (hw0% AND &HFE00) = &HF800 THEN
        dis.dec32$ = dis.decLdSt32$(hw0%, hw1%)
        EXIT FUNCTION
    END IF

    ' LDM.W/STM.W/LDMDB/STMDB/LDRD/STRD/TBB/TBH: hw0[15:9]=1110100
    IF (hw0% AND &HFE00) = &HE800 THEN
        dis.dec32$ = dis.decLdm32$(hw0%, hw1%)
        EXIT FUNCTION
    END IF

    ' MRS (T1): hw0=&HF3EF, hw1=10 0 0 Rd SYSm
    IF hw0% = &HF3EF AND (hw1% AND &HF000) = &H8000 THEN
        rd%   = (hw1% >> 8) AND &HF
        dis.dec32$ = dis.fmtMne$("MRS") + dis.rn$(rd%) + "," + dis.sysm$(hw1% AND &HFF)
        EXIT FUNCTION
    END IF

    ' MSR (T1): hw0=&HF380|Rn, hw1=10 0 0 mask 0 0 SYSm
    IF (hw0% AND &HFFF0) = &HF380 AND (hw1% AND &HFF00) = &H8800 THEN
        rn2%  = hw0% AND &HF
        dis.dec32$ = dis.fmtMne$("MSR") + dis.sysm$(hw1% AND &HFF) + "," + dis.rn$(rn2%)
        EXIT FUNCTION
    END IF

    ' DMB/DSB/ISB/CLREX: hw0=&HF3BF, hw1=1000 1111 op option
    IF hw0% = &HF3BF AND (hw1% AND &HFF00) = &H8F00 THEN
        SELECT CASE (hw1% >> 4) AND &HF
        CASE &H2 : dis.dec32$ = "CLREX" : EXIT FUNCTION
        CASE &H4 : dis.dec32$ = dis.fmtMne$("DSB") + dis.bopt$(hw1% AND &HF) : EXIT FUNCTION
        CASE &H5 : dis.dec32$ = dis.fmtMne$("DMB") + dis.bopt$(hw1% AND &HF) : EXIT FUNCTION
        CASE &H6 : dis.dec32$ = dis.fmtMne$("ISB") + dis.bopt$(hw1% AND &HF) : EXIT FUNCTION
        END SELECT
    END IF

    ' 32-bit DP modified immediate: hw0[15:11]=11110, hw0[9]=0, hw1[15]=0
    IF (hw0% AND &HFA00) = &HF000 AND (hw1% AND &H8000) = 0 THEN
        dis.dec32$ = dis.decDP32imm$(hw0%, hw1%)
        EXIT FUNCTION
    END IF

    ' ADDW (T4) / ADR.W (T2): hw0[15:11]=11110, hw0[9:8]=10, hw0[7:4]=0000
    IF (hw0% AND &HFBF0) = &HF200 THEN
        rn2%   = hw0% AND &HF
        rd%    = (hw1% >> 8) AND &HF
        ib%    = (hw0% >> 10) AND 1
        imm3%  = (hw1% >> 12) AND 7
        imm8%  = hw1% AND &HFF
        imm16% = (ib% << 11) OR (imm3% << 8) OR imm8%
        IF rn2% = 15 THEN
            dis.dec32$ = dis.fmtMne$("ADR.W") + dis.rn$(rd%) + "," + dis.h32$(((addr% + 4) AND &HFFFFFFFC) + imm16%)
        ELSE
            dis.dec32$ = dis.fmtMne$("ADDW") + dis.rn$(rd%) + "," + dis.rn$(rn2%) + ",#" + STR$(imm16%)
        END IF
        EXIT FUNCTION
    END IF

    ' SUBW (T4) / ADR.W (T3): hw0[15:11]=11110, hw0[9:8]=10, hw0[7:4]=1010
    IF (hw0% AND &HFBF0) = &HF2A0 THEN
        rn2%   = hw0% AND &HF
        rd%    = (hw1% >> 8) AND &HF
        ib%    = (hw0% >> 10) AND 1
        imm3%  = (hw1% >> 12) AND 7
        imm8%  = hw1% AND &HFF
        imm16% = (ib% << 11) OR (imm3% << 8) OR imm8%
        IF rn2% = 15 THEN
            dis.dec32$ = dis.fmtMne$("ADR.W") + dis.rn$(rd%) + "," + dis.h32$(((addr% + 4) AND &HFFFFFFFC) - imm16%)
        ELSE
            dis.dec32$ = dis.fmtMne$("SUBW") + dis.rn$(rd%) + "," + dis.rn$(rn2%) + ",#" + STR$(imm16%)
        END IF
        EXIT FUNCTION
    END IF

    ' SBFX (T1): hw0 AND &HFFF0 = &HF340
    IF (hw0% AND &HFFF0) = &HF340 THEN
        rn2%  = hw0% AND &HF
        rd%   = (hw1% >> 8) AND &HF
        off%  = (((hw1% >> 12) AND 7) << 2) OR ((hw1% >> 6) AND 3)
        mask% = (hw1% AND &H1F) + 1
        dis.dec32$ = dis.fmtMne$("SBFX") + dis.rn$(rd%) + "," + dis.rn$(rn2%) + ",#" + STR$(off%) + ",#" + STR$(mask%)
        EXIT FUNCTION
    END IF

    ' BFI (T1) / BFC: hw0 AND &HFFF0 = &HF360  (Rn=1111 → BFC)
    IF (hw0% AND &HFFF0) = &HF360 THEN
        rn2%  = hw0% AND &HF
        rd%   = (hw1% >> 8) AND &HF
        off%  = (((hw1% >> 12) AND 7) << 2) OR ((hw1% >> 6) AND 3)
        mask% = (hw1% AND &H1F) - off% + 1
        IF rn2% = 15 THEN
            dis.dec32$ = dis.fmtMne$("BFC") + dis.rn$(rd%) + ",#" + STR$(off%) + ",#" + STR$(mask%)
        ELSE
            dis.dec32$ = dis.fmtMne$("BFI") + dis.rn$(rd%) + "," + dis.rn$(rn2%) + ",#" + STR$(off%) + ",#" + STR$(mask%)
        END IF
        EXIT FUNCTION
    END IF

    ' UBFX (T1): hw0 AND &HFFF0 = &HF3C0
    IF (hw0% AND &HFFF0) = &HF3C0 THEN
        rn2%  = hw0% AND &HF
        rd%   = (hw1% >> 8) AND &HF
        off%  = (((hw1% >> 12) AND 7) << 2) OR ((hw1% >> 6) AND 3)
        mask% = (hw1% AND &H1F) + 1
        dis.dec32$ = dis.fmtMne$("UBFX") + dis.rn$(rd%) + "," + dis.rn$(rn2%) + ",#" + STR$(off%) + ",#" + STR$(mask%)
        EXIT FUNCTION
    END IF

    ' Hint instructions (T2): NOP.W / YIELD.W / WFE.W / WFI.W / SEV.W / DBG
    ' hw0 = 0xF3AF, hw1[15:8] = 1000_0000 → op8 in hw1[7:0]
    ' DBG: hw1[15:4] = 1000_0000_1111 → option in hw1[3:0]
    IF hw0% = &HF3AF AND (hw1% AND &HFF00) = &H8000 THEN
        IF (hw1% AND &HF0) = &HF0 THEN
            dis.dec32$ = dis.fmtMne$("DBG") + "#" + STR$(hw1% AND &HF)
            EXIT FUNCTION
        END IF
        IF (hw1% AND &HFF) = 0 THEN dis.dec32$ = "NOP.W"   : EXIT FUNCTION
        IF (hw1% AND &HFF) = 1 THEN dis.dec32$ = "YIELD.W" : EXIT FUNCTION
        IF (hw1% AND &HFF) = 2 THEN dis.dec32$ = "WFE.W"   : EXIT FUNCTION
        IF (hw1% AND &HFF) = 3 THEN dis.dec32$ = "WFI.W"   : EXIT FUNCTION
        IF (hw1% AND &HFF) = 4 THEN dis.dec32$ = "SEV.W"   : EXIT FUNCTION
    END IF

    ' SSAT / SSAT16  : hw0[15:5]=11110011 00x, bit4=0 (sh in bit 5)
    ' USAT / USAT16  : hw0[15:5]=11110011 10x, bit4=0
    ' Guard hw1[15]=0: branch/misc-control space sets hw1[15]=1 and must not match here.
    IF ((hw0% AND &HFFC0) = &HF300 OR (hw0% AND &HFFC0) = &HF380) AND (hw1% AND &H8000) = 0 THEN
        satRn%   = hw0% AND &HF
        satRd%   = (hw1% >> 8) AND &HF
        satImm3% = (hw1% >> 12) AND 7
        satImm2% = (hw1% >> 6) AND 3
        satSh5%  = (hw0% >> 5) AND 1
        satIsU%  = ((hw0% >> 7) AND 1)
        satShN%  = (satImm3% << 2) OR satImm2%
        ' SSAT16/USAT16 alias when sh=1 AND no shift bits in hw1.
        satIs16% = (satSh5% = 1) AND (satImm3% = 0) AND (satImm2% = 0)
        IF satIs16% THEN
            ' SSAT16 stores sat-1 in bits[3:0]; USAT16 stores sat directly.
            IF satIsU% THEN
                satSatI% = hw1% AND &HF
                dis.dec32$ = dis.fmtMne$("USAT16") + dis.rn$(satRd%) + ",#" + STR$(satSatI%) + "," + dis.rn$(satRn%)
            ELSE
                satSatI% = (hw1% AND &HF) + 1
                dis.dec32$ = dis.fmtMne$("SSAT16") + dis.rn$(satRd%) + ",#" + STR$(satSatI%) + "," + dis.rn$(satRn%)
            END IF
        ELSE
            ' SSAT stores sat-1 in bits[4:0]; USAT stores sat directly.
            IF satIsU% THEN
                satSatI% = hw1% AND &H1F
                satMne$ = "USAT"
            ELSE
                satSatI% = (hw1% AND &H1F) + 1
                satMne$ = "SSAT"
            END IF
            dis.dec32$ = dis.fmtMne$(satMne$) + dis.rn$(satRd%) + ",#" + STR$(satSatI%) + "," + dis.rn$(satRn%)
            IF satShN% > 0 THEN
                IF satSh5% THEN
                    dis.dec32$ = dis.dec32$ + ",ASR #" + STR$(satShN%)
                ELSE
                    dis.dec32$ = dis.dec32$ + ",LSL #" + STR$(satShN%)
                END IF
            END IF
        END IF
        EXIT FUNCTION
    END IF

    ' SXTAH/UXTAH/SXTAB16/UXTAB16/SXTAB/UXTAB  +  no-accum SXTH.W/UXTH.W/SXTB16/UXTB16/SXTB.W/UXTB.W
    ' Pattern: hw0[15:7]=1111_1010_0, hw0[7:4]=op (0..5), hw0[3:0]=Rn (1111 = no accum)
    ' hw1[15:12]=1111, hw1[11:8]=Rd, hw1[7:6]=10, hw1[5:4]=rotate, hw1[3:0]=Rm
    IF (hw0% AND &HFF80) = &HFA00 AND (hw1% AND &HF0C0) = &HF080 THEN
        extOp%  = (hw0% >> 4) AND 7
        IF extOp% <= 5 THEN
            extRn%  = hw0% AND &HF
            extRd%  = (hw1% >> 8) AND &HF
            extRm%  = hw1% AND &HF
            extRot% = ((hw1% >> 4) AND 3) * 8
            IF extRn% = 15 THEN
                SELECT CASE extOp%
                CASE 0 : extMne$ = "SXTH.W"
                CASE 1 : extMne$ = "UXTH.W"
                CASE 2 : extMne$ = "SXTB16"
                CASE 3 : extMne$ = "UXTB16"
                CASE 4 : extMne$ = "SXTB.W"
                CASE 5 : extMne$ = "UXTB.W"
                END SELECT
                dis.dec32$ = dis.fmtMne$(extMne$) + dis.rn$(extRd%) + "," + dis.rn$(extRm%)
            ELSE
                SELECT CASE extOp%
                CASE 0 : extMne$ = "SXTAH"
                CASE 1 : extMne$ = "UXTAH"
                CASE 2 : extMne$ = "SXTAB16"
                CASE 3 : extMne$ = "UXTAB16"
                CASE 4 : extMne$ = "SXTAB"
                CASE 5 : extMne$ = "UXTAB"
                END SELECT
                dis.dec32$ = dis.fmtMne$(extMne$) + dis.rn$(extRd%) + "," + dis.rn$(extRn%) + "," + dis.rn$(extRm%)
            END IF
            IF extRot% > 0 THEN dis.dec32$ = dis.dec32$ + ",ROR #" + STR$(extRot%)
            EXIT FUNCTION
        END IF
    END IF

    ' PKHBT / PKHTB  : hw0 AND &HFFF0 = &HEAC0,  hw1[5]=tb (0=BT/LSL, 1=TB/ASR)
    IF (hw0% AND &HFFF0) = &HEAC0 AND (hw1% AND &H8000) = 0 THEN
        pkRn%   = hw0% AND &HF
        pkRd%   = (hw1% >> 8) AND &HF
        pkImm3% = (hw1% >> 12) AND 7
        pkImm2% = (hw1% >> 6) AND 3
        pkTb%   = (hw1% >> 5) AND 1
        pkRm%   = hw1% AND &HF
        pkShN%  = (pkImm3% << 2) OR pkImm2%
        IF pkTb% THEN
            dis.dec32$ = dis.fmtMne$("PKHTB") + dis.rn$(pkRd%) + "," + dis.rn$(pkRn%) + "," + dis.rn$(pkRm%)
            IF pkShN% = 0 THEN pkShN% = 32
            dis.dec32$ = dis.dec32$ + ",ASR #" + STR$(pkShN%)
        ELSE
            dis.dec32$ = dis.fmtMne$("PKHBT") + dis.rn$(pkRd%) + "," + dis.rn$(pkRn%) + "," + dis.rn$(pkRm%)
            IF pkShN% > 0 THEN dis.dec32$ = dis.dec32$ + ",LSL #" + STR$(pkShN%)
        END IF
        EXIT FUNCTION
    END IF

    ' Parallel SIMD ADD/SUB/SAX/ASX  +  misc reg-ops (CLZ/RBIT/REV.W/REV16.W/REVSH.W) +  SEL
    ' All share hw0[15:7]=11111010_1; hw1[15:12]=1111.
    ' Parallel SIMD: hw1[7]=0, prefix in hw1[6:4], op-kind in hw0[7:4].
    ' Misc reg-ops:  hw1[7]=1, hw1[6:4] selects within group.
    IF (hw0% AND &HFF80) = &HFA80 AND (hw1% AND &HF000) = &HF000 THEN
        siKind% = (hw0% >> 4) AND &HF
        siRn%   = hw0% AND &HF
        siRd%   = (hw1% >> 8) AND &HF
        siRm%   = hw1% AND &HF
        siPre%  = (hw1% >> 4) AND &HF
        ' SEL  : hw0[7:4]=1010, hw1[7:4]=1000
        IF siKind% = &HA AND siPre% = 8 THEN
            dis.dec32$ = dis.fmtMne$("SEL") + dis.rn$(siRd%) + "," + dis.rn$(siRn%) + "," + dis.rn$(siRm%)
            EXIT FUNCTION
        END IF
        ' QADD/QDADD/QSUB/QDSUB  : hw0[7:4]=1000, hw1[7:4] in {1000,1001,1010,1011}
        ' Operand order is Rd, Rm, Rn (Rn from hw0, Rm from hw1[3:0]).
        IF siKind% = 8 AND (siPre% AND &HC) = 8 THEN
            SELECT CASE siPre% AND 3
            CASE 0 : qMne$ = "QADD"
            CASE 1 : qMne$ = "QDADD"
            CASE 2 : qMne$ = "QSUB"
            CASE 3 : qMne$ = "QDSUB"
            END SELECT
            dis.dec32$ = dis.fmtMne$(qMne$) + dis.rn$(siRd%) + "," + dis.rn$(siRm%) + "," + dis.rn$(siRn%)
            EXIT FUNCTION
        END IF
        ' CLZ  : hw0[7:4]=1011, hw1[7:4]=1000  (Rn must equal Rm by convention; we just decode Rm)
        IF siKind% = &HB AND siPre% = 8 THEN
            dis.dec32$ = dis.fmtMne$("CLZ") + dis.rn$(siRd%) + "," + dis.rn$(siRm%)
            EXIT FUNCTION
        END IF
        ' REV.W/REV16.W/RBIT/REVSH.W : hw0[7:4]=1001, hw1[7:4] in {1000,1001,1010,1011}
        IF siKind% = 9 AND (siPre% AND &HC) = 8 THEN
            SELECT CASE siPre% AND 3
            CASE 0 : miscMne$ = "REV.W"
            CASE 1 : miscMne$ = "REV16.W"
            CASE 2 : miscMne$ = "RBIT"
            CASE 3 : miscMne$ = "REVSH.W"
            END SELECT
            dis.dec32$ = dis.fmtMne$(miscMne$) + dis.rn$(siRd%) + "," + dis.rn$(siRm%)
            EXIT FUNCTION
        END IF
        ' Parallel ADD/SUB/SAX/ASX : hw1[7]=0, prefix index = hw1[6:4]
        IF (siPre% AND 8) = 0 THEN
            SELECT CASE siKind%
            CASE &H8 : siBase$ = "ADD"  : siSfx$ = "8"
            CASE &H9 : siBase$ = "ADD"  : siSfx$ = "16"
            CASE &HA : siBase$ = "ASX"  : siSfx$ = ""
            CASE &HC : siBase$ = "SUB"  : siSfx$ = "8"
            CASE &HD : siBase$ = "SUB"  : siSfx$ = "16"
            CASE &HE : siBase$ = "SAX"  : siSfx$ = ""
            CASE ELSE: siBase$ = ""
            END SELECT
            IF LEN(siBase$) > 0 THEN
                SELECT CASE siPre% AND 7
                CASE 0 : siPfx$ = "S"
                CASE 1 : siPfx$ = "Q"
                CASE 2 : siPfx$ = "SH"
                CASE 4 : siPfx$ = "U"
                CASE 5 : siPfx$ = "UQ"
                CASE 6 : siPfx$ = "UH"
                CASE ELSE: siPfx$ = ""
                END SELECT
                IF LEN(siPfx$) > 0 THEN
                    dis.dec32$ = dis.fmtMne$(siPfx$ + siBase$ + siSfx$) + dis.rn$(siRd%) + "," + dis.rn$(siRn%) + "," + dis.rn$(siRm%)
                    EXIT FUNCTION
                END IF
            END IF
        END IF
    END IF

    ' 32-bit DP shifted register: hw0[15:9]=1110101, hw1[15]=0
    IF (hw0% AND &HFE00) = &HEA00 AND (hw1% AND &H8000) = 0 THEN
        dis.dec32$ = dis.decDP32reg$(hw0%, hw1%)
        EXIT FUNCTION
    END IF

    ' 32-bit DP register (shift-by-reg, extend, REV/CLZ/RBIT): hw0[15:8]=11111010, hw1[15:12]=1111
    IF (hw0% AND &HFF00) = &HFA00 AND (hw1% AND &HF000) = &HF000 THEN
        dis.dec32$ = dis.decDP32misc$(hw0%, hw1%)
        EXIT FUNCTION
    END IF

    ' 32-bit multiply / divide: hw0[15:8]=11111011
    IF (hw0% AND &HFF00) = &HFB00 THEN
        dis.dec32$ = dis.decMul32$(hw0%, hw1%)
        EXIT FUNCTION
    END IF

    ' Fallback: emit raw halfwords
    dis.dec32$ = dis.fmtMne$(".hword") + dis.h16$(hw0%) + "," + dis.h16$(hw1%)
END FUNCTION
