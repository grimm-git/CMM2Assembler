    
    .include "armcfunctions.i"

    // CSUB Signature:  CSUB printText STRING
    // Call from MMBasic:  printText str$
    //
    // r0 -> MMBasic string: length byte, then characters (no null terminator)

main:
    push    {r4, r5, r6, lr}

    mov     r5, r0              // r5 = string pointer
    ldrb    r6, [r5], #1        // r6 = length, r5 -> first char

    ldr     r4, =putConsole
    ldr     r4, [r4]

loop:
    cbz     r6, done
    ldrb    r0, [r5], #1
    blx     r4
    subs    r6, r6, #1
    b       loop

done:
    mov     r0, #13
    blx     r4
    mov     r0, #10
    blx     r4

    pop     {r4, r5, r6, pc}
