    
    .include "armcfunctions.i"

    // CSUB SIgnature: CSUB printText
    // Call from MMBasic: printText()

main:
    push    {r4, lr}

    adr     r0, msg
    ldr     r4, =MMPrintString
    ldr     r4, [r4]
    blx     r4

    pop     {r4, pc}

    .align  2
msg:
    .asciz  "My first CSUB!\n"
