// ============================================================================
//  armcfunctions.inc
//
//  CMM2 MMBasic Interpreter API vector slots and related constants,
//  for use from assembler (CSUBs).  Converted from ARMCFunctions.h (V5.2).
//
//  Each name below is the address of a *slot* in the firmware vector table.
//  The slot itself contains the live function pointer (Thumb bit already set).
//  Typical call sequence:
//
//      ldr     r4, =DrawLine     // address of the slot
//      ldr     r4, [r4]          // -> live function entry
//      blx     r4                // call (AAPCS args in r0..r3, then stack)
//
// ============================================================================

// ---------------------------------------------------------------------------
// API vector-table slot addresses
// ---------------------------------------------------------------------------
    .equ uSec,              0x080002A0   // void uSec(unsigned int us)
    .equ putConsole,        0x080002A4   // void putConsole(int c)
    .equ getConsole,        0x080002A8   // int  getConsole(void)
    .equ ExtCfg,            0x080002AC   // void ExtCfg(int pin, int cfg, int option)
    .equ ExtSet,            0x080002B0   // void ExtSet(int pin, int val)
    .equ ExtInp,            0x080002B4   // int  ExtInp(int pin)
    .equ PinSetBit,         0x080002B8   // void PinSetBit(int pin, unsigned int offset)
    .equ PinRead,           0x080002BC   // int  PinRead(int pin)
    .equ MMPrintString,     0x080002C0   // void MMPrintString(char *s)
    .equ IntToStr,          0x080002C4   // void IntToStr(char *s, long long nbr, unsigned int base)
    .equ CheckAbort,        0x080002C8   // void CheckAbort(void)
    .equ GetMemory,         0x080002CC   // void *GetMemory(size_t msize)
    .equ GetTempMemory,     0x080002D0   // void *GetTempMemory(int NbrBytes)
    .equ FreeMemory,        0x080002D4   // void FreeMemory(void *addr)
    .equ DrawRectangle,     0x080002D8   // void DrawRectangle(int x1,int y1,int x2,int y2,int c)
    .equ DrawBitmap,        0x080002DC   // void DrawBitmap(int x1,int y1,int w,int h,int s,int fg,int bg,unsigned char *bmp)
    .equ DrawLine,          0x080002E0   // void DrawLine(int x1,int y1,int x2,int y2,int w,int c)
    .equ FontTable,         0x080002E4   // const unsigned char *FontTable[FONT_NBR]   (data slot)
    .equ ExtCurrentConfig,  0x080002E8   // int   ExtCurrentConfig[NBRPINS+1]          (data slot)
    .equ HRes,              0x080002EC   // unsigned int HRes                          (data slot)
    .equ VRes,              0x080002F0   // unsigned int VRes                          (data slot)
    .equ SoftReset,         0x080002F4   // void SoftReset(void)
    .equ error,             0x080002F8   // void error(char *msg)
    .equ ProgFlash,         0x080002FC   // int *ProgFlash                             (data slot)
    .equ vartbl,            0x08000300   // struct s_vartbl *vartbl                    (data slot)
    .equ varcnt,            0x08000304   // unsigned int varcnt                        (data slot)
    .equ DrawBuffer,        0x08000308   // void DrawBuffer(int x1,int y1,int x2,int y2,char *buf)
    .equ ReadBuffer,        0x0800030C   // void ReadBuffer(int x1,int y1,int x2,int y2,char *buf)
    .equ FloatToStr,        0x08000310   // void FloatToStr(char *s, MMFLOAT f, int m, int n, char ch)
    .equ ExecuteProgram,    0x08000314   // void ExecuteProgram(char *fname)  (a.k.a. RunBasicSub)
    .equ CFuncmSec,         0x08000318   // unsigned int CFuncmSec                     (data slot)
    .equ CFuncRam,          0x0800031C   // int *CFuncRam (StartOfCFuncRam)            (data slot)
    .equ ScrollLCD,         0x08000320   // void ScrollLCD(int lines, int blank)
    .equ ScrollBufferV,     0x08000324   // void ScrollBufferV(int lines, int blank)
    .equ ScrollBufferH,     0x08000328   // void ScrollBufferH(int pixels)
    .equ DrawBufferFast,    0x0800032C   // void DrawBufferFast(int x1,int y1,int x2,int y2,char *p)
    .equ ReadBufferFast,    0x08000330   // void ReadBufferFast(int x1,int y1,int x2,int y2,char *p)
    .equ MoveBuffer,        0x08000334   // void MoveBuffer(int x1,int y1,int x2,int y2,int w,int h,int flip)
    .equ DrawPixel,         0x08000338   // void DrawPixel(int x, int y, int c)
    .equ RoutineChecks,     0x0800033C   // void routinechecks(void)
    .equ GetPageAddress,    0x08000340   // uint32_t GetPageAddress(int page)
    .equ mycopysafe,        0x08000344   // void mycopysafe(void *out, const void *in, int n)
    .equ IntToFloat,        0x08000348   // MMFLOAT IntToFloat(long long a)
    .equ FloatToInt,        0x0800034C   // long long FloatToInt64(MMFLOAT x)
    .equ Option,            0x08000350   // struct option_s *Option                    (data slot)
    .equ ReadPageAddress,   0x08000354   // unsigned int ReadPageAddress               (data slot)
    .equ WritePageAddress,  0x08000358   // unsigned int WritePageAddress              (data slot)
    .equ Timer,             0x0800035C   // uint64_t timer(void)
    .equ CFuncInt1,         0x08000360   // unsigned int CFuncInt1                     (data slot)
    .equ CFuncInt2,         0x08000364   // unsigned int CFuncInt2                     (data slot)
    .equ FastTimer,         0x08000368   // uint64_t fasttimer(void)
    .equ TicksPerUsec,      0x0800036C   // unsigned int ticks_per_microsecond         (data slot)
    .equ Map,               0x08000370   // int map(int cl)
    .equ Sine,              0x08000374   // MMFLOAT sin(MMFLOAT)
    .equ VideoColour,       0x08000378   // int VideoColour                            (data slot)
    .equ DrawCircle,        0x0800037C   // void DrawCircle(int x,int y,int r,int w,int c,int fill,MMFLOAT aspect)
    .equ DrawTriangle,      0x08000380   // void DrawTriangle(int x0,int y0,int x1,int y1,int x2,int y2,int c,int fill)

// ---------------------------------------------------------------------------
// Variable-table types  (T_*  – field 'type' in struct s_vartbl, often ORed)
// ---------------------------------------------------------------------------
    .equ T_NOTYPE,    0x00     // type not set or discovered
    .equ T_NBR,       0x01     // number (float / double) type
    .equ T_STR,       0x02     // string type
    .equ T_INT,       0x04     // 64-bit integer type
    .equ T_PTR,       0x08     // variable points to another variable's data
    .equ T_IMPLIED,   0x10     // type does not have to be specified with a suffix
    .equ T_CONST,     0x20     // contents cannot be changed

// Layout constants for s_vartbl (handy for indexing vartbl from asm)
    .equ MAXVARLEN,   32       // max length of a variable name
    .equ MAXDIM,      5        // max number of array dimensions

// ---------------------------------------------------------------------------
// Port-register offsets (used with GetPortAddr(a,b))
// ---------------------------------------------------------------------------
    .equ ANSEL,       -8
    .equ ANSELCLR,    -7
    .equ ANSELSET,    -6
    .equ ANSELINV,    -5
    .equ TRIS,        -4
    .equ TRISCLR,     -3
    .equ TRISSET,     -2
    .equ TRISINV,     -1
    .equ PORT,         0
    .equ PORTCLR,      1
    .equ PORTSET,      2
    .equ PORTINV,      3
    .equ LAT,          4
    .equ LATCLR,       5
    .equ LATSET,       6
    .equ LATINV,       7
    .equ ODC,          8
    .equ ODCCLR,       9
    .equ ODCSET,      10
    .equ ODCINV,      11
    .equ CNPU,        12
    .equ CNPUCLR,     13
    .equ CNPUSET,     14
    .equ CNPUINV,     15
    .equ CNPD,        16
    .equ CNPDCLR,     17
    .equ CNPDSET,     18
    .equ CNPDINV,     19
    .equ CNCON,       20
    .equ CNCONCLR,    21
    .equ CNCONSET,    22
    .equ CNCONINV,    23
    .equ CNEN,        24
    .equ CNENCLR,     25
    .equ CNENSET,     26
    .equ CNENINV,     27
    .equ CNSTAT,      28
    .equ CNSTATCLR,   29
    .equ CNSTATSET,   30
    .equ CNSTATINV,   31

// ---------------------------------------------------------------------------
// I/O pin configuration codes (used with ExtCfg(pin, cfg, option))
// ---------------------------------------------------------------------------
    .equ EXT_NOT_CONFIG,      0    // not configured
    .equ EXT_ANA_IN,          1    // analogue input
    .equ EXT_DIG_IN,          2    // digital input
    .equ EXT_FREQ_IN,         3    // frequency-measurement input
    .equ EXT_PER_IN,          4    // period-measurement input
    .equ EXT_CNT_IN,          5    // pulse-counting input
    .equ EXT_INT_HI,          6    // interrupt on RISING edge
    .equ EXT_INT_LO,          7    // interrupt on FALLING edge
    .equ EXT_DIG_OUT,         8    // digital output
    .equ EXT_OC_OUT,          9    // digital output, open-collector
    .equ EXT_INT_BOTH,       10    // interrupt on BOTH edges
    .equ EXT_COM_RESERVED,       100    // reserved – SETPIN/PIN cannot use
    .equ EXT_BOOT_RESERVED,      101    // reserved at boot
    .equ EXT_DS18B20_RESERVED,   102    // reserved for DS18B20

