# CMM2Assembler
An assembler suite for the Color Maximite 2 (CMM2). Assemble and disassemble
ARM assembler direct on the CMM2

Yes, you have read correctly. This is obviously the moste useless programm
suite since the book "How to learn French" was translated into French. Anyway,
it was a challenge, a killer and alone the feeling to have it mastered was
worth all the pain and trears. And, to be honest, I had a little help from my
new assistent Claude.

This suite contains some basic programs arround ARM assmebler

## as.bas [-lsIo <output file>] <input file>
An assembler that understands Thumb and Thumb-2 assembler code. The output is binary code
    
With the option -l the assembler will print the labels table to the screen Option -s does
the same for symbols found in the code. With -I you can define the include directory from
which all include files (see .include directive) will be read. If the output file is not
defined, the input file with the extemsion ".bin" is used.

Just call it: >run "as.bas", "-lso test.bin test.s"

The assembler is a 1.5 pass assembler. In fact it uses only one pass but does some tricks
to do some forward reference scanning. This method comes to a limit when the distance of
a forward reference would require to change from 16-bit to 32-bit command. A two pass
assembler would just replace the command, this one can't and reports an error. In these
cases you have to add .W manually.

## dis.bas [-o <output file>] <input file>
An disassembler, which decodes an ARM assembler file and prints the assembler mnemonics
to the screen or save it into a file

## bin2csub.bas [-o <output file>] <input file>
Convertes a binary file, created by the assembler, into a CSUB module that can be used
in Basic programs

## csub2bin.bas [-pPo <output file>] <input file>
Convertes a CSUB module, extracted from a Basic source file, into a binary file. This was
created just for the symetry :-)

Usually a CSUB has a name and sometimes it is defined with a signature of requested data
types. The binary file cannot preserve this information therefore the flags -p and -P are there.
With -p the binary will get the name of the CSUB Module. In case -P is given then additionally
to the CSUB name, all datatypes will be added to the name. Option -o wil be overwritten by -p or -P.

E.g: CSUB fixBytes STRING,INTEGER,INTEGER
-> the output name will be: "fixbytes_STRING_INTEGER_INTEGER.bin"


## elf2csub.bas [-jo <output file>] <input file>
This little tool reads compiled code (assembler or C) from an ELF binary and converts
it into a CSUB Module. The default behaviour is to create one CSUB Module from all
modules in the ELF file. If you set the -j flag, a seperate CSUB Module wil be created
for each module in the ELF file. This mimics the behaviour of the tool armcfV143.bas.




