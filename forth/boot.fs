( Configuration words: RAMSTART, RS_ADDR )
H@ 256 /MOD 2 PC! 2 PC!

( STABLE ABI
  Those jumps below are supposed to stay at these offsets,
  always. If they change bootstrap binaries have to be
  adjusted because they rely on them. Those entries are
  referenced directly by their offset in Forth code with a
  comment indicating what that number refers to.
)

H@ ORG !

0 JPnn,           ( 00, main )
0 JPnn,           ( 03, find )
RAMSTART 0x06 + , ( 06, IP )
NOP, NOP,         ( 08, LATEST )
NOP,              ( 0a, unused )
0 JPnn,           ( 0b, cellWord )
0 JPnn,           ( 0e, compiledWord )
0 JPnn,           ( 11, pushRS )
0 JPnn,           ( 14, popRS )
JP(IY), NOP,      ( 17, nativeWord )
0 JPnn,           ( 1a, next )
0 JPnn,           ( 1d, chkPS )
NOP, NOP,         ( 20, numberWord )
NOP, NOP,         ( 22, litWord )
RAMSTART ,        ( 24, INITIAL_SP )
RAMSTART 0x0e + , ( 26, WORDBUF )
0 JPnn,           ( 28, flagsToBC )
0 JPnn,           ( 2b, doesWord )
RS_ADDR ,         ( 2e, RS_ADDR )
RAMSTART 0x0c + , ( 30, CINPTR )
RAMSTART 0x2e + , ( 32, SYSVNXT )
RAMSTART 0x08 + , ( 34, FLAGS )
RAMSTART 0x0a + , ( 36, PARSEPTR )
RAMSTART 0x04 + , ( 38, HERE )
RAMSTART 0x02 + , ( 3a, CURRENT )

( BOOT DICT
  There are only 5 words in the boot dict, but these words'
  offset need to be stable, so they're part of the "stable
  ABI"
)
'E' A, 'X' A, 'I' A, 'T' A,
0 A,,   ( prev )
4 A,
L1 BSET ( EXIT )
    0x17 A,,         ( nativeWord )
    0x14 CALLnn,     ( popRS )
    0x06 @ LD(nn)HL, ( 0x06 == IP )
    JPNEXT,

NOP, NOP, NOP,   ( unused )

'(' A, 'b' A, 'r' A, ')' A,
H@ L1 @ - A,, ( prev )
4 A,
L1 BSET ( BR )
    0x17 A,,         ( nativeWord )
L2 BSET ( used in CBR )
    0x06 @ LDHL(nn), ( 0x06 == IP )
    E (HL) LDrr,
    HL INCss,
    D (HL) LDrr,
    HL DECss,
    DE ADDHLss,
    0x06 @ LD(nn)HL, ( 0x06 == IP )
    JPNEXT,

'(' A, '?' A, 'b' A, 'r' A, ')' A,
H@ L1 @ - A,, ( prev )
5 A,
L1 BSET ( CBR )
    0x17 A,,         ( nativeWord )
    HL POPqq,
    chkPS,
    A H LDrr,
    L ORr,
    JRZ, L2 BWR ( BR + 2. False, branch )
    ( True, skip next 2 bytes and don't branch )
    0x06 @ LDHL(nn), ( 0x06 == IP )
    HL INCss,
    HL INCss,
    0x06 @ LD(nn)HL, ( 0x06 == IP )
    JPNEXT,

'E' A, 'X' A, 'E' A, 'C' A, 'U' A, 'T' A, 'E' A,
H@ L1 @ - A,, ( prev )
7 A,
L3 BSET ( EXECUTE, used for _bend )
    0x17 A,,         ( nativeWord )
L2 BSET ( used frequently below )
    IY POPqq,        ( is a wordref )
    chkPS,
    L 0 IX+ LDrIXY,
    H 1 IX+ LDrIXY,
    ( HL points to code pointer )
    IY INCss,
    IY INCss,
    ( IY points to PFA )
    JP(HL),

( END OF STABLE ABI )

( Name of BOOT word )
L1 BSET
'B' A, 'O' A, 'O' A, 'T' A, 0 A,

PC ORG @ 1 + ! ( main )
( STACK OVERFLOW PROTECTION:
  To avoid having to check for stack underflow after each pop
  operation (which can end up being prohibitive in terms of
  costs), we give ourselves a nice 6 bytes buffer. 6 bytes
  because we seldom have words requiring more than 3 items
  from the stack. Then, at each "exit" call we check for stack
  underflow.
)
    SP 0xfffa LDddnn,
    0x24 @ SP LD(nn)dd, ( 24 == INITIAL_SP )
    IX RS_ADDR LDddnn,
( LATEST is a label to the latest entry of the dict. It is
  written at offset 0x08 by the process or person building
  Forth. )
    0x08 @ LDHL(nn),
    0x3a @ LD(nn)HL,    ( 3a == CURRENT )
    0x38 @ LD(nn)HL,    ( 38 == HERE )
    HL L1 @ LDddnn,
    0x03 CALLnn,        ( 03 == find )
    DE PUSHqq,
    L2 @ CALLnn,

PC ORG @ 4 + ! ( find )
( Find the entry corresponding to word where (HL) points to
  and sets DE to point to that entry. Z if found, NZ if not. )

    BC PUSHqq,
    HL PUSHqq,
	( First, figure out string len )
    BC 0 LDddnn,
    A XORr,
    CPIR,
	( C has our length, negative, -1 )
    A C LDrr,
    NEG,
    A DECr,
	( special case. zero len? we never find anything. )
    JRZ, L1 FWR ( fail )

    C A LDrr, ( C holds our length )
( Let's do something weird: We'll hold HL by the *tail*.
  Because of our dict structure and because we know our
  lengths, it's easier to compare starting from the end.
  Currently, after CPIR, HL points to char after null. Let's
  adjust. Because the compare loop pre-decrements, instead of
  DECing HL twice, we DEC it once. )
    HL DECss,
    DE 0x3a @ LDddnn,   ( 3a == CURRENT )
L3 BSET ( inner )
    ( DE is a wordref, first step, do our len correspond? )
    HL PUSHqq,          ( --> lvl 1 )
    DE PUSHqq,          ( --> lvl 2 )
    DE DECss,
    LDA(DE),
    0x7f ANDr,          ( remove IMMEDIATE flag )
    C CPr,
    JRZ, L4 FWR ( loopend )
    ( match, let's compare the string then )
    DE DECss, ( Skip prev field. One less because we )
    DE DECss, ( pre-decrement )
    B C LDrr, ( loop C times )
L5 BSET ( loop )
    ( pre-decrement for easier Z matching )
    DE DECss,
    HL DECss,
    LDA(DE),
    (HL) CPr,
    JRNZ, L6 FWR ( loopend )
    DJNZ, L5 BWR ( loop )
L4 FSET L6 FSET ( loopend )
( At this point, Z is set if we have a match. In all cases, we
  want to pop HL and DE )
    DE POPqq,           ( <-- lvl 2 )
    HL POPqq,           ( <-- lvl 1 )
    JRZ, L4 FWR ( end, match? we're done! )
    ( no match, go to prev and continue )
    HL PUSHqq,          ( --> lvl 1 )
    DE DECss,
    DE DECss,
    DE DECss,           ( prev field )
    DE PUSHqq,          ( --> lvl 2 )
    EXDEHL,
    E (HL) LDrr,
    HL INCss,
    D (HL) LDrr,
    ( DE conains prev offset )
    HL POPqq,           ( <-- lvl 2 )
    ( HL is prev field's addr. Is offset zero? )
    A D LDrr,
    E OR,
    JRZ, L6 FWR ( noprev )
    ( get absolute addr from offset )
    ( carry cleared from "or e" )
    DE SBCHLss,
    EXDEHL,             ( result in DE )
L6 FSET ( noprev )
    HL POPqq,           ( <-- lvl 1 )
    JRNZ, L3 BWR ( inner, try to match again )
    ( Z set? end of dict, unset Z )
L1 FSET ( fail )
    A XORr,
    A INCr,
L4 FSET ( end )
    HL POPqq,
    BC POPqq,
    RET,

H@ 256 /MOD 2 PC! 2 PC!
