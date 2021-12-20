00 101005 mov al, [0x22]
03 201001 add al, [0x20]
06 111005 mov [0x22], al
09 101004 mov al, [0x21]
0c 221000 sub al, 0x01
0f 111004 mov 0x21, al
12 101003 jnz 0x00
15 20001e halt
18 111003 invalid
1b 330000 invalid
1e ff00   invalid
20 a7     data 167
21 1c     data 28
22 00     result
