00 10 mov ecx, 0x0e
01 10 mov al, 0x08
02 10 mov eax, [al]
03 cc int 0x80
04 28 sub ecx, 0x01
05 70 jz 0x17
06 05 add al, 0x01
07 e9 jmp 0x02
08 48 data
09 65 data
0a 6c data
0b 6c data
0c 6f data
0d 2c data
0e 20 data
10 77 data
11 6f data
12 72 data
13 6c data
14 64 data
15 21 data
16 00 data
17 ff halt
