.286
.model tiny
.code
org 100h

.radix 16
locals @@

Start:
		xor bx, bx
		mov es, bx              ;es <- 0
		mov bx, 4*9             ;shift cause of first 8 + 1 timer itrs

		mov ax, es:[bx]
		mov Old09_Ofs, ax       ;store old offset

		cli
		mov es:[bx], offset New09   ;new offset

		mov ax, es:[bx+2]
		mov Old09_Seg, ax       ;store old segment

		mov ax, cs
		mov es:[bx+2], ax       ;new segment
		sti

                xor bx, bx
		mov es, bx              ;es <- 0
		mov bx, 4*8             ;shift cause of first 8

		mov ax, es:[bx]
		mov Old08_Ofs, ax       ;store old offset

		cli
		mov es:[bx], offset New08   ;new offset

		mov ax, es:[bx+2]
		mov Old08_Seg, ax       ;store old segment

		mov ax, cs
		mov es:[bx+2], ax       ;new segment
		sti

		mov ax, 3100h           ;Terminate and stay resident
		mov dx, offset EOP	;give memory to resident programm
		shr dx, 4               ;but in paragraphs, not bytes! So shr 4
		inc dx                  ;inc 1 for not  loose
		int 21h


New09 proc
		push ax bx cx dx es ds

		in al, 60h
                cmp al, 29h             ;check if it is "`"

                jne Finish

                cmp Flag, 0             ;check if box is opened
                jne SetFlag0

                mov Flag, 1             ;cause we opened box
                jmp Finish

SetFlag0:
                mov Flag, 0             ;cause we closed box

		;in al, 61h             ;beep to aparatura
		;or al, 80h
		;out 61h, al
		;and al, not 80h
		;out 61h, al

		;mov al, 20h
		;out 20h, al             ;move to itc

Finish:
		pop ds es dx cx bx ax

		db 0EAh         ;jmp in old intr function

Old09_Ofs	dw 0
Old09_Seg	dw 0

New09 endp


New08 proc
		push ax bx cx dx es ds

                cmp Flag, 0             ;check if box is closed
                je DoNothing

        ;we are here if box is opened, so

                call DrawRes

DoNothing:
		pop ds es dx cx bx ax

		db 0EAh                 ;jmp in old intr function

Old08_Ofs	dw 0
Old08_Seg	dw 0

New08 endp

DrawRes proc
                mov ax, cs
                mov ds, ax

                mov ax, 0B800
                mov es, ax		;es -> VRAM

                mov ax, DefX
                mov dx, DefY
                mov bx, DefH
                mov cx, DefW

                mov si, offset DOUBLES

                call DrawFrame

                ret
DrawRes endp

include box.asi

Flag           db 0

ShiftS          dw 0001h
SingleS	        db 0DAh, 0C4h, 0BFh, 0B3h, 20h, 0B3h, 0C0h, 0C4h, 0D9h
DoubleS	        db 0C9h, 0CDh, 0BBh, 0BAh, 20h, 0BAh, 0C8h, 0CDh, 0BCh
LovelyS         db 03h, 03h, 03h, 03h, 20h, 03h, 03h, 03h, 03h

ShadowS         db 0B1h

BoxClr          db 50h
ShdClr          db 70h

DefX            dw 0d
DefY            dw 0d
DefH            dw 10d
DefW            dw 20d

Message         db 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h

EOP:
end		Start
