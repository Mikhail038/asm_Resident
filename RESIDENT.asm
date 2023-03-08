.286
.model tiny
.code
org 100h

.radix 16
locals @@

Start:
                ;call New08
                ;ret

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

                ;jmp NOTMYKEY

                mov ax, cs
                mov ds, ax

		in al, 60h              ;take scan code

                cmp al, 29h             ;check if it is "`"
                je MyKey

                cmp al, 89h             ;check if it is "`"-unpress
                je FinishItr

                jmp NotMyKey            ;we are here if it is not my key

MyKey:
                cmp Flag, 0             ;check if box is opened
                jne SetFlag0

                mov Flag, 1             ;cause we open box
                jmp FinishItr

SetFlag0:
                mov Flag, 0             ;cause we close box

FinishItr:
                pop ds es dx cx bx ax

		in al, 61h             ;beep to aparatura
		or al, 80h
		out 61h, al
		and al, not 80h
		out 61h, al

		mov al, 20h
		out 20h, al             ;move to itc

                ;iret

NotMyKey:
		pop ds es dx cx bx ax

		db 0EAh         ;jmp in old intr function

Old09_Ofs	dw 0
Old09_Seg	dw 0

New09 endp


New08 proc
		push ax bx cx dx es ds

                ;jmp DoNothing

                mov ax, cs
                mov ds, ax

                cmp Flag, 0             ;check if box is closed
                je DoNothing

        ;we are here if box is opened, so

                mov ax, 0B800
                mov es, ax		;es -> VRAM

                mov ax, DefX
                mov dx, DefY
                mov bx, DefH
                mov cx, DefW

                cmp InitFlag, 1
                je NoInitNeeded

                call InitSave
                mov InitFlag, 1

NoInitNeeded:
                call DrawBuf

                ;call DrawSave

                call DrawVRAM

DoNothing:
		pop ds es dx cx bx ax

                ;ret
		db 0EAh                 ;jmp in old intr function

Old08_Ofs	dw 0
Old08_Seg	dw 0

New08 endp

;========================================================================================
;Initialize Save Buffer
;----------------------------------------------------------------------------------------
;Expect:        es - B800, ds = cs
;Parameters:	ax - X. dx - Y. cx - width. bx - high.
;Returns:
;Destroys:	di, si, ax, dx, es, ds
;----------------------------------------------------------------------------------------
InitSave proc
                push ax bx cx dx
                push es ds

                push ax                 ;store ax
                mov ax, 0B800
                mov ds, ax              ;source seg (ds) <- VRAM

                mov ax, cs
                mov es, ax              ;dest seg (es) <- cs
                pop ax                  ;restore ax

                call CoordShift         ;now di points to VRAM

                xchg di, si             ;now si points to VRAM

                mov di, offset SaveBuffer  ;now di points to Save Buffer

                xchg bx, cx             ;now cx is H and bx is W

                push cx                 ;store cx

                ;add bx, 2               ;cause of shadow

                ;add cx, 1               ;cause of shadow
@@Cycle:
                call CopyLine

                loop @@Cycle

                ;sub bx, 2               ;restore bx
                pop cx                  ;restore cx

                xchg bx, cx             ;now cx is W and bx is H

                pop ds es
                pop dx cx bx ax

                ret
InitSave endp


DrawBuf proc
                push ax                 ;store ax
                mov ax, cs
                mov ds, ax              ;source seg (ds) <- cs

                mov ax, cs
                mov es, ax              ;dest seg (es) <- cs
                pop ax                  ;restore ax

                mov di, offset DrawBuffer
                mov si, offset DoubleS

                push ax bx cx dx

                call NNDrawFrame

                pop dx cx bx ax

                ret
DrawBuf endp


DrawSave proc

                ret
DrawSave endp


DrawVRAM proc
                nop

                push ax                 ;store ax
                mov ax, cs
                mov ds, ax              ;source seg (ds) <- cs

                mov ax, 0B800
                mov es, ax              ;dest seg (es) <- VRAM
                pop ax

                call CoordShift         ;now di points to VRAM

                mov si, offset DrawBuffer  ;now si points to Draw Buffer

                xchg bx, cx             ;now cx is H and bx is W

                push cx                 ;store cx

                ;add bx, 2               ;cause of shadow

                ;add cx, 1               ;cause of shadow
@@Cycle:
                call CopyLine

                ;sub di, 4

		add di, 160d            ;newline

		sub di, bx              ;sub W
		sub di, bx              ;sub W

                loop @@Cycle

                ;sub bx, 2              ;restore bx
                pop cx                  ;restore cx

                xchg bx, cx             ;now cx is W and bx is H

                ret
DrawVRAM endp

;========================================================================================
;Copy line
;----------------------------------------------------------------------------------------
;Expect:        es, ds
;Parameters:	bx - length
;Returns:
;Destroys:	di, si
;----------------------------------------------------------------------------------------
CopyLine proc
                push bx
                xchg bx, cx     ;now cx is length

@@PlaceCopy:
                movsw
                loop @@PlaceCopy

                xchg bx, cx
                pop bx

                ret
CopyLine endp

DrawRegs proc

                ret
DrawRegs endp

include box.asi

Flag            db 0
InitFlag        db 0

ShiftS          dw 0001h
SingleS	        db 0DAh, 0C4h, 0BFh, 0B3h, 20h, 0B3h, 0C0h, 0C4h, 0D9h
DoubleS	        db 0C9h, 0CDh, 0BBh, 0BAh, 20h, 0BAh, 0C8h, 0CDh, 0BCh
LovelyS         db 03h, 03h, 03h, 03h, 20h, 03h, 03h, 03h, 03h

ShadowS         db 0B1h

BoxClr          db 50h
ShdClr          db 70h

DefX            dw 0d
DefY            dw 0d
DefH            dw 1 + 4 + 1
DefW            dw 1 + 2 + 1 + 4 + 1

SaveBuffer      dw 200 dup (0)
DrawBuffer      dw 200 dup (0)

Message         db 200 dup (0)

EOP:
end		Start
