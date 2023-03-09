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
		push ax bx cx dx si di es ds

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
                mov ax, DefX
                mov dx, DefY
                mov bx, DefH
                mov cx, DefW

                mov si, offset SaveBuffer  ;now si points to Draw Buffer
                call DrawRAM
                mov Flag, 0             ;cause we close box

FinishItr:

		in al, 61h             ;beep to aparatura
		or al, 80h
		out 61h, al
		and al, not 80h
		out 61h, al

		mov al, 20h
		out 20h, al             ;move to itc

		pop ds es di si dx cx bx ax

                iret

NotMyKey:
		pop ds es di si dx cx bx ax

		db 0EAh         ;jmp in old intr function

Old09_Ofs	dw 0
Old09_Seg	dw 0

New09 endp


New08 proc
		push ax bx cx dx si di es ds

                push ax
                mov ax, cs
                mov ds, ax
                pop ax

                mov start_ax, ax
                mov start_bx, bx
                mov start_cx, cx
                mov start_dx, dx

                mov start_di, di
                mov start_si, si

                mov ax, cs
                mov start_cs, ax
                mov ax, ds
                mov start_ds, ax
                mov ax, ss
                mov start_ss, ax
                mov ax, es
                mov start_es, ax

                mov start_sp, sp
                mov start_bp, bp


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
                ;cli

                call DrawSave
                ;sti

                call DrawBuf


                mov si, offset DrawBuffer  ;now si points to Draw Buffer
                call DrawRAM

DoNothing:
		pop ds es di si dx cx bx ax

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
                push ax bx cx dx es ds

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

		add si, 160d

		sub si, bx              ;W
		sub si, bx              ;W

                loop @@Cycle

                ;sub bx, 2               ;restore bx
                pop cx                  ;restore cx

                xchg bx, cx             ;now cx is W and bx is H

                pop ds es dx cx bx ax

                ret
InitSave endp

;========================================================================================
;Draw Buff
;----------------------------------------------------------------------------------------
;Expect:        color in BoxClr
;Parameters:	ax - X. dx - Y. cx - width. bx - high.
;Returns:	-
;Destroys:	si, di, ds, es
;----------------------------------------------------------------------------------------
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
                push ax bx cx dx es ds

                push ax                 ;store ax
                mov ax, 0B800
                mov ds, ax              ;source seg (ds) <- VRAM

                mov ax, cs
                mov es, ax              ;dest seg (es) <- cs
                pop ax                  ;restore ax

                call CoordShift         ;now di points to VRAM

                xchg di, si             ;now si points to VRAM

                mov di, offset DrawBuffer  ;now di points to Draw Buffer

                xchg bx, cx             ;now cx is H and bx is W

                push cx                 ;store cx

                ;add bx, 2               ;cause of shadow

                ;add cx, 1               ;cause of shadow
@@Cycle:
                push bx
                xchg bx, cx     ;now cx is length

@@PlaceCopy:
                call CopyDifferentFromDraw
                add di, 2
                add si, 2
                loop @@PlaceCopy

                xchg bx, cx
                pop bx

		add si, 160d

		sub si, bx              ;W
		sub si, bx              ;W

                loop @@Cycle

                ;sub bx, 2               ;restore bx
                pop cx                  ;restore cx

                xchg bx, cx             ;now cx is W and bx is H

                pop ds es dx cx bx ax

                ret
DrawSave endp

CopyDifferentFromDraw proc
                push ax

                mov ax, word ptr ds:[si]        ;load symbol and color from VRAM

                cmp ax, word ptr es:[di]        ;cmp if VRAM is different from Draw Buf
                je @@NoChange

                push di                         ;store di
                sub di, offset DrawBuffer
                add di, offset SaveBuffer       ;now di points to correct position in Save Buffer
                mov word ptr es:[di], ax        ;change Save Buffer
                pop di                          ;restore di

@@NoChange:
                pop ax

                ret
CopyDifferentFromDraw endp

;========================================================================================
;Draws RAM is si to VRAM
;----------------------------------------------------------------------------------------
;Expect:
;Parameters:	ax - X. dx - Y. cx - width. bx - high. si - offset of RAM
;Returns:
;Destroys:	di
;----------------------------------------------------------------------------------------
DrawRAM proc
                push ax                 ;store ax
                mov ax, cs
                mov ds, ax              ;source seg (ds) <- cs

                mov ax, 0B800
                mov es, ax              ;dest seg (es) <- VRAM
                pop ax

                call CoordShift         ;now di points to VRAM

                xchg bx, cx             ;now cx is H and bx is W

                push cx                 ;store cx

                ;add bx, 2               ;cause of shadow

                ;add cx, 1               ;cause of shadow
@@Cycle:
                call CopyLine

		add di, 160d            ;newline

		sub di, bx              ;sub W
		sub di, bx              ;sub W

                loop @@Cycle

                ;sub bx, 2              ;restore bx
                pop cx                  ;restore cx

                xchg bx, cx             ;now cx is W and bx is H

                ret
DrawRAM endp

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

;========================================================================================
;Draw registers
;----------------------------------------------------------------------------------------
;Expect:        ds = cs, es -> Buf, txt_cnt
;Parameters:
;Returns:
;Destroys:	di, si
;----------------------------------------------------------------------------------------
DrawRegs proc
                mov si, offset txt_cnt
                add si, 1                          ;si points to first txt

                push bx                 ;store

                mov bx, start_ax
                call PrntReg

                mov bx, start_bx
                call PrntReg

                mov bx, start_cx
                call PrntReg

                mov bx, start_dx
                call PrntReg

                add di, cx
                add di, cx              ;skip line

                mov bx, start_si
                call PrntReg

                mov bx, start_di
                call PrntReg

                add di, cx
                add di, cx              ;skip line

                mov bx, start_cs
                call PrntReg

                mov bx, start_ds
                call PrntReg

                mov bx, start_ss
                call PrntReg

                mov bx, start_es
                call PrntReg

                add di, cx
                add di, cx              ;skip line

                mov bx, start_sp
                call PrntReg

                mov bx, start_bp
                call PrntReg

                pop bx                  ;restore

                ret
DrawRegs endp

;========================================================================================
;Print register
;----------------------------------------------------------------------------------------
;Expect:        di -> VRAM
;Parameters:	si - txt
;Returns:
;Destroys:	di, si
;----------------------------------------------------------------------------------------
PrntReg proc
                movsb           ;first letter
                inc di

                movsb           ;second letter
                inc di

                inc di          ;space
                inc di

                call PrntHexBx

                sub di, (1+2+4)*2           ;two letters + space + 4 hex digits
                add di, cx
                add di, cx              ;skip line


                ret
PrntReg endp

PrntHexBx proc
                push ax bx

                ;mov bx, 1AB5h

                mov ax, bx
                shr ax, 4*3             ;1 digit

                call PrntHexDig

                mov ax, bx
                shl ax, 4
                shr ax, 4*3             ;2 digit

                call PrntHexDig

                mov ax, bx
                shl ax, 4*2
                shr ax, 4*3             ;3 digit

                call PrntHexDig

                mov ax, bx
                shl ax, 4*3
                shr ax, 4*3             ;3 digit

                call PrntHexDig

                pop bx ax

                ret
PrntHexBx endp

PrntHexDig proc
                add ax, '0'             ;0 -> '0'

                cmp ax, '9'
                jb @@NoLetterShift

                add ax, 'A' - '9' - 1   ;'9' + 1 -> 'A'
@@NoLetterShift:

                stosb
                inc di

                ret
PrntHexDig endp

include box.asi

Flag            db 0
InitFlag        db 0

ShiftS          dw 0001h
SingleS	        db 0DAh, 0C4h, 0BFh, 0B3h, 20h, 0B3h, 0C0h, 0C4h, 0D9h
DoubleS	        db 0C9h, 0CDh, 0BBh, 0BAh, 20h, 0BAh, 0C8h, 0CDh, 0BCh
LovelyS         db 03h, 03h, 03h, 03h, 20h, 03h, 03h, 03h, 03h

txt_cnt         db 0
txt_ax          db 'a', 'x'
txt_bx          db 'b', 'x'
txt_cx          db 'c', 'x'
txt_dx          db 'd', 'x'

txt_si          db 's', 'i'
txt_di          db 'd', 'i'

txt_cs          db 'c', 's'
txt_ds          db 'd', 's'
txt_ss          db 's', 's'
txt_es          db 'e', 's'

txt_sp          db 's', 'p'
txt_bp          db 'b', 'p'

start_ax        dw 0
start_bx        dw 0
start_cx        dw 0
start_dx        dw 0
start_si        dw 0
start_di        dw 0
start_cs        dw 0
start_ds        dw 0
start_ss        dw 0
start_es        dw 0
start_sp        dw 0
start_bp        dw 0

ShadowS         db 0B1h

BoxClr          db 50h
ShdClr          db 70h

DefX            dw 0d
DefY            dw 0d
DefH            dw 1 + 4 + 2 + 4 + 2 + 1 + 1 + 3;3 skipline
DefW            dw 1 + 1 + 2 + 1 + 4 + 1 + 1

SaveBuffer      dw 200 dup (0)
DrawBuffer      dw 200 dup (0)

EOP:
end		Start
