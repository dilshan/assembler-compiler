; Simple example
; Writes Hello World to the output
; This script is based on Schweigi's assembler-simulator sample.

	jmp start
hello:
	db "Hello World!"
    db 0

start:
	mov c hello
	mov d 232
	call print
    hlt

print:
	push a
	push b
	mov b 0
	
loop:
	mov a [c]
	mov [d] a
	inc c
	inc d
	cmp b [c]
	jnz loop

	pop b
	pop a
	ret      