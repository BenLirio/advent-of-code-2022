// syscall NR x8
// return x0
// arg<i> x<i>


.equ EOF, 0
.equ INT_SIZE, 4
.equ BUF_SIZE, 0x2
.equ NEW_LINE, 10
p .req x10
i .req x11
j .req x12
k .req x13
v .req x14
vw .req w14
u .req x15
x .req x16
y .req x17
z .req x18
acc .req x16


buf_reg .req x20

out .req x0
arg0 .req x0
arg1 .req x1


.macro syscall nr:req, arg0, arg1, arg2
  mov x0, \arg0
  mov x1, \arg1
  mov x2, \arg2
  mov x8, \nr
  svc #0
.endm

.macro open, fn:req
  mov x0, #-100
  ldr x1, =\fn
  mov x2, #0
  mov x3, #0
  mov x8, #0x38
  svc #0
.endm

.macro read, fd:req, buf:req, count:req
  syscall 0x3f, \fd, \buf, \count
.endm

.macro show, buf:req, count:req
  syscall #0x40, #1, \buf, \count
.endm

.macro malloc, size:req
  mov	x0, #0		// addr = NULL
	mov	x1, \size	// len = 512 bytes
	mov	x2, #3		// Protection = PROT_READ|PROT_WRITE
	mov	x3, #34		// Shared = Private|Anonymous
	mov	x4, #0		// fd = 0
	mov	x5, #0		// offset = 0
	mov	x8, #0xde
  svc #0
.endm

.macro inc reg:req
  add \reg, \reg, #1
.endm


.globl	main
main:
	sub	sp, sp, #0x50
	stur	x30, [sp, #8]

//; Allocate Buffer (used on all passes)
  malloc BUF_SIZE
  ldr p, =pbuff
  str out, [p]

//; ================ [1st Pass] #lines =================
//; Open input file
  open input_small
  ldr p, =fd
  str out, [p]

//; Calculate number of lines
  mov j, #0
1:
  ldr p, =fd
  ldr arg0, [p]
  ldr p, =pbuff
  ldr arg1, [p]
  read arg0, arg1, BUF_SIZE
  cmp out, EOF
  beq 1f
  ldr p, =num_read
  str out, [p]

  mov i, #-1
  mov k, #0
  ldr p, =pbuff
  ldr buf_reg, [p]
2:
  inc i
  ldrb vw, [buf_reg, i]
  cmp vw, NEW_LINE
  bne 3f
  inc j
3:
  ldr p, =num_read
  ldr v, [p]
  cmp i, v
  bne 2b
  b 1b
1:
  ldr p, =num_lines
  str j, [p]

//; ================ [2nd Pass] Read Values =================

//; Allocate Memory to store input
  ldr p, =num_lines
  ldr v, [p]
  mov u, INT_SIZE
  mul v, v, u
  malloc v
  ldr p, =data
  str out, [p]

//; Reset file offset
  open input_small
  ldr p, =fd
  str out, [p]

//; Read loop
  mov j, #0
  mov acc, #0
1:
  ldr p, =fd
  ldr arg0, [p]
  ldr p, =pbuff
  ldr arg1, [p]
  read arg0, arg1, BUF_SIZE
  cmp out, EOF
  beq 1f
  ldr p, =num_read
  str out, [p]

  mov i, #-1
  ldr p, =pbuff
  ldr buf_reg, [p]
2:
  inc i
  
  mov v, #0 //; zero out entire reg to be safe
  ldrb vw, [buf_reg, i]
  cmp vw, NEW_LINE
  beq 3f
  mov u, #10
  mul acc, acc, u
  add acc, acc, v
  b 4f
3:
  ldr p, =data
  str acc, [p, j]
  inc j
  mov acc, #0
4:

  ldr p, =num_read
  ldr v, [p]
  cmp i, v
  bne 2b
  b 1b
1:



//; print values
  mov i, #-1
1:
  inc i
  ldr p, =data
  ldr v, [p, i]

  mov u, #1
2:
  cmp u, v
  bgt 2f
  mov x, #10
  mul u, u, x
  b 2b
2:
  
  





  ldr p, =num_lines
  ldr v, [p]
  cmp i, v
  bne 1b




	ldur	x30, [sp, #8]
	add	sp, sp, #0x50
	mov	x0, #0
	ret

.data
input_small:  .ascii "input_small.txt\0"
pbuff:        .quad 0
fd:           .quad 0
num_read:     .quad 0
num_lines:    .quad 0
data:         .quad 0
.end
