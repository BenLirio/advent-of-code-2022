# ------------------------ #
# ----- read_input ------- #
# ------------------------ #
  .globl read_input
read_input:
  pushq %rbp
	movq	%rsp, %rbp
	subq $128, %rsp

	movl %edi, -4(%rbp) # fd

 # zero out counts
	movb $0, -104(%rbp) # 0th byte
	movb $0, -103(%rbp) # 1st byte
	movb $0, -102(%rbp) # 2nd byte
	movb $0, -101(%rbp) # 3rd byte

	movb $0, -100(%rbp) # skip rest

  movq $0, -112(%rbp) # line idx
	movq $0, -96(%rbp) # zero out counts
	movq $0, -88(%rbp) # ^
	movq $0, -80(%rbp) # ^
	movq $0, -72(%rbp) # ^
.read_loop:
	movl -4(%rbp), %edi # fd
	leaq -64(%rbp), %rsi # buf
	mov $16, %edx # read count
	callq read
	cmpl $-1, %eax
	je .read_failed
	cmpl $0, %eax
	je .read_done
	movq %rax, -128(%rbp) # read count
	movq $0, -120(%rbp) # buf index

# Print: read success
#	mov $1, %rdi
#	movabsq $.read_success_msg, %rsi
#	mov $13, %rdx
#	callq write

.buffer_loop:
	movq -120(%rbp), %rax # buf index
	movb -64(%rbp, %rax, 1), %al # char
	cmpb $10, %al
	je .found_newline
	jmp .found_symbol

.found_newline:
  movq $0, -112(%rbp) # zero line index
	movq $0, -96(%rbp) # zero out counts
	movq $0, -88(%rbp) # ^
	movq $0, -80(%rbp) # ^
	movq $0, -72(%rbp) # ^
	movb $0, -100(%rbp) # skip rest
	jmp .next_char

.found_symbol:
	# Save char in array
	movq -120(%rbp), %rax # buf index
	movb -64(%rbp, %rax, 1), %al # char
	sub $96, %al # char val

	movq -112(%rbp), %rbx # line index
	and $3, %rbx # line index mod 4
	movb %al, -104(%rbp, %rbx, 1) # arr[line index mod 4] = char

	movq -112(%rbp), %rbx # line index
	cmpq $4, %rbx # line index >= 4
	jle .next_char

	movb -104(%rbp), %al # 0th byte
	cmpb %al, -103(%rbp)
	je .next_char
	cmpb %al, -102(%rbp)
	je .next_char
	cmpb %al, -101(%rbp)
	je .next_char

	movb -103(%rbp), %al # 1st byte
	cmpb %al, -102(%rbp)
	je .next_char
	cmpb %al, -101(%rbp)
	je .next_char

	movb -102(%rbp), %al # 2nd byte
	cmpb %al, -101(%rbp)
	je .next_char

	cmpb $1, -100(%rbp) # skip rest
	je .next_char

	movabsq	$.digit_format, %rdi
	movl -112(%rbp), %esi # line index
	movb	$0, %al
	callq	printf
	movb $1, -100(%rbp) # skip rest

.next_char:
	incq -112(%rbp) # inc line idx
	incq -120(%rbp) # buf index
	movq -120(%rbp), %rax
	cmpq %rax, -128(%rbp) # read count, buf index
	je .read_loop
	jmp .buffer_loop

.read_failed:
# Print: read failed
	mov $1, %rdi
	movabsq $.read_failed_msg, %rsi
	mov $12, %rdx
	callq write

.read_done:
	mov $0, %eax
	jmp .read_cleanup

.read_cleanup:
	addq $128, %rsp
  popq %rbp
  retq

# ------------------------ #
# ----- open_input ------- #
# ------------------------ #
  .globl open_input
open_input:
# Setup Stack
  pushq %rbp
	movq	%rsp, %rbp
	subq	$16, %rsp

# Open Input
	movabsq	$.file_name, %rdi
	movq $0, %rsi
	callq open
	movl %eax, -4(%rbp)
	cmpl	$-1, -4(%rbp)
	je .open_failed
# Print: open success
#	mov $1, %rdi
#	movabsq $.open_success_msg, %rsi
#	mov $13, %rdx
#	callq write

	mov -4(%rbp), %eax # return fd
	jmp .open_input_done

.open_failed:
# Print: open failed
	mov $1, %rdi
	movabsq $.open_failed_msg, %rsi
	mov $12, %rdx
	callq write
# Return 1 when open fails
	mov $1, %eax

# Cleanup stack
.open_input_done:
	addq	$16, %rsp
	popq	%rbp
  retq


# ------------------------ #
# -------- solve --------- #
# ------------------------ #
  .globl solve
solve:
# Setup Stack
  pushq %rbp
	movq	%rsp, %rbp
	subq	$16, %rsp


	callq open_input
	movl %eax, -4(%rbp)
	movl -4(%rbp), %edi
	callq read_input


# Close Input
	movl -4(%rbp), %edi
	callq close

# Return 0
	mov $0, %eax
	jmp .solve_done

# Cleanup stack
.solve_done:
	addq	$16, %rsp
	popq	%rbp
  retq

  
# ------------------------ #
# -------- main ---------- #
# ------------------------ #
  .globl main
main:
  pushq %rbp
	movq	%rsp, %rbp

  callq solve

  popq %rbp
  retq


# Constant Strings
.file_name:
	.asciz	"input_small.txt"
	.size	.file_name, 16
.open_failed_msg:
	.asciz	"open failed\n"
	.size	.read_failed_msg, 12
.open_success_msg:
	.asciz	"open success\n"
	.size	.open_success_msg, 13
.read_success_msg:
	.asciz	"read success\n"
	.size	.read_success_msg, 13
.read_failed_msg:
	.asciz	"read failed\n"
	.size	.read_failed_msg, 12
.digit_format:
	.asciz	"%d\n"
	.size	.L.str, 4

# Imports
	.addrsig_sym open
	.addrsig_sym write
	.addrsig_sym close

# 1964