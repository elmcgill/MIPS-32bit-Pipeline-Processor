 .data
ARRAY:        .word       5,8,12,14,15,20,6,7,14,9,11,4,3,2,1,45

    .text
    .globl  begin
	
begin:
	addi $9, $0, 16
main:
	addi $1, $0, 1
	
	sll $0, $0 0
	sll $0, $0 0
	sll $0, $0 0
	
	sub $5, $9, $1
	
	sll $0, $0 0
	sll $0, $0 0
	sll $0, $0 0
	
	slt $6, $0, $5
	
	sll $0, $0 0
	sll $0, $0 0
	sll $0, $0 0
	
	beq $6, $0, end_sequence
	
	sll $0, $0 0
	
	lui $1, 0x00001001
	
	sll $0, $0 0
	sll $0, $0 0
	sll $0, $0 0
	
	ori $4, $1, 0
	
	addi $10, $0, 0
	
	jal arr_check
	
	sll $0, $0 0
	
	beq $10, $0, end_sequence
	
	sll $0, $0 0
	
	addi $1, $0, 1
	
	sll $0, $0 0
	sll $0, $0 0
	sll $0, $0 0
	
	subu $9, $9, $1
	
	beq $0, $0, main
	
	sll $0, $0 0
	
end_sequence:

	j end
	
	sll $0, $0 0
	
arr_check:

	lw $17, 0($4)
	
	sll $0, $0 0
	sll $0, $0 0
	sll $0, $0 0
	sll $0, $0 0
	
	lw $17, 4($4)
	
	sll $0, $0 0
	sll $0, $0 0
	sll $0, $0 0
	sll $0, $0 0
	
	slt $1, $18, $17
	
	sll $0, $0 0
	sll $0, $0 0
	sll $0, $0 0
	
	bne $1, $0, element_Swap
	
	sll $0, $0 0
	
checkNext_Element:

	addi $4, $4, 4
	
	addi $1, $0, 1
	
	sll $0, $0, 0
	sll $0, $0, 0
	sll $0, $0, 0
	
	sub $5, $5, $1
	
	sll $0, $0 0
	sll $0, $0 0
	sll $0, $0 0
	
	slt $7, $0, $5
	
	sll $0, $0 0
	sll $0, $0 0
	sll $0, $0 0
	
	bne $7, $0, arr_check
	
	sll $0, $0 0
	
	jr $31
	
	sll $0, $0 0
	
element_Swap:

	sw $17, 4($4)
	
	sll $0, $0 0
	sll $0, $0 0
	sll $0, $0 0
	sll $0, $0 0
	
	sw $18, 0($4)
	
	sll $0, $0 0
	sll $0, $0 0
	sll $0, $0 0
	sll $0, $0 0
	
	addi $10, $0, 1
	
	j checkNext_Element
	
	sll $0, $0 0
	
end:
	addi $2, $0, 10
	syscall
