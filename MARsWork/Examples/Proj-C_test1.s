#
# Testing our phase 1 instructions
#
# data section
.data

# code/instruction section
.text

addi  $1,  $0,  1 		# Place â€œ1â€? in $1
addi  $2,  $0,  2		# Place â€œ2â€? in $2
addi  $3,  $0,  3		# Place â€œ3â€? in $3
addi  $4,  $0,  4		# Place â€œ4â€? in $4
addi  $5,  $0,  5		# Place â€œ5â€? in $5
addi  $6,  $0,  6		# Place â€œ6â€? in $6
addi  $7,  $0,  7		# Place â€œ7â€? in $7
addi  $8,  $0,  8		# Place â€œ8â€? in $8
addi  $9,  $0,  9		# Place â€œ9â€? in $9
lui  $10,  0x1001
sll $0, $0, 0
sll $0, $0, 0
sll $0, $0, 0

sw    $1,   0($10)       # Store "1" at mem 256
sw    $2,   4($10)       # Store "2" at mem 260
sw    $3,   8($10)       # Store "3" at mem 264
sw    $4,  12($10)       # Store "4" at mem 268
sw    $5,  16($10)       # Store "5" at mem 272
sw    $6,  20($10)       # Store "6" at mem 276
sw    $7,  24($10)       # Store "7" at mem 280
sw    $8,  28($10)       # Store "8" at mem 284
sw    $9,  32($10)       # Store "9" at mem 288
sw    $10, 36($10)       # Store "10" at mem 292


lw    $11, 0($10)		 # Load from dMem($10) into $11
lw    $12, 4($10)		 # Load from dMem($10) into $11
lw    $13, 8($10)		 # Load from dMem($10) into $11
lw    $14, 12($10)		 # Load from dMem($10) into $11
lw    $15, 16($10)		 # Load from dMem($10) into $11

sll $0, $0, 0	     # nop
sll $0, $0, 0
sll $0, $0, 0

addi $10, $0, 10

bne $1, $11, addSeq
sll $0, $0, 0

j projBTestp1
sll $0, $0, 0

addSeq:

add $16, $8, $8
add $17, $9, $8
add $18, $9, $9
add $19, $10, $9
add $20, $10, $10

jal projBTestp2
sll $0, $0, 0


addi  $2,  $0,  10              # Place "10" in $v0 to signal an "exit" or "halt"
syscall


projBTestp1:

add $11, $1, $2

addiu $12, $5, 10

addu $13, $4, $5

and $14, $7, $7

andi $15, $9, 120

addi $1, $0, 10

addi $2, $0, 10

sll $0, $0, 0
sll $0, $0, 0
sll $0, $0, 0

beq $1, $2, addSeq
sll $0, $0, 0

projBTestp2:
nor $16, $1, $5

xor $17, $11, $1

xori $18, $4, 8

or $19, $5, $4

ori $20, $1, 256

slt $21, $4, $5

slti $22, $10, 250

sltiu $23, $7, 3

sltu $24, $5, $2

sll $25, $5, 5

srl $26, $6, 3

sra $27, $3, 5

sllv $28, $10, $2

srlv $29, $9, $3

srav $30, $5, $3

sub $22, $19, $5

subu $20, $10, $3

jr $31

sll $0, $0, 0
