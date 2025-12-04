/*
 *  linux/boot/head.s
 *
 *  (C) 1991  Linus Torvalds
 */

/*
 *  head.s contains the 32-bit startup code.
 *
 * NOTE!!! Startup happens at absolute address 0x00000000, which is also where
 * the page directory will exist. The startup code will be overwritten by
 * the page directory.
 */

 /*（head.s 作为开头，与各种 .c 和其他 .s 等文件一起）
 *  编译并链接成 system，放在硬盘的随后 240 个扇区
 */

 代码段寄存器（cs）、数据段寄存器（ds）、栈段寄存器（ss）
 cs:eip 表示了我们要执行哪里的代码。ds:xxx 表示了我们要访问哪里的数据。ss:esp 表示了我们的栈顶地址在哪里。
 包括去哪找代码、去哪找数据、去哪找栈，以及如何通过分段和分页机制将逻辑地址转换为最终的物理地址

.text
.globl idt,gdt,pg_dir,tmp_floppy_area
pg_dir:
.globl startup_32
startup_32:
	movl $0x10,%eax
	mov %ax,%ds
	mov %ax,%es
	mov %ax,%fs
	mov %ax,%gs             ！表示这几个段寄存器的值为指向全局描述符表中的第二个段描述符，也就是数据段描述符  0x10即 0001 0000 参考段选择子的结构图
	lss stack_start,%esp    ！lss 指令相当于让 ss:esp 这个栈顶指针，指向了 _stack_start 这个标号的位置 定义在sched.c中
	                        ! 赋值给 ss 的 0x10 仍然按照保护模式下的段选择子去解读，其指向的是全局描述符表中的第二个段描述符（数据段描述符），段基址是 0
	                        ! 赋值给 esp 寄存器的就是 user_stack 数组的末端地址，那最终的栈顶地址，也指向了这里，后面的压栈操作，就是往这个新的栈顶地址处压咯
	call setup_idt
	call setup_gdt
	movl $0x10,%eax		# reload all the segment registers
	mov %ax,%ds		    # after changing gdt. CS was already
	mov %ax,%es		    # reloaded in 'setup_gdt'
	mov %ax,%fs
	mov %ax,%gs
	lss stack_start,%esp
	xorl %eax,%eax
1:	incl %eax		# check that A20 really IS enabled
	movl %eax,0x000000	# loop forever if it isn't
	cmpl %eax,0x100000
	je 1b

/*
 * NOTE! 486 should set bit 16, to check for write-protect in supervisor
 * mode. Then it would be unnecessary with the "verify_area()"-calls.
 * 486 users probably want to set the NE (#5) bit also, so as to use
 * int 16 for math errors.
 */
	movl %cr0,%eax		# check math chip
	andl $0x80000011,%eax	# Save PG,PE,ET
/* "orl $0x10020,%eax" here for 486 might be good */
	orl $2,%eax		# set MP
	movl %eax,%cr0
	call check_x87
	jmp after_page_tables

/*
 * We depend on ET to be correct. This checks for 287/387.
 */
check_x87:
	fninit
	fstsw %ax
	cmpb $0,%al
	je 1f			/* no coprocessor: have to set bits */
	movl %cr0,%eax
	xorl $6,%eax		/* reset MP, set EM */
	movl %eax,%cr0
	ret
.align 2
1:	.byte 0xDB,0xE4		/* fsetpm for 287, ignored by 387 */
	ret

/*
 *  setup_idt
 *
 *  sets up a idt with 256 entries pointing to
 *  ignore_int, interrupt gates. It then loads
 *  idt. Everything that wants to install itself
 *  in the idt-table may do so themselves. Interrupts
 *  are enabled elsewhere, when we can be relatively
 *  sure everything is ok. This routine will be over-
 *  written by the page tables.
 */
setup_idt:
	lea ignore_int,%edx     # ignore_int是一个默认的中断处理程序，当发生未定义的中断时会调用它
	movl $0x00080000,%eax
	movw %dx,%ax		/* selector = 0x0008 = cs */
	movw $0x8E00,%dx	/* interrupt gate - dpl=0, present */

	lea idt,%edi        # 将IDT表的起始地址加载到%edi寄存器中。
	mov $256,%ecx       # 将256移动到%ecx寄存器，表示我们要设置256个中断门(对应256种中断类型)。
rp_sidt:
	movl %eax,(%edi)    # 前4字节(%eax)包含偏移地址的低16位和段选择子
	movl %edx,4(%edi)   # 后4字节(%edx)包含属性和其他信息。  两条指令将之前构造好的中断门描述符(8字节)写入IDT表中当前项
	addl $8,%edi        # 将%edi增加8，指向下一项
	dec %ecx            # 将%ecx减1
	jne rp_sidt         # 如果%ecx不为0，则跳转回rp_sidt继续设置下一项
	lidt idt_descr
	ret

/*
 *  setup_gdt
 *
 *  This routines sets up a new gdt and loads it.
 *  Only two entries are currently built, the same
 *  ones that were built in init.s. The routine
 *  is VERY complicated at two whole lines, so this
 *  rather long comment is certainly needed :-).
 *  This routine will beoverwritten by the page tables.
 */
setup_gdt:
	lgdt gdt_descr
	ret

/*
 * I put the kernel page tables right after the page directory,
 * using 4 of them to span 16 Mb of physical memory. People with
 * more than 16MB will have to expand this.
 */
 # 放置 4 个页表
.org 0x1000
pg0:

.org 0x2000
pg1:

.org 0x3000
pg2:

.org 0x4000
pg3:

.org 0x5000
/*
 * tmp_floppy_area is used by the floppy-driver when DMA cannot
 * reach to a buffer-block. It needs to be aligned, so that it isn't
 * on a 64kB border.
 */
tmp_floppy_area:
	.fill 1024,1,0

after_page_tables:
	pushl $0		# These are the parameters to main :-)   #push 指令就是压栈
	pushl $0
	pushl $0
	pushl $L6		# return address for main, if it decides to.
	pushl $main
	jmp setup_paging
L6:
	jmp L6			# main should never return here, but
				# just in case, we know what happens.

/* This is the default interrupt "handler" :-) */
int_msg:
	.asciz "Unknown interrupt\n\r"
.align 2
ignore_int:
	pushl %eax
	pushl %ecx
	pushl %edx
	push %ds
	push %es
	push %fs
	movl $0x10,%eax
	mov %ax,%ds
	mov %ax,%es
	mov %ax,%fs
	pushl $int_msg
	call printk
	popl %eax
	pop %fs
	pop %es
	pop %ds
	popl %edx
	popl %ecx
	popl %eax
	iret


/*
 * Setup_paging
 *
 * This routine sets up paging by setting the page bit
 * in cr0. The page tables are set up, identity-mapping
 * the first 16MB. The pager assumes that no illegal
 * addresses are produced (ie >4Mb on a 4Mb machine).
 *
 * NOTE! Although all physical memory should be identity
 * mapped by this routine, only the kernel page functions
 * use the >1Mb addresses directly. All "normal" functions
 * use just the lower 1Mb, or the local data space, which
 * will be mapped to some other place - mm keeps track of
 * that.
 *
 * For those with more memory than 16 Mb - tough luck. I've
 * not got it, why should you :-) The source is here. Change
 * it. (Seriously - it shouldn't be too difficult. Mostly
 * change some constants etc. I left it at 16Mb, as my machine
 * even cannot be extended past that (ok, but it was cheap :-)
 * I've tried to show which constants to change by having
 * some kind of marker at them (search for "16Mb"), but I
 * won't guarantee that's all :-( )
 */
 # 举例说明
 # 线性地址（已经经过了分段机制的转换）是15M 二进制：0000000011_0100000000_000000000000
 # 线性地址被CPU拆分为高 10 位：中间 10 位：后 12 位
 # 高 10 位负责在页目录表中找到一个页目录项，
 # 这个页目录项的值加上中间 10 位拼接后的地址去页表中去寻找一个页表项，
 # 这个页表项的值，再加上后 12 位偏移地址，就是最终的物理地址。
 # Linux0.11认为总共可以使用的内存不会超过 16M，也即最大地址空间为 0xFFFFFF 也就是：4（页表数）* 1024（页表项数） * 4KB（一页大小）= 16MB
 # 1 个页目录表最多包含 1024 个页目录项（也就是 1024 个页表），1 个页表最多包含 1024 个页表项（也就是 1024 个页），1 页为 4KB（因为有 12 位偏移地址）
 # 可得知32位系统寻址的最大地址空间为 4GB = 1024 * 1024 * 4KB
 # 这种页表方案叫做二级页表，第一级叫页目录表 PDE，第二级叫页表 PTE
 # 这一切的操作，都离不开计算机的一个硬件叫 MMU，中文名叫内存管理单元，有时也叫 PMMU，中文名是分页内存管理单元。这个部件负责的就是把虚拟地址转换为物理地址。
.align 2
setup_paging:
	movl $1024*5,%ecx		/* 5 pages - pg_dir+4 page tables */
	                        将 1024*5（即5120）移动到 %ecx 寄存器。这代表要处理的内存单元数量。1个页目录表(pg_dir)和4个页表，每个页面包含1024个32位条目。
	xorl %eax,%eax
	xorl %edi,%edi			/* pg_dir is at 0x000 */ %edi 用作内存操作的目标地址指针
	cld;rep;stosl           cld 清除方向标志位(Direction Flag)，使字符串操作指令递增地址
                            rep 是重复前缀，表示重复执行后续指令 %ecx 次
                            stosl 将 %eax 的值存储到 %edi 指向的内存位置，并递增 %edi
                            三者结合的效果是：将从地址0开始的5个页面（20KB）全部清零。
        页目录项/页表项的格式：
        31-12: 页表地址\页物理地址
        11-0:
            第0位 - Present (P)位：
                值为1：表示该页或页表在内存中存在
                值为0：表示该页或页表不在内存中（可能被交换到磁盘）
            第1位 - Read/Write (R/W)位：
                值为0：只读页面，任何写操作都会引发页故障
                值为1：可读写页面
            第2位 - User/Supervisor (U/S)位：
                值为0：仅超级用户(内核态)可以访问此页面
                值为1：用户态和超级用户都可以访问此页面
            第3位 - Write-Through (PWT)位：
                值为0：使用回写(write-back)缓存策略
                值为1：使用直写(write-through)缓存策略
            第4位 - Cache Disable (PCD)位：
                值为0：允许对该页进行缓存
                值为1：禁止对该页进行缓存
            第5位 - Accessed (A)位：
                值为0：自上次清零以来未被访问过
                值为1：已被访问过（由CPU自动设置）
            第6位 - Dirty (D)位（仅页表项有）：
                值为0：页面自上次清零以来未被写入
                值为1：页面已被写入（由CPU自动设置）
                注意：页目录项没有这一位
            第7位 - Page Size (PS)位（仅页目录项有）：
                值为0：使用4KB页面
                值为1：使用4MB大页面（仅在启用页大小扩展时有效）
            第8位 - Global (G)位（仅页表项有）：
                值为0：非全局页面，在TLB刷新时会被清除
                值为1：全局页面，TLB刷新时不被清除（CR4中的PGE位必须置1）
            第9-11位 - Available (AVL)位：
                这3位供操作系统使用，CPU不会修改它们
                可用于存储操作系统特定的信息

	movl $pg0+7,pg_dir		/* set present bit/user r/w */
	movl $pg1+7,pg_dir+4		/*  --------- " " --------- */
	movl $pg2+7,pg_dir+8		/*  --------- " " --------- */
	movl $pg3+7,pg_dir+12		/*  --------- " " --------- */
	                        pg0, pg1, pg2, pg3 是四个页表
                            加上 7 表示设置页表项的标志位：bit 0（Present）和 bit 1（Read/Write）置1，允许用户态访问
                            每个条目占4字节，所以地址依次增加4
	movl $pg3+4092,%edi
	movl $0xfff007,%eax		/*  16Mb - 4096 + 7 (r/w user,p) */
	std
1:	stosl			/* fill pages backwards - more efficient :-) */
	subl $0x1000,%eax       # 将 %eax 的值减去 0x1000（4096，一页的大小）。这是为了准备下一个页表项的值。
	                        # 4KB 是 计算机内存管理的默认页面大小，是计算机科学中广泛采用的约定。
	jge 1b          # 如果 %eax 的值大于等于 0，则跳转回标签 1 继续执行循环 实现了反向填充页表
	xorl %eax,%eax		/* pg_dir is at 0x0000 */ 将 %eax 寄存器清零（异或自身）
	movl %eax,%cr3		/* cr3 - page directory start */    将 %eax 的值（0）移动到控制寄存器 %cr3 中。
	                    这告诉处理器页目录表的起始地址是 0x0000, 再通过页目录表就可以找到所有的页表。%cr3 寄存器用于存储页目录表的物理基地址。
	movl %cr0,%eax      将控制寄存器 %cr0 的值读取到 %eax 寄存器中。%cr0 包含了处理器的各种控制标志。
	orl $0x80000000,%eax        将 %eax 的值与 0x80000000 进行按位或运算。这个值的最高位（第31位）是分页启用位（PG bit）为了设置并启用分页机制
	movl %eax,%cr0		/* set paging (PG) bit */  再将 %eax 的值写入控制寄存器 %cr0 中。这会触发分页机制，并使处理器开始使用页目录表和页表来管理内存地址转换。
	                    cr0 尾部的PE 位（第0位）是保护启用位（Protection Enable bit），用于启用保护模式。
	ret			/* this also flushes prefetch-queue */      ret返回指令，CPU 机械地把栈顶的元素值当做返回地址，跳转去那里执行
	            # 栈顶在 after_page_tables 后push 0,0,0,L6,main

.align 2
.word 0
idt_descr:
	.word 256*8-1		# idt contains 256 entries
	.long idt
.align 2
.word 0
gdt_descr:
	.word 256*8-1		# so does gdt (not that that's any
	.long gdt		# magic number, but it works for me :^)

	.align 8
idt:	.fill 256,8,0		# idt is uninitialized

gdt:	.quad 0x0000000000000000	/* NULL descriptor */
	.quad 0x00c09a0000000fff	/* 16Mb */
	.quad 0x00c0920000000fff	/* 16Mb */
	.quad 0x0000000000000000	/* TEMPORARY - don't use */
	.fill 252,8,0			/* space for LDT's and TSS's etc */
