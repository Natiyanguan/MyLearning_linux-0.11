!
!	setup.s		(C) 1991 Linus Torvalds
!
! setup.s is responsible for getting the system data from the BIOS,
! and putting them into the appropriate places in system memory.
! both setup.s and system has been loaded by the bootblock.
!
! This code asks the bios for memory/disk/other parameters, and
! puts them in a "safe" place: 0x90000-0x901FF, ie where the
! boot-block used to be. It is then up to the protected mode
! system to read them from there before the area is overwritten
! for buffer-blocks.
!

! NOTE! These had better be the same as in bootsect.s!

! 把 setup.s 编译成 setup 放在硬盘的 2~5 扇区；

INITSEG  = 0x9000	! we move boot here - out of the way 把系统数据从 BIOS 移动到 0x90000-0x901FF 这个很远的存储空间里；
SYSSEG   = 0x1000	! system loaded at 0x10000 (65536).
SETUPSEG = 0x9020	! this is the current segment

.globl begtext, begdata, begbss, endtext, enddata, endbss
.text
begtext:
.data
begdata:
.bss
begbss:
.text

entry start
start:

! ok, the read went well so we get current cursor position and save it for
! posterity.

	mov	ax,#INITSEG	! this is done in bootsect already, but...
	mov	ds,ax       ! 设置数据段
	mov	ah,#0x03	! read cursor pos  在同一个中断内部，通过 ah 寄存器的值来选择具体执行哪个功能
	xor	bh,bh
	int	0x10		! save it in known place, con_init fetches
	                ! int 0x10 中断程序执行完毕并返回时，将会在 dx 寄存器里存储好光标的位置，具体说来其高八位 dh 存储了行号，低八位 dl 存储了列号
	mov	[0],dx		! it from 0x90000.      [0] 是数据段的起始地址，0x90000，类似下标访问
	                ! dx寄存器的用法相当于入参和返回值，这里的 0x10 中断号相当于方法名
! Get memory size (extended mem, kB)

	mov	ah,#0x88    ! ah设置为0x88 对应读取内存大小
	int	0x15        ! 调用0x15 中断程序
	mov	[2],ax      ! 返回值存储在 0x90002 处

! Get video-card data:

	mov	ah,#0x0f
	int	0x10
	mov	[4],bx		! bh = display page  bh 寄存器存储了显示页面
	mov	[6],ax		! al = video mode, ah = window width  al 寄存器存储了视频模式，ah 寄存器存储了窗口宽度

! check for EGA/VGA and some config parameters

	mov	ah,#0x12
	mov	bl,#0x10
	int	0x10
	mov	[8],ax		! ax 寄存器存储了配置参数
	mov	[10],bx		! bx 寄存器存储了配置参数
	mov	[12],cx		! cx 寄存器存储了配置参数

! Get hd0 data      ! 获取第一块硬盘的信息。

	mov	ax,#0x0000
	mov	ds,ax
	lds	si,[4*0x41]
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0080
	mov	cx,#0x10
	rep
	movsb

! Get hd1 data      ! 获取第二块硬盘的信息。

	mov	ax,#0x0000
	mov	ds,ax
	lds	si,[4*0x46]
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0090
	mov	cx,#0x10
	rep
	movsb

! Check that there IS a hd1 :-)

	mov	ax,#0x01500
	mov	dl,#0x81
	int	0x13
	jc	no_disk1
	cmp	ah,#3
	je	is_disk1
no_disk1:
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0090
	mov	cx,#0x10
	mov	ax,#0x00
	rep
	stosb
is_disk1:

! now we want to move to protected mode ...

	cli			! no interrupts allowed !  关闭中断
	            ! 后面我们要覆盖掉原本 BIOS 写好的中断向量表，也就是破坏掉原有的表，写上我们自己的中断向量表，所以此时是不允许中断进来的

! first we move the system to it's rightful place

	mov	ax,#0x0000
	cld			! 'direction'=0, movs moves forward
do_move:
	mov	es,ax		! destination segment
	add	ax,#0x1000
	cmp	ax,#0x9000
	jz	end_move    ! ax 增加到 0x9000 时通过 jz end_move 指令跳出
	mov	ds,ax		! source segment
	sub	di,di
	sub	si,si
	mov 	cx,#0x8000
	rep     ! rep 会让紧随其后的指令重复执行 cx 寄存器指定的次数
	movsw   ! 把内存地址 0x10000 处开始往后一直到 0x90000 的内容，统统复制到内存的最开始的 0 位置
	jmp	do_move

! 内存最开始的 0 到 0x80000 这 512K 被 system 模块给占用了，主要包括此前的操作<硬盘第 6 个扇区开始往后的 240 个扇区（约120k）>
! 这个 system 模块就是除了 bootsect 和 setup 之外的全部程序（head.s 作为开头，main.c 和其他文件紧随其后）链接在一起的结果
! 可以理解为操作系统的全部代码。

! 此前为实模式 实模式下叫做段基址
! then we load the segment descriptors
! 移动到保护模式 保护模式下叫段选择子 段选择子里存储着段描述符的索引，
! 通过段描述符索引，可以从全局描述符表 gdt 中找到一个段描述符，段描述符里存储着段基址
! 段基址取出来，再和偏移地址相加，就得到了物理地址（准确说是线性地址，再经过分页转换后才是物理地址）

end_move:
	mov	ax,#SETUPSEG	! right, forgot this at first. didn't work :-)
	mov	ds,ax
	lidt	idt_48		! load idt with 0,0
	lgdt	gdt_48		! load gdt with whatever appropriate lgdt 就表示把后面的值（gdt_48）放在 gdtr 寄存器中
	! CPU 全局描述符表（gdt）

! that was painless, now we enable A20

	call	empty_8042
	mov	al,#0xD1		! command write
	out	#0x64,al
	call	empty_8042
	mov	al,#0xDF		! A20 on
	out	#0x60,al
	call	empty_8042

! well, that went ok, I hope. Now we have to reprogram the interrupts :-(
! we put them right after the intel-reserved hardware interrupts, at
! int 0x20-0x2F. There they won't mess up anything. Sadly IBM really
! messed this up with the original PC, and they haven't been able to
! rectify it afterwards. Thus the bios puts interrupts at 0x08-0x0f,
! which is used for the internal hardware interrupts as well. We just
! have to reprogram the 8259's, and it isn't fun.

	mov	al,#0x11		! initialization sequence
	out	#0x20,al		! send it to 8259A-1
	.word	0x00eb,0x00eb		! jmp $+2, jmp $+2
	out	#0xA0,al		! and to 8259A-2
	.word	0x00eb,0x00eb
	mov	al,#0x20		! start of hardware int's (0x20)
	out	#0x21,al
	.word	0x00eb,0x00eb
	mov	al,#0x28		! start of hardware int's 2 (0x28)
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0x04		! 8259-1 is master
	out	#0x21,al
	.word	0x00eb,0x00eb
	mov	al,#0x02		! 8259-2 is slave
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0x01		! 8086 mode for both
	out	#0x21,al
	.word	0x00eb,0x00eb
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0xFF		! mask off all interrupts for now
	out	#0x21,al
	.word	0x00eb,0x00eb
	out	#0xA1,al

! well, that certainly wasn't fun :-(. Hopefully it works, and we don't
! need no steenking BIOS anyway (except for the initial loading :-).
! The BIOS-routine wants lots of unnecessary data, and it's less
! "interesting" anyway. This is how REAL programmers do it.
!
! Well, now's the time to actually move into protected mode. To make
! things as simple as possible, we do no register set-up or anything,
! we let the gnu-compiled 32-bit programs do that. We just jump to
! absolute address 0x00000, in 32-bit protected mode.
	mov	ax,#0x0001	! protected mode (PE) bit
	lmsw	ax		! This is it!
	jmpi	0,8		! jmp offset 0 of segment 8 (cs)

! This routine checks that the keyboard command queue is empty
! No timeout is used - if this hangs there is something wrong with
! the machine, and we probably couldn't proceed anyway.
empty_8042:
	.word	0x00eb,0x00eb
	in	al,#0x64	! 8042 status port
	test	al,#2		! is input buffer full?
	jnz	empty_8042	! yes - loop
	ret

gdt:
	.word	0,0,0,0		! dummy

	.word	0x07FF		! 8Mb - limit=2047 (2048*4096=8Mb)
	.word	0x0000		! base address=0
	.word	0x9A00		! code read/exec
	.word	0x00C0		! granularity=4096, 386

	.word	0x07FF		! 8Mb - limit=2047 (2048*4096=8Mb)
	.word	0x0000		! base address=0
	.word	0x9200		! data read/write
	.word	0x00C0		! granularity=4096, 386

idt_48:
	.word	0			! idt limit=0
	.word	0,0			! idt base=0L

gdt_48:                 ! 表示一个 48 位的数据 表示在本文件 setup.s （被编译成 setup 二进制文件后）内的偏移量，gdt 所在的内存偏移量。
	.word	0x800		! gdt limit=2048, 256 GDT entries   ! 低16位 - GDT限制(界限)
	.word	512+gdt,0x9	! gdt base = 0X9xxxx                ! 高32位 - GDT基地址
	
.text
endtext:
.data
enddata:
.bss
endbss:
