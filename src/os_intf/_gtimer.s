
| Procedure to read the Global_Clock and write the scope bits
| from user address space.
| The moves instruction uses the DFC or SFC register to change
| the address space for that move only. The address is either
| 0xFFFF0010 for MVME147s or 0x00C00010 for HKV3Ds.

|  data = rd_Global_Clock ( address of Global Clock)
|     (returns int in d0 )

        .globl  _rd_Global_Clock

_rd_Global_Clock:

        movl    sp@(0x4), a1    | get address of Global Clock
        moveq   #1, d1          | load in user address space
        movc    d1, sfc         | load source function register

        moveq   #0, d0          | clear d0
        movsw   a1@, d0         | get MSW data in user space
        swap    d0              | move data to upper half
        addql   #2, a1          | incr address by two
        movsw   a1@, d0         | get LSW data in user space
                                | return value is in d0
        rts                     | return to calling function


|  wr_Scope_Port ( address of oscilloscope port, data to be written )

        .globl  _wr_Scope_Port

_wr_Scope_Port:

        movl    sp@(0x4), a1    | get address of scope port
        movl    sp@(0x8), d0    | get data to be written
        moveq   #1, d1          | load in user address space
        movc    d1, dfc         | load destination function register

        movsw   d0, a1@         | write data in user space

        rts                     | return to calling function
