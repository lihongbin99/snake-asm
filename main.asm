.386
.model flat, STDCALL

start_game PROTO
printNum PROTO, number:DWORD
numToStr PROTO, number:DWORD, writeTo:PTR WORD
strLen   PROTO, message:PTR WORD
exit     PROTO, message:PTR WORD
exitGame PROTO, exitCode:DWORD

; Api
INCLUDE api.inc

.data
INCLUDE message.inc
inputHandle    DWORD 0
outputHandle   DWORD 0
outputWriteLen DWORD 0
errorMessage   DWORD 0

bufferInfo CONSOLE_SCREEN_BUFFER_INFO <>
cursorPosition COORD <>
pconsoleCursorInfo PCONSOLE_CURSOR_INFO <>
xMiddle WORD 0
yMiddle WORD 0
info1Index   WORD 0
info2Index   WORD 0
info3Index   WORD 0

writePosition COORD <>
tempBuf       DWORD 0
tempBufLen    DWORD 65536
numBuf        WORD 16 DUP(0)

is_start BYTE 0
need_exit BYTE 0
start_time DWORD 0
game_time  DWORD 0
player1Direction BYTE 4
player2Direction BYTE 6
inputRecord INPUT_RECORD <>
inputRecordLen DWORD 0

heap         DWORD 0
payler1Heap  DWORD 0
payler1Index DWORD 0
payler1End   DWORD 0
payler2Heap  DWORD 0
payler2Index DWORD 0
payler2End   DWORD 0
deadHeap     DWORD 0
palyer1Start BYTE 0
palyer2Start BYTE 0
palyer1Init  BYTE 0
palyer2Init  BYTE 0
seed         DWORD 0
mainSize     DWORD 0
windowsSize  DWORD 0
food         WORD 00020H, 0
foodIndex    COORD <>
needAddFood  BYTE 0
game_over    BYTE 0
player1Food  DWORD 0
player2Food  DWORD 0

.code
main PROC
    ; 获取输出流
    INVOKE GetStdHandle, -11
    CMP EAX, -1
    JZ exit
    MOV outputHandle, EAX

    ; 分配堆
    INVOKE HeapCreate, 0, 0, 536870912; 512MB
    MOV heap, EAX
    INVOKE HeapAlloc, heap, 8, tempBufLen
    MOV tempBuf, EAX

    ; 获取输入流
    INVOKE GetStdHandle, -10
    MOV inputHandle, EAX
    CMP inputHandle, -1
    MOV errorMessage, OFFSET GetInputStdHandleError
    JZ return

    ;关闭光标发亮
    INVOKE GetConsoleCursorInfo, outputHandle, ADDR pconsoleCursorInfo
    CMP EAX, 0
    MOV errorMessage, OFFSET GetConsoleCursorInfoError
    JZ return

    ; 初始化
    pre_init:
        CALL init

    game:
        ; 监听按键
        INVOKE ReadConsoleInputW, inputHandle, ADDR inputRecord, 1, ADDR inputRecordLen
        MOV AX, inputRecord.EventType
        ; 判断是否改变了游戏窗口大小
        CMP AX, 04H
        JZ change_windows_size
        ; 判断是否按了按键
        CMP AX, 01H
        JNZ game
        ;INVOKE printNum, inputRecord.Event.KeyEvent.uChar.UnicodeChar; 打印字符代码
        CMP inputRecord.Event.KeyEvent.wRepeatCount, 0
        JZ game

        CMP inputRecord.Event.KeyEvent.uChar.UnicodeChar, 22272
        JZ direction1
        CMP inputRecord.Event.KeyEvent.uChar.UnicodeChar, 16640
        JZ direction2
        CMP inputRecord.Event.KeyEvent.uChar.UnicodeChar, 21248
        JZ direction3
        CMP inputRecord.Event.KeyEvent.uChar.UnicodeChar, 17408
        JZ direction4
        CMP inputRecord.Event.KeyEvent.uChar.UnicodeChar, 9728
        JZ direction5
        CMP inputRecord.Event.KeyEvent.uChar.UnicodeChar, 9472
        JZ direction6
        CMP inputRecord.Event.KeyEvent.uChar.UnicodeChar, 10240
        JZ direction7
        CMP inputRecord.Event.KeyEvent.uChar.UnicodeChar, 9984
        JZ direction8

        ; 循环监听按键
        JMP game

    direction1:
        CMP palyer1Start, 0
        JZ no_change1
        CMP player1Direction, 3
        JZ no_change1
        MOV player1Direction, 1
        no_change1:
        MOV palyer1Start, 1
        CMP is_start, 0
        JZ start
        JMP game
    direction2:
        CMP palyer1Start, 0
        JZ no_change2
        CMP player1Direction, 4
        JZ no_change2
        MOV player1Direction, 2
        no_change2:
        MOV palyer1Start, 1
        CMP is_start, 0
        JZ start
        JMP game
    direction3:
        MOV palyer1Start, 1
        CMP player1Direction, 1
        JZ no_change3
        MOV player1Direction, 3
        no_change3:
        CMP is_start, 0
        JZ start
        JMP game
    direction4:
        MOV palyer1Start, 1
        CMP player1Direction, 2
        JZ no_change4
        MOV player1Direction, 4
        no_change4:
        CMP is_start, 0
        JZ start
        JMP game
    direction5:
        MOV palyer2Start, 1
        CMP player2Direction, 7
        JZ no_change5
        MOV player2Direction, 5
        no_change5:
        CMP is_start, 0
        JZ start
        JMP game
    direction6:
        MOV palyer2Start, 1
        CMP player2Direction, 8
        JZ no_change6
        MOV player2Direction, 6
        no_change6:
        CMP is_start, 0
        JZ start
        JMP game
    direction7:
        CMP palyer2Start, 0
        JZ no_change7
        CMP player2Direction, 5
        JZ no_change7
        MOV player2Direction, 7
        no_change7:
        MOV palyer2Start, 1
        CMP is_start, 0
        JZ start
        JMP game
    direction8:
        CMP palyer2Start, 0
        JZ no_change8
        CMP player2Direction, 6
        JZ no_change8
        MOV player2Direction, 8
        no_change8:
        MOV palyer2Start, 1
        CMP is_start, 0
        JZ start
        JMP game

    start:
        MOV is_start, 1
        INVOKE CreateThread, 0, 4096, OFFSET start_game, 0, 0, 0
        JMP game

    change_windows_size:
        CMP is_start, 1
        JZ ok_exit; 如果游戏已经开始则退出程序
        JMP pre_init; 如果游戏未开始则重新初始化窗口

    ok_exit:
    MOV need_exit, 1; 通知多线程关闭
    CALL gameOverFun

    ; 异常退出
    return:
        INVOKE exit, errorMessage
        RET
main ENDP

init PROC USES EAX ECX EDX EDI
    ;关闭光标发亮
    MOV pconsoleCursorInfo.bVisible, 0
    INVOKE SetConsoleCursorInfo, outputHandle, ADDR pconsoleCursorInfo
    CMP EAX, 0
    MOV errorMessage, OFFSET SetConsoleCursorInfoError
    JZ return

    ; 获取窗口大小
    INVOKE GetConsoleScreenBufferInfo, outputHandle, ADDR bufferInfo
    CMP EAX, 0
    MOV errorMessage, OFFSET GetConsoleScreenBufferInfoError
    JZ return

    ; 清屏
    INVOKE SetConsoleTextAttribute, outputHandle, 00FH
    MOV writePosition.X, 0
    MOV writePosition.Y, 0
    MOV ECX, 0
    MOV CX, bufferInfo.dwSdwMaximumWindowSizeize.Y
    clearConsole:
        PUSH ECX
        INVOKE SetConsoleCursorPosition, outputHandle, writePosition
        INVOKE WriteConsoleW, outputHandle, tempBuf, bufferInfo.dwSize.X, ADDR outputWriteLen, 0
        INC writePosition.Y
        POP ECX
        LOOP clearConsole

    ; 画出边框
    MOV ECX, 0
    MOV EDI, tempBuf
    padding:
        MOV WORD PTR [EDI], 020H
        ADD EDI, 2
        ADD ECX, 2
        CMP CX, bufferInfo.dwSdwMaximumWindowSizeize.X
        JBE padding
    MOV WORD PTR [EDI], 0
    ; 上边框
    INVOKE SetConsoleTextAttribute, outputHandle, 0CCH
    MOV writePosition.X, 0
    MOV writePosition.Y, 0
    INVOKE SetConsoleCursorPosition, outputHandle, writePosition
    INVOKE WriteConsoleW, outputHandle, tempBuf, bufferInfo.dwSdwMaximumWindowSizeize.X, ADDR outputWriteLen, 0
    ; 下边框
    MOV AX, bufferInfo.dwSdwMaximumWindowSizeize.Y
    DEC AX
    MOV writePosition.Y, AX
    INVOKE SetConsoleCursorPosition, outputHandle, writePosition
    INVOKE WriteConsoleW, outputHandle, tempBuf, bufferInfo.dwSdwMaximumWindowSizeize.X, ADDR outputWriteLen, 0
    ; 中间区域
    MOV ECX, 1
    background_draw:
        ; 左边框
        MOV writePosition.X, 0
        MOV writePosition.Y, CX
        PUSH ECX
        INVOKE SetConsoleTextAttribute, outputHandle, 0CCH
        INVOKE SetConsoleCursorPosition, outputHandle, writePosition
        INVOKE WriteConsoleW, outputHandle, tempBuf, 1, ADDR outputWriteLen, 0
        ; 中间
        INVOKE SetConsoleTextAttribute, outputHandle, 000H
        MOV writePosition.X, 1
        INVOKE SetConsoleCursorPosition, outputHandle, writePosition
        MOV AX, bufferInfo.dwSdwMaximumWindowSizeize.X
        SUB AX, 2
        INVOKE WriteConsoleW, outputHandle, tempBuf, AX, ADDR outputWriteLen, 0
        ; 右边框
        INVOKE SetConsoleTextAttribute, outputHandle, 0CCH
        MOV AX, bufferInfo.dwSdwMaximumWindowSizeize.X
        DEC AX
        MOV writePosition.X, AX
        INVOKE SetConsoleCursorPosition, outputHandle, writePosition
        INVOKE WriteConsoleW, outputHandle, tempBuf, 1, ADDR outputWriteLen, 0
        ; 边框绘制完成
        POP ECX
        INC CX
        MOV AX, bufferInfo.dwSdwMaximumWindowSizeize.Y
        DEC AX
        CMP CX, AX
        JB background_draw
    ; 清空信息栏
    INVOKE SetConsoleTextAttribute, outputHandle, 000H
    MOV writePosition.X, 0
    MOV AX, bufferInfo.dwSdwMaximumWindowSizeize.Y
    MOV writePosition.Y, AX
    INVOKE SetConsoleCursorPosition, outputHandle, writePosition
    INVOKE WriteConsoleW, outputHandle, tempBuf, bufferInfo.dwSdwMaximumWindowSizeize.X, ADDR outputWriteLen, 0

    ; 设置提示栏
    ; 获取屏幕的中间地址
    MOV DX, 0
    MOV AX, bufferInfo.dwSdwMaximumWindowSizeize.Y
    MOV CX, 2
    DIV CX
    MOV yMiddle, AX
    MOV DX, 0
    MOV AX, bufferInfo.dwSdwMaximumWindowSizeize.X
    MOV CX, 2
    DIV CX
    MOV xMiddle, AX
    ; 提示消息1
    MOV AX, yMiddle
    MOV writePosition.Y, AX
    MOV AX, xMiddle
    SUB AX, HintLen
    MOV writePosition.X, AX
    INVOKE SetConsoleTextAttribute, outputHandle, 00CH
    INVOKE SetConsoleCursorPosition, outputHandle, writePosition
    INVOKE strLen, ADDR Hint
    INVOKE WriteConsoleW, outputHandle, ADDR Hint, EAX, ADDR outputWriteLen, 0
    ; 提示消息2
    MOV AX, yMiddle
    INC AX
    MOV writePosition.Y, AX
    MOV AX, xMiddle
    SUB AX, Player1HintLen
    MOV writePosition.X, AX
    INVOKE SetConsoleTextAttribute, outputHandle, 00FH
    INVOKE SetConsoleCursorPosition, outputHandle, writePosition
    INVOKE strLen, ADDR Player1Hint
    INVOKE WriteConsoleW, outputHandle, ADDR Player1Hint, EAX, ADDR outputWriteLen, 0
    ; 提示消息3
    MOV AX, yMiddle
    ADD AX, 2
    MOV writePosition.Y, AX
    MOV AX, xMiddle
    SUB AX, Player2HintLen
    MOV writePosition.X, AX
    INVOKE SetConsoleCursorPosition, outputHandle, writePosition
    INVOKE strLen, ADDR Player2Hint
    INVOKE WriteConsoleW, outputHandle, ADDR Player2Hint, EAX, ADDR outputWriteLen, 0

    ; 设置信息栏
    MOV AX, bufferInfo.dwSdwMaximumWindowSizeize.Y
    MOV writePosition.Y, AX
    ; info1
    MOV writePosition.X, 0
    INVOKE SetConsoleCursorPosition, outputHandle, writePosition
    INVOKE strLen, ADDR Info1
    MOV info1Index, AX
    ADD info1Index, 2
    INVOKE WriteConsoleW, outputHandle, ADDR Info1, EAX, ADDR outputWriteLen, 0
    ; info2
    MOV AX, xMiddle
    MOV info2Index, AX
    SUB AX, Info2Len
    SUB AX, 3
    MOV writePosition.X, AX
    INVOKE SetConsoleCursorPosition, outputHandle, writePosition
    INVOKE strLen, ADDR Info2
    INVOKE WriteConsoleW, outputHandle, ADDR Info2, EAX, ADDR outputWriteLen, 0
    ; info3
    MOV AX, bufferInfo.dwSdwMaximumWindowSizeize.X
    SUB AX, 10
    MOV info3Index, AX
    SUB AX, Info3Len
    DEC AX
    MOV writePosition.X, AX
    INVOKE SetConsoleCursorPosition, outputHandle, writePosition
    INVOKE strLen, ADDR Info3
    INVOKE WriteConsoleW, outputHandle, ADDR Info3, EAX, ADDR outputWriteLen, 0
    ; 打印信息栏数据
    MOV AX, info1Index
    MOV writePosition.X, AX
    INVOKE SetConsoleCursorPosition, outputHandle, writePosition
    INVOKE printNum, 0
    MOV AX, info2Index
    MOV writePosition.X, AX
    INVOKE SetConsoleCursorPosition, outputHandle, writePosition
    INVOKE printNum, 0
    MOV AX, info3Index
    MOV writePosition.X, AX
    INVOKE SetConsoleCursorPosition, outputHandle, writePosition
    INVOKE printNum, 0

    RET
    ; 异常退出
    return:
        INVOKE exit, errorMessage
        RET
init ENDP

start_game PROC
    ; 清屏
    INVOKE SetConsoleTextAttribute, outputHandle, 000H
    MOV ECX, 1
    background_draw:
        PUSH CX
        MOV writePosition.Y, CX
        MOV writePosition.X, 1
        INVOKE SetConsoleCursorPosition, outputHandle, writePosition
        MOV AX, bufferInfo.dwSdwMaximumWindowSizeize.X
        SUB AX, 2
        INVOKE WriteConsoleW, outputHandle, tempBuf, AX, ADDR outputWriteLen, 0
        POP CX
        INC CX
        MOV AX, bufferInfo.dwSdwMaximumWindowSizeize.Y
        DEC AX
        CMP CX, AX
        JB background_draw
    
    ; 分配堆
    MOV AX, bufferInfo.dwSdwMaximumWindowSizeize.X
    MOV BX, bufferInfo.dwSdwMaximumWindowSizeize.Y
    MUL BX
    SAL EDX, 16
    ADD DX, AX
    MOV windowsSize, EDX
    PUSH EDX
    INVOKE HeapAlloc, heap, 8, EDX
    MOV deadHeap, EAX
    POP EDX
    MOV EAX, EDX
    MOV EDX, 0
    MOV EBX, 4
    MUL EBX
    MOV EBX, EAX
    PUSH EBX
    INVOKE HeapAlloc, heap, 8, EBX
    MOV payler1Heap, EAX
    POP EBX
    INVOKE HeapAlloc, heap, 8, EBX
    MOV payler2Heap, EAX

    ; 设置初始死亡区域
    MOV EDI, deadHeap
    ; 上边框
    MOV EBP, 0
    MOV ECX, 0
    MOV CX, bufferInfo.dwSdwMaximumWindowSizeize.X
    padding_up:
        MOV BYTE PTR [EDI+EBP], 1
        INC EBP
        LOOP padding_up
    ; 下边框
    MOV AX, bufferInfo.dwSdwMaximumWindowSizeize.X
    MOV BX, bufferInfo.dwSdwMaximumWindowSizeize.Y
    SUB BX, 1
    MUL BX
    SAL EDX, 16
    ADD DX, AX
    MOV EBP, EDX
    MOV ECX, 0
    MOV CX, bufferInfo.dwSdwMaximumWindowSizeize.X
    padding_down:
        MOV BYTE PTR [EDI+EBP], 1
        INC EBP
        LOOP padding_down
    ; 中间边框
    MOV ECX, 1
    padding_center:
        PUSH ECX
        ; 左边框
        MOV EDX, 0
        MOV EAX, 0
        MOV AX, bufferInfo.dwSdwMaximumWindowSizeize.X
        MUL ECX
        MOV EBP, EAX
        MOV BYTE PTR [EDI+EBP], 1
        ; 右边框
        MOV EAX, 0
        MOV AX, bufferInfo.dwSdwMaximumWindowSizeize.X
        ADD EBP, EAX
        DEC EBP
        MOV BYTE PTR [EDI+EBP], 1
        ; 判断是否结束
        POP ECX
        INC CX
        MOV AX, bufferInfo.dwSdwMaximumWindowSizeize.Y
        SUB AX, 1
        CMP CX, AX
        JB padding_center

    ; 初始化第一个食物
    MOV AX, bufferInfo.dwSdwMaximumWindowSizeize.X
    SUB AX, 2
    MOV BX, bufferInfo.dwSdwMaximumWindowSizeize.Y
    SUB BX, 3
    MUL BX
    SAL EDX, 16
    ADD DX, AX
    MOV mainSize, EDX
    INVOKE GetTickCount
    MOV seed, EAX
    CALL addFood

    ; TEST
    ;INVOKE SetConsoleTextAttribute, outputHandle, 00FH
    ;MOV writePosition.X, 0
    ;MOV writePosition.Y, 29
    ;INVOKE SetConsoleCursorPosition, outputHandle, writePosition
    ;INVOKE printNum, seed

    INVOKE GetTickCount
    MOV start_time, EAX
    game:
        ; 判断是否需要退出
        CMP need_exit, 1
        JZ return

        ; 改变时间
        INVOKE GetTickCount
        SUB EAX, start_time
        MOV EDX, 0
        MOV EBX, 1000
        DIV EBX
        CMP EAX, game_time
        JZ skip_print_time
        MOV game_time, EAX
        INVOKE SetConsoleTextAttribute, outputHandle, 00FH
        MOV AX, bufferInfo.dwSdwMaximumWindowSizeize.Y
        MOV writePosition.Y, AX
        MOV AX, info2Index
        MOV writePosition.X, AX
        INVOKE SetConsoleCursorPosition, outputHandle, writePosition
        INVOKE printNum, game_time
        skip_print_time:

        ; 玩家1
        CMP palyer1Start, 0
        JZ skipPlayer1
        CMP palyer1Init, 1
        JZ skipPlayer1Init
        MOV palyer1Init, 1
        ; 玩家1初始化
        INVOKE SetConsoleTextAttribute, outputHandle, 0EFH
        MOV writePosition.X, 1
        MOV writePosition.Y, 1
        MOV payler1End, 4
        MOV EDI, payler1Heap
        MOV WORD PTR [EDI], 1
        MOV WORD PTR [EDI+2], 1
        INVOKE SetConsoleCursorPosition, outputHandle, writePosition
        INVOKE WriteConsoleW, outputHandle, ADDR food, 1, ADDR outputWriteLen, 0
        JMP skipPlayer1
        skipPlayer1Init:
        ; 玩家1行走
        MOV EBP, payler1End
        MOV EDI, payler1Heap
        MOV AX, WORD PTR [EDI+EBP-4]
        MOV BX, WORD PTR [EDI+EBP-2]
        CMP player1Direction, 1
        JNZ change2
        DEC BX
        JMP exit_change1
        change2:
        CMP player1Direction, 2
        JNZ change3
        DEC AX
        JMP exit_change1
        change3:
        CMP player1Direction, 3
        JNZ change4
        INC BX
        JMP exit_change1
        change4:
        CMP player1Direction, 4
        JNZ exit_change1
        INC AX
        exit_change1:
        MOV WORD PTR [EDI+EBP], AX
        MOV WORD PTR [EDI+EBP+2], BX
        ADD payler1End, 4
        MOV writePosition.X, AX
        MOV writePosition.Y, BX
        
        MOV DX, 0
        PUSH AX
        MOV AX, bufferInfo.dwSdwMaximumWindowSizeize.X
        MUL BX
        SHL EDX, 16
        ADD DX, AX
        MOV EBP, EDX
        MOV EAX, 0
        POP AX
        ADD EBP, EAX
        MOV EDI, deadHeap
        CMP BYTE PTR [EDI+EBP], 1
        JZ return
        MOV BYTE PTR[EDI+EBP], 1
        MOV EDI, payler1Heap

        INVOKE SetConsoleTextAttribute, outputHandle, 0EFH
        INVOKE SetConsoleCursorPosition, outputHandle, writePosition
        INVOKE WriteConsoleW, outputHandle, ADDR food, 1, ADDR outputWriteLen, 0

        MOV AX, writePosition.X
        CMP AX, foodIndex.X
        JNZ skipPalyer1Food
        MOV AX, writePosition.Y
        CMP AX, foodIndex.Y
        JNZ skipPalyer1Food
        INC player1Food
        MOV AX, info1Index
        MOV AX, info1Index
        MOV writePosition.X, AX
        MOV AX, bufferInfo.dwSdwMaximumWindowSizeize.Y
        MOV writePosition.Y, AX
        INVOKE SetConsoleCursorPosition, outputHandle, writePosition
        INVOKE SetConsoleTextAttribute, outputHandle, 00FH
        INVOKE printNum, player1Food
        MOV needAddFood, 1
        JMP skipPlayer1

        skipPalyer1Food:
        MOV EBP, payler1Index
        MOV AX, WORD PTR [EDI+EBP]
        MOV BX, WORD PTR [EDI+EBP+2]
        ADD payler1Index, 4
        MOV writePosition.X, AX
        MOV writePosition.Y, BX

        MOV DX, 0
        PUSH AX
        MOV AX, bufferInfo.dwSdwMaximumWindowSizeize.X
        MUL BX
        SHL EDX, 16
        ADD DX, AX
        MOV EBP, EDX
        MOV EAX, 0
        POP AX
        ADD EBP, EAX
        MOV EDI, deadHeap
        MOV BYTE PTR[EDI+EBP], 0
        MOV EDI, payler1Heap

        INVOKE SetConsoleTextAttribute, outputHandle, 00FH
        INVOKE SetConsoleCursorPosition, outputHandle, writePosition
        INVOKE WriteConsoleW, outputHandle, ADDR food, 1, ADDR outputWriteLen, 0
        skipPlayer1:

        ; 玩家2
        CMP palyer2Start, 0
        JZ skipPlayer2
        CMP palyer2Init, 1
        JZ skipPlayer2Init
        MOV palyer2Init, 1
        ; 玩家2初始化
        INVOKE SetConsoleTextAttribute, outputHandle, 09FH
        MOV EDI, payler2Heap
        MOV AX, bufferInfo.dwSdwMaximumWindowSizeize.X
        SUB AX, 2
        MOV writePosition.X, AX
        MOV WORD PTR [EDI], AX
        MOV AX, bufferInfo.dwSdwMaximumWindowSizeize.Y
        SUB AX, 2
        MOV writePosition.Y, AX
        MOV WORD PTR [EDI+2], AX
        MOV payler2End, 4
        INVOKE SetConsoleCursorPosition, outputHandle, writePosition
        INVOKE WriteConsoleW, outputHandle, ADDR food, 1, ADDR outputWriteLen, 0
        JMP skipPlayer2
        skipPlayer2Init:
        ; 玩家2行走
        MOV EBP, payler2End
        MOV EDI, payler2Heap
        MOV AX, WORD PTR [EDI+EBP-4]
        MOV BX, WORD PTR [EDI+EBP-2]
        CMP player2Direction, 5
        JNZ change6
        DEC BX
        JMP exit_change2
        change6:
        CMP player2Direction, 6
        JNZ change7
        DEC AX
        JMP exit_change2
        change7:
        CMP player2Direction, 7
        JNZ change8
        INC BX
        JMP exit_change2
        change8:
        CMP player2Direction, 8
        JNZ exit_change2
        INC AX
        exit_change2:
        MOV WORD PTR [EDI+EBP], AX
        MOV WORD PTR [EDI+EBP+2], BX
        ADD payler2End, 4
        MOV writePosition.X, AX
        MOV writePosition.Y, BX
        
        MOV DX, 0
        PUSH AX
        MOV AX, bufferInfo.dwSdwMaximumWindowSizeize.X
        MUL BX
        SHL EDX, 16
        ADD DX, AX
        MOV EBP, EDX
        MOV EAX, 0
        POP AX
        ADD EBP, EAX
        MOV EDI, deadHeap
        CMP BYTE PTR [EDI+EBP], 1
        JZ return
        MOV BYTE PTR[EDI+EBP], 1
        MOV EDI, payler2Heap

        INVOKE SetConsoleTextAttribute, outputHandle, 09FH
        INVOKE SetConsoleCursorPosition, outputHandle, writePosition
        INVOKE WriteConsoleW, outputHandle, ADDR food, 1, ADDR outputWriteLen, 0

        MOV AX, writePosition.X
        CMP AX, foodIndex.X
        JNZ skipPalyer2Food
        MOV AX, writePosition.Y
        CMP AX, foodIndex.Y
        JNZ skipPalyer2Food
        INC player2Food
        MOV AX, info3Index
        MOV AX, info3Index
        MOV writePosition.X, AX
        MOV AX, bufferInfo.dwSdwMaximumWindowSizeize.Y
        MOV writePosition.Y, AX
        INVOKE SetConsoleCursorPosition, outputHandle, writePosition
        INVOKE SetConsoleTextAttribute, outputHandle, 00FH
        INVOKE printNum, player2Food
        MOV needAddFood, 1
        JMP skipPlayer1

        skipPalyer2Food:
        MOV EBP, payler2Index
        MOV AX, WORD PTR [EDI+EBP]
        MOV BX, WORD PTR [EDI+EBP+2]
        ADD payler2Index, 4
        MOV writePosition.X, AX
        MOV writePosition.Y, BX

        MOV DX, 0
        PUSH AX
        MOV AX, bufferInfo.dwSdwMaximumWindowSizeize.X
        MUL BX
        SHL EDX, 16
        ADD DX, AX
        MOV EBP, EDX
        MOV EAX, 0
        POP AX
        ADD EBP, EAX
        MOV EDI, deadHeap
        MOV BYTE PTR[EDI+EBP], 0
        MOV EDI, payler2Heap

        INVOKE SetConsoleTextAttribute, outputHandle, 00FH
        INVOKE SetConsoleCursorPosition, outputHandle, writePosition
        INVOKE WriteConsoleW, outputHandle, ADDR food, 1, ADDR outputWriteLen, 0
        skipPlayer2:

        CMP needAddFood, 1
        JNZ contiuse
        CALL addFood
        MOV needAddFood, 0

        contiuse:
        INVOKE Sleep, 88
        JMP game

    return:
        CALL gameOverFun
        RET
start_game ENDP

addFood PROC
    start:
    MOV EAX, 01234567H
    MUL seed
    MOV seed, EAX
    MOV EAX, EDX
    MOV EDX, 0
    MOV EBX, windowsSize
    SUB BX, bufferInfo.dwSdwMaximumWindowSizeize.X
    SUB BX, bufferInfo.dwSdwMaximumWindowSizeize.X
    DIV EBX
    MOV EBP, 0
    MOV BP, bufferInfo.dwSdwMaximumWindowSizeize.X
    ADD EBP, EDX
    ; 判断位置是否被占用, 如果被占用则循环找, 自处最好优化成向下找到可用的空间
    MOV EDI, deadHeap
    CMP BYTE PTR [EDI+EBP], 1
    JZ start
    ; 计算食物位置
    MOV EAX, EBP
    MOV EDX, 0
    MOV EBX, 0
    MOV BX, bufferInfo.dwSdwMaximumWindowSizeize.X
    DIV EBX
    ; 打印食物
    MOV writePosition.X, DX
    MOV writePosition.Y, AX
    MOV foodIndex.X, DX
    MOV foodIndex.Y, AX
    INVOKE SetConsoleCursorPosition, outputHandle, writePosition
    INVOKE SetConsoleTextAttribute, outputHandle, 020H
    INVOKE WriteConsoleW, outputHandle, ADDR food, 1, ADDR outputWriteLen, 0
    RET
addFood ENDP

printNum PROC USES EAX EBX ECX EDX EDI ESI EBP, number:DWORD
    INVOKE numToStr, number, ADDR numBuf
    INVOKE WriteConsoleW, outputHandle, ADDR numBuf, 10, ADDR outputWriteLen, 0
    RET
printNum ENDP

numToStr PROC USES EAX EBX EDX EDI, number:DWORD, writeTo:PTR WORD
    MOV EDI, writeTo

    MOV EAX, number
    MOV EDX, 0
    MOV EBX, 1000000000
    DIV EBX
    ADD AX, 48
    MOV [EDI], AX
    ADD EDI, 2

    MOV EAX, EDX
    MOV EDX, 0
    MOV EBX, 100000000
    DIV EBX
    ADD AX, 48
    MOV [EDI], AX
    ADD EDI, 2

    MOV EAX, EDX
    MOV EDX, 0
    MOV EBX, 10000000
    DIV EBX
    ADD AX, 48
    MOV [EDI], AX
    ADD EDI, 2

    MOV EAX, EDX
    MOV EDX, 0
    MOV EBX, 1000000
    DIV EBX
    ADD AX, 48
    MOV [EDI], AX
    ADD EDI, 2

    MOV EAX, EDX
    MOV EDX, 0
    MOV EBX, 100000
    DIV EBX
    ADD AX, 48
    MOV [EDI], AX
    ADD EDI, 2

    MOV EAX, EDX
    MOV EDX, 0
    MOV EBX, 10000
    DIV EBX
    ADD AX, 48
    MOV [EDI], AX
    ADD EDI, 2

    MOV EAX, EDX
    MOV EDX, 0
    MOV EBX, 1000
    DIV EBX
    ADD AX, 48
    MOV [EDI], AX
    ADD EDI, 2

    MOV EAX, EDX
    MOV EDX, 0
    MOV EBX, 100
    DIV EBX
    ADD AX, 48
    MOV [EDI], AX
    ADD EDI, 2

    MOV EAX, EDX
    MOV EDX, 0
    MOV EBX, 10
    DIV EBX
    ADD AX, 48
    MOV [EDI], AX
    ADD EDI, 2

    ADD DX, 48
    MOV [EDI], DX
    ADD EDI, 2

    MOV WORD PTR [EDI], 0
    RET
numToStr ENDP

; 获取字符串长度
strLen PROC USES ESI, message:PTR WORD
    MOV EAX, 0
    MOV ESI, message
    start:
        CMP WORD PTR[ESI], 0
        JZ return
        INC EAX
        ADD ESI, 2
        JMP start
    return:
        RET
strLen ENDP

; 打印错误信息后退出程序
exit PROC, message:PTR WORD
    ; 打印错误信息
    CMP outputHandle, 0
    JZ return
    INVOKE SetConsoleTextAttribute, outputHandle, 00CH
    INVOKE strLen, message
    INVOKE WriteConsoleW, outputHandle, message, EAX, ADDR outputWriteLen, 0
    ; 打印错误代码
    INVOKE GetLastError
    INVOKE numToStr, EAX, tempBuf
    INVOKE strLen, tempBuf
    INVOKE WriteConsoleW, outputHandle, tempBuf, EAX, ADDR outputWriteLen, 0
    INVOKE SetConsoleTextAttribute, outputHandle, 007H

    ; 退出程序
    return:
        INVOKE exitGame, 1
exit ENDP

gameOverFun PROC
    ; 恢复光标颜色
    INVOKE SetConsoleTextAttribute, outputHandle, bufferInfo.wAttributes

    ;恢复光标发亮
    MOV pconsoleCursorInfo.bVisible, 1
    INVOKE SetConsoleCursorInfo, outputHandle, ADDR pconsoleCursorInfo
    CMP EAX, 0
    MOV errorMessage, OFFSET SetConsoleCursorInfoError
    JZ return

    ; 设置光标位置
    MOV AX, bufferInfo.dwSdwMaximumWindowSizeize.X
    MOV cursorPosition.X, AX
    MOV AX, bufferInfo.dwSdwMaximumWindowSizeize.Y
    MOV cursorPosition.Y, AX
    INVOKE SetConsoleCursorPosition, outputHandle, cursorPosition
    CMP EAX, 0
    MOV errorMessage, OFFSET SetConsoleCursorPositionError
    JZ return

    ; 正常退出
    INVOKE exitGame, 0

    ; 异常退出
    return:
        INVOKE exit, errorMessage
        INVOKE exitGame, 1
gameOverFun ENDP

exitGame PROC, exitCode:DWORD
    CMP outputHandle, 0
    JZ return
    INVOKE strLen, ADDR GameOverMessage
    INVOKE WriteConsoleW, outputHandle, ADDR GameOverMessage, EAX, ADDR outputWriteLen, 0
    INVOKE ReadConsoleInputW, inputHandle, ADDR inputRecord, 1, ADDR inputRecordLen
    return:
    INVOKE ExitProcess, exitCode
exitGame ENDP

END main