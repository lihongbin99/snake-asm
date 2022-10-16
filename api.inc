COORD STRUCT
    X WORD 0
    Y WORD 0
COORD ENDS

SMALL_RECT STRUCT
    Left   WORD 0
    Top    WORD 0
    Right  WORD 0
    Bottom WORD 0
SMALL_RECT ENDS

CONSOLE_SCREEN_BUFFER_INFO STRUCT
    dwSize                    COORD <>; 缓冲区大小
    dwCursorPosition          COORD <>
    wAttributes               WORD  0
    srWindow                  DWORD 0; SMALL_RECT
    dwSdwMaximumWindowSizeize COORD <>; 窗口大小
CONSOLE_SCREEN_BUFFER_INFO ENDS

PCONSOLE_CURSOR_INFO STRUCT
    dwSize   DWORD 0; 光标大小
    bVisible BYTE  0; 是否可见
PCONSOLE_CURSOR_INFO ENDS

KEY_EVENT_RECORD STRUCT
    bKeyDown         BYTE 0
    wRepeatCount     WORD 0
    wVirtualKeyCode  WORD 0
    wVirtualScanCode WORD 0
    UNION uChar
        UnicodeChar WORD 0
        AsciiChar   BYTE 0
    ENDS
    dwControlKeyState DWORD 0
KEY_EVENT_RECORD ENDS

MOUSE_EVENT_RECORD STRUCT
    dwMousePosition   COORD <>
    dwButtonState     DWORD 0
    dwControlKeyState DWORD 0
    dwEventFlags      DWORD 0
MOUSE_EVENT_RECORD ENDS

WINDOW_BUFFER_SIZE_RECORD STRUCT
    dwSize COORD <>
WINDOW_BUFFER_SIZE_RECORD ENDS

MENU_EVENT_RECORD STRUCT
    dwCommandId DWORD 0
MENU_EVENT_RECORD ENDS

FOCUS_EVENT_RECORD STRUCT
    bSetFocus BYTE 0
FOCUS_EVENT_RECORD ENDS

INPUT_RECORD STRUCT
    EventType WORD 0
    UNION Event
        KeyEvent              KEY_EVENT_RECORD <>
        MouseEvent            MOUSE_EVENT_RECORD <>
        WindowBufferSizeEvent WINDOW_BUFFER_SIZE_RECORD <>
        MenuEvent             MENU_EVENT_RECORD <>
        FocusEvent            FOCUS_EVENT_RECORD <>
    ENDS
INPUT_RECORD ENDS

; 退出程序
; uExitCode: 退出状态码
ExitProcess PROTO, uExitCode:DWORD

; 获取最后的错误代码
GetLastError PROTO

; 获取输入输入流
; nStdHandle: 输入流(-10), 输出流(-11), 错误流(-12)
GetStdHandle PROTO, nStdHandle:DWORD

; 设置输出颜色
; hConsoleOutput: 输出流的StdHandle
; wAttributes: 颜色参数(0背景字体H), 黑色(0), 淡蓝(1), 淡绿(2), 湖蓝(3), 淡红(4), 紫色(5), 黄色(6), 白色(7), 灰色(8), 蓝色(9), 绿色(A), 但浅绿(B), 红色(C), 淡紫(D), 淡黄(E), 亮白(F)
SetConsoleTextAttribute PROTO, hConsoleOutput:DWORD, wAttributes:WORD

; 输出信息
; hConsoleOutput: 输出流的StdHandle
; lpBuffer: 写入文本的地址
; nNumberOfCharsToWrite: 写入数量
; lpNumberOfCharsWritten: 实际写入数量
; lpReserved: 0
WriteConsoleW PROTO, hConsoleOutput:DWORD, lpBuffer:PTR WORD, nNumberOfCharsToWrite:DWORD, lpNumberOfCharsWritten:PTR DOWRD, lpReserved:PTR BYTE

; 向指定位置写入信息
; hConsoleOutput: 输出流的StdHandle
; lpCharacter: 写入文本的地址
; nLength: 写入数量
; dwWriteCoord: 写入位置
; lpNumberOfCharsWritten: 实际写入数量
WriteConsoleOutputCharacterW PROTO, hConsoleOutput:DWORD, lpCharacter:PTR WORD, nLength:DWORD, dwWriteCoord:COORD, lpNumberOfCharsWritten:PTR DWORD

; 获取控制台缓冲区信息
; hConsoleOutput: 输出流的StdHandle
; lpConsoleScreenBufferInfo: 控制台缓冲区信息
GetConsoleScreenBufferInfo PROTO, hConsoleOutput:DWORD, lpConsoleScreenBufferInfo:PTR BYTE

; 设置光标位置
; hConsoleOutput: 输出流的StdHandle
; dwCursorPosition(COORD): 光标位置
SetConsoleCursorPosition PROTO, hConsoleOutput:DWORD, dwCursorPosition:COORD

; 获取光标属性
; hConsoleOutput: 输出流的StdHandle
; lpConsoleCursorInfo: 光标属性
GetConsoleCursorInfo PROTO, hConsoleOutput:DWORD, lpConsoleCursorInfo:PTR BYTE

;设置光标属性
; hConsoleOutput: 输出流的StdHandle
; lpConsoleCursorInfo: 光标属性
SetConsoleCursorInfo PROTO, hConsoleOutput:DWORD, lpConsoleCursorInfo:PTR BYTE

; 读取事件
; hConsoleInput: 输入流的StdHandle
; lpBuffer: 接收时间的数组
; nLength: 数组的长度
; lpNumberOfEventsRead: 返回事件的数量
ReadConsoleInputW PROTO, hConsoleInput:DWORD, lpBuffer:PTR BYTE, nLength:DWORD, lpNumberOfEventsRead:PTR DWORD

; 创建线程
; lpThreadAttributes
; dwStackSize
; lpStartAddress
; lpParameter
; dwCreationFlags
; lpThreadId
CreateThread PROTO, lpThreadAttributes:PTR BYTE, dwStackSize:DWORD, lpStartAddress:PTR BYTE, lpParameter:PTR BYTE, dwCreationFlags:DWORD, lpThreadId:PTR DWORD

; 睡眠
; dwMilliseconds: 睡眠的毫秒时间
Sleep PROTO, dwMilliseconds:DWORD

; 获取系统启动后经历的时间毫秒
GetTickCount PROTO

; 创建堆
; flOptions: 0
; dwInitialSize: 初始大小
; dwMaximumSize: 最大大小
HeapCreate PROTO, flOptions:DWORD, dwInitialSize:DWORD, dwMaximumSize:DWORD

; 分配堆内存
; hHeap: 创建堆时返回的句柄
; dwFlags: 初始化为0(8)
; dwBytes: 分配的大小
HeapAlloc PROTO, hHeap:DWORD, dwFlags:DWORD, dwBytes:DWORD
