HelloWorld                      WORD 00048H, 00065H, 0006CH, 0006CH, 0006FH, 00020H, 00057H, 0006FH, 00072H, 0006CH, 00064H, 00021H, 0
GetInputStdHandleError          WORD 083B7H, 053D6H, 063A7H, 05236H, 053F0H, 08F93H, 05165H, 06D41H, 05931H, 08D25H, 0
GetConsoleCursorInfoError       WORD 083B7H, 053D6H, 05149H, 06807H, 04FE1H, 0606FH, 05931H, 08D25H, 0
SetConsoleCursorInfoError       WORD 08BBEH, 07F6EH, 05149H, 06807H, 04FE1H, 0606FH, 05931H, 08D25H, 0
GetConsoleScreenBufferInfoError WORD 083B7H, 053D6H, 063A7H, 05236H, 053F0H, 04FE1H, 0606FH, 05931H, 08D25H, 0
SetConsoleCursorPositionError   WORD 08BBEH, 07F6EH, 05149H, 06807H, 04F4DH, 07F6EH, 05F02H, 05E38H, 0
Hint                            WORD 06E38H, 0620FH, 05F00H, 059CBH, 0540EH, 07981H, 06B62H, 06539H, 053D8H, 07A97H, 053E3H, 05927H, 05C0FH, 00028H, 07A97H, 053E3H, 053EAH, 080FDH, 06162H, 06162H, 0653EH, 05927H, 00029H, 0
HintLen                         WORD ($-Hint)/2
Player1Hint                     WORD 073A9H, 05BB6H, 00031H, 0003AH, 00077H, 00061H, 00073H, 00064H, 0
Player1HintLen                  WORD ($-Player1Hint)/2
Player2Hint                     WORD 073A9H, 05BB6H, 00032H, 0003AH, 02191H, 02190H, 02193H, 02192H, 0
Player2HintLen                  WORD ($-Player2Hint)/2
Info1                           WORD 073A9H, 05BB6H, 00031H, 0003AH, 00020H, 0
Info1Len                        WORD ($-Info1)/2
Info2                           WORD 06E38H, 0620FH, 065F6H, 095F4H, 0003AH, 00020H, 0
Info2Len                        WORD ($-Info2)/2
Info3                           WORD 073A9H, 05BB6H, 00032H, 0003AH, 00020H, 0
Info3Len                        WORD ($-Info3)/2
GameOverMessage                 WORD 06E38H, 0620FH, 07ED3H, 0675FH, 00028H, 08F93H, 05165H, 04EFBH, 0610FH, 06309H, 0952EH, 09000H, 051FAH, 07A0BH, 05E8FH, 00029H, 0