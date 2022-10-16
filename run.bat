"C:\Program Files (x86)\Microsoft Visual Studio\2019\Enterprise\VC\Tools\MSVC\14.29.30037\bin\Hostx86\x86\ml.exe" /c main.asm || goto error

"C:\Program Files (x86)\Microsoft Visual Studio\2019\Enterprise\VC\Tools\MSVC\14.29.30037\bin\Hostx86\x86\link.exe" /subsystem:console "C:\Program Files (x86)\Windows Kits\10\Lib\10.0.19041.0\um\x86\kernel32.Lib" main.obj || goto error

main.exe

echo %errorlevel%

pause
exit

:error
    echo "error"
    echo %errorlevel%
    pause