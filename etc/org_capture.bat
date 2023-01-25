@echo off
set URL=%1
set URL=%URL:&=^&%
set URL=%URL:/?=?%
set URL=%URL:://=:///%
start "" "D:\Applications\Scoop\shims\emacsclientw.exe" "%URL%"
