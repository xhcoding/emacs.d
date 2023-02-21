# Start Emacs in daemon mode on system startup
# Michael Powe
# Usage:
# - put script in a local folder
# - open the Startup folder (`shell:StartUp` in the File Explorer)
# - create a new shortcut there and point it to the script.
# - "C:\Program Files\PowerShell\7\pwsh.exe" -ExecutionPolicy Bypass -NoProfile -File  C:\Users\micha\Dropbox\src\powershell\emacsclient-on-startup.ps1
# 20211106T013351

$env:SERVER_HOME="$env:APPDATA\.emacs.d"

# open log file
# overwrites each time server is started
# log file format:
# {micha} [4] --> cat .\StartupPSLog.txt
# 10:28:25.038 2021-11-08 Script started
# 10:28:28.127 2021-11-08 Script removed old server
# 10:28:28.137 2021-11-08 Starting server
# 10:28:31.548 2021-11-08 New emacs daemon has Process Id 7784
# 10:28:32.294 2021-11-08 Daemon started at 11/08/2021 10:28:28

Set-Content -Path $env:TEMP\StartupPSLog.txt -Value (([datetime]::Now.ToString("HH:mm:ss.fff yyyy-MM-dd")) + (" Script started"))

# Warning - PS is lousy at recursively deleting directories that contain subdirectories. Sometimes, it pukes.
# Might need error trapping here.
if(Test-Path $env:SERVER_HOME\server\){
    # server directory found, remove artefacts
	Remove-Item -Force -Recurse $env:SERVER_HOME\server\*.*
	Add-Content -Path $env:TEMP\StartupPSLog.txt -Value (([datetime]::Now.ToString("HH:mm:ss.fff yyyy-MM-dd")) + (" Script removed old server"))
} else {
    # new server, go
	Add-Content -Path $env:TEMP\StartupPSLog.txt -Value  (([datetime]::Now.ToString("HH:mm:ss.fff yyyy-MM-dd")) + (" Old server not found "))
}

Add-Content -Path $env:TEMP\StartupPSLog.txt -Value (([datetime]::Now.ToString("HH:mm:ss.fff yyyy-MM-dd")) + (" Starting server"))
#
# Do it
#
runemacs --daemon

# make a space for the service to start before looking for it below. Otherwise, it may not have come up yet.
Start-Sleep -Seconds 3

Add-Content -Path $env:TEMP\StartupPSLog.txt -Value (([datetime]::Now.ToString("HH:mm:ss.fff yyyy-MM-dd")) + (" New emacs daemon has Process Id " + ((get-process "emacs").Id)))
Add-Content -Path $env:TEMP\StartupPSLog.txt -Value (([datetime]::Now.ToString("HH:mm:ss.fff yyyy-MM-dd")) + (" Daemon started at " + ((get-process "emacs").StartTime)))
