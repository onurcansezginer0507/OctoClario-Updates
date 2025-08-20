!macro customUnInstall
  ; Try to kill the exact R process we started (pid stored by the app)
  StrCpy $0 "$APPDATA\OctoClario\r-pid.txt"
  IfFileExists "$0" 0 +6
    FileOpen $1 "$0" r
    FileRead $1 $2
    FileClose $1
    ; $2 is the PID
    nsExec::ExecToStack 'taskkill /PID $2 /T /F'
    Pop $3

  ; Fallback: kill common R executables just in case
  nsExec::ExecToStack 'taskkill /F /IM Rscript.exe /T'
  Pop $4
  nsExec::ExecToStack 'taskkill /F /IM R.exe /T'
  Pop $5
!macroend
