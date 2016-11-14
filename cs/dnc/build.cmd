@call powershell -ExecutionPolicy RemoteSigned -NoLogo -NoProfile -File "%~dp0\build.ps1" %*
@if %errorlevel% neq 0 exit /b %errorlevel%
