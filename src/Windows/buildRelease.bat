rem Check if %1 was provided at all
if "%~1"=="" (
    echo No argument supplied.
    goto :eof
)

rem Now check if %1 equals 0
if "%~1"=="0" (
    echo The first argument is 0.
) else (
    echo The first argument is NOT 0. It is "%~1".
)

IF [%VisualStudioVersion%] == [] GOTO VcVars
GOTO SkipVcVars
:VcVars
REM call "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\vcvarsall.bat" x86
call "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvarsall.bat" x86
:SkipVcVars

cd /d %~dp0

if "%~1"=="0" (
    echo Not building.
) else (
    devenv SquirrelConquest.sln /Build "Release|x64" /Project "SquirrelConquest"
)

REM Exit if error
IF ERRORLEVEL 1 EXIT /B %ERRORLEVEL%

REM https://stackoverflow.com/questions/46836546/store-git-id-in-variable-in-batch-script
for /f %%i in ('git.exe rev-parse HEAD') do set COMMIT_ID=%%i

mkdir Build
cd Build
cp -a ../../../src/Game/game.ch8 ../../../lib/install_release/bin/SDL2.dll ../../../src/Windows/SquirrelConquest/x64/Release/SquirrelConquest.exe ./
7z a SquirrelConquest_%COMMIT_ID%.zip SquirrelConquest.exe *.dll *.ch8
