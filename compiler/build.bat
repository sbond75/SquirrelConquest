cd %~dp0
cat game.code | cabal run
if %errorlevel% neq 0 exit /b %errorlevel%
cp -a game.ch8 ../src/Windows/Build/
cp -a game.ch8 ../html/GAMES/CUBE8.ch8
cp -a game.ch8 ../src/Game/
pushd .
cd ..\src\Windows\Build
SquirrelConquest.exe
popd
