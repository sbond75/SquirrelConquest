IF [%VisualStudioVersion%] == [] GOTO VcVars
GOTO SkipVcVars
:VcVars
call "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvarsall.bat" x64
:SkipVcVars


cd SDL
mkdir build_release
cd build_release
REM %comspec% /k ""C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\vcvarsall.bat"" amd64
REM %comspec% /k ""C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\vcvarsall.bat"" x86

REM https://stackoverflow.com/questions/3827567/how-to-get-the-path-of-the-batch-script-in-windows
SET mypath=%~dp0
set "mypath_backslashes=%mypath%"
REM https://stackoverflow.com/questions/23542453/change-backslash-to-forward-slash-in-windows-batch-file , https://stackoverflow.com/questions/13737370/cmake-error-invalid-escape-sequence-u
set "mypath=%mypath:\=/%"

cmake .. -G "Visual Studio 17 2022" -DCMAKE_BUILD_TYPE=Release "-DCMAKE_INSTALL_PREFIX=%mypath%/install_release" -DCMAKE_C_FLAGS_RELEASE=/Zi -DCMAKE_CXX_FLAGS_RELEASE=/Zi -DCMAKE_EXE_LINKER_FLAGS_RELEASE="/DEBUG" -DCMAKE_SHARED_LINKER_FLAGS_RELEASE="/DEBUG /OPT:REF" -DCMAKE_STATIC_LINKER_FLAGS="/DEBUG /OPT:REF"
cmake --build . --config Release

cmake --install . --config Release
REM copy pdb since cmake doesn't do this when installing for some reason
set "dest=%mypath_backslashes%\install_release\lib"
move "Release\*.pdb" "%dest%"
