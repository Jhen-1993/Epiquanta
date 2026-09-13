@echo off
cd /d "%~dp0"
set "BIOSTAT_R="
for /d %%D in ("%ProgramFiles%\R\R-4.5.*") do if exist "%%~D\bin\Rscript.exe" set "BIOSTAT_R=%%~D\bin\Rscript.exe"
if not defined BIOSTAT_R for /d %%D in ("%LOCALAPPDATA%\Programs\R\R-4.5.*") do if exist "%%~D\bin\Rscript.exe" set "BIOSTAT_R=%%~D\bin\Rscript.exe"
if not defined BIOSTAT_R for /d %%D in ("%ProgramFiles%\R\R-*") do if exist "%%~D\bin\Rscript.exe" set "BIOSTAT_R=%%~D\bin\Rscript.exe"
if not defined BIOSTAT_R for /d %%D in ("%LOCALAPPDATA%\Programs\R\R-*") do if exist "%%~D\bin\Rscript.exe" set "BIOSTAT_R=%%~D\bin\Rscript.exe"
if not defined BIOSTAT_R for /f "delims=" %%R in ('where Rscript.exe 2^>nul') do set "BIOSTAT_R=%%R"
if not defined BIOSTAT_R (
 echo Rscript.exe not found. Install Windows x64 R 4.5.x before starting Epiquanta.
 echo Open index.html for installation instructions.
 pause
 exit /b 1
)
if /i "%~1"=="--setup" (
 "%BIOSTAT_R%" --vanilla "%~dp0setup.R"
 pause
 exit /b
)
"%BIOSTAT_R%" --vanilla "%~dp0Start_BioStat.R" %*
if errorlevel 1 pause
