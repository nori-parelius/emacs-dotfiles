@echo off

REM Get date time
for /f "tokens=1-4 delims=/ " %%a in ('date /t') do (set mydate=%%a-%%b-%%c)
for /f "tokens=1-2 delims=: " %%a in ('time /t') do (set mytime=%%a-%%b)

REM CompNotes first
cd "C:\Users\ELPAR\OneDrive - Forsvarets forskningsinstitutt\_Documents\Reference System\CompNotes"

git status --porcelain
IF NOT ERRORLEVEL 1 (
    echo Changes detected, staging changes...
    git add -A



    REM Commit changes with a message
    echo Commiting changes...
    git commit -m "Automated commit on %mydate% at %mytime%"
) ELSE (
    echo No changes to commit.
)

REM Push changes to the remote repository
echo Pushing changes...
git push origin master

REM Then .emacs.d
cd "C:\Users\ELPAR\AppData\Roaming\.emacs.d"
echo %cd%

git status --porcelain
IF NOT ERRORLEVEL 1 (
    echo Changes detected, staging changes...
    git add -A



    REM Commit changes with a message
    echo Commiting changes...
    git commit -m "Automated commit on %mydate% at %mytime%"
) ELSE (
    echo No changes to commit.
)

REM Push changes to the remote repository
echo Pushing changes...
git push origin master

REM Then Notes
cd "C:\Users\ELPAR\MyDocuments\Notes"
echo %cd%

git status --porcelain
IF NOT ERRORLEVEL 1 (
    echo Changes detected, staging changes...
    git add -A



    REM Commit changes with a message
    echo Commiting changes...
    git commit -m "Automated commit on %mydate% at %mytime%"
) ELSE (
    echo No changes to commit.
)

REM Push changes to the remote repository
echo Pushing changes...
git push origin master

REM can add 'pause' to wait