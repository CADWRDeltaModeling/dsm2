echo off

svnversion .. > ..\version.txt || echo Version script failed
echo This is the SVN version control number (or range) of this distribution. >> ..\version.txt

svnversion ..\bin > ..\bin\version.txt || echo Version script failed
echo This is the SVN version control number (or range) of this directory.>> ..\bin\version.txt

svnversion ..\scripts > ..\scripts\version.txt || echo Version script failed
echo This is the SVN version control number (or range) of this directory.>> ..\scripts\version.txt

