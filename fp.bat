@ECHO OFF

IF NOT EXIST P:\haskell\ghc\package.cache (
  goto :install-fp
) ELSE (
  goto :install-fp-end
)

:install-fp
  md P:\haskell
  md P:\haskell\bin
  md P:\haskell\doc
  md P:\haskell\lib
  ghc-pkg init P:\haskell\ghc
  set PATH=%PATH%;C:\Program Files (x86)\Haskell Platform\2012.2.0.0\lib\extralibs\bin;P:\haskell\bin
  cabal update
  cabal install hscolour --enable-documentation --prefix=P:\haskell\lib --docdir=P:\haskell\doc --bindir=P:\haskell\bin --doc-index-file=P:\haskell\doc\index.html
  cabal install gloss --flags="-GLUT GLFW" --enable-documentation --haddock-hyperlink-source --prefix=P:\haskell\lib --docdir=P:\haskell\doc --bindir=P:\haskell\bin --doc-index-file=P:\haskell\doc\index.html
  cabal install twentefp --enable-documentation --haddock-hyperlink-source --prefix=P:\haskell\lib --docdir=P:\haskell\doc --doc-index-file=P:\haskell\doc\index.html
  xcopy %appdata%\ghc\i386-mingw32-7.4.1\package.conf.d P:\haskell\ghc /y
  ghc-pkg recache --package-conf=P:\haskell\ghc
:install-fp-end

:get-glut
  IF NOT EXIST P:\haskell\bin\glut32.dll (
    goto :download-glut
  ) ELSE (
    goto :get-glut-end
  )
:download-glut
  bitsadmin /reset
  IF NOT EXIST P:\haskell\bin\glut-3.7.6-bin.zip (
    goto :download-glut-start
  ) ELSE (
    goto :download-glut-end
  )
:download-glut-start
  bitsadmin /create /DOWNLOAD DownLoadGLUT32DLL
  bitsadmin /setpriority DownLoadGLUT32DLL high
  bitsadmin /addfile DownLoadGLUT32DLL http://user.xmission.com/~nate/glut/glut-3.7.6-bin.zip p:\haskell\bin\glut-3.7.6-bin.zip
  bitsadmin /resume DownLoadGLUT32DLL
  ping 1.1.1.1 -n 1 -w 5000 >NUL
  bitsadmin /resume DownLoadGLUT32DLL
  ping 1.1.1.1 -n 1 -w 5000 >NUL
  bitsadmin /complete DownLoadGLUT32DLL
  goto :download-glut
:download-glut-end
  cd /d P:\haskell\bin
  "C:\Program Files (x86)\7-Zip\7z.exe" x P:\haskell\bin\glut-3.7.6-bin.zip
  xcopy P:\haskell\bin\glut-3.7.6-bin\glut32.dll P:\haskell\bin\ /y
  cd /d P:\
  del /q P:\haskell\bin\glut-3.7.6-bin.zip
  rmdir /s /q P:\haskell\bin\glut-3.7.6-bin
:get-glut-end

set ghc_package_path=P:\haskell\ghc;
setx ghc_package_path P:\haskell\ghc;
set path=%PATH%;P:\haskell\bin
setx path P:\haskell\bin
md %appdata%/ghc
echo :set -XNoImplicitPrelude >> %appdata%/ghc/ghci.conf
echo :set -NoMonomorphismRestriction >> %appdata%/ghc/ghci.conf
