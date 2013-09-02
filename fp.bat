@ECHO OFF

set ghc_package_path=U:\ComputerRooms\EWI\FP\ghc;
setx ghc_package_path U:\ComputerRooms\EWI\FP\ghc;
set path=%PATH%;C:\Program Files (x86)\Haskell Platform\2012.2.0.0\lib\extralibs\bin;U:\ComputerRooms\EWI\FP\bin
setx path "C:\Program Files (x86)\Haskell Platform\2012.2.0.0\lib\extralibs\bin;U:\ComputerRooms\EWI\FP\bin"

IF NOT EXIST %appdata%\ghc\ghci.conf (
  goto :make-ghci-conf
) ELSE (
  goto :make-ghci-conf-end
)

:make-ghci-conf
  md %appdata%\ghc
  echo :set -XNoImplicitPrelude >> %appdata%\ghc\ghci.conf
  echo :set -NoMonomorphismRestriction >> %appdata%\ghc\ghci.conf
:make-ghci-conf-end
