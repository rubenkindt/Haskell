in admin powershell
	Set-ExecutionPolicy Bypass -Scope Process
	
	Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))

	refreshenv

	choco install haskell-dev
	refreshenv

	cabal update
	cabal install hlint

to run:
	gchi
	:l file.hs

to check layout haskell
	hlint file.hs