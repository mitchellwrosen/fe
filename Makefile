ghcid:
	ghcid --restart stack.yaml --restart package.yaml

fe:
	stack build && stack exec fe
