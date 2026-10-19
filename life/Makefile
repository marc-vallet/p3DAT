
.PHONY: help build serve dbg-build dbg-repl clean

js_cabal=../bin/js-cabal
app=life

help:
	@echo "Targets:"
	@echo "  build      Compila l'aplicació a JavaScript i prepara el directori 'public'"
	@echo "  serve      Serveix el directori 'public' amb un servidor HTTP local"
	@echo "  dbg-build  Compila l'aplicació amb el compilador natiu (no JS), per depurar"
	@echo "  dbg-repl   Obre una sessió interactiva (GHCi) amb l'aplicació compilada, per depurar"
	@echo "  clean      Neteja els fitxers generats"

# Compila l'aplicació a JavaScript i prepara el directori public.
build:
	$(js_cabal) build $(app)
	rm -rf public
	cp -r static public
	$(eval app_bin=$(shell $(js_cabal) list-bin $(app)))
	cp -v $(app_bin).jsexe/all.js public/

# Serveix el directori public amb un servidor HTTP local.
serve:
	cabal run http-serve public

# Compila l'aplicació amb el compilador natiu (no JS), per depurar.
dbg-build:
	cabal build $(app)

# Obre una sessió interactiva (GHCi) amb l'aplicació carregada, per depurar.
dbg-repl:
	cabal repl $(app)

# Neteja els fitxers generats.
clean:
	rm -rf public

