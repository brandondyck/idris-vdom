IDRIS=idris

ifeq ($(OS),Windows_NT)
	RM=del
	PATHSEP2=\\
else
	RM=rm
	PATHSEP2=/
endif

install: installmodules installdoc

installmodules:
	$(IDRIS) --install idris-vdom.ipkg

installdoc:
	$(IDRIS) --installdoc idris-vdom.ipkg

build: modules doc

modules:
	$(IDRIS) --build idris-vdom.ipkg

doc:
	$(IDRIS) --mkdoc idris-vdom.ipkg

test: modules
	$(IDRIS) --codegen javascript -p specdris -i src -o vdom-test.js test/Main.idr

example: modules
	$(IDRIS) --codegen javascript -i src -o example/Example.js example/Example.idr

clean:
	$(IDRIS) --clean idris-vdom.ipkg
	$(RM) example$(PATHSEP2)Example.ibc
	$(RM) test$(PATHSEP2)Main.ibc
	$(RM) vdom-test.js
	$(RM) example$(PATHSEP2)Example.js
