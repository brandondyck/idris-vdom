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

# I would prefer not to require installation, but I haven't found a better way.
test: installmodules
	$(IDRIS) --codegen javascript -p idris-vdom -p specdris -o vdom-test.js src/VirtualDOM/Test/Main.idr

example: installmodules
	$(IDRIS) --codegen javascript -p idris-vdom -o example/Example.js example/Example.idr

clean:
	$(IDRIS) --clean idris-vdom.ipkg
	$(RM) vdom-test.js
	$(RM) example$(PATHSEP2)Example.js
