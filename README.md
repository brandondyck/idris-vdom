# idris-vdom

Virtual DOM in pure Idris. Only works with the JavaScript backend, of course.

The Idris test harness only works with the C backend. To run tests,
run `make test`, open `vdom-test.html` in a browser that
allows local JavaScript, and check the JavaScript console.

## To do

- [x] Naïve renderer
- [x] Event listeners on nodes
- [x] Properties on nodes
- [x] Event listener options
- [x] Use diffing renderer that sets listeners only once per node
- [x] Allow rendering on any root element, not just `<body>`
- [x] Write some tests already
- [ ] Allow updating listeners when diffing
- [ ] Clean up the API
