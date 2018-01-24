# idris-vdom

Virtual DOM in pure Idris. Only works with the JavaScript backend, of course.

The Idris test harness only works with the C backend. To run tests,
run `idris --build idris-vdom.ipkg`, open `vdom-test.html` in a browser that
allows local JavaScript, and check the JavaScript console.

## To do

- [x] Naïve renderer
- [x] Event listeners on nodes
- [x] Properties on nodes
- [ ] Event listener options
- [ ] Use diffing renderer that sets listeners only once per node
- [ ] Allow updating listeners when rendering
- [ ] Allow rendering on any root element, not just `<body>`
- [ ] Write some tests already
