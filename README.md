# JsonGenerators
Elm Json Encoder and Decoder generation from types


# Test
Test can be generated semi automatic the node module `TestGenerator.js` loop over every file in `tests/TestInputs/` and generate decoders and encoder for them. These are placed in `tests/GeneratedTests`.


In UnitTest.elm these tests for different generated modules (modules in `tests/GeneratedTests` can be made. Most of them are on test that check that `input == decode encode input`

The types of test are Basic records, Custom types, AnonymousRecord ect.


# TODO
Implement support for

- [ ] Array
- [ ] Char
- [x] Dict
- [x] Maybe
- [ ] Result
- [x] Set
