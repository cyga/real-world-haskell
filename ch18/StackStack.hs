-- file: ch18/StackStack.hs
type Bar = ReaderT Bool Foo

barPut :: String -> Bar ()
barPut = lift . lift . put-- file: ch18/StackStack.hs
innerPut :: String -> Foo ()
innerPut = lift . put-- file: ch18/StackStack.hs
outerPut :: Int -> Foo ()
outerPut = put-- file: ch18/StackStack.hs
type Foo = StateT Int (State String)
