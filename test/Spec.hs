import Happstack.Server
import Lib


main :: IO ()
main = simpleHTTP nullConf $ editFile "foo"