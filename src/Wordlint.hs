import Prelude hiding (Word)
import System.Console.CmdArgs
import Text.Wordlint.Args
import Text.Wordlint.Linters
import Text.Wordlint.Output

main :: IO ()
main = do
    -- Execute command line arguments, retrieve flags
    cargs <- cmdArgs cliargs
    checkIfHumanHeader cargs
    linter <- getLinter cargs
    let results = runAllLinters linter
    produceOutput linter results
