import System.Console.CmdArgs
import Wordlint.Args
import Wordlint.Linters
import Wordlint.Output

main :: IO ()
main = do
    -- Execute command line arguments, retrieve flags
    cargs <- cmdArgs cliargs
    checkIfHumanHeader cargs
    linter <- getLinter cargs
    let results = runAllLinters linter
    produceOutput linter results
