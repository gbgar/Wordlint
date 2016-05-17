-- Copyright © 2014-2016 Blake Gardner github.com/gbgar
-- This file is part of Wordlint.

-- Wordlint is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- Wordlint is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with Wordlint.  If not, see <http://www.gnu.org/licenses/>.

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