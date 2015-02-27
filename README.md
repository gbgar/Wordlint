wordlint 0.2.0.4: a plaintext redundancy linter written in Haskell

#Description


Wordlint locates matching pairs of words repeated within a user-defined
distance.  Text may be linted by distance between words (that is, by word
count), by line count, and/or by percentage of the total words in the file.
Multiple lint types may be specified at one time. The user may also choose
a minimum word length for matches.

Filters are available to remove punctuation, capitalization, and/or a
user-defined list of words from the list of potential matches.

Various modes exist for data output, which is machine-readable by default with
column-based formatting. Results may be sorted by alphabetically by word, by
position (line number), or by intervening distance between matches; and may be
used with a human-readable mode.  Additionally, an "error" mode may supersede
these options to provide output designed for easy integration with text
editors.

#Installation


Following haskell convention, run 

`cabal update && cabal install wordlint`

to install via Hackage.

To build locally, clone this repository, `cd` to it, and execute:

`cabal update && cabal install`

Afterward, ensure the binary `wordlint` is available in your system's
`$PATH`. A man page is also available and may be copied to the user's .cabal
directory: 

`cp man/man1/wordlint.1 ~/.cabal/share/man/man1/wordlint.1`


#Options

\-\-help

    Display condensed help and exit.

\-f, \-\-file *FILE*

    Specify an input file. If none is given, wordlint reads from stdin.

##Linting Options

\-w, \-\-words *INT*
    Specify maximum intervening distance between returned word-pairs
	measuring by word count. This may intersect with the --lines and
	--percent options, but is ignored if -a is provided. Default is 250.

\-l, \-\-distance *INT*
    Specify maximum intervening distance between returned word-pairs
	measuring by line count. This may intersect with the --words and
	--percent options, but is ignored if -a is provided. Default is 0 (off).

\-p, \-\-percent *DOUBLE*
    Specify maximum intervening distance between returned word-pairs
	measuring by percentage of words. This may intersect with the --words and
	--lines options, but is ignored if -a is provided. Default is 0 (off).

\-m ,\-\-matchlength *NUMBER*
    Specify minimum length of words to be matched, i.e. to reduce hits for "the".
	Default is 5.

##Filters

\-b, \-\-blacklist

    Specify a file containing a newline-separated list of words (no spaces) to
    filter from matches. Pairs well with --nopunct, which is applied before, but 
    activated prior to application of --nocaps filter. Thus, --nocaps will not
    interfere, for example, with proper names given in the blacklist.

\-\-nocaps

    Ignore capitalization when determining matches.

\-\-nopunct

    Ignore punctuation when determining matches.

##Output Options

\-a, \-\-all

    Return all matched pairs of words regardless of intervening distance. Deactivates -d parameter.

\-h, \-\-human

    Return human-readable output. Compatible with all sorting except for 
    `--show vim`, which will supersede `--human`.

\-s, \-\-sort *word|position|distance|vim*

    Sort word pairs alphabetically, by line number, or by intervening
    distance; or provides output designed for error checking in text
    editors---respective to the following options:

        - word
        - position (default)
        - distance
        - error

#Examples

  `wordlint --file file.txt`

Runs the default check: a word-based check on words of five or more characters.
The distance between each match is to be no more than 250
words. The results are in a machine-readable table format (i.e. for easy
use with `awk`, `sed`, and the like).

  `wordlint --lines 20 --matchlength 7 --file file.txt`

Finds matching strings consisting of seven characters or more and which
have an intervening distance of twenty lines or less. Returns
machine-readable format.

  `wordlint -w 100 -l 20 -m 7 -f file.txt`

Finds matching strings consisting of seven characters or more and which fall
within an intervening distance of *both* 100 words *and* twenty lines
or less. Returns machine-readable format.

  `cat file.txt | wordlint --percent 2.5 -a -s word -h`

Finds all matching, five-characters-or-longer strings within a 2.5%
distance of one-another within the file, and returns the output sorted
alphabetically and in "human-readable form.

  `wordlint -f file.txt -b dir/blacklist.txt --nopunct --nocaps -s error`

Finds matching strings consisting of 5 characters or more, and which
have had punctuation, a list of words, and all capitalization stripped
from the possible matches. Returns output designed for use in text
editors (i.e. Vim's 'erororformat' option).

#See Also

A Vim front-end to Wordlint, creatively named Wordlint.vim, is available
at https://github.com/gbgar/Wordlint.vim

A compiler or flycheck interface are available for Emacs:
https://github.com/gbgar/wordlint.el
https://github.com/gbgar/flycheck-wordlint.el
