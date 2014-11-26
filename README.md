wordlint 0.1.0.0: a plaintext redundancy linter written in Haskell

#Description


Wordlint locates matching pairs of words repeated within a user-defined
distance. Text may be linted by distance between words (that is, by word
count), by line count, or by percentage of the total words in the file. The
user may also choose a minimum word length for matches.

Filters are available to remove punctuation, capitalization, and/or a
user-defined list of words from the list of potential matches.

Various modes exist for data output, which is machine-readable by default.
Results may be sorted by alphabetically by word, by position (line number), or
by intervening distance between matches; and may be used with a human-readable
mode. Additionally, a Vim mode provides output for a plugin.

#Installation


Following haskell convention, run 

`cabal update && cabal install wordlint`

to install via Hackage.

To build locally, clone this repository, `cd` to it, and execute the same
command as above.  Afterward, the binary `wordlint` must be copied to a
directory in the user's `$PATH`. A man page is also available and may be copied
to the user's .cabal directory: 

`cp man/man1/wordlint.1 ~/.cabal/share/man/man1/wordlint.1`


#Options

\-\-help

    Display condensed help and exit.

\-f, \-\-file *FILE*

    Specify an input file. If none is given, wordlint reads from stdin.

##Linting Options

\-d, \-\-distance *INT | FLOAT*

    Specify maximum intervening distance between returned word-pairs. **If the
    type of lint is either "word" or "line", an integer must be used**, while a
    "percentage" check will accept a float value. Ignored if -a is used. Default is
    250.

\-t, \-\-type *word|line|percentage*

    Specify type of lint to perform, which affects which the calculation of
    intervening distance between word pairs. Options are:

        - word (default)
        - line
        - percentage

    A word-type check will define a word's "position" as it's word count, while a
    line check uses the line number on which a word is found. A percentage check
    sets this value according to a word's count divided by the total count of words
    in the input.

\-w ,\-\-wordlength *NUMBER*

    Specify minimum length of words to be matched, i.e. to reduce hits for
    "there". Default is 5.

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

   Sort word pairs alphabetically, by line number, or by intervening distance;
   or provides Vim plugin output respective to the following options:

        - word
        - position (default)
        - distance
        - vim 

#Examples

  `wordlint --file file.txt`

Runs the default check: a word-based check on words of five or more characters.
The distance between each match is to be no more than 250 words. The results
are in a machine-readable table format.

  `wordlint --type line --distance 20 --wordlength 7 --file file.txt`

Finds matching strings consisting of seven characters or more and which have an
intervening distance of twenty lines or less. 

  `cat file.txt | wordlint -t percentage -d 2.5 -a -s word -h`

Finds all matching, five-characters-or-longer strings within a 2.5% distance of
one-another within the file, and returns the output sorted alphabetically and
in human-readable form.

  `wordlint -f file.txt -b dir/blacklist.txt --nopunct --nocaps`

Finds matching strings consisting of 5 characters or more, and which have had
punctuation, a list of words, and all capitalization stripped from the possible
matches.

#See Also

A Vim front-end to Wordlint, creatively named Wordlint.vim, is available
at https://github.com/gbgar/Wordlint.vim

