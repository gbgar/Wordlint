wordlint 0.1.0.0: a plaintext redundancy linter written in Haskell

#Description


Wordlint locates matching pairs of words repeated within a user-defined
distance. Text may be linted by distance between words (that is, by word
count), by line count, or by percentage of the total words in the file.
The user may also choose a minimum word length for matches.

Options exist for data output, which is machine-readable by default.
Results may be sorted by alphabetically by word, by position (line
number), or by intervening distance between matches; and may be used
with a human-readable mode. Additionally, a Vim mode provides output for
a plugin.

#Installation


Following haskell convention, run `cabal update && cabal install wordlint`
to install via Hackage.

To build locally, clone this repository, `cd` to it, and execute the same command as above.
Afterward, the binary `wordlint` must be copied to a directory in the
user's `$PATH`.

#Options


-f *FILE*
    Specify an input file. If none is given, wordlint reads from stdin.

#$Linting Options

-d *INT | FLOAT*

    Specify maximum intervening distance between returned word-pairs.
    **If the type of lint is either "word" or "line", an integer must be
    used**, while a "percentage" check will accept a float value.
    Ignored if -a is used. Default is 250.

-t *word|line|percentage*

    Specify type of lint to perform, which affects which the calculation
    of intervening distance between word pairs. Options are:

        - word (default)
        - line
        - percentage

    A word-type check will define a word's "position" as it's word
    count, while a line check uses the line number on which a word is
    found. A percentage check sets this value according to a word's
    count divided by the total count of words in the input.

-w *NUMBER*

    Specify minimum length of words to be matched, i.e. to reduce hits
    for "there". Default is 5.

##Output Options


-a

    Return all matched pairs of words regardless of intervening
    distance. Deactivates -d parameter.

-h

    Return human-readable output. Compatible with all sorting modes except
    "vim".

-s *word|position|distance|vim*

    Sort word pairs alphabetically, by line number, or by intervening
    distance; or provides Vim plugin output respective to the following
    options:

        - word
        - position (default)
        - distance
        - vim

#Examples


    wordlint -t line -d 20 -w 7 -f file.txt

Finds matching strings consisting of seven characters or more and which
have an intervening distance of twenty or less.

    cat file.txt | wordlint -t percentage -d 2.5 -a -h -s word

Finds all matching, five-character-or-more strings found within a 2.5%
distance of one-another, and returns the output in human-readable form,
sorted alphabetically.

#See Also


A Vim front-end to Wordlint, creatively named Wordlint.vim, is available
at https://github.com/gbgar/Wordlint.vim

