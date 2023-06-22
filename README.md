# rbgrep

![test](https://github.com/harehare/rbgrep/actions/workflows/test.yml/badge.svg)
[![MIT License](http://img.shields.io/badge/license-MIT-blue.svg?style=flat)](LICENSE)

rbgrep is a AST-based line-oriented search cli tool that recursively searches **ruby files** in the current directory for a regex patterns.

### Screenshot of search results

![A screenshot of a sample search with rbgrep](assets/rbgrep.png)

## Install

```bash
cargo install --git https://github.com/harehare/rbgrep.git
```

### Manually

```
git clone https://github.com/harehare/rbgrep.git
cd rbgrep
cargo run
```

## Usage

```bash
Usage: rbgrep [OPTIONS] <QUERY> [STDIN]

Arguments:
  <QUERY>
  [STDIN]

Options:
  -c, --count
          Only print the count of individual matches for each file
  -i, --ignore-case
          Case insensitive search
  -m, --exact-match
          Exact match Search
  -., --hidden
          Search hidden files and directory
  -e, --regexp
          Regex pattern search
      --exclude <EXCLUDE>
          If specified, it excludes files or directories matching the given filename pattern from the search
  -S, --start-pattern <START_PATTERN>
          AST start pattern to match [possible values: alias, and, and-asgn, arg, args, array, array-pattern, array-pattern-with-tail, back-ref, begin, block, block-pass, blockarg, break, c-send, case, case-match, casgn, cbase, class, complex, const, const-pattern, cvar, cvasgn, def, defined, defs, dstr, dsym, e-flip-flop, empty-else, encoding, ensure, erange, false, file, find-pattern, float, for, forward-arg, forwarded-args, gvar, gvasgn, hash, hash-pattern, heredoc, i-flip-flop, if, if-guard, if-mod, if-ternary, in-pattern, index, index-asgn, int, irange, ivar, ivasgn, kw-begin, kwarg, kwargs, kwnilarg, kwoptarg, kwrestarg, kwsplat, lambda, line, lvar, lvasgn, masgn, match-alt, match-as, match-current-line, match-nil-pattern, match-pattern, match-pattern-p, match-rest, match-var, match-with-lvasgn, mlhs, module, next, nil, nth-ref, numblock, op-asgn, optarg, or, or-asgn, pair, pin, postexe, preexe, procarg0, rational, redo, reg-opt, regexp, rescue, rescue-body, restarg, retry, return, s-class, self, send, shadowarg, splat, str, super, sym, true, undef, unless-guard, until, until-post, when, while, while-post, x-heredoc, xstr, yield, z-super]
  -E, --end-pattern <END_PATTERN>
          AST end pattern to match [possible values: alias, and, and-asgn, arg, args, array, array-pattern, array-pattern-with-tail, back-ref, begin, block, block-pass, blockarg, break, c-send, case, case-match, casgn, cbase, class, complex, const, const-pattern, cvar, cvasgn, def, defined, defs, dstr, dsym, e-flip-flop, empty-else, encoding, ensure, erange, false, file, find-pattern, float, for, forward-arg, forwarded-args, gvar, gvasgn, hash, hash-pattern, heredoc, i-flip-flop, if, if-guard, if-mod, if-ternary, in-pattern, index, index-asgn, int, irange, ivar, ivasgn, kw-begin, kwarg, kwargs, kwnilarg, kwoptarg, kwrestarg, kwsplat, lambda, line, lvar, lvasgn, masgn, match-alt, match-as, match-current-line, match-nil-pattern, match-pattern, match-pattern-p, match-rest, match-var, match-with-lvasgn, mlhs, module, next, nil, nth-ref, numblock, op-asgn, optarg, or, or-asgn, pair, pin, postexe, preexe, procarg0, rational, redo, reg-opt, regexp, rescue, rescue-body, restarg, retry, return, s-class, self, send, shadowarg, splat, str, super, sym, true, undef, unless-guard, until, until-post, when, while, while-post, x-heredoc, xstr, yield, z-super]
  -P, --pattern <PATTERN>
          AST pattern to match [possible values: alias, and, and-asgn, arg, args, array, array-pattern, array-pattern-with-tail, back-ref, begin, block, block-pass, blockarg, break, c-send, case, case-match, casgn, cbase, class, complex, const, const-pattern, cvar, cvasgn, def, defined, defs, dstr, dsym, e-flip-flop, empty-else, encoding, ensure, erange, false, file, find-pattern, float, for, forward-arg, forwarded-args, gvar, gvasgn, hash, hash-pattern, heredoc, i-flip-flop, if, if-guard, if-mod, if-ternary, in-pattern, index, index-asgn, int, irange, ivar, ivasgn, kw-begin, kwarg, kwargs, kwnilarg, kwoptarg, kwrestarg, kwsplat, lambda, line, lvar, lvasgn, masgn, match-alt, match-as, match-current-line, match-nil-pattern, match-pattern, match-pattern-p, match-rest, match-var, match-with-lvasgn, mlhs, module, next, nil, nth-ref, numblock, op-asgn, optarg, or, or-asgn, pair, pin, postexe, preexe, procarg0, rational, redo, reg-opt, regexp, rescue, rescue-body, restarg, retry, return, s-class, self, send, shadowarg, splat, str, super, sym, true, undef, unless-guard, until, until-post, when, while, while-post, x-heredoc, xstr, yield, z-super]
      --no-git-ignore
          Don't respect .gitignore files
  -N, --no-file-name
          Never print the file path with the matched lines
      --no-line-no
          Never print the line number with the matched lines
      --no-color
          Not colored the output results
  -o, --only-matching
          Print only matched parts of a line
  -C, --context <CONTEXT>
          Show lines before and after each match
  -A, --after-context <AFTER_CONTEXT>
          Show lines before each match
  -B, --before-context <BEFORE_CONTEXT>
          Show lines before each match
      --context-separator <CONTEXT_SEPARATOR>
          The string used to separate
      --with-nodes
          Print nodes with the matched lines
      --with-warning
          Print warning after parse of ruby file
      --threads <THREADS>
          Number of grep worker threads to use
      --max-depth <MAX_DEPTH>
          The maximum depth to recurse
      --json
          Show search results in a JSON format
      --csv
          Show search results in a CSV format
      --csv-delimiter <CSV_DELIMITER>
          Specify the CSV delimiter
  -q, --quiet
          Do not output matched lines. instead, exit with status 0 when there is a match and with non-zero status when there isn’t
  -p, --path <PATH>
          Searches for specified files and directories
  -h, --help
          Print help
  -V, --version
          Print version
```

## Command line usage example

### Example

```rb
$ rbgrep -S begin -E hash -E pair -E sym title
./example.rb:7     article = {title: title, content: content, published_on: Time.now}
```

```rb
$ rbgrep -S begin -E index -E sym title
./example.rb:14       puts "Title: #{article[:title]}"
```

```rb
$ cat example.rb | rbgrep -S begin -E index -E sym title -
:14       puts "Title: #{article[:title]}"
```

```rb
$ rbgrep -S begin -E index -E sym -p example.rb -p example2.rb title
:14       puts "Title: #{article[:title]}"
```

## License

[MIT](http://opensource.org/licenses/MIT)
