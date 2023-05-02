# rbgrep

![test](https://github.com/harehare/rbgrep/actions/workflows/test.yml/badge.svg)
[![MIT License](http://img.shields.io/badge/license-MIT-blue.svg?style=flat)](LICENSE)

rbgrep is a line-oriented search cli tool that recursively searches **ruby files** in the current directory for a regex patterns.

### Screenshot of search results

![A screenshot of a sample search with rbgrep](assets/rbgrep.jpg)

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
Usage: rbgrep [OPTIONS] <QUERY> [PATH]...

Arguments:
  <QUERY>
  [PATH]...

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
  -I, --include-node <INCLUDE_NODE>
          Include node type [possible values: alias, and, and-asgn, arg, args, array, array-pattern, array-pattern-with-tail, back-ref, begin, block, block-pass, blockarg, break, c-send, case, case-match, casgn, cbase, class, complex, const, const-pattern, cvar, cvasgn, def, defined, defs, dstr, dsym, e-flip-flop, empty-else, encoding, ensure, erange, false, file, find-pattern, float, for, forward-arg, forwarded-args, gvar, gvasgn, hash, hash-pattern, heredoc, i-flip-flop, if, if-guard, if-mod, if-ternary, in-pattern, index, index-asgn, int, irange, ivar, ivasgn, kw-begin, kwarg, kwargs, kwnilarg, kwoptarg, kwrestarg, kwsplat, lambda, line, lvar, lvasgn, masgn, match-alt, match-as, match-current-line, match-nil-pattern, match-pattern, match-pattern-p, match-rest, match-var, match-with-lvasgn, mlhs, module, next, nil, nth-ref, numblock, op-asgn, optarg, or, or-asgn, pair, pin, postexe, preexe, procarg0, rational, redo, reg-opt, regexp, rescue, rescue-body, restarg, retry, return, s-class, self, send, shadowarg, splat, str, super, sym, true, undef, unless-guard, until, until-post, when, while, while-post, x-heredoc, xstr, yield, z-super]
  -E, --exclude-node <EXCLUDE_NODE>
          Exclude node type [possible values: alias, and, and-asgn, arg, args, array, array-pattern, array-pattern-with-tail, back-ref, begin, block, block-pass, blockarg, break, c-send, case, case-match, casgn, cbase, class, complex, const, const-pattern, cvar, cvasgn, def, defined, defs, dstr, dsym, e-flip-flop, empty-else, encoding, ensure, erange, false, file, find-pattern, float, for, forward-arg, forwarded-args, gvar, gvasgn, hash, hash-pattern, heredoc, i-flip-flop, if, if-guard, if-mod, if-ternary, in-pat
  -S, --start-nodes <START_NODES>
          Start nodes [possible values: alias, and, and-asgn, arg, args, array, array-pattern, array-pattern-with-tail, back-ref, begin, block, block-pass, blockarg, break, c-send, case, case-match, casgn, cbase, class, complex, const, const-pattern, cvar, cvasgn, def, defined, defs, dstr, dsym, e-flip-flop, empty-else, encoding, ensure, erange, false, file, find-pattern, float, for, forward-arg, forwarded-args, gvar, gvasgn, hash, hash-pattern, heredoc, i-flip-flop, if, if-guard, if-mod, if-ternary, in-pattern, index, index-asgn, int, irange, ivar, ivasgn, kw-begin, kwarg, kwargs, kwnilarg, kwoptarg, kwrestarg, kwsplat, lambda, line, lvar, lvasgn, masgn, match-alt, match-as, match-current-line, match-nil-pattern, match-pattern, match-pattern-p, match-rest, match-var, match-with-lvasgn, mlhs, module, next, nil, nth-ref, numblock, op-asgn, optarg, or, or-asgn, pair, pin, postexe, preexe, procarg0, rational, redo, reg-opt, regexp, rescue, rescue-body, restarg, retry, return, s-class, self, send, shadowarg, splat, str, super, sym, true, undef, unless-guard, until, until-post, when, while, while-post, x-heredoc, xstr, yield, z-super]
      --exclude <EXCLUDE>
          If specified, it excludes files or directories matching the given filename pattern from the search
  -T, --end-nodes <END_NODES>
          End nodes [possible values: alias, and, and-asgn, arg, args, array, array-pattern, array-pattern-with-tail, back-ref, begin, block, block-pass, blockarg, break, c-send, case, case-match, casgn, cbase, class, complex, const, const-pattern, cvar, cvasgn, def, defined, defs, dstr, dsym, e-flip-flop, empty-else, encoding, ensure, erange, false, file, find-pattern, float, for, forward-arg, forwarded-args, gvar, gvasgn, hash, hash-pattern, heredoc, i-flip-flop, if, if-guard, if-mod, if-ternary, in-pattern, index, index-asgn, int, irange, ivar, ivasgn, kw-begin, kwarg, kwargs, kwnilarg, kwoptarg, kwrestarg, kwsplat, lambda, line, lvar, lvasgn, masgn, match-alt, match-as, match-current-line, match-nil-pattern, match-pattern, match-pattern-p, match-rest, match-var, match-with-lvasgn, mlhs, module, next, nil, nth-ref, numblock, op-asgn, optarg, or, or-asgn, pair, pin, postexe, preexe, procarg0, rational, redo, reg-opt, regexp, rescue, rescue-body, restarg, retry, return, s-class, self, send, shadowarg, splat, str, super, sym, true, undef, unless-guard, until, until-post, when, while, while-post, x-heredoc, xstr, yield, z-super]
      --no-git-ignore
          Dont respect .gitignore files
  -N, --no-file-name
          Never print the file path with the matched lines
      --no-line-no
          Never print the line number with the matched lines
      --no-color
          Not colored the output results
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
  -q, --quiet
          Do not output matched lines. instead, exit with status 0 when there is a match and with non-zero status when there isn’t
  -h, --help
          Print help
  -V, --version
          Print version
```

## Command line usage example

### Example

```rb
$ rbgrep -S begin -T hash -T pair -T sym title
./example.rb:7     article = {title: title, content: content, published_on: Time.now}
```

```rb
$ rbgrep -S begin -T index -T sym title
./example.rb:14       puts "Title: #{article[:title]}"
```

## License

[MIT](http://opensource.org/licenses/MIT)
