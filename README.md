# gen_nntp

[![CI](https://github.com/sntran/gen_nntp/actions/workflows/elixir.yml/badge.svg)](https://github.com/sntran/gen_nntp/actions/workflows/elixir.yml)
[![Hex Version](https://img.shields.io/hexpm/v/gen_nntp.svg)](https://hex.pm/packages/gen_nntp)
[![License](https://img.shields.io/github/license/sntran/gen_nntp.svg)](https://choosealicense.com/licenses/apache-2.0/)

The Erlang NNTP client and server library.

## Installation

The package can be installed by adding `gen_nntp` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:gen_nntp, "~> 0.19.0"}
  ]
end
```

Documentation can be found at [https://hexdocs.pm/gen_nntp](https://hexdocs.pm/gen_nntp).

## NNTP Protocols

### Commands

- [x] CAPABILITIES
- [x] HEAD
- [x] HELP
- [x] QUIT
- [x] STAT
- [ ] HDR
- [ ] LIST HEADERS
- [ ] IHAVE
- [ ] LIST
- [ ] LIST ACTIVE
- [ ] LIST ACTIVE.TIMES
- [ ] LIST DISTRIB.PATS
- [ ] LIST NEWSGROUPS
- [ ] MODE READER
- [ ] NEWNEWS
- [ ] OVER
- [ ] LIST OVERVIEW.FMT
- [x] POST
- [ ] ARTICLE
- [x] BODY
- [ ] DATE
- [x] GROUP
- [x] LAST
- [x] LISTGROUP
- [ ] NEWGROUPS
- [x] NEXT
