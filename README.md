# gen_nntp

[![CI](https://github.com/sntran/gen_nntp/actions/workflows/elixir.yml/badge.svg)](https://github.com/sntran/gen_nntp/actions/workflows/elixir.yml)

The Erlang NNTP client and server library.

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `gen_nntp` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:gen_nntp, "~> 0.1.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/gen_nntp](https://hexdocs.pm/gen_nntp).

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
- [ ] POST
- [ ] ARTICLE
- [x] BODY
- [ ] DATE
- [x] GROUP
- [x] LAST
- [x] LISTGROUP
- [ ] NEWGROUPS
- [x] NEXT
