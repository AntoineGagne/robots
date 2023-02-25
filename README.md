# robots

[![Build Status](https://github.com/AntoineGagne/robots/actions/workflows/erlang.yml/badge.svg)](https://github.com/AntoineGagne/robots/actions)
[![Hex Pm](http://img.shields.io/hexpm/v/robots.svg?style=flat)](https://hex.pm/packages/robots)
[![Docs](https://img.shields.io/badge/hex-docs-green.svg?style=flat)](https://hexdocs.pm/robots)
[![Releases](https://img.shields.io/github/release/AntoineGagne/robots?color=brightgreen)](https://github.com/AntoineGagne/robots/releases)
[![Coverage](https://coveralls.io/repos/github/AntoineGagne/robots/badge.svg?branch=master)](https://coveralls.io/github/AntoineGagne/robots?branch=master)

A library that parses and validates rules from `robots.txt`.

## Installation

This library is available on [hex.pm](https://hex.pm/packages/robots).
To install this library, simply add the following lines to your
`rebar.config`:

```erlang
{robots, "1.1.1"}
```

## Usage

```erlang
Content = <<"User-Agent: bot\nAllow: /fish">>,
%% This will return an opaque type that contains all the rules and their agents
{ok, RulesIndex} = robots:parse(Content, 200),
true = robots:is_allowed(<<"bot/1.0.0">>, <<"/fish/salmon.html">>, RulesIndex),
true = robots:is_allowed(<<"bot/1.0.0">>, <<"/Fish.asp">>, RulesIndex),
```

## Development

### Running all the tests and linters

You can run all the tests and linters with the `rebar3` alias:

```sh
rebar3 check
```
