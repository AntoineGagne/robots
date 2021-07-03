======
robots
======

.. image:: https://github.com/AntoineGagne/robots/actions/workflows/erlang.yml/badge.svg
    :target: https://github.com/AntoineGagne/robots/actions

.. image:: http://img.shields.io/hexpm/v/robots.svg?style=flat
    :target: https://hex.pm/packages/robots

.. image:: https://img.shields.io/github/release/AntoineGagne/robots?color=brightgreen
    :target: https://github.com/AntoineGagne/robots/releases

.. image:: https://coveralls.io/repos/github/AntoineGagne/robots/badge.svg?branch=master
    :target: https://coveralls.io/github/AntoineGagne/robots?branch=master


:Author: `Antoine Gagné <gagnantoine@gmail.com>`_

.. contents::
    :backlinks: none

.. sectnum::

A library that parses and validates rules from ``robots.txt``.

Installation
============

This library is available on `hex.pm <https://hex.pm/packages/robots>`_.
To install this library, simply add the following lines to your
``rebar.config``:

.. code-block:: erlang

    {robots, "1.0.0"}

Usage
=====

.. code-block:: erlang

    Content = <<"User-Agent: bot\nAllow: /fish">>,
    %% This will return an opaque type that contains all the rules and their agents
    {ok, RulesIndex} = robots:parse(Content, 200),
    true = robots:is_allowed(<<"bot/1.0.0">>, <<"/fish/salmon.html">>, RulesIndex),
    false = robots:is_allowed(<<"bot/1.0.0">>, <<"/Fish.asp">>, RulesIndex),

Development
===========

Running all the tests and linters
---------------------------------

You can run all the tests and linters with the ``rebar3`` alias:

.. code-block:: sh

    rebar3 check
