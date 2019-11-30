======
robots
======

.. image:: https://travis-ci.org/AntoineGagne/robots.svg?branch=master
    :target: https://travis-ci.org/AntoineGagne/robots

:Author: `Antoine Gagné <gagnantoine@gmail.com>`_

.. contents::
    :backlinks: none

.. sectnum::

A library that parses and validates rules from ``robots.txt``.

Usage
=====

.. code::block:: erlang

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
