[![Build Status][gh badge]][gh]
[![Hex.pm version][hexpm version]][hexpm]
[![Hex.pm Downloads][hexpm downloads]][hexpm]
[![Hex.pm Documentation][hexdocs documentation]][hexdocs]
[![Erlang Versions][erlang version badge]][gh]
[![License][license]](LICENSE)

router
======

router is an in-memory trie based path router for Erlang.
It can be used to route paths to destinations. It has single-level,
and multi-level wildcard levels.

It can be used for arbitrary path/hierarchical based routing, and you
can have multiple independent routers.

Usage
-----

Create a new router like this:

```erlang
   Router = router:new()
```

This creates multiple ets tables which are owned by the calling process. It
is advised to call this inside a `gen_server`.

Now you can add routes like this:

```erlang
   router:add(Router, [<<"a">>, <<"b">>], MyDestination),
   ...
```

This creates a route from path ```<<"a">>, <<"b">>``` to the information inside 
`MyDestination`. It is important that routes are added synchronized. It is best
to do it inside a `handle_call` or `handle_cast` of a single `gen_server`.

After this it is possible to route request paths to destinations:

```erlang
  Routes = router:route(Router, [<<"foo">>, <<"bar">>]),
  ...
```

Calls to ```route``` can be called in parallel.


Wildcards
---------

Router has two different wildcards, multi-level and single-level wildcards.

The following wildcards are implemented:

- `'+'`, matches one level on the path.
- `{'+', test}`, matches one level on the paths and binds the path element to test.
- `{'+', test, {module, function}}`, matches the element on the path if the call
  to `module:function(PathElement)` returns true.

There is one multi-level wildcard `'#'` which matches all elements on the path.

Example single-level wildcard route:

```erlang
   router:add(Router, [<<"rsc">>, {'+', id}], {controller_rsc, [{foo, <<"bar">>]})
```

When you now call `router:route(Router,[<<"rsc">>, <<"12312">>])`, you get a list
with route tuples with all matching destinations and the list of bound variables in a
proplist. In this case: `[{route, {controller_rsc, [{foo, <<"bar">>}]}, [{id, <<"12312">>}]`.

You get a list of matching destinations, and all elements which are bound in a proplist
as result.

Example multi-level wildcard route:

```Erlang

    router:add(Router, [<<"truck">>, <<"00001">>, '#'], self())
```

This will match all routes to `[<<"truck">>, <<"00001">>]` and all its sub paths. For 
example: `[<<"truck">>, <<"00001">>, <<"temperature">>]` will match and also 
`[<<"truck">>, <<"00001">>, <<"location">>]`.



<!-- Badges -->
[hexpm]: https://hex.pm/packages/router
[hexpm version]: https://img.shields.io/hexpm/v/router.svg?style=flat-curcle "Hex version"
[hexpm downloads]: https://img.shields.io/hexpm/dt/router.svg?style=flat-curcle
[hexdocs documentation]: https://img.shields.io/badge/hex-docs-purple.svg?style=flat-curcle
[hexdocs]: https://hexdocs.pm/router
[gh]: https://github.com/zotonic/router/actions/workflows/test.yaml
[gh badge]: https://github.com/zotonic/router/workflows/Test/badge.svg
[erlang version badge]: https://img.shields.io/badge/Supported%20Erlang%2FOTP-22%20to%2024-blue.svg?style=flat-curcle
[license]: https://img.shields.io/badge/License-MIT-blue.svg "MIT"
