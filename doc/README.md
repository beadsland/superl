

#Welcome to the superly good style checker (superl)#


Copyright (c) 2012 Beads D. Land-Trujillo

__Version:__ 0.1.0

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).

This is the superly good style checker for Erlang modules.
  See [`superl`](superl.md) module documentation for features. 

###<a name="Installation">Installation</a>##
 

Superl is relatively lightweight, and be included as a project
  dependency via `rebar.config`: 

`{deps, [     {pose, ".*",       {git, "git://github.com/beadsland/pose", {branch, master}}}    ]}` 

Following `rebar get-deps`, the style checker can be invoked easily 
from an erl command-line: 

`> erl -pa deps/superl/ebin -s superl -s init stop` 

It is recommended that the above be incorporated into a `good` rule 
in project makefiles, resulting in an invocation of: `> make good`

##Modules##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="superl.md" class="module">superl</a></td></tr></table>

