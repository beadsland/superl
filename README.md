

#Welcome to the superly good style checker (superl)#


Copyright (c) 2012 Beads D. Land-Trujillo

__Version:__ 0.1.0

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).

__<font color="red">To do</font>__
<br></br>
* <font color="red">add dependency soft-linking option to rebar</font>


This is the superly good style checker for Erlang modules.
  See [`superl`](http://github.com/beadsland/nosh/blob/master/doc/superl.md) module documentation for features and usage.  * [Installation](http://github.com/beadsland/nosh/blob/master/doc/README.md#Installation)
 * [Converting Tabs](http://github.com/beadsland/nosh/blob/master/doc/README.md#Converting_Tabs)
  

###<a name="Installation">Installation</a>##
 

Superl is relatively lightweight, and be included as a project
  dependency via `rebar.config`: 

`{deps, [     {pose, ".*",       {git, "git://github.com/beadsland/pose", {branch, master}}}    ]}` 

Following `rebar get-deps`, the style checker can be invoked easily 
from an erl command-line: 

`> erl -pa deps/superl/ebin -s superl -s init stop` 

It is recommended that the above be incorporated into a `good` rule 
in project makefiles, resulting in an invocation of: 

`> make good` 

###<a name="Converting_Tabs">Converting Tabs</a>##
 

The first test `superl` runs on any file is to check of there are 
leading tabs on any line.  The number of spaces tabs are rendered 
at in any given editor are not guaranteed, and thus they should be 
converted to spaces to ensure stylistic indentation is portable across 
development environments. If using the Eclipse IDE, one way to handle this is with the AnyEdit
  Tools plugin, available in the Eclipse Marketplace.

##Modules##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/beadsland/nosh/blob/master/doc/superl.md" class="module">superl</a></td></tr></table>

