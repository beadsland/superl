

#Module superl#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


This is the superly good style checker for Erlang modules.

Copyright (c) 2012, 2013 Beads D. Land-Trujillo

__Version:__ 0.1.11

__Behaviours:__ [`gen_command`](gen_command.md).

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).

__References__* Rudimentary checks for
[
Erlang style conventions](http://www.erlang.se/doc/programming_rules..md#REF11301).
* See also
[
Most Common Mistakes](http://www.erlang.se/doc/programming_rules..md#REF66257) section of same document.
* For wisdom on how many parameters are too many, see
[C2 Wiki](http://c2.com/cgi/wiki?TooManyParameters) and
[Stack Overflow](http://goo.gl/lACxP) discussions of same.


__<font color="red">To do</font>__
<br></br>
* <font color="red">check for bad return types (rewrite as edoc doclet??)</font>
* <font color="red">check for deep nesting (largely dealt with by line/func length)</font>
* <font color="red">simple variable naming tests</font>
<a name="description"></a>

##Description##




Checks all modules and header files in an OTP project for good style,
and reports the highest priority bad smells found in each file.
Files are sorted by last modification date, such that the issues
in the most recently updated files are the first identified.



In addition to identifying issues with leading tabs, long lines,
long files, and long modules, `superl` also looks for good commenting.
A rule of thumb for good code-to-comments is a ratio of 1:4 (not
counting white space).  However, given the importance of inline `edoc`
comments in Erlang modules, `superl` currently reports any file with
less than a 40% ratio.



Similarly, looks for good comment distribution, in that on
average, longer functions are expected to have at least one comment
line as a header.  This comment line serves two functions.  First,
it helps to break up code visually in color-coded IDEs.  Second, it
provides an opportunity to narrate multi-arity progressions of
functions, where such functions often represent several steps of the
same complex operation, broken up to avoid nesting `if` and `case`
clauses within a single over-long function.

That said, high-arity functions--in addition to being harder to read
and understand--likely indicate either a failure to encapsulate related data
or an attempt to make one function responsible for too many things.
<a name="types"></a>

##Data Types##




###<a name="type-env_prop">env_prop()</a>##



	env_prop() = atom() | {atom(), string()}
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#run-3">run/3</a></td><td></td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>Equivalent to <a href="#start-1"><tt>start([])</tt></a>.</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>Start as a blocking function.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="run-3"></a>

###run/3##


	run(IO::#std{in = pid(), out = pid(), err = pid(), stop = boolean(), echo = boolean()}, ARG::#arg{cmd = atom(), v = list()}, ENV::#env{all = [<a href="#type-env_prop">env_prop()</a>]}) -> no_return()
<br></br>


<a name="start-0"></a>

###start/0##


	start() -&gt; no_return()
<br></br>


Equivalent to [`start([])`](#start-1).<a name="start-1"></a>

###start/1##


	start(Param::[atom()]) -&gt; no_return()
<br></br>


Start as a blocking function.