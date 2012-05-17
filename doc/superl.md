

#Module ?module#

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


This is the superly good style checker for Erlang modules.

Copyright (c) 2012 Beads D. Land-Trujillo

__Version:__ 0.1.7

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).

__References__
* Rudimentary checks for
[
Erlang style conventions](http://www.erlang.se/doc/programming_rules..md#REF11301).
* See also
[
Most Common Mistakes](http://www.erlang.se/doc/programming_rules..md#REF66257) section of same document.


__<font color="red">To do</font>__
<br></br>

* <font color="red">check for bad return types (rewrite as edoc doclet??)</font>
* <font color="red">check for deep nesting (largely dealt with by line/func length)</font>
* <font color="red">simple variable naming tests</font>
<a name="description"></a>

##Description##


Checks all modules and header files in a project src directory for
good style, and reports the first issue it finds in each file.
Files are sorted by last modification date, such that the issues
in the most recently updated files are the first identified.<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#run-3">run/3</a></td><td>Start superly good style check as a
<a href="http://github.com/beadsland/pose">pose</a> process.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>Start superly good style check as a blocking function.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="run-3"></a>

###run/3##


<pre>run(IO::#std{}, ARG::#arg{}, ENV::#env{}) -&gt; no_return()</pre>
<br></br>


Start superly good style check as a
[pose](http://github.com/beadsland/pose) process.<a name="start-0"></a>

###start/0##


<pre>start() -&gt; ok | nogood</pre>
<br></br>


Start superly good style check as a blocking function.
All results are written to standard output.