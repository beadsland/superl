%% CDDL HEADER START
%% -----------------------------------------------------------------------
%% The contents of this file are subject to the Common Development and
%% Distribution License, Version 1.0 (the "License"); you may not use
%% this file except in compliance with the License.  You should have
%% received a copy of the Common Development and Distribution License
%% along with this software.  If not, it can be retrieved online at
%% http://www.opensource.org/licenses/CDDL-1.0
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% When distributing Covered Code, include this CDDL Header Notice in
%% each file and include the License file at CDDL-LICENSE.  If applicable
%% add the following below the CDDL Header, with the fields enclosed
%% by brackets replaced by your own identifying information:
%% "Portions Copyright [year] [name of copyright owner]"
%%
%% Copyright 2012 Beads D. Land-Trujillo.  All Rights Reserved
%% -----------------------------------------------------------------------
%% CDDL HEADER END

%% @doc Simple style checker for Erlang modules.
%% @reference Rudimentary checks for
%% <a href="http://www.erlang.se/doc/programming_rules.shtml#REF11301">
%% Erlang style conventions</a>.
%% @end
%% @reference See also
%% <a href="http://www.erlang.se/doc/programming_rules.shtml#REF66257">
%% Most Common Mistakes</a> section of same document.
%% @end
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% @todo add nosh executable behaviour (not yet defined)
%% @todo check for bad return types (rewrite as edoc doclet??)
%% @todo check for deep nesting (largely dealt with by line/func length)
%% @todo simple variable naming tests

%% @version 0.1.3
-module(superl).
-version("0.1.3").

%%
%% Include files
%%

-include("macro.hrl").

-include_lib("kernel/include/file.hrl").

-record(lineinfo, {	tabs, max, total, bigfunc, curfunc, hlines, clines, specs } ).

%%
%% Exported Functions
%%
-export([start/0]).

%%
%% API Functions
%%

start() ->
  io:format("Running Superl ~s good style checker~n", [?VERSION(?MODULE)]),
  Pwd = filename:absname(""),

  Src = filename:absname("src"),
  file:set_cwd(Src),

  {ok, Listing} = file:list_dir(Src),
  Sorted = lists:sort(fun more_recently_modified/2, Listing),

  case review(Src, Sorted) of
    good	-> io:format("All good!\n");
    nogood	-> io:format("Not yet superly good.\n")
  end,

  file:set_cwd(Pwd).

%%
%% Local Functions
%%

more_recently_modified(File1, File2) ->
  {ok, FileInfo1} = file:read_file_info(File1),
  {ok, FileInfo2} = file:read_file_info(File2),
  if FileInfo1#file_info.mtime > FileInfo2#file_info.mtime 	-> true;
     true														-> false
  end.

review(_Dir, []) -> good;
review(Dir, [Head | Tail]) ->
  {ok, MP} = re:compile("\.[he]rl$"),
  case re:run(Head, MP, [{capture, none}]) of
    nomatch	-> review(Dir, Tail);
    match	-> HeadResult = test_lines(Dir, Head),
           TailResult = review(Dir, Tail),
           case HeadResult of
             nogood	-> nogood;
             good		-> TailResult
           end
  end.

test_lines(Dir, File) ->
  {ok, FileID} = file:open(Dir ++ "/" ++ File, [read]),
  {ok, MP} = re:compile("^(.*)\.[he]rl$"),
  {match, [Module]} = re:run(File, MP, [{capture, [1], list}]),
  Info = line_info(FileID),
  report_results(Module, Info).

-define(WARN_TABS, "~s: avoid leading tabs (convert to spaces)~n").
-define(WARN_LINES, "~s: avoid long lines (~p char line found)~n").
-define(WARN_MODULES, "~s: avoid long modules (~p lines found)~n").
-define(WARN_FUNC, "~s: avoid long functions (~p line function found)~n").
-define(WARN_DOC, "~s: document code (~p% comment-to-code ratio found)~n").

report_results(Module, Info) ->
  Ratio = round(Info#lineinfo.hlines / Info#lineinfo.clines * 100),
  if Info#lineinfo.tabs == true ->
      io:format(?WARN_TABS, [Module]), nogood;
     Info#lineinfo.max > 80 ->
      io:format(?WARN_LINES, [Module, Info#lineinfo.max]), nogood;
     Info#lineinfo.total > 400 ->
         io:format(?WARN_MODULES, [Module, Info#lineinfo.total]), nogood;
     Info#lineinfo.bigfunc > 20 ->
         io:format(?WARN_FUNC, [Module, Info#lineinfo.bigfunc]), nogood;
     Ratio < 25 ->
         io:format(?WARN_DOC, [Module, Ratio]), nogood;
     true -> good
  end.

line_info(FileID) ->
  case file:read_line(FileID) of
    eof			-> #lineinfo{	tabs = false,
                  max = 0,
                  total = 0,
                  curfunc = 0,
                  bigfunc = 0,
                  hlines = 0,
                  clines = 0 };
    {ok, Line} 	-> line_info(FileID, Line)
  end.

% @TODO rewrite this so it's tail recursive
line_info(FileID, Line) ->
  Info = line_info(FileID),

  case re:run(Line, "^\s*\t+", [{capture, none}]) of
    nomatch	-> NewTabs = Info#lineinfo.tabs;
    match	-> NewTabs = true
  end,

  NewMax = max(string:len(Line), Info#lineinfo.max),
     NewTotal = Info#lineinfo.total + 1,
     {NewCurFunc, LineType} = function_length(Info#lineinfo.curfunc, Line),
     NewBigFunc = max(Info#lineinfo.bigfunc, NewCurFunc),

  {NewHLine, NewCLine} = line_counters(Info, LineType),

  Info#lineinfo{	tabs = NewTabs,
          max = NewMax,			total = NewTotal,
             curfunc = NewCurFunc,	bigfunc = NewBigFunc,
             hlines = NewHLine,		clines = NewCLine	}.

line_counters(Info, LineType) ->
  if LineType == header ->
       {Info#lineinfo.hlines + 1, Info#lineinfo.clines};
     LineType == code ->
       {Info#lineinfo.hlines, Info#lineinfo.clines + 1};
     true ->	% LineType == blank
       {Info#lineinfo.hlines, Info#lineinfo.clines}
     end.

function_length(Count, Line) ->
  {ok, MP} = re:compile("^[^\\%]+\\.\\ *$"),
  case re:run(Line, MP, [{capture, none}]) of
    match	-> {1, code};
    nomatch -> case Line of
            [$%, $% | _More]	-> {Count, header};
            "\n"				-> {Count, blank};
            _Else				-> {Count + 1, code}
           end
  end.