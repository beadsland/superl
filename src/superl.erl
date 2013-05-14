%% CDDL HEADER START    -*-Erlang-*-
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
%% Copyright 2012, 2013 Beads D. Land-Trujillo.  All Rights Reserved.
%% -----------------------------------------------------------------------
%% CDDL HEADER END

%% @doc This is the superly good style checker for Erlang modules.
%%
%% Checks all modules and header files in an OTP project for good style,
%% and reports the highest priority issue found in each file.
%% Files are sorted by last modification date, such that the issues
%% in the most recently updated files are the first identified.
%%
%% In addition to identifying issues with leading tabs, long lines,
%% long files, and long modules, `superl' also looks for good commenting.
%%
%% A rule of thumb for good code-to-comments is a ratio of 1:4 (not
%% counting white space).  However, given the importance of inline `edoc'
%% comments in Erlang modules, `superl' currently reports any file with
%% less than a 40% ratio.
%%
%% Similarly, `edoc' looks for good comment distribution, in that on
%% average, longer functions are expected to have at least one comment
%% line as a header.  This comment line serves two functions.  First,
%% it helps to break up code visually in color-coded IDEs.  Second, it
%% provides an opportunity to narrate multi-arity progressions of
%% functions, where such functions often represent several steps of the
%% same complex operation, broken up to avoid nesting `if' and `case'
%% clauses within a single over-long function.
%% @end
%% @reference Rudimentary checks for
%% <a href="http://www.erlang.se/doc/programming_rules.shtml#REF11301">
%% Erlang style conventions</a>.
%% @end
%% @reference See also
%% <a href="http://www.erlang.se/doc/programming_rules.shtml#REF66257">
%% Most Common Mistakes</a> section of same document.
%% @end
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012, 2013 Beads D. Land-Trujillo

%% @todo check for bad return types (rewrite as edoc doclet??)
%% @todo check for deep nesting (largely dealt with by line/func length)
%% @todo simple variable naming tests

%% @version 0.1.10

-define(module, superl).

% BEGIN POSE PACKAGE PATTERN
-ifndef(package).
-module(?module).
-package(default).
-else.
-module(?package.?module).
-package(?package).
-endif.
% END POSE PACKAGE PATTERN

-version("0.1.10").

%%
%% Include files
%%

%-define(debug, true).
-include_lib("pose/include/interface.hrl").
-include_lib("pose/include/macro.hrl").

-record(lineinfo,
        {   tabs, max, total, bigfunc, curfunc,
            hlines, clines, curspan, maxspan } ).

-include_lib("kernel/include/file.hrl").

% BEGIN POSE PACKAGE IMPORTS
-ifdef(package).
-import(gen_command).
-import(filename).
-import(file).
-import(lists).
-import(re).
-import(string).
-endif.
% END POSE PACKAGE IMPORTS

%%
%% Exported Functions
%%

-behaviour(gen_command).

% API entry points
-export([start/0, start/1, run/3]).

% Private callbacks
-export([do_run/2]).

%%
%% API Functions
%%

-spec start() -> no_return().
%% @equiv start([])
start() -> start([]).

-spec start(Param :: [atom()]) -> no_return().
%% @doc Start as a blocking function.
start(Param) -> gen_command:start(Param, ?MODULE).

-spec run(IO :: #std{}, ARG :: #arg{}, ENV :: #env{}) -> no_return().
%% doc Start as a `pose' command.
run(IO, ARG, ENV) -> gen_command:run(IO, ARG, ENV, ?MODULE).

%%
%% Callback Functions
%%

%% @private Callback entry point for gen_command behaviour.
do_run(IO, _ARG) ->
  ?STDOUT("Running Superl ~s good style checker~n", [?VERSION(?MODULE)]),

  Src = filename:absname("src"),
  Inc = filename:absname("include"),

  {ok, SrcList} = file:list_dir(Src),
  {ok, IncList} = file:list_dir(Inc),

  Pred = fun(X) -> lists:suffix(".erl", X) orelse lists:suffix(".hrl", X) end,
  SrcComp = [ lists:append([Src, "/", X]) || X <- SrcList, Pred(X) ],
  IncComp = [ lists:append([Inc, "/", X]) || X <- IncList, Pred(X) ],

  Sorted = lists:sort(fun more_recently_modified/2, SrcComp ++ IncComp),

  case review(IO, Sorted) of
    good    -> ?STDOUT("All good!\n"), exit(ok);
    nogood  -> ?STDOUT("Not yet superly good.\n"), exit(nogood)
  end.

%%
%% Local Functions
%%

%%%
% Report results
%%%

-define(ANYEDIT, 
        "~s: AnyEdit auto-convert not working in Juno 4.2: use ctrl-s~n").
-define(WARN_TABS, "~s: avoid leading tabs (convert to spaces)~n").
-define(WARN_LINES, "~s: avoid long lines (~p char line found)~n").
-define(WARN_MODULES, "~s: avoid long modules (~p lines found)~n").
-define(WARN_FUNC, "~s: avoid long functions (~p line function found)~n").
-define(WARN_DOC, "~s: document code (~p% comment-to-code ratio found)~n").
-define(WARN_SPAN, "~s: avoid long undocumented spans (~p line span found)~n").

report_results(IO, Module, Info) ->
  Ratio = round(Info#lineinfo.hlines / (Info#lineinfo.clines + 1) * 100),
  if Info#lineinfo.tabs == true     ->
       ?STDOUT(?WARN_TABS, [Module]), ?STDOUT(?ANYEDIT, [Module]), nogood;
     Info#lineinfo.max > 80         ->
       ?STDOUT(?WARN_LINES, [Module, Info#lineinfo.max]), nogood;
     Info#lineinfo.total > 400      ->
       ?STDOUT(?WARN_MODULES, [Module, Info#lineinfo.total]), nogood;
     Info#lineinfo.bigfunc > 20     ->
       ?STDOUT(?WARN_FUNC, [Module, Info#lineinfo.bigfunc]), nogood;
     Ratio < 40                     ->
       ?STDOUT(?WARN_DOC, [Module, Ratio]), nogood;
     Info#lineinfo.maxspan > 30     ->
       ?STDOUT(?WARN_SPAN, [Module, Info#lineinfo.maxspan]), nogood;
     true -> good
  end.

%%%
% Do analysis
%%%

% Iterate over files.
review(_IO, []) -> good;
review(IO, [Head | Tail]) ->
  TailResult = review(IO, Tail),
  case test_lines(IO, Head) of
    nogood  -> nogood;
    good    -> TailResult
  end.

% Analyze a specific file.
test_lines(IO, File) ->
  {ok, FileID} = file:open(File, [read]),
  Info = line_info(FileID),
  {ok, MP} = re:compile("([^\\/]+)\\.[he]rl$"),
  {match, [Module]} = re:run(File, MP, [{capture, [1], list}]),
  report_results(IO, Module, Info).

% Open a file to analyze same.
line_info(FileID) ->
  case file:read_line(FileID) of
    eof         -> #lineinfo{   tabs = false,
                                max = 0,
                                total = 0,
                                curfunc = 0,
                                bigfunc = 0,
                                hlines = 0,
                                clines = 0,
                                curspan = 0,
                                maxspan = 0 };
    {ok, Line}  -> line_info(FileID, Line)
  end.

% Iterate over each line in a file.
% @TODO rewrite this so it's tail recursive
line_info(FileID, Line) ->
  Info = line_info(FileID),

  TabsMatch = re:run(Line, "^\s*\t+", [{capture, none}]),
  NewTabs = case TabsMatch of nomatch -> Info#lineinfo.tabs; match -> true end,

  NewMax = max(string:len(string:strip(Line, right, $\n)), Info#lineinfo.max),
  NewTotal = Info#lineinfo.total + 1,
  {NewCurFunc, LineType} = function_length(Info#lineinfo.curfunc, Line),
  NewBigFunc = max(Info#lineinfo.bigfunc, NewCurFunc),
  {NewHLine, NewCLine} = line_counters(Info, LineType),
  NewCurSpan = if LineType==header -> 0; true -> Info#lineinfo.curspan + 1 end,
  NewMaxSpan = max(NewCurSpan, Info#lineinfo.maxspan),

  Info#lineinfo{    tabs = NewTabs,
                    max = NewMax,           total = NewTotal,
                    curfunc = NewCurFunc,   bigfunc = NewBigFunc,
                    hlines = NewHLine,      clines = NewCLine,
                    curspan = NewCurSpan,   maxspan = NewMaxSpan    }.

% Track header (comment) lines and code lines.
line_counters(Info, LineType) ->
  if LineType == header ->
       {Info#lineinfo.hlines + 1, Info#lineinfo.clines};
     LineType == code ->
       {Info#lineinfo.hlines, Info#lineinfo.clines + 1};
     true ->    % LineType == blank
       {Info#lineinfo.hlines, Info#lineinfo.clines}
  end.

% Track the length of each function.
function_length(Count, Line) ->
  {ok, MP} = re:compile("^[^\\%]+\\.\\ *$"),
  case re:run(Line, MP, [{capture, none}]) of
    match   -> {1, code};
    nomatch -> case Line of
            [$% | _More]        -> {Count, header};
            "\n"                -> {Count, blank};
            _Else               -> {Count + 1, code}
           end
  end.

%%%
% File comparisons
%%%

more_recently_modified(File1, File2) ->
  {ok, FileInfo1} = file:read_file_info(File1),
  {ok, FileInfo2} = file:read_file_info(File2),
  if FileInfo1#file_info.mtime > FileInfo2#file_info.mtime  -> true;
     true                                                   -> false
  end.