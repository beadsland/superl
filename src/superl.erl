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

%% @doc This is the superly good style checker for Erlang modules.
%%
%% Checks all modules and header files in a project src directory for
%% good style, and reports the first issue it finds in each file.
%% Files are sorted by last modification date, such that the issues
%% in the most recently updated files are the first identified.
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
%% @copyright 2012 Beads D. Land-Trujillo

%% @todo check for bad return types (rewrite as edoc doclet??)
%% @todo check for deep nesting (largely dealt with by line/func length)
%% @todo simple variable naming tests

%% @version 0.1.6

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

-version("0.1.6").

%%
%% Include files
%%

%-define(debug, true).
-include("pose/include/interface.hrl").

-import(filename).
-import(file).
-import(lists).
-import(io).
-import(re).
-import(string).

-include_lib("kernel/include/file.hrl").
-include("macro.hrl").

-record(lineinfo,
        {	tabs, max, total, bigfunc, curfunc, hlines, clines, specs } ).

%%
%% Exported Functions
%%

% API functions
-export([start/0, run/3]).

% Qualified functions
-export([loop/2]).

%%
%% API Functions
%%

%% @doc Start superly good style check as a blocking function.
%% All results are written to standard output.
%% @end
-spec start() -> ok | nogood.
%
start() ->
  IO = ?IO(self()),
  RunPid = spawn_link(?MODULE, run, [IO, ?ARG(?MODULE), ?ENV]),
  ?MODULE:loop(IO, RunPid).

%% @doc Start superly good style check as a
%% <a href="http://github.com/beadsland/pose"><cmd>pose</cmd></a> process.
%% @end
-spec run(IO :: #std{}, ARG :: #arg{}, ENV :: #env{}) -> no_return().
%
run(IO, ARG, ENV) ->
  ?INIT_POSE,
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
    good	-> ?STDOUT("All good!\n"), exit(ok);
    nogood	-> ?STDOUT("Not yet superly good.\n"), exit(nogood)
  end.

%%
%% Local Functions
%%

% @hidden Export to allow for hotswap.
loop(IO, RunPid) ->
  receive
    {purging, _Pid, _Mod}       -> ?MODULE:loop(IO, RunPid);
    {'EXIT', RunPid, Reason}    -> Reason;
    {MsgTag, RunPid, Line}      -> do_output(MsgTag, Line),
                                   ?MODULE:loop(IO, RunPid);
    Noise                       -> do_noise(Noise),
                                   ?MODULE:loop(IO, RunPid)
  end.

% Handle stderr and stdout messages.
do_output(MsgTag, Output) ->
  case MsgTag of
    stdout  -> io:format("~s", [Output]);
    erlout  -> io:format("~p: data: ~p~n", [?MODULE, Output]);
    erlerr  -> Erlerr = ?FORMAT_ERLERR(Output),
               io:format(standard_error, "** ~s~n", [Erlerr]);
    stderr  -> io:format(standard_error, "** ~s", [Output]);
    debug   -> io:format(standard_error, "-- ~s", [Output])
  end.

% Handle message queue noise.
do_noise(Noise) ->
  io:format(standard_error, "noise: ~p ~p~n", [Noise, self()]).

more_recently_modified(File1, File2) ->
  {ok, FileInfo1} = file:read_file_info(File1),
  {ok, FileInfo2} = file:read_file_info(File2),
  if FileInfo1#file_info.mtime > FileInfo2#file_info.mtime 	-> true;
     true													-> false
  end.

review(_IO, []) -> good;
review(IO, [Head | Tail]) ->
  TailResult = review(IO, Tail),
  case test_lines(IO, Head) of
    nogood	-> nogood;
    good	-> TailResult
  end.

test_lines(IO, File) ->
  {ok, FileID} = file:open(File, [read]),
  {ok, MP} = re:compile("^(.*)\.[he]rl$"),
  {match, [Module]} = re:run(File, MP, [{capture, [1], list}]),
  Info = line_info(FileID),
  report_results(IO, Module, Info).

-define(WARN_TABS, "~s: avoid leading tabs (convert to spaces)~n").
-define(WARN_LINES, "~s: avoid long lines (~p char line found)~n").
-define(WARN_MODULES, "~s: avoid long modules (~p lines found)~n").
-define(WARN_FUNC, "~s: avoid long functions (~p line function found)~n").
-define(WARN_DOC, "~s: document code (~p% comment-to-code ratio found)~n").

report_results(IO, Module, Info) ->
  Ratio = round(Info#lineinfo.hlines / (Info#lineinfo.clines+1) * 100),
  if Info#lineinfo.tabs == true ->
       ?STDOUT(?WARN_TABS, [Module]), nogood;
     Info#lineinfo.max > 80 ->
       ?STDOUT(?WARN_LINES, [Module, Info#lineinfo.max]), nogood;
     Info#lineinfo.total > 400 ->
       ?STDOUT(?WARN_MODULES, [Module, Info#lineinfo.total]), nogood;
     Info#lineinfo.bigfunc > 20 ->
       ?STDOUT(?WARN_FUNC, [Module, Info#lineinfo.bigfunc]), nogood;
     Ratio < 25 ->
       ?STDOUT(?WARN_DOC, [Module, Ratio]), nogood;
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