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
%% Copyright 2013 Beads D. Land-Trujillo.  All Rights Reserved.
%% -----------------------------------------------------------------------
%% CDDL HEADER END

%% @doc Intelligent PTL-managing wrapper for `dialyzer'.
%% @reference See <a href="http://www.erlang.org/doc/man/dialyzer.html">dialyzer
%% man page</a> for more details.
%% @end
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2013 Beads D. Land-Trujillo

%% @version 0.1.1
-define(module, tuner).

% BEGIN POSE PACKAGE PATTERN
-ifndef(package).
-module(?module).
-package(default).
-else.
-module(?package.?module).
-package(?package).
-endif.
% END POSE PACKAGE PATTERN

-version("0.1.1").

%%
%% Include files
%%

-define(debug, true).
-include_lib("pose/include/interface.hrl").
-include_lib("pose/include/macro.hrl").

% BEGIN POSE PACKAGE IMPORTS
-ifdef(package).
-import(gen_command).
-import(pose).
-import(filename).
-import(filelib).
-import(sets).
-import(pose_syntax).
-import(code).
-import(pose_file).
-import(string).
-import(erlang).
-import(io_lib).
-import(re).
-import(file).
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
  ?STDOUT("Running Tuner ~s dialyzer PLT optimizer~n", [?VERSION(?MODULE)]),
  {ok, Proj} = file:get_cwd(),
  Ebin = filename:join(Proj, "ebin"),  
  Src = filename:join(Proj, "src"),
  
  Release = erlang:system_info(otp_release),
  DepsDir = re:replace(pose:deps(), "\\.", "_", [global]),
  PLT = filename:join(Ebin, io_lib:format("~s-~s.plt", [Release, DepsDir])),
  
  ?STDOUT("Not doing anything yet.\n"),
  exit(ok),
  
  ?BAIL(PLT),
  
  Erls = filelib:wildcard(filename:join(Src, "*.erl")),
  _UsedErls = used_sources(Erls, sets:from_list(Erls)).


%%
%% Local Functions
%%

used_sources([], Set) -> sets:to_list(Set);
used_sources([Head | Tail], Set) -> 
  %?DEBUG({?WHEREAMI, Head}),
  used_sources(Tail, Set, pose_syntax:qualifiers(Head)).

used_sources(Erls, Set, []) -> used_sources(Erls, Set);
used_sources(Erls, Set, [Head | Tail]) ->
  %?DEBUG({?WHEREAMI, Head}),
  Source = source(Head),
  if is_atom(Source) -> used_sources(Erls, Set, Tail);
     true            -> used_sources(Erls, Set, Tail, Source)
  end.

used_sources(Erls, Set, Tail, Source) ->
  case sets:is_element(Source, Set) of
    true  -> used_sources(Erls, Set, Tail);
    false -> used_sources([Source | Erls], sets:add_element(Source, Set), Tail)
  end.

source(Module) ->
  case code:which(Module) of
    preloaded               -> source(Module, preloaded);
    Atom when is_atom(Atom) -> ?DEBUG({Atom, Module}), Atom;
    File                    -> source(Module, File)
  end.

source(Module, preloaded) ->
  case code:get_object_code(Module) of
    error                   -> ?DEBUG({error, Module}), error;
    {Module, _Binary, File} -> source(Module, File)
  end;
source(Module, File) ->
  case filename:extension(File) of
    ".beam" -> source(Module, File, beam);
    ".erl"  -> File
  end.

source(Module, File, beam) ->
  case pose_file:find_parallel_folder("ebin", "src", filename:dirname(File)) of
    false       -> no_parallel;
    {true, Src} -> Erl = string:concat(atom_to_list(Module), ".erl"),
                   filename:join(Src, Erl)
  end.