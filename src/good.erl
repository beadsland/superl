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

%% @doc Pose-compatible alias for {@link superl}.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

-define(module, good).

% BEGIN POSE PACKAGE PATTERN
-ifndef(package).
-module(?module).
-package(default).
-else.
-module(?package.?module).
-package(?package).
-endif.
% END POSE PACKAGE PATTERN

%%
%% Include files
%%

%-define(debug, true).
-include("pose/include/interface.hrl").

-import(pose).
-import(pose_command).

%%
%% Exported Functions
%%
-export([run/3]).

%%
%% API Functions
%%

-spec run(IO :: #std{}, ARG :: #arg{}, ENV :: #env{}) -> no_return().
%% @equiv superl:run(IO)
run(IO, ARG, ENV) ->
  ?INIT_POSE,
  case pose_command:load(superl) of
    {module, Module, Warnings}    -> pose:load_warn(IO, superl, Warnings),
                                     Module:run(IO, ARG, ENV);
    {error, What, Warnings}       -> pose:load_warn(IO, superl, Warnings),
                                     ?STDERR({superl, What})
  end.

%%
%% Local Functions
%%

