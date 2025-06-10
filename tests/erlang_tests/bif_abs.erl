%
% This file is part of AtomVM.
%
% Copyright 2019 Davide Bettio <davide@uninstall.it>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(bif_abs).
-define(ID(X), ?MODULE:id(X)).
-export([start/0, id/1]).

start() ->
    Literal = ?ID(-16#800_0000),
    16#800_0000 = abs(Literal),

    Calculated = pow(-2, ?ID(27)),
    16#800_0000 = abs(Calculated),

    case wordbits() of
        32 ->
            Boxed = pow(-2, ?ID(31)),
            true = is_boxed(Boxed),
            16#8000_0000 = abs(Boxed);
        64 ->
            Boxed = pow(-2, ?ID(62)),
            true = is_boxed(Boxed),
            16#4000_0000_0000_0000 = abs(Boxed)
    end,

    Float = ?ID(-10.0),
    true = 10.0 =:= abs(Float),
    ok.

id(X) ->
    X.

pow(N, 0) when is_number(N) ->
    1;
pow(N, M) ->
    N * pow(N, M - 1).

is_boxed(T) -> erts_debug:flat_size(T) =/= 0.
wordbits() -> erlang:system_info(wordsize) * 8.
