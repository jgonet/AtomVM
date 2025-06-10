%
% This file is part of AtomVM.
%
% Copyright 2018 Davide Bettio <davide@uninstall.it>, Paul Guyot <pguyot@kallisys.net>
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

-module(vm_guards).
-define(ID(X1), ?MODULE:id(X1)).
-define(ID(X1, X2), ?MODULE:id(X1), ?MODULE:id(X2)).
-define(ID(X1, X2, X3), ?MODULE:id(X1), ?MODULE:id(X2), ?MODULE:id(X3)).
-define(ID(X1, X2, X3, X4), ?MODULE:id(X1), ?MODULE:id(X2), ?MODULE:id(X3), ?MODULE:id(X4)).
-export([start/0, id/1, loop/0]).

start() ->
    ok = test_numbers_equalities(),
    ok = test_raising_gc_bif1(),
    ok = test_raising_gc_bif2(),
    ok = test_raising_gc_bif3(),
    ok = test_raising_bif1(),
    ok = test_raising_bif2(),
    ok = test_min_max(),
    ok = test_types(),
    ok =
        case atomvm:platform() of
            generic_unix -> test_port_type();
            _ -> ok
        end,
    0.

test_numbers_equalities() ->
    gte = cmp_relative(?ID(1, 1)),
    lt = cmp_relative(?ID(1, 2)),
    gt = cmp_relative(?ID(3, 1)),

    eq_exact = cmp_eq(?ID(1, 1)),
    eq = cmp_eq(?ID(1, 1.0)),
    neq_exact = cmp_neq_exact(?ID(1, 1.0)),
    neq = cmp_neq(?ID(1, 2)),
    ok.

test_port_type() ->
    Port = open_port({spawn, "echo"}, []),
    port = guard_type(?ID(Port)),
    ok.

test_types() ->
    Pid = spawn_opt(?MODULE, loop, [], []),
    pid = guard_type(?ID(Pid)),

    list = guard_type(?ID([])),
    list = guard_type(?ID("abc")),
    list = guard_type(?ID([ok, ok])),
    list = guard_type(?ID([ok | ok])),

    binary = guard_type(?ID(<<1, 2, 3>>)),
    binary = guard_type(?ID(<<"a">>)),
    binary = guard_type(?ID(<<>>)),

    number = guard_type(?ID(2)),
    number = guard_type(?ID(2.0)),
    integer = guard_number_exact(?ID(2)),
    float = guard_number_exact(?ID(2.0)),

    boolean = guard_type(?ID(true)),
    boolean = guard_type(?ID(false)),

    atom = guard_type(ok),
    ok.

test_min_max() ->
    ok = min_max_guard(?ID(min, 2, 1, 2)),
    ok = min_max_guard(?ID(max, 2, 1, 1)),
    ok = min_max_case(?ID(min, 2, 1, 2)),
    ok = min_max_case(?ID(max, 2, 1, 1)),
    1 = tail_min(?ID(infinity, [5, 4, 3, 2, 1])),
    5 = tail_max(?ID(0, [1, 2, 3, 4, 5])),
    ok.

cmp_relative(A, B) when A > B -> gt;
cmp_relative(A, B) when A < B -> lt;
cmp_relative(A, B) when A >= B -> gte;
cmp_relative(A, B) when A =< B -> lte.

cmp_eq(A, B) when A =:= B -> eq_exact;
cmp_eq(A, B) when A == B -> eq.

cmp_neq_exact(A, B) when A =/= B -> neq_exact.
cmp_neq(A, B) when A /= B -> neq.

guard_type(S) when is_pid(S) -> pid;
guard_type(S) when is_port(S) -> port;
guard_type(S) when is_number(S) -> number;
guard_type(S) when is_list(S) -> list;
guard_type(S) when is_binary(S) -> binary;
guard_type(S) when is_boolean(S) -> boolean;
guard_type(S) when is_atom(S) -> atom.

guard_number_exact(N) when is_integer(N) -> integer;
guard_number_exact(N) when is_float(N) -> float.

loop() -> loop().

test_raising_gc_bif1() ->
    NotAList = ?ID(not_a_list),
    if
        length(NotAList) < 42 -> fail;
        true -> ok
    end.

test_raising_gc_bif2() ->
    NotAnInteger = ?ID(not_an_integer),
    if
        NotAnInteger rem 42 < 20 -> fail;
        true -> ok
    end.

test_raising_gc_bif3() ->
    case erlang:function_exported(erlang, binary_part, 3) of
        true ->
            NotABin = ?ID(not_a_bin),
            ok =
                if
                    binary_part(NotABin, 0, 2) =:= <<"he">> -> fail;
                    true -> ok
                end;
        false ->
            erlang:display({warning, erlang, binary_part, 3, unimplemented}),
            ok
    end.

test_raising_bif1() ->
    NotAList = ?ID(not_a_list),
    if
        tl(NotAList) > 1 -> fail;
        true -> ok
    end.

test_raising_bif2() ->
    NotAMap = ?ID(not_a_map),
    if
        is_map_key(b, NotAMap) -> fail;
        true -> ok
    end.

-ifdef(OTP_RELEASE).
%% OTP 21 or higher
-if(?OTP_RELEASE >= 26).
min_max_guard(min, X, Y, Z) when min(X, Y) < Z ->
    ok;
min_max_guard(max, X, Y, Z) when max(X, Y) > Z ->
    ok;
min_max_guard(_Op, _X, _Y, _Z) ->
    fail.
-else.
min_max_guard(_Op, _X, _Y, _Z) ->
    ok.
-endif.
-else.
min_max_guard(_Op, _X, _Y, _Z) ->
    ok.
-endif.

% jgonet: not sure if this can be folded to Z = min(?ID(X, Y))
% in any case, should be tested separately in bif_guards
min_max_case(min, X, Y, Z) ->
    case min(X, Y) < Z of
        true -> ok;
        false -> fail
    end;
min_max_case(max, X, Y, Z) ->
    case max(X, Y) > Z of
        true -> ok;
        false -> fail
    end;
min_max_case(_Op, _X, _Y, _Z) ->
    fail.

% jgonet: under new test arch tail calls should be tested separately in vm_tail_call
tail_min(X, [Y]) ->
    % OP_CALL_EXT_ONLY
    min(X, Y);
tail_min(X, [H | T]) ->
    tail_min(min(X, H), T).

tail_max(X, [Y, Z]) ->
    M1 = max(Y, Z),
    % OP_CALL_EXT_LAST
    max(X, M1);
tail_max(X, [H | T]) ->
    tail_max(max(X, H), T).

id(X) -> X.
