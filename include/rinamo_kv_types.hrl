% redefined here so rinamo doesn't have to
% specify riak_kv as a dependency.
%
% originally from:
%   riak_kv/include/riak_kv_types.hrl
-record(crdt_op, {mod, op, ctx}).

-define(CRDT_OP, #crdt_op).
