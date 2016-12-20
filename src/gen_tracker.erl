%%%-------------------------------------------------------------------
%%% @author stark
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Nov 2016 4:05 PM
%%%-------------------------------------------------------------------
-module(gen_tracker).
-author("stark").


-include_lib("stdlib/include/ms_transform.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1, find_or_create/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(entry, {
  key,
  pid,
  ref
}).

-record(state, {
  zone
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Zone :: atom()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Zone) ->
  Tracker = list_to_atom(atom_to_list(Zone) ++ "_tracker"),
  gen_server:start_link({local, Tracker}, ?MODULE, [Zone], []).

-spec find_or_create(Zone :: atom(), ChildSpec :: supervisor:child_spec())
      -> {ok, Pid :: pid()}.
find_or_create(Zone, #{id := ChildId} = ChildSpec) ->
  {ok, Pid_} =
    case ets:lookup(Zone, ChildId) of
      [] ->
        Tracker = list_to_atom(atom_to_list(Zone) ++ "_tracker"),
        gen_server:call(Tracker, {find_or_create, ChildSpec});
      [#entry{pid = Pid}] ->
        {ok, Pid}
    end,
  {ok, Pid_}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Zone]) ->
  ets:new(Zone, [set, protected, named_table, {keypos, #entry.key}, {read_concurrency, true}]),
  {ok, #state{zone = Zone}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({find_or_create, #{id := ChildId} = ChildSpec}, _From, #state{zone = Zone} = State) ->
  Pid =
    case ets:lookup(Zone, ChildId) of
      [] ->
        TrackerSup = list_to_atom(atom_to_list(Zone) ++ "_tracker_sup"),
        create_child(Zone, TrackerSup, ChildId, ChildSpec);
      [#entry{pid = Pid_}] ->
        Pid_
    end,
  {reply, {ok, Pid}, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info({'DOWN', _, process, ChildPid, _Reason}, #state{zone = Zone} = State) ->
  ets:select_delete(Zone, ets:fun2ms(fun(#entry{pid = Pid}) when ChildPid == Pid -> true end)),
  {noreply, State};
handle_info(Info, State) ->
  {stop, {unknown_message, Info}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
create_child(Zone, TrackerSup, ChildId, ChildSpec) ->
  {ok, Pid} = supervisor:start_child(TrackerSup, ChildSpec),
  Ref = erlang:monitor(process, Pid),
  ets:insert(Zone, #entry{key = ChildId, ref = Ref, pid = Pid}),
  Pid.