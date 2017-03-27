-module(ertl_433_server).
-behaviour(gen_server).

-include("../include/ertl_433.hrl").

-export([start_link/0]).
-export([log_terminal/1, log_file/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).
-export([get_all_sensors/0]).
-export([get_sensor/1, get_sensor_value/1]).
-export([reset_port/0]).
-export([tab2file/0]).

%% IDs just for easier database referenes
-define(TEMPERATURE, 30).
-define(HUMIDITY, 40).
-define(RAIN, 50).
-define(WIND_GUST, 60).
-define(WIND_AVERAGE, 70).
-define(WIND_DIRECTION, 80).

-define(OREGON_SCIENTIFIC, 100).

-record(state, {port, acc_str, sensor_cnt}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->

    process_flag(trap_exit, true),

    gen_event:start({local, ertl_433_monitor}),

    log_terminal(off),
    log_file(on, "log/ertl_433.txt"),

    %% If we have stored sensor values then use them
    DetsSensorFile = "etc/sensor.txt",
    case filelib:is_regular(DetsSensorFile)  of
	false ->
	    ets:new(sensor_table, [named_table, set, {keypos, #sensor.id}, public]);
	true ->
	    file2tab(DetsSensorFile)
    end,
    
    Self = self(),

    error_logger:info_msg("Startig ~p pid ~p~n ", [?MODULE, Self]),

    Pid = launch_rtl_433(),
    
    gen_server:cast(?MODULE, {start_watchdog, 300}),
    {ok, #state{port = Pid, acc_str = [], sensor_cnt=0}}.


launch_rtl_433() ->
    %% Optionally add only the sensors you want analysed here (but if you can take the extra 10%
    %% CPU on the RPI 2 / 3 then you might find it interesting to log to file everything going
    %% on in your 433MHz spectrum)
    {ok, ErlProcPid, OsPid} = 
	exec:run("rtl_433 -F json",
		 [{stdout, fun(_StreamType,_OsPid,Msg) -> 
				   gen_server:call(?MODULE, {json_stream, Msg}) end}]),
    
    error_logger:info_msg("Started rtl_433 OS pid ~p~n ", [OsPid]),
    ErlProcPid.

%% read from file to ets table
file2tab(File) ->
    case ets:file2tab(File) of
	{ok, Tab} ->
	    error_logger:info_msg("Table read ~p~n ", [Tab]);
	{error, Reason} ->
	    error_logger:error_msg("Table read ~p~n ", [Reason])
    end.

%% write ets tables to file
tab2file() ->
    ets:tab2file(sensor_table, "etc/sensor.txt").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_max(not_found, Value, _Date) ->
    Value;
get_max({sensor, _Id, _Val, _Prev, _Min, Max, Today, _Battery,_Upd}, Value, Date) 
  when Max > Value,  Date == Today ->
    Max;
get_max(_, Value, _Date) ->
    Value.

get_min(not_found, Value, _Date) ->
    Value;
get_min({sensor, _Id, _Val, _Prev, Min, _Max, Today, _Battery, _Upd}, Value, Date) 
  when Min < Value,  Date == Today ->
    Min;
get_min(_, Value, _Date) ->
    Value.

get_sensor(Id) ->
    lookup(sensor_table, Id).

get_sensor_value(Id) ->
    Rec = lookup(sensor_table, Id),
    case Rec of
	{sensor, Id, Val, _Min, _Max, _Today, _Battery, _Upd} ->
	    Val;
	_ -> not_found
    end.

get_all_sensors() ->
    Table = sensor_table,
    First = ets:first(Table),
    get_next(Table, First, []).

reset_port() ->
    gen_server:call(?MODULE, {reset_port}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Logging
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_terminal(on) ->
    gen_event:add_handler(ertl_433_monitor, ertl_433_terminal_logger, []);
log_terminal(off) ->
    gen_event:delete_handler(ertl_433_monitor, ertl_433_terminal_logger, []).

log_file(on, File) ->
    gen_event:add_handler(ertl_433_monitor, ertl_433_file_logger, [File]);
log_file(off, _File) ->
    gen_event:delete_handler(ertl_433_monitor, ertl_433_file_logger, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interpreting the received json string from rtl433
%% Add more models here if wanted
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

battery_convert(<<"OK">>) ->
    good;
battery_convert(_) ->
    low.

validate_model(<<"THGR810">>, M) ->
    Channel = maps:get(<<"channel">>, M),
    Battery = battery_convert(maps:get(<<"battery">>, M)),
    Temp = maps:get(<<"temperature_C">>, M),
    Humidity = maps:get(<<"humidity">>, M),

    %% even multi sensors are stored separately 
    update_sensor(?OREGON_SCIENTIFIC+?TEMPERATURE+Channel, Temp, Battery, true),
    update_sensor(?OREGON_SCIENTIFIC+?HUMIDITY+Channel, Humidity, Battery, true),
    ok;


validate_model(<<"PCR800">>, M) ->
    Channel = maps:get(<<"channel">>, M),
    Battery = battery_convert(maps:get(<<"battery">>, M)),
    _RainRate = maps:get(<<"rain_rate">>, M),
    RainTotal = maps:get(<<"rain_total">>, M),

    update_sensor(?OREGON_SCIENTIFIC+?RAIN+Channel, RainTotal, Battery, true),
    ok;

validate_model(<<"WGR800">>, M) ->
    Channel = maps:get(<<"channel">>, M),
    Battery = battery_convert(maps:get(<<"battery">>, M)),
    Gust = maps:get(<<"gust">>, M),
    Average = maps:get(<<"average">>, M),
    Direction = trunc(maps:get(<<"direction">>, M) / 22.5),

    %% even multi sensors are stored separately 
    %% Note no validation since this type of sensor is just transient
    update_sensor(?OREGON_SCIENTIFIC+?WIND_GUST+Channel, Gust, Battery, false),
    update_sensor(?OREGON_SCIENTIFIC+?WIND_AVERAGE+Channel, Average, Battery, false),
    update_sensor(?OREGON_SCIENTIFIC+?WIND_DIRECTION+Channel, Direction, Battery, false),
    ok;

validate_model(_Model, _Map) ->
    ok.

handle_stdout_stream(Acc, Cnt) ->
    case string:str(Acc, "\n") of
	0 -> 
	    {Acc, Cnt};
	Index ->
	    Line = string:substr(Acc, 1, Index-1),
	    NewAcc = string:substr(Acc, Index+1),
	    Map = jsone:decode(list_to_binary(Line)),

	    case maps:is_key(<<"model">>, Map) of
		true ->
		    Model = maps:get(<<"model">>, Map),
		    validate_model(Model, Map); 
		_ ->
		    ok
	    end,

	    gen_event:notify(ertl_433_monitor, io_lib:format("~p ~p: ~p~n ", 
	    						 [erlang:localtime(), 
	    						  ?MODULE, re:replace(Line,"\\\"","",[global, {return,list}])])),
	    	    
	    handle_stdout_stream(NewAcc, Cnt+1)
    end.

get_next(_Table, '$end_of_table', Acc) ->
    Acc;
get_next(Table, Prev, Acc) ->
    [Val] = ets:lookup(Table, Prev),
    get_next(Table, ets:next(Table, Prev), [Val|Acc]).


lookup(Table, Id) ->
    Val = ets:lookup(Table, Id),
    case Val of
	[] -> not_found;
	List -> [Ret] = List, 
		Ret
    end.
    
validate_sensor_value(not_found, Value) ->
    Value;
validate_sensor_value({sensor, _Id, _Val, Prev, _Min, _Max, _Today, _Battery, _Upd}, Value) 
  when (Prev + 1.0) > Value, (Prev - 1.0) < Value ->
    Value;
validate_sensor_value({sensor, _Id, Val, _Prev, _Min, _Max, _Today, _Battery, _Upd}, _Value) ->
    Val.


update_sensor(Id, Value, Battery, Validate)->
    Sensor = lookup(sensor_table, Id),

    Date = erlang:date(), 

    ValidatedValue = 
	case Validate of
	    true ->
		validate_sensor_value(Sensor, Value);
	    _ ->
		Value
	end,

    Max = get_max(Sensor, ValidatedValue, Date),
    Min = get_min(Sensor, ValidatedValue, Date),
    
    ets:insert(sensor_table, 
	       #sensor{id=Id, 
		       value=ValidatedValue,
		       prev_value=Value,
		       min=Min,
		       max=Max,
		       today=Date,
		       battery=Battery,
		       last_update_time=erlang:timestamp()}),
    
    ok.

       
check_port(0, Pid) ->
    exec:stop(Pid),
    %%exit(Pid, kill);
    
    NewPid = launch_rtl_433(), 
    NewPid;

%% We have receieved some sensors (Cnt > 0) so the port is most likely not hanging 
check_port(_, Pid) ->
    Pid.
		      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% callbacks for the server 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({send, Binary}, _From, #state{port = Pid} = State) ->
    Pid ! {send, Binary},
    {reply, ok, State};

handle_call({json_stream, Msg}, _From, #state{port = _Pid} = State) ->
    {NewAcc, NewCnt} = 
	handle_stdout_stream(State#state.acc_str ++ binary_to_list(Msg), State#state.sensor_cnt),	    
    {reply, ok, State#state{acc_str=NewAcc, sensor_cnt=NewCnt}};

handle_call({reset_port}, _From, #state{port = Pid, acc_str = Str, sensor_cnt = _Cnt}) ->

    NewPid = check_port(0, Pid),
    NewState = #state{port = NewPid, acc_str = Str, sensor_cnt = 0},
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    Reply = ok,
    error_logger:info_msg("handle_call~n", []),
    {reply, Reply, State}.

handle_cast({start_watchdog, Secs}, State) ->
    timer:send_after(Secs*1000, self(), {watchdog_timeout, Secs}),
    error_logger:info_msg("Starting wd ~p~n", [Secs]),
    {noreply, State};

handle_cast(_Msg, State) ->
    error_logger:info_msg("handle_cast~n", []),
    {noreply, State}.

handle_info({watchdog_timeout, Secs}, #state{port = Pid, acc_str = Str, sensor_cnt = Cnt}) ->
    NewPid = check_port(Cnt, Pid),
    timer:send_after(Secs*1000, self(), {watchdog_timeout, Secs}),
    
    NewState = #state{port = NewPid, acc_str = Str, sensor_cnt = 0},
    {noreply, NewState};

handle_info(Info, State) ->
    error_logger:info_msg("handle_info ~p~n", [Info]),
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
terminate(Reason, _State) ->
    error_logger:error_msg("~p ~p: ~p~n ", [erlang:localtime(), ?MODULE, Reason]),    
    ok.
    
