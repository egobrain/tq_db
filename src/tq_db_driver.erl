-module(tq_db_driver).

-type model() :: tuple(any()).

-callback start_pool(PoolName :: atom(), Opts :: any()) ->
    ok |
    {ok, Pid :: pid()} |
    {error, Reason :: any()}.

-callback 'query'(PoolName :: module(), Sql, Args, Constructor) ->
    {ok, Rows} |
    {ok, Count} |
    {ok, Count, Rows} |
    {error, Reason :: any()}
        when
      Sql :: binary(),
      Args :: [{Type :: atom(), Data :: any()}],
      Constructor :: fun((list()) -> model()),
      Count :: non_neg_integer(),
      Rows :: [model()].

-callback parse(Module :: module(), Query) ->
    {ok, {Sql, Fields, ArgTransform}} |
    {error, Reason :: any()}
        when
      Query :: any(),
      Sql :: binary(),
      Fields :: list(atom()),
      ArgTransform :: fun((any()) -> {Type :: atom(), Value :: any()}).

-callback find(PoolName :: atom(), Module :: module(), Query, QueryArgs) ->
    {ok, Models :: [model()]} |
    {error, Reason :: any()}
        when
      Query :: any(),
      QueryArgs :: [any()].

-callback get(PoolName :: atom(), Module :: module(), IndexFV) ->
    {ok, Model :: model()} |
    {error, Reason :: any()}
        when
      IndexFV :: [{FieldName :: atom(), Value :: any()}].

-callback insert(PoolName :: atom(), Module :: module(), ChangedFV) ->
    {ok, Model :: model()} |
    {error, Reason :: any} when
      ChangedFV :: [{FieldName :: atom(), Value :: any()}].

-callback update(PoolName :: atom(), Module :: module(), ChangedFV, IndexFV) ->
    {ok, Model :: model()} |
    {error, Reason :: any} when
      ChangedFV :: [{FieldName :: atom(), Value :: any()}],
      IndexFV :: [{FieldName :: atom(), Value :: any()}].

-callback delete(PoolName :: atom(), Module :: module(), IndexFV) ->
    ok |{error, Reason :: any()}
        when
      IndexFV :: [{FieldName :: atom(), Value :: any()}].
