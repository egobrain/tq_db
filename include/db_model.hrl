-record(db_model, {
          module,
          fields = [],
          table,
          stores_in_db,

          %% Generate
          get=false,
          save=false,
          delete=false,
          find=false,

          %% Hooks
          from_db_funs = [] :: atom() | {atom(), atom()} | {atom(), list()} | {atom(), atom(), list()},

          before_save = [],
          after_create = [],
          after_update = [],
          before_delete = [],
          after_delete = []
         }).

-record(db_field, {
          name,
          type,
          alias,
          is_index = false,
          record,

          from_db_funs = [] :: atom() | {atom(), atom()} | {atom(), list()} | {atom(), atom(), list()},
          to_db_funs = [] :: atom() | {atom(), atom()} | {atom(), list()} | {atom(), atom(), list()}
         }).
