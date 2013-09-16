-record(access_mode,{
          r = true :: boolean(),
          sr = true :: boolean(),
          w = true :: boolean(),
          sw = true :: boolean()
         }).

-record(model, {
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
          init_fun = [],

          before_save = [],
          after_create = [],
          after_update = [],
          before_delete = [],
          after_delete = []
         }).

-record(field, {
          name,
          type,
          alias,
          is_index = false,
          mode = #access_mode{}
         }).
